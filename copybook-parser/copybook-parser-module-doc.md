# copybook-parser — Authoritative Module Documentation (Fixed-Format + Parsed PIC Model)

## 1. Module Mission (Non-Negotiable)

`copybook-parser` converts **fixed-format COBOL copybooks** into a **fully parsed, deterministic Abstract Syntax Tree (AST)** that explicitly represents **all supported semantic structure** without performing **any layout, encoding, or runtime interpretation**.

This module is the **single source of truth** for COBOL *structure*. Downstream stages are forbidden from inferring, guessing, or re-parsing copybook text.

**If the meaning exists in COBOL syntax, it must exist explicitly in the AST.**

---

## 2. Hard Scope Boundaries

### This module MUST
- Parse fixed-format COBOL copybooks
- Enforce column rules deterministically
- Tokenize and parse:
  - levels
  - data names
  - PIC clauses into structured tokens
  - USAGE
  - fixed OCCURS
  - REDEFINES
  - COPY statements (detection only)
- Preserve declaration order
- Produce a stable, immutable AST
- Fail fast on unsupported or ambiguous syntax

### This module MUST NOT
- Compute byte lengths
- Expand OCCURS
- Interpret numeric encoding sizes
- Generate Java
- Resolve COPY includes
- Guess missing syntax
- Tolerate malformed input “flexibly”

---

## 3. Input Contract (Fixed-Format COBOL)

### Column Rules (Strict)

| Columns | Meaning | Handling |
|------|--------|--------|
| 1–6 | Sequence area | Ignored |
| 7 | Indicator area | `*` = comment line |
| 8–72 | Program text | Parsed |
| 73+ | Identification | Ignored |

### Comments
- Line is a comment **iff** column 7 contains `*`
- Inline comments are **not supported**
- Comment lines are discarded during normalization

### Continuation Rules
- **Not supported**
- A logical data description **must be fully contained in columns 8–72**
- If a clause spans lines → **parse error**

This is intentional to preserve determinism.

---

## 4. Normalization Phase

### Purpose
Convert raw file lines into a clean, position-aware token stream.

### Algorithm
For each physical line:
1. Discard columns 1–6
2. If column 7 == `*`, discard line
3. Extract columns 8–72 verbatim
4. Preserve:
   - original line number
   - starting column offset (always 8)

### Output
A sequence of:

```java
record NormalizedLine(int lineNumber, String text) {}
```

No semantic interpretation occurs here.

---

## 5. Tokenization Phase

### Token Categories (Closed Set)

| Token | Example |
|-----|--------|
| LEVEL_NUMBER | `01`, `05`, `15` |
| IDENTIFIER | `CUSTOMER-ID`, `ITEM-AMT` |
| KEYWORD | `PIC`, `PICTURE`, `USAGE`, `OCCURS`, `REDEFINES`, `COPY`, `TIMES` |
| INTEGER | `9`, `10`, `99` |
| PUNCT | `.`, `(`, `)`, `V` |
| PIC_SYMBOL | `X`, `9`, `S` |

### Rules
- Keywords are case-insensitive, normalized to uppercase
- Identifiers preserve original spelling
- Hyphenated names are single identifiers
- Token stream is position-aware (line + column)
- Invalid token → **fatal error**

---

## 6. Parsing Strategy (Deterministic)

### Entry Types
Parser recognizes exactly two top-level entries:
1. **Data Description Entry**
2. **COPY Statement**

Anything else → parse error.

---

## 7. Data Description Entry Grammar

### Canonical Form
```
<level> <data-name> [REDEFINES <data-name>] {<clause>} .
```

### Clause Order
- Clause order is **free**
- Clauses may appear in any order
- Each clause may appear **at most once**

### Supported Clauses
- `PIC` / `PICTURE`
- `USAGE`
- `OCCURS n TIMES`
- `REDEFINES target`

Missing required structure or duplicated clauses → error.

---

## 8. Level Semantics

### Accepted Levels
- `01`–`49`

### Rejected Levels
- `66`, `77`, `88` (explicitly unsupported)

Unsupported levels → **fatal parse error**

---

## 9. Hierarchy Construction (Level Stack)

### Algorithm (Authoritative)
Maintain a stack of `DataItemNode`.

For each parsed entry:
1. While stack not empty and stack.top.level ≥ current.level → pop
2. If stack empty → node is root
3. Else → node is child of stack.top
4. Push node

### Validation
- Child level must be numerically greater than parent
- Illegal nesting → error

---

## 10. PIC Clause Parsing (Structured Model)

### PIC Is Fully Parsed — Never Raw

#### Supported PIC Grammar
```
PIC ::= [S] { X(n) | 9(n) | V | 9 | X }+
```

Examples:
- `PIC X(30)`
- `PIC 9(7)V99`
- `PIC S9(5)V99`

### PIC AST Model

```java
sealed interface PicElement {}

record PicSymbol(char symbol) implements PicElement {}        // 'X' or '9'
record PicRepeat(char symbol, int count) implements PicElement {}
record PicVirtualDecimal() implements PicElement {}
record PicSign() implements PicElement {}
```

### PicClause

```java
record PicClause(boolean signed, List<PicElement> elements) {}
```

### Rules
- `V` produces a `PicVirtualDecimal`
- `S` must appear first if present
- Parentheses must contain a positive integer
- Any invalid sequence → fatal error

---

## 11. USAGE Clause

### Accepted Values
- `DISPLAY`
- `COMP`
- `COMP-3`

```java
enum Usage { DISPLAY, COMP, COMP_3 }
```

Unsupported USAGE → error.

---

## 12. OCCURS Clause (Fixed Only)

### Grammar
```
OCCURS <integer> TIMES
```

### Rules
- Integer must be > 0
- DEPENDING ON is **not allowed**
- OCCURS is stored structurally only (no expansion)

```java
record OccursClause(int count) {}
```

---

## 13. REDEFINES Clause (Overlay Model)

### Grammar
```
REDEFINES <identifier>
```

### Rules
- Target must be a **previous sibling**
- Target must exist
- Cross-scope redefines → error

```java
record RedefinesClause(String targetName) {}
```

### Linking Phase
After full parse:
- Resolve `targetName` → node reference (deterministic lookup)
- Store resolved overlay link (use stable node id/path in snapshots)
- Unresolved target → fatal error

---

## 14. COPY Statement Handling

### Detection Only
Grammar:
```
COPY <identifier>.
```

Representation:
```java
record CopyNode(String copybookName, SourceSpan span) {}
```

Resolution is external.

---

## 15. AST Structure (Final)

```java
record CopybookAst(List<AstNode> roots, List<Diagnostic> diagnostics) {}

sealed interface AstNode {}

record DataItemNode(
    int level,
    String name,
    PicClause pic,
    Usage usage,
    OccursClause occurs,
    RedefinesClause redefines,
    List<DataItemNode> children,
    SourceSpan span
) implements AstNode {}

record CopyNode(String copybookName, SourceSpan span) implements AstNode {}
```

All nodes are immutable.

---

## 16. Error Handling (Strict)

### Error Categories
- NormalizationError
- LexicalError
- ParseError
- SemanticError

All errors include:
- error code
- message
- source span
- exact failing token

Parser never silently recovers.

---

## 17. Testing Requirements (Definition of Correctness)

### Mandatory
- Snapshot tests for full AST
- Clause permutation tests
- REDEFINES resolution tests
- OCCURS tests
- PIC parsing tests
- Invalid syntax tests (must fail)

### Snapshot Stability Rule
Any AST snapshot change is a breaking change unless explicitly approved.

---

## 18. Determinism Guarantees

- Same input bytes → identical AST
- No reflection
- No map-iteration nondeterminism
- No environment influence
- No heuristic interpretation

---

## 19. Position in the System

```text
COBOL COPYBOOK
   ↓ (fixed-format parsing)
copybook-parser
   ↓ (explicit AST)
layout computation
   ↓
codec generation
   ↓
runtime transport
```

---

## 20. Final Invariant (Contract)

> If a downstream component needs to “re-interpret” COBOL syntax, then this module is incomplete or wrong.

This parser defines the semantic boundary between COBOL and Java. That boundary is hard, explicit, and unambiguous.

---

# Parser Checklist (Use for PR Review and Release Gates)

This checklist is **mandatory** for:
- initial implementation PRs
- any grammar/normalization changes
- any AST model changes
- any bugfix that changes parsing outcomes
- any snapshot update PR

## A. Determinism & Stability
- [ ] Parser output is identical across runs for identical input bytes.
- [ ] No usage of unordered iteration that can change order (e.g., HashMap iteration) in AST emission or snapshot serialization.
- [ ] AST node ordering matches source declaration ordering.
- [ ] Snapshot serializer is deterministic (stable ordering, stable node identifiers).
- [ ] Any intentional AST change is documented and snapshots are updated in the same PR with rationale.

## B. Fixed-Format Enforcement
- [ ] Columns 1–6 ignored; columns 73+ ignored.
- [ ] Column 7 `*` comment lines are removed deterministically.
- [ ] Only columns 8–72 are parsed.
- [ ] Multi-line continuation is rejected (explicit error) unless feature has been added and documented.
- [ ] Errors report original line number and column (mapped correctly to fixed-format positions).

## C. Tokenization Quality
- [ ] Tokenizer normalizes keywords to uppercase and treats COBOL as case-insensitive.
- [ ] Hyphenated data names are tokenized as a single IDENTIFIER.
- [ ] Token locations (line/column) are correct and tested.
- [ ] Unknown characters or malformed tokens fail fast with LexicalError.

## D. Grammar Coverage (Current Scope)
- [ ] Data description entries: `<level> <name> ...`
- [ ] Level support restricted to 01–49; 66/77/88 rejected with clear error.
- [ ] PIC clause parsed into structured token model (no raw-only fallback).
- [ ] USAGE supports DISPLAY/COMP/COMP-3 only; others rejected.
- [ ] OCCURS supports fixed integer only; DEPENDING ON rejected.
- [ ] REDEFINES recognized and captured for linking.
- [ ] COPY statements detected and represented (or rejected intentionally if configured).

## E. PIC Parsing Correctness (Parsed Token Model)
- [ ] PIC supports `S` only as first symbol; otherwise error.
- [ ] PIC supports `X`, `9`, `V`, and repeats `X(n)` / `9(n)` where n>0.
- [ ] Parentheses numeric parsing is strict; invalid forms error.
- [ ] PIC element list is minimal and normalized (e.g., `9(1)` becomes either repeat or single symbol consistently—choose one policy and enforce it everywhere).
- [ ] PIC parser has unit tests for:
  - [ ] `X(30)`
  - [ ] `9(7)V99`
  - [ ] `S9(5)V99`
  - [ ] invalid sequences (e.g., `VS9`, `S` not first, missing `)`, non-numeric repeat)

## F. Hierarchy Construction (Level Stack)
- [ ] Level stack algorithm is implemented exactly (pop while >= current, attach to parent with < current).
- [ ] Illegal nesting generates SemanticError with source span.
- [ ] Tree building is covered by tests with multi-level nesting.
- [ ] Duplicate names are allowed (COBOL allows) unless explicitly forbidden by downstream contract (if forbidden, reject deterministically and document why).

## G. REDEFINES Linking Rules
- [ ] REDEFINES target must be a previous sibling under the same parent scope.
- [ ] Target resolution is deterministic and tested.
- [ ] Missing target produces fatal SemanticError (not warning).
- [ ] Snapshot output represents REDEFINES links using stable ids/paths (not memory pointers).

## H. OCCURS Rules
- [ ] OCCURS count must be > 0 (reject 0 or negative).
- [ ] OCCURS is not expanded in AST (no duplication of nodes).
- [ ] OCCURS nodes preserve children structure if present.
- [ ] Tests cover nested OCCURS and OCCURS on groups.

## I. COPY Handling
- [ ] COPY statement is represented with copybook name + span.
- [ ] If COPY is not allowed in this module (policy choice), parser errors clearly and early with actionable message.
- [ ] COPY does not silently disappear; it must be detectable in AST or rejected.

## J. Error Reporting & Diagnostics
- [ ] All errors include category, code, message, and source span.
- [ ] Errors include the failing token and a small deterministic excerpt.
- [ ] Error messages do not depend on runtime environment.
- [ ] No silent recovery (unless tolerant mode exists and is explicitly configured; if so, tolerant mode behavior is documented and tested separately).

## K. Test Suite Requirements
- [ ] Snapshot tests exist for representative real copybooks (occurs + redefines + mixed usage).
- [ ] Unit tests exist for tokenization edge cases and PIC parsing.
- [ ] Negative tests validate correct failure mode and error category.
- [ ] Tests run in CI and are required for merge.
- [ ] Snapshot updates are intentional and reviewed as breaking changes.

## L. Release Gate
- [ ] All checklist sections pass.
- [ ] Snapshot diffs reviewed and approved (or none).
- [ ] Any new syntax support documented in this file.
- [ ] Versioning decision made if AST schema changes (major/minor per your project rules).

