# copybook-model — Authoritative Module Documentation (AST → Concrete Byte Layout)

## 1. Module Mission (Non-Negotiable)

`copybook-model` is responsible for converting a fully parsed **CopybookAst** into a **deterministic, concrete LayoutModel** that defines *exact byte-level storage* for every elementary field.

This module is the **single source of truth for memory layout**.  
All downstream stages (codec generation, runtime serialization, validation) must treat its output as authoritative.

**No downstream component may infer sizes, offsets, or overlays independently.**

---

## 2. Hard Scope Boundaries

### This module MUST
- Compute byte lengths for all elementary items
- Compute absolute byte offsets for all fields
- Expand fixed OCCURS into repeated layout entries
- Apply IBM-standard numeric sizing rules
- Construct REDEFINES overlay groups
- Validate total record length deterministically
- Produce stable, ordered, immutable layout output
- Emit deterministic layout reports (text + JSON)

### This module MUST NOT
- Parse COBOL syntax
- Interpret raw PIC strings
- Guess or infer missing AST data
- Apply runtime alignment or padding
- Perform serialization or I/O
- Depend on environment or configuration

This module is a **pure function**.

---

## 3. Input Contract

### Input
```java
CopybookAst
```

Assumptions:
- AST is syntactically and semantically valid
- All PIC clauses are fully parsed
- All OCCURS are fixed
- All REDEFINES are resolved structurally

If these assumptions are violated, this module **fails fast**.

---

## 4. Output Contract

### Output
```java
LayoutModel
```

The LayoutModel is:
- Fully flattened
- Order-preserving
- Immutable
- Deterministic
- Path-addressable

Every leaf represents a *real byte range*.

---

## 5. Layout Model (Conceptual)

```java
record LayoutModel(
    int totalLength,
    List<LayoutField> fields,
    List<OverlayGroup> overlays
) {}

record LayoutField(
    String path,
    int offset,
    int length,
    Usage usage,
    PicSummary pic,
    int occursIndex
) {}

record OverlayGroup(
    String basePath,
    int offset,
    int length,
    List<String> memberPaths
) {}
```

No group items appear as LayoutFields.

---

## 6. Size Computation Rules (Authoritative)

### DISPLAY
- 1 byte per digit
- Sign consumes 1 byte if present
- `V` (virtual decimal) consumes 0 bytes

### COMP (IBM Binary)
| Digits | Bytes |
|------|------|
| 1–4 | 2 |
| 5–9 | 4 |
| 10–18 | 8 |

### COMP-3 (Packed Decimal)
- Size = `ceil((digits + 1) / 2)`
- Includes sign nibble
- `V` ignored

These rules are **not configurable**.

---

## 7. OCCURS Expansion

- OCCURS are expanded into repeated LayoutFields
- Nested OCCURS multiply
- Each occurrence has:
  - unique absolute offset
  - stable occurrence index
- No stride-based collapsing occurs in LayoutModel

---

## 8. Group Items

Group items:
- Have no independent storage
- Offset = offset of first child
- Length = sum of children (post-expansion)
- Exist only as structural parents

---

## 9. REDEFINES (Overlay Model)

- REDEFINES create OverlayGroups
- All redefining fields share the same offset
- Length = max(length of all members)
- Member fields retain full declared length

OverlayGroups are explicit and enumerable.

---

## 10. Offset Computation Algorithm

1. Walk AST depth-first
2. Expand OCCURS eagerly
3. Accumulate offset monotonically
4. Apply REDEFINES by offset reuse
5. Validate final offset == totalLength

No padding, alignment, or backtracking allowed.

---

## 11. Validation Rules

- All offsets ≥ 0
- No negative lengths
- No overlapping fields except via REDEFINES
- Total length matches expected record size
- All REDEFINES targets resolved

Violation → fatal error.

---

## 12. Layout Reports

### Text Report
- Ordered field list
- Offsets and lengths
- Overlay group summaries

### JSON Report
- Machine-readable
- Snapshot-testable
- Stable field ordering

---

## 13. Testing Requirements

- Golden-layout snapshot tests
- OCCURS expansion tests
- REDEFINES overlay tests
- Numeric sizing tests
- Full-record length validation

Any layout change is **breaking** unless approved.

---

## 14. Determinism Guarantees

- Same AST → same LayoutModel
- No maps with undefined ordering
- No reflection
- No environment influence

---

## 15. Position in the System

```text
Copybook Text
   ↓
copybook-parser
   ↓
CopybookAst
   ↓
copybook-model
   ↓
LayoutModel
   ↓
codec generation
   ↓
runtime
```

---

## 16. Final Invariant

> If a downstream component needs to “recompute” layout, this module is wrong.

This module defines memory truth.
