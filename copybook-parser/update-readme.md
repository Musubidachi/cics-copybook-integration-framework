# Copybook Parser – LLM Task Execution Plan

This document defines **LLM-ready task prompts** for completing the
`copybook-parser` module.

Each task:
- is **self-contained**
- includes **explicit verification criteria**
- must be completed **in order**
- must not modify any module other than `copybook-parser`

The parser is the foundation of the entire system. Downstream modules
(`copybook-model`, `codegen`, `runtime`) MUST NOT be touched until all
tasks below pass verification.

---

## Global Rules for the LLM

- Modify **only** `copybook-parser`
- Use `transactionspec-testcases/01-parser/**` as the source of truth
- Do not invent new COBOL features
- Unsupported features must fail fast with structured errors
- Tests are mandatory for every implemented feature

---

# TASK 1 — Fixture-Driven Parser Tests

### LLM PROMPT
> Implement JUnit 5 tests for the `copybook-parser` module that execute all fixtures under `transactionspec-testcases/01-parser/**`.
>
> Each test must:
> - load the fixture input file
> - invoke the parser
> - serialize the AST to JSON
> - compare it against the expected snapshot
>
> Negative fixtures must assert on structured parse errors.

### VERIFICATION
- `mvn -q -pl copybook-parser test` runs all fixtures
- Every fixture is executed by a test
- Failures produce readable diffs or error assertions

---

# TASK 2 — Stable AST Serialization

### LLM PROMPT
> Implement a deterministic AST-to-JSON serializer for `copybook-parser`.
>
> Serialization must ensure:
> - stable ordering of lists and maps
> - consistent null/default handling
> - predictable formatting of source positions
>
> Snapshot outputs must not change across runs.

### VERIFICATION
- Running tests twice produces identical JSON
- Refactoring parser internals does not change snapshots

---

# TASK 3 — Level-88 (Condition Name) Parsing

### LLM PROMPT
> Extend the parser to fully support COBOL level-88 condition names.
>
> Requirements:
> - Accept level `88` entries
> - Attach them to the immediately preceding data item
> - Parse VALUE clauses supporting:
>   - single literals
>   - multiple literals
>   - THRU / THROUGH ranges
> - Represent condition names explicitly in the AST
>
> Level-88 entries must not affect layout sizing.

### VERIFICATION
- All level-88 fixtures pass
- Parser no longer throws “unsupported level 88”
- Conditions are visible in serialized AST output

---

# TASK 4 — COPY ... REPLACING Syntax Parsing

### LLM PROMPT
> Extend the parser to support `COPY ... REPLACING` syntax.
>
> Requirements:
> - Parse `COPY name.`
> - Parse `COPY name REPLACING ...`
> - Support multiple REPLACING pairs
> - Preserve raw replacement tokens in the AST
> - Do not expand COPY statements yet
>
> Introduce a dedicated AST node for COPY.

### VERIFICATION
- COPY REPLACING fixtures parse successfully
- AST snapshots match expected outputs
- No COPY syntax is ignored or silently dropped

---

# TASK 5 — COPY Expansion (Include Resolution)

### LLM PROMPT
> Implement COPY expansion so that COPY statements are resolved and expanded into the AST.
>
> Requirements:
> - Introduce a `CopybookResolver` abstraction
> - Resolve copybooks from fixture directories
> - Parse included copybooks recursively
> - Splice expanded nodes into the parent AST
> - Emit a structured error if a copybook is missing
>
> Expansion must occur before AST is returned.

### VERIFICATION
- Expanded AST matches fixture expectations
- Missing copybooks produce `COPYBOOK_NOT_FOUND` errors
- Nested COPY statements resolve correctly

---

# TASK 6 — REPLACING Transformation Logic

### LLM PROMPT
> Implement correct REPLACING behavior for COPY expansion.
>
> Requirements:
> - Perform REPLACING at the token-stream level
> - Support multiple replacement pairs
> - Handle COBOL delimiters (`==`, quotes)
> - Ensure deterministic left-to-right replacement
>
> Do NOT use naive string replacement.

### VERIFICATION
- All REPLACING expansion fixtures pass
- No malformed tokens or parse regressions occur

---

# TASK 7 — Harden Tokenization and Numeric Parsing

### LLM PROMPT
> Improve tokenizer and parser robustness to handle real COBOL syntax.
>
> Requirements:
> - Support signed numeric literals
> - Support decimal literals
> - Recognize THRU / THROUGH keywords
> - Prevent NPEs or index errors from malformed PIC clauses
>
> Errors must be structured, not crashes.

### VERIFICATION
- Parser never crashes on malformed input
- Errors include line/column info
- All existing fixtures still pass

---

# TASK 8 — Structured Parse Errors

### LLM PROMPT
> Replace all ad-hoc runtime exceptions with structured parse errors.
>
> Requirements:
> - Define a `ParseError` model with:
>   - error code (enum)
>   - message
>   - line and column
>   - optional context
> - Throw only structured errors from the parser
>
> Tests must assert on error codes and positions.

### VERIFICATION
- Negative fixtures assert error code + location
- Error messages are stable and human-readable

---

# TASK 9 — Explicit Rejection of Unsupported Features

### LLM PROMPT
> Detect and explicitly reject unsupported COBOL features instead of misparsing them.
>
> At minimum, reject:
> - OCCURS DEPENDING ON
> - RENAMES
> - Unsupported USAGE types
> - Unsupported continuation forms
>
> Errors must be precise and location-aware.

### VERIFICATION
- Unsupported constructs produce `UNSUPPORTED_FEATURE` errors
- No silent acceptance of unsupported syntax

---

# TASK 10 — Public Parser API

### LLM PROMPT
> Create a single public entrypoint for the copybook parser.
>
> Requirements:
> - Expose a method such as:
>
> ```java
> CopybookAst parse(
>   String copybookName,
>   Reader source,
>   CopybookResolver resolver,
>   ParserOptions options
> )
> ```
>
> - Options should include:
>   - COPY expansion toggle
>   - strict vs permissive mode
>   - source position tracking
>
> Tests must use only this entrypoint.

### VERIFICATION
- No tests rely on internal parser classes
- Downstream modules can consume AST cleanly

---

## FINAL ACCEPTANCE CHECKLIST

- [ ] All parser fixtures are automated
- [ ] Level-88 fully supported
- [ ] COPY REPLACING parsed and expanded
- [ ] Errors are structured and deterministic
- [ ] AST output is stable
- [ ] `mvn -q -pl copybook-parser test` passes

---

**IMPORTANT:**  
Do NOT modify `copybook-model`, `copybook-codegen`, or runtime modules
until all tasks above are complete and verified.
