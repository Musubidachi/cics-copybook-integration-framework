# copybook-parser

## Overview

`copybook-parser` is responsible for parsing COBOL copybooks into a **stable, lossless Abstract Syntax Tree (AST)** suitable for downstream layout computation and code generation.

This module **does not** compute byte offsets, expand layouts, or generate Java code. Its sole responsibility is to **faithfully represent the logical structure of a copybook**, including hierarchy, data descriptions, and structural relationships such as `OCCURS` and `REDEFINES`.

The AST produced by this module is treated as a **semantic contract**: later stages depend on its determinism and completeness.

---

## Responsibilities

### Owns
- Copybook lexical analysis and parsing
- Construction of `CopybookAst`
- Structural correctness and hierarchy preservation

### Does *Not* Own
- COPY resolution (may be injected or handled externally)
- Byte layout computation
- Java generation
- Runtime encoding/decoding

---

## Inputs

- COBOL copybook source text
- COPY statements may already be resolved **or**
- A resolver API may be provided externally

```text
Input → String (copybook text)
```

---

## Outputs

- `CopybookAst`

The AST preserves:
- Declaration order
- Hierarchical relationships
- Raw semantic attributes (PIC, USAGE, OCCURS, REDEFINES)
- Source-level intent without layout interpretation

---

## Supported COBOL Constructs (Initial Scope)

### Structural Elements
- Group items
- Elementary items
- Level numbers (01–49, optional 66/77)

### Clauses
- `PIC` / `PICTURE`
- `USAGE` (DISPLAY, COMP, COMP-3, etc.)
- Fixed `OCCURS`
- `REDEFINES`

### COPY
- Minimal detection of `COPY` statements
- No expansion logic required

---

## AST Model (Conceptual)

```text
CopybookAst
 └── DataItemNode
     ├── level
     ├── name
     ├── pic
     ├── usage
     ├── occurs (optional)
     ├── redefines (optional)
     ├── children[]
```

Notes:
- `REDEFINES` are represented as explicit overlay links
- `OCCURS` is represented structurally, not expanded

---

## Small Tasks (1–2 Hours Each)

1. Tokenizer & normalization (columns, comments, continuation)
2. Parse group and elementary items
3. Parse fixed OCCURS
4. Parse REDEFINES overlay links
5. Detect COPY statements
6. Unit tests using real copybooks

---

## Testing Strategy

- Snapshot-based AST tests
- One copybook per test case
- Include nested groups, OCCURS, REDEFINES, mixed USAGE types

---

## Definition of Done

- Parses representative copybooks
- Correct OCCURS and REDEFINES representation
- Deterministic AST output
- Snapshot tests passing
- No layout or encoding assumptions

---

## Design Principles

- Determinism over cleverness
- Lossless parsing
- Explicit structure
- AST is a contract

---

## Position in the Pipeline

```text
COPYBOOK TEXT
      ↓
copybook-parser
      ↓
CopybookAst
      ↓
layout engine
      ↓
codec generation
```
