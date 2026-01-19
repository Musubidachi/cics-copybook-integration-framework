# copybook-model

## Overview

`copybook-model` computes the **authoritative byte layout** for COBOL copybooks.

It converts a parsed `CopybookAst` into a deterministic `LayoutModel` containing:
- absolute offsets
- field lengths
- OCCURS expansion
- REDEFINES overlays

This module is a **pure function** and enforces non-negotiable rules.

---

## Responsibilities

### Owns
- Byte length computation
- Offset calculation
- OCCURS expansion
- REDEFINES overlay construction
- Layout validation

### Does *Not* Own
- Parsing
- Java generation
- Encoding/decoding
- Runtime behavior

---

## Inputs

```text
CopybookAst
```

---

## Outputs

```text
LayoutModel
```

Fully flattened and immutable.

---

## Design Principles

- Determinism over flexibility
- Explicit layout over inference
- Fail fast on ambiguity
- Layout is a contract

---

## Testing

- Golden layout snapshots
- OCCURS and REDEFINES cases
- Numeric sizing validation

Any change in layout is a breaking change.

---

## Pipeline Position

```text
copybook-parser → copybook-model → codec generation
```

---

## Status

This module is foundational.
Treat its output as law.
