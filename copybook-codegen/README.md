# copybook-codegen

## Overview

`copybook-codegen` generates **deterministic Java DTOs and byte-level codecs**
from an authoritative `LayoutModel`, optionally constrained by a `TransactionSpec`.

This module produces:
- Strongly typed Java domain objects (DTOs)
- Explicit pack/unpack codecs that serialize and deserialize fixed-length records
- No reflection
- No runtime inference
- No layout computation

All generated code operates strictly at the **byte level** and treats the
`LayoutModel` as immutable truth.

---

## Responsibilities

### Owns
- Java DTO generation
- OCCURS array modeling
- REDEFINES union-style views
- Byte-level pack/unpack codec generation
- Numeric encoding/decoding (DISPLAY, COMP, COMP-3)
- String encoding and padding policy hooks
- Round-trip and golden-byte testing

### Does *Not* Own
- Copybook parsing
- Layout computation
- Offset or length inference
- Runtime transport (Camel, CICS, IPIC)
- Channel/container orchestration

---

## Inputs

```text
LayoutModel        (required)
TransactionSpec    (optional)
```

---

## Outputs

```text
Generated Java Source
```

---

## Design Principles

- Generated code > handwritten code
- Determinism over convenience
- Byte correctness over abstraction
- No reflection
- No runtime layout decisions

If bytes differ, the code is wrong.

---

## Status

This module is code-generative and deterministic.
