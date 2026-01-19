# copybook-codegen — Authoritative DTO and Codec Generation

## Module Mission

`copybook-codegen` converts an authoritative `LayoutModel` into:
- Strongly typed Java DTOs
- Explicit byte-level pack/unpack codecs

This module does not compute layout.
It consumes layout that is already correct and treats it as immutable law.

---

## Hard Scope Boundaries

### MUST
- Generate Java DTOs mirroring layout
- Generate codecs with explicit byte offsets
- Encode/decode DISPLAY, COMP, COMP-3 numerics
- Model OCCURS as fixed arrays
- Model REDEFINES as union overlays
- Generate golden-byte and round-trip tests

### MUST NOT
- Infer layout
- Use reflection
- Perform runtime discovery
- Modify offsets or lengths

---

## Definition of Done

For one transaction:
- DTOs compile
- Codec compiles
- Known DTO packs into expected bytes
- Expected bytes unpack into expected DTO
- Golden tests pass

If any byte differs, the module is incomplete.
