# Test Fixtures Pack — CICS Copybook Integration Framework

This ZIP is designed to support running **specific subsets** of tests by folder.

## Folder conventions
- `01-parser/` : copybooks + expected AST snapshots (and negative cases)
- `02-layout/`  : AST inputs + expected LayoutModel snapshots (and negative cases)
- `03-codegen/` : LayoutModel inputs + DTO samples + golden bytes (and negative cases)
- `04-runtime/` : TransactionSpecs + container maps + expected MainframeResult envelopes
- `05-e2e/`     : full vertical-slice scenarios (copybooks + TransactionSpec + DTO + bytes)

Each category is further split into:
- `positive/` : should succeed
- `negative/` : should fail fast with a clear error

## How to use
1. Copy the desired scenario folder into the module’s test resources.
2. Point your tests at that folder and run only that suite.
   - Example Maven pattern: `-Dtest=*Parser*` (depending on your test class names)

## Notes
- Files ending in `.expected.json` are snapshots (AST or LayoutModel).
- Files ending in `.expected.err.txt` are the error-message expectations.
- Golden byte files are stored as hex (`.hex`) to stay diff-friendly.
