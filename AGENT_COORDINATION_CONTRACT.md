# CICS Copybook Integration Framework — Agent Coordination Contract (Single Complete Version)

This document is the **single source of truth** for how agents collaborate on the framework.
Parallel work is only possible if **constraints are enforced**.

---

## Core Constraint (Non‑Negotiable)

> **Each agent must own exactly one module with:**
> - a clear API boundary (interfaces + input/output types)
> - fixtures/tests that lock behavior at that boundary
> - **zero creative freedom outside its module**

Violating this leads to:
- divergent naming conventions  
- incompatible type systems  
- inconsistent error handling  
- endless merge conflicts  

This document exists to prevent that.

---

## Agent 0 — System Integrator / Architect

**Role:** Guardian of global truth and integration contracts.

### Owns (authoritative):
- **TransactionSpec schema** (YAML/JSON)
- **Canonical transport domain:** `Map<String, byte[]>`
- **Result & error model:** `MainframeResult<T>`
- Naming conventions + package layout
- Module‑to‑module integration contracts

### Responsibilities
- No agent invents schemas, container names, or envelopes.
- Any cross‑module change goes through Agent 0.
- Maintains `/docs/architecture.md` and `/docs/contracts.md`.

---

## Agent 1 — Copybook Parser → AST

**Module:** `copybook-parser`

### Inputs
- Copybook text
- COPY resolver API (does *not* compute layout)

### Outputs
- `CopybookAst` (lossless structural representation)

### Responsibilities
- Tokenization + line normalization (columns, comments, continuation)
- Parse:
  - group & elementary items (level, name)
  - PIC clauses
  - USAGE
  - OCCURS (fixed)
  - REDEFINES
  - **COPY statements (including REPLACING clauses)**
  - **Level‑88 condition names**
- Preserve structure exactly as written

### Must NOT
- Compute offsets, lengths, encodings
- Resolve semantics

### Definition of Done
- Representative copybooks parse into stable AST
- Snapshot tests pass
- COPY REPLACING and 88s appear explicitly in AST

---

## Agent 2 — Layout Computation (AST → LayoutModel)

**Module:** `copybook-layout` (or `copybook-model`)

### Inputs
- `CopybookAst`

### Outputs
- `LayoutModel`
  - flattened leaf fields
  - offsets & lengths
  - storage type
  - OCCURS stride
  - REDEFINES overlay groups
  - condition-name (88) mappings

### Responsibilities
- Compute leaf sizes from PIC + USAGE
- Walk AST deterministically to compute offsets
- Apply OCCURS expansion
- Build redefine overlays (same byte range, multiple views)
- Bind 88‑level condition values to parent fields
- Produce layout report for debugging

### Must NOT
- Generate Java
- Encode/decode bytes

### Definition of Done
- Golden tests validate:
  - total record length
  - critical field offsets
  - redefine overlays
  - OCCURS stride correctness

---

## Agent 3 — Code Generation (DTOs + Codecs)

**Module:** `copybook-codegen`

### Inputs
- `LayoutModel`
- `TransactionSpec` (optional)

### Outputs
Generated Java:
- DTO classes
- pack/unpack codecs

### Responsibilities
- Deterministic naming + type mapping
- PIC mapping:
  - `X` → `String`
  - COMP → `long/int`
  - COMP‑3 → `BigDecimal`
- OCCURS → arrays/lists
- REDEFINES → union‑style views
- 88‑levels → boolean getters/setters
- Implement:
  - IBM037 (EBCDIC) string encoding
  - padding + trimming policy hooks
  - COMP / COMP‑3 encode & decode
- Golden byte + round‑trip tests

### Must NOT
- Interpret copybook semantics beyond LayoutModel

### Definition of Done
- Known DTO packs to expected bytes
- Bytes unpack into expected DTO
- Round‑trip byte equality holds

---

## Agent 4 — Runtime + Camel Adapter

### 4A — Runtime Container Mapping

**Module:** `mainframe-runtime`

#### Inputs
- DTO
- TransactionSpec

#### Outputs
- `Map<String, byte[]>`
- `MainframeResult<T>`

#### Responsibilities
- TransactionSpec registry
- Build container map using generated codecs
- Classify containers (payload / status / diagnostic)
- Validate container lengths
- Decode responses into `MainframeResult<T>`

---

### 4B — Camel Adapter

**Module:** `mainframe-camel`

#### Route Contract
- `direct:cicsInvoke`

#### Responsibilities
- Translate canonical container map ↔ CICS component
- Header mapping
- Exception → failure envelope mapping
- Route tests with mocks

### Definition of Done
- DTO → containers → Camel route (mock) → containers → parsed result works end‑to‑end

---

## Agent 6 — End‑to‑End Verifier

**Role:** Stop‑the‑line authority.

### Responsibilities
- Verify:
  - full compilation
  - generated code compiles
  - end‑to‑end byte correctness
- Run only public APIs (no internals)

### Definition of Done
- Single command (`mvn test` or `mvn verify`) proves:
  - Parser → Layout → Codegen → Runtime → Camel → Result
- CI‑ready
- Fails loudly on any contract violation

---

## Global Encoding Rules (Locked)

- **Charset:** IBM037 (EBCDIC)
- DISPLAY strings padded with EBCDIC space
- COMP: explicit endian (must be tested and locked)
- COMP‑3: explicit sign‑nibble rules (tested)
- Encoding decisions live in one place only

---

## COPY … REPLACING Rules

- Fully supported
- REPLACING applied during layout phase
- AST preserves original + replaced forms
- Nested COPY allowed
- Unsupported REPLACING patterns must fail fast

---

## Level‑88 Semantics

- Parsed as condition-name nodes
- Bound to parent field in LayoutModel
- Generated as boolean accessors in DTO
- Encode/decode uses parent field value only

---

## Stop‑the‑Line Conditions (Agent 6)

- Byte round‑trip mismatch
- AST or LayoutModel drift without snapshot update
- Any module re‑interpreting semantics
- Ad‑hoc container naming
- Encoding not IBM037

---

## Sequencing (Non‑Concurrent Friendly)

1. Agent 0 — contracts & schemas
2. Agent 1 — parser + AST tests
3. Agent 2 — layout + offset tests
4. Agent 3 — codegen + golden bytes
5. Agent 4 — runtime + Camel
6. Agent 6 — end‑to‑end verification
