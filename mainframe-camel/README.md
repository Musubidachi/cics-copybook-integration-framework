# mainframe-camel

Apache Camel routing layer for Red Hat CICS integration.

## Overview

mainframe-camel provides the Camel route used to invoke CICS programs via the Red Hat CICS Transaction Gateway using ECI channels and containers over IPIC.

It acts strictly as a transport adapter between runtime-generated container maps and the CICS component.

---

## Responsibilities

- Expose direct:cicsInvoke
- Convert canonical container maps to CICS component format
- Invoke CICS synchronously
- Normalize returned containers
- Map errors into a consistent failure envelope

---

## Non-Responsibilities

- Copybook parsing
- Field encoding/decoding
- DTO construction
- Layout enforcement
- Semantic validation

---

## Route Contract

### Input
Map<String, byte[]>
Headers:
- spec-id
- CICS program metadata

### Output
Map<String, byte[]>

---

## Usage

producerTemplate.requestBodyAndHeaders(
    "direct:cicsInvoke",
    containerMap,
    headers,
    Map.class
);

---

## Error Handling

All failures are intercepted and normalized.
No raw Camel or CICS exceptions are exposed.

---

## Testing

- Camel route tests
- Mocked CICS component
- Deterministic assertions

---

## Design Philosophy

- Camel is infrastructure, not logic
- Explicit, deterministic behavior
- Clear separation from runtime semantics
