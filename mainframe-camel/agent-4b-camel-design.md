# Agent 4B Design Document
Camel Route for Red Hat CICS Integration

## 1. Purpose & Scope

Agent 4B owns the Apache Camel route layer responsible for synchronous invocation of CICS programs via the Red Hat CICS component using ECI channels and containers over IPIC.

This agent is intentionally transport-only:
- No copybook parsing
- No layout enforcement
- No DTO construction or decoding
- No business validation

All semantic interpretation is delegated to generated code and the runtime layer.

---

## 2. Owned Module

**Module:** mainframe-camel

### Responsibility Boundary

| Concern | Owned |
|------|------|
| Camel routing | Yes |
| CICS component invocation | Yes |
| Container map conversion | Yes |
| Exception → failure envelope mapping | Yes |
| Copybook layout | No |
| Serialization / deserialization | No |
| Domain construction | No |

---

## 3. Route Contract

### Entry Point
direct:cicsInvoke

### Inputs
- Map<String, byte[]> (canonical container map)
- Headers:
  - spec-id
  - CICS metadata (program, channel, etc.)

### Outputs
- Map<String, byte[]> (raw output containers)

---

## 4. Canonical Container Model

Agent 4B operates exclusively on:

Map<String, byte[]>

Rules:
- Keys are container names
- Values are exact byte payloads
- No payload mutation
- No semantic interpretation

---

## 5. Conversion Layer

### Input Conversion
- Translate canonical map to CICS component container structure
- Preserve names, byte order, and length

### Output Conversion
- Extract returned containers
- Normalize back to Map<String, byte[]>
- Preserve byte-level fidelity

---

## 6. Exception Mapping Strategy

All exceptions are mapped into a consistent failure envelope.

| Source | Handling |
|------|---------|
| CICS error | Structured failure |
| Component error | Wrapped failure |
| Timeout | Transport failure |
| Unexpected | Generic failure |

No raw exceptions escape the route.

---

## 7. Route Flow

direct:cicsInvoke  
→ validate headers  
→ adapt container map  
→ invoke CICS component  
→ normalize output  
→ return container map

---

## 8. Testing Strategy

- Camel route tests with mocks
- No live CICS dependency
- Byte-for-byte assertions

---

## 9. Definition of Done

- End-to-end test:
  DTO → container map → Camel route (mock) → returned container map → parsed MainframeResult<T>
- Deterministic behavior
- Transport-only responsibility

---

## 10. Design Principles

- Transport-only concerns
- Deterministic execution
- No semantic coupling
- Symmetry with runtime envelope
