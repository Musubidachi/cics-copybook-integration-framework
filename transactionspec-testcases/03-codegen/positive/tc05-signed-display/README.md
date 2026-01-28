# TC05: Signed DISPLAY Numeric with Trailing Overpunch

This test case validates the encoding and decoding of signed DISPLAY numeric
fields using trailing overpunch sign representation (IBM037).

## Copybook Definition

```cobol
       01  SIGNED-REC.
           05  BALANCE        PIC S9(5).
           05  AMOUNT         PIC S9(3)V99.
           05  CODE           PIC 9(3).
```

## Field Specifications

| Field   | PIC        | Offset | Length | Type              | Signed | Scale |
|---------|------------|--------|--------|-------------------|--------|-------|
| BALANCE | S9(5)      | 0      | 5      | DISPLAY_NUMERIC   | true   | 0     |
| AMOUNT  | S9(3)V99   | 5      | 5      | DISPLAY_NUMERIC   | true   | 2     |
| CODE    | 9(3)       | 10     | 3      | DISPLAY_NUMERIC   | false  | 0     |

**Note:** For signed DISPLAY with overpunch, the field length equals the number
of digits. The sign is encoded in the zone nibble of the last digit byte, NOT
as a separate byte.

## Test Values

| Field   | Value     | Encoded Bytes (hex) |
|---------|-----------|---------------------|
| BALANCE | +12345    | F1 F2 F3 F4 C5      |
| AMOUNT  | -123.45   | F1 F2 F3 F4 D5      |
| CODE    | 007       | F0 F0 F7            |

## Overpunch Reference (IBM037)

### Trailing Overpunch Encoding

| Digit | Positive (C) | Negative (D) | Unsigned (F) |
|-------|--------------|--------------|--------------|
| 0     | C0           | D0           | F0           |
| 1     | C1           | D1           | F1           |
| 2     | C2           | D2           | F2           |
| 3     | C3           | D3           | F3           |
| 4     | C4           | D4           | F4           |
| 5     | C5           | D5           | F5           |
| 6     | C6           | D6           | F6           |
| 7     | C7           | D7           | F7           |
| 8     | C8           | D8           | F8           |
| 9     | C9           | D9           | F9           |
