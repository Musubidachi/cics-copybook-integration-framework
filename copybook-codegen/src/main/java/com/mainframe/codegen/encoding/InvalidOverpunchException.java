package com.mainframe.codegen.encoding;

/**
 * Thrown when an invalid overpunch byte is encountered during DISPLAY
 * signed numeric decoding.
 *
 * <p>For zoned decimal with trailing overpunch, the last byte must be
 * a valid overpunch code representing both the sign and the last digit.
 * For IBM037:
 * <ul>
 *   <li>Positive 0-9: 0xC0-0xC9 or 0xF0-0xF9</li>
 *   <li>Negative 0-9: 0xD0-0xD9</li>
 * </ul>
 */
public final class InvalidOverpunchException extends CodecException {

    private final byte[] offendingBytes;
    private final int invalidByte;

    /**
     * Constructs a new InvalidOverpunchException.
     *
     * @param recordName     the name of the record being processed
     * @param fieldPath      the path of the field within the record
     * @param offset         the byte offset of the field
     * @param length         the byte length of the field
     * @param offendingBytes the bytes containing the invalid overpunch
     * @param invalidByte    the invalid overpunch byte value
     */
    public InvalidOverpunchException(String recordName, String fieldPath, int offset, int length,
                                     byte[] offendingBytes, int invalidByte) {
        super(String.format("Invalid overpunch byte 0x%02X in signed DISPLAY field, bytes: %s",
                invalidByte & 0xFF, bytesToHex(offendingBytes)),
                recordName, fieldPath, offset, length);
        this.offendingBytes = offendingBytes != null ? offendingBytes.clone() : null;
        this.invalidByte = invalidByte & 0xFF;
    }

    /**
     * @return a copy of the offending bytes
     */
    public byte[] getOffendingBytes() {
        return offendingBytes != null ? offendingBytes.clone() : null;
    }

    /**
     * @return the invalid overpunch byte value (0x00-0xFF)
     */
    public int getInvalidByte() {
        return invalidByte;
    }
}
