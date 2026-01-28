package com.mainframe.codegen.encoding;

/**
 * Thrown when an invalid sign nibble is encountered during COMP-3 decoding.
 *
 * <p>For packed decimal (COMP-3), the last nibble must contain a valid
 * sign indicator: 0x0C or 0x0F for positive, 0x0D or 0x0B for negative.</p>
 */
public final class InvalidSignNibbleException extends CodecException {

    private final byte[] offendingBytes;
    private final int signNibble;

    /**
     * Constructs a new InvalidSignNibbleException.
     *
     * @param recordName     the name of the record being processed
     * @param fieldPath      the path of the field within the record
     * @param offset         the byte offset of the field
     * @param length         the byte length of the field
     * @param offendingBytes the bytes containing the invalid sign
     * @param signNibble     the invalid sign nibble value (0x00-0x0F)
     */
    public InvalidSignNibbleException(String recordName, String fieldPath, int offset, int length,
                                      byte[] offendingBytes, int signNibble) {
        super(String.format("Invalid sign nibble 0x%X in COMP-3 field, bytes: %s",
                signNibble, bytesToHex(offendingBytes)),
                recordName, fieldPath, offset, length);
        this.offendingBytes = offendingBytes != null ? offendingBytes.clone() : null;
        this.signNibble = signNibble;
    }

    /**
     * @return a copy of the offending bytes
     */
    public byte[] getOffendingBytes() {
        return offendingBytes != null ? offendingBytes.clone() : null;
    }

    /**
     * @return the invalid sign nibble value
     */
    public int getSignNibble() {
        return signNibble;
    }
}
