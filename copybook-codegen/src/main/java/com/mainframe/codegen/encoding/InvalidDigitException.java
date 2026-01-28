package com.mainframe.codegen.encoding;

/**
 * Thrown when an invalid digit is encountered during decoding.
 *
 * <p>This exception is raised when a byte that should represent
 * a digit (0-9) contains an invalid value in the digit nibble(s).</p>
 */
public final class InvalidDigitException extends CodecException {

    private final byte[] offendingBytes;
    private final int invalidByteOffset;

    /**
     * Constructs a new InvalidDigitException.
     *
     * @param recordName        the name of the record being processed
     * @param fieldPath         the path of the field within the record
     * @param offset            the byte offset of the field
     * @param length            the byte length of the field
     * @param offendingBytes    the bytes containing the invalid digit
     * @param invalidByteOffset the offset within the field of the invalid byte
     */
    public InvalidDigitException(String recordName, String fieldPath, int offset, int length,
                                 byte[] offendingBytes, int invalidByteOffset) {
        super(String.format("Invalid digit at byte offset %d, bytes: %s",
                invalidByteOffset, bytesToHex(offendingBytes)),
                recordName, fieldPath, offset, length);
        this.offendingBytes = offendingBytes != null ? offendingBytes.clone() : null;
        this.invalidByteOffset = invalidByteOffset;
    }

    /**
     * @return a copy of the offending bytes
     */
    public byte[] getOffendingBytes() {
        return offendingBytes != null ? offendingBytes.clone() : null;
    }

    /**
     * @return the offset within the field of the invalid byte
     */
    public int getInvalidByteOffset() {
        return invalidByteOffset;
    }
}
