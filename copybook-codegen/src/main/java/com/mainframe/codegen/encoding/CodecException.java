package com.mainframe.codegen.encoding;

/**
 * Base exception for all codec-related errors during encoding or decoding
 * of COBOL data fields.
 *
 * <p>All subclasses include contextual information such as record name,
 * field path, offset, and length to aid in debugging and error reporting.
 */
public abstract class CodecException extends RuntimeException {

    private final String recordName;
    private final String fieldPath;
    private final int offset;
    private final int length;

    /**
     * Constructs a new CodecException with full context.
     *
     * @param message    the detail message
     * @param recordName the name of the record being processed
     * @param fieldPath  the path of the field within the record
     * @param offset     the byte offset of the field
     * @param length     the byte length of the field
     */
    protected CodecException(String message, String recordName, String fieldPath, int offset, int length) {
        super(formatMessage(message, recordName, fieldPath, offset, length));
        this.recordName = recordName;
        this.fieldPath = fieldPath;
        this.offset = offset;
        this.length = length;
    }

    /**
     * Constructs a new CodecException with a cause.
     *
     * @param message    the detail message
     * @param cause      the underlying cause
     * @param recordName the name of the record being processed
     * @param fieldPath  the path of the field within the record
     * @param offset     the byte offset of the field
     * @param length     the byte length of the field
     */
    protected CodecException(String message, Throwable cause, String recordName, String fieldPath, int offset, int length) {
        super(formatMessage(message, recordName, fieldPath, offset, length), cause);
        this.recordName = recordName;
        this.fieldPath = fieldPath;
        this.offset = offset;
        this.length = length;
    }

    private static String formatMessage(String message, String recordName, String fieldPath, int offset, int length) {
        return String.format("%s [record=%s, field=%s, offset=%d, length=%d]",
                message, recordName, fieldPath, offset, length);
    }

    /**
     * @return the name of the record being processed
     */
    public String getRecordName() {
        return recordName;
    }

    /**
     * @return the path of the field within the record
     */
    public String getFieldPath() {
        return fieldPath;
    }

    /**
     * @return the byte offset of the field
     */
    public int getOffset() {
        return offset;
    }

    /**
     * @return the byte length of the field
     */
    public int getLength() {
        return length;
    }

    /**
     * Converts a byte array to a hex string for error reporting.
     * Truncates output to 64 bytes maximum.
     *
     * @param bytes the bytes to format
     * @return hex string representation
     */
    protected static String bytesToHex(byte[] bytes) {
        if (bytes == null) {
            return "null";
        }
        int limit = Math.min(bytes.length, 64);
        StringBuilder sb = new StringBuilder(limit * 2);
        for (int i = 0; i < limit; i++) {
            sb.append(String.format("%02X", bytes[i] & 0xFF));
        }
        if (bytes.length > 64) {
            sb.append("...(truncated)");
        }
        return sb.toString();
    }
}
