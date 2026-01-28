package com.mainframe.codegen.encoding;

/**
 * Thrown when a numeric value exceeds the capacity of its field.
 *
 * <p>This exception is raised during encoding when a value cannot
 * be represented within the allocated byte length, or when the
 * number of digits exceeds the PIC definition.</p>
 */
public final class OverflowException extends CodecException {

    private final String offendingValue;

    /**
     * Constructs a new OverflowException.
     *
     * @param recordName     the name of the record being processed
     * @param fieldPath      the path of the field within the record
     * @param offset         the byte offset of the field
     * @param length         the byte length of the field
     * @param offendingValue the value that caused the overflow
     */
    public OverflowException(String recordName, String fieldPath, int offset, int length, String offendingValue) {
        super(String.format("Value '%s' overflows field capacity", truncateValue(offendingValue)),
                recordName, fieldPath, offset, length);
        this.offendingValue = offendingValue;
    }

    /**
     * Constructs a new OverflowException with max digits info.
     *
     * @param recordName     the name of the record being processed
     * @param fieldPath      the path of the field within the record
     * @param offset         the byte offset of the field
     * @param length         the byte length of the field
     * @param offendingValue the value that caused the overflow
     * @param maxDigits      the maximum number of digits allowed
     */
    public OverflowException(String recordName, String fieldPath, int offset, int length, String offendingValue, int maxDigits) {
        super(String.format("Value '%s' overflows field capacity (max %d digits)",
                truncateValue(offendingValue), maxDigits),
                recordName, fieldPath, offset, length);
        this.offendingValue = offendingValue;
    }

    private static String truncateValue(String value) {
        if (value == null) {
            return "null";
        }
        if (value.length() <= 64) {
            return value;
        }
        return value.substring(0, 64) + "...(truncated)";
    }

    /**
     * @return the value that caused the overflow
     */
    public String getOffendingValue() {
        return offendingValue;
    }
}
