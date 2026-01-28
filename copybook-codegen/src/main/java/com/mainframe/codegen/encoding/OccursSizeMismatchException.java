package com.mainframe.codegen.encoding;

/**
 * Thrown when a list size does not match the expected OCCURS count.
 *
 * <p>For fixed-size OCCURS arrays, the DTO list must contain exactly
 * the specified number of elements during encoding.</p>
 */
public final class OccursSizeMismatchException extends CodecException {

    private final int expectedSize;
    private final int actualSize;

    /**
     * Constructs a new OccursSizeMismatchException.
     *
     * @param recordName   the name of the record being processed
     * @param fieldPath    the path of the OCCURS array field
     * @param offset       the byte offset of the array start
     * @param length       the total byte length of the array region
     * @param expectedSize the expected number of elements (from OCCURS clause)
     * @param actualSize   the actual number of elements in the list
     */
    public OccursSizeMismatchException(String recordName, String fieldPath, int offset, int length,
                                       int expectedSize, int actualSize) {
        super(String.format("OCCURS size mismatch: expected %d elements, got %d",
                expectedSize, actualSize),
                recordName, fieldPath, offset, length);
        this.expectedSize = expectedSize;
        this.actualSize = actualSize;
    }

    /**
     * @return the expected number of elements
     */
    public int getExpectedSize() {
        return expectedSize;
    }

    /**
     * @return the actual number of elements provided
     */
    public int getActualSize() {
        return actualSize;
    }
}
