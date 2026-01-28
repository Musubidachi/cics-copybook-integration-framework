package com.mainframe.codegen.encoding;

import java.util.List;

/**
 * Thrown when more than one overlay member is set during encoding.
 *
 * <p>When encoding a record with REDEFINES overlay groups, at most one
 * member of each overlay group may be non-null. This exception is thrown
 * if multiple members are found to be set.</p>
 */
public final class OverlayViolationException extends CodecException {

    private final List<String> setMembers;

    /**
     * Constructs a new OverlayViolationException.
     *
     * @param recordName the name of the record being processed
     * @param fieldPath  the base path of the overlay group
     * @param offset     the byte offset of the overlay region
     * @param length     the byte length of the overlay region
     * @param setMembers the list of member paths that were set
     */
    public OverlayViolationException(String recordName, String fieldPath, int offset, int length,
                                     List<String> setMembers) {
        super(String.format("Multiple overlay members set: %s", setMembers),
                recordName, fieldPath, offset, length);
        this.setMembers = List.copyOf(setMembers);
    }

    /**
     * @return an unmodifiable list of the member paths that were set
     */
    public List<String> getSetMembers() {
        return setMembers;
    }
}
