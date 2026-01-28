package com.mainframe.codegen.encoding;

/**
 * Specifies how alphanumeric fields are trimmed during decoding.
 */
public enum AlphaTrimMode {

    /**
     * No trimming - return the full fixed-length string as-is.
     */
    NONE,

    /**
     * Right-trim EBCDIC spaces (0x40) from alphanumeric fields.
     * This is the default and most common behavior.
     */
    RTRIM_EBCDIC_SPACE
}
