package com.mainframe.codegen.encoding;

/**
 * Specifies the byte order for COMP (binary) fields.
 *
 * <p>IBM mainframes use big-endian byte order, which is the default.</p>
 */
public enum CompBinaryEndianness {

    /**
     * Big-endian byte order (most significant byte first).
     * This is the standard for IBM mainframes.
     */
    BIG_ENDIAN,

    /**
     * Little-endian byte order (least significant byte first).
     * Used by Intel/AMD architectures.
     */
    LITTLE_ENDIAN
}
