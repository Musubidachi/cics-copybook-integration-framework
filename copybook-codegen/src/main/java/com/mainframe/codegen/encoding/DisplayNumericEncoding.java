package com.mainframe.codegen.encoding;

/**
 * Specifies the encoding format for DISPLAY numeric fields.
 *
 * <p>COBOL DISPLAY numeric fields store digits as character bytes.
 * On IBM mainframes, EBCDIC zoned decimal is the standard format.</p>
 */
public enum DisplayNumericEncoding {

    /**
     * EBCDIC zoned decimal encoding.
     *
     * <p>Each digit occupies one byte using EBCDIC digit codes:
     * <ul>
     *   <li>Digits 0-9 use bytes 0xF0-0xF9 (IBM037)</li>
     *   <li>For signed fields with trailing overpunch, the last digit
     *       uses modified zone nibble (0xC for positive, 0xD for negative)</li>
     * </ul>
     * The virtual decimal point (V) is implied and does not occupy storage.</p>
     */
    EBCDIC_ZONED
}
