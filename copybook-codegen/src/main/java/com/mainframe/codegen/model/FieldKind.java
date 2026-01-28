package com.mainframe.codegen.model;

/**
 * Enumeration of field kinds for code generation.
 *
 * <p>Each kind corresponds to a specific encoding type in COBOL:</p>
 * <ul>
 *   <li>ALPHANUMERIC - character strings (PIC X, PIC A)</li>
 *   <li>DISPLAY_NUMERIC - zoned decimal (PIC 9 with DISPLAY usage)</li>
 *   <li>COMP - binary integer (COMP, COMP-4, BINARY)</li>
 *   <li>COMP3 - packed decimal (COMP-3)</li>
 * </ul>
 */
public enum FieldKind {
    /**
     * Alphanumeric field (character string).
     */
    ALPHANUMERIC,

    /**
     * Display numeric field (zoned decimal).
     */
    DISPLAY_NUMERIC,

    /**
     * Binary integer field (COMP).
     */
    COMP,

    /**
     * Packed decimal field (COMP-3).
     */
    COMP3
}
