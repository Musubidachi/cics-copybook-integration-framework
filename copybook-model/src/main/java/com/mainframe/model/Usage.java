package com.mainframe.model;

/**
 * Enumerates the supported storage types for COBOL data items.
 *
 * <p>The layout engine uses these values to determine how many bytes
 * a numeric field consumes on the mainframe.  The rules for each
 * usage are defined in {@link PicSummary} and the layout builder.
 *
 * <p>Only DISPLAY, COMP (binary) and COMP‑3 (packed decimal) are
 * presently supported.  Additional usages can be added here as the
 * framework grows.  Unknown or unsupported usages should result
 * in a validation error during layout computation rather than
 * guessing.</p>
 */
public enum Usage {
    /**
     * Standard character or numeric display format.  Each digit
     * occupies a single byte.  A sign, if present, occupies its own
     * byte.  Virtual decimal points (V) do not consume space.
     */
    DISPLAY,

    /**
     * IBM binary storage format (COMP).  The number of bytes is
     * determined by the total number of digits.  1–4 digits → 2
     * bytes, 5–9 digits → 4 bytes, 10–18 digits → 8 bytes.
     */
    COMP,

    /**
     * Packed decimal (COMP‑3).  The number of bytes is the
     * ceiling of ((digits + 1) / 2).  A sign nibble is always
     * included.  Virtual decimal points are ignored.
     */
    COMP3
}