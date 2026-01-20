package com.mainframe.model;

/**
 * Summarises a COBOL PIC clause in terms of the number of digits,
 * the scale (number of decimal places) and whether a sign is
 * explicitly required.  Parsing of raw PIC strings into this
 * structure is expected to occur in the parser module; the model
 * simply uses this abstraction to compute field sizes.
 *
 * <p>For example, the PIC clause <code>9(5)V99</code> would result
 * in a {@code PicSummary} with {@code digits = 7},
 * {@code scale = 2} and {@code signed = false}.  The
 * virtual decimal point contributes to scale but not to the
 * physical byte count for DISPLAY and COMP‑3 usages.</p>
 */
public record PicSummary(int digits, int scale, boolean signed) {

    /**
     * Computes the number of bytes required to store a field with
     * this PIC summary when using the supplied usage.
     *
     * @param usage storage type (DISPLAY, COMP, COMP3)
     * @return number of bytes consumed by this field
     */
    public int byteLength(Usage usage) {
        switch (usage) {
            case DISPLAY -> {
                // Each digit occupies a byte; the sign (if any) is
                // represented as a separate byte in DISPLAY fields.  A
                // virtual decimal point does not consume space.
                int length = digits;
                if (signed) {
                    length += 1;
                }
                return length;
            }
            case COMP -> {
                // IBM binary.  Only the total number of digits matters.
                if (digits <= 4) {
                    return 2;
                }
                if (digits <= 9) {
                    return 4;
                }
                return 8;
            }
            case COMP3 -> {
                // Packed decimal.  The total number of nibbles is
                // digits plus one sign nibble.  Each byte contains
                // two nibbles.
                int nibbles = digits + 1;
                return (nibbles + 1) / 2;
            }
            default -> throw new IllegalArgumentException("Unsupported usage: " + usage);
        }
    }
}