package com.mainframe.codegen.encoding;

import com.mainframe.model.PicSummary;
import com.mainframe.model.Usage;

/**
 * Calculates byte lengths for COBOL fields based on encoding configuration.
 *
 * <p>This utility provides byte length calculations that account for different
 * sign encoding modes. The standard {@link PicSummary#byteLength(Usage)} method
 * assumes separate sign encoding for DISPLAY fields, which adds an extra byte.
 * For trailing overpunch encoding, the sign is embedded in the last digit and
 * does NOT add an extra byte.</p>
 *
 * <p><strong>DISPLAY numeric byte length:</strong></p>
 * <ul>
 *   <li>Separate sign (traditional): digits + 1 for signed fields</li>
 *   <li>Trailing overpunch: digits only (sign embedded in last digit zone)</li>
 *   <li>Unsigned: digits only</li>
 * </ul>
 */
public final class ByteLengthCalculator {

    private ByteLengthCalculator() {
        // utility class
    }

    /**
     * Calculate the byte length for a DISPLAY numeric field with the given sign mode.
     *
     * @param digits   the total number of digits
     * @param signed   true if the field is signed
     * @param signMode the sign encoding mode
     * @return the byte length required
     */
    public static int displayNumericByteLength(int digits, boolean signed, DisplaySignMode signMode) {
        if (!signed) {
            return digits;
        }

        switch (signMode) {
            case OVERPUNCH_TRAILING:
                // Sign is embedded in last digit zone - no extra byte
                return digits;
            case UNSIGNED:
                // Treat as unsigned
                return digits;
            default:
                throw new IllegalArgumentException("Unsupported sign mode: " + signMode);
        }
    }

    /**
     * Calculate the byte length using traditional DISPLAY rules (separate sign byte).
     * This matches the behavior of {@link PicSummary#byteLength(Usage)} for DISPLAY.
     *
     * @param digits the total number of digits
     * @param signed true if the field is signed
     * @return the byte length with separate sign byte for signed fields
     */
    public static int displayNumericByteLengthSeparateSign(int digits, boolean signed) {
        return signed ? digits + 1 : digits;
    }

    /**
     * Calculate the byte length for a COMP (binary) field.
     *
     * @param digits the total number of digits
     * @return the byte length (2, 4, or 8 bytes)
     */
    public static int compByteLength(int digits) {
        if (digits <= 4) {
            return 2;
        }
        if (digits <= 9) {
            return 4;
        }
        return 8;
    }

    /**
     * Calculate the byte length for a COMP-3 (packed decimal) field.
     *
     * @param digits the total number of digits
     * @return the byte length (each byte holds 2 nibbles, plus one nibble for sign)
     */
    public static int comp3ByteLength(int digits) {
        // Total nibbles = digits + 1 sign nibble
        int nibbles = digits + 1;
        // Each byte holds 2 nibbles, round up
        return (nibbles + 1) / 2;
    }

    /**
     * Validate that a field's declared length matches the expected length
     * for the given encoding configuration.
     *
     * @param declaredLength the length declared in the layout
     * @param digits         the number of digits
     * @param signed         true if the field is signed
     * @param usage          the field usage (DISPLAY, COMP, COMP3)
     * @param signMode       the sign mode for DISPLAY fields
     * @return true if the length matches, false otherwise
     */
    public static boolean validateLength(int declaredLength, int digits, boolean signed,
                                         Usage usage, DisplaySignMode signMode) {
        int expectedLength;
        switch (usage) {
            case DISPLAY:
                expectedLength = displayNumericByteLength(digits, signed, signMode);
                break;
            case COMP:
                expectedLength = compByteLength(digits);
                break;
            case COMP3:
                expectedLength = comp3ByteLength(digits);
                break;
            default:
                return false;
        }
        return declaredLength == expectedLength;
    }

    /**
     * Get the expected length for a field with the given encoding configuration.
     *
     * @param digits   the number of digits
     * @param signed   true if the field is signed
     * @param usage    the field usage
     * @param signMode the sign mode for DISPLAY fields
     * @return the expected byte length
     */
    public static int expectedLength(int digits, boolean signed, Usage usage, DisplaySignMode signMode) {
        switch (usage) {
            case DISPLAY:
                return displayNumericByteLength(digits, signed, signMode);
            case COMP:
                return compByteLength(digits);
            case COMP3:
                return comp3ByteLength(digits);
            default:
                throw new IllegalArgumentException("Unsupported usage: " + usage);
        }
    }
}
