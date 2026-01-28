package com.mainframe.codegen.encoding;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Codec for DISPLAY numeric fields using EBCDIC zoned decimal encoding.
 *
 * <p>Zoned decimal encoding stores each digit as a single byte where:
 * <ul>
 *   <li>The high nibble (zone) is 0xF for unsigned digits</li>
 *   <li>The low nibble contains the digit value 0-9</li>
 *   <li>For signed fields with trailing overpunch, the last byte uses
 *       a modified zone nibble to encode the sign</li>
 * </ul>
 *
 * <p><strong>Overpunch encoding (IBM037):</strong>
 * <table border="1">
 *   <tr><th>Digit</th><th>Positive</th><th>Negative</th><th>Unsigned</th></tr>
 *   <tr><td>0</td><td>0xC0 ({)</td><td>0xD0 (})</td><td>0xF0</td></tr>
 *   <tr><td>1</td><td>0xC1 (A)</td><td>0xD1 (J)</td><td>0xF1</td></tr>
 *   <tr><td>2</td><td>0xC2 (B)</td><td>0xD2 (K)</td><td>0xF2</td></tr>
 *   <tr><td>3</td><td>0xC3 (C)</td><td>0xD3 (L)</td><td>0xF3</td></tr>
 *   <tr><td>4</td><td>0xC4 (D)</td><td>0xD4 (M)</td><td>0xF4</td></tr>
 *   <tr><td>5</td><td>0xC5 (E)</td><td>0xD5 (N)</td><td>0xF5</td></tr>
 *   <tr><td>6</td><td>0xC6 (F)</td><td>0xD6 (O)</td><td>0xF6</td></tr>
 *   <tr><td>7</td><td>0xC7 (G)</td><td>0xD7 (P)</td><td>0xF7</td></tr>
 *   <tr><td>8</td><td>0xC8 (H)</td><td>0xD8 (Q)</td><td>0xF8</td></tr>
 *   <tr><td>9</td><td>0xC9 (I)</td><td>0xD9 (R)</td><td>0xF9</td></tr>
 * </table>
 *
 * <p>For overpunch fields, the field length equals the number of digits;
 * the sign does NOT add an extra byte.</p>
 */
public final class ZonedDecimalCodec {

    // Overpunch mapping tables for IBM037
    // Index is digit (0-9), value is the overpunch byte

    /**
     * Positive overpunch bytes for digits 0-9 (zone nibble 0xC).
     */
    private static final byte[] POSITIVE_OVERPUNCH = {
            (byte) 0xC0, // 0 → '{'
            (byte) 0xC1, // 1 → 'A'
            (byte) 0xC2, // 2 → 'B'
            (byte) 0xC3, // 3 → 'C'
            (byte) 0xC4, // 4 → 'D'
            (byte) 0xC5, // 5 → 'E'
            (byte) 0xC6, // 6 → 'F'
            (byte) 0xC7, // 7 → 'G'
            (byte) 0xC8, // 8 → 'H'
            (byte) 0xC9  // 9 → 'I'
    };

    /**
     * Negative overpunch bytes for digits 0-9 (zone nibble 0xD).
     */
    private static final byte[] NEGATIVE_OVERPUNCH = {
            (byte) 0xD0, // 0 → '}'
            (byte) 0xD1, // 1 → 'J'
            (byte) 0xD2, // 2 → 'K'
            (byte) 0xD3, // 3 → 'L'
            (byte) 0xD4, // 4 → 'M'
            (byte) 0xD5, // 5 → 'N'
            (byte) 0xD6, // 6 → 'O'
            (byte) 0xD7, // 7 → 'P'
            (byte) 0xD8, // 8 → 'Q'
            (byte) 0xD9  // 9 → 'R'
    };

    /**
     * Unsigned digit bytes (zone nibble 0xF).
     */
    private static final byte[] UNSIGNED_DIGITS = {
            (byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4,
            (byte) 0xF5, (byte) 0xF6, (byte) 0xF7, (byte) 0xF8, (byte) 0xF9
    };

    private ZonedDecimalCodec() {
        // utility class
    }

    /**
     * Encode an integer value into unsigned zoned decimal bytes.
     *
     * @param value      the value to encode
     * @param length     the number of bytes (digits) to produce
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return byte array of exactly {@code length} bytes
     * @throws OverflowException if the value has more digits than length allows
     */
    public static byte[] encodeUnsigned(long value, int length,
                                        String recordName, String fieldPath, int offset) {
        if (value < 0) {
            throw new OverflowException(recordName, fieldPath, offset, length,
                    String.valueOf(value), length);
        }
        return encodeDigits(value, length, false, recordName, fieldPath, offset);
    }

    /**
     * Encode an integer value into signed zoned decimal bytes with trailing overpunch.
     *
     * @param value      the value to encode (may be negative)
     * @param length     the number of bytes (digits) to produce
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return byte array of exactly {@code length} bytes with trailing overpunch
     * @throws OverflowException if the absolute value has more digits than length allows
     */
    public static byte[] encodeSigned(long value, int length,
                                      String recordName, String fieldPath, int offset) {
        return encodeDigits(value, length, true, recordName, fieldPath, offset);
    }

    /**
     * Encode a BigDecimal value into unsigned zoned decimal bytes.
     * The scale is implied (not encoded); caller must ensure correct digit count.
     *
     * @param value      the value to encode
     * @param length     the number of bytes (digits) to produce
     * @param scale      the implied decimal scale (for unscaling)
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return byte array of exactly {@code length} bytes
     * @throws OverflowException if the value has more digits than length allows
     */
    public static byte[] encodeUnsignedDecimal(BigDecimal value, int length, int scale,
                                               String recordName, String fieldPath, int offset) {
        if (value.signum() < 0) {
            throw new OverflowException(recordName, fieldPath, offset, length,
                    value.toPlainString(), length);
        }
        BigInteger unscaled = scaleValue(value, scale);
        return encodeDigits(unscaled, length, false, recordName, fieldPath, offset);
    }

    /**
     * Encode a BigDecimal value into signed zoned decimal bytes with trailing overpunch.
     * The scale is implied (not encoded); caller must ensure correct digit count.
     *
     * @param value      the value to encode
     * @param length     the number of bytes (digits) to produce
     * @param scale      the implied decimal scale (for unscaling)
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return byte array of exactly {@code length} bytes with trailing overpunch
     * @throws OverflowException if the absolute value has more digits than length allows
     */
    public static byte[] encodeSignedDecimal(BigDecimal value, int length, int scale,
                                             String recordName, String fieldPath, int offset) {
        BigInteger unscaled = scaleValue(value, scale);
        return encodeDigits(unscaled, length, true, recordName, fieldPath, offset);
    }

    /**
     * Decode unsigned zoned decimal bytes to a long value.
     *
     * @param bytes      the zoned decimal bytes
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return the decoded long value
     * @throws InvalidDigitException if any byte is not a valid unsigned digit
     */
    public static long decodeUnsigned(byte[] bytes,
                                      String recordName, String fieldPath, int offset) {
        long result = 0;
        for (int i = 0; i < bytes.length; i++) {
            int digit = extractUnsignedDigit(bytes[i]);
            if (digit < 0) {
                throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
            }
            result = result * 10 + digit;
        }
        return result;
    }

    /**
     * Decode signed zoned decimal bytes with trailing overpunch to a long value.
     *
     * @param bytes      the zoned decimal bytes with trailing overpunch
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return the decoded long value (may be negative)
     * @throws InvalidDigitException     if a leading byte is not a valid unsigned digit
     * @throws InvalidOverpunchException if the last byte is not a valid overpunch
     */
    public static long decodeSigned(byte[] bytes,
                                    String recordName, String fieldPath, int offset) {
        if (bytes.length == 0) {
            return 0;
        }

        long result = 0;
        boolean negative = false;

        // Process all but the last byte as unsigned digits
        for (int i = 0; i < bytes.length - 1; i++) {
            int digit = extractUnsignedDigit(bytes[i]);
            if (digit < 0) {
                throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
            }
            result = result * 10 + digit;
        }

        // Process the last byte as overpunch
        byte lastByte = bytes[bytes.length - 1];
        int[] signAndDigit = decodeOverpunch(lastByte);
        if (signAndDigit == null) {
            throw new InvalidOverpunchException(recordName, fieldPath, offset, bytes.length, bytes, lastByte);
        }
        negative = signAndDigit[0] < 0;
        result = result * 10 + signAndDigit[1];

        return negative ? -result : result;
    }

    /**
     * Decode unsigned zoned decimal bytes to a BigDecimal with scale.
     *
     * @param bytes      the zoned decimal bytes
     * @param scale      the implied decimal scale
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return the decoded BigDecimal value
     * @throws InvalidDigitException if any byte is not a valid unsigned digit
     */
    public static BigDecimal decodeUnsignedDecimal(byte[] bytes, int scale,
                                                   String recordName, String fieldPath, int offset) {
        BigInteger unscaled = decodeToUnscaled(bytes, false, recordName, fieldPath, offset);
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Decode signed zoned decimal bytes with trailing overpunch to a BigDecimal.
     *
     * @param bytes      the zoned decimal bytes with trailing overpunch
     * @param scale      the implied decimal scale
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param offset     field offset for error context
     * @return the decoded BigDecimal value
     * @throws InvalidDigitException     if a leading byte is not a valid unsigned digit
     * @throws InvalidOverpunchException if the last byte is not a valid overpunch
     */
    public static BigDecimal decodeSignedDecimal(byte[] bytes, int scale,
                                                 String recordName, String fieldPath, int offset) {
        BigInteger unscaled = decodeToUnscaled(bytes, true, recordName, fieldPath, offset);
        return new BigDecimal(unscaled, scale);
    }

    // -------------------------------------------------------------------------
    // Internal helper methods
    // -------------------------------------------------------------------------

    /**
     * Encode a long value to zoned decimal bytes.
     */
    private static byte[] encodeDigits(long value, int length, boolean signed,
                                       String recordName, String fieldPath, int offset) {
        boolean negative = value < 0;
        long absValue = Math.abs(value);

        // Convert to digit string with leading zeros
        String digits = String.format("%0" + length + "d", absValue);
        if (digits.length() > length) {
            throw new OverflowException(recordName, fieldPath, offset, length, String.valueOf(value), length);
        }

        byte[] result = new byte[length];
        for (int i = 0; i < length - 1; i++) {
            int d = Character.digit(digits.charAt(i), 10);
            result[i] = UNSIGNED_DIGITS[d];
        }

        // Last byte: overpunch if signed, unsigned digit otherwise
        int lastDigit = Character.digit(digits.charAt(length - 1), 10);
        if (signed) {
            result[length - 1] = negative ? NEGATIVE_OVERPUNCH[lastDigit] : POSITIVE_OVERPUNCH[lastDigit];
        } else {
            result[length - 1] = UNSIGNED_DIGITS[lastDigit];
        }

        return result;
    }

    /**
     * Encode a BigInteger value to zoned decimal bytes.
     */
    private static byte[] encodeDigits(BigInteger value, int length, boolean signed,
                                       String recordName, String fieldPath, int offset) {
        boolean negative = value.signum() < 0;
        BigInteger absValue = value.abs();

        String digits = absValue.toString();
        // Pad with leading zeros
        while (digits.length() < length) {
            digits = "0" + digits;
        }
        if (digits.length() > length) {
            throw new OverflowException(recordName, fieldPath, offset, length, value.toString(), length);
        }

        byte[] result = new byte[length];
        for (int i = 0; i < length - 1; i++) {
            int d = Character.digit(digits.charAt(i), 10);
            result[i] = UNSIGNED_DIGITS[d];
        }

        // Last byte: overpunch if signed, unsigned digit otherwise
        int lastDigit = Character.digit(digits.charAt(length - 1), 10);
        if (signed) {
            result[length - 1] = negative ? NEGATIVE_OVERPUNCH[lastDigit] : POSITIVE_OVERPUNCH[lastDigit];
        } else {
            result[length - 1] = UNSIGNED_DIGITS[lastDigit];
        }

        return result;
    }

    /**
     * Scale a BigDecimal value to produce an unscaled integer for encoding.
     */
    private static BigInteger scaleValue(BigDecimal value, int scale) {
        // Move decimal point to the right by 'scale' positions
        return value.movePointRight(scale).toBigIntegerExact();
    }

    /**
     * Decode zoned bytes to an unscaled BigInteger.
     */
    private static BigInteger decodeToUnscaled(byte[] bytes, boolean signed,
                                               String recordName, String fieldPath, int offset) {
        if (bytes.length == 0) {
            return BigInteger.ZERO;
        }

        StringBuilder digits = new StringBuilder();
        boolean negative = false;

        if (signed) {
            // Process all but last byte as unsigned
            for (int i = 0; i < bytes.length - 1; i++) {
                int d = extractUnsignedDigit(bytes[i]);
                if (d < 0) {
                    throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
                }
                digits.append(d);
            }

            // Last byte is overpunch
            byte lastByte = bytes[bytes.length - 1];
            int[] signAndDigit = decodeOverpunch(lastByte);
            if (signAndDigit == null) {
                throw new InvalidOverpunchException(recordName, fieldPath, offset, bytes.length, bytes, lastByte);
            }
            negative = signAndDigit[0] < 0;
            digits.append(signAndDigit[1]);
        } else {
            // All bytes are unsigned digits
            for (int i = 0; i < bytes.length; i++) {
                int d = extractUnsignedDigit(bytes[i]);
                if (d < 0) {
                    throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
                }
                digits.append(d);
            }
        }

        BigInteger result = new BigInteger(digits.toString());
        return negative ? result.negate() : result;
    }

    /**
     * Extract a digit (0-9) from an unsigned EBCDIC digit byte.
     *
     * @param b the byte to decode
     * @return the digit value (0-9), or -1 if invalid
     */
    private static int extractUnsignedDigit(byte b) {
        int unsigned = b & 0xFF;
        if (unsigned >= 0xF0 && unsigned <= 0xF9) {
            return unsigned - 0xF0;
        }
        return -1;
    }

    /**
     * Decode an overpunch byte to extract sign and digit.
     *
     * @param b the overpunch byte
     * @return array of [sign, digit] where sign is 1 for positive, -1 for negative,
     *         or null if invalid
     */
    private static int[] decodeOverpunch(byte b) {
        int unsigned = b & 0xFF;
        int zone = (unsigned >> 4) & 0x0F;
        int digit = unsigned & 0x0F;

        if (digit > 9) {
            return null; // Invalid digit nibble
        }

        switch (zone) {
            case 0x0C: // Positive overpunch (C0-C9)
            case 0x0F: // Unsigned (F0-F9) - treat as positive
            case 0x0A: // Alternate positive (some systems)
            case 0x0E: // Alternate positive (some systems)
                return new int[]{1, digit};
            case 0x0D: // Negative overpunch (D0-D9)
            case 0x0B: // Alternate negative (some systems)
                return new int[]{-1, digit};
            default:
                return null;
        }
    }

    // -------------------------------------------------------------------------
    // Buffer-based operations for codec integration
    // -------------------------------------------------------------------------

    /**
     * Encode a long value into a buffer at the specified offset.
     *
     * @param value      the value to encode
     * @param buffer     the target buffer
     * @param bufOffset  the offset in the buffer
     * @param length     the number of bytes (digits) to write
     * @param signed     true for trailing overpunch
     * @param recordName record name for error context
     * @param fieldPath  field path for error context
     * @param fieldOffset field offset for error context
     */
    public static void encodeInto(long value, byte[] buffer, int bufOffset, int length, boolean signed,
                                  String recordName, String fieldPath, int fieldOffset) {
        byte[] encoded = signed
                ? encodeSigned(value, length, recordName, fieldPath, fieldOffset)
                : encodeUnsigned(value, length, recordName, fieldPath, fieldOffset);
        System.arraycopy(encoded, 0, buffer, bufOffset, length);
    }

    /**
     * Encode a BigDecimal value into a buffer at the specified offset.
     *
     * @param value       the value to encode
     * @param buffer      the target buffer
     * @param bufOffset   the offset in the buffer
     * @param length      the number of bytes (digits) to write
     * @param scale       the implied decimal scale
     * @param signed      true for trailing overpunch
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param fieldOffset field offset for error context
     */
    public static void encodeInto(BigDecimal value, byte[] buffer, int bufOffset, int length, int scale, boolean signed,
                                  String recordName, String fieldPath, int fieldOffset) {
        byte[] encoded = signed
                ? encodeSignedDecimal(value, length, scale, recordName, fieldPath, fieldOffset)
                : encodeUnsignedDecimal(value, length, scale, recordName, fieldPath, fieldOffset);
        System.arraycopy(encoded, 0, buffer, bufOffset, length);
    }

    /**
     * Decode a long value from a buffer at the specified offset.
     *
     * @param buffer      the source buffer
     * @param bufOffset   the offset in the buffer
     * @param length      the number of bytes (digits) to read
     * @param signed      true if trailing overpunch is expected
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param fieldOffset field offset for error context
     * @return the decoded long value
     */
    public static long decodeFrom(byte[] buffer, int bufOffset, int length, boolean signed,
                                  String recordName, String fieldPath, int fieldOffset) {
        byte[] fieldBytes = new byte[length];
        System.arraycopy(buffer, bufOffset, fieldBytes, 0, length);
        return signed
                ? decodeSigned(fieldBytes, recordName, fieldPath, fieldOffset)
                : decodeUnsigned(fieldBytes, recordName, fieldPath, fieldOffset);
    }

    /**
     * Decode a BigDecimal value from a buffer at the specified offset.
     *
     * @param buffer      the source buffer
     * @param bufOffset   the offset in the buffer
     * @param length      the number of bytes (digits) to read
     * @param scale       the implied decimal scale
     * @param signed      true if trailing overpunch is expected
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param fieldOffset field offset for error context
     * @return the decoded BigDecimal value
     */
    public static BigDecimal decodeDecimalFrom(byte[] buffer, int bufOffset, int length, int scale, boolean signed,
                                               String recordName, String fieldPath, int fieldOffset) {
        byte[] fieldBytes = new byte[length];
        System.arraycopy(buffer, bufOffset, fieldBytes, 0, length);
        return signed
                ? decodeSignedDecimal(fieldBytes, scale, recordName, fieldPath, fieldOffset)
                : decodeUnsignedDecimal(fieldBytes, scale, recordName, fieldPath, fieldOffset);
    }

    // -------------------------------------------------------------------------
    // Utility methods for testing and validation
    // -------------------------------------------------------------------------

    /**
     * Get the positive overpunch byte for a digit.
     *
     * @param digit the digit (0-9)
     * @return the positive overpunch byte (0xC0-0xC9)
     */
    public static byte getPositiveOverpunch(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return POSITIVE_OVERPUNCH[digit];
    }

    /**
     * Get the negative overpunch byte for a digit.
     *
     * @param digit the digit (0-9)
     * @return the negative overpunch byte (0xD0-0xD9)
     */
    public static byte getNegativeOverpunch(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return NEGATIVE_OVERPUNCH[digit];
    }

    /**
     * Get the unsigned digit byte for a digit.
     *
     * @param digit the digit (0-9)
     * @return the unsigned digit byte (0xF0-0xF9)
     */
    public static byte getUnsignedDigit(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return UNSIGNED_DIGITS[digit];
    }

    /**
     * Check if a byte is a valid overpunch byte (can decode sign and digit).
     *
     * @param b the byte to check
     * @return true if the byte is a valid overpunch
     */
    public static boolean isValidOverpunch(byte b) {
        return decodeOverpunch(b) != null;
    }
}
