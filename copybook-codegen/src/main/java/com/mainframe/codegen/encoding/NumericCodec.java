package com.mainframe.codegen.encoding;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Utilities for encoding and decoding numeric values used by copybook
 * fields. Supports binary (COMP) and packed decimal (COMP-3) formats.
 *
 * <p><strong>COMP (binary):</strong>
 * <ul>
 *   <li>Big-endian two's complement representation</li>
 *   <li>Length 1-4 bytes maps to int</li>
 *   <li>Length 5-8 bytes maps to long</li>
 *   <li>Sign extension is applied during decode for signed fields</li>
 *   <li>Overflow checking during encode</li>
 * </ul>
 *
 * <p><strong>COMP-3 (packed decimal):</strong>
 * <ul>
 *   <li>Each byte contains two digit nibbles (high and low)</li>
 *   <li>Last nibble is the sign: 0x0C/0x0F for positive, 0x0D/0x0B for negative</li>
 *   <li>Maximum digits = (length * 2) - 1</li>
 *   <li>Invalid sign nibbles and digit nibbles are detected</li>
 * </ul>
 */
public final class NumericCodec {

    private NumericCodec() {
        // utility class
    }

    // =========================================================================
    // COMP (Binary) Encoding/Decoding
    // =========================================================================

    /**
     * Encode an int value into a fixed-length big-endian binary representation.
     *
     * @param value       the integer to encode (negative values use two's complement)
     * @param length      the number of bytes to emit (1-4)
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return a byte array of length {@code length}
     * @throws OverflowException if the value cannot be represented in the given length
     */
    public static byte[] encodeComp(int value, int length,
                                    String recordName, String fieldPath, int offset) {
        validateCompRange(value, length, recordName, fieldPath, offset);

        byte[] result = new byte[length];
        for (int i = 0; i < length; i++) {
            int shift = 8 * (length - i - 1);
            result[i] = (byte) ((value >> shift) & 0xFF);
        }
        return result;
    }

    /**
     * Encode a long value into a fixed-length big-endian binary representation.
     *
     * @param value       the long to encode (negative values use two's complement)
     * @param length      the number of bytes to emit (1-8)
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return a byte array of length {@code length}
     * @throws OverflowException if the value cannot be represented in the given length
     */
    public static byte[] encodeCompLong(long value, int length,
                                        String recordName, String fieldPath, int offset) {
        validateCompRangeLong(value, length, recordName, fieldPath, offset);

        byte[] result = new byte[length];
        for (int i = 0; i < length; i++) {
            int shift = 8 * (length - i - 1);
            result[i] = (byte) ((value >> shift) & 0xFF);
        }
        return result;
    }

    /**
     * Decode a big-endian binary (COMP) integer from bytes with sign extension.
     *
     * <p>This method properly sign-extends the value when the byte array
     * is shorter than 4 bytes and the high bit is set (indicating a negative value).</p>
     *
     * @param bytes the bytes representing the integer
     * @return the decoded int value with proper sign extension
     */
    public static int decodeComp(byte[] bytes) {
        if (bytes.length == 0) {
            return 0;
        }

        // Check if the high bit is set (negative number)
        boolean negative = (bytes[0] & 0x80) != 0;

        // Start with sign extension: all 1s for negative, all 0s for positive
        int value = negative ? -1 : 0;

        // Shift in each byte
        for (byte b : bytes) {
            value = (value << 8) | (b & 0xFF);
        }

        return value;
    }

    /**
     * Decode a big-endian binary (COMP) long from bytes with sign extension.
     *
     * @param bytes the bytes representing the long value
     * @return the decoded long value with proper sign extension
     */
    public static long decodeCompLong(byte[] bytes) {
        if (bytes.length == 0) {
            return 0L;
        }

        // Check if the high bit is set (negative number)
        boolean negative = (bytes[0] & 0x80) != 0;

        // Start with sign extension
        long value = negative ? -1L : 0L;

        // Shift in each byte
        for (byte b : bytes) {
            value = (value << 8) | (b & 0xFF);
        }

        return value;
    }

    /**
     * Encode an int value into a buffer at the specified offset.
     */
    public static void encodeCompInto(int value, byte[] buffer, int bufOffset, int length,
                                      String recordName, String fieldPath, int fieldOffset) {
        byte[] encoded = encodeComp(value, length, recordName, fieldPath, fieldOffset);
        System.arraycopy(encoded, 0, buffer, bufOffset, length);
    }

    /**
     * Encode a long value into a buffer at the specified offset.
     */
    public static void encodeCompLongInto(long value, byte[] buffer, int bufOffset, int length,
                                          String recordName, String fieldPath, int fieldOffset) {
        byte[] encoded = encodeCompLong(value, length, recordName, fieldPath, fieldOffset);
        System.arraycopy(encoded, 0, buffer, bufOffset, length);
    }

    /**
     * Decode an int value from a buffer at the specified offset.
     */
    public static int decodeCompFrom(byte[] buffer, int bufOffset, int length) {
        byte[] fieldBytes = new byte[length];
        System.arraycopy(buffer, bufOffset, fieldBytes, 0, length);
        return decodeComp(fieldBytes);
    }

    /**
     * Decode a long value from a buffer at the specified offset.
     */
    public static long decodeCompLongFrom(byte[] buffer, int bufOffset, int length) {
        byte[] fieldBytes = new byte[length];
        System.arraycopy(buffer, bufOffset, fieldBytes, 0, length);
        return decodeCompLong(fieldBytes);
    }

    // =========================================================================
    // COMP-3 (Packed Decimal) Encoding/Decoding
    // =========================================================================

    /**
     * Encode a {@link BigDecimal} into a packed decimal (COMP-3) representation.
     *
     * <p>The result array will have the specified length. The last nibble encodes
     * the sign (C for positive, D for negative). Maximum digits = (length * 2) - 1.</p>
     *
     * @param decimal     the decimal value to encode
     * @param length      the length in bytes of the packed decimal field
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return a byte array of length {@code length}
     * @throws OverflowException if the value has too many digits
     */
    public static byte[] encodeComp3(BigDecimal decimal, int length,
                                     String recordName, String fieldPath, int offset) {
        BigInteger unscaled = decimal.unscaledValue().abs();
        String digits = unscaled.toString();
        boolean negative = decimal.signum() < 0;

        // Maximum digits that can fit: (length * 2) - 1 (one nibble for sign)
        int maxDigits = (length * 2) - 1;
        if (digits.length() > maxDigits) {
            throw new OverflowException(recordName, fieldPath, offset, length,
                    decimal.toPlainString(), maxDigits);
        }

        // Pad with leading zeros to fill all digit positions
        while (digits.length() < maxDigits) {
            digits = "0" + digits;
        }

        byte[] result = new byte[length];
        int digitIndex = 0;
        int byteIndex = 0;

        // Pack digit pairs into bytes (high nibble = first digit, low nibble = second digit)
        // For the last byte, low nibble is the sign
        while (byteIndex < length - 1) {
            int high = Character.digit(digits.charAt(digitIndex++), 10);
            int low = Character.digit(digits.charAt(digitIndex++), 10);
            result[byteIndex++] = (byte) ((high << 4) | low);
        }

        // Last byte: last digit in high nibble, sign in low nibble
        int lastDigit = Character.digit(digits.charAt(digitIndex), 10);
        int signNibble = negative ? 0x0D : 0x0C;
        result[length - 1] = (byte) ((lastDigit << 4) | signNibble);

        return result;
    }

    /**
     * Encode a long value into packed decimal (COMP-3) representation.
     *
     * @param value       the long value to encode
     * @param length      the length in bytes of the packed decimal field
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return a byte array of length {@code length}
     */
    public static byte[] encodeComp3Long(long value, int length,
                                         String recordName, String fieldPath, int offset) {
        return encodeComp3(BigDecimal.valueOf(value), length, recordName, fieldPath, offset);
    }

    /**
     * Decode a packed decimal (COMP-3) value into a {@link BigDecimal}.
     *
     * @param bytes       the packed decimal bytes
     * @param scale       the number of decimal places to apply
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return the decoded {@link BigDecimal}
     * @throws InvalidDigitException      if a digit nibble contains a value > 9
     * @throws InvalidSignNibbleException if the sign nibble is invalid
     */
    public static BigDecimal decodeComp3(byte[] bytes, int scale,
                                         String recordName, String fieldPath, int offset) {
        if (bytes.length == 0) {
            return BigDecimal.ZERO;
        }

        StringBuilder digits = new StringBuilder();
        boolean negative = false;

        for (int i = 0; i < bytes.length; i++) {
            int b = bytes[i] & 0xFF;
            int high = (b >> 4) & 0x0F;
            int low = b & 0x0F;

            if (i == bytes.length - 1) {
                // Last byte: high nibble is digit, low nibble is sign
                if (high > 9) {
                    throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
                }
                digits.append(high);

                // Validate sign nibble
                switch (low) {
                    case 0x0C: // Positive
                    case 0x0F: // Unsigned/Positive
                    case 0x0A: // Alternate positive
                    case 0x0E: // Alternate positive
                        negative = false;
                        break;
                    case 0x0D: // Negative
                    case 0x0B: // Alternate negative
                        negative = true;
                        break;
                    default:
                        throw new InvalidSignNibbleException(recordName, fieldPath, offset, bytes.length, bytes, low);
                }
            } else {
                // Regular byte: both nibbles are digits
                if (high > 9) {
                    throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
                }
                if (low > 9) {
                    throw new InvalidDigitException(recordName, fieldPath, offset, bytes.length, bytes, i);
                }
                digits.append(high).append(low);
            }
        }

        BigInteger unscaled = new BigInteger(digits.toString());
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Decode a packed decimal (COMP-3) value into a long.
     *
     * @param bytes       the packed decimal bytes
     * @param recordName  record name for error context
     * @param fieldPath   field path for error context
     * @param offset      field offset for error context
     * @return the decoded long value
     */
    public static long decodeComp3Long(byte[] bytes,
                                       String recordName, String fieldPath, int offset) {
        BigDecimal result = decodeComp3(bytes, 0, recordName, fieldPath, offset);
        return result.longValueExact();
    }

    /**
     * Encode a BigDecimal into a buffer at the specified offset.
     */
    public static void encodeComp3Into(BigDecimal value, byte[] buffer, int bufOffset, int length,
                                       String recordName, String fieldPath, int fieldOffset) {
        byte[] encoded = encodeComp3(value, length, recordName, fieldPath, fieldOffset);
        System.arraycopy(encoded, 0, buffer, bufOffset, length);
    }

    /**
     * Decode a BigDecimal from a buffer at the specified offset.
     */
    public static BigDecimal decodeComp3From(byte[] buffer, int bufOffset, int length, int scale,
                                             String recordName, String fieldPath, int fieldOffset) {
        byte[] fieldBytes = new byte[length];
        System.arraycopy(buffer, bufOffset, fieldBytes, 0, length);
        return decodeComp3(fieldBytes, scale, recordName, fieldPath, fieldOffset);
    }

    // =========================================================================
    // Legacy methods (without error context) - kept for backwards compatibility
    // =========================================================================

    /**
     * Encode an int value into big-endian bytes (legacy method without error context).
     *
     * @param value  the integer to encode
     * @param length the number of bytes to emit
     * @return a byte array of length {@code length}
     */
    public static byte[] encodeComp(int value, int length) {
        byte[] result = new byte[length];
        for (int i = 0; i < length; i++) {
            int shift = 8 * (length - i - 1);
            result[i] = (byte) ((value >> shift) & 0xFF);
        }
        return result;
    }

    /**
     * Encode a BigDecimal into packed decimal (legacy method without error context).
     *
     * @param decimal the decimal value to encode
     * @param length  the length in bytes
     * @return a byte array of length {@code length}
     */
    public static byte[] encodeComp3(BigDecimal decimal, int length) {
        return encodeComp3(decimal, length, "unknown", "unknown", 0);
    }

    /**
     * Decode packed decimal to BigDecimal (legacy method without error context).
     *
     * @param bytes the packed decimal bytes
     * @param scale the number of decimal places
     * @return the decoded BigDecimal
     */
    public static BigDecimal decodeComp3(byte[] bytes, int scale) {
        return decodeComp3(bytes, scale, "unknown", "unknown", 0);
    }

    // =========================================================================
    // Validation helpers
    // =========================================================================

    /**
     * Validate that an int value can fit in the specified byte length.
     */
    private static void validateCompRange(int value, int length,
                                          String recordName, String fieldPath, int offset) {
        if (length < 1 || length > 4) {
            throw new IllegalArgumentException("COMP int length must be 1-4, got: " + length);
        }

        long minValue;
        long maxValue;

        if (length == 4) {
            minValue = Integer.MIN_VALUE;
            maxValue = Integer.MAX_VALUE;
        } else {
            int bits = length * 8;
            minValue = -(1L << (bits - 1));
            maxValue = (1L << (bits - 1)) - 1;
        }

        if (value < minValue || value > maxValue) {
            throw new OverflowException(recordName, fieldPath, offset, length,
                    String.valueOf(value));
        }
    }

    /**
     * Validate that a long value can fit in the specified byte length.
     */
    private static void validateCompRangeLong(long value, int length,
                                              String recordName, String fieldPath, int offset) {
        if (length < 1 || length > 8) {
            throw new IllegalArgumentException("COMP long length must be 1-8, got: " + length);
        }

        if (length == 8) {
            // All long values fit in 8 bytes
            return;
        }

        int bits = length * 8;
        long minValue = -(1L << (bits - 1));
        long maxValue = (1L << (bits - 1)) - 1;

        if (value < minValue || value > maxValue) {
            throw new OverflowException(recordName, fieldPath, offset, length,
                    String.valueOf(value));
        }
    }
}
