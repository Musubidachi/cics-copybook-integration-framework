package com.mainframe.codegen.encoding;

import java.math.BigDecimal;

/**
 * Utilities for encoding and decoding numeric values used by copybook
 * fields. Supports binary (COMP) and packed decimal (COMP-3) formats.
 */
public final class NumericCodec {

    private NumericCodec() {
        // utility class
    }

    /**
     * Encode an integer into a fixed-length big-endian binary representation.
     *
     * @param value  the integer to encode
     *     (negative values will be encoded in two's complement)
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
     * Decode a big-endian binary (COMP) integer from bytes.
     *
     * @param bytes the bytes representing the integer
     * @return the decoded int value
     */
    public static int decodeComp(byte[] bytes) {
        int value = 0;
        for (byte b : bytes) {
            value = (value << 8) | (b & 0xFF);
        }
        return value;
    }

    /**
     * Encode a {@link BigDecimal} into a packed decimal (COMP-3) representation.
     *
     * The result array will have the specified length. The last nibble encodes
     * the sign (C for positive, D for negative). A leading zero nibble is
     * inserted if the number of digits is odd.
     *
     * @param decimal the decimal value to encode
     * @param length  the length in bytes of the packed decimal field
     * @return a byte array of length {@code length}
     */
    public static byte[] encodeComp3(BigDecimal decimal, int length) {
        String digits = decimal.unscaledValue().abs().toString();
        boolean negative = decimal.signum() < 0;
        // ensure even number of digits
        if (digits.length() % 2 != 0) {
            digits = "0" + digits;
        }
        byte[] result = new byte[length];
        int byteIndex = 0;
        int charIndex = 0;
        while (charIndex < digits.length() && byteIndex < length) {
            int high = Character.digit(digits.charAt(charIndex++), 10);
            int low = Character.digit(digits.charAt(charIndex++), 10);
            result[byteIndex++] = (byte) ((high << 4) | low);
        }
        // place sign nibble in the low half of the last byte
        byte signNibble = (byte) (negative ? 0x0D : 0x0C);
        // if we haven't filled all bytes, ensure sign nibble set
        int lastIndex = length - 1;
        result[lastIndex] = (byte) ((result[lastIndex] & 0xF0) | (signNibble & 0x0F));
        return result;
    }

    /**
     * Decode a packed decimal (COMP-3) value into a {@link BigDecimal}.
     *
     * @param bytes the packed decimal bytes
     * @param scale the number of decimal places to apply
     * @return the decoded {@link BigDecimal}
     */
    public static BigDecimal decodeComp3(byte[] bytes, int scale) {
        StringBuilder digits = new StringBuilder();
        boolean negative = false;
        for (int i = 0; i < bytes.length; i++) {
            int b = bytes[i] & 0xFF;
            if (i == bytes.length - 1) {
                int high = (b >> 4) & 0x0F;
                int sign = b & 0x0F;
                digits.append(high);
                // sign nibble: C or F = positive, D or B = negative
                negative = (sign == 0x0D || sign == 0x0B);
            } else {
                int high = (b >> 4) & 0x0F;
                int low = b & 0x0F;
                digits.append(high).append(low);
            }
        }
        java.math.BigInteger unscaled = new java.math.BigInteger(digits.toString());
        if (negative) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }
}
