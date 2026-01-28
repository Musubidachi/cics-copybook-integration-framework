package com.mainframe.codegen.encoding;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link ZonedDecimalCodec} with golden vector byte comparisons.
 *
 * <p>These tests verify the correct encoding and decoding of DISPLAY numeric
 * fields using EBCDIC zoned decimal format with trailing overpunch sign.</p>
 *
 * <p><strong>IBM037 Overpunch Reference:</strong></p>
 * <pre>
 * Digit | Unsigned (F) | Positive (C) | Negative (D)
 * ------+--------------+--------------+-------------
 *   0   |     F0       |     C0       |     D0
 *   1   |     F1       |     C1       |     D1
 *   2   |     F2       |     C2       |     D2
 *   3   |     F3       |     C3       |     D3
 *   4   |     F4       |     C4       |     D4
 *   5   |     F5       |     C5       |     D5
 *   6   |     F6       |     C6       |     D6
 *   7   |     F7       |     C7       |     D7
 *   8   |     F8       |     C8       |     D8
 *   9   |     F9       |     C9       |     D9
 * </pre>
 */
class ZonedDecimalCodecTest {

    private static final String RECORD = "TEST-REC";
    private static final String FIELD = "TEST-FIELD";
    private static final int OFFSET = 0;

    @Nested
    @DisplayName("Unsigned Zoned Decimal Encoding")
    class UnsignedEncodingTests {

        @Test
        @DisplayName("Encode zero as 3-digit unsigned")
        void encodeZeroThreeDigits() {
            // 000 → F0 F0 F0
            byte[] result = ZonedDecimalCodec.encodeUnsigned(0, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF0, (byte) 0xF0, (byte) 0xF0};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode 123 as 3-digit unsigned")
        void encode123ThreeDigits() {
            // 123 → F1 F2 F3
            byte[] result = ZonedDecimalCodec.encodeUnsigned(123, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode 42 as 5-digit unsigned with leading zeros")
        void encode42FiveDigits() {
            // 00042 → F0 F0 F0 F4 F2
            byte[] result = ZonedDecimalCodec.encodeUnsigned(42, 5, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF0, (byte) 0xF0, (byte) 0xF0, (byte) 0xF4, (byte) 0xF2};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode max value for 9 digits")
        void encodeMaxNineDigits() {
            // 999999999 → F9 F9 F9 F9 F9 F9 F9 F9 F9
            byte[] result = ZonedDecimalCodec.encodeUnsigned(999999999L, 9, RECORD, FIELD, OFFSET);
            byte[] expected = new byte[9];
            for (int i = 0; i < 9; i++) {
                expected[i] = (byte) 0xF9;
            }
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Overflow throws exception for negative value")
        void encodeNegativeThrowsOverflow() {
            assertThrows(OverflowException.class, () ->
                    ZonedDecimalCodec.encodeUnsigned(-1, 3, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Overflow throws exception for too many digits")
        void encodeTooManyDigitsThrowsOverflow() {
            assertThrows(OverflowException.class, () ->
                    ZonedDecimalCodec.encodeUnsigned(1000, 3, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("Signed Zoned Decimal Encoding (Trailing Overpunch)")
    class SignedEncodingTests {

        @Test
        @DisplayName("Encode positive zero with overpunch")
        void encodePositiveZero() {
            // +0 (3 digits) → F0 F0 C0
            byte[] result = ZonedDecimalCodec.encodeSigned(0, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF0, (byte) 0xF0, (byte) 0xC0};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode positive 123 with overpunch")
        void encodePositive123() {
            // +123 → F1 F2 C3
            byte[] result = ZonedDecimalCodec.encodeSigned(123, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xC3};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode negative 123 with overpunch")
        void encodeNegative123() {
            // -123 → F1 F2 D3
            byte[] result = ZonedDecimalCodec.encodeSigned(-123, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xD3};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode positive 5 with single digit")
        void encodePositive5SingleDigit() {
            // +5 → C5
            byte[] result = ZonedDecimalCodec.encodeSigned(5, 1, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xC5};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode negative 7 with single digit")
        void encodeNegative7SingleDigit() {
            // -7 → D7
            byte[] result = ZonedDecimalCodec.encodeSigned(-7, 1, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xD7};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode positive 99 (each digit 0-9 verified)")
        void encodePositive99() {
            // +99 → F9 C9
            byte[] result = ZonedDecimalCodec.encodeSigned(99, 2, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF9, (byte) 0xC9};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode negative 10 with overpunch")
        void encodeNegative10() {
            // -10 → F1 D0
            byte[] result = ZonedDecimalCodec.encodeSigned(-10, 2, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xD0};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode large negative number")
        void encodeLargeNegative() {
            // -987654 (6 digits) → F9 F8 F7 F6 F5 D4
            byte[] result = ZonedDecimalCodec.encodeSigned(-987654, 6, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF9, (byte) 0xF8, (byte) 0xF7,
                    (byte) 0xF6, (byte) 0xF5, (byte) 0xD4};
            assertArrayEquals(expected, result);
        }
    }

    @Nested
    @DisplayName("Unsigned Zoned Decimal Decoding")
    class UnsignedDecodingTests {

        @Test
        @DisplayName("Decode zero")
        void decodeZero() {
            byte[] input = {(byte) 0xF0, (byte) 0xF0, (byte) 0xF0};
            long result = ZonedDecimalCodec.decodeUnsigned(input, RECORD, FIELD, OFFSET);
            assertEquals(0, result);
        }

        @Test
        @DisplayName("Decode 123")
        void decode123() {
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3};
            long result = ZonedDecimalCodec.decodeUnsigned(input, RECORD, FIELD, OFFSET);
            assertEquals(123, result);
        }

        @Test
        @DisplayName("Decode with leading zeros")
        void decodeWithLeadingZeros() {
            byte[] input = {(byte) 0xF0, (byte) 0xF0, (byte) 0xF4, (byte) 0xF2};
            long result = ZonedDecimalCodec.decodeUnsigned(input, RECORD, FIELD, OFFSET);
            assertEquals(42, result);
        }

        @Test
        @DisplayName("Invalid digit throws exception")
        void invalidDigitThrows() {
            byte[] input = {(byte) 0xF1, (byte) 0xAA, (byte) 0xF3}; // 0xAA is invalid
            assertThrows(InvalidDigitException.class, () ->
                    ZonedDecimalCodec.decodeUnsigned(input, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("Signed Zoned Decimal Decoding (Trailing Overpunch)")
    class SignedDecodingTests {

        @Test
        @DisplayName("Decode positive zero")
        void decodePositiveZero() {
            // C0 = positive 0
            byte[] input = {(byte) 0xF0, (byte) 0xF0, (byte) 0xC0};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(0, result);
        }

        @Test
        @DisplayName("Decode negative zero")
        void decodeNegativeZero() {
            // D0 = negative 0, which is still 0
            byte[] input = {(byte) 0xF0, (byte) 0xF0, (byte) 0xD0};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(0, result);
        }

        @Test
        @DisplayName("Decode positive 123")
        void decodePositive123() {
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xC3};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(123, result);
        }

        @Test
        @DisplayName("Decode negative 123")
        void decodeNegative123() {
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xD3};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(-123, result);
        }

        @Test
        @DisplayName("Decode with zone F (unsigned/positive)")
        void decodeWithZoneF() {
            // F9 = unsigned 9, treated as positive
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF9};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(129, result);
        }

        @Test
        @DisplayName("Decode single digit positive")
        void decodeSinglePositive() {
            byte[] input = {(byte) 0xC5}; // +5
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(5, result);
        }

        @Test
        @DisplayName("Decode single digit negative")
        void decodeSingleNegative() {
            byte[] input = {(byte) 0xD7}; // -7
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(-7, result);
        }

        @Test
        @DisplayName("Decode large negative")
        void decodeLargeNegative() {
            byte[] input = {(byte) 0xF9, (byte) 0xF8, (byte) 0xF7,
                    (byte) 0xF6, (byte) 0xF5, (byte) 0xD4};
            long result = ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET);
            assertEquals(-987654, result);
        }

        @Test
        @DisplayName("Invalid overpunch throws exception")
        void invalidOverpunchThrows() {
            // 0x13 has zone nibble 1, which is invalid
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0x13};
            assertThrows(InvalidOverpunchException.class, () ->
                    ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Invalid digit nibble in overpunch throws exception")
        void invalidDigitInOverpunchThrows() {
            // 0xCA has digit nibble A (10), which is invalid
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xCA};
            assertThrows(InvalidOverpunchException.class, () ->
                    ZonedDecimalCodec.decodeSigned(input, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("Round-trip Encoding/Decoding")
    class RoundTripTests {

        @Test
        @DisplayName("Round-trip unsigned values")
        void roundTripUnsigned() {
            long[] values = {0, 1, 42, 123, 999, 12345, 999999999L};
            for (long value : values) {
                byte[] encoded = ZonedDecimalCodec.encodeUnsigned(value, 9, RECORD, FIELD, OFFSET);
                long decoded = ZonedDecimalCodec.decodeUnsigned(encoded, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + value);
            }
        }

        @Test
        @DisplayName("Round-trip signed positive values")
        void roundTripSignedPositive() {
            long[] values = {0, 1, 42, 123, 999, 12345, 999999999L};
            for (long value : values) {
                byte[] encoded = ZonedDecimalCodec.encodeSigned(value, 9, RECORD, FIELD, OFFSET);
                long decoded = ZonedDecimalCodec.decodeSigned(encoded, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + value);
            }
        }

        @Test
        @DisplayName("Round-trip signed negative values")
        void roundTripSignedNegative() {
            long[] values = {-1, -42, -123, -999, -12345, -999999999L};
            for (long value : values) {
                byte[] encoded = ZonedDecimalCodec.encodeSigned(value, 9, RECORD, FIELD, OFFSET);
                long decoded = ZonedDecimalCodec.decodeSigned(encoded, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + value);
            }
        }
    }

    @Nested
    @DisplayName("BigDecimal with Scale")
    class DecimalScaleTests {

        @Test
        @DisplayName("Encode decimal with scale 2")
        void encodeDecimalScale2() {
            // 12.34 with scale 2 → stored as 1234 → F1 F2 F3 F4
            BigDecimal value = new BigDecimal("12.34");
            byte[] result = ZonedDecimalCodec.encodeUnsignedDecimal(value, 4, 2, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode negative decimal with scale 2")
        void encodeNegativeDecimalScale2() {
            // -12.34 with scale 2 → stored as -1234 → F1 F2 F3 D4
            BigDecimal value = new BigDecimal("-12.34");
            byte[] result = ZonedDecimalCodec.encodeSignedDecimal(value, 4, 2, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xD4};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Decode decimal with scale 2")
        void decodeDecimalScale2() {
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4};
            BigDecimal result = ZonedDecimalCodec.decodeUnsignedDecimal(input, 2, RECORD, FIELD, OFFSET);
            assertEquals(new BigDecimal("12.34"), result);
        }

        @Test
        @DisplayName("Decode negative decimal with scale 2")
        void decodeNegativeDecimalScale2() {
            byte[] input = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xD4};
            BigDecimal result = ZonedDecimalCodec.decodeSignedDecimal(input, 2, RECORD, FIELD, OFFSET);
            assertEquals(new BigDecimal("-12.34"), result);
        }

        @Test
        @DisplayName("Round-trip decimal values")
        void roundTripDecimals() {
            String[] values = {"0.00", "1.23", "99.99", "123.45", "0.01"};
            for (String v : values) {
                BigDecimal value = new BigDecimal(v);
                byte[] encoded = ZonedDecimalCodec.encodeUnsignedDecimal(value, 5, 2, RECORD, FIELD, OFFSET);
                BigDecimal decoded = ZonedDecimalCodec.decodeUnsignedDecimal(encoded, 2, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + v);
            }
        }
    }

    @Nested
    @DisplayName("Overpunch Byte Utilities")
    class OverpunchUtilityTests {

        @Test
        @DisplayName("Get positive overpunch bytes")
        void positiveOverpunchBytes() {
            assertEquals((byte) 0xC0, ZonedDecimalCodec.getPositiveOverpunch(0));
            assertEquals((byte) 0xC1, ZonedDecimalCodec.getPositiveOverpunch(1));
            assertEquals((byte) 0xC5, ZonedDecimalCodec.getPositiveOverpunch(5));
            assertEquals((byte) 0xC9, ZonedDecimalCodec.getPositiveOverpunch(9));
        }

        @Test
        @DisplayName("Get negative overpunch bytes")
        void negativeOverpunchBytes() {
            assertEquals((byte) 0xD0, ZonedDecimalCodec.getNegativeOverpunch(0));
            assertEquals((byte) 0xD1, ZonedDecimalCodec.getNegativeOverpunch(1));
            assertEquals((byte) 0xD5, ZonedDecimalCodec.getNegativeOverpunch(5));
            assertEquals((byte) 0xD9, ZonedDecimalCodec.getNegativeOverpunch(9));
        }

        @Test
        @DisplayName("Get unsigned digit bytes")
        void unsignedDigitBytes() {
            assertEquals((byte) 0xF0, ZonedDecimalCodec.getUnsignedDigit(0));
            assertEquals((byte) 0xF1, ZonedDecimalCodec.getUnsignedDigit(1));
            assertEquals((byte) 0xF5, ZonedDecimalCodec.getUnsignedDigit(5));
            assertEquals((byte) 0xF9, ZonedDecimalCodec.getUnsignedDigit(9));
        }

        @Test
        @DisplayName("Invalid digit throws exception")
        void invalidDigitThrows() {
            assertThrows(IllegalArgumentException.class, () -> ZonedDecimalCodec.getPositiveOverpunch(-1));
            assertThrows(IllegalArgumentException.class, () -> ZonedDecimalCodec.getPositiveOverpunch(10));
            assertThrows(IllegalArgumentException.class, () -> ZonedDecimalCodec.getNegativeOverpunch(-1));
            assertThrows(IllegalArgumentException.class, () -> ZonedDecimalCodec.getNegativeOverpunch(10));
        }

        @Test
        @DisplayName("Check valid overpunch bytes")
        void isValidOverpunch() {
            // Positive overpunch C0-C9
            for (int i = 0; i <= 9; i++) {
                assertTrue(ZonedDecimalCodec.isValidOverpunch((byte) (0xC0 + i)));
            }
            // Negative overpunch D0-D9
            for (int i = 0; i <= 9; i++) {
                assertTrue(ZonedDecimalCodec.isValidOverpunch((byte) (0xD0 + i)));
            }
            // Unsigned digits F0-F9
            for (int i = 0; i <= 9; i++) {
                assertTrue(ZonedDecimalCodec.isValidOverpunch((byte) (0xF0 + i)));
            }
            // Invalid bytes
            assertFalse(ZonedDecimalCodec.isValidOverpunch((byte) 0x00));
            assertFalse(ZonedDecimalCodec.isValidOverpunch((byte) 0x40)); // EBCDIC space
            assertFalse(ZonedDecimalCodec.isValidOverpunch((byte) 0xCA)); // digit nibble > 9
            assertFalse(ZonedDecimalCodec.isValidOverpunch((byte) 0xFA)); // digit nibble > 9
        }
    }

    @Nested
    @DisplayName("Buffer Operations")
    class BufferOperationTests {

        @Test
        @DisplayName("Encode into buffer at offset")
        void encodeIntoBuffer() {
            byte[] buffer = new byte[10];
            ZonedDecimalCodec.encodeInto(123, buffer, 3, 3, true, RECORD, FIELD, OFFSET);

            // Check bytes at positions 3, 4, 5 contain F1 F2 C3
            assertEquals((byte) 0xF1, buffer[3]);
            assertEquals((byte) 0xF2, buffer[4]);
            assertEquals((byte) 0xC3, buffer[5]);

            // Check other bytes are still 0
            assertEquals((byte) 0x00, buffer[0]);
            assertEquals((byte) 0x00, buffer[6]);
        }

        @Test
        @DisplayName("Decode from buffer at offset")
        void decodeFromBuffer() {
            byte[] buffer = {(byte) 0x00, (byte) 0x00, (byte) 0xF1, (byte) 0xF2, (byte) 0xD3, (byte) 0x00};
            long result = ZonedDecimalCodec.decodeFrom(buffer, 2, 3, true, RECORD, FIELD, OFFSET);
            assertEquals(-123, result);
        }
    }

    @Nested
    @DisplayName("Golden Vector Verification")
    class GoldenVectorTests {

        @Test
        @DisplayName("All positive overpunch golden vectors")
        void allPositiveOverpunchVectors() {
            // Test all 10 positive overpunch encodings
            byte[][] expected = {
                    {(byte) 0xC0}, {(byte) 0xC1}, {(byte) 0xC2}, {(byte) 0xC3}, {(byte) 0xC4},
                    {(byte) 0xC5}, {(byte) 0xC6}, {(byte) 0xC7}, {(byte) 0xC8}, {(byte) 0xC9}
            };
            for (int i = 0; i <= 9; i++) {
                byte[] result = ZonedDecimalCodec.encodeSigned(i, 1, RECORD, FIELD, OFFSET);
                assertArrayEquals(expected[i], result, "Positive overpunch mismatch for digit: " + i);
            }
        }

        @Test
        @DisplayName("All negative overpunch golden vectors")
        void allNegativeOverpunchVectors() {
            // Test all 10 negative overpunch encodings
            byte[][] expected = {
                    {(byte) 0xD0}, {(byte) 0xD1}, {(byte) 0xD2}, {(byte) 0xD3}, {(byte) 0xD4},
                    {(byte) 0xD5}, {(byte) 0xD6}, {(byte) 0xD7}, {(byte) 0xD8}, {(byte) 0xD9}
            };
            for (int i = 0; i <= 9; i++) {
                byte[] result = ZonedDecimalCodec.encodeSigned(-i, 1, RECORD, FIELD, OFFSET);
                // Note: -0 encodes same as +0 for the absolute value
                if (i == 0) {
                    assertArrayEquals(new byte[]{(byte) 0xC0}, result, "Zero encodes as positive");
                } else {
                    assertArrayEquals(expected[i], result, "Negative overpunch mismatch for digit: -" + i);
                }
            }
        }

        @Test
        @DisplayName("Complex golden vector: +12345")
        void complexPositiveVector() {
            // +12345 in 5 bytes → F1 F2 F3 F4 C5
            byte[] result = ZonedDecimalCodec.encodeSigned(12345, 5, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4, (byte) 0xC5};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Complex golden vector: -12345")
        void complexNegativeVector() {
            // -12345 in 5 bytes → F1 F2 F3 F4 D5
            byte[] result = ZonedDecimalCodec.encodeSigned(-12345, 5, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4, (byte) 0xD5};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Golden vector with leading zeros: +007")
        void leadingZerosPositiveVector() {
            // +7 in 3 bytes → F0 F0 C7
            byte[] result = ZonedDecimalCodec.encodeSigned(7, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF0, (byte) 0xF0, (byte) 0xC7};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Golden vector with leading zeros: -007")
        void leadingZerosNegativeVector() {
            // -7 in 3 bytes → F0 F0 D7
            byte[] result = ZonedDecimalCodec.encodeSigned(-7, 3, RECORD, FIELD, OFFSET);
            byte[] expected = {(byte) 0xF0, (byte) 0xF0, (byte) 0xD7};
            assertArrayEquals(expected, result);
        }
    }
}
