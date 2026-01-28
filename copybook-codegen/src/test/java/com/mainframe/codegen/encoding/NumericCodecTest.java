package com.mainframe.codegen.encoding;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link NumericCodec} covering COMP (binary) and COMP-3 (packed decimal).
 *
 * <p>These tests verify correct sign extension for COMP and nibble packing
 * with sign nibble validation for COMP-3.</p>
 */
class NumericCodecTest {

    private static final String RECORD = "TEST-REC";
    private static final String FIELD = "TEST-FIELD";
    private static final int OFFSET = 0;

    @Nested
    @DisplayName("COMP (Binary) Encoding")
    class CompEncodingTests {

        @Test
        @DisplayName("Encode zero as 2 bytes")
        void encodeZeroTwoBytes() {
            byte[] result = NumericCodec.encodeComp(0, 2);
            assertArrayEquals(new byte[]{0x00, 0x00}, result);
        }

        @Test
        @DisplayName("Encode positive value as 2 bytes")
        void encodePositiveTwoBytes() {
            // 256 = 0x0100
            byte[] result = NumericCodec.encodeComp(256, 2);
            assertArrayEquals(new byte[]{0x01, 0x00}, result);
        }

        @Test
        @DisplayName("Encode positive value as 4 bytes")
        void encodePositiveFourBytes() {
            // 16909060 = 0x01020304
            byte[] result = NumericCodec.encodeComp(16909060, 4);
            assertArrayEquals(new byte[]{0x01, 0x02, 0x03, 0x04}, result);
        }

        @Test
        @DisplayName("Encode negative value as 2 bytes (two's complement)")
        void encodeNegativeTwoBytes() {
            // -1 = 0xFFFF
            byte[] result = NumericCodec.encodeComp(-1, 2);
            assertArrayEquals(new byte[]{(byte) 0xFF, (byte) 0xFF}, result);
        }

        @Test
        @DisplayName("Encode -256 as 2 bytes")
        void encodeNegative256TwoBytes() {
            // -256 = 0xFF00
            byte[] result = NumericCodec.encodeComp(-256, 2);
            assertArrayEquals(new byte[]{(byte) 0xFF, 0x00}, result);
        }

        @Test
        @DisplayName("Encode max positive for 2 bytes")
        void encodeMaxPositiveTwoBytes() {
            // 32767 = 0x7FFF
            byte[] result = NumericCodec.encodeComp(32767, 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x7F, (byte) 0xFF}, result);
        }

        @Test
        @DisplayName("Encode min negative for 2 bytes")
        void encodeMinNegativeTwoBytes() {
            // -32768 = 0x8000
            byte[] result = NumericCodec.encodeComp(-32768, 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{(byte) 0x80, 0x00}, result);
        }

        @Test
        @DisplayName("Overflow throws for value too large")
        void overflowForTooLarge() {
            // 32768 exceeds 2-byte signed range
            assertThrows(OverflowException.class, () ->
                    NumericCodec.encodeComp(32768, 2, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Overflow throws for value too small")
        void overflowForTooSmall() {
            // -32769 exceeds 2-byte signed range
            assertThrows(OverflowException.class, () ->
                    NumericCodec.encodeComp(-32769, 2, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("COMP (Binary) Decoding with Sign Extension")
    class CompDecodingTests {

        @Test
        @DisplayName("Decode zero")
        void decodeZero() {
            byte[] input = {0x00, 0x00};
            assertEquals(0, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode positive value")
        void decodePositive() {
            // 0x0100 = 256
            byte[] input = {0x01, 0x00};
            assertEquals(256, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode negative value with sign extension")
        void decodeNegativeWithSignExtension() {
            // 0xFFFF = -1 in 2-byte two's complement
            byte[] input = {(byte) 0xFF, (byte) 0xFF};
            assertEquals(-1, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode negative from single byte with sign extension")
        void decodeNegativeSingleByte() {
            // 0xFF = -1 in 1-byte two's complement
            byte[] input = {(byte) 0xFF};
            assertEquals(-1, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode -128 from single byte")
        void decodeNegative128SingleByte() {
            // 0x80 = -128 in 1-byte two's complement
            byte[] input = {(byte) 0x80};
            assertEquals(-128, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode 127 from single byte")
        void decode127SingleByte() {
            // 0x7F = 127
            byte[] input = {0x7F};
            assertEquals(127, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode -256 with sign extension")
        void decodeNegative256() {
            // 0xFF00 = -256
            byte[] input = {(byte) 0xFF, 0x00};
            assertEquals(-256, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode max positive 2 bytes")
        void decodeMaxPositive() {
            // 0x7FFF = 32767
            byte[] input = {0x7F, (byte) 0xFF};
            assertEquals(32767, NumericCodec.decodeComp(input));
        }

        @Test
        @DisplayName("Decode min negative 2 bytes")
        void decodeMinNegative() {
            // 0x8000 = -32768
            byte[] input = {(byte) 0x80, 0x00};
            assertEquals(-32768, NumericCodec.decodeComp(input));
        }
    }

    @Nested
    @DisplayName("COMP Long Encoding/Decoding")
    class CompLongTests {

        @Test
        @DisplayName("Encode long positive value")
        void encodeLongPositive() {
            // 0x0102030405060708
            byte[] result = NumericCodec.encodeCompLong(0x0102030405060708L, 8, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08}, result);
        }

        @Test
        @DisplayName("Encode long negative value")
        void encodeLongNegative() {
            // -1L = 0xFFFFFFFFFFFFFFFF
            byte[] result = NumericCodec.encodeCompLong(-1L, 8, RECORD, FIELD, OFFSET);
            byte[] expected = new byte[8];
            for (int i = 0; i < 8; i++) expected[i] = (byte) 0xFF;
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Decode long positive value")
        void decodeLongPositive() {
            byte[] input = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08};
            assertEquals(0x0102030405060708L, NumericCodec.decodeCompLong(input));
        }

        @Test
        @DisplayName("Decode long negative value with sign extension")
        void decodeLongNegative() {
            byte[] input = {(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF};
            assertEquals(-1L, NumericCodec.decodeCompLong(input));
        }

        @Test
        @DisplayName("Round-trip long values")
        void roundTripLong() {
            long[] values = {0L, 1L, -1L, Long.MAX_VALUE, Long.MIN_VALUE, 123456789012345L, -123456789012345L};
            for (long value : values) {
                byte[] encoded = NumericCodec.encodeCompLong(value, 8, RECORD, FIELD, OFFSET);
                long decoded = NumericCodec.decodeCompLong(encoded);
                assertEquals(value, decoded, "Round-trip failed for: " + value);
            }
        }
    }

    @Nested
    @DisplayName("COMP-3 (Packed Decimal) Encoding")
    class Comp3EncodingTests {

        @Test
        @DisplayName("Encode zero as 2 bytes")
        void encodeZeroTwoBytes() {
            // 0 in 2 bytes: 000C (3 digits max)
            byte[] result = NumericCodec.encodeComp3(BigDecimal.ZERO, 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x00, 0x0C}, result);
        }

        @Test
        @DisplayName("Encode positive value")
        void encodePositive() {
            // 123 in 2 bytes: 12 3C
            byte[] result = NumericCodec.encodeComp3(new BigDecimal("123"), 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x12, 0x3C}, result);
        }

        @Test
        @DisplayName("Encode negative value")
        void encodeNegative() {
            // -123 in 2 bytes: 12 3D
            byte[] result = NumericCodec.encodeComp3(new BigDecimal("-123"), 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x12, 0x3D}, result);
        }

        @Test
        @DisplayName("Encode with leading zeros")
        void encodeWithLeadingZeros() {
            // 5 in 3 bytes (5 digits max): 00 05 C
            byte[] result = NumericCodec.encodeComp3(new BigDecimal("5"), 3, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{0x00, 0x00, 0x5C}, result);
        }

        @Test
        @DisplayName("Encode max value for 2 bytes")
        void encodeMaxTwoBytes() {
            // 999 in 2 bytes: 99 9C
            byte[] result = NumericCodec.encodeComp3(new BigDecimal("999"), 2, RECORD, FIELD, OFFSET);
            assertArrayEquals(new byte[]{(byte) 0x99, (byte) 0x9C}, result);
        }

        @Test
        @DisplayName("Overflow for too many digits")
        void overflowTooManyDigits() {
            // 1000 has 4 digits, but 2 bytes can only hold 3 digits
            assertThrows(OverflowException.class, () ->
                    NumericCodec.encodeComp3(new BigDecimal("1000"), 2, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("COMP-3 (Packed Decimal) Decoding")
    class Comp3DecodingTests {

        @Test
        @DisplayName("Decode zero")
        void decodeZero() {
            byte[] input = {0x00, 0x0C};
            assertEquals(BigDecimal.ZERO, NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode positive with sign C")
        void decodePositiveC() {
            // 12 3C = 123
            byte[] input = {0x12, 0x3C};
            assertEquals(new BigDecimal("123"), NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode positive with sign F")
        void decodePositiveF() {
            // 12 3F = 123 (F = unsigned/positive)
            byte[] input = {0x12, 0x3F};
            assertEquals(new BigDecimal("123"), NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode negative with sign D")
        void decodeNegativeD() {
            // 12 3D = -123
            byte[] input = {0x12, 0x3D};
            assertEquals(new BigDecimal("-123"), NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode negative with sign B")
        void decodeNegativeB() {
            // 12 3B = -123 (B = alternate negative)
            byte[] input = {0x12, 0x3B};
            assertEquals(new BigDecimal("-123"), NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode with scale")
        void decodeWithScale() {
            // 12 34 5C with scale 2 = 123.45
            byte[] input = {0x12, 0x34, 0x5C};
            assertEquals(new BigDecimal("123.45"), NumericCodec.decodeComp3(input, 2, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Decode negative with scale")
        void decodeNegativeWithScale() {
            // 12 34 5D with scale 2 = -123.45
            byte[] input = {0x12, 0x34, 0x5D};
            assertEquals(new BigDecimal("-123.45"), NumericCodec.decodeComp3(input, 2, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Invalid sign nibble throws exception")
        void invalidSignNibbleThrows() {
            // 12 30 has sign nibble 0, which is invalid
            byte[] input = {0x12, 0x30};
            assertThrows(InvalidSignNibbleException.class, () ->
                    NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Invalid digit nibble throws exception")
        void invalidDigitNibbleThrows() {
            // AB CD has digit A (10) in high nibble, which is invalid
            byte[] input = {(byte) 0xAB, (byte) 0xCD};
            assertThrows(InvalidDigitException.class, () ->
                    NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }

        @Test
        @DisplayName("Invalid digit in last byte high nibble")
        void invalidDigitInLastByte() {
            // 12 AC has A (10) in last byte high nibble
            byte[] input = {0x12, (byte) 0xAC};
            assertThrows(InvalidDigitException.class, () ->
                    NumericCodec.decodeComp3(input, 0, RECORD, FIELD, OFFSET));
        }
    }

    @Nested
    @DisplayName("COMP-3 Round-trip Tests")
    class Comp3RoundTripTests {

        @Test
        @DisplayName("Round-trip positive values")
        void roundTripPositive() {
            String[] values = {"0", "1", "12", "123", "1234", "12345"};
            for (String v : values) {
                BigDecimal value = new BigDecimal(v);
                byte[] encoded = NumericCodec.encodeComp3(value, 3, RECORD, FIELD, OFFSET);
                BigDecimal decoded = NumericCodec.decodeComp3(encoded, 0, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + v);
            }
        }

        @Test
        @DisplayName("Round-trip negative values")
        void roundTripNegative() {
            String[] values = {"-1", "-12", "-123", "-1234", "-12345"};
            for (String v : values) {
                BigDecimal value = new BigDecimal(v);
                byte[] encoded = NumericCodec.encodeComp3(value, 3, RECORD, FIELD, OFFSET);
                BigDecimal decoded = NumericCodec.decodeComp3(encoded, 0, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + v);
            }
        }

        @Test
        @DisplayName("Round-trip with scale")
        void roundTripWithScale() {
            String[] values = {"0.00", "1.23", "12.34", "123.45", "-0.01", "-12.34"};
            for (String v : values) {
                BigDecimal value = new BigDecimal(v);
                byte[] encoded = NumericCodec.encodeComp3(value, 3, RECORD, FIELD, OFFSET);
                BigDecimal decoded = NumericCodec.decodeComp3(encoded, 2, RECORD, FIELD, OFFSET);
                assertEquals(value, decoded, "Round-trip failed for: " + v);
            }
        }
    }

    @Nested
    @DisplayName("Golden Vector Tests")
    class GoldenVectorTests {

        @Test
        @DisplayName("COMP golden vectors")
        void compGoldenVectors() {
            // 2-byte vectors
            assertArrayEquals(new byte[]{0x00, 0x00}, NumericCodec.encodeComp(0, 2));
            assertArrayEquals(new byte[]{0x00, 0x01}, NumericCodec.encodeComp(1, 2));
            assertArrayEquals(new byte[]{0x7F, (byte) 0xFF}, NumericCodec.encodeComp(32767, 2));
            assertArrayEquals(new byte[]{(byte) 0x80, 0x00}, NumericCodec.encodeComp(-32768, 2));
            assertArrayEquals(new byte[]{(byte) 0xFF, (byte) 0xFF}, NumericCodec.encodeComp(-1, 2));

            // 4-byte vectors
            assertArrayEquals(new byte[]{0x00, 0x00, 0x00, 0x00}, NumericCodec.encodeComp(0, 4));
            assertArrayEquals(new byte[]{0x7F, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF},
                    NumericCodec.encodeComp(Integer.MAX_VALUE, 4));
            assertArrayEquals(new byte[]{(byte) 0x80, 0x00, 0x00, 0x00},
                    NumericCodec.encodeComp(Integer.MIN_VALUE, 4));
        }

        @Test
        @DisplayName("COMP-3 golden vectors")
        void comp3GoldenVectors() {
            // 2-byte vectors (3 digit capacity)
            assertArrayEquals(new byte[]{0x00, 0x0C},
                    NumericCodec.encodeComp3(new BigDecimal("0"), 2, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{0x00, 0x1C},
                    NumericCodec.encodeComp3(new BigDecimal("1"), 2, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{0x12, 0x3C},
                    NumericCodec.encodeComp3(new BigDecimal("123"), 2, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{0x12, 0x3D},
                    NumericCodec.encodeComp3(new BigDecimal("-123"), 2, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{(byte) 0x99, (byte) 0x9C},
                    NumericCodec.encodeComp3(new BigDecimal("999"), 2, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{(byte) 0x99, (byte) 0x9D},
                    NumericCodec.encodeComp3(new BigDecimal("-999"), 2, RECORD, FIELD, OFFSET));

            // 3-byte vectors (5 digit capacity)
            assertArrayEquals(new byte[]{0x01, 0x23, 0x4C},
                    NumericCodec.encodeComp3(new BigDecimal("1234"), 3, RECORD, FIELD, OFFSET));
            assertArrayEquals(new byte[]{0x12, 0x34, 0x5C},
                    NumericCodec.encodeComp3(new BigDecimal("12345"), 3, RECORD, FIELD, OFFSET));
        }
    }
}
