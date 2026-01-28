package com.mainframe.codegen.encoding;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for {@link EbcdicCodecUtil} covering EBCDIC string encoding/decoding.
 *
 * <p>IBM037 EBCDIC reference values:</p>
 * <ul>
 *   <li>Space: 0x40</li>
 *   <li>'A'-'Z': 0xC1-0xE9 (with gaps)</li>
 *   <li>'0'-'9': 0xF0-0xF9</li>
 * </ul>
 */
class EbcdicCodecUtilTest {

    @Nested
    @DisplayName("EBCDIC Encoding")
    class EncodingTests {

        @Test
        @DisplayName("Encode simple string")
        void encodeSimple() {
            // "ALICE" in IBM037
            byte[] result = EbcdicCodecUtil.encode("ALICE", 5);
            byte[] expected = {(byte) 0xC1, (byte) 0xD3, (byte) 0xC9, (byte) 0xC3, (byte) 0xC5};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode with padding")
        void encodeWithPadding() {
            // "AB" in 5 bytes with space padding
            byte[] result = EbcdicCodecUtil.encode("AB", 5);
            byte[] expected = {(byte) 0xC1, (byte) 0xC2, (byte) 0x40, (byte) 0x40, (byte) 0x40};
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode with truncation")
        void encodeWithTruncation() {
            // "HELLO" truncated to 3 bytes
            byte[] result = EbcdicCodecUtil.encode("HELLO", 3);
            byte[] expected = {(byte) 0xC8, (byte) 0xC5, (byte) 0xD3}; // "HEL"
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode empty string")
        void encodeEmpty() {
            byte[] result = EbcdicCodecUtil.encode("", 3);
            byte[] expected = {(byte) 0x40, (byte) 0x40, (byte) 0x40}; // All spaces
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode null string")
        void encodeNull() {
            byte[] result = EbcdicCodecUtil.encode(null, 3);
            byte[] expected = {(byte) 0x40, (byte) 0x40, (byte) 0x40}; // All spaces
            assertArrayEquals(expected, result);
        }

        @Test
        @DisplayName("Encode digits")
        void encodeDigits() {
            // "0123456789" in IBM037
            byte[] result = EbcdicCodecUtil.encode("0123456789", 10);
            byte[] expected = {
                    (byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4,
                    (byte) 0xF5, (byte) 0xF6, (byte) 0xF7, (byte) 0xF8, (byte) 0xF9
            };
            assertArrayEquals(expected, result);
        }
    }

    @Nested
    @DisplayName("EBCDIC Decoding")
    class DecodingTests {

        @Test
        @DisplayName("Decode simple string")
        void decodeSimple() {
            byte[] input = {(byte) 0xC1, (byte) 0xD3, (byte) 0xC9, (byte) 0xC3, (byte) 0xC5};
            assertEquals("ALICE", EbcdicCodecUtil.decode(input));
        }

        @Test
        @DisplayName("Decode with trailing spaces trimmed")
        void decodeWithTrim() {
            byte[] input = {(byte) 0xC1, (byte) 0xC2, (byte) 0x40, (byte) 0x40, (byte) 0x40};
            assertEquals("AB", EbcdicCodecUtil.decode(input));
        }

        @Test
        @DisplayName("Decode all spaces returns empty string")
        void decodeAllSpaces() {
            byte[] input = {(byte) 0x40, (byte) 0x40, (byte) 0x40};
            assertEquals("", EbcdicCodecUtil.decode(input));
        }

        @Test
        @DisplayName("Decode digits")
        void decodeDigits() {
            byte[] input = {
                    (byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xF3, (byte) 0xF4,
                    (byte) 0xF5, (byte) 0xF6, (byte) 0xF7, (byte) 0xF8, (byte) 0xF9
            };
            assertEquals("0123456789", EbcdicCodecUtil.decode(input));
        }

        @Test
        @DisplayName("Decode with embedded spaces preserved")
        void decodeEmbeddedSpaces() {
            // "A B" with trailing spaces
            byte[] input = {(byte) 0xC1, (byte) 0x40, (byte) 0xC2, (byte) 0x40, (byte) 0x40};
            assertEquals("A B", EbcdicCodecUtil.decode(input));
        }
    }

    @Nested
    @DisplayName("Round-trip Tests")
    class RoundTripTests {

        @Test
        @DisplayName("Round-trip various strings")
        void roundTripStrings() {
            String[] values = {"HELLO", "WORLD", "TEST", "A", "AB", "ABC", "ABCDEF"};
            for (String value : values) {
                byte[] encoded = EbcdicCodecUtil.encode(value, 10);
                String decoded = EbcdicCodecUtil.decode(encoded);
                assertEquals(value, decoded, "Round-trip failed for: " + value);
            }
        }

        @Test
        @DisplayName("Round-trip with exact length")
        void roundTripExactLength() {
            String value = "EXACT";
            byte[] encoded = EbcdicCodecUtil.encode(value, 5);
            String decoded = EbcdicCodecUtil.decode(encoded);
            assertEquals(value, decoded);
        }
    }

    @Nested
    @DisplayName("Golden Vector Tests")
    class GoldenVectorTests {

        @Test
        @DisplayName("EBCDIC alphabet golden vectors (selected letters)")
        void alphabetGoldenVectors() {
            // Selected letters and their EBCDIC codes
            assertEquals((byte) 0xC1, EbcdicCodecUtil.encode("A", 1)[0]); // A
            assertEquals((byte) 0xC2, EbcdicCodecUtil.encode("B", 1)[0]); // B
            assertEquals((byte) 0xC3, EbcdicCodecUtil.encode("C", 1)[0]); // C
            assertEquals((byte) 0xC9, EbcdicCodecUtil.encode("I", 1)[0]); // I
            assertEquals((byte) 0xD1, EbcdicCodecUtil.encode("J", 1)[0]); // J
            assertEquals((byte) 0xD9, EbcdicCodecUtil.encode("R", 1)[0]); // R
            assertEquals((byte) 0xE2, EbcdicCodecUtil.encode("S", 1)[0]); // S
            assertEquals((byte) 0xE9, EbcdicCodecUtil.encode("Z", 1)[0]); // Z
        }

        @Test
        @DisplayName("EBCDIC digit golden vectors")
        void digitGoldenVectors() {
            for (int i = 0; i <= 9; i++) {
                byte[] encoded = EbcdicCodecUtil.encode(String.valueOf(i), 1);
                assertEquals((byte) (0xF0 + i), encoded[0], "Digit " + i + " mismatch");
            }
        }

        @Test
        @DisplayName("EBCDIC space golden vector")
        void spaceGoldenVector() {
            assertEquals((byte) 0x40, EbcdicCodecUtil.encode(" ", 1)[0]);
        }
    }
}
