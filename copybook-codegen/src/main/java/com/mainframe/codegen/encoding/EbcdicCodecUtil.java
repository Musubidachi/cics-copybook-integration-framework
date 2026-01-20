package com.mainframe.codegen.encoding;

import java.nio.charset.Charset;

/**
 * Utilities for encoding and decoding EBCDIC (IBM037) strings.
 *
 * This helper provides fixed-length padding and trimming behaviour
 * consistent with DISPLAY fields in COBOL copybooks. It always uses
 * the IBM037 charset and pads unused bytes with the EBCDIC space (0x40).
 */
public final class EbcdicCodecUtil {

    private static final Charset IBM037 = Charset.forName("IBM037");

    private EbcdicCodecUtil() {
        // utility class
    }

    /**
     * Encode a Java {@link String} into an EBCDIC byte array of the given length.
     * If the string is shorter than the length, the remainder is padded with
     * EBCDIC spaces. If it is longer, it is truncated.
     *
     * @param value  the input string, may be {@code null}
     * @param length the fixed byte length of the output
     * @return a byte array of exactly {@code length} bytes
     */
    public static byte[] encode(String value, int length) {
        if (value == null) {
            value = "";
        }
        byte[] bytes = value.getBytes(IBM037);
        if (bytes.length > length) {
            byte[] truncated = new byte[length];
            System.arraycopy(bytes, 0, truncated, 0, length);
            return truncated;
        }
        byte[] result = new byte[length];
        System.arraycopy(bytes, 0, result, 0, bytes.length);
        // pad unused bytes with EBCDIC space
        for (int i = bytes.length; i < length; i++) {
            result[i] = (byte) 0x40;
        }
        return result;
    }

    /**
     * Decode a fixed-length EBCDIC byte array into a Java {@link String}.
     * Trailing EBCDIC spaces are trimmed.
     *
     * @param bytes the EBCDIC bytes to decode
     * @return the decoded and trimmed string
     */
    public static String decode(byte[] bytes) {
        String raw = new String(bytes, IBM037);
        return raw.replaceAll("\\s+$", "");
    }
}
