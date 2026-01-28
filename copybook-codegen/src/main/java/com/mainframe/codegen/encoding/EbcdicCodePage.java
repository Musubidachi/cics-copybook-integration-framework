package com.mainframe.codegen.encoding;

import java.nio.charset.Charset;

/**
 * Enumerates supported EBCDIC code pages for character and numeric encoding.
 *
 * <p>The default and most common code page for IBM mainframe CICS applications
 * is IBM037 (US EBCDIC). Other code pages may be added as needed.</p>
 */
public enum EbcdicCodePage {

    /**
     * IBM037 - US/Canada EBCDIC code page.
     *
     * <p>This is the standard code page for most US-based IBM mainframe
     * applications. Character encoding uses the IBM037 charset, and
     * zoned decimal encoding uses:
     * <ul>
     *   <li>Digit bytes: 0xF0-0xF9 for digits 0-9</li>
     *   <li>Positive overpunch: 0xC0-0xC9 for digits 0-9</li>
     *   <li>Negative overpunch: 0xD0-0xD9 for digits 0-9</li>
     *   <li>EBCDIC space: 0x40</li>
     * </ul>
     */
    IBM037("IBM037", (byte) 0x40);

    private final String charsetName;
    private final byte ebcdicSpace;
    private final Charset charset;

    EbcdicCodePage(String charsetName, byte ebcdicSpace) {
        this.charsetName = charsetName;
        this.ebcdicSpace = ebcdicSpace;
        this.charset = Charset.forName(charsetName);
    }

    /**
     * @return the Java charset name for this code page
     */
    public String getCharsetName() {
        return charsetName;
    }

    /**
     * @return the EBCDIC space character byte value
     */
    public byte getEbcdicSpace() {
        return ebcdicSpace;
    }

    /**
     * @return the Java Charset for this code page
     */
    public Charset getCharset() {
        return charset;
    }

    /**
     * Returns the EBCDIC byte for an unsigned digit (0-9).
     *
     * @param digit the digit value (0-9)
     * @return the EBCDIC byte representing this digit
     * @throws IllegalArgumentException if digit is not 0-9
     */
    public byte digitToByte(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return (byte) (0xF0 + digit);
    }

    /**
     * Returns the digit value (0-9) for an unsigned EBCDIC digit byte.
     *
     * @param b the EBCDIC byte (expected 0xF0-0xF9)
     * @return the digit value (0-9), or -1 if invalid
     */
    public int byteToDigit(byte b) {
        int unsigned = b & 0xFF;
        if (unsigned >= 0xF0 && unsigned <= 0xF9) {
            return unsigned - 0xF0;
        }
        return -1;
    }

    /**
     * Returns the EBCDIC overpunch byte for a signed digit with positive sign.
     *
     * @param digit the digit value (0-9)
     * @return the positive overpunch byte (0xC0-0xC9 for IBM037)
     * @throws IllegalArgumentException if digit is not 0-9
     */
    public byte positiveOverpunchByte(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return (byte) (0xC0 + digit);
    }

    /**
     * Returns the EBCDIC overpunch byte for a signed digit with negative sign.
     *
     * @param digit the digit value (0-9)
     * @return the negative overpunch byte (0xD0-0xD9 for IBM037)
     * @throws IllegalArgumentException if digit is not 0-9
     */
    public byte negativeOverpunchByte(int digit) {
        if (digit < 0 || digit > 9) {
            throw new IllegalArgumentException("Digit must be 0-9, got: " + digit);
        }
        return (byte) (0xD0 + digit);
    }

    /**
     * Decodes an overpunch byte to extract sign and digit information.
     *
     * @param b the overpunch byte
     * @return an array of [sign, digit] where sign is 1 for positive, -1 for negative,
     *         or null if the byte is not a valid overpunch
     */
    public int[] decodeOverpunch(byte b) {
        int unsigned = b & 0xFF;
        int zone = (unsigned >> 4) & 0x0F;
        int digit = unsigned & 0x0F;

        if (digit > 9) {
            return null; // Invalid digit nibble
        }

        switch (zone) {
            case 0x0C: // Positive overpunch
            case 0x0F: // Unsigned/positive (standard digit zone)
            case 0x0A: // Some systems use A for positive
            case 0x0E: // Some systems use E for positive
                return new int[]{1, digit};
            case 0x0D: // Negative overpunch
            case 0x0B: // Some systems use B for negative
                return new int[]{-1, digit};
            default:
                return null; // Unknown zone
        }
    }

    /**
     * Checks if a byte is a valid unsigned EBCDIC digit.
     *
     * @param b the byte to check
     * @return true if the byte is a valid unsigned digit (0xF0-0xF9)
     */
    public boolean isValidDigit(byte b) {
        int unsigned = b & 0xFF;
        return unsigned >= 0xF0 && unsigned <= 0xF9;
    }

    /**
     * Checks if a byte is a valid overpunch byte (signed or unsigned digit).
     *
     * @param b the byte to check
     * @return true if the byte represents a valid signed or unsigned digit
     */
    public boolean isValidOverpunch(byte b) {
        return decodeOverpunch(b) != null;
    }
}
