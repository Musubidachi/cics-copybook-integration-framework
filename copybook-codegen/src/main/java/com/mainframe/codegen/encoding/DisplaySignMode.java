package com.mainframe.codegen.encoding;

/**
 * Specifies how the sign is encoded for signed DISPLAY numeric fields.
 *
 * <p>COBOL DISPLAY numeric fields can represent signed values in several ways.
 * This enum defines the supported sign encoding modes:</p>
 *
 * <ul>
 *   <li>{@link #OVERPUNCH_TRAILING} - The sign is encoded in the zone nibble
 *       of the last digit byte (trailing overpunch). This is the most common
 *       format on IBM mainframes.</li>
 *   <li>{@link #UNSIGNED} - Field is unsigned; any attempt to encode a negative
 *       value will throw an exception.</li>
 * </ul>
 *
 * <p><strong>Overpunch encoding (IBM037):</strong> For trailing overpunch, the
 * last byte uses a modified zone nibble to indicate sign:
 * <ul>
 *   <li>Positive: zone nibble 0xC (or 0xF for unsigned/positive)</li>
 *   <li>Negative: zone nibble 0xD</li>
 * </ul>
 * This means the last byte value for digit N would be:
 * <ul>
 *   <li>Positive: 0xCN or 0xFN</li>
 *   <li>Negative: 0xDN</li>
 * </ul>
 * where N is 0-9.</p>
 */
public enum DisplaySignMode {

    /**
     * Trailing overpunch sign encoding.
     *
     * <p>The sign is encoded in the zone nibble of the last digit byte.
     * This does NOT add an extra byte; the sign information is embedded
     * in the final digit's zone portion.</p>
     *
     * <p>For IBM037:
     * <ul>
     *   <li>Positive 0-9: 0xC0-0xC9 (or 0xF0-0xF9)</li>
     *   <li>Negative 0-9: 0xD0-0xD9</li>
     * </ul>
     */
    OVERPUNCH_TRAILING,

    /**
     * Unsigned DISPLAY numeric.
     *
     * <p>All digits use the standard zone nibble (0xF for IBM037).
     * Attempting to encode a negative value will result in an error.</p>
     */
    UNSIGNED
}
