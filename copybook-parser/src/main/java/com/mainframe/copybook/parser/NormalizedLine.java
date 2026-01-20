package com.mainframe.copybook.parser;

/**
 * A NormalizedLine represents a single physical line of a COBOL copybook after
 * fixed‑format normalization.  During normalization columns 1–6 are dropped,
 * column 7 is used exclusively for the comment indicator, and columns 8–72
 * comprise the program text.  The original line number is preserved so that
 * diagnostics can map back to source.
 *
 * @param lineNumber 1‑based physical line number in the input file
 * @param text       the extracted program text from columns 8–72 (may be blank)
 */
public record NormalizedLine(int lineNumber, String text) {
}