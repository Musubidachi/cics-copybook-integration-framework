package com.mainframe.copybook.parser;

import java.util.ArrayList;
import java.util.List;

/**
 * Static utility to normalize raw copybook text into a sequence of program
 * lines.  Normalization applies the fixed‑format rules described in the
 * module documentation: columns 1–6 are ignored, column 7 denotes a full
 * comment line when it contains '*', and only columns 8–72 inclusive are
 * retained for parsing.  Lines shorter than 8 characters contribute an
 * empty text segment.  The original line numbers are preserved.
 */
public final class Normalizer {
    private Normalizer() {}

    /**
     * Normalize the input copybook text.  The input may contain Windows
     * (CRLF) or Unix (LF) line separators.  Trailing carriage returns are
     * ignored.  No trimming is performed on the extracted program text.
     *
     * @param source raw copybook text
     * @return list of normalized lines in order of appearance
     */
    public static List<NormalizedLine> normalize(String source) {
        List<NormalizedLine> lines = new ArrayList<>();
        if (source == null || source.isEmpty()) {
            return lines;
        }
        String[] rawLines = source.split("\r?\n", -1);
        for (int i = 0; i < rawLines.length; i++) {
            String raw = rawLines[i];
            int lineNumber = i + 1;
            // Determine if this is a comment line: column 7 (index 6) contains '*'
            char indicator = raw.length() >= 7 ? raw.charAt(6) : ' ';
            if (indicator == '*') {
                continue; // skip comment lines entirely
            }
            // Extract columns 8–72 inclusive (indices 7–71).  If the line is
            // shorter, extract empty or partial substring.
            int start = 7;
            int end = Math.min(raw.length(), 72);
            String text = start < end ? raw.substring(start, end) : "";
            lines.add(new NormalizedLine(lineNumber, text));
        }
        return lines;
    }
}