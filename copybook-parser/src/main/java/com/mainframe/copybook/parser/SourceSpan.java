package com.mainframe.copybook.parser;

/**
 * Encapsulates a contiguous region in the input source.  Spans are defined
 * using 1‑based line and column coordinates and are inclusive at both ends.
 */
public record SourceSpan(int startLine, int startColumn, int endLine, int endColumn) {
    /**
     * Construct a span representing a single point.  Useful for tokens and
     * diagnostics that refer to a single location.
     */
    public static SourceSpan single(int line, int column) {
        return new SourceSpan(line, column, line, column);
    }
}