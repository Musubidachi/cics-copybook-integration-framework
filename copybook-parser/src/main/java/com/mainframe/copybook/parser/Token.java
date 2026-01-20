package com.mainframe.copybook.parser;

/**
 * A lexical token produced by the tokenizer.  Each token carries its type,
 * the original lexeme (exact spelling), and the source position in
 * 1‑based line/column coordinates.
 *
 * Column numbers are based on COBOL fixed‑format positions; normalization
 * discards columns 1–7 but the tokenizer reports positions relative to
 * column 1 of the original file for accurate diagnostics.
 *
 * @param type   category of token
 * @param lexeme original text value (case preserved for identifiers)
 * @param line   1‑based line number
 * @param column 1‑based column number in original file
 */
public record Token(TokenType type, String lexeme, int line, int column) {
    @Override
    public String toString() {
        return "%s('%s' @ %d:%d)".formatted(type, lexeme, line, column);
    }
}