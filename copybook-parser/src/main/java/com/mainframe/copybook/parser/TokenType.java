package com.mainframe.copybook.parser;

/**
 * Closed set of token categories produced by the tokenizer.  Categories are
 * intentionally coarse; the parser is responsible for interpreting tokens
 * according to context (e.g. distinguishing level numbers from integers in
 * clause bodies).
 */
public enum TokenType {
    /** Two‑digit level numbers in the range 01–49. */
    LEVEL_NUMBER,
    /** Identifiers (data names) and unrecognized keywords. */
    IDENTIFIER,
    /** COBOL keywords such as PIC, USAGE, OCCURS, REDEFINES, COPY, TIMES. */
    KEYWORD,
    /** Arbitrary integer literals used in OCCURS counts and PIC repeats. */
    INTEGER,
    /** Punctuation marks: '.', '(', ')', 'V'. */
    PUNCT,
    /** Single character PIC symbols: X, 9, S. */
    PIC_SYMBOL,
    /** End of file sentinel produced by the tokenizer. */
    EOF
}