package com.mainframe.copybook.parser;

/**
 * Closed set of token categories produced by the tokenizer.  Categories are
 * intentionally coarse; the parser is responsible for interpreting tokens
 * according to context (e.g. distinguishing level numbers from integers in
 * clause bodies).
 */
public enum TokenType {
    /** Two‑digit level numbers in the range 01–49, 66, 77, 88. */
    LEVEL_NUMBER,
    /** Identifiers (data names) and unrecognized keywords. */
    IDENTIFIER,
    /** COBOL keywords such as PIC, USAGE, OCCURS, REDEFINES, COPY, TIMES, VALUE, THRU, etc. */
    KEYWORD,
    /** Arbitrary integer literals used in OCCURS counts and PIC repeats. */
    INTEGER,
    /** Signed or decimal numeric literals. */
    NUMERIC_LITERAL,
    /** String literals enclosed in single quotes. */
    STRING_LITERAL,
    /** Punctuation marks: '.', '(', ')', 'V'. */
    PUNCT,
    /** Single character PIC symbols: X, 9, S. */
    PIC_SYMBOL,
    /** The == delimiter used in COPY REPLACING clauses. */
    PSEUDO_TEXT_DELIMITER,
    /** End of file sentinel produced by the tokenizer. */
    EOF
}