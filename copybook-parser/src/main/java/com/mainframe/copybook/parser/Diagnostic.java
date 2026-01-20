package com.mainframe.copybook.parser;

/**
 * Represents an error or warning encountered during normalization, tokenization
 * or parsing.  Diagnostics are collected and attached to the final AST rather
 * than thrown immediately, enabling callers to inspect all issues at once.
 */
public record Diagnostic(String category, String code, String message, SourceSpan span) {
}