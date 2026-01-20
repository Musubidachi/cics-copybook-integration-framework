package com.mainframe.copybook.parser;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;

/**
 * Converts normalized program lines into a flat stream of tokens.  The
 * tokenizer follows the lexical rules in copybook-parser-module-doc.md.  It
 * reports LexicalError diagnostics for unknown characters but continues to
 * produce a token stream to permit best-effort parsing of partially valid
 * copybooks.
 */
public final class Tokenizer {
    private static final Set<String> KEYWORDS = new HashSet<>(Set.of(
            "PIC", "PICTURE", "USAGE", "OCCURS", "REDEFINES", "COPY", "TIMES",
            "VALUE", "VALUES", "THRU", "THROUGH", "REPLACING", "BY",
            "DEPENDING", "ON", "TO", "RENAMES", "IS", "COMP-3"
    ));

    private final List<Token> tokens = new ArrayList<>();
    private final List<Diagnostic> diagnostics = new ArrayList<>();

    /**
     * Tokenize the given normalized lines into a list of tokens terminated by
     * EOF.  Any lexical errors encountered are captured in diagnostics and do
     * not interrupt tokenization.
     *
     * @param lines normalized lines from the normalizer
     * @return list of tokens including EOF sentinel
     */
    public List<Token> tokenize(List<NormalizedLine> lines) {
        tokens.clear();
        diagnostics.clear();
        for (NormalizedLine line : lines) {
            String text = line.text();
            int len = text.length();
            boolean firstToken = true;
            int i = 0;
            while (i < len) {
                char c = text.charAt(i);
                int column = 8 + i; // columns are 1-based; normalized text starts at column 8
                if (Character.isWhitespace(c)) {
                    i++;
                    continue;
                }

                // Pseudo-text delimiter ==
                if (c == '=' && i + 1 < len && text.charAt(i + 1) == '=') {
                    tokens.add(new Token(TokenType.PSEUDO_TEXT_DELIMITER, "==", line.lineNumber(), column));
                    i += 2;
                    firstToken = false;
                    continue;
                }

                // String literal in single quotes
                if (c == '\'') {
                    int start = i;
                    i++; // skip opening quote
                    StringBuilder sb = new StringBuilder();
                    while (i < len) {
                        char ch = text.charAt(i);
                        if (ch == '\'') {
                            // Check for escaped quote ''
                            if (i + 1 < len && text.charAt(i + 1) == '\'') {
                                sb.append('\'');
                                i += 2;
                            } else {
                                i++; // skip closing quote
                                break;
                            }
                        } else {
                            sb.append(ch);
                            i++;
                        }
                    }
                    tokens.add(new Token(TokenType.STRING_LITERAL, sb.toString(), line.lineNumber(), 8 + start));
                    firstToken = false;
                    continue;
                }

                // Punctuation tokens (but not V in PIC context - that's handled differently)
                if (c == '.' || c == '(' || c == ')') {
                    tokens.add(new Token(TokenType.PUNCT, String.valueOf(c), line.lineNumber(), column));
                    i++;
                    firstToken = false;
                    continue;
                }

                // V as punctuation (for PIC clause virtual decimal)
                if ((c == 'V' || c == 'v') && i + 1 < len) {
                    char next = text.charAt(i + 1);
                    // If V is followed by a digit or '(' or '9' it's a virtual decimal point
                    if (Character.isDigit(next) || next == '(' || next == '9') {
                        tokens.add(new Token(TokenType.PUNCT, "V", line.lineNumber(), column));
                        i++;
                        firstToken = false;
                        continue;
                    }
                }

                // Signed numeric literal: starts with + or -
                if ((c == '+' || c == '-') && i + 1 < len && Character.isDigit(text.charAt(i + 1))) {
                    int start = i;
                    i++; // skip sign
                    while (i < len && (Character.isDigit(text.charAt(i)) || text.charAt(i) == '.')) {
                        i++;
                    }
                    String literal = text.substring(start, i);
                    tokens.add(new Token(TokenType.NUMERIC_LITERAL, literal, line.lineNumber(), 8 + start));
                    firstToken = false;
                    continue;
                }

                // Numeric token: sequence of digits (possibly with decimal point)
                if (Character.isDigit(c)) {
                    int start = i;
                    boolean hasDecimal = false;
                    while (i < len) {
                        char ch = text.charAt(i);
                        if (Character.isDigit(ch)) {
                            i++;
                        } else if (ch == '.' && !hasDecimal && i + 1 < len && Character.isDigit(text.charAt(i + 1))) {
                            // This is a decimal point in a numeric literal
                            hasDecimal = true;
                            i++;
                        } else {
                            break;
                        }
                    }
                    String digits = text.substring(start, i);
                    TokenType type;
                    if (hasDecimal) {
                        type = TokenType.NUMERIC_LITERAL;
                    } else if (firstToken && (digits.length() == 2 || digits.equals("88") || digits.equals("66") || digits.equals("77"))) {
                        type = TokenType.LEVEL_NUMBER;
                    } else {
                        type = TokenType.INTEGER;
                    }
                    tokens.add(new Token(type, digits, line.lineNumber(), 8 + start));
                    firstToken = false;
                    continue;
                }

                // Identifier or keyword or PIC symbol (allow colon for :TAG: style placeholders)
                if (Character.isLetter(c) || c == '-' || c == ':') {
                    int start = i;
                    while (i < len) {
                        char ch = text.charAt(i);
                        if (Character.isLetterOrDigit(ch) || ch == '-' || ch == ':') {
                            i++;
                        } else {
                            break;
                        }
                    }
                    String lexeme = text.substring(start, i);
                    String upper = lexeme.toUpperCase(Locale.ROOT);
                    TokenType type;
                    if (KEYWORDS.contains(upper)) {
                        type = TokenType.KEYWORD;
                        // normalize keyword lexeme to uppercase to ease downstream comparison
                        lexeme = upper;
                    } else if (lexeme.length() == 1 && (upper.equals("X") || upper.equals("9") || upper.equals("S"))) {
                        type = TokenType.PIC_SYMBOL;
                        lexeme = upper;
                    } else if (upper.equals("V")) {
                        // Standalone V is a virtual decimal
                        type = TokenType.PUNCT;
                        lexeme = upper;
                    } else {
                        type = TokenType.IDENTIFIER;
                    }
                    tokens.add(new Token(type, lexeme, line.lineNumber(), 8 + start));
                    firstToken = false;
                    continue;
                }

                // Unknown character: report lexical error and skip
                diagnostics.add(new Diagnostic(
                        "LexicalError",
                        "LEX1",
                        "Unrecognized character '" + c + "'",
                        SourceSpan.single(line.lineNumber(), column)
                ));
                i++;
            }
        }
        // Add EOF sentinel token at the end; line/column after last line
        int eofLine = lines.isEmpty() ? 1 : lines.get(lines.size() - 1).lineNumber();
        tokens.add(new Token(TokenType.EOF, "", eofLine + 1, 1));
        return tokens;
    }

    /**
     * Return any diagnostics produced during the most recent call to
     * {@link #tokenize(List)}.  The returned list is unmodifiable.
     */
    public List<Diagnostic> diagnostics() {
        return List.copyOf(diagnostics);
    }
}
