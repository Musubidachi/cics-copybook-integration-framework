package com.mainframe.copybook.parser;

/**
 * Supported USAGE clause values.  These values correspond exactly to the
 * allowed encodings enumerated in copybook-parser-module-doc.md.  Any other
 * usage specified in a copybook should result in a parse error.
 */
public enum Usage {
    DISPLAY,
    COMP,
    COMP_3;

    /**
     * Look up a Usage by COBOL spelling.  The input may be any case and may
     * include a hyphen.
     *
     * @param lexeme lexeme from the token stream
     * @return the corresponding Usage value
     * @throws IllegalArgumentException if the lexeme does not map to a known usage
     */
    public static Usage fromLexeme(String lexeme) {
        var normalized = lexeme.toUpperCase().replace('-', '_');
        return switch (normalized) {
            case "DISPLAY" -> DISPLAY;
            case "COMP" -> COMP;
            case "COMP_3" -> COMP_3;
            default -> throw new IllegalArgumentException("Unsupported USAGE: " + lexeme);
        };
    }
}