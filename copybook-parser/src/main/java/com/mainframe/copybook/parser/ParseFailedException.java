package com.mainframe.copybook.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Exception thrown when parsing fails in strict mode.
 * Contains all diagnostics collected during parsing.
 */
public final class ParseFailedException extends RuntimeException {

    private final List<Diagnostic> diagnostics;

    /**
     * Create a new ParseFailedException with the given diagnostics.
     *
     * @param diagnostics the diagnostics that caused the failure
     */
    public ParseFailedException(List<Diagnostic> diagnostics) {
        super(buildMessage(diagnostics));
        this.diagnostics = Collections.unmodifiableList(new ArrayList<>(diagnostics));
    }

    /**
     * Create a new ParseFailedException with a single diagnostic.
     *
     * @param diagnostic the diagnostic that caused the failure
     */
    public ParseFailedException(Diagnostic diagnostic) {
        this(List.of(diagnostic));
    }

    /**
     * Get the diagnostics that caused this failure.
     *
     * @return unmodifiable list of diagnostics
     */
    public List<Diagnostic> diagnostics() {
        return diagnostics;
    }

    /**
     * Get the error diagnostics (category "error" or code starts with error indicator).
     *
     * @return list of error diagnostics
     */
    public List<Diagnostic> errors() {
        return diagnostics.stream()
                .filter(d -> isError(d))
                .toList();
    }

    /**
     * Check if a diagnostic is an error (vs warning).
     */
    private static boolean isError(Diagnostic d) {
        String category = d.category().toLowerCase();
        return category.contains("error") ||
               category.equals("parseerror") ||
               category.equals("lexicalerror");
    }

    private static String buildMessage(List<Diagnostic> diagnostics) {
        if (diagnostics.isEmpty()) {
            return "Parse failed with no diagnostics";
        }

        if (diagnostics.size() == 1) {
            Diagnostic d = diagnostics.get(0);
            return formatDiagnostic(d);
        }

        StringBuilder sb = new StringBuilder();
        sb.append("Parse failed with ").append(diagnostics.size()).append(" error(s):\n");
        for (Diagnostic d : diagnostics) {
            sb.append("  - ").append(formatDiagnostic(d)).append("\n");
        }
        return sb.toString();
    }

    private static String formatDiagnostic(Diagnostic d) {
        StringBuilder sb = new StringBuilder();
        if (d.span() != null) {
            sb.append("Line ").append(d.span().startLine())
              .append(":").append(d.span().startColumn()).append(" ");
        }
        sb.append("[").append(d.code()).append("] ").append(d.message());
        return sb.toString();
    }
}
