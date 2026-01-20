package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.AstNode;
import com.mainframe.copybook.parser.ast.CopybookAst;
import com.mainframe.copybook.parser.ast.CopyNode;
import com.mainframe.copybook.parser.ast.DataItemNode;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Public API for parsing COBOL copybooks.
 * This is the main entry point for the copybook-parser module.
 */
public final class CopybookParser {

    private CopybookParser() {
        // Utility class
    }

    /**
     * Parse a copybook from a Reader.
     *
     * @param copybookName the name of the copybook (for diagnostics)
     * @param source       the source reader
     * @param resolver     resolver for COPY statements
     * @param options      parser options
     * @return the parsed AST
     * @throws IOException if reading fails
     */
    public static CopybookAst parse(
            String copybookName,
            Reader source,
            CopybookResolver resolver,
            ParserOptions options
    ) throws IOException {
        // Read source content
        String content = readAll(source);
        return parseString(copybookName, content, resolver, options);
    }

    /**
     * Parse a copybook from a string.
     *
     * @param copybookName the name of the copybook (for diagnostics)
     * @param source       the source text
     * @param resolver     resolver for COPY statements
     * @param options      parser options
     * @return the parsed AST
     */
    public static CopybookAst parseString(
            String copybookName,
            String source,
            CopybookResolver resolver,
            ParserOptions options
    ) {
        // Phase 1: Normalize
        List<NormalizedLine> lines = Normalizer.normalize(source);

        // Phase 2: Tokenize
        Tokenizer tokenizer = new Tokenizer();
        List<Token> tokens = tokenizer.tokenize(lines);
        List<Diagnostic> diagnostics = new ArrayList<>(tokenizer.diagnostics());

        // Phase 3: Parse
        Parser parser = new Parser(tokens);
        CopybookAst ast = parser.parse();
        diagnostics.addAll(ast.diagnostics());

        // Phase 4: COPY expansion (if enabled)
        List<AstNode> roots = new ArrayList<>(ast.roots());
        if (options.expandCopy() && resolver != null) {
            Set<String> visited = new HashSet<>();
            visited.add(copybookName);
            roots = expandCopyStatements(roots, resolver, options, diagnostics, visited);
        }

        return new CopybookAst(roots, diagnostics);
    }

    /**
     * Parse a copybook from a string with default options and no COPY resolution.
     *
     * @param source the source text
     * @return the parsed AST
     */
    public static CopybookAst parseString(String source) {
        return parseString("unnamed", source, CopybookResolver.NONE, ParserOptions.NO_EXPANSION);
    }

    /**
     * Expand COPY statements in the AST.
     */
    private static List<AstNode> expandCopyStatements(
            List<AstNode> roots,
            CopybookResolver resolver,
            ParserOptions options,
            List<Diagnostic> diagnostics,
            Set<String> visited
    ) {
        List<AstNode> expanded = new ArrayList<>();

        for (AstNode node : roots) {
            if (node instanceof CopyNode copyNode) {
                String name = copyNode.copybookName();

                // Check for circular inclusion
                if (visited.contains(name)) {
                    diagnostics.add(new Diagnostic(
                            "ParseError",
                            "COPY_CIRCULAR",
                            "Circular COPY detected: " + name,
                            copyNode.span()
                    ));
                    continue;
                }

                // Resolve the copybook
                Optional<Reader> resolved = resolver.resolve(name);
                if (resolved.isEmpty()) {
                    diagnostics.add(new Diagnostic(
                            "ParseError",
                            "COPYBOOK_NOT_FOUND",
                            "Copybook not found: " + name,
                            copyNode.span()
                    ));
                    // Keep the CopyNode in the AST when not expanded
                    expanded.add(copyNode);
                    continue;
                }

                try {
                    String content = readAll(resolved.get());

                    // Apply REPLACING if present
                    if (!copyNode.replacingPairs().isEmpty()) {
                        content = applyReplacing(content, copyNode.replacingPairs());
                    }

                    // Parse the included copybook
                    visited.add(name);
                    CopybookAst includedAst = parseString(name, content, resolver,
                            ParserOptions.builder()
                                    .expandCopy(options.expandCopy())
                                    .strictMode(options.strictMode())
                                    .trackSourcePositions(options.trackSourcePositions())
                                    .build());
                    visited.remove(name);

                    diagnostics.addAll(includedAst.diagnostics());

                    // Add expanded nodes
                    expanded.addAll(includedAst.roots());

                } catch (IOException e) {
                    diagnostics.add(new Diagnostic(
                            "ParseError",
                            "COPY_READ_ERROR",
                            "Error reading copybook " + name + ": " + e.getMessage(),
                            copyNode.span()
                    ));
                    expanded.add(copyNode);
                }
            } else if (node instanceof DataItemNode dataItem) {
                // Recursively expand any COPY statements in children
                // (though typically COPY statements appear at root level)
                expanded.add(dataItem);
            } else {
                expanded.add(node);
            }
        }

        return expanded;
    }

    /**
     * Apply REPLACING transformations to copybook content.
     * This performs token-level replacement.
     */
    private static String applyReplacing(String content, List<CopyNode.ReplacingPair> pairs) {
        // Normalize and tokenize for proper token-level replacement
        List<NormalizedLine> lines = Normalizer.normalize(content);
        Tokenizer tokenizer = new Tokenizer();
        List<Token> tokens = tokenizer.tokenize(lines);

        StringBuilder result = new StringBuilder();
        int lastLine = 1;
        int lastCol = 8;

        for (Token token : tokens) {
            if (token.type() == TokenType.EOF) {
                break;
            }

            // Add whitespace/newlines between tokens
            while (lastLine < token.line()) {
                result.append("\n       "); // COBOL fixed format
                lastLine++;
                lastCol = 8;
            }
            while (lastCol < token.column()) {
                result.append(' ');
                lastCol++;
            }

            // Check if token should be replaced
            String lexeme = token.lexeme();
            String replacement = lexeme;
            for (CopyNode.ReplacingPair pair : pairs) {
                if (lexeme.equals(pair.from()) || lexeme.equalsIgnoreCase(pair.from())) {
                    replacement = pair.to();
                    break;
                }
                // Also check for partial matches in identifiers (e.g., :TAG: in :TAG:-ID)
                if (lexeme.contains(pair.from())) {
                    replacement = lexeme.replace(pair.from(), pair.to());
                    break;
                }
            }

            result.append(replacement);
            lastCol += replacement.length();
        }

        return result.toString();
    }

    /**
     * Read all content from a Reader into a String.
     */
    private static String readAll(Reader reader) throws IOException {
        StringBuilder sb = new StringBuilder();
        char[] buffer = new char[4096];
        int read;
        while ((read = reader.read(buffer)) != -1) {
            sb.append(buffer, 0, read);
        }
        return sb.toString();
    }
}
