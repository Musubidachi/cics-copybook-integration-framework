package com.mainframe.copybook.parser.ast;

import com.mainframe.copybook.parser.Diagnostic;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The top‑level result of parsing a COBOL copybook.  A CopybookAst contains
 * a list of top‑level AST nodes (either DataItemNode roots or CopyNode
 * declarations) and a list of diagnostics that were produced during
 * normalization, tokenization or parsing.  Clients should consult the
 * diagnostics list before proceeding; a non‑empty list indicates errors
 * or warnings occurred.
 *
 * @param roots       immutable list of AST root nodes
 * @param diagnostics immutable list of diagnostics
 */
public record CopybookAst(List<AstNode> roots, List<Diagnostic> diagnostics) {
    public CopybookAst {
        if (roots == null) {
            roots = Collections.emptyList();
        } else {
            roots = Collections.unmodifiableList(new ArrayList<>(roots));
        }
        if (diagnostics == null) {
            diagnostics = Collections.emptyList();
        } else {
            diagnostics = Collections.unmodifiableList(new ArrayList<>(diagnostics));
        }
    }
}