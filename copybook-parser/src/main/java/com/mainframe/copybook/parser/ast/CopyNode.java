package com.mainframe.copybook.parser.ast;

import com.mainframe.copybook.parser.SourceSpan;

/**
 * Represents a COPY statement in the copybook.  The resolver is external to
 * this module; the parser only records the name of the included copybook and
 * the source span of the COPY statement.
 *
 * @param copybookName the identifier of the copybook being included
 * @param span         the source span covering the COPY statement
 */
public record CopyNode(String copybookName, SourceSpan span) implements AstNode {
}