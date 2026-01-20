package com.mainframe.copybook.parser.ast;

import com.mainframe.copybook.parser.SourceSpan;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a COPY statement in the copybook.  The resolver is external to
 * this module; the parser only records the name of the included copybook,
 * any REPLACING pairs, and the source span of the COPY statement.
 *
 * @param copybookName   the identifier of the copybook being included
 * @param replacingPairs list of REPLACING pairs (from/to), may be empty
 * @param span           the source span covering the COPY statement
 */
public record CopyNode(String copybookName, List<ReplacingPair> replacingPairs, SourceSpan span) implements AstNode {

    public CopyNode {
        if (replacingPairs == null) {
            replacingPairs = Collections.emptyList();
        } else {
            replacingPairs = Collections.unmodifiableList(new ArrayList<>(replacingPairs));
        }
    }

    /**
     * Convenience constructor for COPY statements without REPLACING.
     */
    public CopyNode(String copybookName, SourceSpan span) {
        this(copybookName, Collections.emptyList(), span);
    }

    /**
     * Represents a single REPLACING pair: ==from== BY ==to==.
     *
     * @param from the text to be replaced
     * @param to   the replacement text
     */
    public record ReplacingPair(String from, String to) {
        public ReplacingPair {
            if (from == null || from.isEmpty()) {
                throw new IllegalArgumentException("REPLACING 'from' value cannot be null or empty");
            }
            if (to == null) {
                throw new IllegalArgumentException("REPLACING 'to' value cannot be null");
            }
        }
    }
}