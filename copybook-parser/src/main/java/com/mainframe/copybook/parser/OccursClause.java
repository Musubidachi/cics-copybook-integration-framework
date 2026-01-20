package com.mainframe.copybook.parser;

/**
 * Represents a fixed OCCURS clause.  The repeat count must be positive.
 *
 * @param count number of occurrences (times)
 */
public record OccursClause(int count) {
    public OccursClause {
        if (count <= 0) {
            throw new IllegalArgumentException("OCCURS count must be positive: " + count);
        }
    }
}