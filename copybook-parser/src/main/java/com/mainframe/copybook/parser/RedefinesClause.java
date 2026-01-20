package com.mainframe.copybook.parser;

/**
 * Represents a REDEFINES clause which overlays the current item onto a
 * previously declared sibling.  The resolution of target names into actual
 * nodes occurs in a linking phase after the full parse.
 *
 * @param targetName the name of the data item being redefined
 */
public record RedefinesClause(String targetName) {
    public RedefinesClause {
        if (targetName == null || targetName.isBlank()) {
            throw new IllegalArgumentException("REDEFINES target name must be non‑blank");
        }
    }
}