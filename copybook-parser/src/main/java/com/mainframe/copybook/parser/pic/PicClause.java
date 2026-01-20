package com.mainframe.copybook.parser.pic;

import java.util.List;

/**
 * A fully parsed PIC clause capturing whether the field is signed and the
 * sequence of PIC elements in order of appearance.  Sign information is
 * stored separately from the element list to mirror the grammar in
 * copybook-parser-module-doc.md.
 *
 * @param signed   true if the PIC begins with an 'S'
 * @param elements non‑empty list of elements describing the picture string
 */
public record PicClause(boolean signed, List<PicElement> elements) {
    public PicClause {
        if (elements == null || elements.isEmpty()) {
            throw new IllegalArgumentException("PIC clause must have at least one element");
        }
    }
}