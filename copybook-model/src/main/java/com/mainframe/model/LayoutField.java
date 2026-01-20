package com.mainframe.model;

/**
 * Represents a single flattened elementary field within a copybook
 * layout.  Group items are not represented directly; only the
 * terminal (leaf) nodes that occupy a contiguous byte range are
 * included as {@code LayoutField} instances.
 *
 * @param path       canonical dot‑separated path to this field (e.g. "CUSTOMER.ADDRESS.ZIPCODE")
 * @param offset     absolute byte offset from the start of the record (0‑based)
 * @param length     number of bytes consumed by this field
 * @param usage      storage type used (DISPLAY, COMP, COMP‑3)
 * @param pic        parsed summary of the PIC clause
 * @param occursIndex zero‑based index of this occurrence when expanded via OCCURS
 */
public record LayoutField(
        String path,
        int offset,
        int length,
        Usage usage,
        PicSummary pic,
        int occursIndex
) {
}