package com.mainframe.model;

import java.util.Collections;
import java.util.List;

/**
 * Immutable representation of a fully flattened copybook layout.
 *
 * <p>Every elementary field is represented as a {@link LayoutField},
 * ordered by absolute offset.  Overlay relationships created by
 * REDEFINES are captured separately as {@link OverlayGroup}s.  The
 * {@code totalLength} reflects the exact number of bytes consumed
 * by the record (after expansion of all OCCURS).</p>
 *
 * <p>Instances of this class are created by the layout engine and
 * treated as authoritative.  Downstream components must not modify
 * fields, offsets, or overlays.</p>
 *
 * @param totalLength total byte length of the record
 * @param fields      ordered list of flattened fields
 * @param overlays    list of overlay groups (may be empty)
 */
public record LayoutModel(
        int totalLength,
        List<LayoutField> fields,
        List<OverlayGroup> overlays
) {
    public LayoutModel {
        // Ensure defensive copies to guarantee immutability
        fields = List.copyOf(fields);
        overlays = List.copyOf(overlays);
    }
}