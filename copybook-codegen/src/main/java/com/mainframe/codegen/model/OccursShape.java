package com.mainframe.codegen.model;

import java.util.Objects;

/**
 * Represents the shape of an OCCURS clause in a COBOL copybook.
 *
 * <p>This class captures the fixed count of occurrences derived from
 * contiguous [i] path segments. For example, a field path like
 * "ITEMS[0].CODE" indicates an OCCURS array with some count N.</p>
 *
 * <p>Instances are immutable and can be safely shared.</p>
 */
public final class OccursShape {

    private final int count;

    /**
     * Creates an OccursShape with the specified count.
     *
     * @param count the fixed number of occurrences (must be positive)
     * @throws IllegalArgumentException if count is not positive
     */
    public OccursShape(int count) {
        if (count <= 0) {
            throw new IllegalArgumentException("OCCURS count must be positive, got: " + count);
        }
        this.count = count;
    }

    /**
     * @return the fixed number of occurrences
     */
    public int getCount() {
        return count;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OccursShape that = (OccursShape) o;
        return count == that.count;
    }

    @Override
    public int hashCode() {
        return Objects.hash(count);
    }

    @Override
    public String toString() {
        return "OccursShape{count=" + count + '}';
    }
}
