package com.mainframe.copybook.parser.ast;

import com.mainframe.copybook.parser.SourceSpan;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a COBOL level-88 condition name entry. Condition names are
 * attached to a parent data item and define named values or value ranges
 * that can be used for conditional testing.
 *
 * @param name   the condition name identifier
 * @param values list of value specifications (single values or THRU ranges)
 * @param span   the source span covering this entry
 */
public record ConditionNameNode(
        String name,
        List<ValueSpec> values,
        SourceSpan span
) implements AstNode {

    public ConditionNameNode {
        if (values == null) {
            values = Collections.emptyList();
        } else {
            values = Collections.unmodifiableList(new ArrayList<>(values));
        }
    }

    /**
     * Represents a single value or a range (VALUE 'X' THRU 'Z') in a condition.
     *
     * @param value     the value literal (without quotes)
     * @param thruValue the end of range if THRU is used, null otherwise
     */
    public record ValueSpec(String value, String thruValue) {
        /**
         * Create a single value spec (no range).
         */
        public static ValueSpec single(String value) {
            return new ValueSpec(value, null);
        }

        /**
         * Create a range value spec.
         */
        public static ValueSpec range(String from, String thru) {
            return new ValueSpec(from, thru);
        }
    }
}
