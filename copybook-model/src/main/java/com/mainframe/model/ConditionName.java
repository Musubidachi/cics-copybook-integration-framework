package com.mainframe.model;

/**
 * Represents a level‑88 condition name associated with a parent field.
 *
 * <p>Condition names provide mnemonic boolean flags bound to a
 * specific literal value.  The layout engine does not allocate
 * storage for level‑88 items; they overlay the parent field.</p>
 *
 * @param name  the symbolic name of the condition
 * @param value the literal string or numeric value that activates the condition
 */
public record ConditionName(String name, String value) {
}