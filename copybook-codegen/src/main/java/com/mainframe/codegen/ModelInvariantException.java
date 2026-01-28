package com.mainframe.codegen;

/**
 * Thrown when a layout model invariant is violated during adaptation.
 *
 * <p>This exception is raised by the ModelAdapter when the input
 * LayoutModel contains invalid or inconsistent data that would
 * prevent correct code generation.</p>
 */
public final class ModelInvariantException extends RuntimeException {

    private final String invariantType;
    private final String fieldPath;

    /**
     * Constructs a new ModelInvariantException with full context.
     *
     * @param invariantType the type of invariant that was violated
     * @param fieldPath     the field path where the violation occurred
     * @param message       detailed description of the violation
     */
    public ModelInvariantException(String invariantType, String fieldPath, String message) {
        super(String.format("Model invariant violation [%s] at '%s': %s",
                invariantType, fieldPath, message));
        this.invariantType = invariantType;
        this.fieldPath = fieldPath;
    }

    /**
     * Constructs a new ModelInvariantException without a specific field.
     *
     * @param invariantType the type of invariant that was violated
     * @param message       detailed description of the violation
     */
    public ModelInvariantException(String invariantType, String message) {
        super(String.format("Model invariant violation [%s]: %s", invariantType, message));
        this.invariantType = invariantType;
        this.fieldPath = null;
    }

    /**
     * @return the type of invariant that was violated
     */
    public String getInvariantType() {
        return invariantType;
    }

    /**
     * @return the field path where the violation occurred, or null
     */
    public String getFieldPath() {
        return fieldPath;
    }
}
