package com.mainframe.codegen;

/**
 * Thrown during code generation when an unsupported feature is encountered.
 *
 * <p>This exception is raised at generation time (not runtime) when the
 * generator encounters a COBOL construct that is not yet supported.</p>
 */
public final class UnsupportedFeatureException extends RuntimeException {

    private final String featureName;
    private final String context;

    /**
     * Constructs a new UnsupportedFeatureException.
     *
     * @param featureName the name of the unsupported feature
     * @param context     additional context about where the feature was encountered
     */
    public UnsupportedFeatureException(String featureName, String context) {
        super(String.format("Unsupported feature '%s': %s", featureName, context));
        this.featureName = featureName;
        this.context = context;
    }

    /**
     * Constructs a new UnsupportedFeatureException with a simple message.
     *
     * @param featureName the name of the unsupported feature
     */
    public UnsupportedFeatureException(String featureName) {
        super(String.format("Unsupported feature: %s", featureName));
        this.featureName = featureName;
        this.context = null;
    }

    /**
     * @return the name of the unsupported feature
     */
    public String getFeatureName() {
        return featureName;
    }

    /**
     * @return additional context, or null if not provided
     */
    public String getContext() {
        return context;
    }
}
