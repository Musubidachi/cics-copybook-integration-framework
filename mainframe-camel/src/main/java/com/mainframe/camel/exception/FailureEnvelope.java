package com.mainframe.camel.exception;

import java.util.Objects;

/**
 * Simple value object used by the Camel adapter to encapsulate failures in a
 * consistent structure. The purpose of this class is to abstract away
 * underlying exceptions and provide callers with a stable format that can be
 * interpreted by the runtime or higher layers without exposing stack traces
 * or implementation details.
 */
public final class FailureEnvelope {
    /** Human‑readable category for the error, e.g. "validation", "timeout". */
    private final String type;
    /** User‑facing message describing the error. */
    private final String message;

    public FailureEnvelope(String type, String message) {
        this.type = type;
        this.message = message;
    }

    public String getType() {
        return type;
    }

    public String getMessage() {
        return message;
    }

    /**
     * Factory method to convert an arbitrary exception into a failure envelope.
     * The mapping rules are intentionally simple and can be extended later
     * according to the contract. At the moment the method distinguishes
     * between validation errors and generic errors.
     *
     * @param exception the exception to convert (non‑null)
     * @return failure envelope capturing the exception reason
     */
    public static FailureEnvelope fromException(Exception exception) {
        Objects.requireNonNull(exception, "exception must not be null");
        String type = "generic";
        if (exception instanceof IllegalArgumentException) {
            type = "validation";
        }
        return new FailureEnvelope(type, exception.getMessage());
    }

    @Override
    public String toString() {
        return "FailureEnvelope{" +
                "type='" + type + '\'' +
                ", message='" + message + '\'' +
                '}';
    }
}