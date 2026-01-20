package com.mainframe.runtime;

import java.util.Collections;
import java.util.Map;

/**
 * Generic result envelope returned by the runtime layer.
 *
 * <p>Instances of this class carry the decoded payload (if any), the raw
 * container map returned by the mainframe integration layer, and a flag
 * indicating whether the call succeeded.  A message may be provided to
 * convey diagnostic or error information.</p>
 *
 * @param <T> the DTO type that was decoded from the response
 */
public class MainframeResult<T> {

    private final T payload;
    private final Map<String, byte[]> containers;
    private final boolean success;
    private final String message;

    /**
     * Create a new {@code MainframeResult}.
     *
     * @param payload the decoded DTO payload, or {@code null} on failure
     * @param containers the canonical container map returned by the mainframe
     * @param success whether the invocation was successful
     * @param message optional diagnostic or error message
     */
    public MainframeResult(T payload,
                           Map<String, byte[]> containers,
                           boolean success,
                           String message) {
        this.payload = payload;
        this.containers = containers != null
                ? Collections.unmodifiableMap(containers)
                : Collections.emptyMap();
        this.success = success;
        this.message = message;
    }

    /**
     * Returns the decoded payload.  May be {@code null} on failure.
     */
    public T getPayload() {
        return payload;
    }

    /**
     * Returns an unmodifiable view of the canonical container map.
     */
    public Map<String, byte[]> getContainers() {
        return containers;
    }

    /**
     * Returns whether the call was successful.
     */
    public boolean isSuccess() {
        return success;
    }

    /**
     * Returns the diagnostic or error message, or {@code null} if none.
     */
    public String getMessage() {
        return message;
    }

    /**
     * Creates a successful result with the given payload and containers.
     */
    public static <T> MainframeResult<T> success(T payload, Map<String, byte[]> containers) {
        return new MainframeResult<>(payload, containers, true, null);
    }

    /**
     * Creates a failure result with the given containers and message.
     */
    public static <T> MainframeResult<T> failure(Map<String, byte[]> containers, String message) {
        return new MainframeResult<>(null, containers, false, message);
    }
}