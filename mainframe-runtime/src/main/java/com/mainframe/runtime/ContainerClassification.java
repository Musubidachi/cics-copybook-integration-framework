package com.mainframe.runtime;

/**
 * Classifies a container defined in a TransactionSpec.
 *
 * <p>Payload containers contain the primary request or response data.  Status
 * containers hold status or error information.  Diagnostic containers are
 * optional fields that may be returned by the mainframe for debugging or
 * operational purposes.</p>
 */
public enum ContainerClassification {
    /** Primary request/response payload container. */
    PAYLOAD,
    /** Status or error container returned by the mainframe. */
    STATUS,
    /** Diagnostic container with auxiliary information. */
    DIAGNOSTIC
}