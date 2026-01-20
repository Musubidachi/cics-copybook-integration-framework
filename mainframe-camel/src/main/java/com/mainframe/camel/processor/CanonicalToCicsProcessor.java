package com.mainframe.camel.processor;

import java.util.Map;
import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * Processor that adapts the canonical container map into the request structure
 * expected by the CICS component. In this transport‑only layer the
 * conversion is intentionally trivial: the processor verifies that the body
 * is a {@code Map<String, byte[]>} and propagates it unchanged. Any type
 * mismatch or null body results in an {@link IllegalArgumentException} that
 * is ultimately mapped into a structured failure envelope.
 */
@SuppressWarnings("unchecked")
public class CanonicalToCicsProcessor implements Processor {
    @Override
    public void process(Exchange exchange) {
        Object body = exchange.getMessage().getBody();
        if (body == null) {
            throw new IllegalArgumentException("Body cannot be null");
        }
        if (!(body instanceof Map)) {
            throw new IllegalArgumentException("Expected body of type Map<String, byte[]>");
        }
        // The canonical map is already in the correct form for the CICS component.
        Map<String, byte[]> containerMap = (Map<String, byte[]>) body;
        exchange.getMessage().setBody(containerMap);
    }
}