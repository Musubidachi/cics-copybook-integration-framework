package com.mainframe.camel.processor;

import java.util.Arrays;
import java.util.List;
import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * Processor that validates the presence of required headers for the CICS
 * invocation. The framework uses simple string keys rather than strongly
 * typed constants to avoid accidental coupling to other modules. If a
 * required header is missing the processor throws an {@link IllegalArgumentException}
 * which is later translated into a {@code FailureEnvelope} by the route's
 * global exception handler.
 */
public class ValidateHeadersProcessor implements Processor {

    /** List of headers that must be present on the message. */
    private static final List<String> REQUIRED_HEADERS = Arrays.asList(
            "spec-id",    // identifier of the TransactionSpec used by the runtime
            "program",    // CICS program name
            "channel"     // channel name used for container transport
    );

    @Override
    public void process(Exchange exchange) {
        for (String header : REQUIRED_HEADERS) {
            Object value = exchange.getMessage().getHeader(header);
            if (value == null) {
                throw new IllegalArgumentException("Missing required header: " + header);
            }
        }
    }
}