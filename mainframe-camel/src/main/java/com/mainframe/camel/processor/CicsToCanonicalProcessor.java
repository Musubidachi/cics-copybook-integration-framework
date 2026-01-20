package com.mainframe.camel.processor;

import java.util.Map;
import org.apache.camel.Exchange;
import org.apache.camel.Processor;

/**
 * Processor that normalises the CICS component response back into the
 * canonical container map. The runtime expects the body of the exchange to
 * remain a {@code Map<String, byte[]>}. This processor validates the
 * response type and propagates it unchanged. If the response is null or of
 * an unexpected type an {@link IllegalArgumentException} is thrown.
 */
@SuppressWarnings("unchecked")
public class CicsToCanonicalProcessor implements Processor {
    @Override
    public void process(Exchange exchange) {
        Object body = exchange.getMessage().getBody();
        if (body == null) {
            throw new IllegalArgumentException("CICS response cannot be null");
        }
        if (!(body instanceof Map)) {
            throw new IllegalArgumentException("Expected CICS response of type Map<String, byte[]>");
        }
        Map<String, byte[]> containerMap = (Map<String, byte[]>) body;
        exchange.getMessage().setBody(containerMap);
    }
}