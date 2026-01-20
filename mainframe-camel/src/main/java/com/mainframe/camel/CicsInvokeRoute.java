package com.mainframe.camel;

import org.apache.camel.Exchange;
import org.apache.camel.builder.RouteBuilder;
import com.mainframe.camel.exception.FailureEnvelope;
import com.mainframe.camel.processor.CanonicalToCicsProcessor;
import com.mainframe.camel.processor.CicsToCanonicalProcessor;
import com.mainframe.camel.processor.ValidateHeadersProcessor;

/**
 * Camel route that performs the synchronous invocation of a CICS program.
 *
 * <p>The route accepts a canonical container map ({@code Map&lt;String, byte[]&gt;})
 * on the {@code direct:cicsInvoke} endpoint along with a set of required
 * headers describing the CICS call (for example the program name and channel).
 * It validates the headers, performs a transport‑only adaptation of the
 * container map into the form expected by the CICS component, delegates
 * invocation to a component endpoint and finally normalises the returned
 * container map back into the canonical form. Any exception that escapes
 * these steps is caught and mapped into a {@link FailureEnvelope} so that
 * callers always receive a structured failure instead of a raw exception.
 */
public class CicsInvokeRoute extends RouteBuilder {

    /**
     * Public constant for the entrypoint used by the runtime to invoke CICS.
     */
    public static final String ENTRYPOINT = "direct:cicsInvoke";

    /**
     * Public constant for the internal endpoint name that represents the
     * underlying CICS component invocation. Tests can intercept this endpoint
     * using Camel's AdviceWith to stub out the external dependency.
     */
    public static final String CICS_COMPONENT_ENDPOINT = "direct:cicsComponent";

    @Override
    public void configure() {
        // Global exception handling. Any exception thrown by downstream
        // processors or endpoints will be handled here and translated
        // into a FailureEnvelope. The route will continue and return the
        // envelope as the message body.
        onException(Exception.class)
            .handled(true)
            .process(exchange -> {
                Exception exception = exchange.getProperty(Exchange.EXCEPTION_CAUGHT, Exception.class);
                FailureEnvelope envelope = FailureEnvelope.fromException(exception);
                exchange.getMessage().setBody(envelope);
            });

        // Define the main CICS invocation route.
        from(ENTRYPOINT)
            .routeId("cicsInvokeRoute")
            // Validate that all required headers are present
            .process(new ValidateHeadersProcessor())
            // Convert the canonical container map into the request format
            // expected by the CICS component. As the adapter is transport‑only
            // this processor preserves container names and byte arrays.
            .process(new CanonicalToCicsProcessor())
            // Delegate the call to the CICS component. This uses a direct
            // endpoint so that tests can easily stub the invocation.
            .to(CICS_COMPONENT_ENDPOINT)
            // Convert the response back into a canonical container map.
            .process(new CicsToCanonicalProcessor());
    }
}