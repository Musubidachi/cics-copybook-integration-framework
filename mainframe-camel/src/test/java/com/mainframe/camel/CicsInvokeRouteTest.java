package com.mainframe.camel;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.builder.AdviceWith;
import org.apache.camel.builder.RouteBuilder;
import org.apache.camel.test.junit5.CamelTestSupport;
import org.junit.jupiter.api.Test;

public class CicsInvokeRouteTest extends CamelTestSupport {

    @Override
    protected RouteBuilder createRouteBuilder() {
        return new CicsInvokeRoute();
    }

    @Override
    public boolean isUseAdviceWith() {
        return true;
    }

    @Test
    void testSuccessfulInvocation() throws Exception {
        Map<String, byte[]> response = new HashMap<>();
        response.put("OUTPUT", new byte[]{0x03, 0x04});

        AdviceWith.adviceWith(context, "cicsInvokeRoute", a -> {
            a.interceptSendToEndpoint(CicsInvokeRoute.CICS_COMPONENT_ENDPOINT)
             .skipSendToOriginalEndpoint()
             .process(e -> e.getMessage().setBody(response));
        });

        context.start();

        Exchange exchange = template.request(CicsInvokeRoute.ENTRYPOINT, e -> {
            e.getMessage().setBody(Map.of("INPUT", new byte[]{0x01, 0x02}));
            e.getMessage().setHeader("spec-id", "txn-123");
            e.getMessage().setHeader("program", "SAMPLEPGM");
            e.getMessage().setHeader("channel", "CHAN");
        });

        assertEquals(response, exchange.getMessage().getBody());
    }
}
