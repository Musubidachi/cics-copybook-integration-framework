package com.mainframe.copybook.parser;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.Assertions;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public final class JsonAssertions {
    private static final ObjectMapper MAPPER = new ObjectMapper().setSerializationInclusion(JsonInclude.Include.NON_NULL);

    private JsonAssertions() {}

    public static void assertJsonEquals(String expectedJson, String actualJson) {
        try {
            JsonNode expected = MAPPER.readTree(expectedJson);
            JsonNode actual = MAPPER.readTree(actualJson);

            // Option A: if expected has nulls for optional fields, allow them to be omitted in actual
            JsonNode normalizedExpected = expected.deepCopy();
            stripNullsThatAreMissingInActual(normalizedExpected, actual);

            Assertions.assertEquals(normalizedExpected, actual);
        } catch (Exception e) {
            throw new AssertionError("Failed to compare JSON", e);
        }
    }

    private static void stripNullsThatAreMissingInActual(JsonNode expected, JsonNode actual) {
        if (expected == null) return;

        if (expected.isObject()) {
            ObjectNode expectedObj = (ObjectNode) expected;

            // iterate over a copy to avoid concurrent modification
            List<String> fieldNames = new ArrayList<>();
            expectedObj.fieldNames().forEachRemaining(fieldNames::add);

            for (String field : fieldNames) {
                JsonNode expChild = expectedObj.get(field);
                JsonNode actChild = (actual != null && actual.isObject()) ? actual.get(field) : null;

                // If expected explicitly says null but actual omits the key, drop it from expected
                if (expChild != null && expChild.isNull() && actChild == null) {
                    expectedObj.remove(field);
                    continue;
                }

                stripNullsThatAreMissingInActual(expChild, actChild);
            }
            return;
        }

        if (expected.isArray()) {
            if (actual == null || !actual.isArray()) {
                // can't align arrays; just recurse on expected elements with null actual
                for (JsonNode expElem : expected) {
                    stripNullsThatAreMissingInActual(expElem, null);
                }
                return;
            }

            int n = Math.min(expected.size(), actual.size());
            for (int i = 0; i < n; i++) {
                stripNullsThatAreMissingInActual(expected.get(i), actual.get(i));
            }
        }
    }
}
