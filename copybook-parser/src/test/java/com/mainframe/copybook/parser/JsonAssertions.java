package com.mainframe.copybook.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.fail;

/**
 * Structural JSON comparison utilities for test assertions.
 * Provides meaningful diff output when JSON structures don't match.
 */
public final class JsonAssertions {

    private JsonAssertions() {
        // Utility class
    }

    /**
     * Assert that two JSON strings are structurally equal.
     * Parses both strings and compares their structure, ignoring whitespace differences.
     *
     * @param expected the expected JSON string
     * @param actual   the actual JSON string
     */
    public static void assertJsonEquals(String expected, String actual) {
        assertJsonEquals(expected, actual, "");
    }

    /**
     * Assert that two JSON strings are structurally equal with custom message prefix.
     *
     * @param expected      the expected JSON string
     * @param actual        the actual JSON string
     * @param messagePrefix prefix for error messages
     */
    public static void assertJsonEquals(String expected, String actual, String messagePrefix) {
        Object expectedObj = parseJson(expected);
        Object actualObj = parseJson(actual);

        List<String> differences = new ArrayList<>();
        compareJson(expectedObj, actualObj, "$", differences);

        if (!differences.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            if (!messagePrefix.isEmpty()) {
                sb.append(messagePrefix).append("\n");
            }
            sb.append("JSON structures differ:\n");
            for (String diff : differences) {
                sb.append("  - ").append(diff).append("\n");
            }
            sb.append("\nExpected:\n").append(prettyPrint(expected));
            sb.append("\n\nActual:\n").append(prettyPrint(actual));
            fail(sb.toString());
        }
    }

    /**
     * Compare two parsed JSON values and collect differences.
     */
    @SuppressWarnings("unchecked")
    private static void compareJson(Object expected, Object actual, String path, List<String> differences) {
        if (expected == null && actual == null) {
            return;
        }

        if (expected == null) {
            differences.add(path + ": expected null but got " + formatValue(actual));
            return;
        }

        if (actual == null) {
            differences.add(path + ": expected " + formatValue(expected) + " but got null");
            return;
        }

        // Handle "null" string as null value
        if (expected.equals("null") || (expected instanceof String && "null".equals(expected))) {
            if (actual.equals("null") || actual == null || (actual instanceof String && "null".equals(actual))) {
                return;
            }
        }

        if (!expected.getClass().equals(actual.getClass())) {
            // Special case: number comparisons
            if (expected instanceof Number && actual instanceof Number) {
                if (!Objects.equals(((Number) expected).doubleValue(), ((Number) actual).doubleValue())) {
                    differences.add(path + ": expected " + expected + " but got " + actual);
                }
                return;
            }
            differences.add(path + ": type mismatch - expected " + expected.getClass().getSimpleName() +
                    " but got " + actual.getClass().getSimpleName());
            return;
        }

        if (expected instanceof Map) {
            Map<String, Object> expectedMap = (Map<String, Object>) expected;
            Map<String, Object> actualMap = (Map<String, Object>) actual;

            // Check for missing keys
            for (String key : expectedMap.keySet()) {
                if (!actualMap.containsKey(key)) {
                    differences.add(path + ": missing key '" + key + "'");
                }
            }

            // Check for extra keys
            for (String key : actualMap.keySet()) {
                if (!expectedMap.containsKey(key)) {
                    differences.add(path + ": unexpected key '" + key + "'");
                }
            }

            // Compare common keys
            for (String key : expectedMap.keySet()) {
                if (actualMap.containsKey(key)) {
                    compareJson(expectedMap.get(key), actualMap.get(key), path + "." + key, differences);
                }
            }
        } else if (expected instanceof List) {
            List<Object> expectedList = (List<Object>) expected;
            List<Object> actualList = (List<Object>) actual;

            if (expectedList.size() != actualList.size()) {
                differences.add(path + ": array size mismatch - expected " + expectedList.size() +
                        " but got " + actualList.size());
            }

            int minSize = Math.min(expectedList.size(), actualList.size());
            for (int i = 0; i < minSize; i++) {
                compareJson(expectedList.get(i), actualList.get(i), path + "[" + i + "]", differences);
            }
        } else if (!expected.equals(actual)) {
            differences.add(path + ": expected " + formatValue(expected) + " but got " + formatValue(actual));
        }
    }

    /**
     * Format a value for display in error messages.
     */
    private static String formatValue(Object value) {
        if (value instanceof String) {
            return "\"" + value + "\"";
        }
        return String.valueOf(value);
    }

    /**
     * Simple JSON parser that returns Map, List, String, Number, Boolean, or null.
     */
    public static Object parseJson(String json) {
        return new JsonParser(json).parse();
    }

    /**
     * Pretty print a JSON string.
     */
    public static String prettyPrint(String json) {
        try {
            Object parsed = parseJson(json);
            return prettyPrint(parsed, 0);
        } catch (Exception e) {
            return json; // Return as-is if parsing fails
        }
    }

    private static String prettyPrint(Object obj, int indent) {
        StringBuilder sb = new StringBuilder();
        String pad = "  ".repeat(indent);
        String padInner = "  ".repeat(indent + 1);

        if (obj == null) {
            sb.append("null");
        } else if (obj instanceof Map) {
            @SuppressWarnings("unchecked")
            Map<String, Object> map = (Map<String, Object>) obj;
            if (map.isEmpty()) {
                sb.append("{}");
            } else {
                sb.append("{\n");
                Iterator<Map.Entry<String, Object>> it = map.entrySet().iterator();
                while (it.hasNext()) {
                    Map.Entry<String, Object> entry = it.next();
                    sb.append(padInner).append("\"").append(entry.getKey()).append("\": ");
                    sb.append(prettyPrint(entry.getValue(), indent + 1));
                    if (it.hasNext()) {
                        sb.append(",");
                    }
                    sb.append("\n");
                }
                sb.append(pad).append("}");
            }
        } else if (obj instanceof List) {
            @SuppressWarnings("unchecked")
            List<Object> list = (List<Object>) obj;
            if (list.isEmpty()) {
                sb.append("[]");
            } else {
                sb.append("[\n");
                for (int i = 0; i < list.size(); i++) {
                    sb.append(padInner);
                    sb.append(prettyPrint(list.get(i), indent + 1));
                    if (i < list.size() - 1) {
                        sb.append(",");
                    }
                    sb.append("\n");
                }
                sb.append(pad).append("]");
            }
        } else if (obj instanceof String) {
            sb.append("\"").append(escapeString((String) obj)).append("\"");
        } else {
            sb.append(obj);
        }

        return sb.toString();
    }

    private static String escapeString(String s) {
        StringBuilder sb = new StringBuilder();
        for (char c : s.toCharArray()) {
            switch (c) {
                case '"' -> sb.append("\\\"");
                case '\\' -> sb.append("\\\\");
                case '\n' -> sb.append("\\n");
                case '\r' -> sb.append("\\r");
                case '\t' -> sb.append("\\t");
                default -> sb.append(c);
            }
        }
        return sb.toString();
    }

    /**
     * Minimal JSON parser implementation.
     */
    private static class JsonParser {
        private final String json;
        private int pos;

        JsonParser(String json) {
            this.json = json;
            this.pos = 0;
        }

        Object parse() {
            skipWhitespace();
            return parseValue();
        }

        private Object parseValue() {
            skipWhitespace();
            if (pos >= json.length()) {
                return null;
            }

            char c = json.charAt(pos);

            if (c == '{') {
                return parseObject();
            } else if (c == '[') {
                return parseArray();
            } else if (c == '"') {
                return parseString();
            } else if (c == 't' || c == 'f') {
                return parseBoolean();
            } else if (c == 'n') {
                return parseNull();
            } else if (c == '-' || Character.isDigit(c)) {
                return parseNumber();
            }

            throw new RuntimeException("Unexpected character '" + c + "' at position " + pos);
        }

        private Map<String, Object> parseObject() {
            Map<String, Object> map = new HashMap<>();
            pos++; // skip '{'
            skipWhitespace();

            if (pos < json.length() && json.charAt(pos) == '}') {
                pos++;
                return map;
            }

            while (pos < json.length()) {
                skipWhitespace();

                // Parse key
                if (json.charAt(pos) != '"') {
                    throw new RuntimeException("Expected '\"' at position " + pos);
                }
                String key = parseString();

                // Parse colon
                skipWhitespace();
                if (pos >= json.length() || json.charAt(pos) != ':') {
                    throw new RuntimeException("Expected ':' at position " + pos);
                }
                pos++;

                // Parse value
                skipWhitespace();
                Object value = parseValue();
                map.put(key, value);

                // Check for comma or end
                skipWhitespace();
                if (pos >= json.length()) {
                    break;
                }
                char c = json.charAt(pos);
                if (c == '}') {
                    pos++;
                    break;
                } else if (c == ',') {
                    pos++;
                } else {
                    throw new RuntimeException("Expected ',' or '}' at position " + pos);
                }
            }

            return map;
        }

        private List<Object> parseArray() {
            List<Object> list = new ArrayList<>();
            pos++; // skip '['
            skipWhitespace();

            if (pos < json.length() && json.charAt(pos) == ']') {
                pos++;
                return list;
            }

            while (pos < json.length()) {
                skipWhitespace();
                list.add(parseValue());

                skipWhitespace();
                if (pos >= json.length()) {
                    break;
                }
                char c = json.charAt(pos);
                if (c == ']') {
                    pos++;
                    break;
                } else if (c == ',') {
                    pos++;
                } else {
                    throw new RuntimeException("Expected ',' or ']' at position " + pos);
                }
            }

            return list;
        }

        private String parseString() {
            pos++; // skip opening '"'
            StringBuilder sb = new StringBuilder();

            while (pos < json.length()) {
                char c = json.charAt(pos);
                if (c == '"') {
                    pos++;
                    return sb.toString();
                } else if (c == '\\') {
                    pos++;
                    if (pos >= json.length()) {
                        break;
                    }
                    char escaped = json.charAt(pos);
                    switch (escaped) {
                        case '"' -> sb.append('"');
                        case '\\' -> sb.append('\\');
                        case '/' -> sb.append('/');
                        case 'b' -> sb.append('\b');
                        case 'f' -> sb.append('\f');
                        case 'n' -> sb.append('\n');
                        case 'r' -> sb.append('\r');
                        case 't' -> sb.append('\t');
                        case 'u' -> {
                            if (pos + 4 < json.length()) {
                                String hex = json.substring(pos + 1, pos + 5);
                                sb.append((char) Integer.parseInt(hex, 16));
                                pos += 4;
                            }
                        }
                        default -> sb.append(escaped);
                    }
                    pos++;
                } else {
                    sb.append(c);
                    pos++;
                }
            }

            throw new RuntimeException("Unterminated string");
        }

        private Number parseNumber() {
            int start = pos;
            boolean hasDecimal = false;
            boolean hasExponent = false;

            if (json.charAt(pos) == '-') {
                pos++;
            }

            while (pos < json.length()) {
                char c = json.charAt(pos);
                if (Character.isDigit(c)) {
                    pos++;
                } else if (c == '.' && !hasDecimal) {
                    hasDecimal = true;
                    pos++;
                } else if ((c == 'e' || c == 'E') && !hasExponent) {
                    hasExponent = true;
                    pos++;
                    if (pos < json.length() && (json.charAt(pos) == '+' || json.charAt(pos) == '-')) {
                        pos++;
                    }
                } else {
                    break;
                }
            }

            String numStr = json.substring(start, pos);
            if (hasDecimal || hasExponent) {
                return Double.parseDouble(numStr);
            } else {
                long value = Long.parseLong(numStr);
                if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE) {
                    return (int) value;
                }
                return value;
            }
        }

        private Boolean parseBoolean() {
            if (json.startsWith("true", pos)) {
                pos += 4;
                return true;
            } else if (json.startsWith("false", pos)) {
                pos += 5;
                return false;
            }
            throw new RuntimeException("Expected boolean at position " + pos);
        }

        private Object parseNull() {
            if (json.startsWith("null", pos)) {
                pos += 4;
                return null;
            }
            throw new RuntimeException("Expected null at position " + pos);
        }

        private void skipWhitespace() {
            while (pos < json.length() && Character.isWhitespace(json.charAt(pos))) {
                pos++;
            }
        }
    }
}
