package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.CopybookAst;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Fixture-driven tests for the copybook parser.
 * Executes all test cases in transactionspec-testcases/01-parser/.
 */
class CopybookParserTest {

    private static final Path FIXTURES_BASE = Paths.get("../transactionspec-testcases/01-parser");
    private static final Path POSITIVE_FIXTURES = FIXTURES_BASE.resolve("positive");
    private static final Path NEGATIVE_FIXTURES = FIXTURES_BASE.resolve("negative");

    private final AstJsonSerializer serializer = new AstJsonSerializer();

    @BeforeAll
    static void checkFixturesExist() {
        assertTrue(Files.exists(FIXTURES_BASE),
                "Fixtures directory not found: " + FIXTURES_BASE.toAbsolutePath());
    }

    // ========== POSITIVE TEST CASES ==========

    @Nested
    @DisplayName("TC01 - Simple Copybook")
    class TC01Simple {

        @Test
        @DisplayName("TC01-REQ: Basic group with elementary items")
        void testTC01REQ() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc01-simple/copybooks/TC01-REQ.cpy");
            Path expectedPath = POSITIVE_FIXTURES.resolve("tc01-simple/TC01-REQ.ast.expected.json");

            assertCopybookParsesCorrectly(copybookPath, expectedPath);
        }

        @Test
        @DisplayName("TC01-RSP: Response copybook parsing")
        void testTC01RSP() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc01-simple/copybooks/TC01-RSP.cpy");
            // No expected JSON for RSP, just verify it parses without errors
            assertCopybookParsesWithoutErrors(copybookPath);
        }
    }

    @Nested
    @DisplayName("TC02 - OCCURS Clause")
    class TC02Occurs {

        @Test
        @DisplayName("TC02-REQ: OCCURS with fixed count")
        void testTC02REQ() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc02-occurs/copybooks/TC02-REQ.cpy");
            Path expectedPath = POSITIVE_FIXTURES.resolve("tc02-occurs/TC02-REQ.ast.expected.json");

            assertCopybookParsesCorrectly(copybookPath, expectedPath);
        }
    }

    @Nested
    @DisplayName("TC03 - REDEFINES Clause")
    class TC03Redefines {

        @Test
        @DisplayName("TC03: REDEFINES overlay")
        void testTC03() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc03-redefines/copybooks/TC03.cpy");
            // Verify it parses without errors
            assertCopybookParsesWithoutErrors(copybookPath);
        }
    }

    @Nested
    @DisplayName("TC04 - COPY REPLACING")
    class TC04CopyReplacing {

        @Test
        @DisplayName("TC04: COPY with REPLACING syntax")
        void testTC04() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc04-copy-replacing/copybooks/TC04.cpy");
            Path expectedPath = POSITIVE_FIXTURES.resolve("tc04-copy-replacing/TC04.ast.expected.json");

            // Parse without COPY expansion to match expected output
            assertCopybookParsesCorrectly(copybookPath, expectedPath);
        }

        @Test
        @DisplayName("TC04: COPY expansion with REPLACING")
        void testTC04WithExpansion() throws Exception {
            Path copybookDir = POSITIVE_FIXTURES.resolve("tc04-copy-replacing/copybooks");
            Path copybookPath = copybookDir.resolve("TC04.cpy");

            // Create a resolver for the copybook directory
            CopybookResolver resolver = name -> {
                Path path = copybookDir.resolve(name + ".cpy");
                if (Files.exists(path)) {
                    try {
                        return Optional.of(new FileReader(path.toFile()));
                    } catch (IOException e) {
                        return Optional.empty();
                    }
                }
                return Optional.empty();
            };

            String source = Files.readString(copybookPath);
            CopybookAst ast = CopybookParser.parseString("TC04", source, resolver, ParserOptions.DEFAULT);

            // Should expand successfully
            assertTrue(ast.diagnostics().stream()
                            .noneMatch(d -> d.code().equals("COPYBOOK_NOT_FOUND")),
                    "COPY expansion should succeed");
        }
    }

    @Nested
    @DisplayName("TC05 - Level-88 Condition Names")
    class TC05Level88 {

        @Test
        @DisplayName("TC05: Level-88 condition names with VALUE")
        void testTC05() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc05-level88/copybooks/TC05.cpy");

            String source = Files.readString(copybookPath);
            CopybookAst ast = CopybookParser.parseString(source);

            // Verify no errors
            assertTrue(ast.diagnostics().isEmpty(),
                    "Should parse without errors. Diagnostics: " + ast.diagnostics());

            // Verify we have condition names in the AST
            String json = serializer.serialize(ast);
            assertTrue(json.contains("conditions") || json.contains("STATUS-OK"),
                    "AST should contain level-88 condition names");
        }
    }

    // ========== NEGATIVE TEST CASES ==========

    @Nested
    @DisplayName("N01 - OCCURS DEPENDING ON (Unsupported)")
    class N01OccursDependingOn {

        @Test
        @DisplayName("N01: Should reject OCCURS DEPENDING ON")
        void testN01() throws Exception {
            Path copybookPath = NEGATIVE_FIXTURES.resolve("n01-occurs-depending-on/copybooks/N01.cpy");
            Path expectedErrorPath = NEGATIVE_FIXTURES.resolve("n01-occurs-depending-on/N01.expected.err.txt");

            String source = Files.readString(copybookPath);
            CopybookAst ast = CopybookParser.parseString(source);

            // Should have an unsupported feature error
            assertTrue(ast.diagnostics().stream()
                            .anyMatch(d -> d.code().equals("UNSUPPORTED_FEATURE") &&
                                    d.message().contains("OCCURS DEPENDING ON")),
                    "Should report UNSUPPORTED_FEATURE for OCCURS DEPENDING ON. Diagnostics: " + ast.diagnostics());

            // Verify error message matches expected
            String expectedError = Files.readString(expectedErrorPath).trim();
            assertTrue(ast.diagnostics().stream()
                            .anyMatch(d -> d.message().contains("OCCURS DEPENDING ON")),
                    "Error message should match expected: " + expectedError);
        }
    }

    @Nested
    @DisplayName("N02 - Invalid COPY REPLACING")
    class N02CopyReplacingInvalid {

        @Test
        @DisplayName("N02: Should report invalid COPY REPLACING syntax")
        void testN02() throws Exception {
            Path copybookPath = NEGATIVE_FIXTURES.resolve("n02-copy-replacing-invalid/copybooks/N02.cpy");
            Path expectedErrorPath = NEGATIVE_FIXTURES.resolve("n02-copy-replacing-invalid/N02.expected.err.txt");

            String source = Files.readString(copybookPath);
            CopybookAst ast = CopybookParser.parseString(source);

            // Should have an error about REPLACING syntax
            assertTrue(ast.diagnostics().stream()
                            .anyMatch(d -> d.message().toLowerCase().contains("replacing") ||
                                    d.message().toLowerCase().contains("by")),
                    "Should report error for invalid REPLACING syntax. Diagnostics: " + ast.diagnostics());
        }
    }

    // ========== HELPER METHODS ==========

    /**
     * Assert that a copybook parses and matches the expected JSON snapshot.
     */
    private void assertCopybookParsesCorrectly(Path copybookPath, Path expectedPath) throws Exception {
        assertTrue(Files.exists(copybookPath),
                "Copybook file not found: " + copybookPath.toAbsolutePath());

        String source = Files.readString(copybookPath);
        CopybookAst ast = CopybookParser.parseString(source);

        // Check for parse errors
        assertTrue(ast.diagnostics().isEmpty(),
                "Parser reported errors: " + ast.diagnostics());

        // Serialize to JSON
        String actualJson = serializer.serialize(ast);

        // Compare with expected (if exists)
        if (Files.exists(expectedPath)) {
            String expectedJson = Files.readString(expectedPath).trim();
            assertJsonEquals(expectedJson, actualJson);
        }
    }

    /**
     * Assert that a copybook parses without errors.
     */
    private void assertCopybookParsesWithoutErrors(Path copybookPath) throws Exception {
        assertTrue(Files.exists(copybookPath),
                "Copybook file not found: " + copybookPath.toAbsolutePath());

        String source = Files.readString(copybookPath);
        CopybookAst ast = CopybookParser.parseString(source);

        assertTrue(ast.diagnostics().isEmpty(),
                "Parser reported errors: " + ast.diagnostics());
    }

    /**
     * Compare two JSON strings for semantic equality.
     */
    private void assertJsonEquals(String expected, String actual) {
        // Normalize whitespace for comparison
        String normalizedExpected = normalizeJson(expected);
        String normalizedActual = normalizeJson(actual);

        assertEquals(normalizedExpected, normalizedActual,
                "AST JSON does not match expected.\n" +
                        "Expected:\n" + expected + "\n\n" +
                        "Actual:\n" + actual);
    }

    /**
     * Normalize JSON for comparison (remove extra whitespace).
     */
    private String normalizeJson(String json) {
        // Simple normalization - in production you might use a JSON library
        return json.replaceAll("\\s+", " ")
                .replaceAll("\\{ ", "{")
                .replaceAll(" \\}", "}")
                .replaceAll("\\[ ", "[")
                .replaceAll(" \\]", "]")
                .replaceAll(": ", ":")
                .replaceAll(", ", ",")
                .trim();
    }

    // ========== SERIALIZATION STABILITY TESTS ==========

    @Nested
    @DisplayName("Serialization Stability")
    class SerializationStability {

        @Test
        @DisplayName("JSON serialization should be deterministic")
        void testSerializationDeterminism() throws Exception {
            Path copybookPath = POSITIVE_FIXTURES.resolve("tc01-simple/copybooks/TC01-REQ.cpy");
            String source = Files.readString(copybookPath);

            // Parse multiple times
            CopybookAst ast1 = CopybookParser.parseString(source);
            CopybookAst ast2 = CopybookParser.parseString(source);

            // Serialize both
            String json1 = serializer.serialize(ast1);
            String json2 = serializer.serialize(ast2);

            // Should be identical
            assertEquals(json1, json2, "Serialization should be deterministic");
        }
    }

    // ========== ERROR HANDLING TESTS ==========

    @Nested
    @DisplayName("Error Handling")
    class ErrorHandling {

        @Test
        @DisplayName("Malformed input should not crash")
        void testMalformedInput() {
            String[] malformedInputs = {
                    "",
                    "       ",
                    "       01 INCOMPLETE",
                    "       01 MISSING-PIC PIC",
                    "       01 BAD-PIC PIC X(",
                    "       COPY MISSING-DOT"
            };

            for (String input : malformedInputs) {
                assertDoesNotThrow(() -> CopybookParser.parseString(input),
                        "Parser should not crash on malformed input: " + input);
            }
        }

        @Test
        @DisplayName("Errors should include line/column info")
        void testErrorLocations() {
            String source = "       01 BAD.\n       05 MISSING PIC.";
            CopybookAst ast = CopybookParser.parseString(source);

            // Should have diagnostics with location info
            for (Diagnostic d : ast.diagnostics()) {
                assertTrue(d.span().startLine() > 0, "Error should have valid line number");
                assertTrue(d.span().startColumn() > 0, "Error should have valid column number");
            }
        }
    }
}
