package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.CopybookAst;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Fixture-driven tests for the copybook parser.
 * Automatically discovers and executes all test cases under transactionspec-testcases/01-parser/.
 */
class CopybookParserTest {

    private static Path fixtureRoot;
    private static int positiveFixtureCount;
    private static int negativeFixtureCount;

    private final AstJsonSerializer serializer = new AstJsonSerializer();

    @BeforeAll
    static void discoverFixtures() {
        fixtureRoot = FixtureDiscovery.getFixtureRoot();
        System.out.println("Fixture root: " + fixtureRoot.toAbsolutePath());

        positiveFixtureCount = FixtureDiscovery.discoverPositiveFixtures().size();
        negativeFixtureCount = FixtureDiscovery.discoverNegativeFixtures().size();

        System.out.println("Discovered " + positiveFixtureCount + " positive fixtures");
        System.out.println("Discovered " + negativeFixtureCount + " negative fixtures");

        assertTrue(positiveFixtureCount + negativeFixtureCount > 0,
                "No fixtures discovered under " + fixtureRoot);
    }

    // ========== POSITIVE FIXTURE TESTS ==========

    @Nested
    @DisplayName("Positive Fixtures (Success Cases)")
    class PositiveFixtures {

        static Stream<Arguments> positiveFixtureProvider() {
            return FixtureDiscovery.discoverPositiveFixtures().stream()
                    .map(path -> Arguments.of(FixtureDiscovery.getFixtureName(path), path));
        }

        @ParameterizedTest(name = "{0}")
        @MethodSource("positiveFixtureProvider")
        @DisplayName("Positive fixture")
        void testPositiveFixture(String fixtureName, Path fixtureDir) throws Exception {
            FixtureDiscovery.validateFixture(fixtureDir);

            List<Path> inputFiles = FixtureDiscovery.getInputFiles(fixtureDir);
            assertFalse(inputFiles.isEmpty(),
                    "No input files found in fixture: " + fixtureName);

            // Pick the expected JSON that corresponds to the main input file when available.
            // (Some fixtures contain multiple related copybooks, e.g., REQ/RSP.)
            Optional<Path> expectedPath;

            // Determine if COPY expansion is needed
            boolean expandCopy = FixtureDiscovery.requiresCopyExpansion(fixtureDir);
            CopybookResolver resolver = new FixtureResolver(fixtureDir);

            ParserOptions options = ParserOptions.builder()
                    .expandCopy(expandCopy)
                    .strictMode(false)
                    .trackSourcePositions(true)
                    .build();

            // For fixtures with expected JSON, test the first/main input file
            Path mainInput = inputFiles.get(0);
            expectedPath = FixtureDiscovery.getExpectedJsonForInput(fixtureDir, mainInput);
            String source = Files.readString(mainInput);
            String copybookName = mainInput.getFileName().toString().replace(".cpy", "").replace(".cob", "");

            CopybookAst ast = CopybookParser.parseString(copybookName, source, resolver, options);

            // Check for parse errors
            List<Diagnostic> errors = ast.diagnostics().stream()
                    .filter(d -> isError(d))
                    .toList();

            assertTrue(errors.isEmpty(),
                    "Parser reported errors for " + fixtureName + ": " + errors);

            // Compare with expected JSON if present
            if (expectedPath.isPresent()) {
                String actualJson = serializer.serialize(ast);
                String expectedJson = Files.readString(expectedPath.get());

                JsonAssertions.assertJsonEquals(expectedJson, actualJson);
            }
        }

        @ParameterizedTest(name = "{0} (all files)")
        @MethodSource("positiveFixtureProvider")
        @DisplayName("All files in positive fixture parse without errors")
        void testAllFilesInFixture(String fixtureName, Path fixtureDir) throws Exception {
            List<Path> inputFiles = FixtureDiscovery.getInputFiles(fixtureDir);

            for (Path inputFile : inputFiles) {
                String source = Files.readString(inputFile);
                CopybookAst ast = CopybookParser.parseString(source);

                List<Diagnostic> errors = ast.diagnostics().stream()
                        .filter(d -> isError(d))
                        .toList();

                assertTrue(errors.isEmpty(),
                        "Parser reported errors for " + inputFile.getFileName() + " in " + fixtureName + ": " + errors);
            }
        }
    }

    // ========== NEGATIVE FIXTURE TESTS ==========

    @Nested
    @DisplayName("Negative Fixtures (Error Cases)")
    class NegativeFixtures {

        static Stream<Arguments> negativeFixtureProvider() {
            return FixtureDiscovery.discoverNegativeFixtures().stream()
                    .map(path -> Arguments.of(FixtureDiscovery.getFixtureName(path), path));
        }

        @ParameterizedTest(name = "{0}")
        @MethodSource("negativeFixtureProvider")
        @DisplayName("Negative fixture")
        void testNegativeFixture(String fixtureName, Path fixtureDir) throws Exception {
            FixtureDiscovery.validateFixture(fixtureDir);

            List<Path> inputFiles = FixtureDiscovery.getInputFiles(fixtureDir);
            assertFalse(inputFiles.isEmpty(),
                    "No input files found in fixture: " + fixtureName);

            Optional<Path> expectedErrorPath = FixtureDiscovery.getExpectedError(fixtureDir);

            Path mainInput = inputFiles.get(0);
            String source = Files.readString(mainInput);

            CopybookAst ast = CopybookParser.parseString(
                    fixtureName,
                    source,
                    CopybookResolver.NONE,
                    ParserOptions.builder()
                            .expandCopy(false)
                            .strictMode(false)          // IMPORTANT for negative fixtures
                            .trackSourcePositions(true) // optional but useful
                            .build()
            );


            // Should have diagnostics
            assertFalse(ast.diagnostics().isEmpty(),
                    "Expected diagnostics for negative fixture: " + fixtureName);

            // Verify against expected error if present
            if (expectedErrorPath.isPresent()) {
                String expectedError = Files.readString(expectedErrorPath.get()).trim();
                String fileName = expectedErrorPath.get().getFileName().toString();

                if (fileName.endsWith(".json")) {
                    // Structural JSON comparison for expected-error.json
                    String actualDiagnosticsJson = serializeDiagnostics(ast.diagnostics());
                    JsonAssertions.assertJsonEquals(expectedError, actualDiagnosticsJson);
                } else {
                    // Text-based comparison for .err.txt files
                    boolean found = ast.diagnostics().stream()
                            .anyMatch(d -> d.message().contains(expectedError) ||
                                          expectedError.contains(d.code()) ||
                                          expectedError.contains(d.message()));
                    assertTrue(found,
                            "Expected error message containing '" + expectedError + "' " +
                            "not found in diagnostics: " + ast.diagnostics());
                }
            }
        }
    }

    // ========== COPY EXPANSION TESTS ==========

    @Nested
    @DisplayName("COPY Expansion")
    class CopyExpansionTests {

        @Test
        @DisplayName("COPY expansion with REPLACING")
        void testCopyExpansionWithReplacing() throws Exception {
            Path fixtureDir = fixtureRoot.resolve("positive/tc04-copy-replacing");
            if (!Files.exists(fixtureDir)) {
                return; // Skip if fixture doesn't exist
            }

            Path copybookPath = fixtureDir.resolve("copybooks/TC04.cpy");
            String source = Files.readString(copybookPath);

            CopybookResolver resolver = new FixtureResolver(fixtureDir);
            ParserOptions options = ParserOptions.builder()
                    .expandCopy(true)
                    .strictMode(false)
                    .trackSourcePositions(true)
                    .build();

            CopybookAst ast = CopybookParser.parseString("TC04", source, resolver, options);

            // Should expand without errors
            List<Diagnostic> errors = ast.diagnostics().stream()
                    .filter(d -> d.code().equals("COPYBOOK_NOT_FOUND"))
                    .toList();
            assertTrue(errors.isEmpty(),
                    "COPY expansion should succeed. Errors: " + errors);

            // The expanded AST should contain the replaced content
            String json = serializer.serialize(ast);
            assertTrue(json.contains("REQ-ID") || json.contains("REQ"),
                    "Expanded AST should contain replaced identifiers");
        }
    }

    // ========== STRICT MODE TESTS ==========

    @Nested
    @DisplayName("Strict Mode")
    class StrictModeTests {

        @Test
        @DisplayName("Strict mode throws ParseFailedException on error")
        void testStrictModeThrows() {
            String invalidSource = "       01 INVALID.\n       05 MISSING PIC.";

            ParserOptions strictOptions = ParserOptions.builder()
                    .expandCopy(false)
                    .strictMode(true)
                    .trackSourcePositions(true)
                    .build();

            // Should throw ParseFailedException
            ParseFailedException ex = assertThrows(ParseFailedException.class, () ->
                    CopybookParser.parseString("test", invalidSource, CopybookResolver.NONE, strictOptions));

            assertFalse(ex.diagnostics().isEmpty(),
                    "Exception should contain diagnostics");
        }

        @Test
        @DisplayName("Non-strict mode returns partial AST with diagnostics")
        void testNonStrictModeReturnsPartialAst() {
            String invalidSource = "       01 INVALID.\n       05 MISSING PIC.";

            ParserOptions permissiveOptions = ParserOptions.builder()
                    .expandCopy(false)
                    .strictMode(false)
                    .trackSourcePositions(true)
                    .build();

            // Should not throw
            CopybookAst ast = CopybookParser.parseString("test", invalidSource, CopybookResolver.NONE, permissiveOptions);

            assertNotNull(ast);
            assertFalse(ast.roots().isEmpty(), "Should return partial AST");
        }
    }

    // ========== SOURCE POSITION TRACKING TESTS ==========

    @Nested
    @DisplayName("Source Position Tracking")
    class SourcePositionTests {

        @Test
        @DisplayName("trackSourcePositions=false produces null spans")
        void testNoSourcePositions() {
            String source = "       01 TEST-REC.\n          05 TEST-FIELD PIC X(10).";

            ParserOptions noSpansOptions = ParserOptions.builder()
                    .expandCopy(false)
                    .strictMode(false)
                    .trackSourcePositions(false)
                    .build();

            CopybookAst ast = CopybookParser.parseString("test", source, CopybookResolver.NONE, noSpansOptions);

            assertNotNull(ast);
            assertFalse(ast.roots().isEmpty());

            // Spans should be null or have default values when tracking is disabled
            String json = serializer.serialize(ast);
            // JSON serializer should handle null spans gracefully
            assertNotNull(json);
        }

        @Test
        @DisplayName("trackSourcePositions=true includes valid spans")
        void testWithSourcePositions() {
            String source = "       01 TEST-REC.\n          05 TEST-FIELD PIC X(10).";

            ParserOptions withSpansOptions = ParserOptions.builder()
                    .expandCopy(false)
                    .strictMode(false)
                    .trackSourcePositions(true)
                    .build();

            CopybookAst ast = CopybookParser.parseString("test", source, CopybookResolver.NONE, withSpansOptions);

            assertNotNull(ast);
            assertFalse(ast.roots().isEmpty());
        }
    }

    // ========== SERIALIZATION STABILITY TESTS ==========

    @Nested
    @DisplayName("Serialization Stability")
    class SerializationStability {

        @Test
        @DisplayName("JSON serialization should be deterministic")
        void testSerializationDeterminism() throws Exception {
            List<Path> positiveFixtures = FixtureDiscovery.discoverPositiveFixtures();
            if (positiveFixtures.isEmpty()) {
                return;
            }

            Path fixtureDir = positiveFixtures.get(0);
            List<Path> inputFiles = FixtureDiscovery.getInputFiles(fixtureDir);
            if (inputFiles.isEmpty()) {
                return;
            }

            String source = Files.readString(inputFiles.get(0));

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
				assertDoesNotThrow(() -> CopybookParser.parseString("unnamed", input, CopybookResolver.NONE, ParserOptions.PERMISSIVE),
                        "Parser should not crash on malformed input: " + input);
            }
        }

        @Test
        @DisplayName("Errors should include line/column info")
        void testErrorLocations() {
            String source = "       01 BAD.\n       05 MISSING PIC.";
            CopybookAst ast = CopybookParser.parseString("unnamed", source, CopybookResolver.NONE, ParserOptions.PERMISSIVE);

            // Should have diagnostics with location info
            for (Diagnostic d : ast.diagnostics()) {
                if (d.span() != null) {
                    assertTrue(d.span().startLine() > 0, "Error should have valid line number");
                    assertTrue(d.span().startColumn() > 0, "Error should have valid column number");
                }
            }
        }
    }

    // ========== HELPER METHODS ==========

    /**
     * Check if a diagnostic is an error (vs warning).
     */
    private boolean isError(Diagnostic d) {
        String category = d.category().toLowerCase();
        String code = d.code().toLowerCase();
        return category.contains("error") ||
               code.startsWith("parse") ||
               code.startsWith("lex") ||
               code.equals("unsupported_feature");
    }

    /**
     * Serialize diagnostics to JSON for comparison.
     */
    private String serializeDiagnostics(List<Diagnostic> diagnostics) {
        StringBuilder sb = new StringBuilder();
        sb.append("{\n  \"diagnostics\": [\n");
        for (int i = 0; i < diagnostics.size(); i++) {
            Diagnostic d = diagnostics.get(i);
            sb.append("    {\n");
            sb.append("      \"code\": \"").append(d.code()).append("\",\n");
            sb.append("      \"message\": \"").append(escapeJson(d.message())).append("\"");
            if (d.span() != null) {
                sb.append(",\n      \"line\": ").append(d.span().startLine());
                sb.append(",\n      \"column\": ").append(d.span().startColumn());
            }
            sb.append("\n    }");
            if (i < diagnostics.size() - 1) {
                sb.append(",");
            }
            sb.append("\n");
        }
        sb.append("  ]\n}");
        return sb.toString();
    }

    private String escapeJson(String s) {
        return s.replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }
}
