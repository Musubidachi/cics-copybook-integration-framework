package com.mainframe.model;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.mainframe.copybook.parser.CopybookParser;
import com.mainframe.copybook.parser.ast.CopybookAst;

/**
 * Fixture-driven tests for transactionspec-testcases/02-layout.
 *
 * What it does:
 *  - Discovers test case directories under 02-layout/{positive,negative}/{**}/tc*
 *  - Reads an input copybook file (first *.cob / *.cpy / *.txt it finds in that case dir)
 *  - For positive cases: builds a LayoutModel and JSON-compares to the expected JSON
 *  - For negative cases: asserts LayoutEngine.build throws; optionally validates error message
 *
 * How it finds fixtures:
 *  1) If -Dfixtures.dir is set, uses that as the 02-layout root.
 *     Example: -Dfixtures.dir=../transactionspec-testcases/02-layout
 *  2) Otherwise, searches upward from user.dir for a folder named "transactionspec-testcases/02-layout".
 *
 * Expected files (flexible naming):
 *  - Positive cases: any JSON file whose name contains "expected" (preferred: layout.expected.json)
 *  - Negative cases: either:
 *      a) any JSON file whose name contains "error" and "expected" (preferred: error.expected.json)
 *         with optional fields:
 *           { "messageContains": "..." }
 *      b) or just omit expected error JSON; the test will only assert it throws.
 */
public class LayoutEngineFixtureTest {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT);

    @TestFactory
    List<DynamicTest> layoutFixtures() throws IOException {
        Path root = resolve02LayoutRoot();
        assertTrue(Files.isDirectory(root), "02-layout root not found: " + root.toAbsolutePath());

        List<Path> caseDirs = discoverCaseDirs(root);
        assertFalse(caseDirs.isEmpty(), "No fixture case directories found under: " + root.toAbsolutePath());

        return caseDirs.stream()
                .map(dir -> DynamicTest.dynamicTest(testName(root, dir), () -> runCase(root, dir)))
                .collect(Collectors.toList());
    }

    // --------------------
    // Case runner
    // --------------------

    private static void runCase(Path root, Path caseDir) throws Exception {
        boolean isNegative = isUnder(caseDir, root.resolve("negative"));

        CopybookAst ast = loadAst(caseDir);

        LayoutEngine engine = new LayoutEngine();

        if (!isNegative) {
            Path expectedJson = findExpectedLayoutJson(caseDir);
            assertNotNull(expectedJson, "No expected layout JSON found in " + caseDir + " (name should include 'expected' and end with .json)");

            LayoutModel actual = engine.build(ast);

            JsonNode actualNode = MAPPER.valueToTree(actual);
            JsonNode expectedNode = MAPPER.readTree(Files.readString(expectedJson, StandardCharsets.UTF_8));

            assertJsonEquals(expectedNode, actualNode, () -> diffHint(expectedJson, actualNode));
        } else {
            Path expectedErrorJson = findExpectedErrorJson(caseDir);

            Throwable thrown = assertThrows(Throwable.class, () -> engine.build(ast),
                    "Negative fixture should throw: " + caseDir);

            if (expectedErrorJson != null) {
                JsonNode err = MAPPER.readTree(Files.readString(expectedErrorJson, StandardCharsets.UTF_8));
                JsonNode messageContains = err.get("messageContains");
                if (messageContains != null && messageContains.isTextual()) {
                    String needle = messageContains.asText();
                    String msg = String.valueOf(thrown.getMessage());
                    assertTrue(msg.contains(needle),
                            "Error message mismatch.\nExpected message to contain: " + needle + "\nActual: " + msg);
                }
            }
        }
    }

    // --------------------
    // Discovery helpers
    // --------------------

    private static Path resolve02LayoutRoot() throws IOException {
        String configured = System.getProperty("fixtures.dir");
        if (configured != null && !configured.isBlank()) {
            return Paths.get(configured).normalize();
        }

        // Search upward from user.dir for transactionspec-testcases/02-layout
        Path start = Paths.get(System.getProperty("user.dir")).toAbsolutePath().normalize();
        for (Path p = start; p != null; p = p.getParent()) {
            Path candidate = p.resolve("transactionspec-testcases").resolve("02-layout");
            if (Files.isDirectory(candidate)) return candidate;
        }

        // Also try common mono-repo layouts: ../transactionspec-testcases/02-layout
        Path alt = start.getParent() != null
                ? start.getParent().resolve("transactionspec-testcases").resolve("02-layout")
                : null;

        if (alt != null && Files.isDirectory(alt)) return alt;

        throw new IllegalStateException(
                "Could not locate transactionspec-testcases/02-layout. " +
                "Set -Dfixtures.dir=../transactionspec-testcases/02-layout (or the correct path). " +
                "Starting from: " + start
        );
    }

    private static List<Path> discoverCaseDirs(Path root) throws IOException {
        // Any directory that contains a copybook file and at least one json file is considered a case dir.
        try (Stream<Path> stream = Files.walk(root)) {
            return stream
                    .filter(Files::isDirectory)
                    .filter(dir -> {
                        try {
                            return findCopybookFile(dir) != null || findAstJson(dir) != null;
                        } catch (IOException e) {
                            return false;
                        }
                    })
                    .sorted()
                    .collect(Collectors.toList());
        }
    }

    private static String testName(Path root, Path caseDir) {
        Path rel = root.relativize(caseDir);
        return rel.toString().replace('\\', '/');
    }

    private static boolean isUnder(Path dir, Path possibleAncestor) {
        Path normDir = dir.toAbsolutePath().normalize();
        Path normAnc = possibleAncestor.toAbsolutePath().normalize();
        return normDir.startsWith(normAnc);
    }

    // --------------------
    // File selection (flexible)
    // --------------------

    private static Path findCopybookFile(Path caseDir) throws IOException {
        try (Stream<Path> files = Files.list(caseDir)) {
            return files
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
                        return n.endsWith(".cob") || n.endsWith(".cpy") || n.endsWith(".txt");
                    })
                    .findFirst()
                    .orElse(null);
        }
    }

    private static Path findExpectedLayoutJson(Path caseDir) throws IOException {
        // Prefer files that contain both "layout" and "expected", fallback to any "*expected*.json"
        List<Path> jsons = listJsonFiles(caseDir);

        Optional<Path> preferred = jsons.stream()
                .filter(p -> {
                    String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
                    return n.contains("layout") && n.contains("expected");
                })
                .findFirst();

        if (preferred.isPresent()) return preferred.get();

        return jsons.stream()
                .filter(p -> p.getFileName().toString().toLowerCase(Locale.ROOT).contains("expected"))
                .findFirst()
                .orElse(null);
    }

    private static Path findExpectedErrorJson(Path caseDir) throws IOException {
        List<Path> jsons = listJsonFiles(caseDir);
        // Prefer "error.expected.json" shapes
        Optional<Path> preferred = jsons.stream()
                .filter(p -> {
                    String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
                    return n.contains("error") && n.contains("expected");
                })
                .findFirst();
        return preferred.orElse(null);
    }

    private static List<Path> listJsonFiles(Path caseDir) throws IOException {
        try (Stream<Path> files = Files.list(caseDir)) {
            return files
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".json"))
                    .sorted()
                    .collect(Collectors.toList());
        }
    }

    // --------------------
    // JSON comparison helpers
    // --------------------

    private static void assertJsonEquals(JsonNode expected, JsonNode actual, Supplier<String> extraHint) throws Exception {
        if (jsonEquals(expected, actual)) return;

        String expectedPretty = MAPPER.writeValueAsString(expected);
        String actualPretty = MAPPER.writeValueAsString(actual);

        fail("Layout JSON mismatch.\n\nExpected:\n" + expectedPretty +
                "\n\nActual:\n" + actualPretty +
                "\n\n" + extraHint.get());
    }

    private static boolean jsonEquals(JsonNode a, JsonNode b) {
        // JsonNode#equals is structural; good enough for fixtures.
        return Objects.equals(a, b);
    }

    private static String diffHint(Path expectedPath, JsonNode actualNode) {
        // Write actual beside expected to make local debugging quick.
        // Note: doesn't modify repo; it writes under target/ if possible.
        try {
            Path targetDir = Paths.get("target", "layout-fixture-actuals").toAbsolutePath().normalize();
            Files.createDirectories(targetDir);

            String safeName = expectedPath.getParent().getFileName().toString() + "__" + expectedPath.getFileName().toString();
            Path out = targetDir.resolve(safeName.replaceAll("[^a-zA-Z0-9._-]", "_"));

            Files.writeString(out, MAPPER.writeValueAsString(actualNode), StandardCharsets.UTF_8, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            return "Wrote actual JSON to: " + out;
        } catch (Exception e) {
            return "Could not write actual JSON to target/: " + e.getMessage();
        }
    }
    
    private static CopybookAst loadAst(Path caseDir) throws IOException {
        Path copybook = findCopybookFile(caseDir);
        if (copybook != null) {
            String src = Files.readString(copybook, StandardCharsets.UTF_8);
            return CopybookParser.parseString(src);
        }

        Path astJson = findAstJson(caseDir);
        if (astJson != null) {
            // IMPORTANT: this assumes CopybookAst is Jackson-deserializable
            return MAPPER.readValue(Files.readString(astJson, StandardCharsets.UTF_8), CopybookAst.class);
        }

        throw new IllegalStateException("No input found in " + caseDir +
                ". Expected a copybook (*.cob/*.cpy/*.txt) or an AST json (*.ast.json).");
    }

    
    private static Path findAstJson(Path caseDir) throws IOException {
        try (var files = Files.list(caseDir)) {
            return files
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".ast.json"))
                    .findFirst()
                    .orElse(null);
        }
    }

}
