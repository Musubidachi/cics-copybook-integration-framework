package com.mainframe.copybook.parser;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Discovers test fixtures under transactionspec-testcases/01-parser directory.
 * Supports automatic discovery of all fixture directories for parameterized tests.
 */
public final class FixtureDiscovery {

    private static Path fixtureRoot;

    private FixtureDiscovery() {
        // Utility class
    }

    /**
     * Get the fixture root directory, locating it through system property or directory traversal.
     *
     * @return the path to the 01-parser fixture root
     * @throws IllegalStateException if the fixture root cannot be found
     */
    public static Path getFixtureRoot() {
        if (fixtureRoot != null) {
            return fixtureRoot;
        }

        // First, check for system property
        String fixturesDir = System.getProperty("fixtures.dir");
        if (fixturesDir != null && !fixturesDir.isEmpty()) {
            Path path = Paths.get(fixturesDir);
            if (Files.isDirectory(path)) {
                fixtureRoot = path;
                return fixtureRoot;
            }
            throw new IllegalStateException(
                    "System property 'fixtures.dir' points to non-existent directory: " + fixturesDir);
        }

        // Walk up from module directory to find transactionspec-testcases/01-parser
        Path current = Paths.get("").toAbsolutePath();

        // If we're in copybook-parser, go up one level
        if (current.endsWith("copybook-parser")) {
            current = current.getParent();
        }

        // Try various relative paths
        Path[] candidates = {
            current.resolve("transactionspec-testcases/01-parser"),
            current.resolve("../transactionspec-testcases/01-parser"),
            current.getParent().resolve("transactionspec-testcases/01-parser"),
        };

        for (Path candidate : candidates) {
            if (Files.isDirectory(candidate)) {
                fixtureRoot = candidate.normalize();
                return fixtureRoot;
            }
        }

        // Walk up to find it
        Path searchPath = current;
        for (int i = 0; i < 10; i++) {
            Path testcases = searchPath.resolve("transactionspec-testcases/01-parser");
            if (Files.isDirectory(testcases)) {
                fixtureRoot = testcases.normalize();
                return fixtureRoot;
            }
            searchPath = searchPath.getParent();
            if (searchPath == null) {
                break;
            }
        }

        throw new IllegalStateException(
                "Cannot locate fixture root directory 'transactionspec-testcases/01-parser'. " +
                "Set -Dfixtures.dir=/path/to/transactionspec-testcases/01-parser to specify the location.");
    }

    /**
     * Discover all positive (success) fixture directories.
     * A positive fixture has an input file and expected.json.
     *
     * @return list of fixture directory paths
     */
    public static List<Path> discoverPositiveFixtures() {
        Path positiveDir = getFixtureRoot().resolve("positive");
        return discoverFixtures(positiveDir, false);
    }

    /**
     * Discover all negative (error) fixture directories.
     * A negative fixture has an input file and expected-error.json or *.expected.err.txt.
     *
     * @return list of fixture directory paths
     */
    public static List<Path> discoverNegativeFixtures() {
        Path negativeDir = getFixtureRoot().resolve("negative");
        return discoverFixtures(negativeDir, true);
    }

    /**
     * Discover fixture directories under the given parent.
     */
    private static List<Path> discoverFixtures(Path parent, boolean isNegative) {
        List<Path> fixtures = new ArrayList<>();
        if (!Files.isDirectory(parent)) {
            return fixtures;
        }

        try (Stream<Path> stream = Files.list(parent)) {
            stream.filter(Files::isDirectory)
                  .filter(dir -> hasInputFile(dir).isPresent())
                  .forEach(fixtures::add);
        } catch (IOException e) {
            throw new RuntimeException("Error scanning fixture directory: " + parent, e);
        }

        return fixtures;
    }

    /**
     * Find the input file in a fixture directory.
     * Supported: input.cpy, input.cob, input.txt, or any *.cpy in copybooks/ subdirectory.
     *
     * @param fixtureDir the fixture directory
     * @return the path to the input file, or empty if not found
     */
    public static Optional<Path> hasInputFile(Path fixtureDir) {
        // Check for standard input files
        String[] inputNames = {"input.cpy", "input.cob", "input.txt"};
        for (String name : inputNames) {
            Path input = fixtureDir.resolve(name);
            if (Files.exists(input)) {
                return Optional.of(input);
            }
        }

        // Check for copybooks subdirectory with .cpy files
        Path copybooks = fixtureDir.resolve("copybooks");
        if (Files.isDirectory(copybooks)) {
            try (Stream<Path> stream = Files.list(copybooks)) {
                Optional<Path> cpy = stream.filter(p -> p.toString().endsWith(".cpy"))
                                           .findFirst();
                if (cpy.isPresent()) {
                    return cpy;
                }
            } catch (IOException e) {
                // Ignore
            }
        }

        return Optional.empty();
    }

    /**
     * Find all input files in a fixture directory.
     *
     * @param fixtureDir the fixture directory
     * @return list of input file paths
     */
    public static List<Path> getInputFiles(Path fixtureDir) {
        List<Path> inputs = new ArrayList<>();

        // Check for standard input files
        String[] inputNames = {"input.cpy", "input.cob", "input.txt"};
        for (String name : inputNames) {
            Path input = fixtureDir.resolve(name);
            if (Files.exists(input)) {
                inputs.add(input);
            }
        }

        // Check for copybooks subdirectory
        Path copybooks = fixtureDir.resolve("copybooks");
        if (Files.isDirectory(copybooks)) {
            try (Stream<Path> stream = Files.list(copybooks)) {
                List<Path> discovered = stream
                        .filter(p -> {
                            String fileName = p.getFileName().toString();
                            return fileName.endsWith(".cpy") || fileName.endsWith(".cob");
                        })
                        .toList();

                // Heuristic ordering: pick the most "main" file first so that
                // fixture comparisons are deterministic even when copybooks/
                // contains multiple related files.
                //
                // Preference order:
                //  1) A file whose stem matches the fixture id (e.g. tc04 -> TC04.cpy)
                //  2) A file containing "MAIN" in the stem
                //  3) Largest file (by size)
                //  4) Name ascending
                final String fixtureId = extractFixtureId(fixtureDir.getFileName().toString());
                discovered.stream()
                        .sorted((a, b) -> compareInputs(a, b, fixtureId))
                        .forEach(inputs::add);
            } catch (IOException e) {
                // Ignore
            }
        }

        return inputs;
    }

    private static String extractFixtureId(String fixtureName) {
        // Examples: tc04-copy-replacing -> TC04, n02-copy-replacing-invalid -> N02
        String lower = fixtureName.toLowerCase();
        if (lower.startsWith("tc") && lower.length() >= 4) {
            return ("TC" + lower.substring(2, 4)).toUpperCase();
        }
        if (lower.startsWith("n") && lower.length() >= 3) {
            return ("N" + lower.substring(1, 3)).toUpperCase();
        }
        return "";
    }

    private static int compareInputs(Path a, Path b, String fixtureId) {
        String aStem = stripExtension(a.getFileName().toString()).toUpperCase();
        String bStem = stripExtension(b.getFileName().toString()).toUpperCase();

        int aScore = scoreInput(a, aStem, fixtureId);
        int bScore = scoreInput(b, bStem, fixtureId);
        if (aScore != bScore) {
            return Integer.compare(bScore, aScore); // higher score first
        }

        // Tie-breaker: larger file first
        long aSize = safeSize(a);
        long bSize = safeSize(b);
        if (aSize != bSize) {
            return Long.compare(bSize, aSize);
        }

        // Final tie-breaker: name ascending
        return a.getFileName().toString().compareToIgnoreCase(b.getFileName().toString());
    }

    private static int scoreInput(Path p, String stemUpper, String fixtureId) {
        int score = 0;
        if (!fixtureId.isEmpty() && stemUpper.equals(fixtureId)) {
            score += 100;
        }
        if (stemUpper.contains("MAIN")) {
            score += 50;
        }
        return score;
    }

    private static String stripExtension(String name) {
        int idx = name.lastIndexOf('.');
        return idx >= 0 ? name.substring(0, idx) : name;
    }

    private static long safeSize(Path p) {
        try {
            return Files.size(p);
        } catch (IOException e) {
            return 0L;
        }
    }

    /**
     * Find the expected JSON output file for a positive fixture.
     * Looks for expected.json or *.ast.expected.json files.
     *
     * @param fixtureDir the fixture directory
     * @return the expected JSON path, or empty if not found
     */
    public static Optional<Path> getExpectedJson(Path fixtureDir) {
        // Check for expected.json
        Path expected = fixtureDir.resolve("expected.json");
        if (Files.exists(expected)) {
            return Optional.of(expected);
        }

        // Check for *.ast.expected.json pattern
        try (Stream<Path> stream = Files.list(fixtureDir)) {
            Optional<Path> astExpected = stream
                    .filter(p -> p.getFileName().toString().endsWith(".ast.expected.json"))
                    .findFirst();
            if (astExpected.isPresent()) {
                return astExpected;
            }
        } catch (IOException e) {
            // Ignore
        }

        return Optional.empty();
    }

    /**
     * Find the expected error file for a negative fixture.
     * Looks for expected-error.json, *.expected.err.txt, or similar patterns.
     *
     * @param fixtureDir the fixture directory
     * @return the expected error path, or empty if not found
     */
    public static Optional<Path> getExpectedError(Path fixtureDir) {
        // Check for expected-error.json
        Path expectedJson = fixtureDir.resolve("expected-error.json");
        if (Files.exists(expectedJson)) {
            return Optional.of(expectedJson);
        }

        // Check for files matching expected*error pattern
        try (Stream<Path> stream = Files.list(fixtureDir)) {
            Optional<Path> errorFile = stream
                    .filter(p -> {
                        String name = p.getFileName().toString().toLowerCase();
                        return (name.contains("expected") && name.contains("err")) ||
                               name.equals("expected-error.json");
                    })
                    .findFirst();
            if (errorFile.isPresent()) {
                return errorFile;
            }
        } catch (IOException e) {
            // Ignore
        }

        return Optional.empty();
    }

    /**
     * Check if the fixture requires COPY expansion.
     * Returns true if expand-copy=true marker file exists.
     *
     * @param fixtureDir the fixture directory
     * @return true if COPY expansion should be enabled
     */
    public static boolean requiresCopyExpansion(Path fixtureDir) {
        return Files.exists(fixtureDir.resolve("expand-copy=true"));
    }

    /**
     * Validate that a fixture directory is properly configured.
     * Throws if both expected.json and expected-error.json exist.
     *
     * @param fixtureDir the fixture directory
     * @throws IllegalStateException if the fixture is invalid
     */
    public static void validateFixture(Path fixtureDir) {
        boolean hasExpected = getExpectedJson(fixtureDir).isPresent();
        boolean hasExpectedError = getExpectedError(fixtureDir).isPresent();

        if (hasExpected && hasExpectedError) {
            throw new IllegalStateException(
                    "Invalid fixture: " + fixtureDir.getFileName() +
                    " - cannot have both expected.json and expected-error.json");
        }
    }

    /**
     * Get a human-readable name for a fixture directory.
     *
     * @param fixtureDir the fixture directory
     * @return display name
     */
    public static String getFixtureName(Path fixtureDir) {
        return fixtureDir.getFileName().toString();
    }
}
