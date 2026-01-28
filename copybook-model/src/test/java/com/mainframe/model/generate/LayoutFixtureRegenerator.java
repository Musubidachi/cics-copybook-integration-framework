package com.mainframe.model.generate;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Locale;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.mainframe.copybook.parser.CopybookParser;
import com.mainframe.copybook.parser.ast.CopybookAst;
import com.mainframe.model.LayoutEngine;
import com.mainframe.model.LayoutModel;

public class LayoutFixtureRegenerator {

    private static final ObjectMapper MAPPER = new ObjectMapper()
            .enable(SerializationFeature.INDENT_OUTPUT);

    @Test
    void regenerate02LayoutPositiveAstAndExpected() throws IOException {
        Path root = resolve02LayoutRoot().resolve("positive");
        assertTrue(Files.isDirectory(root), "02-layout/positive not found: " + root.toAbsolutePath());

        LayoutEngine engine = new LayoutEngine();

        int cases = 0;

        System.out.println("02-layout positive root: " + root.toAbsolutePath());

        try (Stream<Path> dirs = Files.walk(root)) {
            for (Path caseDir : dirs.filter(Files::isDirectory).toList()) {
                if (caseDir.equals(root)) continue;

                Path copybook = findCopybookFile(caseDir);
                if (copybook == null) continue; // not a case dir

                cases++;
                System.out.println("CASE: " + caseDir.getFileName() + " copybook=" + copybook.getFileName());

                String src = Files.readString(copybook, StandardCharsets.UTF_8);
                CopybookAst ast = CopybookParser.parseString(src);

                // Guard: don’t freeze bad parses
                if (ast.diagnostics() != null && !ast.diagnostics().isEmpty()) {
                    fail("Parser diagnostics present for " + caseDir + ":\n" + ast.diagnostics());
                }

                // 1) Write AST JSON (for human inspection/debug)
                Path astOut = astOutputPath(caseDir);
                Files.writeString(
                        astOut,
                        MAPPER.writeValueAsString(ast) + System.lineSeparator(),
                        StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.TRUNCATE_EXISTING
                );
                System.out.println("  wrote: " + astOut.getFileName());

                // 2) Build + write expected LayoutModel JSON (this is what tests should compare)
                LayoutModel model = engine.build(ast);

                Path expectedOut = expectedLayoutPath(caseDir);
                Files.writeString(
                        expectedOut,
                        MAPPER.writeValueAsString(model) + System.lineSeparator(),
                        StandardCharsets.UTF_8,
                        StandardOpenOption.CREATE,
                        StandardOpenOption.TRUNCATE_EXISTING
                );
                System.out.println("  wrote: " + expectedOut.getFileName());
            }
        }

        assertTrue(cases > 0, "No cases found under: " + root.toAbsolutePath() +
                " (expected positive/tc*/copybook.cob)");
    }

    private static Path astOutputPath(Path caseDir) throws IOException {
        // overwrite existing *.ast.json if present
        try (Stream<Path> files = Files.list(caseDir)) {
            Path existing = files
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().toLowerCase(Locale.ROOT).endsWith(".ast.json"))
                    .findFirst()
                    .orElse(null);
            if (existing != null) return existing;
        }
        // default
        return caseDir.resolve("copybook.ast.json");
    }

    private static Path expectedLayoutPath(Path caseDir) throws IOException {
        // overwrite existing *layout*expected*.json if present
        try (Stream<Path> files = Files.list(caseDir)) {
            Path existing = files
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
                        return n.endsWith(".json") && n.contains("layout") && n.contains("expected");
                    })
                    .findFirst()
                    .orElse(null);
            if (existing != null) return existing;
        }
        // default
        return caseDir.resolve("layout.expected.json");
    }

    private static Path findCopybookFile(Path caseDir) throws IOException {
        try (Stream<Path> files = Files.list(caseDir)) {
            return files
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String n = p.getFileName().toString().toLowerCase(Locale.ROOT);
                        return n.endsWith(".cob") || n.endsWith(".cpy") || n.endsWith(".cbl") || n.endsWith(".txt");
                    })
                    .findFirst()
                    .orElse(null);
        }
    }

    private static Path resolve02LayoutRoot() {
        String configured = System.getProperty("fixtures.dir");
        if (configured != null && !configured.isBlank()) {
            return Paths.get(configured).normalize();
        }

        Path start = Paths.get(System.getProperty("user.dir")).toAbsolutePath().normalize();
        for (Path p = start; p != null; p = p.getParent()) {
            Path candidate = p.resolve("transactionspec-testcases").resolve("02-layout");
            if (Files.isDirectory(candidate)) return candidate;
        }

        throw new IllegalStateException(
                "Could not locate transactionspec-testcases/02-layout. " +
                        "Set -Dfixtures.dir=... pointing at the 02-layout folder."
        );
    }
}
