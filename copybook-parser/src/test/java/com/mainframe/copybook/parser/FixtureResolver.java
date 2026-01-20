package com.mainframe.copybook.parser;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

/**
 * A CopybookResolver implementation for test fixtures.
 * Searches for copybooks in fixture directories and related locations.
 */
public final class FixtureResolver implements CopybookResolver {

    private final Path fixtureDir;
    private final Path fixtureRoot;

    /**
     * Create a resolver for a specific fixture directory.
     *
     * @param fixtureDir the fixture directory containing the test case
     */
    public FixtureResolver(Path fixtureDir) {
        this.fixtureDir = fixtureDir;
        this.fixtureRoot = FixtureDiscovery.getFixtureRoot();
    }

    /**
     * Create a resolver that searches in specific directories.
     *
     * @param fixtureDir  the fixture directory
     * @param fixtureRoot the fixture root directory
     */
    public FixtureResolver(Path fixtureDir, Path fixtureRoot) {
        this.fixtureDir = fixtureDir;
        this.fixtureRoot = fixtureRoot;
    }

    @Override
    public Optional<Reader> resolve(String copybookName) {
        // Search order:
        // 1. Current fixture directory
        // 2. copybooks/ subdirectory within fixture
        // 3. Sibling copybooks/ directory under fixture root
        // 4. Fixture root itself

        Path[] searchPaths = {
            fixtureDir,
            fixtureDir.resolve("copybooks"),
            fixtureRoot.resolve("copybooks"),
            fixtureRoot
        };

        // Extensions to try
        String[] extensions = {".cpy", ""};

        for (Path searchPath : searchPaths) {
            if (!Files.isDirectory(searchPath)) {
                continue;
            }

            for (String ext : extensions) {
                Path candidate = searchPath.resolve(copybookName + ext);
                if (Files.exists(candidate) && Files.isRegularFile(candidate)) {
                    try {
                        return Optional.of(new FileReader(candidate.toFile()));
                    } catch (IOException e) {
                        // Continue searching
                    }
                }
            }
        }

        return Optional.empty();
    }

    /**
     * Create a resolver from a path to a single copybook file.
     * The resolver will search the parent directory and related locations.
     *
     * @param copybookPath path to the main copybook file
     * @return a resolver for that copybook's context
     */
    public static FixtureResolver fromCopybookPath(Path copybookPath) {
        Path parent = copybookPath.getParent();
        if (parent != null && parent.getFileName() != null &&
            parent.getFileName().toString().equals("copybooks")) {
            // The fixture directory is the parent of copybooks/
            return new FixtureResolver(parent.getParent());
        }
        return new FixtureResolver(parent != null ? parent : copybookPath);
    }
}
