package com.mainframe.runtime;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

/**
 * Registry of {@link TransactionSpec} YAML definitions.
 *
 * <p>The runtime layer must be able to locate a {@code TransactionSpec} by id
 * in order to construct container maps and decode responses.  The registry
 * scans the {@code classpath:/transactions/} resource folder on startup and
 * caches the YAML for each spec.  Only a minimal amount of parsing is
 * performed: the {@code metadata.id} field is extracted to serve as the
 * lookup key.</p>
 */
public class TransactionSpecRegistry {

    private static final Logger LOGGER = Logger.getLogger(TransactionSpecRegistry.class.getName());

    private final Map<String, String> specs = new HashMap<>();

    public TransactionSpecRegistry() {
        reload();
    }

    /**
     * Reloads all transaction specifications from the classpath.
     */
    public final void reload() {
        specs.clear();
        try {
            ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
            URL dirUrl = classLoader.getResource("transactions");
            if (dirUrl == null) {
                LOGGER.warning("No transactions directory found on classpath");
                return;
            }
            if ("file".equals(dirUrl.getProtocol())) {
                // Running from exploded directory (e.g. during tests)
                File dir = new File(dirUrl.toURI());
                File[] files = dir.listFiles((d, name) -> name.endsWith(".yaml") || name.endsWith(".yml"));
                if (files != null) {
                    for (File f : files) {
                        loadSpec(new FileInputStream(f));
                    }
                }
            } else {
                // Directory could be inside a JAR; fall back to loading known sample
                try (InputStream is = classLoader.getResourceAsStream("transactions/sample-transaction.yaml")) {
                    if (is != null) {
                        loadSpec(is);
                    }
                }
            }
        } catch (IOException | URISyntaxException e) {
            LOGGER.log(Level.SEVERE, "Failed to load transaction specs", e);
        }
    }

    /**
     * Returns the raw YAML for the transaction spec with the given id.
     *
     * @param id the spec id (metadata.id)
     * @return the YAML definition if present
     */
    public Optional<String> getSpecYaml(String id) {
        return Optional.ofNullable(specs.get(id));
    }

    private void loadSpec(InputStream in) throws IOException {
        if (in == null) {
            return;
        }
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {
            String yaml = reader.lines().collect(Collectors.joining("\n"));
            String id = extractId(yaml);
            if (id != null && !id.isEmpty()) {
                specs.put(id, yaml);
            }
        }
    }

    /**
     * Extracts the {@code metadata.id} field from a YAML string.  This is a
     * simplistic parser that looks for the first line beginning with {@code id:}
     * under the {@code metadata} section.  It does not support arbitrary YAML
     * and should be replaced with a real parser when a YAML library is added.
     */
    private String extractId(String yaml) {
        boolean inMetadata = false;
        for (String line : yaml.split("\n")) {
            String trimmed = line.trim();
            if (trimmed.startsWith("metadata:")) {
                inMetadata = true;
                continue;
            }
            if (inMetadata) {
                if (trimmed.startsWith("id:")) {
                    String value = trimmed.substring(3).trim();
                    // remove surrounding quotes if present
                    if (value.startsWith("\"") && value.endsWith("\"")) {
                        value = value.substring(1, value.length() - 1);
                    }
                    if (value.startsWith("'") && value.endsWith("'") && value.length() > 1) {
                        value = value.substring(1, value.length() - 1);
                    }
                    return value;
                }
                // stop parsing metadata section if indentation resets
                if (!line.startsWith("  ")) {
                    inMetadata = false;
                }
            }
        }
        return null;
    }
}