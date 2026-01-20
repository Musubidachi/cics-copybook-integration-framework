package com.mainframe.runtime;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Core runtime entry point for mapping DTOs to canonical container maps and
 * decoding responses back into typed DTOs.
 *
 * <p>This class delegates to generated codec classes (produced by Agent 3)
 * using reflection.  A {@link TransactionSpecRegistry} is consulted to
 * locate the appropriate codecs and container definitions.  If codecs or
 * specifications cannot be found, empty container maps are returned and
 * decoding will yield {@code MainframeResult.failure}.</p>
 */
public class MainframeRuntime {

    private static final Logger LOGGER = Logger.getLogger(MainframeRuntime.class.getName());

    private final TransactionSpecRegistry registry;

    public MainframeRuntime(TransactionSpecRegistry registry) {
        this.registry = registry;
    }

    /**
     * Encodes a DTO into a canonical container map according to the given
     * transaction specification id.
     *
     * <p>The generated DTO codec must implement a static {@code pack}
     * method with signature {@code Map<String, byte[]> pack(T dto)}.  This
     * method locates and invokes the codec via reflection.</p>
     *
     * @param dto the DTO to encode
     * @param specId the transaction specification id
     * @return a map of container names to byte arrays; empty if encoding fails
     */
    public <T> Map<String, byte[]> encode(T dto, String specId) {
        Map<String, byte[]> result = new HashMap<>();
        Optional<String> specYamlOpt = registry.getSpecYaml(specId);
        if (specYamlOpt.isEmpty()) {
            LOGGER.warning("No TransactionSpec found for id: " + specId);
            return result;
        }
        String specYaml = specYamlOpt.get();
        // Extract codec class names from YAML; because this implementation does not parse
        // full YAML, it looks for lines containing codecClass definitions.
        String requestCodec = extractFirstMatch(specYaml, "codecClass:");
        if (requestCodec != null) {
            invokeCodecPack(requestCodec, dto, result);
        }
        return result;
    }

    /**
     * Decodes a canonical container map into a DTO using the supplied
     * transaction specification id and DTO class.
     *
     * <p>The generated codec must implement a static {@code unpack}
     * method with signature {@code T unpack(Map<String, byte[]> containers)}.
     * This method locates and invokes the codec via reflection.</p>
     *
     * @param containers the canonical container map returned by the mainframe
     * @param specId the transaction specification id used to locate codecs
     * @param dtoClass the class of the DTO to decode into
     * @return a {@link MainframeResult} carrying the decoded DTO and the containers
     */
    @SuppressWarnings("unchecked")
    public <T> MainframeResult<T> decode(Map<String, byte[]> containers, String specId, Class<T> dtoClass) {
        Optional<String> specYamlOpt = registry.getSpecYaml(specId);
        if (specYamlOpt.isEmpty()) {
            LOGGER.warning("No TransactionSpec found for id: " + specId);
            return MainframeResult.failure(containers, "Unknown transaction spec: " + specId);
        }
        String specYaml = specYamlOpt.get();
        String responseCodec = extractLastMatch(specYaml, "codecClass:");
        if (responseCodec != null) {
            T decoded = (T) invokeCodecUnpack(responseCodec, containers);
            return MainframeResult.success(decoded, containers);
        }
        return MainframeResult.failure(containers, "No codec defined for spec: " + specId);
    }

    private <T> void invokeCodecPack(String className, T dto, Map<String, byte[]> target) {
        try {
            Class<?> codecClass = Class.forName(className);
            // find static method pack(dto)
            Method packMethod = null;
            for (Method m : codecClass.getMethods()) {
                if (m.getName().equals("pack") && m.getParameterCount() == 1) {
                    packMethod = m;
                    break;
                }
            }
            if (packMethod != null) {
                Object result = packMethod.invoke(null, dto);
                if (result instanceof Map) {
                    Map<?, ?> map = (Map<?, ?>) result;
                    for (Map.Entry<?, ?> entry : map.entrySet()) {
                        if (entry.getKey() instanceof String && entry.getValue() instanceof byte[]) {
                            target.put((String) entry.getKey(), (byte[]) entry.getValue());
                        }
                    }
                }
            } else {
                LOGGER.warning("No pack method found on codec: " + className);
            }
        } catch (ClassNotFoundException | IllegalAccessException | InvocationTargetException e) {
            LOGGER.log(Level.SEVERE, "Failed to invoke codec pack method on " + className, e);
        }
    }

    private Object invokeCodecUnpack(String className, Map<String, byte[]> containers) {
        try {
            Class<?> codecClass = Class.forName(className);
            // find static unpack method (Map<String, byte[]>)
            Method unpackMethod = null;
            for (Method m : codecClass.getMethods()) {
                if (m.getName().equals("unpack") && m.getParameterCount() == 1) {
                    unpackMethod = m;
                    break;
                }
            }
            if (unpackMethod != null) {
                return unpackMethod.invoke(null, containers);
            } else {
                LOGGER.warning("No unpack method found on codec: " + className);
            }
        } catch (ClassNotFoundException | IllegalAccessException | InvocationTargetException e) {
            LOGGER.log(Level.SEVERE, "Failed to invoke codec unpack method on " + className, e);
        }
        return null;
    }

    private String extractFirstMatch(String yaml, String key) {
        for (String line : yaml.split("\n")) {
            String trimmed = line.trim();
            if (trimmed.startsWith(key)) {
                String value = trimmed.substring(key.length()).trim();
                if (value.startsWith("\"") && value.endsWith("\"")) {
                    value = value.substring(1, value.length() - 1);
                }
                if (value.startsWith("'") && value.endsWith("'")) {
                    value = value.substring(1, value.length() - 1);
                }
                return value;
            }
        }
        return null;
    }

    private String extractLastMatch(String yaml, String key) {
        String last = null;
        for (String line : yaml.split("\n")) {
            String trimmed = line.trim();
            if (trimmed.startsWith(key)) {
                String value = trimmed.substring(key.length()).trim();
                if (value.startsWith("\"") && value.endsWith("\"")) {
                    value = value.substring(1, value.length() - 1);
                }
                if (value.startsWith("'") && value.endsWith("'")) {
                    value = value.substring(1, value.length() - 1);
                }
                last = value;
            }
        }
        return last;
    }
}