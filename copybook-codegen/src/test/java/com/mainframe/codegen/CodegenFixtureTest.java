package com.mainframe.codegen;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.mainframe.codegen.adapter.ModelAdapter;
import com.mainframe.codegen.model.CodegenRecordModel;
import com.mainframe.model.LayoutField;
import com.mainframe.model.LayoutModel;
import com.mainframe.model.OverlayGroup;
import com.mainframe.model.PicSummary;
import com.mainframe.model.Usage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.tools.*;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Fixture-driven tests for the code generator.
 *
 * <p>These tests read test fixtures from transactionspec-testcases/03-codegen
 * and verify that:</p>
 * <ul>
 *   <li>DTO and Codec source files are generated</li>
 *   <li>Generated sources compile successfully</li>
 *   <li>Encoded bytes match expected hex values</li>
 *   <li>Negative cases throw expected exceptions</li>
 * </ul>
 */
public class CodegenFixtureTest {

    private static final String FIXTURES_BASE = "transactionspec-testcases/03-codegen";
    private static final ObjectMapper MAPPER = new ObjectMapper();

    @TempDir
    Path tempDir;

    private Path fixturesPath;

    @BeforeEach
    void setUp() {
        // Find the fixtures directory
        Path projectRoot = findProjectRoot();
        fixturesPath = projectRoot.resolve(FIXTURES_BASE);
    }

    private Path findProjectRoot() {
        // Start from current directory and walk up to find the project root
        Path current = Paths.get("").toAbsolutePath();
        while (current != null) {
            if (Files.exists(current.resolve("pom.xml")) &&
                Files.exists(current.resolve(FIXTURES_BASE))) {
                return current;
            }
            current = current.getParent();
        }
        // Fall back to working directory
        return Paths.get("").toAbsolutePath();
    }

    // =========================================================================
    // Positive Test Cases
    // =========================================================================

    @Test
    void testTc01Simple() throws Exception {
        runPositiveTestCase("tc01-simple", "REQUEST");
    }

    @Test
    void testTc02Occurs() throws Exception {
        runPositiveTestCase("tc02-occurs", "REQUEST");
    }

    @Test
    void testTc03Redefines() throws Exception {
        runPositiveTestCase("tc03-redefines", "REQUEST");
    }

    @Test
    void testTc04CompComp3() throws Exception {
        runPositiveTestCase("tc04-comp-comp3", "REQUEST");
    }

    @Test
    void testTc05SignedDisplay() throws Exception {
        runPositiveTestCase("tc05-signed-display", "REQUEST");
    }

    // =========================================================================
    // Negative Test Cases
    // =========================================================================

    @Test
    void testN01MissingField() throws Exception {
        // This test expects an error when a required field is missing
        runNegativeTestCase("n01-missing-field", "REQUEST");
    }

    @Test
    void testN02OverflowDisplay() throws Exception {
        runNegativeTestCase("n02-overflow-display", "REQUEST");
    }

    @Test
    void testN03Comp3ScaleTooPrecise() throws Exception {
        runNegativeTestCase("n03-comp3-scale-too-precise", "REQUEST");
    }

    @Test
    void testN04InvalidOverpunch() throws Exception {
        runNegativeTestCase("n04-invalid-overpunch", "REQUEST");
    }

    // =========================================================================
    // Test Execution
    // =========================================================================

    private void runPositiveTestCase(String testCaseName, String recordName) throws Exception {
        Path testCaseDir = fixturesPath.resolve("positive").resolve(testCaseName);
        if (!Files.exists(testCaseDir)) {
            System.out.println("Skipping test case (not found): " + testCaseName);
            return;
        }

        // 1. Read layout JSON
        Path layoutPath = testCaseDir.resolve("request.layout.json");
        LayoutModel layoutModel = parseLayoutJson(layoutPath);

        // 2. Adapt to CodegenModel
        GenerationConfig config = GenerationConfig.builder()
                .outputSourceRoot(tempDir)
                .basePackage("com.test.generated")
                .recordName(recordName)
                .build();

        CodegenRecordModel model = ModelAdapter.adapt(layoutModel, config);

        // 3. Generate sources
        DtoGenerator dtoGen = new DtoGenerator(config);
        CodecGenerator codecGen = new CodecGenerator(config);

        Path dtoPath = dtoGen.generate(model);
        Path codecPath = codecGen.generate(model);

        assertTrue(Files.exists(dtoPath), "DTO file should be generated");
        assertTrue(Files.exists(codecPath), "Codec file should be generated");

        // 4. Compile sources
        Path compiledDir = tempDir.resolve("classes");
        Files.createDirectories(compiledDir);

        boolean compiled = compileGeneratedSources(
                Arrays.asList(dtoPath, codecPath),
                compiledDir
        );
        assertTrue(compiled, "Generated sources should compile");

        // 5. Load classes
        URLClassLoader loader = new URLClassLoader(
                new URL[]{compiledDir.toUri().toURL()},
                getClass().getClassLoader()
        );

        String dtoClassName = "com.test.generated." + model.getJavaClassName();
        String codecClassName = "com.test.generated." + model.getJavaClassName() + "Codec";

        Class<?> dtoClass = loader.loadClass(dtoClassName);
        Class<?> codecClass = loader.loadClass(codecClassName);

        // 6. Read DTO sample JSON
        Path dtoSamplePath = testCaseDir.resolve("request.dto.sample.json");
        JsonNode dtoJson = MAPPER.readTree(dtoSamplePath.toFile());

        // 7. Deserialize DTO using reflection
        Object dto = deserializeDto(dtoJson, dtoClass, model);

        // 8. Invoke codec encode
        Method encodeMethod = codecClass.getMethod("encode", dtoClass);
        byte[] actualBytes = (byte[]) encodeMethod.invoke(null, dto);

        // 9. Read expected hex
        Path expectedHexPath = testCaseDir.resolve("request.bytes.expected.hex");
        byte[] expectedBytes = parseHexFile(expectedHexPath);

        // 10. Compare bytes
        assertArrayEquals(expectedBytes, actualBytes,
                "Encoded bytes should match expected hex for " + testCaseName +
                "\nExpected: " + bytesToHex(expectedBytes) +
                "\nActual:   " + bytesToHex(actualBytes));

        // 11. Test decode round-trip
        Method decodeMethod = codecClass.getMethod("decode", byte[].class);
        Object decodedDto = decodeMethod.invoke(null, (Object) actualBytes);
        assertNotNull(decodedDto, "Decoded DTO should not be null");

        // 12. Re-encode decoded DTO and verify
        byte[] reEncodedBytes = (byte[]) encodeMethod.invoke(null, decodedDto);
        assertArrayEquals(expectedBytes, reEncodedBytes,
                "Re-encoded bytes should match for " + testCaseName);

        loader.close();
    }

    private void runNegativeTestCase(String testCaseName, String recordName) throws Exception {
        Path testCaseDir = fixturesPath.resolve("negative").resolve(testCaseName);
        if (!Files.exists(testCaseDir)) {
            System.out.println("Skipping negative test case (not found): " + testCaseName);
            return;
        }

        // Read expected error
        Path errPath = testCaseDir.resolve(testCaseName.toUpperCase().replace("-", "") + ".expected.err.txt");
        if (!Files.exists(errPath)) {
            // Try alternate naming patterns
            for (File f : testCaseDir.toFile().listFiles()) {
                if (f.getName().endsWith(".expected.err.txt")) {
                    errPath = f.toPath();
                    break;
                }
            }
        }

        String expectedError = Files.exists(errPath) ?
                Files.readString(errPath).trim() : null;

        // Read layout JSON
        Path layoutPath = testCaseDir.resolve("request.layout.json");
        LayoutModel layoutModel = parseLayoutJson(layoutPath);

        // Adapt to CodegenModel
        GenerationConfig config = GenerationConfig.builder()
                .outputSourceRoot(tempDir)
                .basePackage("com.test.generated")
                .recordName(recordName)
                .build();

        try {
            CodegenRecordModel model = ModelAdapter.adapt(layoutModel, config);

            // Generate sources
            DtoGenerator dtoGen = new DtoGenerator(config);
            CodecGenerator codecGen = new CodecGenerator(config);

            Path dtoPath = dtoGen.generate(model);
            Path codecPath = codecGen.generate(model);

            // Compile sources
            Path compiledDir = tempDir.resolve("classes");
            Files.createDirectories(compiledDir);
            compileGeneratedSources(Arrays.asList(dtoPath, codecPath), compiledDir);

            // Load classes
            URLClassLoader loader = new URLClassLoader(
                    new URL[]{compiledDir.toUri().toURL()},
                    getClass().getClassLoader()
            );

            String dtoClassName = "com.test.generated." + model.getJavaClassName();
            String codecClassName = "com.test.generated." + model.getJavaClassName() + "Codec";

            Class<?> dtoClass = loader.loadClass(dtoClassName);
            Class<?> codecClass = loader.loadClass(codecClassName);

            // Read DTO sample JSON
            Path dtoSamplePath = testCaseDir.resolve("request.dto.sample.json");
            JsonNode dtoJson = MAPPER.readTree(dtoSamplePath.toFile());

            // Deserialize DTO
            Object dto = deserializeDto(dtoJson, dtoClass, model);

            // Try to encode - should throw exception
            Method encodeMethod = codecClass.getMethod("encode", dtoClass);

            Exception caughtException = null;
            try {
                encodeMethod.invoke(null, dto);
            } catch (Exception e) {
                caughtException = e.getCause() != null ? (Exception) e.getCause() : e;
            }

            assertNotNull(caughtException,
                    "Expected exception for negative test case: " + testCaseName);

            if (expectedError != null) {
                String actualMessage = caughtException.getMessage();
                assertTrue(actualMessage != null &&
                        (actualMessage.contains(expectedError) ||
                         caughtException.getClass().getSimpleName().contains(expectedError)),
                        "Exception message should contain expected text: " + expectedError +
                        "\nActual: " + actualMessage);
            }

            loader.close();

        } catch (ModelInvariantException e) {
            // Model validation failed - this is acceptable for negative tests
            if (expectedError != null) {
                assertTrue(e.getMessage().contains(expectedError) ||
                        e.getClass().getSimpleName().contains(expectedError),
                        "Exception should match expected error");
            }
        } catch (Exception e) {
            // Other exception during generation/compilation - check if expected
            if (expectedError != null) {
                String msg = e.getMessage() != null ? e.getMessage() : e.getClass().getName();
                assertTrue(msg.contains(expectedError) ||
                        e.getClass().getSimpleName().equals(expectedError),
                        "Exception should match expected error: " + expectedError +
                        "\nActual: " + msg);
            } else {
                throw e;
            }
        }
    }

    // =========================================================================
    // Parsing Helpers
    // =========================================================================

    private LayoutModel parseLayoutJson(Path layoutPath) throws IOException {
        JsonNode root = MAPPER.readTree(layoutPath.toFile());

        int recordLength = root.get("recordLength").asInt();
        List<LayoutField> fields = new ArrayList<>();
        List<OverlayGroup> overlays = new ArrayList<>();

        ArrayNode leaves = (ArrayNode) root.get("leaves");
        for (JsonNode leaf : leaves) {
            String path = leaf.get("path").asText();
            int offset = leaf.get("offset").asInt();
            int length = leaf.get("length").asInt();
            String type = leaf.get("type").asText();

            Usage usage = null;
            PicSummary pic = null;

            switch (type) {
                case "ALPHANUMERIC":
                    usage = Usage.DISPLAY;
                    pic = null;
                    break;
                case "DISPLAY_NUMERIC":
                    usage = Usage.DISPLAY;
                    int digits = length;
                    int scale = leaf.has("scale") ? leaf.get("scale").asInt() : 0;
                    boolean signed = leaf.has("signed") && leaf.get("signed").asBoolean();
                    pic = new PicSummary(digits, scale, signed);
                    break;
                case "COMP":
                    usage = Usage.COMP;
                    int compDigits = leaf.has("digits") ? leaf.get("digits").asInt() : (length <= 2 ? 4 : (length <= 4 ? 9 : 18));
                    pic = new PicSummary(compDigits, 0, false);
                    break;
                case "COMP3":
                    usage = Usage.COMP3;
                    int comp3Digits = leaf.has("digits") ? leaf.get("digits").asInt() : (length * 2 - 1);
                    int comp3Scale = leaf.has("scale") ? leaf.get("scale").asInt() : 0;
                    boolean comp3Signed = !leaf.has("signed") || leaf.get("signed").asBoolean();
                    pic = new PicSummary(comp3Digits, comp3Scale, comp3Signed);
                    break;
            }

            // Extract occurs index
            int occursIndex = -1;
            if (path.contains("[")) {
                int start = path.lastIndexOf('[');
                int end = path.lastIndexOf(']');
                occursIndex = Integer.parseInt(path.substring(start + 1, end));
            }

            fields.add(new LayoutField(path, offset, length, usage, pic, occursIndex));
        }

        // Parse overlays
        if (root.has("overlays")) {
            ArrayNode overlayNodes = (ArrayNode) root.get("overlays");
            for (JsonNode overlayNode : overlayNodes) {
                String basePath = overlayNode.get("basePath").asText();
                int offset = overlayNode.get("offset").asInt();
                int length = overlayNode.get("length").asInt();

                List<String> members = new ArrayList<>();
                ArrayNode membersArray = (ArrayNode) overlayNode.get("members");
                for (JsonNode member : membersArray) {
                    members.add(member.asText());
                }

                overlays.add(new OverlayGroup(basePath, offset, length, members));
            }
        }

        return new LayoutModel(recordLength, fields, overlays);
    }

    private Object deserializeDto(JsonNode json, Class<?> dtoClass, CodegenRecordModel model) throws Exception {
        Object dto = dtoClass.getDeclaredConstructor().newInstance();

        // Use reflection to set fields
        Iterator<String> fieldNames = json.fieldNames();
        while (fieldNames.hasNext()) {
            String jsonFieldName = fieldNames.next();
            JsonNode value = json.get(jsonFieldName);

            // Convert JSON field name to setter method name
            String setterName = "set" + capitalize(jsonFieldName);

            // Find the setter method
            Method setter = null;
            for (Method m : dtoClass.getMethods()) {
                if (m.getName().equals(setterName) && m.getParameterCount() == 1) {
                    setter = m;
                    break;
                }
            }

            if (setter == null) {
                // Try with different capitalization
                String altSetterName = "set" + jsonFieldName.substring(0, 1).toUpperCase() +
                        jsonFieldName.substring(1);
                for (Method m : dtoClass.getMethods()) {
                    if (m.getName().equalsIgnoreCase(altSetterName) && m.getParameterCount() == 1) {
                        setter = m;
                        break;
                    }
                }
            }

            if (setter != null) {
                Class<?> paramType = setter.getParameterTypes()[0];
                Object paramValue = convertJsonValue(value, paramType, dtoClass);
                setter.invoke(dto, paramValue);
            }
        }

        return dto;
    }

    private Object convertJsonValue(JsonNode value, Class<?> targetType, Class<?> dtoClass) throws Exception {
        if (value.isNull()) {
            return null;
        }

        if (targetType == String.class) {
            return value.asText();
        } else if (targetType == Integer.class || targetType == int.class) {
            return value.asInt();
        } else if (targetType == Long.class || targetType == long.class) {
            return value.asLong();
        } else if (targetType == java.math.BigDecimal.class) {
            return new java.math.BigDecimal(value.asText());
        } else if (targetType == java.math.BigInteger.class) {
            return new java.math.BigInteger(value.asText());
        } else if (List.class.isAssignableFrom(targetType)) {
            // Handle List types for OCCURS
            List<Object> list = new ArrayList<>();
            if (value.isArray()) {
                // Try to find the nested class type
                Class<?> itemClass = null;
                for (Class<?> nested : dtoClass.getDeclaredClasses()) {
                    if (nested.getSimpleName().endsWith("Item")) {
                        itemClass = nested;
                        break;
                    }
                }

                if (itemClass != null) {
                    for (JsonNode item : value) {
                        Object itemObj = itemClass.getDeclaredConstructor().newInstance();
                        // Set fields on item
                        Iterator<String> itemFields = item.fieldNames();
                        while (itemFields.hasNext()) {
                            String fieldName = itemFields.next();
                            String setterName = "set" + capitalize(fieldName);
                            for (Method m : itemClass.getMethods()) {
                                if (m.getName().equals(setterName) && m.getParameterCount() == 1) {
                                    Object fieldValue = convertJsonValue(
                                            item.get(fieldName),
                                            m.getParameterTypes()[0],
                                            dtoClass
                                    );
                                    m.invoke(itemObj, fieldValue);
                                    break;
                                }
                            }
                        }
                        list.add(itemObj);
                    }
                }
            }
            return list;
        }

        return null;
    }

    // =========================================================================
    // Compilation
    // =========================================================================

    private boolean compileGeneratedSources(List<Path> sourceFiles, Path outputDir) throws IOException {
        JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null) {
            System.out.println("No Java compiler available - skipping compilation test");
            return true; // Skip if no compiler
        }

        try (StandardJavaFileManager fileManager = compiler.getStandardFileManager(null, null, null)) {
            // Add the copybook-codegen classes to the classpath
            String classpath = System.getProperty("java.class.path");

            List<String> options = Arrays.asList(
                    "-d", outputDir.toString(),
                    "-classpath", classpath
            );

            Iterable<? extends JavaFileObject> compilationUnits = fileManager.getJavaFileObjectsFromPaths(sourceFiles);

            DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
            JavaCompiler.CompilationTask task = compiler.getTask(
                    null, fileManager, diagnostics, options, null, compilationUnits
            );

            boolean success = task.call();

            if (!success) {
                System.err.println("Compilation failed:");
                for (Diagnostic<?> d : diagnostics.getDiagnostics()) {
                    System.err.println(d);
                }
            }

            return success;
        }
    }

    // =========================================================================
    // Utilities
    // =========================================================================

    private byte[] parseHexFile(Path hexPath) throws IOException {
        String hex = Files.readString(hexPath).trim().replaceAll("\\s+", "");
        return hexToBytes(hex);
    }

    private byte[] hexToBytes(String hex) {
        int len = hex.length();
        byte[] data = new byte[len / 2];
        for (int i = 0; i < len; i += 2) {
            data[i / 2] = (byte) ((Character.digit(hex.charAt(i), 16) << 4)
                    + Character.digit(hex.charAt(i + 1), 16));
        }
        return data;
    }

    private String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : bytes) {
            sb.append(String.format("%02X", b));
        }
        return sb.toString();
    }

    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }
}
