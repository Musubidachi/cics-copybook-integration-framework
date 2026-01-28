package com.mainframe.codegen;

import com.mainframe.codegen.model.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Generates Java DTO source files from a CodegenRecordModel.
 *
 * <p>The generated DTOs follow these rules:</p>
 * <ul>
 *   <li>Nested groups generate nested DTO types (inner classes)</li>
 *   <li>OCCURS groups generate List&lt;T&gt; fields</li>
 *   <li>Strict fixed size: encode requires list size == N</li>
 *   <li>Field types follow COBOL-to-Java mapping</li>
 *   <li>Overlay members each get their own nullable DTO field</li>
 *   <li>All numeric fields use boxed types</li>
 * </ul>
 */
public final class DtoGenerator {

    private static final String INDENT = "    ";

    private final GenerationConfig config;

    public DtoGenerator(GenerationConfig config) {
        this.config = Objects.requireNonNull(config);
    }

    /**
     * Generate DTO source file for the given model.
     *
     * @param model the codegen record model
     * @return the path to the generated file
     * @throws IOException if writing fails
     */
    public Path generate(CodegenRecordModel model) throws IOException {
        Path packageDir = config.getPackagePath();
        Files.createDirectories(packageDir);

        Path sourceFile = packageDir.resolve(model.getJavaClassName() + ".java");

        try (Writer writer = Files.newBufferedWriter(sourceFile)) {
            writeDto(writer, model);
        }

        return sourceFile;
    }

    private void writeDto(Writer writer, CodegenRecordModel model) throws IOException {
        // Package declaration
        writer.write("package ");
        writer.write(config.getBasePackage());
        writer.write(";\n\n");

        // Imports
        writeImports(writer, model);

        // Class declaration
        writer.write("/**\n");
        writer.write(" * Generated DTO for COBOL record: ");
        writer.write(model.getRecordName());
        writer.write("\n");
        writer.write(" * Total length: ");
        writer.write(String.valueOf(model.getTotalLength()));
        writer.write(" bytes\n");
        writer.write(" */\n");
        writer.write("public class ");
        writer.write(model.getJavaClassName());
        writer.write(" {\n\n");

        // Track which fields we've written
        Set<String> writtenFieldNames = new TreeSet<>();

        // Track OCCURS groups
        Map<String, OccursGroupInfo> occursGroups = identifyOccursGroups(model);

        // Write OCCURS group fields first
        for (OccursGroupInfo groupInfo : occursGroups.values()) {
            if (!writtenFieldNames.contains(groupInfo.javaFieldName)) {
                writer.write(INDENT);
                writer.write("private List<");
                writer.write(groupInfo.javaClassName);
                writer.write("> ");
                writer.write(groupInfo.javaFieldName);
                writer.write(";\n");
                writtenFieldNames.add(groupInfo.javaFieldName);
            }
        }

        // Write regular fields (non-OCCURS)
        for (CodegenFieldInfo field : model.getFlatFields()) {
            // Skip fields that are part of OCCURS groups
            if (field.isInOccurs()) {
                continue;
            }

            String javaFieldName = field.getJavaName();
            if (!writtenFieldNames.contains(javaFieldName)) {
                writer.write(INDENT);
                writer.write("private ");
                writer.write(getSimpleTypeName(field.getJavaType()));
                writer.write(" ");
                writer.write(javaFieldName);
                writer.write(";\n");
                writtenFieldNames.add(javaFieldName);
            }
        }

        writer.write("\n");

        // Default constructor
        writer.write(INDENT);
        writer.write("public ");
        writer.write(model.getJavaClassName());
        writer.write("() {\n");
        writer.write(INDENT);
        writer.write("}\n\n");

        // Getters and setters
        writeGettersSetters(writer, model, occursGroups, INDENT);

        // Nested types for OCCURS groups
        writeNestedTypes(writer, model, occursGroups, INDENT);

        // Close class
        writer.write("}\n");
    }

    private void writeImports(Writer writer, CodegenRecordModel model) throws IOException {
        Set<String> imports = new TreeSet<>();

        // Add imports based on field types
        for (CodegenFieldInfo field : model.getFlatFields()) {
            String type = field.getJavaType();
            if (type.startsWith("java.math.")) {
                imports.add(type);
            }
        }

        // Add List import if there are OCCURS groups
        if (hasOccursGroups(model)) {
            imports.add("java.util.List");
            imports.add("java.util.ArrayList");
        }

        for (String imp : imports) {
            writer.write("import ");
            writer.write(imp);
            writer.write(";\n");
        }

        if (!imports.isEmpty()) {
            writer.write("\n");
        }
    }

    private boolean hasOccursGroups(CodegenRecordModel model) {
        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.isInOccurs()) {
                return true;
            }
        }
        return false;
    }

    private void writeGettersSetters(Writer writer, CodegenRecordModel model,
                                      Map<String, OccursGroupInfo> occursGroups,
                                      String indent) throws IOException {
        Set<String> writtenMethods = new TreeSet<>();

        // Write getters/setters for OCCURS groups
        for (OccursGroupInfo groupInfo : occursGroups.values()) {
            String fieldName = groupInfo.javaFieldName;
            String methodSuffix = capitalize(fieldName);

            if (!writtenMethods.contains("get" + methodSuffix)) {
                // Getter
                writer.write(indent);
                writer.write("public List<");
                writer.write(groupInfo.javaClassName);
                writer.write("> get");
                writer.write(methodSuffix);
                writer.write("() {\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("return ");
                writer.write(fieldName);
                writer.write(";\n");
                writer.write(indent);
                writer.write("}\n\n");

                // Setter
                writer.write(indent);
                writer.write("public void set");
                writer.write(methodSuffix);
                writer.write("(List<");
                writer.write(groupInfo.javaClassName);
                writer.write("> ");
                writer.write(fieldName);
                writer.write(") {\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("this.");
                writer.write(fieldName);
                writer.write(" = ");
                writer.write(fieldName);
                writer.write(";\n");
                writer.write(indent);
                writer.write("}\n\n");

                writtenMethods.add("get" + methodSuffix);
            }
        }

        // Write getters/setters for regular fields
        for (CodegenFieldInfo field : model.getFlatFields()) {
            // Skip OCCURS fields
            if (field.isInOccurs()) {
                continue;
            }

            String fieldName = field.getJavaName();
            String methodSuffix = capitalize(fieldName);
            String simpleType = getSimpleTypeName(field.getJavaType());

            if (!writtenMethods.contains("get" + methodSuffix)) {
                // Getter
                writer.write(indent);
                writer.write("public ");
                writer.write(simpleType);
                writer.write(" get");
                writer.write(methodSuffix);
                writer.write("() {\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("return ");
                writer.write(fieldName);
                writer.write(";\n");
                writer.write(indent);
                writer.write("}\n\n");

                // Setter
                writer.write(indent);
                writer.write("public void set");
                writer.write(methodSuffix);
                writer.write("(");
                writer.write(simpleType);
                writer.write(" ");
                writer.write(fieldName);
                writer.write(") {\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("this.");
                writer.write(fieldName);
                writer.write(" = ");
                writer.write(fieldName);
                writer.write(";\n");
                writer.write(indent);
                writer.write("}\n\n");

                writtenMethods.add("get" + methodSuffix);
            }
        }
    }

    private void writeNestedTypes(Writer writer, CodegenRecordModel model,
                                   Map<String, OccursGroupInfo> occursGroups,
                                   String indent) throws IOException {
        for (OccursGroupInfo groupInfo : occursGroups.values()) {
            writer.write(indent);
            writer.write("/**\n");
            writer.write(indent);
            writer.write(" * Nested type for OCCURS group: ");
            writer.write(groupInfo.basePath);
            writer.write("\n");
            writer.write(indent);
            writer.write(" * Fixed size: ");
            writer.write(String.valueOf(groupInfo.count));
            writer.write("\n");
            writer.write(indent);
            writer.write(" */\n");
            writer.write(indent);
            writer.write("public static class ");
            writer.write(groupInfo.javaClassName);
            writer.write(" {\n\n");

            // Fields for the nested type
            String nestedIndent = indent + INDENT;
            Set<String> writtenFields = new TreeSet<>();

            for (CodegenFieldInfo field : groupInfo.fields) {
                String fieldName = field.getJavaName();
                if (!writtenFields.contains(fieldName)) {
                    writer.write(nestedIndent);
                    writer.write("private ");
                    writer.write(getSimpleTypeName(field.getJavaType()));
                    writer.write(" ");
                    writer.write(fieldName);
                    writer.write(";\n");
                    writtenFields.add(fieldName);
                }
            }
            writer.write("\n");

            // Constructor
            writer.write(nestedIndent);
            writer.write("public ");
            writer.write(groupInfo.javaClassName);
            writer.write("() {\n");
            writer.write(nestedIndent);
            writer.write("}\n\n");

            // Getters and setters
            Set<String> writtenMethods = new TreeSet<>();
            for (CodegenFieldInfo field : groupInfo.fields) {
                String fieldName = field.getJavaName();
                String methodSuffix = capitalize(fieldName);
                String simpleType = getSimpleTypeName(field.getJavaType());

                if (!writtenMethods.contains("get" + methodSuffix)) {
                    // Getter
                    writer.write(nestedIndent);
                    writer.write("public ");
                    writer.write(simpleType);
                    writer.write(" get");
                    writer.write(methodSuffix);
                    writer.write("() {\n");
                    writer.write(nestedIndent);
                    writer.write(INDENT);
                    writer.write("return ");
                    writer.write(fieldName);
                    writer.write(";\n");
                    writer.write(nestedIndent);
                    writer.write("}\n\n");

                    // Setter
                    writer.write(nestedIndent);
                    writer.write("public void set");
                    writer.write(methodSuffix);
                    writer.write("(");
                    writer.write(simpleType);
                    writer.write(" ");
                    writer.write(fieldName);
                    writer.write(") {\n");
                    writer.write(nestedIndent);
                    writer.write(INDENT);
                    writer.write("this.");
                    writer.write(fieldName);
                    writer.write(" = ");
                    writer.write(fieldName);
                    writer.write(";\n");
                    writer.write(nestedIndent);
                    writer.write("}\n\n");

                    writtenMethods.add("get" + methodSuffix);
                }
            }

            writer.write(indent);
            writer.write("}\n\n");
        }
    }

    private Map<String, OccursGroupInfo> identifyOccursGroups(CodegenRecordModel model) {
        Map<String, OccursGroupInfo> groups = new LinkedHashMap<>(); // Preserve insertion order

        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.getOccursShape() != null) {
                // This field is part of an OCCURS group
                String basePath = extractOccursBasePath(field.getPath());
                if (basePath != null) {
                    OccursGroupInfo info = groups.get(basePath);
                    if (info == null) {
                        info = new OccursGroupInfo();
                        info.basePath = basePath;
                        info.count = field.getOccursShape().getCount();
                        info.javaFieldName = toJavaFieldName(basePath);
                        info.javaClassName = capitalize(info.javaFieldName) + "Item";
                        info.fields = new ArrayList<>();
                        groups.put(basePath, info);
                    }

                    // Add this field if it's from the first occurrence (index 0)
                    if (field.getOccursIndex() == 0) {
                        info.fields.add(field);
                    }
                }
            }
        }

        return groups;
    }

    private String extractOccursBasePath(String path) {
        int bracketIndex = path.indexOf('[');
        return bracketIndex > 0 ? path.substring(0, bracketIndex) : null;
    }

    private String toJavaFieldName(String basePath) {
        // Get the last segment and convert to camelCase
        String[] parts = basePath.split("\\.");
        String name = parts[parts.length - 1];
        // Convert COBOL name to Java field name
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = false;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext && result.length() > 0) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else if (result.length() == 0) {
                    result.append(Character.toLowerCase(c));
                } else {
                    result.append(Character.toLowerCase(c));
                }
            }
        }
        return result.toString();
    }

    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        return Character.toUpperCase(str.charAt(0)) + str.substring(1);
    }

    private String getSimpleTypeName(String fullType) {
        int lastDot = fullType.lastIndexOf('.');
        return lastDot >= 0 ? fullType.substring(lastDot + 1) : fullType;
    }

    /**
     * Helper class to track OCCURS group information.
     */
    private static class OccursGroupInfo {
        String basePath;
        int count;
        String javaFieldName;
        String javaClassName;
        List<CodegenFieldInfo> fields;
    }

    // =========================================================================
    // Legacy method for backwards compatibility
    // =========================================================================

    /**
     * Generate a placeholder DTO into the given output directory.
     *
     * @param outputDir directory where Java source files will be written
     * @throws IOException if any I/O error occurs
     * @deprecated Use {@link #generate(CodegenRecordModel)} instead
     */
    @Deprecated
    public void generate(String outputDir) throws IOException {
        File dir = new File(outputDir, "com/mainframe/generated");
        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Failed to create output directory: " + dir);
        }
        String className = "GeneratedDto";
        File file = new File(dir, className + ".java");
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
            writer.write("package com.mainframe.generated;\n\n");
            writer.write("public class " + className + " {\n");
            writer.write("    // TODO: define fields based on LayoutModel\n");
            writer.write("    private String placeholder;\n\n");
            writer.write("    public String getPlaceholder() {\n");
            writer.write("        return placeholder;\n");
            writer.write("    }\n\n");
            writer.write("    public void setPlaceholder(String placeholder) {\n");
            writer.write("        this.placeholder = placeholder;\n");
            writer.write("    }\n");
            writer.write("}\n");
        }
    }
}
