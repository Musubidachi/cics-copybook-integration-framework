package com.mainframe.codegen;

import com.mainframe.codegen.model.*;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Generates metadata JSON files from a CodegenRecordModel.
 *
 * <p>The generated JSON includes:</p>
 * <ul>
 *   <li>Generator version constant</li>
 *   <li>Canonical record name</li>
 *   <li>Total length</li>
 *   <li>Fields: path, offset, length, usage, digits, scale, signed</li>
 *   <li>Overlays: basePath, offset, length, memberPaths</li>
 * </ul>
 *
 * <p>All collections are sorted deterministically.</p>
 */
public final class MetadataGenerator {

    private static final String GENERATOR_VERSION = "1.0.0";
    private static final String INDENT = "  ";

    private final GenerationConfig config;

    public MetadataGenerator(GenerationConfig config) {
        this.config = Objects.requireNonNull(config);
    }

    /**
     * Generate metadata JSON file for the given model.
     *
     * @param model the codegen record model
     * @return the path to the generated file, or null if metadata generation is disabled
     * @throws IOException if writing fails
     */
    public Path generate(CodegenRecordModel model) throws IOException {
        if (!config.isEmitMetadataJson()) {
            return null;
        }

        Path metaDir = config.getMetadataPath();
        Files.createDirectories(metaDir);

        Path jsonFile = metaDir.resolve(model.getJavaClassName() + ".json");

        try (Writer writer = Files.newBufferedWriter(jsonFile)) {
            writeMetadataJson(writer, model);
        }

        return jsonFile;
    }

    private void writeMetadataJson(Writer writer, CodegenRecordModel model) throws IOException {
        writer.write("{\n");

        // Generator version
        writeProperty(writer, 1, "generatorVersion", GENERATOR_VERSION, true);

        // Record name
        writeProperty(writer, 1, "recordName", model.getRecordName(), true);

        // Java class name
        writeProperty(writer, 1, "javaClassName", model.getJavaClassName(), true);

        // Total length
        writeProperty(writer, 1, "totalLength", model.getTotalLength(), true);

        // Fields array
        writeFieldsArray(writer, model);

        // Overlays array (if any)
        if (!model.getOverlayGroups().isEmpty()) {
            writer.write(",\n");
            writeOverlaysArray(writer, model);
        }

        writer.write("\n}\n");
    }

    private void writeFieldsArray(Writer writer, CodegenRecordModel model) throws IOException {
        writer.write(",\n");
        writer.write(INDENT);
        writer.write("\"fields\": [\n");

        List<CodegenFieldInfo> fields = model.getFlatFields();
        for (int i = 0; i < fields.size(); i++) {
            CodegenFieldInfo field = fields.get(i);
            writeFieldObject(writer, field, i == fields.size() - 1);
        }

        writer.write(INDENT);
        writer.write("]");
    }

    private void writeFieldObject(Writer writer, CodegenFieldInfo field, boolean isLast) throws IOException {
        String indent2 = INDENT + INDENT;
        String indent3 = indent2 + INDENT;

        writer.write(indent2);
        writer.write("{\n");

        writeProperty(writer, 3, "path", field.getPath(), true);
        writeProperty(writer, 3, "javaName", field.getJavaName(), true);
        writeProperty(writer, 3, "offset", field.getOffset(), true);
        writeProperty(writer, 3, "length", field.getLength(), true);
        writeProperty(writer, 3, "kind", field.getKind().name(), true);
        writeProperty(writer, 3, "javaType", field.getJavaType(), true);

        if (field.isNumeric()) {
            writeProperty(writer, 3, "digits", field.getDigits(), true);
            writeProperty(writer, 3, "scale", field.getScale(), true);
            writeProperty(writer, 3, "signed", field.isSigned(), true);
        }

        if (field.isInOccurs()) {
            writeProperty(writer, 3, "occursIndex", field.getOccursIndex(), true);
            if (field.getOccursShape() != null) {
                writeProperty(writer, 3, "occursCount", field.getOccursShape().getCount(), true);
            }
        }

        if (field.isInOverlay()) {
            writeProperty(writer, 3, "inOverlay", true, true);
            writeProperty(writer, 3, "overlayBasePath", field.getOverlayBasePath(), false);
        } else {
            // Remove trailing comma from last property
            writeProperty(writer, 3, "inOverlay", false, false);
        }

        writer.write(indent2);
        writer.write("}");
        if (!isLast) {
            writer.write(",");
        }
        writer.write("\n");
    }

    private void writeOverlaysArray(Writer writer, CodegenRecordModel model) throws IOException {
        writer.write(INDENT);
        writer.write("\"overlays\": [\n");

        List<CodegenOverlayInfo> overlays = model.getOverlayGroups();
        for (int i = 0; i < overlays.size(); i++) {
            CodegenOverlayInfo overlay = overlays.get(i);
            writeOverlayObject(writer, overlay, i == overlays.size() - 1);
        }

        writer.write(INDENT);
        writer.write("]");
    }

    private void writeOverlayObject(Writer writer, CodegenOverlayInfo overlay, boolean isLast) throws IOException {
        String indent2 = INDENT + INDENT;
        String indent3 = indent2 + INDENT;

        writer.write(indent2);
        writer.write("{\n");

        writeProperty(writer, 3, "basePath", overlay.getBasePath(), true);
        writeProperty(writer, 3, "offset", overlay.getOffset(), true);
        writeProperty(writer, 3, "length", overlay.getLength(), true);
        writeProperty(writer, 3, "baseKind", overlay.getBaseKind().name(), true);

        // Member paths array
        writer.write(indent3);
        writer.write("\"memberPaths\": [");
        List<String> members = overlay.getMemberPaths();
        for (int i = 0; i < members.size(); i++) {
            if (i > 0) {
                writer.write(", ");
            }
            writer.write("\"");
            writer.write(escapeJson(members.get(i)));
            writer.write("\"");
        }
        writer.write("]\n");

        writer.write(indent2);
        writer.write("}");
        if (!isLast) {
            writer.write(",");
        }
        writer.write("\n");
    }

    private void writeProperty(Writer writer, int indentLevel, String name, String value, boolean hasMore) throws IOException {
        writeIndent(writer, indentLevel);
        writer.write("\"");
        writer.write(name);
        writer.write("\": \"");
        writer.write(escapeJson(value));
        writer.write("\"");
        if (hasMore) {
            writer.write(",");
        }
        writer.write("\n");
    }

    private void writeProperty(Writer writer, int indentLevel, String name, int value, boolean hasMore) throws IOException {
        writeIndent(writer, indentLevel);
        writer.write("\"");
        writer.write(name);
        writer.write("\": ");
        writer.write(String.valueOf(value));
        if (hasMore) {
            writer.write(",");
        }
        writer.write("\n");
    }

    private void writeProperty(Writer writer, int indentLevel, String name, boolean value, boolean hasMore) throws IOException {
        writeIndent(writer, indentLevel);
        writer.write("\"");
        writer.write(name);
        writer.write("\": ");
        writer.write(String.valueOf(value));
        if (hasMore) {
            writer.write(",");
        }
        writer.write("\n");
    }

    private void writeIndent(Writer writer, int level) throws IOException {
        for (int i = 0; i < level; i++) {
            writer.write(INDENT);
        }
    }

    private String escapeJson(String value) {
        if (value == null) {
            return "";
        }
        StringBuilder result = new StringBuilder();
        for (char c : value.toCharArray()) {
            switch (c) {
                case '"':
                    result.append("\\\"");
                    break;
                case '\\':
                    result.append("\\\\");
                    break;
                case '\n':
                    result.append("\\n");
                    break;
                case '\r':
                    result.append("\\r");
                    break;
                case '\t':
                    result.append("\\t");
                    break;
                default:
                    if (c < 32) {
                        result.append(String.format("\\u%04x", (int) c));
                    } else {
                        result.append(c);
                    }
            }
        }
        return result.toString();
    }
}
