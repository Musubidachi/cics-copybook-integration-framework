package com.mainframe.codegen;

import com.mainframe.codegen.model.*;

import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Generates Java Codec source files from a CodegenRecordModel.
 *
 * <p>Each generated codec has:</p>
 * <ul>
 *   <li>{@code public static final int LENGTH} - record byte length</li>
 *   <li>{@code encode(dto)} - encode DTO to byte array</li>
 *   <li>{@code encodeInto(dto, buffer, baseOffset)} - encode into existing buffer</li>
 *   <li>{@code decode(buffer)} - decode byte array to DTO</li>
 *   <li>{@code decodeFrom(buffer, baseOffset)} - decode from buffer at offset</li>
 * </ul>
 *
 * <p>Encoding rules:</p>
 * <ul>
 *   <li>Initialize to EBCDIC space (0x40) before writing</li>
 *   <li>Write fields strictly by offset using runtime codecs</li>
 *   <li>OCCURS: encode requires list size == N, decode creates list size N</li>
 *   <li>Overlay: only one member can be non-null during encode</li>
 * </ul>
 */
public final class CodecGenerator {

    private static final String INDENT = "    ";
    private static final String GENERATOR_VERSION = "1.0.0";

    private final GenerationConfig config;

    public CodecGenerator(GenerationConfig config) {
        this.config = Objects.requireNonNull(config);
    }

    /**
     * Generate Codec source file for the given model.
     *
     * @param model the codegen record model
     * @return the path to the generated file
     * @throws IOException if writing fails
     */
    public Path generate(CodegenRecordModel model) throws IOException {
        Path packageDir = config.getPackagePath();
        Files.createDirectories(packageDir);

        String codecClassName = model.getJavaClassName() + "Codec";
        Path sourceFile = packageDir.resolve(codecClassName + ".java");

        try (Writer writer = Files.newBufferedWriter(sourceFile)) {
            writeCodec(writer, model, codecClassName);
        }

        return sourceFile;
    }

    private void writeCodec(Writer writer, CodegenRecordModel model, String codecClassName) throws IOException {
        String dtoClassName = model.getJavaClassName();

        // Package declaration
        writer.write("package ");
        writer.write(config.getBasePackage());
        writer.write(";\n\n");

        // Imports
        writeImports(writer, model);

        // Class declaration
        writer.write("/**\n");
        writer.write(" * Generated Codec for COBOL record: ");
        writer.write(model.getRecordName());
        writer.write("\n");
        writer.write(" * Generator version: ");
        writer.write(GENERATOR_VERSION);
        writer.write("\n");
        writer.write(" */\n");
        writer.write("public final class ");
        writer.write(codecClassName);
        writer.write(" {\n\n");

        // Constants
        writer.write(INDENT);
        writer.write("/** Record byte length */\n");
        writer.write(INDENT);
        writer.write("public static final int LENGTH = ");
        writer.write(String.valueOf(model.getTotalLength()));
        writer.write(";\n\n");

        writer.write(INDENT);
        writer.write("/** EBCDIC space character for padding */\n");
        writer.write(INDENT);
        writer.write("private static final byte EBCDIC_SPACE = (byte) 0x40;\n\n");

        writer.write(INDENT);
        writer.write("/** Record name for error messages */\n");
        writer.write(INDENT);
        writer.write("private static final String RECORD_NAME = \"");
        writer.write(model.getRecordName());
        writer.write("\";\n\n");

        // Private constructor
        writer.write(INDENT);
        writer.write("private ");
        writer.write(codecClassName);
        writer.write("() {\n");
        writer.write(INDENT);
        writer.write("}\n\n");

        // encode method
        writeEncodeMethod(writer, model, dtoClassName);

        // encodeInto method
        writeEncodeIntoMethod(writer, model, dtoClassName);

        // decode method
        writeDecodeMethod(writer, model, dtoClassName);

        // decodeFrom method
        writeDecodeFromMethod(writer, model, dtoClassName);

        // Close class
        writer.write("}\n");
    }

    private void writeImports(Writer writer, CodegenRecordModel model) throws IOException {
        Set<String> imports = new TreeSet<>();

        // Always need the encoding classes
        imports.add("com.mainframe.codegen.encoding.EbcdicCodecUtil");
        imports.add("com.mainframe.codegen.encoding.ZonedDecimalCodec");
        imports.add("com.mainframe.codegen.encoding.NumericCodec");

        // Add exception imports
        imports.add("com.mainframe.codegen.encoding.OccursSizeMismatchException");
        imports.add("com.mainframe.codegen.encoding.OverlayViolationException");

        // Check for BigDecimal/BigInteger usage
        for (CodegenFieldInfo field : model.getFlatFields()) {
            String type = field.getJavaType();
            if (type.startsWith("java.math.")) {
                imports.add(type);
            }
        }

        // Check for List usage
        if (hasOccursGroups(model)) {
            imports.add("java.util.List");
            imports.add("java.util.ArrayList");
        }

        for (String imp : imports) {
            writer.write("import ");
            writer.write(imp);
            writer.write(";\n");
        }

        writer.write("\n");
    }

    private boolean hasOccursGroups(CodegenRecordModel model) {
        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.isInOccurs()) {
                return true;
            }
        }
        return false;
    }

    private void writeEncodeMethod(Writer writer, CodegenRecordModel model, String dtoClassName) throws IOException {
        writer.write(INDENT);
        writer.write("/**\n");
        writer.write(INDENT);
        writer.write(" * Encode a DTO to a new byte array.\n");
        writer.write(INDENT);
        writer.write(" *\n");
        writer.write(INDENT);
        writer.write(" * @param dto the DTO to encode\n");
        writer.write(INDENT);
        writer.write(" * @return byte array of exactly LENGTH bytes\n");
        writer.write(INDENT);
        writer.write(" */\n");
        writer.write(INDENT);
        writer.write("public static byte[] encode(");
        writer.write(dtoClassName);
        writer.write(" dto) {\n");

        writer.write(INDENT);
        writer.write(INDENT);
        writer.write("byte[] buffer = new byte[LENGTH];\n");
        writer.write(INDENT);
        writer.write(INDENT);
        writer.write("encodeInto(dto, buffer, 0);\n");
        writer.write(INDENT);
        writer.write(INDENT);
        writer.write("return buffer;\n");

        writer.write(INDENT);
        writer.write("}\n\n");
    }

    private void writeEncodeIntoMethod(Writer writer, CodegenRecordModel model, String dtoClassName) throws IOException {
        writer.write(INDENT);
        writer.write("/**\n");
        writer.write(INDENT);
        writer.write(" * Encode a DTO into an existing buffer at the specified offset.\n");
        writer.write(INDENT);
        writer.write(" *\n");
        writer.write(INDENT);
        writer.write(" * @param dto the DTO to encode\n");
        writer.write(INDENT);
        writer.write(" * @param buffer the target buffer\n");
        writer.write(INDENT);
        writer.write(" * @param baseOffset the offset in the buffer to start writing\n");
        writer.write(INDENT);
        writer.write(" */\n");
        writer.write(INDENT);
        writer.write("public static void encodeInto(");
        writer.write(dtoClassName);
        writer.write(" dto, byte[] buffer, int baseOffset) {\n");

        String indent2 = INDENT + INDENT;

        // Initialize buffer to EBCDIC spaces
        writer.write(indent2);
        writer.write("// Initialize to EBCDIC spaces\n");
        writer.write(indent2);
        writer.write("for (int i = 0; i < LENGTH; i++) {\n");
        writer.write(indent2);
        writer.write(INDENT);
        writer.write("buffer[baseOffset + i] = EBCDIC_SPACE;\n");
        writer.write(indent2);
        writer.write("}\n\n");

        // Track OCCURS groups
        Map<String, OccursGroupInfo> occursGroups = identifyOccursGroups(model);

        // Handle OCCURS groups
        for (OccursGroupInfo groupInfo : occursGroups.values()) {
            writeOccursEncode(writer, model, groupInfo, dtoClassName, indent2);
        }

        // Handle overlay groups
        for (CodegenOverlayInfo overlay : model.getOverlayGroups()) {
            writeOverlayEncode(writer, model, overlay, indent2);
        }

        // Encode regular fields (non-OCCURS, non-overlay)
        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.isInOccurs()) {
                continue; // Handled by OCCURS group
            }
            if (field.isInOverlay()) {
                continue; // Handled by overlay group
            }

            writeFieldEncode(writer, field, "dto", indent2);
        }

        writer.write(INDENT);
        writer.write("}\n\n");
    }

    private void writeOccursEncode(Writer writer, CodegenRecordModel model,
                                    OccursGroupInfo groupInfo, String dtoClassName,
                                    String indent) throws IOException {
        String getterName = "get" + capitalize(groupInfo.javaFieldName) + "()";
        String listVar = groupInfo.javaFieldName + "List";
        String itemClassName = groupInfo.javaClassName;

        // Validate list size
        writer.write(indent);
        writer.write("// Encode OCCURS group: ");
        writer.write(groupInfo.basePath);
        writer.write("\n");
        writer.write(indent);
        writer.write("List<");
        writer.write(dtoClassName);
        writer.write(".");
        writer.write(itemClassName);
        writer.write("> ");
        writer.write(listVar);
        writer.write(" = dto.");
        writer.write(getterName);
        writer.write(";\n");

        writer.write(indent);
        writer.write("if (");
        writer.write(listVar);
        writer.write(" == null || ");
        writer.write(listVar);
        writer.write(".size() != ");
        writer.write(String.valueOf(groupInfo.count));
        writer.write(") {\n");
        writer.write(indent);
        writer.write(INDENT);
        writer.write("throw new OccursSizeMismatchException(RECORD_NAME, \"");
        writer.write(groupInfo.basePath);
        writer.write("\", ");
        writer.write(String.valueOf(groupInfo.count));
        writer.write(", ");
        writer.write(listVar);
        writer.write(" == null ? 0 : ");
        writer.write(listVar);
        writer.write(".size());\n");
        writer.write(indent);
        writer.write("}\n");

        // Calculate element size
        int elementSize = calculateOccursElementSize(groupInfo, model);

        writer.write(indent);
        writer.write("for (int i = 0; i < ");
        writer.write(String.valueOf(groupInfo.count));
        writer.write("; i++) {\n");

        String nestedIndent = indent + INDENT;
        writer.write(nestedIndent);
        writer.write(dtoClassName);
        writer.write(".");
        writer.write(itemClassName);
        writer.write(" item = ");
        writer.write(listVar);
        writer.write(".get(i);\n");

        writer.write(nestedIndent);
        writer.write("int itemOffset = baseOffset + ");
        writer.write(String.valueOf(groupInfo.baseOffset));
        writer.write(" + (i * ");
        writer.write(String.valueOf(elementSize));
        writer.write(");\n");

        // Encode each field in the OCCURS group
        for (CodegenFieldInfo field : groupInfo.fields) {
            int relativeOffset = field.getOffset() - groupInfo.baseOffset;
            writeOccursFieldEncode(writer, field, relativeOffset, nestedIndent);
        }

        writer.write(indent);
        writer.write("}\n\n");
    }

    private void writeOccursFieldEncode(Writer writer, CodegenFieldInfo field, int relativeOffset, String indent) throws IOException {
        String getterName = "get" + capitalize(field.getJavaName()) + "()";
        String fieldPath = field.getPath();

        switch (field.getKind()) {
            case ALPHANUMERIC:
                writer.write(indent);
                writer.write("if (item.");
                writer.write(getterName);
                writer.write(" != null) {\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("byte[] bytes = EbcdicCodecUtil.encode(item.");
                writer.write(getterName);
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                writer.write(indent);
                writer.write(INDENT);
                writer.write("System.arraycopy(bytes, 0, buffer, itemOffset + ");
                writer.write(String.valueOf(relativeOffset));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("}\n");
                break;

            case DISPLAY_NUMERIC:
                writer.write(indent);
                writer.write("if (item.");
                writer.write(getterName);
                writer.write(" != null) {\n");
                writer.write(indent);
                writer.write(INDENT);
                if (field.getScale() > 0) {
                    writer.write("ZonedDecimalCodec.encodeInto(item.");
                    writer.write(getterName);
                    writer.write(", buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(");\n");
                } else {
                    writer.write("ZonedDecimalCodec.encodeInto(item.");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(");\n");
                }
                writer.write(indent);
                writer.write("}\n");
                break;

            case COMP:
                writer.write(indent);
                writer.write("if (item.");
                writer.write(getterName);
                writer.write(" != null) {\n");
                writer.write(indent);
                writer.write(INDENT);
                if (field.getLength() <= 4) {
                    writer.write("NumericCodec.encodeCompInto(item.");
                    writer.write(getterName);
                    writer.write(".intValue(), buffer, itemOffset + ");
                } else {
                    writer.write("NumericCodec.encodeCompLongInto(item.");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, itemOffset + ");
                }
                writer.write(String.valueOf(relativeOffset));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(", RECORD_NAME, \"");
                writer.write(fieldPath);
                writer.write("\", ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("}\n");
                break;

            case COMP3:
                writer.write(indent);
                writer.write("if (item.");
                writer.write(getterName);
                writer.write(" != null) {\n");
                writer.write(indent);
                writer.write(INDENT);
                if (field.getScale() == 0 && field.getDigits() <= 18) {
                    writer.write("NumericCodec.encodeComp3LongInto(item.");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, itemOffset + ");
                } else {
                    writer.write("NumericCodec.encodeComp3Into(item.");
                    writer.write(getterName);
                    writer.write(", buffer, itemOffset + ");
                }
                writer.write(String.valueOf(relativeOffset));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(", RECORD_NAME, \"");
                writer.write(fieldPath);
                writer.write("\", ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("}\n");
                break;
        }
    }

    private void writeOverlayEncode(Writer writer, CodegenRecordModel model,
                                     CodegenOverlayInfo overlay, String indent) throws IOException {
        writer.write(indent);
        writer.write("// Overlay group at offset ");
        writer.write(String.valueOf(overlay.getOffset()));
        writer.write(", length ");
        writer.write(String.valueOf(overlay.getLength()));
        writer.write("\n");

        // Check which member is set
        writer.write(indent);
        writer.write("{\n");

        String innerIndent = indent + INDENT;
        writer.write(innerIndent);
        writer.write("int overlayMembersSet = 0;\n");
        writer.write(innerIndent);
        writer.write("String selectedMember = null;\n\n");

        // Check each member
        for (String memberPath : overlay.getMemberPaths()) {
            Optional<CodegenFieldInfo> fieldOpt = model.findField(memberPath);
            if (fieldOpt.isPresent()) {
                CodegenFieldInfo field = fieldOpt.get();
                String getterName = "get" + capitalize(field.getJavaName()) + "()";

                writer.write(innerIndent);
                writer.write("if (dto.");
                writer.write(getterName);
                writer.write(" != null) {\n");
                writer.write(innerIndent);
                writer.write(INDENT);
                writer.write("overlayMembersSet++;\n");
                writer.write(innerIndent);
                writer.write(INDENT);
                writer.write("selectedMember = \"");
                writer.write(memberPath);
                writer.write("\";\n");
                writer.write(innerIndent);
                writer.write("}\n");
            }
        }

        // Check for violation
        writer.write("\n");
        writer.write(innerIndent);
        writer.write("if (overlayMembersSet > 1) {\n");
        writer.write(innerIndent);
        writer.write(INDENT);
        writer.write("throw new OverlayViolationException(RECORD_NAME, \"");
        writer.write(overlay.getBasePath());
        writer.write("\", ");
        writer.write(String.valueOf(overlay.getOffset()));
        writer.write(", ");
        writer.write(String.valueOf(overlay.getLength()));
        writer.write(", \"Multiple overlay members are set\");\n");
        writer.write(innerIndent);
        writer.write("}\n\n");

        // Initialize overlay region based on base type
        writer.write(innerIndent);
        writer.write("// Initialize overlay region\n");
        if (overlay.getBaseKind() == FieldKind.ALPHANUMERIC) {
            writer.write(innerIndent);
            writer.write("// Alpha base: already initialized to EBCDIC spaces\n");
        } else {
            writer.write(innerIndent);
            writer.write("// Numeric base: initialize to zeros\n");
            writer.write(innerIndent);
            writer.write("for (int i = 0; i < ");
            writer.write(String.valueOf(overlay.getLength()));
            writer.write("; i++) {\n");
            writer.write(innerIndent);
            writer.write(INDENT);
            writer.write("buffer[baseOffset + ");
            writer.write(String.valueOf(overlay.getOffset()));
            writer.write(" + i] = (byte) 0xF0;\n");
            writer.write(innerIndent);
            writer.write("}\n");
        }

        // Encode the selected member
        writer.write("\n");
        writer.write(innerIndent);
        writer.write("// Encode selected member\n");

        for (String memberPath : overlay.getMemberPaths()) {
            Optional<CodegenFieldInfo> fieldOpt = model.findField(memberPath);
            if (fieldOpt.isPresent()) {
                CodegenFieldInfo field = fieldOpt.get();
                String getterName = "get" + capitalize(field.getJavaName()) + "()";

                writer.write(innerIndent);
                writer.write("if (\"");
                writer.write(memberPath);
                writer.write("\".equals(selectedMember)) {\n");

                writeFieldEncodeBody(writer, field, "dto", innerIndent + INDENT);

                writer.write(innerIndent);
                writer.write("}\n");
            }
        }

        writer.write(indent);
        writer.write("}\n\n");
    }

    private void writeFieldEncode(Writer writer, CodegenFieldInfo field, String dtoVar, String indent) throws IOException {
        String getterName = "get" + capitalize(field.getJavaName()) + "()";
        String fieldPath = field.getPath();

        writer.write(indent);
        writer.write("// Field: ");
        writer.write(fieldPath);
        writer.write(" at offset ");
        writer.write(String.valueOf(field.getOffset()));
        writer.write("\n");

        writer.write(indent);
        writer.write("if (");
        writer.write(dtoVar);
        writer.write(".");
        writer.write(getterName);
        writer.write(" != null) {\n");

        writeFieldEncodeBody(writer, field, dtoVar, indent + INDENT);

        writer.write(indent);
        writer.write("}\n\n");
    }

    private void writeFieldEncodeBody(Writer writer, CodegenFieldInfo field, String dtoVar, String indent) throws IOException {
        String getterName = "get" + capitalize(field.getJavaName()) + "()";
        String fieldPath = field.getPath();

        switch (field.getKind()) {
            case ALPHANUMERIC:
                writer.write(indent);
                writer.write("byte[] bytes = EbcdicCodecUtil.encode(");
                writer.write(dtoVar);
                writer.write(".");
                writer.write(getterName);
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("System.arraycopy(bytes, 0, buffer, baseOffset + ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                break;

            case DISPLAY_NUMERIC:
                if (field.getScale() > 0) {
                    writer.write(indent);
                    writer.write("ZonedDecimalCodec.encodeInto(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(", buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(");\n");
                } else {
                    writer.write(indent);
                    writer.write("ZonedDecimalCodec.encodeInto(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(");\n");
                }
                break;

            case COMP:
                if (field.getLength() <= 4) {
                    writer.write(indent);
                    writer.write("NumericCodec.encodeCompInto(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(".intValue(), buffer, baseOffset + ");
                } else {
                    writer.write(indent);
                    writer.write("NumericCodec.encodeCompLongInto(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, baseOffset + ");
                }
                writer.write(String.valueOf(field.getOffset()));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(", RECORD_NAME, \"");
                writer.write(fieldPath);
                writer.write("\", ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(");\n");
                break;

            case COMP3:
                if (field.getScale() == 0 && field.getDigits() <= 18) {
                    writer.write(indent);
                    writer.write("NumericCodec.encodeComp3LongInto(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(".longValue(), buffer, baseOffset + ");
                } else {
                    writer.write(indent);
                    writer.write("NumericCodec.encodeComp3Into(");
                    writer.write(dtoVar);
                    writer.write(".");
                    writer.write(getterName);
                    writer.write(", buffer, baseOffset + ");
                }
                writer.write(String.valueOf(field.getOffset()));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(", RECORD_NAME, \"");
                writer.write(fieldPath);
                writer.write("\", ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(");\n");
                break;
        }
    }

    private void writeDecodeMethod(Writer writer, CodegenRecordModel model, String dtoClassName) throws IOException {
        writer.write(INDENT);
        writer.write("/**\n");
        writer.write(INDENT);
        writer.write(" * Decode a byte array to a new DTO.\n");
        writer.write(INDENT);
        writer.write(" *\n");
        writer.write(INDENT);
        writer.write(" * @param buffer the buffer to decode (must be at least LENGTH bytes)\n");
        writer.write(INDENT);
        writer.write(" * @return the decoded DTO\n");
        writer.write(INDENT);
        writer.write(" */\n");
        writer.write(INDENT);
        writer.write("public static ");
        writer.write(dtoClassName);
        writer.write(" decode(byte[] buffer) {\n");

        writer.write(INDENT);
        writer.write(INDENT);
        writer.write("return decodeFrom(buffer, 0);\n");

        writer.write(INDENT);
        writer.write("}\n\n");
    }

    private void writeDecodeFromMethod(Writer writer, CodegenRecordModel model, String dtoClassName) throws IOException {
        writer.write(INDENT);
        writer.write("/**\n");
        writer.write(INDENT);
        writer.write(" * Decode from a buffer at the specified offset.\n");
        writer.write(INDENT);
        writer.write(" *\n");
        writer.write(INDENT);
        writer.write(" * @param buffer the buffer to decode from\n");
        writer.write(INDENT);
        writer.write(" * @param baseOffset the offset in the buffer to start reading\n");
        writer.write(INDENT);
        writer.write(" * @return the decoded DTO\n");
        writer.write(INDENT);
        writer.write(" */\n");
        writer.write(INDENT);
        writer.write("public static ");
        writer.write(dtoClassName);
        writer.write(" decodeFrom(byte[] buffer, int baseOffset) {\n");

        String indent2 = INDENT + INDENT;

        writer.write(indent2);
        writer.write(dtoClassName);
        writer.write(" dto = new ");
        writer.write(dtoClassName);
        writer.write("();\n\n");

        // Track OCCURS groups
        Map<String, OccursGroupInfo> occursGroups = identifyOccursGroups(model);

        // Decode OCCURS groups
        for (OccursGroupInfo groupInfo : occursGroups.values()) {
            writeOccursDecode(writer, model, groupInfo, dtoClassName, indent2);
        }

        // Decode regular fields (non-OCCURS)
        // For overlays, only decode the base member
        Set<String> decodedOverlayBases = new HashSet<>();

        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.isInOccurs()) {
                continue; // Handled by OCCURS group
            }

            // For overlay fields, only decode the base
            if (field.isInOverlay()) {
                if (!field.isOverlayBase()) {
                    continue; // Skip redefining members
                }
                decodedOverlayBases.add(field.getPath());
            }

            writeFieldDecode(writer, field, indent2);
        }

        writer.write(indent2);
        writer.write("return dto;\n");
        writer.write(INDENT);
        writer.write("}\n");
    }

    private void writeOccursDecode(Writer writer, CodegenRecordModel model,
                                    OccursGroupInfo groupInfo, String dtoClassName,
                                    String indent) throws IOException {
        String setterName = "set" + capitalize(groupInfo.javaFieldName);
        String listVar = groupInfo.javaFieldName + "List";
        String itemClassName = groupInfo.javaClassName;

        int elementSize = calculateOccursElementSize(groupInfo, model);

        writer.write(indent);
        writer.write("// Decode OCCURS group: ");
        writer.write(groupInfo.basePath);
        writer.write("\n");
        writer.write(indent);
        writer.write("List<");
        writer.write(dtoClassName);
        writer.write(".");
        writer.write(itemClassName);
        writer.write("> ");
        writer.write(listVar);
        writer.write(" = new ArrayList<>(");
        writer.write(String.valueOf(groupInfo.count));
        writer.write(");\n");

        writer.write(indent);
        writer.write("for (int i = 0; i < ");
        writer.write(String.valueOf(groupInfo.count));
        writer.write("; i++) {\n");

        String nestedIndent = indent + INDENT;
        writer.write(nestedIndent);
        writer.write(dtoClassName);
        writer.write(".");
        writer.write(itemClassName);
        writer.write(" item = new ");
        writer.write(dtoClassName);
        writer.write(".");
        writer.write(itemClassName);
        writer.write("();\n");

        writer.write(nestedIndent);
        writer.write("int itemOffset = baseOffset + ");
        writer.write(String.valueOf(groupInfo.baseOffset));
        writer.write(" + (i * ");
        writer.write(String.valueOf(elementSize));
        writer.write(");\n");

        // Decode each field in the OCCURS group
        for (CodegenFieldInfo field : groupInfo.fields) {
            int relativeOffset = field.getOffset() - groupInfo.baseOffset;
            writeOccursFieldDecode(writer, field, relativeOffset, nestedIndent);
        }

        writer.write(nestedIndent);
        writer.write(listVar);
        writer.write(".add(item);\n");

        writer.write(indent);
        writer.write("}\n");

        writer.write(indent);
        writer.write("dto.");
        writer.write(setterName);
        writer.write("(");
        writer.write(listVar);
        writer.write(");\n\n");
    }

    private void writeOccursFieldDecode(Writer writer, CodegenFieldInfo field, int relativeOffset, String indent) throws IOException {
        String setterName = "set" + capitalize(field.getJavaName());
        String fieldPath = field.getPath();

        switch (field.getKind()) {
            case ALPHANUMERIC:
                writer.write(indent);
                writer.write("byte[] bytes_");
                writer.write(field.getJavaName());
                writer.write(" = new byte[");
                writer.write(String.valueOf(field.getLength()));
                writer.write("];\n");
                writer.write(indent);
                writer.write("System.arraycopy(buffer, itemOffset + ");
                writer.write(String.valueOf(relativeOffset));
                writer.write(", bytes_");
                writer.write(field.getJavaName());
                writer.write(", 0, ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("item.");
                writer.write(setterName);
                writer.write("(EbcdicCodecUtil.decode(bytes_");
                writer.write(field.getJavaName());
                writer.write("));\n");
                break;

            case DISPLAY_NUMERIC:
                writer.write(indent);
                if (field.getScale() > 0) {
                    writer.write("item.");
                    writer.write(setterName);
                    writer.write("(ZonedDecimalCodec.decodeDecimalFrom(buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n");
                } else {
                    String javaType = field.getJavaType();
                    writer.write("item.");
                    writer.write(setterName);
                    writer.write("(");
                    if ("Integer".equals(javaType)) {
                        writer.write("(int) ");
                    }
                    writer.write("ZonedDecimalCodec.decodeFrom(buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n");
                }
                break;

            case COMP:
                writer.write(indent);
                writer.write("item.");
                writer.write(setterName);
                writer.write("(");
                if (field.getLength() <= 4) {
                    writer.write("NumericCodec.decodeCompFrom(buffer, itemOffset + ");
                } else {
                    writer.write("NumericCodec.decodeCompLongFrom(buffer, itemOffset + ");
                }
                writer.write(String.valueOf(relativeOffset));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write("));\n");
                break;

            case COMP3:
                writer.write(indent);
                writer.write("item.");
                writer.write(setterName);
                writer.write("(");
                if (field.getScale() == 0 && field.getDigits() <= 18) {
                    writer.write("NumericCodec.decodeComp3LongFrom(buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write("));\n");
                } else {
                    writer.write("NumericCodec.decodeComp3From(buffer, itemOffset + ");
                    writer.write(String.valueOf(relativeOffset));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n");
                }
                break;
        }
    }

    private void writeFieldDecode(Writer writer, CodegenFieldInfo field, String indent) throws IOException {
        String setterName = "set" + capitalize(field.getJavaName());
        String fieldPath = field.getPath();

        writer.write(indent);
        writer.write("// Field: ");
        writer.write(fieldPath);
        writer.write("\n");

        switch (field.getKind()) {
            case ALPHANUMERIC:
                writer.write(indent);
                writer.write("byte[] bytes_");
                writer.write(field.getJavaName());
                writer.write(" = new byte[");
                writer.write(String.valueOf(field.getLength()));
                writer.write("];\n");
                writer.write(indent);
                writer.write("System.arraycopy(buffer, baseOffset + ");
                writer.write(String.valueOf(field.getOffset()));
                writer.write(", bytes_");
                writer.write(field.getJavaName());
                writer.write(", 0, ");
                writer.write(String.valueOf(field.getLength()));
                writer.write(");\n");
                writer.write(indent);
                writer.write("dto.");
                writer.write(setterName);
                writer.write("(EbcdicCodecUtil.decode(bytes_");
                writer.write(field.getJavaName());
                writer.write("));\n\n");
                break;

            case DISPLAY_NUMERIC:
                writer.write(indent);
                if (field.getScale() > 0) {
                    writer.write("dto.");
                    writer.write(setterName);
                    writer.write("(ZonedDecimalCodec.decodeDecimalFrom(buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n\n");
                } else {
                    String javaType = field.getJavaType();
                    writer.write("dto.");
                    writer.write(setterName);
                    writer.write("(");
                    if ("Integer".equals(javaType)) {
                        writer.write("(int) ");
                    }
                    writer.write("ZonedDecimalCodec.decodeFrom(buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.isSigned()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n\n");
                }
                break;

            case COMP:
                writer.write(indent);
                writer.write("dto.");
                writer.write(setterName);
                writer.write("(");
                if (field.getLength() <= 4) {
                    writer.write("NumericCodec.decodeCompFrom(buffer, baseOffset + ");
                } else {
                    writer.write("NumericCodec.decodeCompLongFrom(buffer, baseOffset + ");
                }
                writer.write(String.valueOf(field.getOffset()));
                writer.write(", ");
                writer.write(String.valueOf(field.getLength()));
                writer.write("));\n\n");
                break;

            case COMP3:
                writer.write(indent);
                writer.write("dto.");
                writer.write(setterName);
                writer.write("(");
                if (field.getScale() == 0 && field.getDigits() <= 18) {
                    writer.write("NumericCodec.decodeComp3LongFrom(buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write("));\n\n");
                } else {
                    writer.write("NumericCodec.decodeComp3From(buffer, baseOffset + ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getLength()));
                    writer.write(", ");
                    writer.write(String.valueOf(field.getScale()));
                    writer.write(", RECORD_NAME, \"");
                    writer.write(fieldPath);
                    writer.write("\", ");
                    writer.write(String.valueOf(field.getOffset()));
                    writer.write("));\n\n");
                }
                break;
        }
    }

    // =========================================================================
    // Helper methods
    // =========================================================================

    private Map<String, OccursGroupInfo> identifyOccursGroups(CodegenRecordModel model) {
        Map<String, OccursGroupInfo> groups = new LinkedHashMap<>();

        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.getOccursShape() != null) {
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
                        info.baseOffset = Integer.MAX_VALUE;
                        groups.put(basePath, info);
                    }

                    // Track the minimum offset for this OCCURS group
                    if (field.getOccursIndex() == 0) {
                        info.fields.add(field);
                        info.baseOffset = Math.min(info.baseOffset, field.getOffset());
                    }
                }
            }
        }

        return groups;
    }

    private int calculateOccursElementSize(OccursGroupInfo groupInfo, CodegenRecordModel model) {
        int minOffset = Integer.MAX_VALUE;
        int maxEndOffset = 0;

        for (CodegenFieldInfo field : model.getFlatFields()) {
            if (field.getOccursShape() != null && field.getOccursIndex() == 0) {
                String basePath = extractOccursBasePath(field.getPath());
                if (groupInfo.basePath.equals(basePath)) {
                    minOffset = Math.min(minOffset, field.getOffset());
                    maxEndOffset = Math.max(maxEndOffset, field.getOffset() + field.getLength());
                }
            }
        }

        return maxEndOffset - minOffset;
    }

    private String extractOccursBasePath(String path) {
        int bracketIndex = path.indexOf('[');
        return bracketIndex > 0 ? path.substring(0, bracketIndex) : null;
    }

    private String toJavaFieldName(String basePath) {
        String[] parts = basePath.split("\\.");
        String name = parts[parts.length - 1];
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

    /**
     * Helper class to track OCCURS group information.
     */
    private static class OccursGroupInfo {
        String basePath;
        int count;
        String javaFieldName;
        String javaClassName;
        List<CodegenFieldInfo> fields;
        int baseOffset;
    }
}
