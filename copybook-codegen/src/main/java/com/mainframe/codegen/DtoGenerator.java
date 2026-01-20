package com.mainframe.codegen;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * A simple demonstration DTO generator.
 *
 * In a fully fledged implementation this class would traverse a LayoutModel
 * (produced by the copybook-model module) and generate corresponding DTO
 * classes and codecs. For now it emits a single placeholder DTO to verify
 * that code generation infrastructure is working.
 */
public class DtoGenerator {

    /**
     * Generate a placeholder DTO into the given output directory.
     *
     * @param outputDir directory where Java source files will be written
     * @throws IOException if any I/O error occurs
     */
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
