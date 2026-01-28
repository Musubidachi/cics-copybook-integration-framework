package com.mainframe.codegen;

import com.mainframe.codegen.encoding.AlphaTrimMode;
import com.mainframe.codegen.encoding.CompBinaryEndianness;
import com.mainframe.codegen.encoding.DisplayNumericEncoding;
import com.mainframe.codegen.encoding.DisplaySignMode;
import com.mainframe.codegen.encoding.EbcdicCodePage;

import java.nio.file.Path;
import java.util.Objects;

/**
 * Configuration for the copybook code generator.
 *
 * <p>This class specifies all options for generating Java DTOs and codecs
 * from copybook layout models. Use the {@link Builder} to construct
 * instances with the desired settings.</p>
 *
 * <p><strong>Required fields:</strong></p>
 * <ul>
 *   <li>{@code outputSourceRoot} - directory for generated source files</li>
 *   <li>{@code basePackage} - Java package for generated classes</li>
 * </ul>
 *
 * <p><strong>Example usage:</strong></p>
 * <pre>{@code
 * GenerationConfig config = GenerationConfig.builder()
 *     .outputSourceRoot(Path.of("target/generated-sources/copybook"))
 *     .basePackage("com.example.generated")
 *     .generateAsRecords(true)
 *     .build();
 * }</pre>
 */
public final class GenerationConfig {

    /**
     * Default configuration for common use cases.
     * Note: recordName should be set before using this configuration.
     */
    public static final GenerationConfig DEFAULT = builder()
            .outputSourceRoot(Path.of("target/generated-sources/copybook"))
            .basePackage("com.mainframe.generated")
            .recordName("Record")
            .build();

    // Required fields
    private final Path outputSourceRoot;
    private final String basePackage;
    private final String recordName;

    // DTO generation options
    private final boolean generateAsRecords;
    private final boolean generateNestedGroups;
    private final boolean useListsForOccurs;

    // Encoding options
    private final EbcdicCodePage ebcdicCodePage;
    private final AlphaTrimMode alphaTrimMode;
    private final DisplayNumericEncoding displayNumericEncoding;
    private final DisplaySignMode displaySignMode;
    private final CompBinaryEndianness compBinaryEndianness;

    // Enforcement options
    private final boolean strictOverlayEnforcement;
    private final boolean strictOccursSize;
    private final boolean strictOverflow;

    // Metadata options
    private final boolean emitMetadataJson;

    private GenerationConfig(Builder builder) {
        this.outputSourceRoot = Objects.requireNonNull(builder.outputSourceRoot, "outputSourceRoot is required");
        this.basePackage = Objects.requireNonNull(builder.basePackage, "basePackage is required");
        this.recordName = Objects.requireNonNull(builder.recordName, "recordName is required");

        this.generateAsRecords = builder.generateAsRecords;
        this.generateNestedGroups = builder.generateNestedGroups;
        this.useListsForOccurs = builder.useListsForOccurs;

        this.ebcdicCodePage = builder.ebcdicCodePage;
        this.alphaTrimMode = builder.alphaTrimMode;
        this.displayNumericEncoding = builder.displayNumericEncoding;
        this.displaySignMode = builder.displaySignMode;
        this.compBinaryEndianness = builder.compBinaryEndianness;

        this.strictOverlayEnforcement = builder.strictOverlayEnforcement;
        this.strictOccursSize = builder.strictOccursSize;
        this.strictOverflow = builder.strictOverflow;

        this.emitMetadataJson = builder.emitMetadataJson;
    }

    /**
     * @return a new builder instance
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * @return the output directory for generated source files
     */
    public Path getOutputSourceRoot() {
        return outputSourceRoot;
    }

    /**
     * @return the Java package for generated classes
     */
    public String getBasePackage() {
        return basePackage;
    }

    /**
     * @return the COBOL record name (typically from the 01 level)
     */
    public String getRecordName() {
        return recordName;
    }

    /**
     * @return true if DTOs should be generated as Java records
     */
    public boolean isGenerateAsRecords() {
        return generateAsRecords;
    }

    /**
     * @return true if nested group structures should be generated as nested DTOs
     */
    public boolean isGenerateNestedGroups() {
        return generateNestedGroups;
    }

    /**
     * @return true if OCCURS should be represented as Lists
     */
    public boolean isUseListsForOccurs() {
        return useListsForOccurs;
    }

    /**
     * @return the EBCDIC code page for character encoding
     */
    public EbcdicCodePage getEbcdicCodePage() {
        return ebcdicCodePage;
    }

    /**
     * @return the trim mode for alphanumeric fields
     */
    public AlphaTrimMode getAlphaTrimMode() {
        return alphaTrimMode;
    }

    /**
     * @return the encoding format for DISPLAY numeric fields
     */
    public DisplayNumericEncoding getDisplayNumericEncoding() {
        return displayNumericEncoding;
    }

    /**
     * @return the sign encoding mode for signed DISPLAY fields
     */
    public DisplaySignMode getDisplaySignMode() {
        return displaySignMode;
    }

    /**
     * @return the byte order for COMP binary fields
     */
    public CompBinaryEndianness getCompBinaryEndianness() {
        return compBinaryEndianness;
    }

    /**
     * @return true if overlay constraints should be strictly enforced
     */
    public boolean isStrictOverlayEnforcement() {
        return strictOverlayEnforcement;
    }

    /**
     * @return true if OCCURS list sizes must exactly match
     */
    public boolean isStrictOccursSize() {
        return strictOccursSize;
    }

    /**
     * @return true if numeric overflow should throw exceptions
     */
    public boolean isStrictOverflow() {
        return strictOverflow;
    }

    /**
     * @return true if metadata JSON files should be emitted
     */
    public boolean isEmitMetadataJson() {
        return emitMetadataJson;
    }

    /**
     * @return the resolved path for generated Java sources
     */
    public Path getPackagePath() {
        return outputSourceRoot.resolve(basePackage.replace('.', '/'));
    }

    /**
     * @return the resolved path for metadata files
     */
    public Path getMetadataPath() {
        return getPackagePath().resolve("meta");
    }

    @Override
    public String toString() {
        return "GenerationConfig{" +
                "outputSourceRoot=" + outputSourceRoot +
                ", basePackage='" + basePackage + '\'' +
                ", recordName='" + recordName + '\'' +
                ", generateAsRecords=" + generateAsRecords +
                ", generateNestedGroups=" + generateNestedGroups +
                ", useListsForOccurs=" + useListsForOccurs +
                ", ebcdicCodePage=" + ebcdicCodePage +
                ", alphaTrimMode=" + alphaTrimMode +
                ", displayNumericEncoding=" + displayNumericEncoding +
                ", displaySignMode=" + displaySignMode +
                ", compBinaryEndianness=" + compBinaryEndianness +
                ", strictOverlayEnforcement=" + strictOverlayEnforcement +
                ", strictOccursSize=" + strictOccursSize +
                ", strictOverflow=" + strictOverflow +
                ", emitMetadataJson=" + emitMetadataJson +
                '}';
    }

    /**
     * Builder for {@link GenerationConfig}.
     */
    public static final class Builder {

        private Path outputSourceRoot;
        private String basePackage;
        private String recordName;

        // DTO defaults
        private boolean generateAsRecords = false;
        private boolean generateNestedGroups = true;  // Required by spec
        private boolean useListsForOccurs = true;     // Required by spec

        // Encoding defaults
        private EbcdicCodePage ebcdicCodePage = EbcdicCodePage.IBM037;
        private AlphaTrimMode alphaTrimMode = AlphaTrimMode.RTRIM_EBCDIC_SPACE;
        private DisplayNumericEncoding displayNumericEncoding = DisplayNumericEncoding.EBCDIC_ZONED;
        private DisplaySignMode displaySignMode = DisplaySignMode.OVERPUNCH_TRAILING; // Required default
        private CompBinaryEndianness compBinaryEndianness = CompBinaryEndianness.BIG_ENDIAN;

        // Enforcement defaults
        private boolean strictOverlayEnforcement = true;
        private boolean strictOccursSize = true;
        private boolean strictOverflow = true;

        // Metadata defaults
        private boolean emitMetadataJson = true;

        private Builder() {
        }

        /**
         * Sets the output directory for generated source files.
         *
         * @param outputSourceRoot the output directory path
         * @return this builder
         */
        public Builder outputSourceRoot(Path outputSourceRoot) {
            this.outputSourceRoot = outputSourceRoot;
            return this;
        }

        /**
         * Sets the Java package for generated classes.
         *
         * @param basePackage the Java package name
         * @return this builder
         */
        public Builder basePackage(String basePackage) {
            this.basePackage = basePackage;
            return this;
        }

        /**
         * Sets the COBOL record name (typically from the 01 level).
         *
         * @param recordName the record name
         * @return this builder
         */
        public Builder recordName(String recordName) {
            this.recordName = recordName;
            return this;
        }

        /**
         * Sets whether to generate DTOs as Java records.
         *
         * @param generateAsRecords true for records, false for classes
         * @return this builder
         */
        public Builder generateAsRecords(boolean generateAsRecords) {
            this.generateAsRecords = generateAsRecords;
            return this;
        }

        /**
         * Sets whether to generate nested group structures.
         * (This is required by spec and cannot be disabled.)
         *
         * @param generateNestedGroups must be true
         * @return this builder
         */
        public Builder generateNestedGroups(boolean generateNestedGroups) {
            this.generateNestedGroups = generateNestedGroups;
            return this;
        }

        /**
         * Sets whether to use Lists for OCCURS arrays.
         * (This is required by spec and cannot be disabled.)
         *
         * @param useListsForOccurs must be true
         * @return this builder
         */
        public Builder useListsForOccurs(boolean useListsForOccurs) {
            this.useListsForOccurs = useListsForOccurs;
            return this;
        }

        /**
         * Sets the EBCDIC code page.
         *
         * @param ebcdicCodePage the code page
         * @return this builder
         */
        public Builder ebcdicCodePage(EbcdicCodePage ebcdicCodePage) {
            this.ebcdicCodePage = ebcdicCodePage;
            return this;
        }

        /**
         * Sets the alphanumeric trim mode.
         *
         * @param alphaTrimMode the trim mode
         * @return this builder
         */
        public Builder alphaTrimMode(AlphaTrimMode alphaTrimMode) {
            this.alphaTrimMode = alphaTrimMode;
            return this;
        }

        /**
         * Sets the DISPLAY numeric encoding format.
         *
         * @param displayNumericEncoding the encoding format
         * @return this builder
         */
        public Builder displayNumericEncoding(DisplayNumericEncoding displayNumericEncoding) {
            this.displayNumericEncoding = displayNumericEncoding;
            return this;
        }

        /**
         * Sets the sign mode for signed DISPLAY fields.
         * Default is OVERPUNCH_TRAILING as required by spec.
         *
         * @param displaySignMode the sign mode
         * @return this builder
         */
        public Builder displaySignMode(DisplaySignMode displaySignMode) {
            this.displaySignMode = displaySignMode;
            return this;
        }

        /**
         * Sets the byte order for COMP binary fields.
         *
         * @param compBinaryEndianness the byte order
         * @return this builder
         */
        public Builder compBinaryEndianness(CompBinaryEndianness compBinaryEndianness) {
            this.compBinaryEndianness = compBinaryEndianness;
            return this;
        }

        /**
         * Sets whether overlay constraints should be strictly enforced.
         *
         * @param strictOverlayEnforcement true for strict enforcement
         * @return this builder
         */
        public Builder strictOverlayEnforcement(boolean strictOverlayEnforcement) {
            this.strictOverlayEnforcement = strictOverlayEnforcement;
            return this;
        }

        /**
         * Sets whether OCCURS list sizes must exactly match.
         *
         * @param strictOccursSize true for strict size checking
         * @return this builder
         */
        public Builder strictOccursSize(boolean strictOccursSize) {
            this.strictOccursSize = strictOccursSize;
            return this;
        }

        /**
         * Sets whether numeric overflow should throw exceptions.
         *
         * @param strictOverflow true for strict overflow checking
         * @return this builder
         */
        public Builder strictOverflow(boolean strictOverflow) {
            this.strictOverflow = strictOverflow;
            return this;
        }

        /**
         * Sets whether metadata JSON files should be emitted.
         *
         * @param emitMetadataJson true to emit metadata
         * @return this builder
         */
        public Builder emitMetadataJson(boolean emitMetadataJson) {
            this.emitMetadataJson = emitMetadataJson;
            return this;
        }

        /**
         * Builds the configuration.
         *
         * @return the configured {@link GenerationConfig}
         * @throws NullPointerException if required fields are missing
         */
        public GenerationConfig build() {
            return new GenerationConfig(this);
        }
    }
}
