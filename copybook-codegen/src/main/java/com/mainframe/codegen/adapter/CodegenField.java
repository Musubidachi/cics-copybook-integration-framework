package com.mainframe.codegen.adapter;

import com.mainframe.model.Usage;

import java.util.Objects;

/**
 * Immutable internal representation of a field for code generation.
 *
 * <p>This class is used by the code generator and is decoupled from
 * the upstream copybook-model classes.</p>
 */
public final class CodegenField {

    private final String path;
    private final String javaName;
    private final int offset;
    private final int length;
    private final FieldType type;
    private final Usage usage;
    private final int digits;
    private final int scale;
    private final boolean signed;
    private final int occursIndex;  // -1 if not part of OCCURS

    private CodegenField(Builder builder) {
        this.path = Objects.requireNonNull(builder.path);
        this.javaName = Objects.requireNonNull(builder.javaName);
        this.offset = builder.offset;
        this.length = builder.length;
        this.type = Objects.requireNonNull(builder.type);
        this.usage = builder.usage;
        this.digits = builder.digits;
        this.scale = builder.scale;
        this.signed = builder.signed;
        this.occursIndex = builder.occursIndex;
    }

    public String getPath() {
        return path;
    }

    public String getJavaName() {
        return javaName;
    }

    public int getOffset() {
        return offset;
    }

    public int getLength() {
        return length;
    }

    public FieldType getType() {
        return type;
    }

    public Usage getUsage() {
        return usage;
    }

    public int getDigits() {
        return digits;
    }

    public int getScale() {
        return scale;
    }

    public boolean isSigned() {
        return signed;
    }

    public int getOccursIndex() {
        return occursIndex;
    }

    public boolean isPartOfOccurs() {
        return occursIndex >= 0;
    }

    public boolean isNumeric() {
        return type == FieldType.DISPLAY_NUMERIC || type == FieldType.COMP || type == FieldType.COMP3;
    }

    public boolean isAlphanumeric() {
        return type == FieldType.ALPHANUMERIC;
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenField{" +
                "path='" + path + '\'' +
                ", javaName='" + javaName + '\'' +
                ", offset=" + offset +
                ", length=" + length +
                ", type=" + type +
                ", usage=" + usage +
                ", digits=" + digits +
                ", scale=" + scale +
                ", signed=" + signed +
                ", occursIndex=" + occursIndex +
                '}';
    }

    /**
     * Builder for CodegenField.
     */
    public static final class Builder {
        private String path;
        private String javaName;
        private int offset;
        private int length;
        private FieldType type;
        private Usage usage;
        private int digits;
        private int scale;
        private boolean signed;
        private int occursIndex = -1;

        private Builder() {
        }

        public Builder path(String path) {
            this.path = path;
            return this;
        }

        public Builder javaName(String javaName) {
            this.javaName = javaName;
            return this;
        }

        public Builder offset(int offset) {
            this.offset = offset;
            return this;
        }

        public Builder length(int length) {
            this.length = length;
            return this;
        }

        public Builder type(FieldType type) {
            this.type = type;
            return this;
        }

        public Builder usage(Usage usage) {
            this.usage = usage;
            return this;
        }

        public Builder digits(int digits) {
            this.digits = digits;
            return this;
        }

        public Builder scale(int scale) {
            this.scale = scale;
            return this;
        }

        public Builder signed(boolean signed) {
            this.signed = signed;
            return this;
        }

        public Builder occursIndex(int occursIndex) {
            this.occursIndex = occursIndex;
            return this;
        }

        public CodegenField build() {
            return new CodegenField(this);
        }
    }

    /**
     * Field types for code generation.
     */
    public enum FieldType {
        ALPHANUMERIC,
        DISPLAY_NUMERIC,
        COMP,
        COMP3
    }
}
