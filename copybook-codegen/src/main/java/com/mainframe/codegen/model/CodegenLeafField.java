package com.mainframe.codegen.model;

import java.util.Objects;

/**
 * Represents a leaf field within a group structure for code generation.
 *
 * <p>This is a simplified representation of a field that contains only
 * the information needed for the group tree structure. It references
 * the underlying flat field for full details.</p>
 */
public final class CodegenLeafField {

    private final String name;
    private final String javaName;
    private final String fullPath;
    private final int offset;
    private final int length;
    private final FieldKind kind;
    private final String javaType;
    private final boolean inOverlay;

    private CodegenLeafField(Builder builder) {
        this.name = Objects.requireNonNull(builder.name);
        this.javaName = Objects.requireNonNull(builder.javaName);
        this.fullPath = Objects.requireNonNull(builder.fullPath);
        this.offset = builder.offset;
        this.length = builder.length;
        this.kind = Objects.requireNonNull(builder.kind);
        this.javaType = Objects.requireNonNull(builder.javaType);
        this.inOverlay = builder.inOverlay;
    }

    /**
     * @return the COBOL name of this field
     */
    public String getName() {
        return name;
    }

    /**
     * @return the Java identifier name for this field
     */
    public String getJavaName() {
        return javaName;
    }

    /**
     * @return the full path to this field (e.g., "RECORD.CUSTOMER.NAME")
     */
    public String getFullPath() {
        return fullPath;
    }

    /**
     * @return the byte offset of this field
     */
    public int getOffset() {
        return offset;
    }

    /**
     * @return the byte length of this field
     */
    public int getLength() {
        return length;
    }

    /**
     * @return the field kind (alphanumeric, display numeric, etc.)
     */
    public FieldKind getKind() {
        return kind;
    }

    /**
     * @return the boxed Java type name for this field
     */
    public String getJavaType() {
        return javaType;
    }

    /**
     * @return true if this field is part of an overlay group
     */
    public boolean isInOverlay() {
        return inOverlay;
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenLeafField{" +
                "name='" + name + '\'' +
                ", javaName='" + javaName + '\'' +
                ", offset=" + offset +
                ", length=" + length +
                ", kind=" + kind +
                ", javaType='" + javaType + '\'' +
                ", inOverlay=" + inOverlay +
                '}';
    }

    /**
     * Builder for CodegenLeafField.
     */
    public static final class Builder {
        private String name;
        private String javaName;
        private String fullPath;
        private int offset;
        private int length;
        private FieldKind kind;
        private String javaType;
        private boolean inOverlay;

        private Builder() {
        }

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder javaName(String javaName) {
            this.javaName = javaName;
            return this;
        }

        public Builder fullPath(String fullPath) {
            this.fullPath = fullPath;
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

        public Builder kind(FieldKind kind) {
            this.kind = kind;
            return this;
        }

        public Builder javaType(String javaType) {
            this.javaType = javaType;
            return this;
        }

        public Builder inOverlay(boolean inOverlay) {
            this.inOverlay = inOverlay;
            return this;
        }

        public CodegenLeafField build() {
            return new CodegenLeafField(this);
        }
    }
}
