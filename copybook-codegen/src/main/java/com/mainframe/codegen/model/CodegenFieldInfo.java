package com.mainframe.codegen.model;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Complete internal representation of a field for code generation.
 *
 * <p>This class contains all information needed to generate DTO fields
 * and codec operations, including:</p>
 * <ul>
 *   <li>Full canonical path and parsed segments</li>
 *   <li>Byte offset and length</li>
 *   <li>Usage type and PIC summary</li>
 *   <li>Field kind and boxed Java type</li>
 *   <li>Occurs shape and overlay membership</li>
 * </ul>
 */
public final class CodegenFieldInfo {

    private final String path;
    private final List<PathSegment> pathSegments;
    private final String javaName;
    private final int offset;
    private final int length;
    private final FieldKind kind;
    private final String javaType;
    private final int digits;
    private final int scale;
    private final boolean signed;
    private final OccursShape occursShape;
    private final int occursIndex;
    private final boolean inOverlay;
    private final String overlayBasePath;

    private CodegenFieldInfo(Builder builder) {
        this.path = Objects.requireNonNull(builder.path);
        this.pathSegments = Collections.unmodifiableList(builder.pathSegments);
        this.javaName = Objects.requireNonNull(builder.javaName);
        this.offset = builder.offset;
        this.length = builder.length;
        this.kind = Objects.requireNonNull(builder.kind);
        this.javaType = Objects.requireNonNull(builder.javaType);
        this.digits = builder.digits;
        this.scale = builder.scale;
        this.signed = builder.signed;
        this.occursShape = builder.occursShape;
        this.occursIndex = builder.occursIndex;
        this.inOverlay = builder.inOverlay;
        this.overlayBasePath = builder.overlayBasePath;
    }

    /**
     * @return the full canonical path (e.g., "CUSTOMER.ADDRESS.ZIPCODE")
     */
    public String getPath() {
        return path;
    }

    /**
     * @return the parsed path segments
     */
    public List<PathSegment> getPathSegments() {
        return pathSegments;
    }

    /**
     * @return the Java identifier name for this field
     */
    public String getJavaName() {
        return javaName;
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
     * @return the field kind
     */
    public FieldKind getKind() {
        return kind;
    }

    /**
     * @return the boxed Java type name
     */
    public String getJavaType() {
        return javaType;
    }

    /**
     * @return the number of digits for numeric fields
     */
    public int getDigits() {
        return digits;
    }

    /**
     * @return the decimal scale for numeric fields
     */
    public int getScale() {
        return scale;
    }

    /**
     * @return true if this is a signed numeric field
     */
    public boolean isSigned() {
        return signed;
    }

    /**
     * @return the occurs shape if this field is part of an OCCURS, null otherwise
     */
    public OccursShape getOccursShape() {
        return occursShape;
    }

    /**
     * @return the 0-based index within the OCCURS, or -1 if not in OCCURS
     */
    public int getOccursIndex() {
        return occursIndex;
    }

    /**
     * @return true if this field is part of an OCCURS array
     */
    public boolean isInOccurs() {
        return occursIndex >= 0;
    }

    /**
     * @return true if this field is part of an overlay group
     */
    public boolean isInOverlay() {
        return inOverlay;
    }

    /**
     * @return the base path of the overlay group, or null if not in overlay
     */
    public String getOverlayBasePath() {
        return overlayBasePath;
    }

    /**
     * @return true if this is the base field of an overlay group
     */
    public boolean isOverlayBase() {
        return inOverlay && path.equals(overlayBasePath);
    }

    /**
     * @return true if this is a numeric field
     */
    public boolean isNumeric() {
        return kind == FieldKind.DISPLAY_NUMERIC || kind == FieldKind.COMP || kind == FieldKind.COMP3;
    }

    /**
     * @return true if this is an alphanumeric field
     */
    public boolean isAlphanumeric() {
        return kind == FieldKind.ALPHANUMERIC;
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenFieldInfo{" +
                "path='" + path + '\'' +
                ", javaName='" + javaName + '\'' +
                ", offset=" + offset +
                ", length=" + length +
                ", kind=" + kind +
                ", javaType='" + javaType + '\'' +
                ", digits=" + digits +
                ", scale=" + scale +
                ", signed=" + signed +
                ", occursIndex=" + occursIndex +
                ", inOverlay=" + inOverlay +
                '}';
    }

    /**
     * Represents a single segment in a field path.
     */
    public static final class PathSegment {
        private final String name;
        private final int occursIndex;

        public PathSegment(String name, int occursIndex) {
            this.name = Objects.requireNonNull(name);
            this.occursIndex = occursIndex;
        }

        public PathSegment(String name) {
            this(name, -1);
        }

        /**
         * @return the segment name
         */
        public String getName() {
            return name;
        }

        /**
         * @return the occurs index, or -1 if not an OCCURS segment
         */
        public int getOccursIndex() {
            return occursIndex;
        }

        /**
         * @return true if this segment has an OCCURS index
         */
        public boolean isOccurs() {
            return occursIndex >= 0;
        }

        @Override
        public String toString() {
            return isOccurs() ? name + "[" + occursIndex + "]" : name;
        }
    }

    /**
     * Builder for CodegenFieldInfo.
     */
    public static final class Builder {
        private String path;
        private List<PathSegment> pathSegments = List.of();
        private String javaName;
        private int offset;
        private int length;
        private FieldKind kind;
        private String javaType = "Object";
        private int digits;
        private int scale;
        private boolean signed;
        private OccursShape occursShape;
        private int occursIndex = -1;
        private boolean inOverlay;
        private String overlayBasePath;

        private Builder() {
        }

        public Builder path(String path) {
            this.path = path;
            return this;
        }

        public Builder pathSegments(List<PathSegment> pathSegments) {
            this.pathSegments = List.copyOf(pathSegments);
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

        public Builder kind(FieldKind kind) {
            this.kind = kind;
            return this;
        }

        public Builder javaType(String javaType) {
            this.javaType = javaType;
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

        public Builder occursShape(OccursShape occursShape) {
            this.occursShape = occursShape;
            return this;
        }

        public Builder occursIndex(int occursIndex) {
            this.occursIndex = occursIndex;
            return this;
        }

        public Builder inOverlay(boolean inOverlay) {
            this.inOverlay = inOverlay;
            return this;
        }

        public Builder overlayBasePath(String overlayBasePath) {
            this.overlayBasePath = overlayBasePath;
            return this;
        }

        public CodegenFieldInfo build() {
            return new CodegenFieldInfo(this);
        }
    }
}
