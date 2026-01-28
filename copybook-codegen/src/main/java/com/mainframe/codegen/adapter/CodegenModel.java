package com.mainframe.codegen.adapter;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Immutable internal representation of a copybook record for code generation.
 *
 * <p>This model is the output of {@link ModelAdapter} and contains all
 * information needed to generate DTOs and codecs. It is decoupled from
 * upstream copybook-model classes.</p>
 */
public final class CodegenModel {

    private final String recordName;
    private final String javaClassName;
    private final int totalLength;
    private final List<CodegenField> fields;
    private final List<CodegenOverlayGroup> overlayGroups;

    private CodegenModel(Builder builder) {
        this.recordName = Objects.requireNonNull(builder.recordName);
        this.javaClassName = Objects.requireNonNull(builder.javaClassName);
        this.totalLength = builder.totalLength;
        this.fields = List.copyOf(builder.fields);
        this.overlayGroups = List.copyOf(builder.overlayGroups);
    }

    /**
     * @return the COBOL record name
     */
    public String getRecordName() {
        return recordName;
    }

    /**
     * @return the Java class name for the generated DTO
     */
    public String getJavaClassName() {
        return javaClassName;
    }

    /**
     * @return the total byte length of the record
     */
    public int getTotalLength() {
        return totalLength;
    }

    /**
     * @return an unmodifiable list of all leaf fields in offset order
     */
    public List<CodegenField> getFields() {
        return fields;
    }

    /**
     * @return an unmodifiable list of overlay groups
     */
    public List<CodegenOverlayGroup> getOverlayGroups() {
        return overlayGroups;
    }

    /**
     * @return the number of fields
     */
    public int getFieldCount() {
        return fields.size();
    }

    /**
     * @return true if there are overlay groups
     */
    public boolean hasOverlays() {
        return !overlayGroups.isEmpty();
    }

    /**
     * Find a field by its path.
     *
     * @param path the field path
     * @return the field, or empty if not found
     */
    public Optional<CodegenField> findField(String path) {
        return fields.stream()
                .filter(f -> f.getPath().equals(path))
                .findFirst();
    }

    /**
     * Find the overlay group that contains a field path.
     *
     * @param path the field path
     * @return the overlay group, or empty if not in an overlay
     */
    public Optional<CodegenOverlayGroup> findOverlayForField(String path) {
        return overlayGroups.stream()
                .filter(g -> g.containsMember(path))
                .findFirst();
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenModel{" +
                "recordName='" + recordName + '\'' +
                ", javaClassName='" + javaClassName + '\'' +
                ", totalLength=" + totalLength +
                ", fieldCount=" + fields.size() +
                ", overlayGroupCount=" + overlayGroups.size() +
                '}';
    }

    /**
     * Builder for CodegenModel.
     */
    public static final class Builder {
        private String recordName;
        private String javaClassName;
        private int totalLength;
        private List<CodegenField> fields = List.of();
        private List<CodegenOverlayGroup> overlayGroups = List.of();

        private Builder() {
        }

        public Builder recordName(String recordName) {
            this.recordName = recordName;
            return this;
        }

        public Builder javaClassName(String javaClassName) {
            this.javaClassName = javaClassName;
            return this;
        }

        public Builder totalLength(int totalLength) {
            this.totalLength = totalLength;
            return this;
        }

        public Builder fields(List<CodegenField> fields) {
            this.fields = fields;
            return this;
        }

        public Builder overlayGroups(List<CodegenOverlayGroup> overlayGroups) {
            this.overlayGroups = overlayGroups;
            return this;
        }

        public CodegenModel build() {
            return new CodegenModel(this);
        }
    }
}
