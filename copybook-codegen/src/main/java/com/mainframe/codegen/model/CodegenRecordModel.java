package com.mainframe.codegen.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

/**
 * Complete internal representation of a copybook record for code generation.
 *
 * <p>This model is the output of the ModelAdapter and contains all information
 * needed to generate DTOs and codecs:</p>
 * <ul>
 *   <li>Record name and Java class name</li>
 *   <li>Total byte length</li>
 *   <li>Flat fields (sorted deterministically by offset, then path, then length)</li>
 *   <li>Overlay groups (sorted deterministically by offset, then base path)</li>
 *   <li>Root group (hierarchical structure for nested DTOs)</li>
 * </ul>
 */
public final class CodegenRecordModel {

    private final String recordName;
    private final String javaClassName;
    private final int totalLength;
    private final List<CodegenFieldInfo> flatFields;
    private final List<CodegenOverlayInfo> overlayGroups;
    private final CodegenGroup rootGroup;

    private CodegenRecordModel(Builder builder) {
        this.recordName = Objects.requireNonNull(builder.recordName);
        this.javaClassName = Objects.requireNonNull(builder.javaClassName);
        this.totalLength = builder.totalLength;

        // Sort fields deterministically
        List<CodegenFieldInfo> sortedFields = new ArrayList<>(builder.flatFields);
        sortedFields.sort(Comparator
                .comparingInt(CodegenFieldInfo::getOffset)
                .thenComparing(CodegenFieldInfo::getPath)
                .thenComparingInt(CodegenFieldInfo::getLength));
        this.flatFields = Collections.unmodifiableList(sortedFields);

        // Sort overlay groups deterministically
        List<CodegenOverlayInfo> sortedOverlays = new ArrayList<>(builder.overlayGroups);
        sortedOverlays.sort(Comparator
                .comparingInt(CodegenOverlayInfo::getOffset)
                .thenComparing(CodegenOverlayInfo::getBasePath));
        this.overlayGroups = Collections.unmodifiableList(sortedOverlays);

        this.rootGroup = builder.rootGroup;
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
     * @return an unmodifiable list of all flat fields, sorted deterministically
     */
    public List<CodegenFieldInfo> getFlatFields() {
        return flatFields;
    }

    /**
     * @return an unmodifiable list of overlay groups, sorted deterministically
     */
    public List<CodegenOverlayInfo> getOverlayGroups() {
        return overlayGroups;
    }

    /**
     * @return the root group of the hierarchical structure, may be null
     */
    public CodegenGroup getRootGroup() {
        return rootGroup;
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
    public Optional<CodegenFieldInfo> findField(String path) {
        return flatFields.stream()
                .filter(f -> f.getPath().equals(path))
                .findFirst();
    }

    /**
     * Find the overlay group that contains a field path.
     *
     * @param path the field path
     * @return the overlay group, or empty if not in an overlay
     */
    public Optional<CodegenOverlayInfo> findOverlayForField(String path) {
        return overlayGroups.stream()
                .filter(g -> g.containsMember(path))
                .findFirst();
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenRecordModel{" +
                "recordName='" + recordName + '\'' +
                ", javaClassName='" + javaClassName + '\'' +
                ", totalLength=" + totalLength +
                ", fieldCount=" + flatFields.size() +
                ", overlayGroupCount=" + overlayGroups.size() +
                '}';
    }

    /**
     * Builder for CodegenRecordModel.
     */
    public static final class Builder {
        private String recordName;
        private String javaClassName;
        private int totalLength;
        private List<CodegenFieldInfo> flatFields = new ArrayList<>();
        private List<CodegenOverlayInfo> overlayGroups = new ArrayList<>();
        private CodegenGroup rootGroup;

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

        public Builder flatFields(List<CodegenFieldInfo> flatFields) {
            this.flatFields = new ArrayList<>(flatFields);
            return this;
        }

        public Builder addField(CodegenFieldInfo field) {
            this.flatFields.add(field);
            return this;
        }

        public Builder overlayGroups(List<CodegenOverlayInfo> overlayGroups) {
            this.overlayGroups = new ArrayList<>(overlayGroups);
            return this;
        }

        public Builder addOverlayGroup(CodegenOverlayInfo group) {
            this.overlayGroups.add(group);
            return this;
        }

        public Builder rootGroup(CodegenGroup rootGroup) {
            this.rootGroup = rootGroup;
            return this;
        }

        public CodegenRecordModel build() {
            return new CodegenRecordModel(this);
        }
    }
}
