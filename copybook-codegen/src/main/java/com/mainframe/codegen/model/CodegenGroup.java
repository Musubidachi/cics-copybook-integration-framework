package com.mainframe.codegen.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * Represents a group node in the copybook structure for code generation.
 *
 * <p>A group contains child groups and leaf fields. Groups can have an
 * associated {@link OccursShape} if they represent an OCCURS array.</p>
 *
 * <p>The group tree is built deterministically from field paths, where
 * dotted segments become nested groups and [i] segments are collapsed
 * into a single group with an OccursShape.</p>
 */
public final class CodegenGroup {

    private final String name;
    private final String javaName;
    private final String fullPath;
    private final List<CodegenGroup> childGroups;
    private final List<CodegenLeafField> leafFields;
    private final OccursShape occursShape;

    private CodegenGroup(Builder builder) {
        this.name = Objects.requireNonNull(builder.name);
        this.javaName = Objects.requireNonNull(builder.javaName);
        this.fullPath = Objects.requireNonNull(builder.fullPath);

        // Sort child groups deterministically by name
        List<CodegenGroup> sortedGroups = new ArrayList<>(builder.childGroups);
        sortedGroups.sort(Comparator.comparing(CodegenGroup::getName));
        this.childGroups = Collections.unmodifiableList(sortedGroups);

        // Sort leaf fields deterministically by offset, then name
        List<CodegenLeafField> sortedFields = new ArrayList<>(builder.leafFields);
        sortedFields.sort(Comparator
                .comparingInt(CodegenLeafField::getOffset)
                .thenComparing(CodegenLeafField::getName));
        this.leafFields = Collections.unmodifiableList(sortedFields);

        this.occursShape = builder.occursShape;
    }

    /**
     * @return the COBOL name of this group
     */
    public String getName() {
        return name;
    }

    /**
     * @return the Java identifier name for this group
     */
    public String getJavaName() {
        return javaName;
    }

    /**
     * @return the full path to this group (e.g., "RECORD.CUSTOMER.ADDRESS")
     */
    public String getFullPath() {
        return fullPath;
    }

    /**
     * @return an unmodifiable list of child groups, sorted by name
     */
    public List<CodegenGroup> getChildGroups() {
        return childGroups;
    }

    /**
     * @return an unmodifiable list of leaf fields, sorted by offset then name
     */
    public List<CodegenLeafField> getLeafFields() {
        return leafFields;
    }

    /**
     * @return the occurs shape if this is an OCCURS group, null otherwise
     */
    public OccursShape getOccursShape() {
        return occursShape;
    }

    /**
     * @return true if this group has an OCCURS shape
     */
    public boolean isOccurs() {
        return occursShape != null;
    }

    /**
     * @return true if this group has no children (neither groups nor fields)
     */
    public boolean isEmpty() {
        return childGroups.isEmpty() && leafFields.isEmpty();
    }

    public static Builder builder() {
        return new Builder();
    }

    @Override
    public String toString() {
        return "CodegenGroup{" +
                "name='" + name + '\'' +
                ", javaName='" + javaName + '\'' +
                ", childGroups=" + childGroups.size() +
                ", leafFields=" + leafFields.size() +
                ", occursShape=" + occursShape +
                '}';
    }

    /**
     * Builder for CodegenGroup.
     */
    public static final class Builder {
        private String name = "";
        private String javaName = "";
        private String fullPath = "";
        private List<CodegenGroup> childGroups = new ArrayList<>();
        private List<CodegenLeafField> leafFields = new ArrayList<>();
        private OccursShape occursShape;

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

        public Builder addChildGroup(CodegenGroup group) {
            this.childGroups.add(group);
            return this;
        }

        public Builder childGroups(List<CodegenGroup> childGroups) {
            this.childGroups = new ArrayList<>(childGroups);
            return this;
        }

        public Builder addLeafField(CodegenLeafField field) {
            this.leafFields.add(field);
            return this;
        }

        public Builder leafFields(List<CodegenLeafField> leafFields) {
            this.leafFields = new ArrayList<>(leafFields);
            return this;
        }

        public Builder occursShape(OccursShape occursShape) {
            this.occursShape = occursShape;
            return this;
        }

        public CodegenGroup build() {
            return new CodegenGroup(this);
        }
    }
}
