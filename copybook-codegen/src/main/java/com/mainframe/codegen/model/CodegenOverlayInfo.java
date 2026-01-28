package com.mainframe.codegen.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Represents an overlay group (REDEFINES) for code generation.
 *
 * <p>An overlay group represents a region of a record where multiple field
 * definitions share the same bytes. The base member is the original definition,
 * and redefining members provide alternative interpretations of those bytes.</p>
 *
 * <p>Member paths are sorted deterministically for consistent code generation.</p>
 */
public final class CodegenOverlayInfo {

    private final String basePath;
    private final int offset;
    private final int length;
    private final List<String> memberPaths;
    private final FieldKind baseKind;

    /**
     * Constructs a new CodegenOverlayInfo.
     *
     * @param basePath    the path of the base (original) field
     * @param offset      the byte offset of the overlay region
     * @param length      the byte length of the overlay region (max of all members)
     * @param memberPaths all member paths including the base path
     * @param baseKind    the field kind of the base member
     */
    public CodegenOverlayInfo(String basePath, int offset, int length,
                              List<String> memberPaths, FieldKind baseKind) {
        this.basePath = Objects.requireNonNull(basePath);
        this.offset = offset;
        this.length = length;
        // Sort member paths deterministically
        List<String> sorted = new ArrayList<>(memberPaths);
        Collections.sort(sorted);
        this.memberPaths = Collections.unmodifiableList(sorted);
        this.baseKind = Objects.requireNonNull(baseKind);
    }

    /**
     * @return the path of the base (original) field
     */
    public String getBasePath() {
        return basePath;
    }

    /**
     * @return the byte offset of the overlay region
     */
    public int getOffset() {
        return offset;
    }

    /**
     * @return the byte length of the overlay region
     */
    public int getLength() {
        return length;
    }

    /**
     * @return an unmodifiable list of all member paths, sorted alphabetically
     */
    public List<String> getMemberPaths() {
        return memberPaths;
    }

    /**
     * @return the field kind of the base member
     */
    public FieldKind getBaseKind() {
        return baseKind;
    }

    /**
     * @return the number of members in this overlay group
     */
    public int getMemberCount() {
        return memberPaths.size();
    }

    /**
     * Check if a path is a member of this overlay group.
     *
     * @param path the path to check
     * @return true if the path is a member
     */
    public boolean containsMember(String path) {
        return memberPaths.contains(path);
    }

    /**
     * Check if a path is a redefining member (not the base).
     *
     * @param path the path to check
     * @return true if the path is a redefining member
     */
    public boolean isRedefiningMember(String path) {
        return !basePath.equals(path) && memberPaths.contains(path);
    }

    /**
     * @return the list of redefining member paths (excluding the base)
     */
    public List<String> getRedefiningMembers() {
        List<String> result = new ArrayList<>();
        for (String path : memberPaths) {
            if (!path.equals(basePath)) {
                result.add(path);
            }
        }
        return Collections.unmodifiableList(result);
    }

    @Override
    public String toString() {
        return "CodegenOverlayInfo{" +
                "basePath='" + basePath + '\'' +
                ", offset=" + offset +
                ", length=" + length +
                ", memberPaths=" + memberPaths +
                ", baseKind=" + baseKind +
                '}';
    }
}
