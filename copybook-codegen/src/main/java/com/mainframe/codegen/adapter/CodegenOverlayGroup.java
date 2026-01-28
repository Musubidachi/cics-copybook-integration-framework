package com.mainframe.codegen.adapter;

import java.util.List;
import java.util.Objects;

/**
 * Immutable representation of an overlay group (REDEFINES) for code generation.
 *
 * <p>An overlay group represents a region of a record where multiple field
 * definitions share the same bytes. The base member is the original definition,
 * and redefining members provide alternative interpretations of those bytes.</p>
 */
public final class CodegenOverlayGroup {

    private final String basePath;
    private final int offset;
    private final int length;
    private final List<String> memberPaths;

    /**
     * Constructs a new CodegenOverlayGroup.
     *
     * @param basePath    the path of the base (original) field
     * @param offset      the byte offset of the overlay region
     * @param length      the byte length of the overlay region (max of all members)
     * @param memberPaths all member paths including the base path
     */
    public CodegenOverlayGroup(String basePath, int offset, int length, List<String> memberPaths) {
        this.basePath = Objects.requireNonNull(basePath);
        this.offset = offset;
        this.length = length;
        this.memberPaths = List.copyOf(memberPaths);
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
     * @return an unmodifiable list of all member paths
     */
    public List<String> getMemberPaths() {
        return memberPaths;
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

    @Override
    public String toString() {
        return "CodegenOverlayGroup{" +
                "basePath='" + basePath + '\'' +
                ", offset=" + offset +
                ", length=" + length +
                ", memberPaths=" + memberPaths +
                '}';
    }
}
