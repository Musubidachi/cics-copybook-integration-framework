package com.mainframe.model;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a node within the parsed copybook abstract syntax tree.
 * A node may be a group (if {@code children} is non‑empty) or an
 * elementary field (if {@code children} is empty).  Occurrences and
 * redefines apply to the current node only; nested OCCURS multiply
 * through the tree.
 *
 * <p>This class lives in the model module purely to allow the layout
 * engine to operate without depending on the parser implementation.
 * The actual parser should populate instances of this class and
 * assemble them into a {@link CopybookAst}.</p>
 */
public class DataItemNode {
    private final int level;
    private final String name;
    private final PicSummary pic;
    private final Usage usage;
    private final Integer occurs;
    private final String redefines;
    private final List<DataItemNode> children;
    private final List<ConditionName> conditions;

    public DataItemNode(int level, String name, PicSummary pic, Usage usage,
                        Integer occurs, String redefines,
                        List<DataItemNode> children,
                        List<ConditionName> conditions) {
        this.level = level;
        this.name = name;
        this.pic = pic;
        this.usage = usage;
        this.occurs = occurs;
        this.redefines = redefines;
        this.children = children != null ? List.copyOf(children) : List.of();
        this.conditions = conditions != null ? List.copyOf(conditions) : List.of();
    }

    public int level() {
        return level;
    }

    public String name() {
        return name;
    }

    public PicSummary pic() {
        return pic;
    }

    public Usage usage() {
        return usage;
    }

    /**
     * Returns the OCCURS count, or {@code null} if no fixed OCCURS is
     * defined for this node.  A value greater than one indicates that
     * the structure (or elementary field) repeats in sequence.
     */
    public Integer occurs() {
        return occurs;
    }

    /**
     * Returns the name of the field that this node redefines, or
     * {@code null} if it is not redefining another field.  The
     * redefined field must precede this one in declaration order.
     */
    public String redefines() {
        return redefines;
    }

    public List<DataItemNode> children() {
        return children;
    }

    public List<ConditionName> conditions() {
        return conditions;
    }

    /**
     * Returns {@code true} if this node represents a group item (i.e.
     * has children), otherwise {@code false}.
     */
    public boolean isGroup() {
        return !children.isEmpty();
    }
}