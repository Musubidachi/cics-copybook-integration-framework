package com.mainframe.model;

import java.util.List;

/**
 * Root of the copybook abstract syntax tree.  Consists of one or
 * more top‑level data items.  Level 01 items should be the only
 * permitted top‑level nodes; however this class does not enforce
 * level restrictions to allow for flexibility during parsing.
 */
public record CopybookAst(List<DataItemNode> topLevelItems) {
    public CopybookAst {
        topLevelItems = List.copyOf(topLevelItems);
    }
}