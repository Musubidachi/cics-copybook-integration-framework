package com.mainframe.copybook.parser.ast;

/**
 * Marker interface for nodes in the Copybook AST.  All node types must
 * implement this interface so that the AST can be traversed uniformly.
 */
public sealed interface AstNode permits DataItemNode, CopyNode, ConditionNameNode {
}