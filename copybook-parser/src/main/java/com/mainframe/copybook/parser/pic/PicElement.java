package com.mainframe.copybook.parser.pic;

/**
 * Marker interface for elements that comprise a PIC clause.  A PIC clause
 * consists of a sequence of one or more PicElement values.  Elements are
 * sealed to ensure exhaustive pattern matching when interpreting a PicClause.
 */
public sealed interface PicElement permits PicSymbol, PicRepeat, PicVirtualDecimal, PicSign {
}