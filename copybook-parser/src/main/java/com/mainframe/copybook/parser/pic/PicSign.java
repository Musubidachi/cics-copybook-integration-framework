package com.mainframe.copybook.parser.pic;

/**
 * Represents the optional 'S' sign indicator at the beginning of a PIC
 * specification.  When present, the PIC clause is considered signed.  This
 * element is emitted separately from the symbol list to simplify parsing.
 */
public record PicSign() implements PicElement {
}