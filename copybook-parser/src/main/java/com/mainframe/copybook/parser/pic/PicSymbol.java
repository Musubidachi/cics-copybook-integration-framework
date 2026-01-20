package com.mainframe.copybook.parser.pic;

/**
 * A single PIC symbol with implicit repeat of one.  Supported symbols are
 * 'X' (alphanumeric) and '9' (numeric).  A standalone 'S' is not emitted
 * directly but represented as a PicSign when appearing at the beginning of a
 * PIC clause.
 *
 * @param symbol the PIC symbol character
 */
public record PicSymbol(char symbol) implements PicElement {
    public PicSymbol {
        if (symbol != 'X' && symbol != '9') {
            throw new IllegalArgumentException("Unsupported PIC symbol: " + symbol);
        }
    }
}