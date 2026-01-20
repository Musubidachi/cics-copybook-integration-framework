package com.mainframe.copybook.parser.pic;

/**
 * A PIC symbol repeated a fixed number of times, e.g. X(30) or 9(5).
 *
 * @param symbol the PIC symbol being repeated ('X' or '9')
 * @param count  the positive repeat count
 */
public record PicRepeat(char symbol, int count) implements PicElement {
    public PicRepeat {
        if (symbol != 'X' && symbol != '9') {
            throw new IllegalArgumentException("Unsupported PIC symbol: " + symbol);
        }
        if (count <= 0) {
            throw new IllegalArgumentException("Repeat count must be positive: " + count);
        }
    }
}