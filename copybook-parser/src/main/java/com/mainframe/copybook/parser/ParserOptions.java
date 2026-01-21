package com.mainframe.copybook.parser;

/**
 * Configuration options for the copybook parser.
 */
public record ParserOptions(
        boolean expandCopy,
        boolean strictMode,
        boolean trackSourcePositions
) {
    /**
     * Default options: COPY expansion enabled, strict mode, source positions tracked.
     */
    public static final ParserOptions DEFAULT = new ParserOptions(true, true, true);

    /**
     * Options for parsing without COPY expansion.
     */
    public static final ParserOptions NO_EXPANSION = new ParserOptions(false, true, true);
    
    public static final ParserOptions NO_EXPANSION_LENIENT = new ParserOptions(false, false, true);

    /**
     * Options for permissive parsing without COPY expansion.
     */
    public static final ParserOptions PERMISSIVE = new ParserOptions(false, false, true);

    /**
     * Builder for ParserOptions.
     */
    public static class Builder {
        private boolean expandCopy = false;
        private boolean strictMode = true;
        private boolean trackSourcePositions = true;

        public Builder expandCopy(boolean expand) {
            this.expandCopy = expand;
            return this;
        }

        public Builder strictMode(boolean strict) {
            this.strictMode = strict;
            return this;
        }

        public Builder trackSourcePositions(boolean track) {
            this.trackSourcePositions = track;
            return this;
        }

        public ParserOptions build() {
            return new ParserOptions(expandCopy, strictMode, trackSourcePositions);
        }
    }

    public static Builder builder() {
        return new Builder();
    }
}
