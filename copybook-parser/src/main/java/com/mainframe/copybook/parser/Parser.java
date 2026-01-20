package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.AstNode;
import com.mainframe.copybook.parser.ast.ConditionNameNode;
import com.mainframe.copybook.parser.ast.CopybookAst;
import com.mainframe.copybook.parser.ast.CopyNode;
import com.mainframe.copybook.parser.ast.DataItemNode;
import com.mainframe.copybook.parser.pic.PicClause;
import com.mainframe.copybook.parser.pic.PicElement;
import com.mainframe.copybook.parser.pic.PicRepeat;
import com.mainframe.copybook.parser.pic.PicSymbol;
import com.mainframe.copybook.parser.pic.PicVirtualDecimal;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * Top-level parser that consumes a flat token stream and produces a
 * CopybookAst.  The parser implements a straightforward recursive descent
 * grammar for data description entries and COPY statements, building a
 * hierarchical tree of DataItemNode builders which are materialized into
 * immutable DataItemNode values when parsing completes.  Errors encountered
 * during parsing are collected into diagnostics instead of throwing.
 */
public final class Parser {
    private final List<Token> tokens;
    private final ParserOptions options;
    private int pos;
    private final List<Diagnostic> diagnostics = new ArrayList<>();

    /**
     * Build a parser for the given token stream.  The list of tokens should
     * include an EOF sentinel at the end.
     */
    public Parser(List<Token> tokens) {
        this(tokens, ParserOptions.NO_EXPANSION);
    }

    /**
     * Build a parser for the given token stream with specified options.
     */
    public Parser(List<Token> tokens, ParserOptions options) {
        this.tokens = tokens;
        this.options = options != null ? options : ParserOptions.NO_EXPANSION;
        this.pos = 0;
    }

    /**
     * Parse the token stream into a CopybookAst.  The resulting AST roots and
     * diagnostics lists will never be null, though either may be empty.
     *
     * @return the parsed CopybookAst
     */
    public CopybookAst parse() {
        // Root order preserves the order of declarations; each entry is either
        // a NodeBuilder (for data entries) or a CopyNode for COPY statements.
        List<Object> rootOrder = new ArrayList<>();
        // Stack of group builders to manage hierarchy.  A builder remains on
        // the stack until a lower or equal level number is encountered.
        Deque<NodeBuilder> stack = new ArrayDeque<>();

        while (!check(TokenType.EOF)) {
            Token t = peek();
            if (t.type() == TokenType.KEYWORD && t.lexeme().equals("COPY")) {
                // COPY statements terminate any current group context.  Clear the
                // stack so subsequent data entries become new roots.
                stack.clear();
                rootOrder.add(parseCopyStatement());
            } else if (t.type() == TokenType.LEVEL_NUMBER) {
                int levelNum = Integer.parseInt(t.lexeme());

                if (levelNum == 88) {
                    // Level-88 condition name - attach to the most recent data item
                    ConditionNameNode condNode = parseConditionName();
                    if (condNode != null && !stack.isEmpty()) {
                        stack.peek().conditionNames().add(condNode);
                    } else if (condNode != null) {
                        // No parent data item - report error
                        error("PARSE_L88_NO_PARENT", "Level-88 condition name has no parent data item", t);
                    }
                } else if (levelNum == 66) {
                    // RENAMES - unsupported
                    error("UNSUPPORTED_FEATURE", "Unsupported feature: RENAMES (level 66)", t);
                    skipToNextEntry();
                } else if (levelNum == 77) {
                    // Level-77 standalone items - parse as regular data entry
                    NodeBuilder builder = parseDataEntry();
                    if (builder != null) {
                        rootOrder.add(builder);
                    }
                } else {
                    NodeBuilder builder = parseDataEntry();
                    if (builder != null) {
                        // Determine hierarchical placement
                        while (!stack.isEmpty() && stack.peek().level() >= builder.level()) {
                            stack.pop();
                        }
                        if (stack.isEmpty()) {
                            rootOrder.add(builder);
                        } else {
                            stack.peek().children().add(builder);
                        }
                        stack.push(builder);
                    }
                }
            } else if (t.type() == TokenType.PUNCT && t.lexeme().equals(".")) {
                // Stray period outside of an entry; skip and record error
                error("PARSE0", "Unexpected '.' outside of data entry", t);
                advance();
            } else {
                // Unexpected token; attempt to recover by consuming it
                error("PARSE1", "Unexpected token '" + t.lexeme() + "'", t);
                advance();
            }
        }
        // Convert root builders into immutable AST nodes
        List<AstNode> roots = new ArrayList<>();
        for (Object obj : rootOrder) {
            if (obj instanceof NodeBuilder nb) {
                roots.add(nb.toAstNode());
            } else if (obj instanceof CopyNode copy) {
                roots.add(copy);
            }
        }
        return new CopybookAst(roots, diagnostics);
    }

    /**
     * Parse a Level-88 condition name entry.
     */
    private ConditionNameNode parseConditionName() {
        Token levelTok = advance(); // consume 88

        // Expect identifier (condition name)
        Token nameTok;
        if (check(TokenType.IDENTIFIER)) {
            nameTok = advance();
        } else {
            error("PARSE_L88_NO_NAME", "Level-88 entry missing condition name", peek());
            skipToNextEntry();
            return null;
        }

        // Expect VALUE or VALUES keyword
        if (!check(TokenType.KEYWORD) || (!peek().lexeme().equals("VALUE") && !peek().lexeme().equals("VALUES"))) {
            error("PARSE_L88_NO_VALUE", "Level-88 entry missing VALUE clause", peek());
            skipToNextEntry();
            return null;
        }
        advance(); // consume VALUE/VALUES

        // Optional IS keyword
        if (check(TokenType.KEYWORD) && peek().lexeme().equals("IS")) {
            advance();
        }

        // Parse value specifications
        List<ConditionNameNode.ValueSpec> values = new ArrayList<>();
        while (!check(TokenType.EOF)) {
            Token valueTok = peek();
            if (valueTok.type() == TokenType.PUNCT && valueTok.lexeme().equals(".")) {
                break;
            }

            String value = null;
            if (check(TokenType.STRING_LITERAL)) {
                value = advance().lexeme();
            } else if (check(TokenType.INTEGER) || check(TokenType.NUMERIC_LITERAL)) {
                value = advance().lexeme();
            } else if (check(TokenType.IDENTIFIER)) {
                value = advance().lexeme();
            } else {
                break;
            }

            // Check for THRU/THROUGH
            String thruValue = null;
            if (check(TokenType.KEYWORD) && (peek().lexeme().equals("THRU") || peek().lexeme().equals("THROUGH"))) {
                advance(); // consume THRU/THROUGH
                if (check(TokenType.STRING_LITERAL)) {
                    thruValue = advance().lexeme();
                } else if (check(TokenType.INTEGER) || check(TokenType.NUMERIC_LITERAL)) {
                    thruValue = advance().lexeme();
                } else if (check(TokenType.IDENTIFIER)) {
                    thruValue = advance().lexeme();
                } else {
                    error("PARSE_L88_THRU_VALUE", "THRU missing end value", peek());
                }
            }

            if (thruValue != null) {
                values.add(ConditionNameNode.ValueSpec.range(value, thruValue));
            } else {
                values.add(ConditionNameNode.ValueSpec.single(value));
            }
        }

        // Expect terminating period
        Token endTok = peek();
        if (check(TokenType.PUNCT) && peek().lexeme().equals(".")) {
            endTok = advance();
        } else {
            error("PARSE_L88_NO_PERIOD", "Level-88 entry missing terminating '.'", peek());
        }

        SourceSpan span = createSpan(levelTok.line(), levelTok.column(), endTok.line(), endTok.column());
        return new ConditionNameNode(nameTok.lexeme(), values, span);
    }

    /**
     * Parse a COPY statement of the form: COPY <identifier> [REPLACING ...] .
     */
    private CopyNode parseCopyStatement() {
        Token copyTok = expect(TokenType.KEYWORD, "COPY");
        Token nameTok = null;
        if (check(TokenType.IDENTIFIER)) {
            nameTok = advance();
        } else {
            error("PARSE2", "COPY statement missing copybook name", peek());
        }

        // Check for REPLACING clause
        List<CopyNode.ReplacingPair> replacingPairs = new ArrayList<>();
        if (check(TokenType.KEYWORD) && peek().lexeme().equals("REPLACING")) {
            advance(); // consume REPLACING
            replacingPairs = parseReplacingPairs();
        }

        // Expect terminating period
        Token endTok = peek();
        if (!check(TokenType.PUNCT) || !peek().lexeme().equals(".")) {
            error("PARSE3", "COPY statement missing terminating '.'", peek());
        } else {
            endTok = advance();
        }

        String name = nameTok != null ? nameTok.lexeme() : "";
        SourceSpan span = createSpan(copyTok.line(), copyTok.column(), endTok.line(), endTok.column());
        return new CopyNode(name, replacingPairs, span);
    }

    /**
     * Parse REPLACING pairs: ==from== BY ==to== ... or 'from' BY 'to' ...
     * Supports both pseudo-text (==...==) and quote-delimited ('...' or "...") forms.
     */
    private List<CopyNode.ReplacingPair> parseReplacingPairs() {
        List<CopyNode.ReplacingPair> pairs = new ArrayList<>();

        while (!check(TokenType.EOF)) {
            Token tok = peek();
            if (tok.type() == TokenType.PUNCT && tok.lexeme().equals(".")) {
                break;
            }

            String fromText;
            String toText;

            // Check for quote-delimited form first (string literal)
            if (check(TokenType.STRING_LITERAL)) {
                // Quote-delimited form: 'from' BY 'to'
                fromText = advance().lexeme();

                // Expect BY keyword
                if (!check(TokenType.KEYWORD) || !peek().lexeme().equals("BY")) {
                    error("PARSE_REPLACING_NO_BY", "Invalid COPY REPLACING clause: expected BY after quoted text", peek());
                    break;
                }
                advance(); // consume BY

                // Expect quoted 'to' text
                if (!check(TokenType.STRING_LITERAL)) {
                    error("PARSE_REPLACING_NO_TO", "REPLACING clause missing quoted 'to' text", peek());
                    break;
                }
                toText = advance().lexeme();
            }
            // Check for pseudo-text delimiter form (==...==)
            else if (check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                advance(); // consume opening ==

                // Read the 'from' text (everything until next ==)
                StringBuilder fromBuilder = new StringBuilder();
                while (!check(TokenType.EOF) && !check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                    Token t = advance();
                    if (fromBuilder.length() > 0) {
                        fromBuilder.append(" ");
                    }
                    fromBuilder.append(t.lexeme());
                }

                if (!check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                    error("PARSE_REPLACING_NO_CLOSE", "REPLACING clause missing closing '=='", peek());
                    break;
                }
                advance(); // consume closing ==
                fromText = fromBuilder.toString();

                // Expect BY keyword
                if (!check(TokenType.KEYWORD) || !peek().lexeme().equals("BY")) {
                    error("PARSE_REPLACING_NO_BY", "Invalid COPY REPLACING clause: expected pairs of FROM/BY replacements", peek());
                    break;
                }
                advance(); // consume BY

                // Expect ==to== or 'to' (allow mixed forms)
                if (check(TokenType.STRING_LITERAL)) {
                    toText = advance().lexeme();
                } else if (check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                    advance(); // consume opening ==

                    // Read the 'to' text (everything until next ==)
                    StringBuilder toBuilder = new StringBuilder();
                    while (!check(TokenType.EOF) && !check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                        Token t = advance();
                        if (toBuilder.length() > 0) {
                            toBuilder.append(" ");
                        }
                        toBuilder.append(t.lexeme());
                    }

                    if (!check(TokenType.PSEUDO_TEXT_DELIMITER)) {
                        error("PARSE_REPLACING_NO_CLOSE2", "REPLACING clause missing closing '=='", peek());
                        break;
                    }
                    advance(); // consume closing ==
                    toText = toBuilder.toString();
                } else {
                    error("PARSE_REPLACING_NO_TO", "REPLACING clause missing 'to' pseudo-text or quoted text", peek());
                    break;
                }
            } else {
                // Neither pseudo-text nor string literal - end of REPLACING clause
                break;
            }

            try {
                pairs.add(new CopyNode.ReplacingPair(fromText, toText));
            } catch (IllegalArgumentException e) {
                error("PARSE_REPLACING_INVALID", e.getMessage(), tok);
            }
        }

        return pairs;
    }

    /**
     * Parse a data description entry beginning with a level number.  Returns a
     * NodeBuilder or null if a fatal error prevented parsing this entry.
     */
    private NodeBuilder parseDataEntry() {
        // LEVEL_NUMBER
        Token levelTok = advance();
        int level = 0;
        try {
            level = Integer.parseInt(levelTok.lexeme());
        } catch (NumberFormatException ex) {
            error("PARSE4", "Invalid level number", levelTok);
        }
        // Validate level range
        if (level != 77 && (level < 1 || level > 49)) {
            error("PARSE5", "Unsupported level number: " + levelTok.lexeme(), levelTok);
        }
        // IDENTIFIER
        Token nameTok;
        if (check(TokenType.IDENTIFIER)) {
            nameTok = advance();
        } else {
            error("PARSE6", "Missing data name after level number", peek());
            // attempt to continue but use empty name
            nameTok = new Token(TokenType.IDENTIFIER, "", levelTok.line(), levelTok.column());
        }
        String name = nameTok.lexeme();
        PicClause pic = null;
        Usage usage = null;
        OccursClause occurs = null;
        RedefinesClause redefines = null;
        // Parse optional clauses in any order until we hit a period
        clauseLoop:
        while (!check(TokenType.EOF)) {
            Token tok = peek();
            if (tok.type() == TokenType.PUNCT && tok.lexeme().equals(".")) {
                break;
            }
            if (tok.type() == TokenType.KEYWORD) {
                String keyword = tok.lexeme();
                switch (keyword) {
                    case "REDEFINES" -> {
                        advance();
                        if (redefines != null) {
                            error("PARSE7", "Duplicate REDEFINES clause", tok);
                        }
                        if (check(TokenType.IDENTIFIER)) {
                            Token target = advance();
                            redefines = new RedefinesClause(target.lexeme());
                        } else {
                            error("PARSE8", "REDEFINES missing target name", peek());
                        }
                    }
                    case "PIC", "PICTURE" -> {
                        advance();
                        // Optional IS keyword
                        if (check(TokenType.KEYWORD) && peek().lexeme().equals("IS")) {
                            advance();
                        }
                        if (pic != null) {
                            error("PARSE9", "Duplicate PIC clause", tok);
                        }
                        pic = parsePicClause();
                    }
                    case "USAGE" -> {
                        advance();
                        // Optional IS keyword
                        if (check(TokenType.KEYWORD) && peek().lexeme().equals("IS")) {
                            advance();
                        }
                        if (usage != null) {
                            error("PARSE10", "Duplicate USAGE clause", tok);
                        }
                        if (check(TokenType.IDENTIFIER) || check(TokenType.KEYWORD)) {
                            Token u = advance();
                            try {
                                usage = Usage.fromLexeme(u.lexeme());
                            } catch (IllegalArgumentException ex) {
                                error("UNSUPPORTED_FEATURE", "Unsupported USAGE: " + u.lexeme(), u);
                            }
                        } else {
                            error("PARSE12", "USAGE missing value", peek());
                        }
                    }
                    case "COMP-3" -> {
                        // COMP-3 can appear without USAGE keyword
                        advance();
                        if (usage != null) {
                            error("PARSE10", "Duplicate USAGE clause", tok);
                        }
                        usage = Usage.COMP_3;
                    }
                    case "OCCURS" -> {
                        advance();
                        if (occurs != null) {
                            error("PARSE13", "Duplicate OCCURS clause", tok);
                        }
                        occurs = parseOccursClause(tok);
                    }
                    case "RENAMES" -> {
                        error("UNSUPPORTED_FEATURE", "Unsupported feature: RENAMES", tok);
                        skipToNextEntry();
                        return null;
                    }
                    default -> {
                        // Unknown keyword inside entry - might be end of entry
                        break clauseLoop;
                    }
                }
            } else if (tok.type() == TokenType.IDENTIFIER) {
                // Could be an implicit USAGE value like COMP or COMP-3
                String upper = tok.lexeme().toUpperCase();
                if (upper.equals("COMP") || upper.equals("COMP-3") || upper.equals("DISPLAY")) {
                    advance();
                    if (usage != null) {
                        error("PARSE10", "Duplicate USAGE clause", tok);
                    }
                    try {
                        usage = Usage.fromLexeme(tok.lexeme());
                    } catch (IllegalArgumentException ex) {
                        error("PARSE11", ex.getMessage(), tok);
                    }
                } else {
                    // Unexpected identifier in clause position
                    error("PARSE18", "Unexpected token '" + tok.lexeme() + "' in clause list", tok);
                    advance();
                }
            } else {
                // Unexpected token in clause position
                error("PARSE18", "Unexpected token '" + tok.lexeme() + "' in clause list", tok);
                advance();
            }
        }
        // Expect terminating period
        if (check(TokenType.PUNCT) && peek().lexeme().equals(".")) {
            Token dot = advance();
            // Build source span from level token to period
            SourceSpan span = createSpan(levelTok.line(), levelTok.column(), dot.line(), dot.column());
            return new NodeBuilder(level, name, pic, usage, occurs, redefines, span);
        } else {
            error("PARSE19", "Data entry missing terminating '.'", peek());
            return new NodeBuilder(level, name, pic, usage, occurs, redefines, createSpan(levelTok.line(), levelTok.column(), levelTok.line(), levelTok.column()));
        }
    }

    /**
     * Parse an OCCURS clause, detecting unsupported DEPENDING ON.
     */
    private OccursClause parseOccursClause(Token occursTok) {
        // Check for variable-length array syntax: OCCURS n TO m TIMES DEPENDING ON
        if (check(TokenType.INTEGER)) {
            Token firstNum = advance();

            // Check for DEPENDING ON (variable length - unsupported)
            if (check(TokenType.KEYWORD) && peek().lexeme().equals("TO")) {
                advance(); // consume TO
                if (check(TokenType.INTEGER)) {
                    advance(); // consume max count
                }
                // Skip optional TIMES
                if (check(TokenType.KEYWORD) && peek().lexeme().equals("TIMES")) {
                    advance();
                }
                // Check for DEPENDING ON
                if (check(TokenType.KEYWORD) && peek().lexeme().equals("DEPENDING")) {
                    error("UNSUPPORTED_FEATURE", "Unsupported feature: OCCURS DEPENDING ON (v1)", occursTok);
                    // Skip to end of clause
                    while (!check(TokenType.EOF) && !check(TokenType.PUNCT)) {
                        advance();
                    }
                    return null;
                }
            }

            // Normal fixed OCCURS
            try {
                int count = Integer.parseInt(firstNum.lexeme());
                // optional TIMES keyword
                if (check(TokenType.KEYWORD) && peek().lexeme().equals("TIMES")) {
                    advance();
                }
                return new OccursClause(count);
            } catch (NumberFormatException ex) {
                error("PARSE14", "Invalid OCCURS count", firstNum);
                return null;
            } catch (IllegalArgumentException ex) {
                error("PARSE15", ex.getMessage(), firstNum);
                return null;
            }
        } else {
            error("PARSE16", "OCCURS missing integer count", peek());
            return null;
        }
    }

    /**
     * Parse a PIC clause.  This method assumes the 'PIC' or 'PICTURE'
     * keyword has already been consumed.  It consumes tokens up to but not
     * including the next keyword or '.', building a structured PicClause.
     */
    private PicClause parsePicClause() {
        boolean signed = false;
        List<PicElement> elements = new ArrayList<>();
        while (!check(TokenType.EOF)) {
            Token tok = peek();
            // Stop at next clause keyword or period
            if (tok.type() == TokenType.KEYWORD || (tok.type() == TokenType.PUNCT && tok.lexeme().equals("."))) {
                break;
            }
            // Stop at identifier that could be a USAGE value or next clause
            if (tok.type() == TokenType.IDENTIFIER) {
                String upper = tok.lexeme().toUpperCase();
                if (upper.equals("COMP") || upper.equals("COMP-3") || upper.equals("DISPLAY")) {
                    break;
                }
            }
            // Sign indicator
            if (tok.type() == TokenType.PIC_SYMBOL && tok.lexeme().equals("S") && elements.isEmpty() && !signed) {
                signed = true;
                advance();
                continue;
            }
            if (tok.type() == TokenType.PIC_SYMBOL) {
                char symbol = tok.lexeme().charAt(0);
                advance();
                // Check for repeat, e.g. X(30)
                if (check(TokenType.PUNCT) && peek().lexeme().equals("(")) {
                    advance(); // consume '('
                    if (check(TokenType.INTEGER)) {
                        Token countTok = advance();
                        int count;
                        try {
                            count = Integer.parseInt(countTok.lexeme());
                            elements.add(new PicRepeat(symbol, count));
                        } catch (NumberFormatException ex) {
                            error("PARSE20", "Invalid PIC repeat count", countTok);
                        } catch (IllegalArgumentException ex) {
                            error("PARSE21", ex.getMessage(), countTok);
                        }
                    } else {
                        error("PARSE22", "PIC repeat missing integer", peek());
                    }
                    // Expect ')'
                    if (check(TokenType.PUNCT) && peek().lexeme().equals(")")) {
                        advance();
                    } else {
                        error("PARSE23", "PIC repeat missing closing ')'", peek());
                    }
                } else {
                    // Single symbol
                    try {
                        elements.add(new PicSymbol(symbol));
                    } catch (IllegalArgumentException ex) {
                        error("PARSE24", ex.getMessage(), tok);
                    }
                }
                continue;
            }
            // Virtual decimal (V)
            if (tok.type() == TokenType.PUNCT && (tok.lexeme().equals("V") || tok.lexeme().equals("v"))) {
                elements.add(new PicVirtualDecimal());
                advance();
                continue;
            }
            // Handle inline digits after V (e.g., V99)
            if (tok.type() == TokenType.INTEGER && !elements.isEmpty() &&
                elements.get(elements.size() - 1) instanceof PicVirtualDecimal) {
                // The digits represent repeated 9s
                String digits = tok.lexeme();
                advance();
                // Each digit character represents a '9'
                for (int i = 0; i < digits.length(); i++) {
                    try {
                        elements.add(new PicSymbol('9'));
                    } catch (IllegalArgumentException ex) {
                        error("PARSE24", ex.getMessage(), tok);
                    }
                }
                continue;
            }
            // Stop on unexpected token
            break;
        }
        if (elements.isEmpty()) {
            error("PARSE26", "Empty PIC clause", peek());
            // Insert a placeholder symbol to allow AST construction
            elements.add(new PicSymbol('X'));
        }
        return new PicClause(signed, elements);
    }

    /**
     * Skip tokens until the next entry (level number or EOF).
     */
    private void skipToNextEntry() {
        while (!check(TokenType.EOF)) {
            if (check(TokenType.PUNCT) && peek().lexeme().equals(".")) {
                advance();
                break;
            }
            if (check(TokenType.LEVEL_NUMBER)) {
                break;
            }
            advance();
        }
    }

    /**
     * Check whether the current token has the given type.
     */
    private boolean check(TokenType type) {
        return peek().type() == type;
    }

    /**
     * Return the current token without consuming it.
     */
    private Token peek() {
        if (pos < tokens.size()) {
            return tokens.get(pos);
        }
        return tokens.get(tokens.size() - 1);
    }

    /**
     * Consume and return the current token.
     */
    private Token advance() {
        if (pos < tokens.size()) {
            return tokens.get(pos++);
        }
        return tokens.get(tokens.size() - 1);
    }

    /**
     * Consume the current token if it matches the expected type and optional
     * lexeme.  Otherwise emit a diagnostic and return the current token.
     */
    private Token expect(TokenType type, String lexeme) {
        Token tok = peek();
        if (tok.type() == type && (lexeme == null || tok.lexeme().equals(lexeme))) {
            return advance();
        }
        error("PARSEEXPECT", "Expected " + type + (lexeme != null ? "('" + lexeme + "')" : "") + " but found '" + tok.lexeme() + "'", tok);
        return advance();
    }

    /**
     * Add a diagnostic with the given code and message associated with the
     * provided token.
     */
    private void error(String code, String message, Token token) {
        SourceSpan span = options.trackSourcePositions()
                ? SourceSpan.single(token.line(), token.column())
                : null;
        diagnostics.add(new Diagnostic("ParseError", code, message, span));
    }

    /**
     * Create a SourceSpan respecting the trackSourcePositions option.
     */
    private SourceSpan createSpan(int startLine, int startCol, int endLine, int endCol) {
        if (!options.trackSourcePositions()) {
            return null;
        }
        return new SourceSpan(startLine, startCol, endLine, endCol);
    }

    /**
     * Mutable builder for data items.  Builders collect children and condition
     * names and are converted to immutable DataItemNode values when parsing
     * is complete.
     */
    private static final class NodeBuilder {
        private final int level;
        private final String name;
        private final PicClause pic;
        private final Usage usage;
        private final OccursClause occurs;
        private final RedefinesClause redefines;
        private final SourceSpan span;
        private final List<NodeBuilder> children = new ArrayList<>();
        private final List<ConditionNameNode> conditionNames = new ArrayList<>();

        NodeBuilder(int level, String name, PicClause pic, Usage usage, OccursClause occurs, RedefinesClause redefines, SourceSpan span) {
            this.level = level;
            this.name = name;
            this.pic = pic;
            this.usage = usage;
            this.occurs = occurs;
            this.redefines = redefines;
            this.span = span;
        }
        int level() { return level; }
        List<NodeBuilder> children() { return children; }
        List<ConditionNameNode> conditionNames() { return conditionNames; }
        DataItemNode toAstNode() {
            List<DataItemNode> childAsts = new ArrayList<>();
            for (NodeBuilder child : children) {
                childAsts.add(child.toAstNode());
            }
            return new DataItemNode(level, name, pic, usage, occurs, redefines, childAsts, conditionNames, span);
        }
    }
}
