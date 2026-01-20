package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.AstNode;
import com.mainframe.copybook.parser.ast.CopyNode;
import com.mainframe.copybook.parser.ast.DataItemNode;
import com.mainframe.copybook.parser.pic.PicClause;
import com.mainframe.copybook.parser.pic.PicElement;
import com.mainframe.copybook.parser.pic.PicRepeat;
import com.mainframe.copybook.parser.pic.PicSymbol;
import com.mainframe.copybook.parser.pic.PicVirtualDecimal;
import com.mainframe.copybook.parser.pic.PicSign;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

/**
 * Top‑level parser that consumes a flat token stream and produces a
 * CopybookAst.  The parser implements a straightforward recursive descent
 * grammar for data description entries and COPY statements, building a
 * hierarchical tree of DataItemNode builders which are materialized into
 * immutable DataItemNode values when parsing completes.  Errors encountered
 * during parsing are collected into diagnostics instead of throwing.
 */
public final class Parser {
    private final List<Token> tokens;
    private int pos;
    private final List<Diagnostic> diagnostics = new ArrayList<>();

    /**
     * Build a parser for the given token stream.  The list of tokens should
     * include an EOF sentinel at the end.
     */
    public Parser(List<Token> tokens) {
        this.tokens = tokens;
        this.pos = 0;
    }

    /**
     * Parse the token stream into a CopybookAst.  The resulting AST roots and
     * diagnostics lists will never be null, though either may be empty.
     *
     * @return the parsed Copybook AST
     */
    public AstResult parse() {
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
        return new AstResult(new DataAst(roots), diagnostics);
    }

    /**
     * Parse a COPY statement of the form: COPY <identifier> .
     */
    private CopyNode parseCopyStatement() {
        Token copyTok = expect(TokenType.KEYWORD, "COPY");
        Token nameTok = null;
        if (check(TokenType.IDENTIFIER)) {
            nameTok = advance();
        } else {
            error("PARSE2", "COPY statement missing copybook name", peek());
        }
        // Expect terminating period
        if (!check(TokenType.PUNCT) || !peek().lexeme().equals(".")) {
            error("PARSE3", "COPY statement missing terminating '.'", peek());
        } else {
            advance();
        }
        String name = nameTok != null ? nameTok.lexeme() : "";
        // The span covers from the COPY keyword to the dot; approximate end
        SourceSpan span = new SourceSpan(copyTok.line(), copyTok.column(), copyTok.line(), copyTok.column());
        return new CopyNode(name, span);
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
        if (level < 1 || level > 49 || level == 66 || level == 77 || level == 88) {
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
                        if (pic != null) {
                            error("PARSE9", "Duplicate PIC clause", tok);
                        }
                        pic = parsePicClause();
                    }
                    case "USAGE" -> {
                        advance();
                        if (usage != null) {
                            error("PARSE10", "Duplicate USAGE clause", tok);
                        }
                        if (check(TokenType.IDENTIFIER) || check(TokenType.KEYWORD)) {
                            Token u = advance();
                            try {
                                usage = Usage.fromLexeme(u.lexeme());
                            } catch (IllegalArgumentException ex) {
                                error("PARSE11", ex.getMessage(), u);
                            }
                        } else {
                            error("PARSE12", "USAGE missing value", peek());
                        }
                    }
                    case "OCCURS" -> {
                        advance();
                        if (occurs != null) {
                            error("PARSE13", "Duplicate OCCURS clause", tok);
                        }
                        if (check(TokenType.INTEGER)) {
                            Token countTok = advance();
                            try {
                                int count = Integer.parseInt(countTok.lexeme());
                                occurs = new OccursClause(count);
                            } catch (NumberFormatException ex) {
                                error("PARSE14", "Invalid OCCURS count", countTok);
                            } catch (IllegalArgumentException ex) {
                                error("PARSE15", ex.getMessage(), countTok);
                            }
                            // optional TIMES keyword
                            if (check(TokenType.KEYWORD) && peek().lexeme().equals("TIMES")) {
                                advance();
                            }
                        } else {
                            error("PARSE16", "OCCURS missing integer count", peek());
                        }
                    }
                    default -> {
                        // Unknown keyword inside entry
                        error("PARSE17", "Unexpected keyword '" + keyword + "'", tok);
                        advance();
                    }
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
            SourceSpan span = new SourceSpan(levelTok.line(), levelTok.column(), dot.line(), dot.column());
            return new NodeBuilder(level, name, pic, usage, occurs, redefines, span);
        } else {
            error("PARSE19", "Data entry missing terminating '.'", peek());
            return new NodeBuilder(level, name, pic, usage, occurs, redefines, new SourceSpan(levelTok.line(), levelTok.column(), levelTok.line(), levelTok.column()));
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
        boolean expectElement = true;
        while (!check(TokenType.EOF)) {
            Token tok = peek();
            // Stop at next clause keyword or period
            if (tok.type() == TokenType.KEYWORD || (tok.type() == TokenType.PUNCT && tok.lexeme().equals("."))) {
                break;
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
            // Unexpected token in PIC
            error("PARSE25", "Unexpected token in PIC clause: '" + tok.lexeme() + "'", tok);
            advance();
        }
        if (elements.isEmpty()) {
            error("PARSE26", "Empty PIC clause", peek());
            // Insert a placeholder symbol to allow AST construction
            elements.add(new PicSymbol('X'));
        }
        return new PicClause(signed, elements);
    }

    /**
     * Check whether the current token has the given type (and optional lexeme).
     */
    private boolean check(TokenType type) {
        return peek().type() == type;
    }
    private boolean check(TokenType type, String lexeme) {
        Token tok = peek();
        return tok.type() == type && tok.lexeme().equals(lexeme);
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
    private Token expect(TokenType type) {
        return expect(type, null);
    }

    /**
     * Add a diagnostic with the given code and message associated with the
     * provided token.
     */
    private void error(String code, String message, Token token) {
        diagnostics.add(new Diagnostic("ParseError", code, message, SourceSpan.single(token.line(), token.column())));
    }

    /**
     * Container type used internally to hold AST results.  Separates the root
     * nodes from diagnostics so the parser can return both together.
     */
    public record AstResult(DataAst ast, List<Diagnostic> diagnostics) {}

    /**
     * DataAst wraps the root nodes; defined as a separate class to permit
     * additional metadata if needed in future iterations.
     */
    public record DataAst(List<AstNode> roots) {}

    /**
     * Mutable builder for data items.  Builders collect children and are
     * converted to immutable DataItemNode values when parsing is complete.
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
        DataItemNode toAstNode() {
            List<DataItemNode> childAsts = new ArrayList<>();
            for (NodeBuilder child : children) {
                childAsts.add(child.toAstNode());
            }
            return new DataItemNode(level, name, pic, usage, occurs, redefines, childAsts, span);
        }
    }
}