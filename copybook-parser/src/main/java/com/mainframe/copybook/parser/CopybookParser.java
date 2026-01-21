package com.mainframe.copybook.parser;

import com.mainframe.copybook.parser.ast.AstNode;
import com.mainframe.copybook.parser.ast.ConditionNameNode;
import com.mainframe.copybook.parser.ast.CopybookAst;
import com.mainframe.copybook.parser.ast.CopyNode;
import com.mainframe.copybook.parser.ast.DataItemNode;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * Public API for parsing COBOL copybooks. This is the main entry point for the
 * copybook-parser module.
 */
public final class CopybookParser {

	private CopybookParser() {
		// Utility class
	}

	/**
	 * Parse a copybook from a Reader.
	 *
	 * @param copybookName the name of the copybook (for diagnostics)
	 * @param source       the source reader
	 * @param resolver     resolver for COPY statements
	 * @param options      parser options
	 * @return the parsed AST
	 * @throws IOException          if reading fails
	 * @throws ParseFailedException if strictMode is enabled and errors occur
	 */
	public static CopybookAst parse(String copybookName, Reader source, CopybookResolver resolver,
			ParserOptions options) throws IOException {
		// Read source content
		String content = readAll(source);
		return parseString(copybookName, content, resolver, options);
	}

	/**
	 * Parse a copybook from a string.
	 *
	 * @param copybookName the name of the copybook (for diagnostics)
	 * @param source       the source text
	 * @param resolver     resolver for COPY statements
	 * @param options      parser options
	 * @return the parsed AST
	 * @throws ParseFailedException if strictMode is enabled and errors occur
	 */
	public static CopybookAst parseString(String copybookName, String source, CopybookResolver resolver,
			ParserOptions options) {
		// Phase 1: Normalize
		List<NormalizedLine> lines = Normalizer.normalize(source);

		// Phase 2: Tokenize
		Tokenizer tokenizer = new Tokenizer();
		List<Token> tokens = tokenizer.tokenize(lines);
		List<Diagnostic> diagnostics = new ArrayList<>(tokenizer.diagnostics());

		// Phase 3: Parse
		Parser parser = new Parser(tokens, options);
		CopybookAst ast = parser.parse();
		diagnostics.addAll(ast.diagnostics());

		// Check for errors in strict mode after parsing
		if (options.strictMode()) {
			List<Diagnostic> errors = getErrors(diagnostics);
			if (!errors.isEmpty()) {
				throw new ParseFailedException(errors);
			}
		}

		// Phase 4: COPY expansion (if enabled)
		List<AstNode> roots = new ArrayList<>(ast.roots());
		if (options.expandCopy() && resolver != null) {
			Set<String> visited = new HashSet<>();
			visited.add(copybookName);
			roots = expandCopyStatements(roots, resolver, options, diagnostics, visited);
		}

		// Check for errors in strict mode after expansion
		if (options.strictMode()) {
			List<Diagnostic> errors = getErrors(diagnostics);
			if (!errors.isEmpty()) {
				throw new ParseFailedException(errors);
			}
		}

		return new CopybookAst(roots, diagnostics);
	}

	/**
	 * Parse a copybook from a string with default options and no COPY resolution.
	 *
	 * @param source the source text
	 * @return the parsed AST
	 */
	public static CopybookAst parseString(String source) {
		return parseString("unnamed", source, CopybookResolver.NONE, ParserOptions.NO_EXPANSION_LENIENT);
	}

	/**
	 * Get error diagnostics from the list.
	 */
	private static List<Diagnostic> getErrors(List<Diagnostic> diagnostics) {
		return diagnostics.stream().filter(CopybookParser::isError).toList();
	}

	/**
	 * Check if a diagnostic is an error.
	 */
	private static boolean isError(Diagnostic d) {
		String category = d.category().toLowerCase();
		if (!category.contains("error"))
			return false;

		// Don't consider UNSUPPORTED_FEATURE fatal (report it, but don't throw)
		return !d.code().equalsIgnoreCase("UNSUPPORTED_FEATURE");
	}

	/**
	 * Expand COPY statements in the AST recursively. This handles both root-level
	 * and nested COPY statements within DataItemNode children.
	 */
	/**
	 * Expand COPY statements in the AST recursively. Handles both root-level and
	 * nested COPY statements within DataItemNode children.
	 */
	private static List<AstNode> expandCopyStatements(List<AstNode> roots, CopybookResolver resolver,
			ParserOptions options, List<Diagnostic> diagnostics, Set<String> visited) {
		if (roots == null || roots.isEmpty()) {
			return List.of();
		}

		List<AstNode> expanded = new ArrayList<>();

		for (AstNode node : roots) {
			if (node instanceof CopyNode copyNode) {
				// splice at root level
				expanded.addAll(expandCopyNode(copyNode, resolver, options, diagnostics, visited));
			} else if (node instanceof DataItemNode dataItem) {
				expanded.add(expandDataItemNode(dataItem, resolver, options, diagnostics, visited));
			} else {
				expanded.add(node);
			}
		}

		return expanded;
	}

	/**
	 * Expand a single CopyNode.
	 */
	private static List<AstNode> expandCopyNode(CopyNode copyNode, CopybookResolver resolver, ParserOptions options,
			List<Diagnostic> diagnostics, Set<String> visited) {
		String name = copyNode.copybookName();

		// Check for circular inclusion
		if (visited.contains(name)) {
			diagnostics.add(
					new Diagnostic("ParseError", "COPY_CIRCULAR", "Circular COPY detected: " + name, copyNode.span()));
			return List.of();
		}

		// Resolve the copybook
		Optional<Reader> resolved = resolver.resolve(name);
		if (resolved.isEmpty()) {
			diagnostics.add(
					new Diagnostic("ParseError", "COPYBOOK_NOT_FOUND", "Copybook not found: " + name, copyNode.span()));
			// Keep the CopyNode in the AST when not expanded
			return List.of(copyNode);
		}

		try {
			String content = readAll(resolved.get());

			// Apply REPLACING if present using token-sequence matching
			if (!copyNode.replacingPairs().isEmpty()) {
				content = applyReplacing(content, copyNode.replacingPairs());
			}

			// Parse the included copybook
			visited.add(name);
			CopybookAst includedAst = parseString(name, content, resolver,
					ParserOptions.builder().expandCopy(options.expandCopy()).strictMode(false) // Don't throw from
																								// nested parse
							.trackSourcePositions(options.trackSourcePositions()).build());
			visited.remove(name);

			diagnostics.addAll(includedAst.diagnostics());

			// Return expanded nodes (they are already recursively expanded)
			return new ArrayList<>(includedAst.roots());

		} catch (IOException e) {
			diagnostics.add(new Diagnostic("ParseError", "COPY_READ_ERROR",
					"Error reading copybook " + name + ": " + e.getMessage(), copyNode.span()));
			return List.of(copyNode);
		}
	}

	/**
	 * Recursively expand COPY statements within a DataItemNode's children. Creates
	 * a new DataItemNode with expanded children, preserving all other attributes.
	 */
	private static DataItemNode expandDataItemNode(DataItemNode dataItem, CopybookResolver resolver,
			ParserOptions options, List<Diagnostic> diagnostics, Set<String> visited) {
		List<AstNode> children = dataItem.children();
		if (children.isEmpty()) {
			return dataItem;
		}

		boolean changed = false;
		List<AstNode> expandedChildren = new ArrayList<>(children.size());

		for (AstNode child : children) {
			if (child instanceof CopyNode copyChild) {
				// splice: replace COPY node with its expanded nodes (preserve ordering)
				List<AstNode> repl = expandCopyNode(copyChild, resolver, options, diagnostics, visited);
				expandedChildren.addAll(repl);
				changed = true;
			} else if (child instanceof DataItemNode childDataItem) {
				// recurse into nested group/field
				DataItemNode expandedChild = expandDataItemNode(childDataItem, resolver, options, diagnostics, visited);
				expandedChildren.add(expandedChild);
				if (expandedChild != childDataItem) {
					changed = true;
				}
			} else {
				// any other AstNode type is preserved
				expandedChildren.add(child);
			}
		}

		if (!changed) {
			return dataItem;
		}

		// rebuild DataItemNode with expanded children, preserving all attributes
		return new DataItemNode(dataItem.level(), dataItem.name(), dataItem.pic(), dataItem.usage(), dataItem.occurs(),
				dataItem.redefines(), expandedChildren, dataItem.conditionNames(), dataItem.span());
	}

	/**
	 * Apply REPLACING transformations to copybook content using token-sequence
	 * matching. This replaces multi-token sequences, not just single tokens.
	 */
	private static String applyReplacing(String content, List<CopyNode.ReplacingPair> pairs) {
		// Normalize and tokenize the content
		List<NormalizedLine> lines = Normalizer.normalize(content);
		Tokenizer tokenizer = new Tokenizer();
		List<Token> tokens = tokenizer.tokenize(lines);

		// Convert ReplacingPairs to token sequences
		List<TokenSequencePair> tokenPairs = new ArrayList<>();
		for (CopyNode.ReplacingPair pair : pairs) {
			List<String> fromTokens = tokenizeReplacingText(pair.from());
			List<String> toTokens = tokenizeReplacingText(pair.to());
			tokenPairs.add(new TokenSequencePair(fromTokens, toTokens));
		}

		// Apply token-sequence replacement
		List<Token> resultTokens = applyTokenSequenceReplacing(tokens, tokenPairs);

		// Reconstruct the source text
		return reconstructSource(resultTokens);
	}

	/**
	 * Tokenize replacement text (from pseudo-text or quoted text). Returns a list
	 * of token lexemes.
	 */
	private static List<String> tokenizeReplacingText(String text) {
		if (text == null || text.isEmpty()) {
			return List.of();
		}

		// Normalize and tokenize
		List<NormalizedLine> lines = Normalizer.normalize("       " + text);
		Tokenizer tokenizer = new Tokenizer();
		List<Token> tokens = tokenizer.tokenize(lines);

		// Extract lexemes, excluding EOF
		List<String> lexemes = new ArrayList<>();
		for (Token token : tokens) {
			if (token.type() != TokenType.EOF) {
				lexemes.add(token.lexeme());
			}
		}
		return lexemes;
	}

	/**
	 * Apply token-sequence replacement to a token list.
	 */
	private static List<Token> applyTokenSequenceReplacing(List<Token> tokens, List<TokenSequencePair> pairs) {
		List<Token> result = new ArrayList<>(tokens);

		// Apply each pair in order
		for (TokenSequencePair pair : pairs) {
			result = applyOneTokenSequence(result, pair);
		}

		return result;
	}

	/**
	 * Apply a single token-sequence replacement pair.
	 */
	private static List<Token> applyOneTokenSequence(List<Token> tokens, TokenSequencePair pair) {
		if (pair.from().isEmpty()) {
			return tokens;
		}

		List<Token> result = new ArrayList<>();
		int i = 0;

		while (i < tokens.size()) {
			Token current = tokens.get(i);

			// Skip EOF
			if (current.type() == TokenType.EOF) {
				result.add(current);
				i++;
				continue;
			}

			// Try to match the 'from' sequence starting at position i
			if (matchesSequence(tokens, i, pair.from())) {
				// Replace with 'to' tokens
				for (String toLexeme : pair.to()) {
					// Create synthetic token with the first matched token's position
					result.add(new Token(determineTokenType(toLexeme), toLexeme, current.line(), current.column()));
				}
				// Skip the matched 'from' tokens
				i += pair.from().size();
			} else {
				// Check for partial match within identifier (e.g., :TAG: in :TAG:-ID)
				String lexeme = current.lexeme();
				boolean replaced = false;

				if (pair.from().size() == 1) {
					String fromText = pair.from().get(0);
					if (lexeme.contains(fromText) && !lexeme.equals(fromText)) {
						// Partial replacement within token
						String toText = pair.to().isEmpty() ? "" : String.join("", pair.to());
						String newLexeme = lexeme.replace(fromText, toText);
						result.add(new Token(current.type(), newLexeme, current.line(), current.column()));
						replaced = true;
					}
				}

				if (!replaced) {
					result.add(current);
				}
				i++;
			}
		}

		return result;
	}

	/**
	 * Check if tokens starting at index match the given sequence.
	 */
	private static boolean matchesSequence(List<Token> tokens, int startIndex, List<String> sequence) {
		if (startIndex + sequence.size() > tokens.size()) {
			return false;
		}

		for (int j = 0; j < sequence.size(); j++) {
			Token token = tokens.get(startIndex + j);
			String expected = sequence.get(j);

			// Skip EOF in comparison
			if (token.type() == TokenType.EOF) {
				return false;
			}

			// Case-insensitive comparison for keywords/identifiers
			if (!token.lexeme().equalsIgnoreCase(expected)) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Determine token type for a lexeme.
	 */
	private static TokenType determineTokenType(String lexeme) {
		if (lexeme.matches("\\d+")) {
			return TokenType.INTEGER;
		}
		String upper = lexeme.toUpperCase();
		if (Set.of("PIC", "PICTURE", "USAGE", "OCCURS", "REDEFINES", "COPY", "TIMES", "VALUE", "VALUES", "THRU",
				"THROUGH", "REPLACING", "BY", "DEPENDING", "ON", "TO", "RENAMES", "IS", "COMP-3").contains(upper)) {
			return TokenType.KEYWORD;
		}
		return TokenType.IDENTIFIER;
	}

	/**
	 * Reconstruct source text from tokens.
	 */
	private static String reconstructSource(List<Token> tokens) {
		StringBuilder sb = new StringBuilder();
		sb.append("       "); // fixed format start
		Token prev = null;

		for (Token t : tokens) {
			if (t.type() == TokenType.EOF)
				break;

			if (prev != null) {
				// newline preservation: if original token line changes, emit newline
				if (t.line() > prev.line()) {
					sb.append("\n       ");
				} else {
					// same line: separate tokens with a space unless punctuation
					if (needsSpace(prev, t))
						sb.append(' ');
				}
			}

			sb.append(t.lexeme());
			prev = t;
		}
		return sb.toString();
	}

	private static boolean needsSpace(Token prev, Token cur) {
		// No space before punctuation like '.' ')' ',' etc.
		if (cur.type() == TokenType.PUNCT)
			return false;
		// No space after '('
		if (prev.type() == TokenType.PUNCT && prev.lexeme().equals("("))
			return false;
		return true;
	}

	/**
	 * Represents a REPLACING pair with tokenized from/to sequences.
	 */
	private record TokenSequencePair(List<String> from, List<String> to) {
	}

	/**
	 * Read all content from a Reader into a String.
	 */
	private static String readAll(Reader reader) throws IOException {
		StringBuilder sb = new StringBuilder();
		char[] buffer = new char[4096];
		int read;
		while ((read = reader.read(buffer)) != -1) {
			sb.append(buffer, 0, read);
		}
		return sb.toString();
	}
}
