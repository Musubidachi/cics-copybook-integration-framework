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

import java.util.List;

/**
 * Serializes a CopybookAst to JSON in a deterministic, stable format. The
 * output format matches the expected snapshot files in the test fixtures. This
 * implementation uses manual JSON construction without external dependencies.
 */
public final class AstJsonSerializer {

	private static final String INDENT = "  ";

	/**
	 * Serialize a CopybookAst to JSON string.
	 *
	 * @param ast the AST to serialize
	 * @return JSON string representation
	 */
	public String serialize(CopybookAst ast) {
		StringBuilder sb = new StringBuilder();
		sb.append("{\n");

		List<AstNode> roots = ast.roots();
		if (roots.size() == 1) {
			boolean omitRootLevel = false;

			if (roots.get(0) instanceof DataItemNode rootItem) {
			    // If the root has children and ALL of them are COPY nodes, omit root.level
			    omitRootLevel = !rootItem.children().isEmpty()
			            && rootItem.children().stream().allMatch(c -> c instanceof CopyNode);
			}
			// Single root - use "root" key
			sb.append(INDENT).append("\"root\": ");
			serializeNode(roots.get(0), sb, 1, omitRootLevel);
		} else if (roots.size() > 1) {
			// Multiple roots - use "roots" array
			sb.append(INDENT).append("\"roots\": [\n");
			for (int i = 0; i < roots.size(); i++) {
				indent(sb, 2);
				serializeNode(roots.get(i), sb, 2, false);
				if (i < roots.size() - 1) {
					sb.append(",");
				}
				sb.append("\n");
			}
			sb.append(INDENT).append("]");
		}

		sb.append("\n}");
		return sb.toString();
	}

	/**
	 * Serialize a single AST node.
	 */
	private void serializeNode(AstNode node, StringBuilder sb, int depth, boolean omit) {
		if (node instanceof DataItemNode item) {
			serializeDataItem(item, sb, depth, omit);
		} else if (node instanceof CopyNode copy) {
			serializeCopyNode(copy, sb, depth);
		} else if (node instanceof ConditionNameNode cond) {
			serializeConditionName(cond, sb, depth);
		} else {
			throw new IllegalArgumentException("Unsupported AstNode type: " + node.getClass().getName());
		}
	}

	/**
	 * Serialize a DataItemNode.
	 */
	private void serializeDataItem(DataItemNode item, StringBuilder sb, int depth, boolean omitLevel) {
		sb.append("{\n");

		// Name
		indent(sb, depth + 1);
		sb.append("\"name\": ").append(jsonString(item.name()));

		// Level (omit when requested)
		if (!omitLevel) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"level\": ").append(item.level());
		}

		// PIC (as string representation)
		if (item.pic() != null) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"pic\": ").append(jsonString(picToString(item.pic())));
		}

		// Usage
		if (item.usage() != null) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"usage\": ").append(jsonString(item.usage().name()));
		}

		// Occurs
		if (item.occurs() != null) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"occurs\": ").append(item.occurs().count());
		}

		// Redefines
		if (item.redefines() != null) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"redefines\": ").append(jsonString(item.redefines().targetName()));
		}

		// Condition names (level-88)
		if (item.conditionNames() != null && !item.conditionNames().isEmpty()) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"conditions\": [\n");
			for (int i = 0; i < item.conditionNames().size(); i++) {
				indent(sb, depth + 2);
				serializeConditionName(item.conditionNames().get(i), sb, depth + 2);
				if (i < item.conditionNames().size() - 1) {
					sb.append(",");
				}
				sb.append("\n");
			}
			indent(sb, depth + 1);
			sb.append("]");
		}

		// Children
		if (item.children() != null && !item.children().isEmpty()) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"children\": [\n");
			for (int i = 0; i < item.children().size(); i++) {
				indent(sb, depth + 2);
				serializeNode(item.children().get(i), sb, depth + 2, false);
				if (i < item.children().size() - 1) {
					sb.append(",");
				}
				sb.append("\n");
			}
			indent(sb, depth + 1);
			sb.append("]");
		}

		sb.append("\n");
		indent(sb, depth);
		sb.append("}");
	}

	/**
	 * Serialize a CopyNode.
	 */
	private void serializeCopyNode(CopyNode copy, StringBuilder sb, int depth) {
		sb.append("{\n");

		// Type
		indent(sb, depth + 1);
		sb.append("\"type\": \"COPY\"");

		// Name
		sb.append(",\n");
		indent(sb, depth + 1);
		sb.append("\"name\": ").append(jsonString(copy.copybookName()));

		// Replacing pairs
		if (copy.replacingPairs() != null && !copy.replacingPairs().isEmpty()) {
			sb.append(",\n");
			indent(sb, depth + 1);
			sb.append("\"replacing\": [\n");
			for (int i = 0; i < copy.replacingPairs().size(); i++) {
				CopyNode.ReplacingPair pair = copy.replacingPairs().get(i);
				indent(sb, depth + 2);
				sb.append("{\n");
				indent(sb, depth + 3);
				sb.append("\"from\": ").append(jsonString(pair.from())).append(",\n");
				indent(sb, depth + 3);
				sb.append("\"to\": ").append(jsonString(pair.to())).append("\n");
				indent(sb, depth + 2);
				sb.append("}");
				if (i < copy.replacingPairs().size() - 1) {
					sb.append(",");
				}
				sb.append("\n");
			}
			indent(sb, depth + 1);
			sb.append("]");
		}

		sb.append("\n");
		indent(sb, depth);
		sb.append("}");
	}

	/**
	 * Serialize a ConditionNameNode.
	 */
	private void serializeConditionName(ConditionNameNode cond, StringBuilder sb, int depth) {
		sb.append("{\n");

		// Name
		indent(sb, depth + 1);
		sb.append("\"name\": ").append(jsonString(cond.name()));

		// Level (always 88 for condition names)
		sb.append(",\n");
		indent(sb, depth + 1);
		sb.append("\"level\": 88");

		// Values
		if (cond.values() != null && !cond.values().isEmpty()) {
			if (cond.values().size() == 1) {
				ConditionNameNode.ValueSpec spec = cond.values().get(0);
				if (spec.thruValue() != null) {
					sb.append(",\n");
					indent(sb, depth + 1);
					sb.append("\"valueFrom\": ").append(jsonString(spec.value()));
					sb.append(",\n");
					indent(sb, depth + 1);
					sb.append("\"valueThru\": ").append(jsonString(spec.thruValue()));
				} else {
					sb.append(",\n");
					indent(sb, depth + 1);
					sb.append("\"value\": ").append(jsonString(spec.value()));
				}
			} else {
				sb.append(",\n");
				indent(sb, depth + 1);
				sb.append("\"values\": [\n");
				for (int i = 0; i < cond.values().size(); i++) {
					ConditionNameNode.ValueSpec spec = cond.values().get(i);
					indent(sb, depth + 2);
					if (spec.thruValue() != null) {
						sb.append("{\n");
						indent(sb, depth + 3);
						sb.append("\"from\": ").append(jsonString(spec.value())).append(",\n");
						indent(sb, depth + 3);
						sb.append("\"thru\": ").append(jsonString(spec.thruValue())).append("\n");
						indent(sb, depth + 2);
						sb.append("}");
					} else {
						sb.append(jsonString(spec.value()));
					}
					if (i < cond.values().size() - 1) {
						sb.append(",");
					}
					sb.append("\n");
				}
				indent(sb, depth + 1);
				sb.append("]");
			}
		}

		sb.append("\n");
		indent(sb, depth);
		sb.append("}");
	}

	/**
	 * Convert a PicClause to its string representation.
	 */
	private String picToString(PicClause pic) {
		StringBuilder sb = new StringBuilder();

		if (pic.signed()) {
			sb.append('S');
		}

		for (PicElement elem : pic.elements()) {
			if (elem instanceof PicSymbol s) {
				sb.append(s.symbol());
			} else if (elem instanceof PicRepeat r) {
				sb.append(r.symbol()).append('(').append(r.count()).append(')');
			} else if (elem instanceof PicVirtualDecimal) {
				sb.append('V');
			} else {
				// intentionally ignore unknown PicElement types
			}
		}

		return sb.toString();
	}

	/**
	 * Escape a string for JSON output.
	 */
	private String jsonString(String value) {
		if (value == null) {
			return "null";
		}
		StringBuilder sb = new StringBuilder();
		sb.append('"');
		for (char c : value.toCharArray()) {
			switch (c) {
			case '"' -> sb.append("\\\"");
			case '\\' -> sb.append("\\\\");
			case '\b' -> sb.append("\\b");
			case '\f' -> sb.append("\\f");
			case '\n' -> sb.append("\\n");
			case '\r' -> sb.append("\\r");
			case '\t' -> sb.append("\\t");
			default -> {
				if (c < 0x20) {
					sb.append(String.format("\\u%04x", (int) c));
				} else {
					sb.append(c);
				}
			}
			}
		}
		sb.append('"');
		return sb.toString();
	}

	/**
	 * Add indentation.
	 */
	private void indent(StringBuilder sb, int depth) {
		for (int i = 0; i < depth; i++) {
			sb.append(INDENT);
		}
	}
}
