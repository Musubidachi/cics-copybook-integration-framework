package com.mainframe.model;

import com.mainframe.copybook.parser.Diagnostic;
import com.mainframe.copybook.parser.OccursClause;
import com.mainframe.copybook.parser.RedefinesClause;
import com.mainframe.copybook.parser.ast.AstNode;
import com.mainframe.copybook.parser.ast.ConditionNameNode;
import com.mainframe.copybook.parser.ast.CopyNode;
import com.mainframe.copybook.parser.ast.DataItemNode;
import com.mainframe.copybook.parser.pic.PicClause;
import com.mainframe.copybook.parser.pic.PicElement;
import com.mainframe.copybook.parser.pic.PicRepeat;
import com.mainframe.copybook.parser.pic.PicSymbol;
import com.mainframe.copybook.parser.pic.PicVirtualDecimal;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Adapter utilities that convert the stable AST produced by {@code copybook-parser}
 * into the simpler AST expected by {@code copybook-model}.
 *
 * <p>The model module intentionally does not understand COPY statements, source
 * spans, or multi-valued condition name specifications. Those concepts are either
 * resolved/expanded in the parser module or flattened here.</p>
 */
public final class ParserAstAdapter {

    private ParserAstAdapter() { }

    /**
     * Convert a parser {@link com.mainframe.copybook.parser.ast.CopybookAst} into the model's {@link CopybookAst}.
     *
     * <p>Rules:
     * <ul>
     *   <li>If diagnostics are present, an {@link IllegalArgumentException} is thrown.</li>
     *   <li>If COPY nodes are present in roots, an {@link IllegalArgumentException} is thrown; enable COPY expansion in the parser.</li>
     *   <li>Non-data-item root nodes are ignored (future-proofing), except COPY nodes.</li>
     * </ul>
     * </p>
     */
    public static CopybookAst toModelAst(com.mainframe.copybook.parser.ast.CopybookAst parserAst) {
        Objects.requireNonNull(parserAst, "parserAst");

        if (parserAst.diagnostics() != null && !parserAst.diagnostics().isEmpty()) {
            String msg = parserAst.diagnostics().stream()
                    .map(ParserAstAdapter::formatDiagnostic)
                    .collect(Collectors.joining("\n"));
            throw new IllegalArgumentException("Parser diagnostics present; refusing to build layout:\n" + msg);
        }

        List<com.mainframe.model.DataItemNode> top = new ArrayList<>();
        for (AstNode node : parserAst.roots()) {
            if (node instanceof CopyNode) {
                throw new IllegalArgumentException(
                        "Unexpanded COPY statement encountered in roots. " +
                        "Re-run copybook-parser with options.expandCopy=true and a resolver.");
            }
            if (node instanceof DataItemNode di) {
                top.add(toModelDataItem(di));
            }
        }
        return new CopybookAst(top);
    }

    private static String formatDiagnostic(Diagnostic d) {
        if (d == null) return "(null diagnostic)";
        String where = d.span() == null ? "" : (" @ " + d.span());
        return "[" + d.category() + ":" + d.code() + "] " + d.message() + where;
    }

    private static com.mainframe.model.DataItemNode toModelDataItem(DataItemNode n) {
        PicSummary pic = toPicSummary(n.pic());
        com.mainframe.model.Usage usage = toUsage(n.usage());
        Integer occurs = toOccursCount(n.occurs());
        String redefines = toRedefinesName(n.redefines());

        List<com.mainframe.model.DataItemNode> children = new ArrayList<>();
        for (AstNode child : n.children()) {
            if (child instanceof DataItemNode di) {
                children.add(toModelDataItem(di));
            }
        }

        List<ConditionName> conditions = new ArrayList<>();
        for (ConditionNameNode cn : n.conditionNames()) {
            conditions.addAll(toConditionNames(cn));
        }

        return new com.mainframe.model.DataItemNode(
                n.level(),
                n.name(),
                pic,
                usage,
                occurs,
                redefines,
                children,
                conditions
        );
    }

    private static Integer toOccursCount(OccursClause occurs) {
        return occurs == null ? null : occurs.count();
    }

    private static String toRedefinesName(RedefinesClause redefines) {
        return redefines == null ? null : redefines.targetName();
    }

    private static com.mainframe.model.Usage toUsage(com.mainframe.copybook.parser.Usage usage) {
        if (usage == null) {
            // COBOL default is DISPLAY when no USAGE is specified.
            return com.mainframe.model.Usage.DISPLAY;
        }
        return switch (usage) {
            case DISPLAY -> com.mainframe.model.Usage.DISPLAY;
            case COMP -> com.mainframe.model.Usage.COMP;
            case COMP_3 -> com.mainframe.model.Usage.COMP3;
        };
    }

    /**
     * Flatten the parser PIC clause into the model's {@link PicSummary}.
     *
     * <p>This implementation supports the subset that copybook-model's byte length rules cover:
     * symbols 9/X, repeats, and implied decimal (V). Other PIC constructs should be added
     * as the framework grows.</p>
     */
    private static PicSummary toPicSummary(PicClause pic) {
        if (pic == null) return null;

        int digits = 0;
        int scale = 0;
        boolean inDecimal = false;

        for (PicElement el : pic.elements()) {
            int[] ds = countDigitsAndScale(el, inDecimal);
            digits += ds[0];
            scale += ds[1];
            // Update decimal flag if element was V (or contained V via repeat)
            inDecimal = inDecimal || containsVirtualDecimal(el);
        }

        return new PicSummary(digits, scale, pic.signed());
    }

    private static boolean containsVirtualDecimal(PicElement el) {
        if (el instanceof PicVirtualDecimal) return true;
        if (el instanceof PicRepeat) return false;
        return false;
    }

    /**
     * @return int[]{digitsAdded, scaleAdded}
     */
    private static int[] countDigitsAndScale(PicElement el, boolean inDecimal) {
        if (el instanceof PicVirtualDecimal) {
            return new int[]{0, 0};
        }
        if (el instanceof PicSymbol ps) {
            char c = Character.toUpperCase(ps.symbol());
            if (c == '9' || c == 'X') {
                return new int[]{1, (c == '9' && inDecimal) ? 1 : 0};
            }
            // Unsupported symbol: ignore here; layout will likely be wrong, so fail fast.
            throw new IllegalArgumentException("Unsupported PIC symbol: " + ps.symbol());
        }
        if (el instanceof PicRepeat pr) {
            char c = Character.toUpperCase(pr.symbol());
            if (c == '9' || c == 'X') {
                int digitsAdded = pr.count();
                int scaleAdded = (c == '9' && inDecimal) ? pr.count() : 0;
                return new int[]{digitsAdded, scaleAdded};
            }
            throw new IllegalArgumentException("Unsupported PIC repeat symbol: " + pr.symbol());
        }

throw new IllegalArgumentException("Unsupported PIC element: " + el.getClass().getSimpleName());
    }

    private static List<ConditionName> toConditionNames(ConditionNameNode cn) {
        if (cn == null) return List.of();
        if (cn.values() == null || cn.values().isEmpty()) {
            return List.of(new ConditionName(cn.name(), ""));
        }

        // copybook-model currently models a single "value" string. Flatten multiple specs into a readable encoding.
        String flattened = cn.values().stream()
                .map(v -> v.thruValue() == null ? v.value() : (v.value() + " THRU " + v.thruValue()))
                .collect(Collectors.joining(", "));

        return List.of(new ConditionName(cn.name(), flattened));
    }
}
