package com.mainframe.model;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Computes a {@link LayoutModel} from a {@link CopybookAst}.  This
 * implementation walks the AST depth‑first, expanding fixed OCCURS
 * clauses, computing field lengths based on PIC/USAGE, assigning
 * absolute byte offsets and constructing overlay groups for
 * REDEFINES.  The algorithm is deterministic and does not perform
 * any alignment or padding beyond the COBOL rules described in the
 * module documentation.
 */
public class LayoutEngine {
    /**
     * Builds a concrete layout model from the supplied AST.
     *
     * @param ast fully parsed copybook AST
     * @return deterministic {@code LayoutModel}
     */
    
    /**
     * Convenience overload that accepts the AST produced by {@code copybook-parser}.
     * This method converts the parser AST into the model AST and then delegates
     * to {@link #build(CopybookAst)}.
     *
     * @param parserAst AST returned by copybook-parser
     * @return computed layout model
     */
    public LayoutModel build(com.mainframe.copybook.parser.ast.CopybookAst parserAst) {
        return build(ParserAstAdapter.toModelAst(parserAst));
    }

public LayoutModel build(CopybookAst ast) {
        List<LayoutField> fields = new ArrayList<>();
        Map<String, LayoutField> pathToField = new LinkedHashMap<>();
        Map<String, OverlayGroupBuilder> overlayBuilders = new LinkedHashMap<>();

        int offset = 0;
        for (DataItemNode top : ast.topLevelItems()) {
            offset = processNode(top, "", offset, fields, pathToField, overlayBuilders);
        }
        int totalLength = offset;

        // Build overlay groups from builders
        List<OverlayGroup> overlays = new ArrayList<>();
        for (OverlayGroupBuilder b : overlayBuilders.values()) {
            overlays.add(new OverlayGroup(b.basePath, b.offset, b.length, List.copyOf(b.memberPaths)));
        }
        return new LayoutModel(totalLength, fields, overlays);
    }

    private int processNode(DataItemNode node,
                            String currentPath,
                            int offset,
                            List<LayoutField> fields,
                            Map<String, LayoutField> pathToField,
                            Map<String, OverlayGroupBuilder> overlayBuilders) {
        int occursCount = node.occurs() != null ? node.occurs() : 1;
        int localOffset = offset;
        for (int i = 0; i < occursCount; i++) {
            String path = currentPath.isEmpty() ? node.name() : currentPath + "." + node.name();
            if (occursCount > 1) {
                path = path + "[" + i + "]";
            }
            if (node.isGroup()) {
                int childOffset = localOffset;
                for (DataItemNode child : node.children()) {
                    childOffset = processNode(child, path, childOffset, fields, pathToField, overlayBuilders);
                }
                localOffset = childOffset;
            } else {
                int fieldLength = node.pic().byteLength(node.usage());
                int fieldOffset = localOffset;
                // Handle REDEFINES: overlay on existing field
                if (node.redefines() != null) {
                    // Determine the base path relative to the current scope
                    String basePath = currentPath.isEmpty() ? node.redefines()
                            : currentPath + "." + node.redefines();
                    LayoutField baseField = pathToField.get(basePath);
                    if (baseField == null) {
                        throw new IllegalStateException(
                                "REDEFINES refers to unknown field '" + basePath + "'");
                    }
                    // overlay uses the same offset as the base
                    fieldOffset = baseField.offset();
                    // Register overlay group builder
                    OverlayGroupBuilder builder = overlayBuilders.computeIfAbsent(basePath, bp -> {
                        // base group initialised with base as first member
                        OverlayGroupBuilder ob = new OverlayGroupBuilder(bp, baseField.offset(), baseField.length());
                        ob.memberPaths.add(basePath);
                        return ob;
                    });
                    builder.memberPaths.add(path);
                    // Keep track of maximum length across all members
                    if (fieldLength > builder.length) {
                        builder.length = fieldLength;
                    }
                    // Do not advance localOffset on redefines
                } else {
                    // Not a redefine, so increment offset normally
                    localOffset += fieldLength;
                }
                LayoutField field = new LayoutField(path, fieldOffset, fieldLength,
                        node.usage(), node.pic(), occursCount > 1 ? i : 0);
                fields.add(field);
                pathToField.put(path, field);
            }
        }
        return localOffset;
    }

    /**
     * Internal mutable builder used to accumulate overlay information
     * during traversal.  Once traversal is complete the builders are
     * converted into immutable {@link OverlayGroup} instances.
     */
    private static class OverlayGroupBuilder {
        final String basePath;
        final int offset;
        int length;
        final List<String> memberPaths = new ArrayList<>();

        OverlayGroupBuilder(String basePath, int offset, int length) {
            this.basePath = basePath;
            this.offset = offset;
            this.length = length;
        }
    }
}