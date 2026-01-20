package com.mainframe.model;

import java.util.List;

/**
 * Represents a collection of fields that share the same byte range via
 * the COBOL REDEFINES clause.  The base path refers to the first
 * declared field, and {@code memberPaths} lists all fields that
 * overlay the same region (including the base itself).  The offset
 * and length define the common byte range for the overlay.
 *
 * <p>The layout engine is responsible for constructing overlay groups
 * based on REDEFINES declarations in the AST.</p>
 *
 * @param basePath    path of the first declared field in the overlay group
 * @param offset      absolute offset where the overlay begins
 * @param length      maximum length among all members
 * @param memberPaths list of all member field paths (ordered as declared)
 */
public record OverlayGroup(
        String basePath,
        int offset,
        int length,
        List<String> memberPaths
) {
}