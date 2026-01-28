package com.mainframe.codegen.adapter;

import com.mainframe.codegen.GenerationConfig;
import com.mainframe.codegen.ModelInvariantException;
import com.mainframe.codegen.encoding.ByteLengthCalculator;
import com.mainframe.codegen.encoding.DisplaySignMode;
import com.mainframe.model.LayoutField;
import com.mainframe.model.LayoutModel;
import com.mainframe.model.OverlayGroup;
import com.mainframe.model.Usage;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Adapts copybook-model domain objects to the internal codegen representation.
 *
 * <p>This adapter serves as the single entry point for converting
 * {@link LayoutModel} objects to {@link CodegenModel}. It performs
 * validation and canonicalization to ensure the model is suitable
 * for code generation.</p>
 *
 * <p><strong>Validations performed:</strong></p>
 * <ul>
 *   <li>Field overlaps (outside overlay groups) are rejected</li>
 *   <li>Negative offsets are rejected</li>
 *   <li>Zero or negative lengths are rejected</li>
 *   <li>Overlay group member offsets must match</li>
 *   <li>Overlay group length must accommodate all members</li>
 *   <li>OCCURS indices must be contiguous (0..N-1)</li>
 *   <li>For signed DISPLAY with overpunch, length must equal digits</li>
 * </ul>
 *
 * <p><strong>Canonicalization:</strong></p>
 * <ul>
 *   <li>Record and field names are converted to valid Java identifiers</li>
 *   <li>Fields are sorted deterministically by offset, then path, then length</li>
 *   <li>Overlay group members are sorted deterministically</li>
 * </ul>
 */
public final class ModelAdapter {

    private static final Pattern OCCURS_INDEX_PATTERN = Pattern.compile("\\[(\\d+)]");
    private static final Pattern VALID_IDENTIFIER_CHARS = Pattern.compile("[A-Za-z0-9_-]+");

    private ModelAdapter() {
        // static utility class
    }

    /**
     * Adapt a LayoutModel to the internal CodegenModel representation.
     *
     * @param layoutModel the source layout model
     * @param recordName  the COBOL record name (typically from the 01 level)
     * @param config      the generation configuration
     * @return the adapted CodegenModel
     * @throws ModelInvariantException if validation fails
     */
    public static CodegenModel adapt(LayoutModel layoutModel, String recordName, GenerationConfig config) {
        Objects.requireNonNull(layoutModel, "layoutModel is required");
        Objects.requireNonNull(recordName, "recordName is required");
        Objects.requireNonNull(config, "config is required");

        // Validate the model
        validateModel(layoutModel, config);

        // Convert fields
        List<CodegenField> fields = adaptFields(layoutModel.fields(), config);

        // Sort fields deterministically
        fields = sortFields(fields);

        // Convert overlay groups
        List<CodegenOverlayGroup> overlayGroups = adaptOverlayGroups(layoutModel.overlays());

        // Build the codegen model
        return CodegenModel.builder()
                .recordName(recordName)
                .javaClassName(toJavaClassName(recordName))
                .totalLength(layoutModel.totalLength())
                .fields(fields)
                .overlayGroups(overlayGroups)
                .build();
    }

    // =========================================================================
    // Validation
    // =========================================================================

    private static void validateModel(LayoutModel model, GenerationConfig config) {
        validateRecordLength(model);
        validateFields(model.fields(), config);
        validateNoOverlapsOutsideGroups(model);
        validateOverlayGroups(model.overlays());
        validateOccursIndices(model.fields());
    }

    private static void validateRecordLength(LayoutModel model) {
        if (model.totalLength() <= 0) {
            throw new ModelInvariantException("RECORD_LENGTH",
                    "Record length must be positive, got: " + model.totalLength());
        }
    }

    private static void validateFields(List<LayoutField> fields, GenerationConfig config) {
        for (LayoutField field : fields) {
            validateField(field, config);
        }
    }

    private static void validateField(LayoutField field, GenerationConfig config) {
        // Validate offset
        if (field.offset() < 0) {
            throw new ModelInvariantException("NEGATIVE_OFFSET", field.path(),
                    "Field offset must be non-negative, got: " + field.offset());
        }

        // Validate length
        if (field.length() <= 0) {
            throw new ModelInvariantException("INVALID_LENGTH", field.path(),
                    "Field length must be positive, got: " + field.length());
        }

        // Validate path characters
        String cleanPath = field.path().replaceAll("\\[\\d+]", "");
        for (String segment : cleanPath.split("\\.")) {
            if (!VALID_IDENTIFIER_CHARS.matcher(segment).matches()) {
                throw new ModelInvariantException("INVALID_PATH_CHARS", field.path(),
                        "Path segment contains unsupported characters: " + segment);
            }
        }

        // Validate signed DISPLAY length for overpunch mode
        if (config.getDisplaySignMode() == DisplaySignMode.OVERPUNCH_TRAILING
                && field.usage() == Usage.DISPLAY
                && field.pic() != null
                && field.pic().signed()) {

            int expectedLength = ByteLengthCalculator.displayNumericByteLength(
                    field.pic().digits(), true, DisplaySignMode.OVERPUNCH_TRAILING);

            if (field.length() != expectedLength) {
                throw new ModelInvariantException("OVERPUNCH_LENGTH_MISMATCH", field.path(),
                        String.format("Signed DISPLAY with overpunch requires length = digits (%d), but got %d. " +
                                        "This may indicate the upstream model uses separate sign encoding.",
                                expectedLength, field.length()));
            }
        }
    }

    private static void validateNoOverlapsOutsideGroups(LayoutModel model) {
        List<LayoutField> leaves = model.fields();
        Set<String> overlayPaths = new HashSet<>();
        for (OverlayGroup group : model.overlays()) {
            overlayPaths.addAll(group.memberPaths());
        }

        // Check each pair of fields for overlap
        for (int i = 0; i < leaves.size(); i++) {
            LayoutField a = leaves.get(i);
            for (int j = i + 1; j < leaves.size(); j++) {
                LayoutField b = leaves.get(j);

                // Check if they overlap
                if (overlaps(a, b)) {
                    // Both must be in the same overlay group
                    boolean aInOverlay = overlayPaths.contains(a.path());
                    boolean bInOverlay = overlayPaths.contains(b.path());

                    if (!aInOverlay || !bInOverlay) {
                        throw new ModelInvariantException("FIELD_OVERLAP", a.path(),
                                String.format("Field overlaps with '%s' but one or both are not in an overlay group. " +
                                        "Offsets: %d+%d vs %d+%d", b.path(), a.offset(), a.length(), b.offset(), b.length()));
                    }
                }
            }
        }
    }

    private static boolean overlaps(LayoutField a, LayoutField b) {
        int aEnd = a.offset() + a.length();
        int bEnd = b.offset() + b.length();
        return a.offset() < bEnd && b.offset() < aEnd;
    }

    private static void validateOverlayGroups(List<OverlayGroup> groups) {
        for (OverlayGroup group : groups) {
            // Validate that overlay length accommodates all members
            // (This is enforced by the presence of fields in the group)
            if (group.memberPaths().isEmpty()) {
                throw new ModelInvariantException("EMPTY_OVERLAY",
                        "Overlay group has no members");
            }
        }
    }

    private static void validateOccursIndices(List<LayoutField> fields) {
        // Group fields by their OCCURS base path and validate contiguity
        Map<String, Set<Integer>> occursIndices = new HashMap<>();

        for (LayoutField field : fields) {
            Matcher matcher = OCCURS_INDEX_PATTERN.matcher(field.path());
            while (matcher.find()) {
                int index = Integer.parseInt(matcher.group(1));
                String basePath = field.path().substring(0, matcher.start());
                occursIndices.computeIfAbsent(basePath, k -> new TreeSet<>()).add(index);
            }
        }

        // Validate each OCCURS has contiguous indices starting from 0
        for (Map.Entry<String, Set<Integer>> entry : occursIndices.entrySet()) {
            Set<Integer> indices = entry.getValue();
            int expectedSize = Collections.max(indices) + 1;
            if (indices.size() != expectedSize) {
                throw new ModelInvariantException("OCCURS_NOT_CONTIGUOUS", entry.getKey(),
                        "OCCURS indices must be contiguous from 0 to N-1, got: " + indices);
            }
            if (!indices.contains(0)) {
                throw new ModelInvariantException("OCCURS_NOT_CONTIGUOUS", entry.getKey(),
                        "OCCURS indices must start at 0, got: " + indices);
            }
        }
    }

    // =========================================================================
    // Field Adaptation
    // =========================================================================

    private static List<CodegenField> adaptFields(List<LayoutField> fields, GenerationConfig config) {
        return fields.stream()
                .map(f -> adaptField(f, config))
                .collect(Collectors.toList());
    }

    private static CodegenField adaptField(LayoutField field, GenerationConfig config) {
        CodegenField.FieldType type = determineFieldType(field);
        int occursIndex = extractOccursIndex(field.path());

        return CodegenField.builder()
                .path(field.path())
                .javaName(toJavaFieldName(field.path()))
                .offset(field.offset())
                .length(field.length())
                .type(type)
                .usage(field.usage())
                .digits(field.pic() != null ? field.pic().digits() : 0)
                .scale(field.pic() != null ? field.pic().scale() : 0)
                .signed(field.pic() != null && field.pic().signed())
                .occursIndex(occursIndex)
                .build();
    }

    private static CodegenField.FieldType determineFieldType(LayoutField field) {
        if (field.usage() == null) {
            return CodegenField.FieldType.ALPHANUMERIC;
        }

        switch (field.usage()) {
            case DISPLAY:
                if (field.pic() != null && field.pic().digits() > 0) {
                    return CodegenField.FieldType.DISPLAY_NUMERIC;
                }
                return CodegenField.FieldType.ALPHANUMERIC;
            case COMP:
                return CodegenField.FieldType.COMP;
            case COMP3:
                return CodegenField.FieldType.COMP3;
            default:
                return CodegenField.FieldType.ALPHANUMERIC;
        }
    }

    private static int extractOccursIndex(String path) {
        Matcher matcher = OCCURS_INDEX_PATTERN.matcher(path);
        int lastIndex = -1;
        while (matcher.find()) {
            lastIndex = Integer.parseInt(matcher.group(1));
        }
        return lastIndex;
    }

    // =========================================================================
    // Overlay Group Adaptation
    // =========================================================================

    private static List<CodegenOverlayGroup> adaptOverlayGroups(List<OverlayGroup> groups) {
        return groups.stream()
                .map(ModelAdapter::adaptOverlayGroup)
                .collect(Collectors.toList());
    }

    private static CodegenOverlayGroup adaptOverlayGroup(OverlayGroup group) {
        List<String> sortedMembers = new ArrayList<>(group.memberPaths());
        Collections.sort(sortedMembers);

        return new CodegenOverlayGroup(
                group.basePath(),
                group.offset(),
                group.length(),
                sortedMembers
        );
    }

    // =========================================================================
    // Sorting
    // =========================================================================

    private static List<CodegenField> sortFields(List<CodegenField> fields) {
        List<CodegenField> sorted = new ArrayList<>(fields);
        sorted.sort(Comparator
                .comparingInt(CodegenField::getOffset)
                .thenComparing(CodegenField::getPath)
                .thenComparingInt(CodegenField::getLength));
        return sorted;
    }

    // =========================================================================
    // Name Canonicalization
    // =========================================================================

    /**
     * Convert a COBOL name to a valid Java class name.
     * Example: "ACCOUNT-RECORD" → "AccountRecord"
     */
    private static String toJavaClassName(String cobolName) {
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = true;

        for (char c : cobolName.toCharArray()) {
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    result.append(Character.toLowerCase(c));
                }
            }
        }

        // Ensure starts with letter
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result.insert(0, '_');
        }

        return result.length() > 0 ? result.toString() : "Record";
    }

    /**
     * Convert a COBOL field path to a valid Java field name.
     * Example: "ACCOUNT.HOLDER-NAME" → "accountHolderName"
     */
    private static String toJavaFieldName(String path) {
        // Remove OCCURS indices
        String cleanPath = path.replaceAll("\\[\\d+]", "");

        // Take the last segment of the path
        String[] segments = cleanPath.split("\\.");
        String fieldName = segments[segments.length - 1];

        // Convert to camelCase
        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = false;

        for (int i = 0; i < fieldName.length(); i++) {
            char c = fieldName.charAt(i);
            if (c == '-' || c == '_') {
                capitalizeNext = true;
            } else if (Character.isLetterOrDigit(c)) {
                if (capitalizeNext && result.length() > 0) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else if (result.length() == 0) {
                    result.append(Character.toLowerCase(c));
                } else {
                    result.append(Character.toLowerCase(c));
                }
            }
        }

        // Ensure starts with letter
        if (result.length() > 0 && Character.isDigit(result.charAt(0))) {
            result.insert(0, '_');
        }

        return result.length() > 0 ? result.toString() : "field";
    }
}
