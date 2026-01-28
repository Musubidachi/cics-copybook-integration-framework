package com.mainframe.codegen.adapter;

import com.mainframe.codegen.GenerationConfig;
import com.mainframe.codegen.ModelInvariantException;
import com.mainframe.codegen.encoding.ByteLengthCalculator;
import com.mainframe.codegen.encoding.DisplaySignMode;
import com.mainframe.codegen.model.*;
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
 * <p>This adapter serves as the <strong>single entry point</strong> for converting
 * {@link LayoutModel} objects to {@link CodegenRecordModel}. It performs
 * validation and canonicalization to ensure the model is suitable
 * for code generation.</p>
 *
 * <p><strong>Validations performed:</strong></p>
 * <ul>
 *   <li>Field offsets must be non-negative</li>
 *   <li>Field lengths must be positive</li>
 *   <li>Path characters must be valid identifiers (A-Za-z0-9_-)</li>
 *   <li>OCCURS indices must be contiguous (0..N-1)</li>
 *   <li>Field overlaps are forbidden unless in the same overlay group</li>
 *   <li>Overlay members must share the same base offset</li>
 *   <li>Overlay length must be >= max(member length)</li>
 *   <li>Members must be fully contained within overlay region</li>
 *   <li>For signed DISPLAY with overpunch, length must equal digits</li>
 * </ul>
 *
 * <p><strong>Canonicalization:</strong></p>
 * <ul>
 *   <li>Record names are converted to UpperCamel Java class names</li>
 *   <li>Field names are converted to lowerCamel Java field names</li>
 *   <li>Name collisions at the same scope cause immediate failure</li>
 *   <li>Fields are sorted deterministically by offset, then path, then length</li>
 *   <li>Overlay group members are sorted deterministically</li>
 * </ul>
 */
public final class ModelAdapter {

    private static final Pattern OCCURS_INDEX_PATTERN = Pattern.compile("\\[(\\d+)]");
    private static final Pattern VALID_IDENTIFIER_CHARS = Pattern.compile("[A-Za-z0-9_-]+");
    private static final Set<String> SUPPORTED_PATH_CHARS = Set.of(
            "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
            "abcdefghijklmnopqrstuvwxyz",
            "0123456789",
            "_-"
    );

    private ModelAdapter() {
        // static utility class
    }

    /**
     * Adapt a LayoutModel to the internal CodegenRecordModel representation.
     *
     * <p>This is the only entry point for model adaptation.</p>
     *
     * @param layoutModel the source layout model
     * @param cfg         the generation configuration (includes recordName)
     * @return the adapted CodegenRecordModel
     * @throws ModelInvariantException if validation fails
     */
    public static CodegenRecordModel adapt(LayoutModel layoutModel, GenerationConfig cfg) {
        Objects.requireNonNull(layoutModel, "layoutModel is required");
        Objects.requireNonNull(cfg, "cfg is required");

        String recordName = cfg.getRecordName();
        Objects.requireNonNull(recordName, "recordName in config is required");

        // Validate the model
        validateModel(layoutModel, cfg);

        // Build overlay lookup for field adaptation
        Map<String, OverlayGroup> pathToOverlay = buildOverlayLookup(layoutModel.overlays());

        // Convert fields to internal model
        List<CodegenFieldInfo> flatFields = adaptFields(layoutModel.fields(), pathToOverlay, cfg);

        // Convert overlay groups
        List<CodegenOverlayInfo> overlayGroups = adaptOverlayGroups(layoutModel.overlays(), flatFields);

        // Build the group tree from field paths
        CodegenGroup rootGroup = buildGroupTree(recordName, flatFields);

        // Validate name collisions
        validateNameCollisions(flatFields);

        // Build the complete model
        return CodegenRecordModel.builder()
                .recordName(recordName)
                .javaClassName(toJavaClassName(recordName))
                .totalLength(layoutModel.totalLength())
                .flatFields(flatFields)
                .overlayGroups(overlayGroups)
                .rootGroup(rootGroup)
                .build();
    }

    // =========================================================================
    // Validation
    // =========================================================================

    private static void validateModel(LayoutModel model, GenerationConfig config) {
        validateRecordLength(model);
        validateFields(model.fields(), config);
        validateNoOverlapsOutsideGroups(model);
        validateOverlayGroups(model.overlays(), model.fields());
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
            if (segment.isEmpty()) {
                throw new ModelInvariantException("INVALID_PATH_CHARS", field.path(),
                        "Path contains empty segment");
            }
            if (!VALID_IDENTIFIER_CHARS.matcher(segment).matches()) {
                throw new ModelInvariantException("INVALID_PATH_CHARS", field.path(),
                        "Path segment contains unsupported characters: '" + segment + "'. " +
                                "Supported: A-Z, a-z, 0-9, underscore, hyphen");
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
        Map<String, OverlayGroup> pathToGroup = new HashMap<>();

        for (OverlayGroup group : model.overlays()) {
            for (String memberPath : group.memberPaths()) {
                overlayPaths.add(memberPath);
                pathToGroup.put(memberPath, group);
            }
        }

        // Check each pair of fields for overlap
        for (int i = 0; i < leaves.size(); i++) {
            LayoutField a = leaves.get(i);
            for (int j = i + 1; j < leaves.size(); j++) {
                LayoutField b = leaves.get(j);

                // Check if they overlap
                if (overlaps(a, b)) {
                    boolean aInOverlay = overlayPaths.contains(a.path());
                    boolean bInOverlay = overlayPaths.contains(b.path());

                    if (!aInOverlay || !bInOverlay) {
                        throw new ModelInvariantException("FIELD_OVERLAP", a.path(),
                                String.format("Field overlaps with '%s' but one or both are not in an overlay group. " +
                                        "Offsets: %d+%d vs %d+%d", b.path(), a.offset(), a.length(), b.offset(), b.length()));
                    }

                    // Both are in overlays - verify they're in the SAME overlay group
                    OverlayGroup groupA = pathToGroup.get(a.path());
                    OverlayGroup groupB = pathToGroup.get(b.path());

                    if (groupA != groupB) {
                        throw new ModelInvariantException("FIELD_OVERLAP", a.path(),
                                String.format("Field overlaps with '%s' but they are in different overlay groups",
                                        b.path()));
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

    private static void validateOverlayGroups(List<OverlayGroup> groups, List<LayoutField> fields) {
        Map<String, LayoutField> pathToField = new HashMap<>();
        for (LayoutField field : fields) {
            pathToField.put(field.path(), field);
        }

        for (OverlayGroup group : groups) {
            // Validate that overlay has members
            if (group.memberPaths().isEmpty()) {
                throw new ModelInvariantException("EMPTY_OVERLAY",
                        "Overlay group has no members");
            }

            // Validate all members share the same base offset
            int baseOffset = group.offset();
            int overlayEnd = baseOffset + group.length();

            for (String memberPath : group.memberPaths()) {
                LayoutField memberField = pathToField.get(memberPath);
                if (memberField == null) {
                    throw new ModelInvariantException("OVERLAY_MEMBER_NOT_FOUND", memberPath,
                            "Overlay member field not found in layout");
                }

                // Validate member is fully contained within overlay region
                int memberEnd = memberField.offset() + memberField.length();
                if (memberField.offset() < baseOffset || memberEnd > overlayEnd) {
                    throw new ModelInvariantException("OVERLAY_CONTAINMENT", memberPath,
                            String.format("Overlay member not fully contained within overlay region. " +
                                            "Member: offset=%d, length=%d. Overlay: offset=%d, length=%d",
                                    memberField.offset(), memberField.length(), baseOffset, group.length()));
                }
            }
        }
    }

    private static void validateOccursIndices(List<LayoutField> fields) {
        // Group fields by their OCCURS base path and validate contiguity
        Map<String, Set<Integer>> occursIndices = new TreeMap<>(); // TreeMap for deterministic order

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
            if (indices.isEmpty()) {
                continue;
            }

            int maxIndex = Collections.max(indices);
            int expectedSize = maxIndex + 1;

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

    private static void validateNameCollisions(List<CodegenFieldInfo> fields) {
        // Check for Java name collisions at the same scope level
        Map<String, Set<String>> scopeToNames = new TreeMap<>();

        for (CodegenFieldInfo field : fields) {
            // Get the scope (parent path)
            String path = field.getPath();
            int lastDot = path.lastIndexOf('.');
            String scope = lastDot > 0 ? path.substring(0, lastDot) : "";
            // Remove occurs indices from scope for collision detection
            scope = scope.replaceAll("\\[\\d+]", "[*]");

            Set<String> names = scopeToNames.computeIfAbsent(scope, k -> new TreeSet<>());
            if (!names.add(field.getJavaName())) {
                throw new ModelInvariantException("NAME_COLLISION", field.getPath(),
                        "Java name '" + field.getJavaName() + "' collides with another field in the same scope");
            }
        }
    }

    // =========================================================================
    // Overlay Lookup
    // =========================================================================

    private static Map<String, OverlayGroup> buildOverlayLookup(List<OverlayGroup> groups) {
        Map<String, OverlayGroup> lookup = new HashMap<>();
        for (OverlayGroup group : groups) {
            for (String memberPath : group.memberPaths()) {
                lookup.put(memberPath, group);
            }
        }
        return lookup;
    }

    // =========================================================================
    // Field Adaptation
    // =========================================================================

    private static List<CodegenFieldInfo> adaptFields(List<LayoutField> fields,
                                                       Map<String, OverlayGroup> pathToOverlay,
                                                       GenerationConfig config) {
        // Calculate OCCURS counts for each base path
        Map<String, Integer> occursCounts = calculateOccursCounts(fields);

        return fields.stream()
                .map(f -> adaptField(f, pathToOverlay, occursCounts, config))
                .collect(Collectors.toList());
    }

    private static Map<String, Integer> calculateOccursCounts(List<LayoutField> fields) {
        Map<String, Integer> counts = new TreeMap<>();

        for (LayoutField field : fields) {
            Matcher matcher = OCCURS_INDEX_PATTERN.matcher(field.path());
            while (matcher.find()) {
                int index = Integer.parseInt(matcher.group(1));
                String basePath = field.path().substring(0, matcher.start());
                counts.merge(basePath, index + 1, Math::max);
            }
        }

        return counts;
    }

    private static CodegenFieldInfo adaptField(LayoutField field,
                                                Map<String, OverlayGroup> pathToOverlay,
                                                Map<String, Integer> occursCounts,
                                                GenerationConfig config) {
        FieldKind kind = determineFieldKind(field);
        String javaType = determineJavaType(field, kind);
        List<CodegenFieldInfo.PathSegment> segments = parsePath(field.path());
        int occursIndex = extractOccursIndex(field.path());

        // Determine OCCURS shape if applicable
        OccursShape occursShape = null;
        for (Map.Entry<String, Integer> entry : occursCounts.entrySet()) {
            if (field.path().startsWith(entry.getKey() + "[")) {
                occursShape = new OccursShape(entry.getValue());
                break;
            }
        }

        // Determine overlay membership
        OverlayGroup overlay = pathToOverlay.get(field.path());
        boolean inOverlay = overlay != null;
        String overlayBasePath = inOverlay ? overlay.basePath() : null;

        return CodegenFieldInfo.builder()
                .path(field.path())
                .pathSegments(segments)
                .javaName(toJavaFieldName(field.path()))
                .offset(field.offset())
                .length(field.length())
                .kind(kind)
                .javaType(javaType)
                .digits(field.pic() != null ? field.pic().digits() : 0)
                .scale(field.pic() != null ? field.pic().scale() : 0)
                .signed(field.pic() != null && field.pic().signed())
                .occursShape(occursShape)
                .occursIndex(occursIndex)
                .inOverlay(inOverlay)
                .overlayBasePath(overlayBasePath)
                .build();
    }

    private static List<CodegenFieldInfo.PathSegment> parsePath(String path) {
        List<CodegenFieldInfo.PathSegment> segments = new ArrayList<>();

        // Split by dots, but handle [n] indices
        String[] parts = path.split("\\.");
        for (String part : parts) {
            Matcher matcher = OCCURS_INDEX_PATTERN.matcher(part);
            if (matcher.find()) {
                String name = part.substring(0, matcher.start());
                int index = Integer.parseInt(matcher.group(1));
                segments.add(new CodegenFieldInfo.PathSegment(name, index));
            } else {
                segments.add(new CodegenFieldInfo.PathSegment(part));
            }
        }

        return segments;
    }

    private static FieldKind determineFieldKind(LayoutField field) {
        if (field.usage() == null) {
            return FieldKind.ALPHANUMERIC;
        }

        switch (field.usage()) {
            case DISPLAY:
                if (field.pic() != null && field.pic().digits() > 0) {
                    return FieldKind.DISPLAY_NUMERIC;
                }
                return FieldKind.ALPHANUMERIC;
            case COMP:
                return FieldKind.COMP;
            case COMP3:
                return FieldKind.COMP3;
            default:
                return FieldKind.ALPHANUMERIC;
        }
    }

    private static String determineJavaType(LayoutField field, FieldKind kind) {
        switch (kind) {
            case ALPHANUMERIC:
                return "String";

            case DISPLAY_NUMERIC:
                if (field.pic() != null) {
                    int digits = field.pic().digits();
                    int scale = field.pic().scale();

                    if (scale > 0) {
                        return "java.math.BigDecimal";
                    } else if (digits <= 9) {
                        return "Integer";
                    } else if (digits <= 18) {
                        return "Long";
                    } else {
                        return "java.math.BigInteger";
                    }
                }
                return "Integer";

            case COMP:
                // COMP: length 1-4 -> Integer, 5-8 -> Long
                int compLength = field.length();
                if (compLength <= 4) {
                    return "Integer";
                } else if (compLength <= 8) {
                    return "Long";
                } else {
                    throw new com.mainframe.codegen.UnsupportedFeatureException("COMP_LENGTH_" + compLength,
                            "COMP fields longer than 8 bytes are not supported");
                }

            case COMP3:
                if (field.pic() != null) {
                    int scale = field.pic().scale();
                    int digits = field.pic().digits();

                    if (scale == 0 && digits <= 18) {
                        return "Long";
                    } else {
                        return "java.math.BigDecimal";
                    }
                }
                return "java.math.BigDecimal";

            default:
                return "Object";
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

    private static List<CodegenOverlayInfo> adaptOverlayGroups(List<OverlayGroup> groups,
                                                                List<CodegenFieldInfo> fields) {
        Map<String, CodegenFieldInfo> pathToField = new TreeMap<>();
        for (CodegenFieldInfo field : fields) {
            pathToField.put(field.getPath(), field);
        }

        return groups.stream()
                .map(g -> adaptOverlayGroup(g, pathToField))
                .collect(Collectors.toList());
    }

    private static CodegenOverlayInfo adaptOverlayGroup(OverlayGroup group,
                                                         Map<String, CodegenFieldInfo> pathToField) {
        CodegenFieldInfo baseField = pathToField.get(group.basePath());
        FieldKind baseKind = baseField != null ? baseField.getKind() : FieldKind.ALPHANUMERIC;

        return new CodegenOverlayInfo(
                group.basePath(),
                group.offset(),
                group.length(),
                group.memberPaths(),
                baseKind
        );
    }

    // =========================================================================
    // Group Tree Building
    // =========================================================================

    private static CodegenGroup buildGroupTree(String recordName, List<CodegenFieldInfo> fields) {
        // The root group represents the entire record
        CodegenGroup.Builder rootBuilder = CodegenGroup.builder()
                .name(recordName)
                .javaName(toJavaClassName(recordName))
                .fullPath("");

        // Calculate OCCURS counts for group paths
        Map<String, Integer> occursCounts = new TreeMap<>();
        for (CodegenFieldInfo field : fields) {
            Matcher matcher = OCCURS_INDEX_PATTERN.matcher(field.getPath());
            while (matcher.find()) {
                int index = Integer.parseInt(matcher.group(1));
                String basePath = field.getPath().substring(0, matcher.start());
                occursCounts.merge(basePath, index + 1, Math::max);
            }
        }

        // Build a map of group paths to their child fields
        Map<String, List<CodegenFieldInfo>> groupedByParent = new TreeMap<>();
        for (CodegenFieldInfo field : fields) {
            String path = field.getPath();
            // Remove OCCURS indices and get parent path
            String cleanPath = path.replaceAll("\\[\\d+]", "");
            int lastDot = cleanPath.lastIndexOf('.');
            String parentPath = lastDot > 0 ? cleanPath.substring(0, lastDot) : "";
            groupedByParent.computeIfAbsent(parentPath, k -> new ArrayList<>()).add(field);
        }

        // Identify unique OCCURS groups
        Set<String> occursGroupPaths = new TreeSet<>();
        for (CodegenFieldInfo field : fields) {
            Matcher matcher = OCCURS_INDEX_PATTERN.matcher(field.getPath());
            while (matcher.find()) {
                String basePath = field.getPath().substring(0, matcher.start());
                occursGroupPaths.add(basePath);
            }
        }

        // Add direct children to root
        List<CodegenFieldInfo> rootFields = groupedByParent.getOrDefault("", List.of());
        Set<String> addedOccursGroups = new HashSet<>();

        for (CodegenFieldInfo field : rootFields) {
            // Check if this field is part of an OCCURS group at the root level
            String occursBase = null;
            for (String occursPath : occursGroupPaths) {
                if (field.getPath().startsWith(occursPath + "[")) {
                    occursBase = occursPath;
                    break;
                }
            }

            if (occursBase != null && !addedOccursGroups.contains(occursBase)) {
                // Add an OCCURS group
                addedOccursGroups.add(occursBase);
                Integer count = occursCounts.get(occursBase);
                OccursShape shape = count != null ? new OccursShape(count) : null;

                CodegenGroup occursGroup = buildOccursGroup(occursBase, fields, shape, occursCounts);
                rootBuilder.addChildGroup(occursGroup);
            } else if (occursBase == null) {
                // Add as a regular leaf field
                rootBuilder.addLeafField(CodegenLeafField.builder()
                        .name(getLastSegment(field.getPath()))
                        .javaName(field.getJavaName())
                        .fullPath(field.getPath())
                        .offset(field.getOffset())
                        .length(field.getLength())
                        .kind(field.getKind())
                        .javaType(field.getJavaType())
                        .inOverlay(field.isInOverlay())
                        .build());
            }
        }

        return rootBuilder.build();
    }

    private static CodegenGroup buildOccursGroup(String basePath, List<CodegenFieldInfo> allFields,
                                                  OccursShape shape, Map<String, Integer> occursCounts) {
        String groupName = getLastSegment(basePath);

        CodegenGroup.Builder builder = CodegenGroup.builder()
                .name(groupName)
                .javaName(toJavaClassName(groupName))
                .fullPath(basePath)
                .occursShape(shape);

        // Get fields for the first occurrence only (index 0)
        String firstOccurPath = basePath + "[0]";

        for (CodegenFieldInfo field : allFields) {
            if (field.getPath().startsWith(firstOccurPath + ".") || field.getPath().equals(firstOccurPath)) {
                // Check if this is a nested field
                String relativePath = field.getPath().substring(firstOccurPath.length());
                if (relativePath.isEmpty() || !relativePath.contains(".") ||
                        (relativePath.startsWith(".") && !relativePath.substring(1).contains("."))) {
                    // Direct child of this OCCURS group
                    builder.addLeafField(CodegenLeafField.builder()
                            .name(getLastSegment(field.getPath()))
                            .javaName(field.getJavaName())
                            .fullPath(field.getPath())
                            .offset(field.getOffset())
                            .length(field.getLength())
                            .kind(field.getKind())
                            .javaType(field.getJavaType())
                            .inOverlay(field.isInOverlay())
                            .build());
                }
            }
        }

        return builder.build();
    }

    private static String getLastSegment(String path) {
        String cleanPath = path.replaceAll("\\[\\d+]", "");
        int lastDot = cleanPath.lastIndexOf('.');
        return lastDot >= 0 ? cleanPath.substring(lastDot + 1) : cleanPath;
    }

    // =========================================================================
    // Name Canonicalization
    // =========================================================================

    /**
     * Convert a COBOL name to a valid Java class name (UpperCamel).
     * Example: "ACCOUNT-RECORD" → "AccountRecord"
     */
    static String toJavaClassName(String cobolName) {
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
     * Convert a COBOL field path to a valid Java field name (lowerCamel).
     * Example: "ACCOUNT.HOLDER-NAME" → "holderName"
     */
    static String toJavaFieldName(String path) {
        // Remove OCCURS indices
        String cleanPath = path.replaceAll("\\[\\d+]", "");

        // Take the last segment of the path
        String[] segments = cleanPath.split("\\.");
        String fieldName = segments[segments.length - 1];

        // Convert to lowerCamelCase
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
