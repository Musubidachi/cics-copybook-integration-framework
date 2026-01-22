package com.mainframe.model;

import com.mainframe.copybook.parser.CopybookParser;
import com.mainframe.copybook.parser.ast.CopybookAst;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration-style tests that validate copybook-model can consume the
 * AST produced by copybook-parser (via {@link LayoutEngine#build(CopybookAst)}).
 *
 * <p>These tests intentionally parse short inlined copybooks rather than
 * constructing the model AST manually. This ensures the adapter boundary is
 * exercised and catches drift between parser output and model expectations.</p>
 */
class LayoutEngineIntegrationTest {

    private static CopybookAst parse(String src) {
        // The parser returns diagnostics for invalid input; the adapter refuses to proceed
        // when any diagnostics exist, so tests should use valid copybooks only.
        return CopybookParser.parseString(src);
    }

    @Test
    void computesOffsetsForFlatAndNestedGroups() {
        String copybook = """
                000100 01 REC.
                000200 05 A PIC X(10).
                000300 05 B PIC 9(5).
                000400 05 GRP.
                000500 10 C PIC X(3).
                000600 10 D PIC X(2).
                """;

        LayoutModel model = new LayoutEngine().build(parse(copybook));

        List<LayoutField> fields = model.fields();
        assertEquals(4, fields.size());

        assertField(fields.get(0), "REC.A", 0, 10);
        assertField(fields.get(1), "REC.B", 10, 5);
        assertField(fields.get(2), "REC.GRP.C", 15, 3);
        assertField(fields.get(3), "REC.GRP.D", 18, 2);

        assertEquals(20, model.totalLength());
        assertTrue(model.overlays().isEmpty());
    }

    @Test
    void expandsOccursAndComputesSequentialOffsets() {
        String copybook = """
                000100 01 REC.
                000200 05 ITEMS OCCURS 3.
                000300 10 ITEM PIC 9(2).
                """;

        LayoutModel model = new LayoutEngine().build(parse(copybook));
        List<LayoutField> fields = model.fields();
        assertEquals(3, fields.size());

        assertField(fields.get(0), "REC.ITEMS[0].ITEM", 0, 2);
        assertField(fields.get(1), "REC.ITEMS[1].ITEM", 2, 2);
        assertField(fields.get(2), "REC.ITEMS[2].ITEM", 4, 2);
        assertEquals(6, model.totalLength());
    }

    @Test
    void handlesRedefinesAsOverlayWithoutAdvancingTotalLength() {
        String copybook = """
                000100 01 REC.
                000200 05 A PIC X(10).
                000300 05 B REDEFINES A PIC 9(5).
                """;

        LayoutModel model = new LayoutEngine().build(parse(copybook));
        List<LayoutField> fields = model.fields();
        assertEquals(2, fields.size());

        LayoutField a = fields.get(0);
        LayoutField b = fields.get(1);

        assertField(a, "REC.A", 0, 10);
        assertField(b, "REC.B", 0, 5);
        assertEquals(10, model.totalLength(), "REDEFINES must not increase record length");

        assertEquals(1, model.overlays().size());
        OverlayGroup group = model.overlays().get(0);
        assertEquals("REC.A", group.basePath());
        assertEquals(0, group.offset());
        assertEquals(10, group.length(), "overlay group length should be max member length");
        assertEquals(List.of("REC.A", "REC.B"), group.memberPaths());
    }

    @Test
    void computesPackedDecimalComp3ByteLength() {
        String copybook = """
                000100 01 REC.
                000200 05 AMT PIC S9(7)V99 COMP-3.
                """;

        LayoutModel model = new LayoutEngine().build(parse(copybook));
        List<LayoutField> fields = model.fields();
        assertEquals(1, fields.size());

        LayoutField amt = fields.get(0);
        assertField(amt, "REC.AMT", 0, 5);
        assertEquals(Usage.COMP3, amt.usage());
        assertEquals(new PicSummary(9, 2, true), amt.pic());
        assertEquals(5, model.totalLength());
    }

    @Test
    void signedDisplayAddsAByte() {
        String copybook = """
                000100 01 REC.
                000200 05 SVAL PIC S9(3).
                """;

        LayoutModel model = new LayoutEngine().build(parse(copybook));
        LayoutField f = model.fields().get(0);
        assertField(f, "REC.SVAL", 0, 4);
        assertEquals(Usage.DISPLAY, f.usage());
        assertEquals(new PicSummary(3, 0, true), f.pic());
        assertEquals(4, model.totalLength());
    }

    private static void assertField(LayoutField f, String path, int offset, int length) {
        assertEquals(path, f.path());
        assertEquals(offset, f.offset());
        assertEquals(length, f.length());
    }
}
