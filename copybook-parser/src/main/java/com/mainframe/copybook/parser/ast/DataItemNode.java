package com.mainframe.copybook.parser.ast;

import com.mainframe.copybook.parser.OccursClause;
import com.mainframe.copybook.parser.RedefinesClause;
import com.mainframe.copybook.parser.SourceSpan;
import com.mainframe.copybook.parser.Usage;
import com.mainframe.copybook.parser.pic.PicClause;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a group or elementary data description entry in a copybook.  Each
 * DataItemNode may have child nodes if it is a group (i.e. its level number
 * is less than the level numbers of following entries).  Fields such as PIC,
 * USAGE, OCCURS and REDEFINES are optional; absent values are represented
 * by nulls.
 *
 * @param level      the numeric level of the item (01–49)
 * @param name       the data name identifier
 * @param pic        the parsed PIC clause or null
 * @param usage      the USAGE clause or null
 * @param occurs     the OCCURS clause or null
 * @param redefines  the REDEFINES clause or null
 * @param children   mutable list of child items (will be wrapped as unmodifiable in constructor)
 * @param span       the source span covering this entry
 */
public record DataItemNode(
        int level,
        String name,
        PicClause pic,
        Usage usage,
        OccursClause occurs,
        RedefinesClause redefines,
        List<DataItemNode> children,
        SourceSpan span
    ) implements AstNode {
    public DataItemNode {
        // Defensive copy: always wrap children into an unmodifiable list.  If null,
        // replace with an empty unmodifiable list.
        if (children == null) {
            children = Collections.emptyList();
        } else {
            children = Collections.unmodifiableList(new ArrayList<>(children));
        }
    }
}