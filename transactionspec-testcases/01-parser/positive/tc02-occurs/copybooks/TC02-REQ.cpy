       01 TC02-REQ-REC.
          05 REQ-COUNT             PIC 9(2).
          05 REQ-ITEMS OCCURS 3 TIMES.
             10 REQ-ITEM-ID        PIC X(4).
             10 REQ-ITEM-QTY       PIC 9(3).
