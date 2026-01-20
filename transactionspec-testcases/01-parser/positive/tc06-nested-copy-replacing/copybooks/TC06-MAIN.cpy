       01 TC06-MAIN-REC.
          05 TC06-HEADER           PIC X(10).
          COPY TC06-NESTED REPLACING ==:PREFIX:== BY ==TC06==.
          05 TC06-FOOTER           PIC X(5).
