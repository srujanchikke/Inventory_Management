       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MANAGEMENT-SYSTEM.
       DATE-WRITTEN. 2026-06-04.
      *
      * Stock register for the shop counter.
      * Everything is kept in memory for one session.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY. FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-STATE.
           05 WS-RUNNING               PIC X VALUE 'Y'.
           05 WS-MENU-CHOICE           PIC X(2) VALUE SPACES.
           05 WS-INDEX                 PIC 9(4) COMP-5 VALUE 0.
           05 WS-INDEX-2               PIC 9(4) COMP-5 VALUE 0.
           05 WS-ID-FOUND              PIC X VALUE 'N'.
              88 PRODUCT-FOUND         VALUE 'Y'.
           05 WS-SELECTED-ID           PIC 9(6) VALUE 0.
           05 WS-SELECTED-INDEX        PIC 9(4) COMP-5 VALUE 0.
           05 WS-NEXT-PRODUCT-ID       PIC 9(6) VALUE 1.
           05 WS-INPUT-PRICE           PIC 9(7)V99 VALUE 0.
           05 WS-INPUT-QUANTITY        PIC S9(7) COMP-5 VALUE 0.
           05 WS-TEMP-NAME             PIC X(30) VALUE SPACES.
           05 WS-TEMP-CATEGORY         PIC X(20) VALUE SPACES.
           05 WS-TEMP-SUPPLIER         PIC X(30) VALUE SPACES.
           05 WS-REPORT-COUNT          PIC 9(4) COMP-5 VALUE 0.
           05 WS-CURRENCY              PIC X(3) VALUE 'INR'.
       01  WS-DISPLAY-FIELDS.
           05 WS-DISPLAY-ID            PIC ZZZZZ9.
           05 WS-DISPLAY-QUANTITY      PIC ZZZZZZZ9.
           05 WS-DISPLAY-AMOUNT        PIC Z,ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-COUNT         PIC ZZZZ9.
       01  PRODUCT-TABLE.
           05 PRODUCT-ENTRY OCCURS 200 TIMES.
              10 PRODUCT-ID            PIC 9(6).
              10 PRODUCT-NAME          PIC X(30).
              10 PRODUCT-CATEGORY      PIC X(20).
              10 PRODUCT-SUPPLIER      PIC X(30).
              10 PRODUCT-QUANTITY      PIC S9(7) COMP-5.
              10 PRODUCT-UNIT-PRICE    PIC 9(7)V99.
              10 PRODUCT-REORDER-LEVEL PIC 9(7).
              10 PRODUCT-ACTIVE        PIC X.
       01  WS-TABLE-COUNTERS.
           05 WS-PRODUCT-COUNT         PIC 9(4) COMP-5 VALUE 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INITIALIZE-SYSTEM
           PERFORM UNTIL WS-RUNNING NOT = 'Y'
               PERFORM DISPLAY-MAIN-MENU
               PERFORM ACCEPT-MENU-CHOICE
               PERFORM PROCESS-MENU-CHOICE
           END-PERFORM
           PERFORM SHUTDOWN-SYSTEM
           STOP RUN.

      * Start of session - empty tables and print the banner.
       INITIALIZE-SYSTEM.
           MOVE 0 TO WS-PRODUCT-COUNT
           MOVE 1 TO WS-NEXT-PRODUCT-ID
           PERFORM CLEAR-PRODUCT-TABLE
           PERFORM LOAD-SAMPLE-DATA
           DISPLAY ' '
           DISPLAY '   INVENTORY MANAGEMENT SYSTEM'
           DISPLAY '========================================'
           DISPLAY 'Sample data has been loaded.'
           DISPLAY ' '.

       CLEAR-PRODUCT-TABLE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 200
               INITIALIZE PRODUCT-ENTRY(WS-INDEX)
               MOVE 'N' TO PRODUCT-ACTIVE(WS-INDEX)
           END-PERFORM.

      * A few items so the menus have something to show while testing.
       LOAD-SAMPLE-DATA.
           MOVE 'Notebook - Ruled' TO WS-TEMP-NAME
           MOVE 'Stationery' TO WS-TEMP-CATEGORY
           MOVE 'Supplier A' TO WS-TEMP-SUPPLIER
           MOVE 120 TO WS-INPUT-QUANTITY
           MOVE 45.00 TO WS-INPUT-PRICE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Ball Pen - Blue' TO WS-TEMP-NAME
           MOVE 'Stationery' TO WS-TEMP-CATEGORY
           MOVE 'Supplier A' TO WS-TEMP-SUPPLIER
           MOVE 250 TO WS-INPUT-QUANTITY
           MOVE 10.00 TO WS-INPUT-PRICE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'USB-C Cable' TO WS-TEMP-NAME
           MOVE 'Electronics' TO WS-TEMP-CATEGORY
           MOVE 'Supplier B' TO WS-TEMP-SUPPLIER
           MOVE 35 TO WS-INPUT-QUANTITY
           MOVE 199.00 TO WS-INPUT-PRICE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Wireless Mouse' TO WS-TEMP-NAME
           MOVE 'Electronics' TO WS-TEMP-CATEGORY
           MOVE 'Supplier B' TO WS-TEMP-SUPPLIER
           MOVE 18 TO WS-INPUT-QUANTITY
           MOVE 649.00 TO WS-INPUT-PRICE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Bath Soap (Pack of 4)' TO WS-TEMP-NAME
           MOVE 'Household' TO WS-TEMP-CATEGORY
           MOVE 'Supplier C' TO WS-TEMP-SUPPLIER
           MOVE 42 TO WS-INPUT-QUANTITY
           MOVE 156.00 TO WS-INPUT-PRICE
           PERFORM ADD-PRODUCT-FROM-TEMP.

       DISPLAY-MAIN-MENU.
           DISPLAY ' '
           DISPLAY '-------------- MAIN MENU --------------'
           DISPLAY '1. List all products'
           DISPLAY '2. Search product by ID'
           DISPLAY '3. Add a product'
           DISPLAY '4. Update product details'
           DISPLAY '0. Exit'
           DISPLAY '---------------------------------------'.

       ACCEPT-MENU-CHOICE.
           MOVE SPACES TO WS-MENU-CHOICE
           DISPLAY 'Enter choice: ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE.

       PROCESS-MENU-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN '1' PERFORM LIST-PRODUCTS
               WHEN '2' PERFORM SEARCH-PRODUCT
               WHEN '3' PERFORM ADD-PRODUCT
               WHEN '4' PERFORM UPD-PRODUCT
               WHEN '0' MOVE 'N' TO WS-RUNNING
               WHEN OTHER
                   DISPLAY 'Invalid menu choice. Please try again.'
           END-EVALUATE.

      * Prints every active product in one table.
       LIST-PRODUCTS.
           DISPLAY ' '
           DISPLAY 'PRODUCT INVENTORY'
           DISPLAY 'ID     NAME                           QTY'
               '         PRICE'
           DISPLAY '------ ------------------------------'
               ' --------- ---------'
           MOVE 0 TO WS-REPORT-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-PRODUCT-COUNT
               IF PRODUCT-ACTIVE(WS-INDEX) = 'Y'
                   PERFORM DISPLAY-ONE-PRODUCT
                   ADD 1 TO WS-REPORT-COUNT
               END-IF
           END-PERFORM
           MOVE WS-REPORT-COUNT TO WS-DISPLAY-COUNT
           DISPLAY 'Active products: ' WS-DISPLAY-COUNT.

       DISPLAY-ONE-PRODUCT.
           MOVE PRODUCT-ID(WS-INDEX) TO WS-DISPLAY-ID
           MOVE PRODUCT-QUANTITY(WS-INDEX) TO WS-DISPLAY-QUANTITY
           MOVE PRODUCT-UNIT-PRICE(WS-INDEX) TO WS-DISPLAY-AMOUNT
           DISPLAY WS-DISPLAY-ID ' '
               PRODUCT-NAME(WS-INDEX) ' '
               WS-DISPLAY-QUANTITY ' '
               WS-DISPLAY-AMOUNT.

      * Shows all details of one product.
       SEARCH-PRODUCT.
           PERFORM ACCEPT-PRODUCT-ID
           PERFORM FIND-PRODUCT-BY-ID
           IF WS-ID-FOUND = 'Y'
               DISPLAY 'Product found:'
               DISPLAY 'ID: ' PRODUCT-ID(WS-SELECTED-INDEX)
               DISPLAY 'Name: ' PRODUCT-NAME(WS-SELECTED-INDEX)
               DISPLAY 'Category: ' PRODUCT-CATEGORY(WS-SELECTED-INDEX)
               DISPLAY 'Supplier: ' PRODUCT-SUPPLIER(WS-SELECTED-INDEX)
               DISPLAY 'Quantity: ' PRODUCT-QUANTITY(WS-SELECTED-INDEX)
               DISPLAY 'Unit price: '
                   PRODUCT-UNIT-PRICE(WS-SELECTED-INDEX)
               DISPLAY 'Reorder level: '
                   PRODUCT-REORDER-LEVEL(WS-SELECTED-INDEX)
           ELSE
               DISPLAY 'No active product exists with that ID.'
           END-IF.

       ACCEPT-PRODUCT-ID.
           MOVE 0 TO WS-SELECTED-ID
           DISPLAY 'Enter product ID: ' WITH NO ADVANCING
           ACCEPT WS-SELECTED-ID.

      * Sets WS-ID-FOUND and WS-SELECTED-INDEX for the entered ID.
       FIND-PRODUCT-BY-ID.
           MOVE 'N' TO WS-ID-FOUND
           MOVE 0 TO WS-SELECTED-INDEX
           PERFORM VARYING WS-INDEX-2 FROM 1 BY 1
               UNTIL WS-INDEX-2 > WS-PRODUCT-COUNT
               IF PRODUCT-ACTIVE(WS-INDEX-2) = 'Y'
                   AND PRODUCT-ID(WS-INDEX-2) = WS-SELECTED-ID
                   MOVE 'Y' TO WS-ID-FOUND
                   MOVE WS-INDEX-2 TO WS-SELECTED-INDEX
               END-IF
           END-PERFORM.

      * Asks for each field, then reuses ADD-PRODUCT-FROM-TEMP.
       ADD-PRODUCT.
           IF WS-PRODUCT-COUNT >= 200
               DISPLAY 'Product table is full.'
           ELSE
               PERFORM ACCEPT-PRODUCT-NAME
               PERFORM ACCEPT-PRODUCT-CATEGORY
               PERFORM ACCEPT-PRODUCT-SUPPLIER
               PERFORM ACCEPT-INITIAL-STOCK
               PERFORM ACCEPT-UNIT-PRICE
               PERFORM ADD-PRODUCT-FROM-TEMP
               DISPLAY 'Product added successfully.'
           END-IF.

       ACCEPT-PRODUCT-NAME.
           MOVE SPACES TO WS-TEMP-NAME
           DISPLAY 'Product name: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-NAME
           IF WS-TEMP-NAME = SPACES
               MOVE 'UNKNOWN PRODUCT' TO WS-TEMP-NAME
           END-IF.

       ACCEPT-PRODUCT-CATEGORY.
           MOVE SPACES TO WS-TEMP-CATEGORY
           DISPLAY 'Category: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-CATEGORY
           IF WS-TEMP-CATEGORY = SPACES
               MOVE 'GENERAL' TO WS-TEMP-CATEGORY
           END-IF.

       ACCEPT-PRODUCT-SUPPLIER.
           MOVE SPACES TO WS-TEMP-SUPPLIER
           DISPLAY 'Supplier: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-SUPPLIER
           IF WS-TEMP-SUPPLIER = SPACES
               MOVE 'UNSPECIFIED' TO WS-TEMP-SUPPLIER
           END-IF.

       ACCEPT-INITIAL-STOCK.
           MOVE 0 TO WS-INPUT-QUANTITY
           DISPLAY 'Initial quantity: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-QUANTITY
           IF WS-INPUT-QUANTITY < 0
               MOVE 0 TO WS-INPUT-QUANTITY
           END-IF.

       ACCEPT-UNIT-PRICE.
           MOVE 0 TO WS-INPUT-PRICE
           DISPLAY 'Unit price: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-PRICE.

      * Copies the WS-TEMP / WS-INPUT fields into the next free row.
       ADD-PRODUCT-FROM-TEMP.
           IF WS-PRODUCT-COUNT < 200
               ADD 1 TO WS-PRODUCT-COUNT
               MOVE WS-NEXT-PRODUCT-ID TO
                   PRODUCT-ID(WS-PRODUCT-COUNT)
               MOVE WS-TEMP-NAME TO PRODUCT-NAME(WS-PRODUCT-COUNT)
               MOVE WS-TEMP-CATEGORY TO
                   PRODUCT-CATEGORY(WS-PRODUCT-COUNT)
               MOVE WS-TEMP-SUPPLIER TO
                   PRODUCT-SUPPLIER(WS-PRODUCT-COUNT)
               MOVE WS-INPUT-QUANTITY TO
                   PRODUCT-QUANTITY(WS-PRODUCT-COUNT)
               MOVE WS-INPUT-PRICE TO
                   PRODUCT-UNIT-PRICE(WS-PRODUCT-COUNT)
               MOVE 5 TO PRODUCT-REORDER-LEVEL(WS-PRODUCT-COUNT)
               MOVE 'Y' TO PRODUCT-ACTIVE(WS-PRODUCT-COUNT)
               ADD 1 TO WS-NEXT-PRODUCT-ID
           END-IF.

       SHUTDOWN-SYSTEM.
           DISPLAY ' '
           DISPLAY 'Session ended.'.

       UPD-PRODUCT.
           PERFORM ACCEPT-PRODUCT-ID
           PERFORM FIND-PRODUCT-BY-ID
           IF NOT PRODUCT-FOUND
               DISPLAY 'Product ID was not found.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'Leave blank (or 0) to keep the current value.'
           MOVE SPACES TO WS-TEMP-NAME
           DISPLAY 'New name: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-NAME
           IF WS-TEMP-NAME NOT = SPACES
               MOVE WS-TEMP-NAME TO PRODUCT-NAME(WS-SELECTED-INDEX)
           END-IF
           MOVE SPACES TO WS-TEMP-CATEGORY
           DISPLAY 'New category: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-CATEGORY
           IF WS-TEMP-CATEGORY NOT = SPACES
               MOVE WS-TEMP-CATEGORY
                 TO PRODUCT-CATEGORY(WS-SELECTED-INDEX)
           END-IF
           MOVE SPACES TO WS-TEMP-SUPPLIER
           DISPLAY 'New supplier: ' WITH NO ADVANCING
           ACCEPT WS-TEMP-SUPPLIER
           IF WS-TEMP-SUPPLIER NOT = SPACES
               MOVE WS-TEMP-SUPPLIER
                 TO PRODUCT-SUPPLIER(WS-SELECTED-INDEX)
           END-IF
           MOVE 0 TO WS-INPUT-PRICE
           DISPLAY 'New unit price: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-PRICE
           IF WS-INPUT-PRICE > 0
               MOVE WS-INPUT-PRICE
                 TO PRODUCT-UNIT-PRICE(WS-SELECTED-INDEX)
           END-IF
      *> reorder level: 0 = keep, negative is rejected
           MOVE 0 TO WS-INPUT-QUANTITY
           DISPLAY 'New reorder level: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-QUANTITY
           EVALUATE TRUE
               WHEN WS-INPUT-QUANTITY < 0
                   DISPLAY 'Reorder level cannot be negative, kept.'
               WHEN WS-INPUT-QUANTITY > 0
                   MOVE WS-INPUT-QUANTITY
                     TO PRODUCT-REORDER-LEVEL(WS-SELECTED-INDEX)
           END-EVALUATE
           DISPLAY 'Product ' WS-SELECTED-ID ' updated.'.
