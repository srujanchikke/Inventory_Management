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
           05 WS-REPORT-VALUE          PIC 9(11)V99 VALUE 0.
           05 WS-CURRENCY              PIC X(3) VALUE 'INR'.
           05 WS-NEXT-TRANSACTION-ID   PIC 9(8) VALUE 1.
           05 WS-TRANSACTION-MODE      PIC X(8) VALUE SPACES.
           05 WS-QUANTITY              PIC S9(7) COMP-5 VALUE 0.
              88 IS-BULK-SALE          VALUE 50 THRU 9999999.
           05 WS-UNIT-PRICE            PIC 9(7)V99 VALUE 0.
           05 WS-TOTAL                 PIC 9(9)V99 VALUE 0.
           05 WS-GRAND-TOTAL           PIC 9(11)V99 VALUE 0.
           05 WS-TAX-AMOUNT            PIC 9(9)V99 VALUE 0.
           05 WS-GST-PCT               PIC 99 VALUE 0.
              88 VALID-GST-SLAB        VALUES 0 5 18.
           05 WS-GST-RATE              PIC 9V99 VALUE 0.
           05 WS-DISCOUNT-RATE         PIC 9V99 VALUE 0.
           05 WS-DISCOUNT-AMOUNT       PIC 9(9)V99 VALUE 0.
           05 WS-BULK-DISC-RATE        PIC 9V99 VALUE 0.05. *> 50+ qty
           05 WS-DATE-TEXT             PIC X(8) VALUE SPACES.
       01  WS-DISPLAY-FIELDS.
           05 WS-DISPLAY-ID            PIC ZZZZZ9.
           05 WS-DISPLAY-QUANTITY      PIC ZZZZZZZ9.
           05 WS-DISPLAY-AMOUNT        PIC Z,ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-TAX           PIC Z,ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-DISCOUNT      PIC Z,ZZZ,ZZZ,ZZ9.99.
           05 WS-DISPLAY-COUNT         PIC ZZZZ9.
      *> narrower pictures so a history row fits on one line
       01  WS-HIST-FIELDS.
           05 WS-H-QTY                 PIC -(6)9.
           05 WS-H-AMT                 PIC Z(7)9.99.
           05 WS-H-GST                 PIC Z(6)9.99.
           05 WS-H-DISC                PIC Z(6)9.99.
       01  WS-REPORT-DETAILS.
           05 WS-REPORT-TITLE          PIC X(50) VALUE SPACES.
           05 WS-REPORT-LINE           PIC X(78) VALUE ALL '='.
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
              10 PRODUCT-SOLD-COUNT    PIC 9(9) COMP-5.
              10 PRODUCT-RETURN-COUNT  PIC 9(9) COMP-5.
              10 PRODUCT-GST-RATE      PIC 9V99.
      * One row per stock movement (receipt for now, sales later).
       01  TRANSACTION-TABLE.
           05 TRANSACTION-ENTRY OCCURS 1000 TIMES.
              10 TRANSACTION-ID        PIC 9(8).
              10 TRANSACTION-PRODUCT   PIC 9(6).
              10 TRANSACTION-TYPE      PIC X(8).
              10 TRANSACTION-QUANTITY  PIC S9(7) COMP-5.
              10 TRANSACTION-AMOUNT    PIC 9(9)V99.
              10 TRANSACTION-GST       PIC 9(9)V99.
              10 TRANSACTION-DISCOUNT  PIC 9(9)V99.
              10 TRANSACTION-DATE      PIC X(10).
              10 TRANSACTION-STATUS    PIC X.
       01  WS-TABLE-COUNTERS.
           05 WS-PRODUCT-COUNT         PIC 9(4) COMP-5 VALUE 0.
           05 WS-TRANSACTION-COUNT     PIC 9(4) COMP-5 VALUE 0.
           05 WS-TRANSACTION-LIMIT     PIC 9(4) COMP-5 VALUE 1000.
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
           MOVE 0 TO WS-TRANSACTION-COUNT
           MOVE 1 TO WS-NEXT-PRODUCT-ID
           MOVE 1 TO WS-NEXT-TRANSACTION-ID
           PERFORM CLEAR-PRODUCT-TABLE
           PERFORM CLEAR-TRANSACTION-TABLE
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

       CLEAR-TRANSACTION-TABLE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 1000
               INITIALIZE TRANSACTION-ENTRY(WS-INDEX)
           END-PERFORM.

      * A few items so the menus have something to show while testing.
       LOAD-SAMPLE-DATA.
           MOVE 'Notebook - Ruled' TO WS-TEMP-NAME
           MOVE 'Stationery' TO WS-TEMP-CATEGORY
           MOVE 'Supplier A' TO WS-TEMP-SUPPLIER
           MOVE 120 TO WS-INPUT-QUANTITY
           MOVE 45.00 TO WS-INPUT-PRICE
           MOVE 0 TO WS-GST-RATE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Ball Pen - Blue' TO WS-TEMP-NAME
           MOVE 'Stationery' TO WS-TEMP-CATEGORY
           MOVE 'Supplier A' TO WS-TEMP-SUPPLIER
           MOVE 250 TO WS-INPUT-QUANTITY
           MOVE 10.00 TO WS-INPUT-PRICE
           MOVE 0.18 TO WS-GST-RATE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'USB-C Cable' TO WS-TEMP-NAME
           MOVE 'Electronics' TO WS-TEMP-CATEGORY
           MOVE 'Supplier B' TO WS-TEMP-SUPPLIER
           MOVE 35 TO WS-INPUT-QUANTITY
           MOVE 199.00 TO WS-INPUT-PRICE
           MOVE 0.18 TO WS-GST-RATE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Wireless Mouse' TO WS-TEMP-NAME
           MOVE 'Electronics' TO WS-TEMP-CATEGORY
           MOVE 'Supplier B' TO WS-TEMP-SUPPLIER
           MOVE 18 TO WS-INPUT-QUANTITY
           MOVE 649.00 TO WS-INPUT-PRICE
           MOVE 0.18 TO WS-GST-RATE
           PERFORM ADD-PRODUCT-FROM-TEMP
           MOVE 'Bath Soap (Pack of 4)' TO WS-TEMP-NAME
           MOVE 'Household' TO WS-TEMP-CATEGORY
           MOVE 'Supplier C' TO WS-TEMP-SUPPLIER
           MOVE 42 TO WS-INPUT-QUANTITY
           MOVE 156.00 TO WS-INPUT-PRICE
           MOVE 0.05 TO WS-GST-RATE
           PERFORM ADD-PRODUCT-FROM-TEMP.

       DISPLAY-MAIN-MENU.
           DISPLAY ' '
           DISPLAY '-------------- MAIN MENU --------------'
           DISPLAY '1. List all products'
           DISPLAY '2. Search product by ID'
           DISPLAY '3. Add a product'
           DISPLAY '4. Update product details'
           DISPLAY '5. Receive stock'
           DISPLAY '6. Record a sale'
           DISPLAY '7. Process a return'
           DISPLAY '8. Low-stock report'
           DISPLAY '9. Inventory valuation'
           DISPLAY '10. Transaction history'
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
               WHEN '5' PERFORM RECEIVE-STOCK
               WHEN '6' PERFORM RECORD-SALE
               WHEN '7' PERFORM PROCESS-RETURN
               WHEN '8' PERFORM LOW-STOCK-REPORT
               WHEN '9' PERFORM INVENTORY-VALUATION
               WHEN '10' PERFORM DISPLAY-TRANSACTION-HISTORY
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
               COMPUTE WS-GST-PCT =
                   PRODUCT-GST-RATE(WS-SELECTED-INDEX) * 100
               DISPLAY 'GST %: ' WS-GST-PCT
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
               PERFORM GET-GST-RATE
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

       GET-GST-RATE.
      *> slabs after the Sept 2025 rate cut: 0 / 5 / 18
      *> (40% is only sin goods - we don't stock any)
           PERFORM WITH TEST AFTER UNTIL VALID-GST-SLAB
               MOVE 0 TO WS-GST-PCT
               DISPLAY 'GST % (0/5/18): ' WITH NO ADVANCING
               ACCEPT WS-GST-PCT
               IF NOT VALID-GST-SLAB
                   DISPLAY 'Not a GST slab, try again.'
               END-IF
           END-PERFORM
           COMPUTE WS-GST-RATE = WS-GST-PCT / 100.

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
               MOVE 0 TO PRODUCT-SOLD-COUNT(WS-PRODUCT-COUNT)
               MOVE 0 TO PRODUCT-RETURN-COUNT(WS-PRODUCT-COUNT)
               MOVE WS-GST-RATE TO PRODUCT-GST-RATE(WS-PRODUCT-COUNT)
               ADD 1 TO WS-NEXT-PRODUCT-ID
           END-IF.

      * Goods coming in from a supplier.
       RECEIVE-STOCK.
           PERFORM ACCEPT-PRODUCT-ID
           PERFORM FIND-PRODUCT-BY-ID
           IF WS-ID-FOUND = 'Y'
               IF WS-TRANSACTION-COUNT >= WS-TRANSACTION-LIMIT
                   DISPLAY 'Transaction history full;'
                       ' receipt cancelled.'
               ELSE
                   PERFORM ACCEPT-RECEIVED-QUANTITY
                   IF WS-INPUT-QUANTITY > 0
                       MOVE WS-INPUT-QUANTITY TO WS-QUANTITY
                       MOVE PRODUCT-UNIT-PRICE(WS-SELECTED-INDEX)
                           TO WS-UNIT-PRICE
                       COMPUTE WS-TOTAL ROUNDED =
                           WS-QUANTITY * WS-UNIT-PRICE
                       MOVE 0 TO WS-TAX-AMOUNT    *> no GST on inward
                       MOVE 0 TO WS-DISCOUNT-AMOUNT
                       ADD WS-INPUT-QUANTITY TO
                           PRODUCT-QUANTITY(WS-SELECTED-INDEX)
                       MOVE 'RECEIPT' TO WS-TRANSACTION-MODE
                       PERFORM WRITE-TRANSACTION
                       DISPLAY 'Stock received successfully.'
                   ELSE
                       DISPLAY 'Quantity must be greater than zero.'
                   END-IF
               END-IF
           ELSE
               DISPLAY 'Product ID was not found.'
           END-IF.

       ACCEPT-RECEIVED-QUANTITY.
           MOVE 0 TO WS-INPUT-QUANTITY
           DISPLAY 'Quantity received: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-QUANTITY.

      * Counter sale - checks stock, logs it and prints a receipt.
       RECORD-SALE.
           PERFORM ACCEPT-PRODUCT-ID
           PERFORM FIND-PRODUCT-BY-ID
           IF WS-ID-FOUND = 'Y'
               IF WS-TRANSACTION-COUNT >= WS-TRANSACTION-LIMIT
                   DISPLAY 'Transaction history full; sale cancelled.'
               ELSE
                   PERFORM ACCEPT-SALE-QUANTITY
                   IF WS-INPUT-QUANTITY > 0
                       IF PRODUCT-QUANTITY(WS-SELECTED-INDEX)
                           >= WS-INPUT-QUANTITY
                           PERFORM CALCULATE-SALE-TOTAL
                           PERFORM APPLY-SALE-TO-STOCK
                           MOVE 'SALE' TO WS-TRANSACTION-MODE
                           PERFORM WRITE-TRANSACTION
                           PERFORM DISPLAY-SALE-RECEIPT
                       ELSE
                           DISPLAY 'Insufficient stock for this sale.'
                       END-IF
                   ELSE
                       DISPLAY 'Sale quantity must be positive.'
                   END-IF
               END-IF
           ELSE
               DISPLAY 'Product ID was not found.'
           END-IF.

       ACCEPT-SALE-QUANTITY.
           MOVE 0 TO WS-INPUT-QUANTITY
           DISPLAY 'Quantity sold: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-QUANTITY.

       CALCULATE-SALE-TOTAL.
           PERFORM RESET-CALCULATION-FIELDS
           MOVE WS-INPUT-QUANTITY TO WS-QUANTITY
           MOVE PRODUCT-UNIT-PRICE(WS-SELECTED-INDEX) TO WS-UNIT-PRICE
           COMPUTE WS-TOTAL ROUNDED = WS-QUANTITY * WS-UNIT-PRICE
           IF IS-BULK-SALE
               MOVE WS-BULK-DISC-RATE TO WS-DISCOUNT-RATE
           END-IF
           COMPUTE WS-DISCOUNT-AMOUNT ROUNDED =
               WS-TOTAL * WS-DISCOUNT-RATE
      *> GST is on the value after discount, not the list price
           COMPUTE WS-TAX-AMOUNT ROUNDED =
               (WS-TOTAL - WS-DISCOUNT-AMOUNT)
               * PRODUCT-GST-RATE(WS-SELECTED-INDEX)
           COMPUTE WS-GRAND-TOTAL =
               WS-TOTAL - WS-DISCOUNT-AMOUNT + WS-TAX-AMOUNT.

       APPLY-SALE-TO-STOCK.
           SUBTRACT WS-INPUT-QUANTITY FROM
               PRODUCT-QUANTITY(WS-SELECTED-INDEX)
           ADD WS-INPUT-QUANTITY TO
               PRODUCT-SOLD-COUNT(WS-SELECTED-INDEX).

       DISPLAY-SALE-RECEIPT.
           MOVE WS-TOTAL TO WS-DISPLAY-AMOUNT
           MOVE WS-TAX-AMOUNT TO WS-DISPLAY-TAX
           MOVE WS-DISCOUNT-AMOUNT TO WS-DISPLAY-DISCOUNT
           DISPLAY ' '
           DISPLAY '----------- SALE RECEIPT -----------'
           DISPLAY 'Product: ' PRODUCT-NAME(WS-SELECTED-INDEX)
           DISPLAY 'Quantity: ' WS-INPUT-QUANTITY
           DISPLAY 'Subtotal: ' WS-DISPLAY-AMOUNT
           IF WS-DISCOUNT-AMOUNT > 0
               DISPLAY 'Discount: ' WS-DISPLAY-DISCOUNT ' (bulk)'
           END-IF
           DISPLAY 'GST:      ' WS-DISPLAY-TAX
           DISPLAY 'Total:    ' WS-GRAND-TOTAL
           DISPLAY 'Currency: ' WS-CURRENCY
           DISPLAY '------------------------------------'.

      * Customer brings items back. Can't return more than was sold.
       PROCESS-RETURN.
           PERFORM ACCEPT-PRODUCT-ID
           PERFORM FIND-PRODUCT-BY-ID
           IF WS-ID-FOUND = 'Y'
               IF WS-TRANSACTION-COUNT >= WS-TRANSACTION-LIMIT
                   DISPLAY 'Transaction history full; return cancelled.'
               ELSE
                   PERFORM ACCEPT-RETURN-QUANTITY
                   IF WS-INPUT-QUANTITY > 0
                       IF PRODUCT-SOLD-COUNT(WS-SELECTED-INDEX)
                           >= WS-INPUT-QUANTITY
                           PERFORM APPLY-RETURN-TO-STOCK
                           PERFORM LOG-RETURN
                           DISPLAY 'Return processed successfully.'
                       ELSE
                           DISPLAY 'Return exceeds sold quantity.'
                       END-IF
                   ELSE
                       DISPLAY 'Return quantity must be positive.'
                   END-IF
               END-IF
           ELSE
               DISPLAY 'Product ID was not found.'
           END-IF.

       ACCEPT-RETURN-QUANTITY.
           MOVE 0 TO WS-INPUT-QUANTITY
           DISPLAY 'Quantity returned: ' WITH NO ADVANCING
           ACCEPT WS-INPUT-QUANTITY.

       APPLY-RETURN-TO-STOCK.
           ADD WS-INPUT-QUANTITY TO
               PRODUCT-QUANTITY(WS-SELECTED-INDEX)
           SUBTRACT WS-INPUT-QUANTITY FROM
               PRODUCT-SOLD-COUNT(WS-SELECTED-INDEX)
           ADD WS-INPUT-QUANTITY TO
               PRODUCT-RETURN-COUNT(WS-SELECTED-INDEX).

       LOG-RETURN.
           PERFORM RESET-CALCULATION-FIELDS
           MOVE 'RETURN' TO WS-TRANSACTION-MODE
           MOVE WS-INPUT-QUANTITY TO WS-QUANTITY
           MOVE PRODUCT-UNIT-PRICE(WS-SELECTED-INDEX) TO WS-UNIT-PRICE
           COMPUTE WS-TOTAL = WS-QUANTITY * WS-UNIT-PRICE
           PERFORM WRITE-TRANSACTION.

       RESET-CALCULATION-FIELDS.
           MOVE 0 TO WS-TOTAL
           MOVE 0 TO WS-GRAND-TOTAL
           MOVE 0 TO WS-TAX-AMOUNT
           MOVE 0 TO WS-DISCOUNT-RATE
           MOVE 0 TO WS-DISCOUNT-AMOUNT
           MOVE 0 TO WS-QUANTITY
           MOVE 0 TO WS-UNIT-PRICE.

      * Adds a row to the transaction table.
      * Caller sets WS-TRANSACTION-MODE, WS-QUANTITY and WS-TOTAL.
       WRITE-TRANSACTION.
           IF WS-TRANSACTION-COUNT < WS-TRANSACTION-LIMIT
               ADD 1 TO WS-TRANSACTION-COUNT
               MOVE WS-NEXT-TRANSACTION-ID TO
                   TRANSACTION-ID(WS-TRANSACTION-COUNT)
               MOVE PRODUCT-ID(WS-SELECTED-INDEX) TO
                   TRANSACTION-PRODUCT(WS-TRANSACTION-COUNT)
               MOVE WS-TRANSACTION-MODE TO
                   TRANSACTION-TYPE(WS-TRANSACTION-COUNT)
               MOVE WS-QUANTITY TO
                   TRANSACTION-QUANTITY(WS-TRANSACTION-COUNT)
               MOVE WS-TOTAL TO
                   TRANSACTION-AMOUNT(WS-TRANSACTION-COUNT)
               MOVE WS-TAX-AMOUNT TO
                   TRANSACTION-GST(WS-TRANSACTION-COUNT)
               MOVE WS-DISCOUNT-AMOUNT TO
                   TRANSACTION-DISCOUNT(WS-TRANSACTION-COUNT)
               ACCEPT WS-DATE-TEXT FROM DATE YYYYMMDD
               MOVE WS-DATE-TEXT TO
                   TRANSACTION-DATE(WS-TRANSACTION-COUNT)
               MOVE 'Y' TO TRANSACTION-STATUS(WS-TRANSACTION-COUNT)
               ADD 1 TO WS-NEXT-TRANSACTION-ID
           ELSE
               DISPLAY 'Transaction history is full.'
           END-IF.

      * Anything at or under its reorder level, so we know what to
      * order on the next supplier visit.
       LOW-STOCK-REPORT.
           MOVE 'LOW-STOCK REPORT' TO WS-REPORT-TITLE
           PERFORM PRINT-REPORT-HEADER
           DISPLAY 'Products at or below their reorder level:'
           MOVE 0 TO WS-REPORT-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-PRODUCT-COUNT
               IF PRODUCT-ACTIVE(WS-INDEX) = 'Y'
                   AND PRODUCT-QUANTITY(WS-INDEX)
                       <= PRODUCT-REORDER-LEVEL(WS-INDEX)
                   PERFORM DISPLAY-ONE-PRODUCT
                   DISPLAY '       supplier: '
                       PRODUCT-SUPPLIER(WS-INDEX)
                   ADD 1 TO WS-REPORT-COUNT
               END-IF
           END-PERFORM
           IF WS-REPORT-COUNT = 0
               DISPLAY 'No products are currently low in stock.'
           ELSE
               MOVE WS-REPORT-COUNT TO WS-DISPLAY-COUNT
               DISPLAY WS-DISPLAY-COUNT ' item(s) need restocking.'
           END-IF.

      * Stock value at unit price (not cost price - we don't store it).
       INVENTORY-VALUATION.
           MOVE 0 TO WS-REPORT-VALUE
           MOVE 0 TO WS-REPORT-COUNT
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-PRODUCT-COUNT
               IF PRODUCT-ACTIVE(WS-INDEX) = 'Y'
                   COMPUTE WS-REPORT-VALUE = WS-REPORT-VALUE
                       + (PRODUCT-QUANTITY(WS-INDEX)
                       * PRODUCT-UNIT-PRICE(WS-INDEX))
                   ADD 1 TO WS-REPORT-COUNT
               END-IF
           END-PERFORM
           MOVE WS-REPORT-VALUE TO WS-DISPLAY-AMOUNT
           MOVE WS-REPORT-COUNT TO WS-DISPLAY-COUNT
           MOVE 'INVENTORY VALUATION' TO WS-REPORT-TITLE
           PERFORM PRINT-REPORT-HEADER
           DISPLAY 'Active product types: ' WS-DISPLAY-COUNT
           DISPLAY 'Total inventory value: ' WS-DISPLAY-AMOUNT
           DISPLAY 'Currency: ' WS-CURRENCY.

       DISPLAY-TRANSACTION-HISTORY.
           MOVE 'TRANSACTION HISTORY' TO WS-REPORT-TITLE
           PERFORM PRINT-REPORT-HEADER
           IF WS-TRANSACTION-COUNT = 0
               DISPLAY 'No transactions yet.'
               EXIT PARAGRAPH
           END-IF
           DISPLAY 'TXN#     DATE       TYPE     PROD       QTY'
                   '      AMOUNT        GST   DISCOUNT'
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > WS-TRANSACTION-COUNT
               IF TRANSACTION-STATUS(WS-INDEX) = 'Y'
                   MOVE TRANSACTION-QUANTITY(WS-INDEX) TO WS-H-QTY
                   MOVE TRANSACTION-AMOUNT(WS-INDEX)   TO WS-H-AMT
                   MOVE TRANSACTION-GST(WS-INDEX)      TO WS-H-GST
                   MOVE TRANSACTION-DISCOUNT(WS-INDEX) TO WS-H-DISC
                   DISPLAY TRANSACTION-ID(WS-INDEX) ' '
                       TRANSACTION-DATE(WS-INDEX) ' '
                       TRANSACTION-TYPE(WS-INDEX) ' '
                       TRANSACTION-PRODUCT(WS-INDEX) ' '
                       WS-H-QTY ' ' WS-H-AMT ' '
                       WS-H-GST ' ' WS-H-DISC
               END-IF
           END-PERFORM.

       PRINT-REPORT-HEADER.
      *> set WS-REPORT-TITLE before calling
           DISPLAY ' '
           DISPLAY WS-REPORT-LINE
           DISPLAY '  ' FUNCTION TRIM(WS-REPORT-TITLE)
           DISPLAY WS-REPORT-LINE.

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
