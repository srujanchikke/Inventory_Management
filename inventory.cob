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
           05 WS-NEXT-PRODUCT-ID       PIC 9(6) VALUE 1.
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
           DISPLAY ' '
           DISPLAY '   INVENTORY MANAGEMENT SYSTEM'
           DISPLAY '========================================'
           DISPLAY ' '.

       CLEAR-PRODUCT-TABLE.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
               UNTIL WS-INDEX > 200
               INITIALIZE PRODUCT-ENTRY(WS-INDEX)
               MOVE 'N' TO PRODUCT-ACTIVE(WS-INDEX)
           END-PERFORM.

       DISPLAY-MAIN-MENU.
           DISPLAY ' '
           DISPLAY '-------------- MAIN MENU --------------'
           DISPLAY '0. Exit'
           DISPLAY '---------------------------------------'.

       ACCEPT-MENU-CHOICE.
           MOVE SPACES TO WS-MENU-CHOICE
           DISPLAY 'Enter choice: ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE.

       PROCESS-MENU-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN '0' MOVE 'N' TO WS-RUNNING
               WHEN OTHER
                   DISPLAY 'Invalid menu choice. Please try again.'
           END-EVALUATE.

       SHUTDOWN-SYSTEM.
           DISPLAY ' '
           DISPLAY 'Session ended.'.
