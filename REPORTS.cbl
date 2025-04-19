      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION - REPORTS MODULE
      * 
      * This module handles reporting functions including account
      * listings and account summaries.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REPORTS.
       AUTHOR. PNC-DEMO.
       DATE-WRITTEN. 2023-07-19.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO 'ACCOUNTS.dat'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
           COPY 'DATADEF.cpy'.
       
       WORKING-STORAGE SECTION.
           COPY 'DATADEF.cpy'.
       
       PROCEDURE DIVISION.
       
      *-----------------------------------------------------------------
      * MAIN PROCEDURE
      *-----------------------------------------------------------------
       000-MAIN-PARA.
           PERFORM 600-REPORTING-PARA.
           GOBACK.
       
      *-----------------------------------------------------------------
      * REPORTING MENU
      *-----------------------------------------------------------------
       600-REPORTING-PARA.
           PERFORM 610-DISPLAY-REPORT-MENU-PARA.
           PERFORM 620-GET-REPORT-MENU-CHOICE-PARA.
           
           EVALUATE WS-SUB-MENU-CHOICE
               WHEN 1
                   PERFORM 630-ACCOUNT-LISTING-PARA
               WHEN 2
                   PERFORM 640-ACCOUNT-SUMMARY-PARA
               WHEN 0
                   DISPLAY "Returning to main menu..."
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.
       
      *-----------------------------------------------------------------
      * DISPLAY REPORTING MENU
      *-----------------------------------------------------------------
       610-DISPLAY-REPORT-MENU-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "REPORTING MENU".
           DISPLAY WS-SEPARATOR.
           DISPLAY "1. Account Listing".
           DISPLAY "2. Account Summary".
           DISPLAY "0. Return to Main Menu".
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * GET REPORT MENU CHOICE
      *-----------------------------------------------------------------
       620-GET-REPORT-MENU-CHOICE-PARA.
           DISPLAY "Enter your choice (0-2): " WITH NO ADVANCING.
           ACCEPT WS-SUB-MENU-CHOICE.
           
           IF WS-SUB-MENU-CHOICE NOT NUMERIC OR
              WS-SUB-MENU-CHOICE < 0 OR
              WS-SUB-MENU-CHOICE > 2
               MOVE 9 TO WS-SUB-MENU-CHOICE
           END-IF.
       
      *-----------------------------------------------------------------
      * ACCOUNT LISTING REPORT
      *-----------------------------------------------------------------
       630-ACCOUNT-LISTING-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "ACCOUNT LISTING REPORT".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Account Number    Customer Name                  Type       Balance".
           DISPLAY "---------------- ------------------------------ ---------- ---------------".
           
           OPEN INPUT ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               DISPLAY "ERROR: Unable to open account file."
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       MOVE AR-BALANCE TO WS-FORMATTED-BALANCE
                       DISPLAY AR-ACCOUNT-NUMBER "  " 
                               AR-CUSTOMER-NAME "  " 
                               AR-ACCOUNT-TYPE "  " 
                               WS-FORMATTED-BALANCE
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
           
           DISPLAY WS-SEPARATOR.
           DISPLAY "End of Report".
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * ACCOUNT SUMMARY REPORT
      *-----------------------------------------------------------------
       640-ACCOUNT-SUMMARY-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "ACCOUNT SUMMARY REPORT".
           DISPLAY WS-SEPARATOR.
           
           MOVE 0 TO WS-TEMP-BALANCE.
           MOVE 0 TO WS-TEMP-TRANSACTION-COUNT.
           MOVE 0 TO WS-SUB-MENU-CHOICE.
           
           OPEN INPUT ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               DISPLAY "ERROR: Unable to open account file."
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       ADD 1 TO WS-SUB-MENU-CHOICE
                       ADD AR-BALANCE TO WS-TEMP-BALANCE
                       ADD AR-TRANSACTION-COUNT TO WS-TEMP-TRANSACTION-COUNT
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
           
           MOVE WS-TEMP-BALANCE TO WS-FORMATTED-BALANCE.
           
           DISPLAY "Total Accounts: " WS-SUB-MENU-CHOICE.
           DISPLAY "Total Balance: " WS-FORMATTED-BALANCE.
           DISPLAY "Total Transactions: " WS-TEMP-TRANSACTION-COUNT.
           
           DISPLAY WS-SEPARATOR.
           DISPLAY "End of Report".
           DISPLAY WS-SEPARATOR. 