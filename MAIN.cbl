      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION - MAIN PROGRAM
      * 
      * This is the main program that controls the banking application
      * flow and calls the appropriate modules for specific functions.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKING.
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
      * MAIN PROGRAM CONTROL
      *-----------------------------------------------------------------
       000-MAIN-PARA.
           PERFORM 100-INITIALIZE-PARA.
           PERFORM 200-PROCESS-MENU-PARA UNTIL WS-MENU-CHOICE = 0.
           PERFORM 900-TERMINATE-PARA.
           STOP RUN.
       
      *-----------------------------------------------------------------
      * INITIALIZE PROGRAM
      *-----------------------------------------------------------------
       100-INITIALIZE-PARA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           DISPLAY SPACE.
           DISPLAY WS-HEADER.
           DISPLAY WS-SEPARATOR.
           DISPLAY "Do you want to enter demo mode? (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONFIRM.
           
           IF CONFIRM-YES
               MOVE 'Y' TO WS-DEMO-MODE-FLAG
               PERFORM 150-LOAD-DEMO-DATA-PARA
           END-IF.
       
      *-----------------------------------------------------------------
      * LOAD DEMO DATA
      *-----------------------------------------------------------------
       150-LOAD-DEMO-DATA-PARA.
           DISPLAY "Loading demo data...".
           
           OPEN OUTPUT ACCOUNT-FILE.
           
           MOVE "1000000001" TO AR-ACCOUNT-NUMBER.
           MOVE "JOHN SMITH" TO AR-CUSTOMER-NAME.
           MOVE "CHECKING" TO AR-ACCOUNT-TYPE.
           MOVE 5000.00 TO AR-BALANCE.
           MOVE "2023-01-15" TO AR-OPEN-DATE.
           MOVE "2023-07-18" TO AR-LAST-ACCESS-DATE.
           MOVE 12 TO AR-TRANSACTION-COUNT.
           MOVE "A" TO AR-STATUS.
           WRITE ACCOUNT-RECORD.
           
           MOVE "1000000002" TO AR-ACCOUNT-NUMBER.
           MOVE "JANE DOE" TO AR-CUSTOMER-NAME.
           MOVE "SAVINGS" TO AR-ACCOUNT-TYPE.
           MOVE 15000.00 TO AR-BALANCE.
           MOVE "2022-05-20" TO AR-OPEN-DATE.
           MOVE "2023-07-10" TO AR-LAST-ACCESS-DATE.
           MOVE 8 TO AR-TRANSACTION-COUNT.
           MOVE "A" TO AR-STATUS.
           WRITE ACCOUNT-RECORD.
           
           MOVE "1000000003" TO AR-ACCOUNT-NUMBER.
           MOVE "ROBERT JOHNSON" TO AR-CUSTOMER-NAME.
           MOVE "CHECKING" TO AR-ACCOUNT-TYPE.
           MOVE 2500.75 TO AR-BALANCE.
           MOVE "2023-03-10" TO AR-OPEN-DATE.
           MOVE "2023-07-15" TO AR-LAST-ACCESS-DATE.
           MOVE 5 TO AR-TRANSACTION-COUNT.
           MOVE "A" TO AR-STATUS.
           WRITE ACCOUNT-RECORD.
           
           CLOSE ACCOUNT-FILE.
           
           DISPLAY "Demo data loaded successfully.".
           
      *-----------------------------------------------------------------
      * MAIN MENU PROCESSING
      *-----------------------------------------------------------------
       200-PROCESS-MENU-PARA.
           PERFORM 210-DISPLAY-MAIN-MENU-PARA.
           PERFORM 220-GET-MENU-CHOICE-PARA.
           
           EVALUATE WS-MENU-CHOICE
               WHEN 1
                   CALL 'ACCOUNT' 
               WHEN 2
                   CALL 'TRANSACT' 
               WHEN 3
                   PERFORM 500-BALANCE-INQUIRY-PARA
               WHEN 4
                   CALL 'REPORTS'
               WHEN 0
                   DISPLAY "Exiting program..."
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.
       
      *-----------------------------------------------------------------
      * DISPLAY MAIN MENU
      *-----------------------------------------------------------------
       210-DISPLAY-MAIN-MENU-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "MAIN MENU".
           DISPLAY WS-SEPARATOR.
           DISPLAY "1. Account Management".
           DISPLAY "2. Transaction Processing".
           DISPLAY "3. Balance Inquiry".
           DISPLAY "4. Reports".
           DISPLAY "0. Exit Program".
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * GET MENU CHOICE
      *-----------------------------------------------------------------
       220-GET-MENU-CHOICE-PARA.
           DISPLAY "Enter your choice (0-4): " WITH NO ADVANCING.
           ACCEPT WS-MENU-CHOICE.
           
           IF WS-MENU-CHOICE NOT NUMERIC OR
              WS-MENU-CHOICE < 0 OR
              WS-MENU-CHOICE > 4
               MOVE 9 TO WS-MENU-CHOICE
           END-IF.
           
      *-----------------------------------------------------------------
      * BALANCE INQUIRY
      *-----------------------------------------------------------------
       500-BALANCE-INQUIRY-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "BALANCE INQUIRY".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter account number: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 510-VALIDATE-ACCOUNT-PARA.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Account not found."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE WS-TEMP-BALANCE TO WS-FORMATTED-BALANCE.
           
           DISPLAY WS-SEPARATOR.
           DISPLAY "ACCOUNT SUMMARY".
           DISPLAY WS-SEPARATOR.
           DISPLAY "Account Number: " WS-TEMP-ACCOUNT-NUMBER.
           DISPLAY "Customer Name: " WS-TEMP-CUSTOMER-NAME.
           DISPLAY "Account Type: " WS-TEMP-ACCOUNT-TYPE.
           DISPLAY "Current Balance: " WS-FORMATTED-BALANCE.
           DISPLAY "Last Access Date: " WS-TEMP-LAST-ACCESS-DATE.
           DISPLAY "Transaction Count: " WS-TEMP-TRANSACTION-COUNT.
           DISPLAY WS-SEPARATOR.
           
      *-----------------------------------------------------------------
      * VALIDATE ACCOUNT
      *-----------------------------------------------------------------
       510-VALIDATE-ACCOUNT-PARA.
           MOVE 'N' TO WS-ACCOUNT-FOUND-FLAG.
           
           OPEN INPUT ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               DISPLAY "ERROR: Unable to open account file."
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM UNTIL END-OF-FILE OR ACCOUNT-FOUND
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       IF AR-ACCOUNT-NUMBER = WS-ACCOUNT-NUMBER
                           MOVE 'Y' TO WS-ACCOUNT-FOUND-FLAG
                           MOVE AR-ACCOUNT-NUMBER TO WS-TEMP-ACCOUNT-NUMBER
                           MOVE AR-CUSTOMER-NAME TO WS-TEMP-CUSTOMER-NAME
                           MOVE AR-ACCOUNT-TYPE TO WS-TEMP-ACCOUNT-TYPE
                           MOVE AR-BALANCE TO WS-TEMP-BALANCE
                           MOVE AR-OPEN-DATE TO WS-TEMP-OPEN-DATE
                           MOVE AR-LAST-ACCESS-DATE TO WS-TEMP-LAST-ACCESS-DATE
                           MOVE AR-TRANSACTION-COUNT TO WS-TEMP-TRANSACTION-COUNT
                           MOVE AR-STATUS TO WS-TEMP-STATUS
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
       
      *-----------------------------------------------------------------
      * TERMINATE PROGRAM
      *-----------------------------------------------------------------
       900-TERMINATE-PARA.
           DISPLAY WS-SEPARATOR.
           DISPLAY WS-FOOTER.
           DISPLAY WS-SEPARATOR.
           STOP RUN. 