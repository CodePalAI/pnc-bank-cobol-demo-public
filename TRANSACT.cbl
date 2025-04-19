      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION - TRANSACTION PROCESSING MODULE
      * 
      * This module handles transaction processing including deposits,
      * withdrawals, and fund transfers.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSACT.
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
           PERFORM 400-TRANSACTION-PROCESSING-PARA.
           GOBACK.
       
      *-----------------------------------------------------------------
      * TRANSACTION PROCESSING MENU
      *-----------------------------------------------------------------
       400-TRANSACTION-PROCESSING-PARA.
           PERFORM 410-DISPLAY-TRANSACTION-MENU-PARA.
           PERFORM 420-GET-TRANSACTION-MENU-CHOICE-PARA.
           
           EVALUATE WS-SUB-MENU-CHOICE
               WHEN 1
                   PERFORM 430-DEPOSIT-PARA
               WHEN 2
                   PERFORM 440-WITHDRAWAL-PARA
               WHEN 3
                   PERFORM 450-TRANSFER-PARA
               WHEN 0
                   DISPLAY "Returning to main menu..."
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.
       
      *-----------------------------------------------------------------
      * DISPLAY TRANSACTION MENU
      *-----------------------------------------------------------------
       410-DISPLAY-TRANSACTION-MENU-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "TRANSACTION PROCESSING MENU".
           DISPLAY WS-SEPARATOR.
           DISPLAY "1. Deposit".
           DISPLAY "2. Withdrawal".
           DISPLAY "3. Funds Transfer".
           DISPLAY "0. Return to Main Menu".
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * GET TRANSACTION MENU CHOICE
      *-----------------------------------------------------------------
       420-GET-TRANSACTION-MENU-CHOICE-PARA.
           DISPLAY "Enter your choice (0-3): " WITH NO ADVANCING.
           ACCEPT WS-SUB-MENU-CHOICE.
           
           IF WS-SUB-MENU-CHOICE NOT NUMERIC OR
              WS-SUB-MENU-CHOICE < 0 OR
              WS-SUB-MENU-CHOICE > 3
               MOVE 9 TO WS-SUB-MENU-CHOICE
           END-IF.
       
      *-----------------------------------------------------------------
      * DEPOSIT TRANSACTION
      *-----------------------------------------------------------------
       430-DEPOSIT-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "DEPOSIT".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter account number: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Account not found."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY "Enter deposit amount: " WITH NO ADVANCING.
           ACCEPT WS-AMOUNT.
           
           IF WS-AMOUNT NOT NUMERIC OR WS-AMOUNT <= 0
               DISPLAY "ERROR: Deposit amount must be a positive number."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE "DEPOSIT" TO WS-TR-TYPE.
           MOVE WS-AMOUNT TO WS-TR-AMOUNT.
           
           PERFORM 432-PROCESS-TRANSACTION-PARA.
       
      *-----------------------------------------------------------------
      * VALIDATE ACCOUNT
      *-----------------------------------------------------------------
       431-VALIDATE-ACCOUNT-PARA.
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
      * PROCESS TRANSACTION
      *-----------------------------------------------------------------
       432-PROCESS-TRANSACTION-PARA.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           
           MOVE WS-CURRENT-YEAR TO WS-DATE-FORMATTED(1:4).
           MOVE '-' TO WS-DATE-FORMATTED(5:1).
           MOVE WS-CURRENT-MONTH TO WS-DATE-FORMATTED(6:2).
           MOVE '-' TO WS-DATE-FORMATTED(8:1).
           MOVE WS-CURRENT-DAY TO WS-DATE-FORMATTED(9:2).
           MOVE WS-DATE-FORMATTED TO WS-TR-DATE.
           
           MOVE WS-CURRENT-HOUR TO WS-TR-TIME(1:2).
           MOVE ':' TO WS-TR-TIME(3:1).
           MOVE WS-CURRENT-MINUTE TO WS-TR-TIME(4:2).
           MOVE ':' TO WS-TR-TIME(6:1).
           MOVE WS-CURRENT-SECOND TO WS-TR-TIME(7:2).
           
           EVALUATE WS-TR-TYPE
               WHEN "DEPOSIT"
                   ADD WS-TR-AMOUNT TO WS-TEMP-BALANCE
                   MOVE "SUCCESS" TO WS-TR-RESULT
                   MOVE "Deposit completed" TO WS-TR-REMARKS
               WHEN "WITHDRAWAL"
                   IF WS-TR-AMOUNT > WS-TEMP-BALANCE
                       MOVE "FAILURE" TO WS-TR-RESULT
                       MOVE "Insufficient funds" TO WS-TR-REMARKS
                   ELSE
                       SUBTRACT WS-TR-AMOUNT FROM WS-TEMP-BALANCE
                       MOVE "SUCCESS" TO WS-TR-RESULT
                       MOVE "Withdrawal completed" TO WS-TR-REMARKS
                   END-IF
               WHEN "XFER-OUT"
                   IF WS-TR-AMOUNT > WS-TEMP-BALANCE
                       MOVE "FAILURE" TO WS-TR-RESULT
                       MOVE "Insufficient funds for transfer" TO WS-TR-REMARKS
                   ELSE
                       SUBTRACT WS-TR-AMOUNT FROM WS-TEMP-BALANCE
                       MOVE "SUCCESS" TO WS-TR-RESULT
                       MOVE "Transfer out completed" TO WS-TR-REMARKS
                   END-IF
               WHEN "XFER-IN"
                   ADD WS-TR-AMOUNT TO WS-TEMP-BALANCE
                   MOVE "SUCCESS" TO WS-TR-RESULT
                   MOVE "Transfer in completed" TO WS-TR-REMARKS
           END-EVALUATE.
           
           IF TR-SUCCESS
               ADD 1 TO WS-TEMP-TRANSACTION-COUNT
               MOVE WS-DATE-FORMATTED TO WS-TEMP-LAST-ACCESS-DATE
               PERFORM 433-UPDATE-ACCOUNT-PARA
           END-IF.
           
           MOVE WS-TEMP-BALANCE TO WS-FORMATTED-BALANCE.
           MOVE WS-TR-AMOUNT TO WS-FORMATTED-AMOUNT.
           
           DISPLAY WS-SEPARATOR.
           DISPLAY "Transaction Result: " WS-TR-RESULT.
           DISPLAY "Amount: " WS-FORMATTED-AMOUNT.
           DISPLAY "New Balance: " WS-FORMATTED-BALANCE.
           DISPLAY "Remarks: " WS-TR-REMARKS.
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * UPDATE ACCOUNT
      *-----------------------------------------------------------------
       433-UPDATE-ACCOUNT-PARA.
           OPEN INPUT ACCOUNT-FILE.
           OPEN OUTPUT ACCOUNT-FILE ASSIGN TO WS-TEMP-FILE-NAME.
           
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       IF AR-ACCOUNT-NUMBER = WS-TEMP-ACCOUNT-NUMBER
                           MOVE WS-TEMP-ACCOUNT-NUMBER TO AR-ACCOUNT-NUMBER
                           MOVE WS-TEMP-CUSTOMER-NAME TO AR-CUSTOMER-NAME
                           MOVE WS-TEMP-ACCOUNT-TYPE TO AR-ACCOUNT-TYPE
                           MOVE WS-TEMP-BALANCE TO AR-BALANCE
                           MOVE WS-TEMP-OPEN-DATE TO AR-OPEN-DATE
                           MOVE WS-TEMP-LAST-ACCESS-DATE TO AR-LAST-ACCESS-DATE
                           MOVE WS-TEMP-TRANSACTION-COUNT TO AR-TRANSACTION-COUNT
                           MOVE WS-TEMP-STATUS TO AR-STATUS
                       END-IF
                       WRITE ACCOUNT-RECORD
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           CLOSE ACCOUNT-FILE ASSIGN TO WS-TEMP-FILE-NAME.
           
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
       
      *-----------------------------------------------------------------
      * WITHDRAWAL TRANSACTION
      *-----------------------------------------------------------------
       440-WITHDRAWAL-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "WITHDRAWAL".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter account number: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Account not found."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY "Enter withdrawal amount: " WITH NO ADVANCING.
           ACCEPT WS-AMOUNT.
           
           IF WS-AMOUNT NOT NUMERIC OR WS-AMOUNT <= 0
               DISPLAY "ERROR: Withdrawal amount must be a positive number."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE "WITHDRAWAL" TO WS-TR-TYPE.
           MOVE WS-AMOUNT TO WS-TR-AMOUNT.
           
           PERFORM 432-PROCESS-TRANSACTION-PARA.
       
      *-----------------------------------------------------------------
      * TRANSFER FUNDS
      *-----------------------------------------------------------------
       450-TRANSFER-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "FUNDS TRANSFER".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter source account number: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Source account not found."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE WS-TEMP-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER.
           
           DISPLAY "Enter target account number: " WITH NO ADVANCING.
           ACCEPT WS-TRANSFER-TO-ACCT.
           
           IF WS-TRANSFER-TO-ACCT = WS-ACCOUNT-NUMBER
               DISPLAY "ERROR: Source and target accounts cannot be the same."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE WS-TRANSFER-TO-ACCT TO WS-ACCOUNT-NUMBER.
           MOVE 'N' TO WS-ACCOUNT-FOUND-FLAG.
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Target account not found."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE WS-TRANSFER-TO-ACCT TO WS-TRANSFER-TO-ACCT.
           MOVE WS-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER.
           
           DISPLAY "Enter transfer amount: " WITH NO ADVANCING.
           ACCEPT WS-AMOUNT.
           
           IF WS-AMOUNT NOT NUMERIC OR WS-AMOUNT <= 0
               DISPLAY "ERROR: Transfer amount must be a positive number."
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM 451-PROCESS-TRANSFER-PARA.
       
      *-----------------------------------------------------------------
      * PROCESS TRANSFER
      *-----------------------------------------------------------------
       451-PROCESS-TRANSFER-PARA.
           MOVE WS-ACCOUNT-NUMBER TO WS-ACCOUNT-NUMBER.
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           IF WS-AMOUNT > WS-TEMP-BALANCE
               DISPLAY "ERROR: Insufficient funds for transfer."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE "XFER-OUT" TO WS-TR-TYPE.
           MOVE WS-AMOUNT TO WS-TR-AMOUNT.
           PERFORM 432-PROCESS-TRANSACTION-PARA.
           
           IF NOT TR-SUCCESS
               EXIT PARAGRAPH
           END-IF.
           
           MOVE WS-TRANSFER-TO-ACCT TO WS-ACCOUNT-NUMBER.
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
           MOVE "XFER-IN" TO WS-TR-TYPE.
           MOVE WS-AMOUNT TO WS-TR-AMOUNT.
           PERFORM 432-PROCESS-TRANSACTION-PARA. 