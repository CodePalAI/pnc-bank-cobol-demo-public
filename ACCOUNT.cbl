      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION - ACCOUNT MANAGEMENT MODULE
      * 
      * This module handles account management functions including
      * creating, viewing, and deleting accounts.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCOUNT.
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
           PERFORM 300-ACCOUNT-MANAGEMENT-PARA.
           GOBACK.
       
      *-----------------------------------------------------------------
      * ACCOUNT MANAGEMENT MENU
      *-----------------------------------------------------------------
       300-ACCOUNT-MANAGEMENT-PARA.
           PERFORM 310-DISPLAY-ACCOUNT-MENU-PARA.
           PERFORM 320-GET-ACCOUNT-MENU-CHOICE-PARA.
           
           EVALUATE WS-SUB-MENU-CHOICE
               WHEN 1
                   PERFORM 330-CREATE-ACCOUNT-PARA
               WHEN 2
                   PERFORM 340-VIEW-ACCOUNT-PARA
               WHEN 3
                   PERFORM 350-DELETE-ACCOUNT-PARA
               WHEN 0
                   DISPLAY "Returning to main menu..."
               WHEN OTHER
                   DISPLAY "Invalid choice. Please try again."
           END-EVALUATE.
       
      *-----------------------------------------------------------------
      * DISPLAY ACCOUNT MANAGEMENT MENU
      *-----------------------------------------------------------------
       310-DISPLAY-ACCOUNT-MENU-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "ACCOUNT MANAGEMENT MENU".
           DISPLAY WS-SEPARATOR.
           DISPLAY "1. Create New Account".
           DISPLAY "2. View Account Details".
           DISPLAY "3. Delete Account".
           DISPLAY "0. Return to Main Menu".
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * GET ACCOUNT MENU CHOICE
      *-----------------------------------------------------------------
       320-GET-ACCOUNT-MENU-CHOICE-PARA.
           DISPLAY "Enter your choice (0-3): " WITH NO ADVANCING.
           ACCEPT WS-SUB-MENU-CHOICE.
           
           IF WS-SUB-MENU-CHOICE NOT NUMERIC OR
              WS-SUB-MENU-CHOICE < 0 OR
              WS-SUB-MENU-CHOICE > 3
               MOVE 9 TO WS-SUB-MENU-CHOICE
           END-IF.
       
      *-----------------------------------------------------------------
      * CREATE NEW ACCOUNT
      *-----------------------------------------------------------------
       330-CREATE-ACCOUNT-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "CREATE NEW ACCOUNT".
           DISPLAY WS-SEPARATOR.
           
           PERFORM 331-GET-ACCOUNT-DATA-PARA.
           
           IF VALID-DATA
               PERFORM 332-SAVE-NEW-ACCOUNT-PARA
           ELSE
               DISPLAY "Account creation aborted."
           END-IF.
       
      *-----------------------------------------------------------------
      * GET ACCOUNT DATA
      *-----------------------------------------------------------------
       331-GET-ACCOUNT-DATA-PARA.
           MOVE 'N' TO WS-VALID-DATA-FLAG.
           
           DISPLAY "Enter account number (10 digits): " WITH NO ADVANCING.
           ACCEPT WS-TEMP-ACCOUNT-NUMBER.
           
           IF WS-TEMP-ACCOUNT-NUMBER IS NOT NUMERIC OR 
              WS-TEMP-ACCOUNT-NUMBER = SPACES
               DISPLAY "ERROR: Account number must be numeric and not empty."
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM 333-CHECK-ACCOUNT-EXISTS-PARA.
           
           IF ACCOUNT-FOUND
               DISPLAY "ERROR: Account number already exists."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY "Enter customer name: " WITH NO ADVANCING.
           ACCEPT WS-TEMP-CUSTOMER-NAME.
           
           IF WS-TEMP-CUSTOMER-NAME = SPACES
               DISPLAY "ERROR: Customer name cannot be empty."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY "Enter account type (CHECKING/SAVINGS): " 
               WITH NO ADVANCING.
           ACCEPT WS-TEMP-ACCOUNT-TYPE.
           
           IF WS-TEMP-ACCOUNT-TYPE NOT = "CHECKING" AND
              WS-TEMP-ACCOUNT-TYPE NOT = "SAVINGS"
               DISPLAY "ERROR: Account type must be CHECKING or SAVINGS."
               EXIT PARAGRAPH
           END-IF.
           
           DISPLAY "Enter initial deposit amount: " WITH NO ADVANCING.
           ACCEPT WS-TEMP-BALANCE.
           
           IF WS-TEMP-BALANCE NOT NUMERIC OR WS-TEMP-BALANCE <= 0
               DISPLAY "ERROR: Initial deposit must be a positive number."
               EXIT PARAGRAPH
           END-IF.
           
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA.
           MOVE WS-CURRENT-YEAR TO WS-DATE-FORMATTED(1:4).
           MOVE '-' TO WS-DATE-FORMATTED(5:1).
           MOVE WS-CURRENT-MONTH TO WS-DATE-FORMATTED(6:2).
           MOVE '-' TO WS-DATE-FORMATTED(8:1).
           MOVE WS-CURRENT-DAY TO WS-DATE-FORMATTED(9:2).
           
           MOVE WS-DATE-FORMATTED TO WS-TEMP-OPEN-DATE.
           MOVE WS-DATE-FORMATTED TO WS-TEMP-LAST-ACCESS-DATE.
           MOVE 0 TO WS-TEMP-TRANSACTION-COUNT.
           MOVE 'A' TO WS-TEMP-STATUS.
           
           MOVE 'Y' TO WS-VALID-DATA-FLAG.
       
      *-----------------------------------------------------------------
      * CHECK IF ACCOUNT EXISTS
      *-----------------------------------------------------------------
       333-CHECK-ACCOUNT-EXISTS-PARA.
           MOVE 'N' TO WS-ACCOUNT-FOUND-FLAG.
           
           OPEN INPUT ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               CLOSE ACCOUNT-FILE
               EXIT PARAGRAPH
           END-IF.
           
           PERFORM UNTIL END-OF-FILE OR ACCOUNT-FOUND
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       IF AR-ACCOUNT-NUMBER = WS-TEMP-ACCOUNT-NUMBER
                           MOVE 'Y' TO WS-ACCOUNT-FOUND-FLAG
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
       
      *-----------------------------------------------------------------
      * SAVE NEW ACCOUNT
      *-----------------------------------------------------------------
       332-SAVE-NEW-ACCOUNT-PARA.
           OPEN EXTEND ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               OPEN OUTPUT ACCOUNT-FILE
           END-IF.
           
           MOVE WS-TEMP-ACCOUNT-NUMBER TO AR-ACCOUNT-NUMBER.
           MOVE WS-TEMP-CUSTOMER-NAME TO AR-CUSTOMER-NAME.
           MOVE WS-TEMP-ACCOUNT-TYPE TO AR-ACCOUNT-TYPE.
           MOVE WS-TEMP-BALANCE TO AR-BALANCE.
           MOVE WS-TEMP-OPEN-DATE TO AR-OPEN-DATE.
           MOVE WS-TEMP-LAST-ACCESS-DATE TO AR-LAST-ACCESS-DATE.
           MOVE WS-TEMP-TRANSACTION-COUNT TO AR-TRANSACTION-COUNT.
           MOVE WS-TEMP-STATUS TO AR-STATUS.
           
           WRITE ACCOUNT-RECORD.
           
           IF FILE-SUCCESS
               DISPLAY "Account created successfully."
           ELSE
               DISPLAY "ERROR: Failed to create account."
           END-IF.
           
           CLOSE ACCOUNT-FILE.
       
      *-----------------------------------------------------------------
      * VIEW ACCOUNT DETAILS
      *-----------------------------------------------------------------
       340-VIEW-ACCOUNT-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "VIEW ACCOUNT DETAILS".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter account number: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 341-FIND-AND-DISPLAY-ACCOUNT-PARA.
       
      *-----------------------------------------------------------------
      * FIND AND DISPLAY ACCOUNT
      *-----------------------------------------------------------------
       341-FIND-AND-DISPLAY-ACCOUNT-PARA.
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
                           PERFORM 342-DISPLAY-ACCOUNT-DETAILS-PARA
                       END-IF
               END-READ
           END-PERFORM.
           
           IF NOT ACCOUNT-FOUND
               DISPLAY "Account not found."
           END-IF.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
       
      *-----------------------------------------------------------------
      * DISPLAY ACCOUNT DETAILS
      *-----------------------------------------------------------------
       342-DISPLAY-ACCOUNT-DETAILS-PARA.
           DISPLAY WS-SEPARATOR.
           DISPLAY "ACCOUNT DETAILS".
           DISPLAY WS-SEPARATOR.
           DISPLAY "Account Number: " AR-ACCOUNT-NUMBER.
           DISPLAY "Customer Name: " AR-CUSTOMER-NAME.
           DISPLAY "Account Type: " AR-ACCOUNT-TYPE.
           
           MOVE AR-BALANCE TO WS-FORMATTED-BALANCE.
           DISPLAY "Balance: " WS-FORMATTED-BALANCE.
           
           DISPLAY "Open Date: " AR-OPEN-DATE.
           DISPLAY "Last Access: " AR-LAST-ACCESS-DATE.
           DISPLAY "Transaction Count: " AR-TRANSACTION-COUNT.
           
           EVALUATE AR-STATUS
               WHEN 'A'
                   DISPLAY "Status: Active"
               WHEN 'C'
                   DISPLAY "Status: Closed"
               WHEN 'S'
                   DISPLAY "Status: Suspended"
               WHEN OTHER
                   DISPLAY "Status: Unknown"
           END-EVALUATE.
           
           DISPLAY WS-SEPARATOR.
       
      *-----------------------------------------------------------------
      * DELETE ACCOUNT
      *-----------------------------------------------------------------
       350-DELETE-ACCOUNT-PARA.
           DISPLAY SPACE.
           DISPLAY WS-SEPARATOR.
           DISPLAY "DELETE ACCOUNT".
           DISPLAY WS-SEPARATOR.
           
           DISPLAY "Enter account number to delete: " WITH NO ADVANCING.
           ACCEPT WS-ACCOUNT-NUMBER.
           
           PERFORM 351-FIND-ACCOUNT-TO-DELETE-PARA.
           
           IF ACCOUNT-FOUND
               DISPLAY "Confirm deletion (Y/N): " WITH NO ADVANCING
               ACCEPT WS-CONFIRM
               
               IF CONFIRM-YES
                   PERFORM 352-PERFORM-ACCOUNT-DELETION-PARA
               ELSE
                   DISPLAY "Deletion cancelled."
               END-IF
           ELSE
               DISPLAY "Account not found."
           END-IF.
       
      *-----------------------------------------------------------------
      * FIND ACCOUNT TO DELETE
      *-----------------------------------------------------------------
       351-FIND-ACCOUNT-TO-DELETE-PARA.
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
                           PERFORM 342-DISPLAY-ACCOUNT-DETAILS-PARA
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           MOVE 'N' TO WS-END-OF-FILE-FLAG.
       
      *-----------------------------------------------------------------
      * PERFORM ACCOUNT DELETION
      *-----------------------------------------------------------------
       352-PERFORM-ACCOUNT-DELETION-PARA.
           OPEN INPUT ACCOUNT-FILE.
           IF NOT FILE-SUCCESS
               DISPLAY "ERROR: Unable to open account file."
               EXIT PARAGRAPH
           END-IF.
           
           OPEN OUTPUT ACCOUNT-FILE ASSIGN TO WS-TEMP-FILE-NAME.
           
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE-FLAG
                   NOT AT END
                       IF AR-ACCOUNT-NUMBER NOT = WS-ACCOUNT-NUMBER
                           WRITE ACCOUNT-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           
           CLOSE ACCOUNT-FILE.
           CLOSE ACCOUNT-FILE ASSIGN TO WS-TEMP-FILE-NAME.
           
           DISPLAY "Account deleted successfully.".
           MOVE 'N' TO WS-END-OF-FILE-FLAG. 