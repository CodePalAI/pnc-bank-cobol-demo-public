      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION
      * 
      * This program demonstrates a banking application with core 
      * banking functionalities including account management,
      * transaction processing, and reporting.
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
       FD  ACCOUNT-FILE
           LABEL RECORDS ARE STANDARD.
       01  ACCOUNT-RECORD.
           05  AR-ACCOUNT-NUMBER      PIC X(10).
           05  AR-CUSTOMER-NAME       PIC X(30).
           05  AR-ACCOUNT-TYPE        PIC X(10).
           05  AR-BALANCE             PIC 9(9)V99.
           05  AR-OPEN-DATE           PIC X(10).
           05  AR-LAST-ACCESS-DATE    PIC X(10).
           05  AR-TRANSACTION-COUNT   PIC 9(5).
           05  AR-STATUS              PIC X(1).
               88 AR-ACTIVE           VALUE 'A'.
               88 AR-CLOSED           VALUE 'C'.
               88 AR-SUSPENDED        VALUE 'S'.
       
       WORKING-STORAGE SECTION.
       01  FILE-STATUS                PIC X(2).
           88 FILE-SUCCESS            VALUE '00'.
           88 FILE-EOF                VALUE '10'.
       
       01  WS-FLAGS.
           05  WS-END-OF-FILE-FLAG    PIC X(1) VALUE 'N'.
               88 END-OF-FILE         VALUE 'Y'.
           05  WS-EOF-FLAG            PIC X(1) VALUE 'N'.
               88 EOF                 VALUE 'Y'.
           05  WS-VALID-DATA-FLAG     PIC X(1) VALUE 'N'.
               88 VALID-DATA          VALUE 'Y'.
           05  WS-ACCOUNT-FOUND-FLAG  PIC X(1) VALUE 'N'.
               88 ACCOUNT-FOUND       VALUE 'Y'.

       01  WS-CURRENT-DATE-DATA.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR    PIC 9(4).
               10  WS-CURRENT-MONTH   PIC 9(2).
               10  WS-CURRENT-DAY     PIC 9(2).
           05  WS-CURRENT-TIME.
               10  WS-CURRENT-HOUR    PIC 9(2).
               10  WS-CURRENT-MINUTE  PIC 9(2).
               10  WS-CURRENT-SECOND  PIC 9(2).
               10  WS-CURRENT-MS      PIC 9(2).
       
       01  WS-WORKING-AREAS.
           05  WS-MENU-CHOICE         PIC 9.
           05  WS-SUB-MENU-CHOICE     PIC 9.
           05  WS-ACCOUNT-NUMBER      PIC X(10).
           05  WS-AMOUNT              PIC 9(7)V99.
           05  WS-TRANSFER-TO-ACCT    PIC X(10).
           05  WS-CONFIRM             PIC X.
               88 CONFIRM-YES         VALUE 'Y' 'y'.
               88 CONFIRM-NO          VALUE 'N' 'n'.
           
       01  WS-TRANSACTION.
           05  WS-TR-TYPE             PIC X(10).
               88 TR-DEPOSIT          VALUE 'DEPOSIT'.
               88 TR-WITHDRAWAL       VALUE 'WITHDRAWAL'.
               88 TR-TRANSFER-OUT     VALUE 'XFER-OUT'.
               88 TR-TRANSFER-IN      VALUE 'XFER-IN'.
           05  WS-TR-AMOUNT           PIC 9(7)V99.
           05  WS-TR-DATE             PIC X(10).
           05  WS-TR-TIME             PIC X(8).
           05  WS-TR-RESULT           PIC X(10).
               88 TR-SUCCESS          VALUE 'SUCCESS'.
               88 TR-FAILURE          VALUE 'FAILURE'.
           05  WS-TR-REMARKS          PIC X(50).
       
       01  WS-TEMP-ACCOUNT.
           05  WS-TEMP-ACCOUNT-NUMBER     PIC X(10).
           05  WS-TEMP-CUSTOMER-NAME      PIC X(30).
           05  WS-TEMP-ACCOUNT-TYPE       PIC X(10).
           05  WS-TEMP-BALANCE            PIC 9(9)V99.
           05  WS-TEMP-OPEN-DATE          PIC X(10).
           05  WS-TEMP-LAST-ACCESS-DATE   PIC X(10).
           05  WS-TEMP-TRANSACTION-COUNT  PIC 9(5).
           05  WS-TEMP-STATUS             PIC X(1).
       
       01  WS-TEMP-FILE-NAME              PIC X(20) VALUE 'TEMP.dat'.
       
       01  WS-ERROR-MESSAGE               PIC X(100).
       
      * Display Constants
       01  WS-DISPLAY-CONSTANTS.
           05  WS-HEADER.
               10  FILLER               PIC X(25) VALUE SPACE.
               10  FILLER               PIC X(30) 
                   VALUE "PNC BANK - BANKING APPLICATION".
               10  FILLER               PIC X(25) VALUE SPACE.
           05  WS-FOOTER.
               10  FILLER               PIC X(30) VALUE SPACE.
               10  FILLER               PIC X(20) 
                   VALUE "THANK YOU FOR USING".
               10  FILLER               PIC X(30) VALUE SPACE.
           05  WS-SEPARATOR             PIC X(80) VALUE ALL "-".
           
       01  WS-FORMATTED-FIELDS.
           05  WS-FORMATTED-BALANCE     PIC $Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-FORMATTED-AMOUNT      PIC $Z,ZZZ,ZZZ,ZZ9.99.
           05  WS-DATE-FORMATTED        PIC X(10).
       
       01  WS-DEMO-MODE-FLAG            PIC X VALUE 'N'.
           88 DEMO-MODE-ACTIVE         VALUE 'Y'.
       
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
                   PERFORM 300-ACCOUNT-MANAGEMENT-PARA
               WHEN 2
                   PERFORM 400-TRANSACTION-PROCESSING-PARA
               WHEN 3
                   PERFORM 500-BALANCE-INQUIRY-PARA
               WHEN 4
                   PERFORM 600-REPORTING-PARA
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
       
      *-----------------------------------------------------------------
      * TERMINATE PROGRAM
      *-----------------------------------------------------------------
       900-TERMINATE-PARA.
           DISPLAY WS-SEPARATOR.
           DISPLAY WS-FOOTER.
           DISPLAY WS-SEPARATOR.
           STOP RUN.
       
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
           
           PERFORM 431-VALIDATE-ACCOUNT-PARA.
           
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
      * REPORTING
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