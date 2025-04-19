      ******************************************************************
      * PNC BANK COBOL DEMO APPLICATION - COMMON DATA DEFINITIONS
      * 
      * This copybook contains common data definitions used across
      * multiple modules in the banking application.
      ******************************************************************
       
      * File Definitions
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
       
      * Common Working Storage Items
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