# PNC BANK COBOL DEMO APPLICATION

## Application Overview
This demo application demonstrates a banking application with core banking functionalities including account management, transaction processing, and reporting.

## Modular Structure
The application is divided into multiple modules to promote better organization, maintainability, and reusability:

1. **MAIN.cbl** - Main program that controls the overall flow and calls appropriate modules
2. **ACCOUNT.cbl** - Account management module (create, view, delete accounts)
3. **TRANSACT.cbl** - Transaction processing module (deposits, withdrawals, transfers)
4. **REPORTS.cbl** - Reporting module (account listings and summaries)
5. **DATADEF.cpy** - Common data definitions used across modules (copybook)

## File Structure
- `MAIN.cbl` - Main program entry point
- `ACCOUNT.cbl` - Account management functionality
- `TRANSACT.cbl` - Transaction processing functionality
- `REPORTS.cbl` - Reporting functionality
- `DATADEF.cpy` - Shared data definitions (copybook)
- `ACCOUNTS.dat` - Data file containing account records

## Compilation and Execution
To compile and run the application, use your COBOL compiler:

```
cobc -x -o BANKING MAIN.cbl ACCOUNT.cbl TRANSACT.cbl REPORTS.cbl
./BANKING
```

## Demo Mode
The application includes a demo mode that pre-loads sample accounts for testing purposes.

## Features
- Account Management (create, view, delete)
- Transaction Processing (deposits, withdrawals, transfers)
- Balance Inquiry
- Reporting (account listing, account summary)

## Notes
- This is a demonstration application without proper security features
- File operations use sequential access method
- Data persistence is through flat files
