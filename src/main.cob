      ******************************************************************
      * Author: Naitsabot
      * Description:
      *     Stregsystem TUI written in GnuCOBOL
      * Version: a0.0.4
      * Version history:
      *     a0.0.1: Initial commit, basic program
      *     a0.0.2: Working basic program, and helper file corrected
      *     a0.0.3: Fixed format for punch card style COBOL
      *     a0.0.4: Added HTTP client module (not used yet)
      *     a0.1.0: Test HTTP client module (Note: CBL IO is a headache)
      *     a0.2.0: Use GNU Netcat
      *     a0.2.1: Working requests to localhost with GNU netcat
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  USER-NAME            PIC A(30).
       01  ITEM                 PIC A(30).
       01  USER-CHOICE          PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL 'HELPER'.

           DISPLAY " "
           DISPLAY "1. Manual input test"
           DISPLAY "2. Run HTTP tests"
           DISPLAY "Choose option: " WITH NO ADVANCING
           ACCEPT USER-CHOICE

           EVALUATE USER-CHOICE
               WHEN 1
                   PERFORM MANUAL-INPUT-TEST
               WHEN 2
                   PERFORM HTTP-TEST
               WHEN OTHER
                   DISPLAY "Invalid choice"
           END-EVALUATE

           STOP RUN.

       MANUAL-INPUT-TEST.
           DISPLAY "What is your username?"
           ACCEPT USER-NAME
           DISPLAY "Nice to meet you, "
                   FUNCTION TRIM(USER-NAME) "!"
           DISPLAY "What item do you want to buy?"
           ACCEPT ITEM
           DISPLAY FUNCTION TRIM(USER-NAME) " wants to buy: "
                   FUNCTION TRIM(ITEM).

       HTTP-TEST.
           DISPLAY "Starting HTTP client tests..."
           DISPLAY " "
           CALL 'HTTP-CLIENT'.
