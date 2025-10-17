      ******************************************************************
      * Author: Naitsabot
      * Description:
      *     Stregsystem TUI written in GnuCOBOL
      * Version: a0.2.3
      * Version history:
      *     a0.0.1: Initial commit, basic program
      *     a0.0.2: Working basic program, and helper file corrected
      *     a0.0.3: Fixed format for punch card style COBOL
      *     a0.0.4: Added HTTP client module (not used yet)
      *     a0.1.0: Test HTTP client module (Note: CBL IO is a headache)
      *     a0.2.0: Use GNU Netcat
      *     a0.2.1: Working requests to localhost with GNU netcat
      *     a0.2.2: Very minor changes in strings
      *     a0.2.3: Formatting
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  user-name            PIC A(30).
       01  item                 PIC A(30).
       01  user-choice          PIC 9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL "HELPER".

           DISPLAY " "
           DISPLAY "1. Manual input test"
           DISPLAY "2. Run HTTP tests"
           DISPLAY "Choose option: " WITH NO ADVANCING
           ACCEPT user-choice

           EVALUATE user-choice
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
           ACCEPT user-name
           DISPLAY "Nice to meet you, "
                   FUNCTION TRIM(user-name) "!"
           DISPLAY "What item do you want to buy?"
           ACCEPT item
           DISPLAY FUNCTION TRIM(user-name) " wants to buy: "
                   FUNCTION TRIM(item).

       HTTP-TEST.
           DISPLAY "Starting HTTP client tests..."
           DISPLAY " "
           CALL 'HTTP-CLIENT'.
