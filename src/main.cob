      ******************************************************************
      * Author: Naitsabot
      * Description:
      *     Stregsystem TUI written in GnuCOBOL
      * Version: a0.0.3
      * Version history:
      *     a0.0.1: Initial commit, basic program
      *     a0.0.2: Working basic program, and helper file corrected
      *     a0.0.3: Fixed format for punch card style COBOL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
      * Functions in use: TRIM
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  USER-NAME            PIC A(30).
       01  ITEM                 PIC A(30).
       PROCEDURE DIVISION.
           CALL 'HELPER'.
           DISPLAY "Hello, what is your username?" UPON CONSOLE.
           ACCEPT USER-NAME.
           DISPLAY "Nice to meet you, " USER-NAME "!".
           DISPLAY "What item do you want to buy?" UPON CONSOLE.
           ACCEPT ITEM.
           DISPLAY FUNCTION TRIM(USER-NAME) " wants to buy: "
                   FUNCTION TRIM(ITEM).
           STOP RUN.
