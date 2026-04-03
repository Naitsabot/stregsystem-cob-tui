      ******************************************************************
      * Author: Naitsabot
      * Description:
      *     Stregsystem TUI written in GnuCOBOL
      * Version: a0.3.3
      * Version history:
      *     a0.0.1: Initial commit, basic program.
      *     a0.0.2: Working basic program, and helper file corrected.
      *     a0.0.3: Fixed format for punch card style COBOL.
      *     a0.0.4: Added HTTP client module (not used yet).
      *     a0.1.0: Test HTTP client module (Note:CBL IO is a headache).
      *     a0.2.0: Use GNU Netcat.
      *     a0.2.1: Working requests to localhost with GNU netcat.
      *     a0.2.2: Very minor changes in strings.
      *     a0.2.3: Formatting.
      *     a0.2.4: Genreallized nc http-client functionality.
      *     a0.2.5: Updated general nc functionality + logging levels.
      *     a0.2.6: Added structure for more API endpoints.
      *     a0.2.7: Logic and tests for all theroretically needed
      *             stregsystem-endpoints for the application.
      *     a0.2.8: Change from transportation layer comms. protoocl
      *             to application layer protocol. Netcat -> Curl.
      *             (reduces overhead, removed HTTP build painpoint).
      *     a0.3.0: Beginning of JSON encoding and decoding.
      *     a0.3.1: Introduces copybooks for linkage sections.
      *     a0.3.2: Fix configurations. Added enc support.
      *     a0.3.3: Added JSON encoding and decoding tests.
      *     a0.4.0: Integrate JSON encoding and decoding into API calls.
      *             decoding added. updated tests. added copybooks.
      *     a0.4.1: Add simple screen section example.
      *     a0.4.2: Add first part of TUI using SCREEN SECTION.
      *     a0.4.3: Load configs, and refactor temp files
      *     a0.4.4; Add active produts screen
      *     a0.4.5: Even cooler TUI
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
       01  test-thing           PIC X(4) VALUE "8000".

       SCREEN SECTION.
       01  SIMPLE-SCREEN.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 5 VALUE "Screen section example".
           05 LINE 4 COLUMN 5 VALUE "Press Enter to continue.".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL "TUI".

      *    DISPLAY " "
      *    DISPLAY "1. Manual input test"
      *    DISPLAY "2. Run SCREEN SECTION test"
      *    DISPLAY "Choose option: " WITH NO ADVANCING
      *    ACCEPT user-choice
      *
      *    EVALUATE user-choice
      *        WHEN 1
      *            PERFORM MANUAL-INPUT-TEST
      *        WHEN 2
      *            PERFORM SCREEN-EXAMPLE
      *        WHEN OTHER
      *            DISPLAY "Invalid choice"
      *    END-EVALUATE

           STOP RUN.

       SCREEN-EXAMPLE.
           DISPLAY simple-screen
           ACCEPT simple-screen.

       MANUAL-INPUT-TEST.
           DISPLAY "What is your username?"
           ACCEPT user-name
           DISPLAY "Nice to meet you, "
                   FUNCTION TRIM(user-name) "!"
           DISPLAY "What item do you want to buy?"
           ACCEPT item
           DISPLAY FUNCTION TRIM(user-name) " wants to buy: "
                   FUNCTION TRIM(item).
