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
      *     a0.4.3: Load configs, and refactor temp files.
      *     a0.4.4; Add active produts screen.
      *     a0.4.5: Even cooler TUI.
      *     a0.4.6: refactor weird coding convention.
      *             add more lines to kiosk screen.
      *             Changed colours.
      *      1.0.0: Fully working system!
      *             Added Success/fail screen.
      *             Added buy logic when buying.
      *      1.1.0: Added install and uninstall method. played with env.
      *      1.1.1: Fix formatting a bit
      *      1.2.0: Add a ton of logging. Fix some TUI formatting.
      *             Remove redundant code
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           CALL "TUI".
           STOP RUN.
