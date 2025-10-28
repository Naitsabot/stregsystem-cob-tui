      ******************************************************************
      * Author: Naitsabot
      * Purpose: Test program for Stregsystem API
      * Description:
      *     Demonstrates usage of stregsystem-api module
      *     Tests real stregsystem sale endpoint
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-HTTP-API.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  api-request.
           05  api-operation    PIC X(20).
           05  api-room-id      PIC X(5).
           05  api-product-id   PIC X(5).
           05  api-username     PIC X(30).
       01  api-status           PIC S9(9) COMP-5.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=== Testing Stregsystem API ==="
           DISPLAY " "

      *    Test: Create a sale (JSON to /api/sale)
           DISPLAY "Test: POST /api/sale (Room 1, Product 14, tester)"
           MOVE "SALE" TO api-operation
           MOVE "1" TO api-room-id
           MOVE "14" TO api-product-id
           MOVE "tester" TO api-username

           CALL "STREGSYSTEM-API" USING api-request api-status
           END-CALL

           DISPLAY "Status: " api-status
           DISPLAY " "

      *    Another sale test (JSON to /api/sale)
           DISPLAY "Test: POST /api/sale (Room 1, Product 44, tester)"
           MOVE "SALE" TO api-operation
           MOVE "1" TO api-room-id
           MOVE "44" TO api-product-id
           MOVE "tester" TO api-username

           CALL "STREGSYSTEM-API" USING api-request api-status
           END-CALL

           DISPLAY "Status: " api-status.

           STOP RUN.
