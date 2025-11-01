      ******************************************************************
      * Author: Naitsabot
      * Purpose: Test program for Stregsystem API
      * Description:
      *     Demonstrates usage of stregsystem-api module
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-HTTP-API.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  api-request-data.
           05  api-operation    PIC X(20).
           05  api-member-id    PIC X(5).
           05  api-room-id      PIC X(5).
           05  api-product-id   PIC X(5).
           05  api-username     PIC X(30).
       01  api-response-status  PIC S9(9) COMP-5.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "=== Testing Stregsystem API ==="
           DISPLAY " "

      *    Test: Fetch active products for room 1
           DISPLAY "Test: GET /api/products/active_products?"
                   "room_id={room_id} (Room 1)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_ACTIVE_PRODUCTS" to  api-operation
           MOVE "1" to api-room-id

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "

      *    Test: Fetch named products
           DISPLAY "Test: GET /api/products/named_products"

           MOVE SPACES TO api-request-data
           MOVE "xGET_NAMED_PRODUCTS" to  api-operation

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "

      *    Test: Fetch member_id for username 'tester'
           DISPLAY "Test: GET /api/member/get_id?"
                   "username=tester (tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER_ID" TO api-operation
           MOVE "tester" TO api-username

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "

      *    Test: Fetch user-info for member with id "1"
           DISPLAY "Test: GET /api/member?member_id={member_id}"
                   " (1, tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER" TO api-operation
           MOVE "1" TO api-member-id

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "

      *    Test: Fetch sales history of member with id of "1"
           DISPLAY "Test: GET /api/member/sales?member_id={member_id}"
                   " (1, tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER_SALES" TO api-operation
           MOVE "1" TO api-member-id

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "


      *    Test: Create a sale (JSON to /api/sale)
           DISPLAY "Test: POST /api/sale (Room 1, Product 14, tester)"

           MOVE SPACES TO api-request-data
           MOVE "xPOST_SALE" TO api-operation
           MOVE "1" TO api-member-id
           MOVE "1" TO api-room-id
           MOVE "14" TO api-product-id
           MOVE "tester" TO api-username

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-status
           END-CALL

           DISPLAY "Status: " api-response-status
           DISPLAY " "

           STOP RUN.
