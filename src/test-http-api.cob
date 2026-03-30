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
       COPY "copybooks/api-request.cpy".
       COPY "copybooks/api-response.cpy".
       COPY "copybooks/parsed-member-info.cpy".
       01  WS-IDX               PIC 99 COMP-5.
       01  display-limit        PIC 99 COMP-5.

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
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               DISPLAY "Parsed products (active):"
               DISPLAY FUNCTION TRIM(api-response-body)
           END-IF
           DISPLAY " "

      *    Test: Fetch named products
           DISPLAY "Test: GET /api/products/named_products"

           MOVE SPACES TO api-request-data
           MOVE "xGET_NAMED_PRODUCTS" to  api-operation

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               DISPLAY "Parsed products (named):"
               DISPLAY FUNCTION TRIM(api-response-body)
           END-IF
           DISPLAY " "

      *    Test: Fetch member_id for username 'tester'
           DISPLAY "Test: GET /api/member/get_id?"
                   "username=tester (tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER_ID" TO api-operation
           MOVE "tester" TO api-username

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               DISPLAY "Member id: " FUNCTION TRIM(api-response-body)
           END-IF
           DISPLAY " "

      *    Test: Fetch user-info for member with id "1"
           DISPLAY "Test: GET /api/member?member_id={member_id}"
                   " (1, tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER" TO api-operation
           MOVE "1" TO api-member-id

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               UNSTRING api-response-body DELIMITED BY X"09"
                   INTO member-balance
                        member-username
                        member-active
                        member-name
               END-UNSTRING
               DISPLAY "Member balance: " member-balance
               DISPLAY "Member username: "
                   FUNCTION TRIM(member-username)
               DISPLAY "Member active: " FUNCTION TRIM(member-active)
               DISPLAY "Member name: " FUNCTION TRIM(member-name)
           END-IF
           DISPLAY " "

      *    Test: Fetch sales history of member with id of "1"
           DISPLAY "Test: GET /api/member/sales?member_id={member_id}"
                   " (1, tester)"

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER_SALES" TO api-operation
           MOVE "1" TO api-member-id

           CALL "STREGSYSTEM-API"
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               DISPLAY "Member sales count: " member-sales-count
               MOVE 5 TO display-limit
               IF member-sales-count < display-limit
                   MOVE member-sales-count TO display-limit
               END-IF
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > display-limit
                   DISPLAY "  " FUNCTION TRIM(sale-timestamp(WS-IDX))
                           " | " FUNCTION TRIM(sale-product(WS-IDX))
                           " | " sale-price(WS-IDX)
               END-PERFORM
           END-IF
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
               USING api-request-data api-response-data
           END-CALL

           DISPLAY "Status: " api-response-status
           IF api-response-status = 0
               DISPLAY "Sale status: " sale-status
               DISPLAY "Sale message: " FUNCTION TRIM(sale-message)
               DISPLAY "Sale cost: " sale-cost
               DISPLAY "Member balance: " sale-member-balance
               DISPLAY "Promille: " FUNCTION TRIM(sale-promille)
               DISPLAY "Ballmer peaking: "
                   FUNCTION TRIM(sale-ballmer-flag)
           END-IF
           DISPLAY " "

           STOP RUN.
