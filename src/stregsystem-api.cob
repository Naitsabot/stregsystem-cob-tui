      ******************************************************************
      * Author: Naitsabot
      * Purpose: Stregsystem API interface
      * Description:
      *     High-level API calls for stregsystem operations
      *     Uses the generic HTTP-CLIENT module
      *     Supports real stregsystem endpoints
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STREGSYSTEM-API.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * HTTP client request structure
       01  http-request.
           05  req-method       PIC X(10).
           05  req-host         PIC X(100). *> Dep. nc
           05  req-port         PIC X(4). *> Dep. nc
           05  req-url          PIC X(200).
           05  req-path         PIC X(200).
           05  req-body         PIC X(1000).
       01  http-status          PIC S9(9) COMP-5.
       01  buystring            PIC X(100).

      * logging control
       01  logging-control.
           05  api-init-done    PIC 9 VALUE 0.
           05  api-log-level    PIC 9 VALUE 0.
           05  api-env-val      PIC X(10).

      * API configuration
       01  api-host             PIC X(100) VALUE "127.0.0.1". *> Dep.
       01  api-port             PIC X(4) VALUE "8000". *> Dep.
       01  api-url              PIC X(200) VALUE"http://localhost:8000".

       LINKAGE SECTION.
       01  api-request-data.
           05  api-operation    PIC X(20).
           05  api-member-id    PIC X(5).
           05  api-room-id      PIC X(5).
           05  api-product-id   PIC X(5).
           05  api-username     PIC X(30).
       01  api-response-status  PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING api-request-data
                                api-response-status.

       MAIN-LOGIC.
           MOVE SPACE TO http-request
           IF api-init-done = 0
               PERFORM INIT-LOGGING
           END-IF


           EVALUATE api-operation
               WHEN "xPOST_SALE"
                   PERFORM API-xPOST-SALE
               WHEN "xGET_ACTIVE_PRODUCTS"
                   PERFORM API-xGET-ACTIVE-PRODUCTS
               WHEN "xGET_NAMED_PRODUCTS"
                   PERFORM API-xGET-NAMED-PRODUCTS
               WHEN "xGET_MEMBER_ID"
                   PERFORM API-xGET-MEMBER-ID
               WHEN "xGET_MEMBER"
                   PERFORM API-xGET-MEMBER
               WHEN "xGET_MEMBER_SALES"
                   PERFORM API-xGET-MEMBER-SALES
               WHEN "TEST"
                   PERFORM API-xGET-TEST
               WHEN OTHER
                   DISPLAY "Unknown API operation: " api-operation
                   MOVE 1 TO api-response-status
           END-EVALUATE

           MOVE SPACES TO api-operation
           MOVE SPACES TO api-member-id
           MOVE SPACES TO api-room-id
           MOVE SPACES TO api-product-id
           MOVE SPACES TO api-username

           GOBACK.

      * GET /api/products/active_products?room_id={room_id}
      * Request: room_id via query parameter
      * Response: dictionary of active products for the room
      * {
      *   "123": {
      *     "name": "Beer",
      *     "price": 600
      *   },
      *   "124": {
      *     "name": "Beer2",
      *     "price": 650
      *   }
      * }
       API-xGET-ACTIVE-PRODUCTS.
           IF api-log-level >= 1
               DISPLAY "Fetching active products for room "
               api-room-id "..."
           END-IF

           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           STRING
               "/api/products/active_products?room_id="
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Active products fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Active products fetch failed"
               END-IF
           END-IF.

      * GET /api/products/named_products
      *
      * Request: (no parameters)
      * Response: JSON list of named products key-valued to IDs
      * {
      *   "beer": 123
      * }
       API-xGET-NAMED-PRODUCTS.
           IF api-log-level >= 1
               DISPLAY "Fetching named products..."
           END-IF

           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           MOVE "/api/products/named_products" TO req-path
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Named products fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Named products fetch failed"
               END-IF
           END-IF.

      * GET /api/member/get_id?username={username}
      * Request: username via query parameter
      * Response: member id of the user
      * {
      *   "member_id": 321,
      * }
       API-xGET-MEMBER-ID.
           IF api-log-level >= 1
               DISPLAY "Fetching member id for username "
               api-username "..."
           END-IF

           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           STRING "/api/member/get_id?username="
               FUNCTION TRIM(api-username) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member id fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member id fetch failed"
               END-IF
           END-IF.

      * GET /api/member?member_id={member_id}
      * Request: member id via query parameter
      * Response:
      * {
      *   "balance": 20000,
      *   "username": "kresten",
      *   "active": true,
      *   "name": "Kresten Laust"
      * }
       API-xGET-MEMBER.
           IF api-log-level >= 1
               DISPLAY "Fetching member info for id "
                       api-member-id "..."
           END-IF

           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           STRING "/api/member?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member info fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member info fetch failed"
               END-IF
           END-IF.

      * GET /api/member/sales?member_id={member_id}
      * Request: member id via query parameter
      * Response: list of a member's purchases
      * {
      *   "sales": [
      *     {
      *       "timestamp": "2004-01-07T15:30:55Z",
      *       "product": "Beer",
      *       "price": 600
      *     }
      *   ]
      * }
       API-xGET-MEMBER-SALES.
           IF api-log-level >= 1
               DISPLAY "Fetching sales for member id "
                       api-member-id "..."
           END-IF

           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           STRING "/api/member/sales?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member sales fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Member sales fetch failed"
               END-IF
           END-IF.

      * POST /api/sale
      * Request body:
      * {
      *   "member_id": 321,
      *   "buystring": "kresten beer:3",
      *   "room": 10
      * }
      * Response:
      * {
      *   "status": 200,
      *   "msg": "OK",
      *   "values": {
      *     "order": {
      *       "room": 10,
      *       "member": 321,
      *       "created_on": "2024-05-12T18:26:09.508Z",
      *       "items": [
      *         123,
      *         123,
      *         123
      *       ]
      *     },
      *     "promille": 0.2,
      *     "is_ballmer_peaking": false,
      *     "bp_minutes": null,
      *     "bp_seconds": null,
      *     "caffeine": 2,
      *     "cups": 4,
      *     "product_contains_caffeine": true,
      *     "is_coffee_master": false,
      *     "cost": 1800,
      *     "give_multibuy_hint": true,
      *     "sale_hints":
      *       "<span class=\"username\">kresten</span> 123:3",
      *     "member_has_low_balance": false,
      *     "member_balance": 182
      *   }
      * }
       API-xPOST-SALE.
           IF api-log-level >= 1
               DISPLAY "Creating sale for room " api-room-id "..."
           END-IF

      *    Build sale endpoint path: /api/sale
           MOVE "/api/sale" TO req-path

           MOVE "POST" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url

      *    Build buystring: username + space + product-id
           MOVE SPACES TO buystring
           STRING
               FUNCTION TRIM(api-username) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(api-product-id) DELIMITED BY SIZE
               INTO buystring
           END-STRING

      *    Build JSON body:
      *    {"member_id":..., "buystring":..., "room":...}
           MOVE SPACES TO req-body
           STRING
               '{"member_id":' DELIMITED BY SIZE
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               ',"buystring":"' DELIMITED BY SIZE
               FUNCTION TRIM(buystring) DELIMITED BY SIZE
               '","room":' DELIMITED BY SIZE
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
               '}' DELIMITED BY SIZE
               INTO req-body
           END-STRING

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
               DISPLAY "Request body: " FUNCTION TRIM(req-body)
           END-IF

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Sale created successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Sale creation failed"
               END-IF
           END-IF.

       API-xGET-TEST.
           IF api-log-level >= 1
               DISPLAY "Calling test endpoint..."
           END-IF
           MOVE "GET" TO req-method
      *     MOVE api-host TO req-host
      *     MOVE api-port TO req-port
           MOVE api-url TO req-url
           MOVE "/test" TO req-path
           MOVE SPACES TO req-body

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL
           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Test endpoint call successful"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY " "
                   DISPLAY "Test endpoint call failed"
               END-IF
           END-IF.

       INIT-LOGGING.
      *    Read environment variable COB_HTTP_CLIENT_LOG;
      *    interpret 0=none,1=minimal,2=verbose
           ACCEPT api-env-val FROM ENVIRONMENT "COB_HTTP_CLIENT_LOG"
           MOVE FUNCTION TRIM(api-env-val) TO api-env-val
           MOVE FUNCTION UPPER-CASE(api-env-val) TO api-env-val
           IF api-env-val = "2"
               MOVE 2 TO api-log-level
           ELSE IF api-env-val = "TRUE"
               MOVE 2 TO api-log-level
           ELSE IF api-env-val = "1"
               MOVE 1 TO api-log-level
           ELSE IF api-env-val = "MINIMAL"
               MOVE 1 TO api-log-level
           ELSE
               MOVE 0 TO api-log-level
           END-IF
           MOVE 1 TO api-init-done
           . *> end of function
