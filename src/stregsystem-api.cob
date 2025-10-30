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
           05  req-host         PIC X(100).
           05  req-port         PIC X(4).
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
       01  api-host             PIC X(100) VALUE "127.0.0.1".
       01  api-port             PIC X(4) VALUE "8000".

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
           IF api-init-done = 0
               PERFORM INIT-LOGGING
           END-IF
           EVALUATE api-operation
               WHEN "SALE"
                   PERFORM API-SALE
               WHEN "ACTIVE_PRODUCTS"
                   PERFORM API-ACTIVE-PRODUCTS
               WHEN "NAMED_PRODUCTS"
                   PERFORM API-NAMED-PRODUCTS
               WHEN "GET_MEMBER_ID"
                   PERFORM API-GET-MEMBER-ID
               WHEN "GET_MEMBER"
                   PERFORM API-GET-MEMBER
               WHEN "GET_MEMBER_SALES"
                   PERFORM API-GET-MEMBER-SALES
               WHEN "TEST"
                   PERFORM API-TEST
               WHEN OTHER
                   DISPLAY "Unknown API operation: " api-operation
                   MOVE 1 TO api-response-status
           END-EVALUATE

           GOBACK.
      * GET /api/products/active_products?room_id={room_id}
       API-ACTIVE-PRODUCTS.
           IF api-log-level >= 1
               DISPLAY "Fetching active products for room " api-room-id "..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
           STRING "/api/products/active_products?room_id="
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
                   DISPLAY "Active products fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Active products fetch failed"
               END-IF
           END-IF.

      * GET /api/products/named_products
       API-NAMED-PRODUCTS.
           IF api-log-level >= 1
               DISPLAY "Fetching named products..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
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
                   DISPLAY "Named products fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Named products fetch failed"
               END-IF
           END-IF.

      * GET /api/member/get_id?username={username}
       API-GET-MEMBER-ID.
           IF api-log-level >= 1
               DISPLAY "Fetching member id for username " api-username "..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
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
                   DISPLAY "Member id fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Member id fetch failed"
               END-IF
           END-IF.

      * GET /api/member?member_id={member_id}
       API-GET-MEMBER.
           IF api-log-level >= 1
               DISPLAY "Fetching member info for id " api-member-id "..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
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
                   DISPLAY "Member info fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Member info fetch failed"
               END-IF
           END-IF.

      * GET /api/member/sales?member_id={member_id}
       API-GET-MEMBER-SALES.
           IF api-log-level >= 1
               DISPLAY "Fetching sales for member id " api-member-id "..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
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
                   DISPLAY "Member sales fetched successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Member sales fetch failed"
               END-IF
           END-IF.

       API-SALE.
           IF api-log-level >= 1
               DISPLAY "Creating sale for room " api-room-id "..."
           END-IF

      *    Build sale endpoint path: /api/sale
           MOVE "/api/sale" TO req-path

           MOVE "POST" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port

      *    Build buystring: username + space + product-id
           MOVE SPACES TO buystring
           STRING
               api-username DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               api-product-id DELIMITED BY SIZE
               INTO buystring
           END-STRING

      *    Build JSON body:
      *    {"member_id":..., "buystring":..., "room":...}
           MOVE SPACES TO req-body
           STRING
               '{"member_id":' DELIMITED BY SIZE
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
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
                   DISPLAY "Sale created successfully"
               END-IF
           ELSE
               IF api-log-level >= 1
                   DISPLAY "Sale creation failed"
               END-IF
           END-IF.

       API-TEST.
           IF api-log-level >= 1
               DISPLAY "Calling test endpoint..."
           END-IF
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
           MOVE "/test" TO req-path
           MOVE SPACES TO req-body

           CALL "HTTP-CLIENT" USING http-request http-status
               END-CALL

           MOVE http-status TO api-response-status

           IF http-status = 0
               IF api-log-level >= 1
                   DISPLAY "Test endpoint call successful"
               END-IF
           ELSE
               IF api-log-level >= 1
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
           .
