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

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT HTTP-RESPONSE-FILE
               ASSIGN TO WS-HTTP-RESPONSE-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD HTTP-RESPONSE-FILE.
       01 HTTP-RESPONSE-LINE   PIC X(8192).

       WORKING-STORAGE SECTION.
      * HTTP client request structure
       COPY "copybooks/http-request.cpy".
       COPY "copybooks/http-response-status.cpy".
       01 buystring            PIC X(100).

      * Temp file paths
       01 WS-TEMP-DIR          PIC X(256).
       01 WS-TEMP-DIR-ENV      PIC X(256).
       01 WS-HTTP-RESPONSE-PATH PIC X(256).
       01 WS-TEMP-CMD          PIC X(512).

      * JSON decoder variables
       01 json-input           PIC X(8192).
       01 parse-operation      PIC X(20).
       01 parsed-output        PIC X(8192).
       01 parse-status         PIC S9(9) COMP-5.

      * Parsed structures
       COPY "copybooks/parsed-member-info.cpy".
       COPY "copybooks/parsed-products.cpy".
       COPY "copybooks/product-dictionary.cpy".

      * Response parsing helpers
       01 response-pos        PIC 9(5) COMP-5.
       01 response-eof        PIC 9 VALUE 0.
       01 line-pos            PIC 9(5) COMP-5.
       01 product-line        PIC X(256).
       01 WS-IDX              PIC 99 COMP-5.
       01 response-bytes      PIC 9(6) COMP-5.

      * centralized logging
       COPY "copybooks/logging.cpy".

      * one-time init guard for API setup
       01 api-init-done       PIC 9 VALUE 0.

      * API configuration
       01 api-url              PIC X(200).

       LINKAGE SECTION.
       COPY "copybooks/api-request.cpy".
       COPY "copybooks/api-response.cpy".

       PROCEDURE DIVISION USING api-request-data
                                api-response-data.

       MAIN-LOGIC.
           MOVE SPACE TO http-request-data
           MOVE SPACES TO api-response-body
           MOVE 0 TO member-sales-count
           MOVE 0 TO sale-status
           MOVE 0 TO sale-cost
           MOVE 0 TO sale-member-balance
           MOVE SPACES TO sale-message

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 100
               MOVE SPACES TO sale-timestamp(WS-IDX)
               MOVE SPACES TO sale-product(WS-IDX)
               MOVE 0 TO sale-price(WS-IDX)
           END-PERFORM
           IF api-init-done = 0
               MOVE "STREGSYSTEM-API" TO log-component
               PERFORM LOG-INIT
               PERFORM INIT-API-CONFIG
               PERFORM INIT-TEMP-DIR
               MOVE 1 TO api-init-done
           END-IF

           MOVE SPACES TO log-message
           STRING "API operation: " DELIMITED BY SIZE
               FUNCTION TRIM(api-operation) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO


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
                   MOVE SPACES TO log-message
                   STRING "Unknown API operation: " DELIMITED BY SIZE
                        FUNCTION TRIM(api-operation) DELIMITED BY SIZE
                        INTO log-message
                   END-STRING
                   PERFORM LOG-ERROR
                   MOVE 1 TO api-response-status
           END-EVALUATE

           MOVE SPACES TO api-operation
           MOVE SPACES TO api-member-id
           MOVE SPACES TO api-room-id
           MOVE SPACES TO api-order
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
           MOVE SPACES TO log-message
           STRING "GET active products for room_id=" DELIMITED BY SIZE
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO
           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           STRING
               "/api/products/active_products?room_id="
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
                FUNCTION TRIM(req-path) DELIMITED BY SIZE
                INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_ACTIVE_PRODUCTS" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-ACTIVE-PRODUCTS-LIST
                   MOVE "ACTIVE" TO dict-work-source
                   PERFORM LOAD-PRODUCTS-TO-DICTIONARY
               END-IF
               MOVE "Active products fetched successfully"
                    TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Active products fetch failed" TO log-message
               PERFORM LOG-WARN
           END-IF.

      * GET /api/products/named_products
      *
      * Request: (no parameters)
      * Response: JSON list of named products key-valued to IDs
      * {
      *   "beer": 123
      * }
       API-xGET-NAMED-PRODUCTS.
           MOVE "Fetching named products" TO log-message
           PERFORM LOG-INFO

           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           MOVE "/api/products/named_products" TO req-path
           MOVE SPACES TO req-body

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
               FUNCTION TRIM(req-path) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_NAMED_PRODUCTS" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-NAMED-PRODUCTS-LIST
                   MOVE "NAMED" TO dict-work-source
                   PERFORM LOAD-PRODUCTS-TO-DICTIONARY
               END-IF
               MOVE "Named products fetched successfully" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Named products fetch failed" TO log-message
               PERFORM LOG-WARN
           END-IF.

      * GET /api/member/get_id?username={username}
      * Request: username via query parameter
      * Response: member id of the user
      * {
      *   "member_id": 321,
      * }
       API-xGET-MEMBER-ID.
           MOVE SPACES TO log-message
           STRING "Fetching member id for username " DELIMITED BY SIZE
                FUNCTION TRIM(api-username) DELIMITED BY SIZE
                INTO log-message
           END-STRING
           PERFORM LOG-INFO

           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           STRING "/api/member/get_id?username="
               FUNCTION TRIM(api-username) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
               FUNCTION TRIM(req-path) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_MEMBER_ID" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               MOVE "Member id fetched successfully" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Member id fetch failed" TO log-message
               PERFORM LOG-WARN
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
           MOVE SPACES TO log-message
           STRING "Fetching member info for id " DELIMITED BY SIZE
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO

           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           STRING "/api/member?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
               FUNCTION TRIM(req-path) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_MEMBER" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   UNSTRING parsed-output DELIMITED BY X"09"
                       INTO member-balance
                            member-username
                            member-active
                            member-name
                   END-UNSTRING
               END-IF
               MOVE "Member info fetched successfully" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Member info fetch failed" TO log-message
               PERFORM LOG-WARN
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
           MOVE SPACES TO log-message
           STRING "Fetching sales for member id " DELIMITED BY SIZE
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO

           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           STRING "/api/member/sales?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
               FUNCTION TRIM(req-path) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_MEMBER_SALES" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-MEMBER-SALES-LIST
               END-IF
               MOVE "Member sales fetched successfully" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Member sales fetch failed" TO log-message
               PERFORM LOG-WARN
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
           MOVE SPACES TO log-message
           STRING "Creating sale for room " DELIMITED BY SIZE
               FUNCTION TRIM(api-room-id) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO

      *    Build sale endpoint path: /api/sale
           MOVE "/api/sale" TO req-path

           MOVE "POST" TO req-method
           MOVE api-url TO req-url

      *    Build buystring: username + space + product-id
           MOVE SPACES TO buystring
           STRING
               FUNCTION TRIM(api-username) DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               FUNCTION TRIM(api-order) DELIMITED BY SIZE
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

           MOVE SPACES TO log-message
           STRING "POST_SALE member id: " DELIMITED BY SIZE
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           MOVE SPACES TO log-message
           STRING "Request path: " DELIMITED BY SIZE
               FUNCTION TRIM(req-path) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           MOVE SPACES TO log-message
           STRING "Request body: " DELIMITED BY SIZE
               FUNCTION TRIM(req-body) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-TRACE

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           MOVE SPACES TO log-message
           MOVE http-response-status TO log-num-text
           STRING "HTTP status: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "POST_SALE" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   UNSTRING parsed-output DELIMITED BY X"09"
                       INTO sale-status
                            sale-message
                            sale-cost
                            sale-member-balance
                   END-UNSTRING
               END-IF
               MOVE "Sale created successfully" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Sale creation failed" TO log-message
               PERFORM LOG-WARN
               MOVE http-response-status TO sale-status
               MOVE http-response-status TO log-num-text
               STRING "HTTP request failed (status " DELIMITED BY SIZE
                   log-num-text DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
                   INTO sale-message
               END-STRING
           END-IF.

       API-xGET-TEST.
           MOVE "Calling test endpoint" TO log-message
           PERFORM LOG-INFO
           MOVE "GET" TO req-method
           MOVE api-url TO req-url
           MOVE "/test" TO req-path
           MOVE SPACES TO req-body

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
               MOVE "Test endpoint call successful" TO log-message
               PERFORM LOG-INFO
           ELSE
               MOVE "Test endpoint call failed" TO log-message
               PERFORM LOG-WARN
           END-IF.

      * READ-HTTP-RESPONSE - Load curl output into json-input
       READ-HTTP-RESPONSE.
           MOVE SPACES TO json-input
           MOVE 1 TO response-pos
           MOVE 0 TO response-eof

           OPEN INPUT HTTP-RESPONSE-FILE

           PERFORM UNTIL response-eof = 1
               MOVE SPACES TO HTTP-RESPONSE-LINE
               READ HTTP-RESPONSE-FILE
                   AT END
                       MOVE 1 TO response-eof
                   NOT AT END
                       INSPECT HTTP-RESPONSE-LINE
                           REPLACING ALL LOW-VALUE BY SPACE
                       INSPECT HTTP-RESPONSE-LINE
                           REPLACING ALL X"0D" BY SPACE
                       IF FUNCTION TRIM(HTTP-RESPONSE-LINE) NOT = SPACES
                           STRING
                               FUNCTION TRIM(HTTP-RESPONSE-LINE)
                               DELIMITED BY SIZE
                               INTO json-input
                               WITH POINTER response-pos
                           END-STRING
                       END-IF
               END-READ
           END-PERFORM

           CLOSE HTTP-RESPONSE-FILE

           COMPUTE response-bytes = response-pos - 1
           MOVE SPACES TO log-message
           MOVE response-bytes TO log-num-text
           STRING "HTTP response bytes: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      * PARSE-JSON-RESPONSE - Run JSON-DECODER and store output
       PARSE-JSON-RESPONSE.
           MOVE SPACES TO parsed-output
           MOVE SPACES TO log-message
           STRING "Parsing JSON operation: " DELIMITED BY SIZE
               FUNCTION TRIM(parse-operation) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           IF FUNCTION TRIM(json-input) = SPACES
               MOVE 4 TO parse-status
               MOVE "Empty HTTP response body" TO log-message
               PERFORM LOG-WARN
               MOVE parse-status TO api-response-status
               EXIT PARAGRAPH
           END-IF
           CALL "JSON-DECODER" USING
               json-input
               parse-operation
               parsed-output
               parse-status
           END-CALL

           IF parse-status = 0
               MOVE parsed-output TO api-response-body
               MOVE "JSON parse OK" TO log-message
               PERFORM LOG-DEBUG
           ELSE
               MOVE SPACES TO log-message
               MOVE parse-status TO log-num-text
               STRING "JSON parse failed: " DELIMITED BY SIZE
                   log-num-text DELIMITED BY SIZE
                   INTO log-message
               END-STRING
               PERFORM LOG-WARN
               MOVE parse-status TO api-response-status
           END-IF.

      * PARSE-ACTIVE-PRODUCTS-LIST - Parse tab-delimited lines
       PARSE-ACTIVE-PRODUCTS-LIST.
           MOVE 0 TO products-count
           MOVE 1 TO line-pos

           PERFORM UNTIL line-pos >
               FUNCTION LENGTH(FUNCTION TRIM(parsed-output))
               MOVE SPACES TO product-line
               UNSTRING parsed-output DELIMITED BY X"0A"
                   INTO product-line
                   WITH POINTER line-pos
               END-UNSTRING

               IF FUNCTION TRIM(product-line) NOT = SPACES
                   IF products-count < 100
                       ADD 1 TO products-count
                       UNSTRING product-line DELIMITED BY X"09"
                           INTO prod-id(products-count)
                                prod-name(products-count)
                                prod-price(products-count)
                       END-UNSTRING
                   END-IF
               END-IF
           END-PERFORM

           MOVE SPACES TO log-message
           MOVE products-count TO log-num-text
           STRING "Parsed active products: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      * PARSE-NAMED-PRODUCTS-LIST - Parse tab-delimited lines
       PARSE-NAMED-PRODUCTS-LIST.
           MOVE 0 TO products-count
           MOVE 1 TO line-pos

           PERFORM UNTIL line-pos >
               FUNCTION LENGTH(FUNCTION TRIM(parsed-output))
               MOVE SPACES TO product-line
               UNSTRING parsed-output DELIMITED BY X"0A"
                   INTO product-line
                   WITH POINTER line-pos
               END-UNSTRING

               IF FUNCTION TRIM(product-line) NOT = SPACES
                   IF products-count < 100
                       ADD 1 TO products-count
                       UNSTRING product-line DELIMITED BY X"09"
                           INTO prod-name(products-count)
                                prod-id(products-count)
                       END-UNSTRING
                       MOVE 0 TO prod-price(products-count)
                   END-IF
               END-IF
           END-PERFORM

           MOVE SPACES TO log-message
           MOVE products-count TO log-num-text
           STRING "Parsed named products: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      * PARSE-MEMBER-SALES-LIST - Parse tab-delimited lines
       PARSE-MEMBER-SALES-LIST.
           MOVE 0 TO member-sales-count
           MOVE 1 TO line-pos

           PERFORM UNTIL line-pos >
               FUNCTION LENGTH(FUNCTION TRIM(parsed-output))
               MOVE SPACES TO product-line
               UNSTRING parsed-output DELIMITED BY X"0A"
                   INTO product-line
                   WITH POINTER line-pos
               END-UNSTRING

               IF FUNCTION TRIM(product-line) NOT = SPACES
                   IF member-sales-count < 100
                       ADD 1 TO member-sales-count
                       UNSTRING product-line DELIMITED BY X"09"
                           INTO sale-timestamp(member-sales-count)
                                sale-product(member-sales-count)
                                sale-price(member-sales-count)
                       END-UNSTRING
                   END-IF
               END-IF
           END-PERFORM

           MOVE SPACES TO log-message
           MOVE member-sales-count TO log-num-text
           STRING "Parsed member sales: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      * Product dictionary helper procedures
       COPY "copybooks/product-dict-procedures.cob".

      * Logging procedures
       COPY "copybooks/logging-procedures.cob".

       INIT-TEMP-DIR.
           MOVE SPACES TO WS-TEMP-DIR-ENV
           ACCEPT WS-TEMP-DIR-ENV FROM ENVIRONMENT "XDG_RUNTIME_DIR"
           IF FUNCTION TRIM(WS-TEMP-DIR-ENV) = SPACES
               ACCEPT WS-TEMP-DIR-ENV FROM ENVIRONMENT "TMPDIR"
           END-IF
           IF FUNCTION TRIM(WS-TEMP-DIR-ENV) = SPACES
               MOVE "/tmp" TO WS-TEMP-DIR-ENV
           END-IF

           MOVE SPACES TO WS-TEMP-DIR
           STRING
               FUNCTION TRIM(WS-TEMP-DIR-ENV) DELIMITED BY SIZE
               "/stregsystem-tui" DELIMITED BY SIZE
               INTO WS-TEMP-DIR
           END-STRING

           MOVE SPACES TO WS-TEMP-CMD
           STRING
               "mkdir -p " DELIMITED BY SIZE
               FUNCTION TRIM(WS-TEMP-DIR) DELIMITED BY SIZE
               INTO WS-TEMP-CMD
           END-STRING
           CALL "SYSTEM" USING WS-TEMP-CMD
           END-CALL

           MOVE SPACES TO WS-HTTP-RESPONSE-PATH
           STRING
               FUNCTION TRIM(WS-TEMP-DIR) DELIMITED BY SIZE
               "/http-response.txt" DELIMITED BY SIZE
               INTO WS-HTTP-RESPONSE-PATH
           END-STRING

           MOVE SPACES TO log-message
           STRING "HTTP response path: " DELIMITED BY SIZE
               FUNCTION TRIM(WS-HTTP-RESPONSE-PATH) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

       INIT-API-CONFIG.
      *    Read environment variable for API configuration
           ACCEPT api-url FROM ENVIRONMENT "STREGSYSTEM_URL"
           IF api-url = SPACES
               MOVE "https://stregsystem.fklub.dk" TO api-url
           END-IF

           MOVE SPACES TO log-message
           STRING "API base URL: " DELIMITED BY SIZE
               FUNCTION TRIM(api-url) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO
           .
