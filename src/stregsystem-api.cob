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

      * logging control
       01 logging-control.
           05 api-init-done    PIC 9 VALUE 0.
           05 api-log-level    PIC 9 VALUE 0.
           05 api-env-val      PIC X(10).

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
               PERFORM INIT-LOGGING
               PERFORM INIT-API-CONFIG
               PERFORM INIT-TEMP-DIR
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
          IF api-log-level >= 2
              DISPLAY "Request path: " FUNCTION TRIM(req-path)
              DISPLAY "Request body: " FUNCTION TRIM(req-body)
          END-IF
           MOVE "GET" TO req-method
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

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_ACTIVE_PRODUCTS" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-ACTIVE-PRODUCTS-LIST
                   MOVE "ACTIVE" TO dict-work-source
                   PERFORM LOAD-PRODUCTS-TO-DICTIONARY
               END-IF
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
           MOVE api-url TO req-url
           MOVE "/api/products/named_products" TO req-path
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_NAMED_PRODUCTS" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-NAMED-PRODUCTS-LIST
                   MOVE "NAMED" TO dict-work-source
                   PERFORM LOAD-PRODUCTS-TO-DICTIONARY
               END-IF
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
           MOVE api-url TO req-url
           STRING "/api/member/get_id?username="
               FUNCTION TRIM(api-username) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_MEMBER_ID" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
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
           MOVE api-url TO req-url
           STRING "/api/member?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

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
           MOVE api-url TO req-url
           STRING "/api/member/sales?member_id="
               FUNCTION TRIM(api-member-id) DELIMITED BY SIZE
               INTO req-path
           END-STRING
           MOVE SPACES TO req-body

           IF api-log-level >= 2
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
           END-IF

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
               PERFORM READ-HTTP-RESPONSE
               MOVE "GET_MEMBER_SALES" TO parse-operation
               PERFORM PARSE-JSON-RESPONSE
               IF parse-status = 0
                   PERFORM PARSE-MEMBER-SALES-LIST
               END-IF
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

           IF api-log-level >= 2
               DISPLAY "POST_SALE member id: "
                   FUNCTION TRIM(api-member-id)
               DISPLAY "Request path: " FUNCTION TRIM(req-path)
               DISPLAY "Request body: " FUNCTION TRIM(req-body)
           END-IF

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

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
           MOVE api-url TO req-url
           MOVE "/test" TO req-path
           MOVE SPACES TO req-body

           CALL "HTTP-CLIENT" USING
               http-request-data
               http-response-status
           END-CALL
           MOVE http-response-status TO api-response-status

           IF http-response-status = 0
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

           CLOSE HTTP-RESPONSE-FILE.

      * PARSE-JSON-RESPONSE - Run JSON-DECODER and store output
       PARSE-JSON-RESPONSE.
           MOVE SPACES TO parsed-output
           CALL "JSON-DECODER" USING
               json-input
               parse-operation
               parsed-output
               parse-status
           END-CALL

           IF parse-status = 0
               MOVE parsed-output TO api-response-body
           ELSE
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
           END-PERFORM.

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
           END-PERFORM.

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
           END-PERFORM.

      * Product dictionary helper procedures
       COPY "copybooks/product-dict-procedures.cob".

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
           END-STRING.

       INIT-API-CONFIG.
      *    Read environment variable for API configuration
           ACCEPT api-url FROM ENVIRONMENT "STREGSYSTEM_URL"
           IF api-url = SPACES
               MOVE "https://stregsystem.fklub.dk" TO api-url
           END-IF.
