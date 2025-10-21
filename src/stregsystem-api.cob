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
           05  req-port         PIC X(5).
           05  req-path         PIC X(200).
           05  req-body         PIC X(1000).
       01  http-status          PIC S9(9) COMP-5.
       01  buystring            PIC X(100).

      * API configuration
       01  api-host             PIC X(100) VALUE "127.0.0.1".
       01  api-port             PIC X(4) VALUE "8000".

       LINKAGE SECTION.
       01  api-request-data.
           05  api-operation    PIC X(20).
           05  api-room-id      PIC 9(5).
           05  api-product-id   PIC 9(5).
           05  api-username     PIC X(30).
       01  api-response-status  PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING api-request-data
                                api-response-status.

       MAIN-LOGIC.
           EVALUATE api-operation
               WHEN "SALE"
                   PERFORM API-SALE
               WHEN "TEST"
                   PERFORM API-TEST
               WHEN OTHER
                   DISPLAY "Unknown API operation: " api-operation
                   MOVE 1 TO api-response-status
           END-EVALUATE

           GOBACK.

       API-SALE.
           DISPLAY "Creating sale for room " api-room-id "..."

      *    Build sale endpoint path: /api/sale
           MOVE "/api/sale" TO req-path

           MOVE "POST" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port

      *    Build buystring: username + space + product-id
           MOVE SPACES TO buystring
           STRING
               api-username DELIMITED BY SPACE
               " " DELIMITED BY SIZE
               api-product-id DELIMITED BY SPACE
               INTO buystring
           END-STRING

      *    Build JSON body: {"member_id":..., "buystring":..., "room":...}
           MOVE SPACES TO req-body
           STRING
               '{"member_id":' DELIMITED BY SIZE
               api-room-id DELIMITED BY SPACE
               ',"buystring":"' DELIMITED BY SIZE
               buystring DELIMITED BY SPACE
               '","room":' DELIMITED BY SIZE
               api-room-id DELIMITED BY SPACE
               '}' DELIMITED BY SIZE
               INTO req-body
           END-STRING

           DISPLAY "Request path: " FUNCTION TRIM(req-path)
           DISPLAY "Request body: " FUNCTION TRIM(req-body)

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL

           MOVE http-status TO api-response-status

           IF http-status = 0
               DISPLAY "Sale created successfully"
           ELSE
               DISPLAY "Sale creation failed"
           END-IF.

       API-TEST.
           DISPLAY "Calling test endpoint..."
           MOVE "GET" TO req-method
           MOVE api-host TO req-host
           MOVE api-port TO req-port
           MOVE "/test" TO req-path
           MOVE SPACES TO req-body

           CALL "HTTP-CLIENT" USING http-request http-status
           END-CALL

           MOVE http-status TO api-response-status

           IF http-status = 0
               DISPLAY "Test endpoint call successful"
           ELSE
               DISPLAY "Test endpoint call failed"
           END-IF.
