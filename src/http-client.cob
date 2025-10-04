      ******************************************************************
      * Author: Naitsabot
      * Purpose: Basic HTTP/1.1 client using shell commands
      * Description:
      *     Uses printf and nc for HTTP requests
      *     Avoids COBOL file I/O complications
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Connection parameters
       01  host-name            PIC X(100).
       01  host-port            PIC 9(5).

      * HTTP components
       01  http-method          PIC X(10).
       01  http-path            PIC X(200).
       01  http-body            PIC X(1000).
       01  body-length       PIC 9(5).

      * Working variables
       01  system-cmd           PIC X(2000).
       01  system-result        PIC S9(9) COMP-5.
       01  request-cmd          PIC X(2000).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    Test GET request to /test endpoint
           DISPLAY "=== Testing GET /test ==="
           MOVE "127.0.0.1" TO host-name
           MOVE 8080 TO host-port
           MOVE "GET" TO http-method
           MOVE "/test" TO http-path
           MOVE SPACES TO http-body

           PERFORM EXECUTE-GET-REQUEST
           DISPLAY " "

      *    Test POST request to /api/purchase
           DISPLAY "=== Testing POST /api/purchase ==="
           MOVE "127.0.0.1" TO host-name
           MOVE 8080 TO host-port
           MOVE "POST" TO http-method
           MOVE "/api/purchase" TO http-path
           MOVE '{"username":"cobol_user","item":"coffee"}'
               TO http-body

           PERFORM EXECUTE-POST-REQUEST

           GOBACK.

       EXECUTE-GET-REQUEST.
      *    Build HTTP GET request using printf and nc
           STRING
               "printf '"
               FUNCTION TRIM(http-method) " "
               FUNCTION TRIM(http-path) " HTTP/1.1\r\n"
               "Host: " FUNCTION TRIM(host-name) "\r\n"
               "User-Agent: COBOL-HTTP-Client/1.0\r\n"
               "Accept: */*\r\n"
               "Connection: close\r\n"
               "\r\n' | "
               "nc " FUNCTION TRIM(host-name) " " host-port
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           DISPLAY "Sending GET request..."
           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           IF system-result NOT = 0
               DISPLAY "Request failed with status: " system-result
           END-IF.

       EXECUTE-POST-REQUEST.
      *    Calculate content length
           COMPUTE body-length =
               FUNCTION LENGTH(FUNCTION TRIM(http-body))

      *    Build HTTP POST request using printf and nc
           STRING
               "printf '"
               FUNCTION TRIM(http-method) " "
               FUNCTION TRIM(http-path) " HTTP/1.1\r\n"
               "Host: " FUNCTION TRIM(host-name) "\r\n"
               "User-Agent: COBOL-HTTP-Client/1.0\r\n"
               "Accept: */*\r\n"
               "Content-Type: application/json\r\n"
               "Content-Length: " body-length "\r\n"
               "Connection: close\r\n"
               "\r\n"
               FUNCTION TRIM(http-body) "' | "
               "nc " FUNCTION TRIM(host-name) " " host-port
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           DISPLAY "Sending POST request..."
           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           IF system-result NOT = 0
               DISPLAY "Request failed with status: " system-result
           END-IF.
