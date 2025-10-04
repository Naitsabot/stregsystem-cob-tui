******************************************************************
      * Author: Naitsabot
      * Purpose: Basic HTTP/1.1 client using netcat
      * Description:
      *     Uses netcat (nc) for socket communication
      *     Pure COBOL with minimal external dependency
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT http-request-file ASSIGN TO "/tmp/cobol_http_req"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT http-response-file ASSIGN TO "/tmp/cobol_http_resp"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  http-request-file.
       01  request-line         PIC X(200).

       FD  http-response-file.
       01  response-line        PIC X(200).

       WORKING-STORAGE SECTION.
      * Connection parameters
       01  host-name            PIC X(100).
       01  host-port            PIC 9(5).

      * HTTP components
       01  http-method          PIC X(10).
       01  http-path            PIC X(200).
       01  http-body            PIC X(1000).
       01  content-length       PIC 9(5).

      * Working variables
       01  crlf                 PIC X(2) VALUE X"0D0A".
       01  system-cmd           PIC X(500).
       01  system-result        PIC S9(9) COMP-5.
       01  response-buffer      PIC X(8000).
       01  eof-flag             PIC 9 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    Test GET request to /test endpoint
           DISPLAY "=== Testing GET /test ==="
           MOVE "127.0.0.1" TO host-name
           MOVE 8080 TO host-port
           MOVE "GET" TO http-method
           MOVE "/test" TO http-path
           MOVE SPACES TO http-body

           PERFORM EXECUTE-HTTP-REQUEST
           PERFORM DISPLAY-RESPONSE
           DISPLAY " "

      *    Test POST request to /api/purchase
           DISPLAY "=== Testing POST /api/purchase ==="
           MOVE "127.0.0.1" TO host-name
           MOVE 8080 TO host-port
           MOVE "POST" TO http-method
           MOVE "/api/purchase" TO http-path
           MOVE '{"username":"cobol_user","item":"coffee"}'
               TO http-body

           PERFORM EXECUTE-HTTP-REQUEST
           PERFORM DISPLAY-RESPONSE

           GOBACK.

       EXECUTE-HTTP-REQUEST.
      *    Create HTTP request file
           OPEN OUTPUT http-request-file

      *    Write request line
           STRING
               FUNCTION TRIM(http-method) " "
               FUNCTION TRIM(http-path) " "
               "HTTP/1.1"
               DELIMITED BY SIZE
               INTO request-line
           END-STRING
           WRITE request-line

      *    Write Host header
           STRING
               "Host: " FUNCTION TRIM(host-name)
               DELIMITED BY SIZE
               INTO request-line
           END-STRING
           WRITE request-line

      *    Write User-Agent
           MOVE "User-Agent: COBOL-HTTP-Client/1.0" TO request-line
           WRITE request-line

      *    Write Accept
           MOVE "Accept: */*" TO request-line
           WRITE request-line

      *    Write Connection
           MOVE "Connection: close" TO request-line
           WRITE request-line

      *    If POST, add Content-Type and Content-Length
           IF http-method = "POST"
               MOVE "Content-Type: application/json" TO request-line
               WRITE request-line

               COMPUTE content-length =
                   FUNCTION LENGTH(FUNCTION TRIM(http-body))

               STRING
                   "Content-Length: " content-length
                   DELIMITED BY SIZE
                   INTO request-line
               END-STRING
               WRITE request-line
           END-IF

      *    Write blank line (end of headers)
           MOVE " " TO request-line
           WRITE request-line

      *    Write body if POST
           IF http-method = "POST"
               MOVE FUNCTION TRIM(http-body) TO request-line
               WRITE request-line
           END-IF

           CLOSE http-request-file

      *    Execute netcat to send request and receive response
           STRING
               "nc " FUNCTION TRIM(host-name) " " host-port
               " < /tmp/cobol_http_req > /tmp/cobol_http_resp"
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL.

       DISPLAY-RESPONSE.
           MOVE SPACES TO response-buffer
           MOVE 0 TO eof-flag

           OPEN INPUT http-response-file

           DISPLAY "Response:"
           PERFORM UNTIL eof-flag = 1
               READ http-response-file INTO response-line
                   AT END
                       MOVE 1 TO eof-flag
                   NOT AT END
                       DISPLAY FUNCTION TRIM(response-line)
               END-READ
           END-PERFORM

           CLOSE http-response-file.
