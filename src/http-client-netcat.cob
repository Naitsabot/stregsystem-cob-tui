      ******************************************************************
      * Author: Naitsabot
      * Purpose: Reusable HTTP/1.1 client module
      * Description:
      *     Generic HTTP client that can be called from other programs
      *     Supports GET and POST methods
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  crlf                 PIC X(2) VALUE X"0D0A".
       01  system-cmd           PIC X(2000).
       01  system-result        PIC S9(9) COMP-5.
       01  body-length          PIC 9(5).

       LINKAGE SECTION.
       01  http-request-data.
           05  req-method       PIC X(10).
           05  req-host         PIC X(100).
           05  req-port         PIC X(5).
           05  req-path         PIC X(200).
           05  req-body         PIC X(1000).
       01  http-response-status PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING http-request-data
                                http-response-status.

       MAIN-LOGIC.
           EVALUATE req-method
               WHEN "GET"
                   PERFORM EXECUTE-GET-REQUEST
               WHEN "POST"
                   PERFORM EXECUTE-POST-REQUEST
               WHEN OTHER
                   DISPLAY "Unsupported HTTP method: " req-method
                   MOVE 1 TO http-response-status
           END-EVALUATE

           GOBACK.

       EXECUTE-GET-REQUEST.
           STRING
               "printf '"
               FUNCTION TRIM(req-method) " "
               FUNCTION TRIM(req-path) " HTTP/1.1" crlf
               "Host: " FUNCTION TRIM(req-host) crlf
               "User-Agent: COBOL-HTTP-Client/1.0" crlf
               "Accept: */*" crlf
               "Connection: close" crlf
               crlf "' | "
               "nc " FUNCTION TRIM(req-host) " "
               FUNCTION TRIM(req-port)
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           MOVE system-result TO http-response-status.

       EXECUTE-POST-REQUEST.
           COMPUTE body-length =
               FUNCTION LENGTH(FUNCTION TRIM(req-body))

           STRING
               "printf '"
               FUNCTION TRIM(req-method) " "
               FUNCTION TRIM(req-path) " HTTP/1.1" crlf
               "Host: " FUNCTION TRIM(req-host) crlf
               "User-Agent: COBOL-HTTP-Client/1.0" crlf
               "Accept: */*" crlf
               "Content-Type: application/x-www-form-urlencoded" crlf
               "Content-Length: " body-length crlf
               "Connection: close" crlf
               crlf
               FUNCTION TRIM(req-body) "' | "
               "nc " FUNCTION TRIM(req-host) " "
               FUNCTION TRIM(req-port)
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           MOVE system-result TO http-response-status.
