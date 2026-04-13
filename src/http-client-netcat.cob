      ******************************************************************
      * Author: Naitsabot
      * Purpose: Reusable HTTP/1.1 client module
      * Description:
      *     Generic HTTP client that can be called from other programs
      *     Supports GET and POST methods
      *     Natcat implementation, with host and port specification
      *     (*Deprecated*, but interesting)
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HTTP-CLIENT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 crlf                 PIC X(2) VALUE X"0D0A".
       01 system-cmd           PIC X(2000).
       01 system-cmd-full      PIC X(4400).
       01 system-result        PIC S9(9) COMP-5.
       01 body-length          PIC 9(5).
      * centralized logging
       COPY "copybooks/logging.cpy".

      * one-time init guard for http client
       01 http-init-done       PIC 9 VALUE 0.

       LINKAGE SECTION.
       01 http-request-data.
           05 req-method       PIC X(10).
           05 req-url          PIC X(200).
           05 req-host         PIC X(100).
           05 req-port         PIC X(10).
           05 req-path         PIC X(200).
           05 req-body         PIC X(1000).
       01 http-response-status PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING http-request-data
                                http-response-status.

       MAIN-LOGIC.
           IF http-init-done = 0
               MOVE "HTTP-CLIENT" TO log-component
               PERFORM LOG-INIT
               MOVE 1 TO http-init-done
           END-IF

           MOVE SPACES TO log-message
           STRING "HTTP request method: " DELIMITED BY SIZE
               FUNCTION TRIM(req-method) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           EVALUATE req-method
               WHEN "GET"
                   PERFORM EXECUTE-GET-REQUEST
               WHEN "POST"
                   PERFORM EXECUTE-POST-REQUEST
               WHEN OTHER
                   MOVE SPACES TO log-message
                   STRING "Unsupported HTTP method: " DELIMITED BY SIZE
                       FUNCTION TRIM(req-method) DELIMITED BY SIZE
                       INTO log-message
                   END-STRING
                   PERFORM LOG-ERROR
                   MOVE 1 TO http-response-status
           END-EVALUATE

           GOBACK.

       EXECUTE-GET-REQUEST.
           MOVE SPACES TO system-cmd
           STRING
               "printf '"
               FUNCTION TRIM(req-method) " "
               FUNCTION TRIM(req-path) " HTTP/1.1" crlf
               "Host: " FUNCTION TRIM(req-host) crlf
               "User-Agent: COBOL-HTTP-Client/1.0" crlf
               "Accept: */*" crlf
               "Connection: close" crlf
               crlf "' | "
               "nc '" FUNCTION TRIM(req-host) "' '"
               FUNCTION TRIM(req-port) "'"
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           MOVE SPACES TO log-message
           STRING "Netcat command: " DELIMITED BY SIZE
               FUNCTION TRIM(system-cmd) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-TRACE

           IF log-level < 4
      *        Prefix with exec to redirect shell stdout/stderr
      *        for the whole command
               STRING "exec >/dev/null 2>&1; " DELIMITED BY SIZE
                   system-cmd DELIMITED BY SIZE
                   INTO system-cmd-full
               END-STRING
               MOVE system-cmd-full TO system-cmd
           END-IF
           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           MOVE system-result TO http-response-status.

            MOVE SPACES TO log-message
            MOVE system-result TO log-num-text
            STRING "HTTP system result: " DELIMITED BY SIZE
                log-num-text DELIMITED BY SIZE
                INTO log-message
            END-STRING
            PERFORM LOG-DEBUG

       EXECUTE-POST-REQUEST.
           COMPUTE body-length =
               FUNCTION LENGTH(FUNCTION TRIM(req-body))

           MOVE SPACES TO system-cmd

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
               "nc '" FUNCTION TRIM(req-host) "' '"
               FUNCTION TRIM(req-port) "'"
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           MOVE SPACES TO log-message
           STRING "Netcat command: " DELIMITED BY SIZE
               FUNCTION TRIM(system-cmd) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-TRACE

           IF log-level < 4
      *        Prefix with exec to redirect shell stdout/stderr
      *        for the whole command
               STRING "exec >/dev/null 2>&1; " DELIMITED BY SIZE
                   system-cmd DELIMITED BY SIZE
                   INTO system-cmd-full
               END-STRING
               MOVE system-cmd-full TO system-cmd
           END-IF
           CALL "SYSTEM" USING system-cmd RETURNING system-result
           END-CALL

           MOVE system-result TO http-response-status.

           MOVE SPACES TO log-message
           MOVE system-result TO log-num-text
           STRING "HTTP system result: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      * Logging procedures
       COPY "copybooks/logging-procedures.cob".
