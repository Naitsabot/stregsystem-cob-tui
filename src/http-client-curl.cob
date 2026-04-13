      ******************************************************************
      * Author: Naitsabot
      * Purpose: Reusable HTTP/1.1 client module
      * Description:
      *     Generic HTTP client that can be called from other programs
      *     Supports GET and POST methods
      *     Curl implementation, with URL specification
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
       01 response-output-file PIC X(256).
       01 WS-TEMP-DIR          PIC X(256).
       01 WS-TEMP-DIR-ENV      PIC X(256).
       01 WS-TEMP-CMD          PIC X(512).
      * centralized logging
       COPY "copybooks/logging.cpy".

      * one-time init guard for http client
       01 http-init-done       PIC 9 VALUE 0.

       LINKAGE SECTION.
       COPY "copybooks/http-request.cpy".
       COPY "copybooks/http-response-status.cpy".

       PROCEDURE DIVISION USING http-request-data
                                http-response-status.

       MAIN-LOGIC.
           IF http-init-done = 0
               MOVE "HTTP-CLIENT" TO log-component
               PERFORM LOG-INIT
               PERFORM INIT-TEMP-DIR
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
               "curl -s -X 'GET' '"
               FUNCTION TRIM(req-url)
               FUNCTION TRIM(req-path)
               DELIMITED BY SIZE "' "
               "-H 'accept: application/json' "
               "-o " DELIMITED BY SIZE
               FUNCTION TRIM(response-output-file)
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           MOVE SPACES TO log-message
           STRING "Curl command: " DELIMITED BY SIZE
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

           MOVE SPACES TO response-output-file
           STRING
               FUNCTION TRIM(WS-TEMP-DIR) DELIMITED BY SIZE
               "/http-response.txt" DELIMITED BY SIZE
               INTO response-output-file
           END-STRING

           MOVE SPACES TO log-message
           STRING "HTTP response path: " DELIMITED BY SIZE
               FUNCTION TRIM(response-output-file) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

       EXECUTE-POST-REQUEST.
           COMPUTE body-length =
               FUNCTION LENGTH(FUNCTION TRIM(req-body))

           MOVE SPACES TO system-cmd

           STRING
               "curl -s -X 'POST' '"
               FUNCTION TRIM(req-url)
               FUNCTION TRIM(req-path)
               DELIMITED BY SIZE "' "
               "-H 'accept: application/json' "
               "-H 'Content-Type: application/json' "
               "-d '" FUNCTION TRIM(req-body) "' "
               "-o " DELIMITED BY SIZE
               FUNCTION TRIM(response-output-file)
               DELIMITED BY SIZE
               INTO system-cmd
           END-STRING

           MOVE SPACES TO log-message
           STRING "Curl command: " DELIMITED BY SIZE
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
