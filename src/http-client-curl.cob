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
       01  crlf                 PIC X(2) VALUE X"0D0A".
       01  system-cmd           PIC X(2000).
       01  system-cmd-full      PIC X(4400).
       01  system-result        PIC S9(9) COMP-5.
       01  body-length          PIC 9(5).
       01  response-output-file PIC X(256).
       01  WS-TEMP-DIR          PIC X(256).
       01  WS-TEMP-DIR-ENV      PIC X(256).
       01  WS-TEMP-CMD          PIC X(512).
       01  http-client-init-flag.
           05  init-done        PIC 9 VALUE 0.
           05  log-level        PIC 9 VALUE 0.
           05  env-val          PIC X(10).

       LINKAGE SECTION.
       COPY "copybooks/http-request.cpy".
       COPY "copybooks/http-response-status.cpy".

       PROCEDURE DIVISION USING http-request-data
                                http-response-status.

       MAIN-LOGIC.
           IF init-done = 0
               PERFORM INIT-LOGGING
               PERFORM INIT-TEMP-DIR
           END-IF

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

           IF log-level = 2
               DISPLAY "SYSTEM CMD: " FUNCTION TRIM(system-cmd)
           END-IF
           IF log-level < 2
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

       INIT-LOGGING.
      *    Read environment variable COB_HTTP_CLIENT_LOG;
      *    interpret 0=none,1=minimal,2=verbose
           ACCEPT env-val FROM ENVIRONMENT "COB_HTTP_CLIENT_LOG"
           MOVE FUNCTION TRIM(env-val) TO env-val
           MOVE FUNCTION UPPER-CASE(env-val) TO env-val
           IF env-val = "2"
               MOVE 2 TO log-level
           ELSE IF env-val = "TRUE"
               MOVE 2 TO log-level
           ELSE IF env-val = "VERBOSE"
               MOVE 2 TO log-level
           ELSE IF env-val = "1"
               MOVE 1 TO log-level
           ELSE IF env-val = "MINIMAL"
               MOVE 1 TO log-level
           ELSE
               MOVE 0 TO log-level
           END-IF
           MOVE 1 TO init-done
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
           END-STRING.

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

           IF log-level = 2
               DISPLAY "SYSTEM CMD: " FUNCTION TRIM(system-cmd)
           END-IF
           IF log-level < 2
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
