      * Copybook: Logging Procedures (shared)
      * Usage: COPY in PROCEDURE DIVISION

       LOG-INIT.
           IF log-init-done = 1
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO log-env-val
           MOVE SPACES TO log-sink
           MOVE SPACES TO log-file-path
           ACCEPT log-env-val FROM ENVIRONMENT "STREGSYSTEM_LOG_LEVEL"
           ACCEPT log-sink FROM ENVIRONMENT "STREGSYSTEM_LOG_SINK"
           ACCEPT log-file-path FROM ENVIRONMENT "STREGSYSTEM_LOG_FILE"
           IF FUNCTION TRIM(log-env-val) = SPACES
               ACCEPT log-env-val FROM ENVIRONMENT "LOG_LEVEL"
           END-IF
           IF FUNCTION TRIM(log-env-val) = SPACES
               ACCEPT log-env-val FROM ENVIRONMENT "COB_HTTP_CLIENT_LOG"
           END-IF

           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(log-env-val))
               TO log-env-val
           MOVE FUNCTION UPPER-CASE(FUNCTION TRIM(log-sink))
               TO log-sink

           IF FUNCTION TRIM(log-file-path) NOT = SPACES
               MOVE "FILE" TO log-sink
           END-IF

           IF log-sink = SPACES
               MOVE "STDERR" TO log-sink
           END-IF

           IF log-sink = "FILE"
               MOVE SPACES TO log-cmd
               STRING
                   "touch " DELIMITED BY SIZE
                   FUNCTION TRIM(log-file-path) DELIMITED BY SIZE
                   INTO log-cmd
               END-STRING
               CALL "SYSTEM" USING log-cmd
               END-CALL

               MOVE SPACES TO log-cmd
               STRING
                   "printf '%s\n' 'logger init' >> '" DELIMITED BY SIZE
                   FUNCTION TRIM(log-file-path) DELIMITED BY SIZE
                   "'" DELIMITED BY SIZE
                   INTO log-cmd
               END-STRING
               CALL "SYSTEM" USING log-cmd
               END-CALL
           END-IF

           IF log-env-val = SPACES
               MOVE 0 TO log-level
           ELSE IF log-env-val(1:1) IS NUMERIC
               MOVE FUNCTION NUMVAL(log-env-val) TO log-level
           ELSE IF log-env-val = "ERROR" OR log-env-val = "ERR"
               MOVE 1 TO log-level
           ELSE IF log-env-val = "WARN" OR log-env-val = "WARNING"
               MOVE 2 TO log-level
           ELSE IF log-env-val = "INFO"
               MOVE 3 TO log-level
           ELSE IF log-env-val = "DEBUG"
               MOVE 4 TO log-level
           ELSE IF log-env-val = "TRACE" OR log-env-val = "VERBOSE"
               MOVE 5 TO log-level
           ELSE IF log-env-val = "TRUE"
               MOVE 4 TO log-level
           ELSE IF log-env-val = "MINIMAL"
               MOVE 2 TO log-level
           ELSE
               MOVE 0 TO log-level
           END-IF

           MOVE 1 TO log-init-done
           IF log-level >= 1
               MOVE "INFO" TO log-level-name
               MOVE "logging initialized" TO log-message
               PERFORM LOG-EMIT
           END-IF
           .

       LOG-EMIT.
           IF log-level < log-min-level
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO log-line
           STRING
               "[" DELIMITED BY SIZE
               FUNCTION TRIM(log-level-name) DELIMITED BY SIZE
               "] " DELIMITED BY SIZE
               FUNCTION TRIM(log-component) DELIMITED BY SIZE
               ": " DELIMITED BY SIZE
               FUNCTION TRIM(log-message) DELIMITED BY SIZE
               INTO log-line
           END-STRING

           IF log-sink = "STDOUT"
               DISPLAY FUNCTION TRIM(log-line)
           ELSE IF log-sink = "FILE"
               MOVE SPACES TO log-cmd
               STRING
                   "printf '%s\n' '" DELIMITED BY SIZE
                   FUNCTION TRIM(log-line) DELIMITED BY SIZE
                   "' >> '" DELIMITED BY SIZE
                   FUNCTION TRIM(log-file-path) DELIMITED BY SIZE
                   "'" DELIMITED BY SIZE
                   INTO log-cmd
               END-STRING
               CALL "SYSTEM" USING log-cmd
               END-CALL
           ELSE
               DISPLAY FUNCTION TRIM(log-line) UPON SYSERR
           END-IF
           .

       LOG-ERROR.
           MOVE 1 TO log-min-level
           MOVE "ERROR" TO log-level-name
           PERFORM LOG-EMIT
           .

       LOG-WARN.
           MOVE 2 TO log-min-level
           MOVE "WARN" TO log-level-name
           PERFORM LOG-EMIT
           .

       LOG-INFO.
           MOVE 3 TO log-min-level
           MOVE "INFO" TO log-level-name
           PERFORM LOG-EMIT
           .

       LOG-DEBUG.
           MOVE 4 TO log-min-level
           MOVE "DEBUG" TO log-level-name
           PERFORM LOG-EMIT
           .

       LOG-TRACE.
           MOVE 5 TO log-min-level
           MOVE "TRACE" TO log-level-name
           PERFORM LOG-EMIT
           .
