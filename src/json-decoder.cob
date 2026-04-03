      ******************************************************************
      * Author: Naitsabot
      * Purpose: JSON decoding using jq external utility
      * Description:
      *     Decode JSON responses from stregsystem API
      *     Uses jq.exe as external JSON parser
      *     Specific parsers for each API endpoint response
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-DECODER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JSON-INPUT
               ASSIGN TO WS-JSON-INPUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT JSON-OUTPUT
               ASSIGN TO WS-JSON-OUTPUT-PATH
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
        FD JSON-INPUT.
        01 JSON-INPUT-LINE     PIC X(8192).
        FD JSON-OUTPUT.
        01 JSON-OUTPUT-LINE     PIC X(8192).

       WORKING-STORAGE SECTION.
      * JQ command configuration
       01 jq-command           PIC X(8192).
       01 jq-filter            PIC X(512).
       01 jq-executable        PIC X(100) VALUE "jq".
       01 jq-result            PIC S9(9) COMP-5.
       01 temp-output-file     PIC X(100)
           VALUE "temp-json-output.txt".
       01 temp-input-file      PIC X(100)
           VALUE "temp-json-input.txt".
       01 temp-json-escaped    PIC X(8192).

      * Temp file paths
       01 WS-TEMP-DIR          PIC X(256).
       01 WS-TEMP-DIR-ENV      PIC X(256).
       01 WS-JSON-INPUT-PATH   PIC X(256).
       01 WS-JSON-OUTPUT-PATH  PIC X(256).
       01 WS-TEMP-CMD          PIC X(512).

      * String processing for escaping
       01 src-pos              PIC 9(5) COMP-5.
       01 dest-pos             PIC 9(5) COMP-5.
       01 src-len              PIC 9(5) COMP-5.
       01 current-char         PIC X.
       01 output-pos           PIC 9(5) COMP-5.
       01 output-eof           PIC 9 VALUE 0.
       01 input-pos            PIC 9(5) COMP-5.
       01 input-line           PIC X(8192).

      * logging control
       01 logging-control.
           05 decoder-init-done PIC 9 VALUE 0.
           05 decoder-log-level PIC 9 VALUE 0.
           05 decoder-env-val   PIC X(10).

       LINKAGE SECTION.
      * Input: JSON string to parse
       01 json-input-data      PIC X(8192).

      * Input: Parse operation type
       01 parse-operation      PIC X(20).

      * Output: Parsed data (structure depends on operation)
       01 parsed-output-data   PIC X(8192).

      * Output: Status code
       01 parse-status         PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING json-input-data
                                parse-operation
                                parsed-output-data
                                parse-status.

       MAIN-LOGIC.
           IF decoder-init-done = 0
               PERFORM INIT-LOGGING
               PERFORM INIT-TEMP-DIR
           END-IF

           MOVE 0 TO parse-status
           MOVE SPACES TO parsed-output-data

           EVALUATE parse-operation
               WHEN "GET_MEMBER_ID"
                   PERFORM PARSE-MEMBER-ID
               WHEN "GET_MEMBER"
                   PERFORM PARSE-MEMBER-INFO
               WHEN "GET_BALANCE"
                   PERFORM PARSE-BALANCE
               WHEN "GET_ACTIVE_PRODUCTS"
                   PERFORM PARSE-ACTIVE-PRODUCTS
               WHEN "GET_NAMED_PRODUCTS"
                   PERFORM PARSE-NAMED-PRODUCTS
               WHEN "GET_MEMBER_SALES"
                   PERFORM PARSE-MEMBER-SALES
               WHEN "POST_SALE"
                   PERFORM PARSE-SALE-RESULT
               WHEN "GET_VALUE"
                   PERFORM PARSE-GENERIC-VALUE
               WHEN OTHER
                   DISPLAY "Unknown parse operation: " parse-operation
                   MOVE 1 TO parse-status
           END-EVALUATE

           GOBACK.

      * INIT-LOGGING - Initialize logging configuration
       INIT-LOGGING.
           ACCEPT decoder-env-val FROM ENVIRONMENT "LOG_LEVEL"
           IF decoder-env-val = SPACE OR LOW-VALUE
               MOVE 0 TO decoder-log-level
           ELSE
               MOVE FUNCTION NUMVAL(decoder-env-val)
                   TO decoder-log-level
           END-IF
           MOVE 1 TO decoder-init-done
           IF decoder-log-level >= 1
               DISPLAY "JSON-DECODER initialized with log level "
                       decoder-log-level
           END-IF.

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

           MOVE SPACES TO WS-JSON-INPUT-PATH
           STRING
               FUNCTION TRIM(WS-TEMP-DIR) DELIMITED BY SIZE
               "/json-input.txt" DELIMITED BY SIZE
               INTO WS-JSON-INPUT-PATH
           END-STRING

           MOVE SPACES TO WS-JSON-OUTPUT-PATH
           STRING
               FUNCTION TRIM(WS-TEMP-DIR) DELIMITED BY SIZE
               "/json-output.txt" DELIMITED BY SIZE
               INTO WS-JSON-OUTPUT-PATH
           END-STRING

           MOVE WS-JSON-INPUT-PATH TO temp-input-file
           MOVE WS-JSON-OUTPUT-PATH TO temp-output-file.

      * PARSE-MEMBER-ID - Extract member_id from response
      * Example: {"member_id": 321}
       PARSE-MEMBER-ID.
           MOVE ".member_id" TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed member_id: "
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      * PARSE-MEMBER-INFO - Extract member info from response
      * Example: {"balance": 20000, "username": "kresten",
      *           "active": true, "name": "Kresten Laust"}
      * Returns: balance<TAB>username<TAB>active<TAB>name
       PARSE-MEMBER-INFO.
           MOVE '"\(.balance)\t\(.username)\t\(.active)\t\(.name)"'
               TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed member info: "
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      * PARSE-BALANCE - Extract balance from response
      * Example: {"balance": 20000}
       PARSE-BALANCE.
           MOVE ".balance" TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed balance: "
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      * PARSE-ACTIVE-PRODUCTS - Extract active products
      * Example: {"123": {"name": "Beer", "price": 600}}
      * Returns: List of product_id<TAB>name<TAB>price (one per line)
       PARSE-ACTIVE-PRODUCTS.
           STRING
               'to_entries | .[] | "\(.key)\t\(.value.name)\t'
               '\(.value.price)"'
               DELIMITED BY SIZE
               INTO jq-filter
           END-STRING
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed active products"
               END-IF
           END-IF.

      * PARSE-NAMED-PRODUCTS - Extract named products
      * Example: {"beer": 123}
      * Returns: List of name<TAB>product_id (one per line)
       PARSE-NAMED-PRODUCTS.
           MOVE 'to_entries | .[] | "\(.key)\t\(.value)"'
               TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed named products"
               END-IF
           END-IF.

      * PARSE-MEMBER-SALES - Extract member sales list
      * Returns: List of timestamp<TAB>product<TAB>price (one per line)
       PARSE-MEMBER-SALES.
           MOVE '.sales[] | "\(.timestamp)\t\(.product)\t\(.price)"'
               TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed member sales"
               END-IF
           END-IF.

      * PARSE-SALE-RESULT - Extract sale result
      * Returns sale status and details
       PARSE-SALE-RESULT.
           STRING
               '"\(.status)\t\(.msg)\t\(.values.cost)\t'
               '\(.values.member_balance)"'
               DELIMITED BY SIZE
               INTO jq-filter
           END-STRING
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed sale result"
               END-IF
           END-IF.

      * PARSE-GENERIC-VALUE - Extract a simple value
      * Generic parser for simple key-value extraction
       PARSE-GENERIC-VALUE.
           MOVE "." TO jq-filter
           PERFORM EXECUTE-JQ.

      * EXECUTE-JQ - Execute jq command with current filter
       EXECUTE-JQ.
           PERFORM WRITE-INPUT-TO-FILE

      *    Build jq command with input file
           MOVE SPACES TO jq-command
           STRING
               jq-executable DELIMITED BY SPACE
               " -r '" DELIMITED BY SIZE
               FUNCTION TRIM(jq-filter) DELIMITED BY SIZE
               "' " DELIMITED BY SIZE
               FUNCTION TRIM(temp-input-file) DELIMITED BY SPACE
               " > " DELIMITED BY SIZE
               temp-output-file DELIMITED BY SPACE
               INTO jq-command
           END-STRING

           IF decoder-log-level >= 2
               DISPLAY "Executing jq filter: " FUNCTION TRIM(jq-filter)
           END-IF

      *    Execute jq command
           CALL "SYSTEM" USING jq-command RETURNING jq-result
           END-CALL

           IF jq-result NOT = 0
               DISPLAY "jq command failed with code: " jq-result
               MOVE 2 TO parse-status
               GOBACK
           END-IF

      *    Read output from temp file
           PERFORM READ-OUTPUT-FROM-FILE.

      * ESCAPE-JSON-FOR-SHELL - Escape single quotes in JSON
      * Replaces ' with '\'' for safe shell execution in bash
       ESCAPE-JSON-FOR-SHELL.
           MOVE SPACES TO temp-json-escaped
           MOVE 1 TO src-pos
           MOVE 1 TO dest-pos
           MOVE FUNCTION LENGTH(FUNCTION TRIM(json-input-data))
               TO src-len

           PERFORM UNTIL src-pos > src-len
               MOVE json-input-data(src-pos:1) TO current-char

               IF current-char = "'"
      *            Replace ' with '\''
                   STRING
                       "'\\''" DELIMITED BY SIZE
                       INTO temp-json-escaped
                       WITH POINTER dest-pos
                   END-STRING
               ELSE
                   MOVE current-char TO temp-json-escaped(dest-pos:1)
                   ADD 1 TO dest-pos
               END-IF

               ADD 1 TO src-pos
           END-PERFORM.

      * WRITE-INPUT-TO-FILE - Write JSON input to temp file
       WRITE-INPUT-TO-FILE.
           OPEN OUTPUT JSON-INPUT
           MOVE 1 TO input-pos

           PERFORM UNTIL input-pos >
               FUNCTION LENGTH(FUNCTION TRIM(json-input-data))
               MOVE SPACES TO input-line
               UNSTRING json-input-data DELIMITED BY X"0A"
                   INTO input-line
                   WITH POINTER input-pos
               END-UNSTRING
               INSPECT input-line REPLACING ALL X"0D" BY SPACE
               WRITE JSON-INPUT-LINE FROM input-line
           END-PERFORM

           IF FUNCTION LENGTH(FUNCTION TRIM(json-input-data)) = 0
               MOVE SPACES TO input-line
               WRITE JSON-INPUT-LINE FROM input-line
           END-IF

           CLOSE JSON-INPUT.

      * READ-OUTPUT-FROM-FILE - Read jq output from temp file
       READ-OUTPUT-FROM-FILE.
           OPEN INPUT JSON-OUTPUT
           MOVE SPACES TO parsed-output-data
           MOVE 1 TO output-pos
           MOVE 0 TO output-eof

           PERFORM UNTIL output-eof = 1
               READ JSON-OUTPUT
                   AT END
                       MOVE 1 TO output-eof
                   NOT AT END
                       IF FUNCTION TRIM(JSON-OUTPUT-LINE) NOT = SPACES
                           STRING
                               FUNCTION TRIM(JSON-OUTPUT-LINE)
                               DELIMITED BY SIZE
                               X"0A" DELIMITED BY SIZE
                               INTO parsed-output-data
                               WITH POINTER output-pos
                           END-STRING
                       END-IF
               END-READ
           END-PERFORM

           IF output-pos = 1
               DISPLAY "jq produced no output"
               MOVE 3 TO parse-status
               CLOSE JSON-OUTPUT
               GOBACK
           END-IF

      *    Remove trailing newline
           SUBTRACT 1 FROM output-pos
           MOVE SPACE TO parsed-output-data(output-pos:1)

           IF decoder-log-level >= 3
               DISPLAY "Read output: "
                   FUNCTION TRIM(parsed-output-data)
           END-IF

           CLOSE JSON-OUTPUT.

       END PROGRAM JSON-DECODER.
