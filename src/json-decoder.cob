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
               ASSIGN TO "temp-json-input.json"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT JSON-OUTPUT
               ASSIGN TO "temp-json-output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  JSON-INPUT.
       01  JSON-INPUT-LINE      PIC X(4096).

       FD  JSON-OUTPUT.
       01  JSON-OUTPUT-LINE     PIC X(1024).

       WORKING-STORAGE SECTION.
      * JQ command configuration
       01  jq-command           PIC X(2048).
       01  jq-filter            PIC X(512).
       01  jq-executable        PIC X(100) VALUE "jq".
       01  jq-result            PIC S9(9) COMP-5.
       
      * Temp file paths
       01  temp-input-file      PIC X(100) 
           VALUE "temp-json-input.json".
       01  temp-output-file     PIC X(100) 
           VALUE "temp-json-output.txt".

      * logging control
       01  logging-control.
           05  decoder-init-done PIC 9 VALUE 0.
           05  decoder-log-level PIC 9 VALUE 0.
           05  decoder-env-val   PIC X(10).

       LINKAGE SECTION.
      * Input: JSON string to parse
       01  json-input-data      PIC X(8192).
      
      * Input: Parse operation type
       01  parse-operation      PIC X(20).
      
      * Output: Parsed data (structure depends on operation)
       01  parsed-output-data   PIC X(2048).
      
      * Output: Status code
       01  parse-status         PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING json-input-data
                                parse-operation
                                parsed-output-data
                                parse-status.

       MAIN-LOGIC.
           IF decoder-init-done = 0
               PERFORM INIT-LOGGING
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
               WHEN "POST_SALE"
                   PERFORM PARSE-SALE-RESULT
               WHEN "GET_VALUE"
                   PERFORM PARSE-GENERIC-VALUE
               WHEN OTHER
                   DISPLAY "Unknown parse operation: " parse-operation
                   MOVE 1 TO parse-status
           END-EVALUATE

           GOBACK.

      ******************************************************************
      * INIT-LOGGING - Initialize logging configuration
      ******************************************************************
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

      ******************************************************************
      * PARSE-MEMBER-ID - Extract member_id from response
      * Example: {"member_id": 321}
      ******************************************************************
       PARSE-MEMBER-ID.
           MOVE ".member_id" TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed member_id: " 
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      ******************************************************************
      * PARSE-MEMBER-INFO - Extract member info from response
      * Example: {"balance": 20000, "username": "kresten", 
      *           "active": true, "name": "Kresten Laust"}
      * Returns: balance|username|active|name (pipe-delimited)
      ******************************************************************
       PARSE-MEMBER-INFO.
           MOVE '"\(.balance)|\(.username)|\(.active)|\(.name)"'
               TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed member info: " 
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      ******************************************************************
      * PARSE-BALANCE - Extract balance from response
      * Example: {"balance": 20000}
      ******************************************************************
       PARSE-BALANCE.
           MOVE ".balance" TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed balance: " 
                       FUNCTION TRIM(parsed-output-data)
               END-IF
           END-IF.

      ******************************************************************
      * PARSE-ACTIVE-PRODUCTS - Extract active products
      * Example: {"123": {"name": "Beer", "price": 600}}
      * Returns: List of product_id|name|price (one per line)
      ******************************************************************
       PARSE-ACTIVE-PRODUCTS.
           STRING 
               'to_entries | .[] | "\(.key)|\(.value.name)|'
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

      ******************************************************************
      * PARSE-NAMED-PRODUCTS - Extract named products
      * Example: {"beer": 123}
      * Returns: List of name|product_id (one per line)
      ******************************************************************
       PARSE-NAMED-PRODUCTS.
           MOVE 'to_entries | .[] | "\(.key)|\(.value)"'
               TO jq-filter
           PERFORM EXECUTE-JQ
           IF parse-status = 0
               IF decoder-log-level >= 2
                   DISPLAY "Parsed named products"
               END-IF
           END-IF.

      ******************************************************************
      * PARSE-SALE-RESULT - Extract sale result
      * Returns sale status and details
      ******************************************************************
       PARSE-SALE-RESULT.
      *    For now, just return the whole JSON as-is
      *    TODO: Parse specific fields when sale structure is known
           MOVE FUNCTION TRIM(json-input-data) TO parsed-output-data
           MOVE 0 TO parse-status.

      ******************************************************************
      * PARSE-GENERIC-VALUE - Extract a simple value
      * Generic parser for simple key-value extraction
      ******************************************************************
       PARSE-GENERIC-VALUE.
           MOVE "." TO jq-filter
           PERFORM EXECUTE-JQ.

      ******************************************************************
      * EXECUTE-JQ - Execute jq command with current filter
      ******************************************************************
       EXECUTE-JQ.
      *    Write JSON input to temp file
           PERFORM WRITE-JSON-TO-FILE
           IF parse-status NOT = 0
               GOBACK
           END-IF

      *    Build jq command
           MOVE SPACES TO jq-command
           STRING
               jq-executable DELIMITED BY SPACE
               " -r '" DELIMITED BY SIZE
               FUNCTION TRIM(jq-filter) DELIMITED BY SIZE
               "' " DELIMITED BY SIZE
               temp-input-file DELIMITED BY SPACE
               ' > ' DELIMITED BY SIZE
               temp-output-file DELIMITED BY SPACE
               INTO jq-command
           END-STRING

           IF decoder-log-level >= 2
               DISPLAY "Executing: " FUNCTION TRIM(jq-command)
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

      ******************************************************************
      * WRITE-JSON-TO-FILE - Write JSON input to temp file
      ******************************************************************
       WRITE-JSON-TO-FILE.
           OPEN OUTPUT JSON-INPUT
           IF decoder-log-level >= 3
               DISPLAY "Writing JSON to file: " 
                   FUNCTION TRIM(json-input-data)
           END-IF
           MOVE json-input-data TO JSON-INPUT-LINE
           WRITE JSON-INPUT-LINE
           CLOSE JSON-INPUT.

      ******************************************************************
      * READ-OUTPUT-FROM-FILE - Read jq output from temp file
      ******************************************************************
       READ-OUTPUT-FROM-FILE.
           OPEN INPUT JSON-OUTPUT
           
      *    Read first line (for simple values)
           READ JSON-OUTPUT
               AT END
                   DISPLAY "jq produced no output"
                   MOVE 3 TO parse-status
                   CLOSE JSON-OUTPUT
                   GOBACK
           END-READ
           
           MOVE FUNCTION TRIM(JSON-OUTPUT-LINE) TO parsed-output-data
           
           IF decoder-log-level >= 3
               DISPLAY "Read output: " 
                   FUNCTION TRIM(parsed-output-data)
           END-IF
           
           CLOSE JSON-OUTPUT.

       END PROGRAM JSON-DECODER.
