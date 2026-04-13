      ******************************************************************
      * Author: Naitsabot
      * Purpose: JSON encoding helper functions
      * Description:
      *     Helper functions for encoding JSON formatted strings
      *     JSON GENERATE statement is available in GnuCOBOL 3.x
      *     but manual string building is simpler for small objects
      *
      *     For JSON decoding, see json-decoder.cob which uses jq.exe
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. JSON-ENCODER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 temp-string          PIC X(1024).
       01 quote-char           PIC X VALUE '"'.
      * centralized logging
       COPY "copybooks/logging.cpy".

       LINKAGE SECTION.
      * Input: Encoding operation type
       01 encode-operation     PIC X(20).

      * Input: Data to encode (structure varies by operation)
       01 input-data           PIC X(1024).

      * Output: JSON string
       01 json-output          PIC X(2048).

      * Output: Status code
       01 encode-status        PIC S9(9) COMP-5.

       PROCEDURE DIVISION USING encode-operation
                                input-data
                                json-output
                                encode-status.

       MAIN-LOGIC.
           IF log-init-done = 0
               MOVE "JSON-ENCODER" TO log-component
               PERFORM LOG-INIT
           END-IF

           MOVE 0 TO encode-status
           MOVE SPACES TO json-output

           MOVE SPACES TO log-message
           STRING "Encode operation: " DELIMITED BY SIZE
               FUNCTION TRIM(encode-operation) DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG

           EVALUATE encode-operation
               WHEN "SALE_REQUEST"
                   PERFORM ENCODE-SALE-REQUEST
               WHEN "SIMPLE_OBJECT"
                   PERFORM ENCODE-SIMPLE-OBJECT
               WHEN OTHER
                   MOVE SPACES TO log-message
                   STRING "Unknown encode operation: " DELIMITED BY SIZE
                       FUNCTION TRIM(encode-operation)
                           DELIMITED BY SIZE
                       INTO log-message
                   END-STRING
                   PERFORM LOG-ERROR
                   MOVE 1 TO encode-status
           END-EVALUATE

           GOBACK.

      * ENCODE-SALE-REQUEST - Build JSON for POST /api/sale
      * Input format: member_id<TAB>buystring (tab-delimited)
      * Output: {"member_id": 123, "buystring": "beer"}
       ENCODE-SALE-REQUEST.
           MOVE SPACES TO json-output
           MOVE SPACES TO temp-string
           UNSTRING input-data DELIMITED BY X"09"
               INTO temp-string(1:50)
                    temp-string(51:200)
           END-UNSTRING
           STRING
               '{"member_id": '
               FUNCTION TRIM(temp-string(1:50))
               ', "buystring": "'
               FUNCTION TRIM(temp-string(51:200))
               '"}'
               DELIMITED BY SIZE
               INTO json-output
           END-STRING

           MOVE SPACES TO log-message
           MOVE FUNCTION LENGTH(FUNCTION TRIM(json-output))
               TO log-num-text
           STRING "Encoded sale request length: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-TRACE
           .

      * ENCODE-SIMPLE-OBJECT - Generic single key-value JSON
      * Input format: key<TAB>value (tab-delimited)
      * Output: {"key": "value"}
       ENCODE-SIMPLE-OBJECT.
           MOVE SPACES TO json-output
           MOVE SPACES TO temp-string
           UNSTRING input-data DELIMITED BY X"09"
               INTO temp-string(1:50)
                    temp-string(51:200)
           END-UNSTRING
           STRING
               '{"'
               FUNCTION TRIM(temp-string(1:50))
               '": "'
               FUNCTION TRIM(temp-string(51:200))
               '"}'
               DELIMITED BY SIZE
               INTO json-output
           END-STRING

           MOVE SPACES TO log-message
           MOVE FUNCTION LENGTH(FUNCTION TRIM(json-output))
               TO log-num-text
           STRING "Encoded simple object length: " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-TRACE
           .

      * Logging procedures
       COPY "copybooks/logging-procedures.cob".

       END PROGRAM JSON-ENCODER.
