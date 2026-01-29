      ******************************************************************
      * Author: Naitsabot
      * Purpose: Test JSON encoding and decoding
      * Description:
      *     Demonstrates JSON encoding with json.cob
      *     and JSON decoding with json-decoder.cob using jq.exe
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-JSON.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Test data
       01  test-json-input      PIC X(8192).
       01  test-encode-input    PIC X(1024).
       01  test-json-output     PIC X(2048).
       01  test-parsed-output   PIC X(2048).
       01  test-operation       PIC X(20).
       01  test-status          PIC S9(9) COMP-5.

      * Test member data
       01  test-member-json     PIC X(512) VALUE
           '{"balance": 25000, "username": "testuser", "active": tru'
           & 'e, "name": "Test User"}'.

       01  test-member-id-json  PIC X(256) VALUE
           '{"member_id": 321}'.

       01  test-products-json   PIC X(1024) VALUE
           '{"123": {"name": "Beer", "price": 600}, "124": {"name": '
           & '"Soda", "price": 400}}'.

      * Copybooks
       COPY "copybooks/parsed-member-info.cpy".

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           DISPLAY "==================================="
           DISPLAY "JSON ENCODING & DECODING TEST"
           DISPLAY "==================================="
           DISPLAY " "

      *    Test 1: Encode Sale Request
           DISPLAY "TEST 1: Encoding Sale Request"
           DISPLAY "-----------------------------------"
           MOVE "SALE_REQUEST" TO test-operation
           MOVE "123|beer beer" TO test-encode-input
           
           CALL "JSON-ENCODER" USING
               test-operation
               test-encode-input
               test-json-output
               test-status
           END-CALL

           IF test-status = 0
               DISPLAY "Input: " FUNCTION TRIM(test-encode-input)
               DISPLAY "JSON Output: " FUNCTION TRIM(test-json-output)
               DISPLAY "Status: SUCCESS"
           ELSE
               DISPLAY "Encoding failed with status: " test-status
           END-IF
           DISPLAY " "

      *    Test 2: Decode Member ID
           DISPLAY "TEST 2: Decoding Member ID"
           DISPLAY "-----------------------------------"
           MOVE "GET_MEMBER_ID" TO test-operation
           MOVE test-member-id-json TO test-json-input
           
           CALL "JSON-DECODER" USING
               test-json-input
               test-operation
               test-parsed-output
               test-status
           END-CALL

           IF test-status = 0
               DISPLAY "JSON Input: " FUNCTION TRIM(test-json-input)
               DISPLAY "Parsed member_id: " 
                   FUNCTION TRIM(test-parsed-output)
               DISPLAY "Status: SUCCESS"
           ELSE
               DISPLAY "Decoding failed with status: " test-status
           END-IF
           DISPLAY " "

      *    Test 3: Decode Member Info
           DISPLAY "TEST 3: Decoding Member Info"
           DISPLAY "-----------------------------------"
           MOVE "GET_MEMBER" TO test-operation
           MOVE test-member-json TO test-json-input
           
           CALL "JSON-DECODER" USING
               test-json-input
               test-operation
               test-parsed-output
               test-status
           END-CALL

           IF test-status = 0
               DISPLAY "JSON Input: " FUNCTION TRIM(test-json-input)
               DISPLAY "Parsed output (balance|username|active|name):"
               DISPLAY "  " FUNCTION TRIM(test-parsed-output)
               PERFORM PARSE-MEMBER-FIELDS
               DISPLAY "Status: SUCCESS"
           ELSE
               DISPLAY "Decoding failed with status: " test-status
           END-IF
           DISPLAY " "

      *    Test 4: Decode Active Products
           DISPLAY "TEST 4: Decoding Active Products"
           DISPLAY "-----------------------------------"
           MOVE "GET_ACTIVE_PRODUCTS" TO test-operation
           MOVE test-products-json TO test-json-input
           
           CALL "JSON-DECODER" USING
               test-json-input
               test-operation
               test-parsed-output
               test-status
           END-CALL

           IF test-status = 0
               DISPLAY "JSON Input: " FUNCTION TRIM(test-json-input)
               DISPLAY "Parsed products (id|name|price per line):"
               DISPLAY "  " FUNCTION TRIM(test-parsed-output)
               DISPLAY "Status: SUCCESS"
           ELSE
               DISPLAY "Decoding failed with status: " test-status
           END-IF
           DISPLAY " "

           DISPLAY "==================================="
           DISPLAY "TEST COMPLETE"
           DISPLAY "==================================="
           
           STOP RUN.

      ******************************************************************
      * PARSE-MEMBER-FIELDS - Parse pipe-delimited member info
      ******************************************************************
       PARSE-MEMBER-FIELDS.
           UNSTRING test-parsed-output DELIMITED BY "|"
               INTO member-balance
                    member-username
                    member-active
                    member-name
           END-UNSTRING

           DISPLAY "  Structured data:"
           DISPLAY "    Balance: " member-balance
           DISPLAY "    Username: " FUNCTION TRIM(member-username)
           DISPLAY "    Active: " FUNCTION TRIM(member-active)
           DISPLAY "    Name: " FUNCTION TRIM(member-name).

       END PROGRAM TEST-JSON.
