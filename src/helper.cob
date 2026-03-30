      ******************************************************************
      * Author: Naitsabot
      * Purpose:
      *     This file is intended to contain helper procedures and
      *     shared utilities for use in the main program or other
      *     modules.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELPER.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * X(32) means a fixed-length alphanumeric string of 32 characters
      * A(32) means a fixed-length alphabetic string of 32 characters
       01  GREETING-MESSAGE     PIC X(32).
       01  SCREEN-INPUTS.
           05  SCREEN-USERNAME   PIC X(64).
           05  SCREEN-ORDER      PIC X(64).
           05  SCREEN-ROOM-ID    PIC X(8).

       01  FEEDBACK-DATA.
           05  FEEDBACK-LINE     PIC X(80).

       01  lookup-work.
           05  ws-member-id       PIC X(8).
           05  ws-member-id-raw   PIC X(64).

       COPY "copybooks/api-request.cpy".
       COPY "copybooks/api-response.cpy".

       SCREEN SECTION.
       01  HELPER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 5 VALUE "Stregsystem quick order".
           05 LINE 4 COLUMN 5 VALUE "Username:".
           05 LINE 4 COLUMN 24 PIC X(64) USING SCREEN-USERNAME.
           05 LINE 5 COLUMN 5 VALUE "Order:".
           05 LINE 5 COLUMN 24 PIC X(64) USING SCREEN-ORDER.
           05 LINE 6 COLUMN 5 VALUE "Room ID:".
           05 LINE 6 COLUMN 24 PIC X(8) USING SCREEN-ROOM-ID.
           05 LINE 8 COLUMN 5 VALUE "Press Enter to submit.".

       01  HELPER-FEEDBACK-SCREEN.
           05 BLANK SCREEN.
           05 LINE 3 COLUMN 5 PIC X(80) USING FEEDBACK-LINE.
           05 LINE 5 COLUMN 5 VALUE "Press Enter to continue.".

       PROCEDURE DIVISION.
           PERFORM HELPER-INIT
           PERFORM HELPER-SCREEN-EXAMPLE
           GOBACK.

       HELPER-INIT.
           MOVE "Welcome to the Stregsystem!" TO GREETING-MESSAGE
           DISPLAY GREETING-MESSAGE.

       SHOW-FEEDBACK.
           DISPLAY HELPER-FEEDBACK-SCREEN
           ACCEPT HELPER-FEEDBACK-SCREEN.

       HELPER-SCREEN-EXAMPLE.
           MOVE SPACES TO SCREEN-INPUTS
           DISPLAY HELPER-SCREEN
           ACCEPT HELPER-SCREEN

           MOVE SPACES TO api-request-data
           MOVE "xGET_MEMBER_ID" TO api-operation
           MOVE SCREEN-USERNAME TO api-username

           CALL "STREGSYSTEM-API" USING
               api-request-data
               api-response-data
           END-CALL

           IF api-response-status NOT = 0
               MOVE "Failed to resolve member id" TO FEEDBACK-LINE
               PERFORM SHOW-FEEDBACK
               EXIT PARAGRAPH
           END-IF

           MOVE api-response-body TO ws-member-id-raw
           INSPECT ws-member-id-raw REPLACING ALL X"0A" BY SPACE
           INSPECT ws-member-id-raw REPLACING ALL X"0D" BY SPACE
           MOVE FUNCTION TRIM(ws-member-id-raw) TO ws-member-id
           MOVE ws-member-id TO api-member-id

           MOVE SCREEN-ORDER TO api-order

           MOVE SPACES TO api-request-data
           MOVE "xPOST_SALE" TO api-operation
           MOVE ws-member-id TO api-member-id
           MOVE SCREEN-ORDER TO api-order
           MOVE SCREEN-ROOM-ID TO api-room-id
           MOVE SCREEN-USERNAME TO api-username

           CALL "STREGSYSTEM-API" USING
               api-request-data
               api-response-data
           END-CALL

           IF api-response-status = 0 AND sale-status = 200
               MOVE "Purchase successful." TO FEEDBACK-LINE
           ELSE
               MOVE "Purchase failed." TO FEEDBACK-LINE
           END-IF

           PERFORM SHOW-FEEDBACK

           DISPLAY "API status: " api-response-status
           DISPLAY "Sale status: " sale-status
           DISPLAY "Sale message: " FUNCTION TRIM(sale-message).
