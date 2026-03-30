      ******************************************************************
      * Author: Naitsabot
      * Purpose:
      *     Terminal UI for Stregsystem COBOL TUI
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TUI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS CRT-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

         78  KEY-F1            VALUE 1001.
         78  KEY-F2            VALUE 1002.
         78  KEY-F3            VALUE 1003.
         78  KEY-F4            VALUE 1004.
         78  KEY-F5            VALUE 1005.
         78  KEY-F6            VALUE 1006.
         78  KEY-F7            VALUE 1007.
         78  KEY-F8            VALUE 1008.

        01  CRT-STATUS        PIC 9(4) VALUE 0.
         01  DONE              PIC 9 VALUE 0.
         01  BG-COLOUR         PIC 9 VALUE 3.
         01  FG-COLOUR         PIC 9 VALUE 7.
         01  SCREEN-INPUTS.
           05 SCREEN-MENU-CHOICE PIC x(1) VALUE "1".
           05 SCREEN-USERNAME    PIC X(64).
           05 SCREEN-PRODUCT-ORDER  PIC X(64) VALUE "1".
           05 SCREEN-ROOM-ID     PIC X(8).

       01 FEEDBACK-DATA.
           05 FEEDBACK-LINE      PIC X(80).

       01  lookup-work.
           05  member-id       PIC X(5).
           05  member-id-raw   PIC X(50).

       COPY "copybooks/api-request.cpy".
       COPY "copybooks/api-response.cpy".
      *COPY "copybooks/screenio.cpy".

       SCREEN SECTION.
       01 MAIN-SELECTION-SCREEN
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 PIC X(64) VALUE
               "Welcome to the stregsystem TUI written in COBOL! :D".
           05 LINE 4 COLUMN 4 VALUE "Choose an action".
           05 LINE 5 COLUMN 4 VALUE "1. Stregsystem (Default: room 1)".
           05 LINE 6 COLUMN 4 VALUE "2. Enter other room".
           05 LINE 8 COLUMN 4 VALUE "Choice:".
           05 LINE 8 COLUMN 12 PIC X(1) USING SCREEN-MENU-CHOICE.

       01 ROOM-SELECTION-SCREEN
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 VALUE "Choose one of the following rooms".
           05 LINE 4 COLUMN 4 VALUE "BUNCH OF ROOMS HERE".
           05 LINE 8 COLUMN 4 VALUE "Choice:".
           05 LINE 8 COLUMN 12 PIC X(8) USING SCREEN-ROOM-ID.

       01 KIOSK-SELECTION-SCREEN
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 VALUE "HERE BE DRAGONS".
           05 LINE 8 COLUMN 12 PIC X(64) USING SCREEN-USERNAME.
           05 LINE 9 COLUMN 12 PIC X(64) USING SCREEN-PRODUCT-ORDER.
           05 LINE 10 COLUMN 5 VALUE "Press Enter to submit.".

       PROCEDURE DIVISION.
           MOVE SPACES TO SCREEN-INPUTS
           PERFORM MAIN-SELECTION.
           PERFORM KIOSK-SELECTION.

           DISPLAY

           GOBACK.

       MAIN-SELECTION.
           MOVE 0 TO DONE
           PERFORM UNTIL DONE = 1
               DISPLAY MAIN-SELECTION-SCREEN
               ACCEPT MAIN-SELECTION-SCREEN

               PERFORM HANDLE-KEY-COLOR
           END-PERFORM

           IF SCREEN-MENU-CHOICE = "2"
              PERFORM ROOM-SELECTION
           END-IF.

       ROOM-SELECTION.
           MOVE 0 TO DONE
           PERFORM UNTIL DONE = 1
               DISPLAY ROOM-SELECTION-SCREEN
               ACCEPT ROOM-SELECTION-SCREEN

               PERFORM HANDLE-KEY-COLOR
           END-PERFORM.

       KIOSK-SELECTION.
           MOVE 0 TO DONE
           PERFORM UNTIL DONE = 1
               DISPLAY KIOSK-SELECTION-SCREEN
               ACCEPT KIOSK-SELECTION-SCREEN

               PERFORM HANDLE-KEY-COLOR
            END-PERFORM.

       HANDLE-KEY-COLOR.
           EVALUATE CRT-STATUS
               WHEN KEY-F1
                   MOVE 0 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F2
                   MOVE 1 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F3
                   MOVE 2 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F4
                   MOVE 3 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F5
                   MOVE 4 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F6
                   MOVE 5 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
               WHEN KEY-F7
                   MOVE 6 TO BG-COLOUR
                   MOVE 0 TO FG-COLOUR
               WHEN KEY-F8
                   MOVE 7 TO BG-COLOUR
                   MOVE 0 TO FG-COLOUR
               WHEN OTHER
                   MOVE 1 TO DONE
           END-EVALUATE.
