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

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CONFIG-FILE
               ASSIGN TO CONFIG-PATH
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS CONFIG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  CONFIG-FILE.
        01  CONFIG-LINE         PIC X(256).

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
           05 SCREEN-MENU-CHOICE PIC x(1).
           05 SCREEN-USERNAME    PIC X(64).
           05 SCREEN-PRODUCT-ORDER  PIC X(64).
           05 SCREEN-ROOM-ID     PIC X(8).

       01  FEEDBACK-DATA.
           05 FEEDBACK-LINE      PIC X(80) VALUE "STATUS STRING HERE".

       01  lookup-work.
           05  member-id       PIC X(5).
           05  member-id-raw   PIC X(50).

       01  config-work.
           05  HOME-DIR         PIC X(256).
           05  CONFIG-DIR       PIC X(256).
           05  CONFIG-PATH      PIC X(256).
           05  CONFIG-STATUS    PIC XX.
           05  CONFIG-KEY       PIC X(64).
           05  CONFIG-VALUE     PIC X(128).
           05  CONFIG-NUM-TEXT  PIC X(16).
           05  CONFIG-CMD       PIC X(512).
           05  CONFIG-EOF       PIC 9 VALUE 0.

       01  inventory-work.
           05  INV-HEADER       PIC X(64).
           05  INV-LINES.
               10  INV-LINE     OCCURS 30 TIMES PIC X(26).
           05  INV-COUNT        PIC 9(3) COMP-5.
           05  INV-ID           PIC X(10).
           05  INV-NAME         PIC X(50).
           05  INV-PRICE        PIC X(20).
           05  INV-LINE-RAW     PIC X(256).
           05  INV-POS          PIC 9(5) COMP-5.
           05  INV-IDX          PIC 99 COMP-5.

       COPY "copybooks/api-request.cpy".
       COPY "copybooks/api-response.cpy".
      *COPY "copybooks/screenio.cpy".

       SCREEN SECTION.
       01 MAIN-SELECTION-SCREEN
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 VALUE "WELCOME TO THE".
           05 LINE 4 COLUMN 4 VALUE "  .###:   ##   #####.   ##   #  " &
      -        "          ####### #    # ##### ".
           05 LINE 5 COLUMN 4 VALUE " .#: .# :#  #: #   :# :#  #: #  " &
      -        "             #    #    #   #   ".
           05 LINE 6 COLUMN 4 VALUE " #:     #.  .# #    # #.  .# #  " &
      -        "             #    #    #   #   ".
           05 LINE 7 COLUMN 4 VALUE " #      #    # #   :# #    # #  " &
      -        "             #    #    #   #   ".
           05 LINE 8 COLUMN 4 VALUE " #      #    # #####. #    # #  " &
      -        "Stregsystem  #    #    #   #   ".
           05 LINE 9 COLUMN 4 VALUE " #      #    # #   :# #    # #  " &
      -        "             #    #    #   #   ".
           05 LINE 10 COLUMN 4 VALUE " #:     #.  .# #    # #.  .# # " &
      -        "              #    #    #   #   ".
           05 LINE 11 COLUMN 4 VALUE " .#: .  :#  #: #   :# :#  #: # " &
      -        "              #    #:  :#   #   ".
           05 LINE 12 COLUMN 4 VALUE "  :###:   ##   #####.   ##   ##" &
      -        "####          #     ####  ##### ".
           05 LINE 14 COLUMN 4 VALUE "Choose an action:".
           05 LINE 15 COLUMN 4 VALUE "1. Stregsystem (room ".
           05 LINE 15 COLUMN 37 PIC X(8) FROM SCREEN-ROOM-ID.
           05 LINE 16 COLUMN 4 VALUE "2. Enter other room".
           05 LINE 17 COLUMN 4 VALUE "3. Quit".
           05 LINE 19 COLUMN 4 VALUE "Choice:".
           05 LINE 19 COLUMN 12 PIC X(1) USING SCREEN-MENU-CHOICE.
           05 LINE 21 COLUMN 4 VALUE "Press ENTER TO confirm choice".
           05 LINE 22 COLUMN 4 VALUE"Use F1-8 to change colour scheme!".

       01 ROOM-SELECTION-SCREEN
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 VALUE "Choose one of the".
           05 LINE 2 COLUMN 22 VALUE " following rooms".
           05 LINE 4 COLUMN 4 VALUE "BUNCH OF ROOMS".
           05 LINE 5 COLUMN 4 VALUE "1 : Default test kiosk".
           05 LINE 6 COLUMN 4 VALUE "10: Stregsystem kiosk".
           05 LINE 8 COLUMN 4 VALUE "Choice:".
           05 LINE 8 COLUMN 12 PIC X(8) USING SCREEN-ROOM-ID.
           05 LINE 10 COLUMN 4 VALUE "Press ENTER TO confirm choice".
           05 LINE 11 COLUMN 4 VALUE"Use F1-8 to change colour scheme!".

       01 KIOSK-SELECTION-SCREEN-INVENTORY
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 LINE 21 COLUMN 4 PIC X(64) FROM INV-HEADER.
           05 LINE 22 COLUMN 4 VALUE "ID  PRICE  NAME".
           05 LINE 22 COLUMN 30 VALUE "ID  PRICE  NAME".
           05 LINE 22 COLUMN 58 VALUE "ID  PRICE  NAME".

           05 LINE 24 COLUMN 4 PIC X(26) FROM INV-LINE(1).
           05 LINE 24 COLUMN 30 PIC X(26) FROM INV-LINE(11).
           05 LINE 24 COLUMN 58 PIC X(26) FROM INV-LINE(21).

           05 LINE 25 COLUMN 4 PIC X(26) FROM INV-LINE(2).
           05 LINE 25 COLUMN 30 PIC X(26) FROM INV-LINE(12).
           05 LINE 25 COLUMN 58 PIC X(26) FROM INV-LINE(22).

           05 LINE 26 COLUMN 4 PIC X(26) FROM INV-LINE(3).
           05 LINE 26 COLUMN 30 PIC X(26) FROM INV-LINE(13).
           05 LINE 26 COLUMN 58 PIC X(26) FROM INV-LINE(23).

           05 LINE 27 COLUMN 4 PIC X(26) FROM INV-LINE(4).
           05 LINE 27 COLUMN 30 PIC X(26) FROM INV-LINE(14).
           05 LINE 27 COLUMN 58 PIC X(26) FROM INV-LINE(24).

           05 LINE 28 COLUMN 4 PIC X(26) FROM INV-LINE(5).
           05 LINE 28 COLUMN 30 PIC X(26) FROM INV-LINE(15).
           05 LINE 28 COLUMN 58 PIC X(26) FROM INV-LINE(25).

           05 LINE 29 COLUMN 4 PIC X(26) FROM INV-LINE(6).
           05 LINE 29 COLUMN 30 PIC X(26) FROM INV-LINE(16).
           05 LINE 29 COLUMN 58 PIC X(26) FROM INV-LINE(26).

           05 LINE 30 COLUMN 4 PIC X(26) FROM INV-LINE(7).
           05 LINE 30 COLUMN 30 PIC X(26) FROM INV-LINE(17).
           05 LINE 30 COLUMN 58 PIC X(26) FROM INV-LINE(27).

           05 LINE 31 COLUMN 4 PIC X(26) FROM INV-LINE(8).
           05 LINE 31 COLUMN 30 PIC X(26) FROM INV-LINE(18).
           05 LINE 31 COLUMN 58 PIC X(26) FROM INV-LINE(28).

           05 LINE 32 COLUMN 4 PIC X(26) FROM INV-LINE(9).
           05 LINE 32 COLUMN 30 PIC X(26) FROM INV-LINE(19).
           05 LINE 32 COLUMN 58 PIC X(26) FROM INV-LINE(29).

           05 LINE 33 COLUMN 4 PIC X(26) FROM INV-LINE(10).
           05 LINE 33 COLUMN 30 PIC X(26) FROM INV-LINE(20).
           05 LINE 33 COLUMN 58 PIC X(26) FROM INV-LINE(30).

       01 KIOSK-SELECTION-SCREEN-START
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 BLANK SCREEN.
           05 LINE 2 COLUMN 4 VALUE
               "What would you like to buy from the".
           05 LINE 4 COLUMN 4 VALUE "  .###:   ##   #####.   ##   #  " &
      -        "          ####### #    # ##### ".
           05 LINE 5 COLUMN 4 VALUE " .#: .# :#  #: #   :# :#  #: #  " &
      -        "             #    #    #   #   ".
           05 LINE 6 COLUMN 4 VALUE " #:     #.  .# #    # #.  .# #  " &
      -        "             #    #    #   #   ".
           05 LINE 7 COLUMN 4 VALUE " #      #    # #   :# #    # #  " &
      -        "             #    #    #   #   ".
           05 LINE 8 COLUMN 4 VALUE " #      #    # #####. #    # #  " &
      -        "Stregsystem  #    #    #   #   ".
           05 LINE 9 COLUMN 4 VALUE " #      #    # #   :# #    # #  " &
      -        "             #    #    #   #   ".
           05 LINE 10 COLUMN 4 VALUE " #:     #.  .# #    # #.  .# # " &
      -        "              #    #    #   #   ".
           05 LINE 11 COLUMN 4 VALUE " .#: .  :#  #: #   :# :#  #: # " &
      -        "              #    #:  :#   #   ".
           05 LINE 12 COLUMN 4 VALUE "  :###:   ##   #####.   ##   ##" &
      -        "####          #     ####  ##### ".
           05 LINE 14 COLUMN 4 VALUE "???".

       01 KIOSK-SELECTION-SCREEN-SELECT
           BACKGROUND-COLOR BG-COLOUR
           FOREGROUND-COLOR FG-COLOUR.
           05 LINE 16 COLUMN 4 VALUE "Username :".
           05 LINE 16 COLUMN 15 PIC X(64) USING SCREEN-USERNAME.
           05 LINE 17 COLUMN 4 VALUE "Buy Order:".
           05 LINE 17 COLUMN 15 PIC X(64) USING SCREEN-PRODUCT-ORDER.
           05 LINE 19 COLUMN 4 VALUE "Press ENTER to buy, Use arrow " &
               "keys UP and DOWN to move around.".

       PROCEDURE DIVISION.
           PERFORM INIT-CONFIG
           PERFORM LOAD-CONFIG
           PERFORM INIT-DEFAULTS
           PERFORM MAIN-SELECTION.
           IF SCREEN-MENU-CHOICE NOT = "3"
               PERFORM KIOSK-SELECTION
           END-IF.

           PERFORM SAVE-CONFIG.

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
           PERFORM SAVE-CONFIG.

       KIOSK-SELECTION.
           PERFORM KIOSK-INVENTORY-LOAD
           MOVE 0 TO DONE
           PERFORM UNTIL DONE = 1
               DISPLAY KIOSK-SELECTION-SCREEN-START
               DISPLAY KIOSK-SELECTION-SCREEN-INVENTORY
               DISPLAY KIOSK-SELECTION-SCREEN-SELECT
               ACCEPT KIOSK-SELECTION-SCREEN-SELECT

               PERFORM HANDLE-KEY-COLOR
           END-PERFORM.
           PERFORM SAVE-CONFIG.

       KIOSK-INVENTORY-LOAD.
           MOVE SPACES TO INV-LINES
           MOVE 0 TO INV-COUNT
           MOVE 1 TO INV-POS

           MOVE SPACES TO api-request-data
           MOVE "xGET_ACTIVE_PRODUCTS" TO api-operation
           MOVE SCREEN-ROOM-ID TO api-room-id

           CALL "STREGSYSTEM-API" USING
               api-request-data
               api-response-data
           END-CALL

           IF api-response-status NOT = 0
               MOVE "Failed to load products" TO INV-HEADER
           ELSE
               MOVE SPACES TO INV-HEADER
               STRING
                   "Active products (room " DELIMITED BY SIZE
                   FUNCTION TRIM(SCREEN-ROOM-ID) DELIMITED BY SIZE
                   ")" DELIMITED BY SIZE
                   INTO INV-HEADER
               END-STRING

               PERFORM UNTIL INV-POS >
                       FUNCTION LENGTH(FUNCTION TRIM(api-response-body))
                   MOVE SPACES TO INV-LINE-RAW
                   MOVE SPACES TO INV-ID
                   MOVE SPACES TO INV-NAME
                   MOVE SPACES TO INV-PRICE
                   UNSTRING api-response-body DELIMITED BY X"0A"
                       INTO INV-LINE-RAW
                       WITH POINTER INV-POS
                   END-UNSTRING
                   IF FUNCTION TRIM(INV-LINE-RAW) NOT = SPACES
                       IF INV-COUNT < 30
                           ADD 1 TO INV-COUNT
                           UNSTRING INV-LINE-RAW DELIMITED BY X"09"
                               INTO INV-ID
                                    INV-NAME
                                    INV-PRICE
                           END-UNSTRING
                           MOVE SPACES TO INV-LINE(INV-COUNT)
                           STRING
                               FUNCTION TRIM(INV-ID) DELIMITED BY SIZE
                               "  " DELIMITED BY SIZE
                              FUNCTION TRIM(INV-PRICE) DELIMITED BY SIZE
                               "  " DELIMITED BY SIZE
                               FUNCTION TRIM(INV-NAME) DELIMITED BY SIZE
                               INTO INV-LINE(INV-COUNT)
                           END-STRING
                       END-IF
                   END-IF
               END-PERFORM
           END-IF.

       INIT-DEFAULTS.
           IF SCREEN-ROOM-ID = SPACES
               MOVE "1" TO SCREEN-ROOM-ID
           END-IF.

       INIT-CONFIG.
           MOVE SPACES TO HOME-DIR
           ACCEPT HOME-DIR FROM ENVIRONMENT "HOME"
           IF FUNCTION TRIM(HOME-DIR) = SPACES
               MOVE "/tmp" TO HOME-DIR
           END-IF

           MOVE SPACES TO CONFIG-DIR
           STRING
               FUNCTION TRIM(HOME-DIR) DELIMITED BY SIZE
               "/.config/stregsystem-tui" DELIMITED BY SIZE
               INTO CONFIG-DIR
           END-STRING

           MOVE SPACES TO CONFIG-PATH
           STRING
               FUNCTION TRIM(CONFIG-DIR) DELIMITED BY SIZE
               "/config.txt" DELIMITED BY SIZE
               INTO CONFIG-PATH
           END-STRING

           MOVE SPACES TO CONFIG-CMD
           STRING
               "mkdir -p " DELIMITED BY SIZE
               FUNCTION TRIM(CONFIG-DIR) DELIMITED BY SIZE
               INTO CONFIG-CMD
           END-STRING
           CALL "SYSTEM" USING CONFIG-CMD
           END-CALL.

       LOAD-CONFIG.
           MOVE 0 TO CONFIG-EOF
           OPEN INPUT CONFIG-FILE
           IF CONFIG-STATUS = "35"
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL CONFIG-EOF = 1
               READ CONFIG-FILE
                   AT END
                       MOVE 1 TO CONFIG-EOF
                   NOT AT END
                       MOVE SPACES TO CONFIG-KEY
                       MOVE SPACES TO CONFIG-VALUE
                       UNSTRING CONFIG-LINE DELIMITED BY "="
                           INTO CONFIG-KEY
                                CONFIG-VALUE
                       END-UNSTRING

                       MOVE FUNCTION UPPER-CASE(
                               FUNCTION TRIM(CONFIG-KEY))
                           TO CONFIG-KEY

                       IF CONFIG-KEY = "BG"
                           MOVE FUNCTION NUMVAL(CONFIG-VALUE)
                               TO BG-COLOUR
                       ELSE IF CONFIG-KEY = "FG"
                           MOVE FUNCTION NUMVAL(CONFIG-VALUE)
                               TO FG-COLOUR
                       ELSE IF CONFIG-KEY = "ROOM"
                           MOVE FUNCTION TRIM(CONFIG-VALUE)
                               TO SCREEN-ROOM-ID
                       ELSE IF CONFIG-KEY = "USER"
                           MOVE FUNCTION TRIM(CONFIG-VALUE)
                               TO SCREEN-USERNAME
                       END-IF
               END-READ
           END-PERFORM

           CLOSE CONFIG-FILE.

       SAVE-CONFIG.
           MOVE 0 TO CONFIG-EOF
           OPEN OUTPUT CONFIG-FILE

           MOVE BG-COLOUR TO CONFIG-NUM-TEXT
           MOVE SPACES TO CONFIG-LINE
           STRING "BG=" DELIMITED BY SIZE
               FUNCTION TRIM(CONFIG-NUM-TEXT) DELIMITED BY SIZE
               INTO CONFIG-LINE
           END-STRING
           WRITE CONFIG-LINE

           MOVE FG-COLOUR TO CONFIG-NUM-TEXT
           MOVE SPACES TO CONFIG-LINE
           STRING "FG=" DELIMITED BY SIZE
               FUNCTION TRIM(CONFIG-NUM-TEXT) DELIMITED BY SIZE
               INTO CONFIG-LINE
           END-STRING
           WRITE CONFIG-LINE

           MOVE SPACES TO CONFIG-LINE
           STRING "ROOM=" DELIMITED BY SIZE
               FUNCTION TRIM(SCREEN-ROOM-ID) DELIMITED BY SIZE
               INTO CONFIG-LINE
           END-STRING
           WRITE CONFIG-LINE

           MOVE SPACES TO CONFIG-LINE
           STRING "USER=" DELIMITED BY SIZE
               FUNCTION TRIM(SCREEN-USERNAME) DELIMITED BY SIZE
               INTO CONFIG-LINE
           END-STRING
           WRITE CONFIG-LINE

           CLOSE CONFIG-FILE.


       HANDLE-KEY-COLOR.
           EVALUATE CRT-STATUS
               WHEN KEY-F1
                   MOVE 0 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F2
                   MOVE 1 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F3
                   MOVE 2 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F4
                   MOVE 3 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F5
                   MOVE 4 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F6
                   MOVE 5 TO BG-COLOUR
                   MOVE 7 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F7
                   MOVE 6 TO BG-COLOUR
                   MOVE 0 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN KEY-F8
                   MOVE 7 TO BG-COLOUR
                   MOVE 0 TO FG-COLOUR
                   PERFORM SAVE-CONFIG
               WHEN OTHER
                   MOVE 1 TO DONE
           END-EVALUATE.
