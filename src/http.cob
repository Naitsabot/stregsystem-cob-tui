      ******************************************************************
      * Author: Naitsabot
      * Purpose:
      *     Basic HTTP/1.1 client using sockets
      ******************************************************************
       identification division.
       PROGRAM-ID. http-client.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Socket handing variables
      * Variables for socket creation and management
       01  socket-handle        PIC S9(9) COMP-5.
       01  socket-family        PIC S9(9) COMP-5 VALUE 2.
       01  socket-type          PIC S9(9) COMP-5 VALUE 1.
       01  socket-protocol      PIC S9(9) COMP-5 VALUE 0.
       01  socket-status        PIC S9(9) COMP-5.

      * Connection params
       01  host-name            PIC X(100). *> VALUE "example.com".
       01  host-port            PIC 9(5) COMP-5.
       01  host-ip              PIC X(15).

      * Socket address structure
       01  socket-addr.
           05  sa-family        PIC S9(9) COMP-5 VALUE 2.
           05  sa-port          PIC S9(9) COMP-5.
           05  sa-ip-addr       PIC S9(9) COMP-5.
           05  filler           PIC X(8) VALUE LOW-VALUES.

      * HTTP request and response buffers
       01  http-request         PIC X(2000). *> X(512).
       01  http-response        PIC X(8000). *> X(2048).
       01  send-length          PIC S9(9) COMP-5.
       01  recv-length          PIC S9(9) COMP-5.
       01  bytes-sent           PIC S9(9) COMP-5.
       01  bytes-recv           PIC S9(9) COMP-5.

      * HTTP components
       01  http-method          PIC X(10). *> VALUE "GET".
       01  http-path            PIC X(200). *> VALUE "/".
       01  http-host            PIC X(100). *> VALUE "example.com".
       01  http-body            PIC X(1000). *> VALUE "".
       01  content-length       PIC 9(9) COMP-5.

      * Working variables
       01  ws-counter           PIC S9(9) COMP-5.
       01  crlf                 PIC X(2) VALUE X'0D0A'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    Assign values
           MOVE "example.com" TO host-name
           MOVE 80 TO host-port
           MOVE "GET" TO http-method
           MOVE "/" TO http-path

           PERFORM CONNECT-TO-HOST *> Preforms procedure

           IF socket-status = 0 *> Not failed
      *        Do something here ...
               PERFORM CLOSE-SOCKET
               DISPLAY "Response received:"
               DISPLAY http-response
           ELSE
               DISPLAY "Failed to connect to host"
           END-IF

           STOP RUN.

       CONNECT-TO-HOST.
      * Create socket with CBL_SOCKET_* routines
           CALL "CBL_SOCKET_OPEN" USING
               socket-handle
               socket-family
               socket-type
               socket-protocol
               socket-status
           END-CALL

           IF socket-status NOT = 0 *> If failed
               DISPLAY "Socket creation failed: " socket-status
               GOBACK
           END-IF

      *    We want to resolve hostname IP
      *    For now: simplified for localhost
      *    For full impl: CBL_SOCKET_GETHOSTBYNAME
           MOVE host-name TO host-ip

      *    Port to network byte order
           MOVE host-port TO sa-port

      *    Actually connect to server
           CAll "CALL_SOCKET_CONNECT" USING
               socket-handle
               socket-addr *> an obj
               LENGTH OF socket-handle
               socket-status
           END-CALL

           IF socket-status NOT = 0 *> If failed
               DISPLAY "Socket connection failed: " socket-status
           ELSE *> Sucess
               DISPLAY "Connected to " FUNCTION TRIM(HOST-NAME)
           END-IF. *> A . (dot), meaning end of procedure

       CLOSE-SOCKET.
      * A socket was opend, a socket needs closing
           CALL "CBL_SOCKET_CLOSE" USING
               socket-handle
               socket-status
           END-CALL

           IF socket-status NOT = 0 *> If failed
               DISPLAY "Socket close failed: " socket-status
           ELSE
               DISPLAY "Socket closed"
           END-IF.

      *BUILD-HTTP-REQUEST.



      *SEND-HTTP-REQUEST.



      *RECEIVE-HTTP-RESPONSE.
