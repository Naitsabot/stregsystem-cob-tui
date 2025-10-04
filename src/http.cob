      ******************************************************************
      * Author: Naitsabot
      * Purpose:
      *     Basic HTTP/1.1 client using sockets
      *     Uses GnuCOBOL socket routines (CBL_SOCKET_*)
      ******************************************************************
       identification division.
       PROGRAM-ID. HTTP-CLIENT.

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

      * IP address conversion
       01  ip-bytes.
           05  ip-byte-1        PIC 9(3).
           05  ip-byte-2        PIC 9(3).
           05  ip-byte-3        PIC 9(3).
           05  ip-byte-4        PIC 9(3).
       01  ip-addr-num          PIC 9(10) COMP-5.

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
       01  body-length           PIC 9(9) COMP-5.
       01  body-length-str       PIC X(5). *> avoid func interpretation

      * Working variables
       01  ws-counter           PIC S9(9) COMP-5.
       01  CRLF                 PIC X(2) VALUE X'0D0A'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *    Assign values
           DISPLAY "=== Testing GET /test ==="
           MOVE "127.0.0.1" TO host-name
           MOVE 8080 TO host-port
           MOVE "GET" TO http-method
           MOVE "/test" TO http-path

           PERFORM CONNECT-TO-HOST *> Preforms procedure

           IF socket-status = 0 *> Not failed
      *        Do something here ...
               PERFORM BUILD-HTTP-REQUEST
               PERFORM SEND-HTTP-REQUEST
               PERFORM RECEIVE-HTTP-RESPONSE
               PERFORM CLOSE-SOCKET
               DISPLAY "Response:"
               DISPLAY FUNCTION TRIM(http-response)
               DISPLAY " "
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
      *    Convert localhost IP (127.0.0.1) to binary
           MOVE 127 TO ip-byte-1
           MOVE 0 TO ip-byte-2
           MOVE 0 TO ip-byte-3
           MOVE 1 TO ip-byte-4

      *    Pack IP address (network byte order - big endian)
           COMPUTE sa-ip-addr =
               ip-byte-1 * 16777216 +
               ip-byte-2 * 65536 +
               ip-byte-3 * 256 +
               ip-byte-4

      *    Convert port to network byte order (swap bytes)
           COMPUTE sa-port =
               FUNCTION MOD(host-port, 256) * 256 +
               host-port / 256

      *    Actually connect to server
           CAll "CALL-SOCKET-CONNECT" USING
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

       BUILD-HTTP-REQUEST.
      * Build HTTP/1.1 request
           MOVE SPACES TO http-request *> Filles with spaces

      *    Time to make a beautiful string of the request
      *    Header
           STRING
               FUNCTION TRIM(http-method) " "
               FUNCTION TRIM(http-path) " "
               "HTTP/1.1" CRLF *> Required for the HTTP protocol
               "Host: " FUNCTION TRIM(host-name) CRLF
               "User-Agent: COBOL-HTTP-CLIENT/1.0" CRLF
               "Accept: */*" CRLF
               "Connection: close" CRLF
               DELIMITED BY SIZE
               INTO http-request
           END-STRING

      *    Request body, per operation
      *    POST
           IF http-method = "POST"
               COMPUTE body-length =
                   FUNCTION LENGTH(FUNCTION TRIM(http-body))
               MOVE body-length TO body-length-str
               STRING
                   http-request DELIMITED BY "  "
                   "Content-Length: " body-length-str CRLF
                   "Content-Type: application/x-www-form-urlencoded"
                   CRLF CRLF
                   FUNCTION TRIM(http-body)
                   DELIMITED BY SIZE
                   INTO http-request
               END-STRING
           ELSE
      *        End headers with blank line
               STRING
                   http-request DELIMITED BY "  "
                   CRLF
                   DELIMITED BY SIZE
                   INTO http-request
               END-STRING
           END-IF

           COMPUTE send-length =
               FUNCTION LENGTH(FUNCTION TRIM(http-request)).

       SEND-HTTP-REQUEST.
           CALL "CBL_SOCKET_SEND" USING
               socket-handle
               http-request
               send-length
               0
               bytes-sent
               socket-status
           END-CALL

           IF socket-status NOT = 0
               DISPLAY "Send failed: " socket-status
           ELSE
               DISPLAY "Sent " bytes-sent " bytes"
           END-IF.

       RECEIVE-HTTP-RESPONSE.
           MOVE SPACES TO http-response
           MOVE 8000 TO recv-length

           CALL "CBL_SOCKET_RECV" USING
               socket-handle
               http-response
               recv-length
               0
               bytes-recv
               socket-status
           END-CALL

           IF socket-status NOT = 0 AND socket-status NOT = 10054
               DISPLAY "Receive failed: " socket-status
           ELSE
               DISPLAY "Received " bytes-recv " bytes"
           END-IF.
