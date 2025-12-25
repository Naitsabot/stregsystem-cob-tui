      * Copybook: HTTP Request Structure
      * Used by: http-client-curl.cob (LINKAGE),
      *          stregsystem-api.cob (WS)
       01  http-request-data.
           05  req-method       PIC X(10).
           05  req-url          PIC X(200).
           05  req-path         PIC X(200).
           05  req-body         PIC X(1000).
