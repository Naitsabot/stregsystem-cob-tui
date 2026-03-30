      * Copybook: API Request Structure
      * Used by: stregsystem-api.cob (LINKAGE), test-http-api.cob (WS)
       01  api-request-data.
           05  api-operation    PIC X(24).
           05  api-member-id    PIC X(8).
           05  api-room-id      PIC X(8).
           05  api-order        PIC X(64).
           05  api-username     PIC X(64).
