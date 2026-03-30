      * Copybook: API Request Structure
      * Used by: stregsystem-api.cob (LINKAGE), test-http-api.cob (WS)
       01  api-request-data.
           05  api-operation    PIC X(20).
           05  api-member-id    PIC X(5).
           05  api-room-id      PIC X(5).
           05  api-product-id   PIC X(5).
           05  api-username     PIC X(30).
