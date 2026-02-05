      * Copybook: API Response Structure
      * Used by: stregsystem-api.cob (LINKAGE), test-http-api.cob (WS)
      * Note: api-response-body contains raw or parsed response
      *       Use appropriate JSON-DECODER operation for each API call
       01  api-response-data.
           05  api-response-status  PIC S9(9) COMP-5.
           05  api-response-body    PIC X(8192).
