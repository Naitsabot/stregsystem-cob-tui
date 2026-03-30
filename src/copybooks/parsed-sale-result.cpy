      * Copybook: Parsed Sale Result Structure
      * Used after JSON-DECODER parses POST_SALE response
       05  api-sale-result.
           10  sale-status          PIC S9(9) COMP-5.
           10  sale-message         PIC X(80).
           10  sale-cost            PIC S9(9) COMP-5.
           10  sale-member-balance  PIC S9(9) COMP-5.
