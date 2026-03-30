      * Copybook: Parsed Member Sales Structure
      * Used after JSON-DECODER parses GET_MEMBER_SALES response
       05  api-member-sales.
           10  member-sales-count  PIC 9(4) COMP-5.
           10  member-sales-table OCCURS 100 TIMES.
               15  sale-timestamp  PIC X(25).
               15  sale-product    PIC X(50).
               15  sale-price      PIC S9(9) COMP-5.
