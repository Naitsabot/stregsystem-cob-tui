      * Copybook: Parsed Member Info Structure
      * Used after JSON-DECODER parses GET_MEMBER response
       01  parsed-member-info.
           05  member-balance       PIC S9(9) COMP-5.
           05  member-username      PIC X(30).
           05  member-active        PIC X(5).
           05  member-name          PIC X(50).
