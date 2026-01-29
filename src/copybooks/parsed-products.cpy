      * Copybook: Parsed Product Structure
      * Used after JSON-DECODER parses product lists
       01  parsed-product.
           05  product-id           PIC X(10).
           05  product-name         PIC X(50).
           05  product-price        PIC S9(9) COMP-5.

      * Array for multiple products
       01  parsed-products-list.
           05  products-count       PIC 9(4) COMP-5.
           05  products-table OCCURS 100 TIMES.
               10  prod-id          PIC X(10).
               10  prod-name        PIC X(50).
               10  prod-price       PIC S9(9) COMP-5.
