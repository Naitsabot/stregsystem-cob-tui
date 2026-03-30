      * Copybook: Product Dictionary (Key-Value Store)
      * Purpose: Global product cache for active and named products
      * Usage: Include in WORKING-STORAGE for shared product access
      * Max Capacity: 35 products (uses linear search for lookup)
       01  product-dictionary.
           05  dict-entry-count     PIC 99 COMP-5 VALUE 0.
           05  dict-max-entries     PIC 99 COMP-5 VALUE 35.
           05  dict-last-updated    PIC X(20).
           05  dict-entries OCCURS 35 TIMES
                            INDEXED BY dict-idx.
               10  dict-prod-id         PIC X(10).
               10  dict-prod-name       PIC X(50).
               10  dict-prod-price      PIC S9(9) COMP-5.
               10  dict-is-active       PIC 9 VALUE 0.
                   88  dict-prod-active     VALUE 1.
                   88  dict-prod-inactive   VALUE 0.
               10  dict-source          PIC X(10).
                   88  dict-from-active     VALUE "ACTIVE".
                   88  dict-from-named      VALUE "NAMED".

      * Working variables for dictionary operations
       01  dict-work-vars.
           05  dict-search-id       PIC X(10).
           05  dict-search-result   PIC 9 VALUE 0.
               88  dict-found           VALUE 1.
               88  dict-not-found       VALUE 0.
           05  dict-insert-result   PIC 9 VALUE 0.
               88  dict-inserted        VALUE 1.
               88  dict-duplicate       VALUE 2.
               88  dict-full            VALUE 3.
           05  dict-work-name       PIC X(50).
           05  dict-work-price      PIC S9(9) COMP-5.
           05  dict-work-active     PIC 9.
           05  dict-work-source     PIC X(10).
