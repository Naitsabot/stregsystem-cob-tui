      * Copybook: Product Dictionary Helper Procedures
      * Purpose: Reusable procedures for product dictionary operations
      * Usage: COPY this into PROCEDURE DIVISION after including
      *        product-dictionary.cpy in WORKING-STORAGE
      * Requires: log-level (PIC 9), WS-IDX (PIC 99 COMP-5)

      ******************************************************************
      * ADD-TO-DICTIONARY
      * Adds a new product to the dictionary
      * Input: dict-search-id, dict-work-name, dict-work-price,
      *        dict-work-active, dict-work-source
      * Output: dict-insert-result (1=inserted, 2=duplicate, 3=full)
      ******************************************************************
       ADD-TO-DICTIONARY.
           SET dict-not-found TO TRUE
           SET dict-inserted TO TRUE

      *    Check if product already exists
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               MOVE SPACES TO log-message
               STRING "Product " DELIMITED BY SIZE
                   FUNCTION TRIM(dict-search-id) DELIMITED BY SIZE
                   " already in dictionary, updating" DELIMITED BY SIZE
                   INTO log-message
               END-STRING
               PERFORM LOG-INFO
               MOVE dict-work-name TO dict-prod-name(dict-idx)
               MOVE dict-work-price TO dict-prod-price(dict-idx)
               MOVE dict-work-active TO dict-is-active(dict-idx)
               MOVE dict-work-source TO dict-source(dict-idx)
               SET dict-duplicate TO TRUE
               MOVE SPACES TO log-message
               STRING "Updated product " DELIMITED BY SIZE
                   FUNCTION TRIM(dict-prod-id(dict-idx))
                   DELIMITED BY SIZE
                   " in dictionary" DELIMITED BY SIZE
                   INTO log-message
               END-STRING
               PERFORM LOG-DEBUG
           ELSE
      *        Check if dictionary is full
               IF dict-entry-count >= dict-max-entries
                   SET dict-full TO TRUE
                   MOVE "Product dictionary is full (max 35)"
                       TO log-message
                   PERFORM LOG-WARN
               ELSE
      *            Add new entry
                   ADD 1 TO dict-entry-count
                   SET dict-idx TO dict-entry-count
                   MOVE dict-search-id TO dict-prod-id(dict-idx)
                   MOVE dict-work-name TO dict-prod-name(dict-idx)
                   MOVE dict-work-price TO dict-prod-price(dict-idx)
                   MOVE dict-work-active TO dict-is-active(dict-idx)
                   MOVE dict-work-source TO dict-source(dict-idx)
                   SET dict-inserted TO TRUE
                   MOVE SPACES TO log-message
                   MOVE dict-entry-count TO log-num-text
                   STRING "Added product " DELIMITED BY SIZE
                       FUNCTION TRIM(dict-prod-id(dict-idx))
                       DELIMITED BY SIZE
                       " to dictionary (entry " DELIMITED BY SIZE
                       log-num-text DELIMITED BY SIZE
                       ")" DELIMITED BY SIZE
                       INTO log-message
                   END-STRING
                   PERFORM LOG-DEBUG
               END-IF
           END-IF
           .

      ******************************************************************
      * SEARCH-DICTIONARY
      * Searches for a product by ID using linear search
      * Input: dict-search-id
      * Output: dict-search-result, dict-idx (if found)
      ******************************************************************
       SEARCH-DICTIONARY.
           SET dict-not-found TO TRUE
           SET dict-idx TO 1

           SEARCH dict-entries
               AT END
                   SET dict-not-found TO TRUE
               WHEN dict-prod-id(dict-idx) = dict-search-id
                   SET dict-found TO TRUE
                   MOVE SPACES TO log-message
                   MOVE dict-idx TO log-num-text
                   STRING "Found product " DELIMITED BY SIZE
                       FUNCTION TRIM(dict-search-id) DELIMITED BY SIZE
                       " at index " DELIMITED BY SIZE
                       log-num-text DELIMITED BY SIZE
                       INTO log-message
                   END-STRING
                   PERFORM LOG-DEBUG
           END-SEARCH
           .

      ******************************************************************
      * UPDATE-DICTIONARY-ENTRY
      * Updates an existing dictionary entry
      * Assumes dict-idx is already set to the target entry
      * Input: dict-idx (position), plus dict-work-vars for new values
      ******************************************************************
       UPDATE-DICTIONARY-ENTRY.
      *    Entry already exists at dict-idx, just update it
      *    Product ID stays the same, update other fields as needed

           MOVE SPACES TO log-message
           STRING "Updated product " DELIMITED BY SIZE
               FUNCTION TRIM(dict-prod-id(dict-idx))
               DELIMITED BY SIZE
               " in dictionary" DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-DEBUG
           .

      ******************************************************************
      * CLEAR-DICTIONARY
      * Clears all entries from the dictionary
      ******************************************************************
       CLEAR-DICTIONARY.
           MOVE 0 TO dict-entry-count
           MOVE SPACES TO dict-last-updated

           PERFORM VARYING dict-idx FROM 1 BY 1
                   UNTIL dict-idx > dict-max-entries
               MOVE SPACES TO dict-prod-id(dict-idx)
               MOVE SPACES TO dict-prod-name(dict-idx)
               MOVE 0 TO dict-prod-price(dict-idx)
               MOVE 0 TO dict-is-active(dict-idx)
               MOVE SPACES TO dict-source(dict-idx)
           END-PERFORM

           MOVE "Product dictionary cleared" TO log-message
           PERFORM LOG-INFO
           .

      ******************************************************************
      * LOAD-PRODUCTS-TO-DICTIONARY
      * Loads products from parsed-products-list into dictionary
      * Input: parsed-products-list (from parsed-products.cpy)
      * Requires: COPY "copybooks/parsed-products.cpy" in WS
      * Note: Requires WS-IDX variable: PIC 99 COMP-5
      ******************************************************************
       LOAD-PRODUCTS-TO-DICTIONARY.
           MOVE SPACES TO log-message
           MOVE products-count TO log-num-text
           STRING "Loading " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               " products into dictionary" DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO

           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > products-count
                      OR WS-IDX > 35
               MOVE prod-id(WS-IDX) TO dict-search-id
               MOVE prod-name(WS-IDX) TO dict-work-name
               MOVE prod-price(WS-IDX) TO dict-work-price
               MOVE 1 TO dict-work-active
      *        dict-work-source should be set before calling this
               PERFORM ADD-TO-DICTIONARY
           END-PERFORM

           MOVE SPACES TO log-message
           MOVE dict-entry-count TO log-num-text
           STRING "Dictionary now contains " DELIMITED BY SIZE
               log-num-text DELIMITED BY SIZE
               " products" DELIMITED BY SIZE
               INTO log-message
           END-STRING
           PERFORM LOG-INFO
           .

      ******************************************************************
      * DISPLAY-DICTIONARY
      * Displays all dictionary entries (for debugging)
      ******************************************************************
       DISPLAY-DICTIONARY.
           DISPLAY "=== Product Dictionary ==="
           DISPLAY "Entries: " dict-entry-count " / " dict-max-entries
           DISPLAY "Last Updated: " dict-last-updated
           DISPLAY " "

           PERFORM VARYING dict-idx FROM 1 BY 1
                   UNTIL dict-idx > dict-entry-count
               DISPLAY "  [" dict-idx "] ID: " dict-prod-id(dict-idx)
               DISPLAY "      Name: " dict-prod-name(dict-idx)
               DISPLAY "      Price: " dict-prod-price(dict-idx)
               DISPLAY "      Active: " dict-is-active(dict-idx)
               DISPLAY "      Source: " dict-source(dict-idx)
               DISPLAY " "
           END-PERFORM
           DISPLAY "========================="
           .
