      ******************************************************************
      * Author: Naitsabot
      * Purpose: Test program for Product Dictionary
      * Description:
      *     Demonstrates usage of product dictionary operations
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PRODUCT-DICT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * Product dictionary structure
       COPY "copybooks/product-dictionary.cpy".

      * Parsed products (for loading tests)
       COPY "copybooks/parsed-products.cpy".

      * Required working variables
       01 api-log-level        PIC 9 VALUE 0.
       01 WS-IDX               PIC 99 COMP-5.

       PROCEDURE DIVISION.

      * Product dictionary helper procedures
       COPY "copybooks/product-dict-procedures.cob".

       MAIN-PROCEDURE.
           DISPLAY "=== Testing Product Dictionary ==="
           DISPLAY " "

      *    Test: Clear dictionary
           DISPLAY "Test: Clear dictionary"

           PERFORM CLEAR-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           DISPLAY " "

      *    Test: Add single product
           DISPLAY "Test: Add single product (P001, Coffee, 2500)"

           PERFORM CLEAR-DICTIONARY
           MOVE "P001" TO dict-search-id
           MOVE "Coffee" TO dict-work-name
           MOVE 2500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           DISPLAY "Insert result: " dict-insert-result
           DISPLAY " "

      *    Test: Search existing product
           DISPLAY "Test: Search existing product (P001)"

           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY

           DISPLAY "Search result: " dict-search-result
           IF dict-found
               DISPLAY "Found - Name: " dict-prod-name(dict-idx)
               DISPLAY "        Price: " dict-prod-price(dict-idx)
           END-IF
           DISPLAY " "

      *    Test: Search non-existent product
           DISPLAY "Test: Search non-existent product (P999)"

           MOVE "P999" TO dict-search-id
           PERFORM SEARCH-DICTIONARY

           DISPLAY "Search result: " dict-search-result
           DISPLAY " "

      *    Test: Update duplicate product
           DISPLAY "Test: Update duplicate product "
                   "(P001, Premium Coffee, 3500)"

           MOVE "P001" TO dict-search-id
           MOVE "Premium Coffee" TO dict-work-name
           MOVE 3500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           DISPLAY "Insert result: " dict-insert-result
           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               DISPLAY "Updated - Name: " dict-prod-name(dict-idx)
               DISPLAY "          Price: " dict-prod-price(dict-idx)
           END-IF
           DISPLAY " "

      *    Test: Add multiple products
           DISPLAY "Test: Add multiple products (5 items)"

           PERFORM CLEAR-DICTIONARY

           MOVE "P001" TO dict-search-id
           MOVE "Coffee" TO dict-work-name
           MOVE 2500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P002" TO dict-search-id
           MOVE "Tea" TO dict-work-name
           MOVE 1500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P003" TO dict-search-id
           MOVE "Soda" TO dict-work-name
           MOVE 1000 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "NAMED" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P004" TO dict-search-id
           MOVE "Water" TO dict-work-name
           MOVE 500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P005" TO dict-search-id
           MOVE "Juice" TO dict-work-name
           MOVE 2000 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           DISPLAY " "

      *    Test: Dictionary full condition
           DISPLAY "Test: Dictionary full condition (35 max + 1)"

           PERFORM CLEAR-DICTIONARY

           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 35
               MOVE WS-IDX TO dict-search-id
               MOVE "Product" TO dict-work-name
               MOVE 1000 TO dict-work-price
               MOVE 1 TO dict-work-active
               MOVE "ACTIVE" TO dict-work-source
               PERFORM ADD-TO-DICTIONARY
           END-PERFORM

           DISPLAY "Entry count after 35: " dict-entry-count

           MOVE "P999" TO dict-search-id
           MOVE "Overflow" TO dict-work-name
           MOVE 9999 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           DISPLAY "Entry count after 36th: " dict-entry-count
           DISPLAY "Insert result: " dict-insert-result
           DISPLAY " "

      *    Test: Clear and verify empty
           DISPLAY "Test: Clear and verify empty"

           PERFORM CLEAR-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           DISPLAY "Search for P001 result: " dict-search-result
           DISPLAY " "

      *    Test: Load from parsed list
           DISPLAY "Test: Load from parsed list (3 products)"

           PERFORM CLEAR-DICTIONARY

           MOVE 3 TO products-count
           MOVE "P100" TO prod-id(1)
           MOVE "Energy Drink" TO prod-name(1)
           MOVE 3000 TO prod-price(1)
           MOVE "P101" TO prod-id(2)
           MOVE "Chocolate" TO prod-name(2)
           MOVE 1200 TO prod-price(2)
           MOVE "P102" TO prod-id(3)
           MOVE "Chips" TO prod-name(3)
           MOVE 800 TO prod-price(3)

           MOVE "ACTIVE" TO dict-work-source
           PERFORM LOAD-PRODUCTS-TO-DICTIONARY

           DISPLAY "Entry count: " dict-entry-count
           MOVE "P101" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               DISPLAY "Sample product - Name: "
                   dict-prod-name(dict-idx)
               DISPLAY "                 Price: "
                   dict-prod-price(dict-idx)
           END-IF
           DISPLAY " "

      *    Test: Active/inactive flags
           DISPLAY "Test: Active/inactive flags"

           PERFORM CLEAR-DICTIONARY

           MOVE "P001" TO dict-search-id
           MOVE "Active Product" TO dict-work-name
           MOVE 1000 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P002" TO dict-search-id
           MOVE "Inactive Product" TO dict-work-name
           MOVE 2000 TO dict-work-price
           MOVE 0 TO dict-work-active
           MOVE "NAMED" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               DISPLAY "P001 - Active: " dict-is-active(dict-idx)
               DISPLAY "       Source: " dict-source(dict-idx)
           END-IF

           MOVE "P002" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               DISPLAY "P002 - Active: " dict-is-active(dict-idx)
               DISPLAY "       Source: " dict-source(dict-idx)
           END-IF
           DISPLAY " "

           STOP RUN.
