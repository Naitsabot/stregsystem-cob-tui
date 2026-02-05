      ******************************************************************
      * Author: Naitsabot
      * Purpose: Test Product Dictionary Functions
      * Description:
      *     Comprehensive tests for product dictionary operations:
      *     - Add products (single and bulk)
      *     - Search products (found and not found)
      *     - Update duplicates
      *     - Dictionary full condition
      *     - Clear dictionary
      *     - Load from parsed-products-list
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
       01  api-log-level        PIC 9 VALUE 0.
       01  WS-IDX               PIC 99 COMP-5.

      * Test control variables
       01  test-counter         PIC 99 VALUE 0.
       01  test-passed          PIC 99 VALUE 0.
       01  test-failed          PIC 99 VALUE 0.
       01  test-name            PIC X(50).

       PROCEDURE DIVISION.

      * Product dictionary helper procedures
       COPY "copybooks/product-dict-procedures.cob".

       MAIN-LOGIC.
           DISPLAY "========================================".
           DISPLAY "  PRODUCT DICTIONARY TEST SUITE".
           DISPLAY "========================================".
           DISPLAY " ".

           PERFORM TEST-01-CLEAR-DICTIONARY
           PERFORM TEST-02-ADD-SINGLE-PRODUCT
           PERFORM TEST-03-SEARCH-EXISTING-PRODUCT
           PERFORM TEST-04-SEARCH-NONEXISTENT-PRODUCT
           PERFORM TEST-05-UPDATE-DUPLICATE
           PERFORM TEST-06-ADD-MULTIPLE-PRODUCTS
           PERFORM TEST-07-DICTIONARY-CAPACITY
           PERFORM TEST-08-CLEAR-AND-VERIFY
           PERFORM TEST-09-LOAD-FROM-PARSED-LIST
           PERFORM TEST-10-ACTIVE-INACTIVE-FLAGS

           DISPLAY " ".
           DISPLAY "========================================".
           DISPLAY "  TEST RESULTS".
           DISPLAY "========================================".
           DISPLAY "Total Tests:  " test-counter.
           DISPLAY "Passed:       " test-passed.
           DISPLAY "Failed:       " test-failed.
           
           IF test-failed = 0
               DISPLAY " "
               DISPLAY "*** ALL TESTS PASSED ***"
           ELSE
               DISPLAY " "
               DISPLAY "*** SOME TESTS FAILED ***"
           END-IF
           
           DISPLAY "========================================".
           
           STOP RUN.

      ******************************************************************
      * TEST-01-CLEAR-DICTIONARY
      * Verify dictionary can be cleared
      ******************************************************************
       TEST-01-CLEAR-DICTIONARY.
           MOVE "Clear dictionary" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
           IF dict-entry-count = 0
               PERFORM TEST-PASS
           ELSE
               DISPLAY "Expected count 0, got " dict-entry-count
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-02-ADD-SINGLE-PRODUCT
      * Verify single product can be added
      ******************************************************************
       TEST-02-ADD-SINGLE-PRODUCT.
           MOVE "Add single product" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           MOVE "P001" TO dict-search-id
           MOVE "Coffee" TO dict-work-name
           MOVE 2500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY
           
           IF dict-entry-count = 1 AND dict-inserted
               PERFORM TEST-PASS
           ELSE
               DISPLAY "Expected count 1 and inserted=true"
               DISPLAY "Got count " dict-entry-count
               DISPLAY "Inserted flag: " dict-insert-result
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-03-SEARCH-EXISTING-PRODUCT
      * Verify existing product can be found
      ******************************************************************
       TEST-03-SEARCH-EXISTING-PRODUCT.
           MOVE "Search existing product" TO test-name
           ADD 1 TO test-counter
           
           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           
           IF dict-found AND dict-prod-name(dict-idx) = "Coffee"
               PERFORM TEST-PASS
           ELSE
               DISPLAY "Expected to find P001 with name Coffee"
               DISPLAY "Found: " dict-search-result
               IF dict-found
                   DISPLAY "Name: " dict-prod-name(dict-idx)
               END-IF
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-04-SEARCH-NONEXISTENT-PRODUCT
      * Verify non-existent product returns not found
      ******************************************************************
       TEST-04-SEARCH-NONEXISTENT-PRODUCT.
           MOVE "Search non-existent product" TO test-name
           ADD 1 TO test-counter
           
           MOVE "P999" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           
           IF dict-not-found
               PERFORM TEST-PASS
           ELSE
               DISPLAY "Expected not-found for P999"
               DISPLAY "Found: " dict-search-result
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-05-UPDATE-DUPLICATE
      * Verify duplicate ID updates existing entry
      ******************************************************************
       TEST-05-UPDATE-DUPLICATE.
           MOVE "Update duplicate product" TO test-name
           ADD 1 TO test-counter
           
           MOVE "P001" TO dict-search-id
           MOVE "Premium Coffee" TO dict-work-name
           MOVE 3500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY
           
           IF dict-entry-count = 1 AND dict-duplicate
               MOVE "P001" TO dict-search-id
               PERFORM SEARCH-DICTIONARY
               IF dict-prod-name(dict-idx) = "Premium Coffee"
                   AND dict-prod-price(dict-idx) = 3500
                   PERFORM TEST-PASS
               ELSE
                   DISPLAY "Product not updated correctly"
                   DISPLAY "Name: " dict-prod-name(dict-idx)
                   DISPLAY "Price: " dict-prod-price(dict-idx)
                   PERFORM TEST-FAIL
               END-IF
           ELSE
               DISPLAY "Expected count 1 and duplicate=true"
               DISPLAY "Got count " dict-entry-count
               DISPLAY "Duplicate flag: " dict-insert-result
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-06-ADD-MULTIPLE-PRODUCTS
      * Verify multiple products can be added
      ******************************************************************
       TEST-06-ADD-MULTIPLE-PRODUCTS.
           MOVE "Add multiple products" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
      *    Add 5 products
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
           
           IF dict-entry-count = 5
               PERFORM TEST-PASS
           ELSE
               DISPLAY "Expected count 5, got " dict-entry-count
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-07-DICTIONARY-CAPACITY
      * Verify dictionary handles full condition (35 max)
      ******************************************************************
       TEST-07-DICTIONARY-CAPACITY.
           MOVE "Dictionary full condition" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
      *    Add 35 products (max capacity)
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > 35
               MOVE WS-IDX TO dict-search-id
               MOVE "Product" TO dict-work-name
               MOVE 1000 TO dict-work-price
               MOVE 1 TO dict-work-active
               MOVE "ACTIVE" TO dict-work-source
               PERFORM ADD-TO-DICTIONARY
           END-PERFORM
           
           IF dict-entry-count = 35
      *        Try to add one more (should fail)
               MOVE "P999" TO dict-search-id
               MOVE "Overflow" TO dict-work-name
               MOVE 9999 TO dict-work-price
               MOVE 1 TO dict-work-active
               MOVE "ACTIVE" TO dict-work-source
               PERFORM ADD-TO-DICTIONARY
               
               IF dict-entry-count = 35 AND dict-full
                   PERFORM TEST-PASS
               ELSE
                   DISPLAY "Dictionary should reject 36th product"
                   DISPLAY "Count: " dict-entry-count
                   DISPLAY "Full flag: " dict-insert-result
                   PERFORM TEST-FAIL
               END-IF
           ELSE
               DISPLAY "Failed to add 35 products"
               DISPLAY "Count: " dict-entry-count
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-08-CLEAR-AND-VERIFY
      * Verify clear removes all entries
      ******************************************************************
       TEST-08-CLEAR-AND-VERIFY.
           MOVE "Clear and verify empty" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
           IF dict-entry-count = 0
               MOVE "P001" TO dict-search-id
               PERFORM SEARCH-DICTIONARY
               IF dict-not-found
                   PERFORM TEST-PASS
               ELSE
                   DISPLAY "Found product after clear"
                   PERFORM TEST-FAIL
               END-IF
           ELSE
               DISPLAY "Dictionary not cleared"
               DISPLAY "Count: " dict-entry-count
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-09-LOAD-FROM-PARSED-LIST
      * Verify bulk load from parsed-products-list
      ******************************************************************
       TEST-09-LOAD-FROM-PARSED-LIST.
           MOVE "Load from parsed list" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
      *    Setup parsed-products-list
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
           
           IF dict-entry-count = 3
               MOVE "P101" TO dict-search-id
               PERFORM SEARCH-DICTIONARY
               IF dict-found 
                   AND dict-prod-name(dict-idx) = "Chocolate"
                   AND dict-prod-price(dict-idx) = 1200
                   PERFORM TEST-PASS
               ELSE
                   DISPLAY "Product P101 not loaded correctly"
                   PERFORM TEST-FAIL
               END-IF
           ELSE
               DISPLAY "Expected 3 products, got " dict-entry-count
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * TEST-10-ACTIVE-INACTIVE-FLAGS
      * Verify active/inactive status tracking
      ******************************************************************
       TEST-10-ACTIVE-INACTIVE-FLAGS.
           MOVE "Active/inactive flags" TO test-name
           ADD 1 TO test-counter
           
           PERFORM CLEAR-DICTIONARY
           
      *    Add active product
           MOVE "P001" TO dict-search-id
           MOVE "Active Product" TO dict-work-name
           MOVE 1000 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY
           
      *    Add inactive product
           MOVE "P002" TO dict-search-id
           MOVE "Inactive Product" TO dict-work-name
           MOVE 2000 TO dict-work-price
           MOVE 0 TO dict-work-active
           MOVE "NAMED" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY
           
      *    Verify first product
           MOVE "P001" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found AND dict-prod-active(dict-idx)
               AND dict-from-active(dict-idx)
      *        Verify second product
               MOVE "P002" TO dict-search-id
               PERFORM SEARCH-DICTIONARY
               IF dict-found AND dict-prod-inactive(dict-idx)
                   AND dict-from-named(dict-idx)
                   PERFORM TEST-PASS
               ELSE
                   DISPLAY "P002 flags incorrect"
                   PERFORM TEST-FAIL
               END-IF
           ELSE
               DISPLAY "P001 flags incorrect"
               PERFORM TEST-FAIL
           END-IF
           .

      ******************************************************************
      * Helper procedures
      ******************************************************************
       TEST-PASS.
           DISPLAY "[PASS] " test-name
           ADD 1 TO test-passed
           .

       TEST-FAIL.
           DISPLAY "[FAIL] " test-name
           ADD 1 TO test-failed
           .
