      ******************************************************************
      * Author: Naitsabot
      * Purpose: Product Dictionary Usage Example
      * Description:
      *     Demonstrates how to use the product dictionary copybook
      *     Shows add, search, load, and iteration operations
      *     Can be compiled and run as test/demonstration
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCT-DICT-EXAMPLE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      * Product dictionary structure
       COPY "copybooks/product-dictionary.cpy".
       
      * Parsed products (for loading examples)
       COPY "copybooks/parsed-products.cpy".

      * Required working variables
       01  api-log-level        PIC 9 VALUE 1.
       01  WS-IDX               PIC 99 COMP-5.

       PROCEDURE DIVISION.

       MAIN-LOGIC.
           DISPLAY "=== Product Dictionary Example ==="
           DISPLAY " "

      *    Example 1: Initialize and clear dictionary
           PERFORM CLEAR-DICTIONARY

      *    Example 2: Add products manually
           DISPLAY "--- Adding products manually ---"
           MOVE "123" TO dict-search-id
           MOVE "Coffee" TO dict-work-name
           MOVE 2500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "456" TO dict-search-id
           MOVE "Tea" TO dict-work-name
           MOVE 1500 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

           MOVE "789" TO dict-search-id
           MOVE "Soda" TO dict-work-name
           MOVE 1000 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "NAMED" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY

      *    Example 3: Search for a product
           DISPLAY " "
           DISPLAY "--- Searching for product 456 ---"
           MOVE "456" TO dict-search-id
           PERFORM SEARCH-DICTIONARY
           IF dict-found
               DISPLAY "Found: " dict-prod-name(dict-idx)
               DISPLAY "Price: " dict-prod-price(dict-idx)
           ELSE
               DISPLAY "Product not found"
           END-IF

      *    Example 4: Try to add duplicate
           DISPLAY " "
           DISPLAY "--- Trying to add duplicate ---"
           MOVE "456" TO dict-search-id
           MOVE "Green Tea" TO dict-work-name
           MOVE 1800 TO dict-work-price
           MOVE 1 TO dict-work-active
           MOVE "ACTIVE" TO dict-work-source
           PERFORM ADD-TO-DICTIONARY
           IF dict-duplicate
               DISPLAY "Product was updated (duplicate detected)"
           END-IF

      *    Example 5: Display entire dictionary
           DISPLAY " "
           PERFORM DISPLAY-DICTIONARY

      *    Example 6: Load from parsed-products-list
      *    (This would typically be done after API call and JSON parsing)
           DISPLAY " "
           DISPLAY "--- Example: Loading from API response ---"
           MOVE 2 TO products-count
           MOVE "P001" TO prod-id(1)
           MOVE "Energy Drink" TO prod-name(1)
           MOVE 3000 TO prod-price(1)
           MOVE "P002" TO prod-id(2)
           MOVE "Water" TO prod-name(2)
           MOVE 500 TO prod-price(2)
           MOVE "ACTIVE" TO dict-work-source
           
           PERFORM CLEAR-DICTIONARY
           PERFORM LOAD-PRODUCTS-TO-DICTIONARY
           PERFORM DISPLAY-DICTIONARY

      *    Example 7: Iterate through dictionary
           DISPLAY " "
           DISPLAY "--- Iterating through all active products ---"
           PERFORM VARYING dict-idx FROM 1 BY 1
                   UNTIL dict-idx > dict-entry-count
               IF dict-prod-active(dict-idx)
                   DISPLAY dict-prod-id(dict-idx) ": " 
                          dict-prod-name(dict-idx)
               END-IF
           END-PERFORM

           DISPLAY " "
           DISPLAY "=== Example Complete ==="
           STOP RUN.

      * Product dictionary helper procedures
       COPY "copybooks/product-dict-procedures.cob".
