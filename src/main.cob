      * Author: Naitsabot
      * Version: a0.0.1

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 USER-NAME     PIC A(30).
       01 ITEM          PIC A(30). 

       PROCEDURE DIVISION.
           DISPLAY "Hello, what is your username?".
           ACCEPT USER-NAME.
           DISPLAY "Nice to meet you, " USER-NAME "!".
           DISPLAY "What item do you want to buy?".
           ACCEPT ITEM.
           DISPLAY "You want to buy: " ITEM.

           STOP RUN.