      ******************************************************************
      * Author: Naitsabot
      * Purpose:
      *     This file is intended to contain helper procedures and
      *     shared utilities for use in the main program or other
      *     modules.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELPER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * X(30) means a fixed-length alphanumeric string of 30 characters
      * A(30) means a fixed-length alphabetic string of 30 characters
       01  GREETING-MESSAGE     PIC X(30).
       PROCEDURE DIVISION.
           MOVE "Welcome to the Stregsystem!" TO GREETING-MESSAGE.
           DISPLAY GREETING-MESSAGE.
           GOBACK.
