      ******************************************************************
      * Author: VIRTUAL HEART
      * Date: 10-DEC-2021
      * Purpose: PALINDROME
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PALINDROME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-PAL          PIC X(9).
       01 WS-TEMP         PIC X(9).
       01 WS-I            PIC 99.
       01 WS-LEN          PIC 99.

       PROCEDURE DIVISION.

       0000-MAIN-PROCEDURE.
           ACCEPT WS-PAL.
           PERFORM 1000-FIND-PARA THRU
                   1000-FIND-PARA-EXIT

           PERFORM 9000-TERM-PARA THRU
                   9000-TERM-PARA-EXIT.


       0000-MAIN-PROCEDURE-EXIT.
           EXIT.

       1000-FIND-PARA.

           INSPECT FUNCTION REVERSE(WS-PAL) TALLYING WS-I FOR
                                       LEADING SPACES

           COMPUTE WS-LEN = WS-I - 9
           ADD 1 TO WS-I
           MOVE FUNCTION REVERSE(WS-PAL) TO WS-TEMP

           DISPLAY WS-TEMP(WS-I:WS-LEN)

           IF WS-PAL = WS-TEMP(WS-I:WS-LEN)
             DISPLAY ' PALINDROME'
           ELSE
             DISPLAY 'NOT PALINDROME'
           END-IF

           MOVE SPACE TO WS-PAL
           MOVE ZEROS TO WS-LEN
           MOVE ZEROS TO WS-I

           ACCEPT WS-PAL

           IF WS-PAL NOT = SPACES
               GO TO 1000-FIND-PARA  *> PERFORM ALSO WORKING HERE
           END-IF.

       1000-FIND-PARA-EXIT.
           EXIT.

       9000-TERM-PARA.
           STOP RUN.
       9000-TERM-PARA-EXIT.
           EXIT.

       END PROGRAM PALINDROME.
