       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. JOHN DOE.
       INSTALLATION. XYZ COMPANY.
       DATE-WRITTEN. 2023-10-01.
       DATE-COMPILED. 2023-10-02.
       SECURITY. CONFIDENTIAL.
       REMARKS. THIS IS A SAMPLE PROGRAM.
       
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TRANSACTION.
           05  TDATE     PIC X(10).
           05  NUM-DATE  REDEFINES TDATE PIC 9(10).
           05  FILLER    PIC X VALUE SPACES.
           05  TADDRSS   PIC X(34).
           05  FILLER    PIC X VALUE SPACES.
           05  TLABEL    PIC X(10).
           05  FILLER    PIC X VALUE SPACES.
           05  TAMOUNT.
               10  TAMT-SIGN                PIC X.
                   88 TAMT-SIGN-POSITIVE    VALUE '+'.
                   88 TAMT-SIGN-NEGATIVE    VALUE '-'.
               10  TAMT-INTEGER-PART        PIC X(8).
               10  TAMT-DEC-POINT           PIC X.
               10  TAMT-DECIMAL-PART        PIC X(8).       
       PROCEDURE DIVISION.
       DOGE-MAIN.
      *
           IF EIBCALEN > ZERO THEN
               MOVE DFHCOMMAREA TO DOGECOMMS-AREA.

           IF EIBCALEN EQUAL TO ZERO
              MOVE 'Displaying first 7 Transactions' TO WTO-MESSAGE
              PERFORM DOGE-WTO
              PERFORM LET-ER-RIP
              PERFORM DOGE-LIST-TRANSACTIONS
      * MAP IS DFHMDI FROM THE MAPSET
      * MAPSET IS WHAT WE SET IN THE PCT (WITH CEDA)
              EXEC CICS SEND MAP('DOGETR1')
                  MAPSET('DOGETR') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF8 AND
                           START-RECORD-ID NOT EQUAL TO '9999999999'
              MOVE 'Showing next screen' TO WTO-MESSAGE
              PERFORM DOGE-WTO
              PERFORM LET-ER-RIP
              PERFORM DOGE-LIST-TRANSACTIONS
              MOVE 'PF7 PREV -' TO PREVO
              EXEC CICS SEND MAP('DOGETR1')
                  MAPSET('DOGETR') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF7
              MOVE 'Showing prev screen' TO WTO-MESSAGE
              PERFORM DOGE-WTO
              PERFORM LET-ER-RIP
              PERFORM BACK-IT-UP 15 TIMES
              PERFORM DOGE-LIST-TRANSACTIONS
              EXEC CICS SEND MAP('DOGETR1')
                  MAPSET('DOGETR') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF3
               EXEC CICS XCTL 
                   PROGRAM('DOGEQUIT')
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHENTER
                   PERFORM RECEIVE-OPTION
                   PERFORM PARSE-OPTION.  
           EXEC CICS
               RETURN TRANSID('DTRN')
                      COMMAREA(DOGECOMMS-AREA)
           END-EXEC.
       END PROGRAM HELLO-WORLD.