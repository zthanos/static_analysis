      */////////////////////////////////////////////////////////////// 
      * DOGE Coin CICS/KICKS Application
      * DOGESEND:
      *   Accepts user input for wallet address and amount to send
      *   Sends a record to the output printer D running on port
      *   3506. Uses dogedcams.py to process printer output and send
      *   funds.
      *
      * AUTHOR:
      *   Philip Young aka Soldier of FORTRAN
      *
      * 08/30/2020
      * License GPL v3
      */////////////////////////////////////////////////////////////// 
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DOGESEND.
       AUTHOR. SOLDIER OF FORTRAN.
       INSTALLATION. DOGE BANK.
       DATE-WRITTEN. 08/30/20.
       SECURITY. CONFIDENTIAL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SYSOUT-TOKEN        PIC X(8)  VALUE SPACES.
       01  DOGECOMMS-AREA.
           05  START-RECORD-ID PIC 9(10) VALUE 0000000002.
       01  WTO-MESSAGE         PIC X(38) VALUE SPACES.
       01  TO-SEND.
           05  DOGEID          PIC X(10)B VALUE 'DOGECICS99'.
           05  TO-ADDRESS      PIC X(34)B.
           05  SEND-AMOUNT     PIC X(17).
       01  TO-SEND-LEN         PIC 99 VALUE 63.  
       01  TOP-MESSAGE.
           05 TEXT-MESSAGE     PIC X(7)B VALUE 'SENDING'.
           05 TEXT-AMOUNT      PIC X(17)B VALUE '00000000.00000000'.
           05 TEXT-CURRENCY    PIC X(4) VALUE 'DOGE'.
      * COPY DOGESN.
      * COPY DFHAID.
      * COPY DFHBMSCA.
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA                       PIC X(10).
      *
       PROCEDURE DIVISION.
       DOGE-MAIN.
      * Main procedure run
           IF EIBCALEN > ZERO THEN
               MOVE DFHCOMMAREA TO DOGECOMMS-AREA.

           IF EIBCALEN EQUAL TO ZERO
              MOVE 'Displaying Send Menu' TO WTO-MESSAGE
              PERFORM DOGE-WTO
              EXEC CICS SEND MAP('DOGESN1')
                  MAPSET('DOGESN') ERASE
              END-EXEC
           ELSE   
           IF EIBAID EQUAL TO DFHPF3
               EXEC CICS XCTL 
                   PROGRAM('DOGEQUIT')
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHENTER
                   PERFORM RECEIVE-INPUT
                   PERFORM PARSE-INPUT. 
                
           EXEC CICS
               RETURN TRANSID('DSND')
                      COMMAREA(DOGECOMMS-AREA)
           END-EXEC.
      
       DOGE-WTO.
      * Sends WTO-MESSAGE to MVS Console 
           EXEC CICS WRITE OPERATOR
               TEXT(WTO-MESSAGE)
           END-EXEC.
           MOVE SPACES TO WTO-MESSAGE.
       RECEIVE-INPUT.
      * Get the option the user enters

      *     MOVE 'DSND - Getting Input from User.' TO WTO-MESSAGE.
      *     PERFORM DOGE-WTO.
           EXEC CICS
               RECEIVE MAP('DOGESN1')
                       MAPSET('DOGESN')
                       INTO(DOGESN1I)
                       ASIS
           END-EXEC.
       PARSE-INPUT.
      *    Do we want to leave this screen? 
           IF OPTIONI EQUAL TO 'T' OR OPTIONI EQUAL TO 't'
      -       OR OPTIONI EQUAL TO 'M' OR OPTIONI EQUAL TO 'm'
               MOVE 'Opening Transaction History' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGETRAN')
               END-EXEC
           ELSE
           IF OPTIONI EQUAL TO 'W' OR OPTIONI EQUAL TO 'w'
               MOVE 'Opening Main Menu' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               MOVE 'W' TO DOGECOMMS-AREA
               EXEC CICS XCTL 
                   PROGRAM('DOGECOIN')
                   COMMAREA(DOGECOMMS-AREA)
               END-EXEC
           ELSE
           IF OPTIONI EQUAL TO 'S' OR OPTIONI EQUAL TO 's'
               MOVE 'Opening Such Send' TO WTO-MESSAGE
               PERFORM DOGE-WTO
           ELSE
               PERFORM MOVE-SOME-DOGE.
           MOVE SPACES TO WTO-MESSAGE.
       MOVE-SOME-DOGE.
      *    Ok, now to send some funds 
           MOVE PAYTOI TO TO-ADDRESS.
           MOVE AMOUNTI TO SEND-AMOUNT.
           MOVE 'Sending to address' TO WTO-MESSAGE.
           PERFORM DOGE-WTO.
           MOVE TO-ADDRESS TO WTO-MESSAGE.
           PERFORM DOGE-WTO.
      *     MOVE 'Amount:' TO WTO-MESSAGE.
      *     PERFORM DOGE-WTO.
      *     MOVE SEND-AMOUNT TO WTO-MESSAGE.
      *     PERFORM DOGE-WTO.
      *    Just some simple check incase a person hits enter
           IF TO-ADDRESS EQUAL TO 'Enter address here'
               MOVE DFHREVRS TO PAYTOH
               MOVE 'Invalid DOGE Coin address' TO SNDMSGO
           ELSE
               MOVE SEND-AMOUNT TO TEXT-AMOUNT
               MOVE TOP-MESSAGE TO SNDMSGO
               MOVE SPACES TO AMOUNTO     

               EXEC CICS SPOOLOPEN OUTPUT
                   TOKEN(SYSOUT-TOKEN) CLASS('D')
                   USERID('*') NODE('*')
               END-EXEC
        
               EXEC CICS SPOOLWRITE
                   TOKEN(SYSOUT-TOKEN) FROM(TO-SEND)
                   FLENGTH(TO-SEND-LEN)
               END-EXEC
        
               EXEC CICS SPOOLCLOSE
                   TOKEN(SYSOUT-TOKEN)
               END-EXEC.
        
           EXEC CICS
               SEND MAP('DOGESN1')
                   MAPSET('DOGESN')
           END-EXEC.