      *///////////////////////////////////////////////////////////////
      * DOGE Coin CICS/KICKS Application
      * DOGEMAIN:
      *   Displays the DOGE EBCDIC art and the main menu.
      *
      * AUTHOR:
      *   Philip Young aka Soldier of FORTRAN
      *
      * 08/30/2020
      * License GPL v3
      *///////////////////////////////////////////////////////////////
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DOGECOIN.
       AUTHOR. SOLDIER OF FORTRAN.
       INSTALLATION. DOGE BANK.
       DATE-WRITTEN. 08/30/20.
       SECURITY. CONFIDENTIAL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * To Create outgoing commands
       77  RC            PIC S9(4) COMP VALUE +0.
       77  SYSOUT-TOKEN  PIC X(8)  VALUE SPACES.
      * Outgoing Comms ('B' is a space instead of using FILLER)
       01  DOGEMSG.
           05  DOGEID    PIC X(10)B VALUE 'DOGECICS99'.
           05  ADDRSS    PIC X(34)B.
           05  AMOUNT    PIC Z(02),Z(03),Z(02)9.9(8).
       01  DOGEMSG-LEN   PIC 99 VALUE 61.
      * VSAM Record Layout
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
       01  THE-AMOUNT                       PIC S9(8)V9(8).
       01  FILLER REDEFINES THE-AMOUNT.
           05  THE-AMOUNT-INTEGER           PIC X(8).
           05  THE-AMOUNT-DECIMAL           PIC S9(8).
       01  AVAILABLE-AMOUNT                 PIC S9(8)V9(8).
       01  RECENT-COLOR                     PIC X.
       01  DISPLAY-TRAN.
           05  DDATE     PIC X(10).
           05  FILLER    PIC X VALUE SPACES.
           05  DLABEL    PIC X(10).
           05  FILLER    PIC X VALUE SPACES.
           05  DSIGN     PIC X.
           05  DAMOUNT   PIC Z(02),Z(03),Z(02)9.9(8).
           05  FILLER    PIC X VALUE SPACES.
           05  DTYPE     PIC XXXX VALUE 'DOGE'.
       01  TEMP-DATE     PIC 9(15) COMP-3.
       01  DOGEMSG-LEN   PIC 99 VALUE 61.
       01  START-RECORD-ID PIC 9(10) VALUE 9999999999.
       01  SINCE-EPOCH   PIC S9(15) COMP-3 VALUE +2208988800000.
       01  RESPONSE-CODE  PIC S9(4) COMP.
       01  RESPONSE-CODE2 PIC S9(4) COMP.
       01  DOGECOMMS-AREA.
           05  DOGE-FLAG                            PIC X.
               88  SUCH-DOGE                        VALUE 'D'.
               88  WOW-MENU                         VALUE 'W'.
               88  SUCH-SEND                        VALUE 'S'.
               88  SUCH-HISTORY                     VALUE 'T'.
           05  FILLER PIC X(9).
       01  WTO-MESSAGE PIC X(38) VALUE SPACES.
       FILE SECTION.
       FD SAMPLE-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 10 RECORDS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS SAMPLE-RECORD.

       01 SAMPLE-RECORD.
           05 RECORD-ID PIC 9(4).
           05 RECORD-DATA PIC X(76).
      *
      * COPY DOGECN.
      * COPY DOGEMN.
      * COPY DFHAID.
      * COPY DFHBMSCA.
       LINKAGE SECTION.
      *
       01  DFHCOMMAREA                       PIC X(10).
      *
       PROCEDURE DIVISION.
       00000-MAIN.
      *
           IF EIBCALEN > ZERO THEN
               MOVE DFHCOMMAREA TO DOGECOMMS-AREA.
      *
           IF EIBCALEN EQUAL TO ZERO OR SUCH-DOGE

               MOVE 'STARTING DOGE CICS.' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               MOVE 'DISPLAYING DOGE.' TO WTO-MESSAGE
               PERFORM DOGE-WTO

      * MAP IS DFHMDI FROM THE MAPSET
      * MAPSET IS WHAT WE SET IN CEDA/CICS
               EXEC CICS
                    SEND MAP('DOGECN1')
                         MAPSET('DOGECN')
                         ERASE
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF3
               EXEC CICS XCTL 
                   PROGRAM('DOGEQUIT')
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF5
               PERFORM DOGE-MAIN-SCREEN
           ELSE
           IF WOW-MENU
               MOVE 'T' TO DOGECOMMS-AREA
               PERFORM DOGE-MAIN-SCREEN
           ELSE
           IF EIBAID EQUAL TO DFHENTER
                   PERFORM RECEIVE-OPTION
                   PERFORM PARSE-OPTION.
           EXEC CICS
               RETURN TRANSID('DOGE')
                      COMMAREA(DOGECOMMS-AREA)
           END-EXEC.
           SORT SORT-FILE
                ON ASCENDING KEY RECORD-ID1, RECORD-ID2   
                INPUT PROCEDURE 100-INITIALIZE THRU 200-PROCESS-DATA
                OUTPUT PROCEDURE 300-PROCESS-DATA THRU 400-TRANSFER-CONTROL.

       100-INITIALIZE.
           DISPLAY "Initialization start".
           PERFORM 110-SETUP THRU 120-VALIDATE.
           DISPLAY "Initialization complete".

       110-SETUP.
           DISPLAY "Setting up variables".
           MOVE 100 TO WS-VAR1.
           MOVE 200 TO WS-VAR2.
           SET WS-STATUS TO "READY".
           INITIALIZE SAMPLE-RECORD.
           MOVE 1 TO RECORD-ID OF SAMPLE-RECORD. 
           MOVE SPACES TO RECORD-DATA OF SAMPLE-RECORD.

       120-VALIDATE.
           DISPLAY "Validating variables".
           IF WS-VAR1 IS NOT EQUAL TO 100
               DISPLAY "Error in initialization - WS-VAR1"
           END-IF.
           IF WS-VAR2 IS NOT EQUAL TO 200
               DISPLAY "Error in initialization - WS-VAR2"
           END-IF.
           IF WS-STATUS = "READY"
               DISPLAY "Initialization successful"
           ELSE
               DISPLAY "Initialization failed"
           END-IF.

       200-PROCESS-DATA.
           DISPLAY "Processing data".
           PERFORM 210-CALCULATE-RESULT THRU 220-SAVE-RESULT.

       210-CALCULATE-RESULT.
           COMPUTE WS-VAR1 = WS-VAR1 + WS-VAR2.
           DISPLAY "Calculated Result: " WS-VAR1.

       220-SAVE-RESULT.
           DISPLAY "Saving result".
           MOVE WS-VAR1 TO WS-VAR2.
           MOVE WS-NAME TO RECORD-DATA OF SAMPLE-RECORD. 
           WRITE SAMPLE-RECORD.
           INSPECT WS-NAME REPLACING ALL "TEST" BY "RUN".

       300-LOOP-EXAMPLE.
           PERFORM 310-INCREMENT UNTIL WS-COUNTER > 10.

       310-INCREMENT.
           ADD 1 TO WS-COUNTER.
           DISPLAY "Counter: " WS-COUNTER.

       400-TRANSFER-CONTROL.
           DISPLAY "Testing control transfer".
           GO TO 500-ERROR-HANDLING.

       500-ERROR-HANDLING.
           DISPLAY "Error occurred, exiting program".
           EXIT PROGRAM.

       600-FILE-OPERATIONS.
           DISPLAY "Starting file operations".
           OPEN INPUT SAMPLE-FILE.
           IF NOT OPENED SAMPLE-FILE
              DISPLAY "ERROR: File SAMPLE-FILE was not opened."
              GO TO 800-FINALIZE
           END-IF
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ SAMPLE-FILE
                   AT END
                       DISPLAY "End of file reached"
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                      DISPLAY "Record ID: " RECORD-ID
                      DISPLAY "Record Data: " RECORD-DATA
               END-READ
           END-PERFORM.
           CLOSE SAMPLE-FILE.
           IF NOT CLOSED SAMPLE-FILE
              DISPLAY "ERROR: File SAMPLE-FILE was not closed."
              GO TO 800-FINALIZE
           END-IF.
           GO TO 700-TRANSACTION-OPERATIONS.

       700-TRANSACTION-OPERATIONS.
           DISPLAY "Starting transaction operations".
           EXEC CICS
               SEND MAP('MAP1')
                    MAPSET('MAPSET1')
                    ERASE
           END-EXEC
           EXEC CICS
               XCTL
                    PROGRAM('PROGRAM1')
            END-EXEC
            DISPLAY "Returned from CICS transaction".

       800-FINALIZE.
           DISPLAY "Finalizing program".
           DISPLAY "Program completed successfully".
           STOP RUN.

       DOGE-EXIT.
           GOBACK.
      *
       RECEIVE-OPTION.
      * Get the option the user enters

           MOVE 'Getting Input from User.' TO WTO-MESSAGE.
           PERFORM DOGE-WTO.
           EXEC CICS
               RECEIVE MAP('DOGEMN1')
                       MAPSET('DOGEMN')
                       INTO(DOGEMN1I)
           END-EXEC.

       PARSE-OPTION.
      *    Parse the user entry 
           IF OPTIONI EQUAL TO 'T' OR OPTIONI EQUAL TO 'M'
               MOVE 'Opening Transaction History' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGETRAN')
               END-EXEC
           ELSE
           IF OPTIONI EQUAL TO 'W'         
               MOVE 'Opening Main Menu' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGECOIN')
               END-EXEC
           ELSE
           IF OPTIONI EQUAL TO 'D'
               MOVE 'Opening Transaction Details' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGEDEET')
               END-EXEC
           ELSE
           IF OPTIONI EQUAL TO 'S'
               MOVE 'Opening Such Send' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGESEND')
               END-EXEC.
           MOVE SPACES TO WTO-MESSAGE.

       DOGE-MAIN-SCREEN.
      *    Show the main doge screen
           MOVE 'Sending Doge CICS Main Screen.' TO WTO-MESSAGE.
           PERFORM DOGE-WTO.
           EXEC CICS STARTBR FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
           END-EXEC.
      *        First is our dummy 'eyecatcher' records at the bottom
           EXEC CICS READPREV FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
                INTO(TRANSACTION)
           END-EXEC.
      *        Next is our last record
           EXEC CICS READPREV FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
                INTO(TRANSACTION)
           END-EXEC.
           PERFORM CONVERT-DATE.
           MOVE TLABEL TO DLABEL.
           PERFORM CONVERT-AMOUNT-TO-DISPLAY.
           MOVE RECENT-COLOR TO RECNT2C.
           MOVE DISPLAY-TRAN TO RECNT2O.
      *        Then our second to last record
           EXEC CICS READPREV FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
                INTO(TRANSACTION)
           END-EXEC.
           PERFORM CONVERT-DATE.
           MOVE TLABEL TO DLABEL.
           PERFORM CONVERT-AMOUNT-TO-DISPLAY.
           MOVE RECENT-COLOR TO RECNT1C.
      *    If theres only one historical record move it up     
           IF TDATE = 0000000002
               MOVE RECNT2O TO RECNT1O
               MOVE SPACES TO RECNT1O
           ELSE
               MOVE DISPLAY-TRAN TO RECNT1O.
      *    Now we get the current ammount
      *    First we get reset the browse
           MOVE 0000000001 TO START-RECORD-ID.
           EXEC CICS RESETBR FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
           END-EXEC.
      *    Then read the record
           EXEC CICS READNEXT FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
                INTO(TRANSACTION)
           END-EXEC.
      *    Convert it and put on the map
           PERFORM CONVERT-AMOUNT-TO-DISPLAY.
           MOVE THE-AMOUNT TO AVAILABLE-AMOUNT.
           MOVE THE-AMOUNT TO AVAILO.
      *    Get the next record
           EXEC CICS READNEXT FILE('DOGEVSAM')
                RIDFLD(START-RECORD-ID)
                INTO(TRANSACTION)
           END-EXEC.
           PERFORM CONVERT-AMOUNT-TO-DISPLAY.
           MOVE THE-AMOUNT TO PENDNGO.
           ADD AVAILABLE-AMOUNT TO THE-AMOUNT.
           MOVE THE-AMOUNT TO TOTALO.
      *    Aaaaaand were done show the map now

           EXEC CICS ENDBR 
               FILE('DOGEVSAM')
           END-EXEC.

           EXEC CICS
                SEND MAP('DOGEMN1')
                     MAPSET('DOGEMN')
                     ERASE
           END-EXEC.
      *    
       CONVERT-AMOUNT-TO-DISPLAY.
      * Converts the number from VSAM to ##,###,###.########
           MOVE DFHGREEN TO RECENT-COLOR.
           MOVE TAMT-INTEGER-PART TO THE-AMOUNT-INTEGER.
           MOVE TAMT-DECIMAL-PART TO THE-AMOUNT-DECIMAL.
           IF TAMT-SIGN-NEGATIVE
               MOVE DFHRED TO RECENT-COLOR
               SUBTRACT THE-AMOUNT FROM ZERO GIVING THE-AMOUNT.
           MOVE THE-AMOUNT TO DAMOUNT.
           MOVE TAMT-SIGN TO DSIGN.
      *
       CONVERT-DATE.
      *
      * Converts Linux EPOCH to CICS Absolute Time
      * and places it in DISPLAY-TRAN:DDATE as MM/DD/YYYY
      *
           MOVE NUM-DATE TO TEMP-DATE.
           MULTIPLY 1000 BY TEMP-DATE.
           ADD SINCE-EPOCH TO TEMP-DATE.
           EXEC CICS FORMATTIME ABSTIME(TEMP-DATE)
                DATESEP('/')
                MMDDYYYY(DDATE)
           END-EXEC.
      *
       DOGE-WTO.
           EXEC CICS WRITE OPERATOR
               TEXT(WTO-MESSAGE)
           END-EXEC.
           MOVE SPACES TO WTO-MESSAGE.


       

