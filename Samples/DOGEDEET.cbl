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
       PROGRAM-ID.   DOGEDEET.
       AUTHOR. SOLDIER OF FORTRAN.
       INSTALLATION. DOGE BANK.
       DATE-WRITTEN. 08/30/20.
       SECURITY. CONFIDENTIAL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
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
      * Edit to display the amount         
       01  THE-AMOUNT                       PIC S9(8)V9(8).
       01  FILLER REDEFINES THE-AMOUNT.
           05  THE-AMOUNT-INTEGER           PIC X(8).
           05  THE-AMOUNT-DECIMAL           PIC S9(8).
       01  RECENT-COLOR                     PIC X.
       01  DISPLAY-TRAN.
           05  DDATE.
               10  FDATE PIC X(10)B.
               10  FTIME PIC X(8).     
           05  DTYPE     PIC X(10) VALUE 'RECV FROM'.
           05  DLABEL    PIC X(10).
           05  DADDRESS  PIC X(34).
           05  DSIGN     PIC X VALUE '+'.
           05  DAMOUNT   PIC Z(02),Z(03),Z(02)9.9(8).
       01  TEMP-DATE     PIC 9(15) COMP-3.
       01  SINCE-EPOCH   PIC S9(15) COMP-3 VALUE +2208988800000.
       01  RESPONSE-CODE  PIC S9(4) COMP.
       01  DOGECOMMS-AREA.
           05  RECORD-ID PIC 9(10) VALUE 0000000002.
       01  LINE-NUMBER PIC 9 VALUE 0.
       01  WTO-MESSAGE PIC X(38) VALUE SPACES.
       01  FOUND-RECORD PIC X(4).
           88  WE-GOT-IT VALUE 'YEAH'.
           88  NOPE-DONT-GOT-IT VALUE 'NOPE'.
       01  DIRECTION PIC X.
           88  FORWARD  VALUE 'F'.
           88  BACKWARD VALUE 'B'.
       01  BAD-KEY.
           05  KEY-MSG1 PIC X(3)B VALUE 'KEY'.
           05  KEY-ID   PIC X(10)B.
           05  KEY-MSG2 PIC X(15) VALUE 'DOES NOT EXIST.'.
      *
      *COPY DOGEDT.
      *COPY DFHAID.
      *COPY DFHBMSCA.
      *COPY ERRPARM.
      *
       LINKAGE SECTION.
       01  DFHCOMMAREA                       PIC X(10).
       PROCEDURE DIVISION.
       DOGE-MAIN.
      *
           IF EIBCALEN > ZERO THEN
               MOVE DFHCOMMAREA TO DOGECOMMS-AREA.

           IF EIBCALEN EQUAL TO ZERO
              MOVE 'Displaying Empty Details.' TO WTO-MESSAGE
              PERFORM DOGE-WTO
      * MAP IS DFHMDI FROM THE MAPSET
      * MAPSET IS WHAT WE SET IN THE PCT (WITH CEDA)
              EXEC CICS SEND MAP('DOGEDT1')
                  MAPSET('DOGEDT') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF8
              MOVE 'Showing next transaction' TO WTO-MESSAGE
              PERFORM DOGE-WTO
              MOVE 'F' TO DIRECTION
              PERFORM DOGE-START-BROWSE
              PERFORM DOGE-SHOW-TRANSACTION
              EXEC CICS SEND MAP('DOGEDT1')
                  MAPSET('DOGEDT') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF7
              MOVE 'Showing prev transaction' TO WTO-MESSAGE
              MOVE 'B' TO DIRECTION
              PERFORM DOGE-START-BROWSE
              PERFORM DOGE-SHOW-TRANSACTION
              EXEC CICS SEND MAP('DOGEDT1')
                  MAPSET('DOGEDT') ERASE
              END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF3
               EXEC CICS XCTL 
                   PROGRAM('DOGEQUIT')
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHPF6
               MOVE 'Going back to transactions' TO WTO-MESSAGE
               PERFORM DOGE-WTO
               EXEC CICS XCTL 
                   PROGRAM('DOGETRAN')
               END-EXEC
           ELSE
           IF EIBAID EQUAL TO DFHENTER
               PERFORM RECEIVE-KEY
               PERFORM PARSE-KEY
               EXEC CICS SEND MAP('DOGEDT1')
                   MAPSET('DOGEDT') ERASE
               END-EXEC.  
           EXEC CICS
               RETURN TRANSID('DEET')
                      COMMAREA(DOGECOMMS-AREA)
           END-EXEC.
       DOGE-EXIT.
           GOBACK.
      *
       DOGE-WTO.
           EXEC CICS WRITE OPERATOR
               TEXT(WTO-MESSAGE)
           END-EXEC.
           MOVE SPACES TO WTO-MESSAGE.

      * Start by checking where we are or if we have a valid key
       DOGE-START-BROWSE.

           EXEC CICS
               STARTBR FILE('DOGEVSAM')
                       RIDFLD(RECORD-ID)
                       EQUAL
                       RESP(RESPONSE-CODE)
           END-EXEC.

           IF RESPONSE-CODE IS EQUAL TO DFHRESP(NOTFND) THEN
               MOVE 'NOPE' TO FOUND-RECORD
           ELSE
               MOVE 'YEAH' TO FOUND-RECORD.

           IF RECORD-ID IS EQUAL TO 
              0000000001 OR  0000000002 OR 9999999999 THEN
               MOVE 'NOPE' TO FOUND-RECORD.
           
      * Shush compiler warnings         
           MOVE SPACES TO WTO-MESSAGE.

       DOGE-SHOW-TRANSACTION.

           IF WE-GOT-IT AND BACKWARD THEN
               EXEC CICS READPREV FILE('DOGEVSAM')
                   RIDFLD(RECORD-ID)
                   INTO(TRANSACTION)
               END-EXEC
               EXEC CICS READPREV FILE('DOGEVSAM')
                   RIDFLD(RECORD-ID)
                   INTO(TRANSACTION)
               END-EXEC
            ELSE
            IF WE-GOT-IT AND FORWARD THEN
               EXEC CICS READNEXT FILE('DOGEVSAM')
                   RIDFLD(RECORD-ID)
                   INTO(TRANSACTION)
               END-EXEC
               EXEC CICS READNEXT FILE('DOGEVSAM')
                   RIDFLD(RECORD-ID)
                   INTO(TRANSACTION)
               END-EXEC
            ELSE
            IF WE-GOT-IT THEN
               EXEC CICS READNEXT FILE('DOGEVSAM')
                   RIDFLD(RECORD-ID)
                   INTO(TRANSACTION)
               END-EXEC.
           
           IF RECORD-ID EQUAL TO 0000000002 OR 9999999999 THEN
               MOVE 'NOPE' TO FOUND-RECORD.

           IF WE-GOT-IT THEN
               PERFORM CONVERT-DATE
               PERFORM CONVERT-AMOUNT-TO-DISPLAY
               MOVE TLABEL TO DLABEL
               MOVE TADDRSS TO DADDRESS
               PERFORM FILL-SCREEN-DATA
           ELSE
               MOVE RECORD-ID TO KEY-ID
               MOVE BAD-KEY TO ERRORO
               MOVE DFHREVRS TO KEYH.
      * Shush compiler warnings    
           MOVE SPACES TO WTO-MESSAGE.

       FILL-SCREEN-DATA.
           MOVE TDATE TO KEYO.
           MOVE DDATE TO DATEO.
           IF TAMT-SIGN-NEGATIVE THEN
               MOVE '  to' TO FROMO
               MOVE ' debit' TO CREDITO.
           MOVE DLABEL TO LABELO.
           MOVE DADDRESS TO ADDRESSO.
           MOVE DAMOUNT TO AMOUNTO.
           
       CONVERT-AMOUNT-TO-DISPLAY.
      * Converts the number from VSAM to ##,###,###.########
           MOVE DFHGREEN TO RECENT-COLOR.
           MOVE TAMT-INTEGER-PART TO THE-AMOUNT-INTEGER.
           MOVE TAMT-DECIMAL-PART TO THE-AMOUNT-DECIMAL.
           MOVE 'RECV FROM' TO DTYPE.
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
                MMDDYYYY(FDATE)
           END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(TEMP-DATE)
                TIMESEP(':')
                TIME(FTIME)
           END-EXEC.

       RECEIVE-KEY.
      * Get the option the user enters
           EXEC CICS
               RECEIVE MAP('DOGEDT1')
                       MAPSET('DOGEDT')
                       INTO(DOGEDT1I)
                       ASIS
           END-EXEC.
       PARSE-KEY.
      *    Do we want to leave this screen? 
           IF OPTIONI EQUAL TO 'T' OR OPTIONI EQUAL TO 't'
      *       OR OPTIONI EQUAL TO 'M' OR OPTIONI EQUAL TO 'm'
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
               EXEC CICS XCTL 
                   PROGRAM('DOGESEND')
               END-EXEC
           ELSE
               MOVE KEYI TO RECORD-ID
               MOVE 'DEET - GOT RECORD ID:' TO WTO-MESSAGE.
               PERFORM DOGE-WTO
               MOVE RECORD-ID TO WTO-MESSAGE.
               PERFORM DOGE-WTO
               PERFORM DOGE-START-BROWSE
               PERFORM DOGE-SHOW-TRANSACTION.
           MOVE SPACES TO WTO-MESSAGE.
      * Shush compiler warnings    
           MOVE SPACES TO WTO-MESSAGE.
               