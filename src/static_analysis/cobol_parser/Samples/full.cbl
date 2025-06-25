       IDENTIFICATION DIVISION.
       PROGRAM-ID. FULL-TEST-PROGRAM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VAR1 PIC 9(4) VALUE 1000.
       01 WS-VAR2 PIC 9(4) VALUE 2000.
       01 WS-NAME PIC A(20) VALUE "COBOL TEST".
       01 WS-COUNTER PIC 9(2) VALUE 0.
       01 WS-STATUS PIC X(10) VALUE "INIT".
       01 WS-EOF-FLAG PIC X VALUE 'N'.  *> 'N' for No, 'Y' for Yes

       FILE SECTION.
       FD SAMPLE-FILE
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 10 RECORDS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS SAMPLE-RECORD.

       01 SAMPLE-RECORD.
           05 RECORD-ID PIC 9(4).
           05 RECORD-DATA PIC X(76).

       PROCEDURE DIVISION.

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
           MOVE 1 TO RECORD-ID OF SAMPLE-RECORD. *> Initialize the record id
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
           MOVE WS-NAME TO RECORD-DATA OF SAMPLE-RECORD. *> Move to the correct part of the record
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

