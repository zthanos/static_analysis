       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECEIVEDATAQ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  DTAQ-NAME             PIC X(10) VALUE 'MYDATAQ'.
       01  LIB-NAME              PIC X(10) VALUE 'MYLIB'.
       01  MESSAGE               PIC X(256).
       01  MESSAGE-LEN           PIC 9(5) BINARY.
       01  WAIT-TIME             PIC 9(5) BINARY VALUE 10.
       01  STATUS                PIC S9(9) BINARY.

       PROCEDURE DIVISION.
           CALL 'QRCVDTAQ' USING
               BY REFERENCE DTAQ-NAME
               BY REFERENCE LIB-NAME
               BY REFERENCE MESSAGE-LEN
               BY REFERENCE MESSAGE
               BY REFERENCE WAIT-TIME
               BY REFERENCE STATUS.

           IF STATUS NOT = 0
               DISPLAY 'Error receiving from Data Queue! Status: ' STATUS
           ELSE
               DISPLAY 'Message received: ' MESSAGE.

           STOP RUN.
