       IDENTIFICATION DIVISION.
       PROGRAM-ID. SENDDATAQ.
       ENVIRONMENT DIVISION.
       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01  DTAQ-NAME             PIC X(10) VALUE 'MYDATAQ'.
       01  LIB-NAME              PIC X(10) VALUE 'MYLIB'.
       01  MESSAGE               PIC X(256) VALUE 'Hello from COBOL!'.
       01  MESSAGE-LEN           PIC 9(5) BINARY.
       01  STATUS                PIC S9(9) BINARY.

       PROCEDURE DIVISION.
           MOVE LENGTH OF MESSAGE TO MESSAGE-LEN.

           CALL 'QSNDDTAQ' USING
               BY REFERENCE DTAQ-NAME
               BY REFERENCE LIB-NAME
               BY REFERENCE MESSAGE-LEN
               BY REFERENCE MESSAGE
               BY REFERENCE STATUS.

           IF STATUS NOT = 0
               DISPLAY 'Error sending to Data Queue! Status: ' STATUS
           ELSE
               DISPLAY 'Message sent successfully!'.

           STOP RUN.
