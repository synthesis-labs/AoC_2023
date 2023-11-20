       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2021DAYONE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
           FILE-CONTROL.
           SELECT SONAR-DATA ASSIGN TO 'sonar-data'
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD SONAR-DATA.
       01 FD-SONAR-FILE.
           05 FD-SONAR-VALUE   PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-SONAR-DATA.
           05 WS-DEPTH-VALUE   PIC 9(4).
       01 WS-PREVIOUs.
           05 WS-WS-DATA       PIC 9(4).
       01 WS-EOF               PIC A(1) VALUE 'N'.
       01 WS-INCREASES         PIC 9(4) VALUE 0.
       01 WS-COUNT             PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
           PERFORM 1001-INPUT-FILE.
           DISPLAY "NUMBER OF POINTS - " WS-COUNT  
           DISPLAY "NUMBER OF INCREASES - " WS-INCREASES
           STOP RUN.

       1001-INPUT-FILE.
           OPEN INPUT SONAR-DATA.
           PERFORM READ-INPUT-FILE.
           CLOSE SONAR-DATA.

       READ-INPUT-FILE.
           PERFORM UNTIL WS-EOF='Y'
               READ SONAR-DATA INTO WS-SONAR-DATA
                   AT END SET WS-EOF TO 'Y'
                   NOT AT END
                       ADD 1 TO WS-COUNT
                       IF WS-SONAR-DATA > WS-PREVIOUS
                           ADD 1 TO WS-INCREASES
                       END-IF
                       SET WS-PREVIOUS TO WS-SONAR-DATA
               END-READ
           END-PERFORM.
