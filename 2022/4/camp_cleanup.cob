       IDENTIFICATION DIVISION.
       PROGRAM-ID. camp_cleanup.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 FileLine PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "input".

       LOCAL-STORAGE SECTION.

       01 LS-EOF PIC 9(1) VALUE 0.

       01 F-E1-L PIC 9(3).
       01 F-E1-H PIC 9(3).
       01 F-E2-L PIC 9(3).
       01 F-E2-H PIC 9(3).

       01 LS-EncasedIn PIC 9(3) VALUE IS ZERO.
       01 LS-Overlapping PIC 9(3) VALUE IS ZERO.

       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC 9(10) VALUE IS ZERO.
       01 L-Result-2 PIC 9(10) VALUE IS ZERO.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM MOVE-RESULT-ROUTINE.
           EXIT PROGRAM.
           STOP RUN.
       END-ROUTINE.

       OPEN-FILE-ROUTINE.
           MOVE L-Filename TO WS-Filename.
           OPEN INPUT F-input-FILE.
       END-ROUTINE.

       READ-LINE-ROUTINE.
           READ F-input-FILE RECORD
               AT END SET LS-EOF TO 1
               NOT AT END PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           UNSTRING FileLine
               DELIMITED BY "-" OR ","
               INTO F-E1-L,
                    F-E1-H,
                    F-E2-L,
                    F-E2-H.
           IF F-E1-L <= F-E2-L AND F-E1-H >= F-E2-H THEN
               ADD 1 TO LS-EncasedIn
           ELSE IF F-E2-L <= F-E1-L AND F-E2-H >= F-E1-H THEN
               ADD 1 TO LS-EncasedIn
           END-IF.

           IF F-E1-H < F-E2-L OR F-E2-H < F-E1-L THEN
               CONTINUE
           ELSE
               ADD 1 TO LS-Overlapping
           END-IF.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-EncasedIn TO L-Result-1.
           MOVE LS-Overlapping TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM camp_cleanup.

