       IDENTIFICATION DIVISION.
       PROGRAM-ID. linux_du.
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
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.
       01 LS-EOF PIC 9(1) VALUE 0.
       01 LS-EOL PIC 9(1) VALUE 0.

       01 LS-FILLER PIC X(80).
       01 LS-i PIC 9(3) VALUE 1.
       01 LS-j PIC 9(3) VALUE 1.
       01 LS-k PIC 9(3) VALUE 1.
       01 LS-H PIC X(10).
       01 LS-T PIC X(80).
       01 STRPV PIC 9(3) VALUE 1.
       01 STRPS PIC 9(3) VALUE 1.

       01 LS-CWD PIC X(80) VALUE IS SPACE.
       01 LS-CWD-TMP PIC X(80) VALUE IS SPACE.
       01 LS-CWD-PNT PIC 9(3) VALUE IS 1.
       01 LS-CMD PIC X(10).
       01 LS-CMD-ARGS PIC X(80).
       01 LS-LS-IS-SET PIC 1 VALUE IS 0.

       01 LS-SEARCH-STR PIC X(80) VALUE IS SPACE.
       01 LS-SEARCH-LEN PIC 9(3) VALUE IS 1.
       01 LS-SEARCH-LEN-TMP PIC 9(3) VALUE IS 1.
       01 LS-TMP-DIR PIC X(80) VALUE IS SPACE.
       01 LS-TMP-TMP-DIR PIC X(80) VALUE IS SPACE.
       01 LS-Name-PNT PIC 9(3) VALUE IS 1.
       01 LS-Size-Sum PIC 9(10) VALUE IS 0.
       01 LS-Curr-Min PIC 9(10) VALUE IS 0.
       01 LS-Free-Space PIC 9(10) VALUE IS 0.
       01 LS-Need-Space PIC 9(10) VALUE IS 0.
       01 LS-Full-Name PIC X(80) VALUE IS SPACE.
       01 LS-File-CNT PIC 9(4) VALUE 0.
       01 LS-File-TBL.
          02 LS-File-REC OCCURS 1 TO 1000 TIMES
                                        DEPENDING ON LS-File-CNT
                                        ASCENDING KEY IS LS-Dir-Name
                                        ASCENDING KEY IS LS-File-Name
                                        INDEXED BY D1.
           03 LS-File-Size PIC 9(10) VALUE IS 0.
           03 LS-Is-Dir PIC 1 VALUE IS 0.
           03 LS-File-Name PIC X(10) VALUE IS SPACE.
           03 LS-Dir-Name  PIC X(80) VALUE IS SPACE.


       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(10) VALUE IS SPACE.
       01 L-Result-2 PIC X(10) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF = 1 OR LS-EOL = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM CALCULATE-ROOT-SIZE-ROUTINE.
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
           SET STRPS TO 1.
           UNSTRING FileLine
               DELIMITED BY ALL SPACES
               INTO LS-H,
                    LS-T COUNT STRPV
               WITH POINTER STRPS
               ON OVERFLOW
                   ADD 2 TO STRPV
                   MOVE FileLine(STRPS:) TO LS-T(STRPV:).

           IF LS-H = "$" THEN
               PERFORM COMMAND-LINE-ROUTINE
           ELSE
               PERFORM OUTPUT-LINE-ROUTINE
           END-IF.
       END-ROUTINE.

       COMMAND-LINE-ROUTINE.
           SET LS-LS-IS-SET TO 0.
           UNSTRING LS-T
               DELIMITED BY SPACE
               INTO LS-CMD, LS-CMD-ARGS.
           IF LS-CMD = "cd" THEN
               PERFORM COMMAND-CD-ROUTINE
           ELSE IF LS-CMD = "ls"
               SET LS-LS-IS-SET TO 1
           ELSE
               DISPLAY "Unknown CMD: ", LS-CMD
           END-IF.
       END-ROUTINE.

       COMMAND-CD-ROUTINE.
           IF LS-CMD-ARGS = ".." THEN
               *> MOVE SPACES TO LS-CWD-TMP
               PERFORM VARYING STRPS FROM 1 UNTIL STRPS > 80
                       IF LS-CWD(STRPS:1) = "/" THEN
                           SUBTRACT 1 FROM STRPS GIVING STRPV
                       END-IF
               END-PERFORM
               IF LS-CWD(1:STRPV) = SPACE THEN
                   MOVE "/" TO LS-CWD-TMP
               ELSE
                   MOVE LS-CWD(1:STRPV) TO LS-CWD-TMP
               END-IF
               MOVE LS-CWD-TMP TO LS-CWD
           ELSE IF LS-CMD-ARGS(1:1) = "/" THEN
               MOVE LS-CMD-ARGS TO LS-CWD
           ELSE
               SET LS-CWD-PNT TO 1
               IF LS-CWD = "/" THEN
                   STRING
                       LS-CWD DELIMITED BY SPACE,
                       LS-CMD-ARGS DELIMITED BY SPACE
                       INTO LS-CWD-TMP
               ELSE
                   STRING
                       LS-CWD DELIMITED BY SPACE,
                       "/",
                       LS-CMD-ARGS DELIMITED BY SPACE
                       INTO LS-CWD-TMP
               END-IF
               MOVE LS-CWD-TMP TO LS-CWD
           END-IF.
       END-ROUTINE.

       OUTPUT-LINE-ROUTINE.
           IF LS-LS-IS-SET = 1 THEN
               SEARCH LS-File-REC
                   AT END
                       ADD 1 TO LS-File-CNT
                       MOVE LS-T TO LS-File-Name(LS-File-CNT)
                       MOVE LS-CWD TO LS-Dir-Name(LS-File-CNT)
                       IF LS-H = "dir" THEN
                           MOVE 0 TO LS-File-Size(LS-File-CNT)
                           MOVE 1 TO LS-Is-Dir(LS-File-CNT)
                       ELSE
                           MOVE LS-H TO LS-File-Size(LS-File-CNT)
                       END-IF
                   WHEN LS-T = LS-File-Name(D1)
                    AND LS-CWD = LS-Dir-Name(D1)
                       DISPLAY "Duplicate entry ", LS-T, LS-CWD
               END-SEARCH
           ELSE
               DISPLAY "UNKNOWN OUTPUT: ", FileLine
           END-IF.
       END-ROUTINE.

       CALCULATE-ROOT-SIZE-ROUTINE.
           PERFORM FIND-DIRS-ROUTINE.

           MOVE "/" TO LS-TMP-DIR.
           PERFORM CALCULATE-DIR-SIZES-ROUTINE.
           *> 30000000 - (70000000 - LS-Size-Sum)
           SUBTRACT LS-Size-Sum FROM 70000000 GIVING LS-Free-Space.
           SUBTRACT LS-Free-Space FROM 30000000 GIVING LS-Need-Space.

           PERFORM FIND-SIZES-ROUTINE.
       END-ROUTINE.

       FIND-DIRS-ROUTINE.
           PERFORM VARYING LS-i FROM 1 UNTIL LS-i > LS-File-CNT
               IF LS-File-Size(LS-i) = 0 THEN
                   PERFORM SET-FULL-NAME-ROUTINE
                   MOVE LS-Full-Name TO LS-TMP-DIR
                   PERFORM CALCULATE-DIR-SIZES-ROUTINE
                   MOVE LS-Size-Sum TO LS-File-Size(LS-i)
               END-IF
           END-PERFORM.
       END-ROUTINE.

       CALCULATE-DIR-SIZES-ROUTINE.
           SET LS-Size-Sum TO 0.
           INITIALIZE LS-SEARCH-STR.
           SET LS-Name-PNT TO 1.
           STRING LS-TMP-DIR DELIMITED BY SPACE
               INTO LS-SEARCH-STR
               WITH POINTER LS-Name-PNT.
           SUBTRACT 1 FROM LS-Name-PNT GIVING LS-SEARCH-LEN.
           PERFORM VARYING LS-j FROM 1 UNTIL LS-j > LS-File-CNT
               IF LS-SEARCH-STR =
                       LS-Dir-Name(LS-j)(1:LS-SEARCH-LEN) THEN
                   ADD 1 TO LS-SEARCH-LEN GIVING LS-SEARCH-LEN-TMP
                   IF LS-Dir-Name(LS-j)(LS-SEARCH-LEN-TMP:1) = "/" OR
                      LS-Dir-Name(LS-j)(LS-SEARCH-LEN-TMP:1) = SPACE
                           THEN
                       ADD LS-File-Size(LS-j) TO LS-Size-Sum
                   END-IF
               END-IF
           END-PERFORM.
       END-ROUTINE.

       SET-FULL-NAME-ROUTINE.
           INITIALIZE LS-Full-Name.
           IF LS-Dir-Name(LS-i) = "/" THEN
               SET LS-Name-PNT TO 1
               STRING LS-Dir-Name(LS-i) DELIMITED BY SPACE,
                   LS-File-Name(LS-i) DELIMITED BY SPACE
                   INTO LS-Full-Name
                   WITH POINTER LS-Name-PNT
               SUBTRACT 1 FROM LS-Name-PNT
           ELSE
               SET LS-Name-PNT TO 1
               STRING LS-Dir-Name(LS-i) DELIMITED BY SPACE,
                   "/",
                   LS-File-Name(LS-i) DELIMITED BY SPACE
                   INTO LS-Full-Name
                   WITH POINTER LS-Name-PNT
               SUBTRACT 1 FROM LS-Name-PNT
           END-IF.
       END-ROUTINE.

       FIND-SIZES-ROUTINE.
           SET LS-Size-Sum TO 0.
           SET LS-Curr-Min TO 70000000.
           PERFORM VARYING LS-i FROM 1 UNTIL LS-i > LS-File-CNT
               IF LS-Is-Dir(LS-i) = 1 AND
                       LS-File-Size(LS-i) <= 100000 THEN
                   ADD LS-File-Size(LS-i) TO LS-Size-Sum
               END-IF
               IF LS-Is-Dir(LS-i) = 1 AND
                       LS-File-Size(LS-i) >= LS-Need-Space THEN
                   IF LS-File-Size(LS-i) < LS-Curr-Min THEN
                       SET LS-Curr-Min TO LS-File-Size(LS-i)
                   END-IF
               END-IF
           END-PERFORM.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-Size-Sum TO L-Result-1.
           MOVE LS-Curr-Min TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM linux_du.
