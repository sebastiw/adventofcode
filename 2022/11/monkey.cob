       IDENTIFICATION DIVISION.
       PROGRAM-ID. monkey.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 F-Line PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.
       01 LS-EOF PIC 9(1) VALUE 0.

       01 LS-H PIC X(80).
       01 LS-T PIC X(80).

       01 LS-i PIC 9(4).
       01 LS-Current-Monkey PIC 9.
       01 LS-Receiver PIC 9.
       01 LS-Common-Divisor PIC 9(15) VALUE 1.

       01 LS-P1 PIC 9(25).
       01 LS-P2 PIC 9(25).
       01 LS-Test PIC 9(15).
       01 LS-D PIC 9(5).
       01 LS-Round PIC 9(5).
       01 LS-Rounds PIC 9(5).
       01 LS-Highest-1 PIC 9(10) VALUE 0.
       01 LS-Highest-2 PIC 9(10) VALUE 0.
       01 LS-Result PIC 9(15) VALUE 0.

       01 LS-Monkey-CNT PIC 9 VALUE IS 1.
       01 LS-Monkey-TBL.
           05 LS-Monkey OCCURS 10 TIMES.
               10 LS-Monkey-IDX PIC 9.
               10 LS-Items-CNT PIC 9(4) VALUE IS 0.
               10 LS-Inspected-CNT PIC 9(10) VALUE IS 0.
               10 LS-Items OCCURS 100 TIMES.
                    15 LS-Item  PIC 9(25).
               10 LS-Operation.
                    15 LS-Result-In   PIC X(3).
                    15 LS-Parameter-1 PIC X(3).
                    15 LS-Op          PIC X(1).
                    15 LS-Parameter-2 PIC X(3).
               10 LS-Test-Divisable PIC 9(5).
               10 LS-Test-True  PIC 9.
               10 LS-Test-False PIC 9.

       01 LS-Monkey-TBL-INIT.
           05 LS-Monkey-tmp OCCURS 10 TIMES.
               10 LS-Monkey-IDX-tmp PIC 9.
               10 LS-Items-CNT-tmp PIC 9(4) VALUE IS 0.
               10 LS-Inspected-CNT-tmp PIC 9(10) VALUE IS 0.
               10 LS-Items-tmp OCCURS 100 TIMES.
                    15 LS-Item-tmp  PIC 9(25).
               10 LS-Operation-tmp.
                    15 LS-Result-In-tmp   PIC X(3).
                    15 LS-Parameter-1-tmp PIC X(3).
                    15 LS-Op-tmp          PIC X(1).
                    15 LS-Parameter-2-tmp PIC X(3).
               10 LS-Test-Divisable-tmp PIC 9(5).
               10 LS-Test-True-tmp  PIC 9.
               10 LS-Test-False-tmp PIC 9.


       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(15) VALUE IS SPACE.
       01 L-Result-2 PIC X(15) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE TEST AFTER UNTIL LS-EOF = 1.
           PERFORM CLOSE-FILE-ROUTINE.

           MOVE LS-Monkey-TBL TO LS-Monkey-TBL-INIT
           SET LS-Rounds TO 20

           PERFORM CALCULATE-ROUND-ROUTINE
               VARYING LS-Round FROM 1
               UNTIL LS-Round > LS-Rounds.

           PERFORM MOVE-RESULT-ROUTINE.
           MOVE LS-Monkey-TBL-INIT TO LS-Monkey-TBL
           SET LS-Rounds TO 10000

           PERFORM CALCULATE-ROUND-ROUTINE
               VARYING LS-Round FROM 1
               UNTIL LS-Round > LS-Rounds.

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
               AT END
                   SET LS-EOF TO 1
               NOT AT END
                   PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           UNSTRING F-Line
               DELIMITED BY ALL ":"
               INTO LS-H,
                    LS-T.

           EVALUATE LS-H
               WHEN "  Starting items"
                   UNSTRING LS-T
                       DELIMITED BY ALL ", "
                       INTO LS-Item(LS-Monkey-CNT, 1),
                            LS-Item(LS-Monkey-CNT, 2),
                            LS-Item(LS-Monkey-CNT, 3),
                            LS-Item(LS-Monkey-CNT, 4),
                            LS-Item(LS-Monkey-CNT, 5),
                            LS-Item(LS-Monkey-CNT, 6),
                            LS-Item(LS-Monkey-CNT, 7),
                            LS-Item(LS-Monkey-CNT, 8),
                            LS-Item(LS-Monkey-CNT, 9)
                   PERFORM TEST AFTER
                           VARYING LS-i FROM 1
                           UNTIL LS-Item(LS-Monkey-CNT, LS-i) = 0
                       CONTINUE
                   END-PERFORM
                   SUBTRACT 1 FROM LS-i
                       GIVING LS-Items-CNT(LS-Monkey-CNT)

               WHEN "  Operation"
                   UNSTRING LS-T(2:)
                       DELIMITED BY ALL " = "
                                 OR ALL " "
                       INTO LS-Result-In(LS-Monkey-CNT),
                            LS-Parameter-1(LS-Monkey-CNT),
                            LS-Op(LS-Monkey-CNT),
                            LS-Parameter-2(LS-Monkey-CNT)
               WHEN "  Test"
                   MOVE LS-T(14:) TO LS-Test-Divisable(LS-Monkey-CNT)
                   MULTIPLY LS-Test-Divisable(LS-Monkey-CNT)
                       BY LS-Common-Divisor
               WHEN "    If true"
                   MOVE LS-T(17:) TO LS-Test-True(LS-Monkey-CNT)
               WHEN "    If false"
                   MOVE LS-T(17:) TO LS-Test-False(LS-Monkey-CNT)
               WHEN SPACE
                   ADD 1 TO LS-Monkey-CNT
               WHEN OTHER
                   IF LS-H(1:6) = "Monkey" THEN
                       UNSTRING F-Line
                           DELIMITED BY ALL SPACES
                                     OR ALL ":"
                           INTO LS-H, LS-T
                       MOVE LS-T TO LS-Monkey-IDX(LS-Monkey-CNT)
                   ELSE
                       DISPLAY "O: ", F-Line
                   END-IF
           END-EVALUATE.
       END-ROUTINE.

       CALCULATE-ROUND-ROUTINE.
           PERFORM MONKEY-ROUND-ROUTINE
               VARYING LS-Current-Monkey FROM 1
               UNTIL LS-Current-Monkey > LS-Monkey-CNT.
       END-ROUTINE.

       MONKEY-ROUND-ROUTINE.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Items-CNT(LS-Current-Monkey)

               ADD 1 TO LS-Inspected-CNT(LS-Current-Monkey)

               IF LS-Parameter-1(LS-Current-Monkey) = "old" THEN
                   MOVE LS-Item(LS-Current-Monkey, LS-i) TO LS-P1
               ELSE
                   MOVE LS-Parameter-1(LS-Current-Monkey) TO LS-P1
               END-IF
               IF LS-Parameter-2(LS-Current-Monkey) = "old" THEN
                   MOVE LS-Item(LS-Current-Monkey, LS-i) TO LS-P2
               ELSE
                   MOVE LS-Parameter-2(LS-Current-Monkey) TO LS-P2
               END-IF

               COMPUTE LS-P1 = FUNCTION
                   MOD(LS-P1, LS-Common-Divisor)
               COMPUTE LS-P2 = FUNCTION
                   MOD(LS-P2, LS-Common-Divisor)

               EVALUATE LS-Op(LS-Current-Monkey)
                   WHEN "+"
                       ADD LS-P1 TO LS-P2
                   WHEN "*"
                       MULTIPLY LS-P1 BY LS-P2
                   WHEN OTHER
                       DISPLAY "Unknown Op: ", LS-Op(LS-Current-Monkey)
               END-EVALUATE

               IF LS-Rounds = 20 THEN
                   DIVIDE LS-P2 BY 3 GIVING LS-P2
               ELSE
                   COMPUTE LS-P2 = FUNCTION
                       MOD(LS-P2, LS-Common-Divisor)
               END-IF

               MOVE LS-Test-Divisable(LS-Current-Monkey) TO LS-D

               COMPUTE LS-Test = FUNCTION MOD(LS-P2, LS-D)

               IF LS-Test = 0 THEN
                   ADD 1 TO LS-Test-True(LS-Current-Monkey)
                       GIVING LS-Receiver
               ELSE
                   ADD 1 TO LS-Test-False(LS-Current-Monkey)
                       GIVING LS-Receiver
               END-IF

               ADD      1   TO LS-Items-CNT(LS-Receiver)

               MOVE LS-P2 TO LS-Item(LS-Receiver,
                                     LS-Items-CNT(LS-Receiver))
               SET LS-Item(LS-Current-Monkey, LS-i) TO 0
           END-PERFORM.
           SET LS-Items-CNT(LS-Current-Monkey) TO 0.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           SET LS-Highest-1 TO 0
           SET LS-Highest-2 TO 0
           PERFORM VARYING LS-Current-Monkey FROM 1
                   UNTIL LS-Current-Monkey > LS-Monkey-CNT
               IF LS-Inspected-CNT(LS-Current-Monkey) > LS-Highest-2
                       THEN
                   IF LS-Inspected-CNT(LS-Current-Monkey) > LS-Highest-1
                           THEN
                       SET LS-Highest-2 TO LS-Highest-1
                       SET LS-Highest-1
                           TO LS-Inspected-CNT(LS-Current-Monkey)
                   ELSE
                       SET LS-Highest-2
                           TO LS-Inspected-CNT(LS-Current-Monkey)
                   END-IF
               END-IF
           END-PERFORM.

           MULTIPLY LS-Highest-1 BY LS-Highest-2 GIVING LS-Result.

           IF LS-Rounds = 20 THEN
               MOVE LS-Result TO L-Result-1
           ELSE
               MOVE LS-Result TO L-Result-2
           END-IF.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM monkey.
