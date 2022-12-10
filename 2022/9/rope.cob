       IDENTIFICATION DIVISION.
       PROGRAM-ID. rope.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 F-Move-TBL.
         05 F-Direction PIC X.
         05 FILLER PIC Z.
         05 F-Distance PIC Z(3).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.
       01 LS-EOF PIC 9(1) VALUE 0.

       01 LS-Knots CONSTANT 11.
       01 LS-Offset CONSTANT 5000.
       01 LS-Grid-Size CONSTANT 10000.

       01 LS-Rope OCCURS LS-Knots TIMES.
         02 LS-X PIC S9(3) VALUE IS ZERO.
         02 LS-Y PIC S9(3) VALUE IS ZERO.

       01 LS-Num-Visited PIC 9(5) OCCURS LS-Knots TIMES.
       01 LS-History
                                        OCCURS LS-Grid-Size TIMES.
         02 LS-H-Rows
                                        OCCURS LS-Grid-Size TIMES.
            03 LS-Visited PIC 1 OCCURS LS-Knots TIMES VALUE 0.

       01 LS-Distance PIC 9(3).
       01 LS-i PIC 9(3).
       01 LS-j PIC 9(2).
       01 LS-k PIC 9(2).
       01 LS-tmp-Rope OCCURS LS-Knots TIMES.
           02 LS-X-tmp PIC S9(3).
           02 LS-Y-tmp PIC S9(3).

       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(10) VALUE IS SPACE.
       01 L-Result-2 PIC X(10) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM VARYING LS-j FROM 1 UNTIL LS-j > LS-Knots
               SET LS-Num-Visited(LS-j) TO 1
               SET LS-Visited(0+LS-Offset,
                              0+LS-Offset,
                              LS-j) TO 1
           END-PERFORM.

           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE TEST AFTER UNTIL LS-EOF = 1.
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
           READ F-input-FILE RECORD INTO F-Move-TBL
               AT END
                   SET LS-EOF TO 1
               NOT AT END
                   PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           UNSTRING
               F-Distance DELIMITED BY SPACES
               INTO LS-Distance.

           EVALUATE F-Direction
               when "R"
                   ADD LS-Distance TO LS-X(1)
               when "L"
                   SUBTRACT LS-Distance FROM LS-X(1)
               when "U"
                   SUBTRACT LS-Distance FROM LS-Y(1)
               when "D"
                   ADD LS-Distance TO LS-Y(1)
           END-EVALUATE.
           PERFORM CHECK-DISTANCE-ROUTINE.
       END-ROUTINE.

       CHECK-DISTANCE-ROUTINE.
           PERFORM
               VARYING LS-i FROM 1
               UNTIL LS-i > LS-Distance
               PERFORM MOVE-T-ONE-ROUTINE
                   VARYING LS-j FROM 2
                   UNTIL LS-j > LS-Knots
           END-PERFORM.
       END-ROUTINE.

       MOVE-T-ONE-ROUTINE.
           SUBTRACT 1 FROM LS-j GIVING LS-k.

           SUBTRACT LS-X(LS-k) FROM LS-X(LS-j) GIVING LS-X-Tmp(LS-j)
           SUBTRACT LS-Y(LS-k) FROM LS-Y(LS-j) GIVING LS-Y-Tmp(LS-j)

           EVALUATE LS-X-Tmp(LS-j) ALSO LS-Y-Tmp(LS-j)
               WHEN 1 ALSO 1
                   CONTINUE
               WHEN 1 ALSO 0
                   CONTINUE
               WHEN 1 ALSO -1
                   CONTINUE
               WHEN 0 ALSO 1
                   CONTINUE
               WHEN 0 ALSO 0
                   CONTINUE
               WHEN 0 ALSO -1
                   CONTINUE
               WHEN -1 ALSO 1
                   CONTINUE
               WHEN -1 ALSO 0
                   CONTINUE
               WHEN -1 ALSO -1
                   CONTINUE
               WHEN > 0 ALSO > 0
                   SUBTRACT 1 FROM LS-X(LS-j)
                   SUBTRACT 1 FROM LS-Y(LS-j)
               WHEN < 0 ALSO < 0
                   ADD 1 TO LS-X(LS-j)
                   ADD 1 TO LS-Y(LS-j)
               WHEN > 0 ALSO < 0
                   SUBTRACT 1 FROM LS-X(LS-j)
                   ADD 1 TO LS-Y(LS-j)
               WHEN < 0 ALSO > 0
                   ADD 1 TO LS-X(LS-j)
                   SUBTRACT 1 FROM LS-Y(LS-j)

               WHEN > 0 ALSO ANY
                   SUBTRACT 1 FROM LS-X(LS-j)
               WHEN < 0 ALSO ANY
                   ADD 1 TO LS-X(LS-j)
               WHEN ANY ALSO > 0
                   SUBTRACT 1 FROM LS-Y(LS-j)
               WHEN ANY ALSO < 0
                   ADD 1 TO LS-Y(LS-j)
           END-EVALUATE.

           IF LS-Visited(LS-Y(LS-j)+LS-Offset,
                         LS-X(LS-j)+LS-Offset,
                         LS-j) = 0 THEN
               SET LS-Visited(LS-Y(LS-j)+LS-Offset,
                              LS-X(LS-j)+LS-Offset,
                              LS-j) TO 1
               ADD 1 TO LS-Num-Visited(LS-j)
           END-IF.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-Num-Visited(2)  TO L-Result-1.
           MOVE LS-Num-Visited(10) TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM rope.
