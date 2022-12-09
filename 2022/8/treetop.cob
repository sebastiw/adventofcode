       IDENTIFICATION DIVISION.
       PROGRAM-ID. treetop.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 F-Forest-TBL.
         05 F-Rows OCCURS 100 TIMES.
             15 F-Cols OCCURS 100 TIMES.
                20 F-Height PIC 9.

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.
       01 LS-EOF PIC 9(1) VALUE 0.

       01 LS-a PIC 9 VALUE 0.
       01 LS-b PIC 9 VALUE 0.
       01 LS-i PIC 9(3) VALUE 1.
       01 LS-j PIC 9(3) VALUE 1.
       01 LS-k PIC 9(3) VALUE 1.
       01 LS-INVISIBLE PIC 1 VALUE 0.
       01 LS-FROM PIC 9(3) VALUE 1.

       01 LS-Rows-CNT PIC 9(3) VALUE IS 1.
       01 LS-Forest-TBL.
          03 LS-Rows OCCURS 100 TIMES.
             05 LS-Cols-CNT PIC 9(3) VALUE IS 1.
             05 LS-Cols OCCURS 100 TIMES.
                07 LS-Height PIC 9.
                07 LS-Visible PIC 1 VALUE IS ZERO.
                07 LS-View PIC 9(8) VALUE IS ZERO.

       01 LS-Visible-Trees-Sum PIC 9(4).
       01 LS-View-Distance PIC 9(8).
       01 LS-View-tmp PIC 9(8).
       01 LS-View-stp PIC 1 VALUE 0.

       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(10) VALUE IS SPACE.
       01 L-Result-2 PIC X(10) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
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
           READ F-input-FILE RECORD INTO F-Rows(LS-Rows-CNT)
               AT END
                   *> SUBTRACT 1 FROM LS-Rows-CNT
                   SET LS-EOF TO 1
               NOT AT END
                   PERFORM DO-LINE-ROUTINE
                   ADD 1 TO LS-Rows-CNT.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           PERFORM VARYING LS-i FROM 1
                   UNTIL F-Cols(LS-Rows-CNT, LS-i) = SPACE OR
                         F-Cols(LS-Rows-CNT, LS-i) = LOW-VALUE
                   MOVE F-Cols(LS-Rows-CNT, LS-i)
                       TO LS-Cols(LS-Rows-CNT, LS-i)
           END-PERFORM.
           SUBTRACT 1 FROM LS-i GIVING LS-Cols-CNT(LS-Rows-CNT).
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           SUBTRACT 1 FROM LS-Rows-CNT
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Rows-CNT
               PERFORM VARYING LS-j FROM 1
                       UNTIL LS-j > LS-Cols-CNT(LS-i)

                   SET LS-View-Distance TO 1

                   *> Check POS -> DOWN
                   SET LS-INVISIBLE TO 0
                   SET LS-View-tmp TO 0
                   SET LS-View-stp TO 0
                   ADD 1 TO LS-i GIVING LS-FROM
                   PERFORM VARYING LS-k FROM LS-FROM
                           UNTIL LS-k > LS-Rows-CNT
                       IF LS-View-stp = 0 THEN
                           ADD 1 TO LS-View-tmp
                       END-IF
                       IF LS-Height(LS-k, LS-j) >= LS-Height(LS-i, LS-j)
                               THEN
                           SET LS-INVISIBLE TO 1
                           SET LS-View-stp TO 1
                       END-IF
                   END-PERFORM
                   IF LS-INVISIBLE = 0 THEN
                       SET LS-Visible(LS-i, LS-j) TO 1
                   END-IF

                   MULTIPLY LS-View-tmp BY LS-View-Distance

                   *> Check POS -> UP
                   SET LS-INVISIBLE TO 0
                   SET LS-View-tmp TO 0
                   SET LS-View-stp TO 0
                   SUBTRACT 1 FROM LS-i GIVING LS-FROM
                   PERFORM VARYING LS-k FROM LS-FROM BY -1
                           UNTIL LS-k < 1
                       IF LS-View-stp = 0 THEN
                           ADD 1 TO LS-View-tmp
                       END-IF
                       IF LS-Height(LS-k, LS-j) >= LS-Height(LS-i, LS-j)
                               THEN
                           SET LS-INVISIBLE TO 1
                           SET LS-View-stp TO 1
                       END-IF
                   END-PERFORM
                   IF LS-INVISIBLE = 0 THEN
                       SET LS-Visible(LS-i, LS-j) TO 1
                   END-IF

                   MULTIPLY LS-View-tmp BY LS-View-Distance

                   *> Check POS -> RIGHT
                   SET LS-INVISIBLE TO 0
                   SET LS-View-tmp TO 0
                   SET LS-View-stp TO 0
                   ADD 1 TO LS-j GIVING LS-FROM
                   PERFORM VARYING LS-k FROM LS-FROM
                           UNTIL LS-k > LS-Cols-CNT(LS-i)
                       IF LS-View-stp = 0 THEN
                           ADD 1 TO LS-View-tmp
                       END-IF
                       IF LS-Height(LS-i, LS-k) >= LS-Height(LS-i, LS-j)
                               THEN
                           SET LS-INVISIBLE TO 1
                           SET LS-View-stp TO 1
                       END-IF
                   END-PERFORM
                   IF LS-INVISIBLE = 0 THEN
                       SET LS-Visible(LS-i, LS-j) TO 1
                   END-IF

                   MULTIPLY LS-View-tmp BY LS-View-Distance

                   *> Check POS -> LEFT
                   SET LS-INVISIBLE TO 0
                   SET LS-View-tmp TO 0
                   SET LS-View-stp TO 0
                   SUBTRACT 1 FROM LS-j GIVING LS-FROM
                   PERFORM VARYING LS-k FROM LS-FROM BY -1
                           UNTIL LS-k < 1
                       IF LS-View-stp = 0 THEN
                           ADD 1 TO LS-View-tmp
                       END-IF
                       IF LS-Height(LS-i, LS-k) >= LS-Height(LS-i, LS-j)
                               THEN
                           SET LS-INVISIBLE TO 1
                           SET LS-View-stp TO 1
                       END-IF
                   END-PERFORM
                   IF LS-INVISIBLE = 0 THEN
                       SET LS-Visible(LS-i, LS-j) TO 1
                   END-IF

                   MULTIPLY LS-View-tmp BY LS-View-Distance
                   MOVE LS-View-Distance TO LS-View(LS-i, LS-j)
               END-PERFORM
           END-PERFORM.


           SET LS-View-Distance TO 0.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Rows-CNT
               PERFORM VARYING LS-j FROM 1
                       UNTIL LS-j > LS-Cols-CNT(LS-i)
                   ADD LS-Visible(LS-i, LS-j) TO LS-Visible-Trees-Sum
                   IF LS-View(LS-i, LS-j) > LS-View-Distance THEN
                       MOVE LS-View(LS-i, LS-j) TO LS-View-Distance
                   END-IF
               END-PERFORM
           END-PERFORM.

           MOVE LS-Visible-Trees-Sum TO L-Result-1.
           MOVE LS-View-Distance TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM treetop.
