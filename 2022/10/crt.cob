       IDENTIFICATION DIVISION.
       PROGRAM-ID. crt.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 F-Inst-TBL.
         05 F-Instruction PIC X(4).
         05 FILLER PIC Z.
         05 F-IncWith PIC X(3).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.
       01 LS-EOF PIC 9(1) VALUE 0.

       01 LS-i PIC 9.
       01 LS-IncWith PIC S9(3) VALUE 0.
       01 LS-Cycle PIC 9(3) VALUE 0.
       01 LS-XValue PIC S9(3) VALUE 1.
       01 LS-Signal PIC 9(10) VALUE 0.
       01 LS-Signal-SUM PIC 9(10) VALUE 0.
       01 LS-Sprite-Low PIC S9(3) VALUE 0.
       01 LS-Sprite-High PIC S9(3) VALUE 0.
       01 LS-Offset PIC 9(3) VALUE 0.

       01 LS-Dark OCCURS 240 TIMES PIC 1 VALUE 0.

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
           READ F-input-FILE RECORD INTO F-Inst-TBL
               AT END
                   SET LS-EOF TO 1
               NOT AT END
                   PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           UNSTRING
               F-IncWith DELIMITED BY SPACES
               INTO LS-IncWith.
           IF F-Instruction = "addx" THEN
               PERFORM CYCLE-ROUTINE
               PERFORM CYCLE-ROUTINE
               ADD LS-IncWith TO LS-XValue
           ELSE IF F-Instruction = "noop" THEN
               PERFORM CYCLE-ROUTINE
                    VARYING LS-i FROM 1 UNTIL LS-i > 1
           ELSE
               DISPLAY "ERROR"
           END-IF.
       END-ROUTINE.

       CYCLE-ROUTINE.
           SUBTRACT 1 FROM LS-XValue GIVING LS-Sprite-Low.
           ADD      1   TO LS-XValue GIVING LS-Sprite-High.
           ADD LS-Offset TO LS-Sprite-Low.
           ADD LS-Offset TO LS-Sprite-High.
           IF LS-Cycle >= LS-Sprite-Low AND
              LS-Cycle <= LS-Sprite-High THEN
               SET LS-Dark(LS-Cycle) TO 1
           END-IF.
           IF
                   LS-Cycle = 40 OR
                   LS-Cycle = 80 OR
                   LS-Cycle = 120 OR
                   LS-Cycle = 160 OR
                   LS-Cycle = 200 OR
                   LS-Cycle = 240 THEN
               SET LS-Offset TO LS-Cycle
           END-IF.

           ADD 1 TO LS-Cycle.
           IF
                   LS-Cycle = 20 OR
                   LS-Cycle = 60 OR
                   LS-Cycle = 100 OR
                   LS-Cycle = 140 OR
                   LS-Cycle = 180 OR
                   LS-Cycle = 220 THEN
               MULTIPLY LS-Cycle BY LS-XValue GIVING LS-Signal
               ADD LS-Signal TO LS-Signal-SUM
           END-IF.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-Signal-SUM TO L-Result-1.

           PERFORM VARYING LS-Cycle FROM 1 UNTIL LS-Cycle > 240
               IF LS-Dark(LS-Cycle) = 1
                   DISPLAY "#" NO ADVANCING
               ELSE
                   DISPLAY " " NO ADVANCING
               END-IF
               IF
                       LS-Cycle = 40 OR
                       LS-Cycle = 80 OR
                       LS-Cycle = 120 OR
                       LS-Cycle = 160 OR
                       LS-Cycle = 200 OR
                       LS-Cycle = 240 THEN
                   DISPLAY " "
               END-IF
           END-PERFORM.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM crt.
