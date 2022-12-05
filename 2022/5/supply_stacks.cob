       IDENTIFICATION DIVISION.
       PROGRAM-ID. supply_stacks.
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

       01 LS-Stk-TBL OCCURS 15 TIMES.
         02 LS-Col-REC OCCURS 15 TIMES.
           03 FILLER PIC X VALUE "[".
           03 LS-CHR PIC X.
           03 FILLER PIC X VALUE "]".
           03 FILLER PIC Z.

       01 LS-Col-CNT BINARY-CHAR.
       01 LS-Stack-CNT PIC 9(2) VALUE 0.

       01 LS-ReadInst-BOOL PIC 1 VALUE 0.

       01 LS-Instr-CNT PIC 9(3) VALUE 0.
       01 LS-FILLER PIC Z(1).
       01 LS-move-plus-1-NUM PIC 9(2).
       01 LS-Instr-TBL OCCURS 1 TO 999 TIMES DEPENDING ON LS-Instr-CNT.
         02 LS-move-NUM PIC 9(2).
         02 LS-from-NUM PIC 9(2).
         02 LS-to-NUM PIC 9(2).

       01 LS-Temp1-STR PIC X(80) VALUE IS SPACE.
       01 LS-Temp2-STR PIC X(80) VALUE IS SPACE.
       01 LS-COUNT PIC 99 VALUE 1.
       01 LS-i PIC 9(3) VALUE 0.
       01 LS-j PIC 9(3) VALUE 0.

       01 LS-Col-main-TBL.
         02 LS-Col-TBL OCCURS 15 TIMES.
           03 LS-Column-CNT PIC 9(2) VALUE IS 1.
           03 LS-Col-STR PIC X(80) VALUE IS SPACE.

       01 LS-Col-init-TBL.
         02 LS-INIT-Col-TBL OCCURS 15 TIMES.
           03 LS-INIT-Column-CNT PIC 9(2) VALUE IS 1.
           03 LS-INIT-Col-STR PIC X(80) VALUE IS SPACE.

       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(10) VALUE IS SPACE.
       01 L-Result-2 PIC X(10) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.

       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM CALCULATE-RESULT-ROUTINE.
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
               NOT AT END
                   IF LS-ReadInst-BOOL = 0 THEN
                       PERFORM DO-STACK-LINES-ROUTINE
                   ELSE
                       PERFORM DO-INSTR-LINES-ROUTINE
                   END-IF
           END-READ.
       END-ROUTINE.

       DO-STACK-LINES-ROUTINE.
           IF FileLine = " " THEN
               SET LS-ReadInst-BOOL TO 1
               PERFORM VARYING LS-i FROM 1 UNTIL LS-i > LS-Col-CNT
                   PERFORM VARYING LS-j FROM 1 UNTIL LS-j > LS-Stack-CNT
                       IF NOT LS-CHR(LS-j, LS-i) = '-' THEN
                           STRING LS-CHR(LS-j, LS-i)
                               INTO LS-Col-STR(LS-i)
                               WITH POINTER LS-Column-CNT(LS-i)
                       END-IF
                   END-PERFORM
               END-PERFORM
           ELSE
               INITIALIZE LS-Col-CNT
               INSPECT FileLine
                   TALLYING LS-Col-CNT
                   FOR ALL "["
               IF LS-Col-CNT = 0 THEN
                   INSPECT FileLine
                       TALLYING LS-Col-CNT
                       FOR ALL SPACES
                   SUBTRACT LS-Col-CNT FROM 80 GIVING LS-Col-CNT
               ELSE
                   ADD 1 TO LS-Stack-CNT
                   INSPECT FileLine
                      REPLACING LEADING "    " BY "[-] "
                                ALL     "    " BY " [-]"
                   MOVE FileLine TO LS-Stk-TBL(LS-Stack-CNT)
               END-IF
           END-IF.
       END-ROUTINE.

       DO-INSTR-LINES-ROUTINE.
           ADD 1 TO LS-Instr-CNT.
           UNSTRING FileLine
               DELIMITED BY ALL "move"
                         OR "from"
                         OR "to"
               INTO LS-FILLER,
                    LS-move-NUM(LS-Instr-CNT),
                    LS-from-NUM(LS-Instr-CNT),
                    LS-to-NUM(LS-Instr-CNT).
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       CALCULATE-RESULT-ROUTINE.
           MOVE LS-Col-main-TBL TO LS-Col-init-TBL.
           PERFORM CALCULATE-RESULT-1-ROUTINE
               VARYING LS-i FROM 1 UNTIL LS-i > LS-Instr-CNT.
           PERFORM MOVE-RESULT-ROUTINE.
           MOVE LS-Temp1-STR TO L-Result-1.
           MOVE LS-Col-init-TBL TO LS-Col-main-TBL.
           PERFORM CALCULATE-RESULT-2-ROUTINE
               VARYING LS-i FROM 1 UNTIL LS-i > LS-Instr-CNT.
           PERFORM MOVE-RESULT-ROUTINE.
           MOVE LS-Temp1-STR TO L-Result-2.
       END-ROUTINE.

       CALCULATE-RESULT-1-ROUTINE.
           MOVE FUNCTION
               REVERSE(LS-Col-STR(
               LS-from-NUM(LS-i))(1:LS-move-NUM(LS-i)))
               TO LS-Temp1-STR.

           PERFORM MOVE-CRATE-ROUTINE.
       END-ROUTINE.

       CALCULATE-RESULT-2-ROUTINE.
           MOVE LS-Col-STR(
               LS-from-NUM(LS-i))(1:LS-move-NUM(LS-i))
               TO LS-Temp1-STR.

           PERFORM MOVE-CRATE-ROUTINE.
       END-ROUTINE.

       MOVE-CRATE-ROUTINE.
           SET LS-COUNT TO 1.

           STRING LS-Temp1-STR(1:LS-move-NUM(LS-i))
               LS-Col-STR(LS-to-NUM(LS-i))
               INTO LS-Temp2-STR
               WITH POINTER LS-COUNT.
           MOVE LS-Temp2-STR TO LS-Col-STR(LS-to-NUM(LS-i)).

           ADD 1 TO LS-move-NUM(LS-i) GIVING LS-move-plus-1-NUM.
           MOVE LS-Col-STR(LS-from-NUM(LS-i))(LS-move-plus-1-NUM:)
               TO LS-Col-STR(LS-from-NUM(LS-i)).
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           PERFORM VARYING LS-i FROM 1 UNTIL LS-i > LS-Col-CNT
               MOVE LS-Col-STR(LS-i)(1:1) TO LS-Temp1-STR(LS-i:1)
           END-PERFORM.
       END-ROUTINE.

       END PROGRAM supply_stacks.
