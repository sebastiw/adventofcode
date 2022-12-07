       IDENTIFICATION DIVISION.
       PROGRAM-ID. tuning_trouble.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 FileLine PIC X(5000).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(40) VALUE IS "testinput".

       LOCAL-STORAGE SECTION.

       01 LS-EOF PIC 9(1) VALUE 0.
       01 LS-EOL PIC 9(1) VALUE 0.

       01 LS-COMPARE-LEN CONSTANT 14.

       01 LS-line-CNT PIC 9(4) VALUE 1.
       01 LS-line-PNT PIC 9(4) VALUE 1.
       01 LS-buffer-PNT PIC 9(4) VALUE 1.
       01 LS-buffer-offset PIC 9(4) VALUE 1.
       01 LS-buffer-STR PIC X(5000).
       01 LS-duplicates-BOOL PIC 1 VALUE 0.
       01 LS-duplicate-CNT PIC 99 VALUE 2.
       01 LS-i PIC 99 VALUE 0.


       LINKAGE SECTION.
       01 L-Filename PIC X(40) VALUE IS "testinput".
       01 L-Result-1 PIC X(10) VALUE IS SPACE.
       01 L-Result-2 PIC X(10) VALUE IS SPACE.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF = 1 OR LS-EOL = 1.
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
           ADD 1 TO LS-line-CNT
           PERFORM VARYING LS-line-PNT FROM 1
                   UNTIL LS-EOL = 1
               IF FileLine(LS-line-PNT:1) = SPACE OR
                  FileLine(LS-line-PNT:1) = LOW-VALUE THEN
                   SET LS-EOL TO 1
                   NEXT SENTENCE
               END-IF
               MOVE FileLine(LS-line-PNT:1)
                   TO LS-buffer-STR(LS-buffer-PNT:1)
               PERFORM CHECK-DUPLICATES-ROUTINE
               ADD 1 TO LS-buffer-PNT
               IF LS-duplicates-BOOL = 0 THEN
                   SUBTRACT 1 FROM LS-line-PNT
                   NEXT SENTENCE
               END-IF
           END-PERFORM.
       END-ROUTINE.

       CHECK-DUPLICATES-ROUTINE.
           SET LS-duplicates-BOOL TO 0.
           IF LS-buffer-PNT > LS-COMPARE-LEN THEN
               SUBTRACT LS-COMPARE-LEN FROM LS-buffer-PNT
                   GIVING LS-buffer-offset
               PERFORM VARYING LS-i FROM 0 UNTIL LS-i >= LS-COMPARE-LEN
                   SET LS-duplicate-CNT TO 0
                   INSPECT
                       LS-buffer-STR(LS-buffer-offset:LS-COMPARE-LEN)
                       TALLYING LS-duplicate-CNT
                       FOR ALL LS-buffer-STR(LS-buffer-offset+LS-i:1)
                   IF LS-duplicate-CNT > 1 THEN
                       SET LS-duplicates-BOOL to 1
                       NEXT SENTENCE
                   END-IF
               END-PERFORM
           ELSE
               SET LS-duplicates-BOOL TO 1
           END-IF.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-line-PNT TO L-Result-1.
           MOVE LS-line-PNT TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM tuning_trouble.
