       IDENTIFICATION DIVISION.
       PROGRAM-ID. rucksack_reorg.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE
         RECORD VARYING 2 TO 80 DEPENDING ON WS-Line-Len.
       01 FileLine PIC A(80).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(20).

       77 WS-Line-Len PIC 9(3).

       LOCAL-STORAGE SECTION.
       01 LS-EOF-BOOL PIC 9(1) VALUE 0.

       01 LS-priorities PIC 9(4) VALUE IS ZERO.
       01 LS-badges PIC 9(4) VALUE IS ZERO.

       01 LS-Half-Len PIC 9(3).
       01 LS-HALF PIC A(80) OCCURS 3 TIMES.

       01 LS-ALL-CHARACTERS
          VALUE IS 'abcdefghijklmnopqrstuvwxyz' &
                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         02 LS-CHAR PIC A(1) OCCURS 52 TIMES.

       01 LS-FOUND-BOOL PIC 9(1) VALUE 0.
       01 LS-FOUND-CNT PIC 9(1) VALUE 0.

       01 LS-i PIC 9(2).
       01 LS-j PIC 9(1).
       01 LS-CURRENT-CHAR PIC A(1).
       01 LS-Char-COUNT PIC S9(3) OCCURS 3 TIMES.

       01 LS-Saved-Lines-COUNT PIC 9(1) VALUE IS ZERO.
       01 LS-Line PIC A(80) OCCURS 3 TIMES.

       LINKAGE SECTION.
       01 L-Filename PIC X(40).
       01 L-Result-1 PIC X(10) VALUE IS ZERO.
       01 L-Result-2 PIC X(10) VALUE IS ZERO.

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF-BOOL = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM MOVE-RESULT-ROUTINE.
           EXIT PROGRAM.
           STOP RUN.

       OPEN-FILE-ROUTINE.
           MOVE L-Filename TO WS-Filename.
           OPEN INPUT F-input-FILE.
       END-ROUTINE.

       READ-LINE-ROUTINE.
           READ F-input-FILE RECORD
               AT END SET LS-EOF-BOOL TO 1
               NOT AT END PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           PERFORM SPLIT-IN-HALFS-ROUTINE.
           PERFORM GET-NON-UNIQUE-CHAR-ROUTINE-1.

           PERFORM FIND-BADGE-ROUTINE.
       END-ROUTINE.

       SPLIT-IN-HALFS-ROUTINE.
           DIVIDE WS-Line-Len BY 2 GIVING LS-Half-Len.
           MOVE FileLine(1:LS-Half-Len) TO LS-Half(1).
           ADD 1 to LS-Half-Len.
           MOVE FileLine(LS-Half-Len:) TO LS-Half(2).
       END-ROUTINE.

       FIND-BADGE-ROUTINE.
           ADD 1 TO LS-Saved-Lines-COUNT
           MOVE FileLine TO LS-Line(LS-Saved-Lines-COUNT)
           IF LS-Saved-Lines-COUNT = 3 THEN
               SET LS-Saved-Lines-COUNT TO 0
               PERFORM GET-NON-UNIQUE-CHAR-ROUTINE-2
           END-IF.
       END-ROUTINE.

       GET-NON-UNIQUE-CHAR-ROUTINE-1.
           SET LS-FOUND-BOOL TO 0.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i = 53 OR LS-FOUND-BOOL = 1
               MOVE LS-CHAR(LS-i) TO LS-CURRENT-CHAR
               PERFORM TEST-NON-UNIQUE-CHAR-ROUTINE-1
           END-PERFORM.
       END-ROUTINE.

       GET-NON-UNIQUE-CHAR-ROUTINE-2.
           SET LS-FOUND-BOOL TO 0.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i = 53 OR LS-FOUND-BOOL = 1
               MOVE LS-CHAR(LS-i) TO LS-CURRENT-CHAR
               PERFORM TEST-NON-UNIQUE-CHAR-ROUTINE-2
           END-PERFORM.
       END-ROUTINE.

       TEST-NON-UNIQUE-CHAR-ROUTINE-1.
           SET LS-FOUND-CNT TO 0.
           PERFORM TEST-NON-UNIQUE-CHAR-ONE-ROUTINE-1
                   VARYING LS-j FROM 1
                   UNTIL LS-j > 2.
           IF LS-FOUND-CNT = 2 THEN
               ADD LS-i TO LS-priorities
               SET LS-FOUND-BOOL TO 1
           END-IF.
       END-ROUTINE.

       TEST-NON-UNIQUE-CHAR-ROUTINE-2.
           SET LS-FOUND-CNT TO 0.
           PERFORM TEST-NON-UNIQUE-CHAR-ONE-ROUTINE-2
                   VARYING LS-j FROM 1
                   UNTIL LS-j > 3.
           IF LS-FOUND-CNT = 3 THEN
               ADD LS-i TO LS-badges
               SET LS-FOUND-BOOL TO 1
           END-IF.
       END-ROUTINE.

       TEST-NON-UNIQUE-CHAR-ONE-ROUTINE-1.
           SET LS-Char-COUNT(LS-j) TO 0.
           INSPECT LS-Half(LS-j)
               TALLYING LS-Char-COUNT(LS-j)
               FOR ALL LS-CURRENT-CHAR.
           IF LS-Char-COUNT(LS-j) > 0 THEN
               ADD 1 to LS-FOUND-CNT
           END-IF.
       END-ROUTINE.

       TEST-NON-UNIQUE-CHAR-ONE-ROUTINE-2.
           SET LS-Char-COUNT(LS-j) TO 0.
           INSPECT LS-Line(LS-j)
               TALLYING LS-Char-COUNT(LS-j)
               FOR ALL LS-CURRENT-CHAR.
           IF LS-Char-COUNT(LS-j) > 0 THEN
               ADD 1 to LS-FOUND-CNT
           END-IF.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
          MOVE LS-priorities TO L-Result-1.
          MOVE LS-badges TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM rucksack_reorg.
