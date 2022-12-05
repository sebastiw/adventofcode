       IDENTIFICATION DIVISION.
       PROGRAM-ID. rock_paper_scissor.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 FileLine.
          02 OpponentChoice PIC A(1).
          02 FILLER PIC Z(1).
          02 MyChoice PIC A(1).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(20).

       LOCAL-STORAGE SECTION.
       01 LS-Points1-TOTAL PIC 9(7) VALUE 0.
       01 LS-Points2-TOTAL PIC 9(7) VALUE 0.

       01 LS-EOF-BOOL PIC 9(1) VALUE 0.

      *    Points table part 1
      *    AB  X  Y  Z <- your choice
      *    A   4  8  3
      *    B   1  5  9
      *    C   7  2  6
      *    ^ opponents choice
      *
      *    A/X = Rock
      *    B/Y = Paper
      *    C/Z = Scissor
       01 LS-points1-VALUES.
         02 PIC 9(3) VALUE "483".
         02 PIC 9(3) VALUE "159".
         02 PIC 9(3) VALUE "726".

       01 LS-points1-TABLE REDEFINES LS-points1-VALUES.
         02 LS-points1-ROWS OCCURS 3 TIMES INDEXED BY I.
           03 LS-points1-COLS OCCURS 3 TIMES INDEXED BY J.
             04 LS-points1 PIC 9(1).

      *    Points table part 2
      *    AB  X  Y  Z <- your choice
      *    A   3  4  8
      *    B   1  5  9
      *    C   2  6  7
      *    ^ opponents choice
      *
      *    A = Rock
      *    B = Paper
      *    C = Scissor
      *    X = Lose
      *    Y = Draw
      *    Z = Win
       01 LS-points2-VALUES.
         02 PIC 9(3) VALUE "348".
         02 PIC 9(3) VALUE "159".
         02 PIC 9(3) VALUE "267".

       01 LS-points2-TABLE REDEFINES LS-points2-VALUES.
         02 LS-points2-ROWS OCCURS 3 TIMES INDEXED BY I.
           03 LS-points2-COLS OCCURS 3 TIMES INDEXED BY J.
             04 LS-points2 PIC 9(1).

       01 LS-A-Choice PIC 9(1).
       01 LS-B-Choice PIC 9(1).

       LINKAGE SECTION.
       01 L-Filename PIC X(40).
       01 L-Result-1 PIC X(10).
       01 L-Result-2 PIC X(10).

       PROCEDURE DIVISION USING L-Filename, L-Result-1, L-Result-2.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL LS-EOF-BOOL = 1.
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
               AT END SET LS-EOF-BOOL TO 1
               NOT AT END PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           PERFORM SET-OPPONENT-ROUTINE.
           PERFORM SET-CONTESTANT-ROUTINE.
           ADD LS-points1(LS-A-Choice, LS-B-Choice)
               TO LS-Points1-TOTAL.
           ADD LS-points2(LS-A-Choice, LS-B-Choice)
               TO LS-Points2-TOTAL.
       END-ROUTINE.

       SET-OPPONENT-ROUTINE.
           EVALUATE OpponentChoice
               WHEN "A"
                   SET LS-A-Choice TO 1
               WHEN "B"
                   SET LS-A-Choice TO 2
               WHEN "C"
                   SET LS-A-Choice TO 3
           END-EVALUATE.
       END-ROUTINE.

       SET-CONTESTANT-ROUTINE.
           EVALUATE MyChoice
               WHEN "X"
                   SET LS-B-Choice TO 1
               WHEN "Y"
                   SET LS-B-Choice TO 2
               WHEN "Z"
                   SET LS-B-Choice TO 3
           END-EVALUATE.
       END-ROUTINE.

       MOVE-RESULT-ROUTINE.
           MOVE LS-Points1-TOTAL TO L-Result-1.
           MOVE LS-Points2-TOTAL TO L-Result-2.
       END-ROUTINE.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-ROUTINE.

       END PROGRAM rock_paper_scissor.
