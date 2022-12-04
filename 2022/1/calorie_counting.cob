       IDENTIFICATION DIVISION.
       PROGRAM-ID. calorie_counting.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO WS-Filename
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 FileLine PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-Filename PIC X(20).

       LOCAL-STORAGE SECTION.
       01 LS-EOF-BOOL PIC 9(1) VALUE 0.

       01 LS-Cal PIC 9(6).

       01 LS-Elf-IDX BINARY-LONG VALUE 1.

       01 LS-Elf-Sums-SORT
          OCCURS 2256 TIMES
          VALUES ARE ZEROES.
          02 LS-Elf-Sums PIC 9(6).

       01 LS-Elf-TOTAL PIC 9(7) VALUE 0.

       LINKAGE SECTION.
       01 L-Filename PIC X(40).
       01 L-Result-1 PIC 9(10) VALUE IS ZERO.
       01 L-Result-2 PIC 9(10) VALUE IS ZERO.

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
       END-METHOD.

       READ-LINE-ROUTINE.
           READ F-input-FILE RECORD INTO LS-Cal
               AT END SET LS-EOF-BOOL TO 1
               NOT AT END PERFORM DO-LINE-ROUTINE.
       END-METHOD.

       DO-LINE-ROUTINE.
           ADD LS-Cal to LS-Elf-Sums(LS-Elf-IDX).
           IF LS-Cal = 0
               ADD 1 to LS-Elf-IDX
           END-IF.
       END-METHOD.

       MOVE-RESULT-ROUTINE.
           SORT LS-Elf-Sums-SORT ON DESCENDING KEY LS-Elf-Sums.
           ADD LS-Elf-Sums(1) LS-Elf-Sums(2)
               LS-Elf-Sums(3) TO LS-Elf-TOTAL.
           MOVE LS-Elf-Sums(1) TO L-Result-1.
           MOVE LS-Elf-TOTAL TO L-Result-2.
       END-METHOD.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-METHOD.

       END PROGRAM calorie_counting.
