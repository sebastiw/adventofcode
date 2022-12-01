       IDENTIFICATION DIVISION.
       PROGRAM-ID. calorie_counting.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-input-FILE
           ASSIGN TO "input"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-input-FILE.
       01 FileLine PIC 9(10).

       WORKING-STORAGE SECTION.
       01 WS-Elf-Sums-SORT
          OCCURS 2256 TIMES
          VALUES ARE ZEROES.
          02 WS-Elf-Sums PIC 9(6).

       01 WS-Elf-TOTAL PIC 9(7) VALUE 0.

       01 WS-EOF.
         02 WS-EOF-BOOL PIC 9(1) VALUE 0.

       LOCAL-STORAGE SECTION.
       01 LS-Cal PIC 9(6).

       01 LS-Elf-IDX BINARY-LONG VALUE 1.

       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL WS-EOF-BOOL = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM DISPLAY-RESULT-ROUTINE.
           STOP RUN.

       OPEN-FILE-ROUTINE.
           OPEN INPUT F-input-FILE.
       END-METHOD.

       READ-LINE-ROUTINE.
           READ F-input-FILE RECORD INTO LS-Cal
               AT END SET WS-EOF-BOOL TO 1.
           ADD LS-Cal to WS-Elf-Sums(LS-Elf-IDX)
           IF LS-Cal = 0
               ADD 1 to LS-Elf-IDX
           END-IF.
       END-METHOD.

       DISPLAY-RESULT-ROUTINE.
           DISPLAY "Number of Elfs: ", LS-Elf-IDX.
           SORT WS-Elf-Sums-SORT ON DESCENDING KEY WS-Elf-Sums.
           DISPLAY "Top 3 Elfs carrying:"
           DISPLAY "1: ", WS-Elf-Sums(1).
           DISPLAY "2: ", WS-Elf-Sums(2).
           DISPLAY "3: ", WS-Elf-Sums(3).
           ADD WS-Elf-Sums(1) WS-Elf-Sums(2)
               WS-Elf-Sums(3) TO WS-Elf-TOTAL.
           DISPLAY "Amount: ", WS-Elf-TOTAL.
       END-METHOD.

       CLOSE-FILE-ROUTINE.
           CLOSE F-input-FILE.
       END-METHOD.

       END PROGRAM calorie_counting.
