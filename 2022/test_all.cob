       IDENTIFICATION DIVISION.
       PROGRAM-ID. test_all.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-test-FILE
           ASSIGN TO "tests.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD F-test-FILE.
       01 F-FileLine PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF-BOOL PIC 9(1) VALUE 0.

       01 WS-Test OCCURS 1 TO 50 TIMES DEPENDING ON LS-Test-CNT.
         02 WS-DirName PIC X(2).
         02 WS-FileName PIC X(60).
         02 WS-Test-File PIC X(40) VALUE IS "testinput".
         02 WS-Expected-Result-1 PIC X(15) VALUE IS SPACE.
         02 WS-Expected-Result-2 PIC X(15) VALUE IS SPACE.
         02 WS-Result-1 PIC X(15) VALUE IS SPACE.
         02 WS-Result-2 PIC X(15) VALUE IS SPACE.
         02 WS-Skipped PIC 9(2) VALUE IS ZERO.
         02 WS-Success PIC 9(2) VALUE IS ZERO.
         02 WS-Failure PIC 9(2) VALUE IS ZERO.

       77 WS-Line-Len PIC 9(3).

       01  RESPONSES.
           05  RESPONSE-IN-WS  PIC X        VALUE " ".

       LOCAL-STORAGE SECTION.
       01 LS-Full-FileName PIC X(40).
       01 LS-Full-TestName PIC X(40).

       01 LS-Argument PIC X(80) VALUE IS SPACES.
       01 LS-j PIC 9(2) VALUE IS ZERO.
       01 LS-ARGS-CNT PIC 9 VALUE IS ZERO.
       01 LS-ALL-ARGS.
          02 LS-ARGS PIC X(15) OCCURS 1 TO 9 TIMES
                                        DEPENDING ON LS-ARGS-CNT.

       01 LS-i PIC 9(2) VALUE IS ZERO.
       01 LS-Test-CNT PIC 9(2) VALUE IS ZERO.

       01 LS-garbage-CNT PIC 9(2) VALUE IS ZERO.
       01 LS-Result-1 PIC X(15) VALUE IS SPACE.
       01 LS-Result-2 PIC X(15) VALUE IS SPACE.

       01 LS-DirName-IN PIC X(2) VALUE IS SPACE.
       01 LS-FileName-IN PIC X(60) VALUE IS SPACE.
       01 LS-TestFile-IN PIC X(40) VALUE IS SPACE.
       01 LS-Skipped-IN PIC 9(2) VALUE IS ZERO.
       01 LS-Success-IN PIC 9(2) VALUE IS ZERO.
       01 LS-Failure-IN PIC 9(2) VALUE IS ZERO.
       01 LS-Expected-Result-1-IN PIC X(15) VALUE IS SPACE.
       01 LS-Expected-Result-2-IN PIC X(15) VALUE IS SPACE.
       01 LS-Result-1-IN PIC X(15) VALUE IS SPACE.
       01 LS-Result-2-IN PIC X(15) VALUE IS SPACE.

       01 LS-SET-ARGS.
         02 LS-DisplayScreen PIC X(1) VALUE IS ZERO.

       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  SUMMARY-ID-SECTION.
               10  VALUE "TEST RESULTS SCREEN"  BLANK SCREEN
                                              LINE 01 COL 30.
           05  FAILURE-ID-SECTION.
               10  VALUE "TEST RESULTS SCREEN"  BLANK SCREEN
                                              LINE 01 COL 30.
               10  VALUE "ID"                 LINE 05 COL 05.
               10  VALUE "NAME"                       COL 10.
               10  VALUE "INPUT"                      COL 35.

           05  RESULT-SECTION.
               10  VALUE "Results:"           LINE 04 COL 05.
               10  VALUE "ID"                 LINE 05 COL 05.
               10  VALUE "NAME"                       COL 10.
               10  VALUE "INPUT"                      COL 35.
               10  VALUE "Success"                    COL 55.
               10  VALUE "Failure"                    COL 65.
               10  VALUE "Skipped"                    COL 75.
           05  RESULT-TEST-SECTION         LINE PLUS LS-i.
               12  ID-ON-SCR-IN                       COL 05
                       PIC X(2)         FROM LS-DirName-IN AUTO.
               12  NAME-ON-SCR-IN                     COL 10
                       PIC X(60)        FROM LS-FileName-IN AUTO.
               12  TEST-ON-SCR-IN                     COL 35
                       PIC X(40)        FROM LS-TestFile-IN AUTO.
               12  SUCCESS-ON-SCR-IN                  COL 55
                       FOREGROUND-COLOR IS 2
                       PIC 9(2)         FROM LS-Success-IN.
               12  FAILURE-ON-SCR-IN                  COL 65
                       FOREGROUND-COLOR IS 4
                       PIC 9(2)         FROM LS-Failure-IN.
               12  SKIPPED-ON-SCR-IN                  COL 75
                       FOREGROUND-COLOR IS 3
                       PIC 9(2)         FROM LS-Skipped-IN.
           05  EXPECTED-SECTION.
               10  VALUE "TEST 1"         LINE PLUS 1 COL 05.
               10  VALUE "Expected:"      LINE PLUS 1 COL 07.
               10  EXPECTED-ON-SCR-IN                 COL 17
                       FOREGROUND-COLOR IS 1
                       PIC X(15)
                       USING LS-Expected-Result-1-IN.
               10  VALUE "Actual:"        LINE PLUS 1 COL 07.
               10  ACTUAL-ON-SCR-IN                   COL 17
                       FOREGROUND-COLOR IS 3
                       PIC X(15)
                       FROM LS-Result-1-IN.
               10  VALUE "TEST 2"         LINE PLUS 1 COL 05.
               10  VALUE "Expected:"      LINE PLUS 1 COL 07.
               10  EXPECTED-ON-SCR-IN                 COL 17
                       FOREGROUND-COLOR IS 1
                       PIC X(15)
                       USING LS-Expected-Result-2-IN.
               10  VALUE "Actual:"        LINE PLUS 1 COL 07.
               10  ACTUAL-ON-SCR-IN                   COL 17
                       FOREGROUND-COLOR IS 3
                       PIC 9(10)
                       FROM LS-Result-2-IN.

           05  RESPONSE-SECTION.
               10  VALUE "C - TO CONTINUE"    LINE 26 COL 30.
               10  VALUE "Q - TO QUIT"        LINE 27 COL 30.
               10  VALUE "ENTER CHOICE:"      LINE 29 COL 30.
               10  RESPONSE-SCR               LINE 29 COL 45
                       PIC X     TO RESPONSE-IN-WS.

       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           PERFORM PARSE-ARGS-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL WS-EOF-BOOL = 1.
           PERFORM CLOSE-FILE-ROUTINE.
           PERFORM DISPLAY-RESULTS-ROUTINE.
           STOP RUN.
       END-ROUTINE.

       PARSE-ARGS-ROUTINE.
           ACCEPT LS-Argument FROM COMMAND-LINE.
           UNSTRING LS-Argument
               DELIMITED BY ALL SPACES
               INTO LS-ALL-ARGS COUNT IN LS-ARGS-CNT.
           PERFORM VARYING LS-j FROM 0
                   UNTIL LS-j >= LS-ARGS-CNT
               EVALUATE LS-ARGS(LS-j)
                   WHEN "--screen"
                       DISPLAY "SET SCREEN"
                   WHEN OTHER
                       DISPLAY "DROPPING ARG >", LS-Argument, "<"
                       DISPLAY ">", LS-ALL-ARGS
               END-EVALUATE
           END-PERFORM.
       END-ROUTINE.

       DISPLAY-RESULTS-ROUTINE.
           IF LS-DisplayScreen = 1 THEN
               PERFORM DISPLAY-SCREEN-ROUTINE-LOOP
           ELSE
               PERFORM DISPLAY-TERM-ROUTINE
           END-IF.
       END-ROUTINE.

       DISPLAY-TERM-ROUTINE.
           DISPLAY "Results:".

           DISPLAY
                "ID NAME                                             " &
                "           INPUT " &
                "                        Success Failure Skipped".

           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Test-CNT
               MOVE WS-DirName(LS-i) TO LS-DirName-IN
               MOVE WS-FileName(LS-i) TO LS-FileName-IN
               MOVE WS-Test-File(LS-i) TO LS-TestFile-IN
               MOVE WS-Success(LS-i) TO LS-Success-IN
               MOVE WS-Failure(LS-i) TO LS-Failure-IN
               MOVE WS-Skipped(LS-i) TO LS-Skipped-IN
               DISPLAY WS-DirName(LS-i), " ",
                       WS-FileName(LS-i),
                       WS-Test-File(LS-i),
                       WS-Success(LS-i), "   ",
                       WS-Failure(LS-i), "   ",
                       WS-Skipped(LS-i)

           END-PERFORM.
       END-ROUTINE.

       DISPLAY-SCREEN-ROUTINE-LOOP.
           PERFORM DISPLAY-SUMMARY-SCREEN-ROUTINE
               UNTIL RESPONSE-IN-WS = "Q" OR
                     RESPONSE-IN-WS = "C".
           IF RESPONSE-IN-WS = "Q" THEN
               STOP RUN
           ELSE IF RESPONSE-IN-WS = "C"
               PERFORM DISPLAY-TC-FAILURE-SCREEN-ROUTINE
                   UNTIL RESPONSE-IN-WS = "Q"
           END-IF.
       END-ROUTINE.

       DISPLAY-SUMMARY-SCREEN-ROUTINE.
           DISPLAY SUMMARY-ID-SECTION.
           DISPLAY RESULT-SECTION.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Test-CNT
                   MOVE WS-DirName(LS-i) TO LS-DirName-IN
                   MOVE WS-FileName(LS-i) TO LS-FileName-IN
                   MOVE WS-Test-File(LS-i) TO LS-TestFile-IN
                   MOVE WS-Success(LS-i) TO LS-Success-IN
                   MOVE WS-Failure(LS-i) TO LS-Failure-IN
                   MOVE WS-Skipped(LS-i) TO LS-Skipped-IN
                   DISPLAY RESULT-TEST-SECTION
           END-PERFORM.
           DISPLAY RESPONSE-SECTION.
           ACCEPT RESPONSE-SCR.
       END-ROUTINE.

       DISPLAY-TC-FAILURE-SCREEN-ROUTINE.
           DISPLAY FAILURE-ID-SECTION.
           DISPLAY RESULT-SECTION.
           PERFORM VARYING LS-i FROM 1
                   UNTIL LS-i > LS-Test-CNT
               IF WS-Failure(LS-i) > 0 THEN
                   MOVE WS-DirName(LS-i) TO LS-DirName-IN
                   MOVE WS-FileName(LS-i) TO LS-FileName-IN
                   MOVE WS-Test-File(LS-i) TO LS-TestFile-IN
                   MOVE WS-Success(LS-i) TO LS-Success-IN
                   MOVE WS-Failure(LS-i) TO LS-Failure-IN
                   MOVE WS-Skipped(LS-i) TO LS-Skipped-IN
                   DISPLAY EXPECTED-SECTION
               END-IF
           END-PERFORM.
           DISPLAY RESPONSE-SECTION.
           ACCEPT RESPONSE-SCR.
       END-ROUTINE.

       OPEN-FILE-ROUTINE.
           OPEN INPUT F-test-FILE.
       END-ROUTINE.

       READ-LINE-ROUTINE.
           READ F-test-FILE RECORD
               AT END SET WS-EOF-BOOL TO 1
               NOT AT END PERFORM DO-LINE-ROUTINE.
       END-ROUTINE.

       DO-LINE-ROUTINE.
           ADD 1 TO LS-Test-CNT.

           UNSTRING F-FileLine
               DELIMITED BY ALL SPACES
               INTO WS-DirName(LS-Test-CNT),
                    WS-FileName(LS-Test-CNT),
                    WS-Test-File(LS-Test-CNT),
                    WS-Expected-Result-1(LS-Test-CNT),
                    WS-Expected-Result-2(LS-Test-CNT).

           INITIALIZE LS-Full-FileName,
                      LS-Full-TestName,
                      LS-Result-1,
                      LS-Result-2.

           STRING WS-DirName(LS-Test-CNT) DELIMITED BY SPACE,
                  '/' DELIMITED BY SIZE,
                  WS-FileName(LS-Test-CNT) DELIMITED BY SPACE
                  INTO LS-Full-FileName.
           STRING WS-DirName(LS-Test-CNT) DELIMITED BY SPACE,
                  '/' DELIMITED BY SIZE,
                  WS-Test-File(LS-Test-CNT) DELIMITED BY SPACE
                  INTO LS-Full-TestName.

           CALL LS-Full-FileName
               USING BY REFERENCE LS-Full-TestName,
                                  LS-Result-1,
                                  LS-Result-2.

           MOVE 0 TO LS-garbage-CNT
           INSPECT LS-Result-1
               TALLYING LS-garbage-CNT FOR LEADING ZERO
           MOVE LS-Result-1(LS-garbage-CNT + 1 :)
                TO WS-Result-1(LS-Test-CNT)

           MOVE 0 TO LS-garbage-CNT
           INSPECT LS-Result-2
               TALLYING LS-garbage-CNT FOR LEADING ZERO
           MOVE LS-Result-2(LS-garbage-CNT + 1 :)
                TO WS-Result-2(LS-Test-CNT)

           MOVE " " TO RESPONSE-IN-WS.

           *> SET WS-Skipped(LS-Test-CNT) TO ZERO.
           *> SET WS-Success TO ZERO.
           *> SET WS-Failure TO ZERO.
           IF WS-Expected-Result-1(LS-Test-CNT) = " " THEN
               ADD 1 TO WS-Skipped(LS-Test-CNT)
           ELSE IF WS-Expected-Result-1(LS-Test-CNT) =
                   WS-Result-1(LS-Test-CNT) THEN
               ADD 1 TO WS-Success(LS-Test-CNT)
           ELSE
               DISPLAY "EXP: ", WS-Expected-Result-1(LS-Test-CNT)
               DISPLAY "ACT: ", WS-Result-1(LS-Test-CNT)
               ADD 1 TO WS-Failure(LS-Test-CNT)
           END-IF.
           IF WS-Expected-Result-2(LS-Test-CNT) = " " THEN
               ADD 1 TO WS-Skipped(LS-Test-CNT)
           ELSE IF WS-Expected-Result-2(LS-Test-CNT) =
                   WS-Result-2(LS-Test-CNT) THEN
               ADD 1 TO WS-Success(LS-Test-CNT)
           ELSE
               ADD 1 TO WS-Failure(LS-Test-CNT)
           END-IF.
       END-ROUTINE.


       CLOSE-FILE-ROUTINE.
           CLOSE F-test-FILE.
       END-ROUTINE.

       END PROGRAM test_all.
