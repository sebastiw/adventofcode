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

       01 WS-File.
         02 WS-DirName PIC X(2).
         02 WS-FileName PIC X(60).
         02 WS-Expected-Result-1 PIC 9(10) VALUE IS ZERO.
         02 WS-Expected-Result-2 PIC 9(10) VALUE IS ZERO.

       01 WS-Test-File PIC X(40) VALUE IS "testinput".
       01 WS-Result-1 PIC 9(10) VALUE IS ZERO.
       01 WS-Result-2 PIC 9(10) VALUE IS ZERO.

       77 WS-Line-Len PIC 9(3).

       01 WS-Skipped PIC 9(2) VALUE IS ZERO.
       01 WS-Success PIC 9(2) VALUE IS ZERO.
       01 WS-Failure PIC 9(2) VALUE IS ZERO.

       01  RESPONSES.
           05  RESPONSE-IN-WS  PIC X        VALUE "C".
       01  DATA-FROM-SCREEN.
           05  ID-IN-WS        PIC XXXX     VALUE SPACES.
           05  NAME-IN-WS      PIC X(20)    VALUE SPACES.

       LOCAL-STORAGE SECTION.
       01 LS-Full-FileName PIC X(40).
       01 LS-Full-TestName PIC X(40).

       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05  ID-SECTION.
               10  VALUE "TEST RESULTS SCREEN"  BLANK SCREEN
                                              LINE 01 COL 30.
               10  VALUE "ID #: "             LINE 05 COL 05.
               10  ID-ON-SCR-IN               LINE 05 COL 15
                       PIC XXXX USING WS-DirName.
           05  NAME-SECTION.
               10  VALUE "NAME:"              LINE 07 COL 05.
               10  NAME-ON-SCR-IN             LINE 07 COL 15
                       PIC X(60)        FROM WS-FileName.
           05  RESULT-SECTION.
               10  VALUE "Results:"           LINE 09 COL 05.
               10  VALUE "Success:"           LINE 10 COL 08.
               10  SUCCESS-ON-SCR-IN          LINE 10 COL 17
                       FOREGROUND-COLOR IS 2
                       PIC 9(2)
                       FROM WS-Success.
               10  VALUE "Failure:"           LINE 11 COL 08.
               10  FAILURE-ON-SCR-IN          LINE 11 COL 17
                       FOREGROUND-COLOR IS 4
                       PIC 9(2)
                       FROM WS-Failure.
               10  VALUE "Skipped:"           LINE 12 COL 08.
               10  SKIPPED-ON-SCR-IN          LINE 12 COL 17
                       FOREGROUND-COLOR IS 3
                       PIC 9(2)
                       FROM WS-Skipped.
           05  EXPECTED-SECTION.
               10  VALUE "TEST 1"             LINE 14 COL 05.
               10  VALUE "Expected:"          LINE 15 COL 07.
               10  EXPECTED-ON-SCR-IN         LINE 15 COL 17
                       FOREGROUND-COLOR IS 1
                       PIC 9(10)
                       USING WS-Expected-Result-1.
               10  VALUE "Actual:"            LINE 16 COL 07.
               10  ACTUAL-ON-SCR-IN           LINE 16 COL 17
                       FOREGROUND-COLOR IS 3
                       PIC 9(10)
                       FROM WS-Result-1.
               10  VALUE "TEST 2"             LINE 18 COL 05.
               10  VALUE "Expected:"          LINE 19 COL 07.
               10  EXPECTED-ON-SCR-IN         LINE 19 COL 17
                       FOREGROUND-COLOR IS 1
                       PIC 9(10)
                       USING WS-Expected-Result-2.
               10  VALUE "Actual:"            LINE 20 COL 07.
               10  ACTUAL-ON-SCR-IN           LINE 20 COL 17
                       FOREGROUND-COLOR IS 3
                       PIC 9(10)
                       FROM WS-Result-2.

           05  RESPONSE-SECTION.
               10  VALUE "C - TO CONTINUE"    LINE 26 COL 30.
               10  VALUE "Q - TO QUIT"        LINE 27 COL 30.
               10  VALUE "ENTER CHOICE:"      LINE 29 COL 30.
               10  RESPONSE-SCR               LINE 29 COL 45
                       PIC X     TO RESPONSE-IN-WS.

       PROCEDURE DIVISION.
       MAIN-ROUTINE.
           PERFORM OPEN-FILE-ROUTINE.
           PERFORM READ-LINE-ROUTINE UNTIL WS-EOF-BOOL = 1.
           PERFORM CLOSE-FILE-ROUTINE.

           STOP RUN.
       END-ROUTINE.

       DISPLAY-RESULTS-ROUTINE.
           DISPLAY ID-SECTION.
           DISPLAY NAME-SECTION.
           DISPLAY RESULT-SECTION.
           IF WS-Failure > 0 THEN
               DISPLAY EXPECTED-SECTION
           END-IF.
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
           INITIALIZE WS-Expected-Result-1,
                      WS-Expected-Result-2,
                      WS-Result-1,
                      WS-Result-2.

           UNSTRING F-FileLine
               DELIMITED BY ALL SPACES
               INTO WS-DirName,
                    WS-FileName,
                    WS-Test-File,
                    WS-Expected-Result-1,
                    WS-Expected-Result-2.

           INITIALIZE LS-Full-FileName.
           INITIALIZE LS-Full-TestName.

           STRING WS-DirName DELIMITED BY SPACE
                  '/' DELIMITED BY SIZE
                  WS-FileName DELIMITED BY SPACE
                  INTO LS-Full-FileName.
           STRING WS-DirName DELIMITED BY SPACE
                  '/' DELIMITED BY SIZE
                  WS-Test-File DELIMITED BY SPACE
                  INTO LS-Full-TestName.

           CALL LS-Full-FileName
               USING BY REFERENCE LS-Full-TestName,
                                  WS-Result-1,
                                  WS-Result-2.

           MOVE " " TO RESPONSE-IN-WS.

           SET WS-Skipped TO ZERO.
           SET WS-Success TO ZERO.
           SET WS-Failure TO ZERO.
           IF WS-Expected-Result-1 = 0 THEN
               ADD 1 TO WS-Skipped
           ELSE IF WS-Expected-Result-1 = WS-Result-1 THEN
               ADD 1 TO WS-Success
           ELSE
               ADD 1 TO WS-Failure
           END-IF.
           IF WS-Expected-Result-2 = 0 THEN
               ADD 1 TO WS-Skipped
           ELSE IF WS-Expected-Result-2 = WS-Result-2 THEN
               ADD 1 TO WS-Success
           ELSE
               ADD 1 TO WS-Failure
           END-IF.

           PERFORM DISPLAY-RESULTS-ROUTINE
               UNTIL RESPONSE-IN-WS = "Q" OR RESPONSE-IN-WS = "C".

           IF RESPONSE-IN-WS = "Q" THEN
               STOP RUN
           END-IF.
       END-ROUTINE.


       CLOSE-FILE-ROUTINE.
           CLOSE F-test-FILE.
       END-ROUTINE.

       END PROGRAM test_all.
