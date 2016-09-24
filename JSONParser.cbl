       IDENTIFICATION DIVISION.
       PROGRAM-ID.  JSONParser.
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      *                                                               *
      *                                                               *
      *                                                               *
      * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JSONFile ASSIGN TO "JSON.dat"
                     ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD JSONFile.
       01 JSON-REC.
          88  EndOfJSONFile  VALUE HIGH-VALUES.
          02  JSONRec       PIC x(10000).
          
       WORKING-STORAGE SECTION.
       01 WS-VARS.
            10 WS-JSON-INPUT-LEN PIC S9(05) COMP-3.
            10 WS-EXIT-OPTION     PIC X(01)
                            VALUE "7".
            10 WS-JSON-MAX    PIC S9(09) COMP.
            10 WS-AWARE     PIC X(01) VALUE SPACE.
                88 PROCESSING-OBJECT  VALUE 'A'.
                88 PROCESSING-TOKEN   VALUE 'B'.
                88 PROCESSING-KEY     VALUE 'C'.
                88 PROCESSING-VALUE   VALUE 'D'.
                88 PROCESSING-NUMERIC VALUE 'E'.
                88 PROCESSING-NONE    VALUE ' '.
            10 WS-KEY-READ       PIC X(01).
                88 READING-A-KEY      VALUE 'Y'.
                88 NOT-READING-A-KEY  VALUE 'N'.
            10 WS-KEY-FOUND     PIC X(01).
                88 KEY-FOUND    VALUE 'Y'.
                88 KEY-UNFOUND  VALUE 'N'.
            10 WS-VAL-READ       PIC X(01).
                88 READING-A-VAL      VALUE 'Y'.
                88 NOT-READING-A-VAL  VALUE 'N'.
            10 WS-VAL-FOUND     PIC X(01).
                88 VAL-FOUND    VALUE 'Y'.
                88 VAL-UNFOUND  VALUE 'N'.
            10 WS-TOKEN    PIC X(01).
                88 TOKEN-KEY    VALUE '1'.
                88 TOKEN-VAL    VALUE '2'.
            10 WS-DOUBLE-QUOTE PIC X(01) VALUE '"'.
            10 WS-COLON        PIC X(01) VALUE ':'.
            10 WS-COMMA        PIC X(01) VALUE ','.
            10 WS-OPENING-BRACES  PIC X(01) VALUE '{'.
            10 WS-CLOSING-BRACES  PIC X(01) VALUE '}'.
            10 WS-KEY   PIC X(20) VALUE SPACES.
            10 WS-VAL   PIC X(20) VALUE SPACES.
            10 WS-TEMP  PIC S9(05) COMP VALUE ZEROS.
            10 WS-TEMP-CHAR  PIC X(01).
            10 FILLER REDEFINES WS-TEMP-CHAR.
               15 WS-TEMP-NUM PIC 9(01).
            10 WS-TEMP-KEY-LEN PIC S9(05) COMP.
            10 WS-TEMP-VAL-LEN PIC S9(05) COMP.
            
       01 WS-JSON-INPUT      PIC X(10000).
       01 WS-GARBAGE          PIC X(10000).
       01 WS-JSON-STRING.
           05 WS-JSON-CHAR-ARRAY OCCURS 0 TO 10000 TIMES
                           DEPENDING ON WS-JSON-MAX
                           INDEXED BY WS-JSON-IDX.
              10 WS-JSON-CHAR PIC X(01).
       01 WS-JSON.
           05 WS-JSON-NUM-ITEMS   PIC S9(05) COMP.
           05 WS-JSON-MAX-ITEMS   PIC S9(05) COMP.
           05 WS-JSON-ARRAY OCCURS 100 TIMES
      *                    DEPENDING ON WS-JSON-MAX2
                           INDEXED BY WS-JSON-IDX2.
              10 WS-JSON-KEY-LEN  PIC S9(05) COMP.
              10 WS-JSON-KEY      PIC X(10).
              10 WS-JSON-VAL-LEN  PIC S9(05) COMP.
              10 WS-JSON-VAL-TYPE PIC X(10).
                 88 JSON-VAL-TYPE-STRING   VALUE 'STRING    '.
                 88 JSON-VAL-TYPE-NUMERIC  VALUE 'NUMERIC   '.
                 88 JSON-VAL-TYPE-DECIMAL  VALUE 'DECIMAL   '.
                 88 JSON-VAL-TYPE-OBJECT   VALUE 'OBJECT    '.
                 88 JSON-VAL-TYPE-BOOLEAN  VALUE 'BOOLEAN   '.
                 88 JSON-VAL-TYPE-UNKNOWN  VALUE 'UNKNOWN   '.
              10 WS-JSON-VAL      PIC X(10).
              
              
       PROCEDURE DIVISION.
       0000-MAIN-SECTION.
       
              PERFORM 1000-INITIALIZE
                 THRU 1000-EXIT.
          PERFORM UNTIL EndOfJSONFile
              PERFORM 2000-DISPLAY
                 THRU 2000-EXIT
             READ JSONFile
                AT END SET EndOfJSONFile TO TRUE
             END-READ
          END-PERFORM.
          CLOSE JSONFile.
      *         UNTIL WS-MENU-OPTION IS EQUAL TO WS-EXIT-OPTION.
                
           STOP RUN.
              
       1000-INITIALIZE.
              INITIALIZE WS-JSON-INPUT
                         WS-KEY
                         WS-VAL
                         WS-JSON-NUM-ITEMS.
      *                  WS-JSON-IDX.
              MOVE +100 TO WS-JSON-MAX-ITEMS
              SET NOT-READING-A-KEY TO TRUE.
              SET KEY-UNFOUND TO TRUE.
              SET NOT-READING-A-VAL TO TRUE.
              SET VAL-UNFOUND TO TRUE.
              SET TOKEN-KEY TO TRUE.
              SET PROCESSING-NONE TO TRUE.
          OPEN INPUT JSONFile
          READ JSONFile
             AT END SET EndOfJSONFile TO TRUE
          END-READ.
              
       1000-EXIT.
              EXIT.
              
       2000-DISPLAY.
           DISPLAY "#####2000-DISPLAY".
      *    DISPLAY "PLEASE ENTER YOUR JSON STRING TO PARSE:".
      
      *    ACCEPT WS-JSON-INPUT.
           MOVE JSONRec TO WS-JSON-INPUT.
      *    INITIALIZE WS-JSON.
           MOVE 1 TO WS-JSON-INPUT-LEN.
           UNSTRING WS-JSON-INPUT(1:10000)
              DELIMITED BY ';;'
              INTO WS-GARBAGE
              WITH POINTER WS-JSON-INPUT-LEN
           END-UNSTRING.
           IF WS-JSON-INPUT-LEN >= 10000 THEN
              DISPLAY "NO END DELIMITER. "
                  "PLEASE FINISH THE STRING"
                  " WITH ;;"
              GO TO 2000-EXIT
           END-IF.
           COMPUTE WS-JSON-MAX = 
             WS-JSON-INPUT-LEN - 3.
           MOVE +0 TO WS-JSON-IDX.
           MOVE +1 TO WS-JSON-IDX2.
           SET JSON-VAL-TYPE-UNKNOWN(WS-JSON-IDX2)
            TO TRUE.
           PERFORM UNTIL WS-JSON-IDX >= WS-JSON-MAX 
             ADD +1 TO WS-JSON-IDX
             MOVE WS-GARBAGE(WS-JSON-IDX:1)
               TO WS-JSON-CHAR(WS-JSON-IDX)
             EVALUATE WS-JSON-CHAR(WS-JSON-IDX)
                WHEN WS-OPENING-BRACES
                     PERFORM 2001-PROCESS-OPENING-BRACES
                        THRU 2001-EXIT
                WHEN WS-CLOSING-BRACES
                     PERFORM 2002-PROCESS-CLOSING-BRACES
                        THRU 2002-EXIT
                WHEN WS-DOUBLE-QUOTE
                     PERFORM 2010-PROCESS-QUOTE
                        THRU 2010-EXIT
                WHEN WS-COLON
                     PERFORM 2020-PROCESS-COLON
                        THRU 2020-EXIT
                WHEN WS-COMMA
                     PERFORM 2030-PROCESS-COMMA
                        THRU 2030-EXIT
                WHEN OTHER 
                     PERFORM 2090-PROCESS-OTHER
                        THRU 2090-EXIT
             END-EVALUATE
           END-PERFORM.
           
      *
           PERFORM 9000-DISPLAY
              THRU 9000-EXIT.
              
       2000-EXIT.
              EXIT.
       2001-PROCESS-OPENING-BRACES.
           DISPLAY "#####2001-PROCESS-OPENING-BRACES".
           MOVE +1 TO WS-JSON-NUM-ITEMS.
       2001-EXIT.
              EXIT.
       2002-PROCESS-CLOSING-BRACES.
           DISPLAY "#####2002-PROCESS-CLOSING-BRACES".
       2002-EXIT.
              EXIT.
       2010-PROCESS-QUOTE.
           DISPLAY "#####2010-PROCESS-QUOTE".
              IF TOKEN-KEY THEN
                     PERFORM 2011-PROCESS-KEY
                        THRU 2011-EXIT
              ELSE
                     PERFORM 2012-PROCESS-VAL
                        THRU 2012-EXIT
              END-IF.
       2010-EXIT.
              EXIT.
       2011-PROCESS-KEY.
           DISPLAY "#####2011-PROCESS-KEY".
                IF NOT-READING-A-KEY THEN
                   SET READING-A-KEY
                    TO TRUE
                   SET KEY-FOUND TO TRUE
                   MOVE ZERO TO WS-TEMP-KEY-LEN
                ELSE
                   SET NOT-READING-A-KEY
                    TO TRUE
                   SET KEY-UNFOUND TO TRUE
                   MOVE WS-KEY
                     TO WS-JSON-KEY(WS-JSON-IDX2)
                   MOVE WS-TEMP-KEY-LEN
                     TO WS-JSON-KEY-LEN(WS-JSON-IDX2)
                   MOVE SPACES TO WS-KEY 
                   MOVE ZERO TO WS-TEMP-KEY-LEN 
                END-IF.
       2011-EXIT.
              EXIT.
       2012-PROCESS-VAL.
           DISPLAY "#####2012-PROCESS-VAL".
                IF NOT-READING-A-VAL THEN
                   SET READING-A-VAL
                    TO TRUE
                   SET VAL-FOUND TO TRUE
                   MOVE ZERO TO WS-TEMP-KEY-LEN
                ELSE
                   SET NOT-READING-A-VAL
                    TO TRUE
                   SET VAL-UNFOUND TO TRUE
                   MOVE WS-VAL
                     TO WS-JSON-VAL(WS-JSON-IDX2)
                   MOVE WS-TEMP-VAL-LEN
                     TO WS-JSON-VAL-LEN(WS-JSON-IDX2)
                   MOVE SPACES TO WS-VAL   
                   MOVE ZERO TO WS-TEMP-VAL-LEN 
                END-IF.
       2012-EXIT.
              EXIT.
       2020-PROCESS-COLON.
           DISPLAY "#####2020-PROCESS-COLON".
              IF TOKEN-KEY THEN
                     SET TOKEN-VAL TO TRUE
                     SET PROCESSING-VALUE TO TRUE
                     INITIALIZE WS-VAL
              ELSE
                     SET TOKEN-KEY TO TRUE
                     SET PROCESSING-KEY TO TRUE
                     INITIALIZE WS-KEY
              END-IF.
       2020-EXIT.
              EXIT.
       2030-PROCESS-COMMA.
           DISPLAY "#####2030-PROCESS-COMMA".
            ADD +1 TO WS-JSON-NUM-ITEMS.
            SET TOKEN-KEY TO TRUE.
            SET NOT-READING-A-KEY TO TRUE.
            ADD +1 TO WS-JSON-IDX2.
           SET JSON-VAL-TYPE-UNKNOWN(WS-JSON-IDX2)
            TO TRUE.
       2030-EXIT.
              EXIT.
       2090-PROCESS-OTHER.
           DISPLAY "#####2090-PROCESS-OTHER".
             IF PROCESSING-VALUE THEN
                IF TOKEN-KEY THEN
                 ADD +1 TO WS-TEMP-KEY-LEN
                 STRING 
                   WS-KEY DELIMITED BY SPACES
                   WS-JSON-CHAR(WS-JSON-IDX)
                            DELIMITED BY SIZE
                   INTO WS-KEY
                 END-STRING
                ELSE
                 ADD +1 TO WS-TEMP-VAL-LEN
                 MOVE WS-JSON-CHAR(WS-JSON-IDX)
                   TO WS-TEMP-CHAR
                 IF JSON-VAL-TYPE-UNKNOWN(WS-JSON-IDX2) THEN
                     IF WS-TEMP-NUM IS NUMERIC THEN
                        SET JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) TO TRUE
                     ELSE
                        SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                     END-IF
                 ELSE
                    IF JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) THEN
                       IF WS-TEMP-NUM IS NUMERIC THEN
                          SET JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) 
                           TO TRUE
                       ELSE
                          SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                       END-IF
                    ELSE
                          SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                    END-IF
                 END-IF
                 STRING 
                   WS-VAL DELIMITED BY SPACES
                   WS-JSON-CHAR(WS-JSON-IDX)
                            DELIMITED BY SIZE
                   INTO WS-VAL
                END-IF
             ELSE
                IF TOKEN-KEY THEN
                 MOVE WS-JSON-CHAR(WS-JSON-IDX2)
                   TO WS-TEMP-CHAR
                 ADD +1 TO WS-TEMP-KEY-LEN
                 IF JSON-VAL-TYPE-UNKNOWN(WS-JSON-IDX2) THEN
                     IF WS-TEMP-NUM IS NUMERIC THEN
                        SET JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) TO TRUE
                     ELSE
                        SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                     END-IF
                 ELSE
                    IF JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) THEN
                       IF WS-TEMP-NUM IS NUMERIC THEN
                          SET JSON-VAL-TYPE-NUMERIC(WS-JSON-IDX2) 
                           TO TRUE
                       ELSE
                          SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                       END-IF
                    ELSE
                          SET JSON-VAL-TYPE-STRING(WS-JSON-IDX2) TO TRUE
                    END-IF
                 END-IF
                 STRING 
                   WS-KEY DELIMITED BY SPACES
                   WS-JSON-CHAR(WS-JSON-IDX)
                            DELIMITED BY SIZE
                   INTO WS-KEY
                 END-STRING
                ELSE
                 ADD +1 TO WS-TEMP-VAL-LEN
                 STRING 
                   WS-VAL DELIMITED BY SPACES
                   WS-JSON-CHAR(WS-JSON-IDX)
                            DELIMITED BY SIZE
                   INTO WS-VAL
                END-IF
             END-IF.
       2090-EXIT.
              EXIT.
       9000-DISPLAY. 
           DISPLAY "JSON: " WS-JSON-STRING.
           DISPLAY "NUMBER OF ITEMS: "
           WS-JSON-NUM-ITEMS.
           
           DISPLAY "ALL KEYS BELOW: "
           DISPLAY 
           "KEY        (LEN)   |VALUE       (LEN)    |TYPE"
           DISPLAY 
           "-------------------|---------------------|----------"
           MOVE +0 TO WS-JSON-IDX2.
           PERFORM UNTIL 
               WS-JSON-IDX2 >= WS-JSON-MAX-ITEMS OR 
               WS-JSON-IDX2 = WS-JSON-NUM-ITEMS
             ADD +1 TO WS-JSON-IDX2
             DISPLAY WS-JSON-KEY(WS-JSON-IDX2)
             " (" WS-JSON-KEY-LEN(WS-JSON-IDX2) ")"
             "| " WS-JSON-VAL(WS-JSON-IDX2)
             " (" WS-JSON-VAL-LEN(WS-JSON-IDX2) ")"
             " | " WS-JSON-VAL-TYPE(WS-JSON-IDX2)
           END-PERFORM.
       9000-EXIT.
              EXIT.
       END PROGRAM JSONParser.
