123456*8901234567890123456789012345678901234567890
       IDENTIFICATION    DIVISION.
       PROGRAM-ID.       CONBOLER.
       DATA              DIVISION.
       WORKING-STORAGE   SECTION.
      *FOR MESSAGE
       01 SEL-LV.
           03 SEL-LV-HEADER PIC X(15) VALUE "You select LV- ".
           03 SEL-LV-VAL PIC X(1).
      *FOR SCREEN
       01 POS-INDEX-LIST VALUE "abcdefghijklmnopqrstuvwxyz".
           03 POS-INDEX PIC X OCCURS 26.
       01 CMN-IDX-DISP.
           03 CMN-IDX-DISP-SP PIC X.
           03 CMN-IDX-DISP-BODY PIC X OCCURS 26.
       01 ROW-DISP.
           03 ROW-IDX PIC X.
           03 ROW-BODY PIC X(26).
       01 DISP-MARK.
           03 UNKNOWN PIC X VALUE "O".
           03 MINE PIC X VALUE "F".
           03 NEAR-MINES VALUE "12345678.".
               05 NEAR-MINE PIC X OCCURS 9.
           03 EX-MINE PIC X VALUE "B".
           03 SUSPECT PIC X VALUE "?".
       01 MINE-MARK.
           03 ISSAFE PIC 9 VALUE 0.
           03 ISMINE PIC 9 VALUE 1.
      *FOR START
       77 LV    PIC X(1).
       01 MAP-CONF.
           03 ISSET    PIC X(1).
           03 WIDTH    PIC 9(2).
           03 HEIGHT   PIC 9(2).
           03 N-MINE     PIC 9(2).
       01 CONF-B PIC X(7) VALUE "Y090910".
       01 CONF-I PIC X(7) VALUE "Y161640".
       01 CONF-V PIC X(7) VALUE "Y261699".
      *MAP
       01 GAME-MAP.
           03 CLMN OCCURS 16.
             05 ROW VALUE ALL " ".
               07 CELL PIC X OCCURS 26.
             05 MINE-ROW.
               07 MINE-CELL PIC 9 OCCURS 26.
       77 FIXED-POS PIC 9(3).
      *CTL
       77 ON-GAME PIC X.
       01 EDGE.
           03 UP-EDGE PIC X.
           03 DOWN-EDGE PIC X.
           03 LEFT-EDGE PIC X.
           03 RIGHT-EDGE PIC X.
      *WK
       01 PXY.
           03 PX PIC 9(2).
           03 PY PIC 9(2).
       77 FG-A PIC X.
       77 N-NEAR-MINE PIC 9.

      *CNT
       77 CNTI PIC 9(3).
       77 CNTJ PIC 9(3).
      *CMD
       01 CMD-STR.
          03 X-STR PIC X.
          03 Y-STR PIC X.
          03 C-STR PIC X.
       01 CMD.
          03 X PIC 9(2).
          03 Y PIC 9(2).
          03 C PIC X.

       PROCEDURE        DIVISION.
       MAIN SECTION.
           PERFORM SEL-LEVEL THRU EXIT-INIT.
           PERFORM DISP THRU EXIT-PLAY UNTIL ON-GAME NOT = " ".
       STOP RUN.

       INIT SECTION.
       SEL-LEVEL.
           DISPLAY "Welcome to COinBOLer!".
           PERFORM UNTIL ISSET = "Y"
               DISPLAY "Select Level (b/i/v)"
               ACCEPT LV
               EVALUATE LV
                   WHEN "b"
                       MOVE CONF-B TO MAP-CONF
                   WHEN "i"
                       MOVE CONF-I TO MAP-CONF
                   WHEN "v"
                       MOVE CONF-V TO MAP-CONF
                   WHEN OTHER
                       DISPLAY "Wrong input"
                       CONTINUE
               END-EVALUATE
           END-PERFORM.
           MOVE LV TO SEL-LV-VAL.
           DISPLAY SEL-LV.
       SET-MINE.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > N-MINE
               MOVE "A" TO FG-A
               PERFORM UNTIL FG-A = " "
                   COMPUTE PX = FUNCTION RANDOM * (WIDTH + 1)
                   COMPUTE PY = FUNCTION RANDOM * (HEIGHT + 1)
                   MOVE " " TO FG-A
                   IF MINE-CELL(PY, PX) = ISMINE
                       THEN
                           MOVE "A" TO FG-A
                   END-IF
               END-PERFORM
               MOVE ISMINE TO MINE-CELL(PY, PX)
           END-PERFORM.
       INIT-SCREEN.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > WIDTH
               MOVE POS-INDEX(CNTI) TO CMN-IDX-DISP-BODY(CNTI)
           END-PERFORM.
       INIT-MAP.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > HEIGHT
               PERFORM VARYING CNTJ FROM 1 BY 1 UNTIL CNTJ > WIDTH
                   MOVE UNKNOWN TO CELL(CNTI, CNTJ)
               END-PERFORM
           END-PERFORM.
       EXIT-INIT.
           EXIT.

       PLAY SECTION.
       DISP.
           PERFORM SCREEN-OUT THRU EXIT-SCREEN-OUT.
           PERFORM MINE-SCREEN-OUT THRU EXIT-MINE-SCREEN-OUT.
       GET-INPUT.
           PERFORM WITH TEST AFTER UNTIL C NOT = " "
               DISPLAY "Guess (XYC(mINE/oK/sUSPECT/uNKNOWN)): "
               ACCEPT CMD-STR
               PERFORM PARSE-CMD THRU EXIT-PARSE-CMD
               IF CELL(Y, X) NOT = "O" AND NOT = "?" AND NOT = "F"
                   THEN
                       DISPLAY "NOT EFFECTIVE COMMAND"
                       MOVE " " TO C
           END-PERFORM.
           DISPLAY CMD.
       EXECUTE-CMD.
           EVALUATE C
               WHEN "m"
                   MOVE MINE TO CELL(Y, X)
               WHEN "s"
                   MOVE SUSPECT TO CELL(Y, X)
               WHEN "u"
                   MOVE UNKNOWN TO CELL(Y, X)
               WHEN "o"
                   PERFORM CHK-MINE THRU EXIT-CHK-MINE
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.
       EXIT-PLAY.
           EXIT.

       CHK-MINE.
           IF MINE-CELL(Y,X) = ISMINE
               THEN
                   MOVE "E" TO ON-GAME
               ELSE
                   PERFORM CHK-NEAR THRU EXIT-CHK-NEAR
                   IF N-NEAR-MINE = 0
                       THEN
                           MOVE NEAR-MINE(9) TO CELL(Y,X)
                       ELSE
                           MOVE NEAR-MINE(N-NEAR-MINE) TO CELL(Y,X)
                   END-IF
           END-IF.
       EXIT-CHK-MINE.
           EXIT.


       CMN SECTION.
       SCREEN-OUT.
           DISPLAY CMN-IDX-DISP.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > HEIGHT
               MOVE POS-INDEX(CNTI) TO ROW-IDX
               MOVE ROW(CNTI) TO ROW-BODY
               DISPLAY ROW-DISP
           END-PERFORM.
       EXIT-SCREEN-OUT.
           EXIT.
       PARSE-CMD.
           INITIALIZE CMD.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > WIDTH
               IF X-STR = POS-INDEX(CNTI)
                   THEN
                       MOVE CNTI TO X
               END-IF
           END-PERFORM.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > HEIGHT
               IF Y-STR = POS-INDEX(CNTI)
                   THEN
                       MOVE CNTI TO Y
               END-IF
           END-PERFORM.
           IF X NOT = 0  AND Y NOT = 0
               THEN
                   IF C-STR = "m" OR "o" OR "s" OR "u"
                       THEN
                           MOVE C-STR TO C
                   END-IF
           END-IF.
       EXIT-PARSE-CMD.
       MINE-SCREEN-OUT.
           DISPLAY CMN-IDX-DISP.
           PERFORM VARYING CNTI FROM 1 BY 1 UNTIL CNTI > HEIGHT
               MOVE POS-INDEX(CNTI) TO ROW-IDX
               MOVE MINE-ROW(CNTI) TO ROW-BODY
               DISPLAY ROW-DISP
           END-PERFORM.
       EXIT-MINE-SCREEN-OUT.
           EXIT.

       CHK-NEAR.
           PERFORM CHK-EDGE THRU EXIT-CHK-EDGE.
           INITIALIZE N-NEAR-MINE.
           DISPLAY EDGE.
           IF UP-EDGE = " "
               THEN
                   ADD MINE-CELL(Y - 1,X) TO N-NEAR-MINE
           END-IF.
           IF UP-EDGE = " " AND LEFT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y - 1,X - 1) TO N-NEAR-MINE
           END-IF.
           IF LEFT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y,X - 1) TO N-NEAR-MINE
           END-IF.
           IF DOWN-EDGE = " " AND LEFT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y + 1,X - 1) TO N-NEAR-MINE
           END-IF.
           IF DOWN-EDGE = " "
               THEN
                   ADD MINE-CELL(Y + 1,X) TO N-NEAR-MINE
           END-IF.
           IF DOWN-EDGE = " " AND RIGHT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y + 1,X + 1) TO N-NEAR-MINE
           END-IF.
           IF RIGHT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y ,X + 1) TO N-NEAR-MINE
           END-IF.
           IF UP-EDGE = " " AND RIGHT-EDGE = " "
               THEN
                   ADD MINE-CELL(Y - 1  ,X + 1) TO N-NEAR-MINE
           END-IF.
           DISPLAY N-NEAR-MINE.
       EXIT-CHK-NEAR.
           EXIT.

       CHK-EDGE.
           INITIALIZE EDGE.
           IF X = 1
               THEN
                   MOVE "E" TO LEFT-EDGE
               ELSE
                   IF X = WIDTH
                       THEN
                           MOVE "E" TO RIGHT-EDGE
                   END-IF
           END-IF.
           IF Y = 1
               THEN
                   MOVE "E" TO UP-EDGE
               ELSE
                   IF X = HEIGHT
                       THEN
                           MOVE "E" TO DOWN-EDGE
                   END-IF
           END-IF.
       EXIT-CHK-EDGE.
           EXIT.



