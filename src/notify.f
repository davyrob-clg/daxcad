C
C     @(#)  412.1 date 6/11/92 notify.f 
C
C
C     Filename    : notify.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:10
C     Last change : 92/06/11 14:36:39
C
C     Copyright : Practical Technology International Limited  
C     File :- notify.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,TIME_OUT,ST)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C     ===================================================
C1    VARYPE                  I4  I4 I4  I4     I4    I4
C1    IOSTAT                  I   O  O   O      O     O
C
C2    This routine will get data from the key borad printing to the 
C2    start position requested The other parameters are global and should be 
C2    set at initalisation. 
C2    It also handles button input 
C
C
      include    'include/gpr.ins.inc'
      include    'include/edit.inc'
      include    'include/apfont.inc'
      include    'include/input.inc'
      include    'include/dialog.inc'
      include    'include/event.inc'
      include    'include/buttons.inc'
C
      INTEGER*4 DT_EVTYPE
      INTEGER*4 DT_EVNUM
      INTEGER*4 ET
      INTEGER*4 OLDINP,CURNUM,CURS
      INTEGER*4 ST
      INTEGER*4 BUTTON
      INTEGER*4 BUTNUM,INPNUM,TTYNUM
      INTEGER*4 EVTYPE,EVNUM,X,Y
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,XP,YP
      INTEGER*4 NLEN,BUFLEN
      INTEGER*4 PID
      INTEGER*4 FONT
      INTEGER*4 EDGE
      INTEGER*4 LABNUM
      INTEGER*4 ICHAR
      INTEGER*2 SCRPOS(2)
      INTEGER*2 TIMEC
      INTEGER*2 N,I,POS
      INTEGER*2 DIRECT
      INTEGER*2 SX,SY,ICH,NLEN2
      INTEGER*2 EVT
      LOGICAL OK,PNTBOX
      LOGICAL TCOL
      LOGICAL LOCATOR
      LOGICAL LWAIT
      CHARACTER EVDATA*1,TEXT*81
      DOUBLE PRECISION TIME_OUT
      DOUBLE PRECISION TIMER
C
      EXTERNAL PNTBOX
      EXTERNAL ICHAR
C
C     check events here

      CALL CURSOR_ON(.TRUE.)
1020  CONTINUE
C     wait for something to happen
      LWAIT=GPR_$EVENT_WAIT(EVT,EVDATA,SCRPOS,ST)

      EVTYPE = EVT

3000  CONTINUE
C
      IF (EVTYPE.EQ.GPR_$ENTERED_WINDOW) THEN
C
          CALL CURSOR_ON(.TRUE.)
          CONTINUE
C
      ENDIF

      IF(EVTYPE.EQ.GPR_$LEFT_WINDOW) THEN
C	
          CALL CURSOR_ON(.FALSE.)
          CONTINUE
C
      ENDIF
C
      LOCATOR = EVTYPE.EQ.GPR_$LOCATOR.OR.
     +   EVTYPE.EQ.GPR_$LOCATOR_UPDATE.OR.
     +   EVTYPE.EQ.GPR_$KEYSTROKE
C     dialog box check for range
C     special event this locator

      IF(EVTYPE.EQ.GPR_$KEYSTROKE) THEN
C         make sure locator not taken
          LOCATOR = .FALSE.
      ENDIF
C
      IF(LOCATOR) THEN
C         check for locator movement on user defined cursor or system cursor
          CALL CURSOR_UPDATE(SCRPOS,ST)
          GOTO 1020
      ELSEIF(EVTYPE.EQ.GPR_$KEYSTROKE) THEN
C         test for an active input device
          ICH = ICHAR(EVDATA)
C         look for possible move in a window
          IF(CINPN.EQ.0) THEN
C
              IF ( ICH .EQ. 136 ) THEN
                  evtype = DAX_EVENT_CURSORUP
              ELSE IF ( ICH .EQ. 138 ) THEN
                  evtype = DAX_EVENT_CURSORLEFT
              ELSE IF ( ICH .EQ. 140 ) THEN
                  evtype = DAX_EVENT_CURSORRIGHT
              ELSE IF ( ICH .EQ. 142 ) THEN
                  evtype = DAX_EVENT_CURSORDN
              ELSE
C                 ordinary key hit
                  EVTYPE = DAX_EVENT_KEY
                  EVNUM = ICH
                  BUTTON = ICH
                  GOTO 900
              END IF
              EVNUM = 0
              BUTTON = ICH
              GOTO 900
          ENDIF
          CALL CURSOR_ON(.FALSE.)
          OLDINP = CINPN
          IF(CINPN.GT.0) THEN
              FONT= INPFNT(CINPN)
              CALL SET_FONT(FONT,ST)
          ENDIF
C         primary filter 
          IF(ICHAR(EVDATA).EQ.TXTERM.OR.ICH.EQ.13) THEN
C             escape used clean if nes
              CALL EDIT_CLEANUP()
C             set input number
              EVNUM = OLDINP
              EVTYPE = DAX_EVENT_RETURN
              GOTO 900
          ELSEIF(ICH.EQ.UPCHR) THEN
              CALL EDIT_CLEANUP()
C             set input number
              EVNUM = OLDINP
              EVTYPE = DAX_EVENT_CURSORUP
              GOTO 900
          ELSEIF(ICH.EQ.DNCHR) THEN
              CALL EDIT_CLEANUP()
C             set input number
              EVNUM = OLDINP
              EVTYPE = DAX_EVENT_CURSORDN
              GOTO 900
          ENDIF
c         make sure text font set
          IF(ICH.EQ.PASTEC) THEN
C             we can paste from a paste buffer file
              CALL EDIT_PASTE(ST)
              GOTO 1000
          ELSEIF(ICH.EQ.LINEDL) THEN
C             this will delete the line completely
              CALL EDIT_LINE_DEL()
              GOTO 1000
          ELSEIF(ICH.EQ.LEFEXT) THEN
              CALL CURSOR_LEFT()
          ELSEIF(ICH.EQ.RIGEXT) THEN
              CALL CURSOR_RIGHT()
          ELSE
C             move cursor if not possible then no text available
              CALL EDIT_TEXT_CHAR(EVDATA,DIRECT,ST)
          ENDIF

C
C         update positions
C
          GOTO 1000
      ELSEIF(EVTYPE.EQ.GPR_$BUTTONS) THEN

C         get pressed button
          BUTTON_PRESSED = ICHAR(EVDATA)
C         set return value
          BUTTON = BUTTON_PRESSED
C
          BUTTON_STAT = BUTTON_PRESSED.EQ.BUTTON_1_DOWN.OR.
     +       BUTTON_PRESSED.EQ.BUTTON_2_DOWN.OR.
     +       BUTTON_PRESSED.EQ.BUTTON_3_DOWN
C         if not first button then ignore it.

          IF(.NOT.BUTTON_STAT) THEN
C             If an upstroke then exit out with event
              EVNUM = 0
              EVTYPE = DAX_EVENT_NULL
              GOTO 900
          ENDIF

          IF ( CINPN.GT.0.AND.BUTTON.EQ.BUTTON_3_DOWN ) THEN
C
C              Emulate CR on 3rd button hit
C
               EVTYPE = GPR_$KEYSTROKE
               EVDATA = CHAR(TXTERM)
               GOTO 3000
          ENDIF
          CALL BUTTON_TEST(SCRPOS,BUTNUM,ST)
          EVNUM = 0
C

          IF(BUTNUM.GT.0.AND.BUTTON.EQ.BUTTON_1_DOWN) THEN
              CALL CURSOR_ON(.FALSE.)
              IF(.NOT.BUTHI(BUTNUM)) THEN
                  CALL BUTTON_HIGH(BUTNUM,ST)
               ELSE
                  CALL BUTTON_LOW(BUTNUM,ST)
               ENDIF
               EVNUM = BUTNUM
C              turn off edit pads
               CALL EDIT_CLEANUP()
               EVTYPE = DAX_EVENT_BUTTON
C              jump out for event now
               GOTO 900
          ENDIF
C         turn cursor off now to save any overwrite
          CALL SET_INPUT(SCRPOS,INPNUM,ST)
          IF(ST.EQ.0) THEN
               EVNUM =INPNUM
               EVTYPE = DAX_EVENT_INPUT
C              jump out for event now
               GOTO 900
          ENDIF
C
          IF ( BUTTON_STAT) THEN
               EVTYPE = DAX_EVENT_BUTTON
               EVNUM = 0
               GOTO 900
          ENDIF

      ELSE
C         Unknown event or otherwise
	  GOTO 1020

      ENDIF
C
900   CONTINUE
C     set points 
      X = SCRPOS(1)
      Y = SCRPOS(2)
1000  continue
C     Second entry point
      END
C
