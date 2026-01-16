C
C     @(#)  412.1 date 6/11/92 buttons.f 
C
C
C     Filename    : buttons.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:13
C     Last change : 92/06/11 14:24:43
C
C     Copyright : Practical Technology International Limited  
C     File :- buttons.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C


      SUBROUTINE BUTTONINIT(ST)
C     =========================
C1    VARYPE                  I4
C1    IOSTAT                  I
C
C2    Inialises all button values Only to be called at the start of the program
C  
      include    'include/edit.inc'
      include    'include/apfont.inc'
      include    'include/buttons.inc'
C
      INTEGER*4 ST
      INTEGER*2 I,APFNT
C
      DO 10 I=1,BUTMAX
C         set active flag
          BUTACT(I) = .FALSE.
          BUTDIS(I) = .FALSE.
C         reset text ascociated with it
          BUTTXT(I) = ' ' 
C         set fonts
          BUTFNT(I) = 0
C         colors
          BUTCOL(1,I) = 0
          BUTCOL(2,I) = 0
C         type (standard 0)
          BUTTYP(I) = 0
C         thickness of frame
          BUTTHK(I) = 0
C         button is low
          BUTHI(I) = .FALSE.

          BUTOX(I) = 4
          BUTOY(I) = 4

          BUTHI(I) = .FALSE.
10    CONTINUE
C
C     set curwent count
      BUTCNT = 0
C     set netx button posion
      BUTNXT(1) = BUTOX(1)
      BUTNXT(2) = BUTOY(1)
      END
C
      SUBROUTINE BUTTON_MAKE(XP,YP,FONT,COLOR,BCOL,
     +                       THICK,TYPE,
     +                       TEXT,LENGTH,JUST,BUTNUM,ST)
C     ==============================================
C1    VARYPE                I4 I4 L      L    I4
C1                             I4   I4 
C1                            I4    I4      
C1                            C*(*) I4    I4   I4
C2    IOSTAT                              O    O
C
C2    This routine is called when the user wants to place 
C2    a button on the screen. A button number is returned
C2    as the identifier which will be returned on any hits
C2    made on the button when displayed
C
      include    'include/buttons.inc'
      include    'include/apfont.inc'
      include    'include/edit.inc'
C
      INTEGER*4 ST
      INTEGER*4 FONT
      INTEGER*4 X,Y
      INTEGER*4 LENGTH,JUST
      INTEGER*4 BUTNUM
      INTEGER*4 BCOL
      INTEGER*4 XP,YP
      INTEGER*4 COLOR,THICK,TYPE,AUTOGAP
      INTEGER*4 BOXH,BOXW,BOX(4),BOY(4)
      CHARACTER*(*) TEXT
      INTEGER*2 I,LT,NLEN2,FONTID,DIRECT
      LOGICAL AUTOX,AUTOY,AUTO
C
      IF(BUTNUM.EQ.0)  THEN
          DO 10 I=1,BUTMAX
              IF(.NOT.BUTACT(I)) THEN
                  BUTNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF
C
20    CONTINUE
C     set apollo font number from array
      IF(FONT.LE.0.OR.FONT.GT.100) THEN
C         set as current loaded font
          CALL INQ_FONT(FONT,ST)
      ENDIF
C
      X = XP
      Y = YP
C
      BUTFNT(BUTNUM) = FONT
      BUTACT(BUTNUM) = .TRUE.
      BUTDIS(BUTNUM) = .FALSE.
      BUTCOL(1,BUTNUM) = COLOR
      BUTCOL(2,BUTNUM) = BCOL
      BUTTHK(BUTNUM) = THICK
      BUTTYP(BUTNUM) = TYPE
C
C     store text with this one
      BUTTXT(BUTNUM) = TEXT(1:LENGTH)
      BUTTXT(BUTNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
C     
C     increment button count
      BUTCNT = BUTCNT + 1
      ST = 0
C
C     set for jsutification
      CALL SET_FONT(FONT,ST)
      CALL SET_TEXT(BUTTXT(BUTNUM)(:LENGTH+1) ,ST)
      BOXW = TEXTW +(2*BUTOX(BUTNUM))
      BOXH = TEXTH +2*(BUTOY(BUTNUM))
C
C     save height and width
C
      BUTPOS(3,BUTNUM) = BOXW
      BUTPOS(4,BUTNUM) = BOXH
C
      IF(JUST.EQ.1) THEN
          X = X - BOXW/2
      ELSEIF(JUST.EQ.2) THEN
          X = X - BOXW
      ELSEIF(JUST.EQ.3) THEN
          Y = Y - BOXH/2
      ELSEIF(JUST.EQ.4) THEN
          X = X - BOXW/2
          Y = Y - BOXH/2
      ELSEIF(JUST.EQ.5) THEN
          X = X - BOXW
          Y = Y - BOXH/2
      ELSEIF(JUST.EQ.6) THEN
          Y = Y - BOXH
      ELSEIF(JUST.EQ.7) THEN
          Y = Y - BOXH
          X = X - BOXW/2
      ELSEIF(JUST.EQ.8) THEN
          Y = Y - BOXH
          X = X - BOXW
      ENDIF
      BUTPOS(1,BUTNUM) = MAX(X,BUTOX(BUTNUM))
      BUTPOS(2,BUTNUM) = MAX(Y,BUTOY(BUTNUM))
C
C     save in case we have to go again
      PRVBUT = BUTNUM
C
C
      END
C
C
      SUBROUTINE BUTTON_TEST(SCRPOS,BUTNUM,ST)
C     =======================================
C1    VARYPE                 I2      I4   I4
C1    IOSTAT                 I       O    O
C
C2    Quite simple tests all buttons active and displayed
C2    for a range test If found a button number is returned
C2    anything goes wrong and ST is set
C2    If no button found then BUTNUM is 0
C
      include    'include/edit.inc'
      include    'include/buttons.inc'
      include    'include/apfont.inc'
      include    'include/dialog.inc'
C
      LOGICAL PNTBOX,INBOX,OK
      INTEGER*2 NLEN2
      INTEGER*2 SCRPOS(2)
      INTEGER*4 BUTNUM
      INTEGER*2 CNT,N
      INTEGER*2 DIRECT
      INTEGER*2 SX,SY
      INTEGER*4 FONTID
      INTEGER*4 XP,YP,ST
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,I
      CHARACTER TEXT*100

C
      EXTERNAL PNTBOX,NLEN2
C
      BUTNUM = 0 
      CNT = 0
10    CONTINUE
      CNT = CNT+1
C     is button displayed
      OK = PARENT_DIALOG(CNT).EQ.DIAGCR
C
      IF(BUTDIS(CNT).AND.OK) THEN
          INBOX = .FALSE.
C         this has text in it and is visable
C         calculate box size here
          XMIN = BUTPOS(1,CNT) - BUTOX(CNT)
          YMIN = BUTPOS(2,CNT) - BUTOY(CNT)
          XMAX = XMIN+BUTPOS(3,CNT)
          YMAX = YMIN+BUTPOS(4,CNT)
          XP = SCRPOS(1)
          YP = SCRPOS(2)
          INBOX = PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)
          IF(INBOX) THEN
              BUTNUM = CNT
              GOTO 999
          ENDIF
      ENDIF
      IF(CNT.LT.BUTMAX) GOTO 10
999   CONTINUE
C     reset all 
      END
C
      SUBROUTINE BUTTON_HIGH(BUTNUM,ST)
C     =================================
C1    VARYPE                  I4    I4
C1    IOSTAT                  I     O
C
C2    Routine will highlight a cell by inverting the window around it
C2  
C2  
      include    'include/buttons.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*4 TCOL
      INTEGER*4 BUTNUM
C
C     range check 
      IF(BUTNUM.LT.1.OR.BUTNUM.GT.BUTMAX) THEN
C         out of range
          ST = 1
          RETURN
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         button not active
          ST = 2
          RETURN
      ENDIF
      BUTHI(BUTNUM) = .TRUE.
      CALL BUTTON_MARK(.TRUE.,BUTNUM,ST)
      END
C
      SUBROUTINE BUTTON_LOW(BUTNUM,ST)
C     ================================
C1    VARYPE                  I4    I4
C1    IOSTAT                  I     O
C
C2    Routine will highlight a cell by inverting the window around it
C2  
C2  
      include    'include/buttons.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*4 BUTNUM
      INTEGER*4 TCOL
C
C     range check 
      IF(BUTNUM.LT.1.OR.BUTNUM.GT.BUTMAX) THEN
C         out of range
          ST = 1
          RETURN
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         button not active
          ST = 2
          RETURN
      ENDIF
      BUTHI(BUTNUM) = .FALSE.
      CALL BUTTON_DRAW(BUTNUM,.TRUE.,ST)
      END
C
      SUBROUTINE BUTTON_MARK(HIGHB,BUTNUM,ST)
C     ======================================
C1    VARYPE                  L     I4    I4
C1    IOSTAT                  I     I     O
C
C2    Routine will highlight a cell by inverting the window around it
C2  
C2  
      include    'include/daxcolor.inc'
      include    'include/edit.inc'
      include    'include/buttons.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 ST,TYPE
      INTEGER*4 BUTNUM
      INTEGER*2 RECT(4),BOX(4),BOY(4),XP(9)
      INTEGER*2 YP(9),NPOS
      INTEGER*2 X,Y
      INTEGER*2 BOXH,BOXW
      INTEGER*2 N
      INTEGER*2 NLEN2
      INTEGER*4 FONTID
      INTEGER*4 TXBAL
      CHARACTER*100 TEXT
      LOGICAL HIGHB
C
      EXTERNAL NLEN2
C
      IF(.NOT.BUTACT(BUTNUM)) THEN
          RETURN
      ENDIF
C
C     quick test for current standings
      FONTID = BUTFNT(BUTNUM)
      CALL SET_FONT(FONTID,ST)
C     find out text extents
      TEXT = BUTTXT(BUTNUM)
      CALL SET_TEXT(TEXT,ST)
      BOXW = TEXTW
      BOXH = TEXTH
C
      IF(HIGHB) THEN
          CALL TOOLPEN(BUTCOL(1,BUTNUM))
      ELSE
          CALL TOOLPEN(BUTCOL(2,BUTNUM))
      ENDIF
C
      TYPE = BUTTYP(BUTNUM)
      IF(TYPE.EQ.1.OR.TYPE.EQ.3.OR.TYPE.EQ.4) THEN
C         no clipping on this box 
          CLPSIZ = 0
      ELSE
          CLPSIZ = 4
      ENDIF
      NPOS = 9
      BOX(1) = BUTPOS(1,BUTNUM)-BUTOX(BUTNUM)
      BOY(1) = BUTPOS(2,BUTNUM)-BUTOY(BUTNUM)
      BOX(2) = BOX(1) + (2*BUTOX(BUTNUM)) + BOXW
      BOY(2) = BOY(1)
      BOX(3) = BOX(2) 
      BOY(3) = BOY(2)+ (2*BUTOY(BUTNUM)) + BOXH
      BOX(4) = BOX(1) 
      BOY(4) = BOY(3)
C
      XP(1) = BOX(1) + CLPSIZ
      YP(1) = BOY(1)
      XP(2) = BOX(2) - CLPSIZ
      YP(2) = BOY(1)

      XP(3) = BOX(2)
      YP(3) = BOY(2) + CLPSIZ
      XP(4) = BOX(3)
      YP(4) = BOY(3) - CLPSIZ

      XP(5) = BOX(3) - CLPSIZ
      YP(5) = BOY(3) 

      XP(6) = BOX(4) + CLPSIZ
      YP(6) = BOY(4) 
 
      XP(7) = BOX(4) 
      YP(7) = BOY(4) - CLPSIZ

      XP(8) = BOX(1) 
      YP(8) = BOY(1) + CLPSIZ
      XP(9) = XP(1)
      YP(9) = YP(1)
C
      CALL ROPREP()
C     fill button
      CALL BUTTON_POLYFILL(XP,YP,NPOS,ST)

      IF(HIGHB) THEN
	  CALL TOOLPEN_TEXT(BUTCOL(2,BUTNUM),BUTCOL(1,BUTNUM),.FALSE.,ST)
      ELSE
	  CALL TOOLPEN_TEXT(BUTCOL(1,BUTNUM),BUTCOL(2,BUTNUM),.FALSE.,ST)
      ENDIF
C
      IF(FONTID.GT.0) THEN
          TXBAL = TXTBAL(FONTID)
      ENDIF
      N= NLEN2(TEXT)-1
      X = BUTPOS(1,BUTNUM)+TORGX+TXSPACE/2
      Y = BUTPOS(2,BUTNUM)+TORGY+TXBAL
      CALL TOOLS_TEXT(X,Y,TEXT,N,ST)
C
      CALL GPR_$SERVER_FLUSH_X(ST)
C
      END
C
      SUBROUTINE BUTTON_DRAW(BUTNUM,DISP,ST)
C     =====================================
C1    VARYPE                   I4    L   I4
C1    IOSTAT            
C
C2    Routine constructs a button and draws it in the screen
C2    You must use BUTTON_ERASE to remove it The construction is
C2    something like this
C2     origin   ______________________
C2             /                      \
C2            |      TEXT IN BOX       |
C2             \______________________/
C2
C2
      include    'include/daxcolor.inc'
      include    'include/edit.inc'
      include    'include/buttons.inc'
      include    'include/apfont.inc'
      include    'include/interface.inc'
      include    'include/dialog.inc'     
C
      CHARACTER*100 TEXT
      INTEGER*2 NLEN2
      INTEGER*2 RECT(4)
      INTEGER*2 BOXH,BOXW,N
      INTEGER*2 XP(18),YP(18),NPOS,X,Y,BOX(4),BOY(4),TXBAL
      INTEGER*4 ST,I,TYPE,THICK,COL
      INTEGER*4 BUTNUM
      INTEGER*4 FONTID
      INTEGER*4 SHAD
      LOGICAL FIRST,UNOBSCURED,DISP
      LOGICAL HIGHB
C
      EXTERNAL NLEN2
C
      IF(.NOT.BUTACT(BUTNUM)) THEN
          RETURN
      ENDIF
C     set current font please
      FONTID = BUTFNT(BUTNUM)
      CALL SET_FONT(FONTID,ST)
C     find out text extents
      TEXT = BUTTXT(BUTNUM)
      CALL SET_TEXT(TEXT,ST)
      BOXW = TEXTW
      BOXH = TEXTH
C
      CALL ROPREP()
      NPOS = 18
C     do we need the box clipped at each corner type
      TYPE = BUTTYP(BUTNUM)
      THICK = MAX(1,BUTTHK(BUTNUM))
      IF(TYPE.EQ.1.OR.TYPE.EQ.3) THEN
          CLPSIZ = 0
      ELSE
          CLPSIZ = 4
      ENDIF
C     set points for box Note taht 4 represent the clipped box
      IF(.NOT.DISP) THEN
C         clear the whole thing
          IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
              SHAD = 4
          ELSE
              SHAD = 0
          ENDIF
          RECT(1) = BUTPOS(1,BUTNUM)-BUTOX(BUTNUM) - THICK 
          RECT(2) = BUTPOS(2,BUTNUM)-BUTOY(BUTNUM) - THICK
          RECT(3) = (2*BUTOX(BUTNUM)) + BOXW + (2*THICK) + SHAD + 1
          RECT(4) = (2*BUTOY(BUTNUM)) + BOXH + (2*THICK) + SHAD + 1

          COL = DIALOGB
          CALL TOOLPEN(COL)
          CALL TOOLS_RECTANGLE(RECT,ST)
          RETURN
      ELSE
C         a handy place to set the parent dialog of the button
          PARENT_DIALOG(BUTNUM) = DIAGCR
      ENDIF
      BOX(1) = BUTPOS(1,BUTNUM)-BUTOX(BUTNUM)
      BOY(1) = BUTPOS(2,BUTNUM)-BUTOY(BUTNUM)
      BOX(2) = BOX(1) + (2*BUTOX(BUTNUM)) + BOXW
      BOY(2) = BOY(1)
      BOX(3) = BOX(2) 
      BOY(3) = BOY(2)+ (2*BUTOY(BUTNUM)) + BOXH
      BOX(4) = BOX(1) 
      BOY(4) = BOY(3)
C

      FIRST = .TRUE.
C
      DO 10 I=0,9,9
          XP(I+1) = BOX(1) + CLPSIZ
          YP(I+1) = BOY(1)

          XP(I+2) = BOX(2) - CLPSIZ
          YP(I+2) = BOY(1)

          XP(I+3) = BOX(2)
          YP(I+3) = BOY(2) + CLPSIZ
  
          XP(I+4) = BOX(3)
          YP(I+4) = BOY(3) - CLPSIZ

          XP(I+5) = BOX(3) - CLPSIZ
          YP(I+5) = BOY(3) 

          XP(I+6) = BOX(4) + CLPSIZ
          YP(I+6) = BOY(4) 
 
          XP(I+7) = BOX(4) 
          YP(I+7) = BOY(4) - CLPSIZ

          XP(I+8) = BOX(1) 
          YP(I+8) = BOY(1) + CLPSIZ

          XP(I+9) = XP(I+1)
          YP(I+9) = YP(I+1)
C         do the border
          IF(FIRST) THEN
              BOX(1) = BOX(1) -THICK
              BOY(1) = BOY(1) -THICK
              BOX(2) = BOX(2) +THICK
              BOY(2) = BOY(2) -THICK
              BOX(3) = BOX(3) +THICK
              BOY(3) = BOY(3) +THICK
              BOX(4) = BOX(4) -THICK
              BOY(4) = BOY(4) +THICK
              FIRST= .FALSE.
              CLPSIZ = CLPSIZ + 1
          ENDIF

10    CONTINUE
C
      
40    CONTINUE
      IF(DISP) THEN
          CALL TOOLPEN(BUTCOL(1,BUTNUM))
      ELSE
          CALL TOOLPEN(INT4(COLBAK))
      ENDIF
      IF(TYPE.NE.4) THEN
          CALL BUTTON_POLYFILL(XP,YP,NPOS,ST)
      ENDIF
C     highlight backgroung color as well

      HIGHB = BUTHI(BUTNUM) 
      BUTHI(BUTNUM) = DISP
      CALL BUTTON_MARK(.NOT.DISP,BUTNUM,ST)
C     Now for the text
      BUTHI(BUTNUM) = HIGHB

 30   CONTINUE
C     do a shadow
      IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
C         do the shadow
          XP(1) = BOX(2)
          YP(1) = BOY(2) + CLPSIZ + 4
          XP(2) = BOX(2)
          YP(2) = BOY(3) - CLPSIZ
          XP(3) = BOX(3) - CLPSIZ
          YP(3) = BOY(3)
          XP(4) = BOX(4) + CLPSIZ+4
          YP(4) = BOY(4)
          BOX(3) = BOX(3) + 4
          BOY(3) = BOY(3) + 4
          CLPSIZ = CLPSIZ + 1
          XP(5) = XP(4)
          YP(5) = YP(4) + 4
          XP(6) = BOX(3) - CLPSIZ
          YP(6) = YP(5)
          XP(7) = BOX(3)
          YP(7) = BOY(3) - CLPSIZ
          XP(8) = XP(1) + 4
          YP(8) = YP(1)
          XP(9) = XP(1)
          YP(9) = YP(1)
          NPOS = 9
C         request a shadow color
          CALL ROPREP()
          IF(DISP) THEN
C             set shadow drawing color
              COL = 256
          ELSE
C             set shadow erase color to canvas background
              COL = COLBAK
          ENDIF
          CALL TOOLPEN(COL)
          CALL BUTTON_POLYFILL(XP,YP,NPOS,ST)
      ENDIF

C
C
      END
C

      SUBROUTINE BUTDRW(BUTNUM,DISP,ST)
C     =================================
C1    VARYPE              I4    L   I4
C1    IOSTAT              I     I   O
C
C2    User driven routine to draw on and off input
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/buttons.inc'     
C
      INTEGER*4 BUTNUM
      INTEGER*4 ST
      LOGICAL DISP
C
      IF(DISP.AND..NOT.BUTDIS(BUTNUM)) THEN
          BUTDIS(BUTNUM) = .TRUE.
      ELSEIF(.NOT.DISP.AND.BUTDIS(BUTNUM)) THEN
          IF(BUTHI(BUTNUM)) THEN
              CALL BUTTON_LOW(BUTNUM,ST)
          ENDIF
          BUTHI(BUTNUM) = .FALSE.
          BUTDIS(BUTNUM) = .FALSE.
      ELSE
          RETURN
      ENDIF
C     draw or erase the text
      CALL BUTTON_DRAW(BUTNUM,DISP,ST)
      IF(BUTHI(BUTNUM).AND.DISP) THEN
          BUTHI(BUTNUM) = .FALSE.
          CALL BUTTON_HIGH(BUTNUM,ST)
      ENDIF

      END
C
      SUBROUTINE BUTTON_POLYFILL(NX,NY,NOE,ST)
C     ========================================
C1    VARYPE                     I2 I2 I2  I4
C1    IOSTAT                     I  I  I   O
C
C2    Fills a polygon boundary for buttons
C2  
C2  
      INTEGER*4 ST
      INTEGER*2 NOE,NX(NOE),NY(NOE)
      LOGICAL UNOBSCURED
C
C     fill the polygon
C
      CALL TOOLS_POLYFILL(NX,NY,NOE,ST)
C
      END


      SUBROUTINE BUTMOD(BUTNUM,BUFF,BUFLEN,ST)
C     ========================================
C1    VARYPE               I4    C     I4   I4
C1    IOSTAT               I     I     I    O
C
C2    The routine will modify nthe text associated eith the number
C
      include    'include/buttons.inc'
      include    'include/edit.inc'
C
      CHARACTER*100 BUFF
      INTEGER*2 NLEN2,LENGTH
      INTEGER*4 BUTNUM
      INTEGER*4 FONT
      INTEGER*4 ST,BUFLEN,LL
      LOGICAL REDRAW
      LOGICAL HIGHB
C
      EXTERNAL NLEN2
C
      ST = 0
      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) THEN
          ST = 1
          GOTO 999
      ENDIF
      REDRAW = BUTDIS(BUTNUM)
      HIGHB = BUTHI(BUTNUM)
      IF(BUTACT(BUTNUM)) THEN 

C         Get the text and strip off the null
          IF(BUFLEN.GT.0) THEN
              BUTTXT(BUTNUM) = BUFF(1:BUFLEN)
              LENGTH = BUFLEN
              IF(LENGTH.EQ.0) GOTO 999
C             append the null please
              LL = LEN(BUTTXT(BUTNUM)) 
              IF(LL.LE.LENGTH) LENGTH = LENGTH -1
              BUTTXT(BUTNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
          ENDIF
          IF(REDRAW) THEN
              CALL BUTTON_MARK(HIGHB,BUTNUM,ST)
          ENDIF
      ENDIF

      FONT = BUTFNT(BUTNUM)
      CALL SET_FONT(FONT,ST)
      CALL SET_TEXT(BUTTXT(BUTNUM)(:LENGTH+1) ,ST)
      BUTPOS(3,BUTNUM) = TEXTW +(2*BUTOX(BUTNUM))
      BUTPOS(4,BUTNUM) = TEXTH +2*(BUTOY(BUTNUM))
C
      RETURN
C
999   CONTINUE
      END
C
      SUBROUTINE BUTMOD_REPAINT(BUTNUM,NEWBUF,NEWLEN,ST)
C     ==================================================
C     VARTYPE                     I4      C*    I4   I4
C     IOSTAT                      I       I     I    O
C     
C     this is a routine which will undraw a button before 
C     modifying it then repainting it. this is to avoid 
C     having badly resized buttons on a butmod.
C     
      include    'include/buttons.inc'
      INTEGER*4 BUTNUM
      INTEGER*4 ST
      INTEGER*4 ST1
      INTEGER*4 NEWLEN

      LOGICAL HI
      LOGICAL DIS

      CHARACTER*100 NEWBUF

C     assume button is not displayed
      DIS = .FALSE.
      IF (BUTDIS(BUTNUM)) THEN
C        reset the flag if it is displayed.
         DIS = .TRUE.
      ENDIF

C     assume the button is low
      HI = .FALSE.
      IF (BUTHI(BUTNUM)) THEN
C        reset the flag if high
         HI = .TRUE.
      ENDIF

C     now hi and dis contain the hi/lo and 
      IF (HI) THEN
C        low the button (just to be safe)
         CALL BUTTON_LOW(BUTNUM,ST)
      ENDIF
C     display status of the button
      IF (DIS) THEN
C        undraw the button
         CALL BUTDRW(BUTNUM,.FALSE.,ST1)
      ENDIF

      CALL BUTMOD(BUTNUM,NEWBUF,NEWLEN,ST1)

C     redraw the button if required
      IF (DIS) THEN
         CALL BUTDRW(BUTNUM,.TRUE.,ST1)
      ENDIF

C     make hi again if required
      IF (HI) THEN
         CALL BUTTON_HIGH(BUTNUM,ST1)
      ENDIF

      END

      SUBROUTINE INQ_BUTTON_STAT(BUTNUM,HILO,ST)
C     ==========================================
C1    VARYPE                      I4     L   I4
C1    IOSTAT                      I      O   O
C
C2    Returns button status either high or low
C2    CUSTOMER USEABLE
C2  
C
      include    'include/buttons.inc'
C
      INTEGER*4 BUTNUM,ST
      LOGICAL HILO
C
      ST = 0
C     range check
      IF(BUTNUM.LT.1.OR.BUTNUM.GT.BUTMAX) THEN
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.BUTDIS(BUTNUM)) THEN
          ST = 2
          GOTO 999
      ENDIF
C     set return state
      HILO = BUTHI(BUTNUM)
      RETURN
999   CONTINUE
C     error here 
      END
C
C
C
      SUBROUTINE BUTTON_GET_TEXT(BUTNUM,BUFF,INLEN,BUFLEN,ST)
C     =======================================================
C1    VARYPE                      I4   C       I4   I4    I4
C1    IOSTAT                      I    O       I4   I/O     O
C
C2    This routine reads the input sttring currently in INPNUM
C  
      include    'include/buttons.inc'
C
      INTEGER*4 BUFLEN,ST,LENGTH
      INTEGER*4 BUTNUM,NLEN
      INTEGER*4 INLEN
      CHARACTER*100 BUFF
C
      EXTERNAL NLEN
C
C     range check
      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) GOTO 999
C
      LENGTH = NLEN(BUTTXT(BUTNUM))
C
      IF(INLEN.GT.LEN(BUFF)) THEN
C         set buffer please but modify
          BUFF = BUTTXT(BUTNUM)(1:LENGTH-1)
      ELSE
C         set buffer to users length
          BUFF(1:INLEN) = BUTTXT(BUTNUM)(1:LENGTH-1)
      ENDIF
C
C     set outgoing buffer length
      BUFLEN = NLEN(BUTTXT(BUTNUM)) 
C
      ST =0 
      RETURN
999   CONTINUE
      ST = 1
      END
C
      SUBROUTINE BUTTON_SET_ATTR(BUTNUM,TOOL_ATTRIBUTES,TOOL_TEXT,ST)
C     ===============================================================
C1    VARYPE                       I4       STRUCT         STRUCT I4
C1    IOSTAT                       I          I              I    O
C
C2    This routine sets attributes of a button
C2  
C2  
      include    'include/buttons.inc'
      include    'include/attributes.inc'
C
      INTEGER*4 BUTNUM
      INTEGER*4 LENGTH
      INTEGER*4 ST
      LOGICAL REDRAW
C
      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
      IF(TOOL_ATTRIBUTES(1).NE.1) THEN
C         attributres array not been set
          ST = 3
          GOTO 999
      ENDIF
C
      REDRAW = BUTDIS(BUTNUM)
C
      IF(REDRAW) THEN
          CALL BUTDRW(BUTNUM,.FALSE.,ST)
      ENDIF
C
C     x and y postion
      BUTPOS(1,BUTNUM) = TOOL_ATTRIBUTES(2)
      BUTPOS(2,BUTNUM) = TOOL_ATTRIBUTES(3)
C     font
      BUTFNT(BUTNUM) =   TOOL_ATTRIBUTES(6)
C     color
      BUTCOL(1,BUTNUM) = TOOL_ATTRIBUTES(7)
      BUTCOL(2,BUTNUM) = TOOL_ATTRIBUTES(8)
C     type
      BUTTYP(BUTNUM) = TOOL_ATTRIBUTES(9)
C     now dependant coding
      BUTOX(BUTNUM) = TOOL_ATTRIBUTES(11)
      BUTOY(BUTNUM) = TOOL_ATTRIBUTES(12)
C
C     text now
      LENGTH = TOOL_ATTRIBUTES(10)
      IF(LENGTH.GT.0) THEN
          LENGTH = MIN(LENGTH,LEN(BUTTXT(BUTNUM))-1)
          BUTTXT(BUTNUM)(1:LENGTH) = TOOL_TEXT(1:LENGTH)
C         put in a null
          BUTTXT(BUTNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
      ENDIF

      IF(REDRAW) THEN
          CALL BUTDRW(BUTNUM,.TRUE.,ST)
      ENDIF
      ST = 0
999   CONTINUE
      END
C
      SUBROUTINE BUTTON_GET_ATTR(BUTNUM,TOOL_ATTRIBUTES,TOOL_TEXT,ST)
C     ===============================================================
C1    VARYPE                       I4       STRUCT         STRUCT I4
C1    IOSTAT                       I          O              O    O
C
C2    This routine Gets attributes of a button
C2  
C2  
      include    'include/buttons.inc'
      include    'include/attributes.inc'
C
      INTEGER*4 BUTNUM
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      INTEGER*4 ST
      LOGICAL REDRAW
C
      EXTERNAL NLEN
C
      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
C     set header 
      TOOL_ATTRIBUTES(1) = 1
C
C     save all attributes
      TOOL_ATTRIBUTES(2)= BUTPOS(1,BUTNUM) 
      TOOL_ATTRIBUTES(3)= BUTPOS(2,BUTNUM) 
      TOOL_ATTRIBUTES(6)= BUTFNT(BUTNUM) 
      TOOL_ATTRIBUTES(7)= BUTCOL(1,BUTNUM) 
      TOOL_ATTRIBUTES(8)= BUTCOL(2,BUTNUM) 
      TOOL_ATTRIBUTES(9)= BUTTYP(BUTNUM) 
      TOOL_ATTRIBUTES(11)= BUTOX(BUTNUM) 
      TOOL_ATTRIBUTES(12)= BUTOY(BUTNUM) 
C
C     text now
      LENGTH = NLEN(BUTTXT(BUTNUM))-1
      IF(LENGTH.GT.1) THEN
C         set text please
          TOOL_ATTRIBUTES(10) = LENGTH
          TOOL_TEXT(1:LENGTH) = BUTTXT(BUTNUM)(1:LENGTH)
      ELSE
C         nothing in theis button
          LENGTH = 0
      ENDIF
      ST = 0
999   CONTINUE
      END

      SUBROUTINE BUTTON_KILL(BUTNUM,ST)
C     ================================
C1    VARYPE                   I4   I4
C1    IOSTAT                    I   I
C
C2    Resets a button for use
C  
      include    'include/buttons.inc'
C
      INTEGER*4 BUTNUM
      INTEGER*4 ST
C
      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
C     must now undraw if currently displayed
      IF(BUTDIS(BUTNUM)) THEN
C         undraw it
          CALL BUTDRW(BUTNUM,.FALSE.,ST)
      ENDIF
C
C     set flag
      BUTACT(BUTNUM) = .FALSE.
      ST = 0
999   CONTINUE
C
      END
C
C
      SUBROUTINE CURSOR_TO_BUTTON(BUTNUM,ST)
C     ======================================
C1    VARYPE                        I4   I4
C1    IOSTAT                        I    I
C
C2    This routine allows the user to plonk his cursor right in the
C2    middle of a button for a sort of implied hit
C  
      include    'include/buttons.inc'
      include    'include/cursor.inc'

      INTEGER*4 BUTNUM
      INTEGER*4 ST
      INTEGER*4 XMIN,YMIN
      INTEGER*4 XP,YP
      INTEGER*4 POS(2)

      IF(BUTNUM.LE.0.OR.BUTNUM.GT.BUTMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.BUTACT(BUTNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
      XMIN = BUTPOS(1,BUTNUM) - BUTOX(BUTNUM)
      YMIN = BUTPOS(2,BUTNUM) - BUTOY(BUTNUM)
C
      XP = XMIN+BUTPOS(3,BUTNUM)/2
      YP = YMIN+BUTPOS(4,BUTNUM)/2
C
      POS(1) = XP
      POS(2) = YP
C
      CALL  CURSOR_MODIFY(POS,ST)
C
      RETURN
C
999   CONTINUE
      END



      SUBROUTINE BUTTONSIZE(BUTNUM,SIZE)
C     ==================================
C1    VARYPE                  I4   I4(2)
C1    IOSTAT                  I      O
C
C2    Returns the dimensions of the buttons text
C2  
C2    Arguments:-
C2  
C2    BUTNUM		->		Button Id
C2    SIZE		->		X and Y size
C2  
C2  
C2    Error Returns:
C2  
C2    NONE ( Make sure you use the correct id )
C2  
C2  
      include    'include/edit.inc'
      include    'include/buttons.inc'
C
      INTEGER*4 BUTNUM
      INTEGER*4 FONT
      INTEGER*4 SIZE(2)
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
C
      EXTERNAL NLEN
C
      FONT = BUTFNT(BUTNUM)
      CALL SET_FONT(FONT,ST)

      LENGTH = NLEN(BUTTXT(BUTNUM))
      IF ( LENGTH.GT.0 ) THEN

          CALL SET_TEXT(BUTTXT(BUTNUM)(:LENGTH),ST)
          SIZE(1) = TEXTW +(2*BUTOX(BUTNUM)) + 10
          SIZE(2) = TEXTH +2*(BUTOY(BUTNUM)) + 10
      ELSE
          SIZE(1) = 2*BUTOX(BUTNUM) + 10
          SIZE(2) = 2*BUTOY(BUTNUM) + 10
      ENDIF
C
      END


