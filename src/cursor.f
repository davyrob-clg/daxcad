C
C     @(#)  412.1 date 6/11/92 cursor.f 
C
C
C     Filename    : cursor.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:04
C     Last change : 92/06/11 14:25:58
C
C     Copyright : Practical Technology International Limited  
C     File :- cursor.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CURSORINIT(ST)
C     SUBROUTINE CURSOR_MAKE(CURNUM,PATTERN,LENGTH,CHR,FONT,COLOR,
C     SUBROUTINE CURSOR_DRAW(CURNUM,ON,ST)
C     SUBROUTINE CURDRW(CURNUM,ST)
C     SUBROUTINE CURSOR_DRAW_XHAIR(ST)
C     SUBROUTINE CURSOR_DRAW_SCROSS(ST)
C     SUBROUTINE CURSOR_DRAW_CROSS(ST)
C     SUBROUTINE CURSOR_DRAW_X(ST)
C     SUBROUTINE CURSOR_DRAW_CHAR(DRAW,ST)
C     SUBROUTINE CURSOR_UPDATE(SCRPOS,ST)
C     SUBROUTINE CURSOR_ON(VISAB)
C     SUBROUTINE NEW_CURSOR(CURNUM,ST)
C     SUBROUTINE INQ_CURSOR(CURNUM,POS,ST)
C     SUBROUTINE INQ_CURSOR_STATE(STATE,ST)
C     SUBROUTINE CURSOR_MODIFY(POS,ST)
C
C     |-----------------------------------------------------------------|
C
C     
C
      SUBROUTINE CURSORINIT(ST)
C     ==========================
C1    VARYPE                 I4
C1    IOSTAT                 O
C
C2  
C2  
C2  
      include    'include/cursor.inc'
      INTEGER*4 ST,I
C
C
      CURSBD = .FALSE.
C     make sure cursor is switched on by default
      CURSON = .FALSE.
C     cursor type should be 0 to start with
      CURSTY = 0
C     frame dependant flag
      INFRAM = .FALSE.
      ST = 0
      DO 10 I=1,MAXCUR
          CURSDF(I) = .FALSE.
 10   CONTINUE
      END



      SUBROUTINE CURSOR_MAKE(CURNUM,PATTERN,LENGTH,CHR,FONT,COLOR,
     +                       ACTIVE,ST)
C     ====================================================================
C1    VARYPE                   I4     I4     I4    C   I4    I4     L   I4   
C1    IOSTAT                   I      I      I     I   I     I      I   O
C
C2    This routine will define the cursor pattern and subseqent 
C2    activeness of the cursor ie whether locator events are returned
C2    It sends a structure of data for the cursor pattern such as 
C2    Thickness length font character etc
C  
      include    'include/apollo.inc'
      include    'include/event.inc'
      include    'include/cursor.inc'
      include    'include/bitmap.inc'
C
      INTEGER*4 CURNUM
      INTEGER*4 PATTERN
      INTEGER*4 ST
      INTEGER*4 LENGTH(4)
      INTEGER*4 FONT
      INTEGER*4 COLOR,I
      INTEGER*4 BMSIZE(2)
      LOGICAL ACTIVE
      CHARACTER CHR
C
C     make sure cursor bitmap defined is not until we need it
C
C     set maximum size of cursor bitmap
      BMSIZE(1) = 100
      BMSIZE(2) = 100
      IF(CURNUM.EQ.0)  THEN
          DO 10 I=1,MAXCUR
              IF(.NOT.CURSDF(I)) THEN
                  CURNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF
C
20    CONTINUE
C
C     wee touch of error checking
      IF(CURNUM.EQ.0) THEN
          PRINT*, 'Too many cursor patterns defined'
          GOTO 999
      ELSEIF(CURNUM.GT.MAXCUR) THEN
          PRINT*, 'Pattern out of range'
          GOTO 999
      ENDIF
C     defined cursor now
      CURSDF(CURNUM) = .TRUE.
C     set color
      CURSCO(CURNUM) = COLOR
C     lenght in both x and y
      CURSLN(1,CURNUM) = LENGTH(1)
      CURSLN(2,CURNUM) = LENGTH(2)
      CURSLN(3,CURNUM) = LENGTH(3)
      CURSLN(4,CURNUM) = LENGTH(4)
      CURSEV(CURNUM) = ACTIVE
C     font file and character to use
      IF(FONT.GT.0) THEN
          CURSFN(CURNUM) = FONT
      ENDIF
      CURSCH(CURNUM) = CHR
      CURSNO(CURNUM) = PATTERN
C     need to define the bitmap at this posint if required
      IF(PATTERN.EQ.CURSOR_CHAR.AND..NOT.CURSBD) THEN
C         allocate a bitmap now into bitmap system
          CURSBT = 0
C         make bitmap to save postion underneath
          CALL MAKE_BITMAP(CURSBT,BMSIZE,0,ST)
          CURSBD = .TRUE.
C         ok all done if we get here
      ENDIF
      ST = 0
      RETURN
999   CONTINUE
      ST = 1
      END
C
C
C
      SUBROUTINE CURSOR_DRAW(CURNUM,ON,ST)
C     ===================================
C1    VARYPE                   I4   I4
C1    IOSTAT                   I    O
C
C2    Draws a cursor into the screen at the current cursor postion
C
C
      include    'include/cursor.inc'
      include    'include/style.inc'
C
      INTEGER*4 CURNUM
      LOGICAL ON
      INTEGER*4 ST,PATTERN,COL
C
C     set pattern
      PATTERN = CURSNO(CURNUM)
      COL = CURSCO(CURNUM)
      CALL TOOLPEN(COL)
      IF(PATTERN.EQ.CURSOR_XHAIR) THEN
C         simple cross hair pattern going across canvas
          CALL ROPXOR()
          CALL CURSOR_DRAW_XHAIR(ST)
          CALL ROPREP()
      ELSEIF(PATTERN.EQ.CURSOR_CROSS) THEN
          CALL ROPXOR()
          CALL CURSOR_DRAW_CROSS(ST)
          CALL ROPREP()
      ELSEIF(PATTERN.EQ.CURSOR_SCROSS) THEN
          CALL ROPXOR()
          CALL CURSOR_DRAW_SCROSS(ST)
          CALL ROPREP()
      ELSEIF(PATTERN.EQ.CURSOR_X) THEN
          CALL ROPXOR()
          CALL CURSOR_DRAW_X(ST)
          CALL ROPREP()
      ELSEIF(PATTERN.EQ.CURSOR_CHAR) THEN
          CALL ROPREP()
          CALL CURSOR_DRAW_CHAR(ON,ST)
      ENDIF
      END

      SUBROUTINE CURDRW(CURNUM,ST)
C     ===========================
C1    VARYPE              I4   I4
C1    IOSTAT              I     O
C
C2    Draws a cursor into the screen at the current cursor postion
C2    if it currenttly displayed or not
C2    If CURNUM is 0 then it will turn of any existing cursor
C2    The system cursor will also be set and activated if curnum is 0
C
      include    'include/apollo.inc'
      include    'include/edit.inc'
      include    'include/cursor.inc'
C
      INTEGER*2 CTYPE,FONTID,X,Y
      INTEGER*4 FONT
      LOGICAL DISP,CSTAT
      INTEGER*4 ST
      INTEGER*4 PATTERN
      INTEGER*4 CURNUM
      CHARACTER CHR
C
C     test type first
      IF(CURNUM.LT.0.OR.CURNUM.GT.MAXCUR) GOTO 999
c
C     save current state of the cursor
      CSTAT = CURSON
C     if on the turn of while we change pattern
      IF(CURSON) THEN
C         merely turns current cursor off
          CALL CURSOR_ON(.FALSE.)
      ENDIF
C
C     set up defintions
      IF(CURNUM.GT.0) THEN
C         user defined cursor required set required eventing
          IF(.NOT.CURSDF(CURNUM)) GOTO 999
      ELSE
C         disable standard user locator information
          CURSTY = 0
C         turn system cursor on now and return immedialtly
          CALL CURSOR_ON(.TRUE.)
          RETURN
      ENDIF
C     set global cursor identifier now
      CURSTY = CURNUM
C     set pattern type
      PATTERN = CURSNO(CURNUM)
C
C     if user cursor pattern then draw the pattern into the bitmap now
      IF(PATTERN.EQ.CURSOR_CHAR) THEN
C         get details about character we are to use
          CHR = CURSCH(CURNUM)
          FONT = CURSFN(CURNUM)
          CALL SET_FONT(FONT,ST)
          CALL SET_TEXT(CHR,ST)
C         save local origin of this character
          CURSLN(1,CURNUM) = TEXTW
          CURSLN(2,CURNUM) = TEXTH
          CURSLN(3,CURNUM) = TORGX
          CURSLN(4,CURNUM) = TORGY
      ENDIF
C     turn on the cursor now please
      CALL CURSOR_ON(.TRUE.)
C     ok thats all folks
      ST =0
      RETURN
999   CONTINUE
C     error has occurred
      ST = 1
      END
C
C
C
      SUBROUTINE CURSOR_DRAW_XHAIR(ST)
C     ================================
C1    VARYPE                       I4
C1    IOSTAT                        O
C
C2    Draws the cursor at desired postions
C2    color and draw mode allready set
C
      include    'include/cursor.inc'
      include    'include/curwin.inc'
      include    'include/style.inc'
C
      INTEGER*2 X1,Y1
      INTEGER*2 X2,Y2
      INTEGER*2 CURNUM
      INTEGER*4 ST
      INTEGER*4 COL
C
      COL = COLOUR
      CALL TOOLPEN(COL)
      CURNUM = CURSTY
C     set system cursor pos
      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C     draw out own cursor
      X1 = CURSLN(1,CURNUM)
      Y1 = CURSXY(2)
      X2 = X1+CURSLN(3,CURNUM)
      Y2 = Y1
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)
C
      Y1 = CURSLN(2,CURNUM)
      X1 = CURSXY(1)
      X2 = X1
      Y2 = Y1+CURSLN(4,CURNUM)
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)

      X1 = CURSXY(1) - XWIDTH
      Y1 = CURSXY(2) - YWIDTH
      X2 = CURSXY(1) + XWIDTH
      Y2 = CURSXY(2) + YWIDTH
C
C  NEEB didnt like this - GCU.
      CALL TOOLS_BOX(X1,Y1,X2,Y2,ST)
      END
C
      SUBROUTINE CURSOR_DRAW_SCROSS(ST)
C     ================================
C1    VARYPE                       I4
C1    IOSTAT                        O
C
C2    Draws the cursor at desired postions
C2    color and draw mode allready set
C
      include    'include/cursor.inc'
      include    'include/style.inc'
      include    'include/curwin.inc'
C
      INTEGER*2 X1,Y1
      INTEGER*2 X2,Y2
      INTEGER*2 CURNUM
      INTEGER*4 ST
C
      CURNUM = CURSTY
C     set system cursor pos
      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C     draw out own cursor
      Y1 = CURSXY(2) 
      Y2 = CURSXY(2) 
      X1 = CURSXY(1) - CURSLN(1,CURNUM)
      X2 = X1+(2*CURSLN(1,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)

      X1 = CURSXY(1)
      X2 = CURSXY(1)
      Y1 = CURSXY(2) - CURSLN(2,CURNUM)
      Y2 = Y1+(2*CURSLN(2,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)
C
C     Pick box now
C
      X1 = CURSXY(1) - XWIDTH	
      Y1 = CURSXY(2) - YWIDTH
      X2 = CURSXY(1) + XWIDTH
      Y2 = CURSXY(2) + YWIDTH
C SPB 280994 - NEEB don't like this either ...
      CALL TOOLS_BOX(X1,Y1,X2,Y2,ST)
C SPB 280994 - EOF

      END
C
      SUBROUTINE CURSOR_DRAW_CROSS(ST)
C     ================================
C1    VARYPE                       I4
C1    IOSTAT                        O
C
C2    Draws the cursor at desired postions
C2    color and draw mode allready set
C
      include    'include/cursor.inc'
      include    'include/style.inc'
C
      INTEGER*2 X1,Y1
      INTEGER*2 X2,Y2
      INTEGER*2 CURNUM
      INTEGER*4 ST
C
      CURNUM = CURSTY
C     set system cursor pos
      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C     draw out own cursor
      Y1 = CURSXY(2) 
      Y2 = CURSXY(2) 
      X1 = CURSXY(1) - CURSLN(1,CURNUM)
      X2 = X1+(2*CURSLN(1,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)

      Y1 = CURSXY(2) -1 
      Y2 = CURSXY(2) -1 
      X1 = CURSXY(1) - CURSLN(1,CURNUM) 
      X2 = X1+(2*CURSLN(1,CURNUM)) 

      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)
C
      X1 = CURSXY(1)
      X2 = CURSXY(1)
      Y1 = CURSXY(2) - CURSLN(2,CURNUM)
      Y2 = Y1+(2*CURSLN(2,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)

      X1 = CURSXY(1) -1 
      X2 = CURSXY(1) -1 
      Y1 = CURSXY(2) - CURSLN(2,CURNUM) 
      Y2 = Y1+(2*CURSLN(2,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)

      END
C
      SUBROUTINE CURSOR_DRAW_X(ST)
C     =============================
C1    VARYPE                   I4
C1    IOSTAT                    O
C
C2    Draws the cursor at desired postions
C2    color and draw mode allready set
C
      include    'include/cursor.inc'
C
      INTEGER*2 X1,Y1
      INTEGER*2 X2,Y2
      INTEGER*2 CURNUM
      INTEGER*4 ST
C
      CURNUM = CURSTY
C     set system cursor pos
      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C     draw out own cursor
      X1 = CURSXY(1) - CURSLN(1,CURNUM)
      Y1 = CURSXY(2) - CURSLN(1,CURNUM)
      X2 = X1 + (2*CURSLN(1,CURNUM))
      Y2 = Y1 + (2*CURSLN(1,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)
C
      X1 = CURSXY(1) - CURSLN(1,CURNUM)
      Y1 = CURSXY(2) + CURSLN(1,CURNUM)
      X2 = X1 + (2*CURSLN(1,CURNUM))
      Y2 = Y1 - (2*CURSLN(1,CURNUM))
      CALL TOOLS_LINE(X1,Y1,X2,Y2,ST)
C
      END
C
      SUBROUTINE CURSOR_DRAW_CHAR(DRAW,ST)
C     ====================================
C1    VARYPE                      L   I4
C1    IOSTAT                      I    O
C
C2    Draws the cursor at desired postions
C2    color and draw mode allready set
C2    if DRAW is true then the area under the 
C2    text will be saved and the text drawn on top
C     if DRAW is false then the area will be recovered
C
      include    'include/cursor.inc'
C
      INTEGER*2 X,Y,CURNUM,TCURSXY(2)
      INTEGER*4 WINDOW(4),ORG(2)
      INTEGER*4 ST
      INTEGER*4 FONT
      INTEGER*2 INT2
      CHARACTER*1 CHR
      LOGICAL DRAW
      SAVE TCURSXY
C
      CURNUM = CURSTY
C     set system cursor pos
      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C     draw out own cursor
      IF(DRAW) THEN
          ORG(1) = 0
          ORG(2) = 0
          WINDOW(1) = MAX(0,CURSXY(1) - CURSLN(3,CURNUM))
          WINDOW(2) = MAX(0,CURSXY(2) - CURSLN(4,CURNUM))
          WINDOW(3) = CURSLN(1,CURNUM)
          WINDOW(4) = CURSLN(2,CURNUM)
C         save portion under cursor
          CALL ROPREP()
          TCURSXY(1) = CURSXY(1)
          TCURSXY(2) = CURSXY(2)
          CALL BITMAP_COPY(0,CURSBT,WINDOW,ORG,ST)
          X = MAX(CURSXY(1),CURSLN(3,CURNUM))
          Y = MAX(CURSXY(2),CURSLN(4,CURNUM))
          CHR = CURSCH(CURNUM)
          FONT = CURSFN(CURNUM)
          CALL SET_FONT(FONT,ST)
C         draw tthe text character
          CALL TOOLS_TEXT(X,Y,CHR,INT2(1),ST)
      ELSE
C          IF(TCURSXY(1).NE.CURSXY(1).OR.TCURSXY(2).NE.CURSXY(2)) THEN
C            CURSXY(1) = TCURSXY(1)
C            CURSXY(2) = TCURSXY(2)
C            WRITE(*,*)'VALIN AND OUT ',TCURSXY,CURSXY
C          END IF
C         just recover portion under cursor
C          ORG(1) = MAX(0,CURSXY(1) - CURSLN(3,CURNUM))
C          ORG(2) = MAX(0,CURSXY(2) - CURSLN(4,CURNUM))
          ORG(1) = MAX(0,TCURSXY(1) - CURSLN(3,CURNUM))
          ORG(2) = MAX(0,TCURSXY(2) - CURSLN(4,CURNUM))
          WINDOW(1) = 0
          WINDOW(2) = 0
          WINDOW(3) = CURSLN(1,CURNUM)
          WINDOW(4) = CURSLN(2,CURNUM)
          CALL ROPREP()
          CALL BITMAP_COPY(CURSBT,0,WINDOW,ORG,ST)
      ENDIF
C
      END

         
      SUBROUTINE CURSOR_UPDATE(SCRPOS,ST)
C     ==================================
C1    VARYPE                     I2   I4
C1    IOSTAT                     I    O
C
C2    Updates the cursor postion
C  
      include    'include/cursor.inc'
      include    'include/daxcad_x.inc'
C
      INTEGER*2 SCRPOS(2),CURNUM
      INTEGER*4 ST,PATTERN
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,XP,YP,COL
      LOGICAL PNTBOX
C
      EXTERNAL PNTBOX
      ST = 0
C     global cursor check
      IF(.NOT.CURSON) THEN
C          update without drawing
	   IF ( .NOT.XVERSION ) THEN
               CALL TOOLS_SET_CURSOR(SCRPOS,ST)
           ENDIF
           CURSXY(1)= SCRPOS(1)
           CURSXY(2)= SCRPOS(2)
           GOTO 999
      ENDIF
      CURNUM = CURSTY
      IF(CURNUM.LT.0.OR.CURNUM.GT.MAXCUR) THEN
C         range check
          ST = 1
          GOTO 999
      ENDIF
      IF(CURNUM.EQ.0) THEN
C         update tools global cursor pos (SYSTEM)
          CURSXY(1)= SCRPOS(1)
          CURSXY(2)= SCRPOS(2)
C         set actual cursor pos
	  IF ( .NOT.XVERSION ) THEN
              CALL TOOLS_SET_CURSOR(SCRPOS,ST)
          ENDIF
          ST = 0
          GOTO 999
      ENDIF
C
      PATTERN = CURSNO(CURNUM)
C
C     raster ops
      CALL ROPXOR()
      COL = CURSCO(CURNUM)
      CALL TOOLPEN(COL)
      IF(PATTERN.EQ.CURSOR_XHAIR) THEN
C         simple cross hair pattern going across canvas
C         test box postion first
          XMIN = CURSLN(1,CURNUM)
          YMIN = CURSLN(2,CURNUM)
          XMAX = XMIN + CURSLN(3,CURNUM)
          YMAX = YMIN + CURSLN(4,CURNUM)
          XP = SCRPOS(1)
          YP = SCRPOS(2)
          IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)) THEN
C             update and draw new cursor into place
              CALL ROPXOR()
              CALL CURSOR_DRAW_XHAIR(ST)
              CURSXY(1)= SCRPOS(1)
              CURSXY(2)= SCRPOS(2)
              CALL CURSOR_DRAW_XHAIR(ST)
          ENDIF
      ELSEIF(PATTERN.EQ.CURSOR_CROSS) THEN
C         update and draw new cursor into place
          CALL ROPXOR()
          CALL CURSOR_DRAW_CROSS(ST)
          CURSXY(1)= SCRPOS(1)
          CURSXY(2)= SCRPOS(2)
          CALL CURSOR_DRAW_CROSS(ST)
      ELSEIF(PATTERN.EQ.CURSOR_SCROSS) THEN
C         update and draw new cursor into place
          CALL ROPXOR()
          CALL CURSOR_DRAW_SCROSS(ST)
          CURSXY(1)= SCRPOS(1)
          CURSXY(2)= SCRPOS(2)
          CALL CURSOR_DRAW_SCROSS(ST)
      ELSEIF(PATTERN.EQ.CURSOR_X) THEN
          CALL ROPXOR()
          CALL CURSOR_DRAW_X(ST)
          CURSXY(1)= SCRPOS(1)
          CURSXY(2)= SCRPOS(2)
          CALL CURSOR_DRAW_X(ST)
      ELSEIF(PATTERN.EQ.CURSOR_CHAR) THEN
C         uswer defined icon cursor pattern
C         tell repainting source that the cursor is off
          CURSON = .FALSE.
          CALL CURSOR_DRAW_CHAR(.FALSE.,ST)
          CURSXY(1)= SCRPOS(1)
          CURSXY(2)= SCRPOS(2)
C         repostion cursor
          CALL CURSOR_DRAW_CHAR(.TRUE.,ST)
          CURSON = .TRUE.
      ENDIF
      ST = 0
      RETURN
999   CONTINUE
      ST = 0
      END
C
      SUBROUTINE CURSOR_ON(VISAB)
C     ===========================
C1    VARYPE                 L
C1    IOSTAT                 O
C
C2    Swithces cursor on or off depending on logical arg.
C
      include    'include/apollo.inc'
      include    'include/cursor.inc'
      include    'include/daxcad_x.inc'
C
      INTEGER*4 ST
      INTEGER*4 CURNUM
      LOGICAL VISAB
C
C     global cursor check
      CURNUM = CURSTY
      IF ( XVERSION ) THEN
C        This cursor is going to be an X cursor
         CALL GPR_$SET_X_CURSOR(2,ST)
         CALL GPR_$SET_INPUT_FOCUS(ST)
      ENDIF
      IF(CURNUM.EQ.0) THEN
          IF((CURSON.AND..NOT.VISAB).OR.(.NOT.CURSON.AND.VISAB)) THEN
C             set system cursor active as required
              CALL TOOLS_SET_CURSOR(CURSXY,ST)
              CALL TOOLS_SYSTEM_CURSOR(VISAB,ST)
              CURSON = .NOT.CURSON
              RETURN
          ENDIF
      ENDIF
      IF((CURSON.AND..NOT.VISAB).OR.(.NOT.CURSON.AND.VISAB)) THEN
C         set user defined cursor thus set system curor off
          CALL TOOLS_SYSTEM_CURSOR(.FALSE.,ST)
C         set drwing mode and draw or erase the cursor
          CALL TOOLPEN(CURSCO(CURNUM))
C         draw the cursor now
C         switch cursor flag off first
          CURSON = .FALSE.
          CALL CURSOR_DRAW(CURNUM,VISAB,ST)
C         set flag afetr draw
          CURSON = VISAB
      ENDIF
      END
C
C
      SUBROUTINE NEW_CURSOR(CURNUM,ST)
C     ================================
C1    VARYPE                  I4   I4
C1    IOSTAT                  I    O
C
C2    This routine will draw in a new cursor
C2    or just set if not currently drawn
C2
C2            CUSTOMER AVAILABLE
C2
C  
C
      include    'include/cursor.inc'
C
      INTEGER*4 CURNUM,ST
C
      ST = 0
C     test type first
      IF(CURNUM.LT.0.OR.CURNUM.GT.MAXCUR) GOTO 999
C
      IF ( CURSON.AND.CURNUM.EQ.CURSTY ) THEN
          RETURN
      ENDIF
C     draw the cursor now 
      CALL CURDRW(CURNUM,ST)
      RETURN
999   ST = 1
      END
C
C
C
      SUBROUTINE INQ_CURSOR(CURNUM,POS,ST)
C     ====================================
C1    VARYPE                  I4   I4  I4
C1    IOSTAT                  O   O   O
C
C2    Returns current cursor id and postion
C2
C2
C2            CUSTOMER AVAILABLE
C2
C  
C
      include    'include/cursor.inc'
C
      INTEGER*4 CURNUM,ST,POS(2)
C
      CURNUM = CURSTY
      POS(1) = CURSXY(1)
      POS(2) = CURSXY(2)
      END
C
      SUBROUTINE INQ_CURSOR_STATE(STATE,ST)
C     ===================================
C1    VARYPE                       L   I4
C1    IOSTAT                       I   O
C
C2    returns cursor on/off state
C2
C2            CUSTOMER AVAILABLE
C2
C  
C
      include    'include/cursor.inc'
C
      LOGICAL STATE
      INTEGER*4 ST
C
      STATE = CURSON
      END
c
      SUBROUTINE CURSOR_MODIFY(POS,ST)
C     ================================
C1    VARYPE                   I4  I4
C1    IOSTAT                   I   O
C
C2    Modifies DAXTOOLS and current cursor positon
C  
      include    'include/cursor.inc'
C
      INTEGER*4 POS(2),ST
C
      CURSXY(1) = POS(1)
      CURSXY(2) = POS(2)

      CALL TOOLS_SET_CURSOR(CURSXY,ST)
C
C
      END
      




