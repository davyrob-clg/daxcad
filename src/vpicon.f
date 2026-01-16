
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 vpicon.f   */


      SUBROUTINE ICON_FIND(POS)
C     =========================
C1    VARTYPE               I4
C1    IOSTAT                O
C
C2    This routinw finds the first unused icon postion
C
      include  'include/viewport.inc'
      include  'include/icon.inc'
C
      INTEGER*4 POS,I
C
C     search along please
      DO 10 I=1,MAXVP
          IF(.NOT.ICPOS(I)) THEN
              POS = I
              RETURN
          ENDIF
 10   CONTINUE
C
      END
C
C       @(#)  256.1 date 12/16/89 icon_init.ftn 
      SUBROUTINE ICON_INIT()
C     ======================
C1    NO ARGS
C
C2    Inialise icon details positions so on so forth
C
      include  'include/viewport.inc'
      include  'include/icon.inc'
C
      INTEGER*2 I,SX,SY
C
C
      SX = VPXMAX - (60+10)*MAXVP-20
      SY = VPYMIN + 20
C
      DO 10 I=1,MAXVP
          ICPOS(I) = .FALSE.
          ICNUM(I) = .FALSE.
          ICONEX(1,I) = SX + (I-1)*(60+10)
          ICONEX(2,I) = SY
          ICONEX(3,I) = ICONEX(1,I) +60
          ICONEX(4,I) = SY +60
10    CONTINUE
C
C     set positions for icon on screen
C     each icon is 60 pixels wide
C
      END
C
C       @(#)  256.1 date 12/16/89 icon_paint.ftn 
      SUBROUTINE ICON_PAINT(NUM)
C     ==========================
C1    VARTPE                  I2
C1    IOSTAT                  I
C
C2    Paintsd in an icon of the requested number
C
      include  'include/gpr.ins.inc'
      include  'include/viewport.inc'
      include  'include/icon.inc'
      include  'include/apfont.inc'
      include  'include/daxcolor.inc'
      include  'include/daxcad_x.inc'
      include  'include/interface.inc'
C
      LOGICAL ACTIVE
      INTEGER*4 ST,I,OPT
      INTEGER*4 FORE
      INTEGER*4 BACK
      INTEGER*2 NUM,FONTID,CURPOS(2)
      INTEGER*2 RECT(4)
      INTEGER*2 X1,Y1,X2,Y2
      CHARACTER LOOKUP(5),CHR
      INTEGER*2 CURPEN
      CHARACTER*(*) STRING*32
      INTEGER*2 STRLEN
C
      FORE = COLFOR
      BACK = COLBAK
C
      ACTIVE = CVPN.EQ.NUM
      IF(NUM.LT.0) THEN
          NUM = ABS(NUM)
          CALL DRAW_BACK(NUM)
      ELSE
C         set text up to look active or not
          IF(.NOT.ACTIVE) THEN
              CALL TOOLPEN(FORE)
          ELSE
              CALL TOOLPEN(BACK)
          ENDIF
C
          CURPOS(1) = ICONEX(1,NUM)
          CURPOS(2) = ICONEX(2,NUM)
C
          RECT(1) = CURPOS(1)
          RECT(2) = CURPOS(2)
          RECT(3) = 60
          RECT(4) = 60
C
          X1 = RECT(1)
          Y1 = RECT(2)
          X2 = X1 + RECT(3) -1
          Y2 = Y1 + RECT(4) -1
C
          CALL TOOLPEN(BACK)
          CALL GPR_$RECTANGLE(RECT,ST)

          CALL TOOLPEN(FORE)
          CALL GPR_$DRAW_BOX(X1,Y1,X2,Y2,ST)

          RECT(1) = CURPOS(1)
          RECT(2) = CURPOS(2)
          RECT(3) = 60
          RECT(4) = 10
C
          CALL TOOLPEN(FORE)
          CALL GPR_$RECTANGLE(RECT,ST)

          CALL TOOLPEN(BACK)
          X1 = RECT(1) + 5
          Y1 = RECT(2) + 5
          X2 = X1 + 50
          Y2 = Y1
          CALL GPR_$MOVE(X1,Y1,ST)
          CALL GPR_$LINE(X2,Y2,ST)
C----------------------------------------
C  paint a label on it.  GCU 19/11/93

C     save current pen color
          CURPEN = COLPEN
          CALL SETDRW(COLFOR)
          CALL TOOLPEN_TEXT(FORE,BACK,.TRUE.,ST)

C     create string
          WRITE(UNIT=STRING,FMT='(A,I2)') 'VP', NUM
          STRLEN = NLEN(STRING)
C     get start positions for text
          X1 = RECT(1) + 30 - ((STRLEN*9)/2)
          Y1 = RECT(2) + 35
          CALL GPR_$MOVE(X1,Y1,ST)
          CALL GPR_$TEXT(STRING,STRLEN,ST)

C     reset color
          CALL SETDRW(CURPEN)
       
C-----------------------------------------
      ENDIF
      END
 
C      @(#)  256.1 date 12/16/89 icon_toggle.ftn 
      SUBROUTINE ICON_TOGGLE(NUM)
C     ===========================
C1    VARTPE                  I2
C1    IOSTAT                  I
C
C2    Routine will toggle icon into a window or vice versa
C
      include  'include/viewport.inc'
      include  'include/icon.inc'
      include  'include/wtov.inc'
C
      REAL WX1,WY1,WX2,WY2
      LOGICAL ICOPEN
      INTEGER*2 NUM,I
      INTEGER*4 ST,IX,IY
      INTEGER*2 LNUM
      INTEGER*2 LIST(2,MAXVP)
C
C     pop this window to the back of the list please
      CALL WINDOW_BACK(NUM)
      IF( ICNUM(NUM)) THEN
C         we must open our window do the necessary
          CALL ICON_VECTOR(NUM)
C         rub out this one please
          CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
          CALL WINDOW_PAINT(-NUM)
          DO 10 I=1,4
              VIEWEX(I,NUM) = ICONBF(I,NUM)
10        CONTINUE
          VIEWPO(1,NUM) = ICONPO(1,NUM)
          VIEWPO(2,NUM) = ICONPO(2,NUM)
C         copy in old data please taransforms remain the same
          VXMIN = VIEWEX(1,NUM)
          VYMIN = VIEWEX(2,NUM)+VPBANN
          VXMAX = VIEWEX(3,NUM)
          VYMAX = VIEWEX(4,NUM)
C         no longer an icon
          ICNUM(NUM) = .FALSE.
C         paint in the opened window
C         paint anything else involved
          DO 30 I=2,LNUM
              CALL WINDOW_PAINT(LIST(2,I))
 30       CONTINUE
C         make sure window is active before hand
          CALL WINDOW_ACTIVE()
          CALL WINDOW_PAINT(NUM)
          CALL WINDOW_ADD(NUM)
C         recove world transform please
          WX1 = VIEWPS(1,NUM+1)
          WY1 = VIEWPS(2,NUM+1)
          WX2 = VIEWPS(3,NUM+1)
          WY2 = VIEWPS(4,NUM+1)
          CALL WORLD(WX1,WY1,WX2,WY2)
C         save new world transform
          CALL TRNSAV(NUM)
      ELSE
C         rub out this one please
          DO 25 I=1,4
              ICONBF(I,NUM) = VIEWEX(I,NUM)
25        CONTINUE
C
          ICONPO(1,NUM) = VIEWPO(1,NUM)
          ICONPO(2,NUM) = VIEWPO(2,NUM)
C
          CALL ICON_VECTOR(NUM)
C         build a new point list and paint out old area
          CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
          CALL WINDOW_PAINT(-NUM)
C         copy in old data please taransforms remain the same
          DO 20 I=1,4
              VIEWEX(I,NUM) = ICONEX(I,NUM)
20        CONTINUE
          VIEWPO(1,NUM) = ICONEX(3,NUM)-ICONEX(1,NUM)
          VIEWPO(2,NUM) = ICONEX(4,NUM)-ICONEX(2,NUM)
          VXMIN = ICONEX(1,NUM)
          VYMIN = ICONEX(2,NUM)
          VXMAX = ICONEX(3,NUM)
          VYMAX = ICONEX(4,NUM)
C         rub out this one please
          ICNUM(NUM) = .TRUE.
C         add window
          DO 50 I=2,LNUM
              CALL WINDOW_PAINT(LIST(2,I))
 50       CONTINUE
C         set this on down please
          IX = 0
          IY = 0
          CALL WINDOW_SET(IX,IY,.TRUE.)
          CALL WINDOW_PAINT(NUM)
          CALL WINDOW_ADD(NUM)
      ENDIF
C     keep every thing together in the new window
      DO 40 I=1,MAXVP
C         swapping list if window is unobscured
          IF(VPNUM(I).AND.I.NE.NUM) THEN
              OBSLST(NUM,I) = .FALSE.
          ENDIF
 40   CONTINUE
C
      END
 
 
C       @(#)  256.1 date 12/16/89 icon_vector.ftn 
      SUBROUTINE ICON_VECTOR(NUM)
C     ===========================
C1    VARTPE                  I2
C1    IOSTAT                  I
C
C2    routine will draw the vectors from each corner.
C2    dead naff actually. Not stolen from the suns.........
C2    ok so I admit it. well who cares anyway?? everybody does it
C2    look theres no need for that kine of language....what...right...PISS OFF
C
      include  'include/viewport.inc'
      include  'include/icon.inc'
C
      INTEGER*4 ST,I
      INTEGER*2 NUM
C
      CALL ROPXOR()
      DO 10 I=1,2
      CALL GPR_$MOVE(ICONEX(1,NUM),ICONEX(2,NUM),ST)
      CALL GPR_$LINE(ICONBF(1,NUM),ICONBF(2,NUM),ST)
C
      CALL GPR_$MOVE(ICONEX(3,NUM),ICONEX(2,NUM),ST)
      CALL GPR_$LINE(ICONBF(3,NUM),ICONBF(2,NUM),ST)
C
      CALL GPR_$MOVE(ICONEX(3,NUM),ICONEX(4,NUM),ST)
      CALL GPR_$LINE(ICONBF(3,NUM),ICONBF(4,NUM),ST)
C
      CALL GPR_$MOVE(ICONEX(1,NUM),ICONEX(4,NUM),ST)
      CALL GPR_$LINE(ICONBF(1,NUM),ICONBF(4,NUM),ST)
 
      CALL DGWAIT(0.25)
10    CONTINUE
      CALL ROPREP()
C
C
      END
C       @(#)  256.1 date 12/16/89 window_back.ftn 
      SUBROUTINE WINDOW_BACK(NUM)
C     ==========================
C1    VARTPE                  I2
C1    IOSTAT                  I
C
C2    makes a window go to the back of the list if obscured
C
      include  'include/viewport.inc'
C
      INTEGER*4 I
      INTEGER*2 NUM
      LOGICAL TEMP
 
      DO 10 I=1,MAXVP
C         swapping list if window is unobscured
          IF(VPNUM(I).AND.I.NE.NUM) THEN
C             if the window is allready obs then leave it
              IF(OBSLST(I,NUM)) THEN
                 TEMP = OBSLST(I,NUM)
                 OBSLST(I,NUM) =OBSLST(NUM,I)
                 OBSLST(NUM,I) =TEMP
              ENDIF
           ENDIF
 10   CONTINUE
      END
 
