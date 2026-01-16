C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 grflib1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION  ARCTPT(XC,YC,SANG,EANG,X,Y)
C     FUNCTION  CHKLN(XV1,XV2,X)
C     FUNCTION CLKWS(XC,YC,BX,BY,TX,TY)
C     FUNCTION DCLKWS(XC,YC,BX,BY,TX,TY)
C     FUNCTION DDEG(RAD1)
C     FUNCTION DEG(RAD1)
C     FUNCTION   DRAD(DEG1)
C     FUNCTION   RAD(DEG1)
C     FUNCTION RCLKWS(XC,YC,BX,BY,TX,TY)
C     FUNCTION SETCLP(VAL,MIN,MAX)
C     FUNCTION   ZERO (VAL)
C     SUBROUTINE ARCAUX(MIP,AX1,AX2,OK)
C     SUBROUTINE ARCBOX(XC,YC,RADIUS,SANG,EANG,XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE  ARCCHK(XC,YC,RADIUS,SANG,EANG,TOTAL,OK)
C     SUBROUTINE   CHKBOX(XMIN,YMIN,XMAX,YMAX,TOTAL,OK)
C     SUBROUTINE CLIP(X1,Y1,X2,Y2,VISAB)
C     SUBROUTINE DCROSS(XI,YI)
C     SUBROUTINE DRAWLW(X1,Y1,X2,Y2)
C     SUBROUTINE ECROSS(XI,YI)
C     SUBROUTINE HFONT(HARDF)
C     SUBROUTINE I2SWAP(X,Y)
C     SUBROUTINE I4SWAP(X,Y)
C     SUBROUTINE  NEREST(CXP,CYP,X1,Y1,X2,Y2,X3,Y3,X4,Y4,ST,XP,YP,ST1)
C     SUBROUTINE NEWPNT(CX,CY,MAJRAD,MINRAD,INCANG,ANG,XP,YP)
C     SUBROUTINE NEWPT1(CX,CY,MAJRAD,ANG,XP,YP)
C     SUBROUTINE ROTP2D(X,Y,THETA,A)
C     SUBROUTINE RSWAP(X,Y)
C     SUBROUTINE SCAP2D(X,Y,SX,SY,A)
C     SUBROUTINE  SRT3PT(X1,Y1,X2,Y2,X3,Y3,CENX,CENY,SANG,EANG)
C     SUBROUTINE  SRTAR4(CENX,CENY,CXP,CYP,CXP1,CYP1,X1,Y1,X2,Y2,X3,Y3,
C     SUBROUTINE STOWVX(V,W)
C     SUBROUTINE SWINDO(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE WTOSVX(W,V)
C     SUBROUTINE WTOSVY(W,V)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ARCAUX(MIP,AX1,AX2,OK)
C     =================================
C1    iotype            I2   R   R   L
c1    iostat            I    O   O   O
C
C2    This routine generates any auxilary  data 
C2    for an arc or ellipse from the master index pointer
C2    passed to it.
      include   'include/nbuff.inc'
      include   'include/entity.inc'
C
      REAL AX1,AX2,RARAY1(6),RARAY2(6)
      INTEGER*2 TBUFF1(4),TBUFF2(4),MIP,TPNT
      LOGICAL OK
C
      CALL DIR500(MIP,OK)
      TPNT = IMBUFF(7)
      CALL DBR550(TPNT,TBUFF1,RARAY1,OK)
C     Check to see if it is an arc
      IF(IMBUFF(2).EQ.ARC) THEN
c        min rad = maj rad
         AX1 = RARAY1(4)      
c        inc angle  = 0.0
         AX2 = 0.0     
      ELSE
         TPNT = TBUFF1(3)
         CALL DBR550(TPNT,TBUFF2,RARAY2,OK)
c        min rad read from database
         AX1 = RARAY2(1)      
c        inc angle read from database
         AX2 = RARAY2(2)      
      END IF

      END
      SUBROUTINE ARCBOX(XC,YC,RADIUS,SANG,EANG,XMIN,YMIN,XMAX,YMAX)
C     =============================================================
C1                       R, R,     R,   R,   R,   R,   R,   R,   R
C1                       I, I,     I,   I,   I,   O,   O,   O,   O
C
C2       ARCBOX returns the enclosing box of the arc XC,YC,RADIUS
C
      REAL XC,YC,RADIUS,SANG,EANG,XMIN,YMIN,XMAX,YMAX,TPX,TPY
      LOGICAL ARCTPT
      INTRINSIC SIN,COS
      EXTERNAL RSWAP,ARCTPT
C
      XMIN=XC+RADIUS*COS(SANG)
      YMIN=YC+RADIUS*SIN(SANG)
C
      XMAX=XC+RADIUS*COS(EANG)
      YMAX=YC+RADIUS*SIN(EANG)
C
      IF ( XMAX .LT. XMIN ) CALL RSWAP(XMIN,XMAX)
      IF ( YMAX .LT. YMIN ) CALL RSWAP(YMIN,YMAX)
C
      TPY=YC-RADIUS
      TPX=XC
C
      IF ( ARCTPT(XC,YC,SANG,EANG,TPX,TPY)) YMIN=TPY
C
      TPY=YC+RADIUS
C      TPX=XC
C
      IF ( ARCTPT(XC,YC,SANG,EANG,TPX,TPY)) YMAX=TPY
C
      TPY=YC
      TPX=XC-RADIUS
C
      IF ( ARCTPT(XC,YC,SANG,EANG,TPX,TPY)) XMIN=TPX
C
C      TPY=YC
      TPX=XC+RADIUS
C
      IF ( ARCTPT(XC,YC,SANG,EANG,TPX,TPY)) XMAX=TPX
C
      END
C
C
      SUBROUTINE  ARCCHK(XC,YC,RADIUS,SANG,EANG,TOTAL,OK)
C     ===================================================
C
      LOGICAL OK
      INTEGER TOTAL
      REAL XC,YC,RADIUS,SANG,EANG,XMIN,YMIN,XMAX,YMAX
      EXTERNAL ARCBOX,CHKBOX
C
      CALL ARCBOX(XC,YC,RADIUS,SANG,EANG,XMIN,YMIN,XMAX,YMAX)
      CALL CHKBOX(XMIN,YMIN,XMAX,YMAX,TOTAL,OK)
C
      END
C
C
C
      FUNCTION  ARCTPT(XC,YC,SANG,EANG,X,Y)
C     =====================================
C
C1    vartype      L   R  R   R    R   R R
C1    iostatus     O   I  I   I    I   I I
C1
C2    Check that the hit point lies within the
C2    enclosing angle of the arc
C
      LOGICAL ARCTPT,SAME
      REAL SANG,EANG,CANG,ANG,X,Y,XC,YC,PI
      EXTERNAL CANG,PI
C
      ANG=CANG(XC,YC,X,Y)
 
      IF(SAME(EANG,PI(2.0)).AND.SAME(ANG,0.0)  ) ANG = PI(2.0)
C
      ARCTPT=SAME(ANG,SANG).OR.
     +       SAME(ANG,EANG)
C
      IF ( ARCTPT ) RETURN
C
      IF ( SANG .GT. EANG ) THEN
C      Passes thro' the Origin a damned nuisance I say
          ARCTPT=ANG.GE.SANG.OR.ANG.LE.EANG
      ELSE
          ARCTPT=ANG.GT.SANG.AND.ANG.LT.EANG
      END IF
C
      END
C
*
      SUBROUTINE   CHKBOX(XMIN,YMIN,XMAX,YMAX,TOTAL,OK)
C     =================================================
C1                         R,   R,   R,   R     I    L
C1                         I,   I,   I,   I     O    O
C1     (XMIN,YMIN) Bottom left hand corner
C1     (XMAX,YMAX) Top   right   "    "
C2      OK is set to .TRUE. if any of the box
C2      defined is showing in the current viewport
c2 ****** Note total changed to I*4 14/7/88 ********************
C2
C2      TOTAL is set 0 if all the box is totally surrounded
C2      by the current viewport
C
C2      TOTAL is set 1 if the box  totally surrounds
C2      the current viewport
C
C
C2      TOTAL is set 2 if the box is partially visible
C2     
C
 
      LOGICAL OK
      INTEGER TOTAL
      REAL XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2
C
      include  'include/wtov.inc'
C
      EXTERNAL CLIP
C
C
C     Box totally surrounded by the viewport.
      OK= XMAX .LE. WXMAX .AND.
     A    XMIN .GE. WXMIN .AND.
     B    YMAX .LE. WYMAX .AND.
     C    YMIN .GE. WYMIN
C
      TOTAL = 0
      IF ( OK ) RETURN
 
C       Box totally surrounds the viewport
      OK=XMAX .GT. WXMAX .AND.
     A      XMIN .LT. WXMIN .AND.
     B      YMAX .GT. WYMAX .AND.
     C      YMIN .LT. WYMIN
C
      TOTAL = 1
      IF (OK) RETURN
       TOTAL = 2
C      IF ( TOTAL ) RETURN
C     Checking the bottom line of box for
C     crossing viewport
      X1=XMIN
      Y1=YMIN
      X2=XMAX
      Y2=YMIN
C
      CALL CLIP(X1,Y1,X2,Y2,OK)
C
      IF ( OK ) RETURN
C
C     Checking the right line of box for
C     crossing viewport
      X1=XMAX
      Y1=YMIN
      X2=XMAX
      Y2=YMAX
C
      CALL CLIP(X1,Y1,X2,Y2,OK)
C
      IF ( OK ) RETURN
C
C     Checking the top line of box for
C     crossing viewport
      X1=XMAX
      Y1=YMAX
      X2=XMIN
      Y2=YMAX
C
      CALL CLIP(X1,Y1,X2,Y2,OK)
C
      IF ( OK ) RETURN
C
C     Checking the left line of box for
C     crossing viewport
      X1=XMIN
      Y1=YMAX
      X2=XMIN
      Y2=YMIN
C
      CALL CLIP(X1,Y1,X2,Y2,OK)
C
C       If get this far box totally outside current viewport
C
C
      END
C
C
      FUNCTION  CHKLN(XV1,XV2,X)
C     ==========================
C
C1                       R,  R,R
C1                       I,  I,I
C
C2       CHKLN returns .true. if X lies
C2       between XV1 and XV2
C
      LOGICAL CHKLN,SAME
      REAL  XV1,XV2,X
      EXTERNAL SAME
C
      CHKLN=SAME(X,XV1).OR.SAME(X,XV2)
C
      IF ( CHKLN ) RETURN
C
      IF ( XV2 .GT. XV1 ) THEN
         CHKLN=X.GE.XV1.AND.X.LE.XV2
      ELSE
         CHKLN=X.GE.XV2.AND.X.LE.XV1
      END IF
C
      END
C
C
      SUBROUTINE CLIP(X1,Y1,X2,Y2,VISAB)
C     ==================================
C1                     R, R, R, R,    L
C1                     B, B, B, B,    O
C2       Subroutine CLIP checks whether the line between
C2       X1,Y1 and X2,Y2 invisible on the cuurrent viewport
C2         If it is then VISAB will be set .TRUE. else it
C2       will be set at .FALSE.
C
      LOGICAL VISAB,FPOINT
      INTEGER IX1,IX2,IY1,IY2,IX,IY,CEN,LEFT,RIGHT,TOP,BOTTOM,SETCLP
C
      PARAMETER(CEN=0,LEFT=-1,BOTTOM=-1,RIGHT=1,TOP=1)
C
      REAL X,Y,X1,X2,Y1,Y2,A,B,C,D,VAR,NEW
C
      include  'include/wtov.inc'
C
      EXTERNAL SETCLP
C
C        Linear interpolation eqution used for clipping
C        lines to limit of viewport
C
      NEW(A,B,C,D,VAR)=A+((B-A)*(VAR-C)/(D-C))
C
C       Check position of points on viewport
C        X coordinate       -1 off left
C                            0 centre
C                           +1 off right
C        Y coordinate       -1 off bottom
C                            0 centre
C                           +1 off top
C
      IX1=SETCLP(X1,WXMIN,WXMAX)
      IY1=SETCLP(Y1,WYMIN,WYMAX)
      IX2=SETCLP(X2,WXMIN,WXMAX)
      IY2=SETCLP(Y2,WYMIN,WYMAX)
C
C       Indicates if line exists on the screen
      VISAB=.FALSE.
C
 10   IF((IX1*IX2).EQ.1 .OR. (IY1*IY2).EQ.1 )  RETURN
C
C     If Fpoint .true. then first point is inside window
      FPOINT = IX1.EQ.CEN .AND. IY1.EQ.CEN
C
C     If VISAB .true. then both end points are inside window
      VISAB  = FPOINT .AND. IX2.EQ.CEN .AND. IY2.EQ.CEN
C
      IF ( VISAB ) RETURN
C
      IF ( FPOINT ) THEN
C
C       Start with first point of line
         IX=IX2
         IY=IY2
      ELSE
C
C       If first point totally inside viewport start
C       on second point
         IX=IX1
         IY=IY1
C
      END IF
C
      IF ( IX .EQ. LEFT ) THEN
C
C       If X coordinate off left of window interpolate to
C       left edge of viewport X coordinate = left boundary
         Y=NEW(Y1,Y2,X1,X2,WXMIN)
         X=WXMIN
C
      ELSE IF ( IX .EQ. RIGHT ) THEN
C
C       If X coordinate off right of window interpolate to
C       right edge of viewport X coordinate = right boundary
         Y=NEW(Y1,Y2,X1,X2,WXMAX )
         X=WXMAX
C
      ELSE IF ( IY .EQ. TOP ) THEN
C
C       If Y coordinate off top of window interpolate to
C       top edge of viewport Y coordinate = top boundary
         X=NEW(X1,X2,Y1,Y2,WYMAX)
         Y=WYMAX
C
      ELSE IF ( IY .EQ. BOTTOM ) THEN
C
C       If Y coordinate off bottom of window interpolate to
C       bottom edge of viewport Y coordinate = bottom boundary
         X=NEW(X1,X2,Y1,Y2,WYMIN)
         Y=WYMIN
C
      END IF
C
      IF ( IX .EQ. IX1 .AND. IY .EQ. IY1 ) THEN
C
C       If first point check transfer new points to X1,Y1
         X1=X
         Y1=Y
         IX1=SETCLP(X1,WXMIN,WXMAX)
         IY1=SETCLP(Y1,WYMIN,WYMAX)
C
      ELSE
C
C       If second point check transfer new points to X2,Y2
         X2=X
         Y2=Y
         IX2=SETCLP(X2,WXMIN,WXMAX)
         IY2=SETCLP(Y2,WYMIN,WYMAX)
C
      END IF
C
      GOTO 10
C
      END
*
      FUNCTION CLKWS(XC,YC,BX,BY,TX,TY)
C     =================================
C1       L             R, R, R, R, R, R
C
      REAL  XC,YC,BX,BY,TX,TY,
     +     L1,L2,L3,VD0D13,DIST
      LOGICAL CLKWS
      EXTERNAL VD0D13,CV0L14
C
      CALL CV0L14(XC,YC,TX,TY,L1,L2,L3)
      DIST=VD0D13(L1,L2,L3,BX,BY)
      CLKWS= DIST .GE. 0.0
C
      END
*
      FUNCTION DCLKWS(XC,YC,BX,BY,TX,TY)
C     ==================================
C1       L             D, D, D, D, D, D
C
C
      DOUBLE PRECISION XC,YC,BX,BY,TX,TY,
     +                 L1,L2,L3,DVDD13,DIST
      LOGICAL DCLKWS
      EXTERNAL DVDD13,DCVL14
C
 
      CALL DCVL14(XC,YC,TX,TY,L1,L2,L3)
      DIST=DVDD13(L1,L2,L3,BX,BY)
      DCLKWS= DIST .GE. 0.0
C
      END
*
      SUBROUTINE DCROSS(XI,YI)
C     =====================
C1                       R,R
C1                       I,I
      include 'include/wtov.inc'
      include 'include/dcross.inc' 
      include 'include/viewport.inc' 
C
      INTEGER*2 D1
      REAL SIZE,X,Y,X1,Y1,X2,Y2,XI,YI
      LOGICAL OK,HARDF
      EXTERNAL DRAWLS,WRKHIT,ROPREP
      DATA SIZE/10.0/
C
C     if scrosses are switched off, return.
      IF (.NOT.DCROS) RETURN
C
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set viewporting draw code please
          DDCODE = 2
      ENDIF
      CALL WO2SC(XI,YI,X,Y)
      CALL WRKHIT(X,Y,OK)
      IF ( .NOT. OK ) RETURN
      X1=MAX(VXMIN,X-SIZE)
      X2=MIN(VXMAX,X+SIZE)
      Y1=MAX(VYMIN,Y)
      Y2=MIN(VYMAX,Y)
      CALL DRAWLS(X1,Y1,X2,Y2)
      X1=MAX(VXMIN,X)
      X2=MIN(VXMAX,X)
      Y1=MAX(VYMIN,Y-SIZE)
      Y2=MIN(VYMAX,Y+SIZE)
      CALL DRAWLS(X1,Y1,X2,Y2)
10    CONTINUE
C     reset drawing code
      DDCODE = 0
C
      END
 
      FUNCTION DDEG(RAD1)
C     ==================
C1       R          R
C1       O          I
C2      Function DEG converts the value of RAD1 (radians)
C2      to degrees
C
      DOUBLE PRECISION DDEG,C1,RAD1,DPI
      EXTERNAL DPI
C
C     C1 is 180 Divided by PI degrees
      C1=180.0/DPI(1.0D0)
      DDEG=RAD1*C1
C
      END
C
C
      FUNCTION DEG(RAD1)
C     ==================
C1       R          R
C1       O          I
C2      Function DEG converts the value of RAD1 (radians)
C2      to degrees
C
      REAL DEG,C1,RAD1,PI
      EXTERNAL PI
C
C     C1 is 180 Divided by PI degrees
      C1=180.0/PI(1.0)
      DEG=RAD1*C1
C
      END
C
C
      FUNCTION   DRAD(DEG1)
C     ====================
C1       R            R
C1       O            I
C2      Function RAD converts the value of DEG1 (degrees)
C2      to Radians
C
      DOUBLE PRECISION DRAD,C1,DEG1,DPI
      EXTERNAL DPI
C
C     C1 is Pi divided by 180 degrees
      C1=DPI(1.0D0)/180.0
      DRAD=DEG1*C1
C
      END
C
C
      SUBROUTINE DRAWLW(X1,Y1,X2,Y2)
C     ==============================
C1                       R, R, R, R
C1                       I, I, I, I
C2      Subroutine DRAWLW draws a line between (X1,Y1) & (X2,Y2)
C2      if it exists on the current viewport
C2      X1,Y1,X2,Y2 are in world coordinates
C
      include  'include/masti.inc'
      include  'include/curpos.inc'
C
      REAL  X1,Y1,X2,Y2,
     A      WX1,WY1,WX2,WY2,
     B     VX1,VY1,VX2,VY2
C
      INTRINSIC ABS
C
      EXTERNAL CLIP,WO2SC,DRAWLS
C
C     Transfer of values so that the input values are not
C     corrupted by clipping subroutine
      WX1=X1
      WY1=Y1
      WX2=X2
      WY2=Y2
C
C     Clipping routine will supply new line to be drawn if
C     visible if line not visible logical variable DISPV
C     will be set to .false.
      CALL CLIP(WX1,WY1,WX2,WY2,DISPV)
      IF( DISPV ) THEN
C        display file
C        line still in world coordinates have to convert to
C        screen coordinates
         CALL WO2SC(WX1,WY1,VX1,VY1)
         CALL WO2SC(WX2,WY2,VX2,VY2)
C        once converted draw line on screen
         CALL DRAWLS(VX1,VY1,VX2,VY2)
      END IF
C
      WCPX=X2
      WCPY=Y2
C
      END
C
      SUBROUTINE ECROSS(XI,YI)
C     =====================
C1                       R,R
C1                       I,I
      include 'include/wtov.inc'
      REAL XI,YI
C
      CALL PENERS()
      CALL DCROSS(XI,YI)
      CALL PENDRW()
      END
 
      SUBROUTINE HFONT(HARDF)
C     =====================
C1    VARTYPE            L
C1    IOSTATUS           I
C
C2    This routine sets the hardware font status
C2    whether true or false
C
      include 'include/wtov.inc'
      INTEGER*2 D1
      LOGICAL HARDF
      EXTERNAL HARDWF,ROPREP,ROPXOR
C
      IF(HARDF) THEN
          D1=2
          CALL HARDWF(D1)
      ELSE
          D1=0
          CALL HARDWF(D1)
      ENDIF
      END
 
      SUBROUTINE I2SWAP(X,Y)
C     =====================
C1                     R,R
C1                     B,B
C2      Subroutine RSWAP swaps the values of X and Y
C2      round
C
      INTEGER*2 X,Y,T
C
      T=X
      X=Y
      Y=T
      END
C
      SUBROUTINE I4SWAP(X,Y)
C     =====================
C1                     R,R
C1                     B,B
C2      Subroutine RSWAP swaps the values of X and Y
C2      round
C
      INTEGER*4 X,Y,T
C
      T=X
      X=Y
      Y=T
      END
C
C
C
      SUBROUTINE  NEREST(CXP,CYP,X1,Y1,X2,Y2,X3,Y3,X4,Y4,ST,XP,YP,ST1)
C     ===============================================================
C1    IOTYPE              R   R  R  R  R  R  R  R  R  R  I  R  R  I
C1    IOSTAT              I   I  I  I  I  I  I  I  I  I  I  O  O  O
C2    This routine is used to find the closest intersection point
C2    to the point CXP,CYP.   The number of points is ST and the total
C2    number of points is 4
      REAL CXP,CYP,X1,Y1,X2,Y2,X3,Y3,X4,Y4,XP,YP,DISTXY
      REAL RVAL(4,2),DISVAL(4),MINVAL
      INTEGER ST,ST1,I
      EXTERNAL DISTXY
       
      RVAL(1,1) = X1
      RVAL(1,2) = Y1

      RVAL(2,1) = X2
      RVAL(2,2) = Y2

      RVAL(3,1) = X3
      RVAL(3,2) = Y3

      RVAL(4,1) = X4
      RVAL(4,2) = Y4

      MINVAL = DISTXY(CXP,CYP,RVAL(1,1),RVAL(1,2))
      ST1 = 1
      DO 10 I = 1,ST
         DISVAL(I)=DISTXY(CXP,CYP,RVAL(I,1),RVAL(I,2))
         IF(MIN(DISVAL(I),MINVAL).NE.MINVAL) THEN
            MINVAL = DISVAL(I)
            ST1 = I
         END IF
  10  CONTINUE

      XP = RVAL(ST1,1)
      YP = RVAL(ST1,2)
      END
      SUBROUTINE NEWPNT(CX,CY,MAJRAD,MINRAD,INCANG,ANG,XP,YP)
C     ========================================================
C1                      R    L   R(3,3)
C1                      I    I    I 
C                                                                   
C2    Subroutine DRELL draws the ellips from its start angle to its
C2    end angle.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'

      REAL CX,CY,MAJRAD,MINRAD,STRANG,ENDANG,INCANG,RARAY1(6),PI,XP,YP
      REAL AINC,ANG,X,Y,OX,OY,X1,Y1,TX,TY,RAD,ANGLE,COUNT,SRAD,RADS
      DOUBLE PRECISION DX,DY,DX1,DY1,DINC
      INTEGER*2 TMIP,TTMIP,IBUF1(4)
      LOGICAL USED,OK ,FIRST
      REAL M(3,3)

      EXTERNAL RAD,PI
        
                                       
      STRANG = 0.0              
      ENDANG = PI(2.0)            
C     Handle any rotation in the ellipse 
      X = CX + COS(ANG) * MAJRAD
      Y = CY + SIN(ANG) * MINRAD
      IF (INCANG .NE. 0.0) THEN
         DX1 = DBLE(CX)
         DY1 = DBLE(CY)
         DINC = DBLE(INCANG)
         CALL CLC2DT(DX1,DY1,DINC)
C        Now rotate the complete x,y co-ords  
         DX= DBLE(X)
         DY= DBLE(Y)
         CALL ROT2DF(DX,DY,DX1,DY1)
         X = REAL(DX1)
         Y = REAL(DY1)
      END IF
      XP = X
      YP = Y

      END
C
      SUBROUTINE NEWPT1(CX,CY,MAJRAD,ANG,XP,YP)
C     ========================================================
C1                      R    L   R(3,3)
C1                      I    I    I 
C                                                                   
C2    Subroutine DRELL draws the ellips from its start angle to its
C2    end angle.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'

      REAL CX,CY,MAJRAD,XP,YP,ANG
      DOUBLE PRECISION DCX,DCY,DMAJ,DANG,DXP,DYP   
               
      DCX= DBLE(CX)
      DCY= DBLE(CY)
      DANG = DBLE(ANG)
      DMAJ = DBLE(MAJRAD)
                                       
C     Handle any rotation in the ellipse 
      DXP = DCX + DCOS(DANG) * DMAJ
      DYP = DCY + DSIN(DANG) * DMAJ
                                   
      XP = REAL(DXP)
      YP = REAL(DYP)
      END
      FUNCTION   RAD(DEG1)
C     ====================
C1       R            R
C1       O            I
C2      Function RAD converts the value of DEG1 (degrees)
C2      to Radians
C
      REAL RAD,C1,DEG1,PI
      EXTERNAL PI
C
C     C1 is Pi divided by 180 degrees
      C1=PI(1.0)/180.0
      RAD=DEG1*C1
C
      END
C
C
      FUNCTION RCLKWS(XC,YC,BX,BY,TX,TY)
C     ==================================
C1       L             R, R, R, R, R, R
C
C
      REAL XC,YC,BX,BY,TX,TY,VD0D13,
     +     L1,L2,L3,DIST
      LOGICAL RCLKWS
      EXTERNAL VD0D13,CV0L14
C
C
      CALL CV0L14(XC,YC,TX,TY,L1,L2,L3)
C
      DIST=VD0D13(L1,L2,L3,BX,BY)
C
      RCLKWS= DIST .GE. 0.0
C
      END
C
C
      SUBROUTINE ROTP2D(X,Y,THETA,A)
C     ==============================
C1                      R,R,  R  ,R(3*3)
C1                      I,I,  I  ,O
C2       Subroutine ROTP2D sets A the 3*3 matrice to rotate
C2       a point by the value of theta
C
      REAL X,Y,THETA,A(3,3),B(3,3),C(3,3)
      EXTERNAL TRAN2D,ROT2D,MULT3M
C        translates back to origin 0,0
      CALL TRAN2D(-X,-Y,A)
C        rotate about 0,0
      CALL ROT2D(THETA,B)
      CALL MULT3M(A,B,C)
C        translate back to original position X,Y
      CALL TRAN2D(X,Y,B)
      CALL MULT3M(C,B,A)
      END
C
C
      SUBROUTINE RSWAP(X,Y)
C     =====================
C1                     R,R
C1                     B,B
C2      Subroutine RSWAP swaps the values of X and Y
C2      round
C
      REAL X,Y,T
C
      T=X
      X=Y
      Y=T
C
      END
C
      SUBROUTINE SCAP2D(X,Y,SX,SY,A)
C     ==============================
C1                      R,R, R, R,R(3,3)
C1                      I,I, I, I,O
C
C2       Subroutine SCAP2D scales a point about another
C2       point given by X,Y and the ratios by SX,SY
C
      REAL X,Y,SX,SY,A(3,3),B(3,3),C(3,3)
      EXTERNAL SCAL2D,MULT3M,TRAN2D
C       translate back to 0,0
      CALL TRAN2D(-X,-Y,A)
C        scale from 0,0
      CALL SCAL2D(SX,SY,B)
C       concatenate the two matrices
      CALL MULT3M(A,B,C)
C       translate back to original position X,Y
      CALL TRAN2D(X,Y,B)
C       concatenate again result being one matrice with
C       all three transformation in it
      CALL MULT3M(C,B,A)
      END
C
C
      FUNCTION SETCLP(VAL,MIN,MAX)
C     ==============================
C1       I               R , R , R
C1       O               I , I , I
C
C2        Function SETCLP is set according to
C2        the following conditions :
C2
C2         -1 if VAL < MIN
C2          0 if VAL >= MIN .or. VAL <= MAX
C2         +1 if VAL > MAX
C2
      REAL VAL,MIN,MAX
      INTEGER SETCLP
C
      IF( VAL .LT. MIN ) THEN
         SETCLP=-1
      ELSE IF( VAL .GT. MAX ) THEN
         SETCLP=1
      ELSE
         SETCLP=0
      END IF
C
      END
C
C
      SUBROUTINE  SRT3PT(X1,Y1,X2,Y2,X3,Y3,CENX,CENY,SANG,EANG)
C     ===============================================================
C1    IOTYPE              R   R  R  R  R  R  R  R  R  R  I  R  R  I
C1    IOSTAT              I   I  I  I  I  I  I  I  I  I  I  O  O  O
C2    This routine is used to find the start and eng angle
C2     This is calculated from two intersection points and one hit point
      REAL X1,Y1,X2,Y2,X3,Y3,CENX,CENY
      REAL RVAL(3),RVAL1(3),CANG,PI,SANG,EANG,TX1
      INTEGER ST1,ST,STMP(3),STMP1(3),I,TI1,STP,STPIND
      LOGICAL SWAP,DIRECT
                     
      EXTERNAL CANG,PI
      ST = 2
               
c     set the pivit point around which to sort to 1
      STP = 1
c     store in arrays the angle and relative position
c     NOTE rval is sorted and rval1 is static
C     ---------------------------------------
      RVAL(1) = CANG(CENX,CENY,X1,Y1)
      RVAL1(1) = RVAL(1)
      STMP(1) = 1


      RVAL(2) = CANG(CENX,CENY,X3,Y3)
      RVAL1(2) = RVAL(2)
      STMP(2) = 2

      RVAL(3) = CANG(CENX,CENY,X2,Y2)
      RVAL1(3) = RVAL(3)
      STMP(3) = 3

c     Do a quick bubble sort of the angles about 0 degrees
c     ===================================================
   5  CONTINUE
                      
      SWAP = .FALSE.

      DO 10 I = 1,ST
         IF(RVAL(I).GT.RVAL(I+1) ) THEN
            TX1 = RVAL(I)
            RVAL(I) = RVAL (I+1)
            RVAL(I+1) = TX1
            TI1 = STMP(I)
            STMP(I) = STMP(I+1)
            STMP(I+1) = TI1
            SWAP = .TRUE.
         END IF
 10   CONTINUE   
      IF(SWAP ) GOTO 5
c     ===================================================
                      
c     now let us find out where the pivit point was
      DO 20 I = 1,ST+1
         IF( STMP(I) .EQ.STP) STPIND = I-2
 20   CONTINUE   

c     ************** Now let us sort according to the pivit point ************      
      DO 30 I = 1,ST+1
        STMP1(I) = STMP(MOD(I+STPIND,ST+1)+1)
 30   CONTINUE   

c     Ok start angle is the pivit angle i.e. the fisrt angle
      SANG = RVAL1(STMP1(1)) 
c     now we know they are sorted clockwise relative to pivit
C     so we can determit the direction by seeing if the next
C     point was the hit point or not
      DIRECT = STMP1(2).EQ.2
        
      IF (DIRECT) THEN       
        EANG = RVAL1(STMP1(3))
      ELSE
        EANG = SANG
        SANG = RVAL1(STMP1(2))
      END IF
C
c     Alway ensure start angle is less than end angle
C      IF(EANG.LT.SANG) EANG = EANG+PI(2.0)

C
      END
      SUBROUTINE  SRTAR4(CENX,CENY,CXP,CYP,CXP1,CYP1,X1,Y1,X2,Y2,X3,Y3,
     +                   X4,Y4,ST1,XP,YP,STMP1)
C     ===============================================================
C1    IOTYPE              R   R  R  R  R  R  R  R  R  R  I  R  R  I
C1    IOSTAT              I   I  I  I  I  I  I  I  I  I  I  O  O  O
C2    This routine is used to find the closest intersection point
C2    to the point CXP,CYP.   The number of points is ST and the total
C2    number of points is 4
      REAL CXP,CYP,X1,Y1,X2,Y2,X3,Y3,X4,Y4,XP,YP,DISTXY,CENX,CENY
      REAL RVAL(5),RVAL1(5,2),MINVAL,TX1,TY1,CXP1,CYP1,CANG,TX2,TY2
      REAL RVALS(5)
      INTEGER ST1,ST,STMP(5),STMP1(5),I,TI1,STP,STPIND
      LOGICAL SWAP,DIRECT,SAME
                     
      EXTERNAL CANG,SAME
c
c     Note this suboutine works by calculating the ngles of all the points
C     and sorting them (EXCLUDING the direction point CXP1,CYP1) relative to
C     0 radians.  Then it sorts these values relative to a Pivit point and then
C     depening on the direction chooses the next value clockwise or couter clockwise
C     
      ST = ST1
c     let us tempararilly store the pivit point
C     plus the point to determit direction               
      TX1 = CXP1
      TY1 = CYP1
      TX2 = CXP
      TY2 = CYP

c     call reoutine that always store 1st point with bigger angle     
      CALL SRTARC(TX1,TY1,TX2,TY2,CENX,CENY)
c     direct counter clockwise unless the 2 temp values were swapped
      DIRECT = (TX1.NE.CXP.OR.TY1.NE.CYP)
      
                                        
c     set the pivit point around which to sort to 1
      STP = 1
c     store in arrays the angle and relative position
c     NOTE rval is sorted and rval1 is static
C     ---------------------------------------
      RVAL(1) = CANG(CENX,CENY,CXP,CYP)
      RVALS(1) = RVAL(1)
      RVAL1(1,1) = CXP
      RVAL1(1,2) = CYP
      STMP(1) = 1


      RVAL(2) = CANG(CENX,CENY,X1,Y1)
      RVALS(2) = RVAL(2)
      RVAL1(2,1) = X1
      RVAL1(2,2) = Y1
      STMP(2) = 2

      RVAL(3) = CANG(CENX,CENY,X2,Y2)
      RVALS(3) = RVAL(3)
      RVAL1(3,1) = X2
      RVAL1(3,2) = Y2
      STMP(3) = 3
      IF(ST.GT.2) THEN
         RVAL(4) = CANG(CENX,CENY,X3,Y3)
         RVALS(4) = RVAL(4)
         RVAL1(4,1) = X3
         RVAL1(4,2) = Y3
         STMP(4) = 4
      END IF
      IF(ST.GT.3) THEN
         RVAL(5) = CANG(CENX,CENY,X4,Y4)
         RVALS(5) = RVAL(5)
         RVAL1(5,1) = X4
         RVAL1(5,2) = Y4
         STMP(5) = 5
      END IF


c     Do a quick bubble sort of the angles about 0 degrees
c     ===================================================
   5  CONTINUE
                      
      SWAP = .FALSE.

      DO 10 I = 1,ST
         IF(RVAL(I).GT.RVAL(I+1) ) THEN
            TX1 = RVAL(I)
            RVAL(I) = RVAL (I+1)
            RVAL(I+1) = TX1
            TI1 = STMP(I)
            STMP(I) = STMP(I+1)
            STMP(I+1) = TI1
            SWAP = .TRUE.
         END IF
 10   CONTINUE   
      IF(SWAP ) GOTO 5
c     ===================================================

c     now let us find out where the pivit point was
      DO 20 I = 1,ST+1
         IF( STMP(I) .EQ.STP) STPIND = I-2
 20   CONTINUE   
c
c     ***** Now let us sort according to the pivit point *****      
      DO 30 I = 1,ST+1
        STMP1(I) = STMP(MOD(I+STPIND,ST+1)+1)
 30   CONTINUE   
      


c     we know if counter clock wise then the
c     index nubmer is the next sequential value
C     else it was the last value
      IF (DIRECT) THEN
         I = 2
  40     CONTINUE
         STP = STMP1(I)
C        This ensures that a coincident point is not chosen
         IF(SAME(RVALS(STP),RVALS(1))) THEN
            I = I + 1 
            IF(I.LE.ST+1) GOTO 40
         END IF
      ELSE
         I = ST+1 
  50     CONTINUE
         STP = STMP1(I)
C        This ensures that a coincident point is not chosen
         IF(SAME(RVALS(STP),RVALS(1))) THEN
            I = I - 1 
            IF(I.GE.1)  GOTO 50
         END IF
      END IF 
c
      XP = RVAL1(STP,1)
      YP = RVAL1(STP,2)
C
      END
      SUBROUTINE STOWVX(V,W)
C     ======================
C1                      R,R
C2       Subroutine returns the value of V
C2      which is in screen dimensions in world
C2      dimensions.
C
      REAL ZERO,W,V,DUM1
      EXTERNAL SC2WO
      INTRINSIC ABS
C
      CALL SC2WO(0.0,0.0,ZERO,DUM1)
      CALL SC2WO(V,0.0,W,DUM1)
C
      W=ABS(W-ZERO)
C
      END
C
C
      SUBROUTINE SWINDO(XMIN,YMIN,XMAX,YMAX)
C     ======================================
C1                         R,   R,   R,   R
C1                         O,   O,   O,   O
C
C2      Subroutine SWINDO returns the coordinates of the screen
C2      window defined by the user using the graphics cursor
C2      twice
C
      INTEGER C
      REAL XMIN,XMAX,YMIN,YMAX
      EXTERNAL SCURS
C
      CALL SCURS(C,XMIN,YMIN)
      CALL SCURS(C,XMAX,YMAX)
C
C       Check to ensure X1 is the minimum X value
      IF(XMAX.GT.XMIN) GOTO 10
      CALL RSWAP(XMIN,XMAX)
C
C       Check to ensure Y1 is the minimum Y value
 10   IF(YMAX.GT.YMIN) RETURN
      CALL RSWAP(XMIN,XMAX)
C
      END
C
      SUBROUTINE WTOSVX(W,V)
C     ======================
C
C1                      R,R
C
      REAL ZERO,W,V,DUM1
      EXTERNAL WO2SC
      INTRINSIC ABS
C
      CALL WO2SC(0.0,0.0,ZERO,DUM1)
      CALL WO2SC(W,0.0,V,DUM1)
C
      V=ABS(V-ZERO)
C
      END
C
C
      SUBROUTINE WTOSVY(W,V)
C     ======================
C
C1                      R,R
C
      REAL ZERO,W,V,DUM1
      EXTERNAL WO2SC
      INTRINSIC ABS
C
      CALL WO2SC(0.0,0.0,DUM1,ZERO)
      CALL WO2SC(0.0,W,DUM1,V)
C
      V=ABS(V-ZERO)
C
      END
C
C
      FUNCTION   ZERO (VAL)
C     =====================
C
C1       R              R
C1       O              I
C2      Function ZERO sets VAL to zero if
C2      it's value is less than 1e-5
      include 'include/wtov.inc'
C
      REAL VAL,ZERO,ATOL,EXT
      INTRINSIC ABS
      DATA ATOL/1E-7/
C      DATA ATOL/1E-3/
C
      EXT=WPXMAX-WPXMIN
      ZERO=VAL
      IF ( ABS(VAL) .LE. EXT*ATOL ) ZERO = 0.0
C
      END
C
C
