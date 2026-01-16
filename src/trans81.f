C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 trans81.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION RZERO(VAL)
C     SUBROUTINE DECODM(M,PARS)
C     SUBROUTINE SC2WO(VX,VY,WX,WY)
C     SUBROUTINE VIEW(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE WO2SC(WX,WY,VX,VY)
C     SUBROUTINE WORLD(XMIN,YMIN,XMAX,YMAX)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DECODM(M,PARS)
C     =========================
C
C1    vartype        R(3,3) R(9)
C1    iostatus          I    O
C
C2    Subroutine DECODM takes a transformation
C2    matrix M as input,and decodes the effective
C2    transform into its basic elements,which
C2    are returned in array PARS.
C2    Elements 1,2,3 ... translation
C2    Elements 4,5,6 ... Scaling
C2    Elements 7,8,9 ... Rotation
C
      REAL M(3,3),PARS(9),CANG,DISTXY,M2(3,3),M3(3,3),FM(3,3)
      REAL X1,Y1,X2,Y2,X3,Y3,XX1,YY1,XX2,YY2,X4,Y4,PI
      LOGICAL SAME
      EXTERNAL NEWXY,CANG,DISTXY,PI,SAME
      DATA X1,Y1,X2,Y2/0.0,0.0,10.0,0.0/
C
C     find transformed position
      CALL NEWXY(X1,Y1,XX1,YY1,M)
      CALL NEWXY(X2,Y2,XX2,YY2,M)
C     save location point
      PARS(1)=XX1
      PARS(2)=YY1
      PARS(3)=0.0
C     get rotation angle
      PARS(7)=0.0
      PARS(8)=0.0
      PARS(9)=CANG(XX1,YY1,XX2,YY2)
C     If the angle is pi then it could be a mirror
      IF(PARS(9).EQ.PI(1.0)) THEN
C         move the perpendicular
          CALL NEWXY(Y2,X2,X3,Y3,M)
C         if it does not move then it is a mirror
          IF(SAME(Y2,X3).AND.SAME(X2,Y3)) PARS(9)=0.0
      ENDIF
C     get scale factor on X
C     clear out temporary matrices
      CALL I3M(M2)
      CALL I3M(M3)
      CALL I3M(FM)
C     create a reverse angle matrix
      CALL ROT2D(-PARS(9),M2)
C     create a negative transform
      CALL TRAN2D(-PARS(1),-PARS(2),M3)
C     concatonate
      CALL MULT3M(M2,M3,FM)
C     transform the original line back with the new matrix
C     what we have left must be an unscaled line
      CALL NEWXY(XX1,YY1,X3,Y3,FM)
      CALL NEWXY(XX2,YY2,X4,Y4,FM)
C     must be the X axis scale factor magnitude and polarity
      PARS(4)=SIGN((DISTXY(X3,Y3,X4,Y4)/DISTXY(X1,Y1,X2,Y2)),
     +        (X4-X3))
C     calc scale on Y
C     move the original line with the incoming matrix exept this
C     is the Y axis this time
      CALL NEWXY(Y2,X2,XX2,YY2,M)
      CALL NEWXY(X1,Y1,XX1,YY1,M)
C     transform it back with the 'reverse matrix' to find the scale
      CALL NEWXY(XX1,YY1,X3,Y3,FM)
      CALL NEWXY(XX2,YY2,X4,Y4,FM)
C     must be the Y axis scale factor magnitude and polarity
      PARS(5)=SIGN((DISTXY(X3,Y3,X4,Y4)/DISTXY(X1,Y1,X2,Y2)),
     +        (Y4-Y3))
      PARS(6)=1.0
C
      END
 
 
 
 
 
      LOGICAL FUNCTION RZERO(VAL)
C     ==========================
C1    VARTYPE                 R
C1    IOSTATUS                I
C
C2    This function will return a true zero on a REAL number.
 
      REAL VAL,TOL
 
      TOL  = 1E-5
 
      IF ( VAL .LT. TOL ) THEN
 
          RZERO = .TRUE.
 
          RETURN
 
      ENDIF
 
      RZERO  = .FALSE.
 
      END
      SUBROUTINE SC2WO(VX,VY,WX,WY)
C     =============================
C1                      R, R, R, R
C1                      I, I, O, O
C2      Subroutine SC2WO converts VX,VY (screen coordinates)
C2      into world coordinates WX,WY
C
      include  'include/wtov.inc'
      REAL VX,VY,WX,WY
C
      EXTERNAL NEWXY
C
C       Using screen to world transformation VWXY
      CALL NEWXY(VX,VY,WX,WY,VWXY)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE VIEW(XMIN,YMIN,XMAX,YMAX)
C     ====================================
C1                       R,   R,   R,   R
C1                       I,   I,   I,   I
C
C2        Subroutine VIEW set the window (viewport) on the
C2        device screen
C2    COMMON /WTOV/WXMIN,WXMAX,WYMIN,WYMAX,
C2   1       SCXMIN,SCXMAX,SCYMIN,SCYMAX,
C2   2       VXMIN,VXMAX,VYMIN,VYMAX,WVXY,VWXY
C
C
      include  'include/wtov.inc'
      include  'include/viewport.inc'
C
      REAL  XMIN, XMAX, YMIN, YMAX
C
      EXTERNAL SETW2V,BELL
C
C       Check that limits are in the right order
C
      IF(XMIN.GE.XMAX.OR.YMIN.GE.YMAX) THEN
         CALL BELL
         RETURN
      END IF
C
C       Ensure that the screen window does not exceed
C       the terminal screen coordinate limits
c      IF(XMIN.LT.SCXMIN) XMIN=SCXMIN
c      IF(XMAX.GT.SCXMAX) XMAX=SCXMAX
c      IF(YMIN.LT.SCYMIN) YMIN=SCYMIN
C      IF(YMAX.GT.SCYMAX) YMAX=SCYMAX
C
      VXMIN=XMIN
      VXMAX=XMAX
      VYMIN=YMIN
      VYMAX=YMAX
C
C     save the values for viewport dimensions
      VIEWPS(1,1) = WXMIN
      VIEWPS(2,1) = WYMIN
      VIEWPS(3,1) = WXMAX
      VIEWPS(4,1) = WYMAX
      VPXMAX = VXMAX
      VPYMAX = VYMAX
      VPXMIN = VXMIN
      VPYMIN = VYMIN
C
      CALL SETW2V
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE WO2SC(WX,WY,VX,VY)
C     ==============================
C
C1                      R, R, R, R
C1                      I, I, O, O
C2      Subroutine WO2SC converts WX,WY to screen coordinates
C
      include  'include/wtov.inc'
      REAL  WX,WY,VX,VY
C
      EXTERNAL NEWXY
C       Using world to screen transformation WVXY
      CALL NEWXY(WX,WY,VX,VY,WVXY)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE WORLD(XMIN,YMIN,XMAX,YMAX)
C     =====================================
C1                        R,   R,   R,   R
C1                        I,   I,   I,   I
C2       Subroutine WORLD sets the world coordinates window for
C2       the transformation
C
C
      include  'include/wtov.inc'
C
      LOGICAL RZERO
      REAL XMIN,XMAX,YMIN,YMAX,SFA
      EXTERNAL SETW2V,BELL,SAME
C
C       Check that window limits are in right order
      IF(XMIN.GE.XMAX.OR.YMIN.GE.YMAX) THEN
         CALL BELL
         RETURN
      END IF
C
C       Transfer to world limits
      WXMIN=XMIN
      WXMAX=XMAX
      WYMIN=YMIN
      WYMAX=YMAX
C
C       obtain new transformation matrices
      CALL SETW2V
C
      END
C
C-----------------------------------------------------------------
C
