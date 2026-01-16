C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 vector1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION   CANG(X1,Y1,X2,Y2)
C     FUNCTION   CD0D13 (X1,Y1,X2,Y2,X,Y)
C     FUNCTION   CDSD13 (X1,Y1,X2,Y2,X,Y)
C     FUNCTION   DISTXY(X1,Y1,X2,Y2)
C     FUNCTION   DOTPRD (V1,V2,V3,P1,P2,P3)
C     FUNCTION   MAGLIN (L1,L2,L3)
C     FUNCTION   PI(X)
C     FUNCTION   QUAD(XC,YC)
C     FUNCTION   QUADA(ANG)
C     FUNCTION   VD0D13 (L1,L2,L3,X,Y)
C     FUNCTION   VN00D6(V1,V2,V3,L1,L2,L3)
C     SUBROUTINE CANG2A(ANG1,ANG2,ANGLE)
C     SUBROUTINE CC00P3(X1,Y1,X2,Y2,XM,YM)
C     SUBROUTINE CC00P5(X11,Y11,X12,Y12,X21,Y21,X22,Y22,X,Y,OK)
C     SUBROUTINE CN00D6(X11,Y11,X12,Y12,X21,Y21,X22,Y22,ANG)
C     SUBROUTINE CRSPRD(V1,V2,V3,P1,P2,P3,C1,C2,C3)
C     SUBROUTINE CV00L4(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     SUBROUTINE CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     SUBROUTINE HOMOXY(L1,L2,L3,X,Y)
C     SUBROUTINE LPRLDS (L1,L2,L3,S,D,C1,C2,C3)
C     SUBROUTINE SRTEND(PX,PY,VX,VY,HX,HY,X1,Y1,X2,Y2,XP,YP)
C     SUBROUTINE UC00P4(X1,Y1,D,U1,U2,X2,Y2)
C     SUBROUTINE UCU001(X1,Y1,X2,Y2,U1,U2)
C     SUBROUTINE VC00L1(L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,NX1,NY1,NX2,NY2)
C     SUBROUTINE VC00P4(X1,Y1,DIST,L1,L2,L3,X2,Y2)
C     SUBROUTINE VC00P5 (V1,V2,V3,P1,P2,P3,X,Y,OK)
C     SUBROUTINE VC00P6(L1,L2,L3,HPX,HPY,X,Y)
C     SUBROUTINE VC00P9(XC,YC,RAD,L1,L2,L3,X1,Y1,X2,Y2,OK)
C     SUBROUTINE VC0PLP(L1,L2,L3,X,Y,XP,YP)
C     SUBROUTINE VV00L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C     SUBROUTINE VV00L5(L1,L2,L3,X,Y,ANG,C1,C2,C3)
C     SUBROUTINE VV00L6 (L1,L2,L3,X,Y,C1,C2,C3)
C     SUBROUTINE VV0L15(L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3)
C
C     |-----------------------------------------------------------------|
C
      FUNCTION   CANG(X1,Y1,X2,Y2)
C     ============================
C
C1        R          R, R, R, R
C1        O          I, I, I, I
C
C
      REAL CANG,X1,Y1,X2,Y2,XDIF,YDIF,ZERO,PI,VAL
      INTEGER QUAD,QU
      INTRINSIC ATAN,ABS
      EXTERNAL ZERO,QUAD
C
      XDIF=ZERO(X2-X1)
      YDIF=ZERO(Y2-Y1)
C
      IF ( XDIF .EQ. 0.0 .AND. YDIF .GT. 0.0 ) THEN
C        90 degrees between points
         CANG=PI(0.5)
         RETURN
      ELSE IF ( XDIF .EQ. 0.0 .AND. YDIF .LE. 0.0 ) THEN
C       270 degrees between points
         CANG=PI(3.0/2.0)
         RETURN
      ELSE IF ( YDIF .EQ. 0.0 .AND. XDIF .GT. 0.0 ) THEN
C         0 degrees between points
         CANG=0.0
         RETURN
      ELSE IF ( YDIF .EQ. 0.0 .AND. XDIF .LE. 0.0 ) THEN
C       180 degrees between points
         CANG=PI(1.0)
         RETURN
      END IF
C
      VAL=ABS(YDIF)/ABS(XDIF)
      CANG=ATAN(VAL)
C
      GOTO( 10,20,30,40) QUAD(XDIF,YDIF)
 10   RETURN
 20   CANG=PI(1.0)-CANG
      RETURN
 30   CANG=CANG+PI(1.0)
      RETURN
 40   CANG=PI(2.0)-CANG
C
      END
C
C
      SUBROUTINE CANG2A(ANG1,ANG2,ANGLE)
C     ==================================
C2    VARTYPE            R    R     R    
C2    IOSTATUS           I    I     O    
C
C2    Gets the ANGLE between to angles assuming CCW direction
C2    and ang 1 is the first angle.
C
      REAL ANG1
      REAL ANG2
      REAL ANGLE
      REAL PI2
      REAL PI
C
      EXTERNAL PI
C
C     calculate 2* pi
      PI2 = PI(2.0)
C
C     get distance.
      ANGLE = MOD(ANG2 - ANG1 + PI2 , PI2)
C
      END

      SUBROUTINE CC00P3(X1,Y1,X2,Y2,XM,YM)
C     ====================================
C
C1                       R, R, R, R, R, R
C1                       I, I, I, I, O, O
C2      Subroutine CC00P3 returns the mid-point XM,YM
C2      of the line (X1,Y1) (X2,Y2)
C
      REAL X1,Y1,X2,Y2,XM,YM
C
      XM=0.5*(X1+X2)
      YM=0.5*(Y1+Y2)
C
      END
C
C
      SUBROUTINE CC00P5(X11,Y11,X12,Y12,X21,Y21,X22,Y22,X,Y,OK)
C     =========================================================
C
C1                        R,  R,  R,  R,  R,  R,  R,  R,R,R, L
C1                        I,  I,  I,  I,  I,  I,  I,  I,O,O, O
C
C2      Subroutine CC00P5 returns the point of intersection
C2      of the two lines (X11,Y11,X12,X12) (X21,Y21,X22,Y22)
C2      returning the point in X,Y
C2      If the lines are parallel then OK = .FALSE.
C
      REAL X11,Y11,X12,Y12,X21,Y21,X22,Y22,X,Y,
     +     L1,L2,L3,M1,M2,M3
      LOGICAL OK
      EXTERNAL CV0L14,VC00P5
C
C       Convert from cartesian points to vectors
      CALL CV0L14(X11,Y11,X12,Y12,M1,M2,M3)
      CALL CV0L14(X21,Y21,X22,Y22,L1,L2,L3)
C       Call vector subroutine VC00P5
      CALL VC00P5(L1,L2,L3,M1,M2,M3,X,Y,OK)
C
      END
C
      FUNCTION   CD0D13 (X1,Y1,X2,Y2,X,Y)
C     ===================================
C1       R                R, R, R, R,R,R
C1       O                I, I, I, I,I,I
C2      Function CD0D13 returns the perpendicular distance
C2      that a point X,Y is from the line X1,Y1 X2,Y2
C
      REAL  X1,Y1,X2,Y2,X,Y,CD0D13,C1,C2,C3
      INTRINSIC SQRT
C
      C1=Y2-Y1
      C2=X1-X2
      C3=-X2*C1-Y2*C2
 
      CD0D13=(C1*X+C2*Y+C3)/SQRT(C1*C1+C2*C2)
C
      END
C
      FUNCTION   CDSD13 (X1,Y1,X2,Y2,X,Y)
C     ===================================
C1       R                R, R, R, R,R,R
C1       O                I, I, I, I,I,I
C2      Function CD0D13 returns the square of the distance
C2      that a point X,Y is from the line X1,Y1 X2,Y2
C
      REAL  X1,Y1,X2,Y2,X,Y,CDSD13,C1,C2,C3
C
      C1=Y2-Y1
      C2=X1-X2
      C3=-X2*C1-Y2*C2
 
      CDSD13=(C1*X+C2*Y+C3)*(C1*X+C2*Y+C3)
C     squre it
      CDSD13 = CDSD13/(C1*C1+C2*C2)
C
      END
      SUBROUTINE CN00D6(X11,Y11,X12,Y12,X21,Y21,X22,Y22,ANG)
C     ======================================================
C1                       R   R   R   R   R   R   R   R   R
C1                       I   I   I   I   I   I   I   I   O
C
C2    routine will return the angle between two lines, in cartesian form
C
      REAL X11,Y11,X12,Y12,X21,Y21,X22,Y22,ANG,L1,L2,L3,C1,C2,C3
      REAL VN00D6
C
      EXTERNAL VN00D6
C
      CALL CV0L14 (X11,Y11,X12,Y12,L1,L2,L3)
      CALL CV0L14 (X21,Y21,X22,Y22,C1,C2,C3)
C
      ANG = VN00D6(L1,L2,L3,C1,C2,C3)
C
      END
 
 
      SUBROUTINE CRSPRD(V1,V2,V3,P1,P2,P3,C1,C2,C3)
C     =============================================
C
C1                       R, R, R, R, R, R, R, R, R
C1                       I, I, I, I, I, I, O, O, O
C
C2      Subroutine CRSPRD returns in arguments C1,C2,C3
C2      the cross product of vector V (V1,V2,V3) and
C2      P (P1,P2,P3) .
C
      REAL  V1,V2,V3,
     +      P1,P2,P3,
     +      C1,C2,C3
C
      C1 = P2 * V3 - P3 * V2
      C2 = P3 * V1 - P1 * V3
      C3 = P1 * V2 - P2 * V1
C
      END
C
C
      SUBROUTINE CV00L4(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     ===============================================
C
C1                       R, R, R, R,  R,  R, R, R, R
C1                       I,  I, I, I, I,  I, O, O, O
C2      Subroutine CV00L4 returns the line (M1,M2,M3)
C2      which passes through the point (HPX,HPY)
C2      and is parallel to line (X1,Y1,X2,Y2).
C
      REAL HPX,HPY,L1,L2,L3,M1,M2,M3,X1,Y1,X2,Y2
      EXTERNAL CV0L14,VV00L4
C
C     generate vector thru x1y1,x2y2
      CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     generate parallel vector thru HPXHPY
      CALL VV00L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C
      END
C
C
      SUBROUTINE CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     =======================================
C1                       R, R, R, R, R, R, R
C1                       I, I, I, I, O, O, O
C2      Subroutine CV0L14 return the line vector
C2      (L1,L2,L3) which passes through the points
C2      (X1,Y1) and (X2,Y2)
C
      REAL X1,Y1,X2,Y2,L1,L2,L3
C
      L1=Y2-Y1
      L2=-(X2-X1)
      L3=-X2*L1-Y2*L2
C
      END
C
C
      FUNCTION   DISTXY(X1,Y1,X2,Y2)
C     ==============================
C
C1       R             R, R, R, R
C1       O             I, I, I, I
C
C2      Function DISTXY returns the distance between the
C2      points (X1,Y1) and (X2,Y2)
C
      REAL DISTXY,X1,Y1,X2,Y2
      INTRINSIC SQRT
C
      DISTXY= SQRT((X1-X2)**2 + (Y1-Y2)**2)
C
      END
C
C
      FUNCTION   DOTPRD (V1,V2,V3,P1,P2,P3)
C     =====================================
C
C1        R               R, R, R, R, R, R
C1        O               I, I, I, I, I, I
C
C2      Function DOTPRD returns the dot product
C2      of vectors V (V1,V2,V3) and P (P1,P2,P3).
C
      REAL  V1,V2,V3,
     +      P1,P2,P3,DOTPRD
C
      DOTPRD = V1*P1 + V2*P2 + V3*P3
C
      END
C
C
      SUBROUTINE HOMOXY(L1,L2,L3,X,Y)
C     ===============================
C
C1                       R, R, R,R,R
C1                       I, I, I,O,O
C
C2      Subroutine HOMOXY returns the homogeneous coordinates
C2      of the vector L (L1,L2,L3)
C
      REAL L1,L2,L3,X,Y
C
      X=L1/L3
      Y=L2/L3
C
      END
C
C
      SUBROUTINE LPRLDS (L1,L2,L3,S,D,C1,C2,C3)
C     =========================================
C
C1                        R, R, R,R,R, R, R, R
C1                        I, I, I,I,I, O, O, O
C
C2     Subroutine LPRLDS returns the line C(C1,C2,C3)
C2      which is parallel to line L(L1,L2,L3) and is on the
C2      side indicated by the value of S -ve put to left
C2                                       +ve put to right
C
      REAL MAGLIN,L1,L2,L3,S,C1,C2,C3,D,DL3
C
C
      EXTERNAL MAGLIN
C
      DL3=D*MAGLIN(L1,L2,L3)
      C1= L1
      C2= L2
      C3= (L3-S*DL3)
C
      END
C
C
      FUNCTION   MAGLIN (L1,L2,L3)
C     ============================
C
C1       R                R, R, R
C1       O                I, I, I
C
C2      Function MAGLIN returns the magnitude of the line
C2      vector  L (L1,L2,L3).
C
      REAL MAGLIN,L1,L2,L3
      INTRINSIC SQRT
C
      MAGLIN = SQRT( L1*L1 + L2*L2 )
C
      END
C
C
      FUNCTION   PI(X)
C     ================
C1       R          R
C1       O          I
      REAL P,PI,X
      PARAMETER (P=3.1415926535898)
C-----RETURNS X*PI.  D=(D).
      PI=X*P
      END
*
      FUNCTION   QUAD(XC,YC)
C     ====================
C1       I           R, R
C1       O           I, I
C2      Quad returns a value depending on the x and y
C2      coordinates    QUAD   X    Y
C2                      1    +ve  +ve
C2                      2    -ve  +ve
C2                      3    -ve  -ve
C2                      4    +ve  -ve
C
      INTEGER QUAD
      REAL XC,YC
      LOGICAL XPOS,YPOS
      XPOS = XC .GT. 0.0
      YPOS = YC .GT. 0.0
      IF ( XPOS.AND.YPOS ) THEN
         QUAD = 1
      ELSE IF ( .NOT.XPOS.AND.YPOS ) THEN
         QUAD = 2
      ELSE IF ( .NOT.XPOS.AND..NOT.YPOS ) THEN
         QUAD = 3
      ELSE
         QUAD = 4
      END IF
      END
C
C
      FUNCTION   QUADA(ANG)
C     ====================
C1       I            R
C1       O            I
C2      Quada returns a value depending on the angle ANG
C2      passed indicating the quadrant in which the angle
C2      exists. Does not test for values greater than 2PI.
C2                      1    0-PI/2
C2                      2    PI/2-PI
C2                      3    PI-3PI/2
C2                      4    3PI/2-2PI
C
      INTEGER QUADA
      REAL PI,ANG,ANG2,ABS
      INTRINSIC ABS
C
      EXTERNAL PI
C
      ANG2=ABS(ANG)
      IF (ANG2.LT.PI(0.5) ) THEN
         QUADA = 1
      ELSE IF (ANG2.LT.PI(1.0) ) THEN
         QUADA = 2
      ELSE IF (ANG2.LT.PI(3.0/2.0)) THEN
         QUADA = 3
      ELSE
         QUADA = 4
      END IF
C     fix for negative angle
      IF (ANG.LT.0) QUADA=5-QUADA
C
      END
C
C
      SUBROUTINE SRTEND(PX,PY,VX,VY,HX,HY,X1,Y1,X2,Y2,XP,YP)
C1    ======================================================
C1    IOSTAT            I  I   I  I  I  I  I  I  I  I  O  O
C1    IOTYPE            R  R   R  R  R  R  R  R  R  R  R  R
C2
C2    This routine calculates which out of two points is the
C2    correct point which going along the line px,py to vx,vy
C2    in the direction px,py to hx,hy is the next point out of
C2    two possibles X1,Y1 and X2,Y2 answer XP,YP
C2
C2    This is achieved by calculating a vector between PX,PY and VX,VY
C2    and finding the nearest point on this vector to HX,HY.  This is
C2    then used to calculate a new vector and then the "Closest" point
C2    is then calculated

      REAL PX,PY,VX,VY,HX,HY,X1,Y1,X2,Y2,XP,YP
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,NL1,NL2,NL3,
     +                 DHX1,DHY1,DC1,DC2,DVDD14
      LOGICAL FPOINT,DSAME
      EXTERNAL DVDD14,DSAME

C     This section is equivalent to the call to NEREST            
      DX1 = DBLE(PX)
      DY1 = DBLE(PY)

      DX2 = DBLE(VX)
      DY2 = DBLE(VY)
C     calculate vector through old line
      CALL DCVL14(DX1,DY1,DX2,DY2,NL1,NL2,NL3)

      DX3 = DBLE(HX)
      DY3 = DBLE(HY)
c     calulate hit point lying on the vector
      CALL DVCPLP(NL1,NL2,NL3,DX3,DY3,DHX1,DHY1)

C     calculate vector through intersection point and
C     going through hit point lying on vector
      CALL DCVL14(DX1,DY1,DHX1,DHY1,NL1,NL2,NL3)
     
c     calculate the distances between the old intersection
c     point and the two new intersection points.  
c     NOTE: - DVDD14 uses the distance along a vector
C            so that an intersection point lying to the 
c            (wrong) side of the old intersection point
c            with respect of the old hit point will be -ve
      DC1=DVDD14(DX1,DY1,NL1,NL2,NL3,DBLE(X1),DBLE(Y1))
      DC2=DVDD14(DX1,DY1,NL1,NL2,NL3,DBLE(X2),DBLE(Y2))
C                 
      FPOINT =((DC1.LT.DC2.AND.DC2.GT.0.0.AND.DC1.GT.0.0).OR.
     +      (DC1.GT.DC2.AND.DC2.LT.0.0))
      FPOINT = FPOINT .OR.(DSAME(DC2,DBLE(0.0)))
      FPOINT = FPOINT .AND.(.NOT.DSAME(DC1,DBLE(0.0)))
      IF(FPOINT) THEN 
         XP = X1
         YP = Y1
      ELSE
         XP = X2
         YP = Y2
      END IF
      END

      SUBROUTINE UC00P4(X1,Y1,D,U1,U2,X2,Y2)
C     ======================================
C
C2    Subroutine UC00P4 returns the point X2Y2
C2    which is distance D along unit vector U1U2
C2    from the point X1Y1.
C
      REAL X1,Y1,D,U1,U2,X2,Y2
C
C     Find new point.
      X2=X1-D*U2
      Y2=Y1+D*U1
C
      END
C
      SUBROUTINE UCU001(X1,Y1,X2,Y2,U1,U2)
C     ====================================
C
C2    Subroutine UCU001 finds the unit vector of the line between
C2    X1Y1 and X2Y2 and returns it in unit vector form in U1U2.
C
      REAL X1,Y1,X2,Y2,U1,U2,M,ABS
      INTRINSIC SQRT,ABS
C
C     Find length of line.
      M=SQRT((X2-X1)**2+(Y2-Y1)**2)
C
C     Test for zero length line.
      IF (M.EQ.0) THEN
C        Return zero vector.
         U1=0
         U2=0
      ELSE
         U2=-(X2-X1)/M
c         U2=(X2-X1)/M
         IF (ABS(U2).LT.1E-4) U2=0.0
         U1=(Y2-Y1)/M
         IF (ABS(U1).LT.1E-4) U1=0.0
      ENDIF
C
      END
C
C
      SUBROUTINE VC00L1(L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,NX1,NY1,NX2,NY2)
C     =============================================================
C1    vartype           R  R  R  R  R  R  R  R  R   R   R   R   R
C1    iostatus          I  I  I  I  I  I  I  I  I   O   O   O   O
C1
C2    returns the endpoints of the  line passing through X3,Y3
C2    parallel to vector L,and trimmed by lines perpendicular
C2    to L,passing through X1,Y1 and X2,Y2.The end points are
C2    returned in NX1,NY1,NX2,NY2
C
C
      REAL L1,L2,L3,M1,M2,M3,X1,Y1,X2,Y2,X3,Y3,NX1,NY1,NX2,NY2
C
      LOGICAL OK
C
      EXTERNAL VV00L6,VC00P5
C
C     Find the vector perpto L passing through X1,Y1,return in M
      CALL VV00L6(L1,L2,L3,X1,Y1,M1,M2,M3)
C     Find the intersection between L and M,store as end point
C     of new line.
      CALL VC00P5(L1,L2,L3,M1,M2,M3,NX1,NY1,OK)
C     find the vector perpto L passing through X2,Y2
C     return in M.
      CALL VV00L6(L1,L2,L3,X2,Y2,M1,M2,M3)
C     Find the intersection between L and M,store as end point of
C     dimension line.
      CALL VC00P5(L1,L2,L3,M1,M2,M3,NX2,NY2,OK)
C
      RETURN
      END
C
C
      SUBROUTINE VC00P4(X1,Y1,DIST,L1,L2,L3,X2,Y2)
C     ============================================
C
C1                       R, R,   R, R, R, R, R, R
C1                       I, I,   I, I, I, I, O, O
C2      Subroutine VC00P4 finds X2,Y2 a distance DIST along
C2      along the line L1,L2,L3 from point X1,Y1
C
      REAL X1,X2,Y1,Y2,L1,L2,L3,DIST,MAGLIN,TP
      EXTERNAL MAGLIN
C
      TP=DIST/MAGLIN(L1,L2,L3)
      X2=X1-(TP*L2)
      Y2=Y1+(TP*L1)
C
      END
C
C
      SUBROUTINE VC00P5 (V1,V2,V3,P1,P2,P3,X,Y,OK)
C     ============================================
C
C1                        R, R, R, R, R, R,R,R, L
C1                        I, I, I, I, I, I,O,O, O
C
C2    Subroutine VC00P5 returns the point of intersection
C2    of the two lines V (V1,V2,V3) and P (P1,P2,P3)
C2    returning the point in X,Y
C2    If the lines are parallel then OK = .FALSE.
C
      REAL  V1,V2,V3,
     +      P1,P2,P3,
     +      L1,L2,L3,ZERO,X,Y
C
      LOGICAL OK
      INTRINSIC INT
      EXTERNAL ZERO,CRSPRD,HOMOXY
C
C     Find the intersection by using crossproduct
      CALL CRSPRD(V1,V2,V3,P1,P2,P3,L1,L2,L3)
C     If value of third argument is close to zero
C     it is set by function ZERO
      L3=ZERO ( L3 )
      OK= L3 .NE. 0.0
      IF ( OK ) CALL HOMOXY(L1,L2,L3,X,Y)
C
      END
C
C
      SUBROUTINE VC00P6(L1,L2,L3,HPX,HPY,X,Y)
C     =======================================
C
C1    vartype           R   R  R  R   R  R  R
C1    iostatus          I   I  I  I   I  O  O
C
C2    Subroutine VC00P6 returns the point X,Y which
C2    is the point on the line L1,L2,L3 closest to the
C2    hit point HPX,HPY
C
      REAL L1,L2,L3,HPX,HPY,M1,M2,M3,P1,P2,P3,X,Y
C
      EXTERNAL VV00L6,CRSPRD
C
C     Generate the vector perpto L thru HPX,HPY
      CALL VV00L6(L1,L2,L3,HPX,HPY,M1,M2,M3)
C     find the point of intersection
      CALL CRSPRD(L1,L2,L3,M1,M2,M3,P1,P2,P3)
C     return the 2D coordinate point
      X=P1/P3
      Y=P2/P3
C
      END
C
C
      SUBROUTINE VC00P9(XC,YC,RAD,L1,L2,L3,X1,Y1,X2,Y2,OK)
C     ====================================================
C1                       R, R,  R, R, R, R, R, R, R, R, L
C1                       I, I,  I, I, I, I, O, O, O, O, O
C
C2      Subroutine VC00P9 returns the two points which intersect
C2      the circle XC,YC RAD in X1,Y1 and X2,Y2
C2      OK is .false. if the line does not intersect the
C2      circle
C
      REAL XC,YC,RAD,L1,L2,L3,M1,M2,M3,N1,N2,N3,
     +     X,Y,X1,Y1,X2,Y2,DIST,S,MAGLIN,VD0D13
      LOGICAL OK,SAME
      INTRINSIC SQRT,ABS
      EXTERNAL VC00P4,VC00P5,VD0D13,VC0PLP
c
C        Check distance from line to centre of circle
      DIST=ABS(VD0D13(L1,L2,L3,XC,YC))
      OK = ABS(DIST) .LT. RAD
      IF ( .NOT. OK ) THEN
         OK=SAME(DIST,RAD)
C       WRITE(10,*) '[DVC0P9]',OK,DIST,RAD
         IF ( OK  ) THEN
            CALL VC0PLP(L1,L2,L3,XC,YC,X1,Y1)
            X2=X1
            Y2=Y1
         END IF
         RETURN
      END IF
 
C        If greater than radius line does not intersect circle
      IF ( .NOT. OK ) RETURN
      S = SQRT ( RAD**2-DIST**2 )
      M2=-L1
      M1=L2
      M3= -L2*XC + L1*YC
      CALL VC00P5(L1,L2,L3,M1,M2,M3,X,Y,OK)
      CALL VC00P4(X,Y, S,L1,L2,L3,X1,Y1)
      CALL VC00P4(X,Y,-S,L1,L2,L3,X2,Y2)
C
      END
C
C
      SUBROUTINE VC0PLP(L1,L2,L3,X,Y,XP,YP)
C     =====================================
C1                       R, R, R,R,R, R, R
C1                       I, I, I,I,I, O, O
C
C2     DCCPLP returns the point XP,YP which lies on the
C2       line L1,L2,L3  and is the shortest distance
C2       from the indicating point X,Y
      REAL L1,L2,L3,T1,T2,T3,X,Y,XP,YP
      LOGICAL OK
      EXTERNAL VV00L6,VC00P5
C
      CALL VV00L6(L1,L2,L3,X,Y,T1,T2,T3)
      CALL VC00P5(T1,T2,T3,L1,L2,L3,XP,YP,OK)
C
      END
C
C
      FUNCTION   VD0D13 (L1,L2,L3,X,Y)
C     ================================
C
C1       R                R, R, R,R,R
C1       O                I, I, I,I,I
C
C2      Function VD0D13 returns the perpendicular distance
C2      from a point (X,Y) to the line L (L1,L2,L3)
C
      REAL VD0D13,L1,L2,L3,X,Y,MAGLIN,DOTPRD
      EXTERNAL DOTPRD,MAGLIN
C
C      VD0D13=DOTPRD(L1,L2,L3,X,Y,1.0)/MAGLIN(L1,L2,L3)
      VD0D13=(L1*X+L2*Y+L3)/SQRT(L1*L1+L2*L2)
C
      END
C
C
      FUNCTION   VN00D6(V1,V2,V3,L1,L2,L3)
C     ====================================
C
C1       R               R, R, R, R, R, R
C1       O               I, I, I, I, I, I
C
C2      Function VN00D6 returns the angle between the two line
C2      line vectors in radians
C2      V1,V2,V3 the first line vector
C2      L1,L2,L3  "  second  "     "
C
      REAL L1,L2,L3,V1,V2,V3,MAGLIN,DOTPRD,VN00D6,PI,CHECK
      EXTERNAL MAGLIN
C
      VN00D6=L1*V1+L2*V2
      VN00D6=VN00D6/MAGLIN(L1,L2,L3)
      VN00D6=VN00D6/MAGLIN(V1,V2,V3)
      IF (VN00D6.GT.0.0) VN00D6 = MIN(VN00D6,1.0)
      IF (VN00D6.LT.0.0) VN00D6 = MAX(VN00D6,-1.0)
      VN00D6=ACOS(VN00D6)
      CHECK=V1*L2-V2*L1
      IF ( CHECK.LT.0.0 ) VN00D6=PI(2.0)-VN00D6
C
      END
C
C
      SUBROUTINE VV00L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C     ============================================
C
C1                        R,  R, R, R, R, R, R, R
C1                        I,  I, I, I, I, O, O, O
C
C2      Subroutine VV00L4 returns the line (M1,M2,M3)
C2      which passes through the point (HPX,HPY)
C2      and is parallel to line (L1,L2,L3).
C
      REAL  HPX,HPY,L1,L2,L3,M1,M2,M3
C
      M1=L1
      M2=L2
      M3= -L1*HPX - L2*HPY
C
      END
C
C
      SUBROUTINE VV00L5(L1,L2,L3,X,Y,ANG,C1,C2,C3)
C     ============================================
C
C1                       R, R, R,R,R,  R, R, R, R
C1                       I, I, I,I,I,  I, O, O, O
C
C2      Subroutine VV00L5 returns the line vector (C1,C2,C3)
C2      which is at an angle of ANG radians to the line
C2      (L1,L2,L3) and passing through  the point X,Y
C
      REAL L1,L2,L3,X,Y,ANG,C1,C2,C3,COSAN,SINAN,ZERO
      INTRINSIC COS,SIN
      EXTERNAL ZERO
C
      COSAN=COS(ANG)
      SINAN=SIN(ANG)
C
      C1 = ZERO(L1*COSAN - L2*SINAN)
      C2 = ZERO(L1*SINAN + L2*COSAN)
      C3 = ZERO(-C1*X    - C2*Y    )
C
      END
C
C
      SUBROUTINE VV00L6 (L1,L2,L3,X,Y,C1,C2,C3)
C     =========================================
C
C1                        R, R, R,R,R, R, R, R
C1                        I, I, I,I,I, I, O, O
C     
C2      Subroutine VV00L6 returns the vector C (C1,C2,C3)
C2      which is perpendicular to L (L1,L2,L3) and passes
C2      through the point P (X,Y)
C
      REAL L1,L2,L3,C1,C2,C3,X,Y
C
      C1 = L2
      C2 =-L1
      C3 = L1*Y - L2*X
C
      END
C
C
      SUBROUTINE VV0L15(L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3)
C     =================================================
C
C1                       R, R, R,  R, R, R, R, R, R, R
C1                       I, I, I,  I, O, O, O, O, O, O
C
C2      Subroutine VV0L15 returns the two (C1,C2,C3),
C2      (M1,M2,M3) line which are parallel to line
C2      (L1,L2,L3) and separated by the value SEP
C
      REAL L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3,MAGLIN,SIZE
      EXTERNAL MAGLIN
C
      SIZE=SEP*MAGLIN(L1,L2,L3)
      C1=L1
      M1=L1
      C2=L2
      M2=L2
      C3=L3-SIZE
      M3=L3+SIZE
C
      END
 
 
