C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 dvector1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION   DCDD13 (X1,Y1,X2,Y2,X,Y)
C     FUNCTION   DDTPRD(V1,V2,V3,P1,P2,P3)
C     FUNCTION   DMAGLN (L1,L2,L3)
C     FUNCTION   DVDD13 (L1,L2,L3,X,Y)
C     FUNCTION   DVDD14 (X1,Y1,L1,L2,L3,X,Y)
C     FUNCTION   DVN0D6(V1,V2,V3,L1,L2,L3)
C     SUBROUTINE DCC0C7(X11,Y11,X12,Y12,X21,Y21,X22,RAD,NUM,
C     SUBROUTINE DCC0C9(L1,L2,L3,M1,M2,M3,RAD,XC,YC,OK)
C     SUBROUTINE DCC0L2(X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,X3,Y3,X4,Y4)
C     SUBROUTINE DCC0P3(X1,Y1,X2,Y2,XM,YM)
C     SUBROUTINE DCC0P4(X3,Y3,DIST,X1,Y1,X2,Y2,X4,Y4)
C     SUBROUTINE DCC0P5(X11,Y11,X12,Y12,X21,Y21,X22,Y22,X,Y,OK)
C     SUBROUTINE DCC0P9(XC1,YC1,RAD1,X1,Y1,X2,Y2,XP1,YP1,XP2,YP2,OK)
C     SUBROUTINE DCCC10(X1,Y1,X2,Y2,X3,Y3,X,Y,RAD,OK)
C     SUBROUTINE DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,X1,Y1,X2,Y2,OK)
C     SUBROUTINE DCCPAP(XC,YC,RAD,X1,Y1,X2,Y2)
C     SUBROUTINE DCCPLP(X1,Y1,X2,Y2,X,Y,XP,YP)
C     SUBROUTINE DCRSPD(V1,V2,V3,P1,P2,P3,C1,C2,C3)
C     SUBROUTINE DCV0L4(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     SUBROUTINE DCV0L5(X1,Y1,X2,Y2,X,Y,ANG,C1,C2,C3)
C     SUBROUTINE DCV0L6(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     SUBROUTINE DCV0L7(X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,M1,M2,M3)
C     SUBROUTINE DCV0L8(X,Y,XC,YC,RAD,C1,C2,C3,L1,L2,L3,OK)
C     SUBROUTINE DCV1L8(X,Y,HX,HY,XC,YC,RAD,L1,L2,L3,OK)
C     SUBROUTINE DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
C     SUBROUTINE DCVL15(X1,Y1,X2,Y2,SEP,L1,L2,L3,N1,N2,N3)
C     SUBROUTINE DCVL20(XC1,YC1,RAD1,XC2,YC2,RAD2,
C     SUBROUTINE DCVL21(XC1,YC1,RAD1,XC2,YC2,RAD2,L,OK)
C     SUBROUTINE DCVL22(XC1,YC1,RAD1,XC2,YC2,RAD2,
C     SUBROUTINE DHOMXY(L1,L2,L3,X,Y)
C     SUBROUTINE DLPRLD (L1,L2,L3,S,D,C1,C2,C3)
C     SUBROUTINE DNEWXY(OX,OY,NX,NY,A)
C     SUBROUTINE DVC0C7(L1,L2,L3,M1,M2,M3,RAD,XC,YC,NUM,OK)
C     SUBROUTINE DVC0C8(L1,L2,L3,M1,M2,M3,RAD,
C     SUBROUTINE DVC0P4(X1,Y1,DIST,L1,L2,L3,X2,Y2)
C     SUBROUTINE DVC0P5 (V1,V2,V3,P1,P2,P3,X,Y,OK)
C     SUBROUTINE DVC0P9(XC,YC,RAD,L1,L2,L3,X1,Y1,X2,Y2,OK)
C     SUBROUTINE DVCPLP(L1,L2,L3,X,Y,XP,YP)
C     SUBROUTINE DVV0C1(L1,L2,L3,XC,YC,RADIUS,M1,M2,M3)
C     SUBROUTINE DVV0L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C     SUBROUTINE DVV0L5(L1,L2,L3,X,Y,ANG,C1,C2,C3)
C     SUBROUTINE DVV0L6 (L1,L2,L3,X,Y,C1,C2,C3)
C     SUBROUTINE DVVL15(L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3)
C     SUBROUTINE MCC0P9(XC1,YC1,RAD1,RAD2,INCA,
C     SUBROUTINE MCCP19(XC1,YC1,RAD1,RAD2,INCA,
C     SUBROUTINE MVC0P9(XC1,YC1,RAD1,RAD2,INCA,L1,L2,L3,
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DCC0C7(X11,Y11,X12,Y12,X21,Y21,X22,RAD,NUM,
     +                  XC,YC,OK)
C     =======================================================
C1      All D.Precision (except OK logical)
C1      Top line input,bottom line output
C2      Subroutine DCC0C7 returns the eight fillet centres
C2      which exist for the line and arc joining the points
C2      (X11,Y11,X12,Y12) and (X21,Y21,X22),
C2      OK set .false. if arc and line do not intersect
C
      DOUBLE PRECISION X11,Y11,X12,Y12,X21,Y21,X22,RAD,
     +  XC(8),YC(8),M1,M2,M3
      INTEGER*4 NUM
      LOGICAL OK
C
      EXTERNAL DCVL14,DVC0C7
C
C       Convert from cartesian points to vector
      CALL DCVL14(X11,Y11,X12,Y12,M1,M2,M3)
C       Call vector subroutine DVC0C8
      CALL DVC0C7(M1,M2,M3,X21,Y21,X22,RAD,NUM,XC,YC,OK)
C
      END
C
C
      SUBROUTINE DCC0C9(L1,L2,L3,M1,M2,M3,RAD,XC,YC,OK)
C     =================================================
C1      All D.Precision (except OK logical)
C1      Top line input,bottom line output
C2      Subroutine DCC0C8 returns the four fillet centres
C2      which exist for the lines (L1,L2,L3) and (M1,M2,M3),
C2      OK set .false. if parallel lines
C
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,RAD,XC(8),YC(8)
      LOGICAL OK(4)
      EXTERNAL DCCP19
C
      CALL DCCP19(L1,L2,(L3+RAD),M1,M2,(M3+RAD),
     +             XC(1),YC(1),XC(5),YC(5),OK(1) )
C
      CALL DCCP19(L1,L2,(L3+RAD),M1,M2,(M3-RAD),
     +             XC(2),YC(2),XC(6),YC(6),OK(2) )
C
      CALL DCCP19(L1,L2,(L3-RAD),M1,M2,(M3+RAD),
     +             XC(3),YC(3),XC(7),YC(7),OK(3) )
C
      CALL DCCP19(L1,L2,(L3-RAD),M1,M2,(M3-RAD),
     +             XC(4),YC(4),XC(8),YC(8),OK(4) )
C
      END
C
C
      SUBROUTINE DCC0L2(X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,X3,Y3,X4,Y4)
C     ===============================================================
C1    vartype           D  D  D  D  D  D    D     D   D  D  D  D  D
C1    iostatus          I  I  I  I  I  I    I     I   I  O  O  O  O
C
C2    subroutine DCC0L2 returns the line endpoints X3,Y3,X4,Y4
C2    of the line which is tangent to circle XC,Yc,RADIUS and
C2    closest to HPX,HPY and parallel to line through the points
C2    X1,Y1,X2,Y2.....PHEW!!!!!
C
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,X4,Y4,HPX,HPY,XC,YC,RADIUS
      DOUBLE PRECISION L1,L2,L3
C
      EXTERNAL DCV0L7,DVCPLP
C
C     generate the tangent vector
      CALL DCV0L7(X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,L1,L2,L3)
C     trim the vector to the original line end points
      CALL DVCPLP(L1,L2,L3,X1,Y1,X3,Y3)
      CALL DVCPLP(L1,L2,L3,X2,Y2,X4,Y4)
C     return with end points in X3,Y3,X4,Y4
C
      END
C
C
      SUBROUTINE DCC0P3(X1,Y1,X2,Y2,XM,YM)
C     ====================================
C1                       D, D, D, D, D, D
C1                       I, I, I, I, O, O
C2      Subroutine CC00P3 returns the mid-point XM,YM
C2      of the line (X1,Y1) (X2,Y2)
C
      DOUBLE PRECISION X1,Y1,X2,Y2,XM,YM
C
      XM=(X1+X2)/2.0d0
      YM=(Y1+Y2)/2.0d0
C
      END
C
C
      SUBROUTINE DCC0P4(X3,Y3,DIST,X1,Y1,X2,Y2,X4,Y4)
C     ===============================================
C1                       D, D,   D, D, D, D, D, D
C1                       I, I,   I, I, I, I, O, O
C2      Subroutine DVC0P4 finds X4,Y4 a distance DIST along
C2      along the line X1,Y1,X2,Y2 from point X3,Y3
C
      DOUBLE PRECISION  X1,Y1,X2,Y2,X3,Y3,X4,Y4,DIST
     +                  ,L1,L2,L3
      EXTERNAL DCVL14,DVC0P4
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVC0P4(X3,Y3,DIST,L1,L2,L3,X4,Y4)
C
      END
C
C
      SUBROUTINE DCC0P5(X11,Y11,X12,Y12,X21,Y21,X22,Y22,X,Y,OK)
C     =========================================================
C1                        D,  D,  D,  D,  D,  D,  D,  D,D,D, L
C1                        I,  I,  I,  I,  I,  I,  I,  I,O,O, O
C2      Subroutine CC00P5 returns the point of intersection
C2      of the two lines (X11,Y11,X12,X12) (X21,Y21,X22,Y22)
C2      returning the point in X,Y
C2      If the lines are parallel then OK = .FALSE.
C
      DOUBLE PRECISION X11,Y11,X12,Y12,X21,Y21,X22,Y22
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,X,Y
      LOGICAL OK
      EXTERNAL DCVL14,DVC0P5
C
C     Convert from cartesian points to vectors
      CALL DCVL14(X11,Y11,X12,Y12,M1,M2,M3)
      CALL DCVL14(X21,Y21,X22,Y22,L1,L2,L3)
C     Call vector subroutine VC00P5
      CALL DVC0P5(L1,L2,L3,M1,M2,M3,X,Y,OK)
C
      END
C
C
      SUBROUTINE DCC0P9(XC1,YC1,RAD1,X1,Y1,X2,Y2,XP1,YP1,XP2,YP2,OK)
C     ==============================================================
C1                        D,  D,  D, D, D, D, D,  D,  D,  D,  D, D
C1                        I,  I,  I, I, I, I, I,  O,  O,  O,  O, O
C2      Subroutine DCC0P9 returns the two points (XP1,YP1)
C2      (XP2,YP2) which intersect the line (X1,Y1 X2,Y2) and
C2      and circle (XC1,YC1) RAD1
C
      DOUBLE PRECISION  XC1,YC1,RAD1,X1,Y1,X2,Y2,
     +                  XP1,YP1,XP2,YP2,L1,L2,L3
      LOGICAL OK
      EXTERNAL DCVL14,DVC0P9
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVC0P9(XC1,YC1,RAD1,L1,L2,L3,XP1,YP1,XP2,YP2,OK)
C
      END
C
C
      SUBROUTINE DCCC10(X1,Y1,X2,Y2,X3,Y3,X,Y,RAD,OK)
C     ===============================================
C1                       D, D, D, D, D, D,D,D,  D, L
C1                       I, I, I, I, I, I,O,O,  O, O
C
C2      Subroutine DCCC10 calculates the centre and radius of a
C2      circle given three points (X1,Y1) (X2,Y2) (X3,Y3)
C2      which lie on the circumference of the circle
C2      X,Y coordinates of the centre circle
C2      RAD radius of circle
C2      OK indicates whether or not it was possible to calculate
C2      the circle using the 3 points provided
C
      LOGICAL OK
      DOUBLE PRECISION X1,X2,X3,Y1,Y2,Y3,X,Y,RAD,M1,M2,M3,
     A         L1,L2,L3,T1,T2,T3,S1,S2,S3,DDSTXY
      EXTERNAL DCVL14,DVV0L6,DVC0P5,DCC0P3,DDSTXY
C
C       Calculate the first line vector
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
C
C       then the second line vector
      CALL DCVL14(X2,Y2,X3,Y3,M1,M2,M3)
C
C       find the mid-point of the first line
      CALL DCC0P3(X1,Y1,X2,Y2,X,Y)
C
C      find the line perpendicular to first line passing
C       through the mid-point
      CALL DVV0L6(L1,L2,L3,X,Y,T1,T2,T3)
C
C       find the mid-point of the second line
      CALL DCC0P3(X2,Y2,X3,Y3,X,Y)
C
C       find the line perpendicular to second line passing
C       through the mid-point
      CALL DVV0L6(M1,M2,M3,X,Y,S1,S2,S3)
C
C       find the intersection of the two lines
      CALL DVC0P5(T1,T2,T3,S1,S2,S3,X,Y,OK)
C
C       If .not. OK the lines do not intersect
      If ( .NOT. OK ) RETURN
C
C       calculate the radius by pythaegoras
      RAD=DDSTXY(X1,Y1,X,Y)
C
      END
C
C
      SUBROUTINE DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,X1,Y1,X2,Y2,OK)
C     ===========================================================
C1                        D,  D,   D,  D,  D,   D, D, D, D, D, L
C1                        I,  I,   I,  I,  I,   I, O, O, O, O, O
C2      Subroutine CC0P19 returns the two points which intersect
C2      the two circles C1 and C2
C2      OK is .false. if the two circles do not intersect
C
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,
     +   X1,Y1,X2,Y2,DDSTXY,M1,M2,M3,K,S
      LOGICAL OK,DSAME
      INTRINSIC ABS
      EXTERNAL DDSTXY,DCC0P9,DSAME,DVC0P9
C
C     Find distance between the two centres
      S=DDSTXY(XC1,YC1,XC2,YC2)
C
      OK= S.LT.(RAD1+RAD2).AND.S.GT.ABS(RAD1-RAD2).OR.DSAME(S,0.0D0)
C        Distance Should be  >diff and <sum
C        If not .OK. then return
      IF ( .NOT. OK ) THEN
C        arcs might miss but lets find out
         IF ( DSAME(S,RAD1+RAD2) .OR. DSAME(S,ABS(RAD1-RAD2))) THEN
C           Numerical error was small
            CALL DCC0P9(XC1,YC1,RAD1,XC1,YC1,XC2,YC2,
     +                    X1,Y1,X2,Y2,OK)
         END IF
         RETURN
      END IF
C        Complicated equation see notes M.Johnston (P.C.Technology)
      K=( (XC1**2)-(XC2**2)+(YC1**2)-(YC2**2)-(RAD1**2)+(RAD2**2) )/2.0
C
      M1=XC1-XC2
      M2=YC1-YC2
C
C        Both M1 and M2 cannot be zero
      OK = .NOT.(M1.EQ.0.AND.M2.EQ.0)
C        If both are return with OK .false.
      If ( .NOT. OK ) RETURN
      M3=-K
C
      CALL DVC0P9(XC1,YC1,RAD1,M1,M2,M3,X1,Y1,X2,Y2,OK)
C
      END
C
C
      SUBROUTINE DCCPAP(XC,YC,RAD,X1,Y1,X2,Y2)
C     ========================================
C1                       D, D,  D, D, D, D, D
C1                       I, I,  I, I, I, O, O
C2    DCCPAP returns the nearest point X2,Y2 which lies on
C2    the circumference of the arc (XC,YC,RAD) and is the
C2    shortest distance to X1,Y1
C
      DOUBLE PRECISION XC,YC,RAD,X1,Y1,X2,Y2,ANG,DCANG
      INTRINSIC DBLE,COS,SIN
      EXTERNAL DCANG
C
      ANG=DCANG(XC,YC,X1,Y1)
      X2=XC+DBLE(RAD*COS(ANG))
      Y2=YC+DBLE(RAD*SIN(ANG))
C
      END
C
C
      SUBROUTINE DCCPLP(X1,Y1,X2,Y2,X,Y,XP,YP)
C     ========================================
C1                       D, D, D, D,D,D, D, D
C1                       I, I, I, I,I,I, O, O
C
C2     DCCPLP returns the point XP,YP which lies on the
C2       line X1,Y1 X2,Y2 and is the shortest distance
C2       from the indicating point X,Y
      DOUBLE PRECISION  X1,Y1,X2,Y2,X,Y,XP,YP,L1,L2,L3
      EXTERNAL DVCPLP,DCVL14
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVCPLP(L1,L2,L3,X,Y,XP,YP)
C
      END
C
C
      FUNCTION   DCDD13 (X1,Y1,X2,Y2,X,Y)
C     ===================================
C1       D                D, D, D, D,D,D
C1       O                I, I, I, I,I,I
C2      Function CD0D13 returns the perpendicular distance
C2      that a point X,Y is from the line X1,Y1 X2,Y2
C
      DOUBLE PRECISION X1,Y1,X2,Y2,X,Y
      DOUBLE PRECISION DCDD13,C1,C2,C3
      INTRINSIC DSQRT
C
      C1=Y2-Y1
      C2=X1-X2
      C3=-X2*C1-Y2*C2
      DCDD13=(C1*X+C2*Y+C3)/DSQRT(C1*C1+C2*C2)
C
      END
C
C
      SUBROUTINE DCRSPD(V1,V2,V3,P1,P2,P3,C1,C2,C3)
C     =============================================
C1                       D, D, D, D, D, D, D, D, D
C1                       I, I, I, I, I, I, O, O, O
C2      Subroutine DCRSPD returns in arguments C1,C2,C3
C2      the cross product of vector V (V1,V2,V3) and
C2      P (P1,P2,P3) .
C
      DOUBLE PRECISION  V1,V2,V3,
     +                  P1,P2,P3,
     +                  C1,C2,C3
C
      C1 = (P2 * V3 - P3 * V2)
      C2 = (P3 * V1 - P1 * V3)
      C3 = (P1 * V2 - P2 * V1)
C
      END
C
C
      SUBROUTINE DCV0L4(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     ===============================================
C1                       D, D, D, D,  D,  D, D, D, D
C1                       I,  I, I, I, I,  I, O, O, O
C2      Subroutine DCV0L4 returns the line (M1,M2,M3)
C2      which passes through the point (HPX,HPY)
C2      and is parallel to line (X1,Y1,X2,Y2).
C
      DOUBLE PRECISION HPX,HPY,L1,L2,L3,M1,M2,M3,X1,Y1,X2,Y2
      EXTERNAL DCVL14,DVV0L4
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVV0L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C
      END
C
C
      SUBROUTINE DCV0L5(X1,Y1,X2,Y2,X,Y,ANG,C1,C2,C3)
C     ===============================================
C1                       D, D, D, D,D,D,  D, D, D, D
C1                       I, I, I, I,I,I,  I, O, O, O
C2      Subroutine DCV0L5 returns the line vector (C1,C2,C3)
C2      which is at an angle of ANG radians to the line
C2      (X1,Y1,X2,Y2) and passes through the point X,Y
C
      DOUBLE PRECISION X1,Y1,X2,Y2,L1,L2,L3,X,Y,ANG,C1,C2,C3
      EXTERNAL DCVL14,DVV0L5
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVV0L5(L1,L2,L3,X,Y,ANG,C1,C2,C3)
C
      END
C
C
      SUBROUTINE DCV0L6(X1,Y1,X2,Y2,HPX,HPY,M1,M2,M3)
C     ===============================================
C1                       D, D, D, D,  D,  D, D, D, D
C1                       I,  I, I, I, I,  I, O, O, O
C2      Subroutine CV00L6 returns the line (M1,M2,M3)
C2      which passes through the point (HPX,HPY)
C2      and is perpendicular to line (X1,Y1,X2,Y2).
C
      DOUBLE PRECISION HPX,HPY,L1,L2,L3,M1,M2,M3,X1,Y1,X2,Y2
      EXTERNAL DCVL14,DVV0L6
C
      CALL DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
      CALL DVV0L6 (L1,L2,L3,HPX,HPY,M1,M2,M3)
C
      END
*
      SUBROUTINE DCV0L7(X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,M1,M2,M3)
C     ============================================================
C1    vartype           D  D  D  D  D  D     D    D   D  D  D  D
C1    iostatus          I  I  I  I  I  I     I    I   I  O  O  O
C
C2    subroutine DCV0L7 returns the line vector M which
C2    is tangent to the circle XC,YC,RADIUS closest to
C2    hit point HPX,HPY and parallel to the line through
C2    X1,Y1,X2,Y2.
C
      DOUBLE PRECISION X1,Y1,X2,Y2,XC,YC,RADIUS,HPX,HPY,M1,M2,M3
      DOUBLE PRECISION L1,L2,L3
C
      EXTERNAL DCV0L4,DVV0C1
C
C3    generate a vector parallel to original line
C3    through the hit point
      CALL DCV0L4(X1,Y1,X2,Y2,HPX,HPY,L1,L2,L3)
C3    generate the vector parallel to L and the
C3    nearest tangent to circle
      CALL DVV0C1(L1,L2,L3,XC,YC,RADIUS,M1,M2,M3)
C3    return with required vector in M
C
      END
C
C
      SUBROUTINE DCV0L8(X,Y,XC,YC,RAD,C1,C2,C3,L1,L2,L3,OK)
C     =====================================================
C1                      D,D, D, D,  D, D, D, D, D, D, D, L
C1                      I,I, I, I,  I, O, O, O, O, O, O, O
C
C2      Subroutine DCV0L8 calculates the tangent line from X,Y
C2      to the circle XC,YC (RAD) returning the values of the the
C2      end point (X1,Y1) which lies on the circumference of the
C2      circle.
C
      DOUBLE PRECISION X,Y,XC,YC,RAD,
     +      L1,L2,L3,C1,C2,C3,DIST,
     1      X0,SX0,Y0,SY0,ONE
      LOGICAL OK,DSAME
      INTRINSIC ASIN,DSIGN,SQRT,ABS
      EXTERNAL DSWAP,DSAME
C
C       Find the distance between the point and the centre
C       of the circle
C      WRITE(10,*) X,Y,XC,YC,RAD
      DIST=0.0
      IF ( DSAME(RAD,DIST) ) THEN
         CALL DCVL14(XC,YC,X,Y,L1,L2,L3)
         CALL DCVL14(XC,YC,X,Y,C1,C2,C3)
         OK=.TRUE.
         RETURN
      END IF
C
      X0=X-XC
      Y0=Y-YC
      DIST=X0**2+Y0**2-RAD**2
C     Check to see the start point does not lie within the circle
      OK=DIST .GT. 0.0
C
      IF ( .NOT. OK ) RETURN
C
      DIST=SQRT(DIST)
      ONE=1.0
      SY0=DSIGN(ONE,Y0)
      SX0=DSIGN(ONE,X0)
C
      C1=  Y0*ABS(X0)+(RAD*DIST*SY0)
      L1=-(RAD**2-Y0**2)*SY0
 
      L2=-(X0*ABS(Y0)+(RAD*DIST*SX0))
      C2= (RAD**2-X0**2)*SX0
C
      C3=-X*C1-Y*C2
      L3=-X*L1-Y*L2
C
      IF ( C1*X0+C2*Y0 .LT. 0.0  ) THEN
         CALL DSWAP(L1,C1)
         CALL DSWAP(L2,C2)
         CALL DSWAP(L3,C3)
      END IF
C
C      WRITE(10,*) '[DCV0L8] V1:',L1,L2,L3
C      WRITE(10,*) '[DCV0L8] V2:',C1,C2,C3
 
      END
C
C
      SUBROUTINE DCV1L8(X,Y,HX,HY,XC,YC,RAD,L1,L2,L3,OK)
C     ==================================================
C1     vartype          D,D, D, D, D, D,  D, D, D, D, L
C1     iostatus         I,I, I, I, I, I,  I, O, O, O, O
C
C2      Subrountine DCV1L8 calculates the tangent line from X,Y
C2      to the circle XC,YC (RAD) returning the line vector which
C2      is closest to hit point HX,HY
C
      DOUBLE PRECISION X,Y,HX,HY,XC,YC,RAD,L1,L2,L3,
     +                 C1,C2,C3,DVDD13
      LOGICAL OK
      REAL ABS
      INTRINSIC ABS
C
      EXTERNAL DCV0L8,DVDD13
C
      CALL  DCV0L8(X,Y,XC,YC,RAD,C1,C2,C3,L1,L2,L3,OK)
C
      IF ( .NOT. OK ) RETURN
C
      IF ( ABS(DVDD13(L1,L2,L3,HX,HY)) .GT.
     +     ABS(DVDD13(C1,C2,C3,HX,HY)) ) THEN
         L1=C1
         L2=C2
         L3=C3
      END IF
C
      END
C
      SUBROUTINE DCVL14(X1,Y1,X2,Y2,L1,L2,L3)
C     =======================================
C
C1                       D, D, D, D, D, D, D
C1                       I, I, I, I, O, O, O
C2      Subroutine CV0L14 return the line vector
C2      (L1,L2,L3) which passes through the points
C2      (X1,Y1) and (X2,Y2)
C
      DOUBLE PRECISION X1,Y1,X2,Y2,L1,L2,L3
C
      L1=  Y2-Y1
C     This is what it should be.
C     L2=-(X2-X1)
      L2=  X1-X2
      L3=-X2*L1-Y2*L2
C
      END
C
      SUBROUTINE DCVL15(X1,Y1,X2,Y2,SEP,L1,L2,L3,N1,N2,N3)
C     ====================================================
C1    vartype           D  D  D  D  D  D     D    D   D  D  D  D
C1    iostatus          I  I  I  I  I  I     I    I   I  O  O  O
C
C2    Subroutine DCVL15 returns the line vectors L1,L2,L3
C2    N1,N2,N3 which are parallel to (X1,Y1) (X2,Y2)
C2    at a distance of SEP on each side of the line.
C
      DOUBLE PRECISION X1,Y1,X2,Y2,SEP
      DOUBLE PRECISION L1,L2,L3,N1,N2,N3,M1,M2,M3
C
      EXTERNAL DCVL14,DVVL15
C
      CALL DCVL14(X1,Y1,X2,Y2,M1,M2,M3)
      CALL DVVL15(M1,M2,M3,SEP,L1,L2,L3,N1,N2,N3)
C
      END
C
      SUBROUTINE DCVL20(XC1,YC1,RAD1,XC2,YC2,RAD2,
     +                  HPX1,HPY1,HPX2,HPY2,C1,C2,C3,OK)
C     ===================================================
C1      All real except OK (logical)
C1                         I,  I,   I,  I,  I,   I,
C1                         I,   I,   I,   I, O, O, O, O
C2      Subroutine LTAN2C find the line that is tangent to
C2      the two circles supplied C1 and C2
C2      HPX1,HPY1 indicates approximately where to start
C2      HPX2,HPY2    "           "          "   "  finish
C
      DOUBLE PRECISION  DIST,P,P1,P2,DVDD13,DMAGLN,NX,NY,A(3,3),
     +     XC1,YC1,RAD1,XC2,YC2,RAD2,RADDIF,RADSUM,S,
     1     HPX1,HPY1,HPX2,HPY2,ANG,HYP,DHP1,DHP2,
     2     L1,L2,L3,C1,C2,C3,DDSTXY,ONE
C
      LOGICAL SWAPPD,OK
      INTRINSIC ASIN
      EXTERNAL DSWAP,DVDD13,DCRSPD,DROTP2,DNEWXY,DLPRLD,
     +         VV00L6,DVC0P5,BELL,HOMOXY,DMAGLN,DDSTXY
C
C       Too ensure drawing line vector from small circle
C       to the larger
		ONE=1.0
      SWAPPD= RAD2 .LT. RAD1
      IF ( SWAPPD ) THEN
         CALL DSWAP(RAD1,RAD2)
         CALL DSWAP(XC1,XC2)
         CALL DSWAP(YC1,YC2)
      END IF
C
      CALL DCRSPD(XC1,YC1,ONE,XC2,YC2,ONE,L1,L2,L3)
      DIST=DMAGLN(L1,L2,L3)
      RADDIF=RAD2-RAD1
      RADSUM=RAD2+RAD1
C
      OK = DIST .GE. RADDIF
      IF( .NOT. OK ) THEN
         IF ( SWAPPD ) THEN
            CALL DSWAP(RAD1,RAD2)
            CALL DSWAP(XC1,XC2)
            CALL DSWAP(YC1,YC2)
         END IF
         CALL BELL
         RETURN
      END IF
      P1=DVDD13(L1,L2,L3,HPX1,HPY1)
      P2=DVDD13(L1,L2,L3,HPX2,HPY2)
C
      IF ( (P1*P2 .GE. 0) .OR. (DIST .LT. RADSUM ) ) THEN
         ANG=ASIN(RADDIF/DIST)
         S=-1.0
         IF ( P1 .GE. 0 ) THEN
            ANG=-ANG
            S= 1.0
         END IF
      ELSE
         HYP=DIST*RAD1/(RAD1+RAD2)
         ANG=ASIN(RAD1/HYP)
         DHP1=DDSTXY(XC1,YC1,HPX1,HPY1)
         DHP2=DDSTXY(XC1,YC1,HPX2,HPY2)
         S=1.0
         P=P1
         IF ( DHP1 .GT . DHP2 ) P=P2
         IF ( P .LT. 0 ) THEN
            ANG=-ANG
            S=-1.0
         END IF
      END IF
C       Rotate about the centre of the smaller diameter
C       circle by the chosen angle ANG
      CALL DROTP2(XC1,YC1,ANG,A)
C       Rotate the centre of the larger circle through the
C       angle about the centre of the smaller circle
      CALL DNEWXY(XC2,YC2,NX,NY,A)
C       Calculate the new line vector which parallel to the
C       vector we want
      CALL DCRSPD(XC1,YC1,ONE,NX,NY,ONE,L1,L2,L3)
C       Offset this line vector by radius of the smaller circle
C       and to side which has been chosen by the Hit point
C       which closest to the first circle indicated.
      CALL DLPRLD(L1,L2,L3,S,RAD1,C1,C2,C3)
C       If the circles properties where swapped round then
C       swap them back again
      IF(SWAPPD) THEN
         CALL DSWAP(RAD1,RAD2)
         CALL DSWAP(XC1,XC2)
         CALL DSWAP(YC1,YC2)
      END IF
C
      END
C
C
      SUBROUTINE DCVL21(XC1,YC1,RAD1,XC2,YC2,RAD2,L,OK)
C     =================================================
C1             All Real except OK (logical)
C1             Input top line,output bottom line
C2      Subroutine CV0L21 returns the four lines which are tangent
C2      to the two circles (XC1,YC1,RAD1) (XC2,YC2,RAD2)
C2      OK is set .false. if no tangents exists
C
C
      DOUBLE PRECISION  XC1,YC1,RAD1,XC2,YC2,RAD2,DDSTXY,C,
     +     RADDIF,DIST,RAD,T1,T2,T3,P1,P2,P3,Q1,Q2,Q3,L(4,3)
      INTEGER COUNT,ODDCHK
      LOGICAL OK,SIGN
      INTRINSIC ABS
      EXTERNAL DDSTXY,DCV0L8,DVVL15
C
C     Find the distance between the circle centres
      C = DDSTXY(XC1,YC1,XC2,YC2)
C
      IF ( RAD1.GT.RAD2) THEN
         CALL DSWAP(RAD1,RAD2)
         CALL DSWAP(XC1,XC2)
         CALL DSWAP(YC1,YC2)
      END IF
C     Find the absolute difference between the radii
      RADDIF=ABS(RAD1-RAD2)
C
C     If the distance between the centres is less than
      OK=C.GE.RADDIF
      IF ( .NOT. OK ) RETURN
C
      DIST=RAD2
      SIGN=RAD1.GE.RAD2
      COUNT=0
      ODDCHK=1
C
 10   CONTINUE
C      WRITE(10,*) 'FIRST LOOP',COUNT,RAD2,SIGN
C     set radius of imaginary circle
      RAD=ABS(RAD1-RAD2)
C     increment number of line vectors to stored
      COUNT=COUNT+1
C
      CALL DCV0L8(XC2,YC2,XC1,YC1,RAD,P1,P2,P3,Q1,Q2,Q3,OK)
      IF (OK) THEN
C        tangent line did exists
         IF ( SIGN ) THEN
            T1 = P1
            T2 = P2
            T3 = P3
         ELSE
            T1 = Q1
            T2 = Q2
            T3 = Q3
         END IF
         CALL DVVL15(T1,T2,T3,DIST,P1,P2,P3,Q1,Q2,Q3)
C
         IF ( SIGN ) THEN
            L(COUNT,1)=Q1
            L(COUNT,2)=Q2
            L(COUNT,3)=Q3
         ELSE
            L(COUNT,1)=P1
            L(COUNT,2)=P2
            L(COUNT,3)=P3
         END IF
C      WRITE(10,*) 'VECTOR',L(COUNT,1),L(COUNT,2),L(COUNT,3)
C      CALL DRAWT(L(COUNT,1),L(COUNT,2),L(COUNT,3),2*COUNT)
C      CALL CPRMXP('Hello:',ANS)
C
      ELSE
         L(COUNT,1)=0.0
         L(COUNT,2)=0.0
         L(COUNT,3)=0.0
         OK=.TRUE.
      END IF
C
      RAD2=-RAD2
C     odd counter indicates only one of two possible
C     lines have been stored.
      IF ( MOD(COUNT,2).EQ.1 ) GOTO 10
      SIGN=.NOT.SIGN
      IF ( COUNT.LT.4 ) GOTO 10
C
      END
C
C
      SUBROUTINE DCVL22(XC1,YC1,RAD1,XC2,YC2,RAD2,
     +                  HPX1,HPY1,HPX2,HPY2,C,OK)
C     ============================================
C1
C
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,DVDD13,ZERO,
     +      HPX1,HPY1,HPX2,HPY2,L(4,3),C(3),TDIST1,TDIST
      INTEGER*4 I
      LOGICAL OK,DSAME
C
      INTRINSIC DABS
      EXTERNAL DVDD13,DCVL21,DSAME
C
      CALL DCVL21(XC1,YC1,RAD1,XC2,YC2,RAD2,L,OK)
C      WRITE(10,*) 'RAD 1',XC1,YC1,RAD1
C      WRITE(10,*) 'RAD 2',XC2,YC2,RAD2
C      WRITE(10,*) 'HP',HPX1,HPY1,HPX2,HPY2
C
      IF ( .NOT. OK ) RETURN
C
C     We have four lines which are tangent to the two
C     arcs/circles
C     WHICH ONE IS IT  ????
C
      ZERO=0.0
      I=0
 11   CONTINUE
      I=I+1
C     If first two elements are zero the vector is not a line.
      IF ( .NOT.(DSAME(L(I,1),ZERO).AND.DSAME(L(I,2),ZERO))  ) THEN
         TDIST1=ABS(DVDD13(L(I,1),L(I,2),L(I,3),HPX1,HPY1)) +
     +          ABS(DVDD13(L(I,1),L(I,2),L(I,3),HPX2,HPY2))
         TDIST=TDIST1
         C(1)=L(I,1)
         C(2)=L(I,2)
         C(3)=L(I,3)
C         WRITE(10,*) '[DCVL22] Pass 1',C(1),C(2),C(3)
C         WRITE(10,*) '[DCVL22] Pass 1',TDIST,I
      ELSE IF ( I .LT. 4 ) THEN
         GOTO 11
      ELSE
         OK=.FALSE.
         RETURN
      END IF
C
 13   CONTINUE
      I=I+1
C
C     If first two elements are zero the vector is not a line.
      IF ( .NOT.(DSAME(L(I,1),ZERO).AND.DSAME(L(I,2),ZERO))  ) THEN
         TDIST1=ABS(DVDD13(L(I,1),L(I,2),L(I,3),HPX1,HPY1)) +
     +          ABS(DVDD13(L(I,1),L(I,2),L(I,3),HPX2,HPY2))
C         WRITE(10,*) '[DCVL22] pass 2',L(I,1),L(I,2),L(I,3)
C         WRITE(10,*) '[DCVL22] Pass 2',TDIST1,I
         IF ( TDIST1 .LT. TDIST ) THEN
            TDIST=TDIST1
            C(1)=L(I,1)
            C(2)=L(I,2)
            C(3)=L(I,3)
         END IF
      END IF
      IF ( I.LT.4 ) GOTO 13
 
C      WRITE(10,*) 'Final choice:',C
      END
C
      FUNCTION   DDTPRD(V1,V2,V3,P1,P2,P3)
C     ====================================
C
C1        D               D, D, D, D, D, D
C1        O               I, I, I, I, I, I
C2      Function DOTPRD returns the dot product
C2      of vectors V (V1,V2,V3) and P (P1,P2,P3).
C
      DOUBLE PRECISION  V1,V2,V3,
     +         P1,P2,P3,DDTPRD
C
      DDTPRD = V1*P1 + V2*P2 + V3*P3
C
      END
C
C
      SUBROUTINE DHOMXY(L1,L2,L3,X,Y)
C     ===============================
C
C1                       D, D, D,D,D
C1                       I, I, I,O,O
C2      Subroutine DHOMXY returns the homogeneous coordinates
C2      of the vector L (L1,L2,L3)
C
      DOUBLE PRECISION L1,L2,L3,X,Y
C
      X=L1/L3
      Y=L2/L3
C
      END
C
C
      SUBROUTINE DLPRLD (L1,L2,L3,S,D,C1,C2,C3)
C     =========================================
C
C1                        D, D, D,D,D, D, D, D
C1                        I, I, I,I,I, O, O, O
C
C2     Subroutine LPRLDS returns the line C(C1,C2,C3)
C2      which is parallel to line L(L1,L2,L3) and is on the
C2      side indicated by the value of S -ve put to left
C2                                       +ve put to right
C
      DOUBLE PRECISION DMAGLN,L1,L2,L3,S,C1,C2,C3,D,DL3
C
C
      EXTERNAL DMAGLN
C
      DL3=D*DMAGLN(L1,L2,L3)
      C1= L1
      C2= L2
      C3= (L3-S*DL3)
C
      END
C
C
 
      FUNCTION   DMAGLN (L1,L2,L3)
C     ============================
C
C1       D                D, D, D
C1       O                I, I, I
C
C2      Function DMAGLN returns the magnitude of the line
C2      vector  L (L1,L2,L3).
C
      DOUBLE PRECISION DMAGLN,L1,L2,L3
      INTRINSIC DSQRT
C
      DMAGLN = DSQRT( L1*L1 + L2*L2 )
C
      END
C
C
      SUBROUTINE DNEWXY(OX,OY,NX,NY,A)
C     ================================
C1                       D, D, D, D,D(3*3)
C1                       I, I, O, O,I
C
C2      Subroutine NEWXY multiples OX by the transformation
C2      matrice A to obtain the transformed value NX
C2      the same also with OY to NY
C
      DOUBLE PRECISION  OX,OY,NX,NY,A(3,3)
C
      NX=OX*A(1,1)+OY*A(2,1)+A(3,1)
      NY=OX*A(1,2)+OY*A(2,2)+A(3,2)
C
      END
C
C
      SUBROUTINE DVC0C7(L1,L2,L3,M1,M2,M3,RAD,XC,YC,NUM,OK)
C     =====================================================
C1      All D.Precision (except OK logical)
C1      Top line input,bottom line output
C2      Subroutine DVC0C8 returns the four fillet centres
C2      which exist for the lines (L1,L2,L3) and (M1,M2,M3),
C2      OK set .false. if parallel lines
C
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,RAD,XC(8),YC(8),
     1     C1,C2,C3,N1,N2,N3
      INTEGER*4 NUM
      LOGICAL OK
      EXTERNAL DVVL15,DVC0P9
C
      CALL DVVL15(L1,L2,L3,RAD,C1,C2,C3,N1,N2,N3)
C
      CALL DVC0P9(M1,M2,(M3+RAD),C1,C2,C3,
     +             XC(1),YC(1),XC(5),YC(5),OK )
      CALL DVC0P9(M1,M2,(M3+RAD),N1,N2,N3,
     +             XC(2),YC(2),XC(6),YC(6),OK )
      CALL DVC0P9(M1,M2,(M3-RAD),C1,C2,C3,
     +             XC(3),YC(3),XC(7),YC(7),OK )
      CALL DVC0P9(M1,M2,(M3-RAD),N1,N2,N3,
     +             XC(4),YC(4),XC(8),YC(8),OK )
C
      END
C
C
      SUBROUTINE DVC0C8(L1,L2,L3,M1,M2,M3,RAD,
     +                  XC1,YC1,XC2,YC2,XC3,YC3,XC4,YC4,OK)
C     =====================================================
C1      All D.Precision (except OK logical)
C1      Top line input,bottom line output
C2      Subroutine DVC0C8 returns the four fillet centres
C2      which exist for the lines (L1,L2,L3) and (M1,M2,M3),
C2      OK set .false. if parallel lines
C
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,RAD,
     +       XC1,YC1,XC2,YC2,XC3,YC3,XC4,YC4,
     1     C1,C2,C3,D1,D2,D3,N1,N2,N3,O1,O2,O3
      LOGICAL OK
      EXTERNAL DVVL15,DVC0P5
C
      CALL DVVL15(L1,L2,L3,RAD,C1,C2,C3,N1,N2,N3)
      CALL DVVL15(M1,M2,M3,RAD,D1,D2,D3,O1,O2,O3)
      CALL DVC0P5(C1,C2,C3,D1,D2,D3,XC1,YC1,OK)
      IF ( .NOT. OK ) RETURN
      CALL DVC0P5(C1,C2,C3,O1,O2,O3,XC2,YC2,OK)
      CALL DVC0P5(D1,D2,D3,N1,N2,N3,XC3,YC3,OK)
      CALL DVC0P5(O1,O2,O3,N1,N2,N3,XC4,YC4,OK)
C
      END
C
C
      SUBROUTINE DVC0P4(X1,Y1,DIST,L1,L2,L3,X2,Y2)
C     ============================================
C
C1                       D, D,   D, D, D, D, D, D
C1                       I, I,   I, I, I, I, O, O
C2      Subroutine DVC0P4 finds X2,Y2 a distance DIST along
C2      along the line L1,L2,L3 from point X1,Y1
c
      DOUBLE PRECISION X1,X2,Y1,Y2,L1,L2,L3,DIST,DMAGLN,TP
      EXTERNAL DMAGLN
C
      TP=DIST/DMAGLN(L1,L2,L3)
      X2=X1-(TP*L2)
      Y2=Y1+(TP*L1)
C
      END
C
C
      SUBROUTINE DVC0P5 (V1,V2,V3,P1,P2,P3,X,Y,OK)
C     ============================================
C1                        D, D, D, D, D, D,D,D, L
C1                        I, I, I, I, I, I,O,O, O
C2      Subroutine DVC0P5 returns the point of intersection
C2      of the two lines V (V1,V2,V3) and P (P1,P2,P3)
C2      returning the point in X,Y
C2      If the lines are parallel then OK = .FALSE.
      DOUBLE PRECISION  V1,V2,V3,
     +  P1,P2,P3,L1,L2,L3,DZERO,X,Y
C
      LOGICAL OK
      INTRINSIC INT
      EXTERNAL DZERO,DCRSPD,DHOMXY
C
C        Find the intersection by using crossproduct
      CALL DCRSPD(V1,V2,V3,P1,P2,P3,L1,L2,L3)
C        If value of third argument is close to zero
C        it is set by function DZERO
      L3=DZERO ( L3 )
      OK=L3.NE.0.0
      IF ( OK ) CALL DHOMXY(L1,L2,L3,X,Y)
C
      END
C
C
      SUBROUTINE DVC0P9(XC,YC,RAD,L1,L2,L3,X1,Y1,X2,Y2,OK)
C     ====================================================
C1                       D, D,  D, D, D, D, D, D, D, D, L
C1                       I, I,  I, I, I, I, O, O, O, O, O
C2      Subroutine DVC0P9 returns the two points which intersect
C2      the circle XC,YC RAD in X1,Y1 and X2,Y2
C2      OK is .false. if the line does not intersect the
C2      circle
      DOUBLE PRECISION XC,YC,RAD,L1,L2,L3,M1,M2,M3,
     +     X,Y,X1,Y1,X2,Y2,DIST,S,DVDD13
      LOGICAL OK,DSAME,LOK
      INTRINSIC SQRT,ABS
      EXTERNAL DVC0P4,DVC0P5,DVDD13,DSAME,DVCPLP
c
C     Check distance from line to centre of circle
      DIST=ABS(DVDD13(L1,L2,L3,XC,YC))
C
      OK = DIST.LT.RAD
      LOK=DSAME(DIST,RAD)
C     If greater than radius line does not intersect circle
      IF ( LOK ) THEN
C         special case of tangency lets get it right
          CALL DVCPLP(L1,L2,L3,XC,YC,X1,Y1)
          X2=X1
          Y2=Y1
          OK = .TRUE.
          RETURN
      END IF
      IF(.NOT.OK) RETURN
C
      S = SQRT ( RAD**2-DIST**2 )
      M2=-L1
      M1= L2
      M3=-L2*XC + L1*YC
      CALL DVC0P5(L1,L2,L3,M1,M2,M3,X,Y,OK)
C
      CALL DVC0P4(X,Y, S,L1,L2,L3,X1,Y1)
      S=-S
      CALL DVC0P4(X,Y,S,L1,L2,L3,X2,Y2)
C
      END
C
C
      SUBROUTINE DVCPLP(L1,L2,L3,X,Y,XP,YP)
C     =====================================
C
C1                       D, D, D,D,D, D, D
C1                       I, I, I,I,I, O, O
C
C2     DCCPLP returns the point XP,YP which lies on the
C2       line L1,L2,L3  and is the shortest distance
C2       from the indicating point X,Y
      DOUBLE PRECISION L1,L2,L3,T1,T2,T3,X,Y,XP,YP
      LOGICAL OK
      EXTERNAL DVV0L6,DVC0P5
C
      CALL DVV0L6(L1,L2,L3,X,Y,T1,T2,T3)
      CALL DVC0P5(T1,T2,T3,L1,L2,L3,XP,YP,OK)
C
      END
C
C
      FUNCTION   DVDD13 (L1,L2,L3,X,Y)
C     ================================
C1       D                D, D, D,D,D
C1       O                I, I, I,I,I
C2      Function DVDD13 returns the perpendicular distance
C2      from a point (X,Y) to the line L (L1,L2,L3)
C
      DOUBLE PRECISION DVDD13,L1,L2,L3,X,Y,DMAGLN,DDTPRD
      EXTERNAL DDTPRD,DMAGLN
C
      DVDD13=(L1*X+L2*Y+L3)/DSQRT(L1*L1+L2*L2)
C      DVDD13=DDTPRD(L1,L2,L3,X,Y,1.0D0)/DMAGLN(L1,L2,L3)
C
      END
C
      FUNCTION   DVDD14 (X1,Y1,L1,L2,L3,X,Y)
C     ================================
C1       D                D, D, D,D,D
C1       O                I, I, I,I,I
C2      Function DVDD13 returns signed distance
C2      from a point (X,Y) to the line L (L1,L2,L3)
C
      DOUBLE PRECISION DVDD14,L1,L2,L3,X,Y,DMAGLN,DDTPRD,
     +                        NL1,NL2,NL3,X1,Y1,DVDD13
      EXTERNAL DDTPRD,DMAGLN,DVDD13
C
      CALL DVV0L6 (L1,L2,L3,X1,Y1,NL1,NL2,NL3)
      DVDD14 = -(DVDD13(NL1,NL2,NL3,X,Y))
C
      END
C
C
      FUNCTION   DVN0D6(V1,V2,V3,L1,L2,L3)
C     ====================================
C1       D               D, D, D, D, D, D
C1       O               I, I, I, I, I, I
C
C2      Function VN00D6 returns the angle between the two line
C2      line vectors in radians
C2      V1,V2,V3 the first line vector
C2      L1,L2,L3  "  second  "     "
C
      DOUBLE PRECISION L1,L2,L3,V1,V2,V3,
     +       DMAGLN,DVN0D6,PI2,CHECK
      INTRINSIC ACOS,ATAN
      EXTERNAL DMAGLN
C
      PI2= 8.0D0*ATAN(1.0D0)
      DVN0D6=L1*V1+L2*V2
      DVN0D6=DVN0D6/DMAGLN(L1,L2,L3)
      DVN0D6=DVN0D6/DMAGLN(V1,V2,V3)
      DVN0D6=ACOS(DVN0D6)
      CHECK=V1*L2-V2*L1
      IF ( CHECK .LT. 0 ) DVN0D6=PI2-DVN0D6
C
      END
C
C
      SUBROUTINE DVV0C1(L1,L2,L3,XC,YC,RADIUS,M1,M2,M3)
C     =================================================
C1    vartype           D  D  D  D  D    D    D  D  D
C1    iostatus          I  I  I  I  I    I    O  O  O
C
C2    subroutine DVV0C1 returns the line vector M
C2    parallel to L which is the closest tangent
C2    to the circle XC,YC,RADIUS
C
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,XC,YC,RADIUS
      DOUBLE PRECISION XP,YP,XT,YT
C
      EXTERNAL DVCPLP,DCCPAP,DVV0L4
C
C3    find the point on L closest to arc centre
      CALL DVCPLP(L1,L2,L3,XC,YC,XP,YP)
C3    find the tangent point on the circumference
C3    of the arc closest to XP,YP
      CALL DCCPAP(XC,YC,RADIUS,XP,YP,XT,YT)
C3    find the vector through the tangent point
C3    parallel to L (ie the tangent vector)
      CALL DVV0L4(XT,YT,L1,L2,L3,M1,M2,M3)
C3    return with tangent vector in M
C
      END
C
C
      SUBROUTINE DVV0L4(HPX,HPY,L1,L2,L3,M1,M2,M3)
C     ============================================
C1                        D,  D, D, D, D, D, D, D
C1                        I,  I, I, I, I, O, O, O
C2      Subroutine VV00L4 returns the line (M1,M2,M3)
C2      which passes through the point (HPX,HPY)
C2      and is parallel to line (L1,L2,L3).
C
      DOUBLE PRECISION HPX,HPY,L1,L2,L3,M1,M2,M3
C
      M1=L1
      M2=L2
      M3= -L1*HPX - L2*HPY
C
      END
C
C
      SUBROUTINE DVV0L5(L1,L2,L3,X,Y,ANG,C1,C2,C3)
C     ============================================
C1                       D, D, D,D,D,  D, D, D, D
C1                       I, I, I,I,I,  I, O, O, O
C2      Subroutine VV00L5 returns the line vector (C1,C2,C3)
C2      which is at an angle of ANG radians to the line
C2      (L1,L2,L3) and passing through  the point X,Y
C
      DOUBLE PRECISION L1,L2,L3,X,Y,ANG,C1,C2,C3,COSAN,SINAN,DZERO
      INTRINSIC DCOS,DSIN
      EXTERNAL DZERO
C
      COSAN=DCOS(ANG)
      SINAN=DSIN(ANG)
C
      C1 = DZERO(L1*COSAN - L2*SINAN)
      C2 = DZERO(L1*SINAN + L2*COSAN)
      C3 = DZERO(-C1*X    - C2*Y    )
C
      END
C
C
      SUBROUTINE DVV0L6 (L1,L2,L3,X,Y,C1,C2,C3)
C     =========================================
C1                        D, D, D,D,D, D, D, D
C1                        I, I, I,I,I, I, O, O
C2      Subroutine DVV0L6 returns the vector C (C1,C2,C3)
C2      which is perpendicular to L (L1,L2,L3) and passes
C2      through the point P (X,Y)
C
      DOUBLE PRECISION L1,L2,L3,C1,C2,C3,X,Y,DZERO
      EXTERNAL DZERO
C
      C1=L2
      C2=-L1
      C3 = DZERO(L1*Y - L2*X)
C
      END
C
C
      SUBROUTINE DVVL15(L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3)
C     =================================================
C1                       D, D, D,  D, D, D, D, D, D, D
C1                       I, I, I,  I, O, O, O, O, O, O
C2      Subroutine DVVL15 returns the two (C1,C2,C3),
C2      (M1,M2,M3) line which are parallel to line
C2      (L1,L2,L3) and separated by the value SEP
C
      DOUBLE PRECISION L1,L2,L3,SEP,C1,C2,C3,M1,M2,M3,DMAGLN,SIZE
      EXTERNAL DMAGLN
C
      SIZE=SEP*DMAGLN(L1,L2,L3)
      C1=L1
      M1=L1
      C2=L2
      M2=L2
      C3=L3-SIZE
      M3=L3+SIZE
C
      END
C
C
C
C
      SUBROUTINE MCC0P9(XC1,YC1,RAD1,RAD2,INCA,
     +                  X1,Y1,X2,Y2,XP1,YP1,XP2,YP2,OK)
C     ==============================================================
C1                        D,  D,  D, D, D, D, D,  D,  D,  D,  D, D
C1                        I,  I,  I, I, I, I, I,  O,  O,  O,  O, O
C2      Subroutine DCC0P9 returns the two points (XP1,YP1)
C2      (XP2,YP2) which intersect the line (X1,Y1 X2,Y2) and
C2      and circle (XC1,YC1) RAD1
C
      DOUBLE PRECISION  XC1,YC1,RAD1,X1,Y1,X2,Y2,
     +                  XP1,YP1,XP2,YP2,L1,L2,L3
      DOUBLE PRECISION RAD2,INCA,STA,ENDA
      REAL PI
      DOUBLE PRECISION DPI
      INTEGER*4 ST
      LOGICAL OK
      EXTERNAL DEI0LE,PI,DPI
C
      IF(RAD1.NE.RAD2) THEN
         CALL ECC0P9(XC1,YC1,RAD1,RAD2,INCA,X1,Y1,X2,Y2,
     +               XP1,YP1,XP2,YP2,OK)
      ELSE 
         CALL DCC0P9(XC1,YC1,RAD1,X1,Y1,X2,Y2,XP1,YP1,XP2,YP2,OK)
      END IF
C
      END
C
C
      SUBROUTINE MCCP19(XC1,YC1,RAD1,RAD2,INCA,
     +                  XC2,YC2,RAD3,RAD4,INCA1,
     +                  X1,Y1,X2,Y2,X3,Y3,X4,Y4,ST,OK)
C     ===========================================================
C1                        D,  D,   D,  D,  D,   D, D, D, D, D, L
C1                        I,  I,   I,  I,  I,   I, O, O, O, O, O
C2      Subroutine CC0P19 returns the two points which intersect
C2      the two circles C1 and C2
C2      OK is .false. if the two circles do not intersect
C
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,
     +   X1,Y1,X2,Y2,X3,Y3,X4,Y4,DDSTXY,M1,M2,M3,K,S
      LOGICAL OK,DSAME
      INTRINSIC ABS
C
      DOUBLE PRECISION  XP1,YP1,XP2,YP2,L1,L2,L3
      DOUBLE PRECISION RAD3,RAD4,INCA,INCA1,STA,STA1,ENDA,ENDA1
      REAL PI
      DOUBLE PRECISION DPI
      EXTERNAL DDSTXY,DCC0P9,DSAME,DVC0P9
C
      INTEGER*4 ST
      EXTERNAL DEI0EE,PI,DPI
C
      IF((RAD1.NE.RAD2).OR.(RAD3.NE.RAD4)) THEN
         CALL ECCP19(XC1,YC1,RAD1,RAD2,INCA,
     +               XC2,YC2,RAD3,RAD4,INCA1,
     +               X1,Y1,X2,Y2,X3,Y3,X4,Y4,ST,OK)
         OK = (ST.GT.0)
      ELSE 
         CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD3,X1,Y1,X2,Y2,OK)
         IF (OK) ST = 2
C                                   
      END IF
C
      END
C
C
      SUBROUTINE MVC0P9(XC1,YC1,RAD1,RAD2,INCA,L1,L2,L3,
     +                  XP1,YP1,XP2,YP2,OK)
C     ====================================================
C1                       D, D,  D, D, D, D, D, D, D, D, L
C1                       I, I,  I, I, I, I, O, O, O, O, O
C2      Subroutine DVC0P9 returns the two points which intersect
C2      the circle XC,YC RAD in X1,Y1 and X2,Y2
C2      OK is .false. if the line does not intersect the
C2      circle
C
      DOUBLE PRECISION  XC1,YC1,RAD1,X1,Y1,X2,Y2,XC,YC,RAD,
     +                  XP1,YP1,XP2,YP2,L1,L2,L3,TX1,TY1
      DOUBLE PRECISION RAD2,INCA,STA,ENDA
      REAL PI
      DOUBLE PRECISION DPI
      INTEGER*4 ST
      LOGICAL OK
      EXTERNAL DEI0LE,PI,DPI
C
      IF(RAD1.NE.RAD2) THEN
         CALL EVC0P9(XC1,YC1,RAD1,RAD2,INCA,L1,L2,L3,
     +               XP1,YP1,XP2,YP2,OK)
      ELSE 
         CALL DVC0P9(XC1,YC1,RAD1,L1,L2,L3,XP1,YP1,XP2,YP2,OK)
      END IF
C
      END
C
C
