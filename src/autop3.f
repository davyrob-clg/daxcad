C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 autop3.f   */
C

      SUBROUTINE ARROW(OX,OY,L1,L2,L3,DIST)
 
      include 'include/ndata.inc'
 
      REAL L1,L2,L3,DIST,OX,OY,D
      REAL XP1,YP1,XP2,YP2
      REAL P1,P2,P3,X2,Y2
 
      DIST = DIST/10
      CALL VC00P4(OX,OY,DIST,L1,L2,L3,X2,Y2)
 
      CALL VV00L6 (L1,L2,L3,X2,Y2,P1,P2,P3)
      DIST = DIST /5
      CALL VC00P4(X2,Y2,DIST,P1,P2,P3,XP1,YP1)
      CALL VC00P4(X2,Y2,-DIST,P1,P2,P3,XP2,YP2)
 
      CALL DRAWLT(OX,OY,XP1,YP1)
      CALL DRAWLT(XP2,YP2,XP1,YP1)
      CALL DRAWLT(XP2,YP2,OX,OY)
 
 
      END
 
      FUNCTION  CHKLN2(XV1,XV2,X)
C     ==========================
C
C1                       R,  R,R
C1                       I,  I,I
C
C2       CHKLN returns .true. if X lies
C2       between XV1 and XV2
C
      LOGICAL CHKLN2,SAMEV
      REAL  XV1,XV2,X
      EXTERNAL SAMEV
C
      CHKLN2=SAMEV(X,XV1).OR.SAMEV(X,XV2)
C
      IF ( CHKLN2 ) RETURN
C
      IF ( XV2 .GT. XV1 ) THEN
         CHKLN2=X.GE.XV1.AND.X.LE.XV2
      ELSE
         CHKLN2=X.GE.XV2.AND.X.LE.XV1
      END IF
C
      END
 
      LOGICAL FUNCTION CORVEC(X1,Y1,X2,Y2)
C     =====================================
C1    VARTYPE                 R  R  R  R
C1    IOSTAT                  I  I  I  I
C
C2    This function will return a value of true if the sent vector has
C2    been stored acording to the sdet rules of AUTOPROFILE
 
      include 'include/profile.inc'
 
C2    This function will return the correct sense of the vector requred
 
      DOUBLE PRECISION DANG,P1,P2,P3
      REAL PI,XF,YF,DIST,ANG
      REAL X1,Y1,X2,Y2,PI2
      LOGICAL SEN,SENSE,ANGV,SAMEV,SAMEA,ANGS
      LOGICAL ANG0,ANG180,OVER
 
      PI2 = PI(1.0)
C
C     set flag false
      OVER = .TRUE.
      CORVEC = .FALSE.
      CALL DCVL14(DBLE(X1),DBLE(Y1),DBLE(X2),DBLE(Y2),P1,P2,P3)
      CALL DVANG ( DBLE(L1),DBLE(L2),DBLE(L3),P1,P2,P3,DANG)
      SEN = SENSE(CLOCK,L1,L2,L3,X2,Y2)
      ANG = DANG
      ANG0 = SAMEA(0.0,ANG)
      ANG180 = SAMEA(ANG,PI2)
C
C      WRITE(10,*) '[CORVEC] SENSE ANG= ',SEN,ANG
C
C
C     tangency condition exists for this test.
      IF(CURARC) THEN
C         difficult to describes what happens here.
          IF(ANG0) THEN
C             if angle of direction is 0 on main vector
C             and we have an arc then priority depends
C             on the direction of the arc CW or CCW
              IF( (.NOT.CLOCK.AND..NOT.ARCCLK) .OR.
     +           (CLOCK.AND.ARCCLK) ) THEN
                  OVER = .FALSE.
              ELSE
                  ARCSTD = .TRUE.
              ENDIF
          ELSEIF(ANG180) THEN
              IF( (CLOCK.AND..NOT.ARCCLK) .OR.
     +           (.NOT.CLOCK.AND.ARCCLK) ) THEN
                  OVER = .FALSE.
              ELSE
                  ARCSTD = .TRUE.
              ENDIF
          ENDIF
      ENDIF
C     tangency condition exists for this test.
      IF(PRVARC) THEN
C         difficult to describes what happens here.
          IF(ANG0) THEN
C             if angle of direction is 0 on main vector
C             and we have an arc then priority depends
C             on the direction of the arc CW or CCW
              IF( (.NOT.CLOCK.AND..NOT.PCLOCK) .OR.
     +           (CLOCK.AND.PCLOCK) ) THEN
                  OVER = .FALSE.
              ELSE
                  LINSTD = .TRUE.
              ENDIF
          ELSEIF(ANG180) THEN
              IF( (CLOCK.AND..NOT.PCLOCK) .OR.
     +           (.NOT.CLOCK.AND.PCLOCK) ) THEN
                  OVER = .FALSE.
              ELSE
                  LINSTD = .TRUE.
              ENDIF
          ENDIF
      ENDIF
C
C
      IF ((SEN.OR.ANG0.OR.ANG180).AND.OVER) THEN
C
C
C         vector has required sense
          IF( .NOT. VCOR) THEN
C            if no ocrrect senses are store then have it
              ANGC = PI(2.0)
              VCOR = .TRUE.
          ENDIF
C         test for same vector
          ANGS = SAMEA(ANG,ANGC)
C         we allready have the correct line types
          IF(ANGS.AND.(ARCSTD.OR.LINSTD)) RETURN
C         we will allow similar vectors
          IF (ANG .LT. ANGC.OR.ANGS) THEN
C             store this vector
C             current arc is being tested
              CORVEC = .TRUE.
              ANGC = ANG
          ENDIF
 
      ELSE
 
C         if we have a viable vector then piss off
          IF (. NOT. VCOR) THEN
C             test for same vector
              ANGS = SAMEA(ANG,ANGW)
              IF(ANGS.AND.(ARCSTD.OR.LINSTD)) RETURN
              IF (ANG .GT. ANGW.OR.ANGS) THEN
C                store this vector
                 ANGW = ANG
                 CORVEC = .TRUE.
              ENDIF
 
          ENDIF
 
      ENDIF
C     go home
      END
 
      SUBROUTINE DVANG(L1,L2,L3,P1,P2,P3,ANG)
C     =========================================
C1    VARTYPE          D  D  D  D  D  D   D
C1    IOSTST           I  I  I  I  I  I   O
C
C2    The is finds the angle between two vectors
      DOUBLE PRECISION ANG
      DOUBLE PRECISION XS,YS,XF,YF,P1,P2,P3,DOT,MAG1,MAG2,PROD
      DOUBLE PRECISION DMAGLN
      DOUBLE PRECISION L1,L2,L3
 
 
      DOT =  L1*P1 + L2*P2
      MAG1 = DMAGLN(L1,L2,L3)
      MAG2 = DMAGLN(P1,P2,P3)
      PROD = DOT / (MAG1*MAG2)
 
 
      IF (PROD.GT.0.0) PROD = MIN(PROD,DBLE(1.0))
      IF (PROD.LT.0.0) PROD = MAX(PROD,DBLE(-1.0))
      ANG = DABS(DACOS( PROD ))
 
 
      END
 
 
 
 
 
      SUBROUTINE IPWRT(X,Y)
C     =====================
C1    VARTYPE          R R
C1    IOSTAT           I I
C
C2    This sub is used to update the intersection points
 
      include 'include/profile.inc'
 
      REAL X,Y
 
      IPCNT = IPCNT + 1
      WRITE(UNIT=IPUNIT,REC=IPCNT) X,Y
      END
 
      SUBROUTINE MULTND()
C     ===================
C     NO ARGUMENTS
C2    This function sets an array of logicals
C2    for multinode retries
C
      include 'include/profile.inc'
 
      INTEGER*2 I
      DO 10 I=1,200
          NODEC(I) = .TRUE.
 10   CONTINUE
 
      END
 
      LOGICAL FUNCTION PONAA(BUFF1,BUFF2,X,Y)
 
      REAL X,Y,BUFF1(6),BUFF2(6),DIST1,DISTXY,DIST2
      LOGICAL ARCTPT,SAMEV,OK1,OK2
 
 
      DIST1 = DISTXY(BUFF1(1),BUFF1(2),X,Y)
      DIST2 = DISTXY(BUFF2(1),BUFF2(2),X,Y)
      OK1 = SAMEV(BUFF1(4),DIST1)
      OK2 = SAMEV(BUFF2(4),DIST2)
 
 
      PONAA = ARCTPT(BUFF1(1),BUFF1(2),BUFF1(5),BUFF1(6),X,Y).AND.
     +         ARCTPT(BUFF2(1),BUFF2(2),BUFF2(5),BUFF2(6),X,Y).AND.
     +         OK1.AND.OK2
 
 
      END
 
C****************************************************************
 
 
 
 
 
      LOGICAL FUNCTION PONAL (BUFF1,BUFF2,X,Y)
 
      REAL X,Y,BUFF1(6),BUFF2(6)
      INTEGER*4 I
      LOGICAL CHKLN2,ARCTPT
 
 
      PONAL = CHKLN2(BUFF2(1),BUFF2(4),X).AND.CHKLN2(BUFF2(2),
     +         BUFF2(5),Y).AND.ARCTPT(BUFF1(1),BUFF1(2),BUFF1(5),
     +         BUFF1(6),X,Y)
 
 
      END
 
C*******************************************
 
      LOGICAL FUNCTION PONLA(BUFF1,BUFF2,X,Y)
 
      REAL X,Y,BUFF1(6),BUFF2(6)
      INTEGER*4 I
      LOGICAL CHKLN2,ARCTPT
 
 
 
      PONLA = CHKLN2(BUFF1(1),BUFF1(4),X).AND.CHKLN2(BUFF1(2),
     +         BUFF1(5),Y).AND.ARCTPT(BUFF2(1),BUFF2(2),BUFF2(5),
     +         BUFF2(6),X,Y)
 
 
      END
C*******************************************
 
      LOGICAL FUNCTION PONLL(BUFF1,BUFF2,X,Y)
 
      REAL X,Y,BUFF1(6),BUFF2(6)
      LOGICAL CHKLN2
 
 
      PONLL = CHKLN2(BUFF1(1),BUFF1(4),X).AND.CHKLN2(BUFF1(2),
     +         BUFF1(5),Y) .AND. CHKLN2(BUFF2(1),BUFF2(4),X).
     1         AND.CHKLN2(BUFF2(2),BUFF2(5),Y)
 
      END
 
C*******************************************
 
      LOGICAL FUNCTION SAMEA(A1,A2)
C     =============================
C1    VARYYPE                R   R
C1    IAOSTA                 I   I
C
C2    This routine will check for similary without relative tolerance
C2
      REAL A,TOL,A1,A2
C     four decimal places is good enough
      TOL = 1E-4
C
C
      SAMEA = ABS(A1-A2) .LT. TOL
C
      END
C
C
C
      LOGICAL FUNCTION SAMEIP(X,Y)
C     ============================
C1    VARTYPE                 R R
C1    IOSTAT                  I I
C
C2    This function chech to see if anothe intersection point
C2    already found is being used. This is illegal
 
      include 'include/profile.inc'
 
      REAL X,Y,X1,Y1
      INTEGER*2 I
      LOGICAL SAMEPT
 
      DO 10 I=1,IPCNT
 
          READ(UNIT=IPUNIT,REC=I) X1,Y1
          IF( SAMEPT(X,Y,X1,Y1) ) THEN
              SAMEIP = .TRUE.
              RETURN
          ENDIF
 10   CONTINUE
 
      SAMEIP =.FALSE.
      END
 
      LOGICAL FUNCTION SAMEPT(X1,X2,X3,X4)
C     ====================================
C2
 
      include 'include/wtov.inc'
      REAL X1,X2,X3,X4
      REAL WPMAX,WPMIN,DIST
      LOGICAL SAME
      EXTERNAL SAME
 
 
C      write(10,*) '[samept] x1 x2 x3 x4 ',x1,x2,x3,x4
C     Cut accuracy
      WPMAX = WPXMAX
      WPMIN = WPXMIN
      DIST = (WPMAX-WPMIN)*50
      WPXMIN = 0.0
      WPXMAX = DIST
 
      SAMEPT = SAME(X1,X3).AND.SAME(X2,X4)
 
C     reset all values
      WPXMAX = WPMAX
      WPXMIN = WPMIN
 
      END
C
      LOGICAL FUNCTION SAMEV(X,Y)
C     ===========================
C1    VARTYPE                R R
C1    IOSTATUS               I I
C
C2    This function is a accuracy reducer for intersections
 
      include 'include/wtov.inc'
 
      LOGICAL SAME
      REAL X,Y
      REAL WPMAX,WPMIN,DIST
 
      EXTERNAL SAME
 
 
C     Cut accuracy
      WPMAX = WPXMAX
      WPMIN = WPXMIN
      DIST = (WPMAX-WPMIN)*50
      WPXMIN = 0.0
      WPXMAX = DIST
 
      SAMEV = SAME(X,Y)
 
C     reset all values
      WPXMAX = WPMAX
      WPXMIN = WPMIN
 
      END
 
 
 
 
 
      LOGICAL FUNCTION SENSE( CLOCK,L1,L2,L3,X,Y)
C     ===========================================
 
      REAL L1,L2,L3,X,Y,DIST,ZERO,MAGLIN
      LOGICAL CLOCK,SAMEV
 
      EXTERNAL SAMEV
 
 
      ZERO = 0.0000000
c      WRITE(10,*) '[SENSE] L1,L2,L3 ',L1,L2,L3
      DIST = (L1*X + L2*Y + L3)/MAGLIN(L1,L2,L3)
 
C      WRITE(10,*) '[SENSE] CLOCK= ',CLOCK
C      WRITE(10,*) '[SENSE] DIST= ',DIST
      IF ( SAMEV(DIST,ZERO) ) THEN
 
          SENSE = .TRUE.
 
 
      ELSE IF(DIST.LT.0.0.AND.CLOCK) THEN
 
          SENSE = .TRUE.
 
      ELSE IF(DIST.GT.0.0.AND..NOT.CLOCK) THEN
 
          SENSE = .TRUE.
 
      ELSE
 
          SENSE = .FALSE.
 
      ENDIF
 
      END
 
 
 
 
 
 
 
 
      SUBROUTINE VANG(L1,L2,L3,P1,P2,P3,ANG)
C     =========================================
 
      REAL XS,YS,XF,YF,P1,P2,P3,DOT,MAG1,MAG2,PROD,ANG
      REAL MAGLIN
      REAL L1,L2,L3
 
 
      DOT =  L1*P1 + L2*P2
      MAG1 = MAGLIN(L1,L2,L3)
      MAG2 = MAGLIN(P1,P2,P3)
C      WRITE(10,*) '[VANG] DOT MAG1 MAG2  ',DOT,MAG1,MAG2
      PROD = DOT / (MAG1*MAG2)
 
C      WRITE(10,*) '[VANG] PROD ',PROD
 
      IF (PROD.GT.0.0) PROD = MIN(PROD,1.0)
      IF (PROD.LT.0.0) PROD = MAX(PROD,-1.0)
C      WRITE(10,*) '[VANG] PROD 2',PROD
      ANG = ABS(ACOS( PROD ))
 
 
      END
 
      SUBROUTINE VECREV(L1,L2,L3)
 
      REAL   L1,L2,L3
 
      L1 = -L1
      L2 = -L2
      L3 = -L3
 
      END
 
 
