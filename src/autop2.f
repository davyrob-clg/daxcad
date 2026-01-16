C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 autop2.f   */
C

      LOGICAL FUNCTION AAVAL(BUFF,X1,Y1,X2,Y2,OKP1,OKP2 )
C     ====================================================
C1    VARTYPE                 R*6   R  R  R  R  L    L
C1    IOSTATUS                 I  I/O I/O I  I  I  I    I
C
C2    This function returns a valid point for ARCS
 
      include 'include/profile.inc'
C
      REAL X1,Y1,DIST,X2,Y2,ANG,DISTXY,NONE,ZERO
      REAL VN00D6,PI,BUFF(6),PI2,VAL
      REAL P1,P2,P3,V1,V2,V3,XS,YS,XF,YF
      REAL ANG1,ANG2,CANG,ANGI,DIST1,DIST2
      INTEGER*4 I
 
      LOGICAL SAMEPT,SAMEV,FULCIR,OKP1,OKP2,ARCTPT
      EXTERNAL ZERO
 
C     set to true
      NONE = 0.0
      IF(SAMEPT(OX,OY,X1,Y1)) THEN
C         same point as last time
          OKP1 = .FALSE.
      ELSE IF(SAMEPT(OX,OY,X2,Y2)) THEN
          OKP2 = .FALSE.
      ENDIF
 
      PI2 = PI(2.0)
      AAVAL = .FALSE.
C     get arc details
      CALL ARCSE(BUFF,XS,YS,XF,YF,FULCIR)
      ANGI = CANG(BUFF(1),BUFF(2),OX,OY)
      ANG1 = CANG(BUFF(1),BUFF(2),X1,Y1)
      ANG2 = CANG(BUFF(1),BUFF(2),X2,Y2)
 
C      WRITE(10,*) '[AAVAL] PCLOCK ANGI=',PCLOCK,ANGI
C      WRITE(10,*) '[AAVAL] BUFF= ',(BUFF(I),I=1,6)
C      WRITE(10,*) '[AAVAL] X1,Y1= ',X1,Y1
      IF(PCLOCK) THEN
 
C         clockwise round arc
          IF(OKP1) then
              IF(.NOT.FULCIR) THEN
                 OKP1 = ARCTPT(BUFF(1),BUFF(2),BUFF(5),ANGI,X1,Y1)
              ENDIF
              DIST1 = MOD(ANGI - ANG1 + PI2 , PI2)*BUFF(4)
          ENDIF
          IF(OKP2) then
              IF(.NOT.FULCIR) THEN
                 OKP2 = ARCTPT(BUFF(1),BUFF(2),BUFF(5),ANGI,X2,Y2)
              ENDIF
              DIST2 = MOD(ANGI - ANG2 + PI2 , PI2)*BUFF(4)
          ENDIF
 
      ELSE
 
C         anti-clockwise round arc
          IF(OKP1) then
              IF(.NOT.FULCIR) THEN
                 OKP1 = ARCTPT(BUFF(1),BUFF(2),ANGI,BUFF(6),X1,Y1)
              ENDIF
              DIST1 = MOD(ANG1 -ANGI + PI2 , PI2)*BUFF(4)
          ENDIF
          IF(OKP2) then
              IF(.NOT.FULCIR) THEN
                 OKP2 = ARCTPT(BUFF(1),BUFF(2),ANGI,BUFF(6),X2,Y2)
              ENDIF
              DIST2 = MOD(ANG2 - ANGI  + PI2 , PI2)*BUFF(4)
          ENDIF
      ENDIF
 
C     what have we got now
C      WRITE(10,*) '[AAVAL] OKP1 OKP2 = ',OKP1,OKP2
      IF(.NOT.OKP1.AND..NOT.OKP2) THEN
          AAVAL = .FALSE.
          RETURN
      ELSEIF(OKP1.AND.OKP2) THEN
          IF(DIST1.GT.DIST2) THEN
              X1 = X2
              Y1 = Y2
              DIST = DIST2
          ELSE
              DIST = DIST1
          ENDIF
 
      ELSEIF(OKP2) THEN
          DIST = DIST2
          X1 = X2
          Y1 = Y2
      ELSEIF(OKP1) THEN
          DIST = DIST1
      ENDIF
C     after all this we cant use it
C      WRITE(10,*) '[AAVAL] DIST= ',DIST
C      WRITE(10,*) '[AAVAL] X1,Y1=',X1,Y1
      VAL = ZERO(DISTI-DIST)
      IF( .NOT.SAMEV(VAL,NONE) ) THEN
 
           IF(VAL.LT.NONE) THEN
 
              AAVAL = .FALSE.
              RETURN
          ENDIF
      ENDIF
 
 
      IF(RETRY.AND..NOT.SAMEV(DIST,REDIST) )THEN
C         not corect correct node for retry
          AAVAL = .FALSE.
          RETURN
      ENDIF
 
 
      IF(.NOT.SAMEV (DIST,DISTI) )THEN
C        if this is a NEW node then reset vector values
          IF(.NOT.RETRY) CALL MULTND()
          VECTOT = 0
          ARCSTD = .FALSE.
          LINSTD = .FALSE.
          ANGC = PI(2.0)
          ANGW = 0.0
          VCOR = .FALSE.
       ENDIF
C      possible same node
      DISTI = DIST
      AAVAL = .TRUE.
 
      END
 
 
 
C       @(#)  256.1 date 12/16/89 arc2v.ftn Daxcad revision 1.19
      SUBROUTINE ARC2V(BUFF,X1,Y1,X2,Y2,X3,Y3,OKP1,OKP2)
C     ==================================================
C1    VARTYPE           R*6  R  R  R  R  R  R  L    L
C1    IOSTATS            I   O  O  O  O  O  O  O    O
C
C2    Genaerate two vectors coming off circle
 
 
 
      REAL X1,Y1,P1,P2,P3,C1,C2,C3,BUFF(6)
      REAL XS,YS,XF,YF,L1,L2,L3,MAGLIN,DIST
      REAL X3,Y3,X2,Y2
      INTEGER*2 TFONT
 
      LOGICAL OKP1,OKP2,SAMEPT,FULCIR
 
      TFONT = 1
C
      CALL ARCSE(BUFF,XS,YS,XF,YF,FULCIR)
 
C      write(10,*) '[arc2v] fulcir = ',fulcir
C      write(10,*) '[arc2v] x1 y1= ',x1,y1
C      write(10,*) '[arc2v] xs ys= ',xs,ys
C      write(10,*) '[arc2v] xf yf= ',xf,yf
      OKP1 = .FALSE.
      OKP2 = .FALSE.
      IF(.NOT.SAMEPT(X1,Y1,XS,YS) .OR.FULCIR) THEN
 
 
C      write(10,*) '[arc2v] okp1'
C         generate clockwise
          CALL CV0L14(BUFF(1),BUFF(2),X1,Y1,L1,L2,L3)
          CALL VV00L6 (L1,L2,L3,X1,Y1,P1,P2,P3)
          DIST = MAGLIN(P1,P2,P3)
          CALL VC00P4(X1,Y1,DIST,P1,P2,P3,X2,Y2)
          OKP1 = .TRUE.
C          CALL DRWFLW(X1,Y1,X2,Y2,TFONT)
      ENDIF
      IF(.NOT.SAMEPT(X1,Y1,XF,YF) .OR.FULCIR) THEN
 
C      write(10,*) '[arc2v] okp2'
C         generate clockwise
          CALL CV0L14(X1,Y1,BUFF(1),BUFF(2),L1,L2,L3)
          CALL VV00L6 (L1,L2,L3,X1,Y1,C1,C2,C3)
          DIST = MAGLIN(C1,C2,C3)
          CALL VC00P4(X1,Y1,DIST,C1,C2,C3,X3,Y3)
          OKP2 = .TRUE.
C          CALL DRWFLW(X1,Y1,X3,Y3,TFONT)
      ENDIF
 
      END
 
C       @(#)  256.1 date 12/16/89 arcse.ftn Daxcad revision 1.19
      SUBROUTINE ARCSE(BUFF,XS,YS,XF,YF,FULCIR)
C     =========================================
C1    VARTYPE           R*6  R  R  R  R   L
C1    IOSTATS            I   O  O  O  O   O
C
C2    This function will return the start and end points
C2    of a circle and determine whether it is a full circle
C
      REAL BUFF(6),XS,YS,XF,YF,DISTXY,ZERO
      LOGICAL FULCIR,SAMEV
C
C
      ZERO = 0.000000
      XS = BUFF(1) + BUFF(4)*COS(BUFF(5))
      YS = BUFF(2) + BUFF(4)*SIN(BUFF(5))
      XF = BUFF(1) + BUFF(4)*COS(BUFF(6))
      YF = BUFF(2) + BUFF(4)*SIN(BUFF(6))
      FULCIR = SAMEV( (DISTXY(XS,YS,XF,YF)*BUFF(4)) , ZERO)
C
      END
 
C       @(#)  256.1 date 12/16/89 autop3.ftn Daxcad revision 1.19
      SUBROUTINE AUTOP3(TMIP,OK)
 
C2    This routine will test the current and previous entity for
C2    any legal intersect between the two entities. It will then perform
C2    a further test. it stores the closest inbtersection point to the old point
C2    which will be for the test of a second entity which intersects on
C2    the same point. It will either update a global distanc and its current
C2    mip or it will leave well alone.
 
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/hdata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
 
C     **********************************
C               VARIABLES
C     **********************************
C
      REAL P1,P2,P3,C1,C2,C3,ANG,X1,Y1,X2,Y2,X3,Y3,CANG
      REAL SX1,SY1,SX2,SY2
      INTEGER*4 ST
      REAL PI,DISTXY,VN00D6,DIST
      REAL LDBUFF(6)
      REAL V1,V2,V3
      REAL XS,YS,XF,YF,AUX1,AUX2,AUX3,AUX4
      REAL DIST1,DIST2,ZERO,TANG
      REAL CENX,CENY,ANGS,ANGF,RADIUS
 
      LOGICAL SAMEPT,LLVAL,PONLL,USEDVC,STAT,OKP1,OKP2,PONLA
      LOGICAL PONAL,PONAA,AAVAL,SAMEV,TANGNT
 
      INTEGER*2 TMIP,I,ENT
 
      LOGICAL OK,CORVEC,FULCIR,CONECT,LNZERO
 
 
 
C     ***********************************
C                MAIN PROGRAM
C     ***********************************
C
      CALL DER500( PRMIP , OK)
 
C     copy to a new buffer
      DO 200 I =1,6
 
          LDBUFF(I) = RDBUFF(I)
 
 200  CONTINUE
 
      ENT1 = IMBUFF(2)
 
      CALL DER500( TMIP,OK)
 
C     if this is a delete ent then get rid of it
      IF(IMBUFF(1).EQ.100) RETURN
 
      ENT2 = IMBUFF(2)
C     Do a check for a zero length line
      IF(ENT2.EQ.LINE) THEN
          IF(LNZERO(RDBUFF) ) RETURN
      ENDIF
 
C    set istance of current line
 
 
 
      OKP1 = .FALSE.
      OKP2 = .FALSE.
C      CALL CONNEC(ENT1,ENT2,LDBUFF,RDBUFF,X1,Y1,CONECT)
C     stop any unset conditions occurring
C      X2 = X1
C      Y2 = Y1
C     check for point being same as last intersection point
C      IF(CONECT) THEN
C         if so then do intersection 
C          CONECT = .NOT.SAMEPT(X1,Y1,OX,OY)
C      ENDIF
                           
      AUX1 = LDBUFF(4)
      AUX2 = 0.0
      AUX3 = RDBUFF(4)
      AUX4 = 0.0
      CONECT = .FALSE.
      IF(.NOT.CONECT)CALL INTERS(ENT1, LDBUFF(1),LDBUFF(2),LDBUFF(4),
     1            LDBUFF(5),AUX1,AUX2,ENT2,RDBUFF(1),
     2            RDBUFF(2),RDBUFF(4),RDBUFF(5),AUX3,AUX4,
     3            X1,Y1,X2,Y2,SX1,SY1,SX2,SY2,ST,OK)
 
 
      IF (OK) THEN
C
C         intersection occured. does it lie on the entity
 
C**************************************************************
C
C         O L D   L I N E    A N D  A  N E W  L I N E
C
C**************************************************************
          IF(ENT1 .EQ. LINE.AND.ENT2.EQ.LINE) THEN
C
C
C
              CURARC = .FALSE.
C             does the intersection poinmt lies on the line
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X1,Y1
C      WRITE(10,*) '[AUTOP3] INTERSECTION OX,OY= ',OX,OY
              IF ( PONLL(RDBUFF, LDBUFF,X1,Y1) ) THEN
C
C                 can we use this point
                  IF( .NOT.LLVAL(X1,Y1) ) RETURN
C
                  IF(.NOT.SAMEPT(X1,Y1,RDBUFF(1),RDBUFF(2)) ) THEN
C
                      VECTOT = VECTOT + 1
                      IF(RETRY) THEN
                         OK = NODEC(VECTOT)
                      ELSE
                         OK =.TRUE.
                      ENDIF
                      IF(OK) THEN
                          IF( CORVEC(X1,Y1,RDBUFF(1),RDBUFF(2))) THEN
C
                              ENBUFF(4) = RDBUFF(1)
                              ENBUFF(5) = RDBUFF(2)
                              ENBUFF(1) = X1
                              ENBUFF(2) = Y1
                              XYI(1,1) = X1
                              XYI(1,2) = Y1
                              CRMIP = TMIP
                              CURVEC = VECTOT
C
                          ENDIF
                      ENDIF
                  ENDIF
C
                  IF(.NOT.SAMEPT(X1,Y1,RDBUFF(4),RDBUFF(5)) ) THEN
C
                      VECTOT = VECTOT + 1
                      IF(RETRY) THEN
                          OK = NODEC(VECTOT)
                      ELSE
                          OK =.TRUE.
                      ENDIF
                      IF(OK) THEN
                         IF( CORVEC(X1,Y1,RDBUFF(4),RDBUFF(5)) ) THEN
C
                              ENBUFF(4) = RDBUFF(4)
                              ENBUFF(5) = RDBUFF(5)
                              ENBUFF(1) = X1
                              ENBUFF(2) = Y1
                              XYI(1,1) = X1
                              XYI(1,2) = Y1
                              CRMIP = TMIP
                              CURVEC = VECTOT
C
                         ENDIF
                      ENDIF
                  ENDIF
              ENDIF
C**************************************************************
C
C         O L D   L I N E    A N D  A  N E W  A R C
C
C**************************************************************
          ELSE IF(ENT1 .EQ. LINE.AND.ENT2.EQ.ARC) THEN
 
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X1,Y1
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X2,Y2
C      WRITE(10,*) '[AUTOP3] INTERSECTION OX,OY= ',OX,OY
              IF(CONECT) THEN
                 OKP1 = .TRUE.
              ELSE
                 OKP1 = PONLA(LDBUFF, RDBUFF,X1,Y1)
                 OKP2 = PONLA(LDBUFF, RDBUFF,X2,Y2)
              ENDIF
C
C
C      WRITE(10,*) '[AUTOP3] INTERSECTION OKP1 OKP2 ',OKP1,OKP2
C             chose intersection point if two. closest to start
              IF(OKP1.AND.OKP2) THEN
                  ZERO = 0.000000000
                  TANG = DISTXY(X1,Y1,X2,Y2)
                  IF( SAMEV(TANG,ZERO) ) THEN
                      X1 = (X1+X2)/2
                      Y1 = (Y1+Y2)/2
                      OKP2=.FALSE.
                      OKP1=.TRUE.
                  ENDIF
              ENDIF
              IF(SAMEPT(X1,Y1,OX,OY) ) OKP1 = .FALSE.
              IF(SAMEPT(X2,Y2,OX,OY) ) OKP2 = .FALSE.
              IF(.NOT.(OKP1.OR.OKP2) )RETURN
 
C             This could be in the middle in an arc
              IF(OKP1.AND.OKP2) THEN
                  DIST = DISTXY(OX,OY,X1,Y1)
                  CALL CV0L14(OX,OY,X1,Y1,P1,P2,P3)
                  CALL VANG(L1,L2,L3,P1,P2,P3,ANG)
                  OKP1 = (ANG.GT.PI(0.5) )
                  DIST = DISTXY(OX,OY,X2,Y2)
                  CALL CV0L14(OX,OY,X2,Y2,P1,P2,P3)
                  CALL VANG(L1,L2,L3,P1,P2,P3,ANG)
                 OKP2 = (ANG.GT.PI(0.5) )
              ENDIF
C      WRITE(10,*) '[AUTOP3] testing OKP1 OKP2 ',OKP1,OKP2
C      WRITE(10,*) '[AUTOP3] same =',SAMEPT(X1,Y1,X2,Y2)
 
              IF(OKP1.AND.OKP2) THEN
                  DIST1 = DISTXY(OX,OY,X1,Y1)
                  DIST2 = DISTXY(OX,OY,X2,Y2)
                  IF(DIST1.GT.DIST2) THEN
                      X1 = X2
                      Y1 = Y2
                  ENDIF
              ELSEIF(OKP2) THEN
                      X1 = X2
                      Y1 = Y2
              ENDIF
 
C             final test
              IF(SAMEPT(X1,Y1,OX,OY) ) RETURN
 
 
C             can we use this point
              IF( .NOT.LLVAL(X1,Y1) ) RETURN
              CENX = RDBUFF(1)
              CENY = RDBUFF(2)
              RADIUS = RDBUFF(4)
              ANGS = RDBUFF(5)
              ANGF = RDBUFF(6)
C
C             set type flag
              CURARC = .TRUE.
              ARCRAD = RADIUS
C             generate tw vectors of the arc
              CALL ARC2V(RDBUFF,X1,Y1,X2,Y2,X3,Y3,OKP1,OKP2)
C             clockwise vector
              IF(OKP1) THEN
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                     OK = NODEC(VECTOT)
                  ELSE
                     OK =.TRUE.
                  ENDIF
                  IF(OK) THEN
                      ARCCLK = .TRUE.
                      IF( CORVEC(X1,Y1,X2,Y2) ) THEN
                          ENBUFF(6) = CANG(CENX,CENY,X1,Y1)
                          ENBUFF(5) = ANGS
                          ENBUFF(1) = CENX
                          ENBUFF(2) = CENY
                          ENBUFF(4) = RADIUS
                          XYI(1,1) = X1
                          XYI(1,2) = Y1
                          CRMIP = TMIP
                          CURVEC = VECTOT
                          ACLOCK = .TRUE.
 
                      ENDIF
                  ENDIF
              ENDIF
              IF(OKP2) THEN
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                     OK = NODEC(VECTOT)
                  ELSE
                     OK =.TRUE.
                  ENDIF
                  IF(OK) THEN
                      ARCCLK = .FALSE.
                      IF( CORVEC(X1,Y1,X3,Y3) ) THEN
                          ENBUFF(5) = CANG(CENX,CENY,X1,Y1)
                          ENBUFF(6) = ANGF
                          ENBUFF(1) = CENX
                          ENBUFF(2) = CENY
                          ENBUFF(4) = RADIUS
                          XYI(1,1) = X1
                          XYI(1,2) = Y1
                          CRMIP = TMIP
                          CURVEC = VECTOT
                          ACLOCK = .FALSE.
 
                      ENDIF
                  ENDIF
              ENDIF
C**************************************************************
C
C         O L D   A R C  A N D   A   N E W   L I N E
C
C**************************************************************
 
          ELSE IF(ENT1 .EQ. ARC.AND.ENT2.EQ.LINE) THEN
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X1,Y1
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X2,Y2
C      WRITE(10,*) '[AUTOP3] INTERSECTION OX,OY= ',OX,OY
 
              IF(CONECT) THEN
                 OKP1 = .TRUE.
              ELSE
                 OKP1 = PONAL(LDBUFF, RDBUFF,X1,Y1)
                 OKP2 = PONAL(LDBUFF, RDBUFF,X2,Y2)
              ENDIF
C      WRITE(10,*) '[AUTOP3] testing OKP1 OKP2 ',OKP1,OKP2
C
              IF(OKP1.AND.OKP2) THEN
                  ZERO = 0.000000000
                  TANG = DISTXY(X1,Y1,X2,Y2)
                  IF( SAMEV(TANG,ZERO) ) THEN
                      X1 = (X1+X2)/2
                      Y1 = (Y1+Y2)/2
                      OKP2=.FALSE.
                      OKP1=.TRUE.
C                     set tangency flag
                      TANGNT = .TRUE.
                  ENDIF
              ENDIF
C
              IF(SAMEPT(OX,OY,X1,Y1).AND.SAMEPT(OX,OY,X2,Y2)) RETURN
              IF(.NOT.(OKP1.OR.OKP2)) RETURN
C
              IF(.NOT.AAVAL(LDBUFF,X1,Y1,X2,Y2,OKP1,OKP2)) RETURN
C             gebaerate a direction vector at this point on the arc
C             save the current direction vector
              IF(PCLOCK) THEN
                  CALL CV0L14(X1,Y1,LDBUFF(1),LDBUFF(2),P1,P2,P3)
              ELSE
                  CALL CV0L14(LDBUFF(1),LDBUFF(2),X1,Y1,P1,P2,P3)
              ENDIF
C
              CALL VV00L6 (P1,P2,P3,X1,Y1,L1,L2,L3)
C
C      write(10,*) '[autop3] rdbuff= ',(rdbuff(i),i=1,6)
 
              CURARC = .FALSE.
              IF(.NOT.SAMEPT(X1,Y1,RDBUFF(1),RDBUFF(2)) ) THEN
 
C      write(10,*) '[autop3] rdbuff 1 2'
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                     OK = NODEC(VECTOT)
                  ELSE
                     OK =.TRUE.
                  ENDIF
C
                  IF(OK) THEN
 
                      IF( CORVEC(X1,Y1,RDBUFF(1),RDBUFF(2))) THEN
 
                           ENBUFF(4) = RDBUFF(1)
                           ENBUFF(5) = RDBUFF(2)
                           ENBUFF(1) = X1
                           ENBUFF(2) = Y1
                           XYI(1,1) = X1
                           XYI(1,2) = Y1
                           CRMIP = TMIP
                           CURVEC = VECTOT
 
                      ENDIF
                  ENDIF
              ENDIF
              IF(.NOT.SAMEPT(X1,Y1,RDBUFF(4),RDBUFF(5)) ) THEN
 
C      write(10,*) '[autop3] rdbuff 4 5'
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                      OK = NODEC(VECTOT)
                  ELSE
                      OK =.TRUE.
                  ENDIF
                  IF(OK) THEN
                     IF( CORVEC(X1,Y1,RDBUFF(4),RDBUFF(5)) ) THEN
 
                          ENBUFF(4) = RDBUFF(4)
                          ENBUFF(5) = RDBUFF(5)
                          ENBUFF(1) = X1
                          ENBUFF(2) = Y1
                          XYI(1,1) = X1
                          XYI(1,2) = Y1
                          CRMIP = TMIP
                          CURVEC = VECTOT
 
                     ENDIF
                  ENDIF
              ENDIF
C**************************************************************
C
C         O L D   A R C  A N D   A   N E W   A R C
C
C**************************************************************
 
          ELSE IF(ENT1 .EQ. ARC.AND.ENT2.EQ.ARC) THEN
C             which points lie on the intersection
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X1,Y1
C      WRITE(10,*) '[AUTOP3] INTERSECTION X1,Y1= ',X2,Y2
C      WRITE(10,*) '[AUTOP3] INTERSECTION OX,OY= ',OX,OY
              IF(CONECT) THEN
                 OKP1 = .TRUE.
              ELSE
                 OKP1 = PONAA(LDBUFF, RDBUFF,X1,Y1)
                 OKP2 = PONAA(LDBUFF, RDBUFF,X2,Y2)
              ENDIF
C      WRITE(10,*) '[AUTOP3] testing OKP1 OKP2 ',OKP1,OKP2
              IF(OKP1.AND.OKP2) THEN
                  ZERO = 0.000000000
                  TANG = DISTXY(X1,Y1,X2,Y2)
                  IF( SAMEV(TANG,ZERO) ) THEN
                      X1 = (X1+X2)/2
                      Y1 = (Y1+Y2)/2
                      OKP2=.FALSE.
                      OKP1=.TRUE.
                  ENDIF
              ENDIF
 
              IF(SAMEPT(OX,OY,X1,Y1).AND.SAMEPT(OX,OY,X2,Y2)) RETURN
              IF(.NOT.(OKP1.OR.OKP2)) RETURN
C
C      WRITE(10,*) '[AUTOP3] final testing OKP1 OKP2 ',OKP1,OKP2
              IF(.NOT.AAVAL(LDBUFF,X1,Y1,X2,Y2,OKP1,OKP2)) RETURN
C
C
              IF(PCLOCK) THEN
                  CALL CV0L14(X1,Y1,LDBUFF(1),LDBUFF(2),P1,P2,P3)
              ELSE
                  CALL CV0L14(LDBUFF(1),LDBUFF(2),X1,Y1,P1,P2,P3)
              ENDIF
 
              CALL VV00L6 (P1,P2,P3,X1,Y1,L1,L2,L3)
              CENX = RDBUFF(1)
              CENY = RDBUFF(2)
              RADIUS = RDBUFF(4)
              ANGS = RDBUFF(5)
              ANGF = RDBUFF(6)
C
              CURARC = .TRUE.
C             generate two vectors of the arc
              CALL ARC2V(RDBUFF,X1,Y1,X2,Y2,X3,Y3,OKP1,OKP2)
C             clockwise vector
              IF(OKP1) THEN
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                     OK = NODEC(VECTOT)
                  ELSE
                     OK =.TRUE.
                  ENDIF
                  IF(OK) THEN
                      ARCCLK = .TRUE.
                      IF( CORVEC(X1,Y1,X2,Y2) ) THEN
                          ENBUFF(6) = CANG(CENX,CENY,X1,Y1)
                          ENBUFF(5) = ANGS
                          ENBUFF(1) = CENX
                          ENBUFF(2) = CENY
                          ENBUFF(4) = RADIUS
                          XYI(1,1) = X1
                          XYI(1,2) = Y1
                          CRMIP = TMIP
                          CURVEC = VECTOT
                          ACLOCK = .TRUE.
 
                      ENDIF
                  ENDIF
              ENDIF
              IF(OKP2) THEN
                  VECTOT = VECTOT + 1
                  IF(RETRY) THEN
                     OK = NODEC(VECTOT)
                  ELSE
                     OK =.TRUE.
                  ENDIF
                  IF(OK) THEN
                      ARCCLK = .FALSE.
                      IF( CORVEC(X1,Y1,X3,Y3) ) THEN
                          ENBUFF(5) = CANG(CENX,CENY,X1,Y1)
                          ENBUFF(6) = ANGF
                          ENBUFF(1) = CENX
                          ENBUFF(2) = CENY
                          ENBUFF(4) = RADIUS
                          XYI(1,1) = X1
                          XYI(1,2) = Y1
                          CRMIP = TMIP
                          CURVEC = VECTOT
                          ACLOCK = .FALSE.
 
                      ENDIF
                  ENDIF
              ENDIF
 
 
 
 
 
          ENDIF
      ENDIF
C      WRITE(10,*) '[AUTOP3] EXIT CURVEC= ',CURVEC
 
 
 
      END
 
C       @(#)  256.1 date 12/16/89 autop4.ftn Daxcad revision 1.19
      SUBROUTINE AUTOP4()
C     ===================
C1    NO ARGS
C
C2    This routine is designed to use the continuation of the
C2    current vector
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
C
 
C
      INTEGER*2 ENT
      REAL XI,YI,ANG1,ANG2,MAGLIN,D1,D2,D3
      REAL XS,YS,XF,YF,DIST
      REAL C1,C2,C3,P1,P2,P3,X1,Y1,PI
      LOGICAL SAMEPT,OK,CORVEC,FULCIR
C
      CALL DER500(PRMIP,OK)
      ENT = IMBUFF(2)
C     get current intersection point
 
      XI = XYI(1,1)
      YI = XYI(1,2)
      IF(ENT.EQ.LINE) THEN
 
          CURARC = .FALSE.
          IF(SAMEPT( XI,YI,RDBUFF(1),RDBUFF(2)).OR.
     +        SAMEPT( XI,YI,RDBUFF(4),RDBUFF(5) )) RETURN
 
          CALL CV0L14(XI,YI,RDBUFF(1),RDBUFF(2),P1,P2,P3)
          CALL CV0L14(XI,YI,RDBUFF(4),RDBUFF(5),C1,C2,C3)
 
          CALL VANG(L1,L2,L3,P1,P2,P3,ANG1)
          CALL VANG(L1,L2,L3,C1,C2,C3,ANG2)
C      WRITE(10,*) '[AUTOP4] ANG1 ANG2 = ',ANG1,ANG2
          IF(ANG1.GT.PI(0.5) ) THEN
              X1 = RDBUFF(1)
              Y1 = RDBUFF(2)
          ELSE IF(ANG2.GT.PI(0.5) ) THEN
              X1 = RDBUFF(4)
              Y1 = RDBUFF(5)
          ENDIF
 
          VECTOT = VECTOT + 1
          IF(RETRY) THEN
             OK = NODEC(VECTOT)
          ELSE
             OK =.TRUE.
          ENDIF
C      WRITE(10,*) '[AUTOP4] X1 Y1 = ',X1,Y1
          IF(OK) THEN
              IF( CORVEC(XI,YI,X1,Y1)) THEN
 
                  ENBUFF(4) = X1
                  ENBUFF(5) = Y1
                  CRMIP = PRMIP
                  CURVEC = VECTOT
 
              ENDIF
          ENDIF
      ELSE IF(ENT.EQ.ARC) THEN
C
          CURARC = .TRUE.
          ARCRAD = RDBUFF(4)
          ARCCLK = PCLOCK
          CALL ARCSE(RDBUFF,XS,YS,XF,YF,FULCIR)
          IF(.NOT.FULCIR.AND.(SAMEPT( XI,YI,XS,YS).OR.
     +        SAMEPT( XI,YI,XF,YF ))) RETURN
C
C         gen centre vector
          CALL CV0L14(RDBUFF(1),RDBUFF(2),XI,YI,D1,D2,D3)
C
          CALL VV00L6 (D1,D2,D3,XI,YI,C1,C2,C3)
          IF(.NOT.PCLOCK) CALL VECREV(C1,C2,C3)
          DIST = MAGLIN(D1,D2,D3)
          CALL VC00P4 (XI,YI,DIST,C1,C2,C3,X1,Y1)
C
C
          VECTOT = VECTOT + 1
C
          IF(RETRY) THEN
             OK = NODEC(VECTOT)
          ELSE
             OK =.TRUE.
          ENDIF
C      WRITE(10,*) '[AUTOP4] ARC X1 Y1 = ',X1,Y1
          IF(OK) THEN
              IF( CORVEC(XI,YI,X1,Y1)) THEN
C
                  CRMIP = PRMIP
                  CURVEC = VECTOT
C
              ENDIF
          ENDIF
      ENDIF
      END
C
C       @(#)  256.1 date 12/16/89 connec.ftn Daxcad revision 1.19
      SUBROUTINE CONNEC(ENT1,ENT2,BUFF1,BUFF2,X1,Y1,OKP1)
 
      include 'include/entity.inc'
      REAL BUFF1(6),BUFF2(6)
      REAL X1,Y1
      REAL  XS1,YS1,XS2,YS2,XF1,YF1,XF2,YF2
 
      INTEGER*2 ENT1,ENT2
      LOGICAL OK,OKP1,OKP2
      LOGICAL SAMEPT,SAMEV
C
 
 
      IF(ENT1.EQ.LINE.AND.ENT2.EQ.LINE) THEN
C         get points
          XS1 = BUFF1(1)
          YS1 = BUFF1(2)
          XS2 = BUFF1(4)
          YS2 = BUFF1(5)
          XF1 = BUFF2(1)
          YF1 = BUFF2(2)
          XF2 = BUFF2(4)
          YF2 = BUFF2(5)
 
 
      ELSE IF(ENT1.EQ.LINE.AND.ENT2.EQ.ARC) THEN
 
          XS1 = BUFF1(1)
          YS1 = BUFF1(2)
          XS2 = BUFF1(4)
          YS2 = BUFF1(5)
          XF1 = BUFF2(1) + BUFF2(4)*COS(BUFF2(5))
          YF1 = BUFF2(2) + BUFF2(4)*SIN(BUFF2(5))
          XF2 = BUFF2(1) + BUFF2(4)*COS(BUFF2(6))
          YF2 = BUFF2(2) + BUFF2(4)*SIN(BUFF2(6))
 
      ELSE IF(ENT1.EQ.ARC.AND.ENT2.EQ.LINE) THEN
 
          XS1 = BUFF2(1)
          YS1 = BUFF2(2)
          XS2 = BUFF2(4)
          YS2 = BUFF2(5)
          XF1 = BUFF1(1) + BUFF1(4)*COS(BUFF1(5))
          YF1 = BUFF1(2) + BUFF1(4)*SIN(BUFF1(5))
          XF2 = BUFF1(1) + BUFF1(4)*COS(BUFF1(6))
          YF2 = BUFF1(2) + BUFF1(4)*SIN(BUFF1(6))
 
      ELSE IF(ENT1.EQ.ARC.AND.ENT2.EQ.ARC) THEN
 
          XS1 = BUFF2(1) + BUFF2(4)*COS(BUFF2(5))
          YS1 = BUFF2(2) + BUFF2(4)*SIN(BUFF2(5))
          XS2 = BUFF2(1) + BUFF2(4)*COS(BUFF2(6))
          YS2 = BUFF2(2) + BUFF2(4)*SIN(BUFF2(6))
 
          XF1 = BUFF1(1) + BUFF1(4)*COS(BUFF1(5))
          YF1 = BUFF1(2) + BUFF1(4)*SIN(BUFF1(5))
          XF2 = BUFF1(1) + BUFF1(4)*COS(BUFF1(6))
          YF2 = BUFF1(2) + BUFF1(4)*SIN(BUFF1(6))
 
      ENDIF
 
 
      OKP1=SAMEPT(XS1,YS1,XF1,YF1).OR.
     +     SAMEPT(XS1,YS1,XF2,YF2)
 
      OKP2=SAMEPT(XS2,YS2,XF1,YF1).OR.
     +     SAMEPT(XS2,YS2,XF2,YF2)
      IF(OKP1) THEN
          X1 = XS1
          Y1 = YS1
      ELSEIF(OKP2) THEN
          X1 = XS2
          Y1 = YS2
          OKP1 = .TRUE.
      ENDIF
      END
 
C       @(#)  256.1 date 12/16/89 llval.ftn Daxcad revision 1.19
      LOGICAL FUNCTION LLVAL(X1,Y1)
C     =============================
C1    VARTYPE                R  R
C1    IOSTATUS               I  I
C
C2    This function returns a valid point for 2 LINES
 
      include 'include/profile.inc'
C
      REAL X1,Y1,DIST,X2,Y2,ANG,DISTXY,ZERO,NONE
      REAL VN00D6,PI,VAL
      REAL P1,P2,P3,V1,V2,V3
 
      LOGICAL SAMEPT,SAMEV
 
      EXTERNAL ZERO
C     set to true
      NONE = 0.0
      LLVAL = .FALSE.
 
      IF(SAMEPT(OX,OY,X1,Y1)) THEN
 
C         same point as last time
          LLVAL = .FALSE.
          RETURN
 
      ENDIF
 
      DIST = DISTXY(OX,OY,X1,Y1)
      CALL CV0L14(OX,OY,X1,Y1,P1,P2,P3)
 
      CALL VANG(L1,L2,L3,P1,P2,P3,ANG)
C      WRITE(10,*) '[LLVAL] ang = ',ang
C      WRITE(10,*) '[LLVAL] x1 y1 = ',x1,y1
C      WRITE(10,*) '[LLVAL] dist = ',dist
 
 
      IF (ANG.GT.PI(0.5) ) THEN
 
C         This is a valid point
 
C         if we have been here before then
 
 
C          WRITE(10,*) '[LLVAL] DIST DISTI= ',DIST,DISTI
          VAL = ZERO(DISTI-DIST)
          IF( .NOT.SAMEV(VAL,NONE) ) THEN
 
              IF(VAL.LT.NONE) THEN
 
C          WRITE(10,*) '[LLVAL] not chosen'
 
                  LLVAL = .FALSE.
                  RETURN
              ENDIF
          ENDIF
 
          IF(RETRY.AND..NOT.SAMEV(DIST,REDIST) )THEN
C             not corect correct node for retry
              LLVAL = .FALSE.
              RETURN
          ENDIF
 
 
          IF(.NOT.SAMEV (DIST,DISTI) )THEN
C             if this is a NEW node then reset vector values
              IF(.NOT.RETRY) CALL MULTND()
              VECTOT = 0
              ARCSTD = .FALSE.
              LINSTD = .FALSE.
              ANGC = PI(2.0)
              ANGW = 0.0
              VCOR = .FALSE.
          ENDIF
C         possible same node
          DISTI = DIST
          LLVAL = .TRUE.
 
 
      ENDIF
 
      END
 
 
C       @(#)  256.1 date 12/16/89 lnzero.ftn Daxcad revision 1.19
      LOGICAL FUNCTION LNZERO(RDBUFF)
C     ===============================
C1    VARTYPE                   R(6)
C1    IOSTAT                     I
C
C2    This funtion returns true if line is zero length
C2
      LOGICAL SAMEPT
      REAL RDBUFF(6)
C
      LNZERO = .FALSE.
      IF( SAMEPT(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5)) )
     +   LNZERO = .TRUE.
 
      END
 
C       @(#)  256.1 date 12/16/89 pass1.ftn Daxcad revision 1.19
      SUBROUTINE PASS1(OK)
C     ====================
C1    VARTYPE           L
C1    IOSTATUS          I
C
C2    Thsi function will take on pass through the data batse
C2    looking for an intersection with the current entity
C2    and any other. It will also require an old point to take
C2    its dataum. It will store the correct vector depending
C2    on the rules of AUTOPROFILE
C
 
      include 'include/profile.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include  'include/viewport.inc'
 
      REAL PI
      INTEGER*4 CRSTYP
      INTEGER*2 TMIP,DFPK,ENT
      LOGICAL OK,DSTOP
 
 
C     reset all variables
      DISTI = 1E38
      CRMIP = 0
      DFPK=0
      ARCSTD = .FALSE.
      LINSTD = .FALSE.
      ANGC = PI(2.0)
      ANGW = 0.0
      VCOR = .FALSE.
 
      DFP=MOD(INT(DFP),INT(LDFILE(CVPN)-1))+DFPINC
 10   CONTINUE
 
      DFPK=DFPK+1
      CALL RDISPF(DFPK,ENT,TMIP,OK)
C      WRITE(10,*) '[PASS1] TMIP = ',TMIP
 
 
C     conditions we will noit allow
      IF(TMIP.LT.1) GOTO 20
      IF(PRMIP.EQ.TMIP) GOTO 20
C     check for a line or arc
      IF(ENT.EQ.ARC.OR.ENT.EQ.LINE) THEN
C         get a valid intersection hopefully
          CALL AUTOP3(TMIP,OK)
      ENDIF
 
20    CONTINUE
      IF ( DFPK .LT. LDFILE(CVPN)-1 ) GOTO 10
 
C      WRITE(10,*) '[PASS1] CRMIP= ',CRMIP
      IF(CRMIP.GT.0) THEN
C         update crosss file
          CRSTYP = 1
          CALL DRWCRS(XYI(1,1),XYI(1,2),CRSTYP)
      ENDIF
C     set a found flag after this pass
      OK = CRMIP .GT.0
      IF(OK)  CALL AUTOP4()
 
      END
 
 
 
C       @(#)  256.1 date 12/16/89 searc.ftn Daxcad revision 1.19
      SUBROUTINE SEARC(CENX,CENY,RADIUS,XS,YS,XF,YF,BUFF)
C     ==================================================
C1    VARTYPE            R   R     R    R  R  R  R  R*6
C1    IOSTATS            I   I     I    I  I  I  I   O
C
C2    This function is to reconstiute an arc int daxcad format
C2    from start and end points and center + radius
C
      REAL BUFF(6),XS,YS,XF,YF,CENX,CENY,RADIUS,CANG
C
      BUFF(5) = CANG(CENX,CENY,XS,YS)
      BUFF(6) = CANG(CENX,CENY,XF,YF)
      BUFF(4) = RADIUS
      BUFF(1) = CENX
      BUFF(2) = CENY
C
      END
 
