C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 autop.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION USEDVC(VECNUM)
C     SUBROUTINE SPCG00(ENTP)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE SPCG00(ENTP)
C     =======================
C1    VARTYPE            I2  
C1    IOSTAT             I   
C
C2    This is the main auto profile routine
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/hdata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
      include  'include/viewport.inc'
C
      REAL X,Y,P1,P2,P3
      REAL XS,YS,XF,YF,ANG,STARTX,STARTY
      REAL CENX,CENY,C1,C2,C3,LDBUFF(6)
      REAL PI2,TBUFF(6),LV1,LV2,LV3,ANGS,ANGF
      REAL ANG1,ANG2,RADIUS,CANG,DIAM,SANG,DIST
      REAL X1,X2,Y1,X3,Y2,XP,YP,DIST1,DIST2,BX(2),BY(2)
C     fuctions
      REAL DOTPRD,MAGLIN,PI ,CURRAD,DISTXY
      LOGICAL SENSE,SEN,PONLL,PONAL,PONLA,PONAA
      LOGICAL VVCON,VACON,AVCON,AACON,REENTR,OPEN
      INTEGER*4 C,REC,VECNUM,VECNT,TFONT,PNUM,DNUM
      INTEGER*4 AMEN,ACELL
      INTEGER*2 SMIP,ENTP,ENT,DFPK,TMIP,LASTP
      INTEGER*2 D1,I,ARCMIP,INTPNT,ITMIP,NUM,STENT,SENTP
      LOGICAL OK1P,OKP2,ARCTPT,DELETE,CONECT,STAT1,SCLOCK
      LOGICAL OK,QUIT,OKP1,SAME,FINISH,SAMEPT,CORVEC,SAMEIP
      LOGICAL USEDVC,DSTOP,RCLOCK,DRAW,CONTIN
      CHARACTER *2 INPL
C
      EXTERNAL DOTPRD,MAGLIN,PI ,SAMEPT
C
C
      IF( LDFILE(CVPN) .LT. 1 ) THEN
          DNUM = 694 
          CALL DEPRNT(DNUM)
          RETURN
      ENDIF
C     **********************************************
C                 MAIN AREA INITIALISAION
C     **********************************************
C     save current autoprofile cell
      ACELL = CELLN
      AMEN = MEN
      CONTIN  =.FALSE.
      VECNUM = 0
      CURVEC = 1
      IPCNT = 0
      RETRY = .FALSE.
      PI2 = PI(2.0)
      TFONT = 1
      SENTP = ENTP
      CALL MULTND()
      LINSTD = .FALSE.
      ARCSTD = .FALSE.
      PRVARC = .FALSE.
      CURARC = .FALSE.
C     **********************************************
C            STARTING ENTITY SENSE AND DIRECTION
C     **********************************************
 20   CONTINUE
      DNUM = 695
      CALL DCPRNT(DNUM)
      DNUM = 696
      CALL DCPRNT(DNUM)
      CALL TCURS( C,X,Y )
C     if we have not hit an entity then return
      IF(CCMD.EQ.'Z') THEN
C         low the auto profile cell
          CALL GTMCLO(MEN,CELLN)
          RETURN
      ENDIF
C     test for valid hatch options
      IF(MEN.EQ.3) THEN
          CALL HATCHM(OKP1)
          IF(.NOT.OKP1) THEN
              CALL GTMCLO(AMEN,ACELL)
              RETURN
          ELSE
              GOTO 20
          ENDIF
      ENDIF
C
      IF (MEN.EQ.2.OR.CCMD.EQ.'q') RETURN
C     search for something
      CALL DSE801( X,Y,OKP1 )
C
      IF(.NOT.OKP1) THEN
         DNUM = 697
         CALL DEPRNT(DNUM)
         GOTO 20
      ENDIF
C     set starting master index
      SMIP = MIP
C     get info
      CALL DER500(SMIP,OK)
C     get a starting direction
100   CONTINUE
C     get main main ifo by a stroke indicating the direction an sense
      DNUM = 698
      CALL DCPRNT(DNUM)
      CALL AUTODR(OK,CLOCK,QUIT,L1,L2,L3)
C
      IF (QUIT) RETURN
      IF(.NOT.OK) THEN
         DNUM = 699
         CALL DEPRNT(DNUM)
         GOTO 100
      ENDIF
C     **********************************************
C          START MAIN PROCESSING AND GET PROFILE
C     **********************************************
C     open scratch file for storing intersection points
      REC=8
      CALL OURSCR(IPUNIT,REC,OK)
      STENT = IMBUFF(2)
      ENT = IMBUFF(2)
      IF (IMBUFF(2) .EQ. LINE) THEN
          XS = RDBUFF(1)
          YS = RDBUFF(2)
          XF = RDBUFF(4)
          YF = RDBUFF(5)
C         get starting point from direction vector
 
          CALL CV0L14 (XS,YS,XF,YF,P1,P2,P3)
 
          CALL VANG(L1,L2,L3,P1,P2,P3,ANG)
 
          IF ( ANG .GT. PI(0.5) ) THEN
 
C             reverse the starting points
              XS = RDBUFF(4)
              YS = RDBUFF(5)
              XF = RDBUFF(1)
              YF = RDBUFF(2)
 
          ENDIF
          CALL CV0L14 (XS,YS,XF,YF,L1,L2,L3)
C         get nearest point on vector
          CALL VC0PLP(L1,L2,L3,X,Y,OX,OY)
 
          PRMIP = SMIP
C         first pass will get an end point
          CALL PASS1(OK)
C
          IF(OK) THEN
 
              OX = XYI(1,1)
              OY = XYI(1,2)
 
          ELSE
              OX = XS
              OY = YS
          ENDIF
          STARTX = OX
          STARTY = OY
C
C
C         gen direction vector
C
          CALL CV0L14(XF,YF,XS,YS,L1,L2,L3)
C
C
C         write out the start vector
          CALL DER500(SMIP,OK)
          WRITE(UNIT=HUNIT,REC=ENTP) SMIP,ENT,X,Y,
     +            OX,OY,XF,YF,YF,
     1       RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          CALL SSFLAG(SMIP,BX(1),BY(1),DFP,D1,.FALSE.)
 
      ELSE IF ( ENT .EQ. ARC ) THEN
 
          XF = RDBUFF(1) + RDBUFF(4) * COS(RDBUFF(6))
          YF = RDBUFF(2) + RDBUFF(4) * SIN(RDBUFF(6))
          XS = RDBUFF(1) + RDBUFF(4) * COS(RDBUFF(5))
          YS = RDBUFF(2) + RDBUFF(4) * SIN(RDBUFF(5))
 
          CENX = RDBUFF(1)
          CENY = RDBUFF(2)
          RADIUS = RDBUFF(4)
 
          DIST = 50
C         generate the start vector
          CALL VV00L6 (L1,L2,L3,CENX,CENY,P1,P2,P3)
          CALL VC00P5 (L1,L2,L3,P1,P2,P3,XP,YP,OK)
          CALL CV0L14 (CENX,CENY,XP,YP,P1,P2,P3 )
          CALL VV00L6 (P1,P2,P3,XP,YP,C1,C2,C3)
          CALL VANG(L1,L2,L3,C1,C2,C3,ANG)
          CALL VC00P4( XP,YP,DIST,C1,C2,C3,OX,OY )
 
          PCLOCK = ANG.LT.PI(0.5)
          RCLOCK = PCLOCK
          CALL CV0L14 (CENX,CENY,X,Y,P1,P2,P3 )
          CALL VC00P4( CENX,CENY,RADIUS,P1,P2,P3,OX,OY )
 
          PCLOCK = .NOT.PCLOCK
          PRMIP = SMIP
C         first pass will get an end point
 
          CALL PASS1(OK)
C
          IF(OK) THEN
 
              OX = XYI(1,1)
              OY = XYI(1,2)
 
          ELSE
              IF(RCLOCK) THEN
                  OX = XF
                  OY = YF
              ELSE
                  OX = XS
                  OY = YS
              ENDIF
 
          ENDIF
          CALL DER500(SMIP,OK)
          PCLOCK = RCLOCK
          IF(PCLOCK) THEN
              ANGF = CANG(RDBUFF(1),RDBUFF(2),OX,OY)
              ANGS = RDBUFF(5)
          ELSE
              ANGS = CANG(RDBUFF(1),RDBUFF(2),OX,OY)
              ANGF = RDBUFF(6)
          ENDIF
          STARTX = OX
          STARTY = OY
 
          WRITE(UNIT=HUNIT,REC=ENTP) SMIP,ENT,OX,OY,
     +        RDBUFF(1),RDBUFF(2),RDBUFF(4),ANGS,ANGF,
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          CALL SSFLAG(SMIP,BX(1),BY(1),DFP,D1,.FALSE.)
      ENDIF
 
 
C     reset all flags
      CALL MULTND()
      VECTOT = 0
      CALL ALLDRW(STENT,SMIP)
 40   CONTINUE
      PNUM = 520
      IF(DSTOP('Q',PNUM)) THEN
          ENTP=ENTP-1
          RETURN
      ENDIF
      CRMIP = 0
      CALL PASS1(OK)
C     if this point has been used befor it is illegal except finish
      FINISH = SAMEPT(XYI(1,1),XYI(1,2),STARTX,STARTY)
C
C     check to see if last point was open or closed
      OPENP = .NOT.OK
      IF(.NOT.FINISH) REENTR = SAMEIP(XYI(1,1),XYI(1,2))
C     special case of reentrant profile
      IF(OK.AND.REENTR) THEN
          DNUM = 700
          CALL DEPRNT(DNUM)
          GOTO 300
      ENDIF
      IF(OK) THEN
          CALL DER500(CRMIP,OK)
          ENT = IMBUFF(2)
          LASTP = ENTP
      ELSEIF( USEDVC(VECNUM) ) THEN
C         we are going to back track
 
C         set the last two ents
          NODEC(CURVEC) = .FALSE.
          VECTOT = 0
          OX = PROX
          OY = PROY
          DISTI = 1E38
          CRMIP = 0
          ANGC = PI(2.0)
          ANGW = 0.0
          VCOR = .FALSE.
          PRMIP = REMIP
          RETRY=.TRUE.
          IPCNT = IPCNT -1
          IF(.NOT.CONTIN) THEN
              ENTP = ENTP - 1
          ENDIF
          PCLOCK = RCLOCK
          CURVEC = 0
          L1 = LV1
          L2 = LV2
          L3 = LV3
          GOTO 40
C
      ELSE
          IF(RETRY)  ENTP = ENTP+1
          IF(OPENP) THEN
              DNUM = 701
              CALL DEPRNT(DNUM)
              CALL PRODRW(ENTP)
          ELSE     
              DNUM = 702
              CALL DEPRNT(DNUM)
          ENDIF
          GOTO 300
C
      ENDIF
C
C
C     current intersection point
      XP = XYI(1,1)
      YP = XYI(1,2)
C
C     **********************************************
C     IF WE HAVE A FINISH POINT THEN MODIFY LAST ENT
C     **********************************************
C
      IF(FINISH) THEN
          READ(UNIT=HUNIT,REC=ENTP) CRMIP,ENT,OX,OY,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          IF(ENT.EQ.LINE) THEN
              TBUFF(4) = XP
              TBUFF(5) = YP
          ELSEIF(ENT.EQ.ARC) THEN
              IF(PCLOCK) THEN
                  TBUFF(5) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ELSE
                  TBUFF(6) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ENDIF
          ENDIF
 
          WRITE(UNIT=HUNIT,REC=ENTP) CRMIP,ENT,OX,OY,
     +    TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1    RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          CALL PRODRW(ENTP)
 
          GOTO 300
 
      ENDIF
 
      IF(ENT.EQ.LINE) THEN
 
C     ****************************************
C         CHOSEN ENTITY WAS AN LINE
C     ****************************************
 
 
          ENTP = ENTP+1
          IF(CRMIP.NE.PRMIP) THEN
              WRITE(UNIT=HUNIT,REC=ENTP) CRMIP,ENT,OX,OY,
     +          ENBUFF(1),ENBUFF(2),ENBUFF(4),ENBUFF(5),ENBUFF(6),
     1          RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
                CALL SSFLAG(CRMIP,BX(1),BY(1),DFP,D1,.FALSE.)
 
              CONTIN = .FALSE.
          ELSE
              CONTIN = .TRUE.
          ENDIF
C         section used for any subsequent retries
          RCLOCK = PCLOCK
          REMIP = PRMIP
          PRMIP = CRMIP
          REDIST = DISTI
          PROX = OX
          PROY = OY
          LV1= L1
          LV2= L2
          LV3= L3
          VECNUM = VECTOT
          DRAW = .NOT.RETRY
          RETRY = .FALSE.
C          WRITE(10,*) '[AUTOP] OX = ',OX,OY
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(1)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(2)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(3)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(4)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(5)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(6)
C          WRITE(10,*) '[AUTOP] XYI= ',XYI(1,1),XYI(1,2)
          OX = XYI(1,1)
          OY = XYI(1,2)
          CALL CV0L14(ENBUFF(4),ENBUFF(5),OX,OY,L1,L2,L3)
          CALL IPWRT(OX,OY)
C          WRITE(10,*) '[AUTOP] L1 L2 L3 ',L1,L2,L3
 
 
C         *******************************************
C                    MODIFY LAST ENTITY
C         *******************************************
C
          READ(UNIT=HUNIT,REC=ENTP-1) CRMIP,ENT,X,Y,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          IF(ENT.EQ.LINE) THEN
 
              TBUFF(4) = XP
              TBUFF(5) = YP
          ELSE IF(ENT.EQ.ARC) THEN
              IF(PCLOCK) THEN
                  TBUFF(5) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ELSE
                  TBUFF(6) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ENDIF
          ENDIF
          WRITE(UNIT=HUNIT,REC=ENTP-1) CRMIP,ENT,X,Y,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 
 
          LASTP = ENTP-1
          IF(CONTIN) ENTP = ENTP -1
C         this is not an arc
          PRVARC = .FALSE.
          IF(DRAW) CALL PRODRW(LASTP)
C     ****************************************
C         CHOSEN ENTITY WAS AN ARC
C     ****************************************
 
      ELSE IF(ENT.EQ.ARC) THEN
 
          ENTP = ENTP+1
          IF(CRMIP.NE.PRMIP) THEN
              WRITE(UNIT=HUNIT,REC=ENTP) CRMIP,ENT,OX,OY,
     +              ENBUFF(1),ENBUFF(2),ENBUFF(4),ENBUFF(5),ENBUFF(6),
     1              RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
              CALL SSFLAG(CRMIP,BX(1),BY(1),DFP,D1,.FALSE.)
 
              CONTIN = .FALSE.
          ELSE
              CONTIN = .TRUE.
          ENDIF
 
C         section used for any subsequent retries
          LV1= L1
          LV2= L2
          LV3= L3
          RCLOCK = PCLOCK
          REMIP = PRMIP
          PRMIP = CRMIP
          REDIST = DISTI
          PROX = OX
          PROY = OY
          VECNUM = VECTOT
          DRAW = .NOT.RETRY
          RETRY = .FALSE.
C          WRITE(10,*) '[AUTOP] OX = ',OX,OY
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(1)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(2)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(3)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(4)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(5)
C          WRITE(10,*) '[AUTOP] EB= ',ENBUFF(6)
C          WRITE(10,*) '[AUTOP] XYI= ',XYI(1,1),XYI(1,2)
          OX = XYI(1,1)
          OY = XYI(1,2)
          CALL IPWRT(OX,OY)
 
 
 
C         *******************************************
C                    MODIFY LAST ENTITY
C         *******************************************
C
          READ(UNIT=HUNIT,REC=ENTP-1) CRMIP,ENT,X,Y,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
          IF(ENT.EQ.LINE) THEN
 
              TBUFF(4) = XP
              TBUFF(5) = YP
 
          ELSE IF(ENT.EQ.ARC) THEN
              IF(PCLOCK) THEN
                  TBUFF(5) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ELSE
                  TBUFF(6) = CANG(TBUFF(1),TBUFF(2),XP,YP)
              ENDIF
          ENDIF
          WRITE(UNIT=HUNIT,REC=ENTP-1) CRMIP,ENT,X,Y,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 
          LASTP = ENTP-1
          IF(CONTIN) ENTP = ENTP -1
          IF(DRAW) CALL PRODRW(LASTP)
          RCLOCK = PCLOCK
          PCLOCK = ACLOCK
          PRVARC = .TRUE.
 
 
 
      ENDIF
 
      IF( .NOT.FINISH ) GOTO 40
 300  CONTINUE
C      CALL CPRMXP('PROFILE COMPLETE',INPL)
      ENTP = ENTP + 1
      CLOSE(UNIT= IPUNIT)
 
 
      END
 
 
 
      LOGICAL FUNCTION USEDVC(VECNUM)
      include 'include/profile.inc'
 
 
      INTEGER*4 VECNUM,I
 
 
      IF(CURVEC.EQ.0) THEN
          USEDVC = .FALSE.
          RETURN
      ENDIF
 
      DO 10 I=1,VECNUM
          IF(NODEC(I)) THEN
              USEDVC= .TRUE.
              RETURN
          ENDIF
 10   CONTINUE
      USEDVC = .FALSE.
 
      END
 
 
