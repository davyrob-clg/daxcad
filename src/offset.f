C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 offset.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION LINEOK(BUFF,X1,Y1,X2,Y2)
C     LOGICAL FUNCTION RADOK(RAD)
C     SUBROUTINE CRTOF0()
C     SUBROUTINE CRTOF1()
C     SUBROUTINE CRTOF3(NUMOFF,NUMARR,NUMP,MULTP,INPL,OK,QUIT)
C     SUBROUTINE CRTOF5(MIP1,MIP2)
C     SUBROUTINE CRTOF6(MIP1)
C     SUBROUTINE CRTOF8(XP,YP,LEFT,RIGHT,OK)
C     SUBROUTINE MNCRTF()
C     SUBROUTINE SORTSC(OX,OY,CLOCK)
C     SUBROUTINE VECOFF(BUFF,LEFT,RIGHT,OFFSET,L1,L2,L3)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CRTOF0()
      CALL MNCRTF
      CALL CRTOF1()
      END
C
      SUBROUTINE CRTOF1()
C     ===================
C
C2    This routine creates an offset to a creatred profile
C2    The return from vd0d13 are
C2    POSITIVE.....RIGHT
C2    NEGATIVE.....LEFT
 
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/hdata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
      include 'include/vntable.inc'
      include 'include/offset.inc'
      include 'include/viewport.inc'
 
      INTEGER*2 ENTP,FN,ENT,ENTO,LASTP,NUM,CUMIP,CUPDP,TMIP
      INTEGER*2 TLAY,FONTI2,P,NLEN,T1,T2,TDFPNT
      INTEGER*2 STACK(3,50),STKCNL
      INTEGER*4 REC,C,PUNIT,I,TFONT,TMEN,TCELL,DUM,SINGCT,MULTCT
      REAL X,Y,XBUFF(6)
      REAL TBUFF(6),DISTXY,VD0D13,DIST,LDBUFF(6)
      REAL MAGLIN
      REAL C1,C2,C3,OFFSET,DUMMY
      REAL V1,V2,V3,CENX,CENY,RAD,M1,M2,M3
      REAL XF,YF,XS,YS,STARTX,STARTY,XP,YP
      REAL FX,FY,SX,SY,X1,Y1,X2,Y2,RADIUS
      REAL SX1,SY1,SX2,SY2,AUX1,AUX2,AUX3,AUX4
      INTEGER*4 IST
      CHARACTER*80 INPL,STRING
      DOUBLE PRECISION DN
      LOGICAL OK,LEFT,RIGHT,MULT,QUIT,OKP1,OKP2,GENP
      LOGICAL SAMEV,SAMEPT,FULCIR,FINISH,PONAA,CLOCKP
      LOGICAL LINEOK,RADOK
      INTEGER*2 TNUMP
      REAL XP1,XP2,YP1,YP2
      REAL ANGS,ANGF,ANG1,ANG2
      REAL CANG,ZERO,TANG,RADL
      REAL XS1,YS1,XS2,YS2,ANG2S,ANG2F,CEN2X,CEN2Y,DIST1,DIST2
      REAL XX,YY
 
      EXTERNAL SAMEV,SAMEPT,NLEN,RADOK,LINEOK
C     ********************************************
C               INITIALISE VARIBLES
C     ********************************************
C
      TARGLY = CLAYER
      STKCNL = 1
      NO = 0
      STRING = ' '
      NUMP = 0
      NUMOFF =0
      GENP= .FALSE.
      MEN = 3
      CCMD = 'N'
      CALL GTHFMC(MEN,CCMD,CELLN)
C     open a scratch to put the profile into
      REC=52
      CALL OURSCR(HUNIT,REC,OK)
      REC = 24
      CALL OURSCR(PUNIT,REC,OK)
      CALL GTMCHI(MEN,CELLN)
      GOTO 2030
C
2000  CONTINUE
C     main control loop
      IF(NO.EQ.0) THEN
          CALL DCPRNT(525)
      ELSE IF(NUMP.EQ.0.AND.NUMOFF.EQ.0) THEN
          CALL DCPRNT(527)
      ELSE
          CALL DCPRNT(528)
      ENDIF
      CALL TCURS(C,XP,YP)
C     store menu
2030  CONTINUE
C     profile control flag
      GENP = .FALSE.
      TCELL = CELLN
      TMEN = MEN
      IF(MEN.EQ.3) THEN
C
          IF(CCMD.EQ.'O') THEN
2010          CONTINUE
              CALL DPRMXP(529,STRING)
              IF(NLEN(STRING).EQ.0) THEN
                  GOTO 2000
              ELSE
C                 calculate the offsets again
                  CALL CRTOF3(NUMOFF,NUMARR,NUMP,
     +                        MULTP,STRING,OK,QUIT)
              ENDIF
              CALL GTMCLO(TMEN,TCELL)
              IF(QUIT) GOTO 2000
              IF(.NOT.OK) GOTO 2010
          ELSEIF(CCMD.EQ.'N') THEN
C             set the function number
              GENP = .FALSE.
              FN = 2
C             get the boundary
              IF(NO.GT.0) THEN
C                sraw out old boundary
                 DO 210 I = 1,NO
 
                   READ (UNIT=HUNIT,REC=I) CRMIP,ENT,OX,OY,
     +             RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1                     XS,YS,XF,YF,XP
                   CALL ALLDRW(ENT,CRMIP)
 
 210             CONTINUE
              ENDIF
              NO = 0
              CALL PULENT(FN)
              IF(CCMD.EQ.'N') GOTO 2030
              CALL GTMCLO(TMEN,TCELL)
              IF(MEN.EQ.2.OR.CCMD.EQ.'q') GOTO 900
          ELSE IF(CCMD.EQ.CHAR(149)) THEN
c              cancel has been hit
               IF(STKCNL.GT.1 ) THEN
                  CALL PENERS()
C                 erase the curent profile
                  CUMIP = STACK(1,STKCNL-1)
C
C                 draw out the ofsset
                  DO 230 TMIP =CUMIP,NMIPOS-1
                      VPCAN = .TRUE.
                      CALL ALLDRW(ENT,TMIP)
                      VPCAN = .FALSE.
230               CONTINUE
C
                  CALL PENDRW()
C                 decremrnt stack pointer
                  STKCNL = STKCNL-1
                  NMIPOS = STACK(1,STKCNL)
                  NPDPOS = STACK(2,STKCNL)
                  LDFILE(CVPN) = STACK(3,STKCNL)
C                 make the last profile now eligble for cancel
              ELSE
                  CALL DEPRNT(534)
              ENDIF
              CALL GTMCLO(TMEN,TCELL)
 
          ELSEIF(CCMD.EQ.'S') THEN
C             print out the full vale of the offset chosen
              CALL CPRINT(DICT01(482)(:NLEN(DICT01(482)))//STRING)
              CALL GTMCLO(TMEN,TCELL)
          ELSEIF(CCMD.EQ.'L') THEN
C
              CALL HATCHM(OK)
C
          ELSE
              CALL DEPRNT(131)
              CALL GTMCLO(TMEN,TCELL)
 
          ENDIF
      ELSEIF(TMEN.EQ.2.OR.TMEN.EQ.3.OR.CCMD.EQ.'q') THEN
          GOTO 900
      ELSEIF(MEN.EQ.0) THEN
          GENP = .FALSE.
          IF(NO.EQ.0) THEN
              CALL GTMCLO(TMEN,TCELL)
              CALL DEPRNT(536)
              GOTO 2000
          ELSE IF(NUMP.EQ.0.AND.NUMOFF.EQ.0) THEN
              CALL GTMCLO(TMEN,TCELL)
              CALL DEPRNT(537)
              GOTO 2000
          ELSE
C             store current offsets
              T1 = NUMP
              T2 = NUMOFF
              GENP = .TRUE.
          ENDIF
       ENDIF
 
      IF(.NOT.GENP) GOTO 2000
C
C     get profile
      OFFSET = 0
      IF(MEN.NE.0) THEN
          CALL GTMCLO(TMEN,TCELL)
          GOTO 2030
      ENDIF
C
C     chances are were going to make a profile save pointers
      STACK(1,STKCNL) = NMIPOS
      STACK(2,STKCNL) = NPDPOS
      STACK(3,STKCNL) = LDFILE(CVPN)
C
C     sort the first and last entiies
      CALL SORTSC(OX,OY,CLOCK)
C     get me the direction left or right
      CALL CRTOF8(XP,YP,LEFT,RIGHT,OK)
      IF(.NOT.OK) THEN
          CALL DEPRNT(538)
          GOTO 2000
      ENDIF
C     ****************************
C     start main offset processing
C     ****************************
C     set multiple counters
      MULTCT = 0
      SINGCT = 0
1000  CONTINUE
      ENTP = 1
      DUMMY = 0
      TFONT = 1
C
C     do multiples first
      IF(MULTCT.LT.NUMP) THEN
          IF(.NOT.MULT) THEN
C             increment the current number
              MULTCT = MULTCT + 1
              MULT=.TRUE.
          ENDIF
C         If this array has anything in it then set multioffset true
          IF(MULTP(1,MULTCT).EQ.0) THEN
C             nothing in this one go onto the next
C             decremrnt multple counter
C             set flag
              MULT = .FALSE.
              IF(NUMP.EQ.MULTCT.AND.NUMOFF.EQ.SINGCT) GOTO 910
              GOTO 1000
          ENDIF
C         decrement multiple counter
          MULTP(1,MULTCT) = MULTP(1,MULTCT) -1
C         new offset
          OFFSET = OFFSET + MULTP(2,MULTCT)
C
      ELSEIF(SINGCT.LT.NUMOFF) THEN
          SINGCT = SINGCT + 1
          OFFSET = OFFSET + NUMARR(SINGCT)
      ENDIF
 
C
      READ(UNIT=HUNIT,REC=1) CRMIP,ENT,XX,YY,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 
C
      CALL SORTSC(OX,OY,CLOCK)
      IF (ENT.EQ.LINE) THEN
C
C         set origin coords
          STARTX = TBUFF(1)
          STARTY = TBUFF(2)
          OX = TBUFF(4)
          OY = TBUFF(5)
C         gen a vector along path
          CALL CV0L14(TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),C1,C2,C3)
C         get perpendicular distance
          DIST = VD0D13(C1,C2,C3,XP,YP)
C         set left or right profile
C          LEFT =  DIST.LT.0.0
C          RIGHT = DIST.GT.0.0
C      WRITE(10,*) '[OFFSET] LEFT RIGHT =',LEFT,RIGHT
 
          CALL VECOFF(TBUFF,LEFT,RIGHT,OFFSET,V1,V2,V3)
C      WRITE(10,*) '[OFFSET] OFFSET VECTOR ',V1,V2,V3
 
          IF(LEFT) CALL VECREV(C1,C2,C3)
          CALL VV00L6(C1,C2,C3,STARTX,STARTY,M1,M2,M3)
          CALL VC00P4(STARTX,STARTY,OFFSET,M1,M2,M3,SX,SY)
          CALL VC00P4(SX,SY,MAGLIN(V1,V2,V3),V1,V2,V3,XF,YF)
 
          WRITE(UNIT=PUNIT,REC=ENTP) ENT,CRMIP,
     +          SX,SY,XF,YF,DUMMY
C      WRITE(10,*) '[OFFSET] SX SY=',SX,SY
 
          SX = XF
          SY = YF
 
 
      ELSEIF(ENT.EQ.ARC) THEN
 
C         stored ox and oy from the last time
          RADIUS = TBUFF(4)
          DIST = DISTXY(TBUFF(1),TBUFF(2),XP,YP)
C         generate
          CALL ARCSE(TBUFF,XS,YS,XF,YF,FULCIR)
          IF(SAMEPT(XS,YS,OX,OY)) THEN
 
              CLOCK = .FALSE.
              OX = XF
              OY = YF
              STARTX = XS
              STARTY = YS
 
          ELSE IF(SAMEPT(XF,YF,OX,OY)) THEN
 
              CLOCK = .TRUE.
              OX = XS
              OY = YS
              STARTX = XF
              STARTY = YF
 
          ENDIF
 
c          LEFT = (DIST.GT.RADIUS.AND.CLOCK).OR.(DIST.LT.RADIUS.AND.
c     +           .NOT.CLOCK)
c          RIGHT = (DIST.LT.RADIUS.AND.CLOCK).OR.(DIST.GT.RADIUS.AND.
c     +           .NOT.CLOCK)
 
          IF(CLOCK.AND.LEFT.OR.RIGHT.AND..NOT.CLOCK) THEN
              RADIUS = TBUFF(4)+OFFSET
          ELSE
              RADIUS = TBUFF(4)-OFFSET
          ENDIF
          IF(.NOT.RADOK(RADIUS)) GOTO 910
 
C         write it out
          WRITE(UNIT=PUNIT,REC=1) ENT,CRMIP,
     +         TBUFF(1),TBUFF(2),RADIUS,TBUFF(5),TBUFF(6)
 
          IF(CLOCK) THEN
              SX = TBUFF(1)+RADIUS*COS(TBUFF(5))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(5))
          ELSE
              SX = TBUFF(1)+RADIUS*COS(TBUFF(6))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(6))
          ENDIF
 
 
      ENDIF
C     **************************************************
C         Start to read the data and generate offsets
C     **************************************************
 
 
200   CONTINUE
 
      LASTP = ENTP
      IF(SAMEPT(OX,OY,STARTX,STARTY)) THEN
          FINISH = .TRUE.
          ENTP = 1
      ELSE
          FINISH = .FALSE.
          ENTP = ENTP + 1
      ENDIF
 
      IF(ENTP.GT.NO) THEN
         GOTO 500
      ENDIF
 
C      WRITE(10,*) '[OFFSET] LOOPING ENTP NO= ',ENTP,NO
 
C     GET entities
      READ(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,X,Y,
     +        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 
C      WRITE(10,*) '[OFFSET] TBUFF =',(TBUFF(I),I=1,6)
C      WRITE(10,*) '[OFFSET] CRMIP X Y = ',CRMIP,X,Y
      READ(UNIT=HUNIT,REC=LASTP) MIP2,ENT2,X,Y,
     +        XBUFF(1),XBUFF(2),XBUFF(4),XBUFF(5),XBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
C      WRITE(10,*) '[OFFSET] XBUFF =',(TBUFF(I),I=1,6)
 
C      WRITE(10,*) '[OFFSET ENTITY READ  OX OY = ',OX,OY
      IF(ENT1.EQ.LINE.AND.ENT2.EQ.LINE) THEN
 
C         generate a new vector
          CALL VECOFF(TBUFF,LEFT,RIGHT,OFFSET,C1,C2,C3)
C          WRITE(10,*) '[OFFSET LINE LINE C1 C2 C3= ',C1,C2,C3
          CALL VECOFF(XBUFF,LEFT,RIGHT,OFFSET,V1,V2,V3)
          CALL VC00P5 (V1,V2,V3,C1,C2,C3,XP1,YP1,OK)
 
          IF(LEFT) CALL VECREV(C1,C2,C3)
          OX = TBUFF(4)
          OY=  TBUFF(5)
          CALL VV00L6(C1,C2,C3,OX,OY,M1,M2,M3)
          CALL VC00P4(OX,OY,OFFSET,M1,M2,M3,SX,SY)
 
C          write(10,*) '[OFFSET] X Y =',XP1,YP1
 
C         finish point is found
 
          IF(OK) THEN
 
              IF(FINISH) THEN
                  READ(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +              X1,Y1,X2,Y2,DUMMY
                    X1 = XP1
                    Y1 = YP1
                  WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +              X1,Y1,X2,Y2,DUMMY
                    X1 = XP1
                    Y1 = YP1
 
              ELSE
                  WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +              XP1,YP1,SX,SY,DUMMY
              ENDIF
              READ(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +          X1,Y1,X2,Y2,DUMMY
              X2 = XP1
              Y2 = YP1
              IF(.NOT.LINEOK(XBUFF,X1,Y1,X2,Y2)) GOTO 910
              WRITE(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +          X1,Y1,X2,Y2,DUMMY
 
 
          ELSE
              CALL CRTOF5(MIP1,MIP2)
              GOTO 910
 
          ENDIF
      ELSEIF(ENT1.EQ.LINE.AND.ENT2.EQ.ARC) THEN
 
C      WRITE(10,*) '[OFFSET] TBUFF =',(TBUFF(I),I=1,6)
C      WRITE(10,*) '[OFFSET] LEFT RIGHT = ',LEFT,RIGHT
C      WRITE(10,*) '[OFFSET] OFFSET = ',OFFSET
 
          CALL VECOFF(TBUFF,LEFT,RIGHT,OFFSET,C1,C2,C3)
          IF(LEFT) CALL VECREV(C1,C2,C3)
C      WRITE(10,*) '[OFFSET] C1 C2 C3 = ',C1,C2,C3
          OX = TBUFF(1)
          OY=  TBUFF(2)
          CALL VV00L6(C1,C2,C3,OX,OY,M1,M2,M3)
          CALL VC00P4(OX,OY,OFFSET,M1,M2,M3,SX,SY)
C         modify last arc
          XS1 = SX
          YS1 = SY
          OX = TBUFF(4)
          OY=  TBUFF(5)
          CALL VC00P4(OX,OY,OFFSET,M1,M2,M3,SX,SY)
 
C          WRITE(10,*) '[OFFSET] XS1 YS1 = ',XS1,YS1
C          WRITE(10,*) '[OFFSET] XS2 YS2 = ',XS2,YS2
          READ(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +           CENX,CENY,RAD,ANGS,ANGF
 
          IF(CLOCK) THEN
             XS2 = CENX+RAD*COS(ANGS)
             YS2 = CENY+RAD*SIN(ANGS)
          ELSE
             XS2 = CENX+RAD*COS(ANGF)
             YS2 = CENY+RAD*SIN(ANGF)
          ENDIF
C          WRITE(10,*) '[OFFSET] XS2 YS2 = ',XS2,YS2
          CALL VC00P9(CENX,CENY,RAD,C1,C2,C3,XP1,YP1,XP2,YP2,OK)
C          write(10,*) '[OFFSET] INTERSECTION X Y =',XP1,YP1,XP2,YP2
          IF(.NOT.OK) THEN
              CALL CRTOF5(MIP1,MIP2)
              GOTO 910
          ENDIF
 
C         which intersection point do we use
          DIST1 = DISTXY(XS1,YS1,XP1,YP1)+DISTXY(XS2,YS2,XP1,YP1)
          DIST2 = DISTXY(XS1,YS1,XP2,YP2)+DISTXY(XS2,YS2,XP2,YP2)
C          write(10,*) '[OFFSET] TRIM DIST = ',DIST1,DIST2
          IF(DIST1.GT.DIST2) THEN
              XP1 = XP2
              YP1 = YP2
          ENDIF
          IF(CLOCK) THEN
              ANGS = CANG(CENX,CENY,XP1,YP1)
          ELSE
              ANGF = CANG(CENX,CENY,XP1,YP1)
          ENDIF
 
C         modfify last arc
          WRITE(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +           CENX,CENY,RAD,ANGS,ANGF
C         write out or modify exixting line
          IF(FINISH) THEN
              READ(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +               X1,Y1,X2,Y2,DUMMY
              X1 = XP1
              Y1 = YP1
 
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +               X1,Y1,X2,Y2,DUMMY
          ELSE
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +               XP1,YP1,SX,SY,DUMMY
 
          ENDIF
      ELSEIF(ENT1.EQ.ARC.AND.ENT2.EQ.LINE) THEN
 
 
          CALL ARCSE(TBUFF,XS,YS,XF,YF,FULCIR)
C      WRITE(10,*) '[OFFSET ARC ARC XS YS XF YF= ',XS,YS,XF,YF
          IF(SAMEPT(OX,OY,XS,YS)) THEN
              CLOCK=.FALSE.
              OX = XF
              OY = YF
          ELSE
              CLOCK=.TRUE.
              OX = XS
              OY = YS
          ENDIF
C      WRITE(10,*) '[OFFSET CLOCK = ',CLOCK
          CENX = TBUFF(1)
          CENY = TBUFF(2)
          RAD  = TBUFF(4)
          ANGS = TBUFF(5)
          ANGF = TBUFF(6)
          IF(CLOCK.AND.LEFT.OR.RIGHT.AND..NOT.CLOCK) THEN
              RADIUS = TBUFF(4)+OFFSET
          ELSE
              RADIUS = TBUFF(4)-OFFSET
          ENDIF
C
C         radius check for new arc
          IF(.NOT.RADOK(RADIUS) ) GOTO 910
          IF(CLOCK) THEN
             XS1 = CENX+RADIUS*COS(ANGF)
             YS1 = CENY+RADIUS*SIN(ANGF)
          ELSE
             XS1 = CENX+RADIUS*COS(ANGS)
             YS1 = CENY+RADIUS*SIN(ANGS)
          ENDIF
          READ(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +          X1,Y1,X2,Y2,DUMMY
          XS2 = X2
          YS2 = Y2
          CALL VECOFF(XBUFF,LEFT,RIGHT,OFFSET,C1,C2,C3)
 
C          WRITE(10,*) '[OFFSET C1 C2 C3= ',C1,C2,C3
 
          CALL VC00P9(CENX,CENY,RADIUS,C1,C2,C3,XP1,YP1,XP2,YP2,OK)
 
          IF(.NOT.OK) THEN
              CALL CRTOF5(MIP1,MIP2)
              GOTO 910
          ENDIF
C          write(10,*) '[OFFSET] X Y =',XP1,YP1,XP2,YP2
          DIST1 = DISTXY(XS1,YS1,XP1,YP1)+DISTXY(XS2,YS2,XP1,YP1)
          DIST2 = DISTXY(XS1,YS1,XP2,YP2)+DISTXY(XS2,YS2,XP2,YP2)
          IF(DIST1.GT.DIST2) THEN
              XP1 = XP2
              YP1 = YP2
          ENDIF
 
 
C          WRITE(10,*) '[OFSSET] RADIUS XC YC=',RADIUS,CENX,CENY
C          WRITE(10,*) '[OFSSET] OFFSET = ',OFFSET
C          WRITE(10,*) '[OFSSET] XBUFF= ',(XBUFF(I),I=1,6)
C          WRITE(10,*) '[OFSSET] LEFT RIGHT= ',LEFT,RIGHT
 
          IF(CLOCK) THEN
              ANGF = CANG(CENX,CENY,XP1,YP1)
          ELSE
              ANGS = CANG(CENX,CENY,XP1,YP1)
          ENDIF
C         write it out
          IF(FINISH) THEN
              READ(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           CENX,CENY,RAD,ANG1,ANG2
              IF(CLOCK) THEN
                  ANG2 = ANGF
              ELSE
                  ANG1 = ANGS
              ENDIF
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           CENX,CENY,RAD,ANG1,ANG2
          ELSE
 
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           TBUFF(1),TBUFF(2),RADIUS,ANGS,ANGF
          ENDIF
 
 
          READ (UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +        X1,Y1,X2,Y2,DUMMY
          X2 = XP1
          Y2 = YP1
 
C         check for line reversal
          IF(.NOT.LINEOK(XBUFF,X1,Y1,X2,Y2)) GOTO 910
          WRITE(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +        X1,Y1,X2,Y2,DUMMY
          IF( CLOCK) THEN
              SX = TBUFF(1)+RADIUS*COS(TBUFF(5))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(5))
          ELSE
              SX = TBUFF(1)+RADIUS*COS(TBUFF(6))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(6))
          ENDIF
 
 
      ELSEIF(ENT1.EQ.ARC.AND.ENT2.EQ.ARC) THEN
 
          PCLOCK = CLOCK
          CALL ARCSE(TBUFF,XS,YS,XF,YF,FULCIR)
          IF(SAMEPT(OX,OY,XS,YS)) THEN
              CLOCK=.FALSE.
              OX = XF
              OY = YF
          ELSE
              CLOCK=.TRUE.
              OX = XS
              OY = YS
          ENDIF
          ANGS = TBUFF(5)
          ANGF = TBUFF(6)
          CENX = TBUFF(1)
          CENY = TBUFF(2)
          IF(CLOCK.AND.LEFT.OR.RIGHT.AND..NOT.CLOCK) THEN
              RADIUS = TBUFF(4)+OFFSET
          ELSE
              RADIUS = TBUFF(4)-OFFSET
          ENDIF
          IF(.NOT.RADOK(RADIUS)) GOTO 910
C         new arc
          IF(CLOCK) THEN
             XS1 = CENX+RADIUS*COS(ANGF)
             YS1 = CENY+RADIUS*SIN(ANGF)
          ELSE
             XS1 = CENX+RADIUS*COS(ANGS)
             YS1 = CENY+RADIUS*SIN(ANGS)
          ENDIF
          IF(PCLOCK.AND.LEFT.OR.RIGHT.AND..NOT.PCLOCK) THEN
              RADL = XBUFF(4)+OFFSET
          ELSE
              RADL= XBUFF(4)-OFFSET
          ENDIF
C         old arc
          ANG2S = XBUFF(5)
          ANG2F = XBUFF(6)
          CEN2X = XBUFF(1)
          CEN2Y = XBUFF(2)
 
          IF(PCLOCK) THEN
             XS2 = CEN2X+RADL*COS(ANG2S)
             YS2 = CEN2Y+RADL*SIN(ANG2S)
          ELSE
             XS2 = CEN2X+RADL*COS(ANG2F)
             YS2 = CEN2Y+RADL*SIN(ANG2F)
          ENDIF
          AUX1 = RADIUS
          AUX2 = 0.0
          AUX3 = RADL
          AUX4 = 0.0
C          write(10,*) '[OFFSET] X Y =',XP1,YP1,XP2,YP2
          CALL INTERS(ENT1,TBUFF(1),TBUFF(2),RADIUS,DUMMY,AUX1,AUX2,
     +                ENT2,XBUFF(1),XBUFF(2),RADL,DUMMY,AUX3,AUX4,
     +                XP1,YP1,XP2,YP2,SX1,SY1,SX2,SY2,IST,OK)
          IF(.NOT.OK) THEN
              CALL CRTOF5(MIP1,MIP2)
              GOTO 910
          ENDIF
          DIST1 = DISTXY(XS1,YS1,XP1,YP1)+DISTXY(XS2,YS2,XP1,YP1)
          DIST2 = DISTXY(XS1,YS1,XP2,YP2)+DISTXY(XS2,YS2,XP2,YP2)
          IF(DIST1.GT.DIST2) THEN
              XP1 = XP2
              YP1 = YP2
          ENDIF
          TBUFF(4) = RADIUS
          XBUFF(4) = RADL
 
          IF(CLOCK) THEN
              ANGF = CANG(CENX,CENY,XP1,YP1)
          ELSE
              ANGS = CANG(CENX,CENY,XP1,YP1)
          ENDIF
C         write it out
          IF(FINISH) THEN
              READ(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           CENX,CENY,RAD,ANG1,ANG2
              IF(CLOCK) THEN
                  ANG2 = ANGF
              ELSE
                  ANG1 = ANGS
              ENDIF
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           CENX,CENY,RAD,ANG1,ANG2
          ELSE
 
              WRITE(UNIT=PUNIT,REC=ENTP) ENT1,MIP1,
     +           TBUFF(1),TBUFF(2),RADIUS,ANGS,ANGF
          ENDIF
 
          READ(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +        CENX,CENY,RAD,ANG1,ANG2
 
          IF(PCLOCK) THEN
              ANG1 = CANG(CENX,CENY,XP1,YP1)
          ELSE
              ANG2 = CANG(CENX,CENY,XP1,YP1)
          ENDIF
 
          WRITE(UNIT=PUNIT,REC=LASTP) ENT2,MIP2,
     +        CENX,CENY,RAD,ANG1,ANG2
          IF( CLOCK) THEN
              SX = TBUFF(1)+RADIUS*COS(TBUFF(5))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(5))
          ELSE
              SX = TBUFF(1)+RADIUS*COS(TBUFF(6))
              SY = TBUFF(2)+RADIUS*SIN(TBUFF(6))
          ENDIF
 
      ENDIF
 
      IF(FINISH) GOTO 500
      GOTO 200
C     open profile lets finish up now
 
500   CONTINUE
C
      ENTP =LASTP
      DO 510 I =1,ENTP
          READ(UNIT=PUNIT,REC=I) ENT,CRMIP,
     +              SX,SY,X,Y,DUMMY
C          WRITE(10,*)'[OFFSET] ENT,SX,SY,X,Y= ',ENT,SX,SY,X,Y
          CALL DIR500(CRMIP,OK)
C         set font could be changed layer
          FONTI2 = CLFONT
          TLAY= TARGLY
          IF(ENT.EQ.LINE) THEN
C             do the write
              VPADD = .TRUE.
              CALL DEWC03(SX,SY,X,Y,FONTI2,TLAY,P,OK)
C             If target layer visable draw
              IF(VLAYER(TARGLY)) THEN
                  CALL ALLDRW(ENT,P)
              ELSE
                  LDFILE(CVPN)=LDFILE(CVPN)-1
              ENDIF
              VPADD = .FALSE.
C
          ELSE
C             do the write
              VPADD = .TRUE.
              CALL DEWC05(SX,SY,X,Y,DUMMY,FONTI2,TLAY,P,OK)
C             If target layer visable draw
              IF(VLAYER(TARGLY)) THEN
                  CALL ALLDRW(ENT,P)
              ELSE
                  LDFILE(CVPN)=LDFILE(CVPN)-1
              ENDIF
              VPADD = .FALSE.
          ENDIF
510   CONTINUE
      IF(MULTCT.LT.NUMP) GOTO 1000
      IF(SINGCT.LT.NUMOFF) GOTO 1000
C
910   CONTINUE
      NUMP = T1
      NUMOFF = T2
      GENP = .FALSE.
      CALL GTMCLO(TMEN,TCELL)
C     increment the stack if ent have been added
      IF( NMIPOS.GT.STACK(1,STKCNL)) STKCNL=STKCNL+1
      GOTO 2000
C
C
900   CONTINUE
C     clear anything in the buffer
      CALL ERSPRF()
      CALL UNFLAG(.TRUE.)
      CLOSE(UNIT=PUNIT)
      CLOSE(UNIT=HUNIT)
      END
 
 
 
      SUBROUTINE CRTOF3(NUMOFF,NUMARR,NUMP,MULTP,INPL,OK,QUIT)
C     ==========================================================
C1    VARTYPE           I2     R*40  I2   R*40    C    L    L
C1    IOSTATUS          O      O      O     O     I    O    O
C
      LOGICAL OK,QUIT
      INTEGER*2 NUMOFF,NUMP,T1,T2
      INTEGER*4 TMEN,TCELL
      REAL MULTP(2,40),NUMARR(40)
      CHARACTER*80 INPL,STRING,TOKEN*2
      INTEGER*4 FIELD(30),NF,NLEN,SP,EP,I,J
      DOUBLE PRECISION DN
 
      TMEN = 3
      QUIT = .FALSE.
      OK = .TRUE.
 
      T1 = NUMP
      T2 = NUMOFF
      NUMP = 0
      NUMOFF = 0
      CALL PARSES(INPL,FIELD,NF)
      SP = 1
      DO 10 I=1,NF
C         decode the field
          EP = FIELD(I)
          STRING = INPL(SP:EP)
          CALL SUBSTC(STRING,',',' ')
          J = INDEX(STRING,'@')
          IF(J.GT.1) THEN
              NUMP = NUMP + 1
              CALL AEXPRN(STRING(1:J-1),DN,*99)
              MULTP(1,NUMP) = DN
              CALL AEXPRN(STRING(J+1:),DN,*99)
              MULTP(2,NUMP) = DN
          ELSE
              CALL AEXPRN(STRING,DN,*99)
              NUMOFF = NUMOFF +1
              NUMARR(NUMOFF) = DN
          ENDIF
          SP =EP
 10   CONTINUE
      TOKEN='O'
C     load up new offset
      CALL GTHFMC(TMEN,TOKEN,TCELL)
      TCELL=TCELL+1
      CALL GTCLRC(3,TCELL)
      TOKEN='S'
      CALL GTPMEN(INPL,TOKEN,TMEN,TCELL)
      RETURN
 
 99   OK = .FALSE.
      NUMP = T1
      NUMOFF = T2
 
      END
 
 
      SUBROUTINE CRTOF5(MIP1,MIP2)
C     ===========================
C1    VARTYPE            I2   I2
C2    IOSTAUS            I    I
C
C2    Marks the two enties which do not work
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/vntable.inc'
C
      LOGICAL OK
      CHARACTER*5 ENT1,ENT2,OUTSTR*80
      INTEGER*2 MIP1,MIP2,ENT
C
      CALL DIR500(MIP1,OK)
      IF(IMBUFF(2).EQ.LINE) THEN
          ENT1 = 'LINE '
      ELSE
          ENT1= 'ARC '
      ENDIF
      CALL DIR500(MIP2,OK)
      IF(IMBUFF(2).EQ.LINE) THEN
          ENT2 = 'LINE'
      ELSE
          ENT2= 'ARC'
      ENDIF
      OUTSTR = ENT1//ENT2//DICT01(542)
      CALL EPRINT(OUTSTR)
C
      CALL ALLDRW(ENT,MIP1)
      CALL ALLDRW(ENT,MIP2)
      END
 
      SUBROUTINE CRTOF6(MIP1)
C     ======================
C1    VARTYPE            I2
C2    IOSTAUS            I
C
C2    Marks theenties which do not work
C
      INTEGER*2 MIP1,ENT
      CALL ALLDRW(ENT,MIP1)
      END
 
      SUBROUTINE CRTOF8(XP,YP,LEFT,RIGHT,OK)
C     ======================================
C1    VARTYPE           R   R   L    L
C1    IOSTA             I   I   O    O
C
C2    This routine will return the profile offset
C2    control flags. Either outside or in side the profile
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/hdata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
      include 'include/vntable.inc'
      include 'include/curwin.inc'
C
      REAL XX,YY,TBUFF(6),XP,YP,DIST,MINDST,XBUFF(6)
      REAL C1,C2,C3,X1S,Y1S,X1F,Y1F,RADIUS,LIM,SLIM
      REAL X2S,Y2S,X2F,Y2F,DISTXY,VD0D13,OWIDTH,X,Y
      LOGICAL LEFT,RIGHT,FULCIR,ENDE,SAME,CHKLN
      LOGICAL OK,GOOD
      INTEGER*4 I,NEXTE,ENTP
      INTEGER*2 ENT,MMIP
C
      EXTERNAL SAME,DISTXY,VD0D13,CHKLN
C
C     loop round the profile and find the closest distance to them
C
C     getsome kind of limit
      MINDST = 1E38
      DIST = 0
      GOOD = .FALSE.
      ENTP = 0
C
      DO 20 I=1,NO
C
          READ(UNIT=HUNIT,REC=I) CRMIP,ENT,XX,YY,
     +        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1        TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5),TBUFF(6)
C
          IF(ENT.EQ.LINE) THEN
C             genearte vector from line
              OWIDTH = XWIDTH
              XWIDTH=XWIDTH*4
              CALL  DSE881(ENT,XP,YP,OK,.FALSE.,MMIP)
              XWIDTH = OWIDTH
              IF(OK) THEN
C                 generate vector
                  CALL CV0L14(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +                        RDBUFF(5),C1,C2,C3)
C                 get distance from line
                  DIST = VD0D13(C1,C2,C3,XP,YP)
                  ENTP = I
C                 set left or right profile might as well do it here
                  LEFT =  DIST.LT.0.0
                  RIGHT = DIST.GT.0.0
                  OK = .TRUE.
                  RETURN
              ENDIF
 
              IF(ABS(DIST).LT.MINDST) THEN
C                 save the nesesary
                  MINDST = ABS(DIST)
                  ENTP = I
C                 set left or right profile might as well do it here
                  LEFT =  DIST.LT.0.0
                  RIGHT = DIST.GT.0.0
              ENDIF
          ELSEIF(ENT.EQ.ARC) THEN
C             Bit more tricky this one
C             set radius
C             genearte vector from line
              OWIDTH = XWIDTH
              CALL STOWVX(XWIDTH,LIM)
              XWIDTH = XWIDTH*4
C             look for a valid one
              CALL  DSE881(ENT,XP,YP,OK,.FALSE.,MMIP)
              XWIDTH = OWIDTH
              IF(OK) THEN
                  RADIUS = RDBUFF(4)
C                 get distance from arc and save it
                  DIST = DISTXY(RDBUFF(1),RDBUFF(2),XP,YP)
C
                  IF(I.EQ.NO) THEN
                     NEXTE = 1
                     ENDE = .FALSE.
                  ELSE
                     NEXTE = I-1
                     ENDE = .TRUE.
                  ENDIF
C                 generate the nessaery details about arc
                  CALL ARCSE(RDBUFF,X1S,Y1S,X1F,Y1F,FULCIR)
C
                  READ(UNIT=HUNIT,REC=NEXTE) CRMIP,ENT,XX,YY,
     +            XBUFF(1),XBUFF(2),XBUFF(4),XBUFF(5),XBUFF(6),
     1            RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
                  IF(ENT.EQ.LINE) THEN
C                     dead easy we know the points allready
                      CLOCK=(SAME(X1S,XBUFF(1)).AND.SAME(Y1S,XBUFF(2)))
     +                .OR.(SAME(X1S,XBUFF(4)).AND.SAME(Y1S,XBUFF(5)))
                  ELSEIF (ENT.EQ.ARC) THEN
C                     get second arcs points
                      CALL ARCSE(XBUFF,X2S,Y2S,X2F,Y2F,FULCIR)
                      CLOCK=(SAME(X1S,X2S).AND.SAME(Y1S,Y2S)).OR.
     +               (SAME(X1S,X2F).AND.SAME(Y1S,Y2F))
                  ENDIF
C                 set the direction according to number
                  IF(ENDE) CLOCK = .NOT.CLOCK
C                 set left and right controls
                  LEFT=(DIST.GT.RADIUS.AND.CLOCK).OR.(DIST.LT.
     +                  RADIUS.AND..NOT.CLOCK)
                  RIGHT=(DIST.LT.RADIUS.AND.CLOCK).OR.(DIST.GT
     +                  .RADIUS.AND..NOT.CLOCK)
                  ENTP = I
C                 go home
                  RETURN
              ENDIF
C
         ENDIF
 20   CONTINUE
C
C     set retrun flag
      OK = .FALSE.
      END
 
 
      LOGICAL FUNCTION LINEOK(BUFF,X1,Y1,X2,Y2)
C     =========================================
C1    VARTYPE                  R6  R  R  R  R
C1    IOSTATUS                 I   I  I  I  I
C
C2    This function will return a true status if the line gened
C2    in the same direction as the original
C
      INTEGER*4 TFONT
      REAL BUFF(6),X1,Y1,X2,Y2,L1,L2,L3,C1,C2,C3,ANG
      REAL VN00D6,PI,ZERO,DISTXY
      LOGICAL SAMEV
C
      EXTERNAL VN00D6,PI,DISTXY,SAMEV
C
      ZERO = 0.000000
      TFONT =1
      LINEOK = .NOT.SAMEV( DISTXY(X1,Y1,X2,Y2),ZERO)
 
      IF(LINEOK) THEN
C
          CALL CV0L14(BUFF(1),BUFF(2),BUFF(4),BUFF(5),L1,L2,L3)
          CALL CV0L14(X1,Y1,X2,Y2,C1,C2,C3)
C
          CALL VANG (C1,C2,C3,L1,L2,L3,ANG)
 
          LINEOK = ANG.LT.PI(0.5)
      ENDIF
      IF(.NOT.LINEOK) THEN
          CALL DEPRNT(543)
          CALL DRWFLW(BUFF(1),BUFF(2),BUFF(4),BUFF(5),TFONT)
      ENDIF
 
      END
 
      SUBROUTINE MNCRTF()
C     ===================
C1    NO ARGS
C
C2    This routine loads up the CREATE OFFSET menu
C
      include 'include/masti.inc'
      include 'include/menun.inc'
C
      INTEGER*4 I
      CALL GTCLRM(3)
C     autoprofile
      CALL GTDMEN(406,3)
 
      CALL GTDMEN(408,3)
      CALL GTDMEN(469,3)
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(121,3)
C2    CHAR(150) is the token for ACCEPT.
      CALL GTDMEN(126,3)
C
C     target layer
      CALL GTDMEN(325,3)
      CCMD='L'
      I = CLAYER
C     Find the menu and hightlite it
      CALL GTMCWI(3,CCMD,I)
      END
C
      LOGICAL FUNCTION RADOK(RAD)
C     ===========================
C1    VARTYPE                  R
C1    IOSTATUS                 I
C
C2    This function will return a true status if the arc gened
C2    radius is ok
C
      REAL RAD,ZERO
      LOGICAL SAMEV
      ZERO = 0.0
 
      RADOK = RAD.GT.ZERO
      IF(RADOK) RETURN
      IF(.NOT.RADOK) THEN
          CALL DEPRNT(544)
      ENDIF
      END
 
 
      SUBROUTINE SORTSC(OX,OY,CLOCK)
C     ==============================
C1    VARTYPE            R  R   L
C1    IOSTATUS           O  O   O
C
C2    Purpose to sort an NC profile from pulent. I blame
C2    Neil for this.
C
      include 'include/hdata.inc'
      include 'include/entity.inc'
 
      INTEGER*2 LAST
      REAL OX,OY,XF,YF
      LOGICAL CLOCK,SAMEPT
      REAL BX,BY,XS1,YS1,XF1,YF1,ZH1,X1,Y1,X2,Y2,Z1
      REAL XS2,YS2,XF2,YF2,ZH2,XAF,YAF,XAS,YAS
      REAL XH1,YH1,XH2,YH2,ZZ1
 
C
      EXTERNAL SAMEPT
C
      READ(UNIT=HUNIT,REC=1) MIP1,ENT1,BX,BY,XS1,YS1,XF1,YF1,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
C
C     set special case i.e. full single circle
      IF(NO.EQ.1.AND.ENT1.EQ.ARC) THEN
C     full circle so set to start end
          CLOCK = .FALSE.
          OX = XH1 + ABS(XH2) * COS(YH2)
          OY = YH1
          RETURN
      ENDIF
C
C
      READ(UNIT=HUNIT,REC=2,ERR=99) MIP2,ENT2,BX,BY,
     +                       XS2,YS2,XF2,YF2,ZH2,
     +                       X1,Y1,X2,Y2,Z1
C

      IF(ENT2.EQ.ARC) THEN
C
C         start
          XAS=XS2+XF2*COS(YF2)
          YAS=YS2+XF2*SIN(YF2)
C         end
          XAF=XS2+XF2*COS(ZH2)
          YAF=YS2+XF2*SIN(ZH2)
C         set points
          XS2 = XAS
          YS2 = YAS
          XF2 = XAF
          YF2 = YAF
 
C
      ENDIF
      IF(ENT1.EQ.ARC) THEN
C
C         start
          XAS=XS1+XF1*COS(YF1)
          YAS=YS1+XF1*SIN(YF1)
C         end
          XAF=XS1+XF1*COS(ZH1)
          YAF=YS1+XF1*SIN(ZH1)
C         set points
          XS1 = XAS
          YS1 = YAS
          XF1 = XAF
          YF1 = YAF
 
      ENDIF
C
      IF(ENT1.EQ.LINE) THEN
          IF(SAMEPT(XS1,YS1,XF1,YF1)) THEN
              XS1 = XH2
              YS1 = YH2
          ENDIF
      ENDIF
      IF(SAMEPT(XS1,YS1,XS2,YS2).OR.SAMEPT(XS1,YS1,XF2,YF2)) THEN
          CLOCK = .TRUE.
          OX = XF1
          OY = YF1
          XF = XS1
          YF = YS1
      ELSE
          CLOCK = .FALSE.
          OX = XS1
          OY = YS1
          XF = XF1
          YF = YF1
      ENDIF
      IF(ENT1.EQ.LINE) THEN
          WRITE(UNIT=HUNIT,REC=1) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
     +                       X1,Y1,X2,Y2,Z1
C          WRITE(10,*) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
C
      ENDIF
      LAST =NO
 
C     sort last entity if its a line
      READ(UNIT=HUNIT,REC=LAST) MIP1,ENT1,BX,BY,XS1,YS1,XF1,YF1,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
      READ(UNIT=HUNIT,REC=LAST-1) MIP2,ENT2,BX,BY,XS2,YS2,XF2,YF2,ZH2,
     +                       X1,Y1,X2,Y2,Z1
C
      IF(ENT2.EQ.ARC) THEN
C
C         start
          XAS=XS2+XF2*COS(YF2)
          YAS=YS2+XF2*SIN(YF2)
C         end
          XAF=XS2+XF2*COS(ZH2)
          YAF=YS2+XF2*SIN(ZH2)
C         set points
          XS2 = XAS
          YS2 = YAS
          XF2 = XAF
          YF2 = YAF
 
C
 
      ENDIF
C     sort it if its a line
      IF(ENT1.EQ.LINE) THEN
          IF(SAMEPT(XS1,YS1,XF1,YF1)) THEN
              X1 = XS1
              Y1 = YS1
              X2 = XH1
              Y2 = YH1
              WRITE(UNIT=HUNIT,REC=LAST) MIP1,ENT1,BX,BY,
     +                       X1,Y1,X2,Y2,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
          ENDIF
      ENDIF
C          WRITE(10,*) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
C
99    CONTINUE
      END
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
 
 
      SUBROUTINE VECOFF(BUFF,LEFT,RIGHT,OFFSET,L1,L2,L3)
C     ==================================================
C1    VARTYPE            R6   L    L      R    R  R  R
C1    IOSTATUS           I    I    I      I    O  O  O
C
C2    This routine will generate a vector parrallel to
C2    the points in the buffer. The line will be
C2    generated left or right.
 
      LOGICAL RIGHT,LEFT
      REAL V1,V2,V3,OFFSET,BUFF(6)
      REAL M1,M2,M3,P1,P2,P3,L1,L2,L3
 
C     generat vector
      CALL CV0L14(BUFF(1),BUFF(2),BUFF(4),BUFF(5),V1,V2,V3)
C     get two vectors either side of line
      CALL VV0L15(V1,V2,V3,OFFSET,P1,P2,P3,M1,M2,M3)
 
      IF(RIGHT) THEN
          L1 = P1
          L2 = P2
          L3 = P3
      ELSEIF(LEFT) THEN
          L1 = M1
          L2 = M2
          L3 = M3
      ENDIF
 
C     go home
      END
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C
