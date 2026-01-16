C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 plots.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE PLTA05(X1,X2,X4,X5,X6)
C     SUBROUTINE PLTL03(X1,Y1,X2,Y2)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE PLTA05(X1,X2,X4,X5,X6)
C1    =================================
C1   vartyp              r, r, r, r, r
C1   iostat              i  i  i  i  i
C2     also check done for radius vs chord angle.
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/gerber.inc'
      include 'include/ndata.inc'
C
C     Set the coordinate info local
C     Arc Centre point
C
      CHARACTER*80 OLINE
      INTEGER THK,END,I
      REAL X1,X2,X4,X5,X6,XT1,XT2,XT4,XT5,XT6,
     +    ANG,TOT,INC,STEP,PRAD,EXT,PI,DEG,PT
      LOGICAL COMCIR
      EXTERNAL PI,DEG
C
C---  Is new start point same as last end point
      XT1=X1
      XT2=X2
      XT4=X4
      XT5=X5
      XT6=X6
      ANG=0.0
      EXT=0.0
C
C      WRITE(10,*) '[PLTA05]: MIP,X1,X2,X4,X5,X6',MIP,X1,X2,X4,X5,X6
 
      I=IMBUFF(12)
      TOT=PAPTOW*LTHKR(1,MOD(I,64))/2.0
      CALL CHGTHK(I)
      THK=PLTHKI(I)
      COMCIR=.FALSE.
      IF(IMBUFF(12).GT.0) COMCIR=(TOLTYP(IMBUFF(12)).EQ.'B'.AND.
     +          MOD(X6,PI(2.0)).EQ.MOD(X5,PI(2.0)))
C
C      WRITE(10,*) '[PLTA05] THK,TOLTYP(IMBUFF(12)),X6,X5 ',
C     +                      THK,TOLTYP(IMBUFF(12)),X6,X5
      IF ( THK.GT.0 ) THEN
        TOT=PAPTOW*LTHKR(1,MOD(THK,64))/2.0
        END=MOD(THK,8192)/1024
        IF ( END.GT.1 ) THEN
          IF (.NOT.(ABS(X6-X5-PI(2.0)).LT.1E-5)) THEN
            EXT=TOT
            IF((EXT/(2*X4)).LT.1.0.OR.
     +         (EXT/(2*X4)).LE.-1.0) ANG=2*ASIN(EXT/(2*X4))
          ELSE
            END=1
          END IF
          XT5=X5-ANG
          XT6=X6+ANG
        END IF
      END IF
C
      IF ( THK.GT.0.AND.TOT.GT.0 ) THEN
        CALL PLTARC(XT1,XT2,XT4,XT5,X6)
        TOT=TOT-((LTHKR(2,MOD(THK,64))/2.0)*PAPTOW)
C        WRITE(10,*) '[PLTA05] TOT = ',TOT
        IF ( TOT.GT.0 ) THEN
          INC=(LTHKR(2,MOD(THK,64))/2.5)*FACT
          CALL PTOWVX(INC,STEP)
          I=MAX(1,NINT(TOT/STEP))
          STEP=TOT/REAL(I)
          INC=STEP
C        WRITE(10,*) '[PLTA05] STEP,INC = ',STEP,INC
 5        CONTINUE
          IF ( .NOT.(INC.LE.TOT) ) RETURN
          IF ( INC.GT.X4 ) RETURN
          IF ( END.EQ.2 )  THEN
             EXT=SQRT(TOT*TOT-INC*INC)
             IF((EXT/(2*X4)).LT.1.0.OR.
     +          (EXT/(2*X4)).GT.-1.0) ANG=2*ASIN(EXT/(2*X4))
          END IF
          XT5=X5-ANG
          XT6=X6+ANG
          DO 12 I=1,2
             IF ( I.EQ.1 ) XT4=X4+INC
             IF ( I.EQ.2 ) XT4=X4-INC
             CALL PLTARC(XT1,XT2,XT4,XT5,X6)
 12       CONTINUE
          INC=INC+STEP
          GOTO 5
        END IF
      ELSE IF ( TOT.LT.0.OR.COMCIR ) THEN
        IF ( END.EQ.1 ) THEN
C         square end.
          XT4=X1-2.0*X4
          X1 =X1+2.0*X4
          X5 =X2
          CALL PLTL03(X1,X2,XT4,X5)
        ELSE IF ( END.EQ.2.AND.MOD(TOLSET(THK,1)+0,255).EQ.0 ) THEN
C         finger pad.
          TOT=PAPTOW*ABS(TOLSET(THK,2))
          IF ( TOLSET(THK,2).LT.0 ) THEN
C           vertical finger pad.
            XT4=X2
            X2=X2+TOT
            X5 =XT4-TOT
            XT4=X1
          ELSE
C           horizontal finger pad.
            XT4=X1-TOT
            X1 =X1+TOT
            X5 =X2
          END IF
          CALL PLTL03(X1,X2,XT4,X5)
        ELSE
          CALL HARARC(XT1,XT2,XT4,XT5,X6)
        END IF
      ELSE
        CALL PLTARC(XT1,XT2,XT4,XT5,X6)
      END IF
C
      END
*
      SUBROUTINE PLTL03(X1,Y1,X2,Y2)
C     ==============================
C
      include  'include/masti.inc'
      include  'include/lfont.inc'
      include  'include/pendat.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/gerber.inc'
 
      REAL X1,Y1,X2,Y2,PI,CANG,PT,DEG,
     +     XT1,YT1,XT2,YT2,
     A     WX1,WY1,WX2,WY2,
     B     VX1,VY1,VX2,VY2,
     C     L1,L2,L3,INC,STEP,TOT,EXT,VAL,TOT1,
     D     N1(2),N2(2),N3(2)
      CHARACTER*80 OLINE
      INTEGER I,END,THK
      LOGICAL OK,SAME
      EXTERNAL PI,CANG,OUTLIN,DEG,SAME
C
C     line still in world coordinates have
C     to convert to screen coordinates
      EXT=0.0
      XT1=X1
      YT1=Y1
      XT2=X2
      YT2=Y2
C      WRITE(10,*) '[PLTL03]: MIP,XT1,YT1,XT2,YT2:',MIP,XT1,YT1,XT2,YT2
C     check for line thickness
C
      I=IMBUFF(12)
C     do not output anything if Flash tool
C      WRITE(10,*)'[PLTA03] I,LTHKR(1,I) ',I,LTHKR(1,I)
      IF(LTHKR(1,I).LT.0) RETURN
      CALL CHGTHK(I)
      THK=PLTHKI(I)
C
      IF ( THK.GT.0 ) THEN
        I=MOD(THK,64)
        TOT=ABS(PAPTOW*((LTHKR(1,I)-(LTHKR(2,I)))/2.0))
        END=MOD(THK,8192)/1024
        CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
        IF(SAME(X1,X2).AND.SAME(Y1,Y2)) RETURN
        IF ( END.GT.1 ) THEN
          EXT=TOT
          CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
          CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
        END IF
      END IF
C      WRITE(10,*) 'THK INDEX,TOT,END:',THK,TOT,END
      IF(CLIPIT) THEN
          CALL CLIPP(XT1,YT1,XT2,YT2,OK)
      ELSE
          OK=.TRUE.
      END IF
 
      IF ( .NOT.OK ) RETURN
 
C     Line thickness draw
      IF ( THK.GT.0.AND.TOT.GT.0 ) THEN
        PT=((LTHKR(2,MOD(THK,64))/2.0)*FACT)
        CALL PTOWVX(PT,STEP)
        I=MAX(NINT(TOT/STEP),1)
        STEP=TOT/REAL(I)
        INC=STEP
        CALL WO2PL(XT1,YT1,VX1,VY1)
        CALL WO2PL(XT2,YT2,VX2,VY2)
C       now we have screen coords VX1,VY1,VX2,VY2
C       must draw line with correct fonting
        CALL OUTLIN(VX1,VY1,VX2,VY2)
 5      CONTINUE
C        WRITE(10,*) 'INC,TOT:',INC,TOT
        IF ( INC.LE.TOT ) THEN
          CALL VV0L15(L1,L2,L3,INC,N1(1),N2(1),N3(1),
     +                             N1(2),N2(2),N3(2))
          IF ( END.EQ.2 )  EXT=SQRT(TOT*TOT-INC*INC)
          IF ( EXT.GT.0 ) THEN
            CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
            CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
          ELSE
            XT1=X1
            XT2=X2
            YT1=Y1
            YT2=Y2
          END IF
          DO 10 I=1,2
            CALL VC0PLP(N1(I),N2(I),N3(I),XT1,YT1,WX1,WY1)
            CALL VC0PLP(N1(I),N2(I),N3(I),XT2,YT2,WX2,WY2)
            IF(CLIPIT) THEN
               CALL CLIPP( WX1,WY1,WX2,WY2,OK)
            ELSE
               OK=.TRUE.
            END IF
            IF ( OK ) THEN
              CALL WO2PL(WX1,WY1,VX1,VY1)
              CALL WO2PL(WX2,WY2,VX2,VY2)
C             once converted draw line on screen
C      WRITE(10,*) '[PLTL03]:LINE ',WX1,WY1,WX2,WY2
              CALL OUTLIN(VX1,VY1,VX2,VY2)
            END IF
 10       CONTINUE
          INC=INC+STEP
          GOTO 5
        END IF
      ELSE
        CALL WO2PL(XT1,YT1,VX1,VY1)
        CALL WO2PL(XT2,YT2,VX2,VY2)
C        WRITE(10,*) '[PLTL03] XT1,YT1,XT2,YT2 ',XT1,YT1,XT2,YT2
C        WRITE(10,*) '[PLTL03] VX1,VY1,VX2,VY2 ',VX1,VY1,VX2,VY2
C       now we have screen coords VX1,VY1,VX2,VY2
C       must draw line with correct fonting
        CALL OUTLIN(VX1,VY1,VX2,VY2)
      END IF
 
      END
*
*
 
