C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 special2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE SPCF00()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE SPCF00()
C1    no args reqd
C
C2    This function should trim either one or two splines
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/macro.inc'
      include 'include/ndata.inc'
C
      INTEGER*2 ENT,CURP,TMIP(2),TSPDP,TFPDP,MAXPDP,MINPDP,P2,TPDP
      INTEGER*2 T1PDP
      INTEGER*4 C,I
      REAL HPX,HPY,M(3,3),XS,YS,LIMD,DISTXY,DS,DF,XP1,YP1,X,Y
      REAL DLIM(2),XF,YF
      LOGICAL OPTION,QUIT,DELETE,OK,TYPE(2),FLAGIT
C     load up points mode
10    CONTINUE
      CALL GTCLRM(3)
      CALL MNLPTS()
      DLIM(1)=20.0*PAPTOW
      DLIM(2)=20.0*PAPTOW
      TYPE(1)=.TRUE.
      TYPE(2)=.TRUE.
      MAXPDP=0
      MINPDP=0
      FLAGIT=.FALSE.
C     get the hit point
      CALL  FINDP0(319,HPX,HPY,OPTION,QUIT)
C     unload points menu
      CALL MNUPTS()
      CALL GTCLRM(3)
      IF ( QUIT.OR.OPTION) THEN
          CALL GTCLRM(3)
          CCMD='Q'
          RETURN
      ENDIF
C     draw a cross at the point
      CALL WO2SC(HPX,HPY,XP1,YP1)
      CALL BCROSS(XP1,YP1)
      CALL GTDMEN(13,3)
      CALL GTDMEN(12,3)
      CALL GTDMEN(99,3)
      CALL DCPRNT(690)
      CALL TCURS(C,X,Y)
      IF(CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
          CALL GTCLRM(3)
          RETURN
      ELSE IF (CCMD.NE.CHAR(13)) THEN
C         lo the cancel cell
          CALL GTMCLO(MEN,CELLN)
C         undraw the previous cross
          CALL BCROSS(XP1,YP1)
          GOTO 10
      ENDIF
C     search the data base for splines
      DO 100 CURP=1,NMIPOS-1
         CALL ALLRD(CURP,ENT,M,DELETE)
         IF (DELETE) GOTO 110
         IF(ENT.EQ.SPLINE) THEN
C           spline is found flagit
            FLAGIT=.TRUE.
            TPDP=IMBUFF(7)
C           get the start and end points of this spline
            CALL DBR500(TPDP,OK)
            TPDP=IDBUFF(3)
            CALL DBR500(TPDP,OK)
            TSPDP=IDBUFF(3)
            CALL DBR500(TSPDP,OK)
            XS=RDBUFF(1)
            YS=RDBUFF(2)
 20         CONTINUE
            TPDP=IDBUFF(3)
            IF( TPDP.EQ.0) THEN
                XF=RDBUFF(1)
                YF=RDBUFF(2)
            ELSE
                CALL DBR500 ( TPDP,OK)
                T1PDP=TPDP
                GOTO 20
            ENDIF
            TFPDP=T1PDP
C           compare the points to the hit point
            DS=DISTXY(XS,YS,HPX,HPY)
            DF=DISTXY(XF,YF,HPX,HPY)
            IF ( DS.LT.DLIM(1)) THEN
                DLIM(2)=DLIM(1)
                DLIM(1)=DS
                MAXPDP=MINPDP
                TYPE(2)=TYPE(1)
                MINPDP=TSPDP
                TYPE(1)=.TRUE.
            ELSEIF(DS.LT.DLIM(2)) THEN
                DLIM(2)=DS
                MAXPDP=TSPDP
                TYPE(2)=.TRUE.
            ENDIF
            IF ( DF.LT.DLIM(1)) THEN
                DLIM(2)=DLIM(1)
                DLIM(1)=DF
                MAXPDP=MINPDP
                TYPE(2)=TYPE(1)
                MINPDP=TFPDP
                TYPE(1)=.FALSE.
            ELSEIF(DF.LT.DLIM(2)) THEN
                DLIM(2)=DF
                MAXPDP=TFPDP
                TYPE(2)=.FALSE.
            ENDIF
         ENDIF
 110     CONTINUE
 100  CONTINUE
C     modify the end points of the curves
      IF(.NOT.FLAGIT) THEN
         CALL DEPRNT(691)
         CALL GTCLRM(3)
         RETURN
      ENDIF
      WRITE(10,*) '[SPCF00] HPX= ',HPX,' HPY= ',HPY
      IF(MAXPDP.EQ.0.OR.MINPDP.EQ.0) THEN
         CALL DEPRNT(692)
         CALL BCROSS(XP1,YP1)
         CALL GTMCLO(MEN,CELLN)
         GOTO 10
      ENDIF
C     read the part data of the point in question
C     write the new part data
      CALL DBR500(MINPDP,OK)
      WRITE(10,*) '[SPCF00] RDBUFF(1)= ',RDBUFF(1)
      WRITE(10,*) '[SPCF00] RDBUFF(2)= ',RDBUFF(2)
      RDBUFF(1)=HPX
      RDBUFF(2)=HPY
      WRITE(10,*) '[SPCF00] RDBUFF(1)= ',RDBUFF(1)
      WRITE(10,*) '[SPCF00] RDBUFF(2)= ',RDBUFF(2)
      CALL DBM500(MINPDP,OK)
C     write the new part data
      CALL DBR500(MAXPDP,OK)
      WRITE(10,*) '[SPCF00] RDBUFF(1)= ',RDBUFF(1)
      WRITE(10,*) '[SPCF00] RDBUFF(2)= ',RDBUFF(2)
      RDBUFF(1)=HPX
      RDBUFF(2)=HPY
      WRITE(10,*) '[SPCF00] RDBUFF(1)= ',RDBUFF(1)
      WRITE(10,*) '[SPCF00] RDBUFF(2)= ',RDBUFF(2)
      CALL DBM500(MAXPDP,OK)
      CALL REDRAW()
      GOTO 10
      END
 
 
