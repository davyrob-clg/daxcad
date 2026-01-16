C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writept.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE WRTPT1()
C     SUBROUTINE WRTPT2(FUNIT,OUNIT,TYPE,N,ORGX,ORGY,NUMTRP)
C     SUBROUTINE WRTPT3(FUNIT,NP)
C     SUBROUTINE WRTPTO(LINE,OUNIT)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE WRTPT1()
C     ====================
C2    This routine will write to a file in the PATHTRACE
C2    NC machining system
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/hdata.inc'
      include 'include/lfu.inc'
      include 'include/params.inc'
C
      DOUBLE PRECISION DN
      CHARACTER*80 OLIN,SEQ*20,UNF*20,MODE*2,BUFF,DBU*2
      INTEGER*4 I,REC,TYPE,SKNO,INCK,NLEN,OUNIT
      INTEGER*4 IP,TMEN,TCELL,NUMTRP,TUNIT,NLEN1
      INTEGER*2 FN
      REAL ORGX,ORGY,HPX,HPY
      LOGICAL OK,YES,OPTION,QUIT,HEADW
C
      EXTERNAL OURSCR,DCPRNT,DEPRNT,PULENT,MEASA3,NLEN,NLEN1
C
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU.NE.'MM' .AND. DBU.NE.'IN' ) THEN
C       wrong units for GNC input
         I=448
         CALL DEPRNT(I)
C        abort process
         MEN=0
         GOTO 999
      END IF
      REC=52
      HEADW=.TRUE.
      NUMTRP=0
      CALL OURSCR(HUNIT,REC,OK)
      REC=80
      CALL OURSCR(TUNIT,REC,OK)
      IF ( .NOT. OK ) THEN
C        This would be a strange thing but it could happen
         I=143
         CALL DCPRNT(I)
         I=90
         CALL DEPRNT(I)
         GOTO 999
      END IF
C
      CALL DPRMXP(81,OLIN)
C     if null file name assume abort required
      IF ( NLEN(OLIN).EQ.0 ) THEN
         MEN=0
         GOTO 997
      END IF
      INQUIRE(FILE=OLIN,EXIST=YES)
      INQUIRE(FILE=OLIN,SEQUENTIAL=SEQ,UNFORMATTED=UNF)
C
      IF ( YES ) THEN
C     WRITE(10,*) 'GNC',SEQ,UNF
         IF ( SEQ.NE.'YES'.OR.UNF.NE.'NO' ) THEN
            CALL DEPRNT(11)
            GOTO 997
         END IF
         CALL DELETE(OLIN,YES)
C     WRITE(10,*) 'GNC File deletion was ',YES
         IF ( .NOT. YES  ) THEN
            CALL DEPRNT(707)
            GOTO 997
         END IF
      END IF
      CALL FINDU2(OUNIT,OLIN,YES)
      IF ( .NOT. YES ) THEN
         RETURN
      END IF
      OPEN(UNIT=OUNIT,FILE=OLIN,STATUS='NEW',ACCESS='SEQUENTIAL')
CIBM
C      LFU(OUNIT)=.TRUE.
CIBM
699   CONTINUE
C     find origin for GNC data
      CALL MNLPTS()
C     prompt for origin point
      I=44
      CALL FINDP0(I,ORGX,ORGY,OPTION,QUIT)
      CALL MNUPTS()
      IF (QUIT) GOTO 998
      IF (OPTION) GOTO 699
      CALL WO2SC(ORGX,ORGY,HPX,HPY)
      CALL BCROSS(HPX,HPY)
C     default the span curve type
      TYPE=1
C     Find menu cell which has been set for Profile
      CALL FNDPOS(301,I)
      CALL GTMCHI(3,I)
      TMEN=3
      TCELL=I
 10   CONTINUE
      IF ( TYPE.EQ.1 )  THEN
C        profile curve.
         FN=2
         CALL PULENT(FN)
         I=NO
         MODE=' N'
      ELSE IF ( TYPE.EQ.2 ) THEN
C        points kcurve.
         I=0
         CALL MNLPTS()
         CALL WRTPT3(HUNIT,I)
         CALL MNUPTS()
         MODE=' D'
      END IF
C     switch off profile/points cell
      CALL GTMCLO(TMEN,TCELL)
      IF ( MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C        Get rid of temporary scratch files
         GOTO 998
      END IF
C
      IF ( CCMD.EQ.'P'.OR.CCMD.EQ.'p' ) THEN
         TMEN=MEN
         TCELL=CELLN
      END IF
C
      IF ( I.GT.0 ) THEN
C     write out the header sequence only once
      IF(HEADW) THEN
C         write out a header sequence
C         points or profile (D or N)
          WRITE(OUNIT,'(A)') MODE
C         null line
          WRITE(OUNIT,'(A)')
C         six 0's
          DO 20 IP=1,6
              WRITE(OUNIT,'(A)') ' 0 '
20        CONTINUE
          HEADW=.FALSE.
      ENDIF
          CALL WRTPT2(HUNIT,OUNIT,TYPE,I,ORGX,ORGY,NUMTRP)
      END IF
C     make sure nothing left in buffer.
      CALL UNFLAG(.TRUE.)
C
      IF ( MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C        Get rid of temporary scratch files
         GOTO 998
      END IF
C
      IF ( CCMD.EQ.'P' ) THEN
C        profile option been chosen
C        get rid of what we have here before
C        going back.
         TYPE=1
      ELSE IF ( CCMD.EQ.'p' ) THEN
C        Points option been chosen
C        get rid of what we have here before
C        going to start again.
         TYPE=2
      END IF
 
      IF ( CCMD.NE.CHAR(150) ) GOTO 10
      REWIND(OUNIT)
      I=1
 310  CONTINUE
      READ(UNIT=OUNIT,FMT='(A)',END=300) BUFF
      WRITE(UNIT=TUNIT,REC=I) BUFF
      I=I+1
      GOTO 310
 300  CONTINUE
      REWIND(OUNIT)
      DO 320 REC=1,8
          READ(UNIT=TUNIT,REC=REC) BUFF
          WRITE(UNIT=OUNIT,FMT='(A)') BUFF(:NLEN1(BUFF))
 320  CONTINUE
      WRITE(UNIT=BUFF,FMT='(I6)') NUMTRP-1
      CALL WRTPTO(BUFF,OUNIT)
      DO 330 REC=9,I-1
          READ(UNIT=TUNIT,REC=REC) BUFF
          WRITE(UNIT=OUNIT,FMT='(A)') BUFF(:NLEN1(BUFF))
 330  CONTINUE
C
      CLOSE(UNIT=OUNIT)
      CLOSE(UNIT=TUNIT)
CIBM
C      LFU(OUNIT)=.FALSE.
C      LFU(TUNIT)=.FALSE.
CIBM
C
      CALL POPPD1(OLIN,673,120,350,667)
C      CALL POPPD1(OLIN,100,200,350,350)
C
C     show the user what he has created
C     ********************************************
C     return normally with files closed
C     ********************************************
      RETURN
C
 997  CONTINUE
C     ********************************************
C     return and close scratch file
C     ********************************************
      CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
      RETURN
C     ********************************************
C     return and close both scratch and work files
C     ********************************************
 998  CONTINUE
      CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
      CLOSE(UNIT=OUNIT)
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
      RETURN
C
C     ********************************************
C     return with no action
C     ********************************************
 999  CONTINUE
C
      END
C
C     ----------------------------------------------
C
      SUBROUTINE WRTPT2(FUNIT,OUNIT,TYPE,N,ORGX,ORGY,NUMTRP)
C     ======================================================
C1        vartyp           I4   I4  I4   I4   I4  R  R    I4
C1        iostat            I    I  I     I    I  I  I    I/O
C
C2    This routine calculates and writes the profiles
      include 'include/menun.inc'
      include 'include/wtov.inc'
      include 'include/entity.inc'
      include 'include/params.inc'
C
      CHARACTER*24 FORMAT,OLIN*80
      CHARACTER MODE,DBU*2
      LOGICAL SAME,YES,OP,HOLD,QUIT,OPTION,FULCIR,CW
      INTEGER*2 MIP1,ENT,ENTH
      INTEGER*4 N,I,NP,FUNIT,OUNIT,ON,NLEN,TYPE,NUMTRP,TUNIT
      DOUBLE PRECISION DX1,DY1
      REAL ORGX,ORGY,SINGX,SINGY,O1X,O1Y
      REAL X1,X2,Y1,Y2,Z1,BX,BY,OX,OY,XH1,YH1,XH2,YH2,ZH1,XP1,YP1,
     +     T1,T2,T3,DISTXY,PI
      REAL HPX,HPY
      EXTERNAL SAME,DPI,NLEN,CD0D13,DISTXY,PI
C
C
C        WRITE(10,*) '[WRTPT2] ORGX= ',ORGX,' ORGY= ',ORGY
      IF ( CCMD.NE.'P'.OR.CCMD.NE.'p' ) THEN
         CALL GTMCLO(MEN,CELLN)
      END IF
C     use different format for database units.
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU .EQ. 'MM' ) THEN
         FORMAT='(F12.4)'
      ELSE  IF ( DBU .EQ. 'IN' ) THEN
         FORMAT='(F12.5)'
      END IF
 100  CONTINUE
C
      IF ( TYPE.EQ.1 ) THEN
C        profile needs start point.
C        go find an point to put it through
         READ(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY, X1, Y1, X2, Y2, Z1,
     +                                          XH1,YH1,XH2,YH2,ZH1
        CALL MNLPTS()
        CALL  FINDP0(381,XP1,YP1,OPTION,QUIT)
        IF ( QUIT.OR.OPTION ) RETURN
        CALL MNUPTS()
        CALL WO2SC(XP1,YP1,HPX,HPY)
        CALL BCROSS(HPX,HPY)
        NUMTRP=NUMTRP+1
C
          IF ( ENT .EQ. ARC) THEN
C            arc found
             CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +       DBLE(XP1),DBLE(YP1),DX1,DY1)
             OX=REAL(DX1)
             OY=REAL(DY1)
             WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,
     +       X1,Y1, X2, YH2, ZH1
          ELSE
             CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +       DBLE(XP1),DBLE(YP1),DX1,DY1)
             OX=REAL(DX1)
             OY=REAL(DY1)
             WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,
     +       OX, OY, X2, Y2, Z1,XH1,YH1,XH2,YH2,ZH1
          ENDIF
      END IF
C      WRITE(10,*) '[WRTPT2] OX= ',OX,' OY= ',OY
C     we have to do atest for the number of triplets
C     2 for and arc and 1 for a point or line
      DO 30 I=1,N
         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY, X1, Y1, X2, Y2, Z1,
     +                                          XH1,YH1,XH2,YH2,ZH1
          IF(ENT.EQ.ARC) THEN
              NUMTRP=NUMTRP+2
          ELSE
              NUMTRP=NUMTRP+1
          ENDIF
   30 CONTINUE
C
C     the number of coordinate triplets inculding discontinuity
      DO 20 I=1,N
         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
D       WRITE(UNIT=10,FMT=*) 'MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1'
D       WRITE(UNIT=10,FMT=*) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
         FULCIR=.FALSE.
         IF ( ENT.EQ. ARC ) THEN
C           Is the single boundary a full circle
            IF ( Y2.LT.0.01.AND.
     +           Z1.GE.PI(2.0)-0.01.AND.N.EQ.1 ) THEN
C                Yes it is so we will allow that
               FULCIR=.TRUE.
C     make the full circle start at the bottom
               Z1=PI(3.0/2.0)
               Y2=Z1
C      WRITE(10,*) '[WRPT2] BELTER! WE GOT A FULL CIRCLE CONDITION'
            END IF
            XH1=X1+X2*COS(Z1)
            YH1=Y1+X2*SIN(Z1)
            XH2=X1+X2*COS(Y2)
            YH2=Y1+X2*SIN(Y2)
            IF(FULCIR) THEN
                 OX=XH1
                 OY=YH2
            ENDIF
C           calculate mid point of arc
C         WRITE(10,*) 'old x,y',OX,' ',OY
C         WRITE(10,*) 'End angle',XH1,' ',YH1
C         WRITE(10,*) 'Start angle',XH2,' ',YH2
            IF ( SAME(OX,XH1).AND.
     +           SAME(OY,YH1) ) THEN
               OX=XH2
               OY=YH2
               T1=XH1
               T2=YH1
               X2=-X2
C               WRITE(10,*) 'End point used'
            ELSE IF ( SAME(OX,XH2).AND.
     +                SAME(OY,YH2) ) THEN
               OX=XH1
               OY=YH1
               T1=XH2
               T2=YH2
C               WRITE(10,*) 'Start point used'
            ELSE
                WRITE(UNIT=OUNIT,FMT='(A,I3)')
     +          'Geometry error at entity ',I
                WRITE(UNIT=OLIN,FMT='(A,I3)')
     +          'Geometry error at entity ',I
                CALL EPRINT(OLIN)
 
            END IF
C           if the entity is a full circle then write a start point
C           as well which is 270 degs ie the bottom of the circle
C           and radius positive (thats what it look like to me)
            IF(FULCIR) X2=ABS(X2)
C           If this is a start arc then modify the start point
C           first triplet
            IF(SAME((T1-ORGX),0.0).AND.SAME((T2-ORGY),0.0)) THEN
C               Whose idea was this ?
                WRITE(OUNIT,'(A)') ' 1E-33 '
                WRITE(OUNIT,'(A)') ' 1E-33 '
                WRITE(OUNIT,'(A)') ' 0 '
            ELSE
                WRITE(OLIN,FORMAT) T1-ORGX
                CALL WRTPTO(OLIN,OUNIT)
                WRITE(OLIN,FORMAT) T2-ORGY
                CALL WRTPTO(OLIN,OUNIT)
                WRITE(OUNIT,'(A)') ' 0 '
            ENDIF
C           second triplet
            WRITE(OLIN,FORMAT) X1-ORGX
            CALL WRTPTO(OLIN,OUNIT)
            WRITE(OLIN,FORMAT) Y1-ORGY
            CALL WRTPTO(OLIN,OUNIT)
            WRITE(OLIN,FORMAT) X2
            CALL WRTPTO(OLIN,OUNIT)
            SINGX=OX
            SINGY=OY
         ELSE IF ( ENT .EQ. LINE ) THEN
            IF(X1-ORGX.EQ.0.AND.Y1-ORGY.EQ.0) THEN
C               Whose idea was this ?
                WRITE(OUNIT,'(A)') ' 1E-33 '
                WRITE(OUNIT,'(A)') ' 1E-33 '
                WRITE(OUNIT,'(A)') ' 0 '
            ELSE
                WRITE(OLIN,FORMAT) X1-ORGX
                CALL WRTPTO(OLIN,OUNIT)
                WRITE(OLIN,FORMAT) Y1-ORGY
                CALL WRTPTO(OLIN,OUNIT)
                WRITE(OUNIT,'(A)') ' 0 '
            ENDIF
            OX=X2
            OY=Y2
         ELSE IF ( ENT .EQ. 2 ) THEN
            WRITE(OLIN,FORMAT) X1-ORGX
            CALL WRTPTO(OLIN,OUNIT)
            WRITE(OLIN,FORMAT) Y1-ORGY
            CALL WRTPTO(OLIN,OUNIT)
            WRITE(OUNIT,'(A)') ' 0 '
            OX=X1
            OY=Y1
         END IF
 
 20   CONTINUE
C
      IF ( TYPE.EQ.1 ) THEN
C        profile needs end point.
C        go find an point to put it through
         IF (.NOT.(ENT.EQ.ARC.AND.N.EQ.1) ) THEN
             CALL MNLPTS()
             CALL  FINDP0(382,XP1,YP1,OPTION,QUIT)
             CALL MNUPTS()
C
             IF ( QUIT.OR.OPTION ) RETURN
             CALL WO2SC(XP1,YP1,HPX,HPY)
             CALL BCROSS(HPX,HPY)
 
         READ(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY, X1, Y1, X2, Y2, Z1,
     +                                          XH1,YH1,XH2,YH2,ZH1
             IF ( ENT .EQ. ARC) THEN
C               arc found
                CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +          DBLE(XP1),DBLE(YP1),DX1,DY1)
                OX=REAL(DX1)
                OY=REAL(DY1)
             ELSE
                CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +          DBLE(XP1),DBLE(YP1),DX1,DY1)
                OX=REAL(DX1)
                OY=REAL(DY1)
             END IF
C
         END IF
C        write out the last point
         IF(FULCIR) THEN
             OX=SINGX
             OY=SINGY
             CCMD=CHAR(32)
         ENDIF
C      WRITE(10,*) '[WRTPT2] ',SAME((OX-ORGX),0),SAME((OY-ORGY),0)
 
         IF(SAME((OX-ORGX),0).AND.SAME((OY-ORGY),0)) THEN
             WRITE(OUNIT,'(A)') ' 1E-33 '
             WRITE(OUNIT,'(A)') ' 1E-33 '
         ELSE
             WRITE(OLIN,FORMAT) OX-ORGX
             CALL WRTPTO(OLIN,OUNIT)
             WRITE(OLIN,FORMAT) OY-ORGY
             CALL WRTPTO(OLIN,OUNIT)
         ENDIF
         WRITE(OUNIT,'(A)') ' 0 '
      END IF
C     Discontinue the profile
      WRITE(OUNIT,'(A)') ' 0 '
      WRITE(OUNIT,'(A)') ' 0 '
      WRITE(OUNIT,'(A)') ' 0 '
      NUMTRP=NUMTRP+1
C     The last drill point will have been written anyway so dont bother
      END
C
      SUBROUTINE WRTPT3(FUNIT,NP)
C     ===========================
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
C
      INTEGER*4 NP,FUNIT,II
      INTEGER*2 P,MD,ED,I,J
      LOGICAL OPTION,QUIT,OK
      REAL FP(3),HPX,HPY,RD1,RD2
C
      RD1=0.0
      RD2=0.0
      ED=2
      MD=0
C
 5    CONTINUE
C
C     go find an point to put it through
      CALL  FINDP0(164,HPX,HPY,OPTION,QUIT)
C
      IF ( QUIT.OR.(OPTION.AND.MEN.EQ.2) ) RETURN
C
      IF ( OPTION ) THEN
C
         IF ( CCMD .EQ. CHAR(150) ) THEN
            IF ( NP .GT. 1 ) THEN
               CCMD='p'
               RETURN
            ELSE IF ( NP .EQ. 1 ) THEN
               CALL DEPRNT(155)
               CALL GTMCLO(MEN,CELLN)
            ELSE
               RETURN
            END IF
         ELSE IF ( CCMD .EQ. CHAR(149) ) THEN
            IF ( NP .GT. 0 ) THEN
               READ(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,
     +         FP(1),FP(2),FP(3),RD1,RD2
               CALL WO2SC(FP(1),FP(2),HPX,HPY)
               CALL BCROSS(HPX,HPY)
               NP=NP-1
               CALL GTMCLO(MEN,CELLN)
            ELSE
               CALL DEPRNT(733)
            END IF
         ELSE IF ( CCMD .EQ. 'W' ) THEN
C           Window
            CALL DCPRNT(88)
            CALL WINDOW(.FALSE.)
            DO 111 II=1,NDATA
               CALL RSCRF(II,MD,RD1,RD2,I,J)
               CALL DER500(MD,OK)
               IF ( .NOT. OK ) THEN
                  CALL UNFLAG(.TRUE.)
                  RETURN
               END IF
               NP=NP+1
               WRITE(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,
     +           RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1           RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 111        CONTINUE
            CALL GTMCLO(MEN,CELLN)
         ELSE IF ( CCMD.EQ.'B' ) THEN
C           Next Bounary option
C           going back.
            RETURN
         ELSE IF ( CCMD.EQ.'P' ) THEN
C           profile option been chosen
C           get rid of what we have here before
C           going back.
            RETURN
         ELSE IF ( CCMD.EQ.'p' ) THEN
C           Points option been chosen
C           get rid of what we have here before
C           going to start again.
            RETURN
         ELSE
            CALL DEPRNT(8)
            CALL GTMCLO(MEN,CELLN)
         END IF
 
         GOTO 5
C
      END IF
C
      NP=NP+1
      FP(1)=HPX
      FP(2)=HPY
      FP(3)=0.0
C         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
      WRITE(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,FP(1),FP(2),FP(3),0.0,0.0
      CALL WO2SC(FP(1),FP(2),HPX,HPY)
      CALL BCROSS(HPX,HPY)
C
      GOTO 5
C
      END
C
C     ---------------------------------------------------
C
 
 
      SUBROUTINE WRTPTO(LINE,OUNIT)
C1    VARTYPE            C*10 I4
C1    IOSTAT              I   I
C
C2    writes out a line to unit OUNIT
C
      CHARACTER LINE*(*),INTFIL*80
      REAL NUM
      INTEGER*4 OUNIT,NLEN
      EXTERNAL CRUNCH,NLEN
      CALL CRUNCH (LINE)
      WRITE(UNIT=INTFIL,FMT='(A)') LINE
      READ(UNIT=INTFIL,FMT='(F15.0)') NUM
      IF(NUM.LT.0.0) THEN
          WRITE(UNIT=OUNIT,FMT='(A)') LINE(:NLEN(LINE))
      ELSE
C         Just in case Neil shouts at me I didnt know whether the
C         character would have spaces after the active length
          WRITE(UNIT=OUNIT,FMT='(3A)') ' ',LINE(:NLEN(LINE)),' '
      ENDIF
      END
C     ----------------------------------------------------
C
