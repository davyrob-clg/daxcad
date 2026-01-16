C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insertc.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INSD01()
C     SUBROUTINE INSDS1(FUNIT,TYPE)
C     SUBROUTINE INSDS2(FLAG)
C     SUBROUTINE MNICUR
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE INSD01()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT CURVE option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include 'include/menun.inc'
      include 'include/lfu.inc'
      include 'include/save.inc'
C
      INTEGER*4 CP,C,TMEN,TCELL,FUNIT,REC,TYPE,TWCELL
      CHARACTER*20 INPL
      LOGICAL OK,CVERFY
      REAL X,Y
C
      EXTERNAL TCURS,CVERFY
C
      REC=24
      CALL OURSCR(FUNIT,REC,OK)
C
C     Making B-spline the default insert curve
      MEN=3
      CCMD='b'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,CELLN)
      CALL GTMCHI(MEN,CELLN)
C     kid on we just hit a spline
      GOTO 104
C
 103  continue
C
C     find type of arc required
      CALL TCURS(C,X,Y)
C
 104  CONTINUE
C     if another major option,return to INSERT control routine
      IF (MEN.EQ.2.OR.CCMD.EQ.'q') GOTO 605
C     select type of arc entry required
      IF (MEN.EQ.3) THEN
 105     CONTINUE
         TCELL=CELLN
         TMEN=MEN
C        arc construction options in here
         IF (CCMD.EQ.'b' ) THEN
C           'B' Hit  'stores a B-spline
            CALL GTMCHI(MEN,CELLN)
            TYPE=24
            CALL INSDS1(FUNIT,TYPE)
         ELSE IF (CCMD.EQ.'h' ) THEN
C           This is a hermite curve
            CALL GTMCHI(MEN,CELLN)
            TYPE=16
            CALL INSDS1(FUNIT,TYPE)
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            CALL GTMCLO(MEN,CELLN)
            GOTO 103
         END IF
C        if this is menu3 then we could be trying for another curve
         CALL GTMCLO(TMEN,TCELL)
         IF(TYPE.EQ.16) THEN
C            hermite thumb weight cell
             INPL='              '
             CALL GTMCWT(3,'t',INPL)
         ENDIF
         IF(CVERFY(CCMD,'bh')) THEN
             GOTO 105
         ELSEIF(CVERFY(CCMD,'Qq').OR.MEN.NE.3) THEN
             GOTO 605
         ELSE
             CALL GTMCLO(MEN,CELLN)
             GOTO 103
         ENDIF
      END IF
      CALL DEPRNT(38)
      CALL GTMCLO(MEN,CELLN)
      GOTO 103
C
 605  CONTINUE
      CLOSE(FUNIT)
CIBM
C      LFU(FUNIT)=.FALSE.
CIBM
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE INSDS1(FUNIT,TYPE)
C     =============================
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/save.inc'
      include 'include/viewport.inc'  
      include 'include/gtxt2.inc'  
      include 'include/vntable.inc'  
C
      INTEGER*4 NP,FUNIT,MODE,MNCODE,I,MINP,TYPE,CELL
      INTEGER*4 NLEN,TWCELL,DNUM
      INTEGER*2 P,ENT,POINT,SMIPOS,SPDP,LP,DFPT
      DOUBLE PRECISION DN
      LOGICAL OPTION,QUIT,OK,DELETE,CVERFY,HEADC
      REAL FP(3),HPX,HPY,M(3,3),TW
      REAL XMAX,XMIN,YMAX,YMIN
      CHARACTER*80 STRING,INPL,FORM*6
      EXTERNAL NLEN,AEXPRN,CVERFY
C
C     save current masterindex pointers
C
      POINT=0
C
      HEADC = .FALSE.
      ENT=SPLINE
      IF (TYPE.EQ.16) THEN
         MINP=4
      ELSE IF (TYPE.EQ.24) THEN
         MINP=3
      ENDIF
C
      TW=0.25
      FORM='(F7.3)'
      CALL GTHFMC(3,'t',TWCELL)
      IF(TYPE.EQ.16) THEN
C         hermite thumb weight cell
          CALL GTMCWR(MEN,'t',TW,FORM)
      ENDIF
 1    CONTINUE
C
C     save current database pointers
C      WRITE(10,*) '[INSDS1] NMIPOS= ',NMIPOS,' NPDPOS= ',NPDPOS
      CALL INSDS2(.TRUE.)
      CALL DCPRNT(495)
C     set limits of curve
      XMIN=1E30
      YMIN=1E30
      XMAX=-1E30
      YMAX=-1E30
C
 5    CONTINUE
C
C     go find an point to put it through
      CALL  FINDP0(164,HPX,HPY,OPTION,QUIT)
C
      IF ((OPTION.AND.MEN.EQ.2).OR.
     +    QUIT.OR.CVERFY(CCMD,'bh')) THEN
C         reset flags
C         Reset pointers
          IF(HEADC) THEN
               CALL ERSSPL(SMIPOS,.FALSE.,M)
C              delete from display file
               DFPT=LDFILE(CVPN)-1
               CALL WDISPF(DFPT,SPLINE,-SMIPOS,OK)
C              get data
               CALL DIR500(SMIPOS,OK)
C              delete from layer monitor.
               CALL DELMON(IMBUFF(4),.FALSE.)
               CALL INSDS2(.FALSE.)
          ENDIF
          RETURN
      ENDIF
C
      IF ( OPTION ) THEN
C
         IF ( CCMD.EQ.CHAR(150) ) THEN
            IF ( POINT.LT.MINP ) THEN
               CALL DEPRNT(155)
               CALL GTMCLO(MEN,CELLN)
            ELSE
               CALL ERSSPL(SMIPOS,.FALSE.,M)
               CALL DIR500(SMIPOS,OK)
               IMBUFF(5)=SMODE+TYPE
               CALL DIM500(SMIPOS,OK)
C              Thumb weight
               DO 50 I=1,6
                   RDBUFF(I)=0.0
 50            CONTINUE
               CALL DBR500(IMBUFF(7),OK)
               RDBUFF(6)=TW
               POINT=IDBUFF(3)
               CALL DBM500(IMBUFF(7),OK)
 
C              update limits
               CALL CHGE40(SMIPOS)

               VPADD = .TRUE.
               ENT=SPLINE
               CALL ALLDRW(ENT,SMIPOS)
               VPADD = .FALSE.

               CALL GTMCLO(MEN,CELLN)
C              valid curve save it
               POINT=0
               HEADC = .FALSE.
               GOTO 1
            END IF
         ELSE IF ( CCMD .EQ. 'c' ) THEN
            IF ( POINT .GT. 0 ) THEN
               CALL DBR500(SPDP,OK)
               CALL ECROSS ( RDBUFF(1),RDBUFF(2) )
               CALL DBR500(LP,OK)
               IDBUFF(3)=0
               CALL DBM500(LP,OK)
               POINT=POINT-1
               NPDPOS = NPDPOS - 1
               SPDP=SPDP-1
               LP=LP-1
            ELSE
               CALL DEPRNT(419)
            END IF
            CALL GTMCLO(MEN,CELLN)
         ELSE IF ( CCMD .EQ. 'm' ) THEN
C           set MODE of curve to be created
            CELL = CELLN
            MNCODE = 18
            CALL MENPOP(MNCODE,OK)
            CALL GTMCLO(3,CELL)
            IF (OK) THEN
C               Update mode.
                SMODE = ICHAR(CCMD) - 1
C               Update mode display cell.
                GTMULT = .TRUE.
                CALL GTDMWT(359,3,VNOUN(360+SMODE))
            ENDIF
         ELSE IF ( CCMD .EQ. 't'.AND.TYPE.EQ.16 ) THEN
C           set thumb weight for HERMITE curve
C           "Enter thumb weight (in range 0 to 1):"
            DNUM=533
200         CONTINUE
            CALL DPRMXP(DNUM,INPL)
            IF (NLEN(INPL).NE.0) THEN
               CALL AEXPRN(INPL,DN,*200)
C              apply range check to keep within 0 to 1
               IF (DN.GT.1 .OR. DN.LT.0) GOTO 200
C              accept in range thumb weight
               TW=DN
            ENDIF
C           show new thumb weight
            CALL GTMCWR(MEN,CCMD,TW,FORM)
         ELSE
            CALL DEPRNT(8)
            CALL GTMCLO(MEN,CELLN)
         END IF
C
         GOTO 5
C
      END IF
C
      IF(POINT.EQ.0.AND..NOT.HEADC)  THEN
C         create header
          VPADD = .TRUE.
          CALL DEWC07(TYPE,CLAYER,SMIPOS,SPDP,OK)
          VPADD = .FALSE.
          HEADC = .TRUE.
      ENDIF
      LP=SPDP
      POINT=POINT+1
C     clear buffer
      DO 40 I=1,6
          RDBUFF(I)=0.0
 40   CONTINUE
 
C     value of the point
      RDBUFF(1)=HPX
      RDBUFF(2)=HPY
C     set the part data info
      IDBUFF(1)=SPLINE
      IDBUFF(2)=SMIPOS
      CALL DBW500(P,OK)
      CALL DBR500(SPDP,OK)
      IDBUFF(3)=P
      CALL DBM500(SPDP,OK)
      SPDP=P
      CALL WCROSS(HPX,HPY)
C
      GOTO 5
C
      END
C
C     -------------------------------------------------------
C




      SUBROUTINE INSDS2(FLAG)
C     ======================
C1    VARTYPE             L
C1    IOSTATUS            I
C
C2    This routine sets and resets the master indedx pointers of the curve
C
      include 'include/save.inc'
      include 'include/masti.inc'
C
      LOGICAL FLAG
C
      IF (FLAG) THEN
          TMPMIP=NMIPOS
          TMPPDP=NPDPOS
      ELSE
          NMIPOS=TMPMIP
          NPDPOS=TMPPDP
      ENDIF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNICUR
C     =================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the INSERT CURVE option list.
C2
C2
      include 'include/ndata.inc'
      include  'include/gtxt2.inc'
      include  'include/vntable.inc'
C
      EXTERNAL GTDMEN,GTCLRM,GTDMWT
C
C     Clear minor option menu.
      CALL GTCLRM(3)
C
C     Display the lines display mode.
      GTMULT = .TRUE.
      CALL GTDMWT(359,3,VNOUN(369))
C     Load up B-spline
      CALL GTDMEN(350,3)
      CALL GTDMEN(351,3)
      CALL GTDMEN(353,3)
C
      CALL GTDMEN(356,3)
C     and accept
      CALL GTDMEN(358,3)
C     Load the point modes.
      CALL MNLPTS()
C
      END
C
C     ---------------------------------------------------------
C
 
