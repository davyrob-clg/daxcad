C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 plot2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE PLT066(M)
C     SUBROUTINE PLTARC(XT1,XT2,XT4,XT5,XT6)
C     SUBROUTINE PLTD00()
C     SUBROUTINE PLTD01()
C     SUBROUTINE PLTDIM(OK)
C     SUBROUTINE PLTDRW(ENT,TMIP,USET,M)
C     SUBROUTINE PLTFAW(X,Y,WRAD,SANG,EANG,FONT)
C     SUBROUTINE PLTFLW(X1,X2,X4,X5,FONT)
C     SUBROUTINE PLTHAT(TMIP1,USET,M)
C     SUBROUTINE PLTMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     SUBROUTINE PLTS00()
C     SUBROUTINE PLTS01()
C     SUBROUTINE PLTS02()
C     SUBROUTINE PLTSW0()
C     SUBROUTINE PLTW00()
C     SUBROUTINE PLVIEW(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE PPTS(POINT,USET,M,OK)
C     SUBROUTINE PTOWVX(W,V)
C     SUBROUTINE PWORLD(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE SCAN()
C     SUBROUTINE SETLAY(ALL,QUIT)
C     SUBROUTINE SETPAP(OVROT,TXP1,TYP1,TXP2,TYP2,OK)
C     SUBROUTINE SETPEN(ALL,QUIT)
C     SUBROUTINE SETPLT()
C     SUBROUTINE SETW2P()
C     SUBROUTINE SOFARC(TOL,X1,X2,X4,X5,X6)
C     SUBROUTINE WO2PL(WX,WY,VX,VY)
C     SUBROUTINE WTOPVX(W,V)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE PLT066(M)
C     ====================
C
C1    vartype        R(3,3)
C1    iostatus         I
C
C2    This is a brilliant new component draw routine
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/wtov.inc'
      include 'include/masti.inc'
      include 'include/compkr.inc'
      include 'include/save.inc'
      include 'include/pendat.inc'
C
      CHARACTER*2 INPL
      INTEGER*2 TTMIP,ENTPNT,RELP,STKP
      INTEGER*2 ENT,I,J,DI
      LOGICAL OK,FINISH,DELETE
      LOGICAL L0,L1,L2,L3,L4,VISBAS
C     start relation list
 
C     save logical flags
      L0=OPFLAG(1)
      L1=OPFLAG(6)
      L2=OPFLAG(7)
      L3=OPFLAG(3)
      L4=OPFLAG(11)
C     set opflags to ensure scale done properly
      OPFLAG(1)=.TRUE.
      OPFLAG(6)=.TRUE.
      OPFLAG(7)=.TRUE.
      OPFLAG(3)=.FALSE.
      OPFLAG(11)=.FALSE.
C     save incoming matrix
      CALL I3M(IDM)
      CALL I3M(IMM)
      CALL MULT3M (M,IDM,CDT)
C     set variables
      STKP=0
      RELP = IMBUFF(10)
      FINISH = .FALSE.
      ENTPNT = 4
C
C     if relation pointer is undefined then dinna draw
      IF ( RELP.LE.0 ) GOTO 200
 
100   CONTINUE
C     read the first record and continuation if nec
 
      CALL DRR950(RELP,OK)
 
      IF(RLBUFF(1).NE.200.OR.ENTPNT.EQ.11) THEN
 
          RELP=RLBUFF(2)
C         test for last line
          IF ( RELP.LE.0 ) THEN
              FINISH = .TRUE.
          ELSE
              CALL DRR950(RELP,OK)
C             reset subscript count
              ENTPNT = 4
 
          END IF
      ENDIF
 
      IF ( .NOT.FINISH ) THEN
 
C         save entity to be read
          TTMIP = RLBUFF(ENTPNT)
 
C         are we finished.
          IF (TTMIP.LE.0) THEN
 
               ENTPNT = 11
               GOTO 100
C
          ENDIF
 
          ENTPNT = ENTPNT + 1
 
C         get current entity
          CALL ALLRD ( TTMIP,ENT,ME,DELETE)
 
          IF ( ENT.EQ.COMPI.OR.ENT.EQ.SYMBI ) THEN
C             Its an instance go and draw it but
C             first save the current pointers and transform
 
C             check nesting level and whether instance resolved
C             This variable is set in the include file COMPKR
              IF(STKP.GE.NSTLIM.OR.IMBUFF(10).LE.0) GOTO 100
 
C             increment stack pointer
              STKP = STKP + 1
 
C             save array
              DO 20 I=1,3
                 DO 20 J=1,3
 
 20                STKTFM(J,I,STKP) = CDT(J,I)
 
 
C             save pointers
              RELMIP(STKP) = RELP
              RELSUB(STKP) = ENTPNT
 
C             recover this instance details and start reading it
              FINISH = .FALSE.
              RELP = IMBUFF(10)
              ENTPNT = 4
 
C             update the current drawing transform
              CALL MULT3M(ME,CDT,IMM)
              CALL MULT3M(IMM,IDM,CDT)
 
          ELSE
 
C                 Bog standard just PLOT it
                  CALL ALLTFM(ENT,CDT)
C                 if this entity is on a plotable layer then do it
                  IF(LAYERN( IMBUFF(4)) .EQ. CURPEN) THEN
 
                      CALL PLTDRW(ENT,TTMIP,.TRUE.,CDT)
 
                  ENDIF
 
          ENDIF
 
 
      ELSE
 
C         recover if there is any thing on the stack
 
 
          IF(STKP.EQ.0) THEN
C             nothin on stack go home
              GOTO 200
          ELSE
 
              DO 30 I=1,3
                 DO 30 J=1,3
 30                  CDT(J,I) = STKTFM(J,I,STKP)
 
 
              FINISH=.FALSE.
              RELP = RELMIP(STKP)
 
              ENTPNT = RELSUB(STKP)
              STKP = STKP - 1
 
 
         ENDIF
 
      ENDIF
 
C     go read the next entity whatever it may be
      GOTO 100
 
200   CONTINUE
 
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
      OPFLAG(3)=L3
      OPFLAG(11)=L4
 
      END
C
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTARC(XT1,XT2,XT4,XT5,XT6)
C     ======================================
C
C1    vartyp             R   R   R   R   R
C1    iostatus           I   I   I   I   I
C
C2    Subroutine PLTARC sets the Interpolation level
C2    and redirects to the appropriate Arc plotting
C2    function depending upon which plotter model is
C2    currently selected.
C2      MODEL  NO       NAME
C2              1      'HP 7475'.
C2              2      'HP 7585B'.
C2              3      'Gould 6320'.
C2              4      'Benson 1625/45 (Z80) (VDF,TIM100)'.
C2              5      'HP 7550'.
C2              6      'ROLAND A2'.
C2              7      'Calcomp '.
C2     ref 2    8      'HP 7580B'.
C2              9      'Hitachi A3/A4'.
C2             10      'Benson 1302  (ISDP)'.
C2             11      'Gerber Photoplotter'.
C2     ref 2   12      'HP 7586B'.
C2             13      'metric advent'.
C2             14      'postscript (300 dpi)'.
C2             15      'Roland  DPX 3300'
C2             16      'Houston DMP language'
C2             17      'Interleaf'
C
      include 'include/pendat.inc'
C
      REAL XT1,XT2,XT4,XT5,XT6,PRAD
C
C            1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
      GOTO ( 1,1,1,2,1,1,2,1,1,2,4,1,2,3,1,1,5,1) PLOMOD
C     *******************************************
 1    CONTINUE
C     *******************************************
C     ***  HP-GL  type Plotter                ***
C     *******************************************
C     Arc Tolerance is set to 1.8 Thou (0.0018 Inch)
C     set arc interpolation tolerance (plotter units)
C     Plotter units are Thousands of Inch (0.001 Inch)
      PRAD=1.8
      IF ( PLOWIN.OR..NOT.HARD.OR.PLTVPT  ) THEN
        CALL SOFARC(PRAD,XT1,XT2,XT4,XT5,XT6)
      ELSE
        CALL HARARC(XT1,XT2,XT4,XT5,XT6)
      END IF
      GOTO 10
C     *******************************************
 2    CONTINUE
C     *******************************************
C     ***  BENSON type Plotter                ***
C     *******************************************
C     Arc Tolerance is set to 1.8 Thou (0.0018 Inch)
C     set arc interpolation tolerance (plotter units)
C     Plotter units are Centimetres (1 cm)
      PRAD=0.004
C     generate arc by software
      CALL SOFARC(PRAD,XT1,XT2,XT4,XT5,XT6)
      GOTO 10
C     *******************************************
 3    CONTINUE
C     *******************************************
C     ***  POSTSCRIPT type Plotter            ***
C     *******************************************
      IF ( PLOWIN.OR.PLTVPT  ) THEN
C       set arc interpolation tolerance (plotter units)
        PRAD=0.05
C       generate arc by software
        CALL SOFARC(PRAD,XT1,XT2,XT4,XT5,XT6)
      ELSE
C       generate arc by hardware.
        CALL HARARC(XT1,XT2,XT4,XT5,XT6)
      END IF
      GOTO 10
C     *******************************************
 4    CONTINUE
C     *******************************************
C     ***  GERBER Photo Plotter               ***
C     *******************************************
C     set arc interpolation tolerance (plotter units)
C     Plotter Units are Thou (Thousandths of an Inch)
C     Hi-Res mode of the plotter allows for a resolution
C     of 1/10 Thou.
C     Arc-Tolerance default is set to 0.5 of resolvable unit.
      PRAD=ARCTOL
      IF ( PLOWIN.OR..NOT.HARD.OR.PLTVPT  ) THEN
        CALL SOFARC(PRAD,XT1,XT2,XT4,XT5,XT6)
      ELSE
        CALL HARARC(XT1,XT2,XT4,XT5,XT6)
      END IF
      GOTO 10
C     *******************************************
 5    CONTINUE
C     *******************************************
C     ***  I n t e r l e a f                  ***
C     *******************************************
C     set arc interpolation tolerance (plotter units)
      PRAD=.001
      IF ( PLOWIN.OR.PLTVPT ) THEN
         CALL SOFARC(PRAD,XT1,XT2,XT4,XT5,XT6)
      ELSE
         CALL HARARC(XT1,XT2,XT4,XT5,XT6)
      ENDIF
      GOTO 10
C     *******************************************
 10   CONTINUE
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTD00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine PLTD00 is the mediator for the
C2    PLOT DRAWING function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,PLTD01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the PLOT DRAWING routine
      CALL PLTD01()
C     ensure option for trim arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTD01()
C     ===================
C1    no arguments required
C
 
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/pendat.inc'
      include 'include/wtov.inc'
C
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,PLTD02
C
      TMEN=MEN
      TCELL=CELLN
C
C     enter the TRIM ARC routine
      PLOWIN=.FALSE.
      XP1=WPXMIN
      YP1=WPYMIN
      XP2=WPXMAX
      YP2=WPYMAX
      CALL PLTD02(.FALSE.)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTDIM(OK)
C     ======================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine PLTDIM is a general routine for PLOTTING of
C2    any  dimension type.
C
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/masti.inc'
C
      REAL PDAT(2,4)
      INTEGER*4 I,RCDCNT,K,LCRDAT(10),I2I4,IWORK2(10),FONT
      LOGICAL OK
      EXTERNAL PLTTXT,PLTFLW,AROHED,I2I4,PLTA05
C
      OK=.FALSE.
C     set the font style to be solid.
      FONT=1
      K=0
C     Loop through the stored dimension records.
      DO 101 I=1,RECCNT(1)
C     Check supression state of sub-record entity
      IF ( IWORK(4,I) .GE. 0 ) THEN
C        was not surpressed so continue
         IF ( IWORK(1,I) .EQ. TEXSEG ) THEN
C           was a text record so use text buffer as well
            K=K+1
            CALL PLTTXT(RWORK(1,I),RWORK(2,I),RWORK(3,I),
     +           RWORK(4,I),RWORK(5,I),RWORK(6,I),DIMCHR(K))
         ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
C           was arrowhead so must decode the arrowhead parameters.
            CALL AROHED(RWORK(1,I),RWORK(2,I),PAPTOW*RWORK(5,I),
     +                   PAPTOW*RWORK(6,I),RWORK(3,I),RWORK(4,I),
     1                   PDAT(1,1),PDAT(2,1),PDAT(1,2),PDAT(2,2))
            CALL PLTFLW(PDAT(1,1),PDAT(2,1),PDAT(1,2),PDAT(2,2),FONT)
            CALL PLTFLW(RWORK(1,I),RWORK(2,I),PDAT(1,2),PDAT(2,2),
     +                                                       FONT)
            CALL PLTFLW(RWORK(1,I),RWORK(2,I),PDAT(1,1),PDAT(2,1),
     +                                                       FONT)
C
         ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
C           Was a line so use data direct
            CALL PLTFLW(RWORK(1,I),RWORK(2,I),RWORK(4,I),RWORK(5,I),
     +                                                        FONT)
         ELSE IF ( IWORK(1,I) .EQ. ARCSEG ) THEN
C           was an arc type segment
            CALL PLTA05(RWORK(1,I),RWORK(2,I),RWORK(4,I),
     +                               RWORK(5,I),RWORK(6,I))
         ELSE
C           Unrecognised subrecord type.
C            WRITE(10,*)'[PLTDIM] Non Geometric Sub-Record ',IWORK(1,I)
         END IF
      END IF
 101  CONTINUE
C     end of draw routine.
      OK=.TRUE.
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTDRW(ENT,TMIP,USET,M)
C     ==================================
C1
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/pendat.inc'
C
      INTEGER*4 I4,FSTYLE,ENT4,DRWTYP
      INTEGER*2 ENT,TMIP
      REAL M(3,3)
      LOGICAL OK,USET
C
      EXTERNAL DIR500,DBR500,DTR500,DRWFLW,DRWFAW,DRWHT,
     +         DRWTXT,DRWDIM
C
C     check that the entity is on this layer.
      IF ( .NOT.VLAYER(IMBUFF(4)) ) RETURN
      IF ( CURPEN.NE.LAYERN(IMBUFF(4)) ) RETURN
C      WRITE(10,*) 'PLDRW',CURPEN,IMBUFF(4),LAYERN(IMBUFF(4))
 
C
      ENT4 = ENT
C
C           1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
C          21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
C          41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
C          61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
C          81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99100
C
      GOTO( 1, 2, 3, 1, 5, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1,30,31, 1,33,33,33,33,33, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1,85, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     1      ) ENT4
C
 1    CONTINUE
      RETURN    
 2    CONTINUE
C        Marker to be drawn
         CALL PLTMRK(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),RDBUFF(6),IMBUFF(5),IMBUFF(6))
         RETURN
C
 3    CONTINUE
C        Line to be drawn
         I4=IMBUFF(6)
         CALL PLTFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I4)
C
      RETURN
 5    CONTINUE
C        Arc to be drawn
         I4=IMBUFF(6)
         CALL PLTFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),RDBUFF(6),I4)
      RETURN
 7    CONTINUE                 
         DRWTYP = -1
         CALL DRWSPL(TMIP,USET,M,DRWTYP)
      RETURN
 30   CONTINUE
C        Center line to be drawn.
         CALL DRWCEN(.FALSE.)
 31   CONTINUE
C        Hatch to be drawn
         CALL PLTHAT(TMIP,USET,M)
      RETURN
 33   CONTINUE
C        Dimension dimension to be plotted
         CALL PLTDIM(OK)
      RETURN
 85   CONTINUE
C        Text to be drawn.
         CALL PLTTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),RDBUFF(4)
     +                        ,RDBUFF(5),RDBUFF(6),CBUFF)
 
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTFAW(X,Y,WRAD,SANG,EANG,FONT)
C     ==========================================
C
C1    vartype           R R  R    R    R    I4
C1    iostatus          I I  I    I    I    I
C
C2       Subroutine PLTFAW draw a arc  centre X,Y
C2       with radius WRAD drawing anti-clockwise
C2       using line font FONT
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      INTEGER*2 TMIP1
      INTEGER*4 FONT,NFPATS,I,J,LFSEGP,INT
C
      REAL X,Y,WRAD,SANG,EANG,ANGLE,TEMP
      REAL PI,RADS,SRAD,ANG1,ANG2,PATLEN,WFONTL,ABS
C
      LOGICAL TOTAL,OK,VISIBL,PENDWN
C     INTRINSIC ABS,INT
C
      EXTERNAL PLTA05
C
C     We are going to have to draw something now.
      IF ( FONT.EQ.1 ) THEN
C        solid line construction wil do
         CALL PLTA05(X,Y,WRAD,SANG,EANG)
      ELSE
C        fonting is required,must do some work
C        find pointer to font segment data
         LFSEGP=LFONTI(1,FONT)
C        find world length for complete font pattern in radians
         PATLEN=PAPTOW*LFONTR(1,FONT)/WRAD
C        find INCLUDEd angle of arc
         ANGLE=ABS(EANG-SANG)
         IF (EANG.LT.SANG) ANGLE=PI(2.0)-ANGLE
C        now find number of complete patterns
         NFPATS=INT(ANGLE/PATLEN)
C        trap for NFPATS=0
         IF (NFPATS.LT.1) THEN
C           cannot draw a proper font pattern
C           so just draw solid
            CALL PLTA05(X,Y,WRAD,SANG,EANG)
            GOTO 99
         END IF
C        now calculate actual font length
         WFONTL=ANGLE/NFPATS
C        update incremental seg lengths for
C        line font in use here,should save
C        some time during drawing of arc
C        cycle through pattern segments
         DO 50 I=0,LFONTI(2,FONT)-1
C           save incremental angle for this segment
            LFTAB2(2,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*WFONTL
 50      CONTINUE
C        all pattern segment increments up to date
C
C        set first position on arc
         ANG1=SANG
C        now go and draw the fonted arc
C        draw the required number of patterns
         DO 70 I=1,NFPATS
C           draw a single font pattern
C           assume starts with solid segment
            PENDWN=.TRUE.
            DO 60 J=0,LFONTI(2,FONT)-1
C              draw a single segment
               ANG2=ANG1+LFTAB2(2,LFSEGP+J)
               IF (PENDWN) CALL PLTA05(X,Y,WRAD,ANG1,ANG2)
C              update start point for next segment
               ANG1=ANG2
C              toggle pen status
               PENDWN=.NOT.PENDWN
 60         CONTINUE
 70      CONTINUE
C
      END IF
 99   CONTINUE
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTFLW(X1,X2,X4,X5,FONT)
C     ===================================
C1                        I2
C1                         I
C2     This routine uses the passed MIP pointer
C2     to read the part data file at that position.
C2     the data held there will be the definition
C2     for a line , which is then converted into the
C2     appropriate plotter commands. Data read in part
C2     data file held in Masti common block.
C2     Checks are done for optimidation , i.e is last plotter
C2     same as one of new ones , if so set new start to old point.
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      CHARACTER*80 LINE*80,OLINE*80
      INTEGER*2 TMIP1
      REAL  X1,X2,X4,X5,X6,DIST1,DIST2,DISTXY,
     +      XT1,YT1,XT2,YT2
      INTEGER*4 FONT,NFPATS,LFSEGP,I,J,NINT
C
      REAL   Y1, Y2,
     +     WX1,WY1,WX2,WY2,
     1     VX1,VY1,VX2,VY2
      REAL SL,LL,LSTSEG,SFONTL,SFDX,SFDY
      REAL SX1,SY1,SX2,SY2,WSL,WL
C
C
      LOGICAL PENDWN,OK
C
      INTRINSIC ABS,SQRT,NINT
C
      EXTERNAL CLIPP,PLTL03,DISTXY
C
C     Read the Part data file at position Tmip1
C     check for fonted line definition.
C
C     Transfer of values so that the end points are not
C     corrupted by clipping subroutine
      WX1=X1
      WY1=X2
      WX2=X4
      WY2=X5
C
C     Clipping routine will supply new line to be drawn
C     If visible logical flag DISPV returned .TRUE.
C      WRITE(10,*) '[PLTFLW] BEFORE CLIPP',WX1,WY1,WX2,WY2
      IF (CLIPIT) THEN
          CALL CLIPP( WX1,WY1,WX2,WY2,DISPV)
      ELSE
         DISPV=.TRUE.
      END IF
C      WRITE(10,*) '[PLTFLW] AFTER CLIPP',WX1,WY1,WX2,WY2,DISPV
C
      IF (.NOT.DISPV) RETURN
 
      FONT=MAX(FONT,1)
C     if font 1 (solid) no tests needed or
C     current viewscale may mean fonting not visible
C     either case means use solid line
C     the limit 0.75 will need adjusted for
C     different display systems
      IF (FONT.EQ.1 ) THEN
C        solid line construction
         CALL PLTL03(WX1,WY1,WX2,WY2)
      ELSE
C        not a solid line,got some work to do
C        find pointer to segment data
         LFSEGP=LFONTI(1,FONT)
C        must do some calcs to allow fonting
C        find line length in world units
         WL=DISTXY(WX1,WY1,WX2,WY2)
C        find world length for complete font patterns
C        WSL is the world segment length
         WSL=LFONTR(1,FONT)*PAPTOW
C        allow for solid segment at end of line
C        save length of end segment
         LSTSEG=LFTAB2(1,LFSEGP)*WSL
         LL=WL-LSTSEG
C        now find number of complete patterns
         NFPATS=NINT(LL/WSL)
C        trap for NFPATS=0
         IF (NFPATS.LT.1) THEN
C           cannot draw a proper font pattern
C           so just draw solid
            CALL PLTL03(WX1,WY1,WX2,WY2)
            RETURN
         END IF
C        now calculate actual font length
         SFONTL=LL/NFPATS
C        find incremental world coords for
C        one pattern length
         SFDX=SFONTL*(WX2-WX1)/WL
         SFDY=SFONTL*(WY2-WY1)/WL
C        update incremental seg lengths for
C        line font in use here,should save
C        some time during drawing of line
C        cycle through pattern segments
         DO 50 I=0,LFONTI(2,FONT)-1
C           save world increments for this segment
            LFTAB2(2,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*SFDX
            LFTAB2(3,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*SFDY
 50      CONTINUE
C        all pattern segment increments up to date
C        set first position on line
         SX1=WX1
         SY1=WY1
C        now go and draw the fonted line
C        draw the required number of patterns
         DO 70 I=1,NFPATS
C           draw a single font pattern
C           assume starts with solid segment
            PENDWN=.TRUE.
            DO 60 J=0,LFONTI(2,FONT)-1
C              draw a single segment
               SX2=SX1+LFTAB2(2,LFSEGP+J)
               SY2=SY1+LFTAB2(3,LFSEGP+J)
               IF (PENDWN) CALL PLTL03(SX1,SY1,SX2,SY2)
C              update start point for next segment
               SX1=SX2
               SY1=SY2
C              toggle pen status
               PENDWN=.NOT.PENDWN
 60         CONTINUE
 70      CONTINUE
C        now draw the last segment
         IF (PENDWN) CALL PLTL03(SX1,SY1,WX2,WY2)
      END IF
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTHAT(TMIP1,USET,M)
C     ===============================
C1                        I2
C1                         I
C2     This routine uses the passed MIP pointer
C2     to read the part data file at that position.
C2     the data held there will be the definition
C2     for a hatch , which is then converted into the
C2     appropriate plotter commands. Data read in part
C2     data file held in Masti common block.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      INTEGER*2 TMIP1,POINT,TFONT,TPDFP
      INTEGER*4 I4,I
      REAL M(3,3),THDIST,L1,L2,L3,DIST,VD0D13
      LOGICAL OK,USET
      EXTERNAL DBR500,PLTFLW,DER500,VD0D13
C
C
C     Read the Part data file at position Tmip1
      THDIST = HDIST
      CALL DER500(TMIP1,OK)
C
      I=0
      TPDFP = IDBUFF(3)
      IF ( TPDFP .EQ. NPDPOS.OR.TPDFP .EQ. 0) THEN
          DISPV = .FALSE.
          RETURN
      ENDIF
C     are there any hatch lines stored
      CALL DBR500(TPDFP,OK)
C     GET first hatch line
      CALL CV0L14(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     +               L1,L2,L3)
      TPDFP=IDBUFF(3)
C
 
 
      IF(TPDFP.GT.0) THEN
C        Read the next hatch line.
         CALL DBR500(TPDFP,OK)
C        get Pitch between lines (Paper Units)
         DIST=VD0D13(L1,L2,L3,RDBUFF(1),RDBUFF(2))/PAPTOW
C        store current hatch pitch
         HDIST = DIST
      ENDIF
      CALL DER500(TMIP1,OK)
 33   POINT=IDBUFF(3)
         I=I+1
         CALL DBR500(POINT,OK)
C        Put out line like normal
         IF ( USET ) CALL MV0003(RDBUFF,M)
         CALL CHKHLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     +   I,IMBUFF(5),TFONT,OK)
         IF ( OK ) THEN
         I4=TFONT
         CALL PLTFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     +               I4)
         END IF
      IF (IDBUFF(3).NE.0) GOTO 33
C
C     restore hatch pitch
      HDIST = THDIST
 
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     ===================================================
C1    VARTYPE           R  R    R     R     R    I*2  I*2
C1    IOSTAT            I  I    I     I     I     I    I
C
C2    This routine is used to draw a marker entity at the
c2    point X1,Y1 at orientation ANGLE with scale in x
C2    SCALX and scale in y SCALY.  The form refers to the
C2    marker index number into the preloaded marker table
c
C
      include 'include/marker.inc'
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include  'include/wtov.inc'
      REAL X1,Y1,ANGLE,SCALX,SCALY,STX,STY,A(3,3),B(3,3),C(3,3)
      REAL NSX1,NSY1,NFX1,NFY1,BX1,BX2,BX3,BX4,BY1,BY2,BY3,BY4
      REAL BLX,BLY,TRX,TRY,D(3,3),RAD1,SANG,EANG,CTX,CTY,WRAD,CIRDIF
      REAL TANGLE,PI,RAD,OX,OY,NX,NY,AP,PRAD,ALPH,TOL,RADS,TRS(3,3)
      INTEGER*2 FORM,FONT,MIND,I,ENTCOL,I1
      INTEGER*4 TOTAL,FONT4,COUNT
      LOGICAL NEWBLK
      EXTERNAL RAD,PI
C
      MIND=MRKIND(FORM)
C
C
      IF(MIND.EQ.0) THEN
         RETURN
      END IF
      TOL = 0.01
                    
C     scale the point arround the world origin
      CALL SCAL2D(SCALX,SCALY,A)
C     rotate the point arround the world origin
      CALL ROT2D(ANGLE,B)
C     Concatenate result
      CALL MULT3M(A,B,C)
c     translate the point arround the world origin
      CALL TRAN2D(X1,Y1,B)
C     Concatenate result
      CALL MULT3M(C,B,A)
 
      CALL MULT3M(A,PLWVXY,C)
      
C     let us transform the bounding box
      CALL NEWXY(MRKR(MIND,1),MRKR(MIND,2),BX1,BY1,A)
      CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND+1,2),BX2,BY2,A)
      CALL NEWXY(MRKR(MIND,1),MRKR(MIND+1,2),BX3,BY3,A)
      CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND,2),BX4,BY4,A)
C     This is a check to see if it is worth drawing the
C     marker or not by checking its surounding box
C
      FONT4 = FONT
      STX=MRKR(MIND+2,1)
      STY=MRKR(MIND+2,2)
      CALL NEWXY(MRKR(MIND+2,1),MRKR(MIND+2,2),NSX1,NSY1,A)
         I = MIND+3
  10     CONTINUE
           IF (MRKI(I).EQ.99) THEN
             IF (MRKR(I,1) .GT. 0.0 ) THEN
                 IMBUFF(12) = INT(MRKR(I,1))
             ELSE IF (MRKR(I,2) .GT. 1.0 ) THEN
                 FONT4 = INT(MRKR(I,2))
             END IF    
             I = I + 1
           ELSE
             CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,A)
             IF(MRKI(I).EQ.1) THEN
                CALL PLTFLW(NSX1,NSY1,NFX1,NFY1,FONT4)
             ELSE IF(MRKI(I) .EQ. 5) THEN
                CTX = MRKR(I,1)
                CTY = MRKR(I,2)
                I = I + 1 
                WRAD = MRKR(I,1)
                CIRDIF = MRKR(I,2)
                I = I + 1
                SANG=RAD(MRKR(I,1))
                EANG=RAD(MRKR(I,2))
                IF(EANG.LT.SANG) THEN
                    CALL RSWAP(SANG,EANG)
                END IF
                TANGLE=ABS(EANG-SANG)
                IF ( EANG .LT. SANG ) TANGLE=PI(2.0)-ANGLE
C               Recalculate the angle to obtain an whole value which
C               will draw a complete arc.
                AP = ABS(EANG-SANG)
                IF ( EANG.LT.SANG ) AP=PI(2.0)-AP
C               convert Radius of ARC to Plotter units
                CALL WTOPVX(WRAD,PRAD)
C               do not attempt to draw extremely small arcs
                IF ( PRAD.LT.LIM ) RETURN
  11            CONTINUE
C               check limits of arc interpolation
                IF (PRAD.GT.2*TOL) THEN
C                  safe to use normal procedure
C                  calculate interpolation angle
                   ALPH=2*ACOS(1-TOL/PRAD)
                ELSE
C                  must set default interpolation angle
C                  circle degenerates into triangle
                   ALPH=2.0
                END IF
C               calculate nearest whole number of intervals needed
                COUNT=1+NINT(AP/ALPH)
                RADS=TANGLE/REAL(COUNT)
C               Obtain the transformation matrice which rotates about
C               the centre of the circle X,Y by the angle RADS.
                CALL ROTP2D(CTX,CTY,RADS,TRS)
C               starting point of the drawing of the circle.
                OX=CTX+WRAD*COS(SANG)
                OY=CTY+WRAD*SIN(SANG)
C               loop for the whole number calculated above.
                DO 12 I1=1,COUNT,1
C                 transform OX to NX by transformation matrice
C                 and also OY to NY.
                  CALL NEWXY(OX,OY,NX,NY,TRS)
C                 Draw between old point and new.
                  CALL NEWXY(OX,OY,NSX1,NSY1,A)
                  CALL NEWXY(NX,NY,NFX1,NFY1,A)
                  CALL PLTFLW(NSX1,NSY1,NFX1,NFY1,FONT4)
C                 old now new point on the circumference
                  OX=NX
                  OY=NY
 12             CONTINUE
                IF (CIRDIF.GT.0.0.AND.WRAD-CIRDIF.GT.0.0) THEN
                    WRAD= WRAD - CIRDIF
C                   convert Radius of ARC to Plotter units
                    CALL WTOPVX(WRAD,PRAD)
                    GOTO 11
                END IF
                I = I + MRKI(I)+1
             END IF
                NSX1=NFX1
                NSY1=NFY1
           END IF

           I = I + 1
           IF (I.LE. MIND+MRKI(MIND)) GOTO 10
      END
 
 
      SUBROUTINE PLTS00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine PLTS00 is the mediator for the
C2    PLOT SCREEN (ie screen dump) function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,PLTS01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the PLOT DRAWING routine
      CALL PLTS01()
C     ensure option for trim arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTS01()
C     ===================
CAPOLLO
      include 'include/pendat.inc'
C
      INTEGER*2 TT
      INTEGER*4 NLEN1,ST,NLEN
      CHARACTER*100 CMD1
      EXTERNAL SHELL,DPRMXP,NLEN1,WAIT,NLEN
C
      CALL DPRMXP(81,PLTFIL)
      IF ( NLEN(PLTFIL).EQ.0 ) RETURN
C
C     release the display for popping window
      CALL RELDIS(TT)
C
C     Move the cursor away from the drawing areas
      CALL SHELLP('xdmc (909,13)dr',ST)
C
      CMD1 ='xdmc cpo /com/cpscr '//PLTFIL(1:NLEN1(PLTFIL))//' -inv'
C
      CALL SHELLP(CMD1,ST)
C
      CALL TIMEWAIT(5.0)
C
C     re-aquire the display by the number of times
      CALL ACRDIS(TT)
C
C
CAPOLLO
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTS02()
C     ===================
CAPOLLO
      include 'include/pendat.inc'
      include 'include/library.inc'
C
      INTEGER*2 I2,TT
      INTEGER*4 NLEN,ST,NLEN1
      CHARACTER*100 CMD1
      LOGICAL UNOB,EX
      EXTERNAL SHELL,DPRMXP,NLEN,WAIT,DEPRNT,NLEN1
C
C     Not used now
C      PLTFIL=LIBRARY(1:NLEN1(LIBRARY))//'/screen/'//
C      + PRODUCT(1:NLEN1(PRODUCT))//'.screen'
C
      ST=0
 5    CONTINUE
      WRITE(PLTFIL,FMT='(A,I2.2)') 
     +      LIBRARY(1:NLEN1(LIBRARY))//'/screen/epson',ST
      INQUIRE(FILE=PLTFIL,EXIST=EX)
C
      IF ( EX ) THEN
         ST=ST+1
         IF ( ST .GT. 99 ) THEN
            CALL DEPRNT(423)
            RETURN
         END IF
         GOTO 5
      END IF
C
C     release the display for popping window
      CALL RELDIS(TT)
C
C     Move the cursor away from the drawing areas
      CALL SHELLP('xdmc (909,13)dr',ST)
C
      CMD1 ='xdmc cpo /com/cpscr '//PLTFIL(1:NLEN1(PLTFIL))//' -inv'
C
      CALL SHELLP(CMD1,ST)
C
      CALL TIMEWAIT(5.0)
C
C     re-aquire the display by the number of times
      CALL ACRDIS(TT)
C
CAPOLLO
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTSW0()
C     ===================
C
C1    no arguments required
C
C2    Subroutine PLTD00 is the mediator for the
C2    PLOT DRAWING function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/pendat.inc'
      include 'include/menun.inc'
      include 'include/wtov.inc'
C
      LOGICAL OK
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,PLTD02
C
      TMEN=MEN
      TCELL=CELLN
 
C     enter the TRIM ARC routine
      PLOWIN=.FALSE.
      XP1=WPXMIN
      YP1=WPYMIN
      XP2=WPXMAX
      YP2=WPYMAX
      CALL PLTD02(.TRUE.)
 
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLTW00()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/pendat.inc'
      include 'include/wtov.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,MNLPW1,PLTD02
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize PLOT WINDOW option menu
      CALL MNLPW1()
C
      PLOWIN=.TRUE.
C     enter the PLOT routine
      XP1=WXMIN
      YP1=WYMIN
      XP2=WXMAX
      YP2=WYMAX
      CALL PLTD02(.FALSE.)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLVIEW(XMIN,YMIN,XMAX,YMAX)
C     ======================================
C1                       R,   R,   R,   R
C1                       I,   I,   I,   I
C
C2        Subroutine PLVIEW set the window (viewport) on the
C2        device , here it is a plotter
C
      include  'include/pendat.inc'
C
      REAL  XMIN, XMAX, YMIN, YMAX
      EXTERNAL SETW2P,BELL
C
C     Check that limits are in the right order
      IF(XMIN.GE.XMAX.OR.YMIN.GE.YMAX) THEN
         CALL BELL
         RETURN
      END IF
C
      PVXMIN=XMIN
      PVXMAX=XMAX
      PVYMIN=YMIN
      PVYMAX=YMAX
C
      CALL SETW2P()
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PPTS(POINT,USET,M,OK)
C     ================================
C1                      I2
C1                      I
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
      REAL M(3,3),XP,YP,XP1,YP1
      REAL X(5),Y(5)
      INTEGER*2 POINT
      LOGICAL OK,USET,VISBAS
C
      VISBAS=.FALSE.
 10   CONTINUE
C
      IF ( USET ) THEN
         CALL NEWXY(RDBUFF(1),RDBUFF(2),XP,YP,M)
      ELSE
         XP=RDBUFF(1)
         YP=RDBUFF(2)
      END IF
      CALL DCROSS(XP,YP)
      CALL WO2SC(XP,YP,XP1,YP1)
      CALL WRKHIT(XP1,YP1,OK)
C     lock visability
      IF(OK)  VISBAS=.TRUE.
C
      IF ( POINT .NE. 0 ) THEN
         CALL DBR500(POINT,OK)
         IF ( .NOT. OK ) RETURN
         POINT=IDBUFF(3)
         GOTO 10
      END IF
C     set visibility flag
      CALL SETVIS(VISBAS)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PTOWVX(W,V)
C     ======================
C
C1    vartype           R,R
C1    iostatus          I O
C
      REAL ZERO,W,V,DUM1
      INTRINSIC ABS
      EXTERNAL WO2PL
C
      CALL PL2WO(0.0,0.0,ZERO,DUM1)
      CALL PL2WO(W,0.0,V,DUM1)
C
      V=ABS(V-ZERO)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PWORLD(XMIN,YMIN,XMAX,YMAX)
C     ======================================
C1                        R,   R,   R,   R
C1                        I,   I,   I,   I
C2       Subroutine WORLD sets the world coordinates window for
C2       the transformation
C
      include  'include/pendat.inc'
C
      REAL XMIN,XMAX,YMIN,YMAX
      EXTERNAL SETW2P,BELL
C
C     Check that window limits are in right order
      IF(XMIN.GE.XMAX.OR.YMIN.GE.YMAX) THEN
         CALL BELL
         RETURN
      END IF
C
C     Transfer to world limits
      PWXMIN=XMIN
      PWXMAX=XMAX
      PWYMIN=YMIN
      PWYMAX=YMAX
C
C     obtain new transformation matrices
      CALL SETW2P()
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SCAN()
C     ==================
C1    no arguments needed
C2                  SUBROUTINE SCAN
C2     SCAN for drawing database extents and set extents of
C2     coordinate points.
C
      include 'include/wtov.inc'
      include 'include/pendat.inc'
C
C     The Drawing extents are set by WPxMIN,WPYMIN,WPXMAX,WPYMAX
C     which are held in Common Block WTOV.INC
C     Set the paper sheet extents , in world coordinates , to the
C     plotter extents .
C
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SETLAY(ALL,QUIT)
C     ===========================
C1      no arguments needed
C2                  SUBROUTINE SETLAY
C2     PROMPT FOR LAYERS USED WITH PENS CHOSEN AND SET ARRAY
C2     FOR I=0,255 ,  LAYERN(I) HOLDS ASSOCIATED PEN NUMBER
C
C
      include 'include/vntable.inc'
      include 'include/pendat.inc'
      include 'include/masti.inc'
C
      CHARACTER*80 OLIN,LINE*80
      INTEGER*4 I,J,DATA(1:256),NDATA,NLEN1,NLEN
      LOGICAL ALL,QUIT
      EXTERNAL STRNUM,NLEN,NLEN1,CPRMXP,DEPRNT,DCPRNT
C
C     set layers to negative so that they can be
C     check after setting of pens and layers is done
      QUIT=.TRUE.
C     set pen for layer negative
C     to indicate it has not benn set yet.
      DO 41 I=0,255
         LAYERN(I)=-1
 41   CONTINUE
C
      DO 11 I=1,MAXPEN
         IF ( PENN(I) ) THEN
            IF ( .NOT.ALL ) THEN
 100           CONTINUE
              WRITE(OLIN,'(A,I3,A3)')DICT01(530)(1:NLEN1(DICT01(530)))
     +         ,I,' : '
C     +         'Which layers to be plotted with pen ',I,' : '
               CALL CPRMXP(OLIN,LINE)
               IF ( NLEN(LINE) .EQ. 0 ) THEN
                  CALL DEPRNT(134)
                  RETURN
               END IF
               CALL STRNUM(LINE,DATA,NDATA)
               DO 21 J=1,NDATA
                  IF ( DATA(J).LT.0.OR.DATA(J).GT.255 ) THEN
                     CALL DEPRNT(134)
                     GOTO 100
                  ELSE
                     LAYERN(DATA(J))=I
                  END IF
 21            CONTINUE
            ELSE
               DO 31 J=0,255
                  IF ( VLAYER(J) ) LAYERN(J)=I
 31            CONTINUE
            END IF
         END IF
 11   CONTINUE
C
      DO 51 J=0,255
         IF ( VLAYER(J).AND.LAYERN(J).LT.0 ) THEN
            WRITE(OLIN,'(A,I4,A3)')DICT01(531)(1:NLEN1(DICT01(531)))
     +      ,J,' : '
C     +      'Which pen to be used with layer    ',J,' : '
 61         CONTINUE
            CALL CPRMXP(OLIN,LINE)
            IF ( NLEN(LINE) .EQ. 0 ) THEN
               RETURN
            END IF
            CALL STRNUM(LINE,DATA,NDATA)
            IF ( NDATA .NE. 1 ) THEN
               CALL DCPRNT(294)
               GOTO 61
            ELSE IF ( DATA(1).LT.MINPEN .OR.
     +                DATA(1).GT.MAXPEN  )    THEN
C              Pen was not assign when asked therefore not going to
C              to use it
               CALL DEPRNT(295)
               CALL DCPRNT(296)
               GOTO 61
            ELSE IF ( .NOT.PENN(DATA(1)) ) THEN
C              Pen was not assign when asked therefore not going to
C              to use it
               CALL DCPRNT(90)
               CALL DCPRNT(298)
               GOTO 61
            ELSE
C              Valid pen stored pointer in layer file
               LAYERN(J)=DATA(1)
            END IF
         END IF
 51   CONTINUE
C
      QUIT=.FALSE.
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SETPAP(OVROT,TXP1,TYP1,TXP2,TYP2,OK)
C     ======================================
C1                        I4  I4   I4   I4  L
C1                         O   O    O    O  O
C2                  Subroutine SETPAP
C2     Scan for drawing database extents and set extents of
C2     coordinate points.
C
C
      include 'include/pendat.inc'
      include 'include/params.inc'
      include 'include/wtov.inc'
      include 'include/ndata.inc'
C
      INTEGER*4 TXP1,TYP1,TXP2,TYP2,I,TSIZE,POINT,PSIZE
      INTEGER INDEX
      CHARACTER*10 ANS*1,OLIN*40,CSCALE
      LOGICAL OK,PLROT,OVROT
      REAL      RATIO,TRAT,PX1,PY1,PX2,PY2,PI,TTX,TTY,
     +          AX,AY,SCALE,A(3,3),B(3,3),OFFSET,OFFSTX,OFFSTY,
     +          OFSTX1,OFSTY1,PXP1,PXP2,PYP1,PYP2
      INTRINSIC ABS,INDEX
      DOUBLE PRECISION DSCALE
      EXTERNAL PLVIEW,PWORLD,PL2WO,ROTP2D,TRAN2D,MULT3M,I3M
C
C     set plotter max extents according to plotter
C     model held in plomod.
      TSIZE=INDEX('A0 A1 A2 A3 A4 ',DRWSHT(:2))
      IF ( TSIZE.GT.0 ) THEN
        TSIZE=1+((TSIZE-1)/3)
        SIZE =INDEX('A0 A1 A2 A3 A4 ',PLTSHT(:2))
        SIZE =1+((SIZE-1)/3)
      ELSE
C       no an A series piece of paper.
        SIZE=1
C       set the paper size for the drawing.
        TSIZE=0
        I=0
 50     TSIZE=MOD(TSIZE,5)+1
        I=I+1
C       find the pointer to current sheet size.
        OK=I.LT.6
        IF (PAPLST(TSIZE).NE.DRWSHT(1:2).AND.OK) GOTO 50
        IF (.NOT.OK ) THEN
          IF ( DRWSHT(1:2).EQ.'AA') THEN
            PAPLIM(SIZE,3)=DRWSIZ(1)
            PAPLIM(SIZE,4)=DRWSIZ(2)
            TSIZE=1
          ELSE
            RETURN
          ENDIF
        ELSE
          PAPLIM(SIZE,3)=PAPSIZ(1,TSIZE)*FACT
          PAPLIM(SIZE,4)=PAPSIZ(2,TSIZE)*FACT
          TSIZE=1
        ENDIF
      END IF
C
      OK=.TRUE.
C
C           1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18
      GOTO (1,10,1,10,1,3,10,2,1,10,10,10,10,10,2,10,10,1) PLOMOD
C
 1    CONTINUE
      PSIZE=4
      GOTO 8
 2    CONTINUE
      PSIZE=1
      GOTO 8
 3    CONTINUE
      PSIZE=2
      GOTO 8
C
 8    CONTINUE
 
      IF ( SIZE.LT.PSIZE ) THEN
C        check to see if the paper would fit on the plotter.
         CALL DEPRNT(299)
         OK=.FALSE.
         RETURN
      END IF
C
      GOTO 10
 
 10   CONTINUE
C
      PLXMIN=PAPLIM(SIZE,1)
      PLYMIN=PAPLIM(SIZE,2)
      PLXMAX=PAPLIM(SIZE,3)
      PLYMAX=PAPLIM(SIZE,4)
C
      IF (OVROT) THEN
        PLROT =.FALSE.
        ROTATE=.FALSE.
      ELSE
        PLROT=(PLYMAX-PLYMIN)/(PLXMAX-PLXMIN).GT.1.0
C
        ROTATE=(YP2-YP1)/(XP2-XP1).GT.1.0
      END IF

      IF (PLOWIN) THEN
C     Save window clip limits ?
          PXP1 = XP1
          PXP2 = XP2
          PYP1 = YP1
          PYP2 = YP2
      ENDIF

C
C      WRITE(10,*) 'SETPAP:',OVROT,PLROT,ROTATE
      IF ( (.NOT.PLROT.AND..NOT.ROTATE) .OR.
     +       (PLROT.AND.ROTATE) ) THEN
         ROTATE=.FALSE.
         TEXANG=  0.0
      ELSE IF ( PLROT .AND. .NOT. ROTATE ) THEN
C        The paper in held long side Y-axis
C        But the orientation of the plot is long edge
C        on the X-axis.
C        Therefore have to rotate plot round to fit
         ROTATE=.TRUE.
C        Text rotated by 90 degrees anti-clockwise.
         TEXANG= 90.0
         CALL ROTP2D(XP1,YP1,PI(0.5),A)
         CALL TRAN2D(YP2-YP1,0.0,B)
         CALL MULT3M(A,B,ROTARY)
         TTX=XP1+(YP2-YP1)
         TTY=YP1+(XP2-XP1)
         XP2=TTX
         YP2=TTY              
      ELSE IF ( .NOT.PLROT .AND. ROTATE ) THEN
C        The paper in held long side X-axis
C        But the orientation of the plot is long edge
C        on the Y-axis.
C        Therefore have to rotate plot back to fit
         ROTATE=.TRUE.
C        Text rotated by 90 degrees clockwise.
         TEXANG= -90.0
         CALL ROTP2D(XP1,YP1,-PI(0.5),A)
         CALL TRAN2D(0.0,XP2-XP1,B)
         CALL MULT3M(A,B,ROTARY)
         TTX=XP1+(YP2-YP1)
         TTY=YP1+(XP2-XP1)
         XP2=TTX
         YP2=TTY
      END IF
C      WRITE(10,*) 'SETPAP:',ROTATE,TEXANG
C
      IF (PLOMOD.EQ.1) THEN
         IF ( ROTATE ) THEN
            IF ( SIZE-1.EQ.4 ) THEN
               ROTATE=.TRUE.
            END IF
         ELSE
            IF ( SIZE-1.EQ.2 ) THEN
               ROTATE=.TRUE.
            END IF
         END IF
         OFFSTX=642
         OFFSTY=420
         OFSTX1=0
         OFSTY1=OFFSTY
      
         IF ( SIZE-1.EQ.4 ) THEN
            OFFSTX=420
            OFFSTY=345
            OFSTX1=OFFSTX
            OFSTY1=OFFSTY
         END IF
      
         PLXMIN=0-OFFSTX
         PLYMIN=0-OFFSTY
         PX1=PLXMIN
         PY1=PLYMIN
         PLXMAX=PLXMAX
         PLYMAX=PLYMAX
         PX2=PLXMAX+OFSTX1
         PY2=PLYMAX+OFSTY1
      ELSE
         PX1=PLXMIN-(FACT*PLBOR(1))
         PX2=PLXMAX+(FACT*PLBOR(4))
         PY1=PLYMIN-(FACT*PLBOR(2))
         PY2=PLYMAX+(FACT*PLBOR(3))
      ENDIF
C
      CALL PLVIEW(PX1,PY1,PX2,PY2)
      CALL PWORLD(XP1,YP1,XP2,YP2)
C                             
      CALL PL2WO(PLXMIN,PLYMIN,XP1,YP1)
      CALL PL2WO(PLXMAX,PLYMAX,XP2,YP2)
C
      CALL PLVIEW(PLXMIN,PLYMIN,PLXMAX,PLYMAX)
      CALL PWORLD(XP1,YP1,XP2,YP2)
C
      IF (      TEXANG.EQ.90.0  ) THEN
         PWXMIN=WPXMIN+(YP1-WPYMIN)
         PWYMIN=WPXMIN+(WPYMAX-XP2)
         PWYMAX=WPYMAX-(XP1-WPXMIN)
         PWXMAX=WPXMIN+(YP2-WPYMIN)
      ELSE IF ( TEXANG.EQ.-90.0) THEN
         PWXMIN=WPXMIN+(YP1-WPYMIN)
         PWYMIN=WPYMIN+(XP1-WPXMIN)
         PWXMAX=WPXMAX-(YP1-WPYMIN)
         PWYMAX=WPYMIN+(XP2-WPXMIN)
      END IF
C

      IF (PLOWIN) THEN
C     Reset window clip limits ?
          PWXMIN = PXP1
          PWXMAX = PXP2
          PWYMIN = PYP1
          PWYMAX = PYP2
      ENDIF
      PX1=WPXMIN+PLBOR(1)
      PX2=WPXMAX-PLBOR(4)
      PY1=WPYMIN+PLBOR(2)
      PY2=WPYMAX-PLBOR(3)
 
      IF ( ROTATE ) THEN
         CALL MULT3M(ROTARY,PLWVXY,A)
         CALL I3M(B)
         CALL MULT3M(A,B,PLWVXY)
C
      END IF
C
      TXP1=PAPLIM(SIZE,1)
      TYP1=PAPLIM(SIZE,2)
      TXP2=PAPLIM(SIZE,3)
      TYP2=PAPLIM(SIZE,4)
C
      CALL WTOPVX(1.0*PAPTOW,PAPSCL)
      PAPSCL=PAPSCL/FACT
 
 999  CONTINUE
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SETPEN(ALL,QUIT)
C    ============================
C1        no arguments needed
C1
C
C2     PROMPT FOR PEN NUMBERS TO BE USED AND SET ARRAYS i.e.
c2     FOR I=1,8 PENN(I) IS TRUE IF PEN REQUESTED.FALSE IF NOT
C2     FOR I=0,255 LAYERN(I) HOLDS DEFAULT PEN NUMBER 1
C
      include 'include/pendat.inc'
      include 'include/vntable.inc'
C
      CHARACTER*40 LINE*80
      INTEGER*4 I,DATA(1:255),NDATA,NLEN
      LOGICAL OK,ALL,QUIT
      LOGICAL YES,ABORT
      EXTERNAL STRNUM,NLEN,DEPRNT,DPRMXP
C
      QUIT=.FALSE.
      ALL=.FALSE.
C
C     set all pens .false.
      DO 21 I=1,MAXPEN
         PENN(I)=.FALSE.
 21   CONTINUE
C
  1   CONTINUE
      IF ( MAXPEN.GT.1 ) THEN
C        ask user for multi pen plot or not
         CALL CONFIRMABORT(DICT01(291),.FALSE.,YES,ABORT)
         IF ( ABORT ) THEN
C            signal a quit to stop layer setting
             QUIT = .TRUE.
             RETURN 
         ENDIF
C        option for all pens are reveresed
         ALL = .NOT.YES
      ELSE
C         Not possible since only one pen supported in plotter
          ALL=.TRUE.
      END IF
C
      IF ( ALL ) THEN
C        Wants to use one pen only
         PENN(1)=.TRUE.
         RETURN
      END IF
C
C     Ask for pen details
 11   CALL DPRMXP(292,LINE)
      QUIT= NLEN(LINE).EQ.0
      IF ( QUIT ) RETURN
C
      CALL STRNUM(LINE,DATA,NDATA)
      IF ( NDATA.EQ.0 ) THEN
         CALL  DEPRNT(294)
         GOTO 11
      END IF
C
      I=1
 31   CONTINUE
      IF ( DATA(NDATA).GT.MAXPEN
     + .OR.DATA(NDATA).LT.MINPEN ) THEN
         CALL DEPRNT(293)
         GOTO 11
      ELSE
         PENN(DATA(I))=.TRUE.
         I=I+1
         IF ( I .LE. NDATA ) GOTO 31
      END IF
C
      DO 41 I=0,255
         LAYERN(I)=1
 41   CONTINUE
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SETPLT()
C     ===================
 
C     set plotter model here for display purposes
C     normal default is 2 for HP7580B
C
      include  'include/pendat.inc'
      include 'include/lfu.inc'
      include 'include/library.inc'
      include 'include/vntable.inc'
C
      CHARACTER*100 NAME
      CHARACTER*80 ERROR
      INTEGER*4 OUNIT,I,IOSTAT,NLEN,st,LINENO
      LOGICAL OK
      EXTERNAL NLEN
 
CSUN|APOLLO
      NAME='plotter.pcf'
      CALL FPARS(NAME,ST)
CSUN|APOLLO
CIBM
C      NAME='PLDEF.PCF'
C      INQUIRE(FILE=NAME,EXIST=OK)
C      IF(.NOT.OK) NAME=LIBRARY(1:NLEN(LIBRARY))//'\pldef.pcf'
CIBM
CPC386
C      NAME='PLDEF.PCF'
C      INQUIRE(FILE=NAME,EXIST=OK)
C      IF(.NOT.OK) NAME=LIBRARY(1:NLEN(LIBRARY))//'\\pldef.pcf'
CPC386
C
      LINENO = 0
      CALL OPNOFF(NAME,OUNIT,OK)
      IF ( .NOT.OK ) GOTO 100
C
      CURPLT=1
C
      READ(UNIT=OUNIT,FMT=*,ERR=200,END=200) MODELS
      LINENO = LINENO + 1

C     one model.
      DO 10 I=1,MODELS
         READ(UNIT=OUNIT,FMT=*,ERR=200,END=200) MODLST(I)
         LINENO = LINENO + 1
 10   CONTINUE
C     set for benson
C      MODLST(1)=4
C      MODLST(2)=2
C
      DO 20 I=1,MODELS
         READ(UNIT=OUNIT,FMT='(A)',ERR=200,END=200) TYPE(I)
         LINENO = LINENO + 1
 20   CONTINUE
 
C     directory to use.
      DO 30 I=1,MODELS
         READ(UNIT=OUNIT,FMT='(A)',ERR=200,END=200) PLTNAM(I)
         LINENO = LINENO + 1
 30   CONTINUE
 
      CLOSE(UNIT=OUNIT)
      LFU(OUNIT)=.FALSE.
 
      RETURN
C
 100  CONTINUE
      WRITE(ERROR,'(A)') DICT01(839)(1:NLEN(DICT01(838)))
      CALL ERRORDIALOG(ERROR)
      MODELS=0
      RETURN
 200  CONTINUE
      WRITE(ERROR,'(A,I2)') DICT01(838)(1:NLEN(DICT01(838))),LINENO
      CALL EPRINT(ERROR)
      CALL ERRORDIALOG(ERROR)
      MODELS=0
 
      END
C
C     -----------------------------------------------------------
C
 
      SUBROUTINE SETW2P()
C     ===================
C1    no arguments needed
C
C2       Subroutine SETW2P intialises the world to view
C2       coordinates transformation matix PLWVXY
C
      include 'include/pendat.inc'
C
      INTEGER I
      REAL  A(3,3),WRATIO,VRATIO,WVSCAL,WVTRAX,WVTRAY,
     +      B(3,3),C(3,3),VAL
C
      EXTERNAL MULT3M,SCAL2D,TRAN2D,PL2WO
C
C        Aspect ratio of world,screen,viewport systems
C        determine the scaling conditions used
C
      WRATIO=(PWXMAX-PWXMIN)/(PWYMAX-PWYMIN)
      VRATIO=(PVXMAX-PVXMIN)/(PVYMAX-PVYMIN)
C
C        Test for ruling condition
      IF(WRATIO.LT.VRATIO) THEN
         WVSCAL=(PVYMAX-PVYMIN)/(PWYMAX-PWYMIN)
      ELSE
         WVSCAL=(PVXMAX-PVXMIN)/(PWXMAX-PWXMIN)
      END IF
C
C     translate  world window to world origin where
C     -PWXMIN & -PWYMIN are points to go to
      CALL TRAN2D(-PWXMIN,-PWYMIN,A)
C     scale to viewport size
      CALL SCAL2D(WVSCAL,WVSCAL,B)
      CALL MULT3M(A,B,C)
      CALL TRAN2D(PVXMIN,PVYMIN,B)
      CALL MULT3M(C,B,PLWVXY)
C
C        Matrix PLWVXY contains World to Viewport transformation
C        Find inverse of PLWVXY , call it PLVWXY
C        Matrix PLVWXY contains Viewport to World transformation
C
      CALL TRAN2D(-PVXMIN,-PVYMIN,A)
      CALL SCAL2D(1.0/WVSCAL,1.0/WVSCAL,B)
      CALL MULT3M(A,B,C)
      CALL TRAN2D(PWXMIN,PWYMIN,B)
      CALL MULT3M(C,B,PLVWXY)
C
C       Set world window exactly on the plotter window
C2     Note there is a change of VYMAX and VYMIN for
C2     plotters s which have their origin in the top left
C
      CALL PL2WO(PVXMIN,PVYMIN,PWXMIN,PWYMIN)
      CALL PL2WO(PVXMAX,PVYMAX,PWXMAX,PWYMAX)
C
C      CALL WO2PL(PWXMIN,PWYMIN,PVXMIN,PVYMIN)
C      CALL WO2PL(PWXMAX,PWYMAX,PVXMAX,PVYMAX)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SOFARC(TOL,X1,X2,X4,X5,X6)
C     =====================================
C
      include 'include/pendat.inc'
C
      REAL TOL,X1,X2,X4,X5,X6,ALPH,PRAD,
     +     AXP1,AYP1,AP,PI,AXP2,AYP2,DEG,DISTXY,
     1     VX1,VY1,VX2,VY2,XT1,YT1,XT2,YT2
      DOUBLE PRECISION TRS(3,3),NX,NY,DAXP1,DAYP1,RADS
      LOGICAL OK
      INTEGER*4 COUNT,I
C
      INTRINSIC SQRT,DBLE,SIN,COS
      EXTERNAL PI,WTOPVX,DROTP2D,DNEWXY,DDEG,DISTXY
C
C     Find total included angle
      AP=ABS(X6-X5)
C     allow for reverse angle given
      IF ( X6.LT.X5 ) AP=PI(2.0)-AP
C     convert Radius of ARC to Plotter units
      CALL WTOPVX(X4,PRAD)
C     do not attempt to draw extremely small arcs
      IF ( PRAD.LT.LIM ) RETURN
C     check limits of arc interpolation
      IF (PRAD.GT.2*TOL) THEN
C        safe to use normal procedure
C        calculate interpolation angle
         ALPH=2*ACOS(1-TOL/PRAD)
      ELSE
C        must set default interpolation angle
C        circle degenerates into triangle
         ALPH=2.0
      END IF
C     calculate nearest whole number of intervals needed
      COUNT=1+NINT(AP/ALPH)
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc
      RADS=DBLE(DBLE(AP)/DBLE(COUNT))
C     find coords of start position
      AXP1=X1+X4*COS(X5)
      AYP1=X2+X4*SIN(X5)
      CALL WO2PL(AXP1,AYP1,VX1,VY1)
C     find coords of end position
      AXP2=X1+X4*COS(X6)
      AYP2=X2+X4*SIN(X6)
      CALL WO2PL(AXP2,AYP2,VX2,VY2)
C     swap start and end if necessary
      IF ( DISTXY(AXP,AYP,VX2,VY2).LT.
     +     DISTXY(AXP,AYP,VX1,VY1)     ) THEN
         CALL RSWAP(AXP1,AXP2)
         CALL RSWAP(AYP1,AYP2)
         RADS=-RADS
      END IF
C     Obtain the transformation matrice which rotates about
C     the centre of the circle X,Y by the angle RADS
      DAXP1=X1
      DAYP1=X2
      CALL DROTP2(DAXP1,DAYP1,RADS,TRS)
      DAXP1=AXP1
      DAYP1=AYP1
C
C     starting point of the drawing of the circle
C     loop for the whole number calculated above
      DO 100 I=1,COUNT,1
C        transform OX to NX by transformation matrice
C        and also OY to NY
         CALL DNEWXY(DAXP1,DAYP1,NX,NY,TRS)
C        Draw between old point and new
         XT1=DAXP1
         YT1=DAYP1
         XT2=NX
         YT2=NY
C        changed for Gerber
         IF (CLIPIT) THEN
               CALL CLIPP(XT1,YT1,XT2,YT2,OK)
         ELSE
              OK=.TRUE.
         END IF
         IF ( OK ) THEN
            CALL WO2PL(XT1,YT1,VX1,VY1)
            CALL WO2PL(XT2,YT2,VX2,VY2)
C           now we have screen coords VX1,VY1,VX2,VY2
C           must draw line with correct fonting
            CALL OUTLIN(VX1,VY1,VX2,VY2)
         END IF
C        old now new point on the circumference
         DAXP1=NX
         DAYP1=NY
 100  CONTINUE
C
C     update current position globals
      AXP1=DAXP1
      AYP1=DAYP1
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE WO2PL(WX,WY,VX,VY)
C     =============================
C1                      R, R, R, R
C1                      I, I, O, O
C2      Subroutine WO2PL converts WX,WY to plotter coordinates
C
      include  'include/pendat.inc'
C
      REAL  WX,WY,VX,VY
      EXTERNAL NEWXY
C     Using world to plotter transformation PLWVXY
      CALL NEWXY(WX,WY,VX,VY,PLWVXY)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE WTOPVX(W,V)
C     ======================
C
C1    vartype           R,R
C1    iostatus          I O
C
      include 'include/pendat.inc'
C
      REAL ZERO,W,V,DUM1,DUM2
      INTRINSIC ABS
      EXTERNAL WO2PL
C
      IF ( ROTATE ) THEN
         CALL WO2PL(0.0,0.0,ZERO,DUM1)
         CALL WO2PL(W,0.0,V,DUM2)
         V=ABS(DUM2-DUM1)
      ELSE
         CALL WO2PL(0.0,0.0,ZERO,DUM1)
         CALL WO2PL(W,0.0,V,DUM1)
         V=ABS(V-ZERO)
      END IF
C
      END
C
C     ----------------------------------------------------
C
