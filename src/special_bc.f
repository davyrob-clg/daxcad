C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 special.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CPDSSC()
C     SUBROUTINE GETNGR (X,Y,NGR,OK)
C     SUBROUTINE INSECT(FILNM,OK)
C     SUBROUTINE MAJSPC()
C     SUBROUTINE MNISPC()
C     SUBROUTINE NUMSEC( NSECTS )
C     SUBROUTINE OSAMAP(SECTNM,MAPNAM,OSMWIN,ST)
C     SUBROUTINE OSCLRM()
C     SUBROUTINE OSETMM()
C     SUBROUTINE OSLODA()
C     SUBROUTINE OSLODB()
C     SUBROUTINE OSPLOT()
C     SUBROUTINE SPCA00()
C     SUBROUTINE SPCA01()
C     SUBROUTINE SPCA02()
C     SUBROUTINE SPCA03(WINX1,WINY1,WINX2,WINY2,FILNM)
C     SUBROUTINE SPCB00()
C     SUBROUTINE SPCB01()
C     SUBROUTINE SPCB02()
C     SUBROUTINE SPCB03()
C     SUBROUTINE SPCB04(FILNM,OK)
C     SUBROUTINE SPCC00()
C     SUBROUTINE SPCC01()
C     SUBROUTINE SPCC02()
C     SUBROUTINE SPCD00()
C     SUBROUTINE SPCD01()
C     SUBROUTINE SPCE00()
CC
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CPDSSC()
C     ===================
C
C1    no arguments required
C
C2    Subroutine CPDSSC copies the content of the
C2    display file into the scratch file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 I,J,K,ENT,TMIP
      REAL SX,SY
      LOGICAL OK
      EXTERNAL RDISPF,SSFLAG
C
C     initialize dummies
      SX=0.0
      SY=0.0
      J=LDFILE(CVPN)-1
      K=0
C
C     copy content of display file into scratch file
      DO 5 I=1,J
C       read from display file
        CALL RDISPF(I,ENT,TMIP,OK)
C       write to scratch file,with no flagging
        IF (OK) CALL SSFLAG(TMIP,SX,SY,I,K,.FALSE.)
 5    CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE GETNGR (X,Y,NGR,OK)
C
C     calculate the NGR for offset X,Y into currect drawing
C     return OK = false if this is not a OS map
C
      include 'include/section.inc'
      include 'include/params.inc'
 
      REAL X,Y,NORTH,EAST
      CHARACTER *(*) NGR
      LOGICAL OK
 
      OK=.FALSE.
 
CSUN
      EAST = OSGXY(1,0)
      NORTH = OSGXY(2,0)
C
      IF (EAST.EQ.0.0 .OR. NORTH.EQ.0.0) THEN
CC     Not a map sheet (except for SV00SE which is in the sea somewhere
CC     off the Isles of Scilly and therefore probably does not exist)
        OK= .FALSE.
      ELSE
CC        WRITE (*,*) 'GETNGR'
CC        WRITE (*,*) 'Sheet Origin =  EAST=', EAST,' NORTH = ',NORTH
CC        WRITE (*,*) 'Offset       =     X=', X,   '     Y = ',Y
CC
CC       convert true offset to metres
        EAST=(EAST + X) * (DBUFAC / 100)
        NORTH=(NORTH + Y) * (DBUFAC / 100)
C
CC        WRITE (*,*) 'True Point =  EAST=', EAST,' NORTH = ',NORTH
C
        CALL GENNGR(EAST,NORTH,NGR)
        OK = (NGR .NE. ' ')
      END IF
CSUN
      END
 
      SUBROUTINE INSECT(FILNM,OK)
C     ===========================
C
C1    vartype            C*(*)   L
C1    iostatus             I     O
C
C2    Subroutine INSECT takes the data currently
C2    identified in the SECTION file and inserts it
C2    in the database.Returns Ok if op complete
C
      include 'include/masti.inc'
      include 'include/props.inc'
      include 'include/filunit.inc'
      include 'include/lfu.inc'
      include 'include/section.inc'
      include 'include/movdat.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,TTPRP,TTPCP
      INTEGER*4 FN,TSEC,I,TYPE
      LOGICAL OK,OPTION,QUIT
      CHARACTER*80 FILNM
      EXTERNAL INSP04,OPENNM,INSP05,MOVMAS,INSP07
C
C     clear movcopy flags
      CALL CLROPF()
C     set FN for SECTION
      TYPE=2
      FN=4
C     save the current record pointers
      TTMIP=NMIPOS
      TTPDP=NPDPOS
      TTTXP=NTXPOS
      TTRLP=NRLPOS
      TTPRP=NPRPOS
      TTPCP=NPCPOS
C     save display file state
      TLDFIL=LDFILE(CVPN)
C
C     Try loading the SECTION
C
C     find position for SECTION
C      CALL INSP04(OPTION,QUIT)
      IF (OPTION.OR.QUIT) RETURN
C
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
      IF ( .NOT. OK ) THEN
C        something wrong with file
         WRITE(UNIT=10,FMT=*)'[INSECT] Cannot open section file:',filnm
         RETURN
      END IF
C
C     dump section data
      TSEC=SECNUM
      SECNUM=16
C     go store the data
      CALL INSP05(FILNM,'SECT',TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
C     ensure dummy section data ignored
      SECTOK(SECNUM)=.FALSE.
C     recover section number in use
      SECNUM=TSEC
C
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
C     set origin for section data
      REFDAT(4,1)=WWGXY(1,SECNUM)
      REFDAT(4,2)=WWGXY(2,SECNUM)
      OPFLAG(4)=.TRUE.
      WRITE(10,*)' ***********************'
      WRITE(10,*)'   TRANSFORMATION DATA'
      WRITE(10,*)(REFDAT(1,I),I=1,3)
      WRITE(10,*)(REFDAT(2,I),I=1,3)
      WRITE(10,*)(REFDAT(3,I),I=1,3)
      WRITE(10,*)(REFDAT(4,I),I=1,3)
C     transform data including database units,and scales
      CALL INSP07(FN,TTMIP,TYPE)
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[INSECT] Error in section file:',filnm
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE MAJSPC()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the SPECIAL mode
C2    of operation is selected from the master menu.
C2    controls operation of the SPECIAL function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      EXTERNAL MNISPC,GTHFMC,SPCS00,SPCT00,SPCP00,SPCD00
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,GTCLRM,UNFLAG
C
C     Now activate the SPECIAL major option menu
      CALL MNISPC()
C
C     clear the error and prompt windows
      CALL CLRPEW
C
 10   CONTINUE
C     Read a cursor hit to select CHANGE mode
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
C        ensure grouped entities can be returned
C        as individual entities
         GSSTAT=1
         IF (CCMD.EQ.'A') THEN
C           Special function 1
            CALL SPCA00()
C           clear token to avoid loop
            CCMD=' '
            MEN=0
         ELSE IF (CCMD.EQ.'B') THEN
C           Special function 2
            CALL SPCB00()
C           clear token to avoid loop
            CCMD=' '
            MEN=0
         ELSE IF (CCMD.EQ.'C') THEN
C           Special function 3
            CALL SPCC00()
C           clear token to avoid loop
            CCMD=' '
            MEN=0
         ELSE IF (CCMD.EQ.'D') THEN
C           Special function 4
            CALL SPCD00()
C           clear token to avoid loop
            CCMD=' '
            MEN=0
         ELSE IF (CCMD.EQ.'E') THEN
C           Special function 5
            CALL SPCE00()
C           clear token to avoid loop
            CCMD=' '
            MEN=0
         ELSE IF (CCMD.EQ.'F') THEN
C           Special function 6
            CALL SPCF00()
C           clear token to avoid loop
         ELSE IF (CCMD.EQ.'G') THEN
C           Special function 7
            write(10,*) '[calling] spcg00'
            CALL SPCG00()
            write(10,*) '[from] spcg00'
C           clear token to avoid loop
         ELSE
C           unrecognized delete option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         IF (CCMD.EQ.'q'.OR.CCMD.EQ.'Q') GOTO 99
         GOTO 10
      ELSE
         CALL DEPRNT(24)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
      CALL CLROPF()
C
      RETURN
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNISPC()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major SPECIAL options.
C2
      EXTERNAL GTDMEN,GTCLRM,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the SPECIAL major option.
      CALL GTDMHD(34,2)
C
C     Load the nouns into major option menu.
C     Enter the SPECIAL FUNC 1 token 'A'
      CALL GTDMEN(456,2)
C     Enter the SPECIAL FUNC 1 token 'B'
      CALL GTDMEN(457,2)
C     Enter the SPECIAL FUNC 1 token 'C'
      CALL GTDMEN(458,2)
C     Enter the SPECIAL FUNC 1 token 'D'
      CALL GTDMEN(459,2)
C     Enter the SPECIAL FUNC 1 token 'E'
      CALL GTDMEN(460,2)
C     Enter the SPECIAL FUNC 1 token 'F'
      CALL GTDMEN(467,2)
C
C     Enter the SPECIAL FUNC 1 token 'F'
      CALL GTDMEN(468,2)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE NUMSEC( NSECTS )
C
C     calculate the number of sections
C
      include 'include/section.inc'
 
      INTEGER*4 NSECTS,I
 
      NSECTS=0
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
C          count sections
           NSECTS=NSECTS+1
        END IF
  10  CONTINUE
 
      END
      SUBROUTINE OSAMAP(SECTNM,MAPNAM,OSMWIN,ST)
C     ==========================================
C
C1    vartype             C*80   C*80 I*4(4) I*4
C1    iostatus             I       I    I     O
C
C2    Subroutine OSAMAP adds a map section
C2    to the section control block.
C2    SECTNM passes the DRAWING section name
C2    MAPNAM passes the BITMAP name
C2    OSMWIN passes the origin and extents
C2    of the section in OS grid coordinates.
C2    ST returns completion status,0 for success.
C
      include 'include/section.inc'
C
      INTEGER*4 I,J,ST
      REAL OSMWIN(4)
      CHARACTER*80 SECTNM,MAPNAM
C
C     ensure correct names are used
      CALL SUFFIX(SECTNM,'.sec')
C SPB - 031194 - Don't force an extension on the filename
C      CALL SUFFIX(MAPNAM,'.im')
C     search for free entry
      I=0
 10   CONTINUE
      I=I+1
      IF (I.LE.16 .AND. .NOT. SECTOK(I)) THEN
C       free entry use it
        SECNAM(1,I)=SECTNM
        SECNAM(2,I)=MAPNAM
        DO 20 J=1,4
          OSGXY(J,I)=OSMWIN(J)
 20     CONTINUE
        SECTOK(I)=.TRUE.
        ST=0
      ELSE IF (I.LT.16) THEN
C       go try next one
        GOTO 10
      ELSE
C       ran out of map storage space
        ST=1
      END IF
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE OSCLRM()
C     ===================
C
C2    Subroutine OSCLRM clears all section
C2    flags for use with OS grid and mapping
C2    operations.
C
      include 'include/section.inc'
C
      INTEGER*4 I
C
      DO 10 I=0,16
        SECTOK(I)=.FALSE.
        SECNAM(1,I)=' '
        SECNAM(2,I)=' '
 10   CONTINUE
C
C     I guess if the sections are gone then so should the backcloths. GCU
      CALL FREBAK()
 
      END
C
C     --------------------------------------------------
C
      SUBROUTINE OSETMM()
C     ===================
C
C2    Subroutine OSETMM sets the main map limits and
C2    flags for use with OS grid and mapping
C2    operations.
C2    Uses the previously stored map sections,and
C2    sets clipping limits for each of them.
C
      include 'include/params.inc'
      include 'include/ndata.inc'
      include 'include/wtov.inc'
      include 'include/section.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 I,J,NSECTS,NLEN1
      REAL X,ODRWSC
      CHARACTER*80 TEMP,INPUT
      EXTERNAL NLEN1
C
      WRITE(10,*)' ======== ENTERING OSETMM ======='
C     set limits to wild values
      OSGXY(1,0)=1.0E38
      OSGXY(2,0)=1.0E38
      OSGXY(3,0)=-1.0E38
      OSGXY(4,0)=-1.0E38
C
C     set main map limits to min-max of section
C     elements to be included.
      NSECTS=0
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
C          count sections
           NSECTS=NSECTS+1
C          set minimum xy
           OSGXY(1,0)=MIN(OSGXY(1,0),OSGXY(1,I))
           OSGXY(2,0)=MIN(OSGXY(2,0),OSGXY(2,I))
C          set maximum xy
           OSGXY(3,0)=MAX(OSGXY(3,0),OSGXY(3,I))
           OSGXY(4,0)=MAX(OSGXY(4,0),OSGXY(4,I))
        END IF
 10   CONTINUE
C
C     main map limits in OS coords now in (OSGXY(1,I),I=1,4)
C
      IF (NSECTS.GT.0) THEN
C        find world limits to apply in main drawing
         WWGXY(1,0)=0.0
         WWGXY(2,0)=0.0
         WWGXY(3,0)=OSGXY(3,0)-OSGXY(1,0)
         WWGXY(4,0)=OSGXY(4,0)-OSGXY(2,0)
      END IF
C
C
C     find split lines in local world coords
C     for map segments,and store for later use
      WRITE(10,*)' Local coordinates'
      DO 20 I=1,16
        IF (SECTOK(I)) THEN
           WWGXY(1,I)=OSGXY(1,I)-OSGXY(1,0)
           WWGXY(2,I)=OSGXY(2,I)-OSGXY(2,0)
           WWGXY(3,I)=OSGXY(3,I)-OSGXY(1,0)
           WWGXY(4,I)=OSGXY(4,I)-OSGXY(2,0)
           SECNUM=I
           WRITE(10,*)'SECTION NAME:',SECNAM(1,I)
           WRITE(10,*)'BITMAP NAME:',SECNAM(2,I)
           WRITE(10,FMT=*) (WWGXY(J,I),J=1,4)
        END IF
 20   CONTINUE
C
C     set temporary paper limits,for use with
C     combined drawing
      IF (NSECTS.GT.0) THEN
         DRWSIZ(1)=WWGXY(3,0)*DBUFAC/PAPFAC*SECSCL
         DRWSIZ(2)=WWGXY(4,0)*DBUFAC/PAPFAC*SECSCL
C        set mapping of paper on world
         WPXMIN=WWGXY(1,0)
         WPYMIN=WWGXY(2,0)
         WPXMAX=WWGXY(3,0)
         WPYMAX=WWGXY(4,0)
         WPORGX=WPXMIN
         WPORGY=WPYMIN
         SHTNAM='Merged'
         SHTKEY='AA'
         SHTSIZ(1)=DRWSIZ(1)
         SHTSIZ(2)=DRWSIZ(2)
         DRWSHT='AA'
C        remember cuerrent scale
         ODRWSC = DRWSCL
C        ensure drawing scale matches sections
         DRWSCL=SECSCL
C        must set PAPTOW to match the drawing scale
C        set PAPTOW ratio
         PAPTOW=1/DRWSCL*PAPFAC/DBUFAC
         IF (DRWSCL.LE.1) THEN
C           set new character string for scale
            X=1/DRWSCL
            WRITE(UNIT=TEMP,FMT='(F10.3)') X
            INPUT='1/'//TEMP(1:NLEN1(TEMP))
         ELSE
            X=DRWSCL
            WRITE(UNIT=TEMP,FMT='(F10.3)') X
            INPUT=TEMP(1:NLEN1(TEMP))//'/1'
         END IF
         CALL CRUNCH(INPUT)
         DRGSCL=INPUT(1:NLEN1(INPUT))
C
         IF (ODRWSC.NE.DRWSCL) THEN
           INPUT=DICT01(492)(1:NLEN1(DICT01(492)))//DRGSCL
           CALL CPRINT(INPUT)
         END IF
      END IF
C
C     reset view to full extents
      CALL ZOMEXT()
C
C      WRITE(10,*)' NSECTS=',NSECTS
C      WRITE(10,*)' SHEET KEY=',SHTKEY,' NAME=',SHTNAM
C      WRITE(10,*)' SHEET SIZE=',SHTSIZ(1),' ',SHTSIZ(2)
C      WRITE(10,*)' SHEET SCALE=',SECSCL
C      WRITE(10,*)' DRAWING SCALE=',DRWSCL
C      WRITE(10,*)' DRAWING SIZE=',DRWSIZ(1),' ',DRWSIZ(2)
C      WRITE(10,*)' DBUFAC=',DBUFAC,' PAPFAC=',PAPFAC
C      WRITE(10,*)' ======== LEAVING OSETMM ======='
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE OSLODA()
C     ===================
C
C2    Subroutine OSLODA loads all required bitmaps
C2    Uses the previously stored map sections
C
      include 'include/section.inc'
      include 'include/macro.inc'
C
      CHARACTER *80 NAME
      INTEGER*4 I,J
C
CSUN
CC     keep count of backcloth sections
      J=0
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
CC         load the bitmap sections
CC         pass bitmap name,and local origin
          NAME=PATHN(6)(:NLEN1(PATHN(6)))//SECNAM(2,I)
          CALL CRUNCH(NAME)
C	  pass full pathname instead of localname ja
C          CALL DEFBAK(SECNAM(2,I),WWGXY(1,I),WWGXY(2,I))
          CALL DEFBAK(NAME,WWGXY(1,I),WWGXY(2,I))
        END IF
        J=J+1
 10   CONTINUE
CC
CC     go get all bitmap sections
      IF (J.GT.0) CALL LDBAK()
CSUN
      END
C
C     --------------------------------------------------
C
      SUBROUTINE OSLODB()
C     ===================
C
C2    Subroutine OSLODB loads all required SECTIONS
C2    Uses the previously stored map sections
C
      include 'include/section.inc'
C
      INTEGER*4 I,J
      LOGICAL OK
      CHARACTER*80 NAME
C
C     keep count of sections
      J=0
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
C         load the section
C         pass section name
          NAME=SECNAM(1,I)
          WRITE(10,*)'[OSLODB] LOADING:',NAME
C         set current section number
          SECNUM=I
          CALL INSECT(NAME,OK)
        END IF
 10   CONTINUE
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE OSPLOT()
C     ===================
      include 'include/section.inc'
      include 'include/pendat.inc'
      include 'include/macro.inc'
C
      CHARACTER *80 NAME
      CHARACTER *132 OLINE
      REAL X,Y,SCALE
      LOGICAL OK
      INTEGER*4 I
      EXTERNAL NLEN2,NLEN1
C
CSUN
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
CC  determine raster scale to use for the plot
          CALL PLTBAK(SECNAM(2,I),SCALE,OK)
CC  OK is true if backcloth is to be shown.
	  IF (OK) THEN
CC  convert world offsets to mm
            X = (WWGXY(1,I)-PWXMIN) * PLWVXY(1,1)
            Y = (WWGXY(2,I)-PWYMIN) * PLWVXY(1,1)
CC  subtract viewport offset
	    X = X - VPORGX
	    Y = Y - VPORGY
CC  use long name
            NAME=PATHN(6)(:NLEN1(PATHN(6)))//SECNAM(2,I)
            CALL CRUNCH(NAME)
CC  get full hierarcal name for plot file
	    CALL FHNAME(NAME)
            WRITE(OLINE,'(A,A,F15.5,A,F15.5,A,F15.5)')
     +         NAME(1:NLEN2(NAME)),',',X,',',Y,',',SCALE
CC call plotter output routine
            CALL OUTOLL(.FALSE.,OLINE)
	  END IF
        END IF
 10   CONTINUE

CSUN
      END
 
 
      SUBROUTINE SPCA00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the SPECIAL FUNC 1 function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SPCA01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SPECIAL FUNC 1 routine
      CALL SPCA01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCA01()
C     ===================
C
      EXTERNAL GTCLRM,SPCA02,MNLSP1
C
C     initialize option menu for FUNC 1
      CALL GTCLRM(3)
C     load menu with FUNC 1 options
C      CALL MNLSP1()
C     go do the function
      CALL SPCA02()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCA02()
C     ===================
C
      include 'include/section.inc'
C
      REAL TW1X1,TW1Y1,TW1X2,TW1Y2
      INTEGER*4 I
      CHARACTER*80 FILNM
C
C     divide work space into defined sections
C     maximum of 16 sections catered for
C     **********************************
C     *       *        *       *       *
C     *       *        *       *       *
C     **********************************
C     *       *        *       *       *
C     *       *        *       *       *
C     **********************************
C     *       *        *       *       *
C     *       *        *       *       *
C     **********************************
C     *       *        *       *       *
C     *       *        *       *       *
C     **********************************
C
C     generate section outputs from
C     section data block.
      DO 10 I=1,16
        IF (SECTOK(I)) THEN
C          set current section number
           SECNUM=I
C          get clipping limits
           TW1X1=WWGXY(1,I)
           TW1Y1=WWGXY(2,I)
           TW1X2=WWGXY(3,I)
           TW1Y2=WWGXY(4,I)
C          set limits to first section
           CALL WORLD(TW1X1,TW1Y1,TW1X2,TW1Y2)
C          get section name
           FILNM=SECNAM(1,I)
      WRITE(10,*) '[SPCA02] FILNM= ',FILNM
C          create SECTION file
           CALL SPCA03(TW1X1,TW1Y1,TW1X2,TW1Y2,FILNM)
        END IF
 10   CONTINUE
C     reset back to full view
      CALL ZOMEXT()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCA03(WINX1,WINY1,WINX2,WINY2,FILNM)
C     ================================================
C
C1    vartype            R      R     R     R    C*(*)
C1    iostatus           I      I     I     I      I
C
C2    Subroutine SPCA03 creates a section file of
C2    the data visible in the window defined by
C2    WINX1,WINY1,WINX2,WINY2.
C
      include 'include/masti.inc'
      include 'include/movdat.inc'
      include 'include/wtov.inc'
C
      INTEGER*4 I
      REAL WINX1,WINY1,WINX2,WINY2
      REAL X1,Y1,X2,Y2
      CHARACTER*(*) FILNM
C
C     ensure all layers visible
      DO 5 I=0,255
        VLAYER(I)=.TRUE.
 5    CONTINUE
C     reset clipping limits to lie exactly
C     on the section limits
C     save current clipping window
      X1=WXMIN
      Y1=WYMIN
      X2=WXMAX
      Y2=WYMAX
C     reset to section limits
      WXMIN=WINX1
      WYMIN=WINY1
      WXMAX=WINX2
      WYMAX=WINY2
C     regenerate the current viewport
      CALL REGEND
C     ensure scratch file is emptied
      CALL ZRFLAG()
C     copy all entries from display file
C     into scratch file
      CALL CPDSSC()
C     clear all transformations
      CALL CLROPF()
C     set translation for origin
      REFDAT(4,1)=-WINX1
      REFDAT(4,2)=-WINY1
      OPFLAG(4)=.TRUE.
C     create SECTION file
      I=4
      CALL CRTC04(I,.FALSE.,.FALSE.,FILNM)
C     recover current clipping window
      WXMIN=X1
      WYMIN=Y1
      WXMAX=X2
      WYMAX=Y2
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCB00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the SPECIAL FUNC 2 function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SPCB01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SPECIAL FUNC 2 routine
      CALL SPCB01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCB01()
C     ===================
C
      EXTERNAL GTCLRM,SPCB02,MNLSP2
C
C     initialize option menu for FUNC 2
      CALL GTCLRM(3)
C     load menu with FUNC 2 options
C      CALL MNLSP2()
C     go do the function
      CALL SPCB02()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCB02()
C     ===================
C
      include 'include/section.inc'
C
      INTEGER*4 N,NLEN,DNUM
      INTEGER*4 WANTED
      LOGICAL OK
      CHARACTER*80 NAME
      EXTERNAL GTCLRM,SPCB03,MNLSP2,NLEN
C
C     clear section block
      CALL OSCLRM()
      N=0
C	Could keep prompting till blank entry .
      CALL ASKINT(490,.FALSE.,1,WANTED)
C     go get a section file name
      DNUM=485
 5    CONTINUE
      CALL DPRMXP(DNUM,NAME)
      CALL SUFFIX(NAME,'.sec')
      N=N+1
      IF ( N.LE.WANTED) THEN
C       check valid name
        IF (NLEN(NAME).EQ.0) THEN
          N=N-1
          GOTO 5
        END IF
C       go load the section
C       set current section number
        SECNUM=N
C       preload the section data
        CALL SPCB04(NAME,OK)
C       do not count missed preloads
        IF (.NOT.OK) N=N-1
C       go get next section
        IF (N.LT.WANTED) GOTO 5
      END IF
C     go set complete mapping
      CALL OSETMM()
C     go load all bitmaps
      CALL OSLODA()
C     go load the sections
      CALL OSLODB()
C     go do the filter function
      CALL SPCB03()
C     zoom to complete view
      CALL ZOMEXT()
C
      END
C
 
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCB03()
C     ===================
C
C2    Subroutine SPCB03 performs entity
C2    filtering operations,to remove duplicate
C2    entities from the database.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 I,J,LASTI,ENT
      INTEGER*2 II,JJ,ENT2,IMB(13)
      INTEGER*4 K1,K2
      REAL M1(3,3),M2(3,3),RR(6),R1
      LOGICAL DELETE,OK,SIMPLE,SAME,COMSYM
      CHARACTER*80 STRING
      EXTERNAL SAME
C
      write(10,*)'******************************************'
      write(10,*)'***      START OF ENTITY FILTER       ****'
      write(10,*)'******************************************'
      LASTI=NMIPOS-1
      DO 10 I=1,LASTI
C      read next entity
       CALL ALLRD(I,ENT,M1,DELETE)
       IF (.NOT.DELETE) THEN
        IF (IMBUFF(1).NE.COMPM .AND. IMBUFF(1).NE.SYMBM) THEN
C        test this entity against database
C        save text if present
         IF (IMBUFF(9).GT.0)  THEN
          STRING=CBUFF
         ELSE
          STRING=' '
         END IF
C        set flag for simple entity
         SIMPLE=ENT.EQ.LINE .OR. ENT.EQ.ARC .OR. ENT.EQ.TEXT
         COMSYM=ENT.EQ.COMPI .OR. ENT.EQ.SYMBI
C        copy MI record
C        and first geometric record
         DO 20 J=1,6
          RR(J)=RDBUFF(J)
 20      CONTINUE
         DO 21 J=1,13
          IMB(J)=IMBUFF(J)
 21      CONTINUE
C        scan database for match
         DO 30 II=1,LASTI
C         read next MI record
          CALL DIR500(II,OK)
          IF (IMBUFF(1).NE.100 .AND. II.NE.I) THEN
           IF (IMBUFF(1).NE.COMPM .AND. IMBUFF(1).NE.SYMBM) THEN
C           may be worth comparing
C           compare contents of index records
            OK=IMB(1).EQ.IMBUFF(1)
            DO 22 J=2,6
             OK=OK.AND.(IMB(J).EQ.IMBUFF(J))
 22         CONTINUE
            IF (OK) THEN
C            entity types are same
C            try for match in data record
             IF (SIMPLE) THEN
C             read entity record
              CALL DER500(II,OK)
C             compare contents of data records
              OK=SAME(RR(1),RDBUFF(1))
              DO 23 J=2,6
               OK=OK .AND. SAME(RR(J),RDBUFF(J))
 23           CONTINUE
C             test for same line,but reverse direction
              IF (.NOT.OK .AND. ENT.EQ.LINE) THEN
C              reverse data and try again
               DO 26 J=1,3
                R1=RDBUFF(J)
                RDBUFF(J)=RDBUFF(J+3)
                RDBUFF(J+3)=R1
 26            CONTINUE
              END IF
C             compare contents of data records
              OK=SAME(RR(1),RDBUFF(1))
              DO 27 J=2,6
               OK=OK .AND. SAME(RR(J),RDBUFF(J))
 27           CONTINUE
             ELSE IF (COMSYM) THEN
C             Instance entity
              CALL ALLRD(II,ENT2,M2,DELETE)
C             compare text names
              IF (STRING.EQ.CBUFF) THEN
C              same names,test transform
               OK=SAME(M1(1,1),M2(1,1))
               IF (OK) THEN
C               compare complete arrays
                DO 24 K1=1,3
                 DO 25 K2=1,3
                  OK=OK .AND. SAME(M1(K1,K2),M2(K1,K2))
 25              CONTINUE
 24             CONTINUE
               END IF
              ELSE
C              cannot be the same
               OK=.FALSE.
              END IF
             ELSE
              OK=.FALSE.
             END IF
             IF (OK) THEN
C             delete this entity
              IMBUFF(1)=100
              CALL DIM500(II,OK)
             END IF
            END IF
           END IF
          END IF
 30      CONTINUE
        END IF
       END IF
 10   CONTINUE
C
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCB04(FILNM,OK)
C     ===========================
C
      include 'include/filunit.inc'
      include 'include/lfu.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/params.inc'
C
      INTEGER*4 NLEN1
      LOGICAL OK
      CHARACTER*(*) FILNM
      EXTERNAL NLEN1,RSTFHD,RSTDHD,RSTSSC
C
      CALL SUFFIX(FILNM,'.sec')
C     set file type
      FILTYP='SECT'
C     open the file
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
      IF ( .NOT. OK ) THEN
C        something wrong with file
         WRITE(10,*)'[SPCB04] Cannot open:',FILNM
         RETURN
      END IF
C     ensure reading starts at beginning of file
      REWIND(UNIT=PARFUN,ERR=99)
C     read the file header
      CALL RSTFHD(OK)
      IF (.NOT.OK) GOTO 99
C     check file type
      IF (CFHEAD(1)(1:NLEN1(CFHEAD(1))).NE.'SECT') GOTO 98
C
      OROTRV=RFHEAD(1)
C
C     read the drawing header
      CALL RSTDHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     section files contain additional records
C     recover SECTION records
      CALL RSTSSC(OK)
      IF (.NOT.OK) GOTO 99
C
      CLOSE(UNIT=PARFUN,STATUS='KEEP')
      RETURN
C
 98   CONTINUE
      WRITE(UNIT=10,FMT=*)'[SPCB04] File is not correct type'
      WRITE(UNIT=10,FMT=*)'[SPCB04] TYPE=',CFHEAD(1)
      CLOSE(UNIT=PARFUN,STATUS='KEEP')
      RETURN
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[SPCB04] Error reading part data'
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCC00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the SPECIAL FUNC 3 function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SPCC01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SPECIAL FUNC 2 routine
      CALL SPCC01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCC01()
C     ===================
C
      EXTERNAL GTCLRM,SPCC02,MNLSP3
C
C     initialize option menu for FUNC 3
      CALL GTCLRM(3)
C     load menu with FUNC 3 options
C      CALL MNLSP3()
C     go do the function
      CALL SPCC02()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCC02()
C     ===================
C
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE SPCD00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the SPECIAL FUNC 4 function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SPCD01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the CHANGE DIMEN routine
      CALL SPCD01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCD01()
C     ===================
C
      EXTERNAL GTCLRM,SPCA02,MNLSP4
C
C     initialize option menu for FUNC 4
      CALL GTCLRM(3)
C     load menu with FUNC 4 options
C      CALL MNLSP4()
C     go do the function
C      CALL SPCD02()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SPCE00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the SPECIAL FUNC 5 function
C
      include 'include/menun.inc'
      include 'include/masti.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SPCE01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SPECIAL FUNC 5 routine
      WRITE(10,*) 'SPCE00 NMIPOS,NPDPOS:',NMIPOS,NPDPOS
      CALL SPCE01()
      WRITE(10,*) 'SPCE00 NMIPOS,NPDPOS:',NMIPOS,NPDPOS
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
      SUBROUTINE SPCE01()
C     ===================
 
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      INTEGER*2 START,I
      LOGICAL SAME,OK
      REAL BF(6)
      EXTERNAL GTCLRM,SPCE02,MNLSP5,SAME
C
C     initialize option menu for FUNC 5
      CALL GTCLRM(3)
C     load menu with FUNC 5 options
C      CALL MNLSP5()
C     go do the function
C      CALL SPCE02()
 
      MEN=0
      CCMD='Q'
      START=NMIPOS-1
C
      WRITE(10,*) 'NMIPOS,NPDPOS:',NMIPOS,NPDPOS
 5    CALL DIR500(START,OK)
      IF ( .NOT.OK ) RETURN
      IF ( IMBUFF(2).NE.LINE) RETURN
      IF ( IMBUFF(7).LT.32000) THEN
        CALL DBR500(IMBUFF(7),OK)
      ELSE
        START=START-1
        GOTO 5
      END IF
 
      DO 10 I=1,6
        BF(I)=RDBUFF(I)
 10   CONTINUE
 
 11   START=START-1
 
      NMIPOS=START+2
      NPDPOS=IMBUFF(7)+1
 
      CALL DER500(START,OK)
 
      IF ( .NOT.OK ) RETURN
      IF ( IMBUFF(2).NE.LINE) RETURN
 
      OK=SAME(BF(1),RDBUFF(1))
      IF ( OK ) THEN
        DO 12 I=2,6
          OK=OK.AND.SAME(BF(I),RDBUFF(I))
 12     CONTINUE
      END IF
      IF ( OK.AND.START.GT.1 ) GOTO 11
C
      END
C
C-----------------------------------------------------------
C
