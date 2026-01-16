C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 select.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GRID()
C     SUBROUTINE GRIDM0(QUIT)
C     SUBROUTINE ISTD10()
C     SUBROUTINE MAJSEL()
C     SUBROUTINE MENINT()
C     SUBROUTINE MENLOD()
C     SUBROUTINE MENSAV(MNAME)
C     SUBROUTINE MENSET()
C     SUBROUTINE MNIGRD()
C     SUBROUTINE MNISEL()
C     SUBROUTINE MNISL0()
C     SUBROUTINE MNIST0()
C     SUBROUTINE MNLDIG()
C     SUBROUTINE MNLMEN()
C     SUBROUTINE MNLPN0()
C     SUBROUTINE MNLSD0
C     SUBROUTINE MNLSJ0()
C     SUBROUTINE MNLSS0()
C     SUBROUTINE SELD00()
C     SUBROUTINE SELD02()
C     SUBROUTINE SELD03(OK)
C     SUBROUTINE SELDD0()
C     SUBROUTINE SELDD1(WX,WY,QUIT)
C     SUBROUTINE SELDD2(WX,WY,QUIT)
C     SUBROUTINE SELDD3(ANGLE,QUIT)
C     SUBROUTINE SELDD4(DNUM,SCALE,QUIT)
C     SUBROUTINE SELJ00()
C     SUBROUTINE SELJ02()
C     SUBROUTINE SELJ03(OK)
C     SUBROUTINE SELL00()
C     SUBROUTINE SELL02()
C     SUBROUTINE SELL03(PRMNAM,MENNAM,TXNAM,DRWNAM,CHANGE)
C     SUBROUTINE SELM00()
C     SUBROUTINE SELPN0()
C     SUBROUTINE SELS00()
C     SUBROUTINE SELS02()
C     SUBROUTINE SELT00()
C     SUBROUTINE SELT02()
C     SUBROUTINE SETDIG(WX,WY,DGX,DGY,SCALE,ANGLE)
C     SUBROUTINE STDFLS()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE GRID()
C     =================
C
C1    no arguments required
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/wtov.inc'
      include  'include/viewport.inc'
C
      REAL X,Y
      LOGICAL QUIT,OK
      CHARACTER TOKEN*1
      INTEGER*4 TC,C,TMEN,TCELL
      EXTERNAL TCURS,MNIGRD,GRID1,FNDTOK,GTMCWR
C                    
C     Show we are in the Grid select menu
      GRDMNU = .TRUE.                    
C
      TMEN=MEN
      TCELL=CELLN
C     Erase the old grid
C
      CALL MNIGRD()
C
C     find token for X:
      CALL FNDTOK(287,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDOX,'(F12.2)')
C
C     find token for Y:
      CALL FNDTOK(288,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDOY,'(F12.2)')
C
C     find token for X Dist:
      CALL FNDTOK(285,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDSX,'(F8.2)')
C
C     find token for X Dist:
      CALL FNDTOK(286,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDSY,'(F8.2)')
C
 10   CONTINUE
C     tell the guy what to do
      CALL DCPRNT(38)
C
      CALL TCURS(C,X,Y)
      IF ( MEN.EQ.0 ) THEN
         CALL DEPRNT(117)
         GOTO 10
      ELSE IF ( MEN .EQ. 3 ) THEN
         CALL GRIDM0(QUIT)
C        Save the contents of the grid into the current viewport
         CALL SAVGRD(CVPN,OK)
         IF ( .NOT.QUIT ) GOTO 10
      ELSE IF ( CCMD .EQ. 'q'.OR.MEN.EQ.2) THEN
C         IF ( SETGRD ) THEN
C            CALL GRID1()
C         END IF
      END IF
      CALL GTCLRM(3)
      CALL GTMCLO(TMEN,TCELL)
                   
C     Save the contents of the grid into the current viewport
      CALL SAVGRD(CVPN,OK)
C     Show we are exiting the Grid select menu
      GRDMNU = .FALSE.                    
C

C
      END
C
      SUBROUTINE GRIDM0(QUIT)
C     ===================
C
C1    no arguments required
C
      include  'include/ndata.inc'
      include  'include/menun.inc'
      include  'include/nbuff.inc'
      include  'include/gtxt2.inc'
      include  'include/vntable.inc'
C
C
      INTEGER*4 C,NLEN,TMEN,TCELL,MNCODE
      DOUBLE PRECISION DIST
      LOGICAL QUIT,OPTION, OK
      CHARACTER*16 OLIN,TOKEN*1
      REAL GRX,GRY
C
      EXTERNAL AEXPRN,NLEN,GTPMEN,GTMCLO,GTHFMC,
     +         GETANS,GTMCWR,FNDTOK,GTDMWT
C
 10   CONTINUE
C
      QUIT=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C
      IF ( CCMD .EQ. 'O' ) THEN
        CALL MNLPTS()
        CALL DCPRNT(119)
        CALL GETANS(C,GRX,GRY)
        CALL MNUPTS()
        IF ( MEN .NE. 0 ) THEN
          CALL GTMCLO(TMEN,TCELL)
          IF ( MEN .EQ. 3 ) GOTO 10
          QUIT=.TRUE.
          RETURN
        END IF
C       draw out old grid NOW!
        IF(SETGRD) CALL EGRID()
C       set old values
        GRIDOX = GRX
        GRIDOY = GRY
C       find token for X:
        CALL FNDTOK(287,TOKEN)
C       update cell content
        CALL GTMCWR(3,TOKEN,GRIDOX,'(F12.2)')
C       find token for Y:
        CALL FNDTOK(288,TOKEN)
C       update cell content
        CALL GTMCWR(3,TOKEN,GRIDOY,'(F12.2)')
C       draw the new grid
        IF(SETGRD) CALL GRID1()
C       reset hilite on origin cell
        CALL GTMCLO(TMEN,TCELL)
C
      ELSE IF ( CCMD .EQ. 't' ) THEN
C*************************************************
C      GRID TYPE OPTION POPUP                    *
C*************************************************
         GTMULT = .TRUE.
         MNCODE = 13
C        stick up the popup
         CALL MENPOP(MNCODE,OK)
         IF(OK) THEN
           IF(SETGRD) CALL EGRID()
C    
           IF(CCMD .EQ. 'C') THEN
C****************************************************
C               cartesian grid                      *
C****************************************************
              GRTYPE = 1
              CALL GTDMWT(348,3,VNOUN(58))
              IF(SETGRD) CALL GRID1()
           ELSEIF(CCMD .EQ. 'i') THEN
C****************************************************
C               isometric grid                      *
C****************************************************
              GRTYPE = 2
              CALL GTDMWT(348,3,VNOUN(344))                
              IF (SETGRD) CALL GRID1()
           ENDIF 
         ENDIF
C*************************************************
C      GRID TYPE OPTION FINISHED                 *
C*************************************************
      ELSE IF ( CCMD .EQ. 'A' ) THEN
 111    CONTINUE
        CALL DPRMXP(120,CBUFF)
        IF ( NLEN(CBUFF) .GT. 0 ) THEN
C         evaluate an arithmetic expression from the keyboard
          CALL AEXPRN(CBUFF,DIST,*111)
          IF ( DIST .NE. 0.0 ) THEN
            IF(SETGRD) CALL EGRID()
            GRIDSX=REAL(DIST)
            IF(SETGRD) CALL GRID1()
C           find token for X Dist:
            CALL FNDTOK(285,TOKEN)
C           update cell content
            CALL GTMCWR(3,TOKEN,GRIDSX,'(F8.2)')
          ELSE
C           prompt user that zero radius is illegal
C           and try again
            CALL DEPRNT(121)
            GOTO 111
          END IF
        END IF
      ELSE IF ( CCMD .EQ. 'B' ) THEN
 112    CONTINUE
        CALL DPRMXP(122,CBUFF)
        IF ( NLEN(CBUFF) .GT. 0 ) THEN
C         evaluate an arithmetic expression from the keyboard
          CALL AEXPRN(CBUFF,DIST,*112)
          IF ( DIST .NE. 0.0 ) THEN
            IF(SETGRD) CALL EGRID()
            GRIDSY=REAL(DIST)
            IF(SETGRD) CALL GRID1()
C           find token for Y Dist:
            CALL FNDTOK(286,TOKEN)
C           update cell content
            CALL GTMCWR(3,TOKEN,GRIDSY,'(F8.2)')
          ELSE
C           prompt user that zero radius is illegal
C           and try again
            CALL DEPRNT(123)
            GOTO 112
          END IF
        END IF
      ELSE
C       unrecognized delete option
        CALL DEPRNT(38)
      END IF
 
      CALL GTMCLO(TMEN,TCELL)
C
      END
*
*
      SUBROUTINE ISTD10()
C     ===================
C1    no arguments required
C2
C2    controls operation of the SELECT STANDARD option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/dimendat.inc'
C
      INTEGER*4 CHK,C
      EXTERNAL STDFLS
 
C
      IF ( BS )   CHK=411
      IF ( DIN )  CHK=412
      IF ( ANSI ) CHK=413
C
      CALL FNDPOS(CHK,C)
      CALL GTMCLO(3,C)
C
      IF ( CCMD.EQ.'B') THEN
C        Set for British Standard 308
         CALL STDFLS()
         BS=.TRUE.
         GAPL=3
      ELSE IF ( CCMD.EQ.'D') THEN
C        Set for DIN 406
         CALL STDFLS()
         DIN=.TRUE.
C        hence set the Gapl length accordingly.
         GAPL=0.0
      ELSE IF ( CCMD.EQ.'A') THEN
C        set for ANSI
         CALL STDFLS()
         ANSI=.TRUE.
         GAPL=3
C
      END IF
C
      IF ( BS )   CHK=411
      IF ( DIN )  CHK=412
      IF ( ANSI ) CHK=413
      CALL FNDPOS(CHK,C)
      CALL GTMCHI(3,C)
C
 
      END
*
      SUBROUTINE MAJSEL()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the SELECT mode
C2    of operation is selected from the master menu.
C2    controls operation of the SELECT function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
      LOGICAL OK
      REAL X,Y
C
      EXTERNAL MNISEL,SELD00,GTCLRM,GRID,GTHFMC,SELT00,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,SELJ00
C
C     Now activate the SELECT major option menu
      CALL MNISEL()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C     Making single line the default insert text
      MEN=2
C     'D' is the token used by select dimension
      CCMD='D'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
 
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select SELECT mode
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
         IF (CCMD.EQ.'D') THEN
C           select dimension option
            CALL SELD00()
         ELSE IF (CCMD.EQ.'G') THEN
C           select grid option
            CALL GRID()
         ELSE IF (CCMD.EQ.'T') THEN
C           select text option
            CALL SELT00()
         ELSE IF (CCMD.EQ.'L') THEN
C           select line option
            CALL SELL00()
         ELSE IF (CCMD.EQ.'S') THEN
C           select dimension standard option
            CALL SELS00()
         ELSE IF (CCMD.EQ.'d') THEN
C           select digitiser
            CALL SELDD0()
         ELSE IF (CCMD.EQ.'P') THEN
C           select Pathname change
            CALL SELPN0()
         ELSE IF (CCMD.EQ.'M') THEN
C           select MENU option
            CALL SELM00()
         ELSE IF (CCMD.EQ.'K') THEN
C           select JOURNAL option
            CALL SELJ00()
         ELSE IF (CCMD.EQ.'V') THEN
C           select MENU option
            CALL SELV00()
         ELSE
C           unrecognized delete option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         CALL DEPRNT(284)
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
C
      RETURN
      END
*
      SUBROUTINE MENINT()
C     ===================	
C2    This subroutine loads the default menu definition
C
      include 'include/menug.inc'
      include 'include/dig2wo.inc'
      include 'include/vntable.inc'
      include 'include/datatype.inc'
C
      CHARACTER*80 MNAME
      INTEGER*4 UNIT,DNUM,ST
      INTEGER*2 I,J
      LOGICAL OK,KEEP
C
C     make sure the digitiser has been switched on
      IF (.NOT.DIGIT) THEN
         IF (STRIDO.EQ.0) RETURN
         DIGIT=.TRUE.
      END IF
C
C     open menu definition file
      MNAME='daxcad.dmf'
      CALL FPARS(MNAME,ST)
      CALL OPNOFF(MNAME,UNIT,OK)
      IF (.NOT.OK) THEN
         DNUM=287
         CALL EPRINT(DICT01(DNUM))
         RETURN
      END IF
C
C     load No. of menu cells
      READ(UNIT,*,ERR=99,END=99) MXN,MYN
C     load cell increments
      READ(UNIT,'(3(F10.3))',ERR=99,END=99) MINCX,MINCY
C     load grid limits
      READ(UNIT,'(4(F10.3))',ERR=99,END=99) MLIMIT(1),MLIMIT(2),
     +                                MLIMIT(3),MLIMIT(4)
C     load transformation
      DO 10 I=1,3
         READ(UNIT,'(4(F10.3))',ERR=99,END=99) MENWXY(I,1),
     +                            MENWXY(I,2),MENWXY(I,3)
 10   CONTINUE
C     load the text strings
      DO 15 I=1,MXN
         DO 20 J=1,MYN
            READ(UNIT,'(A)',ERR=99,END=99) MENTXT(I,J)
20       CONTINUE
15    CONTINUE
C
C     close the file and keep it
      KEEP=.TRUE.
      CALL CLOSUN(UNIT,KEEP,OK)
      IF (.NOT.OK) GOTO 99
C
      DNUM=571
      CALL CPRINT(DICT01(DNUM))
      RETURN
C
 99   CONTINUE
      DNUM=422
      CALL EPRINT(DICT01(DNUM))
C
      END
C
      SUBROUTINE MENLOD()
C     ===================
C1    no arguments required
C2    handles loading up the selected menu file
C2    part of the CREATE MENU code
C2    created 14/7/88 - mlw
C
      include 'include/vntable.inc'
      include 'include/menug.inc'
      include 'include/macro.inc'
      include 'include/library.inc'
C
      CHARACTER*80 MNAME,SFILE
      REAL X,Y
      INTEGER*4 DNUM,UNIT,PHNUM,NLEN
      LOGICAL YESOK,OK,KEEP,QUIT
      INTEGER*2 I,J
C
      EXTERNAL YESOK,NLEN
C
      MFILOK=.FALSE.
10    CONTINUE
C     get the name of a file to load it from
C      DNUM=58
C      CALL DPRMXP(DNUM,MNAME)
      PHNUM = 9     

C      PATHN(9) = '/DAXCAD/DMFDIR/'
       PATHN(9)=LIBRARY(1:NLEN(LIBRARY))//'/DMFDIR/'

      CALL  DISFIL(SFILE,MNAME,PHNUM,OK,QUIT)
C     null entry indicates he wants to forget it
      IF (MNAME.EQ.' ') RETURN
C     check for invalid file name (eg DAXCAD)
      CALL CHKNAM(MNAME,OK)
      IF (.NOT.OK.OR.QUIT) GOTO 100
C     open the file
      CALL OPNOFF(MNAME,UNIT,OK)
      IF (.NOT.OK) GOTO 10
C
C     save the number of cells in each axis
      READ(UNIT,*,ERR=99,END=98) XN,YN
      MXN = XN
      MYN = YN
C     save cell increments
      READ(UNIT,'(2(F10.3))',ERR=99,END=98) INCX,INCY
      MINCX = INCX
      MINCY = INCY
C     save the origin and extents of the grid
      READ(UNIT,'(4(F10.3))',ERR=99,END=98) GLIMIT(1),GLIMIT(2),
     +                                GLIMIT(3),GLIMIT(4)
      MLIMIT(1) = GLIMIT(1)
      MLIMIT(2) = GLIMIT(2)
      MLIMIT(3) = GLIMIT(3)
      MLIMIT(4) = GLIMIT(4)
C
C     load transformation
      DO 12 I=1,3
         READ(UNIT,'(4(F10.3))',ERR=99,END=99) MENWXY(I,1),
     +                            MENWXY(I,2),MENWXY(I,3)
 12   CONTINUE
C     save the text strings
      DO 15 I=1,XN
         DO 20 J=1,YN
            READ(UNIT,'(A)',ERR=99,END=98) DMENU(I,J)
             MENTXT(I,J) = DMENU(I,J)
20       CONTINUE
15    CONTINUE
C
C     close the file and keep it
      KEEP=.TRUE.
      CALL CLOSUN(UNIT,KEEP,OK)
      IF (.NOT.OK) GOTO 98
C
      MFILOK=.TRUE.
C     make the new menu be stored in common
      DMFILE = MNAME
      DNUM=569
      CALL CPRINT(DICT01(DNUM))
      RETURN
C
 98   CONTINUE
      DNUM=422
      CALL EPRINT(DICT01(DNUM))
C
 99   CONTINUE
      DNUM=83
      CALL EPRINT(DICT01(DNUM))
C
 100  CONTINUE
C
      END
*
      SUBROUTINE MENSAV(MNAME)
C     ===================	
C1    IOSTAT             I
C1    IOTYPE            C*(*)
C2    This subroutine saves the menu parameters 	
C
      include 'include/menug.inc'
      include 'include/vntable.inc'
C
      CHARACTER*(*) MNAME
      REAL A(3,3),B(3,3),C(3,3),DANGLE,MANGLE,CANG,TLIMIT(4)
      INTEGER*4 UNIT,DNUM
      INTEGER*2 I,J
      LOGICAL OK,KEEP
C
C     calculate correction angle for menu
      DANGLE=CANG(DLIMIT(1),DLIMIT(2),DLIMIT(3),DLIMIT(4))
      MANGLE=CANG(GLIMIT(1),GLIMIT(2),GLIMIT(3),GLIMIT(4))
      MNANG=DANGLE-MANGLE
C
C     set up transform matrix for menu
      CALL I3M(A)
      CALL I3M(B)
      CALL I3M(C)
      CALL TRAN2D(-DLIMIT(1),-DLIMIT(2),A)
      CALL ROT2D(-MNANG,B)
      CALL MULT3M(A,B,C)
C
C     correct the menu limits before saving
      CALL NEWXY(DLIMIT(1),DLIMIT(2),TLIMIT(1),TLIMIT(2),C)
      CALL NEWXY(DLIMIT(3),DLIMIT(4),TLIMIT(3),TLIMIT(4),C)
C
C     calculate cell increments
      INCX=(TLIMIT(3)-TLIMIT(1))/XN
      INCY=(TLIMIT(4)-TLIMIT(2))/YN
C
C     open menu definition file
C      MNAME='DAXCAD.DMF'
      CALL OPNFFF(MNAME,UNIT,OK)
      IF (.NOT.OK) THEN
         DNUM=287
         CALL EPRINT(DICT01(DNUM))
         RETURN
      END IF
C
C     save No. of menu cells
      WRITE(UNIT,*,ERR=99) XN,YN
C     save cell increments
      WRITE(UNIT,'(2(F10.3))',ERR=99)INCX,INCY
C     save grid limits
      WRITE(UNIT,'(4(F10.3))',ERR=99) TLIMIT(1),TLIMIT(2),
     +                                TLIMIT(3),TLIMIT(4)
C     save transformation
      DO 10 I=1,3
         WRITE(UNIT,'(4(F10.3))',ERR=99) C(I,1),C(I,2),
     +                                   C(I,3)
 10   CONTINUE
C     save the text strings
      DO 15 I=1,XN
         DO 20 J=1,YN
            WRITE(UNIT,'(A)',ERR=99) DMENU(I,J)
20       CONTINUE
15    CONTINUE
C
C     close the file and keep it
      KEEP=.TRUE.
      CALL CLOSUN(UNIT,KEEP,OK)
      IF (.NOT.OK) GOTO 99
C
      MFILOK=.TRUE.
      DNUM=570
      CALL CPRINT(DICT01(DNUM))
      IF (MENOK) THEN
          CALL MENINT()
      END IF
      RETURN
C
 99   CONTINUE
      DNUM=83
      CALL EPRINT(DICT01(DNUM))
C
      END
C
      SUBROUTINE MENSET()
C     ===================	
C2    This subroutine sets the transformation for the digitiser 	
C2    to world coordinates.
C
      include 'include/menug.inc'
      include 'include/vntable.inc'
      include 'include/dig2wo.inc'
      include 'include/datatype.inc'
C
      REAL TLIMIT
      CHARACTER CBUFF*80
      INTEGER*4 DNUM,C,NLEN1
C
      EXTERNAL CANG,NLEN1
C
C     make sure the digitiser has been switched on
      IF (.NOT.DIGIT) THEN
         IF (STRIDO.EQ.0) RETURN
          DIGIT=.TRUE.
      END IF
C
      MSETOK=.FALSE.
      WRITE(CBUFF,FMT='(A,I3)')
     +     DICT01(566)(1:NLEN1(DICT01(566))),DIGBUT
      CALL CPRINT(CBUFF)
C     record point coords
      CALL SCURS(C,DLIMIT(1),DLIMIT(2))
      IF ( C-153.NE.DIGBUT) THEN
         CALL DEPRNT(430)
         RETURN
      END IF
      WRITE(CBUFF,FMT='(A,I3)')
     +     DICT01(567)(1:NLEN1(DICT01(567))),DIGBUT
      CALL CPRINT(CBUFF)
C     record point coords
      CALL SCURS(C,DLIMIT(3),DLIMIT(4))
      IF ( C-153.NE.DIGBUT) THEN
         CALL DEPRNT(430)
         RETURN
      END IF
C
C     get the coordinates sorted out
      IF (DLIMIT(1).GT.DLIMIT(3)) THEN
          TLIMIT=DLIMIT(1)
          DLIMIT(1)=DLIMIT(3)
          DLIMIT(3)=TLIMIT
      END IF
      IF (DLIMIT(2).GT.DLIMIT(4)) THEN
          TLIMIT=DLIMIT(2)
          DLIMIT(2)=DLIMIT(4)
          DLIMIT(4)=TLIMIT
      END IF
C
      DNUM=568
      CALL CPRINT(DICT01(DNUM))
      MSETOK=.TRUE.
C
      END
C
      SUBROUTINE MNIGRD()
C     =================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major INSERT options.
C2
      include 'include/ndata.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
      EXTERNAL GTPMEN,GTCLRM,GTDMWT
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Enter the SET GRID major option.
C
C     Load the entities types into major option menu.
C
C2    O is the token for new GRID origin
      CALL GTDMEN(284,3)
C2    t is the token for type
      GTMULT = .TRUE.
      IF(GRTYPE .EQ. 1) THEN
C       cartesian type
        CALL GTDMWT(348,3,VNOUN(58))
      ELSE
C       isometric type
        CALL GTDMWT(348,3,VNOUN(344))
      ENDIF
C2    A is the token for New Distnce GRIDX
      CALL GTDMEN(285,3)
C2    B is the token for New Distnce GRIDY
      CALL GTDMEN(286,3)
C2    C is the token for origin X GRIDOX
      CALL GTDMEN(287,3)
C2    B is the token for origin Y GRIDOY
      CALL GTDMEN(288,3)
C
C
C
      END
C
      SUBROUTINE MNISEL()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major SELECT options.
C2
      EXTERNAL GTPMEN,GTCLRM,GTMCHI
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the SELECT  major option.
      CALL GTDMHD(14,2)
C
C      CALL GTDMEN(280,2)
C     enter the grid option
      CALL GTDMEN(281,2)
C     Load the nouns into major option menu.
      CALL GTDMEN(282,2)
C
      CALL GTDMEN(283,2)
C
      CALL GTDMEN(279,2)
C     Load the SELECT INTERNATIONAL STANDARD option
      CALL GTDMEN(410,2)
C     Enter the Pathname option
      CALL GTDMEN(267,2)
C     enter the select menu option
      CALL GTDMEN(487,2)
C     enter the select JOURNAL option
      CALL GTDMEN(48,2)
C     enter the select viewport system
      CALL GTDMEN(515,2)
C
      END
*
      SUBROUTINE MNISL0()
C     ===================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the SELECT LANGUAGE option list.
C
      EXTERNAL GTCLRM,GTDMEN
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     load ENGLISH option
      CALL GTDMEN(400,3)
C     load FRENCH option
      CALL GTDMEN(401,3)
C     load GERMAN option
      CALL GTDMEN(402,3)
C     load ITALIAN option
      CALL GTDMEN(403,3)
C     load SPANISH option
      CALL GTDMEN(404,3)
C
C     load ACCEPT option
      CALL GTDMEN(13,3)
C
      END
C
*
      SUBROUTINE MNIST0()
C     ===================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the SELECT TEXT option list.
C2
C2    Tokens used here are X and (unassigned).
C2
      INTEGER*4 I
C
      EXTERNAL GTCLRM,MNLTX1
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Load the text parameters.
      CALL MNLTX1()
C
      END
C
C
      SUBROUTINE MNLDIG()
C     ===================
C
C   292     6   (D)     "Digitise ON"
C   293     6   (D)     "Digitise OFF"
C   294     8   (S)     "Define"
 
      include 'include/dig2wo.inc'
C
      INTEGER*4 DNUM
C
C     clear menu 3
      CALL GTCLRM(3)
C
C     scale of drawing.
      DNUM=10
      CALL GTDMEN(DNUM,3)
      DNUM=3
      CALL GTMCWR(DNUM,'S',SCADIG,'(F6.3)')
C
      IF ( DIGIT ) THEN
         DNUM=292
      ELSE
         DNUM=293
      END IF
      CALL GTDMEN(DNUM,3)
C
C     digitise origin
      DNUM=296
      CALL GTDMEN(DNUM,3)
C     screen origin
      DNUM=297
      CALL GTDMEN(DNUM,3)
C     Aligment line.
      DNUM=298
      CALL GTDMEN(DNUM,3)
C     accept settings.
      DNUM=13
      CALL GTDMEN(DNUM,3)
 
C     Aligment line.
      DNUM=299
      CALL GTDMWR(DNUM,3,DIGANG,'(F6.3)')
C     Aligment line.
      DNUM=294
      CALL GTDMWR(DNUM,3,DIGSNA,'(F6.3)')
 
      END
C
      SUBROUTINE MNLMEN()
C     ===================
C
      include 'include/menug.inc'
C
      INTEGER*4 DNUM
C
C     clear menu 3
      CALL GTCLRM(3)
C
      IF ( MENOK ) THEN
         DNUM=488
      ELSE
         DNUM=489
      END IF
      CALL GTDMEN(DNUM,3)
C
C     menu name
      DNUM=490
      CALL GTDMEN(DNUM,3)
C     set menu
      DNUM=491
      CALL GTDMEN(DNUM,3)
C     accept menu.
      DNUM=13
      CALL GTDMEN(DNUM,3)
C
      END
C
      SUBROUTINE MNLPN0()
C     ===================
C1    no arguments required
      include  'include/macro.inc'
C
C2    writes out SELECT PATHNAME options load up the cells
      CALL GTCLRM(3)
      CALL GTDMEN(289,3)
      CALL GTDMEN(319,3)
      CALL GTDMEN(322,3)
      CALL GTDMEN(323,3)
      CALL GTDMEN(334,3)
      CALL GTDMEN(333,3)
c      CALL GTDMEN(338,3)
CIBM|PC386
C      IF ( MFILE ) THEN
C        CALL GTDMEN(336,3)
C      ELSE
C       CALL GTDMEN(337,3)
C      ENDIF
CIBM|PC386
 
CSUN - backcloth
      CALL GTDMEN(338,3)
CSUN
 
      END
C
      SUBROUTINE MNLSD0
C     =================
C1    No arguments required.
C
C2    Subroutine MNLSD0 loads the option menu
C2    with the parameters for selection in
C2    select dimension function
C2
C2    Tokens used are 'G e H W g L a P'
C
      include 'include/ndata.inc'
      include 'include/vntable.inc'
C
      CHARACTER*1 C,FORM*20
      INTEGER*4 GTCID4
      EXTERNAL GTPMEN,GTMCWR
C
C     Set start point for loading of menu
C     with these functions.
C
      FORM='(F5.2)'
C2    G is the token for GAPL
      CALL GTDMWR(260,3,GAPL,FORM)
C2    e is the token for EXTL
      CALL GTDMWR(261,3,EXTL,FORM)
C2    H is the token for TEXT HEIGHT
      CALL GTDMWR(262,3,DTHGT,FORM)
C2    h is the token for TEXT WIDTH
      CALL GTDMWR(263,3,DTWDT,FORM)
C2    g is the token for Text gap
      CALL GTDMWR(264,3,DTOFF,FORM)
C2    L is the token for ARROW LENGTH
      CALL GTDMWR(265,3,ALNG,FORM)
C2    a is the token for ARROW WIDTH
      CALL GTDMWR(266,3,AWDT,FORM)
C2               
C2    write precision to menu cell
      CALL GTDMWI(626,3,PREC)

      END
C
C
      SUBROUTINE MNLSJ0()
C     =================
C1    No arguments required.
C
C2    Subroutine MNLSJ0 loads the option menu
C2    with the options for selection of journaling
C2    in the selection of journaling  function
C
      include 'include/journal.inc'
C
      EXTERNAL GTDMEN, GTDMCH
C
C     Set start point for loading of menu
C     with these functions.
C
C

      IF(JOURON) THEN
C        j is the token for journal on
         CALL GTDMEN(68,3)
C        hilite the cell
         CALL GTDMCH(68,3)
      ELSE
C        J is the token for journal off
         CALL GTDMEN(67,3)
      ENDIF
C
C     i is the token for a journal comment
      CALL GTDMEN(444,3)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE MNLSS0()
C     =================
C1    No arguments required.
      include  'include/dimendat.inc'
C
C2    Clears the major and minor options, and
C2    enters the  STANDARD options.
C2
      INTEGER*4 CHK,C
      EXTERNAL GTDMEN,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Load the entities types into major option menu.
C
C2    O is the token for BS308
      CALL GTDMEN(411,3)
C2    A is the token for DIN
      CALL GTDMEN(412,3)
C2    B is the token for ANSI
      CALL GTDMEN(413,3)
C
      IF ( BS )   CHK=411
      IF ( DIN )  CHK=412
      IF ( ANSI ) CHK=413
      CALL FNDPOS(CHK,C)
      CALL GTMCHI(3,C)
 
      END
 
      SUBROUTINE SELD00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls operation of the SELECT DIMENSION function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNLSD0,SELD02,GTCLRM,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C     initialize SELECT DIMENSION menu
      CALL MNLSD0()
C     cancel all change text flags
      CALL CLROPF()
C     call the select dim routine
      CALL SELD02()
C     clear option menu
      CALL GTCLRM(3)
C
C     ensure cell no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
      SUBROUTINE SELD02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine SELD02 is the main working routine
C2    for the SELECT DIMENSION function.Uses the normal
C2    window search routines for entity selection.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/movdat.inc'
C
      REAL X,Y
C
      INTEGER*4 TMEN,TCELL,I,C
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL SELD03,CPRINT,TCURS
C
C
 10   CONTINUE
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
      IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') RETURN
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL SELD03(OK)
         GOTO 10
      ELSE IF ( MEN.EQ.0 ) THEN
c        117    "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SELD03(OK)
C     =====================
C
C2    Subroutine SELD03 is the main workiing routine
C2    for the selection of dimension parameters
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
C
      REAL WX1,WY1,TSLA
C
      INTEGER*2 MP,NCHARS,NLEN2,TJUST
      INTEGER*4 TMEN,TCELL,C,I,NLEN
C
      REAL TDTHGT,REAL
C
      DOUBLE PRECISION DN
C
      LOGICAL OK,OPTION,QUIT,CVERFY,CHANGE
C
      CHARACTER*80 CCBUFF, FORM*20
      INTRINSIC REAL
C
      EXTERNAL GTMCLO
      EXTERNAL NLEN,CVERFY,AEXPRN,GTMCWR
C
C
      FORM='(F5.2)'
 25   CONTINUE
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processinig here
 20   CONTINUE
      IF ( CVERFY(CCMD,'GeHhgLaP') ) THEN
C        He wants to change the parameters
C***************************************************************
C                     GAPL  OPTION                             *
C***************************************************************
         IF (CCMD.EQ.'G') THEN
 111     CALL DPRMXP(31,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*111)
               GAPL=REAL(DN)
               CALL GTDMWR(VNCCMD,MEN,GAPL,FORM)
               WRITE(CCBUFF,FORM) GAPL
            END IF
C***************************************************************
C                     EXTL  OPTION                             *
C***************************************************************
         ELSE IF (CCMD.EQ.'e') THEN
 112     CALL DPRMXP(32,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*112)
               EXTL=REAL(DN)
               CALL GTDMWR(VNCCMD,MEN,EXTL,FORM)
            END IF
C***************************************************************
C                 TEXT HEIGHT  OPTION                          *
C***************************************************************
         ELSE IF (CCMD.EQ.'H') THEN
 113       CONTINUE
           CALL DPRMXP(26,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*113)
               IF (DN.NE.0.0) THEN
                  TDTHGT=REAL(DN)
                  DTWDT=TDTHGT*(DTWDT/DTHGT)
                  DTHGT=TDTHGT
C                 stick in the new width
                  CALL GTDMWR(263,MEN,DTWDT,FORM)
                  CALL GTDMWR(VNCCMD,MEN,DTHGT,FORM)
               ELSE
C                 zero height not allowed
                  CALL DEPRNT(27)
                  GOTO 113
               END IF
            END IF
C***************************************************************
C                   TEXT WIDTH  OPTION                         *
C***************************************************************
         ELSE IF (CCMD.EQ.'h') THEN
 114     CALL DPRMXP(28,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*114)
               IF (DN.NE.0.0) THEN
                  DTWDT=REAL(DN)
                  CALL GTDMWR(VNCCMD,MEN,DTWDT,FORM)
               ELSE
C                 zero height not allowed
                  CALL DEPRNT(27)
                  GOTO 114
               END IF
            END IF
C***************************************************************
C                   TEXT OFFSET  OPTION                        *
C***************************************************************
         ELSE IF (CCMD.EQ.'g') THEN
 115     CALL DPRMXP(312,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*115)
               DTOFF=REAL(DN)
               WRITE(CCBUFF,FORM) DTOFF
               CALL GTDMWR(VNCCMD,MEN,DTOFF,FORM)
            END IF
C***************************************************************
C                   ARROW LENGTH  OPTION                       *
C***************************************************************
         ELSE IF (CCMD.EQ.'L') THEN
 116     CONTINUE
            CALL DPRMXP(29,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*116)
               ALNG=REAL(DN)
               CALL GTDMWR(VNCCMD,MEN,ALNG,FORM)
            END IF
C***************************************************************
C                    ARROW WIDTH  OPTION                       *
C***************************************************************
         ELSE IF (CCMD.EQ.'a') THEN
 117     CALL DPRMXP(30,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               CALL GTMCLO(MEN,CELLN)
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*117)
               AWDT=REAL(DN)
               CALL GTDMWR(VNCCMD,MEN,AWDT,FORM)
            END IF
C***************************************************************
C                     Precision  OPTION                        *
C***************************************************************
         ELSE IF (CCMD.EQ.'P') THEN
118          CALL DPRMXP(111,CCBUFF)
             IF (NLEN(CCBUFF).EQ.0 )THEN
C                user has returned zero length string
C                assume that he has change his mind and return for input
                 CALL GTMCLO(MEN,CELLN)
             ELSE
C                evaluate an arithmetic expression from the keyboard
                 CALL AEXPRN(CCBUFF,DN,*118)
C                Check range of precision is valid
                 IF ( INT(DN).GT.6 .OR. INT(DN).LT.0 ) THEN
                     CALL DEPRNT(234)
                     GOTO 118
                 END IF
                 PREC=INT(DN)
C                write precision to menu cell
                 CALL GTDMWI(VNCCMD,3,PREC)
             ENDIF
         END IF
      END IF
C     ensure caller no longer hilited
      CALL GTDMCL(VNCCMD,TMEN)
C
      END
*
*
      SUBROUTINE SELDD0()
C     =====================
C
C2    Subroutine SELD03 is the main workiing routine
C2    for the selection of Digitiser.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/dig2wo.inc'
      include  'include/datatype.inc'
      include  'include/library.inc'
C
C   292     6   (D)     "Digitise ON"
C   293     6   (D)     "Digitise OFF"
C   294     8   (S)     "Define"
C
      REAL WX1,WY1,TSLA
C
      INTEGER*2 MP,NCHARS,NLEN2,TJUST
      INTEGER*4 TMEN,TCELL,C,I,NLEN,DNUM,ST
      REAL TDTHGT,REAL
      DOUBLE PRECISION DN
      LOGICAL TDIG,OK,QUIT
      INTRINSIC REAL
C
      EXTERNAL DEPRNT,GTMCLO,INITMS
      EXTERNAL NLEN,CVERFY,CPRMXP,AEXPRN,GTMCWR
C
C********************************************************
C     PHONE  A D V E N T  S Y S T E M S
C     I'll have to do something about this some day. GCU
C********************************************************
 
      CBUFF='mouse.type'
      CALL FPARS(CBUFF,ST)
      INQUIRE (FILE=CBUFF,EXIST=OK)
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(433)
         CCMD='q'
         RETURN
      END IF
C     load digiise menu
      CALL MNLDIG()
      IF ( .NOT.OK ) GOTO 99
C
 10   CONTINUE
C     Read a cursor hit to select SELECT mode
      CALL TCURS(C,WX1,WY1)
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
      IF (MEN.EQ.3) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'s') THEN
C           select dimension option
            CALL GTCLRC(TMEN,TCELL)
            DIGIT=.NOT.DIGIT
c            IF (STRIDO.EQ.0) DIGIT=.FALSE.
            IF ( DIGIT ) THEN
C               intialise the the serial port for the digitiser
               CALL INITMS(OK)
               DNUM=292
            ELSE
               IF ( STRIDO.NE.0 ) CALL CLOSMS()
               DNUM=293
            END IF
            CALL GTDMEN(DNUM,3)
            GOTO 10
         ELSE IF (CCMD.EQ.'O') THEN
C           select define screen origin
            TDIG=DIGIT
            DIGIT=.TRUE.
            CALL SELDD1(WRLDX,WRLDY,QUIT)
            DIGIT=TDIG
         ELSE IF (CCMD.EQ.'o') THEN
C           select define digitise origin
            TDIG=DIGIT
            DIGIT=.TRUE.
            CALL SELDD2(DIGX,DIGY,QUIT)
            DIGIT=TDIG
         ELSE IF (CCMD.EQ.'A') THEN
C           select define horizontal alignment.
            TDIG=DIGIT
            DIGIT=.TRUE.
            CALL SELDD3(ANGDIG,QUIT)
            DIGIT=TDIG
         ELSE IF (CCMD.EQ.'S') THEN
C           select define digitise origin
            CALL SELDD4(14,SCADIG,QUIT)
         ELSE IF (CCMD.EQ.'G') THEN
C           select define digitise origin
            CALL SELDD4(431,DIGSNA,QUIT)
         ELSE IF (CCMD.EQ.'a') THEN
C           select define digitise origin
            CALL SELDD4(432,DIGANG,QUIT)
         ELSE IF (CCMD.EQ.CHAR(13)) THEN
C           select define digitise origin
            CALL SETDIG(WRLDX,WRLDY,DIGX,DIGY,SCADIG,ANGDIG)
         ELSE
C           unrecognized delete option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (MEN.EQ.2) GOTO 99
         IF (CCMD.EQ.'q' ) GOTO 99
C        ensure the entry cell is not hilited any more
         CALL DGWAIT(.3)
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         GOTO 10
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
C
 99   CONTINUE
C
      END
*
      SUBROUTINE SELDD1(WX,WY,QUIT)
C     =============================
C1                       R, R,  L
C1                       O, O,  O
C2    defines the origin on the drawing.
C
      include 'include/dig2wo.inc'
 
      INTEGER*4 C
      REAL WX,WY
      LOGICAL QUIT
 
C     Tell him to indicate the world origin
      CALL DCPRNT(426)
C     find and record coords
      CALL MNLPTS()
      CALL GETANS(C,WX,WY)
      CALL MNUPTS()
      QUIT=.FALSE.
 
      END
*
*
      SUBROUTINE SELDD2(WX,WY,QUIT)
C     =============================
C1                       R, R,  L
C1                       O, O,  O
C2    Defines the origin on the digitiser.
C
      include 'include/dig2wo.inc'
      include 'include/nbuff.inc'
      include 'include/vntable.inc'
C
      REAL WX,WY
      INTEGER*4 NLEN1,C
      LOGICAL QUIT
      EXTERNAL NLEN1
 
C     Tell him to indicate the digitiser origin
 10   CONTINUE
      WRITE(CBUFF,FMT='(A,I3)')
     +     DICT01(427)(1:NLEN1(DICT01(427))),DIGBUT
      CALL CPRINT(CBUFF)
C     find and record coords
      CALL SCURS(C,WX,WY)
      IF ( C-153.NE.DIGBUT) THEN
         CALL DEPRNT(430)
         GOTO 10
      END IF
 
      END
*
*
      SUBROUTINE SELDD3(ANGLE,QUIT)
C     =============================
C2    This subroutine sets the variable DIGIT provided
C2    the digitiser option has been selected.It is
C2    also used to set the scale, paper origin and
C2    screen origin.
C
      include 'include/dig2wo.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/vntable.inc'
C
      REAL X,Y,ANGLE,CANG
      REAL HORX1,HORY1,HORX2,HORY2
      INTEGER*4 TMEN,TCELL,C,NLEN1
      LOGICAL QUIT
C
      EXTERNAL CANG,NLEN1
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SELECT DIGITISER routine -------------
C     set variable to indicate that a digitiser is required
C
C     tell him to indicate horizontal
 20   CONTINUE
      WRITE(CBUFF,FMT='(A,I3)')
     +     DICT01(428)(1:NLEN1(DICT01(428))),DIGBUT
      CALL CPRINT(CBUFF)
C     record point coords
      CALL SCURS(C,HORX1,HORY1)
      IF ( C-153.NE.DIGBUT) THEN
         CALL DEPRNT(430)
         GOTO 20
      END IF
      WRITE(CBUFF,FMT='(A,I3)')
     +     DICT01(429)(1:NLEN1(DICT01(429))),DIGBUT
 30   CONTINUE
      CALL CPRINT(CBUFF)
C     record point coords
      CALL SCURS(C,HORX2,HORY2)
      IF ( C-153.NE.DIGBUT) THEN
         CALL DEPRNT(430)
         GOTO 30
      END IF
C
C     calculate angle of rotation required (in radians)
      ANGLE=CANG(HORX1,HORY1,HORX2,HORY2)
      QUIT=.FALSE.
C
      END
*
*
      SUBROUTINE SELDD4(DNUM,SCALE,QUIT)
C     ==================================
C1                        I4     R,   L
C1                         I     O    O
C
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/vntable.inc'
 
      REAL SCALE
      INTEGER*4 TMEN,TCELL,DNUM,NLEN
      DOUBLE PRECISION DN
      LOGICAL QUIT
      EXTERNAL NLEN
 
      TMEN=MEN
      TCELL=TMEN
  99  CONTINUE
      CALL DPRMXP(DNUM,CBUFF)
      QUIT=NLEN(CBUFF).EQ.0
      IF ( QUIT )THEN
C        user has returned zero length string
C        assume that he has change his mind and return for input
         CALL GTMCLO(MEN,CELLN)
      ELSE
C        evaluate an arithmetic expression from the keyboard
         CALL AEXPRN(CBUFF,DN,*99)
         SCALE=REAL(DN)
         CALL GTMCWR(TMEN,CCMD,SCALE,'(F6.3)')
      END IF
C
C
      END
*
*
      SUBROUTINE SELJ00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls operation of the SELECT JOURNAL function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNLSJ0,SELJ02,GTCLRM,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C     initialize SELECT JOURNAL menu
      CALL MNLSJ0()
C     call the select  routine
      CALL SELJ02()
C     clear option menu
      CALL GTCLRM(3)
C
C     ensure cell no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
      SUBROUTINE SELJ02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine SELJ02 is the main working routine
C2    for the SELECT JOURNAL function.
C
      include 'include/menun.inc'
C
      REAL X,Y
C
      INTEGER*4 C
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL SELD03,CPRINT,TCURS
C
C
 10   CONTINUE
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
      IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') RETURN
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL SELJ03(OK)
         GOTO 10
      ELSE IF ( MEN.EQ.0 ) THEN
c        117    "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SELJ03(OK)
C     =====================
C                       
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SELJ03 is the main working routine
C2    for the selection of JOURNALING
      include 'include/menun.inc'
      include 'include/journal.inc'
      include 'include/macro.inc'
C
      INTEGER*4 TMEN,TCELL
C
C
      LOGICAL OK,CVERFY
C
C
      EXTERNAL GTMCLO,GTCLRC,GTDMEN,CLSJRN,OPNJRN,COMJRN,
     +DEPRNT, GTDOMN
C
C
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processing here
      IF (CVERFY(CCMD,'Jj')) THEN
C***************************************************************
C                      JOURNALING OPTION                       *
C***************************************************************
C         toggle the journal cell but only if the MACROS
C         are not running
          IF(.NOT. MACOP) THEN
               IF (JOURON) THEN
C                 if the journaling option 
C                 is turned on turn it off
C                 close the journal file
                  CALL CLSJRN(OK)               
                  CALL GTDOMN(67,3)
                  OK = .TRUE.
               ELSE
C                 if the journaling option  
C                 is turned off then turn it on
C                 open the journal file               
                  CALL OPNJRN(OK)
                  IF(OK) THEN
                    CALL GTDOMN(68,3)
                    CALL GTDMCH(68,3)
                    OK = .TRUE.
                  ELSE
                    OK = .FALSE.
                    CALL GTMCLO(TMEN, TCELL)
                  ENDIF
               ENDIF 
          ENDIF
      ELSEIF (CCMD .EQ. 'i') THEN
C***************************************************************
C                      COMMENT  OPTION                         *
C***************************************************************
               IF (JOURON) THEN
C                 if the journaling option 
C                 is turned on ten prompt
C                 for a comment and punt it 
C                 out
                  CALL COMJRN(OK)               
               ELSE
C                 if the journaling option  
C                 is turned off then tell the 
C                 punter that it is not possible
C                 to output a comment
                  CALL DEPRNT(610)
                  OK = .FALSE.
               ENDIF 
               CALL GTMCLO(TMEN, TCELL)
C
      END IF
C
      END
      SUBROUTINE SELL00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    controls operation of the SELECT LANGUAGE function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SELL02
C
      TMEN=MEN
      TCELL=CELLN
C     enter the SELECT LANGUAGE routine
      CALL MNISL0()
C     go get selection
      CALL SELL02()
C     clear option menu
      CALL GTCLRM(3)
C     ensure cell no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     if not menu 2,then return to VERB menu
      IF (MEN.NE.2) THEN
C        return to VERB menu to show new language.
         CCMD='q'
      END IF
C
      END
C
      SUBROUTINE SELL02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine SELL02 is the main working routine
C2    for the SELECT LANGUAGE function.
C
      include 'include/menun.inc'
      include 'include/vntable.inc'
      include 'include/library.inc'
C
      REAL X,Y
      INTEGER*2 FONTID,NLEN2
      INTEGER*4 I,C,ST,NLEN
      LOGICAL CHANGE
      CHARACTER*80 PRMNAM,MENNAM,TXNAM,DRWNAM
      EXTERNAL SELL03,TCURS,DCPRNT,NLEN2
C
C     set default menu and prompt file names
CAPOLLO|SUN
      PRMNAM='dict/p.000'
      CALL FPARS(PRMNAM,ST)
      MENNAM='dict/m.000'
      CALL FPARS(MENNAM,ST)
      TXNAM ='dict/crypt.000'
      CALL FPARS(TXNAM,ST)
      DRWNAM='dict/font.000'
      CALL FPARS(DRWNAM,ST)
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.000'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.000'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.000'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.000'
CPC386
      CHANGE=.FALSE.
C
 10   CONTINUE
      CALL DCPRNT(63)
      CALL TCURS(C,X,Y)
      IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') RETURN
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         IF (CCMD.NE.CHAR(13)) THEN
C           option selected
            CALL SELL03(PRMNAM,MENNAM,TXNAM,DRWNAM,CHANGE)
            GOTO 10
         ELSE
            IF (CHANGE) THEN
C              tell him to wait
               CALL DCPRNT(66)
C              menu text in selected lanuage
               CALL LODVNT(MENNAM,ST)
               IF (ST.NE.0) THEN
C                 error in menu file
                  CALL DEPRNT(75)
                  CHANGE=.FALSE.
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               END IF
C              prompt text in selected lanuage
               CALL LODPRT(PRMNAM,ST)
               IF (ST.NE.0) THEN
C                 error in prompt file
                  CALL DEPRNT(65)
                  CHANGE=.FALSE.
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               END IF
C              load apollo font
CAPOLLO
               CALL GPR_$LOAD_FONT_FILE(TXNAM,NLEN2(TXNAM),FONTID,ST)
CAPOLLO
CSUN|PC386 - Not implemented yet, force error. GCU
C              ST=-1
CSUN|PC386
               IF ( ST.NE.0 ) THEN
                  CHANGE=.FALSE.
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               END IF
 
C              Activate text font
CAPOLLO
               CALL GPR_$SET_TEXT_FONT(FONTID,ST)
CAPOLLO
               IF ( ST.NE.0 ) THEN
                  CHANGE=.FALSE.
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               END IF
C
               CALL LODFNT(DRWNAM,ST)
               IF (ST.NE.0) THEN
                  CHANGE=.FALSE.
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               END IF
C
               CALL GTCLRM(4)
               CALL GTCLRM(1)
            ELSE
C              no change made
               CALL DEPRNT(74)
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
         END IF
      ELSE IF ( MEN.EQ.0 ) THEN
c        117    "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SELL03(PRMNAM,MENNAM,TXNAM,DRWNAM,CHANGE)
C     ====================================================
C1    vartype             C*80    C*80 C*80   C*80     L
C1    iostatus             IO      IO   IO    IO       IO
C
      include 'include/menun.inc'
      include 'include/library.inc'
C
      LOGICAL CHANGE,PROK,MNOK,TXOK,DROK,OK
      CHARACTER*80 PRMNAM,MENNAM,TXNAM,DRWNAM,
     +             PRMNM2,MENNM2,TXNAM2,DRWNA2
      CHARACTER*200 OUTS,INS*80,LEXT*3
      CHARACTER*80 WILD
      INTEGER*4 NLEN,UNITN,I,ST,ST1,ST2,ST3,ST4
      INTEGER*2 N
      EXTERNAL GTHIMC,NLEN
C
C     Highlight option that has been selected
      CALL GTHIMC(MEN,CCMD,'EFGIS',CELLN)
 
 
      GOTO (10,20,30,40,50) INDEX('EFGIS',CCMD)
 
 
 10   CONTINUE
C*******************************************************
C                                                English
C*******************************************************
CAPOLLO|SUN
        LEXT = '044'
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.044'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.044'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.044'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.044'
CPC386
 
      GOTO 100
 20   CONTINUE
C*******************************************************
C                                                 French
C*******************************************************
CAPOLLO|SUN
         LEXT = '033'
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.033'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.033'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.033'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.033'
CPC386
 
      GOTO 100
 
 30   CONTINUE
C*******************************************************
C                                                 German
C*******************************************************
CAPOLLO|SUN
         LEXT = '049'
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.049'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.049'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.049'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.049'
CPC386
      GOTO 100
 40   CONTINUE
C*******************************************************
C                                                Italian
C*******************************************************
CAPOLLO|SUN
         LEXT = '039'
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.039'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.039'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.039'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.039'
CPC386
 
      GOTO 100
 50   CONTINUE
C*******************************************************
C                                                Spanish
C*******************************************************
CAPOLLO|SUN
         LEXT = '034'
CAPOLLO|SUN
CIBM
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\p.034'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\dict\m.034'
CIBM
CPC386
C      PRMNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\p.034'
C      MENNAM=LIBRARY(1:NLEN(LIBRARY))//'\\dict\\m.034'
CPC386
 
 100  CONTINUE
C     test for existance of files
      PRMNM2='dict/p.'//LEXT
      CALL FPARS(PRMNM2,ST1)
      MENNM2='dict/m.'//LEXT
      CALL FPARS(MENNM2,ST2)
C
C     Note: the following has been removed because fpars uses the inquire 
C           statement to test for existance however from 9.6 onwards the os
C           cannot detect the occurance so I have manually added the library
C           name to the text and hope for the best.
c      TXNAM2='dict/crypt.'//LEXT
c      CALL FPARS(TXNAM2,ST3)
      TXNAM2 = LIBRARY(1:NLEN(LIBRARY))//'dict/crypt.'//LEXT
      ST3 = 0
      DRWNA2='dict/font.'//LEXT
      CALL FPARS(DRWNA2,ST4)
CAPOLLO
 
      OK = (ST1+ST2+ST3+ST4.EQ.0)
CAPOLLO
CIBM|SUN|PC386
C      INQUIRE(FILE=PRMNM2,EXIST=PROK)
C      INQUIRE(FILE=MENNM2,EXIST=MNOK)
C      INQUIRE(FILE=TXNAM2,EXIST=TXOK)
C      INQUIRE(FILE=DRWNA2,EXIST=DROK)
C      OK=PROK.AND.MNOK.AND.TXOK.AND.DROK
CIBM|SUN|PC386
C
      IF (OK) THEN
C        ensure new file names are loaded
         PRMNAM=PRMNM2
         MENNAM=MENNM2
         TXNAM=TXNAM2
         DRWNAM=DRWNA2
         CHANGE=.TRUE.
      ELSE
C        cannot find files
C        tell him,and cancel selection
         CALL DEPRNT(62)
         CALL GTMCLO(MEN,CELLN)
      END IF
C
      END
*
      SUBROUTINE SELM00()
C     =====================
C     no arguments required
C2    Main working routine for the SELECT MENU option
C2    created 14/7/88 - mlw
C
      include 'include/menun.inc'
      include 'include/menug.inc'
C      include 'INCLUDE/ENTITY.INC'
C      include 'INCLUDE/MASTI.INC'
C      include 'INCLUDE/NBUFF.INC'
C      include 'INCLUDE/NDATA.INC'
C      include 'INCLUDE/MOVDAT.INC'
      include 'include/dig2wo.inc'
      include 'include/datatype.inc'
      include 'include/vntable.inc'
      include 'include/library.inc'
C
      REAL WX1,WY1,TSLA
C
      CHARACTER*80 CBUFF,MNAM
      INTEGER*2 X,Y,MP,NCHARS,NLEN2,TJUST
      INTEGER*4 TMEN,TCELL,C,I,NLEN,DNUM,ST
      REAL TDTHGT,REAL,MOX,MOY,MANG
      DOUBLE PRECISION DN
      LOGICAL TDIG,OK,QUIT
      INTRINSIC REAL
C
      EXTERNAL DEPRNT,GTMCLO,INITMS
      EXTERNAL NLEN,CVERFY,CPRMXP,AEXPRN,GTMCWR
C
C     load menu
      CALL MNLMEN()
C
C     first have to initialise the digitiser for use
      CBUFF='mouse.type'
      CALL FPARS(CBUFF,ST)
      INQUIRE (FILE=CBUFF,EXIST=OK)
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(433)
         CCMD='q'
         RETURN
      END IF
      IF ( .NOT.OK ) GOTO 99
C
 10   CONTINUE
      DNUM=565
      CALL CPRINT(DICT01(DNUM))
C     Read a cursor hit to select SELECT mode
      CALL TCURS(C,X,Y)
      IF (MEN.EQ.0) THEN
          DNUM=117
          CALL EPRINT(DICT01(DNUM))
          GOTO 10
      END IF
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     check for option
      IF (MEN.EQ.3) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'s') THEN
C            check a menu file has been loaded
             IF (.NOT.MENOK) THEN
                 CALL INITMS(OK)
                 CALL MENINT()
                 DNUM=488
             ELSE
                 IF ( STRIDO.NE.0 ) CALL CLOSMS()
                 CALL GTDMEN(DNUM,3)
                 DNUM=489
             END IF
C            switch the menu ON/OFF
             MENOK=.NOT.MENOK
             CALL GTCLRC(TMEN,TCELL)
             CALL GTDMEN(DNUM,3)
             GOTO 10
         ELSE IF (CCMD.EQ.'n') THEN
C           wants to load a menu file
            CALL MENLOD()
            CALL MNLMEN()
         ELSE IF (CCMD.EQ.'S') THEN
C           set up menu on digitiser
            CALL MENSET()
         ELSE IF (CCMD.EQ.CHAR(13)) THEN
            IF (.NOT.MFILOK) THEN
                DNUM=563
                CALL EPRINT(DICT01(DNUM))
                CALL GTMCLO(TMEN,TCELL)
                GOTO 10
            END IF
C            IF (.NOT.MSETOK) THEN
C                DNUM=564
C                CALL EPRINT(DICT01(DNUM))
C                CALL GTMCLO(TMEN,TCELL)
C                GOTO 10
C            END IF
C           save the menu parameters
C           save the new orientation in the file
          IF(MSETOK) THEN
               MNAM = DMFILE
               CALL MENSAV(MNAM)
C      
               MNAM = 'DAXCAD.DMF'
c              CALL FPARS(MNAM,ST)
              CALL MENSAV(MNAM)
          END IF
         ELSE
C           unrecognized option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C
         IF (MEN.EQ.2) GOTO 99
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
 30      CONTINUE
C        ensure the entry cell is not hilited any more
         CALL DGWAIT(.3)
         CALL GTMCLO(TMEN,TCELL)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
      SUBROUTINE SELPN0()
C     ===================
C1    no arguments required
C
C2    This routine controls the pathname select option
C
      include 'include/menun.inc'
      include 'include/gtxt2.inc'
      include 'include/macro.inc'
      include 'include/vntable.inc'
      CHARACTER TOKEN*1,TEXT*48
      INTEGER*4 I,C,SUBSC,ST,NLEN,PATH
      LOGICAL OK
      REAL X,Y
      EXTERNAL INQFS1,EPRINT,GTMCHI,GTMCLO,GTCLRC,GTPMEN,DEPRNT
     +         MNLPN0,NLEN,VALDIR,DCPRNT
C     Initialise the variables
      TOKEN='n'
      MEN=3
      CELLN=1
C     increased for backcloth, 5 -> 6 ja
      PATH = 6
C     Load the menu cells n menu 3.
      CALL MNLPN0()
C     Write the current pathname list under each cell.
      DO 10 I=3,3*PATH,3
        CALL GTPMEN(PATHN(I/3),TOKEN,3,I-1)
10    CONTINUE
C     Get the new pathname if digitised.
20    CONTINUE
      CALL GTMCLO(MEN,CELLN)
      CALL TCURS (C,X,Y)
      IF(CCMD.EQ.'Q'.OR.CCMD.EQ.'q'.OR.CCMD.EQ.CHAR(150)
     +   .OR.MEN.EQ.2) GOTO 200
C     Has one of the valid cells been digitised.
      IF(CCMD.EQ.'m'.OR.CCMD.EQ.'p'.OR.CCMD.EQ.'c'.OR.
     +   CCMD.EQ.'s'.OR.CCMD.EQ.'d'.OR.CCMD.EQ.'b'
     +   .OR.CCMD.EQ.'M') THEN
         CALL GTMCHI(MEN,CELLN)
         SUBSC=(CELLN+2)/3
         CALL DPRMXP(480,TEXT)
C        Check for a  backslash in the name and do it.
         I=NLEN(TEXT)
         IF (I.GT.0.AND.I.LT.48) THEN
            IF(TEXT(I:I).NE.'/') TEXT(I+1:I+1)='/'
         ELSEIF (I.EQ.48) THEN
            CALL DEPRNT(481)
            GOTO 20
         ELSEIF(I.EQ.0 ) THEN
             TEXT = './'
             I = 2
         ENDIF
C        The routine will test the whether a valid directory
C        has been selected The error message will also be
C        generated at that level.
         CALL VALDIR(TEXT,OK)
         CALL DCPRNT(38)
         IF(OK) THEN
            PATHN(SUBSC)=TEXT
         ELSE
            CALL DEPRNT(280)
            GOTO 20
         ENDIF
C        clear out the exixting cell and write in the new pathname
         CALL GTCLRC(3,(SUBSC*3)-1)
         CALL GTPMEN(PATHN(SUBSC),TOKEN,3,(SUBSC*3)-1)
      ELSEIF(CCMD.EQ.'n') THEN
C        Display the full pathname if digitised
         SUBSC=(CELLN+1)/3
         CALL CPRINT(DICT01(482)(:NLEN(DICT01(482)))//PATHN(SUBSC))
CIBM
C      ELSE IF(CCMD.EQ.'f') THEN
C         CALL GTMCLO(MEN,CELLN)
C         MFILE=.NOT.MFILE
C         IF ( MFILE ) THEN
C            CALL GTDMEN(336,3)
C         ELSE
C            CALL GTDMEN(337,3)
C         ENDIF
CIBM
      ELSE
          CALL DEPRNT(117)
      ENDIF
      GOTO 20
200   CONTINUE
      END
C
      SUBROUTINE SELS00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls the SELECT DIMENSION STANDARD function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SELS02
C
      TMEN=MEN
      TCELL=CELLN
C     initialize SELECT DIMENSION menu
      CALL MNLSS0()
C     call the select dim routine
      CALL SELS02()
C     clear option menu
      CALL GTCLRM(3)
C     ensure cell no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
*
      SUBROUTINE SELS02()
C     ===================
C1    no arguments required
C2
C2    controls operation of the SELECT STANDARD option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/dimendat.inc'
C
      INTEGER CP,C,TMEN,TCELL
      REAL X,Y
      LOGICAL CVERFY
      EXTERNAL TCURS,GTMCLO,CVERFY
      EXTERNAL ISTD10,CPRINT,GTHIMC
C
 10   CONTINUE
C     Tell him to select a menu option
      CALL DCPRNT(38)
C     find action required
      CALL TCURS(C,X,Y)
C
 11   CONTINUE
C     if another major option,return to SELECT control routine
      IF (MEN.EQ.2) GOTO 605
C     select type of standard action required
      IF (MEN.EQ.3) THEN
 105     CONTINUE
         IF ( CVERFY(CCMD,'BDA') ) THEN
C           parameter change required
C           Highlight option that has been selected
            CALL GTHIMC(MEN,CCMD,'BDA',CELLN)
C           go handle the change selected
            CALL ISTD10()
C           Switch off finished with alteration
            GOTO 10
         ELSE IF ( MEN.EQ.0 ) THEN
            CALL DEPRNT(117)
            GOTO 10
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(216)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C
      END IF
C     test for quit character
      IF ( CCMD.EQ.'q') GOTO 605
      IF (MEN.NE.0) GOTO 11
C     go try again
      GOTO 10
C
 605  CONTINUE
C
      END
*
      SUBROUTINE SELT00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls operation of the SELECT TEXT function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,SELT02
C
      TMEN=MEN
      TCELL=CELLN
C     initialize SELECT TEXT menu
      CALL MNIST0()
C     call the select text routine
      CALL SELT02()
C     clear option menu
      CALL GTCLRM(3)
C
C     ensure cell no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
      SUBROUTINE SELT02()
C     ===================
C1    no arguments required
C2
C2    controls operation of the SELECT TEXT option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL
      REAL X,Y
      LOGICAL CVERFY
      EXTERNAL TCURS,GTMCLO,CVERFY
      EXTERNAL INST10,CPRINT,GTHIMC
C
 10   CONTINUE
C     Tell him to select a menu option
      CALL DCPRNT(38)
C     find action required
      CALL TCURS(C,X,Y)
C
 11   CONTINUE
C     if another major option,return to SELECT control routine
      IF (MEN.EQ.2) GOTO 605
C     select type of text action required
      IF (MEN.EQ.3) THEN
 105     CONTINUE
         IF ( CVERFY(CCMD,'KLAHWV') ) THEN
C           parameter change required
C           Highlight option that has been selected
            CALL GTHIMC(MEN,CCMD,'KLAHWV',CELLN)
C           go handle the text status change selected
            CALL INST10()
C           Switch off finished with alteration
            CALL GTMCLO(MEN,CELLN)
            GOTO 10
         ELSE IF ( MEN.EQ.0 ) THEN
c           117    "Invalid area of screen"
            CALL DEPRNT(117)
            GOTO 10
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(216)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C
      END IF
C     test for quit character
      IF ( CCMD.EQ.'q') GOTO 605
      IF (MEN.NE.0) GOTO 11
C     go try again
      GOTO 10
C
 605  CONTINUE
C
      END
C
      SUBROUTINE SETDIG(WX,WY,DGX,DGY,SCALE,ANGLE)
C     ====================================================	
C2    This subroutine sets the transformation for the digitiser 	
C2    to world coordinates.
C
      include 'include/dig2wo.inc'
      include 'include/params.inc'
C
      REAL SCALE,ZX,ZY,WX,WY,DGX,DGY,ANGLE,WX1,WY1,WX2,WY2
      REAL A(3,3),B(3,3),C(3,3),SCALX,SCALY
C
      EXTERNAL MULT3M,SCAL2D,TRAN2D,I3M,ROT2D
C
C2    DBUFAC is the conversion factor for current DB units
C
      CALL I3M(A)
      CALL I3M(B)
      CALL I3M(C)
      CALL I3M(DIGWXY)
C
C     set scaling factor
      SCALX=DRES/SCALE/(DBUFAC*10.0)
      SCALY=SCALX
C
C     translate  world window to world origin where
C     -DIGX & -DIGY are points to go to
      CALL TRAN2D(-DGX,-DGY,A)
C
      CALL ROT2D(-ANGLE,B)
C
      CALL MULT3M(A,B,C)
C
C     scale to viewport size
      CALL SCAL2D(SCALX,SCALY,B)
C
      CALL MULT3M(C,B,A)
C
      CALL TRAN2D(WX,WY,B)
C
      CALL MULT3M(A,B,DIGWXY)
C
      WDIGS =DIGSNA/SCALE
      WDIGAN=DIGANG/SCALE
C
      END
C
C-------------------------------------------------------------
C
 
      SUBROUTINE STDFLS()
C     ===================
C1    no arguments required
C2
C2    controls operation of the SELECT STANDARD option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include  'include/dimendat.inc'
 
       BS=.FALSE.
       DIN=.FALSE.
       ANSI=.FALSE.
 
       END
C
