C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 ap.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DAXCAD(XVER)
C     SUBROUTINE INITMN()C
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE daxcad(XVER,DEMO,DAXSERV,SMAC,SMACLEN,CURSOR,TRACK,
     +                  REDRAW_INT)
C     ==============================================================
C1    VARYPE              L   L      L     C*1024 I4     I4*3  L
C1    IOSTAT              I   I      I      I    I        i    I
C
C     ==============================================================
C1    VARYPE               L   
C1    IOSTAT               I  
C
C2    Main DAXCAD operating routine. Does all Initalisation
C2    and control cursor control with MACROS
C2  
C2    Arguments:-
C2  
C2    XVER        ->          If TRUE then DAXCAD X version is running otherwise
C2                            APOLLO GPR version running.
C2  
C2    DEMO        ->          Sets demo version
C2    DAXSERV     ->          Sets daxcad as server ( NO graphics )
C2    SMAC        ->          Server macro to run
C2    SMACLEN     ->          Length of macro name
C2    CURSOR      ->          Cursor number and length
C2    TRACK       ->          Defines whether cursor will track
C2                            on screen
C2    TRACK       ->          Defines whether cursor will track
C2                            on screen
C2    REDRAW_INT  ->          TRUE if redraw keyboard interrupt 
C2                            allowed
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include  'include/style.inc'
      include  'include/masti.inc'
      include  'include/props.inc'
      include  'include/menun.inc'
      include  'include/swind.inc'
      include  'include/nbuff.inc'
      include  'include/wtov.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/filunit.inc'
      include  'include/lfont.inc'
      include  'include/apollo.inc'
      include  'include/vntable.inc'
      include  'include/movdat.inc'
      include  'include/dimendat.inc'
      include  'include/macro.inc'
      include  'include/lfu.inc'
      include  'include/product.inc'
      include  'include/library.inc'
      include  'include/ftypes.inc'
      include  'include/faults.inc'
      include  'include/cross.inc'
      include  'include/krap.inc'
      include  'include/menpop.inc'
      include  'include/viewport.inc'
      include  'include/vpgrid.inc'
      include  'include/daxcad_x.inc'
      include  'include/server.inc'
      include  'include/cursor.inc'
C
      INTEGER*4 C,I,NLEN,ST,J,NN(4)
      INTEGER*4 INTWIN(4)
      INTEGER*4 LEN1
      INTEGER*4 LEN2
      INTEGER*4 LEN3
      INTEGER*2 LENGTH
      INTEGER*2 DSPCNT
      INTEGER*4 SMACLEN
      INTEGER*4 CURSOR(3)
      REAL X,Y
      LOGICAL OK,YESOK,MSFST,COK,GO,EX,XVER,REPAINT,YES
      LOGICAL DEMO
      LOGICAL TRACK
      LOGICAL DAXSERV
      LOGICAL SERVERMAC
      LOGICAL REDRAW_INT
      CHARACTER ANS*3,TEMP*100,CTMP*80
      CHARACTER TITLE*512
      CHARACTER SMAC*1024
      CHARACTER MACTMP*80
      EXTERNAL BELL,CLEAR,CPRINT,DCPRNT,DEPRNT,DIAG,DPRMXP,
     +         GINIT,GTCLRM,GTMCLO,INCALC,INITDB,INITMN,INMAC,INTPLT,
     1         KILLWF,LEVEL1,LFSETS,LOAD2,MAC100,MAJAT1,MAJCHG,MAJCR1,
     2         MAJDF1,MAJDIM,MAJDL1,MAJEX1,MAJIN1,MAJME1,MAJMV1,MAJPLT,
     3         MAJRED,MAJREG,MAJSEL,MAJTRM,MAJWRT,MAJXX1,MAKDHD,MAKFHD,
     4         MAKNHD,MAKPHD,MAKVHD,MNICAL,MNIDIS,MNIMAS,OPENNM,OPENWF,
     5         OURSCR,REGEND,SAVALL,SETPLT,STINIT,STORE2,TCURS,
     6         WORLD,YESOK,NLEN,MAKRPT,INATTX
C
C
      XVERSION = XVER
      SERVER = DAXSERV
      REDRAW_INTERRUPT = REDRAW_INT
C
C     Intialise the database
      CALL STINIT()
C     call protection and diagnostics
      CALL DIAG1(DEMO)
C
      CTMP=PRODUCT
      CALL FOLDUP(CTMP)
      WRITE(*,'(2A)') 'LOADING ',CTMP(1:NLEN(CTMP))
C
C     initiales menu
      CALL INITMN()
C
C     Set cursor size and tracking mode
C
      DAXCURS(1) = CURSOR(1)
      DAXCURS(2) = CURSOR(2)
      DAXCURS(3) = CURSOR(3)
      DAXTRACK = TRACK
C
      IF ( .NOT.SERVER ) THEN
C         initialize window management,and main data space
          CALL GINIT(I)
          SERVERMAC = .FALSE.
      ELSE
          CALL SERVERINIT()
          SERVERMAC = .TRUE.
      ENDIF

      

C     set the workspace size for transformation
C     make it 3% bigger then the paper mapping
      CALL WORLD(WPXMIN*1.03,WPYMIN*1.03,WPXMAX*1.03,WPYMAX*1.03)
C
C     clear the data pointers to zero
      CALL INITDB()
C
C     initialise attribute text data
      CALL INATTX()
C
C     set PAPTOW ratio
      PAPTOW=1/DRWSCL*PAPFAC/DBUFAC
C     initialise macro variables.
      CALL INMAC()
C
C     initialize plotter data space
      CALL SETPLT()
      CALL INTPLT()
C     initialise viewporting stuff
C
C     initalise the popup code.
      CALL INTPOP()

C
C		Other Stuff we can set up
C
		CEDIT = .FALSE.
		LEDIT = .FALSE.
C

C     test for existance of temporary workfile
C     ask for recovery instructions if found.
      INQUIRE(FILE=PRNAM(1:NLEN(PRNAM))//'.drg',EXIST=OK)
      INQUIRE(FILE=PRNAM(1:NLEN(PRNAM))//'.save.drg',EXIST=COK)
      IF (OK.OR.COK) THEN
C        must have been a crash,temp file exists.
 676     CONTINUE
         CALL BELL()
         GO = .FALSE.
         IF(COK) THEN
C           crash file exists
            CALL DCPRNT(520)
            CALL CONFIRMATION(DICT01(355),.TRUE.,GO)
            CRASHF = COK
C            CALL DPRMXP(355,ANS)
C            IF (NLEN(ANS).EQ.0  ) GOTO 676
C            GO = YESOK(ANS)
         ENDIF 
         IF(.NOT.GO.AND.OK) THEN
C            no crash file an secure file
             CALL DCPRNT(342)
C             CALL DPRMXP(355,ANS)
C             IF (NLEN(ANS).EQ.0  ) GOTO 676
            CALL CONFIRMATION(DICT01(355),.TRUE.,GO)
         ENDIF
c        set crash status
         IF ( GO ) THEN
            CALL LOAD2(.TRUE.)
            CALL MNIMAS()
            CALL REGEND()
            WRITE(UNIT=TEMP,FMT='(A,A)') 'Drawing name=',
     +      DRGNAM(1:NLEN(DRGNAM))
            CALL CPRINT(TEMP)
            DFNAM=DRGNAM
C           delete the crash file 
            IF(CRASHF) CALL KILLWF()
         ELSE
C           delete the workfile.
            CALL KILLWF()
            GOTO 675
         END IF
C        start normal drawing ops.
         GOTO 605
      END IF
C
      INQUIRE(FILE=PRNAM(1:NLEN(PRNAM))//'.mac',EXIST=OK)
C
      IF ( OK ) THEN
           CALL MAC100()
      ELSE
           IF ( SERVERMAC ) THEN
              IF (SMACLEN.EQ.0) THEN
                   WRITE(*,'(A)') 
     +             '[SERVER] Macro filename must be supplied'
                  STOP
              ELSE
                  MACTMP = ' '
                  MACTMP = SMAC(1:SMACLEN)
                  INQUIRE(FILE=MACTMP,EXIST=OK)
                  IF ( .NOT.OK) THEN
                       WRITE(*,'(A)') 
     +               '[SERVER] Macro filename does not exist'
                       CALL SABORT()

                  ELSE
                      WRITE(*,'(2A)')
     +               '[SERVER] Starting using: ',MACTMP
                      CALL MACRUN(MACTMP)
                  ENDIF
              ENDIF
           ENDIF
      ENDIF
 675  CONTINUE
C
      WRITE(TITLE,'(A,X,F5.2,A)') PRODUCT(1:NLEN(PRODUCT))
     +                            ,ROOTRV,' :: No Drawing Loaded'

      LENGTH = NLEN(TITLE)
      CALL GPR_$SET_TITLE(0,TITLE,LENGTH,ST)
C     not yet into drawing system
      DRAWNG = .FALSE.



C     Turn the display menu on the middle mouse button off.
      DSPMNU = .FALSE.                      
C     Turn the viewport stuff on the Right hand mouse button off.
      VPSMNU = .FALSE.                      
C
C     clear the data pointers to zero.
      CALL INITDB()
C     ensure backcloth space is cleared
      CALL FREBAK()
C
C     ensure display menu cleared.
      CALL GTCLRM(4)
      CALL GTCLRM(1)
C     clear the drawing area.
      IF ( .NOT.SERVER ) THEN
          CALL CLEAR()
      ENDIF
C
      IF ( XVERSION ) THEN
         WRITE(TEMP,'(A,F5.2,A)') CTMP(1:NLEN(CTMP))//
     +                            ' (Revision',ROOTRV,
     +') (X)'
      ELSE
         WRITE(TEMP,'(A,F5.2,A)') CTMP(1:NLEN(CTMP))//
     +                            ' (Revision',ROOTRV,
     +')'
      ENDIF


      CALL CPRINT(TEMP)
      TEMP=' '
C
C     initialize line fonts
      CALL LFSETS()
C
C     go to initialize mode
      CALL LEVEL1()
C
      CALL MAKFHD()
      CALL MAKDHD()
      CALL MAKVHD()
      CALL MAKPHD()
      CALL MAKNHD()
      DISLAY=.FALSE.
C
C     now draw any veiwports if thay have been recovered
      IF(MVPACT.AND..NOT.MAWS) THEN
          CALL WNPNTZ()
      ENDIF
C     save current world coords
      VIEWPS(1,1) = WXMIN
      VIEWPS(2,1) = WYMIN
      VIEWPS(3,1) = WXMAX
      VIEWPS(4,1) = WYMAX
C     save into arrays
      CALL TRNSAV(CVPN)
      CALL SAVLAY(CVPN,OK)
      CALL SAVGRD(CVPN,OK)
C     now start the normal drawing operations.
 605  CONTINUE
C
         WRITE(TITLE,'(A,X,F5.2,2A)') PRODUCT(1:NLEN(PRODUCT)),ROOTRV,
     +   ' :: Drawing Loaded > ',DRGNAM(1:NLEN(DRGNAM))
      LENGTH = NLEN(TITLE)
      CALL GPR_$SET_TITLE(0,TITLE,LENGTH,ST)
C
C     Show the main menu to get the bloke started
      CALL MNIMAS()
C     load calculator cell
      CALL MNICAL()
C     Enter the display menu options.
      CALL MNIDIS
C
C     Turn the display menu on the middle mouse button on.
      DSPMNU = .TRUE.
C     Turn the viewport menu on the Right hand mouse button on.
      VPSMNU = .TRUE.
C
C     Now into drawing menu system
      DRAWNG = .TRUE.
 604  CONTINUE
C
C     Let him hit something, and find out what he wants to do.
      CALL TCURS(C,X,Y)

C      PRINT*, 'COMMAND SELECTED',CCMD
C
C     The returned command character from the main menu.
C     directs us to the next option.
C     if quit char go back get another command.
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         CALL GTMCLO(MEN,CELLN)
         GOTO 604
      END IF
C
      IF ( CCMD .EQ. 'D' ) THEN
C
C        The letter 'D' is a request for the DELETE option *****
         CALL MAJDL1()
C
      ELSE IF ( CCMD .EQ. 'a' ) THEN
C
C        The letter 'a' is a request for the ATTACH option *****
         CALL MAJAT1()
      ELSE IF ( CCMD .EQ. 'd' ) THEN
C
C        The letter 'd' is a request for the DIMENSION option *****
         CALL MAJDIM()
C
C      ELSE IF ( CCMD .EQ. 'X' ) THEN
C       run AutoDAX so release display and reacquire
C	CALL RELDIS(DSPCNT)
C	CALL GENACAD()
C	CALL ACRDIS(DSPCNT)
C
      ELSE IF ( CCMD .EQ. 'e' ) THEN
C
C        The letter 'e' is a request for the DEFINE option *****
         CALL MAJDF1()
C
      ELSE IF ( CCMD .EQ. 'E' ) THEN
C
C        The letter 'E' is a request for the EXPLODE option *****
         CALL MAJEX1()
C
      ELSE IF ( CCMD .EQ. 'm' ) THEN
C
C        The letter 'm' is a request for the MEASURE  option *****
         CALL MAJME1()
C
      ELSE IF ( CCMD .EQ. 't' ) THEN
C
C        The letter 't' is a request for the TRIM  option *****
         CALL MAJTRM()
C
      ELSE IF ( CCMD .EQ. 'A' ) THEN
C
C        call the major option CHANGE control routine
         CALL MAJCHG()
C
      ELSE IF ( CCMD .EQ. 'I' ) THEN
C
C        call the major option INSERT control routine
         CALL MAJIN1()
C
C        reset parameter data for creation of parts etc.
         CALL MAKFHD()
         CALL MAKDHD()
         CALL MAKVHD()
         CALL MAKPHD()
         CALL MAKNHD()
      ELSE IF ( CCMD .EQ. 'O' ) THEN
C
C        call the major option EXTRACT control routine
         CALL MAJXX1()
C
      ELSE IF ( CCMD .EQ. 'P' ) THEN
C     ***** The letter 'P' is a request for MOVE GEOMETRY *****
C        call the major option MOVE control routine
         CALL MAJMV1()
C
      ELSE IF ( CCMD .EQ. 'p' ) THEN
C     ***** The letter 'p' is a request for PLOT  *****
C        call the major option PLOT control routine
          IF(DEMO) THEN
              CALL    ERRORDIALOG(
     + 'This is a DEMO version and cannot PLOT')
              CALL GTMCLO(MEN,CELLN)
              GOTO 604
          ENDIF
         CALL MAJPLT()
C
      ELSE IF ( CCMD .EQ. 'V' ) THEN
C     ***** The letter 'V' is a request for CREATE *****
C     call the major option CREATE control routine
         CALL MAJCR1()
C
      ELSE IF ( CCMD .EQ. 'R' ) THEN
C     ***** The letter 'R' is a request for READ *****
C        call the major option READ control routine
         CALL MAJRED()
C
      ELSE IF ( CCMD .EQ. 'W' ) THEN
C     ***** The letter 'W' is a request for WRITE *****
C     call the major option WRITE control routine
          IF(DEMO) THEN
              CALL    ERRORDIALOG(
     + 'This is a DEMO version and cannot write')
              CALL GTMCLO(MEN,CELLN)
              GOTO 604
          ENDIF
         CALL MAJWRT()
C
      ELSE IF ( CCMD .EQ. 'Y' ) THEN
C     ***** The letter 'Y' is a request for SPECIAL *****
C     call the major option SPECIAL control routine
         CALL MAJSPC()
C
      ELSE IF (CCMD.EQ.'x') THEN
C     ***** The letter 'x' is a request for  *****
C
C        restore a named file
         CALL LOAD2(.FALSE.)
         CALL MNIMAS()
         CALL REGEND()
C
      ELSE IF (CCMD.EQ.'z') THEN
C     ***** The letter 'z' is a request for SECURE *****
C
          IF(DEMO) THEN
              CALL    ERRORDIALOG(
     + 'This is a DEMO version and will not SECURE')
              CALL GTMCLO(MEN,CELLN)
              GOTO 604
          ENDIF
C        save to the work file
         CALL STORE2(.TRUE.)
C
      ELSE IF ( CCMD .EQ. 'S' ) THEN
C     ***** The letter 'S' is a request for SELECT *****
C
         CALL MAJSEL()
C
      ELSE IF (CCMD.EQ.'T') THEN
C     ***** The letter 'T' is a request for REGENERATE *****
         CALL MAJREG()
C
      ELSE IF ( CCMD .EQ. 'Z' ) THEN
C     ***** The letter 'Z' is a request for MACRO *****
C
C        Macro processing.
         CALL MAC100()
C
         GOTO 604
C                                                       
      ELSE IF ( CCMD .EQ. 'F' ) THEN
          IF(DEMO) THEN
              CALL    ERRORDIALOG(
     + 'This is a DEMO version and cannot save')
              GOTO 675
          ENDIF
C     ***** The letter 'F' is a request for FINISH *****
C        save the drawing
C - This is for a demo version which wont save drawings !!!!
C         IF ( MACOP ) GOTO 675
C
 776     CONTINUE
C
C        Force drawing type to DRAWING
C
         DAXTYP = DAXDRG
C
C        Ask if he wants to save drawing
C
         DFNAM=DRGNAM
         LEN1 = NLEN(DICT01(313))
         LEN2 = NLEN(DFNAM)
         IF(LEN1+LEN2 .GT. 80 ) THEN
              LEN3 = LEN2 - (80-LEN1)
                  
              WRITE(TEMP,FMT='(A,X,A)') DICT01(313)(1:LEN1),
     +            DFNAM(LEN3:LEN2)
            ELSE
              WRITE(TEMP,FMT='(A,X,A)') DICT01(313)(1:LEN1),
     +            DFNAM(1:LEN2)

         ENDIF
         CALL CONFIRMATION(TEMP,.TRUE.,YES)

         IF ( YES ) THEN
C
C           Save current drawing ( Overwrites confirmed here for safety )
C
            CALL OVERWRITE(DFNAM,OK)
            IF ( .NOT.OK ) THEN
C                go back to verb menu
                 CALL GTMCLO(MEN,CELLN)
                 GOTO 604
            ENDIF
C
            CALL SUFFIX(DFNAM,'.drg')
C
            CALL DELETE_PIC(DFNAM,OK)
            CALL OPENNM(DFNAM,PARFUN,.FALSE.,OK)
C
            IF ( .NOT. OK ) THEN
C
C              Fault in opening drawing
C
               CALL ERROROUT(DICT01(12),NLEN(DICT01(12)))
               CALL GTMCLO(MEN,CELLN)
               GOTO 604
            END IF
C
C           go save the drawing - SPB 061294 - Always compact it.
            CALL SAVALL(.FALSE.,.FALSE.,.FALSE.,OK)

            IF (.NOT.OK) THEN
               CALL ERRORDIALOG('Error writing drawing')
               CALL GTMCLO(MEN,CELLN)
               CLOSE(UNIT=PARFUN,STATUS='DELETE',ERR=778)
               GOTO 604 
            ENDIF
            CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=778)
            CALL CREATEBROWSEIMAGE(DFNAM,DAXDRG,ST)
C           go create report file containing COMP/SYMB list
C           The report file should be named DRAWINGNAME.RPT
C           and is ASCII text format for easy access.
C           get the drawing name
           
            CALL MAKRPT(ST)
            OK=.TRUE.
C           now have report in file DRAWINGNAME.RPT
C           go clear database for next drawing
            GOTO 675
         ELSE
            INQUIRE(FILE=DFNAM,EXIST=EX)
            IF(.NOT.EX) THEN
               CALL EPRINT(
     +         'WARNING. Current drawing is not saved on disk')
            ENDIF
C
C           does he wish to abandon then ?
C
            CALL CONFIRMATION(DICT01(4),.FALSE.,YES)
            IF ( YES ) THEN
                CALL GTMCLO(MEN,CELLN)
                GOTO 675
            ELSE
C               does not want to abandon thus go back to verb
                CALL GTMCLO(MEN,CELLN)
                GOTO 604
            ENDIF
         END IF

778      CONTINUE
C        error in closing file
         CALL ERRORDIALOG(DICT01(82))
         GOTO 604
C
      ELSE
C     *****
C     ***** the idiot hit a blank menu cell,smack his hand!!! *****
C     *****
         I=6
         CALL DEPRNT(I)
         GOTO 604
C
      END IF
C
      GOTO 605
C
      END
C
C       @(#)  256.1 date 12/16/89 initmn.ftn Daxcad revision 1.19
      SUBROUTINE INITMN()
C     ===================
C1       INITMN initialises the menu system
C1    and the default font.
      include  'include/movdat.inc'
      include  'include/macro.inc'
      include  'include/product.inc'
      include  'include/library.inc'
      include  'include/vntable.inc'
      include  'include/krap.inc'
      include  'include/server.inc'
 
      INTEGER*4 NLEN1,NLEN
      INTEGER*4 DEM(4),STD(3),VIE(25)
      CHARACTER*80 TEMP
      CHARACTER*256 PATH
      LOGICAL OK
      INTEGER*4 ST,J
      INTEGER*4 PLEN
      EXTERNAL NLEN1,NLEN,INTPAP
      DATA DEM /3,25,26,27/
      DATA STD /2,25,26/
      DATA VIE /24,15,16,17,18,19,20,21,23,24,25,26,28,29,
     +          31,32,410,281,282,283,487,319,322,323,334/
 
CPRIME
C      INTEGER*2 KUNIT,FUNIT,PATHL,BUFL,ECODE
C      CHARACTER*80 BUFF
C      KUNIT=2
C      BUFL=80
C      CALL GPATH$(KUNIT,FUNIT,BUFF,BUFL,PATHL,ECODE)
C      IF ( ECODE.EQ.0 ) THEN
C         I=0
C 5       CONTINUE
C         I=I+1
C         IF ( BUFF(I:I).NE.'>') GOTO 5
C         PATHL=I
C         BUFF(I+1:)='           '
C      ELSE
C         PRINT*,'Error:',ECODE
C      END IF
CPRIME
C
C     This is here cos we don't what them having everything
C     load the dictionary system
C     load the default menu system
 
C*******************************************************************
C            SYSTEM 2 FILE
C******************************************************
CAPOLLO
      TEMP=PRNAM(1:NLEN(PRNAM))//'.sys.2'
      CALL FPARS(TEMP,ST)
CAPOLLO
CSUN
CC     Load a different menu set for the server to allow for
CC     special commands not in interactive version. GCU.
C      IF (SERVER) THEN
C	TEMP=LIBRARY(1:NLEN(LIBRARY))//'/'//
C     +     PRNAM(1:NLEN(PRNAM))//'.server.sys.2'
C      ELSE
C	TEMP=LIBRARY(1:NLEN(LIBRARY))//'/'//
C     +     PRNAM(1:NLEN(PRNAM))//'.sys.2'
C      END IF
CSUN
CIBM|PC386
C      TEMP=PRNAM(1:NLEN(PRNAM))//'.s2'
C      CALL FPARS(TEMP)
CIBM|PC386
CPRIME
C      TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>'
C     +         //PRNAM(1:NLEN(PRNAM))//'.sys2'
CPRIME
C
CAPOLLO|IBM|PC386
CC
CAPOLLO|IBM|PC386
		
      CALL LODTOK(.TRUE.,TEMP,ST)
      
      IF (ST.NE.0) THEN
C        failure in loading token file
         WRITE(UNIT=*,FMT=*)' Cannot load system file [1]'
         CALL PGM_$EXIT()
      END IF
C
C
C******************************************************
C            END OF  SYSTEM 2 FILE
C*******************************************************************
C
C
C
C*******************************************************************
C            USER  FILE
C******************************************************
CAPOLLO
      WRITE(*,'(A)') 'User menu file'
CAPOLLO
 
CAPOLLO
      TEMP='dict/m.000'
      CALL FPARS(TEMP,ST)
CAPOLLO
CSUN
C      IF (SERVER) THEN
C	TEMP=LIBRARY(1:NLEN(LIBRARY))//'/dict/m.server.000'
C      ELSE
C	TEMP=LIBRARY(1:NLEN(LIBRARY))//'/dict/m.000'
C      END IF
CSUN
CIBM|PC386
C      TEMP='m.000'
C      CALL FPARS(TEMP)
CIBM|PC386
CPRIME
C      TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>m.000'
CPRIME
      CALL LODVNT(TEMP,ST)
C
      IF (ST.NE.0) THEN
C        failure in loading verb/noun file
         WRITE(UNIT=*,FMT='(A)')' Cannot load system file [2]'
         CALL PGM_$EXIT()
      END IF
C
C******************************************************
C        END OF USER FILE
C*******************************************************************
C
C*******************************************************************
C        PROMPT FILE
C******************************************************
 
C     keep him interested
CAPOLLO
CC     load default prompt dictionary
      WRITE(*,'(A)') 'Loading Dictionary'
CAPOLLO
CSUN|APOLLO
      TEMP='dict/p.000'
      CALL FPARS(TEMP,ST)
CSUN|APOLLO
CIBM|PC386
C      TEMP='p.000'
C      CALL FPARS(TEMP)
CIBM|PC386
CPRIME
C      TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>p.000'
CPRIME
      CALL LODPRT(TEMP,ST)
      IF (ST.NE.0) THEN
C        failure in loading token file
         WRITE(UNIT=*,FMT='(A)') ' Cannot load system file [3]'
         CALL PGM_$EXIT()
      END IF
C
C******************************************************
C        END OF  PROMPT FILE
C*******************************************************************
C
C
C
C*******************************************************************
C        FONT FILE
C******************************************************
C     load default prompt dictionary
CAPOLLO
      WRITE(*,'(A)') 'Loading Font definitions'
CAPOLLO
CIBM|PC386
C      TEMP='FONT.000'
C      CALL FPARS(TEMP)
CIBM|PC386
CSUN|APOLLO
      TEMP = 'dict/font.000'
      CALL FPARS(TEMP,ST)
CSUN|APOLLO
CPRIME
C      PRNAM(1:NLEN(PRNAM))//'>font.000'
CPRIME
      CALL LODFNT(TEMP,ST)
      IF (ST.NE.0) THEN
C        failure in loading token file
         WRITE(UNIT=*,FMT='(A)')' Cannot load system file [4]'
         CALL PGM_$EXIT()
      END IF
C
C******************************************************
C        END OF FONT FILE
C*******************************************************************
C
C
C
C*******************************************************************
C        LINE STYLE DEFINITION FILE
C******************************************************
CAPOLLO
      WRITE(*,'(A)') 'Loading Line definitions'
      TEMP='line.style'
CAPOLLO
CSUN
C       TEMP=HOME(1:NLEN(HOME))//'/.style.pcf'
CSUN
CIBM|PC386
C      TEMP='style.pcf'
C      CALL FPARS(TEMP)
CIBM|PC386
      CALL LDLSTY(TEMP,ST)
      IF (ST.NE.0) THEN
CAPOLLO
         TEMP=LIBRARY(1:NLEN(LIBRARY))//'/line.style'
CAPOLLO
CSUN
C	  TEMP=LIBRARY(1:NLEN(LIBRARY))//'/style.pcf'
CSUN
CIBM
CC         TEMP='\'//PRNAM(1:NLEN(PRNAM))//'\style.dax'
CIBM
CPRIME
C         TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>line.style'
CPRIME
         CALL LDLSTY(TEMP,ST)
         IF ( ST .NE. 0 ) THEN
C           failure in loading token file
            WRITE(UNIT=*,FMT=*)' Cannot load system file [5]'
         CALL PGM_$EXIT()
         END IF
      END IF
 
C******************************************************
C        END OF LINE STYLE DEFINITION FILE
C*******************************************************************
 
 
 
C*******************************************************************
C        LINE WIDTH DEFINTIION FILE
C******************************************************
CAPOLLO
      WRITE(*,'(A)') 'Loading Line width definitions'
 
      TEMP='line.width'
CAPOLLO
CSUN
C       TEMP=HOME(1:NLEN(HOME))//'/.width.pcf'
CSUN
CIBM|PC386
C      TEMP='width.pcf'
C      CALL FPARS(TEMP)
CIBM|PC386
      CALL LDLTHK(TEMP,ST)
      IF (ST.NE.0) THEN
CAPOLLO
          TEMP=LIBRARY(1:NLEN(LIBRARY))//'/line.width'
CAPOLLO
CSUN
C         TEMP=LIBRARY(1:NLEN(LIBRARY))//'/width.pcf'
CSUN
CIBM
CC         TEMP='\'//PRNAM(1:NLEN(PRNAM))//'\width.dax'
CIBM
CPRIME
C         TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>line.width'
CPRIME
         CALL LDLTHK(TEMP,ST)
C        WRITE(UNIT=*,FMT=*)' Load linewidth Result: ',ST
 
         IF ( ST .NE. 0 ) THEN
C           failure in loading token file
            WRITE(UNIT=*,FMT=*)' Cannot load system file [6]',ST
            CALL PGM_$EXIT()
         END IF
      END IF
 
C*******************************************************************
C        MARKER DEFINTIION FILE
C******************************************************
CAPOLLO
		WRITE(*,'(A)') 'Loading Marker definitions'
 
      TEMP='marker.pcf'
CAPOLLO
CSUN
C       TEMP=HOME(1:NLEN(HOME))//'/.marker.pcf'
CSUN
CIBM|PC386
C      TEMP='marker.pcf'
C      CALL FPARS(TEMP)
CIBM|PC386
      CALL LDLMRK(TEMP,ST)
      IF (ST.NE.0) THEN
CAPOLLO
          TEMP=LIBRARY(1:NLEN(LIBRARY))//'/marker.pcf'
CAPOLLO
CSUN
C         TEMP=LIBRARY(1:NLEN(LIBRARY))//'/marker.pcf'
CSUN
CIBM
CC         TEMP='\'//PRNAM(1:NLEN(PRNAM))//'\marker.dax'
CIBM
CPRIME
C         TEMP=BUFF(1:PATHL)//PRNAM(1:NLEN(PRNAM))//'>marker.pcf'
CPRIME
         CALL LDLMRK(TEMP,ST)
      END IF
 
C******************************************************
C        END OF LINE WIDTH DEFINITION FILE
C*******************************************************************
C
    
C******************************************************
C         PAPER DEFINTIION FILE
C******************************************************
CAPOLLO
		WRITE(*,'(A)') 'Loading Paper definitions'
 
      TEMP='paper.pcf'
CAPOLLO
CIBM|PC386
C      TEMP='paper.pcf'
CIBM|PC386
CSUN
C      TEMP=HOME(1:NLEN(HOME))//'/.paper.pcf'
CSUN
      CALL LDLPAP(TEMP,ST)
      IF (ST.NE.0) THEN
CSUN|APOLLO
        TEMP=LIBRARY(1:NLEN(LIBRARY))//'/paper.pcf'
CSUN|APOLLO
CIBM|PC386
C        TEMP='paper.pcf'
C        CALL FPARS(TEMP)
CIBM|PC386
CPRIME
C        TEMP=PRNAM(1:NLEN(PRNAM))//'>paper.pcf'
CPRIME
        CALL LDLPAP(TEMP,ST)
CSUN
CC	need some error message. GCU
C         IF ( ST .NE. 0 ) THEN
CC           failure in loading paper definition file
C            WRITE(UNIT=*,FMT=*)' Cannot load system file [7]'
C            CALL EXIT()
C         END IF
CSUN
      END IF
C
C     set the paper sizes in the verb/noun table
C     and set the PAPLST, and the PAPNAM depending
C     on whether the paper has been user defined
C     or set up by DAXCAD.
      CALL INTPAP(ST)
C
C******************************************************
C        END OF PAPER DEFINITION FILE
C*******************************************************************
C*******************************************************************
C        CONFIGURATION  DEFINTIION FILE
C******************************************************
c
CAPOLLO
      WRITE(*,'(A)') 'Loading Master definitions'
 
      TEMP=PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CAPOLLO
CSUN
C     TEMP=HOME(1:NLEN(HOME))//'/.daxcad.pcf'
CSUN
CIBM|PC386
C     TEMP=PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CIBM|PC386
      CALL LDLCON(TEMP,ST)
      IF (ST.NE.0) THEN
CAPOLLO
        TEMP=LIBRARY(1:NLEN(LIBRARY))//'/'//
     +     PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CAPOLLO
CSUN
C       TEMP=LIBRARY(1:NLEN(LIBRARY))//'/'//
C     +     PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CSUN
CIBM
C       TEMP=LIBRARY(1:NLEN(LIBRARY))//'/'//
C     +     PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CIBM
CPRIME
C        TEMP=BUFF(1:PATHL)//PRODUCT(1:NLEN(PRODUCT))//'>'
C     +         //PRODUCT(1:NLEN(PRODUCT))//'.pcf'
CPRIME
         CALL LDLCON(TEMP,ST)
      END IF
 
C*******************************************************************
C         PATHNAME FILE
C******************************************************
C     clear out the create pathnames
      DO 25 ST=1,NPATH
          PATHN(ST)=' '
25    CONTINUE
      CALL TOOLSGETCWD(PATH,PLEN,ST)
      PATHN(1) = PATH(1:PLEN)//'/'
      PATHN(2) = PATH(1:PLEN)//'/'
      PATHN(3) = PATH(1:PLEN)//'/'
      PATHN(4) = PATH(1:PLEN)//'/'
      PATHN(5) = PATH(1:PLEN)//'/'
C     added for backcloth ja
      PATHN(6) = PATH(1:PLEN)//'/'
C     save the name of the start up directory
      STARTUPDIR = PATH(1:PLEN)//'/'
CAPOLLO|IBM
      WRITE(*,'(A)') 'Loading Pathname definitions'
      TEMP='pathname.pcf'
CAPOLLO|IBM
CSUN
C      TEMP=HOME(1:NLEN(HOME))//'/.pathname.pcf'
CSUN
C     does it exist ?
      INQUIRE(FILE=TEMP,EXIST=OK)
      IF(.NOT.OK) THEN
C        if not then try the DAXCAD directory
CSUN|APOLLO
         TEMP=LIBRARY(1:NLEN(LIBRARY))//'/pathname.pcf'
CSUN|APOLLO
CIBM|PC386
C         TEMP='pathname.pcf'
C         CALL FPARS(TEMP)
CIBM|PC386
CPRIME
C         TEMP=PRNAM(1:NLEN(PRNAM))//'>pathname.pcf'
CPRIME
C         does it exist ?
          INQUIRE(FILE=TEMP,EXIST=OK)
C         neither exists just go home
      ENDIF
C
      ST=1
 
      IF ( OK ) THEN
        CALL FINDU1(MACUNT,OK)
        OPEN(UNIT=MACUNT,FILE=TEMP,ERR=29)
26      CONTINUE
C       If all pathnames set close file
        IF(ST.GT.NPATH) GOTO 30
        READ(UNIT=MACUNT,FMT='(A)',END=30) PATHN(ST)
C       comment line ignore it
        IF(PATHN(ST)(1:1).EQ.'*') GOTO 26
C       space. now this is a valid pathnme use it
        IF(NLEN(PATHN(ST)).EQ.0) THEN
           ST=ST+1
           GOTO 26
        ELSEIF(PATHN(ST)(NLEN1(PATHN(ST)):).NE.'/') THEN
C           Big pathname make sure there is a / char
            PATHN(ST)(NLEN1(PATHN(ST))+1:)='/'
        ENDIF
C       check it as a valid directory
        CALL VALDIR(PATHN(ST),OK)
C       if it is not the set to the current directory
        IF(.NOT.OK) THEN
            PATHN(ST) =PATH(1:PLEN)
        ENDIF
        IF ( NLEN(PATHN(ST)).EQ.0) THEN
            PATHN(ST) =PATH(1:PLEN)
        ENDIF
        ST=ST+1
        GOTO 26
C       close the pathname file
30      CLOSE (UNIT=MACUNT)
C       all system files loaded
      ENDIF
28    CONTINUE

      WRITE(*,'(A)') 'Phase 1 load Completed'
 
      RETURN
29    CONTINUE
      WRITE(UNIT=*,FMT='(2A)')'Cannot open ',TEMP
C
      END
C
      SUBROUTINE LOADCOLORS(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Load up colormap from daxcad control file
C2    Will make a color map but will be loaded at cursor level
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
C
      include  'include/movdat.inc'
      include  'include/macro.inc'
      include  'include/product.inc'
      include  'include/library.inc'
      include  'include/vntable.inc'
      include  'include/server.inc'
      include  'include/daxcolor.inc'
      include  'include/interface.inc'
      include  'include/gpr.ins.inc'
C
      INTEGER*4 ST
      INTEGER*4 UNITN
      INTEGER*4 COL
      INTEGER*4 POS
      INTEGER*4 R,B,G
      INTEGER*4 P1,P2
      INTEGER*4 STARTP
      INTEGER*4 I
      INTEGER*4 COLORMAP(3,0:256)
      INTEGER*4 COLORE
      INTEGER*4 COLORV
      INTEGER*4 Pixel

      CHARACTER*80 TEMP
      CHARACTER*20 NAME
      CHARACTER*256 STRING
C
      LOGICAL OK
      LOGICAL UNOBSCURED 
C
      EXTERNAL COLORE
C
      TEMP = 'user.colormap'
      CALL FPARS(TEMP,ST)
C
      CALL FINDU1(UNITN,OK)
C
      OPEN(UNIT=UNITN,FILE=TEMP,STATUS='OLD',ERR=999)
      REWIND(UNITN)
C
C     reset values so as no to store them
C
	  WRITE(*,'(A)') 'Loading Colormap definitions from user.colormap'


1000  CONTINUE
C
      READ(UNIT=UNITN,FMT='(A)',END=2000 ) STRING

      CALL FOLDUP(STRING)

      IF ( STRING(1:1).EQ.'#') THEN
           GOTO 1000

      ELSEIF ( STRING(1:5).EQ.'COLOR') THEN

           POS = INDEX(STRING(7:),' ') - 1
           NAME = STRING(7:POS+7)
           CALL DBOUND(STRING(POS+7:),P1,P2,*2000)
           STARTP = POS + 7
           READ(STRING(P1+STARTP-1:P2+STARTP-1),FMT='(I2)') COL
           P2 = P2  + STARTP 
           STARTP = P2
           CALL DBOUND(STRING(P2:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I3)') R
           P2 = P2  + STARTP 
           STARTP = P2
           CALL DBOUND(STRING(P2:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I3)') G
           P2 = P2 + STARTP 
           STARTP = P2
           CALL DBOUND(STRING(P2:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I3)') B
           COLORMAP(1,COL) = R
           COLORMAP(2,COL) = G
           COLORMAP(3,COL) = B
           IF ( COL.GT.0 )  THEN
               VNOUN(COL+413) = NAME
           ENDIF

      ELSEIF ( STRING(1:10).EQ.'FOREGROUND') THEN

           STARTP = 11
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') COLFOR

      ELSEIF ( STRING(1:10).EQ.'BACKGROUND') THEN

           STARTP = 11
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') COLBAK

      ELSEIF ( STRING(1:15).EQ.'MENU FOREGROUND') THEN
           STARTP = 16
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') MENUF
      ELSEIF ( STRING(1:15).EQ.'VIEW FOREGROUND') THEN
           STARTP = 16
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') VIEWF
      ELSEIF ( STRING(1:15).EQ.'VIEW BACKGROUND') THEN
           STARTP = 16
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') VIEWB
      ELSEIF ( STRING(1:15).EQ.'MENU BACKGROUND') THEN
           STARTP = 16
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') MENUB
      ELSEIF ( STRING(1:17).EQ.'DIALOG FOREGROUND') THEN
           STARTP = 19
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') DIALOGF
      ELSEIF ( STRING(1:17).EQ.'DIALOG BACKGROUND') THEN
           STARTP = 19
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') DIALOGB
      ELSEIF ( STRING(1:6).EQ.'OFFSET') THEN
           STARTP = 8
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I3)') 
     +     COLOR_MAP_OFFSET
      ELSEIF ( STRING(1:6).EQ.'POPMRK') THEN
           STARTP = 8
           CALL DBOUND(STRING(STARTP:),P1,P2,*2000)
           READ(STRING(STARTP+P1-1:STARTP+P2-1),FMT='(I2)') POPMRK
      ENDIF

      GOTO 1000

2000  CONTINUE
      CLOSE(UNITN)
C
C     Load colormap
C 
C     set up the DAXCAD/system color map
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
C      COLOR_MAP(0)=COLOR_MAP_OFFSET
      CALL GPR_$GET_SYSTEM_COLORMAP(COLOR_MAP, COLOR_MAP_SIZE, ST)
C
C     set up the view forground/background ja
c       IF (COLOR_MAP_SIZE.GT.1) THEN
c           CALL setbackclothvalues(COLOR_MAP(VIEWF), COLOR_MAP(VIEWB))
c       ENDIF
C
      DO 3000 I = 0,COLOR_MAP_SIZE-1
			
          R = COLORMAP(1,I)
          G = COLORMAP(2,I)
          B = COLORMAP(3,I)
          CALL GprAllocColor(R,G,B,Pixel)
          COLOR_MAP(I) = Pixel         
          
C		  COLORV = COLORE(R,G,B)

 
        call testx
C		  CALL GPR_$SET_COLOR_MAP(COLOR_MAP(I),INT2(1),COLORV,ST)
3000  CONTINUE
C
      CALL GPR_$RELEASE_DISPLAY(ST)


C
999   CONTINUE
C
      END
C
      SUBROUTINE CMPCT1()
      END
      SUBROUTINE INTGRY(OK)
      LOGICAL OK
      OK = .TRUE.
      END

