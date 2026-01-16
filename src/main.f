C
C     @(#)  412.1 date 6/11/92 main.f 
C
C
C     Filename    : main.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:39:55
C     Last change : 92/06/11 14:34:01
C
C     Copyright : Practical Technology International Limited  
C     File :- main.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE CREATD()
C     =-=-=-=-=-=-=-=-=-=
C
C1    no arguments required
C
C2    Subroutine CREATD controls the CREATE DRAWING
C2    option from the level1 menu.The parameters for
C2    use in the drawing may be set in this routine.
C
      include  'include/menun.inc'
      include  'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
      include 'include/wtov.inc'
      include 'include/filunit.inc'
      include 'include/dimendat.inc'
      include 'include/macro.inc'
      include 'include/lfu.inc'
      include 'include/vntable.inc'
      include 'include/ftypes.inc'
      include 'include/product.inc'
C
      REAL X,Y
      DOUBLE PRECISION DN1,DN2
      INTEGER*4 C,NLEN,P,TMEN,TCELL,L,I,FILU,NLEN1
      INTEGER*4 ST
      INTEGER*2 LENGTH

      LOGICAL YES
      LOGICAL OK,OPTION,QUIT,YESOK
      CHARACTER*80  INPUT,TEMP
      CHARACTER*12 TEMP1,TEMP2,FILNM*80,ANS*3
      CHARACTER*512 TITLE
      CHARACTER*1 TOKEN,TDBU1*2,TDBU2*2
      INTRINSIC CHAR,REAL,INDEX,MOD
      EXTERNAL MNICRD,CHGP04,SHOPAP,TCURS,YESOK,NLEN,
     +     GTMCLO,OPENNM,FINDP0,PEN,DISFRA,RSWAP,AEXPRN,NLEN1,
     +     CRUNCH,CLRPEW,CPRINT,GTMCWT,FNDTOK,CHGSCL,CHGPAP
C
C     Clear the drawing filename
      DRGNAM='          '
C     Allow for loading a drawing with a different paper
C     than those defined.
C     Find the pointer to current sheet size
      DO 70 I=1,5
      IF (PAPLST(I).EQ.DRWSHT(:2)) GOTO 71
 70   CONTINUE
      I=1
      DRWSHT(:2)=PAPLST(I)
 71   CONTINUE
C     Allow for raster backcloth resetting the sheet size
C     Reset the paper size for the drawing
      DRWSIZ(1)=PAPSIZ(1,I)
      DRWSIZ(2)=PAPSIZ(2,I)
 
C     initialize menu system for creation of drawing
      CALL MNICRD()
C     need to show correct conditions
      TMEN=3
C
C     Set paper size and rotate status
      CALL CHGP04()
C
C     show initial conditions
C      IF (DRWSHT(3:3).EQ.'R') THEN
CC        sheet is already rotated
CC        set back to normal position
C          CALL RSWAP(DRWSIZ(1),DRWSIZ(2))
C      ENDIF
      CALL SHOPAP()
C     Let him hit something, and find out what he wants to do
 600  CONTINUE
C     go get cursor hit
      CALL DCPRNT(259)
      CALL TCURS(C,X,Y)
C
 601  CONTINUE
C     The returned command character from the main menu
C     directs us to the next option.
C     if exit ptcad then stop here
      IF(MEN.EQ.0) THEN
C        hit in graphics area not allowed
         CALL DEPRNT(9)
         GOTO 600
      END IF
C
      TMEN=MEN
      TCELL=CELLN
C
      IF (CCMD.EQ.'N') THEN
C
 66      CONTINUE
C        name the drawing to create
         CALL DPRMXP(10,FILNM)
C			write(*,*), 'Name is from dialog:',filnm
		
C        if null file name assume abort required
         IF (NLEN(FILNM).EQ.0 ) THEN
C           go back and start again
C           ensure caller menu,cell not hilited
            CALL GTMCLO(TMEN,TCELL)
            GOTO 600
         END IF
C        check that it is not the product name
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C         CALL UNFOLD(FILNM)
         IF(INDEX(FILNM,PRNAM(1:NLEN(PRNAM))).GT.0) THEN
             CALL DEPRNT(276)
             GOTO 66
         END IF
C***************************************************
C     note this will prevent people not creating
C     their drawings without .drg.
C***************************************************
         CALL SUFFIX(FILNM,'.drg')
         IF(INDEX(FILNM,'/').EQ.0) THEN
C            do not override pathname
             WRITE(UNIT=TEMP,FMT='(2A)')PATHN(1)(:NLEN1(PATHN(1))),FILNM
     +        (:NLEN1(FILNM))
             FILNM=TEMP
             CALL CRUNCH(FILNM)
         ENDIF
 
C			print*, 'Name is:',filnm
C        set the draing type
         CALL OVERWRITE(FILNM,OK)
         IF ( .NOT.OK ) THEN
             CALL GTMCLO(TMEN,TCELL)
             GOTO 600
         ELSE
             CALL SUFFIX(FILNM,'.drg')
         ENDIF
          
         DAXTYP = DAXDRG
         CALL OPENNM(FILNM,FILU,.FALSE.,OK)
 
 
         IF ( .NOT. OK ) THEN
 
             CALL GTMCLO(TMEN,TCELL)
             GOTO 600
 
         END IF
C
C        save drawing name in common block
         DRGNAM=FILNM
         DFNAM=FILNM


C
         CLOSE(UNIT=FILU,STATUS='DELETE',ERR=201)
C
         WRITE(TITLE,'(A,X,F5.2,2A)') PRODUCT(1:NLEN(PRODUCT)),ROOTRV,
     +   ' :: Drawing Loaded > ',DRGNAM(1:NLEN(DRGNAM))
         LENGTH = NLEN(TITLE)
         CALL GPR_$SET_TITLE(0,TITLE,LENGTH,ST)
C
C        ensure caller menu,cell not hilited
         CALL GTMCLO(TMEN,TCELL)
         TOKEN='N'
         CALL GTHFMC(3,TOKEN,TCELL)
         TCELL=TCELL+1
         CALL GTCLRC(3,TCELL)
         TOKEN='X'
         CALL GTPMEN(FILNM,TOKEN,3,TCELL)
         CALL GTMCLO(TMEN,TCELL)
         GOTO 600
 201     CONTINUE
         CALL DEPRNT(12)
C        ensure caller menu,cell not hilited
         CALL GTMCLO(TMEN,TCELL)
         GOTO 600
C
      ELSE IF (CCMD.EQ.'P') THEN
C        set the paper size for the drawing
         CALL CHGPAP(OK)
C        show new paper in position
         IF(OK)  THEN
           CALL SHOPAP()      
         ENDIF                    
         CALL GTMCLO(TMEN,TCELL)
C
      ELSE IF (CCMD.EQ.'R') THEN
C        rotate the paper for the drawing
C        test for rotated sheet first
         IF (DRWSHT(3:3).EQ.'R') THEN
C           sheet is already rotated
C           set back to normal position
            CALL RSWAP(DRWSIZ(1),DRWSIZ(2))
C           reset drawing sheet descriptor
            DRWSHT(3:3)=' '
C           ensure cell no longer hilited
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           sheet positioned normally
C           must rotate it now
            CALL RSWAP(DRWSIZ(1),DRWSIZ(2))
C           set sheet descriptor accordingly
            DRWSHT(3:3)='R'
         END IF
C        paper rotation now complete
C        show new position of paper
         CALL SHOPAP()
C
      ELSE IF (CCMD.EQ.'U') THEN
C        set the database units for the drawing
         CALL CHDBUN(OK)
         CALL GTMCLO(TMEN,TCELL)
      ELSE IF (CCMD.EQ.'S') THEN
C        set the drawing scale
         CALL CHGSCL(OK)
         CALL SHOPAP()
         CALL DCPRNT(259)
C
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C        if null file name assume abort required
         IF (NLEN(DRGNAM).EQ.0) THEN
C           null file name not allowed
            CALL DEPRNT(15)
            CALL GTMCLO(TMEN,TCELL)
            GOTO 600
         END IF
C        ask for the OK to continue
         WRITE(UNIT=TEMP,FMT='(A,A)') 'Creating drawing ',
     +   DRGNAM(1:NLEN(DRGNAM))
         CALL CPRINT(TEMP)
         CALL CONFIRMATION(DICT01(16),.TRUE.,YES)
         IF ( .NOT.YES ) THEN
            CALL GTMCLO(TMEN,TCELL)
            GOTO 600
         END IF
C         CALL DPRMXP(16,ANS)
C        if not ok,go back to start
C         IF ( .NOT.YESOK(ANS) ) THEN
C            CALL GTMCLO(TMEN,TCELL)
C            GOTO 600
C         END IF
C        all preparations complete,continue to
C        next stage of drawing
C        show paper in new position
         CALL SHOPAP()
         CALL GTMCLO(TMEN,TCELL)
C        nullify the raster sections info - GCU
         CALL OSCLRM()
         RETURN
      ELSE IF (CCMD.EQ.'Q') THEN
C        return to caller with quit status
         RETURN
      ELSE IF (CCMD.EQ.'X') THEN
C        print out the pathname
         CALL CPRINT(DICT01(482)(:NLEN(DICT01(482)))//FILNM)
         CALL GTMCLO(TMEN,TCELL)
      ELSE
C        function not available at the moment
         CALL DEPRNT(131)
         CALL GTMCLO(TMEN,TCELL)
      END IF
      GOTO 600
C
      END
C
      SUBROUTINE DRWINF()
C     =-=-=-=-=-=-=-=-=-=
C
      include 'include/masti.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/fhead.inc'
      include  'include/vntable.inc'
      include  'include/lfu.inc'
C
      CHARACTER*80 FILNM,INPL*50,NUM*3
      INTEGER*2 I
      INTEGER*4 NLEN,LENG,LP,PS
      LOGICAL OK
C
      EXTERNAL NLEN,CRUNCH
C
C     go find a unit for the file
CAPOLLO|SUN
      FILNM=DFNAM
      CALL SUFFIX(FILNM,'.inf')
CAPOLLO|SUN
CIBM|PC386
C      PS=INDEX(DFNAM,'.')-1
C      IF(PS.EQ.-1) PS=NLEN(DFNAM)
C      FILNM=DFNAM(1:PS)//'.inf'
CIBM|PC386
C
      CALL FINDU2(PARFUN,FILNM,OK)
C
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(275)
         RETURN
      END IF
C
  6   CONTINUE
C     ensure correct drawing name is used
C
C
      OPEN(UNIT=PARFUN,FILE=FILNM,ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED',STATUS='UNKNOWN',ERR=101)
CIBM
C      LFU(PARFUN)=.TRUE.
CIBM
C
      WRITE(UNIT=PARFUN,FMT='(A)',ERR=102) ' '
      REWIND(PARFUN)
      WRITE(UNIT=PARFUN,FMT='(A)')
     + '************************************************'
      WRITE(UNIT=PARFUN,FMT='(5X,A,F4.2)')
     +   DICT01(325)(1:NLEN(DICT01(325))),OROTRV
      WRITE(UNIT=PARFUN,FMT='(A,//)')
     + '************************************************'
C
      WRITE(UNIT=PARFUN,FMT='(9X,A,//)')
     +   DICT01(326)(1:NLEN(DICT01(326)))
C
      WRITE(UNIT=PARFUN,FMT='(4A)')
     +   DICT01(318)(1:NLEN(DICT01(318))),'"',DFNAM(1:NLEN(DFNAM)),'"'
      WRITE(UNIT=PARFUN,FMT='(/,4A)')
     +  DICT01(324)(1:NLEN(DICT01(324))),'"',
     1            DRGNAM(1:NLEN(DRGNAM)),'"'
C
      WRITE(UNIT=PARFUN,FMT='(A,F5.2)')
     +  DICT01(311)(1:NLEN(DICT01(311))),DRGREV
C
      WRITE(UNIT=PARFUN,FMT='(A,I2,A,I2.2,A,I2,2X,2(I2,A),I4)')
     +  DICT01(304)(1:NLEN(DICT01(304))),
     1  IFHEAD(4),':',IFHEAD(5),':',IFHEAD(6),
     2  IFHEAD(3),'/',IFHEAD(2),'/',IFHEAD(1)
C
      WRITE(UNIT=PARFUN,FMT='(2A)')
     +   DICT01(262)(1:NLEN(DICT01(262))),DBUNIT
C
      WRITE(UNIT=PARFUN,FMT='(2A)')
     +   DICT01(162)(1:NLEN(DICT01(162))),DRGSCL
C
      WRITE(UNIT=PARFUN,FMT='(2A)')
     +   DICT01(310)(1:NLEN(DICT01(310))),DRWSHT
C
      WRITE(UNIT=PARFUN,FMT='(/,2X,A)')
     +   DICT01(301)(1:NLEN(DICT01(301)))
      WRITE(UNIT=PARFUN,FMT='(A)' )
     +'  ===== =====     ===='
C
      DO 70 I=0,255
         IF ( TLAYER(I).NE.0.OR.NLEN(LNAME(I)).GT.0) THEN
            IF ( NLEN(LNAME(I)) .GT. 0 ) THEN
               IF ( TLAYER(I).NE.0 ) THEN
                  WRITE(UNIT=PARFUN,FMT='(2I6,6X,A)' )
     +               I,TLAYER(I),LNAME(I)(1:NLEN(LNAME(I)))
               ELSE
                  WRITE(UNIT=PARFUN,FMT='(2I6,6X,A)' )
     +               I,0,LNAME(I)(1:NLEN(LNAME(I)))
               END IF
            ELSE
               IF ( TLAYER(I).NE.0 ) THEN
                  WRITE(UNIT=PARFUN,FMT='(2I6)' ) I,TLAYER(I)
               END IF
            END IF
         END IF
 70   CONTINUE
C
      WRITE(UNIT=PARFUN,FMT='(/,A,I5)')
     +   DICT01(309)(1:NLEN(DICT01(309))),CLAYER
C
      WRITE(UNIT=PARFUN,FMT='(/,A,I5)')
     +  DICT01(314)(1:NLEN(DICT01(314))),NMIPOS-1-DELENT
C
      WRITE(UNIT=PARFUN,FMT='(/,A,I5)')
     +  DICT01(615)(1:NLEN(DICT01(615))),NPDPOS-1
C
C
      WRITE(UNIT=PARFUN,FMT='(//,9X,A)')
     +   DICT01(316)(1:NLEN(DICT01(316)))
C
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=201)
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
C
      CALL POPPD1(FILNM,150,200,500,500)
C
      RETURN
C
 101  CONTINUE
      CALL DCPRNT(287)
      RETURN
 102  CONTINUE
      CLOSE(PARFUN)
      CALL DEPRNT(835)
      RETURN
C
 201  CALL DCPRNT(82)
C
      END
*
      SUBROUTINE LEVEL1()
C     =-=-=-=-=-=-=-=-=-=
C
C1    no arguments required
C
C2    Subroutine LEVEL1 controls the startup operations
C2     of the program for craetion and retrieval of
C2    drawings.
C
      include  'include/menun.inc'
      include  'include/swind.inc'
      include  'include/filunit.inc'
      include  'include/vntable.inc'
      include  'include/style.inc'
      include  'include/lfu.inc'
      include  'include/params.inc'
      include  'include/journal.inc'
      include  'include/product.inc'
C
      CHARACTER*40 FN(10)
      CHARACTER*512 TITLE
      REAL X,Y
      INTEGER*4 C
      INTEGER*2 TCOL,N
      INTEGER*2 LENGTH
      INTEGER*4 ST
      INTEGER*4 NLEN
      LOGICAL OK
C
      EXTERNAL NLEN
C
 610  CONTINUE
C     load the initial menu system
      CALL MNILV1()
C
C     Let him hit something, and find out what he wants to do
 600  CONTINUE
C
      CALL TCURS(C,X,Y)
C
C     The returned command character from the main menu
C     directs us to the next option.
C     if exit ptcad then stop here
      IF(MEN.EQ.0) THEN
C        hit in graphics area not allowed
         CALL DEPRNT(7)
         GOTO 600
      END IF
C
      IF (CCMD.EQ.'E') THEN
C        The chap wants to finsh , well thats allright
C        close scratch work file
         CLOSE(UNIT=SWINDU,STATUS='DELETE')
C
C        Daxcad diagnostics delete
C
         CLOSE(UNIT=10,STATUS='DELETE')
CIBM
C         LFU(SWINDU)=.FALSE.
CIBM
C        delete the temporary workfile
         CALL KILLWF()
C        shut down the apollo graphics system
         CALL INMOV(.FALSE.)
         CALL WRCMDS()
C        if the journaling is running then stop it
         IF(JOURON) CALL CLSJRN(OK)
         CALL APSHUT()
C
         WRITE(*,'(A)') ' '
         WRITE(*,'(2A)')
     +       PRODUCT(1:NLEN(PRODUCT)),': SHUTDOWN COMPLETE'
         WRITE(*,'(A)') ' '
C
         CALL PGM_$EXIT()
      ELSE IF (CCMD.EQ.'C') THEN
C        he wants to create a new drawing
         CALL CREATD()
C        if quit status,start again
         IF (CCMD.EQ.'Q') GOTO 610
      ELSE IF (CCMD.EQ.'R') THEN
C        he wants to retrieve an old drawing
         CALL LOAD2(.FALSE.)
C        if abort,start again
         IF (PARFUN.EQ.0) GOTO 610
         CALL GTCLRM(3)
         CALL REGEND()
C        tell him to close information pad to continue
         CALL DCPRNT(227)
C
         DRGNAM=DFNAM

         WRITE(TITLE,'(A,X,F5.2,2A)') PRODUCT(1:NLEN(PRODUCT)),ROOTRV,
     +   ' :: Drawing Loaded > ',DRGNAM(1:NLEN(DRGNAM))
         LENGTH = NLEN(TITLE)
         CALL GPR_$SET_TITLE(0,TITLE,LENGTH,ST)

         CALL DRWINF()
         TCOL=COLOUR
         CALL SETDRW(TCOL)
      ELSE IF (CCMD.EQ.'B') THEN
         CALL GENC(OK)
         CALL GTMCLO(MEN,CELLN)
         GOTO 600
      ELSE
C        function not available at the moment
         CALL DEPRNT(8)
         GOTO 600
      END IF
C
C     if abort from function,then start again
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 610
C
      END
C
      SUBROUTINE MNICRD()
C     ===================
C
C1    No arguments required.
C
C2    Loads the CREATE DRAWING MENU options.
C
      include 'include/params.inc'
      include 'include/macro.inc'
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 I,TCELL,TMEN
      CHARACTER TOKEN*1
      INTRINSIC CHAR
      EXTERNAL GTDMEN,GTDMHD,GTCLRM,MNLCRD,FNDTOK,GTMCWT
C                           
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C
      I=1
C
C     place header from dict pos 6 into menu 3.
      CALL GTDMHD(6,3)
C
C     enter the option available at this point.
C
C2    N is the token for drawing name
      CALL GTDMEN(7,3)
 
C
C2    P is the token for paper size
C     show current sheet size, and it is
C     a popup so set the flag for multiple
      GTMULT = .TRUE.
      CALL GTDMWT(8,3,DRWSHT(:2))
C
C     show paper selected in the cell below.
      TOKEN = ' '
      TMEN = 3
      TCELL=VNPOS(8) + 1
      CALL GTPMEN(PAPNAM(CURPNO),TOKEN,TMEN,TCELL)
C
C2    U is the token for database units
      CALL GTDMEN(9,3)
C     show units selected
      CALL FNDTOK(9,TOKEN)
      GTMULT = .TRUE.
      CALL GTMCWT(3,TOKEN,DBUNIT)
C
C2    S is the token for drawscale
      CALL GTDMEN(10,3)
C     show scale selected
      CALL FNDTOK(10,TOKEN)
      CALL GTMCWT(3,TOKEN,DRGSCL)
C
C2    R is the token for rotate drawing sheet
      CALL GTDMEN(11,3)
C
C2    Q is the token for abort
      CALL GTDMEN(12,3)
C
C2    <CR> is the token for ACCEPT
      CALL GTDMEN(13,3)
C
      END
*
      SUBROUTINE MNILV1()
C     ===================
C
C1    No arguments required.
C
C2    Loads the LEVEL1 MENU options and
C
      INTEGER I
      EXTERNAL GTDMEN,GTMCHI,GTCLRM
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Enter the master menu options.
C
      I=1
C
C      CALL GTPMEN(' INITIAL MENU ',' ',3,I)
C     place header from dict pos 2 into menu 3
      CALL GTDMHD(2,3)
C
      CALL DCPRNT(258)
C
C     enter the option available at this point
C2    C is the token for create drawing
C      CALL GTPMEN('CREATE DRAWING','C',3,I+3)
C     place command from dict pos 3 into menu 3
      CALL GTDMEN(3,3)
C
C2    R is the token for reyrieve drawing
C      CALL GTPMEN('RETRIEVE DRAWING','R',3,I+10)
C     place command from dict pos 4 into menu 3
      CALL GTDMEN(4,3)
C
      CALL GTPMEN('Convert Files','B',3,I+15)
C2    E is the token for exit ptcad
C      CALL GTPMEN('EXIT DAXCAD','E',3,I+25)
C     place command from dict pos 5 into menu 3
      CALL GTDMEN(5,3)
C
      END
C
C
 
