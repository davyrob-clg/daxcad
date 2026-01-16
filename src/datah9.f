C
C     @(#)  412.1 date 6/11/92 datah9.f 
C
C
C     Filename    : datah9.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:33:14
C     Last change : 92/06/11 14:26:46
C
C     Copyright : Practical Technology Int Limited  
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION BAKDRG(FILNAM)
C     SUBROUTINE BACKUP(FILNM,UBACK,OK)
C     SUBROUTINE CMPACT(REGEN,OK) - Not any longer !
C     SUBROUTINE KILLWF()
C     SUBROUTINE LABFLG(TMIP)
C     SUBROUTINE LABNFG(TMIP)
C     SUBROUTINE LOAD2(WORKF)
C     SUBROUTINE OPENWF(OK)
C     SUBROUTINE RSTALL(OK)
C     SUBROUTINE SAFESC(REGEN,LT)
C     SUBROUTINE SAVALL(WORKF,PACK,REGEN,OK)
C     SUBROUTINE STORE2(WORKF)
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE BACKUP(FILNM,UBACK,OK)
C     =================================
C
C2    Subroutine BACKUP creates a security copy
C2    of the current drawing,using the drawing
C2    revision number as the unique suffix on
C2    the drawing file.
C
      include  'include/params.inc'
      include  'include/filunit.inc'
      include  'include/product.inc'
C
      INTEGER*4 NLEN,UBACK
      LOGICAL OK,EX,DRGEX
      CHARACTER*120 FILEB,FILNM*(*)
      EXTERNAL NLEN,CRUNCH,CHKNAM,COPYF,CPRINT
C
      UBACK=0
      IF ( DFNAM.EQ.PRNAM(1:NLEN(PRNAM))//'.drg'.OR.
     + DFNAM.EQ.PRNAM(1:NLEN(PRNAM))//'.save.drg' ) THEN
         DFNAM=DRGNAM
      END IF
      WRITE(FILEB,FMT='(2A,I3.3)') DFNAM(1:NLEN(DFNAM)),'.',
     +                           INT(100.0*(DRGREV-1.0))
      CALL CRUNCH(FILEB)
C     test for valid name
      CALL CHKNAM(FILEB,OK)
      IF ( .NOT. OK ) RETURN
C     test for existance
      INQUIRE(FILE=FILEB,EXIST=EX)
      INQUIRE(FILE=DFNAM,EXIST=DRGEX)
      OK=.NOT.EX.AND.DRGEX
      IF ( .NOT.OK ) THEN
         UBACK=-1
         RETURN
      END IF
C
C      CALL FINDU1(UBACK,OK)
C      IF ( .NOT. OK ) RETURN
      FILNM=FILEB
C
C     copy the file
CAPOLLO|SUN
      CALL CPRINT('Saving backup file '//FILEB(1:NLEN(FILEB)))
      CALL COPYF(DFNAM(1:NLEN(DFNAM)),FILEB(1:NLEN(FILEB)))
      UBACK=-1
      OK=.FALSE.
CAPOLLO|SUN
C
      END
C
C     -----------------------------------------
C
      LOGICAL FUNCTION BAKDRG(FILNAM)
C1
C1    This function will return whether the drawing is a backup name
 
      INTEGER*4 L,I,NLEN
      CHARACTER*10 STRING
      CHARACTER FILNAM*(*)
      STRING = '0123456789'
 
      L = NLEN(FILNAM)
      IF (L.GT.3) THEN
 
          DO 10 I = L,L-2,-1
              IF(INDEX(STRING,FILNAM(I:I)).EQ.0) GOTO 20
 10       CONTINUE
 
          BAKDRG = .TRUE.
          RETURN
 
      ENDIF
 
20    CONTINUE
      BAKDRG = .FALSE.
      END

C
C SPB - 031194 - This next bit has been superceeded by the code
C                now in the fortran module compact.f
      SUBROUTINE OLDCMPACT(REGEN,OK)
C     ===========================
C
C1    vartype           L     L
C1    iostatus          I     O
C
C2    Subroutine CMPACT performs a database packing
C2    operation,normally when more than 20% of the
C2    drawing has been deleted. OK returns completion status.
C
      include  'include/masti.inc'
      include  'include/ndata.inc'
      include  'include/movdat.inc'
      include  'include/fhead.inc'
      include  'include/dhead.inc'
      include  'include/nhead.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include  'include/nbuff.inc'
      include  'include/swind.inc'
      include  'include/props.inc'
      include  'include/entity.inc'
      include  'include/product.inc'
      include  'include/ftypes.inc'
      include  'include/viewport.inc'
C
      REAL SX,SY
      LOGICAL OK,YES,IGNORE,REGEN
      CHARACTER*80 FILNM*40,TEMP
      INTEGER*4 DNUM,DRGU,NLEN
      INTEGER*2 TMIP,D1,D2,WUN,LT
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,II,PMIP,TTPRP,TTPCP
C
      EXTERNAL OPENNM,DEPRNT,DIR500,SSFLAG,MOVMAS,CRTP05,INSP05
      WUN=1
      IF (NPDPOS.GT.PDPLIM/2.OR.NTXPOS.GT.TXPLIM/2.OR.
     +   NT2POS.GT.PRILIM/2) THEN
           DNUM = 631
           CALL DEPRNT(DNUM)
           DNUM = 632
           CALL DCPRNT(DNUM)
           OK=.FALSE.
           RETURN
      ENDIF
      D1=0
      D2=0
C     the drawing file will be open at this time
      DRGU=PARFUN
      DRGNAM=DFNAM
C     CLOSE(UNIT=PARFUN,STATUS='DELETE',ERR=99)
C
C     WRITE(UNIT=10,FMT=*)'*********** ENTERED CMPACT ************'
C     open file to create part into
 
      DAXTYP = 1
      DAXTYP = DAXPRT
      FILTYP = 'PART'
      FILNM=PRNAM(1:NLEN(PRNAM))//'.prt'
 
C      WRITE(10,*) '[CMPCT] FILNM = ',FILNM
C      WRITE(10,*) '[CMPCT] DAXPRT DAXTYP = ',DAXPRT,DAXTYP
      CALL  OPENNM(FILNM,PARFUN,.FALSE.,OK)
      IF (.NOT.OK) THEN
C        invalid file for some reason
         DNUM=78
         CALL DEPRNT(DNUM)
         PARFUN=DRGU
         OK=.TRUE.
         RETURN
      END IF
C
      SX=0.0
      SY=0.0
C     save the current record pointers
      TTMIP=NMIPOS
      TTPDP=NPDPOS
      TTTXP=NTXPOS
      TTRLP=NRLPOS
      TTPRP=NPRPOS
      TTPCP=NPCPOS
C     clear all the transformation flags
      CALL CLROPF()
C     set copy flag
      COPYIT=.TRUE.
C     set number of copies
      NNCOPY=1
C     disable erase and draw of entities during transformation
      OPFLAG(9)=.FALSE.
      OPFLAG(10)=.FALSE.
C     go do the move
C     ensure masters for comps and symbs are protected
      LT=TTTXP-1
      IF ( LT.GT.0 ) CALL SAFESC(REGEN,LT)
      NDATA=0
C     any master geometry required is now in scratch file
C
C     cycle through the master index
      TMIP=0
 10   CONTINUE
      TMIP=TMIP+1
C     read master index
      CALL DIR500(TMIP,OK)
C
      IGNORE=IMBUFF(1).EQ.100  .OR.
     +       IMBUFF(1).EQ.COMPM .OR. IMBUFF(1).EQ.SYMBM .OR.
     1       IMBUFF(2).EQ.COMPM .OR. IMBUFF(2).EQ.SYMBM.OR.
     2       IMBUFF(1).EQ.GROUP
      IF (.NOT.IGNORE) THEN
C        collect this entity
         IF(IMBUFF(2).EQ.GROUP) THEN
             CALL GETGRP(TMIP,OK)
             IF(.NOT.OK) THEN
                 DNUM = 633
                 CALL DEPRNT(DNUM)
                 WRITE(10,*) '[CMPACT] TMIP= ',TMIP
                 GOTO 10
             ENDIF
CMLW             CALL GETGRP(TMIP,OK)
         ELSE
             CALL SSFLAG(TMIP,SX,SY,D1,D2,.FALSE.)
         ENDIF
      END IF
C
      IF ( TMIP .LT. (NMIPOS-1) ) GOTO 10
C
      IF ( NDATA+NMIPOS.GT.MIPLIM )  THEN
         DNUM=410
         CALL DEPRNT(DNUM)
         DNUM = 632
         CALL DCPRNT(DNUM)
         PARFUN=DRGU
      ELSE
         IF ( NDATA .GT. 0 ) THEN
C           save the current record pointers
C            TTMIP=NMIPOS
C            TTPDP=NPDPOS
C            TTTXP=NTXPOS
C            TTRLP=NRLPOS
C            TTPRP=NPRPOS
C            TTPCP=NPCPOS
C           save display file state
            TLDFIL=LDFILE(CVPN)
C
C           clear all the transformation flags
            CALL CLROPF()
C           set copy flag
            COPYIT=.TRUE.
C           set number of copies
            NNCOPY=1
C           disable erase and draw of entities during transformation
            OPFLAG(9)=.FALSE.
            OPFLAG(10)=.FALSE.
C           go do the move
            CALL MOVMAS(.FALSE.)
C           reset the copy flag
            COPYIT=.FALSE.
C           reset the number of copies
            NNCOPY=0
C           disable erase and draw of entities during transformation
            OPFLAG(9)=.FALSE.
            OPFLAG(10)=.FALSE.
C           enable add to display file
            OPFLAG(8)=.TRUE.
C           go store the data
            CALL CRTP05(FILNM,'PART',TTMIP,TTPDP,TTTXP,
     +                               TTRLP,TTPRP,TTPCP)
C           reset pointers to start
            NMIPOS=WUN
            NPDPOS=WUN
            NTXPOS=WUN
            NRLPOS=WUN
            NPRPOS=WUN
            NPCPOS=WUN
C
            TTMIP=NMIPOS
            TTPDP=NPDPOS
            TTTXP=NTXPOS
            TTRLP=NRLPOS
            TTPRP=NPRPOS
            TTPCP=NPCPOS
C           insert part now
C
            CALL INSP05(FILNM,'PART',TTMIP,TTPDP,TTTXP,
     +                               TTRLP,TTPRP,TTPCP)
C
            NDATA=0
            DO 40 II=TTMIP,NMIPOS-1
C              save the MI pointer in work file
               CALL SSFLAG(II,SX,SY,D1,D2,.FALSE.)
 40         CONTINUE
C           ensure flags are not drawn
            VNDATA=NDATA+1
C
            CALL MOVMAS(.TRUE.)
C
         END IF
C        enable erase and draw of entities during transformation
         OPFLAG(9)=.TRUE.
         OPFLAG(10)=.TRUE.
C        disable add to display file
         OPFLAG(8)=.FALSE.
C        close compact part file and delete
         CALL CLOSNM(FILNM,.FALSE.,OK)
C
         IF ( .NOT. OK ) THEN
            DNUM=411
            CALL DEPRNT(DNUM)
            WRITE(10,*) 'Error closing compact file'
         END IF
C        leave the original drawing file open now
         CLOSE(UNIT=DRGU,STATUS='DELETE',ERR=99)
          DAXTYP = DAXDRG
         CALL OPENNM(DRGNAM,PARFUN,.FALSE.,OK)
         IF ( .NOT. OK ) RETURN
      END IF
C
      OK=.TRUE.
C
C     WRITE(UNIT=10,FMT=*)'*********** LEAVING CMPACT ************'
      RETURN
C
99    CONTINUE
C
      OK=.FALSE.
C
      END
C
C     -----------------------------------------
C
      SUBROUTINE KILLWF()
C     ===================
C1    no arguments required
C
C2    Deletes the current workfile form disc
C2    should only be called at termination of the program
C
      include  'include/filunit.inc'
      include  'include/lfu.inc'
      include  'include/product.inc'
      include  'include/faults.inc'
      include  'include/vntable.inc'
C
      INTEGER*4 D,NLEN
      INTEGER*4 PRMPT
C
      LOGICAL REMOVE
      LOGICAL OK
      LOGICAL EX
C
      CHARACTER*80 FILNM
      EXTERNAL FINDU2
C
      IF(CRASHF) THEN
          FILNM=PRNAM(1:NLEN(PRNAM))//'.save.drg'
          PRMPT = 837
      ELSE
          FILNM=PRNAM(1:NLEN(PRNAM))//'.drg'
          PRMPT = 836
      ENDIF
C
C     Check file exists and offer him deletion
C
      INQUIRE(FILE=FILNM,EXIST=EX)
      IF ( EX ) THEN
C
           CALL CONFIRMATION(DICT01(PRMPT),.FALSE.,REMOVE)  
C      
           IF(REMOVE) THEN
               CALL DELETE(FILNM,OK)
           ENDIF
      ENDIF
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE LABFLG(TMIP)
C     =======================
C1    vartyp             I2
C1    iostat              I
C
C2    Subroutine LABFLG sets the MI status for  entry TMIP
C2    to indicate that it has been found.This prevents the same
C2    entity being found during sequential searches.
C
      include  'include/nbuff.inc'
C
      INTEGER*2 TMIP
      LOGICAL OK
      EXTERNAL DIR500,DIM500
C
      CALL DIR500(TMIP,OK)
      IF ( IMBUFF(1).LT.128 ) IMBUFF(1)=IMBUFF(1)+128
      CALL DIM500(TMIP,OK)
C
      END
C
C     -------------------------------------------
C
      SUBROUTINE LABNFG(TMIP)
C     =======================
C1    vartyp             I2
C1    iostat              I
C
C2    Subroutine LABNFG resets the MI status to clear the found
C2    status for the entity pointed to by TMIP.
C
      include  'include/nbuff.inc'
      include  'include/masti.inc'
C
      INTEGER*2 TMIP
      LOGICAL OK
      EXTERNAL DIR500,DIM500
C
      CALL DIR500(TMIP,OK)
      IF ( IMBUFF(1).GT.128 ) THEN
         IMBUFF(1)=IMBUFF(1)-128
         MIP=TMIP
         CALL DIM500(TMIP,OK)
      END IF
C
      END
C
C     ------------------------------------------
C
 
      SUBROUTINE LOAD2(WORKF)
C     =======================
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/style.inc'
      include  'include/wtov.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include  'include/lfu.inc'
      include  'include/macro.inc'
      include  'include/menun.inc'
      include  'include/product.inc'
      include  'include/wildc.inc'
      include  'include/ftypes.inc'
      include  'include/faults.inc'
      include  'include/vntable.inc'
      include  'include/lfont.inc'
      include  'include/viewport.inc'
C
C     Incase anybody is wondering why we need an include macro
C     the select pathnames are stored in there. They are needed
C     for the menu select files OK ?
C
      INTEGER*4 NLEN,DNUM,NLEN1,MEN4,MEN3,DNUM2
      INTEGER*2 MASK,ENDVAL,AND_2
      CHARACTER NAME*20
      CHARACTER*80 FILNM,ANS*1,SFILE
      LOGICAL OK,WORKF,QUIT,OK1,BAKDRG
      EXTERNAL NLEN,RSTALL,OPENNM,WORLD,DISFIL,GTCLRM,MNILV1,CRUNCH
      EXTERNAL NLEN1,GTDMEN,BAKDRG,AND_2,INATTX
C
C     set drawing name to null
      DRGNAM='         '
      FILTYP='DRAWING'
C     clear section data block
      CALL OSCLRM()
C
      IF (WORKF) THEN
C        load workfile only
         IF(CRASHF) THEN
             FILNM=PRNAM(1:NLEN(PRNAM))//'.save.drg'
         ELSE
             FILNM=PRNAM(1:NLEN(PRNAM))//'.drg'
         ENDIF
C
      ELSE
C        Ask user for filename
10        MEN3=3
          MEN4=4
CIBM|PC386
C          SFILE=PATHN(1)(:NLEN1(PATHN(1)))//'drg.msf'
C          CALL CRUNCH(SFILE)
C          IF ( MFILE ) THEN
C             FILNM=PATHN(1)(:NLEN1(PATHN(1)))//WILDC(1)
C             CALL CRUNCH(FILNM)
C             CALL DIRFIN(SFILE,FILNM,OK)
C          ENDIF
CC         call the display file routine to select a group of macro
CC         programs. If the file is not found then it will return with
CC         a not ok flag and allow user input
C          CALL CRUNCH(SFILE)
CIBM|PC386
          CALL GTCLRM(MEN4)
          DNUM=49
          CALL GTDMEN(DNUM,MEN4)
          DNUM=0
          CALL DISFIL(SFILE,FILNM,DNUM,OK,QUIT)
          CALL GTCLRM(MEN3)
          CALL GTCLRM(MEN4)
CIBM
C          IF ( MFILE ) CALL DELETE(SFILE,OK1)
CIBM
C         select a ne pathname for all the things
C          CALL MNILV1()
C          DISLAY=.TRUE.
C          CALL MNIDIS()
C         If we quit out of the Display file option menu
C         then let this routine handle it go back to the Start
C         routine just as if we came out of TCURS
          IF (QUIT) THEN
              PARFUN=0
              GOTO 200
          ENDIF
          DNUM=81
          IF(.NOT.OK) THEN
C         Either the MSF file has not been found or the
C         user has decided to select his own little drawing name
             CALL DPRMXP(DNUM,FILNM)
          ENDIF
C        if null file name assume abort required
         IF (NLEN(FILNM).EQ.0) THEN
            PARFUN=0
            GOTO 200
         END IF
C        ensure correct suffix in drawing name
 
         IF ( .NOT. BAKDRG(FILNM) )THEN
             CALL SUFFIX(FILNM,'.drg')
         ENDIF
      END IF
C     *********************************************************
C     will have to be moved out
C     verify name of file chosen drawing is correct
 
      DAXTYP = DAXDRG
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
 
C
C     save the filename globally
      DFNAM=FILNM
      IF (.NOT.OK) THEN
         PARFUN=0
         GOTO 200
      END IF
C
      LDFILE(CVPN)=1
C
C     read the drawing back now
C
      CALL RSTALL(OK)
C
C      WRITE(10,*) '[load2] NMIPOS=',NMIPOS
C
      IF ( .NOT. OK ) THEN
         DNUM=83
         CALL DCPRNT(DNUM)
      END IF
C
C     set the limits which the user left the drawing
      CALL WORLD(WXMIN,WYMIN,WXMAX,WYMAX)
C
C     set zoom previous to same to avoid errors
      LWXMIN=WXMIN
      LWYMIN=WYMIN
      LWXMAX=WXMAX
      LWYMAX=WYMAX
C
      CLOSE(UNIT=PARFUN,STATUS='KEEP')
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
C
C      IF ( .NOT. WORKF ) THEN
C         CALL BACKUP()
C      END IF
C
C     right let's set the text to go in the FONT, COLOR, and
C     THICK attribute menu cells after the colon
C
      CALL INATTX()
C
      CCMD='O'
      MEN=3
      CELLN=1
      RETURN
200   CONTINUE
      CCMD='Q'
      END
C
C     ------------------------------------------
C
      SUBROUTINE OPENWF(OK)
C     =====================
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine OPENWF opens all temporary work files
C2    required to support the draughting software.Returns the
C2    unit numbers connected in common block MASTI. The logical
C2    argument OK is returned TRUE if all files are successfully
C2    connected.All file pointers are initialized before return
C2    from this routine.
C
      include 'include/masti.inc'
      include 'include/swind.inc'
      include 'include/props.inc'
      include  'include/viewport.inc'
C
      LOGICAL OK
C
      CHARACTER*40 MFILN
C      EXTERNAL ACREAT
      INTEGER*2 ACREAT
      INTEGER*4 TEMP,I
C
C     Open the master index work file (record length 18)
      CALL OURSCR(TEMP,26,OK)
      MIFILU=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='MASTIP.TMP'
C      MIFILU=ACREAT(MFILN)
C     open the part data work file (record length 32)
      CALL OURSCR(TEMP,32,OK)
      PDFILU=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='PDFIL.TMP'
C      PDFILU=ACREAT(MFILN)
C     open the text data work file (record length 42)
      CALL OURSCR(TEMP,84,OK)
      TXFILU=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='TXFIL.TMP'
C      TXFILU=ACREAT(MFILN)
C
C     open the 2 Display data work fileS (record length 4)
      CALL OURSCR(TEMP,4,OK)
      DDFILU(1)=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='DFIL1.TMP'
C      DDFILU(1)=ACREAT(MFILN)
C      WRITE(10,*)'DFIL1.TMP =',DDFILU(1)
C
      CALL OURSCR(TEMP,4,OK)
      DDFILU(2)=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='DFIL2.TMP'
C      DDFILU(2)=ACREAT(MFILN)
C      WRITE(10,*)'DFIL2.TMP =',DDFILU(2)
      CALL OURSCR(TEMP,20,OK)
      RLFILU=TEMP
      IF (.NOT.OK) GOTO 999
      CALL OURSCR(TEMP,14,OK)
      SWINDU=TEMP
C      WRITE(100,*)'[NUDATAOR] SWINDU= ',SWINDU
      IF (.NOT.OK) GOTO 999
      CALL OURSCR(TEMP,84,OK)
      PRCFLU=TEMP
      IF (.NOT.OK) GOTO 999
      CALL OURSCR(TEMP,16,OK)
      PRIFLU=TEMP
      IF (.NOT.OK) GOTO 999
C      MFILN='RLFIL.TMP'
C      RLFILU=ACREAT(MFILN)
C
C      MFILN='SWIND.TMP'
C      SWINDU=ACREAT(MFILN)
C      WRITE(10,*)SWINDU
C     initialize all file pointers
C     MI pointers
      MIP=0
      NMIPOS=1
C     PD pointers
      PDFP=0
      NPDPOS=1
C     TX pointers
      TXTP=0
      NTXPOS=1
C     Disp pointers
      DO 10,I=0,MAXVP
         LDFILE(I)=1
 10   CONTINUE
C      WRITE(100,*)'PDFILU,MIFILU,DDFILU(1),DDFILU(2),TXFILU'
C      WRITE(100,*) PDFILU,MIFILU,DDFILU(1),DDFILU(2),TXFILU
 999  CONTINUE
C
      END
C
C     -------------------------------------
C
      SUBROUTINE RSTALL(OK)
C     =====================
C
C2    Subroutine RSTALL reads all the database
C2    contents from the currently selected drawing
C2    file.A header contains all the major parameters
C2    for the drawing such as scale.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/style.inc'
      include  'include/wtov.inc'
      include  'include/ndata.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include  'include/arcdat.inc'
      include  'include/props.inc'
      include  'include/viewport.inc'
      include  'include/lfu.inc'
C
      INTEGER*2 I,J,WUN
      INTEGER*4 I4,DNUM,BACKU,MAINU,TOTAL,PCENT,BLOW,RECORD
      INTEGER*4 NMIPOS4
      INTEGER*4 NPDPOS4
      INTEGER*4 NTXPOS4
      INTEGER*4 NRLPOS4
      INTEGER*4 NPRPOS4
      INTEGER*4 NPCPOS4
      CHARACTER*40 OUTPUT
C
      LOGICAL OK,BACK,DOABACK
C
      EXTERNAL RSTFHD,GETFHD,RSTDHD,GETDHD,RSTVHD,GETVHD,
     +         RSTPHD,GETPHD,RSTNHD,GETNHD,RSTMI0,GTPMSG,
     1         RSTPD0,RSTTX0,RSTRL0,DIR500,ADDMON,RSTPI0,
     2         RSTPC0,DEPRNT,RSTSEC,OSLODA

C SPB 220994 - Bits to see if you want to back up your drawings ...
      EXTERNAL DOABU
C SPB 220994 - EOF

C
      WUN=1
C     WRITE(UNIT=10,FMT=*)'[RSTALL]** STARTING TO RESTORE DRAWING **'
C     go to start of file
      REWIND(UNIT=PARFUN,ERR=99)
      MAINU=PARFUN
C
 
C     ***************************************
C         retrieve file header block
C     ***************************************
C
      CALL RSTFHD(OK)
C      WRITE(10,*) '[RSTALL] FILE HEADER'
      IF (.NOT.OK) GOTO 99
      CALL GETFHD()
C
C     ***************************************
C         retrieve drawing header block
C     ***************************************
C
      CALL RSTDHD(OK)
C      WRITE(10,*) '[RSTALL] DRAWING HEADER'
      IF (.NOT.OK) GOTO 99
      CALL GETDHD()
C
C     ***************************************
C         retrieve viewing header block
C     ***************************************
C
      CALL RSTVHD(OK)
C      WRITE(10,*) '[RSTALL] VIEWING HEADER'
      IF (.NOT.OK) GOTO 99
      CALL GETVHD()
C
C     ***************************************
C     retrieve drawing parameters header block
C     ***************************************
C
      CALL RSTPHD(OK)
      IF (.NOT.OK) GOTO 99
      CALL GETPHD()
C
C     ***************************************
C     retrieve section header block
C     ***************************************
C
      IF (OROTRV .GE. 2.201) THEN
c       from Rev 2.20 sections included
        CALL RSTSEC(OK)
C      WRITE(10,*) '[RSTALL] SECTION HEADER'
        IF (.NOT.OK) GOTO 99
      ELSE
C       ensure section block is clear
        CALL OSCLRM()
      END IF
C
C     ***************************************
C         retrieve database header block
C     ***************************************
C
      CALL RSTNHD(OK)
C      WRITE(10,*) '[RSTALL] DATABASE HEADER'
      IF (.NOT.OK) GOTO 99
      CALL GETNHD()
C
C
C     ***************************************
C        end of header block construction
C     ***************************************
C
CAPOLLO|SUN
C SPB 210994 - Add bits in for conditional backups ...
      CALL DOABU(DOABACK)
      IF (DOABACK) THEN
        CALL BACKUP(OUTPUT,BACKU,BACK)
      ELSE
C       set flags to ensure backup is not attempted below
        BACK=.FALSE.
	BACKU=-1
      END IF
C SPB 210994 - EOF

CC
      IF ( .NOT.BACK ) THEN
         IF ( BACKU.GE.0 ) THEN
            DNUM=412
            CALL DEPRNT(DNUM)
         END IF
      ELSE
         CALL OPENNM(OUTPUT,BACKU,.FALSE.,BACK)
         IF ( .NOT. BACK ) THEN
            DNUM=412
            CALL DEPRNT(DNUM)
         ELSE
CC           creating back un
            PARFUN=BACKU
CC           save file header
            CALL SAVFHD(OK)
            IF (.NOT.OK) GOTO 99
CC           save drawing header
            CALL SAVDHD(OK)
            IF (.NOT.OK) GOTO 99
CC           save view header
            CALL SAVVHD(OK)
            IF (.NOT.OK) GOTO 99
CC           save parameter header
            CALL SAVPHD(OK)
            IF (.NOT.OK) GOTO 99
CC           save section header
            CALL SAVSEC(OK)
            IF (.NOT.OK) GOTO 99
CC           save database header
            CALL SAVNHD(OK)
            IF (.NOT.OK) GOTO 99
            PARFUN=MAINU
         END IF
      END IF
CAPOLLO|SUN
      I4=1
      CALL GTPMSG(DFNAM,I4)
 
      CALL GTPMSG('Loading Master Index Data',I4)
C
C     set next position pointers to correct limits
      NMIPOS=NMIPOS+WUN
      NPDPOS=NPDPOS+WUN
      NTXPOS=NTXPOS+WUN
      NRLPOS=NRLPOS+WUN
      NPRPOS=NPRPOS+WUN
      NPCPOS=NPCPOS+WUN
c
      NMIPOS4 =NMIPOS+WUN
      NPDPOS4 =NPDPOS+WUN
      NTXPOS4 =NTXPOS+WUN
      NRLPOS4 =NRLPOS+WUN
      NPRPOS4 =NPRPOS+WUN
      NPCPOS4 =NPCPOS+WUN
      TOTAL = NMIPOS4+NPDPOS4+NTXPOS4+NRLPOS4+NPRPOS4+NPCPOS4
      RECORD = (TOTAL/100)+1
      BLOW=0
C
C     now get down to reading the part data it's self
C     start with the MI data
      DO 10 I=1,NMIPOS-WUN
C        read the MI record
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
         CALL RSTMI0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
CC           creating back un
            PARFUN=BACKU
            CALL SAVMI0(I,OK)
            IF ( .NOT. OK ) GOTO 99
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 10   CONTINUE
C
      CALL GTPMSG('Loading Geometric Data',I4)
C
C     now read all the coordinate data
C     all the coordinate data
      DO 20 I=1,NPDPOS-WUN
C        read the PD record
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
         CALL RSTPD0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVPD0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 20   CONTINUE
C
      CALL GTPMSG('Loading Text Data',I4)
C     now read all the text data
      DO 30,I=1,NTXPOS-WUN
C        read the text record
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
         CALL RSTTX0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVTX0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 30   CONTINUE
C
      CALL GTPMSG('Loading Component Relation Data',I4)
C
C     now read the relation data
      DO 31 I=1,NRLPOS-WUN
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
C        read the RL record
         CALL RSTRL0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVRL0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 31   CONTINUE
C
      CALL GTPMSG('Loading Property Data',I4)
C
C     now read the property index data
      DO 32 I=1,NPRPOS-WUN
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
C        read the PR record
         CALL RSTPI0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVPI0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 32   CONTINUE
C
      CALL GTPMSG('Loading Property Character Data',I4)
C
C     now read the property character data
      DO 33 I=1,NPCPOS-WUN
         BLOW=BLOW+1
         IF(MOD(BLOW,RECORD).EQ.0) THEN
             PCENT = BLOW*100/TOTAL+1
             CALL DMETER(PCENT)
         ENDIF
C        read the PC record
         CALL RSTPC0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVPC0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 33   CONTINUE
C
      CALL GTPMSG('Loading ALL Layer Data',I4)
 
      DO 34 I=1,NLYPOS
C        read the LAYER records
         CALL RSTLY0(I,OK)
         IF ( .NOT. OK ) GOTO 99
CAPOLLO|SUN
         IF ( BACK ) THEN
C           creating back un
            PARFUN=BACKU
            CALL SAVLY0(I,OK)
            PARFUN=MAINU
         END IF
CAPOLLO|SUN
 34   CONTINUE
 
C     clear layer visibility
      DO 35,I=0,255
         TLAYER(I)=0
         VLAYER(I)=.FALSE.
 35   CONTINUE
C
C     set layer visibility
      DO 40,I=1,NMIPOS-WUN
         CALL DIR500(I,OK)
C         VLAYER(IMBUFF(4))=.TRUE.
C        recontruct the monitor file.
         CALL ADDMON(IMBUFF(4))
 40   CONTINUE
C     set construction layer
C     This is to stop layer wandering from its
C     normal range.
      CLAYER=ABS(MOD(CLAYER+0,256))
      VLAYER(CLAYER)=.TRUE.
C
      CALL GTPMSG('Drawing Loaded. Please Wait.....',I4)
C     WRITE(UNIT=10,FMT=*)'[RSTALL]** DRAWING RESTORED **'
      GOTO 98
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*) '[RSTALL] READ ERROR ON INPUT FILE'
C     that's the lot for now,file complete
 98   CONTINUE
C
C     Save current drawing extents of viewport 0
      CALL TRNDP()
C     Save current layering information of viewport 0
      CALL SAVLAY(CVPN,OK)
C     Save current grid information of viewport 0
      CALL SAVGRD(CVPN,OK)

C     now rebuild any viewports please
      CALL VIEW_REBUILD(OK)
      IF(.NOT.OK) THEN
C         No bitmaps are here lets reset The whole thing
          CALL INITVP()
          OK = .TRUE.
      ELSE
C         if backup file then restore the drawing
          IF( BACK) THEN
              PARFUN = BACKU
              CALL VIEW_BUILD(OK)
              PARFUN = MAINU
          ENDIF
      ENDIF
C     make sure we restore main drawing transform
      CVPN = 0
      VXMIN = VPXMIN
      VYMIN = VPYMIN
      VXMAX = VPXMAX
      VYMAX = VPYMAX
C     Restore main tranform
      CALL TRNRST(CVPN)
C     Restore current layering information of viewport 0
      CALL GETLAY(CVPN,OK)
C     Restore current grid information of viewport 0 
      CALL GETGRD(CVPN,OK)
C
C
CAPOLLO|SUN
      IF ( BACK ) CLOSE(UNIT=BACKU,STATUS='KEEP')
CAPOLLO|SUN
C
C     load bitmaps if required
      CALL OSLODA()
      CALL METERC()
C     rsetore views
C
      END
C
C     ----------------------------------------
C

      SUBROUTINE SAFESC(REGEN,LT)
C     ===========================
C
C1    vartype             L   I2
C1    iostatus            I   I
C
C2    Subroutine SAFESC ensures the integrity of the
C2    drawing database during a packing operation,by testing
C2    for the existance of external masters for all symbols
C2    and components used in the drawing.If external masters
C2    do exist,then the internal master is deleted from the
C2    database,in preparation for the regeneration function
C2    which takes place after a database pack.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/ftypes.inc'
C
      INTEGER*2 P,PMIP,TIP,IP,IV,TENT,LT,TTMIP
      INTEGER*4 FN,FT,ST,DAXNAM
      LOGICAL OK,REGEN
      CHARACTER*80 TNAME
C
      EXTERNAL DAXNAM
C
C     set scratch file to zero
      CALL ZRFLAG()
C     cycle through text for names of comps or symbs
      P=0
 10   CONTINUE
      P=P+1
      CALL DTR500(P,OK)
      PMIP=ICBUFF(1)
      CALL DIR500(PMIP,OK)
      IF (IMBUFF(1).NE.100) THEN
C        entity is valid part of database
         TENT=IMBUFF(2)
         IF (TENT.EQ.COMPM) THEN
C           Component master
            FN=1
C           set file type for external master validation
            DAXTYP = DAXCMP
            FT=2
         ELSE IF (TENT.EQ.SYMBM) THEN
            FN=2
C           set file type for extarnal master validation
            FT=3
            DAXTYP = DAXSYM
         ELSE
            FN=0
         END IF
C
         IF (FN.NE.0) THEN
C           have a master to deal with
            TNAME=CBUFF
C           test for use within the drawing
            CALL INSC10(TNAME,FN+2,TIP,OK)
            IF (OK) THEN
C               WRITE(10,*)' USED MASTER:',TNAME
C              master is required,test for existance
C              externally
 
               CALL EXFILE(TNAME,FT,ST)
               IF (ST.EQ.0 .AND.REGEN) THEN
C                  WRITE(10,*)' DELETE LOCAL MASTER:',TNAME
C                 external master is present
C                 can delete local master for regen
                  IP=1
                  IV=100
                  CALL DIM501(PMIP,IP,IV,OK)
               ELSE
C                  WRITE(10,*)' PRESERVE LOCAL MASTER:',TNAME
C                 get pointer to first entity of master
                  TTMIP=NMIPOS
C                 master does not exist externally
C                 use the internal copy
                  CALL GETCSM(PMIP,OK)
C                 copy all the master geometry
                  CALL MOVMAS(.TRUE.)
C                 create new master,as copy of old
                  CALL DER500(PMIP,OK)
                  CALL DEW056(FN,TTMIP,.TRUE.,PMIP,OK)
C                  WRITE(10,*)' CREATE NEW MASTER:',TNAME
               END IF
            END IF
         END IF
      END IF
      IF (P.LT.LT) GOTO 10
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE SAVALL(WORKF,PACK,REGEN,OK)
C     ======================================
C
C1    vartype             L    L     L   L
C1    iostatus            I    I     I   O
C
C2    Subroutine SAVALL places all the database
C2    contents into the currently selected drawing
C2    file.A header contains all the major parameters
C2    for the drawing such as scale.WORKF is passed true
C2    if a workfile is to be created,false if a normal
C2    drawing file. PACK is passed true if a database
C2    packing operation is to be forced.
C2    OK returns completion status.
C
      include  'include/masti.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include  'include/props.inc'
      include  'include/ftypes.inc'
      include  'include/faults.inc'
      include  'include/config.inc'
      include  'include/viewport.inc'
      include  'include/vntable.inc'
C
      INTEGER*2 I,J,WUN
      INTEGER*4 NLEN,DNUM
      CHARACTER*3 ANS
      REAL REAL
      REAL X,Y
      LOGICAL OK,OKCHK,WORKF,PACK,YESOK,REGEN,OKI,YES,DRGCOMPACTED
C     INTRINSIC REAL
      EXTERNAL SAVMI0,SAVPD0,SAVTX0,DCPRNT,CMPACT,SAVRL0
      EXTERNAL SAVPI0,SAVPC0,NLEN,YESOK,CPRMXP
      EXTERNAL MAKFHD,MAKDHD,MAKVHD,MAKPHD,MAKNHD
      EXTERNAL SAVFHD,SAVDHD,SAVVHD,SAVPHD,SAVNHD
C
      WUN=1
CAPOLLO
C     if fault handler in operation then fuck a compact
C     lets just get this fucker out
      IF (DAXCAD_FAULT_OCCURED) GOTO 2000
CAPOLLO
C     viewport bit. We need to reset to main view
      X = 0.0
      Y = 0.0
      CALL SETDP(X,Y,.TRUE.)
C     now check everything else
      DRGCOMPACTED=.FALSE.
      IF( CHEKIT) THEN
         CALL INTGRY(OKI)
         IF(.NOT.OKI) THEN
           DNUM = 693
           CALL DCPRNT(DNUM)
         END IF
      END IF
      IF (.NOT.WORKF) THEN
C        Check if percentage and outwith percentage threadshold 
C    or        if not percentage and outwith entity threashold
C    or        if pack
         IF ((PERCNT.AND.
     +        (100*REAL(DELENT)/REAL(NMIPOS).GT.REAL(CMPVAL)))
     +       .OR.
     +       (.NOT.PERCNT.AND.DELENT.GT.CMPVAL)
     +       .OR.PACK) THEN
            IF (PACK) THEN
               IF (NEWCMP) THEN
                  CALL CMPCT1(REGEN,OK)
               ELSE
C                 force packing of drawing anyway
C But if the user didn't want it or an error occured,
C continue on writing it out as normal ...
                  CALL CMPACT(REGEN,OK)
                  IF (OK) DRGCOMPACTED=.TRUE.
               END IF
            ELSE
               DNUM=435
               CALL DCPRNT(DNUM)
               DNUM=440
               CALL CONFIRMATION(DICT01(DNUM),.TRUE.,YES)
               IF (YES) THEN
C                 More then 20% of this drawing is deleted
                  DNUM=79
                  CALL DCPRNT(DNUM)
                  IF (NEWCMP) THEN
                     CALL CMPCT1(REGEN,OK)
                  ELSE
C                 force packing of drawing anyway
C But if the user didn't want it or an error occured,
C continue on writing it out as normal ...
                     CALL CMPACT(REGEN,OK)
                     IF (OK) DRGCOMPACTED=.TRUE.
                  END IF
               END IF
            END IF
         END IF
      END IF
2000  CONTINUE
      IF (DRGCOMPACTED) RETURN

C     go to start of file
      REWIND(UNIT=PARFUN,ERR=99)
C
C     ***************************************
C         create file header block
C     ***************************************
C
C     ensure filtype is DRAWING
      DAXTYP = DAXDRG
      FILTYP='DRAWING'
      CALL MAKFHD()
      CALL SAVFHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C         create drawing header block
C     ***************************************
C
      CALL MAKDHD()
      CALL SAVDHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C         create viewing header block
C     ***************************************
C
      CALL MAKVHD()
      CALL SAVVHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C     create drawing parameters header block
C     ***************************************
C
      CALL MAKPHD()
      CALL SAVPHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C     create section header block
C     ***************************************
C
      CALL SAVSEC(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C         create database header block
C     ***************************************
C
      CALL MAKNHD()
      CALL SAVNHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     ***************************************
C        end of header block construction
C     ***************************************
C
C     now get down to writing the part data it's self
C     start with the MI data
      OKCHK=.TRUE.
      DO 10 I=1,NMIPOS-1
C        write the MI record
         CALL SAVMI0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 10   CONTINUE
C
C     now save all the coordinate data
      OKCHK=.TRUE.
      DO 20 I=1,NPDPOS-WUN
C        write the PD record
         CALL SAVPD0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 20   CONTINUE
C
C     now save all the text data
      OKCHK=.TRUE.
      DO 30,I=1,NTXPOS-WUN
C        write the text record
         CALL SAVTX0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 30   CONTINUE
C
C     now save all the relation data
      DO 31 I=1,NRLPOS-WUN
C        write the MI record
         CALL SAVRL0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 31   CONTINUE
C
C     now save all the property index data
      DO 32 I=1,NPRPOS-WUN
C        write the MI record
         CALL SAVPI0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 32   CONTINUE
C
C     now save all the property character data
      DO 33 I=1,NPCPOS-WUN
C        write the MI record
         CALL SAVPC0(I,OK)
         IF ( .NOT. OK ) GOTO 99
 33   CONTINUE
C
C     now save all the name layer.
      DO 34 I=0,255
C        write the MI record
         IF ( NLEN(LNAME(I)).GT.0 ) THEN
            CALL SAVLY0(I,OK)
         END IF
         IF ( .NOT. OK ) GOTO 99
 34   CONTINUE
C
CAPOLLO|SUN
      IF (.NOT.WORKF) THEN
          CALL VIEW_BUILD(OK)
      END IF
CAPOLLO|SUN
      OK=.TRUE.
C
      RETURN
C
 99   CONTINUE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*) '[SAVALL] Error on writing to drawing file'
C     that's the lot for now,file complete
C
C
C
      END
C
C     ----------------------------------------
C
      SUBROUTINE STORE2(WORKF)
C     ========================
C1    no arguments required
C
C2    Saves the current contents of the data base in a disc file
C2    of the name given by the user in response to a request from
C2    this subroutine.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include 'include/lfu.inc'
      include 'include/product.inc'
      include  'include/faults.inc'
      include  'include/ftypes.inc'
      include  'include/macro.inc'
C
      INTEGER*4 NLEN,D
      INTEGER*4 ST
      CHARACTER*40 FILNM,ANS*1
      LOGICAL OK,WORKF,DELOK
      EXTERNAL NLEN,SAVALL,OPENNM
C
      IF (NMIPOS .EQ. 1) RETURN
C
      DAXTYP = DAXDRG
CAPOLLO
      IF (DAXCAD_FAULT_OCCURED) THEN
          FILNM=PRNAM(1:NLEN(PRNAM))//'.save.drg'
          CALL TOOLSSETCWD(STARTUPDIR,NLEN(STARTUPDIR),ST)
          GOTO 10
      ENDIF
CAPOLLO
      FILTYP='DRAWING'
C
      IF (WORKF) THEN
C        save workfile only
         FILNM=PRNAM(1:NLEN(PRNAM))//'.drg'
         D=80
         CALL DCPRNT(D)
      ELSE
 5       CONTINUE
         D=81
         CALL DPRMXP(D,FILNM)
C        if null file name assume abort required
         IF (NLEN(FILNM).EQ.0 ) THEN
            PARFUN=0
            RETURN
         END IF
         CALL SUFFIX(FILNM,'.drg')
      END IF
C
 10   CONTINUE
      CALL OPENNM(FILNM,PARFUN,.FALSE.,OK)
C
      IF ( .NOT. OK ) THEN
C         Drawing file cannot be opned
          PARFUN = 0
          RETURN
      ENDIF

C
 6    CONTINUE
C
C     go save the drawing
      IF (.NOT.WORKF) THEN
C        ensure drawing name is correct
         DFNAM=FILNM
      END IF
C
      CALL SAVALL(WORKF,.FALSE.,.FALSE.,OK)
      IF (.NOT.OK) THEN
          WRITE(UNIT=10,FMT=*)'Error writing drawing file'
C
          CLOSE(UNIT=PARFUN,STATUS='DELETE',ERR=201)
          PARFUN = 0
      ELSE
          CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=201)
      ENDIF
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
      RETURN
C
 201  CONTINUE
      D=82
      CALL DCPRNT(D)
C
      END
C
C     -----------------------------------------
C
