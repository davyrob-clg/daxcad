C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insert2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CLROPF()
C     SUBROUTINE CLRSCL()
C     SUBROUTINE INMOV(STATUS)
C     SUBROUTINE INSC00()
C     SUBROUTINE INSC01()
C     SUBROUTINE INSC02(FN)
C     SUBROUTINE INSC03(PMIP,FN,OPTION,QUIT,OK)
C     SUBROUTINE INSC10(CMPNAM,FN,PMIP,OK)
C     SUBROUTINE INSC90(INSNAM,INSTYP,INSMIP,IPRELP,INSFND)
C     SUBROUTINE INSP00()
C     SUBROUTINE INSP03(FILNM,OPTION,QUIT,OK)
C     SUBROUTINE INSP04(OPTION,QUIT)
C     SUBROUTINE INSP05(FNAM,FTYP,TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
C     SUBROUTINE INSP06()
C     SUBROUTINE INSP07(FN,TTMIP,TYPE)
C     SUBROUTINE INSP55(FNAM,FTYP,INUNIT,TTMIP,TTPDP,TTTXP,TTRLP,
C     SUBROUTINE INSS00()
C     SUBROUTINE INSS01()
C     SUBROUTINE MNLP01(FN)
C     SUBROUTINE RSTMOV()
C     SUBROUTINE SAVMOV()
C     SUBROUTINE SETTFM(M)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CLROPF()
C     ===================
C1    no arguments required
C
C2    Subroutine CLROPF clears all OP flags
C2    for use in transformation functions.
C
      include 'include/movdat.inc'
C
      INTEGER I
C
C     clear the transform flags
      DO 50 I=1,12
         OPFLAG(I)=.FALSE.
 50   CONTINUE
      END
C
      SUBROUTINE CLRSCL()
C     ===================
C1    no arguments required
C
C2    Subroutine CLROPF clears all OP flags
C2    for use in transformation functions.
C
      include 'include/movdat.inc'
C
      INTEGER I,J
C
C     clear the transform flags
      DO 50 I=1,10
         DO 100 J=1,3
            REFDAT(I,J)=0.0
100      CONTINUE
 50   CONTINUE
      END
C
      SUBROUTINE INMOV(STATUS)
C     =========================
C
C1    VARTYPE            L
C1    IOSTATUS           I
C
      include 'include/save.inc'
      include 'include/lfu.inc'
      include 'include/library.inc'
C
      INTEGER*4 RECL,WUN,NLEN,NLEN1
      LOGICAL OK,STATUS
      CHARACTER*80 TEMP
C
      EXTERNAL OURSCR
      EXTERNAL NLEN,NLEN1
 
      WUN=1
C      WRITE(10,*) '[INMOV] STATUS= ',STATUS
      IF(.NOT.STATUS) THEN
 
CIBM|PC386
C
C          CALL DELETE(SAVNAM,OK)
C
CIBM|PC386
 
C
CAPOLLO|SUN
C
          CLOSE(UNIT=SUNIT)
C
CAPOLLO|SUN
          RETURN
      ENDIF
 
CAPOLLO|SUN
      CALL FINDU1(SUNIT,OK)
      IF(.NOT.OK) GOTO 17
      OPEN (UNIT=SUNIT,FORM='UNFORMATTED',
     +       ACCESS='DIRECT',RECL=60,STATUS='SCRATCH',ERR=17)
      WRITE(UNIT=SUNIT,REC=1) WUN
CAPOLLO|SUN
 
 
 
CIBM
C      SAVNAM=LIBRARY(1:NLEN1(LIBRARY))//'\RESTORE'
CIBM
CPC386
C      SAVNAM=LIBRARY(1:NLEN1(LIBRARY))//'\\RESTORE'
CPC386
CIBM|PC386
C      CALL FINDU1(SUNIT,OK)
C      CALL CRUNCH(SAVNAM)
C      OPEN (UNIT=SUNIT,FILE=SAVNAM,FORM='UNFORMATTED',
C     +       ACCESS='DIRECT',RECL=60,STATUS='UNKNOWN',ERR=17)
C
C
C      LFU(SUNIT)=.TRUE.
CC      set nest level
C      WRITE(UNIT=SUNIT,REC=1) WUN
C      CLOSE(UNIT=SUNIT,STATUS='KEEP')
C      LFU(SUNIT)=.FALSE.
CIBM|PC386
 
 
 
      RETURN
17    CALL DEPRNT(287)
C
      END
 
      SUBROUTINE INSC00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT COMPONENT option
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL INSC01,GTMCLO,CLRPEW,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C     clear the transform flags
      CALL CLROPF()
C     call mediator routine
      CALL INSC01()
C     ensure PART option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSC01()
C     ===================
C1    no arguments required
C
C2    controls operation of the INSERT COMPONENT option
C
      INTEGER FN
      EXTERNAL INSC02,MNLP01
C
C     set function number for COMP
      FN=1
C     initialize INSERT PART/COMP/SYMB option menu
      CALL MNLP01(FN)
C     go insert the component
      CALL INSC02(FN)
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSC02(FN)
C     =====================
C
C1    vartype           I4
C1    iostatus          I
C
C2    Subroutine INSC02 is the main working routine
C2    for the INSERT COMPONENT/SYMBOL/PART function.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/params.inc'
      include 'include/filunit.inc'
      include 'include/props.inc'
      include 'include/lfu.inc'
      include 'include/macro.inc'
      include 'include/save.inc'
      include 'include/vntable.inc'
      include 'include/ftypes.inc'
      include 'include/viewport.inc'
C
C     Incase anybody is wondering why we need an include macro
C     the select pathnames are stored in there. They are needed
C     for the menu select files OK ?
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP,TLDFIL
      INTEGER*2 TMIP,TPDP,TTXP,TRLP,TPRP,TPCP,TDFIL,TTMIP2
      INTEGER*2 TP2,TP3,II,D1,D2,PMIP,L
      INTEGER*4 NLEN,C,TMEN,TCELL,FN,DNUM1,DNUM2,I,J,NLEN1,PHNUM
      INTEGER*4 TYPE
      REAL X,Y,RAD,REFTMP(2,3),REFT73
      DOUBLE PRECISION DN
      LOGICAL OK,OPTION,QUIT,CVERFY,INSFLG,OPFTMP(4),OK1
      CHARACTER*80 FILNM,FILETP,CCBUFF,SFILE,EXT*4
      CHARACTER TOKEN*1
      EXTERNAL INSC03,NLEN,AEXPRN,RAD,INSC10
      EXTERNAL CLROPF,INSP07,CRUNCH,NLEN1
C
C     set default scale factors
      TYPE=1
 5    CONTINUE
C     Get the filenames for the MENU SELECT FILES
      REFDAT(1,3)=1.0
      REFDAT(2,3)=0.0
      IF (FN.EQ.1) THEN
C        Components
         DNUM1=238
         FILETP='COMPM'
         FILTYP='COMPM'
         PHNUM=2
         DAXTYP = DAXCMP
         EXT='.cmp'
      ELSE IF (FN.EQ.2) THEN
C        Symbols
         DNUM1=361
         FILETP='SYMBM'
         FILTYP='SYMBM'
         DAXTYP = DAXSYM
         PHNUM=3
         EXT='.sym'
      ELSE
C        Parts
         DNUM1=48
         FILETP='PART'
         FILTYP='PART'
         DAXTYP = DAXPRT
         PHNUM=1
         EXT='.prt'
      END IF
C
 
      DNUM2=58
C
C     clear the insert flag
      INSFLG=.FALSE.
C
C     Force the user for a component name first
      MEN=3
      CCMD='n'
      GOTO 11
C
 10   CONTINUE
      CALL DCPRNT(DNUM1)
      CALL TCURS(C,X,Y)
C
 11   CONTINUE
C
      IF (MEN.EQ.3) THEN
 20       CONTINUE
          TMEN=MEN
          TCELL=CELLN
          IF (CCMD.EQ.'n') THEN
              CALL GTHFMC(MEN,CCMD,TCELL)
              CALL SAVMOV()
              CALL CLROPF()
              CALL CLRSCL()
              CALL GTMCHI(MEN,TCELL)
C***************************************************************
C                     NAME OPTION                              *
C***************************************************************
C     go get a file name for the part
C     call the display file routine to select a group of macro
C     programs. If the file is not found then it will return with
C     a not ok flag and allow user input
C              SFILE=PATHN(PHNUM)(:NLEN1(PATHN(PHNUM)))//EXT(2:4)//'.msf'
C              FILNM=PATHN(PHNUM)(:NLEN1(PATHN(PHNUM)))//WILDC(PHNUM)
C              CALL CRUNCH(FILNM)
C              CALL CRUNCH(SFILE)
C              CALL DIRFIN(SFILE,FILNM,OK)
C             clear the part name
              FILNM='                                    '
              CALL DISFIL(SFILE,FILNM,PHNUM,OK,QUIT)
C             delete file if using auto file generation.
C              IF ( MFILE ) CALL DELETE(SFILE,OK1)
C             Bring back the original menu
              CALL GTCLRM(3)
C             If we quit out of the Display file option menu
C             then let this routine handle it go back to the Start
C             routine just as if we came out of TCURS
              IF (QUIT) GOTO 12
C             fold down for consistency
              IF(.NOT.OK) THEN
C             Either the MSF file has not been found or the
C             user has decided to select his own little component or
C             whatever name.
                  CALL DPRMXP(DNUM2,FILNM)
                  IF (NLEN(FILNM).EQ.0) THEN
 
                     GOTO 12
                  ENDIF
                  L = NLEN(FILNM)
                  IF(FILNM(L:L).EQ.'*') CALL SUFFIX(FILNM,EXT)
              ENDIF
C             add extension required.
 
CAPOLLO
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C              CALL UNFOLD(FILNM)
CAPOLLO
 
              IF (FN.LE.2) THEN
C              test for existance of master within database
               CALL INSC10(FILNM,FN,PMIP,OK)
               IF (.NOT.OK) THEN
C                 does not already exist in the drawing
C                 test existance of external component file
                  CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
C
                  IF (.NOT.OK) THEN
C                    cannot find the file
                     FILNM='      '
                     CALL GTMCLO(TMEN,TCELL)
                     CALL MNLP01(FN)
                     GOTO 5
                  ELSE
C                    save the current record pointers
                     TTMIP=NMIPOS
                     TTPDP=NPDPOS
                     TTTXP=NTXPOS
                     TTRLP=NRLPOS
                     TTPRP=NPRPOS
                     TTPCP=NPCPOS
C                    must go get the file and enter to database
                     CALL INSP05(FILNM,FILETP,
     +               TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
                     CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C                     LFU(PARFUN)=.FALSE.
CIBM
C                    find the MIP for the component master
C                    read it from the first entity of the master
                     CALL DIR500(TTMIP,OK)
C                    save parent pointer to header
                     D1=IMBUFF(8)
C                    read header MI entry
                     CALL DIR500(D1,OK)
C                    now read relation header entry for master
                     CALL DRR950(IMBUFF(10),OK)
C                    save MI pointer to component master
                     PMIP=RLBUFF(3)
C                    read text of comp/symb master name
                     CALL DIR500(PMIP,OK)
                     TP2=IMBUFF(9)
                     CALL DTR500(TP2,OK)
C                    ensure master name is corrected to filename
C                    used for reference,prevents file-name changes
C                    conflicting with internal comp names.
                     CBUFF=FILNM
                     CALL DTM500(TP2,PMIP,OK)
C                    transform data for all master data
C                    in component pulled in from file
                     TMIP=NMIPOS
                     NMIPOS=CMIPOS
                     CALL INSP07(FN,TTMIP,TYPE)
                     NMIPOS=TMIP
                  END IF
               END IF
            ELSE
C              search for part file
C              test existance of file
               CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
               IF (.NOT.OK) THEN
C                 cannot find the file
                  FILNM='      '
                  CALL GTMCLO(TMEN,TCELL)
                  GOTO 12
               ELSE
                  CLOSE(UNIT=PARFUN,STATUS='KEEP')
CIBM
C                  LFU(PARFUN)=.FALSE.
CIBM
               END IF
            END IF
C           refresh the cells
 12         CONTINUE
            CALL RSTMOV()
            IF(QUIT) RETURN
            CALL MNLP01(FN)
C           put current name in below the Name symbol
            TOKEN='n'
            CALL GTHFMC(3,TOKEN,TCELL)
            TCELL=TCELL+1
            CALL GTCLRC(3,TCELL)
            TOKEN='X'
            CALL GTPMEN(FILNM,TOKEN,3,TCELL)
         ELSE IF (CCMD.EQ.'R') THEN
C***************************************************************
C                    LOCATE OPTION                             *
C***************************************************************
C           repeat insert called for
 222        CONTINUE
            IF (FILNM.NE.' ') THEN
C              save display file state
               TLDFIL=LDFILE(CVPN)
C              save the current record pointers
               TTMIP=NMIPOS
               TTPDP=NPDPOS
               TTTXP=NTXPOS
               TTRLP=NRLPOS
               TTPRP=NPRPOS
               TTPCP=NPCPOS
C
C              save state of transform data
                CALL SAVMOV()
C               OPFTMP(1)=OPFLAG(1)
C               OPFTMP(2)=OPFLAG(2)
C               OPFTMP(3)=OPFLAG(6)
C               OPFTMP(4)=OPFLAG(7)
C               DO 881 I=1,2
C                  DO 882 J=1,3
C                     REFTMP(I,J)=REFDAT(I,J)
C 882              CONTINUE
C 881           CONTINUE
C               REFT73=REFDAT(7,3)
C
               IF (FN.LE.2) THEN
C                 insert the instance component/symbol
                  CALL INSC03(PMIP,FN,OPTION,QUIT,OK)
               ELSE
C                 insert the part
                  CALL INSP03(FILNM,OPTION,QUIT,OK)
               END IF
               IF (OPTION.OR.QUIT) THEN
                  CALL GTMCLO(TMEN,TCELL)
                  IF ( OPTION.AND.CCMD.EQ.'R' ) THEN
                     CALL RSTMOV()
                     CALL GTMCHI(MEN,CELLN)
                     GOTO 222
                  END IF
                  TTMIP=TMIP
                  TTPDP=TPDP
                  TTTXP=TTXP
                  TTRLP=TRLP
                  TTPRP=TPRP
                  TTPCP=TPCP
                  TLDFIL=TDFIL
                  CALL RSTMOV()
                  IF(MEN.NE.3) RETURN
                  GOTO 20
               END IF
C              set flag to indicate insert done
               TDFIL=TLDFIL
               TMIP=TTMIP
               TPDP=TTPDP
               TTXP=TTTXP
               TRLP=TTRLP
               TPRP=TTPRP
               TPCP=TTPCP
               INSFLG=.TRUE.
               CALL RSTMOV()
C              retrieve state of transform data
C               OPFLAG(1)=OPFTMP(1)
C               OPFLAG(2)=OPFTMP(2)
CC               OPFLAG(6)=OPFTMP(3)
C               OPFLAG(7)=OPFTMP(4)
C               DO 884 I=1,2
C                  DO 883 J=1,3
C                     REFDAT(I,J)=REFTMP(I,J)
C 883              CONTINUE
C 884           CONTINUE
C               REFDAT(7,3)=REFT73
 
               GOTO 222
            ELSE
C              no file name assigned
               CALL DEPRNT(47)
            END IF
         ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C                    CANCEL OPTION                             *
C***************************************************************
C           erase inserted part if allowed
            IF (INSFLG) THEN
               CALL SAVMOV()
C              erase the part
C              place Mi pointers in SWIND for erase
               NDATA=0
               DO 40 II=TTMIP,NMIPOS-1
C                 save the MI pointer in work file
                  CALL SSFLAG(II,0.0,0.0,D1,D2,.FALSE.)
 40            CONTINUE
C              ensure flags are not drawn
               VNDATA=NDATA+1
C              erase the part
               CALL MASDEL()
C              recover the current record pointers
               NMIPOS=TTMIP
               NPDPOS=TTPDP
               NTXPOS=TTTXP
               NRLPOS=TTRLP
               NPRPOS=TTPRP
               NPCPOS=TTPCP
C              recover display file state
C              Only reset the display file if in main view
               IF(CVPN.EQ.0) THEN
                   LDFILE(CVPN)=TLDFIL
               ELSE
                   VPLIM=VPLIM-1
               ENDIF
C              set flag to indicate erase done
               INSFLG=.FALSE.
               CALL RSTMOV()
            ELSE
C              no erase allowed
               CALL DEPRNT(241)
            END IF
         ELSE IF (CVERFY(CCMD,'Ssrp')) THEN
C***************************************************************
C                     TRANSFORM OPTIONS                        *
C***************************************************************
            CALL INSP06()
            GOTO 10
         ELSE IF (CCMD.EQ.'X') THEN
C            print out the pathname
             CALL CPRINT(DICT01(482)(:NLEN(DICT01(482)))//FILNM)
             CALL GTMCLO(TMEN,TCELL)
         END IF
C***************************************************************
C                     END OF OPTIONS                           *
C***************************************************************
 99      CONTINUE
         CALL GTMCLO(TMEN,TCELL)
         GOTO 10
      ELSE IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
C        return to previous level
         RETURN
      ELSE
C        invalid cursor hit
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSC03(PMIP,FN,OPTION,QUIT,OK)
C     =======================================
C
C1    vartype            I2   I    L     L  L
C1    iostatus           I    I    O     O  O
C
C2    Subroutine INSC03 adds a Component/Symbol
C2    instance to the database.PMIP passes the
C2    MI of the Master geometry for the instance,
C2    FN passes the type of instance being added.
C
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/wrkdat.inc'
      include 'include/masti.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL M(3,3)
      INTEGER I,FN
      INTEGER*2 PMIP,TMIP,ITYPE
      LOGICAL OK,OPTION,QUIT
      EXTERNAL INSP04,DEW566,SETTFM,MV0003,DRW066,DEPRNT
C
      IF (FN.EQ.1) THEN
         ITYPE=COMPI
      ELSE IF (FN.EQ.2) THEN
         ITYPE=SYMBI
      ELSE
         ITYPE=0
      END IF
C     find position for component
      CALL INSP04(OPTION,QUIT)
C
      IF (OPTION.OR.QUIT) RETURN
C     create transform for instance
      CALL SETTFM(M)
C     read master data entry.
      CALL DER500(PMIP,OK)
C     create an instance.
C     set entity type to Instance type.
      IMBUFF(2)=ITYPE
C     ensure pointer to parent entry is zero.
      IMBUFF(8)=0
C     set work layer.
      IMBUFF(4)=CLAYER
C     copy window limits into instance
      DO 50 I=1,6
         RWORK(I,1)=RDBUFF(I)
         RWORK(I,2)=RDBUFF(I)
 50   CONTINUE
C     sort second diagonal data
      RWORK(2,2)=RWORK(5,1)
      RWORK(5,2)=RWORK(2,1)
C     transform enclosing box of component
      CALL MV0003(RWORK(1,1),M)
      CALL MV0003(RWORK(1,2),M)
C     write instance to database
      IF ( IMBUFF(10).LE.0) THEN
         CALL DEPRNT(418)
      END IF
CAPOLLO|SUN
      VPADD=.TRUE.
CAPOLLO|SUN
      CALL DEW566(M,TMIP,OK)
C     draw the component in place
      CALL ALLDRW(ITYPE,TMIP)
CAPOLLO|SUN
      VPADD=.FALSE.
CAPOLLO|SUN
C
C      CALL DRW066(M)
C     test for assigned properties,and update them
      CALL UPR066(TMIP,OK)
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSC10(CMPNAM,FN,PMIP,OK)
C     =======================================
C
C1    vartype           C*(*)   I4   I2  L
C1    iostatus            I     I    O   O
C
C2    Subroutine INSC10 searches within the current
C2    database for an entity of type SYMBM or COMPM
C2    based on the value of FN, and name CMPNAM
C2    returning the PMIP of the master if found
C2    OK indicates master found within existing file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*4 FN,NLEN1
      INTEGER*2 PMIP,P2,TENT
      LOGICAL OK
      CHARACTER*(*) CMPNAM
      EXTERNAL NLEN1
C
      IF (FN.EQ.1) THEN
C        COMPM required
         TENT=COMPM
      ELSE IF (FN.EQ.2) THEN
C        SYMBM required
         TENT=SYMBM
      ELSE IF (FN.EQ.3) THEN
C        COMPI required
         TENT=COMPI
      ELSE IF (FN.EQ.4) THEN
C        SYMBI required
         TENT=SYMBI
      ELSE
C        invalid function
         TENT=0
         RETURN
      END IF
C
      OK=.FALSE.
      P2=1
 
 10   CONTINUE
      IF (P2.GE.NTXPOS) RETURN
      CALL DTR500(P2,OK)
 
CAPOLLO
C     fold this name down if on apollo to compare
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(CBUFF)
CAPOLLO
 
c      write(10,*) '[insc10] cbuff= ',cbuff
c      write(10,*) '[insc10] cmpnam= ',cmpnam
c      write(10,*) '[insc10] tent= ',tent
      IF ( CBUFF(1:NLEN1(CBUFF)) .EQ.
     +    CMPNAM(1:NLEN1(CMPNAM))) THEN
C        found correct name,test it
         PMIP=ICBUFF(1)
         CALL DIR500(PMIP,OK)
c      write(10,*) '[insc10] imbuff(2)= ',imbuff(2)
         IF (IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.TENT) THEN
C           correct type,return this one
            OK=.TRUE.
            RETURN
         END IF
      END IF
C     increment to next
      P2=P2+1
      OK=.FALSE.
      GOTO 10
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSC90(INSNAM,INSTYP,INSMIP,IPRELP,INSFND)
C     =====================================================
C
C1    vartype           C*(*)    I2    I2      I2     L
C1    iostatus            I      I     I       O      O
C
C2    Subroutine INSC90 resolves the instance of name INSNAM
C2    by searching within the database,and externally for
C2    a master COMPONENT or SYMBOL.
C2    INSFND returned true if resolution achieved.
C
      include 'include/masti.inc'
      include 'include/filunit.inc'
      include 'include/nbuff.inc'
      include 'include/params.inc'
      include 'include/entity.inc'
      include 'include/props.inc'
      include 'include/lfu.inc'
      include 'include/ftypes.inc'
C
      INTEGER*2 INSTYP,INSMIP,IPRELP,T2MIP,T2PDP,T2TXP,T2RLP
      INTEGER*2 TPMIP,T2PRP,T2PCP,ID2
      INTEGER*4 INUNIT,ITMP,FN,TYPE
      LOGICAL INSFND,OK
      CHARACTER*(*) INSNAM,TFTYP*80,FILETP*80,FILNAM*80
C
      EXTERNAL INSC10,DIM502,OPENNM,INSP55,DIR500
      EXTERNAL DTR500,DTM500,INSP07,CLROPF,SAVMOV,RSTMOV
C
      TYPE=1
      ID2=0
      TFTYP=' '
      IF (     INSTYP.EQ.COMPM) THEN
         DAXTYP = 2
         FN=1
      ELSE IF (INSTYP.EQ.SYMBM) THEN
         DAXTYP = 3
         FN=2
      ELSE IF (INSTYP.EQ.COMPI) THEN
         DAXTYP = 2
         FN=1
      ELSE IF (INSTYP.EQ.SYMBI) THEN
         DAXTYP = 3
         FN=2
      END IF
C     test for existance within the database
      CALL INSC10(INSNAM,FN,TPMIP,INSFND)
      IF (INSFND) THEN
C        master does exist within database
C        resolve relation pointer for the instance
         CALL DIM502(TPMIP,INSMIP,IPRELP,OK)
      ELSE
C        external reference from here
C        test existance of external file
         ITMP=PARFUN
         TFTYP=FILTYP
         IF (INSTYP.EQ.COMPI) THEN
            FILETP='COMPM'
            FILTYP='COMPM'
         ELSE
            FILETP='SYMBM'
            FILTYP='SYMBM'
         END IF
         CALL OPENNM(INSNAM,PARFUN,.TRUE.,INSFND)
         INUNIT=PARFUN
         PARFUN=ITMP
         IF (INSFND) THEN
C            file found
             T2MIP=NMIPOS
             T2PDP=NPDPOS
             T2TXP=NTXPOS
             T2RLP=NRLPOS
             T2PRP=NPRPOS
             T2PCP=NPCPOS
CAPOLLO
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C             CALL UNFOLD(INSNAM)
CAPOLLO
             CALL INSP55(INSNAM,FILETP,INUNIT,T2MIP,
     +                           T2PDP,T2TXP,T2RLP,T2PRP,T2PCP)
             CLOSE(UNIT=INUNIT,STATUS='KEEP')
CIBM
C             LFU(INUNIT)=.FALSE.
CIBM
C            find component master pointer from
C            first entity of component
             CALL DIR500(T2MIP,OK)
C            save MI pointer to component master
             TPMIP=IMBUFF(8)
C            ensure master name is correct
C            (override internal name from master file)
             CALL DIR500(TPMIP,OK)
C            read the associated name
             CALL DTR500(IMBUFF(9),OK)
C            overwrite name with correct reference
             CBUFF=INSNAM
C            modify text record
             CALL DTM500(IMBUFF(9),ID2,OK)
C            now set instance relation to be same as master
             CALL DIM502(TPMIP,INSMIP,IPRELP,OK)
C            save movdat block
             CALL SAVMOV()
C            must correct master data for current database unit
             CALL CLROPF()
             CALL INSP07(FN,T2MIP,TYPE)
C            restore movdat block
             CALL RSTMOV()
         END IF
C        retrieve file status
         FILTYP=TFTYP
         PARFUN=ITMP
      END IF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT PART option
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 TMEN,TCELL,FN
      EXTERNAL GTMCLO,CLRPEW,MNLP01,INSC02
C
      TMEN=MEN
      TCELL=CELLN
C     clear the transform flags
      CALL CLROPF()
      FN=3
C     initialize INSERT PART option menu
      CALL MNLP01(FN)
C     go insert the part
      CALL INSC02(FN)
C     ensure PART option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP03(FILNM,OPTION,QUIT,OK)
C     =======================================
C
C1    vartype            C*(*)   L     L  L
C1    iostatus             I     O     O  O
C
C2    Subroutine INSP03 takes the data currently
C2    identified in the part file and inserts it
C2    in the database.Returns Ok if op complete
C2    QUIT indicates quit status,OPTION option hit
C
      include 'include/masti.inc'
      include 'include/props.inc'
      include 'include/filunit.inc'
      include 'include/lfu.inc'
      include 'include/movdat.inc'
      include 'include/ftypes.inc'
      include 'include/dhead.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,TTPRP,TTPCP
      INTEGER*4 FN,i,TYPE
      LOGICAL OK,OPTION,QUIT
      CHARACTER*80 FILNM
      EXTERNAL INSP04,OPENNM,INSP05,MOVMAS,INSP07
      REAL TSCALE,TSCALE3
C
C     set function number for PART
      TYPE=2
      FN=3
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
C     Try loading the part
C
C     find position for part
      CALL INSP04(OPTION,QUIT)
      IF (OPTION.OR.QUIT) RETURN
C
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
      IF ( .NOT. OK ) THEN
C        something wrong with file
         RETURN
      END IF
C
C     go store the data
      TSCALE = RDHEAD(1)
      TSCALE3 = RDHEAD(3)
      CALL SAVMOV()
      CALL INSP05(FILNM,'PART',TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
C     transform data including database units,and scales
      CALL RSTMOV()
      DAXTYP = DAXPRT
      CALL INSP07(FN,TTMIP,TYPE)
      RDHEAD(1) = TSCALE
      RDHEAD(3) = TSCALE3
      RETURN
C
 99   CONTINUE
      CALL RSTMOV()
      WRITE(UNIT=10,FMT=*)'[INSP03] Error in part file'
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP04(OPTION,QUIT)
C     ==============================
C
C2    Subroutine INSP04 is an auxiliary routine
C2    for the inserting of parts.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL WX1,WY1
      LOGICAL OPTION,QUIT
      EXTERNAL FINDP0,MNLPTS,MNUPTS
C
C***************************************************************
C                 PART ORIGIN  DEFINE                          *
C***************************************************************
 
C     load the point modes to menu
      CALL MNLPTS()
C     go get a point
      CALL FINDP0(244,WX1,WY1,OPTION,QUIT)
C     unload the point modes
      CALL MNUPTS()
      IF (QUIT.OR.OPTION) RETURN
C     must have the origin point ok
C     save the translation
      REFDAT(4,1)=WX1
      REFDAT(4,2)=WY1
      OPFLAG(4)=.TRUE.
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP05(FNAM,FTYP,TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
C     ================================================================
C
C1    vartype          C*(*) C*(*) I2    I2    I2     I2    I2    I2
C1    iostatus           I     I   I     I     I      I     I     I
C
C2    Subroutine INSP05  reads a part data file
C2    from the previously opened file
C2    connected to PARFUN,using the information contained
C2    in the header block buffer,and the movdat block.
C
      include 'include/masti.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/props.inc'
      include 'include/save.inc'
C
      REAL TDHEAD(20)
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,PMIP,TTPRP,TTPCP
      INTEGER*2 T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,T2PCP
      INTEGER*2 INSTYP,INSMIP,TPMIP,TPRELP,WUN
C
      INTEGER*4 I,TUNIT,NINSTS,INUNIT,ITMP,NLEN1
      LOGICAL OK
C
      CHARACTER*(*) FNAM,FTYP,CNAM*80
C
      EXTERNAL RSTFHD,RSTDHD,RSTNHD,RSTMI1,RSTPD1,RSTTX1,RSTRL1
      EXTERNAL RSTPR1,RSTPR2,REGC04,NLEN1
C
C     ensure reading starts at beginning of file
      REWIND(UNIT=PARFUN,ERR=99)
C
C     set the loading pointers to correct values
      WUN=1
      T2MIP=TTMIP-WUN
      T2PDP=TTPDP-WUN
      T2TXP=TTTXP-WUN
      T2RLP=TTRLP-WUN
      T2PRP=TTRLP-WUN
      T2PRP=TTPRP-WUN
      T2PCP=TTPCP-WUN
C     read the file header
      CALL RSTFHD(OK)
      IF (.NOT.OK) GOTO 99
C     check file type
      IF (FTYP.NE.CFHEAD(1)(1:NLEN1(CFHEAD(1)))) GOTO 98
C
      OROTRV=RFHEAD(1)
C
C     read the drawing header
      CALL RSTDHD(OK)
      IF (.NOT.OK) GOTO 99
C      fix part limits
C      RDHEAD(6)=REFDAT(9,1)
C      RDHEAD(7)=REFDAT(9,2)
C      RDHEAD(8)=REFDAT(10,1)
C      RDHEAD(9)=REFDAT(10,2)
C
C     section files contain additional records
C     save the file header in case of difficulty with instances
      DO 100 I=1,20
          TDHEAD(I) = RDHEAD(I)
100   CONTINUE
      IF (FTYP.EQ.'SECT') THEN
C       recover SECTION records
        CALL RSTSSC(OK)
        IF (.NOT.OK) GOTO 99
      END IF
C
C     read the database header
      CALL RSTNHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     now get down to reading the part data it's self
C     start with the MI data
      DO 30 I=1,NHEADI(1)
C        read the MI record
         CALL RSTMI1(T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,OK)
         IF (.NOT.OK) GOTO 99
 30   CONTINUE
C
C     now save all the coordinate data
      DO 31 I=1,NHEADI(2)
C        read the PD record
         CALL RSTPD1(T2MIP,T2PDP,OK)
         IF (.NOT.OK) GOTO 99
 31   CONTINUE
C
C     now save all the text data
      DO 32 I=1,NHEADI(3)
C        read the text record
         CALL RSTTX1(T2MIP,T2TXP,OK)
         IF (.NOT.OK) GOTO 99
 32   CONTINUE
C
C     now the RL data
      DO 33 I=1,NHEADI(4)
C        read the RL record
         CALL RSTRL1(T2MIP,T2PDP,T2TXP,T2RLP,OK)
         IF (.NOT.OK) GOTO 99
 33   CONTINUE
C
C     now read the property index data
      DO 34 I=1,NHEADI(5)
C        read the PR record
         CALL RSTPR1(T2MIP,T2PRP,T2PCP,OK)
         IF ( .NOT. OK ) GOTO 99
 34   CONTINUE
C
C     now read the property character data
      DO 35 I=1,NHEADI(6)
C        read the PC record
         CALL RSTPR2(T2PRP,T2PCP,OK)
         IF ( .NOT. OK ) GOTO 99
 35   CONTINUE
C
      IF (.NOT.OK) GOTO 99
C
C     test for included component instances in the part
C     initialize count of resolved instances
      NINSTS=0
C     save the end of the part data position
      CMIPOS=NMIPOS
C     resolve form data entry point forward
      CALL REGC04(TTMIP,NINSTS,OK)
C
C     recover thethe file header
      DO 110 I=1,20
          RDHEAD(I) = TDHEAD(I)
110   CONTINUE
      RETURN
C
 98   CONTINUE
      WRITE(UNIT=10,FMT=*)'[INSP05] File is not correct type'
      RETURN
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[INSP05] Error reading part data'
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP06()
C     ===================
C
C2    Subroutine handles options on the
C2    INSERT PART function
C2    for scale,scale text,rotate
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 NLEN,TCELL
C
      LOGICAL DSAME
      REAL RAD,REAL,DEG
      DOUBLE PRECISION DN
C
      CHARACTER*80 CCBUFF,TOKEN*1
C     INTRINSIC REAL
      EXTERNAL NLEN,AEXPRN,RAD,GTMCLO,DEG,DSAME
C
      IF (CCMD.EQ.'S') THEN
C***************************************************************
C                     SCALE OPTION                             *
C***************************************************************
         IF (OPFLAG(1)) THEN
C           scale already set,cancel it
            CALL GTMCLO(MEN,CELLN)
            OPFLAG(1)=.FALSE.
C           reset text scale to 1
            REFDAT(7,3)=1.0
            REFDAT(1,3)=1.0
            WRITE(UNIT=CCBUFF,FMT='(F12.2)') REFDAT(1,3)
            CALL CRUNCH(CCBUFF)
            CALL GTMCWT(3,CCMD,CCBUFF)
         ELSE
C           enable scaling
            OPFLAG(1)=.TRUE.
C           save the caller menu cell
            MNCELL(1,1)=MEN
            MNCELL(1,2)=CELLN
C           set the scaling origin
            REFDAT(1,1)=0
            REFDAT(1,2)=0
C           get the scale factor
 211        CONTINUE
            CALL DPRMXP(245,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and
C              return for input.
               OPFLAG(1)=.FALSE.
               CALL GTMCLO(MNCELL(1,1),MNCELL(1,2))
            ELSE
C              evaluate an arithmetic expression
C              from the keyboard
               CALL AEXPRN(CCBUFF,DN,*211)
               IF ( DSAME(DN,0.0) ) GOTO 211
               REFDAT(1,3)=REAL(DN)
C              set text scale factor
               REFDAT(7,3)=REFDAT(1,3)
               CCBUFF=' '
               WRITE(UNIT=CCBUFF,FMT='(F12.2)') REFDAT(7,3)
               CALL CRUNCH(CCBUFF)
               CALL GTMCWT(3,CCMD,CCBUFF)
               CALL GTMCHI(MEN,CELLN)
            END IF
C            CALL GTCLRC(MNCELL(1,1),MNCELL(1,2)+1)
C            WRITE(UNIT=CCBUFF,FMT='(F12.2)') REFDAT(7,3)
C            CALL CRUNCH(CCBUFF)
C            CALL GTPMEN(CCBUFF,' ',MNCELL(1,1),MNCELL(1,2)+1)
         END IF
      ELSE IF (CCMD.EQ.'s') THEN
C***************************************************************
C                  SCALE TEXT OPTION                           *
C***************************************************************
        IF (OPFLAG(7)) THEN
C           set scale text to no
            OPFLAG(7)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE IF (OPFLAG(6) ) THEN
            CALL GTHFMC(MEN,'p',TCELL)
            CALL GTMCLO(MEN,TCELL)
            OPFLAG(6)=.FALSE.
            OPFLAG(7)=.TRUE.
         ELSE
C           set scale text to yes
            OPFLAG(7)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'p') THEN
C***************************************************************
C              PROPORTIONAL SCALE OPTION                       *
C***************************************************************
         IF (OPFLAG(6))  CALL GTMCLO(MEN,CELLN)
         OPFLAG(6)=.NOT.OPFLAG(6)
         IF (OPFLAG(7) ) THEN
            CALL GTHFMC(MEN,'s',TCELL)
            CALL GTMCLO(MEN,TCELL)
            OPFLAG(7)=.FALSE.
         ENDIF
      ELSE IF (CCMD.EQ.'r') THEN
C***************************************************************
C                     ROTATE OPTION                            *
C***************************************************************
         IF (OPFLAG(2)) THEN
C           cancel the previous rotation
            OPFLAG(2)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
            REFDAT(2,3)=0.0
            WRITE(UNIT=CCBUFF,FMT='(F12.2)') REFDAT(2,3)
            CALL CRUNCH(CCBUFF)
            CALL GTMCWT(3,CCMD,CCBUFF)
         ELSE
C           set the flag to true
            OPFLAG(2)=.TRUE.
C           save the caller menu cell
            MNCELL(2,1)=MEN
            MNCELL(2,2)=CELLN
C           save the rotation origin
            REFDAT(2,1)=0
            REFDAT(2,2)=0
C           get the rotation angle
 311        CONTINUE
            CALL DPRMXP(246,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and
C              return for input
               OPFLAG(2)=.FALSE.
               CALL GTMCLO(MNCELL(2,1),MNCELL(2,2))
            ELSE
C              evaluate an arithmetic expression
C              from the keyboard
               CALL AEXPRN(CCBUFF,DN,*311)
C              save the rotation angle
               REFDAT(2,3)=RAD(REAL(DN))
               CCBUFF=' '
               WRITE(UNIT=CCBUFF,FMT='(F12.2)') DEG(REFDAT(2,3))
               CALL CRUNCH(CCBUFF)
               CALL GTMCWT(3,CCMD,CCBUFF)
               CALL GTMCHI(MEN,CELLN)
            END IF
         END IF
C         CALL GTCLRC(MNCELL(2,1),MNCELL(2,2)+1)
C         WRITE(UNIT=CCBUFF,FMT='(F12.2)') DEG(REFDAT(2,3))
C         CALL CRUNCH(CCBUFF)
C         CALL GTPMEN(CCBUFF,' ',MNCELL(2,1),MNCELL(2,2)+1)
      END IF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP07(FN,TTMIP,TYPE)
C     ================================
C
C1    vartype           I4   I2   I4
C1    iostatus          I    I    I
C
C2    Subroutine INSP07 takes care of transformation
C2    of data inserted from PART files,or COMPONENT/SYMBOL
C2    masters,by taking account of database unit conversions
C2    and drawing scale changes.TTMIP passes the MI record
C2    of the first entity in the datafile entry to the database.
C2         FN=1   COMPONENT
C2         FN=2   SYMBOL
C2         FN=3   PART
C2         FN=4   SECTION
C          TYPE=1 MASTER DATA
C          TYPE=2 OTHER DATA
C
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/params.inc'
      include 'include/swind.inc'
C
      REAL TMPSCL
      INTEGER*2 WUN,TTMIP,II
      INTEGER*4 FN,TYPE,J,I
      LOGICAL SCLSET,CSRSET,OK
      EXTERNAL MOVMAS
C
 
      WUN=1
 
C     set comp/symb reset flag.
      CSRSET=(FN.LE.2)
C     place Mi pointers in SWIND for transformation.
      OPFLAG(12) = .TRUE.
      NDATA=0
      IF(TYPE.EQ.1) THEN
C     master data being transformed to databae units
      DO 41 II=TTMIP,NMIPOS-WUN
C        save the MI pointer in work file.
         CALL DIR500(II,OK)
         IF((IMBUFF(1).EQ.SYMBM.OR.IMBUFF(1).EQ.COMPM)
     +   .OR.(IMBUFF(1).EQ.10.AND.(IMBUFF(2).EQ.COMPM.OR
     +   .IMBUFF(2).EQ.SYMBM)) ) THEN
C            Condition chosen if data is entity informnation
C            which is master but not instance
 
             NDATA=NDATA+1
             WRITE(UNIT=SWINDU,REC=NDATA) II,0.0,0.0,0.0
C            master marked for move cannot do again
 
         ENDIF
 41   CONTINUE
      ELSE
 
 
      DO 40 II=TTMIP,NMIPOS-WUN
C        save the MI pointer in work file.
         CALL DIR500(II,OK)
         IF(IMBUFF(1).EQ.10.AND..NOT.(IMBUFF(2)
     +    .EQ.COMPM.OR.IMBUFF(2).EQ.SYMBM).OR.IMBUFF(1).EQ.GROUP) THEN
C            Condition chosen if data is entity informnation
C            which is anything but master also instance
             NDATA=NDATA+1
             WRITE(UNIT=SWINDU,REC=NDATA) II,0.0,0.0,0.0
         ENDIF
 40   CONTINUE
      ENDIF
C     ensure flags are not drawn.
      VNDATA=NDATA+1
C     go transform to correct position.
C     set copy flag
      COPYIT=.FALSE.
C     set number of copies.
      NNCOPY=0
C     change to current DB units.
 
      REFDAT(1,1)=0
      REFDAT(1,2)=0
 
      IF (OPFLAG(1)) THEN
C        scale of part is called for.
C        combine with database units conversion.
C        flag geometric scale required.
         SCLSET=.TRUE.
         TMPSCL=REFDAT(1,3)
         IF ( FN.EQ.2 ) THEN
            REFDAT(1,3)=REFDAT(1,3)/DRWSCL
         END IF
C        save scale factor for local text ops.
         REFDAT(7,3)=REFDAT(1,3)
         REFDAT(1,3)=REFDAT(1,3)*RDHEAD(1)/DBUFAC
 
      ELSE
         IF (FN.EQ.1) THEN
C           COMPONENT
C           set scale factor to correct database units.
            REFDAT(1,3)=RDHEAD(1)/DBUFAC
         ELSE IF (FN.EQ.2) THEN
C           SYMBOL
C           set scale factor to accomodate constant.
C           paper size of symbol
C            REFDAT(1,3)=1.0
C            REFDAT(1,3)=RDHEAD(1)/DBUFAC
            REFDAT(1,3)=1/DRWSCL*RDHEAD(1)/DBUFAC
         ELSE IF (FN.EQ.4) THEN
C           SECTION file
            REFDAT(1,3)=1.0
            OPFLAG(1)=.FALSE.
         ELSE
C           PART
C           set scale factor to correct database units.
            REFDAT(1,3)=RDHEAD(1)/DBUFAC
         END IF
C
         IF (FN.NE.4) THEN
C           set scaling flag
            OPFLAG(1)=.TRUE.
            SCLSET=.FALSE.
C           ensure text is scaled by unit only
            REFDAT(7,3)=1.0
C           ensure TMPSCL contains a valid value
            TMPSCL=1.0
         END IF
      END IF
C     set proportional scale if required
      IF ( CSRSET ) THEN
         OPFLAG(6)=.TRUE.
      END IF
      IF (OPFLAG(6)) THEN
C        ensure text scale is in correct proportion
C         WRITE(10,*) 'RDHEAD(3)*RDHEAD(1)/DBUFAC/PAPTOW*TMPSCL',
C     +                RDHEAD(3),RDHEAD(1),DBUFAC,PAPTOW,TMPSCL
         IF ( FN.EQ.2 ) THEN
            REFDAT(7,3)=(RDHEAD(1)/DBUFAC)*(RDHEAD(3)/PAPTOW)/DRWSCL
         ELSE
            REFDAT(7,3)=RDHEAD(3)*RDHEAD(1)/DBUFAC/PAPTOW*TMPSCL
         END IF
      END IF
C     disable erase of entities during transformation
      OPFLAG(9)=.FALSE.
      IF ( CSRSET ) THEN
C        disable add to display file.
 
         OPFLAG(8)=.FALSE.
C        disable drawing of entities.
         OPFLAG(10)=.FALSE.
      ELSE
C        enable add to display file.
         OPFLAG(8)=.TRUE.
C        enable drawing of entities.
         OPFLAG(10)=.TRUE.
      END IF
C
C     go do the move.
c      OPFLAG(4)=.FALSE.
      IF(NDATA.GT.0)  CALL MOVMAS(CSRSET)
C     enable erase and draw of entities during transformation.
      OPFLAG(9)=.TRUE.
      OPFLAG(10)=.TRUE.
C     disable add to display file.
      OPFLAG(8)=.FALSE.
      IF (SCLSET) THEN
C        retrieve geometric scale.
         REFDAT(1,3)=TMPSCL
      ELSE
C        ensure no geometric scale used next time.
         OPFLAG(1)=.FALSE.
      END IF
C
C     clear translate flag.
      OPFLAG(4)=.FALSE.
      OPFLAG(12) = .FALSE.
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSP55(FNAM,FTYP,INUNIT,TTMIP,TTPDP,TTTXP,TTRLP,
     +                                   TTPRP,TTPCP)
C     ===========================================================
C
C1    vartype          C*(*) C*(*)   I4   I2    I2    I2     I2
C1    iostatus           I     I     I    I     I     I      I
C
C2    Subroutine INSP55  reads a component master file
C2    from the previously opened file
C2    connected to PARFUN,using the information contained
C2    in the header block buffer,and the movdat block.
C
      include 'include/masti.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/props.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,II,PMIP
      INTEGER*2 T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,T2PCP
      INTEGER*2 INSTYP,INSMIP,TPMIP,TPRELP,TTPRP,TTPCP,WUN
      INTEGER*4 I,TUNIT,INUNIT
      LOGICAL OK
      CHARACTER  FTYP*(*),FNAM*(*)
      EXTERNAL RSTFHD,RSTDHD,RSTNHD,RSTMI1,RSTPD1,RSTTX1
      EXTERNAL RSTRL1,RSTPR1,RSTPR2
C
C     WRITE(UNIT=10,FMT=*)'[INSP55] *** entered ***'
C     write(unit=10,fmt=*)'[insP55 L,fnam=',LEN(FNAM),FNAM
C     write(unit=10,fmt=*)'[insP55] L,ftyp=',LEN(FTYP),FTYP
C     WRITE(UNIT=10,FMT=*)'[INSP55]',ttmip,ttpdp,tttxp,ttrlp,ttprp,
C    +                               ttpcp
C     set the loading pointers to correct values
      WUN=1
      T2MIP=TTMIP-WUN
      T2PDP=TTPDP-WUN
      T2TXP=TTTXP-WUN
      T2RLP=TTRLP-WUN
      T2PRP=TTRLP-WUN
      T2PRP=TTPRP-WUN
      T2PCP=TTPCP-WUN
C     swap unit numbers
      TUNIT=PARFUN
      PARFUN=INUNIT
C     ensure reading starts at beginning of file
      REWIND(UNIT=PARFUN,ERR=99)
C
C     read the file header
      CALL RSTFHD(OK)
      IF (.NOT.OK) GOTO 99
C     check file type
      IF (CFHEAD(1).NE.FTYP) GOTO 98
C
C     read the drawing header
      CALL RSTDHD(OK)
      IF (.NOT.OK) GOTO 99
C      fix part limits
C      RDHEAD(6)=REFDAT(9,1)
C      RDHEAD(7)=REFDAT(9,2)
C      RDHEAD(8)=REFDAT(10,1)
C      RDHEAD(9)=REFDAT(10,2)
C     read the database header
      CALL RSTNHD(OK)
      IF (.NOT.OK) GOTO 99
C
C
C     now get down to reading the part data it's self
C     start with the MI data
      DO 30 I=1,NHEADI(1)
C        read the MI record
C     write(unit=10,fmt=*)'[insp55] calling rstmi1'
         CALL RSTMI1(T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,OK)
         IF (.NOT.OK) GOTO 99
 30   CONTINUE
C
C        now save all the coordinate data
         DO 31 I=1,NHEADI(2)
C           read the PD record
            CALL RSTPD1(T2MIP,T2PDP,OK)
            IF (.NOT.OK) GOTO 99
 31      CONTINUE
C
C        now save all the text data
         DO 32 I=1,NHEADI(3)
C           read the text record
            CALL RSTTX1(T2MIP,T2TXP,OK)
            IF (.NOT.OK) GOTO 99
 32      CONTINUE
C
C     now the RL data
      DO 33 I=1,NHEADI(4)
C        read the RL record
         CALL RSTRL1(T2MIP,T2PDP,T2TXP,T2RLP,OK)
         IF (.NOT.OK) GOTO 99
 33   CONTINUE
C
C     now read the property index data
      DO 34 I=1,NHEADI(5)
C        read the PR record
         CALL RSTPR1(T2MIP,T2PRP,T2PCP,OK)
         IF ( .NOT. OK ) GOTO 99
 34   CONTINUE
C
C     now read the property character data
      DO 35 I=1,NHEADI(6)
C        read the PC record
         CALL RSTPR2(T2PRP,T2PCP,OK)
         IF ( .NOT. OK ) GOTO 99
 35   CONTINUE
C
      IF (.NOT.OK) GOTO 99
C
      PARFUN=TUNIT
C
C     WRITE(UNIT=10,FMT=*)'[INSP55] *** exitted ***'
      RETURN
C
 98   CONTINUE
C     WRITE(UNIT=10,FMT=*)'[INSP55] File is not correct type'
C     WRITE(UNIT=10,FMT=*)'[INSP55] FTYP,CFHEAD ',FTYP,CFHEAD(1)
      PARFUN=TUNIT
      RETURN
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[INSP55] Error reading part data'
      PARFUN=TUNIT
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSS00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT COMPONENT option
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL INSS01,GTMCLO,CLRPEW,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C     clear the transform flags
      CALL CLROPF()
C     call mediator routine
      CALL INSS01()
C     ensure PART option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE INSS01()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT COMPONENT option
C
      INTEGER FN
      EXTERNAL INSC02,MNLP01
C
C     set function number for SYMB
      FN=2
C     initialize INSERT SYMBOL option menu
      CALL MNLP01(FN)
C     go insert the symbol
      CALL INSC02(FN)
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE MNLP01(FN)
C     =====================
C
C1    vartype           I4
C1    iostatus          I
C2
C2    loads menu system of the INSERT PART
C2    or COMPONENT option depending on the
C2    value of FN at entry.
C
      include 'include/movdat.inc'
 
      CHARACTER*1 TOKEN,NUM*20
      INTEGER*4 FN,CELL
      REAL DEG
      EXTERNAL GTDMEN,DEG
C
      IF (FN.EQ.1.OR.FN.EQ.2 ) THEN
C        COMPONENT being used
C        SYMBOL being used
C        load the new name option
         CALL GTDMEN(150,3)
      ELSE IF (FN.EQ.3) THEN
C        PART being used
C        load the new name option
         CALL GTDMEN(140,3)
C        load scale text option
         CALL GTDMEN(141,3)
C        load the proportion option
         CALL GTDMEN(142,3)
      END IF
C
C     load the rotate option
      CALL GTDMEN(143,3)
C     load the scale option
      CALL GTDMEN(144,3)
C     load the repeat option
      CALL GTDMEN(145,3)
C     load the cancel option
      CALL GTDMEN(146,3)
C     get the token for rotate
      CALL FNDTOK(143,TOKEN)
C     get the current rotate factor
      WRITE(UNIT=NUM,FMT='(F12.2)') DEG(REFDAT(2,3))
      CALL CRUNCH(NUM)
C     load the current cell with the current rotation angle
      CALL GTMCWT(3,TOKEN,NUM)
C     if its still there hihlightit
      IF(OPFLAG(2)) THEN
          CALL GTHFMC(3,TOKEN,CELL)
          CALL GTMCHI(3,CELL)
      ENDIF
      CALL FNDTOK(144,TOKEN)
      WRITE(UNIT=NUM,FMT='(F12.2)') REFDAT(1,3)
      CALL CRUNCH(NUM)
      CALL GTMCWT(3,TOKEN,NUM)
      IF(OPFLAG(1)) THEN
          CALL GTHFMC(3,TOKEN,CELL)
          CALL GTMCHI(3,CELL)
      ENDIF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE RSTMOV()
C     ===================
C
C
C     restores state of move
C
      include 'include/movdat.inc'
      include 'include/save.inc'
C
      INTEGER*4 I,J
      LOGICAL OK
      EXTERNAL OURSCR
C
CIBM|PC386
C      CALL FINDU1(SUNIT,OK)
C      OPEN (UNIT=SUNIT,FILE=SAVNAM,FORM='UNFORMATTED',
C     +    ACCESS='DIRECT',RECL=60,ERR=17)
CC     scratch file open
CPC386
C     LFU(SUNIT)=.TRUE.
CIBM
C
C      WRITE(10,*) '[RSTMOV] DATA RECOVERED'
      READ(UNIT=SUNIT,REC=1) RECC
      RECC=RECC-1
      IF(RECC.EQ.1) THEN
CIBM|PC386
C          CLOSE(UNIT=SUNIT)
CIBM|PC386
           GOTO 18
      ENDIF
      READ(UNIT=SUNIT,REC=RECC+1)(OPFLAG(J),J=1,12),CURSCL
C      WRITE(UNIT=10,FMT=*)(OPFLAG(J),J=1,12),CURSCL
      RECC=RECC-1
      DO 20 I=2,1, -1
         READ(UNIT=SUNIT,REC=RECC+1)(MNCELL(J,I),J=1,10)
C         WRITE(UNIT=10,FMT=*)(MNCELL(J,I),J=1,10)
         RECC=RECC-1
 20   CONTINUE
      DO 10 I=3,1, -1
         READ(UNIT=SUNIT,REC=RECC+1)(REFDAT(J,I),J=1,10)
C         WRITE(UNIT=10,FMT=*)(REFDAT(J,I),J=1,10)
         RECC=RECC-1
 10   CONTINUE
      RECC=RECC+1
      WRITE(UNIT=SUNIT,REC=1) RECC
CIBM
C      LFU(SUNIT)=.FALSE.
CPC386
C      CLOSE(UNIT=SUNIT,STATUS='KEEP')
CIBM|PC386
 
      RETURN
17    CALL DEPRNT(287)
18    CONTINUE
C
      END
C
C---------------------------------------------------------------------
C
C---------------------------------------------------------------------
C
      SUBROUTINE SAVMOV()
C     ===================
C
C
C2    Subroutine SAVMOV saves the content of the
C2    MOVDAT common block,in a scratch file
C2    opened by this routine on SUNIT
C
      include 'include/movdat.inc'
      include 'include/save.inc'
C
      INTEGER*4 I,J,TUNIT
      LOGICAL OK
      EXTERNAL OURSCR
CIBM|PC386
C      CALL FINDU1(SUNIT,OK)
C      OPEN (UNIT=SUNIT,FILE=SAVNAM,FORM='UNFORMATTED',
C     +    ACCESS='DIRECT',RECL=60,ERR=17)
CPC386
C     LFU(SUNIT)=.TRUE.
CIBM
C     scratch file open
C
C
C      WRITE(10,*) '[SAVMOV] DATA SAVED'
      READ(UNIT=SUNIT,REC=1) RECC
      DO 10 I=1,3
         WRITE(UNIT=SUNIT,REC=RECC+1)(REFDAT(J,I),J=1,10)
C         WRITE(UNIT=10,FMT=*)(REFDAT(J,I),J=1,10)
         RECC=RECC+1
 10   CONTINUE
      DO 20 I=1,2
         WRITE(UNIT=SUNIT,REC=RECC+1)(MNCELL(J,I),J=1,10)
C         WRITE(UNIT=10,FMT=*)(MNCELL(J,I),J=1,10)
 
         RECC=RECC+1
 20   CONTINUE
      WRITE(UNIT=SUNIT,REC=RECC+1)(OPFLAG(J),J=1,12),CURSCL
C      WRITE(UNIT=10,FMT=*)(OPFLAG(J),J=1,12),CURSCL
 
      RECC=RECC+1
      WRITE(UNIT=SUNIT,REC=1) RECC
CIBM|PC386
C      CLOSE(UNIT=SUNIT,STATUS='KEEP')
CPC386
C      LFU(SUNIT)=.FALSE.
CIBM
      RETURN
17    CALL DEPRNT(287)
 
C
      END
C
C---------------------------------------------------------------------
      SUBROUTINE SETTFM(M)
C     ====================
C
C1    vartype         R(3,3)
C1    iostatus          O
C
C2    Subroutine SETTFM sets and returns a 3x3
C2    matrix defining a geometric transformation
C2    on the basis of data in the MOVDAT work variables.
C
      include 'include/movdat.inc'
C
      REAL SM(3,3),RM(3,3),TM(3,3),M(3,3),M2(3,3),RDUM
      EXTERNAL I3M,SCAP2D,ROTP2D,TRAN2D,MULT3M
C
C     initialize the matrices
      CALL I3M(M)
      CALL I3M(SM)
      CALL I3M(RM)
      CALL I3M(TM)
C
C     scale if required
      IF (OPFLAG(1)) THEN
         RDUM=REFDAT(1,3)
         CALL SCAP2D(REFDAT(1,1),REFDAT(1,2),
     +               RDUM,REFDAT(1,3),SM)
      END IF
C     rotate if required
      IF (OPFLAG(2)) THEN
         CALL ROTP2D(REFDAT(2,1),REFDAT(2,2),REFDAT(2,3),RM)
      END IF
C     translate if required
      IF (OPFLAG(4)) THEN
         CALL TRAN2D(REFDAT(4,1),REFDAT(4,2),TM)
      END IF
C     concatenate the transforms
      CALL MULT3M(SM,RM,M2)
      CALL MULT3M(M2,TM,M)
C     M now contains the working transform
C
      END
C
C---------------------------------------------------------------------
C
