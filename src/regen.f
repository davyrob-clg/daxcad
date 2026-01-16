C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 regen.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GETCSM(TMIP,OK)
C     SUBROUTINE MAJREG()
C     SUBROUTINE MNIREG
C     SUBROUTINE MNLRG1
C     SUBROUTINE MNLRG2
C     SUBROUTINE REGA00()
C     SUBROUTINE REGA01()
C     SUBROUTINE REGC00()
C     SUBROUTINE REGC02(FN)
C     SUBROUTINE REGC04(TTMIP,NINSTS,OK)
C     SUBROUTINE REGC66(ENTNAM,TENT,ERASE,SCUNIT,NINSTS)
C     SUBROUTINE REGC67(ENTNAM,TENT,FN,PMIP,NINSTS,OK)
C     SUBROUTINE REGC68(ENTNAM,TENT,FN,TMIP,OK)
C     SUBROUTINE REGS00()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE GETCSM(TMIP,OK)
C     ===========================
C
C1    vartype            I2   L
C1    iostatus           I    O
C
C2    Subroutine GETCSM finds all elements of the
C2    COMPONENT or SYMBOL MASTER at MI position TMIP
C2    and returns them in the scratch file attached to SWINDU.
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/swind.inc'
C
      INTEGER*4 I,J,NTEMP
      INTEGER*2 TMIP,TTMIP,RELP,NRRECS,NXTRLR,NENTS,D1,D2
      REAL SX,SY
      LOGICAL OK
C
      EXTERNAL STSENT,DRR950,DIR500
C
CIBM|PC386
C      NTEMP=NDATA
CIBM|PC386
      SX=0.0
      SY=0.0
      D1=0
      D2=0
C
C     read the MI data first
      CALL DIR500(TMIP,OK)
      IF (.NOT.OK) RETURN
C     find relation header
      RELP=IMBUFF(10)
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
      OK=(RLBUFF(1).EQ.SYMBM .OR. RLBUFF(1).EQ.COMPM)
      IF (.NOT.OK) THEN
         RETURN
      END IF
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
      NENTS=RLBUFF(5)
      DO 15 J=1,NRRECS
         NXTRLR=RLBUFF(2)
C        read the list of entities
         CALL DRR950(NXTRLR,OK)
         DO 10 I=4,10
C           save MI of each entity in SWIND
            TTMIP=RLBUFF(I)
            IF (TTMIP.GT.0) CALL SSFLAG(TTMIP,SX,SY,D1,D2,.FALSE.)
C           re-read relation record,may have been changed
C           by comp or symbol
            CALL DRR950(NXTRLR,OK)
 10      CONTINUE
 15   CONTINUE
C
C     load master header to scratch file
C      CALL SSFLAG(TMIP,SX,SY,D1,D2,.FALSE.)
C
      END
C
C
      SUBROUTINE MAJREG()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the REGEN mode
C2    of operation is selected from the master menu.
C2    controls operation of the REGEN function
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL DCPRNT,DEPRNT,GTHFMC,GTMCHI,CLRPEW,GTMCLO
      EXTERNAL UNFLAG,GTCLRM
      EXTERNAL REGA00,REGC00
C
C     Now activate the REGEN major option menu
      CALL MNIREG
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     regen. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     prompt for option select
C     "Select an option from the NOUN menu"
      CALL DCPRNT(388)
C     Making Component the default
C      CCMD='C'
C      MEN=2
C     Find the menu and hightlite it
C      CALL GTHFMC(MEN,CCMD,TCELL)
C      CALL GTMCHI(MEN,TCELL)
C      CELLN=TCELL
C      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select REGEN mode
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
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'D') THEN
C           REGEN DRAWING option
            CALL REGA00()
         ELSE IF (CCMD.EQ.'C') THEN
C           REGEN COMPONENT option
            CALL REGC00()
         ELSE IF (CCMD.EQ.'S') THEN
C           REGEN SYMBOL option
            CALL REGS00()
         ELSE
C           unrecognized REGEN option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the error and prompt windows
         CALL CLRPEW()
         IF (CCMD.EQ.'q') GOTO 99
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         IF (MEN.EQ.3) CALL GTMCLO(MEN,CELLN)
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
C     clear the minor option menu
      CALL GTCLRM(3)
C
      RETURN
      END
C
C
      SUBROUTINE MNIREG
C     =================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the REGEN major options.
C2
C
      EXTERNAL GTPMEN,GTMCHI,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the REGEN major options.
C     Hilite the option header
C     "REGENERATE"
      CALL GTDMHD(15,2)
C
C     Load the entity descriptors.
C2    'D' is the token for DRAWING
      CALL GTDMEN(450,2)
C2     C   is the token for COMP
      CALL GTDMEN(451,2)
C2     S   is the token for SYMBOL
      CALL GTDMEN(452,2)
C
      END
C
C
      SUBROUTINE MNLRG1
C     =================
C
C2    Subroutine MNLRG1 loads the menu
C2    options for REGEN COMP function.
C
      EXTERNAL GTDMEN
C
C     load NAME option
      CALL GTDMEN(150,3)
C     load ACCEPT option
      CALL GTDMEN(13,3)
      END
C
C
      SUBROUTINE MNLRG2
C     =================
C
C2    Subroutine MNLRG2 loads the menu
C2    options for REGEN DRAWING function.
C
      EXTERNAL GTDMEN
C
C     load ACCEPT option
      CALL GTDMEN(13,3)
      END
C
C
C
      SUBROUTINE REGA00()
C     ===================
C
C1    no arguments required
C2
C2    controls operation of the REGEN DRAWING function
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      EXTERNAL REGA01
C
C     clear the minor option menu
      CALL GTCLRM(3)
C     load option menu
      CALL MNLRG2()
C     enter the REGEN DRAWING routine
      CALL REGA01()
C
      END
C
C
      SUBROUTINE REGA01()
C     =====================
C
C2    Subroutine REGA01 is the main working routine
C2    for the REGEN DRAWING function.
C
      include 'include/menun.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/masti.inc'
      include 'include/ftypes.inc'
C
      INTEGER*4 C,TMEN,TCELL
      REAL X,Y
      LOGICAL OK
      CHARACTER*80 FILNM
      EXTERNAL GTMCHI,SAVALL,CMPACT,GTMCLO,DEPRNT
C
 10   CONTINUE
C
C     prompt for command
C     "Select an option from the MODIFIER menu"
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
C
      IF (MEN.EQ.3) THEN
         TMEN=MEN
         TCELL=CELLN
         CALL GTMCHI(MEN,CELLN)
         IF (CCMD.EQ.CHAR(13)) THEN
C           *********************************
C           *        ACCEPT OPTION          *
C           *********************************
C           ensure safe workfile created first
            CALL STORE2(.TRUE.)
C           "Starting regen ... do not disturb"
            CALL DCPRNT(370)
C           now attempt compact operation
C           kill existing file first
C            CALL UKILLF(DRGNAM,OK)
C           force a database pack,and a regenerate
 
            CALL SAVALL(.FALSE.,.TRUE.,.TRUE.,OK)
            IF (OK) THEN
C              ensure safe workfile is updated
               CALL STORE2(.TRUE.)
C              reset deleted entity counter
               DELENT=0
C              " ... Regen complete"
               CALL DCPRNT(371)
            ELSE
C              recover content of temporary file
C              "Error during regen ..."
               CALL DEPRNT(372)
C               " ... Recovering previous data"
               CALL DEPRNT(373)
               CALL LOAD2(.TRUE.)
            END IF
C           reconstruct current view
            CALL REGEND()
C           *********************************
C           *         END OF OPTIONS        *
C           *********************************
         END IF
C
 99      CONTINUE
         CALL GTMCLO(TMEN,TCELL)
         GOTO 10
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') THEN
         RETURN
      ELSE
C        invalid cursor hit
         CALL DEPRNT(117)
         GOTO 10
      END IF
C     function complete
      END
C
C
      SUBROUTINE REGC00()
C     ===================
C
C1    no arguments required
C2
C2    controls operation of the REGEN COMPONENT function
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      INTEGER FN
      EXTERNAL MNLRG1,REGC02
C
C     set function number to indicate
C     component regeneration.
      FN=1
C     load menu options for REGEN COMP
      CALL MNLRG1
C     enter the REGEN COMP OR SYMB routine
      CALL REGC02(FN)
C
      END
C
C
      SUBROUTINE REGC02(FN)
C     =====================
C
C2    Subroutine REGC02 is the main working routine
C2    for the REGEN COMPONENT function.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/params.inc'
      include 'include/filunit.inc'
      include 'include/props.inc'
      include 'include/entity.inc'
      include 'include/lfu.inc'
      include 'include/ftypes.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,II,D1,D2,PMIP,IMIP
      INTEGER*2 TTPRP,TTPCP,TENT,IPRELP
      INTEGER*4 NLEN,C,TMEN,TCELL,FN,NINSTS,I,TYPE
      REAL X,Y,RAD
      DOUBLE PRECISION DN
      LOGICAL OK,OPTION,QUIT,EXIST,USED
      CHARACTER*80 FILNM,SFILE*80
      EXTERNAL NLEN,INSC10,REGC67
C
      NINSTS=0
C     set FN for component
      IF (FN.EQ.1) THEN
C        Component entity
C        load entitty type for COMPI
         DAXTYP = DAXCMP
         TENT=COMPI
         FILTYP='COMPM'
      ELSE
C        Symbol entity
C        load entity type for SYMBI
         DAXTYP = DAXSYM
         TENT=SYMBI
         FILTYP='SYMBM'
      END IF
C
 5    CONTINUE
C     clear the part name
      FILNM=' '
C
C     Force the user for a component name first
      MEN=3
C     'n' is the token used by component name
      CCMD='n'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
C
      CELLN=TCELL
      GOTO 11
C
 10   CONTINUE
C
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
C
 11   CONTINUE
C
      IF (MEN.EQ.3) THEN
 20      CONTINUE
         TMEN=MEN
         TCELL=CELLN
         CALL GTMCHI(MEN,CELLN)
         IF (CCMD.EQ.'n') THEN
C***************************************************************
C                     NAME OPTION                              *
C***************************************************************
C           go get a file name for the component
 15         CONTINUE
            IF (FN.EQ.1) THEN
C              prompt for Component
C              "Type in Component name :"
               I=389
            ELSE
C              prompt for Symbol
C              "Type in SYMBOL name :"
               I=390
            END IF
            CALL DCPRNT(I)
            IF(FN.EQ.1) TYPE = 2
            IF(FN.EQ.2) TYPE = 3
            CALL DISFIL(SFILE,FILNM,TYPE,OK,QUIT)
            IF(QUIT) THEN
                GOTO 10
            ENDIF
            IF(.NOT.OK) THEN
                CALL DPRMXP(I,FILNM)
                IF (NLEN(FILNM).EQ.0) THEN
                   CALL MNLRG1
                   GOTO 99
                ENDIF
            ENDIF
C           fold name to upper case
C           test for existance of instance within database
            TYPE=FN+2
            CALL GTMCLO(MEN,CELLN)
C
CAPOLLO
C          fold this name down if on apollo to compare
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C           CALL UNFOLD(FILNM)
CAPOLLO
C
 
            CALL CRUNCH(FILNM)
            CALL INSC10(FILNM,TYPE,IMIP,OK)
            IF (.NOT.OK) THEN
C              no instance in the drawing
               USED=.FALSE.
               IF (FN.EQ.1) THEN
C                 Component not found
C                 "Component not used in drawing"
                  I=377
               ELSE
C                 Symbol not found
C                 "Symbol not used in drawing"
                  I=378
               END IF
               CALL DEPRNT(I)
               CALL GTMCLO(TMEN,TCELL)
C              regen the menu
               GOTO 15
            ELSE
C              instance of given name present
C              test for master externally
               USED=.TRUE.
               CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
               IF (.NOT.OK) THEN
C                 external reference not found
                  EXIST=.FALSE.
C                 "No external master found"
                  CALL DEPRNT(375)
               ELSE
C                 external master does exist
                  EXIST=.TRUE.
                  CLOSE(UNIT=PARFUN,STATUS='KEEP')
CIBM
C                  LFU(PARFUN)=.FALSE.
CIBM
               END IF
            END IF
C           regenerate the menu
            CALL MNLRG1
         ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                    ACCEPT OPTION                             *
C***************************************************************
C           test for valid regen status
            IF (USED .AND. EXIST) THEN
C              safe to regenerate
               CALL REGC67(FILNM,TENT,FN,PMIP,NINSTS,OK)
C              " ... Regen complete"
               CALL DCPRNT(371)
            ELSE
C              nothing to regenerate
C              "Nothing to regenerate!"
               CALL DEPRNT(374)
            END IF
C           ensure scratch file is cleared
            VNDATA=NDATA+1
            CALL UNFLAG(.TRUE.)
C***************************************************************
C                     END OF OPTIONS                           *
C***************************************************************
         END IF
C
 99      CONTINUE
         CALL GTMCLO(TMEN,TCELL)
         GOTO 10
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') THEN
         RETURN
      ELSE
C        invalid cursor hit
         CALL DEPRNT(117)
         GOTO 10
      END IF
C     function complete
      END
C
C
      SUBROUTINE REGC04(TTMIP,NINSTS,OK)
C     ==================================
C
C1    vartype             I2  I4   L
C1    iostatus            I   O    O
C
C2    Subroutine REGC04 searches for component or symbol
C2    instances,and resolves all of them
C2    The search starts from TTMIP and continues until
C2    no more data is added to the data base.
C2    The total number of instances resolved is returned
C2    in NINSTS
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
C
      INTEGER*4 I,K,NINSTS,NRESLV,K1,K2,PN
      INTEGER*2 TTMIP,HUND2,TMPENT,IPRELP,TMIP
      LOGICAL OK,FOUND,RESOLV
      CHARACTER*80 SRCNAM
      EXTERNAL DIR500,DTR500,INSC90
C
      NINSTS=0
      NRESLV=0
      K=0
      HUND2=100
      K1=TTMIP
      K2=NMIPOS
      PN=0
 5    CONTINUE
C     keep track of the number of passes required
      PN=PN+1
C     start first pass through data
      DO 50 I=K1,K2-1
C        read MI data
         TMIP=I
         CALL DIR500(TMIP,OK)
         IF (.NOT.OK) GOTO 99
         TMPENT=IMBUFF(2)
         FOUND=IMBUFF(1).NE.HUND2.AND.
     +         (TMPENT.EQ.COMPI.OR.TMPENT.EQ.SYMBI)
         IF (FOUND) THEN
C           found valid comp or symb instance
            NINSTS=NINSTS+1
C           read the name
            CALL DTR500(IMBUFF(9),OK)
            IF (.NOT.OK) GOTO 99
C           buffer the name
            SRCNAM=CBUFF
C           use INSC90 to resolve instances
C           WRITE(10,*)'[REGC04] Resolving',TMIP,IMBUFF(4),' ',SRCNAM
            CALL INSC90(SRCNAM,TMPENT,TMIP,IPRELP,RESOLV)
            IF (.NOT.RESOLV) THEN
C              tell him cannot find master
C              WRITE(UNIT=10,FMT=*)'[regc04] Cannot resolve ',SRCNAM
              CALL CPRINT('CANNOT FIND MASTER '//SRCNAM)
              K=NMIPOS
            ELSE
               NRESLV=NRESLV+1
C              update end pointer as new instances
C              are resolved
               K=NMIPOS
            END IF
         END IF
 50   CONTINUE
      IF (K.GT.K2) THEN
C        data has been added during this pass
C        must do another pass to complete the update
C        set start to previous NMIPOS
         K1=K2
C        set limit to current NMIPOS
         K2=K
         GOTO 5
      END IF
C
C      WRITE(UNIT=10,FMT=*)'[regc04] Total of ',NINSTS,' Insts found'
C      WRITE(UNIT=10,FMT=*)'[regc04] Total of ',NRESLV,' Insts resolved'
 99   CONTINUE
C
      END
C
C
      SUBROUTINE REGC66(ENTNAM,TENT,ERASE,SCUNIT,NINSTS)
C     =========================================================
C
C1    vartype           C*(*)   I2    L     I4     I4
C1    iostatus            I     I     I     I      IO
C
C2    Subroutine REGC66 searches within the current
C2    database for an entity of type TENT
C2    based on the value of FN, and name ENTNAM
C2    returning the number of instances found
C2    If ERASE is true,then the instances should
C2    be erased from the screen.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 PMIP,P2,TENT,D1,D2
      INTEGER*4 NINSTS,SCUNIT
      REAL R1,R2,M(3,3)
      LOGICAL OK,ERASE
      CHARACTER*(*) ENTNAM
      EXTERNAL DTR500,DIR500,SSFLAG,DER566,ERS066
C
      NINSTS=0
      R1=0.0
      R2=0.0
      D1=0
      D2=0
      P2=1
 10   CONTINUE
      IF (P2.GE.NTXPOS) RETURN
      CALL DTR500(P2,OK)
      IF (CBUFF.EQ.ENTNAM) THEN
C        found correct name,test it
         PMIP=ICBUFF(1)
         CALL DIR500(PMIP,OK)
         IF (IMBUFF(1).NE.100 .AND. IMBUFF(2).EQ.TENT) THEN
C           correct type,return this one
C           increment count of instances
            NINSTS=NINSTS+1
            WRITE(UNIT=SCUNIT,REC=NINSTS) PMIP
            IF (ERASE .AND. VLAYER(IMBUFF(4))) THEN
C              erase allowed,and visible,read the instance
               CALL DER566(PMIP,M,OK)
C              erase from screen
               CALL ERS066(M)
            END IF
         END IF
      END IF
C     increment to next
      P2=P2+1
      GOTO 10
C
      END
C
C
      SUBROUTINE REGC67(ENTNAM,TENT,FN,PMIP,NINSTS,OK)
C     ================================================
C
C1    vartype            C*(*)  I2  I4  I2    I4   L
C1    iostatus             I    I   I   O     O    O
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/params.inc'
      include 'include/props.inc'
      include 'include/filunit.inc'
      include 'include/wrkdat.inc'
C
      INTEGER*2 TENT,PMIP,IMIP,IPRELP,D1,D2,TP2,ENT
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP
      INTEGER*2 MMIP
      INTEGER*4 FN,NINSTS,I,SCUNIT,SCPNT,RECL,TYPE,J
      REAL R1,R2,M(3,3)
      LOGICAL OK,IMOK
      CHARACTER*(*) ENTNAM,FILETP*80,CCBUFF*80
C
      EXTERNAL INSC10,REGC66,INSC90,DER566,DRW066,OURSCR
      EXTERNAL DIM501,DCPRNT,INSP05,DIR500,DTM500,CLROPF
      EXTERNAL INSP07,DRR950
C
C     reset scratch file pointer
      TYPE=1
      SCPNT=0
      RECL=4
C     open scratch file
      CALL OURSCR(SCUNIT,RECL,OK)
C     find internal master
      CALL INSC10(ENTNAM,FN,PMIP,IMOK)
      CALL OPENNM(ENTNAM,PARFUN,.TRUE.,OK)
C     external exists lets do a regen
      IF(OK) THEN
C         test existance of external component file
C         find all existing instances and erase from
C         screen,before regeneration
C         must go get the file and enter to database
C         save the current record pointers
          TTMIP=NMIPOS
          TTPDP=NPDPOS
          TTTXP=NTXPOS
          TTRLP=NRLPOS
          TTPRP=NPRPOS
          TTPCP=NPCPOS
C         put the new master in
          CALL DCPRNT(379)
          IF (FN.EQ.1) THEN
              FILETP='COMPM'
              FILTYP='COMPM'
          ELSE
              FILETP='SYMBM'
              FILTYP='SYMBM'
          END IF
          CALL INSP05(ENTNAM,FILETP,
     +          TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
          CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C          LFU(PARFUN)=.FALSE.
CIBM
          IF( TTMIP.EQ.NMIPOS ) THEN
              CALL DEPRNT(678)
              GOTO 99
          END IF
      ELSE
          CALL DEPRNT(679)
          GOTO 99
      ENDIF
C     was there an internal master
      IF(IMOK) THEN
C        delete the old master
         D1=1
         D2=100
         CALL DIM501(PMIP,D1,D2,OK)
      ENDIF
      CALL REGC66(ENTNAM,TENT,.TRUE.,SCUNIT,NINSTS)
C     "Resolving instances"
C     load new master file
C     find the MIP for the component master
C     read it from the first entity of the master
      CALL DIR500(TTMIP,OK)
C     save parent pointer to header
      D1=IMBUFF(8)
C     read header MI entry
      CALL DIR500(D1,OK)
C     save relation pointer
      IPRELP=IMBUFF(10)
C     now read relation header entry for master
      CALL DRR950(IMBUFF(10),OK)
C     save MI pointer to component master
      PMIP=RLBUFF(3)
      MMIP=PMIP
C     read text of comp/symb master name
      CALL DIR500(PMIP,OK)
      TP2=IMBUFF(9)
      CALL DTR500(TP2,OK)
C     ensure master name is corrected to filename
C     used for reference,prevents file-name changes
C     conflicting with internal comp names.
      CBUFF=ENTNAM
      CALL DTM500(TP2,PMIP,OK)
C     must correct master data for current database unit
      CALL CLROPF()
      CALL INSP07(FN,TTMIP,TYPE)
C
C     modify position 10 in MI to resolve the instance
      D1=10
C     resolve all instances to new master
      DO 150 I=1,NINSTS
C        find instance MIP from scratch file
         READ(UNIT=SCUNIT,REC=I) IMIP
C        resolve the instance
         CALL DIM501(IMIP,D1,IPRELP,OK)
C        get current transform
         CALL ALLRD(IMIP,ENT,M,OK)
C        get master header box
         CALL DER500(MMIP,OK)
C        get master data
         DO 50 J=1,6
             RWORK(J,1)=RDBUFF(J)
             RWORK(J,2)=RDBUFF(J)
 50      CONTINUE
C        sort second diagonal data
         RWORK(2,2)=RWORK(5,1)
         RWORK(5,2)=RWORK(2,1)
C        transform enclosing box of component
         CALL MV0003(RWORK(1,1),M)
         CALL MV0003(RWORK(1,2),M)
C        write instance to database
C         IF ( IMBUFF(10).LE.0) THEN
C            CALL DEPRNT(418)
C         END IF
C        read instance header
         CALL DIR500(IMIP,OK)
         MIP=IMIP
C        modify instance
         CALL DEM566(M,IMIP,OK)
 
         IF (OK) THEN
C           read the instance data
            CALL DER566(IMIP,M,OK)
C           draw the instance
            CALL DRW066(M,OK)
         END IF
 150  CONTINUE
C
 99   CONTINUE
C     close scratch file
      CLOSE(UNIT=SCUNIT,STATUS='DELETE')
CIBM
C      LFU(SCUNIT)=.FALSE.
CIBM
      END
C
C
C
      SUBROUTINE REGC68(ENTNAM,TENT,FN,TMIP,OK)
C     =========================================
C
C1    vartype            C*(*)  I2  I4  I2   L
C1    iostatus             I    I   I   O    O
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/params.inc'
      include 'include/props.inc'
      include 'include/filunit.inc'
      include 'include/wrkdat.inc'
      include   'include/swind.inc'
C
      INTEGER*2 TENT,PMIP,TMIP,IPRELP,D1,D2,TP2,ENT
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP
      INTEGER*2 MMIP
      INTEGER*4 FN,NINSTS,I,SCPNT,RECL,TYPE,J
      REAL R1,R2,M(3,3)
      LOGICAL OK,IMOK
      CHARACTER*(*) ENTNAM,FILETP*80,CCBUFF*80
C
      EXTERNAL INSC10,REGC66,INSC90,DER566,DRW066,OURSCR
      EXTERNAL DIM501,DCPRNT,INSP05,DIR500,DTM500,CLROPF
      EXTERNAL INSP07,DRR950
C
C     reset scratch file pointer
      TYPE=1
      SCPNT=0
C     find internal master
      CALL INSC10(ENTNAM,FN,PMIP,IMOK)
      IF (.NOT.IMOK) THEN
         CALL OPENNM(ENTNAM,PARFUN,.TRUE.,OK)
C        external exists lets do a regen
         IF(OK) THEN
C            test existance of external component file
C            find all existing instances and erase from
C            screen,before regeneration
C            must go get the file and enter to database
C            save the current record pointers
             TTMIP=NMIPOS
             TTPDP=NPDPOS
             TTTXP=NTXPOS
             TTRLP=NRLPOS
             TTPRP=NPRPOS
             TTPCP=NPCPOS
C            put the new master in
             IF (FN.EQ.1) THEN
                 FILETP='COMPM'
                 FILTYP='COMPM'
             ELSE
                 FILETP='SYMBM'
                 FILTYP='SYMBM'
             END IF
             CALL INSP05(ENTNAM,FILETP,
     +             TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
             CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C             LFU(PARFUN)=.FALSE.
CIBM
             IF( TTMIP.EQ.NMIPOS ) THEN
                 CALL DEPRNT(673)
                 GOTO 99
             END IF
C            find the MIP for the component master
C            read it from the first entity of the master
             CALL DIR500(TTMIP,OK)
C            save parent pointer to header
             D1=IMBUFF(8)
         ELSE
             CALL DEPRNT(674)
             GOTO 99
         ENDIF
      ELSE
C       there was an internal master
        D1 = PMIP
      ENDIF                   
C     "Resolving instances"
C     load new master file
C     read header MI entry
      CALL DIR500(D1,OK)
C     save relation pointer
      IPRELP=IMBUFF(10)
C     now read relation header entry for master
      CALL DRR950(IMBUFF(10),OK)
C     save MI pointer to component master
      PMIP=RLBUFF(3)
      MMIP=PMIP  
      IF (.NOT.IMOK) THEN
C        read text of comp/symb master name
         CALL DIR500(PMIP,OK)
         TP2=IMBUFF(9)
         CALL DTR500(TP2,OK)
C        ensure master name is corrected to filename
C        used for reference,prevents file-name changes
C        conflicting with internal comp names.
         CBUFF=ENTNAM
         CALL DTM500(TP2,PMIP,OK)
C        must correct master data for current database unit
         CALL CLROPF()
         CALL INSP07(FN,TTMIP,TYPE)
      ENDIF
C
C     modify position 10 in MI to resolve the instance
      D1=10
C     resolve the instance
      CALL DIM501(TMIP,D1,IPRELP,OK)
C     get current transform
      CALL ALLRD(TMIP,ENT,M,OK)
C     get master header box
      CALL DER500(MMIP,OK)
C     get master data
      DO 50 J=1,6
          RWORK(J,1)=RDBUFF(J)
          RWORK(J,2)=RDBUFF(J)
 50   CONTINUE
C     sort second diagonal data
      RWORK(2,2)=RWORK(5,1)
      RWORK(5,2)=RWORK(2,1)
C     transform enclosing box of component
      CALL MV0003(RWORK(1,1),M)
      CALL MV0003(RWORK(1,2),M)
C     read instance header
      CALL DIR500(TMIP,OK)
C     modify instance 
      CALL DEM566(M,TMIP,OK)
      
      IF (OK) THEN
C        read the instance data
         CALL DER566(TMIP,M,OK)
C        draw the instance
         CALL DRW066(M,OK)
      END IF
C
 99   CONTINUE                
      END
C
C
      SUBROUTINE REGS00()
C     ===================
C
C1    no arguments required
C2
C2    controls operation of the REGEN SYMBOL function
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      INTEGER FN
      EXTERNAL REGC02
C
C     set function number to indicate
C     symbol regeneration.
      FN=2
C     load menu options for REGEN SYMB
      CALL MNLRG1
C     enter the REGEN COMP OR SYMB routine
      CALL REGC02(FN)
C
      END
C
C
