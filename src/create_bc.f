C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 create.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     
C     SUBROUTINE CRTC00()
C     SUBROUTINE CRTC01(FN)
C     SUBROUTINE CRTC02(FN)
C     SUBROUTINE CRTC03(FN,ATTACH,QUIT,OK)
C     SUBROUTINE CRTC04(FN,ONLINE,ATTACH,FILNM)
C     SUBROUTINE CRTC10(TMIP,OK)
C     SUBROUTINE CRTG00()
C     SUBROUTINE CRTG01()
C     SUBROUTINE CRTG02()
C     SUBROUTINE CRTG03(OK)
C     SUBROUTINE CRTM00()
C     SUBROUTINE CRTP00()
C     SUBROUTINE CRTP02()
C     SUBROUTINE CRTP05(FNAM,FTYP,TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
C     SUBROUTINE CRTS00()
C     SUBROUTINE DEW050(PMIP,OK)
C     SUBROUTINE DEW056(FN,TTMIP,LOCAL,PMIP,OK)
C     SUBROUTINE FLGENT(TENT)
C     SUBROUTINE GETGRP(TMIP,OK)
C     SUBROUTINE GRPSIZ(PMIPP,NGENTS)
C     SUBROUTINE MAJCR1()
C     SUBROUTINE MNICRT()
C     SUBROUTINE MNLC00()
C     SUBROUTINE MNLC01()
C     SUBROUTINE MNLM02()
C     SUBROUTINE MNLP00()
C     SUBROUTINE STSENT(TMIP)
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CRTC00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the CREATE COMPONENT function
C
      include 'include/menun.inc'
C
      INTEGER FN
      EXTERNAL CRTC01
C
C     set function number to 1 for component
      FN=1
C     enter the CREATE COMPONENT routine
      CALL CRTC01(FN)
C
      END
C
C
      SUBROUTINE CRTC01(FN)
C     =====================
C
C1    vartype           I
C1    iostatus          I
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/params.inc'
C
      INTEGER FN
      LOGICAL OK
      CHARACTER CHAR
      EXTERNAL GTMCLO,GTCLRM,CRTC02,MNLC00,UNFLAG,CLROPF
C
C     clear mode flags for transformation
      CALL CLROPF()
      OK=.TRUE.
C     initialize CREATE COMPONENT menu
      CALL MNLC00()
C     must test for valid units and scale for symbol
      IF (FN.EQ.2) THEN
C        test DB units
C        only mm and 1/1 allowed for symbols
         OK=DBUFAC.EQ.0.1.AND.DRWSCL.EQ.1.0
      END IF
      IF (OK) THEN
C       enter the CREATE COMPONENT or SYMBOL routine
         CALL CRTC02(FN)
      ELSE
C        tell him symbol not allowed
         CALL DEPRNT(409)
         MEN=0
      END IF
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C
      SUBROUTINE CRTC02(FN)
C     =====================
C
C1    vartype           I
C1    iostatus          I
C
C2    Subroutine CRTC02 is the main working routine
C2    for the CREATE COMPONENT/SYMBOL function.Uses the normal
C2    window search routines for entity selection.
C2       FN=1 create COMPONENT
C2       FN=2 create SYMBOL
C2       FN=3 create PART
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      REAL X,Y
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I,FN,DNUM
      LOGICAL OK,OPTION,QUIT,ATTACH
      EXTERNAL CRTC03,FINDET,ALSRCH
C
      ATTACH=.FALSE.
C     enable searching for all entity types
      CALL ALSRCH()
CCIBM
CCC     This is to stop nested components.
CC      ENSRCH(COMPI)=.FALSE.
CC      ENSRCH(SYMBI)=.FALSE.
CCIBM
      IF ( FN .EQ. 1 ) THEN
         DNUM=43
      ELSE
         DNUM=354
      END IF
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(DNUM,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) THEN
         IF ( OPFLAG(4) ) THEN
            CALL WO2SC(-REFDAT(4,1),-REFDAT(4,2),X,Y)
            CALL BCROSS(X,Y)
         END IF
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL CRTC03(FN,ATTACH,QUIT,OK)
C        OK indicates completion
         IF (OK .OR. QUIT ) GOTO 99
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE CRTC03(FN,ATTACH,QUIT,OK)
C     ====================================
C
C1    vartype            I    L    L   L
C1    iostatus           I    IO   O   O
C
C2    Subroutine CRTC03 is the main working routine
C2    for the creation of components/symbols
C2       FN=1 create COMPONENT
C2       FN=2 create SYMBOL
C2       FN=3 create PART
C2    ATTACH is a logical which indicates whether
C2    properties should be attached to the component
C2    or not.It is set from the option list.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL WX1,WY1
      DOUBLE PRECISION DN
      INTEGER*4 TMEN,TCELL,C,I,FN,DNUM
      CHARACTER CHAR,FILNM*80
      LOGICAL OK,OPTION,QUIT,ATTACH
      INTRINSIC CHAR
C
      EXTERNAL MNLPTS,MNUPTS,WINDOW,GTMCLO
      EXTERNAL ZSFLAG,FINDP0,CRTC04,UNFLAG
C
 25   CONTINUE
C     Yeah you've guessed it, crash time again
C     initialise all REALS cos the code is in RAGS
      WX1 = 0.0
      WY1 = 0.0      
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processinig here
 20   CONTINUE
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
      ELSE IF(CCMD.EQ.'O') THEN
C***************************************************************
C                 COMP ORIGIN  OPTION                          *
C***************************************************************
         IF ( OPFLAG(4) ) THEN
C           User has already defined it once
C           We will get rid of this one first
            CALL WO2SC(-REFDAT(4,1),-REFDAT(4,2),WX1,WY1)
            CALL BCROSS(WX1,WY1)
         END IF
C        load the point modes to menu
         CALL MNLPTS()
         IF (FN.EQ.1) THEN
C           COMPONENT
            DNUM=44
         ELSE IF (FN.EQ.2) THEN
C           SYMBOL
            DNUM=44
         ELSE
C           PART
            DNUM=55
         END IF
C        go get a point
         CALL FINDP0(DNUM,WX1,WY1,OPTION,QUIT)
C        unload the point modes
         CALL MNUPTS()
         IF (QUIT) GOTO 99
         IF (OPTION) THEN
C           ensure caller not hilited
            CALL GTMCLO(TMEN,TCELL)
            GOTO 25
         END IF
C        must have the origin point ok
C        save the translation
         REFDAT(4,1)=-WX1
         REFDAT(4,2)=-WY1
         CALL WO2SC(-REFDAT(4,1),-REFDAT(4,2),WX1,WY1)
         CALL BCROSS(WX1,WY1)
C        set translate flag
         OPFLAG(4)=.TRUE.
      ELSE IF (CCMD.EQ.'P') THEN
C***************************************************************
C             ATTACH PROPERTIES  OPTION                        *
C***************************************************************
         ATTACH=.NOT.ATTACH
         IF (.NOT.ATTACH) THEN
C           cancel attachment of properties
            CALL GTMCLO(TMEN,TCELL)
         END IF
         RETURN
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C             RETRACE  OPTION                                  *
C***************************************************************
C        if backspace char,remove last entity list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.FALSE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C        reset return status
         OK=.FALSE.
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
C           test for origin definition
            IF (OPFLAG(4)) THEN
               IF (FN.EQ.1) THEN
                  DNUM=45
               ELSE IF (FN.EQ.2) THEN
                  DNUM=357
               ELSE
                  DNUM=56
               END IF
               CALL DCPRNT(DNUM)
               IF (FN.EQ.3) THEN
C                 go and create a part file
                  CALL CRTC04(FN,.TRUE.,.FALSE.,FILNM)
               ELSE
C                 go and create a component/symbol file
C                 tell the routine we are online
C                 ATTACH true if properties required
C                 FILNM will return the name used
                  CALL CRTC04(FN,.TRUE.,ATTACH,FILNM)
C                 set flag to indicate completion
                  OK=.TRUE.
C                 ensure screen flags are cleared before leaving
                  CALL UNFLAG(.TRUE.)
C                  CALL BCROSS(-REFDAT(4,1),-REFDAT(4,2))
                  CALL BCROSS(WX1,WY1)
               END IF
            ELSE
C              no defined origin,tell him
               CALL DEPRNT(46)
            END IF
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
      END IF
 99   CONTINUE
C     unload the point modes
      CALL MNUPTS()
C     ensure caller no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE CRTC04(FN,ONLINE,ATTACH,FILNM)
C     =========================================
C
C1    vartype           I    L       L   C*80
C1    iostatus          I    I       I    IO
C
C2    Subroutine CRTC04 takes the data currently
C2    idintified in the SWIND buffer and uses it
C2    to create a COMPONENT file.Logical ONLINE
C2    indicates interactive use if true,batch use if not.
C2       FN=1 create COMPONENT
C2       FN=2 create SYMBOL
C2       FN=3 create PART
C2       FN=4 create SECTION
C2    ATTACH passed true if properties required on component.
C2    If batch use,then FILNM should contain the name
C2    of the component to be created.
C
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/nbuff.inc'
      include 'include/props.inc'
      include 'include/lfu.inc'
      include 'include/macro.inc'
      include 'include/ftypes.inc'
      include 'include/product.inc'
      include  'include/viewport.inc'
      include  'include/swind.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,II,PMIP,TTPRP,TTPCP
      INTEGER*4 NLEN,FN,DNUM,DNUM2,DNUM3
      INTEGER*4 TYPE
      INTEGER*4 ST
      INTEGER*4 TDATA
      LOGICAL OK,ONLINE,ATTACH
      CHARACTER*80 FILNM,COMSYM*5,EXT*4
      EXTERNAL NLEN,OPENNM,DEPRNT,MOVMAS,DEW056,CRTP05
C
C     set type to create
      DNUM=58
      DNUM2=59
      DNUM3=368
      IF (FN.EQ.1) THEN
C        COMPONENT creation
         COMSYM='COMPM'
         EXT='.cmp'
         DAXTYP = DAXCMP
      ELSE IF (FN.EQ.2) THEN
C        SYMBOL creation
         COMSYM='SYMBM'
         EXT='.sym'
         DAXTYP = DAXSYM
      ELSE IF (FN.EQ.4) THEN
C        SECTION creation
         COMSYM='SECT'
         DAXTYP = DAXSEC
         EXT='.sec'
      ELSE
C        PART creation
         COMSYM='PART'
         DAXTYP = DAXPRT
         EXT='.prt'
      END IF
C     Set basic type and save number of elements for use by browser
      TYPE = DAXTYP
      TDATA = NDATA
C
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
 10   CONTINUE
      IF (ONLINE) THEN
C        go get a file name for the part
         CALL DPRMXP(DNUM,FILNM)
         IF (NLEN(FILNM).EQ.0) RETURN
C        check that it is not the product name
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C         CALL UNFOLD(FILNM)
         IF(INDEX(FILNM,PRNAM(1:NLEN(PRNAM))).GT.0) THEN
             CALL DEPRNT(276)
             GOTO 10
         END IF
         CALL SUFFIX(FILNM,EXT)
         IF(INDEX(FILNM,'/').EQ.0) THEN
C          do not override pathname
           IF(FN.EQ.1) THEN
               CBUFF=PATHN(3)//FILNM
           ELSEIF(FN.EQ.2) THEN
               CBUFF=PATHN(5)//FILNM
           ELSEIF(FN.EQ.3) THEN
               CBUFF=PATHN(4)//FILNM
CSUN
           ELSEIF(FN.EQ.4) THEN
               CBUFF=PATHN(6)//FILNM
CSUN
            ENDIF
 
            CALL CRUNCH(CBUFF)
            FILNM=CBUFF
         ENDIF
CAPOLLO|IBM|PC386
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C         CALL UNFOLD(FILNM)
CAPOLLO|IBM|PC386
         CALL EPRINT('NEW NAME IS '//FILNM)
      END IF
C     ensure correct suffix in use
C      write(10,*)'[crtc04] filnm=',FILNM
      IF (.NOT.ONLINE) THEN
C       if batch mode,kill the file to ensure
C       overwrite if necessary
        CALL UKILLF(FILNM,OK)
      END IF
C     open a new file,ask if exists
      CALL  OPENNM(FILNM,PARFUN,.FALSE.,OK)
C
      IF (.NOT.OK) THEN
C        invalid file for some reason
         CALL DEPRNT(DNUM2)
         IF (ONLINE) GOTO 10
C        if batch,do nothing and return
         RETURN
      END IF
 
C
C     save component name
      CBUFF=FILNM
C
C     set copy flag
      COPYIT=.TRUE.
C     set number of copies
      NNCOPY=1
C     disable erase and draw of entities during transformation
      OPFLAG(4)=.TRUE.
      OPFLAG(9)=.FALSE.
      OPFLAG(10)=.FALSE.
C     go do the move
      CALL MOVMAS(.FALSE.)
C     reset the copy flag
      COPYIT=.FALSE.
C     reset the number of copies
      NNCOPY=0
C     enable erase and draw of entities during transformation
      OPFLAG(9)=.TRUE.
      OPFLAG(10)=.TRUE.
C
      IF (FN.LE.2) THEN
C        place component limits in buffer area
         RDBUFF(1)=REFDAT(9,1)
         RDBUFF(2)=REFDAT(9,2)
         RDBUFF(3)=0
         RDBUFF(4)=REFDAT(10,1)
         RDBUFF(5)=REFDAT(10,2)
         RDBUFF(6)=0
C        place name in text buffer
         CBUFF=FILNM
      END IF
C
      IF (FN.LE.2) THEN
C        write the component/symbol definition into
C        the work files,and create relation list etc
         CALL DEW056(FN,TTMIP,.FALSE.,PMIP,OK)
C        PMIP returns MI used for COMPONENT/SYMBOL header
         IF (ATTACH) THEN
C           tell him we need properties
C           "Attach properties to component"
            CALL DCPRNT(DNUM3)
C           try to attach properties
            CALL CRTC10(PMIP,OK)
         END IF
      END IF
C
C     TRY SAVING THE component
C     go store the data
      CALL CRTP05(FILNM,COMSYM,TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
C
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
C
      NDATA = TDATA
      CALL CREATEBROWSEIMAGE(FILNM,TYPE,ST)
      NDATA = 0
C     set the pointers back to original values
      NMIPOS=TTMIP
      NPDPOS=TTPDP
      NTXPOS=TTTXP
      NRLPOS=TTRLP
      NPRPOS=TTPRP
      NPCPOS=TTPCP
      LDFILE(CVPN)=TLDFIL
C     clear translate flag
      OPFLAG(4)=.FALSE.
C
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[CRTC04] Error closing file fn=',FN
C
      END
C
C
      SUBROUTINE CRTC10(TMIP,OK)
C     ==========================
C
C1    vartype            I2  L
C1    iostatus           I   O
C
      INTEGER*2 TMIP,PRPNT
      INTEGER*4 NLEN
      LOGICAL OK
      CHARACTER*80 PNAME
      EXTERNAL ATTP11,ATTP03,NLEN
C
      OK=.FALSE.
 10   CONTINUE
      CALL ATTP11(PNAME,PRPNT,OK)
      IF (OK) THEN
C        property  has been found,and is in buffer
C        PRPNT now points to it
C        now attach entity
         CALL ATTP03(PNAME,PRPNT,TMIP,OK)
C        go back for more
         GOTO 10
      ELSE
C        no property found
         IF (NLEN(PNAME).GT.0) GOTO 10
      END IF
C
      END
C
C
      SUBROUTINE CRTG00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the CREATE GROUP function
C
      include 'include/menun.inc'
C
      EXTERNAL CRTG01
C
C     enter the CREATE GROUP routine
      CALL CRTG01()
C
      END
C
C
      SUBROUTINE CRTG01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
C
      EXTERNAL GTCLRM,MNLC01,CRTG02,UNFLAG,CLROPF
C
C     clear mode flags for transformation
      CALL CLROPF()
C
C     initialize CREATE GROUP menu
      CALL MNLC01()
C     enter the CREATE GROUP routine
      CALL CRTG02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C
      SUBROUTINE CRTG02()
C     ====================
C
C2    Subroutine CRTG02 is the main working routine
C2    for the CREATE GROUP function.Uses the normal
C2    window search routines for entity selection.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
C
      REAL X,Y
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I
      LOGICAL OK,OPTION,QUIT
      EXTERNAL CRTG03,FINDET,ALSRCH
C
C     enable searching for all entity types
      CALL ALSRCH()
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(49,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) RETURN
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL CRTG03(OK)
C        OK indicates completion
         IF (OK) GOTO 99
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE CRTG03(OK)
C     =====================
C
C2    Subroutine CRTG03 is the main workiing routine
C2    for the creation of GROUPS
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL WX1,WY1
      DOUBLE PRECISION DN
      INTEGER*2 P
      INTEGER*4 C,I
      CHARACTER CHAR
      LOGICAL OK,OPTION,QUIT
      INTRINSIC CHAR
      EXTERNAL MNLPTS,MNUPTS,EPRINT,WINDOW,GTMCLO
      EXTERNAL ZSFLAG,DEW050,UNFLAG
C
 25   CONTINUE
      OK=.FALSE.
C     start option processinig here
 20   CONTINUE
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
         CALL GTMCLO(MEN,CELLN)
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C             RETRACE  OPTION                                  *
C***************************************************************
C        if backspace char,remove last entity list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.TRUE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C        reset return status
         OK=.FALSE.
         CALL GTMCLO(MEN,CELLN)
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
C           set flag to indicate completion
            CALL DCPRNT(50)
            CALL DEW050(P,OK)
            OK=.TRUE.
C           ensure screen flags are cleared before leaving
            CALL UNFLAG(.TRUE.)
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
      END IF
 99   CONTINUE
C
      END
C
C
      SUBROUTINE CRTM00()
C     ===================
C1    no arguments required
C
C2    Controls the CREATE MENU OPTION
C
      include 'include/menun.inc'
      include 'include/macro.inc'
      include 'include/wildc.inc'
 
      CHARACTER FILNAM*80,CURPN*48,OUTFIL*80,TEXT*80
      CHARACTER SFILE*80,FILES*80
      INTEGER*4 C,ACTCEL,LPATH,LFILE,NLEN1
      LOGICAL OK,FOK
      REAL X,Y
C     load upm the menu cells on menu 3
      CALL MNLM02()
C     Highlight the drawings cell for default.
C     initialise the variables
      ACTCEL=1
      CALL GTMCHI(3,ACTCEL)
      CALL DCPRNT(38)
10    CALL TCURS(C,X,Y)
      IF(CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
      IF(MEN.EQ.2) RETURN
C     Highlight the actice cell and set register
      IF(MEN.EQ.3.AND.(CELLN.GE.1.AND.CELLN.LT.(1+(3*NPATH)) )) THEN
C        dont know if it is already active ie the plonker
C        tries to dig' it again
         IF(CELLN.NE.ACTCEL) THEN
            CALL GTMCLO(3,ACTCEL)
            ACTCEL=CELLN
         ENDIF
         GOTO 10
      ELSE IF(CCMD.EQ.CHAR(150)) THEN
C        accept the options and generate the output file.
         CURPN=PATHN((ACTCEL+2)/3)
         LPATH=NLEN1(CURPN)
         FILNAM=WILDC((ACTCEL+2)/3)
         LFILE=NLEN1(FILNAM)
         IF (LFILE.GT.4) LFILE=4
         FILES=CURPN(:LPATH)//WILDC((ACTCEL+2)/3)
         OUTFIL=CURPN(:LPATH)//FILNAM(:LFILE)//'.msf'
         CALL CRUNCH(OUTFIL)
         CALL CRUNCH(FILES)
C        Serch the directory and generate the file
         CALL DCPRNT(468)
         CALL CPRINT(' '//OUTFIL)
         CALL DIRFIN(OUTFIL,FILES,FOK)
         IF(.NOT.FOK) THEN
C           Error generated display it
            CALL DEPRNT(469)
         ELSE
C           Everything OK loop back round
            CALL DCPRNT(470)
         ENDIF
C        set accept cell low
         CALL GTMCLO(3,16)
C        tell him  to do, something from the modifier menu
         CALL DCPRNT(284)
      ELSE
         CALL DEPRNT(117)
      ENDIF
      GOTO 10
      END
C
      SUBROUTINE CRTP00()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
      EXTERNAL GTCLRM,MNLP00,CRTP02,UNFLAG,CLROPF
C
C     clear mode flags for transformation
      CALL CLROPF()
C     initialize CREATE PART menu
      CALL MNLP00()
C     enter the CREATE PART routine
      CALL CRTP02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
      SUBROUTINE CRTP02()
C     ====================
C
C2    Subroutine CRTP02 is the main working routine
C2    for the CREATE PART function.Uses the normal
C2    window search routines for entity selection.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      REAL X,Y
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I,FN
      LOGICAL OK,OPTION,QUIT
      EXTERNAL FINDET,ALSRCH
C
C     set function number for PART
      FN=3
C     enable searching for all entity types
      CALL ALSRCH()
CCIBM
CCC     This is to stop nested components.
CC      ENSRCH(COMPI)=.FALSE.
CC      ENSRCH(SYMBI)=.FALSE.
CCIBM
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(54,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) THEN
         IF ( OPFLAG(4) ) THEN
            CALL WO2SC(-REFDAT(4,1),-REFDAT(4,2),X,Y)
            CALL BCROSS(X,Y)
         END IF
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL CRTC03(FN,OPTION,QUIT,OK)
C        OK indicates completion
         IF ( OK .OR. QUIT ) GOTO 99
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE CRTP05(FNAM,FTYP,TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
C     ================================================================
C
C1    vartype          C*(*) C*(*) I2    I2    I2     I2    I2    I2
C1    iostatus           I     I   I     I     I      I     I     I
C
C2    Subroutine CRTP05  writes the part data between
C2    the temporary pointers and the end of the current
C2    database entries to the previously opened file
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
      include 'include/props.inc'
C
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP,II
      INTEGER*2 T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,T2PCP,WUN
      LOGICAL OK
      CHARACTER*(*) FNAM,FTYP
C
      EXTERNAL MAKFHD,SAVFHD,MAKDHD,SAVDHD,SAVNHD,
     + SAVMI1,SAVPD1,SAVTX1,SAVRL1,SAVPR1,SAVPR2,UPR100
C
C     must update properties on the copied entities
C     before writing to external file
      CALL UPR100(TTMIP,OK)
C
C     WRITE(UNIT=10,FMT=*)'[CRTP05] *** CREATING PART ***'
C     ensure writing starts at beginning of file
      REWIND(UNIT=PARFUN,ERR=99)
C
      WUN=1
      T2MIP=TTMIP-WUN
      T2PDP=TTPDP-WUN
      T2TXP=TTTXP-WUN
      T2RLP=TTRLP-WUN
      T2PRP=TTRLP-WUN
      T2PRP=TTPRP-WUN
      T2PCP=TTPCP-WUN
C
C     create file header
      CALL MAKFHD()
C     fix part rev no
      RFHEAD(2)=1.00
C     fix part name and type
      CFHEAD(1)=FTYP
C      WRITE(UNIT=10,FMT=*)'[CRTP05] FTYP,CFHEAD=',FTYP,CFHEAD(1)
      CFHEAD(2)=FNAM
C     write the file header
      CALL SAVFHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     create a drawing header
      CALL MAKDHD()
C     fix part limits
      RDHEAD(6)=REFDAT(9,1)
      RDHEAD(7)=REFDAT(9,2)
      RDHEAD(8)=REFDAT(10,1)
      RDHEAD(9)=REFDAT(10,2)
C     write the drawing header
      CALL SAVDHD(OK)
      IF (.NOT.OK) GOTO 99
      IF (FTYP.EQ.'SECT') THEN
C        SECTION file being created
C        write the section header
         CALL SAVSSC(OK)
         IF (.NOT.OK) GOTO 99
      END IF
C     create a database header
C     set pointers in database header
      NHEADI(1)=NMIPOS-TTMIP
      NHEADI(2)=NPDPOS-TTPDP
      NHEADI(3)=NTXPOS-TTTXP
      NHEADI(4)=NRLPOS-TTRLP
      NHEADI(5)=NPRPOS-TTPRP
      NHEADI(6)=NPCPOS-TTPCP
      NHEADI(7)=0
C     write the database header
      CALL SAVNHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     now get down to writing the part data it's self
C     start with the MI data
      DO 30 II=TTMIP,NMIPOS-WUN
C        write the MI record
         CALL SAVMI1(II,T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,OK)
 30   CONTINUE
C
C        now save all the coordinate data
         DO 31 II=TTPDP,NPDPOS-1
C           write the PD record
            CALL SAVPD1(II,T2MIP,T2PDP,OK)
 31      CONTINUE
C
      IF (NTXPOS.GT.TTTXP) THEN
C        now save all the text data
         DO 32,II=TTTXP,NTXPOS-1
C           write the text record
            CALL SAVTX1(II,T2MIP,T2TXP,OK)
 32      CONTINUE
      END IF
      IF (.NOT.OK) GOTO 99
C
C        now save all the relation data
         DO 33 II=TTRLP,NRLPOS-1
C           write the RL record
            CALL SAVRL1(II,T2MIP,T2PDP,T2TXP,T2RLP,OK)
 33      CONTINUE
C
C        now save all the property index data
         DO 34 II=TTPRP,NPRPOS-1
C           write the PRI record
            CALL SAVPR1(II,T2MIP,T2PRP,T2PCP,OK)
 34      CONTINUE
C
C        now save all the property character data
         DO 35 II=TTPCP,NPCPOS-1
C           write the PRC record
            CALL SAVPR2(II,T2PRP,T2PCP,OK)
 35      CONTINUE
C
C     WRITE(UNIT=10,FMT=*)'[CRTP05] *** PART CREATED ***'
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[CRTPO5] ERROR WRITING PART DATA'
C
      END
C
      SUBROUTINE CRTS00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the CREATE COMPONENT function
C
      include 'include/menun.inc'
C
      INTEGER FN
      EXTERNAL CRTC01
C
C     set function number for SYMBOL
      FN=2
C     enter the CREATE COMPONENT routine
      CALL CRTC01(FN)
C
      END
C
C
      SUBROUTINE DEW050(PMIP,OK)
C     ==========================
C
C1    vartype            I2   L
C1    iostatus           O    O
C
C2    Subroutine DEW050 writes to the database
C2    the definition of a GROUP of entities
C2    using the relation file entry to define the
C2    group from that currently in SWIND scratch file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
C
      INTEGER*2 P,PMIP
      INTEGER*4 NRRECS,NENTS,NXTENT,I,J,II
      LOGICAL OK
C
C
      EXTERNAL FILL,DIW500,DRW950,DRW951,DIR500,DIM500
C
C     clear MI buffer to defaults
      CALL FILL()
C     set correct entity type
      IMBUFF(2)=GROUP
C     ensure font is zero
      IMBUFF(6)=0
C     ensure PDP is zero
      IMBUFF(7)=0
C     ensure TXTP is zero
      IMBUFF(9)=0
C     set pointer to relation header
      IMBUFF(10)=NRLPOS
C     now write header to MI file
      CALL DIW500(PMIP,OK)
C     set relation record type
      RLBUFF(1)=GROUP
C     save parent MI in relation
      RLBUFF(3)=PMIP
C     clear relation buffer
      DO 10 I=4,10
         RLBUFF(I)=0
 10   CONTINUE
C
C     find number of entities in group
      NENTS=NDATA
C     find number of relation records required
      NRRECS=NENTS/7
      IF (NENTS.GT.NRRECS*7) NRRECS=NRRECS+1
C     save number of records
      RLBUFF(4)=NRRECS
C     save number of entities
      RLBUFF(5)=NENTS
C     write relation record
      CALL DRW951(P,OK)
C
C     now write MILIST to relation
      RLBUFF(1)=MILIST
C
      NXTENT=1
      IF (NRRECS.EQ.1) GOTO 40
      DO 30 I=1,NRRECS-1
C        fill the record with MI pointers
         DO 35 J=4,10
C           find the MI position from workfile
            READ(UNIT=SWINDU,REC=NXTENT) RLBUFF(J)
C           read the MI data for the entity
C           and set GROUP status
            CALL DIR500(RLBUFF(J),OK)
            IMBUFF(1)=GROUP
C           point to parent MI
            IMBUFF(8)=PMIP
            CALL DIM500(RLBUFF(J),OK)
            NXTENT=NXTENT+1
 35      CONTINUE
C        write the record to relation file
         CALL DRW951(P,OK)
 30   CONTINUE
C
 40   CONTINUE
C     last relation record to be written
C     clear relation buffer
      DO 20 I=4,10
         RLBUFF(I)=0
 20   CONTINUE
C     fill the record with MI pointers
      DO 50 J=1,NENTS-NXTENT+1
C        find the MI position from workfile
         READ(UNIT=SWINDU,REC=NXTENT) RLBUFF(J+3)
C        read the MI data for the entity
C        and set GROUP status
         CALL DIR500(RLBUFF(J+3),OK)
         IMBUFF(1)=GROUP
C        point to parent MI
         IMBUFF(8)=PMIP
         CALL DIM500(RLBUFF(J+3),OK)
         NXTENT=NXTENT+1
 50   CONTINUE
C     write the last record to relation file
      CALL DRW950(P,OK)
C
      END
C
C
      SUBROUTINE DEW056(FN,TTMIP,LOCAL,PMIP,OK)
C     =========================================
C
C1    vartype            I4 I2     L    I2   L
C1    iostatus           I  I      I    O    O
C
C2    Subroutine DEW056 writes to the database
C2    the definition of a COMPONENT of entities
C2    using the relation file entry to define the
C2    group from that currently in workfiles starting
C2    at MI position TTMIP.
C2    FN = 1 COMPONENT
C2    FN = 2 SYMBOL
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
      include 'include/movdat.inc'
C
      INTEGER*2 I2,P,P2,PMIP,TTMIP,LTTMIP,COMSYM
      INTEGER*4 FN,NRRECS,NENTS,NXTENT,I,J,II
      LOGICAL OK,LOCAL
C
      EXTERNAL FILL,DIW500,DRW950,DRW951,DIR500,DIM500
      EXTERNAL DTW500,DBW500
C
C
C     save pointer to last entity in list
      LTTMIP=NMIPOS-1
C
      IF (.NOT.LOCAL) THEN
C        clear MI buffer to defaults
         CALL FILL()
      END IF
C     set correct entity type
      IF (FN.EQ.1) THEN
         COMSYM=COMPM
      ELSE
         COMSYM=SYMBM
      END IF
      IMBUFF(2)=COMSYM
C     ensure font is zero
      IMBUFF(6)=0
C     ensure PDP is next free space
      IMBUFF(7)=NPDPOS
C     ensure TXTP is next free space for name
      IMBUFF(9)=NTXPOS
C     set pointer to relation header
      IMBUFF(10)=NRLPOS
C     now write header to MI file
      CALL DIW500(PMIP,OK)
C     PMIP now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=PMIP
C     write text to file
      CALL DTW500(P,OK)
C     ensure entity types agree
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=MIP
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 contains pointer to entry in PDF
C
C     *********************************************
C     *********************************************
C
C     create relation structure for entities between
C     TTMIP and LTTMIP
C
C     set the entity status to component master
      DO 50 I2=TTMIP,LTTMIP
C        read MI data for entity
         CALL DIR500(I2,OK)
C        set entity status
         IMBUFF(1)=COMSYM
C        set PMIP for entity
         IMBUFF(8)=PMIP
C        detach individual properties
         IMBUFF(11)=0
C        write MI data back to file
         CALL DIM500(I2,OK)
 50   CONTINUE
C
C     now fill a relation record with the
C     header data for the relation data
C     set relation record type
      RLBUFF(1)=COMSYM
C     save parent MI in relation
      RLBUFF(3)=PMIP
C     clear relation buffer
      DO 10 I=4,10
         RLBUFF(I)=0
 10   CONTINUE
C
C     find number of entities in group
      NENTS=(LTTMIP-TTMIP)+1
C     find number of relation records required
      NRRECS=NENTS/7
      IF (NENTS.GT.NRRECS*7) NRRECS=NRRECS+1
C     save number of records
      RLBUFF(4)=NRRECS
C     save number of entities
      RLBUFF(5)=NENTS
C     write relation record
      CALL DRW951(P,OK)
C
C     now write MILIST to relation
      RLBUFF(1)=MILIST
C
      NXTENT=TTMIP
      IF (NRRECS.EQ.1) GOTO 40
      DO 30 I=1,NRRECS-1
C        fill the record with MI pointers
         DO 35 J=4,10
C           place the MI in the relation list
            RLBUFF(J)=NXTENT
            NXTENT=NXTENT+1
 35      CONTINUE
C        write the record to relation file
         CALL DRW951(P,OK)
 30   CONTINUE
C
 40   CONTINUE
C     last relation record to be written
C     clear relation buffer
      DO 20 I=4,10
         RLBUFF(I)=0
 20   CONTINUE
C     fill the record with MI pointers
      I=4
      DO 31 I2=NXTENT,LTTMIP
         RLBUFF(I)=I2
         I=I+1
 31   CONTINUE
C     write the last record to relation file
      CALL DRW950(P,OK)
C
      END
C
C
      SUBROUTINE FLGENT(TENT)
C     ========================
C
C1    vartype            I2
C1    iostatus           I
C
C2    Subroutine FLGENT flags the entity currently
C2    in the buffers and enters it in the scratch file
C2    connected to SWINDU.
C
      include 'include/nbuff.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
C
      REAL BX,BY
      INTEGER*2 TENT,TDFP,D1,TMIP,PP
C
      EXTERNAL SSFLAG
C
C     don't know TDFP so set it to zero
      TDFP=0
      D1=0
C     set current MIP
      TMIP=MIP
C
      BX=0.0
      BY=0.0
      CALL SSFLAG(TMIP,BX,BY,TDFP,D1,.TRUE.)
C
      END
C
      SUBROUTINE GETGRP(TMIP,OK)
C     ===========================
C
C1    vartype            I2   L
C1    iostatus           I    O
C
C2    Subroutine GETGRP finds all elements of the
C2    GROUP of entities defined in MI position TMIP
C2    flags each and enters it in the scratch file
C2    attached to SWINDU.
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/swind.inc'
C
      INTEGER*4 I,J,NTEMP
      INTEGER*2 TMIP,TTMIP,RELP,NRRECS,NXTRLR,NENTS
      LOGICAL OK
C
      EXTERNAL STSENT,DRR950,DIR500
C
CCIBM
CC      NTEMP=NDATA
CCIBM
C     read the MI data first
      CALL DIR500(TMIP,OK)
      IF (.NOT.OK) RETURN
C     find relation header
      RELP=IMBUFF(10)
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
      OK=(RLBUFF(1).EQ.GROUP)
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
C           and flag each of the entity.
            TTMIP=RLBUFF(I)
CCIBM
CC            IF ( TTMIP.GT.0 ) THEN
CC               CALL DIR500(TTMIP,OK)
CC               IF ( .NOT.ENSRCH(IMBUFF(2)) ) GOTO 99
CC            END IF
CCIBM
            IF (TTMIP.GT.0) CALL STSENT(TTMIP)
C           re-read relation record,may have been changed
C           by comp or symbol
            CALL DRR950(NXTRLR,OK)
 10      CONTINUE
 15   CONTINUE
C
      RETURN
CCIBM
CC 99   CONTINUE
CC      CALL ZSFLAG(.TRUE.,OK)
CC      IF ( NDATA.GT.NTEMP ) GOTO 99
CC      CALL DEPRNT(639)
CC      NDATA=NTEMP
CCIBM
      END
C
C
      SUBROUTINE GRPSIZ(PMIPP,NGENTS)
C     ===============================
C
C1    vartype            I2     I4
C1    iostatus           I      O
C
C2    Subroutine GRPSIZ returns in NGENTS the number of
C2    entities in thr group defined by header at MI position
C2    PMIPP.NGENTS returned zero if error.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
      INTEGER*2 PMIPP
      INTEGER*4 NGENTS
      LOGICAL OK
      EXTERNAL DIR500,DRR950
C
      NGENTS=0
C     read the MI data
      CALL DIR500(PMIPP,OK)
      IF (OK) THEN
C        read relation record
         CALL DRR950(IMBUFF(10),OK)
         IF (OK) THEN
C           get count from record
            NGENTS=RLBUFF(5)
         END IF
      END IF
C
      END
C
C
      SUBROUTINE MAJCR1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the CREATE mode
C2    of operation is selected from the master menu.
C2    controls operation of the CREATE function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
      include 'include/krap.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL MNICRT,CRTP00,CRTC00,CRTG00,GTCLRM
      EXTERNAL TCURS,DEPRNT,GTMCLO,GTMCHI,CLRPEW,UNFLAG
      EXTERNAL GTHFMC
C
C     Now activate the CREATE major option menu
      CALL MNICRT()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     initialize number of copies to ONE
      NNCOPY=1
      IF(KDEMO) THEN
C        Making CREATE GROUP the default if  DEMO 
C        'G' is the token used by CREATE GROUP
         CCMD='G'
      ELSE
C        Making CREATE PART the default if not DEMO 
C        'P' is the token used by CREATE PART
         CCMD='P'
      ENDIF
C
C     Find the menu and hightlite it
      MEN=2
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 20
C
C
 10   CONTINUE
C     tell him what to do
C     "Select NOUN"
      CALL DCPRNT(284)
C     Read a cursor hit to select CREATE mode
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
         IF (CCMD.EQ.'P') THEN
C           CREATE PART option
C           ensure grouped entities are returned
C           as groups
            IF(KDEMO) THEN
               CALL    ERRORDIALOG(
     +        'This is a DEMO version and cannot CREATE a PART')
               MEN=0
            ELSE
               GSSTAT=2
               CALL CRTP00()
            ENDIF
         ELSE IF (CCMD.EQ.'C') THEN
C           CREATE COMPONENT option
C           ensure grouped entities are not allowed
            IF(KDEMO) THEN
               CALL    ERRORDIALOG(
     +        'This is a DEMO version and cannot CREATE a COMPONENT')
               MEN=0
            ELSE
               GSSTAT=3
               CALL CRTC00()
            ENDIF
         ELSE IF (CCMD.EQ.'G') THEN
C           CREATE GROUP option
C           ensure grouped entities are not allowed
            GSSTAT=3
            CALL CRTG00()
         ELSE IF (CCMD.EQ.'S') THEN
C           CREATE SYMBOL option
C           ensure grouped entities are not allowed
            IF(KDEMO) THEN
               CALL    ERRORDIALOG(
     +        'This is a DEMO version and cannot CREATE a SYMBOL')
               MEN=0
            ELSE
               GSSTAT=3
               CALL CRTS00()
            ENDIF
C         ELSE IF (CCMD.EQ.'M') THEN
C           CREATE MENU option
C           ensure grouped entities are not allowed
C            GSSTAT=3
C            CALL CRTM00()
         ELSE IF (CCMD.EQ.'O') THEN
C           CREATE MENU option
C           ensure grouped entities are not allowed
            CALL CRTOF0()
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
C
      RETURN
      END
C
C
      SUBROUTINE MNICRT()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the CREATE major options.
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
C     Enter the CREATE major options.
C     Hilite the option header
C      CALL GTPMEN('CREATE',' ',2,1)
      CALL GTDMHD(23,2)
C
C     Load the options for create
C2    P is the token for CREATE PART
C      CALL GTPMEN('Part','P',2,2)
      CALL GTDMEN(270,2)
C2    G is the token for CREATE GROUP
C      CALL GTPMEN('Group','G',2,3)
      CALL GTDMEN(271,2)
C2    D is the token for CREATE DETAIL
C      CALL GTPMEN('Detail','D',2,4)
C      CALL GTDMEN(272,2)
C2    C is the token for CREATE COMPONENT
C      CALL GTPMEN('Comp','C',2,5)
      CALL GTDMEN(273,2)
C2    S is the token for CREATE SYMBOL
C      CALL GTPMEN('Symbol','S',2,6)
      CALL GTDMEN(274,2)
      CALL GTDMEN(277,2)
C2    M is the token for CREATE MENU
C      CALL GTDMEN(277,2)
C
      END
C
C
      SUBROUTINE MNLC00()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLC00 loads the COMP ORIGIN
C2    portion of the option menu no3.
C2    Intended for use in CREATE COMPONENT
C2
C2    Tokens used here are O
C2
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTPMEN,MNLC01
C
C2    O is the token for ORIGIN
C     load COMPONENT ORIGIN option
      CALL GTDMEN(278,3)
C2    P is the token for PROPERTIES
      CALL GTDMEN(405,3)
C     load the area,cancel and accept options
      CALL MNLC01()
C
      END
C
C
      SUBROUTINE MNLC01()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLC01 loads the AREA
C2    and CANCEL,and ACCEPT portions of the option
C2    menu no3.
C2    Intended for use in CREATE
C2
C2    Tokens used here are W e CHAR(149) and CHAR(150).
C2
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTPMEN
C
C2    W is the token for INSIDE AREA.
C      CALL GTPMEN('Inside  area','W',3,1)
      CALL GTDMEN(210,3)
C2    e is the token for EXCLUDE
C      CALL GTPMEN('Exclude',' ',3,2)
C     CHAR(149) is the token for CANCEL.
C      CALL GTPMEN('Cancel',CHAR(149),3,16)
      CALL GTDMEN(146,3)
C2    CHAR(13) is token for ACCEPT.
C      CALL GTPMEN('Accept',CHAR(13),3,26)
      CALL GTDMEN(13,3)
C2
      END
      SUBROUTINE MNLM02()
C     ===================
C1    no arguments required
C
      include 'include/macro.inc'
C2    writes out CREATE MENU options
C     load up the cells
      CALL GTCLRM(3)
C
      CALL GTDMEN(289,3)
      CALL GTDMEN(319,3)
      CALL GTDMEN(322,3)
      CALL GTDMEN(323,3)
      CALL GTDMEN(334,3)
      CALL GTDMEN(333,3)
CSUN - backcloth
      CALL GTDMEN(338,3)
CSUN
 
      END
C
C
      SUBROUTINE MNLP00()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLP00 loads the PART ORIGIN
C2    portion of the option menu no3.
C2    Intended for use in CREATE PART
C2
C2    Tokens used here are O
C2
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTDMEN,MNLC01
C
C2    O is the token for ORIGIN
C     load PART ORIGIN option
      CALL GTDMEN(275,3)
C     load the area,cancel and accept options
      CALL MNLC01()
C
      END
C
C
      SUBROUTINE STSENT(TMIP)
C     =======================
C
C1    vartype            I2
C1    iostatus           I
C
C2    Subroutine STSENT locates the entity at MI position
C2    TMIP,flags it on screen,and enters it in the scratch file
c2    connected to SWINDU.
C
      include 'include/nbuff.inc'
C
      INTEGER*2 TMIP,TENT
      REAL M(3,3)
      LOGICAL DELETE
C
      EXTERNAL FLGENT,ALLRD
C
      CALL ALLRD(TMIP,TENT,M,DELETE)
      IF (.NOT.DELETE) THEN
C        flag the entity
         CALL FLGENT(TENT)
      END IF
C
      END
C
C
C
