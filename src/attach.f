C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 attach.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     
C     SUBROUTINE ATTL00()
C     SUBROUTINE ATTN00()
C     SUBROUTINE ATTN01()
C     SUBROUTINE ATTN02()
C     SUBROUTINE ATTP00()
C     SUBROUTINE ATTP01()
C     SUBROUTINE ATTP02()
C     SUBROUTINE ATTP03(PNAME,MASPRP,MIPP,OK)
C     SUBROUTINE ATTP10(PNAME,PRPNT,OK)
C     SUBROUTINE ATTP11(PNAME,PRPNT,OK)
C     SUBROUTINE ATTP50(MASPRP,TMIP,PRPNT,OK)
C     SUBROUTINE ATTP51(DTYPE,ACTION,OK)
C     SUBROUTINE ATTP60(MIPP,MASPRP,PRPNT,OK)
C     SUBROUTINE ATTP70(MIPP,MASPRP,PNAME,OLDLNK,OK)
C     SUBROUTINE MAJAT1()
C     SUBROUTINE MNIATT()
C     SUBROUTINE MNLAN0()
C     SUBROUTINE MNLAP0()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ATTL00()
C     ===================
C1    No arguments required.
C2    Will attach a name to layer so that a layer
C2    can be Hidden,Shown by Name
C
      include 'include/menun.inc'
      include 'include/masti.inc'
 
      CHARACTER*10 ANS,TNAME*20
      INTEGER*4 NLEN,LAY,VAL,LEN,J,TLEN
      LOGICAL YESOK
      DOUBLE PRECISION NUM
      EXTERNAL NLEN,AEXPRN,YESOK
C
 5    CONTINUE
C
      CALL DPRMXP(436,ANS)
      IF ( NLEN(ANS) .EQ. 0 ) THEN
         CCMD='q'
         RETURN
      END IF
C
      CALL AEXPRN(ANS,NUM,*5)
      VAL=INT(NUM)
      IF ( VAL.LT.0.OR.VAL.GT. 255 ) THEN
         CALL DEPRNT(134)
         GOTO 5
      END IF
C
      LEN=NLEN(LNAME(VAL))
      IF ( LEN.GT.0)  THEN
         CALL DEPRNT(393)
         CALL CPRINT('"'//LNAME(VAL)(1:LEN)//'"')
         CALL DPRMXP(437,ANS)
         CALL GTCLRM(2)
         IF (.NOT.YESOK(ANS) )  GOTO 5
         LNAME(VAL)=' '
      END IF
C
 10   CONTINUE
C     ask for new name
      CALL DPRMXP(438,TNAME)
C
      CALL FOLDUP(TNAME)
      LEN=NLEN(TNAME)
      IF(LEN.EQ.0) THEN
          LNAME(VAL)=' '
          GOTO 5
      ENDIF
C
C     Check for a numeric layer name
      IF(ICHAR(TNAME(1:1)).LT.65) THEN
         CALL DEPRNT(59)
         GOTO 10
      ENDIF
C     if not blank line check against existing names
      IF ( LEN .GT. 0 ) THEN
         DO 15 J=0,255
            TLEN=NLEN(LNAME(J))
            IF ( TLEN.GT.0 ) THEN
               IF ( TNAME(1:LEN).EQ.LNAME(J)(1:TLEN) ) THEN
                  CALL DEPRNT(394)
                  GOTO 5
               END IF
            END IF
 15      CONTINUE
      END IF
C
      LNAME(VAL)=TNAME
C
      END
*
      SUBROUTINE ATTN00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the ATTACH NAME function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,ATTN01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the ATTACH NAME routine
      CALL ATTN01()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
*
      SUBROUTINE ATTN01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
C
C
      INTEGER TMEN,TCELL,I
C
      EXTERNAL GTMCLO,GTCLRM,MNLAP0,UNFLAG
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize ATTACH NAME menu
      CALL MNLAN0()
C     enter the ATTACH NAME routine
      CALL ATTN02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
*
      SUBROUTINE ATTN02()
C     ===================
C1    No arguments required.
C
      include 'include/menun.inc'
 
      INTEGER*4 TCELL,TMEN,C
      INTEGER I
      REAL X,Y
 
C     clear the error and prompt windows
      CALL CLRPEW
 
 10   CONTINUE
C     Read a cursor hit to select NAME option mode
      I=38
      CALL DCPRNT(I)
      CALL TCURS(C,X,Y)
C
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
         IF (CCMD.EQ.'L') THEN
C           NAME LAYER option
            CALL ATTL00()
C           ensure cell is low
            CALL GTMCLO(TMEN,TCELL)
         ELSE
C           unrecognized attach option
C           "option not yet available"
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
         IF (MEN.EQ.2) GOTO 99
      ELSE IF(MEN.EQ.2) THEN
          RETURN
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
      END
 
      SUBROUTINE ATTP00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the ATTACH PROPERTY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO
C
      TMEN=MEN
      TCELL=CELLN
C     enter the ATTACH PROPERTY routine
      CALL ATTP01()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE ATTP01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
C
C
      INTEGER TMEN,TCELL,I
C
      EXTERNAL GTMCLO,GTCLRM,MNLAP0,UNFLAG
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize ATTACH PROPERTY menu
      CALL MNLAP0()
C     enter the ATTACH PROPERTY routine
      CALL ATTP02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE ATTP02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine ATTP02 handles the attachment
C2    of properties to
C2    entities in the graphical database.
C
      include 'include/menun.inc'
      include 'include/props.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
C
      INTEGER*4 I,J
      INTEGER*2 PRPNT,MIPP,TENT,OLDLNK,TPRPNT
      INTEGER*2 MASPRP
      REAL X,Y
      LOGICAL OPTION,QUIT,OK
      CHARACTER*80 PNAME
      EXTERNAL ALSRCH,FINDET,ATTP10,DEPRNT,UNFLAG
      EXTERNAL FNDTOK,FNDPOS,ATTP03
C
C     enable searching for all entities
      CALL ALSRCH()
C     ensure grouped entities returned
      GSSTAT=4
      PROPOK=.FALSE.
C     goto default entry point by asking for
C     name of property to change
C     load token for "Name" option
      CALL FNDTOK(343,CCMD)
C     ensure "Name" is hilited
      MEN=3
      CALL FNDPOS(343,CELLN)
      CALL GTMCHI(MEN,CELLN)
      OPTION=.TRUE.
      QUIT=.FALSE.
      GOTO 21
C
 10   CONTINUE
C     print prompt for user
C     "Select an option from the MINOR menu"
C     find and flag an entity at hit point
      CALL FINDET(488,X,Y,MIPP,TENT,OPTION,QUIT)
C
 21   CONTINUE
C
      IF (QUIT) RETURN
      IF (.NOT.OPTION) THEN
C        go attach property to entity
         CALL ATTP03(PNAME,MASPRP,MIPP,OK)
C        ensure flag status is cleared
         CALL UNFLAG(.TRUE.)
C        If we get to here then property has been assigned
         GOTO 10
      END IF
C
      IF (MEN.EQ.3) THEN
C        go handle options
         CALL ATTP10(PNAME,MASPRP,OK)
         IF ( CCMD.EQ.'q') RETURN
         IF(PROPOK) THEN
             GOTO 10
         ELSE
             IF(.NOT.OK) RETURN
             GOTO 21
         ENDIF
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE
C       "Invalid area of screen"
         I=117
         CALL DEPRNT(I)
         GOTO 10
      END IF
C
 99   CONTINUE
C
C
      END
C
C
      SUBROUTINE ATTP03(PNAME,MASPRP,MIPP,OK)
C     =======================================
C
C1    vartype            C*(*)  I2   I2   L
C1    iostatus             I    I    I    O
C
      include 'include/nbuff.inc'
C
      INTEGER*2 PRPNT,MIPP,OLDLNK,TPRPNT,I2
      INTEGER*2 MASPRP
      LOGICAL OK
      CHARACTER*(*) PNAME
      EXTERNAL ATTP60,ATTP70
C
C     test for property link on the entity
      OLDLNK=IMBUFF(11)
      I2=0
      IF (OLDLNK.EQ.I2) THEN
C        must construct link,and attach property
         CALL ATTP60(MIPP,MASPRP,PRPNT,OK)
      ELSE
C        link already exists
C        attach to existing link
         CALL ATTP70(MIPP,MASPRP,PNAME,OLDLNK,OK)
      END IF
C
      END
C
C
      SUBROUTINE ATTP10(PNAME,PRPNT,OK)
C     =================================
C
C1    vartype           I2      I2  L
C1    iostatus          IO      IO  O
C
      include 'include/menun.inc'
      include 'include/props.inc'
C
      INTEGER*4 NLEN
      INTEGER*2 PRPNT
      LOGICAL OK
      CHARACTER*80 PNAME
      EXTERNAL ATTP11,GTMCLO
C
      IF (CCMD.EQ.'N') THEN
C        NAME selected
C        ask him for it
         CALL ATTP11(PNAME,PRPNT,OK)
C        reset caller menu cell
         CALL GTMCLO(MEN,CELLN)
      END IF
C
C
      END
C
C
      SUBROUTINE ATTP11(PNAME,PRPNT,OK)
C     =================================
C
C1    vartype           C*(*)   I2  L
C1    iostatus           IO     O   O
C
C2    Subroutine ATTP11 asks the user for the name of a property
C2    and validates his input.If valid property,then PRPNT returns
C2    the pointer to the property,and OK returned true.
C
      include 'include/props.inc'
C
      INTEGER*2 TPRPNT,PRPNT
      INTEGER*4 NLEN
      LOGICAL OK,FOUND
      CHARACTER*(*) PNAME
      EXTERNAL DPRMXP,NLEN,FOLDUP,DSPR01,DEPRNT,DRPR01
C
      OK=.FALSE.
      PRPNT=0
C     "Enter name of property:"
      CALL DPRMXP(281,PNAME)
      IF (NLEN(PNAME).EQ.0) THEN
         RETURN
      END IF
C     fold name to ,upper case
      CALL FOLDUP(PNAME)
C     go check for previous definition of same name
      CALL DSPR01(PNAME,TPRPNT,FOUND)
      IF (.NOT.FOUND) THEN
         PROPOK=.FALSE.
         OK =.TRUE.
C        does not exist
C        "Property of this name does not exist"
         CALL DEPRNT(116)
      ELSE
C        must read property data to buffers
         CALL DRPR01(TPRPNT,OK)
         IF (OK) THEN
C           property found
            PROPOK=.TRUE.
            PROPPP=TPRPNT
            PRPNT=TPRPNT
         END IF
      END IF
C
      END
C
C
C
      SUBROUTINE ATTP50(MASPRP,TMIP,PRPNT,OK)
C     =======================================
C
C1    vartype             I2    I2   I2   L
C1    iostatus            I     I    O    O
C
C2    Subroutine ATTP50 attaches a property
C2    pointed to by MASPRP,to the
C2    entity pointed to by TMIP,and
C     returns the property position occupied
C2    in PRPNT.Completion status in OK.
C
      include 'include/props.inc'
C
      INTEGER*4 I,DTYPE,ACTION,NLEN
      INTEGER*2 TMIP,PRPNT,TPRPNT,MASPRP,I2
      LOGICAL DATAOK,OK
      EXTERNAL DWPRC0,DWPRI0,DRPR01,ATTP51,NLEN
C
      DATAOK=.FALSE.
C     read the master property data to buffers
      CALL DRPR01(MASPRP,OK)
      IF (.NOT.OK) RETURN
C     set mip pointer in property index
C     to the related entity
      PRIBUF(3)=TMIP
C     change record type to attached property
      PRIBUF(1)=5
C     get data for the property
C     get data from user and place in buffer
      CALL ATTP51(DTYPE,ACTION,OK)
C
      IF(.NOT.OK) RETURN
      TPRPNT=NPRPOS
      DO 50 I=1,4
C        write text data to file
         IF (NLEN(PRCBUF(I)).GT.0) THEN
C           write each record to file
            I2=I+4
            CALL DWPRC0(TPRPNT,I2,PRCBUF(I),PRIBUF(I+4),OK)
         ELSE
            PRIBUF(I+4) = 0
         END IF
 50   CONTINUE
C     write control block to file
      CALL DWPRI0(PRPNT,OK)
C
      END
C
C
C
      SUBROUTINE ATTP51(DTYPE,ACTION,OK)
C     ==================================
C
C1    vartype            I4     I4    L
C1    iostatus           O      O     O
C
C2    Subroutine ATTP51 prompts the user for
C2    properties data,using control data contained
C2    in the current property buffers.The returned
C2    data is verified to the defined data type,
C2    and returned in the data buffer PRCBUF(3).
C
      include 'include/props.inc'
C
      INTEGER*4 DTYPE,ACTION,NLEN,INVAL,TPRIBF
      INTEGER*2 I2
      DOUBLE PRECISION DN
      REAL RVAL
      LOGICAL OK
      CHARACTER*80 PPTEXT
      EXTERNAL CPRMXP,CRUNCH,VRFINT,DEPRNT,RVALU,NLEN
      EXTERNAL DPRMX3
C
C     decode data type and action code
      DTYPE=PRIBUF(2)/256
      TPRIBF=PRIBUF(2)
      ACTION=MOD(TPRIBF,256)
C     prompt for the data
C     prompt normally comes from definition
 60   CONTINUE
      I2=0
      IF (PRCBI(1,2).GT.I2) THEN
C        use stored prompt
         CALL CPRMXP(PRCBUF(2),PPTEXT)
      ELSE
C        use default prompt from prompt file
         CALL DPRMX3(369,PRCBUF(1),PPTEXT)
      END IF
      IF (NLEN(PPTEXT).GT.0) THEN
         IF (DTYPE.EQ.2) THEN
C           integer data required
            CALL CRUNCH(PPTEXT)
            CALL VRFINT(PPTEXT,INVAL,OK)
            IF (.NOT.OK) THEN
C              tell the idiot to enter integer only
               CALL DEPRNT(286)
               GOTO 60
            END IF
         ELSE IF (DTYPE.EQ.4) THEN
C           real data required
            CALL CRUNCH(PPTEXT)
            CALL  AEXPRN(PPTEXT,DN,*65)
            RVAL = REAL(DN)
C           write in the pptext the real value
            WRITE(PPTEXT,'(F14.7)')RVAL
            CALL CRUNCH(PPTEXT)
            GOTO 66
C           ==== not a valid number
  65        CONTINUE
C            tell the idiot to enter real only
            CALL DEPRNT(71)
            GOTO 60
  66        CONTINUE
         END IF
      END IF
C     place data in buffer
      PRCBUF(3)=PPTEXT
      OK=.TRUE.
C
      END
C
      SUBROUTINE ATTP60(MIPP,MASPRP,PRPNT,OK)
C     =======================================
C
C1    vartype            I2    I2    I2   L
C1    iostatus           I     I     O    O
C
C2    Subroutine ATTP60 attaches the property pointed
C2    to by MASPRP to entity MIPP,assuming no property
C2    link exists for the entity.The index position
C2    of the link is returned in PRPNT.completion
C2    status is returned in OK.
C
      include 'include/props.inc'
      include 'include/nbuff.inc'
C
      INTEGER*2 MIPP,MASPRP,PRPNT,PRIPNT,I2
      LOGICAL OK
      EXTERNAL INPLNK,ATTP50,DWPL01,DIM501
C
C     must construct PROPERTY LINK structure
      CALL INPLNK()
C     set pointer to related entity
      PRLBUF(3,1)=MIPP
      PRLBUF(3,2)=MIPP
C     set record type for link
      PRLBUF(1,1)=2
C     set record type for property list
      PRLBUF(1,2)=4
C     store no records and props in link
      PRLBUF(2,1)=2*256+1
C     try attaching single property to entity
      CALL ATTP50(MASPRP,MIPP,PRIPNT,OK)
C     write property pointer into link list
      PRLBUF(5,2)=PRIPNT
C     now write link structure to file
      CALL DWPL01(MIPP,PRPNT)
C     modify MI entry for entity to point to property
      I2=11
      CALL DIM501(MIPP,I2,PRPNT,OK)
C
      END
C
C
      SUBROUTINE ATTP70(MIPP,MASPRP,PNAME,OLDLNK,OK)
C     ==============================================
C
C1    vartype             I2     I2   C*80  I2   L
C1    iostatus            I      I      I   IO   O
C
C2    Subroutine ATTP70 attaches property MASPRP to the
C2    entity MIPP using existing property link structure OLDLNK.
C2    property name passed in PNAME.
C2    If the link does not belong to the entity,then another
C2    link is created which does belong directly to the entity.
C2    A message is issued to the user if the property
C2    is already attached to the link.Completion status
C2    is returned in OK.
C
      include 'include/props.inc'
C
      INTEGER*4 NRECS,NPROPS,NR,NP,LP,PAVREC,PAVFLD,SPP,APBP,I,ST
      INTEGER*2 MIPP,OLDLNK,MASPRP,TPRPNT,I2
      LOGICAL OK,PAVAIL
      CHARACTER PNAME*80
      EXTERNAL DRPL01,DWPL10,DCPR05,DEPRNT,DWPRI5,ATTP50
      EXTERNAL DMPL01,DCPRNT
C
C     read the link data
      CALL DRPL01(OLDLNK,NRECS,NPROPS,OK)
C     assume data is ok
C     check for matching MI pointer
      IF (PRLBUF(3,1).NE.MIPP) THEN
C        link does not actually belong to this entity
C        copy the link structure,and properties to a new
C        link which does belong to this entity
         CALL DWPL10(MIPP,OLDLNK,OK)
      END IF
C     start to process the link list
C     point to first list record in buffer
      NR=2
      NP=0
      PAVAIL=.FALSE.
 100  CONTINUE
C     process the current record
      IF (NR.LE.NRECS) THEN
C        process the list
C        initialize pointer into record
         LP=5
 110     CONTINUE
         IF (NP.LT.NPROPS) THEN
            IF (LP.LE.8) THEN
C              read pointer from list
               TPRPNT=PRLBUF(LP,NR)
               IF (TPRPNT.GT.0) THEN
C                 process the property
C                 test for matching name
                  I2=5
                  CALL DCPR05(TPRPNT,I2,PNAME,ST)
                  IF (ST.EQ.0) THEN
C                    property of this name already attached
                     CALL DEPRNT(72)
                     GOTO 299
                  END IF
C                 update count of props searched
                  NP=NP+1
               ELSE
C                 there is a null pointer space available
                  IF (.NOT.PAVAIL) THEN
C                    do not already have a space
C                    so store this one,record number,field number
                     PAVREC=NR
                     PAVFLD=LP
                     PAVAIL=.TRUE.
                  END IF
               END IF
C              update pointer into list
               LP=LP+1
C              go try next in list
               GOTO 110
            ELSE
C              ran off end of list
C              go try next record
               NR=NR+1
               GOTO 100
            END IF
         END IF
C        all props have now been tested
      END IF
C     all records have been tested
C     need to append property to current list
      IF (.NOT.PAVAIL) THEN
C        must find space,could be some at end of list
         SPP=MOD(NPROPS,4)
         IF (SPP.GT.0) THEN
C           if SPP gt 0 must be space in last record
C           save record and field pointers.
            PAVREC=NRECS
            PAVFLD=5+SPP
         ELSE
C           must append list record to link
C           point to next available buffer record
            APBP=NRECS+1
C           clear the buffer record
            DO 50 I=1,8
               PRLBUF(I,APBP)=0
 50         CONTINUE
C           set record type to list
            PRLBUF(1,APBP)=4
C           set MIP for list
            PRLBUF(3,APBP)=MIPP
C           write record to file
            CALL DWPRI5(PRLBUF(1,APBP),TPRPNT,OK)
C           new record added to property link
C           update continuation pointer from previous record
            PRLBUF(4,NRECS)=TPRPNT
C           update number of records
            NRECS=NRECS+1
            PRLBUF(2,1)=NRECS*256+NPROPS
C           set pointers to entry position of property
            PAVFLD=5
            PAVREC=APBP
         END IF
      END IF
C     space now available for property
C     attach property to entity
      CALL ATTP50(MASPRP,MIPP,TPRPNT,OK)
C     now update the link buffer
      PRLBUF(PAVFLD,PAVREC)=TPRPNT
C     update no recs and props
      NPROPS=NPROPS+1
      PRLBUF(2,1)=NRECS*256+NPROPS
C     write modified link back to file
      CALL DMPL01(MIPP,OLDLNK)
C     confirm property attached
      CALL DCPRNT(275)
C
 299  CONTINUE
C
      END
*
      SUBROUTINE MAJAT1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the ATTACH mode
C2    of operation is selected from the master menu.
C2    controls operation of the ATTACH function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
      include 'include/props.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR,I,J
C
      REAL X,Y
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL MNIATT,ATTP00,CPRINT,GTCLRM
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,UNFLAG,FNDTOK,FNDPOS
C
C     Now activate the ATTACH major option menu
      CALL MNIATT()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     attach. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     goto default entry point by asking for
C     name of property to attach
C     load token for "Property" option
C      CALL FNDTOK(320,CCMD)
C     ensure "Property" is hilited
C      MEN=2
C      CALL FNDPOS(320,CELLN)
C      CALL GTMCHI(MEN,CELLN)
C      GOTO 20
C
 10   CONTINUE
C     tell him what to do
C     "select an option from the minor menu"
      I=284
      CALL DCPRNT(I)
C     Read a cursor hit to select ATTACH mode
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
C           ATTACH PROPERTY option
C           ensure grouped entities are returned
C           as groups
            GSSTAT=4
            CALL ATTP00()
         ELSE IF (CCMD.EQ.'N') THEN
C           attach name to layer option
C           ensure grouped entities are returned
C           as groups
            CALL ATTN00()
         ELSE
C           unrecognized attach option
C           "option not yet available"
            I=8
            CALL DEPRNT(I)
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
C        "No MAJOR menu option has been selected"
         I=24
         CALL DEPRNT(I)
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
C
      RETURN
      END
C
C
      SUBROUTINE MNIATT()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the ATTACH major options.
C2
C
      EXTERNAL GTCLRM,GTDMEN,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the ATTACH major options.
C     Hilite the option header
      CALL GTDMHD(31,2)
C
C     Load the options for create
C2    P is the token for ATTACH PROPERTY
      CALL GTDMEN(320,2)
 
C2    N is the token for name
      CALL GTDMEN(321,2)
C
      END
C
C
      SUBROUTINE MNLAN0()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLAP0 loads the ATTACH PROPERTY
C2    portion of the option menu no3.
C2
C
      EXTERNAL GTDMEN
C
C     load the name option
C     "Layer"
      CALL GTDMEN(325,3)
C
      END
C
C
C
      SUBROUTINE MNLAP0()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLAP0 loads the ATTACH PROPERTY
C2    portion of the option menu no3.
C2
C
      EXTERNAL GTDMEN
C
C     load the name option
C     "Name"
      CALL GTDMEN(343,3)
C
      END
 
