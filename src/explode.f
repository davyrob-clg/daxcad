C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 explode.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE EXP066(TMIP)
C     SUBROUTINE EXPG02(FN)
C     SUBROUTINE EXPG03(FN,OK)
C     SUBROUTINE EXPGCS(FN)
C     SUBROUTINE EXPWRT(ENT,MIPP)
C     SUBROUTINE GRPEXP(FN)
C     SUBROUTINE MAJEX1()
C     SUBROUTINE MNIEXP()
C     SUBROUTINE MNLEX1()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE EXP066(TMIP)
C     =======================
C
C1    vartyp             I2
C1    iostatus           I
C
C2    Subroutine EXP066 explodes the component
C2    or symbol instance pointed to by TMIP,into individual
C2    database entities,and deletes the instance
C2    from the database.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include 'include/compd.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 TMIP,TTMIP,ENT,ITHIS,INEXT,PDP,MIPP,I2
      INTEGER*2 TDFP,DFPT
      INTEGER*4 I,J
      LOGICAL OK,L0,L1,L2,DELETE
      REAL INSTFM(3,3),MTMP(3,3),TX1,TY1
      EXTERNAL DER566,DRR950,EXPWRT,NEWXY,DBR500,DBW501,DBW500
      EXTERNAL MV0003
C
C     read the Instance header
      CALL DER566(TMIP,INSTFM,OK)
C     M now contains instance transform
C     find relation header from instance data
      RELP=IMBUFF(10)
C     if null relation pointer,then unresolved instance
      I2=0
      IF (RELP.LE.I2) then
C        write to diag
         WRITE(UNIT=10,FMT=*)
     +   '*** Unresolved COMPONENT instance [EXP066]',CBUFF
         GOTO 20
      END IF
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
C     test for valid component or symbol relation
C      WRITE(10,*) '[EXP066]',TMIP,RLBUFF(1)
      IF ( RLBUFF(1).NE.COMPM.AND.RLBUFF(1).NE.SYMBM ) GOTO 20
C
C     ensure text scaled proportionaly during display
C     save opflags
      L0=OPFLAG(1)
      L1=OPFLAG(6)
      L2=OPFLAG(7)
C     set opflags to ensure scale done properly
      OPFLAG(1)=.TRUE.
      OPFLAG(6)=.TRUE.
      OPFLAG(7)=.TRUE.
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
      NENTS=RLBUFF(5)
C
      DO 15 J=1,NRRECS
         NXTRLR=RLBUFF(2)
C        read the list of entities
         CALL DRR950(NXTRLR,OK)
         DO 10 I=4,10
            TTMIP=RLBUFF(I)
            IF (TTMIP.GT.0) THEN
C              read the entity and store it discretely
               CALL ALLRD(TTMIP,ENT,MTMP,DELETE)
               TDFP=LDFILE(CVPN)
               IF (ENT.EQ.COMPI.OR.ENT.EQ.SYMBI) THEN
C                 transform and store nested instance
C                 clear parent pointer
                  I2=0
                  IMBUFF(8)=I2
C                 transform the instance
                  CALL MV0066(INSTFM,MTMP)
C                 reset staus to discrete entity
                  IMBUFF(1)=10
C                 write to database
                  CALL DEW566(MTMP,MIPP,OK)
               ELSE IF (ENT.EQ.SPLINE) THEN
C                 transform and store spline data
C                 read the first header record again
                  CALL DER500(TTMIP,OK)
C                 point to continuation
                  INEXT=IDBUFF(3)
C                 write first header record
C                 reset status to normal
                  I2=10
                  IMBUFF(1)=I2
C                 clear parent pointer
                  I2=0
                  IMBUFF(8)=I2
C                 clear relation pointer
                  I2=0
                  IMBUFF(10)=I2
C                 must maintain connectivity to control points
                  CALL DEW501(MIPP,OK)
C                 MIPP contains MI pointer of the entity
C                 second header record to copy as well
                  CALL DBR500(INEXT,OK)
C                 Save continuation pointer
                  INEXT=IDBUFF(3)
C                 write out second header record
C                 set MI pointer
                  IDBUFF(2)=MIPP
                  CALL DBW501(PDP,OK)
C                 INEXT points to first control point
 82               CONTINUE
                  IF (INEXT.NE.0) THEN
C                    read the next control point
                     CALL DBR500(INEXT,OK)
                     ITHIS=INEXT
                     INEXT=IDBUFF(3)
C                    transform the control point
                     CALL NEWXY(RDBUFF(1),RDBUFF(2),TX1,TY1,INSTFM)
                     RDBUFF(1)=TX1
                     RDBUFF(2)=TY1
                  END IF
C                 movement of control point now complete
C                 write out copy transformed to next free space
C                 set MI pointer
                  IDBUFF(2)=MIPP
                  IF (INEXT.EQ.0) THEN
C                    last control point, no connectivity
                     CALL DBW500(PDP,OK)
                  ELSE
C                    connected control point
                     CALL DBW501(PDP,OK)
C                    try next control point
                     GOTO 82
                  END IF
C                 all control points have been transformed
               CALL CHGE40(MIPP)
               ELSE IF (ENT.EQ.HATCH) THEN
C                 transform and store hatch data
C                 read the containment box record again
                  CALL DER500(TTMIP,OK)
C                 point to continuation
                  INEXT=IDBUFF(3)
C                 write containment box record
C                 reset status to normal
                  IMBUFF(1)=10
C                 clear parent pointer
                  IMBUFF(8)=0
C                 clear relation pointer
                  IMBUFF(10)=0
C                 transform the hatch containment box
                  CALL MV0003(RDBUFF,INSTFM)
C                 must maintain connectivity to control points
                  CALL DEW501(MIPP,OK)
C                 MIPP contains MI pointer of the entity
C                 INEXT points to first hatch line
 80               CONTINUE
                  IF (INEXT.NE.0) THEN
C                    read the next control point
                     CALL DBR500(INEXT,OK)
                     ITHIS=INEXT
                     INEXT=IDBUFF(3)
C                    transform the hatch line
                     CALL MV0003(RDBUFF,INSTFM)
                  END IF
C                 movement of hatch line now complete
C                 write out copy transformed to next free space
C                 set MI pointer
                  IDBUFF(2)=MIPP
                  IF (INEXT.EQ.0) THEN
C                    last hatch line, no connectivity
                     CALL DBW500(PDP,OK)
                  ELSE
C                    connected hatch line
                     CALL DBW501(PDP,OK)
C                    try next control point
                     GOTO 80
                  END IF
C                 all hatch lines have been transformed
                  CALL CHGE40(MIPP)
               ELSE
C                 transform and store the entity
C                 transform the entity
                  CALL ALLTFM(ENT,INSTFM)
C                 store the transformed entity
                  CALL EXPWRT(ENT,TTMIP)
                  MIPP=TTMIP
               END IF
C              read the new entity again
               CALL DIR500(MIPP,OK)
C              if an entity has been added but is hidden then withdraw it
 
C      WRITE(10,*) '[EXPLODE] TDPF LDFILE IMBUFF(4) ',
C     +          TDFP,LDFILE(CVPN), IMBUFF(4)
 
               IF(LDFILE(CVPN).GT.TDFP.AND..NOT.VLAYER(IMBUFF(4)))
     +             THEN
                    DFPT=LDFILE(CVPN)-1
                    CALL WDISPF(DFPT,IMBUFF(2),-MIPP,OK)
               ENDIF
            END IF
 10      CONTINUE
 15   CONTINUE
C
C     recover opflags
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
C
C     delete instance
      CALL DIR500(TMIP,OK)
C     set delete status
      IMBUFF(1)=100
      CALL DIM500(TMIP,OK)
C
 20   CONTINUE
CD      WRITE(UNIT=10,FMT=*)'[EXP066] end of Component'
C
      END
C
C
      SUBROUTINE EXPG02(FN)
C     =====================
C
C2    Subroutine EXPG02 is the main working routine
C2    for the EXPLODE GROUP function.Uses the normal
C2    window search routines for entity selection.
C2    FN passes the function number to indicate the
C2    entity type to explode.
C2       FN=1 GROUP
C2       FN=2 COMPONENT INSTANCE
C2       FN=3 SYMBOL INSTANCE
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      REAL X,Y
C
      INTEGER*2 MIPP,ENT
      INTEGER*4 FN,PRMPTN
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL EXPG03,FINDET,ALSRCH,NOSRCH,ADSRCH
C
      IF (FN.EQ.1) THEN
C        enable searching for GROUPS only
CDHR     this change will enable group searches only now
         CALL ADSRCH(GROUP)
C         CALL ALSRCH()
C        set group search to return complete groups
         GSSTAT=2
C        set pointer to prompt
C       "Pick group to be exploded"
         PRMPTN=140
      ELSE IF (FN.EQ.2) THEN
C        enable searching for COMPONENTS
         CALL NOSRCH()
         CALL ADSRCH(COMPI)
C        set group status to disallow grouped entity
         GSSTAT=3
C        set pointer to prompt
C       "Pick component to be exploded"
         PRMPTN=387
      ELSE IF (FN.EQ.3) THEN
C        enable searching for SYMBOLS
         CALL NOSRCH()
         CALL ADSRCH(SYMBI)
C        set group status to disallow grouped entity
         GSSTAT=3
C        set pointer to prompt
C       "Pick group to be exploded"
         PRMPTN=391
      END IF
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(PRMPTN,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) RETURN
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL EXPG03(FN,OK)
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
      SUBROUTINE EXPG03(FN,OK)
C     ========================
C
C2    Subroutine EXPG03 is the main workiing routine
C2    for the explosion of GROUPS,and COMPONENTS
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      INTEGER*4 TMEN,TCELL,FN
      LOGICAL OK
      INTRINSIC CHAR
C
      EXTERNAL MNLPTS,MNUPTS,WINDOW,GTMCLO
      EXTERNAL ZSFLAG,GRPEXP,UNFLAG
C
 25   CONTINUE
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processinig here
 20   CONTINUE
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        load the point modes to menu
         CALL MNLPTS()
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
C        unload the point modes
         CALL MNUPTS()
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
C           set flag to indicate completion
            IF (FN.EQ.1) THEN
C              tell the user
C              "Group explosion in progress"
               CALL DCPRNT(141)
C              go explode the group
               CALL GRPEXP(FN)
            ELSE IF (FN.EQ.2) THEN
C              tell the user
C              "Group explosion in progress"
               CALL DCPRNT(141)
               CALL GRPEXP(FN)
            ELSE IF (FN.EQ.3) THEN
C              tell the user
C              "Group explosion in progress"
               CALL DCPRNT(141)
               CALL GRPEXP(FN)
            END IF
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
C     ensure caller no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE EXPGCS(FN)
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
C
C
      INTEGER*4 TMEN,TCELL,FN
C
      EXTERNAL GTMCLO,GTCLRM,MNLEX1,UNFLAG,EXPG02
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize EXPLODE COMPONENT menu
      CALL MNLEX1()
C     enter the EXPLODE COMPONENT routine
      CALL EXPG02(FN)
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
      SUBROUTINE EXPWRT(ENT,MIPP)
C     ===========================
C
C1    vartype           I2   I2
C1    iostatus          I    O
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
C
C2    Subroutine EXPWRT writes the entity
C2    data currently in the buffers to the database
C2    Ensures entities are discrete entries with
C2    no parent or relation pointers.
C2    The MI used is returned in MIPP
C
      INTEGER*2 ENT,MIPP,STATUS
      LOGICAL OK
C
      EXTERNAL DEW500,DEWDIM
C
C     clear parent pointer
      IMBUFF(8)=0
C     clear relation pointer
      IMBUFF(10)=0
C     set default status
      STATUS=10
      IMBUFF(1)=STATUS
      IF (ENT.EQ.MARKER) THEN
         CALL DEW500(MIPP,OK)
      ELSE IF (ENT.EQ.LINE) THEN
         CALL DEW500(MIPP,OK)
      ELSE IF (ENT.EQ.ARC) THEN
         CALL DEW500(MIPP,OK)
      ELSE IF (ENT.EQ.TEXT) THEN
         CALL DEW500(MIPP,OK)
      ELSE IF (ENT.EQ.CENLIN) THEN
         CALL DEW500(MIPP,OK)
      ELSE IF (ENT.EQ.LDIMN) THEN
         CALL DEWDIM(MIPP,LDIMN,OK)
      ELSE IF (ENT.EQ.RDIMN) THEN
         CALL DEWDIM(MIPP,RDIMN,OK)
      ELSE IF (ENT.EQ.DDIMN) THEN
         CALL DEWDIM(MIPP,DDIMN,OK)
      ELSE IF (ENT.EQ.ADIMN) THEN
         CALL DEWDIM(MIPP,ADIMN,OK)
      ELSE IF (ENT.EQ.GLABEL) THEN
         CALL DEWDIM(MIPP,GLABEL,OK)
      END IF
C
      END
C
C
CC
C
      SUBROUTINE GRPEXP(FN)
C     =====================
C
C2    subroutine GRPEXP explodes all grouped entities
C2    whose MI pointers are stored in the scratch
C2    workfile attached to unit SWINDU.The group headers
C2    are delete from the MI file,and the entity MI entries
C2    are reset to normal single entity status.
C2    The entity counter NDATA is reset to zero
C2    on exit from this routine.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/swind.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
C
C
      INTEGER*4 I,FN
      INTEGER*2 P,D1,D2,LPMIP,PMIP
      REAL      BX,BY
C
      LOGICAL OK,GRP
      EXTERNAL DIR500,ALLDRW,UNFLAG,DIM500,RSCRF,PENDRW
      EXTERNAL EXP066
C
      IF (NDATA.EQ.0) RETURN
C
      CALL UNFLAG(.FALSE.)
C
      LPMIP=-1
C
      DO 10 I=1,NDATA,1
C
C        read from workfile
         CALL RSCRF(I,P,BX,BY,D1,D2)
C
         IF (FN.EQ.1) THEN
C           explode group
            CALL DIR500(P,OK)
C           test for grouped status
            IF (IMBUFF(1).EQ.GROUP) THEN
C              get Parent MIP
               PMIP=IMBUFF(8)
               GRP=.TRUE.
            ELSE
               GRP=.FALSE.
            END IF
C           Set status to normal
            IMBUFF(1)=10
C           reset parent MI pointer
            IMBUFF(8)=0
C           write back to storage
            CALL DIM500(P,OK)
C
C           must delete header for grouped entities
C           only need to do it once for each group
            IF (GRP.AND.(PMIP.NE.LPMIP)) THEN
C              read MI of Parent
               CALL DIR500(PMIP,OK)
C              Set status to delete
               IMBUFF(1)=100
C              write back to storage
               CALL DIM500(PMIP,OK)
               LPMIP=PMIP
            END IF
C
         ELSE IF (FN.EQ.2) THEN
C           explode component
            CALL EXP066(P)
         ELSE IF (FN.EQ.3) THEN
C           explode symbol
            CALL EXP066(P)
         END IF
 10   CONTINUE
C
C     ensure pen back to draw colour
      CALL PENDRW()
C
C     reset entity counter to zero before return.
      NDATA=0
      VNDATA=1
C
      END
C
C
      SUBROUTINE MAJEX1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the EXPLODE mode
C2    of operation is selected from the master menu.
C2    controls operation of the EXPLODE function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER C,TMEN,TCELL
      REAL X,Y
C
      EXTERNAL MNIEXP,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW
C
C     Now activate the EXPLODE major option menu
      CALL MNIEXP()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     initialize number of copies to ONE
      NNCOPY=1
C
 10   CONTINUE
C     tell him what to do
      CALL DCPRNT(284)
C     Read a cursor hit to select EXPLODE mode
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
         IF (CCMD.EQ.'G') THEN
C           EXPLODE GROUP option
C           ensure grouped entities are returned
            GSSTAT=2
            CALL EXPGCS(1)
         ELSE IF (CCMD.EQ.'C') THEN
C           EXPLODE COMPONENT option
            CALL EXPGCS(2)
         ELSE IF (CCMD.EQ.'S') THEN
C           EXPLODE SYMBOL option
            CALL EXPGCS(3)
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
      SUBROUTINE MNIEXP()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the EXPLODE major options.
C2
C
      EXTERNAL GTDMEN,GTDMHD,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the EXPLODE major options.
C     Hilite the option header
C      CALL GTPMEN('EXPLODE',' ',2,1)
      CALL GTDMHD(24,2)
C
C2    G is the token for EXPLODE GROUP
      CALL GTDMEN(271,2)
C2    C is the token for EXPLODE COMPONENT
      CALL GTDMEN(273,2)
C2    S is the token for EXPLODE SYMBOL
      CALL GTDMEN(274,2)
      END
C
C
      SUBROUTINE MNLEX1()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLEX1 loads the AREA
C2    and CANCEL,and ACCEPT portions of the option
C2    menu no3.
C2    Intended for use in EXPLODE
C2
C2    Tokens used here are W e CHAR(149) and CHAR(150).
C2
      CHARACTER CHAR
C
      INTRINSIC CHAR
C
      EXTERNAL GTPMEN
C
C2    W is the token for INSIDE AREA.
      CALL GTDMEN(210,3)
C     CHAR(149) is the token for CANCEL.
      CALL GTDMEN(146,3)
C2    CHAR(150) is token for ACCEPT.
      CALL GTDMEN(13,3)
C2
      END
C
C
