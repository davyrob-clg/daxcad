C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insert.f  
C
      SUBROUTINE CCOLOR()
C     ===================
C
C2    Subroutine CCOLOR changes the selected draw colour
C2    and updates the menu cell showing the colour index.
C
      include 'include/menun.inc'
      include 'include/style.inc'
      include 'include/daxcolor.inc'
      include 'include/gtxt2.inc'
      include 'include/menpop.inc'
      include 'include/vntable.inc' 
C
      LOGICAL OK
      INTEGER*4 C,MNCODE
      INTEGER*2 TCOL
C
      MNCODE = 6
C     toggle colour number
C     limit of 16 colours for now
      CALL MENPOP(MNCODE,OK)
      IF(.NOT.OK) THEN
          CALL GTMCLO(MEN,CELLN)
          RETURN
      ENDIF
C     get colour from token values 
      TCOL=ICHAR(CCMD)-64
C     avoid zero colour index
      IF (TCOL.EQ.0) TCOL=1
      C=TCOL
      COLOUR=TCOL
C     update the colour display cell.
      TXCLR = VNOUN(413+TCOL)
C     set current draw colour and database default colour
      CALL SETDRW(TCOL)
      COLPEN=TCOL
      COLDRW=TCOL
C
C
      END
C
C--------------------------------------------------------
C
C
C
      SUBROUTINE INSA00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT ARC function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNIARC,INSA01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     initialize INSERT ARC option menu
      CALL MNIARC()
C     enter the INSERT ARC routine
      CALL INSA01()
C     Block the points menu again
      CALL MNUPTS()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSATT(TOKEN)
C     =====================
C1                      I
C1                      C*1
C
C2    Subroutine INSATT get a token and calls
C2    the correct change attribute functions
C2    used in INSERT etc
C
      include  'include/style.inc'
      include  'include/gtxt2.inc'
      CHARACTER*1 TOKEN
      EXTERNAL GTDMWT, CFONT, CCOLOR
C     ****************************
C       Change line attributes.
C     ****************************
      IF (TOKEN.EQ.'f') THEN
C        *************************************
C         LINE FONTING (Style)
C        *************************************
         CALL CFONT()
         GTMULT = .TRUE.
         CALL GTDMWT(407,3,TXFONT)
      ELSEIF(TOKEN .EQ. '=') THEN
C        *************************************
C         LINE FONTING (Thickness)
C        *************************************
         CALL CFONT()
         GTMULT = .TRUE.
         CALL GTDMWT(409,3,TXTHK)
      ELSEIF(TOKEN.EQ.'k') THEN
C        *************************************
C        LINE COLOUR
C        *************************************
         CALL CCOLOR()
         GTMULT = .TRUE.
         CALL  GTDMWT(399,3,TXCLR)
      ENDIF
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSCNL(FMIPOS,ENTYP,REMV,FIRST,OK)
C     =============================================
C1    IOTYPE              I2    I2    I4    L    L
C1    IOSTAT              I     I     I     I    O
C
C2    here goes on what I think this routine does (MM 20/07/88)
C2    This routine is used to cancel an entity from the database
C2    It assumes that this entity is the last entity to be inserted
C2    (so watch out if it not you are likely to find yourself
C2    with a drawing with any entity after the one to be delete being
C2    being lost)
C2    So far I think it is only used by line,arc,text,dimensions and now marker
C2    NOTE:- REMV does not seam to be used so your guess is as good as mine
C@           but I think it was going to be the number to remove
C2
C
C2    Subroutine INSCNL does God only knows what!!!
C2    Using variables which may as well have been
C2    defined in hell!!!
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
CSUN:APOLLO
      include 'include/viewport.inc'
CSUN:APOLLO
C
      REAL M1(3,3)
      INTEGER*2 FMIPOS,NMPOS,ENTYPE,TMIP1,ENTYP,PDBUF,TXBUF,RLBUF
      INTEGER*2 ENT,TMIP,DFP1,DFINDX
      INTEGER*4 REMV
      LOGICAL FIRST,OK
      EXTERNAL PENERS,PENDRW
C
      IF ( .NOT.FIRST ) THEN
         TMIP1=NMIPOS-1
         CALL ALLRD(TMIP1,ENTYPE,M1,OK)
         PDBUF=IMBUFF(7)
         TXBUF=IMBUFF(9)
         RLBUF=IMBUFF(10)
         IF (ENTYPE.EQ.ENTYP.OR.
     +      (ENTYP.EQ.LDIMN.AND.ENTYPE.EQ.RDIMN).OR.
     1      (ENTYP.EQ.LDIMN.AND.ENTYPE.EQ.DDIMN).OR.
     2      (ENTYP.EQ.LDIMN.AND.ENTYPE.EQ.ADIMN).OR.
     3      (ENTYP.EQ.LDIMN.AND.ENTYPE.EQ.GLABEL)   ) THEN
            VPCAN = .TRUE.
            CALL PENERS()
            CALL ALLDRW(ENTYPE,TMIP1)
            CALL PENDRW()
            VPCAN = .FALSE.
            NMIPOS=NMIPOS-1
            IF ( PDBUF.NE.0 ) NPDPOS=PDBUF
            IF ( TXBUF.NE.0 ) NTXPOS=TXBUF
            IF ( RLBUF.NE.0 ) NRLPOS=RLBUF
C           decrement only if visable or inside a view
            DFP1 = LDFILE(CVPN)-1
C           find out if we can still see the ent to be cancelled
            CALL RDISPF(DFP1,ENT,TMIP,OK)
            IF(TMIP.EQ.TMIP1) THEN
C              Is it ent on the end of the disp' file, where we put it
               LDFILE(CVPN)=LDFILE(CVPN)-1
            ELSE
C              A zoom or pan might have moved it, go looking.
               DFINDX=1
C
 10            CONTINUE
               CALL RDISPF(DFINDX,ENT,TMIP,OK)
               IF (TMIP.EQ.TMIP1) THEN
C                  GOTCHA! Mark it as deleted.
                   MIP = 0 - MIP
                   CALL WDISPF(DFINDX,ENT,MIP,OK)
               ELSE
C                  NO? Oh well, try the next.
                   DFINDX = DFINDX + 1
                   IF (DFINDX.LT.DFP1) GOTO 10
               ENDIF
C
            ENDIF
C           Subtract one entity from layer monitor
            CALL DELMON(IMBUFF(4),.TRUE.)
            FIRST=.NOT.(NMIPOS.GT.FMIPOS)
            OK=.TRUE.
         ELSE
            CALL DEPRNT(33)
            OK=.FALSE.
         END IF
         CALL GTMCLO(MEN,CELLN)
      ELSE
         IF ( ENTYP .EQ. LINE ) THEN
            CALL DEPRNT(193)
         ELSE IF ( ENTYP .EQ. ARC ) THEN
            CALL DEPRNT(190)
         ELSE IF ( ENTYP .EQ. TEXT ) THEN
            CALL DEPRNT(302)
         ELSE
            CALL DEPRNT(33)
         END IF
      END IF
C
      CALL GTMCLO(MEN,CELLN)
      OK=.FALSE.
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INSD00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT CURVE function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNICUR,INSD01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     initialize INSERT CURVE option menu
      CALL MNICUR()
C     enter the INSERT CURVE routine
      CALL INSD01()
C     Block the points menu again
      CALL MNUPTS()
C     ensure insert option for curve is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSF00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT FILLET option
C
      include 'include/menun.inc'
      include 'include/masti.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL MNIFIL,INS0F5,GTMCLO,CLRPEW,GTMCWI,MNICOL,MNISTD
C
      TMEN=MEN
      TCELL=CELLN
C     ensure cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C     initialize INSERT FILLET option menu
      CALL MNIFIL()
C     enter style option
      CALL MNISTD()
C
      CALL INS0F5()
C     ensure FILLET option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSH00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT HATCH option
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNIHAT,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     initialize hatching option menu
      CALL MNIHAT
C     show the font number in the cell
      CALL MNISTD()
C
C     go perform the hatch ops
      CALL HATCHB()
C     ensure insert option for hatch is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSL00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT LINE option
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/lfont.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNILIN,INS0L3,GTMCLO,CLRPEW,GTMCWI
C
      TMEN=MEN
      TCELL=CELLN
C     initialize INSERT LINE option menu
      CALL MNILIN
C     must show current line font
C     write the current font number into the cell
      CALL MNISTD()
C     insert line routine.
      CALL INS0L3()
C     Block the points menu again
      CALL MNUPTS()
C     ensure LINE option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INST00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT TEXT function
C
      include  'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL MNTIXT,GTMCLO
C
      EXTERNAL CLRPEW,INST01,MNISTD
C
C     save the caller menu and cell
      TMEN=MEN
      TCELL=CELLN
C     initialize INSERT TEXT option menu
      CALL MNITXT()
C SPB 101194 - Add bits to do thickness ...
      CALL MNITXTTHK()
C     enter the INSERT TEXT routine
      CALL INST01()
C     Block the points menu again
      CALL MNUPTS()
C     ensure insert option for text is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MAJIN1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the INSERT mode
C2    of operation is selected from the master menu.
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/movdat.inc'
C
      INTEGER C,TMEN,TCELL
C
      REAL X,Y
C
      EXTERNAL MNIINS,CLRPEW,GTHFMC,INST00,INSP00,INSC00,GTCLRM
      EXTERNAL INSF00,INSA00,INSH00,INSL00
      EXTERNAL TCURS,GTMCLO,GTMCHI
C
C     Clear major and minor menus, and enter the
C     major INSERT options.
      CALL MNIINS()
C
C     reset scale values
      REFDAT(1,3)=1.0
      REFDAT(2,3)=0.0
C     clear the error and prompt windows
      CALL CLRPEW
C     Making single line the default insert text
      MEN=2
C     'L' is the token used by insert line
      CCMD='L'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select input mode
      CALL TCURS(C,X,Y)
C
C     save pointers to menu and cell which was hit
 20   CONTINUE
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
C        major INSERT option selected
         IF (CCMD.EQ.'A') THEN
C           insert arc option
            CALL INSA00()
         ELSE IF (CCMD.EQ.'D') THEN
C           insert CURVE option
            CALL INSD00()
         ELSE IF (CCMD.EQ.'H') THEN
C           insert hatch option
C           ensure grouped entities are returned
C           as individual entities for the hatch boundary
            GSSTAT=1
            CALL INSH00()
         ELSE IF (CCMD.EQ.'T') THEN
C           insert text option
            CALL INST00()
         ELSE IF (CCMD.EQ.'L') THEN
C           insert line option
            CALL INSL00()
         ELSE IF (CCMD.EQ.'F') THEN
C           insert fillet option
            CALL INSF00()
         ELSE IF (CCMD.EQ.'P') THEN
C           insert part option
            CALL INSP00()
         ELSE IF (CCMD.EQ.'C') THEN
C           insert component option
            CALL INSC00()
         ELSE IF (CCMD.EQ.'S') THEN
C           insert symbol option
            CALL INSS00()
         ELSE IF (CCMD.EQ.'M') THEN
C           insert marker option.
            CALL INSM00()
         ELSE IF (CCMD.EQ.'E') THEN
C           insert elipse option.
            CALL INSE00()
         ELSE IF (CCMD.EQ.'c') THEN
C           insert Chamfer option
            CALL INSCF0()
         ELSE IF (CCMD.EQ.'l') THEN
C           insert center line option
            CALL INSCEN()
         ELSE
C           UNRECOGNISED option
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
         IF (MEN.EQ.2) THEN
               CALL GTHFMC(MEN, CCMD ,CELLN)
            GOTO 20
         ENDIF
         GOTO 10
      ELSE
         CALL DEPRNT(176)
      END IF
      GOTO 10
C
 99   CONTINUE
C
 
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MNICOL()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNICOL initializes and loads
C2    the sub-menu options for  colour
C2    control of entities
C
      include  'include/style.inc'
      include  'include/gtxt2.inc'
      EXTERNAL GTDMWT
C
C     load colour control cell
C     k is the token for colour
      GTMULT = .TRUE.
      CALL GTDMWT(399,3,TXCLR)
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MNIINS
C     =================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major INSERT options.
C2
      EXTERNAL GTPMEN,GTCLRM,MNLINS,GTMCHI
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the INSERT major option.
      CALL GTDMHD(16,2)
C
C     Load the entities types into major option menu.
      CALL MNLINS()
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MNISTD()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNISTD initializes and loads
C2    the sub-menu options for style
C2    control of entities
C
      include  'include/lfont.inc'
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/style.inc'
      include  'include/daxcolor.inc'
      include  'include/gtxt2.inc'
C
C     load the colour cell
      CALL MNICOL()
C         
C     f is the token for font  
      GTMULT = .TRUE.
      CALL GTDMWT(407,3,TXFONT)
C
C
C     = is the token for thickness
      GTMULT = .TRUE.
      CALL GTDMWT(409,3,TXTHK)
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MNITXTTHK()
C     ======================
C     No arguments required.
C SPB 101194
C     This routine was plagerised from MNISTD
C
C     Subroutine MNITXTTHK initializes and loads the
C     sub-menu options for style control of entities
C
      EXTERNAL MNICOL, GTDMWT
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/style.inc'
      include  'include/daxcolor.inc'
      include  'include/gtxt2.inc'

C     load the colour cell
      CALL MNICOL()
C
C     = is the token for thickness
      GTMULT = .TRUE.
      CALL GTDMWT(409,3,TXTHK)
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE MNLINS()
C     ===================
C1    No arguments required.
C
C2    Loads the entitiy type descriptors into the
C2    INSERT major option list menu.(menu no 2).
C
C
      EXTERNAL GTPMEN
C
C2    P is the token for PART
      CALL GTDMEN(70,2)
C2    H is the token for HATCH.
      CALL GTDMEN(71,2)
C2    T is the token for TEXT.
      CALL GTDMEN(72,2)
C2    F is the token for FILLET.
      CALL GTDMEN(73,2)
C2    A is the token for ARC.
      CALL GTDMEN(74,2)
C2    L is the token for LINE.
      CALL GTDMEN(75,2)
C2    C is the token for COMPONENT.
      CALL GTDMEN(76,2)
C2    D is the token for CURVE.
      CALL GTDMEN(77,2)
C2    S is the token for SYMBOL.
      CALL GTDMEN(78,2)
C2    M is the token for MARKER.
      CALL GTDMEN(79,2)
C2    E is the token for ELIPSE.
C      CALL GTDMEN(80,2)
C2    c is the token for CHAMFER.
      CALL GTDMEN(81,2)
C2    l is the token for CENTER LINE.
C2    NOTE: At the moment this overwrites elipse !!
      CALL GTDMEN(82,2)
C2
      END
C
C--------------------------------------------------------
C
