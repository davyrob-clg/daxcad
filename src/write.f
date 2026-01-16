C
C     @(#)  412.1 date 6/11/92 write.f 
C
C
C     Filename    : write.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:45:59
C     Last change : 92/06/11 14:43:43
C
C     Copyright : Practical Technology Limited  
C     File :- write.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION TNGENT(ENT1,X1,Y1,X2,Y2,ENT2,X3,Y3,X4,Y4)
C     SUBROUTINE MAJWRT()
C     SUBROUTINE MNIGNC()
C     SUBROUTINE MNIWRT()
C     SUBROUTINE WRTCV0()
C     SUBROUTINE WRTG00()
C     SUBROUTINE WRTGI0()
C     SUBROUTINE WRTGP0()
C     SUBROUTINE WRTGT0()
C     SUBROUTINE WRTIL0()
C     SUBROUTINE WRTPT0()
C     SUBROUTINE WWINDO(WWXMIN,WWYMIN,WWXMAX,WWYMAX,OK)
C
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE MAJWRT()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the WRITE mode
C2    of operation is selected from the master menu.
C2    controls operation of the CHANGE function
C
CIBM
C      include '\include\vertype.inc'
CIBM
CPC386
C      include 'include\\vertype.inc'
CPC386
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/krap.inc'
      include 'include/pwmgr.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
      LOGICAL AUTFLG
      REAL X,Y
C
      EXTERNAL MNIWRT,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW
      EXTERNAL BITSET,CHKPRD
C
CIBM|PC386
C      IF(RFIDF.NE.4.20.OR.RFIDF2.NE.2.46) RETURN
CIBM|PC386
C     Now activate the WRITE major option menu
      CALL MNIWRT()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
 10   CONTINUE
      CALL DCPRNT(284)
C     Read a cursor hit to select READ mode
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
         IF (CCMD.EQ.'E') THEN
C           IGES file output
            CALL CHKPRD(IGSCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL IGW000()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE IGES  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'G') THEN
C           write GNC spans file
            CALL CHKPRD(GNCCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTG00()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE GNC  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'p') THEN
C           write PEPS file
            CALL CHKPRD(PEPCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTGP0()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE PEPS  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'W') THEN
C           write GENIO format file
            CALL CHKPRD(MIFCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTGI0()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE GENIO  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'V') THEN
C           CV execute file
            CALL CHKPRD(CVXCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTCV0()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE CV-Exec  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'A') THEN
C           Pathtrace option
            CALL CHKPRD(PATCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTPT0()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE PATHTRACE  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'D') THEN
C           dxf file format output for AutoCAD
            CALL CHKPRD(DXFCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRDXF0(.FALSE.)
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE DXF  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'i') THEN
C           Interleaf document format.
            CALL CHKPRD(INTCODE, USELEV, AUTFLG)
            IF (AUTFLG) THEN
                CALL WRTIL0()
            ELSE
                CALL    ERRORDIALOG(
     +          'WRITE Interleaf  option not authorised')
                CALL GTMCLO(MEN,CELLN)
                GOTO 10
            ENDIF
C     ***************************************************************
         ELSE IF (CCMD.EQ.'Z') THEN
C           GEneric NC output from DAXCAD
            CALL WRMX00()
C     ***************************************************************
         ELSE
C           unrecognized read option
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
C     ------------------------------------------------------------------
C
      SUBROUTINE MNIGNC()
C     =================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2
C2    Tokens used here are B and CHAR(150)
C2
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTPMEN,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(121,3)
C2    B is the token for NEXT BOUNDARY.
      CALL GTDMEN(122,3)
C2    CHAR(150) is the token for ACCEPT.
      CALL GTDMEN(126,3)
C     AREA
      CALL GTDMEN(120,3)
C     Curve Entity mode.
      CALL GTDMEN(332,3)
C     Points mode.
      CALL GTDMEN(300,3)
C     Profile mode.
      CALL GTDMEN(301,3)
C
      CALL GTDMEN(469,3)
C
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE MNIWRT()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major WRITE options.
C2
      EXTERNAL GTPMEN,GTCLRM,GTMCHI
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the WRITE major option.
      CALL GTDMHD(26,2)
C
C     Load the nouns into major option menu.
C     CV execute file output
      CALL GTDMEN(339,2)
C     GNC Spans data
      CALL GTDMEN(305,2)
C     GENIO 'MOSS' Format
      CALL GTDMEN(307,2)
C     IGES file output
      CALL GTDMEN(304,2)
C     PEPS geometry format
      CALL GTDMEN(309,2)
C     PATHTRACE format
      CALL GTDMEN(461,2)
C     DXF output file format
      CALL GTDMEN(473,2)
C     INTERLEAF output file format
      CALL GTDMEN(474,2)
C     GENERIC NC output
      CALL GTDMEN(616,2)
C
      END
C
C     ------------------------------------------------------
C
      FUNCTION TNGENT(ENT1,X1,Y1,X2,Y2,ENT2,X3,Y3,X4,Y4)
C     ==================================================
C
C1    vartype    L     I2  R  R  R  R   I2  R  R  R  R
C1    iostatus   O     I   I  I  I  I   I   I  I  I  I
C
C2    Function TNGENT returns a logical result of tangency
C2    tests between the two passed line or arc entities.
C2    If both entities are lines then tangency is defined
C2    as the lines being co-linear.
C
      include 'include/entity.inc'
C
      INTEGER*2 ENT1,ENT2
      LOGICAL TNGENT,SAME
      REAL X1,Y1,X2,Y2,X3,Y3,X4,Y4,R1,CD0D13,DISTXY
      EXTERNAL CD0D13,SAME,DISTXY
C
      IF (ENT1.EQ.LINE) THEN
         IF (ENT2.EQ.LINE) THEN
C           reject LINE-LINE for now
            TNGENT=.FALSE.
         ELSE
C           ENT2 must be an ARC
C           find distance between arc centre and line
            R1=ABS(CD0D13(X1,Y1,X2,Y2,X3,Y3))
C           test for equality with arc radius
            TNGENT=SAME(R1,X4)
         END IF
      ELSE
C        ENT1 must be ARC
         IF (ENT2.EQ.LINE) THEN
C           find distance between arc centre and line
            R1=ABS(CD0D13(X3,Y3,X4,Y4,X1,Y1))
C           test for equality with arc radius
            TNGENT=SAME(R1,X2)
         ELSE
C           ENT2 must be ARC
C           find distance between centres
            R1=DISTXY(X1,Y1,X3,Y3)
            TNGENT=SAME(R1,X2+X4)
            IF (.NOT.TNGENT) TNGENT=SAME(R1,ABS(X2-X4))
         END IF
      END IF
C     return with TNGENT set to result
C
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WRTCV0()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls operation of the WRITE CV execute file
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      CHARACTER*1 CC
C
      EXTERNAL GTMCLO,WRTCV1
C
      TMEN=MEN
      TCELL=CELLN
C     enter the WRITE CV execute file routine
      CALL WRTCV1()
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     avoid infinite loop until implemented
      CCMD='q'
C
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WRTG00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls operation of the WRITE GNC spans
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      CHARACTER*1 CC
C
      EXTERNAL GTMCLO,WRTCV1
C
      TMEN=MEN
      TCELL=CELLN
      CALL MNIGNC()
C     enter the WRITE GNS SPANS
      CALL WRTG01()
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     avoid infinite loop until implemented
C
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WRTGI0()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    controls operation of the WRITE GENIO function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,WRTCV1
C
      TMEN=MEN
      TCELL=CELLN
C     enter the WRITE GENIO FUNCTION
      CALL WRTGI1()
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE WRTGP0()
C     ===================
C     This routine sets up the next cell for turning
C     milling or puncning operations
C     clear minor option
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR,MMEN,MCELL
      REAL X,Y
C
      EXTERNAL MNIWRT,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW
C
C     save the menu pointers to the main cell
      MMEN=MEN
      MCELL=CELLN
C     set up secondary menu for milling turning punching
      CALL GTDMEN(470,3)
C     peps turning format
      CALL GTDMEN(471,3)
C     peps punching
      CALL GTDMEN(472,3)
C     AUTOPROFILER
      CALL GTDMEN(469,3)
 10   CONTINUE
      CALL DCPRNT(38)
C     Read a cursor hit to select READ mode
      CALL TCURS(C,X,Y)
C
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     Go activate an option if you find one
C     ensure menu cell is hilited
      IF(MEN.EQ.2) GOTO 99
      IF(MEN.EQ.3) THEN
          CALL GTMCHI(TMEN,TCELL)
          IF(CCMD.EQ.'p') THEN
              CALL WRTGP1(1)
          ELSE IF (CCMD.EQ.'t') THEN
              CALL WRTGP1(2)
          ELSE IF (CCMD.EQ.'s') THEN
              CALL WRTGP1(3)
          ELSE
             CALL DEPRNT(8)
C            set MEN to 0 to avoid infinite loop on unassigned cell
             MEN=0
             GOTO 10
          END IF
      ENDIF
      CALL GTMCLO(MMEN,MCELL)
      CALL GTCLRM(3)
99    CONTINUE
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WRTGT0()
C     ===================
C     This routine sets up the next cell for turning
C     milling or puncning operations
C     clear minor option
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR,MMEN,MCELL
C
      REAL X,Y
C
C
      EXTERNAL MNIWRT,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW
C
C
C     save the menu pointers to the main cell
      MMEN=MEN
      MCELL=CELLN
C     set up secondary menu for milling turning punching
      CALL GTDMEN(470,3)
C     GTL turning format
      CALL GTDMEN(471,3)
C     AUTOPROFILER
      CALL GTDMEN(469,3)
 10   CONTINUE
      CALL DCPRNT(38)
C     Read a cursor hit to select READ mode
      CALL TCURS(C,X,Y)
C
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     Go activate an option if you find one
C     ensure menu cell is hilited
      IF(MEN.EQ.2) GOTO 99
      IF(MEN.EQ.3) THEN
          CALL GTMCHI(TMEN,TCELL)
          IF(CCMD.EQ.'p') THEN
              CALL WRTGT1(1)
          ELSE IF (CCMD.EQ.'t') THEN
              CALL WRTGT1(2)
          ELSE
             CALL DEPRNT(8)
C            set MEN to 0 to avoid infinite loop on unassigned cell
             MEN=0
             GOTO 10
          END IF
      ENDIF
      CALL GTMCLO(MMEN,MCELL)
      CALL GTCLRM(3)
99    CONTINUE
      END
C
C     ------------------------------------------------------------------
C
 
      SUBROUTINE WRTIL0()
C     ===================
C     This routine controls the WRITE INTERLEAF option.
C     for writing an Interleaf document file.
C     NOTE: This operation is really a PLOT function pretending to
C           be a write operation.
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/pendat.inc'
      include   'include/gerber.inc'
      include   'include/params.inc'
      include   'include/wrtinlf.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR,MMEN,MCELL,I,OLDPLT,WPOS,HPOS,
     +        INTA,INTB,DSCELL
      REAL X,Y,VAL,PROPF,PROPP,OLDLIM
      DOUBLE PRECISION DVAL
      CHARACTER WORD*16,TOKEN*1,TEMPS*30
      LOGICAL WINFLG
C
      EXTERNAL MNIWRT,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW
C
      TMEN = 3
      CLIPIT = .TRUE.
      CURTL=0
      OLDPLT = CURPLT
      PLOMOD=17
      OLDLIM = 1.0
      DO 5,I=1,10
         IF (PLOMOD.EQ.MODLST(I)) THEN
            CURPLT = I
         ENDIF
 5    CONTINUE
      CALL INTPLT()
      PROPP = DRWSIZ(1) / DRWSIZ(2)
C
      CALL FNDPOS(475,WPOS)
      CALL FNDPOS(476,HPOS)
      TOKEN = ' '
C
C     save the menu pointers to the main cell
      MMEN=MEN
      MCELL=CELLN
C     set up secondary menu forframe size, text type and accept.
C     Frame width
      CALL GTDMEN(475,3)
      WRITE (WORD,'(F8.2)') PAPLIM(1,3)
      CALL GTPMEN(WORD,TOKEN,3,WPOS+1)
C     Frame height
      CALL GTDMEN(476,3)
      WRITE (WORD,'(F8.2)') PAPLIM(1,4)
      CALL GTPMEN(WORD,TOKEN,3,HPOS+1)
C     Micro document
      CALL GTDMEN(477,3)
C     Convert to outline.
      CALL GTDMEN(478,3)
C     Hardware text.
      CALL GTDMEN(479,3)
C     Software text.
      CALL GTDMEN(314,3)
C     Accept.
      CALL GTDMEN(13,3)
C     Drawing.
      CALL GTDMEN(382,3)
C     Screen.
      CALL GTDMEN(383,3)
C
C     Highlight the default text mode.
      INTFLG = 'O'
      HARD = .TRUE.
      CALL FNDPOS(478,TCELL)
      CALL GTMCHI(TMEN,TCELL)
C     Highlight the default 'Drawing' mode.
      WINFLG = .FALSE.
      CALL FNDPOS(382,DSCELL)
      CALL GTMCHI(TMEN,DSCELL)
C
 10   CONTINUE
         CALL DCPRNT(38)
C        Read a cursor hit to select READ mode
         CALL TCURS(C,X,Y)
C
C        test for quit character
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C        Go activate an option if you find one
C        ensure menu cell is hilited
         IF(MEN.EQ.2) GOTO 99
         IF(MEN.EQ.3) THEN
            CALL GTMCHI(MEN,CELLN)
            IF(CCMD.EQ.CHAR(13)) THEN
C              Accept.
C              Go write it all out.
               IF (.NOT.WINFLG) THEN
C                 Check if we need to rotata the text.
                  PROPF = PAPLIM(1,3) / PAPLIM(1,4)
                  ROTTXT = 0
                  IF (PROPF.GT.1 .AND. PROPP.LT.1) ROTTXT = -90
                  IF (PROPF.LT.1 .AND. PROPP.GT.1) ROTTXT = 90
                  CALL PLTD00()
               ELSE
                  CALL PLTW00()
               ENDIF
               GOTO 100
            ELSE IF (CCMD.EQ.'d' .OR. CCMD.EQ.'s') THEN
C              All the drawing, or just the screen.
C              If its already selected, do nothing.
               IF (CELLN.NE.DSCELL) THEN
                  CALL GTMCLO(TMEN,DSCELL)
C                 save pointers to menu and cell which was hit
                  DSCELL=CELLN
                  IF (CCMD.EQ.'d') THEN
                     WINFLG = .FALSE.
C                    Redraw the text options.
C                    Micro document
                     CALL GTDMEN(477,3)
C                    Convert to outline.
                     CALL GTDMEN(478,3)
C                    Hardware text.
                     CALL GTDMEN(479,3)
C                    Software text.
                     CALL GTDMEN(314,3)
C                    And re highlight the currently selected one.
                     CALL GTMCHI(TMEN,TCELL)
C                    Modify the frame
                     PROPF = PAPLIM(1,3) / PAPLIM(1,4)
                     IF (PROPF.GT.1.AND.PROPP.LT.1) THEN
                        DO 30 I=1,5
                           PAPLIM(I,4) = OLDLIM
 30                     CONTINUE
                        WRITE (WORD,'(F8.2)') PAPLIM(1,4)
                        CALL GTPMEN(WORD,TOKEN,3,HPOS+1)
                     ENDIF
                  ELSE
                     WINFLG = .TRUE.
C                    clear the text options.
C                    Micro document
                     CALL FNDPOS(477,INTA)
                     CALL GTCLRC(3,INTA)
C                    Convert to outline.
                     CALL FNDPOS(478,INTA)
                     CALL GTCLRC(3,INTA)
C                    Hardware text.
                     CALL FNDPOS(479,INTA)
                     CALL GTCLRC(3,INTA)
C                    Software text.
                     CALL FNDPOS(314,INTA)
                     CALL GTCLRC(3,INTA)
C                    Modify the frame.
                     PROPF = PAPLIM(1,3) / PAPLIM(1,4)
                     OLDLIM = PAPLIM(1,4)
                     IF (PROPF.LT.1) THEN
                        PAPLIM(1,4) = PAPLIM(1,3)/1.2
                        DO 40 I=2,5
                           PAPLIM(I,4) = PAPLIM(1,4)
 40                     CONTINUE
                        WRITE (WORD,'(F8.2)') PAPLIM(1,4)
                        CALL GTPMEN(WORD,TOKEN,3,HPOS+1)
                     ENDIF
                  ENDIF
               ENDIF
            ELSE IF (CCMD.EQ.'w' .OR. CCMD.EQ.'f') THEN
C              Changing frame size.
               IF (CCMD.EQ.'w') THEN
C                 Changing the width.
                  CALL DPRMXP(498,TEMPS)
                  INTA = WPOS+1
                  INTB = 3
               ELSE
C                 Changing the height.
                  CALL DPRMXP(499,TEMPS)
                  INTA = HPOS+1
                  INTB = 4
               ENDIF
               CALL AEXPRN(TEMPS,DVAL,*10)
               VAL = REAL(DVAL)
               IF (VAL.LE.0) VAL = 1.0
               DO 20 I=1,5
                  PAPLIM(I,INTB) = VAL
 20            CONTINUE
               CALL GTCLRC(3,INTA)
               CALL GTCLRC(3,HPOS+1)
               WRITE (WORD,'(F8.2)') PAPLIM(1,3)
               CALL GTPMEN(WORD,TOKEN,3,WPOS+1)
               WRITE (WORD,'(F8.2)') PAPLIM(1,4)
               CALL GTPMEN(WORD,TOKEN,3,HPOS+1)
C              Ensure that the cell is no longer highlighted.
               CALL GTMCLO(MEN,CELLN)
            ELSE
C              Changing text form.
C              Ensure that the last cell hit is no longer highlighted.
               CALL GTMCLO(TMEN,TCELL)
C              save pointers to menu and cell which was hit
               TCELL=CELLN
               IF (CCMD.EQ.'m') THEN
                  INTFLG = 'M'
                  HARD = .TRUE.
               ELSE IF (CCMD.EQ.'o') THEN
                  INTFLG = 'O'
                  HARD = .TRUE.
               ELSE IF (CCMD.EQ.'h') THEN
                  INTFLG = 'T'
                  HARD = .TRUE.
               ELSE IF (CCMD.EQ.'T') THEN
                  HARD = .FALSE.
               ELSE
                 CALL DEPRNT(8)
               END IF
            ENDIF
         ENDIF
C           set MEN to 0 to avoid infinite loop on unassigned cell
         MEN=0
      GOTO 10
C
C     All done, go home.
 100  CONTINUE
      CALL GTMCLO(MMEN,MCELL)
      CALL GTCLRM(3)
 99   CONTINUE
      CURPLT = OLDPLT
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WRTPT0()
C     ===================
C1    vartype
C1    iostatus
C2
C2    controls operation of the PATHTRACE operation
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      CHARACTER*1 CC
      EXTERNAL GTMCLO,WRTCV1
C
      TMEN=MEN
      TCELL=CELLN
      CALL MNIGNC()
C     enter the WRITE GNS SPANS
      CALL WRTPT1()
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     avoid infinite loop until implemented
C
      END
C
C     ------------------------------------------------------------------
C
      SUBROUTINE WWINDO(WWXMIN,WWYMIN,WWXMAX,WWYMAX,OK)
C     =================================================
C
C1    vartype             R      R      R      R     L
C1    iostatus            O      O      O      O     O
C
      include 'include/macro.inc'
      INTEGER PRNUM
      REAL WWXMIN,WWYMIN,WWXMAX,WWYMAX,SXMIN,SXMAX,SYMIN,SYMAX
      REAL RTMP
      LOGICAL OK
C
      EXTERNAL GETWIN,SC2WO,DEPRNT,DCPRNT
C
      TCONT=(GINPUT.AND.GANS)
      IF(MACOP.AND..NOT.GINPUT) THEN
         CALL MAC101(WWXMIN,WWYMIN,OK)
         IF(.NOT.OK.OR.GINPUT) GOTO 20
         CALL MAC101(WWXMAX,WWYMAX,OK)
         IF(.NOT.OK.OR.GINPUT) GOTO 20
         IF(WWXMIN.GT.WWXMAX) THEN
            CALL RSWAP(WWXMIN,WWXMAX)
         END IF
         IF(WWYMIN.GT.WWYMAX) THEN
            CALL RSWAP(WWYMIN,WWYMAX)
         END IF
C       WRITE(10,*)'OUT OF MAC101 WITH CCMD,MEN,CELLN',CCMD,MEN,CELLN
         GOTO 44
      END IF
 20   CONTINUE
 10   CONTINUE
C     find the screen area of interest
      CALL GETWIN(SXMIN,SYMIN,SXMAX,SYMAX,OK)
C     abort this function if no valid area returned
      IF (.NOT.OK) RETURN
C     limit minimum window to 6 pixels
      IF (ABS(SXMAX-SXMIN).LT.6.OR.ABS(SYMAX-SYMIN).LT.6 ) THEN
         PRNUM=21
         CALL DEPRNT(PRNUM)
         GOTO 10
      END IF
C
C     convert to real world coords for return
      CALL SC2WO(SXMIN,SYMAX,WWXMIN,WWYMIN)
      CALL SC2WO(SXMAX,SYMIN,WWXMAX,WWYMAX)
C     sort coords into ascending order
      IF (WWXMIN.GT.WWXMAX) THEN
         RTMP=WWXMAX
         WWXMAX=WWXMIN
         WWXMIN=RTMP
      END IF
      IF (WWYMIN.GT.WWYMAX) THEN
         RTMP=WWYMAX
         WWYMAX=WWYMIN
         WWYMIN=RTMP
      END IF
 44   CONTINUE
      OK=.TRUE.
C
      END
C
C     -------------------------------------------------
C
