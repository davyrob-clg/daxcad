C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 inserta.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C     
C     SUBROUTINE DRWCEN(SCRN)
C     SUBROUTINE ICPRMS(CLTYPE,PNUM,FMIPOS,POPNUM,FIRST)
C     SUBROUTINE INS0A5()
C     SUBROUTINE INS0AM(OK)
C     SUBROUTINE INS0C5(FULL)
C     SUBROUTINE INS1A5()
C     SUBROUTINE INS1C5()
C     SUBROUTINE INSA01()
C     SUBROUTINE INSC3T()
C     SUBROUTINE INSCEN()
C     SUBROUTINE INSCN1()
C     SUBROUTINE INSCN2(XCEN,YCEN)
C     SUBROUTINE INSCNA()
C     SUBROUTINE INSCT0()
C     SUBROUTINE INSDC0()
C     SUBROUTINE INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
C     SUBROUTINE INSRT0()
C     SUBROUTINE MNI01A()
C     SUBROUTINE MNI02A()
C     SUBROUTINE MNIARC
C     SUBROUTINE MNICEN()
C     SUBROUTINE MNU01A()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DRWCEN(SCRN)
C     =======================
C     Vartype            L
C     Iostatus           I
C
C2  Drwcen draws the centerline entity. The necessary data is assumed 
C2  to be in the NBUFF data. Ir SCRN is true, the center line is drawn
C2  to the screen, otherwise it is plotted.
C2  NOTE: For an arc     minor radius = major radius,
C2         "  an elipse    "     "    <   "     "   ,
C2         "  a single center line   minor radius = 0.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2
      REAL X1,Y1, X2,Y2, MAJRAD,MINRAD, INCANG,ANG, DUMMY, CENX,CENY,
     +     RCX,RCY,BRDR,PI,STANG,ENANG,CANG, B
      INTEGER*4 FONT
      INTEGER*2 CLTYPE
      LOGICAL OK,SCRN,DISPLY
      EXTERNAL PI,CANG
C
C     right wobbles doesn't check that DCCP19 fails
C     so some defense required
C
      DX1 = 0.0
      DX2 = 0.0
      DY1 = 0.0
      DY2 = 0.0
C
C     Default for single center lines.
      DISPLY = .FALSE.
C     Get all the stuff out of the buffer.
      CENX = RDBUFF(1)
      CENY = RDBUFF(2)
      MAJRAD = RDBUFF(3)
      MINRAD = RDBUFF(4)
      INCANG = 0
      ANG = RDBUFF(5)
C     Get the extension and convert it from paper to world.
      CALL PAP2SC(RDBUFF(6),B)
      CALL SC2WO(B,0.0,BRDR,DUMMY)
      CALL SC2WO(0.0,0.0,DUMMY,B)
      BRDR = BRDR - DUMMY
      CLTYPE = IDBUFF(4)
      FONT = IMBUFF(6)
C
      IF (CLTYPE.EQ.3) THEN
C        P.C.D. center line.
         DUMMY = MAJRAD
C        Find the referance arc center point.
         CALL NEWPNT(CENX,CENY,MAJRAD,DUMMY,INCANG,ANG,RCX,RCY)
C 
C        Draw the staight, radial line.
         MINRAD = MINRAD + BRDR
         DUMMY = MINRAD
C        First line, find finish point.
         CALL NEWPNT(RCX,RCY,MINRAD,DUMMY,INCANG,ANG,X2,Y2)
C        and start point is the same dist again, in the other direction.
         X1 = RCX - (X2 - RCX)
         Y1 = RCY - (Y2 - RCY)
         IF (SCRN) THEN
            CALL DRWFLW(X1,Y1,X2,Y2,FONT)
         ELSE
            CALL PLTFLW(X1,Y1,X2,Y2,FONT)
         ENDIF
C        Store the display flag.
         DISPLY = DISPV
C
C        Draw the curved portion.
         ANG = ANG + PI(0.5)
C        Find start and finish point.
         XC1 = DBLE(CENX)
         YC1 = DBLE(CENY)
         RAD1 = DBLE(MAJRAD)
         XC2 = DBLE(RCX)
         YC2 = DBLE(RCY)
         RAD2 = DBLE(MINRAD)
         CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2,OK)
         X1 = REAL(DX1)
         Y1 = REAL(DY1)
         X2 = REAL(DX2)
         Y2 = REAL(DY2)
C        Convert the points to angles,
         STANG = CANG(CENX,CENY,X1,Y1)
         ENANG = CANG(CENX,CENY,X2,Y2)
C        and draw.
         IF (SCRN) THEN
            CALL DRWFAW(CENX,CENY,MAJRAD,STANG,ENANG,FONT)
         ELSE
            CALL PLTFAW(CENX,CENY,MAJRAD,STANG,ENANG,FONT)
         ENDIF
      ELSE
C        Cross or single center lines.
C
C        First line, find finish point.
         MAJRAD = MAJRAD + BRDR
         DUMMY = MAJRAD
         CALL NEWPNT(CENX,CENY,MAJRAD,DUMMY,INCANG,ANG,X2,Y2)
C        and start point is the same dist again, in the other direction.
         X1 = CENX - (X2 - CENX)
         Y1 = CENY - (Y2 - CENY)
         IF (SCRN) THEN
            CALL DRWFLW(X1,Y1,X2,Y2,FONT)
         ELSE
            CALL PLTFLW(X1,Y1,X2,Y2,FONT)
         ENDIF
C
C        Second line, if required.
         IF (CLTYPE.EQ.1) THEN
C           Store the display flag.
            DISPLY = DISPV
C           Same stuff an 90 degrees.
            MINRAD = MINRAD + BRDR
            DUMMY = MINRAD
            ANG = ANG + PI(0.5)
            CALL NEWPNT(CENX,CENY,MINRAD,DUMMY,INCANG,ANG,X2,Y2)
C        and start point is the same dist again, in the other direction.
            X1 = CENX - (X2 - CENX)
            Y1 = CENY - (Y2 - CENY)
            IF (SCRN) THEN
               CALL DRWFLW(X1,Y1,X2,Y2,FONT)
            ELSE
               CALL PLTFLW(X1,Y1,X2,Y2,FONT)
            ENDIF
         ENDIF
      ENDIF
C     If either line is vivible, set DISPV.
      DISPV = DISPV .OR. DISPLY
C
      END
C
      SUBROUTINE ICPRMS(CLTYPE,PNUM,FMIPOS,POPNUM,FIRST)
C     ==================================================
C1    VARTYPE             I2    I4    I2     I4     L
C1    IOSTATUS            O     O     I      I      I
C
C2    Changing the center line modifiers.  
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
      include 'include/inscen.inc'
      include  'include/lfont.inc'
C
      CHARACTER*80 INSTR
      CHARACTER*10 STRNG, TEMP
      INTEGER*4 PNUM,POPNUM,NLEN,MNUM,TMEN,TCELL
      INTEGER*2 FMIPOS,REMV,ENT,CLTYPE
      REAL DEGANG,RAD,DEG
      DOUBLE PRECISION DNUM
      LOGICAL OK,FIRST,MOK,CVERFY,TOKN
      EXTERNAL RAD,DEG,NLEN,CVERFY
C
      TMEN = MEN
      TCELL=CELLN
      IF (CCMD.EQ.'t') then
C        Type of center line. (single or cross).
         CALL MENPOP(POPNUM,OK)
         IF (.NOT.OK) THEN
            CALL GTMCLO(TMEN,TCELL)
            RETURN
         ENDIF
         IF (CCMD.EQ.CHAR(1)) THEN
C           A single center line.
            TEMP = VNOUN(86)
            GTMULT = .TRUE.
            CALL GTDMWT(83,3,TEMP)
            CLTYPE = 2
            PNUM = 250
C           Enable points mode.
            CALL MNLPTS()
C           Remove the 'Rotation Angle' cells.
            CALL GTCLRC(3,10)
            CALL GTCLRC(3,11)
C           Remove the 'Widow' and 'Accept' cells.
            CALL GTCLRC(3,17)
            CALL GTCLRC(3,16)
         ELSE IF (CCMD.EQ.CHAR(2)) THEN
C           A cross over an arc or elipse.
            TEMP = VNOUN(87)
            GTMULT = .TRUE.
            CALL GTDMWT(83,3,TEMP)
            CLTYPE = 1
            PNUM = 201
C           Disable Points mode.
            CALL MNUPTS()
C           Put the 'Rotation Angle" cells back.
C           r token for ROTATION ANGLE
            CALL GTDMEN(85,3)
C           And the size ...
            DEGANG = DEG(ROTANG)
            WRITE(STRNG,'(F8.3)') DEGANG
            CALL GTPMEN(STRNG,' ',3,11)
C           Put the 'Widow' cell up.
            CALL GTDMEN(120,3)
C           Disable the 'Accept' cell .
            CALL GTCLRC(3,16)
C           Initalise the scratch file.
            CALL UNFLAG(.TRUE.)
         ELSE
C           A P.C.D center line.
            TEMP = VNOUN(167)
            GTMULT = .TRUE.
            CALL GTDMWT(83,3,TEMP)
            CLTYPE = 3
            PNUM = 605
C           Enable points mode.
            CALL MNLPTS()
C           Remove the 'Rotation Angle' cells.
            CALL GTCLRC(3,10)
            CALL GTCLRC(3,11)
C           Remove the 'Widow'  and 'Accept cells. We'll put them up
C           when we need them.
            CALL GTCLRC(3,17)
            CALL GTCLRC(3,16)
C           Initalise the scratch file.
            CALL UNFLAG(.TRUE.)
         ENDIF
         CALL GTMCLO(TMEN,TCELL)
C
      ELSE IF (CCMD.EQ.'b') then
C        Extension  size. (How far the center line goes beyond the object.)
 10      CONTINUE
C        Get the input and convert it to a number.
         CALL DPRMXP(195,INSTR)
         IF (NLEN(INSTR).GT.0) THEN
            CALL AEXPRN(INSTR,DNUM,*10)
            BRDRSZ = REAL(DNUM)
C           Don't trust user's input to be tidy. Write it my way.
            WRITE(STRNG,'(F8.3)') BRDRSZ
C           Put it into the menu cell for the world to see.
            CALL GTPMEN(STRNG,' ',3,8)
         ENDIF
         CALL GTMCLO(3,7)
C
      ELSE IF (CCMD.EQ.'r') then
C        Rotation angle. This is only relevant for 'cross'.
C        Otherwise, it is calculated from the data.
 20      CONTINUE
C        Get the input and convert it to a number.
         CALL DPRMXP(188,INSTR)
         IF (NLEN(INSTR).GT.0) THEN
            CALL AEXPRN(INSTR,DNUM,*20)
            DEGANG = REAL(DNUM)
C           He's in degrees, but I want radians.
            ROTANG = RAD(DEGANG)
C           Don't trust user's input to be tidy. Write it my way.
            WRITE(STRNG,'(F8.3)') DEGANG
C           Put it into the menu cell for the world to see.
            CALL GTPMEN(STRNG,' ',3,11)
         ENDIF
         CALL GTMCLO(3,10)
C
      ELSE IF (CCMD.EQ.'c') then
C        Cancel.
         REMV = 1
         ENT = CENLIN
         CALL INSCNL(FMIPOS,ENT,REMV,FIRST,OK)
         CALL GTMCLO(3,19)
      ELSE IF (CCMD.EQ.'f') THEN
C******************************************************
C             New font for centre lines.              *
C******************************************************
         CALL MENPOP(-1,TOKN)
C        Check for menu hit.
         IF ( TOKN ) THEN
C           Update the global data.
            CLINEF=ICHAR(CCMD)
C           Find the name of the font.
C           Put that name into the menu cell.
            CLINET = FONTNM(CLINEF)
            GTMULT = .TRUE.
            CALL GTDMWT(407,3,CLINET)
         END IF
         CALL GTMCLO(TMEN,TCELL)                                  
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      ELSE IF (CVERFY(CCMD,'=k')) THEN
C           ****************************
C            Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(TMEN,TCELL)                                  
      ENDIF 
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INS0A5()
C     ===================
C2       No arguments passed
C2       Common blocks used are STYLE,MASTI,MASTC
C2    Draws an arc given centre point and two points
C2    on the circumference.The first circumferential
C2    point defines the radius.(CCW)
C2    selected by 'A' from arc option menu.
C2
C2    assumes at entry that the calling menu cell is hilited,
C2    at exit ensures that the calling menu cell is no longer
C2    hilited.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/arcdat.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 NEW,ENTYPE,P,FMIPOS
      INTEGER*4 C,TMEN,TCELL,TCELL1
      LOGICAL OK,FIRST
      REAL ZERO,DISTXY,X1,X2,Y1,Y2,RADIUS,X3,Y3,SANG,EANG,
     + SX1,SX2,SY1,SY2,SX3,SY3,PI
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,
     +                 DDSTXY,DCANG,DZERO
      INTRINSIC DBLE,REAL,MOD
      EXTERNAL DZERO,DCANG,CROSS,GETANS,WO2SC,DDSTXY,
     1         MNLPTS,MNUPTS,GTMCLO,GTMCHI,GTMCWI,MNI01A,MNI02A,
     2         INS0AM,GTHFMC,DEWC05,DRWFAW,MNU01A
C
      TMEN=MEN
      TCELL=CELLN
      FMIPOS=NMIPOS
      FIRST=.TRUE.
C                
C     initialise the screen hits & world hits for 
C     the usual reasons
      X1 = 0
      X2 = 0
      X3 = 0
C
      SX1 = 0
      SX2 = 0
      SX3 = 0
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C
C     Load fixed Radius option
      CALL MNI01A()
C     Load fixed ANGLE option
      CALL MNI02A()
C     write the current font number into the cell
      CALL MNISTD()
C     enter the point modes to the option menu
      CALL MNLPTS()
C
 10   CONTINUE
      CALL DCPRNT(178)
      CALL GETANS(C,X1,Y1)
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
         IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 10
         END IF
         CALL INS0AM(OK)
         IF ( OK ) GOTO 10
         GOTO 605
      END IF
C
      DX1=DBLE(X1)
      DY1=DBLE(Y1)
C     show first point on screen
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
C
 20   CONTINUE
C
      IF ( .NOT. ( RADSET .AND. ARCSET ) ) THEN
         CALL DCPRNT(179)
         CALL GETANS(C,X2,Y2)
         IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
         IF (MEN.EQ.2) GOTO 605
         IF (MEN.EQ.3) THEN
            IF ( CCMD .EQ. 'c' ) THEN
               CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
               GOTO 20
            END IF
            CALL INS0AM(OK)
            IF ( OK ) GOTO 20
            GOTO 605
         END IF
C
C        show second point on screen
         CALL WO2SC(X2,Y2,SX2,SY2)
         CALL CROSS(SX2,SY2)
         DX2=DBLE(X2)
         DY2=DBLE(Y2)
      END IF
C
      IF ( .NOT. RADSET ) THEN
         RADIUS=REAL(DZERO(DDSTXY(DX1,DY1,DX2,DY2)))
      ELSE
         RADIUS=ARCRAD
      END IF
C
      IF ( .NOT. ARCSET ) THEN
         SANG=REAL(DCANG(DX1,DY1,DX2,DY2))
      ELSE
         SANG=ANGLE
         ARCSET=.FALSE.
         CALL GTHFMC(3,'U',TCELL1)
         CALL GTMCLO(3,TCELL1)
      END IF
C
      IF ( RADIUS .EQ. 0.0 ) THEN
C        erase second point from screen
         CALL CROSS(SX2,SY2)
         CALL DEPRNT(94)
         GOTO 20
      END IF
C     find end point
30    CONTINUE
      IF ( .NOT. ARCSET ) THEN
         CALL DCPRNT(180)
         CALL GETANS(C,X3,Y3)
         IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
         IF (MEN.EQ.2) GOTO 605
         IF (MEN.EQ.3) THEN
            IF ( CCMD .EQ. 'c' ) THEN
               CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
               GOTO 30
            END IF
            CALL INS0AM(OK)
            IF ( OK ) GOTO 30
            GOTO 605
         END IF
         DX3=DBLE(X3)
         DY3=DBLE(Y3)
         EANG=REAL(DCANG(DX1,DY1,DX3,DY3))
      ELSE
         EANG=MOD(SANG+ANGLE,PI(2.0))
      END IF
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     store the new arc
      CALL DEWC05(X1,Y1,RADIUS,SANG,EANG,CLFONT,CLAYER,P,OK)
C     erase crosses from screen
      CALL CROSS(SX1,SY1)
      CALL CROSS(SX2,SY2)
      IF ( .NOT. OK ) THEN
         GOTO 605
      END IF
C     draw the arc just created
      C=CLFONT
      ENTYPE=ARC
      CALL ALLDRW(ENTYPE,P)
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
      FIRST=.FALSE.
      GOTO 10
C
 605   CONTINUE
C
C     Unload fixed Radius option
      CALL MNU01A()
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INS0AM(OK)
C     =====================
C1                      O
C1                      L
C2    Subroutine INS0AM handles option
C2    calls from the INSERT ARC function
C
      include  'include/menun.inc'
      include  'include/masti.inc'
      include  'include/arcdat.inc'
      include  'include/lfont.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
      include  'include/gtxt2.inc'
C
      DOUBLE PRECISION DIST
      REAL RAD
      CHARACTER*16 OLIN
      LOGICAL OK,FOK,CVERFY
      INTEGER*4 NLEN,MNUM
      INTRINSIC REAL,MOD
      EXTERNAL NLEN,GTMCLO,GTMCWI,GTMCHI,AEXPRN,GTCLRC,GTPMEN,
     +         RAD, CVERFY, INSATT
C
      OK=.TRUE.
C
      IF ( CCMD .EQ. 'S' ) THEN
         IF ( RADSET ) THEN
            CALL GTMCLO(MEN,CELLN)
            RADSET=.FALSE.
         ELSE
            CALL GTMCHI(MEN,CELLN)
            RADSET=.TRUE.
         END IF
      ELSEIF ( CCMD .EQ. 'C' ) THEN
         IF ( CIRSET ) THEN
            CALL GTMCLO(MEN,CELLN)
            CIRSET=.FALSE.
         ELSE
            CALL GTMCHI(MEN,CELLN)
            CIRSET=.TRUE.
         END IF
      ELSE IF ( CCMD .EQ. 'c' ) THEN
C          cancel last entity based on FIRST1 
          CALL INSCNL(FPOS,ARC,1,FIRST1,FOK)
C
      ELSE IF ( CCMD .EQ. 'U' ) THEN
         IF ( ARCSET ) THEN
            CALL GTMCLO(MEN,CELLN)
            ARCSET=.FALSE.
         ELSE
            CALL GTMCHI(MEN,CELLN)
            ARCSET=.TRUE.
         END IF
      ELSE IF ( CCMD .EQ. 'R' ) THEN
 111     CALL DPRMXP(186,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C          evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DIST,*111)
            IF ( DIST .GT. 0.0 ) THEN
               ARCRAD=REAL(DIST)
C              update cell contents for RAD:
               CALL GTDMWR(115,3,ARCRAD,'(F8.2)')
            ELSE
C              prompt user that zero radius is illegal
C              and try again
               CALL DEPRNT(187)
               GOTO 111
            END IF
         END IF
C
      ELSE IF (CVERFY(CCMD,'=fk')) THEN
C    ****************************
C       Change line attributes.  
C    ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(MEN, CELLN)                                  
C
      ELSE IF ( CCMD .EQ. 'V' ) THEN
 112     CALL DPRMXP(188,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DIST,*112)
C           user input is degrees
            ANGLE=REAL(DIST)
C           update cell contents for ANGLE:
            CALL GTDMWR(117,3,ANGLE,'(F8.2)')
C           remember internal storage is radians
            ANGLE=RAD(ANGLE)
C
         END IF
      ELSE
         OK=.FALSE.
      END IF
 
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INS0C5(FULL)
C     ===================
C1       No arguments passed
C1       Common blocks used are STYLE,MASTI,MASTC
C2
C2    Draws a circle through three circumferential points
C2    Selected by 'G' from arc option menu
C2    assumes at entry that the calling menu cell is hilited,
C2    at exit ensures that the calling menu cell is no longer
C2    hilited.
C
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/lfont.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 NEW,ENTYPE,P,FMIPOS
      INTEGER*4 C,TMEN,TCELL
      REAL ZERO,X1,X2,Y1,Y2,RADIUS,X3,Y3,SANG,EANG,
     + SX1,SX2,SY1,SY2,SX3,SY3,XC,YC,PI,CANG
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,
     +                 DXC,DYC,DRAD,DDSTXY,DCANG
      LOGICAL OK,FIRST,FULL,DCLKWS,CVERFY
      INTRINSIC ATAN,DBLE,REAL,MOD
C
      EXTERNAL CROSS,GETANS,WO2SC,DCCC10,GTMCWI,GTMCLO,
     +         GTMCHI,PI,DCLKWS,
     1         MNLPTS,DEWC05,DRWFAW,MNUPTS,CANG,INSATT,CVERFY
C
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
      FIRST=.TRUE.
      FMIPOS=NMIPOS
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C     write the current font number into the cell
      CALL MNISTD()
C     enter the point modes to the option menu
      CALL MNLPTS()
C
 10   CONTINUE
      CALL DCPRNT(183)
      CALL GETANS(C,X1,Y1)
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
C
         IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C           Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(MEN, CELLN)                                  
            GOTO 10 
         ELSE IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 10
         ELSE
            GOTO 605
         END IF
      END IF
C
      DX1=DBLE(X1)
      DY1=DBLE(Y1)
C     show first point
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
C
 20   CONTINUE
      CALL DCPRNT(184)
      CALL GETANS(C,X2,Y2)
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
C
         IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C           Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(MEN, CELLN)                                  
            GOTO 20
         ELSE IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 20
         ELSE
            GOTO 605
         END IF
      END IF
C
      DX2=DBLE(X2)
      DY2=DBLE(Y2)
C     show second point
      CALL WO2SC(X2,Y2,SX2,SY2)
      CALL CROSS(SX2,SY2)
C
 30   CONTINUE
      CALL DCPRNT(185)
      CALL GETANS(C,X3,Y3)
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
C
         IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C           Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(MEN, CELLN)                                  
            GOTO 30
         ELSE IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 30
         ELSE
            GOTO 605
         END IF
      END IF
C
      DX3=DBLE(X3)
      DY3=DBLE(Y3)
C     show third point
      CALL WO2SC(X3,Y3,SX3,SY3)
      CALL CROSS(SX3,SY3)
C
C     construct the circle
      CALL DCCC10(DX1,DY1,DX2,DY2,DX3,DY3,DXC,DYC,DRAD,OK)
C
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(94)
C        erase crosses from screen
         CALL CROSS(SX1,SY1)
         CALL CROSS(SX2,SY2)
         CALL CROSS(SX3,SY3)
         GOTO 10
      END IF
C
      IF ( FULL ) THEN
         SANG=0.0
         EANG=PI(2.0)
      ELSE
         IF ( DCLKWS(DXC,DYC,DX2,DY2,DX1,DY1) ) THEN
            SANG=CANG(REAL(DXC),REAL(DYC),REAL(DX3),REAL(DY3))
            EANG=CANG(REAL(DXC),REAL(DYC),REAL(DX1),REAL(DY1))
         ELSE
            SANG=CANG(REAL(DXC),REAL(DYC),REAL(DX1),REAL(DY1))
            EANG=CANG(REAL(DXC),REAL(DYC),REAL(DX3),REAL(DY3))
         END IF
      END IF
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     store the new arc
      CALL DEWC05(REAL(DXC),REAL(DYC),REAL(DRAD),
     +   SANG,EANG,CLFONT,CLAYER,P,OK)
      IF ( .NOT. OK ) THEN
         GOTO 605
      END IF
C     erase crosses from screen
      CALL CROSS(SX1,SY1)
      CALL CROSS(SX2,SY2)
      CALL CROSS(SX3,SY3)
C     draw the arc just created
      C=CLFONT
      ENTYPE=ARC
      CALL ALLDRW(ENTYPE,P)
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
 
      FIRST=.FALSE.
      GOTO 10
C
 605   CONTINUE
C
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INS1A5()
C     ===================
C2       No arguments passed
C2       Common blocks used are STYLE,MASTI,MASTC
C2    Draws an arc given centre point and two points
C2    on the circumference.The first circumferential
C2    point defines the radius. CW.
C2    selected by 'B' from arc option menu.
C2
C2    assumes at entry that the calling menu cell is hilited,
C2    at exit ensures that the calling menu cell is no longer
C2    hilited.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/arcdat.inc'
      include   'include/entity.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 NEW,ENTYPE,P,FMIPOS
      INTEGER*4 C,TMEN,TCELL,TCELL1
      LOGICAL OK,FIRST,CR1,CR2,CR3
      REAL ZERO,DISTXY,X1,X2,Y1,Y2,RADIUS,X3,Y3,SANG,EANG,
     + SX1,SX2,SY1,SY2,SX3,SY3,PI
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,
     +                 DDSTXY,DCANG,DZERO
      INTRINSIC DBLE,REAL,MOD
      EXTERNAL DZERO,DCANG,CROSS,GETANS,WO2SC,
     +         DEWC05,DDSTXY,DRWFAW,
     1         MNLPTS,MNUPTS,GTMCLO,GTMCHI,GTMCWI,MNI01A,MNI02A,
     2         INS0AM,GTHFMC,MNU01A
C
C     save pointers to menu cell which was the caller
      CR1 = .FALSE.
      CR2 = .FALSE.
      CR3 = .FALSE.
      TMEN=MEN
      TCELL=CELLN
      FMIPOS=NMIPOS
      FIRST=.TRUE.
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C
C     Load fixed Radius option
      CALL MNI01A()
C     Load fixed ANGLE option
      CALL MNI02A()
C     write the current font number into the cell
      CALL MNISTD()
C     enter the point modes to the option menu
      CALL MNLPTS()
C
 10   CONTINUE
      CALL DCPRNT(178)
      CALL GETANS(C,X1,Y1)
C
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
         IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 10
         END IF
         CALL INS0AM(OK)
         IF ( OK ) GOTO 10
         GOTO 605
      END IF
C
C     show the first point
      CR1 = .TRUE.
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
C     save in DP form
      DX1=DBLE(X1)
      DY1=DBLE(Y1)
C
 20   CONTINUE
C
      IF ( .NOT. (RADSET .AND. ARCSET) ) THEN
         CALL DCPRNT(179)
         CALL GETANS(C,X2,Y2)
         IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
         IF (MEN.EQ.2) GOTO 605
         IF (MEN.EQ.3) THEN
            IF ( CCMD .EQ. 'c' ) THEN
               CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
               GOTO 20
            END IF
            CALL INS0AM(OK)
            IF ( OK ) GOTO 20
            GOTO 605
         END IF
C
C        show second point
         CALL WO2SC(X2,Y2,SX2,SY2)
         CALL CROSS(SX2,SY2)
         DX2=DBLE(X2)
         DY2=DBLE(Y2)
C
      END IF
C
      IF ( .NOT. RADSET ) THEN
         CR2 = .TRUE.
         RADIUS=REAL(DZERO(DDSTXY(DX1,DY1,DX2,DY2)))
      ELSE
         RADIUS=ARCRAD
      END IF
C
      IF ( .NOT. ARCSET ) THEN
         CR2 = .TRUE.
         EANG=REAL(DCANG(DX1,DY1,DX2,DY2))
      ELSE
         EANG=ANGLE
         ARCSET=.FALSE.
         CALL GTHFMC(3,'U',TCELL1)
         CALL GTMCLO(3,TCELL1)
      END IF
C
      IF ( RADIUS .EQ. 0.0 ) THEN
        CALL CROSS(SX1,SY1)
        CALL CROSS(SX2,SY2)
        CALL DEPRNT(94)
        GOTO 20
      END IF
C     find end point
30    CONTINUE
      IF ( .NOT. ARCSET ) THEN
         CALL DCPRNT(180)
         CALL GETANS(C,X3,Y3)
         IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
         IF (MEN.EQ.2) GOTO 605
         IF (MEN.EQ.3) THEN
            IF ( CCMD .EQ. 'c' ) THEN
               CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
               GOTO 30
            END IF
            CALL INS0AM(OK)
            IF ( OK ) GOTO 30
            GOTO 605
         END IF
C        show third point
         CALL WO2SC(X3,Y3,SX3,SY3)
         CALL CROSS(SX3,SY3)
         DX3=DBLE(X3)
         DY3=DBLE(Y3)
         SANG=REAL(DCANG(DX1,DY1,DX3,DY3))
         CR3 = .TRUE.
      ELSE
         SANG=MOD(EANG-ANGLE,PI(2.0))
C        negative angle created no worky
         IF(SANG.LT.0) SANG=SANG+PI(2.0)
      END IF
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     store the new arc
      CALL DEWC05(X1,Y1,RADIUS,SANG,EANG,CLFONT,CLAYER,P,OK)
C     erase crosses from screen
      IF( CR1 ) CALL CROSS(SX1,SY1)
      IF( CR2 ) CALL CROSS(SX2,SY2)
      IF( CR3 ) CALL CROSS(SX3,SY3)
      IF ( .NOT. OK ) THEN
         GOTO 605
      END IF
C     draw the arc just created
      C=CLFONT
      ENTYPE=ARC
      CALL ALLDRW(ENTYPE,P)
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
      FIRST=.FALSE.
      GOTO 10
C
 605   CONTINUE
C
C     Unload fixed Radius option
      CALL MNU01A()
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INS1C5()
C     ===================
C2       No arguments passed
C2    Draws an arc given centre point and one point
C2    on the circumference.The circumferential
C2    point defines the radius.
C2    selected by 'F' from arc option menu.
C2
C2    assumes at entry that the calling menu cell is hilited,
C2    at exit ensures that the calling menu cell is no longer
C2    hilited.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/arcdat.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 NEW,ENTYPE,P,FMIPOS
      INTEGER*4 C,TMEN,TCELL
      LOGICAL OK,FIRST
      REAL ZERO,DISTXY,X1,X2,Y1,Y2,RADIUS,X3,Y3,SANG,EANG,
     + SX1,SX2,SY1,SY2,SX3,SY3,ATAN,PI
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,
     +                 DDSTXY,DCANG,DZERO
      INTRINSIC DBLE,REAL,ATAN
      EXTERNAL DZERO,CROSS,GETANS,WO2SC,
     +         DDSTXY,DEWC05,DRWFAW,PI,
     1         MNLPTS,MNUPTS,GTMCLO,GTMCHI,GTMCWI,
     2         MNI01A,INS0AM,MNU01A
C
      FMIPOS=NMIPOS
      FIRST=.TRUE.
 
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C
C     write the current font number into the cell
      CALL MNISTD()
C     Load fixed Radius option
      CALL MNI01A()
C     Load the point modes to the option menu
      CALL MNLPTS()
C
 10   CONTINUE
C
      CALL DCPRNT(181)
      CALL GETANS(C,X1,Y1)
      DX1=DBLE(X1)
      DY1=DBLE(Y1)
      IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
      IF (MEN.EQ.2) GOTO 605
      IF (MEN.EQ.3) THEN
         IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
            GOTO 10
         END IF
         CALL INS0AM(OK)
         IF ( OK ) GOTO 10
         GOTO 605
      END IF
C     show first point on screen
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
C
 20   CONTINUE
C
      IF ( .NOT. RADSET ) THEN
         CALL DCPRNT(182)
         CALL GETANS(C,X2,Y2)
         DX2=DBLE(X2)
         DY2=DBLE(Y2)
         IF ( CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) GOTO 605
         IF (MEN.EQ.2) GOTO 605
         IF (MEN.EQ.3) THEN
            IF ( CCMD .EQ. 'c' ) THEN
               CALL INSCNL(FMIPOS,ARC,1,FIRST,OK)
               GOTO 20
            END IF
            CALL INS0AM(OK)
            IF ( OK ) GOTO 20
            GOTO 605
         END IF
C
         RADIUS=REAL(DZERO(DDSTXY(DX1,DY1,DX2,DY2)))
C
         IF ( RADIUS .EQ. 0.0 ) THEN
           CALL CROSS(SX1,SY1)
           CALL CROSS(SX2,SY2)
           CALL DEPRNT(94)
           GOTO 20
         END IF
      ELSE
C        fixed radius set use it
         RADIUS=ARCRAD
      END IF
      SANG=0.0
      EANG=PI(2.0)
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     store the new arc
      CALL DEWC05(X1,Y1,RADIUS,SANG,EANG,CLFONT,CLAYER,P,OK)
C     erase first point from screen
      CALL CROSS(SX1,SY1)
      IF ( .NOT. OK ) THEN
         GOTO 605
      END IF
C     draw the arc just created
      C=CLFONT
 
      ENTYPE=ARC
      CALL ALLDRW(ENTYPE,P)
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
 
C      CALL DRWFAW(X1,Y1,RADIUS,SANG,EANG,C)
      FIRST=.FALSE.
      GOTO 10
C
 605  CONTINUE
C
C     Unload fixed Radius option
      CALL MNU01A()
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE INSA01()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT ARC option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include 'include/menun.inc'
      include 'include/arcdat.inc'
C
      LOGICAL OK
      INTEGER CP,C,TMEN,TCELL
      INTEGER*2 FMIPOS
      REAL X,Y
C
      EXTERNAL TCURS,INS0A5,INS0C5,GTHFMC,
     +         GTMCHI,INS1A5,INS1C5
C
C
      COUNT = 0
C     Making single line the default insert text
      MEN=3
C     'F' is the token used by arc Centre Radius
      CCMD='F'
C     Find the menu and hightlite it
 12   CONTINUE
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 11
C
 103  continue
C
C     find type of arc required
      CALL TCURS(C,X,Y)
C
 11   CONTINUE
C     if another major option,return to INSERT control routine
      IF (MEN.EQ.2) GOTO 605
C     select type of arc entry required
      IF (MEN.EQ.3) THEN
C        arc construction options in here
 105     CONTINUE
         TMEN = MEN
         TCELL = CELLN
         IF (CCMD.EQ.'A' ) THEN
C           'A' Hit  'Arc Centre Two points on circumference'
C           the first being clockwise of the second
C            the real one
            CALL INS0A5()
         ELSE IF (CCMD.EQ.'B' ) THEN
C           'B' Hit  'Arc Centre Two points on circumference'
C           the first being counter-clockwise of the second
            CALL INS1A5()
         ELSE IF (CCMD.EQ.'G' ) THEN
C           (was C)'G' Hit  'Circle Three points on circumference'
            CALL INS0C5(.TRUE.)
         ELSE IF (CCMD.EQ.'g' ) THEN
C           (was C)'G' Hit  'Circle Three points on circumference'
            CALL INS0C5(.FALSE.)
         ELSE IF (CCMD.EQ.'F' ) THEN
C           (was D)'F' Hit  'Circle 1 point on circumference'
            CALL INS1C5()
         ELSE IF (CCMD.EQ.'O') THEN
C           'O' Hit ... 'Concentric circles rel'
            CALL INSCON()
         ELSE IF (CCMD.EQ.'o') THEN
C           'o' Hit ... 'Concentric circles abs'
            CALL INSCNA()
         ELSE IF (CCMD.EQ.'L' ) THEN
            CALL INSC3T()
         ELSE IF (CCMD.EQ.'H' ) THEN
C           Circle, center & TAN to 1 entity.
            CALL INSCT0()
         ELSE IF (CCMD.EQ.'X' ) THEN
C           Circle, radius & TAN to 2 entities.
            CALL INSRT0()
         ELSE IF (CCMD.EQ.'M' ) THEN
C           Circle, defined by picking diameter.
            CALL INSDC0()
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
C
         END IF
C     if menu3 returned,then stay in this loop
      CALL GTMCLO(TMEN,TCELL)
      IF (MEN.EQ.3) GOTO 105
C
      END IF
C
      IF (MEN.EQ.2) GOTO 605
C     test for quit character
      IF ( CCMD.EQ.'q') GOTO 605
C
C     go try again
      GOTO 103
C
 605  CONTINUE
C
      END
C
C----------------------------------------------------------------
C
C
      SUBROUTINE INSC3T()
C     ===================
C1    no arguments required
C2               
C2    This subroutine inserts a circle tangent to 3 entities.
C2
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
      include   'include/nbuff.inc'
      include   'include/viewport.inc'
C
      REAL X(3),Y(3),CENX,CENY,RAD,SANG,EANG,PI,MINDST,THSDST,DISTXY
      DOUBLE PRECISION LINES(4,3), ARCS(3,3), RESULT(3,8)
      INTEGER*2 ENT,P,FMIPOS,TMIP
      INTEGER*4 C,I,J,PLST(3),LINCNT,ARCCNT,NUMRES,MNUM,FLG,CELL
      CHARACTER*40 DUDVAR
      LOGICAL OK,MOK,GOTPNT,ONLY1,LIN(3),CVERFY,FIRSTC,FIRST
      EXTERNAL PI,DISTXY,INSATT,CVERFY
C
C     NOTE:  LINES(1,) = Point on line, X value.
C            LINES(2,) = Point on line, Y value.
C            LINES(3,) = Line vector, X value.
C            LINES(4,) = Line vector, Y value.
C            ARCS(1,) = Center , X value.
C            ARCS(2,) = Center , Y value.
C            ARCS(3,) = Radius.
C            RESULT(1,) = Center , X value.
C            RESULT(2,) = Center , Y value.
C            RESULT(3,) = Radius. 
C            LIN(n) = Was the nTH pick a line. (ie or an arc).
C
C     ensure the caller cell is hilited
      CALL GTMCHI(MEN,CELLN)
C
C     Enable the points mode.
      CALL MNLPTS()
C                                                     
      FIRSTC = .TRUE.
      ONLY1 = .TRUE.
      FMIPOS = NMIPOS
      GOTPNT = .FALSE.       
      SANG = 0.0
      EANG = PI(2.0)
      PLST(1) = 222
      PLST(2) = 223
      PLST(3) = 600
      CALL NOSRCH()
      ENT = ARC
      CALL ADSRCH(ENT)
      ENT = LINE
      CALL ADSRCH(ENT)
C     set this cos it will be checked by this lamentable 
C     code before it is set
      MINDST=0
 10   CONTINUE
C                      
      FIRSTC = .TRUE.
      LINCNT = 0
      ARCCNT = 0
      I = 1
C     This should be a do-loop but CANCEL wouldn,t be able to
C     force an extra round.
 20   continue
C        Get the point.
 30      CONTINUE 
         IF(.NOT.GOTPNT) THEN
            CALL DCPRNT(PLST(I))
            CALL GETANS(C,X(I),Y(I))
         ELSE
            GOTPNT = .FALSE.
         ENDIF
C        A menu hit? - leave.
         IF(MEN.NE.0) GOTO 100
C        Is there an entity at this point?
         CALL DSE800(X(I),Y(I),OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(142)
            GOTO 30
         ENDIF
C        Get the data and store it in the correct parameter list.
         CALL DIR500(MIP,OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(77)
            GOTO 30
         ENDIF
         CALL DBR500(IMBUFF(7),OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(61)
            GOTO 30
         ENDIF
         IF (IMBUFF(2).EQ.LINE) THEN
            LINCNT = LINCNT + 1
            LINES(1,LINCNT) = DBLE(RDBUFF(1))
            LINES(2,LINCNT) = DBLE(RDBUFF(2))
            LINES(3,LINCNT) = DBLE(RDBUFF(4) - RDBUFF(1))
            LINES(4,LINCNT) = DBLE(RDBUFF(5) - RDBUFF(2))
            LIN(I) = .TRUE.
         ELSE
            ARCCNT = ARCCNT + 1
            ARCS(1,ARCCNT) = DBLE(RDBUFF(1))
            ARCS(2,ARCCNT) = DBLE(RDBUFF(2))
            ARCS(3,ARCCNT) = DBLE(RDBUFF(4))
            LIN(I) = .FALSE.
         ENDIF
C
C     End of do_loop.
      I = I + 1
      IF (I .LE.3) GOTO 20
C
C     Calculate all possible answers.
      IF (LINCNT.EQ.0) THEN
C        All 3 entities are arcs.
         CALL CCT3C(ARCS(1,1),ARCS(2,1),ARCS(3,1),ARCS(1,2),
     +   ARCS(2,2),ARCS(3,2),ARCS(1,3),ARCS(2,3),ARCS(3,3),
     +   RESULT(1,1),RESULT(2,1),RESULT(3,1),
     +   RESULT(1,2),RESULT(2,2),RESULT(3,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(3,3),
     +   RESULT(1,4),RESULT(2,4),RESULT(3,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(3,5),
     +   RESULT(1,6),RESULT(2,6),RESULT(3,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(3,7),
     +   RESULT(1,8),RESULT(2,8),RESULT(3,8),NUMRES,FLG)
      ELSE IF (LINCNT.EQ.1) THEN
C        Entities are 2 arcs, 1 line.
         CALL CCT2CL(ARCS(1,1),ARCS(2,1),ARCS(3,1),
     +   ARCS(1,2),ARCS(2,2),ARCS(3,2),
     +   LINES(1,1),LINES(2,1),LINES(3,1),LINES(4,1),
     +   RESULT(1,1),RESULT(2,1),RESULT(3,1),
     +   RESULT(1,2),RESULT(2,2),RESULT(3,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(3,3),
     +   RESULT(1,4),RESULT(2,4),RESULT(3,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(3,5),
     +   RESULT(1,6),RESULT(2,6),RESULT(3,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(3,7),
     +   RESULT(1,8),RESULT(2,8),RESULT(3,8),NUMRES,FLG)
      ELSE IF (LINCNT.EQ.2) THEN
C        Entities are 1 arc, 2 lines.
         CALL CCT2LC(LINES(1,1),LINES(2,1),LINES(3,1),LINES(4,1),
     +   LINES(1,2),LINES(2,2),LINES(3,2),LINES(4,2),
     +   ARCS(1,1),ARCS(2,1),ARCS(3,1),
     +   RESULT(1,1),RESULT(2,1),RESULT(3,1),
     +   RESULT(1,2),RESULT(2,2),RESULT(3,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(3,3),
     +   RESULT(1,4),RESULT(2,4),RESULT(3,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(3,5),
     +   RESULT(1,6),RESULT(2,6),RESULT(3,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(3,7),
     +   RESULT(1,8),RESULT(2,8),RESULT(3,8),NUMRES,FLG)
      ELSE IF (LINCNT.EQ.3) THEN
         CALL CCT3L(LINES(1,1),LINES(2,1),LINES(1,2),LINES(2,2),
     +   LINES(1,3),LINES(2,3),LINES(3,1),LINES(4,1),
     +   LINES(3,2),LINES(4,2),LINES(3,3),LINES(4,3),
     +   RESULT(1,1),RESULT(2,1),RESULT(3,1),
     +   RESULT(1,2),RESULT(2,2),RESULT(3,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(3,3),
     +   RESULT(1,4),RESULT(2,4),RESULT(3,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(3,5),
     +   RESULT(1,6),RESULT(2,6),RESULT(3,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(3,7),NUMRES,FLG)
      ELSE
         WRITE(10,*) '[INSC3T] ERROR ... LINCNT wrong.'
         CALL DCPRNT(643)
      ENDIF
C
      CALL UNFLAG(.TRUE.)
C
      IF (NUMRES.EQ.0 .OR. FLG.NE.0) THEN
         CALL DEPRNT(142)
         GOTO 10
      ENDIF
C
C     Pick the best fit.
      j = 0
      DO 40,I=1,NUMRES
         THSDST = DISTXY(X(1),Y(1),REAL(RESULT(1,I)),
     +                                        REAL(RESULT(2,I)) )
         THSDST = THSDST + DISTXY(X(2),Y(2),REAL(RESULT(1,I)),
     +                                        REAL(RESULT(2,I)) )
         THSDST = THSDST + DISTXY(X(3),Y(3),REAL(RESULT(1,I)),
     +                                        REAL(RESULT(2,I)) )
         IF (MINDST.GT.THSDST .OR. J.EQ.0) THEN
            MINDST = THSDST
            J = I
         ENDIF
 40   CONTINUE
C
C     Put the 'NEXT' option up.
      CALL GTDMEN(47,3)
      CALL DCPRNT(222)
C
C     Allow the user to select the fit required.
 50   CONTINUE
         ENT = ARC      
         CENX = REAL(RESULT(1,J))
         CENY = REAL(RESULT(2,J))
         RAD  = REAL(RESULT(3,J))
         IF (FIRSTC) THEN
C            Create the first one in the database properly 
             CALL DEWC05(CENX,CENY,RAD,SANG,EANG,CLFONT,CLAYER,TMIP,OK)
             IF (.NOT.OK) THEN
                CALL DEPRNT(616)
                RETURN
             ENDIF        
         ELSE
C            Modify the existing one but before we do that erase the old one
             VPMOV = .TRUE.
             CALL PENERS()
             CALL ALLDRW(ENT,TMIP)
             VPMOV = .FALSE.
             RDBUFF(1) = CENX
             RDBUFF(2) = CENY
             RDBUFF(4) = RAD
             RDBUFF(5) = SANG
             RDBUFF(6) = EANG

             CALL DEM500(TMIP,OK)
             IF (.NOT.OK) THEN
                 CALL DEPRNT(616)
                 RETURN
             ENDIF        
         ENDIF

C        Set pen colour and then draw it 
C        If created then add else mov to be used
         IF (FIRSTC) THEN
             VPADD = .TRUE.
         ELSE 
             VPMOV = .TRUE.
         ENDIF

         CALL PENDRW()     
         CALL ALLDRW(ENT,TMIP)
                                                
C        Same as comment above 
         IF (FIRSTC) THEN
             VPADD = .FALSE.
         ELSE 
             VPMOV = .FALSE.
         ENDIF
         FIRSTC = .FALSE.                       

C        Now get user input 
         CALL GETANS(C,X(1),Y(1))
C        Anything but "Next Circle" - leave..
C        The following code handles if the user changes the line type 
         IF(MEN.EQ.3 ) THEN
              IF (CVERFY(CCMD,'=fk')) THEN
C               *******************************************************
C               Change line attributes.                        *
C               *******************************************************
                CALL INSATT(CCMD)
C               Don't forget to un highlight the "Attribues" cell.
                CALL GTMCLO(MEN, CELLN)                                                
                GOTO 50
              ELSEIF( CCMD.EQ.'U') THEN
                J = J + 1
                IF (J.GT.NUMRES) J=1
                CALL GTMCLO(3,CELLN)
                GOTO 50
              ENDIF
         ENDIF             
C     Clear the 'NEXT' option.
      CALL GTDHFM(3,47,CELL)
      CALL GTMCLO(3,CELL)
      CALL GTCLRC(3,CELL)
C
C     Unless cancel, Now go store it.
      IF (MEN.NE.3 .OR. CCMD.NE.'c' ) THEN
         GOTPNT = .TRUE.
      ELSE       
C     Unhighlight the Highlit cancel cell
         CALL GTMCLO(3,19)
         GOTPNT = .FALSE.
         GOTO 100
      END IF
C     If nothing is pending, go around again.
      IF (MEN.EQ.0) GOTO 10
C
 100  CONTINUE             
C         sub options in here 
C
C******************************************************
C                       CANCEL                        *
C******************************************************
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel the last tangent entity.
C           First, unflag it.            
            CALL GTMCLO(3,19)
C           Cancel it out of the database 
            FIRST = .FALSE.
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
            GOTO 10
         END IF
C
      IF (CVERFY(CCMD,'=fk')) THEN
C*******************************************************
C       Change line attributes.                        *
C*******************************************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(MEN, CELLN)                                  
         GOTO 30
      END IF
C
      CALL UNFLAG(.TRUE.)
C
C     Un highlight the '3 ents' cell before leaving.
      CALL GTMCLO(3,10)
C     Disable the points mode.
      CALL MNUPTS()
C
      END
      SUBROUTINE INSCEN()
C     ===================
C
C2    This subroutine inserts a center_line entity into the drawing.
C               
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/inscen.inc'
      include 'include/viewport.inc'
C
      INTEGER*4 C,PNUM,POPNUM
      INTEGER*2 P,ENT,FMIPOS,CLTYPE,TDFP,P2
      REAL X1,Y1, X2,Y2, CENX,CENY,LINANG,MAJRAD,MNIRAD,PI,DISTXY,CANG
      LOGICAL OK,SAME,FIRST
      EXTERNAL PI,DISTXY,SAME,CANG
C
      FMIPOS=NMIPOS
      POPNUM = 12
      FIRST=.TRUE.
      ENT = CENLIN
C     Default type is a cross.
      CLTYPE = 1
C     Set search mask for arc and centre line.
      CALL NOSRCH()
      CALL ADSRCH(ARC)
      CALL ADSRCH(CENLIN)
C     Allow entities to be picked from groups.
      GSSTAT = 1
      CALL MNICEN()
      PNUM = 201
C
 10   CONTINUE
C     Here, quick! Nip out and get some user input.
      CALL DCPRNT(PNUM)         
      CALL GETANS(C,X1,Y1)
      IF (MEN.NE.0) THEN
C        Grief! What does he want to change now ?
         IF (MEN.EQ.3) THEN
            IF (CCMD.LE.CHAR(3)) THEN
C              Don't let him hit the 'cross/single' cell.
               CALL GTMCLO(3,5)
            ELSE IF (CCMD.EQ.'W') THEN
C              The 'Window' option.( Can only be picked here if the
C              center line type is 'Cross'.)
               CALL INSCN1()
               CALL GTMCLO(3,17)
               FIRST = .FALSE.
            ELSE
C              Go modify a parameter, cancel or whatever. Hurry back!
               CALL ICPRMS(CLTYPE,PNUM,FMIPOS,POPNUM,FIRST)
            ENDIF
            GOTO 10
         ENDIF
C        Alright. Go and do something else. See if I care.
         GOTO 999
      ENDIF
C
      IF (CLTYPE.EQ.1) THEN
C        Two center lines, in a cross, at the center of the circle
C        picked by the TCURS above.
         CALL DSE800(X1,Y1,OK)
         IF (.NOT.OK) THEN
C           Ain't no arc here twit.
            CALL DEPRNT(108)
            GOTO 10
         ENDIF
C        We can also use the curved portion of a PCD centre line.
         IF (IMBUFF(2).EQ.CENLIN) THEN
            IF (IDBUFF(4).EQ.3) THEN
C              PCD. Use the curve as the arc.
C              Gather the details.
               CENX = RDBUFF(1)
               CENY = RDBUFF(2)
               MAJRAD = RDBUFF(3)
               MNIRAD = MAJRAD
            ELSE
C              Ain't no arc here twit.
               CALL DEPRNT(108)
               CALL ZSFLAG(.TRUE.,OK)
               GOTO 10
            ENDIF
         ELSE
C           Gather the details.
            CENX = RDBUFF(1)
            CENY = RDBUFF(2)
            MAJRAD = RDBUFF(4)
            MNIRAD = MAJRAD
         ENDIF
C
C        Store The Center Line,
         CALL DEWC30(CENX,CENY, MAJRAD,MNIRAD, ROTANG, BRDRSZ,
     +               CLTYPE,CLINEF,P)
C        and draw it.
         VPADD = .TRUE.
         CALL ALLDRW(ENT,P)
         CALL UNFLAG(.TRUE.)
         FIRST = .FALSE.
      ELSE IF (CLTYPE.EQ.2) THEN
C        One center line, between two points. We have the first,
C        go get the second.
 20      CONTINUE
         CALL DCPRNT(251)         
         CALL GETANS(C,X2,Y2)
         IF (MEN.NE.0) THEN
C           Grief! What does he want to change now ?
            IF (MEN.EQ.3 .AND. CCMD.LE.CHAR(3)) THEN 
C              Don't let him hit the 'cross/single/PCD' cell.
               CALL GTMCLO(3,5)
               GOTO 20
            ELSE IF (MEN.EQ.3) THEN
               IF (CCMD.EQ.'c') THEN
C                 Cancel the point only.
                  CALL GTMCLO(3,19)
                  GOTO 10
               ENDIF
C              Go modify a parameter.
               CALL ICPRMS(CLTYPE,PNUM,FMIPOS,POPNUM,.FALSE.)
               IF (CLTYPE.NE.2) GOTO 10
               GOTO 20
            ENDIF
C           Alright. Go and do something else. See if I care.
            GOTO 999
         ENDIF
C        Calculate the details.
         CALL CC00P3(X1,Y1,X2,Y2,CENX,CENY)
         MAJRAD = DISTXY(X1,Y1,CENX,CENY)
         MNIRAD = 0.0
         LINANG = CANG(X1,Y1,X2,Y2)
C
C        Store The Center Line,
         CALL DEWC30(CENX,CENY, MAJRAD,MNIRAD, LINANG, BRDRSZ,
     +               CLTYPE,CLINEF,P)
C        and draw it.
         VPADD = .TRUE.
         CALL ALLDRW(ENT,P)
         FIRST = .FALSE.
      ELSE IF (CLTYPE.EQ.3) THEN
C        A PCD center line. We have the center, now get the arcs
C        needing the center lines.
C        Put the 'Widow' cell up.
         CALL GTDMEN(120,3)
C        Put the 'Accept' cell up.
         CALL GTDMEN(13,3)
C
C        Set search path to arc only.
         CALL NOSRCH()
         CALL ADSRCH(ARC)
C
 30      CONTINUE
            CALL DCPRNT(201)
            CALL GETANS(C,X2,Y2)
            IF (MEN.NE.0) THEN
C              Grief! What does he want to change now ?
               IF (MEN.NE.3) THEN
C                 Alright. Go and do something else. See if I care.
                  GOTO 999
               ELSE
                  IF (CCMD.LE.CHAR(3)) THEN 
C                    Don't let him hit the 'cross/single/PCD' cell.
                     CALL GTMCLO(3,5)
                  ELSE IF (CCMD.EQ.'W') THEN
C                    Window option.
                     CALL WINDOW(.TRUE.)
                     CALL GTMCLO(3,17)
                  ELSE IF (CCMD.EQ.CHAR(13)) THEN
C                    Accept option.
                     CALL INSCN2(X1,Y1)
                     FIRST = .FALSE.
C                    Remove the 'Widow'  and 'Accept cells. We'll put
C                    them up again when we need them.
                     CALL GTCLRC(3,17)
                     CALL GTCLRC(3,16)
C                    Put the centre line back into the search.
                     CALL ADSRCH(CENLIN)
                     GOTO 10
                  ELSE IF (CCMD.EQ.'c') THEN
C                    Cancel one arc in the list.
                     CALL ZSFLAG(.TRUE.,OK)
                     CALL GTMCLO(3,19)
                     IF (.NOT.OK) THEN
C                       Nothing is flagged, cancel center point.
C                       Remove the 'Widow'  and 'Accept cells. We'll put
C                       them up again when we need them.
                        CALL GTCLRC(3,17)
                        CALL GTCLRC(3,16)
C                       Put the centre line back into the search.
                        CALL ADSRCH(CENLIN)
                        GOTO 10
                     ENDIF
                  ELSE
C                    Go modify a parameter.
                     CALL ICPRMS(CLTYPE,PNUM,FMIPOS,POPNUM,.FALSE.)
                     IF (CLTYPE.NE.3) GOTO 10
                  ENDIF
               ENDIF
            ELSE
C              Calculate the details.
               CALL DSE800(X2,Y2,OK)
               IF (.NOT.OK) THEN
C                 Ain't no arc here twit.
                  CALL DEPRNT(108)
                  GOTO 30
               ENDIF
            ENDIF
         GOTO 30
      ENDIF
C
C     Ok. Let's go do it all again.
      GOTO 10
C
 999  CONTINUE
      CALL UNFLAG(.TRUE.)
C     unload the points mode
      CALL MNUPTS()
      END
C
C-----------------------------------------------------------------------
C
C
      SUBROUTINE INSCN1()
C     ===================
C
C2    This subroutine inserts a center_line 'Cross' entities into the
C2    drawing using the wondow option.
C               
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/inscen.inc'
C
      INTEGER*4 I
      INTEGER*2 P, ENT, CLTYPE, MP,TDFP,D1
      REAL BX,BY,  CENX,CENY, MAJRAD,MNIRAD
      LOGICAL OK
C
      CLTYPE = 1
      ENT = CENLIN
      CALL WINDOW(.TRUE.)
      IF (NDATA.GT.0) THEN
C        Crosses ... Don't wait for an accept, draw them now.
         DO 10 I=1,NDATA
C           read the entity from SWINDU.
            CALL RSCRF(I,MP,BX,BY,TDFP,D1)
            CALL DER500(MP,OK)
C
C           Gather the details.
            CENX = RDBUFF(1)
            CENY = RDBUFF(2)
            MAJRAD = RDBUFF(4)
            MNIRAD = MAJRAD
C
C           Store The Center Line,
            CALL DEWC30(CENX,CENY, MAJRAD,MNIRAD, ROTANG,
     +                  BRDRSZ,CLTYPE,CLINEF,P)
C           and draw it.
            CALL ALLDRW(ENT,P)
 10      CONTINUE
         CALL UNFLAG(.TRUE.)
      ELSE
         CALL DEPRNT(34)
      ENDIF
C
      END
C
      SUBROUTINE INSCN2(XCEN,YCEN)
C     ============================
C1    VARTYPE            R    R 
C1    IOSTATUS           I    I
C
C2    This subroutine inserts center_line 'PCD' entities into the
C2    drawing.
C               
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/inscen.inc'
      include 'include/viewport.inc'
C
      INTEGER*4 I
      INTEGER*2 CLTYPE,ENT,P, MP,TDFP,D1
      REAL BX,BY, XCEN,YCEN,LINANG,X1,Y1, DISTXY,CANG,
     +     MAJRAD,MNIRAD
      EXTERNAL PI,DISTXY,SAME,CANG
      LOGICAL OK
C
      CLTYPE = 3
      ENT = CENLIN
C
      IF (NDATA.NE.0) THEN
         DO 10 I=1,NDATA
C           Get each arc from SWINDU and create a pcd for it.
            CALL RSCRF(I,MP,BX,BY,TDFP,D1)
            CALL DER500(MP,OK)
C           Get the center point of the circle.
            X1 = RDBUFF(1)
            Y1 = RDBUFF(2)        
C           And the other stuff.
            MNIRAD = RDBUFF(4)
            MAJRAD = DISTXY(XCEN,YCEN,X1,Y1)
            LINANG = CANG(XCEN,YCEN,X1,Y1)
C
C           Store The Center Line,
            CALL DEWC30(XCEN,YCEN, MAJRAD,MNIRAD, LINANG, BRDRSZ,
     +                  CLTYPE,CLINEF,P)
C           and draw it.
            VPADD = .TRUE.
            CALL ALLDRW(ENT,P)
 10      CONTINUE
         CALL UNFLAG(.TRUE.)
      ELSE
         CALL DEPRNT(34)
      ENDIF
C
      END
      SUBROUTINE INSCNA()
C     ===================
C1    No arguments needed.
C2
C2    This subroutine draws arcs concentric (Absolute)  to an existing
C2    one selected by the user.
C2
      include  'include/masti.inc'
      include  'include/entity.inc'
      include  'include/menun.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 FMIPOS,SIDE,ENT
      INTEGER*4 TMEN,TCELL,C,ICOM,IMUL,INDX,CRCLS,ST,EN,I,JMP,MNUM,
     +          TPCELL,TPMEN
      REAL X1,Y1,CENTRX,CENTRY,RADIUS,SANG,EANG,RADINC,DIST,PI
      DOUBLE PRECISION DLENGT
      CHARACTER*80 INPL
      LOGICAL OK,FIRST,MOK,CVERFY
C
      INTEGER*4 NLEN
      REAL DISTXY
      INTRINSIC REAL,INT
      EXTERNAL GTMCHI,DCPRNT,TCURS,NOSRCH,ADSRCH,DSE801,DIR500,
     +         DPRMXP,NLEN,DISTXY,PI,CVERFY
C
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
C                              
      ENT = ARC
      FMIPOS=NMIPOS
      FIRST=.TRUE.
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C     enter the point modes to the option menu
      CALL MNLPTS()
C
C     Get the reference arc.
 10   CONTINUE
      CALL UNFLAG(.TRUE.)
      CALL DCPRNT(181)
      CALL GETANS(C,X1,Y1)
C     Hit another menu go find out what
      IF ( MEN .NE. 0 ) THEN
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel.
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
            GOTO 10
         END IF
         JMP = 1
         GOTO 300
      ENDIF
C      CALL NOSRCH()
C      CALL ADSRCH(ARC)
C      CALL DSE800(X1,Y1,OK)
C      IF ( OK ) THEN
CC        Recover from data base
C         CALL DIR500(MIP,OK)
C         CALL DBR500(IMBUFF(7),OK)
C         CENTRX = RDBUFF(1)
C         CENTRY = RDBUFF(2)
C         RADIUS = RDBUFF(4)
C         SANG = RDBUFF(5)
C         EANG = RDBUFF(6)
CC               
CC
CC        Now get the distances.
         SANG = 0.0
         EANG = PI(2.0)
         CENTRX = X1
         CENTRY = Y1
 100     CONTINUE
         CALL DPRMXP(613,INPL)
C         CALL ALLDRW(ENT,MIP)
         IF ( NLEN(INPL).EQ.0) THEN
C           switch off since user has returned zero length
C            CALL GTMCLO(TMEN,TCELL)
            GOTO 10
         END IF
C
C        Multpile circles?
         INDX = 1
50       CONTINUE
         ICOM=INDEX(INPL(INDX:),',')
         IF( (ICOM.GT.0) .AND. ((ICOM.LT.IMUL) .OR. (IMUL.EQ.0)) )THEN
C
C           Single increment then comma.
            EN = INDX + ICOM - 1
            CALL AEXPRN(INPL(INDX:(EN-1)),DLENGT,*200)
            RADIUS = REAL(DLENGT)
            CALL INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
            FIRST = .FALSE.
            INDX = EN + 1
            GOTO 50
C
         ELSE
C
C           Single new arc.
            CALL AEXPRN(INPL(INDX:),DLENGT,*200)
            RADIUS = REAL(DLENGT) 
            CALL INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
            FIRST = .FALSE.
C
         ENDIF
c      ELSE
cC        Couldn,t find any arc. Try again.
c         CALL DEPRNT(108)
c      ENDIF
C
      GOTO 10
C
 200  CONTINUE
      CALL DEPRNT(209)
      GOTO 100
C
 300  CONTINUE
C        process sub options in here 
C
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
         IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C           Change line attributes.  
C           ****************************
            TPMEN = MEN
            TPCELL = CELLN
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attributes" cell.
            CALL GTMCLO(TPMEN, TPCELL)                                  
            IF(JMP .EQ. 1) GOTO 10
         END IF
C
C
 350  CONTINUE
      CALL UNFLAG(.TRUE.)
      CALL GTMCLO(TMEN,TCELL)
      END
      SUBROUTINE INSCT0()
C     ===================
C1    no arguments required
C2               
C2    This subroutine inserts a circle which is tangential to an
C2    entity, by inputting the center point and selecting the 
C2    relavent entity.
C2
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
      include   'include/nbuff.inc'
C
      REAL DISTXY,PI ,X1,Y1 ,CENX,CENY ,RAD(2), SANG,EANG,
     +     SX1,SY1
      DOUBLE PRECISION XC,YC,DRAD(2),DCENX,DCENY,DX1,DY1,DX2,DY2
      INTEGER*2 TENT,ENT,P,FMIPOS,REMV
      INTEGER*4 C,NUMRES,FLG,I,CELL,JMP
      LOGICAL OK,FIRST,CVERFY
      INTRINSIC DBLE
      EXTERNAL DISTXY,PI,CVERFY
C
C     ensure the caller cell is hilited
      CALL GTMCHI(MEN,CELLN)
C                     
      FMIPOS=NMIPOS
      FIRST = .TRUE.
      SANG = 0.0
      EANG = PI(2.0)
      ENT = ARC
C     Enable the points mode.
      CALL MNLPTS()
C            
 10   CONTINUE
C     Get the center point.
      CALL DCPRNT(181)
      CALL GETANS(C,X1,Y1)
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
      IF(MEN.NE.0) THEN
        JMP=1
        GOTO 100
      ENDIF
C
 20   CONTINUE
      CENX = X1
      CENY = Y1
C     Get the tangent entity.
      CALL DCPRNT(584)
      CALL GETANS(C,X1,Y1)
C     Cancel.
      IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
         CALL GTMCLO(3,20)
         GOTO 10
      ENDIF
      IF(MEN.NE.0) THEN
        JMP=2
        GOTO 100
      ENDIF
      CALL NOSRCH()
      TENT = ARC
      CALL ADSRCH(TENT)
      TENT = LINE
      CALL ADSRCH(TENT)
      CALL DSE800(X1,Y1,OK)
      IF (.NOT.OK) THEN
         CALL DEPRNT(142)
         GOTO 20
      ENDIF
C
C     Work out the nearest point on the entity to the center 
C     point and, from that, the radius.
      CALL DIR500(MIP,OK)
      CALL DBR500(IMBUFF(7),OK)
      IF (IMBUFF(2).EQ.LINE) THEN
C        Tangent to a line.
         DX1 = DBLE(RDBUFF(1))
         DY1 = DBLE(RDBUFF(2))
         DX2 = DBLE(RDBUFF(4)) - DX1
         DY2 = DBLE(RDBUFF(5)) - DY1
         DCENX = DBLE(CENX)
         DCENY = DBLE(CENY)
         CALL CCTL(DCENX,DCENY,DX1,DY1,DX2,DY2,DRAD(1),NUMRES,FLG)
         RAD(1) = REAL(DRAD(1))
      ELSE
C        Tangent to an arc.
         DX1 = DBLE(RDBUFF(1))
         DY1 = DBLE(RDBUFF(2))
         DX2 = DBLE(RDBUFF(4))
         DCENX = DBLE(CENX)
         DCENY = DBLE(CENY)
         CALL CCTC(DCENX,DCENY,DX1,DY1,DX2,DRAD(1),DRAD(2),NUMRES,FLG)
         RAD(1) = REAL(DRAD(1))
         RAD(2) = REAL(DRAD(2))
      ENDIF
C
C     Delete the construction.
      CALL CROSS(SX1,SY1)    
      CALL UNFLAG(.TRUE.)
C
      IF (NUMRES.EQ.0 .OR. FLG.NE.0) THEN
         CALL DEPRNT(317)
         GOTO 10
      ENDIF
C
      I = 1
      IF (NUMRES .GT. 1) THEN
C        Allow the user to select which.
C
C        Put the 'NEXT' option up.
         CALL GTDMEN(47,3)
         CALL DCPRNT(222)
 50      CONTINUE
            CALL DRWARC(CENX,CENY,RAD(I),SANG,EANG)
            CALL GETANS(C,X1,Y1)
            CALL PENERS()
            CALL DRWARC(CENX,CENY,RAD(I),SANG,EANG)
            CALL PENDRW()  
C           Anything but "Next Circle" - leave..
            IF(MEN.EQ.3 ) THEN
              IF (CVERFY(CCMD,'=fk')) THEN
C               *******************************************************
C               Change line attributes.                        *
C               *******************************************************
                CALL INSATT(CCMD)
C               Don't forget to un highlight the "Attribues" cell.
                CALL GTMCLO(MEN, CELLN)                                                
                GOTO 50
              ELSEIF( CCMD.EQ.'U') THEN
                I = I + 1
                IF (I.GT.2) I=1
                CALL GTMCLO(3,CELLN)
                GOTO 50
              ENDIF
            ENDIF             
C        Clear the 'NEXT' option.
         CALL GTDHFM(3,47,CELL)
         CALL GTMCLO(3,CELL)
         CALL GTCLRC(3,CELL)
         IF (MEN.EQ.0) THEN
C           Use this point as the center of the next tangent.
            CALL WO2SC(X1,Y1,SX1,SY1)
            CALL CROSS(SX1,SY1)
         ENDIF
      ENDIF
C
C     Unless cancel, Now go store it.
      IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
         CALL GTMCLO(3,20)
         GOTO 10
      ELSE
C        Now go store it.                                    
         CALL DEWC05(CENX,CENY,RAD(I),SANG,EANG,CLFONT,CLAYER,P,OK)
C        Draw the circle.
         CALL ALLDRW(ENT,P)
         FIRST = .FALSE.
      END IF
C     If nothing is pending, go around again.
      IF (MEN.EQ.0) THEN
         IF (NUMRES.GT.1) GOTO 20
         GOTO 10
      ENDIF
C
 100  CONTINUE
      IF (MEN.EQ.3) THEN
         IF (CVERFY(CCMD,'=fk')) THEN
C           *******************************************************
C           Change line attributes.                        *
C           *******************************************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(MEN, CELLN)                                  
            IF(JMP .EQ. 1)  GOTO 10
            IF(JMP .EQ. 2)  GOTO 20          
         ELSEIF(CCMD.EQ.'c') THEN
C           Cancel.
            REMV = 1
            CALL INSCNL(FMIPOS,ENT,REMV,FIRST,OK)
            CALL GTMCLO(3,19)
            GOTO 10
         ENDIF
      ENDIF
C     Delete the construction cross.
      CALL CROSS(SX1,SY1)
C     Disable the points mode.
      CALL MNUPTS()
C
      END
C
      SUBROUTINE INSDC0()
C     ===================
C1    no arguments required
C2               
C2    This subroutine inserts a circle, by inputting two points
C2    to use as the diameter.
C2
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
C
      REAL DISTXY,PI ,X1,Y1 ,X2,Y2 ,CENX,CENY ,RAD, SANG,EANG,
     +     SX1,SY1
      INTEGER*2 ENT,P,FMIPOS,REMV
      INTEGER*4 C,JMP       
      LOGICAL OK,FIRST,CVERFY
      EXTERNAL DISTXY,PI,CVERFY
C
      FMIPOS=NMIPOS
      ENT = ARC
      FIRST=.TRUE.
      REMV = 1
C
C     ensure the caller cell is hilited
      CALL GTMCHI(MEN,CELLN)
C
C     Enable the points mode.
      CALL MNLPTS()
C            
 10   CONTINUE
C     Get the first point.
      CALL DCPRNT(183)
      CALL GETANS(C,X1,Y1)
      CALL WO2SC(X1,Y1,SX1,SY1)
      CALL CROSS(SX1,SY1)
      IF(MEN.NE.0) THEN
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel.
C           Delete the last circle.
            CALL INSCNL(FMIPOS,ENT,REMV,FIRST,OK)
            GOTO 10
         END IF
         JMP=1
         GOTO 100
      ENDIF
C
C     Get the second point.
 20   CONTINUE
      CALL DCPRNT(184)
      CALL GETANS(C,X2,Y2)
      IF(MEN.NE.0) THEN
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel.
C           Delete the construction cross.
            CALL CROSS(SX1,SY1)
            CALL GTMCLO(3,19)
            GOTO 10
         END IF
         JMP = 2
         GOTO 100
      ENDIF
C
C     Find the mid point (ie the center of the circle).
      CENX = (X1+X2) / 2.0d0
      CENY = (Y1+Y2) / 2.0d0
C
C     OK So what's the radius ?
      RAD = DISTXY(X1,Y1,CENX,CENY)
CCC      WRITE(10,*) '*** [INSDC0] *** X1,Y1  = (',X1,Y1,')'
CCC      WRITE(10,*) '                 X2,Y2  = (',X2,Y2,')'
CCC      WRITE(10,*) '                 CENTRE = (',CENX,CENY,')'
CCC      WRITE(10,*) '                 RADIUS = ',RAD
C 
C     Now go store it.                                    
      SANG = 0.0
      EANG = PI(2.0)
      CALL DEWC05(CENX,CENY,RAD,SANG,EANG,CLFONT,CLAYER,P,OK)
C     Delete the construction cross.
      CALL CROSS(SX1,SY1)
C     Draw the circle.
      CALL ALLDRW(ENT,P)
C
      FIRST = .FALSE.
C     OK. Now go back and start again.
      GOTO 10
C
 100  CONTINUE
      IF (CVERFY(CCMD,'=fk')) THEN
C        *******************************************************
C        Change line attributes.                        *
C        *******************************************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(MEN, CELLN)                                  
         IF(JMP .EQ. 1)  GOTO 10
         IF(JMP .EQ. 2)  GOTO 20          
      ENDIF
C     Disable the points mode.
      CALL MNUPTS()
C
      END
      SUBROUTINE INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
C     -------------------------------------------------
C1    VARTYPE             R      R       R    R     R
C1    IOSTATUS            I      I       I    I     I
C
C2    This routine puts the arc created in INSCON into the database
C2    and onto the screen.
C
      include  'include/masti.inc'
      include  'include/viewport.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P,ENT
      INTEGER*4 C
      REAL CENTRX,CENTRY,RADIUS,SANG,EANG
      LOGICAL OK
C
      EXTERNAL DEWC05, DRWFAW
C
      C = CLFONT     
      ENT = ARC
      IF(RADIUS .LE. 0.0) THEN
        CALL DEPRNT(94)
      ELSE
         CALL DEWC05(CENTRX,CENTRY,RADIUS,SANG,EANG,CLFONT,CLAYER,P,OK)
C        draw the arc just created
         C=CLFONT
         VPADD = .TRUE.
         CALL ALLDRW(ENT,P)
         VPADD = .FALSE.
         CALL DRWFAW(CENTRX,CENTRY,RADIUS,SANG,EANG,C)
      ENDIF
C
      END
C
      SUBROUTINE INSRT0()
C     ===================
C1    no arguments required
C2               
C2    This subroutine inserts a circle tangent to two entities,
C2    with a fixed radius.
C2
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
      include   'include/nbuff.inc'
      include   'include/arcdat.inc'
      include   'include/viewport.inc'

C
      REAL X(2),Y(2),CENX,CENY,RAD,SANG,EANG,PI,MINDST,THSDST,DISTXY
      DOUBLE PRECISION LINES(4,2), ARCS(3,2), RESULT(2,8),DPRAD
      INTEGER*2 ENT,P,FMIPOS,TMIP
      INTEGER*4 NLEN,C,I,J,PLST(2),LINCNT,ARCCNT,NUMRES,MNUM,FLG,CELL
      LOGICAL OK,MOK,GOTPNT,ONLY1,LIN(2),CVERFY,FIRSTC,FIRST
      EXTERNAL PI,DISTXY,NLEN,CVERFY
C
C     NOTE:  LINES(1,) = Point on line, X value.
C            LINES(2,) = Point on line, Y value.
C            LINES(3,) = Line vector, X value.
C            LINES(4,) = Line vector, Y value.
C            ARCS(1,) = Center , X value.
C            ARCS(2,) = Center , Y value.
C            ARCS(3,) = Radius.
C            RESULT(1,) = Center , X value.
C            RESULT(2,) = Center , Y value.
C            RESULT(3,) = Radius.
C
C     ensure the caller cell is hilited
      CALL GTMCHI(MEN,CELLN)
C
C     Enable the points mode.
      CALL MNLPTS()
C     Display the radius cell.
      CALL GTDMWR(115,3,ARCRAD,'(F8.2)')
      DPRAD = DBLE(ARCRAD)
C
      FIRSTC = .TRUE.
      ONLY1 = .TRUE.
      FMIPOS = NMIPOS
      GOTPNT = .FALSE.       
      SANG = 0.0
      EANG = PI(2.0)
      PLST(1) = 222
      PLST(2) = 223
      CALL NOSRCH()
      ENT = ARC
      CALL ADSRCH(ENT)
      ENT = LINE
      CALL ADSRCH(ENT)
C     set this cos it will be checked by this lamentable 
C     code before it is set
      MINDST=0
 10   CONTINUE
C
C     Get the two tangent entities.
      FIRSTC = .TRUE.
      LINCNT = 0
      ARCCNT = 0
      I = 1
C     This should be a do-loop but CANCEL wouldn,t be able to
C     force an extra round.
 20   continue
C        Get the point.
 30      CONTINUE 
         IF(.NOT.GOTPNT) THEN
            CALL DCPRNT(PLST(I))
            CALL GETANS(C,X(I),Y(I))
         ELSE
            GOTPNT = .FALSE.
         ENDIF
C        A menu hit? - leave.
         IF(MEN.NE.0) GOTO 100
C        Is there an entity at this point?
         CALL DSE800(X(I),Y(I),OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(142)
            GOTO 30
         ENDIF
C        Get the data and store it in the correct parameter list.
         CALL DIR500(MIP,OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(77)
            GOTO 30
         ENDIF
         CALL DBR500(IMBUFF(7),OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(61)
            GOTO 30
         ENDIF
         IF (IMBUFF(2).EQ.LINE) THEN
            LINCNT = LINCNT + 1
            LINES(1,LINCNT) = DBLE(RDBUFF(1))
            LINES(2,LINCNT) = DBLE(RDBUFF(2))
            LINES(3,LINCNT) = DBLE(RDBUFF(4) - RDBUFF(1))
            LINES(4,LINCNT) = DBLE(RDBUFF(5) - RDBUFF(2))
            LIN(I) = .TRUE.
         ELSE
            ARCCNT = ARCCNT + 1
            ARCS(1,ARCCNT) = DBLE(RDBUFF(1))
            ARCS(2,ARCCNT) = DBLE(RDBUFF(2))
            ARCS(3,ARCCNT) = DBLE(RDBUFF(4))
            LIN(I) = .FALSE.
         ENDIF
C
C     End of do_loop.
      I = I + 1
      IF (I .LE. 2) GOTO 20
C                                    
 111  CONTINUE
C     Calculate all possible answers.
      IF (LINCNT.EQ.0) THEN
C        Both entities are arcs.
         CALL CCT2C(DPRAD,ARCS(1,1),ARCS(2,1),ARCS(3,1),
     +   ARCS(1,2),ARCS(2,2),ARCS(3,2),
     +   RESULT(1,1),RESULT(2,1),RESULT(1,2),RESULT(2,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(1,4),RESULT(2,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(1,6),RESULT(2,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(1,8),RESULT(2,8),
     +   NUMRES,FLG)
      ELSE IF (LINCNT.EQ.1) THEN
C        Entities are 1 arc, 1 line.
         CALL CCTLC(DPRAD,LINES(1,1),LINES(2,1),LINES(3,1),
     +   LINES(4,1),ARCS(1,1),ARCS(2,1),ARCS(3,1),
     +   RESULT(1,1),RESULT(2,1),RESULT(1,2),RESULT(2,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(1,4),RESULT(2,4),
     +   RESULT(1,5),RESULT(2,5),RESULT(1,6),RESULT(2,6),
     +   RESULT(1,7),RESULT(2,7),RESULT(1,8),RESULT(2,8),
     +   NUMRES,FLG)
      ELSE IF (LINCNT.EQ.2) THEN
C        Entities are both lines.
         CALL CCT2L(LINES(1,1),LINES(2,1),LINES(3,1),LINES(4,1),
     +   LINES(1,2),LINES(2,2),LINES(3,2),LINES(4,2),DPRAD,
     +   RESULT(1,1),RESULT(2,1),RESULT(1,2),RESULT(2,2),
     +   RESULT(1,3),RESULT(2,3),RESULT(1,4),RESULT(2,4),
CCC     +   RESULT(1,5),RESULT(2,5),RESULT(1,6),RESULT(2,6),
CCC     +   RESULT(1,7),RESULT(2,7),RESULT(1,8),RESULT(2,8),
     +   NUMRES,FLG)
      ELSE
         WRITE(10,*) '[INSRT0] ERROR ... LINCNT wrong.'
         CALL DCPRNT(643)
      ENDIF
C
      CALL UNFLAG(.TRUE.)
C
      IF (NUMRES.LE.0 .OR. FLG.NE.0) THEN
         CALL DEPRNT(142)
         GOTO 10
      ENDIF
C
C     Pick the best fit.
      j = 0
      DO 40,I=1,NUMRES
         THSDST = DISTXY(X(1),Y(1),REAL(RESULT(1,I)),
     +                                        REAL(RESULT(2,I)) )
         THSDST = THSDST + DISTXY(X(2),Y(2),REAL(RESULT(1,I)),
     +                                        REAL(RESULT(2,I)) )
         IF (MINDST.GT.THSDST .OR. J.EQ.0) THEN
            MINDST = THSDST
            J = I
         ENDIF
 40   CONTINUE
C
C     Put the 'NEXT' option up.
      CALL GTDMEN(47,3)
      CALL DCPRNT(222)
C
C     Allow the user to select the fit required.
      RAD = DPRAD
 50   CONTINUE
         ENT = ARC      
         CENX = REAL(RESULT(1,J))
         CENY = REAL(RESULT(2,J))
         IF (FIRSTC) THEN
C            Create the first one in the database properly 
             CALL DEWC05(CENX,CENY,RAD,SANG,EANG,CLFONT,CLAYER,TMIP,OK)
             IF (.NOT.OK) THEN
                CALL DEPRNT(616)
                RETURN
             ENDIF        
         ELSE
C            Modify the existing one but before we do that erase the old one
             VPMOV = .TRUE.
             CALL PENERS()
             CALL ALLDRW(ENT,TMIP)
             VPMOV = .FALSE.
             RDBUFF(1) = CENX
             RDBUFF(2) = CENY
             RDBUFF(5) = SANG
             RDBUFF(6) = EANG

             CALL DEM500(TMIP,OK)
             IF (.NOT.OK) THEN
                 CALL DEPRNT(616)
                 RETURN
             ENDIF        
         ENDIF

C        Set pen colour and then draw it 
C        If created then add else mov to be used
         IF (FIRSTC) THEN
             VPADD = .TRUE.
         ELSE 
             VPMOV = .TRUE.
         ENDIF

         CALL PENDRW()     
         CALL ALLDRW(ENT,TMIP)

C        Same as comment above 
         IF (FIRSTC) THEN
             VPADD = .FALSE.
         ELSE 
             VPMOV = .FALSE.
         ENDIF
         FIRSTC = .FALSE.                       

C        Now get user input 
         CALL GETANS(C,X(1),Y(1))
C        Anything but "Next Circle" - leave..
C        The following code handles if the user changes the line type 
         IF(MEN.EQ.3 ) THEN
              IF (CVERFY(CCMD,'=fk')) THEN
C               *******************************************************
C               Change line attributes.                        *
C               *******************************************************
                CALL INSATT(CCMD)
C               Don't forget to un highlight the "Attribues" cell.
                CALL GTMCLO(MEN, CELLN)                                                
                GOTO 50
              ELSEIF( CCMD.EQ.'U') THEN
                J = J + 1
                IF (J.GT.NUMRES) J=1
                CALL GTMCLO(3,CELLN)
                GOTO 50
              ENDIF
         ENDIF             
C     Clear the 'NEXT' option.
      CALL GTDHFM(3,47,CELL)
      CALL GTMCLO(3,CELL)
      CALL GTCLRC(3,CELL)
C
C     Unless cancel, Now go store it.
      IF (MEN.NE.3 .OR. CCMD.NE.'c' ) THEN
         GOTPNT = .TRUE.
      ELSE
         CALL GTMCLO(3,19)
         GOTPNT = .FALSE.
         GOTO 100
      END IF
C     If nothing is pending, go around again.
      IF (MEN.EQ.0) GOTO 10
C
 100  CONTINUE             
C      sub options in here 
C******************************************************
C*                  CHANGE OF RADIUS                  *
C******************************************************
      IF ( CCMD .EQ. 'R' ) THEN
C        Now we need the radius of the tangent.
         CALL DPRMXP(186,CBUFF)
C
         IF ( NLEN(CBUFF) .NE. 0 ) THEN
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DPRAD,*111)
            CALL GTMCLO(MEN,CELLN)
            IF ( DPRAD .LE. 0.0 ) THEN
C              prompt user that zero radius is illegal, and try again
               CALL DEPRNT(187)
            ELSE
C              Store and display the new radius.
               ARCRAD = REAL(DPRAD)
C              Display the radius cell.
               CALL GTDMWR(115,3,ARCRAD,'(F8.2)')
            END IF
         END IF
         GOTPNT = .FALSE.
C        If we broke out of the 'next circle' loop, go to the start,
         IF (I.GT.2) GOTO 10
C        ... otherwise go for the next entity.
         GOTO 30
      END IF
C
C
C******************************************************
C                       CANCEL                        *
C******************************************************
      IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C        Cancel the last tangent entity.
C        First, unflag it.            
         CALL GTMCLO(3,CELLN)

C        Cancel it out of the database 
         FIRST = .FALSE.
         CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
         GOTO 10
      END IF
C
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      IF (CVERFY(CCMD,'=fk')) THEN
C    ****************************
C       Change line attributes.  
C    ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(MEN, CELLN)                                  
         GOTPNT = .FALSE.
C        If we broke out of the 'next circle' loop, go to the start,
         IF (I.GT.2) GOTO 10
C        ... otherwise go for the next entity.
         GOTO 30
      END IF
C
      CALL UNFLAG(.TRUE.)
C
C     Unload fixed Radius option
      CALL MNU01A()
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(MEN,CELLN)
      END
      SUBROUTINE MNI01A()
C     ===================
C1    no arguments needed
C2
      include  'include/arcdat.inc'
C
C2    This adds the fixed radius option to arc sub-menu
      INTEGER*4 I
      CHARACTER*16 OLIN,C*1
      EXTERNAL GTPMEN,GTMCHI
C
C2    S is the token for fixed radius
      CALL GTDMEN(114,3)
C     retrieve cell number used
      CALL FNDPOS(114,I)
      IF ( RADSET ) CALL GTMCHI(3,I)
C2    R is the token for RADIUS value
C     show current value
      CALL GTDMWR(115,3,ARCRAD,'(F8.2)')
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE MNI02A()
C     ===================
C1    no arguments needed
C2
      include  'include/arcdat.inc'
C
C2    This adds the fixed ANGLE option to arc sub-menu
      INTEGER*4 I
      REAL DEG
      CHARACTER*16 OLIN,C*1
      EXTERNAL DEG,GTPMEN,GTMCHI
C
C     retrieve cell position
      CALL FNDPOS(116,I)
      CALL GTCLRC(3,I)
C2    U is the token for LOCKED ANGLE
      CALL GTDMEN(116,3)
      IF ( ARCSET ) CALL GTMCHI(3,I)
C2    V is the token for ANGLE of ARC
C     show current condition
      CALL GTDMWR(117,3,DEG(ANGLE),'(F8.2)')
C
      END
C
C----------------------------------------------------------------
C
 
      SUBROUTINE MNIARC
C     =================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the INSERT ARC option list.
C2
C2
      EXTERNAL GTPMEN,GTCLRM
C
C     Clear minor option menu.
      CALL GTCLRM(3)
C
C     Enter the option available for arc input.
C2    O is the token for Concentric Rel.
      CALL GTDMEN(129,3)
C2    o is the token for Concentric Abs.
      CALL GTDMEN(530,3)
C2    F is the token for CIRC: CEN.RADIUS.
      CALL GTDMEN(110,3)
C2    G is the token for CIRC 3PTS CIRCUM
      CALL GTDMEN(111,3)
C2    g is the token for ARC 3PTS CIRCUM
      CALL GTDMEN(109,3)
C2    B is the token for ARC CEN 2PTS  CW.
      CALL GTDMEN(112,3)
C2    A is the token for ARC CEN 2PTS CCW.
      CALL GTDMEN(113,3)
C2    Pick diameter.
      CALL GTDMEN(105,3)
C2    Tan 3 ents.
      CALL GTDMEN(106,3)
C2    Tan 2 ents + rad.
      CALL GTDMEN(107,3)
C2    Center + 1 tangent.
      CALL GTDMEN(108,3)
C2    i is the token for INTOF , not needed at this stage.
C
C     c is the token for cancel last input
      CALL GTDMEN(119,3)
C
      END
C
C----------------------------------------------------------------
C
C
C
      SUBROUTINE MNICEN()
C     ===================
C
C2    Center line modifier menu.
C
      REAL DEGANG,DEG
      CHARACTER*10 STRNG, TEMP*20
      EXTERNAL DEG, GTDMEN, GTDMWT
C
      include 'include/masti.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
      include 'include/inscen.inc'
      include  'include/style.inc'
C
C     t token for TYPE

      GTMULT=.TRUE.
C     default type is CROSS.
      TEMP = VNOUN(87)
      CALL GTDMWT(83,3,TEMP)
C     default type is CROSS.
C
C     b token for BORDER SIZE
      CALL GTDMEN(84,3)
C     And the size ...
      WRITE(STRNG,'(F8.3)') BRDRSZ
      CALL GTPMEN(STRNG,' ',3,8)
C
C     r token for ROTATION ANGLE
      CALL GTDMEN(85,3)
C     And the size ...
      DEGANG = DEG(ROTANG)
      WRITE(STRNG,'(F8.3)') DEGANG
      CALL GTPMEN(STRNG,' ',3,11)
C
C     W token for WINDOW
      CALL GTDMEN(120,3)
C     c token for CANCEL
      CALL GTDMEN(99,3)
C
C     Load the ATTRIBUTES cells.
C     k is the token for colour
      GTMULT = .TRUE.
      CALL GTDMWT(399,3,TXCLR)
C     f is the token for font  
      GTMULT = .TRUE.
      CALL GTDMWT(407,3,CLINET)
C     = is the token for thickness
      GTMULT = .TRUE.
      CALL GTDMWT(409,3,TXTHK)
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MNU01A()
C     ===================
C1    no arguments needed
C
      include 'include/vntable.inc'
C
      INTEGER I,J
      EXTERNAL GTCLRC
C
C2    clear cells containing
C2       FIXED RADIUS
C2       RADIUS:
C2       ANGLE LOCK
C2       ANGLE:
C
      DO 10 I=114,117
         J=VNPOS(I)
         CALL GTCLRC(3,J)
 10   CONTINUE
C
      END
*
