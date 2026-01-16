C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 movcopy.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ANIMOV(SPEED)
C     SUBROUTINE ARRINP(OK)
C     SUBROUTINE ARRINT()
C     SUBROUTINE MAJMV1()
C     SUBROUTINE MNIMOV
C     SUBROUTINE MNLM11()
C     SUBROUTINE MNLM12()
C     SUBROUTINE MOVARR(OK, CSRSET)
C     SUBROUTINE MOVL02(ENT,ALL)
C     SUBROUTINE MOVL03(DYNAM,ANIMAT,HOLDON,SPEED)
C     SUBROUTINE MOVL04(DYNAM,RET)
C     SUBROUTINE MOVL05(OK)
C     SUBROUTINE MOVMAS(CSRSET)
C     SUBROUTINE MV0002(RARRAY,M)
C     SUBROUTINE MV0003(RARRAY,M)
C     SUBROUTINE MV0005(RARRAY,M)
C     SUBROUTINE MV0030(RARRAY,M)
C     SUBROUTINE MV0033(M,OK)
C     SUBROUTINE MV0034(M)
C     SUBROUTINE MV0035(DIMTYP,M)
C     SUBROUTINE MV0043(RARRAY,M)
C     SUBROUTINE MV0046(RARRAY,M)
C     SUBROUTINE MV0050(PMIP,STRTP,ENDP)
C     SUBROUTINE MV0066(M,MC)
C     SUBROUTINE MV0085(RARRAY,M)
C     SUBROUTINE MV0166(M,MC)
C     SUBROUTINE MV0DIM(DIMTYP,M)
C     SUBROUTINE MVDIM0(M,DIMTYP,OK)
C     SUBROUTINE RSTDDT()
C     SUBROUTINE SAVDDT()
C     SUBROUTINE TYPARR(OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ANIMOV(SPEED)
C     ========================
C
C2    Subroutine ANIMOV generates a moving image of the
C2    selected entities,by linearly interpolating between
C2    the start and end points of the defined transformation.
C
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL ANIDAT(10,3),DXY1,DXYW,R1,R2,R3,TFM(3,3),TFM1(3,3)
      REAL DX,DY,DA,DS,OFFX,OFFY,OFFA,OFFS,SAVDAT(4,3),TFM2(3,3)
      INTEGER*4 SPEED,NSTEPS,I,J,NUMENT,NPASS
      LOGICAL ANIFLG(10),OK,REVERS,QUIT
      CHARACTER*1 KEY
      EXTERNAL ROPXOR,ROPREP,DRWBUF,SETTFM,I3M,SC2WO
      EXTERNAL MULT3M
C
      NUMENT=NDATA
      REVERS=.FALSE.
      NPASS=0
      DO 4 I=1,4
         SAVDAT(I,1)=REFDAT(I,1)
         SAVDAT(I,2)=REFDAT(I,2)
         SAVDAT(I,3)=REFDAT(I,3)
 4    CONTINUE
 1    CONTINUE
      CALL I3M(TFM1)
      CALL I3M(TFM2)
 3    CONTINUE
      NPASS=NPASS+1
      CALL I3M(TFM)
C     initialize offsets
      OFFX=0.0
      OFFY=0.0
      OFFA=0.0
      OFFS=0.0
C     save transformation data
      DO 5 I=1,10
         ANIDAT(I,1)=REFDAT(I,1)
         ANIDAT(I,2)=REFDAT(I,2)
         ANIDAT(I,3)=REFDAT(I,3)
         ANIFLG(I)=OPFLAG(I)
 5    CONTINUE
C     cancel mirror op
      OPFLAG(3)=.FALSE.
C     find increment in world space
      CALL SC2WO(0.0,0.0,R1,R2)
      CALL SC2WO(1.0,0.0,R3,R2)
      DXY1=ABS(R3-R1)
 7    CONTINUE
C     set incremental move in world space
      DXYW=DXY1*SPEED
      IF (OPFLAG(4)) THEN
C        translate in process use it to control movement
C        find distance left to travel on X
         R2=ANIDAT(4,1)-OFFX
C        find distance left to travel on Y
         R3=ANIDAT(4,2)-OFFY
C        find total distance to travel
         R1=SQRT(R2*R2+R3*R3)
C        find number of steps required for translation
         NSTEPS=NINT(R1/DXYW)
C        avoid negative or zero errors
         IF (NSTEPS.LE.0) NSTEPS=1
         DX=R2/NSTEPS
         DY=R3/NSTEPS
         DA=(ANIDAT(2,3)-OFFA)/NSTEPS
         DS=(ANIDAT(1,3)-1.0-OFFS)/NSTEPS
      ELSE IF (OPFLAG(2)) THEN
C        rotate in process,use it to control motion
C        find angle left to travel
         R2=ANIDAT(2,3)-OFFA
C        find number of steps required for rotation
         DXYW=3.14159/180*5*SPEED
         NSTEPS=ABS(NINT(R2/DXYW))
C        avoid negative or zero errors
         IF (NSTEPS.LE.0) NSTEPS=1
         DX=0.0
         DY=0.0
         DA=(ANIDAT(2,3)-OFFA)/NSTEPS
         DS=(ANIDAT(1,3)-1.0-OFFS)/NSTEPS
      ELSE IF (OPFLAG(1)) THEN
C        scale in process,use it to control motion
C        find scale left to travel
         R2=ANIDAT(1,3)-1.0-OFFS
C        find number of steps required for scaling
         DXYW=R2*0.01*SPEED
         NSTEPS=NINT(R2/DXYW)
C        avoid negative or zero errors
         IF (NSTEPS.LE.0) NSTEPS=1
         DX=0.0
         DY=0.0
         DA=0.0
         DS=R2/NSTEPS
      END IF
C     loop required number of times
      CALL ROPXOR()
C
      I=0
 10   CONTINUE
      I=I+1
      IF (I.LE.NSTEPS) THEN
C        set transform data for this step
         REFDAT(2,3)=I*DA+OFFA
         REFDAT(4,1)=I*DX+OFFX
         REFDAT(4,2)=I*DY+OFFY
         REFDAT(1,3)=I*DS+OFFS+1.0
C        erase data at current position
         CALL DRWBUF(TFM2)
C        get new transform
         CALL SETTFM(TFM)
         call MULT3M(TFM1,TFM,TFM2)
C        redraw data in new position
         CALL DRWBUF(TFM2)
C        test for interrupt
         CALL KBINTR(KEY,OK)
         IF (OK) THEN
C           find the key hit
            QUIT=(KEY.EQ.'Q' .OR. KEY.EQ.'q')
            IF (QUIT) GOTO 11
            IF (KEY.EQ.'R' .OR. KEY.EQ.'r') THEN
C              toggle reverse mode
               REVERS=.NOT.REVERS
               GOTO 10
            END IF
            J=ICHAR(KEY)
            IF (J.GT.48 .AND. J.LE.57) THEN
C              reset speed of transformation
               SPEED=J-48
               IF (SPEED.GT.1) SPEED=SPEED*SPEED/2
C              update offset
               OFFA=REFDAT(2,3)
               OFFX=REFDAT(4,1)
               OFFY=REFDAT(4,2)
               OFFS=REFDAT(1,3)
               GOTO 7
            END IF
         END IF
         GOTO 10
      END IF
C     now transform data to target position
C     retrieve transformation data
 11   CONTINUE
      DO 15 I=1,10
         REFDAT(I,1)=ANIDAT(I,1)
         REFDAT(I,2)=ANIDAT(I,2)
         REFDAT(I,3)=ANIDAT(I,3)
         OPFLAG(I)=ANIFLG(I)
 15   CONTINUE
C     update primary transform
      CALL SETTFM(TFM)
      CALL MULT3M(TFM1,TFM,TFM2)
      CALL I3M(TFM)
      CALL MULT3M(TFM2,TFM,TFM1)
C     TFM1 now contains total transform to this point
      CALL ROPREP()
      IF (QUIT) GOTO 99
C     update the position of the centre of rotation
      CALL NEWXY(ANIDAT(2,1),ANIDAT(2,2),REFDAT(2,1),REFDAT(2,2),TFM1)
C     update the position of the centre of scaling
      CALL NEWXY(ANIDAT(1,1),ANIDAT(1,2),REFDAT(1,1),REFDAT(1,2),TFM1)
      IF (REVERS) THEN
C        reverse transform the data
         IF (OPFLAG(1)) THEN
C           invert the scale factor
            REFDAT(1,3)=1/REFDAT(1,3)
         END IF
         IF (OPFLAG(2)) THEN
C           negate the angle of rotation
            REFDAT(2,3)=-REFDAT(2,3)
         END IF
         IF (OPFLAG(4)) THEN
C           negate the translation vector
            REFDAT(4,1)=-REFDAT(4,1)
            REFDAT(4,2)=-REFDAT(4,2)
         END IF
C        recover number of ents in buffer
         NDATA=NUMENT
C        go restart
         IF (REVERS .AND. MOD(NPASS,2).NE.0) GOTO 3
      END IF
 99   CONTINUE
      DO 100 I=1,4
         REFDAT(I,1)=SAVDAT(I,1)
         REFDAT(I,2)=SAVDAT(I,2)
         REFDAT(I,3)=SAVDAT(I,3)
 100  CONTINUE
      IF (REVERS) GOTO 1
C     ensure replace mode with current colour
      CALL ROPREP()
      CALL PENDRW()
C
      END
C
C---------------------------------------------------------------
C
      SUBROUTINE ARRINP(OK)
C     =====================
C1    vartype            L
C1    iostatus           O
C
C2    This function gets all the necessary
C2    information together to build an array
C2    Returns OK FALSE if the ARRAY info has
C2    not been set properly it only does this
C2    if the array type has been set
C
      CHARACTER CCBUFF*40
      DOUBLE PRECISION DN
      LOGICAL OK,RESPON
      INTEGER*4 NLEN
      REAL PI
      include 'include/movdat.inc'
      EXTERNAL MOVL05, AEXPRN, DPRMXP, PI, DEPRNT
            OK = .TRUE.
            IF(.NOT. ARRTYP) THEN
              CALL DEPRNT(598)
              GOTO 99
            ELSE
C             are the chosen elements to be part of the
C             the array
 222          CALL DPRMXP(590, CCBUFF)
              IF(RESPON(CCBUFF,1)) THEN
                MEMBER = .TRUE.
              ELSEIF(RESPON(CCBUFF,2)) THEN
                MEMBER = .FALSE.
              ELSE
                GOTO 99
              ENDIF
C
              IF (NLEN(CCBUFF).EQ.0 )THEN
C                user has returned zero length string
C                assume that he has change his mind and
C                return for input
C                and make ARRAY option unset
                 GOTO 99
              ENDIF
C
              IF(RADIAL) THEN
C                  Right it's a radial array let's get it's
C                  configuration user wants to set the number of
C                  elements prompt for new number
 333               CALL DPRMXP(591,CCBUFF)
C
                   IF (NLEN(CCBUFF).EQ.0 )THEN
C                    user has returned zero length string
C                    assume that he has change his mind and
C                    return for input
C                    and make ARRAY option unset
                     GOTO 99
                   ENDIF
C
C                  evaluate an arithmetic expression from the keyboard
                   CALL AEXPRN(CCBUFF,DN,*333)
                   ARRNO = DN
C
                   IF(.NOT. MEMBER) THEN
C                    user wants to set the radius of array
C                    prompt for new radius and return expression
 444                 CALL DPRMXP(592 ,CCBUFF)
C
                     IF (NLEN(CCBUFF).EQ.0 )THEN
C                      user has returned zero length string
C                      assume that he has change his mind and
C                      return for input
C                      and make ARRAY option unset
                       GOTO 99
                     ENDIF
C
C                    evaluate an arithmetic expression from the keyboard
                     CALL AEXPRN(CCBUFF,DN,*444)
                     ARRAD = DN
C
C                    user wants to set the start angle of a radial
C                    array
 454                 CALL DPRMXP(593 ,CCBUFF)
C
                     IF (NLEN(CCBUFF).EQ.0 )THEN
C                      user has returned zero length string
C                      assume that he has change his mind and
C                      return for input
C                      and make ARRAY option unset
                       GOTO 99
                     ENDIF
C
C                    evaluate an arithmetic expression from the keyboard
C                    and convert to radians
                     CALL AEXPRN(CCBUFF,DN,*454)
                     STANG = PI(1.0)*DN/180.0
                   ENDIF
C
C
                 ELSE
C                  So it's a rectangular array so lets
C                  get it's configuration
C                  user wants to set the number of elements
C                  prompt for number of ROWS and return expression
 555               CALL DPRMXP(594,CCBUFF)
C
                   IF (NLEN(CCBUFF).EQ.0 )THEN
C                    user has returned zero length string
C                    assume that he has change his mind and
C                    return for input and make  ARRAY option  unset
                     GOTO 99
                   ENDIF
C
C                  evaluate an arithmetic expression
C                  from the keyboard
                   CALL AEXPRN(CCBUFF,DN,*555)
                   ROWS = DN
C
C                  user wants to set the number of elements
C                  prompt for number of COLUMNS and return expression
 666               CALL DPRMXP(595 ,CCBUFF)
C
                   IF (NLEN(CCBUFF).EQ.0 )THEN
C                    user has returned zero length string
C                    assume that he has change his mind and
C                    return for input
C                    and make ARRAY type unset
                     GOTO 99
                   ENDIF
C
C                  evaluate an arithmetic expression from the
C                  keyboard
                   CALL AEXPRN(CCBUFF,DN,*666)
                   COLS = DN
C
C                  user wants to set the distance between ROWS
C                  prompt for the distance between ROWS
C                  the sign of this distance determines the
C                  orientation of the array
 777               CALL DPRMXP(596,CCBUFF)
C
                   IF (NLEN(CCBUFF).EQ.0 )THEN
C                    user has returned zero length string
C                    assume that he has change his mind and
C                    return for input
C                    and make ARRAY type unset
                     GOTO 99
                   ENDIF
C
C                  evaluate an arithmetic expression from the
C                  keyboard
                   CALL AEXPRN(CCBUFF,DN,*777)
                   ARRYD = DN
 
C                  user wants to set the distance between COLUMNS
C                  prompt for the distance between COLUMNS
C                  the sign of this distance determines the
C                  orientation of the array
 888               CALL DPRMXP(597,CCBUFF)
C
                   IF (NLEN(CCBUFF).EQ.0 )THEN
C                    user has returned zero length string
C                    assume that he has change his mind and
C                    return for input
C                    and make ARRAY type unset
                     GOTO 99
                   ENDIF
C
C                  evaluate an arithmetic expression from the
C                  keyboard
                   CALL AEXPRN(CCBUFF,DN,*888)
                   ARRXD = DN
                 ENDIF
C                right we have been successful so get some
C                screen input get the array position etc
C                from user and set the MOVMAS control flags
C
                 CALL MOVL05(OK)
                 IF(.NOT. OK) GOTO 99
            ENDIF
            RETURN
C
 99   CONTINUE
C     Right botched the array so return
      ARRON = .FALSE.
      OK = .FALSE.
      RETURN
      END
      SUBROUTINE ARRINT()
C     ====================
C
C1    no args
C
C2    This subroutine initialises the ARRAY
C2    control variables
C
C     set the ARRAY type control flag to FALSE
C     to ensure that he selects the rectangular
C     or radial option.
      include 'include/movdat.inc'
      ARRTYP = .FALSE.
C     set array option flag to FALSE
      ARRON =.FALSE.
C
C     set ARRAY type control values
C     set to a rectangular ARRAY and set ROTATED elements
C     flag for RADIAL ARRAY to FALSE
      ARRON =.FALSE.
      RADIAL = .FALSE.
      ROTAT  = .TRUE.
      BORDER = .TRUE.
C     set the flag to show that the chosen entities
C     are part of the array
      MEMBER = .TRUE.
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MAJMV1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the MOVE mode
C2    of operation is selected from the master menu.
C2    controls operation of the MOVE function
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL TCURS,GTMCLO,GTMCHI
      EXTERNAL CLRPEW,MNLM12,MNIMOV,
     +         GTHFMC,UNFLAG,GTCLRM,ARRINT
C
C     Now activate the MOVE major option menu
      CALL MNIMOV
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     initialize the move flags
      CALL CLROPF()
C     enable erase and draw of entities
      OPFLAG(9)=.TRUE.
      OPFLAG(10)=.TRUE.
C          
C     Initialise ARRAY control flags 
      CALL ARRINT                  
C     initialize text scale factor to 1
      REFDAT(7,3)=1
C     initialize number of copies to zero
      NNCOPY=0
C     set default target layer
      MNCELL(5,1)=CLAYER
C
C     initialize MOVE menu
      CALL MNLM12()
C     prompt for option select
      CALL DCPRNT(38)
C     Making single line the default insert text
      MEN=2
C     'a' is the token used by insert line
      CCMD='a'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C
C     Read a cursor hit to select MOVE mode
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
         IF (CCMD.EQ.'A') THEN
C           MOVE ARC option
            CALL MOVL02(ARC,.FALSE.)
         ELSE IF (CCMD.EQ.'a') THEN
C           MOVE ALL option
            CALL MOVL02(LINE,.TRUE.)
         ELSE IF (CCMD.EQ.'H') THEN
C           MOVE HATCH option
            CALL MOVL02(HATCH,.FALSE.)
         ELSE IF (CCMD.EQ.'G') THEN
C           MOVE HATCH option
            CALL MOVL02(GROUP,.FALSE.)
         ELSE IF (CCMD.EQ.'T') THEN
C           MOVE TEXT option
            CALL MOVL02(TEXT,.FALSE.)
         ELSE IF (CCMD.EQ.'L') THEN
C           MOVE LINE option
            CALL MOVL02(LINE,.FALSE.)
         ELSE IF (CCMD.EQ.'D') THEN
C           MOVE DIMENSION option
            CALL MOVL02(LDIMN,.FALSE.)
         ELSE IF (CCMD.EQ.'C') THEN
C           MOVE CURVE option
            CALL MOVL02(SPLINE,.FALSE.)
         ELSE IF (CCMD.EQ.'c') THEN
C           MOVE Compnent option
            CALL MOVL02(COMPI,.FALSE.)
         ELSE IF (CCMD.EQ.'s') THEN
C           MOVE Symbol option
            CALL MOVL02(SYMBI,.FALSE.)
         ELSE IF (CCMD.EQ.'l') THEN
C           MOVE center line option
            CALL MOVL02(CENLIN,.FALSE.)
         ELSE
C           unrecognized delete option
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
         CALL DEPRNT(263)
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
      CALL CLROPF()
C
      RETURN
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MNIMOV
C     =================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the MOVE major options.
C2
C
      INTEGER*4 I
C
      EXTERNAL GTMCHI,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the MOVE major options.
C     Hilite the option header
      CALL GTDMHD(18,2)
C
C     Load the entity descriptors.
      I=2
C2    H is the token for HATCH.
      CALL GTDMEN(180,2)
C2    T is the token for TEXT.
      CALL GTDMEN(181,2)
C2    A is the token for ARC.
      CALL GTDMEN(182,2)
C2    L is the token for LINE.
      CALL GTDMEN(183,2)
C2    d is the token for DIMEN
      CALL GTDMEN(184,2)
C2    'a' is the token for ANY
      CALL GTDMEN(185,2)
C2        is the token for SYMBOL
      CALL GTDMEN(186,2)
C2        is the token for COMP
      CALL GTDMEN(187,2)
C2        is the token for GROUP
      CALL GTDMEN(188,2)
C2    'l' is the token for Center Line
      CALL GTDMEN(82,2)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MNLM11()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLM11 loads the
C2    options for use in move operations
C2    menu no3.
C2
C2    Tokens used here are W and CHAR(150).
C2
      INTEGER*4 I
C
      CHARACTER CHAR
C
      INTRINSIC CHAR
C                     
      include 'include/gtxt2.inc'
      EXTERNAL GTPMEN GTDMEN
C
C2    W is the token for INSIDE AREA.
      CALL GTDMEN(210,3)
C2    e is the token for EXCLUDE
C      CALL GTPMEN('Exclude','e',3,(2))
C
C     set initial position for loading menu
      I=4
C     show that the cell is a popup
      GTMULT = .TRUE.
C2    b is the token for ARRAY
      CALL GTDMEN(35,3)
C
C2    t is the token for TRANSLATE
      CALL GTDMEN(190,3)
C2    r is the token for rotation
      CALL GTDMEN(191,3)
C
C2    s is the token for scaling
      CALL GTDMEN(192,3)
C2    S is the token for scale all
      CALL GTDMEN(193,3)
C
C2    m is the token for mirror
      CALL GTDMEN(194,3)
C
C2    D is the token for DYNAMIC
      CALL GTDMEN(199,3)
C2    L is the token for LAYER
      CALL GTDMEN(195,3)
C
C2    M is the token for copies
      CALL GTDMEN(196,3)
C
C2    A is the token for ANIMATE
      CALL GTDMEN(211,3)
C
C2    H is the token for HOLD
      CALL GTDMEN(212,3)
C2    R is the token for stretch
      CALL GTDMEN(213,3)
C
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(197,3)
C
C2    CHAR(150) is token for ACCEPT.
      CALL GTDMEN(198,3)
C2
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MNLM12()
C     ===================
C
C2    Subroutine MNML12 loads menu 3
C2    with the options appropriate to
C2    the general MOVE major option.
C2    The number of copies is initialized
C2    to the number contained in NNCOPY
C@    which is within 'NDATA.INC'
C
      include 'include/ndata.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 I
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL GTHFMC,MNLM11,GTMCHI,GTMCWI
C
C     initialize MOVE LINE option menu
      CALL MNLM11()
C     ensure current number of copies is shown in menu
      CALL GTMCWI(3,'M',NNCOPY)
C     ensure target layer is correct default
      CALL GTMCWI(3,'L',MNCELL(5,1))
C     ensure copy status is correct
      IF(NNCOPY.GT.0) THEN
         COPYIT=.TRUE.
      ELSE
         COPYIT=.FALSE.
      END IF
C
C     Ensure menu hilites ar enabled
C     on current transformations.
C     Curent status us available from 'MOVDAT.INC'
      DO 20,I=1,4
         IF (OPFLAG(I)) THEN
            CALL GTMCHI(MNCELL(I,1),MNCELL(I,2))
         END IF
 20   CONTINUE
      IF (OPFLAG(5)) THEN
C        move layer is set
         CALL GTHFMC(3,'L',I)
C        hilite the cell
         CALL GTMCHI(3,I)
      END IF
      IF (OPFLAG(7)) THEN
C        scale text is set
         CALL GTHFMC(3,'S',I)
C        hilite the cell
         CALL GTMCHI(3,I)
      END IF
C
      END
C
C--------------------------------------------------------------------
C
C
C--------------------------------------------------------------------
C
 
 
 
      SUBROUTINE MOVARR(OK, CSRSET)
C     ============================
C
C1    vartype           L    L
C1    iostatus          O    I
C
C2    This function controls the construction of arrays
C2    within the MOVE major option the position of the
C2    ARRAY is set by a previous call to MOVL05 and is
C2    contained in the COMMON block ARRAY, CSRSET
C2    is passed to MOVMAS to control comp/sym master
C2    reset.
C
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/props.inc'
C
      LOGICAL OK, TCOPYT, CSRSET, YESOK, OPTION, QUIT, CHECK,
     +TOOBIG,MENSAV
C
C
      INTEGER*4 TNCOPY, I, J, TNDATA, C, SIZE, TCOLS, TARRNO
C
      INTEGER*2 TNMIP, TNPDF, TNRLP, ENT, TMIP,
     +          TNTXP, TNPRP, TNPCP
C
      CHARACTER*3 ANS
C
      REAL ORIGDX, ORIGDY, ANGLE, TANGLE, COS, SIN,
     +PI, HPX, HPY
C
      INTRINSIC COS, SIN
C
      EXTERNAL TCURS,CPRMXP,YESOK,SAVMOV,RSTMOV,
     +ALLDRW,PENERS,PENDRW,MOVMAS,PI,FINDP0,BELL,
     1DEPRNT,GTCLRM,GTDMEN,SAVEMN,RSTMEN,REGEND
 
C
C     set FINDP0 return flags 
      OPTION = .FALSE.
      QUIT = .FALSE. 
C     flag to ensure that check for sufficient
C     remaining space in the database to hold
C     array
      CHECK = .TRUE.
C     flag to ensure that database pointers
C     are rewound  on exit if the array was 
C     too large
      TOOBIG = .FALSE.
C     flag to show that menus have been saved and
C     must be restored
      MENSAV = .FALSE.
C
C     let's just check that the ARRAY is not a monster
C     i.e. > 2000 entities
C
C     get total No of entities
C
      IF(RADIAL) THEN
         SIZE = ARRNO * NDATA
      ELSE
         SIZE = ROWS * COLS * NDATA
      ENDIF
C
      IF(SIZE .GT. 2000) THEN
        CALL DPRMXP(656,ANS)
        IF(.NOT. YESOK(ANS)) THEN
           OK = .FALSE.
           RETURN
        ENDIF
      ENDIF
C
C     right lets save all the database pointers
C     in case the man's ARRAY is a bit of a
C     disaster
      TNMIP = NMIPOS
      TNPDF = NPDPOS
      TNRLP  = NRLPOS
      TNTXP  = NTXPOS
      TNPRP  = NPRPOS
      TNPCP  = NPCPOS
C
C
C
C
C
C     right let's save all the OPFLAGS & REFDAT
C
      CALL SAVMOV()
C
C     set the No of copies to one
C
      TNCOPY = NNCOPY
      NNCOPY = 1
      TCOPYT = COPYIT
      COPYIT = .TRUE.
C     save NDATA for reassignment as ndata
C     will be reduced to zero by each call
C     to MOVMAS
C
      TNDATA = NDATA
C     save the distance of the origin of the
C     geometric array from the original
C     construction element
C
      ORIGDX = ARRPOS(1) - ELECEN(1)
      REFDAT(4,1) = ORIGDX
      ORIGDY = ARRPOS(2) - ELECEN(2)
      REFDAT(4,2) = ORIGDY
C
      IF(.NOT. RADIAL .AND. .NOT. BORDER) THEN
C*********************************************************
C         RECTANGULAR OPTION
C*********************************************************
C     right it's a rectangular array
C     set the opflags for a rectangular array
        OPFLAG(1) = .FALSE.
        OPFLAG(2) = .FALSE.
        OPFLAG(3) = .FALSE.
        OPFLAG(4) = .TRUE.
        OPFLAG(8) = .TRUE.
        OPFLAG(10) = .TRUE.
        OPFLAG(11) = .FALSE.
C
        IF(MEMBER) THEN
          TCOLS = COLS - 1
C         make a rect array with the instanced entities
C         as the cornerstone element
          DO 18 I=1,TCOLS
C           reset the value of NDATA
            NDATA = TNDATA
C           move to next column
            REFDAT(4,1) = REFDAT(4,1) + ARRXD
C           draw the element
            CALL MOVMAS(CSRSET)
C           right if it is the first call to MOVMAS then check 
C           that there is space in the database for the rest of
C           the array
            IF(CHECK) THEN
              IF(((NPDPOS-TNPDF)*ROWS*COLS+TNPDF) .GE. PDPLIM) THEN
C               it will blow up the database 
C               right get down and erase the junk that 
C               the single call to movmas has put into
C               the database
                TOOBIG = .TRUE.                                
                GOTO 418
              ELSE
                CHECK = .FALSE.
              ENDIF
            ENDIF
 18       CONTINUE
C         right so that's the first row done
C         move to next row
C         set position back to first column
          REFDAT(4,1) = ORIGDX
          REFDAT(4,2) = REFDAT(4,2) + ARRYD
C         reset the value of NDATA
          NDATA = TNDATA
C
C
C         Right do the rest of the arrays
          DO 16 I=2,ROWS
C            go along a rows
             DO 116 J=1,COLS
C               reset the value of NDATA
                NDATA = TNDATA
C
C               draw the element
                CALL MOVMAS(CSRSET)
C               move to next column
                REFDAT(4,1) = REFDAT(4,1) + ARRXD
 116         CONTINUE
C            set position back to first column
             REFDAT(4,1) = ORIGDX
C            move to next row
             REFDAT(4,2) = REFDAT(4,2) + ARRYD
 16       CONTINUE
C
        ELSE
C so the chosen entities are not part of the array
C
          DO 14 I=1,ROWS
C           go along a rows
            DO 114 J=1,COLS
C             reset the value of NDATA
              NDATA = TNDATA
C
C             draw the element
              CALL MOVMAS(CSRSET)
C             right if it is the first call to MOVMAS then check 
C             that there is space in the database for the rest of
C             the array
              IF(CHECK) THEN
                IF(((NPDPOS-TNPDF)*ROWS*COLS+TNPDF) .GE. PDPLIM) THEN
C                 it will blow up the database 
C                 right get down and erase the junk that 
C                 the single call to movmas has put into
C                 the database
                  TOOBIG = .TRUE.                                
                  GOTO 418
                ELSE
                  CHECK = .FALSE.
                ENDIF
              ENDIF
C             move to next column
              REFDAT(4,1) = REFDAT(4,1) + ARRXD
 114        CONTINUE
C           set position back to first column
            REFDAT(4,1) = ORIGDX
C           move to next row
            REFDAT(4,2) = REFDAT(4,2) + ARRYD
 14       CONTINUE
        ENDIF
      ELSEIF(.NOT. RADIAL .AND. BORDER) THEN
C*********************************************************
C         RECTANGULAR BORDER
C*********************************************************
C     right it's a rectangular border
C     set the opflags for a rectangular border
         OPFLAG(1) = .FALSE.
         OPFLAG(2) = .FALSE.
         OPFLAG(3) = .FALSE.
         OPFLAG(4) = .TRUE.
         OPFLAG(8) = .TRUE.
         OPFLAG(10) = .TRUE.
         OPFLAG(11) = .FALSE.
C
C       right fix whether the instanced element
C       is going to be part of the array
        IF(MEMBER) THEN
          TCOLS  = COLS - 1
          REFDAT(4,1) = REFDAT(4,1) + ARRXD
        ELSE
          TCOLS  = COLS
        ENDIF
C       do the base row
          DO 115 J=1,TCOLS
C           reset the value of NDATA
            NDATA = TNDATA
C
C           draw the element
            CALL MOVMAS(CSRSET)
C           right if it is the first call to MOVMAS then check 
C           that there is space in the database for the rest of
C           the array
            IF(CHECK) THEN
              IF(((NPDPOS-TNPDF)*(2*COLS+2*(ROWS-2))+TNPDF) 
     +          .GE. PDPLIM) THEN
C               it will blow up the database 
C               right get down and erase the junk that 
C               the single call to movmas has put into
C               the database
                TOOBIG = .TRUE.                                
                GOTO 418
              ELSE
                CHECK = .FALSE.
              ENDIF
            ENDIF
C           move to next column
            REFDAT(4,1) = REFDAT(4,1) + ARRXD
 115      CONTINUE
C
C           reset the value of NDATA
            NDATA = TNDATA
C
C         do the the side columns
          DO 215 J=1, ROWS-2
C           move to next row
            REFDAT(4,2) = REFDAT(4,2) + ARRYD
C           set position back to first column
            REFDAT(4,1) = ORIGDX
C           draw the element
            CALL MOVMAS(CSRSET)
C           reset the value of NDATA
            NDATA = TNDATA
C           set position  to last column
            REFDAT(4,1) = ORIGDX + ARRXD * (COLS-1)
C           draw the element
            CALL MOVMAS(CSRSET)
C           reset the value of NDATA
            NDATA = TNDATA
 215      CONTINUE
C
C         move to top row
          REFDAT(4,2) = ORIGDY + ARRYD * (ROWS-1)
C         set position back to first column
          REFDAT(4,1) = ORIGDX
C         do the top row
          DO 315 J=1,COLS
C           reset the value of NDATA
            NDATA = TNDATA
C
C           draw the element
            CALL MOVMAS(CSRSET)
C           move to next column
            REFDAT(4,1) = REFDAT(4,1) + ARRXD
 315      CONTINUE
      ELSEIF(RADIAL .AND. ROTAT) THEN
C*********************************************************
C      RADIAL AND ROTATED OPTION
C*********************************************************
C
C      set the rotation angle
       ANGLE = PI(2.0)/ARRNO
C
        IF(MEMBER) THEN
C         set the rotation origin and angle
          REFDAT(2,1) = ARRPOS(1)
          REFDAT(2,2) = ARRPOS(2)
          REFDAT(2,3) = ANGLE
C
C         set the opflags
          OPFLAG(1) = .FALSE.
          OPFLAG(2) = .TRUE.
          OPFLAG(3) = .FALSE.
          OPFLAG(4) = .FALSE.
          OPFLAG(8) = .TRUE.
          OPFLAG(10) = .TRUE.
          OPFLAG(11) = .FALSE.
C
C         set No of translations
          TARRNO = ARRNO-1
        ELSE
C         the element is not a member of the array
C         set the rotation origin and angle
C
C
          REFDAT(2,1) = ELECEN(1) - ARRAD
          REFDAT(2,2) = ELECEN(2)
          REFDAT(2,3) = STANG
C         set the translation
          REFDAT(4,1) = ORIGDX + ARRAD
          REFDAT(4,2) = ORIGDY
C
C
C         set the opflags
          OPFLAG(1) = .FALSE.
          OPFLAG(2) = .TRUE.
          OPFLAG(3) = .FALSE.
          OPFLAG(4) = .TRUE.
          OPFLAG(8) = .TRUE.
          OPFLAG(10) = .TRUE.
          OPFLAG(11) = .FALSE.
C
C         set No of translations
          TARRNO = ARRNO
        ENDIF
C
C      right lets get moving
       DO 214 I =1, TARRNO
C        reset value of NDATA
         NDATA = TNDATA
C        draw the element
         CALL MOVMAS(CSRSET)
C        right if it is the first call to MOVMAS then check 
C        that there is space in the database for the rest of
C        the array
         IF(CHECK) THEN
           IF(((NPDPOS-TNPDF)*TARRNO+TNPDF) .GE. PDPLIM) THEN
C            it will blow up the database 
C            right get down and erase the junk that 
C            the single call to movmas has put into
C            the database
             TOOBIG = .TRUE.                                
             GOTO 418
           ELSE
             CHECK = .FALSE.
           ENDIF
         ENDIF
C        increment the angle of rotation
         REFDAT(2,3) = REFDAT(2,3) + ANGLE
 214   CONTINUE
      ELSEIF(RADIAL) THEN
C*********************************************************
C            RADIAL UNROTATED
C*********************************************************
C
C      set the rotation angle
       ANGLE = PI(2.0)/ARRNO
C
C      set the opflags
       OPFLAG(1) = .FALSE.
       OPFLAG(2) = .FALSE.
       OPFLAG(3) = .FALSE.
       OPFLAG(4) = .TRUE.
       OPFLAG(8) = .TRUE.
       OPFLAG(10) = .TRUE.
       OPFLAG(11) = .FALSE.
C
       IF(MEMBER) THEN
         TANGLE = STANG + ANGLE
         TARRNO = ARRNO -1
C        right lets get moving
         DO 316 I =1, TARRNO
C           reset value of NDATA
            NDATA = TNDATA
C           get translation from element to centre of the array
C           and back to new position
            REFDAT(4,1) = COS(TANGLE)*ARRAD + ORIGDX
            REFDAT(4,2) = SIN(TANGLE)*ARRAD + ORIGDY
            TANGLE = TANGLE + ANGLE
C           draw the element
            CALL MOVMAS(CSRSET)
            IF(CHECK) THEN
              IF(((NPDPOS-TNPDF)*TARRNO+TNPDF) .GE. PDPLIM) THEN
C               it will blow up the database 
C               right get down and erase the junk that 
C               the single call to movmas has put into
C               the database
                TOOBIG = .TRUE.                                
                GOTO 418
              ELSE
                CHECK = .FALSE.
              ENDIF
            ENDIF
 316     CONTINUE
 
       ELSE
         TANGLE = STANG
C
C        right lets get moving
         DO 314 I =1, ARRNO
C          reset value of NDATA
           NDATA = TNDATA
C          get total translation from instance
C          to origin to element
           REFDAT(4,1) = ORIGDX + COS(TANGLE)*ARRAD
           REFDAT(4,2) = ORIGDY + SIN(TANGLE)*ARRAD
           TANGLE = TANGLE + ANGLE
C          draw the element
           CALL MOVMAS(CSRSET)
C           right if it is the first call to MOVMAS then check 
C           that there is space in the database for the rest of
C           the array
            IF(CHECK) THEN
              IF(((NPDPOS-TNPDF)*ARRNO+TNPDF) .GE. PDPLIM) THEN
C               it will blow up the database 
C               right get down and erase the junk that 
C               the single call to movmas has put into
C               the database
                TOOBIG = .TRUE.                                
                GOTO 418
              ELSE
                CHECK = .FALSE.
              ENDIF
            ENDIF
 314     CONTINUE
       ENDIF
C*********************************************************
C      ARRAY CHOICES FINISHED
C*********************************************************
      ENDIF
C
C     reset the global copy flags
C
      NNCOPY = TNCOPY
      COPYIT = TCOPYT
C
C     get back the original move global data
C 
      CALL RSTMOV()
C
C     turn off the ACCEPT menu cell that was hit
C     to invoke MOVARR
      CALL GTMCLO(MEN,CELLN)
C
C     now lets see if he is happy with his
C     tasty little array
C 
C     right save the menus 
C     set the flag
      MENSAV = .TRUE.
      CALL SAVEMN(3)
      CALL SAVEMN(2)
C     clear modifier menu and stick in 
C     CANCEL and ACCEPT
C
      CALL GTCLRM(3)
C
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(197,3)
C
C2    CHAR(150) is token for ACCEPT.
      CALL GTDMEN(198,3)
C
 414  CONTINUE
      CALL FINDP0(599, HPX, HPY, OPTION, QUIT)
 418  CONTINUE
      IF(CCMD .EQ. CHAR(149) .OR. TOOBIG) THEN
C        he wants to cancel the array
         CALL PENERS()
         DO 514 TMIP = TNMIP, NMIPOS-1
            CALL ALLDRW(ENT, TMIP)
 514     CONTINUE
         CALL PENDRW()
C        Reset the database pointers effectively deleting
C        the newly added array
         NMIPOS=TNMIP
         NPDPOS=TNPDF
         NRLPOS = TNRLP
         NTXPOS = TNTXP
         NPRPOS = TNPRP
         NPCPOS = TNPCP
C        regenerate display in case he has done
C        a ZOOM in 
         CALL REGEND()
         OK = .FALSE.
         CALL GTMCLO(MEN, CELLN)
         IF(TOOBIG) THEN
           WRITE(10,*) '[MOVARR] array abandoned insufficient space'
           CALL DEPRNT(601)
         ENDIF
C        go home
         GOTO 614
      ELSEIF(CCMD .EQ. CHAR(150)) THEN
C        show that the array move has been successful
         OK = .TRUE.
C        go home
         GOTO 614
      ELSEIF(OPTION .OR. QUIT) THEN
C        a non relevant menu cell has been hit
C        check to fix it's highlighting back
C        has it been highlighted
C        turn it off
         CALL GTMCLO(MEN, CELLN)
      ENDIF
C     has'nt chosen the correct menu hit so
C     bang the gong and tell him to ACCEPT or
C     CANCEL
      CALL BELL()
      GOTO 414
C
 614  CONTINUE
C     check if the menu has to be restored
      IF(MENSAV) THEN
        CALL GTCLRM(3)
        CALL GTCLRM(2)
        CALL RSTMEN(3)
        CALL RSTMEN(2)
      ENDIF
C
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MOVL02(ENT,ALL)
C     ==========================
C
C1    vartype            I2   L
C1    iostatus           I    I
C
C2    Subroutine MOVL02 handles all of the work
C2    within the move options.The selection of masks
C2    etc is passed to this routine from the caller
C2    and acted upon.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL X,Y
      REAL WX2,WY2
C
      INTEGER*4 C,TMEN,TCELL,I,PTXT,SPEED,PR
      INTEGER*2 MIPP,ENT,TENT
C
      LOGICAL OK,OPTION,QUIT,ALL,DYNAM,ANIMAT,HOLDON
C
C
      EXTERNAL MOVL03,NOSRCH,ADSRCH,FINDET,ALSRCH
C
      DYNAM=.FALSE.
      ANIMAT=.FALSE.
      HOLDON=.FALSE.
      ARRON =.FALSE.
      SPEED=4
      CALL NOSRCH()
C     set search status to disallow grouped entities
      GSSTAT=3
C
C     set search mask to correct entity types
      IF (ALL) THEN
C        allow searching for any entity type
         CALL ALSRCH()
C        set grouped entity search status to return
C        complete groups
         GSSTAT=2
         PTXT=333
      ELSE IF (ENT.EQ.LINE) THEN
C        enable line searching only
         CALL ADSRCH(LINE)
         PTXT=334
      ELSE IF (ENT.EQ.ARC) THEN
C        enable arc searching only
         CALL ADSRCH(ARC)
         PTXT=335
      ELSE IF (ENT.EQ.HATCH) THEN
C        enable hatch searching only
         CALL ADSRCH(HATCH)
         PTXT=336
      ELSE IF (ENT.EQ.CENLIN) THEN
C        enable centerline searching only
         CALL ADSRCH(CENLIN)
         PTXT=334
      ELSE IF (ENT.EQ.GROUP) THEN
C        enable hatch searching only
C         CALL ALSRCH()
         CALL ADSRCH(GROUP)
         GSSTAT=4
         PTXT=333
      ELSE IF (ENT.EQ.LDIMN) THEN
C        enable dimension searching only
         CALL ADSRCH(LDIMN)
         CALL ADSRCH(RDIMN)
         CALL ADSRCH(DDIMN)
         CALL ADSRCH(ADIMN)
         PTXT=337
      ELSE IF (ENT.EQ.TEXT) THEN
C        enable text searching only
         CALL ADSRCH(TEXT)
         PTXT=338
      ELSE IF (ENT.EQ.SPLINE) THEN
C        enable spline searching only
         CALL ADSRCH(SPLINE)
         PTXT=333
      ELSE IF (ENT.EQ.COMPI) THEN
C        enable spline searching only
         CALL ADSRCH(COMPI)
         PTXT=333
      ELSE IF (ENT.EQ.SYMBI) THEN
C        enable spline searching only
         CALL ADSRCH(SYMBI)
         PTXT=333
      END IF
C
 10   CONTINUE
C     find and flag an entity at hit point
      IF ( NDATA.GT.0 ) THEN
         PR=445
      ELSE
         PR=PTXT
      END IF
      CALL FINDET(PR,X,Y,MIPP,TENT,OPTION,QUIT)
C
      IF (QUIT) RETURN
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL MOVL03(DYNAM,ANIMAT,HOLDON,SPEED)
         IF ( CCMD.EQ.'q') RETURN
         IF ( MEN.EQ.2 ) RETURN
      END IF
C
      GOTO 10
C
 99   CONTINUE
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MOVL03(DYNAM,ANIMAT,HOLDON,SPEED)
C     ============================================
C
C1    vartype             L     L      L      I4
C1    iostatus            IO    IO     IO     IO
C
      include 'include/ndata.inc'
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/movdat.inc'
      include 'include/gtxt2.inc'
C
      REAL WX1,WY1,WX2,WY2,RAD,CANG,THETA
      REAL TFM(3,3)
      DOUBLE PRECISION DN
      INTEGER*4 TMEN,TCELL,C,NLEN,I,RET,INT,DYNMEN,DYNCEL,NHOLD
      INTEGER*4 SPEED
      LOGICAL CVERFY,OK,DYNAM,ANIMAT,HOLDON,OPTION,QUIT
      INTRINSIC CHAR,INT
      CHARACTER CCBUFF*40,OLIN*16,TEMP*8
      EXTERNAL GTMCLO,GTMCHI,MOVL04,
     1         WINDOW,MOVMAS,GTMCWI,DRAG,
     2         NLEN,CVERFY,AEXPRN,ZSFLAG 
     3         TYPARR,ARRINP
C
C     start option processinig here
 20   CONTINUE
C     process the independants first
C
      TMEN=MEN
      TCELL=CELLN
C
      IF ( CCMD .EQ. 'M' ) THEN
C***************************************************************
C              NUMBER  OF COPIES OPTION                        *
C***************************************************************
C        user wants to change the number of copies
C        prompt for new number and return expression
 111     CALL DPRMXP(264,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*111)
            NNCOPY=INT(DN)
C           ensure copy status is correct
            IF(NNCOPY.GT.0) THEN
               COPYIT=.TRUE.
            ELSE
               COPYIT=.FALSE.
            END IF
            CALL GTMCWI(3,CCMD,NNCOPY)
         END IF
         RETURN
      ELSE IF ( CCMD .EQ. 'L' ) THEN
C***************************************************************
C              LAYER OPTION                                    *
C***************************************************************
         IF (OPFLAG(5)) THEN
C           cancel layer option
            OPFLAG(5)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           user wants to change the target layer
C           prompt for new number and return expression
C           set flag to indicate layer enabled
            OPFLAG(5)=.TRUE.
 222        CALL DPRMXP(265,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0)THEN
C              user has returned zero length string
C              assume that default layer to be used
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*222)
C              save the target layer number
               IF ( DN .LT.0.OR.DN.GT.255 ) THEN
                  CALL DEPRNT(134)
                  GOTO 222
               END IF
               MNCELL(5,1)=INT(DN)
               CALL GTMCWI(3,CCMD,MNCELL(5,1))
            END IF
C           ensure cell hilited
            CALL GTMCHI(MEN,CELLN)
         END IF
         RETURN
C***************************************************************
C                OOPS OPTION                                   *
C***************************************************************
C     if backspace char,remove last entity from delete list
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
         TMEN=MEN
         TCELL=CELLN
C        clear the last entity flag in buffer
         CALL ZSFLAG(.FALSE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C
C***************************************************************
      ELSE IF (CVERFY(CCMD,'Strsm')) THEN
C***************************************************************
C        must be scale,translate,mirror,rotate option or scale text
         CALL MOVL04(DYNAM,RET)
         GOTO (20) RET
         RETURN
      ELSE IF (CCMD.EQ.'x') THEN
C***************************************************************
C                    DYNAMIC OPTION                            *
C***************************************************************
            DYNAM=.NOT.DYNAM
            IF (DYNAM) THEN
               IF (NDATA.GT.0) THEN
C                 must be entities in buffer
C                 save cell hilited
                  DYNMEN=TMEN
                  DYNCEL=TCELL
C                 load the point modes to menu
                  CALL MNLPTS()
C                 get the reference point
                  CALL FINDP0(268,WX1,WY1,OPTION,QUIT)
C                 unload the point modes to menu
                  CALL MNUPTS()
                  IF (OPTION) GOTO 20
                  IF (.NOT.QUIT) THEN
C                    activate dynamic movement
                     CALL DRAG(WX1,WY1,WX2,WY2,THETA,TFM)
C                    clear dynamic flag
                     DYNAM=.FALSE.
                     CALL GTMCLO(DYNMEN,DYNCEL)
                  END IF
               ELSE
C                 nothing in buffer,tell the idiot
                  CALL DEPRNT(34)
                  DYNAM=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
               END IF
            ELSE
C              cancel dynamic option
               CALL GTMCLO(TMEN,TCELL)
            END IF
            RETURN
      ELSE IF (CCMD.EQ.'A') THEN
C***************************************************************
C                    ANIMATE OPTION                            *
C***************************************************************
            ANIMAT=.NOT.ANIMAT
            IF (ANIMAT) THEN
C              ensure HOLD is active
               HOLDON=.TRUE.
C              find HOLD cell and hilite it
               CALL GTHFMC(3,'H',I)
C              hilite the cell
               CALL GTMCHI(3,I)
            ELSE
C              cancel animate option
               CALL GTMCLO(TMEN,TCELL)
            END IF
            RETURN
      ELSE IF (CCMD.EQ.'H') THEN
C***************************************************************
C                    HOLD OPTION                               *
C***************************************************************
            IF (.NOT.ANIMAT) THEN
C              do not over-ride hold condition if animate active
               HOLDON=.NOT.HOLDON
               IF (.NOT.HOLDON) THEN
C                 cancel hold option
                  CALL GTMCLO(TMEN,TCELL)
               END IF
            END IF
            RETURN
      ELSE IF (CCMD.EQ.'R') THEN
C***************************************************************
C           S T R E T C H   O P T I O N                        *
C***************************************************************
            IF (OPFLAG(11)) THEN
C              user is cancelling stretch.
               CALL GTMCLO(TMEN,TCELL)
               OPFLAG(11)=.FALSE.
            ELSE
               OPFLAG(11)=.TRUE.
            END IF
            RETURN
      ELSE IF (CCMD.EQ.CHAR(150)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
            IF (NDATA.GT.0) THEN
C              must be entities in buffer
               CALL DCPRNT(266)
C              hold number of entities if necessary
               IF (HOLDON) NHOLD=NDATA
               IF (ANIMAT) THEN
C                 go do the move and animate it
C
                  OK=OPFLAG(1).OR.OPFLAG(2).OR.
     +               OPFLAG(3).OR.OPFLAG(4)
                  IF ( OK ) THEN
                     CALL ANIMOV(SPEED)
                  ELSE
                     CALL DEPRNT(147)
                  END IF
               ELSE
C                 go do the move
                  IF(ARRON) THEN
C                   build array and clear menu cell
                    CALL MOVARR(OK, .FALSE.) 
                    CALL GTMCLO(MNCELL(6,1),MNCELL(6,2))
C                   stick array back in array menu cell
C                   instead of the type
                    CALL GTCLRC(MNCELL(6,1), MNCELL(6,2))
                    GTMULT = .TRUE.
                    CALL GTDMEN(35,3)
                    ARRON = .FALSE.
                  ELSE
                    CALL MOVMAS(.FALSE.)
                  ENDIF
               END IF
               IF (HOLDON) THEN
C                 recover scratch contents
                  NDATA=NHOLD
                  VNDATA=NDATA+1
               ELSE
C                 clear transformations
                  DO 50 C=1,4
                     IF (OPFLAG(C)) THEN
                        OPFLAG(C)=.FALSE.
                        CALL GTMCLO(MNCELL(C,1),MNCELL(C,2))
                     END IF
 50               CONTINUE
C                 reset text scale factor to 1
                  REFDAT(7,3)=1
               END IF
            ELSE
C              nothing in buffer,tell the idiot
               CALL DEPRNT(34)
            END IF
      ELSE  IF (CVERFY(CCMD,'bkdef')) THEN
C***************************************************************
C      GEOMETRIC ARRAY GENERATION OPTION                       *
C***************************************************************
C
C             get the array position etc from user and set
C             the MOVMAS control flags
C             set the ARRAY type
              CALL GTMCLO(TMEN,TCELL)
              CALL TYPARR(OK)
C
              CALL ARRINP(OK)
              IF (OK) THEN
                  RETURN
              ELSE
C                 stick array entry cell back in
                  CALL GTMCLO(TMEN,TCELL)
                  CALL GTCLRC(TMEN,TCELL)
                  GTMULT = .TRUE.
                  CALL GTDMEN(35,3)
              ENDIF
C                                                
      ELSE  IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        process the normal options now
C           use window for selection of entities
            CALL DCPRNT(88)
            CALL WINDOW(.TRUE.)
      ELSE
         CALL DEPRNT(8)
      END IF
C     ensure menu cell is switched off
      CALL GTMCLO(TMEN,TCELL)
C
 99   CONTINUE
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MOVL04(DYNAM,RET)
C     ============================
C
C1    vartype             L   I4
C1    iostatus            IO  O
C
C2    Subroutine MOVL04 sets the translate
C2    scale,rotate and mirror controls for
C2    use in geometry manipulation.The required
C2    control tokens must be available from the
C2    option menu currently in use.RET returns
C2    an integer to be used in a computed goto
C2    at the caller,so that the correct action is
C2    taken
C
      include 'include/ndata.inc'
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/movdat.inc' 
      include 'include/style.inc' 

C
      REAL WX1,WY1,WX2,WY2,RAD,CANG,REAL,THETA
      REAL TFM(3,3)
      DOUBLE PRECISION DN
      INTEGER*2 I2
      INTEGER*4 TMEN,TCELL,C,NLEN,I,RET
      LOGICAL OPTION,QUIT,SAME,DYNAM
      CHARACTER CCBUFF*40,OLIN*16,TEMP*8
      INTRINSIC CHAR,REAL
C
      EXTERNAL GTMCLO,GTMCHI,MNUPTS,GTHFMC,
     1         CANG,FINDP0,RSWAP,NLEN,RAD,AEXPRN,SAME
C
      IF (CCMD.EQ.'t') THEN
C***************************************************************
C                 TRANSLATE OPTION                             *
C***************************************************************
         IF (.NOT.OPFLAG(4)) THEN
C           set the flag to true
            OPFLAG(4)=.TRUE.
C           save the caller menu cell
            MNCELL(4,1)=MEN
            MNCELL(4,2)=CELLN
C           ensure cell hilited
            CALL GTMCHI(MEN,CELLN)
C           load the point modes to menu
            CALL MNLPTS()
C           get the origin point of original position
            CALL FINDP0(268,WX1,WY1,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 't' .OR. QUIT) THEN
C              User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(4)=.FALSE.
               CALL GTMCLO(MNCELL(4,1),MNCELL(4,2))
               CALL MNUPTS()
               RETURN
            END IF
C
            CALL FINDP0(125,WX2,WY2,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 't' .OR. QUIT) THEN
C                 User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(4)=.FALSE.
               CALL GTMCLO(MNCELL(4,1),MNCELL(4,2))
               CALL MNUPTS()
               RETURN
            END IF
C
C           calculate required movement
            REFDAT(4,1)=WX2-WX1
            REFDAT(4,2)=WY2-WY1
            I2=2
            CALL HARDWF(I2)
            CALL SETAPN(COLOUR)
            CALL DRAWLW(WX1,WY1,WX2,WY2)
            I2=0
            CALL HARDWF(I2)
            IF ( SAME(WX2,WX1).AND.SAME(WY2,WY1) ) THEN
               CALL DEPRNT(147)
C              cancel the previous translate
               OPFLAG(4)=.FALSE.
               CALL GTMCLO(MNCELL(4,1),MNCELL(4,2))
            END IF
C
         ELSE
C           cancel the previous translate
            OPFLAG(4)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         END IF
C        unload the point modes
         CALL MNUPTS()
         RET=2
         RETURN
C
      ELSE IF (CCMD.EQ.'S') THEN
C***************************************************************
C                SCALE TEXT OPTION                             *
C***************************************************************
C        set flag for scaling of text
         IF (OPFLAG(7)) THEN
C           toggle scale text flag
            OPFLAG(7)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           text scaling factor loaded by geometric scale option
C           enable it's use
            OPFLAG(7)=.TRUE.
            REFDAT(7,3)=REFDAT(1,3)
         END IF
         RET=2
         RETURN
C
      ELSE IF (CCMD.EQ.'s') THEN
C***************************************************************
C                     SCALE OPTION                             *
C***************************************************************
         IF (.NOT.OPFLAG(1)) THEN
C           set the flag to true
            OPFLAG(1)=.TRUE.
C           save the caller menu cell
            MNCELL(1,1)=MEN
            MNCELL(1,2)=CELLN
C           ensure cell hilited
            CALL GTMCHI(MEN,CELLN)
C           load the point modes to menu
            CALL MNLPTS()
            CALL FINDP0(270,WX1,WY1,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 's' .OR. QUIT) THEN
C              User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(1)=.FALSE.
               CALL GTMCLO(MNCELL(1,1),MNCELL(1,2))
               CALL MNUPTS()
               RETURN
            END IF
C
C           save the scaling origin
            REFDAT(1,1)=WX1
            REFDAT(1,2)=WY1
C           unload the point modes
            CALL MNUPTS()
C           get the scale factor
 211        CONTINUE
            CALL DPRMXP(245,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               OPFLAG(1)=.FALSE.
               CALL GTMCLO(MNCELL(1,1),MNCELL(1,2))
               RET=2
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*211)
               IF ( SAME(0.0,REAL(DN))) THEN
                  CALL DEPRNT(160)
                  OPFLAG(1)=.FALSE.
                  CALL GTMCLO(MNCELL(1,1),MNCELL(1,2))
                  RET=2
                  RETURN
               END IF
               REFDAT(1,3)=REAL(DN)
C              save the scale factor for local text operations
               REFDAT(7,3)=REFDAT(1,3)
            END IF
         ELSE
C           cancel the previous scaling
            OPFLAG(1)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
C           ensure text scale factor is unity
            REFDAT(7,3)=1.0
         END IF
C        unload the point modes
         CALL MNUPTS()
         RET=2
         RETURN
C
      ELSE IF (CCMD.EQ.'r') THEN
C***************************************************************
C                     ROTATE OPTION                            *
C***************************************************************
         IF (.NOT.OPFLAG(2)) THEN
C           set the flag to true
            OPFLAG(2)=.TRUE.
C           save the caller menu cell
            MNCELL(2,1)=MEN
            MNCELL(2,2)=CELLN
C           ensure cell hilited
            CALL GTMCHI(MEN,CELLN)
C           load the point modes to menu
            CALL MNLPTS()
            CALL FINDP0(271,WX1,WY1,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 'r' .OR. QUIT ) THEN
C              User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(2)=.FALSE.
               CALL GTMCLO(MNCELL(2,1),MNCELL(2,2))
               CALL MNUPTS()
               RETURN
            END IF
C
C           save the rotation origin
            REFDAT(2,1)=WX1
            REFDAT(2,2)=WY1
C           unload the point modes
            CALL MNUPTS()
C           get the rotation angle
 311        CALL DPRMXP(246,CCBUFF)
C
            IF (NLEN(CCBUFF).EQ.0 )THEN
C              user has returned zero length string
C              assume that he has change his mind and return for input
               OPFLAG(2)=.FALSE.
               CALL GTMCLO(MNCELL(2,1),MNCELL(2,2))
               RET=2
               RETURN
            ELSE
C              evaluate an arithmetic expression from the keyboard
               CALL AEXPRN(CCBUFF,DN,*311)
C              save the rotation angle
               IF ( ABS(DN).LT.0.01) THEN
                  CALL DEPRNT(261)
                  OPFLAG(2)=.FALSE.
                  CALL GTMCLO(MNCELL(2,1),MNCELL(2,2))
                  RET=2
               END IF
               REFDAT(2,3)=RAD(REAL(DN))
            END IF
C
         ELSE
C           cancel the previous scaling
            OPFLAG(2)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         END IF
C        unload the point modes
         CALL MNUPTS()
         RET=2
         RETURN
C
      ELSE IF (CCMD.EQ.'m') THEN
C***************************************************************
C                 MIRROR   OPTION                              *
C***************************************************************
         IF (.NOT.OPFLAG(3)) THEN
C           set the flag to true
            OPFLAG(3)=.TRUE.
C           save the caller menu cell
            MNCELL(3,1)=MEN
            MNCELL(3,2)=CELLN
C           ensure cell hilited
            CALL GTMCHI(MEN,CELLN)
C           load the point modes to menu
            CALL MNLPTS()
C           get the origin point of hinge line
            CALL FINDP0(272,WX1,WY1,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 'm' .OR. QUIT ) THEN
C              User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(3)=.FALSE.
               CALL GTMCLO(MNCELL(3,1),MNCELL(3,2))
               CALL MNUPTS()
               RETURN
            END IF
C
C           get the new position of reference point
            CALL FINDP0(273,WX2,WY2,OPTION,QUIT)
            IF (OPTION .OR. QUIT) THEN
               IF ( CCMD .EQ. 'm' ) THEN
C              User has hit twice go home with light off
                  RET=2
               ELSE
                  RET=1
               END IF
C              abort if not valid data
               OPFLAG(3)=.FALSE.
               CALL GTMCLO(MNCELL(3,1),MNCELL(3,2))
               CALL MNUPTS()
               RETURN
            END IF
C
C           calculate required movement
            IF ( WX2 .LT. WX1 ) THEN
               CALL RSWAP(WX1,WX2)
               CALL RSWAP(WY1,WY2)
            END IF
            REFDAT(3,1)=WX1
            REFDAT(3,2)=WY1
            REFDAT(3,3)=CANG(WX1,WY1,WX2,WY2)
C
            I2=2
            CALL HARDWF(I2)
            CALL SETAPN(COLOUR)
            CALL DRAWLW(WX1,WY1,WX2,WY2)
            I2=0
            CALL HARDWF(I2)
C
C
         ELSE
C           cancel the previous mirror
            OPFLAG(3)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         END IF
C        unload the point modes
         CALL MNUPTS()
         RET=2
         RETURN
      END IF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MOVL05(OK)
C     ============================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine MOVL05 sets the origin
C2    of the entity(s) that make up
C2    the array elements and sets the
C2    origin of the array.It also evaluates
C2    radii and start angles of the array.
C2    The logical control flag ARRON is
C2    is set TRUE if the correct actions
C2    are performed by the user to enable
C2    the construction of an ARRAY
C
C
      include 'include/movdat.inc'
      include 'include/menun.inc'
C
      REAL WX1, WY1, WX2, WY2,SCX, SCY, DISTXY,
     +     CANG
C
      LOGICAL OK, OPTION, QUIT, SAME
C
      INTEGER*2 I2
 
      INTEGER*4 TMEN, TCELL
C
      EXTERNAL FINDP0, SAME, MNUPTS, MNLPTS, CROSS
     +         DISTXY, WO2SC
C
C     save the caller menu cell
      MNCELL(6,1) = MEN
      MNCELL(6,2) = CELLN
C
C     it may be unnecessary for the origin of the 
C     array element  and the array to be specified 
C     if the element  is to be part of the array in 
C     all cases apart from RADIAL UNROTATED so in these
C     cases the array position and element position are
C     set to zero to give a zero translation
C



      IF(.NOT. MEMBER .OR. RADIAL .AND. .NOT. ROTAT) THEN
C       get the origin point of the array element
C       load the point modes to menu
        CALL MNLPTS()
        CALL FINDP0(585,WX1,WY1,OPTION,QUIT)
C       unload the point modes
        CALL MNUPTS()
        IF (OPTION .OR. QUIT) THEN
C              abort if not valid data
               GOTO 99
        END IF
C
C       make a wee cross
        CALL WO2SC(WX1, WY1, SCX, SCY)
        CALL CROSS(SCX,SCY)
      ELSE
C       right the origin of the array element is 
C       is of no interest
        WX1 = 0.0
        WY1 = 0.0
      ENDIF
C

C     get the target position of the array
C
C     use the correct prompt for RADIAL or RECTANGULAR
C

      IF(RADIAL .OR.  .NOT. MEMBER) THEN
C       load the point modes to menu
        CALL MNLPTS()
        IF(RADIAL) THEN
C         get the centre of the radial array
          CALL FINDP0(586,WX2,WY2,OPTION,QUIT)
        ELSEIF(.NOT. MEMBER) THEN
C         get the corner position of the rectangular array
          CALL FINDP0(587,WX2,WY2,OPTION,QUIT)
        ENDIF
C       unload the point modes
        CALL MNUPTS()
C       make a wee cross
        CALL WO2SC(WX2, WY2, SCX, SCY)
        CALL CROSS(SCX,SCY)
      ENDIF
C
      IF (OPTION .OR. QUIT) THEN
C              abort if not valid data
               GOTO 99
      END IF
C     get position of the element
      ELECEN(1) = WX1
      ELECEN(2) = WY1
C
C     save the positions of the array
C
      IF(.NOT. MEMBER .OR. RADIAL .AND. .NOT. ROTAT) THEN
C       set array position
        ARRPOS(1) = WX2
        ARRPOS(2) = WY2
C       draw fonted line from element centre to
C       centre of the array
        I2=2
        CALL HARDWF(I2)
        CALL DRAWLW(WX1,WY1,WX2,WY2)
        I2=0
        CALL HARDWF(I2)
C       check for zero length
        IF( SAME(WX2,WX1).AND.SAME(WY2,WY1) ) THEN
           CALL DEPRNT(147)
           GOTO 99
        ELSE
C          length o.k. so get length and angle
           IF(RADIAL .AND. MEMBER) THEN
C            get the start angle and the radius
             ARRAD = DISTXY(WX2,WY2,WX1,WY1)
             STANG = CANG(WX2,WY2,WX1,WY1)
           ENDIF
        ENDIF
      ELSE
C     Right it is a rectangular or radial rotated
C     array with the instanced elements as the
C     first element
C     set array position
      ARRPOS(1) = WX2
      ARRPOS(2) = WY2
      ENDIF
C
C
C
C     everything o.k. then go back to get an accept
      OK = .TRUE.
      ARRON = .TRUE.
C
      RETURN
C     botched it oh well!
 99   CONTINUE
      OK = .FALSE.
      ARRON = .FALSE.
      CALL MNUPTS()
      CALL GTMCLO(MEN,CELLN)
C
      END
C
 
      SUBROUTINE MOVMAS(CSRSET)
C     =========================
C
C1    No arguments required
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include 'include/wrkdat.inc'
      include 'include/style.inc'
      include 'include/viewport.inc'
C
      REAL TX1,TY1,TX2,TY2,BX,BY,ANG,CANG,PI,ANG2,
     +     XEA,YEA,XSA,YSA,X(4),Y(4),DEG,XMIN,YMIN,XMAX,YMAX,
     1     SM(3,3),RM(3,3),MM(3,3),TM(3,3),M1(3,3),M2(3,3),MT(3,3),
     2     DM(3,3),TW,TRAD,DUMMY
C
      INTEGER*4 I,J,NN,K,II,IJ,JJ,NRRECS,NXTENT,STRTP,ENDP
      INTEGER*4 NGENTS,GRPCNT
      REAL PARS(9)
C
      INTEGER*2 MP,DPP,TMP,PDP,INEXT,ITHIS,TDFP,D1,TENT,TMIP
      INTEGER*2 PMIP,PREL,T1MIP,T2MIP,TTENT,ENT
      INTEGER*2 TARGLY,CURRLY
C
      LOGICAL FIRST,OK,DRAWIT,TRANSF,TRONLY,THISEG,LASTEG,STARTG
      LOGICAL EOG1,EOG2,NEWG,CSRSET,IGNORE,TMPFLG
C
      INTRINSIC MIN,MAX
      EXTERNAL I3M,SCAP2D,ROTP2D,TRAN2D,ROT2D,MULT3M,
     +         SCAL2D,UNFLAG,RSCRF,DIR500,DER500,MV0050,
     1         DELMON,RDISPF,WDISPF,ADDMON,ERSFLW,MV0003,
     2         DRWFLW,ADDISP,DEM500,DEW500,ERSFAW,MV0005,DRWFAW
      EXTERNAL ARCBOX,ERSHT,DEW501,DBR500,DBM500,DBW500,DBW501,
     +         DRWHT,ERSTXT,MV0085,DRWTXT,TEXBOX,DERDIM,DEWDIM,
     1         DEMDIM,MV0033,MV0035,MV0034,GRPSIZ,PI
C
C
C     initialize grouping flags
      LASTEG=.FALSE.
      THISEG=.FALSE.
      STARTG=.FALSE.
      EOG1=.FALSE.
      EOG2=.FALSE.
      STRTP=1
      ENDP=1
C
C     initialize window limits
      XMIN=1E5
      YMIN=1E5
      XMAX=-1E5
      YMAX=-1E5
C
      REFDAT(9,1)=XMIN
      REFDAT(9,2)=YMIN
      REFDAT(10,1)=XMAX
      REFDAT(10,2)=YMAX
C
C     initialize transformation matrices
      CALL I3M(SM)
      CALL I3M(RM)
      CALL I3M(MM)
      CALL I3M(TM)
      CALL I3M(M1)
      CALL I3M(M2)
C     initialize dummy array
      CALL I3M(DM)
C     scale if required
C      write(10,*) '[MOVMAS] STATE OF OPFLAGS'
C      write(10,*) '[MOVMAS] 1,2,3,4,5 ',(' ',OPFLAG(I),I=1,5)
      IF (OPFLAG(1)) THEN
         CALL SCAP2D(REFDAT(1,1),REFDAT(1,2),REFDAT(1,3),
     +                          REFDAT(1,3),SM)
      END IF
C     rotate if required
      IF (OPFLAG(2)) CALL ROTP2D(REFDAT(2,1),REFDAT(2,2),
     +                           REFDAT(2,3),RM)
C     Mirror is necessary
      IF (OPFLAG(3)) THEN
C        Only allow one copy when mirroring
         IF(NNCOPY.GT.1) THEN
            NNCOPY=1
            CALL DEPRNT(274)
         END IF
         CALL TRAN2D(-REFDAT(3,1),-REFDAT(3,2),MM)
         CALL ROT2D(-REFDAT(3,3),M2)
         CALL MULT3M(MM,M2,M1)
         CALL SCAL2D(1.0,-1.0,M2)
         CALL MULT3M(M1,M2,MM)
         CALL ROT2D(REFDAT(3,3),M1)
         CALL MULT3M(MM,M1,M2)
         CALL TRAN2D(REFDAT(3,1),REFDAT(3,2),M1)
         CALL MULT3M(M2,M1,MM)
C
      END IF
C
C     translate if required
      IF (OPFLAG(4)) CALL TRAN2D(REFDAT(4,1),REFDAT(4,2),TM)
C     concatenate the transforms
      CALL MULT3M(SM,RM,M1)
      CALL MULT3M(M1,MM,M2)
      CALL MULT3M(M2,TM,M1)
C     Save original transform MM
      CALL I3M(M2)
      CALL MULT3M(M1,M2,MM)
C     use the matrix M1 for all transformations
C     MM contains original transform
C     now set initial value of overall scale factor
      IF (OPFLAG(1)) THEN
         CURSCL=REFDAT(1,3)
      ELSE
         CURSCL=1.0
         REFDAT(1,3)=1.0
         REFDAT(7,3)=REFDAT(1,3)
      END IF
C
C     set flag to indicate transformation status
      TRANSF=OPFLAG(1).OR.OPFLAG(2).OR.OPFLAG(3).OR.OPFLAG(4)
C     set flag to indicate translate only required
      TRONLY=OPFLAG(4).AND.
     +      (.NOT.(OPFLAG(1).OR.OPFLAG(2).OR.OPFLAG(3)))
C     initialize counter for copying
      NN=0
C
      IF (OPFLAG(5).AND..NOT.TRANSF.AND.NNCOPY.GT.1 ) THEN
         CALL DEPRNT(242)
         NNCOPY=1
      ELSE IF (.NOT.TRANSF.AND.NNCOPY.GT.1 ) THEN
         CALL DEPRNT(189)
         RETURN
      END IF
C
      IF ( OPFLAG(5) ) THEN
C        save current construction layer
         MNCELL(5,2)=CLAYER
C        set target layer
         CLAYER=MNCELL(5,1)
C        new layer required
         DRAWIT=VLAYER(CLAYER)
      END IF
 
C     remove the attention flags
      CALL UNFLAG(.FALSE.)
C
C     step through the workfile
C     and perform the translation
C
C     copy the entity the required number of times
      IF(NNCOPY.GT.0) NN=NNCOPY-1
C
      IF (NDATA.GT.0) THEN
C
C        do the loop for the number of copies required
         DO 55 J=0,NN
C           initialize grouping flags
            LASTEG=.FALSE.
            THISEG=.FALSE.
            STARTG=.FALSE.
            STRTP=1
            ENDP=1
C        save the dimension parameters in case overwritten
         CALL SAVDDT()
C
         DO 50 I=1,NDATA
C           set flag to indicate op state for this entity
            FIRST=.TRUE.
C           read the entity from list
            CALL RSCRF(I,MP,BX,BY,TDFP,D1)
C           ensure end code is properly set from scratch file
            ENDCOD=D1
C           save source MIP
            T1MIP=MP
C           read the entity data
            CALL DIR500(MP,OK)
            MIP=MP
C           must ignore group or header entries
C           ignore if no part data in the entity
            IF (IMBUFF(7).EQ.0) GOTO 49
C           ignore if master geometry and no comp/symb reset allowed
            IGNORE=(IMBUFF(1).EQ.COMPM .OR. IMBUFF(1).EQ.SYMBM)
     +               .AND. (.NOT.CSRSET)
 
            IF (IGNORE) GOTO 49
            CALL DER500(MP,OK)
C           if copies,take care of groups
            IF (NNCOPY.GT.0) THEN
C              update status flag of last entity
               LASTEG=THISEG
C              set current entity status flag
               THISEG=IMBUFF(1).EQ.GROUP
C              now setup action for groups
               EOG1=.FALSE.
               EOG2=.FALSE.
C
               IF (THISEG.AND. .NOT.LASTEG) THEN
C                 first entity of new group
C                 save parent MI for new group
                  PMIP=IMBUFF(8)
C                 find number of ents in group
                  CALL GRPSIZ(PMIP,NGENTS)
C                 set flag to indicate new group
                  STARTG=.TRUE.
C                 set count of entities to zero
                  GRPCNT=0
C                 re-read mi record for entity
                  CALL DIR500(MP,OK)
               END IF
C
            END IF
C
C           if new target layer,test for visibility
C           after layer change
            IF (OPFLAG(5)) THEN
               IF ( NNCOPY .EQ. 0 ) THEN
C                 decrement entity total for current layer
                  CALL DELMON(IMBUFF(4),.FALSE.)
C                 read the display file a the position where the
C                 entity was found
                  CALL RDISPF(TDFP,TENT,TMIP,OK)
                  IF ( TENT.EQ.IMBUFF(2) ) THEN
                     IF ( OK ) THEN
C                       Entity has moved layer mark the old
C                       one as no longer visible
                        TMIP=-TMIP
                        IF ( VLAYER(CLAYER)) TMIP=ABS(TMIP)
                        CALL WDISPF(TDFP,TENT,TMIP,OK)
                     END IF
                  END IF
                  CALL ADDMON(CLAYER)
               END IF
C              change layer of entity
C               IMBUFF(4)=CLAYER
               IMBUFF(4) = MNCELL(5,1)
            ELSE
C              layer remains constant
               DRAWIT=.TRUE.
            END IF
C           save entity type
            TTENT=IMBUFF(2)
C           updating must be done to view ports
            VPMOV = .TRUE.
            VPADD = .FALSE.
 
C     ************************************************************
            IF (TTENT.EQ.LINE) THEN
C     ************************************************************
               IF (.NOT.COPYIT.AND.OPFLAG(9)) THEN
C                 erase the line from the screen
C                 This must be drawn in other views also but not erased
                  CALL PENERS()
                  CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  CALL PENDRW()
               END IF
C
               IF (TRANSF) THEN
C                 only transform if movement required
                  ENDCOD=D1
                  CALL MV0003(RDBUFF,M1)
               END IF
C              if move then original needs transformation
C              if copy only then original should be untouched
               IF (.NOT. COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
C                    This will already have been done
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM500(MP,OK)
                  FIRST=.FALSE.
               ELSE
C                 write out copy transformed to next free space
C                 This must be added to and taken from the viewports
                  VPADD = .TRUE.
                  CALL DEW500(MP,OK)
C                 This must be added to and taken from the viewports
                  VPADD = .FALSE.
               END IF
C              draw the new entity in postion
               IF (DRAWIT) THEN
C                 layer is visible,so drawing is valid
                  IF (OPFLAG(10)) THEN
C                    This must be added to and taken from the viewports
                     VPADD = COPYIT
C                    set draw mode for copy
C                    drawing is enabled so draw it
                     CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  ELSE
C                    check limits of size
                     XMIN=MIN(RDBUFF(1),RDBUFF(4))
                     YMIN=MIN(RDBUFF(2),RDBUFF(5))
                     XMAX=MAX(RDBUFF(1),RDBUFF(4))
                     YMAX=MAX(RDBUFF(2),RDBUFF(5))
                  END IF
               END IF
C              go back and copy the next entity
C
C     ************************************************************
            ELSE IF (TTENT.EQ.MARKER) THEN
C     ************************************************************
               IF (.NOT.COPYIT.AND.OPFLAG(9)) THEN
C                 erase the line from the screen
C                 This must be drawn in other views also but not erased
                  CALL PENERS()
                  CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  CALL PENDRW()
               END IF
C
               IF (TRANSF) THEN
C                 only transform if movement required
                  ENDCOD=D1
                  CALL MV0002(RDBUFF,M1)
               END IF
C              movement now complete
C              test for move or copy instruction
C              if move then original needs transformation
C              if copy only then original should be untouched
               IF (.NOT. COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
C                    This will already have been done
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM500(MP,OK)
                  FIRST=.FALSE.
               ELSE
C                 write out copy transformed to next free space
                  CALL DEW500(MP,OK)
               END IF
               IF (DRAWIT) THEN
C                 layer is visible,so drawing is valid
                  IF (OPFLAG(10)) THEN
C                    This must be added to and taken from the viewports
                     VPADD = COPYIT
C                    set draw mode for copy
C                    drawing is enabled so draw it
                     CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  ELSE
C                    check limits of size
                     XMIN=MIN(RDBUFF(1),RDBUFF(4))
                     YMIN=MIN(RDBUFF(2),RDBUFF(5))
                     XMAX=MAX(RDBUFF(1),RDBUFF(4))
                     YMAX=MAX(RDBUFF(2),RDBUFF(5))
                  END IF
               END IF
C              go back and copy the next entity
C
C     ************************************************************
            ELSE IF (TTENT.EQ.ARC) THEN
C     ************************************************************
C
               IF (.NOT.COPYIT .AND. OPFLAG(9)) THEN
C                 erase the arc from screen
                  CALL PENERS()
                  CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  CALL PENDRW()
               END IF
C              copy the entity the required number of times
                  IF (TRANSF) THEN
C                    transform the arc
                     CALL MV0005(RDBUFF,M1)
                  END IF
C                 end of transformations
C                 movement now complete
                  IF (.NOT.COPYIT) THEN
C                    add to display file if neccessary
C                    opflag(8) indicates external data source
                     IF (OPFLAG(8)) THEN
                        IF ( VLAYER(IMBUFF(4)) )
     +                  CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                        CALL ADDMON(IMBUFF(4))
                     END IF
C                    write data back to storage
                     MIP=MP
                     CALL DEM500(MP,OK)
                 ELSE
C                    write out copy transformed
                     CALL DEW500(MP,OK)
                 END IF
                  IF (DRAWIT) THEN
                     IF (OPFLAG(10)) THEN
                        VPADD = COPYIT
C                       drawing enabled so draw the arc
                        CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                     ELSE
C                       check limits of entity window
                        CALL ARCBOX(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +                      RDBUFF(5),RDBUFF(6), XMIN,YMIN,XMAX,YMAX)
                     END IF
                  END IF
C
C              go back and copy the next entity
C     ************************************************************
            ELSE IF (TTENT.EQ.HATCH) THEN
C     ************************************************************
C
               IF (.NOT.COPYIT .AND.OPFLAG(9)) THEN
C                 erase the hatch from the screen
                   CALL PENERS()
                   ENT=HATCH
                   CALL ALLDRW(ENT,MP)
                   CALL PENDRW()
               END IF
C              read the first record again
               CALL DER500(MP,OK)
               IF (OPFLAG(5)) THEN
C                 change layer of entity
                  IMBUFF(4)=CLAYER
                  IMBUFF(4)= MNCELL(5,1)
               END IF
                  IF (TRANSF) THEN
C                    transform the hatch containment box
                     CALL MV0003(RDBUFF,M1)
                  END IF
C
C                 get the the pointer to the next PD
C                 record of the entity
                  INEXT=IDBUFF(3)
C                 movement of hatch limits now complete
C                 test for move or copy instruction
C                 if move then original needs transformation
C                 if copy only then original should be untouched
                  IF (.NOT. COPYIT) THEN
C                    add to display file if neccessary
C                    opflag(8) indicates external data source
                     IF (OPFLAG(8)) THEN
                        IF ( VLAYER(IMBUFF(4)) )
     +                  CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                        CALL ADDMON(IMBUFF(4))
                     END IF
C                    write data back to storage
                     MIP=MP
                     CALL DEM500(MP,OK)
                     FIRST=.FALSE.
                  ELSE
C                    write out copy transformed to next free space
C                    must maintain connectivity to hatch lines
                     CALL DEW501(MP,OK)
                  END IF
C
C                 MP contains MI pointer of the entity
C
 80               CONTINUE
C                 INEXT points to the next hatch line to be transformed
                  IF (INEXT.NE.0) THEN
C                    read the next hatch line
                     CALL DBR500(INEXT,OK)
                     ITHIS=INEXT
                     INEXT=IDBUFF(3)
                     IF (TRANSF) THEN
C                       transform the hatch line
                        CALL MV0003(RDBUFF,M1)
C                       update global limits
                        XMIN=MIN(RDBUFF(1),XMIN)
                        YMIN=MIN(RDBUFF(2),YMIN)
                        XMAX=MAX(RDBUFF(1),XMAX)
                        YMAX=MAX(RDBUFF(2),YMAX)
                        XMIN=MIN(RDBUFF(4),XMIN)
                        YMIN=MIN(RDBUFF(5),YMIN)
                        XMAX=MAX(RDBUFF(4),XMAX)
                        YMAX=MAX(RDBUFF(5),YMAX)
                     END IF
C
C                    movement of hatch line now complete
C                    test for move or copy instruction
C                    if move then original needs transformation
C                    if copy only then original should be untouched
                     IF (.NOT. COPYIT) THEN
C                       write data back to storage
                        CALL DBM500(ITHIS,OK)
                     ELSE
C                       write out copy transformed to next free space
C                       set MI pointer
                        IDBUFF(2)=MP
                        IF (INEXT.EQ.0) THEN
C                          last hatch line no connectivity
                           CALL DBW500(PDP,OK)
                        ELSE
C                          connected hatch line
                           CALL DBW501(PDP,OK)
                        END IF
                     END IF
C                    try next hatch line
                     GOTO 80
                  END IF
C                 all hatch lines have been transformed
C
C                 regen box limits
                  CALL CHGE40(MP)
C                 hatch can only be drawn when all elements are copied
                  IF (DRAWIT) THEN
                  IF (OPFLAG(10)) THEN
C                        drawing is enabled so draw it
C                        set flag acording to copy
                         VPADD = COPYIT
                         TENT=HATCH
                         CALL ALLDRW(TENT,MP)
                      END IF
                  END IF
C                 movement now complete
C
C     ************************************************************
            ELSE IF (TTENT.EQ.SPLINE) THEN
C     ************************************************************
C
               IF (.NOT.COPYIT .AND.OPFLAG(9)) THEN
C                 erase the hatch from the screen
                   CALL PENERS()
                   ENT=SPLINE
                   CALL ALLDRW(ENT,MP)
                   CALL PENDRW()
               END IF
C              read the first record again
               CALL DER500(MP,OK)
               TW=RDBUFF(6)
               IF (OPFLAG(5)) THEN
C                 change layer of entity
                  IMBUFF(4)=CLAYER
                  IMBUFF(4)= MNCELL(5,1)
               END IF
C
C              test for move or copy instruction
C              if move then original needs transformation
C              if copy only then original should be untouched
               INEXT=IDBUFF(3)
 
               IF (.NOT. COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM500(MP,OK)
                  FIRST=.FALSE.
               ELSE
C                 write out copy transformed to next free space
C                 must maintain connectivity to control points
                  CALL DEW501(MP,OK)
               END IF
C
C              MP contains MI pointer of the entity
C              Two headers to copy as well
 
               CALL DBR500(INEXT,OK)
C
               IF (.NOT. COPYIT) THEN
C              write data back to storage
                  CALL DBM500(INEXT,OK)
               ELSE
C                 Save continuation pointer cos DBW501 will
C                 alter it.
                  INEXT=IDBUFF(3)
C                 write out copy transformed to next free space
C                 set MI pointer
                  IDBUFF(2)=MP
C                 last hatch line no connectivity
                  CALL DBW501(PDP,OK)
               END IF
C
C              INEXT points to the next control point to be transformed
 82            CONTINUE
               IF (INEXT.NE.0) THEN
C                 read the next control point
                  CALL DBR500(INEXT,OK)
                  ITHIS=INEXT
                  INEXT=IDBUFF(3)
                  IF (TRANSF) THEN
C                    transform the control point
                     CALL NEWXY(RDBUFF(1),RDBUFF(2),TX1,TY1,M1)
                     RDBUFF(1)=TX1
                     RDBUFF(2)=TY1
                  END IF
                  XMIN=MIN(RDBUFF(1),XMIN)
                  YMIN=MIN(RDBUFF(2),YMIN)
                  XMAX=MAX(RDBUFF(1),XMAX)
                  YMAX=MAX(RDBUFF(2),YMAX)
C
C                 movement of control point now complete
C                 test for move or copy instruction
C                 if move then original needs transformation
C                 if copy only then original should be untouched
                  IF (.NOT. COPYIT) THEN
C                    write data back to storage
                     CALL DBM500(ITHIS,OK)
                  ELSE
C                    write out copy transformed to next free space
C                    set MI pointer
                     IDBUFF(2)=MP
                     IF (INEXT.EQ.0) THEN
C                       last Control Point no connectivity
                        CALL DBW500(PDP,OK)
                     ELSE
C                       connected Control Point
                        CALL DBW501(PDP,OK)
                     END IF
                  END IF
C                 try next Control Point
                  GOTO 82
               END IF
C              all Spline Control Points have been transformed
C              Modify Thumb Weight
               CALL DER500(MP,OK)
               RDBUFF(6)=TW
               CALL DBM500(IMBUFF(7),OK)
C              regen box limits
               CALL CHGE40(MP)
               IF (DRAWIT) THEN
                  IF (OPFLAG(10)) THEN
C                    drawing is enabled so draw it
                     TENT=SPLINE
                     VPADD= COPYIT
                     CALL ALLDRW(TENT,MP)
                  END IF
               END IF
C              movement now complete
C
C     ************************************************************
            ELSE IF (TTENT.EQ.CENLIN) THEN
C     ************************************************************
               IF (.NOT.COPYIT.AND.OPFLAG(9)) THEN
C                 erase the line from the screen
                  CALL PENERS()
                  CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  CALL PENDRW()
               END IF
C
               IF (TRANSF) THEN
C                 only transform if movement required
                  ENDCOD=D1
                  CALL MV0030(RDBUFF,M1)
               END IF
C              if move then original needs transformation
C              if copy only then original should be untouched
               IF (.NOT. COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
C                    This will already have been done
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM500(MP,OK)
                  FIRST=.FALSE.
               ELSE
C                 write out copy transformed to next free space
                  CALL DEW500(MP,OK)
               END IF
C              draw the new entity in postion
               IF (DRAWIT) THEN
C                 layer is visible,so drawing is valid
                  IF (OPFLAG(10)) THEN
C                    This must be added to and taken from the viewports
                     VPADD = COPYIT
C                    set draw mode for copy
C                    drawing is enabled so draw it
                     CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  ELSE
C                    check limits of size
                     IF (IDBUFF(4).EQ.1) THEN
C                       Crossed center line..
                        ANG = 0
                        ANG2 = PI(2.0)
                        CALL ARCBOX(RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +                              ANG,ANG2,XMIN,YMIN,XMAX,YMAX)
                     ELSE IF (IDBUFF(4).EQ.2) THEN
C                       Single center line.
C                       Turn it into a line.
                        TRAD = RDBUFF(3) + RDBUFF(6)
                        DUMMY = TRAD
                        ANG = 0
                        CALL NEWPNT(RDBUFF(1),RDBUFF(2),TRAD,DUMMY,
     +                              ANG,RDBUFF(5),TX2,TY2)
                        TX1 = RDBUFF(1) - (TX2 - RDBUFF(1))
                        TY1 = RDBUFF(2) - (TY2 - RDBUFF(2))
C                       Get it's limits.
                        XMIN=MIN(TX1,TX2)
                        YMIN=MIN(TY1,TY2)
                        XMAX=MAX(TX1,TX2)
                        YMAX=MAX(TY1,TY2)
                     ELSE
C                       P.C.D. center line.
C                       Move to center of minor arc.
                        TRAD = RDBUFF(3)
                        DUMMY = RDBUFF(3)
                        ANG = 0
                        CALL NEWPNT(RDBUFF(1),RDBUFF(2),TRAD,DUMMY,
     +                              ANG,RDBUFF(5),TX1,TY1)
                        TRAD = RDBUFF(4) + RDBUFF(6)
                        ANG = 0
                        ANG2 = PI(2.0)
                        CALL ARCBOX(TX1,TY1,RDBUFF(4),TRAD,
     +                              ANG,ANG2,XMIN,YMIN,XMAX,YMAX)
                     ENDIF
                  END IF
               END IF
C              go back and copy the next entity
C
C     ************************************************************
            ELSE IF (TTENT.EQ.TEXT) THEN
C     ************************************************************
               IF (.NOT.COPYIT .AND. OPFLAG(9)) THEN
C                   erase the text from screen
                     CALL PENERS()
                     ENT=TEXT
                     CALL ALLDRW(ENT,MP)
                     CALL PENDRW()
               END IF
C              copy the entity the required number of times
               IF (OPFLAG(5)) THEN
C                 change layer of entity
                  IMBUFF(4)=CLAYER
                  IMBUFF(4)= MNCELL(5,1)
               END IF
               IF (TRANSF) THEN
                  CALL MV0085(RDBUFF,M1)
               END IF
C              end of transformations
C              movement now complete
               IF (.NOT.COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM500(MP,OK)
               ELSE
C                 write out copy to database
                  CALL DEW500(MP,OK)
               END IF
               IF (DRAWIT) THEN
                  IF (OPFLAG(10)) THEN
C                    draw the text in new position
                     VPADD = COPYIT
C                    set mode for view drawing
                     CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                  ELSE
C                    test window limits of entity
                     CALL TEXBOX(RDBUFF(1),RDBUFF(2),
     +   PAPTOW*RDBUFF(3),PAPTOW*RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1                                     XMIN,YMIN,XMAX,YMAX)
                  END IF
               END IF
C     ************************************************************
            ELSE IF (TTENT.EQ.COMPM .OR. TTENT.EQ.SYMBM) THEN
C     ************************************************************
C      WRITE(UNIT=10,FMT=*)'[MOVMAS] CALLING DER500 (COMPM)'
               CALL DER500(MP,OK)
               IF (TRANSF) THEN
C                 only transform if movement required
                  CALL MV0003(RDBUFF,M1)
               END IF
C              end of transformations
C              check limits of size
               XMIN=MIN(RDBUFF(1),RDBUFF(4))
               YMIN=MIN(RDBUFF(2),RDBUFF(5))
               XMAX=MAX(RDBUFF(1),RDBUFF(4))
               YMAX=MAX(RDBUFF(2),RDBUFF(5))
C              movement now complete
               IF (.NOT.COPYIT) THEN
C                 write data back to storage
                  MIP=MP
C      WRITE(UNIT=10,FMT=*)'[MOVMAS] CALLING DEM500 (COMPM) mip=',mip
                  CALL DEM500(MP,OK)
              ELSE
C                 add to database new header entry
                  CALL DEW500(MP,OK)
              END IF
C     ************************************************************
            ELSE IF (TTENT.EQ.COMPI .OR. TTENT.EQ.SYMBI) THEN
C     ************************************************************
C              save current buffers
               IF(OPFLAG(5)) THEN
                  CURRLY=CLAYER
                  TARGLY=IMBUFF(4)
               ENDIF
               CALL DER566(MP,MT,OK)
               IF (.NOT.COPYIT .AND. OPFLAG(9)) THEN
C                 erase the component from screen
                  CALL PENERS()
                  CALL ALLDRW(TTENT,MP)
                  CALL PENDRW()
               END IF
C              copy the entity the required number of times
C
               CALL DER566(MP,MT,OK)
               IF (TRANSF) THEN
                   IF(CSRSET.OR.OPFLAG(12) ) THEN
                       CALL MV0166(M1,MT)
                    ELSE
                       CALL MV0066(M1,MT)
                   END IF
               ENDIF
C              end of transformations
C              check limits of size always 
               XMIN=MIN(XMIN,
     +         RWORK(1,1),RWORK(4,1),RWORK(1,2),RWORK(4,2))
               YMIN=MIN(YMIN,
     +         RWORK(2,1),RWORK(5,1),RWORK(2,2),RWORK(5,2))
               XMAX=MAX(XMAX,
     +         RWORK(1,1),RWORK(4,1),RWORK(1,2),RWORK(4,2))
               YMAX=MAX(YMAX,
     +         RWORK(2,1),RWORK(5,1),RWORK(2,2),RWORK(5,2))
C
               IF(OPFLAG(5)) THEN
                   CLAYER=CURRLY
                   IMBUFF(4)=TARGLY
               ENDIF
C              movement now complete
               IF (.NOT.COPYIT) THEN
C                 add to display file if neccessary
C                 opflag(8) indicates external data source
                  IF (OPFLAG(8)) THEN
                     IF ( VLAYER(IMBUFF(4)) )
     +               CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                     CALL ADDMON(IMBUFF(4))
                  END IF
C                 write data back to storage
                  MIP=MP
                  CALL DEM566(MT,MP,OK)
              ELSE
C                 write out copy to database
                  CALL DEW566(MT,MP,OK)
              END IF
C
              IF (DRAWIT) THEN
                 IF (OPFLAG(10)) THEN
                     VPADD = COPYIT
C                    draw the component in new position
                     CALL ALLDRW(TTENT,MP)
                 END IF
              ENDIF
C     ************************************************************
            ELSE IF (TTENT.EQ.LDIMN .OR. TTENT.EQ.ADIMN
     +               .OR. TTENT.EQ.RDIMN .OR.TTENT.EQ.DDIMN
     1               .OR. TTENT.EQ.GLABEL) THEN
C     ************************************************************
C              read the dim data into buffer
               CALL DERDIM(MP,OK)
               IF (.NOT.COPYIT .AND.OPFLAG(9)) THEN
C                 erase the dimension from the screen
                   CALL PENERS()
                   CALL ALLDRW(TTENT,MP)
                   CALL PENDRW()
               END IF
               IF (OPFLAG(5)) THEN
C                 change layer of entity
                  IMBUFF(4)=CLAYER
                  IMBUFF(4)= MNCELL(5,1)
               END IF
C              copy the entity the required number of times
C
                  IF (TRANSF) THEN
C                    transform the dimension, save & set opflag
                     ENDCOD=D1
                     TMPFLG = OPFLAG(9)
                     OPFLAG(9) = .FALSE.
                     CALL MVDIM0(M1,IMBUFF(2),OK)
                     OPFLAG(9) = TMPFLG
                  END IF
                  IF (OK) THEN
C                    moved dimension is valid
                     IF (DRAWIT) THEN
C                       layer is visible,so drawing is valid
                        IF (OPFLAG(10)) THEN
C                          drawing is enabled so draw it
                           VPADD = COPYIT
                           CALL SPCDRW(TTENT,MP,.FALSE.,DM,.FALSE.)
                        END IF
                     END IF
C                    movement now complete
C                    test for move or copy instruction
C                    if move then original needs transformation
C                    if copy only then original should be untouched
                     IF (.NOT. COPYIT) THEN
C                       add to display file if neccessary
C                       opflag(8) indicates external data source
                        IF (OPFLAG(8)) THEN
                           IF ( VLAYER(IMBUFF(4)) )
     +                     CALL ADDISP(MP,IMBUFF(2),DPP,OK)
                           CALL ADDMON(IMBUFF(4))
                        END IF
C                       write data back to storage
                        MIP=MP
                        CALL DEMDIM(MP,OK)
                        FIRST=.FALSE.
                     ELSE
C                       modify only here
                        CALL DEWDIM(MP,IMBUFF(2),OK)
                     END IF
                  END IF
C     ************************************************************
            END IF
C     ************************************************************
C              save MI of record just written
               T2MIP=MP
C
C
C           set group pointers
            IF (STARTG) THEN
               STRTP=T2MIP
               ENDP=STRTP
               STARTG=.FALSE.
            END IF
C
            IF (THISEG) THEN
C              update count of grouped entities
               GRPCNT=GRPCNT+1
C              test for end of group
               IF (GRPCNT.EQ.NGENTS) THEN
C                 complete the group
                  ENDP=T2MIP
                  CALL MV0050(PMIP,STRTP,ENDP)
C                 end of group list
C                 reset flag
                  THISEG=.FALSE.
               END IF
            END IF
C
C
C           update limits of entity window
C           maximum X
            REFDAT(10,1)=MAX(XMAX,REFDAT(10,1))
C           minimum X
            REFDAT(9,1)=MIN(XMIN,REFDAT(9,1))
C           maximum Y
            REFDAT(10,2)=MAX(YMAX,REFDAT(10,2))
C           minimum Y
            REFDAT(9,2)=MIN(YMIN,REFDAT(9,2))
 49         CONTINUE
 50         CONTINUE
C
C           restore the dimension parameters .
            CALL RSTDDT()
C           concatenate transforms,for next pass
            CALL MULT3M(M1,MM,M2)
            CALL I3M(TM)
            CALL MULT3M(M2,TM,M1)
C           calculate new overall scaling factor
C           use unit length line to establish total effect
            CURSCL=CURSCL*REFDAT(1,3)
C
C
 55      CONTINUE
C        reset entity counter in scratch file
         CALL ZRFLAG()
      ELSE
C        no entity to move
         CALL DEPRNT(34)
      END IF
C
 99   CONTINUE
C
C     recover construction layer
      IF ( OPFLAG(5) ) THEN
         CLAYER=MNCELL(5,2)
      END IF
C     all done for now
C
      END
C
C--------------------------------------------------------------------
      SUBROUTINE MV0002(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0005 performs a geometric transformation
C2    on a circular arc whose data is passed in the real array
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      REAL TX1,TY1,TX2,TY2,TX3,TY3,PI,CANG,RARRAY(6),M(3,3),
     +     TRX1,TRY1,TRX2,TRY2,TRX3,TRY3,DISTXY,COS,SIN,ABS,PARS(9)
      INTRINSIC COS,SIN,ABS
      INTEGER*4 I
C
      EXTERNAL NEWXY,RSWAP,CANG,DISTXY,DECODM
C
C
C     decode the transformation matrix
      CALL DECODM(M,PARS)
C     first reposition the centre point
      TX1=RARRAY(1)
      TY1=RARRAY(2)
      CALL NEWXY(TX1,TY1,RARRAY(1),RARRAY(2),M)
      
C     is there scaling
      IF(OPFLAG(1)) THEN
        RARRAY(5)=RARRAY(5)*PARS(4)
        RARRAY(6)=RARRAY(6)*PARS(5)
      END IF
C     is there rotation
      IF(OPFLAG(2)) THEN
        RARRAY(4)=RARRAY(4)+PARS(9)
      END IF
C
      END
C
C
      SUBROUTINE MV0003(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0003 performs a geometric transformation
C2    on a line whose data is passed in the real array
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      INTEGER I,J
      REAL TX1,TY1,TX2,TY2,RARRAY(6),M(3,3)
      EXTERNAL NEWXY
C
C     copy the points for transformation
      IF ( OPFLAG(11).AND.ENDCOD.NE.0  ) THEN
C        copy the points for transformation
         GOTO (1,2) ENDCOD
 1       CONTINUE
         TX1=RARRAY(1)
         TY1=RARRAY(2)
C        do the transformation
         CALL NEWXY(TX1,TY1,RARRAY(1),RARRAY(2),M)
         GOTO 3
 2       CONTINUE
         TX2=RARRAY(4)
         TY2=RARRAY(5)
C        do the transformation
         CALL NEWXY(TX2,TY2,RARRAY(4),RARRAY(5),M)
         GOTO 3
 3       CONTINUE
      ELSE
         TX1=RARRAY(1)
         TY1=RARRAY(2)
         TX2=RARRAY(4)
         TY2=RARRAY(5)
C        do the transformation
         CALL NEWXY(TX1,TY1,RARRAY(1),RARRAY(2),M)
         CALL NEWXY(TX2,TY2,RARRAY(4),RARRAY(5),M)
 
      END IF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0005(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0005 performs a geometric transformation
C2    on a circular arc whose data is passed in the real array
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      REAL TX1,TY1,TX2,TY2,TX3,TY3,PI,CANG,RARRAY(6),M(3,3),
     +     TRX1,TRY1,TRX2,TRY2,TRX3,TRY3,DISTXY,COS,SIN,ABS,PARS(9)
      INTRINSIC COS,SIN,ABS
      INTEGER*4 I
C
      EXTERNAL NEWXY,RSWAP,CANG,DISTXY,DECODM
C
C
C     decode the transformation matrix
      CALL DECODM(M,PARS)
C     first reposition the centre point
      TX1=RARRAY(1)
      TY1=RARRAY(2)
      CALL NEWXY(TX1,TY1,TRX1,TRY1,M)
C
C     Start angle being calculated in cartesian form
      TX2=RARRAY(1)+RARRAY(4)*COS(RARRAY(5))
      TY2=RARRAY(2)+RARRAY(4)*SIN(RARRAY(5))
      CALL NEWXY(TX2,TY2,TRX2,TRY2,M)
C
C     End angle being calculated in cartesian form
      TX3=RARRAY(1)+RARRAY(4)*COS(RARRAY(6))
      TY3=RARRAY(2)+RARRAY(4)*SIN(RARRAY(6))
      CALL NEWXY(TX3,TY3,TRX3,TRY3,M)
C
      RARRAY(1)=TRX1
      RARRAY(2)=TRY1
C     do the transformation
C     now consider the start and end angles and radius
      IF (ABS(RARRAY(6)-RARRAY(5)-PI(2.0)).LT.1E-5) THEN
         RARRAY(5)=0.0
         RARRAY(6)=PI(2.0)
      ELSE
         RARRAY(5)=CANG(TRX1,TRY1,TRX2,TRY2)
         RARRAY(6)=CANG(TRX1,TRY1,TRX3,TRY3)
      END IF
C     scaling required on radius
      IF (OPFLAG(1)) RARRAY(4)=DISTXY(TRX1,TRY1,TRX2,TRY2)
C
      IF ( OPFLAG(3)) THEN
C        mirroring required
         IF (ABS(RARRAY(6)-RARRAY(5)-PI(2.0)).GT.1E-5) THEN
            CALL RSWAP(RARRAY(5),RARRAY(6))
         END IF
C     code to alter arc directions without opflags
C     used when components are being mirrored
      ELSEIF((PARS(4)*PARS(5)).LT.0.0) THEN
         IF (ABS(RARRAY(6)-RARRAY(5)-PI(2.0)).GT.1E-5) THEN
            CALL RSWAP(RARRAY(5),RARRAY(6))
         ENDIF
      ENDIF
C
      END
C
C--------------------------------------------------------------------
C
C
      SUBROUTINE MV0030(RARRAY,M)
C     ===========================
C1    Vartype            R(6) R(3,3)
C1    Iostatus           I/O   I
C
C2            This subroutine performs a geometric transformation on a 
C2    Center Line whoes data is passed in the real array RARRAY, using
C2    the transformation described in the passed array M. The modified
C2    data is returned in RARRAY.              
C
C2    RARRAY(1) = Center X.
C2    RARRAY(2) = Center Y.
C2    RARRAY(3) = Major radius.
C2    RARRAY(4) = Minor radius. 
C2    RARRAY(5) = Angle.
C2    RARRAY(6) = Border size.
C
      REAL M(3,3),RARRAY(6), TX(3),TY(3),DUMMY,IANG, PI,CANG,DISTXY,
     +     BRDR,ANGLE, CENX,CENY, MAJRAD,MINRAD
      EXTERNAL PI,CANG,DISTXY
C
C     Transform the center.
C
      TX(1) = RARRAY(1)
      TY(1) = RARRAY(2)
      CALL NEWXY(TX(1),TY(1),CENX,CENY,M)               
C
C     Transform the angle and major radius.
C
C     Turn radius into line.
      DUMMY = RARRAY(3)
      IANG = 0.0
      CALL NEWPNT(TX(1),TY(1),RARRAY(3),DUMMY,IANG,RARRAY(5),
     +            TX(2),TY(2))
C     Transform the line.
      CALL NEWXY(TX(2),TY(2),TX(3),TY(3),M)
      ANGLE = CANG(CENX,CENY,TX(3),TY(3))
      MAJRAD = DISTXY(CENX,CENY,TX(3),TY(3))
C
C     Transform the minor radius.
C
      IF (RARRAY(4).GT.0.0) THEN
         MINRAD = RARRAY(4) * (MAJRAD/RARRAY(3))
      ENDIF
C
      RARRAY(1) = CENX
      RARRAY(2) = CENY
      RARRAY(3) = MAJRAD
      RARRAY(4) = MINRAD
      RARRAY(5) = ANGLE
C
      END
C
      SUBROUTINE MV0033(M,OK)
C     ======================
C
C1    vartype        R(3,3) L
C1    iostatus         I    O
C
C2    Subroutine MV0033 performs a geometric transformation
C2    on a linear dimension entity whose data is passed in the
C2    working area RWORK,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C2    OK returned true if moved dimension is a valid one.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      REAL TX1,TY1,TX2,TY2,TX3,TY3,TX4,TY4,M(3,3),
     +     X1,Y1,X2,Y2,X3,Y3,X4,Y4
C
      LOGICAL OK
C
      INTEGER*2 TMIP
C
      INTEGER*4 TMPTOL
C
      CHARACTER*40 TEMP
C
      EXTERNAL NEWXY,DIMF33,DIML03
C
C     store dimension parameters temp.
C     decode control data from scratch arrays and set flags.
      CALL DIMF33(TX1,TY1,TX2,TY2,TX3,TY3,TX4,TY4)

C      IF ( .NOT. REGEN ) THEN
C        CALL MV0DIM(LDIMN,M)
C     ELSE
C     copy original dimension control points.
C     transform dimension control points to new position
C
      IF ( OPFLAG(11).AND.ENDCOD.NE.0 ) THEN
         GOTO (1,2) ENDCOD
 1       CONTINUE
C        do the transformation
         CALL NEWXY(TX1,TY1,X1,Y1,M)
         X2=TX2
         Y2=TY2
         GOTO 3
 2       CONTINUE
C        do the transformation
         CALL NEWXY(TX2,TY2,X2,Y2,M)
         X1=TX1
         Y1=TY1
         GOTO 3
 3       CONTINUE
         X3=TX3
         Y3=TY3
         X4=TX4
         Y4=TY4
      ELSE
         CALL NEWXY(TX1,TY1,X1,Y1,M)
         CALL NEWXY(TX2,TY2,X2,Y2,M)
         CALL NEWXY(TX3,TY3,X3,Y3,M)
         CALL NEWXY(TX4,TY4,X4,Y4,M)
      END IF
C
      IF ( OPFLAG(1) ) THEN
C        scale applied
C        check if paper related parameters need scaled
         IF (OPFLAG(7) .OR. OPFLAG(6)) THEN
C            scale arrow by amount defined
C            in REFDAT(7,3)
             ALNG=ALNG*REFDAT(7,3)
             AWDT=AWDT*REFDAT(7,3)
C            scale gap length  and extension length as well
             GAPL=GAPL*REFDAT(7,3)
             EXTL=EXTL*REFDAT(7,3)
             DTOFF=DTOFF*REFDAT(7,3)
C            scale text parameters
             DTHGT=DTHGT*REFDAT(7,3)
             DTWDT=DTWDT*REFDAT(7,3)
         END IF
      END IF
C     now go and generate the manipulated dimension
C     call dimension generation routine

      CALL DIML03(X1,Y1,X2,Y2,X3,Y3,X4,Y4,.TRUE.,M,OK)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0034(M)
C     ====================
C
C1    vartype          R(3,3)
C1    iostatus           I
C
C2    Subroutine MV0034 performs a geometric transformation
C2    on a angular dimension entity whose data is passed in the
C2    working area RWORK,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      REAL TPDAT(2,4),TLDAT1(6),TLDAT2(6),TX4,TY4,PDAT(2,6),
     +                         LDAT1(6),LDAT2(6),X4,Y4,M(3,3)
C
C     regenerate dimension data
      CALL DIMF34(PDAT,LDAT1,LDAT2,X4,Y4)
C      IF ( .NOT. REGEN ) THEN
C        CALL MV0DIM(ADIMN,M)
C        RETURN
C      ELSE
      CALL NEWXY(PDAT(1,1),PDAT(2,1),TPDAT(1,1),TPDAT(2,1),M)
      CALL NEWXY(PDAT(1,2),PDAT(2,2),TPDAT(1,2),TPDAT(2,2),M)
      CALL NEWXY(PDAT(1,3),PDAT(2,3),TPDAT(1,3),TPDAT(2,3),M)
      CALL NEWXY(PDAT(1,4),PDAT(2,4),TPDAT(1,4),TPDAT(2,4),M)
C
      CALL NEWXY(LDAT1(1),LDAT1(2),TLDAT1(1),TLDAT1(2),M)
      CALL NEWXY(LDAT1(4),LDAT1(5),TLDAT1(4),TLDAT1(5),M)
      TLDAT1(3)=0.0
      TLDAT1(6)=0.0
C
      CALL NEWXY(LDAT2(1),LDAT2(2),TLDAT2(1),TLDAT2(2),M)
      CALL NEWXY(LDAT2(4),LDAT2(5),TLDAT2(4),TLDAT2(5),M)
      TLDAT2(3)=0.0
      TLDAT2(6)=0.0
C
      CALL NEWXY(X4,Y4,TX4,TY4,M)
C
      IF  (OPFLAG(1)) THEN
C        scale set,so scale arrow height and width
         IF (OPFLAG(7) .OR. OPFLAG(6)) THEN
C            scale arrow by amount defined
C            in REFDAT(7,3)
             ALNG=ALNG*REFDAT(7,3)
             AWDT=AWDT*REFDAT(7,3)
C            scale gap length  and extension length as well
             GAPL=GAPL*REFDAT(7,3)
             EXTL=EXTL*REFDAT(7,3)
C            scale text parameters
             DTHGT=DTHGT*REFDAT(7,3)
             DTWDT=DTWDT*REFDAT(7,3)
         END IF
      END IF
C     transform dim text now
      CALL DIMA03(TPDAT,TLDAT1,TLDAT2,TX4,TY4)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0035(DIMTYP,M)
C     ===========================
C
C1    vartype           I2      R(3,3)
C1    iostatus           I      I
C
C2    Subroutine MV0035 performs a geometric transformation
C2    on a radial dimension entity whose data is passed in the
C2    working area RWORK,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
      include 'include/entity.inc'
      include 'include/dhead.inc'
C
      REAL TX1,TY1,TX2,TY2,TX3,TY3,TXC,TYC,M(3,3),
     +     X1,Y1,X2,Y2,X3,Y3,XC,YC,R,RFDAT(20),SANG,EANG
     1     ,DISTXY
C
      CHARACTER*40 TEMP
C
      INTEGER*2 DIMTYP
      INTEGER*4 TMPTOL
c
      LOGICAL OK
C
      EXTERNAL NEWXY,DIMF35,DIMR03,DISTXY
C
C               
C     decode control data from scratch arrays and set flags.
      CALL DIMF35(DIMTYP,TX1,TY1,TX2,TY2,TX3,TY3,TXC,TYC,R,SANG,EANG)
C     copy original dimension control points.
C      IF ( .NOT. REGEN ) THEN
C          CALL MV0DIM(RDIMN,M)
C          RETURN
C      ELSE
C     transform dimension control points to new position
      CALL NEWXY(TX1,TY1,X1,Y1,M)
      CALL NEWXY(TX2,TY2,X2,Y2,M)
      CALL NEWXY(TX3,TY3,X3,Y3,M)
      IF ( DIMTYP .NE. GLABEL ) CALL NEWXY(TXC,TYC,XC,YC,M)
      IF ( OPFLAG(1) ) THEN
C        scale applied
C        scale the radius by factor.
C         IF ( DIMTYP .NE. GLABEL ) R=R*REFDAT(7,3)
         IF ( DIMTYP .NE. GLABEL ) THEN
C          scaling required on radius
           R=DISTXY(XC,YC,X1,Y1)
         END IF
C        check if paper related parameters need scaled
         IF (OPFLAG(7) .OR. OPFLAG(6)) THEN
C           scale arrow by amount defined
C           in REFDAT(7,3)
            ALNG=ALNG*REFDAT(7,3)
            AWDT=AWDT*REFDAT(7,3)
C           scale gap length  and extension length as well
            GAPL=GAPL*REFDAT(7,3)
            EXTL=EXTL*REFDAT(7,3)
C           scale text parameters
            DTHGT=DTHGT*REFDAT(7,3)
            DTWDT=DTWDT*REFDAT(7,3)
         END IF
      END IF
C     now go and generate the manipulated dimension
      IF ( DIMTYP .NE. GLABEL ) THEN
          CALL DIMR03(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,OK)
      ELSE
C         set edit label flag false
          LEDIT = .TRUE.
          CALL DIMN03(X1,Y1,X2,Y2,X3,Y3,OK)
		  LEDIT = .FALSE.
      END IF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0043(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0043 performs a geometric transformation
C2    on a dimension line terminator whose data is passed in
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      INTEGER*4 I
      REAL RARRAY(6),M(3,3),CANG,DISTXY,
     +     TX1,TY1,TRX1,TRY1,
     1     TX2,TY2,TRX2,TRY2,
     3     ANGT,ANG1T,ANG2T,ANG1,ANG2,ANG3
C
      EXTERNAL NEWXY,VV00L5,DISTXY,CANG
C     copy the terminator origin point for transformation
      TX1=RARRAY(1)
      TY1=RARRAY(2)
C     set TX2 to TX1 + arrow length
      TX2=TX1+RARRAY(5)
      TY2=TY1
C     first reposition the all two points
      CALL NEWXY(TX1,TY1,TRX1,TRY1,M)
      CALL NEWXY(TX2,TY2,TRX2,TRY2,M)
C
C     Rotation
      ANG1=CANG(TRX1,TRY1,TRX2,TRY2)
 
      ANG1T=RARRAY(3)
      ANG2T=RARRAY(4)
 
      CALL VV00L5(ANG1T,ANG2T,ANG3,TX1,TY1,ANG1,RARRAY(3),
     +                                        RARRAY(4),ANGT)
C
 
      RARRAY(5)=RARRAY(5)*
     +   DISTXY(TRX1,TRY1,TRX2,TRY2)/
     1   DISTXY(TX1,TY1,TX2,TY2)
      RARRAY(6)=RARRAY(6)*
     +   DISTXY(TRX1,TRY1,TRX2,TRY2)/
     1   DISTXY(TX1,TY1,TX2,TY2)
C
C     now consider the height and width
      IF ( OPFLAG(7) ) THEN
c        NOTE this scaling is done above
C        scale text difference between the two
c         RARRAY(5)=RARRAY(5)*
c     +   DISTXY(TRX1,TRY1,TRX2,TRY2)/
c     1   DISTXY(TX1,TY1,TX2,TY2)
C
c         RARRAY(6)=RARRAY(6)*
c     +   DISTXY(TRX1,TRY1,TRX2,TRY2)/
c     1   DISTXY(TX1,TY1,TX2,TY2)
C
      ELSE IF ( OPFLAG(6) ) THEN
C        Proportional text scaling
         RARRAY(5)=RARRAY(5)*REFDAT(7,3)
         RARRAY(6)=RARRAY(6)*REFDAT(7,3)
C
      END IF
C
C     reposition the origin point
      CALL NEWXY(TX1,TY1,TRX1,TRY1,M)
      RARRAY(1)=TRX1
      RARRAY(2)=TRY1
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0046(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0046 performs a geometric transformation
C2    on a dimension trailer record , whose data is stored in
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      REAL RARRAY(6),M(3,3)
C
C      IF ( OPFLAG(1) ) THEN
C        scale applied
C        check if paper related parameters need scaled
         IF (OPFLAG(7) .OR. OPFLAG(6)) THEN
C           scale text block size and DTOFF by amount defined
C           in REFDAT(7,3)
            RARRAY(1)=RARRAY(1)*REFDAT(7,3)
            RARRAY(5)=RARRAY(5)*REFDAT(7,3)
            RARRAY(6)=RARRAY(6)*REFDAT(7,3)
         END IF
C      END IF
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0050(PMIP,STRTP,ENDP)
C     ==================================
C
C1    vartype            I2    I4    I4
C1    iostatus           I     I     I
C
C2    Subroutine MV0050 handles the copying of
C2    GROUPS during a general move-copy operation
C2    and takes care of creation of new RELATION
C2    records etc to maintain full functionality
C2    of copied groups
C2    PMIP passes the parent MI pointer of the group
C2    in question,STRTP,ENDP indicate the start and end
C2    pointers respectively of the group data in the MI
C2    after transformation and copying.This routine
C2    edits the MI data for those entities,and creates
C2    a relation structure entry for the group.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 I,NN,K,II,IJ,JJ,NRRECS,NXTENT,STRTP,ENDP
C
      INTEGER*2 MP,I2,J
      INTEGER*2 PMIP,PREL
C
      LOGICAL OK,THISEG,LASTEG,STARTG
      LOGICAL EOG,NEWG
C
      EXTERNAL DIR500,DIM500,DIW500,DRR950,DRW950,DRW951
C
C     create relation list to link all
C     of the copied grouped entities.
C     read the parent MI
      CALL DIR500(PMIP,OK)
C     get relation pointer
      PREL=IMBUFF(10)
C     read relation header
      CALL DRR950(PREL,OK)
C     set rel pointer to next position
      IMBUFF(10)=NRLPOS
C     ensure normal status
      IMBUFF(1)=10
C     ensure PMIP cleared
      IMBUFF(8)=0
C     create new group header
      CALL DIW500(PMIP,OK)
C     set pointer to new MIP
      RLBUFF(3)=PMIP
C     write new relation header
      CALL DRW951(PREL,OK)
C     find number of relation records required
      NRRECS=RLBUFF(4)
C     now write MILIST to relation
      RLBUFF(1)=MILIST
C
      NXTENT=STRTP
      IF (NRRECS.EQ.1) GOTO 40
      DO 30 IJ=1,NRRECS-1
C        fill the record with MI pointers
         DO 35 JJ=4,10
C           read the MI data for the entity
C           and set GROUP status
            I2=NXTENT
            CALL DIR500(I2,OK)
C           point to parent MI
            IMBUFF(8)=PMIP
C           write modified MI data to file
            MIP=NXTENT
            I2=NXTENT
            CALL DIM500(I2,OK)
            RLBUFF(JJ)=NXTENT
            NXTENT=NXTENT+1
 35      CONTINUE
C        write the record to relation file
         CALL DRW951(PREL,OK)
 30   CONTINUE
C
 40   CONTINUE
C     last relation record to be written
C     clear relation buffer
      DO 20 IJ=4,10
         RLBUFF(IJ)=0
 20   CONTINUE
C     fill the record with MI pointers
      DO 41 J=NXTENT,ENDP
C        read the MI data for the entity
C        and set GROUP status
         CALL DIR500(J,OK)
C        point to parent MI
         IMBUFF(8)=PMIP
C        write back modified MI data
         MIP=J
         CALL DIM500(J,OK)
         RLBUFF(J-NXTENT+4)=J
 41   CONTINUE
C     write the last record to relation file
      CALL DRW950(PREL,OK)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0066(M,MC)
C     =======================
C
C1    vartype        R(3,3) R(3,3)
C1    iostatus         I      IO
C
C2    Subroutine MV0066 performs a geometric transformation
C2    on a COMPONENT whose data is passed in the
C2    working area RWORK,using the transformation described in the
C2    passed array M.The array MC contains the component transform
C2    to be modified for use in the repositioned component.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
C
      INTEGER I,J
      REAL TX1,TY1,TX2,TY2,M(3,3),MC(3,3),MM(3,3)
C
      EXTERNAL NEWXY,MULT3M
C
C     now concatenate to create new component transform
      CALL MULT3M(MC,M,MM)
C     copy result into component transform for return
      DO 20 I=1,3
         MC(1,I)=MM(1,I)
         MC(2,I)=MM(2,I)
         MC(3,I)=MM(3,I)
 20   CONTINUE
C     transform containment box first
C     first diagonal
      TX1=RWORK(1,1)
      TY1=RWORK(2,1)
      TX2=RWORK(4,1)
      TY2=RWORK(5,1)
      CALL NEWXY(TX1,TY1,RWORK(1,1),RWORK(2,1),M)
      CALL NEWXY(TX2,TY2,RWORK(4,1),RWORK(5,1),M)
C     second diagonal
      TX1=RWORK(1,2)
      TY1=RWORK(2,2)
      TX2=RWORK(4,2)
      TY2=RWORK(5,2)
      CALL NEWXY(TX1,TY1,RWORK(1,2),RWORK(2,2),M)
      CALL NEWXY(TX2,TY2,RWORK(4,2),RWORK(5,2),M)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0085(RARRAY,M)
C     ===========================
C
C1    vartype            R(6) R(3,3)
C1    iostatus           I/O    I
C
C2    Subroutine MV0085 performs a geometric transformation
C2    on a text parameter block whose data is passed in the real array
C2    RARRAY,using the transformation described in the
C2    passed array M.The logical flags stored in the work area
C2    MOVDAT.INC are also used during the transformation.
C2    The modified data is returned in RARRAY.
C
      include 'include/movdat.inc'
C
      REAL DEG,RARRAY(6),M(3,3),RAD,CANG,DISTXY,
     +     TX1,TY1,TX2,TY2,TX3,TY3,ANGT,ANG1T,ANG2T,
     1    TRX1,TRY1,TRX2,TRY2,TRX3,TRY3,ANG1,ANG2,COS,SIN
      INTRINSIC COS,SIN
C
      EXTERNAL NEWXY,DEG,RAD,CANG,DISTXY
C
C      WRITE(10,*) '[MV0085]  IN:',RARRAY
C      WRITE(10,*) 'OPFLAG',OPFLAG(7),OPFLAG(6),OPFLAG(1)
C     copy the text origin point for transformation
      TX1=RARRAY(1)
      TY1=RARRAY(2)
C     do the transformation
      ANGT=RARRAY(5)
      TX2=TX1+ABS(RARRAY(3))*COS(RAD(ANGT))
      TY2=TY1+ABS(RARRAY(3))*SIN(RAD(ANGT))
C
      ANGT=ANGT+90.0
      TX3=TX1+ABS(RARRAY(4))*COS(RAD(ANGT))
      TY3=TY1+ABS(RARRAY(4))*SIN(RAD(ANGT))
C
C     first reposition the all three points
      CALL NEWXY(TX1,TY1,TRX1,TRY1,M)
      CALL NEWXY(TX2,TY2,TRX2,TRY2,M)
      CALL NEWXY(TX3,TY3,TRX3,TRY3,M)
C
      RARRAY(5)=DEG(CANG(TRX1,TRY1,TRX2,TRY2))
      RARRAY(1)=TRX1
      RARRAY(2)=TRY1
C
C     now consider the height and width
C        scale set,so scale height and width
      IF ( OPFLAG(7) ) THEN
C        scale text difference between the two
C         RARRAY(3)=RARRAY(3)*
C     +   DISTXY(TRX1,TRY1,TRX2,TRY2)/
C     1   DISTXY(TX1,TY1,TX2,TY2)
CC
C         RARRAY(4)=RARRAY(4)*
C     +   DISTXY(TRX1,TRY1,TRX3,TRY3)/
C     1   DISTXY(TX1,TY1,TX3,TY3)
         RARRAY(3)=RARRAY(3)*REFDAT(7,3)
         RARRAY(4)=RARRAY(4)*REFDAT(7,3)
CC
      ELSE IF ( OPFLAG(6) ) THEN
C        Proportional text scaling
         RARRAY(3)=RARRAY(3)*REFDAT(7,3)
         RARRAY(4)=RARRAY(4)*REFDAT(7,3)
C
      END IF
C
      IF ( OPFLAG(3) ) THEN
C        mirror flag
C
C        calculate angle of text base line
         ANG1 =DEG(CANG( TX1, TY1, TX2, TY2))
C        calculate angle of text height
         ANG2 =DEG(CANG( TX1, TY1, TX3, TY3))
C        same after transform
         ANG1T=DEG(CANG(TRX1,TRY1,TRX2,TRY2))
         ANG2T=DEG(CANG(TRX1,TRY1,TRX3,TRY3))
C
         IF ( MOD(ANG1+90.0,360.0).NE.ANG2 ) THEN
C           this is if the text has already been mirrored
            ANGT=(ANG1T-ANG1)-(ANG2T-ANG2)
         ELSE
C
            ANGT=(ANG1-ANG1T)-(ANG2-ANG2T)
         END IF
C
         IF ( ANGT.GT.179 ) THEN
            RARRAY(3)=-RARRAY(3)
            RARRAY(5)=MOD(RARRAY(5)+180.0,360.0)
         ELSE IF ( ANGT.LT.179 ) THEN
            RARRAY(4)=-RARRAY(4)
         ELSE
C
         END IF
C
      END IF
C
 
C      WRITE(10,*) '[MV0085] OUT:',RARRAY
 
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0166(M,MC)
C     =======================
C
C1    vartype        R(3,3) R(3,3)
C1    iostatus         I      IO
C
C2    Subroutine MV0066 performs a geometric transformation
C2    on a COMPONENT whose data is passed in the
C2    working area RWORK,using the transformation described in the
C2    passed array M.The array MC contains the component transform
C2    to be modified for use in the repositioned component.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
C
      INTEGER I,J
      REAL TX1,TY1,TX2,TY2,M(3,3),MC(3,3),MM(3,3),PARS(9)
      REAL DM(3,3),NX,NY,TM(3,3),PARS1(9)
C
      EXTERNAL NEWXY,MULT3M
C
C     now concatenate to create new component transform
      CALL I3M(DM)
      CALL I3M(TM)
      CALL DECODM(MC,PARS)
      CALL DECODM(M,PARS1)
C     create a transform of the translation only
      CALL TRAN2D(-PARS(1),-PARS(2),DM)
      CALL MULT3M(MC,DM,MM)
C     get the new location point
      CALL NEWXY(PARS(1),PARS(2),NX,NY,M)
      CALL I3M(DM)
      CALL TRAN2D(NX,NY,DM)
C     new matrix
      CALL MULT3M(MM,DM,TM)
      CALL DECODM(TM,PARS1)
C     copy result into component transform for return
      DO 30 I=1,3
         MM(1,I)=TM(1,I)
         MM(2,I)=TM(2,I)
         MM(3,I)=TM(3,I)
 30   CONTINUE
      DO 20 I=1,3
         MC(1,I)=MM(1,I)
         MC(2,I)=MM(2,I)
         MC(3,I)=MM(3,I)
 20   CONTINUE
C     transform containment box first
C     first diagonal
      TX1=RWORK(1,1)
      TY1=RWORK(2,1)
      TX2=RWORK(4,1)
      TY2=RWORK(5,1)
      CALL NEWXY(TX1,TY1,RWORK(1,1),RWORK(2,1),M)
      CALL NEWXY(TX2,TY2,RWORK(4,1),RWORK(5,1),M)
C     second diagonal
      TX1=RWORK(1,2)
      TY1=RWORK(2,2)
      TX2=RWORK(4,2)
      TY2=RWORK(5,2)
      CALL NEWXY(TX1,TY1,RWORK(1,2),RWORK(2,2),M)
      CALL NEWXY(TX2,TY2,RWORK(4,2),RWORK(5,2),M)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MV0DIM(DIMTYP,M)
C     ===========================
C1    vartype            I2    R(3,3)
C1    iostatus           I     I
C
C2    Subroutine MV0DIM is a general routine for Repositioning
C2    any  dimension type in the display system.
C
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/masti.inc'
C
      INTEGER*2 DIMTYP,I
      REAL M(3,3)
      LOGICAL OK
      EXTERNAL MV0003,MV0005,MV0085,MV0043
C
      OK=.FALSE.
C     Loop through the stored dimension records.
C      WRITE(10,*)'[MV0DIM] Here now'
C     Read first record and manipulate accordingly.
      IF ( DIMTYP .EQ. LDIMN .OR. DIMTYP .EQ. ADIMN
     +     .OR. DIMTYP .EQ. GLABEL) THEN
C        header is of linear type.
         CALL MV0003(RWORK(1,1),M)
      ELSE
C        header is of arc type.
         CALL MV0005(RWORK(1,1),M)
      END IF
C     now do rest of records.
      DO 101 I=2,RECCNT(1)
C      WRITE(10,*) '[MV0DIM]',I,IWORK(1,I)
         IF ( IWORK(1,I) .EQ. HEADER ) THEN
C           Must be a line so use data direct
            CALL MV0003(RWORK(1,I),M)
         ELSE IF ( IWORK(1,I) .EQ. TEXSEG ) THEN
C           was a text record
C         WRITE(10,*) '[MV0DIM]',RWORK(1,I)
            CALL MV0085(RWORK(1,I),M)
         ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
C           was arrowhead so must decode the arrowhead parameters.
            CALL MV0043(RWORK(1,I),M)
         ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
C           Was a line segment so use data direct
            CALL MV0003(RWORK(1,I),M)
         ELSE IF ( IWORK(1,I) .EQ. ARCSEG ) THEN
C           was an arc type segment
            CALL MV0005(RWORK(1,I),M)
         ELSE
C           reset the text block size if required.
            CALL MV0046(RWORK(1,I),M)
         END IF
 101   CONTINUE
C      end of draw routine.
       OK=.TRUE.
       END
C
C--------------------------------------------------------------------
C
      SUBROUTINE MVDIM0(M,DIMTYP,OK)
C     =============================
C
C1    vartype          R(3,3) I2  L
C1    iostatus         I      I   O
C
      include 'include/entity.inc'
C
      REAL M(3,3)
      LOGICAL OK
      INTEGER*2 DIMTYP
C
      IF (DIMTYP.EQ.LDIMN) THEN
         CALL MV0033(M,OK)
      ELSE IF (DIMTYP.EQ.RDIMN .OR. DIMTYP.EQ.DDIMN .OR.
     +                            DIMTYP.EQ.GLABEL) THEN
         CALL MV0035(DIMTYP,M)
      ELSE IF (DIMTYP.EQ.ADIMN) THEN
         CALL MV0034(M)
      END IF
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE RSTDDT()
C     ====================
C1    No arguments required.
C
C2    Subroutine RSTDDT stores in temp work array the
C2    dimension text , arrow etc. parameters.
C2    The modified data is held in RFDAT.
C
      include 'include/tmpdat.inc'
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
C     save data
C     recover text parameters before leaving
      DTHGT=RFDAT(1)
      DTWDT=RFDAT(2)
      ALNG=RFDAT(3)
      AWDT=RFDAT(4)
      GAPL=RFDAT(5)
      EXTL=RFDAT(6)
      PREC=RFDAT(7)
      LTOL=RFDAT(8)
      UTOL=RFDAT(9)
      TOLTYP=RFDAT(10)
      DTOFF=RFDAT(11)
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE SAVDDT()
C     ====================
C1    No argumentsrequired.
C
C2    Subroutine SAVDDT stores in temp work array the
C2    dimension text , arrow etc. parameters.
C2    The modified data is held in RFDAT.
C
      include 'include/tmpdat.inc'
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
C     save data
      RFDAT(1)=DTHGT
      RFDAT(2)=DTWDT
      RFDAT(3)=ALNG
      RFDAT(4)=AWDT
      RFDAT(5)=GAPL
      RFDAT(6)=EXTL
      RFDAT(7)=PREC
      RFDAT(8)=LTOL
      RFDAT(9)=UTOL
      RFDAT(10)=TOLTYP
      RFDAT(11)=DTOFF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE TYPARR(OK)
C     ===================
C1    vartype            L
C1    iostatus           O
C1
C1
C
C2    This function toggles the ARRAY type menu
C2    cell and sets the array control flags for
C2    menu type.
      INTEGER*4 MNCODE 
      LOGICAL OK
      include 'include/movdat.inc'
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
      EXTERNAL MENPOP,GTDMEN,GTCLRC,GTMCLO,GTMCHI
C
      MNCODE=11
      CALL MENPOP(MNCODE,OK)
         IF(.NOT.OK) THEN
            ARRTYP = .FALSE.
            OK = .FALSE.
         ELSE
C           clear  the array menu cell
            CALL GTMCLO(MEN,CELLN)
            CALL GTCLRC(MEN,CELLN)
            GTMULT = .TRUE.
C
            IF(CCMD .EQ. 'k') THEN
C             so let's make it radial with elements
C             retaining their original gravity
              RADIAL = .TRUE.
              ROTAT  = .FALSE.
              CALL GTDMEN(36,3)
C
            ELSEIF(CCMD .EQ. 'd') THEN
C             so let's make it radial with elements
C             rotated
              ROTAT = .TRUE.
              RADIAL = .TRUE.
              CALL GTDMEN(37,3)
C
            ELSEIF(CCMD .EQ. 'e') THEN
C             let's make it rectangular
              RADIAL = .FALSE.
              BORDER = .FALSE.
              CALL GTDMEN(38,3)
C
            ELSEIF(CCMD .EQ. 'f') THEN
C             so let's make it a rectangular border
              RADIAL = .FALSE.
              BORDER = .TRUE.
              CALL GTDMEN(39,3)
C
            ENDIF
C           flag the fact that the array type has been set
C           and hilite the array menu cell
            CALL GTMCHI(MEN,CELLN)
            ARRTYP = .TRUE.
C           OK = .TRUE.
         ENDIF
      END
C
C--------------------------------------------------------------------
C
