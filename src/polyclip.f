C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 polyclip.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE POLY_BUILD(NUM,IX,IY,NOE)
C     SUBROUTINE POLY_CLIP(NX,NY,NOE,WINDOW,WINTAG,ST)
C     SUBROUTINE POLY_DRAW(NX,NY,NOE)
C     SUBROUTINE POLY_EAT(XMIN,YMIN,XMAX,YMAX,XP,YP,POLY,NOP,RES)
C     SUBROUTINE POLY_SORT(NX,NY,NOE,DIR)
C     SUBROUTINE POLY_SWAP(NX,NY,NOE)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE POLY_BUILD(NUM,IX,IY,NOE)
C     ====================================
C1    VARTYPE              I2   I2 I2 I2
C1    IOSTAT               I    I  I  I
C
C2    This routine will actually build and draw a set of polygons
C2    against the viewports which are curently in use and against
C2    the drawing code requested (DDCODE)
C2    It must be used where polygons must be clipped against the windows.
C2    The limiting factor is 200 vertices and one entry and exit point per side
C2    of the window. Thus four polygons can be clipped per window. Stack size
C2    is set at 10 polygons, more than enough. If any smart arse is thinking tha
C2    that the legendary C will cope with this in recursion then forget it since
C2    arrays can only be passed by ref.
C
      include   'include/viewport.inc'
      include   'include/apollo.inc'
      include   'include/polyclip.inc'
      include   'include/icon.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 I,J,NOE,IX(NOE),IY(NOE),JJ
      INTEGER*2 NX(200),NY(200)
      INTEGER*2 STACKX(MAXPG,10)
      INTEGER*2 STACKY(MAXPG,10)
      INTEGER*2 STACKN(10)
      INTEGER*2 STACKW(10)
      INTEGER*2 STACKT(5,10)
      INTEGER*2 WINTAG
      INTEGER*2 NUM
      INTEGER*4 STKP
      INTEGER*4 LIST(MAXVP)
      INTEGER*4 CODE,ST
      INTEGER*4 WINCLP,CLP
      LOGICAL VISAB,DIR
      INTEGER*4 OX1,OY1,OX2,OY2
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,CBITM,WINDOW(4),STATUS,FILL
      DATA STACKT/50*0/
C
C     if an icon the dont bother
      IF(NUM.GT.0) THEN
           IF(ICNUM(NUM)) RETURN
      ENDIF
C     segment has allready been clipped into place thus no need to clip
C     our own polygon
C     set current drawing bitmap
C     copy in to our work array
      DO 2000 I=1,NOE
          NX(I) = IX(I)
          NY(I) = IY(I)
2000  CONTINUE
 
      FILL = TOOLPEN_COLOR
      CALL GPR_$INQ_BITMAP(CBITM,ST)
      CALL GPR_$SET_BITMAP(DISPDE,ST)
C     make sure we can see this one please
      CALL TOOLPEN(FILL)
      J = 0
      IF(NUM.EQ.0) THEN
C         build a list of all viable windows to clip and do it
          DO 30 I=1,MAXVP
              IF(VPNUM(I)) THEN
                  J = J + 1
                  LIST(J) = I
              ENDIF
 30       CONTINUE
      ELSE
         DO 20 I=1,MAXVP
C
C             build a list of polygons to clip segment
              IF(OBSLST(NUM,I).AND.I.NE.NUM) THEN
                 J = J + 1
                 LIST(J) = I
             ENDIF
 20      CONTINUE
      ENDIF
C
C     set clipping active please
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,STATUS)
      IF(J.EQ.0) THEN
C         draw polygon as normal
          CALL POLY_DRAW(NX,NY,NOE)
          CALL GPR_$SET_BITMAP(CBITM,ST)
          RETURN
      ENDIF
C
      WINTAG = 0
      STKP = 0
      CLP = 1
C     set segment values
100   CONTINUE
C
C
C     determine clip window
      WINCLP = LIST(CLP)
C     get window includeing border
      WINDOW(1) = VIEWEX(1,WINCLP)-1
      WINDOW(2) = VIEWEX(2,WINCLP)-1
      WINDOW(3) = VIEWEX(3,WINCLP)+2
      WINDOW(4) = VIEWEX(4,WINCLP)+2
C
      CALL POLY_CLIP(NX,NY,NOE,WINDOW,WINTAG,ST)
      IF(POLYN.EQ.0) THEN
C         This polygon horsed
          CLP = J
      ENDIF
C     returned polygons are in global data space
      IF(CLP.EQ.J) THEN
C         we have all windows draw polygons as required
          DO 70 I=1,POLYN
              CALL POLY_DRAW(NPOLX(1,I),NPOLY(1,I),NPOLYN(I))
 70       CONTINUE
C         if any thing on the stack then remove and continue as before
          IF(STKP.GT.0) THEN
C             save current window number to continue later
              CLP = STACKW(STKP) +1
C             recover urrent total element
              NOE = STACKN(STKP)
C             reset window tag count
              WINTAG = 0
              DO 80 JJ=1,NOE
C                 save contents of split polygon on stack
                  NX(JJ) = STACKX(JJ,STKP)
                  NY(JJ) = STACKY(JJ,STKP)
 80           CONTINUE
          ELSE
              CALL GPR_$SET_BITMAP(CBITM,ST)
              RETURN
          ENDIF
C         decrement stack
          STKP = STKP-1
          GOTO 100
      ENDIF
 
      IF(POLYN.GT.1) THEN
          DO 50 I=2,POLYN
              STKP = STKP+1
C             save current window number to continue later
              STACKW(STKP) = CLP
C             save current total element
              STACKN(STKP) = NPOLYN(I)
              DO 60 JJ=1,NPOLYN(I)
C                 save contents of split polygon on stack
                  STACKX(JJ,STKP) = NPOLX(JJ,I)
                  STACKY(JJ,STKP) = NPOLY(JJ,I)
 60           CONTINUE
 50       CONTINUE
C         now we save data load main array and continue splitting
C
      ENDIF
C
      DO 95 JJ=1,NPOLYN(1)
C         save contents of split polygon on stack
          NX(JJ) = NPOLX(JJ,1)
          NY(JJ) = NPOLY(JJ,1)
 95   CONTINUE
      NOE = NPOLYN(1)
C     window counter
      CLP = CLP+1
      GOTO 100
C
C
      END
 
      SUBROUTINE POLY_CLIP(NX,NY,NOE,WINDOW,WINTAG,ST)
C     ================================================
C1    VARTYPE              I2 I2 I2    I4    I2    I4
C1    IOSTAT               I  I  I     I     I     O
C
C2    This routine will generate a new polygon and clip the
C2    old one It can handle a split of the polygon or
C2    a partial eating of the original the results are placed
C2    in global data
C
      include   'include/polyclip.inc'
C
      INTEGER*2 NOE,NX(NOE),NY(NOE),WINTAG,COUNT
      INTEGER*4 WINDOW(4),ST
      INTEGER*4 XMIN,YMIN,XMAX,YMAX
      INTEGER*4 POLY(2,10),NOP,RES
      INTEGER*4 VERTEX(20)
C
C
      INTEGER*4 OPOLYP,NPOLYP,STATUS
      INTEGER*4 CODE,SX,SY
      INTEGER*4 X1,Y1,X2,Y2
      INTEGER*4 POLY_SIDE,RESTART,FSTAT,OLDP,NEWP
      INTEGER*4 I,J
      LOGICAL VISAB,PNTBOX,ONSIDE,FIRST,FINISH,POLY_OBS
      LOGICAL IGNORE
      LOGICAL OUTSIDE
      CHARACTER*80 TEMP
C
      EXTERNAL POLY_SIDE,POLY_OBS
C     store clip window
      XMIN = WINDOW(1)
      YMIN = WINDOW(2)
      XMAX = WINDOW(3)
      YMAX = WINDOW(4)
C
C     set up corners
      CORNER(1,1) = XMIN
      CORNER(2,1) = YMAX
      CORNER(1,2) = XMAX
      CORNER(2,2) = YMAX
      CORNER(1,3) = XMAX
      CORNER(2,3) = YMIN
      CORNER(1,4) = XMIN
      CORNER(2,4) = YMIN
C
      I=0
      FINISH = .FALSE.
      NUMIP = 0
C     save start point
      SX = NX(1)
      SY = NY(1)
C     start by doing the clipping of each segment
10    CONTINUE
          I=I+1
          USEDP(I) = .FALSE.
C         set vector
          X1 = NX(I)
          Y1 = NY(I)
C         consider end element here
          IF(I.EQ.NOE) THEN
              X2 = NX(1)
              Y2 = NY(1)
          ELSE
              X2 = NX(I+1)
              Y2 = NY(I+1)
          ENDIF
          FINISH = (SX.EQ.X2.AND.SY.EQ.Y2).OR.I.EQ.NOE
 
C
          CODE = 0
          CALL VCLIP(X1,Y1,X2,Y2,XMIN,YMIN,XMAX,YMAX,VISAB,CODE)
C
C         save the code for later use
          VERTEX(I) = CODE
          IF(CODE.EQ.0) THEN
C             both point are inside the window
          ELSEIF(CODE.EQ.1) THEN
C             both point here inside the window
          ELSEIF(CODE.EQ.2) THEN
C             FULL clip both entry and exits
              IF(.NOT.(X1.EQ.X2.AND.Y1.EQ.Y2)) THEN
C                 Do this if we are not on a corner
                  NUMIP = NUMIP + 1
                  INTPS(1,NUMIP) = X1
                  INTPS(2,NUMIP) = Y1
                  TYPE(NUMIP) = ENTRY
                  ASSOC(NUMIP) = I
                  USEDI(NUMIP) = .FALSE.
                  NUMIP = NUMIP + 1
                  INTPS(1,NUMIP) = X2
                  INTPS(2,NUMIP) = Y2
                  TYPE(NUMIP) = EXIT
                  ASSOC(NUMIP) = I
                  USEDI(NUMIP) = .FALSE.
              ENDIF
C
          ELSEIF(CODE.EQ.3) THEN
C             PARTIAL clip entry point
              NUMIP = NUMIP + 1
              INTPS(1,NUMIP) = X2
              INTPS(2,NUMIP) = Y2
              TYPE(NUMIP) = EXIT
              ASSOC(NUMIP) = I
              USEDI(NUMIP) = .FALSE.
          ELSEIF(CODE.EQ.4) THEN
C             PARTIAL clip exit point
              NUMIP = NUMIP + 1
              INTPS(1,NUMIP) = X1
              INTPS(2,NUMIP) = Y1
              TYPE(NUMIP) = ENTRY
              ASSOC(NUMIP) = I
              USEDI(NUMIP) = .FALSE.
          ENDIF
C     finish condition
      IF(.NOT.FINISH) GOTO 10
C
C     for speed test unclipped polygon
      IF(NUMIP.EQ.0) THEN
          POLYN = 1
          NEWP = 0
          OUTSIDE = .FALSE.
          DO 600 I=1,NOE
              X1 = NX(I)
              Y1 = NY(I)
C             check for point in window if not then
C             must be outside as a full line
              IF(.NOT.PNTBOX(XMIN-1,YMIN-1,XMAX+1,YMAX+1,X1,Y1)) THEN
C                 One point point is outside the window
                  OUTSIDE = .TRUE.
              ENDIF
600       CONTINUE
          IF(OUTSIDE) THEN
              DO 610 I=1,NOE
                 NPOLX(I,POLYN) = NX(I)
                 NPOLY(I,POLYN) = NY(I)
610           CONTINUE
C             retruning njumber of elements
              NPOLYN(POLYN) = NOE
          ELSE
              POLYN = 0
          ENDIF
C         no more to do here go home
          GOTO 700
      ENDIF
C
C********************************************
C     CLIPPING ALGORITHM SEE DAVY FOR DETAILS
C********************************************
C
C     now comes the good bit
C     generate new polygons
C
C     set variables concerner with this section
      FINISH = .FALSE.
      FIRST = .TRUE.
      STATUS = 0
      ONSIDE = .FALSE.
      NEWP = 0
      OLDP = 0
      RESTART = 0
 
      POLYN = 1
 
 50   CONTINUE
C
100   CONTINUE
C
C
C     increment old polygon pointer
      OLDP = OLDP +1
      IF(OLDP.GT.NOE) THEN
C         loop round to start point on polygon
          X1 = NX(1)
          Y1 = NY(1)
          IF(SX.EQ.X1.AND.SY.EQ.Y1.AND..NOT.FIRST) THEN
C             make sure first point is not there and the polygon has started
              FINISH = .TRUE.
              FSTAT = 1
C             complete polygon found
              GOTO 300
          ELSE
CC             simply ran out of points
              POLYN = POLYN -1
              FSTAT = 3
              GOTO 500
          ENDIF
      ELSE
C         first check is point in the window
          X1 = NX(OLDP)
          Y1 = NY(OLDP)
      ENDIF
C     test point for entry/exit status
      IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,X1,Y1)) THEN
C         point is either inside the window or 
          IGNORE = .TRUE.
          IF(POLY_SIDE(X1,Y1,XMIN,YMIN,XMAX,YMAX).GT.0) THEN 
              CODE = VERTEX(OLDP)
              IF(CODE.EQ.0) THEN
C                 line is outside needs to be part of it
                  IGNORE = .FALSE.
              ELSEIF(CODE.EQ.1) THEN
C                 inside the window
                  IGNORE = .TRUE.
              ELSE
C                 point lies on a side only
                  IGNORE = .FALSE.
                  DO 120 I=1,NUMIP
                      IF(ASSOC(I).EQ.OLDP.AND.TYPE(I).EQ.EXIT) THEN
C                         we have an exit point must ignore
                          IGNORE = .TRUE.
                      ENDIF
 120              CONTINUE
              ENDIF
          ENDIF
C         do we have a condition to ignore the point ?
          IF(IGNORE) THEN
              USEDP(OLDP) = .TRUE.
C             the status of the point is still exit
              STATUS = EXIT
C             we cannot have this
              GOTO 100
          ENDIF
C         we have not yet found the start yet so continue round
      ENDIF
C
C     we have a new point lets test it against our start point for a complete po
      IF (FIRST) THEN
C         save start point for polygon
          SX = NX(OLDP)
          SY = NY(OLDP)
          FIRST= .FALSE.
      ELSEIF(SX.EQ.X1.AND.SY.EQ.Y1) THEN
C         make sure first point is not there
          FINISH = .TRUE.
          FSTAT = 1
C         complete polygon found
          GOTO 300
      ENDIF
C     do this test only after condition for finish
      IF(USEDP(OLDP)) THEN
C         This point is alleady used no more
          GOTO 100
      ENDIF
C     Store the current point in the new polygon
      NEWP = NEWP + 1
      NPOLX(NEWP,POLYN) = NX(OLDP)
      NPOLY(NEWP,POLYN) = NY(OLDP)
      USEDP(OLDP) = .TRUE.
C     this is the first point save it for comparison later
C     we are now looking for an entry into the polygon with the correct assoc
      DO 110 I=1,NUMIP
          IF(ASSOC(I).EQ.OLDP.AND.TYPE(I).EQ.ENTRY.AND.
     +      .NOT.USEDI(I) ) THEN
C          we now have an entry point into the window
C             store this point
              NEWP = NEWP+1
              NPOLX(NEWP,POLYN) = INTPS(1,I)
              NPOLY(NEWP,POLYN) = INTPS(2,I)
C             the ip has been used test form completeion only
              USEDI(I)  = .TRUE.
C             we have now entered the polygon. We must now
C             eat into the polygon structure. This may split or
C             bite the polygon
              ONSIDE = .TRUE.
C             save the current polygon in case we have to split
              RESTART = ASSOC(I)
          ENDIF
 110  CONTINUE
C
      IF(ONSIDE) THEN
C         complete polygon by looking at sides in a CCW order from our current p
C         get current point
          X2 = NPOLX(NEWP,POLYN)
          Y2 = NPOLY(NEWP,POLYN)
          CALL POLY_EAT(XMIN,YMIN,XMAX,YMAX,X2,Y2,POLY,NOP,RES)
          IF(NOP.EQ.0) THEN
              RETURN
          ENDIF
          DO 400 J=1,NOP
              NEWP=NEWP+1
              NPOLX(NEWP,POLYN) = POLY(1,J)
              NPOLY(NEWP,POLYN) = POLY(2,J)
400       CONTINUE
          OLDP = RES
          ONSIDE= .FALSE.
 
C         calculate the eaten portion of the polygon and add it to the list
      ENDIF
200   CONTINUE
      GOTO 100
C
300   CONTINUE
C     save current polygon status
      NPOLYN(POLYN) = NEWP
C     do we need to go again ?
      IF(RESTART.GT.0) THEN
          POLYN = POLYN+1
          IF(POLYN.LE.4) THEN
              NEWP = 0
              OLDP = RESTART
              RESTART = 0
              FIRST = .TRUE.
              GOTO 100
          ELSE
              POLYN = 4
          ENDIF
      ENDIF
500   CONTINUE
700   CONTINUE
C     finish up please test status
C
 
      END
C
      SUBROUTINE POLY_DRAW(NX,NY,NOE)
C     ===============================
C
      include   'include/polyclip.inc'
      include   'include/viewport.inc'
      include   'include/wtov.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*2 NOE,NX(NOE),NY(NOE),RECT(2,2)
C
C     set clipping active for drawing into the screen
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,ST)
      RECT(1,1)=VXMIN+1
      RECT(2,1)=VYMIN+1
      RECT(1,2)=VXMAX-VXMIN-1
      RECT(2,2)=VYMAX-VYMIN-1
      CALL GPR_$SET_CLIP_WINDOW(RECT,ST)
C     draw the polygon in please
      CALL GPR_$START_PGON(NX(1),NY(1),ST)
      CALL GPR_$PGON_POLYLINE(NX,NY,NOE,ST)
      CALL GPR_$CLOSE_FILL_PGON(ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
      END
C
C
      SUBROUTINE POLY_EAT(XMIN,YMIN,XMAX,YMAX,XP,YP,POLY,NOP,RES)
C     ===========================================================
C1    VARTYPE              I4  I4   I4   I4   I4 I4  I4  I4
C1    IOSTAT               I   I    I    I    I  I   O   O
C
C2    This function will determine a series of points to add to the curent polyg
C2    Starting at a sent point the routine will work its way anticlockwise
C2    round the window
C
      include   'include/polyclip.inc'
C
      INTEGER*4 POLY_SIDE
      INTEGER*4 XP,YP,XMIN,XMAX,YMIN,YMAX,SIDEN,POLY(2,10),NOP
      INTEGER*4 STSIDE,X1,Y1,SN,I,J,RES
      LOGICAL OUT
C
      EXTERNAL POLY_SIDE
C
      STSIDE = POLY_SIDE(XP,YP,XMIN,YMIN,XMAX,YMAX)
C
      OUT = .FALSE.
      NOP = 0
C
      DO 100 I=1,4
C
          OUT = .FALSE.
          DO 110 J=1,NUMIP
C             search the list of intersection points for an exiter closest
C             to the current point
C             test accosiated point
              IF(TYPE(J).EQ.EXIT) THEN
                  X1 = INTPS(1,J)
                  Y1 = INTPS(2,J)
              ELSE IF(TYPE(J).EQ.EXIT) THEN
                  X1 = INTPS(1,J)
                  Y1 = INTPS(2,J)
              ELSE
C                 no point suitable keep going
                  GOTO 120
              ENDIF
C             test for the same point
              SN = POLY_SIDE(X1,Y1,XMIN,YMIN,XMAX,YMAX)
              IF(SN.EQ.STSIDE.AND..NOT.USEDI(J)) THEN
                  OUT = .TRUE.
                  RES = ASSOC(J)
C                 tis other point we must use it
                  NOP = NOP+1
                  POLY(1,NOP) = X1
                  POLY(2,NOP) = Y1
C                 mark as used now
                  USEDI(J) = .TRUE.
C                 we got it out of polygon
                  RETURN
              ENDIF
120       CONTINUE
110       CONTINUE
C         not this side which corner do we use
          NOP = NOP + 1
          POLY(1,NOP) = CORNER(1,STSIDE)
          POLY(2,NOP) = CORNER(2,STSIDE)
C
          STSIDE = STSIDE + 1
          IF(STSIDE.EQ.5) STSIDE = 1
C
100   CONTINUE
      END
 
 
      SUBROUTINE POLY_SORT(NX,NY,NOE,DIR)
C     ===================================
C1    VARTYPE
C1    IOSTAT
C
C2    This routine will calculate the direction of a polygon
C2    it assumes that you have supplied a CLOCKWISE direction
C2    of poly elements. If you have not then DIR is returned as false
C2
      INTEGER*2 NOE,X1,Y1,X2,Y2,SP,count
      INTEGER*2 NX(NOE),NY(NOE)
      LOGICAL DIR
      LOGICAL FIRST
      INTEGER*4 I,OX,OY,TAREA,J
C
C     calculte the area of the polygon
C
      FIRST = .TRUE.
C
      TAREA = 0
      DO 10 I=1,NOE
C
          IF(FIRST) THEN
              FIRST = .FALSE.
          ELSE
C             area of first trapezoid
              TAREA = TAREA + (NX(I)-OX)*(OY+NY(I))/2
          ENDIF
          OX = NX(I)
          OY = NY(I)
C
 10   CONTINUE
      TAREA = TAREA + (NX(1)-OX)*(OY+NY(1))/2
C     get direction
      IF(TAREA .GT. 0) THEN
C         swap round if necessary
          CALL POLY_SWAP(NX,NY,NOE)
      ENDIF
C     take out any duplicate points befor we go on
      SP = 0
100   CONTINUE
      SP = SP+1
110   CONTINUE
      IF(SP.EQ.NOE) RETURN
      X1 = NX(SP)
      Y1 = NY(SP)
C
      X2 = NX(SP+1)
      Y2 = NY(SP+1)
C
      IF(X1.EQ.X2.AND.Y1.EQ.Y2) THEN
          DO 20 I=1,NOE-1
              NX(I) = NX(I+1)
              NY(I) = NY(I+1)
 20       CONTINUE
C
          NOE = NOE-1
          GOTO 110
      ENDIF
      GOTO 100
      END
C
      SUBROUTINE POLY_SWAP(NX,NY,NOE)
C     ===============================
C1    VARTYPE
C1    IOSTAT
C
C2    This routine simply swaps the array passed which
C2    will reverse the direction of a polygon
C
      INTEGER*2 NOE,I,J,TX,TY
      INTEGER*2 NX(200),NY(200)
      INTEGER*2 BNX(200),BNY(200)
C
      DO 10 I=2,NOE
C
          BNX(I) = NX(I)
          BNY(I) = NY(I)
C
10    CONTINUE
C
      DO 20 I=2,NOE
          NX(I) = BNX(NOE-I+2)
          NY(I) = BNY(NOE-I+2)
20    CONTINUE
      END
 
