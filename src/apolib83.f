C
C     @(#)  412.1 date 6/11/92 apolib83.f 
C
C
C     Filename    : apolib83.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:29:59
C     Last change : 92/06/11 14:23:26
C
C     Copyright : Practical Technology International Limited  
C     File :- apolib83.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION MENKEY(MENUN,CURSX,CURSY)
C     FUNCTION MINMAX (VAL,CONST)
C     SUBROUTINE APCURS(HITKEY,SORCE,XC,YC)
C     SUBROUTINE APRREC(SX1,SY1,SX2,SY2,HITKEY,SORCE,OK)
C     SUBROUTINE ARCCT(X1,X2,X4,X5,X6)
C     SUBROUTINE ARCCT1(X1,X2,X4,X5,X6)
C     SUBROUTINE BITMAP(MDESC,BMWIND,DISPOS)
C     SUBROUTINE CLEARW(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE DRAWL1(X1,Y1,X2,Y2)
C     SUBROUTINE DRAWLS(X1,Y1,X2,Y2)
C     SUBROUTINE DRAWLT(X1,Y1,X2,Y2)
C     SUBROUTINE DRSREC(X1,Y1,X2,Y2)
C     SUBROUTINE DRWARC(X,Y,WRAD,SANG,EANG)
C     SUBROUTINE DRWARC1(X,Y,WRAD,SANG,EANG)
C     SUBROUTINE GCIRCL(X1,X2,X4)
C     SUBROUTINE GTSPLN(X,Y,LINSTR,INV)
C     SUBROUTINE GTSPMG(X,Y,LINSTR,INV)
C     SUBROUTINE GTSRLN(CURX,CURY,LINSTR,LINTRM)
C     SUBROUTINE INCURP(X,Y)
C     SUBROUTINE MOVES(XC,YC)
C     SUBROUTINE NBP()
C     SUBROUTINE PEN(COLOR)
C     SUBROUTINE POINTS(X1,Y1)
C     SUBROUTINE RASTER(OPT)
C     SUBROUTINE RELDIS(TT)
C     SUBROUTINE S1GROW()
C     SUBROUTINE S2GROW()
C     SUBROUTINE SCURS(C,XC,YC)
C     SUBROUTINE STCURP(X,Y)
C     SUBROUTINE UPDAAA(APX,APY)
C     SUBROUTINE UPDACP(APX,APY)
C     SUBROUTINE UPDAPC(APX,APY)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE APCURS(HITKEY,SORCE,XC,YC)
C     =====================================
C1    VARYPE             C      I2   I2 I2
C1    IOSTAT             O      O    O  O
C
C2    DAXCADS main cursor control routine. All locator digitiser
C2    and keys hits are controled.
C2
C2    Arguments:-
C2
C2    HITKEY          ->          The key hit
C2    SORCE           ->          The type of event. Maps in GPR event
C2                                types. 8 means source is a button
C2                                held down for more than 0.5 seconds
C2
C2    XC YC           ->          The coordinates of the cursor.
C2
C2
C2    Error Returns:
C2
C2    NONE.
C2
C2
C
      include 'include/gpr.ins.inc'
C
      include  'include/apollo.inc'
      include  'include/dig2wo.inc'
      include  'include/datatype.inc'
      include  'include/macro.inc'
      include  'include/menun.inc'
      include  'include/curwin.inc'
      include  'include/menpop.inc'
      include  'include/daxcolor.inc'
      include  'include/daxcad_x.inc'
      include  'include/style.inc'
      include  'include/ndata.inc'
      include  'include/cursor.inc'
      include  'include/wtov.inc'
C
      INTEGER*4 ST
      INTEGER*4 X,Y
      INTEGER*4 XC,YC
      INTEGER*4 ABX,ABY
      INTEGER*4 TCELL
      INTEGER*4 TMEN
      INTEGER*4 MENKEY
      INTEGER*4 CURSX,CURSY
      INTEGER*4 MEN1,CELLN1
      INTEGER*4 SYSCUR
      INTEGER*4 POS(2)
      INTEGER*2 SCRPOS(2)
      INTEGER*2 EVTYPE
      INTEGER*2 I
      INTEGER*4 KEY,C
      INTEGER*2 SORCE,DGB
      INTEGER*2 ACQCNT
      LOGICAL UNOBSCURED
      LOGICAL LWAIT
      LOGICAL OK
      LOGICAL MENOK
      LOGICAL DIGDV1
      CHARACTER EVDATA*1
      CHARACTER TCCMD*1
      CHARACTER POINTPOS*80
      CHARACTER HITKEY*1
      REAL TIMEO
      REAL PX,PY
      REAL SX,SY
C
      SAVE POINTPOS
      EXTERNAL MENHIT
      EXTERNAL MOUSE
C
      CALL GTICUR(POINTPOS,1,.FALSE.)
      CALL GPR_$SET_AUTO_UPDATE(.FALSE.,ST)
C
      SKEY = 0
C
C
C     Draw the graphics cursor in the default start position
C
C     set cursor color properly
      CALL SETAPN(COLOUR)
      CALL ROPXOR()
      IF ( XVERSION ) THEN
C         Turn on X wait cursor
          CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
	  SYSCUR = 0
      ELSE
	  SYSCUR = 3
      ENDIF
C
      SCRPOS(1) = APCURX(1)
      SCRPOS(2) = APCURY(3)

      CURSLN(1,1) = VXMIN
      CURSLN(2,1) = VYMIN
      CURSLN(3,1) = VXMAX - VXMIN
      CURSLN(4,1) = VYMAX - VYMIN
      CURSX = SCRPOS(1)
      CURSY = SCRPOS(2)
      POS(1) = SCRPOS(1)
      POS(2) = SCRPOS(2)
      CALL CURSOR_MODIFY(POS,ST)
      CALL WRKHIT(REAL(CURSX),REAL(CURSY),OK)
      IF ( OK ) THEN
          CALL NEW_CURSOR(DAXCURS(1),ST)
      ELSE
          CALL NEW_CURSOR(SYSCUR,ST)
      ENDIF

      TCELL=CELLN
      TMEN=MEN
C
C     release the display during this session.
      CALL GPR_$FORCE_RELEASE(ACQCNT,ST)
C     Main GR loop waiting for cursor input
C
 222  CONTINUE
C
C     set cursor color properly
      CALL SETAPN(COLOUR)
C     set exclusive or raster op.
C
      DIGDV1 = .FALSE.
      IF ( DIGIT ) THEN
          LWAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
          IF ( EVTYPE.EQ.GPR_$NO_EVENT ) THEN
C             digitiser
              CALL MOUSE(EVTYPE,DGB,C,EVDATA,X,Y,ABX,ABY)
              IF ( EVTYPE.EQ.GPR_$NO_EVENT ) THEN
                  GOTO 222
              ENDIF
              DIGDV1 = .TRUE.
              SCRPOS(1)=X
              SCRPOS(2)=LIMY-Y
          ENDIF
      ELSE
         DGB=-1
C        look for a button event somewhere else
         LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
      END IF
C
C     If the arrow keys have been used then come back to
C     this routine to move the cursor as normal
 
 225  CONTINUE

      IF (EVTYPE.EQ.GPR_$LOCATOR) THEN
C         update the cursor position.
C         If the macro is operational then do a higligter for
C         menu 3
          UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
          CURSX=SCRPOS(1)
          CURSY=SCRPOS(2)
C		  WRITE(*,*) 'APCURS: ',CURSX,CURSY
          CALL WRKHIT(REAL(CURSX),REAL(CURSY),OK)
          MEN1=0
          IF(.NOT.OK) THEN
C             clear on leaving
              CALL GTICUR(POINTPOS,1,.TRUE.)
              CALL MENHIT(CURSX,CURSY,MEN1,CELLN1,CTEXT,TCCMD,MENOK)
              IF ( MEN1.GT.0 ) THEN
                  CALL NEW_CURSOR(SYSCUR,ST)
              ELSE
                  CALL NEW_CURSOR(0,ST)
              ENDIF
          ELSE
C             this is a plain boring cursor.
              CALL NEW_CURSOR(DAXCURS(1),ST)
          ENDIF
          CURSX=SCRPOS(1)
          CURSY=SCRPOS(2)
C         update position indicator
          CALL SC2WO(REAL(CURSX),REAL(CURSY),PX,PY)
          IF ( SETGRD ) THEN
              CALL SNAPXY(PX,PY,SX,SY)
          ELSE
              SX = PX
              SY = PY
          ENDIF
C
          IF ( OK .AND.DAXTRACK) THEN 
C
              CALL GTICUR(POINTPOS,1,.TRUE.)
              WRITE(POINTPOS,FMT='(A,G20.10,A,G20.10)',ERR=2000)
     +                    'X:',SX,',Y:',SY
              CALL CRUNCH(POINTPOS)
              CALL GTICUR(POINTPOS,1,.FALSE.)
C
          ENDIF
C
          CALL SETAPN(COLOUR)
          IF ( .NOT.XVERSION ) THEN
C             This must not happen under X version
              CALL GPR_$SET_CURSOR_POSITION(SCRPOS,ST)
          ENDIF
          CALL CURSOR_UPDATE(SCRPOS,ST)
          CALL UPDACP(SCRPOS(1),SCRPOS(2))
2000      CONTINUE
          CALL GPR_$RELEASE_DISPLAY(ST)
          GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$BUTTONS) THEN
C        timer for bounce.
         IF(.NOT.DIGDV1.AND.(EVDATA.EQ.'a'.OR.EVDATA.EQ.'c'
     +                       .OR.EVDATA.EQ.'b'))THEN
C
C             wait for a time out
              TIMEO = 1.0
              CALL EVENT_TIMEOUT(TIMEO,OK)
C             If timed out then POPUP needed
              IF(OK) THEN
                  EVTYPE = 8
              ENDIF
         ENDIF
C
         I=ICHAR(EVDATA)
C        In this routine we ignore UPSTROKES
         IF ( I.GE.65 .AND. I.LE.68 ) GOTO 222
C        translate the button hits to acceptable values.
         I=I+57
C        recalculate character because it may have been altered.
         KEY=ICHAR(EVDATA)
         SKEY=KEY
         EVDATA=CHAR(I)
      ELSE IF (EVTYPE.EQ.GPR_$LEFT_WINDOW) THEN
C        
C        Cursor has left please turn it off
C
C         CALL LOAD_COLORMAP(0,ST)
         GOTO 222
C
      ELSE IF (EVTYPE.EQ.GPR_$ENTERED_WINDOW) THEN
C        
C        Cursor has entered please turn it on
C
C         CALL LOAD_COLORMAP(1,ST)
         GOTO 222
C
      ELSE IF (EVTYPE.EQ.GPR_$KEYSTROKE) THEN

         KEY=ICHAR(EVDATA)
         SKEY=KEY
         IF(SKEY.GT.255.OR.SKEY.LT.0) SKEY=0
C
C        arrow keys for mouse,or bitpad failure.
         IF ( KEY .EQ. 136 .OR. KEY .EQ. 138
     +   .OR. KEY .EQ. 140 .OR. KEY .EQ. 142 ) THEN
C
            IF ( KEY .EQ. 136 ) THEN
               SCRPOS(1)=APCURX(1)
               SCRPOS(2)=APCURY(3)-CMOVE
            ELSE IF ( KEY .EQ. 138 ) THEN
               SCRPOS(1)=APCURX(1)-CMOVE
               SCRPOS(2)=APCURY(3)
            ELSE IF ( KEY .EQ. 140 ) THEN
               SCRPOS(1)=APCURX(1)+CMOVE
               SCRPOS(2)=APCURY(3)
            ELSE IF ( KEY .EQ. 142 ) THEN
               SCRPOS(1)=APCURX(1)
               SCRPOS(2)=APCURY(3)+CMOVE
            END IF
C           update the cursor position
C           kid on that we had a locator event
            EVTYPE=GPR_$LOCATOR
            GOTO 225
         ELSE IF (KEY.GT.48.AND.KEY.LT.58) THEN
            CMOVE=3*(KEY-48)-2
C           loop back and test cursor again
            GOTO 222
         ELSE IF (KEY.EQ.48) THEN
            IF ( NPOS.EQ.4  ) THEN
               NPOS=12
            ELSE IF ( NPOS.EQ.12 ) THEN
               NPOS=4
            END IF
C           loop back and test cursor again
            GOTO 222
         END IF
      ELSE
         GOTO 222
      END IF
 
C     return the type of event.
      SORCE=EVTYPE
      HITKEY=EVDATA
      IF ( DIGIT.AND.(DGB.EQ.DIGBUT)) THEN
         XC=ABX
         YC=ABY
      ELSE
         XC=APCURX(1)
         YC=APCURY(3)
      END IF

C     undraw the cursor assuming not in menu 3
      CALL GTICUR(POINTPOS,1,.TRUE.)
      CALL NEW_CURSOR(0,ST)
      CALL CURSOR_ON(.FALSE.)
C     Now set raster op back to default
      CALL ROPREP()
      CALL SETAPN(COLOUR)
C
      IF ( XVERSION ) THEN
C         Turn on X wait cursor
          CALL GPR_$SET_WAIT_CURSOR(ST)
          CALL GPR_$SET_CURSOR_ACTIVE(.TRUE.,ST)
      ENDIF
      INPUTSTATE = 0
      CALL GPR_$SET_AUTO_UPDATE(.TRUE.,ST)
C
      END
C
C======================================================================
C
      SUBROUTINE APRREC(SX1,SY1,SX2,SY2,HITKEY,SORCE,OK)
C     ==================================================
C1    VARYPE             R   R   R   R    C     I4   L
C1    IOSTAT             I   I   O   O    O     O    O
C
C1    Rubberband a rectangular window starting at the anchor
C1    point SX1,SY1 on the screen and return the diagonally
C1    opposite corner in SX2,SY2, the hitkey used in HITKEY,
C1    and the source of the hitkey in SOURCE. The rectangle
C1    is erased before return from this routine.
C2  
C2    Arguments:-
C2  
C2    SX1     ->      Anchor X point
C2    SY1     ->      Anchor Y point
C2    SX2     ->      Opposite X corner point 
C2    SY2     ->      Opposite Y corner point 
C2    HITKEY  ->      The hitkey that was returned
C2    SORCE   ->      The type of hit
C2  
C2  
C2    Error Returns:
C2  
C2    OK Standard LOGICAL True is SUCCES
C2  
C2  
C1
      include 'include/gpr.ins.inc'
C
      include  'include/apollo.inc'
      include  'include/dig2wo.inc'
      include  'include/datatype.inc'
      include  'include/style.inc'
C
      REAL SX1,SY1,SX2,SY2
      INTEGER*2 ANCX,ANCY,APX,APY,ICHAR,TP
      INTEGER*2 SCRPOS(2),EVTYPE,I,DGB
      INTEGER*4 ST,ANCX1,ANCY1,APX1,APY1,X,Y,
     +           SORCE,MENUN,CELLN,OPT,KEY,C
      LOGICAL UNOBSCURED,WAIT,FIRST,OK
      CHARACTER EVDATA*1,HITKEY*1
C
C
      EXTERNAL DRSREC,WRKHIT
C
      ANCX=INT(SX1)
      ANCY=INT(SY1)
C
      CALL GPR_$SET_AUTO_UPDATE(.FALSE.,ST)
      OK = .FALSE.
      FIRST=.TRUE.
C
C     Time to acquire the display now
C
C     set exclusive or raster op
      CALL SETAPN(COLOUR)
      OPT=6
      CALL RASTER(OPT)
C
      WAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
C
 222  CONTINUE
C
      IF ( DIGIT ) THEN
         WAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
      ELSE
         WAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
      END IF
C
      IF ( EVTYPE.EQ.GPR_$NO_EVENT.AND.DIGIT ) THEN
         CALL MOUSE(EVTYPE,DGB,C,EVDATA,X,Y,AX,AY)
C
         SCRPOS(1)=X
         SCRPOS(2)=LIMY-Y
      END IF
      IF ( EVTYPE.EQ.GPR_$NO_EVENT ) THEN
C
         GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$LOCATOR) THEN
          IF (FIRST) THEN
C         draw the box from anchor to this point
              CALL DRSREC(ANCX,ANCY,SCRPOS(1),SCRPOS(2))
C         Save the box endpoint
              APCURX(1)=SCRPOS(1)
              APCURY(3)=SCRPOS(2)
              FIRST=.FALSE.
              GOTO 222
          ELSE
              IF ((ABS(APCURX(1)-SCRPOS(1))+ABS(APCURY(3)-SCRPOS(2)))
     +            .GT.2) THEN
C             erase the old box
                  CALL DRSREC(ANCX,ANCY,APCURX(1),APCURY(3))
C             Save the new box endpoint
                  APCURX(1)=SCRPOS(1)
                  APCURY(3)=SCRPOS(2)
C             draw the new box
                  CALL DRSREC(ANCX,ANCY,APCURX(1),APCURY(3))
              END IF
          GOTO 222
          END IF
      ELSE IF (EVTYPE.EQ.GPR_$BUTTONS) THEN
C         discard upstrokes of puck buttons
          I=ICHAR(EVDATA)
          IF ((I.GE.65).AND.(I.LE.68)) GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$KEYSTROKE) THEN
C
         KEY=ICHAR(EVDATA)
         IF ( KEY .EQ. 136 .OR. KEY .EQ. 138
     +   .OR. KEY .EQ. 140 .OR. KEY .EQ. 142 ) THEN
C
            IF ( KEY .EQ. 136 ) THEN
               SCRPOS(1)=APCURX(1)
               SCRPOS(2)=APCURY(3)-CMOVE
            ELSE IF ( KEY .EQ. 138 ) THEN
               SCRPOS(1)=APCURX(1)-CMOVE
               SCRPOS(2)=APCURY(3)
            ELSE IF ( KEY .EQ. 140 ) THEN
               SCRPOS(1)=APCURX(1)+CMOVE
               SCRPOS(2)=APCURY(3)
            ELSE IF ( KEY .EQ. 142 ) THEN
               SCRPOS(1)=APCURX(1)
               SCRPOS(2)=APCURY(3)+CMOVE
            END IF
C           erase the old box
            CALL DRSREC(ANCX,ANCY,APCURX(1),APCURY(3))
C           Save the new box endpoint
            APCURX(1)=SCRPOS(1)
            APCURY(3)=SCRPOS(2)
C           draw the new box
            CALL DRSREC(ANCX,ANCY,APCURX(1),APCURY(3))
C           loop back and test cursor again
            GOTO 222
C
         ELSE IF (KEY.GT.48.AND.KEY.LT.58) THEN
            CMOVE=2*(KEY-48)-1
C           loop back and test cursor again
            GOTO 222
         END IF
C
C
      ELSE
C         Event is not yet defined or is of no use.
          GOTO 222
      END IF
C
C     Check that window is not too small
      IF ( ABS(ANCX-APCURX(1)).LT.2 .OR.
     +     ABS(ANCY-APCURY(3)).LT.2 ) GOTO 222
C
C     now undraw the box
      CALL DRSREC(ANCX,ANCY,APCURX(1),APCURY(3))
C     update the cursor position
      CALL UPDACP(APCURX(1),APCURY(3))
C     move the current position to end of line
      CALL MOVES(REAL(APCURX(1)),REAL(APCURY(3)))
C
      HITKEY=EVDATA
      SORCE=EVTYPE
      SOURCE=EVTYPE
      APX=APCURX(1)
      APY=APCURY(3)
C
C     Now set raster op back to default
      OPT=3
      CALL RASTER(OPT)
C
C     Check that ANCX,ANCY is the minimum coordiantes
C     of the rectangle
C
      IF ( APX .LT. ANCX ) THEN
         TP=APX
         APX=ANCX
         ANCX=TP
      END IF
C
      IF ( APY .LT. ANCY ) THEN
         TP=APY
         APY=ANCY
         ANCY=TP
      END IF
C
      SX1=REAL(ANCX)
      SY1=REAL(ANCY)
      SX2 =REAL(APX)
      SY2 =REAL(APY)
C
C     translate 'blue' cursor button into quit char
      IF ( HITKEY .EQ. 'c' .AND. SOURCE .EQ.GPR_$BUTTONS ) THEN
C        if quit,then set flag and return
         HITKEY='Q'
         OK=.FALSE.
         GOTO 99
      END IF
C
C     If second hit is not in valid area
C     of screen,then abort this operation
C     test for hit in work area of screen
      CALL WNHIT(REAL(APCURX(1)),REAL(APCURY(3)),OK)
C
 99   CONTINUE
      CALL GPR_$SET_AUTO_UPDATE(.TRUE.,ST)
C
      END
 
 
C
C
C======================================================================
C
C
 
 
      SUBROUTINE ARCCT(X1,X2,X4,X5,X6)
C1    =================================
C1                       I2
C1                        I
C2     This routine uses the passed MIP pointer
C2     to read the part data file at that position.
C2     the data held there will be the definition
C2     for an arc , which is then converted into the
C2     appropriate plotter commands. Data read in part
C2     data file held in Masti common block.
C2     Checks are done for optimidation , i.e is last plotter
C2     same as one of new ones , if so set new start to old point.
C2     also check done for radius vs chord angle.
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/lfont.inc'
      include 'include/gerber.inc'
C
C     Set the coordinate info local
C     Arc Centre point
      INTEGER THK,END
      INTEGER*4 TOTAL
      LOGICAL OK
      REAL X1,X2,X4,X5,X6,XT4,TOT,PI
      EXTERNAL PI
C---  Is new start point same as last end point
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.GT.0 ) THEN
        TOT=PAPTOW*LTHKR(1,MOD(THK,1024))/2.0
        END=MOD(THK,8192)/1024
        THK=MOD(THK,64)
      END IF
C     just testing
      IF ( THK.NE.0.AND.TOT.LT.0.0 ) THEN
C        fill the pad.
         CALL ARCCHK(X1,X2,X4,X5,X6,TOTAL,DISPV)
C        if not ok,then not visible
         IF ( .NOT. DISPV ) RETURN
C
         IF ( END.EQ.1 ) THEN
C           square end.
            XT4=X1-2.0*X4
            X1 =X1+2.0*X4
            X5 =X2
            CALL DRAWLT(X1,X2,XT4,X5)
         ELSE IF ( END.EQ.2.AND.MOD(INT(TOLSET(THK,1)),255).EQ.0 ) THEN
C           finger pad.
            TOT=PAPTOW*ABS(TOLSET(THK,2))
            IF ( TOLSET(THK,2).LT.0 ) THEN
C             vertical finger pad.
              XT4=X2
              X2=X2+TOT
              X5 =XT4-TOT
              XT4=X1
            ELSE
C             horizontal finger pad.
              XT4=X1-TOT
              X1 =X1+TOT
              X5 =X2
            END IF
            CALL DRAWLT(X1,X2,XT4,X5)
         ELSE
            CALL GCIRCL(X1,X2,X4)
            DISPV=.TRUE.
         END IF
      ELSE
         CALL DRWARC(X1,X2,X4,X5,X6)
      END IF
      END
*
*
      SUBROUTINE ARCCT1(X1,X2,X4,X5,X6)
C1    =================================
C1                       I2
C1                        I
C2     This routine uses the passed MIP pointer
C2     to read the part data file at that position.
C2     the data held there will be the definition
C2     for an arc , which is then converted into the
C2     appropriate plotter commands. Data read in part
C2     data file held in Masti common block.
C2     Checks are done for optimidation , i.e is last plotter
C2     same as one of new ones , if so set new start to old point.
C2     also check done for radius vs chord angle.
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include  'include/lfont.inc'
C
C     Set the coordinate info local
C     Arc Centre point
C
      INTEGER THK,END,I
      INTEGER*4 TOTAL
      LOGICAL OK
      REAL X1,X2,X4,X5,X6,XT1,XT2,XT4,XT5,XT6,
     +    ANG,TOT,INC,STEP,SRAD,EXT,PI,DEG
      EXTERNAL PI,DEG
C
C---  Is new start point same as last end point
C
      XT1=X1
      XT2=X2
      XT4=X4
      XT5=X5
      XT6=X6
      ANG=0.0
      EXT=0.0
C
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.GT.0 ) THEN
         TOT=PAPTOW*LTHKR(1,MOD(THK,109))/2.0
         END=MOD(THK,8192)/1024
         IF ( END.GT.1 ) THEN
            IF (.NOT.(ABS(X6-X5-PI(2.0)).LT.1E-5)) THEN
               EXT=TOT
               ANG=2*ASIN(EXT/(2*X4))
            END IF
            XT5=X5-ANG
            XT6=X6+ANG
         END IF
      END IF
      OK=.FALSE.
      CALL DRWARC(XT1,XT2,XT4,XT5,XT6)
      IF ( .NOT. DISPV ) RETURN
C     was any part of the arc drawn on the screen.
      IF ( DISPV ) OK=.TRUE.
      IF ( THK.GT.0.AND.TOT.GT.0 ) THEN
C        just testing
         XT4=X4+TOT
         CALL ARCCHK(XT1,XT2,XT4,XT5,XT6,TOTAL,DISPV)
         XT4=X4
         IF ( .NOT. DISPV ) RETURN
         INC=1.0
         CALL STOWVX(INC,STEP)
         INC=STEP
 5       CONTINUE
         IF ( .NOT.(INC.LE.TOT) ) GOTO 99
         IF ( INC.GT.X4 )         GOTO 99
         IF ( EXT.GT.2*X4 )       GOTO 99
         IF ( END.EQ.2 )  THEN
            EXT=SQRT(TOT*TOT-INC*INC)
            ANG=2*ASIN(EXT/(2*X4))
         END IF
         XT5=X5-ANG
         XT6=X6+ANG
         DO 12 I=1,2
            IF ( I.EQ.1 ) XT4=X4+INC
            IF ( I.EQ.2 ) XT4=X4-INC
            CALL DRWARC(XT1,XT2,XT4,XT5,XT6)
            IF ( DISPV ) OK=.TRUE.
 12      CONTINUE
         INC=INC+STEP
         GOTO 5
      END IF
 99   CONTINUE
      DISPV=OK
 
      END
 
C
C-----------------------------------------------------------------
C
      SUBROUTINE BITMAP(MDESC,BMWIND,DISPOS)
C     ======================================
C1    VARYPE             I4   I2(2X2)  I2(2)
C1    IOSTAT             I      I       I
C
C2    This routine will highlight a cell From a special bitmap defined
C2    
C2  
C2    Arguments:-
C2  
C2    MDESC           ->          Bitmap descripter for hidden filled
C2    BMWIND          ->          Size of the cell
C2    DISPOS          ->          Origin of cell
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/gpr.ins.inc'
      include 'include/interface.inc'
C
      INTEGER*2 GTWID,GTCID,I2
      INTEGER*2 RECT(4)
      INTEGER*2 BMWIND(4),DISPOS(2),rops(8),PLMASK
      INTEGER*4 ST,ADESC,MDESC,GTWID4,GTCID4,OPT,COLORV(0:7),I4
      LOGICAL UNOBSCURED,ACT
C
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
C     do the BLT
      CALL GPR_$PIXEL_BLT(MDESC,BMWIND,DISPOS,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE CLEARW(XMIN,YMIN,XMAX,YMAX)
C     ======================================
C1                        R    R    R    R
C1                        I    I    I    I
C2      Subroutine CLEAR erase the whole screen and graphics
C2      memory
C
      include   'include/gpr.ins.inc'
      include   'include/wtov.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*4 ST,VAL
      REAL  XMIN,YMIN,XMAX,YMAX
      INTEGER*2 RECT(1:2,1:2)
      LOGICAL OBSCURED
C
      OBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C     set raster to black mode
      VAL = 3
      CALL RASTER(VAL)
      RECT(1,1)=INT(XMIN)
      RECT(2,1)=INT(YMIN)
      RECT(1,2)=INT(XMAX-XMIN)
      RECT(2,2)=INT(YMAX-YMIN)
      CALL GPR_$RECTANGLE(RECT,ST)
C     set raster to replace mode
      VAL=3
      CALL RASTER(VAL)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
 
      SUBROUTINE DRAWL1(X1,Y1,X2,Y2)
C     ==============================
C
      include  'include/masti.inc'
      include  'include/lfont.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
 
      REAL X1,Y1,X2,Y2,PI,CANG,
     +     XT1,YT1,XT2,YT2,
     A     WX1,WY1,WX2,WY2,
     B     VX1,VY1,VX2,VY2,
     C     L1,L2,L3,INC,STEP,TOT,EXT,
     D     N1(2),N2(2),N3(2)
      INTEGER I,END,THK
      LOGICAL OK
      EXTERNAL PI,CANG
C
C     line still in world coordinates have
C     to convert to screen coordinates
      XT1=X1
      YT1=Y1
      XT2=X2
      YT2=Y2
      EXT=0.0
C     check for line thickness
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.GT.0 ) THEN
         TOT=PAPTOW*LTHKR(1,MOD(THK,1024))/2.0
         END=MOD(THK,8192)/1024
         CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
         IF ( END.GT.1 ) THEN
            EXT=TOT
            CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
            CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
         END IF
      END IF
C
      CALL CLIP(XT1,YT1,XT2,YT2,DISPV)
      IF ( DISPV ) THEN
         CALL WO2SC(XT1,YT1,VX1,VY1)
         CALL WO2SC(XT2,YT2,VX2,VY2)
C        now we have screen coords VX1,VY1,VX2,VY2
C        must draw line with correct fonting
         CALL DRAWLS(VX1,VY1,VX2,VY2)
C        Line thickness draw
         IF ( THK.GT.0 ) THEN
            INC=MAX(ABS(COS(CANG(X1,Y1,X2,Y2))),
     +              ABS(SIN(CANG(X1,Y1,X2,Y2))) )
            INC=INC*INC
            CALL STOWVX(INC,STEP)
            INC=STEP
 5          CONTINUE
            IF ( INC.LT.TOT ) THEN
               CALL VV0L15(L1,L2,L3,INC,N1(1),N2(1),N3(1),
     +                                  N1(2),N2(2),N3(2))
               IF ( END.EQ.2 ) EXT=SQRT(TOT*TOT-INC*INC)
               IF ( EXT.GT.0 ) THEN
                  CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
                  CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
               ELSE
                  XT1=X1
                  XT2=X2
                  YT1=Y1
                  YT2=Y2
               END IF
               DO 10 I=1,2
                  CALL VC0PLP(N1(I),N2(I),N3(I),XT1,YT1,WX1,WY1)
                  CALL VC0PLP(N1(I),N2(I),N3(I),XT2,YT2,WX2,WY2)
                  CALL CLIP(WX1,WY1,WX2,WY2,OK)
                  IF ( OK ) THEN
                     CALL WO2SC(WX1,WY1,VX1,VY1)
                     CALL WO2SC(WX2,WY2,VX2,VY2)
C                    once converted draw line on screen
                     CALL DRAWLS(VX1,VY1,VX2,VY2)
                  END IF
 10            CONTINUE
               INC=INC+STEP
               GOTO 5
            END IF
         END IF
      END IF
 
      END
*
*
      SUBROUTINE DRAWLS(X1,Y1,X2,Y2)
C     ==============================
C1                       R, R, R, R
C1                       I, I, I, I
C2       Draws line from X1,Y1 to X2,Y2 in screen coordinates
C2      Common Block holds the current screen cursor position
C
C
      include   'include/gpr.ins.inc'
C
      include   'include/curpos.inc'
      include   'include/viewport.inc'
      include   'include/server.inc'
 
      REAL X1,Y1,X2,Y2
      INTEGER*2 IAPX,IAPY
      INTEGER*4 ST,BIT,XP1,YP1,XP2,YP2
      LOGICAL OBSCURED
      INTEGER*2 SEGLST(4,20)
      INTEGER*2 NUMSEG
C
C
C     Server mode no graphics
C
      IF ( SERVER ) THEN
          RETURN
      ENDIF
C     we only want to draw into a single view and no clipping apparant
      IF(DDCODE.EQ.0) THEN
C
C         Normal Draw operation into the screen line already clipped 
C         and the display aquired
C
          IAPX=NINT(X1)
          IAPY=NINT(Y1)
          CALL GPR_$MOVE(IAPX,IAPY,ST)
          IAPX=NINT(X2)
          IAPY=NINT(Y2)
          CALL GPR_$LINE(IAPX,IAPY,ST)
      ELSEIF (DDCODE.EQ.1) THEN
C
C         Draw a clipped line into the current bitmapo allrady clipped then
C         into the main display to clipp againt any DAXPORTS
C
          IAPX=NINT(X1)
          IAPY=NINT(Y1)
          XP1 = IAPX
          YP1 = IAPY
          IAPX = IAPX-VPOX
          IAPY = IAPY-VPOY
          CALL GPR_$MOVE(IAPX,IAPY,ST)
          IAPX=NINT(X2)
          IAPY=NINT(Y2)
          XP2 = IAPX
          YP2 = IAPY
          IAPX = IAPX-VPOX
          IAPY = IAPY-VPOY
          CALL GPR_$LINE(IAPX,IAPY,ST)
C         set segment manger into action
          CALL SEG_BUILD(VPSEGN,XP1,YP1,XP2,YP2,SEGLST,NUMSEG)
      ELSEIF(DDCODE.EQ.2) THEN
C
C         Draw into the main display against the current active viewport number
C
          XP1=NINT(X1)
          YP1=NINT(Y1)
          XP2=NINT(X2)
          YP2=NINT(Y2)
          CALL SEG_BUILD(CVPN,XP1,YP1,XP2,YP2,SEGLST,NUMSEG)
      ENDIF
C
      END

      SUBROUTINE DRAWLT(X1,Y1,X2,Y2)
C     ==============================
C
      include  'include/masti.inc'
      include  'include/lfont.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/wtov.inc'
      include  'include/viewport.inc'
      include  'include/daxcolor.inc'
      include  'include/server.inc'
 
      REAL X1,Y1,X2,Y2,PI,CANG,
     +     XT1,YT1,XT2,YT2,
     A     WX1,WY1,WX2,WY2,WX11,WY11,WX22,WY22,
     B     VX1,VY1,VX2,VY2,
     C     L1,L2,L3,INC,STEP,TOT,EXT,
     D     N1(2),N2(2),N3(2),
     E     TWXMIN,TWYMIN,TWXMAX,TWYMAX,MINMAX,CONST
      INTEGER I,END,THK,II
      INTEGER*2 IX(30),IY(30),N
      INTEGER*4 STATUS,FILVAL
      LOGICAL OK,SAME,DIR
      EXTERNAL PI,CANG,MINMAX,SAME
C
C     line still in world coordinates have
C     to convert to screen coordinates
C
C     Server mode no graphics
C
      IF ( SERVER ) THEN
          RETURN
      ENDIF
      CONST=32000.0
      XT1=X1
      YT1=Y1
      XT2=X2
      YT2=Y2
      WX1=0
      WY1=0
      WX2=0
      WX2=0
C     Check for a zero length line
      IF(SAME(X1,X2).AND.SAME(Y1,Y2) ) RETURN
      EXT=0.0
C     check for line thickness
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.GT.0 ) THEN
         TOT=ABS(PAPTOW*LTHKR(1,MOD(THK,1024))/2.0)
         END=MOD(THK,8192)/1024
         CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
         IF ( END.EQ.4 ) THEN
            EXT=TOT
            CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
            CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
         END IF
      END IF
C*
      CALL CLIP(XT1,YT1,XT2,YT2,DISPV)
C*
      IF ( DISPV ) THEN
C*       Line thickness draw
         IF ( THK.GT.0 ) CALL WTOSVX(TOT,INC)
C
         IF ( THK.GT.0.AND.INC.GT.1.5 ) THEN
            IF ( END.EQ.2 ) CALL GCIRCL(X1,Y1,TOT)
            CALL VV0L15(L1,L2,L3,TOT,N1(1),N2(1),N3(1),
     +                               N1(2),N2(2),N3(2))
            CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
            CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
            TWXMIN=2*WXMIN-WXMAX
            TWYMIN=2*WYMIN-WYMAX
            TWXMAX=2*WXMAX-WXMIN
            TWYMAX=2*WYMAX-WYMIN
            CALL RSWAP(TWXMIN,WXMIN)
            CALL RSWAP(TWYMIN,WYMIN)
            CALL RSWAP(TWXMAX,WXMAX)
            CALL RSWAP(TWYMAX,WYMAX)
            CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,STATUS)
            CALL VC0PLP(N1(1),N2(1),N3(1),XT1,YT1,WX1,WY1)
            CALL VC0PLP(N1(1),N2(1),N3(1),XT2,YT2,WX2,WY2)
            CALL CLIP(WX1,WY1,WX2,WY2,OK)
            CALL WO2SC(WX1,WY1,VX1,VY1)
            CALL WO2SC(WX2,WY2,VX2,VY2)
            IX(1)=NINT(MINMAX(VX1,CONST))-VPOX
            IY(1)=NINT(MINMAX(VY1,CONST))-VPOY
            IX(4)=IX(1)
            IY(4)=IY(1)
            IX(1)=NINT(MINMAX(VX2,CONST))-VPOX
            IY(1)=NINT(MINMAX(VY2,CONST))-VPOY
            CALL VC0PLP(N1(2),N2(2),N3(2),XT2,YT2,WX2,WY2)
            CALL VC0PLP(N1(2),N2(2),N3(2),XT1,YT1,WX1,WY1)
            CALL CLIP(WX1,WY1,WX2,WY2,OK)
            CALL WO2SC(WX2,WY2,VX2,VY2)
            CALL WO2SC(WX1,WY1,VX1,VY1)
            IX(2)=NINT(MINMAX(VX2,CONST))-VPOX 
            IY(2)=NINT(MINMAX(VY2,CONST))-VPOY 
            IX(3)=NINT(MINMAX(VX1,CONST))-VPOX 
            IY(3)=NINT(MINMAX(VY1,CONST))-VPOY 
            N=4
            CALL POLY_SORT(IX,IY,N,DIR)
            FILVAL = TOOLPEN_COLOR
            IF(DDCODE.EQ.0) THEN
                CALL GPR_$START_PGON(IX(1),IY(1),STATUS)
                CALL GPR_$PGON_POLYLINE(IX,IY,N,STATUS)
                CALL GPR_$CLOSE_FILL_PGON(STATUS)
            ELSEIF(DDCODE.EQ.1) THEN
                CALL GPR_$START_PGON(IX(1),IY(1),STATUS)
                CALL GPR_$PGON_POLYLINE(IX,IY,N,STATUS)
                CALL GPR_$CLOSE_FILL_PGON(STATUS)
                DO 10 II=1,4
                  IX(II) = IX(II) +VPOX
                  IY(II) = IY(II) +VPOY
10              CONTINUE
                CALL POLY_BUILD(VPSEGN,IX,IY,N)
            ELSEIF(DDCODE.EQ.2) THEN
                CALL POLY_BUILD(CVPN,IX,IY,N)
            ENDIF
            IF ( END.EQ.2 ) CALL GCIRCL(X2,Y2,TOT)
            CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,STATUS)
C           restore world window.
            CALL RSWAP(TWXMIN,WXMIN)
            CALL RSWAP(TWYMIN,WYMIN)
            CALL RSWAP(TWXMAX,WXMAX)
            CALL RSWAP(TWYMAX,WYMAX)
         ELSE
            CALL WO2SC(XT1,YT1,VX1,VY1)
            CALL WO2SC(XT2,YT2,VX2,VY2)
C           now we have screen coords VX1,VY1,VX2,VY2
C           must draw line with correct fonting
            CALL DRAWLS(VX1,VY1,VX2,VY2)
C SPB - Are we drawing text when we get here ?
         END IF
      END IF
 
      END
C       @(#)  256.1 date 12/16/89 drsrec.ftn Daxcad revision 1.19
      SUBROUTINE DRSREC(X1,Y1,X2,Y2)
C     ==============================
C
C
C1    Draw a rectangle on the screen in screen coords
C1    from the start point X1,Y1 to the diagonally
C1    opposite corner X2,Y2.
C1
C
C
      INTEGER*2 X1,Y1,X2,Y2,AX(5),AY(5),NPOS
      INTEGER*4 ST
      LOGICAL OBSCURED
C
 
      NPOS=5
C     first point
      AX(1)=X1
      AY(1)=Y1
C     second
      AX(2)=X2
      AY(2)=Y1
C     third
      AX(3)=X2
      AY(3)=Y2
C     fourth
      AX(4)=X1
      AY(4)=Y2
C     fifth
      AX(5)=X1
      AY(5)=Y1
 
      CALL GPR_$MOVE(X1,Y1,ST)
      CALL GPR_$POLYLINE(AX,AY,NPOS,ST)
 
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DRWARC(X,Y,WRAD,SANG,EANG)
C     ====================================
C
C1    vartype          R R  R    R    R
C1    iostatus         I I  I    I    I
C
C2       Subroutine ARCCT draw a arc  centre X,Y
C2       with radius WRAD drawing anti-clockwise
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
C
      REAL MAXN
      INTEGER*4 COUNT,I
      INTEGER*4 TOTAL
      LOGICAL OK
      REAL RADS,X,Y,WRAD,NX,NY,TRS(3,3),OX,OY,PIXELS,
     +  SRAD,PI,SANG,EANG,ANGLE,VX1,VY1,VX2,VY2
      PARAMETER( PIXELS=1.8 , MAXN = 1000.0 )
      EXTERNAL WTOSVX,ROTP2D,DRAWLW,NEWXY,ARCCHK
C     Check if arc may exists on the current viewport.
      CALL ARCCHK(X,Y,WRAD,SANG,EANG,TOTAL,DISPV)
C     if not ok,then not visible.
      IF ( .NOT. DISPV ) RETURN
C     test actual screen size of arc.
      CALL WTOSVX(WRAD,SRAD)
C     Calculate the angle in radians which will give a
C     smooth curve for the size of radius.
      RADS=PIXELS/MAX(1.0,SQRT(SRAD))
C     find included angle.
      ANGLE=ABS(EANG-SANG)
      IF ( EANG .LT. SANG ) ANGLE=PI(2.0)-ANGLE
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc.
      COUNT=MIN(1.0+ABS(ANGLE/RADS),MAXN)
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc.
      RADS=DBLE(ANGLE/REAL(COUNT))
C     Obtain the transformation matrice which rotates about
C     the centre of the circle X,Y by the angle RADS.
      CALL ROTP2D(X,Y,RADS,TRS)
C     starting point of the drawing of the circle.
      OX=X+WRAD*COS(SANG)
      OY=Y+WRAD*SIN(SANG)
C     loop for the whole number calculated above.
      OK=.FALSE.
      IF(TOTAL.EQ.0.AND.IMBUFF(12).EQ.0) THEN
         DO 10 I=1,COUNT,1
C           transform OX to NX by transformation matrice
C           and also OY to NY.
            CALL NEWXY(OX,OY,NX,NY,TRS)
C           Draw between old point and new.
            CALL WO2SC(OX,OY,VX1,VY1)
            CALL WO2SC(NX,NY,VX2,VY2)
C           now we have screen coords VX1,VY1,VX2,VY2
C           must draw line with correct fonting
            CALL DRAWLS(VX1,VY1,VX2,VY2)
C           old now new point on the circumference
            OX=NX
            OY=NY
 10      CONTINUE
         OK=.TRUE.
      ELSE
         DO 20 I=1,COUNT,1
C           transform OX to NX by transformation matrice
C           and also OY to NY.
            CALL NEWXY(OX,OY,NX,NY,TRS)
C           Draw between old point and new.
            CALL DRAWLT(OX,OY,NX,NY)
C           old now new point on the circumference
            OX=NX
            OY=NY
            IF ( DISPV ) OK=.TRUE.
 20      CONTINUE
      END IF
C     Arc exists
      DISPV=OK
      END
*
      SUBROUTINE DRWARC1(X,Y,WRAD,SANG,EANG)
C     ====================================
C
C1    vartype          R R  R    R    R
C1    iostatus         I I  I    I    I
C
C2       Subroutine ARCCT draw a arc  centre X,Y
C2       with radius WRAD drawing anti-clockwise
      include 'include/masti.inc'
C
C
      INTEGER COUNT,I,MAXN
      INTEGER*4 TOTAL
      LOGICAL OK
      REAL RADS,X,Y,WRAD,NX,NY,TRS(3,3),OX,OY,PIXELS,
     +  SRAD,PI,SANG,EANG,ANGLE
      PARAMETER( PIXELS=1.8 , MAXN = 1000 )
      EXTERNAL WTOSVX,ROTP2D,DRAWLW,NEWXY,ARCCHK
C     Check if arc may exists on the current viewport
      CALL ARCCHK(X,Y,WRAD,SANG,EANG,TOTAL,DISPV)
C     if not ok,then not visible
      IF ( .NOT. DISPV ) RETURN
C
C     test actual screen size of arc
      CALL WTOSVX(WRAD,SRAD)
C     Calculate the angle in radians which will give a
C     smooth curve for the size of radius
      RADS=PIXELS/SQRT(SRAD)
C     find included angle
      ANGLE=ABS(EANG-SANG)
      IF ( EANG .LT. SANG ) ANGLE=PI(2.0)-ANGLE
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc
      COUNT=MIN(1+NINT(ANGLE/RADS),MAXN)
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc
      RADS=DBLE(ANGLE/REAL(COUNT))
C     Obtain the transformation matrice which rotates about
C     the centre of the circle X,Y by the angle RADS
      CALL ROTP2D(X,Y,RADS,TRS)
C     starting point of the drawing of the circle
      OX=X+WRAD*COS(SANG)
      OY=Y+WRAD*SIN(SANG)
C     loop for the whole number calculated above
      OK=.FALSE.
      DO 10 I=1,COUNT,1
C        transform OX to NX by transformation matrice
C        and also OY to NY
         CALL NEWXY(OX,OY,NX,NY,TRS)
C        Draw between old point and new
         CALL DRAWLW(OX,OY,NX,NY)
C        old now new point on the circumference
         OX=NX
         OY=NY
         IF ( DISPV ) OK=.TRUE.
 10   CONTINUE
C     Arc exists
      DISPV=OK
      END
C
 
      SUBROUTINE GCIRCL(X1,X2,X4)
C     ===========================
C                       R,  R, R
      include   'include/wtov.inc'
C
      REAL X1,X2,X4,XT1,XT2,XT4
      INTEGER *4 STATUS,VAL
      INTEGER*2 CENT(2),SRAD,RECT(2,2)
C
      CALL WO2SC(X1,X2,XT1,XT2)
      CALL WTOSVX(X4,XT4)
C     **********************************
C     23/9/87 ADDED ABS TO STOP CRASHING
C     **********************************
      IF ( ABS(XT1).GT.32768 ) RETURN
      IF ( ABS(XT2).GT.32768 ) RETURN
      IF ( ABS(XT4).GT.32768 ) RETURN
      CENT(1)=NINT(XT1)
      CENT(2)=NINT(XT2)
      SRAD=NINT(XT4)
C
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,STATUS)
      CALL GPR_$CIRCLE_FILLED(CENT,SRAD,STATUS)
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,STATUS)
C
      END
*
*
      SUBROUTINE GTSPLN(X,Y,LINSTR,INV)
C     =============================
C1    vartype             C     L
C1    iostatus            I     I
C2
C2    Formerly GTXT_SYS_PUTLIN prints the string 'LINSTR'
C2    at the current cursor position,leaving the text cursor
C2    at the start of the next line.The text is printed
C2    in inverse video mode if logical flag INV is passed
C2    as .TRUE.,otherwise normal video mode is used.
C
      include 'include/gpr.ins.inc'
      include 'include/dtext.inc'
      include 'include/daxcad_x.inc'
C
      REAL X,Y
      INTEGER*2 BMSIZ(1:2),CURPOS(2),TSTART(2),
     +          TWLIMS(4)
      INTEGER*2 EVENT,FONTID,SCMODE,TEND,L,LEN,CURTMP
      INTEGER*4 DISPDE,ST,LCPNT,ICHAR,NLEN1
      LOGICAL UNOBSCURED,INV
      CHARACTER CH
      EXTERNAL NLEN1
C
      CHARACTER LINSTR*(*)
C
C     Find the length of the string
      L=NLEN1(LINSTR)
C     Grab the display first
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
C
C     Find the current cursor position
C      CALL INCURP(X,Y)
C     Sve the x-position of the cursor
      CURPOS(1)=INT(X)
      CURPOS(2)=INT(Y)
      CURTMP=INT(X)
      CALL MOVES(X,Y)
      IF (INV) THEN
C     Print this message in reverse video
C     Clear the line to be printed to inverse
         TWLIMS(1)=CURPOS(1)-1
         TWLIMS(2)=CURPOS(2)-13
         TWLIMS(3)=TWLIMS(1)+L*TEXW
         TWLIMS(4)=TWLIMS(2)+TEXH+1
C
         CALL CLEARW(REAL(TWLIMS(1)),REAL(TWLIMS(2)),
     +               REAL(TWLIMS(3)),REAL(TWLIMS(4)))
C         CALL GPR_$SET_TEXT_BACKGROUND_VALUE(GPR_$WHITE,ST)
C         CALL GPR_$SET_TEXT_VALUE(GPR_$BLACK,ST)
C
C
C        Print the text to the screen at current position
         CALL GPR_$TEXT(LINSTR,L,ST)
C        Set video back to normal
C         CALL GPR_$SET_TEXT_BACKGROUND_VALUE(GPR_$BLACK,ST)
C         CALL GPR_$SET_TEXT_VALUE(GPR_$WHITE,ST)
      ELSE
C        Print in normal video
C        Print the text to the screen at current position
         CALL GPR_$TEXT(LINSTR,L,ST)
      END IF
C
C     Go find where to place cursor now
C     update current cursor position to start of new line
      CURPOS(1)=CURTMP
      CURPOS(2)=CURPOS(2)+TEXH+3
      X=CURPOS(1)
      Y=CURPOS(2)
C
C     Release display before return to caller
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTSPMG(X,Y,LINSTR,INV)
C     =============================
C1    vartype             C     L
C1    iostatus            I     I
C2
C2    Formerly GTXT_SYS_PUTMSG prints the string 'LINSTR'
C2    at the current cursor position,leaving the text cursor
C2    at the end of the printed text.The text is printed
C2    in inverse video mode if logical flag INV is passed
C2    as .TRUE.,otherwise normal video mode is used.
C
      include 'include/gpr.ins.inc'
      include 'include/daxcolor.inc'
      include 'include/daxcad_x.inc'
      include 'include/dtext.inc'
      include 'include/interface.inc'
C
      REAL X,Y
      INTEGER*2 BMSIZ(1:2),CURPOS(2),CURROP(8),CURORG(2),TSTART(2)
      INTEGER*2 TWLIMS(4)
      INTEGER*2 EVENT,FONTID,SCMODE,TEND,L
      INTEGER*4 DISPDE,ST,CURPAT,LCPNT,NLEN1
      INTEGER*4 COLB
      LOGICAL UNOBSCURED,CURACT,INV
      CHARACTER CH,JUNK,LINBUF*40
      CHARACTER LINSTR*(*)
      EXTERNAL NLEN1
C
C     Find the length of the string
      L=NLEN1(LINSTR)
C     Grab the display first
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
      CURPOS(1)=INT(X)
      CURPOS(2)=INT(Y)
      CALL MOVES(X,Y)
      IF (INV) THEN
C     Print this message in reverse video
         CALL TOOLPEN_TEXT(MENUB,MENUF,.FALSE.,ST)
C        Clear the line to be printed to inverse
         TWLIMS(1)=CURPOS(1)-1
         TWLIMS(2)=CURPOS(2)-13
         TWLIMS(3)=TWLIMS(1)+L*TEXW
         TWLIMS(4)=TWLIMS(2)+TEXH+1
C
         CALL TOOLPEN(MENUB)
         CALL CLEARW(REAL(TWLIMS(1)),REAL(TWLIMS(2)),
     +               REAL(TWLIMS(3)),REAL(TWLIMS(4)))
C
C        Print the text to the screen at current position
         CALL TOOLPEN(MENUF)
         CALL GPR_$TEXT(LINSTR,L,ST)
C        Set video back to normal
         CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
      ELSE
C        Print in normal video
C        Print the text to the screen at current position
         CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
         CALL GPR_$TEXT(LINSTR,L,ST)
      END IF
C
C     Go find where to place cursor now
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C
      SUBROUTINE INCURP(X,Y)
C     ======================
C1                      R,R

      include 'include/gpr.ins.inc'
C
      INTEGER*4 ST,CURPAT
      REAL X,Y
      INTEGER*2 CURPOS(2),CURROP(8),CURORG(2)
      LOGICAL UNOBSCURED,CURACT
C
C     Time to acquire the display now
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$INQ_CURSOR(CURPAT,CURROP,CURACT,CURPOS,CURORG,ST)
      X=REAL(CURPOS(1))
      Y=REAL(CURPOS(2))
      CALL GPR_$RELEASE_DISPLAY(ST)
      END
C
C-----------------------------------------------------------------
C
      FUNCTION MENKEY(MENUN,CURSX,CURSY)
C     ====================================
C
C2     This function returns an integer value equal to the
C2     menu cell number hit in menu number MENUN by the cursor
C2     position CURSX,CURSY. If the hit is not within the menu
C2     area,then a value of zero is returned.
C
      include 'include/gtxt2.inc'
C
      INTEGER*2 MENTMP,MENUX,MENUY,MENUN2
      INTEGER*4 MENKEY,MENUN,CURSX,CURSY
C
      MENUN2=MENUN
C     Check if menu has been initialized,if not
C     return with MENKEY set to zero.
C
      IF (.NOT.GMA(MENUN2)) THEN
C        PRINT*, 'MENKEY is 0',GMA(MENUN2)
          MENTMP=0
          GOTO 999
      END IF
C
      MENUX=CURSX-GML(MENUN2,1)
      MENUY=CURSY-GML(MENUN2,2)+13
C      PRINT*, 'CURSX XY',CURSX,CURSY
C      PRINT*, 'MENU XY',MENUN,MENUX,MENUY
C     Test for existence of cursor hit within the
C     screen limits of the menu area.
C
      IF ((MENUX.LT.0).OR.(MENUY.LT.0)) THEN
          MENTMP=0
          GOTO 999
      ELSE IF ( GMC(MENUN2,6).EQ.0 ) THEN
         IF (MENUX.GT.GML(MENUN2,3)) THEN
             MENTMP=0
             GOTO 999
         END IF
      ELSE IF ( GMC(MENUN2,5).EQ.0 ) THEN
         IF (MENUY.GT.GML(MENUN2,4)) THEN
            MENTMP=0
            GOTO 999
         END IF
C
C
      END IF
C
C     calculate the position of the hit in the menu
C     and return the cell number
C
      IF (GMC(MENUN2,6).EQ.0) THEN
         MENTMP=MENUY/GMC(MENUN2,5)+1
         IF (MENTMP.GT.GML(MENUN2,6)) THEN
             MENTMP=0
             GOTO 999
         END IF
      ELSE
         MENTMP=MENUX/GMC(MENUN2,6)+1
         IF (MENTMP.GT.GML(MENUN2,6)) THEN
             MENTMP=0
             GOTO 999
          END IF
      END IF
C
 999  CONTINUE
      MENKEY=MENTMP
      RETURN
      END
C
C-----------------------------------------------------------------
C
      FUNCTION MINMAX (VAL,CONST)
C     ===========================
C1    VARTYPE    R      R    R
C1    IOSTAT     O      I    I
C
C2    This function returns the minimum
C2    or maximum value within the limits of
C2    the constant. The polarity does not matter
C
      REAL MINMAX,VAL,CONST
C
      CONST=ABS(CONST)
      IF(VAL.GE.0) THEN
          MINMAX=MIN(VAL,CONST)
      ELSE
          MINMAX=MAX(VAL,-CONST)
      ENDIF
      END
*
*
      SUBROUTINE MOVES(XC,YC)
C     =======================
C1                      R, R
C1                      I, I
C2       Subroutine MOVES moves the cursor position to new location
C2      on the screen (note. no checking is done to see if it exists
C2      or not )
C
CC      Record the new position of the screen cursor through
C       the common block SCURSR
C
      include 'include/gpr.ins.inc'
      include  'include/curpos.inc'
C
      REAL XC,YC
      INTEGER*2 IAPX,IAPY
      INTEGER*4 ST
      LOGICAL OBSCURED
      EXTERNAL GPR_$ACQUIRE_DISPLAY,GPR_$MOVE
C
      IF ( ABS(XC) .GT. 32767 .OR. ABS(YC) .GT. 32767 ) THEN
         WRITE(UNIT=10,FMT=*) '[MOVES] out of range XC YC ',XC,YC
         RETURN
      END IF
      IAPX=INT(XC)
      IAPY=INT(YC)
      SCPX=XC
      SCPY=YC
      OBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$MOVE(IAPX,IAPY,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE NBP()
C     ================
C1
C1
C2    Subroutine NBP disables the bitpad service process
C
      CHARACTER*255 COM
      INTEGER*4 ST
C
      WRITE(UNIT=COM,FMT='(4A)',ERR=999) '/com/tctl',
     +                                   ' -line ',
     +                                   ' 2',
     +                                   ' -nobp_enable'

      CALL SHELLP(COM,ST)
      RETURN

999   CONTINUE
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE PEN(COLOR)
C     ======================
C1                     I2
C1                     I
C2    Subroutine PEN changes the data level to the passed
C2    colour number COLOR,range limit of 16 currently.
C2         0 = Black
C2        15 = White
C
      include 'include/daxcolor.inc'
C
      INTEGER*2 COLOR
      INTEGER*4 ST,COLOR4
      INTEGER*4 DUMMY
      COLOR4=COLOR
 
C     set drawing color
      CALL TOOLPEN(COLOR4)
C     save current draw color
      DUMMY= COLOR
      COLPEN = DUMMY
C 
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE POINTS(X1,Y1)
C     ========================
C1                       R, R
C1                       I, I
C
C2    Draws a small point at the specified location
C2    must call DRAWLS to make sure drawing codes are
C2    adhered to 
C
C
      REAL X1,Y1,X2,Y2
C
C
      X2 = X1
      Y2 = Y1
C     draw into the screen now
      CALL DRAWLS(X1,Y1,X2,Y2)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE RASTER(OPT)
C     ======================
C
C1    vartype           I4
C1    iostatus          I
C
C2    Subroutine RASTER sets the raster operation code
C2    OPT as the current mode for raster activity.
C2       OPT has a valid range of 0-15,although all modes
C2    may not be supported in a given system.
C2
C2       OPT=0  - assign zero to all destination values
C2       OPT=1  - assign source AND destination to new destination
C2       OPT=2  - assign source AND complement of dest to new dest
C2       OPT=3  - assign source values to destination
C2       OPT=4  - assign complement of source AND dest to dest
C2       OPT=5  - assign destination values to destination
C2       OPT=6  - assign source XOR dest to dest
C2       OPT=7  - assign source OR dest to dest
C2       OPT=8  - assign complement of source AND comp of dest to dest
C2       OPT=9  - assign source EQUIVALENCE dest to dest
C2       OPT=10 - assign complement of dest to dest
C2       OPT=11 - assign source OR comp of dest to dest
C2       OPT=12 - assign complement of source to dest
C2       OPT=13 - assign complement of source OR dest to dest
C2       OPT=14 - assign comp of source OR comp of dest to dest
C2       OPT=15 - assign 1 to all destination values
C
      include 'include/gpr.ins.inc'
C
      include 'include/apollo.inc'
      include 'include/viewport.inc'
      include 'include/daxcad_x.inc'                             
      include 'include/daxcolor.inc'                             
C 
      INTEGER*4 OPT,ST,OPTL(15),CBITM
      INTEGER*2 WIN(2),PLMASK,ROPS(8),I,AOPT,J
      INTEGER*4 XOR_MASK
      LOGICAL UNOBSCURED,ACTIVE
C

      PLMASK = -1
C
C     use raster op as passed for APOLLO system.
      AOPT=OPT
      CALL GPR_$SET_PLANE_MASK(PLMASK,ST)
      IF ( XVERSION ) THEN
             I = 0
             CALL GPR_$SET_RASTER_OP(I,AOPT,ST)
      ELSE
          DO 25 I=0,HIPLAN
             CALL GPR_$SET_RASTER_OP(I,AOPT,ST)
 25       CONTINUE
      ENDIF

      IF ( HIPLAN.EQ.0) THEN
          RETURN
      ENDIF
      IF(OPT .EQ. XOR_FLAG) THEN
C             set XOR color if not already set to XOR
              CALL DAX_XOR(COLOR_MAP(0), COLOR_MAP(TOOLPEN_COLOR), 
     +                     XOR_MASK)
              CALL GPR_$SET_DRAW_VALUE(XOR_MASK,ST)
              CALL GPR_$SET_FILL_VALUE(XOR_MASK,ST)
      ELSE IF(RASTER_OP_FLAG .EQ. XOR_FLAG) THEN
C             Reset original draw colour that was set by XOR
              CALL GPR_$SET_DRAW_VALUE(COLOR_MAP(TOOLPEN_COLOR),ST)
              CALL GPR_$SET_FILL_VALUE(COLOR_MAP(TOOLPEN_COLOR),ST)
      ENDIF
C     set current raster op global flag
      RASTER_OP_FLAG = OPT
  
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 reldis.ftn Daxcad revision 1.19
      SUBROUTINE RELDIS(TT)
C     ======================
 
      INTEGER*2 TT1,TT
      INTEGER*4 ST
C     release the display for popping window
      CALL GPR_$FORCE_RELEASE(TT,ST)
C      WRITE(10,*) 'Number of Aquired',TT,ST
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 s1grow.ftn Daxcad revision 1.19
      SUBROUTINE S1GROW()
C     =================
C1
C1    Makes button 1 of the mouse abort an apollo pad
C2
C
C
      CHARACTER*255 COM
      INTEGER*4 ST
C
      WRITE(UNIT=COM,FMT='(6A)',ERR=999) '/com/xdmc',
     +                                   ' ''kd',
     +                                   ' m1',
     +                                   ' pw@; ',
     +                                   ' wc -q',
     +                                   ' ke'''

      CALL SHELLP(COM,ST)
      RETURN

999   CONTINUE

C
      END
C
C
C       @(#)  256.1 date 12/16/89 s2grow.ftn Daxcad revision 1.19
      SUBROUTINE S2GROW()
C     =================
C1
C1
C1    Makes button 1 of the mouse window grow or shrink with echo 
C2
C
C
      CHARACTER*255 COM
      INTEGER*4 ST
C
      WRITE(UNIT=COM,FMT='(5A)',ERR=999) '/com/xdmc',
     +                                   ' ''kd',
     +                                   ' m1',
     +                                   ' wge',
     +                                   ' ke'''

      CALL SHELLP(COM,ST)
      RETURN

999   CONTINUE

C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE SCURS(C,XC,YC)
C     =========================
C1                     I, R, R
C1                     O, O, O
C
C2       Returns the value from the terminal
C2       C :First value equal to character typed
C2       XC:x position of graphics cursor in screen coords
C2       YC:y position of graphics cursor in screen coords
C       coordinates being in screen coordinates
C
      include 'include/apollo.inc'
      include 'include/dig2wo.inc'
C
      INTEGER*4 C,IX,IY
      REAL XC,YC
      CHARACTER*1 HITKEY
      LOGICAL  OK
C
      EXTERNAL APCURS
C
      CALL APCURS(HITKEY,SOURCE,IX,IY)
      XC=IX
      YC=IY
      C=ICHAR(HITKEY)
C      PRINT*, 'LEAVING APCURS'
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 stcurp.ftn Daxcad revision 1.19
      SUBROUTINE STCURP(X,Y)
C     ======================
C1                      R,R
C1                      I,I
      include 'include/gpr.ins.inc'
C
      INTEGER*4 ST
      REAL X,Y
      INTEGER*2 CURPOS(2)
      LOGICAL UNOBSCURED
C
      CURPOS(1)=INT(X)
      CURPOS(2)=INT(Y)
C     Time to acquire the display now
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
C      CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
      CALL GPR_$MOVE(CURPOS(1),CURPOS(2),ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 updaaa.ftn Daxcad revision 1.19
      SUBROUTINE UPDAAA(APX,APY)
C     ==========================
C
C1     Update the cursor position if locator movement
C1     is greater then a threshold of 3 pixels total
C1     on both axes.
C
      include 'include/gpr.ins.inc'
C
C
      include 'include/apollo.inc'
      include 'include/menun.inc'
C
C
      INTEGER*2 APX,APY,CIR(2)
      INTEGER*4 ST,SX,SY
      LOGICAL WOK,OK,CLEAR
C
      EXTERNAL UPDACP
C
C     is the hit in the graphics area?
      SX=APX
      SY=APY
      CALL WRKHIT(SX,SY,WOK)
C
      IF ( CURVIS ) THEN
C        erase the old cursor
         CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
         RMN=0
      END IF
C
C     update the cursor position
      CALL UPDACP(APX,APY)
      CURVIS=WOK
C
      IF ( WOK ) THEN
C        draw the cursor at the new position
         CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
         RMN=0
      ELSE
C        menu input request
         CALL MENHIT(SX,SY,MEN,CELLN,CTEXT,CCMD,OK)
         IF ( OK ) THEN
          IF ( .NOT.(RMN.EQ.MEN.AND.RCEL.EQ.CELLN)) THEN
            IF ( RMN.GT.0 ) CALL GTIVMC(RMN,RCEL)
            RMN=MEN
            RCEL=CELLN
            CALL GTIVMC(RMN,RCEL)
          END IF
         ELSE
C           draw the cursor at the new position
            IF ( RMN.GT.0 ) CALL GTIVMC(RMN,RCEL)
            CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
            CURVIS=.TRUE.
         END IF
      END IF
C     move the current position to centre of cursor
      CALL GPR_$MOVE(APCURX(1),APCURY(3),ST)
      CALL ROPXOR()
C
C
      END
*
C       @(#)  256.1 date 12/16/89 updacp.ftn Daxcad revision 1.19
      SUBROUTINE UPDACP(APX,APY)
C     ==========================
C
C
C1     Update the cursor position as defined in the apollo common
C1     block data areas APCURX,APCURY. Does not do anything with
C1     the cursor it's self.
C1
C
      include 'include/apollo.inc'
      include 'include/curwin.inc'
C
C
      REAL RINC
      INTEGER*2 APX,APY,INC
C
C2     Ensure the cursor remains on the screen at all times
C2     by checking for limits of apollo screen being exceeded.
C
C      IF (APX.GT.1280 ) APX=1280
C      IF (APY.GT.1024 ) APY=1024
C      IF (APX.LT.   0 ) APX=0
C      IF (APY.LT.   0 ) APY=0
 
C     set size of search zone around hit point
C
C      IF ( CRSIZE .LT. 60 ) INC=10
C      IF ( CRSIZE .GT. 400 ) INC=-10
C      CRSIZE=CRSIZE+INC
 
C     update the cursor position
C     --------------------
      APCURX(1)=APX
      APCURX(2)=APX
 
      APCURY(1)=APY-CRSIZE
      APCURY(2)=APY+CRSIZE
C     --------------------
      APCURX(3)=APX-CRSIZE
      APCURX(4)=APX+CRSIZE
C
      APCURY(3)=APY
      APCURY(4)=APY
 
      IF ( NPOS.EQ.4 ) RETURN
C
C     --------------------
      APCURX(5)=APX-XWIDTH
      APCURX(6)=APX+XWIDTH
 
      APCURY(5)=APY-XWIDTH
      APCURY(6)=APY-XWIDTH
C     --------------------
      APCURX(7)=APX+XWIDTH
      APCURX(8)=APX+XWIDTH
 
      APCURY(7)=APY-XWIDTH
      APCURY(8)=APY+XWIDTH
C     --------------------
      APCURX(9)=APX+XWIDTH
      APCURX(10)=APX-XWIDTH
 
      APCURY(9)=APY+XWIDTH
      APCURY(10)=APY+XWIDTH
C     --------------------
      APCURX(11)=APX-XWIDTH
      APCURX(12)=APX-XWIDTH
 
      APCURY(11)=APY+XWIDTH
      APCURY(12)=APY-XWIDTH
C     --------------------
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 4/20/90 updapc.ftn Daxcad revision 1.19
      SUBROUTINE UPDAPC(APX,APY)
C     ==========================
C
C1     Update the cursor position if locator movement
C1     is greater then a threshold of 3 pixels total
C1     on both axes.
C
      include 'include/gpr.ins.inc'
C
C
      include 'include/apollo.inc'
C
C
      INTEGER*2 APX,APY,CIR(2)
      INTEGER*4 ST
      LOGICAL UNOB
C
      EXTERNAL UPDACP
C
C     erase the old cursor
      UNOB=GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
C     update the cursor position
      CALL UPDACP(APX,APY)
C     draw the cursor at the new position
      CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
C     move the current position to centre of cursor
      CALL GPR_$MOVE(APCURX(1),APCURY(3),ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTICUR(POSITION,WIN,CLEARIT)
C     =======================================
C1    VARYPE             C*(*)    I4    L
C1    IOSTAT              I       I     I
C
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    POSITION       ->     Cursor position
C2    WIN            ->     Window number
C2    CLEARIT        ->     A Clear text
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      include 'include/gtxt1.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
      include 'include/daxcad_x.inc'
      include 'include/cursor.inc'
C
      INTEGER*4 ST
      INTEGER*4 WIN
      INTEGER*2 X,Y
      INTEGER*2 LENGTH
      INTEGER*2 SIZE(2)
      INTEGER*2 RECT(4)
      INTEGER*4 NLEN
      INTEGER*4 COLF,COLB
      CHARACTER*(*) POSITION
      CHARACTER*40 NULL
      LOGICAL CLEARIT
C
      EXTERNAL NLEN
C
C     Window position
C
      IF ( .NOT.DAXTRACK ) THEN
C         Too much tracking causes problems for now
          RETURN
      ENDIF
      X=REAL(GWL(WIN,1))
      Y=REAL(GWL(WIN,2))
C
C     Draw the text
C
      COLB = VIEWB
      COLF = VIEWF
      LENGTH = NLEN(POSITION)
      IF ( CLEARIT) THEN
          CALL TOOLPEN_TEXT(COLF,COLB,.TRUE.,ST)
      ELSE
          CALL TOOLPEN_TEXT(COLF,COLB,.TRUE.,ST)
      ENDIF
C
      CALL GPR_$MOVE(X,Y,ST)
      CALL GPR_$TEXT(POSITION,LENGTH,ST)
C
      END






