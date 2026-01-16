C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 apolib.f   */
C
      SUBROUTINE APSHUT
C     =================
C1    no arguments required
C
C2    Subroutine  APSHUT releases the apollo display system
C2    and returns the control of the screen to the display
C2    manager.
C2
      include 'include/gpr.ins.inc'
C
      include 'include/apollo.inc'
      include 'include/sioport.inc'
C
      INTEGER*2 I
      INTEGER*4 ST
      LOGICAL KILL
C
C     release the display from GPR control
      CALL GPR_$FORCE_RELEASE(I,ST)
C     disable all input devices
      CALL GPR_$DISABLE_INPUT(GPR_$KEYSTROKE,ST)
      CALL GPR_$DISABLE_INPUT(GPR_$LOCATOR,ST)
      CALL GPR_$DISABLE_INPUT(GPR_$BUTTONS,ST)
C
C     Check the sio ports to see if there are in use
      DO 10 I = 1,3
        IF(SIOUSE(I).NE.0) THEN 
           CALL RSTSIO(I)
           CALL SIOCLS(I)
        END IF
  10  CONTINUE
C     try to delete the bitmap
      KILL=.FALSE.
      CALL GPR_$TERMINATE(KILL,ST)
      CLOSE(10,STATUS='DELETE')


C
C     return with DM back in control--I hope!
C
      END
C
      SUBROUTINE BCROSS(X,Y)
C     =====================
C1                     R,R
C1                     I,I
      include 'include/wtov.inc'
      include 'include/dcross.inc' 
      include 'include/viewport.inc' 
      include 'include/style.inc' 
C
      REAL SIZE,X,Y,X1,X2,Y2,Y1
      LOGICAL OK
      EXTERNAL DRAWLS,WRKHIT,ROPXOR,ROPREP
      DATA SIZE/10.0/
C
      CALL SETAPN(COLOUR)
C     if scrosses are switched off, return.
      IF (.NOT.DCROS) RETURN
C
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set viewporting draw code please
          DDCODE = 2
      ENDIF
C
      CALL WRKHIT(X,Y,OK)
      IF ( .NOT. OK ) RETURN
      CALL ROPXOR()
      X1=MAX(VXMIN,X-SIZE)
      X2=MIN(VXMAX,X+SIZE)
      Y1=MAX(VYMIN,Y)
      Y2=MIN(VYMAX,Y)
      CALL DRAWLS(X1,Y1,X2,Y2)
      X1=MAX(VXMIN,X)
      X2=MIN(VXMAX,X)
      Y1=MAX(VYMIN,Y-SIZE)
      Y2=MIN(VYMAX,Y+SIZE)
      CALL DRAWLS(X1,Y1,X2,Y2)
      CALL ROPREP()
C
C     reset drawing code
      DDCODE = 0
      END
C
C
      SUBROUTINE CROSS(X,Y)
C     =====================
C1                     R,R
C1                     I,I
      include 'include/wtov.inc'
      include 'include/style.inc' 
C
      REAL SIZE,X,Y,X1,Y1,X2,Y2
      LOGICAL OK
      EXTERNAL DRAWLS,WRKHIT,ROPXOR,ROPREP
      DATA SIZE/2.0/
C
      CALL SETAPN(COLOUR)
      CALL WRKHIT(X,Y,OK)
      IF ( .NOT. OK ) RETURN
      CALL ROPXOR()
      X1=MAX(VXMIN,X-SIZE)
      X2=MIN(VXMAX,X+SIZE)
      Y1=MAX(VYMIN,Y)
      Y2=MIN(VYMAX,Y)
      CALL DRAWLS(X1,Y1,X2,Y2)
      X1=MAX(VXMIN,X)
      X2=MIN(VXMAX,X)
      Y1=MAX(VYMIN,Y-SIZE)
      Y2=MIN(VYMAX,Y+SIZE)
      CALL DRAWLS(X1,Y1,X2,Y2)
      CALL ROPREP()
C
      END
*
      SUBROUTINE DBOX(X,Y)
C     ====================
C1                    R,R
C1                    I,I
C
      include 'include/gpr.ins.inc'
      include 'include/style.inc' 
C
      INTEGER*4 NLEN,ST
C
      REAL SIZE,X,Y
      INTEGER*4 OPT
      LOGICAL OK
      EXTERNAL DRAWLS,WRKHIT,ROPXOR,ROPREP
C
      CALL SETAPN(COLOUR)
      CALL WRKHIT(X-3.0,Y-3.0,OK)
      IF ( .NOT. OK ) RETURN
      CALL WRKHIT(X+3.0,Y+3.0,OK)
      IF ( .NOT. OK ) RETURN
C
      SIZE=3.0
C     set XOR mode
      CALL ROPXOR()
      CALL DRSREC(INT(X-SIZE),INT(Y-SIZE),
     +            INT(X+SIZE),INT(Y+SIZE))
      SIZE=4.0
      CALL DRSREC(INT(X-SIZE),INT(Y-SIZE),
     +            INT(X+SIZE),INT(Y+SIZE))
C     set replace mode
      CALL ROPREP
      END
C
C---------------------------------------------------------------
C
      SUBROUTINE DISFRA()
C     ===================
C
C1    Subroutine DISFRA draws the drawing sheet limits
C1    on the screen,indicating the extents of the world
C1    space which is currently mapped to the paper.
C1    Also the useable limits within the paper extents
C
C
      include 'include/wtov.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
      include 'include/pendat.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 II2,NUM
      INTEGER*4 ST,POINT
      LOGICAL  PLROT
      REAL T,B,L,R,TMP
C
      EXTERNAL DRAWLW
C
C     show the backcloths first
C     return
      CALL SHOBAK()
C     this data sets the border on the paper
C     Top,Bottom,Left,Right
C
C     viewport flag
      IF(.NOT.VPADD) GOTO 20
      IF(MVPACT) THEN
C         save current layer list
          NUM = 6
      ELSE
          NUM=-1
          GOTO 20
      ENDIF
10    CONTINUE
C     if view is active then do it
      IF(NUM.GT.1) THEN
C         All viewport update
          IF(VPNUM(NUM-1)) THEN
C            get layers asccociated with this view
C            get the ascossiated layering for this layer
             CALL UPDBTM(NUM)
C            a view has been updated thus we must reset bitmaps
          ELSE
             NUM=NUM-1
C            try again since view was not active
             GOTO 10
          ENDIF
C         loop if this bitmap is not active
      ELSEIF(NUM.EQ.1) THEN
C         update hidden backing store if DAXPORTS
          IF(.NOT.MAWS) THEN
              CALL UPDBTM(NUM)
          ELSE
C             nothing to do here
              NUM=NUM-1
              GOTO 10
          ENDIF
      ELSEIF(NUM.EQ.0.AND.MAWS) THEN
C         update the display on a MAWS bitmap
          CALL UPDBTM(NUM)
      ELSE
C         DAXPORTS allready updated screen here
          CALL UPDBTM(NUM)
          GOTO 999
      ENDIF
20    CONTINUE
      IF(COLPEN.NE.COLERS)  THEN
          CALL SETDRW(COLFOR)
      ELSE
          CALL SETDRW(COLERS)
      ENDIF
C     select the appropriate line font for the border during
C     viewporting ops
      II2=25
      CALL HARDWF(II2)
C     draw the paper extents on the screen
      CALL DRAWLW(WPXMIN,WPYMIN,WPXMIN,WPYMAX)
      CALL DRAWLW(WPXMIN,WPYMAX,WPXMAX,WPYMAX)
      CALL DRAWLW(WPXMAX,WPYMAX,WPXMAX,WPYMIN)
      CALL DRAWLW(WPXMAX,WPYMIN,WPXMIN,WPYMIN)
C
      IF ( MODELS.GT.0 ) THEN
C        does the user have plotter limits
         L=PLBOR(1)
         T=PLBOR(2)
         B=PLBOR(3)
         R=PLBOR(4)
C
         SIZE =INDEX('A0 A1 A2 A3 A4 ',DRWSHT(:2))
         SIZE =1+((SIZE-1)/3)
         PLXMIN=PAPLIM(SIZE,1)
         PLYMIN=PAPLIM(SIZE,2)
         PLXMAX=PAPLIM(SIZE,3)
         PLYMAX=PAPLIM(SIZE,4)
C
         PLROT=(PLYMAX-PLYMIN)/(PLXMAX-PLXMIN).GT.1.0
         ROTATE=(WPYMAX-WPYMIN)/(WPXMAX-WPXMIN).GT.1.0
 
         IF(PLROT.AND..NOT.ROTATE) THEN
            TMP=T
            T=L
            L=B
            B=R
            R=TMP
         ELSE IF (.NOT.PLROT.AND.ROTATE) THEN
            TMP=T
            T=R
            R=B
            B=L
            L=TMP
         END IF
C
         IF ( .NOT.(L.EQ.0.0.AND.B.EQ.0.AND.R.EQ.0.AND.T.EQ.0) ) THEN
C
C        select the appropriate line font for inner border

         II2=15
         CALL HARDWF(II2)
C        the plotting extents of single sheet feed plotting
C        devices is shown by this inner frame.
C        To ensure a complete plot,the user should use
C        this inner frame as the limit of his drawing
C        draw the plotting extents on the screen
         CALL DRAWLW(WPXMIN+PAPTOW*L,WPYMIN+PAPTOW*B,
     +               WPXMIN+PAPTOW*L,WPYMAX-PAPTOW*T)
         CALL DRAWLW(WPXMIN+PAPTOW*L,WPYMAX-PAPTOW*T,
     +               WPXMAX-PAPTOW*R,WPYMAX-PAPTOW*T)
         CALL DRAWLW(WPXMAX-PAPTOW*R,WPYMAX-PAPTOW*T,
     +               WPXMAX-PAPTOW*R,WPYMIN+PAPTOW*B)
         CALL DRAWLW(WPXMAX-PAPTOW*R,WPYMIN+PAPTOW*B,
     +               WPXMIN+PAPTOW*L,WPYMIN+PAPTOW*B)
         END IF
C        reset the line style for this bitmap
         II2=0
         CALL HARDWF(II2)
C        set color for this bitmap
         CALL SETDRW(COLDRW)
C
      END IF
C     if the paper limits to be updated in other views then go round again
999   CONTINUE
      IF(NUM.GT.-1.AND.VPADD) GOTO 10
      CALL ROPREP()
C     reset to solid line font
      DDCODE = 0
      II2=0
      CALL HARDWF(II2)
      CALL SETDRW(COLDRW)
C
      END

      SUBROUTINE DRWBUF1(DRWMDE,DYNAMIC,TFM)
C     ======================================
C1    VARTYPE              L      L    R(3,3)
C1    IOSTATUS             I      I      I
C
C2    Subroutine DRWBUF draws the entities currently referenced
C2    in the SWIND scratch file,transforming each by TFM.
C2    This can now detect cursor movements and stop the drawing process.
C2    It needs to know what is being done either DRAW or ERASE and 
C2    will only draw as meny enities as were actually drawn on the previous draw
C2  
C2  
C2    Arguments:-
C2  
C2    DRWMDE          ->          TRUE indicates DRAW FALSE indicates ERASE
C2    DYNAMIC         ->          TRUE indicates dymic move in progress
C2    TFM             ->          Transform the original data by this matrix.
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C  
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
      include 'include/lcmanager.inc'
      include 'include/daxcolor.inc'
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
C
      REAL R1
      REAL R2
      REAL X,Y
      REAL TFM(3,3)
      REAL M1(3,3)
      REAL M2(3,3)
C
      INTEGER*4 I
      INTEGER*4 PIXELS
      INTEGER*4 COUNT
      INTEGER*2 I21
      INTEGER*2 I22
      INTEGER*2 ENT
      INTEGER*2 TMIP
      INTEGER*2 COL2
      INTEGER*2 SCRPOS(2)
C
      LOGICAL DRWMDE
      LOGICAL DYNAMIC
      LOGICAL OK
      LOGICAL L0
      LOGICAL L1
      LOGICAL L2
      LOGICAL TOTAL
      LOGICAL DELETE
      LOGICAL MOVE
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
C
C     set up count to draw to 
      MOVE = .FALSE.
      IF ( DRWMDE ) THEN
          COUNT = NDATA
      ELSE
          COUNT = LMLDRW
      ENDIF
C
C     set interupt counter
      LMLDRW = 0
C
C     Read list and draw.
C
      DO 10 I=1,COUNT
          CALL RSCRF(I,TMIP,R1,R2,I21,I22)
          ENDCOD=I22
          IF (TMIP.GT.0) THEN
C             read the entity and draw it in position
              CALL ALLRD(TMIP,ENT,M1,DELETE)
              IF (ENT.EQ.COMPI .OR. ENT.EQ.SYMBI) THEN
C                 found an instance
C                 concatenate current position transform
C                 with instance transform
                  CALL MULT3M(M1,TFM,M2)
C                 draw the instance
                  CALL DRW066(M2)
              ELSE
C                 transform and draw the entity
C                 transform the entity
                  CALL ALLTFM(ENT,TFM)
C                 draw the transformed entity
                  IF ( COLPEN.EQ.COLERS) THEN
                       COL2 = COLERS
                       CALL PEN(COL2)
                  ELSE
                       COL2 = IMBUFF(4)
                       CALL PEN(COL2)
                  ENDIF
                  CALL SPCDRW(ENT,TMIP,.TRUE.,TFM,.TRUE.)
              END IF
          END IF
C
          IF ( DYNAMIC ) THEN
C             dynamic move in progress get events
              CALL LMINTR(PIXELS,OK)
C
C             Increment current number drawn
              LMLDRW = LMLDRW + 1
              IF ( OK.AND.DRWMDE ) THEN
C                 Interup locator is detected.
                  GOTO 20
              ENDIF
          ENDIF
C

 10   CONTINUE
C
 20   CONTINUE
C     recover opflags
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
      CALL ROPXOR()
C
C
      END
      SUBROUTINE DRAG(WX1,WY1,WX2,WY2,THETA,TFM)
C     ==========================================
C
C1    vartype          R   R   R   R    R  R(3,3)
C1    iostatus         I   I   O   O    O    O
C
C2    Subroutine DRAG supports dynamic movement of all the
C2    entities currently in the SWIND buffer.The final
C2    transform for the moved data is returned in TFM.
C2    WX1,WY1 contains the original reference point
C2    WX2,WY2 contains the updated  reference point
C2    THETA returns the total angle of rotation
C
      include 'include/gpr.ins.inc'
C
      include   'include/apollo.inc'
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/nbuff.inc'
      include   'include/movdat.inc'
      include   'include/swind.inc'
      include   'include/lcmanager.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 NEW,TEXT,ENTYPE,
     +          TENT,TMIP1
C
      INTEGER*2 SCRPOS(2),EVTYPE,I,ICHAR,DUMP(2),SAVEX,SAVEY
      INTEGER*2 SORC
      INTEGER*2 SPOS(2)
      INTEGER*2 CURCOL
      INTEGER*4 ST,KT,MOVUNT,TNDATA,DNUM,XC,YC,D1
      REAL      ZERO,DISTXY,X(1:3),Y(1:3),X1,Y1,
     +          LENGTH,CANG,ANG,WX1,WY1,WX2,WY2
      INTEGER*4 nlen
      REAL SNX,SNY
      REAL THETA,SX,SY,HX,HY
      REAL TTHETA,TSX,TSY,DSX,DSY,DTHETA
      REAL TFM(3,3),RM(3,3),TM(3,3),SM(3,3),M1(3,3)
      LOGICAL   UNOBSCURED,LWAIT,DRAW,STRETC
      CHARACTER EVDATA,HITKEY,TDATA,BUTTON,HITC
      CHARACTER BUFF*80
      EXTERNAL I3M,TRAN2D,SC2WO,SAVMOV,RSTMOV,UNFLAG,DCPRNT
      EXTERNAL ROPXOR,ROPREP
C
C     clear screen flags from entities
      CALL UNFLAG(.FALSE.)
C     initialize to identity matrix
      CALL I3M(TFM)
      CALL I3M(RM)
      CALL I3M(TM)
      CALL I3M(SM)
      THETA=0.0
      SX=1.0
      SY=1.0
      WX2=WX1
      WY2=WY1
      LMPENDING = .FALSE.
C     store screen postion of local origin
      CALL WO2SC(WX1,WY1,X1,Y1)
C
C     Undraw and set drawing into XOR
C
C
      CALL ROPREP()
      CALL PENERS()
      CALL DRWBUF1(.TRUE.,.FALSE.,TFM)
C
      CALL ROPXOR()
      CALL PENDRW()
      CALL DRWBUF1(.TRUE.,.FALSE.,TFM)
      CALL ROPXOR()
C
C
 111  CONTINUE
C
C     Time to acquire the display now
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
      SAVEX=INT(X1)
      SAVEY=INT(Y1)
C
      X(1)=WX1
      Y(1)=WY1
C     X(1),Y(1)now contains original point
C     wait for button to set mode of drag
C     reset the cursor size for drag mode.
C     set exclusive or raster op
      
      CALL ROPXOR()
      CRSIZE=40
      CALL SETAPN(COLOUR)
      CALL STCURP(REAL(SAVEX),REAL(SAVEY))
      CALL UPDACP(SAVEX,SAVEY)

 11   CONTINUE
C    "Select dynamic mode (buttons)"
      DNUM=118
      CALL DCPRNT(DNUM)

 10   CONTINUE
      LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
 12   CONTINUE
      IF (EVTYPE.EQ.GPR_$KEYSTROKE) THEN
C        test for quit command
         IF (EVDATA.EQ.'Q' .OR. EVDATA.EQ.'q') GOTO 99
         GOTO 10
      ELSE IF (EVTYPE.NE.GPR_$BUTTONS) THEN
C        discard all other events
         GOTO 10
      END IF
C     must have button event to get here
C     discard upstrokes
      IF (ICHAR(EVDATA).LT.97) GOTO 10
C     save button data for later
      BUTTON=EVDATA
C     now we have mode switch for dragging
      IF (BUTTON.EQ.'a') THEN
C        Translate mode
         DNUM=376
      ELSE IF (BUTTON.EQ.'b') THEN
C        Rotate mode
         DNUM=239
      ELSE
C        Stretch mode
         DNUM=269
      END IF
C     tell him which mode he is in
      CALL DCPRNT(DNUM)
      IF (BUTTON.EQ.'b' .OR. BUTTON.EQ.'c') THEN
C        rotate or stretch required,secon point needed
C        "Pick hook point"
         D1=173
         CALL DCPRNT(D1)
C        indicate local origin.
         CALL ROPXOR()
         CURCOL = COLOUR
         CALL SETAPN(CURCOL)
         CALL BCROSS(X1,Y1)
C        show him cursor
         CALL APCURS(HITC,SORC,XC,YC)
         IF (HITC.EQ.'Q' .OR. HITC.EQ.'q') GOTO 99
         SAVEX=XC
         SAVEY=YC
         CALL SC2WO(REAL(SAVEX),REAL(SAVEY),HX,HY)
      END IF
C
      LMLDRW = NDATA
C     ensure XOR mode in effect
      CALL ROPXOR()
      CURCOL = COLOUR
      CALL SETAPN(CURCOL)
      CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
C     now enter normal cycle
 222  CONTINUE
C     wait for input event
      IF ( LMPENDING ) THEN
C         Incomplete draw situation 
          EVTYPE = LMEVTYPE
          EVDATA = CHAR(LMBUTTON)
          SCRPOS(1) = LMPOS(1)
          SCRPOS(2) = LMPOS(2)
          LMPENDING = .FALSE.
      ELSE
C         wait for an event all entities on screen
          LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
C         set other interupt pending flag
          LMPENDING = .FALSE.
      ENDIF
C      WRITE(BUFF,'(I5,A,I5)') SCRPOS(1),' ',SCRPOS(2)
C      CALL PROMPTOUT(BUFF,NLEN(BUFF))
C
C     Main loop for drawing entities and moving.
C
1000  CONTINUE
C
      IF (EVTYPE.EQ.GPR_$LOCATOR) THEN
C         Update before redraw
          CALL ROPXOR()
          CURCOL = COLOUR
          CALL SETAPN(CURCOL)
          CALL UPDAPC(SCRPOS(1),SCRPOS(2))
C         erase data at current position
          CALL DRWBUF1(.FALSE.,.TRUE.,TFM)
          SCRPOS(1) = APCURX(1)
          SCRPOS(2) = APCURY(3)
C
          SPOS(1) = SCRPOS(1)
          SPOS(2) = SCRPOS(2)
C         test for valid movement
C         transform to world coords
          CALL SC2WO(REAL(SCRPOS(1)),REAL(SCRPOS(2)),X(2),Y(2))
          IF ( SETGRD ) THEN
              CALL SNAPXY(X(2),Y(2),SNX,SNY)
          ELSE
              SNX = X(2)
              SNY = Y(2)
          ENDIF
          X(2) = SNX
          Y(2) = SNY
          CALL WO2SC(X(2),Y(2),SNX,SNY)
          SCRPOS(1) = SNX
          SCRPOS(2) = SNY
          IF (BUTTON.EQ.'b') THEN
C             rotation required,find angle
              DTHETA=CANG(WX2,WY2,X(2),Y(2))-CANG(WX2,WY2,HX,HY)
              TTHETA=THETA+DTHETA
C             find rotation matrix
              CALL ROTP2D(WX1,WY1,TTHETA,RM)
          ELSE IF (BUTTON.EQ.'c') THEN
C             stretch function
          ELSE
C             translate only
              CALL TRAN2D(X(2)-X(1),Y(2)-Y(1),TM)
          END IF
C         find new transform
          CALL MULT3M(SM,RM,M1)
          CALL MULT3M(M1,TM,TFM)
C         redraw data in new position
          CALL DRWBUF1(.TRUE.,.TRUE.,TFM)
C         loop back and test cursor again
          GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$BUTTONS) THEN
C       ignore upstrokes
        IF (ICHAR(EVDATA).LT.97) GOTO 222
C       must have button down event
C       if same mode,terminate drag
C       any button stops current mode of drag
        IF (BUTTON.EQ.'c') THEN
C         update stretch scales
          SX=SX*DSX
          SY=SY*DSY
          CALL ROPXOR()
C         cancel local origin
          CURCOL = COLOUR
          CALL SETAPN(CURCOL)
          CALL BCROSS(X1,Y1)
C         set exclusive or raster op
          CALL ROPXOR()
        ELSE IF (BUTTON.EQ.'b') THEN
C         update rotation angle
          THETA=THETA+DTHETA
C         cancel local origin
          CALL ROPXOR()
          CURCOL = COLOUR
          CALL SETAPN(CURCOL)
          CALL BCROSS(X1,Y1)
C         set exclusive or raster op
          CALL ROPXOR()
        ELSE
C         update translated point
          WX2=X(2)
          WY2=Y(2)
          CALL WO2SC(WX2,WY2,X1,Y1)
C         set exclusive or raster op
          CALL ROPXOR()
        END IF
C       loop back and wait for next mode of drag
C       undraw the cursor
        CALL DRWBUF1(.FALSE.,.TRUE.,TFM)
        LMLDRW = NDATA
        CALL DRWBUF1(.FALSE.,.TRUE.,TFM)
C       Cursor is off
        CURCOL = COLOUR
        CALL SETAPN(CURCOL)
        CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
        IF (EVDATA.NE.BUTTON) THEN 
            GOTO 12
      ENDIF
      ELSE IF (EVTYPE.EQ.GPR_$KEYSTROKE) THEN
         IF (EVDATA.EQ.'Q' .OR. EVDATA.EQ.'q') THEN
             CALL ROPXOR()
             CALL SETAPN(COLOUR)
             CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
             GOTO 99
         ENDIF
         GOTO 222
      ELSE
         GOTO 222
      END IF
C
C     do the move on the data before return
C     save the move block
      CALL SAVMOV()
C     initialize the move flags
      STRETC=OPFLAG(11)
      CALL CLROPF()
      OPFLAG(11)=STRETC
C     initialize text scale factor to 1
      REFDAT(7,3)=1.0
C     initialize number of copies to a limit of 1
      IF ( NNCOPY.GT.1 ) NNCOPY=1
C     set the translate flag to true
      OPFLAG(4)=.TRUE.
C     calculate required movement
      REFDAT(4,1)=WX2-WX1
      REFDAT(4,2)=WY2-WY1
C     set rotate if required
      IF (THETA.NE.0.0) THEN
C        set the rotate flag to true
         OPFLAG(2)=.TRUE.
C        save the rotation origin
         REFDAT(2,1)=WX1
         REFDAT(2,2)=WY1
         REFDAT(2,3)=THETA
      END IF
C     save the number of entities in buffer
      TNDATA=NDATA
C     do the move
      CALL MOVMAS(.FALSE.)
C     retrieve the move block
      CALL RSTMOV()
C
C     tell him it is over
C     "Dynamic movement complete"
      DNUM=240
      CALL DCPRNT(DNUM)
C
C     reset the cursor size back to normal.
 999  CRSIZE=200
C     set draw raster op back to default (replace)
      CALL ROPREP()
      RETURN
 
 99   CONTINUE
      CALL ROPXOR()
C     redraw data in new position
      CALL DRWBUF1(.FALSE.,.FALSE.,TFM)
C     initialize to identity matrix
      CALL I3M(TFM)
      LMLDRW = NDATA
C     redraw data in new position
      CALL DRWBUF1(.FALSE.,.FALSE.,TFM)
 
      GOTO 999
      END
C
C
      SUBROUTINE DRAWT(L1,L2,L3,FONT)
C     =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
C
C1    vartype          D  D  D
C1    iostatus         I  I  I
      include 'include/wtov.inc'
      include 'include/gpr.ins.inc'
C
      DOUBLE PRECISION L1,L2,L3,X,Y,X1,Y1,X2,Y2,X3,Y3,M1,M2,M3
      INTEGER*4 FONT,ST
      INTEGER*2 IFONT
      LOGICAL OK
 
      X=WXMAX
      Y=WYMIN
      X3=WXMAX
      Y3=WYMAX
C
      CALL DCVL14(X,Y,X3,Y3,M1,M2,M3)
      CALL DVC0P5(L1,L2,L3,M1,M2,M3,X1,Y1,OK)
      IF ( .NOT. OK ) THEN
         X=WXMIN
         Y=WYMIN
         X3=WXMAX
         Y3=WYMIN
         CALL DCVL14(X,Y,X3,Y3,M1,M2,M3)
         CALL DVC0P5(L1,L2,L3,M1,M2,M3,X1,Y1,OK)
      END IF
 
      X=WXMIN
      Y=WYMIN
      X3=WXMIN
      Y3=WYMAX
      CALL DCVL14(X,Y,X3,Y3,M1,M2,M3)
      CALL DVC0P5(L1,L2,L3,M1,M2,M3,X2,Y2,OK)
      IF ( .NOT. OK ) THEN
         X=WXMIN
         Y=WYMAX
         X3=WXMAX
         Y3=WYMAX
         CALL DCVL14(X,Y,X3,Y3,M1,M2,M3)
         CALL DVC0P5(L1,L2,L3,M1,M2,M3,X2,Y2,OK)
      END IF
      IFONT=FONT
      CALL HARDWF(IFONT)
      CALL DRAWLW(REAL(X1),REAL(Y1),REAL(X2),REAL(Y2))
      IFONT=0
      CALL HARDWF(IFONT)
C
      END
 
C
 
      SUBROUTINE GRID1()
C     ==================
C
C1    no arguments required
C
      include 'include/wtov.inc'
      include 'include/ndata.inc'
      include 'include/vntable.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
      include 'include/vpgrid.inc'
      include 'include/menun.inc'
C
      REAL X,Y,XP,YP,SXG,SYG,GLIM,GMAX,
     +     XL,YL,SNAPG,RAD,SOFFX,HSYG,HSXG
      INTEGER*4 C,TC,TCELL,TMEN,I4
      INTEGER*4 ST
      CHARACTER*1 TOKEN
      EXTERNAL POINTS,WTOSVX,GETANS,SNAPG,GTCLRC,GTDMEN,RAD
      LOGICAL LOFFX
      LOGICAL OK
      INTEGER*2 VPN
C
      GLIM=8
      GMAX=300
      IF(GRTYPE .EQ. 2) THEN
C       set  screen size of isometric grid mesh
C       set isometric angle
        ISOANG = RAD(60.0)
C       get half the width of the iso diamond mesh
        HISOSX  = GRIDSX*SIN(ISOANG)
C       get half the height of the iso diamond mesh
        HISOSY  = GRIDSX*COS(ISOANG)
C       get the width and the height of the iso diamond mesh
        ISOSX   = 2 * HISOSX
        ISOSY   = 2 * HISOSY                   
C       set  screen size of isometric grid mesh
        CALL WTOSVX(ISOSX,SXG)
        CALL WTOSVY(ISOSY,SYG)
        HSXG = SXG/2.0
        HSYG = SYG/2.0
C       save the current grid configuration
        CALL SAVGRD(CVPN,OK)
      ELSE
C       set  screen size of cartesian grid mesh
        CALL WTOSVX(GRIDSX,SXG)
        CALL WTOSVY(GRIDSY,SYG)
      ENDIF

      TMEN=4
C
C    59     8   (U)     "Grid"
 
      IF ( SXG .LT. GLIM .OR. SYG .LT. GLIM ) THEN
C        grid is too small to display
         IF(.NOT.  ISGRID) THEN
C          only pump out error message if inserting grid
           I4=19
           CALL DEPRNT(I4)
           SETGRD = .FALSE.        
C          Right lets do this properly and solve any funnies
C          GRID1 will take the responsibilty to set VPGRID
C          to FALSE
           VPGRID(CVPN) = .FALSE.
           TMEN=4
C          unhi-lite the grid cell     
           I4=59
           IF(DISLAY) CALL GTDMCL(I4,TMEN)
         ENDIF
         RETURN
      ELSE IF ( SXG.GT.GMAX.AND.SYG.GT.GMAX ) THEN
C        grid is to large to display
         IF(.NOT.  ISGRID) THEN
C          only pump out error message if inserting grid
           I4=20
           CALL DEPRNT(I4)
           SETGRD = .FALSE.
C          Right lets do this properly and solve any funnies
C          GRID1 will take the responsibilty to set VPGRID
C          to FALSE
           VPGRID(CVPN) = .FALSE.
           TMEN=4
C          un-hilite the grid cell
           I4=59
           IF(DISLAY) CALL GTDMCL(I4,TMEN)
           RETURN
         ENDIF
      END IF
C  
C     *****************
C     DRAW THE GRID NOW
C     *****************
C
C     select the nesassary configuration for this viepwort
      CALL PORT_INQ(VPN,ST)
      CALL VPDRAW(VPN,.TRUE.,ST)
      IF(COLPEN.EQ.COLERS) THEN
          CALL SETAPN(COLERS)
      ELSE
         CALL SETAPN(COLFOR)
      ENDIF
C     Draw the grid now
      IF(GRTYPE .EQ. 2) THEN
C       geeza an isogrid
C       NB!! y snaps on  1/2 grid width
C       x on a full grid width
        X=SNAPG(WXMIN,ISOSX,GRIDOX)
        Y=SNAPG(WYMIN,HISOSY,GRIDOY)
        IF ( X .LT. WXMIN ) X=X+ISOSX
        IF ( Y .LT. WYMIN ) Y=Y+HISOSY
C       find out whether the grid starts
C       on the odd or even line
        IF(ABS(MOD((NINT((Y-GRIDOY)/HISOSY)),2)) .EQ. 1) THEN
C         odd so first grid line is shifted 1/2 a pitch
          LOFFX = .TRUE.
        ELSE
C         no need to shift
          LOFFX =.FALSE.
        ENDIF
        CALL WO2SC(X,Y,XL,YL)
C    

        DO 20 YP=YL,VYMIN,-HSYG
           IF(LOFFX) THEN
             SOFFX = HSXG
           ELSE
             SOFFX = 0.0
           ENDIF
           LOFFX = .NOT. LOFFX
           DO 10 XP=XL,VXMAX-SOFFX,SXG
              CALL POINTS(REAL(XP+SOFFX),YP)
 10        CONTINUE
 20     CONTINUE
      ELSE
C       geeza a cartesian grid
        X=SNAPG(WXMIN,GRIDSX,GRIDOX)
        Y=SNAPG(WYMIN,GRIDSY,GRIDOY)
        IF ( X .LT. WXMIN ) X=X+GRIDSX
        IF ( Y .LT. WYMIN ) Y=Y+GRIDSY
        CALL WO2SC(X,Y,XL,YL)
C
        DO 25 YP=YL,VYMIN,-SYG
           DO 15 XP=XL,VXMAX,SXG
              CALL POINTS(XP,YP)
 15        CONTINUE
 25     CONTINUE
      ENDIF
C
C     if any viewporting then reset everything back to main display
      CALL VPRES()
C
      END
*
      SUBROUTINE GTRLPN(GTSTRG,GTWID4)
C     ===============================
C1    vartype              C      I4
C1    iostatus             O      I
C2
C2    Formerly GTXT_READ_LINE reads keyboard input and
C2    echoes each character on the current line of window
C2    number GTWID4. The input line is terminated by CR,or
C2    ctrl 'M', at which time the text window is scrolled.
C2    The typed string is returned in 'GTSTRG'.
C
      include 'include/gpr.ins.inc'
      include 'include/gtxt1.inc'
C
      INTEGER*2 GTWID,L,LEN,NLEN2
      INTEGER*2 CP,LP,TL
      INTEGER*4 ST,GTWID4
      CHARACTER GTSTRG*(*),CHAR*1
C
      EXTERNAL GTSCRW,NLEN2
C
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C
      IF (GWA(GTWID,2)) THEN
C     Output window only,cannot read from it
          GTSTRG=GNLINE(1:L)
          RETURN
      END IF
C
C     Set screen cursor to input line
      CALL STCURP(REAL(GWC(GTWID,1)),REAL(GWC(GTWID,2)))
C
C     define the line delimiter
C     Go find a line of text input,and echo to screen
      CALL GTSRPN(GTSTRG,CHAR(13))
C
C     Find length of typed string
      L=NLEN2(GTSTRG)
C      get current cursor position on input line
      CP=GWC(GTWID,5)
C     get last useable cursor position
      LP=GWL(GTWID,5)
C     Calculate total length of the line
      TL=CP+L-1
      IF (TL.GT.LP) THEN
C      Set actual length of string for output
      L=L-(TL-LP)
      END IF
C
C     Place the typed line in the input line of the window
C      GWLINE(GTWID)(CP:)=GTSTRG(1:L)
C     scroll the input line into the output area
      CALL GTSCRW(GTWID4)
C
      END
C
      SUBROUTINE GTSRPN(LINSTR,LINTRM)
C     ================================
C1    vartype              C     C
C1    iostatus             O     I
C2
C2    reads keyboard input but does not echoes back
C2    at the current cursor position.The string typed
C2    is returned in 'LINSTR' and the input terminated by the
C2    character 'LINTRM'. the text cursor is left at the start
C2    of the next input line.
C
      include 'include/gpr.ins.inc'
C
      INTEGER*2 BMSIZ(1:2),CURPOS(2),CURROP(8),CURORG(2),TSTART(2)
      INTEGER*2 EVENT,FONTID,SCMODE,TEND,NLEN2
      INTEGER*4 DISpDE,ST,CURPAT,LCPNT,I,J
      LOGICAL UNOBSCURED,CURACT
      CHARACTER CH,JUNK,LINBUF*80
C
      CHARACTER LINSTR*(*),LINTRM*1
C
      EXTERNAL NLEN2,BCHAR
C
C     Clear buffer pointer
      LCPNT=1
C
      CALL BCHAR(LINBUF)
C
C     Grab the display first of all
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$SET_CURSOR_ACTIVE(.TRUE.,ST)
 300  CONTINUE
C     wait for keyboard input
      UNOBSCURED=GPR_$EVENT_WAIT(EVENT,CH,CURPOS,ST)
      IF (EVENT.EQ.GPR_$KEYSTROKE) THEN
        IF (CH.EQ.LINTRM .OR. ICHAR(CH) .EQ. 150) THEN
C         look for line delimiter
C         Go return line
          GOTO 200
        END IF
C
        IF ( CH .EQ. CHAR(149) .OR. CH .EQ. CHAR(8) ) THEN
C         User has hit backspace or Ctrl-H
C         Check if point is at one
          IF ( LCPNT .EQ. 1 ) GOTO 300
C         Set pointer back one
C         update the buffer pointer
          LCPNT=LCPNT-1
C         set CH to space
          CH=' '
C         save the character in the line buffer
          LINBUF(LCPNT:LCPNT)=CH
C         move cursor back  one char position
          CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
          CURPOS(1)=CURPOS(1)-TEND
          CALL GPR_$MOVE(CURPOS(1),CURPOS(2),ST)
C         CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
C         echo the character
          CALL GPR_$TEXT(CH,INT2(1),ST)
          CALL GPR_$INQ_TEXT_OFFSET(CH,INT2(1),TSTART,TEND,ST)
C         update current cursor position
          CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
          CALL GPR_$SET_CURSOR_ACTIVE(.TRUE.,ST)
        ELSE
C         save the character in the line buffer
          LINBUF(LCPNT:LCPNT)=CH
C         update the buffer pointer
          LCPNT=LCPNT+1
C         move cursor to next char position
          CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
          CALL GPR_$MOVE(CURPOS(1),CURPOS(2),ST)
C         CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
C         echo the character
C         CALL GPR_$TEXT(CH,INT2(1),ST)
          CALL GPR_$INQ_TEXT_OFFSET(CH,INT2(1),TSTART,TEND,ST)
          CURPOS(1)=CURPOS(1)+TEND
C         update current cursor position
          CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
          CALL GPR_$SET_CURSOR_ACTIVE(.TRUE.,ST)
        END IF
      END IF
C     go get next char
      GOTO 300
 200  CONTINUE
C     Got to tidy up the line before return
      CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
C
      LINSTR=LINBUF(1:NLEN2(LINBUF))
C     release the display before return to caller
      CALL GPR_$RELEASE_DISPLAY(ST)
      END
C
      SUBROUTINE HARDWF(II2)
C     ====================
C                      i2
C                      I
      include 'include/gpr.ins.inc'
      include 'include/fill_pattern.inc'
      include 'include/apollo.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 II2,SCALE
      INTEGER*2 CODE
      INTEGER*4 ST,BM_DESC
      INTEGER*4 COUNT
      INTEGER*4 I
      INTEGER*4 CURBIT
C
C     viewporting mod for DAXPORTS only
      SCALE = 1
      IF(.NOT.MAWS.AND.(VPADD.OR.VPMOV.OR.VPCAN)) THEN
          CALL GPR_$INQ_BITMAP(CURBIT,ST)
          CALL GPR_$SET_BITMAP(DISPDE,ST)
          IF ( II2.GT.0 ) THEN
             CODE = II2
             CALL GPR_$SET_LINESTYLE(GPR_$DOTTED,CODE,ST)
             CALL GPR_$SET_FILL_PATTERN (FILL_BITMAP,SCALE,ST)
          ELSE
             CODE = 25
             CALL GPR_$SET_FILL_PATTERN (gpr_$nil_bitmap_desc, SCALE,ST)
             CALL GPR_$SET_LINESTYLE(GPR_$SOLID,CODE,ST)
          END IF
C         reset bitmap
          CALL GPR_$SET_BITMAP(CURBIT,ST)
10    CONTINUE
C
      ENDIF
C
      IF ( II2.GT.0 ) THEN
          CODE = II2
          CALL GPR_$SET_LINESTYLE(GPR_$DOTTED,CODE,ST)
          CALL GPR_$SET_FILL_PATTERN (FILL_BITMAP,SCALE,ST)
      ELSE
          CODE = 25
          CALL GPR_$SET_FILL_PATTERN (gpr_$nil_bitmap_desc, SCALE,ST)
          CALL GPR_$SET_LINESTYLE(GPR_$SOLID,CODE,ST)
      END IF
 
C
C---------------------------------------------------------------
C
      END
      SUBROUTINE KBHOLD(KEY,OK)
C     =========================
C
C1    vartype           C*1 L
C1    iostatus           O  O
C
C2    Subroutine KBINTR tests for a keyboard interrupt
C2    and returns status OK true if interrupt occurred
C2    the key hit is then returned in KEY.
C
      include 'include/gpr.ins.inc'
C
      INTEGER*2 EVTYPE,SCPOSN(2)
      INTEGER*4 ST
      LOGICAL OK,LWAIT
      CHARACTER*1 KEY
C
C     test for KB input
      LWAIT=GPR_$EVENT_WAIT(EVTYPE,KEY,SCPOSN,ST)
      OK=(EVTYPE.EQ.GPR_$KEYSTROKE.AND.ST.EQ.0)
C
      END
C
C---------------------------------------------------------------
C
      SUBROUTINE KBINTR(KEY,OK)
C     =========================
C
C1    vartype           C*1 L
C1    iostatus           O  O
C
C2    Subroutine KBINTR tests for a keyboard interrupt
C2    and returns status OK true if interrupt occurred
C2    the key hit is then returned in KEY.
C
      include 'include/gpr.ins.inc'
C
      INTEGER*2 EVTYPE,SCPOSN(2)
      INTEGER*4 ST
      LOGICAL OK,LWAIT
      CHARACTER*1 KEY
C
C     test for KB input
      LWAIT=GPR_$COND_EVENT_WAIT(EVTYPE,KEY,SCPOSN,ST)
      OK=(EVTYPE.EQ.GPR_$KEYSTROKE.AND.ST.EQ.0)
C
      END

      SUBROUTINE LMINTR(PIXELS,OK)
C     ============================
C1    VARTYPE             I4   L
C1    IOSTATUS             I   O
C
C
C2    This routine will check the EVENT queues for LOCATOR events.
C2    If it detects them them then it will check the new position
C2    againts the current locator position. If the locator has moved but not
C2    sufficuently then OK will be false but the postion will be updated anyway
C2    to insure an absolute inrement.
C2  
C2    This routine must be used in conjuntion with :-
C2
C2    LMUPDT      ->      Update/set current locator position
C2  
C2  
C2    Arguments:-
C2  
C2  
C2    PIXELS      ->      The number of pixels that the locator must have to move
C2                        in order for the LCINTR to register an interupt.
C2
C2    OK          ->      TRUE indicates that a locator movement was pending
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK false means no locator detected.
C2  
C2  

      include 'include/lcmanager.inc'
      include 'include/style.inc'
      include 'include/gpr.ins.inc'
C
      INTEGER*4 ST
      INTEGER*4 PIXELS
      INTEGER*4 DIST
C
      INTEGER*2 EVTYPE
      INTEGER*2 CURCOL
      INTEGER*2 SCRPOS(2)
C
      LOGICAL OK
      LOGICAL LWAIT
C
      CHARACTER KEY*1
C
      OK = .FALSE.
C
C     Look at event buffer
      LWAIT = GPR_$COND_EVENT_WAIT(EVTYPE,KEY,SCRPOS,ST)
C
      IF ( EVTYPE.NE.GPR_$NO_EVENT ) THEN
C         set return flag
          OK = .TRUE.
          LMPENDING = .TRUE.
          LMPOS(1) = SCRPOS(1)
          LMPOS(2) = SCRPOS(2)
          LMBUTTON = ICHAR(KEY)
          LMEVTYPE = EVTYPE
          CURCOL = COLOUR
          CALL SETAPN(CURCOL)
          CALL UPDAPC(SCRPOS(1),SCRPOS(2))
      ELSE
          LMPENDING = .FALSE.
      ENDIF
      


C
      END
C
      SUBROUTINE SPTEXT(CURP,FILNM)
C     =============================
C
      include 'include/gpr.ins.inc'
 
 
      include 'include/vntable.inc'
      include 'include/dtext.inc'
      include 'include/library.inc'
      include 'include/product.inc'
C
      INTEGER*2 BMSIZ(1:2),CURPOS(2),CURP(2),SORG(2),TWLIMS(4)
      INTEGER*2 EVENT,FONTID,SCMODE,TEND,L,LEN,NLEN2,CURPOS_TMP
      INTEGER*4 DISPDE,ST,LCPNT,ICHAR,UNITN,DNUM
      LOGICAL UNOBSCURED,INV,OPTION,QUIT,OK,RESPON
      CHARACTER CH,INPT*100,FILNM*(*)
      EXTERNAL NLEN2,RESPON,NLEN
C
C     CURPOS(1)=CURP(1)+2
C     CURPOS(2)=CURP(2)+14
C
C     INPT=LIBRARY(1:NLEN(LIBRARY))//'/man.font'
C     CALL GPR_$LOAD_FONT_FILE(INPT,NLEN2(INPT),FONTID,ST)
C     CALL GPR_$SET_TEXT_FONT(FONTID,ST)
C     Grab the display first.
C     CALL GPR_$INQ_TEXT_EXTENT('M',1,SORG,ST)
C     UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
 
C     CALL FINDU2(UNITN,FILNM,OK)
C
C     IF ( .NOT. OK ) THEN
C        CALL EPRINT('Error finding file')
C        DNUM = 275
C        CALL DEPRNT(DNUM)
C        GOTO 700
C     END IF
C
C     OPEN(UNIT=UNITN,FILE=FILNM,ACCESS='SEQUENTIAL',
C    +        FORM='FORMATTED',STATUS='OLD',ERR=700)
C
C45   CONTINUE
C
C     READ(UNIT=UNITN,FMT='(A)',END=65) INPT
C
C     Save the x-position of the cursor
C     CURPOS_TMP=CURPOS(1)
C     CALL STCURP(REAL(CURPOS(1)),REAL(CURPOS(2)))
C
C     Print in normal video
C     Print the text to the screen at current position
C     CALL GPR_$TEXT(INPT,NLEN2(INPT),ST)
C
C     Go find where to place cursor now
C     update current cursor position to start of new line
C     CURPOS(1)=CURPOS_TMP
C      CURPOS(2)=CURPOS(2)+17
C     CURPOS(2)=CURPOS(2)+SORG(2)+2
C     CALL STCURP(REAL(CURPOS(1)),REAL(CURPOS(2)))
C
C     GOTO 45
C
C65   CONTINUE
C     CLOSE(UNIT=UNITN)
 
C700  CONTINUE
C     Release display before return to caller
C     CALL GPR_$RELEASE_DISPLAY(ST)
C
cD     WRITE(10,*) 'LANGID',LANGID
cD     IF ( LANGID.EQ.44 ) THEN
cD       INPT=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt.044'
cD     ELSE IF ( LANGID.EQ.49 ) THEN
cD       INPT=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt.049'
cD     ELSE IF ( LANGID.EQ.33 ) THEN
cD       INPT=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt.033'
cD     ENDIF
C     INPT=LIBRARY(1:NLEN(LIBRARY))//'/DICT/CRYPT.044'
C
C     WRITE(10,*)'Name',INPT
C     CALL GPR_$LOAD_FONT_FILE(INPT,NLEN2(INPT),FONTID,ST)
C     Activate text font
C     CALL GPR_$SET_TEXT_FONT(FONTID,ST)
 
      END
*
*
      SUBROUTINE WINDOW(FLAG)
C     ===================
C1      No arguments needed
C
C2    WINDOW returns in the commmon block SWIND the entities
C2    which are inside a window defined interactively by the user
C2    If the user defined window is not valid,then
C2    no entity selection is attempted,and the routine
C2    is exitted immediately.
C
C
      include 'include/macro.inc' 
      include 'include/journal.inc'
C
      INTEGER*4 I4
      REAL SXMIN,SYMIN,SXMAX,SYMAX,
     +     WXMIN,WYMIN,WXMAX,WYMAX,XDIF,YDIF
C
      LOGICAL OK,FLAG
C
      EXTERNAL SC2WO
      EXTERNAL SRCHWN,GETWIN,WRTJRN
C
      TCONT=(GINPUT.AND.GANS)
      IF(MACOP.AND..NOT.GINPUT) THEN
         CALL MAC101(WXMIN,WYMIN,OK)
         IF(.NOT.OK.OR.GINPUT) GOTO 20
         CALL MAC101(WXMAX,WYMAX,OK)
         IF(.NOT.OK.OR.GINPUT) GOTO 20
         IF(WXMIN.GT.WXMAX) THEN
            CALL RSWAP(WXMIN,WXMAX)
         END IF
         IF(WYMIN.GT.WYMAX) THEN
            CALL RSWAP(WYMIN,WYMAX)
         END IF
C       WRITE(10,*)'OUT OF MAC101 WITH CCMD,MEN,CELLN',CCMD,MEN,CELLN
         GOTO 44
      END IF
 20   CONTINUE
      IF(GINPUT) GINPUT = .FALSE.
C     go find the area of interest
      CALL GETWIN(SXMIN,SYMIN,SXMAX,SYMAX,OK)
C     something went wrong.
      IF (.NOT.OK) RETURN
C
C  ****************************************************************
C        Note this is set for origin top,left
C  ****************************************************************
      CALL SC2WO(SXMIN,SYMAX,WXMIN,WYMIN)
      CALL SC2WO(SXMAX,SYMIN,WXMAX,WYMAX)
C
C
C     limit minimum window to 6 pixels
      IF (ABS(SXMAX-SXMIN).LT.6 .OR. ABS(SYMAX-SYMIN).LT.6 ) THEN
         I4=21
         CALL DEPRNT(I4)
         GOTO 20
      END IF
 
 44   CONTINUE
C
      CALL SRCHWN(WXMIN,WYMIN,WXMAX,WYMAX,FLAG)
C     punt out the journaling data reqd
      IF(JOURON) THEN
        CALL WRTJRN(WXMIN, WYMIN, 'w','dummy',0 )
        CALL WRTJRN(WXMAX, WYMAX, 'w','dummy',0 )
      ENDIF

C
      END
*
      SUBROUTINE ZOOM(OK)
C     ===============
C1      No arguments needed
C
C2    subroutine ZOOM gets the defned area of the screen
C2    by way of a rubber banded rectangle,and performs
C2    a zoom into that area of interest to fill the
C2    screen.
C
      include 'include/wtov.inc'
      include 'include/journal.inc'
C
      CHARACTER*2 INPL,RUNFMT*80,BUFF*84
      REAL SXMIN,SYMIN,SXMAX,SYMAX,
     +     XMIN,YMIN,XMAX,YMAX,XDIF,YDIF
      INTEGER*4 PRNUM,DNUM,NLEN
      REAL PARS(9),XVAL
      LOGICAL RZERO
C
C
      LOGICAL OK
C
      EXTERNAL SC2WO,WORLD,ATGEND,OLDVPT,GETWIN,NLEN
C
      OK=.TRUE.
 10   CONTINUE
C     find the screen area of interest
      CALL GETWIN(SXMIN,SYMIN,SXMAX,SYMAX,OK)
C     abort this function if no valid area returned
      IF (.NOT.OK) RETURN
 
C     get the scale factor for viewing transorm
      CALL DECODM(VWXY,PARS)
 
      XVAL = MAX ( (SXMAX-SXMIN),(SYMAX-SYMIN) ) * PARS(4)
 
      IF (RZERO(XVAL) )  THEN
          DNUM = 496
          CALL DEPRNT(DNUM)
          OK=.FALSE.
          RETURN
      ENDIF
C
C*******************************************************************
C         Note this is set for origin top,left
C*******************************************************************
C
 
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C
      CALL SC2WO(SXMIN,SYMAX,XMIN,YMIN)
      CALL SC2WO(SXMAX,SYMIN,XMAX,YMAX)
C
      XDIF=XMAX-XMIN
      YDIF=YMAX-YMIN
C
C     limit minimum window to 6 pixels
      IF (ABS(SXMAX-SXMIN).LT.6.OR.ABS(SYMAX-SYMIN).LT.6 ) THEN
         PRNUM=21
         CALL DEPRNT(PRNUM)
         PRNUM=22
         CALL DCPRNT(PRNUM)
         GOTO 10
      END IF
C
C      WRITE(UNIT=10,FMT=*) XMIN,YMIN,XMAX,YMAX
C
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C
C     write out a journaling entry if necessary
      IF(JOURON) THEN
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A45)',ERR=99)
     +  '(A', NLEN(JRNCMD(6)),  
     1  ',''('',F14.6,'','',F14.6,'','',F14.6,'','',F14.6,'')'')'
        WRITE(UNIT=BUFF,FMT=RUNFMT,ERR=99) JRNCMD(6),XMIN,YMIN,XMAX,YMAX
        CALL WRTJRN(0.0,0.0,'m',BUFF,0)
      ENDIF
C
C      WRITE(UNIT=10,FMT=*) XMIN,YMIN,XMAX,YMAX
C
C      IF ( XMIN .EQ. WXMIN ) THEN
C         YMIN=WYMIN+(YDIF/2.0)-(WYMAX-YMAX)/2.0
C         YMAX=YMIN+(WYMAX-WYMIN)
C      ELSE
C         XMIN=WXMIN+(XDIF/2.0)-(WXMAX-XMAX)/2.0
C         XMAX=XMIN+(WXMAX-WXMIN)
C      END IF
C
C      WRITE(UNIT=10,FMT=*) XMIN,YMIN,XMAX,YMAX
C
C      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C
 99   CONTINUE
      END
C
