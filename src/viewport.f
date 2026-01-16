C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 viewport.f   */
C

      SUBROUTINE ALLOCATE_BITMAP(VPN,OK)
C     ==================================
C1    VARTYPE                     I2  L
C1    IOSTAT                      I   O
C
C2    This routine allocates bitmaps as required.
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
 
      INTEGER*4 ST
      INTEGER*2 VPN
      LOGICAL OK,UNOBSCURED
 
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      IF(VPN.EQ.0) THEN
          CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(ATTM,ST)
          CALL GPR_$ALLOCATE_BITMAP
     +        (BMSIZE,HIPLN,ATTM,MAINBM,ST)
      ELSE
          CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(ATTRB(VPN),ST)
          CALL GPR_$ALLOCATE_BITMAP
     +         (BMSIZE,HIPLN,ATTRB(VPN),VPBITM(VPN),ST)
      ENDIF
 
C     Set status return
      OK = ST.EQ.0
 
      CALL GPR_$RELEASE_DISPLAY(ST)
 
      END
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE BLTVPT()
C     ===================
C
C2    This routine will blt in and out the stored viewports as
C2    requested
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/macro.inc'
      include 'include/masti.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
 
      LOGICAL UNOBSCURED,OK
      INTEGER*4 ST,I,TMEN,TCELL
      INTEGER*2 OLDVP
      INTEGER*2 TVPN
C
C     set old port
      OLDVP = CVPN
C
C
      IF(CCMD.EQ.'a') THEN
          CVPN =1
      ELSEIF(CCMD.EQ.'b') THEN
          CVPN =2
      ELSEIF(CCMD.EQ.'c') THEN
          CVPN =3
      ELSEIF(CCMD.EQ.'d') THEN
          CVPN =4
      ELSEIF(CCMD.EQ.'e') THEN
          CVPN =5
      ENDIF
C
C     store the last command whi blitted
      LCOM = ICHAR(CCMD)
 
      IF(.NOT.MVPACT) THEN
          CVPN = OLDVP
          CALL DEPRNT(506)
          RETURN
      ELSE
          IF(CVPN.GT.0) THEN
              IF(.NOT.VPNUM(CVPN) )THEN
                  CALL DEPRNT(508)
                  CVPN = OLDVP
                  RETURN
              ENDIF
          ENDIF
          IF (CVPN.EQ.OLDVP) THEN
              CALL DEPRNT(507)
              RETURN
          ENDIF
      ENDIF
C
      CALL OLDVPT()
C
C     reset transform
      CALL WORLD( VIEWPS(1,CVPN+1),VIEWPS(2,CVPN+1),
     +            VIEWPS(3,CVPN+1),VIEWPS(4,CVPN+1) )
      TVPN = CVPN
C     get layers for this view
      CALL GETLAY(TVPN,OK)
C     sraw in the viewport details
      CALL GETGRD(TVPN,OK)
C     get this picture could be a bitmap or a redraw
      CALL GETPIC(TVPN,OK)
      PVPN = OLDVP
C     if layer menu active then change cell
      IF( .NOT.DISLAY ) THEN
            CCMD = 'C'
            CALL GTHFMC(4,CCMD,TCELL)
            TMEN = 4
            IF(TCELL.GT.0) CALL GTCLRC(TMEN,TCELL)
            I=CLAYER
            CALL GTDMWI(60,4,I)
            CALL GTCLRC(TMEN,TCELL-1)
            CALL GTPMEN(LNAME(CLAYER),'N',TMEN,TCELL-1)
      ENDIF
C
      END
C
C
C----------------------------------------------------------------------
C
      SUBROUTINE COPYVP(VP1,VP2)
C     ==========================
C
C1    VARTYPE           I2   I2
C1    IOSTAT            I    I
C
C2    this routine copyies the contents of one display file to another
C2    it copies VP1 to VP2
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/swind.inc'
      include   'include/viewport.inc'
      include   'include/wtov.inc'
C
 
      INTEGER*2 VP1,VP2,I,ETYP,MIPP,DDFP,TEMP
      LOGICAL OK
C
C     reset target viewport list
      LDFILE(VP2) = 1
      DO 10 I=1,LDFILE(VP1)-1
C
C         read source
          CALL RDISPF(I,ETYP,MIPP,OK)
C         add to target
          TEMP = CVPN
          CVPN = VP2
          CALL ADDISP(MIPP,ETYP,DDFP,OK)
          CVPN = TEMP
C
 10   CONTINUE
 
 
C
      END
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE CREATE_WINDOW_VIEW(VPN,OK)
C     ====================================
C1    VARTYPE                        I2 L
C1    IASTAT                         I  O
C
C2    This routine creates a view by taking a pass of the data base
C2    and drawing into the bitmap specified
C
C
      include 'include/gpr.ins.inc'
C
      include 'include/apollo.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
C
      REAL SX1,SX2,SY1,SY2
      REAL WX1,WX2,WY1,WY2
      REAL TXMAX,TXMIN,TYMAX,TYMIN
      INTEGER*4 ST
      INTEGER*2 TVPN,VPN,TMIP,DFPK,ENT,MIPP
      LOGICAL OK,UNOBSCURED
C
      OK =.FALSE.
C     get a window to draw into
      IF(VPREC) THEN
C         get exytents from drawing data
          WX1 = VIEWPS(1,VPN+1)
          WY1 = VIEWPS(2,VPN+1)
          WX2 = VIEWPS(3,VPN+1)
          WY2 = VIEWPS(4,VPN+1)
          OK=.TRUE.
      ELSE
C         Get it from a window
          CALL GETWIN(SX1,SY1,SX2,SY2,OK)
          IF(.NOT.OK) THEN
              CALL DEPRNT(504)
              RETURN
          ENDIF
      ENDIF
C
C
C     save current world coords
      TXMAX = WXMAX
      TYMAX = WYMAX
      TXMIN = WXMIN
      TYMIN = WYMIN
C
C     smallest first allready sorted
      IF(.NOT.VPREC) THEN
         CALL SC2WO(SX1,SY2,WX1,WY1)
         CALL SC2WO(SX2,SY1,WX2,WY2)
      ENDIF
      CALL WORLD(WX1,WY1,WX2,WY2)
C
      IF(.NOT.VPREC) THEN
C          save new world extents values
           CALL PORT_WORLD_SAVE(VPN,ST)
      ENDIF
C     save the current transform
      CALL TRNSAV(VPN)
C     Save the work layer here into the arrays 
      CALL SAVLAY(VPN,OK)
C     acquire display now
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C     system call make the bitmap current for the moment
      CALL GPR_$SET_BITMAP(VPBITM(VPN),ST)
C     pass thru database geting all necesary things
C
      CALL CLEAR()
C     draw paper extents
      CALL DISFRA()
C     test for no ents
C     Reset display file pointer to one
      DFPK = 1
      TMIP=0
      LDFILE(VPN) = 1
      IF(TVPN.EQ.-1) THEN
          CALL DEPRNT(503)
          OK =.FALSE.
          RETURN
      ENDIF
C     sett clip flag to use normal line drawing ops
      IF ( NMIPOS .EQ. 1 ) THEN
C         nothing to draw here
          GOTO 89
      ENDIF
C
C     save current viewport
      CALL PORT_INQ(TVPN,ST)
      CALL PORT_SET(VPN,ST)
      CALL PORT_CREATE_SET(.TRUE.,ST)
 10   TMIP=TMIP+1
C     read database
      CALL DIR500(TMIP,OK)
      IF ( VLAYER(IMBUFF(4)) ) THEN
C        check that it is not a deleted entity,
C        or a component master.
         IF (IMBUFF(2).EQ.COMPM .OR. IMBUFF(2).EQ.SYMBM.
     +       OR.IMBUFF(1).EQ.100) GOTO 11
         ENT=IMBUFF(2)
C        draw it to bitmap
         CALL ALLDRW(ENT,TMIP)
C        if visable then increment
C
         IF ( DISPV ) THEN
C             update the display file
              MIPP = TMIP
              CALL ADDISP(MIPP,ENT,DFPK,OK)
         ENDIF
 11      CONTINUE
      END IF
C     check for end
      IF ( TMIP .LT. (NMIPOS-1) ) THEN
          GOTO 10
      ENDIF
C
C     turn on grid if inherited
89    CONTINUE
      IF (SETGRD) THEN
          CALL GRID1()
      ENDIF
C
C     system call to reset the main bitmap
      CALL GPR_$SET_BITMAP( DISPDE , ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
C     reset current viewport
      CALL PORT_CREATE_SET(.FALSE.,ST)
      CALL PORT_SET(TVPN,ST)
C     return to main backcloth
      CALL WORLD (TXMIN,TYMIN,TXMAX,TYMAX)
      OK =.TRUE.
C
C
      END
C
C----------------------------------------------------------------------
C







 
      SUBROUTINE DEFVP1(VPN,WINDOW,OK)
C     ===============================
C1    VARTYPE            I2   L    L
C1    IOSTAST            I    I    O
C
C2    This routine will define a viewport by allocating space
C2    for a bitmap and saving the current view onto the new bitmap
C2    it also save the world coords
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/vntable.inc'
C
C
      CHARACTER*20 INPL
      REAL SX1,SY1,SX2,SY2,TXMAX,TYMAX,TXMIN,TYMIN
      REAL X1,Y1,X2,Y2
      INTEGER*2 VPN,MAIN,C,TVPN,VP1
      INTEGER*4 ST
      INTEGER*4 NLEN,I,ADDRES,DNUM
      LOGICAL OK,REDIF,YESOK,WINDOW,KILLV,MAKEOK
C
      EXTERNAL MAKEOK,YESOK
C
C     set view flags
      OK = .FALSE.
C     set up the pixel rectangles for BLT
C     if maws the we allready know the limits
      ORIGIN(1) = VXMIN
      ORIGIN(2) = VYMIN
      WINBLT(1) = VXMIN
      WINBLT(2) = VYMIN
      WINBLT(3) = (VXMAX-VXMIN)
      WINBLT(4) = (VYMAX-VYMIN)
      MAIN = 0
C
100   CONTINUE
C     test to find out if allready used
C
      REDIF = .FALSE.
      IF(VPN.LT.0) THEN
C         kill bitmap
          VPN = ABS(VPN)
C         No more bitmap
          CALL FREE_BITMAP(VPN,OK)
          IF(.NOT.OK) THEN
C            error trap but leave active 
             CALL PORT_RESET_NAME(VPN,ST)
             DNUM = 749
             CALL DEPRNT(DNUM)
             RETURN
          ENDIF
C
          IF(CVPN.GT.0) THEN
C             only do this if we are looking at a view
              CALL KILLVW(.TRUE.,.FALSE.)
C             clear the message banner
              CALL DRWVPD()
          ENDIF
C         turn off port
          CALL PORT_RESET_NAME(VPN,ST)
          CALL PORT_ACTIVE(VPN,.FALSE.,ST)
          CALL DCPRNT(539)
C         reset view port to 0
          VPN = 0
          CALL PORT_SET(VPN,ST)
C         save layering and grid conditions
          CALL SAVLAY(VPN,OK)
          CALL SAVGRD(VPN,OK)
          RETURN
      ENDIF
C
      IF(.NOT.VPREC) THEN
C         get a name unless saved in drawing
          CALL DPRMXP(505,INPL)
          IF(NLEN(INPL).EQ.0) THEN
C             reset name please
              CALL PORT_RESET_NAME(VPN,ST)
              IF(REDIF) THEN
                  CALL DCPRNT(540)
              ELSE
                  CALL DCPRNT(541)
              ENDIF
C             view is ok not to bother again
              OK = .TRUE.
              RETURN
          ELSE
C             Save name in vpname into verb noun if reading from drawing
              CALL PORT_SET_NAME(VPN,INPL,ST)
          ENDIF
      ELSE
C         Save name in vpname into verb noun if reading from drawing
          CALL PORT_SET_NAME(VPN,VPNAME(VPN),ST)
      ENDIF
C
C     allocate a bitmap for this view
      CALL ALLOCATE_BITMAP( VPN,OK )
      IF(.NOT.OK) THEN
C         bitmap allocation error
          CALL PORT_RESET_NAME(VPN,ST)
          CALL DEPRNT(503)
          GOTO 998
      ENDIF
C     set the current display and save it
      IF(WINDOW) THEN
C         activate the port now
          CALL PORT_ACTIVE(VPN,.TRUE.,ST)
C         create a window view
          CALL CREATE_WINDOW_VIEW ( VPN,OK)
          IF(.NOT.OK)  THEN
C             did not creatit for some reason
              OK = .FALSE.
              GOTO 999
          ENDIF
      ELSE
C         views display file will be set here
          VP1 = CVPN
          CALL COPYVP(VP1,VPN)
          IF(.NOT.OK) THEN
              CALL DEPRNT(503)
              GOTO 999
          ENDIF
C         acivate port
          CALL PORT_ACTIVE(VPN,.TRUE.,ST)
C         direct copy of screen 
          CALL SAVE_BITMAP(VPN,OK)
          IF(.NOT.OK) THEN
C             erro trapping here
              CALL DEPRNT(504)
              GOTO 999
          ENDIF
C         save current world transform
          CALL TRNSAV(VPN)
C         save current window extents as well
          VIEWPS(1,VPN+1) = WXMIN
          VIEWPS(2,VPN+1) = WYMIN
          VIEWPS(3,VPN+1) = WXMAX
          VIEWPS(4,VPN+1) = WYMAX
      ENDIF
C     save layering
      CALL SAVLAY(VPN,OK)
C     save grid settings
      CALL SAVGRD(VPN,OK)
C
C     IMPORTANT set this as current display cos that is wat it is
C
      VPN = 0
      CALL PORT_SET(VPN,ST)
C     copy the contents of the current display file into
C     the malloc one
      CALL PORT_OP_SET(0,ST)
C     save current worls transform also
      CALL TRNSAV(VPN)
      OK = .TRUE.
      RETURN 
999   CONTINUE
C     error condition reset port and name and bitmap
      CALL PORT_ACTIVE(VPN,.FALSE.,ST)
      CALL PORT_RESET_NAME(VPN,ST)
      CALL FREE_BITMAP(VPN,OK)
      OK = .FALSE.
      RETURN
998   CONTINUE
      OK = .FALSE.
      RETURN
C
      END
C
C----------------------------------------------------------------------
C

      SUBROUTINE DRWVPD()
C     ==========================
C
C2    This routine will mark this view with an identifier on
C2    the top right cornaer of the screen
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/wtov.inc'
      include 'include/vntable.inc'
      include 'include/masti.inc'
      include 'include/daxcolor.inc'
C
      LOGICAL UNOBSCURED
      INTEGER*4 ST,INDEX,NLEN,O,I,WUN,OPT
      INTEGER*4 FORE,BACK
      CHARACTER*80 STRING,NAME*20,WORK*20,TEMP
C
      REAL X,Y,D,DR
      INTEGER*2 CURPOS(2),START(2),TLEN
      INTEGER*2 IX,IY,II2,OX,OY,LENGTH,VPN
      INTEGER*2 IXX,IYX,IYM,IXM,WINDOW(4)
C
      IF(.NOT.MAWS) THEN
          CALL WRTBAN()
          RETURN
      ENDIF
C     get name for 'VIEW'
      NAME = VNOUN(496)
      WUN = 1
      IF(CVPN.EQ.0) THEN
          VPN = PVPN
      ELSE
          VPN = CVPN
      ENDIF
 
      LENGTH = NLEN(VPNAME(VPN))
      IF(LENGTH.EQ.0) THEN
          VPNAME(VPN) = 'Undefined'
      ENDIF
      LENGTH = LENGTH + NLEN(NAME)+1
      WORK = VNOUN(60)
      WRITE(UNIT=TEMP,FMT='(A,I3)') WORK(1:NLEN(WORK)),CLAYER
      LENGTH = LENGTH+NLEN(TEMP)+1+3
C
C
      CURPOS(2) = VYMIN+20
C
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$INQ_TEXT_OFFSET('M',1,START,TLEN,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
 
      CURPOS(1) = VXMAX -(9*LENGTH) -3
 
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$MOVE(CURPOS(1),CURPOS(2),ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
      CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
 
      IF(CVPN.EQ.0) THEN
          FORE = COLBAK
          BACK = COLFOR
          CALL TOOLPEN_TEXT(FORE,BACK,.TRUE.,ST)
      ENDIF
      STRING = NAME(1:NLEN(NAME))//' '//VPNAME(VPN)
     +         (1:NLEN(VPNAME(VPN)))//', '//TEMP(1:NLEN(TEMP))
      STRING(LENGTH+1:) = '                        '
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$TEXT(STRING,LENGTH,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
 
      CALL GPR_$SET_TEXT_PATH(GPR_$RIGHT,ST)
C     reset colour values
      FORE = COLFOR
      BACK = COLBAK
      CALL TOOLPEN_TEXT(FORE,BACK,.TRUE.,ST)
C
C
C     draw the details to the bit map and do
 
      END
C
      LOGICAL FUNCTION EQUIV(L1,L2)
C     =============================
C1    VARTPE                 L  L
C1    IOSTS                  I  I
C
C2    This routine will take 2 logicals and work out the
C2    equvalence truth table ie
C2
C2    0 0 1
C2    0 1 0
C2    1 0 0
C2    1 1 1
C
      LOGICAL L1,L2
      EQUIV = .FALSE.
C
      IF(L1.AND.L2.OR..NOT.(L1.AND.L2)) THEN
          EQUIV = .TRUE.
      ENDIF
C
      END
 
 
 
      SUBROUTINE FREE_ALL_BITMAPS()
C     =============================
C1
C2    This routine will free all allocated bitmaps
C2    it resets the atrributre block and bitmap desctripters only
C
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      INTEGER*2 VPN,I
      LOGICAL OK
C
      IF(.NOT.MAWS) THEN
C         only do this bitmap if working with DAXPORTS
          VPN = 0
          CALL FREE_BITMAP(VPN,OK)
C         set descripter back to 0
          ATTM = 0
          MAINBM = 0
      ENDIF
      DO 10 I=1,5
C         loop round to free othe bitmaps if required
          IF(VPNUM(I) ) THEN
C             free this bitmap either in 
              CALL FREE_BITMAP(I,OK)
          ENDIF
C         reset descripters
          VPBITM(I) = 0
          ATTRB(I) = 0
C 
 10   CONTINUE
      END
C
C----------------------------------------------------------------------
C
 
 
 
      SUBROUTINE FREE_BITMAP(VPN,OK)
C     ==================================
C1    VARTYPE                     I2  L
C1    IOSTAT                      I   O
C
C2    This routine DEallocates bitmaps as required.
C
 
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
 
      INTEGER*4 ST
      INTEGER*2 VPN
      LOGICAL OK,UNOBSCURED
 
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      IF(VPN.EQ.0) THEN
          CALL GPR_$DEALLOCATE_ATTRIBUTE_BLOCK(ATTM,ST)
          CALL GPR_$DEALLOCATE_BITMAP ( MAINBM,ST )
      ELSE
          CALL GPR_$DEALLOCATE_ATTRIBUTE_BLOCK (ATTRB(VPN),ST)
          CALL GPR_$DEALLOCATE_BITMAP (VPBITM(VPN),ST)
      ENDIF
 
C     Set status return
      OK = (ST.EQ.0)
 
      CALL GPR_$RELEASE_DISPLAY(ST)
 
      END
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE GET_BITMAP(VPN,OK)
C     =============================
C1    VARTYPE                 I2 L
C1    IOSTAT                   I O
C
C2    This routne will BLT the desird viewport to the cuurent bitmap
C2    Set the new display file and save the current world limits
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
 
 
      INTEGER*2 VPN
      INTEGER*4 ST
      LOGICAL UNOBSCURED,OK
 
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
 
      CALL GPR_$SET_BITMAP(DISPDE,ST)
 
      CALL GPR_$PIXEL_BLT(VPBITM(VPN),WINBLT,ORIGIN,ST)
 
      CALL GPR_$RELEASE_DISPLAY(ST)
C     reset transform
      RETURN
      END
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE GET_MAIN_BITMAP(OK)
C     =============================
C1
C1
C
C2    This routne will BLT the current bitmap to the desired vewport
C2    Set the new display file and save the current world limits
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
 
 
      INTEGER*4 ST
      LOGICAL UNOBSCURED,OK
 
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
 
      CALL GPR_$SET_BITMAP(DISPDE,ST)
 
      CALL GPR_$PIXEL_BLT(MAINBM,WINBLT,ORIGIN,ST)
      OK = ST.EQ.0
 
      CALL GPR_$RELEASE_DISPLAY(ST)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE GETGRD(VPN,OK)
C     =========================
C1    VARTYPE            I2  L
C1    VARTYPE            I   O
C
C2    This routine is saves all grid values
C2
C
      include 'include/viewport.inc'
      include 'include/vpgrid.inc'
      include 'include/ndata.inc'
      include 'include/menun.inc'
C
      LOGICAL OK
      INTEGER*2 VPN
C
      IF(VPN.LT.0.OR.VPN.GT.MAXVP) THEN
          OK = .FALSE.
          RETURN
      ENDIF
      SETGRD = VPGRID(VPN)
C
      GRIDOX = GRIDVP(1,VPN)
      GRIDOY = GRIDVP(2,VPN)
C
      GRIDSX = GRIDVP(3,VPN)
      GRIDSY = GRIDVP(4,VPN)
C
      GRTYPE = GRIDVP(5,VPN)
C     Iso section
C
      ISOANG  = GRIDIS(1,VPN)
      ISOSX   = GRIDIS(2,VPN)
      ISOSY   = GRIDIS(3,VPN)
      HISOSX  = GRIDIS(4,VPN)
      HISOSY  = GRIDIS(5,VPN)
C
      OK = .TRUE.
C
C     If we switch grids within the grid select menu then
C     we must update the menu values else we get confused
      IF (GRDMNU) THEN
          CALL VPUPGS()
      ENDIF
      IF(DISLAY.AND.DRAWNG) THEN
          IF ( SETGRD ) THEN
C             show grid on state in cell
              CALL GTDMCH(59,4)
          ELSE
C             show grid off state in cell
              CALL GTDMCL(59,4)
          ENDIF
      END IF
      END
 
 
 
 
      SUBROUTINE GETLAY(VPN,OK)
C     =========================
C1    VARTYPE            I2  L
C1    IOSTAT             I   O
C
C2    This routine will return the current layer list and attach to the
C2    viewport requested
C2    we assue that the current layer list has been saved.
C
      include   'include/masti.inc'
      include   'include/viewport.inc'
      include   'include/vnames.inc'
      include   'include/menun.inc'
      include   'include/layer.inc'
      include   'include/vpgrid.inc'
      include   'include/vntable.inc'
C
      CHARACTER*80  BUF
      INTEGER*4 I4,INDEX
      INTEGER*2 VPN,I,NUM
      LOGICAL OK                                               
      INTRINSIC INDEX

C     Keep the old layer so not to do any unnessary work
      INTEGER*2 OLDCLAY
      SAVE OLDCLAY

      DATA OLDCLAY /256/
                   
C
C     The current layer must be reset
      CLAYER=VPLAY(1,VPN)
C
C     Set all layers clear
      DO 20 I=0,255
         VLAYER(I) = .FALSE.
20    CONTINUE
C
C     reset all layers
      DO 10 I=3,VPLAY(2,VPN)
C
         VLAYER(VPLAY(I,VPN)) = .TRUE.
C
 10   CONTINUE    

      IF (CLAYER.NE.OLDCLAY) THEN
C
C         update the layer menu cell anyway please
C         if different
C              
C         Find the colon to add the layer in after it.
C         We have to resync on the colon
C         as if we don't then we will just append the layer onto it
          BUF = VNOUN(568)
          I4=INDEX(BUF,':')
C         protect agains no colon.
          IF (I4.EQ.0) I4=11

          WRITE(UNIT=VNOUN(568),FMT='(A,I3)',ERR=99) BUF(1:I4),CLAYER  

          OLDCLAY = CLAYER
                                                
 99       CONTINUE
      ENDIF

      END
C
      SUBROUTINE GETPIC(VPN,OK)
C     =========================
C1    VARTYPE            I2  L
C1    VARTYPE            I   O
C
C2    This routine is an interface onto the daxcad get bitmap routine
C2
C
      include 'include/viewport.inc'
C
      LOGICAL OK
      INTEGER*2 VPN
C
C
      OK = .TRUE.
C     do a range test
      IF(VPN.LT.0.OR.VPN.GT.MAXVP) THEN
          OK = .FALSE.
          RETURN
      ENDIF
C
      IF(MAWS) THEN
          CALL GET_BITMAP(VPN,OK)
          CALL DRWVPD()
      ELSE
          CALL WINDOW_PAINT(VPN)
      ENDIF
      END
 
 
C
      SUBROUTINE INITVP()
C     ===================
C
C2    Initialise the viewporting system
C2    warning this is a destructive system call
C
      include   'include/masti.inc'
      include   'include/viewport.inc'
      include   'include/vnames.inc'
      include   'include/vntable.inc'
      include   'include/wtov.inc'
C
      CHARACTER*20 VIEW
      INTEGER*4 I,J,ADDRES,NLEN
      LOGICAL OK
      INTEGER*2 TVPN,NUM
      REAL X,Y
      EXTERNAL NLEN
C
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set backcloth true first off all please
          X = 0
          Y = 0
          CALL SETDP(X,Y,.TRUE.)
      ENDIF
      VIEW = VNOUN(496)
C     viewports active
      IF(.NOT.MAWS.AND.MVPACT) THEN
          DO 40 NUM=1,MAXVP
              IF(VPNUM(NUM)) THEN
C                 replace mode for painting
                  CALL ROPREP()
C                 paint out all existing windows if on screen
                  CALL WINDOW_PAINT(-NUM)
              ENDIF
40        CONTINUE
      ELSEIF(MAWS.AND.MVPACT) THEN
C         any mays views to be reset
          IF(CVPN.GT.0) THEN
C             only do this if we are looking at a view (Inheritance flag obselete )
              CALL KILLVW(.TRUE.,.FALSE.)
C             clear the message banner
              CALL DRWVPD()
          ENDIF
      ENDIF
C     deallocate bitmaps only if system is in service
      IF(MVPACT) THEN
C         make sure all bitmaps are unactive
          CALL FREE_ALL_BITMAPS()
      ENDIF
C     ok lets go and reset everything
      VPUDEF = VNOUN(454)
      MVPACT = .FALSE.
      DO 10 I =1,MAXVP
         TVPN = I-1
C        make sure all viewport have been freed
C        viewport active
         VPNUM(I) = .FALSE.
C        last entity into viewport visibility
         VPVIS(I)= .FALSE.
         VPNAME(I) = VPUDEF
         ADDRES = 438+I
         VNOUN(ADDRES) = VIEW(1:NLEN(VIEW))//' '//VPUDEF
C        no grids 
         VPGRID(I) = .FALSE.
10    CONTINUE
C     Op types flag
      VP_OPTYPE = 0
      VIEWPORT_CREATE = .FALSE.
C     display file action flags
      VPADD= .FALSE.
      VPMOV= .FALSE.
      VPDEL= .FALSE.
      VPCAN=.FALSE.
C     viewport menu active
      VPDIS= .FALSE.
C     current viewport number
      CVPN = 0
C     previous viewport number
      PVPN = 0
C     recovery flag
      VPREC = .FALSE.
C     main view extentets
C     save current worls transform also
C     ***********************************
C     DAXPORTS ONLY
C     ***********************************
C     reset obscured list for viewports
      DO 20 I=1,MAXVP
          DO 30 J=1,MAXVP
          OBSLST(J,I) = .FALSE.
30    CONTINUE
20    CONTINUE
C     reset all display lists
      DO 50 I=0,MAXVP
          LDFILE(I) = 1
50    CONTINUE
C     Inialise the iconer by doing the nes
      CALL ICON_INIT()
C
      END
C
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE KILLVW(COPY,INHERIT)
C     ===============================
C1    VARYPE             L     L
C1    IOSTAT             I     I
C
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    COPY	->	Copies the display file from the current viewport to the new one ( eg a zoom )
C2    INHERIT	->	Used for layer and grid inheritance from the current view
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
C2    This routine will effectivly destroy a viewport
C2    It transfers control and display list back into the
C2    main display so that the display routine only need to
C2    read one viewport
C
      include   'include/viewport.inc'
      include   'include/wtov.inc'
      include   'include/ndata.inc'
C
      INTEGER*2 VP1
      INTEGER*2 VP2
      INTEGER*2 BACKCLOTH
      LOGICAL OK
      LOGICAL COPY
      LOGICAL EQUIV
      LOGICAL INHERIT
C
      EXTERNAL EQUIV
C
      BACKCLOTH = 0
C     copy current view to main one
C     this routine only appiles to MAWS
      IF(.NOT.MAWS) RETURN
      IF(COPY) THEN
          VP1 = CVPN
C         set target
          VP2 = 0
C         do the copy
          CALL COPYVP(VP1,VP2)
      ENDIF
C     set viewport controls
      PVPN = CVPN
      CVPN = 0
C     set extents of current drawing
      VIEWPS(1,1) = WXMIN
      VIEWPS(2,1) = WYMIN
      VIEWPS(3,1) = WXMAX
      VIEWPS(4,1) = WYMAX
C
C     save contents of layer if they have changed
      CALL SAVLAY(PVPN,OK)
      IF (INHERIT ) THEN
C         Inherit layer and grid conditions from current viewport
          CALL SAVLAY(BACKCLOTH,OK)
          CALL SAVGRD(BACKCLOTH,OK)
      ELSE
C         copy contents of layering into main list
          CALL GETLAY(BACKCLOTH,OK)
C         get grid sizes
          CALL GETGRD(BACKCLOTH,OK)
      ENDIF
      END
C
C----------------------------------------------------------------------
C
 
      SUBROUTINE SAVE_BITMAP(VPN,OK)
C     ==============================
C1    VARTYPE                 I2 L
C1    IOSTAT                   I O
C
C2    This routne will BLT the current bitmap to the desired vewport
C2    Set the new display file and save the current world limits
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
C
C
      INTEGER*2 VPN
      INTEGER*4 ST
      LOGICAL UNOBSCURED,OK
C
C
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      IF(VPN.EQ.0) THEN
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
          CALL GPR_$SET_BITMAP(MAINBM,ST)
      ELSE
C
          CALL GPR_$SET_BITMAP(VPBITM(VPN),ST)
C
      ENDIF
C
C     ensure replace mode is used here
      CALL ROPREP()
      CALL GPR_$PIXEL_BLT(DISPDE,WINBLT,ORIGIN,ST)
      OK = ST.EQ.0
C     reset display bitmap
      CALL GPR_$SET_BITMAP(DISPDE,ST)
C
      CALL GPR_$RELEASE_DISPLAY(ST)
      RETURN
      END
C
C----------------------------------------------------------------------
C
 
C----------------------------------------------------------------------
C
C
 
      SUBROUTINE SAVGRD(VPN,OK)
C     ==========================
C1    VARTYPE            I2  L
C1    VARTYPE            I   O
C
C2    This routine is saves all grid values
C
      include 'include/viewport.inc'
      include 'include/vpgrid.inc'
      include 'include/ndata.inc'
C
      LOGICAL OK
      INTEGER*2 VPN
C
      IF(VPN.LT.0.OR.VPN.GT.MAXVP) THEN
          OK = .FALSE.
          RETURN
      ENDIF
      VPGRID(VPN) = SETGRD
C
      GRIDVP(1,VPN) = GRIDOX
      GRIDVP(2,VPN) = GRIDOY
C
      GRIDVP(3,VPN) = GRIDSX
      GRIDVP(4,VPN) = GRIDSY
C
      GRIDVP(5,VPN) = GRTYPE
C
      GRIDIS(1,VPN) = ISOANG
      GRIDIS(2,VPN) = ISOSX 
      GRIDIS(3,VPN) = ISOSY 
      GRIDIS(4,VPN) = HISOSX
      GRIDIS(5,VPN) = HISOSY
C
      OK = .TRUE.
C
      END
 
 
 
      SUBROUTINE SAVLAY(VPN,OK)
C     ========================
C1    VARTYPE            I2  L
C1    IOSTAT             I   O
C
C2    This routine will save the current layer list and attach to the
C2    viewport requested
 
      include   'include/masti.inc'
      include   'include/viewport.inc'
      include   'include/vnames.inc'
C
      INTEGER*2 VPN,I,NUM
      LOGICAL OK
C
      NUM =2
      VPLAY(1,VPN)=CLAYER
      DO 10 I=0,255
C
         IF(VLAYER(I)) THEN
C
            NUM = NUM+1
            VPLAY(NUM,VPN)=I
C
         ENDIF
 10   CONTINUE
C
      VPLAY(2,VPN) = NUM
C
      END
C
C
C----------------------------------------------------------------------
C
      SUBROUTINE UPDBTM(NUM)
C     ======================
C
C2    This routine will be called from SPCDDRW and
C2    will return a value for the number of loops to do
C2    until all veiwports have been upadated
C2    CODING for NUM is as follows
C2    0 normal display screen as curent viewport
C2    1 backcloth for DAXPORTS updates it as an extra viewport
C2    2-MAXVP+1 set the viewprt transform for screen and hidden
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
      include 'include/icon.inc'
C
      REAL TXMAX,TYMAX,TXMIN,TYMIN
      INTEGER*2 NUM,VPN,TVPN
      INTEGER*2 TEMPI2
      INTEGER*2 TMPVP
      INTEGER*4 ST
      LOGICAL OK
C
C     Initalise variables
      VPOX = 0
      VPOY = 0
      VPSEGF = .FALSE.
      IF(NUM.GT.1) VPVIS(NUM-1) = .FALSE.
      IF (.NOT.VPADD.AND..NOT.VPDEL.AND..NOT.VPMOV.AND..NOT.VPCAN) THEN
          NUM = -1
          RETURN
      ENDIF

C     in a view
      IF(NUM.EQ.1) THEN
C         update the swapper always in viewports
          IF(CVPN.GT.0.OR..NOT.MAWS) THEN
C             if viewports then reset worlds also
 
              IF(.NOT.MAWS) THEN
                  VXMAX = VPXMAX
                  VYMAX = VPYMAX
                  VXMIN = VPXMIN
                  VYMIN = VPYMIN
C                 segment manager flags
                  DDCODE = 1
                  VPSEGN = 0
                  VPOX = 0
                  VPOY = 0
                  TEMPI2 = 0
                  CALL TRNRST(TEMPI2)
C                 do a swapping bitmap test
                  CALL GPR_$SET_BITMAP(MAINBM,ST)
C                 set color to parent display
                  CALL VPCOL(ST)
                  TVPN = 0
                  CALL GETLAY(TVPN,OK)
              ENDIF
C
          ENDIF
C
C     update all  other views
      ELSEIF(NUM.GT.1) THEN
          IF(VPNUM(NUM-1)) THEN
              IF(.NOT.MAWS) THEN
C
                  VXMIN = VIEWEX(1,NUM-1)
                  VYMIN = VIEWEX(2,NUM-1)+VPBANN
                  VXMAX = VIEWEX(3,NUM-1)
                  VYMAX = VIEWEX(4,NUM-1)
C                 segment manager flags
                  DDCODE = 1
                  VPSEGN = NUM-1
C                 icon offset not valid yet please
                  IF(ICNUM(VPSEGN)) THEN
                      VPOX = ICONBF(1,VPSEGN)
                      VPOY = ICONBF(2,VPSEGN)
                  ELSE
                      VPOX = VIEWEX(1,VPSEGN)
                      VPOY = VIEWEX(2,VPSEGN)
                  ENDIF
C
              ENDIF
C             recovver worls transform
              TEMPI2 = NUM - 1
              CALL TRNRST( TEMPI2 )
              CALL GPR_$SET_BITMAP(VPBITM(NUM-1),ST)
C             set color to parent display
              CALL VPCOL(ST)
              TVPN = NUM -1
              CALL GETLAY(TVPN,OK)
          ENDIF
C
C     time to set main display only in a maws view
      ELSE IF(NUM.EQ.0) THEN
C
          CALL GPR_$SET_BITMAP(DISPDE,ST)
C
          IF ( NUM.GT.0) THEN
              TVPN = NUM-1
C             get layer conditions
              TMPVP =  CVPN
              CALL GETLAY ( TMPVP,OK )
          ENDIF
C
          IF(CVPN.GT.0) THEN
C             reset to current viewport dimensions
              IF(.NOT.MAWS) THEN
                  VXMIN = VIEWEX(1,CVPN)
                  VYMIN = VIEWEX(2,CVPN)+VPBANN
                  VXMAX = VIEWEX(3,CVPN)
                  VYMAX = VIEWEX(4,CVPN)
              ENDIF
          ENDIF
          CALL TRNRST(CVPN)
          TVPN = CVPN
          CALL GETLAY(TVPN,OK)
      ENDIF
      NUM = NUM -1
C     set up for a thick line draw
C
      END
C
C
C
C----------------------------------------------------------------------
C
      SUBROUTINE UPDDF(MIPP,ENT,NUM,VISABL)
C     =====================================
C1    VARTYPE            I2  I2   I2   L
C1    IOSTAT             I   I     I   I
C
C2    This routine will add an entity if visible to the current viewport
C2    if visible
 
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
      include 'include/masti.inc'
      include 'include/style.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 TMIP,NUM,MIPP,ENT,TVPN,DDFP,TEMP
      INTEGER*4 ST
      LOGICAL VISABL,OK
C
      IF (.NOT.VPADD.AND..NOT.VPDEL.AND..NOT.VPMOV.AND..NOT.VPCAN) THEN
          RETURN
      ENDIF
C     set number visibility
 
      IF(NUM.GT.0) THEN
C         we will use this on the components
C         NUM allready maps to the viewport number
C         manage the current display files
C         add the entity if control flag and visible
          VPVIS(NUM)= VISABL
          IF( (VPADD.OR.VPMOV).AND.VISABL.AND.IMBUFF(1).EQ.STATUS.OR
     +          .IMBUFF(1).EQ.GROUP) THEN
C
              IF(VPMOV.AND..NOT.VPADD) THEN
C                 check current display list only if moving.
                  CALL VPMOVOK(MIPP,ENT,NUM,OK)
              ELSEIF(NUM.NE.CVPN) THEN
C             only add this entity if not allready in create routines
                  TEMP = CVPN
                  CVPN = NUM
                  CALL ADDISP(MIPP,ENT,DDFP,OK)
                  CVPN = TEMP
              ENDIF
C             increment the current disdplay file pointer if working in view
          ELSE IF (VPCAN.AND.VISABL) THEN
C             cancel this entity from the list
              IF(NUM.NE.CVPN) THEN
C                 only cancel if not current viewport
                  LDFILE(NUM) = LDFILE(NUM) - 1
              ENDIF
          ENDIF
C
      ELSEIF(NUM.EQ.0.AND..NOT.MAWS) THEN
C         only update main display if using viewports
C
          VPVIS(NUM)= VISABL
          IF( (VPADD.OR.VPMOV).AND.VISABL.AND.IMBUFF(1).EQ.STATUS.OR
     +          .IMBUFF(1).EQ.GROUP) THEN
C
              IF(VPMOV.AND..NOT.VPADD) THEN
C                 check current display list only if moving.
                  CALL VPMOVOK(MIPP,ENT,NUM,OK)
              ELSEIF(NUM.NE.CVPN) THEN
C             only add this entity if not allready in create routines
                  TEMP = CVPN
                  CVPN = NUM
                  CALL ADDISP(MIPP,ENT,DDFP,OK)
                  CVPN = TEMP
              ENDIF
C             increment the current disdplay file pointer if working in view
          ELSE IF (VPCAN.AND.VISABL) THEN
C             cancel this entity from the list
              IF(NUM.NE.CVPN) THEN
C                 only cancel if not current viewport
                  LDFILE(NUM) = LDFILE(NUM) - 1
              ENDIF
          ENDIF
      ENDIF
C     make sure clipping has been turned off for this bitmap
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
 
      END
C
 
 
 
 
 
C
C======================================================================
C
C
 
 
 
 
 
      SUBROUTINE VIEW_BUILD(OK)
C     =======================
C1    NO ARGUMENTS YET
C
      include   'include/masti.inc'
      include   'include/viewport.inc'
      include   'include/vnames.inc'
      include   'include/params.inc'
      include   'include/filunit.inc'
      include   'include/faults.inc'
      include   'include/icon.inc'
      include   'include/vpgrid.inc'

C
      INTEGER*2 I,J,NUM
      LOGICAL OK,YESOK
      CHARACTER*80 INPL
 
C
      EXTERNAL YESOK
C
      NUM = 0
      IF(DAXCAD_FAULT_OCCURED) RETURN
      IF(.NOT.MVPACT) RETURN
C
      IF (MAWS) THEN
C        Prompt user for maws 
         CALL DPRMXP(510,INPL)
      ELSE
C        Prompt user for daxports 
         CALL DPRMXP(736,INPL)
      ENDIF

      OK = YESOK(INPL)
      IF(.NOT.OK) THEN
C        No data must be saved
         WRITE(UNIT=PARFUN,ERR=99) 'DAXPORTS 0'
         RETURN
      ELSEIF(MAWS) THEN
C        data will follow
         WRITE(UNIT=PARFUN,ERR=99) 'DAXPORTS 3'
      ELSE
C        data will follow
         WRITE(UNIT=PARFUN,ERR=99) 'DAXPORTS 4'
      ENDIF
C
C
C     get number of active daxports
      DO 25 I=1,MAXVP
         IF(VPNUM(I)) NUM = NUM+1
 25   CONTINUE
C
      WRITE(UNIT=PARFUN,ERR=99) NUM
      DO 26 I=1,MAXVP
         IF(VPNUM(I)) THEN
            WRITE(UNIT=PARFUN,ERR=99) I
          ENDIF
 26   CONTINUE

C      WRITE(UNIT=10,ERR=99,FMT=*) NUM
C     save the viewport number and its extents
      DO 10 I=1,MAXVP
         IF ( VPNUM(I)) THEN
C
C           write out info about view number.
            WRITE(UNIT=PARFUN,ERR=99) I,(VIEWPS(J,I+1),J=1,4)
C            WRITE(UNIT=10,ERR=99,FMT=*) I,(VIEWPS(J,I+1),J=1,4)
C
         ENDIF
C
 10   CONTINUE
C
      IF(.NOT.MAWS) THEN
          DO 15 I=1,MAXVP
              IF(VPNUM(I) )  THEN
                  IF(ICNUM(I)) THEN
C                     recover from iconer
                      DO 11 J=1,4
                          VIEWEX(J,I) = ICONBF(J,I)
 11                   CONTINUE
                  ENDIF
C                 write it out
                  WRITE(UNIT=PARFUN,ERR=99) I,(VIEWEX(J,I),J=1,4)
              ENDIF
 15       CONTINUE
      ENDIF
 
C     save layer list control
      DO 20 I=1,MAXVP
         IF ( VPNUM(I)) THEN
C
C           get number of layers attached to the view
            NUM = VPLAY(2,I)
C           write out info about view number.
            WRITE(UNIT=PARFUN,ERR=99) VPLAY(1,I),VPLAY(2,I)
            WRITE(UNIT=PARFUN,ERR=99) (VPLAY(J,I),J=3,NUM)
C            WRITE(UNIT=10,ERR=99,FMT=*) VPLAY(1,I),VPLAY(2,I),
C     +                 (VPLAY(J,I),J=3,NUM)
         ENDIF
C
 20   CONTINUE
C
C     Save all the view names
      DO 30 I=1,MAXVP
         IF ( VPNUM(I)) THEN
C
C           write out view-name
            WRITE(UNIT=PARFUN,ERR=99) VPNAME(I)
C            WRITE(UNIT=10,ERR=99,FMT=*) VPNAME(I)
 
         ENDIF
C
 30   CONTINUE
C
C
C     Save grid control info for either maws or viewports
      DO 17 I=1,MAXVP
C         Only write out if viewport was enabled
          IF (VPNUM(I)) THEN
C             Write Viewport grid enable flag
              WRITE(UNIT=PARFUN,ERR=99) I,(VPGRID(I))
C             Now write grid type and limits etc 
              WRITE(UNIT=PARFUN,ERR=99) I,(GRIDVP(J,I),J=1,5)
C             Now write out the grid isometric info
              WRITE(UNIT=PARFUN,ERR=99) I,(GRIDIS(J,I),J=1,5)
          ENDIF
 17   CONTINUE

      OK = .TRUE.
      RETURN
C
C     error channel
 99   CONTINUE
      CALL DEPRNT(516)
      OK = .FALSE.
C
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE VIEW_REBUILD(OK)
C     ===========================
C1    NO ARGUMENTS YET
C
C2    This routine will rebuild a view system.
C
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/viewport.inc'
      include   'include/vnames.inc'
      include   'include/params.inc'
      include   'include/filunit.inc'
      include   'include/vntable.inc'
      include   'include/vpgrid.inc'
C
      REAL TPS(4)
      INTEGER*4 NLEN
      INTEGER*4 TPSI(4)
      INTEGER*2 VI(MAXVP)
      INTEGER*2 I,J,NUM,N,NN
      LOGICAL OK,YESOK,WINDOW
      CHARACTER*80 INPL,IDENT*10
      INTEGER*4 DPREV
C
      EXTERNAL NLEN
C
      EXTERNAL YESOK
C
      DPREV = 0
      NUM = 0
C     get daxport identifier
      READ(UNIT=PARFUN,ERR=99,END=98) IDENT
C     set revision
      READ(IDENT(10:),FMT='(I2)',ERR=99) DPREV
      IF(IDENT.EQ.'DAXPORTS 1'.OR.IDENT.EQ.'DAXPORTS 3') THEN
C         views do exist offer user choice
          CALL CPRINT(' ')
C         tell him that MAWS views are present
          CALL BELL()
          CALL DPRMXP(511,INPL)
          OK = YESOK(INPL)
          MAWS = .TRUE.
      ELSEIF(IDENT.EQ.'DAXPORTS 2'.OR.IDENT.EQ.'DAXPORTS 4') THEN
C         views do exist offer user choice
          CALL CPRINT(' ')
C         tell him that DAXPORTS are present
          CALL BELL()
          CALL DPRMXP(737,INPL)
          OK = YESOK(INPL)
          MAWS = .FALSE.
C         set revision
      ELSE
         OK=.FALSE.
      ENDIF
C
      IF(.NOT.OK) RETURN
C
C     set up indexes as 0
      DO 7 I=1,MAXVP
          VI(I) = 0
 7    CONTINUE
C     get number of views active all revisions
      READ(UNIT=PARFUN,ERR=99) NUM
C
C     check for new versions
      IF(DPREV.EQ.3.OR.DPREV.EQ.4) THEN
          DO 9 I=1,NUM
C             viewport index
              READ(UNIT=PARFUN,ERR=99) VI(I)
 9        CONTINUE
      ELSE
C         set up index table please for all 
          DO 8 I=1,NUM
              VI(I) = I
 8        CONTINUE
      ENDIF
C
      DO 10 N=1,NUM
C        read in extents info.
         READ(UNIT=PARFUN,ERR=99) I,(TPS(J),J=1,4)
         DO 12 I=1,4
C           put buffer into extents array
            VIEWPS(I,VI(N)+1)=TPS(I)
 12      CONTINUE
C
C
 10   CONTINUE
C
      IF(.NOT.MAWS) THEN
C         read in viewport information only in this drawing
          DO 15 N=1,NUM
C             read in extents info.
              READ(UNIT=PARFUN,ERR=99) I,(TPSI(J),J=1,4)
              DO 16 I=1,4
C                put buffer into extents array
                 VIEWEX(I,VI(N))=TPSI(I)
C                make sure values are set here as modified
 16           CONTINUE
              VIEWPO(1,VI(N)) = VIEWEX(3,VI(N))-VIEWEX(1,VI(N))
              VIEWPO(2,VI(N)) = VIEWEX(4,VI(N))-VIEWEX(2,VI(N))
C
C
 15       CONTINUE
      ENDIF
C     recover layer list control
      DO 20 I=1,NUM
C
C           get info about current layer and number of attached
          READ(UNIT=PARFUN,ERR=99) VPLAY(1,VI(I)),VPLAY(2,VI(I))
 
C         get number of layers attached to the view
          NN = VPLAY(2,VI(I))
          READ(UNIT=PARFUN,ERR=99) (VPLAY(J,VI(I)),J=3,NN)
C
 20   CONTINUE
C
C     Save all the view names
      DO 30 I=1,NUM
C
C         write out view-name
          READ(UNIT=PARFUN,ERR=99) VPNAME(VI(I))
C
 30   CONTINUE

C     Read in grid info for either maws or viewports if present
      DO 17 N=1,NUM
C        Read Viewport enable flag                             
C        If end of file no grid stuff so jump to put in defaults
         READ(UNIT=PARFUN,END=97,ERR=99) I,VPGRID(VI(N))
C        Now Read grid type and limits etc 
         READ(UNIT=PARFUN,ERR=99) I,(GRIDVP(J,VI(N)),J=1,5)
C        Now Read out the grid isometric info
         READ(UNIT=PARFUN,ERR=99) I,(GRIDIS(J,VI(N)),J=1,5)
 17   CONTINUE
C     Jump over the extra bit 
      GOTO 19         

 97   CONTINUE    
C     Old version less than 2.56 so save current grid info
C     to give viewport something
      DO 18 N=1,NUM             
          CALL SAVGRD(VI(N),OK)
C         Ensure turned off in case main one is on
          VPGRID(VI(N)) = .FALSE.
 18   CONTINUE

 19   CONTINUE

C     information is read lets go home
      IF(.NOT.OK) RETURN
C     Ok lets do the clever bit and rebuild all bitmaps
      VPREC = .TRUE.
      DO 40 I=1,NUM
          IF(.NOT.MAWS) THEN
          WRITE(INPL,FMT='(2A,I2)') DICT01(738)(1:NLEN(DICT01(738))),
     +                         ' ',VI(I)
          ELSE
          WRITE(INPL,FMT='(3A)') DICT01(515)(1:NLEN(DICT01(515))),
     +                         ' ',VPNAME(VI(I))
          ENDIF
          CALL CPRINT(INPL)
          WINDOW = .TRUE.
C         set new layering conditions
          CALL GETLAY(VI(I),OK)
C         set new grid conditions
          CALL GETGRD(VI(I),OK)      
C         Ensure dummy command for ccmd
          CCMD = ' '
          IF(MAWS) THEN
              CALL DEFVP1(VI(I),WINDOW,OK)
          ELSE
C             make sure windows all fit
              CALL WINDOW_MODIFY(VI(I),OK)
C             set the viepwort we want to build
              CVPN = VI(I)
              CALL DEFWIN(OK)
          ENDIF
 40   CONTINUE
      MVPACT = .TRUE.
      VPREC = .FALSE.
      CVPN = 0
C     reset current layering conditions
      CALL GETLAY(CVPN,OK)
C
      RETURN
C
C     error channel
 99   CONTINUE
      CALL DEPRNT(514)
      OK = .FALSE.
      RETURN
 98   CONTINUE
      OK =.TRUE.
 
C
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE VPMOVOK(MI,ENT,TARGET,OK)
C     ====================================
C1    VARYPE             I2  I2   I2   L
C1    IOSTAT             I   I    I    O
C
C2    Checks the target viewport for the entity being added to
C2    the display list. If it allready exists in the list then
C2    The entity will be added to the display list
C2
C2    Arguments:-
C2
C2    MI          ->          Master index pointer of mi
C2    ENT         ->          The entity type.
C2    TARGET      ->          The target viewport required.
C2
C2    Error Returns:
C2
C2    OK         ->           FALSE   Could not add to list
C2                            TRUE    All ok
C2
      include 'include/viewport.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 MI
      INTEGER*2 ENT
      INTEGER*2 TARGET
      INTEGER*2 I
      INTEGER*2 DDFP
      INTEGER*2 TEMP
      LOGICAL OK
C
C     set ok as true.
      OK = .TRUE.
C     loop round the target display file.
      DO 10 I=1,LDFILE(TARGET)-1
 
          IF ( MI.EQ.DFILE(2,I,TARGET).AND.
     +         ENT.EQ.DFILE(1,I,TARGET)) THEN
C             found the current entry. No need to add again.
              OK = .FALSE.
              RETURN
          ENDIF
10    CONTINUE
C     no thing exists so addit to display list
      TEMP = CVPN
      CVPN = TARGET
      CALL ADDISP(MI,ENT,DDFP,OK)
      CVPN = TEMP
 
      END
 
 
 
 
C
C======================================================================
C
C
 
 
 
      SUBROUTINE VPTCTL()
C     ====================
C
C2    This routine controls the main system for viewport
C2    control
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/apollo.inc'
      include 'include/wtov.inc'
      include 'include/macro.inc'
      include 'include/menun.inc'
C
      REAL X,Y
      CHARACTER*80 STRING,INPL*2
      LOGICAL OK,YESOK,CVERFY,WINDOW,CONFRM
      INTEGER*2 C,VPN
      INTEGER*4 CC,IX,IY,TMEN,TCELL
      EXTERNAL CVERFY,CONFRM
C
C
C     set return flag
      VPN = 0
      OK = .FALSE.
      WINDOW = .FALSE.
C
C     view requested get a hit
      IF(CVERFY(CCMD,'abcdef')) THEN
C         call the blitter
          CALL BLTVPT()
C         reset menu system
          OK = .TRUE.
          GOTO 999
      ENDIF
C
C     is he using a window to define the view
      WINDOW = CCMD.EQ.'g'
C
      IF (CVERFY(CCMD,'gh'))  THEN
 
C         get a unused view number
          CALL NEWWIN(VPN,OK)
C         test whether in existance or not
          IF(.NOT.OK) THEN
              CALL DEPRNT(703)
              RETURN
          ENDIF
          IF(VPNUM(VPN)) THEN
              CALL DEPRNT(704)
              OK = .FALSE.
              GOTO 999
          ENDIF
          IF(.NOT.OK) THEN
              CALL DEPRNT(703)
              GOTO 999
          ENDIF
C         call window handler
          CALL DEFVP1(VPN,WINDOW,OK)
          GOTO 999
      ELSEIF(CCMD.EQ.'i') THEN
C         he is removing this window
          IF(CVPN.EQ.0) THEN
              CALL DEPRNT(705)
              GOTO 999
          ENDIF
          IF(CONFRM()) THEN
             VPN = -CVPN
C            undefine the view
             CALL DEFVP1(VPN,WINDOW,OK)
          ENDIF
C
      ENDIF
999   CONTINUE
C     go home
C
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE VPUPGS()
C     =================
C
C1    no arguments required
C     This routine handles the update of the grid
C     select menu if we are in it and switch viewports
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/wtov.inc'
      include  'include/viewport.inc'
      include  'include/vpgrid.inc'
      include  'include/vntable.inc'
      include  'include/gtxt2.inc'
 
C
      CHARACTER TOKEN*1
      EXTERNAL FNDTOK,GTMCWR
C
C     Show we are in the Grid select menu
 
C     Show its a PoPup Cell
      GTMULT = .TRUE.
C     Do update on the PoPuP cell
      IF (GRIDVP(5,CVPN).EQ.1) THEN
          CALL GTDMWT(348,3,VNOUN(58))
      ELSE IF (GRIDVP(5,CVPN).EQ.2) THEN
          CALL GTDMWT(348,3,VNOUN(344))
      ENDIF
C     Turn off PoPup Cell flag
      GTMULT = .FALSE.
 
C     find token for X:
      CALL FNDTOK(287,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDOX,'(F12.2)')
C
C     find token for Y:
      CALL FNDTOK(288,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDOY,'(F12.2)')
C
C     find token for X Dist:
      CALL FNDTOK(285,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDSX,'(F8.2)')
C
C     find token for X Dist:
      CALL FNDTOK(286,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,GRIDSY,'(F8.2)')
C
      END
