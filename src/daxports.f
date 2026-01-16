C
C     @(#)  412.1 date 6/11/92 daxports.f 
C
C
C     Filename    : daxports.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:33:24
C     Last change : 92/06/11 14:27:43
C
C     Copyright : Practical Technology International Limited  
C     File :- daxports.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)
C     SUBROUTINE BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     SUBROUTINE CLEARD(XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE DAXPWN(SX1,SY1,SX2,SY2,OK)
C     SUBROUTINE DEFWIN(OK)
C     SUBROUTINE DRAW_BACK(NUM)
C     SUBROUTINE NEWWIN(NUM,OK)
C     SUBROUTINE SETDP(SX,SY,OVER)
C     SUBROUTINE TRNDP()
C     SUBROUTINE WINCUR(X,Y,OK)
C     SUBROUTINE WINDOW_ACTIVE()
C     SUBROUTINE WINDOW_ADD(NUM)
C     SUBROUTINE WINDOW_BANNER(NUM,STRING,ACTIVE)
C     SUBROUTINE WINDOW_BORDER(NUM)
C     SUBROUTINE WINDOW_DEFINE(NUM,OK)
C     SUBROUTINE WINDOW_DETERMINE(XP,YP)
C     SUBROUTINE WINDOW_INIT(NUM,WINDOW,OK)
C     SUBROUTINE WINDOW_MODIFY(NUM,OK)
C     SUBROUTINE WINDOW_MOVE(NUM)
C     SUBROUTINE WINDOW_OBS_LIST(NUM,LIST)
C     SUBROUTINE WINDOW_PAINT(NUM)
C     SUBROUTINE WINDOW_POP(NUM)
C     SUBROUTINE WINDOW_RESIZE(NUM,OK)
C     SUBROUTINE WINDOW_SET(IX,IY,OVER)
C     SUBROUTINE WINDOW_TEXT(NUM,STRING)
C     SUBROUTINE WNHIT(SX,SY,OK)
C     SUBROUTINE WNPNTA()
C     SUBROUTINE WNPNTZ()
C     SUBROUTINE WNSVBT()
C     SUBROUTINE WNTCTL()
C     SUBROUTINE WRTBAN()
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     ==========================================
C1    VARTYPE                     I2   I2   I2
C1    IOSTST                      I    O    O
C
C2    This routine is a super stack system to build a paint list of
C2    all window currently visable
 
      include   'include/viewport.inc'
C
      INTEGER*2 NUM
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 LNUM
      INTEGER*2 STACK(2,MAXVP),STKP
      INTEGER*2 COUNT
      INTEGER*2 PAINT
      LOGICAL USED,RPLACE,ALL
      INTEGER*2 J,I,TEMP,TNUM
C
C     read the obs list to find out which windows if any will obscured the
C     one to be painted
C
C     are al windows to be painted
      ALL = NUM.EQ.0
      STKP = 0
      COUNT = 1
      PAINT = 1
C     save last paint
C     find the first active window
      IF(ALL) THEN
          LIST(1,1) = 0
          LIST(2,1) = 0
          LNUM = 0
          DO 200 I=1,MAXVP
              IF(VPNUM(I)) THEN
                  LIST(1,1) = 1
                  LIST(2,1) = I
                  LNUM = 1
                  NUM = I
                  GOTO 10
              ENDIF
 200      CONTINUE
C         exit from list no windows active
          RETURN
      ELSE
          LNUM = 1
          LIST(1,1) = 1
          LIST(2,1) = NUM
      ENDIF
C
10    CONTINUE
C         loop round
          IF ( VPNUM(COUNT)) THEN
              IF(OBSLST(NUM,COUNT).AND.COUNT.NE.NUM) THEN
C                 save stack pointer
                  STKP = STKP + 1
                  STACK(1,STKP) = COUNT
                  STACK(2,STKP) = NUM
                  NUM = COUNT
                  COUNT = 1
                  PAINT = PAINT +1
                  GOTO 10
              ENDIF
          ENDIF
          IF(COUNT.EQ.MAXVP) THEN
              IF(STKP.GT.0) THEN
 60               CONTINUE
                  USED = .FALSE.
                  RPLACE = .FALSE.
                  DO 50 J=1,LNUM
                      IF(LIST(2,J).EQ.NUM) THEN
C                         test paint priority
                          USED = .TRUE.
                          RPLACE = PAINT.GT.LIST(1,J)
                          TNUM = J
                      ENDIF
50                CONTINUE
                  IF(.NOT.USED) THEN
                      LNUM = LNUM + 1
                      LIST(1,LNUM) = PAINT
                      LIST(2,LNUM) = NUM
                  ENDIF
C                 If paint priority higher then replace it
                  IF(RPLACE) THEN
                      LIST(1,TNUM) = PAINT
                  ENDIF
                  PAINT  = PAINT -1
                  COUNT = STACK(1,STKP)
                  NUM = STACK(2,STKP)
                  STKP = STKP -1
                  IF(COUNT.EQ.MAXVP) THEN
                      IF(STKP.GT.0) THEN
                          GOTO 60
                      ELSE
                          GOTO 70
                      ENDIF
                  ENDIF
                  COUNT = COUNT + 1
              ELSE
                  GOTO 70
              ENDIF
          ELSE
              COUNT = COUNT + 1
          ENDIF
      GOTO 10
70    CONTINUE
C     if all windows considerd then go round again
      IF(ALL.AND.(NUM.LT.MAXVP)) THEN
          NUM=NUM+1
          COUNT=1
          PAINT = 1
          IF(VPNUM(NUM)) THEN
C             set current window paint
              USED = .FALSE.
              DO 80 J=1,LNUM
                  IF(LIST(2,J).EQ.NUM) THEN
C                     I em zorry to zay zat zees vun is alredree in pent leest
                      USED = .TRUE.
                 ENDIF
80            CONTINUE
C             only add to ze pent leest if not alredeey zere
              IF(.NOT.USED) THEN
                  LNUM = LNUM + 1
                  LIST(1,LNUM) = 1
                  LIST(2,LNUM) = NUM
              ENDIF
              GOTO 10
          ELSE
              GOTO 70
          ENDIF
      ENDIF
 
C     Sort the paint list
      DO 100 I=1,LNUM
C
          DO 110 J=1,LNUM-1
C             sort using bubble sort
              IF(LIST(1,J).GT.LIST(1,J+1)) THEN
                  TEMP =LIST(1,J)
                  LIST(1,J) = LIST(1,J+1)
                  LIST(1,J+1) = TEMP
                  TEMP =LIST(2,J)
                  LIST(2,J) = LIST(2,J+1)
                  LIST(2,J+1) = TEMP
              ENDIF
110       CONTINUE
100   CONTINUE
C
      END
 
C      @(#)  256.1 date 12/16/89 cleard.ftn 
      SUBROUTINE CLEARD(XMIN,YMIN,XMAX,YMAX)
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
      INTEGER*2 XMIN,YMIN,XMAX,YMAX
      INTEGER*2 RECT(1:2,1:2)
      LOGICAL OBSCURED
C
      OBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C     set raster to black mode
      VAL=COLBAK
      CALL RASTER(VAL)
      CALL TOOLPEN(VAL)
      RECT(1,1)=INT(XMIN)
      RECT(2,1)=INT(YMIN)
      RECT(1,2)=INT(XMAX-XMIN)
      RECT(2,2)=INT(YMAX-YMIN)
      CALL GPR_$RECTANGLE(RECT,ST)
      VAL=COLFOR
      CALL TOOLPEN(VAL)
C     set raster to replace mode
      CALL GPR_$RELEASE_DISPLAY(ST)
      CALL ROPREP()
C
      END
 
C      @(#)  256.1 date 12/16/89 daxpwn.ftn 
      SUBROUTINE DAXPWN(SX1,SY1,SX2,SY2,OK)
C     =====================================
C1    vartype            R   R   R   R  L
C1    iostatus           O   O   O   O  O
C
C2    subroutine DAXPWN returns the diagonal
C2    coords of a window on the screen,obtained from
C2    user input by generation of a rubber banded
C2    rectangle on the screen.
C2    If the operation has been successful the logical
C2    flag OK is returned TRUE.
C2    This is the same at GETWIN except on the first hit hit
C2    it does not update the window postion
C
      REAL SX1,SY1,SX2,SY2
C
      INTEGER*4 AX1,AY1,AX2,AY2,C,MENUN,CELLN,SOURCE
C
      LOGICAL OK
C
      CHARACTER CTEXT*80,CCMD*1
C
      EXTERNAL WRKHIT,MENHIT,SCURS,APRREC
C
C     tell the user what to do
 10   CONTINUE
      CALL DCPRNT(88)
      CALL SCURS(C,SX1,SY1)
C     test for hit in wohome
      CALL WNHIT(SX1,SY1,OK)
      IF (.NOT.OK) THEN
C        test for menu cell hit
         CALL MENHIT(INT(SX1),INT(SY1),MENUN,CELLN,CTEXT,CCMD,OK)
         IF (OK) THEN
C           another menu hit,must exit this function
            OK=.FALSE.
            RETURN
         ELSE
C           must be invalid area of screen
            CALL DEPRNT(117)
            GOTO 10
         END IF
      END IF
C
C     go let him define the screen window
      CALL APRREC(SX1,SY1,SX2,SY2,CCMD,SOURCE,OK)
C     return with appropriate status
C
      END
C
C---------------------------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 defwin.ftn 
      SUBROUTINE DEFWIN(OK)
C     =====================
C1    VARTYPE            L
C1    IOSTAST            O
C
C2    This routine will define a viewport by allocating space
C2    for a bitmap and saving the current view onto the new bitmap
C2    it also save the world coords
C
      include 'include/gpr.ins.inc'
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/apollo.inc'
      include 'include/ndata.inc'
      include 'include/icon.inc'
      include 'include/vpops.inc'
C
C
      CHARACTER*20 INPL
      REAL SX1,SY1,SX2,SY2,TXMAX,TYMAX,TXMIN,TYMIN
      REAL X1,Y1,X2,Y2
      REAL X,Y
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 LNUM
      INTEGER*2 VPN,MAIN,C,TVPN,BMSIZ(2),NUM
      INTEGER*4 NLEN,I,IX,IY,DNUM
      INTEGER*4 ST
      LOGICAL OK,REDIF,YESOK,WINDOW,KILLV,MAKEOK,TEMP,WINDEF
      LOGICAL TRIP,CONFRM,CVERFY
      LOGICAL FIRST_PORT
C
      EXTERNAL MAKEOK,YESOK,CONFRM,CVERFY
C
      OK = .FALSE.
      TRIP = .FALSE.
      WINDOW = .FALSE.
C     set up the pixel rectangles for BLT
C     if maws the we allready know the limits
      MAIN = 0
C
100   CONTINUE
C
C     Test wheteher operation valid on an icon
      IF(CVPN.GT.0) THEN
          IF(ICNUM(CVPN).AND.CVERFY(CCMD,'GR')) THEN
              DNUM = 750
              CALL DEPRNT(DNUM)
C             go back round
              OK = .FALSE.
              RETURN
          ENDIF
      ENDIF
C
C     allocate a bitmap for the main drawing
      WINDEF = .TRUE.
      IF(CCMD.EQ.'D') THEN
C         confirm the quit first please
          IF(.NOT.CONFRM() ) RETURN
          NUM = CVPN
C         get the swapping list organised
          DO 80 I=1,MAXVP
C             swapping list if window is unobscured
              IF(VPNUM(I).AND.I.NE.NUM) THEN
C                 if the window is allready obs then leave it
                  IF(OBSLST(I,NUM)) THEN
                     TEMP = OBSLST(I,NUM)
                     OBSLST(I,NUM) =OBSLST(NUM,I)
                     OBSLST(NUM,I) =TEMP
                  ENDIF
               ENDIF
 80       CONTINUE
C         get a list of all windows ascociated with this window
          CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
C         paint in the back ground please
          CALL WINDOW_PAINT(-NUM)
C         set new window size
C         do the the other windows that were obscured
          DO 70 I=2,LNUM
              CALL WINDOW_PAINT(LIST(2,I))
70        CONTINUE
C         Free the bitmap for the next time
          CALL FREE_BITMAP(NUM,OK)
          IF(.NOT.OK) THEN
C             error trap
              VPNAME(NUM) = VPUDEF
              DNUM = 751
              CALL DEPRNT(DNUM)
          ENDIF
          DO 50 I=1,MAXVP
              OBSLST(I,NUM) = .FALSE.
              OBSLST(NUM,I) = .FALSE.
 50       CONTINUE
C         resets iconer lfags
          ICNUM(NUM) = .FALSE.
          ICPOS(NUM) = .FALSE.
C         turn off a name
          CALL PORT_SET_NAME(NUM,' ',ST)
C         flag deleted
          CALL PORT_ACTIVE(NUM,.FALSE.,ST)
C         Show grid is off in case it was still on
          VPGRID(NUM) = .FALSE.
C         are any viewports still active
          IF(.NOT.MVPACT) THEN
C             dealocate the main bitmap and turn of viewporting system
              VPN = 0
              CALL  FREE_BITMAP(VPN,OK)
              IF(.NOT.OK) THEN
                  DNUM = 751
                  CALL DEPRNT(DNUM)
              ENDIF
          ENDIF
C         reset main window
          IX=0
          IY=0
C         over ride window control
          PVPN = NUM
          CVPN = 0
          CALL WINDOW_SET(IX,IY,.TRUE.)
          RETURN
      ELSEIF(CCMD.EQ.'M') THEN
          CALL WINDOW_MOVE(CVPN)
          RETURN
C      ELSEIF(CCMD.EQ.'P') THEN
C          CALL WINDOW_POP(CVPN)
C          RETURN
      ELSEIF(CCMD.EQ.'R') THEN
          CALL WINDOW_RESIZE(CVPN,OK)
          IF(.NOT.OK) RETURN
          WINDEF = .FALSE.
C         replace this window plaese
          VPN = CVPN
      ELSEIF(CCMD.EQ.'I') THEN
          NUM = CVPN
          CALL ICON_TOGGLE(NUM)
          RETURN
      ELSEIF(CCMD.EQ.'G') THEN
C         toggle the grid on or off
          IF(ICNUM(CVPN)) RETURN
          IF ( SETGRD ) THEN
              CALL EGRID()
              SETGRD=.FALSE.
          ELSE
C             Setgrd is before grid1 so if the grid cannot be
C             generated then it is set to false inside grid1
              SETGRD=.TRUE.
              CALL GRID1()
          END IF
          IF ( SETGRD ) THEN
C             show grid on state in cell
              VPGRID(CVPN) = .TRUE.
              CALL GTDMCH(59,4)
          ELSE
C             show grid off state in cell
              VPGRID(CVPN) = .FALSE.
              CALL GTDMCL(59,4)
          END IF
C
C         update the banner
          CALL WRTBAN()
          RETURN
C
C
      ELSEIF(CCMD.EQ.'P') THEN
C         get a name for this view
          NUM = CVPN
          IF(NUM.GT.0) THEN
              CALL DPRMXP(770,VPNAME(NUM))
              CALL WINDOW_PAINT(NUM)
          ENDIF
          RETURN
      ENDIF
C
C     auto rebuild windows allready valued
      IF(CCMD.EQ.'W') THEN
          WINDOW = .TRUE.
      ENDIF
C     get a new number
      IF(VPREC) THEN
C         get number from builder
          VPN = CVPN
      ELSEIF (WINDEF.AND..NOT.VPREC) THEN
C         get next number
          CALL NEWWIN(VPN,OK)
          IF(.NOT.OK) THEN
              DNUM = 752
              CALL DEPRNT(DNUM)
              RETURN
          ENDIF
      ENDIF
C
      FIRST_PORT = .FALSE.
      IF(.NOT.MVPACT) THEN
C         allocate a backcloth 
          CALL ALLOCATE_BITMAP(MAIN,OK)
          IF(.NOT.OK) THEN
              CALL DEPRNT(503)
              RETURN
          ENDIF
          FIRST_PORT = .TRUE.
      ENDIF
C
C
C     get a window size form the user
      IF (WINDEF.AND..NOT.VPREC) THEN
          CALL WINDOW_DEFINE(VPN,OK)
          IF(.NOT.OK) THEN 
C             deactivate the port please
              CALL PORT_ACTIVE(VPN,.FALSE.,ST)
              IF(.NOT.MVPACT) THEN
C                 did not define window system
                  CALL FREE_BITMAP(INT2(0),OK)
                  DNUM = 753
                  CALL DEPRNT(DNUM)
              ENDIF
C             dont want the window
              RETURN
          ENDIF
      ENDIF
C
C
      IF(VPNUM(VPN)) THEN
C         if allready active then free up the bitmap
          CALL FREE_BITMAP(VPN,OK)
      ENDIF
C     save values
      BMSIZ(1) = BMSIZE(1)
      BMSIZ(2) = BMSIZE(2)
      BMSIZE(1) = VIEWPO(1,VPN)+1
      BMSIZE(2) = VIEWPO(2,VPN)+1
C     allocate the bitmap
      CALL ALLOCATE_BITMAP( VPN,OK )
      BMSIZE(1) = BMSIZ(1)
      BMSIZE(2) = BMSIZ(2)
C
      IF(.NOT.OK) THEN
          CALL DEPRNT(503)
C         deactivate the port now
          CALL PORT_ACTIVE(VPN,.FALSE.,ST)
          IF(.NOT.MVPACT) THEN
C             did not define window system
              CALL FREE_BITMAP(INT2(0),OK)
              DNUM = 753
              CALL DEPRNT(DNUM)
              RETURN
         ENDIF
         RETURN
      ENDIF
C     set up port as active now
      CALL PORT_ACTIVE(VPN,.TRUE.,ST)
C
C     set up window contents
      CALL WINDOW_INIT(VPN,WINDOW,OK)


      IF (WINDEF.AND..NOT.VPREC) THEN
C         clear out current name if new window
          CALL PORT_SET_NAME(VPN,' ',ST)
      ENDIF

C     update obs list
      CALL WINDOW_ADD(VPN)
C
      CALL PORT_CREATE_SET(.FALSE.,ST)
      CALL PORT_OP_SET(VP_OP_NORMAL,ST)
C
      IF(FIRST_PORT) THEN
C         save the current configuration if no current views drawn
          CALL PORT_SCREEN_TO_HIDDEN(INT2(0),ST)
      ENDIF
C     paint in the new one
      IF(.NOT.VPREC) THEN
C         pain in a new window please
          CALL WINDOW_PAINT(VPN)
      ENDIF     
      OK = .TRUE.
C
      END
C
C      @(#)  256.1 date 12/16/89 draw_back.ftn 
      SUBROUTINE DRAW_BACK(NUM)
C     ========================
C1    VARTYPE              I2
C1    IOSTAT               I
C
C2    This routine should paint in the background for the required
C2    viewport or not This will be a blt of the main viewport
C
      include   'include/gpr.ins.inc'
      include   'include/fill_pattern.inc'
      include   'include/viewport.inc'
      include   'include/wtov.inc'
C
      INTEGER*4 ST,VAL
      INTEGER*2 XMIN,YMIN,XMAX,YMAX
      INTEGER*2 RECT(2,2)
      INTEGER*2 SCALE,num
      LOGICAL OBSCURED,OK
C
      OBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C     set raster to black mode
      VAL=1
      IF(NUM.GT.0) THEN
C         set size of source window
          WINBLT(1) = VIEWEX(1,NUM)-1
          WINBLT(2) = VIEWEX(2,NUM)-1
          WINBLT(3) = VIEWPO(1,NUM)+3
          WINBLT(4) = VIEWPO(2,NUM)+3
          ORIGIN(1) = VIEWEX(1,NUM)-1
          ORIGIN(2) = VIEWEX(2,NUM)-1
      ELSE
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
      ENDIF
C     get the new bitmap
      CALL GET_MAIN_BITMAP(OK)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C
C------------------------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 newwin.ftn 
      SUBROUTINE NEWWIN(NUM,OK)
C     =========================
C1    VARTYPE            I2 L
C1    IOSTST             I  O
C
C2    The routine will find the first unused daxport in ther list
      include  'include/viewport.inc'
C
      INTEGER*2 NUM,I
      LOGICAL OK
C
      OK = .TRUE.
      DO 10 I=1,MAXVP
          IF(.NOT.VPNUM(I)) THEN
              NUM = I
              RETURN
          ENDIF
10    CONTINUE
      OK = .FALSE.
      END
C
C       @(#)  256.1 date 12/16/89 pntbox.ftn 
      LOGICAL FUNCTION PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)
C     ==================================================
C1    VARTYP                  I4    I4   I4I  I4  I4  I4
C1    IOSTS                   I     I     I   I    I   I
C
C2    Function return whwteher the thing is in the bix
C
      INTEGER*4 XMAX,XMIN,YMAX,YMIN,XP,YP
 
      PNTBOX = XP.GT.XMIN.AND.YP.GT.YMIN.AND.XP.LT.XMAX.
     +           AND.YP.LT.YMAX
      END
C
C      @(#)  256.1 date 12/16/89 setdp.ftn 
      SUBROUTINE SETDP(SX,SY,OVER)
C     ============================
C1    vartype           R   R   L
C1    iostatus          O   O   O
C
C2    This routine will take any coordiante postion out of daxcad
C2    and set the current viewport positon
C
C
      include  'include/server.inc'
      REAL SX,SY
      INTEGER*4 IX,IY
      LOGICAL OVER
C     set window
      IF ( SERVER ) THEN
          RETURN
      ENDIF
      IX = SX
      IY = SY
      CALL WINDOW_SET(IX,IY,OVER)
      END
C
C      @(#)  256.1 date 12/16/89 trndp.ftn 
      SUBROUTINE TRNDP()
C     ==================
C1    NO ARGS
C
C2    This routine saves the current world values for a daxport
C2    transform if any changes have been made
C2
      include  'include/viewport.inc'
      include  'include/vnames.inc'
      include  'include/wtov.inc'
      include  'include/server.inc'
C
      INTEGER*2 NUM
      REAL WX1,WY1,WX2,WY2
C
C
      IF ( SERVER ) THEN
          RETURN
      ENDIF
      WX1 = WXMIN
      WY1 = WYMIN
      WX2 = WXMAX
      WY2 = WYMAX
      IF(CVPN.EQ.0) THEN
          VXMIN = VPXMIN
          VYMIN = VPYMIN
          VXMAX = VPXMAX
          VYMAX = VPYMAX
      ENDIF
C  
      IF(.NOT.MAWS.AND.CVPN.GT.0) THEN
C         current DAXPORT size
          VXMIN = VIEWEX(1,CVPN)
          VYMIN = VIEWEX(2,CVPN)+VPBANN
          VXMAX = VIEWEX(3,CVPN)
          VYMAX = VIEWEX(4,CVPN)
      ENDIF
C     save all worlds of this viewport we may need it
      VIEWPS(1,CVPN+1) = WX1
      VIEWPS(2,CVPN+1) = WY1
      VIEWPS(3,CVPN+1) = WX2
      VIEWPS(4,CVPN+1) = WY2
      CALL WORLD(WX1,WY1,WX2,WY2)
C
C     save this in our array
      NUM = CVPN
      CALL TRNSAV(NUM)
C     Now reset actual display transform
C
      END
C
C
C     ----------------------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 wincur.ftn 
      SUBROUTINE WINCUR(X,Y,OK)
C     =========================
C1    vartype           R   R   L
C1    iostatus          O   O   O
C
C2    This routine will pop a window if the cursor lies in a window box
C2    It does by setting a window current and then determining the
C2    box postionC
C
      include  'include/viewport.inc'
      include  'include/icon.inc'
      include  'include/vnames.inc'
      include  'include/wtov.inc'
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/layer.inc'
C
      REAL X,Y
      LOGICAL PNTBOX,OK,OBS,SINGLE
      INTEGER*2 TVPN,IVPN
      INTEGER*4 IX,IY,XMIN,YMIN,XMAX,YMAX,I
      INTEGER*4 I4
C
C     make a window current
      OK = .FALSE.
      IF(MAWS) RETURN
C     go home if MAWS
      TVPN = CVPN
      IX = X
      IY = Y
      CALL WINDOW_DETERMINE(IX,IY)
      IF(CVPN.GT.0) THEN
          IF(ICNUM(CVPN)) THEN
              CVPN = TVPN
              OK = .FALSE.
              RETURN
          ENDIF
          XMIN = VIEWEX(1,CVPN)
          YMIN = VIEWEX(2,CVPN)
          XMAX = VIEWEX(3,CVPN)
          YMAX = YMIN+VPBANN
C         test for a hit within an icon also
          IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,IX,IY) ) THEN
C             if window is obscuring anything then main window must become curre
C             deal with any problems
C             determine whether cursor hit is in box
C                 lets open it for him since he got it
              OBS = .FALSE.
              DO 10 I=1,MAXVP
                  IF(OBSLST(CVPN,I).AND.I.NE.CVPN) THEN
C                     this window is now obscured and is not current
                      OBS = .TRUE.
                  ENDIF
 10           CONTINUE
C             find out whether we are on our own
              SINGLE = .TRUE.
              DO 20 I=1,MAXVP
                  IF(OBSLST(I,CVPN).AND.I.NE.CVPN) THEN
C                     this window is now obscured and is not current
                      SINGLE = .FALSE.
                  ENDIF
 20           CONTINUE
C
              IVPN = CVPN
              PVPN = TVPN
              CALL WINDOW_ACTIVE()
C             pop the cunt
              IF(.NOT.OBS.AND..NOT.SINGLE) THEN
                  CVPN = 0
                  VXMIN = VPXMIN
                  VYMIN = VPYMIN
                  VXMAX = VPXMAX
                  VYMAX = VPYMAX
C                 restore main windows
                  CALL TRNRST(CVPN)
              ELSE
                  IF(ICNUM(CVPN)) THEN
                      VXMIN = VIEWEX(1,CVPN)
                      VYMIN = VIEWEX(2,CVPN)
                      VXMAX = VIEWEX(3,CVPN)
                      VYMAX = VIEWEX(4,CVPN)
                  ELSE
                      VXMIN = VIEWEX(1,CVPN)
                      VYMIN = VIEWEX(2,CVPN)+VPBANN
                      VXMAX = VIEWEX(3,CVPN)
                      VYMAX = VIEWEX(4,CVPN)
C                     get transform
                      CALL TRNRST(CVPN)
                  ENDIF
              ENDIF
              CALL WINDOW_POP(IVPN)
C
              OK = .TRUE.
              CALL GETGRD(CVPN,OK)
              CALL GETLAY(CVPN,OK)
C             modify grid coords
              IF(.NOT.DISLAY) THEN
                  I4=CLAYER
                  CALL GTDMWI(60,4,I4)
C                 write name of layer directly.
                  IF(LOADED) THEN
                      CALL GTDMEN(336,4)
                  ELSE
                      CALL GTPMEN(LNAME(CLAYER),'N',4,2)
                  ENDIF
               ENDIF
          ELSE
C             do not reset at this stage please
              CVPN = TVPN
          ENDIF
      ELSE
          CVPN = TVPN
      ENDIF
      END
C
C     ----------------------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 window_active.ftn 
      SUBROUTINE WINDOW_ACTIVE()
C     ===========================
C1
C
C2    Determines which window is active and takes nessesarry course of action
C2    by filling in the current
C
      include  'include/viewport.inc'
      include  'include/vnames.inc'
C
      CHARACTER*400 STRING
C
      INTEGER*2 NUM
      INTEGER*4 I
      LOGICAL OBS
C
C     IF(MAWS) RETURN
C
C     wee need to fill in the old viewport before painting
      IF(PVPN.GT.0) THEN
          IF(VPNUM(PVPN)) THEN
              NUM = PVPN
              CALL WINDOW_TEXT(NUM,STRING)
              CALL WINDOW_BANNER(NUM,STRING,.FALSE.)
          ENDIF
      ENDIF
C
      IF(CVPN.GT.0) THEN
          IF(VPNUM(CVPN)) THEN
C             we must paint in the active window now
              NUM = CVPN
              CALL WINDOW_TEXT(NUM,STRING)
              CALL WINDOW_BANNER(NUM,STRING,.TRUE.)
          ENDIF
      ENDIF
      END
C
 
 
C      @(#)  256.1 date 12/16/89 window_add.ftn 
      SUBROUTINE WINDOW_ADD(NUM)
C     ==========================
C1    VARTYPE                I2
C1    IOSTST                 I
C
C2    Add a view to the manager list
C2    This routine assumes the window size has
C2    allready been added into the array
C
      include   'include/viewport.inc'
C
      INTEGER*2 NUM
      INTEGER*4 I
      LOGICAL LIST(MAXVP)
 
      CALL WINDOW_OBS_LIST(NUM,LIST)
      DO 10 I=1,MAXVP
C
          IF(I.NE.NUM.AND.VPNUM(I) ) THEN
C
              IF(LIST(I)) THEN
                  OBSLST(I,NUM) = .TRUE.
 
              ELSE
                  OBSLST(I,NUM) = .FALSE.
              ENDIF
          ELSEIF(I.EQ.NUM) THEN
              OBSLST(I,NUM) = .TRUE.
          ENDIF
C
 10   CONTINUE
      END
 
 
C      @(#)  256.1 date 12/16/89 window_banner.ftn 
      SUBROUTINE WINDOW_BANNER(NUM,STRING,ACTIVE)
C     ===========================================
C1    VARTYPE                  I2   C*(*)  L
C1    IOSTST                   I      I    I
C
C     This routine will paint in the header bar on a daxport
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/icon.inc'
      include 'include/wtov.inc'
      include 'include/apfont.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*2 WINHT,STX,STY,STRLEN
      INTEGER*4 ST,NLEN,OPT
      INTEGER*4 FORE,BACK
      INTEGER*2 NUM,CFNTID,VTFONT
      INTEGER*2 XP1,YP1,XP2,YP2,WINDOW(4)
      INTEGER*2 CURPEN
      LOGICAL ACTIVE
      CHARACTER*(*) STRING
C
      EXTERNAL NLEN
C     save current pen color
      CURPEN = COLPEN
      CALL SETDRW(COLFOR)
C     border height in pixels
C     load up our vt100 text font for daxport
      IF(ICNUM(NUM)) THEN
C         for an icon banner we need to higlight the whole thing
          CALL ICON_PAINT(NUM)
          GOTO 999
      ENDIF
C     swap text values round
      CALL GPR_$SET_TEXT_FONT(APFNTS(2),ST)
C
      STRLEN = NLEN(STRING)
C     set
      WINDOW(1) = VIEWEX(1,NUM)
      WINDOW(2) = VIEWEX(2,NUM)
      WINDOW(3) = VIEWPO(1,NUM)+1
      WINDOW(4) = VPBANN
C     draw it
      IF(.NOT.ACTIVE) THEN
          OPT = COLBAK
          CALL TOOLPEN(OPT)
          CALL GPR_$RECTANGLE(WINDOW,ST)
          CALL GPR_$DRAW_BOX(
     +    INT2(VIEWEX(1,NUM)),
     +    INT2(VIEWEX(2,NUM)),
     +    INT2(VIEWEX(3,NUM)),
     +    INT2(VIEWEX(4,NUM)),
     +    ST)
          OPT = COLFIL
          CALL TOOLPEN(OPT)
      ELSE
          OPT=COLFIL
          CALL TOOLPEN(OPT)
          CALL GPR_$RECTANGLE(WINDOW,ST)
      ENDIF
C
      IF(.NOT.ACTIVE) THEN
          FORE = COLFOR
          BACK = COLBAK
          CALL TOOLPEN_TEXT(FORE,BACK,.TRUE.,ST)
      ELSE
          FORE = COLFOR
          BACK = COLBAK
          CALL TOOLPEN_TEXT(BACK,FORE,.TRUE.,ST)
      ENDIF
C     get start positions for text
      STX = VIEWEX(1,NUM)+2
      STY = VIEWEX(2,NUM)+VPBANN-4
      CALL GPR_$MOVE(STX,STY,ST)
      CALL GPR_$TEXT(STRING,STRLEN,ST)
C
C     swap text values round
      CALL GPR_$SET_TEXT_FONT(APFNTS(1),ST)
      FORE = COLFOR
      BACK = COLBAK
      CALL TOOLPEN_TEXT(FORE,BACK,.TRUE.,ST)
999   CONTINUE
C     reste color please
      CALL SETDRW(CURPEN)
      END
C
C
C      @(#)  256.1 date 12/16/89 window_border.ftn 
      SUBROUTINE WINDOW_BORDER(NUM)
C     =============================
C1    VARTYPE                  I2
C1    IOSTST                   I
C
C2    This routine will paint the border round the viewport
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
      include 'include/apfont.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*2 XP1,YP1,XP2,YP2,NUM
      INTEGER*2 CURPEN
C
C     border height in pixels
C     draw the border
      XP1 = VIEWEX(1,NUM)-1
      YP1 = VIEWEX(2,NUM)-1
      XP2 = VIEWEX(3,NUM)+1
      YP2 = VIEWEX(4,NUM)+1
C
C     save current pen color
      CURPEN = COLPEN
C     draw in as white or somthin
      CALL SETDRW(COLFOR)
C
      CALL DRSREC(XP1,YP1,XP2,YP2)
      CALL SETDRW(CURPEN)
C
      END
C      @(#)  256.1 date 12/16/89 window_define.ftn 
      SUBROUTINE WINDOW_DEFINE(NUM,OK)
C     ================================
C1    VARTYPE                  I2   L
C1    IOSTST                   I    O
C
C2    This routine will define a viewport on the screen
C2
C
      include   'include/viewport.inc'
      INTEGER*2 NUM
      INTEGER*2 X1,Y1,X2,Y2,DISTX,DISTY
      REAL SX1,SX2,SY1,SY2
      LOGICAL OK
C
      CALL DCPRNT(732)
      CALL DAXPWN(SX1,SY1,SX2,SY2,OK)
      IF(.NOT.OK) RETURN
C     convert these reals
      X1 = SX1
      Y1 = SY1
      X2 = SX2
      Y2 = SY2
C
C
      DISTX = ABS(X2-X1)
      DISTY = ABS(Y2-Y1)
C
C     save the current viewport dimensions
      VIEWEX(1,NUM) = X1
      VIEWEX(2,NUM) = Y1
      VIEWEX(3,NUM) = X1+DISTX
      VIEWEX(4,NUM) = Y1+DISTY
      X2 = X1+DISTX
      Y2 = Y1+DISTY
C     set size of bitmap includeing size for border
      VIEWPO(1,NUM) = X2-X1
      VIEWPO(2,NUM) = Y2-Y1
C     Modify the coords
      CALL WINDOW_MODIFY(NUM,OK)
C
      END
C
C
C      @(#)  256.1 date 12/16/89 window_determine.ftn 
      SUBROUTINE WINDOW_DETERMINE(XP,YP)
C     ==================================
C1    VARTYPE                     I4  I4
C1    IOSTAT                      I   I
C
C2    This function will determine the current window environment
C2    It sets the global CVPN to the current viewport numnber
C2
      include   'include/viewport.inc'
C
      INTEGER I,OBS,J,XMAX,XMIN,YMAX,YMIN,XP,YP
      LOGICAL PNTBOX
      INTEGER*2 LIST(2,MAXVP),LNUM,NUM,K,TVPN
      EXTERNAL PNTBOX
C
      OBS = 0
      IF(.NOT.MVPACT) RETURN
C
      TVPN = CVPN
      DO 10 I=1,MAXVP
          IF(VPNUM(I)) THEN
C             Findout if window is obscurred in any way if so the check point
              XMIN = VIEWEX(1,I)
              YMIN = VIEWEX(2,I)
              XMAX = VIEWEX(3,I)
              YMAX = VIEWEX(4,I)
C             loop round this window to test
              IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)) THEN
C                 time to build and paint asccoiated windows
                  CVPN = NUM
                  NUM = I
                  CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
                  DO 30 J=LNUM,1,-1
                      K = LIST(2,J)
                      XMIN = VIEWEX(1,K)
                      YMIN = VIEWEX(2,K)
                      XMAX = VIEWEX(3,K)
                      YMAX = VIEWEX(4,K)
                      IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)) THEN
                         CVPN = K
                         GOTO 200
                      ENDIF
 30               CONTINUE
                  GOTO 200
               ENDIF
 20         CONTINUE
         ENDIF
 10   CONTINUE
      CVPN = OBS
200   CONTINUE
C     have we changed if so then save change
      IF(CVPN.NE.TVPN) PVPN = TVPN
 
      END
 
C      @(#)  256.1 date 12/16/89 window_init.ftn 
      SUBROUTINE WINDOW_INIT(NUM,WINDOW,OK)
C     =======================================
C1    VARTYPE                  I2     L   L
C1    IOSTST                   I      I   O
C
C2    This routine will copy the contents of the display
C2    file into the requested viewport or it will copy
C2
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
      REAL VX1,VX2,VY1,VY2
      REAL TXMAX,TXMIN,TYMAX,TYMIN
      INTEGER*4 CBITM,ST
      INTEGER*4 X1,Y1
      INTEGER*2 TVPN,VPN,TMIP,DFPK,ENT,MIPP
      INTEGER*2 NUM
      LOGICAL OK,WINDOW,UNOBSCURED
C
C     save actual viewport screen extents first
      CALL PORT_SCREEN_EXTENTS_INQ(NUM,
     +                             VX1,
     +                             VY1,
     +                             VX2,
     +                             VY2,
     +                             ST)

C     set window falg on
      IF(WINDOW) THEN
C
C
          SX1 = VIEWEX(1,NUM)
          SY1 = VIEWEX(2,NUM)
          SX2 = VIEWEX(3,NUM)
          SY2 = VIEWEX(4,NUM)
C         smallest first allready sorted
          CALL SC2WO(SX1,SY2,WX1,WY1)
          CALL SC2WO(SX2,SY1,WX2,WY2)
C
C         set up viewport for drawing into hiden first
          CALL PORT_SCREEN_EXTENTS_SET(NUM,
     +                                 REAL(0),
     +                                 REAL(0),
     +                                 REAL(VIEWPO(1,NUM)),
     +                                 REAL(VIEWPO(2,NUM)),
     +                                 ST)
C
C         save all worlds of this viewport we may need it
          VIEWPS(1,NUM+1) = WX1
          VIEWPS(2,NUM+1) = WY1
          VIEWPS(3,NUM+1) = WX2
          VIEWPS(4,NUM+1) = WY2
          VXMIN = 0
          VYMIN = VPBANN
          VXMAX = VIEWPO(1,NUM)
          VYMAX = VIEWPO(2,NUM)
          CALL WORLD(WX1,WY1,WX2,WY2)
C         now rebuild world to look at the whole viewport
          SX1 = VXMIN
          SY1 = VYMIN
          SX2 = VXMAX
          SY2 = VYMAX
          CALL SC2WO(SX1,SY2,WX1,WY1)
          CALL SC2WO(SX2,SY1,WX2,WY2)
C         save all world vales
          VIEWPS(1,NUM+1) = WX1
          VIEWPS(2,NUM+1) = WY1
          VIEWPS(3,NUM+1) = WX2
          VIEWPS(4,NUM+1) = WY2
C         build new transform
          CALL WORLD(WX1,WY1,WX2,WY2)

      ELSE
C
          IF(VPREC) THEN
C             if rebuilding then values allredy loaded 
              WX1 = VIEWPS(1,NUM+1) 
              WY1 = VIEWPS(2,NUM+1) 
              WX2 = VIEWPS(3,NUM+1) 
              WY2 = VIEWPS(4,NUM+1) 
          ELSE
C             noermal extents
              WX1 = WXMIN
              WY1 = WYMIN
              WX2 = WXMAX
              WY2 = WYMAX
          ENDIF
C
C         set up viewport for drawing into hiden first
          CALL PORT_SCREEN_EXTENTS_SET(NUM,
     +                                 REAL(0),
     +                                 REAL(0),
     +                                 REAL(VIEWPO(1,NUM)),
     +                                 REAL(VIEWPO(2,NUM)),
     +                                 ST)
C	    set up at transfrom to draw into the viewport
          VXMIN = 0
          VYMIN = VPBANN
          VXMAX = VIEWPO(1,NUM)
          VYMAX = VIEWPO(2,NUM)
C         now make world coords extents of current worls
          VIEWPS(1,NUM+1) = WX1
          VIEWPS(2,NUM+1) = WY1
          VIEWPS(3,NUM+1) = WX2
          VIEWPS(4,NUM+1) = WY2
          CALL WORLD(WX1,WY1,WX2,WY2)
C         ok now take viewport extants and make that the new world
          SX1 = VXMIN
          SY1 = VYMIN
          SX2 = VXMAX
          SY2 = VYMAX
C
          CALL SC2WO(SX1,SY2,WX1,WY1)
          CALL SC2WO(SX2,SY1,WX2,WY2)
          VIEWPS(1,NUM+1) = WX1
          VIEWPS(2,NUM+1) = WY1
          VIEWPS(3,NUM+1) = WX2
          VIEWPS(4,NUM+1) = WY2
          CALL WORLD(WX1,WY1,WX2,WY2)
C
      ENDIF
C     save transform for create stage
      CALL TRNSAV(NUM)
C     draw into the database
C
C     inherit grid conditions
C     Save the contents of the grid into the current viewport
      CALL SAVGRD(NUM,OK)
C     save the layer ascocated with this view inherited from backcloth
      CALL SAVLAY(NUM,OK)
C     find current port
      CALL PORT_INQ(TVPN,ST)
C     set new port
      CALL PORT_SET(NUM,ST)
C     tell system we are creating a new one
      CALL PORT_CREATE_SET(.TRUE.,ST)
C     set up port to draw
      CALL VPDRAW(NUM,.TRUE.,ST)
C
C     **********************************
C     Start drawing into the hidden view
C     **********************************
C
      CALL CLEAR()
C     draw paper extents
      CALL DISFRA()
C     Reset display file pointer to one
      DFPK = 1
      TMIP=0
C     set display file pointer
      LDFILE(CVPN) = 1
C
      IF ( NMIPOS .EQ. 1 ) THEN
C         nothing in the database
          GOTO 89
      ENDIF
C 
 10   TMIP=TMIP+1
C     read database
      CALL DIR500(TMIP,OK)
      IF ( VLAYER(IMBUFF(4)) ) THEN
C        check that it is not a deleted entity,
C        or a component master.
         IF (IMBUFF(1).NE.10.AND.IMBUFF(1).NE.GROUP) THEN
C             cannot draw this one
              GOTO 11
         ENDIF
         ENT=IMBUFF(2)
C        draw it to bitmap
         CALL ALLDRW(ENT,TMIP)
C        if visable then increment
C
         IF ( DISPV ) THEN
C            update the display file
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
89    CONTINUE
C     Inherit the grid please
      IF( SETGRD ) THEN
C         turn on grid
          CALL GRID1()
      ENDIF
C
C     rset viewport
      CALL VPRES()
C
C     system call to reset the main bitmap
C
C     set on screen extents
      CALL PORT_SCREEN_EXTENTS_SET(NUM,
     +                             VX1,
     +                             VY1,
     +                             VX2,
     +                             VY2,
     +                             ST)
C     set viewport extents as the workd basis
      VXMIN = VX1
      VYMIN = VY1+VPBANN
      VXMAX = VX2
      VYMAX = VY2
C
      WX1 = VIEWPS(1,NUM+1)
      WY1 = VIEWPS(2,NUM+1)
      WX2 = VIEWPS(3,NUM+1)
      WY2 = VIEWPS(4,NUM+1)
C     reset trans form on same as original
      CALL WORLD(WX1,WY1,WX2,WY2)
C     save transform used in this system
      CALL TRNSAV(NUM)
C
C     make sure window reset to main view
      CALL PORT_SET(TVPN,ST)
      CALL PORT_CREATE_SET(.FALSE.,ST)
      X1 = 0
      Y1 = 0
      IF(.NOT.VPREC) THEN
C         reset all to background if not restoruing
          CALL WINDOW_SET(X1,Y1,.TRUE.)
C         reset worlds
          VXMAX = VPXMAX
          VYMAX = VPYMAX
          VXMIN = VPXMIN
          VYMIN = VPYMIN
          TXMIN = VIEWPS(1,1)
          TYMIN = VIEWPS(2,1)
          TXMAX = VIEWPS(3,1)
          TYMAX = VIEWPS(4,1)
          CALL WORLD (TXMIN,TYMIN,TXMAX,TYMAX)
      ENDIF
C
      END
C
C-------------------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 window_modify.ftn 
      SUBROUTINE WINDOW_MODIFY(NUM,OK)
C     ===============================
C1    VARTYPE                  I2  L
C1    IOSTATS                  I   O
C
C2    This routine will modify a window position so that it
C2    will fit onto the existing screen without going over
C2    the limits
C
      include 'include/viewport.inc'
      include 'include/icon.inc'
      include 'include/wtov.inc'
C
      INTEGER*4 DISTX,DISTY
      INTEGER*2 NUM
      INTEGER*4 MAXSIZ
      LOGICAL OK
C
C     set up limits for box limits
C     limit size of box
      VIEWPO(1,NUM) = MIN(VIEWPO(1,NUM),VPXMAX-VPXMIN-5)
      VIEWPO(2,NUM) = MIN(VIEWPO(2,NUM),VPYMAX-VPYMIN-5)
      IF(ICNUM(NUM)) THEN
          MAXSIZ = 60
      ELSE
          MAXSIZ = 100
      ENDIF
C
      VIEWPO(1,NUM) = MAX(VIEWPO(1,NUM),MAXSIZ)
      VIEWPO(2,NUM) = MAX(VIEWPO(2,NUM),MAXSIZ)
 
C     Origin of box should be within main box
      VIEWEX(1,NUM) = MAX(VIEWEX(1,NUM),VPXMIN+2)
      VIEWEX(2,NUM) = MAX(VIEWEX(2,NUM),VPYMIN+2)
      VIEWEX(1,NUM) = MIN(VIEWEX(1,NUM),VPXMAX-VIEWPO(1,NUM)-2)
      VIEWEX(2,NUM) = MIN(VIEWEX(2,NUM),VPYMAX-VIEWPO(2,NUM)-2)
C
      DISTX = VIEWPO(1,NUM)
      DISTY = VIEWPO(2,NUM)
C
C     save the current viewport dimensions
      VIEWEX(3,NUM) = VIEWEX(1,NUM)+DISTX
      VIEWEX(4,NUM) = VIEWEX(2,NUM)+DISTY
 
 
C
      END
C
C      @(#)  256.1 date 12/16/89 window_move.ftn 
      SUBROUTINE WINDOW_MOVE(NUM)
C     ===========================
C1    VARTYPE                 I2
C1    IOSTAT                  I
C
C2    paint a window clear or with contents
C
      include   'include/gpr.ins.inc'
      include   'include/fill_pattern.inc'
      include   'include/viewport.inc'
      include   'include/icon.inc'
      include   'include/wtov.inc'
C
      INTEGER*4 IX,IY
      INTEGER*2 XP1,YP1,XP2,YP2,BACK
      INTEGER*2 OXP1,OYP1,OXP2,OYP2
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 LNUM,EVTYPE,DX,DY
      INTEGER*2 NUM,SCRPOS(2),RECT(2,2)
      LOGICAL TEMP,OBS,LWAIT,UNOBSCURED,FIRST,PLACE,OK
      CHARACTER EVDATA*1
      INTEGER*4 I,ST
      REAL WX1,WY1,WX2,WY2
C
C     Do a graphics move in case he quits in the middlwe of things
      DX = VIEWEX(3,NUM)-VIEWEX(1,NUM)
      DY = VIEWEX(4,NUM)-VIEWEX(2,NUM)
C 
      BACK =0
      FIRST = .TRUE.
      PLACE = .FALSE.
C     Make sure backing bitmap not updated
      CALL GPR_$SET_AUTO_UPDATE(.FALSE.,ST)
      CALL ROPXOR()
C      CALL HFONT(.TRUE.)
      RECT(1,1)=VPXMIN
      RECT(2,1)=VPYMIN
      RECT(1,2)=VPXMAX-VPXMIN
      RECT(2,2)=VPYMAX-VPYMIN
      CALL GPR_$SET_CLIP_WINDOW(RECT,ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,ST)
1000  CONTINUE
      LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
      IF(EVTYPE.EQ.GPR_$LOCATOR) THEN
 
          XP1 = SCRPOS(1)
          YP1 = SCRPOS(2)
          XP2 = XP1 + DX
          YP2 = YP1 + DY
          IF(.NOT.FIRST) THEN
C             draw out old box if not first
              CALL GPR_$DRAW_BOX(OXP1,OYP1,OXP2,OYP2,ST)
          ENDIF
          FIRST = .FALSE.
          CALL GPR_$DRAW_BOX(XP1,YP1,XP2,YP2,ST)
          OXP1 = XP1
          OYP1 = YP1
          OXP2 = XP2
          OYP2 = YP2
      ELSE IF(EVTYPE.EQ.GPR_$BUTTONS) THEN
          CALL FOLDUP(EVDATA)
          IF(EVDATA.EQ.'C'.OR.EVDATA.EQ.'B') THEN
C             Fella does not want to move the port
              CALL GPR_$DRAW_BOX(OXP1,OYP1,OXP2,OYP2,ST)
C             no more clipping
              CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
C             go back
              GOTO 200
          ELSEIF(EVDATA.EQ.'A') THEN
              PLACE = .TRUE.
          ENDIF
      ENDIF
C     loopround until heplaces it
      IF(.NOT.PLACE) GOTO 1000
C     put window to back of list.
C     paint in the background and the paint all forward
C     add and make top of list. here we go
C     draw out the old box
      CALL GPR_$DRAW_BOX(OXP1,OYP1,OXP2,OYP2,ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
C     reset drawing conditions
      CALL GPR_$SET_AUTO_UPDATE(.TRUE.,ST)
      CALL ROPREP()
C      CALL HFONT(.FALSE.)
C     loopround and put this
      DO 10 I=1,MAXVP
C         swapping list if window is unobscured
          IF(VPNUM(I).AND.I.NE.NUM) THEN
C             if the window is allready obs then leave it
              IF(OBSLST(I,NUM)) THEN
                 TEMP = OBSLST(I,NUM)
                 OBSLST(I,NUM) =OBSLST(NUM,I)
                 OBSLST(NUM,I) =TEMP
              ENDIF
           ENDIF
 10   CONTINUE
C     time to build and paint asccoiated windows
      CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     paint in the back ground please
      CALL WINDOW_PAINT(-NUM)
C     set new window size
      VIEWEX(1,NUM) = OXP1
      VIEWEX(2,NUM) = OYP1
      VIEWEX(3,NUM) = OXP2
      VIEWEX(4,NUM) = OYP2
C     modify existing postion if outside limits
      CALL WINDOW_MODIFY(NUM,OK)
C     modify xoords
      IF(ICNUM(NUM)) THEN
          VXMIN = VIEWEX(1,NUM)
          VYMIN = VIEWEX(2,NUM)
          VXMAX = VIEWEX(3,NUM)
          VYMAX = VIEWEX(4,NUM)
C         set global icon data
          ICONEX(1,NUM) = VIEWEX(1,NUM)
          ICONEX(2,NUM) = VIEWEX(2,NUM)
          ICONEX(3,NUM) = VIEWEX(3,NUM)
          ICONEX(4,NUM) = VIEWEX(4,NUM)
      ELSE
C         set window
          VXMIN = VIEWEX(1,NUM)
          VYMIN = VIEWEX(2,NUM)+VPBANN
          VXMAX = VIEWEX(3,NUM)
          VYMAX = VIEWEX(4,NUM)
C         set world coords
          WX1 = VIEWPS(1,NUM+1)
          WY1 = VIEWPS(2,NUM+1)
          WX2 = VIEWPS(3,NUM+1)
          WY2 = VIEWPS(4,NUM+1)
C         reset world coords to main
          CALL WORLD(WX1,WY1,WX2,WY2)
      ENDIF
C
C     do the the other windows that were obscured
      DO 30 I=2,LNUM
           CALL WINDOW_PAINT(LIST(2,I))
30    CONTINUE
C     ok bring the new windo forward and paint it in
      CALL WINDOW_PAINT(NUM)
      CALL WINDOW_ADD(NUM)
      DO 40 I=1,MAXVP
C         swapping list if window is unobscured
          IF(VPNUM(I).AND.I.NE.NUM) THEN
               OBSLST(NUM,I) = .FALSE.
          ENDIF
 40   CONTINUE
C     reset all here before exiting
200   CONTINUE
      CALL ROPREP()
      CALL HFONT(.FALSE.)
C     save the new worls transform
      IF(ICNUM(CVPN)) THEN
C         make sure we do not keep icon highlight
          CALL WINDOW_SET(IX,IY,.TRUE.)
      ELSE
          CALL TRNSAV(CVPN)
      ENDIF
      END
 
 
C      @(#)  256.1 date 12/16/89 window_obs_list.ftn 
      SUBROUTINE WINDOW_OBS_LIST(NUM,LIST)
C     ====================================
C1    VARTYPE                    I2   I4
C1    IOSTST                     I    O
C1
C2    Returns a list of windows which are obscured by the window sent
      include   'include/viewport.inc'
C
      INTEGER*2 NUM
      LOGICAL LIST(MAXVP),VISABL,PNTBOX
      INTEGER*4 XMAX,YMAX,XMIN,YMIN,I,CODE
      INTEGER X1,Y1,X2,Y2
      EXTERNAL PNTBOX
C
C
      DO 10 I=1,MAXVP
C
          LIST(I) = .FALSE.
          IF(VPNUM(I)) THEN
C            get extents of view list
             XMIN = VIEWEX(1,NUM)
             YMIN = VIEWEX(2,NUM)
             XMAX = VIEWEX(3,NUM)
             YMAX = VIEWEX(4,NUM)
             X1 = VIEWEX(1,I)
             Y1 = VIEWEX(2,I)
             X2 = VIEWEX(3,I)
             Y2 = VIEWEX(4,I)
             CALL VCLIP(X1,Y1,X2,Y2,XMIN,YMIN,XMAX,YMAX,VISABL,CODE)
             IF(VISABL) LIST(I) = .TRUE.
             X1 = VIEWEX(1,I)
             Y1 = VIEWEX(4,I)
             X2 = VIEWEX(3,I)
             Y2 = VIEWEX(2,I)
             CALL VCLIP(X1,Y1,X2,Y2,XMIN,YMIN,XMAX,YMAX,VISABL,CODE)
             IF(VISABL) LIST(I) = .TRUE.
             X1 = VIEWEX(1,I)
             Y1 = VIEWEX(2,I)
             X2 = VIEWEX(3,I)
             Y2 = VIEWEX(4,I)
             CALL VCLIP(XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,VISABL,CODE)
             IF(VISABL) LIST(I) = .TRUE.
             XMIN = VIEWEX(1,NUM)
             YMIN = VIEWEX(4,NUM)
             XMAX = VIEWEX(3,NUM)
             YMAX = VIEWEX(2,NUM)
             CALL VCLIP(XMIN,YMIN,XMAX,YMAX,X1,Y1,X2,Y2,VISABL,CODE)
             IF(VISABL) LIST(I) = .TRUE.
C
          ENDIF
 10   CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 window_paint.ftn 
      SUBROUTINE WINDOW_PAINT(NUM)
C     ===========================
C1    VARTYPE                 I2
C1    IOSTAT                  I
C
C2    paint a window clear or with contents If the
C2    argument is negative then paint in the background instead of
C2    the window contents. This could be the whole screen or just a bit of it
C2    its up to you
C
      include   'include/viewport.inc'
      include   'include/icon.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 XP1,YP1,XP2,YP2,NUM
      INTEGER*2 CURPEN
      CHARACTER*400 STRING
      LOGICAL BACK,OK
C
C     save current pen color
      CURPEN = COLPEN
C     draw in as white or somthin
      CALL SETDRW(COLFOR)
C
      IF(ICNUM(ABS(NUM)) ) THEN
          CALL ICON_PAINT(NUM)
          GOTO 999
      ENDIF
      BACK = .FALSE.
      IF(NUM.LE.0) THEN
          BACK = .TRUE.
          NUM = ABS(NUM)
      ENDIF
      XP1 = VIEWEX(1,NUM)
      YP1 = VIEWEX(2,NUM)
      XP2 = VIEWEX(3,NUM)
      YP2 = VIEWEX(4,NUM)
      IF(BACK) THEN
C         draw the background of the window portion only
          CALL DRAW_BACK(NUM)
      ELSE
C         set size of origin window
          WINBLT(1) = 0
          WINBLT(2) = 0
          WINBLT(3) = VIEWPO(1,NUM)+1
          WINBLT(4) = VIEWPO(2,NUM)+1
          ORIGIN(1) = VIEWEX(1,NUM)
          ORIGIN(2) = VIEWEX(2,NUM)
          CALL GET_BITMAP(NUM,OK)
C         draw the border and asccoaited text
          CALL WINDOW_BORDER(NUM)
          CALL WINDOW_TEXT(NUM,STRING)
          OK = NUM.EQ.CVPN
          CALL WINDOW_BANNER(NUM,STRING,OK)
      ENDIF
999   CONTINUE
C     reset pen color please
      CALL SETDRW(CURPEN)

      END
C
C      @(#)  256.1 date 12/16/89 window_pop.ftn 
      SUBROUTINE WINDOW_POP(NUM)
C     ============================
C1    VARTYPE                  I2
C1    IOSTST                   I
C
C1
C2    Viewport brought to front of list
C2    This will make the window the most current if
C2    obscured by anything. If unobscured then
C2    the window must be popped to the back of the obscured list
C2
      include   'include/viewport.inc'
C
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 LNUM
      INTEGER*2 NUM
      LOGICAL TEMP,OBS
      INTEGER I
C
C     is this window obscured by anything
      OBS = .FALSE.
      DO 20 I=1,MAXVP
          IF(OBSLST(NUM,I).AND.I.NE.NUM) THEN
C             yes it is so pop it to the front
              OBS = .TRUE.
          ENDIF
 20   CONTINUE
C     obscured
      IF(OBS) THEN
          CALL WINDOW_PAINT(NUM)
          CALL WINDOW_ADD(NUM)
          DO 40 I=1,MAXVP
C
C             swapping list if window is unobscured
              IF(VPNUM(I).AND.I.NE.NUM) THEN
                  IF(OBSLST(I,NUM)) THEN
                      OBSLST(NUM,I) = .FALSE.
                  ENDIF
               ENDIF
 40       CONTINUE
      ELSE
C         unobscured
          DO 10 I=1,MAXVP
C
C             swapping list if window is unobscured
              IF(VPNUM(I).AND.I.NE.NUM) THEN
                  TEMP = OBSLST(I,NUM)
                  OBSLST(I,NUM) =OBSLST(NUM,I)
                  OBSLST(NUM,I) =TEMP
               ENDIF
 10       CONTINUE
C         time to build and paint asccoiated windows
          CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
          DO 30 I=1,LNUM
              CALL WINDOW_PAINT(LIST(2,I))
30        CONTINUE
      ENDIF
C
      END
C
C
C       @(#)  256.1 date 12/16/89 window_resize.ftn 
      SUBROUTINE WINDOW_RESIZE(NUM,OK)
C     ========================================
C1    VARTYPE                   I2  I2  I2 L
C1    IOSTST                    I   O   O  O
C
C2    This routine will allow the user to resize the window
C2    it does this by a rubberband around the current window and
C2    when set it will free the bitmap and realocate doing all the clever things
C
      include  'include/viewport.inc'
      include  'include/vnames.inc'
      include  'include/wtov.inc'
      include  'include/icon.inc'
C
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 LNUM
      INTEGER*2 IX,IY,NUM
      INTEGER*4 C,J,ANCH(4),K,SORCE,I
      INTEGER*4 ST
      REAL XC,YC,SX1,SY1,SX2,SY2,WX1,WY1,WX2,WY2
      REAL XP(4),YP(4),DISTXY
      REAL D(4),DIST
      CHARACTER HITKEY*1
      LOGICAL OK,TEMP
      EXTERNAL DISTXY
 
      DATA ANCH/3,4,1,2/
C
C     icons not wanted here
      IF(ICNUM(NUM)) RETURN
      CALL DCPRNT(730)
C
      CALL SCURS(C,XC,YC)
C     find out hook point and anchor
      IX = XC
      IY = YC
C     four point in question
      XP(1) = VIEWEX(1,NUM)-1
      YP(1) = VIEWEX(2,NUM)-1
      XP(2) = VIEWEX(3,NUM)+1
      YP(2) = VIEWEX(2,NUM)-1
      XP(3) = VIEWEX(3,NUM)+1
      YP(3) = VIEWEX(4,NUM)+1
      XP(4) = VIEWEX(1,NUM)-1
      YP(4) = VIEWEX(4,NUM)+1
      D(1) = DISTXY(XC,YC,XP(1),YP(1))
      D(2) = DISTXY(XC,YC,XP(2),YP(2))
      D(3) = DISTXY(XC,YC,XP(3),YP(3))
      D(4) = DISTXY(XC,YC,XP(4),YP(4))
C     find the closest point
      DIST = 1E20
      DO 10 I=1,4
          IF(D(I).LT.DIST) THEN
              DIST = D(I)
              J = I
          ENDIF
 10   CONTINUE
C
      K  = ANCH(J)
      SX1 = XP(K)
      SY1 = YP(K)
      SX2 = IX
      SY2 = IY
C
C     find out where we are
      OK = .TRUE.
      SORCE = 10
      CALL APRREC(SX1,SY1,SX2,SY2,HITKEY,SORCE,OK)
      IF(.NOT.OK) GOTO 900
      DO 20 I=1,MAXVP
C         swapping list if window is unobscured
          IF(VPNUM(I).AND.I.NE.NUM) THEN
C             if the window is allready obs then leave it
              IF(OBSLST(I,NUM)) THEN
                 TEMP = OBSLST(I,NUM)
                 OBSLST(I,NUM) =OBSLST(NUM,I)
                 OBSLST(NUM,I) =TEMP
              ENDIF
           ENDIF
 20   CONTINUE
C     time to build and paint asccoiated windows
      CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     paint in the back ground please
      CALL WINDOW_PAINT(-NUM)
C     do the the other windows that were obscured
      DO 30 I=2,LNUM
           CALL WINDOW_PAINT(LIST(2,I))
30    CONTINUE
 
C     get current world coords
C      CALL SC2WO(SX1,SY2,WX1,WY1)
C      CALL SC2WO(SX2,SY1,WX2,WY2)
C     store window extents
      VIEWEX(1,NUM) = SX1
      VIEWEX(2,NUM) = SY1
      VIEWEX(3,NUM) = SX2
      VIEWEX(4,NUM) = SY2
C     store world extents
C      VIEWPS(1,NUM+1) = WX1
C      VIEWPS(2,NUM+1) = WY1
C      VIEWPS(3,NUM+1) = WX2
C      VIEWPS(4,NUM+1) = WY2
C
C     store current extetnts
      WXMIN = VIEWPS(1,NUM+1)
      WYMIN = VIEWPS(2,NUM+1)
      WXMAX = VIEWPS(3,NUM+1)
      WYMAX = VIEWPS(4,NUM+1)
C     set size of bitmap includeing size for border
      VIEWPO(1,NUM) = SX2-SX1
      VIEWPO(2,NUM) = SY2-SY1
C     Modify window postion
      CALL WINDOW_MODIFY(NUM,OK)
C     deactivate this viewport
      IF(.NOT.OK) THEN
C         error trap
          VPNAME(NUM) = VPUDEF
          CALL DEPRNT(731)
      ENDIF
      DO 50 I=1,MAXVP
          OBSLST(I,NUM) = .FALSE.
          OBSLST(NUM,I) = .FALSE.
 50   CONTINUE
C
900   CONTINUE
 
      END
C
C      @(#)  256.1 date 12/16/89 window_set.ftn 
      SUBROUTINE WINDOW_SET(IX,IY,OVER)
C     =================================
C1    VARTYPE               I4 I4  L
C1    IOSTAT                I  I   I
C
C2    Function sets current window depeding on the world coords
C2    If override OVER is true then backcloth becomes active
C
      include 'include/masti.inc'
      include 'include/viewport.inc'
      include 'include/icon.inc'
      include 'include/wtov.inc'
      include 'include/layer.inc'
      include 'include/menun.inc'
      include 'include/vpgrid.inc'
C
      LOGICAL OVER
      INTEGER*4 IX,IY,I,ST,I4
      INTEGER*2 OLDVP,RECT(2,2)
      REAL SX1,SX2,SY1,SY2
      REAL WX1,WX2,WY1,WY2
      REAL SX,SY
      CHARACTER*20 WIN
      LOGICAL OK,POP
C
C     if MAWS active no need to detrmine window so go  home
      IF(MAWS) RETURN
      CALL WNHIT(REAL(IX),REAL(IY),OK)
      IF(OK.OR.OVER) THEN
C         get current window setting
C         find out the window
          IF(.NOT.OVER) THEN
C             check set override
              OLDVP = CVPN
              CALL WINDOW_DETERMINE(IX,IY)
              PVPN = OLDVP
          ELSE
C             automaticall y set the viewport to backcloth
              PVPN = CVPN
              CVPN = 0
          ENDIF
C         no need to set if no change
C         save current cursor postion
          IF(PVPN.NE.CVPN) CALL WINDOW_ACTIVE()
          IF(CVPN.EQ.0) THEN
C             main window
              VXMIN = VPXMIN
              VYMIN = VPYMIN
              VXMAX = VPXMAX
              VYMAX = VPYMAX
              WX1 = VIEWPS(1,1)
              WY1 = VIEWPS(2,1)
              WX2 = VIEWPS(3,1)
              WY2 = VIEWPS(4,1)
C             reset viewing transform
              CALL WORLD(WX1,WY1,WX2,WY2)
C             retrieve the main layer
          ELSEIF(CVPN.LE.MAXVP) THEN
C             set window coords
              VXMIN = VIEWEX(1,CVPN)
              VYMIN = VIEWEX(2,CVPN)+VPBANN
              VXMAX = VIEWEX(3,CVPN)
              VYMAX = VIEWEX(4,CVPN)
              WX1 = VIEWPS(1,CVPN+1)
              WY1 = VIEWPS(2,CVPN+1)
              WX2 = VIEWPS(3,CVPN+1)
              WY2 = VIEWPS(4,CVPN+1)
C
              IF(.NOT.ICNUM(CVPN)) CALL WORLD(WX1,WY1,WX2,WY2)
C             find out where we are and pop this baby to the front
              POP = .FALSE.
              DO 10 I=1,MAXVP
C                 read obscured list
                  IF(OBSLST(CVPN,I).AND.I.NE.CVPN) THEN
                      POP = .TRUE.
                  ENDIF
10            CONTINUE
              IF(POP) THEN
C                 popit
                  CALL WINDOW_POP(CVPN)
              ENDIF
C             reset layer conditions
          ELSE
              CALL DEPRNT(735)
          ENDIF
C         Set clip window
          RECT(1,1)=VXMIN+1
          RECT(2,1)=VYMIN+1
          RECT(1,2)=VXMAX-VXMIN-1
          RECT(2,2)=VYMAX-VYMIN-1
 
C         do a test for the a viewport an use the banmner
          CALL GPR_$SET_CLIP_WINDOW(RECT,ST)
C         make the current display file point to the bitmaps
C         get layering for new viepwort
          CALL GETLAY(CVPN,OK)
C         set up grid conditions
          CALL GETGRD(CVPN,OK)
C         load up the work layer and name if required
          IF(.NOT.DISLAY.AND.DRAWNG) THEN
              I4=CLAYER
              CALL GTDMWI(60,4,I4)
C             write name of layer directly.
              IF(LOADED) THEN
                  CALL GTDMEN(336,4)
              ELSE
                  CALL GTPMEN(LNAME(CLAYER),'N',4,2)
              ENDIF
           ENDIF
C
      ELSE
C         make sure transform is reset to correct worlds displays
          IF(CVPN.EQ.0) THEN
C             main window
              VXMIN = VPXMIN
              VYMIN = VPYMIN
              VXMAX = VPXMAX
              VYMAX = VPYMAX
              WX1 = VIEWPS(1,1)
              WY1 = VIEWPS(2,1)
              WX2 = VIEWPS(3,1)
              WY2 = VIEWPS(4,1)
C             reset viewing transform
              CALL WORLD(WX1,WY1,WX2,WY2)
C             retrieve the main layer
          ELSEIF(CVPN.LE.MAXVP) THEN
C             set window coords
              VXMIN = VIEWEX(1,CVPN)
              VYMIN = VIEWEX(2,CVPN)+VPBANN
              VXMAX = VIEWEX(3,CVPN)
              VYMAX = VIEWEX(4,CVPN)
              WX1 = VIEWPS(1,CVPN+1)
              WY1 = VIEWPS(2,CVPN+1)
              WX2 = VIEWPS(3,CVPN+1)
              WY2 = VIEWPS(4,CVPN+1)
C
              CALL WORLD(WX1,WY1,WX2,WY2)
          ENDIF
      ENDIF
C
      END
C
C      @(#)  256.1 date 12/16/89 window_text.ftn 
      SUBROUTINE WINDOW_TEXT(NUM,STRING)
C     =================================
C1    VARTYPE                 I2   C**
C1    IOSTS                    I   O
C
C2    This routine will try to create a text string based on the
C2    viewport size and current work layer
C
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/wtov.inc'
      include 'include/masti.inc'
      include 'include/icon.inc'
C
C
      INTEGER*4 LENGTH
      INTEGER*4 NLEN,LG
      INTEGER*2 NUM,MAXNOC,TEXTW,S1,S2,CLAY,S3
      CHARACTER*(*) STRING,LAYER*80,NAME*80,GRDWRD*4,GRIDS*10
C
      EXTERNAL NLEN
C
 
      IF(ICNUM(NUM)) THEN
C         Icon up no point in getting text
          GOTO 999
      ENDIF
      IF(VPGRID(NUM)) THEN
          GRDWRD = 'ON'
      ELSE
          GRDWRD = 'OFF'
      ENDIF
C     we know the size of the of the text is 7 for vt100s
      CLAY = VPLAY(1,NUM)
      TEXTW = 9
C     Create the max number of characters in this window
      MAXNOC  = VIEWPO(1,NUM)/TEXTW
C     Create identifier for the window
      LENGTH = NLEN(VPNAME(NUM))
      IF(MAXNOC.GT.30 + LENGTH) THEN
C
          IF(LENGTH.GT.1) THEN
              WRITE(UNIT=NAME,FMT='(A,I2,2A)') 
     +         'Port',NUM,' ',VPNAME(NUM)
          ELSE
              WRITE(UNIT=NAME,FMT='(A,I2)') 'Port',NUM
          ENDIF
          WRITE(UNIT=LAYER,FMT='(A,I3)')  'Work Layer:',CLAY
          WRITE(UNIT=GRIDS,FMT='(2A)')  'GRID ',GRDWRD
      ELSE
          WRITE(UNIT=NAME,FMT='(A,I2)') 'VP',NUM
          WRITE(UNIT=LAYER,FMT='(A,I3)')  'WL:',CLAY
          IF(VPGRID(NUM)) THEN
              WRITE(UNIT=GRIDS,FMT='(A)')  'G'
          ELSE
              WRITE(UNIT=GRIDS,FMT='(A)')  'O'
          ENDIF

      ENDIF
C     ok now put result into string by calculating subscripts
      S1  = 1
      S2 = MAXNOC-NLEN(LAYER)
      LG = NLEN(GRIDS)
      S3 = MAXNOC/2-(LG/2)
      STRING(S1:) = NAME(:NLEN(NAME))
      STRING(S3:) = GRIDS(:NLEN(GRIDS))
      STRING(S2:) = LAYER(:NLEN(LAYER))
C      WRITE(UNIT=STRING(S1:),FMT='(A)') NAME(:NLEN(NAME))
C      WRITE(UNIT=STRING(S3:),FMT='(A)') GRIDS(:NLEN(GRIDS))
C      WRITE(UNIT=STRING(S2:),FMT='(A)') LAYER(:NLEN(LAYER))
999   CONTINUE
      END
C
C
C      @(#)  256.1 date 12/16/89 wnhit.ftn 
      SUBROUTINE WNHIT(SX,SY,OK)
C     ===========================
C1    vartype           R  R  L
C1    iostatus          I  I  O
C2
C     used for the main view at all times
C
      include 'include/wtov.inc'
      include 'include/viewport.inc'
C
      REAL SX,SY
      LOGICAL OK
C
CAPOLLO|SUN
      OK= SX .LE. VPXMAX .AND. SX .GE. VPXMIN .AND.
     +     SY .LE. VPYMAX .AND. SY .GE. VPYMIN
CAPOLLO|SUN
CIBM|PC386
C      OK= SX .LE. DXMAX .AND. SX .GE. DXMIN .AND.
C     +     SY .LE. DYMAX .AND. SY .GE. DYMIN
CIBM|PC386
C
      END
C
C-----------------------------------------------------------------
C      @(#)  256.1 date 12/16/89 wnpnta.ftn 
      SUBROUTINE WNPNTA()
 
      include 'include/viewport.inc'

      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 NUM,I,LNUM,VPN
      LOGICAL OK
 
      NUM = 0
      VPN = 0

C     get all windows hopefully
      IF(MAWS.OR.CVPN.GT.0.OR..NOT.MVPACT ) RETURN
      CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     do the the other windows that were obscured
      IF(LNUM.GT.0) THEN
C         save the current configuration
C         set window coords
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
          CALL SAVE_BITMAP(vpn,OK)
      ENDIF
      DO 30 I=1,LNUM
          CALL WINDOW_PAINT(LIST(2,I))
30    CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 wnpntz.ftn 
      SUBROUTINE WNPNTZ()
C
C     This routine does the same as WNPNTA but also draws the
C     grid in if it has been saved with the viewport
C
      include 'include/wtov.inc'
      include 'include/ndata.inc'
      include 'include/viewport.inc'
C
C
 
      INTEGER*2 LIST(2,MAXVP)
      INTEGER*2 NUM,I,LNUM,VPN,OLDC
      LOGICAL OK
      REAL X,Y
 
      NUM = 0
      VPN = 0
C     get all windows hopefully
      IF(MAWS.OR.CVPN.GT.0) RETURN
      CALL BUILD_PAINT_LIST(NUM,LIST,LNUM)
C     do the the other windows that were obscured
      IF(LNUM.GT.0) THEN
C         save the current configuration
C         set window coords
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
          CALL SAVE_BITMAP(vpn,OK)
      ENDIF
C     Save CVPN to be on the safe side
      OLDC = CVPN
C     Save the current grid values for viewport CVPN
      CALL SAVGRD(CVPN,OK)
      DO 30 I=1,LNUM
          CALL WINDOW_PAINT(LIST(2,I))
C         Redo the grid if required
          IF (VPGRID(I)) THEN
              CVPN = I
C             restore transform
              CALL TRNRST(I)
C             restore limits
              VXMIN = VIEWEX(1,I)
              VYMIN = VIEWEX(2,I)
              VXMAX = VIEWEX(3,I)
              VYMAX = VIEWEX(4,I)
C             Restore the current grid values for this viewport
              CALL GETGRD(I,OK)
              SETGRD = .TRUE.
              CALL GRID1()
              VPGRID(I) = SETGRD
              X = 0
              Y = 0
              CALL SETDP(X,Y,.TRUE.)
          ENDIF
30    CONTINUE
C     Restore CVPN back to what it was
      CVPN = OLDC
C     Restore the current grid values for viewport CVPN
      CALL GETGRD(CVPN,OK)
      END
C
C      @(#)  256.1 date 12/16/89 wnsvbt.ftn 
      SUBROUTINE WNSVBT()
C     ===================
C1    NO ARGS
C
C2    Routine will save the current viewport on the screen to its
C2    own bitmap
C
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      LOGICAL OK
      CHARACTER*400 STRING
C
      IF ( .NOT.MVPACT ) THEN
          RETURN
      ENDIF
      IF(CVPN.EQ.0) THEN
C         save the back cloth
          CALL ROPREP()
C         save current configuration
          CALL TRNDP()
C
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
C         save the current screen full of data
          CALL SAVE_BITMAP(INT2(0),OK)
      ELSE
 
C         make sure all transforms correct
          CALL ROPREP()
C         save current configuration
          CALL TRNDP()
C         we must save this portion of the bitmap into our viewport
          WINBLT(1) = VIEWEX(1,CVPN)
          WINBLT(2) = VIEWEX(2,CVPN)
          WINBLT(3) = VIEWPO(1,CVPN)+1
          WINBLT(4) = VIEWPO(2,CVPN)+1
          ORIGIN(1) = 0
          ORIGIN(2) = 0
          CALL SAVE_BITMAP(CVPN,OK)
C         Repaint the window cos its been drawn
          CALL WINDOW_BORDER(CVPN)
          CALL WINDOW_TEXT(CVPN,STRING)
          CALL WINDOW_BANNER(CVPN,STRING,.TRUE.)
      ENDIF
C
      END
C      @(#)  256.1 date 12/16/89 wntctl.ftn 
      SUBROUTINE WNTCTL()
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
      LOGICAL OK,YESOK,CVERFY,WINDOW
      INTEGER*2 C,VPN,I2X,I2Y,KEY
      INTEGER*4 CC,IX,IY,TMEN,TCELL
      INTEGER*2 WINDOW_DETERMINE
C
C
C     set return flag
      VPN = 0
      OK = .FALSE.
      WINDOW = .FALSE.
C
C     view requested get a hit
 
      IF(CVERFY(CCMD,'abcdef')) THEN
          CALL BLTVPT()
C         reset menu system
          VPDIS = .FALSE.
          CALL MNIDIS()
          OK = .TRUE.
          RETURN
      ENDIF
C
      IF(CCMD.EQ.'g')  THEN
 
          CALL GTSAVW(1)
 10       CONTINUE
C         get a view number
          CALL DCPRNT(509)
C         get a valid key
          TMEN= MEN
          TCELL= CELLN
          CALL SCURS(CC,X,Y)
          IX = X
          IY = Y
C         get the correct window types
          IF(SKEY.EQ.77) THEN
C            move the window
             CALL WINDOW_MOVE(CVPN)
             RETURN
          ELSEIF(SKEY.EQ.88) THEN
C            move the window
             CALL WINDOW_POP(CVPN)
             RETURN
          ENDIF
          CALL MENHIT(IX,IY,MEN,CELLN,CTEXT,CCMD,OK)
C
C
          IF(MEN.NE.4) THEN
              GOTO 999
          ENDIF
C
C         This boy has make a request to define a window view
          WINDOW = (SOURCE.EQ.1).AND.(SKEY.EQ.98)
          IF(CCMD.EQ.'a') THEN
              VPN = 1
          ELSE IF(CCMD.EQ.'b') THEN
              VPN = 2
          ELSE IF(CCMD.EQ.'c') THEN
              VPN = 3
          ELSE IF(CCMD.EQ.'d') THEN
              VPN = 4
          ELSE IF(CCMD.EQ.'e') THEN
              VPN = 5
          ELSE IF(CCMD.EQ.'f') THEN
              OK = .TRUE.
              GOTO 999
          ELSE IF(CCMD.EQ.'g') THEN
              OK = .TRUE.
              GOTO 999
          ELSE
              GOTO 999
          ENDIF
          IF(VPN.GT.0) THEN
C
C             call define handling routine
c              CALL DEFVP1(VPN,WINDOW,OK)
              CALL DEFWIN(OK)
              IF(.NOT.OK) THEN
                 CALL GTMCLO(MEN,CELLN)
                 GOTO 10
              ENDIF
C
          ENDIF
          OK = .TRUE.
          GOTO 999
      ENDIF
      OK = .FALSE.
      RETURN
 999  CONTINUE
C     restore prompt
      CALL GTMCLO(TMEN,TCELL)
C     important to set this first
      VPDIS = .FALSE.
      CALL MNIDIS()
      CALL GTRSTW(1)
C
      END
C
C----------------------------------------------------------------------
C      @(#)  256.1 date 12/16/89 wrtban.ftn 
      SUBROUTINE WRTBAN()
C     ===================
C1    NO ARGS
C
C2    This routne will rpaint the daxport banner into the current
C2    daxport to make it easier this routin may be called when
C2    MAWS is active and hence must return
C
      include  'include/viewport.inc'
      include  'include/vnames.inc'
      include  'include/wtov.inc'
C
      INTEGER*2 NUM
      CHARACTER*400 STRING
C
C     come back if not needed
C     IF(MAWS) RETURN
C     set viewport
      NUM = CVPN
      IF(CVPN.GT.0) THEN
          CALL WINDOW_TEXT(NUM,STRING)
          CALL WINDOW_BANNER(NUM,STRING,.TRUE.)
      ENDIF
      END
