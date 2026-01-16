C
C     @(#)  412.1 date 6/11/92 popup.f 
C
C
C     Filename    : popup.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:50
C     Last change : 92/06/11 14:38:15
C
C     Copyright : Practical Technology International Limited  
C     File :- popup.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE POPUP(MNCODE,CELL,ST)
C     SUBROUTINE POPUP_CELL(POPNUM,XP,YP,CELL,POS,BMWIND)
C     SUBROUTINE POPUP_CONFIG(ST)
C     SUBROUTINE POPUP_LOAD(CODE,OK)
C     SUBROUTINE POPUPPRELOAD(ARRAY,NELEM,ST)
C     SUBROUTINE POPUP_PAINT(MNCODE,X,Y,PAINT,REPAINT,ST)
C     SUBROUTINE TOOLS_RECT_INV(RECT,ST)
C     SUBROUTINE MAKE_POPUP(NOCELLS,ARRAY,BANNER,COLOR,
C     SUBROUTINE POPUP_ALLOCATE(POPNUM,ST)
C     SUBROUTINE POPUP_GET_USER(CELL,BUFF,INLEN,BUFLEN,ST)
C     SUBROUTINE POPUP_GET_TEXT(POPNUM,CELL,BUFF,INLEN,BUFLEN,ST)
C     SUBROUTINE FILE_SCR_ARROW(X,Y,SIZE,UP)
C     SUBROUTINE POPUP_FILE_PAINT(POPNUM,X,Y,ST)
C     SUBROUTINE FILE_MARKER(POPNUM,ST)
C     SUBROUTINE STRIP_FILE(FILNAM,ST)
C     SUBROUTINE INQ_FILE_TARGET(POPNUM,TARGET,INLEN,TLEN,ST)
C     SUBROUTINE SET_FILE_TARGET(POPNUM,TARGET,INLEN,ST)
C     SUBROUTINE INQ_FILE_WILD(POPNUM,WILDC,INLEN,TLEN,ST)
C     SUBROUTINE SET_FILE_WILDC(POPNUM,WILDC,INLEN,ST)
C     SUBROUTINE POPUP_HIGHLIGHT(MNCODE,TOKEN,CELL,OK)
C     SUBROUTINE POPUP_LOCK(ST)
C     SUBROUTINE SET_POPUP_FILE(POPNUM,STRING,BUFLEN,ST)
C     SUBROUTINE GET_FILE_CELL(POPNUM,CELL,BUFF,BUFLEN,ST)
C     SUBROUTINE POPUP_GET_FILE(POPNUM,CELL,BUFF,INLEN,BUFLEN,ST)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE POPUP(MNCODE,CELL,ST)
C     ================================
C1    VARTYPE            I4     I4  L
C1    IOSTAT             I       O  O
C
C2    This rotine will control the PUPUP menus from DAXCAD.
C2    called from TCURS when a MULTIPLE cell is called and
C2    returns the TOKEN from the POPPED cell
C
      include  'include/popup.inc'
C
      LOGICAL OK
      INTEGER*2  XP,YP
      INTEGER*4 MNCODE,TCELL,TMEN,ST,CELL
      CHARACTER TCCMD*1,TOKEN*1
      EXTERNAL WRTJRN
C
C     reset file selection flag
      POPLST = .FALSE.
C     save the current hit values
C
C     range check
      ST =0
      IF(MNCODE.LT.1.OR.MNCODE.GT.MAXPOP) THEN
          ST = 1
          RETURN
      ENDIF
C     load up the popup and wait for a return
      IF(.NOT.POPACT(MNCODE)) THEN
          ST = 1
          RETURN
      ENDIF
      CALL POPUP_LOAD(MNCODE,OK)
      IF(OK)THEN
C         User definable cells possible
          POPUP_CURRENT = MNCODE
          CALL POPUP_PAINT(MNCODE,XP,YP,.TRUE.,.FALSE.,ST)
C         get menu hit
          CALL CURSOR_ON(.TRUE.)
          CALL POPUP_HIGHLIGHT(MNCODE,TOKEN,CELL,OK)
          CALL CURSOR_ON(.FALSE.)
          IF(OK) THEN
              ST = 0
          ELSE
              ST = 1
          ENDIF
          CALL POPUP_PAINT(MNCODE,XP,YP,.FALSE.,.FALSE.,ST)
          POPUP_CURRENT = 0
          PRELOAD = .FALSE.
      ENDIF
C     decremnt cell number
      CELL = CELL - 1
      IF(CELL.LT.0) THEN
          CELL = 0
      ENDIF
 
      END
C
      SUBROUTINE POPUP_CELL(POPNUM,XP,YP,CELL,POS,BMWIND)
C     ===========================================
C1    VARTYPE                 I4    I2 I2  I4 I2(2) I2(4)
C1    STATUS                  I     I   I  1/O   O     I
C
C2    Fuctions return the cell number of the current menu
C2    based on a current postion passed
C
      include   'include/popup.inc'
      include   'include/edit.inc'       
C
      REAL FACTX,FACTY
      INTEGER*2 XP,YP,TEMPX,TEMPY,POS(2),COLS,ROWS
      INTEGER*2 BMWIND(4),SP
      INTEGER*4 CELL,MAXH,ST
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,SX,SY,I
      INTEGER*4 TYPE
      INTEGER*4 POPNUM
      INTEGER*4 COUNT
      CHARACTER*60 TEXT
      LOGICAL PNTBOX
C
      EXTERNAL PNTBOX
C
      TYPE = POPTYP(POPNUM)
      TEXT = DAXMNU(1)
      TEXTH = POPHGT(1)
C     get starting postion
      SX = STARTX
      SY = STARTY  + TGAP +TEXTH +2*LNSPC
C     set increment
      IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
          COUNT = MNTOT+MIN(POPFIL,MAXFIL)
      ELSE
          COUNT = MNTOT
      ENDIF
C     
      DO 10 I=2,COUNT
          TEXTH = POPHGT(I)
          XMIN = SX
          YMIN = SY
          XMAX = XMIN + BMWIND(3)
          YMAX = YMIN + TEXTH+6
          IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,INT4(XP),INT4(YP)) ) THEN
              GOTO 20
          ENDIF
          SY = SY + TEXTH + LNSPC
C
 10   CONTINUE
      RETURN
 20   CONTINUE
C     A simple, 1 column menu.
      CELL = I
      POS(1) = SX
      POS(2) = SY-LNSPC
      BMWIND(4) = TEXTH + 2*LNSPC
      END
C
      SUBROUTINE POPUP_CONFIG(ST)
C     ==========================
C1    VARTYPE                 I4
C1    IOSTAT                  O
C
C2    Initalise popups
C
      include  'include/popup.inc'
      INTEGER*4 ST,I
C
      DO 10 I=1,MAXPOP
          POPACT(I) = .FALSE.
 10   CONTINUE
C     file selection flag
      POPLST = .FALSE.
      POPBUF = ' '
      PRELOAD = .FALSE.
      FILPOS = 1
C     set varibles

      END
C
      SUBROUTINE POPUP_LOAD(CODE,OK)
C     ===============================
C1    VARTYPE                 I   L
C1    IOSTATUS                I4  O
C
C2    The MEnu loader based on CODE
C
      include  'include/popup.inc'
C
      LOGICAL OK
      INTEGER*4 CODE,START,END,BUFFER,NLEN,I,THKNUM
      INTEGER*2 MASK,ENDVAL,AND_2,CELS
C
      EXTERNAL NLEN,AND_2
C     reset totalisers


      IF( PRELOAD) THEN
C         popup allready preloaded
          OK = .TRUE.
          RETURN
      ENDIF
C
      MNTOT = 1
      OK = .FALSE.
C
      IF(CODE.GT.0) THEN
C 
         IF(CODE.LT.1.OR.CODE.GT.501) THEN
             RETURN
         ENDIF
 
C        *************************
C        The rest are normal menus
C        *************************
         START = MNPNTR(1,CODE)
         END = MNPNTR(2,CODE)
C
C        Load banner
         IF(NLEN(DAXBNR(CODE)).GT.0) THEN
             DAXMNU(MNTOT)=DAXBNR(CODE)
             DAXTOK(MNTOT)=' '
         ELSE
             DAXMNU(MNTOT)='DAXCADPopup'
             DAXTOK(MNTOT)=' '
         ENDIF
C         DO 10 MNSTK=START,END
CC
C             MNTOT=MNTOT+1
C 
C             DAXMNU(MNTOT)=VNOUNS(MNSTK)
C 
C10       CONTINUE
      ENDIF
      OK =.TRUE.
      END
C
C
C

      SUBROUTINE POPUPPRELOAD(ARRAY,NELEM,ST)
C     =======================================
C1    VARTYPE                 C*(*)   I4  I4
C1    IOSTATUS                  I     I   O
C
C2    Pre loader for popups 
C2  
C2  
C2    Arguments:-
C2  
C2    ARRAY       ->      An array of cells to be loaded.
C2    NELEM       ->      The number of elements in the list.
C2  
C2  
C2    Error Returns:
C2  
C2    1       ->      number of elements is out of range.
C2  
C2  
C
      include  'include/popup.inc'
C
      INTEGER*4 NELEM
      INTEGER*4 I
      INTEGER*4 ST
      CHARACTER*60  ARRAY(NELEM)

C
      EXTERNAL NLEN

      ST = 0
C
      IF(NELEM.EQ.0.OR.NELEM.GT.MAXCEL) THEN
          ST = 1
          GOTO 999
      ENDIF
C
C     reset totalisers
      MNTOT = 0
 
      DO 10 I=1,NELEM
C
          MNTOT=MNTOT+1
          DAXMNU(MNTOT)=ARRAY(I)
C 
10    CONTINUE
C
      PRELOAD = .TRUE.
C
999   CONTINUE
      END
C
C
      SUBROUTINE POPUP_PAINT(MNCODE,X,Y,PAINT,REPAINT,ST)
C     ===================================================
C1    VARTYPE                   I4   I2I2  L     L    I4
C1    IOSTATUS                  I    I I   I     I    O
C
C2    This rouitne will paint in the menu values from a defined
C2    array held in COMMON. The Menu can also be painteed out
C
      include  'include/popup.inc'
      include  'include/daxcolor.inc'
      include  'include/edit.inc'
      include  'include/shadow.inc'
      include  'include/dialog.inc'
      include  'include/bitmap.inc'
C
      CHARACTER STRING*256
      CHARACTER BUFF1*256
      CHARACTER BUFF2*256
      CHARACTER TYPED*20
      INTEGER*2 BMWIND(4),DISPOS(2)
      INTEGER*2 X,Y, NLEN2
      INTEGER*2 XMAX,YMAX,XMIN,YMIN
      INTEGER*2 COUNT,I,J,RLEN, TEMPX,TEMPY,XSTEP,YSTEP,PENCOL
      INTEGER*4 WINDOW(4)
      INTEGER*4 ORG(2)
      INTEGER*2 BOX(4)
      INTEGER*4 MNCODE,XP,YP,CURBM,ST,NLEN,LN,MDESC,COLR
      LOGICAL PAINT,OK
      LOGICAL REPAINT
      INTEGER*4 CURPAT
      INTEGER*4 COL,BCOL,MAXW,BANH
      INTEGER*4 TYPE
      INTEGER*4 TX,TY
      INTEGER*2 RECT(4)
      INTEGER*4 CURP(2)
      INTEGER*4 POPNUM
      INTEGER*2 SXP,SYP
      INTEGER*2 SIZE
      INTEGER*2 SCROLL
      INTEGER*4 TLEN
      INTEGER*4 BINDEX
      INTEGER*4 CURNUM
      INTEGER*4 LENGTH
      INTEGER*4 HEIGHT
      INTEGER*4 FONT
      INTEGER*4 SCXMAX 
      INTEGER*4 SCYMAX 
      INTEGER*4 SCXMIN 
      INTEGER*4 SCYMIN 
      INTEGER*4 F,D,L
      INTEGER*2 INT2
C
      EXTERNAL DRAWLS,PENCOL,NLEN2,NLEN,BINDEX
C
C     get current cursor postion
      IF(REPAINT) THEN
          CURP(1) = STARTX
          CURP(2) = STARTY
      ELSE
          CALL INQ_CURSOR(CURNUM,CURP,ST)
      ENDIF
C
      TYPE = POPTYP(MNCODE)
      XP = CURP(1)
      YP = CURP(2)
      SCXMAX = ROOTW(1)
      SCYMAX = ROOTW(2)
      SCXMIN = 0
      SCYMIN = 0
      MAXLN = 0
C 
      POPNUM = MNCODE
C     get the current limits of the array loaded
      DO 100 I =1,MNTOT
          LN = NLEN(DAXMNU(I))
          IF(LN.GT.MAXLN) MAXLN = LN
 100  CONTINUE
C
C     calculate size of new menu
C
      IF(.NOT.PAINT) THEN
C         NOTE: This is removing the menu and should be the second call
C         to POPUP_PAINT for this menu.
          IF ( POPUPLOCKED ) THEN
C             popup is locked into place
              RETURN
          ENDIF
          WINDOW(1) = 0
          WINDOW(2) = 0
          WINDOW(3) = BITMW
          WINDOW(4) = BITMH
          ORG(1) = STARTX-BLKBDR
          ORG(2) = STARTY-BLKBDR
          CALL ROPREP()
C         recover underneath
          CALL BITMAP_COPY(SWBITM,0,WINDOW,ORG,ST)
C         dealaocate bitmp
          CALL BITMAP_KILL(SWBITM,ST)
C         close and delete the search file
          IF(POPUNT.GT.0) THEN
             CLOSE(UNIT=POPUNT)
             CALL tmpcleanup(POPFID,ST)
          ENDIF
C         clear flags until next time
          POPFID = 0
          POPUNT = 0
C         clean temp files
          RETURN
      ENDIF
      IF(.NOT.REPAINT) THEN
C         allocate a new bitmap for swapping only if popup is not locked
          IF (POPUPLOCKED) THEN
              IF (POPFID.EQ.0) THEN
C                 allocate the popup
                  CALL POPUP_ALLOCATE(POPNUM,ST)
              ELSE
C                 set colour
                  CALL TOOLPEN(POPCOL(1,POPNUM) )
                  RETURN
              ENDIF
          ELSE
              CALL POPUP_ALLOCATE(POPNUM,ST)
          ENDIF
      ENDIF
C     set heights of popup
      BITMW = POPSIZ(1)
      BITMH = POPSIZ(2)
C
C     get a modified postion
      IF(XP.GT.SCXMAX-BITMW-VERGE) XP =SCXMAX-BITMW-VERGE
      IF(YP.GT.SCYMAX-BITMH-VERGE) YP =SCYMAX-BITMH-VERGE
      IF(XP.LT.VERGE) XP =VERGE
      IF(YP.LT.VERGE) YP =VERGE
C     paint out if requested
C
C     get current bitmap
C     XP and YP now contain the modified origin of the menu
C     set location and size of portion of main bitmap to be saved

      WINDOW(1) = XP-BLKBDR
      WINDOW(2) = YP-BLKBDR
      WINDOW(3) = BITMW
      WINDOW(4) = BITMH
      ORG(1) = 0
      ORG(2) = 0
C     save current portion of screen
      CALL BITMAP_COPY(0,SWBITM,WINDOW,ORG,ST)
C
C     set draw value for background filling
      CALL TOOLPEN(POPCOL(2,POPNUM) )
      RECT(1) = XP - 1
      RECT(2) = YP - 1
      RECT(3) = BITMW - (2*BLKBDR) - SHADOW
      RECT(4) = BITMH - (2*BLKBDR) - SHADOW
C
      BOX(1) = RECT(1)
      BOX(2) = RECT(2)
      BOX(3) = BOX(1)+RECT(3)
      BOX(4) = BOX(2)+RECT(4)
C     remember to clear the menu area SCREEN
C
      CALL TOOLS_RECTANGLE(RECT,ST)
C     set line round menu
C
      CALL TOOLPEN(POPCOL(1,POPNUM))
C
      IF((TYPE.EQ.2.OR.TYPE.EQ.3).AND.POPFIL.GT.MAXFIL) THEN
C         scroll bar section now
          XMIN = BOX(3)
          YMIN = BOX(2)
          XMAX = XMIN - POPBAR
          YMAX = BOX(4)
          CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
          XMIN = BOX(3)
          YMIN = BOX(2)
          XMAX = XMIN - POPBAR 
          YMAX = YMIN + POPBAR 
          CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
          XMIN = BOX(3)
          YMIN = BOX(4)
          XMAX = XMIN - POPBAR
          YMAX = YMIN - POPBAR  
C
          SXP = BOX(3) - 16
          SYP = BOX(4) - (POPBAR/2)
          SIZE = 2
          CALL FILE_SCR_ARROW(SXP,SYP,SIZE,.FALSE.)
          CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
          SXP = BOX(3) - 16
          SYP = BOX(2) + POPBAR/2
          SIZE = 2
          CALL FILE_SCR_ARROW(SXP,SYP,SIZE,.TRUE.)
      ENDIF                  
C
      DO 50 I=1,BLKBDR
         CALL TOOLS_BOX(BOX(1),BOX(2),BOX(3),BOX(4),ST )
         BOX(1)=BOX(1)-1
         BOX(2)=BOX(2)-1
         BOX(3)=BOX(3)+1
         BOX(4)=BOX(4)+1
 50   CONTINUE

C     shadow color
      COL = COLFOR
C     make raster ops are set
      CALL ROPREP()
      CALL TOOLPEN(COL)
      CALL TOOLS_SET_SHADOW(.TRUE.)

      RECT(1) = BOX(3)
      RECT(2) = BOX(2) + SHADOW
      RECT(3) = SHADOW - 3
      RECT(4) = BOX(4) - BOX(2) - 3
      CALL TOOLS_RECTANGLE(RECT,ST)
      RECT(1) = BOX(1) + SHADOW
      RECT(2) = BOX(4)
      RECT(3) = BOX(3) - BOX(1) - 3
      RECT(4) = SHADOW - 3
      CALL TOOLS_RECTANGLE(RECT,ST)
      CALL TOOLS_SET_SHADOW(.FALSE.)
C
C     Now ... paint the cell with text
C
C     set coords for starting postion in COMMON
      STARTX = XP
      STARTY = YP
C     make sure raster op
      CALL ROPREP()
C     A simple, 1 column menu.
      IF(TYPE.EQ.1) THEN
          COUNT = MNTOT 
      ELSEIF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
C         open directory file
          REWIND(UNIT=POPUNT)
C         add one for the banner
          COUNT = MIN(MAXFIL,POPFIL) + 1
      ENDIF
C     set biggest text distance
      MAXW = 0
C     get starting x y point
      TY = YP + TGAP
      TX=  XP + LGAP
C
      FONT =  POPFNT(POPNUM)
C     get maximium height
      CALL FONT_INQ_MAX(FONT,HEIGHT,ST)
C     set size please
      DO 60 I = 1,COUNT
C         Text positon in cell
          IF(I.GT.1.AND.(TYPE.EQ.2.OR.TYPE.EQ.3)) THEN

              READ(UNIT=POPUNT,FMT='(A)',ERR=999,END=999) STRING

          ELSE
C             get text from loader
              STRING = DAXMNU(I)
          ENDIF
          IF(I.EQ.1) THEN
C             set offsets as banner
              CALL SET_TEXT(STRING,ST)
          ENDIF
          POPHGT(I) = HEIGHT
C         set maximum width
          IF(TEXTW.GT.MAXW) THEN
              MAXW = TEXTW
          ENDIF
C         This is where the menu is
          CALL TOOLPEN_TEXT(POPCOL(1,POPNUM),
     +                      POPCOL(2,POPNUM),.TRUE.,ST)
C         draw in text
          CALL TOOLS_TEXT( INT2(TX),INT2(TY+TORGY),
     +                     STRING,NLEN2(STRING),ST)
C         increment to next pos
          TY = TY + HEIGHT + LNSPC
          IF(I.EQ.1) THEN
C             banner gets extra
              TY = TY+LNSPC
          ENDIF

 60   CONTINUE

C     ensure widest text please
C
C     Highlight the BANNER cell
C
999   CONTINUE
      SCROLL = 0
C     what type are we using
      IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
C         set global file selection flag
          POPLST = .TRUE.
          IF(POPFIL.GT.MAXFIL) THEN
              SCROLL = POPBAR
          ENDIF
      ENDIF
C     set text width 
      TEXTW=(BITMW - 2*BLKBDR - SCROLL - SHADOW) -1
      BMWIND(1)= XP 
      BMWIND(2)= YP
      BMWIND(3) = TEXTW
      BMWIND(4) = HEIGHT + 2*LNSPC
C     highlight it
      CALL TOOLPEN(POPCOL(1,POPNUM))
      CALL TOOLS_RECT_INV(BMWIND,ST)
      IF(REPAINT.AND.POPUP_CURRENT_CELL.GT.0) THEN
          CALL TOOLS_RECT_INV(NEWRECT,ST)
      ENDIF

C
      END
C
C
C
C
C
      SUBROUTINE TOOLS_RECT_INV(RECT,ST)
C     ==================================
C1    VARYPE                     I2   I4
C1    IOSTAT                      I    O  
C
C2    This routine will inver a given window on the screen
C
      INTEGER*2 RECT(4)
      INTEGER*4 ST
C
      CALL ROPXOR()
      CALL TOOLS_RECTANGLE(RECT,ST)
C      
      END
C
C
      SUBROUTINE MAKE_POPUP(BANNER,COLOR,
     +                      BCGCOL,FONT,UPSTR,TYPE,
     +                      TARGET,TARGLEN,WILDC,WLEN,CURSOR,POPNUM,ST)
C     ===================================================================
C1    VARYPE                  I4     C(*)   C*(*)  I4   I4  I4   L I4  I4
C1    IOSTAT                  I      I       I     I    I   I    I  O  O
C
C2    This routine makes a popup from the cells etc
C
      include    'include/popup.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 NOCELLS,POPNUM,I,FONT
      CHARACTER ARRAY(MAXFIL)*60,BANNER*(*)
      INTEGER*4 ST,COLOR,BCGCOL,SHCOL
      CHARACTER*(*) TARGET
      CHARACTER*(*) WILDC
      INTEGER*4 TARGLEN
      INTEGER*4 TYPE
      INTEGER*4 WLEN
      INTEGER*4 CURSOR
      LOGICAL UPSTR
C
      NOCELLS = 0
      IF(POPNUM.EQ.0)  THEN
          DO 10 I=1,MAXPOP
              IF(.NOT.POPACT(I)) THEN
                  POPNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF

20    CONTINUE
C     check limits here
      IF(POPNUM.GT.MAXPOP) THEN
          PRINT*, 'No more popups (MAX) ',MAXPOP
          ST = 1
          RETURN
      ELSEIF(NOCELLS.GT.MAXCEL) THEN
          PRINT*, 'Too many cells max ',MAXCEL
          ST = 1
          RETURN
      ENDIF

C      POPSTR = POPEND+1
CC     save starting poostion
C      MNPNTR(1,POPNUM) = POPSTR
C      DO 30 I=1,NOCELLS
C          VNOUNS(I+POPSTR-1) = ARRAY(I)
C30    CONTINUE
C
C      MNPNTR(2,POPNUM) = POPSTR+NOCELLS-1
C      POPEND = POPSTR+NOCELLS-1

      DAXBNR(POPNUM) = BANNER
C     activate this popup
      POPACT(POPNUM) = .TRUE.
      POPCOL(1,POPNUM) = COLOR
      POPCOL(2,POPNUM) = BCGCOL
      PACTUP(POPNUM) = UPSTR
      IF(FONT.LE.0.OR.FONT.GT.100) THEN
C         set as current loaded font
          CALL INQ_FONT(FONT,ST)
      ENDIF
C
      POPFNT(POPNUM) = FONT
C     save type
      POPTYP(POPNUM) = TYPE
C     save target directory
      IF(TARGLEN.EQ.0) THEN
          POPDIR(POPNUM) = '.'
      ELSE
          POPDIR(POPNUM) = TARGET(1:TARGLEN)
      ENDIF
C     wildcarding
      IF(WLEN.EQ.0) THEN
          POPWDC(POPNUM) = '?*'
      ELSE
          POPWDC(POPNUM) = WILDC(1:WLEN)
      ENDIF
C     cursor type 
      POPCUR(POPNUM) = CURSOR
C
      END





      SUBROUTINE POPUP_ALLOCATE(POPNUM,ST)
C     =====================================
C1    VARTYPE                     I4   I4
C1    IOSTAT                      I    O
C
C     alloactes a bitmap for swapping
C2
      include  'include/daxcolor.inc'
      include  'include/popup.inc'
      include  'include/edit.inc'
      include  'include/bitmap.inc'
C
      LOGICAL EX
      LOGICAL OK
      INTEGER*4 BMSIZ(2)
      INTEGER*4 POPNUM
      INTEGER*4 FONT
      INTEGER*4 ST
      INTEGER*4 COL,I
      INTEGER*4 NLEN
      INTEGER*4 MAXW,MAXH,CELLS
      INTEGER*4 TYPE
      INTEGER*4 MATCHNO
      INTEGER*4 SCROLL
      INTEGER*4 LENGTH
      INTEGER*4 TLEN
      INTEGER*4 BINDEX
      INTEGER*4 UNITS
      INTEGER*4 HEIGHT
      INTEGER*4 DIRS
      CHARACTER*60 TEXT
      CHARACTER*256 TARGET
      CHARACTER*256 WILDC
      CHARACTER*256 TMPFIL
      CHARACTER*256 BUFF1
      CHARACTER*256 BUFF2
      CHARACTER*20 TYPED

C
      EXTERNAL NLEN,BINDEX
C
C     set type
      TYPE = POPTYP(POPNUM)
      MAXW = 0
      MAXH = 0
      HEIGHT = 0
C     set font to draw
      FONT = POPFNT(POPNUM)
      CALL SET_FONT(FONT,ST)
C     set up banner defintions
      TEXT = DAXMNU(1)
      CALL SET_TEXT(TEXT,ST)
      HEIGHT = TEXTH
C     set size please
      MAXW = TEXTW
      TMPFIL = ' '
C
      SCROLL = 0
      IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
C         File selector menu
C         get a temporty filename now
          CALL GETTMPFILE(TMPFIL,LENGTH,POPFID,ST)
C         set wild card and targets
          WILDC = POPWDC(POPNUM)
          TARGET = POPDIR(POPNUM)
C         do the search
          IF ( TYPE.EQ.3) THEN
             DIRS = 1
          ELSE
             DIRS = 0
          ENDIF
          CALL GET_DIRS_WILD(TARGET,
     +                       NLEN(TARGET),
     +                       DIRS,
     +                       WILDC,
     +                       NLEN(WILDC),
     +                       TMPFIL,
     +                       LENGTH,
     +                       MATCHNO,
     +                       ST)
          POPFIL = MATCHNO
C         set maximum for for current selection
          MATCHNO = MIN(MAXFIL,MATCHNO)
          CALL FINDU1(UNITS,OK)
          POPUNT = UNITS
C
          OPEN(UNIT=POPUNT,
     +              FILE=TMPFIL(1:LENGTH),
     +              ERR=900)
C
          LENGTH = 0
C
          DO 20 I=1,POPFIL
C
              READ(UNIT=POPUNT,FMT='(A)',ERR=900) BUFF1
C
              TLEN = NLEN(BUFF1)
C
              CALL SET_TEXT(BUFF1,ST)
              IF(TEXTW.GT.MAXW) THEN
                   MAXW = TEXTW
              ENDIF
              IF(TLEN.GT.LENGTH) THEN
                  TEXT = BUFF1
                  LENGTH = TLEN
              ENDIF
C
20        CONTINUE
C
          LENGTH = LENGTH + 2
C         find out maximum height of the font
          CALL FONT_INQ_MAX(FONT,HEIGHT,ST)
C         set size please
          MAXH = (HEIGHT+LNSPC)*(MATCHNO+1)
          IF(TEXTW.GT.MAXW) THEN
C             set maximum width
              MAXW = TEXTW
          ENDIF
          IF(POPFIL.GT.MAXFIL) THEN
              SCROLL = POPBAR
          ENDIF
      ELSE
          CALL FONT_INQ_MAX(FONT,HEIGHT,ST)
          MAXH = HEIGHT+LNSPC
          DO 10 I=2,MNTOT
              TEXT= DAXMNU(I)
              CALL SET_TEXT(TEXT,ST)
C             add text heights for differing sizes
              MAXH = MAXH+HEIGHT+LNSPC
              IF(TEXTW.GT.MAXW) THEN
                  MAXW = TEXTW
              ENDIF
10        CONTINUE
      ENDIF
C     make sure maximun bitmap heights are used
      POPSIZ(1) = MAXW + HBORDR + SCROLL 
      POPSIZ(2) = MAXH + VBORDR + LNSPC
C     allocate a bitmap for the menu and for saving current
      SWBITM = 0
      CALL MAKE_BITMAP(SWBITM,POPSIZ,0,ST)
C     Allocate an attribute block for the bitmap
      COL=COLBAK
C     white it out or clear it out 
      CALL BITMAP_CLEAR(SWBITM,COL,ST)
C     set colors for drawing with
      COL = POPCOL(1,POPNUM)
C     set up color for drawing
      CALL TOOLPEN(COL,ST)
C 
      RETURN
C
C
 910  CONTINUE
      RETURN
 900  CONTINUE
      END
c
      SUBROUTINE POPUP_GET_USER(CELL,BUFF,INLEN,BUFLEN,ST)
C     ====================================================
C1    VARYPE                      I4   I4  C      I4    I4  
C1    IOSTAT                      I    I   O      I     O  
C
C2    Gets the text from a cell form a preloaded popup. Must
C2    be called after the popup call is made.
C2
C2    Arguments:-
C2  
C2    CELL        ->      The cell number you want text from
C2    BUFF        ->      A supplied character buffer
C2    INLEN       ->      The length if the supplied buffer
C2    BUFLEN      ->      The returned length of the text in the cell.
C2  
C2    Error Returns:
C2  
C2    1           ->      Cell is out of range in the popup you want
C2
C2
C
      include  'include/popup.inc'
C
      INTEGER*4 ST
      INTEGER*4 INLEN
      INTEGER*4 BUFLEN
      INTEGER*4 CELL
      INTEGER*4 NLEN
      INTEGER*4 I
      INTEGER*4 POPNUM
      INTEGER*4 POPS
      INTEGER*4 POPE
      INTEGER*4 LENGTH
      CHARACTER*60 BUFF
C
      ST = 0
C
      IF (CELL.GT.MAXCEL.OR.CELL.LE.0 ) THEN
          ST = 1
          GOTO 999
      ENDIF
C
      IF ( INLEN.GT.LEN(VNOUNS(1)) ) THEN
C         chop
          LENGTH = LEN(DAXMNU(1))
      ELSE
C         the size of the buffer he wants to use.
          LENGTH = INLEN
      ENDIF
C
C     load the value he wants.
      BUFF(1:LENGTH) = DAXMNU(CELL+1)
      BUFLEN = NLEN(DAXMNU(CELL+1))
C
      RETURN
999   CONTINUE
      ST = 1
      END

      SUBROUTINE POPUP_GET_TEXT(POPNUM,CELL,BUFF,INLEN,BUFLEN,ST)
C     ===========================================================
C1    VARYPE                      I4   I4  C      I4    I4    I4
C1    IOSTAT                      I    I   O      I     O     O
C
C2    gets text asccoiated with the popup number an cell
C2
C2    Arguments:-
C2  
C2    POPNUM      ->      The popup number id returned from the loader or a 
C2                        popup_make
C2  
C2    CELL        ->      The cell number you want text from
C2    BUFF        ->      A supplied character buffer
C2    INLEN       ->      The length if the supplied buffer
C2    BUFLEN      ->      The returned length of the text in the cell.
C2  
C2    Error Returns:
C2  
C2    1           ->      Popup id is out of range
C2    2           ->      Popup has not been defined ( MAKE_POPUP )
C2    3           ->      Cell is out of range in the popup you want
C2
C2
C
      include  'include/popup.inc'
C
      INTEGER*4 ST
      INTEGER*4 INLEN
      INTEGER*4 BUFLEN
      INTEGER*4 CELL
      INTEGER*4 NLEN
      INTEGER*4 I
      INTEGER*4 POPS
      INTEGER*4 POPE
      INTEGER*4 POPNUM
      INTEGER*4 LENGTH
      CHARACTER*60 BUFF
C
      EXTERNAL NLEN
C
      ST = 0
C     error check
      IF(POPNUM.LT.1.OR.POPNUM.GT.MAXPOP) THEN
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.POPACT(POPNUM)) THEN
          ST = 2
          GOTO 999
      ENDIF

C
      IF ( INLEN.GT.LEN(VNOUNS(1)) ) THEN
C         chop
          LENGTH = LEN(VNOUNS(1))
      ELSE
C         the size of the buffer he wants to use.
          LENGTH = INLEN
      ENDIF
      POPS = MNPNTR(1,POPNUM)
      POPE = MNPNTR(2,POPNUM)
C     save starting poostion
      DO 30 I=POPS,POPE
          IF(CELL+POPS-1.EQ.I) THEN
              BUFF(1:LENGTH) = VNOUNS(I)
              BUFLEN = NLEN(VNOUNS(I))
              ST = 0
              RETURN
          ENDIF
 30   CONTINUE
      ST = 3
      BUFLEN = 0
C
      RETURN
999   CONTINUE
      ST = 1
      END

      SUBROUTINE FILE_SCR_ARROW(X,Y,SIZE,UP)
C     ======================================
C1    VARYPE                    I2I2  I2
C1    IOSTAT                    I I   I 
C
C2    Draws a scroll bar arrow at reqested pos
C2    and scale
C2  
      include    'include/edit.inc'
C
      INTEGER*4 ST,COL
      INTEGER*2 V1(2)
      INTEGER*2 V2(2)
      INTEGER*2 V3(2)
      INTEGER*2 RECT(4)
      LOGICAL UNOBSCURED,UP
      INTEGER*2 SIZE,X,Y,DIST
C
      ST = 0
C     triangle first
      V1(1) = X
      V1(2) = Y
      V2(1) = (6*SIZE)+X
      V2(2) = Y
      V3(1) = (3*SIZE)+X
      IF(UP) THEN
         V3(2) = Y-(4*SIZE)
      ELSE
         V3(2) = (4*SIZE)+Y
      ENDIF
C     Draw the triangle
      CALL ROPREP()
C     set pen coloor fro filling
      CALL TOOLS_TRIANGLE(V1,V2,V3,ST)
      RECT(1) = X+(SIZE*2)
      IF(UP) THEN
          RECT(2) = Y
      ELSE
          RECT(2) = Y-(4*SIZE)
      ENDIF
      RECT(3) = (2*SIZE)
      RECT(4) = 4*SIZE
      CALL TOOLS_RECTANGLE(RECT,ST)
      
      END
C
      SUBROUTINE POPUP_FILE_PAINT(POPNUM,X,Y,ST)
C     ==========================================
C1    VARTYPE                       I4   I2I2  I4
C1    IOSTAT                        I    I I    O
C
C2    Paints a the popup a the location of FILPOS
C
      include  'include/daxcolor.inc'
      include  'include/popup.inc'
      include  'include/edit.inc'
C
      INTEGER*4 ST
      INTEGER*4 POPNUM
      INTEGER*2 X,Y
      INTEGER*2 RECT(4)
      INTEGER*4 XMIN
      INTEGER*4 YMIN
      INTEGER*4 XMAX
      INTEGER*4 YMAX
      INTEGER*4 TX,TY
      INTEGER*4 COUNT
      INTEGER*4 TLEN
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      INTEGER*4 BINDEX
      INTEGER*4 FILCNT
      INTEGER*4 I
      INTEGER*4 COL
      INTEGER*4 HEIGHT
      INTEGER*4 FONT
      INTEGER*2 MARKER(4)
      INTEGER*2 BOX(4)
      INTEGER*2 INT2
      CHARACTER*256 BUFF2
      CHARACTER*256 BUFF
      CHARACTER*20  TYPED
C
      EXTERNAL NLEN,BINDEX
C             
      ST = 0
C     set box sizes for clearing
      FONT = POPFNT(POPNUM)
      CALL FONT_INQ_MAX(FONT,HEIGHT,ST)
      BOX(1) = X
      BOX(2) = Y + HEIGHT + 2*LNSPC
      BOX(3) = BITMW - (2*BLKBDR) - SHADOW -POPBAR -1
      BOX(4) = BITMH - (2*BLKBDR) - SHADOW -HEIGHT - LNSPC -1
C
      CALL ROPREP()
C
C     draw a clear box
      COL = POPCOL(2,POPNUM)
      CALL TOOLPEN(COL)
      CALL TOOLS_RECTANGLE(BOX,ST)
C     
C     increment one done
      TY = Y + TGAP + HEIGHT + 2*LNSPC
      TX=  X + LGAP
      COUNT = POPFIL
      REWIND(UNIT=POPUNT)
      FILCNT = 0
      DO 10 I = 1,COUNT
C         Text positon in cell
          IF(FILCNT.EQ.MAXFIL) GOTO 20
          READ(UNIT=POPUNT,FMT='(A)') BUFF2
          IF(I.GE.FILPOS) THEN
C             do the text paint
              FILCNT = FILCNT + 1
C             set toolcolor
              CALL TOOLPEN_TEXT(POPCOL(1,POPNUM),
     +                      POPCOL(2,POPNUM),.TRUE.,ST)
C             text move
              CALL TOOLS_TEXT( INT2(TX),INT2(TY+TORGY),
     +                        BUFF2,INT2(NLEN(BUFF2)),ST)
              TY = TY + HEIGHT + LNSPC 
C
          ENDIF
 10   CONTINUE
 20   CONTINUE
C
C     draw marker at position
      CALL FILE_MARKER(POPNUM,ST)

      END
C
      SUBROUTINE FILE_MARKER(POPNUM,ST)
C     =================================
C1    VARYPE                  I4    I4
C1    IOSTAT                  I4    O
C
C2    Draws a file scroll block at the desired pos
C  
      include  'include/daxcolor.inc'
      include  'include/popup.inc'
C
      INTEGER*2 MARKER(4)
      INTEGER*4 POPNUM
      INTEGER*4 ST
      INTEGER*4 BOXPIX
      INTEGER*4 XMIN
      INTEGER*4 YMIN
      INTEGER*4 XMAX
      INTEGER*4 YMAX
      INTEGER*4 COLOR
      INTEGER*4 BACKCOL
      LOGICAL UNOBSCURED
C
      ST = 0
      CALL ROPREP()
      BACKCOL = POPCOL(2,POPNUM)
      COLOR = POPCOL(1,POPNUM)
C     get box limits (coordinates here)
      XMIN = STARTX+POPSIZ(1)-POPBAR-SHADOW-2*BLKBDR
      YMIN = STARTY+POPBAR
      XMAX = XMIN + POPBAR
      YMAX = YMIN + POPSIZ(2) - SHADOW - 2*BLKBDR -2*POPBAR
C
C     number of pixels in box
      BOXPIX = YMAX-YMIN+1
C     clear window first
      MARKER(1) = XMIN
      MARKER(2) = YMIN
      MARKER(3) = POPBAR - 1
      MARKER(4) = BOXPIX - 2
C     set color to clear window
      CALL TOOLPEN(BACKCOL)
      CALL TOOLS_RECTANGLE(MARKER,ST)
      MARKER(4) = MAX(((BOXPIX*MAXFIL)/POPFIL)-3,1)
C     starting position 
      MARKER(2) = YMIN+(BOXPIX*(FILPOS-1)/POPFIL)+1
      MARKER(1) = XMIN+1
      MARKER(3) = POPBAR-3
      CALL TOOLPEN(COLOR)
      CALL TOOLS_RECTANGLE(MARKER,ST)
C
      END
C
C
      SUBROUTINE STRIP_FILE(FILNAM,ST)
C     ================================
C1    VARYPE                C*(*)  I4
C1    IOSTAT                   I   O
C
C2    Strips of the pathname of a filename
C
      CHARACTER*(*) FILNAM
      CHARACTER*256 BUFF
      INTEGER*4 ST
      INTEGER*4 TLEN
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      INTEGER*4 BINDEX
C
      EXTERNAL BINDEX,NLEN
C
      ST = 0
      TLEN = BINDEX(FILNAM,'/')
      IF(TLEN.GT.0) THEN
          BUFF = FILNAM(TLEN+1:)
          FILNAM = BUFF
      ENDIF
C
      END

C
      SUBROUTINE INQ_FILE_TARGET(POPNUM,TARGET,INLEN,TLEN,ST)
C     ========================================================
C1    VARYPE                       I4    C*(*)   I4   I4  I4
C1    IOSTAT                       I      I/O     I    O  O
C
C2    This routine will return the target length
C2    for popup file selection system. The buffer 
C2    length must be passed to start with then the 
C2    returned length of the target will be used.
C
      include  'include/popup.inc'
C
      CHARACTER*256 TARGET
      INTEGER*4 INLEN
      INTEGER*4 TLEN
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 POPNUM
C
      EXTERNAL NLEN
C
      ST = 0
C     check for range first 
      IF(POPNUM.GT.0.AND.POPNUM.LE.MAXPOP) THEN
          IF(.NOT.POPACT(POPNUM)) THEN
C             popup not active yet
              ST = 2
              RETURN
          ELSE
C             ok here set status
              ST = 0
          ENDIF
      ELSE
C         out of range error
          ST = 3
          RETURN
      ENDIF
      TLEN = NLEN(POPDIR(POPNUM))
      IF(TLEN.GT.INLEN) THEN
          ST = 1
          RETURN
      ENDIF
C     set target now
      TARGET(1:INLEN) = POPDIR(POPNUM)(1:TLEN)
C     finish now
      END
C

      SUBROUTINE SET_FILE_TARGET(POPNUM,TARGET,INLEN,ST)
C     ==================================================
C1    VARYPE                       I4    C*(*)   I4  I4
C1    IOSTAT                       I      I/O     I  O
C
C2    This routine will set the the target length
C2    for popup file selection system. The buffer 
C2    length must be passed. No Gaurantee that the 
C2    direcotry exists is given
C
      include  'include/popup.inc'
C
      CHARACTER*256 TARGET
      INTEGER*4 INLEN
      INTEGER*4 TLEN
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 POPNUM
C
      EXTERNAL NLEN
C
C     check for range first 
      ST = 0
      IF(POPNUM.GT.0.AND.POPNUM.LE.MAXPOP) THEN
          IF(.NOT.POPACT(POPNUM)) THEN
C             popup not active yet
              ST = 2
              RETURN
          ELSE
C             ok here set status
              ST = 0
          ENDIF
      ELSE
C         out of range error
          ST = 3
          RETURN
      ENDIF
      TLEN = NLEN(POPDIR(POPNUM))
C     set target now
      POPDIR(POPNUM) = TARGET(1:INLEN)
C     finish now
      END
      SUBROUTINE INQ_FILE_WILD(POPNUM,WILDC,INLEN,TLEN,ST)
C     ====================================================
C1    VARYPE                       I4    C*(*)   I4 I4  I4
C1    IOSTAT                       I      I/O     I  O  O
C
C2    This routine will return the wildcard length
C2    for popup file selection system. The buffer 
C2    length must be passed to start with then the 
C2    returned length of the target will be used.
C
      include  'include/popup.inc'
C
      CHARACTER*256 WILDC
      INTEGER*4 INLEN
      INTEGER*4 TLEN
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 POPNUM
C
      EXTERNAL NLEN
C
      ST = 0
C     check for range first 
      IF(POPNUM.GT.0.AND.POPNUM.LE.MAXPOP) THEN
          IF(.NOT.POPACT(POPNUM)) THEN
C             popup not active yet
              ST = 2
              RETURN
          ELSE
C             ok here set status
              ST = 0
          ENDIF
      ELSE
C         out of range error
          ST = 3
          RETURN
      ENDIF
      TLEN = NLEN(POPWDC(POPNUM))
      IF(TLEN.GT.INLEN) THEN
          ST = 1
          RETURN
      ENDIF
C     set target now
      WILDC(1:INLEN) = POPDIR(POPNUM)(1:TLEN)
C     finish now
      END
C
      SUBROUTINE SET_FILE_WILDC(POPNUM,WILDC,INLEN,ST)
C     ===============================================
C1    VARYPE                       I4  C*(*)   I4  I4
C1    IOSTAT                       I    I/O     I  O
C
C2    Sets the wild card target for our 
C
      include  'include/popup.inc'
C
      CHARACTER*256 WILDC
      INTEGER*4 INLEN
      INTEGER*4 TLEN
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 POPNUM
C
      EXTERNAL NLEN
C
      ST = 0
C     check for range first 
      IF(POPNUM.GT.0.AND.POPNUM.LE.MAXPOP) THEN
          IF(.NOT.POPACT(POPNUM)) THEN
C             popup not active yet
              ST = 2
              RETURN
          ELSE
C             ok here set status
              ST = 0
          ENDIF
      ELSE
C         out of range error
          ST = 3
          RETURN
      ENDIF
      TLEN = NLEN(POPDIR(POPNUM))
C     set target now
      POPWDC(POPNUM) = WILDC(1:INLEN)
C     finish now
      END
C
      SUBROUTINE POPUP_HIGHLIGHT(MNCODE,TOKEN,CELL,OK)
C     ================================================
C1    VARTYPE                      I4     C    I4   L
C1    IOSTAT                       I      O    O    O
C
C2    This routine will higlight the popup menu and return
C2    the token associated  with this cell
C
      include  'include/gpr.ins.inc'
      include  'include/popup.inc'
      include  'include/edit.inc'
      include  'include/event.inc'
      include  'include/buttons.inc'
      include  'include/cursor.inc'
C
      INTEGER*4 MNCODE
      INTEGER*4 X,Y
      INTEGER*4 CELL
      INTEGER*4 OCELL
      INTEGER*4 ST
      INTEGER*2 BMWIND(4)
      INTEGER*2 SCRPOS(2)
      INTEGER*2 SCPOSN(2)
      INTEGER*2 CURSX,CURSY
      INTEGER*2 RECT(4)
      INTEGER*2 OLDX,OLDY
      INTEGER*2 CURSP(2),POS(2)
      INTEGER*2 XP,YP
      INTEGER*2 EVTYPE
C      
      INTEGER*4 I
      INTEGER*4 EVENT_TYPE
      INTEGER*4 EVENT_DATA
      INTEGER*4 XMIN,YMIN,XMAX,YMAX
      INTEGER*4 POPNUM
      INTEGER*4 TYPE
      INTEGER*4 SCROLL
      INTEGER*4 CURS
      INTEGER*4 CURNUM
      INTEGER*4 TW,TH
      INTEGER*4 COUNT
      INTEGER*4 BUFLEN
      INTEGER*4 POPLEN
      INTEGER*4 CPOS(2)
C
      CHARACTER*1 EVDATA
      CHARACTER*1 TOKEN
      CHARACTER*20 STRING
      CHARACTER*256 BUFF
      CHARACTER*256 PBUF
C
      LOGICAL INBOX,PNTBOX
      LOGICAL SCRBOX
      LOGICAL NEWCUR
      LOGICAL EX
      LOGICAL FIRST
      LOGICAL KEYHIT
      LOGICAL LWAIT
      LOGICAL OK,MENOK,LOC,OUT,HORFLG,VERFLG
C
      DOUBLE PRECISION TIME_OUT
C
      EXTERNAL ROPXOR,ROPREP,PNTBOX
C
C     *************************************
C     **      Initalise the varibles     **
C     *************************************
C
      ST = 0
C
      NEWCUR = .FALSE.
C
      FIRST = .TRUE.
      POPNUM = MNCODE
      TIME_OUT = 0.0
C     save cursor types now
      CURS = CURSTY
      CURNUM = POPCUR(POPNUM)
      NEWCUR = CURSON
C     draw in cursor typ e for popup
      TW = TEXTW
      TH = TEXTH
      CALL CURDRW(CURNUM,ST)
      TEXTW = TW
      TEXTH = TH
C     save type
      TYPE = POPTYP(POPNUM)
      IF((TYPE.EQ.2.OR.TYPE.EQ.3).AND.POPFIL.GT.MAXFIL) THEN
C         reset starting file position
          IF(.NOT.POPUPLOCKED ) THEN
              FILPOS = 1
C             set scroll bar
          ENDIF
C         reset marker.
          CALL FILE_MARKER(POPNUM,ST)
          SCROLL = POPBAR
      ELSE
          IF ( .NOT.POPUPLOCKED ) THEN
              FILPOS = 1
          ENDIF
          SCROLL = 0
      ENDIF
      OUT = .TRUE.
      OLDX = STARTX-BLKBDR
      OLDY = STARTY-BLKBDR
C     set cursor position
      CURSP(1) = OLDX
      CURSP(2) = OLDY
C     draw the cursor in the start position
      CALL ROPXOR()
C     set size of cell window
      BMWIND(1)=0
      BMWIND(2)=0
      BMWIND(3)=BITMW - 2*BLKBDR - SCROLL - SHADOW -1
      BMWIND(4)=TEXTH+LNSPC
C
      OK = .FALSE.
      CELL =-1
      OCELL =-1
C
      CALL TOOLPEN(POPCOL(1,POPNUM))
C
 222  CONTINUE
C
      IF(FIRST) THEN
C         test first event.
          CALL INQ_CURSOR(CURNUM,CPOS,ST)
          SCRPOS(1) = CPOS(1)
          SCRPOS(2) = CPOS(2)
          EVENT_TYPE = GPR_$LOCATOR
          FIRST = .FALSE.
      ELSE
          LWAIT = GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
          EVENT_TYPE = EVTYPE
      ENDIF
C
C     load parameters for easier working
      CURSX=SCRPOS(1)
      CURSY=SCRPOS(2)
C     is cursor out with our little menu
      XMIN = STARTX
      YMIN = STARTY 
      XMAX = XMIN + POPSIZ(1) - 2*BLKBDR - SHADOW -SCROLL
      YMAX = YMIN + POPSIZ(2) - 2*BLKBDR - SHADOW
      INBOX = PNTBOX(XMIN,YMIN,XMAX,YMAX,INT4(CURSX),INT4(CURSY))

      KEYHIT = .FALSE.
      IF ( EVENT_TYPE.EQ.GPR_$KEYSTROKE) THEN
C         check for special keystrokes to make popup move 
          IF ( EVDATA.EQ.CHAR(2).OR.
     +         EVDATA.EQ.CHAR(20).OR.
     +         EVDATA.EQ.CHAR(136).OR.
     +         EVDATA.EQ.CHAR(142) ) THEN
              EVENT_TYPE = GPR_$BUTTONS
              KEYHIT = .TRUE.
          ENDIF
      ENDIF

      IF (EVENT_TYPE.EQ.GPR_$LOCATOR) THEN
          CALL ROPXOR()
C         find location of cursor
          IF(.NOT.INBOX) THEN
              IF(OUT) THEN
C                 update cursor postion
                  CALL CURSOR_UPDATE(SCRPOS,ST)
              ELSE
C                 paint out the old cell
                  IF(OCELL.GE.0) THEN
                      CALL TOOLS_RECT_INV(NEWRECT,ST)
                  ENDIF
                  CELL = -1
                  OCELL = -1
                  POPUP_CURRENT_CELL = 0
C                 switch cursor on
                  CALL CURSOR_ON(.TRUE.)
              ENDIF
              OUT = .TRUE.
C
          ELSE
C             get the menu cell in the
              CALL POPUP_CELL(POPNUM,CURSX,CURSY,CELL,POS,BMWIND)
              IF(OUT) THEN
C                 turn cursor off
                  CALL CURSOR_ON(.FALSE.)
                  OUT = .FALSE.
              ENDIF

C             has the cell changed ? or is it in cell 0
              IF(CELL.NE.OCELL.AND.CELL.NE.0) THEN
C                 yup so erase the old one
                  IF(OCELL.GE.0) THEN
C                     erase the old window
                      CALL TOOLS_RECT_INV(NEWRECT,ST)
                  ENDIF
C
                  NEWRECT(1) = POS(1)
                  NEWRECT(2) = POS(2)
                  NEWRECT(3) = BMWIND(3)
                  NEWRECT(4) = BMWIND(4)
C                 rectangle invert
                  CALL TOOLS_RECT_INV(NEWRECT,ST)
                  OCELL = CELL
                  POPUP_CURRENT_CELL = CELL
              ENDIF
          ENDIF
C         save current postion
          OLDX = CURSX
          OLDY = CURSY
C         update system cursor
          CALL CURSOR_UPDATE(SCRPOS,ST)
          GOTO 222
      ELSE IF (EVENT_TYPE.EQ.GPR_$KEYSTROKE) THEN
          IF((TYPE.EQ.3.OR.TYPE.EQ.2).AND.POPFIL.GT.MAXFIL) THEN
              STRING = ' '
              STRING(1:1) = EVDATA
C             get a string by a timeout mechanisim.
              CALL KEY_TIMEOUT(1.0,STRING,COUNT,OK)
              IF(OUT) THEN
C                 turn cursor off for now
                  CALL CURSOR_ON(.FALSE.)
              ENDIF
              CALL SET_POPUP_FILE(POPNUM,STRING,COUNT,ST)
C             repaint time if st = 0
              IF(ST.EQ.0) THEN
                 IF( .NOT.OUT ) THEN
C                    repaint cell
                     CALL TOOLS_RECT_INV(NEWRECT,ST)
                 ELSE
C                    redraw cursor
                     CALL CURSOR_ON(.TRUE.)
                 ENDIF
              ENDIF
              GOTO 222
          ENDIF

      ELSE IF (EVENT_TYPE.EQ.GPR_$BUTTONS ) THEN
C         any buttons should take us out
          I=ICHAR(EVDATA)
C         If we get an UPSTROKE ignore it

          BUTTON_PRESSED = ICHAR(EVDATA)
C         status of button either up or down
C
C
          BUTTON_STAT = 
     +                  BUTTON_PRESSED.EQ.BUTTON_1_DOWN.OR.
     +                  BUTTON_PRESSED.EQ.BUTTON_2_DOWN.OR.
     +                  BUTTON_PRESSED.EQ.BUTTON_3_DOWN.OR.
     +                  BUTTON_PRESSED.EQ.2            .OR.
     +                  BUTTON_PRESSED.EQ.20           .OR.
     +                  BUTTON_PRESSED.EQ.136          .OR.
     +                  BUTTON_PRESSED.EQ.142
C
          IF((TYPE.EQ.2.OR.TYPE.EQ.3).
     +          AND.POPFIL.GT.MAXFIL.AND.BUTTON_STAT) THEN
              XMIN = STARTX+POPSIZ(1)-POPBAR-SHADOW-2*BLKBDR
              YMIN = STARTY
              XMAX = XMIN + POPBAR
              YMAX = YMIN + POPSIZ(2) - SHADOW - 2*BLKBDR
              SCRBOX=PNTBOX(XMIN,YMIN,XMAX,YMAX,INT4(CURSX),INT4(CURSY))
              IF(SCRBOX.OR.KEYHIT) THEN
C                 scroll bar hit
                  IF(I.EQ.20) THEN
C
C                     jump to top of popup.
C
                      FILPOS = 1
                  ELSEIF(I.EQ.2) THEN
C
C                     Jump to bottom of record.
C
                      FILPOS = POPFIL-MAXFIL+1
                  ELSEIF(CURSY.GT.YMAX-POPBAR.OR.I.EQ.142) THEN
C                     move down throught popup
                      FILPOS = FILPOS + 1
                      IF(FILPOS.GT.POPFIL-MAXFIL+1) THEN
C                         reset highter
                          FILPOS = FILPOS - 1
                          GOTO 222
                      ENDIF

                  ELSEIF(CURSY.LT.YMIN+POPBAR.OR.I.EQ.136) THEN
                      FILPOS = FILPOS - 1
                      IF(FILPOS.EQ.0) THEN
C                         reste lower end
                          FILPOS = 1
                          GOTO 222
                      ENDIF
                  ELSE
C                     calculate form cursor postion
                      YMIN = STARTY+POPBAR
                      YMAX = YMIN + POPSIZ(2) - 
     +                   SHADOW - 2*BLKBDR -2*POPBAR
                      FILPOS = (CURSY-YMIN)*POPFIL/(YMAX-YMIN+1)
                      IF(FILPOS.GT.POPFIL-MAXFIL+1) THEN
C                         reset highter
                          FILPOS = POPFIL-MAXFIL+1
                      ELSEIF(FILPOS.LE.0) THEN
C                         reste lower end
                          FILPOS = 1
                      ENDIF
                  ENDIF
                  XP = STARTX
                  YP = STARTY
C                 updfate all postions within the file
                  CALL CURSOR_ON(.FALSE.)
                  CALL POPUP_FILE_PAINT(POPNUM,XP,YP,
     +                                  ST)
                  IF(.NOT.OUT) THEN
                      CALL TOOLS_RECT_INV(NEWRECT,ST)
                  ENDIF
                  CALL CURSOR_ON(.TRUE.)
                  GOTO 222
              ENDIF
          ENDIF
          IF ( I.GE.65 .AND. I.LE.68) THEN
c             can we use an upstroke to deactivate the cell
              GOTO 222
          ENDIF
          IF(.NOT.OUT) THEN
             CALL TOOLS_RECT_INV(NEWRECT,ST)
          ENDIF
C         switch off
          CALL CURSOR_ON(.FALSE.)
C         reset old cursor
          CALL CURDRW(CURS,ST)
          IF(.NOT.NEWCUR) THEN
C             draw out old cursor
              CALL CURSOR_ON(.FALSE.)
          ENDIF
          IF(CELL.GT.0) THEN
C             set cell position
              IF(TYPE.EQ.2.OR.TYPE.EQ.3) THEN
                  POPLST = .TRUE.
C                 get text with the cxell
                  CALL GET_FILE_CELL(POPNUM,CELL,BUFF,BUFLEN,ST)
                  POPBUF = BUFF(1:BUFLEN)
C                 colon mapper
C                  CALL COLON_MAP(BUFF,BUFLEN,PBUF,POPLEN)
              ENDIF
              OK = .TRUE.
          ELSE
C             force lock out.
              POPUPLOCKED = .FALSE.
          ENDIF
          IF(.NOT.POPUPLOCKED) THEN
C             reset current file postion
              FILPOS = 1
          ENDIF
      ELSE
          GOTO 222
      ENDIF
C
 999  CONTINUE
      END
C
      SUBROUTINE POPUP_LOCK(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Lock the next popup to be called.
C2  
C2  
C2    Arguments:-NONE
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
C
      include  'include/popup.inc'
C
      INTEGER*4 ST
      POPUPLOCKED = .TRUE.
C
      END
C
      SUBROUTINE SET_POPUP_FILE(POPNUM,STRING,BUFLEN,ST)
C     ==================================================
C1    VARYPE                      I4    C*(*)   I4    I4
C1    IOSTAT                      I       I     I     O
C
C2    Takes the string. Finds a match in the current
C2    active popup and sets the popup list visible
C2    The string must be a match of only 10 characters
C
      include  'include/popup.inc'
C
      INTEGER*4 POPNUM
      INTEGER*4 BUFLEN
      INTEGER*4 ST
      INTEGER*4 I
      INTEGER*2 XP,YP
      CHARACTER*(*) STRING
      CHARACTER*256 BUFF1
      CHARACTER*20 BUFF
C
      ST = 0
      IF(BUFLEN.LE.0) THEN
          ST = 1
          GOTO 999
      ELSEIF(BUFLEN.GT.10) THEN
          BUFLEN = 10
      ENDIF
C
C     rewind the current file for matching
      REWIND(POPUNT)
      DO 10 I=1,POPFIL
          READ(UNIT=POPUNT,FMT='(A)',ERR=999) BUFF1
          BUFF = BUFF1
          IF(BUFF(1:BUFLEN).EQ.STRING(1:BUFLEN)) THEN
C             match has occured repostion file
              FILPOS = MIN(I,POPFIL-MAXFIL+1)
              XP = STARTX
              YP = STARTY
              CALL POPUP_FILE_PAINT(POPNUM,XP,YP,
     +                              ST)
              ST = 0
              RETURN
          ENDIF
10    CONTINUE
      ST = 3
C     dont paint anything No match
      RETURN
999   CONTINUE
C     error from read
      ST = 2
C
      END
C
      SUBROUTINE GET_FILE_CELL(POPNUM,CELL,BUFF,BUFLEN,ST)
C     ====================================================
C1    VARYPE                     I4    I4   C(*)  I4   I4
C1    IOSTAT                     I     I      O   O    O
C
C2    Gets the text ascociated with the cell. Rebinds
C2    the pathname used to get it
C2  
      include  'include/popup.inc'
C
      INTEGER*4 POPNUM
      INTEGER*4 CELL
      INTEGER*4 BUFLEN
      INTEGER*4 ST
      INTEGER*4 I
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      CHARACTER*(*) BUFF
      CHARACTER*256 TBUFF
      CHARACTER*256 NAME
C
      EXTERNAL NLEN
C
      ST = 0
      REWIND(POPUNT)
      DO 10 I=1,POPFIL
          READ(UNIT=POPUNT,FMT='(A)',ERR=999) NAME
          BUFF = NAME
          IF(CELL+FILPOS-2.EQ.I) THEN
C             save name of file
              NAME = BUFF
C             get pathname
              TBUFF = POPDIR(POPNUM)
              LENGTH = NLEN(TBUFF)
              IF(LENGTH.GT.0) THEN
                  IF(TBUFF(LENGTH:LENGTH).NE.'/') THEN
                      LENGTH = LENGTH+1
                      TBUFF(LENGTH:LENGTH) = '/'
                  ENDIF
              ELSE
C                 no path specified
                  TBUFF = './'
              ENDIF
C             reatach name
              BUFF = TBUFF(1:LENGTH)//NAME
              BUFLEN = NLEN(BUFF)
C             return
              ST = 0
              RETURN
          ENDIF
10    CONTINUE
C     no math here
      ST = 1
      BUFLEN = 0
      RETURN
999   CONTINUE
C     read error
      ST = 2
      END


      SUBROUTINE POPUP_GET_FILE(POPNUM,CELL,BUFF,INLEN,BUFLEN,ST)
C     ===========================================================
C1    VARYPE                      I4   I4  C      I4    I4    I4
C1    IOSTAT                      I    I   O      I     O     O
C
C2    gets text asccoiated with the popup file 
C2
C2    Arguments:-
C2  
C2    POPNUM      ->      The popup number id returned from the loader or a 
C2                        popup_make
C2  
C2    CELL        ->      The cell number you want text from
C2    BUFF        ->      A supplied character buffer
C2    INLEN       ->      The length if the supplied buffer
C2    BUFLEN      ->      The returned length of the text in the cell.
C2  
C2    Error Returns:
C2  
C2    1           ->      Popup id is out of range
C2    2           ->      Popup has not been defined ( MAKE_POPUP )
C2    3           ->      This popup has not yet defined a buffer
C2
C2
C
      include  'include/popup.inc'
C
      INTEGER*4 ST
      INTEGER*4 INLEN
      INTEGER*4 BUFLEN
      INTEGER*4 CELL
      INTEGER*4 NLEN
      INTEGER*4 I
      INTEGER*4 POPNUM
      INTEGER*4 LENGTH
      CHARACTER*256 BUFF
C
      EXTERNAL NLEN
C
      ST = 0
      buff  = ' '
C     error check
      IF(POPNUM.LT.1.OR.POPNUM.GT.MAXPOP) THEN
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.POPACT(POPNUM)) THEN
          ST = 2
          GOTO 999
      ENDIF


      IF(POPLST) THEN
          IF ( INLEN.GT.LEN(POPBUF) ) THEN
C             chop
              LENGTH = LEN(BUFF)
          ELSE
C             the size of the buffer he wants to use.
              LENGTH = INLEN
          ENDIF
C         last one was a file selection 
          BUFF(1:LENGTH) = POPBUF
C         returned length
          BUFLEN = NLEN(POPBUF)
C         read the buffer
          POPLST = .FALSE.
          ST = 0
          RETURN
      ELSE
          ST = 3
      ENDIF

999   CONTINUE
      END

