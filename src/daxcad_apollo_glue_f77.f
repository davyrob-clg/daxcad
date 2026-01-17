C
C     @(#)  412.1 date 6/11/92 daxcad_apollo_glue_f77.f 
C
C
C     Filename    : daxcad_apollo_glue_f77.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:48:36
C     Last change : 92/06/11 14:27:10
C
C     Copyright : Practical Technology International Limited
C     File :- daxcad_apollo_glue_f77.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
C     SUBROUTINE GINIT(II)
C     SUBROUTINE PATTERN()
C     SUBROUTINE PTLOGO1()
C     SUBROUTINE STBP()
C     SUBROUTINE ACRDIS(II)
C     SUBROUTINE POPPD1(FILNAM,WNORGX,WNORGY,WNWID,WNHGT)
C     SUBROUTINE SCREENSIZE(PLANES,ROOT)
C     SUBROUTINE BELL()
C     SUBROUTINE SNAQR()
C     SUBROUTINE SNREL()
C     SUBROUTINE TITLE(SCREEN,TEXT)
C     SUBROUTINE GINITX()C
C
C     |-----------------------------------------------------------------|
C
C     Contains all glue routines for DAXCAD version 9.7x GPR ( Apollo )
C     F77 based compiled under Apollo ftn 9.7 directives

      SUBROUTINE GINIT(II,REPAINT)
C     ============================
C                      I2   L
C
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/dtext.inc'
      include 'include/vntable.inc'
      include 'include/style.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
      include 'include/faults.inc'
      include 'include/wtov.inc'
      include 'include/drwspl.inc'
      include 'include/dcross.inc'
      include 'include/library.inc'
      include 'include/product.inc'
      include 'include/apfont.inc'
      include 'include/bitmap.inc'
      include 'include/shadow.inc'
      include 'include/daxcad_x.inc'
C
      INTEGER*2 UNIT
      INTEGER*2 BMSIZ(2)
      INTEGER*2 WIN(4)
      INTEGER*2 BITMID
      INTEGER*2 FONTID
      INTEGER*2 LOOP
      INTEGER*2 SCMODE
      INTEGER*2 I
      INTEGER*2 POS(2)
      INTEGER*2 SMODE
      INTEGER*2 STPX,STPY
      INTEGER*2 STPH
      INTEGER*2 SORG(2)
      INTEGER*2 TIME(3)
      INTEGER*2 I2
      INTEGER*2 RECT(2,2)
      INTEGER*2 PRIM_SET
      INTEGER*2 SCALE
      INTEGER*2 GPRCONFIG
      INTEGER*2 PADTYPE

      INTEGER*4 ST
      INTEGER*4 BLACK
      INTEGER*4 WHITE
      INTEGER*4 OPT
      INTEGER*4 X
      INTEGER*4 II
      INTEGER*4 COLORE
      INTEGER*4 II4(3)
      INTEGER*4 MN4
      INTEGER*4 POSX(4)
      INTEGER*4 POSY(4)
      INTEGER*4 LPC
      INTEGER*4 CPM
      INTEGER*4 MX
      INTEGER*4 MY
      INTEGER*4 COLORV(0:255)
      INTEGER*4 I4
      INTEGER*4 FSTRET
      INTEGER*4 NLEN
      INTEGER*4 COL
      INTEGER*4 DAXCADREPAINT
C
      INTEGER*4 VARSIZE
      INTEGER*4 PLANES
      INTEGER*4 ROOT(2)
      INTEGER*4 TFONTID
C
      LOGICAL REPAINT
      LOGICAL UNOBSCURED
      LOGICAL OP
      LOGICAL EX
      LOGICAL DINGKY
      LOGICAL OK
      LOGICAL SMALL
      LOGICAL MEDIUM
      LOGICAL LARGE
C
      CHARACTER*80 NAME
      REAL XMIN,YMIN,XMAX,YMAX
C
      EXTERNAL KENBLE,GTINGT,GTIMEN,GTIWIN,COLORE
      EXTERNAL ROPREP,FAULT_HANDLER,NLEN
      EXTERNAL WINDOW_REFRESH, WINDOW_PAINT_PROC
      EXTERNAL DAXCADREPAINT
C                       
      SMALL = .FALSE.
      MEDIUM = .FALSE.
      LARGE = .FALSE.
      DCROS = .TRUE.
C     Set spline resolution for writing GNC. ('drwspl.inc')
      RESLTN = 15
C
C     set state of repaint flag
C
      REPAINTPENDING = .FALSE.
C     small version key.
      INQUIRE(FILE='dingky',EXIST=DINGKY)
C     	  
      HIPLAN=0
C
C     set colour default for black and white
C     maximum range 16
      COLIRG=16
C     maximum displayable 2
      COLDRG=2
C     background BLACK
      COLBAK=0
C     foreground WHITE
      COLFOR=1
C     fill colour BLACK
      COLFIL=7
C     draw colour WHITE
      COLDRW=7
C     erase colour as background
      COLERS=COLBAK
C     current colour as draw
      COLPEN=COLDRW
C
C     find out who we are
C

        CALL SCREENSIZE(PLANES,ROOT)
	  
        BMSIZ(1) = ROOT(1)
        BMSIZ(2) = ROOT(2)
	  

      IF (PLANES.EQ.1 ) THEN
           HIPLAN = 0
C          maximum displayable 2
           COLDRG=2
C          background BLACK
           COLBAK=0
C          foreground WHITE
           COLFOR=1
C          fill colour BLACK
           COLFIL=1
C          draw colour WHITE
           COLDRW=1
      ELSEIF(PLANES.EQ.4) THEN
           HIPLAN = 3
C          maximum displayable 16
           COLDRG=16
C          Draw in white
           COLDRW=7
C          WHITE foreground
           COLFOR=7
           COLPEN=7
           COLFIL=7
      ELSEIF(PLANES.EQ.8) THEN
           HIPLAN = 7
C          maximum displayable 256+
           COLDRG=256
C          Draw in white
           COLDRW=7
C          WHITE foreground
           COLFOR=7
           COLFIL=7
           COLPEN=7
      ELSEIF(PLANES.EQ.16) THEN
           HIPLAN = 15
C          maximum displayable 256+
           COLDRG=256
C          Draw in white
           COLDRW=7
C          WHITE foreground
           COLFOR=7
           COLFIL=7
           COLPEN=7
      ELSEIF(PLANES.EQ.24) THEN

           HIPLAN = 23
C          maximum displayable 256+
           COLDRG=256
C          Draw in white
           COLDRW=7
C          WHITE foreground
           COLFOR=7
           COLFIL=7
           COLPEN=7
      ELSE
		   HIPLAN = PLANES -1
C          maximum displayable 256+
           COLDRG=256
C          Draw in white
           COLDRW=7
C          WHITE foreground
           COLFOR=7
           COLFIL=7
           COLPEN=7

      ENDIF
C     erase colour as background
      COLERS=COLBAK
C     current colour as draw
      COLPEN=COLDRW
C
C     set text height (This is a Hack)
      TEXW=9
      TEXH=16
C
   
C
1000  CONTINUE

      LARGE = .TRUE.
      SMALL = .FALSE.
      MEDIUM = .FALSE.

      IF ( DINGKY ) THEN
C        Small version limits.
         BMSIZ(1) = 1024
         BMSIZ(2) = 768
         ROOT(1) = 1024
         ROOT(2) = 768

         TEXW=6
         TEXH=10
         LARGE = .FALSE.
         SMALL = .TRUE.

        XMIN=8.0
        YMIN=130.0
        XMAX=ROOT(1) - 200
        YMAX=ROOT(2) - 80

        POSX(1)=10
        POSY(1)=22

        POSX(2)=10
        POSY(2)=110

C       VERB MENU
        POSX(3)=ROOT(1) -180
        POSY(3)=145
C       DISPLAY MENU

        POSX(4)=ROOT(1) -180
        POSY(4)=720


        BMSIZ(1)=ROOT(1)
        BMSIZ(2)=ROOT(2)

        TEXW=9
        TEXH=16


      ELSE

        BMSIZ(1) = 1680
        BMSIZ(2) = 1050

        ROOT(1) = 1680
        ROOT(2) = 1050



        XMIN=8.0
        YMIN=130.0
        XMAX=ROOT(1) - 200
        YMAX=ROOT(2) - 80

        POSX(1)=10
        POSY(1)=22

        POSX(2)=10
        POSY(2)=110

C       VERB MENU
        POSX(3)=ROOT(1) -180
        POSY(3)=145
C       DISPLAY MENU

        POSX(4)=ROOT(1) -180
        POSY(4)=720


        BMSIZ(1)=ROOT(1)
        BMSIZ(2)=ROOT(2)

        TEXW=9
        TEXH=16

        end if

       VXMIN=0.0
        VXMAX=ROOT(1)
        VYMIN=0.0
        VYMAX=ROOT(2)
        WXMIN=0.0
        WXMAX=ROOT(1)
        WYMIN=0.0
        WYMAX=ROOT(2)
        SCXMIN=0.0
        SCXMAX=ROOT(1)
        SCYMIN=0.0
        SCYMAX=ROOT(2)
C
C     ---------------------------
C     | Initial Window Creation |
C     ---------------------------
C
C
      
      POS(1) = 0
      POS(2) = 0
      CALL GPR_$SET_WINDOW_START(POS,ST)
C
      UNIT = 1
      WIN(1) = 0
      WIN(2) = 0
      WIN(3) = BMSIZ(1)
      WIN(4) = BMSIZ(2)
C
      ROOTW(1) = BMSIZ(1)
      ROOTW(2) = BMSIZ(2)

C
C      ----------------------------
C     | APOLLO GPR INITALISATION |
C     ----------------------------
C
      SCMODE=GPR_$DIRECT
C     initialize graphics in direct mode
      CALL GPR_$INIT(SCMODE,
     +               BITMID,
     +               BMSIZ,
     +               HIPLAN,
     +               DISPDE,
     +               ST)
C
C
      IF ( ST .NE. 0 ) GOTO 999
C     allocate the viewport bitmaps
 
C
      I=10
      CALL ACRDIS(I)
C

      IF ( ST .NE. 0 ) GOTO 999
C     switch off clipping
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
      IF ( ST .NE. 0 ) GOTO 999
C
C     Load default interface params
C
      CALL LOADINTERFACE(ST)
      IF ( HIPLAN.GT.0) THEN
C
C         Load colormap 
C
          CALL LOADCOLORS(ST)
      ENDIF
C
      BMSIZE(1) = BMSIZ(1)
      BMSIZE(2) = BMSIZ(2)
      HIPLN = HIPLAN
C
      CALL LOADSHADOW(ST)
C
C     Further tools init code
C
      CALL INITBITMAP(ST)
      CALL FONTINIT(ST)
      CALL CURSORINIT(ST)
      CALL INPUTINIT(ST)
      CALL DIAGINIT(ST)
      CALL LABELINIT(ST)
      CALL BUTTONINIT(ST)
      CALL FRAMEINIT(ST)
      CALL LOADAPCURSOR(ST)
C
C     Give a nice grey background
C
      IF ( XVERSION ) THEN
          CALL GPR_$SET_REFRESH_ENTRY(DAXCADREPAINT,0,ST) 
      ENDIF
C
		

      CALL ROPREP()
      SCALE = 1
		CALL TestX()

      CALL GPR_$SET_FILL_PATTERN (SHADBT,SCALE,ST)
		CALL TestX()


      CALL GPR_$RECTANGLE(WIN,ST)

      CALL GPR_$SET_FILL_PATTERN (gpr_$nil_bitmap_desc,SCALE,ST)
		

C
C
C     init colors
      OPT = COLBAK
      CALL TOOLPEN(OPT)
      CALL TOOLPEN_TEXT(OPT,OPT,.TRUE.,ST)
C
      CALL GPR_$SET_OBSCURED_OPT(GPR_$OK_IF_OBS,ST)

      IF ( ST .NE. 0 ) GOTO 999
C
C     set raster op to replace
      CALL ROPREP()
      IF ( ST .NE. 0 ) GOTO 999
C     set vector drawing colour
      OPT=COLDRW
      COLPEN=COLDRW
C     save current colour number for construction
      COLOUR=COLPEN
      CALL TOOLPEN(OPT)
      IF ( ST .NE. 0 ) GOTO 999
C     set filling colour value
      OPT=COLFIL
      CALL TOOLPEN(OPT)
      IF ( ST .NE. 0 ) GOTO 999
C     set the fill pattern for entity marking
      CALL PATTERN()
	
C
C
C     ----------------------------------
C          Main Display Fontid
C     ----------------------------------
C
C
      
      
      NAME='9x15bold'
      IF (DINGKY) THEN
            NAME = '6x10'
      ENDIF 
      
      TFONTID = 1
      WRITE(*,'(A)' ) 'Loading X Font', NAME
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)
C     -------------------------------------------------
C     This small hack added by GU - resolve this one day
      IF (ST.EQ.0) GOTO 100

      WRITE(*,*) 'Cannot load font 9x15bold. Trying alternative...'
      NAME='9x15bold'
	  IF (DINGKY) THEN
	      NAME = '6x10'
      ENDIF 
      TFONTID = 1
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)

      IF (ST.EQ.0) GOTO 100

      WRITE(*,*) 'Cannot load font 9x15. Trying alternative...'
      NAME='fixed'
      TFONTID = 1
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)

      IF (ST.NE.0) THEN
         WRITE(*,*) 'Cannot load font fixed - Cannot continue'
         GOTO 999
      ENDIF

 100  CONTINUE
C     -------------------------------------------------
C
C     ----------------------------------
C           Viewport font ID
C     ----------------------------------
C
C
      IF ( XVERSION) THEN
          NAME='vtbold'
      ELSE
C         Apollo font name : /sys/dm/fonts/vt100s
          NAME=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt4.000'
      ENDIF
C
      TFONTID = 2
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)


      TFONTID = 3

      IF ( XVERSION ) THEN
          NAME='vtbold'
      ELSE
          NAME=LIBRARY(1:NLEN(LIBRARY))//'/'//
     +            PRODUCT(1:NLEN(PRODUCT))//'.fnt'
      ENDIF
C
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)

      IF ( XVERSION ) THEN
          NAME='9x15bold'
          IF (DINGKY) THEN
		      NAME = '6x10'
		  ENDIF 
      ELSE
C         Apollo font name : /sys/dm/fonts/courier-boldoblique18
          NAME=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt2.000'
      ENDIF

      TFONTID = 4
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)

      IF ( XVERSION ) THEN
          NAME='9x15bold'
          IF (DINGKY) THEN
		      NAME = '6x10'
		  ENDIF 
      ELSE
C         Apollo font name : /sys/dm/fonts/courier-bold18
          NAME=LIBRARY(1:NLEN(LIBRARY))//'/dict/crypt3.000'
      ENDIF

      TFONTID = 5
      CALL LOAD_FONT(NAME,NLEN(NAME),TFONTID,ST)
C
C     Activate text font
      CALL SET_FONT(1,ST)
C
      IF ( DINGKY ) THEN
C        wee font ( also a Hack )
         TEXW=6
         TEXH=10
      END IF
C
C     Set window to refresh when necessary ( This takes a lot of memory )
      CALL GPR_$SET_AUTO_REFRESH(.TRUE.,ST)
C     Enable CR,BS for apollo keys
      CALL ENABLEAPOLLOKEYS()
C     Enable the keyboard
      CALL GPR_$ENABLE_INPUT(GPR_$KEYSTROKE,KEYSET,ST)

C     Enable the puck buttons,down strokes only
C     clear them all first
      DO 120 I=1,16
      BUTSET(I)=0
 120  CONTINUE
C
C     Enable downstroke buttons,'a','b','c','d'
      CALL KENBLE('a',BUTSET)
      CALL KENBLE('b',BUTSET)
      CALL KENBLE('c',BUTSET)
      CALL KENBLE('d',BUTSET)
C     Need to enable upstrokes to avoid DM grow command
      CALL KENBLE('A',BUTSET)
      CALL KENBLE('B',BUTSET)
      CALL KENBLE('C',BUTSET)
      CALL KENBLE('D',BUTSET)
C
C     Enable input from the bitpad buttons
      CALL GPR_$ENABLE_INPUT(GPR_$BUTTONS,BUTSET,ST)
C
C     Enable input from bitpad  as locator
      CALL GPR_$ENABLE_INPUT(GPR_$LOCATOR,BUTSET,ST)

C     Enable input for the cursor entering and leaving the window
      CALL GPR_$ENABLE_INPUT(GPR_$ENTERED_WINDOW,BUTSET,ST)
      CALL GPR_$ENABLE_INPUT(GPR_$LEFT_WINDOW,BUTSET,ST)

C
C************************************************
C************************************************
C     Initialize all screen control funtions    *
C     for text and menu handling.               *
C************************************************
C************************************************
C
C     make sure corret font used for drawing
      FONTID = APFNTS(1)
      CALL GPR_$SET_TEXT_FONT(FONTID,ST)
C
C     Initialize the graphics text package
      CALL GTINGT(.FALSE.,ST)
C

C******************************************
C      O U T P U T   W I N D O W   N O.  1
C      THIS IS THE NUMBER WINDOW
C******************************************
C
C     Window no.1
C     Initialize a text window,org 415,25
C     with line length40,page length 3
      MN4=1
      MX=15
      MY=155
      LPC=60
      CPM=1
      IF ( DINGKY ) THEN
	     LPC=20
	     MY=155
	  ENDIF
 
      CALL GTIWIN(MN4,MX,MY,LPC,CPM,.TRUE.)
C******************************************
C      O U T P U T   W I N D O W   N O.  2
C      THIS IS THE ERROR WINDOW
C******************************************
C
C     Error Window no.2
C     Initialize a text window,org 415,25
C     with line length40,page length 3
      MN4=2
      MX=15
      MY=180
      LPC=60
      CPM=1

      IF ( DINGKY ) THEN
	     LPC=60
		 MY=180
	  ENDIF

      CALL GTIWIN(MN4,MX,MY,LPC,CPM,.TRUE.)
C
C******************************************
C      M E N U  N O.  2
C******************************************
C
C     the major option menu
      MN4=2
c      ****** miles
      LPC=7
      CPM=13
      CALL GTIMEN(MN4,POSX(2),POSY(2),LPC,CPM,.TRUE.)
C******************************************
C      M E N U  N O.  1
C******************************************
C
C     the REVOKE,QUIT menu
      MN4=1
      LPC=17
      CPM=1
      CALL GTIMEN(MN4,POSX(1),POSY(1),LPC,CPM,.FALSE.)
C
C******************************************
C      M E N U  N O.  3
C******************************************
C
C     initialize the minor option menu
C     org 869,160,24 lines long,16 characters long
      MN4=3
      LPC=17
      CPM=26
      CALL GTIMEN(MN4,POSX(3),POSY(3),LPC,CPM,.FALSE.)
C
C******************************************
C      M E N U  N O.  4
C******************************************
C
C     initialize the display control menu
      MN4=4
      LPC=17
      CPM=9
      CALL GTIMEN(MN4,POSX(4),POSY(4),LPC,CPM,.FALSE.)
C
C
C*******************************************
C
      CALL VIEW(XMIN,YMIN,XMAX,YMAX)
      CALL CLEAR()

C     all systems go.....
C     where ?      n.m  (42)
C     establish fault handler
C     enable asynchronus faults

C
      DAXCAD_FAULT_OCCURED = .FALSE.
C
C     *******************************************
C         TO BE USED AT A LATER DATE PERHAPS 
C     *******************************************

      PRIM_SET = (2 ** GPR_$ROP_BLT)+
     +           (2 ** GPR_$ROP_LINE)+
     +           (2 ** GPR_$ROP_FILL)

      CALL GPR_$RASTER_OP_PRIM_SET(PRIM_SET,ST)
C
      CALL MAKEDIALOGS(ST)
C     make sure corret font used for drawing
      CALL SET_FONT(1,ST)
      FONTID = APFNTS(1)
      CALL GPR_$SET_TEXT_FONT(FONTID,ST)
        print*, 'viewport',xmin,ymin,xmin,xmax
      RECT(1,1)=INT(XMIN)
      RECT(2,1)=INT(YMIN)
      RECT(1,2)=INT(XMAX-XMIN)
      RECT(2,2)=INT(YMAX-YMIN)
      CALL GPR_$SET_CLIP_WINDOW(RECT,ST)


      IF ( XVERSION ) THEN
C
C         Give the xwindow some information
C
          NAME='1986-2026 - DAXCAD Version 5.1 - Cygwin Linux X86 64'
          I2 = NLEN(NAME)
	      CALL GPR_$SET_BANNER(DISPDE,NAME,I2,ST)
          NAME= PRODUCT
          CALL FOLDUP(NAME)
          I4 = NLEN(NAME)
	      CALL GPR_$SET_ICON_NAME(NAME,I4,ST)
          CALL GPR_$SERVER_FLUSH_X(ST)
          CALL GPR_$SET_INPUT_FOCUS(ST)
      ENDIF
C
      RETURN
C
 999  CALL ERROR_$PRINT(ST)
      STOP
      END


      SUBROUTINE PROMPTOUT(TEXT,LENGTH)
C     =================================
C1    VARYPE               C*(*)  I4
C1    IOSTAT                 I    I
C
C2    Outputs text to a pad from which daxcad ran up
C2    This will be either an Apollo PAD or an Xterm
C2  
C2  
C2    Arguments:-
C2  
C2    TEXT      ->      The text to be output
C2    LENGTH    ->      THE length of the text ( C code )
C2  
C2  
C2    Error Returns:NONE
C2  
C2  
C2	 
C
      CHARACTER*256 TEXT
      INTEGER*4 LENGTH
      INTEGER*4 ST
      INTEGER*2 II
C
      CALL GPR_$FORCE_RELEASE(II,ST)
C
      IF ( LENGTH .EQ. 0 ) THEN
           WRITE(*,'(A)') '> '
      ELSE
           WRITE(*,'(2A)') '> ',TEXT(1:LENGTH)
      ENDIF
C
C
C     Reaquiree display still crap
C
      CALL ACRDIS(II)

      END


      SUBROUTINE ERROROUT(TEXT,LENGTH)
C     ================================
C1    VARYPE               C*(*)  I4
C1    IOSTAT                 I    I
C
C2    Outputs text to a pad from which daxcad ran up
C2    This will be either an Apollo PAD or an Xterm
C2    This normally goes to the error window
C2  
C2    Arguments:-
C2  
C2    TEXT      ->      The text to be output
C2    LENGTH    ->      THE length of the text ( C code )
C2  
C2  
C2    Error Returns:NONE
C2  
C2  
C2	 
C
      CHARACTER*256 TEXT
      INTEGER*4 LENGTH
      INTEGER*4 ST
      INTEGER*2 II
C
      CALL GPR_$FORCE_RELEASE(II,ST)
C
      IF ( LENGTH .EQ. 0 ) THEN
           WRITE(*,'(A)') ' '
           WRITE(*,'(A)') '*'
           WRITE(*,'(A)') ' '
      ELSE
           WRITE(*,'(A)') ' '
           WRITE(*,'(2A)') '* ',TEXT(1:LENGTH)
           WRITE(*,'(A)') ' '
      ENDIF
C
C
C     Reaquiree display still crap
C
      CALL ACRDIS(II)


      END


C
      SUBROUTINE TOOLS_SET_CURSOR(POS,ST)
C     ===================================
C1    VARYPE                      2I2 I4
C1    IOSTAT                       I  O
C
C2    Sets the global cursor postion of the 
C2    system cursor if there is one.
C2  
      include  'include/gpr.ins.inc'
      include  'include/obswin.inc'
C
      INTEGER*2 POS(2)
      INTEGER*4 ST
C
      CALL GPR_$SET_CURSOR_POSITION(POS,ST)
C
      END
C
      SUBROUTINE TOOLS_SYSTEM_CURSOR(ON,ST)
C     =====================================
C1    VARYPE                         L  I4
C1    IOSTAT                         I  O
C
C2    Sets the system cursor on or off
C2  
      include  'include/gpr.ins.inc'
      include  'include/obswin.inc'
C
      LOGICAL ON
      INTEGER*4 ST
C
      CALL GPR_$SET_CURSOR_ACTIVE(ON,ST)
C
      END

      SUBROUTINE GPR_$SET_TITLE(BITMAP,TEXT,LENGTH,ST)
C     ================================================
C1    VARYPE                     I4     C*(*) I2   I4
C1    IOSTAT                     I      I     I    O 
C
C2    Sets the title of the drawing
C2  
C2  
C2    Arguments:-
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
      include  'include/daxcolor.inc'
      include  'include/interface.inc'
      INTEGER*4 ST
      INTEGER*4 BITMAP
      INTEGER*4 LEN1
      INTEGER*4 COL
      INTEGER*2 LENGTH
      CHARACTER*(*) TEXT
C
      COL = COLFOR
      LEN1 = LENGTH
      CALL TEXTINWINDOW(2,1,COL,TEXT,LEN1)
      END

      SUBROUTINE KEY_TIMEOUT(TIMEO,STRING,COUNT,OK)
C     ==============================================
C1    VARTYPE                 R     C*(*)   I4   L
C1    IOSTAT                    I    I/0   I    O
C
C2    This routine will wait a specified number of APOLLO
C2    clock Jiffies (Real men come in a jiffy)
C2    Between key presses and builds up a string based
C2    on that time. Will be used for searces of the 
C2    current file list
C                                                  
      include  'include/gpr.ins.inc'
      include  'include/apollo.inc'
C
      INTEGER*2 TIMEC(3),EVTYPE,SCRPOS(2),CURPOS(2)
      INTEGER*4 ST
      INTEGER*4 COUNT
      CHARACTER EVDATA
      CHARACTER*(*) STRING
      DOUBLE PRECISION SEX,SEX2,ELAP
      REAL TIMEO
      LOGICAL OK,LWAIT

C
C     reset string counter
      COUNT = 1
C     get the starting time
      ELAP = 0
      CALL UNIXTIME(SEX)
C     start of loop
10    CONTINUE
C     Ensure cursor is in the daxcad pad so that we will see the event of the
C     get an event if possible
      LWAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
C
C     sample the time after polling
      CALL UNIXTIME(SEX2)
C                                            
C     ealapsed time calculation in apollo jiffies
      ELAP = SEX2-SEX
      IF(ELAP.GT.TIMEO) THEN
C         timout has occured between keypresses
          OK = .TRUE.
          RETURN
      ENDIF
      IF(EVTYPE.EQ.GPR_$KEYSTROKE) THEN
C         reset staring time for timeout
          ELAP = 0
          CALL UNIXTIME(SEX)
          COUNT = COUNT + 1
          IF(COUNT.GT.10) THEN
              OK = .FALSE.
              RETURN
          ENDIF
          STRING(COUNT:COUNT) = EVDATA
      ENDIF


      GOTO 10
      END

      SUBROUTINE DGWAIT(TIMEO)
C     ========================
C1    VARTYPE             R  
C1    IOSTAT              I 
C
C2    This routine is a replacement for WAIT to get the 
C2    resolution of time down beyond 1 second
C2    
C2  
C2    Arguments:-
C2  
C2    TIMEO        ->      the number of seconds to  wait.
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C                                                  
      include 'include/apollo.inc'
      include 'include/gpr.ins.inc'
C
      INTEGER*4 ST
      DOUBLE PRECISION SEX
      DOUBLE PRECISION SEX2
      DOUBLE PRECISION ELAP
      REAL TIMEO
C
C     get the starting time
      ELAP = 0.0
      CALL UNIXTIME(SEX)
C     start of loop
10    CONTINUE
C
C     Call elapsed time check
C
      CALL UNIXTIME(SEX2)
C                                            
C     ealapsed time calculation in jiffies
      ELAP = SEX2-SEX
      IF(ELAP.GT.TIMEO) THEN
C         time has passed
          RETURN
      ENDIF
      GOTO 10
      END
 

      SUBROUTINE POPPD1(FILNAM,WNORGX,WNORGY,WNWID,WNHGT)
C     ===================================================
C1    VARTYPE            C*(*)  I4      I4    I4    I4
C1    IOSTATUS             I    I       I     I     I
C
C2    Subroutine POPPD1 pops a pad on the APOLLO screen
C2    showing the text in the file FILNAM.The top left
C2    of the pad is located at screen coordinates
C2    WNORGX,WNORGY and is of width WNWID and height
C2    WNHGT.
C2  
C2    We have changed this to use the info dialog system
C2  
C2    Arguments:-
C2  
C2    FILNAM      ->      File to display information.
C2    WNORGX      ->      Origin X of file
C2    WNORGY      ->      Origin Y of file
C2    WNWID       ->      Width of window
C2    WNGHT       ->      Height of window
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include 'include/macro.inc'
      include 'include/vntable.inc'
C
      INTEGER*2 WINPOS(4),STRID,WINLST(4,10),WLSZ,WNCNT,TT,TT1,FNL
      INTEGER*2 I
      INTEGER*4 ST,WNORGX,WNORGY,WNWID,WNHGT,NLEN
      INTEGER*4 UNITF
      INTEGER*4 LINEC
      INTEGER*4 NLINE
      PARAMETER(NLINE=256)
      LOGICAL OK
      CHARACTER*(*) FILNAM
      CHARACTER*80 ERRORM
      CHARACTER*80 STRING
      CHARACTER*80 BUFFER(NLINE)
C
      IF ( MACOP ) RETURN
C
      CALL FINDU1(UNITF,OK)
      IF ( .NOT.OK ) THEN
          RETURN
      ENDIF
C
      BUFFER(1) = FILNAM(1:NLEN(FILNAM))//' Is Empty'
      BUFFER(256) = 'No More Information Available. Read File'
C
      OPEN(UNIT=UNITF,
     +     FILE=FILNAM,
     +     STATUS='OLD',
     +     ERR=999)
C
      REWIND(UNITF)
C
      LINEC = 0
C
100   CONTINUE
C
      READ(UNIT=UNITF,FMT='(A)',END=200) STRING
C     increment pointer
      IF ( LINEC.EQ.255 ) THEN
          GOTO 200
      ENDIF
      LINEC = LINEC + 1
      BUFFER(LINEC) = STRING
      GOTO 100
C
200   CONTINUE
      CLOSE(UNITF)
      IF ( LINEC.EQ.0 ) THEN
          LINEC = 1
      ENDIF
C     get the information pad up
      CALL INFODIALOG(BUFFER,LINEC)
      RETURN
C
999   CONTINUE
      WRITE(ERRORM,'(2A)',ERR=998) DICT01(287)(1:NLEN(DICT01(287))),
     +                             FILNAM(1:NLEN(FILNAM))
C     Warn on screen only
      CALL EPRINT(ERRORM)
998   CONTINUE
C
      END
C
C
  

      SUBROUTINE PATTERN()
C     ====================
C2    This routine allocates a bitmap for entity marking
C
      include 'include/gpr.ins.inc'
      include 'include/fill_pattern.inc'
      include 'include/apollo.inc'
      include 'include/interface.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*4 STATUS,AB_FILL,AB_BACK,ST,BM_DESC
      INTEGER*4 COL
      INTEGER*2 WINDOW(4),BMSIZE(2),PLMASK
      INTEGER*2 FILL_VALUE,CENTER(2),RAD
      INTEGER*2 BACK_VALUE,SCALE
      INTEGER*4 I,J
C 
      LOGICAL UNOBSCURED
C 
C     set fill bitmap size
      BMSIZE(1) = 32
      BMSIZE(2) = 32
      PLMASK = -1
      SCALE = 1
 
C     acquire display now
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(STATUS)
C
C     allocate an attributre block for each bitmap
C
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(AB_FILL,STATUS)
C 
      CALL GPR_$ALLOCATE_BITMAP
     +     (BMSIZE,HIPLAN,AB_FILL,FILL_BITMAP,STATUS)
C 
      CALL GPR_$SET_BITMAP(FILL_BITMAP,STATUS)
C
      CALL GPR_$SET_ATTRIBUTE_BLOCK(AB_FILL,STATUS)
C
      CALL GPR_$SET_PLANE_MASK(PLMASK,STATUS)
C
      CALL ROPREP()
      COL = COLBAK
      CALL TOOLPEN(COL)
C
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = 32
      WINDOW(4) = 32
C
      CALL GPR_$RECTANGLE(WINDOW,STATUS)
C
      COL = COLFOR
      CALL TOOLPEN(COL)

 
C     create pattern bitap
      DO 100 J=0,33,4
 
          DO 200 I=0,33,4
 
              WINDOW(1) = I
              WINDOW(2) = J
              WINDOW(3) = 2
              WINDOW(4) = 2
 
              CALL GPR_$RECTANGLE(WINDOW,STATUS)
 
              WINDOW(1) = I + 2
              WINDOW(2) = J + 2
              WINDOW(3) = 2
              WINDOW(4) = 2
 
              CALL GPR_$RECTANGLE(WINDOW,STATUS)
 
 200      CONTINUE
 
100   CONTINUE
 
 
C     reset current bitmap
 
      CALL GPR_$SET_BITMAP(DISPDE,STATUS)
 
      CALL GPR_$RELEASE_DISPLAY(ST)
 
 
      END

      INTEGER*4 FUNCTION DAXCADREPAINT(ST)
C     ===========================
C1    VARYPE                   I4
C1    IOSTAT                   O
C
C2    Fires off repaint of DAXCAD. 
C2    
C2  
C2    Arguments:-
C2  
C2    NONE currently
C2  
C2  
C2  
C2    Error Returns: 
C2
C2  
C2  
C2  
C2  
C
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/daxcad_x.inc'
      include 'include/shadow.inc'
      include 'include/bitmap.inc'
      include 'include/interface.inc'
      include 'include/frames.inc'
      include 'include/cursor.inc'
      include 'include/dialog.inc'
      include 'include/menpop.inc'
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      INTEGER*4 INTWIN(4)
      INTEGER*4 ST
      INTEGER*4 WHITE
      INTEGER*4 BLACK
      INTEGER*4 CURNUM
      INTEGER*4 DESC
      INTEGER*4 NLEN
      INTEGER*4 COUNT
      INTEGER*4 I
      INTEGER*4 MNCODE
      INTEGER*4 IX,IY
      INTEGER*4 CURPOS(2)
C
      INTEGER*2 SCALE
      INTEGER*2 II
      INTEGER*2 TVPN
      INTEGER*2 XP,YP
      INTEGER*2 POS(2)
      INTEGER*2 PLANES
      INTEGER*2 ROOT(2)
      INTEGER*2 WIN(4)
      INTEGER*2 PADNUM
C
      CHARACTER*256 BUFF
C
      LOGICAL REPAINT
      LOGICAL STATE
C
      EXTERNAL NLEN
C
C     check status of repaint flag and set
C
C
      CALL RELDIS(II)
C
C
C
      WIN(1) = 0
      WIN(2) = 0
      WIN(3) = ROOTW(1)
      WIN(4) = ROOTW(2)

      PADNUM = APOLLO_PAD
C
      IF ( .NOT.REPAINTPENDING ) THEN
          CALL RESIZE_WINDOW(PADNUM,DISPDE,ROOTW,ST)
      ELSE
          REPAINTPENDING = .FALSE.
      ENDIF
C
C     if the system is going to repaint due to window size go
C     back and wait
C
      CALL ACRDIS(II)

      IF ( REPAINTPENDING ) THEN
          RETURN
      ENDIF
      IF ( XVERSION ) THEN
C         Collect any outstanding X window Expose events
          CALL GPR_$COLLECT_EXPOSURES(COUNT,st)
      ENDIF
C     get current cursor information
      CALL INQ_CURSOR(CURNUM,CURPOS,ST)
      CALL INQ_CURSOR_STATE(STATE,ST)
C
C     turn off regardless
C
      CALL CURSOR_ON(.FALSE.)
C
C     First pass regeneterate DAXCAD window
C
C     Give a nice grey background
C
C
C     Turn of any inputs that are active
C
      CALL SET_INPUT_OFF()
C

      CALL ROPREP()
      CALL TOOLPEN(MENUF)
C
      SCALE = 1
      CALL GPR_$SET_FILL_PATTERN (SHADBT,SCALE,ST)
      CALL GPR_$RECTANGLE(WIN,ST)
      CALL GPR_$SET_FILL_PATTERN (gpr_$nil_bitmap_desc,SCALE,ST)
C 
      DO 10 I = 1,4
         CALL DAXMPAINT(I)
10    CONTINUE
C
C     redraw all the cells and their stat
C
      CALL MPAINT(ST)
C
C     ** New interface part of main DAXCAD screen **
C
C     Turn off frames and redraw
C
      DIAGCR = 0
C
      FRMDIS(1) = .FALSE.
      FRMDIS(2) = .FALSE.
C
      CALL FRMDRW(1,.TRUE.,ST)
      CALL FRMDRW(2,.TRUE.,ST)
C
C     repaint text in frames
      BUFF = FRMTXT(2)
      CALL TEXTINWINDOW(2,1,MENUF,BUFF,NLEN(BUFF))
      BUFF = FRMTXT(1)
      CALL TEXTINWINDOW(1,1,MENUF,BUFF,NLEN(BUFF))
C
C     set backcloth true
C
      CALL WINDOW_SET(IX,IY,.TRUE.)
C
      CALL CLEAR()
C
      CALL REDRAW()
C
C     make sure viewports reset
C
      TVPN = CVPN
C
      IF(.NOT.MAWS) THEN
C         save portion of bitmap back to backing store
          CALL WNSVBT()
          IF(CVPN.EQ.0) THEN
C             repaint all windows
              CALL WNPNTA()
          ENDIF
      ENDIF
C     DAXCAD popup repainting
C
      IF ( LSTMNC.NE.0) THEN
C         delocate bitmaps and reallocate for possible new fonts
          CALL MENPOP_INIT_GR(.TRUE.,ST)
          XP = STARTX
          YP = STARTY
          MNCODE = LSTMNC
          CALL MENPOP_PAINT(MNCODE,XP,YP,.TRUE.)
      ELSE
          CALL TOOLS_REPAINT(ST)
      ENDIF

C
C     Final cursor repaint
C
      CURSLN(1,1) = VXMIN
      CURSLN(2,1) = VYMIN
      CURSLN(3,1) = VXMAX - VXMIN
      CURSLN(4,1) = VYMAX - VYMIN
      IF ( STATE ) THEN
          CALL CURDRW(CURNUM,ST)
      ENDIF
C     clear repaint flag
      REPAINTPENDING = .FALSE.
C

      END

C
      SUBROUTINE MPAINT(ST)
C     =====================
C1    VARYPE            I4
C1    IOSTAT            O
C
C2    Redraws all the contents of each window and menu
C2  
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 ST
      INTEGER*4 MENU
      INTEGER*4 CELL
      INTEGER*4 TCELL
      LOGICAL HI
C
      CHARACTER*80 WORD
      CHARACTER    TOKEN
C
C     All cells + Special highlite
      DO 10 MENU = 1,4
          DO 20 CELL = 0,31
              IF ( GMCVNM(MENU,CELL).GT.0) THEN
                  GTMULT = GMCMUL(MENU,CELL)
                  WORD = GMB(MENU,CELL)
                  HI = GMCINV(MENU,CELL)
                  TOKEN = GMBC(MENU,CELL)
                  TCELL = CELL+1
                  CALL GTPMEN(WORD,TOKEN,MENU,TCELL)
                  IF ( HI ) THEN
C                     This will invert the cell if currently hightlight
                      GMCINV(MENU,CELL) = .FALSE.
                      TCELL = CELL+1
                      CALL GTMCHI(MENU,TCELL)
                  ENDIF
              ENDIF
20        CONTINUE
10    CONTINUE
C
      END


      SUBROUTINE TOOLS_REPAINT(ST)
C     =================================
C1    VARYPE                   I4
C1    IOSTAT                    O
C
C2    Repaints all the daxcad tools
C  
      include  'include/dialog.inc'
      include  'include/popup.inc'
      include  'include/interface.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      INTEGER*4 I
      INTEGER*4 TMP
      INTEGER*4 PAGENO
      INTEGER*4 MNCODE
      INTEGER*4 XP,YP
      INTEGER*4 DIAG_TMP(0:MAXDIA)
C
C     redraw the dialog boxes
C
C     copy the current stack
      DO 10 I=0,DIAG_POINT
          DIAG_TMP(I) = DIAG_STACK(I) 
 10   CONTINUE
C
      TMP = DIAG_POINT
      DIAG_POINT = 0
      DIAGCR = 0
C     redraw from stack
C
      DO 20 I=1,TMP
          DIANUM = DIAG_TMP(I) 
C         deallocate background bitmap and set to false
          DIADIS(DIANUM) = .FALSE.
          CALL BITMAP_KILL(DIAGBT(DIANUM),ST)
          CALL DIAGDR(DIANUM,.TRUE.,ST)
          CALL BUTTONS_REPAINT(DIANUM, ST)
          CALL LABEL_REPAINT(DIANUM, ST)
          CALL INPUT_REPAINT(DIANUM, ST)
          CALL FRAME_REPAINT(DIANUM, ST)
          IF ( DIANUM.EQ.5 ) THEN
C             Draw the browseng frame sepraratly
              PAGENO = BRPAGE
              CALL BROWSEPAGE(PAGENO,ST)

          ENDIF


 20   CONTINUE

      IF ( POPUP_CURRENT.GT.0) THEN
          MNCODE = POPUP_CURRENT
          CALL POPUP_PAINT(MNCODE,XP,YP,.TRUE.,.TRUE.,ST)
      ENDIF


C
      END
C
C-----------------------------------------------------------------------------------------
C
C                     
      SUBROUTINE BUTTONS_REPAINT(DIANUM, ST)
C     =================================
C1    VARYPE                   I4     I4
C1    IOSTAT                   I       O
C
C     redraw the buttons with the parent dialog
C     box DIANUM
C
      include    'include/buttons.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      INTEGER*4 BUTNUM                   
      LOGICAL DISP
C
      DISP = .TRUE.
C 
      DO 10 BUTNUM=1, BUTMAX
C        part of current dialog and visible ?
         IF(PARENT_DIALOG(BUTNUM) .EQ. DIANUM .AND. BUTDIS(BUTNUM)) THEN
C          draw it !!
           CALL BUTTON_DRAW(BUTNUM,DISP,ST)
C          hilite if required
           IF(BUTHI(BUTNUM)) THEN
             BUTHI(BUTNUM) = .FALSE.
             CALL BUTTON_HIGH(BUTNUM,ST)
           ENDIF
         ENDIF
 10   CONTINUE
C
      END
C
C-----------------------------------------------------------------------------------------
      SUBROUTINE LABEL_REPAINT(DIANUM, ST)
C     =================================
C1    VARYPE                   I4     I4
C1    IOSTAT                   I       O
C
C     redraw the buttons with the parent dialog
C     box DIANUM
C
      include    'include/label.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      INTEGER*4 LABNUM                   
      LOGICAL DISP
C
      DISP = .TRUE.
C 
      DO 10 LABNUM=1, LABMAX
C        part of current dialog and visible ?
         IF(LABDIA(LABNUM) .EQ. DIANUM .AND. LABDIS(LABNUM)) THEN
C          draw it !!
           LABDIS(LABNUM) = .FALSE.
           CALL LABDRW(LABNUM,DISP,ST)
         ENDIF
 10   CONTINUE
C
      END

      SUBROUTINE FRAME_REPAINT(DIANUM, ST)
C     ====================================
C1    VARYPE                   I4     I4
C1    IOSTAT                   I       O
C
C     redraw the frames with the parent dialog
C     box DIANUM
C
      include    'include/frames.inc'
      include    'include/daxcolor.inc'
      include    'include/interface.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      INTEGER*4 FRMNUM                   
      INTEGER*4 NLEN
      LOGICAL DISP
      CHARACTER*256 BUFF
C
      EXTERNAL NLEN
C
      DISP = .TRUE.
C 
      DO 10 FRMNUM=1, MAXFRM
C        part of current dialog and visible ?
         IF(FRMDIA(FRMNUM) .EQ. DIANUM .AND. FRMDIS(FRMNUM)) THEN
C          draw it !!
           FRMDIS(FRMNUM) = .FALSE.
           CALL FRMDRW(FRMNUM,DISP,ST)
           BUFF = FRMTXT(FRMNUM)
           CALL TEXTINWINDOW(FRMNUM,1,MENUF,BUFF,NLEN(BUFF))
         ENDIF
 10   CONTINUE
C
      END

      SUBROUTINE INPUT_REPAINT(DIANUM,ST)
C     ===================================
C1    VARYPE                     I4   I4
C1    IOSTAT                     I    O
C
C2    Repaint the inputs asocated with the current dialog box
C2  
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    DIANUM	->	Dialog box number 
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include    'include/input.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      INTEGER*4 INPNUM
      INTEGER*4 TINPN
      LOGICAL DISP
C
C
      DISP = .TRUE.
C 
      DO 10 INPNUM=1, INPMAX
C        part of current dialog and visible ?
         IF(INPDIA(INPNUM) .EQ. DIANUM .AND. INPDIS(INPNUM)) THEN
C          draw it !!
           INPDIS(INPNUM) = .FALSE.
           CALL INPDRW(INPNUM,DISP,ST)
         ENDIF
 10   CONTINUE
C


      END


