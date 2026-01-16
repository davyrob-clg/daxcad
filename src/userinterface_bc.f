C
C     @(#)  412.1 date 6/11/92 userinterface.f 
C
C
C     Filename    : userinterface.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:30
C     Last change : 92/06/11 14:42:40
C
C     Copyright : Practical Technology International Limited  
C     File :- userinterface.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C


      SUBROUTINE MAKEDIALOGS(ST)
C     ===========================
C1    VARYPE                  I4
C1    IOSTAT                  O
C
C2    Build dialog boxes for DAXCAD
C2  
C2  
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

      include 'include/daxcolor.inc'
      include 'include/interface.inc'
      include 'include/bitmap.inc'
      include 'include/vntable.inc'
      include 'include/cursor.inc'
      include 'include/daxcad_x.inc'


      INTEGER*4 ST
      INTEGER*4 RECT(4)
      INTEGER*4 OX,OY
      INTEGER*4 X,Y
      INTEGER*4 COLF
      INTEGER*4 COLB
      INTEGER*4 NLEN
      INTEGER*4 FONT
      INTEGER*4 CURSOR
      INTEGER*4 POPNUM
      INTEGER*4 TYPE
      INTEGER*4 SIZE(2)
      INTEGER*4 BUTCOLB
      INTEGER*4 COLS(3,0:255)
C
      LOGICAL UPSTR
C
      CHARACTER*30 BUTTONS(25)
      CHARACTER*60 BANNER
C
      EXTERNAL NLEN
C 
      BUTTONS(1) = 'Quit '
      BUTTONS(2) = 'Done '
      BUTTONS(3) = 'Clear'
      BUTTONS(4) = 'Again'

      BUTTONS(5) = 'Confirm'
      BUTTONS(6) = 'Cancel '
      BUTTONS(23) = ' Abort '

      BUTTONS(7) = ' DONE '

      BUTTONS(8) = 'Up'
      BUTTONS(9) = 'Down'
      BUTTONS(10) = 'Home'

      BUTTONS(15) = 'Components'

      BUTTONS(11) = 'Files'
      BUTTONS(12) = 'Browse'
      BUTTONS(13) = 'Done'
      BUTTONS(14) = 'Quit'

      COLF = MENUF
      COLB = MENUB
      BUTCOLB = MENUB

      DIALOG1(1) = ROOTW(1)/2 - 700/2
      DIALOG1(2) = ROOTW(2)/2 - 200/2
      DIALOG1(3) = 700
      DIALOG1(4) = 200

      DIALOG2(1) = ROOTW(1)/2 - 700/2
      DIALOG2(2) = ROOTW(2)/2 - 130/2
      DIALOG2(3) = 700
      DIALOG2(4) = 130

      DIALOG3(1) = ROOTW(1)/2 - 800/2
      DIALOG3(2) = ROOTW(2)/2 - 130/2
      DIALOG3(3) = 800
      DIALOG3(4) = 130

      DIALOG4(1) = ROOTW(1)/2 - 700/2
      DIALOG4(2) = ROOTW(2)/2 - 500/2
      DIALOG4(3) = 700
      DIALOG4(4) = 500
C
C     Browser dialog
C
      DIALOG5(1) = ROOTW(1)/2 - 1010/2
      DIALOG5(2) = ROOTW(2)/2 - 700/2
      DIALOG5(3) = 1010
      DIALOG5(4) = 700

C
C     General infomartion dialog ( Will be resized to suit text )
C
      DIALOG6(1) = ROOTW(1)/2 - 10/2
      DIALOG6(2) = ROOTW(2)/2 - 700/2
      DIALOG6(3) = 1020
      DIALOG6(4) = 700

      IF ( ROOTW(1).GT.1024) THEN
          FRAME2(1) = 180
          FRAME2(2) = 5
          FRAME2(3) = ROOTW(1) - 200
          FRAME2(4) = 30
      ELSE
          FRAME2(1) = 180
          FRAME2(2) = 5
          FRAME2(3) = 650
          FRAME2(4) = 30
      ENDIF


      IF ( ROOTW(1).GT.1024) THEN
           FRAME1(1) = 10
           FRAME1(2) = 50
           FRAME1(3) = ROOTW(1) - 30
           FRAME1(4) = 30
      ELSE
           FRAME1(1) = 10
           FRAME1(2) = 50
           FRAME1(3) = 835
           FRAME1(4) = 30
      ENDIF

      FRAME3(1) = DIALOG5(1) + 20
      FRAME3(2) = DIALOG5(2) + DIALOG5(4) - 90
      FRAME3(3) = DIALOG5(3) - 40
      FRAME3(4) = 30

C
C     Standard input dialog defs
C
      CALL DIAG_DEFINE(1,DIALOG1,DIALOGF,DIALOGB,ST)
      CALL DIAG_DEFINE(2,DIALOG2,DIALOGF,DIALOGB,ST)
      CALL DIAG_DEFINE(3,DIALOG3,DIALOGF,DIALOGB,ST)
      CALL DIAG_DEFINE(4,DIALOG4,DIALOGF,DIALOGB,ST)
      CALL DIAG_DEFINE(5,DIALOG5,DIALOGF,DIALOGB,ST)
      CALL DIAG_DEFINE(6,DIALOG6,DIALOGF,DIALOGB,ST)

      CALL FRAME_DEFINE(1,FRAME1,2,COLF,COLB,0,ST)
      CALL FRAME_DEFINE(2,FRAME2,2,COLF,COLB,0,ST)
      CALL FRAME_DEFINE(3,FRAME3,2,COLF,COLB,0,ST)

      OX = DIALOG1(1)
      OY = DIALOG1(2)

      CALL INPUT_MAKE(OX+20,OY+110,1,COLF,BUTCOLB,1,660,1,ST)

      OX = DIALOG1(1) + DIALOG1(3) - 5
      OY = DIALOG1(2) + 150

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 2,1,
     +                 BUTTONS(1),5,2,1,ST)

      CALL BUTTON_MAKE(OX-60,OY,1,
     +                 COLF,BUTCOLB,
     +                 2,1,
     +                 BUTTONS(2),5,2,2,ST)

      CALL BUTTON_MAKE(OX-120,OY,1,
     +                 COLF,BUTCOLB,
     +                 2,1,
     +                 BUTTONS(3),5,2,3,ST)

      CALL BUTTON_MAKE(OX-180,OY,1,
     +                 COLF,BUTCOLB,
     +                 2,1,
     +                 BUTTONS(4),5,2,4,ST)

      CALL BUTTON_MAKE(OX-180,OY,1,
     +                 COLF,BUTCOLB,
     +                 2,1,
     +                 BUTTONS(4),5,2,4,ST)

      LASTBUF = ' '
C
C     Yes/No dialog
C
      OX = DIALOG2(1)
      OY = DIALOG2(2)

      CALL BUTTON_MAKE(OX+20,OY+90,1,
     +                 COLF,BUTCOLB,
     +                 3,0,
     +                 BUTTONS(5),7,0,5,ST)

      OX = DIALOG2(1) + DIALOG2(3)/2

      CALL BUTTON_MAKE(OX,OY+90,1,
     +                 COLF,BUTCOLB,
     +                 2,0,
     +                 BUTTONS(23),7,1,23,ST)

      OX = DIALOG2(1) + DIALOG2(3) - 10

      CALL BUTTON_MAKE(OX,OY+90,1,
     +                 COLF,BUTCOLB,
     +                 2,0,
     +                 BUTTONS(6),7,2,6,ST)

C
C     Error dialog
C


      OX = DIALOG3(1) + DIALOG3(3)/2
      OY = DIALOG3(2) + 90
      CALL BUTTON_MAKE(OX+20,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,0,
     +                 BUTTONS(7),6,1,7,ST)
C
C     Standard file selection dialog
C
      TYPE = 2
      BANNER = 'File Selection PoPuP'
      FONT = 1
      UPSTR = .TRUE.
      CURSOR = 0
      POPNUM = 1

      CALL MAKE_POPUP(BANNER,
     +                COLF,COLB,
     +                FONT,
     +                UPSTR,
     +                TYPE,
     +                './',2,
     +                '.*',2,
     +                CURSOR,
     +                POPNUM,ST)

      BUTTONS(8) = '  Up  '
      BUTTONS(9) = ' Down '
      BUTTONS(10) = ' Home '

      BUTTONS(15) = 'Components'

      BUTTONS(11) = 'Files'
      BUTTONS(12) = 'Browse'
      BUTTONS(13) = 'Done'
      BUTTONS(14) = 'Quit'

C
C     File selector dialog
C
      OX = DIALOG4(1)
      OY = DIALOG4(2)

      CALL INPUT_MAKE(OX+20,OY+100,2,COLF,BUTCOLB,1,660,1,ST)

      OX = OX+20
      OY = OY + 130
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(8),6,0,8,ST)

      CALL BUTTONSIZE(8,SIZE)
      OX = OX + SIZE(1)
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(9),6,0,9,ST)

      CALL BUTTONSIZE(9,SIZE)
      OX = OX + SIZE(1)
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(10),6,0,10,ST)
C
C     File type and serach criteria 
C
      OY = OY + 110
      OX = DIALOG4(1) + 20
      CALL BUTTON_MAKE(OX,OY-15,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(15),NLEN(BUTTONS(15)),0,15,ST)

      OX = DIALOG4(1) + 300
      CALL INPUT_MAKE(OX,OY,3,COLF,BUTCOLB,1,200,1,ST)
C
C     File selection and browse buttons
C
      OX = DIALOG4(1) + 25
      OY = OY + 110
      CALL INPUT_MAKE(OX,OY,4,COLF,BUTCOLB,1,660,1,ST)

      OY = OY + 30
      CALL BUTTON_MAKE(OX-5,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(11),NLEN(BUTTONS(11)),0,11,ST)

      CALL BUTTONSIZE(11,SIZE)
      OX = OX + SIZE(1)

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(12),NLEN(BUTTONS(12)),0,12,ST)

      CALL BUTTONSIZE(12,SIZE)
C
C     DONE/QUIT Buttons
C
      OX = DIALOG4(2) + DIALOG4(3) - 20
      OY = DIALOG4(2) + DIALOG4(4) - 40

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(14),NLEN(BUTTONS(14)),2,14,ST)

      CALL BUTTONSIZE(14,SIZE)
      OX = OX - SIZE(1)

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(13),NLEN(BUTTONS(13)),2,13,ST)


      TYPE = 3
      BANNER = 'Select Directory'
      FONT = 1
      UPSTR = .TRUE.
      CURSOR = 0
      POPNUM = 2

      CALL MAKE_POPUP(BANNER,
     +                COLF,COLB,
     +                FONT,
     +                UPSTR,
     +                TYPE,
     +                './',2,
     +                '.*',2,
     +                CURSOR,
     +                POPNUM,ST)

      TYPE = 1
      BANNER = VNOUN(624)
      FONT = 1
      UPSTR = .TRUE.
      CURSOR = 0
      POPNUM = 3

      CALL MAKE_POPUP(BANNER,
     +                COLF,COLB,
     +                FONT,
     +                UPSTR,
     +                TYPE,
     +                './',2,
     +                '.*',2,
     +                CURSOR,
     +                POPNUM,ST)


C
C     Browser system interface
C
      BUTTONS(16) = 'Next Page'
      BUTTONS(17) = 'Previous Page'
      BUTTONS(18) = 'Goto Page'
      BUTTONS(19) = 'Done'
      BUTTONS(20) = 'Quit'

      OX = DIALOG5(1)
      OY = DIALOG5(2) + DIALOG5(4)

      OX = OX+30
      OY = OY - 40
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(16),NLEN(BUTTONS(16)),0,16,ST)

      CALL BUTTONSIZE(16,SIZE)
      OX = OX + SIZE(1)
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(17),NLEN(BUTTONS(17)),0,17,ST)

      CALL BUTTONSIZE(17,SIZE)
      OX = OX + SIZE(1)
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(18),NLEN(BUTTONS(18)),0,18,ST)

      CALL BUTTONSIZE(18,SIZE)
C
      BRPAGX = OX + SIZE(1) + 20
      BRPAGY = OY + 20
C
C     DONE/QUIT Buttons for browser
C
      OX = DIALOG5(2) + DIALOG5(3) - 60
      OY = DIALOG5(2) + DIALOG5(4) - 40

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(20),NLEN(BUTTONS(20)),2,20,ST)

      CALL BUTTONSIZE(20,SIZE)
      OX = OX - SIZE(1)

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(19),NLEN(BUTTONS(19)),2,19,ST)
C
C     user interface frames replacing KRs old system
C
C      CALL MAKE_COLORMAP(1,COLS,ST)

C
C     Information dialog buttons
C
      BUTTONS(21) = 'Page Up'
      BUTTONS(22) = 'Page Down'

      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(21),NLEN(BUTTONS(21)),2,21,ST)
      CALL BUTTON_MAKE(OX,OY,1,
     +                 COLF,BUTCOLB,
     +                 3,1,
     +                 BUTTONS(22),NLEN(BUTTONS(22)),2,22,ST)
C

C
C     Make a cross hair cursor
C
      CALL CURSOR_MAKE(1,CURSOR_XHAIR,RECT,'A',0,COLF,.TRUE.,ST)
C
C     Other viewport cursor a cross 
C
      RECT(1) = DAXCURS(2)
      RECT(2) = DAXCURS(3)
      CALL CURSOR_MAKE(2,CURSOR_SCROSS,RECT,'A',3,COLF,.TRUE.,ST)
      IF ( .NOT.XVERSION ) THEN
C
C         Menu cursor
C
          RECT(1) = 20
          RECT(2) = 20
          CALL CURSOR_MAKE(3,CURSOR_CROSS,RECT,'A',3,COLF,.TRUE.,ST)
      ENDIF
C
C     New part added label control
C
C     First label is certain font next four are normal DAX fonts
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                4,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                1,ST)

      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                2,ST)

      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                3,ST)

      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                4,ST)
C      
      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                5,ST)
C
C    This one is also big font for BROWSER title
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                4,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                6,ST)
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                7,ST)
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                8,ST)
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                9,ST)
C
      CALL LABEL_MAKE(0,0,
     +                0,
     +                1,
     +                DIALOGF,DIALOGB,
     +                ' ',1,
     +                .FALSE.,.FALSE.,
     +                10,ST)
C
      CALL FRMDRW(1,.TRUE.,ST)
      CALL FRMDRW(2,.TRUE.,ST)

      ST = 0
999   CONTINUE

      END

      SUBROUTINE FILESELECTOR(DEFAULT,FILE,SUPPLIED,LENGTH,STATUS)
C     ============================================================
C1    VARYPE                    I4    C*(*)  I4       I4   I4
C1    IOSTAT                    O       O    I         O   O
C
C2    main file selection dialog. Allows users to select 
C2    a file for usage. If default type is set then only
C2    that type can be returned. Suffiexs are normal
C2    DAXCAD standard.
C2  
C2  
C2    Arguments:-
C2  
C2    DEFAULT		->	efault types see DAXCAD types
C2    FILE		->      File that was select ( Full path )
C2    SUPPLIED          ->      The supplied length of the buffer
C2    LENGTH		->      THE outgoing length
C2  
C2  
C2    Error Returns:
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/macro.inc'
      include    'include/vntable.inc'
      include    'include/buttons.inc'
      include    'include/input.inc'
      include    'include/library.inc'
      include    'include/ftypes.inc'
      include    'include/dialog.inc'
C
      CHARACTER*256 FILE
      CHARACTER*256 STRING
      CHARACTER*256 PATH
      CHARACTER*256 WILD
      CHARACTER*256 DIR
      CHARACTER*256 BRFILE
      CHARACTER*1 TOKEN
C
      INTEGER*4 STATUS
      INTEGER*4 ST
      INTEGER*4 DEFAULT
      INTEGER*4 SUPPLIED
      INTEGER*4 PLEN
      INTEGER*4 FLEN
      INTEGER*4 DLEN
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
      INTEGER*4 POPNUM
      INTEGER*4 CELL
      INTEGER*4 COL
      INTEGER*4 NOUN
      INTEGER*4 DEFTYPE
C
      INTEGER*4 XT,YT
      INTEGER*4 I4LEN
C
      LOGICAL PREF
      LOGICAL YES
      LOGICAL AUTO
      LOGICAL OK
      LOGICAL EX
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
      EXTERNAL POPUP
C
C     Initialisation code
C
      STATUS = -1
      DEFTYPE = DEFAULT
      FILE = ' '
      CALL INPUT_MODIFY(4,FILE,0,ST)
      CALL INPUT_MODIFY(3,FILE,0,ST)
C
      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(4,.TRUE.,ST)
      CALL BUTDRW(8,.TRUE.,ST)
      CALL BUTDRW(9,.TRUE.,ST)
      CALL BUTDRW(10,.TRUE.,ST)
      CALL BUTDRW(11,.TRUE.,ST)
      CALL BUTDRW(12,.TRUE.,ST)
      CALL BUTDRW(13,.TRUE.,ST)
      CALL BUTDRW(14,.TRUE.,ST)
      CALL INPDRW(2,.TRUE.,ST)
      CALL INPDRW(3,.TRUE.,ST)
      CALL INPDRW(4,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)
      STRING = 'SYSTEM FILE SELECTOR DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG4(1) + DIALOG4(3)/2 - TSIZE(1)/2
      YT = DIALOG4(2) + 20

      I4LEN = NLEN(STRING)
      CALL LABMOD(1,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)

      CALL SET_FONT(1,ST)

      STRING = 'Current Pathname:'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG4(1) + 20
      YT = DIALOG4(2) + 70

      I4LEN = NLEN(STRING)
      CALL LABMOD(2,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(2,.TRUE.,ST)

      STRING = 'File Types:'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG4(1) + 20
      YT = DIALOG4(2) + 210

      I4LEN = NLEN(STRING)
      CALL LABMOD(3,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(3,.TRUE.,ST)

      STRING = 'Search Criteria:'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG4(1) + 300
      YT = DIALOG4(2) + 210

      I4LEN = NLEN(STRING)
      CALL LABMOD(4,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(4,.TRUE.,ST)

      STRING = 'Current File selection:'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG4(1) + 20
      YT = DIALOG4(2) + 310

      I4LEN = NLEN(STRING)
      CALL LABMOD(5,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(5,.TRUE.,ST)

C
C     Automatic control for DAXCAD files
C
      FLEN = 0
      AUTO = .TRUE.
      CALL CURSOR_TO_BUTTON(11,ST)
      FILE = ' '
C
C     Load up default file types and current pathanmes
C
C       DAXDRG  Daxcad drawing file
C       DAXPRT    "    part file
C       DAXCMP    "    component file
C       DAXSYM    "    symbol file
C       DAXSEC    "    property
C       DAXSEC    "    sectiom
C       DAXASC    "    binary file
C       DAXBOM    "    bill of materials 
C       DAXMAC    "    macro
C       DAXMIF    "    mif file
C       DAXGEN    "    genio
C       DAXDMF    "    digitiser menu file
C       DAXDXF    "    dxf AUTOCRUD
C       DAXIGS    "    IGES file
C
2000  CONTINUE
      IF ( DEFTYPE.EQ.-1 ) THEN
C          Defaults to any file type
           WILD = '.*'
           CALL BUTMOD(15,VNOUN(623),NLEN(VNOUN(623)),ST)
           CALL INPUT_MODIFY(2,PATH,PLEN,ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXASC
      ELSEIF ( DEFTYPE.EQ.0 ) THEN
C          Defaults to any file type
           WILD = '.*.drg$'
           CALL BUTMOD(15,VNOUN(617),NLEN(VNOUN(617)),ST)
           PATH = PATHN(1)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXDRG
      ELSEIF ( DEFTYPE.EQ.1 ) THEN
C          Defaults to daxcad parts
           WILD = '.*.prt$'
           CALL BUTMOD(15,VNOUN(619),NLEN(VNOUN(619)),ST)
           PATH = PATHN(4)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXPRT
      ELSEIF ( DEFTYPE.EQ.2 ) THEN
C          Defaults to daxcad components
           WILD = '.*.cmp$'
           CALL BUTMOD(15,VNOUN(618),NLEN(VNOUN(618)),ST)
           PATH = PATHN(3)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXCMP
      ELSEIF ( DEFTYPE.EQ.3 ) THEN
C          Defaults to daxcad symbols
           WILD = '.*.sym$'
           CALL BUTMOD(15,VNOUN(620),NLEN(VNOUN(620)),ST)
           PATH = PATHN(5)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXSYM
      ELSEIF ( DEFTYPE.EQ.4 ) THEN
C          Defaults to daxcad macros
           WILD = '.*.mac$'
           CALL BUTMOD(15,VNOUN(621),NLEN(VNOUN(621)),ST)
           PATH = PATHN(2)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXMAC
      ELSEIF ( DEFTYPE.EQ.5 ) THEN
C          Defaults to DXF
           WILD = '.*.dxf$'
           CALL TOOLSGETCWD(PATH,PLEN,ST)
           CALL BUTMOD(15,VNOUN(622),NLEN(VNOUN(622)),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXDXF
      ELSEIF ( DEFTYPE.EQ.6 ) THEN
C          Defauts to Sun Raster
           WILD = '.*.im$'
           CALL BUTMOD(15,VNOUN(629),NLEN(VNOUN(629)),ST)
           PATH = PATHN(6)
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXASC
      ELSEIF ( DEFTYPE.EQ.7 ) THEN
C          Property tables
           WILD = '.*'
           CALL BUTMOD(15,VNOUN(625),NLEN(VNOUN(625)),ST)
           PATH = LIBRARY(1:NLEN(LIBRARY))//'/ptabl/'
           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
           DAXTYP = DAXBOM
C      ELSEIF ( DEFTYPE.EQ.8 ) THEN
C SPB 031194 - Simon's go at trying to add .TIF file support
C           WILD = '.*.tif$'
C           CALL BUTMOD(15,VNOUN(629),NLEN(VNOUN(629)),ST)
C           PATH = PATHN(6)
C           CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
C           CALL INPUT_MODIFY(3,WILD,NLEN(WILD),ST)
C           DAXTYP = DAXASC
      ENDIF
C
      CALL TOOLSGETCWD(PATH,PLEN,ST)
      CALL INPUT_MODIFY(2,PATH,PLEN,ST)
      CALL BUTDRW(15,.TRUE.,ST)
C
1000  CONTINUE
C
      IF ( .NOT.AUTO ) THEN
          CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
      ELSE
          EVTYPE = DAX_EVENT_BUTTON
          EVNUM = 11
      ENDIF
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN
C
           IF ( EVNUM.EQ.10) THEN
C
C              set to users home directory
C
               CALL SETHOME()
               CALL TOOLSGETCWD(PATH,PLEN,ST)
               CALL INPUT_MODIFY(2,PATH,PLEN,ST)
               CALL SETSTANDARDPATH(DEFTYPE)
C
           ELSEIF ( EVNUM.EQ.8) THEN

C              set directory up 1
C
               PATH = '../'
               CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
               CALL TOOLSGETCWD(PATH,PLEN,ST)
               CALL INPUT_MODIFY(2,PATH,PLEN,ST)
               CALL SETSTANDARDPATH(DEFTYPE)
           ELSEIF ( EVNUM.EQ.9) THEN
C
C              allow user selection of directory
C
               CALL TOOLSSETCWD(PATH,PLEN,ST)
               CALL SET_FILE_TARGET(2,PATH,PLEN,ST)
C
               CALL POPUP(2,CELL,ST)
               IF ( CELL.GT.0) THEN
                   CALL POPUP_GET_FILE(2,CELL,DIR,LEN(DIR),PLEN,ST)
                   CALL TOOLSSETCWD(DIR,PLEN,ST)
                   CALL TOOLSGETCWD(PATH,PLEN,ST)
                   CALL INPUT_MODIFY(2,PATH,PLEN,ST)
               ENDIF
               CALL SETSTANDARDPATH(DEFTYPE)
           ELSEIF ( EVNUM.EQ.11) THEN
C
C              Select a file please and place it in the directory
C
               CALL TOOLSGETCWD(PATH,PLEN,ST)
               CALL SET_FILE_TARGET(1,PATH,PLEN,ST)
               CALL INPGTEXT(3,FILE,LEN(FILE),FLEN,ST)
               CALL SET_FILE_WILDC(1,FILE,FLEN,ST)
               CALL POPUP(1,CELL,ST)
               IF ( CELL.GT.0) THEN
                   CALL POPUP_GET_FILE(1,CELL,FILE,LEN(FILE),FLEN,ST)
                   IF ( AUTO ) THEN
                        GOTO 999
                   ENDIF
                   CALL INPUT_MODIFY(4,FILE,FLEN,ST)

               ELSE
C                  automatic is now switched off
                   AUTO = .FALSE.
               ENDIF
C
           ELSEIF ( EVNUM.EQ.12) THEN
C              Select the BROWSER system

               CALL BRMAIN(DEFTYPE,BRFILE,ST)

               IF ( ST.EQ.0) THEN
C                   selected file must save it for loading
                    CALL TOOLSGETCWD(PATH,LENGTH,ST)
                    IF(LENGTH.GT.0) THEN
                        IF(PATH(LENGTH:LENGTH).NE.'/') THEN
                            LENGTH = LENGTH+1
                            PATH(LENGTH:LENGTH) = '/'
                        ENDIF
                    ELSE
C                   no path specified
                       PATH = './'
                    ENDIF
                    FILE = PATH(1:LENGTH)//BRFILE(1:NLEN(BRFILE))
                    FLEN = NLEN(FILE)
                    CALL INPUT_MODIFY(4,FILE,FLEN,ST)
               ENDIF

           ELSEIF ( EVNUM.EQ.13) THEN
C
C              Give him a selection of something
C
               IF ( NLEN(FILE).GT.0) THEN
                   LENGTH = NLEN(FILE)
                   ST = 0
                   GOTO 999
               ELSE
                   ST = -1
                   LENGTH = 0
                   FILE = ' '
                   GOTO 999
               ENDIF
           ELSEIF ( EVNUM.EQ.14) THEN
C
C              Quit the selection altogether
C
               ST = -1
               LENGTH = 0
               FILE = ' '
               GOTO 999
C
           ELSEIF ( EVNUM.EQ.15) THEN
C
C              Selction of file types and wild card
C
               COL = COLFOR
               CALL TOOLPEN(COL)
C
               CALL LOADFILETYPES(ST)
               CALL POPUP(3,CELL,ST)
C
               IF (CELL .GT.0) THEN
C                   Selection complete
C
C                   drawings
                    IF (CELL.EQ.1) DEFTYPE = 0
C                   components
                    IF (CELL.EQ.2 ) DEFTYPE = 2
C                   parts
                    IF (CELL.EQ.3 ) DEFTYPE = 1
C                   symbols
                    IF (CELL.EQ.4 ) DEFTYPE = 3
C                   macros
                    IF (CELL.EQ.5 ) DEFTYPE = 4
C                   DXF
                    IF (CELL.EQ.6 ) DEFTYPE = 5
C                   Anything
                    IF (CELL.EQ.7 ) DEFTYPE = -1
C                   Prop tables
                    IF (CELL.EQ.8 ) DEFTYPE = 7
C                   Sun Raster ja
                    IF (CELL.EQ.9 ) DEFTYPE = 6
C
                    CALL BUTTON_LOW(EVNUM,ST)
                    CALL BUTDRW(15,.FALSE.,ST)
C
                    GOTO 2000
               ENDIF

           ELSE
               GOTO 1000
           ENDIF
           CALL BUTTON_LOW(EVNUM,ST)
      ELSEIF ( EVTYPE .EQ. DAX_EVENT_RETURN )  THEN
C
C          Input from an input device needed to change things
           IF ( EVNUM.EQ.2) THEN
C
C              Save the path
C
               CALL INPGTEXT(2,PATH,LEN(PATH),PLEN,ST)
               CALL TOOLSSETCWD(PATH,PLEN,ST)
               CALL TOOLSGETCWD(PATH,PLEN,ST)
               CALL INPUT_MODIFY(2,PATH,PLEN,ST)
               CALL SETSTANDARDPATH(DEFTYPE)

           ELSEIF ( EVNUM.EQ.4) THEN
C
C              File to enter Try it
C
               CALL INPGTEXT(4,DIR,LEN(DIR),PLEN,ST)
               IF ( PLEN.GT.0) THEN
                   FILE = DIR(1:PLEN)
               ENDIF
           ENDIF
C

      ENDIF
C
      GOTO 1000
C
999   CONTINUE
C
      STATUS = ST
      CALL CURSOR_ON(.FALSE.)
C
      CALL BUTDRW(8,.FALSE.,ST)
      CALL BUTDRW(9,.FALSE.,ST)
      CALL BUTDRW(10,.FALSE.,ST)
      CALL BUTDRW(11,.FALSE.,ST)
      CALL BUTDRW(12,.FALSE.,ST)
      CALL BUTDRW(13,.FALSE.,ST)
      CALL BUTDRW(14,.FALSE.,ST)
      CALL BUTDRW(15,.FALSE.,ST)
      CALL INPDRW(3,.FALSE.,ST)
      CALL INPDRW(2,.FALSE.,ST)
      CALL INPDRW(4,.FALSE.,ST)

      CALL LABDRW(1,.FALSE.,ST)
      CALL LABDRW(2,.FALSE.,ST)
      CALL LABDRW(3,.FALSE.,ST)
      CALL LABDRW(4,.FALSE.,ST)
      CALL LABDRW(5,.FALSE.,ST)

      CALL DIAGDR(4,.FALSE.,ST)
C     make sure we return to the correct directory
      CALL TOOLSSETCWD(STARTUPDIR,NLEN(STARTUPDIR),ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
C
      END

      SUBROUTINE GETINPUT(PROMPT,TEXT)
C     ================================
C1    VARYPE             C*(*)   C*(*)
C1    IOSTAT                I      O
C
C2    Get text from a dialog box
C2  
C2  
C2    Arguments:-
C2  
C2    PROMPT		->		Prompt for the text
C2    TEXT		->		Outgoing text
C2  
C2  
C2    Error Returns:
C2  
C2    An empty text string indicates nothing selected 
C2    as per normal DAXCAD standard
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/server.inc'
      include    'include/dialog.inc'
C
      CHARACTER*(*) TEXT
      CHARACTER*(*) PROMPT
      CHARACTER*200 BUFFER
      CHARACTER*200 STRING
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
C
      INTEGER*4 XT,YT
      INTEGER*4 I4LEN
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
C

      IF ( SERVER ) THEN
          IF ( NLEN(PROMPT).GT.0 ) THEN
              WRITE(*,'(A,$)') PROMPT(1:NLEN(PROMPT))
          ELSE
              WRITE(*,'(A,$)') '> '
          ENDIF
          READ(*,'(A)' ) TEXT
          RETURN
      ENDIF
      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(1,.TRUE.,ST)

C
C     Clear this first ( Itlooks better )
C
      CALL INPUT_MODIFY(D1INP1,' ',0,ST)
      CALL INPDRW(1,.TRUE.,ST)

      CALL BUTDRW(1,.TRUE.,ST)
      CALL BUTDRW(2,.TRUE.,ST)
      CALL BUTDRW(3,.TRUE.,ST)
      CALL BUTDRW(4,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)

      CALL TOOLS_SET_TEXT()
      STRING = 'TEXT INPUT DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)
		

      XT = DIALOG1(1) + DIALOG1(3)/2 - TSIZE(1)/2
      YT = DIALOG1(2) + 20

      I4LEN = NLEN(STRING)
      CALL LABMOD(1,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)

      CALL SET_FONT(1,ST)

      CALL GET_TEXT_INFO(PROMPT,NLEN(PROMPT),TSIZE,TORG,ST)

      XT = DIALOG1(1) + DIALOG1(3)/2 - TSIZE(1)/2
      YT = DIALOG1(2) + 60

      I4LEN = NLEN(PROMPT)
      CALL LABMOD(2,XT,YT,PROMPT,I4LEN,ST)
      CALL LABDRW(2,.TRUE.,ST)
C
C
      TEXT = ' '
      EVTYPE = -1

      CALL CURSOR_TO_BUTTON(D1BUT4,ST)

      CALL INPACC(1,ST)

      CALL CURSOR_ON(.TRUE.)
C
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN

           IF ( EVNUM.EQ.D1BUT3) THEN
C              Clear the buffer 
               CALL INPUT_MODIFY(D1INP1,' ',0,ST)
               CALL INPACC(D1INP1,ST)
               CALL BUTTON_LOW(EVNUM,ST)
               GOTO 1000
           ELSEIF ( EVNUM.EQ.D1BUT4) THEN
C              Again the text
               CALL INPUT_MODIFY(D1INP1,LASTBUF,NLEN(LASTBUF),ST)
               CALL INPACC(D1INP1,ST)
               CALL BUTTON_LOW(EVNUM,ST)
               GOTO 1000
           ELSEIF ( EVNUM.EQ.D1BUT2) THEN
C              activate a done
               EVTYPE = DAX_EVENT_RETURN
           ELSEIF ( EVNUM.EQ.D1BUT1) THEN
C              Quit the input ie teturn nothing
               TEXT = ' '
               GOTO 999
           ENDIF
      ENDIF

      IF ( EVTYPE.EQ.DAX_EVENT_RETURN ) THEN
           CALL INPGTEXT(D1INP1,BUFFER,LEN(BUFFER),LENGTH,ST)
           IF ( LENGTH.GT.0 ) THEN
               IF ( LEN(TEXT) .LT. LENGTH ) THEN
                   LENGTH = LEN(TEXT)
                   TEXT = BUFFER(1:LENGTH)
               ELSE
                   TEXT = BUFFER(1:LENGTH)
               ENDIF
               LASTBUF = BUFFER(1:LENGTH)
           ENDIF
           GOTO 999
C
      ENDIF
C
C
      GOTO 1000
C
999   CONTINUE
C
C	  print *, 'LEN IS:', nlen(text)

      CALL CURSOR_ON(.FALSE.)
C
      CALL SET_FONT(1,ST)
      CALL BUTDRW(1,.FALSE.,ST)
      CALL BUTDRW(2,.FALSE.,ST)
      CALL BUTDRW(3,.FALSE.,ST)
      CALL BUTDRW(4,.FALSE.,ST)
      CALL INPDRW(1,.FALSE.,ST)
      CALL LABDRW(1,.FALSE.,ST)
      CALL LABDRW(2,.FALSE.,ST)
      CALL DIAGDR(1,.FALSE.,ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
C
      END

      SUBROUTINE CONFIRMATION(PRMPT,PREF,YES)
C     ========================================
C1    VARYPE                  C*(*)   L    L
C1    IOSTAT                    I     I    O
C
C2    Get a confirmation from the user. Preference can be
C2    be specified to either the yes or no button
C2  
C2    Arguments:-
C2  
C2    PRMPT		->		Prompt for the the question
C2    PREF		->		TRUE Yes FALSE No jump to button
C2    YES		->              TRUE Yes FALSE No
C2  
C2    Error Returns:
C2  
C2    NONE Yes or no
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/macro.inc'
      include    'include/comstack.inc'
      include    'include/journal.inc'
      include    'include/server.inc'
C
      CHARACTER*(*) PRMPT
      CHARACTER*200 BUFF
      CHARACTER*200 STRING
      CHARACTER*2 INPL
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
      INTEGER*4 I4LEN
C
      INTEGER*4 XT,YT
C
      LOGICAL PREF
      LOGICAL YES
      LOGICAL OK
      LOGICAL YESOK
C
      REAL XP,YP
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
      EXTERNAL YESOK
C

      IF (MACOP.AND.MACTOK.AND..NOT.TCONT) THEN
           CALL MAC101(XP,YP,OK)
           IF (TIR.AND.OK) THEN
                YES = YESOK(TIS)
                RETURN
           ENDIF
      ENDIF
C
      IF ( SERVER ) THEN
          IF ( NLEN(PRMPT) .EQ.0) THEN
              WRITE(*,'(A,$)' ) '? : '
          ELSE
              WRITE(*,'(A,$)' ) PRMPT(1:NLEN(PRMPT))
          ENDIF
          READ(*,'(A)' ) BUFF
          IF ( NLEN(BUFF) .EQ. 0 ) THEN
              YES = .FALSE.
          ELSE
              YES = YESOK(BUFF)
          ENDIF
          RETURN
      ENDIF

      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(2,.TRUE.,ST)

C
C     Clear this first ( Itlooks better )
C

      CALL BUTDRW(D2BUT1,.TRUE.,ST)
      CALL BUTDRW(D2BUT2,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)

      STRING = 'CONFIRMATION DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG2(1) + DIALOG2(3)/2 - TSIZE(1)/2
      YT = DIALOG2(2) + 20

      I4LEN = NLEN(STRING)
      CALL LABMOD(1,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)

      CALL SET_FONT(1,ST)

      I4LEN = NLEN(PRMPT)
C
C     limit his string to 80 ( OLD DAXCAD HANGOVER )
C
      IF( I4LEN.GT.80 ) THEN
          I4LEN = 80
      ENDIF

      CALL GET_TEXT_INFO(PRMPT,I4LEN,TSIZE,TORG,ST)

      XT = DIALOG2(1) + DIALOG2(3)/2 - TSIZE(1)/2
      YT = DIALOG2(2) + 60

      CALL LABMOD(2,XT,YT,PRMPT,I4LEN,ST)
      CALL LABDRW(2,.TRUE.,ST)
C
C
      EVTYPE = -1

C
C     define cursor jump to button
C
      IF ( PREF ) THEN
          CALL CURSOR_TO_BUTTON(D2BUT1,ST)
      ELSE
          CALL CURSOR_TO_BUTTON(D2BUT2,ST)
      ENDIF

      CALL CURSOR_ON(.TRUE.)
C
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN

           IF ( EVNUM.EQ.D2BUT1) THEN
C              Clear the buffer 
               YES = .TRUE.
           ELSEIF ( EVNUM.EQ.D2BUT2) THEN
               YES = .FALSE.
           ELSE
               GOTO 1000
           ENDIF
           CALL BUTTON_LOW(BUTTON,ST)
           GOTO 999
      ENDIF
C
      GOTO 1000
C
999   CONTINUE
C
      CALL CURSOR_ON(.FALSE.)
C
      CALL SET_FONT(1,ST)
      CALL BUTDRW(D2BUT1,.FALSE.,ST)
      CALL BUTDRW(D2BUT2,.FALSE.,ST)
      CALL LABDRW(1,.FALSE.,ST)
      CALL LABDRW(2,.FALSE.,ST)
      CALL DIAGDR(2,.FALSE.,ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
      IF ( YES ) THEN
          INPL = 'Y'
      ELSE
          INPL = 'N'
      ENDIF
      XP = 0
      YP = 0
      IF(JOURON .AND. .NOT. PNTMOD) CALL WRTJRN(XP, YP, 'a', INPL, 0)
C
      END

      SUBROUTINE CONFIRMABORT(PRMPT,PREF,YES,ABORT)
C     ============================================-
C1    VARYPE                  C*(*)   L    L  L
C1    IOSTAT                    I     I    O  O
C
C2    Get a confirmation from the user. Preference can be
C2    be specified to either the yes or no button
C2  
C2    Arguments:-
C2  
C2    PRMPT		->		Prompt for the the question
C2    PREF		->		TRUE Yes FALSE No jump to button
C2    YES		->              TRUE Yes FALSE No
C2  
C2    Error Returns:
C2  
C2    NONE Yes or no
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/macro.inc'
      include    'include/comstack.inc'
      include    'include/journal.inc'
      include    'include/server.inc'
C
      CHARACTER*(*) PRMPT
      CHARACTER*200 BUFF
      CHARACTER*200 STRING
      CHARACTER*2 INPL
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
      INTEGER*4 I4LEN
C
      INTEGER*4 XT,YT
C
      LOGICAL PREF
      LOGICAL YES
      LOGICAL OK
      LOGICAL ABORT
      LOGICAL YESOK
C
      REAL XP,YP
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
      EXTERNAL YESOK
C

      ABORT = .FALSE.
      IF (MACOP.AND.MACTOK.AND..NOT.TCONT) THEN
           CALL MAC101(XP,YP,OK)
           IF (TIR.AND.OK) THEN
                YES = YESOK(TIS)
                RETURN
           ENDIF
      ENDIF
C
      IF ( SERVER ) THEN
          IF ( NLEN(PRMPT) .EQ.0) THEN
              WRITE(*,'(A,$)' ) '? : '
          ELSE
              WRITE(*,'(A,$)' ) PRMPT(1:NLEN(PRMPT))
          ENDIF
          READ(*,'(A)' ) BUFF
          IF ( NLEN(BUFF) .EQ. 0 ) THEN
              YES = .FALSE.
          ELSE
              YES = YESOK(BUFF)
          ENDIF
          RETURN
      ENDIF

      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(2,.TRUE.,ST)

C
C     Clear this first ( Itlooks better )
C

      CALL BUTDRW(D2BUT1,.TRUE.,ST)
      CALL BUTDRW(D2BUT2,.TRUE.,ST)
      CALL BUTDRW(23,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)

      STRING = 'CONFIRMATION DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG2(1) + DIALOG2(3)/2 - TSIZE(1)/2
      YT = DIALOG2(2) + 20

      I4LEN = NLEN(STRING)
      CALL LABMOD(1,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)

      CALL SET_FONT(1,ST)

      I4LEN = NLEN(PRMPT)
C
C     limit his string to 80 ( OLD DAXCAD HANGOVER )
C
      IF( I4LEN.GT.80 ) THEN
          I4LEN = 80
      ENDIF

      CALL GET_TEXT_INFO(PRMPT,I4LEN,TSIZE,TORG,ST)

      XT = DIALOG2(1) + DIALOG2(3)/2 - TSIZE(1)/2
      YT = DIALOG2(2) + 60

      CALL LABMOD(2,XT,YT,PRMPT,I4LEN,ST)
      CALL LABDRW(2,.TRUE.,ST)
C
C
      ABORT = .FALSE.
      EVTYPE = -1

C
C     define cursor jump to button
C
      IF ( PREF ) THEN
          CALL CURSOR_TO_BUTTON(D2BUT1,ST)
      ELSE
          CALL CURSOR_TO_BUTTON(D2BUT2,ST)
      ENDIF

      CALL CURSOR_ON(.TRUE.)
C
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN

           IF ( EVNUM.EQ.D2BUT1) THEN
C              Clear the buffer 
               YES = .TRUE.
           ELSEIF ( EVNUM.EQ.D2BUT2) THEN
               YES = .FALSE.
           ELSEIF ( EVNUM.EQ.23) THEN
               YES = .FALSE.
               ABORT = .TRUE.
           ELSE
               CALL BUTTON_LOW(BUTTON,ST)
               GOTO 1000
           ENDIF
           CALL BUTTON_LOW(BUTTON,ST)
           GOTO 999
      ENDIF
C
      GOTO 1000
C
999   CONTINUE
C
      CALL CURSOR_ON(.FALSE.)
C
      CALL SET_FONT(1,ST)
      CALL BUTDRW(D2BUT1,.FALSE.,ST)
      CALL BUTDRW(D2BUT2,.FALSE.,ST)
      CALL BUTDRW(23,.FALSE.,ST)
      CALL LABDRW(1,.FALSE.,ST)
      CALL LABDRW(2,.FALSE.,ST)
      CALL DIAGDR(2,.FALSE.,ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
      IF ( YES ) THEN
          INPL = 'Y'
      ELSE
          INPL = 'N'
      ENDIF
      XP = 0
      YP = 0
      IF(JOURON .AND. .NOT. PNTMOD) CALL WRTJRN(XP, YP, 'a', INPL, 0)
C
      END

      SUBROUTINE INFODIALOG(TEXT,LINES)
C     =================================
C1    VARYPE                 C*(400) I4 
C1    IOSTAT                  I     I 
C
C2    Prints information about anything with the number of lines
C2    plus the button done
C2  
C2    Arguments:-
C2  
C2    TEXT	->	Text array can vary on LINES
C2    LINES	->	The maximum number of lines to be displayed
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/macro.inc'
      include    'include/buttons.inc'
      include    'include/bitmap.inc'
      include    'include/dialog.inc'
      include    'include/server.inc'
C
      INTEGER*4 LINES
      INTEGER*4 MAXLINES
      INTEGER*4 MAXCHAR
      INTEGER*4 ST
      INTEGER*4 I
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
      INTEGER*4 COL
      INTEGER*4 DIAGX
      INTEGER*4 DIAGY
      INTEGER*4 TTX
      INTEGER*4 TTY
      INTEGER*4 P1
      INTEGER*4 LP
      INTEGER*4 LSIZE
      INTEGER*4 HEIGHT
C
      INTEGER*4 LNSPC
      INTEGER*4 BBORD
      INTEGER*4 MAXL
      INTEGER*4 TEXTH
      INTEGER*4 OFFSET
C
      INTEGER*2 XT,YT
      INTEGER*4 IXP,IYP
      INTEGER*4 I4LEN
      INTEGER*2 I2LEN
      INTEGER*2 RECT(4)
      INTEGER*2 BOX(4)
C
      CHARACTER*(*) TEXT(LINES)
      CHARACTER*200 STRING
C
      LOGICAL PREF
      LOGICAL YES
      LOGICAL OK
      LOGICAL YESOK
C
      REAL XP,YP
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
      EXTERNAL YESOK
C
      PARAMETER(LNSPC=2)
      PARAMETER(BBORD=10)
      PARAMETER(MAXL=20)
C
C     Set maximum sizes first
C
      IF ( SERVER ) THEN
          DO 300 I=1,LINES
              WRITE(*,'(2A)') '[SERVER] ',TEXT(I)
300       CONTINUE
          RETURN
      ENDIF
C
      DIAGX = 0
      DIAGY = 0
      LSIZE = 0
C
      TTX = 300
      TTY = 170

      IF ( LINES.GT.MAXL) THEN
          MAXLINES = MAXL
      ELSE
          MAXLINES = LINES
      ENDIF
C
      CALL SET_FONT(1,ST)
C
C     generate text box size TTX and TTY
C
      CALL FONT_INQ_MAX(1,HEIGHT,ST)
C
      DO 100 I=1,LINES
C
          CALL GET_TEXT_INFO(TEXT(I),NLEN(TEXT(I)),TSIZE,TORG,ST)
C
C         Save actual text for box clear
C
          IF ( TSIZE(1).GT.TTX ) THEN
               TTX = TSIZE(1)
          ENDIF
C
100   CONTINUE
C
      TTY = ( HEIGHT + LNSPC ) * MAXLINES
C
C     Overall box dimensions
C
      DIAGX = TTX + 150
      DIAGY = TTY + 150

C     generate new dialog size
      DIAGBX(1,6) = ROOTW(1)/2 - DIAGX/2
      DIAGBX(2,6) = ROOTW(2)/2 - DIAGY/2
      DIAGBX(3,6) = DIAGX
      DIAGBX(4,6) = DIAGY
C
      DIALOG6(1) = ROOTW(1)/2 - DIAGX/2
      DIALOG6(2) = ROOTW(2)/2 - DIAGY/2
      DIALOG6(3) = DIAGX
      DIALOG6(4) = DIAGY
C
      BUTPOS(1,21) = DIALOG6(1) + 20 
      BUTPOS(2,21) = DIALOG6(2) + 20
      BUTPOS(1,22) = DIALOG6(1) + 20
      BUTPOS(2,22) = DIALOG6(2) + DIAGY-40
C
      P1 = BUTPOS(2,7)
      BUTPOS(2,7) = DIAGBX(2,6) + DIAGBX(4,6) - 40
C
      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(6,.TRUE.,ST)
C
C     Clear this first ( Itlooks better )
C
      CALL BUTDRW(7,.TRUE.,ST)
      CALL BUTDRW(21,.TRUE.,ST)
      CALL BUTDRW(22,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)
C
      CALL TOOLS_SET_TEXT()
      STRING = 'INFORMATION DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)
C
      IXP = DIALOG6(1) + DIALOG6(3)/2 - TSIZE(1)/2
      IYP = DIALOG6(2) + 20
C
      I4LEN = NLEN(STRING)
      CALL LABMOD(1,IXP,IYP,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)
C
C
      CALL SET_FONT(1,ST)
C
      RECT(1) = DIALOG6(1) + 110
      RECT(2) = DIALOG6(2) + 50
      RECT(3) = TTX + BBORD
      RECT(4) = TTY + BBORD
C
      BOX(1) = RECT(1) - 4
      BOX(2) = RECT(2) - 4
      BOX(3) = BOX(1) + RECT(3) + 8
      BOX(4) = BOX(2) + RECT(4) + 6
C
      CALL TOOLPEN(DIALOGB)
      CALL TOOLS_RECTANGLE(RECT,ST)
      CALL TOOLPEN(DIALOGF)
      CALL TOOLS_BOX(BOX(1),BOX(2),BOX(3),BOX(4),ST)
C
      CALL FONT_INQ_MAX(1,HEIGHT,ST)
      CALL GET_TEXT_INFO(TEXT(1),NLEN(TEXT(1)),TSIZE,TORG,ST)
C
      OFFSET = TORG(2)
      XT = DIALOG6(1) + 120
      YT = DIALOG6(2) + 50 + TORG(2)
C
      CALL TOOLS_SET_TEXT()
      DO 200 I=1,MAXLINES
C
          I2LEN = NLEN(TEXT(I))
          CALL TOOLS_TEXT(XT,YT,TEXT(I),I2LEN,ST)
C
          YT = YT + HEIGHT + LNSPC
C
200   CONTINUE
C
C
      EVTYPE = -1
C
C     define cursor jump to button
C
      CALL CURSOR_ON(.TRUE.)
      CALL CURSOR_TO_BUTTON(7,ST)
C
      LP = 1
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN

           IF ( EVNUM.EQ.7) THEN
C               Ok got button exit whole thing
                CALL BUTTON_LOW(EVNUM,ST)
                GOTO 999
           ELSEIF ( EVNUM.EQ.21) THEN

              XT = DIALOG6(1) + 120
              YT = DIALOG6(2) + 50 + OFFSET
              LP = LP + 1
              IF ( LP.GT.LINES-MAXLINES+1) THEN
                   LP = LINES-MAXLINES+1
              ENDIF
              CALL TOOLPEN(DIALOGB)
              CALL TOOLS_RECTANGLE(RECT,ST)
              CALL TOOLPEN(DIALOGF)
              CALL TOOLS_SET_TEXT()
              DO 210 I=LP,MAXLINES+LP-1
C
                 I2LEN = NLEN(TEXT(I))
                 CALL TOOLS_TEXT(XT,YT,TEXT(I),I2LEN,ST)
                 YT = YT + HEIGHT + LNSPC
C
210           CONTINUE
              CALL TOOLS_BOX(BOX(1),BOX(2),BOX(3),BOX(4),ST)
              CALL BUTTON_LOW(EVNUM,ST)

           ELSEIF ( EVNUM.EQ.22) THEN

              XT = DIALOG6(1) + 120
              YT = DIALOG6(2) + 50 + OFFSET
              LP = LP - 1
              IF ( LP.EQ.0) THEN
                   LP = 1
              ENDIF
              CALL TOOLPEN(DIALOGB)
              CALL TOOLS_RECTANGLE(RECT,ST)
              CALL TOOLPEN(DIALOGF)
              CALL TOOLS_SET_TEXT()
              DO 220 I=LP,MAXLINES+LP-1
C
                 I2LEN = NLEN(TEXT(I))
                 CALL TOOLS_TEXT(XT,YT,TEXT(I),I2LEN,ST)
                 YT = YT + HEIGHT + LNSPC
C
220           CONTINUE
              CALL TOOLS_BOX(BOX(1),BOX(2),BOX(3),BOX(4),ST)
              CALL BUTTON_LOW(EVNUM,ST)
           ELSE
               GOTO 1000
           ENDIF
      ENDIF
C
      GOTO 1000
C
999   CONTINUE
C
      CALL CURSOR_ON(.FALSE.)
C
      BUTPOS(2,7) = P1
      CALL SET_FONT(1,ST)
      CALL BUTDRW(7,.FALSE.,ST)
      CALL BUTDRW(21,.FALSE.,ST)
      CALL BUTDRW(22,.FALSE.,ST)
      CALL DIAGDR(6,.FALSE.,ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
C
      END


      SUBROUTINE OVERWRITE(FILE,OK)
C     =============================
C1    VARYPE               C*(*) L 
C1    IOSTAT                I    O
C
C2    Tests for the existance and if exists gets permission to 
C2    overwrite If we dont get it then ask for a new file
C2    If no file the permission is effectively denied
C2  
C2    Arguments:-
C2  
C2    FILE	->	File name
C2    OK        ->      TRUE permission OK FALSE not
C2  
C2  
C2    Error Returns:
C2  
C2    OK
C2  
C2  
      include 'include/vntable.inc'

      CHARACTER*(*) FILE
      CHARACTER*80 TEMP
      INTEGER*4 NLEN
      LOGICAL OK
      LOGICAL YES
      LOGICAL EX
C
      EXTERNAL NLEN
C
      TEMP = FILE
1000  CONTINUE
C
      INQUIRE(FILE=TEMP,EXIST=EX)
      
      IF ( .NOT.EX ) THEN
C          Nothing doing file does not exist
           OK = .TRUE.
           GOTO 999
      ENDIF
C
      CALL CONFIRMATION(DICT01(282),.FALSE.,YES)
C
      IF ( YES ) THEN
C
C         Permission gained try to delete
C
          CALL DELETE(TEMP ,OK)
          IF ( .NOT.OK) THEN
              CALL ERRORDIALOG('File cannot be deleted')
          ENDIF
          GOTO 999
      ELSE
          CALL GETINPUT(DICT01(58),TEMP)
          IF ( NLEN(TEMP).EQ.0) THEN
              OK = .FALSE.
              GOTO 999
          ELSE
              GOTO 1000
          ENDIF
      ENDIF
C
999   CONTINUE
      FILE = TEMP
      END



      SUBROUTINE ERRORDIALOG(MESSAGE)
C     ===============================
C1    VARYPE                  C*(*)
C1    IOSTAT                     I   
C
C2    Tests for the existance and if exists gets permission to 
C2    overwrite If we dont get it then ask for a new file
C2    If no file the permission is effectively denied
C2  
C2    Arguments:-
C2  
C2    MESSAGE	->	Textual message
C2  
C2    Error Returns:
C2  
C2    OK
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
      include    'include/daxcolor.inc'
      include    'include/server.inc'
C
      CHARACTER*(*) MESSAGE
      CHARACTER*200 STRING
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 NLEN
      INTEGER*4 NLEN2
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 COLF
C
      INTEGER*4 XT,YT
      INTEGER*4 I4LEN
C
      EXTERNAL NLEN
C
      IF ( SERVER ) THEN
          WRITE(*,'(2A)') '[SERVER] ',MESSAGE
          RETURN
      ENDIF
      CALL CURSOR_ON(.FALSE.)
      CALL DIAGDR(3,.TRUE.,ST)

C
C     Clear this first ( Itlooks better )
C
      CALL BUTDRW(D3BUT1,.TRUE.,ST)
C
      CALL SET_FONT(4,ST)
      CALL TOOLS_SET_TEXT()

      STRING = 'DAXCAD MESSAGE DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XT = DIALOG3(1) + DIALOG3(3)/2 - TSIZE(1)/2
      YT = DIALOG3(2) + 20
      I4LEN = NLEN(STRING)

      CALL LABMOD(1,XT,YT,STRING,I4LEN,ST)
      CALL LABDRW(1,.TRUE.,ST)

      CALL SET_FONT(1,ST)


      I4LEN = NLEN(MESSAGE)
      CALL GET_TEXT_INFO(MESSAGE,I4LEN,TSIZE,TORG,ST)

      XT = DIALOG3(1) + DIALOG3(3)/2 - TSIZE(1)/2
      YT = DIALOG3(2) + 60

      CALL LABMOD(2,XT,YT,MESSAGE,I4LEN,ST)
      CALL LABDRW(2,.TRUE.,ST)
C
C
      EVTYPE = -1

C
C     define cursor jump to button
C
      CALL CURSOR_TO_BUTTON(D3BUT1,ST)

      CALL CURSOR_ON(.TRUE.)
C
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN
           GOTO 999
      ENDIF
C
      GOTO 1000
C
999   CONTINUE
C
      CALL CURSOR_ON(.FALSE.)
C
      CALL SET_FONT(1,ST)
      CALL BUTDRW(D3BUT1,.FALSE.,ST)
      CALL DIAGDR(3,.FALSE.,ST)
C
C     Reset drawing color for all 
C
      COLF = COLFOR
      CALL TOOLPEN(COLF,ST)
C
      END

C
      SUBROUTINE LOADSHADOW(ST)
C     ==========================
C2    This routine allocates a pattern bitmap
C2    mainly for shaow marking
C
      include  'include/gpr.ins.inc'
C
      include  'include/shadow.inc'
      include  'include/apollo.inc'
      include  'include/daxcolor.inc'
      include  'include/interface.inc'
C
      INTEGER*4 ST
      INTEGER*4 COL
      INTEGER*2 WINDOW(4),BMSIZE(2),PLMASK
      INTEGER*2 FILL_VALUE,CENTER(2),RAD
      INTEGER*2 BACK_VALUE,SCALE
      INTEGER*2 I,J
 
      LOGICAL UNOBSCURED,ON
 
 
C     set fill bitmap size
      BMSIZE(1) = 32
      BMSIZE(2) = 32
      SCALE = 1
C     acquire display now

      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C     allocate an attributre block for each bitmap
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(SHADAT,ST)
      IF ( ST .NE. 0 ) GOTO 999
      CALL GPR_$ALLOCATE_BITMAP
     +     (BMSIZE,HIPLAN,SHADAT,SHADBT,ST)
      IF ( ST .NE. 0 ) GOTO 999

      CALL GPR_$SET_BITMAP(SHADBT,ST)
      IF ( ST .NE. 0 ) GOTO 999
 
      COL = COLBAK
      CALL TOOLPEN(COL)
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = 32
      WINDOW(4) = 32
      CALL GPR_$RECTANGLE(WINDOW,ST)
      COL = MENUF
      CALL TOOLPEN(COL)
C     create pattern bitap
      ON = .TRUE.
      DO 100 J=0,31
 
          DO 200 I=0,31

              IF(ON) THEN
                  CALL GPR_$MOVE(I,J,ST)
                  CALL GPR_$LINE(I,J,ST)
                  IF ( ST .NE. 0 ) GOTO 999
              ENDIF
              ON = .NOT.ON
 
 200      CONTINUE
          ON = .NOT.ON
 
100   CONTINUE
C     reset current bitmap
      CALL GPR_$SET_BITMAP(DISPDE,ST)
      IF ( ST .NE. 0 ) GOTO 999
      CALL GPR_$RELEASE_DISPLAY(ST)
      RETURN
999   CONTINUE
 
      END

      SUBROUTINE BATCH_ACQUIRE(MODE,ST)
C     =================================
C1    VARYPE                    L   I4
C1    IOSTAT                    I   O
C
C2    Sets aqcuire mode for apollo system. If called
C2    the system sets acquire display
C2    If you release and sisplay has not been aqured
C2    then error will result. Otherwise batch acquire
C2    can only be done once
C
      include  'include/gpr.ins.inc'
      include  'include/obswin.inc'
C
      LOGICAL MODE
      INTEGER*4 ST
C
      IF(MODE.AND.BATCH_AQ) THEN
C         error here display allready got
          ST = 1
          GOTO 999
      ELSEIF(.NOT.MODE.AND..NOT.BATCH_AQ) THEN
C         error here display not got
          ST = 2
          GOTO 999
      ENDIF
      IF(MODE) THEN
          UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      ELSE
          CALL GPR_$RELEASE_DISPLAY(ST)
      ENDIF
      BATCH_AQ = MODE
      ST = 0
C
999   CONTINUE

C
      END


      SUBROUTINE TEXTINWINDOW(FRMNUM,FONT,COL,TEXT,LENGTH)
C     ====================================================
C1    VARYPE                    I4    I4  I4  C*(*)  I4   
C1    IOSTAT                    I     I   I     I    I
C
C2    Puts text of a specific font into the window
C2    and will try to justify it from a height point of
C2    view
C2  
C2    Arguments:-
C2  
C2    FRMNUM
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      include 'include/interface.inc'
      include 'include/daxcolor.inc'
      include 'include/frames.inc'
      INTEGER*4 FRMNUM
      INTEGER*4 FONT
      INTEGER*4 COL
      INTEGER*4 COLB
      INTEGER*4 LENGTH
      INTEGER*4 ST
      INTEGER*4 CURFONT
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 NLEN

      INTEGER*2 RECT(4)
      INTEGER*2 RECT1(4)
      INTEGER*2 X,Y,TLEN
      INTEGER*2 MASK
      LOGICAL ACTIVE

      CHARACTER*(*) TEXT

      EXTERNAL NLEN

      CALL INQ_FONT(CURFONT,ST)

      CALL FRAME_CLEAR(FRMNUM,ST)

      RECT(1) = FRMBOX(1,FRMNUM)
      RECT(2) = FRMBOX(2,FRMNUM)
      RECT(3) = FRMBOX(3,FRMNUM)
      RECT(4) = FRMBOX(4,FRMNUM)

      CALL GPR_$INQ_CONSTRAINTS(RECT1,ACTIVE,MASK,ST)
      CALL GPR_$SET_CLIP_WINDOW(RECT,ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,ST)

      CALL SET_FONT(FONT,ST)
      CALL GET_TEXT_INFO(TEXT,NLEN(TEXT),TSIZE,TORG,ST)


      X = RECT(1) + 5
      Y = RECT(2) + TORG(2) + (RECT(4)-TSIZE(2))/2

      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)

      TLEN = NLEN(TEXT)
      CALL TOOLS_TEXT(X,Y,TEXT,TLEN,ST)

      CALL GPR_$SET_CLIP_WINDOW(RECT1,ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)

      CALL SET_FONT(CURFONT,ST)
C
      FRMTXT(FRMNUM) = TEXT


      END

      SUBROUTINE LOADINTERFACE(ST)
C     ============================
C1    VARYPE                   I4
C1    IOSTAT                   O
C
C2    loadup parameters for interface defintions colors fonts etc
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
      include 'include/interface.inc'
      include 'include/apollo.inc'
      include 'include/daxcolor.inc'
      include 'include/frames.inc'
      INTEGER*4 ST

C     set defaults
      IF ( HIPLAN.EQ.0) THEN
          DIALOGB = 0
          DIALOGF = 1
          MENUF = 1
          MENUB = 0
          VIEWF = 1
          VIEWB = 0
      ELSE
          DIALOGB = 3
          DIALOGF = 7
          MENUF = 7
          MENUB = 0
          VIEWF = 7
          VIEWB = 0
          POPMRK = 1
      ENDIF


      ST = 0
      END

      SUBROUTINE LOADFILETYPES(ST)
C     ============================
C1    VARYPE                   I4
C1    IOSTAT                   O
C
C2    preload the popup for use by file types
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
C2  
C2  
C2  
      INTEGER*4 ST
C
      include    'include/popup.inc'
      include    'include/vntable.inc'
C
      CHARACTER*60 ARRAY(9)
      INTEGER*4 NELEM

      NELEM = 9
      ARRAY(1) = VNOUN(624)
      ARRAY(2) = VNOUN(617)
      ARRAY(3) = VNOUN(618)
      ARRAY(4) = VNOUN(619)
      ARRAY(5) = VNOUN(620)
      ARRAY(6) = VNOUN(621)
      ARRAY(7) = VNOUN(622)
      ARRAY(8) = VNOUN(623)
      ARRAY(9) = VNOUN(625)
      CALL POPUPPRELOAD(ARRAY,NELEM,ST)

      END


      SUBROUTINE LASTDAXPOPUP(TOKEN,NOUN,CELL)
C     =======================================
C1    VARYPE                   C*1  I4   I4
C1    IOSTAT                    O    O   O
C
C2    The current atributes from ther last DAXCAD popup call
C2  
C2  
C2    Arguments:-
C2  
C2    TOKEN	->	DAXCAD token 
C2    NOUN	->	DAXCAD noun
C2    CELL	->      Cell hit
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include    'include/menpop.inc'
      include    'include/menun.inc'
      INTEGER*4 NOUN
      INTEGER*4 CELL
      CHARACTER*1 TOKEN
C
      TOKEN = CCMD
      NOUN = LSTNOU
      CELL  = LSTCEL
C
      END
      SUBROUTINE SETSTANDARDPATH(DEFAULT)
C     ===================================
C1    VARYPE                       I4
C1    IOSTAT                       I
C
C2    Set DAXCAD apthname types when a change has been made
C2  
C2  
C2    Arguments:-
C2  
C2    DEFAULT		->		default type
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      include    'include/macro.inc'
C
      INTEGER*4 DEFAULT
      INTEGER*4 PLEN
      INTEGER*4 ST
      CHARACTER*256 PATH
C

      CALL TOOLSGETCWD(PATH,PLEN,ST)
C
      IF(ST .NE. 0) THEN
C       jobs in rags chief
        CALL ERRORDIALOG('Cannot set current working directory')
        RETURN
      ENDIF
C
      IF ( PLEN.LT.256) THEN
          PATH(PLEN+1:PLEN+1) = '/'
          PLEN = PLEN +1
      ENDIF
C
      IF ( DEFAULT.EQ.0) THEN
           PATHN(1) = PATH(1:PLEN)
      ELSEIF ( DEFAULT.EQ.4) THEN
           PATHN(2) = PATH(1:PLEN)
      ELSEIF ( DEFAULT.EQ.2) THEN
           PATHN(3) = PATH(1:PLEN)
      ELSEIF ( DEFAULT.EQ.1) THEN
           PATHN(4) = PATH(1:PLEN)
      ELSEIF ( DEFAULT.EQ.3) THEN
           PATHN(5) = PATH(1:PLEN)
C     Sun Raster ja
      ELSEIF ( DEFAULT.EQ.6) THEN
           PATHN(6) = PATH(1:PLEN)
      ENDIF
C
      END
C
      subroutine get_paste_file()
      end



      SUBROUTINE TOOLS_SET_TEXT()
C     ===========================
C1    VARYPE            
C1    IOSTAT            
C
C2    Sets colours for drawing text in dialogs
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
      include 'include/interface.inc'

      INTEGER*4 ST
      CALL TOOLPEN_TEXT(DIALOGF,DIALOGB,.TRUE.,ST)
      END

      SUBROUTINE TOOLS_SET_SHADOW(ON)
C     ===============================
C1    VARYPE                      L
C1    IOSTAT                      I
C
C2    Sets the current fill values as a shadow color
C2  
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    ON	->	.TRUE. shadow  .FALSE. no shadow
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include  'include/gpr.ins.inc'
C
      include  'include/shadow.inc'
      include  'include/apollo.inc'
      include  'include/daxcolor.inc'
      include  'include/interface.inc'
C
      LOGICAL ON
      INTEGER*2 SCALE
      INTEGER*4 ST
C
      SCALE = 1
      IF ( ON ) THEN
          CALL GPR_$SET_FILL_PATTERN (SHADBT,SCALE,ST)
      ELSE
          CALL GPR_$SET_FILL_PATTERN (gpr_$nil_bitmap_desc,SCALE,ST)
      ENDIF
      END

