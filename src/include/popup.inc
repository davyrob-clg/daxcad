C
C     @(#)  412.1 date 6/11/92 popup.inc 
C
C
C     Filename    : popup.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:34
C     Last change : 92/06/11 14:38:18
C
C     Copyright : Practical Technology International Limited  
C     File :- popup.inc
C
C     DAXCAD FORTRAN 77 Include file
C

CSOURCE:APOLLO
C
C                 VARIABLE DESCRIPTION
C                 ====================
C
C     DAXMNU      Character array which holds the current popup menu
C                 cells. This is loaded by MENU_LOAD or by another
C                 routine before popping the menu
C     DAXTOK      Character array holding tokens associted with the
C                 values in DAXMNU
C     MNPNTR      Array holding start and end subscript pointers
C                 in the array MNPCEL. This is created at run time
C                 from the daxcad.menu.config file.
C                 The third field contains the number of columns or
C                 0 for a simple menu.
C     MNPCEL      Contains the menu number codes. These codes are
C                 subscript values in the array VNPOS and VNTOK
C                 which are the system dictionary. This array
C                 is a sequential list of all the POPUPS. It is
C                 delimited by the MNPNTR array
C     VNMULT      Is an array containg the POPUP menu code. It is 
C                 effectivley a back pointer to the original POPUP 
C                 menu number. Each subscript value relates to the
C                 system dictionary.
C     MENUID      Array containg the current state of all loaded menus.
C                 it allows direct access to any loaded cells 
C                 dictionary code. It is updated every time a 
C                 dictionary entry is loaded. Used by the MACRO
C                 and others no doubt.
C     MNTOT       The current number of cells that are loaded into
C                 The array DAXMNU. This is used to calculate the
C                 height of the bitmap needed for popping
C     PTMODE      The points mode is active if true
C
C     MNBITM      The bitmap assinged to create a grey back ground
C                 for cells which were toggle but are now popups.
C
C     SWBITM      The bitmap which is used to save the portion of the
C                 main bitmap under the popup. This is saved and
C                 replaced in routine MENPOP_PAINT.
C     STARTX      The start coordinate of the popup. This will be 
C     STARTY      Used for painting and replacing the popup
C
C     NUMCOL      This value is the number of columns in the current
C                 menu.  NOTE: The number of columns (minus the BANNER
C                 muist be a multiple of numcol or cells will be lost)
C                 For a simple, 1 column menu, NUMCOL is set to 0.
C
C     MAXLN       Used to store the largest popup entry. This will 
C                 make the popup size to this value
C
C     MNSTK       Used to pull values from the array holding the POPUP
C                 cells. 
C     MAXCEL      A parameter variable used to store the maximum number
C                 of cells per popup. 25 should be enough !!!!. Again
C                 the popup will size to this value
C     DAXBNR      Array holding each popup name. Referenced by popup code
C
C     PTSMNU      The points menu (mouse button 1) can only be activated
C                 when this flag is set to true.
C     DSPMNU      The popup display menu (mouse button 2) can only be
C                 activated when this flag is true.
C
C     MUBITM      Bitmap descripter for highlighting the cells
C               
C     NUMTHK      The number of thick line definitions to display in
C                 the thickness menu.
C     POPUP       The command has come from a popup rather than a cell
C
C     POPFNT      Font associated with popup
C     POPCOL      1st element text 2nd element backgroud
C                 color. be careful of conflict 3rd element shadow
C     VNOUNS      Maximum number of elements in cells
C     POPACT      active popups
C     POPHGT      Look up for all text heights
C
C     POPTYP      Popup type
C                 1            standard
C                 2            File selection
C
C     POPDIR      Target for fileselection
C     POPWDC      Wildcard for selection
C     POPFIL      Current number of filenames in 
C                 file selection
C     POPBAR      Width of scroll bar on popup
C
C     PACTUP      popup will activate on the upstroke of a puck button
C     POPUNT      Unit number that file is open on 
C     POPFID      tempory file id code
C     POPCUR      Cursor asociated with the popup
C     POPBUF      Buffer containing text of last 
C                 Get on text menu
C     POPLST      Flag to indicate that the last type
C                 was a file selction. Will be reset by
C                 a call to POPUP_GET_TEXT or POPUP
C     POPUP_CURRENT Current popup number on the screen
C     POPUP_CELL  Currently activated cell number
C     PRELOAD     Popup takes it input from a preloaded list rather
C                 than form ppopup load
C     POPUPLOCKED Lock the popup until null hit is taken.
C
C     FILPOS      Current position for file select popup.
C
      INTEGER*2 POPNXT,POPSTR,POPEND
      CHARACTER*60 VNOUNS(1:600)

      INTEGER*4 MAXPOP
      PARAMETER(MAXPOP=25)

      INTEGER*4 MAXFIL
      PARAMETER(MAXFIL=20)

      INTEGER*4 MAXCEL
      PARAMETER(MAXCEL=60)
      INTEGER*4 MNPNTR(2,MAXPOP)

      CHARACTER DAXBNR(MAXPOP)*60
      CHARACTER DAXMNU(MAXCEL)*60
      CHARACTER DAXTOK(MAXCEL)*2
      CHARACTER*256  POPDIR(MAXPOP)
      CHARACTER*256  POPWDC(MAXPOP)
      CHARACTER*256  POPBUF

      INTEGER*4 MNTOT,BITMW,BITMH
      INTEGER*4 MNBITM,SWBITM,MAXLN,MNSTK,SWATT
      INTEGER*2 STARTX,STARTY
      INTEGER*4 MUBITM
      INTEGER*2 POPFNT(MAXPOP)
      INTEGER*4 POPCOL(2,MAXPOP)
      INTEGER*4 POPTYP(MAXPOP)
      INTEGER*4 POPFIL
      INTEGER*4 POPBAR
      INTEGER*4 POPUNT
      INTEGER*4 POPUP_CURRENT
      INTEGER*4 POPUP_CURRENT_CELL
      INTEGER*4 POPCUR(MAXPOP)
      INTEGER*4 POPSIZ(2)
      INTEGER*4 POPFID
      INTEGER*4 FILPOS
      INTEGER*2 POPHGT(MAXCEL)
      INTEGER*2 NEWRECT(4)
      INTEGER*2 OLDRECT(4)
      LOGICAL PACTUP(MAXPOP)
      LOGICAL POPLST
      LOGICAL PRELOAD
      LOGICAL POPUPLOCKED

      LOGICAL PTMODE,PTSMNU,DSPMNU,POPACT(MAXPOP)
C
      COMMON/TOOLS_POPPCH/ DAXMNU,
     +                     DAXTOK,
     +                     DAXBNR,
     +                     POPDIR,
     +                     POPWDC,
     +                     POPBUF
C
      COMMON/TOOLS_POPPLO/PTMODE,
     +                    PTSMNU,
     +                    DSPMNU,
     +                    POPACT,
     +                    PACTUP,
     +                    POPLST,
     +                    PRELOAD,
     +                    POPUPLOCKED
C
      COMMON/TOOLS_POPPI4/ MNTOT,
     +                     SWBITM,
     +                     BITMW,
     +                     BITMH,
     +                     MAXLN,
     +                     MNPNTR,
     +                     MNSTK,
     +                     POPCOL,
     +                     SWATT,
     +                     POPTYP,
     +                     POPFIL,
     +                     POPUNT,
     +                     POPCUR,
     +                     POPUP_CURRENT,
     +                     POPUP_CURRENT_CELL,
     +                     POPFID,
     +                     FILPOS
C
      COMMON/TOOLS_POPPI2/ STARTX,STARTY,
     +                     POPSIZ,
     +                     POPFNT,
     +                     POPHGT,
     +                     NEWRECT,
     +                     OLDRECT
C
      COMMON/TOOLS_POPUP/POPNXT,
     +                   POPSTR,
     +                   POPEND
C
      COMMON/TOOLS_POPCH/VNOUNS


C
C  NOTE: The following values are all in screen pixels.
C
C    BLKBDR    The thickness of the black line surrounding the menu.
C
C    LGAP      The gap between the left border and the start of the text in each
C              menu cell.
C
C    RGAP      The gap between the last character of the longest cell and the
C              right border.
C
C    TGAP      The gap between the top border and the top of the first menu cell
C
C    BGAP      The gap between the bottom of the last menu cell and the bottom
C              border.
C
C    LNSPC     The spacing between menu cells.
C
C    SHADOW    The width of the shadow behind the menu. (NOT USED YET !!!)
C
C    HBORDR    This is the total of the additional widths.
C              (2*BLKBDR) + LGAP + RGAP + SHADOW
C
C    VBORDR    This is the total of the additional heights.
C              (2*BLKBDR) + TGAP + BGAP + SHADOW
C
C    VERGE     The menu is always drawn in from the edge of the screen. This is
C              that border.
C

      INTEGER*4 BLKBDR, LGAP, RGAP, TGAP, BGAP, LNSPC, SHADOW
      INTEGER*4 HBORDR, VBORDR, VERGE
C
C           DON'T FORGET ... When you change any values, you
C           ------------     must also update the VBORDR and
C                            HBORDR values !!!
C
      PARAMETER( BLKBDR =  4 )
      PARAMETER( LGAP   =  5 )
      PARAMETER( RGAP   =  7 )
      PARAMETER( TGAP   =  2 )
      PARAMETER( BGAP   =  3 )
      PARAMETER( LNSPC  =  2 )
      PARAMETER( SHADOW = 10 )
      PARAMETER( HBORDR = 30 )
      PARAMETER( VBORDR = 22 )
      PARAMETER( VERGE  = 40 )
      PARAMETER( POPBAR  =20 )                


