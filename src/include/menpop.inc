C
C     @(#)  412.1 date 6/11/92 menpop.inc 
C
C
C     Filename    : menpop.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:24
C     Last change : 92/06/11 14:35:00
C
C     Copyright : Practical Technology Limited  
C     File :- menpop.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
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
C
C     VNMAX       is the number of menu cell text entries as in VNPMAX
C
C     POPLIM      is the maximum number of popup menus available
C
C     POPHDR      is the list of vnoun numbers for the HEADER type
C                 menu cells in the popup system note that this indexes
C                 from -ve poplim to handle the special popups
C                 
C     POPUP       logical to define whether source came from a popup
C
C
C     VPSMNU      logical to show whether 3rd button is valid
C     LSTCEL      Last cell that was hit adjusted to exclude header
C     LSTNOU      Last noun to be hit
C     LSTMNC      Current/Last popup code used
C
      INTEGER*2 LAST_EVENT
      INTEGER*4 MAXCEL,MAXCHR, VNMAX, POPLIM
      PARAMETER(MAXCEL=25, POPLIM=25)
C     VNMAX should be set the same as VNPMAX in vntable.inc     
      PARAMETER(VNMAX=1000)
      INTEGER*4 MNPNTR(3,VNMAX)
      INTEGER*4 MNPCEL(VNMAX)
      INTEGER*4 VNMULT(VNMAX)
      INTEGER*4 MENUID(1:4,0:31), NUMCOL, NUMTHK, POPHDR(-POPLIM:POPLIM)
      CHARACTER DAXBNR(POPLIM)*20
      CHARACTER DAXMNU(POPLIM)*20
      CHARACTER DAXTOK(POPLIM)*2
      INTEGER*4 MNTOT,BITMW,BITMH
      INTEGER*4 MNBITM,SWBITM,TEXTW,TEXTH,MAXLN,MNSTK
      INTEGER*2 STARTX,STARTY
      INTEGER*4 MUBITM
      INTEGER*4 MAXCOL
      INTEGER*4 LSTCEL
      INTEGER*4 LSTNOU
      INTEGER*4 LSTMNC
      LOGICAL PTMODE,PTSMNU,DSPMNU,VPSMNU
      LOGICAL POPUP
      COMMON/MENPCH/ DAXMNU,DAXTOK,DAXBNR
      COMMON/MENPLO/ PTMODE,PTSMNU,DSPMNU,POPUP,VPSMNU
      COMMON/MENPI4/ MNTOT,MNBITM,SWBITM,TEXTW,TEXTH,
     +               BITMW,BITMH,MAXLN,MNPNTR,MNPCEL,MNSTK,
     1               VNMULT,MENUID,NUMCOL,MUBITM,NUMTHK,POPHDR,
     2               MAXCOL,LSTCEL,LSTNOU,LSTMNC
      COMMON/MENPI2/ STARTX,STARTY,LAST_EVENT




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
C    MAXCHR    The number of characters per cell
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
      PARAMETER( LNSPC  =  8 )
      PARAMETER( SHADOW = 10 )
      PARAMETER( HBORDR = 30 )
      PARAMETER( VBORDR = 22 )
      PARAMETER( VERGE  = 40 )                
      PARAMETER( MAXCHR  =20 )                


