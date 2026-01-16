C
C     @(#)  412.1 date 6/11/92 buttons.inc 
C
C
C     Filename    : buttons.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:57
C     Last change : 92/06/11 14:24:45
C
C     Copyright : Practical Technology International Limited  
C     File :- buttons.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C
C
C     BUTCNT ..... Current number of buttons defined
C     BUTPOS ..... Coordinate position of each button. Note that this is local to the 
C                  dialogue box being constructed 3 4 contain size of box calculated at
C                  load time
C     BUTLNT ...... Length of ascociated text
C     BUTTXT ..... Text associated with each button
C     BUTACT ..... Logical array of active buttons
C     BUTDIS ..... Logical array of cuurently displayed buttons thus can be accessed
C     BUTFNT ..... APOLLO font number to be used for text in buttons
C                  Balance factor is set at load time and button will
C                  be resized autmatically
C     CLPSIZ ..... The size of the clipped corner off the main outer box
C     BUTOX  ..... The size of the outer box from the inner text box
C     BUTOY  ..... The size of the outer box from the inner text box
C     BUTNXT ..... The corrdinate of the next available button position  
C     BUTFRM ..... The current frame around the window being used
C     PRVBUT ..... Previous button to be made
C     BUTCOL ..... Color of button
C     BUTTYP ..... Type identifier to draw button
C     BUTTHK ..... Thicknes of frame round button
C
C     PARENT_DIALOG  ..... parent dialog of a particular button (For Repainting)
C
      INTEGER*2 BUTCNT
      INTEGER*2 BUTMAX
C     set max
      PARAMETER(BUTMAX= 30)
      INTEGER*2 BUTPOS(4,BUTMAX)
      INTEGER*2 BUTFNT(BUTMAX)
      INTEGER*2 CLPSIZ
      INTEGER*2 BUTNXT(2)
      INTEGER*4 BUTCOL(2,BUTMAX)
      INTEGER*2 BUTOX(BUTMAX)
      INTEGER*2 BUTOY(BUTMAX)
      INTEGER*2 BUTFRM(4)
      INTEGER*2 PRVBUT
      INTEGER*4 BUTTHK(BUTMAX)
      INTEGER*4 BUTTYP(BUTMAX)
      CHARACTER*100 BUTTXT(BUTMAX)
      LOGICAL BUTACT(BUTMAX)
      LOGICAL BUTDIS(BUTMAX)
      LOGICAL BUTHI(BUTMAX)
      LOGICAL BUTTON_STAT
      INTEGER*4 BUTTON_PRESSED                                                  
      INTEGER*4 PARENT_DIALOG(BUTMAX)
C
      COMMON/BUTI2/ BUTCNT,BUTPOS,BUTFNT,BUTNXT,BUTFRM,PRVBUT,
     +              BUTOX,BUTOY
      COMMON/BUTI4/ BUTCOL,BUTTYP,BUTTHK
      COMMON/BUTCH/ BUTTXT
      COMMON/BUTLG/ BUTACT,BUTDIS,BUTHI
      COMMON/BUTLG1/ BUTTON_STAT,BUTTON_PRESSED
      COMMON/PARENT/ PARENT_DIALOG
C
