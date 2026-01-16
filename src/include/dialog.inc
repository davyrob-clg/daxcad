C
C     @(#)  412.1 date 6/11/92 dialog.inc 
C
C
C     Filename    : dialog.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:07
C     Last change : 92/06/11 14:28:22
C
C     Copyright : Practical Technology International Limited  
C     File :- dialog.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C       Include file for dialog boxes
C
C
C     DIAGCR      Current active dialog box
C     DIAGBX      Rectangular size of box
C     DIAGST      Structure of dialog box
C     DIAGCL      Foreground and background color of box
C     DIAGBT      Bitmap descripter number
C     DIAGAC      Dalog box has been defined
C     DIADIS      Dialog box is currently on screen
C     DIAG_STACK  Stack of dialog boxes on the screen
C                 each subscript contains the dialog number
C     DIAG_POINT  Pointer to last dialog box on stack
C
C
      INTEGER*4 MAXDIA,MAXDEL,MAXELM
C
      PARAMETER (MAXDIA = 10)
C
      INTEGER*2 DIAGBX(4,MAXDIA)
      INTEGER*4 DIAGCL(2,MAXDIA)
C
      INTEGER*4 DIAGBT(MAXDIA)
      INTEGER*4 DIAGCR
      INTEGER*4 DIAG_STACK(0:MAXDIA)
      INTEGER*4 DIAG_POINT
C
      LOGICAL DIAGAC(MAXDIA)
      LOGICAL DIADIS(MAXDIA)
C
C     /*  Code for dialog box for standard comms  */
      INTEGER*4 DIALOGIN
C     /*  Code for dialog box in error */
      INTEGER*4 DIALOGOUT
C     /*  Code for dialog box in error */
      INTEGER*4 DIALOG_FLOW

      PARAMETER( DIALOGIN  = 1)
      PARAMETER( DIALOGOUT =  2)
      PARAMETER( DIALOG_FLOW =  3)
C
C     dialog definitions for input windows
      CHARACTER*256   DIALOGINPUT

      COMMON/DIAGI2/DIAGBX
C
C     
      COMMON/DIAGI4/DIAGCL,
     +              DIAGBT,
     +              DIAGCR,
     +              DIAG_STACK,
     +              DIAG_POINT
C
C
      COMMON/DIAGL/DIAGAC,
     +             DIADIS
c
      COMMON/DIAGC/DIALOGINPUT
C
