C
C     @(#)  412.1 date 6/11/92 dtext.inc 
C
C
C     Filename    : dtext.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:09
C     Last change : 92/06/11 14:29:04
C
C     Copyright : Practical Technology Limited  
C     File :- dtext.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This file contains the height of text used on the display
C     terminal for the menus

      INTEGER*2 TEXH,TEXW

C     TEXH height in pixels on the screen of 
C     the terminals being used
C     TEXW width in pixels of the text 
C     being used on the terminal

      COMMON /DTXT/TEXW,TEXH
