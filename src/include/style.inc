C
C     @(#)  412.1 date 6/11/92 style.inc 
C
C
C     Filename    : style.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:46
C     Last change : 92/06/11 14:41:24
C
C     Copyright : Practical Technology Limited  
C     File :- style.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C TXCLR is the text after the colon  in the COLOUR menu cell ie colour:txclr
C TXFONT is the text after the colon  in the FONT menu cell ie font:txclr
C TXTHK is the text after the colon  in the text menu cell ie text:txclr


      INTEGER*2      COLOUR,LAYER,THICK,SPARE,FONT,STATUS
      CHARACTER*20   TXCLR, TXFONT, TXTHK
      COMMON /STYLE/ COLOUR,LAYER,THICK,SPARE,FONT,STATUS
      COMMON /TSTYLE/ TXCLR, TXFONT, TXTHK
