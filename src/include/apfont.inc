C     
C     @(#)  412.1 date 6/11/92 apfont.inc 
C
C
C     Filename    : apfont.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:55
C     Last change : 92/06/11 14:23:11
C
C     Copyright : Practical Technology Limited  
C     File :- apfont.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C     MAXFNT ... Maxiumum number of fonts ( Apollo limit )
C     APFNTS ... Array containing handles
C     TXTBAL ... Magic balance factor
C     CURRENT_FONT ... The tools font number loaded
C

      INTEGER*2 MAXFNT
      PARAMETER(MAXFNT=100)
      INTEGER*2 APFNTS(MAXFNT)
      INTEGER*2 TXTBAL(MAXFNT)
      INTEGER*2 CURRENT_FONT

      LOGICAL APFACT(MAXFNT)
C
      COMMON/APFONT/APFNTS,TXTBAL,APFACT,CURRENT_FONT
