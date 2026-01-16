C
C     @(#)  412.1 date 6/11/92 icon.inc 
C
C
C     Filename    : icon.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:17
C     Last change : 92/06/11 14:31:44
C
C     Copyright : Practical Technology Limited  
C     File :- icon.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     This is include file for the viewport icon system
C
C     ICPOS Icon position on the screen is used currently
C     ICNUM Window is currently iconed
C
C
C
      INTEGER*2 ICONEX(4,MAXVP)
      INTEGER*2 ICONBF(4,MAXVP)
      INTEGER*2 ICONPO(2,MAXVP)
      LOGICAL ICPOS(MAXVP)
      LOGICAL ICNUM(MAXVP)

      COMMON/ICONI2/ICONEX,ICONBF,ICONPO
      COMMON/ICONL/ICPOS,ICNUM
