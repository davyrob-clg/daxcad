C
C     @(#)  412.1 date 6/11/92 suffix.inc 
C
C
C     Filename    : suffix.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:47
C     Last change : 92/06/11 14:41:26
C
C     Copyright : Practical Technology Limited  
C     File :- suffix.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      INTEGER*4 NSUF
      PARAMETER(NSUF=15)
      CHARACTER LSUF(NSUF)*4
C

      COMMON/POSTFX/LSUF
