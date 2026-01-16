C
C
C     @(#)  412.1 date 6/11/92 arcdat.inc 
C
C
C     Filename    : arcdat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:55
C     Last change : 92/06/11 14:23:43
C
C     Copyright : Practical Technology Limited  
C     File :- arcdat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      INTEGER*4 COUNT
      REAL ARCRAD,ANGLE
      LOGICAL RADSET,ARCSET
      LOGICAL FIRST1,CIRSET
      INTEGER*2 FPOS
      COMMON/ARCDAT/ARCRAD,ANGLE,RADSET,ARCSET,FIRST1,COUNT,CIRSET,
     +              FPOS
