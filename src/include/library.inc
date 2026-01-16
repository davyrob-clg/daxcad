C
C     @(#)  412.1 date 6/11/92 library.inc 
C
C
C     Filename    : library.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:22
C     Last change : 92/06/11 14:33:35
C
C     Copyright : Practical Technology Limited  
C     File :- library.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C	library.inc
C
C	LIBRARY : where to look for files.
C	This makes running development versions easier.
C
      CHARACTER*80 LIBRARY,HOME
      COMMON/LIB/LIBRARY,HOME
