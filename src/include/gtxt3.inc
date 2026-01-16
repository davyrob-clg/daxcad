C
C     @(#)  412.1 date 6/11/92 gtxt3.inc 
C
C
C     Filename    : gtxt3.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:16
C     Last change : 92/06/11 14:31:25
C
C     Copyright : Practical Technology Limited  
C     File :- gtxt3.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     integer storage area for current
C     line number
      INTEGER*2 IGWBSF(4)
C
C
C     character data storage area
      CHARACTER*80 GWBSAF(1:4,0:31),GWBISF(1:4)
C
C
      COMMON /GTXT3/GWBSAF,GWBISF
C
      COMMON /GTXT4/IGWBSF
C
C
