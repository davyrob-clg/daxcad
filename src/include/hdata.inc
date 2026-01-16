C
C     @(#)  412.1 date 6/11/92 hdata.inc 
C
C
C     Filename    : hdata.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:17
C     Last change : 92/06/11 14:31:34
C
C     Copyright : Practical Technology Limited  
C     File :- hdata.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C      HDATA contains 
C      HUNIT the logical unit number which the scratch file is 
C        attached to for temporary storage of the boundary
C

      INTEGER*2 NO,MIP1,MIP2,ENT1,ENT2,FORM,NFORM,LSFORM
      INTEGER*4 HUNIT,HLINES
      REAL XMIN,YMIN,YMAX,XMAX,XY(5,2),CHANG

      COMMON /HDATA/XMIN,YMIN,XMAX,YMAX,
     +              HUNIT,HLINES,XY,CHANG,NO,NFORM,LSFORM,FORM


