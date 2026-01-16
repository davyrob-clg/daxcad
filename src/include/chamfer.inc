C
C     @(#)  412.1 date 6/11/92 chamfer.inc 
C
C
C     Filename    : chamfer.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:58
C     Last change : 92/06/11 14:25:06
C
C     Copyright : Practical Technology Limited  
C     File :- chamfer.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Temperary include file to test the chamfer code
C     it contains the variables for the first line distance , angle (IF appropriate)
C     and second line distance

      DOUBLE PRECISION DFDIST,DANGL,DSDIST
      COMMON /CHAM/ DFDIST,DANGL,DSDIST
