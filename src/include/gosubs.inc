C
C     @(#)  412.1 date 6/11/92 gosubs.inc 
C
C
C     Filename    : gosubs.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:15
C     Last change : 92/06/11 14:30:36
C
C     Copyright : Practical Technology Limited  
C     File :- gosubs.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     This include file is for the gosub lable routine
C     

      INTEGER*4 GSLIM
      PARAMETER ( GSLIM = 500 )
      INTEGER*4  GSTACK( GSLIM )
      INTEGER*4 GSPNT

      COMMON /GOSUBS/ GSPNT,GSTACK
