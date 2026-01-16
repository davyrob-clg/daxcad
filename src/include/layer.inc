C
C     @(#)  412.1 date 6/11/92 layer.inc 
C
C
C     Filename    : layer.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:20
C     Last change : 92/06/11 14:33:24
C
C     Copyright : Practical Technology Limited  
C     File :- layer.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
      LOGICAL SHOW,HIDE,SHOWAL,HIDEAL,LOADED
      INTEGER*4 FINLAY(1:256),FINTOT

      COMMON /LAYLOG/SHOW,HIDE,SHOWAL,HIDEAL,LOADED

      COMMON /LAYINT/FINLAY,FINTOT
