C
C     @(#)  412.1 date 6/11/92 clock.inc 
C
C
C     Filename    : clock.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:58
C     Last change : 92/06/11 14:25:23
C
C     Copyright : Practical Technology Limited  
C     File :- clock.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     Include file for hands of clock
      LOGICAL CFIRST
      REAL XSH,YSH,XLH,YLH,OANG
      COMMON/CLOCK/ XSH,YSH,XLH,YLH,CFIRST,OANG
C
