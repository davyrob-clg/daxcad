C     
C     @(#)  412.1 date 6/11/92 offset.inc 
C
C
C     Filename    : offset.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:31
C     Last change : 92/06/11 14:37:00
C
C     Copyright : Practical Technology Limited  
C     File :- offset.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     OFFSET INCLUDE FILE
C
      LOGICAL OFFLAY
      REAL MULTP(2,40),NUMARR(40)
      INTEGER*2 NUMOFF,NUMP
      INTEGER*4 TARGLY
C
      COMMON/OFFSR/MULTP,NUMARR
      COMMON/OFFSI2/NUMOFF,NUMP
      COMMON/OFFSI4/OFFLAY
      COMMON/OFFSL/TARGLY
