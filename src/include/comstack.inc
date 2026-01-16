C
C     @(#)  412.1 date 6/11/92 comstack.inc 
C
C
C     Filename    : comstack.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:00
C     Last change : 92/06/11 14:25:39
C
C     Copyright : Practical Technology Limited  
C     File :- comstack.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Include file for command stack
C
      CHARACTER*20 CMDS(20)
      REAL HITPTS(2,20)
      INTEGER CSTKP
      COMMON /COMSTC/CMDS
      COMMON /COMSTR/HITPTS
      COMMON /COMSTI/CSTKP
