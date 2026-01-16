C
C     @(#)  412.1 date 6/11/92 gerber.inc 
C
C
C     Filename    : gerber.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:14
C     Last change : 92/06/11 14:30:30
C
C     Copyright : Practical Technology Limited  
C     File :- gerber.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This contains the gerber apeture data
C     for each of the tools which the plotter has.
      CHARACTER    TOLTYP(100)*1
      INTEGER*2 TOOLNO,TOLSET(100,2),CURTL,OPT

      COMMON /GERB1/TOLTYP
      COMMON /GERB2/TOOLNO,TOLSET,CURTL,OPT
