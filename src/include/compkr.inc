C
C     @(#)  412.1 date 6/11/92 compkr.inc 
C
C
C     Filename    : compkr.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:00
C     Last change : 92/06/11 14:25:36
C
C     Copyright : Practical Technology Limited  
C     File :- compkr.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      INTEGER*2 NSTLIM
      PARAMETER (NSTLIM=50)
C
      REAL ME(3,3),M(3,3),CDT(3,3),IDM(3,3),IMM(3,3)
      REAL STKTFM(3,3,50)
      INTEGER*2 RELMIP(50),RELSUB(50)
C
      COMMON /COMPK1/ME,CDT,IDM,IMM,STKTFM
      COMMON /COMPK2/RELMIP,RELSUB
C





