C
C     @(#)  412.1 date 6/11/92 compd.inc 
C
C
C     Filename    : compd.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:59
C     Last change : 92/06/11 14:25:34
C
C     Copyright : Practical Technology Limited  
C     File :- compd.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      REAL M1(3,3),M2(3,3),MM(3,3),NSTTFM(9,250)
      INTEGER*4 CNEST,NINSTS,NSTLEV(250)
      INTEGER*2 RELP,NRRECS,NXTRLR,NENTS,NSTMIP(250)
      CHARACTER*80 NSTNAM(250)
C
      COMMON /COMP1/M1,M2,MM,NSTTFM,CNEST,NINSTS,NSTLEV,RELP,
     +              NRRECS,NXTRLR,NENTS,NSTMIP
      COMMON /COMP2/NSTNAM
      SAVE   /COMP1/
      SAVE   /COMP2/
C
