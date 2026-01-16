C
C     @(#)  412.1 date 6/11/92 wrkdat.inc 
C
C
C     Filename    : wrkdat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:56
C     Last change : 92/06/11 14:44:14
C
C     Copyright : Practical Technology Limited  
C     File :- wrkdat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This include file contains the declaration of
C     a common block for use as general workspace
C     in the construction of entity types.
C     Should reduce total runtime storage space
C     if used generally.
C
      INTEGER*2 IWORK(4,20)
      REAL RWORK1(120),RWORK(6,20)
      DOUBLE PRECISION DWORK(3,20)
C
      EQUIVALENCE (RWORK,RWORK1,DWORK)
C
      COMMON/WRKDAT/RWORK,IWORK
C
