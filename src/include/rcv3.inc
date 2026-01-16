C
C     @(#)  412.1 date 6/11/92 rcv3.inc 
C
C
C     Filename    : rcv3.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:40
C     Last change : 92/06/11 14:39:29
C
C     Copyright : Practical Technology Limited  
C     File :- rcv3.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This is a temp common block used
C     to store nodal subfigure names in an array
      INTEGER*2   NSFCNT,NSFPNT
      CHARACTER*512 NSFNAM(10)
C
      COMMON/NPNT/NSFCNT,NSFPNT
      COMMON/NNAM/NSFNAM
C
