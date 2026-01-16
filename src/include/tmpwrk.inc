C
C     @(#)  412.1 date 6/11/92 tmpwrk.inc 
C
C
C     Filename    : tmpwrk.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:50
C     Last change : 92/06/11 14:42:10
C
C     Copyright : Practical Technology Limited  
C     File :- tmpwrk.inc
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
      INTEGER*2 IWORKT(4,20)
      REAL RWORKT(6,20)
C
C
      COMMON/TMPWRK/RWORKT,IWORKT
