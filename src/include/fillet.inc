C
C     @(#)  412.1 date 6/11/92 fillet.inc 
C
C
C     Filename    : fillet.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:13
C     Last change : 92/06/11 14:29:55
C
C     Copyright : Practical Technology Limited  
C     File :- fillet.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     This routine is used for the last fillet cancel
C
      INTEGER*2 CFMIP(2)
      REAL BUFF(12)
      COMMON/FILLI/ CFMIP
      COMMON/FILLR/ BUFF

