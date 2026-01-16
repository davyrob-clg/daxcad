C
C     @(#)  412.1 date 6/11/92 drwspl.inc 
C
C
C     Filename    : drwspl.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:09
C     Last change : 92/06/11 14:28:59
C
C     Copyright : Practical Technology Limited  
C     File :- drwspl.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C2    This include file is used by the write option.
C2
C2    NPSPL = Number of points in spline.
C
      INTEGER*4 NPSPL
      REAL  RESLTN
      LOGICAL FRSTPT
C
      COMMON/NPSPL/NPSPL
      COMMON/RESLTN/RESLTN
      COMMON/FRSTPT/FRSTPT

