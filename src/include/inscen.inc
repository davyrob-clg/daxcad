C
C     @(#)  412.1 date 6/11/92 inscen.inc 
C
C
C     Filename    : inscen.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:18
C     Last change : 92/06/11 14:32:03
C
C     Copyright : Practical Technology Limited  
C     File :- inscen.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This file contains the global variables for creating and modifying 
C2    centre lines 
C
C2    Modified for RS 6000 for common block names which conflicted with 
C2    routine names.
C
      INTEGER*2 CLINEF
      CHARACTER*20 CLINET
      REAL ROTANG,BRDRSZ
C
      COMMON/INSCR4/ROTANG,
     +              BRDRSZ

      COMMON/INSCCH/CLINEF
C
      COMMON/INSCI2/CLINET

