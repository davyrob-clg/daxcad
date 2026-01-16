C
C     @(#)  412.1 date 6/11/92 redboard.inc 
C
C
C     Filename    : redboard.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:41
C     Last change : 92/06/11 14:39:56
C
C     Copyright : Practical Technology Limited  
C     File :- redboard.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      CHARACTER*100 RDLINE,LIBNAM*80,AUXNAM*80
      INTEGER EOFL,REDUNT,CURP,ERRCOD,LNUM,
     +        CCODE,CURRUN,CLAY,NANS(10),IDXUNT
      INTEGER*2 NEWCMP
      REAL DSU,SX,RNANS(10)
      EQUIVALENCE (RNANS,NANS)
      COMMON /REDBC/RDLINE,LIBNAM,AUXNAM
      COMMON /REDBN/CURP,EOFL,REDUNT,NANS,IDXUNT,SX,
     +              ERRCOD,LNUM,CCODE,CURRUN,CLAY,DSU,NEWCMP
