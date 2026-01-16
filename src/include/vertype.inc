C
C     @(#)  412.1 date 6/11/92 vertype.inc 
C
C
C     Filename    : vertype.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:51
C     Last change : 92/06/11 14:42:47
C
C     Copyright : Practical Technology Limited  
C     File :- vertype.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
CC
C     this common block is used to determin the
C     version being used.  Variables RFIDF and RFIDF2
c     contain +ve numbers if full version -ve if demo
C     Variables RFIDFC and RFIDF1 are read back from the
C     drawing and should agree with the sign of RFIDF and RFIDF2
      REAL RFIDF,RFIDF2,RFIDFC,RFIDF1
C
      COMMON/VERTYP/RFIDF,RFIDF2,RFIDFC,RFIDF1
C
C
