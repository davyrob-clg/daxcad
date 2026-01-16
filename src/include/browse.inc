C
C     @(#)  412.1 date 6/11/92 browse.inc 
C
C
C     Filename    : browse.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:57
C     Last change : 92/06/11 14:24:23
C
C     Copyright : Practical Technology International Limited  
C     File :- browse.inc
C
C     DAXCAD FORTRAN 77 Include ( Apollo dependant ) file
C
C
C  
C     header variables please set locally. For a more detailed explanation
C     see GPR reference manual
      INTEGER*2 VERSION(2)
      INTEGER*2 SIZE(2)
      INTEGER*2 GROUPS
      INTEGER*2 ARRAY(8)
      INTEGER*2 NSECTS,PIXEL
      INTEGER*2 ALLOCATED,BYTEPL
      INTEGER*2 BYTEPS,POINTER
      INTEGER*4 GENBIT
      INTEGER*4 BMFBIT
      INTEGER*4 BMFATT
      INTEGER*4 GENATT
      EQUIVALENCE (ARRAY(1),NSECTS)
      EQUIVALENCE (ARRAY(2),PIXEL)
      EQUIVALENCE (ARRAY(3),ALLOCATED)
      EQUIVALENCE (ARRAY(4),BYTEPL)
      EQUIVALENCE (ARRAY(5),BYTEPS)
      EQUIVALENCE (ARRAY(7),POINTER)
C

      COMMON/BRAPOLLO/GENBIT,GENATT,BMFBIT,BMFATT
