C
C     @(#)  412.1 date 6/11/92 product.inc 
C
C
C     Filename    : product.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:35
C     Last change : 92/06/11 14:38:41
C
C     Copyright : Practical Technology Limited  
C     File :- product.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     PRODUCT is the name under which the softwareis to be released
C     The current product name is patched in as the disks are 
C     produced.
C
      CHARACTER*13 PRODUCT,PROOT*20,PRNAM
C
      COMMON/PDCT/ PRODUCT,PROOT,PRNAM
C
