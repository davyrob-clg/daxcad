C
C     @(#)  412.1 date 6/11/92 bitmap.inc 
C
C
C     Filename    : bitmap.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:56
C     Last change : 92/06/11 14:24:15
C
C     Copyright : Practical Technology International Limited  
C     File :- bitmap.inc
C
C     DAXCAD FORTRAN 77 include file
C
C     BITMAX ...  Maximum number of bitmaps
C     BITDES ...  structure containing the attributes
C                 1 ... Apollo descripter
C                 2 ... Attribute block
C                 3,4 . size in pixels (X and Y)
C                 5 ..  number of planes
C                 6 ..  unused
C                 7 ..  unused
C
C     note that descripter 1 is the canvas bitmap

      INTEGER*4 BITMAX
      INTEGER*4 ROOTW(2)
      PARAMETER(BITMAX= 30)

      INTEGER*4 BITDES(7,0:BITMAX)
C
      COMMON/BITI4/BITDES,ROOTW
