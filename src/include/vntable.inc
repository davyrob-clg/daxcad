C
C     @(#)  412.1 date 6/11/92 vntable.inc 
C
C
C     Filename    : vntable.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:53
C     Last change : 92/06/11 14:43:07
C
C     Copyright : Practical Technology Limited  
C     File :- vntable.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This include file contains the declarations of
C     storage for the system verb-noun tables,which
C     are loaded at run time,allowing language translation
C     and other modifications to be easily carried out.
C
C     LANGID contains the International Dialling code for
C     for the language used.
      INTEGER VNPMAX
      PARAMETER (VNPMAX=1000)
      INTEGER*4 VNPOS(1:VNPMAX),LANGID
      CHARACTER*16 VNOUN(1:VNPMAX)
      CHARACTER*1 VNTOKE(1:VNPMAX),ANSY
C     FONT1 contains the description of the letters used in 
C     daxcad
      INTEGER*4 FONT1(2000)
C
CAPOLLO|SUN
      CHARACTER*80 DICT01(VNPMAX)
CAPOLLO|SUN
CIBM
C      CHARACTER*50 DICT01(420)
CIBM
C
      COMMON/DICT/DICT01
      COMMON /VNTBL1/VNPOS,LANGID
      COMMON /VNTBL2/VNOUN,VNTOKE,ANSY
C
      COMMON /TEXTFONT/FONT1

