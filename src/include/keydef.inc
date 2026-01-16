C
C     @(#)  412.1 date 6/11/92 keydef.inc 
C
C
C     Filename    : keydef.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:19
C     Last change : 92/06/11 14:32:59
C
C     Copyright : Practical Technology Limited  
C     File :- keydef.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
      REAL KEYX,KEYY
      INTEGER*4 KEYNUM,KLEN,LASTP,KNUM,KTOTAL
      LOGICAL KEYOK,KDEFOK,KSTART,KEYCHG,SAVCRD
      CHARACTER*80 KEYDEF(20),KCOM,KCMD*10,KEY*10,KCODE(20)*10
C                               
C     these are the variables use during KEY definition
      COMMON /KEYDF1/KEYNUM,KLEN,LASTP,KNUM,KTOTAL,KEYX,KEYY,
     +               KEYOK,KDEFOK,KSTART,KEYCHG,SAVCRD                       
      COMMON /KEYDF2/KEYDEF,KCOM,KCMD,KEY,KCODE
C
C


