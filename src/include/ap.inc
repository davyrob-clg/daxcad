C
C
C     @(#)  412.1 date 6/11/92 ap.inc 
C
C
C     Filename    : ap.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:54
C     Last change : 92/06/11 14:23:08
C
C     Copyright : Practical Technology Limited  
C     File :- ap.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
       INTEGER*4 TTYWIN,GRFWIN,EVMASK,WINDOP
       INTEGER*2 WRECT(4)
       INTEGER*2 TTYREC(154),GRFREC(154)
C
       COMMON /AP1/TTYWIN,GRFWIN,EVMASK,WINDOP,WRECT,TTYREC,GRFREC
C
       SAVE /AP1/
C
