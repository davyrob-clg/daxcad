C
C     @(#)  412.1 date 6/11/92 keymap.inc 
C
C
C     Filename    : keymap.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:20
C     Last change : 92/06/11 14:33:07
C
C     Copyright : Practical Technology Limited  
C     File :- keymap.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     used for keeping the key for a line
      integer*4 entkey(0:1,1:32000),kmx1,kmy1,kmx2,kmy2,key(0:1)
      integer*4 kmlen
      common/keymap/entkey,kmx1,kmy1,kmx2,kmy2,key,kmlen
