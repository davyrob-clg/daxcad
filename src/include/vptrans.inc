C
C     @(#)  412.1 date 6/11/92 vptrans.inc 
C
C
C     Filename    : vptrans.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:54
C     Last change : 92/06/11 14:43:24
C
C     Copyright : Practical Technology Limited  
C     File :- vptrans.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C        This common block has been created to store trnasorm information
C        for viewports
c
c         VPWVXY      } These arrays hold the transforms for view to world
C         VPVWXY      } and visa versa which are stored for reasons of speed
C                     } to avoid unneccessary calculations.
c
      REAL VPWVXY(6,3,3),VPVWXY(6,3,3)
C  
      COMMON /VPTR/ VPWVXY,VPVWXY
