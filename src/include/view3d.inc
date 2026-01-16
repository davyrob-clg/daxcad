C
C     @(#)  412.1 date 6/11/92 view3d.inc 
C
C
C     Filename    : view3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:52
C     Last change : 92/06/11 14:42:54
C
C     Copyright : Practical Technology Limited  
C     File :- view3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C1    This file contains all global view
C2    control data.
      REAL VIEWM(4,4),VIEWPT(3),VIEWCN(3)
      COMMON /VIEW3D/ VIEWM,VIEWPT,VIEWCN
C
