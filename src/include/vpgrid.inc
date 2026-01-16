C
C     @(#)  412.1 date 6/11/92 vpgrid.inc 
C
C
C     Filename    : vpgrid.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:54
C     Last change : 92/06/11 14:43:14
C
C     Copyright : Practical Technology Limited  
C     File :- vpgrid.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     This include file is for the individual viewport
C     grid control
C
C     GRIDVP  contains the following.... 1 2 grid origin
C                                        3 4 grid distance
C                                        5  grid type
C
C     GRIDIS  Contains the following     1 ISOANG
C                                        2 ISOSX  
C                                        3 ISOSY  
C                                        4 HISOSX 
C                                        5 HISOSY  
C
C2    ISOANG   isometric angle in rads
C2    ISOSX    width of the isometric diamond
C2    ISOSY    height of the iso diamond
C2    HISOSX   half the width of the isometric diamond
C2    HISOSY   half the height of the iso diamond
C2    DRAWNG   Indicates drawing in progress grid can be u[pdayted
      LOGICAL DRAWNG
      REAL GRIDVP(5,0:MAXVP)
      REAL GRIDIS(5,0:MAXVP)
C
      COMMON/VPGRD/GRIDVP,DRAWNG,GRIDIS
C
