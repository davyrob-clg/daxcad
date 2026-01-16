C
C     @(#)  412.1 date 6/11/92 work3d.inc 
C
C
C     Filename    : work3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:56
C     Last change : 92/06/11 14:43:41
C
C     Copyright : Practical Technology Limited  
C     File :- work3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This common block holds the scratch work  variables used
C     by the 3D module 
C
C     IDAT is the integer element of the database record.
C     WFDAT is the Real element of database.             
C     VRTDAT is the scratch Vertice array.
C
      INTEGER IDAT(6,100)
C
      REAL WFDAT(6,100),VRTDAT(3,100)
C
      COMMON /WORK3D/WFDAT,VRTDAT,IDAT
C

