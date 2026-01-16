C
C     @(#)  412.1 date 6/11/92 menug.inc 
C
C
C     Filename    : menug.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:24
C     Last change : 92/06/11 14:35:11
C
C     Copyright : Practical Technology Limited  
C     File :- menug.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     PLIMIT : contains the limits of the paper used for definition
C     GLIMIT : contains the grid limits of the menu definition
C     DLIMIT : contains the limits of the digitised grid
C     MLIMIT : contains the limits of the default menu loaded       
c
      REAL PLIMIT(2),GLIMIT(4),MLIMIT(4),DLIMIT(4),MENWXY(3,3)
      REAL MNANG,INCX,INCY,MINCX,MINCY,MENANG,TSCALE,GSCALE
      INTEGER*2 XN,YN,CELLX,CELLY,MXN,MYN
      LOGICAL PAPOK,GRIDOK,HITOK,TXTOK,MENDEF,MFILOK,MENOK,MSETOK,FILEOK
      CHARACTER*80 DMENU(50,50),MENTXT(50,50),DMFILE
C                               
C     these are the variables use during menu definition
      COMMON /MENUG1/PLIMIT,GLIMIT,INCX,INCY,DLIMIT,MNANG,TSCALE,
     +               GSCALE,
     +               PAPOK,GRIDOK,HITOK,TXTOK,MENDEF,MFILOK,MSETOK,
     +               XN,YN                       
      COMMON /MENUG2/DMENU
C
C     these are the variables used for the default menu loaded when the
C     digitiser menu is selected
      COMMON /MENUG3/ MENWXY,MLIMIT,MINCX,MINCY,MENANG,
     +                MENOK,FILEOK,
     +                MXN,MYN,CELLX,CELLY
      COMMON /MENUG4/ MENTXT,DMFILE 
C


