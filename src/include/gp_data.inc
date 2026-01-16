C
C     @(#)  412.1 date 6/11/92 gp_data.inc 
C
C
C     Filename    : gp_data.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:15
C     Last change : 92/06/11 14:30:39
C
C     Copyright : Practical Technology Limited  
C     File :- gp_data.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C1    This common block cobtains the data describing
C1    screen coordinate systems.
C
C2    GPXMIN  is min X coord on graphics screen
C2    GPYMIN  is min Y coord on graphics screen
C2    GPXMAX  is max X coord on graphics screen
C2    GPYMAX  is max Y coord on graphics screen
C
      INTEGER*2 GPXMIN,GPYMIN,GPXMAX,GPYMAX
C
      COMMON /GPDATA/GPXMIN,GPYMIN,GPXMAX,GPYMAX
C
