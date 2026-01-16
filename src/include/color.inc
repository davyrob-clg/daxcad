C
C     @(#)  412.1 date 6/11/92 color.inc 
C
C
C     Filename    : color.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:59
C     Last change : 92/06/11 14:25:25
C
C     Copyright : Practical Technology International Limited  
C     File :- color.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C
C
C     Color Table definitions
C
C
C     MAXTAB  ... Maximum number of color map tables
C     COLOR_TABLE  Colormap table. 1st element
C                  is number of planes. and RGB
C                  intensities. 2nd element is 
C                  each color index number. 3rd elemnt
C                  is the table number ID.
C    COLOR_ACTIVE  Whether the color table is active
C    COLOR_DISPLAY Defines type of display 
C                  1 MONO
C                  2 4 plane color
C                  3 8 plane color
C    COLCUR        Current active colormasp loaded
C    COLOR_LOCKED  Flag to indicate the colormap loaded
C                  is locked into  the program and 
C                  cannot be changed by the notifier 
C                  or any other calls load colormap

      INTEGER*4 MAXTAB
      PARAMETER(MAXTAB=10)
C
      INTEGER*4 COLOR_TABLE(3,0:255,0:MAXTAB)
      INTEGER*4 COLOR_DISPLAY(0:MAXTAB)
      INTEGER*4 COLOR_DEFAULT(3,0:15)
      INTEGER*4 COLCUR
      INTEGER*4 COLORMAP
      LOGICAL COLOR_ACTIVE(MAXTAB)
      LOGICAL COLOR_LOCKED

      COMMON/COLTAB/COLOR_TABLE,COLOR_ACTIVE,
     +              COLOR_DISPLAY,
     +              COLCUR,
     +              COLOR_DEFAULT,
     +              COLORMAP,
     +              COLOR_LOCKED
