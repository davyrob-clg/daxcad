C
C     @(#)  412.1 date 6/11/92 dhead.inc 
C
C
C     Filename    : dhead.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:06
C     Last change : 92/06/11 14:28:15
C
C     Copyright : Practical Technology Limited  
C     File :- dhead.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains all of the
C2    declarations required for the creation
C2    of the drawing header for ROOTS system
C2    drawings.
C
      REAL RDHEAD(20)
C
      CHARACTER*8 CDHEAD(10)
C
      COMMON /DHEAD1/CDHEAD
C
      COMMON /DHEAD2/RDHEAD
C
C2    CDHEAD(1) = DBUNIT database units descriptor
C2    CDHEAD(2) = PAPUNT paper units descriptor
C2    CDHEAD(3) = DRGSCL drawing scale descriptor
C2    CDHEAD(4) = DRWSHT drawing sheet descriptor
C2    CDHEAD(5-10) = spare
C
C2    RDHEAD(1) = DBUFAC database units factor
C2    RDHEAD(2) = PAPFAC paper units factor
C2    RDHEAD(3) = PAPTOW paper to world scale ratio
C2    RDHEAD(4) = DRWSIZ(1) paper size on X-axis (paper units)
C2    RDHEAD(5) = DRWSIZ(2) paper size on Y-axis (paper units)
C2    RDHEAD(6) = WPXMIN min X of paper mapping on the world
C2    RDHEAD(7) = WPYMIN min Y of paper mapping on world
C2    RDHEAD(8) = WPXMAX max X of paper mapping on world
C2    RDHEAD(9) = WPYMAX max Y of paper mapping on world
C2    RDHEAD(10)= WPORGX origin in world space of paper X
C2    RDHEAD(11)= WPORGY origin in world space of paper Y
C2    RDHEAD(12)= DRWSCL value of drawing scale
C2    RDHEAD(13-20) = spare
C
C
