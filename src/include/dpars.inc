C
C     @(#)  412.1 date 6/11/92 dpars.inc 
C
C
C     Filename    : dpars.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:08
C     Last change : 92/06/11 14:28:54
C
C     Copyright : Practical Technology Limited  
C     File :- dpars.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains all of the
C2    declarations required for storage and
C2    retrieval of drawing parameters for 
C2    drawings and parts created by the ROOTS
C2    system.
C
      REAL RPARMS(50)
C
      INTEGER*4 IPARMS(50)
C
      LOGICAL LPARMS(10)
C
      COMMON /DPARS/RPARMS,IPARMS,LPARMS
C
C2    RPARMS(1) = DTHGT dimension text height
C2    RPARMS(2) = DTWDT dimension text width
C2    RPARMS(3) = DTOFF dim text offset
C2    RPARMS(4) = GAPL dim gap length
C2    RPARMS(5) = EXTL dim extension length
C2    RPARMS(6) = ALNG dim arrowhead length
C2    RPARMS(7) = AWDT dim arrowhead width
C2    RPARMS(8) = TOL dim tolerance
C
C2    RPARMS(9) = HANG hatching angle (degrees)
C2    RPARMS(10) = LANG hatch angle (radians)
C2    RPARMS(11) = HDIST hatching distance
C
C2    RPARMS(12) = FRAD fillet radius
C2    RPARMS(13) = ARCRAD current arc construction radius
C2    RPARMS(14) = ANGLE current arc construction angle
C
C2    RPARMS(15) = TWIDTH text width
C2    RPARMS(16) = THIGT text height
C2    RPARMS(17) = SLANT text slant
C2    RPARMS(18) = TANGL text base angle (degrees)
C
C2    RPARMS(21) = GRIDOX grid origin on X-axis
C2    RPARMS(22) = GRIDOY grid origin on Y-axis
C2    RPARMS(23) = GRIDSX grid spacing on X-axis
C2    RPARMS(24) = GRIDSY grid spacing on Y-axis
C
C2    IPARMS(1) = PREC dim precision
C
C2    IPARMS(2) = FLTRIM fillet trimming condition
C2    IPARMS(3) = FILLET type of arc for filleting
C2    IPARMS(4) = FLOFF 
C
C2    IPARMS(5) = NNCOPY number of copies for move ops
C
C2    IPARMS(6) = CLFONT current line font
C2    IPARMS(7) = CLAYER current construction layer
C
C2    IPARMS(8) = COLOUR current construction colour
C2    IPARMS(9) = LAYER current entity layer
C2    IPARMS(10) = SPARE
C2    IPARMS(11) = FONT current entity font
C2    IPARMS(12) = STATUS current entity status
C2    IPARMS(13) = GRTYPE set GRID type
C
C2    LPARMS(1) = CROSSH cross hatch flag
C2    LPARMS(2) = COPYIT copy flag
C2    LPARMS(3) = SETGRD grid status
C2    LPARMS(4) = RADSET fixed radius flag
C2    LPARMS(5) = ARCSET fixed arc angle flag
C
C     @(#)  412.1 date 6/11/92 dpars.inc 
C
C
C     Filename    : dpars.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:08
C     Last change : 92/06/11 14:28:54
C
C     Copyright : Practical Technology Limited  
C     File :- dpars.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
