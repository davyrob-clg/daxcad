C
C     @(#)  412.1 date 6/11/92 ddcode.inc 
C
C
C     Filename    : ddcode.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:05
C     Last change : 92/06/11 14:27:53
C
C     Copyright : Practical Technology Limited  
C     File :- ddcode.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Include file for drawing codes
C
C
C     DC_NORMAL          ... Normal drawing ops straight onto screen
C     DC_PORT_AND_SCREEN ... Draws the next of on the screen and into the hidden view as well
C     DC_PORT_ONLY_CLIPPED . Draws only into the current active view with
C                            supported clipping for a viewport.
C     DC_PORT_HIDDEN     ... Will update only the hidden view with an normal
C                            drawing op. This will be the current active port
C
      INTEGER*4 DC_NORMAL
      INTEGER*4 DC_PORT_AND_SCREEN
      INTEGER*4 DC_NORMAL_CLIPPED_TO_PORT
      INTEGER*4 DC_PORT_NORMAL_HIDDEN
C
      PARAMETER ( DC_NORMAL                 = 0 ) 
      PARAMETER ( DC_PORT_AND_SCREEN        = 1 )
      PARAMETER ( DC_NORMAL_CLIPPED_TO_PORT = 2 )
      PARAMETER ( DC_PORT_NORMAL_HIDDEN      =3 )
C
