C
C     @(#)  412.1 date 6/11/92 vpops.inc 
C
C
C     Filename    : vpops.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:54
C     Last change : 92/06/11 14:43:19
C
C     Copyright : Practical Technology Limited  
C     File :- vpops.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     INclude file for viewporting options that apolly to the display lists
C     that are within daxcad
C
C
C     VP_OP_NORMAL .. Nothing happens
C     VP_OP_MOVE   .. will draw a graphic entity withoouyt altering the display
C     VP_OP_DRAW  .. will draw a graphic entity withoouyt altering the display
C     VP_OP_CANCEL .. Removes the last entity entered from the display list
C     VP_OP_ADD    .. Adds a new entity into the display list for the current view 

      INTEGER*4 VP_OP_NORMAL
      INTEGER*4 VP_OP_MOVE
      INTEGER*4 VP_OP_CANCEL
      INTEGER*4 VP_OP_ADD    
      INTEGER*4 VP_OP_DRAW

      PARAMETER ( VP_OP_NORMAL  = 0 ) 
      PARAMETER ( VP_OP_MOVE    = 1 ) 
      PARAMETER ( VP_OP_CANCEL  = 2 )
      PARAMETER ( VP_OP_ADD     = 3 )
      PARAMETER ( VP_OP_DRAW    = 4 )
