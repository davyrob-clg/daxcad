C
C     @(#)  412.1 date 6/11/92 attributes.inc 
C
C
C     Filename    : attributes.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:56
C     Last change : 92/06/11 14:23:53
C
C     Copyright : Practical Technology International Limited  
C     File :- attributes.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C
C     Include file for tool attributes 
C     Customer available
C
C     TOOLS_ATTRIBUTES 
C
C     Element        Description
C        1           Header control Contains code of 1
C                    to indicate array has been set
C        2,3         X and Y origin position 
C        4,5         X amd Y size if applicable
C        6           Font
C        7           Foreground Color
C        8           Background Color
C        9           Basic type code 
C       10           Length of text 
C       11 - 30      Tool dependant codes
C
      INTEGER*4 TOOL_ATTRIBUTES(30)
      CHARACTER*255 TOOL_TEXT
