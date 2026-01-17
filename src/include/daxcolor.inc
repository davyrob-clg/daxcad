C
C     @(#)  412.1 date 6/11/92 daxcolor.inc 
C
C
C     Filename    : daxcolor.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:03
C     Last change : 92/06/11 14:27:36
C
C     Copyright : Practical Technology Limited  
C     File :- daxcolor.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains all necessary declarations
C2    for the full support of colour attributes within the
C2    DAXCAD system.
C
      
      INTEGER*4 COLIRG,COLDRG,COLBAK,COLFOR,COLFIL,COLDRW
      INTEGER*4 COLERS,COLPEN 
      
      INTEGER*4 COLOR_MAP_SIZE
      PARAMETER (COLOR_MAP_SIZE=16)
      INTEGER*4 COLOR_MAP(0:COLOR_MAP_SIZE-1)
      INTEGER*4 COLOR_MAP_OFFSET
C
      INTEGER*4 XOR_FLAG 
      INTEGER*4 OR_FLAG  
      INTEGER*4 REP_FLAG 
      INTEGER*4 TOOLPEN_COLOR
C
      PARAMETER(XOR_FLAG = 6)
      PARAMETER(OR_FLAG  = 7)
      PARAMETER(REP_FLAG = 3)
C
      INTEGER*4 RASTER_OP_FLAG
C
      COMMON /DAXCOL/COLIRG,COLDRG,COLBAK,COLFOR,COLFIL,COLDRW,
     +               COLERS,COLPEN
C
      COMMON /COLTAB/ COLOR_MAP, COLOR_MAP_OFFSET, RASTER_OP_FLAG,
     +                TOOLPEN_COLOR
C
C2       COLIRG - Colour Index Range (maximum available colours)
C2       COLDRG - Colour Display Range (maximum displayable)
C2       COLBAK - Background Colour Index
C2       COLFOR - Foreground Colour Index
C2       COLFIL - Current Fill Colour Index
C2       COLDRW - Current Draw Colour Index
C2       COLERS - Current Erase Colour Index
C2       COLPEN - Current Active Colour Index
C
C
C2       COLOR_MAP  -        Is a look up table of entries
C2                           to map DAXCAD colours to system color
C2                           map entries.
C2
C2       COLOR_MAP_SIZE  -   is the size of the look up table of entries
C2                           to map DAXCAD colours to system color
C2                           map entries.
C2
C2       COLOR_MAP_OFFSET -  Is the first free position in the system
C2                           color map that DAXCAD is using and has
C2                           mapped to COLOR_MAP 
C2
C2       TOOLPEN_COLOR    -  Value of COLOR set by most recent call
C2                           to TOOLPEN()
C2
