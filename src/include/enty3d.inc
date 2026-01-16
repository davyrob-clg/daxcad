C
C     @(#)  412.1 date 6/11/92 enty3d.inc 
C
C
C     Filename    : enty3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:11
C     Last change : 92/06/11 14:29:24
C
C     Copyright : Practical Technology Limited  
C     File :- enty3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C
C     This include file store the number associated 
C     with the entity
C
C             Value from following :
C             --------------------
C               2 - Point
C               3 - Line
C               5 - Arc
C               7 - Spline
C              30 - Centreline
C              31 - Crosshatching
C              33 - Linear  Dimension
C              34 - Angular     "
C              35 - Radial      "
C              36 - General Label
C              37 - Diameter Dimension
C              50 - Group
C              52 - Detail
C              54 - Symbol Master
C              56 - Component Master
C              64 - Symbol Instance
C              66 - Component Instance
C              82 - Text Node
C              85 - Text
C              86 - Text block
C             101 - 3D Box
C
      INTEGER LINE,ARC,HATCH,TEXT,TBLOCK,LDIMN,RDIMN,DDIMN,ADIMN,
     +          GROUP,DETAIL,SYMBM,SYMBI,SPLINE,COMPM,COMPI,MILIST,
     1          BOX
C
C
      COMMON /ENTDAT/LINE,ARC,HATCH,LDIMN,TEXT,TBLOCK,RDIMN,DDIMN,ADIMN,
     +               GROUP,DETAIL,SYMBM,SYMBI,SPLINE,COMPM,COMPI,MILIST
     1               ,BOX
C
C
