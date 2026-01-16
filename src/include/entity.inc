C
C     @(#)  412.1 date 6/11/92 entity.inc 
C
C
C     Filename    : entity.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:11
C     Last change : 92/06/11 14:29:21
C
C     Copyright : Practical Technology Limited  
C     File :- entity.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This include file store the number associated 
C     with the entity
C
C             Value from following :
C             --------------------
C               2 - Marker
C               3 - Line
C               5 - Arc
C               6 - Ellipse
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
C
C     The following sub-records are used for entity record storage:-
C
C              40 - Linear segment record
C              41 - Arc segment record
C              42 - Text segment record
C              43 - Arrowhead segment
C              45 - General Header record
C              46 - General Trailer record
C
C     Note :-  These sub-records have the same storage format as
C              the major entity types.
C
      INTEGER*2 LINE,ARC,HATCH,TEXT,TBLOCK,LDIMN,RDIMN,DDIMN,ADIMN,
     +          GROUP,DETAIL,SYMBM,SYMBI,SPLINE,COMPM,COMPI,MILIST
     1          ,GLABEL,LINSEG,ARCSEG,TEXSEG,TERMIN,HEADER,TRAILR,
     1           MARKER,ELIPSE,CENLIN

CC
C      COMMON /ENTDAT/LINE,ARC,HATCH,LDIMN,TEXT,TBLOCK,RDIMN,DDIMN,ADIMN,
C     +               GROUP,DETAIL,SYMBM,SYMBI,SPLINE,COMPM,COMPI,MILIST
C     1               ,GLABEL,LINSEG,ARCSEG,TEXSEG,TERMIN,HEADER,TRAILR,
C     1                MARKER
CCC
C     Replace the include with a parameter statement to avoid the problem
C     of overwriting any of the entity values
C        
      PARAMETER (
     +           LINE   = 3 ,
     +           ARC    = 5 ,
     +           ELIPSE = 6 ,
     +           SPLINE = 7 ,        
     +           CENLIN = 30,
     +           HATCH  = 31,
     +           TEXT   = 85,
     +           LDIMN  = 33,
     +           ADIMN  = 34,
     +           RDIMN  = 35,
     +           DDIMN  = 37,
     +           GROUP  = 50,
     +           DETAIL = 52,
     +           SYMBM  = 54,
     +           COMPM  = 56,
     +           SYMBI  = 64 )
C
      PARAMETER (
     +           COMPI  = 66,
     +           TBLOCK = 86,
     +           GLABEL = 36,
     +           LINSEG = 40,
     +           ARCSEG = 41,
     +           TEXSEG = 42,
     +           TERMIN = 43,
     +           HEADER = 45,
     +           TRAILR = 46,
     +           MARKER = 2 ,
     +           MILIST = 200 )
C
C     --------------------------------------------------------------
C
