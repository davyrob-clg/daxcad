C
C     @(#)  412.1 date 6/11/92 ndata.inc 
C
C
C     Filename    : ndata.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:30
C     Last change : 92/06/11 14:36:32
C
C     Copyright : Practical Technology Limited  
C     File :- ndata.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This common block contains the default settings
C2    for geometric manipulation of drawing data.
C
C2    PAPTOW   Scale ratio from paper to world space

C2    DTHGT    Dimension text height
C2    DTWDT    Dimension text width
C2    DTOFF    Dimension text offset
C2    GAPL     Dimension gap length
C2    EXTL     Dimension extension length
C2    ALNG     Dimension arrow head length
C2    AWDT     Dimension arrow head width
C2    PREC     Dimension precision
C2    TOL      Dimension tolerance

C2    HANG     Hatch angle in Degrees
C2    HDIST    Hatch spacing 
C2    CROSSH   .TRUE.   cross hatching 
C2             .FALSE.  no cross hatching


C2    FRAD     Fillet radius
C2    FLTRIM   Integer indicating type of trimmming
C2       1     trim both   lines back
C2       2     trim first  line back
C2       3     trim second line back 
C2       4     no trimming
C
C2    FILLET   Integer indicating type of arc to use
C2       1     Minor arc
C2       2     Major arc
C2       3     Full  circle

C2    GRIDOX   x coordinate of the origin of grid
C2    GRIDOY   y coordinate of the origin of grid
C2    GRIDSX   the size of the grid in the x direction
C2    GRIDSY   the size of the grid in the y direction
C2    SETGRD   is set .true. if the grid is on else .false.
C2    ISGRID   is set .true. if  grid1 is being called from egrid with grid inserted
C2    GRTYPE   1 cartesian
C2             2 isometric
C2             3 radial
C2
C2    SMODE This indicate the mode of curve with which it will
C2         be display initially
C2                                0 display points
C2                                1 polygon
C2                                2 curve
C2                                3 polygon & curve
C2    STYPE This indicate the type of curve with which it will
C2         be display initially
C2                                24 spline
C2                                16 hermite

C
      REAL HANG,HDIST,FRAD,LANG,
     +     TWIDTH,THIGT,SLANT,TANGL
      REAL GRIDOX,GRIDOY,GRIDSX,GRIDSY
      REAL DTHGT,DTWDT,DTOFF,GAPL,EXTL,ALNG,AWDT,PAPTOW,TOL
      INTEGER*2 JUST
      INTEGER*4 FLTRIM,FILLET,FLOFF,NNCOPY,PREC,SMODE,STYPE
      LOGICAL CROSSH,COPYIT,SETGRD,ISGRID                
      LOGICAL REDRAW_INTERRUPT
C
      COMMON /NDATA/HANG,HDIST,FRAD,LANG,
     +              TWIDTH,THIGT,SLANT,TANGL,
     2              FLTRIM,FILLET,FLOFF,NNCOPY,
     3              DTHGT,DTWDT,DTOFF,
     4              GAPL,EXTL,
     5              ALNG,AWDT,
     6              GRIDOX,GRIDOY,GRIDSX,GRIDSY,
     7              PAPTOW,PREC,TOL,SMODE,STYPE,
     8              CROSSH,COPYIT,SETGRD,ISGRID,JUST 
C
C2    GRTYPE   1 cartesian
C2             2 isometric
C2             3 radial
C2    ISOANG   isometric angle in rads
C2    ISOSX    width of the isometric diamond
C2    ISOSY    height of the iso diamond
C2    HISOSX   half the width of the isometric diamond
C2    HISOSY   half the height of the iso diamond
C
C2    REDRAW_INTERRUPT TRUE if keyboard redraw interrupts allowed
C         
      REAL ISOANG,ISOSX,ISOSY,HISOSX,HISOSY
      INTEGER*4 GRTYPE
      COMMON /GRIDI4/ GRTYPE,ISOANG,ISOSX,ISOSY,HISOSX,HISOSY
C
      COMMON /ARGS/ REDRAW_INTERRUPT

      
