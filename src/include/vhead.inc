C
C     @(#)  412.1 date 6/11/92 vhead.inc 
C
C
C     Filename    : vhead.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:51
C     Last change : 92/06/11 14:42:49
C
C     Copyright : Practical Technology Limited  
C     File :- vhead.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This include file contains all of the
C2    declarations required for creation of
C2    the viewing header for ROOTS system drawing
C2    and part files.
C
      REAL RVHED1(6,6),RVHED2(9,6),RVHED3(16,12)
C
      CHARACTER*8 CVHEAD(10)
C
      COMMON /VHEAD1/CVHEAD
C
      COMMON /VHEAD2/RVHED1,RVHED2,RVHED3
C
C2    CVHEAD(1-10) = spare character references
C
C2    RVHED1(1) = extents in world space (NOT USED AT THIS TIME)
C2    RVHED1(2) = WXMIN etc,extents of last view used
C2    RVHED1(3) = WPXMIN etc,extents of total view of paper in world
C2    RVHED1(4-6) = spare
C
C2    RVHED2 = 3x3 viewing transforms for 2d work
C2    RVHED2(3) = WVXY world to last view transform
C2    RVHED2(4) = VWXY last view to world transform
C2    RVHED2(5-6) = spare
C
C2    RVHED3 = 4x4 viewing transforms for 3d work
C2    RVHED3(1-12) = spare
C
C
