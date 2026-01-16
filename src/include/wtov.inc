C
C     @(#)  412.1 date 6/11/92 wtov.inc 
C
C
C     Filename    : wtov.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:57
C     Last change : 92/06/11 14:44:21
C
C     Copyright : Practical Technology Limited  
C     File :- wtov.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C1    This common block cobtains the data describing
C1    the relationship between the world,paper and
C1    screen coordinate systems.
C
C1    WXMIN    is the minimum extent in the world space
C1             of the current view port
C1    WXMAX    is the maximum extent in the world space
C1             of the current view port
C1    WYMIN    is the minimum extent in the world space
C1             of the current view port
C1    WYMAX    is the maximum extent in the world space
C1             of the current view port
C
C1    LWXMIN    is the minimum extent in the world space
C1              of the previous view port
C1    LWXMAX    is the maximum extent in the world space
C1              of the previous view port
C1    LWYMIN    is the minimum extent in the world space
C1              of the previous view port
C1    LWYMAX    is the maximum extent in the world space
C1              of the previous view port
C
C1    ZOMLIM   is a logical flag which is set true if
C1             the current vieport maps to the paper limits
C
C1    WPXMIN   is the minimum extent in the world space
C1             which may be mapped to the currently selected
C1             paper at the current draw scale.
C1    WPXMAX   is the maximum extent in the world space
C1             which may be mapped to the currently selected
C1             paper at the current draw scale.
C1    WPYMIN   is the minimum extent in the world space
C1             which may be mapped to the currently selected
C1             paper at the current draw scale.
C1    WPYMAX   is the maximum extent in the world space
C1             which may be mapped to the currently selected
C1             paper at the current draw scale.
C1    WPORGX   is the origin on the world space of the
C1             bottom left corner of the paper
C1    WPORGY   is the origin on the world space of the
C1             bottom left corner of the paper
C
      REAL WXMIN,WXMAX,WYMIN,WYMAX,
     1   SCXMIN,SCXMAX,SCYMIN,SCYMAX,
     2   VXMIN,VXMAX,VYMIN,VYMAX,WVXY(3,3),VWXY(3,3),
     3   WPXMIN,WPXMAX,WPYMIN,WPYMAX,WPORGX,WPORGY,
     4   LWXMIN,LWYMIN,LWXMAX,LWYMAX
C
      LOGICAL ZOMLIM
C
      COMMON /WTOV/WXMIN,WXMAX,WYMIN,WYMAX,
     1       SCXMIN,SCXMAX,SCYMIN,SCYMAX,
     2       VXMIN,VXMAX,VYMIN,VYMAX,WVXY,VWXY,
     3       WPXMIN,WPXMAX,WPYMIN,WPYMAX,WPORGX,WPORGY,
     4       LWXMIN,LWYMIN,LWXMAX,LWYMAX,
     5       ZOMLIM
C
C
