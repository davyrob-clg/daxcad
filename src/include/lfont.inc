C
C     @(#)  412.1 date 6/11/92 lfont.inc 
C
C
C     Filename    : lfont.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:21
C     Last change : 92/06/11 14:33:28
C
C     Copyright : Practical Technology Limited  
C     File :- lfont.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C1    This include file contains the declarations
C1    of all variables necessary for definition
C1    and manipulation of line fonts for use in
C1    the ROOTS draughting system.
C
C     A total of 10 line fonts is allowed for at the
C     present time
C
C2    LFONTI contains pointer to start of pattern
C2           data in LFTAB2,and number of segments
C2           in the pattern
C2    LFONTR contains the length of the font pattern
C2           in paper units,and equivalent screen units
C2           at the current view scale
C2    LFTAB2 contains the pattern segment data in
C2           fractions of a pattern length,and the
C2           incremental screen coords for the segments
C2           of the pattern in the current line
C2    NFONTS contains the number of line fonts available
C
      INTEGER*4 LFONTI(2,0:20),NFONTS
C
      REAL LFONTR(2,0:20),LFTAB2(3,100)
      INTEGER*2 PLTHKI(0:127)
      REAL LTHKR(2,0:127)

      CHARACTER*20 FONTNM(0:20)
C
      COMMON /LINFNT/NFONTS,LFONTI,LFONTR,LFTAB2
      COMMON /LTHKC/LTHKR,PLTHKI
C
      COMMON /LINNAM/FONTNM
