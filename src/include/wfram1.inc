C
C     @(#)  412.1 date 6/11/92 wfram1.inc 
C
C
C     Filename    : wfram1.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:55
C     Last change : 92/06/11 14:43:33
C
C     Copyright : Practical Technology Limited  
C     File :- wfram1.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    this include file contains the data declarations
C2    required for storage of wire-frame edge definitions.
C
      REAL WFLINS(6,1000),VIEWLN(6,1000)
      INTEGER NWFLIN
C
      COMMON /WFRAM1/WFLINS,VIEWLN,NEWFLIN
C
      SAVE /WFRAM1/
C
