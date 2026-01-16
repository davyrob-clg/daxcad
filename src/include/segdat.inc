C
C     @(#)  412.1 date 6/11/92 segdat.inc 
C
C
C     Filename    : segdat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:43
C     Last change : 92/06/11 14:40:28
C
C     Copyright : Practical Technology Limited  
C     File :- segdat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains the declarations
C2    for screen segment mapping.The data blocks are
C2    defined for a maximum of 5*5 screen matrix.
C2         MAPSIZ is the matrix size in use
C2         IXYLIM is the limit number of tests to apply in coord range
C2         CELMAP contains cell id numbers in the matrix
C2         CELWDS contains cell control words in the matrix
C2         MAPCEL contains cell coords by cell id number
C2         XYLIMS contains the upper limits of each cell boundary
C2         SEGMAP contains the mapping word for inter-cell vectors
      INTEGER MAPSIZ,IXYLIM
      INTEGER CELMAP(5,5),MAPCEL(2,5*5)
      INTEGER CELWDS(5,5),SEGMAP(5*5,5*5)
      REAL XYLIMS(2,5)
C
      COMMON /SEGDAT/XYLIMS,MAPSIZ,IXYLIM,CELMAP,MAPCEL,CELWDS,SEGMAP
C
C
