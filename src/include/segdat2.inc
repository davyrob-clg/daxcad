C
C     @(#)  412.1 date 6/11/92 segdat2.inc 
C
C
C     Filename    : segdat2.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:44
C     Last change : 92/06/11 14:40:30
C
C     Copyright : Practical Technology Limited  
C     File :- segdat2.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains the declarations
C2    for screen segment mapping.The data blocks are
C2    defined for a maximum of 8*8 screen matrix.
C2         MAPSIZ is the matrix size in use
C2         IXYLIM is the limit number of tests to apply in coord range
C2         CELMAP contains cell id numbers in the matrix
C2         CELWDS contains cell control words in the matrix
C2         MAPCEL contains cell coords by cell id number
C2         XYLIMS contains the upper limits of each cell boundary
C2         SEGMAP contains the mapping word for inter-cell vectors
C2         CELMAX parameter defines maximum size of cell mapping
      INTEGER CELMAX
      PARAMETER (CELMAX = 8)
      INTEGER MAPSIZ,IXYLIM
      INTEGER CELMAP(CELMAX,CELMAX),MAPCEL(2,CELMAX*CELMAX)
      INTEGER CELWDS(2,CELMAX,CELMAX)
      INTEGER SEGMAP(2,CELMAX*CELMAX,CELMAX*CELMAX)
      REAL XYLIMS(2,CELMAX)
C
      COMMON /SEGDAT/XYLIMS,MAPSIZ,IXYLIM,CELMAP,MAPCEL,CELWDS,SEGMAP
C
C
