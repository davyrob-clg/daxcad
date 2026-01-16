C
C     @(#)  412.1 date 6/11/92 mst.inc 
C
C
C     Filename    : mst.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:28
C     Last change : 92/06/11 14:36:10
C
C     Copyright : Practical Technology Limited  
C     File :- mst.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      INTEGER*2 MIFILE(10),PDFI(4),CMIP(2)
      INTEGER*2 RLFILE(1:10),PRIFIL(8),PRCFII(2)

      INTEGER*2 NMIPOS,NPDPOS,NTXPOS,NPCPOS,NRLPOS,NPRPOS,NLYPOS
C                                
      REAL      PDFR(1:6)
C      
      COMMON /MST/ PDFR,MIFILE,PDFI,CMIP,
     +             NMIPOS,NPDPOS,NTXPOS,NRLPOS,
     +             NPCPOS,NPRPOS,NLYPOS,
     +             RLFILE,PRIFIL

      CHARACTER*80 TXFILE,PRCFIL,LNAME*20
      COMMON /MSTC/ TXFILE,PRCFIL,LNAME

