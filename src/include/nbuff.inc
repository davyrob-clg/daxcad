C
C     @(#)  412.1 date 6/11/92 nbuff.inc 
C
C
C     Filename    : nbuff.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:28
C     Last change : 92/06/11 14:36:14
C
C     Copyright : Practical Technology Limited  
C     File :- nbuff.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This common block contains all necessary buffer storage
C     to hold a complete entity record for use in manipulation
C     procedures. Note that onle entity types totally defined
C     within a single PDF record can be wholly contained in this
C     buffer area.
C
      INTEGER*2 IMBUFF(1:13),IDBUFF(1:4),ICBUFF(1:2),RLBUFF(1:10),
     +          TICBUF(1:6,1:2),IDBUF1(1:4)
      REAL      RDBUFF(1:6),RDBUF1(1:6)

      CHARACTER*80 CBUFF
C
      COMMON /BUFF/RDBUFF,RDBUF1,IMBUFF,IDBUFF,IDBUF1,ICBUFF,
     +             RLBUFF,TICBUF
C
      COMMON /BUFFC/CBUFF


