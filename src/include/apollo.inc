C
C
C     @(#)  412.1 date 6/11/92 apollo.inc 
C
C
C     Filename    : apollo.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:55
C     Last change : 92/06/11 14:23:29
C
C     Copyright : Practical Technology Limited  
C     File :- apollo.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      INTEGER*4 DISPDE,RMN,RCEL
      INTEGER*2 KEYSET(1:16),BUTSET(1:16),APCURX(20),APCURY(20),NPOS
      INTEGER*2 LASTBX,LASTBY,SOURCE,APOLLO_PAD
      INTEGER*2 MODE,TPX,TPY,TPH,ORG(2),CMOVE,CRSIZE,CONFIG,HIPLAN

      LOGICAL CURVIS,REPAINTPENDING
C
      COMMON/APOLL1/DISPDE,CURVIS,RMN,RCEL,KEYSET,BUTSET,
     +              LASTBX,LASTBY,SOURCE,MODE,TPX,TPY,TPH,ORG,CMOVE,
     1              CRSIZE,CONFIG,HIPLAN,APCURX,APCURY,NPOS,APOLLO_PAD
C
      COMMON/APOLL2/REPAINTPENDING

