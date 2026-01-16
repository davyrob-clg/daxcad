C
C     @(#)  412.1 date 6/11/92 spool.inc 
C
C
C     Filename    : spool.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:46
C     Last change : 92/06/11 14:40:58
C
C     Copyright : Practical Technology Limited  
C     File :- spool.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      CHARACTER*60 ERRSTR,FILNM*100
      CHARACTER*1 ASCII,BINARY,READ,WRITE,DIREC,FTYPE
      LOGICAL WRTOK,BATCH
      INTEGER*4 IDNUM,ERRNO,RECL,BLOCKF,MP
      INTEGER*4 IN
      INTEGER*2 STRIDO,STRIDI,FILID

      COMMON /TYPE4/IDNUM,ERRNO,RECL,BLOCKF,WRTOK,BATCH,IN,MP

      COMMON /TYPE2/STRIDI,STRIDO,FILID

      COMMON /ESTR/ERRSTR,FILNM,ASCII,BINARY,READ,
     +                          WRITE,DIREC,FTYPE

