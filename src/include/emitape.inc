C
C     @(#)  412.1 date 6/11/92 emitape.inc 
C
C
C     Filename    : emitape.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:10
C     Last change : 92/06/11 14:29:17
C
C     Copyright : Practical Technology Limited  
C     File :- emitape.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      CHARACTER*60 ERRSTR,FILNM*40
      CHARACTER*1 ASCII,BINARY,READ,WRITE,DIREC,FTYPE
      LOGICAL WRTOK,BATCH
      INTEGER*4 IDNUM,ERRNO,RECL,BLOCKF
      INTEGER*4 IN
      INTEGER*2 STRIDO,STRIDI,FILID

      COMMON /TYPE4/IDNUM,ERRNO,RECL,BLOCKF,WRTOK,BATCH,IN

      COMMON /TYPE2/STRIDI,STRIDO,FILID

      COMMON /ESTR/ERRSTR,FILNM,ASCII,BINARY,READ,
     +                          WRITE,DIREC,FTYPE

