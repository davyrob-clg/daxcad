C
C     @(#)  412.1 date 6/11/92 props.inc 
C
C
C     Filename    : props.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:36
C     Last change : 92/06/11 14:38:48
C
C     Copyright : Practical Technology Limited  
C     File :- props.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains the
C2    definitions required for PROPERTIES
C2    control and processing.
C
      INTEGER*2 PRIBUF(8),PRCBI(2,4)
      INTEGER*2 NPRPOS,NPCPOS,PROPPP
      INTEGER*2 PRLBUF(8,20),PRSBUF(8,20)
      INTEGER*4 PDATYP,PACODE
      LOGICAL PROPOK
C
CAPOLLO|SUN
      INTEGER*2 PRIFIL(8,10000),PRCFII(2,10000)
      CHARACTER*80 PRCFIL(10000)
CAPOLLO|SUN
CIBM
C      INTEGER*2 PRIFIL,PRCFII
C      CHARACTER*80 PRCFIL
CIBM

      INTEGER*2  PRIFLU,PRCFIU,PRCFLU

      CHARACTER*80 PRCBUF(4),PRLNAM,PRSNAM
C
      COMMON /PROP1/PDATYP,PACODE,PROPOK,PRIBUF,PRCBI,NPRPOS,NPCPOS,
     +              PRIFIL,PRCFII,PROPPP,PRLBUF,PRSBUF,
     +              PRIFLU,PRCFIU,PRCFLU
C
      COMMON /PROP2/PRCBUF,PRCFIL,PRLNAM,PRSNAM
C
