C
C     @(#)  412.1 date 6/11/92 sioport.inc 
C
C
C     Filename    : sioport.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:45
C     Last change : 92/06/11 14:40:50
C
C     Copyright : Practical Technology Limited  
C     File :- sioport.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C   This include file contains the characteristics
C   of the sio lines that is aout to be changed
C
      INTEGER*2 ASPEED(3),APARITY(3),ABPC(3),ASTOPB(3)
      INTEGER*2 ADTR(3),AINSYC(3),AHOST(3),ARAW(3),ABPAD(3)
C
      INTEGER*2 AERASE(3),AKILL(3),AEOF(3),ANONL(3),ANLC(3)
      INTEGER*2 AQUITE(3),ARTS(3),ARTSE(3),ADCDE(3)
      INTEGER*2 ACTSE(3),AERRE(3),ACHKFR(3),AQUITC(3)
      INTEGER*2 AINTE(3),AINTC(3),ASUSPE(3),ASUSPC(3)
      INTEGER*2 ARAWN(3),AHUPC(3)


C   This common area is used to determine what is using the
C   serial lines internally to daxcad. Also there is a common
C   block element to count the numaer of times that the line has been
C   called.
      INTEGER*2 SIOUSE(3),CALCNT(3)

      COMMON /SIODAT/ ASPEED,APARITY,ABPC,ASTOPB,ADTR,AINSYC,AHOST,
     +                ARAW,ABPAD,SIOUSE,CALCNT,
     + AERASE,AKILL,AEOF,ANONL,ANLC,
     + AQUITE,ARTS,ARTSE,ADCDE,
     + ACTSE,AERRE,ACHKFR,AQUITC,
     + AINTE,AINTC,ASUSPE,ASUSPC,
     + ARAWN,AHUPC

