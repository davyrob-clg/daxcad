C
C     @(#)  412.1 date 6/11/92 polyclip.inc 
C
C
C     Filename    : polyclip.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:33
C     Last change : 92/06/11 14:38:10
C
C     Copyright : Practical Technology Limited  
C     File :- polyclip.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     include file for polygon clips
C

      INTEGER*4 ENTRY,EXIT,FULClP,PARCLP,SIDE
      INTEGER*4 INTPS(2,200),NUMIP
      INTEGER*4 TYPE(200)
      INTEGER*4 ASSOC(200)
      INTEGER*4 CORNER(2,4)
      INTEGER*4 MAXPG
      LOGICAL  USEDI(200)
      LOGICAL  USEDP(200)
      LOGICAL WINOBS
      

C
      INTEGER*2 NPOLX(200,4)
      INTEGER*2 NPOLY(200,4)
      INTEGER*2 NPOLYN(4),POLYN
C
      PARAMETER(ENTRY = 1,
     +          EXIT = 2,
     +          FULCLP = 1,
     +          PARCLP = 2,
     +          MAXPG = 200)
C
C
C
      COMMON/POLYI4/INTPS,TYPE,ASSOC,NUMIP,CORNER,POLYN,
     +     NPOLX,NPOLY,NPOLYN
      COMMON/POLYL4/USEDI,USEDP,WINOBS
