C
C     @(#)  412.1 date 6/11/92 label.inc 
C
C
C     Filename    : label.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:16:02
C     Last change : 92/06/11 14:33:21
C
C     Copyright : Practical Technology International Limited  
C     File :- label.inc
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
C
C     Label control include file
C
C     LABPOS ... Label postion coord
C     LABFNT ... Label font
C
C
C
      INTEGER*2 LABMAX
      PARAMETER(LABMAX=20)
C
      INTEGER*2 LABPOS(2,LABMAX)
C
      CHARACTER*256 LABTXT(LABMAX)
C
      INTEGER*2 LABFNT(LABMAX)
      INTEGER*4 LABCOL(2,LABMAX)
      INTEGER*4 LABDIA(LABMAX)

      LOGICAL LABACT(LABMAX)
      LOGICAL LABDIS(LABMAX)
      LOGICAL LABFIL(LABMAX)
      LOGICAL LABINV(LABMAX)
C
C
      COMMON/LABI2/LABPOS,LABFNT
      COMMON/LABLG/LABACT,LABDIS,LABCOL,LABFIL,LABINV,LABDIA
      COMMON/LABCH/LABTXT
