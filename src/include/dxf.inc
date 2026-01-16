C
C     @(#)  412.1 date 6/11/92 dxf.inc 
C
C
C     Filename    : dxf.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:09
C     Last change : 92/06/11 14:29:09
C
C     Copyright : Practical Technology Limited  
C     File :- dxf.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     
C     This include file is for the DXF read and write
C     routines.
      INTEGER*4 NOH,NOG
      INTEGER*2 LSTYLE(256),NUMLAY
      INTEGER*2 COLOR(15)
      CHARACTER TSTYLE(10)*10
      CHARACTER*26 LINESN(20)
C
      COMMON/HATCHN/NOH,NOG
      COMMON/NLAY/NUMLAY,LSTYLE
      COMMON/DXFCH/TSTYLE,COLOR,LINESN

