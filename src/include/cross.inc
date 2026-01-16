C
C     @(#)  412.1 date 6/11/92 cross.inc 
C
C
C     Filename    : cross.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:01
C     Last change : 92/06/11 14:25:51
C
C     Copyright : Practical Technology Limited  
C     File :- cross.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Cross file variables
C     CRUNIT      Scratch file unit number asscociated with it.
C     CRSPTR      Current pointer. Reset if displsay redrawn for any reason
C                 include changing of viewports
C
      INTEGER*4 CRUNIT,CRSPTR
      COMMON/CROSSC/CRUNIT,CRSPTR
