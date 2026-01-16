C
C     @(#)  412.1 date 6/11/92 delstore.inc 
C
C
C     Filename    : delstore.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:05
C     Last change : 92/06/11 14:28:13
C
C     Copyright : Practical Technology Limited  
C     File :- delstore.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Include File : delstore.inc.
C
C     This file contains the data used to cancel the last delete between
C     points.
C
      INTEGER*2 DNMIPO,DNPDPO,DIMBUF(1:13),DIDBUF(1:4),OLDMIP,NEWMIP
      REAL      DRDBUF(1:6)
C
      COMMON / DELSTO/DRDBUF,DIMBUF,DIDBUF,DNMIPO,DNPDPO,OLDMIP,NEWMIP
C
