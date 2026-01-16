C
C     @(#)  412.1 date 6/11/92 menun.inc 
C
C
C     Filename    : menun.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:24
C     Last change : 92/06/11 14:35:16
C
C     Copyright : Practical Technology Limited  
C     File :- menun.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     MEN    = menu number of hit cell
C     CELLN  = menu cell number of hit cell
C     DISLAY = display/layer menu flag
C     CTEXT  = text of hit menu cell  
C     ERRMSG = an error message has been punted out
C     CCMD   = token for the hit menu cell
C     VNCCMD = verb/noun number of the hit cell
C
      INTEGER*4 CELLN,MEN,CS,VNCCMD
      LOGICAL ERRMSG
      LOGICAL DISLAY
      CHARACTER*80 CTEXT,CCMD*1
C
      COMMON /MENU/CS,CELLN,MEN,ERRMSG,DISLAY,VNCCMD
      COMMON /MENUT/CTEXT,CCMD

