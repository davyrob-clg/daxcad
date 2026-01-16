C
C     @(#)  412.1 date 6/11/92 save.inc 
C
C
C     Filename    : save.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:42
C     Last change : 92/06/11 14:40:19
C
C     Copyright : Practical Technology Limited  
C     File :- save.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This is an include for the save and restore routines
C      and other general purpose variables for control
C
      INTEGER*4 SUNIT,RECC,CMIPOS,LAYDAT(256),LAYTOT,HATUNT
      INTEGER*2 TMPMIP,TMPPDP
      REAL CMXMAX,CMXMIN,CMYMAX,CMYMIN
      LOGICAL LAYTST,CMPLAY,CMPDRW,CMPERS,CMPBOX
      CHARACTER*80 SAVNAM

      COMMON/SAVEI/ TMPMIP,TMPPDP
      COMMON/SAVEM/ CMIPOS,SUNIT,RECC,LAYTOT,LAYDAT,HATUNT
      COMMON/SAVEC/ SAVNAM
      COMMON/SAVEL/ LAYTST,CMPLAY,CMPDRW,CMPERS,CMPBOX
      COMMON/SAVER/ CMXMAX,CMXMIN,CMYMAX,CMYMIN

