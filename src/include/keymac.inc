C
C     @(#)  412.1 date 6/11/92 keymac.inc 
C
C
C     Filename    : keymac.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:19
C     Last change : 92/06/11 14:33:05
C
C     Copyright : Practical Technology International Limited  
C     File :- keymac.inc
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
C     This include file is used for the keymacro system
C     It consists of a file which is temporary and a logical
C     which says that that the macro file must be deleted
C     on completeion of the macro
C     
      CHARACTER*80 KEYMACFIL
      LOGICAL KEYMACACTIVE

      COMMON/KEYMCH/KEYMACFIL
      COMMON/KEYMLG/KEYMACACTIVE
