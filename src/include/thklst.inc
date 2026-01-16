C
C     @(#)  412.1 date 6/11/92 thklst.inc 
C
C
C     Filename    : thklst.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:49
C     Last change : 92/06/11 14:41:43
C
C     Copyright : Practical Technology Limited  
C     File :- thklst.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C1    This include file contains the list of line thicknesses
C1    defined by the 'thick.dax' file.
C1    Modified for RS 6000 compiler
C
C
      INTEGER*2 FNTDFS(127)
      INTEGER*4 DFSTOT
C
      COMMON /FNTDFS/FNTDFS
      COMMON /FNTDI4/DFSTOT
