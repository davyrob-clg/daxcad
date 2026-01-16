C
C     @(#)  412.1 date 6/11/92 funit.inc 
C
C
C     Filename    : funit.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:14
C     Last change : 92/06/11 14:30:25
C
C     Copyright : Practical Technology Limited  
C     File :- funit.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C1    This include file contains declarations of
C1    file units for use in the handling of the
C1    storage files necessary to maintain the
C1    part database.
C
C2    PARFUN is the file unit attached to
C2    the main part storage file,during a 
C2    complete database save or retrieve operation.
C
C2    ASCII indicates wether the drawing file is being stored
C2    in ascii for transfer to another system.

      INTEGER*4 INPARF,OUPARF
      LOGICAL INPASC
      CHARACTER*255 LINE
C
      COMMON /FILUN/INPARF,OUPARF,INPASC
      COMMON /CODER/LINE
C
