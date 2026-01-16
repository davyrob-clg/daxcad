C
C     @(#)  412.1 date 6/11/92 section.inc 
C
C
C     Filename    : section.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:43
C     Last change : 92/06/11 14:40:26
C
C     Copyright : Practical Technology Limited  
C     File :- section.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This include file contains all references
C     for control of related bitmap files and
C     drawing sections for use in mapping
C     applications.
C     This area of control is specific to 
C     ADVENT SYSTEMS use of the code at the moment.
C     
C2      OSGXY  contains OS grid origin,and extents
C2             for each section in the drawing
C2      WWGXY  contains local world coords of the
C2             origin and extents of each section
C2      SECTOK contains logical flags indicating
C2             the sections in use
C2      SECNAM contains the SECTION and BITMAP
C2             names in use within the drawing
C2      SHTSIZ contains section sheet size
C2      SHTKEY contains key of section sheet
C2      SHTNAM contains name of section sheet
C2      SECNUM contains the current section number
C2             for use in i/o operations.
C2      SECSCL contains the drawing scale
C
      INTEGER*4 SECNUM
      REAL  WWGXY(1:4,0:16),SHTSIZ(1:2),SECSCL
      DOUBLE PRECISION OSGXY(1:4,0:16)
      LOGICAL SECTOK(0:16)
      CHARACTER*80 SECNAM(1:2,0:16),SHTKEY*3,SHTNAM*14
C
      COMMON /SECTA/ OSGXY,WWGXY,SECTOK,SECNUM,SHTSIZ,SECSCL
C
      COMMON /SECTB/ SECNAM,SHTKEY,SHTNAM
C
