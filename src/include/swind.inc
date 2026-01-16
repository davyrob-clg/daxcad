C
C     @(#)  412.1 date 6/11/92 swind.inc 
C
C
C     Filename    : swind.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:48
C     Last change : 92/06/11 14:41:34
C
C     Copyright : Practical Technology Limited  
C     File :- swind.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This include file contains the necessary
C     data to allow window searches for graphic
C     entities from the screen.
C
C1    SWINDU is the unit number of the already open
C1    scratch file (direct access) used for storage
C1    of the MI pointers of the entities found.
C
C2    NDATA is the number of entities in the workfile
C2    VNDATA is the pointer to the first valid display flag
C2           all screen positions before this are no longer
C2           valid since some display function has ocurred.
C
      INTEGER*4 SWINDU,NDATA,VNDATA
      LOGICAL DRAWN
      COMMON /SWIND/NDATA,VNDATA,SWINDU,DRAWN
C
C
