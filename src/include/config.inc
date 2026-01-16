C
C     @(#)  412.1 date 6/11/92 config.inc 
C
C
C     Filename    : config.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:00
C     Last change : 92/06/11 14:25:41
C
C     Copyright : Practical Technology Limited  
C     File :- config.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C   This common block is used to store all the configure informatiom
C   read from the configuration file.  This can be used both for default
C   settings and to define variable settings such as the default threashold
C   point of the autocompact.
C
C   
C   NEWCMP is used as a logical to uses the onld or the new compact routine
C   PERCNT is used to define if the CMPVAL is a percentage threashold 
C          or entity threashold
C   CMPVAL is the user definable threashold used for a compact (also depends on PERCNT)
C
      LOGICAL NEWCMP,PERCNT,CHEKIT

      INTEGER*4 CMPVAL

      COMMON /LCON/ NEWCMP,PERCNT,CHEKIT

      COMMON /ICON/ CMPVAL
