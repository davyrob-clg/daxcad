C
C     @(#)  412.1 date 6/11/92 dig2wo.inc 
C
C
C     Filename    : dig2wo.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:07
C     Last change : 92/06/11 14:28:24
C
C     Copyright : Practical Technology Limited  
C     File :- dig2wo.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     DIGIT is a logical indicating whether the digitise 
C     transform has been set

C1    DIGWXY contains the transform from the 
C1    digitiser to world coordinates.
C1
C1    DIGSNA is the snapping resolution of the digitised input.
C1
C1    DIGANG is the alignment limit. (in mm on the digitiser)
C1
C1    WDIGS the world equivalent of DIGSNA.
C1
C1    WDIGAN world equivalent DIGANG
C1
C1    DX,DY the last digitised point.
C
      REAL DIGSNA,DIGANG,WDIGS,WDIGAN,DX,DY
      REAL WRLDX,WRLDY,DIGX,DIGY,SCADIG,ANGDIG
      REAL DIGWXY(3,3),DRES
      INTEGER*4 DIGBUT
      LOGICAL DIGIT

      COMMON /DIG/DIGWXY,DRES,DIGIT,DIGBUT,
     +            DIGSNA,DIGANG,WDIGS,WDIGAN,DX,DY,
     1            WRLDX,WRLDY,DIGX,DIGY,SCADIG,ANGDIG
