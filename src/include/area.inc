C
C     @(#)  412.1 date 6/11/92 area.inc 
C
C
C     Filename    : area.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:56
C     Last change : 92/06/11 14:23:45
C
C     Copyright : Practical Technology Limited  
C     File :- area.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C       This file contains the common blocks as used in measure area as
C       fixed and supremly modified by the 2 johns 

C       These are the max and min co-ords for the entity that we are doing
C       the measure area for for calculation of the section modulus Z
C       YXMAX = The maximum extent of the shape in the X direction 
C       YXMIN = The minimum extent of the shape in the X direction
C       YYMAX = The maximum extent of the shape in the Y direction
C       YYMIN = The minimum extent of the shape in the Y direction
          
C       These are initialise in measa3.ftn

        REAL    YXMAX,YXMIN,YYMAX,YYMIN

        COMMON /YMAX/ YXMAX,YXMIN,YYMAX,YYMIN

        
