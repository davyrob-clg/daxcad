C
C     @(#)  412.1 date 6/11/92 redscl.inc 
C
C
C     Filename    : redscl.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:42
C     Last change : 92/06/11 14:39:58
C
C     Copyright : Practical Technology Limited  
C     File :- redscl.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     Common block to store scale factors for reading in
C     Gerber and Hp plot files.  Also the scale factor
C     for metric and imperial
C
       REAL REDSCL,AUXSCL
       COMMON /REDS/REDSCL,AUXSCL

