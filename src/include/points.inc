C
C     @(#)  412.1 date 6/11/92 points.inc 
C
C
C     Filename    : points.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:33
C     Last change : 92/06/11 14:38:05
C
C     Copyright : Practical Technology Limited  
C     File :- points.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C      this file contians the max sizes of the various parts of a table
C      mlnhed- max lines in table header
C      mcols- max number of columns
C      mlncol- max lines in column header
       INTEGER*4 MLNHED,MCOLS,MLNCOL  

       COMMON/MAXSIZ/MLNHED,MCOLS,MLNCOL
