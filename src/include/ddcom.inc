C
C     @(#)  412.1 date 6/11/92 ddcom.inc 
C
C
C     Filename    : ddcom.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:05
C     Last change : 92/06/11 14:27:57
C
C     Copyright : Practical Technology Limited  
C     File :- ddcom.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C      This file contains the common block for the debug
C      flags that are used with daxcom on the apollo end

       LOGICAL   COMDEB     
       LOGICAL   LINDEB
C
C      COMDEB is for the comdeb ie the apolloin and apolloout files
C      LINDEB is for the link diagnostics driven for input and output
C      and allow us to visually see what is going on.
C
       COMMON /DDEBUG/COMDEB
       COMMON /LDEBUG/LINDEB
