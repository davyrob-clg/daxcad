C
C     @(#)  412.1 date 6/11/92 server.f 
C
C
C     Filename    : server.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:51:12
C     Last change : 92/06/11 14:40:44
C
C     Copyright : Practical Technology Limited  
C     File :- server.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE SERVERINIT()
C     =======================
C1    VARYPE            
C1    IOSTAT            
C
C2    Inialize server requirments
C2  
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
      include 'include/macro.inc'
      include 'include/server.inc'
      include 'include/wtov.inc'
      include   'include/viewport.inc'
      include 'include/daxcolor.inc'
C
      REAL XMIN,YMIN,XMAX,YMAX
C
C     Initalse menu sizes for command searching
C
      SERVER = .TRUE.
C
C     Noun menu
C
      GML(2,6) = 13
      GML(2,5) = 7
C
C     calculator
C
      GML(1,6) = 1
      GML(1,5) = 17
C
C     Verb menu
C
      GML(3,6) = 26
      GML(3,5) = 17
C
C     Display menu
C
      GML(4,6) = 9
      GML(4,5) = 17

C
C     set up colors to prevent any acciedents
C
      COLDRG = 2
      COLFOR = 1
      COLBAK = 0
      COLFIL = 1
C
C     Setup psedo viewport limits
C
      XMIN = 0
      YMIN = 0
      XMAX = 700
      YMAX = 700
      CALL VIEW(XMIN,YMIN,XMAX,YMAX)

C
      END
