C
C     @(#)  412.1 date 6/11/92 rcv4.inc 
C
C
C     Filename    : rcv4.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:41
C     Last change : 92/06/11 14:39:31
C
C     Copyright : Practical Technology Limited  
C     File :- rcv4.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This is a working data area for the conversion
C2    of CADDS data into local format.
C
      INTEGER*2 ICVBUF(2048)
      REAL RCVBUF(1024)
C
      COMMON/CADDS1/RCVBUF,ICVBUF
C
