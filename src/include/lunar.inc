C
C     @(#)  412.1 date 6/11/92 lunar.inc 
C
C
C     Filename    : lunar.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:22
C     Last change : 92/06/11 14:33:54
C
C     Copyright : Practical Technology Limited  
C     File :- lunar.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      REAL XX(100),YY(100)
      REAL XLIM,YLIM,XLOW,YLOW,XC,YC,DX,DY,XADD
      INTEGER L,N
      INTEGER FUEL,SCVAL
      INTEGER*2 SHIPX(100),SHIPY(100),SHIPDF
      INTEGER*2 CSHIPX(100),CSHIPY(100)
      LOGICAL B8,B9

      COMMON /LUNAR1/XX,YY,XLIM,YLIM,XLOW,YLOW,L,FUEL,SCVAL,
     +  DX,DY,XC,YC,B8,B9,N,XADD,SHIPX,SHIPY,CSHIPX,CSHIPY,SHIPDF
