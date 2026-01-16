C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 text.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE TEPBOX(X1,X2,X3,X4,X5,X6,SCA,X,Y )
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE TEPBOX(X1,X2,X3,X4,X5,X6,SCA,X,Y )
C     ========================================================
C1                       R, R, R, R, R, R,R,R(4),R(4)
C1                       I, I, I, I, I, I,I, O ,  O
C
C
      REAL X(4),Y(4),X1,X2,X3,X4,X5,X6,TEMP,
     +    XC,YC,SLA,A(3,3),SCA,XC1,YC1
      INTEGER*2 NCHAR,JST,I
      INTRINSIC COS,SIN
      INTRINSIC MOD
      EXTERNAL TORGTL,TORGTR,TORGBR,XYNORG,SCAP2D,UCODET,NEWXY
C
      CALL UCODET(X6,SLA,JST,NCHAR)
C
      CALL  TORGTL(X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(1),Y(1))
      CALL  TORGTR(X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(2),Y(2))
      CALL  TORGBR(X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(3),Y(3))
      CALL  XYNORG(X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(4),Y(4))
C
      XC=(X(1)+X(2))/2.0
      YC=(Y(1)+Y(2))/2.0
      CALL SCAP2D(XC,YC,SCA,SCA,A)
C
      XC1=(X(3)+X(4))/2.0
      YC1=(Y(3)+Y(4))/2.0
C
      DO 20 I=3,4
         CALL NEWXY(X(I),Y(I),XC,YC,A)
         X(I)=XC
         Y(I)=YC
 20   CONTINUE
C
      CALL SCAP2D(XC1,YC1,SCA,SCA,A)
      DO 30 I=1,2
         CALL NEWXY(X(I),Y(I),XC,YC,A)
         X(I)=XC
         Y(I)=YC
 30   CONTINUE
C
C     CALL DRAWLW(X(4),Y(4),X(1),Y(1))
C     CALL DRAWLW(X(1),Y(1),X(2),Y(2))
C     CALL DRAWLW(X(2),Y(2),X(3),Y(3))
C     CALL DRAWLW(X(3),Y(3),X(4),Y(4))
C
C
      END
C
C
