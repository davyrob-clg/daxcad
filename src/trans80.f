C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 trans80.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE I3M(A)
C     SUBROUTINE MULT3M( A , B , C )
C     SUBROUTINE NEWXY(OX,OY,NX,NY,A)
C     SUBROUTINE ROT2D(THETA,A)
C     SUBROUTINE SCAL2D(SX,SY,A)
C     SUBROUTINE TRAN2D(TX,TY,A)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE I3M(A)
C     =================
C1                 R(3*3)
C1                   O
C2      Subroutine I3M set matrice A(3*3) to the
C2      identity matrice I(3*3)
C
      REAL A(3,3),I(3,3)
      INTEGER K,J
C
C       Set I to a 3 by 3 indentity matrice
      DATA I/1.0,0.0,0.0,
     A       0.0,1.0,0.0,
     B       0.0,0.0,1.0/
C
      DO 10 J=1,3
         DO 10 K=1,3
            A(J,K)=I(J,K)
 10   CONTINUE
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE MULT3M( A , B , C )
C     ==============================
C1                        3*R(3*3)
C1                       I , I , O
C2       Subroutine MULT3M multiplies matrices A and B
C2       and returns the values in matrice C
C
      REAL A(3,3),B(3,3),C(3,3)
      INTEGER I,J,K
 
c      WRITE(*,*) 'mult3m'
c      WRITE(*,*) ((A(I,J),J=1,3),I=1,3)
c      write(*,*) '-'
c      WRITE(*,*) ((B(I,J),J=1,3),I=1,3)
      DO 1 I=1,3
         DO 2 J=1,3
            C(I,J)=0.0
            DO 3 K=1,3
               C(I,J)=C(I,J)+A(I,K)*B(K,J)
 3          CONTINUE
 2       CONTINUE
 1    CONTINUE
c      write(*,*) '-'
c      WRITE(*,*) ((C(I,J),J=1,3),I=1,3)
c      write(*,*) '======='
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE NEWXY(OX,OY,NX,NY,A)
C     ================================
C1                      R, R, R, R,R(3*3)
C1                      I, I, O, O,I
C
C2      Subroutine NEWXY multiples OX by the transformation
C2      matrice A to obtain the transformed value NX
C2      the same also with OY to NY
C
      REAL OX,OY,NX,NY,A(3,3)
C
      NX=OX*A(1,1)+OY*A(2,1)+A(3,1)
      NY=OX*A(1,2)+OY*A(2,2)+A(3,2)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE ROT2D(THETA,A)
C     =========================
C1                       R ,R(3*3)
C1                       I , O
C2       Subroutine ROT2D rotates a point about the origin
C2       by the theta (radians) returning the transformation
C2       matrice A
C
      REAL A(3,3),THETA
      INTRINSIC SIN,COS
      EXTERNAL I3M
C       set to identity matrice
      CALL I3M(A)
C       set the rotation sin,cos
      A(1,1)=COS(THETA)
      A(2,2)=A(1,1)
      A(1,2)=SIN(THETA)
      A(2,1)=-A(1,2)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE SCAL2D(SX,SY,A)
C     ==========================
C1                       R, R,R(3*3)
C1                       I, I,O
C
C2       Subroutine SCAL2D scales a point about the origin
C2       by the ratios SX in the X-axis and SY in the Y
C
      REAL A(3,3),SX,SY
      EXTERNAL I3M
C       set to identity matrice
      CALL I3M(A)
C        enter the scale factors
      A(1,1)=SX
      A(2,2)=SY
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE TRAN2D(TX,TY,A)
C     ==========================
C1                       R, R,R(3,3)
C1                       I, I,O
C
C2       Subroutine TRAN2D sets array A to translate a
C2       point by the values of TX,TY
C
      REAL A(3,3),TX,TY
      EXTERNAL I3M
C
C       set to indentity matrice
      CALL I3M(A)
C        set translation values
      A(3,1)=TX
      A(3,2)=TY
      END
C
C-----------------------------------------------------------------
C
