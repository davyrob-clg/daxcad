C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 arith81.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION DSAME
C     FUNCTION SAME
C
C
C     |-----------------------------------------------------------------|
C
      FUNCTION DSAME(X,Y)
C     ===================
C
C1    vartype        D,D
C1    iostatus       I,I
C1  INCLUDED 'INCLUDE/wtov'
C
C2       DSAME set .true. if the difference
C2       between X & Y is smaller than the
C2       relative tolerance that is set by the
C2       size of the viewport
C
      include 'include/wtov.inc'
C
      DOUBLE PRECISION X,Y,REL,DIF,DZERO
      REAL EXT
      LOGICAL DSAME
      INTRINSIC DABS
      EXTERNAL DZERO
C
      DATA REL/1E-6/
C
      EXT=WPXMAX-WPXMIN
      DSAME=DABS(X-Y)/DBLE(EXT).LT.REL
C      DSAME=DZERO(DBLE(EXT)*REL*DABS(X-Y)).EQ.0.0
C
      END
C
C----------------------------------------------------------
C
      FUNCTION SAME(X,Y)
C     ===================
C
C1    vartype        R,R
C1    iostatus       I,I
C
C2       SAME set .true. if the difference
C2       between X & Y is smaller than the
C2       relative tolerance that is set by the
C2       size of the viewport
C
      include 'include/wtov.inc'
C
      REAL  X,Y,REL
      REAL EXT
      LOGICAL SAME
      INTRINSIC ABS
C
      DATA REL/1E-6/
C
      EXT=WPXMAX-WPXMIN
C
      SAME=ABS(X-Y)/EXT.LT.REL
C     WRITE(10,*) '[SAME]',SAME,REL,EXT,X,Y,ABS(X-Y)
C
      END
C
C----------------------------------------------------------
C
C
 
