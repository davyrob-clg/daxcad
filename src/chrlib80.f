C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 chrlib80.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION I2I4(I2)
C     FUNCTION NLEN(CHARS)
C     FUNCTION NLEN1(CHARS)
C     FUNCTION NLEN2(CHARS)
C
C     |-----------------------------------------------------------------|
C

      FUNCTION I2I4(I2)
C     =================
C
C1    vartype     I4 I2
C1    iostatus    O  I
C
C2    Subroutine I2I4 changes 2-byte
C2    integers into a single 4-byte integer
C2    by doing a direct bit substitution,to
C2    pack the 32-bit output.
C
      INTEGER*2 I2,ITMP(1:2)
      INTEGER*4 I2I4,I4T
      EQUIVALENCE (I4T,ITMP)
C
CAPOLLO|SUN
      ITMP(2)=0
      ITMP(1)=I2
CAPOLLO|SUN
 
CIBM|PC386
C      ITMP(2)=0
C      ITMP(1)=I2
CIBM|PC386
      I2I4=I4T

C
      END
C
C-----------------------------------------------------------------
C
 
      FUNCTION NLEN(CHARS)
C     ===================
C1    vartype   I4    C
C1    iostatus  O     I
C1
C1    Function NLEN returns the correct active
C1    length of a text string contained in 'CHAR'.
C           a 0 if the string is blank
C
      INTEGER*4 NLEN,I
      CHARACTER*(*) CHARS
      INTRINSIC LEN
C
      I=LEN(CHARS)+1
  10  I=I-1
      IF (CHARS(I:I).EQ.' ') THEN
         IF ( I .GT. 1 ) GOTO 10
         I=0
      END IF
      NLEN=I
      END
C
C-----------------------------------------------------------------
C
      FUNCTION NLEN1(CHARS)
C     ===================
C1    vartype   I4    C
C1    iostatus  O     I
C1
C1    Function NLEN returns the correct active
C1    length of a text string contained in 'CHAR'.
C           a 1 if the string is blank
C
      CHARACTER*(*) CHARS
      INTEGER*4 NLEN,NLEN1
	  INTEGER*4 L
      EXTERNAL NLEN
C
		L = MAX(1,NLEN(CHARS))
	  NLEN1=MAX(1,NLEN(CHARS))

C
      END
C
C-----------------------------------------------------------------
C
      FUNCTION NLEN2(CHARS)
C     ===================
C1    vartype   I2    C
C1    iostatus  O     I
C1
C1    Function NLEN2 returns the correct active
C1    length of a text string contained in 'CHAR'.
C
      INTEGER*2 NLEN2,I
      CHARACTER*(*) CHARS
      INTRINSIC LEN
C
      I=LEN(CHARS)+1
  10  I=I-1
      IF ( CHARS(I:I).EQ.' '.AND. I .GT. 1 ) GOTO 10
      NLEN2=I
      END
C
C-----------------------------------------------------------------
C
