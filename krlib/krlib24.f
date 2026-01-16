C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 krlib2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBOUND(STRING,P1,P2,*)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DBOUND(STRING,P1,P2,*)
C     =================================
C1    vartype            C*(*) I  I
C1    iostatus             I   O  O
C
C2    Subroutine DBOUND finds the bounds of the first
C2    numeric within STRING,returning P1 pointing to
C2    first char of numeric,and P2 pointing to last
C2    char of numeric.
C
      INTEGER NL,NLEN,NVRFY,P1,P2,TABLE(1:8,0:4),CH(0:15),ROW
      CHARACTER*(*) DSTR1*13,DSTR2*15,STRING
      PARAMETER (DSTR1='0123456789.+-', DSTR2='0123456789.+-ED')
      EXTERNAL NLEN,NVRFY
      INTRINSIC INDEX
      DATA CH/0, 10*1, 2, 2*3, 2*4/,
     +     TABLE/0,2*-1,3*0,-1,0, 2*2,2*3,2,3*7, 4,3,-1,0,4,0,-1,0,
     +     5,2*-1,2*0,8,-1,0, 0,2*6,3*0,-1,0/
C
      P1=0
      NL=NLEN(STRING)
      IF (NL.EQ.0) THEN
C        blank string,set P1=P2
         P1=1
         P2=P1
C        return error condition for blank
         RETURN 1
      END IF
 5    CONTINUE
C
      P1=NVRFY(STRING,DSTR1)
C
      IF (P1.GT.0) THEN
         P2=P1
         IF (NL.EQ.P1) THEN
C           only one character in string
            RETURN
         END IF
         ROW=1
   10    IF (P2.LE.NL) THEN
            ROW=TABLE(ROW,CH(INDEX(DSTR2,STRING(P2:P2))))
            IF (ROW.GT.0) THEN
               P2=P2+1
               GO TO 10
            END IF
            IF (ROW.EQ.0) RETURN 1
         ELSE IF (TABLE(ROW,0).EQ.0) THEN
            RETURN 1
         END IF
         P2=P2-1
      ELSE
         P2=0
      END IF
      END
C
C     ------------------------------------------------
