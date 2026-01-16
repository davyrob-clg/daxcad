C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 error.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ASKINT(DNUM,NULLOK,NULVAL,INVAL)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE ASKINT(DNUM,NULLOK,NULVAL,INVAL)
C     ===========================================
C
C1    vartype            I4    L      I4     I4
C1    iostatus           I     I      I      O
C
C2    Subroutine ASKINT propmts the user for typed input
C2    and verifies his input as a valid integer.The prompt
C2    dictionary number DNUM identifies the propmt to be used,
C2    and the logical NULLOK indicates that a null response is
C2    acceptable.The integer NULVAL contains the value to return
C2    in the event of a null response,and INVAL contains the
C2    returned integer value in any case.
C
      INTEGER*4 DNUM,NULVAL,INVAL,NLEN
      LOGICAL NULLOK,OK
      CHARACTER*80 STRING
C
      EXTERNAL DPRMXP,VRFINT,DEPRNT,NLEN
C
 10   CONTINUE
C     prompt for integer value
      CALL DPRMXP(DNUM,STRING)
      IF (NLEN(STRING).GT.0) THEN
C        find integer value from string
         CALL VRFINT(STRING,INVAL,OK)
         IF (.NOT.OK) THEN
C           not a valid integer
C           "Please supply an integer number only"
            CALL DEPRNT(286)
            GOTO 10
         END IF
C        have valid integer
      ELSE
C        null response
         IF (NULLOK) THEN
C           return default null value
            INVAL=NULVAL
         ELSE
C           null return not allowed,must ask again
            GOTO 10
         END IF
      END IF
C
      END
C
C
 
