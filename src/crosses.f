C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 crosses.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DRWCRS(XP,YP,TYPE)
C     SUBROUTINE ERSCRS(ALL)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DRWCRS(XP,YP,TYPE)
C     =============================
C1    VARTYPE           R  R   I4
C1    IOSTAT            I  I   I
C
C2    This routine will draw a cross into the work area
C2    The arguments must be REAL and WORLD coords.
C2    The crosses will be added to a scratch file opened
C2    at the beginning of the program
C2    The crosses can be erased if the display has not been touched
C2    The mode of draw will be XOR
C2    The various type will be defined later. Currently type is 1
C2    for a fairly big cross
C
      include 'include/cross.inc'
C
      REAL XP,YP,X1,Y1
      INTEGER*4 TYPE,I,TYPE2
      LOGICAL SAME
      EXTERNAL SAME
C
C     Do acheck for a cross allready in place
      DO 10 I=1,CRSPTR-1
C         Get info from file
          READ(UNIT=CRUNIT,REC=I) X1,Y1,TYPE2
          IF(SAME(X1,XP).AND.SAME(Y1,YP)) RETURN
10    CONTINUE
      IF(TYPE.EQ.1) THEN
C         cross not allrady here. go ahead
          CALL WCROSS(XP,YP)
      ENDIF
      WRITE(UNIT=CRUNIT,REC=CRSPTR) XP,YP,TYPE
      CRSPTR=CRSPTR+1
      END
 
C
      SUBROUTINE ERSCRS(ALL)
C     ======================
C1    VARTYPE            L
C1    IOSTAT             I
C
C2    This routine will reset all the crosses in the current file or
C2    just the last one!
C
      include 'include/cross.inc'
C
      LOGICAL ALL
      REAL XP,YP
      INTEGER*4 I,TYPE
C
      IF(ALL) THEN
         DO 10 I=1,CRSPTR-1
C            Get info from file
             READ(UNIT=CRUNIT,REC=I) XP,YP,TYPE
              IF(TYPE.EQ.1) THEN
C                 draw out cross
                  CALL WCROSS(XP,YP)
              ENDIF
 10       CONTINUE
C         reset all values
          CRSPTR = 1
      ELSE
          CRSPTR = CRSPTR -1
          READ(UNIT=CRUNIT,REC=CRSPTR) XP,YP,TYPE
          IF(TYPE.EQ.1) THEN
C             draw out cross
              CALL WCROSS(XP,YP)
          ENDIF
      ENDIF
      END
C
C
C
C
C
