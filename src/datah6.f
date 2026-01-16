C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah6.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DER500(P,OK)
C     SUBROUTINE DER566(P,M,OK)
C     SUBROUTINE DERDIM(P,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DER500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DER500 reads the entity data from position
C2    P in the master index file and returns all data in the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P
      LOGICAL OK
C
      EXTERNAL DIR500,DBR500,DTR500
C
C     read the MI data into buffer
      CALL DIR500(P,OK)
      IF (.NOT.OK) RETURN
C     read the PDF data
      CALL DBR500(IMBUFF(7),OK)
      IF (.NOT.OK) RETURN
C     if text attached,then read it too
      IF (IMBUFF(9).NE.0) THEN
         CALL DTR500(IMBUFF(9),OK)
      END IF
C     set pointer to required data
      MIP=P
C
      END
C
C
      SUBROUTINE DER566(P,M,OK)
C     ===========================
C1    vartype         I2 R(3,3) L
C1    iostatus        I     O   O
C
C2    Subroutine DER566 reads the entity data for the component
C2    in the master index file and enters all data in
C2    RWORK and MIBUFF. P passes the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/nbuff.inc'
      include  'include/wrkdat.inc'
      include  'include/entity.inc'
C
      REAL M(3,3)
      INTEGER*2 P,P2,I
      LOGICAL OK
C
      EXTERNAL DER500,DBR550
C
C
C     READ the MI data from file to buffer
      CALL DIR500(P,OK)
C     read the component name
      P2=IMBUFF(9)
      CALL DTR500(P2,OK)
C
C     read data into transformation matrix
C     find pointer to first pdf record
      P2=IMBUFF(7)
C     now read the PD data with continuation
      CALL DBR550(P2,IWORK(1,1),RWORK(1,1),OK)
      CALL DBR550(IWORK(3,1),IWORK(1,2),RWORK(1,2),OK)
      CALL DBR550(IWORK(3,2),IWORK(1,3),RWORK(1,3),OK)
      CALL DBR550(IWORK(3,3),IWORK(1,4),RWORK(1,4),OK)
      CALL DBR550(IWORK(3,4),IWORK(1,5),RWORK(1,5),OK)
C
C     read transform matrix
      DO 20 I=1,3
         M(1,I)=RWORK(I,3)
         M(2,I)=RWORK(I,4)
         M(3,I)=RWORK(I,5)
 20   CONTINUE
C     no need to do any more,OK flag
C     will reflect success or not.
C     all data returned in IWORK,RWORK arrays 3 records
      END
**
      SUBROUTINE DERDIM(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DERDIM reads the entity data for any dimension
C2    in the master index file and enters all data in
C2    RWORK and MIBUFF. P passes the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/wrkdat.inc'
      include  'include/entity.inc'
      include  'include/dimendat.inc'
C
      INTEGER*2 P,P2,I,J,P3
      LOGICAL OK
C
      EXTERNAL DIR500,DBR500,DTR500
C
C
C     READ the MI data from file to buffer
      CALL DIR500(P,OK)
C     read the dimension text
      P2=IMBUFF(9)
C     do a test for no text
      IF(P2.EQ.0) GOTO 20
C     Set the control data for the dimension
      CTRDAT(1)=IMBUFF(5)
C     set counter to zero
      J=0
 5    CONTINUE
C     increment counter
      J=J+1
      IF(J.GT.6) GOTO 20
C     read text record
      CALL DTR550(P2,DIMCHR(J),OK)
      TICBUF(J,1)=ICBUFF(1)
      TICBUF(J,2)=ICBUFF(2)
C     check for continuation pointer to another text record
      IF ( ICBUFF(2) .NE. 0 ) THEN
         P2=ICBUFF(2)
         GOTO 5
      END IF
      NUMSTR=J
      RTALLY(2)=J
 20   CONTINUE
C     find pointer to first pdf record
      P2=IMBUFF(7)
C     set counter to zero
      I=0
 10   CONTINUE
C     increment counter
      I=I+1
C     now read the PD data with continuation
      CALL DBR550(P2,IWORK(1,I),RWORK(1,I),OK)
C     check for continuation pointer to Part data record
      IF ( IWORK(3,I) .NE. 0 ) THEN
         P2=IWORK(3,I)
         GOTO 10
      END IF
C     Update record counter for use by modify routine and draw routine.
      RECCNT(1)=I
      RECCNT(2)=I
C     no need to do any more
C
      END
 
