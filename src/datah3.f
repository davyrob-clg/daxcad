C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah3.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBM500(P,OK)
C     SUBROUTINE DBM550(P,IARRAY,RARRAY,OK)
C     SUBROUTINE DIM500(P,OK)
C     SUBROUTINE DTM500(P,TMIP,OK)
C     SUBROUTINE DTM550(P,TMIP,STRING,OK)
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DBM500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DBM500 writes the data to position
C2    P in the part data file copying it from the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.The record must already exist,
C2    since it will be replaced with the new data.
C2    Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
      INTEGER*4 DICTN
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.NPDPOS.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBM500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBM500]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
         CALL PDRSET(P,IDBUFF,RDBUFF)

C         PDFI(1,P) = IDBUFF(1)
C         PDFI(2,P) = IDBUFF(2)
C         PDFI(3,P) = IDBUFF(3)
C         PDFI(4,P) = IDBUFF(4)
CC
C         PDFR(1,P) = RDBUFF(1)
C         PDFR(2,P) = RDBUFF(2)
C         PDFR(3,P) = RDBUFF(3)
C         PDFR(4,P) = RDBUFF(4)
C         PDFR(5,P) = RDBUFF(5)
C         PDFR(6,P) = RDBUFF(6)
CARRAY
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IDBUFF(I),I=1,4),(RDBUFF(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DBM550(P,IARRAY,RARRAY,OK)
C     =====================================
C1    vartype           I2  I2(4) R(6)  L
C1    iostatus          I    I     I   O
C
C2    Subroutine DBM550 updates the data
C2     in the part data file copying it from the
C2    RARRAY data array to the PDF record
C2    OK true if the operation was successful.
C2    The vector data currently in IARRAY is used for
C2    the write operation,and the connectivity pointer
C2    is maintained in it's current state.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      REAL RARRAY(6)
      INTEGER*2 IARRAY(4),P,I
      INTEGER*4 DICTN
      LOGICAL OK
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         DICTN=61
         WRITE(UNIT=10,FMT=*)'[DBM550] Pointer error P=',P
         CALL DEPRN3('[DBM550]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
C
CARRAY
         CALL PDRSET(P,IARRAY,RARRAY)
C         PDFI(1,P) = IARRAY(1)
C         PDFI(2,P) = IARRAY(2)
C         PDFI(3,P) = IARRAY(3)
C         PDFI(4,P) = IARRAY(4)
CC
C         PDFR(1,P) = RARRAY(1)
C         PDFR(2,P) = RARRAY(2)
C         PDFR(3,P) = RARRAY(3)
C         PDFR(4,P) = RARRAY(4)
C         PDFR(5,P) = RARRAY(5)
C         PDFR(6,P) = RARRAY(6)
CARRAY
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IARRAY(I),I=1,4),(RARRAY(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DIM500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DIM500 writes the data to position
C2    P in the master index file copying it from the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.The record must already exist,
C2    since it will be replaced with the new data.
C2    Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I,I4I2
      INTEGER*4 I4BUFF
      INTEGER*4 DICTN
      LOGICAL OK
      EXTERNAL I4I2
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.NMIPOS.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DIM500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DIM500]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
C
CARRAY
	CALL MIRSET(P,IMBUFF)

C         I4BUFF=256*IMBUFF(1)+IMBUFF(2)
C         MIFILE(1,P) =I4I2(I4BUFF)
C         I4BUFF=256*IMBUFF(3)+IMBUFF(4)
C         MIFILE(2,P) =I4I2(I4BUFF)
C         I4BUFF=256*IMBUFF(5)+IMBUFF(6)
C         MIFILE(3,P) =I4I2(I4BUFF)
CC
C         MIFILE(4,P) = IMBUFF(7)
C         MIFILE(5,P) = IMBUFF(8)
C         MIFILE(6,P) = IMBUFF(9)
C         MIFILE(7,P) = IMBUFF(10)
C         MIFILE(8,P) = IMBUFF(11)
C         MIFILE(9,P) = IMBUFF(12)
C         MIFILE(10,P) = IMBUFF(13)
CARRAY
CFILE
C
C         WRITE(MIFILU,REC=P,ERR=999)
C     +        (IMBUFF(I),I=1,13)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DTM500(P,TMIP,OK)
C     ===========================
C1    vartype           I2  I2 L
C1    iostatus          I   I  O
C
C2    subroutine DTM500 writes the text string from CBUFF
C2    to the position P in the text storage file.
C2    The record must already exist,since it will be overwritten.
C2    logical flag OK is returned true if successful.
C2    The current MIP is linked to the text entry automatically
C2    using TMIP.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*4 DICTN
      INTEGER*2 P,ZERO,TMIP
C
      LOGICAL OK
C
C     prepare flag for failure
      OK=.FALSE.
C     set integer to zero for disk file .
      ZERO=1
C     test for valid pointer position
      IF (P.GE.NTXPOS.OR.P.LT.ZERO) THEN
         WRITE(UNIT=10,FMT=*)'[DTM500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DTM500]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         CMIP(1,P)=ICBUFF(1)
         CMIP(2,P)=ICBUFF(2)
         TXFILE(P)=CBUFF
CARRAY
CFILE
C        WRITE(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),CBUFF
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DTM550(P,TMIP,STRING,OK)
C     ===================================
C1    vartype           I2 I2   C*80   L
C1    iostatus          I  I    I     O
C
C2    subroutine DTM550 writes the text string
C2    to the position P in the text storage file.
 
 
C2    The record must already exist,Logical flag OK
C2    is returned true if successful.The current MIP,in TMIP
C2    is linked to the text entry automatically.
C2    The text continuation pointer is passed back in P3.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*4 DICTN
      INTEGER*2 P,ZERO,TMIP
C
      CHARACTER*80 STRING
      LOGICAL OK
C
      ZERO=0
C     prepare flag for failure
      OK=.FALSE.
C     test for valid pointer position
      IF (P.GE.NTXPOS.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTM550] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DTM550]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         CMIP(1,P)=ICBUFF(1)
         CMIP(2,P)=ICBUFF(2)
         TXFILE(P)=STRING
CARRAY
CFILE
C        WRITE(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),STRING
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
C
C
