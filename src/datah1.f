C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBW500(P,OK)
C     SUBROUTINE DBW501(P,OK)
C     SUBROUTINE DBW550(IARRAY,RARRAY,P,OK)
C     SUBROUTINE DBW551(IARRAY,RARRAY,P,OK)
C     SUBROUTINE DIW500(P,OK)
C     SUBROUTINE DRW950(P,OK)
C     SUBROUTINE DRW951(P,OK)
C     SUBROUTINE DTW500(P,OK)
C     SUBROUTINE DTW550(STRING,P,OK)
C     SUBROUTINE DTW551(STRING,P,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DBW500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DBW500 writes the data to the next position
C2     in the part data file copying it from the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    The next entry pointer to the Pd file is
C2    also updated before return.P returns the
C2    position used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I
      INTEGER*4 DICTN
      LOGICAL OK
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     load P with next available position
      P=NPDPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBW500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBW500]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C         pointer in range alright
          IDBUFF(3)=0
C
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
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IDBUFF(I),I=1,4),(RDBUFF(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
C        update next entry position
         NPDPOS=NPDPOS+1
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DBW501(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DBW500 writes the data to the next position
C2     in the part data file copying it from the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    The next entry pointer to the Pd file is
C2    also updated before return.P returns the
C2    position used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I
      INTEGER*4 DICTN
      LOGICAL OK
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     load P with next available position
      P=NPDPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBW501] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBW501]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
         IDBUFF(3)=P+1
C
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
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IDBUFF(I),I=1,4),(RDBUFF(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
C        update next entry position
         NPDPOS=NPDPOS+1
      END IF
C
 999  CONTINUE
C
      END

      SUBROUTINE DBW502(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    DBW502 Writes a new part data record that assumes
C2    The continuation pointer has been set. This is used
C2    by the component/part/symbol code where exisitng
C2    records have been calculated and an offset simply is applied
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I
      INTEGER*4 DICTN
      LOGICAL OK
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     load P with next available position
      P=NPDPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBW500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBW500]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C
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
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IDBUFF(I),I=1,4),(RDBUFF(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
C        update next entry position
         NPDPOS=NPDPOS+1
      END IF
C
 999  CONTINUE
C
      END

*
      SUBROUTINE DBW550(IARRAY,RARRAY,P,OK)
C     =====================================
C1    vartype             I2(4) R(6) I2 L
C1    iostatus              I     I  O  O
C
C2    Subroutine DBW550 writes the data to the next position
C2     in the part data file copying it from the
C2    RARRAY data array to the PDF record
C2    OK true if the operation was successful.
C2    The vector data currently in IARRAY is used for
C2    the write operation,and the connectivity pointer
C2    is cleared.
C2    The next entry pointer to the Pd file is
C2    also updated before return.P returns the
C2    position used.
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
C     load P with next available position
      P=NPDPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBW550] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBW550]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
C        ensure no connection vector used
         IARRAY(3)=0
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
C        update next entry position
         NPDPOS=NPDPOS+1
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DBW551(IARRAY,RARRAY,P,OK)
C     =====================================
C1    vartype             I2(4) R(6)  I2 L
C1    iostatus              I    I    O  O
C
C2    Subroutine DBW551 writes the data to the next position
C2     in the part data file copying it from the
C2    RARRAY data array to the PDF record
C2    OK true if the operation was successful.
C2    The vector data currently in IARRAY is used for
C2    the write operation,and the connectivity pointer
C2    is set to the next available record after the current one.
C2    The next entry pointer to the Pd file is
C2    also updated before return.P returns the
C2    position used.
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
C     load P with next available position
      P=NPDPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.PDPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DBW551] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBW551]',DICTN)
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
C        ensure correct connection vector used
         IARRAY(3)=P+1
C
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
CFILE
C
C         WRITE(PDFILU,REC=P,ERR=999)
C     +        (IARRAY(I),I=1,4),(RARRAY(I),I=1,6)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
C        update next entry position
         NPDPOS=NPDPOS+1
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DIW500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DIW500 writes the data to the next free
C2    position in the master index file copying it from the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.P returns the
C2    position used
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
C     load P with next available position
      P=NMIPOS
C     test for valid part data file pointer
C     cannot be greater than max pointer length
      IF (P.GE.MIPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DIW500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DIW500]',DICTN)
C        pointer out of range!
            RETURN
      ELSE
C        pointer in range alright
C
CARRAY

	CALL MIRSET(P,IMBUFF)
C
C         I4BUFF=256*IMBUFF(1)+IMBUFF(2)
C         MIFILE(1,P) =I4I2(I4BUFF)
C         I4BUFF=256*IMBUFF(3)+IMBUFF(4)
C         MIFILE(2,P) =I4I2(I4BUFF)
C         I4BUFF=256*IMBUFF(5)+IMBUFF(6)
C         MIFILE(3,P) =I4I2(I4BUFF)
C         MIFILE(4,P) = IMBUFF(7)
C         MIFILE(5,P) = IMBUFF(8)
C         MIFILE(6,P) = IMBUFF(9)
C         MIFILE(7,P) = IMBUFF(10)
C         MIFILE(8,P) = IMBUFF(11)
C         MIFILE(9,P) = IMBUFF(12)
C         MIFILE(10,P)= IMBUFF(13)
CARRAY
CFILE
C         WRITE(MIFILU,REC=P,ERR=999)
C     +        (IMBUFF(I),I=1,13)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
C        update next available MI entry
         NMIPOS=NMIPOS+1
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DRW950(P,OK)
C     =======================
C
C1    VARTYPE           I2 L
C1    IOSTATUS          O  O
C
C2    subroutine DRW950 writes a relation record
C2    to the relation file.
C2    P returns the position filled.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P
      INTEGER*4 I,DICTN
      LOGICAL OK
C
C
      IF (NRLPOS.GT.RLPLIM .OR. NRLPOS.LE.0) THEN
C        NO SPACE LEFT OR CORRUPTION IN POINTER
         OK=.FALSE.
         WRITE(UNIT=10,FMT=*)'[DRW550] POINTER OUT RANGE ',NRLPOS
         DICTN=61
         CALL DEPRNT(DICTN)
      ELSE
C        SET CONTINUATION POINTER TO NULL
         RLBUFF(2)=0
C        WRITE THE RECORD TO RELATION FILE
CARRAY
         DO 10 I=1,10
            RLFILE(I,NRLPOS)=RLBUFF(I)
 10      CONTINUE
CARRAY
C
CFILE
C         WRITE(RLFILU,REC=NRLPOS,ERR=99) (RLBUFF(I),I=1,10)
CFILE
C
C        RETURN POINTER USED
         P=NRLPOS
C        UPDATE NEXT RELATION POINTER
         NRLPOS=NRLPOS+1
         OK=.TRUE.
  99     CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE DRW951(P,OK)
C     =======================
C
C1    VARTYPE           I2 L
C1    IOSTATUS          O  O
C
C2    subroutine DRW951 writes a relation record
C2    to the relation file and ensures that a
C2    continuation pointer is included to point
C2    to the next relation record to be filled.
C2    P returns the position filled.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P
      INTEGER*4 I,DICTN
      LOGICAL OK
      EXTERNAL DEPRNT
C
      IF (NRLPOS.GT.RLPLIM .OR. NRLPOS.LE.0) THEN
C        no space left or corruption in pointer
         OK=.FALSE.
         DICTN=61
         CALL DEPRNT(DICTN)
         WRITE(UNIT=10,FMT=*)'[DRW551] POINTER OUT OF RANGE ',NRLPOS
      ELSE
C        set continuation pointer
         RLBUFF(2)=NRLPOS+1
C        write the record to relation file
CARRAY
         DO 10 I=1,10
            RLFILE(I,NRLPOS)=RLBUFF(I)
 10      CONTINUE
CARRAY
C
CFILE
C         WRITE(RLFILU,REC=NRLPOS,ERR=99) (RLBUFF(I),I=1,10)
CFILE
C        RETURN POINTER USED
         P=NRLPOS
C        UPDATE NEXT RELATION POINTER
         NRLPOS=NRLPOS+1
         OK=.TRUE.
  99     CONTINUE
      END IF
C
      END
C
      SUBROUTINE DTW500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    subroutine DTW500 writes the text string from
C2    to the next free position in the text storage file.
C2    logical flag OK is returned true if successful.
C2    The current MIP is linked to the text entry automatically.
C2    P returns the position in the text file used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,ZERO
      INTEGER*4 DICTN
C
      LOGICAL OK
C
C     prepare flag for failure
      OK=.FALSE.
C     Set integer to zero for write to disk file.
      ZERO=0
      P=NTXPOS
C     test for valid pointer position
      IF (P.GE.TXPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTW500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DTW500]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
C
         ICBUFF(1)=MIP
         ICBUFF(2)=ZERO
CARRAY
         CMIP(1,P)=ICBUFF(1)
         CMIP(2,P)=ICBUFF(2)
         TXFILE(P)=CBUFF
CARRAY
CFILE
C        WRITE(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),CBUFF
CFILE
C        update next available TX entry
         NTXPOS=NTXPOS+1
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DTW550(STRING,P,OK)
C     ==============================
C1    vartype           C*80   I2 L
C1    iostatus           I     O  O
C
C2    subroutine DTW550 writes the text string from input string
C2    to the next free position in the text storage file, and sets
C2    the continuation pointer to ZERO.
C2    logical flag OK is returned true if successful.
C2    The current MIP is linked to the text entry automatically.
C2    P returns the position in the text file used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,ZERO
C
      LOGICAL OK
      INTEGER*4 DICTN
C
      CHARACTER*80 STRING
C
C
C     prepare flag for failure
      OK=.FALSE.
C     Set integer to zero for write to disk file.
      ZERO=0
      P=NTXPOS
C     test for valid pointer position
      IF (P.GE.TXPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTW550] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DTW550]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
C
         ICBUFF(1)=MIP
         ICBUFF(2)=ZERO
CARRAY
         CMIP(1,P)=ICBUFF(1)
         CMIP(2,P)=ICBUFF(2)
         TXFILE(P)=STRING
CARRAY
CFILE
C        WRITE(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),STRING
CFILE
C        update next available TX entry
         NTXPOS=NTXPOS+1
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DTW551(STRING,P,OK)
C     ==============================
C1    vartype           C*80   I2 L
C1    iostatus           I     O  O
C
C2    subroutine DTW551 writes the text string from input string
C2    to the next free position in the text storage file, and sets
C2    the continuation pointer to next text position.
C2    logical flag OK is returned true if successful.
C2    The current MIP is linked to the text entry automatically.
C2    P returns the position in the text file used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P
C
      INTEGER*4 DICTN
      LOGICAL OK
C
      CHARACTER*80 STRING
C
C
C     prepare flag for failure
      OK=.FALSE.
      P=NTXPOS
C     test for valid pointer position
      IF (P.GE.TXPLIM.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTW551] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DTW551]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
C        update next available TX entry
         NTXPOS=NTXPOS+1
         ICBUFF(1)=MIP
         ICBUFF(2)=NTXPOS
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
