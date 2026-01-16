C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBR500(P,OK)
C     SUBROUTINE DBR550(P,IARRAY,RARRAY,OK)
C     SUBROUTINE DIR500(P,OK)
C     SUBROUTINE DIR520(P,OK)
C     SUBROUTINE DRR950(P,OK)
C     SUBROUTINE DTR500(P,OK)
C     SUBROUTINE DTR550(P,STRING,OK)
C     SUBROUTINE RDISPF(DFP1,ETYP,MIPP,OK)
C     SUBROUTINE RDISPF2(DFP1,ETYP,MIPP,OK)
C     SUBROUTINE RSTLY0(OP,OK)
C     SUBROUTINE RSTMI0(P,OK)
C     SUBROUTINE RSTPC0(P,OK)
C     SUBROUTINE RSTPD0(P,OK)
C     SUBROUTINE RSTPI0(P,OK)
C     SUBROUTINE RSTPR1(IOFF,POFF,PCOFF,OK)
C     SUBROUTINE RSTPR2(IOFF,PCOFF,OK)
C     SUBROUTINE RSTRL0(P,OK)
C     SUBROUTINE RSTRL1(IOFF,POFF,TOFF,ROFF,OK)
C     SUBROUTINE RSTTX0(P,OK)
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DBR500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DBR500 reads the data from position
C2    P in the part data file and returns it in the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
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
C     test for valid part data file pointer
      OK = P.LT.NPDPOS .AND. P.GT.0
      IF(.NOT.OK) THEN
C        pointer out of range!
         WRITE(UNIT=10,FMT=*)'[DBR500] Pointer error P=',P
         DICTN=61
         CALL DEPRN3('[DBR500]',DICTN)
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         CALL PDRGET(P,IDBUFF,RDBUFF)
CARRAY
CFILE
C         READ(PDFILU,REC=P,ERR=999)
C     +       (IDBUFF(I),I=1,4),(RDBUFF(I),I=1,6)
CC        must have been successful since we got this far
CFILE
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DBR550(P,IARRAY,RARRAY,OK)
C     =====================================
C1    vartype           I2  I2(4) R(6)  L
C1    iostatus          I    O    O     O
C
C2    Subroutine DBR550 reads the data from position
C2    P in the part data file and returns it in the
C2    array RARRAY logical flag ok returned true
C2    if the operation was successful.
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
      IF (P.GE.NPDPOS .OR. P.LE.0) THEN
C        pointer out of range!
         DICTN=61
         CALL DEPRN3('[DBR550]',DICTN)
         WRITE(UNIT=10,FMT=*)'[DBR550] Pointer error P=',P
         RETURN
      ELSE
C        pointer in range alright
         CALL PDRGET(P,IARRAY,RARRAY)
C         IARRAY(1)= PDFI(1,P)
C         IARRAY(2)= PDFI(2,P)
C         IARRAY(3)= PDFI(3,P)
C         IARRAY(4)= PDFI(4,P)
CC
C         RARRAY(1) = PDFR(1,P)
C         RARRAY(2) = PDFR(2,P)
C         RARRAY(3) = PDFR(3,P)
C         RARRAY(4) = PDFR(4,P)
C         RARRAY(5) = PDFR(5,P)
C         RARRAY(6) = PDFR(6,P)
CFILE
C
C         READ(PDFILU,REC=P,ERR=999)
C     +       (IARRAY(I),I=1,4),(RARRAY(I),I=1,6)
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
      SUBROUTINE DIR500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus           I  O
C
C2    Subroutine DIR500 reads the data from position
C2    P in the master index file and returns it in the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,IT
      INTEGER*4 I4BUFF,I2I4,MOD
      INTEGER*4 DICTN
      LOGICAL OK
      INTRINSIC MOD
      EXTERNAL I2I4
C
C     test for valid part data file pointer
      OK = P.LT.NMIPOS.OR.P.GT.0
      IF(.NOT.OK) THEN
         DICTN=61
         CALL DEPRN3('[DIR500]',DICTN)
         WRITE(UNIT=10,FMT=*)'[DIR500] Pointer error P=',P,NMIPOS
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
C        update current MIP
         MIP=P
C        type of entity
         CALL MIRGET(P,IMBUFF)
CARRAY
CFILE
C
C         READ(MIFILU,REC=P,ERR=999)
C     +       (IMBUFF(I),I=1,13)
CC        must have been successful since we got this far
CFILE
      END IF
C
 999  CONTINUE
C
      END
      SUBROUTINE DIR520(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus           I  O
C
C2    Subroutine DIR500 reads the data from position
C2    P in the master index file and returns it in the
C2    part data buffer contained in common block
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,IT
      INTEGER*4 I4BUFF,I2I4,MOD
      INTEGER*4 DICTN
      LOGICAL OK
      INTRINSIC MOD
      EXTERNAL I2I4
C
C     prepare flag in case of failure
      OK=.FALSE.
C
C     test for valid part data file pointer
      IF (P.GE.NMIPOS.OR.P.LT.1) THEN
         DICTN=61
         CALL DEPRN3('[DIR500]',DICTN)
         WRITE(UNIT=10,FMT=*)'[DIR500] Pointer error P=',P,NMIPOS
C        pointer out of range!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
C
C        update current MIP
         MIP=P
C        type of entity
c         CALL SPLIT_I2(MIFILE(1,P),IMBUFF(1),IMBUFF(2))
c         CALL SPLIT_I2(MIFILE(2,P),IMBUFF(3),IMBUFF(4))
c         CALL SPLIT_I2(MIFILE(3,P),IMBUFF(5),IMBUFF(6))
C         I4BUFF=I2I4(MIFILE(1,P))
C         I4BUFF=MOD(I4BUFF,65536)
C         IMBUFF(2)= MOD(I4BUFF,256)
C         I4BUFF   = I4BUFF-IMBUFF(2)
C        status of entity
C         IMBUFF(1)= I4BUFF/256
C
C         I4BUFF=I2I4(MIFILE(2,P))
C         I4BUFF=MOD(I4BUFF,65536)
C        layer on which entity is placed
C         IMBUFF(4)= MOD(I4BUFF,256)
C         I4BUFF   = I4BUFF-IMBUFF(4)
C        colour of entity
C         IMBUFF(3)= I4BUFF/256
C
C         I4BUFF=I2I4(MIFILE(3,P))
C         I4BUFF=MOD(I4BUFF,65536)
C        font of entity
C         IMBUFF(6)= MOD(I4BUFF,256)
C         I4BUFF   = I4BUFF-IMBUFF(6)
C        spare position
C         IMBUFF(5)= I4BUFF/256
C
C        PDF pointer
c         IMBUFF(7)= MIFILE(4,P)
C        SPARE pointer
c         IMBUFF(8)= MIFILE(5,P)
C        TEXT file pointer
c         IMBUFF(9)= MIFILE(6,P)
C        SPARE pointer
c         IMBUFF(10)= MIFILE(7,P)
C        SPARE pointer
c         IMBUFF(11)= MIFILE(8,P)
C        SPARE pointer
c         IMBUFF(12)= MIFILE(9,P)
C        SPARE pointer
c         IMBUFF(13)= MIFILE(10,P)
CARRAY
CFILE
C
C         READ(MIFILU,REC=P,ERR=999)
C     +       (IMBUFF(I),I=1,13)
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
 
 
 
      SUBROUTINE DRR950(P,OK)
C     =======================
C
C1    VARTYPE           I2 L
C1    IOSTATUS          O  O
C
C2    subroutine drr950 reads a relation record
C2    to the relation buffer
C2    P passes the position to be read.
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
      IF (P.GE.NRLPOS .OR. P.LE.0) THEN
C        corruption in pointer.
         OK=.FALSE.
         WRITE(UNIT=10,FMT=*)'[DRR950] POINTER OUT OF RANGE P=',P
         DICTN=61
         CALL DEPRNT(DICTN)
      ELSE
C        READ THE RECORD FROM RELATION FILE
CARRAY
         DO 10 I=1,10
           RLBUFF(I)=RLFILE(I,P)
 10      CONTINUE
CARRAY
CFILE
C         READ(RLFILU,REC=P,ERR=99) (RLBUFF(I),I=1,10)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE DTR500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    subroutine DTR500 reads the text string from
C2    the text storage file,pointed to by P,and returns
C2    the string in the string buffer in common block
C2    logical flag OK is returned true if successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P
      INTEGER*4 DICTN
C
      LOGICAL OK
C
C     prepare flag for failure
      OK=.FALSE.
C     test for valid pointer position
      IF (P.GE.NTXPOS.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTR500] Pointer error P=',P,NTXPOS
         DICTN=61
         CALL DEPRN3('[DTR500]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         CBUFF=TXFILE(P)
         ICBUFF(1)=CMIP(1,P)
         ICBUFF(2)=CMIP(2,P)
CARRAY
CFILE
C         READ(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),CBUFF
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE DTR550(P,STRING,OK)
C     ================================
C1    vartype           I2 C*80   L
C1    iostatus          I  O      O
C
C2    subroutine DTR550 reads the text string from
C2    the text storage file,pointed to by P,and returns
C2    the string in STRING.
C2    logical flag OK is returned true if successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P
      INTEGER*4 DICTN
C
      LOGICAL OK
C
      CHARACTER*80 STRING
C
C
C     prepare flag for failure
      OK=.FALSE.
C     test for valid pointer position
      IF (P.GE.NTXPOS.OR.P.LT.1) THEN
         WRITE(UNIT=10,FMT=*)'[DTR550] Pointer error P=',P,NTXPOS
         DICTN=61
         CALL DEPRN3('[DTR550]',DICTN)
C        pointer out of range!!!
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         STRING=TXFILE(P)
         ICBUFF(1)=CMIP(1,P)
         ICBUFF(2)=CMIP(2,P)
CARRAY
CFILE
C         READ(TXFILU,REC=P,ERR=999) ICBUFF(1),ICBUFF(2),STRING
CC        must have been successful since we got this far
CFILE
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
*
      SUBROUTINE RDISPF(DFP1,ETYP,MIPP,OK)
C     ===================================
C
C1    vartype           I2   I2   I2  L
C1    iostatus          I    O    O   O
C
C2    Subroutine RDISPF reads the data from
C2    position DFP in the display file,and returns
C2    the entity type in ETYP and the MI pointer
C2    in MIPP. OK returned true if entry DFP is valid
C2    Ok returned false if end of file encountered.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 DFP1,ETYP,MIPP
      LOGICAL OK
C
      OK=.FALSE.
C     test for valid pointer first
CAPOLLO
C      WRITE(10,*) '[RDISPF] ldfile=',LDFILE(CVPN)
CAPOLLO
      IF (DFP1.GT.0) THEN
C        pointer valid,read data
CARRAY
         ETYP=DFILE(1,DFP1,CVPN)
         MIPP=DFILE(2,DFP1,CVPN)
CARRAY
C
CFILE
C         READ(DDFILU(DFPNT),REC=DFP1,ERR=999) ETYP,MIPP
CFILE
         OK=.TRUE.
 999     CONTINUE
      END IF
C
      END
C
      SUBROUTINE RDISPF2(DFP1,ETYP,MIPP,OK)
C     ===================================
C
C1    vartype           I2   I2   I2  L
C1    iostatus          I    O    O   O
C
C2    Subroutine RDISPF reads the data from
C2    position DFP in the display file,and returns
C2    the entity type in ETYP and the MI pointer
C2    in MIPP. OK returned true if entry DFP is valid
C2    Ok returned false if end of file encountered.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 DFP1,ETYP,MIPP
      LOGICAL OK
C
      OK=.FALSE.
C     test for valid pointer first
      IF (DFP1.GT.0.AND.DFP1.LT.LDFILE(CVPN)) THEN
C        pointer valid,read data
CARRAY
         ETYP=DFILE(1,DFP1,CVPN)
         MIPP=DFILE(2,DFP1,CVPN)
CARRAY
C
CFILE
C         READ(DDFILU(DFPNT),REC=DFP1,ERR=999) ETYP,MIPP
CFILE
         OK=.TRUE.
 999     CONTINUE
      END IF
C
      END
C
      SUBROUTINE RSTLY0(OP,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTLY0 writes a layer No and Name
C2    to the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
C
      INTEGER*2 P,I,OP
      LOGICAL OK
      OK=.FALSE.
 
C     pointer in range alright
      READ(UNIT=PARFUN,ERR=99)   P,LNAME(P)
      OK=.TRUE.
 99   CONTINUE
C
      END
*
      SUBROUTINE RSTMI0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTMI0 saves a record of the
C2    MI file in the part storage file.The data
C2    is read from the part storage file and written
C2    to the MI work file.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/lfont.inc'
C
      INTEGER*2 P,IT,I
      INTEGER*4 I4BUFF,I2I4,MOD
      LOGICAL OK
      CHARACTER*30 LINE
      INTRINSIC MOD
C
      EXTERNAL I2I4,EPRINT
CFILE
      INTEGER*2 TMBUFF(10)
      EXTERNAL DIM500
CFILE
C
      OK=.FALSE.
C     go read the data from the part file
      IF (P.GE.MIPLIM) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
C         READ(UNIT=PARFUN,ERR=99) (MIFILE(I,P),I=1,10)
CARRAY
CFILE
           READ(PARFUN,ERR=99) (TMBUFF(I),I=1,10)
C		   print*, (TMBUFF(I),I=1,10)
C           now decode into 13 element array
C           Do status
C        type of entity
         I4BUFF=I2I4(TMBUFF(1))
         I4BUFF=MOD(I4BUFF,65536)
         IMBUFF(2)= MOD(I4BUFF,256)
         I4BUFF   = I4BUFF-IMBUFF(2)
C        status of entity
         IMBUFF(1)= I4BUFF/256

         I4BUFF=I2I4(TMBUFF(2))
         I4BUFF=MOD(I4BUFF,65536)
C        layer on which entity is placed
         IMBUFF(4)= MOD(I4BUFF,256)
         I4BUFF   = I4BUFF-IMBUFF(4)
C        colour of entity
         IMBUFF(3)= I4BUFF/256
C
         I4BUFF=I2I4(TMBUFF(3))
         I4BUFF=MOD(I4BUFF,65536)
C        font of entity
         IMBUFF(6)= MOD(I4BUFF,256)
         I4BUFF   = I4BUFF-IMBUFF(6)
C        spare position
         IMBUFF(5)= I4BUFF/256

C
C		DPS - The Daxcad Preservation Society
C		This needs to be added for drawings that are on INTEL
C

           IMBUFF(1)=TMBUFF(1)/256
           IT=TMBUFF(1)
c           type of entity
           IMBUFF(2)=MOD(IT,256)
c           color
           IMBUFF(3)=TMBUFF(2)/256
           IT=TMBUFF(2)
c           Layer of entity
           IMBUFF(4)=MOD(IT,256)
c           Spare position
           IMBUFF(5)=TMBUFF(3)/256
           IT=TMBUFF(3)
c           entity font
           IMBUFF(6)=MOD(IT,256)
c           now do rest
           IMBUFF(7)=TMBUFF(4)
           IMBUFF(8)=TMBUFF(5)
           IMBUFF(9)=TMBUFF(6)
           IMBUFF(10)=TMBUFF(7)
           IMBUFF(11)=TMBUFF(8)
           IMBUFF(12)=TMBUFF(9)
           IMBUFF(13)=TMBUFF(10)

           CALL DIM500(P,OK)
CFILE
CARRAY
C         IF ( MIFILE(1,P)/256.GE.100 ) DELENT=DELENT+1
CC
C         I4BUFF=I2I4(MIFILE(3,P))
C         I4BUFF=MOD(I4BUFF,65536)
CC        font of entity
C         IMBUFF(6)= MOD(I4BUFF,256)
C         IF ( IMBUFF(6).GT.NFONTS ) THEN
C            WRITE(LINE,FMT='(A,I4)')
C     +       'No definition for Font:',IMBUFF(6)
C            CALL EPRINT(LINE)
C         END IF
CCARRAY
CFILE
        IF ( IMBUFF(1).GE.100 ) DELENT=DELENT+1
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE RSTPC0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTPC0 retrieves a record of the
C2    PC file in the part storage file.The data
C2    is read from the part storage file and written
C2    to the PC work file
C
      include  'include/filunit.inc'
      include  'include/props.inc'
      include  'include/masti.inc'
C
      INTEGER*2 P,I,PC1,PC2
      INTEGER*4 N
      CHARACTER*80 PPTEXT
      LOGICAL OK
C
      OK=.FALSE.
C     go read the data from the part file
      IF (P.GE.PRCLIM) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
C        clear text record
         PRCFIL(P)='                                 '
         READ(UNIT=PARFUN,ERR=99) (PRCFII(I,P),I=1,2),
     +             N,PRCFIL(P)(1:N)
C      WRITE(UNIT=10,FMT=*)'[RSTPC0] p=',P
CARRAY
CFILE
C         PPTEXT='                                 '
C         READ(UNIT=PARFUN,ERR=99) PC1,PC2,
C     +             N,PPTEXT(1:N)
C      WRITE(PRCFLU,REC=P,ERR=99) PC1,PC2,PPTEXT
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE RSTPD0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTPD0 reads a PDF record
C2    from the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
C
      OK=.FALSE.
      IF (P.GE.PDPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
         READ(PARFUN,ERR=99,END=99) (IDBUFF(I),I=1,4),
     +                       (RDBUFF(I),I=1,6)
         CALL DBM500(P,OK)
         IF ( .NOT.OK ) THEN
              RETURN
         ENDIF
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE RSTPI0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTPI0 retrieves a record of the
C2    PI file in the part storage file.The data
C2    is read from the part storage file and written
C2    to the PI work file
C
      include  'include/filunit.inc'
      include  'include/props.inc'
      include  'include/masti.inc'
C
      INTEGER*2 P,I,PRIB(8)
      LOGICAL OK
C
      OK=.FALSE.
C     go read the data from the part file
      IF (P.GE.PRILIM) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
         READ(UNIT=PARFUN,ERR=99) (PRIFIL(I,P),I=1,8)
C      WRITE(UNIT=10,FMT=*)'[RSTPI0] p=',P
CARRAY
C
CFILE
C         READ(PARFUN,ERR=99) (PRIB(I),I=1,8)
C         WRITE(PRIFLU,REC=P,ERR=99) (PRIB(I),I=1,8)
CFILE
C
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE RSTPR1(IOFF,POFF,PCOFF,OK)
C     =======================================
C
C1    VARTYPE           I*2  I*2  I*2   L
C1    IOSTATUS           I    I   I     O
C
C2    SUBROUTINE RSTPR1 RETRIEVES A RECORD OF THE
C2    PROPERTIES MI FILE IN THE PART STORAGE FILE.THE DATA
C2    IS READ FROM THE PART FILE AND WRITTEN
C2    TO THE INTERNAL WORKFILE,THE MI POINTER IS INCREASED
C2    BY THE AMOUNT IOFF AND THE PROPERTIES TEXT POINTER BY PCOFF.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,J,TPRFIL(8),IOFF,POFF,PCOFF
      LOGICAL OK
C
      OK=.FALSE.
C
      READ(UNIT=PARFUN,ERR=99,END=98) (TPRFIL(I),I=1,8)
C        offset the parent mi pointer
         IF (TPRFIL(3).GT.0) TPRFIL(3)=TPRFIL(3)+IOFF
C        offset the pr continuation pointer
         IF (TPRFIL(4).GT.0) TPRFIL(4)=TPRFIL(4)+POFF
         IF (TPRFIL(1).EQ.4) THEN
C           if property list,offset all property control pointers
            DO 20 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)+POFF
 20         CONTINUE
         ELSE IF (TPRFIL(1).EQ.2) THEN
C           if property link,offset name pointer
            IF (TPRFIL(5).GT.0) TPRFIL(5)=TPRFIL(5)+PCOFF
         ELSE IF (TPRFIL(1).EQ.3) THEN
C           if property set,offset name pointer
            IF (TPRFIL(5).GT.0) TPRFIL(5)=TPRFIL(5)+PCOFF
         ELSE IF (TPRFIL(1).EQ.5) THEN
C           if attached property,offset data pointers
            DO 21 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)+PCOFF
 21         CONTINUE
         ELSE IF (TPRFIL(1).EQ.1) THEN
C           if master property,offset data pointers
            DO 22 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)+PCOFF
 22         CONTINUE
         END IF
C
CARRAY
C        write data into workfile.
         DO 50 I=1,8
            PRIFIL(I,NPRPOS)=TPRFIL(I)
 50      CONTINUE
CARRAY
CFILE
CC        *****************************
C         WRITE(PRIFLU,REC=NPRPOS,ERR=99) (TPRFIL(I),I=1,8)
CC        *****************************
C
C
CFILE
C        update next free position pointer.
         NPRPOS=NPRPOS+1
         OK=.TRUE.
         RETURN
 99      CONTINUE
         WRITE(UNIT=10,FMT=*)'[RSTPR1] FILE READ ERROR'
         RETURN
 98      CONTINUE
C        GET HERE IF HIT END OF FILE
         OK=.FALSE.
         WRITE(UNIT=10,FMT=*)'[RSTPR1] END OF FILE ERROR'
C
      END
C
C
      SUBROUTINE RSTPR2(IOFF,PCOFF,OK)
C     ==================================
C
C1    VARTYPE           I*2   I2  L
C1    IOSTATUS           I    I   O
C
C2    Subroutine RSTPR2 reads a properties text record
C2    from the part storage file using pointer offset
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,IOFF,PCOFF,IC,WUN,PC1,PC2
      INTEGER*4 N
      CHARACTER*80 PPTEXT
      LOGICAL OK
C
      OK=.FALSE.
      WUN=1
C     pointer in range alright
CARRAY
C      WRITE(UNIT=10,FMT=*)'[RSTPR2] NPCPOS',NPCPOS
C           clear text record
            PRCFIL(NPCPOS)=' '
            READ(UNIT=PARFUN,ERR=99,END=98)
     +            IC,PRCFII(2,NPCPOS),
     1            N,PRCFIL(NPCPOS)(1:N)
            PRCFII(1,NPCPOS)=IC+IOFF
C      WRITE(UNIT=10,FMT=*)'[RSTPR2] ',PRCFII(1,NPCPOS),
C     +      PRCFII(2,NPCPOS),N,PRCFIL(NPCPOS)(1:N)
CARRAY
CFILE
C
C         PPTEXT='                                 '
C         READ(UNIT=PARFUN,ERR=99,END=98) PC1,PC2,
C     +             N,PPTEXT(1:N)
C      PC1=PC1+IOFF
C      WRITE(PRCFLU,REC=NPCPOS,ERR=99) PC1,PC2,PPTEXT
CFILE
C        UPDATE PROPERTY TEXT POINTER
      NPCPOS=NPCPOS+WUN
         OK=.TRUE.
         RETURN
 99      CONTINUE
         WRITE(UNIT=10,FMT=*)'[RSTPR2] FILE READ ERROR'
         RETURN
 98   CONTINUE
C     GET HERE IF END OF FILE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*)'[RSTPR2] END OF FILE ERROR'
C
      END
C
C
      SUBROUTINE RSTRL0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTRL0 retrieves a record of the
C2    RL file in the part storage file.The data
C2    is read from the part storage file and written
C2    to the RL work file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
C
      OK=.FALSE.
C     go read the data from the part file
      IF (P.GE.RLPLIM) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
         READ(UNIT=PARFUN,ERR=99) (RLFILE(I,P),I=1,10)
CARRAY
C
CFILE
C         READ(PARFUN,ERR=99) (RLBUFF(I),I=1,10)
C         WRITE(RLFILU,REC=P,ERR=99)   (RLBUFF(I),I=1,10)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE RSTRL1(IOFF,POFF,TOFF,ROFF,OK)
C     ===========================================
C
C1    VARTYPE           I*2  I*2  I*2  I*2  L
C1    IOSTATUS           I    I   I    I    O
C
C2    subroutine RSTRL1 retrieves a record of the
C2    RLFILE from the part storage file.The data
C2    is read from the RL part file and written
C2    to the wors file,the MI pointer is increased
C2    by the amount IOFF and the TX pointer by TOFF
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P,I,TRLFIL(10),IOFF,POFF,TOFF,ROFF
      LOGICAL OK
C
      OK=.FALSE.
      READ(UNIT=PARFUN,ERR=99) (TRLFIL(I),I=1,10)
C     offset the continue relation pointer.
      IF (TRLFIL(2).GT.0) TRLFIL(2)=TRLFIL(2)+ROFF
C     offset the parent mi pointer.
      IF (TRLFIL(3).GT.0) TRLFIL(3)=TRLFIL(3)+IOFF
      IF (TRLFIL(1).EQ.MILIST) THEN
C        offset the mi pointers in the list.
         DO 51 I=4,10
             IF (TRLFIL(I).GT.0) TRLFIL(I)=TRLFIL(I)+IOFF
 51      CONTINUE
      END IF
C     set pointer to next free position.
      P=NRLPOS
CARRAY
      DO 50 I=1,10
         RLFILE(I,P)=TRLFIL(I)
 50   CONTINUE
CARRAY
CFILE
C      WRITE(RLFILU,REC=P,ERR=99) (TRLFIL(I),I=1,10)
CFILE
C     update relation pointer.
      NRLPOS=NRLPOS+1
CD      WRITE(10,*)'[RSTRL1] ',P,(TRLFIL(I),I=1,10)
         OK=.TRUE.
 99      CONTINUE
C
      END
C
C
      SUBROUTINE RSTTX0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine RSTTX0 reads a TXT record
C2    from the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
C
      INTEGER*2 P,I,IBUFF,TMIP
      CHARACTER*40 TBUFF
      LOGICAL OK
CFILE
C      EXTERNAL DTM500
CFILE
C
      OK=.FALSE.
      IF (P.GE.TXPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         IF ( OROTRV .GT.1.22 ) THEN
            TXFILE(P)='
     +                                                        '
            READ(UNIT=PARFUN,ERR=99,END=99)
     +           CMIP(1,P),CMIP(2,P),I,TXFILE(P)(1:I)
         ELSE IF ( OROTRV .EQ.1.22 ) THEN
            READ(UNIT=PARFUN,ERR=99,END=99)
     +           CMIP(1,P),CMIP(2,P),TXFILE(P)
         ELSE
            READ(UNIT=PARFUN,ERR=99,END=99)
     +           CMIP(1,P),CMIP(2,P),TBUFF
            TXFILE(P)=TBUFF
         END IF
CARRAY
CFILE
C        CBUFF='   '
C        READ(PARFUN,ERR=99) ICBUFF(1),ICBUFF(2),I,CBUFF(1:I)
C        CALL DTM500(P,TMIP,OK)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
