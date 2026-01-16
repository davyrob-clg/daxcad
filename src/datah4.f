C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah4.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ADDISP(MIPP,ETYP,DDFP,OK)
C     SUBROUTINE RSTMI1(IOFF,POFF,TOFF,ROFF,PRPOFF,OK)
C     SUBROUTINE RSTPD1(IOFF,POFF,OK)
C     SUBROUTINE RSTTX1(IOFF,TOFF,OK)
C     SUBROUTINE SAVLY0(P,OK)
C     SUBROUTINE SAVMI0(P,OK)
C     SUBROUTINE SAVMI1(P,IOFF,POFF,TOFF,ROFF,PRPOFF,OK)
C     SUBROUTINE SAVPC0(P,OK)
C     SUBROUTINE SAVPD0(P,OK)
C     SUBROUTINE SAVPD1(P,IOFF,POFF,OK)
C     SUBROUTINE SAVPI0(P,OK)
C     SUBROUTINE SAVPR1(P,IOFF,POFF,PCOFF,OK)
C     SUBROUTINE SAVPR2(P,IOFF,PCOFF,OK)
C     SUBROUTINE SAVRL0(P,OK)
C     SUBROUTINE SAVRL1(P,IOFF,POFF,TOFF,ROFF,OK)
C     SUBROUTINE SAVTX0(P,OK)
C     SUBROUTINE SAVTX1(P,IOFF,TOFF,OK)
C     SUBROUTINE WDISPF(DFP1,ETYP,MIPP,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE ADDISP(MIPP,ETYP,DDFP,OK)
C     ===================================
C
C1    vartype              I2  I2   I2  L
C1    iostatus             I   I    O   O
C
C2    Subroutine ADDISP writes MIPP & ETYP to the next
C2    position in the display file,if within limits.Returns
C2    the position in P3. The MI pointer & Entity type
C2    Have to be passed to MIPP & ETYP. OK returned
C2    true if entry DFP is valid ,
C2    Ok returned false if end of file encountered.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 DDFP,ETYP,MIPP,TVPN,I
      LOGICAL OK
C
      OK=.FALSE.
C     set the current Display file pointer
      DDFP=LDFILE(CVPN)
C     test for valid pointer first
      IF (LDFILE(CVPN).EQ.MIPLIM) THEN
C        pointer valid,write data
CARRAY
         DFILE(1,LDFILE(CVPN),CVPN)=ETYP
         DFILE(2,LDFILE(CVPN),CVPN)=MIPP
CARRAY
CFILE
C         WRITE(DDFILU(DFPNT),REC=LDFILE(DFPNT),ERR=999) ETYP,MIPP
CFILE
         OK=.TRUE.
      ELSE IF (LDFILE(CVPN).LT.MIPLIM) THEN
C         pointer valid,write data
CARRAY
C         write the out to the viewport required
          DFILE(1,LDFILE(CVPN),CVPN)=ETYP
          DFILE(2,LDFILE(CVPN),CVPN)=MIPP
CARRAY
CFILE
C         WRITE(DDFILU(DFPNT),REC=LDFILE(DFPNT),ERR=999) ETYP,MIPP
CFILE
         LDFILE(CVPN)=LDFILE(CVPN)+1
         OK=.TRUE.
      END IF
C
 999  CONTINUE
C
      END
      SUBROUTINE RSTMI1(IOFF,POFF,TOFF,ROFF,PRPOFF,OK)
C     ================================================
C
C1    vartype           I*2  I*2   I*2   I*2  I2   L
C1    iostatus           I    I     I     I   I    O
C
C2    Subroutine RSTMI1 reads a record of the
C2    MI file from the part storage file.The data
C2    is written to the MI work file
C2    the PD pointer is increased
C2    by the amount POFF and the TX pointer by TOFF
C2    Relation pointer offset by ROFF,parent MI by IOFF
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
C
      INTEGER*2 P,I,TMIFIL(10),IOFF,POFF,TOFF,ROFF,IT,PRPOFF,WUN
      LOGICAL OK
C
C
      WUN=1
      OK=.FALSE.
C     go read the data from the part file
      READ(UNIT=PARFUN,ERR=99) (TMIFIL(I),I=1,10)
C     WRITE(UNIT=10,FMT=*,ERR=99)'[rstmi1]read ',(TMIFIL(I),I=1,10)
C     WRITE(UNIT=10,FMT=*,ERR=99)'[rstmi1]1 IOFF,POFF,TOFF,ROFF,PRPOFF'
C     WRITE(UNIT=10,FMT=*,ERR=99)'[rstmi1]',IOFF,POFF,TOFF,ROFF,PRPOFF
C     offset the PD pointer
      IF (TMIFIL(4).GT.0) TMIFIL(4)=TMIFIL(4)+POFF
C     offset the tx pointer
      IF (TMIFIL(6).GT.0) TMIFIL(6)=TMIFIL(6)+TOFF
C     offset the parent MI pointer
      IF (TMIFIL(5).GT.0) TMIFIL(5)=TMIFIL(5)+IOFF
C     offset the relation pointer
      IF (TMIFIL(7).GT.0) TMIFIL(7)=TMIFIL(7)+ROFF
C     offset the property pointer
      IF (TMIFIL(8).GT.0) TMIFIL(8)=TMIFIL(8)+PRPOFF
C     copy into data structure
      P=NMIPOS

C     now decode into 13 element array
      IMBUFF(1)=TMIFIL(1)/256
      IT=TMIFIL(1)
C     type of entity
      IMBUFF(2)=MOD(IT,256)
C     color
      IMBUFF(3)=TMIFIL(2)/256
      IT=TMIFIL(2)
C     Layer of entity
      IMBUFF(4)=MOD(IT,256)
C     Spare position
      IMBUFF(5)=TMIFIL(3)/256
      IT=TMIFIL(3)
C     entity font
      IMBUFF(6)=MOD(IT,256)
C     now do rest
      IMBUFF(7)=TMIFIL(4)
      IMBUFF(8)=TMIFIL(5)
      IMBUFF(9)=TMIFIL(6)
      IMBUFF(10)=TMIFIL(7)
      IMBUFF(11)=TMIFIL(8)
      IMBUFF(12)=TMIFIL(9)
      IMBUFF(13)=TMIFIL(10)
      CALL DIW500(P,OK)
CFILE
      OK=.TRUE.
C
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[RSTMI1] Error reading part file'
C
      END
C
C
      SUBROUTINE RSTPD1(IOFF,POFF,OK)
C     ===============================
C1    vartype            I*2 I*2  L
C1    iostatus            I   I   O
C
C2    Subroutine RSTPD1 reads a PDF record
C2    from the part storage file using offsets
C2    on the database pointers,(increasing them)
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
C
      INTEGER*2 P,I,POFF,IOFF,TPDFI(4),J,WUN
      LOGICAL OK
      REAL TPDFR(6)
C
      WUN=1
      OK=.FALSE.
      P=NPDPOS
C
C     Read from file first
C
      READ(UNIT=PARFUN,ERR=99) (TPDFI(I),I=1,4),
     +                         (RDBUFF(I),I=1,6)
C     fix MI pointer
      TPDFI(2)=TPDFI(2)+IOFF
C     fix PD connection pointer
      IF (TPDFI(3).NE.0) TPDFI(3)=TPDFI(3)+POFF
C
C     pointer in range alright copy to id buffer
C
      DO 50 I=1,4
         IDBUFF(I)=TPDFI(I)
 50   CONTINUE
C
C     Write data into database with all set as offset
C
      CALL DBW502(P,OK)
C
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[RSTPD1] Error reading part file'
C
      END
 
      SUBROUTINE RSTTX1(IOFF,TOFF,OK)
C     ==============================
C
C1    vartype           I*2  I*2   L
C1    iostatus          I    I     O
C
C2    Subroutine RSTTX1 writes a TXT record
C2    to the part storage file using pointer offset
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
C
      INTEGER*2 P,I,I1,IOFF,TOFF,WUN,TMIP
      CHARACTER*40 TBUFF
      LOGICAL OK
C
C
      WUN=1
      OK=.FALSE.
      P=NTXPOS
C     pointer in range alright
CARRAY
      IF ( OROTRV.GT.1.22 ) THEN
            TXFILE(P)='
     +                                                        '
            READ(UNIT=PARFUN,ERR=99)
     +           CMIP(1,P),CMIP(2,P),I,TXFILE(P)(1:I)
      ELSE IF ( OROTRV.LT.1.22 ) THEN
         READ(UNIT=PARFUN,ERR=99)
     +   CMIP(1,P),CMIP(2,P),TBUFF
         TXFILE(P)=TBUFF
      ELSE
         READ(UNIT=PARFUN,ERR=99)
     +   CMIP(1,P),CMIP(2,P),TXFILE(P)
      END IF
C     fix pointer
      CMIP(1,P)=CMIP(1,P)+IOFF
      IF ( CMIP(2,P) .NE. 0) THEN
         CMIP(2,P)=CMIP(2,P)+TOFF
      END IF
CARRAY
CFILE
C      CBUFF=' '
C      READ(PARFUN,ERR=99) ICBUFF(1),ICBUFF(2),I,CBUFF(1:I)
C      ICBUFF(1)=ICBUFF(1)+IOFF
C      IF(ICBUFF(2).NE.0) THEN
C         ICBUFF(2)=ICBUFF(2)+TOFF
C      END IF
C      WRITE(TXFILU,REC=P) ICBUFF(1),ICBUFF(2),CBUFF
CC      CALL DTM500(P,TMIP,OK)
CFILE
      OK=.TRUE.
      NTXPOS=NTXPOS+WUN
      RETURN
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[RSTTX1] Error reading part file'
C
      END
C
C
      SUBROUTINE SAVLY0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
 
C2    Subroutine SAVTX0 writes a TXT record
C2    to the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
C
      OK=.FALSE.
C
C     pointer in range alright
      WRITE(UNIT=PARFUN,ERR=99)  P,LNAME(P)
      OK=.TRUE.
 99   CONTINUE
C
      END
 
      SUBROUTINE SAVMI0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVMI0 saves a record of the
C2    MI file in the part storage file.The data
C2    is read from the MI work file and written
C2    to the output file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
CFILE
      INTEGER*2 TMBUFF(10)
C
      EXTERNAL DIR500
CFILE
      OK=.FALSE.
C     go read the data from the work file
      IF (P.GE.NMIPOS) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
C         WRITE(UNIT=PARFUN,ERR=99) (MIFILE(I,P),I=1,10)
CARRAY
CFILE
           CALL DIR500(P,OK)

C           compact the MIP's to 10
           TMBUFF(1)=IMBUFF(2)+256*IMBUFF(1)
           TMBUFF(2)=IMBUFF(4)+256*IMBUFF(3)
           TMBUFF(3)=IMBUFF(6)+256*IMBUFF(5)

           TMBUFF(4)=IMBUFF(7)
           TMBUFF(5)=IMBUFF(8)
           TMBUFF(6)=IMBUFF(9)
           TMBUFF(7)=IMBUFF(10)
           TMBUFF(8)=IMBUFF(11)
           TMBUFF(9)=IMBUFF(12)
           TMBUFF(10)=IMBUFF(13)
C			print*, (TMBUFF(I),I=1,10)
C			print*, (IMBUFF(I),I=1,13)
C           now write out the part data to file.
           WRITE(PARFUN,ERR=99) (TMBUFF(I),I=1,10)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVMI1(P,IOFF,POFF,TOFF,ROFF,PRPOFF,OK)
C     ==================================================
C
C1    vartype          I*2 I*2  I*2  I*2  I*2   I2   L
C1    iostatus          I   I    I   I    I     I    O
C
C2    Subroutine SAVMI1 saves a record of the
C2    MI file in the part storage file.The data
C2    is read from the MI work file and written
C2    to the output file,the PD pointer is reduced
C2    by the amount POFF and the TX pointer by TOFF
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
C
      INTEGER*2 P,I,TMIFIL(10),IOFF,POFF,TOFF,ROFF,PRPOFF
      LOGICAL OK
CFILE
C      EXTERNAL DIR500
CFILE
      OK=.FALSE.
C     go read the data from the work file
      IF (P.GE.NMIPOS) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
C         DO 50 I=1,10
C            TMIFIL(I)=MIFILE(I,P)
C 50      CONTINUE
CARRAY
CFILE
        CALL DIR500(P,OK)
C        offset the PD pointer
C        IMBUFF(7)=IMBUFF(7)-POFF
C        offset the TX pointer
C        IF (IMBUFF(9).GT.0) IMBUFF(9)=IMBUFF(9)-TOFF
C        compact the mips to 10
        TMIFIL(1)=IMBUFF(2)+256*IMBUFF(1)
        TMIFIL(2)=IMBUFF(4)+256*IMBUFF(3)
        TMIFIL(3)=IMBUFF(6)+256*IMBUFF(5)
        TMIFIL(4)=IMBUFF(7)
        TMIFIL(5)=IMBUFF(8)
        TMIFIL(6)=IMBUFF(9)
        TMIFIL(7)=IMBUFF(10)
        TMIFIL(8)=IMBUFF(11)
        TMIFIL(9)=IMBUFF(12)
        TMIFIL(10)=IMBUFF(13)
CFILE
C        offset the PD pointer
         IF (TMIFIL(4).GT.0) TMIFIL(4)=TMIFIL(4)-POFF
C        offset the tx pointer
         IF (TMIFIL(6).GT.0) TMIFIL(6)=TMIFIL(6)-TOFF
C        offset the parent MI pointer
         IF (TMIFIL(5).GT.0) TMIFIL(5)=TMIFIL(5)-IOFF
C        offset the relation pointer
         IF (TMIFIL(7).GT.0) TMIFIL(7)=TMIFIL(7)-ROFF
C        offset the property pointer
         IF (TMIFIL(8).GT.0) TMIFIL(8)=TMIFIL(8)-PRPOFF
         WRITE(UNIT=PARFUN,ERR=99) (TMIFIL(I),I=1,10)
C        WRITE(UNIT=10,FMT=*,ERR=99) '[savmi1] ',P,(TMIFIL(I),I=1,10)
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE SAVPC0(P,OK)
C     =======================
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVPC0 saves a record of the
C2    PC file in the part storage file.The data
C2    is read from the PC work file and written
C2    to the output file
C
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,PC1,PC2
      INTEGER*4 NLEN1,N
      CHARACTER*80 PPTEXT
      LOGICAL OK
      EXTERNAL NLEN1
C
      OK=.FALSE.
C     go read the data from the work file
      IF (P.GE.NPCPOS) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
C        get length of string
CARRAY
         N=NLEN1(PRCFIL(P))
            WRITE(UNIT=PARFUN,ERR=99) (PRCFII(I,P),I=1,2),
     +            N,PRCFIL(P)(1:N)
CARRAY
CFILE
C      READ(PRCFLU,REC=P,ERR=99) PC1,PC2,PPTEXT
C      N=NLEN1(PPTEXT)
C      WRITE(UNIT=PARFUN,ERR=99) PC1,PC2,
C     +             N,PPTEXT(1:N)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVPD0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVPD0 writes a PDF record
C2    to the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
CFILE
C      EXTERNAL DBR500
CFILE
      OK=.FALSE.
      IF (P.GE.PDPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
         CALL DBR500(P,OK)
         WRITE(PARFUN,ERR=99) (IDBUFF(I),I=1,4),
     +                         (RDBUFF(I),I=1,6)
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVPD1(P,IOFF,POFF,OK)
C     ==================================
C
C1    vartype          I*2  I*2 I*2  L
C1    iostatus          I    I   I   O
C
C2    Subroutine SAVPD1 writes a PDF record
C2    to the part storage file using offsets
C2    on the database pointers,(increasing them)
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
C
      INTEGER*2 P,I,POFF,IOFF,TPDFI(4),J
      LOGICAL OK
      REAL TPDFR(6)
C
      OK=.FALSE.
      IF (P.GE.PDPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
         CALL DBR500(P,OK)
         DO 50 I=1,4
            TPDFI(I)=IDBUFF(I)
 50      CONTINUE
C        fix MI pointer
         TPDFI(2)=TPDFI(2)-IOFF
C        fix PD connection pointer
         IF (TPDFI(3).NE.0) TPDFI(3)=TPDFI(3)-POFF
C
C        Write out to file
C
         WRITE(UNIT=PARFUN,ERR=99) (TPDFI(I),I=1,4),
     +                             (RDBUFF(I),I=1,6)
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVPI0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVPI0 saves a record of the
C2    PI file in the part storage file.The data
C2    is read from the PI work file and written
C2    to the output file
C
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,PRIB(8)
      LOGICAL OK
C
      OK=.FALSE.
C     go read the data from the work file
      IF (P.GE.NPRPOS) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
         WRITE(UNIT=PARFUN,ERR=99) (PRIFIL(I,P),I=1,8)
C      WRITE(UNIT=10,FMT=*)'[SAVPI0] p=',P
CARRAY
CFILE
C         READ(PRIFLU,REC=P,ERR=99) (PRIB(I),I=1,8)
C         WRITE(UNIT=PARFUN,ERR=99) (PRIB(I),I=1,8)
CFILE
C
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVPR1(P,IOFF,POFF,PCOFF,OK)
C     =======================================
C
C1    VARTYPE          I*2 I*2  I*2  I*2   L
C1    IOSTATUS          I   I    I   I     O
C
C2    SUBROUTINE SAVPR1 SAVES A RECORD OF THE
C2    PROPERTIES MI FILE IN THE PART STORAGE FILE.THE DATA
C2    IS READ FROM THE PROPERTIES MI WORK FILE AND WRITTEN
C2    TO THE OUTPUT FILE,THE MI POINTER IS REDUCED
C2    BY THE AMOUNT IOFF AND THE PROPERTIES TEXT POINTER BY PCOFF.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,J,TPRFIL(8),IOFF,POFF,PCOFF,I2A,I2B,I2C,I2D,I2E
      LOGICAL OK
C
      OK=.FALSE.
C     GO READ THE DATA FROM THE WORK FILE
      IF (P.GE.NPRPOS) THEN
C        POINTER OUT OF RANGE
         RETURN
      ELSE
C        POINTER IS IN RANGE
CFILE
CC        *****************************
C         READ(PRIFLU,REC=P,ERR=99) (TPRFIL(I),I=1,8)
CC        *****************************
CFILE
CARRAY
         DO 50 I=1,8
            TPRFIL(I)=PRIFIL(I,P)
 50      CONTINUE
CARRAY
C        OFFSET THE PARENT MI POINTER
         IF (TPRFIL(3).GT.0) TPRFIL(3)=TPRFIL(3)-IOFF
C        OFFSET THE PR CONTINUATION POINTER
         IF (TPRFIL(4).GT.0) TPRFIL(4)=TPRFIL(4)-POFF
         I2A=4
         I2B=2
         I2C=3
         I2D=5
         I2E=1
         IF (TPRFIL(1).EQ.I2A) THEN
C           IF PROPERTY LIST,OFFSET ALL PROPERTY CONTROL POINTERS
            DO 20 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)-POFF
 20         CONTINUE
         ELSE IF (TPRFIL(1).EQ.I2B) THEN
C           IF PROPERTY LINK,OFFSET NAME POINTER
            IF (TPRFIL(5).GT.0) TPRFIL(5)=TPRFIL(5)-PCOFF
         ELSE IF (TPRFIL(1).EQ.I2C) THEN
C           IF PROPERTY SET,OFFSET NAME POINTER
            IF (TPRFIL(5).GT.0) TPRFIL(5)=TPRFIL(5)-PCOFF
         ELSE IF (TPRFIL(1).EQ.I2D) THEN
C           IF ATTACHED PROPERTY,OFFSET DATA POINTERS
            DO 21 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)-PCOFF
 21         CONTINUE
         ELSE IF (TPRFIL(1).EQ.I2E) THEN
C           IF MASTER PROPERTY,OFFSET DATA POINTERS
            DO 22 J=5,8
               IF (TPRFIL(J).GT.0) TPRFIL(J)=TPRFIL(J)-PCOFF
 22         CONTINUE
         END IF
            WRITE(UNIT=PARFUN,ERR=99) (TPRFIL(I),I=1,8)
CD            WRITE(UNIT=10,FMT=*) '[SAVPR1] ',P,(TPRFIL(I),I=1,8)
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE SAVPR2(P,IOFF,PCOFF,OK)
C     ==================================
C
C1    VARTYPE          I*2 I*2   I2  L
C1    IOSTATUS          I   I    I   O
C
C2    SUBROUTINE SAVPR2 WRITES A PROPERTIES TEXT RECORD
C2    TO THE PART STORAGE FILE USING POINTER OFFSET
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/props.inc'
C
      INTEGER*2 P,I,IOFF,PCOFF,PC1,PC2
      INTEGER*4 NLEN1,N
      CHARACTER*80 PPTEXT
      LOGICAL OK
      EXTERNAL NLEN1
C
      OK=.FALSE.
C        POINTER IN RANGE ALRIGHT
CARRAY
C     FIND LENGTH OF STRING
      N=NLEN1(PRCFIL(P))
         WRITE(UNIT=PARFUN,ERR=99)
     +        PRCFII(1,P)-IOFF,PRCFII(2,P),
     1        N,PRCFIL(P)(1:N)
CARRAY
CFILE
C      READ(PRCFLU,REC=P,ERR=99) PC1,PC2,PPTEXT
C      N=NLEN1(PPTEXT)
C      WRITE(UNIT=PARFUN,ERR=99) PC1-IOFF,PC2,
C     +             N,PPTEXT(1:N)
C
CFILE
         OK=.TRUE.
 99      CONTINUE
CD      WRITE(UNIT=10,FMT=*)'[SAVPR2] ',PRCFII(1,P)-IOFF,PRCFII(2,P),
CD    +         N,PRCFIL(P)(1:N)
C
      END
C
C
      SUBROUTINE SAVRL0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVRL0 saves a record of the
C2    RL file in the part storage file.The data
C2    is read from the RL work file and written
C2    to the output file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
C
      INTEGER*2 P,I
      LOGICAL OK
CFILE
C      INTEGER*2 TMBUFF(10)
CC
C      EXTERNAL DRR950
CFILE
      OK=.FALSE.
C     go read the data from the work file
      IF (P.GE.NRLPOS) THEN
C        pointer out of range
         RETURN
      ELSE
C        pointer is in range
CARRAY
         WRITE(UNIT=PARFUN,ERR=99) (RLFILE(I,P),I=1,10)
CARRAY
CFILE
C        CALL DRR950(P,OK)
C        WRITE(PARFUN,ERR=99) (RLBUFF(I),I=1,10)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
*
      SUBROUTINE SAVRL1(P,IOFF,POFF,TOFF,ROFF,OK)
C     ===========================================
C
C1    VARTYPE          I*2 I*2  I*2  I*2  I*2  L
C1    IOSTATUS          I   I    I   I    I    O
C
C2    Subroutine SAVRL1 saves a record of the
C2    RL file in the part storage file.the data
C2    is read from the RL work file and written
C2    to the output file,the MI pointer is reduced
C2    by the amount IOFF and the TX pointer by toff
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
C     GO READ THE DATA FROM THE WORK FILE
      IF (P.GE.NRLPOS) THEN
C        POINTER OUT OF RANGE
         RETURN
      ELSE
C        POINTER IS IN RANGE
CARRAY
         DO 50 I=1,10
            TRLFIL(I)=RLFILE(I,P)
 50      CONTINUE
CARRAY
CFILE
C         READ(RLFILU,REC=P,ERR=99) (TRLFIL(I),I=1,10)
CFILE
C        OFFSET THE CONTINUE RELATION POINTER
         IF (TRLFIL(2).GT.0) TRLFIL(2)=TRLFIL(2)-ROFF
C        OFFSET THE PARENT MI POINTER
         IF (TRLFIL(3).GT.0) TRLFIL(3)=TRLFIL(3)-IOFF
         IF (TRLFIL(1).EQ.MILIST) THEN
C           OFFSET THE MI POINTERS IN THE LIST
            DO 51 I=4,10
               IF (TRLFIL(I).GT.0) TRLFIL(I)=TRLFIL(I)-IOFF
 51         CONTINUE
         END IF
CD      WRITE(10,*)'[SAVRL1] ',P,(TRLFIL(I),I=1,10)
         WRITE(UNIT=PARFUN,ERR=99) (TRLFIL(I),I=1,10)
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE SAVTX0(P,OK)
C     =======================
C
C1    vartype          I*2 L
C1    iostatus          I  O
C
C2    Subroutine SAVTX0 writes a TXT record
C2    to the part storage file
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
C
      INTEGER*2 P,I
      INTEGER*4 NLEN1
      LOGICAL OK
      EXTERNAL NLEN1
C
      OK=.FALSE.
C
      IF (P.GE.TXPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         I=NLEN1(TXFILE(P))
         WRITE(UNIT=PARFUN,ERR=99)
     +           CMIP(1,P),CMIP(2,P),I,TXFILE(P)(1:I)
C            WRITE(UNIT=PARFUN,ERR=99)
C     +           CMIP(1,P),CMIP(2,P),TXFILE(P)
C      END IF
CARRAY
CFILE
C           CALL DTR500(P,OK)
C           I=NLEN1(CBUFF)
C           WRITE(PARFUN,ERR=99) ICBUFF(1),ICBUFF(2),I,CBUFF(1:I)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
 
      SUBROUTINE SAVTX1(P,IOFF,TOFF,OK)
C     =================================
C
C1    vartype          I*2 I*2  I*2 L
C1    iostatus          I   I   I   O
C
C2    Subroutine SAVTX0 writes a TXT record
C2    to the part storage file using pointer offset
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/filunit.inc'
C
      INTEGER*2 P,I,I1,IOFF,TOFF,N
      INTEGER*4 NLEN1
      LOGICAL OK
      EXTERNAL NLEN1
C
      OK=.FALSE.
      IF (P.GE.TXPLIM) THEN
C        test for valid pointer
         RETURN
      ELSE
C        pointer in range alright
CARRAY
         I=CMIP(1,P)-IOFF
         IF ( CMIP(2,P) .NE. 0 ) THEN
            I1=CMIP(2,P)-TOFF
         ELSE
            I1=0
         END IF
         N=NLEN1(TXFILE(P))
         WRITE(UNIT=PARFUN,ERR=99)
     +                  I,I1,N,TXFILE(P)(1:N)
CARRAY
CFILE
CC        READ(TXFILU,REC=P) ICBUFF(1),ICBUFF(2),CBUFF
C        CALL DTR500(P,OK)
C        I=ICBUFF(1)-IOFF
C         IF ( ICBUFF(2) .NE. 0 ) THEN
C            I1=ICBUFF(2)-TOFF
C         ELSE
C            I1=0
C         END IF
C        N=NLEN1(CBUFF)
C        WRITE(PARFUN,ERR=99) I,I1,N,CBUFF(1:N)
CFILE
         OK=.TRUE.
 99      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE WDISPF(DFP1,ETYP,MIPP,OK)
C     ===================================
C
C1    vartype           I2   I2   I2  L
C1    iostatus          I    I    I   O
C
C2    Subroutine WDISPF writes the data to
C2    position DFP in the display file,and passes
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
         DFILE(1,DFP1,CVPN)=ETYP
         DFILE(2,DFP1,CVPN)=MIPP
CARRAY
CFILE
C        WRITE(DDFILU(DFPNT),REC=DFP1,ERR=999) ETYP,MIPP
CFILE
C
         OK=.TRUE.
 999  CONTINUE
      END IF
C
      END
*
