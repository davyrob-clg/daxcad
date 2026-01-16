C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 igesw.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBRCNT(PDP,NPDR,OK)
C     SUBROUTINE IGADBF(STRING)
C     SUBROUTINE IGAEOR()
C     SUBROUTINE IGAHBF(STRING)
C     SUBROUTINE IGAIBF(I)
C     SUBROUTINE IGARBF(R)
C     SUBROUTINE IGDDBF(D)
C     SUBROUTINE IGW000()
C     SUBROUTINE IGW003(IGDP)
C     SUBROUTINE IGW005(IGDP)
C     SUBROUTINE IGW031(IGDP)
C     SUBROUTINE IGW085(IGDP)
C     SUBROUTINE IGW885(TPARMS,FONT,STRING)
C     SUBROUTINE IGWALL(OK)
C     SUBROUTINE IGWD00(MIBUFF,IGDP)
C     SUBROUTINE IGWD01(ENTNUM,ENTLAB,ENTSUB)
C     SUBROUTINE IGWD02(IGLFNT,IGLWGT,IGPENN)
C     SUBROUTINE IGWD03(IFORM)
C     SUBROUTINE IGWD04(ST1,ST2,ST3,ST4)
C     SUBROUTINE IGWD10(IGNPAR,IGPP)
C     SUBROUTINE IGWDUN(OK)
C     SUBROUTINE IGWGLS(OK)
C     SUBROUTINE IGWINF(FNAM,OK)
C     SUBROUTINE IGWINI()
C     SUBROUTINE IGWOUT(STRING,SECC,OK)
C     SUBROUTINE IGWP00(ENTNUM)
C     SUBROUTINE IGWP10(IGDPP,IGNP,IGPP)
C     SUBROUTINE IGWP11(SRL,P1,P2)
C     SUBROUTINE IGWP12(IGDPP,IGNP,IGPP)
C     SUBROUTINE IGWP20(STRING,OK)
C     SUBROUTINE IGWP99(OK)
C     SUBROUTINE IGWSTS(OK)
C     SUBROUTINE IGWTMS(OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DBRCNT(PDP,NPDR,OK)
C     ==============================
C1    vartype            I2  I4  L
C1    iostatus           I   O   O
C
C2    Subroutine DBRCNT counts the number of connected
C2    PD records occupied starting from the
C2    passed PD pointer PDP. The number of records
C2    occupied is returned in NPDR.
C2    OK returned true if successful.
C
      include 'include/nbuff.inc'
C
      INTEGER*2 PDP,P
      INTEGER*4 NPDR
      LOGICAL OK
      EXTERNAL DBR500
C
C     initialize record count to 1
      NPDR=1
C     initialize PD pointer
      P=IDBUFF(3)
C     test for continuation pointer
      IF (IDBUFF(3).GT.0) THEN
C       count number of records used
 10     CONTINUE
        CALL DBR500(P,OK)
        NPDR=NPDR+1
        P=IDBUFF(3)
C       loop until no continuation pointer
        IF (P.GT.0) GOTO 10
C       re-read original record
        CALL DBR500(PDP,OK)
      ELSE
C       only one record used
        OK=.TRUE.
      END IF
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE IGADBF(STRING)
C     =========================
C1    vartype             C*(*)
C1    iostatus              I
C
C2    Subroutine IGADBF writes a string directly
C2    to the iges work string BIGBUF.
C2    The field delimiter EOV is added to the end of field.
C
      include  'include/iges.inc'
C
      INTEGER*4 L,NLEN1
      CHARACTER STRING*(*)
      EXTERNAL NLEN1
C
C     get length of string
      L=NLEN1(STRING)
C     put string in buffer
      BIGBUF(BIGWP:BIGWP+L)=STRING(1:L)
C     point past string
      BIGWP=BIGWP+L
C     add delimiter
      BIGBUF(BIGWP:BIGWP)=EOV
C     set pointer to next free pos
      BIGWP=BIGWP+1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGAEOR()
C     ===================
C1    vartype
C1    iostatus
C
C2    Subroutine IGAEOR adds the end of record char to the
C2    iges work string BIGBUF.
C
      include  'include/iges.inc'
C
C     set end of record pointer
      BIGBER=BIGWP-1
      BIGBUF(BIGBER:BIGBER)=EOR
C     set active length of buffer
      BIGNL=BIGBER
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
 
      SUBROUTINE IGAHBF(STRING)
C     =========================
C1    vartype             C*(*)
C1    iostatus              I
C
C2    Subroutine IGAHBF writes a string to the
C2    iges work string BIGBUF,in hollerith format.
C2    The field delimiter EOV is added to the end of field.
C
      include  'include/iges.inc'
C
      INTEGER*4 I,L,NLEN1
      CHARACTER*8 BUF,STRING*(*)
      EXTERNAL CRUNCH,NLEN1
C
C     get length of string
      L=NLEN1(STRING)
C     construct hollerith in buffer
      BUF=' '
      WRITE(UNIT=BUF,FMT='(I8)',ERR=99) L
      CALL CRUNCH(BUF)
C     get length of integer string
      I=NLEN1(BUF)
C     add count to buffer
      BIGBUF(BIGWP:BIGWP+I)=BUF(1:I)
C     set pointer past char count
      BIGWP=BIGWP+I
C     write 'H' for hollerith
      BIGBUF(BIGWP:BIGWP)='H'
C     point to first pos for string
      BIGWP=BIGWP+1
C     put string in buffer
      BIGBUF(BIGWP:BIGWP+L)=STRING(1:L)
C     point past string
      BIGWP=BIGWP+L
C     add delimiter
      BIGBUF(BIGWP:BIGWP)=EOV
C     set pointer to next free pos
      BIGWP=BIGWP+1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGAIBF(I)
C     ====================
C1    vartype           I4
C1    iostatus          I
C
C2    Subroutine IGAIBF writes an integer to the
C2    iges work string BIGBUF,minimising the field size
C2    while doing so.
C2    The field delimiter EOV is added to the end of field.
C
      include  'include/iges.inc'
C
      INTEGER*4 I,L,NLEN1
      CHARACTER*16 BUF
      EXTERNAL CRUNCH,NLEN1
C
      BUF=' '
      WRITE(UNIT=BUF,FMT='(I16)',ERR=99) I
      CALL CRUNCH(BUF)
      L=NLEN1(BUF)
      BIGBUF(BIGWP:BIGWP+L)=BUF(1:L)
      BIGWP=BIGWP+L
      BIGBUF(BIGWP:BIGWP)=EOV
      BIGWP=BIGWP+1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGARBF(R)
C     ====================
C1    vartype           R
C1    iostatus          I
C
C2    Subroutine IGARBF writes a real to the
C2    iges work string BIGBUF,minimising the field size
C2    while doing so.
C2    The field delimiter EOV is added to the end of field.
C
      include  'include/iges.inc'
C
      REAL R
      INTEGER*4 L,NLEN1
      CHARACTER*20 BUF
      EXTERNAL CRUNCH,NLEN1
C
      BUF=' '
      WRITE(UNIT=BUF,FMT='(F18.8)',ERR=99) R
C     suppress trailing zeroes
      CALL ZSUPP1(BUF)
C     suppress leading spaces
      CALL CRUNCH(BUF)
C     get active length
      L=NLEN1(BUF)
      BIGBUF(BIGWP:BIGWP+L)=BUF(1:L)
      BIGWP=BIGWP+L
      BIGBUF(BIGWP:BIGWP)=EOV
      BIGWP=BIGWP+1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGDDBF(D)
C     ====================
C1    vartype           R
C1    iostatus          I
C
C2    Subroutine IGARBF writes a double precision to the
C2    iges work string BIGBUF,minimising the field size
C2    while doing so.
C2    The field delimiter EOV is added to the end of field.
C
      include  'include/iges.inc'
C
      DOUBLE PRECISION D
      INTEGER*4 L,NLEN1
      CHARACTER*33 BUF
      EXTERNAL CRUNCH,NLEN1
C
      BUF=' '
      WRITE(UNIT=BUF,FMT='(G32.16)',ERR=99) D
C     suppress trailing zeroes
      CALL ZSUPP1(BUF)
C     suppress leading spaces
      CALL CRUNCH(BUF)
C     get active length
      L=NLEN1(BUF)
      BIGBUF(BIGWP:BIGWP+L)=BUF(1:L)
      BIGWP=BIGWP+L
      BIGBUF(BIGWP:BIGWP)=EOV
      BIGWP=BIGWP+1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGW000()
C     ===================
C1    No arguments
C
C2    This routine will produce a IGES file format
C
      include  'include/menun.inc'
      include  'include/iges.inc'
C
      INTEGER*4 NLEN
      LOGICAL OK
      CHARACTER*80 FILNAM
      EXTERNAL DPRMXP,NLEN
C
C     set IGES version
      IGVERS=3
CCCCCC     set buffer threshold limit  ---  it's a PARAMETER now
CCCCC      BIGSIZ=8000
      CALL DCPRNT(644)
C     get the IGES filename
      CALL DPRMXP(81,FILNAM)
      IF (NLEN(FILNAM).EQ.0) GOTO 99
C     ensure iges suffix applied to file
      CALL SUFFIX(FILNAM,'.IGS')
C     initialize iges data block for writing
      CALL IGWINI()
C     open output,and work files
      CALL IGWINF(FILNAM,OK)
      IF (.NOT.OK) THEN
        CALL DEPRNT(645)
        GOTO 99
      END IF
C
C     write START section to output file
      CALL IGWSTS(OK)
C     write GLOBAL section to output file
      CALL IGWGLS(OK)
C     write entity data to output file
      CALL IGWALL(OK)
C     copy parameter data from scratch into ascii file
      CALL IGWP99(OK)
C     write terminate section to output file
      CALL IGWTMS(OK)
C
C     *********************************************************
C     everything ok close up and go home
C     *********************************************************
      CALL IGWDUN(OK)
      CALL POPPD1(FILNAM,473,120,550,667)
C      CALL DCPRNT(646)
C
      RETURN
 99   CCMD='Q'
      MEN=0
      RETURN
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGW003(IGDP)
C     =======================
C1    vartype            I4
C1    iostatus           I
C
C2    Subroutine IGW003 writes the LINE currently
C2    in the buffer to the iges file structure.
C2    IGDP is the allocated directory entry
C2    Creates on iges LINE entity.
C
      include  'include/iges.inc'
      include  'include/nbuff.inc'
C
      INTEGER*4 ENTNUM,IGDP,I,IGPP,COLOR,THICK
      DATA  ENTNUM/110/
C
C     line data currently in buffer
C     set entity number and label,and entity subscript
      CALL IGWD01(ENTNUM,'    LINE',LINSUB)
C
C     set font,thick,colour here
C     get font number
      I=IMBUFF(6)
C     get thickness
      THICK=IMBUFF(12)
C     get colour number
      COLOR=IMBUFF(3)
      CALL IGWD02(I,THICK,COLOR)
C
C     initialize parameter block for entity
      CALL IGWP00(ENTNUM)
C
C     write parameter data to buffer
      DO 10 I=1,6
        CALL IGARBF(RDBUFF(I))
 10   CONTINUE
C     add number pointers to associativities
      CALL IGAIBF(0)
C     add number pointers to properties
      CALL IGAIBF(0)
C     terminate parameter data
      CALL IGAEOR()
C     write parameter data to file
      CALL IGWP10(IGDP,I,IGPP)
C     I returns number of parameter records written
C     IGPP returns pointer to first parameter record
C     now write directory entry
      CALL IGWD10(I,IGPP)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGW005(IGDP)
C     =======================
C1    vartype            I4
C1    iostatus           I
C
C2    Subroutine IGW005 writes the CIRCLE currently
C2    in the buffer to the iges file structure.
C2    IGDP is the allocated directory entry
C2    Creates on iges CIRCULAR ARC entity.
C
      include  'include/iges.inc'
      include  'include/nbuff.inc'
C
      DOUBLE PRECISION DARC(7),RADIUS,SANG,EANG,DPI
      INTEGER*4 ENTNUM,IGDP,I,IGPP,THICK,COLOR
      LOGICAL SAME
      EXTERNAL DPI,SAME
      DATA  ENTNUM/100/
 
C
C     line data currently in buffer
C     set entity number and label,and entity subscript
      CALL IGWD01(ENTNUM,'  CIRCLE',ARCSUB)
C
C     set font,thick,colour here
C     get font number
      I=IMBUFF(6)
C     get thickness
      THICK=IMBUFF(12)
C     get colour number
      COLOR=IMBUFF(3)
      CALL IGWD02(I,THICK,COLOR)
C
C     initialize parameter block for entity
      CALL IGWP00(ENTNUM)
C
C     write parameter data to buffer
C     get ZT plane for arc
      DARC(1)=DBLE(RDBUFF(3))
C     get circle centre position
      DARC(2)=DBLE(RDBUFF(1))
      DARC(3)=DBLE(RDBUFF(2))
C     get radius
      RADIUS=DBLE(RDBUFF(4))
      SANG=DBLE(RDBUFF(5))
      EANG=DBLE(RDBUFF(6))
C     calculate start and end points
      DARC(4)=DARC(2)+RADIUS*COS(SANG)
      DARC(5)=DARC(3)+RADIUS*SIN(SANG)
      DARC(6)=DARC(2)+RADIUS*COS(EANG)
      DARC(7)=DARC(3)+RADIUS*SIN(EANG)
      IF(SAME(EANG-SANG,DPI(2.0))) THEN
          DARC(6)=DARC(4)
          DARC(7)=DARC(5)
      ENDIF
C     write data to buffer
      DO 10 I=1,7
        CALL IGDDBF(DARC(I))
 10   CONTINUE
C     add number pointers to associativities
      CALL IGAIBF(0)
C     add number pointers to properties
      CALL IGAIBF(0)
C     terminate parameter data
      CALL IGAEOR()
C     write parameter data to file
      CALL IGWP10(IGDP,I,IGPP)
C     I returns number of parameter records written
C     IGPP returns pointer to first parameter record
C     now write directory entry
      CALL IGWD10(I,IGPP)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGW031(IGDP)
C     =======================
C1    vartype            I4
C1    iostatus           I
C
C2    Subroutine IGW031 writes the HATCH currently
C2    in the buffer to the iges file structure.
C2    IGDP is the allocated directory entry
C2    Creates on iges SECTION entity.
C2    Using copious data entity 106
C
      include  'include/iges.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 PDP,P
      INTEGER*4 ENTNUM,IGDP,I,IGPP,COLOR,THICK,NPDR,N,IGNP
      LOGICAL OK,FIRST
      DATA  ENTNUM/106/
C
C     hatch data currently in buffer
C     set entity number and label,and entity subscript
      CALL IGWD01(ENTNUM,'   HATCH',HATSUB)
C
      FIRST=.TRUE.
      IGPP=0
C     set font,thick,colour here
C     get font number
      I=IMBUFF(6)
C     get thickness
      THICK=IMBUFF(12)
C     get colour number
      COLOR=IMBUFF(3)
      CALL IGWD02(I,THICK,COLOR)
C     get form number,offset to bring into iges range
      I=IMBUFF(5)+30
      CALL IGWD03(I)
C
C     initialize parameter block for entity
      CALL IGWP00(ENTNUM)
C
C     find number of pdf records used
C     get pdf pointer
      PDP=IMBUFF(7)
      CALL DBRCNT(PDP,NPDR,OK)
C     number of segments is 1 less
C     than number of pd records,due to
C     header record.
      N=NPDR-1
C     twice this number of coords
      N=N*2
C     write parameter data to buffer
C     write interpretation flag for xy pairs,common z
      CALL IGAIBF(1)
C     write number of coord pairs in list
      CALL IGAIBF(N)
C     write ZT constant
      CALL IGARBF(0.0)
C
C     write the coords to buffer in pairs
      IGNP=0
 10   CONTINUE
      P=IDBUFF(3)
C     avoid buffer overflow
      IF (BIGWP.GE.BIGSIZ) THEN
C       scavenge buffer
        IF (FIRST) THEN
          CALL IGWP12(IGDP,IGNP,IGPP)
          FIRST=.FALSE.
        ELSE
           CALL IGWP12(IGDP,IGNP,I)
        END IF
      END IF
C     read hatch vector
      CALL DBR500(P,OK)
C     write to parameter block
      CALL IGARBF(RDBUFF(1))
      CALL IGARBF(RDBUFF(2))
      CALL IGARBF(RDBUFF(4))
      CALL IGARBF(RDBUFF(5))
      IF (IDBUFF(3).GT.0) GOTO 10
C
C     add number pointers to associativities
      CALL IGAIBF(0)
C     add number pointers to properties
      CALL IGAIBF(0)
C     terminate parameter data
      CALL IGAEOR()
C     write parameter data to file
      N=0
      I=0
      IF (FIRST) THEN
        CALL IGWP10(IGDP,IGNP,IGPP)
        FIRST=.FALSE.
      ELSE
         CALL IGWP10(IGDP,I,N)
      END IF
C     I returns number of parameter records written
C     IGPP returns pointer to first parameter record
      IGNP=IGNP+I
C     now write directory entry
      CALL IGWD10(IGNP,IGPP)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGW085(IGDP)
C     =======================
C1    vartype            I4
C1    iostatus           I
C
C2    Subroutine IGW085 writes the TEXT currently
C2    in the buffer to the iges file structure.
C2    IGDP is the allocated directory entry
C2    Creates on iges GENERAL NOTE entity.
C
      include  'include/iges.inc'
      include  'include/nbuff.inc'
C
      INTEGER*4 ENTNUM,IGDP,I,IGPP,COLOR,THICK,FONT
      DATA  ENTNUM/212/
C
C     textdata currently in buffer
C     set entity number and label,and entity subscript
      CALL IGWD01(ENTNUM,'GEN_NOTE',TXTSUB)
C
C     set font,thick,colour here
C     get font number
      FONT=IMBUFF(6)
C     get thickness
      THICK=IMBUFF(12)
C     get colour number
      COLOR=IMBUFF(3)
      CALL IGWD02(FONT,THICK,COLOR)
C
C     initialize parameter block for entity
      CALL IGWP00(ENTNUM)
C
C     write parameter data to buffer
C     write number of text strings
      CALL IGAIBF(1)
      CALL IGW885(RDBUFF,FONT,CBUFF)
C     add number of chars in 2nd string
      CALL IGAIBF(0)
C
C     add number pointers to associativities
      CALL IGAIBF(0)
C     add number pointers to properties
      CALL IGAIBF(0)
C     terminate parameter data
      CALL IGAEOR()
C     write parameter data to file
      CALL IGWP10(IGDP,I,IGPP)
C     I returns number of parameter records written
C     IGPP returns pointer to first parameter record
C     now write directory entry
      CALL IGWD10(I,IGPP)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGW885(TPARMS,FONT,STRING)
C     =====================================
C1    vartype             R(6)  I4  I4 C*(*)
C1    iostatus              I   I   I    I
C
C2    Subroutine IGW885 adds text string parameters
C2    and the string to the parameter buffer.
C2    In the form used for iges GENERAL NOTE entity
C
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
C
      REAL TPARMS(6),SLA,X1,Y1,X2,Y2,RAD
      INTEGER*2 JST,NCHAR
      INTEGER*4 FONT,I
      CHARACTER*(*) STRING
      EXTERNAL UCODET,XYNORG,IGWP00,IGAIBF,IGARBF,RAD
C
C     find slant,no chars
C     decode the text params
      CALL UCODET(TPARMS(6),SLA,JST,NCHAR)
C
C     find bottom left of text
      CALL  XYNORG(RDBUFF(1),RDBUFF(2),JST,RDBUFF(5),
     +             SLA,NCHAR,RDBUFF(3),RDBUFF(4),X1,Y1)
C     find text enclosing box width and height
      X2=RDBUFF(3)*NCHAR*PAPTOW
      Y2=RDBUFF(4)*PAPTOW
C     find iges slant angle
      SLA=RAD(90-SLA)
C
C     write number of chars in string
      I=NCHAR
      CALL IGAIBF(I)
C     add enclosing box width
      CALL IGARBF(X2)
C     add enclosing box height
      CALL IGARBF(Y2)
C     add text font characteristic
      CALL IGAIBF(FONT)
C     add slant angle
      CALL IGARBF(SLA)
C     add base angle
      CALL IGARBF(RAD(RDBUFF(5)))
C     add mirror flag
      CALL IGAIBF(0)
C     add internal rotate flag
      CALL IGAIBF(0)
C     add text start point
      CALL IGARBF(X1)
      CALL IGARBF(Y1)
      CALL IGARBF(0.0)
C     add text string
      CALL IGAHBF(STRING)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWALL(OK)
C     =====================
C1    VARTYPE           L
C1    IOSTAT            O
C
C2    Subroutine IGWALL writes all entity data from the
C2    local database to the iges file structure already
C2    opened.OK returns true if the write is completed
C2    successfully.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 CURP,ENT
      INTEGER*4 IGDP
      LOGICAL EX,OP,OK,DELETE
      REAL M(3,3)
      CHARACTER*80 BUF
C
      DO 100 CURP=1,NMIPOS-1
        IF (MOD(CURP+0,50).EQ.0) THEN
C         tell him how progress is going
          BUF=' '
          WRITE(BUF,FMT='(I8,A)') CURP,' entities written'
          CALL CPRINT(BUF)
        END IF
        CALL ALLRD(CURP,ENT,M,DELETE)
        IF (.NOT.DELETE) THEN
          IF (ENT.EQ.LDIMN) THEN
C           write LINEAR DIMENSION to iges file
            CALL IGW033(ENT)
            GOTO 120
          ELSEIF (ENT.EQ.ADIMN) THEN
C           write ANGULAR DIMENSION to iges file
            CALL IGW033(ENT)
            GOTO 120
          ELSEIF (ENT.EQ.RDIMN) THEN
C           write ANGULAR DIMENSION to iges file
            CALL IGW033(ENT)
            GOTO 120
          ELSEIF (ENT.EQ.DDIMN) THEN
C           write ANGULAR DIMENSION to iges file
            CALL IGW033(ENT)
            GOTO 120
          ELSEIF (ENT.EQ.GLABEL) THEN
C           write ANGULAR DIMENSION to iges file
            CALL IGW033(ENT)
            GOTO 120
          ENDIF
C         initialize directory entry for entity other then dimension
          CALL IGWD00(IMBUFF,IGDP)
C         IGDP returned as iges directory entry allocated
          IF (ENT.EQ.LINE) THEN
C           write LINE to iges file
            CALL IGW003(IGDP)
          ELSE IF (ENT.EQ.ARC) THEN
C           write ARC to iges file
            CALL IGW005(IGDP)
          ELSE IF (ENT.EQ.HATCH) THEN
C           write HATCH to iges file
            CALL IGW031(IGDP)
          ELSE IF (ENT.EQ.TEXT) THEN
C           write TEXT to iges file
            CALL IGW085(IGDP)
          END IF
C
        END IF
 120  CONTINUE
 100  CONTINUE
C     tell him how progress is going
      BUF=' '
      WRITE(BUF,FMT='(A,I6,A)')'Total of',CURP-1,' entities written'
      CALL CPRINT(BUF)
      RETURN
 110  CONTINUE
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWD00(MIBUFF,IGDP)
C     ==============================
C1    vartype           I2(13)  I4
C1    iostatus            I     O
C
C2    Subroutine IGWD00 constructs an IGES directory record
C2    from DAXCAD MI data passed in MIBUFF
C2    into the next available IGES directory position
C2    returning the entry used in IGDP.
C
      include  'include/iges.inc'
C
      INTEGER*4 I,IGDP
      INTEGER*2 MIBUFF(13)
C
C     clear the directory record array
      DO 5 I=1,20
        IGDDAT(I)=0
 5    CONTINUE
C     clear entity label
      IGLABL=' '
C
C     enter the pointer to the next free parameter record
      IGDDAT(2)=IGPSEQ
C     enter the iges version number
      IGDDAT(3)=IGVERS
C     enter the layer number on which the entity exists
      IGDDAT(5)=MIBUFF(4)
C     return the directory pointer used
      IGDP=IGDSEQ
C     set sequence number
      IGDDAT(10)=IGDSEQ
      IGDDAT(20)=IGDDAT(10)+1
      IGDSEQ=IGDSEQ+2
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWD01(ENTNUM,ENTLAB,ENTSUB)
C     =======================================
C1    vartype             I4     C*8    I4
C1    iostatus            I       I     IO
C
C2    Subroutine IGWD03 modifies the IGES directory record
C2    under construction to contain the entity number
C2    and label passed,together with the entity subscipt
C2    passed in ENTSUB,this subscript is incremented before
C2    return.
C
      include  'include/iges.inc'
C
      INTEGER*4 ENTNUM,ENTSUB
      CHARACTER*8 ENTLAB
C
C     enter entity number
      IGDDAT(1)=ENTNUM
      IGDDAT(11)=ENTNUM
C     enter label
      IGLABL=ENTLAB
C     enter entity subscript
      IGDDAT(19)=ENTSUB
      ENTSUB=ENTSUB+1
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWD02(IGLFNT,IGLWGT,IGPENN)
C     =======================================
C1    vartype             I4     I4     I4
C1    iostatus            I       I     I
C
C2    Subroutine IGWD03 modifies the IGES directory record
C2    under construction to contain the line font number IGLFNT,
C2    line weight number IGLWGT,and pen number IGPENN.
C
      include  'include/iges.inc'
C
      INTEGER*4 IGLFNT,IGLWGT,IGPENN
C
C     enter line font number
      IGDDAT(4)=IGLFNT
C     enter line weight number
      IGDDAT(12)=IGLWGT
C     enter pen number
      IGDDAT(13)=IGPENN
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWD03(IFORM)
C     =========================
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine IGWD03 modifies the IGES directory record
C2    under construction to contain the form number IGFORM.
C
      include  'include/iges.inc'
C
      INTEGER*4 IFORM
C
C     enter the form number
      IGDDAT(15)=IFORM
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWD04(ST1,ST2,ST3,ST4)
C     ==================================
C1    vartype             I4 I4 I4   I4
C1    iostatus            I  I   I    I
C
C2    This routine sets the status value of the entity held in the
C2    directory entry in field 9 the values are as follows
C2    ST1  Blank status
C2         00 visible
C2         01 blankked
C2    ST2  subordinate entity switch
C2         00 independant
C2         01 physically dependant
C2         02 logically dependant
C2         03 both 01 and 02
C2    ST3  entity use flag
C2         00 geometry
C2         01 annotation
C2         02 definition
C2         03 other
C2         04 logical positional
C2         05 2D parametric
C2    ST4  Hierarchy
C2         00 global top down
C2         01 global defer
C2         02 use hierarchy property
C
      include  'include/iges.inc'
C
      INTEGER*4 ST1,ST2,ST3,ST4,STATUS,I
      CHARACTER*8 BUFF
C
      WRITE(UNIT=BUFF,FMT='(4I2)') ST1,ST2,ST3,ST4
D      write(10,*) '[igwd04] buff= ',buff
      DO 10 I=1,8
          IF(BUFF(I:I).EQ.CHAR(32)) BUFF(I:I)='0'
 10   CONTINUE
      READ(UNIT=BUFF,FMT='(I8)') STATUS
D      write(10,*) '[igwd04] status= ',status
      IGDDAT(9)=STATUS
 
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWD10(IGNPAR,IGPP)
C     ==============================
C1    vartype             I4    I4
C1    iostatus            I     I
C
C2    Subroutine IGWD10 modifies the DAXCAD mi record
C2    under construction to contain the number of
C2    parameter records used,and writes the content of
C2    the directory buffer to formatted output file.
C2    IGPP passes the pointer to the first parameter record
C
      include  'include/iges.inc'
C
      INTEGER*4 IGNPAR,I,IGPP
      CHARACTER*80 IGDBUF
C
C     enter the parameter pointer
      IGDDAT(2)=IGPP
C     enter the parameter count
      IGDDAT(14)=IGNPAR
C     write first section of record to file
      WRITE(UNIT=IGDBUF,FMT='(9I8,A1,I7)',ERR=99)
     +     (IGDDAT(I),I=1,9),'D',IGDDAT(10)
      WRITE(UNIT=IGUNIT,FMT='(A80)',ERR=99) IGDBUF
      IGDFP=IGDFP+1
C     write 2nd section of record to file
      IGDBUF=' '
      WRITE(UNIT=IGDBUF,FMT='(5I8)',ERR=99)
     +     (IGDDAT(I),I=11,15)
      IGDBUF(57:64)=IGLABL
      WRITE(UNIT=IGDBUF(65:),FMT='(I8,A1,I7)',ERR=99)
     +     IGDDAT(19),'D',IGDDAT(20)
      WRITE(UNIT=IGUNIT,FMT='(A80)',ERR=99) IGDBUF
      IGDFP=IGDFP+1
C
      RETURN
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[IGWD10] error writing rec:',IGDFP
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWDUN(OK)
C     =====================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine IGWDUN closes the iges translator
C2    output and work files.
C2    OK returned true if files closed properly
C
      include  'include/iges.inc'
C
      LOGICAL OK
      EXTERNAL CLOSUN
C
C     close the formatted iges output file
      CALL CLOSUN(IGUNIT,.TRUE.,OK)
      IF (.NOT.OK) THEN
        WRITE(UNIT=10,FMT=*)'[IGWDUN] cannot close iges file'
      END IF
C     close the scratch file for parameter data
      CALL CLOSUN(IGPUNT,.FALSE.,OK)
      IF (.NOT.OK) THEN
        WRITE(UNIT=10,FMT=*)'[IGWDUN] cannot close scratch file'
      END IF
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWGLS(OK)
C     =====================
C
C1    VARTYPE            L
C1    IOSTAT             O
C
C2    Subroutine IGWGLS writes the GLOBAL section to the
C2    IGES output file in formatted ascii form.
C
      include  'include/iges.inc'
      include  'include/params.inc'
      include  'include/ndata.inc'
      include  'include/wtov.inc'
      include  'include/dimendat.inc'
      include  'include/product.inc'
C
      INTEGER*4 TIMDAT(6)
      INTEGER*4 I,P1,P2,DNUM
      REAL R
      LOGICAL OK
      CHARACTER*80 BUFF,CDBU*4,CBUF*72
      EXTERNAL LOCALT,IGAHBF
C
C     clear the buffer first
      BIGBUF=' '
C     ensure default delimiters are interpreted
      BIGBUF=',,'
C     set pointer to next avail position
      BIGWP=3
C     add drawing name to buffer (hollerith format)
      CALL  IGAHBF(DRGNAM)
C     add iges file name to buffer (hollerith format)
      CALL  IGAHBF(IGFNAM)
C     add DAXCAD and version (hollerith format)
      CALL  IGAHBF(PRNAM)
C     add translator version (hollerith format)
      CALL  IGAHBF('IGES Translator Version 3.0')
C     add integer and float sizes
      CALL  IGADBF('32,9,23,12,52')
C     add drawing name to buffer (hollerith format)
C     use as target system identifier
      CALL  IGAHBF(DRGNAM)
C     add floating point scale
      CALL IGARBF(DRWSCL)
C     add unit flag (integer)
      IF (DBUNIT.EQ.'MM') THEN
C       mm database,iges flag 2
        I=2
        CDBU='MM'
      ELSE IF (DBUNIT.EQ.'CM') THEN
C       cm database,iges flag 10
        I=10
        CDBU='CM'
      ELSE IF (DBUNIT.EQ.'M') THEN
C       M database,iges flag 6
        I=6
        CDBU='M'
      ELSE IF (DBUNIT.EQ.'KM') THEN
C       Km database,iges flag 7
        I=7
        CDBU='KM'
      ELSE IF (DBUNIT.EQ.'IN') THEN
C       inch database,iges flag 1
        I=1
        CDBU='INCH'
      ELSE IF (DBUNIT.EQ.'FT') THEN
C       inch database,iges flag 4
        I=4
        CDBU='FT'
      ELSE IF (DBUNIT.EQ.'MI') THEN
C       inch database,iges flag 5
        I=5
        CDBU='MI'
      ELSE
C       non standard iges unit flag
        I=3
        CDBU=DBUNIT
      END IF
C
      CALL IGAIBF(I)
C     add units id (string)
      CALL IGAHBF(CDBU)
C     add maximum line weight number (integer)
C     set default to increments of .1 mm paper units
C     based on maximum thickness of 12mm paper units
      NIGTHK=12/0.1
      CALL IGAIBF(I)
C     add maximum line width (float)
C     must give max thickness in world units
      R=12*PAPTOW
C     save thickness increment
      THIKIG=R/NIGTHK
      CALL IGARBF(R)
C     add date-time of creation (string-13Hyymmdd.hhmmss)
      CALL LOCALT(TIMDAT)
      BUFF=' '
      WRITE(UNIT=BUFF,FMT='(3I2.2,A1,3I2.2)')
     +  TIMDAT(1)-1900,TIMDAT(2),TIMDAT(3),'.',TIMDAT(4),
     +  TIMDAT(5),TIMDAT(6)
      CALL IGAHBF(BUFF)
C     add minimum resolution in given units (float)
      CALL IGADBF('1E-7')
C     add maximum coord used in the model (float)
      CALL IGARBF(WPXMAX)
C     add name of transmitter of file (string)
      DNUM = 747
      CALL DPRMXP(DNUM,AUTHOR)
      CALL IGAHBF(AUTHOR)
C     add source organization (string)
      DNUM = 748
      CALL DPRMXP(DNUM,COMPNY)
      CALL IGAHBF(COMPNY)
C     add IGES version number code
      CALL IGADBF('4')
C     add Drafting standard code
      IF (BS) THEN
C       BS308 standard in use
        CALL IGADBF('4')
      ELSE IF (ANSI) THEN
C       ANSI standard in use
        CALL IGADBF('3')
      ELSE
C       DIN standard in use
        CALL IGADBF('6')
      END IF
C
C     set active buffer length to last working pos
      BIGBER=BIGWP-1
      BIGNL=BIGBER
      BIGBUF(BIGBER:BIGBER)=EOR
C     write global section to ascii file
C     multiple physical records to write
C     initialize working pointers
      P1=1
C     must split into 72 byte or nearest
C     complete field to 72 chars.
 10   CONTINUE
C     extract 72 byte or nearest
      CALL IGWP11(72,P1,P2)
C     P1,P2 bounds record
      CBUF=BIGBUF(P1:P2)
C     go write global sub_record to work file
      CALL  IGWOUT(CBUF,'G',OK)
C     set new position for pointer
      P1=P2+1
C     test for end of string
      IF (P1.LT.BIGNL) GOTO 10
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWINF(FNAM,OK)
C     ==========================
C1    vartype           C*(*) L
C1    iostatus            I   O
C
C2    Subroutine IGWINF initializes the iges translator
C2    scratch files for writing of an iges file.
C2    FNAM is the name of the iges output file
C2    OK returned true if files opened and ready
C
      include  'include/iges.inc'
C
      LOGICAL OK
      CHARACTER*(*) FNAM
      EXTERNAL OURSCR,OPNFFF
C
C     save iges filename
      IGFNAM=FNAM
C     open the formatted iges output file
      CALL OPNFFF(FNAM,IGUNIT,OK)
      IF (.NOT.OK) THEN
        WRITE(UNIT=10,FMT=*)'[IGWINF] cannot open:',FNAM
        RETURN
      END IF
C     open the scratch file for parameter data
      CALL OURSCR(IGPUNT,80,OK)
      IF (.NOT.OK) THEN
        WRITE(UNIT=10,FMT=*)'[IGWINF] cannot open scratch file'
      END IF
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWINI()
C     ===================
C1    vartype
C1    iostatus
C
C2    Subroutine IGWINI initializes the iges translator
C2    data block for writing of an iges file.
C
      include  'include/iges.inc'
C
      INTEGER*4 I
C
C     set IGES version number
      IGVERS=3
C     set pointers to start positions
      NIGDIP=1
      NIGPAP=1
      IGSSEQ=1
      IGGSEQ=1
      IGDSEQ=1
      IGPSEQ=1
      IGTSEQ=1
      LINSUB=1
      ARCSUB=1
      HATSUB=1
      TXTSUB=1
      IGDFP=1
      IGPFP=1
      BIGWP=1
      BIGNL=1
      BIGBER=1
C     clear entity label
      IGLABL=' '
C     clear directory buffer
      DO 5 I=1,20
        IGDDAT(I)=0
 5    CONTINUE
C
C     initialize delimiters
      EOV=','
      EOR=';'
C
C     clear buffer
      BIGBUF=' '
C     clear error count
      IGERRS=0
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWOUT(STRING,SECC,OK)
C     =================================
C1    VARTYPE             C72   C1  L
C1    IOSTAT              I     I   O
C
C2    Subroutine IGWOUT writes the string
C2    STRING to output file,appending section
C2    identifier SECC,and sequence number to
C2    the string.
C
      include  'include/iges.inc'
C
      INTEGER*4 I
      LOGICAL OK
      CHARACTER*72 STRING,SECC*1
C
      IF (SECC.EQ.'S') THEN
C       start section
        I=IGSSEQ
        IGSSEQ=IGSSEQ+1
      ELSE IF (SECC.EQ.'G') THEN
C       global section
        I=IGGSEQ
        IGGSEQ=IGGSEQ+1
      ELSE IF (SECC.EQ.'D') THEN
C       directory section
        I=IGDSEQ
        IGDSEQ=IGDSEQ+1
      ELSE IF (SECC.EQ.'P') THEN
C       parameter section
        I=IGPSEQ
        IGPSEQ=IGPSEQ+1
      ELSE IF (SECC.EQ.'T') THEN
C       terminate section
        I=IGTSEQ
        IGTSEQ=IGTSEQ+1
      ELSE
C       not a valid call
        WRITE(UNIT=10,FMT=*)'[IGWOUT] illegal call,type:',SECC
        OK=.FALSE.
        RETURN
      END IF
C     write record to output file
      WRITE(UNIT=IGUNIT,FMT='(A72,A1,I7)',ERR=99) STRING,SECC,I
      OK=.TRUE.
      RETURN
 99   CONTINUE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*)'[IGWOUT] error writing file'
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWP00(ENTNUM)
C     =========================
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine IGWP00 initializes the buffer for
C2    construction of a parameter record for the
C2    entity type passed in ENTNUM
C
      include  'include/iges.inc'
C
      INTEGER*4 ENTNUM
      EXTERNAL IGAIBF
C
      BIGBUF=' '
      BIGWP=1
C     add the entity number to buffer
      CALL IGAIBF(ENTNUM)
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWP10(IGDPP,IGNP,IGPP)
C     ==================================
C1    vartype             I4   I4   I4
C1    iostatus            I    O    O
C
C2    Subroutine IGWP10 writes the buffer for
C2    construction of a parameter record to the
C2    parameter scratch file.Returns the number
C2    of physical records used in IGNP.
C2    First parameter record number returned in IGPP
C
      include  'include/iges.inc'
C
      INTEGER*4 IGDPP,IGNP,P1,P2,IGPP
      LOGICAL OK
      CHARACTER*80 CBUF
C
C     set number of first record written
      IGPP=IGPSEQ
      IF (BIGNL.LE.64) THEN
C       single record output only
        CBUF=BIGBUF(1:BIGNL)
C       add back pointer to directory
        WRITE(UNIT=CBUF(65:72),FMT='(I8)',ERR=99) IGDPP
C       go write parameter record to work file
        CALL  IGWP20(CBUF,OK)
C       set physical record count to 1
        IGNP=1
      ELSE
C       multiple physical records to write
C       initialize record count
        IGNP=0
C       initialize working pointers
        P1=1
C       must split into 64 byte or nearest
C       complete field to 64 chars.
 10     CONTINUE
C       extract 64 byte or nearest
        CALL IGWP11(64,P1,P2)
C       P1,P2 bounds record
        CBUF=BIGBUF(P1:P2)
C       add back pointer to directory
        WRITE(UNIT=CBUF(65:72),FMT='(I8)',ERR=99) IGDPP
C       go write parameter record to work file
        CALL  IGWP20(CBUF,OK)
C       increment record count
        IGNP=IGNP+1
C       set new position for pointer
        P1=P2+1
C       test for end of string
        IF (P1.LT.BIGNL) GOTO 10
      END IF
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWP11(SRL,P1,P2)
C     ============================
C1    vartype           I4 I4  I4
C1    iostatus          I  I   O
C
C2    Subroutine IGWP11 extracts from the buffer
C2    the next SRL bytes,or to the nearest complete
C2    field under SRL bytes,starting from position
C2    P1 in BIGBUF.The end position of the sub-record
C2    is returned in P2.
C
      include  'include/iges.inc'
C
      INTEGER*4 SRL,P1,P2,J,BINDEX
      EXTERNAL BINDEX
C
C     try for full SRL bytes
      P2=P1+SRL-1
      IF (P2.GE.BIGBER) THEN
C       limit to end of record
        P2=BIGBER
      ELSE
C       have to test for coincident delimiter
        IF (BIGBUF(P2:P2).NE.EOR) THEN
C         have to search for nearest
C         field limit to break sub_record on
          J=BINDEX(BIGBUF(P1:P2),EOV)
          IF (J.GT.0) THEN
C           found delimiter,use this as break point
            P2=P1+J-1
          END IF
C         get here with P2 modified,or P2
C         in middle of text string
        END IF
C       now have P2 set to break point in string
      END IF
C
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWP12(IGDPP,IGNP,IGPP)
C     ==================================
C1    vartype             I4   I4   I4
C1    iostatus            I    O    O
C
C2    Subroutine IGWP12 writes the buffer for
C2    construction of a parameter record to the
C2    parameter scratch file.Returns the number
C2    of physical records used in IGNP.
C2    First parameter record number returned in IGPP
C
      include  'include/iges.inc'
C
      INTEGER*4 IGDPP,IGNP,IGPP,IN
      LOGICAL OK
      CHARACTER*80 CBUF
C
C     set end of buffer to last char
      BIGNL=BIGWP-1
      BIGBER=BIGNL
C     go write buffer
      CALL IGWP10(IGDPP,IN,IGPP)
C     update total parameter count
      IGNP=IGNP+IN
C     reset buffer pointers
      BIGWP=1
      BIGBUF=' '
      BIGNL=1
      BIGBER=1
C
 99   CONTINUE
      END
C
C     -----------------------------------------------
C
      SUBROUTINE IGWP20(STRING,OK)
C     ============================
C1    VARTYPE             C80   L
C1    IOSTAT              I     O
C
C2    Subroutine IGWP20 writes the string
C2    STRING to parameter file,appending section
C2    identifier ,and sequence number to
C2    the string.
C
      include  'include/iges.inc'
C
      INTEGER*4 I
      LOGICAL OK
      CHARACTER*80 STRING,SECC*1
C
C     parameter section
      I=IGPSEQ
      IGPSEQ=IGPSEQ+1
      SECC='P'
C     append sequence number to string
      WRITE(UNIT=STRING(73:80),FMT='(A1,I7)',ERR=99) SECC,I
C     write record to output file
      WRITE(UNIT=IGPUNT,REC=I,ERR=98) STRING
      OK=.TRUE.
      RETURN
 99   CONTINUE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*)'[IGWP20] error writing string'
      RETURN
 98   CONTINUE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*)'[IGWP20] error writing file'
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWP99(OK)
C     =====================
C
C1    VARTYPE            L
C1    IOSTAT             O
C2    This routine copies the parameter section
C2    to the ASCII IGES file
C
      include  'include/iges.inc'
C
      INTEGER*4 I
      LOGICAL OK
      CHARACTER*80 STRING
C
      DO 10 I=1,IGPSEQ-1
C       read from parameter scratch file
        READ(UNIT=IGPUNT,REC=I,ERR=99) STRING
C       write to formatted file
        WRITE(UNIT=IGUNIT,FMT='(A80)',ERR=99) STRING
 10   CONTINUE
C
      OK=.TRUE.
      RETURN
 99   CONTINUE
      OK=.FALSE.
      WRITE(UNIT=10,FMT=*)'[IGWP99] error writing file'
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWSTS(OK)
C     =====================
C
C1    VARTYPE            L
C1    IOSTAT             O
C
C2    Subroutine IGWSTS writes the START section to the
C2    IGES output file in formatted ascii form.
C
      include  'include/iges.inc'
      include  'include/product.inc'
C
      INTEGER*4 NLEN,L,DNUM
      LOGICAL OK
      CHARACTER*72 STRING
      EXTERNAL IGWOUT,NLEN
C
      STRING=PRNAM//' by Practical Technology Ltd.'
      CALL IGWOUT(STRING,'S',OK)
      STRING='IGES V3.0 Translator.'
      CALL IGWOUT(STRING,'S',OK)
      STRING='User defined prologue follows:'
      CALL IGWOUT(STRING,'S',OK)
C     mark prologue block
      STRING='*********************************************************'
      CALL IGWOUT(STRING,'S',OK)
C     Ask user for prologue
 10   CONTINUE
      STRING=' '      
      DNUM = 771 
      CALL DPRMXP(DNUM,STRING)
      IF (NLEN(STRING).GT.0) THEN
C       write to output file start section
        CALL IGWOUT(STRING,'S',OK)
        GOTO 10
      END IF
C     terminate prologue
      STRING='*********************************************************'
      CALL IGWOUT(STRING,'S',OK)
C
      END
C
C     -------------------------------------------------
C
      SUBROUTINE IGWTMS(OK)
C     =====================
C
C1    VARTYPE            L
C1    IOSTAT             O
C2    This routne writes the terminate section to the IGES file
C
      include  'include/iges.inc'
C
      LOGICAL OK
      CHARACTER*72 STRING
      EXTERNAL IGWOUT
C
      STRING=' '
C     write section counters to output file
      WRITE(UNIT=STRING,FMT=10,ERR=99)
     +      IGSSEQ-1,IGGSEQ-1,IGDSEQ-1,IGPSEQ-1
10    FORMAT('S',I7,'G',I7,'D',I7,'P',I7)
      CALL IGWOUT(STRING,'T',OK)
      IF (.NOT.OK) THEN
         WRITE(UNIT=10,FMT=*)'[IGWTMS] error writing file'
      END IF
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[IGWTMS] error writing STRING'
      OK=.FALSE.
C
      END
C
C     -------------------------------------------------
C
