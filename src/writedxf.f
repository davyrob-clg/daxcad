C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 writedxf.f   */
C
      SUBROUTINE DXF_BUILD_HATCH(OK)
C     ==============================
C1    VARYPE                     L
C1    IOSTAT                     O
C
C2    Builds a list of the hatch elements in
C2    the datbase and sets up a lookup
C2    for each hatch
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      LOGICAL OK
      INTEGER*2 TMIP
      INTEGER*2 ENT
      INTEGER*2 TPDP
      INTEGER*2 STATUS
C
      NOH = 0
      TMIP = 1
C     Get the masters
10    CALL  DIR500(TMIP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      IF((STATUS.EQ.10.OR.
     +   STATUS.EQ.COMPM.OR.
     +   STATUS.EQ.SYMBM.OR.
     +   STATUS.EQ.GROUP).AND.(ENT.EQ.HATCH)) THEN
C
          NOH = NOH + 1
          TPDP = IMBUFF(7)
C         store hatching number
          CALL DBR500(TPDP,OK)
          IDBUFF(4) = NOH
          CALL DBM500(TPDP,OK)
      ENDIF
C
      TMIP = TMIP + 1
      IF(TMIP.LT.NMIPOS) THEN
C         loop back round
          GOTO 10
      ENDIF
100   CONTINUE
C
      END
C
C
C
      SUBROUTINE DXF_RESET_HATCH(OK)
C     ==============================
C1    VARYPE                     L
C1    IOSTAT                     O
C
C2    resets hatching list
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      LOGICAL OK
      INTEGER*2 TMIP
      INTEGER*2 ENT
      INTEGER*2 STATUS
      INTEGER*2 TPDP
C
      NOH = 0
      TMIP = 1
C     Get the masters
10    CALL  DIR500(TMIP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      IF((STATUS.EQ.10.OR.
     +   STATUS.EQ.COMPM.OR.
     +   STATUS.EQ.SYMBM.OR.
     +   STATUS.EQ.GROUP).AND.(ENT.EQ.HATCH)) THEN
C
          TPDP = IMBUFF(7)
C         store hatching number
          CALL DBR500(TPDP,OK)
          IDBUFF(4) = 0
          CALL DBM500(TPDP,OK)
      ENDIF
 
      TMIP = TMIP + 1
      IF(TMIP.LT.NMIPOS) THEN
C         loop back round
          GOTO 10
      ENDIF
100   CONTINUE
C
      END
      SUBROUTINE DXFBUILDHATCH(OK)
C     ==============================
C1    VARYPE                     L
C1    IOSTAT                     O
C
C2    Builds a list of the hatch elements in
C2    the datbase and sets up a lookup
C2    for each hatch
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      LOGICAL OK
      INTEGER*2 TMIP
      INTEGER*2 ENT
      INTEGER*2 TPDP
      INTEGER*2 STATUS
C
      NOH = 0
      TMIP = 1
C     Get the masters
10    CALL  DIR500(TMIP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      IF((STATUS.EQ.10.OR.
     +   STATUS.EQ.COMPM.OR.
     +   STATUS.EQ.SYMBM.OR.
     +   STATUS.EQ.GROUP).AND.(ENT.EQ.HATCH)) THEN
C
          NOH = NOH + 1
          TPDP = IMBUFF(7)
C         store hatching number
          CALL DBR500(TPDP,OK)
          IDBUFF(4) = NOH
          CALL DBM500(TPDP,OK)
      ENDIF
C
      TMIP = TMIP + 1
      IF(TMIP.LT.NMIPOS) THEN
C         loop back round
          GOTO 10
      ENDIF
100   CONTINUE
C
      END
C
C
C
      SUBROUTINE DXFDIM(OUNIT,CURP)
C     =============================
C1    vartype               I2  L
C1    iostatus              I   O
C
C2    Subroutine DRWDIM is a general routine for drawing of
C2    any  dimension type. For an explanation as to the other routines
C2    used here see Charlie W.
C
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/masti.inc'
      include   'include/dxf.inc'
C
      REAL PDAT(2,2)
      INTEGER*2 CURP
      INTEGER*4 I,K,OUNIT
      LOGICAL OK
      EXTERNAL DRWTXT,DRAWLW,AROHED,ARCCT,DERDIM
C
      CALL DERDIM(CURP,OK)
      IF(.NOT.OK) RETURN
      OK=.FALSE.
C     set local counter for text
      K=0
C     Loop through the stored dimension records.
      DO 101 I=1,RECCNT(1)
C     Check supression state of sub-record entity
         IF ( IWORK(4,I) .GE. 0 ) THEN
C        was not surpressed so continue
            IF ( IWORK(1,I) .EQ. TEXSEG ) THEN
C              was a text record so use text buffer as well
               K=K+1
               CALL DXFWT2(OUNIT,RWORK(1,I),RWORK(2,I),RWORK(3,I),
     +                     RWORK(4,I),RWORK(5,I),RWORK(6,I),DIMCHR(K))
            ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
C              was arrowhead so must decode the arrowhead parameters.
               CALL AROHED(RWORK(1,I),RWORK(2,I),PAPTOW*RWORK(5,I),
     +                     PAPTOW*RWORK(6,I),RWORK(3,I),RWORK(4,I),
     1                     PDAT(1,1),PDAT(2,1),PDAT(1,2),PDAT(2,2))
               CALL DXFWL2(OUNIT,PDAT(1,1),PDAT(2,1),
     +                     PDAT(1,2),PDAT(2,2))
               CALL DXFWL2(OUNIT,RWORK(1,I),RWORK(2,I),
     +                     PDAT(1,2),PDAT(2,2))
               CALL DXFWL2(OUNIT,RWORK(1,I),RWORK(2,I),
     +                     PDAT(1,1),PDAT(2,1))
            ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
C              Was a line so use data direct
               CALL DXFWL2(OUNIT,RWORK(1,I),RWORK(2,I),
     +                     RWORK(4,I),RWORK(5,I))
            ELSE IF ( IWORK(1,I) .EQ. ARCSEG ) THEN
C              Arc found so draw direct.
C              was an arc type segment
               CALL DXFWA2(OUNIT,RWORK(1,I),RWORK(2,I),RWORK(4,I),
     +                               RWORK(5,I),RWORK(6,I))
C           ELSE
C              Unrecognised subrecord type.
C              WRITE(10,*)'[DRWDIM] Non Geometric Sub-Record ',IWORK(1,I)
            END IF
         END IF
 101  CONTINUE
      END
C
      SUBROUTINE DXFILTER(NAME,ST)
C     ============================
C1    VARYPE             C*(*) I4
C1    IOSTAT             I/O   O
C
C2    Filters name for any unwanted characters that ACAD will not recognise.
C2    ONly ascii values are permitted in names. That is this range
C2
C2         0-9 a-z A-Z
C2
C2    Arguments:-
C2
C2    NAME        ->          The name in character string
C2
C2
C2
C2    Error Returns:
C2
C2    1       ->              The length of the string is of 0 length
C2    2       ->              name was full of illegal characters
C2
C2
C
C
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 I
      INTEGER*4 C
      CHARACTER*(*) NAME
      CHARACTER*256 TEMP
      CHARACTER CH
C
      ST = 0
C
      TEMP = ' '
      LENGTH = LEN(NAME)
      IF (LENGTH .EQ. 0 ) THEN
          ST = 1
          GOTO 999
      ENDIF
C
C     temp counter for output.
      C = 0
      DO 10 I = 1,LENGTH
          CH = NAME(I:I)
          IF ( (LGE(CH,'a') .AND. LLE(CH,'z')) .OR.
     +         (LGE(CH,'A') .AND. LLE(CH,'Z')) .OR.
     +         (LGE(CH,'0') .AND. LLE(CH,'9'))) THEN
              C = C + 1
              TEMP(C:C) = CH
          ENDIF
10    CONTINUE
C
 
      IF ( C .EQ. 0 ) THEN
          ST = 2
          GOTO 999
      ELSE
          NAME = TEMP
      ENDIF
 
999   CONTINUE
      END
 
 
      SUBROUTINE DXFLINESTYLE(OUNIT,OK)
C     =================================
C1    VARYPE                  I4    L
C1    IOSTAT                   I    O
C
C2    This sets up a DXF table for our line styles
C2    We will be able to read them back into
C2    daxcad. The units used are in mm. Thus
C2    drawings in Autocad may seem to have small
C2    patterns since the units are not paper based.
C
      include  'include/product.inc'
      include  'include/library.inc'
      include  'include/lfont.inc'
      include  'include/dxf.inc'
C
      CHARACTER*80 TEMP
      CHARACTER*80 BUFF
      LOGICAL EX
      LOGICAL OK
      INTEGER*4 NLEN
      INTEGER*4 LUNIT
      INTEGER*4 OUNIT
      INTEGER*4 LENGTH
      INTEGER*4 FIELD(30)
      INTEGER*4 FN
      INTEGER*4 I
      INTEGER*4 NEG
      INTEGER*4 COUNT
      REAL PATTERN(20)
      REAL VALUE
      REAL TOTAL
C
      EXTERNAL NLEN
C
CAPOLLO
      TEMP='line.style'
CAPOLLO
CSUN
C       TEMP=HOME(1:NLEN(HOME))//'/.style.pcf'
CSUN
CIBM|PC386
C      TEMP='style.pcf'
D      CALL FPARS(TEMP)
CIBM|PC386
      INQUIRE(FILE=TEMP,EXIST=EX)
      IF(.NOT.EX) THEN
CAPOLLO
         TEMP=LIBRARY(1:NLEN(LIBRARY))//'/line.style'
CAPOLLO
CSUN
C	  TEMP=LIBRARY(1:NLEN(LIBRARY))//'/style.pcf'
CSUN
CIBM
CC         TEMP='\'//PRNAM(1:NLEN(PRNAM))//'\style.dax'
CIBM
          INQUIRE(FILE=TEMP,EXIST=EX)
          IF(.NOT.EX) THEN
C             cannot load system file for line styles
              OK = .FALSE.
              GOTO 999
          ENDIF
      ENDIF
C     temp now contains the file to load
C
C     get a unit to open
      CALL FINDU1(LUNIT,OK)
      IF(.NOT.OK) THEN
          GOTO 999
      ENDIF
C
      OPEN(UNIT=LUNIT,
     +     FILE=TEMP,
     +     STATUS='OLD',
     +     ACCESS='SEQUENTIAL',
     +     ERR=998)
C
C     write out header defintions
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'TABLE'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'LTYPE'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(I3)',ERR=999) NFONTS
C
C
      COUNT = 1
100   CONTINUE
C
      READ(UNIT=LUNIT,FMT='(A)',END=200) BUFF
C     set length
      LENGTH = NLEN(BUFF)
C     if length is 0 then continue
      IF(LENGTH.EQ.0) THEN
          GOTO 100
      ENDIF
C     check for comments
      IF(BUFF(1:1) .EQ. '*') THEN
          GOTO 100
      ENDIF
C     parse the line into the fields
      CALL PARSES(BUFF,FIELD,FN)
C     build name of line style
      TEMP = PRODUCT(1:NLEN(PRODUCT))//BUFF(1:FIELD(1))
C     take out quotes
      CALL SUBSTC(TEMP,'"',' ')
C     crunch the string
      CALL CRUNCH(TEMP)
      CALL FOLDUP(TEMP)
C
C     Header of the line type table
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'LTYPE'
C     name of line that will be used in AUTOCAD
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) TEMP(1:NLEN(TEMP))
C     save the name for writing out for each entity
      LINESN(COUNT) = TEMP
C     special flag
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '   64'
C     prose of the line must be present
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '3'
      WRITE(UNIT=OUNIT,FMT='(2A)',ERR=999) 'Daxcad line ',
     +      BUFF(1:NLEN(BUFF))
C     Alignment code
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '72'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '   65'
C     now do the clever bit
      TOTAL = 0
      DO 10 I=1,FN-1
          CALL RVALU(BUFF( FIELD(I) :FIELD(I+1)),VALUE,OK  )
          TOTAL= TOTAL+VALUE
          PATTERN(I) = VALUE
10    CONTINUE
      IF(FN.GT.2) THEN
C         number of dash items
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '73'
          WRITE(UNIT=OUNIT,FMT='(I3)',ERR=999)  FN-1
C         total pattern length
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
          WRITE(UNIT=OUNIT,FMT='(G10.5)',ERR=999)  TOTAL
      ELSE
C         number of dash items
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '73'
          WRITE(UNIT=OUNIT,FMT='(I3)',ERR=999)  0
C         use the sold but make it a continuous
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
          WRITE(UNIT=OUNIT,FMT='(G10.5)',ERR=999)  0.0
          FN = 1
      ENDIF
C     loop round and write out dash segments
      NEG = 1
      DO 20 I = 1,FN-1
C         group and dash lengh items
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '49'
          WRITE(UNIT=OUNIT,FMT='(G10.4)',ERR=999) PATTERN(I)*NEG
          NEG = -NEG
20    CONTINUE
C
      COUNT = COUNT + 1
      GOTO 100
C
200   CONTINUE
C
C     close the unit for the pcf file
      CLOSE(UNIT=LUNIT)
C     end of table markers
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'ENDTAB'
      OK = .TRUE.
      RETURN
C
998   CONTINUE
C     open error on the file
      OK = .FALSE.
999   CONTINUE
C
      END
C
      SUBROUTINE DXFMRKINSTANCE(OUNIT,OK)
C     ====================================
C1    VARYPE                      I4   L
C1    IOSTAT                      I    O
C
C2    Writes out a marker as an instance of the
C2    current loaded block table. The marker number
C2    is held in the master index code as form (5)
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
      include 'include/product.inc'
      include 'include/marker.inc'
C
      INTEGER*4 OUNIT
      INTEGER*4 NUM
      INTEGER*4 NLEN
      LOGICAL OK
C
      EXTERNAL NLEN
C
C     write out header information
C
C     get marker number
      NUM = IMBUFF(5)
C     produce a name for this one
      WRITE(UNIT=CBUFF,FMT='(A,I3,A)',ERR=999)
     +                      PRODUCT,
     +                      NUM,
     +                      'MARKER'
      CALL CRUNCH(CBUFF)
      CALL FOLDUP(CBUFF)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'INSERT'
C     layer number
      CALL DXFWLN(OUNIT)
C     now write oot the name
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
C     block name will be marker number
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) CBUFF(1:NLEN(CBUFF))
C
C     write out all tranform values. No need to transform
C     as miles has done that for us allready
C
C     Origin of marker
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) RDBUFF(1)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) RDBUFF(2)
C
C     X scaling factor
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) RDBUFF(5)
C
C     Y scaling factor
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) RDBUFF(6)
C
C     degrees rotation
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '50'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) RDBUFF(4)
C     all ok return back
      OK = .TRUE.
      RETURN
999   CONTINUE
C     error from fortran write
C
      END
C
      SUBROUTINE DXFMRKWRITE(OUNIT,OK)
C     =================================
C1    VARYPE                    I4   L
C1    IOSTAT                    I    O
C
C2    Writes out the marker defintion table currently
C2    defined within DAXCAD. Each marker is a block
C2    defintion. All entries are written.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
      include 'include/marker.inc'
C
C     Parameter defintions for marker codes
      INTEGER*4 MRKSEG
      INTEGER*4 MRKARC
      INTEGER*4 MRKPAR
      INTEGER*4 MRKMOV
C
      PARAMETER ( MRKSEG = 1 )
      PARAMETER ( MRKARC = 5 )
      PARAMETER ( MRKMOV = 0 )
      PARAMETER ( MRKPAR = 99 )
C
      INTEGER*4 OUNIT
      INTEGER*4 MARKER_NUM
      INTEGER*4 MARKER_START
      INTEGER*4 MARK
      INTEGER*4 MARCOL
      INTEGER*4 MARLAY
      INTEGER*4 I
      INTEGER*4 TLAY
      REAL SX1
      REAL SX2
      REAL SY1
      REAL SY2
      REAL RAD
      REAL CIRDIF
      REAL ARCN
      LOGICAL OK
      LOGICAL SAMEA
C
      EXTERNAL RAD
      EXTERNAL SAMEA
C
C     Block header information
C     loop round to get marker data.
C
      IMBUFF(12) = 0
      IMBUFF(6) = 0
      IMBUFF(4) = 1
      IMBUFF(3) = 7
C
      DO 10 MARK = 1,MRKIMX
C
          MARKER_START = MRKIND(MARK)
          IF(MARKER_START.GT.0) THEN
C             write out header
              CALL DXFMRKBLOCK(OUNIT,MARK,OK)
C             ok a marker has been defined write it out
C             jump past limits of marker
              MARKER_NUM = MARKER_START + 2
C             main loop
20            CONTINUE
              IF( MRKI(MARKER_NUM).EQ.MRKSEG) THEN
C                 single segments are used here.
                  RDBUFF(1) = MRKR(MARKER_NUM-1,1)
                  RDBUFF(2) = MRKR(MARKER_NUM-1,2)
                  RDBUFF(4) = MRKR(MARKER_NUM,1)
                  RDBUFF(5) = MRKR(MARKER_NUM,2)
C                 write out a line segment
                  CALL DXFWRL(OUNIT)
              ELSEIF( MRKI(MARKER_NUM).EQ.MRKARC) THEN
C                 arc centre
                  RDBUFF(1) = MRKR(MARKER_NUM,1)
                  RDBUFF(2) = MRKR(MARKER_NUM,2)
C                 arc radius
                  MARKER_NUM=MARKER_NUM + 1
                  RDBUFF(4) = MRKR(MARKER_NUM,1)
C                 arc thickness
                  CIRDIF = MRKR(MARKER_NUM,2)
C                 angles
                  MARKER_NUM=MARKER_NUM + 1
                  RDBUFF(5) = RAD(MRKR(MARKER_NUM,1))
                  RDBUFF(6) = RAD(MRKR(MARKER_NUM,2))
C                 number of arcs to be written
                  IF(SAMEA(CIRDIF,0.0) ) THEN
                      ARCN = 1
                  ELSE
                      ARCN = RDBUFF(4)/CIRDIF
                  ENDIF
                  DO 30 I=1,INT(ARCN)
C                     ok write out the arc
                      CALL DXFWRA(OUNIT)
C                     decrement radius
                      RDBUFF(4) = RDBUFF(4) - CIRDIF
 30               CONTINUE
C                 skip the segments allready in the arc
                  MARKER_NUM = MARKER_NUM+(MRKI(MARKER_NUM)+1)*ARCN
C
              ENDIF
              IF(MARKER_NUM.LT.MARKER_START+MRKI(MARKER_START) ) THEN
C                 increment counter to next record
                  MARKER_NUM = MARKER_NUM + 1
                  GOTO 20
              ENDIF
C             write out end of block
              WRITE(UNIT=OUNIT,FMT='(A)') '0'
              WRITE(UNIT=OUNIT,FMT='(A)') 'ENDBLK'
          ENDIF
C         marker not found here
 10   CONTINUE
 
      END
 
 
      SUBROUTINE DXFRESETHATCH(OK)
C     ==============================
C1    VARYPE                     L
C1    IOSTAT                     O
C
C2    resets hatching list
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      LOGICAL OK
      INTEGER*2 TMIP
      INTEGER*2 ENT
      INTEGER*2 STATUS
      INTEGER*2 TPDP
C
      NOH = 0
      TMIP = 1
C     Get the masters
10    CALL  DIR500(TMIP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      IF((STATUS.EQ.10.OR.
     +   STATUS.EQ.COMPM.OR.
     +   STATUS.EQ.SYMBM.OR.
     +   STATUS.EQ.GROUP).AND.(ENT.EQ.HATCH)) THEN
C
          TPDP = IMBUFF(7)
C         store hatching number
          CALL DBR500(TPDP,OK)
          IDBUFF(4) = 0
          CALL DBM500(TPDP,OK)
      ENDIF
 
      TMIP = TMIP + 1
      IF(TMIP.LT.NMIPOS) THEN
C         loop back round
          GOTO 10
      ENDIF
100   CONTINUE
C
      END
 
      SUBROUTINE DXFWA2(OUNIT,X1,Y1,RAD,ANGS,ANGF)
C     ============================================
C1    VARTYPE            I4   R  R  R  R  R   R
C1    IOSTATS            I    I  I  I  I  I   I
C
C     This routine is an intermentiate control routine for the
C     dimension routine since it is completely diferent from
C     the rest. It simply sets up the incoming arguments into
C     the buffered part data array RBUFF
C     It should cope with an arc or circle
C
      include   'include/nbuff.inc'
      include 'include/dxf.inc'
C
      REAL X1,Y1,RAD,ANGS,ANGF
      INTEGER*4 OUNIT
      RDBUFF(1)=X1
      RDBUFF(2)=Y1
      RDBUFF(4)=RAD
      RDBUFF(5)=ANGS
      RDBUFF(6)=ANGF
      CALL DXFWRA(OUNIT)
      END
 
      SUBROUTINE DXFWBK(OUNIT,OK)
C     ===========================
C1    VARTYPE            I4   L
C1    IOSTATUS           I    O
C
C2    This routine produces the file formats for
C2    BLOCKs. They consist of component masters
C2    component instances (symbols also)
C2    The hatches are written out as blocks (as they are in
C2    AutoCAD ) with a header to indicate an anoymous block
C2    and the standard name ( *X increment )
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      REAL M(3,3)
      INTEGER*4 OUNIT,I
      INTEGER*2 CURP,ENT,STATUS
      LOGICAL OK,DELETE
      OK=.TRUE.
      NOG=0
      CURP=1
C     Writes the section header to file
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'SECTION'
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=OUNIT,FMT='(A)') 'BLOCKS'
C     write out all marker information
      CALL DXFMRKWRITE(OUNIT,OK)
C     Get the masters
10    CALL  DIR500(CURP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      CALL ALLRD(CURP,ENT,M,DELETE)
      IF(STATUS.EQ.10) THEN
          IF (ENT.EQ.COMPM) THEN
              ENT=IMBUFF(9)
              CALL DTR500(ENT,OK)
              CALL DXFWCM(OUNIT,OK)
          ELSEIF (ENT.EQ.SYMBM) THEN
              ENT=IMBUFF(9)
              CALL DTR500(ENT,OK)
              CALL DXFWCM(OUNIT,OK)
          ELSEIF (ENT.EQ.GROUP) THEN
              CALL DXFWCM(OUNIT,OK)
          ELSEIF (ENT.EQ.HATCH) THEN
C             could be a hatch in a nest or normal
              CALL DXFWHC(OUNIT,OK)
          ENDIF
      ELSEIF(STATUS.EQ.10.OR.STATUS.EQ.COMPM.OR.
     +      STATUS.EQ.SYMBM.OR.STATUS.EQ.GROUP) THEN
          IF (ENT.EQ.HATCH) THEN
C             could be a hatch in a nest or normal
              CALL DXFWHC(OUNIT,OK)
          ENDIF
      ENDIF
      IF(.NOT.OK) THEN
C         some error here
          GOTO 100
      ENDIF
      CURP=CURP+1
      IF(CURP.LT.NMIPOS) GOTO 10
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDSEC'
      OK=.TRUE.
      RETURN
100   OK=.FALSE.
      END
 
 
      SUBROUTINE DXFWCM(OUNIT,OK)
C     ===========================
C1    VARTYPE            I4   L
C1    IOSTATUS            I   O
C
C2    This rouitine writes out the component master information in the
C2    block section of the DXF file. It writes out all the
C2    entity informatin asscociated with the component including
C2    nested instances.
C
      include 'include/masti.inc'
      include 'include/product.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/compd.inc'
      include 'include/wrkdat.inc'
      include 'include/wtov.inc'
      include 'include/dxf.inc'
      include 'include/wildc.inc'
C
      REAL M(3,3)
      CHARACTER*100 TEMP
      CHARACTER*80 NAME
      INTEGER*4 I,J,II,JJ,KK,NLEN,STATUS
      INTEGER*2 ENT,TTMIP,ZERO,TLAY
      LOGICAL OK,DELETE,L0,L1,L2,TOTAL,NOFIND,L3,L4
      INTEGER*4 OUNIT
      INTEGER*4 BINDEX
      INTEGER*4 POS
      INTEGER*4 POINT
      INTEGER*4 TMIP
      INTEGER*4 ST
C
      EXTERNAL NLEN
      EXTERNAL BINDEX
C
C     if this is a group then write in a new name for the instance
C     and master
      POS = 1
      IF(IMBUFF(2).EQ.GROUP) THEN
          NOG=NOG+1
          WRITE(UNIT=TEMP,FMT='(2A,I4)')
     +             PRODUCT(1:NLEN(PRODUCT)),
     +             'GROUP',
     +             NOG
      ELSE
C         re-format the name into daxcad specific names. no that name will be ov
          POS = BINDEX(CBUFF,FILCHR)+1
          POINT = BINDEX(CBUFF,'.')
 
C         look for sym or cmp extension.
          IF(POINT.LE.1) THEN
C             strip .cmp or .sym name
              POINT = NLEN(CBUFF)
          ELSE
C             increment past point
              POINT = POINT - 1
          ENDIF
C
          IF(POINT.LT.POS) THEN
              WRITE(10,*) '[DXFWCM] Error in name ',CBUFF
C             duff name
              GOTO 999
          ENDIF
C         set the name of the component
          NAME = CBUFF(POS:POINT)
          CALL SUBSTC(NAME,'.',' ')
          CALL CRUNCH(NAME)
          IF(NLEN(NAME).EQ.0) THEN
              WRITE(10,*) '[DXFWCM] Error in name ',CBUFF
C             duff name
              GOTO 999
          ENDIF
C
C         save current mip for unique ID
C
          TMIP = MIP
          CALL DXFILTER(NAME,ST)
          IF ( ST.EQ.2 ) THEN
C             set this to a value which is still unique but indicates an error
              NAME = 'ILLEGALNAME'
          ENDIF
          WRITE(UNIT=TEMP,FMT='(A,I5,A)',ERR=999)
     +             PRODUCT(1:NLEN(PRODUCT)),
     +             MIP,
     +             NAME(1:NLEN(NAME))
      ENDIF
C     cruch and fold to upper case this one
      CALL CRUNCH(TEMP)
      CALL FOLDUP(TEMP)
C
      NOFIND=.FALSE.
      ZERO=0
C     find relation header from instance data
      RELP=IMBUFF(10)
C     if null relation pointer,then unresolved instance
      IF (RELP.LE.ZERO) then
C        write to diag
         WRITE(UNIT=10,FMT=*)
     +   '*** Unresolved COMPONENT instance [DXFWCM]',CBUFF
         NOFIND=.TRUE.
      END IF
      IF (NOFIND) THEN
          GOTO 999
      ENDIF
C     read the relation header
      CALL DRR950(RELP,OK)
C     test for valid component relation
      IF (RLBUFF(1).NE.COMPM.AND.RLBUFF(1).NE.SYMBM.AND.RLBUFF(1).
     +    NE.GROUP) THEN
         GOTO 999
      END IF
C     Block header information
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'BLOCK'
C     layer number of this block
      CALL DXFWLN(OUNIT)
C     Block name
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(OUNIT,'(A)') TEMP(:NLEN(TEMP))
C     block type flags (Block has attribiutes)
      WRITE(UNIT=OUNIT,FMT='(A)') '70'
      IF( IMBUFF(2).EQ.GROUP.OR.POS.EQ.1 ) THEN
C         flag to indicate that block is inserted and it will be
          WRITE(UNIT=OUNIT,FMT='(A)') '64'
      ELSEIF( POS.GT.1 ) THEN
C         a path has been found in the name and will be attached as an attribute
          WRITE(UNIT=OUNIT,FMT='(A)') '66'
      ENDIF
C     Block base point
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(A)') '  0.0'
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(A)') '  0.0'
      IF(POS.GT.1) THEN
C         add in the attribute defintion now for blocks.
          CALL DXFDEFATTRB(OUNIT,1,OK)
      ENDIF
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
      NENTS=RLBUFF(5)
C
      DO 15 J=1,NRRECS
         NXTRLR=RLBUFF(2)
C        read the list of entities
         CALL DRR950(NXTRLR,OK)
         DO 10 I=4,10
            TTMIP=RLBUFF(I)
            IF (TTMIP.GT.0) THEN
C               read the entity and draw it in position
                CALL ALLRD(TTMIP,ENT,M1,DELETE)
C               Only do this lot if a normal entity prevails
                IF(ENT.EQ.LINE) THEN
                    CALL DXFWRL(OUNIT)
                ELSEIF(ENT.EQ.MARKER)  THEN
                    CALL DXFMRKINSTANCE(OUNIT,OK)
                ELSEIF(ENT.EQ.ARC)  THEN
                    CALL DXFWRA(OUNIT)
                ELSEIF(ENT.EQ.TEXT) THEN
                    CALL DXFWRT(OUNIT)
                ELSEIF(ENT.EQ.HATCH) THEN
                    CALL DXFWRH(OUNIT)
                ELSEIF (ENT.EQ.COMPI .OR. ENT.EQ.SYMBI) THEN
                    CALL DXFWCI(OUNIT,M1)
                END IF
C               re-read back into database the correct values
                CALL DRR950(NXTRLR,OK)
            END IF
 10      CONTINUE
 15   CONTINUE
C     End of block marker
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDBLK'
 20   CONTINUE
      OK = .TRUE.
      RETURN
999   CONTINUE
      OK = .FALSE.
      END
C
 
 
      SUBROUTINE DXFWEN(OUNIT,OK)
C     ===========================
C1    VARTYPE            I4    L
C1    IOSTATUS           I     O
C
C2    This routine produces the file formats for
C2    single entity entries ie LINES ARCS TEXT MARKER
C2    COMPONENT AND SYMBOL and dimensioning
C2    entities instances
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      REAL M(3,3)
      INTEGER*4 OUNIT
      INTEGER*2 CURP,ENT,STATUS
      CHARACTER*80 BUFF
      LOGICAL OK,DELETE
C     initialise variables
      CURP=1
C     reset hatch counter
      NOH=0
      NOG=0
C     Writes the section header to file
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'SECTION'
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENTITIES'
10    CALL  DIR500(CURP,OK)
      IF (.NOT.OK) GOTO 100
      ENT=IMBUFF(2)
      STATUS=IMBUFF(1)
      CALL ALLRD(CURP,ENT,M,DELETE)
C     loop back if this is is a duff entity
      IF(STATUS.EQ.10) THEN
          IF(ENT.EQ.LINE) THEN
              CALL DXFWRL(OUNIT)
          ELSEIF(ENT.EQ.MARKER) THEN
C             write out the marker
              CALL DXFMRKINSTANCE(OUNIT,OK)
          ELSEIF(ENT.EQ.ARC)  THEN
              CALL DXFWRA(OUNIT)
          ELSEIF(ENT.EQ.TEXT) THEN
              CALL DXFWRT(OUNIT)
          ELSEIF(ENT.EQ.HATCH) THEN
              CALL DXFWRH(OUNIT)
          ELSEIF(ENT.EQ.COMPI.OR.ENT.EQ.SYMBI) THEN
              CALL DXFWCI(OUNIT,M)
          ELSEIF(ENT.EQ.GROUP) THEN
              CALL DXFWCI(OUNIT,M)
          ELSEIF(ENT.GT.32.AND.ENT.LT.38) THEN
              CALL DXFDIM(OUNIT,CURP)
          ENDIF
      ENDIF
C     indicate an entity count
      IF ( MOD(CURP+0,100).EQ.0) THEN
          WRITE(UNIT=BUFF,FMT='(I7,A)')
     +      CURP,' Entities converted to DXF'
          CALL CPRINT(BUFF)
      ENDIF
      CURP=CURP+1
C     end of the database
      IF (CURP.EQ.NMIPOS) GOTO 100
      GOTO 10
C     write out the end section
100   WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDSEC'
      OK=.TRUE.
      END
 
      SUBROUTINE DXFWGT(OUNIT,OK)
C     ===========================
C1    VARTYPE           I4    L
C1    IOSTAUS            I    O
C
C2    This routine will produce the block for a hatch entity
C2    The starting locations are allways 0,0 in both definition
C2    and during INSERT.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      REAL M(3,3)
      CHARACTER*6 TEMP
      INTEGER*4 OUNIT,NLEN
      INTEGER*2 CURP,ENT,STATUS,TLAY
      LOGICAL OK,DELETE
      EXTERNAL CRUNCH,NLEN
C
      NOH = IDBUFF(4)
C     write out hatch block information
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'BLOCK'
C     layer number
      TLAY=IMBUFF(4)
      CALL DXFWLN(OUNIT)
C     hatch name
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=TEMP,FMT='(A,I3)') '*X',NOH
      CALL CRUNCH(TEMP)
      WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C     anonymous block flag
      WRITE(UNIT=OUNIT,FMT='(A)') '70'
      WRITE(UNIT=OUNIT,FMT='(A)') '1'
C     starting co-ordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
C     start entity in master index
      CURP=IDBUFF(3)
C     read it in
10    CALL  DBR500(CURP,OK)
      IF (.NOT.OK) GOTO 100
      CALL DXFWRL(OUNIT)
      IF(IDBUFF(3).NE.0) THEN
          CURP=IDBUFF(3)
          GOTO 10
      ENDIF
C     End block marker
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDBLK'
C     second layer specification
C     layer number
      IMBUFF(4)=TLAY
      CALL DXFWLN(OUNIT)
      OK=.TRUE.
      RETURN
100   OK=.FALSE.
      END
C
 
      SUBROUTINE DXFWHC(OUNIT,OK)
C1    VARTYPE           I4    L
C1    IOSTAUS            I    O
C
C2    This routine will produce the block for a hatch entity
C2    The starting locations are allways 0,0 in both definition
C2    and during INSERT.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
      REAL M(3,3)
      CHARACTER*6 TEMP
      INTEGER*4 OUNIT,NLEN
      INTEGER*2 CURP,ENT,STATUS,TLAY
      LOGICAL OK,DELETE
      EXTERNAL CRUNCH,NLEN
C
      NOH = IDBUFF(4)
C     write out hatch block information
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'BLOCK'
C     layer number
      TLAY=IMBUFF(4)
      CALL DXFWLN(OUNIT)
C     hatch name
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=TEMP,FMT='(A,I3)') '*X',NOH
      CALL CRUNCH(TEMP)
      WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C     anonymous block flag
      WRITE(UNIT=OUNIT,FMT='(A)') '70'
      WRITE(UNIT=OUNIT,FMT='(A)') '1'
C     starting co-ordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
C     start entity in master index
      CURP=IDBUFF(3)
C     read it in
10    CALL  DBR500(CURP,OK)
      IF (.NOT.OK) GOTO 100
      CALL DXFWRL(OUNIT)
      IF(IDBUFF(3).NE.0) THEN
          CURP=IDBUFF(3)
          GOTO 10
      ENDIF
C     End block marker
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDBLK'
C     second layer specification
C     layer number
      IMBUFF(4)=TLAY
      CALL DXFWLN(OUNIT)
      OK=.TRUE.
      RETURN
100   OK=.FALSE.
      END
      SUBROUTINE DXFWHD(OUNIT)
C     ========================
C1    VARTYPE            I4   L
C1    IOSTATUS           I    O
C
C     This routine produces header information for AUTOcad variables
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/wtov.inc'
      include 'include/product.inc'
      include 'include/dxf.inc'
C
      INTEGER*4 NLEN
      CHARACTER TEMP*50,MAX*7,MIN*7
      INTEGER*4 OUNIT,I
      INTEGER*4 ST
      EXTERNAL NLEN
C     Writes the section header to file
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'SECTION'
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=OUNIT,FMT='(A)') 'HEADER'
C     Variable information
C     Write out the limits (for AutoCAD pan and zoom) and the drawing
C     extents which will actually be the paper extents in world
C     space.
      DO 10 I=1,2
          IF(I.EQ.1) THEN
              MIN='$EXTMIN'
              MAX='$EXTMAX'
          ELSE
              MIN='$LIMMIN'
              MAX='$LIMMAX'
          ENDIF
          WRITE(UNIT=OUNIT,FMT='(A)') '9'
          WRITE(UNIT=OUNIT,FMT='(A)') MIN
          WRITE(UNIT=OUNIT,FMT='(A)') '10'
          WRITE(UNIT=OUNIT,FMT='(F15.6)') WPXMIN
          WRITE(UNIT=OUNIT,FMT='(A)') '20'
          WRITE(UNIT=OUNIT,FMT='(F15.6)') WPYMIN
          WRITE(UNIT=OUNIT,FMT='(A)') '9'
          WRITE(UNIT=OUNIT,FMT='(A)') MAX
          WRITE(UNIT=OUNIT,FMT='(A)') '10'
          WRITE(UNIT=OUNIT,FMT='(F15.6)') WPXMAX
          WRITE(UNIT=OUNIT,FMT='(A)') '20'
          WRITE(UNIT=OUNIT,FMT='(F15.6)') WPYMAX
 10   CONTINUE
C     Current layer number at time of generation
      WRITE(UNIT=OUNIT,FMT='(A)') '9'
      WRITE(UNIT=OUNIT,FMT='(A)') '$CLAYER'
      WRITE(UNIT=OUNIT,FMT='(A)') '7'
C     layer name (Could be just a number but Auto cad thinks names !)
      WRITE(UNIT=TEMP,FMT='(A,I3,A)')
     +                PRODUCT,
     +                CLAYER,
     +                LNAME(I)
C     take out all ILLEGAL character please
      CALL DXFILTER(TEMP,ST)
 
      WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C     Write end of section marker
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDSEC'
      END
 
      SUBROUTINE DXFWL2(OUNIT,X1,Y1,X2,Y2)
C     ====================================
C1    VARTYPE            I4   R  R  R  R
C1    IOSTATS            I    I  I  I  I
C
C2    This routine is an intermentiate control routine for the
C2    dimension routine since it is completely diferent from
C2    the rest. It simply sets up the incoming arguments into
C2    the buffered part data array RBUFF
C2    It should cope with a line vector
C
      include   'include/nbuff.inc'
      include 'include/dxf.inc'
      REAL X1,Y1,X2,Y2
      INTEGER*4 OUNIT
C
      RDBUFF(1)=X1
      RDBUFF(2)=Y1
      RDBUFF(4)=X2
      RDBUFF(5)=Y2
      CALL DXFWRL(OUNIT)
      END
C
      SUBROUTINE DXFWLN(OUNIT)
C     ========================
C1    VARTYPE             I4
C1    IOSTAT              I
C
C2    This small routine simply writes out the layer name and number
C2    to the unit number that was requseted
C
      include  'include/nbuff.inc'
      include  'include/product.inc'
      include  'include/masti.inc'
      include 'include/dxf.inc'
C
      INTEGER*4 OUNIT
      CHARACTER*80 TEMP
      INTEGER*4 NLEN
      INTEGER*4 LAYNAM
      INTEGER*4 ST
C
      EXTERNAL CRUNCH,NLEN
C
      LAYNAM = IMBUFF(4)
C     layer number
      WRITE(UNIT=OUNIT,FMT='(A)') '8'
C     layer name (Could be just a number but Auto cad thinks names !)
      WRITE(UNIT=TEMP,FMT='(A,I3,A)')
     +        PRODUCT,
     +        IMBUFF(4),
     +        LNAME(IMBUFF(4))
C
C     filter out anything vile.
      CALL DXFILTER(TEMP,ST)
      CALL CRUNCH(TEMP)
      CALL FOLDUP(TEMP)
C
 
      WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C
      IF(IMBUFF(3).GT.0) THEN
C         write out the color number
          WRITE(UNIT=OUNIT,FMT='(A)') '62'
          WRITE(UNIT=OUNIT,FMT='(I3)') COLOR(IMBUFF(3))
      ENDIF
C     Linestyle for this layer only if greater than 0
      IF(IMBUFF(6).GT.0) THEN
          IF(IMBUFF(6).LT.1) THEN
              IMBUFF(6)=1
          ENDIF
C         linestyle
          WRITE(UNIT=OUNIT,FMT='(A)') '6'
          WRITE(UNIT=OUNIT,FMT='(A)') LINESN(IMBUFF(6))
     +      (:NLEN(LINESN(IMBUFF(6))))
      ENDIF
 
C     coordinates
      END

      SUBROUTINE DXFWLR(OUNIT)
C     ========================
C1    VARTYPE            I4
C1    IOSTATUS           I
C
C     This routine produces header information for AUTOcad layer
C     settings
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/wtov.inc'
      include 'include/dxf.inc'
C
      REAL M(3,3)
      INTEGER*4 NOL,OUNIT,I,NOSL
      INTEGER*2 CURP,ENT,STATUS,LAYNAM(0:255)
      LOGICAL OK,DELETE
      EXTERNAL DIR500,ALLRD
C     Initialise
      NOL=0
C     Search the current layer array and bulid up an array of
C     numbers. They will be the layer names for AutoCAD
      NOSL=NOL
C     Write out table information for layer control
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'SECTION'
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=OUNIT,FMT='(A)') 'TABLES'
C
C     This section is line style related
      CALL DXFLINESTYLE(OUNIT,OK)
C     this section is layer related
      CALL DXFSTL(OUNIT,OK)
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDTAB'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'ENDSEC'
      END
C
      SUBROUTINE DXFWRA(OUNIT)
C     ========================
C1    VARYPE              I4
C1    IOSTAT              I
C
C2    Writes out the details for a single ARC or circle entity
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/dxf.inc'
C
      INTEGER*4 OUNIT
      LOGICAL CIRCLE
      LOGICAL SAMEA
      LOGICAL OK
      REAL PI
      REAL RAD,DEG
C
      EXTERNAL PI,DEG,RAD,SAMEA
C
      IF(IMBUFF(12).GT.0) THEN
C         possible thick line here
          CALL DXFWRITEPARC(OUNIT,OK)
          RETURN
      ENDIF
C     Circle entitiy is defined
      CIRCLE=SAMEA(ABS((RDBUFF(6)-RDBUFF(5))),PI(2.0))
C
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      IF(CIRCLE) THEN
          WRITE(UNIT=OUNIT,FMT='(A)') 'CIRCLE'
      ELSE
          WRITE(UNIT=OUNIT,FMT='(A)') 'ARC'
      ENDIF
C     layer number
      CALL DXFWLN(OUNIT)
C     Coordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)') RDBUFF(1)
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)') RDBUFF(2)
      WRITE(UNIT=OUNIT,FMT='(A)') '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)') RDBUFF(4)
C     Only write the angles if its an arc
      IF(.NOT.CIRCLE) THEN
          WRITE(UNIT=OUNIT,FMT='(A)') '50'
          WRITE(UNIT=OUNIT,FMT='(G15.6)') DEG(RDBUFF(5))
          WRITE(UNIT=OUNIT,FMT='(A)') '51'
          WRITE(UNIT=OUNIT,FMT='(G15.6)') DEG(RDBUFF(6))
      ENDIF
      END
 
      SUBROUTINE DXFWRH(OUNIT)
C
 
C2    This routine writes out a HATCH and increments
C2    the hatch counter
      include  'include/nbuff.inc'
      include 'include/dxf.inc'
      CHARACTER TEMP*6
      INTEGER*4 OUNIT,NLEN
      EXTERNAL NLEN,CRUNCH
C
      NOH = IDBUFF(4)
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'INSERT'
C     layer number
      CALL DXFWLN(OUNIT)
C     hatch name
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=TEMP,FMT='(A,I3)') '*X',NOH
      CALL CRUNCH(TEMP)
      WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C     coordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      END
C
      SUBROUTINE DXFWRITEPLINE(OUNIT,OK)
C     =================================
C1    VARYPE                    I4  L
C1    IOSTAT                    I   O
C
C2    Writes a polyline out to autocad DXF format
C2    Should handle thick lines in DAXCAD. Assumes that
C2    line or arc has been read and buffers contain info
C
      include  'include/product.inc'
      include  'include/lfont.inc'
      include  'include/wtov.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/dxf.inc'
C
      INTEGER*4 OUNIT
      INTEGER*4 THK
      INTEGER*4 END
      REAL TOT
      REAL L1,L2,L3
      REAL M1,M2,M3
      REAL EXT
      REAL XT1,YT1
      REAL XT2,YT2
      LOGICAL OK
C
      OK = .FALSE.
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.EQ.0 ) THEN
C         do not draw the line
          OK = .TRUE.
          GOTO 999
      ENDIF
 
      TOT=ABS(PAPTOW*LTHKR(1,MOD(THK,1024)))
      END=MOD(THK,8192)/1024
C     generate new extensions
      IF(END.EQ.4) THEN
C         extended length.
C         must generate an arbtry vector
          CALL CV0L14(RDBUFF(1),
     +                RDBUFF(2),
     +                RDBUFF(4),
     +                RDBUFF(5),
     +                L1,L2,L3)
          EXT=TOT/2
C         extend the ends
          CALL VC00P4(RDBUFF(1),RDBUFF(2),-EXT,L1,L2,L3,XT1,YT1)
          CALL VC00P4(RDBUFF(4),RDBUFF(5), EXT,L1,L2,L3,XT2,YT2)
      ELSE
          XT1 = RDBUFF(1)
          YT1 = RDBUFF(2)
          XT2 = RDBUFF(4)
          YT2 = RDBUFF(5)
      END IF
 
C
C     header
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'POLYLINE'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     vertex follows flag
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '66'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '1'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
C     thick line start and end width
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT1
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT1
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'SEQEND'
C
C     If end is rounded then we must geneatee 2 separate polys
      IF ( END.NE.2) THEN
C         no more ends to be done
          OK = .TRUE.
          GOTO 999
      ENDIF
C     ***************************
C            FIRST END
C     ***************************
C
C     header
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'POLYLINE'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     vertex follows flag
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '66'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '1'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
C     thick line start and end width
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     find first vertext point from vector at starting point
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      CALL CV0L14(RDBUFF(1),
     +            RDBUFF(2),
     +            RDBUFF(4),
     +            RDBUFF(5),
     +            L1,L2,L3)
C
C     get a vector perp to the line
      CALL VV00L6(L1,L2,L3,
     +            RDBUFF(4),RDBUFF(5),
     +            M1,M2,M3)
      EXT = TOT/4
      CALL VC00P4(RDBUFF(4),RDBUFF(5),EXT,M1,M2,M3,XT1,YT1)
      CALL VC00P4(RDBUFF(4),RDBUFF(5),-EXT,M1,M2,M3,XT2,YT2)
C
C     write out rest of flags
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
C     Bulge factor here must be 1 for a semi-circle
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) 1.000
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT1
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT1
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
C     second vertex
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     find first vertext point from vector at starting point
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     write out rest of flags
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
C     Bulge factor here must be 1 for a semi-circle
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) 1.000
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'SEQEND'
C
C     ***************************
C            SECOND END
C     ***************************
C
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'POLYLINE'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     vertex follows flag
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '66'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '1'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
C     thick line start and end width
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     find first vertext point from vector at starting point
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      CALL CV0L14(RDBUFF(4),
     +            RDBUFF(5),
     +            RDBUFF(1),
     +            RDBUFF(2),
     +            L1,L2,L3)
C
C     get a vector perp to the line
      CALL VV00L6(L1,L2,L3,
     +            RDBUFF(4),RDBUFF(5),
     +            M1,M2,M3)
      EXT = TOT/4
      CALL VC00P4(RDBUFF(1),RDBUFF(2),EXT,M1,M2,M3,XT1,YT1)
      CALL VC00P4(RDBUFF(1),RDBUFF(2),-EXT,M1,M2,M3,XT2,YT2)
C
C     write out rest of flags
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
C     Bulge factor here must be 1 for a semi-circle
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) 1.000
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT1
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT1
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
C     second vertex
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     find first vertext point from vector at starting point
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     write out rest of flags
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT/2
C
C     Bulge factor here must be 1 for a semi-circle
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) 1.000
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'SEQEND'
C
      OK = .TRUE.
      RETURN
999   CONTINUE
      END
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
      SUBROUTINE DXFWRL(OUNIT)
C     =======================
C1    VARYPE              I4
C1    IOSTAT              I
C
C2    This routine write the necessary information out to the file
C2    for a single line entity
C
      include  'include/nbuff.inc'
      include 'include/dxf.inc'
      INTEGER*4 OUNIT
      LOGICAL OK
C
      IF(IMBUFF(12).GT.0) THEN
C         possible thick line here
          CALL DXFWRITEPLINE(OUNIT,OK)
          RETURN
      ENDIF
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'LINE'
C     layer number
      CALL DXFWLN(OUNIT)
C     coordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') RDBUFF(1)
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') RDBUFF(2)
      WRITE(UNIT=OUNIT,FMT='(A)') '11'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') RDBUFF(4)
      WRITE(UNIT=OUNIT,FMT='(A)') '21'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') RDBUFF(5)
      END
 
      SUBROUTINE DXFWRT(OUNIT)
C     ========================
C1    VARYPE              I4
C1    IOSTAT              I
C
C2    This routine write the necessary information out to the file
C2    for a single  TEXT entity
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/dxf.inc'
C
      REAL SLA,CODE,XSF,X,Y,TANG,XO,YO,HGT,WID
      INTEGER*2 NCHAR,JMATRX
      CHARACTER*240 BUFF,SPECC*1,SBUFF*20,LCH*1
      INTEGER*4 OUNIT,NLEN1,NLEN,PNTR,I,LENGTH
      EXTERNAL NLEN1,NLEN,CRUNCH              
C
C     X and Y location of text
C     text  coding formats
      LCH=' '
      PNTR=1
      BUFF = ' '
      LENGTH=NLEN(CBUFF)
C     Check text for special characters
      DO 10 I=1,LENGTH
         SPECC=CBUFF(I:I)
         IF ( SPECC.EQ.CHAR(129)) THEN
C            phi symbol
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') '%%127'
             PNTR=PNTR+5
         ELSEIF ( SPECC.EQ.CHAR(130)) THEN
C            Degree symbol
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') '%%129'
             PNTR=PNTR+5
         ELSEIF ( SPECC.EQ.CHAR(128)) THEN
C            tolerance symbol
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') '%%128'
             PNTR=PNTR+5
         ELSEIF ( ICHAR(SPECC).GT.130.OR.ICHAR(SPECC).LT.32) THEN
             WRITE(UNIT=SBUFF,FMT='(A,I3)') '%%',ICHAR(SPECC)
             CALL CRUNCH(SBUFF)
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') SBUFF
             PNTR=PNTR+NLEN(SBUFF)
         ELSEIF(CBUFF(I:MIN(I+1,LENGTH)).EQ.'%%') THEN
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') '%%37%%37'
             PNTR=PNTR+8
         ELSEIF(SPECC.EQ.'%'.AND.LCH.EQ.'%') THEN
             CONTINUE
         ELSE
             WRITE(UNIT=BUFF(PNTR:),FMT='(A)') SPECC
             PNTR=PNTR+1
         ENDIF
         LCH=SPECC
 10   CONTINUE
      CBUFF=BUFF
      X=RDBUFF(1)
      Y=RDBUFF(2)
      TANG=RDBUFF(5)
      WID=RDBUFF(3)*PAPTOW
      HGT=RDBUFF(4)*PAPTOW
      XSF=1
      CALL UCODET(RDBUFF(6),SLA,JMATRX,NCHAR)
      CALL  XYNORG(X,Y,JMATRX,TANG,SLA,NCHAR,WID,HGT,XO,YO)
C     Alter Y cordinate for vertical justification
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'TEXT'
C     layer number
      CALL DXFWLN(OUNIT)
C     coordinates
      WRITE(UNIT=OUNIT,FMT='(A)') '10'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') XO
      WRITE(UNIT=OUNIT,FMT='(A)') '20'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') YO
C     Height of characters
      WRITE(UNIT=OUNIT,FMT='(A)') '40'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') HGT
C     Text string
      WRITE(UNIT=OUNIT,FMT='(A)') '1'
      IF(NCHAR.EQ.0) THEN
          WRITE(UNIT=OUNIT,FMT='(A)') CBUFF(:NLEN1(CBUFF))
      ELSE
          WRITE(UNIT=OUNIT,FMT='(A)') CBUFF(:NLEN1(CBUFF))
      ENDIF
C     rotation angle
      WRITE(UNIT=OUNIT,FMT='(A)') '50'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') TANG
C     scale factor
      IF(HGT.GT.0.0000) XSF=WID/HGT
      WRITE(UNIT=OUNIT,FMT='(A)') '41'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') XSF
C     Obliquing angle
      WRITE(UNIT=OUNIT,FMT='(A)') '51'
      WRITE(UNIT=OUNIT,FMT='(F15.6)') SLA
C     text style
      WRITE(UNIT=OUNIT,FMT='(A)') '7'
      WRITE(UNIT=OUNIT,FMT='(A)') 'STANDARD'
C     The text generation flags are set to left jusytification
C     only. no flags are required
      END
 
 
      SUBROUTINE DXFWT2(OUNIT,X,Y,W,H,ROTA,OBANG,TEXT)
C     ================================================
C1    VARTYPE            I4   R  R  R  R     R    C*(*)
C1    IOSTATS            I    I  I  I  I     I     I
C
C     This routine is an intermentiate control routine for the
C     dimension routine since it is completely diferent from
C     the rest. It simply sets up the incoming arguments into
C     the buffered part data array RBUFF
C     It should cope with a piece text
C
      include   'include/nbuff.inc'
      include 'include/dxf.inc'
      REAL X,Y,W,H,ROTA,OBANG
      INTEGER*4 OUNIT
      CHARACTER TEXT*(*)
C
      RDBUFF(1)=X
      RDBUFF(2)=Y
      RDBUFF(3)=W
      RDBUFF(4)=H
      RDBUFF(5)=ROTA
      RDBUFF(6)=OBANG
      CBUFF=TEXT
      CALL DXFWRT(OUNIT)
      END
 
      LOGICAL FUNCTION NOENTS()
C     =========================
C1    no arguments today chaps
C
C2    This routine will return a true status if nothing exists within database
C2    Could be useful if anybody wants it
C
      include  'include/masti.inc'
C
      NOENTS = NMIPOS.EQ.1
C
      END
 
      SUBROUTINE WRDXF0(NOW)
C     ===================
C1    no arguments as such
C
C2    This routine will produce a DXF file format for AutoCAD
C2    and other associated programs which use it.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/lfu.inc'
      include 'include/dxf.inc'
      include 'include/params.inc'
C
      REAL M(3,3)
      INTEGER*2 I,CURP
      CHARACTER FILNAM*80
      CHARACTER EXT*4
      INTEGER*4 OUNIT,ST,NLEN
      INTEGER*4 LENGTH
      LOGICAL OK,EX,OP,NOENTS
      LOGICAL MAKEOK
      LOGICAL NOW
C
      EXTERNAL NLEN
      EXTERNAL NOENTS
      EXTERNAL MAKEOK
C
      NOH=0
      NOG=0
C
      IF(NOENTS() ) THEN
          CALL DEPRNT(720)
          GOTO 210
      ENDIF
C     Immediate using drawing name
      IF (NOW )THEN
	  LENGTH = NLEN(DRGNAM)
	  FILNAM = DRGNAM(:LENGTH-4)
          EXT='.dxf'
          CALL SUFFIX(FILNAM,EXT)
	  CALL DELETE(FILNAM,OK)
          GOTO 2000
      endif
C     get the DXF filename
      CALL DPRMXP(81,FILNAM)
      IF(NLEN(FILNAM).EQ.0) THEN
          GOTO 210
      ENDIF
C     assign standard extension
      EXT='.dxf'
      CALL SUFFIX(FILNAM,EXT)
2000  CONTINUE
C     Open up an ASCII file to output the data
      CALL FINDU2(OUNIT,FILNAM,OK)
      IF(.NOT.OK) THEN
          GOTO 110
      ENDIF
C     inquire about the file
      INQUIRE(FILE=FILNAM,EXIST=EX,OPENED=OP)
C     If the file exists check that it is not opened on an edit pad
      IF(EX) THEN
          IF( MAKEOK() ) THEN
C             check for open pad on dm apollos only
              CALL INQFS1(FILNAM,ST,OK)
              IF(.NOT.OK) THEN
                  GOTO 120
              ENDIF
              CALL DELETE( FILNAM,OK )
              IF(.NOT.OK) THEN
                  GOTO 110
              ENDIF
          ELSE
C             does not want to overwrite
              GOTO 210
          ENDIF
      ENDIF
C
C     Open the file
C
      OPEN(UNIT=OUNIT,
     +          FILE=FILNAM,
     +          STATUS='NEW',
     +          ACCESS='SEQUENTIAL',
     +          ERR=110)
C     build hatch defintions in the drawing
      CALL DXFBUILDHATCH(OK)
C     Header section definiyion
      CALL DXFWHD(OUNIT)
C     Set the layer table with color and line styles
      CALL DXFWLR(OUNIT)
C     Block defintion section
      CALL DXFWBK(OUNIT,OK)
      IF(.NOT.OK) THEN
C         error in writing header
          GOTO 100
      ENDIF
C     All blocks defined get the entities
      CALL DXFWEN(OUNIT,OK)
      IF(.NOT.OK) THEN
          GOTO 100
      ENDIF
C     write an end of file marker
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'EOF'
C     *********************************************************
C     everything ok close up and go home
C     *********************************************************
C
      CLOSE(UNIT=OUNIT)
C
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
C
      CALL DCPRNT(721)
      GOTO 200
C     *********************************************************
C     Error has occured in the database reading operations
C     we canot generate the DXF file as a result. Should be a
C     very rare occurance.
C     *********************************************************
100   CALL DEPRNT(722)
C
      CLOSE(UNIT=OUNIT)
C
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
C
      GOTO 200
C     *********************************************************
C     File handling error has occured. Almost certainly the
C     on the apollo the pad is open. On the PC the file
C     be in use from something else.
C     *********************************************************
120   CALL DCPRNT(277)
      GOTO 200
C     *********************************************************
C     The open statement cannot open the file. Something is
C     wrong.
C     *********************************************************
110   CALL DEPRNT(723)
      GOTO 110
C
200   CONTINUE
C
C     This is called to reset the hatch
C
      CALL DXFRESETHATCH(OK)
210   CONTINUE
C     no more dxf writes
      CCMD='Q'
      MEN=0
      RETURN
      END
C
