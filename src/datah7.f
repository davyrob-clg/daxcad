C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah7.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DBMMAX(PDP,NC,MMAX,OK)
C     SUBROUTINE DBW701(RDAT,P,OK)
C     SUBROUTINE DEM500(P,OK)
C     SUBROUTINE DEM566(M,P,OK)
C     SUBROUTINE DEMDIM(P,OK)
C     SUBROUTINE DIM501(P,N,V,OK)
C     SUBROUTINE DIM502(MASMIP,INSMIP,PRELP,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DBMMAX(PDP,NC,MMAX,OK)
C     =================================
C1    vartype            I2  I4 R(6) L
C1    iostatus           I   I   O   O
C
C2    Subroutine DBMMAX finds the min-max limits
C2    of the XYZ data contained in the connected set
C2    of PD records starting from the
C2    passed PD pointer PDP. The min-max range
C2    is returned in MMAX array in the order XMIN,YMIN,ZMIN,
C2    XMAX,YMAX,ZMAX.
C2    NC is an interpretation flag which indicates
C2    whether to interpret DB records as one coord
C2    or two coords per record.
C2    OK returned true if successful.
C
      include 'include/nbuff.inc'
C
      INTEGER*2 PDP,P
      INTEGER*4 NC
      REAL MMAX(6)
      LOGICAL OK
      EXTERNAL DBR500
C
C     reset the min-max values
      MMAX(1)=1E20
      MMAX(2)=1E20
      MMAX(3)=1E20
      MMAX(4)=-1E20
      MMAX(5)=-1E20
      MMAX(6)=-1E20
C     set initial pointer
      P=PDP
 10   CONTINUE
C     read PD record
      CALL DBR500(P,OK)
      IF (OK) THEN
C       get min-max
        MMAX(1)=MIN(MMAX(1),RDBUFF(1))
        MMAX(2)=MIN(MMAX(2),RDBUFF(2))
        MMAX(3)=MIN(MMAX(3),RDBUFF(3))
        MMAX(4)=MAX(MMAX(4),RDBUFF(1))
        MMAX(5)=MAX(MMAX(5),RDBUFF(2))
        MMAX(6)=MAX(MMAX(6),RDBUFF(3))
        IF (NC.EQ.2) THEN
C         two coords per record
          MMAX(1)=MIN(MMAX(1),RDBUFF(4))
          MMAX(2)=MIN(MMAX(2),RDBUFF(5))
          MMAX(3)=MIN(MMAX(3),RDBUFF(6))
          MMAX(4)=MAX(MMAX(4),RDBUFF(4))
          MMAX(5)=MAX(MMAX(5),RDBUFF(5))
          MMAX(6)=MAX(MMAX(6),RDBUFF(6))
        END IF
      ELSE
C       error reading PD record
        RETURN
      END IF
C     get continuation PD pointer
      P=IDBUFF(3)
C     test for continuation pointer
      IF (IDBUFF(3).GT.0) GOTO 10
C
      END
C
 
      SUBROUTINE DBW701(RDAT,P,OK)
C     ============================
C1    vartype           R(6) I2 L
C1    iostatus           I   O  O
C
C2    Subroutine DBW701 writes the data to the next position
C2    in the part data file copying it from the
C2    part data buffer contained in common block
C2    The next entry pointer to the Pd file is
C2    also updated before return.P returns the
C2    position used.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I
      REAL RDAT(6)
      LOGICAL OK
C
C     copy data into buffer
      DO 10 I=1,6
         RDBUFF(I)=RDAT(I)
 10   CONTINUE
C     write to files
      CALL DBW501(P,OK)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DEM500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEM500 writes the entity data to position consistant
C2    with the pointers currently stored in the data buffers.
C2    the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    Include file used 'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,I,P2
      LOGICAL OK
C
      EXTERNAL DIM500,DBM500,DTM500
C
C     point to MI to be modified
      P=MIP
C     write the data back to where it came from
      CALL DIM500(P,OK)
      P2=IMBUFF(7)
      CALL DBM500(P2,OK)
      IF (IMBUFF(9).NE.0) THEN
         P2=IMBUFF(9)
         CALL DTM500(P2,P,OK)
      END IF
C
      END
**
      SUBROUTINE DEM566(M,P,OK)
C     =========================
C1    vartype        R(3,3) I2 L
C1    iostatus         I    O  O
C
C2    Subroutine DEW066 writes the entity data to next position
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    Intended for use wtih COMPONENT INSTANCES
C2    The next entry pointers are updated before return,so that
C2    sequential calls may be made with the same data,to create
C2    multiple entities.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/wrkdat.inc'
C
      REAL M(3,3)
      INTEGER*4 I
      INTEGER*2 P,P2,P3
      LOGICAL OK
C
      EXTERNAL DIW500,DBW551,DBW550,DTW500,ADDISP
C
C
      P=MIP
C     write the M I data from buffer
      CALL DIM500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
C
C     write first row of transform matrix
      DO 20 I=1,3
         RWORK(I,3)=M(1,I)
         RWORK(I+3,3)=0.0
 20   CONTINUE
C     write second row of transform matrix
      DO 21 I=1,3
         RWORK(I,4)=M(2,I)
         RWORK(I+3,4)=0.0
 21   CONTINUE
C     write third row of transform matrix
      DO 22 I=1,3
         RWORK(I,5)=M(3,I)
         RWORK(I+3,5)=0.0
 22   CONTINUE
C     now write the PD data with continuation
C     point to first data entry in pdf
      P2=IMBUFF(7)
      DO 50,I=1,5
C        now update the PD data
         CALL DBM550(P2,IWORK(1,I),RWORK(1,I),OK)
C        find pointer to position for next write
         P2=IWORK(3,I)
 50   CONTINUE
C
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C
      END
*
      SUBROUTINE DEMDIM(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          I  O
C
C2    Subroutine DEMDIM updates the entity data for any dimension
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    RWORK and MIBUFF. P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/wrkdat.inc'
      include  'include/entity.inc'
      include  'include/dimendat.inc'
C
      INTEGER*2 P,P2,I,P3,J,K
      LOGICAL OK
C
      EXTERNAL DIM500,DBM550,DTM500
C
C     find pointer to text
      P2=IMBUFF(9)
      DO 45 I=1,NUMSTR
         ICBUFF(1)=TICBUF(I,1)
         ICBUFF(2)=TICBUF(I,2)
C        write the dimension text to text file
         CALL DTM550(P2,P,DIMCHR(I),OK)
         P2=ICBUFF(2)
 45   CONTINUE
C     write the MI data from buffer back to file
      CALL DIM500(P,OK)
      IF (.NOT.OK) RETURN
C
C     point to first data entry in pdf
      P2=IMBUFF(7)
C     Write out the DIMENSION PART DATA to PD file
C     sum the total number of records used
      J=RECCNT(1)
C      J=RTALLY(1)+RTALLY(2)+RTALLY(3)+RTALLY(4)+RTALLY(5)+RTALLY(6)
C     +                    +RTALLY(7)+RTALLY(8)+RTALLY(9)+RTALLY(10)
C
C     Check whether original record count has been exceeded
      IF ( J .EQ. RECCNT(2) ) THEN
         DO 50,I=1,J
C           now update the PD data
            CALL DBM550(P2,IWORK(1,I),RWORK(1,I),OK)
C           find pointer to position for next write
            P2=IWORK(3,I)
 50      CONTINUE
      ELSE
C        set excess counter
         K=J-RECCNT(2)
C        write out unaffected records.
         DO 60,I=1,RECCNT(2)-1
C           now update the PD data
            CALL DBM550(P2,IWORK(1,I),RWORK(1,I),OK)
C           find pointer to position for next write
            P2=IWORK(3,I)
 60      CONTINUE
C        Set pointer to next part data position
         IWORK(3,RECCNT(2))=NPDPOS
         CALL DBM550(P2,IWORK(1,RECCNT(2)),RWORK(1,RECCNT(2)),OK)
C        write out new records
         IF ( K .GT. 1  ) THEN
C            more than one  record so need continuation pointers.
             DO 65 I=RECCNT(2)+1,J-1
C               write out from scratch array
                CALL DBW551(IWORK(1,I),RWORK(1,I),P2,OK)
 65          CONTINUE
C            now write out last record with no continuation ponter
             CALL DBW550(IWORK(1,J),RWORK(1,J),P2,OK)
         ELSE
C            Only one record ,write with no continuation ponter
             CALL DBW550(IWORK(1,J),RWORK(1,J),P2,OK)
         END IF
      END IF
C     no need to do any more,OK flag
C     will reflect success or not.
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIM501(P,N,V,OK)
C     ===========================
C1    vartype          I2 I2 I2 L
C1    iostatus          I  I I  O
C
C2    Subroutine DIM501 modifies the value in position N
C2    of record P in the master index file to value V
C2    working through the part data buffer contained in
C2    common block MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.The record must already exist,
C2    since it will be replaced with the new data.
C2    Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 P,N,V
      LOGICAL OK
      EXTERNAL DIR500,DIM500
C
C     read required MI record
      CALL DIR500(P,OK)
C     set MI data word
      IMBUFF(N)=V
C     write MI record back
      CALL DIM500(P,OK)
C
      END
C
C
      SUBROUTINE DIM502(MASMIP,INSMIP,PRELP,OK)
C     =========================================
C1    vartype             I2     I2    I2    L
C1    iostatus            I      I     O     O
C
C2    Subroutine DIM502 modifies the MI entry for INSMIP
C2    such that it's relation pointer links to the same
C2    data structure as the entry at MASMIP.The relation
C2    pointer for this link is returned in PRELP.
C2    Logical flag OK is returned TRUE
C2    if the operation was successful.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 MASMIP,INSMIP,PRELP,I2
      LOGICAL OK
      EXTERNAL DIR500,DIM500
C
C     find relation pointer for master data
C     read required MI record
      CALL DIR500(MASMIP,OK)
C     get relation pointer
      PRELP=IMBUFF(10)
C     write MI record with relation PRELP
      I2=10
      CALL DIM501(INSMIP,I2,PRELP,OK)
C
      END
C
C
