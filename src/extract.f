C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 extract.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE EXPR01(TMIP,NONE,OK)
C     SUBROUTINE EXPR02(PNAME,PDATA,DTYPE,TPRPNT,BLANK,OK)
C     SUBROUTINE EXPR03(TMIP,NP,OK)
C     SUBROUTINE EXPR70(FN)
C     SUBROUTINE EXPR71(FN,STRING)
C     SUBROUTINE EXPR80(NCALF,NCMPF)
C     SUBROUTINE EXPR81(STUNIT,NTROWS,NCMPF,NCALF)
C     SUBROUTINE EXPR82(TABLN2,NCMPF,FN,OK)
C     SUBROUTINE EXPR83(STUNIT,NTROWS,OK)
C     SUBROUTINE EXPR84(NCALF)
C     SUBROUTINE EXPR85(STUNIT,NTROWS)
C     SUBROUTINE EXPR90(STRING,COLNUM)
C     SUBROUTINE EXPR91(STUNIT,OK)
C     SUBROUTINE EXPR92(STUNIT,NPTREC,OTFNAM,OK)
C     SUBROUTINE EXPR93(NP,OTFNAM,OK)
C     SUBROUTINE EXPR931(NP,OTFNAM,OK)
C     SUBROUTINE EXTP00()
C     SUBROUTINE EXTP01()
C     SUBROUTINE EXTP02()
C     SUBROUTINE EXTP03(FN,ST)
C     SUBROUTINE MAJXX1()
C     SUBROUTINE MNIEXT()
C     SUBROUTINE MNLXX0()
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE EXPR01(TMIP,NONE,OK)
C     ==============================
C
C1    vartype            I2    L   L
C1    iostatus           I     O   O
C
C2    Subroutine EXPR01 searches the entity TMIP
C2    for an attached property link and
C2    reads the link data into buffer space.
C2    NONE is returned true if no link exists.
C2    OK returned false if error in read or link.
C
      include 'include/props.inc'
      include 'include/nbuff.inc'
C
      INTEGER*4 NRECS,NPROPS
      INTEGER*2 TPRLNK,TMIP
      LOGICAL OK,NONE
      EXTERNAL DIR500,DRPL01
C
      NONE=.TRUE.
C     read MI record
      CALL DIR500(TMIP,OK)
      IF (OK) THEN
C        get PR link
         TPRLNK=IMBUFF(11)
         IF (TPRLNK.GT.0) THEN
C           read link structure
            CALL DRPL01(TPRLNK,NRECS,NPROPS,OK)
            IF (OK) NONE=.FALSE.
         ELSE
            NONE=.TRUE.
         END IF
      END IF
C
      END
C
C
      SUBROUTINE EXPR02(PNAME,PDATA,DTYPE,TPRPNT,BLANK,OK)
C     ====================================================
C
C1    vartype            C*(*)   C*(*)   I4  I2    L   L
C1    iostatus             I       O     O   O     O   O
C
C2    Subroutine EXPR02 searches the property link
C2    for an attached property called PNAME,and
C2    returns the data in PDATA. OK returned true
C2    if the required property is found.BLANK returned
C2    true if field is blank.The index number of the
C2    property is returned in TPRPNT.
C
      include 'include/props.inc'
      include 'include/nbuff.inc'
C
      INTEGER*4 I,NRECS,NPROPS,ST,NR,NP,LP,J,DTYPE,NLEN1
      INTEGER*2 TPRPNT,TMIP,TPRLNK
      LOGICAL OK,BLANK
      CHARACTER*(*) PNAME,PDATA
      EXTERNAL DRPR01,NLEN1
C
C     find out how many records and properties
      NRECS=PRLBUF(2,1)/256
      NPROPS=MOD(PRLBUF(2,1)+0,256)
C     search for matching prop
C     start to process the link list
C     point to first list record in buffer
      NR=2
      NP=0
 100  CONTINUE
C     process the current record
      IF (NR.LE.NRECS) THEN
C        process the list
C        initialize pointer into record
         LP=5
 110     CONTINUE
         IF (NP.LT.NPROPS) THEN
            IF (LP.LE.8) THEN
C              read pointer from list
               TPRPNT=PRLBUF(LP,NR)
               IF (TPRPNT.GT.0) THEN
C                 process the property
C                 test for matching name
C                 read properties attached
                  CALL DRPR01(TPRPNT,OK)
                  IF (PRCBUF(1)(1:NLEN1(PRCBUF(1))).EQ.
     +                     PNAME(1:NLEN1(PNAME))) THEN
C                    found the correct prop
C      WRITE(UNIT=10,FMT=*)'[EXPR02] PROP FOUND'
                     IF(PRIBUF(7).GT.0) THEN
C      WRITE(UNIT=10,FMT=*)'[EXPR02] DATA=',PRCBUF(3)
                        PDATA=PRCBUF(3)
                        BLANK=.FALSE.
                     ELSE
C      WRITE(UNIT=10,FMT=*)'[EXPR02] PROP BLANK'
                        PDATA='      '
C      WRITE(UNIT=10,FMT=*)'[EXPR02] DATA=',PDATA
                        BLANK=.TRUE.
                     END IF
C                    return the data type
                     DTYPE=PRIBUF(2)/256
                     OK=.TRUE.
                     RETURN
                  END IF
C                 update count of props searched
                  NP=NP+1
               END IF
C              update pointer into list
               LP=LP+1
C              go try next in list
               GOTO 110
            ELSE
C              ran off end of record
C              go try next record
               NR=NR+1
C      WRITE(UNIT=10,FMT=*)'[EXPR02] next record',NR
               GOTO 100
            END IF
         END IF
C      all props have now been tested
      END IF
C
      OK=.FALSE.
C
      END
C
C
      SUBROUTINE EXPR03(TMIP,NP,OK)
C     ===============================
C
C1    vartype            I2   I4   L
C1    iostatus           I    O    O
C
C2    Subroutine EXPR03 searches the property link
C2    for all attached properties,and loads the names
C2    and the data into ENTPRP. OK returned true
C2    if successful. NP returns number of
C2    properties attached,and in the buffer list.
C
      include 'include/props.inc'
      include 'include/propt.inc'
      include 'include/nbuff.inc'
C
      INTEGER*4 I,NRECS,NPROPS,ST,NR,NP,LP,J,DTYPE
      INTEGER*2 TPRPNT,TMIP,TPRLNK
      LOGICAL OK,BLANK,NONE
      CHARACTER*80 PNAME,PDATA
      EXTERNAL DRPR01,EXPR01
C
C     read the property link,if it exists
      CALL EXPR01(TMIP,NONE,OK)
      IF (NONE) THEN
C        no properties attached
         NP=0
         GOTO 99
      END IF
C     find out how many records and properties attached
      NRECS=PRLBUF(2,1)/256
      NPROPS=MOD(PRLBUF(2,1)+0,256)
C      WRITE(UNIT=10,FMT=*)'[EXPR03] NRECS,NPROPS',NRECS,NPROPS
C     start to process the link list
C     point to first list record in buffer
      NR=2
      NP=0
 100  CONTINUE
C     process the current record
      IF (NR.LE.NRECS) THEN
C        process the list
C        initialize pointer into record
         LP=5
C      WRITE(UNIT=10,FMT=*)'[EXPR03] NR=',NR
 
 
 
 110     CONTINUE
C      WRITE(UNIT=10,FMT=*)'[EXPR03] NP=',NP
         IF (NP.LT.NPROPS) THEN
C      WRITE(UNIT=10,FMT=*)'[EXPR03] LP=',LP
            IF (LP.LE.8) THEN
C              read pointer from list
               TPRPNT=PRLBUF(LP,NR)
C      WRITE(UNIT=10,FMT=*)'[EXPR03] TPRPNT=',TPRPNT
               IF (TPRPNT.GT.0) THEN
C                 process the property
C                 read properties attached
                  CALL DRPR01(TPRPNT,OK)
                  IF (OK) THEN
C                    update count of props searched
                     NP=NP+1
C                    the prop data is now in PRCBUF
                     ENTPRP(1,NP)=PRCBUF(1)
                     ENTPRP(2,NP)=PRCBUF(3)
C      WRITE(UNIT=10,FMT=*)'[EXPR03] PRCBUF(1)=',PRCBUF(1)
C      WRITE(UNIT=10,FMT=*)'[EXPR03] PRCBUF(3)=',PRCBUF(3)
                  END IF
               END IF
C              update pointer into list
               LP=LP+1
C              go try next in list
               GOTO 110
            ELSE
C              ran off end of record
C              go try next record
               NR=NR+1
C      WRITE(UNIT=10,FMT=*)'[EXPR03] next record',NR
               GOTO 100
            END IF
         END IF
C      all props have now been read
      END IF
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE EXPR70(FN)
C     =====================
C
C1    vartype           I4
C1    iostatus          I
C
C2    Subroutine EXPR70 loads the variable processor
C2    such that filed number FN is a recognisable
C2    field for numeric expression evaluation.The
C2    variable loaded is a concatenation of the '@'
C2    character and the field number.The preload value
C2    is 1.
C
      INTEGER*4 FN
      DOUBLE PRECISION DN
      CHARACTER*20 TMPTT
      EXTERNAL AEXPRN
C
C     load data to variable processor
      WRITE(UNIT=TMPTT,FMT='(A,I3,A)')
     +      '@',FN,'=1'
      CALL AEXPRN(TMPTT,DN,*198)
 198  CONTINUE
 
C
      END
C
C
      SUBROUTINE EXPR71(FN,STRING)
C     ============================
C
C1    vartype           I4   C*(*)
C1    iostatus          I      I
C
C2    Subroutine EXPR70 loads the variable processor
C2    such that filed number FN is a recognisable
C2    field for numeric expression evaluation.The
C2    variable loaded is a concatenation of the '@'
C2    character and the field number.The preload value
C2    is contained in the STRING.
C
      INTEGER*4 FN
      DOUBLE PRECISION DN
      INTEGER*4 NLEN1
      CHARACTER*80 TMPTT,STRING*(*)
      EXTERNAL AEXPRN,NLEN1
C
C     load data to variable processor
      WRITE(UNIT=TMPTT,FMT='(A,I3,A,A)')
     +     '@',FN,'=',STRING(1:NLEN1(STRING))
      CALL AEXPRN(TMPTT,DN,*198)
 198  CONTINUE
 
C
      END
C
C
      SUBROUTINE EXPR80(NCALF,NCMPF)
C     ==============================
C
C1    vartype             I4    I4
C1    iostatus            O     O
C
C2    Subroutine EXPR80 creates a list of columns
C2    to compare during the comression of a properties
C2    file.The list is stored in array COMFLD,and the
C2    number of fields to compare in NCMPF.
C2    The fields which require calculation are also
C2    found and stored in CALFLD array,the number of them
C2    in NCALF.
C
      include 'include/propt.inc'
C
      INTEGER*4 NCALF,NCMPF,I
C
C     initialize number of compare fields
      NCMPF=0
C     initialize number of calculate fields
      NCALF=0
C
      DO 50 I=1,NCOLS
         IF (COMACT(I).EQ.1) THEN
C           combination allowed
C           load field number to list
            NCMPF=NCMPF+1
            COMFLD(NCMPF)=I
         END IF
         IF (COLDAT(4,I).EQ.2) THEN
C           load calc field to list
            NCALF=NCALF+1
            CALFLD(NCALF)=I
         END IF
 50   CONTINUE
C
      END
C
C
      SUBROUTINE EXPR81(STUNIT,NTROWS,NCMPF,NCALF)
C     ============================================
C
C1    vartype             I4     I4     I4    I4
C1    iostatus            I      I      I     I
C
C2    Subroutine EXPR81 compresses a properties record
C2    currently in TABLIN,using the control data in
C2    the COMFLD,and CALFLD arrays.The tally fields
C2    are tallied,and all calculated fields are regenerated.
C2    The row resulting from the compression is left in
C2    global TABLIN.
C
      include 'include/propt.inc'
C
      REAL TVAL(20),RTMP
      INTEGER*4 I,NTROWS,STUNIT,LL,FN,NCMPF,NCALF,F1(20),F2(20),J
      LOGICAL OK,OKADD(20)
      CHARACTER*255 TABLN2,TMPT*40,TMPC*80
C
      EXTERNAL EXPR82,RVALU,EXPR90,CRUNCH
C
C     set up for addition of fields
      DO 10 I=1,NCOLS
         IF (COMACT(I).EQ.2) THEN
C           addition required
            F1(I)=COLDAT(2,I)
            F2(I)=F1(I)+COLDAT(1,I)-1
            TMPC=TABLIN(F1(I):F2(I))
            CALL CRUNCH(TMPC)
            CALL RVALU(TMPC,TVAL(I),OKADD(I))
         END IF
 10   CONTINUE
C
      DO 50 I=1,NTROWS
C        read the next table row
         TABLN2=' '
         READ(UNIT=STUNIT,REC=I) LL,TABLN2(1:ABS(LL))
C        filter out rows already compressed
         IF (LL.GT.0) THEN
C           compare this record
            CALL EXPR82(TABLN2,NCMPF,FN,OK)
            IF (OK) THEN
C              comparison succeeded mask this row
               WRITE(UNIT=STUNIT,REC=I) -LL,TABLN2(1:LL)
C              tally the required fields
               DO 20 J=1,NCOLS
                  IF (COMACT(J).EQ.2) THEN
C                    field to be added
                     TMPC=TABLIN(F1(J):F2(J))
                     CALL CRUNCH(TMPC)
                     CALL RVALU(TMPC,RTMP,OK)
C                    update the sum if ok
                     IF (OK.AND.OKADD(J)) TVAL(J)=TVAL(J)+RTMP
                  END IF
 20            CONTINUE
            END IF
         END IF
 50   CONTINUE
C
C     data compression now complete
C     must update tally fields in master row
      DO 30 I=1,NCOLS
         IF (COMACT(I).EQ.2 .AND. OKADD(I)) THEN
C           write data back to field
            TMPT=' '
            IF (COLDAT(5,I).EQ.2) THEN
C              integer data
               WRITE(UNIT=TMPT,FMT=COLCON(2,I)) INT(TVAL(I))
            ELSE IF (COLDAT(5,I).EQ.4) THEN
C              real data
               WRITE(UNIT=TMPT,FMT=COLCON(2,I)) TVAL(I)
            END IF
C           update field in TABLIN
            CALL CRUNCH(TMPT)
            CALL EXPR90(TMPT,I)
         END IF
 30   CONTINUE
C
      END
C
      SUBROUTINE EXPR82(TABLN2,NCMPF,FN,OK)
C     =====================================
C
C1    vartype            C*255   I4  I4  L
C1    iostatus             I     I   O   O
C
C2    Subroutine EXPR82 compares the field contents
C2    of TABLN2 with the contents of the equivalent
C2    fields in TABLIN.The OK flag is returned true
C2    if all the fields match,otherwise FN contains
C2    the number of the first non-matching field.
C2    The number of fields to compare is passed in NCMPF.
C
      include 'include/propt.inc'
C
      INTEGER*4 FN,NCMPF,F1,F2,I
      LOGICAL OK
      CHARACTER*255 TABLN2
C
C     loop for number of fields
      I=0
 10   CONTINUE
      I=I+1
      IF (I.LE.NCMPF) THEN
C        compare the field
         FN=COMFLD(I)
C        find field boundaries
         F1=COLDAT(2,FN)
         F2=F1+COLDAT(1,FN)-1
C        compare the fields
         IF (TABLIN(F1:F2).NE.TABLN2(F1:F2)) THEN
C           field mismatch,no need to go any further
            OK=.FALSE.
            RETURN
         END IF
C        got here if match,try next
         GOTO 10
      END IF
C     all fields must have agreed
      OK=.TRUE.
C
      END
C
      SUBROUTINE EXPR83(STUNIT,NTROWS,OK)
C     ===================================
C
C1    vartype             I4     I4    L
C1    iostatus            I      I     O
C
C2    Subroutine EXPR83 locates the first uncompressed
C2    row in properties table scratch file STUNIT,and
C2    loads it into global TABLIN.The row is also flagged
C2    as having been comressed from the table.
C
      include 'include/propt.inc'
C
      INTEGER*4 I,NTROWS,LL,STUNIT
      LOGICAL OK
C
      I=0
 10   CONTINUE
      I=I+1
      IF (I.LE.NTROWS) THEN
C        read next row
         READ(UNIT=STUNIT,REC=I) LL,TABLIN
         IF (LL.LE.0) GOTO 10
C        write back to file as compressed
         WRITE(UNIT=STUNIT,REC=I) -LL,TABLIN
         OK=.TRUE.
      ELSE
C        ran off end of list,no uncomressed records left
         OK=.FALSE.
      END IF
C
      END
C
C
      SUBROUTINE EXPR84(NCALF)
C     ========================
C
C2    Subroutine EXPR84 recalculates the data
C2    within the property table row contained
C2    in global TABLIN,using the current data
C2    in that row.
C
      include 'include/propt.inc'
C
      INTEGER*4 J,FN,NCALF,NLEN,NLEN1,L
      DOUBLE PRECISION DN
      CHARACTER*80 PDATA,TMPTT
      EXTERNAL NLEN,NLEN1
C
C     clear all calculator variables
      CALL CLRVAR()
C
      DO 50 J=1,NCOLS
         IF (COLDAT(5,J).EQ.2 .OR. COLDAT(5,J).EQ.4) THEN
C           do not need to load calc fields
            IF (COLDAT(4,J).NE.2) THEN
C              load variable processor
               PDATA=TABLIN(COLDAT(2,J):COLDAT(2,J)+COLDAT(1,J)-1)
               L=NLEN(PDATA)
               IF (L.GT.0) THEN
C                 load data to variable processor
                  WRITE(UNIT=TMPTT,FMT='(A,I3,A,A)')
     +            '@',J,'=',PDATA(1:L)
                  CALL AEXPRN(TMPTT,DN,*197)
 197              CONTINUE
               END IF
            END IF
         END IF
 50   CONTINUE
C     now evaluate expressions
      DO 60 J=1,NCALF
         FN=CALFLD(J)
C        expression to evaluate
         CALL AEXPRN(COLCON(1,FN),DN,*198)
C        write result to output
         WRITE(UNIT=PDATA,FMT=COLCON(2,FN)) DN
         CALL CRUNCH(PDATA)
C        write to table line
         CALL EXPR90(PDATA,FN)
C        load data to variable processor
         WRITE(UNIT=TMPTT,FMT='(A,I3,A,A)')
     +      '@',FN,'=',PDATA(1:NLEN1(PDATA))
         CALL AEXPRN(TMPTT,DN,*198)
 198     CONTINUE
 60   CONTINUE
C
      END
C
C
      SUBROUTINE EXPR85(STUNIT,NTROWS)
C     ================================
C
C1    vartype             I4     I4
C1    iostatus            I      I
C
C2    Subroutine EXPR85 totals the allowed columns
C2    in the table in scratch file attached to STUNIT.
C
      include 'include/propt.inc'
C
      REAL TVAL(20),RTMP
      INTEGER*4 I,NTROWS,STUNIT,LL,FN,NCMPF,NCALF,F1(20),F2(20),J
      LOGICAL OK,OKADD(20)
      CHARACTER*80 TMPT*40,TMPC*80
C
      EXTERNAL EXPR82,RVALU,EXPR90,CRUNCH
C
C     set up for addition of fields
      DO 10 I=1,NCOLS
C        set field limits
         F1(I)=COLDAT(2,I)
         F2(I)=F1(I)+COLDAT(1,I)-1
         RTOTS(I)=0.0
 10   CONTINUE
C
      DO 50 I=1,NTROWS
C        read the next table row
         TABLIN=' '
         READ(UNIT=STUNIT,REC=I) LL,TABLIN(1:ABS(LL))
C        total the required fields
         DO 20 J=1,NCOLS
            IF (TOTCOL(J)) THEN
C              field to be added
               TMPC=TABLIN(F1(J):F2(J))
               CALL CRUNCH(TMPC)
               CALL RVALU(TMPC,RTMP,OK)
C              update the sum if ok
               IF (OK) RTOTS(J)=RTOTS(J)+RTMP
            END IF
 20      CONTINUE
 50   CONTINUE
C
C     data totalization now complete
C     must update tally fields in totals row
      TABLIN=' '
      DO 30 I=1,NCOLS
         IF (TOTCOL(I)) THEN
C           write data back to field
            TMPT=' '
            IF (COLDAT(5,I).EQ.2) THEN
C              integer data
               WRITE(UNIT=TMPT,FMT=COLCON(2,I)) INT(RTOTS(I))
            ELSE IF (COLDAT(5,I).EQ.4) THEN
C              real data
               WRITE(UNIT=TMPT,FMT=COLCON(2,I)) RTOTS(I)
            END IF
C           update field in TABLIN
            CALL CRUNCH(TMPT)
            CALL EXPR90(TMPT,I)
         END IF
 30   CONTINUE
C
      END
C
      SUBROUTINE EXPR90(STRING,COLNUM)
C     ================================
C
C1    vartype            C*(*)   I4
C1    iostatus             I     I
C
C2    Subroutine EXPR90 writes the data in STRING
C2    to the field associated with column COLNUM
C2    of the current properties table line
C2    in global TABLIN.The field width,justification etc
C2    is read from the table definition data.
C
      include 'include/propt.inc'
C
      INTEGER*4 OL,NLEN1,COLNUM
      CHARACTER*(*) STRING
C
      EXTERNAL NLEN1
C
C     find output length of string
      OL=NLEN1(STRING)
      IF (OL.GE.COLDAT(1,COLNUM)) THEN
C        fill field with data
         OL=COLDAT(1,COLNUM)
         COLPOS=COLDAT(2,COLNUM)
      ELSE IF (COLDAT(3,COLNUM).EQ.1) THEN
C        left justified
         COLPOS=COLDAT(2,COLNUM)
      ELSE IF (COLDAT(3,COLNUM).EQ.4) THEN
C        centre justified
         COLPOS=COLDAT(2,COLNUM)+(COLDAT(1,COLNUM)-OL)/2
      ELSE
C        must be right justified
         COLPOS=COLDAT(2,COLNUM)+COLDAT(1,COLNUM)-OL
      END IF
C     got correct position,now write the data
      TABLIN(COLPOS:COLPOS+OL-1)=STRING(1:OL)
C
      END
C
C
      SUBROUTINE EXPR91(STUNIT,OK)
C     ============================
C
C1    vartype            C*(*) L
C1    iostatus             I   O
C
C2    Subroutine EXPR91 opens a scratch file
C2    for use in property table extraction.
C2    The unit number is returned in STUNIT.
C2    OK returned true if successful.
C
      include 'include/propt.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 STUNIT
      LOGICAL OK
C
      CALL FINDU1(STUNIT,OK)
      OK=.FALSE.
      OPEN(UNIT=STUNIT,STATUS='SCRATCH',ACCESS='DIRECT',
     +        RECL=280, FORM='UNFORMATTED',ERR=99)
CIBM
C      LFU(STUNIT)=.TRUE.
CIBM
C     file now open and ready for use
      OK=.TRUE.
 99   CONTINUE
C      REWIND(UNIT=STUNIT)
C
      END
C
C
      SUBROUTINE EXPR92(STUNIT,NPTREC,OTFNAM,OK)
C     ==========================================
C
C1    vartype             I4     I4     C*(*) L
C1    iostatus            I      I        I   O
C
C2    Subroutine EXPR92 opens the file named OTFNAM
C2    of type formatted sequential,and writes the
C2    complete properties table as specified by the
C2    currently active table,and the NPTREC records
C2    in the scratch file connected to STUNIT to the
C2    file OTFNAM. OK returned true if successful.
C
      include 'include/propt.inc'
C
      INTEGER*4 STUNIT,NPTREC,I,TABUNT,NLEN1,J,N,LL
      LOGICAL OK
      CHARACTER*(*) OTFNAM
      EXTERNAL NLEN1,OPNFFF,CLOSUN
C
      OK=.FALSE.
 10   CONTINUE
      CALL OPNFFF(OTFNAM,TABUNT,OK)
      IF (OK) THEN
C        file now open for writing
C        write table headers
C        write TABLE HEADER
         DO 336 I=1,NHHLIN
            WRITE(UNIT=TABUNT,FMT='(A)')THEADR(I)(1:NLEN1(THEADR(I)))
C           WRITE(UNIT=10,FMT='(A)')THEADR(I)(1:NLEN1(THEADR(I)))
 336     CONTINUE
         WRITE(UNIT=TABUNT,FMT='(A)')' '
C
C        write COLUMN HEADER
         DO 337 I=1,NCHLIN
            TABLIN=' '
            DO 62 J=1,NCOLS
C              get position for field in output line
               N=COLDAT(2,J)
               TABLIN(N:)=COLHED(I,J)
 62         CONTINUE
            WRITE(UNIT=TABUNT,FMT='(A)')TABLIN(1:NLEN1(TABLIN))
C           WRITE(UNIT=10,FMT='(A)')TABLIN(1:NLEN1(TABLIN))
 337     CONTINUE
         WRITE(UNIT=TABUNT,FMT='(A)')' '
C
C        now copy data from scratch file
         DO 50 I=1,NPTREC
C           read from scratch file
            TABLIN=' '
            READ(UNIT=STUNIT,REC=I)LL,TABLIN(1:LL)
            LL=ABS(LL)
C           write to table file
            WRITE(UNIT=TABUNT,FMT='(A)')TABLIN(1:LL)
C           WRITE(UNIT=10,FMT='(A)')TABLIN(1:LL)
 50      CONTINUE
C
C        close output file
         CALL CLOSUN(TABUNT,.TRUE.,OK)
      END IF
 99   CONTINUE
      END
C
      SUBROUTINE EXPR93(NP,OTFNAM,OK)
C     ===============================
C
C1    vartype            I4  C*(*) L
C1    iostatus           I     I   O
C
C2    Subroutine EXPR93 opens the file named OTFNAM
C2    of type formatted sequential,and writes the
C2    properties data from buffer ENTPRP into it.
C2    The number of properties is passed in NP.
C2    OK returned true if successful.
C
      include 'include/propt.inc'
      include 'include/props.inc'
C
      INTEGER*4 STUNIT,NP,I,TABUNT,NLEN1,LL
      INTEGER*4 PRL
      LOGICAL OK
      CHARACTER OTFNAM*(*)
      EXTERNAL NLEN1,OPNFFF,CLOSUN
C
      OK=.FALSE.
 10   CONTINUE
C     open the named file for output
      CALL OPNFFF(OTFNAM,TABUNT,OK)
      IF (OK) THEN
C        file now open for writing
         WRITE(UNIT=TABUNT,FMT='(A,I4,A/)')'Total of',NP,
     +                        ' attached properties'
C 	 compute longest prompt
         PRL=0
         DO 51 I=1,NP
           PRL=MAX(PRL,NLEN1(ENTPRP(1,I)))
 51      CONTINUE
C        now copy data from buffer
         DO 50 I=1,NP
C           write to file
            LL=NLEN1(ENTPRP(2,I))
            WRITE(UNIT=TABUNT,FMT='(A,A,A)')
     +             ENTPRP(1,I)(1:PRL),' : ',ENTPRP(2,I)(1:LL)
 50      CONTINUE
C        close output file
         CALL CLOSUN(TABUNT,.TRUE.,OK)
      END IF
 99   CONTINUE
      END
C
      SUBROUTINE EXPR931(NP,OTFNAM,OK)
C     ===============================
C
C1    vartype            I4  C*(*) L
C1    iostatus           I     I   O
C
C2    Subroutine EXPR93 opens the file named OTFNAM
C2    of type formatted sequential,and writes the
C2    properties data from buffer ENTPRP into it.
C2    The number of properties is passed in NP.
C2    OK returned true if successful.
C
      include 'include/props.inc'
      include 'include/propt.inc'
C
      INTEGER*4 STUNIT,NP,I,TABUNT,NLEN1,LL
      LOGICAL OK
      CHARACTER OTFNAM*(*)
      EXTERNAL NLEN1,OPNFFF,CLOSUN
C
      OK=.FALSE.
 10   CONTINUE
C     open the named file for output
      CALL OPNFFF(OTFNAM,TABUNT,OK)
      IF (OK) THEN
C        file now open for writing
         WRITE(UNIT=TABUNT,FMT='(A,I4,A/)')'Total of',NP,
     +                        ' attached properties'
C        now copy data from buffer
         DO 50 I=1,NP
C           write to file
            LL=NLEN1(ENTPRP(1,I))
            WRITE(UNIT=TABUNT,FMT='(A,A)')'Name=',ENTPRP(1,I)(1:LL)
            LL=NLEN1(ENTPRP(2,I))
            WRITE(UNIT=TABUNT,FMT='(A,A/)')'Data=',ENTPRP(2,I)(1:LL)
 50      CONTINUE
C        close output file
         CALL CLOSUN(TABUNT,.TRUE.,OK)
      END IF
 99   CONTINUE
      END
C
C
      SUBROUTINE EXTP00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the EXTRACT PROPERTY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,EXTP01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the EXTRACT PROPERTY routine
      CALL EXTP01()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE EXTP01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
C
C
      INTEGER TMEN,TCELL,I
C
      EXTERNAL GTMCLO,GTCLRM,EXTP02,MNLXX0,UNFLAG
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize EXTRACT PROPERTY menu
      CALL MNLXX0()
C     enter the EXTRACT PROPERTY routine
      CALL EXTP02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE EXTP02()
C     ===================
C
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/vntable.inc'
      include 'include/props.inc'
      include 'include/propt.inc'
      include 'include/swind.inc'
      include 'include/filunit.inc'
      include 'include/masti.inc'
      include 'include/lfu.inc'
      include 'include/ftypes.inc'
C
      REAL X,Y
      INTEGER*4 I,J,LL,N
      INTEGER*2 MIPP,TENT,DFPP,SPA,TMIP
      LOGICAL QUIT,OPTION,OK
C
      INTEGER*4 COLNUM(20),NLEN,DTYPE,IDATA,SFRN,STUNIT,NPTREC
      INTEGER*4 NLEN1,NCALF,NCMPF,NCMPR,STUNT2,ST,PHNUM
      INTEGER*4 TCELL,TMEN,PFNAM
      INTEGER*4 BINDEX
      REAL RDATA
      DOUBLE PRECISION DN
      LOGICAL TABLOK,BLANK,NONE,CMPRSS,TOTLOK,YESOK
      CHARACTER*80 COLNAM(20),TTXT,PDATA,TMPTT,STRING,TNAME,OTFNAM
      CHARACTER*80 SFILE, FNAM
C
      EXTERNAL NLEN,RVALU,IVALU,EXPR90,NLEN1,FNDPOS,CPRMXP
      EXTERNAL DTPMSG,ALSRCH,FINDET,UPR101,UNFOLD,FNDTOK,YESOK
      EXTERNAL BINDEX 
C
C     set the global filetype
      DAXTYP = DAXPOP
      TMEN=3
      TABLOK=.FALSE.
      CMPRSS=.FALSE.
      TOTLOK=.FALSE.
      DO 5 I=1,20
         TOTCOL(I)=.FALSE.
 5    CONTINUE
C     allow searching for all entities
      CALL ALSRCH()
C     enable return of complete groups
      GSSTAT=2
C
C     goto default entry point by asking for
C     name of property table
      OPTION=.TRUE.
C     load token for "Table" option
      CALL FNDTOK(370,CCMD)
      MEN=3
C     hilite the "Table" cell
      CALL FNDPOS(370,CELLN)
      CALL GTMCHI(MEN,CELLN)
      GOTO 21
C
 10   CONTINUE
C     update the cell if regenerated
      IF(CMPRSS) THEN
          CALL GTHFMC(TMEN,'C',TCELL)
          CALL GTMCHI(TMEN,TCELL)
      ENDIF
C     print prompt for user
C     "Select an option from the MINOR menu"
C     find and flag an entity at hit point
      CALL FINDET(230,X,Y,MIPP,TENT,OPTION,QUIT)
C
      IF (QUIT) RETURN
      IF (.NOT.OPTION) THEN
C
         GOTO 10
      END IF
 21   CONTINUE
      IF (MEN.EQ.3) THEN
C        go handle options
         IF (ICHAR(CCMD).EQ.13) THEN
            IF (NDATA.EQ.0) THEN
C              nothing in buffer,tell the idiot
               CALL DEPRNT(34)
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
C           accept conditions and create output
            IF (TABLOK .AND. NDATA.GT.0) THEN
C              ensure all property data is updated
               CALL UPR101(OK)
C              open scratch file for working
               CALL EXPR91(STUNIT,OK)
C              initialize scratch record number
               SFRN=0
C
C     cycle through the contents of the scratch file
      DO 60 I=1,NDATA
C        read from scratch file
         CALL RSCRF(I,TMIP,X,Y,DFPP,SPA)
C        test for attached props
         CALL EXPR01(TMIP,NONE,OK)
         IF ((OK.AND.NONE).OR.(.NOT.OK)) GOTO 61
C        properties are attached,process them.
C        output properties on the entity
         TABLIN=' '
C        clear calc variables
         CALL CLRVAR()
         DO 40 J=1,NCOLS
C           extract field data and load into TABLIN
            CALL EXTP03(J,ST)
            IF (ST.NE.0) RETURN
 40      CONTINUE
         LL=NLEN1(TABLIN)
         IF (LL.GT.0) THEN
C           write the table line to scratch file
            SFRN=SFRN+1
            WRITE(UNIT=STUNIT,REC=SFRN)LL,TABLIN
         END IF
 61      CONTINUE
 60   CONTINUE
C
C     all properties data now present in scratch file
C     save number of records in file
      NPTREC=SFRN
      NCMPR=0
      IF (CMPRSS) THEN
C        compress the data
C        create compress control block
         CALL EXPR80(NCALF,NCMPF)
C        open a second scratch file
         CALL EXPR91(STUNT2,OK)
 400     CONTINUE
C        locate first uncomressed row in table
         CALL EXPR83(STUNIT,NPTREC,OK)
C        if found,then compress
         IF (OK) THEN
C           compress the row
            CALL EXPR81(STUNIT,NPTREC,NCMPF,NCALF)
            NCMPR=NCMPR+1
            LL=NLEN1(TABLIN)
C           recalculate expressions
            CALL EXPR84(NCALF)
C           write to new output file
            WRITE(UNIT=STUNT2,REC=NCMPR) LL,TABLIN(1:LL)
C           go try again
            GOTO 400
         ELSE
C           no uncompressed rows left in original table
C           close the original scratch file
            CLOSE(UNIT=STUNIT)
CIBM
C            LFU(STUNIT)=.FALSE.
CIBM
C           set unit number to new scratch file
            STUNIT=STUNT2
C           reload row counter to match new table
            NPTREC=NCMPR
         END IF
      END IF
C
      IF (TOTLOK) THEN
C        total required columns
         CALL EXPR85(STUNIT,NPTREC)
C        TABLIN now contains totals
C        write to output
         LL=NLEN1(TABLIN)
         IF (LL.GT.0) THEN
C           write the table line to scratch file
            NPTREC=NPTREC+1
            WRITE(UNIT=STUNIT,REC=NPTREC)LL,TABLIN
         END IF
      END IF
C     create output file name
      OTFNAM=DFNAM(1:NLEN1(DFNAM))//'.'//FNAM(1:NLEN(FNAM))
C     now go write the properties data to
C     the correct output file
      CALL EXPR92(STUNIT,NPTREC,OTFNAM,OK)
C     close the scratch file
      CLOSE(UNIT=STUNIT)
CIBM
C      LFU(STUNIT)=.FALSE.
CIBM
C     pop the output onto the screen
C     tell the idiot to close the pad
C     "Exit properties file before continuing"
      CALL DCPRNT(206)
C     use coords to completely cover the graphics area
      CALL POPPD1(OTFNAM,10,120,840,670)
C
            ELSE
C              table not selected tell the idiot
               CALL DEPRNT(64)
            END IF
         ELSE IF (CCMD.EQ.'T') THEN
C           TABLE option
C          "Enter name of extract table:"
            PHNUM=7
            CALL DISFIL(SFILE,TNAME,PHNUM,OK,QUIT)
C           regen menu
            CALL MNLXX0()
            IF(QUIT) RETURN
            IF(.NOT.OK) THEN
                CALL DPRMXP(216,TNAME)
                IF (NLEN(TNAME).EQ.0) THEN
                   CALL GTMCLO(MEN,CELLN)
                   GOTO 10
                END IF
            ENDIF
 
C           fold name to upper case
CAPOLLO
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C            CALL UNFOLD(TNAME)
CAPOLLO
D      WRITE(10,*) '[EXTRACT] TNAME= ',TNAME
C           load the table definition
            CALL DEFT97(TNAME,OK)
            IF (OK) THEN
C              now have basic table
C              "Table definition loaded"
               CALL DCPRNT(139)
               TABLOK=.TRUE.
C               get the file name from full path
                PFNAM = BINDEX(TNAME, '/')
                IF(PFNAM .GT. 0) THEN
                   FNAM = TNAME(PFNAM+1:NLEN(TNAME))
                ELSE
                   FNAM = TNAME(1:NLEN(TNAME))
                ENDIF
C              must disable totals control
               IF (TOTLOK) THEN
                  CALL FNDPOS(373,I)
                  CALL GTMCLO(3,I)
                  TOTLOK=.FALSE.
               END IF
            ELSE
C              "Cannot find table definition"
               CALL DEPRNT(127)
               TABLOK=.FALSE.
            END IF
         ELSE IF (CCMD.EQ.'C') THEN
C           compress option
            IF (CMPRSS) THEN
C              toggle compress off
               CMPRSS=.FALSE.
C              ensure cell no longer hilited
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            ELSE
C              toggle compress on
               CMPRSS=.TRUE.
C              leave cell hilited
               GOTO 10
            END IF
         ELSE IF (CCMD.EQ.'t') THEN
C           Total option
C           clear totals flags
            DO 82 I=1,20
               TOTCOL(I)=.FALSE.
 82         CONTINUE
            IF (TOTLOK) THEN
C              cancel totals option
               TOTLOK=.FALSE.
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
            IF (TABLOK) THEN
C              only set totals if table definition loaded
C              cycle through totalable columns
               DO 80 I=1,NCOLS
                  TOTCOL(I)=.FALSE.
C                 only valid on numeric fields
                  IF (COLDAT(5,I).EQ.2 .OR. COLDAT(5,I).EQ.4) THEN
 81                  CONTINUE
C                    ask for confirmation
                     CALL DTPMSG(53,1)
                     WRITE(UNIT=TMPTT,FMT='(I3)') I
                     TTXT=TMPTT(1:3)//' ('//DICT01(1)(1:1)//'/'
     +                                   //DICT01(1)(2:2)//')'
                     CALL CPRMXP(TTXT,TMPTT)
                     TOTCOL(I)=YESOK(TMPTT)
                  END IF
 80            CONTINUE
               TOTLOK=.TRUE.
               GOTO 10
            ELSE
C              reset totals control
               TOTLOK=.FALSE.
            END IF
         ELSE IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C           use window for selection of entities
            CALL DCPRNT(88)
            CALL WINDOW(.TRUE.)
C
         ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C                OOPS OPTION                                   *
C***************************************************************
C           if backspace char,redelete last entity from extract list
C           clear the last entity flag in buffer
            CALL ZSFLAG(.FALSE.,OK)
            IF (.NOT.OK) CALL DEPRNT(33)
         END IF
C        reset caller menu cell
         CALL GTMCLO(MEN,CELLN)
C
         GOTO 10
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE
C       "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
C
      END
C
C
      SUBROUTINE EXTP03(FN,ST)
C     =====================
C
C1    vartype            I4
C1    iostatus           I
C
C2    Subroutine EXTP03 extracts the property
C2    data for the entity whose property link is
C2    currently in the buffers,and enters it into
C2    the buffer TABLIN in the format defined by the
C2    currently active properties table.The filed to be
C2    extracted is passed in FN.
C
      include 'include/propt.inc'
C
      INTEGER*4 FN,DTYPE,IDATA,NLEN1,ST,ILIM
      PARAMETER(ILIM=2**30)
      INTEGER*2 TPRPNT
      REAL RDATA
      DOUBLE PRECISION DN
      LOGICAL OK,BLANK
      CHARACTER*80 STRING,PDATA,TMPTT
      EXTERNAL EXPR02,IVALU,CRUNCH,EXPR71,EXPR70,RVALU,EXPR90
      EXTERNAL AEXPRN,NLEN1
C
C     process the data for the entity
      IF(COLDAT(4,FN).EQ.1) THEN
C        data source is a property,read it
         CALL EXPR02(COLCON(1,FN),PDATA,DTYPE,TPRPNT,BLANK,OK)
         IF (OK .AND. .NOT.BLANK) THEN
            IF (DTYPE.EQ.COLDAT(5,FN)) THEN
C              data type is ok
               STRING=' '
               IF (DTYPE.EQ.2) THEN
C                 data is integer
                  CALL IVALU(PDATA,IDATA,OK)
                  IF (OK) THEN
C                    write data with specified format
                     WRITE(UNIT=STRING,FMT=COLCON(2,FN),ERR=100) IDATA
C                    load data to variable processor
                     CALL EXPR71(FN,STRING)
                     CALL CRUNCH(STRING)
                  END IF
               ELSE IF (DTYPE.EQ.4) THEN
C                 convert data to real
                  CALL RVALU(PDATA,RDATA,OK)
                  IF (OK) THEN
C                    write data with specified format
                     WRITE(UNIT=STRING,FMT=COLCON(2,FN),ERR=101) RDATA
C                    load data to variable processor
                     CALL CRUNCH(STRING)
                     CALL EXPR71(FN,STRING)
                  END IF
               ELSE
C                   data is text only
                    STRING=PDATA(1:COLDAT(1,FN))
               END IF
C              STRING now contains the output data
C              write to table line
               CALL EXPR90(STRING,FN)
C            ELSE
C              data mismatch
C      WRITE(UNIT=10,FMT=*)'[EXTP03] DATA MISMATCH'
            END IF
C         ELSE
C           no data available
C      WRITE(UNIT=10,FMT=*)'[EXTP03] NO DATA AVAILABLE'
         END IF
      ELSE IF (COLDAT(4,FN).EQ.2) THEN
C        expression to evaluate
         CALL AEXPRN(COLCON(1,FN),DN,*198)
C        write result to output
         IF (INDEX(COLCON(2,FN),'I').NE.0) THEN
C           must convert to integer
            IF ( DN.GT.ILIM ) GOTO 99
            WRITE(UNIT=STRING,FMT=COLCON(2,FN),ERR=99) INT(DN)
         ELSE
C           use real format
            WRITE(UNIT=STRING,FMT=COLCON(2,FN),ERR=99) DN
         END IF
         CALL CRUNCH(STRING)
C        write to table line
         CALL EXPR90(STRING,FN)
C        load data to variable processor
         WRITE(UNIT=TMPTT,FMT='(A,I3,A,A)')
     +      '@',FN,'=',STRING(1:NLEN1(STRING))
         CALL AEXPRN(TMPTT,DN,*198)
 198     CONTINUE
      ELSE IF (COLDAT(4,FN).EQ.3) THEN
C        Tally only required
C        load field with unity
         STRING='1'
C        write to table line
         CALL EXPR90(STRING,FN)
C        load field to variable processor
         CALL EXPR70(FN)
      END IF
C
      ST=0
      RETURN
 99   CONTINUE
      ST=1
      WRITE(UNIT=STRING,FMT='(A,I3)')
     + 'Out of range error in column:',FN
      CALL EPRINT(STRING)
      WRITE(10,*) 'Format error:"',
     +             COLCON(2,FN)(1:NLEN1(COLCON(2,FN))),'"'
      WRITE(10,*) 'Expression:"',
     +             COLCON(1,FN)(1:NLEN1(COLCON(1,FN))),'"'
      WRITE(10,*) 'On following expression data:',DN
      RETURN
100   CONTINUE
      ST=1
      WRITE(UNIT=STRING,FMT='(A,I3)')
     + 'Out of range error in column:',FN
      CALL EPRINT(STRING)
      WRITE(10,*) 'Format error:"',
     +            COLCON(2,FN)(1:NLEN1(COLCON(2,FN))),'"'
      WRITE(10,*) 'On following data:',IDATA
      ST=0
      RETURN
101   CONTINUE
      ST=1
      WRITE(UNIT=STRING,FMT='(A,I3)')
     + 'Out of range error in column:',FN
      CALL EPRINT(STRING)
      WRITE(10,*) 'Format error:"',
     +            COLCON(2,FN)(1:NLEN1(COLCON(2,FN))),'"'
      WRITE(10,*) 'On following data:',RDATA
 
      END
C
C
      SUBROUTINE MAJXX1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the EXTRACT mode
C2    of operation is selected from the master menu.
C2    controls operation of the EXTRACT function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      EXTERNAL MNIEXT,EXTP00,GTCLRM,FNDTOK,FNDPOS
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,UNFLAG
C
C     Now activate the EXTRACT major option menu
      CALL MNIEXT()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     attach. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     goto default entry point by setting Property
C     load token for "Property" option
C      CALL FNDTOK(320,CCMD)
C      MEN=2
C     hilite the "Property" cell
C      CALL FNDPOS(320,CELLN)
C      CALL GTMCHI(MEN,CELLN)
C      GOTO 20
C
 10   CONTINUE
C     tell him what to do
C     "select an option from the minor menu"
      CALL DCPRNT(284)
C     Read a cursor hit to select EXTRACT mode
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'T') THEN
C           EXTRACT PROPERTY option
            CALL EXTP00()
         ELSE
C           unrecognized EXTRACT option
C           "option not yet available"
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
C        "No MAJOR menu option has been selected"
         CALL DEPRNT(24)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
C
      RETURN
      END
C
C
      SUBROUTINE MNIEXT()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the EXTRACT major options.
C2
C
      EXTERNAL GTCLRM,GTDMEN,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the EXTRACT major options.
C     Hilite the option header
      CALL GTDMHD(33,2)
C
C     Load the options for create
C2    P is the token for EXTRACT PROPERTY
      CALL GTDMEN(330,2)
C
      END
C
C
      SUBROUTINE MNLXX0()
C     ===================
C1    no arguments requierd
C
C2    Enters the options for EXTRACT PROPERTY
C2    to menu number 3.
C
      EXTERNAL GTCLRM,GTDMEN,GTDMHD
C
C     enter window option
      CALL GTDMEN(210,3)
C     enter TABLE option
      CALL GTDMEN(370,3)
C     enter compress coption
      CALL GTDMEN(372,3)
C     enter total option
      CALL GTDMEN(373,3)
C     enter cancel option
      CALL GTDMEN(197,3)
C     enter ACCEPT option
      CALL GTDMEN(13,3)
C
      END
C
C
