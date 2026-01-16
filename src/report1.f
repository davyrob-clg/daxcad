C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 report1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CNTC09(FUNIT,FN,ST)
C     SUBROUTINE CNTC10(CMPNAM,FN,COUNT)
C     SUBROUTINE DOTCOM(DOTLEV,DOTC,DOTCC)
C     SUBROUTINE MAKRPT(ST)
C     SUBROUTINE RPTC09(DOTLEV,FUNIT,FN,ST)
C     SUBROUTINE RPTC10(CMPNAM,FUNIT,DOTLEV,FN,ST)
C     SUBROUTINE RPTCSA(FUNIT,ST)
C     SUBROUTINE RPTUSE(FUNIT,ST)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CNTC09(FUNIT,FN,ST)
C     ==============================
C
C1    vartype            I4   I4 I4
C1    iostatus           I    I  O
C
C2    Subroutine CNTC09 writes a report on the
C2    useage of COMPS/SYMBS in the current drawing
C2    to the file connected to FUNIT.Returns completion
C2    status in ST,zero if ok.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      INTEGER*4 FUNIT,ST,FN,FN2,COUNT,NLEN1
      INTEGER*2 P,TP,TENT
      LOGICAL OK
      CHARACTER*132 CNAME
      EXTERNAL NLEN1
C
      IF (FN.EQ.1) THEN
C       search for COMPONENTS
        TENT=COMPM
      ELSE IF (FN.EQ.2) THEN
C       search for SYMBOLS
        TENT=SYMBM
      ELSE
C       invalid function
        ST=1
        RETURN
      END IF
C
C     set function number for INSTANCE count
      FN2=FN+2
C     search for MASTERS in database
      DO 10 P=1,NMIPOS-1
        CALL DIR500(P,OK)
        IF (IMBUFF(1).NE.100 .AND. IMBUFF(2).EQ.TENT) THEN
C         get name of component
          TP=IMBUFF(9)
          CALL DTR500(TP,OK)
C         go count number of occurrences of instance
          CNAME=CBUFF
          FN=3
          CALL CNTC10(CNAME,FN2,COUNT)
C         write summary to output file
          WRITE(UNIT=FUNIT,FMT='(A,A,I6)',ERR=99)
     +    CNAME(1:NLEN1(CNAME)),' ',COUNT
        END IF
 10   CONTINUE
      ST=0
      RETURN
 99   CONTINUE
      ST=1
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE CNTC10(CMPNAM,FN,COUNT)
C     =================================
C
C1    vartype           C*(*)   I4   I4
C1    iostatus            I     I    O
C
C2    Subroutine CNTC10 searches within the current
C2    database for an entity of type SYMBI or COMPI
C2    based on the value of FN, and name CMPNAM
C2    returning the total of instances found in COUNT.
C2    Could also be used to test for duplicate MASTERS
C2    if called with FN=1 or 2.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*4 FN,NLEN1,COUNT
      INTEGER*2 PMIP,P2,TENT
      LOGICAL OK
      CHARACTER*(*) CMPNAM
      EXTERNAL NLEN1
C
      IF (FN.EQ.1) THEN
C        COMPM required
         TENT=COMPM
      ELSE IF (FN.EQ.2) THEN
C        SYMBM required
         TENT=SYMBM
      ELSE IF (FN.EQ.3) THEN
C        COMPI required
         TENT=COMPI
      ELSE IF (FN.EQ.4) THEN
C        SYMBI required
         TENT=SYMBI
      ELSE
C        invalid function
         TENT=0
          OK=.FALSE.
         RETURN
      END IF
C
      OK=.FALSE.
      P2=1
      COUNT=0
      DO 10 P2=1,NTXPOS-1
      CALL DTR500(P2,OK)
      IF ( CBUFF(1:NLEN1(CBUFF)) .EQ.
     +    CMPNAM(1:NLEN1(CMPNAM))) THEN
C        found correct name,test it
         PMIP=ICBUFF(1)
         CALL DIR500(PMIP,OK)
         IF (IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.TENT) THEN
C           correct type,return this one
            COUNT=COUNT+1
         END IF
      END IF
 10   CONTINUE
C
      END
C
C-----------------------------------------------------
C
      SUBROUTINE DOTCOM(DOTLEV,DOTC,DOTCC)
C     ====================================
C
C1    vartype             I4   C*4   C*(*)
C1    iostatus            I     I      O
C
C2    Subroutine DOTCOM constructs a dot command
C2    at the passed nesting depth DOTLEV.
C2    The base level dot command is passed in DOTC.
C2    The command word is returned in DOTCC.
C
      INTEGER*4 DOTLEV,I
      CHARACTER*4 DOTC,DOTCMD*8,DOTCC*(*)
C
      DOTCC=' '
      IF (DOTLEV.GT.0) THEN
c        nested dot command
C        prime dot command buffer
         DO 5 I=1,DOTLEV
           DOTCMD(I:I)='.'
 5       CONTINUE
         DOTCC=DOTCMD(1:DOTLEV)
         DOTCC(DOTLEV+1:)=DOTC
      ELSE
C        dot level is zero,dingle dot command
         DOTCC(1:)=DOTC
      END IF
C
      END
C
C-----------------------------------------------------
C
      SUBROUTINE MAKRPT(ST)
C     =====================
C
C1    vartype           I4
C1    iostatus          O
C
C2    Subroutine MAKRPT creates a report file for
C2    the current drawing,containing summaries of
C2    all COMPS/SYMBS used,together with associated
C2    properties.
C2    Completion status is returned in ST,zero if ok.
C
      include  'include/params.inc'
      include  'include/filunit.inc'
      include  'include/product.inc'
C
      INTEGER*4 ST,FUNIT,NLEN
      LOGICAL OK
      CHARACTER*132 FNAME
      EXTERNAL SUFFIX,OPNFFF,EPRINT,NLEN
C
C     ensure filename is correct
      FNAME=DFNAM
      CALL SUFFIX(FNAME,'.rpt')
C     now have filename for report
C     open the file for writing
      CALL OPNFFF(FNAME,FUNIT,OK)
      IF (OK) THEN
C       file open ready for report data
C       write a header record to the file
        WRITE(UNIT=FUNIT,FMT='(3A)',ERR=99) '* ',
     +  PRNAM(1:NLEN(PRNAM)),'  report file'
        WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '* =================='
C       report on SYMB/COMP useage
        CALL RPTUSE(FUNIT,ST)
C       report on COMPONENTS/SYMBOLS in detail
        CALL RPTCSA(FUNIT,ST)
C       close report file,and keep it
        CALL CLOSUN(FUNIT,.TRUE.,OK)
        ST=0
      ELSE
C       cannot open report file
        CALL DEPRNT(652)
        ST=1
      END IF
      RETURN
 99   CONTINUE
C     close file before return
      ST=1
      RETURN
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE RPTC09(DOTLEV,FUNIT,FN,ST)
C     =====================================
C
C1    vartype             I4    I4   I4 I4
C1    iostatus            I     I    O  O
C
C2    Subroutine RPTUSE writes a report on the
C2    useage of COMPS/SYMBS in the current drawing
C2    to the file connected to FUNIT.Returns completion
C2    status in ST,zero if ok.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      INTEGER*4 FUNIT,ST,FN,FN2,COUNT,NLEN1,DOTLEV
      INTEGER*2 P,TP,TENT
      LOGICAL OK
      CHARACTER*132 CNAME
      EXTERNAL NLEN1
C
      IF (FN.EQ.1) THEN
C       search for COMPONENTS
        TENT=COMPM
      ELSE IF (FN.EQ.2) THEN
C       search for SYMBOLS
        TENT=SYMBM
      ELSE
C       invalid function
        ST=1
        RETURN
      END IF
C
C     set function number for INSTANCE output
      FN2=FN+2
C     search for MASTERS in database
      DO 10 P=1,NMIPOS-1
        CALL DIR500(P,OK)
        IF (IMBUFF(1).NE.100 .AND. IMBUFF(2).EQ.TENT) THEN
C         get name of component
          TP=IMBUFF(9)
          CALL DTR500(TP,OK)
C         go report each occurrence of instance
          CNAME=CBUFF
          FN=3
          CALL RPTC10(CNAME,FUNIT,DOTLEV,FN2,ST)
        END IF
 10   CONTINUE
      ST=0
      RETURN
 99   CONTINUE
      ST=1
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE RPTC10(CMPNAM,FUNIT,DOTLEV,FN,ST)
C     ===========================================
C
C1    vartype           C*(*)    I4   I4   I4  I4
C1    iostatus            I      I    I    O   O
C
C2    Subroutine RPTC10 searches within the current
C2    database for an entity of type SYMBI or COMPI
C2    based on the value of FN, and name CMPNAM
C2    returning the total of instances found in COUNT.
C2    Could also be used to test for duplicate MASTERS
C2    if called with FN=1 or 2.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/propt.inc'
C
      INTEGER*4 FUNIT,FN,NLEN1,COUNT,ST,DOTLEV,I,NP,LL
      INTEGER*2 PMIP,P2,TENT
      REAL M(3,3),MM(9)
      LOGICAL OK
      CHARACTER*(*) CMPNAM,DOTCMD*4,DOTC*4,DOTC2*4,DOTCC*8
      EXTERNAL NLEN1
C
      IF (FN.EQ.1) THEN
C        COMPM required
         TENT=COMPM
         DOTC='.CMP'
      ELSE IF (FN.EQ.2) THEN
C        SYMBM required
         TENT=SYMBM
         DOTC='.SYM'
      ELSE IF (FN.EQ.3) THEN
C        COMPI required
         TENT=COMPI
         DOTC='.CMP'
      ELSE IF (FN.EQ.4) THEN
C        SYMBI required
         TENT=SYMBI
         DOTC='.SYM'
      ELSE
C        invalid function
         TENT=0
          OK=.FALSE.
         RETURN
      END IF
C     dot command for properties data
      DOTC2='.PRP'
C
      OK=.FALSE.
      P2=1
      COUNT=0
      DO 10 P2=1,NTXPOS-1
      CALL DTR500(P2,OK)
      IF ( CBUFF(1:NLEN1(CBUFF)) .EQ.
     +    CMPNAM(1:NLEN1(CMPNAM))) THEN
C        found correct name,test it
         PMIP=ICBUFF(1)
         CALL DIR500(PMIP,OK)
         IF (IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.TENT) THEN
C           correct type,return this one
            CALL DER566(PMIP,M,OK)
C           now have transform in M
            CALL DECODM(M,MM)
C           construct dot command
            CALL DOTCOM(DOTLEV,DOTC,DOTCC)
C           write to output file
            WRITE(UNIT=FUNIT,FMT='(A)',ERR=99)
     +           DOTCC(1:NLEN1(DOTCC))
            WRITE(UNIT=FUNIT,FMT='(A,A,I6)',ERR=99)
     +           CMPNAM(1:NLEN1(CMPNAM)),' ',PMIP
            WRITE(UNIT=FUNIT,FMT='(F12.5,1X,F12.5,1X,F12.5)',ERR=99)
     +           MM(1),MM(2),MM(3)
            WRITE(UNIT=FUNIT,FMT='(F12.5,1X,F12.5,1X,F12.5)',ERR=99)
     +           MM(4),MM(5),MM(6)
            WRITE(UNIT=FUNIT,FMT='(F12.5,1X,F12.5,1X,F12.5)',ERR=99)
     +           MM(7),MM(8),MM(9)
C           process properties data if attached
            CALL EXPR03(PMIP,NP,OK)
            IF (OK .AND. NP.GT.0) THEN
C             properties available for output
C             output dot command for properties
              CALL DOTCOM(DOTLEV+1,DOTC2,DOTCC)
C             now copy data from buffer
              DO 50 I=1,NP
C               write to file
                WRITE(UNIT=FUNIT,FMT='(A)',ERR=99)
     +            DOTCC(1:NLEN1(DOTCC))
                LL=NLEN1(ENTPRP(1,I))
                WRITE(UNIT=FUNIT,FMT='(A)',ERR=99)
     +            ENTPRP(1,I)(1:LL)
                LL=NLEN1(ENTPRP(2,I))
                WRITE(UNIT=FUNIT,FMT='(A)',ERR=99)
     +            ENTPRP(2,I)(1:LL)
 50            CONTINUE
            END IF
         END IF
      END IF
 10   CONTINUE
      ST=0
      RETURN
C
 99   CONTINUE
      ST=1
      RETURN
C
      END
C
C-----------------------------------------------------
C
      SUBROUTINE RPTCSA(FUNIT,ST)
C     ===========================
C
C1    vartype            I4   I4
C1    iostatus           I    O
C
C2    Subroutine RPTCSA writes a report on the
C2    useage of COMPS/SYMBS in the current drawing
C2    to the file connected to FUNIT.The reoprt data
C2    for each component contains database tag,location
C2    coordinates,scale,and rotation of the comp.
C2    All associated properties data is also appended
C2    to the componet report.
C2    Returns completion status in ST,zero if ok.
C
      INTEGER*4 FUNIT,ST,FN,DOTLEV
C
C     write all COMPONENTS first
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '.GEO'
      DOTLEV=1
      FN=1
      CALL RPTC09(DOTLEV,FUNIT,FN,ST)
C     write all SYMBOLS
      FN=2
      CALL RPTC09(DOTLEV,FUNIT,FN,ST)
C     write data delimiter to file
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '.EOD'
      ST=0
      RETURN
C
 99   CONTINUE
      ST=1
      RETURN
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE RPTUSE(FUNIT,ST)
C     ===========================
C
C1    vartype            I4   I4
C1    iostatus           I    O
C
C2    Subroutine RPTUSE writes a report on the
C2    useage of COMPS/SYMBS in the current drawing
C2    to the file connected to FUNIT.Returns completion
C2    status in ST,zero if ok.
C
      INTEGER*4 FUNIT,ST,FN,COUNT
      CHARACTER*132 CNAME
C
C     count all COMPONENTS first
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '.EXT'
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '..CMP'
      FN=1
      CALL CNTC09(FUNIT,FN,ST)
C     count all SYMBOLS
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '..SYM'
      FN=2
      CALL CNTC09(FUNIT,FN,ST)
C     write data delimiter to file
      WRITE(UNIT=FUNIT,FMT='(A)',ERR=99) '.EOD'
      ST=0
      RETURN
C
 99   CONTINUE
      ST=1
      RETURN
C
      END
C
C     ----------------------------------------------------
C
