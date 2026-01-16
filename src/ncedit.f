C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 ncedit.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE NCEDIT(MIPP,PNAME)
C     SUBROUTINE NCEXTRACTPROPERTY(TMIP)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE NCEDIT(MIPP,PNAME)
C     ============================
C1    VARTYPE            I2   C*80
C1    IOSTATUS           I     I
C
C2    
C2    Edits the property based on the name given in the argument list
C2    This is a OO version of CHGA02 which does too much to be reusable
C2    
C2    
C2    Arguments:-
C2    
C2    MIPP		->		Master index pointer of element to be editied
C2    PNAME		->		Proerty name to be edited
C2    
C2    
C
      include 'include/props.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
C
      CHARACTER*81 PNAME
      CHARACTER*80 NAME
      CHARACTER*80 PDATA
      INTEGER*4 LENGTH
      INTEGER*4 DTYPE
      INTEGER*4 ACTION
      INTEGER*2 MIPP
      INTEGER*2 TPRPNT
      INTEGER*2 TPLPNT
      LOGICAL OK
      LOGICAL NONE
C
C
C     Convert any null terminated in f77
C
C
      LENGTH = INDEX(PNAME,CHAR(0))
      IF ( LENGTH .GT. 1 ) THEN
           NAME = PNAME(1:LENGTH-1)
      ELSE
           NAME = PNAME
      ENDIF
C
C     test for attached properties
C
      CALL EXPR01(MIPP,NONE,OK)
      IF (NONE) THEN
C        no properties attached
         CALL DEPRNT(343)
      ELSE
C        property link present
C        ensure it is up to date
         CALL UPR001(MIPP,OK)
C        search for named property
         CALL EXPR02(NAME,PDATA,DTYPE,TPRPNT,NONE,OK)
         IF (.NOT.OK) THEN
C           named property not attached
C           "No property of this name attached"
            CALL DEPRNT(344)
         ELSE
C           save pointer to property link
            TPLPNT=IMBUFF(11)
C           show property name
            CALL DCPRN2(346,NAME)
C           show current data value
            CALL DCPRN2(345,PDATA)
C           prompt for new value
            CALL ATTP51(DTYPE,ACTION,OK)
C           write text record,or modify existing
            IF (OK.AND.NONE) THEN
C              no data present add text record
               CALL DWPRC0(TPRPNT,7,PRCBUF(3),PRIBUF(7),OK)
C              modify index record for property
               CALL DMPRI0(TPRPNT,OK)
            ELSE IF (OK) THEN
C              write modified text to file
               CALL DMPRC0(TPRPNT,7,PRCBUF(3),PRIBUF(7),OK)
            END IF
         END IF
      END IF
C
      END

      SUBROUTINE NCEXTRACTPROPERTY(TMIP)
C     =================================
C1    vartype                       I2
C1    iostatus                      I
C
C2    Arguments :-
C2
C2    TMIP	->	Master index pointer of property
C2
C2
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
      CHARACTER*81 PNAME
      CHARACTER*81 PDATA
      INTEGER*4 TYPE
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
C
      EXTERNAL NLEN
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
C                 read properties attached
                  CALL DRPR01(TPRPNT,OK)
                  IF (OK) THEN
C                    update count of props searched
                     NP=NP+1
C                    the prop data is now in PRCBUF
                     PNAME = PRCBUF(1)
                     PDATA = PRCBUF(3)
                     TYPE = PRIBUF(2)/256
C
C                    Add on nulls for C compatibilty
C
                     LENGTH = NLEN(PNAME)
                     PNAME(LENGTH+1:) = CHAR(0)
                     LENGTH = NLEN(PDATA)
                     PDATA(LENGTH+1:) = CHAR(0)
C
                     CALL NCWRITEPROPERTY(PNAME,TYPE,PDATA)
C
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
