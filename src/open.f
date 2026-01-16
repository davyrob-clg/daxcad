C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 open.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION MAKEOK()
C     FUNCTION TYPEOK(UNITN)
C     SUBROUTINE CHKNAM(INAME,OK)
C     SUBROUTINE CLOSNM(NAME,KEEP,OK)
C     SUBROUTINE CLOSUN(UNITN,KEEP,OK)
C     SUBROUTINE DXFILE(FNAME,DAXTYP,ST)
C     SUBROUTINE EXFILE(FNAME,DAXTYP,ST)
C     SUBROUTINE FILERR(DNUM,NAME,OK1)
C     SUBROUTINE  FINDU1(UNIT,OK)
C     SUBROUTINE FINDU2(UNIT,NAME,OK)
C     SUBROUTINE FINDUN(OK)
C     SUBROUTINE FPARS(FNAM,ST)
C     SUBROUTINE OPENNM(NAME,UNITN,LOAD,OK)
C     SUBROUTINE OPNFFA(FNAM,FUNIT,OK)
C     SUBROUTINE OPNFFF(FNAM,FUNIT,OK)
C     SUBROUTINE OPNOFF (NAME,UNIT,OK)
C     SUBROUTINE OURSCR (UNIT,RECLN,OK)
C     SUBROUTINE SUFFIX(STRING,SUFF)
C     SUBROUTINE UKILLF(FNAM,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CHKNAM(INAME,OK)
C     ==========================
C
C1    vartype           C*(*) L
C1    iostatus            I   O
C
C2    This routine checks that the filename NAME is valid
C2    cos if it aint the prog. takes a nose dive
C
      include  'include/product.inc'
      include  'include/library.inc'
      include  'include/params.inc'
C
      CHARACTER*(*) INAME,CH*1,NAME*100,INVCH*13,TNAM*100,TPRNAM*40,
     +              TXNAM*100
      LOGICAL OK,FSTOP,FCOL
      INTEGER*4 I,I2,I3,NLEN1,J,ICH,PL,IP
      EXTERNAL NLEN1,UNFOLD
C                                      
      NAME = INAME
      TPRNAM = PRNAM
      PL=NLEN1(PRNAM)
C     test all names in lower case for
C     compatibility wiyh UNIX systems
C SPB - 021194
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(NAME)
C      CALL UNFOLD(TPRNAM)
C SPB - 021194
      IP = INDEX(NAME,TPRNAM(1:NLEN1(TPRNAM)))      
      
      IF(FILTYP .EQ. 'PTABLE'.AND.IP.GT.0)  THEN
         TNAM = NAME(IP+PL+1:)
      END IF                                      
      TXNAM = LIBRARY(1:NLEN1(LIBRARY))//'/dmfdir/'
      IF(INDEX(NAME,TXNAM(1:NLEN1(TXNAM))).GT. 0) THEN
         OK = .TRUE.
         RETURN
      END IF
CIBM|PC386
C      CALL CRUNCH(NAME)
C      FSTOP=.FALSE.
C      FCOL=.FALSE.
C      INVCH='"/[]|<>+=;,*?'
CIBM|PC386
C
      I=NLEN1(NAME)
CAPOLLO
      IF ( NAME(I:I).EQ.'/'.OR.NAME(I:I).EQ.'\\')   THEN
CAPOLLO
CSUN|PC386
C      IF ( NAME(I:I).EQ.'/'.OR.NAME(I:I).EQ.'\\')   THEN
CSUN|PC386
CIBM
C      IF ( NAME(I:I).EQ.'/'.OR.NAME(I:I).EQ.'\' )   THEN
CIBM
         OK=.FALSE.
      ELSE IF ( INDEX(NAME,PRNAM(1:PL)) .GT. 0 ) THEN
         IF(NAME.EQ.PRNAM(1:PL)//'.drg'.OR.
     +      NAME.EQ.PRNAM(1:PL)//'.save.drg'.OR.
     +      NAME.EQ.PRNAM(1:PL)//'.prt') THEN
            OK=.TRUE.
         ELSE
            OK=.FALSE.
         END IF
      ELSE
         OK=.TRUE.
      END IF
CIBM|PC386
C         DO 24 J=1,I
C         ICH=ICHAR(NAME(J:J))
C         IF((ICH.GT.32.AND.ICH.LT.126).AND.
C     +        INDEX(INVCH,NAME(J:J)).EQ.0) THEN
C             GOTO 23
C         ELSE IF(ICH.EQ.58.AND.(.NOT.FCOL)) THEN
C             FCOL=.TRUE.
C             GOTO 23
C         ELSE IF(ICH.EQ.46.AND.(.NOT.FSTOP)) THEN
C             FSTOP=.TRUE.
C             GOTO 23
C         ELSE IF(ICH.EQ.92) THEN
C             GOTO 23
C         END IF
C         OK=.FALSE.
C         RETURN
C 23      CONTINUE
C 24      CONTINUE
CIBM|PC386
      END
C
C     -----------------------------------------------------
C

      SUBROUTINE CLOSNM(NAME,KEEP,OK)
C     ===============================
C1    Iostatus           I     I   O
C1    Vartype           C*(*)  L   L
C
C     This routine closes a named file. It checks
C     that the file is open and attempts to close
C     it. If the file was not open or if there was
C     and error on the close, a logical OK is returned
C     False. Keep is a logical which is TRUE if to be kept
C     or FALSE if to be deleted
C
      include 'include/lfu.inc'
      CHARACTER  NAME*(*)
      INTEGER UNITN,NUM
      LOGICAL KEEP,OK,OP
C
  10  CONTINUE
C
      OK=.FALSE.
      UNITN = 0
      INQUIRE(FILE=NAME,NUMBER=UNITN)
      IF (UNITN.GT.0) THEN 
         IF (KEEP) THEN
             CLOSE(UNITN,STATUS='KEEP',IOSTAT=NUM)
CIBM
C             LFU(UNITN)=.FALSE.
CIBM
         ELSE
             CLOSE(UNITN,STATUS='DELETE',IOSTAT=NUM)
CIBM
C             LFU(UNITN)=.FALSE.
CIBM
         ENDIF
         IF (NUM.GT.0) GOTO 99
         OK=.TRUE.
      END IF
C
  99  CONTINUE
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE CLOSUN(UNITN,KEEP,OK)
C     ================================
C1    Iostatus           I     I   O
C1    Vartype           I*4    L   L
C
C     This routine closes a file by unit number. It checks
C     that the file is open and attempts to close
C     it. If the file was not open or if there was
C     and error on the close, a logical OK is returned
C     False. Keep is a logical which is TRUE if to be kept
C     or FALSE if to be deleted
C
      include 'include/lfu.inc'
      INTEGER UNITN,NUM
      LOGICAL KEEP,OK,OP
C
  10  CONTINUE
C
      OK=.FALSE.
      OP = .FALSE.
      INQUIRE(UNITN,OPENED=OP)
C
      IF (OP) THEN
         IF (KEEP) THEN
            CLOSE(UNITN,STATUS='KEEP',IOSTAT=NUM)
CIBM
C             LFU(UNITN)=.FALSE.
CIBM
         ELSE
             CLOSE(UNITN,STATUS='DELETE',IOSTAT=NUM)
CIBM
C             LFU(UNITN)=.FALSE.
CIBM
         ENDIF
         IF (NUM.GT.0) GOTO 99
         OK=.TRUE.
      END IF
C
  99  CONTINUE
      END
C
C     -----------------------------------------------------
C
C
      SUBROUTINE EXFILE(FNAME,DAXTYP,ST)
C     ==================================
C
C1    vartype            C*(*)  I4   I4
C1    iostatus             I    I    O
C
C2    Subroutine EXFILE tests for existance of
C2    a file of name FNAME which is of the type
C2    DAXTYP. The completion status of the test
C2    is returned in ST,zero for success.
C2       DAXTYP - 0  DAXCAD Drawing file
C2              - 1  DAXCAD Part file
C2              - 2  DAXCAD Component file
C2              - 3  DAXCAD Symbol file
C2              - 4-6  Reserved
C2              - 7  ASCII file
C2              - 8  Binary file
C2
C2           ST - 0  File exists,and of correct type
C2           ST - 1  File exists,and of incorrect  type
C2           ST - 2  File exists,and is NOT a DAXCAD type
C2           ST - 3  File exists,and already in use
C2           ST - 4  File does not exist
C2           ST - 5  File type out of range
      include 'include/lfu.inc'
C
      INTEGER*4 I,X,TUNIT,ST,DAXTYP,UNITN
      LOGICAL EX,OP,OK,ASCOK,FSEQOK
      CHARACTER*(*) FNAME,UNF*20,SEQ*20,FILTYP*80
      CHARACTER*80 TCFHED
      CHARACTER*12 FTYPES(0:6)
CIBM
C      EXTERNAL CTYPE
CIBM
      DATA FTYPES/'DRAWING','PART','COMPM','SYMBM',' ',' ',' '/
C
C     test for existance of file
      I = 0
      INQUIRE(FILE=FNAME,NUMBER=I,EXIST=EX,IOSTAT=X)
      IF (EX) THEN
C        test for file already in use
         I = 0
         INQUIRE(FILE=FNAME,NUMBER=I,IOSTAT=X)
         IF (I.GT.0) THEN
C           file already in use,cannot re-open it
            ST=3
            RETURN
         END IF
C        test for formatting condition of file
C        DAXCAD created file must be sequential,unformatted
CIBM
C         CALL CTYPE(FNAME,FSEQOK,ASCOK)
CIBM
         IF (DAXTYP.LE.6) THEN
C           test for which type of DAXCAD file it is
CSUN
C             INQUIRE(FILE=FNAME,SEQUENTIAL=SEQ)
C             IF (SEQ.EQ.'yes') THEN
CSUN
CAPOLLO|PC386
            IF (EX) THEN
CAPOLLO|PC386
CIBM
C               IF (FSEQOK) THEN
CIBM
C              set type of file
               FILTYP=FTYPES(DAXTYP)
C              open and check type from header
C              find free unit
               CALL FINDU1(TUNIT,OK)
C              now check that is of the correct form
               OPEN (TUNIT,FILE=FNAME,STATUS='OLD',
     +         FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
CIBM
C               LFU(TUNIT)=.TRUE.
CIBM
C              check correct type of file
               TCFHED=' '
               READ(UNIT=TUNIT,ERR=99) TCFHED
               CLOSE(UNIT=TUNIT,STATUS='KEEP')
CIBM
C               LFU(TUNIT)=.FALSE.
CIBM
C              look at file type descriptor
               OK=(TCFHED.EQ.FILTYP)
               IF (OK) THEN
                  ST=0
                  RETURN
               END IF
 99            CONTINUE
C              error during read of file header
               CLOSE (UNIT=TUNIT,STATUS='KEEP')
CIBM
C               LFU(TUNIT)=.FALSE.
CIBM
               ST=2
               RETURN
            ELSE
C              cannot be daxcad file type
               ST=2
               RETURN
            END IF
         ELSE IF (DAXTYP.EQ.7) THEN
C           must be ASCII file,(formatted,sequential)
CSUN
C             IF (SEQ.EQ.'yes') THEN
CSUN
CAPOLLO|PC386
            SEQ='YES'
            IF (SEQ.EQ.'YES') THEN
CAPOLLO|PC386
CIBM
C            IF(ASCOK) THEN
CIBM
C              got an ascii file alright
               ST=0
               RETURN
            ELSE
C              wrong type of file
               ST=1
            END IF
         ELSE IF (DAXTYP.EQ.8) THEN
C           should be unformatted file
C           must be ASCII file,(formatted,sequential)
CSUN
C             IF (SEQ.EQ.'no') THEN
CSUN
CAPOLLO|PC386
            IF (SEQ.EQ.'NO') THEN
CAPOLLO|PC386
CIBM
C            IF(.NOT.FSEQOK.OR.ASCOK) THEN
CIBM
C              got a random binary file alright
               ST=0
               RETURN
            ELSE
C              wrong type of file
               ST=1
            END IF
         ELSE
C           DAXTYP out of range
            ST=5
            RETURN
         END IF
      ELSE
C        file does not exist
         ST=4
      END IF
C
      END
 
 
 
 
      SUBROUTINE FILERR(DNUM,NAME,OK1)
C     ================================
C1    iostatus           I    O    O
C1    vartype            I4  C*(*) L
C
C     This routine prints the message passed to it
C     and offers the the chance of supplying a new
C     file name. Returns OK1=.TRUE. if he accepts
C     or false if he declines.
C
      include 'include/vntable.inc'
C
      CHARACTER*40 NAME*(*),NAME1
      INTEGER*4 NLEN,DNUM
      LOGICAL OK1
      EXTERNAL  CPRINT,DPRMXP,DCPRN2,NLEN
C
C
C      CALL CPRINT(DICT01(DNUM))
C     tell him the name of the file which has errored
      CALL DCPRN2(DNUM,NAME)
C     prompt for new filename
      CALL DPRMXP(81,NAME1)
      OK1=( NLEN(NAME1).GT.0 )
      IF ( OK1 ) NAME=NAME1
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE  FINDU1(UNIT,OK)
C     ===========================
C1    vartype       I    L
C1    iostatus      O    O
C
C     Finds a free unit.  (I).
C
      INTEGER*4 U1,U2,UNIT
      LOGICAL OK,OD
      PARAMETER (U1=11, U2=126)
C 
      OK=.TRUE.
C

      DO 20, UNIT=U1,U2
         OD = .FALSE.
         INQUIRE (UNIT=UNIT, OPENED=OD, ERR=10)
         IF (.NOT.OD) RETURN
   20  CONTINUE
C      if got here,ran out of possible units
   10  OK=.FALSE.
C
      END
C
C     -----------------------------------------------------
C
C
      SUBROUTINE FINDU2(UNITN,NAME,OK)
C     ==========================
C1    vartype       I   C*(*) L
C1    IOSTATUS      O     I   O
C     FINDS A UNIT FOR A NAMED FILE.  (I,C).
      INTEGER*4 UNITN
      LOGICAL OD,OK
      CHARACTER NAME*(*)
 
      OK=.TRUE.
C	  INQUIRE (FILE=NAME, NUMBER=UNIT, ERR=10)

C     IF (UNITI2.GT.0) THEN
C        CLOSE (UNIT=UNITI2)
C		 UNITN=UNITI2
C         RETURN
C      END IF

      CALL FINDU1(UNITN,OK)
      RETURN
 
   10 OK=.FALSE.
 
      END
 
      SUBROUTINE FINDUN(OK)
C     =====================
      include 'include/lfu.inc'
      LOGICAL OK
C
      OK=.FALSE.
      END
 
      SUBROUTINE FPARS(FNAM,ST)
C     =========================
C                      C*(*) I
C                       I/O  o
C
C     This routine is used to search for a file
C     name in the current directory an then in the
C     default directory.
C     ST = 0 if  file was found
C
      include  'include/library.inc'
      CHARACTER*(*)FNAM,TMPTXT*80
      LOGICAL EX
      INTEGER NLEN,ST
      EXTERNAL NLEN
C
      ST = 0
      INQUIRE(FILE=FNAM,EXIST=EX)
C     file exists so return with st = 1
      IF(EX) then
C           WRITE(*,*) 'FNAM ',FNAM,ST
           RETURN
      END IF
      ST = 1
      TMPTXT=LIBRARY(1:NLEN(LIBRARY))//CHAR(47)//FNAM(1:NLEN(FNAM))
      INQUIRE(FILE=TMPTXT,EXIST=EX)
c     file is not found so return with st = 0
      IF(.NOT.EX) THEN 
c         WRITE(*,*) 'FNAM ',FNAM,ST
         RETURN
      END IF
c     file in library was found so st = 2
      ST = 0
c     return the new name FNAM with the library added
      FNAM=TMPTXT
C         WRITE(*,*) 'FNAM ',FNAM,ST
      END
C

      FUNCTION MAKEOK()
C     =================
C1    iostatus    O
C1    vartype     L
C
C     This function prints the message
C     and checks that it is ok to overwrite
C     file name. Returns .TRUE. if he accepts
C     or false if he declines.
C
      CHARACTER ANS*3
      LOGICAL MAKEOK
      LOGICAL YES
C
      include 'include/vntable.inc'

C
C     Call confirmation dialog to make one answer only calls
C     but jump to no by default
C
      CALL CONFIRMATION(DICT01(282),.FALSE.,YES)
C
      MAKEOK = YES
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE OPENNM(NAME,UNITN,LOAD,OK)
C     =====================================
C1    Iostatus           I     O     I   O
C1    Vartype           C*(*)  I*4    L   L
C
C2    This routine opens a named file. It checks
C2    that the file exists, that is not open
C2    and that it is of the correct form before
C2    it opens the file on UNITN with a logical
C2    OK = true. If an error is encountered the
C2    user is invited to try another file name.
C2    if he accepts the open is tried again, if
C2    he declines the offer a logical false is returned
CDHR*********************
CDHR  MODIFIED FOR SR9.6
CDHR*********************
C
      include 'include/params.inc'
      include 'include/lfu.inc'
      include 'include/product.inc'
      include 'include/ftypes.inc'
      include 'include/suffix.inc'
C
      INTEGER I,X
      INTEGER*4 UNITOP
      INTEGER*4  UNITN,NLEN,BINDEX,DAXNAM,DAXST
      INTEGER*2 NLEN2,M,M1,UNIT2
      LOGICAL OK,OK1,OK2,KEEP,LOAD,EX,OP,MAKEOK,TYPEOK
      LOGICAL BAKDRG
      CHARACTER*40 NAME*(*),TNAME*80,TYPE,SUFX*6
      CHARACTER UNF*20,SEQ*20,FORM*20
 
CPOLLO|SUN|PC386
      CHARACTER*80 ERROR(0:5)
CPOLLO|SUN|PC386
 
      EXTERNAL FILERR,MAKEOK,FINDU2,TYPEOK,UKILLF,NLEN,
     +         CHKNAM,CLOSUN,BINDEX,DAXNAM
 
CPOLLO|SUN|PC386
      DATA ERROR/
     + 'File exists,and of correct type (DAXCAD)',
     + 'File exists,and of incorrect  type (DAXCAD)',
     + 'File exists,and is NOT a DAXCAD type',
     + 'File does not exist',
     + 'File is opened cannot use it',
     + 'File cannot be opened' /
CPOLLO|SUN|PC386
 
C
      IF(NLEN(NAME).EQ.0) GOTO 99
C
C      print*, 'opening the file',name
	  INQUIRE(FILE=NAME,EXIST=EX,IOSTAT=X)
C
      OK = .FALSE.
      I=BINDEX(NAME,'.')
      SUFX='        '
 
      IF ( I.GT.0.AND..NOT.BAKDRG(NAME) )  SUFX=NAME(I:)
 
      DO 600 I =1,NSUF
 
         IF(SUFX.EQ.LSUF(I)) GOTO 610
 
600   CONTINUE
      SUFX = '                '
610   CONTINUE
 
C
      TYPE=FILTYP
      OK1=.FALSE.
      CALL FINDU2(UNITN,NAME,OK1)
      IF (.NOT.OK1) THEN
         CALL DEPRNT(287)
         GOTO 99
      END IF
C
 15   CONTINUE
      CALL SUFFIX(NAME,SUFX)
      DAXST = DAXNAM( NAME )
C      WRITE(10,*) '[OPENNM] DAXYP= ',DAXTYP
C      WRITE(10,*) '[OPENNM] DAXST= ',DAXST
C     if any error occurs then
      IF (DAXST .NE. 0 .AND. DAXST.NE.3  ) THEN
 
C         error has occured try again
C          WRITE(10,*) '[OPENNM] ',ERROR(DAXST)
          CALL DEPRNT(341)
          CALL DPRMXP(81,TNAME)
          IF ( NLEN(TNAME).GT.0 ) THEN
 
             NAME  = TNAME
C            make sure it has the correct sufixx
             CALL SUFFIX(NAME,SUFX)
             GOTO 15
 
          ELSE
 
             GOTO 99
 
          ENDIF
 
      ENDIF
  10  CONTINUE
C     make sure the correct extension is on the filename at all times.
      CALL SUFFIX(NAME,SUFX)
C
      UNITOP = 0
      INQUIRE(FILE=NAME,NUMBER=UNITOP,EXIST=EX,IOSTAT=X)
 
      OK=.FALSE.
      OK1=.FALSE.
C
      CALL CHKNAM(NAME,OK2)
      IF (.NOT.OK2 .AND. DAXTYP .NE. DAXPOP ) THEN
         CALL FILERR(276,NAME,OK1)
         IF (.NOT.OK1) GOTO 99
         GOTO 10
      END IF
C
C     is file already open
C      IF (UNITOP.GT.0) THEN
CC         CALL FILERR(277,NAME,OK1)
C         IF (.NOT.OK1) GOTO 99
C         GOTO 10
C      ENDIF
C     does file exist
      IF (.NOT.EX) THEN
         IF (LOAD) THEN
            CALL FILERR(278,NAME,OK1)
            IF (.NOT.OK1) GOTO 99
            GOTO 15
         ELSE
			
            OPEN (UNIT=UNITN,FILE=NAME,STATUS='NEW',
     +            FORM='UNFORMATTED',ACCESS='SEQUENTIAL',ERR=98)
            OK=.TRUE.
CIBM
C            LFU(UNITN)=.TRUE.
CIBM
            GOTO 99
         ENDIF
      ELSE IF ((.NOT.LOAD).AND.EX) THEN
C           test name in lower case for UNIX
C      WRITE(10,*) '[OPENNM] FILE EXISTS'
            TNAME=NAME
C SPB - 021194
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C            CALL UNFOLD(TNAME)
C SPB - 021194
            IF(TNAME.EQ.PRNAM(1:NLEN(PRNAM))//'.drg'.OR.
     +         TNAME.EQ.PRNAM(1:NLEN(PRNAM))//'.prt'.OR.
     1         TNAME.EQ.PRNAM(1:NLEN(PRNAM))//'.save.drg') THEN
                CALL UKILLF(NAME,OK)
                OPEN (UNITN,FILE=NAME,STATUS='NEW',
     +                FORM='UNFORMATTED',ACCESS='SEQUENTIAL',ERR=98)
CIBM
C                LFU(UNITN)=.TRUE.
CIBM
                OK=.TRUE.
 
                GOTO 99
            END IF
            IF (MAKEOK()) THEN
                CALL UKILLF(NAME,OK)
                OPEN (UNITN,FILE=NAME,STATUS='NEW',
     +                FORM='UNFORMATTED',ACCESS='SEQUENTIAL',ERR=98)
CIBM
C                LFU(UNITN)=.TRUE.
CIBM
                OK=.TRUE.
                GOTO 99
            ELSE
                CALL FILERR(90,NAME,OK1)
                IF (OK1) GOTO 15
                GOTO 99
            ENDIF
      ENDIF
C     now check that is of the correct form
      OPEN (UNITN,FILE=NAME,STATUS='OLD',
     +           FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
      REWIND(UNIT=UNITN)
CIBM
C      LFU(UNITN)=.TRUE.
CIBM
 
C     well it must be a valid file
      OK=.TRUE.
C
  99  CONTINUE
      RETURN
C
 98   CONTINUE
      CALL FILERR(280,NAME,OK1)
      IF (.NOT.OK1) GOTO 99
      GOTO 15
C      OK=.FALSE.
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE OPNFFA(FNAM,FUNIT,OK)
C     ================================
C
C1    vartype           C*(*)  I4   L
C1    iostatus            I    O    O
C
C2    Subroutine OPNFFA opens a new file called FNAM
C2    for sequential,formatted access,on unit FUNIT.
C2    If a file of the given name exists,then the user is
C2    asked for confirmation before deletion of the file.
C2    If the file is open then it is closed and deleted,if
C2    delete is allowed by the user.
C
CDHR*********************
CDHR  MODIFIED FOR SR9.6
CDHR*********************
      include 'include/lfu.inc'
      INTEGER*4 FUNIT
      LOGICAL OK,EX,YESOK,MAKEOK
      CHARACTER*(*) FNAM,FORM*20,YORN*20
      EXTERNAL UKILLF,FINDU1,FOLDUP,YESOK,MAKEOK
C
C     test for existance
      INQUIRE(FILE=FNAM,EXIST=EX)
      OK=.TRUE.
      IF (EX) THEN
C        ask the user for permission to delete
C        "File exists. OK to overwrite? (y/n)"
         OK=MAKEOK()
         IF ( OK ) THEN
C           kill the file
            CALL UKILLF(FNAM,OK)
         END IF
      END IF
C
      IF (OK) THEN
C        find a unit and open the file
         CALL FINDU1(FUNIT,OK)
         IF (.NOT.OK) GOTO 99
         OPEN(UNIT=FUNIT,FILE=FNAM,ACCESS='SEQUENTIAL',
     +         FORM='FORMATTED',ERR=99)
CIBM
C         LFU(FUNIT)=.TRUE.
CIBM
C        file now open for writing
         OK=.TRUE.
      END IF
      RETURN
C
 99   CONTINUE
      OK=.FALSE.
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE OPNFFF(FNAM,FUNIT,OK)
C     ================================
C
C1    vartype           C*(*)  I4   L
C1    iostatus            I    O    O
C
C2    Subroutine OPNFFF opens a new file called FNAM
C2    for sequential,formatted access,on unit FUNIT.
C2    If a file of the given name exists,then it is deleted,
C2    and then opened as a new file.If the file is open
C2    then it is closed and deleted before being opened
C2    as a new file.
      include 'include/lfu.inc'
C
      INTEGER*4 FUNIT
      LOGICAL OK
      CHARACTER*(*) FNAM,FORM*20
      EXTERNAL UKILLF,FINDU1
C
C     ensure no such named file exists
      CALL UKILLF(FNAM,OK)
      IF (OK) THEN
C        find a unit
         CALL FINDU1(FUNIT,OK)
         IF (.NOT.OK) GOTO 99
         OPEN(UNIT=FUNIT,FILE=FNAM,ACCESS='SEQUENTIAL',
     +         FORM='FORMATTED',ERR=99)
CIBM
C         LFU(FUNIT)=.TRUE.
CIBM
C        file now open for writing
         OK=.TRUE.
      END IF
      RETURN
C
 99   CONTINUE
      OK=.FALSE.
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE OPNOFF (NAME,UNIT,OK)
C     ================================
C
C1    vartype             C*(*) I4    L
C1    iostatus              I   O     O
C
C2    open an old file of NAME if it exists and
C2    signal an error if it does not.The
C2    unit number is returned in UNIT,the
C2    file being opened for sequential,formatted
C2    access.OK true if successful.
C
CDHR*********************
CDHR  MODIFIED FOR SR9.6
CDHR*********************
      include 'include/lfu.inc'
C
      INTEGER UNIT,NLEN
      LOGICAL EX,OK
      CHARACTER NAME*(*),STAT*3,SEQ*3,UNF*3,ACC*3
      EXTERNAL NLEN,FINDU1,DEPRNT
C
C
      INQUIRE(FILE=NAME,ACCESS=ACC,EXIST=EX,
     +        SEQUENTIAL=SEQ,UNFORMATTED=UNF,ERR=10)
C
C     check for existance
C
      IF (EX) THEN
        STAT='OLD'
        CALL FINDU1(UNIT,OK)
        IF (OK) THEN
          OPEN (UNIT=UNIT,FILE=NAME,STATUS=STAT,
     +                  FORM='FORMATTED',ERR=10)
          REWIND (UNIT=UNIT,ERR=10)
CIBM
C          LFU(UNIT)=.TRUE.
CIBM
       END IF
      ELSE
C       signal non-existance
C       "File does not exist"
        CALL DEPRNT(278)
      END IF
      RETURN
C
 10   CONTINUE
C     "File handling error has ocurred"
      WRITE(10,*) '[OPENOFF] ERROR'
      CALL DEPRNT(12)
      OK=.FALSE.
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE OURSCR (UNIT,RECLN,OK)
C     =================================
C
C1    vartype              I    I    L
C1    iostatus             O    I    O
C
C     open a scratch file for unformatted access
C     with record length RECLN. Return unit number
C     attached in UNIT and OK true if operation
C     successful
C
      include 'include/lfu.inc'
      INTEGER UNIT,RECLN
C
      LOGICAL OK
C
      EXTERNAL FINDU1,DEPRNT
C
      CALL FINDU1 (UNIT,OK)
      IF (.NOT.OK) THEN
         CALL DEPRNT(248)
         RETURN
      END IF
       OPEN (UNIT=UNIT,STATUS='SCRATCH',FORM='UNFORMATTED',
     +    ACCESS='DIRECT',RECL=RECLN,ERR=11)
CIBM
C       LFU(UNIT)=.TRUE.
CIBM
       RETURN
11     OK=.FALSE.
       END
C
C     -----------------------------------------------------
C
      SUBROUTINE SUFFIX(STRING,SUFF)
C     ==============================
C
C1    vartype            C*(*)  C*4
C1    iostatus             IO    I
C
C2    Subroutine SUFFIX ensures that the filename
C2    passed in STRING contains the suffix SUFF,if
C2    it does not,them SUFF is appended and the
C2    corrected filename returned in STRING.
C2    If more than one '.' is present in the string
C2    then the name is truncated to the first one,then
C2    the suffix is appended.
C
      include 'include/suffix.inc'
      LOGICAL GOAG
      INTEGER NLEN,I,J,I1,I2,T,SLSTR,MAXSIZ
      CHARACTER*(*) STRING,SUFF,TEMP1*132,TEMP2*4
      EXTERNAL NLEN,UNFOLD,NLEN1
      INTRINSIC LEN,MIN
C
C     copy input string.
      TEMP1=STRING
      TEMP2=SUFF

C     fold to lower case.
CAPOLLO
C SPB - 021194
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(TEMP1)
CAPOLLO
      I1=NLEN(TEMP1)
CAPOLLO
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(TEMP2)
CAPOLLO
      I2=NLEN(TEMP2)
C
C     if either string is blank return.
      IF ( I1.EQ.0.OR.I2.EQ.0) RETURN
C
      IF(TEMP1(I1:I1).EQ.'*') THEN
         IF(I1.GT.1) THEN
            STRING=TEMP1(:I1-1)
         ENDIF
         RETURN
      ENDIF
C
C     look for suffix.
      T=0
      GOAG=.TRUE.
      I=I1+1
10    CONTINUE
        T=T+1
        I2=NLEN(LSUF(T))
        J=I1-I2+1
        IF ( J.GT.0 ) THEN
          IF ( TEMP1(J:I1).EQ.LSUF(T)(1:I2) ) THEN
            GOAG=.FALSE.
           I=J
          END IF
        ELSE IF (J.LE.0 ) THEN
C         suffix not at end at all.
          I=I1+1
          IF ( TEMP1(1:I1).EQ.TEMP2(1:I2) ) I=J
        END IF
C     loop round again if i<NSUF
      IF (GOAG.AND.T.LT.NSUF) GOTO 10
C
C     I now contains position for suffix to be placed
C
C     but is there sufficient space for the suffix
C     so truncate to suit if necessary
      MAXSIZ = LEN(STRING)-NLEN(SUFF) + 1
      I = MIN(I,MAXSIZ) 
C
      STRING(I:)=SUFF
C     return with suffix on string
      END
C
      FUNCTION TYPEOK(UNITN)
C     ======================
C
C1    vartype    L     I4
C1    iostatus   O     I
C
C2    Function TYPEOK returns true if the
C2    header in the currently opened drawing
C2    file agrees with the file type defined in
C2    global FILTYP.
C
      include 'include/params.inc'
      include 'include/fhead.inc'
C
      INTEGER UNITN
      LOGICAL TYPEOK,OK
      EXTERNAL RSTFHD
C
C     check correct type of file
      CALL RSTFHD(OK)
C
      IF (OK) THEN
C        look at file type descriptor
C        WRITE(10,'(A,A40)') 'TYPEOK READ',CFHEAD(1)
C        WRITE(10,'(A,A40)') 'TYPEOK ACT ',FILTYP
C         WRITE(10,*)'RFHEAD1= ',RFHEAD(1)
         OK=( CFHEAD(1).EQ.FILTYP)
CIBM|PC386
C         IF(RFIDF.GT.0.0.OR.RFIDF2.GT.0.0) THEN
C            IF (RFHEAD(3).LT.0.0.OR.RFHEAD(4).LT.0.0) OK=.FALSE.
C         END IF
CIBM|PC386
      END IF
 99   CONTINUE
      TYPEOK=OK
C     something wrong with file
      RETURN
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE UKILLF(FNAM,OK)
C     ==========================
C
C1    vartype           C*(*) L
C1    iostatus            I   O
C
C2    Subroutine UKILLF unconditionally
C2    deletes a file of name FNAM,and returns
C2    completion status in OK. If file does not
C2    exist,also returns with OK true.
C
CDHR*********************
CDHR  MODIFIED FOR SR9.6
CDHR*********************
      include 'include/lfu.inc'
      INTEGER*4 FUNIT
      LOGICAL OK,EX,OP
      CHARACTER*(*) FNAM,UNF*20,SEQ*20,FORM*20
      EXTERNAL CLOSNM,FINDU1
C
      INQUIRE(FILE=FNAM,EXIST=EX)
      IF (EX) THEN
C        go a head and delete the file
         CALL DELETE(FNAM,OK)
         IF(.NOT.OK) THEN
              GOTO 99
         ENDIF
      END IF
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
      OK=.FALSE.
      END
C
C     -----------------------------------------------------
C
