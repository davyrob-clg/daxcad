C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 cursor83.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ADDKTX(STRING)
C     SUBROUTINE DISFIL(SFILE,FILNAM,PHNUM,OK,QUIT)
C     SUBROUTINE GETKEY()
C     SUBROUTINE READMN(XP,YP)
C     SUBROUTINE REMKEY()
C     SUBROUTINE SAVKEY()
C     SUBROUTINE STRKEY()
C     SUBROUTINE WRKHIT(SX,SY,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ADDKTX(STRING)
C     =========================	
C2    Adds any text input required
      include 'include/menun.inc'
      include 'include/keydef.inc'
      include 'include/vntable.inc'
C
       INTEGER*4 ST,FN,NLEN1,DNUM
       REAL X,Y
       CHARACTER*80 STRING,ANS*1
       LOGICAL YESOK
C
       EXTERNAL NLEN1,YESOK
C
       DNUM=566
       CALL GTPMSG(DICT01(DNUM),1)
       CALL GTRLIN(ANS,1)
       IF (.NOT.YESOK(ANS)) RETURN
C
       ST=KLEN
       FN=ST+1
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)=',"'
C
       ST=FN+1
       FN=ST+(NLEN1(STRING))
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)=STRING
C
       ST=FN
       FN=ST+1
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)='">'
C
       KLEN=FN
       DNUM=565
       CALL DCPRN2(DNUM,KCOM)
C
 99    CONTINUE
C
       END
C
      SUBROUTINE DISFIL(SFILE,FILNAM,PHNUM,OK,QUIT)
C     =============================================
C1    vartype           C*(*)   C*(*) I4   L   L
C1    iostat              I      O    I    O   O
C
C2    This routine will allow files to be selected from a
C2    cursor driven menu. The SFILE varaible is only used
C2    to pass  the name of a manually generated menu select file
C2
C
      include  'include/menun.inc'
      include  'include/menug.inc'
      include  'include/dig2wo.inc'
      include  'include/macro.inc'
      include  'include/calc.inc'
      include  'include/gtxt2.inc'
      include  'include/vntable.inc'
      include  'include/lfu.inc'
      include  'include/wildc.inc'
      include  'include/library.inc'
      include  'include/apollo.inc'
      include  'include/product.inc'
      include  'include/journal.inc'
C
      CHARACTER*80 FILES,SFILE,TABLE,LIBR,CBUFF
      CHARACTER FILNAM*80,INBUF*80,TOKEN*1,ENDCH*1,TEMP*80
      CHARACTER*256 SELECTFILE
      INTEGER*4 FUNIT,RECL,HUNIT
      INTEGER*4 INLEN
      INTEGER*4 POPNUM
      INTEGER*4 CELL
      INTEGER*4 ST
      INTEGER*4 POS(2)
      INTEGER*4 LENGTH
      INTEGER*4 DEFAULT
      INTEGER*4 I,N,LOGIC,SP,EP,INC,RECN,RETC,LPATH,IP,C,PAGSIZ
      INTEGER*4 MACMEN,MACCEL,MENKEY,CURSX,CURSY,NLEN1,ENDP,TSP
      INTEGER*4 LIST,PHNUM,BINDEX,ID,IM,NLEN,MNPATH
      REAL X,Y
      LOGICAL OK,QUIT,FOK,PAGEUP,PAGEDN,FE,NOK,TMPGNS,TMPGIN
 
      EXTERNAL LOGIC,NLEN1,FINDUN,CPRINT,OURSCR,GTCLRM,
     +         GTPMEN,GTCLRC,MENKEY,OPNFFF,CHKNAM,BINDEX,NLEN
 
C     Initialise the variables.
      TABLE = '/ptabl/?*[^.^..]'
C
C     if menu select is off then abort
C
      IF(MSFOFF) THEN
         QUIT=.FALSE.
         OK=.FALSE.
         RETURN
      ENDIF
C
C
      CALL FILESELECTOR(PHNUM,SELECTFILE,256,LENGTH,ST)
      IF ( ST.EQ.-1) THEN
          OK = .FALSE.
          QUIT= .TRUE.
          RETURN
      ENDIF
      IF ( ST.EQ.0.AND.NLEN(SELECTFILE).GT.0) THEN
          OK = .TRUE.
          QUIT = .FALSE.
          FILNAM = SELECTFILE
          IF(JOURON) CALL WRTJRN(0.0, 0.0, 'a', FILNAM, 0)
          RETURN
      ELSE
          OK = .FALSE.
          QUIT= .FALSE.
          FILNAM = ' ' 
          RETURN
      ENDIF
C
      END
C
C--------------------------------------------------------------
C
      SUBROUTINE GETKEY()
C     ===================	
C2    Saves current command in the command string
      include 'include/menun.inc'
      include 'include/keydef.inc'
      include 'include/vntable.inc'
C
       INTEGER*4 ST,FN,NLEN1,DNUM
       CHARACTER*20 STRING
C
       EXTERNAL NLEN1
C
       LASTP=KLEN
       ST=KLEN+1
       FN=ST
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)='<'
C
       ST=FN+1
       FN=ST+(NLEN1(KCMD))
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)=KCMD
C
       IF(SAVCRD.AND.KCMD.EQ.'GINPUT') THEN
          ST=FN+1
          WRITE(STRING,'(A,F7.2,A,F7.2,A)') '(',KEYX,',',KEYY,')'
          FN=ST+(NLEN1(STRING))
          IF(FN.GT.80) THEN
             DNUM=569
             CALL DEPRNT(DNUM)
             GOTO 99
          END IF
          KCOM(ST:FN)=STRING
       END IF
 
C
       ST=FN
       FN=ST
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       KCOM(ST:FN)='>'
C
       KLEN=FN
       DNUM=565
       IF(FN.GT.80) THEN
          DNUM=569
          CALL DEPRNT(DNUM)
          GOTO 99
       END IF
       CALL DCPRN2(DNUM,KCOM)
C
 99    CONTINUE
C
       END
C
      SUBROUTINE READMN(XP,YP)
C     ==========================	
C2    This subroutine decodes the menu cell selected
C
      include 'include/menug.inc'
      include 'include/dig2wo.inc'
      include 'include/vntable.inc'
C
C
      REAL X,Y,XP,YP
      INTEGER*4 C,DNUM,NLEN1
      CHARACTER*80 CBUFF,ENTRY*20
C
      EXTERNAL NLEN1
C
      FILEOK=.FALSE.
C
C     correct the cursor hit first
      CALL NEWXY(XP,YP,X,Y,MENWXY)
C
      IF (X.LT.MLIMIT(1).OR.X.GT.MLIMIT(3).OR.Y.LT.MLIMIT(2)
     +   .OR.Y.GT.MLIMIT(4)) THEN
          DNUM=573
          CALL EPRINT(DICT01(DNUM))
          RETURN
      ELSE
          CELLX=INT(X/MINCX)+1
          CELLY=INT(Y/MINCY)+1
          DMFILE=MENTXT(CELLX,CELLY)
          IF (DMFILE.EQ.'undefined') THEN
              DNUM=560
              CALL EPRINT(DICT01(DNUM))
              RETURN
          END IF
      END IF
C
      FILEOK=.TRUE.
C
      END
C
      SUBROUTINE REMKEY()
C     ===================	
C2    Removes command from arrays
      include 'include/menun.inc'
      include 'include/keydef.inc'
      include 'include/vntable.inc'
      include 'include/apollo.inc'
C
C
       INTEGER*4 DNUM,KPOS
       INTEGER*2 N
       REAL X,Y
C
C     get the key to which the command string is to be assigned
 10   CONTINUE
      DNUM=567
      CALL GTPMSG(DICT01(DNUM),1)
      CALL GTRLIN(KEY,1)
C
C     get the storage position
      N=0
      KPOS=0
 15   CONTINUE
      N=N+1
      IF(KEY.EQ.KCODE(N)) THEN
         KPOS=N
      ELSE IF (N.LT.KTOTAL) THEN
         GOTO 15
      END IF
C
      IF (KPOS.EQ.0) THEN
          DNUM=561
          CALL DEPRNT(DNUM)
          RETURN
      END IF
C
      DO 20 N=KPOS,KTOTAL-1
         KCODE(N)=KCODE(N+1)
         KEYDEF(N)=KEYDEF(N+1)
 20   CONTINUE
C
      KEYCHG=.TRUE.
      KTOTAL=KTOTAL-1
C
      END
C
      SUBROUTINE SAVKEY()
C     ===================	
C2    Saves current command string into the arrays
      include 'include/menun.inc'
      include 'include/keydef.inc'
      include 'include/vntable.inc'
      include 'include/apollo.inc'
C
C
       INTEGER*4 DNUM,KPOS,NLEN1
       INTEGER*2 N
       CHARACTER*1 ANS
       LOGICAL YESOK
C
       EXTERNAL NLEN1,YESOK
C
C      get the key to which the command string is to be assigned
 10    CONTINUE
       DNUM=560
       CALL DPRMXP(DNUM,KEY)
       IF(NLEN1(KEY).EQ.1) THEN
          DNUM=568
          CALL DPRMXP(DNUM,ANS)
          IF (YESOK(ANS)) RETURN
          GOTO 10
       END IF
C
       KTOTAL=KTOTAL+1
       IF(KTOTAL.GT.20) THEN
          RETURN
       END IF
C
C     get the storage position
      N=0
      KPOS=KTOTAL
 15   CONTINUE
      N=N+1
      IF(KEY.EQ.KCODE(N)) THEN
         KPOS=N
         KTOTAL=KTOTAL-1
      ELSE IF (N.LT.(KTOTAL-1)) THEN
         GOTO 15
      END IF
C
C     store the key
      KCODE(KPOS)=KEY
C     store the command
      KEYDEF(KPOS)=KCOM
C
      KEYCHG=.TRUE.
C
      END
C
      SUBROUTINE STRKEY()
C     ===================	
C2    Stores the key definitions in a file
C     part of the KEY DEFINITION CODE
C     [created 4/8/88 - mlw]
      include 'include/menun.inc'
      include 'include/keydef.inc'
      include 'include/vntable.inc'
      include 'include/lfu.inc'
      include 'include/library.inc'
      include 'include/product.inc'
 
C
C
       CHARACTER*80 FNAME
       CHARACTER*22 STRING
       INTEGER*4 KUNIT,N,NLEN1,ST,NLEN
       LOGICAL OK,KEEP
C
       EXTERNAL NLEN1,NLEN
C
CIBM|PC386
C      fname=PRODUCT(1:NLEN(PRODUCT))//'.KEY'
C      CALL FPARS(TEMP)
CIBM|PC386
CSUN|APOLLO
       FNAME=PRODUCT(1:NLEN(PRODUCT))//'.KEY'
       CALL FPARS(FNAME,ST)
CSUN|APOLLO
CPRIME
C       FNAME='DAXCAD>DAXCAD.KEY'
CPRIME
C
       CALL FINDU1(KUNIT,OK)
       IF (OK) THEN
          OPEN (UNIT=KUNIT,FILE=FNAME,STATUS='UNKNOWN')
C     +                  FORM='FORMATTED')
          REWIND (UNIT=KUNIT)
       END IF
C
       STRING='No. of keys defined : '
       WRITE(UNIT=KUNIT,FMT='(A,I4)') STRING,KTOTAL
       DO 10 N=1,KTOTAL
          WRITE(KUNIT,'(A,2X,A)') KCODE(N),KEYDEF(N)
  10   CONTINUE
C
       KEEP=.TRUE.
       CALL CLOSUN(KUNIT,KEEP,OK)
C
       END
C
      SUBROUTINE WRKHIT(SX,SY,OK)
C     ===========================
C1    vartype           R  R  L
C1    iostatus          I  I  O
C2
C2    tests for screen hit inside garphics area,
C2    if hit is in graphics area,logical flag OK
C2    is returned .TRUE.
C
      include 'include/wtov.inc'
C
      REAL SX,SY
      LOGICAL OK
C
CAPOLLO|SUN
      OK= SX .LE. VXMAX .AND. SX .GE. VXMIN .AND.
     +     SY .LE. VYMAX .AND. SY .GE. VYMIN
CAPOLLO|SUN
CIBM|PC386
C      OK= SX .LE. DXMAX .AND. SX .GE. DXMIN .AND.
C     +     SY .LE. DYMAX .AND. SY .GE. DYMIN
CIBM|PC386
C
      END
C
C-----------------------------------------------------------------
C
