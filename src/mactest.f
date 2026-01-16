C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 mactest.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION DAXNAM (FNAME)
C     SUBROUTINE ADPATH(PHNUM,FILNM)
C     SUBROUTINE CMPREG(TMIP,OK)
C     SUBROUTINE CMPTST()
C     SUBROUTINE MAC050(X,Y,MOK)
C     SUBROUTINE MAC200(OK)
C     SUBROUTINE MAC210(OK)
C     SUBROUTINE MAC220(OK)
C     SUBROUTINE MAC250(OK)
C     SUBROUTINE MAC260(OK)
C     SUBROUTINE MAC270(OK)
C     SUBROUTINE PARSES(BUF,FIELD,FN)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ADPATH(PHNUM,FILNM)
C     ==============================
C1    VARTYE             I(6)  C*(*)
C
C2    This routne adds a pathname to a file if
C2    it is striped clean
C
      include 'include/macro.inc'
      include 'include/wildc.inc'
 
      CHARACTER FILNM*(*),TEMP*80
      INTEGER*4 L,PHNUM,NLEN1
 
      EXTERNAL NLEN1
      L=LEN(FILNM)
      IF(INDEX(FILNM,FILCHR).EQ.0) THEN
          WRITE(UNIT=TEMP,FMT='(2A)') PATHN(PHNUM)
     +          (1:NLEN1(PATHN(PHNUM))),FILNM(1:NLEN1(FILNM))
          FILNM = TEMP(1:NLEN1(TEMP))
      ENDIF
      END
 
      SUBROUTINE CMPREG(TMIP,OK)
C     ==========================
C1    VARTYPE            I2  L
C1    IOSTATUS           I   O
C
C
C     This routine will regenerate the box limits of a component
C
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/save.inc'
C
      INTEGER*2 TMIP,ENT,I
      REAL M(3,3)
      LOGICAL OK,DELETE
C
      CMPBOX=.TRUE.
C     get details of instance
 
      CALL I3M(M)
      CALL ALLRD(TMIP,ENT,M,DELETE)
C
 
      CALL DRW066(M)
      CALL DER500(TMIP,OK)
      END
 
 
      SUBROUTINE CMPTST()
C     ===================
C1    No arguments
C
C2    This routine checks all components in the database
C2    and swithches on their subordinate layers if necessay
C
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/save.inc'
 
      REAL M(3,3)
      INTEGER*2 TTMIP,ENT,MASTP,I
      LOGICAL OK,DELETE
 
C     set flag for drawroutine
      CMPLAY=.TRUE.
      TTMIP=0
 
10    CONTINUE
      TTMIP=TTMIP+1
      IF(TTMIP.EQ.NMIPOS) GOTO 20
      CALL ALLRD (TTMIP,ENT,M,DELETE)
 
      IF((ENT.EQ.SYMBI.OR.ENT.EQ.COMPI).AND.(.NOT.DELETE)) THEN
           IF(IMBUFF(10).LE.0) GOTO 10
           CALL DRR950(IMBUFF(10),OK)
           MASTP=RLBUFF(3)
C      WRITE(10,*) '[CMPTST] MASTP= ',MASTP
           CALL DIR500(MASTP,OK)
           IF (IMBUFF(1).LT.128) THEN
               CALL DIR500(TTMIP,OK)
               CALL DRW066(M)
               CALL DIR500(MASTP,OK)
C      WRITE(10,*) '[CMPTST] MASTP= ',MASTP
C      WRITE(10,*) '[CMPTST] IMBUFF= ',(IMBUFF(I),I=1,12)
               IMBUFF(1)=IMBUFF(1)+128
               CALL DIM500(MASTP,OK)
C      WRITE(10,*) '[CMPTST] IMBUFF= ',(IMBUFF(I),I=1,12)
           ENDIF
      ENDIF
      GOTO 10
20    CONTINUE
      CMPLAY=.FALSE.
      TTMIP=0
30    CONTINUE
      TTMIP=TTMIP+1
      IF(TTMIP.EQ.NMIPOS) GOTO 40
      CALL DIR500(TTMIP,OK)
      ENT=IMBUFF(2)
 
C      WRITE(10,*) '[CMPTST] TTMIP= ',TTMIP,' ENT= ',ENT
C      WRITE(10,*) '[CMPTST] IMBUFF(1)= ',IMBUFF(1)
 
      IF( ENT.EQ.SYMBM.OR.ENT.EQ.COMPM ) THEN
 
          IF(IMBUFF(1).GT.128) THEN
 
              IMBUFF(1)=IMBUFF(1) - 128
 
              CALL DIM500(TTMIP,OK)
 
          ENDIF
      ENDIF
      GOTO 30
 
 40   CONTINUE
 
      END
 
      FUNCTION DAXNAM (FNAME)
C     ==============================
C
C1    vartype           C*(*)
C1    iostatus            I
C
C2    a file of name FNAME which is of the type
C2    DAXTYP. The completion status of the test
C2    is returned in ST,zero for success.
C2       DAXTYP - 0  DAXCAD Drawing file
C2              - 1  DAXCAD Part file
C2              - 2  DAXCAD Component file
C2              - 3  DAXCAD Symbol file
C2              - 4  DAXCAD Property table file
C2              - 5  DAXCAD section file
C2              - 6  Reserved
C2              - 7  any file file non Daxcad
C2
C2           ST - 0  File exists,and of correct type (DAXCAD)
C2           ST - 1  File exists,and of incorrect  type (DAXCAD)
C2           ST - 2  File exists,and is NOT a DAXCAD type
C2           ST - 3  File does not exist
C2           ST - 4  File is opened cannot use it
C2           ST - 5  File cannot be opened
C2           ST - 6  File type out of range
C
      include 'include/ftypes.inc'
      INTEGER*4 DAXNAM
      INTEGER*4 I,X,TUNIT,ST,UNITN
      LOGICAL EX,OP,OK
      CHARACTER*(*) FNAME,UNF*20,SEQ*20,FILETP*80
      CHARACTER*80 TCFHED,INBUFF
      CHARACTER*12 FTYPES(0:7)
C
      DATA FTYPES/'DRAWING','PART','COMPM','SYMBM',
     +           'PTABLE','SECT',' ',' '/
C
C     test for existance of file
 
      INQUIRE( FILE = FNAME, EXIST = EX )
      IF ( EX ) THEN
C
C        test for formatting condition of file
C
C
C        open and check type from header
C        find free unit
         CALL FINDU1(TUNIT,OK)
 
C        now check that is of the correct form
         OPEN (UNIT=TUNIT, FILE=FNAME,
     +         FORM='UNFORMATTED', ERR=96)
 
C        now test what kind of file we want
         IF (DAXTYP.LE.8) THEN
C
C           read in the file header
C
C           test for which type of DAXCAD file it is
            FILETP=FTYPES(DAXTYP)
            TCFHED=' '
            READ(UNIT=TUNIT,ERR=99,END=60) TCFHED
			print*, 'Drawing Header',TCFHED
            CLOSE (UNIT=TUNIT,STATUS='KEEP')
C
            IF (TCFHED.EQ.FILETP) THEN
C               daxcad file
                DAXNAM = 0
C               go home no more here
                RETURN
            ELSE
C               is it a genuine daxcad file ?
                DO 100 I = 0, 5
 
                  IF(FTYPES(I).EQ.TCFHED) THEN
C                     yes it is
                      DAXNAM = 1
                      RETURN
                  ENDIF
 
 100            CONTINUE
 
            END IF
 
C           the file exists but is not daxcad
            DAXNAM = 2
            RETURN
         ENDIF
C        cannot handle this
         DAXNAM = 6
         RETURN
 
      ELSE
 
C        file does not exist
         DAXNAM = 3
 
      END IF
 
      RETURN
 
C     *************************
C         error traps
C     *************************
 
 
 99   CONTINUE
 
C     error during read of file header read
      CLOSE (UNIT=TUNIT,STATUS='KEEP')
      DAXNAM = 2
      RETURN
 
C     file could not even be opened
 96   CONTINUE
      DAXNAM = 5
      RETURN
 
 60   CONTINUE
 
C     immediate end of file
      DAXNAM = 2
      END
C
C     ----------------------------------------------------------------
C
      SUBROUTINE MAC050(X,Y,MOK)
C     ==========================
C1    VARTYPE           R R  L
C1    IOSTAT            O O  O
C
      include 'include/macro.inc'
      include 'include/menun.inc'
      include 'include/calc.inc'
      include 'include/curwin.inc'
      CHARACTER CURWOR*80,STR1*80,TMP*80,FC
      DOUBLE PRECISION DN
      INTEGER FIELD(30),WORDC,SP,WL,NLEN1,NLEN,I
      REAL X,Y,Z
      LOGICAL MOK,OK,FOUND,SOK,GINACT
      SAVE WORDC,FIELD,SP,GINACT
      EXTERNAL ISCOM,CRUNCH,CHARCH,AEXPRN,CEXPRN,NLEN1,NLEN
      EXTERNAL EPRINT,DEPRNT
C     if this is the first call then parse the buffer.
      TIS=' '
      TIR=.FALSE.
      IF(NOWDS.EQ.0) THEN
          CALL PARSES(MACBUF,FIELD,NOWDS)
          WORDC=0
          MACTOK=.TRUE.
          SP=1
          NTOK=.FALSE.
          GINACT=.FALSE.
      ENDIF
C     increment the current word count
10    WORDC=WORDC+1
C     deparse ? the line to eliminate all commas
      FC=MACBUF(FIELD(WORDC):)
      IF(FC.EQ.';'.OR.FC.EQ.','.AND.(WORDC.LT.NOWDS)) GOTO 10
C     if this is the last word then set the flag to false.'
C     dont come back anymore
      MACTOK=WORDC.LT.NOWDS
      CURWOR=MACBUF(SP:FIELD(WORDC))
      CALL CHARCH(CURWOR)
      CALL CRUNCU(CURWOR)
      WL=NLEN1(CURWOR)
C     are we inputing or extracting information
C      WRITE(10,*) '[MAC050] CURWOR= ',CURWOR
      IF(TYPED) GOTO 150
      IF(CURWOR.EQ.'GINPUT') THEN
          CCMD= ' '
          GINACT=.TRUE.
          GINPUT=.TRUE.
 
C         Sort out the muddle of arguments in the GINPUT command
C         If none exist then it is user input
C      WRITE(10,*) '[NOWDS= ',NOWDS
          IF(NOWDS.EQ.1) THEN
              MACTOK=.FALSE.
C             set search limits to normal for user
              XWIDTH =   8.0
              YWIDTH =   8.0
              WIDTHL =   16.0
              GOTO 100
          ELSEIF(NOWDS.EQ.2) THEN
              STR1=MACBUF(FIELD(1)+1:FIELD(2))
              MACTOK=.FALSE.
          ELSEIF(NOWDS.EQ.3) THEN
C             look for a comma
              IF(MACBUF(FIELD(2):FIELD(2)).EQ.',')THEN
                  STR1=MACBUF(FIELD(1)+1:FIELD(3))
                  MACTOK=.FALSE.
              ELSE
                  STR1=MACBUF(FIELD(1)+1:FIELD(2)-1)
                  WORDC=2
              ENDIF
          ELSEIF (NOWDS.GE.4) THEN
              STR1=MACBUF(FIELD(1)+1:FIELD(3)-1)
              WORDC=3
          ENDIF
C         The string STR1 holds the arguments for the coordinate
C         combination
C      WRITE(10,*) '[MACTEST] STR1= ',STR1
          CALL XYCOOR(STR1,X,Y,Z,OK)
250       IF(OK) THEN
              MEN=0
              CCMD=' '
              GINPUT=.FALSE.
          ELSE
              GOTO 210
          ENDIF
          GOTO 100
      ENDIF
C     is the next word part of a funny blurb connected with
C     nasty things like XYCOOR
      IF(MACBUF(FIELD(WORDC):FIELD(WORDC)).EQ.',')THEN
C     concatonate this and the next word
C     by jingo it works
          WORDC=WORDC+1
          MACTOK=WORDC.LT.NOWDS
          TIS=MACBUF(SP:FIELD(WORDC))
          CALL XYCOOR(TIS,X,Y,Z,OK)
          IF(OK) THEN
              TIR=.TRUE.
              GOTO 100
          ELSE
              TIR=.FALSE.
              GOTO 210
          ENDIF
      ENDIF
C     if we are ginputing something then other commands wil be inactive
      IF(GINACT) GOTO 150
C     try to establish a menu command
C     variable commands
      IF (CURWOR(1:4).EQ.'VCOM'.AND.CURWOR(NLEN1(CURWOR):).EQ.'$') THEN
          CALL AEXPRN(CURWOR,DN,*210)
          CALL CEXPRN(CURWOR,SOK)
          IF(.NOT.SOK) GOTO 210
C          WRITE(10,*) '[MACTEST] CURWOR= ',CURWOR
      ENDIF
      CALL ISCOM(CURWOR,WL,FOUND)
      IF(CURWOR(1:3).EQ.'CO-') FOUND=.TRUE.
C      WRITE(10,*) '[MAC050] FOUND= ',FOUND
C      WRITE(10,*) '[MAC050] COMMAND NAME IS ',CURWOR
C      WRITE(10,*) '[MAC050] CURRENT MENU IS ',MEN
      IF(FOUND) THEN
          NTOK=.FALSE.
          GOTO 100
      ELSEIF(NTOK) THEN
          NTOK=.FALSE.
          CALL DEPRNT(407)
          GOTO 210
C     issue a quit to return to main menu and have another go
      ELSEIF(MFIRST) THEN
          CCMD='q'
          VNCCMD=50
          MEN=4
          CELLN=1
          WORDC=WORDC-1
          MACTOK=.TRUE.
          NTOK=.TRUE.
          GOTO 100
      ENDIF
C     ok lets try and decode the line and enter it as a string
C     this bit is also called if we get a command
C     we cannot have just anything om the first line
 150  CONTINUE
      IF(WORDC.EQ.1) GOTO 200
      STR1=MACBUF(SP:FIELD(WORDC))
C      WRITE(10,*) '[MACTEST] STR1= ',STR1
      CALL AEXPRN(STR1,DN,*210)
      CALL CEXPRN(TIS,SOK)
C     if the argument was within a set of quotes then this will occur
      IF(.NOT.SOK) THEN
          WRITE(STR1,'(G10.4)') DN
          CALL CRUNCH(STR1)
          TIS=' '//STR1
      ENDIF
c      WRITE(10,*) '[MACTEST] TIS= ',TIS
      TIR=.TRUE.
      GOTO 100
C     ***************************************************
C       fatal error has ocuured go back and stop things
C     ***************************************************
CCCCC200   CALL EPRINT(' ? ERROR IN DAXCAD COMMAND LINE')
200   CALL DEPRNT(612)
210   MOK=.FALSE.
      NOWDS=0
      MACTOK=.FALSE.
      MACCP=FIELD(WORDC)
      RETURN
C     ***************************************************
C       everything ok go back to TCURS and execute
C       the desired DAXCAD command
C     ***************************************************
100   MOK=.TRUE.
C     if we are going to repeat the command on the main menu
C     then do not update the current starting pointer
      IF(.NOT.MACTOK) NOWDS=0
      IF(.NOT.NTOK) SP=FIELD(WORDC)+1
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MAC200(OK)
C     =====================
C1    VARTYPE            L
C1    IOSTAT             O
C
C2    This routine invokes a task within the Apollo operating
C2    system. It uses an output and an input file
      include 'include/macro.inc'
      include 'include/menun.inc'
      include 'include/calc.inc'
      INTEGER*4 FIELD(30),NLEN1,NLEN,I,NF
      LOGICAL OK
      INTEGER*4 N,ST,BINDEX
      CHARACTER*40 ARGC(10),CMD,STAT*40,DUM*80,BUFF*80
      DOUBLE PRECISION DN
C
      EXTERNAL PARSES,NLEN,NLEN1,NLEN2,BINDEX,EPRINT,AEXPRN,
     +         CHILD,CRUNCH
C     Parse the invoke line with filenames
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(MACBUF)
      CALL PARSES(MACBUF,FIELD,NF)
C     are any of the arguments strings or variable
      IF(NF.EQ.1) THEN
          CALL DEPRNT(647)
          GOTO 200
      ELSE
          N=NF-1
C         Set the arument array
          DO 10 I=1,N
              ARGC(I)=MACBUF(FIELD(I)+1:FIELD(I+1))
10        CONTINUE
      ENDIF
C
      IF(N.EQ.2) THEN
          CALL DEPRNT(648)
          GOTO 200
      ENDIF
      IF(N.LT.4) THEN
          STAT='STATUS'
          N=N-1
      ELSE
          STAT=ARGC(4)
          N=N-1
      ENDIF
C     evaluate the arguments if they are charactes
      DO 20 I=1,N
          IF(INDEX(ARGC(I),'$').GT.0) THEN
               BUFF=ARGC(I)
               CALL AEXPRN(BUFF,DN,*200)
               CALL CEXPRN(DUM,OK)
               IF(.NOT.OK) GOTO 200
               ARGC(I)=DUM
          ENDIF
 20   CONTINUE
      I=BINDEX(ARGC(1),'/')
      IF(I.GT.0) THEN
          CMD=ARGC(1)
          ARGC(1)=ARGC(1)(I+1:)
      ELSE
          CALL DEPRNT(649)
          GOTO 200
      ENDIF
      WRITE(UNIT=DUM,FMT='(2A)') STAT,'=0'
      CALL AEXPRN(DUM,DN,*200)
      CALL CHILD(N,CMD,ARGC,ST)
C
      IF(ST.EQ.0) THEN
          CALL DCPRNT(650)
      ELSEIF(ST.LE.20) THEN
          WRITE(UNIT=DUM,FMT='(2A,I2)') STAT,'=',ST
          CALL CRUNCH(DUM)
          CALL AEXPRN(DUM,DN,*200)
          CALL DEPRNT(651)
      ENDIF
      OK=.TRUE.
      RETURN
200   OK=.FALSE.
      END
C
C
      SUBROUTINE MAC210(OK)
C     =====================
C1    VARTYPE            L
C1    IOSTAT             O
C
C     This routine rewinds the current files
      include 'include/macro.inc'
      include 'include/menun.inc'
      include 'include/calc.inc'
      include 'include/hdata.inc'
      LOGICAL OK
      INTEGER*4 FIELD(30),NLEN1,NLEN,I,NF,NL,LUN,SP
      INTEGER*2 FN
      INTEGER*4 N,ST,BINDEX
      CHARACTER*80 NUM,STRING
      DOUBLE PRECISION DN
      STRING=MACBUF(MACCP+1:)
C      WRITE(10,*) '[MAC210] STRING= ',STRING
      CALL PARSES(STRING,FIELD,NF)
      SP=1
      DO 10 I=1,NF
          NUM=STRING(SP:FIELD(I))
          NL=NLEN(NUM)
          IF(NL.GT.1.AND.NUM(NLEN(NUM):).EQ.',') THEN
              CALL AEXPRN(NUM(:NL-1),DN,*200)
          ELSEIF(NUM(NLEN(NUM):).NE.',') THEN
              CALL AEXPRN(NUM(:NL),DN,*200)
          ELSE
              CALL DEPRNT(657)
              GOTO 200
          ENDIF
          LUN=INT(DN)
          IF(UNITOP(LUN:LUN).EQ.'O') REWIND (UNITAR(LUN))
          SP=FIELD(I)+1
 10   CONTINUE
      OK=.TRUE.
      RETURN
200   CONTINUE
      OK=.FALSE.
      END
 
      SUBROUTINE MAC220(OK)
C     =====================
C1    VARTYPE            L
C1    IOSTAT             O
C
C     This routine sets the window search flag
      include 'include/macro.inc'
      include 'include/vntable.inc'
      LOGICAL OK
 
      OK=.TRUE.
      IF(INDEX(MACBUF,'LARGE').GT.0 ) THEN
          BIGWIN=.TRUE.
      ELSE IF( INDEX(MACBUF,'SMALL').GT.0 ) THEN
          BIGWIN=.FALSE.
       ELSE
          CALL DEPRNT(DICT01(455)//' LARGE/SMALL')
          OK=.FALSE.
      ENDIF
      END
 
      SUBROUTINE MAC250(OK)
C     ============================
C1    vartype            L
C1    iostatus           O
C
C2    This is the main gosub calling routine
C2    it takes the argument label and stores its number
C2    on a reciept of a RETURN the stack will decremrnt
C2    and the program will return to this postion
C
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
      include 'include/gosubs.inc'
 
      INTEGER*4 NLEN,FIELD(30),NFIELD
      INTEGER*4 LN,I
      LOGICAL OK
C
C
      CHARACTER*80 TLAB,STRING,LABEL
C
C
      IF(GSPNT.EQ.GSLIM) THEN
C
          CALL DEPRNT(512)
          OK = .FALSE.
          RETURN
      ENDIF
 
      STRING = MACBUF
      CALL PARSES(STRING,FIELD,NFIELD)
 
      IF( NFIELD.EQ.1 ) GOTO 30
      LABEL = STRING(FIELD(1):FIELD(2))
      CALL CRUNCH (LABEL)
      IF(LABEL(1:1).EQ.'&') LABEL = LABEL(2:)
 
      IF ( NLEN(LABEL).GT.0) THEN
 
          DO 10 I = 1,MACLAB
 
              READ(UNIT=LABUNT,REC=I,ERR=30)LN,STRING(1:20)
C             strip of apersand
              TLAB  =STRING(2:)
              IF(TLAB.EQ.LABEL) THEN
C                 increment stack pointer
                  GSPNT = GSPNT+1
C                 store current line number
C
                  GSTACK(GSPNT) = MACLIN
C                 new number
                  MACLIN = LN
                  OK = .TRUE.
                  RETURN
              ENDIF
 
 10       CONTINUE
 
      ENDIF
 
 30   CONTINUE
      CALL DEPRNT(406)
      OK = .FALSE.
 
      END
 
 
      SUBROUTINE MAC260(OK)
C     ============================
C1    vartype            L
C1    iostatus           O
C
C     This routine is the RETURN statement
C
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
      include 'include/gosubs.inc'
 
      LOGICAL OK
      INTEGER*4 NLEN,FIELD(30),NFIELD
      INTEGER*4 LN
 
      IF (GSPNT.EQ.0) THEN
 
          CALL DEPRNT(513)
 
          OK = .FALSE.
 
          RETURN
 
      ENDIF
 
      MACLIN = GSTACK(GSPNT)
      GSPNT = GSPNT - 1
      OK = .TRUE.
 
      END
 
      SUBROUTINE MAC270(OK)
 
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
      include 'include/gosubs.inc'
      include 'include/menun.inc'
 
      CHARACTER*80 FILNAM,STRING
      INTEGER*4 NLEN,FIELD(30),NF
      DOUBLE PRECISION DN
      LOGICAL SOK,OK
 
      STRING = MACBUF(MACCP+1:)
      CALL PARSES(STRING,FIELD,NF)
 
      IF(NF.GT.1) THEN
          FILNAM=STRING(FIELD(1):FIELD(2))
 
          CALL CPRINT(FILNAM)
          IF(NLEN(FILNAM).GT.0) THEN
              CALL AEXPRN( FILNAM,DN,*99 )
              CALL CEXPRN( FILNAM,SOK)
              IF(SOK) THEN
 
                  MSFOFF = .TRUE.
                  MACBUF = FILNAM
 
                  OK =.TRUE.
                  ATMAC = .TRUE.
                  MACTOK = .FALSE.
                  CCMD = 'Z'
                  RETURN
 
              ENDIF
          ENDIF
      ENDIF
      OK = .TRUE.
      CALL CLSMAC()
      MACTOK = .FALSE.
      MSFOFF = .FALSE.
      ATMAC = .FALSE.
      CCMD = 'Z'
      RETURN
99    CONTINUE
      CALL DEPRNT(460)
      OK =.FALSE.
      END
 
 
 
 
      SUBROUTINE PARSES(BUF,FIELD,FN)
C     ===============================
C1    VARTYPE          C*80 I(30)4 I4
C2    IOSTAT             I    O   O
C
C2    The command line parser will parse a line into separate
C2    field. The last point will be the end of the line. Anything
C2    within a set of parenthesis or quotes will not be
C2    be parsed.
C
      CHARACTER STRING*80,BUF*80,CH*2,NC*2
      CHARACTER SPACE,PAR1,PAR2
      INTEGER*4 NLEN1,SP,BN,FN,SL,I,FIELD(30)
      DOUBLE PRECISION DVAL
      LOGICAL TRIP,PRVSP,NSC
C     initialise the various bits
      PRVSP=.FALSE.
      TRIP=.TRUE.
      NSC=.FALSE.
      SP=0
      FN=1
      NC='  '
      CH='  '
      BN=0
      STRING=BUF
C     get the length of the string. If its a blank line then
C     NLEN1 will be of value 1 and will not cause any undue worry
      SL=NLEN1(STRING)
10    CONTINUE
      SP=SP+1
      IF(SP.GT.SL-1.OR.FN.EQ.30) GOTO 100
C     until we get a non space character keep NSC false and
C     no field will signalled on the first line
      IF(CH.NE.' ') NSC=.TRUE.
      CH(1:1)=STRING(SP:SP)
      NC(1:1)=STRING(SP+1:SP+1)
C     do not parse if inside a set of quotes
      IF(CH.NE.' ') PRVSP=.FALSE.
      IF(CH(1:1).EQ.'"') TRIP=.NOT.TRIP
C     again take a parenthesis count and only parse when 0
      IF(CH.EQ.'('.AND.TRIP) THEN
          BN=BN+1
      ELSEIF(CH.EQ.')'.AND.TRIP) THEN
          BN=BN-1
      ENDIF
C     ok we have a separator. Do we parse
      IF(CH.EQ.','.OR.CH.EQ.';'.OR.(CH.EQ.' '.AND.
     +   NC.NE.' '.AND.(.NOT.PRVSP).AND.
     1   NC.NE.','.AND.NC.NE.';').AND.NSC) THEN
          IF(BN.EQ.0) THEN
              IF(TRIP) THEN
C     yup we sure can. all tests complete go ahead and
C     increment the field pointers.
                  FIELD(FN)=SP
                  FN=FN+1
                  IF(CH.EQ.','.OR.CH.EQ.';') PRVSP=.TRUE.
              ENDIF
          ENDIF
      ENDIF
      GOTO 10
C     set the last field or indeed the only field to the
C     length of the whole string
100   FIELD(FN)=SL
C     finished, lets go home I hope it worked !
      END
C
