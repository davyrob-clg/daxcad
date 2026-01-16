C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 krlib2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION CEIL(X)
C     FUNCTION  COMPI(I)
C     FUNCTION DCEIL(X)
C     FUNCTION DECTOH(ID)
C     FUNCTION DEPS(X)
C     FUNCTION DFLOOR(X)
C     FUNCTION DPI(X)
C     FUNCTION FLOOR(X)
C     FUNCTION HEXTOD(HEXSTR)
C     FUNCTION INTAC(D)
C     FUNCTION JULIAN(D,M,Y)
C     FUNCTION LEAPYR(Y)
C     FUNCTION NDAYS(D1,M1,Y1,D2,M2,Y2)
C     FUNCTION NVRFY(STRING,C)
C     FUNCTION VERHEX(STRING)
C     FUNCTION VRFY(STRING,C)
C     FUNCTION YRDAYS(Y)
C     SUBROUTINE AEXPRN(STRING,AVALUE,*)
C     SUBROUTINE bEXPRN(STRING,AVALUE,*)
C     SUBROUTINE CEXPRN(STRNG,OK)
C     SUBROUTINE CFUNC1(STRING,A1,A2,OUTBUF,*)
C     SUBROUTINE CFUNC2(STRING,A1,OUTBUF,*)
C     SUBROUTINE CFUNC3(STRING,A1,OUTBUF,*)
C     SUBROUTINE CFUNC4(DVAL,OUTBUF,*)
C     SUBROUTINE CFUNC5(STRING,DVAL,*)
C     SUBROUTINE CFUNC6(DVAL,OUTBUF,*)
C     SUBROUTINE CFUNC7(STRING,DVAL,*)
C     SUBROUTINE CLRBIT(BITN,I)
C     SUBROUTINE CLRVAR()
C     SUBROUTINE READP(STRING,CHP1,CHP2)
C     SUBROUTINE CNPRCH(STRING)
C     SUBROUTINE CRUNCH(STRING)
C     SUBROUTINE CRUNCU(STRING)
C     SUBROUTINE DAYSFR(ND,D1,M1,Y1,D2,M2,Y2)
C     SUBROUTINE DBOUND(STRING,P1,P2,*)
C     SUBROUTINE DMPHEX(IDAT,N,BUFF)
C     SUBROUTINE DVALU(STRING,DVAL,OK)
C     SUBROUTINE GREGOK(D,M,Y,ST)
C     SUBROUTINE INCALC()
C     SUBROUTINE LSSUPP(STRING)
C     SUBROUTINE PATREP(STRING,C1,C2,OK)
C     SUBROUTINE PATRP(STRING,C1,C2)
C     SUBROUTINE POPSTK(*,*)
C     SUBROUTINE POSINS(STRING,POS,C)
C     SUBROUTINE POSREM(STRING,POS1,POS2)
C     SUBROUTINE POSREP(STRING,POS1,POS2,C)
C     SUBROUTINE RECAOP(STRING,AOP,OLEN,NARGS,CARGS)
C     SUBROUTINE RECVAR(STRING,VLEN,VP)
C     SUBROUTINE REVSTR(STRING)
C     SUBROUTINE SBOUND(STRING,VALCH,P1,P2)
C     SUBROUTINE SETBT(BITN,I)
C     SUBROUTINE SHIFT(STRING,N)
C     SUBROUTINE SUBSTC(STRING,FC,SC)
C     SUBROUTINE UNJULN(Y,J,D,M)
C     SUBROUTINE VRFINT(STRING,IVAL,OK)
C     SUBROUTINE ZSUPP1(STRING)
C     SUBROUTINE ZSUPP2(STRING)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE AEXPRN(STRING,AVALUE,*)
C     ==================================
C
      include 'include/calc.inc'
C
      INTEGER VP,VLEN,P1,TLEN,I
      LOGICAL ASSIGN
      CHARACTER*20 VARNAM
      CHARACTER*85 ERRLIN,BUFF1*80,BUFF2*80
      LOGICAL PRVOP,CONCAT,ADDING,TRIP
      DOUBLE PRECISION AVALUE
      CHARACTER*(*) STRING,DSTR*20,FORM*15
      PARAMETER (DSTR='0123456789.')
      INTEGER AOP,K,SP,NL,NLEN,J,NBKS,TP,NARGS,CARGS,EOS
      INTEGER*4 NLEN1
      CHARACTER CBUF*80,CH*2
C
C      INTEGER OSL,DSL,NOPS,NOFS
C     set the stack sizes
C      PARAMETER (OSL=40, DSL=30, NOPS=61, NOFS=NOPS-10)
C      INTEGER PREC(0:NOPS),OSTK(0:OSL),DP,OP
C      DOUBLE PRECISION DSTK(1:DSL)
C     DSTR is the numeric character set
C
      EXTERNAL CNPRCH,DBOUND,NLEN,POPSTK,RECAOP,DEPRNT,CPRINT
C
      INTRINSIC INDEX,MAX
C     PREC defines precedence of operators in an expression
      DATA PREC/0,6,2*5,4*4,2,1,3,NOFS*1/
C
C-----'AVALUE' TAKES THE VALUE OF THE NON-COMPLEX ARITHMETIC EXPRESSION
C-----IN THE 'STRING'. '*' IS AN ERROR RETURN LABEL.  (C,D,*).
C
C     NL is active length of supplied string
C      PRINT*, '[AEXP] INCOMING STRING = ',STRING(:NLEN1(STRING))
      CALL CHARCH(STRING)
C     Make sure it is upper case.
      CALL FOLDUP(STRING)
C      write(10,*) '[AEXP STRING AFTER= ',STRING(:NLEN1(STRING))
C     by default the result will be numeric
      PREC(0) = 0
      CERR=.FALSE.
      RESULT='N'
      OPTYPE='N'
      SBUF='REAL'
      ADDING=.FALSE.
      CONCAT=.FALSE.
      NL=NLEN(STRING)
      IF ( NL.EQ.0 ) THEN
          SBUF='NULL'
          RETURN
      ENDIF
C     test for assignment operator
      CH='  '
      P1=0
      TRIP=.TRUE.
      DO 500 I=1,NL
          CH=STRING(I:I)
          IF(CH.EQ.'"') TRIP=.FALSE.
          IF(CH.EQ.'='.AND.TRIP) THEN
              P1=I
              GOTO 510
          ELSEIF(CH.EQ.'='.AND.(.NOT.TRIP))THEN
              GOTO 510
          ENDIF
500   CONTINUE
510   IF (P1.LT.2) THEN
C        no assignment operator
C        attempt to evaluate expression
         SP=1
C        set flag to indicate assignment state
         ASSIGN=.FALSE.
      ELSE
C        separate assignment from expression
C        find variable name
         VARNAM=STRING(:P1-1)
C      WRITE(10,*) '[AEPRN] VARNAM= ',VARNAM
C        set scan start position past '='
         SP=P1+1
C        set flag to indicate assignment state
         ASSIGN=.TRUE.
C     set final result flag character or numeric
         IF(VARNAM(NLEN1(VARNAM):NLEN1(VARNAM)).EQ.'$') RESULT='C'
      END IF
C     ********************************************************
C     ***           START OF EXPRESSION EVALUATION         ***
C     ********************************************************
C     OSTK is the operator stack,containing op-codes for operators
C     set the first op-code to null event
      OSTK(0)=0
      PRVOP=.TRUE.
C     set character stack and pointer
      CARST(0)=' '
      CSP=0
C     set data pointer to zero
      DP=0
C     set operator pointer to zero
      OP=0
C     set count of blocks to zero
      NBKS=0
 10   CONTINUE
C     clear out the temporary buffer
      CBUF=' '
C      PRINT*, '[AEXP] CSP= ',CSP,' CARST(CSP)= ',CARST(CSP)
C      PRINT*, '[AEXP] OP= ',OP,' OSTK(OP)= ',OSTK(OP)
C      IF(DP.GT.0) PRINT*, '[AEXP] DP= ',DP,' DSTK(DP)= ',DSTK(DP)
C      PRINT*, '[AEXP] STRING= ',STRING(SP:)
      IF((CONCAT.AND.(.NOT.PRVOP).AND.OPTYPE.EQ.'N').OR.
     +   (ADDING.AND.(.NOT.PRVOP).AND.OPTYPE.EQ.'C')) THEN
          CALL DEPRNT(619)
          GOTO 99
      ENDIF
      IF (SP.LE.NL) THEN
C        skip past spaces
         IF (STRING(SP:SP).EQ.' ') THEN
C           increment scan pointer
            SP=SP+1
            GO TO 10
         END IF
C     if the next character is a set of quotes then
C     get me the string. Dont forget that two quotes
C     within the string is actually quotes
         IF(STRING(SP:SP).EQ.'"') THEN
C     hokey cokey, we got 'em
C     take every thing within the quotes
             TP=1
600          SP=SP+1
             IF(SP.GT.LEN(STRING)) THEN
                  CALL DEPRNT(620)
                  GOTO 99
             ENDIF
             CH=STRING(SP:SP)
             IF(CH.EQ.'"') THEN
                 IF(SP.LT.LEN(STRING)) THEN
                     IF(STRING(SP+1:SP+1).EQ.'"') THEN
                         SP=SP+1
                         GOTO 610
                     ENDIF
                 ENDIF
C                Just like C we bung on a null char to indicate
C                the end of a string. Clever huh ? no.
                 CBUF(TP:TP)=CHAR(0)
                 CSP=CSP+1
                 SP=SP+1
                 OPTYPE='C'
                 PRVOP=.FALSE.
C                lob it onto the character stack !
                 CARST(CSP)=CBUF
                 GOTO 10
             ENDIF
610          CBUF(TP:TP)=CH(:1)
             TP=TP+1
             GOTO 600
         ENDIF
C        test for valid numeric character
         IF (INDEX(DSTR,STRING(SP:SP)).GT.0) THEN
C-----------now process data.
            IF (.NOT.PRVOP) GO TO 99
            PRVOP=.FALSE.
C           find limits of numeric within string
            CALL DBOUND(STRING(SP:),J,K,*99)
C           K contains length of numeric
            WRITE (UNIT=FORM, FMT='(A,I2.2,A)') '(F',K,'.0)'
C           increment data pointer
            DP=DP+1
C           store numeric in data stack
            READ (UNIT=STRING(SP:), FMT=FORM, ERR=99) DSTK(DP)
C           update scan pointer to next char
            OPTYPE='N'
            SP=SP+K
            GO TO 10
         END IF
C        recover opcode AOP for operator at current scan position
C        length of operator string returned in K
C       WRITE(10,*)  '[AEXP] BEFORE RECOP'
C      WRITE(10,*)  '[AEXP] STRING= ',STRING(SP:)
         CALL RECAOP(STRING(SP:),AOP,K,NARGS,CARGS)
C      PRINT'(63X,A)','AOP,PREC(AOP),K,OSTK(K),OP,NBKS,SP'
C      PRINT'(A1,A60,A1,7I5)','"',STRING(SP:),'"'
C     +     ,AOP,PREC(AOP),K,OSTK(K),OP,NBKS,SP
C
         IF (AOP.GT.0) THEN
            ADDING=.FALSE.
            CONCAT=.FALSE.
C           NOW PROCESS ARITHMETIC OPERATORS.
*                 F(  )  , +- */ **
            GO TO (1, 2, 3, 4, 5, 6), PREC(AOP)
            GO TO 99
    1       IF (.NOT.PRVOP) GO TO 99
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           increment block counter
            NBKS=NBKS+1
C           update scan pointer
            SP=SP+K
            GO TO 10
    2       IF (PRVOP) GO TO 99
   20       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*20,*99)
C           pop the stack and perform operation defined
            CALL POPSTK(*21,*99)
C           decrement count of op blocks
   21       NBKS=NBKS-1
C           update scan pointer
            SP=SP+K
            GO TO 10
    3       IF (PRVOP) GO TO 99
   30       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*30,*99)
            PRVOP=.TRUE.
C           update scan pointer
            SP=SP+K
            GO TO 10
C     do a test for the previous op or val type. If character
C     then mark in array for a concatonation test POPST for any
C     and take of the top of the character stack
    4       IF(OPTYPE.EQ.'C'.AND.(.NOT.PRVOP).AND.AOP.EQ.5) THEN
               CONCAT=.TRUE.
               AOP=AOP+56
               PRVOP=.TRUE.
            ELSEIF (PRVOP.AND.OPTYPE.EQ.'N') THEN
               AOP=AOP+2
C              if current op precedes next in stack,execute this one
               IF (PREC(AOP).LE.PREC(OSTK(OP))) GO TO 99
            ELSE
               ADDING=.TRUE.
   40          CONTINUE
C              if current op precedes next in stack,execute this one
               IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*40,*99)
               PRVOP=.TRUE.
            END IF
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
    5       IF (PRVOP) GO TO 99
   50       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*50,*99)
            PRVOP=.TRUE.
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
    6       IF (PRVOP) GO TO 99
            PRVOP=.TRUE.
C           increment operator stack pointer
            OP=OP+1
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
         ELSE
C           now process variables
C           attempt to recover a variable from string
C       WRITE(10,*)  '[AEXPRN] STRING= ',STRING(SP:)
            CALL RECVAR(STRING(SP:),VLEN,VP)
C      WRITE(10,*) '[AEXPRN] VLEN VP =',VLEN,' ',VP
C           is it a string variable
            IF(OPTYPE.EQ.'C'.AND.VP.GT.0) THEN
               IF(.NOT.PRVOP) GOTO 99
               PRVOP=.FALSE.
               CSP=CSP+1
               CARST(CSP)=CARR(VP)
               SP=SP+VLEN
               GOTO 10
            ENDIF
            IF(VP.GT.0) THEN
C              recover variable value from array
               IF(.NOT.PRVOP) GOTO 99
               PRVOP=.FALSE.
C              increment data pointer
               DP=DP+1
C              store variable value in stack
               DSTK(DP)=VARD(VP)
               IF(STRING(SP+VLEN-1:SP+VLEN-1).EQ.'%') SBUF='INTEGER'
C              update scan pointer to next character
               SP=SP+VLEN
               GOTO 10
            ELSE
C              cannot find variable,must be error
               GO TO 99
            END IF
         END IF
      END IF
      IF (NBKS.EQ.0) THEN
   60    IF (OP.GT.0) CALL POPSTK(*60,*99)
C        if end of stack,return result, test data and stack pointers
C       WRITE(10,*) '[AEXPRN] DP= ',DP,' CSP= ',CSP
         IF((DP.EQ.1.AND.CSP.EQ.0).OR.(DP.EQ.0.AND.CSP.EQ.1)) THEN
C     ********************************************************
C     ***            END OF EXPRESSION EVALUATION          ***
C     ********************************************************
C     check type matching
            IF(ASSIGN.AND.(OPTYPE.NE.RESULT)) THEN
               CALL DEPRNT(621)
               GOTO 99
            ELSE
               RESULT=OPTYPE
            ENDIF
C           extract result from end of stack
            AVALUE=DSTK(1)
C           return with result in AVALUE
            IF(RESULT.EQ.'C'.AND.ASSIGN) THEN
C      WRITE(10,*) '[AEXPRN] CHARACTER ASSIGNMENT'
C      WRITE(10,*) '[AEXPRN] VARNAM =',VARNAM
                CALL RECVAR(VARNAM,VLEN,VP)
                IF(VP.EQ.0) THEN
                     CAP=CAP+1
                     CARNAM(CAP)=VARNAM(1:20)
                     CARR(CAP)=CARST(CSP)
                ELSE
C                    assign the variable
                     CARR(VP)=CARST(CSP)
                ENDIF
                RETURN
            ENDIF
            IF (RESULT.EQ.'N'.AND.ASSIGN) THEN
C              find the variable
C              find length of supplied variable
               VLEN=MIN(20,NLEN(VARNAM))
               VP=0
C              scan through variable list
               DO 33 I=1,NVARS
C                 find length of variable name
                  TLEN=VARL(I)
C                 test for match with variable name
                  IF(VARNAM(:VLEN).EQ.VARS(I)(:TLEN)) THEN
C                    set variable pointer
                     VP=I
                  END IF
 33            CONTINUE
               IF (VP.EQ.0) THEN
C                 variable does not exist,create it
                  NVARS=NVARS+1
                  VARS(NVARS)=VARNAM
                  VARL(NVARS)=VLEN
                  VP=NVARS
               END IF
C              assign data to variable
               IF(VARNAM(VLEN:VLEN).EQ.'%') THEN
                   VARD(VP)=AINT(AVALUE)
                   SBUF='INTEGER'
               ELSE
                   SBUF='REAL'
                   VARD(VP)=AVALUE
               ENDIF
            END IF
            RETURN
         END IF
      END IF
   99 CALL CNPRCH(STRING)
C      WRITE(10,*) '[AEXPRN] CERR= ',CERR
      IF(.NOT.CERR) CALL DEPRNT(421)
      SP=MIN(SP,NLEN(STRING))
      ERRLIN=' '
      IF(SP.GT.1) THEN
C         Write out error
          BUFF1=STRING(1:SP-1)
          BUFF2=STRING(SP:NLEN1(STRING))
          WRITE(UNIT=ERRLIN,FMT='(3A)',ERR=400) BUFF1(:NLEN1(BUFF1))
     +           ,'@ ',BUFF2(:NLEN1(BUFF2))
      ELSE
C          Error at begining of line
           WRITE (UNIT=ERRLIN, FMT='(2A)')
     +        ' @',STRING(:NLEN1(STRING))
      ENDIF
      CALL CPRINT(ERRLIN)
400   CONTINUE
C
C      WRITE (UNIT=FORM, FMT='(A,I3.3,A)') '(/3A/A,T',SP+13,',A/)'
C      WRITE (UNIT=*, FMT=FORM) ' <ERROR>    ''',STRING(:MAX(NL,SP)),
C     +    '''','POSITION:','^...'
      RETURN 1
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE bEXPRN(STRING,AVALUE,*)
C     ==================================
C
      include 'include/calc.inc'
C
      INTEGER VP,VLEN,P1,TLEN,I
      LOGICAL ASSIGN
      CHARACTER*20 VARNAM
      CHARACTER*60 ERRLIN
      LOGICAL PRVOP,CONCAT,ADDING,TRIP
      DOUBLE PRECISION AVALUE
      CHARACTER*(*) STRING,DSTR*20,FORM*15
      PARAMETER (DSTR='0123456789.')
      INTEGER AOP,K,SP,NL,NLEN,J,NBKS,TP,NARGS,CARGS,EOS
      INTEGER*4 NLEN1
      CHARACTER CBUF*80,CH*2
C
C      INTEGER OSL,DSL,NOPS,NOFS
C     set the stack sizes
C      PARAMETER (OSL=40, DSL=30, NOPS=61, NOFS=NOPS-10)
C      INTEGER PREC(0:NOPS),OSTK(0:OSL),DP,OP
C      DOUBLE PRECISION DSTK(1:DSL)
C     DSTR is the numeric character set
C
      EXTERNAL CNPRCH,DBOUND,NLEN,POPSTK,RECAOP,DEPRNT,CPRINT
C
      INTRINSIC INDEX,MAX
C     PREC defines precedence of operators in an expression
      DATA PREC/0,6,2*5,4*4,2,1,3,NOFS*1/
C
C-----'AVALUE' TAKES THE VALUE OF THE NON-COMPLEX ARITHMETIC EXPRESSION
C-----IN THE 'STRING'. '*' IS AN ERROR RETURN LABEL.  (C,D,*).
C
C     NL is active length of supplied string
C      PRINT*, '[AEXP] INCOMING STRING = ',STRING(:NLEN1(STRING))
      CALL CHARCH(STRING)
C     Make sure it is upper case.
      CALL FOLDUP(STRING)
C      PRINT*, '[AEXP STRING AFTER= ',STRING(:NLEN1(STRING))
C     by default the result will be numeric
      RESULT='N'
      OPTYPE='N'
      SBUF='REAL'
      ADDING=.FALSE.
      CONCAT=.FALSE.
      NL=NLEN(STRING)
      IF ( NL.EQ.0 ) THEN
          SBUF='NULL'
          RETURN
      ENDIF
C     test for assignment operator
      CH='  '
      P1=0
      TRIP=.TRUE.
      DO 500 I=1,NL
          CH=STRING(I:I)
          IF(CH.EQ.'"') TRIP=.FALSE.
          IF(CH.EQ.'='.AND.TRIP) THEN
              P1=I
              GOTO 510
          ELSEIF(CH.EQ.'='.AND.(.NOT.TRIP))THEN
              GOTO 510
          ENDIF
500   CONTINUE
510   IF (P1.LT.2) THEN
C        no assignment operator
C        attempt to evaluate expression
         SP=1
C        set flag to indicate assignment state
         ASSIGN=.FALSE.
      ELSE
C        separate assignment from expression
C        find variable name
         VARNAM=STRING(:P1-1)
C        set scan start position past '='
         SP=P1+1
C        set flag to indicate assignment state
         ASSIGN=.TRUE.
C     set final result flag character or numeric
         IF(VARNAM(NLEN1(VARNAM):NLEN1(VARNAM)).EQ.'$') RESULT='C'
      END IF
C     ********************************************************
C     ***           START OF EXPRESSION EVALUATION         ***
C     ********************************************************
C     OSTK is the operator stack,containing op-codes for operators
C     set the first op-code to null event
      OSTK(0)=0
      PRVOP=.TRUE.
C     set character stack and pointer
      CARST(0)=' '
      CSP=0
C     set data pointer to zero
      DP=0
C     set operator pointer to zero
      OP=0
C     set count of blocks to zero
      NBKS=0
 10   CONTINUE
C     clear out the temporary buffer
      CBUF=' '
C      PRINT*, '[AEXP] CSP= ',CSP,' CARST(CSP)= ',CARST(CSP)
C      PRINT*, '[AEXP] OP= ',OP,' OSTK(OP)= ',OSTK(OP)
C      IF(DP.GT.0) PRINT*, '[AEXP] DP= ',DP,' DSTK(DP)= ',DSTK(DP)
C      PRINT*, '[AEXP] STRING= ',STRING(SP:)
      IF((CONCAT.AND.(.NOT.PRVOP).AND.OPTYPE.EQ.'N').OR.
     +   (ADDING.AND.(.NOT.PRVOP).AND.OPTYPE.EQ.'C')) THEN
          CALL DEPRNT(619)
          GOTO 99
      ENDIF
      IF (SP.LE.NL) THEN
C        skip past spaces
         IF (STRING(SP:SP).EQ.' ') THEN
C           increment scan pointer
            SP=SP+1
            GO TO 10
         END IF
C     if the next character is a set of quotes then
C     get me the string. Dont forget that two quotes
C     within the string is actually quotes
         IF(STRING(SP:SP).EQ.'"') THEN
C     hokey cokey, we got 'em
C     take every thing within the quotes
             TP=1
600          SP=SP+1
             IF(SP.GT.LEN(STRING)) THEN
                  CALL DEPRNT(620)
                  GOTO 99
             ENDIF
             CH=STRING(SP:SP)
             IF(CH.EQ.'"') THEN
                 IF(SP.LT.LEN(STRING)) THEN
                     IF(STRING(SP+1:SP+1).EQ.'"') THEN
                         SP=SP+1
                         GOTO 610
                     ENDIF
                 ENDIF
C                Just like C we bung on a null char to indicate
C                the end of a string. Clever huh ? no.
                 CBUF(TP:TP)=CHAR(0)
                 CSP=CSP+1
                 SP=SP+1
                 OPTYPE='C'
                 PRVOP=.FALSE.
C                lob it onto the character stack !
                 CARST(CSP)=CBUF
                 GOTO 10
             ENDIF
610          CBUF(TP:TP)=CH(:1)
             TP=TP+1
             GOTO 600
         ENDIF
C        test for valid numeric character
         IF (INDEX(DSTR,STRING(SP:SP)).GT.0) THEN
C-----------now process data.
            IF (.NOT.PRVOP) GO TO 99
            PRVOP=.FALSE.
C           find limits of numeric within string
            CALL DBOUND(STRING(SP:),J,K,*99)
C           K contains length of numeric
            WRITE (UNIT=FORM, FMT='(A,I2.2,A)') '(F',K,'.0)'
C           increment data pointer
            DP=DP+1
C           store numeric in data stack
            READ (UNIT=STRING(SP:), FMT=FORM, ERR=99) DSTK(DP)
C           update scan pointer to next char
            OPTYPE='N'
            SP=SP+K
            GO TO 10
         END IF
C        recover opcode AOP for operator at current scan position
C        length of operator string returned in K
         CALL RECAOP(STRING(SP:),AOP,K,NARGS,CARGS)
C      PRINT'(63X,A)','AOP,PREC(AOP),K,OSTK(K),OP,NBKS,SP'
C      PRINT'(A1,A60,A1,7I5)','"',STRING(SP:),'"'
C     +     ,AOP,PREC(AOP),K,OSTK(K),OP,NBKS,SP
C
         IF (AOP.GT.0) THEN
            ADDING=.FALSE.
            CONCAT=.FALSE.
C           NOW PROCESS ARITHMETIC OPERATORS.
*                 F(  )  , +- */ **
            GO TO (1, 2, 3, 4, 5, 6), PREC(AOP)
            GO TO 99
    1       IF (.NOT.PRVOP) GO TO 99
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           increment block counter
            NBKS=NBKS+1
C           update scan pointer
            SP=SP+K
            GO TO 10
    2       IF (PRVOP) GO TO 99
   20       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*20,*99)
C           pop the stack and perform operation defined
            CALL POPSTK(*21,*99)
C           decrement count of op blocks
   21       NBKS=NBKS-1
C           update scan pointer
            SP=SP+K
            GO TO 10
    3       IF (PRVOP) GO TO 99
   30       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*30,*99)
            PRVOP=.TRUE.
C           update scan pointer
            SP=SP+K
            GO TO 10
C     do a test for the previous op or val type. If character
C     then mark in array for a concatonation test POPST for any
C     and take of the top of the character stack
    4       IF(OPTYPE.EQ.'C'.AND.(.NOT.PRVOP).AND.AOP.EQ.5) THEN
               CONCAT=.TRUE.
               AOP=AOP+56
               PRVOP=.TRUE.
            ELSEIF (PRVOP.AND.OPTYPE.EQ.'N') THEN
               AOP=AOP+2
C              if current op precedes next in stack,execute this one
               IF (PREC(AOP).LE.PREC(OSTK(OP))) GO TO 99
            ELSE
               ADDING=.TRUE.
   40          CONTINUE
C              if current op precedes next in stack,execute this one
               IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*40,*99)
               PRVOP=.TRUE.
            END IF
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
    5       IF (PRVOP) GO TO 99
   50       CONTINUE
C           if current op precedes next in stack,execute this one
            IF (PREC(AOP).LE.PREC(OSTK(OP))) CALL POPSTK(*50,*99)
            PRVOP=.TRUE.
C           increment operator stack pointer
            OP=OP+1
C           store opcode in stack
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
    6       IF (PRVOP) GO TO 99
            PRVOP=.TRUE.
C           increment operator stack pointer
            OP=OP+1
            OSTK(OP)=AOP
C           update scan pointer
            SP=SP+K
            GO TO 10
         ELSE
C           now process variables
C           attempt to recover a variable from string
            CALL RECVAR(STRING(SP:),VLEN,VP)
C      PRINT*, '[AEXP] STRING= ',STRING(SP:),' VLEN VP =',VLEN,' ',VP
C           is it a string variable
            IF(OPTYPE.EQ.'C'.AND.VP.GT.0) THEN
               IF(.NOT.PRVOP) GOTO 99
               PRVOP=.FALSE.
               CSP=CSP+1
               CARST(CSP)=CARR(VP)
               SP=SP+VLEN
               GOTO 10
            ENDIF
            IF(VP.GT.0) THEN
C              recover variable value from array
               IF(.NOT.PRVOP) GOTO 99
               PRVOP=.FALSE.
C              increment data pointer
               DP=DP+1
C              store variable value in stack
               DSTK(DP)=VARD(VP)
               IF(STRING(SP+VLEN-1:SP+VLEN-1).EQ.'%') SBUF='INTEGER'
C              update scan pointer to next character
               SP=SP+VLEN
               GOTO 10
            ELSE
C              cannot find variable,must be error
               GO TO 99
            END IF
         END IF
      END IF
      IF (NBKS.EQ.0) THEN
   60    IF (OP.GT.0) CALL POPSTK(*60,*99)
C        if end of stack,return result, test data and stack pointers
         IF((DP.EQ.1.AND.CSP.EQ.0).OR.(DP.EQ.0.AND.CSP.EQ.1)) THEN
C     ********************************************************
C     ***            END OF EXPRESSION EVALUATION          ***
C     ********************************************************
C     check type matching
            IF(ASSIGN.AND.(OPTYPE.NE.RESULT)) THEN
               CALL DEPRNT(621)
               GOTO 99
            ELSE
               RESULT=OPTYPE
            ENDIF
C           extract result from end of stack
            AVALUE=DSTK(1)
C           return with result in AVALUE
            IF(RESULT.EQ.'C'.AND.ASSIGN) THEN
                CALL RECVAR(VARNAM,VLEN,VP)
                IF(VP.EQ.0) THEN
                     CAP=CAP+1
                     CARNAM(CAP)=VARNAM(1:20)
                     CARR(CAP)=CARST(CSP)
                ELSE
C                    assign the variable
                     CARR(VP)=CARST(CSP)
                ENDIF
                RETURN
            ENDIF
            IF (RESULT.EQ.'N'.AND.ASSIGN) THEN
C              find the variable
C              find length of supplied variable
               VLEN=MIN(20,NLEN(VARNAM))
               VP=0
C              scan through variable list
               DO 33 I=1,NVARS
C                 find length of variable name
                  TLEN=VARL(I)
C                 test for match with variable name
                  IF(VARNAM(:VLEN).EQ.VARS(I)(:TLEN)) THEN
C                    set variable pointer
                     VP=I
                  END IF
 33            CONTINUE
               IF (VP.EQ.0) THEN
C                 variable does not exist,create it
                  NVARS=NVARS+1
                  VARS(NVARS)=VARNAM
                  VARL(NVARS)=VLEN
                  VP=NVARS
               END IF
C              assign data to variable
               IF(VARNAM(VLEN:VLEN).EQ.'%') THEN
                   VARD(VP)=AINT(AVALUE)
                   SBUF='INTEGER'
               ELSE
                   SBUF='REAL'
                   VARD(VP)=AVALUE
               ENDIF
            END IF
            RETURN
         END IF
      END IF
   99 CALL CNPRCH(STRING)
      CALL DEPRNT(421)
      SP=MIN(SP,NLEN(STRING))
      ERRLIN='         '//STRING(:MAX(NL,SP))
      CALL CPRINT(ERRLIN)
      WRITE (UNIT=FORM, FMT='(A,I3.3,A)') '(A,T',SP+9,',A)'
      WRITE (UNIT=ERRLIN, FMT=FORM)  'Position ','^...'
      CALL CPRINT(ERRLIN)
C
C      WRITE (UNIT=FORM, FMT='(A,I3.3,A)') '(/3A/A,T',SP+13,',A/)'
C      WRITE (UNIT=*, FMT=FORM) ' <ERROR>    ''',STRING(:MAX(NL,SP)),
C     +    '''','POSITION:','^...'
      RETURN 1
      END
C
C     ---------------------------------------------------------
C
      FUNCTION BITSET(BITN,I)
C     =======================
C
C1    vartype    L    I   I
C1    iostatus   O    I   O
C
C2    Function Bitset rwturns a logical
C2    result based on the condition of bit
C2    number BITN in the integer I.
C2    Bits are numbered from 0 to 31.
C2    If bit BITN is set,then BITSET is
C2    returned true,otherwise false.
C
      INTEGER*4 BITN,I,N1,N2,J4,BN,K,ITMP
      INTEGER I2I4
      INTEGER*2 J2(2),I4I2
      LOGICAL BITSET
      EQUIVALENCE (J4,J2)
      EXTERNAL I2I4,I4I2
C
C     get bit number
      BN=ABS(MOD(BITN,32))
C     copy the word
      J4=I
C     copy each half word
      N1=I2I4(J2(1))
      N2=I2I4(J2(2))
      IF (BN.GT.15) THEN
C        upper word affected
         BN=BN-16
         K=2**BN
         ITMP=N1/K
      ELSE
C        lower word affected
         K=2**BN
         ITMP=N2/K
      END IF
C     return the logical result
      BITSET=MOD(ITMP,2).NE.0
C
      END
C
C     -----------------------------------------------------
C
      FUNCTION CEIL(X)
C     ================
C1    vartype    R   R
C1    iostatus   O   I
C
C2    Rounds X to the nearest integer
C2    value which is .GE. 'X'
C
      REAL CEIL,X
      DOUBLE PRECISION DCEIL
      EXTERNAL DCEIL
      INTRINSIC DBLE,REAL
C
      CEIL=REAL(DCEIL(DBLE(X)))
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE CEXPRN(STRNG,OK)
C     ===========================
C1    vartype            c*80 l
C1    iostat              o   o
C
C2    This routine is the character link between AEXPRN.
C2    The operation type in AEPRN may be character or numeric
C2    this routine determines this. If the string sent to AEPRN
C2    is a character string then the reult will be returned in
C2    STRNG and the logical will be true. If the result was numeric
C2    then the value will be returned in the double precision variable
C2    in the AEPRN routine as before.
C
      include 'include/calc.inc'
      LOGICAL OK
      CHARACTER STRNG*80,TXTTMP*80
      DOUBLE PRECISION DVAL
      INTEGER*4 NLEN1,I
      OK= RESULT.EQ.'C'
      IF(OK) THEN
          STRNG=CARST(1)
          I=NLEN1(STRNG)
C         remember and chop off that naughty null character at the end
          IF(ICHAR(STRNG(I:I)).EQ.0) THEN
              IF(I.EQ.1) THEN
                  STRNG=' '
              ELSE
                  TXTTMP=STRNG(:I-1)
                  STRNG=TXTTMP
              ENDIF
          ENDIF
          OK=.TRUE.
          LENSTR=I-1
      ENDIF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC1(STRING,A1,A2,OUTBUF,*)
C     ========================================
C1    VARTYPE            C*80  I4 I4  C*80 AEP
C2    IOSTAT              I   I   I     O   O
C
C3    This function performs the basic MID$ operation
C3    the first argument is the startung position on
C3    the string and the second arg is the number of
C3    characters from that point.
C
      CHARACTER*80 STRING,OUTBUF
      INTEGER A1,A2
C
      IF(A1.LT.0.OR.A1.GT.80.OR.A2.LT.0.OR.A2.GT.80) THEN
          OUTBUF=' '
          CALL DEPRNT(634)
          RETURN 1
      ELSE
          OUTBUF=STRING(A1:A1+A2-1)//CHAR(0)
      ENDIF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC2(STRING,A1,OUTBUF,*)
C     =====================================
C1    VARTYPE             C*80 I4 C*80  AEP
C2    IOSTAT                I   I   O    O
C
C3    This function performs the basic LEFT$ operation
C3    the first argument is the startung position on
C
      CHARACTER*80 STRING,OUTBUF
      INTEGER A1,LP
      INTEGER*4 NLEN1
C
      IF(A1.LT.0.OR.A1.GT.80) THEN
          OUTBUF=' '
          CALL DEPRNT(634)
          RETURN 1
      ELSE
          LP=NLEN1(STRING)
          IF(A1.GT.LP) THEN
              OUTBUF=STRING
          ELSE
              OUTBUF=STRING(1:A1)//CHAR(0)
          ENDIF
      ENDIF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC3(STRING,A1,OUTBUF,*)
C     =====================================
C1    VARTYPE             C*80 I4 C*80  AEP
C2    IOSTAT                I   I   O    O
C
C3    This function performs the basic RIGHT$ operation
C3    the first argument is the end postion from the string
C
      CHARACTER*80 STRING,OUTBUF
      INTEGER A1,RP
      INTEGER*4 NLEN1
C
      IF(A1.LT.0.OR.A1.GT.80) THEN
          OUTBUF=' '
          CALL DEPRNT(634)
          RETURN 1
      ELSE
          RP=NLEN1(STRING)-A1
          IF(RP.LT.1) THEN
              OUTBUF=STRING
          ELSE
              OUTBUF=STRING(RP:)
          ENDIF
      ENDIF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC4(DVAL,OUTBUF,*)
C     ================================
C1    VARTYPE            D    C*80 AEP
C2    IOSTAT             I      O
C3    This routine performs the BASIC STR$ function
C3    which in laymans terms means converting a numeric
C4    variable to a string variable
C
      include 'include/calc.inc'
      CHARACTER*80 INTFIL,OUTBUF,TXTTMP
      DOUBLE PRECISION DVAL
      INTEGER*4 NLEN1
      EXTERNAL NLEN1
      INTRINSIC CHAR
C
      OUTBUF=' '
      IF(SBUF.EQ.'INTEGER') THEN
          WRITE(INTFIL,'(I12)') INT(DVAL)
      ELSE
          WRITE(INTFIL,'(F20.5)') DVAL
      ENDIF
      READ(INTFIL,'(A)') OUTBUF
      TXTTMP=OUTBUF(:NLEN1(OUTBUF))//CHAR(0)
      OUTBUF=TXTTMP
      CALL CRUNCH(OUTBUF)
      RETURN
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC5(STRING,DVAL,*)
C     ================================
C1    VARTYPE            C*80   D  AEP
C2    IOSTAT              I     O
C3    This routine performs the BASIC VAL function
C3    which in laymans terms means converting a string
C4    variable to a numeric variable.
C
      CHARACTER*80 INTFIL,STRING
      INTEGER*4 J,K
      DOUBLE PRECISION DVAL
C
      CALL DBOUND(STRING,J,K,*99)
      WRITE(INTFIL,'(A)') STRING(J:K)
      READ(UNIT=INTFIL,FMT='(G15.0)',ERR=99) DVAL
      RETURN
99    DVAL=0.0
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC6(DVAL,OUTBUF,*)
C     ================================
C1    VARTYPE            D    C*80 AEP
C2    IOSTAT             I      O
C3    This routine performs the BASIC CHR$ function
C3    which in laymans terms means converting a numeric
C4    variable to a string variable
C
      CHARACTER*80 INTFIL,OUTBUF
      INTEGER ASCII
      DOUBLE PRECISION DVAL
C
      OUTBUF=' '
      ASCII=NINT(DVAL)
      IF(ASCII.LT.0.OR.ASCII.GT.255) THEN
          CALL DEPRNT(630)
          RETURN 1
      ELSE
          OUTBUF(1:2)=CHAR(ASCII)//CHAR(0)
      ENDIF
      RETURN
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CFUNC7(STRING,DVAL,*)
C     ================================
C1    VARTYPE            C*80   D  AEP
C2    IOSTAT              I     O
C3    This routine performs the BASIC ASC function
C3    which in laymans terms means converting a string
C4    variable to a numeric variable.
C
      CHARACTER*80 INTFIL,STRING
      INTEGER NUM
      DOUBLE PRECISION DVAL
C
      NUM=ICHAR(STRING(1:1))
      DVAL=DBLE(NUM)
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CLRBIT(BITN,I)
C     =========================
C
C1    vartype    L    I   I
C1    iostatus   O    I   O
C
C2    Function CLRBIT clears bit
C2    number BITN in the integer I.
C2    Bits are numbered from 0 to 31.
C
      INTEGER*4 BITN,I,N1,N2,J4,BN,K,ITMP
      INTEGER I2I4
      INTEGER*2 J2(2),I4I2
      LOGICAL BITSET
      EQUIVALENCE (J4,J2)
      EXTERNAL I2I4,I4I2
C
C     get bit number
      BN=ABS(MOD(BITN,32))
C     copy the word
      J4=I
C     copy each half word
      N1=I2I4(J2(1))
      N2=I2I4(J2(2))
      IF (BN.GT.15) THEN
C        upper word affected
         BN=BN-16
         K=2**BN
         ITMP=N1/K
         BITSET=MOD(ITMP,2).NE.0
C        reset the bit by subtraction
         IF (BITSET) N1=N1-K
      ELSE
C        lower word affected
         K=2**BN
         ITMP=N2/K
         BITSET=MOD(ITMP,2).NE.0
C        reset the bit by subtraction
         IF (BITSET) N2=N2-K
      END IF
C     copy the upper and lower words back
      J2(1)=I4I2(N1)
      J2(2)=I4I2(N2)
C     now get return value
      I=J4
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE CLRVAR()
C     ===================
C
C2    Subroutine CLRVAR clears all variables
C2    currently declared,and resets the variable
C2    pointer to zero.
C
      include 'include/calc.inc'
C
      INTEGER P1
C
C     clear variable names array
      DO 5 P1=1,NVARS
         VARS(P1)=' '
 5    CONTINUE
      NVARS=0
C
      END
C
C     ---------------------------------------------------------
C
C      SUBROUTINE READP(STRING,CHP1,CHP2)
C     ===================================
C
C      INTEGER P1,P2,NLEN
C      CHARACTER*(*) STRING,CHP1,CHP2
C      EXTERNAL NLEN
C-----READS 'STRING' FROM TERMINAL WITH LINE PROMPTS.  (C,C,C).
C-----'AMPERSAND,CR' ALLOWS CONTINUATION ON NEXT LINE.
C-----TERMINATES AT END OF 'STRING' OR WHEN A 'CR' IS READ.
C      WRITE (UNIT=*, FMT='(A)') CHP1
C      P1=1
C   10 READ (UNIT=*, FMT='(A)') STRING(P1:)
C      P2=P1+NLEN(STRING(P1:))-1
C      IF (P1.LE.P2) THEN
C         IF (STRING(P2:P2).EQ.'&') THEN
C            WRITE (UNIT=*, FMT='(A)') CHP2
C            P1=P2
C            GO TO 10
C         END IF
C      END IF
C      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CNPRCH(STRING)
C     =========================
C1    vartype            C*(*)
C1    iostatus            IO
C
C2    Changes all non-printing characters in
C2    STRING to '?'
C
      INTEGER J,NLEN
      CHARACTER STRING*(*),CH,SPACE,TILDE
      PARAMETER (CH='?', SPACE=' ', TILDE='~')
      EXTERNAL NLEN
      INTRINSIC LGT,LLT
C
      DO 10, J=1,NLEN(STRING)
         IF (LLT(STRING(J:J),SPACE) .OR. LGT(STRING(J:J),TILDE))
     +      STRING(J:J)=CH
   10 CONTINUE
C
      END
C
C     ------------------------------------------------
C
      FUNCTION  COMPI(I)
C     ==================
C
C1    vartype     I    I
C1    iostatus    O    I
C
C2    Function COMPI complements all bits
C2    within the passed integer I and returns
C2    the result.
C
      INTEGER COMPI,I,J,K,I4
      LOGICAL BITSET
      EXTERNAL BITSET
C
C     copy argument
      K=I
C     test all bits and complement state
      DO 10 J=0,31
         IF (BITSET(J,K)) THEN
C           must clear bit
            CALL CLRBIT(J,K)
         ELSE
C           must set bit
            CALL SETBT(J,K)
         END IF
 10   CONTINUE
      COMPI=K
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE CRUNCH(STRING)
C     =========================
C1    vartype            C*(*)
C1    iostatus             IO
C
C2    Removes all spaces from STRING
C2    and clears remainder of string
C2    to blanks.
C
      INTEGER J,NL,NLEN,SP
      CHARACTER STRING*(*)
      EXTERNAL NLEN
      INTRINSIC INDEX
C
      NL=NLEN(STRING)
      IF (NL.GT.0) THEN
         SP=INDEX(STRING(:NL),' ')
         IF (SP.GT.0) THEN
            DO 10, J=SP+1,NL
               IF (STRING(J:J).NE.' ') THEN
                  STRING(SP:SP)=STRING(J:J)
                  SP=SP+1
               END IF
   10       CONTINUE
            STRING(SP:)=' '
         END IF
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE CRUNCU(STRING)
C     =========================
C1    vartype            C*(*)
C1    iostatus             IO
C
C2    Removes all '_' from STRING
C2    and clears remainder of string
C2    to blanks.
C
      INTEGER J,NL,NLEN,SP
      CHARACTER STRING*(*)
      EXTERNAL NLEN
      INTRINSIC INDEX
C
      NL=NLEN(STRING)
      IF (NL.GT.0) THEN
         SP=INDEX(STRING(:NL),'_')
         IF (SP.GT.0) THEN
            DO 10, J=SP+1,NL
               IF (STRING(J:J).NE.'_') THEN
                  STRING(SP:SP)=STRING(J:J)
                  SP=SP+1
               END IF
   10       CONTINUE
            STRING(SP:)=' '
         END IF
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE DAYSFR(ND,D1,M1,Y1,D2,M2,Y2)
C     =======================================
C
C1    vartype           I  I  I  I  I  I  I
C1    iostatus          I  I  I  I  O  O  O
C
C2    Subroutine DAYFR returns the date D2,M2,Y2
C2    which is ND days after date D1,M1,Y1.
C2    If the start date or end date are out of the
C2    GREGORIAN range, then D2,M2,Y2 are returned
C2    with zero values.
C2    A negative number of days will generate a date
C2    that number of days before the start date.
C
      INTEGER ND,D1,M1,Y1,D2,M2,Y2
      INTEGER J1,J2,N1,N2,NN,TY
      INTEGER JULIAN,YRDAYS
C
C     clear return values
      Y2=0
      M2=0
      D2=0
C     get julian day of starting date
      J1=JULIAN(D1,M1,Y1)
      IF (J1.EQ.0) RETURN
C     get number of days in year of date 1
      N1=YRDAYS(Y1)
      IF (ND.GT.0) THEN
C        forward date required
C        test for target in same year
         J2=J1+ND
         IF (J2.LE.N1) THEN
C           within same year,simply evaluate
            CALL UNJULN(Y1,J2,D2,M2)
            Y2=Y1
            RETURN
         END IF
C        must be in a succeeding year
C        get number of days beyond current year
         NN=ND-N1+J1
C        set target year to next year
         TY=Y1+1
 5       CONTINUE
         IF (NN.GT.0) THEN
            N2=YRDAYS(TY)
            NN=NN-N2
            TY=TY+1
            GOTO 5
         END IF
C        gone beyond year of interest
         NN=NN+N2
         Y2=TY-1
C        get date in year 2
         CALL UNJULN(Y2,NN,D2,M2)
      ELSE
C        backward date required
C        test for target in same year
         J2=J1+ND
         IF (J2.GT.0) THEN
C           within same year,simply evaluate
            CALL UNJULN(Y1,J2,D2,M2)
            Y2=Y1
            RETURN
         END IF
C        must be in a preceeding year
C        get number of days before current year
         NN=ABS(ND+J1)
C        set target year to previous year
         TY=Y1-1
         N2=0
 6       CONTINUE
C        nn is number of days back into target year
         IF (NN.GE.0) THEN
            N2=YRDAYS(TY)
            NN=NN-N2
            TY=TY-1
            GOTO 6
         END IF
C        gone beyond year of interest
C        get JULIAN day in target year
C        alraedy have it in complement form
         NN=-NN
C        if zero ,then last day of year
         IF (NN.EQ.0) NN=N2
         Y2=TY+1
C        get date in year 2
         CALL UNJULN(Y2,NN,D2,M2)
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE DBOUND(STRING,P1,P2,*)
C     =================================
C1    vartype            C*(*) I  I
C1    iostatus             I   O  O
C
C2    Subroutine DBOUND finds the bounds of the first
C2    numeric within STRING,returning P1 pointing to
C2    first char of numeric,and P2 pointing to last
C2    char of numeric.
C
      INTEGER NL,NLEN,NVRFY,P1,P2,TABLE(1:8,0:4),CH(0:15),ROW
      CHARACTER*(*) DSTR1*13,DSTR2*15,STRING
      PARAMETER (DSTR1='0123456789.+-', DSTR2='0123456789.+-ED')
      EXTERNAL NLEN,NVRFY
      INTRINSIC INDEX
      DATA CH/0,10*1, 2, 2*3, 2*4/,
     +     TABLE/0,2*-1,3*0,-1,0, 2*2,2*3,2,3*7, 4,3,-1,0,4,0,-1,0,
     +     5,2*-1,2*0,8,-1,0, 0,2*6,3*0,-1,0/
C
      P1=0
C
C     PTI 22-6-92  Temp fix for DBOUND bugs.  Something overwrites this data segement
C                  We think its the compilers
C
      CH(0) = 0
      NL=NLEN(STRING)
      IF (NL.EQ.0) THEN
C        blank string,set P1=P2
         P1=1
         P2=P1
C        return error condition for blank
         RETURN 1
      END IF
 5    CONTINUE
C
      P1=NVRFY(STRING,DSTR1)
C
      IF (P1.GT.0) THEN
         P2=P1
         IF (NL.EQ.P1) THEN
C           only one character in string
            RETURN
         END IF
         ROW=1
   10    IF (P2.LE.NL) THEN
            ROW=TABLE(ROW,CH(INDEX(DSTR2,STRING(P2:P2))))
            IF (ROW.GT.0) THEN
               P2=P2+1
               GO TO 10
            END IF
            IF (ROW.EQ.0) RETURN 1
         ELSE IF (TABLE(ROW,0).EQ.0) THEN
            RETURN 1
         END IF
         P2=P2-1
      ELSE
         P2=0
      END IF
      END
C
C     ------------------------------------------------
C
      FUNCTION DCEIL(X)
C     =================
C1    vartype    D   D
C1    iostatus   O   I
C
C2    Rounds X to the nearest integer
C2    value which is .GE. 'X'
C
      DOUBLE PRECISION DCEIL,X
      INTRINSIC AINT
C
      DCEIL=AINT(X)
      IF (DCEIL.LT.X) DCEIL=DCEIL+1D0
C
      END
C
C     ------------------------------------------------
C
      FUNCTION DECTOH(ID)
C     ===================
C
C1    vartype   C*(*) I
C1    iostatus    O   I
C
C2    converts a decimal number passed in 'ID'
C2    to a hexadecimal character string,of a
C2    length declared in the calling program.
C2    If the passed length of DECTOH is not
C2    sufficient for the converted string,
C2    then the return string is filled with
C2    asterisks to indicate overflow.
C
      INTEGER*4 ID,D,D1,I,N,MOD,NLEN1,TMP,L
      LOGICAL NEG
      CHARACTER*(*) DECTOH
      CHARACTER*8 H1
      CHARACTER CHAR
      EXTERNAL NLEN1
C
C     copy input to local buffer
      D=ID
C     clear working strings
      H1=' '
      DECTOH=' '
C     find length of return string
      L=LEN(DECTOH)
C     test for negative number
      NEG=D.LT.0
      IF (NEG) THEN
C        clear sign bit,and add in later
         CALL CLRBIT(31,D)
      END IF
C     use local for manipulation of  entry value
      TMP=D
      I=1
 100  CONTINUE
C     get lowest hex digit
      D1=MOD(TMP,16)
C     strip lowest hex digit from number
      TMP=TMP/16
      IF (I.EQ.8 .AND. NEG) THEN
C        negative must set sign bit
         D1=D1+8
      END IF
      IF (D1.LT.10) THEN
C        numeral 0-9
         H1(I:I)=CHAR(48+D1)
      ELSE
C        character A-F
         H1(I:I)=CHAR(55+D1)
      END IF
C     increment hex digit count
      I=I+1
C     keep going until finished
      IF (I.LE.8) GOTO 100
C     find length of converted string
      I=NLEN1(H1)
      IF (I.GT.L) THEN
C        if hex string longer than passed length
C        of dectoh then fill with asterisks to indicate
C        overflow condition.
         DO 200 N=1,L
            DECTOH(N:N)='*'
 200     CONTINUE
      ELSE
C        now got hex string in reverse order
C        got to fix that by reversal
         CALL REVSTR(H1(1:L))
         DECTOH=H1(1:L)
      END IF
C
      END
C
C     -----------------------------------------------------
C
      FUNCTION DEPS(X)
C     ================
C1    vartype   D   D
C1    iostatus  O   I
C
C2    Returns smallest modal tolerance
C2    on 'X'
C
      DOUBLE PRECISION DEPS,MCEPS,X
C      PARAMETER (MCEPS=2D0**(-46))
      INTRINSIC ABS
C
      MCEPS=2D0**(-46)
      DEPS=ABS(X)*MCEPS
C
      END
C
C     ------------------------------------------------
C
      FUNCTION DFLOOR(X)
C     ==================
C1    vartype    D   D
C1    iostatus   O   I
C
C2    Rounds X to the nearest integer
C2    value which is .LE. 'X'
C
      DOUBLE PRECISION DFLOOR,X
      INTRINSIC AINT
C
      DFLOOR=AINT(X)
      IF (DFLOOR.GT.X) DFLOOR=DFLOOR-1D0
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE DMPHEX(IDAT,N,BUFF)
C     ==============================
C
C1    vartype           I2(N) I C*(*)
C1    iostatus           I    I   O
C
C2    Subroutine DMPHEX dumps the integer array
C2    length N passed in IDAT to the character string
C2    BUFF in Hexadecimal format.
C
      INTEGER*4 I2I4,I4,N,J,K,NLEN,P
      INTEGER*2 IDAT(N)
      LOGICAL OK
      CHARACTER B2*5,B1*4,BUFF*(*),DECTOH*4
C
      EXTERNAL I2I4,NLEN,C2ASC1,DECTOH
C
C     clear output string
      BUFF=' '
C     reset pointer in output string
      P=1
      DO 150 J=1,N
C        reset construction string
         B2='0000'
C        convert to 4-byte integer
         I4=I2I4(IDAT(J))
C        convert to hex
         B1=DECTOH(I4)
C        get position at which to place
C        converted hex string
         K=4-NLEN(B1)
         IF (K.GT.0) THEN
            B2((K+1):)=B1(1:NLEN(B1))
         ELSE
            B2=B1
         END IF
C        place string in output buffer
         BUFF(P:)=B2
C        update buffer pointer
         P=P+4
 150  CONTINUE
C
C     must change embedded apaces to zeroes
      K=NLEN(BUFF)
C
      OK = .TRUE.
C
      DO 5 J=1,K
          IF (BUFF(J:J).NE.' ') THEN
C             end of leading spaces
              OK=.FALSE.
          ELSE IF (BUFF(J:J).EQ.' ' .AND. .NOT.OK) THEN
C             must convert to '0'
              BUFF(J:J)='0'
          END IF
 5    CONTINUE
C
      END
C
C     -----------------------------------------------------
C
      FUNCTION DPI(X)
C     ===============
C1    vartype   D  D
C1    iostatus  O  I
C
C2    Returns the result PI*X
C
      DOUBLE PRECISION P,DPI,X
      PARAMETER (P=3.1415926535898D0)
C
      DPI=X*P
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE DVALU(STRING,DVAL,OK)
C     ================================
C
C1    vartype           C*(*)  R   L
C1    iostatus            I    O   O
C
C2    Subroutine DVALU returns the first numeric
C2    value found in STRING in the double variable DVAL.
C2    Logical flag OK is returned true if successful.
C
      INTEGER*4 J,K
      DOUBLE PRECISION DVAL
      LOGICAL OK
      CHARACTER*(*) STRING,FORM*16
      EXTERNAL DBOUND
C
C     find bounds of numeric field in string
      CALL DBOUND(STRING,J,K,*99)
C     set format to read
      K=K-J+1
      WRITE(UNIT=FORM,FMT='(A,I2.2,A)')'(D',K,'.0)'
C     read with constructed format
      READ(UNIT=STRING(J:),FMT=FORM,ERR=99) DVAL
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
C     return error if fail to read
      DVAL=1
      OK=.FALSE.
C
      END
C
C     ------------------------------------------------
C
      FUNCTION FLOOR(X)
C     =================
C1    vartype    R   R
C1    iostatus   O   I
C
C2    Rounds X to the nearest integer
C2    value which is .LE. 'X'
C
      REAL FLOOR,X
      DOUBLE PRECISION DFLOOR
      EXTERNAL DFLOOR
      INTRINSIC DBLE,REAL
C
      FLOOR=REAL(DFLOOR(DBLE(X)))
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE GREGOK(D,M,Y,ST)
C     ===========================
C
C1    Vartype           I I I I
C1    Iostat            I I I O
C
C2    Subroutine GREGOK test the validity of
C2    a GERGORIAN date passed in the form
C2    Day,Month,Year.
C2    The limiting range of years is 1583 to 4200.
C2    If the date is invalid for any reason, then
C2    ST is returned with a non-zero value.
C2       ST = 1 Year is outwith range  (1583-4200)
C2       ST = 2 Month is outwith range (1-12)
C2       ST = 3 Day is outwith range (for given month)
C
      INTEGER D,M,Y,ST
      INTEGER YLOW,YHIGH,MONTH(12)
      LOGICAL LEAPYR
      PARAMETER (YLOW=1583,YHIGH=4200)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     test for year within range
      IF (Y.LT.YLOW .OR. Y.GT.YHIGH) THEN
C        year out of range
         ST=1
         RETURN
      END IF
C
C     test for month within range
      IF (M.LT.1 .OR. M.GT.12) THEN
C        month out of range
         ST=2
         RETURN
      END IF
C
C     test for leap year
      IF (LEAPYR(Y)) THEN
C        29 days in February
         MONTH(2)=29
      ELSE
C        28 days in Febrauary
         MONTH(2)=28
      END IF
C
C     test validity of day
      IF (D.LT.1 .OR. D.GT.MONTH(M)) THEN
C        day out of range
         ST=3
         RETURN
      END IF
C
C     complete date is acceptable
      ST=0
C
      END
C
C     ------------------------------------------------
C
      FUNCTION HEXTOD(HEXSTR)
C     =======================
C
C1    vartype    I     C*(*)
C1    iostatus   O       I
C
C2    Function HEXTOD returns the decimal
C2    integer equivalent of the passed
C2    hexadecimal character string HEXSTR.
C2    If any of the characters is outwith the
C2    hex range (0-9,A-F) then, the function
C2    returns zero for that character, and the
C2    returned value will of course be in error.
C2    The character string HEXSTR must be a maximum
C2    of 8 characters in length, otherwise only
C2    the rightmost 8 characters will be
C2    converted.
C
      INTEGER HEXTOD
      INTEGER I,J,K,L,N,V1,V2,NN
      INTEGER NLEN1,HEX2D,COMPI
      LOGICAL NEG
      CHARACTER*(*) HEXSTR
      CHARACTER*8 H,H1
      EXTERNAL NLEN1,HEX2D,COMPI
C
C     get actual length of string
      L=NLEN1(HEXSTR)
      H1='00000000'
      IF (L.GE.8) THEN
C        get rightmost 8 characters
         J=L-7
         H1=HEXSTR(J:)
      ELSE
C        place characters in convert string
         J=9-L
         H1(J:)=HEXSTR(1:L)
      END IF
C     work in upper case only
      CALL FOLDUP(H1)
C     get string in reverse order
      CALL REVSTR(H1)
C     initialize value
      N=0
C     convert string character by character
      NEG=.FALSE.
      DO 10 I=1,8
         V1=HEX2D(H1(I:I))
         IF (I.EQ.8 .AND. V1.GE.8) THEN
C           highest nibble, must be negative
            NEG=.TRUE.
C           remove sign bit
            V1=V1-8
         END IF
         V2=V1*(2**(4*(I-1)))
         N=N+V2
 10   CONTINUE
C     need to do more if negative
      IF (NEG) THEN
C        complement all bits
         J=COMPI(N)
         N=J
C        clear sign bit
         CALL CLRBIT(31,N)
C        add 1 for two's complement
         N=N+1
C        now have positive number
C        negate to get correct negative form
         N=-N
      END IF
C     return converted value
      HEXTOD=N
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE INCALC()
C     ===================
C
C2    Subroutine INCALC initializes all
C2    workspace used by the variable processor system,
C2    and clears all variables.
C
      include 'include/calc.inc'
      include 'include/macro.inc'
C
      INTEGER P1
C
C     initialize calculator variables
      NVARS=0
CAPOLLO|SUN|PC386
      MAXV=500
CAPOLLO|SUN|PC386
CIBM
C      MAXV=100
CIBM
C     clear variable names array
      DO 5 P1=1,MAXV
         VARS(P1)='                    '
 5    CONTINUE
C     initialise character stuff
      CAP=0
      CSP=0
C     clear character names
      DO 10 P1=1,100
          CARR(P1)='
     +             '
          CARNAM(P1)='                    '
10    CONTINUE
C     Set various new params
C     Set the macro hilight flag to false
C
      END
C
C     ---------------------------------------------------------
C
      FUNCTION INTAC(D)
C     =================
C1    vartype    L   D
C1    iostatus   O   I
C
C2    Returns true if D may be used as
C2    an integer.(within tolerance)
C
      DOUBLE PRECISION D,DEPS
      LOGICAL INTAC
      EXTERNAL DEPS
      INTRINSIC ABS,ANINT
C
C     test against tolerance level
      IF (ABS(D-ANINT(D)).LE.DEPS(50D0*D)) THEN
         INTAC=.TRUE.
      ELSE
         INTAC=.FALSE.
      END IF
C
      END
C
C     ------------------------------------------------
C
      FUNCTION JULIAN(D,M,Y)
C     ======================
C
C1    vartype     I   I I I
C1    iostatus    O   I I I
C
C2    Function JULIAN returns the JULIAN day of the year
C2    of a GREGORIAN date passed in the form Day,Month,Year.
C2
C2    The limiting range of years is 1583 to 4200.
C2    If the date is invalid for any reason, then
C2    JULIAN is returned zero.
C
      INTEGER D,M,Y,JULIAN
      INTEGER YLOW,YHIGH,MONTH(12),I,J
      LOGICAL LEAPYR
      PARAMETER (YLOW=1583,YHIGH=4200)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     test for year within range
      IF (Y.LT.YLOW .OR. Y.GT.YHIGH) THEN
C        year out of range
         J=0
         RETURN
      END IF
C
C     test for month within range
      IF (M.LT.1 .OR. M.GT.12) THEN
C        month out of range
         J=0
         RETURN
      END IF
C
C     test for leap year
      IF (LEAPYR(Y)) THEN
C        29 days in February
         MONTH(2)=29
      ELSE
C        28 days in Febrauary
         MONTH(2)=28
      END IF
C
C     test validity of day
      IF (D.LT.1 .OR. D.GT.MONTH(M)) THEN
C        day out of range
         J=0
         RETURN
      END IF
C
C     complete date is acceptable
C     now calculate JULIAN day of year
      J=0
      IF (M.GT.1) THEN
         DO 5 I=1,M-1
C           add number of days in month
            J=J+MONTH(I)
 5       CONTINUE
      END IF
C     add days in month of the given date
      J=J+D
C     return result
      JULIAN=J
C
      END
C
C     ------------------------------------------------
C
      FUNCTION LEAPYR(Y)
C     ==================
C
C1    vartype     L    I
C1    iostatus    O    I
C
C2    Function LEAPYR return true if the
C2    passed year Y is a leap year, false
C2    if otherwise.
C
      INTEGER Y
      LOGICAL LEAPYR,Y4,Y100,Y400
C
C     check for divisible by 4
      Y4=MOD(Y,4).EQ.0
C     check for divisible by 100
      Y100=MOD(Y,100).EQ.0
C     check for divisible by 400
      Y400=MOD(Y,400).EQ.0
C     leap year if divisible by 4 and not 100
c     or divisible by 400
      LEAPYR=(Y4.AND. .NOT.Y100) .OR. Y400
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE LSSUPP(STRING)
C     =========================
C
C1    vartype            C*(*)
C1    iostatus             I
C
C2    Subroutine LSSUPP removes all leading spaces
C2    from the text STRING, and returns the modified
C2    STRING.
C
      INTEGER N,L,NLEN
      CHARACTER*(*) STRING
      EXTERNAL NLEN
C
C     find active length of string
      L=NLEN(STRING)
C     test for null string
      IF (L.EQ.0) RETURN
C     set counter
      N=0
C     test for space
 10   CONTINUE
      N=N+1
      IF (STRING(N:N).EQ.' ') GOTO 10
C     N points to first non-space
C     shift is 1 less
      N=-(N-1)
C     shift string left
      CALL SHIFT(STRING,N)
C
      END
C
C     ------------------------------------------------
C
      FUNCTION NDAYS(D1,M1,Y1,D2,M2,Y2)
C     =================================
C
C1    vartype     I  I  I  I  I  I  I
C1    iostatus    O  I  I  I  I  I  I
C
C2    Function NDAYS returns the number of days between
C2    two dates passed in the form Day,Month,Year.
C2    If either of the two dates is outwith the range
C2    of the GREGORIAN calendar, or invalid, then
C2    NDAYS is returned zero.
C2    A negative result indicates that date 2 precedes date 1.
C
      INTEGER D1,M1,Y1,D2,M2,Y2,NDAYS
      INTEGER J1,J2,ND1,ND2,DAYS,I,YRDAYS,JULIAN
C
C     get Julian day of date 1
      J1=JULIAN(D1,M1,Y1)
C     get number of days in year 1
      ND1=YRDAYS(Y1)
C     get Julian day of date 2
      J2=JULIAN(D2,M2,Y2)
C     get number of days in year 2
      ND2=YRDAYS(Y2)
C
      DAYS=0
      IF (Y1.EQ.Y2) THEN
C        both dates in same year
         DAYS=J2-J1
      ELSE IF (Y2.GT.Y1) THEN
C        Year 1 precedes Year 2
         I=Y1+1
 5       CONTINUE
         IF (I.LT.Y2) THEN
C           add number of days in year to tally
            DAYS=DAYS+YRDAYS(I)
            I=I+1
            GOTO 5
         END IF
C        add julian days of final year
         DAYS=DAYS+J2
C        add julian days of first year
         DAYS=DAYS+ND1-J1
      ELSE
C        Year 2 precedes Year 1
         I=Y2+1
 6       CONTINUE
         IF (I.LT.Y1) THEN
C           add number of days in year to tally
            DAYS=DAYS+YRDAYS(I)
            I=I+1
            GOTO 6
         END IF
C        add julian days of final year
         DAYS=DAYS+J1
C        add julian days of first year
         DAYS=DAYS+ND2-J2
         DAYS=-DAYS
      END IF
C     return number of days
      NDAYS=DAYS
C
      END
C
C     ------------------------------------------------
C
      FUNCTION NVRFY(STRING,C)
C     ========================
C1    vartype    I    C*(*) C*(*)
C1    iostatus   O      I     I
C
C2    Returns position of first character
C2    in STRING which is also in C
C
      INTEGER NVERFY,NVRFY,I
      CHARACTER*(*) C,STRING
      INTRINSIC INDEX,LEN
C
      ENTRY NVERFY(STRING,C)
*     ======================
      DO 10, I=1,LEN(STRING)
         NVRFY=I
         NVERFY=I
         IF (INDEX(C,STRING(I:I)).GT.0) RETURN
   10 CONTINUE
      NVRFY=0
      END
C
C     ------------------------------------------------
C
      SUBROUTINE PATREP(STRING,C1,C2,OK)
C     ==================================
C1    vartype            C*(*) C*(*) C*(*) L
C1    iostatus            IO     I     I   O
C
C2    Replaces first occurrence of C1 in STRING
C2    with C2.
C2    Returns OK true if replacement made.
C
      INTEGER P
      LOGICAL OK
      CHARACTER*(*) C1,C2,STRING
      EXTERNAL POSREP
      INTRINSIC INDEX,LEN
C
      P=INDEX(STRING,C1)
      IF (P.GT.0) THEN
C       go do replace
        CALL POSREP(STRING,P,P+LEN(C1)-1,C2)
        OK=.TRUE.
      ELSE
C       no replace to do
        OK=.FALSE.
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE PATRP(STRING,C1,C2)
C     ==============================
C1    vartype           C*(*) C*(*) C*(*)
C1    iostatus            IO    I     I
C
C2    Replaces all occurrences of C1 in STRING
C2    with C2,
C
      LOGICAL OK
      CHARACTER*(*) STRING,C1,C2
      EXTERNAL PATREP
C
 10   CONTINUE
C     replace first occurrence
      CALL PATREP(STRING,C1,C2,OK)
C     keep going until done
      IF (OK) GO TO 10
      END
C
C     ------------------------------------------------
C
      SUBROUTINE POPSTK(*,*)
C     ======================
C
C2    Subroutine POPSTK is intended for use with AEXPRN
C2    and pops data and operators off the stack,and performs
C2    the required arithmetic function.
C2    The result is left in the current data stack position
C2    DSTK(DP).
C
      include 'include/calc.inc'
C
      INTEGER MX,MX1
      PARAMETER (MX=20, MX1=MX+1)
      INTEGER K,I
      REAL R
      DOUBLE PRECISION DDEG,DPI,DRAD,DCEIL,DFLOOR,X(0:MX)
      LOGICAL INTAC
      INTEGER*4 NLEN1,NLEN
      CHARACTER*80 STRING
      EXTERNAL INTAC,DDEG,DRAD,DCEIL,DFLOOR,PI,CFUNC1,CFUNC2,CFUNC4
      EXTERNAL CFUNC5,CFUNC6,CFUNC7,NLEN1,NLEN
      INTRINSIC NINT,AINT,ANINT,ABS,MOD,SIGN,DIM,MAX,MIN,SQRT,EXP,LOG,
     +        LOG10,SIN,COS,TAN,ASIN,ACOS,ATAN,ATAN2,SINH,COSH,
     1        TANH,REAL,INT,INDEX
      DATA X/MX1*0D0/
C-----FOR USE BY SUBROUTINE AEXPRN.  (*,*).
      CERR=.FALSE.
      X(0) = 0.0
      OPTYPE='N'
C     Because we now have a character stack, the DP variable
C     can be zero in certain circumstances. Thus we must not allow
C     this to happen in certain functions
C     set K to current operator stack pointer
      K=OP
C     decrement operator pointer to previous one
      OP=K-1
C      PRINT*, '[POPST] WE ARE TRYING TO EXECUTE ',OSTK(K)
C     go perform current operation
*                  B B U U
*           ** / * - + - + ) ( ,       <FUNCTIONS>
      GO TO (1,2,3,4,5,6,7,7,7,7,11,12,13,14,15,16,17,18,19,20,21,22,
     +       23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,
     +       42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
     +       61) OSTK(K)
      RETURN 2
C     *****************************************
C     ***             OP-CODE 1             ***
C     ***          RAISE TO POWER           ***
C     *****************************************
C     if end of stack return with error
    1 IF (DP.LT.2) RETURN 2
C     set K to current data pointer
      K=DP
C     decrement DP to previous data position
      DP=K-1
      IF (INTAC(DSTK(K))) THEN
C        if current data is integer
C        raise previous data to integer power of current data
         DSTK(DP)=DSTK(DP)**NINT(DSTK(K))
      ELSE
C        raise to real power
         DSTK(DP)=DSTK(DP)**DSTK(K)
      END IF
      RETURN 1
C     *****************************************
C     ***             OP-CODE 2             ***
C     ***              DIVIDE               ***
C     *****************************************
    2 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      IF (ABS(DSTK(K)).LT.1E-6 ) RETURN 2
      DSTK(DP)=DSTK(DP)/DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 3             ***
C     ***             MULTIPLY              ***
C     *****************************************
    3 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=DSTK(DP)*DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 4             ***
C     ***             SUBTRACT              ***
C     *****************************************
    4 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=DSTK(DP)-DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 5             ***
C     ***               ADD                 ***
C     *****************************************
    5 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=DSTK(DP)+DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 6             ***
C     ***              NEGATE               ***
C     *****************************************
    6 IF ( DP.EQ.0 ) RETURN 2
      DSTK(DP)=-DSTK(DP)
C     *****************************************
C     ***             OP-CODE 7             ***
C     ***             DELIMITER             ***
C     *****************************************
    7 RETURN 1
C     *****************************************
C     ***             OP-CODE 11            ***
C     ***                AINT               ***
C     *****************************************
   11 IF(DP.EQ.0) RETURN2
      DSTK(DP)=AINT(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 12            ***
C     ***               ANINT               ***
C     *****************************************
   12 IF(DP.EQ.0) RETURN2
      DSTK(DP)=ANINT(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 13            ***
C     ***                ABS                ***
C     *****************************************
   13 IF(DP.EQ.0) RETURN2
      DSTK(DP)=ABS(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 14            ***
C     ***                MOD                ***
C     *****************************************
   14 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=MOD(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 15            ***
C     ***                SIGN               ***
C     *****************************************
   15 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=SIGN(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 16            ***
C     ***                DIM                ***
C     *****************************************
   16 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=DIM(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 17            ***
C     ***                MAX2               ***
C     *****************************************
   17 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=MAX(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 18            ***
C     ***                MIN2               ***
C     *****************************************
   18 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=MIN(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 19            ***
C     ***                SQRT               ***
C     *****************************************
   19 IF(DP.EQ.0) RETURN2
      DSTK(DP)=SQRT(ABS(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 20            ***
C     ***                EXP                ***
C     *****************************************
   20 IF(DP.EQ.0) RETURN2
      DSTK(DP)=EXP(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 21            ***
C     ***                LOG                ***
C     *****************************************
   21 IF(DP.EQ.0) RETURN2
      DSTK(DP)=LOG(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 22            ***
C     ***               LOG10               ***
C     *****************************************
   22 IF(DP.EQ.0) RETURN2
      DSTK(DP)=LOG10(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 23            ***
C     ***                SIN                ***
C     *****************************************
   23 IF(DP.EQ.0) RETURN2
      DSTK(DP)=SIN(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 24            ***
C     ***                SIND               ***
C     *****************************************
   24 IF(DP.EQ.0) RETURN2
      DSTK(DP)=SIN(DRAD(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 25            ***
C     ***                COS                ***
C     *****************************************
   25 IF(DP.EQ.0) RETURN2
      DSTK(DP)=COS(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 26            ***
C     ***                COSD               ***
C     *****************************************
   26 IF(DP.EQ.0) RETURN2
      DSTK(DP)=COS(DRAD(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 27            ***
C     ***                TAN                ***
C     *****************************************
   27 IF(DP.EQ.0) RETURN2
      DSTK(DP)=TAN(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 28            ***
C     ***                TAND               ***
C     *****************************************
   28 IF(DP.EQ.0) RETURN2
      DSTK(DP)=TAN(DRAD(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 29            ***
C     ***                ASIN               ***
C     *****************************************
   29 IF(DP.EQ.0) RETURN2
      DSTK(DP)=ASIN(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 30            ***
C     ***               ASIND               ***
C     *****************************************
   30 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DDEG(ASIN(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 31            ***
C     ***                ACOS               ***
C     *****************************************
   31 IF(DP.EQ.0) RETURN2
      DSTK(DP)=ACOS(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 32            ***
C     ***               ACOSD               ***
C     *****************************************
   32 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DDEG(ACOS(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 33            ***
C     ***                ATAN               ***
C     *****************************************
   33 IF(DP.EQ.0) RETURN2
      DSTK(DP)=ATAN(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 34            ***
C     ***               ATAND               ***
C     *****************************************
   34 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DDEG(ATAN(DSTK(DP)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 35            ***
C     ***               ATAN2               ***
C     *****************************************
   35 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=ATAN2(DSTK(DP),DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 36            ***
C     ***              ATAN2D               ***
C     *****************************************
   36 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=DDEG(ATAN2(DSTK(DP),DSTK(K)))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 37            ***
C     ***                SINH               ***
C     *****************************************
   37 IF(DP.EQ.0) RETURN2
      DSTK(DP)=SINH(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 38            ***
C     ***                COSH               ***
C     *****************************************
   38 IF(DP.EQ.0) RETURN2
      DSTK(DP)=COSH(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 39            ***
C     ***                TANH               ***
C     *****************************************
   39 IF(DP.EQ.0) RETURN2
      DSTK(DP)=TANH(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 40            ***
C     ***                DEG                ***
C     *****************************************
   40 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DDEG(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 41            ***
C     ***                 PI                ***
C     *****************************************
   41 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DPI(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 42            ***
C     ***                RAD                ***
C     *****************************************
   42 IF(DP.EQ.0) RETURN2
      DSTK(DP)=DRAD(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 43            ***
C     ***                 RD                ***
C     ***           (ROUND DOWN)            ***
C     *****************************************
   43 IF (DP.LT.2) RETURN 2
      K=DP
      DSTK(K)=ABS(DSTK(K))
      DP=K-1
      DSTK(DP)=DFLOOR(DSTK(DP)/DSTK(K))*DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 44            ***
C     ***                 RN                ***
C     ***              (ROUND)              ***
C     *****************************************
   44 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=ANINT(DSTK(DP)/DSTK(K))*DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 45            ***
C     ***                 RU                ***
C     ***             (ROUND UP)            ***
C     *****************************************
   45 IF (DP.LT.2) RETURN 2
      K=DP
      DSTK(K)=ABS(DSTK(K))
      DP=K-1
      DSTK(DP)=DCEIL(DSTK(DP)/DSTK(K))*DSTK(K)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 46            ***
C     ***                 IA                ***
C     *****************************************
   46 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      IF (INTAC(DSTK(DP))) THEN
         DSTK(DP)=(DSTK(K)+1D0)**NINT(DSTK(DP))
      ELSE
         DSTK(DP)=(DSTK(K)+1D0)**DSTK(DP)
      END IF
      RETURN 1
C     *****************************************
C     ***             OP-CODE 47            ***
C     ***                 IN                ***
C     *****************************************
   47 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=LOG(DSTK(K))/LOG(DSTK(DP)+1D0)
      RETURN 1
C     *****************************************
C     ***             OP-CODE 48            ***
C     ***                 IZ                ***
C     *****************************************
   48 IF (DP.LT.2) RETURN 2
      K=DP
      DSTK(K)=1D0/DSTK(K)
      DP=K-1
      IF (INTAC(DSTK(K))) THEN
         DSTK(DP)=DSTK(DP)**NINT(DSTK(K))-1D0
      ELSE
         DSTK(DP)=DSTK(DP)**DSTK(K)-1D0
      END IF
      RETURN 1
C     *****************************************
C     ***             OP-CODE 49            ***
C     ***                LEN                ***
C     ***            REDUNDANT !!!!         ***
C     *** THIS FUNCTION TO LET (GOOD PRICES)***
C     ***           SOLD !!!!!!!            ***
C     *****************************************
  49  IF(CSP.LT.1) RETURN 2
      SBUF='INTEGER'
      K=NLEN(CARST(CSP))-1
      DP=DP+1
      DSTK(DP)=K
      CSP=CSP-1
      RETURN 1
C     *****************************************
C     ***             OP-CODE 50            ***
C     ***              INDEX                ***
C     ***          (REGISTER INPUT)         ***
C     ***            REDUNDANT !!!!         ***
C     *** THIS FUNCTION TO LET (GOOD PRICES)***
C     ***           SOLD !!!!!!!            ***
C     *****************************************
  50  IF (CSP.LT.2) RETURN 2
      K=0
C     the null character must not be on the search pattern
      SBUF='INTEGER'
      I=NLEN(CARST(CSP))-1
      IF (I.LT.1) RETURN 1
      K=INDEX(CARST(CSP-1)(1:NLEN1(CARST(CSP-1))),CARST(CSP)(1:I))
C      WRITE(10,*)'(CARST(CSP-1)= ',(CARST(CSP-1))
C      WRITE(10,*)'CARST(CSP)(1:I)= ',CARST(CSP)(1:I)
C     increment the data stack pointer
      DP=DP+1
      DSTK(DP)=K
C     pull the 2 character off the stack
      CSP=CSP-2
      RETURN 1
C     *****************************************
C     ***             OP-CODE 51            ***
C     ***                REAL               ***
C     *****************************************
   51 IF(DP.EQ.0) RETURN2
      R=REAL(DSTK(DP))
      DSTK(DP)=R
      RETURN 1
C     *****************************************
C     ***             OP-CODE 52            ***
C     ***                PYT                ***
C     *****************************************
   52 IF (DP.LT.2) RETURN 2
      K=DP
      DP=K-1
      DSTK(DP)=SQRT(DSTK(DP)*DSTK(DP)+DSTK(K)*DSTK(K))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 53            ***
C     ***                INT                ***
C     *****************************************
   53 IF(DP.EQ.0) RETURN2
      SBUF='INTEGER'
      DSTK(DP)=INT(DSTK(DP))
      RETURN 1
C     *****************************************
C     ***             OP-CODE 54            ***
C     ***              MID$                 ***
C     *****************************************
   54 CONTINUE
      IF(DP.LT.2.OR.CSP.EQ.0) RETURN 2
      K=DP
      DP=DP-2
C      PRINT*, '[POPSTK] HELP MA BOAB WERE DOING IT'
      CERR=.TRUE.
      CALL CFUNC1(CARST(CSP),NINT(DSTK(K-1)),NINT(DSTK(K)),
     +            STRING,*99)
      CARST(CSP)=STRING
      OPTYPE='C'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 55            ***
C     ***               LEFT$               ***
C     *****************************************
   55 IF(DP.EQ.0.OR.CSP.EQ.0) RETURN 2
      K=DP
      DP=DP-1
      CERR=.TRUE.
      CALL CFUNC2(CARST(CSP),NINT(DSTK(K)),STRING,*99)
      CARST(CSP)=STRING
      OPTYPE='C'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 56            ***
C     ***               RIGHT$              ***
C     *****************************************
   56 IF(DP.EQ.0.OR.CSP.EQ.0) RETURN 2
      K=DP
      DP=DP-1
C      PRINT*, '[POPSTK] HELP MA BOAB WERE DOING IT'
      CERR=.TRUE.
      CALL CFUNC3(CARST(CSP),NINT(DSTK(K)),STRING,*99)
      CARST(CSP)=STRING
      OPTYPE='C'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 57            ***
C     ***               STR$                ***
C     *****************************************
C     here we take an item of the data stack and stick
C     the equivalent character value on the character stack
   57 IF(DP.EQ.0) RETURN 2
      K=DP
      DP=DP-1
      CALL CFUNC4(DSTK(K),STRING,*99)
      CSP=CSP+1
      CARST(CSP)=STRING
      OPTYPE='C'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 58            ***
C     ***                VAL                ***
C     *****************************************
C     here we take an item of the character stack and stick
C     the equivalent numeric value on the data stack. If there
C     are no numeric chars then a zero value is returned
   58 IF(CSP.EQ.0) RETURN 2
      K=CSP
      CSP=CSP-1
      DP=DP+1
      CALL CFUNC5(CARST(K),DSTK(DP),*99)
      OPTYPE='N'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 59            ***
C     ***               CHR$                ***
C     *****************************************
C     here we take an item of the data stack and stick
C     the equivalent character value on the character stack
   59 IF(DP.EQ.0) RETURN 2
      K=DP
      DP=DP-1
      CALL CFUNC6(DSTK(K),STRING,*99)
      CSP=CSP+1
      CARST(CSP)=STRING
      OPTYPE='C'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 60            ***
C     ***                ASC                ***
C     *****************************************
C     here we take an item of the character stack and stick
C     the equivalent numeric value on the data stack. If there
C     are no numeric chars then a zero value is returned
   60 IF(CSP.EQ.0) RETURN 2
      SBUF='INTEGER'
      K=CSP
      CSP=CSP-1
      DP=DP+1
      CALL CFUNC7(CARST(K),DSTK(DP),*99)
      OPTYPE='N'
      RETURN 1
C     *****************************************
C     ***             OP-CODE 61            ***
C     ***          + CONCATONATION          ***
C     *****************************************
   61 IF(CSP.LT.2) RETURN 2
      K=CSP
      CSP=CSP-1
      CARST(CSP)=CARST(CSP)(:NLEN1(CARST(CSP))-1)//
     +           CARST(K)(:NLEN1(CARST(K)))
      OPTYPE='C'
      RETURN 1
99    RETURN 2
      END
C
C     ---------------------------------------------------------
C
C
C     Character functions from POPST
C
C
C     ---------------------------------------------------------
C
      SUBROUTINE POSINS(STRING,POS,C)
C     ===============================
C1    vartype            C*(*)  I C*(*)
C1    iostatus             IO   I   I
C
C2    Inserts sub-string C at position POS
C2    in STRING.
C
      INTEGER L,POS
      CHARACTER*(*) C,STRING
      EXTERNAL SHIFT
      INTRINSIC LEN,MIN
C
C     get length of string to insert
      L=LEN(C)
C     shift right to make space
      CALL SHIFT(STRING(POS:),L)
C     insert sub-string
      STRING(POS:MIN(LEN(STRING),POS+L-1))=C
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE POSREM(STRING,POS1,POS2)
C     ===================================
C1    vartype            C*(*)  I    I
C1    iostatus             IO   I    I
C
C2    Removes characters between POS1 and POS2
C2    from STRING.
C
      INTEGER POS1,POS2
      CHARACTER STRING*(*)
      EXTERNAL SHIFT
      INTRINSIC DIM
C
C     shift sub-string left to eliminate chars
      CALL SHIFT(STRING(POS1:),-DIM(POS2+1,POS1))
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE POSREP(STRING,POS1,POS2,C)
C     =====================================
C1    vartype            C*(*)  I    I  C*(*)
C1    iostatus             IO   I    I   I
C
C2    Replaces characters POS1:POS2 in STRING
C2    with C.
C
      INTEGER POS1,POS2
      CHARACTER*(*) C,STRING
      EXTERNAL POSINS,POSREM
C
C     remove chars between POS! and POS2
      CALL POSREM(STRING,POS1,POS2)
C     insert chars at POS1
      CALL POSINS(STRING,POS1,C)
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE RECAOP(STRING,AOP,OLEN,NARGS,CARGS)
C     ==============================================
C
      INTEGER NOPS
      PARAMETER (NOPS=61)
      INTEGER AOP,OLEN,OPL(1:NOPS),SLEN,NARGS,CARGS
      CHARACTER STRING*(*),OPS(1:NOPS)*8
      INTRINSIC LEN
      DATA OPS/ '**', '/', '*', '-', '+', '-', '+', ')', '(', ',',
     + 'AINT(','ANINT(','ABS(','MOD(','SIGN(','DIM(','MAX2(','MIN2(',
     + 'SQRT(','EXP(','LOG(','LOG10(','SIN(','SIND(','COS(','COSD(',
     + 'TAN(','TAND(','ASIN(','ASIND(','ACOS(','ACOSD(','ATAN(',
     + 'ATAND(','ATAN2(','ATAN2D(','SINH(','COSH(','TANH(',
     + 'DEG(','PI(','RAD(','RD(','RN(','RU(','IA(','IN(','IZ(',
     + 'LEN(','INDEX(','REAL(','PYT(','INT('
     +,'MID$(','LEFT$(','RIGHT$(','STR$(','VAL(','CHR$(','ASC(','+'/
      DATA OPL/ 2,9*1, 5,6,2*4,5,4,2*5,  5,2*4,6,4,5,4,5,
     +          4,2*5,6,5,6,5, 2*6,7,3*5, 4,3,4,6*3,
     +          4,6,5,4,4  ,5,6,7,5,4,5,4,1
     +/
C-----FOR USE BY SUBROUTINE AEXPRN.  (C,I,I).
      SLEN=LEN(STRING)
      IF (SLEN.GE.8) THEN
         DO 10, AOP=1,NOPS
            OLEN=OPL(AOP)
            IF (STRING(:OLEN).EQ.OPS(AOP)(:OLEN)) RETURN
   10    CONTINUE
      ELSE
         DO 20, AOP=1,NOPS
            OLEN=OPL(AOP)
            IF (OLEN.LE.SLEN) THEN
               IF (STRING(:OLEN).EQ.OPS(AOP)(:OLEN)) RETURN
            END IF
   20    CONTINUE
      END IF
      AOP=0
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE RECVAR(STRING,VLEN,VP)
C     =================================
C
C1    vartype            C*(*)  I    I
C1    iostatus             I    O    O
C
C2    Subroutine RECVAR searches for a valid variable
C2    name starting at the begining of STRING,returns
C2    the length of the variable name found in VLEN,
C2    and the pointer to it in the variable stack in VP.
C2    Ensures that longest matching variable name is returned.
C
      include 'include/calc.inc'
C
      INTEGER VLEN,VP,SLEN,I,TLEN,NLEN1,F
      LOGICAL FOUND
      CHARACTER INVAL*20
      CHARACTER*(*) STRING
C
      FOUND=.FALSE.
      VP=0
      VLEN=0
C     check for a character variable but only valid chars
C
      TLEN=INDEX(STRING,',')
      SLEN=INDEX(STRING,'$')
C     if a $ occurs after a delimeter then it will be taken
C     as numeric
      IF(SLEN.GT.TLEN.AND.TLEN.NE.0) SLEN=0
C     is this a string or numeric variable requested
C      write(10,*)  '[RECVAR] STRING= ',STRING
      IF(SLEN.EQ.0) THEN
          OPTYPE='N'
C         find length of supplied string
C          test for string length greater than max variable length
          SLEN=MIN(20,LEN(STRING))
C         scan through variable list
          DO 10 I=1,NVARS
C             find length of variable name
              TLEN=VARL(I)
              IF(TLEN.GT.SLEN) GOTO 10
C      WRITE(10,*) '[REVAR] TLEN= ',TLEN
C      WRITE(10,*) '[REVAR] vars(i) = ',vars(i)
C             test for match with string
              F=INDEX(STRING(:SLEN),VARS(I)(:TLEN))
C
              IF(F.GT.0.AND.(VARS(I)(:TLEN).EQ.STRING(:TLEN))) THEN
C                 make sure that the variable is the first one
C      WRITE(10,*) '[REVAR] VARIABLE FOUND !'
C                 Is the current find bigger than the last one
                  IF(TLEN.GT.VLEN) THEN
                      VP=I
                      VLEN=TLEN
                  ENDIF
              ENDIF
 10       CONTINUE
      ELSE
           OPTYPE='C'
           SLEN=MIN(20,LEN(STRING))
C          scan through character names file and pull out a variable
C          Heres hoping that we find one. if not then vp will be 0
           DO 30 I=1,CAP
              TLEN=NLEN1(CARNAM(I))
C      WRITE(10,*) '[REVAR] STRING(:SLEN)= ',STRING(:SLEN)
C      WRITE(10,*) '[REVAR] CARNAM= ',CARNAM(I)(:TLEN)
              IF(TLEN.GT.SLEN) GOTO 30
              F=INDEX(STRING(:SLEN),CARNAM(I)(:TLEN))
              IF(F.GT.0.AND.(CARNAM(I)(:TLEN).EQ.STRING(:TLEN))) THEN
C      WRITE(10,*) '[REVAR] VARIABLE FOUND F= ',F,' I= ',I
C                 Is the current find bigger than the last one
                  IF(TLEN.GT.VLEN) THEN
                      VP=I
                      VLEN=TLEN
                  ENDIF
              ENDIF
30        CONTINUE
      ENDIF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE REVSTR(STRING)
C     =========================
C
C1    vartype            C*(*)
C1    iostatus            IO
C
C2    Subroutine REVSTR reverses the order
C2    of a character string.
C
      INTEGER L,I,J
      CHARACTER*(*) STRING,CC*256
C
C     set length of string
      L=LEN(STRING)
C     copy string
      CC=STRING
C     now get hex string in reverse order
      J=L
      DO 150,I=1,L
         STRING((I):(I))=CC(J:J)
         J=J-1
 150  CONTINUE
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE SBOUND(STRING,VALCH,P1,P2)
C     =====================================
C1    vartype            C*(*)  C*(*) I  I
C1    iostatus             I      I   O  O
C
C2    Returns bounds P1,P2 of first occurrence
C2    of VALCH in STRING.
C
      INTEGER NVRFY,P1,P2,VRFY
      CHARACTER*(*) STRING,VALCH
      EXTERNAL NVRFY,VRFY
      INTRINSIC LEN
C
      P1=NVRFY(STRING,VALCH)
      IF (P1.GT.0) THEN
         P2=VRFY(STRING(P1:),VALCH)
         IF (P2.GT.0) THEN
            P2=P2+P1-2
         ELSE
            P2=LEN(STRING)
         END IF
      ELSE
         P2=0
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE SETBT(BITN,I)
C     =========================
C
C1    vartype    L    I   I
C1    iostatus   O    I   IO
C
C2    Subroutine SETBT sets bit
C2    number BITN in the integer I.
C2    Bits are numbered from 0 to 31.
C
      INTEGER*4 BITN,I,N1,N2,J4,BN,K,ITMP
      INTEGER I2I4
      INTEGER*2 J2(2),I4I2
      LOGICAL BITSET
      EQUIVALENCE (J4,J2)
      EXTERNAL I2I4,I4I2
C
C     get bit number
      BN=ABS(MOD(BITN,32))
C     copy the word
      J4=I
C     copy each half word
      N1=I2I4(J2(1))
      N2=I2I4(J2(2))
      IF (BN.GT.15) THEN
C        upper word affected
         BN=BN-16
         K=2**BN
         ITMP=N1/K
         BITSET=MOD(ITMP,2).NE.0
C        set the bit by addition
         IF (.NOT.BITSET) N1=N1+K
      ELSE
C        lower word affected
         K=2**BN
         ITMP=N2/K
         BITSET=MOD(ITMP,2).NE.0
C        set the bit by addition
         IF (.NOT.BITSET) N2=N2+K
      END IF
C     copy the upper and lower words back
      J2(1)=I4I2(N1)
      J2(2)=I4I2(N2)
C     now get return value
      I=J4
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE SHIFT(STRING,N)
C     ==========================
C1    vartype           C*(*) I
C1    iostatus           IO   I
C
C2    Shifts string N places.
C2    If N positive,then shift right
C2    If N negative then shift left
C
      INTEGER J,N,NLEN
      CHARACTER STRING*(*)
      EXTERNAL NLEN
      INTRINSIC LEN,MIN
C
      IF (N.GT.0) THEN
C       shift right
        DO 10, J=MIN(LEN(STRING),N+NLEN(STRING)),N+1,-1
          STRING(J:J)=STRING(J-N:J-N)
   10   CONTINUE
        STRING(:J)=' '
      ELSE IF (N.LT.0) THEN
        DO 20, J=1,N+NLEN(STRING)
          STRING(J:J)=STRING(J-N:J-N)
   20   CONTINUE
        STRING(J:)=' '
      END IF
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE SUBSTC(STRING,FC,SC)
C     ===============================
C1    vartype             c*(*) c  c
C2    iostats              i/0  i  i
C3    This routine will substitute the second charcater for
C3    the first character
      CHARACTER STRING*(*),FC,SC
      INTEGER*4 LS,I,NLEN1
      LS=NLEN1(STRING)
      DO 10 I=1,LS
          IF(STRING(I:I).EQ.FC) STRING(I:I)=SC
10    CONTINUE
      END
C
C     ---------------------------------------------------------
C
 
      SUBROUTINE UNJULN(Y,J,D,M)
C     ==========================
C
C1    vartype           I I I I
C1    iostatus          I I O O
C
C2    Subroutine UNJULN returns the day and month of the
C2    JULIAN day J in year Y.
C2
C2    The limiting range of years is 1583 to 4200.
C2    If the JULIAN day or the YEAR is invalid for any reason,
C2    then D,M are returned zero.
C
      INTEGER D,M,Y,JULIAN
      INTEGER YLOW,YHIGH,MONTH(12),I,J,YRDAYS,ND,TMP
      LOGICAL LEAPYR
      PARAMETER (YLOW=1583,YHIGH=4200)
      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
C     clear day and month
      D=0
      M=0
C     test for year within range
      IF (Y.LT.YLOW .OR. Y.GT.YHIGH) RETURN
C     get number of days in year
      ND=YRDAYS(Y)
C     test for JULIAN day in range
      IF (J.LT.1 .OR. J.GT.ND) RETURN
C
C     test for leap year
      IF (ND.EQ.366) THEN
C        29 days in February
         MONTH(2)=29
      ELSE
C        28 days in Febrauary
         MONTH(2)=28
      END IF
C
C     initialize for finding month
      TMP=J
      I=1
 5    CONTINUE
      IF (TMP.GT.0) THEN
C        subtract no days in month
         TMP=TMP-MONTH(I)
         I=I+1
         GOTO 5
      END IF
C     run past month of interest
C     return day
      D=TMP+MONTH(I-1)
C     return month
      M=I-1
C
      END
C
C     ------------------------------------------------
C
      FUNCTION VERHEX(STRING)
C     =======================
C
C1    vartype     L     C*(*)
C1    iostatus    O       I
C
C2    Function VERHEX returns true if STRING
C2    contains only a valid HEXADECIMAL
C2    character string.
C
      INTEGER J,VRFY,NLEN1,L
      LOGICAL VERHEX
      CHARACTER*(*) STRING,C*16
      DATA C/'0123456789ABCDEF'/
C
      L=NLEN1(STRING)
C     test for presence of non-hex
      J=VRFY(STRING(1:L),C)
      IF (J.EQ.0) THEN
C        all characters correct
         VERHEX=.TRUE.
      ELSE
C        wrong character somewhere
         VERHEX=.FALSE.
      END IF
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE VRFINT(STRING,IVAL,OK)
C     =================================
C
C1    vartype           C*(*)  I4   L
C1    iostatus            I    O    O
C
C2    Subroutine VRFINT verifies the passed string as an integer
C2    returning the value in the integer variable IVAL.
C2    Logical flag OK is returned true if successful.
C
      INTEGER*4 IVAL,NINT,VRFY,N,N1,NLEN
      DOUBLE PRECISION DVAL
      LOGICAL OK
      CHARACTER*(*) STRING
C     INTRINSIC NINT
      EXTERNAL VRFY,NLEN
C
      N1=NLEN(STRING)
      N=VRFY(STRING(1:N1),'+-0123456789')
      IF (N.EQ.0) THEN
C        valid integer,convert it
         CALL DVALU(STRING,DVAL,OK)
         IF (.NOT.OK) THEN
            IVAL=1
            OK=.FALSE.
            RETURN
         END IF
         IVAL=NINT(DVAL)
         OK=.TRUE.
      ELSE
C        not a valid integer number
         OK=.FALSE.
      END IF
C
      END
C
C     ------------------------------------------------
C
      FUNCTION VRFY(STRING,C)
C     =======================
C1    vartype    I    C*(*) C*(*)
C1    iostatus   O      I     I
C
C2    Returns position of first character
C2    in STRING which is NOT in C
C
      INTEGER VERIFY,VRFY,I
      CHARACTER*(*) C,STRING
      INTRINSIC INDEX,LEN
C
      VRFY=0
      DO 10, I=1,LEN(STRING)
         IF (INDEX(C,STRING(I:I)).EQ.0) THEN
            VRFY=I
            RETURN
         END IF
   10 CONTINUE
      END
C
C     ------------------------------------------------
C
      FUNCTION YRDAYS(Y)
C     ==================
C
C1    vartype    I    I
C1    iostatus   O    I
C
C2    Function YRDAYS returns the number of days
C2    in the passed year Y. If the year is out of
C2    the GREGORIAN range (1583-4200) then YRDAYS
C2    is returned zero.
C
      INTEGER YRDAYS,Y,JULIAN
      INTEGER D,M
      PARAMETER(D=31,M=12)
C
C     get julian day of 31st December
      YRDAYS=JULIAN(D,M,Y)
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE ZSUPP1(STRING)
C     =========================
C1    vartype            C*(*)
C1    iostatus             I
C
C2    replaces trailing zero's in first
C2    number in character 'string' with
C2    spaces.  (c).
C2    Leaves 1 zero if only 1 after point
C
      INTEGER P1,P2,LAST,NLEN1
      CHARACTER STRING*(*)
      EXTERNAL DBOUND,PATRP,NLEN1
      INTRINSIC INDEX
C
C     get bounds of numeric
      CALL DBOUND(STRING,P1,P2,*10)
      IF (P1.GT.0) THEN
         CALL PATRP(STRING(P1:),'E+0','E+')
         CALL PATRP(STRING(P1:),'E-0','E-')
         IF (INDEX(STRING(P1:P2),'.').GT.0) THEN
            IF (INDEX(STRING(P1:P2),'E').GT.0) THEN
               CALL PATRP(STRING(P1:P2),'00E','0E')
            ELSE IF (STRING(P2-1:P2).EQ.'00') THEN
               STRING(P2:P2)=' '
               CALL PATRP(STRING(P1:P2),'00 ','0')
            END IF
         END IF
      END IF
C
   10 CONTINUE
      END
C
C     ------------------------------------------------
C
      SUBROUTINE ZSUPP2(STRING)
C     =========================
C1    vartype            C*(*)
C1    iostatus             I
C
C2    replaces trailing zero's in first
C2    number in character 'string' with
C2    spaces.  (c).
C2    Leaves no zeroes after decimal point
C
      INTEGER*4 NLEN1,LAST
      CHARACTER*(*) STRING
      EXTERNAL NLEN1,ZSUPP1
C
C     remove all but 1 trailing zero
      CALL ZSUPP1(STRING)
C     Check to see if last chracter is a 0
      LAST=NLEN1(STRING)
C     remove last zero
      IF (STRING(LAST:LAST).EQ.'0') STRING(LAST:LAST)=' '
C
   10 END
C
C     ------------------------------------------------
C
