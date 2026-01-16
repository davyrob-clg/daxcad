C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 krlib2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE bEXPRN(STRING,AVALUE,*)
C
C     |-----------------------------------------------------------------|
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
