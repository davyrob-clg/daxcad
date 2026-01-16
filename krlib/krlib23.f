C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 krlib2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION BITSET(BITN,I)
C     FUNCTION CEIL(X)
C     FUNCTION  COMPI(I)
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
C
C     |-----------------------------------------------------------------|
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
