C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 krlib2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
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
      SAVE X
      EXTERNAL INTAC,DDEG,DRAD,DCEIL,DFLOOR,PI,CFUNC1,CFUNC2,CFUNC4
      EXTERNAL CFUNC5,CFUNC6,CFUNC7,NLEN1,NLEN
      INTRINSIC NINT,AINT,ANINT,ABS,MOD,SIGN,DIM,MAX,MIN,SQRT,EXP,LOG,
     +        LOG10,SIN,COS,TAN,ASIN,ACOS,ATAN,ATAN2,SINH,COSH,
     1        TANH,REAL,INT,INDEX
      DATA X/MX1*0D0/
C-----FOR USE BY SUBROUTINE AEXPRN.  (*,*).
      CERR=.FALSE.
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
