C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 chrlib.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION BINDEX(STR1,STR2)
C     FUNCTION CR(LINE)
C     FUNCTION CVERFY(CHR,STRING)
C     FUNCTION DCANG(X1,Y1,X2,Y2)
C     FUNCTION  DCHKLN(XV1,XV2,X)
C     FUNCTION   DDSTXY(X1,Y1,X2,Y2)
C     FUNCTION DQUAD(XC,YC)
C     FUNCTION   DZERO (VAL)
C     FUNCTION I4I2(I4)
C     FUNCTION LOGIC(LOGEXP)
C     FUNCTION RESPON(C1,FN)
C     FUNCTION YESOK(C1)
C     SUBROUTINE BCHAR(CHAR)
C     SUBROUTINE CHARCH(STRING)
C     SUBROUTINE DSWAP(X,Y)
C     SUBROUTINE FINDLN(STRING,LAYN,OK)
C     SUBROUTINE IVALU(STRING,IVAL,OK)
C     SUBROUTINE LCSUPP(STRING, LIST)
C     SUBROUTINE RVALU(STRING,RVAL,OK)
C     SUBROUTINE STRIPO(CHR,CH,REP)
C     SUBROUTINE STRNUM(LINE,DATA,NDATA)
C     SUBROUTINE STRSTR(LINE,DATA,NDATA)
C     SUBROUTINE TCSUPP(STRING, LIST)
C
C     |-----------------------------------------------------------------|
C


      SUBROUTINE BCHAR(CHAR)
C     =====================
C1    vartype           C
C1    iostatus          I
C2
C2    this routine blanks a character string
C2    by setting all character positions to
C2    spaces.
C
      CHARACTER*(*) CHAR
      INTEGER I
      INTEGER LEN
      INTRINSIC LEN
C
      DO 10 I=1,LEN(CHAR)
 10      CHAR(I:I)=' '
C
      END
C
C     ------------------------------------------------
C
      FUNCTION BINDEX(STR1,STR2)
C     ==========================
C1    vartype         C*(*) C*(*)
C1    iostatus         I     I
C
C2    This function is a backward index. It finds the last
C2    occurance of STR2 within STR1.
C
      INTEGER*4 BINDEX,L1,L2,I,NLEN
      CHARACTER STR1*(*),STR2*(*)
      L1=NLEN(STR1)
      L2=NLEN(STR2)
      BINDEX=0
      DO 10 I=L1-L2+1,1,-1
          IF(STR1(I:I+L2-1).EQ.STR2(:L2)) THEN
              IF(I.EQ.L1) THEN
                  BINDEX=0
              ELSE
                  BINDEX=I
              ENDIF
              RETURN
          ENDIF
10    CONTINUE
      END
C
C     ------------------------------------------------
C
 
      SUBROUTINE CHARCH(STRING)
C     =========================
C3    small routine similar to CRUNCH except
C3    that it leaves anything within quotes well
C3    alone. (useful for strings
      INTEGER*4 NL,NLEN,SP
      CHARACTER STRING*(*),CH*2
      CHARACTER TEMP*80
      EXTERNAL NLEN
      INTRINSIC INDEX
      LOGICAL TRIP
C-----COMPRESSES A CHARACTER 'STRING'.  (C).
      SP=0
      TRIP=.TRUE.
      NL=NLEN(STRING)
      IF (NL.EQ.0) RETURN
10    SP=SP+1
20    CH=STRING(SP:SP)
      IF(SP.EQ.NL) RETURN
      IF(CH.EQ.'"') TRIP=.NOT.TRIP
      IF(TRIP) THEN
          IF (CH.EQ.' ') THEN
              TEMP = STRING(SP+1:)
              STRING(SP:)=TEMP
              NL=NL-1
              GOTO 20
          END IF
      ENDIF
      GOTO 10
      END
C
C     ------------------------------------------------
C
      FUNCTION CR(LINE)
C     =================
C     *******************************************************
C                   Function  CR()
C     *******************************************************
      CHARACTER*120 LINE,TEMP,CR
      INTEGER K,I
      INTRINSIC LEN
      TEMP='
     +                                                  '
      K=0
      I=0
 10   I=I+1
      IF(LINE(I:I).NE.' ') THEN
         K=K+1
         TEMP(K:K)=LINE(I:I)
      END IF
      IF(I.LT.80) GOTO 10
      CR=TEMP(1:80)
      END
C
C     ------------------------------------------------
C
      FUNCTION CVERFY(CHR,STRING)
C     ===========================
C1    vartype    L     C1   C*
C1    iostatus   O     I    I
C2
C2    returns CVERFY .TRUE. if the character
C2    CHR exists in the character string STRING.
C2    otherwise .FALSE.
C
      INTEGER I,J,NLEN1
C
      CHARACTER CHR,STRING*(*)
C
      LOGICAL CVERFY
C
      EXTERNAL NLEN1
C
      CVERFY=.FALSE.
C
C
C      I=NLEN(STRING)-1
C      J=0
C
C 10   J=J+1
C      CVERFY=CHR.EQ.STRING(J:J)
C      IF ( .NOT. CVERFY .AND. J.LT.I ) GOTO 10
C
C      RETURN
C
      DO 20 I=1,NLEN1(STRING)
      IF (CHR.EQ.STRING(I:I)) THEN
         CVERFY=.TRUE.
      END IF
 20   CONTINUE
C
      RETURN
      END
C
C     ------------------------------------------------
C
      FUNCTION DCANG(X1,Y1,X2,Y2)
C     ===========================
C1        D          D, D, D, D
C1        O          I, I, I, I
C
C
      DOUBLE PRECISION DCANG,X1,Y1,X2,Y2,XDIF,YDIF,
     +                 ONE,DZERO,PI,VAL
      INTEGER DQUAD,QU
      INTRINSIC DATAN,ABS
      EXTERNAL DZERO,DQUAD
C
      PI = 4.0D0*DATAN(1.0D0)
      XDIF=DZERO(X2-X1)
      YDIF=DZERO(Y2-Y1)
C
      IF ( XDIF .EQ. 0.0D0 .AND. YDIF .GT. 0.0D0 ) THEN
C        90 degrees between points
         DCANG=PI/2.0
      ELSE IF ( XDIF .EQ. 0.0D0 .AND. YDIF .LE. 0.0D0 ) THEN
C       270 degrees between points
         DCANG=3.0*PI/2.0
      ELSE IF ( YDIF .EQ. 0.0D0 .AND. XDIF .GT. 0.0D0 ) THEN
C         0 degrees between points
         DCANG=0.0
      ELSE IF ( YDIF .EQ. 0.0D0 .AND. XDIF .LE. 0.0D0 ) THEN
C       180 degrees between points
         DCANG=PI
      ELSE
C
         VAL=ABS(YDIF)/ABS(XDIF)
         DCANG=DATAN(VAL)
C
         GOTO( 10,20,30,40)  DQUAD(XDIF,YDIF)
C
 10      GOTO 50
 20        DCANG=PI-DCANG
         GOTO 50
 30        DCANG=DCANG+PI
         GOTO 50
 40        DCANG=2.0*PI-DCANG
C
 50      CONTINUE
C
      END IF
C
      END
C
C     ------------------------------------------------
C
      FUNCTION  DCHKLN(XV1,XV2,X)
C     ===========================
C1                       D,  D,D
C1                       I,  I,I
C2       DCHKLN returns .true. if X lies
C2       between XV1 and XV2
C
      LOGICAL DCHKLN,DSAME
      DOUBLE PRECISION XV1,XV2,X
      EXTERNAL DSAME
C
      DCHKLN=DSAME(X,XV1) .OR. DSAME(X,XV2)
      IF ( DCHKLN ) RETURN
C
      IF ( XV2 .GT. XV1 ) THEN
         DCHKLN= X .GE. XV1 .AND. X .LE. XV2
      ELSE
         DCHKLN= X .GE. XV2 .AND. X .LE. XV1
      END IF
C
      END
C
C     ------------------------------------------------
C
      FUNCTION   DDSTXY(X1,Y1,X2,Y2)
C     ==============================
C1       D             D, D, D, D
C1       O             I, I, I, I
C2      Function DISTXY returns the distance between the
C2      points (X1,Y1) and (X2,Y2)
C
      DOUBLE PRECISION DDSTXY,X1,Y1,X2,Y2
      INTRINSIC DSQRT
C
      DDSTXY= DSQRT( (X1-X2)**2 + (Y1-Y2)**2 )
C
      END
C
C     ------------------------------------------------
C
      FUNCTION DQUAD(XC,YC)
C     =====================
C1       I           D, D
C1       O           I, I
C2      DQuad returns a value depending on the x and y
C2      coordinates    DQUAD   X    Y
C2                      1    +ve  +ve
C2                      2    -ve  +ve
C2                      3    -ve  -ve
C2                      4    +ve  -ve
C
      INTEGER DQUAD
      DOUBLE PRECISION XC,YC
      LOGICAL XPOS,YPOS
      XPOS = XC .GT. 0.0
      YPOS = YC .GT. 0.0
      IF ( XPOS.AND.YPOS ) THEN
         DQUAD = 1
      ELSE IF ( .NOT.XPOS.AND.YPOS ) THEN
         DQUAD = 2
      ELSE IF ( .NOT.XPOS.AND..NOT.YPOS ) THEN
         DQUAD = 3
      ELSE
         DQUAD = 4
      END IF
      END
C
C     ------------------------------------------------
C
      SUBROUTINE DSWAP(X,Y)
C     =====================
C1                     D,D
C1                     B,B
C2      Subroutine DSWAP swaps the values of X and Y
C2      round
C
      DOUBLE PRECISION X,Y,T
C
      T=X
      X=Y
      Y=T
C
      END
C
C     ------------------------------------------------
C
      FUNCTION   DZERO (VAL)
C     ======================
C1       D              D
C1       O              I
C2      Function DZERO sets VAL to zero if
C2      it's value is less than 1D-8
      include 'include/wtov.inc'
C
      DOUBLE PRECISION VAL,DZERO,ATOL
      REAL EXT
      INTRINSIC ABS
      DATA ATOL/1.0D-12/
C
      DZERO=VAL
      EXT=WPXMAX-WPXMIN
      IF ( ABS(VAL) .LE. EXT*ATOL ) DZERO = 0.0D0
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE FINDLN(STRING,LAYN,OK)
C     =================================
C
      include   'include/masti.inc'
C
      CHARACTER*(*) STRING
      INTEGER*4 LAYN,K,LENGTH,NLEN,LEN2,NLEN1
      LOGICAL OK
      EXTERNAL NLEN,NLEN1
C
      LENGTH=NLEN(STRING)
C     Scan through layers looking
C
C     set to illegal layer number.
      LAYN=-1
C
      DO 12 K=0,255
         LEN2=NLEN1(LNAME(K))
         IF ( STRING(1:LENGTH).EQ.LNAME(K)(1:LEN2) ) THEN
            LAYN=K
         END IF
 12   CONTINUE
C
      OK=LAYN.GT.-1
C
      END
C
C     ------------------------------------------------
C
C
C     ------------------------------------------------
C
      FUNCTION I4I2(I4)
C     =================
C
C1    vartype    I2 I4
C1    iostatus    O  I
C
C2    Subroutine I2I4 changes 2-byte
C2    integers into a single 4-byte integer
C2    by doing a direct bit substitution,to
C2    pack the 32-bit output.
C
      INTEGER*2 ITMP(1:2),I4I2
      INTEGER*4 I4,I4T
      EQUIVALENCE (I4T,ITMP)
C
      I4T=I4
CAPOLLO|SUN
      I4I2=ITMP(1)
CAPOLLO|SUN
CIBM
C      I4I2=ITMP(1)
CIBM
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE IVALU(STRING,IVAL,OK)
C     ================================
C
C1    vartype           C*(*)  I4   L
C1    iostatus            I    O   O
C
C2    Subroutine IVALU returns the first numeric
C2    value found in STRING in the integer variable IVAL.
C2    Logical flag Ok is returned true if successful.
C
      INTEGER*4 IVAL,NINT
      REAL RVAL
      LOGICAL OK
      CHARACTER*(*) STRING
C     INTRINSIC NINT
C
      EXTERNAL RVALU
C
      CALL RVALU(STRING,RVAL,OK)
      IF (.NOT.OK) THEN
         IVAL=1
         OK=.FALSE.
         RETURN
      END IF
      IVAL=NINT(RVAL)
      OK=.TRUE.
C
      END
C
C     ------------------------------------------------
C
      SUBROUTINE LCSUPP(STRING, LIST)
C     ===============================
C1    vartype            C*(*) C*(*)
C1    iostatus             I     I
C
C2    removes  leading character from  'STRING'
C2    if they are contained in  'LIST'
C
      INTEGER I, NLEN, INDEX, LEN1, LEN2, NSHIFT
      CHARACTER*(*) STRING, LIST
      EXTERNAL NLEN, SHIFT
      INTRINSIC INDEX, MAX
      LEN1 = NLEN(STRING)
      LEN2 = NLEN(LIST)
C     check that that the list is not a single
C     blank char
      IF(LEN2 .EQ. 0 .AND. LIST(1:1) .EQ. ' ') LEN2 = 1
C     right then loop along 'STRING'
      DO 23 I = 1, LEN1
         IF(INDEX(LIST(1:LEN2),STRING(I:I)) .EQ. 0) THEN
C          right so we have hit the first element of 'STRING'
C          not contained in LIST
C          set size of shift to remove leading chars
           NSHIFT = -(I - 1)
           GOTO 33
         ENDIF
 23   CONTINUE
C     so we've got here so there are no leading chars
C     to be removed  so set NSHIFT to ZERO
      NSHIFT  = 0
 33   CONTINUE
C     move the 'STRING' along to erase the leading chars
      CALL SHIFT(STRING,NSHIFT)
C
      END
C
C---------------------------------------------------------
C
      FUNCTION LOGIC(LOGEXP)
C     ======================
C1    vartype          l
C1    iostat           i
C
C2    The function here is simply able to convert
C2    a LOGICAL true or false into an arithmetic
C2    value. 0 for false and 1 for true
C
      LOGICAL LOGEXP
      INTEGER*4 LOGIC
      LOGIC=0
      IF (LOGEXP) LOGIC=1
      END
C
C     ------------------------------------------------
C
      FUNCTION RESPON(C1,FN)
C     ======================
C  vartyp L       C*(*)
C  iostat O         I
      include  'include/vntable.inc'
C          1234567890123
C         "YNLCRPCTTIRHS"
C          12
C         "YN         " yes/no.
C            345
C         "  LCR      " left/centre/right.
C               678  1
C         "     PCT   " property/calculation/tally.
C                  901
C         "        TIR" text/integer/real.
C                     23
C         "           HS" Hole/solid
      INTEGER FN
      LOGICAL RESPON
      CHARACTER*(*) C1,C11*1
      C11=C1(1:1)
      IF ( ICHAR(C11).GT.96) C11=CHAR(ICHAR(C11)-32)
      RESPON=C11.EQ.DICT01(1)(FN:FN)
      END
C
C     ------------------------------------------------
C
      SUBROUTINE RVALU(STRING,RVAL,OK)
C     ================================
C
C1    vartype           C*(*)  R   L
C1    iostatus            I    O   O
C
C2    Subroutine RVALU returns the first numeric
C2    value found in STRING in the real variable RVAL.
C2    Logical flag Ok is returned true if successful.
C
      INTEGER*4 J,K
      REAL RVAL
      LOGICAL OK
      CHARACTER*(*) STRING,FORM*16
C
      EXTERNAL DBOUND
C
C     find bounds of numeric field in string
      CALL DBOUND(STRING,J,K,*99)
C     set format to read
      K=K-J+1
      WRITE(UNIT=FORM,FMT='(A,I2.2,A)')'(F',K,'.0)'
C
C      WRITE(10,*) '[RVALU]','"',FORM,'"',LEN(STRING),'  ',J
C      WRITE(10,*) '[RVALU]',STRING
C
C     extra check that subscript value is in range
      IF(J.LE.0.OR.J.GT.LEN(STRING)) GOTO 99
      READ(UNIT=STRING(J:),FMT=FORM,ERR=99) RVAL
C
C      WRITE(10,*) '[RVALU]',RVAL
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
      RVAL=1
      OK=.FALSE.
      END
C
C     ------------------------------------------------
C
      SUBROUTINE STRIPO(CHR,CH,REP)
C     ============================
      CHARACTER*(*) CHR,CH,REP
      INTEGER POS,L
 
      L=LEN(CH)-1
   77 CONTINUE
      POS=INDEX(CHR,CH)
      IF ( POS.GT.0 ) THEN
         CHR(POS:POS+L)=REP
         GOTO 77
      END IF
 
      END
C
C     ------------------------------------------------
C
      SUBROUTINE STRNUM(LINE,DATA,NDATA)
C     ==================================
C
      CHARACTER*(*) LINE,OLINE*80,TEMP*80,FORM*10,FORMAT*20
      INTEGER*4 DATA(1:256),NDATA,IT,VARNM,POINTR,I,MINPOS,INDEX,NLEN
      LOGICAL LAST,GLOB
      INTRINSIC INDEX
      EXTERNAL BCHAR,NLEN,CRUNCH
C
 
      IT=NLEN(LINE)
      I=0
 5    CONTINUE
      I=I+1
      IF ( ICHAR(LINE(I:I)).GT.64 ) THEN
         CALL STRSTR(LINE,DATA,NDATA)
         RETURN
      ELSE IF ( I .LT. IT ) THEN
         GOTO 5
      END IF
C
      I=1
      NDATA=1
      LAST=.FALSE.
      GLOB=.FALSE.
C
 10   CONTINUE
C
      FORMAT='(I***)'
C
      IT=I
C
      I=INDEX(LINE,'-')
C
      GLOB= I .NE. 0
C
      IF ( GLOB  ) THEN
         IF ( I .EQ. 1 ) GOTO 20
         MINPOS=I
      END IF
C
      I=IT
C
      I=INDEX(LINE,',')
C
      IF ( I .EQ. 0 ) THEN
         LAST=.TRUE.
         I=NLEN(LINE)+2
      ELSE IF ( I .EQ. 1 ) THEN
         GOTO 20
      END IF
C
      IF ((GLOB.AND.I.GT.MINPOS).OR.(GLOB.AND.LAST)) THEN
C
         CALL BCHAR(TEMP)
         TEMP=LINE(IT:MINPOS-1)
         WRITE(UNIT=FORM,FMT='(I3)' ) MINPOS-IT
         FORMAT(3:5)=FORM
         READ(UNIT=TEMP,FMT=FORMAT,ERR=20) DATA(NDATA)
         POINTR=NDATA
         NDATA=NDATA+1
         CALL BCHAR(TEMP)
C         WRITE(10,*) 'STRNUM',MINPOS+1,I-1
         IF ( MINPOS+1.GT.I-1) GOTO 20
         TEMP=LINE(MINPOS+1:I-1)
         WRITE(UNIT=FORM,FMT='(I3)' ) I-MINPOS
         FORMAT(3:5)=FORM
         READ(UNIT=TEMP,FMT=FORMAT,ERR=20) VARNM
 111     CONTINUE
         DATA(NDATA)=DATA(NDATA-1)+1
         IF(DATA(NDATA).LE.VARNM.AND.NDATA.LT.256) THEN
            NDATA=NDATA+1
            GOTO 111
         ELSE IF ( NDATA.EQ. 256 ) THEN
            NDATA=NDATA+1
         END IF
C
         GLOB=.FALSE.
      ELSE
         CALL BCHAR(TEMP)
         TEMP=LINE(IT:I-1)
         WRITE(UNIT=FORM,FMT='(I3)' ) I-IT
         FORMAT(3:5)=FORM
         CALL CRUNCH(FORMAT)
         READ(UNIT=TEMP,FMT=FORMAT,ERR=20) DATA(NDATA)
         NDATA=NDATA+1
      END IF
      I=I+1
      TEMP(1:)=LINE(I:)
      LINE(1:)=TEMP
      I=1
      IF ( .NOT. LAST ) GOTO 10
C
      NDATA=NDATA-1
      RETURN
 20   NDATA=0
      END
C
C     ------------------------------------------------
C
      SUBROUTINE STRSTR(LINE,DATA,NDATA)
C     ==================================
C1                      C*(*)I4(*) I4
      CHARACTER*(*) LINE,OUTPUT*30
      LOGICAL OK
      INTEGER*4 DATA(1:256),NDATA,COMPOS,ABSPOS,NLEN,LIM
      EXTERNAL NLEN
 
C
C     set into upper case.
      CALL FOLDUP(LINE)
C     set position for searching string
      ABSPOS=1
C     set count for number of layers found
      NDATA=1
C     find end of string
      LIM=NLEN(LINE)
C
 5    CONTINUE
C
C     look for comma in string
      COMPOS=INDEX(LINE(ABSPOS:),',')
C
      IF ( COMPOS .GT. 0 ) THEN
         CALL FINDLN(LINE(ABSPOS:ABSPOS+COMPOS-2),DATA(NDATA),OK)
         IF ( OK ) THEN
C           increment number layers found so far.
            NDATA=NDATA+1
         ELSE
            OUTPUT='Unknown layer name'//LINE(ABSPOS:ABSPOS+COMPOS-1)
            CALL CPRINT(OUTPUT)
         END IF
C        jump past the comma in string.
         ABSPOS=ABSPOS+COMPOS
      ELSE IF ( ABSPOS .LE. LIM ) THEN
C        last layer name.
         CALL FINDLN(LINE(ABSPOS:LIM),DATA(NDATA),OK)
         IF ( .NOT.OK ) THEN
            OUTPUT='Unknown layer name'//LINE(ABSPOS:LIM)
            CALL CPRINT(OUTPUT)
         END IF
         RETURN
      END IF
C
      GOTO 5
 
      END
C
C     ------------------------------------------------
C
      SUBROUTINE TCSUPP(STRING, LIST)
C     ===============================
C1    vartype            C*(*) C*(*)
C1    iostatus             I     I
C
C2    removes  trailing characters from  'STRING'
C2    if they are contained in  'LIST'
C
      INTEGER I, NLEN, INDEX, LEN1, LEN2
      CHARACTER*(*) STRING, LIST, TEST*80
      EXTERNAL NLEN
      INTRINSIC INDEX
C     add a space to the 'LIST' sring of characters
C     that has to be removed
      LEN2 = NLEN(LIST)
      LEN1 = NLEN(STRING)
      DO 43 I = LEN1,1,-1
C        if the character is not to be removed then exit
C        the function
         IF(INDEX(LIST(1:LEN2),STRING(I:I)) .EQ. 0) GOTO 53
C        so the current character is one that has to be removed
         STRING(I:I) =' '
 43   CONTINUE
C
 53   CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      FUNCTION YESOK(C1)
C     ==================
C  vartyp L       C*(*)
C  iostat O         I
      include  'include/vntable.inc'
C          1234567890123
C         "YNLCRPCTTIRHS"
      LOGICAL YESOK
      CHARACTER*(*) C1,C11*1
      C11=C1(1:1)
      IF ( ICHAR(C11).GT.96) C11=CHAR(ICHAR(C11)-32)
      YESOK=C11.EQ.DICT01(1)(1:1)
      END
C
C     ------------------------------------------------
C
