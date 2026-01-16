C
C         SCCS id Keywords             @(#)  300.2 date 12/10/91 menuload.f  
C
      PROGRAM MENLOD
C     ===================
C2
C2    Program MENLOD loads the verb-noun table
C2    into the internal files from an external file.
C
C
      INTEGER*4 VNNUM,UNITN,VNP,P1,P2,LNUM,I,TXTUN,TOKUN,L
      INTEGER*4 NLEN
      INTEGER*4 ST
      LOGICAL EX,OK
      CHARACTER TOKEN*1,WORD*16,FILNAM*80,STRING*68,STRNG2*80
      CHARACTER*80 TXTNAM,TOKNAM
      EXTERNAL IVALU,NLEN
C
      LNUM=0
      UNITN=10
      FILNAM='menuc.044'
      TXTUN=11
      TXTNAM='m.000'
      TOKUN=12
C
      WRITE(UNIT=*,FMT=*)'  Enter name of menu source file-'
      READ(UNIT=*,FMT='(A)') FILNAM
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF(EX) THEN
         WRITE(UNIT=*,FMT=*)'  Enter name of user menu output file-'
         READ(UNIT=*,FMT='(A)') TXTNAM
         WRITE(UNIT=*,FMT=*)'  Enter name of system token file-'
         READ(UNIT=*,FMT='(A)') TOKNAM
         OPEN(UNIT=UNITN,
     +        FILE=FILNAM,
     +        ACCESS='SEQUENTIAL',
     +        FORM ='FORMATTED',
     +        STATUS='OLD',
     +        IOSTAT=ST,
     +        ERR=999)

         REWIND(UNIT=UNITN)
C        now open output files
         OPEN(UNIT=TXTUN,FILE=TXTNAM,ACCESS='SEQUENTIAL')
         OPEN(UNIT=TOKUN,FILE=TOKNAM,ACCESS='SEQUENTIAL',
     +         FORM='UNFORMATTED')
C
C        delete both output files and re-open
         CLOSE(UNIT=TXTUN,STATUS='DELETE')
         OPEN(UNIT=TXTUN,FILE=TXTNAM,ACCESS='SEQUENTIAL')
         CLOSE(UNIT=TOKUN,STATUS='DELETE')
         OPEN(UNIT=TOKUN,FILE=TOKNAM,ACCESS='SEQUENTIAL',
     +         FORM='UNFORMATTED')
         WRITE(UNIT=TOKUN) 999999,'X'
 10      CONTINUE
         READ(UNIT=UNITN,FMT='(A)',END=20)STRNG2
C         WRITE(*,'(A)') STRNG2 
         LNUM=LNUM+1
C        test for comment line
         IF (STRNG2(1:1).EQ.'*') THEN
            WRITE(UNIT=TXTUN,FMT='(A)')STRNG2
            GOTO 10
         END  IF
C        skip over PT internal comments
         IF (STRNG2(1:1).EQ.'$') GOTO 10
C
         READ(STRNG2,FMT='(2I6,A)',ERR=20)VNNUM,VNP,STRING
C        find delimiters for character token
         P1=INDEX(STRING,'(')
         P2=INDEX(STRING,')')
         OK=P1.NE.0 .AND. P2.NE.0 .AND. P2.GT.P1
         IF (OK) THEN
C           found a character token
            TOKEN=STRING(P1+1:P1+1)
         ELSE
C           try for integer token
C           find delimiters for integer token
            P1=INDEX(STRING,'<')
            P2=INDEX(STRING,'>')
            OK=P1.NE.0 .AND. P2.NE.0 .AND. P2.GT.P1
            IF (OK) THEN
C              extract integer value
               CALL IVALU(STRING(P1:P2),I,OK)
               IF (OK) THEN
                  TOKEN=CHAR(I)
               ELSE
C                 error in integer value
                  WRITE(UNIT=*,FMT=*)' Error on line ',LNUM,
     +               ' Invalid character value for token'
                  GOTO 10
               END IF
            ELSE
C              no token found
               WRITE(UNIT=*,FMT=*)' Error on line ',LNUM,' No token'
C               write(unit=*,fmt=*)string
               GOTO 10
            END IF
         END IF
C        find delimiters for character string
         P1=INDEX(STRING,'"')
         P2=INDEX(STRING(P1+1:),'"')
         IF (P2.GT.0) THEN
            P2=P1+P2
         ELSE
            P2=P1
         END IF
         OK=P1.NE.0 .AND. P2.NE.0 .AND. P2.GT.P1
         IF (OK) THEN
C           read the command word
            WORD=STRING(P1+1:P2-1)
         ELSE
C           cannot delimit command word
            WRITE(UNIT=*,FMT=*)' Error on line ',LNUM,' No Command'
            GOTO 10
         END IF
C
         L=NLEN(WORD)
C        write data to editable file
         IF ( L.GT.0 ) THEN
             WRITE(UNIT=TXTUN,FMT='(2I6,3A)')VNNUM,VNP,
     +         ' /',WORD(1:L),'/'
         ELSE
             WRITE(UNIT=TXTUN,FMT='(2I6,3A)')VNNUM,VNP,
     +          ' /','** BLANK **','/'
         ENDIF
C        write data to binary token file
C         WRITE(*,*) 'TOKUN,VNNUM,TOKEN',TOKUN,VNNUM,TOKEN
         WRITE(UNIT=TOKUN)VNNUM,TOKEN
         GOTO 10
 20      CONTINUE
         CLOSE(UNIT=UNITN,STATUS='KEEP')
         CLOSE(UNIT=TXTUN,STATUS='KEEP')
         CLOSE(UNIT=TOKUN,STATUS='KEEP')
C        remind him where to find things
         WRITE(UNIT=*,FMT=*)' Editable user file ',TXTNAM
      WRITE(UNIT=*,FMT=*)' Binary system token list in file ',TOKNAM
C
      ELSE
C        cannot find source file
      WRITE(UNIT=*,FMT=*)' Cannot find ',FILNAM
      STOP
999   CONTINUE
      PRINT*, '[F77 OPEN] Error code ',ST

      END IF
C
C
      END
C
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
 
C      WRITE(10,*) '[RVALU]','"',FORM,'"',LEN(STRING),'  ',J
C      WRITE(10,*) '[RVALU]',STRING
 
      READ(UNIT=STRING(J:),FMT=FORM,ERR=99) RVAL
 
C      WRITE(10,*) '[RVALU]',RVAL
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
      RVAL=1
      OK=.FALSE.
      END
C
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
 
      EXTERNAL RVALU
C
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
C
      SUBROUTINE DBOUND(STRING,P1,P2,*)
C     =================================
      INTEGER NL,NLEN,NVRFY,P1,P2,TABLE(1:8,0:4),CH(0:15),ROW
      CHARACTER*(*) DSTR1*20,DSTR2*20,STRING
      PARAMETER (DSTR1='0123456789.+-', DSTR2='0123456789.+-ED')
      EXTERNAL NLEN,NVRFY
      INTRINSIC INDEX
      DATA CH/0, 10*1, 2, 2*3, 2*4/,
     +     TABLE/0,2*-1,3*0,-1,0, 2*2,2*3,2,3*7, 4,3,-1,0,4,0,-1,0,
     +     5,2*-1,2*0,8,-1,0, 0,2*6,3*0,-1,0/
C-----LOCATES BOUNDS OF FIRST NUMERICAL VALUE IN 'STRING'.  (C,I,I,*).
      P1=NVRFY(STRING,DSTR1)
      IF (P1.GT.0) THEN
         NL=NLEN(STRING)
         P2=P1
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
*
C
      FUNCTION NVRFY(STRING,C)
C     ========================
      INTEGER NVERFY,NVRFY
      CHARACTER*(*) C,STRING
      INTRINSIC INDEX,LEN
C-----RETURNS POSN. OF FIRST CHARACTER IN 'STRING' WHICH IS ALSO IN 'C'.  I=(C,C).
      ENTRY NVERFY(STRING,C)
*     =====
      DO 10, NVRFY=1,LEN(STRING)
         IF (INDEX(C,STRING(NVRFY:NVRFY)).GT.0) RETURN
   10 CONTINUE
      NVRFY=0
      END
*
      FUNCTION VRFY(STRING,C)
C     =======================
      INTEGER VERIFY,VRFY
      CHARACTER*(*) C,STRING
      INTRINSIC INDEX,LEN
C-----RETURNS POSN. OF FIRST CHARACTER IN 'STRING' WHICH IS NOT IN 'C'.  I=(C,C).
      ENTRY VERIFY(STRING,C)
*     =====
      DO 10, VRFY=1,LEN(STRING)
         IF (INDEX(C,STRING(VRFY:VRFY)).EQ.0) RETURN
   10 CONTINUE
      VRFY=0
      END
*
      SUBROUTINE CRUNCH(STRING)
C     =========================
      INTEGER J,NL,NLEN,SP
      CHARACTER STRING*(*)
      EXTERNAL NLEN
      INTRINSIC INDEX
C-----COMPRESSES A CHARACTER 'STRING'.  (C).
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
      END
*
      SUBROUTINE CNPRCH(STRING)
C     =========================
      INTEGER J,NLEN
      CHARACTER STRING*(*),CH,SPACE,TILDE
      PARAMETER (CH='?', SPACE=' ', TILDE='~')
      EXTERNAL NLEN
      INTRINSIC LGT,LLT
C-----CHANGES NON_PRINT_CHARACTERS TO 'CH'.  (C).
      DO 10, J=1,NLEN(STRING)
         IF (LLT(STRING(J:J),SPACE) .OR. LGT(STRING(J:J),TILDE))
     +      STRING(J:J)=CH
   10 CONTINUE
      END

C
      FUNCTION NLEN(CHAR)
C     ===================
C1    vartype   I4    C
C1    iostatus  O     I
C1
C1    Function NLEN returns the correct active
C1    length of a text string contained in 'CHAR'.
C           a 0 if the string is blank
C
      INTEGER*2 I,LEN
      INTEGER*4 NLEN
      CHARACTER*(*) CHAR
      INTRINSIC LEN

      I=LEN(CHAR)+1
  10  I=I-1
      IF ( CHAR(I:I) .EQ. ' ' ) THEN
         IF ( I .GT. 1 ) GOTO 10
         I=0
      END IF
      NLEN=INT(I)
      END
*
C
*
