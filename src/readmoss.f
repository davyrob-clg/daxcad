C
C
C     @(#)  412.1 date 6/11/92 readmoss.f 
C
C
C     Filename    : readmoss.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:43:52
C     Last change : 92/06/11 14:39:50
C
C     Copyright : Practical Technology Limited  
C     File :- readmoss.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION AZIMU(R)
C     FUNCTION AZIMUT(R)
C     DOUBLE PRECISION FUNCTION VALUE(STRING)
C     SUBROUTINE DBOUN1(STRING,P1,P2,ST)
C     SUBROUTINE DEWCF7(FUNIT,NP,FORM,TLAY,P,OK)
C     SUBROUTINE DFORMT(FORM,FORMS,NF,ST)
C     SUBROUTINE DSTAKF(OPSTAK,OPDATA,FORMS,NF,ST)
C     SUBROUTINE FREPT(FORM,P,IRPT,ST)
C     SUBROUTINE GETNUM(STRING,RVAL,INTEG,P2,ST)
C     SUBROUTINE RDGENI(FILNM)
C     SUBROUTINE REDGI0()
C     SUBROUTINE TPAREN(STRING,ST)
C
C
C     |-----------------------------------------------------------------|
C
C
      FUNCTION AZIMU(R)
C     ==================
C
C1    vartype     R   R
C1    iostatus    O   I
C
C2    Function AZIMU returns the AZIMUTH angle
C2    conversion of the conventional angle R.
C2    All angles are expressed in RADIANS.
C2    The function also supports the conversion
C2    of azimuth angles to conventional angles.
C
      REAL AZIMU,R,PI,PI25
      PARAMETER (PI=3.14159265)
      PARAMETER (PI25=PI*2.5)
C
      AZIMU=MOD((PI25-R),PI)
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 azimut.ftn Daxcad revision 1.8
      FUNCTION AZIMUT(R)
C     ==================
C
C1    vartype     R   R
C1    iostatus    O   I
C
C2    Function AZIMUT returns the AZIMUTH angle
C2    conversion of the conventional angle R.
C2    All angles are expressed in DEGREES.
C2    The function also supports the conversion
C2    of azimuth angles to conventional angles.
C
      REAL AZIMUT,R
C
      AZIMUT=MOD((450.0-R),360.0)
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dboun1.ftn Daxcad revision 1.8
      SUBROUTINE DBOUN1(STRING,P1,P2,ST)
C     ==================================
C
C1    vartype            C*(*) I4 I4 I4
C1    iostatus             I   O  O  O
C
C2    Subroutine DBOUN1 locates the bounds of the first numeric
C2    value in STRING,returning the pointers into the string
C2    in P1 and P2. ST returns the completion status 0 for success.
C
      INTEGER*4 NL,NLEN,NVRFY,P1,P2,TABLE(1:8,0:4),CH(0:15),ROW
      INTEGER*4 ST,P
      CHARACTER*(*) DSTR1*13,DSTR2*15,STRING
      PARAMETER (DSTR1='0123456789.+-',DSTR2='0123456789.+-ED')
      EXTERNAL NLEN,NVRFY
      INTRINSIC INDEX
C
      DATA CH/0,10*1,2,2*3,2*4/,
     +     TABLE/0,2*-1,3*0,-1,0,2*2,2*3,2,3*7,4,3,-1,0,4,0,-1,0,
     +     5,2*-1,2*0,8,-1,0,0,2*6,3*0,-1,0/
C
      ST=0
C     test for null string
      NL=NLEN(STRING)
      IF (NL.EQ.0) THEN
C        null string
         P2=0
         ST=1
         RETURN
      END IF
C
C     skip leading spaces in string
      P=0
 5    CONTINUE
      P=P+1
      IF (STRING(P:P).EQ.' ' .AND. P.LT.NL) THEN
         GOTO 5
      END IF
C
      P1=NVRFY(STRING(P:),DSTR1)
      IF (P1.GT.0) THEN
         P1=P1+P-1
         NL=NLEN(STRING)
         P2=P1
         ROW=1
 10      CONTINUE
         IF (P2.LE.NL) THEN
            ROW=TABLE(ROW,CH(INDEX(DSTR2,STRING(P2:P2))))
            IF (ROW.GT.0) THEN
               P2=P2+1
               GOTO 10
            END IF
            IF (ROW.EQ.0) THEN
               ST=1
               RETURN
            END IF
         ELSE IF (TABLE(ROW,0).EQ.0) THEN
            ST=2
            RETURN
         END IF
         P2=P2-1
      ELSE
         P2=0
      END IF
      ST=0
C
      END
C
C     ---------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dewcf7.ftn Daxcad revision 1.8
      SUBROUTINE DEWCF7(FUNIT,NP,FORM,TLAY,P,OK)
C     ==========================================
C1    vartype           I     I4   I4  I2  I2  L
C1    iostatus          I     I    O   O   O   O
C
C2    Subroutine DEWCF7 creates a Spline Curve entity
C2    by using the control data contained in passed file
C2    unit FUNIT,in a total of NP point records.
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,TLAY
      INTEGER*4 NP,FUNIT,I,FORM
      REAL X1,Y1,X2,Y2
      LOGICAL OK
C
      EXTERNAL FILL,DIW500,DBW500,ADDMON,ADDISP
C
C     initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=SPLINE
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     current line thickness
      IMBUFF(12)=THICK
C     should be no text pointer
      IMBUFF(9)=0
C     set passed layer number
      IMBUFF(4)=  TLAY
C     Store the type of spline we have
      IMBUFF(5)=FORM
C     set fixed font for spline
      TFONT=1
      IMBUFF(6)=TFONT
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C     now write the PD data
      RDBUFF(1)=0.0
      RDBUFF(2)=0.0
      RDBUFF(3)=0.0
      RDBUFF(4)=0.0
      RDBUFF(5)=0.0
      RDBUFF(6)=0.0
C     write out two header records for storing
C     end point locks
      CALL DBW501(P2,OK)
      CALL DBW501(P2,OK)
C
C     read points data from file and write
C     to data structure
      DO 30 I=1,NP-1
         READ(UNIT=FUNIT,REC=I) RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +                          RDBUFF(4),RDBUFF(5),RDBUFF(6)
         CALL DBW501(P2,OK)
 30   CONTINUE
C     write last point to structure
      READ(UNIT=FUNIT,REC=NP) RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +                        RDBUFF(4),RDBUFF(5),RDBUFF(6)
      CALL DBW500(P2,OK)
C     P2 returns the PDF position used for last point
C     must regenerate bounding box for spline
      CALL CHGE40(P)
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
      END
C
C     --------------------------------------------------
C
 
 
 
 
C       @(#)  256.1 date 12/16/89 dformt.ftn Daxcad revision 1.8
      SUBROUTINE DFORMT(FORM,FORMS,NF,ST)
C     ===================================
C
C1    vartype         C*(*) C*6(32) I4 I4
C1    iostatus          I     O     O  O
C
C2    Subroutine DFORMT decodes a FORTRAN FORMAT statement
C2    in string FORM into it's individual elements and returns
C2    them in the character array FORMS.The number of elements
C2    is returned in NF.If any errors occur,the status ST
C2    is returned as non-zero.
C
      INTEGER*4 ST
      INTEGER*4 NF,NL,NLEN,OPN,I,J,P2,IRPT,OPSP,OPDP
      INTEGER*4 OPSTAK(3,32)
      LOGICAL OK
      REAL R1
      CHARACTER*6 OPDATA(32),FRMOPS*8,FSTRNG*6,FORMS(32)
      CHARACTER*(*) FORM
      PARAMETER (FRMOPS='?FFFFF()')
C
      FSTRNG=' '
      OPSP=0
      OPDP=0
C     get active length of format statement
      NL=NLEN(FORM)
      IF (NL.EQ.0) THEN
C        blank format
         ST=1
         RETURN
      END IF
C
C     work through statement,and build stack
      I=0
 5    CONTINUE
      I=I+1
      IF (I.LE.NL) THEN
C        test for valid operation
         OPN=INDEX(FRMOPS,FORM(I:I))
         IF (OPN.NE.0) THEN
            IF (OPN.EQ.2) THEN
C              'F' Real format
               CALL FREPT(FORM,I,IRPT,ST)
               IF (ST.NE.0) THEN
C                 error in repeat field
                  RETURN
               END IF
               IF (IRPT.EQ.0) IRPT=1
C              go get actual format next
               CALL GETNUM(FORM(I:),R1,OK,P2,ST)
               IF (ST.NE.0 .OR. OK) THEN
C                 not a real format
                  ST=1
                  RETURN
               END IF
C              get pointer to last char of format
               P2=I+P2-1
C              get format string
               FSTRNG=FORM(I:P2)
C              update current pointer to end of format
               I=P2
C
C              store op in stack
               OPSP=OPSP+1
               OPSTAK(1,OPSP)=OPN
               OPSTAK(2,OPSP)=IRPT
               OPDP=OPDP+1
               OPSTAK(3,OPSP)=OPDP
               OPDATA(OPDP)=FSTRNG
            ELSE IF (OPN.EQ.7) THEN
C              '(' Open parenthesis
               CALL FREPT(FORM,I,IRPT,ST)
               IF (I.EQ.1) THEN
C                 first character of statement
                  IRPT=1
                  ST=0
               ELSE
                  CALL FREPT(FORM,I,IRPT,ST)
               END IF
               IF (ST.NE.0) THEN
C                 error in repeat field
                  RETURN
               END IF
               IF (IRPT.EQ.0) IRPT=1
C              store op in stack
               OPSP=OPSP+1
               OPSTAK(1,OPSP)=OPN
               OPSTAK(2,OPSP)=IRPT
               OPSTAK(3,OPSP)=0
            ELSE IF (OPN.EQ.8) THEN
C              ')' close parenthesis
c              store op in stack
               OPSP=OPSP+1
               OPSTAK(1,OPSP)=OPN
               OPSTAK(2,OPSP)=0
               OPSTAK(3,OPSP)=0
            END IF
         END IF
C        go get next op-code
         GOTO 5
      END IF
C
C     unstack format elements into array FORMS
      CALL DSTAKF(OPSTAK,OPDATA,FORMS,NF,ST)
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dstakf.ftn Daxcad revision 1.8
      SUBROUTINE DSTAKF(OPSTAK,OPDATA,FORMS,NF,ST)
C     ============================================
C
C1    vartype          I4(3,32) C*6(32) C*6(32) I4 I4
C1    iostatus            I       I       O     O  O
C
C2    subroutine DSTAKF interprets the i/o format stack
C2    passed in OPSTAK,OPDATA into format elements in the
C2    array FORMS.The number of elements is returned in NF.
C2    The completion status is returned in ST,zero for success.
C
      INTEGER*4 ST,OPSTAK(3,32),LOOP(8),LSTART(8),PN,OP,RPT
      INTEGER*4 I,J,K,OPSP,OPDP,NF
      CHARACTER*6 OPDATA(32),FORMS(32)
C
C     set pointers to start
      OPSP=0
      OPDP=0
      RPT=1
      PN=0
      NF=0
C
 10   CONTINUE
      OPSP=OPSP+1
C     get current OP code
      OP=OPSTAK(1,OPSP)
C     get current REPEAT count
      RPT=OPSTAK(2,OPSP)
C     get current DATA pointer
      OPDP=OPSTAK(3,OPSP)
C
C     execute OP-code function
      IF (OP.EQ.0) THEN
C        null op
         GOTO 10
      ELSE IF (OP.EQ.2) THEN
C        REAL Format code
         DO 101 I=1,RPT
            NF=NF+1
            FORMS(NF)=OPDATA(OPDP)
 101     CONTINUE
      ELSE IF (OP.EQ.7) THEN
C        start of parenthesis
         PN=PN+1
         LOOP(PN)=RPT
         LSTART(PN)=OPSP
      ELSE IF (OP.EQ.8) THEN
C        end of parenthesis
         LOOP(PN)=LOOP(PN)-1
         IF (LOOP(PN).GT.0) THEN
C           set pointer to start of loop
            OPSP=LSTART(PN)
         ELSE
C           this loop complete
            PN=PN-1
         END IF
         ST=0
      ELSE
C        unknown op-code
         ST=1
      END IF
C     test status before continuing
      IF (PN.GT.0) GOTO 10
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 frept.ftn Daxcad revision 1.8
      SUBROUTINE FREPT(FORM,P,IRPT,ST)
C     ================================
C
C1    vartype         C*(*) I4 I4  I4
C1    iostatus          I   I  O   O
C
      INTEGER*4 P,IRPT,ST
      INTEGER*4 P1,P2,P3
      LOGICAL OK
      REAL R1
      CHARACTER*(*) FORM,DELIMS*8
      PARAMETER (DELIMS=',(//////')
C
C     set start pointer
      P1=P
 5    CONTINUE
C     search backward for delimiter
      P1=P1-1
      IF (P1.GE.1) THEN
         P3=INDEX(DELIMS,FORM(P1:P1))
         IF (P3.EQ.0) GOTO 5
C        found delimiter at position P1
         IF (P-P1.EQ.1) THEN
C           immediately preceding char is delimiter
            IRPT=0
         ELSE
C           possible repeat field present
            P2=P-1
C           get repeat count
            CALL GETNUM(FORM(P1:P2),R1,OK,P3,ST)
            IF (.NOT.OK .OR. ST.NE.0) THEN
C              error in integer field
               ST=1
               RETURN
            END IF
C           integer repeat has been returned
            IRPT=R1
         END IF
         ST=0
      ELSE
C        at start of string
         ST=1
      END IF
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 getnum.ftn Daxcad revision 1.8
      SUBROUTINE GETNUM(STRING,RVAL,INTEG,P2,ST)
C     ==========================================
C
C1    vartype            C*(*)  R    L    I4 I4
C1    iostatus             I    O    O    O  O
C
C2    Subroutine GETNUM returns the first number
C2    either Real or Integer found in STRING.
C2    A blank string is interpreted as Real 0.0
C2    If an Integer is found,then it is returned
C2    as a Real in RVAL and the logical INTEG
C2    is returned true.The position of the last character
C2    of the number in the string is returned in P2.
C2    Completion status is returned in ST,0 for success.
C
      INTEGER*4 ST,P1,P2,P3,L1,L2,I1,NLEN
      REAL RVAL,R1
      LOGICAL INTEG
      CHARACTER STRING*(*),FORM*16
C
      EXTERNAL DBOUN1,NLEN
C
C     search for numeric field first
      CALL DBOUN1(STRING,P1,P2,ST)
      IF (ST.NE.0) THEN
C        test for blank field
         IF (NLEN(STRING).EQ.0) THEN
C           blank field,return zero result
            RVAL=0.0
            INTEG=.FALSE.
            ST=0
            RETURN
         END IF
         ST=2
         RETURN
      END IF
C     test for real number
      P3=INDEX(STRING(P1:P2),'.')
      IF (P3.EQ.0) THEN
C        Integer number found
         INTEG=.TRUE.
         L1=P2-P1+1
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A,I3,A)') '(I',L1,')'
C         WRITE(*,'(A,A,A,I2,A,I2)')
C     +   ' STRING=',STRING,':P1=',P1,' P2=',P2
C         WRITE(*,'(A,A,A,A,A,I2)')
C     +   ' STRING=',STRING(P1:P2),': FORM=',FORM,':L1=',L1
C         WRITE(*,*) ' STRING(P2:P2)=',STRING(P2:P2)
         READ(UNIT=STRING(P1:),FMT=FORM,ERR=99) I1
         RVAL=I1
      ELSE
C        Real number found
         INTEG=.FALSE.
         L1=P2-P1+1
         L2=L1-P3
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A,I2,A,I1,A)') '(F',L1,'.',L2,')'
C         WRITE(*,'(A,A,A,I2,A,I2)')
C     +   ' STRING=',STRING,':P1=',P1,' P2=',P2
C         WRITE(*,'(A,A,A,A,A,I2,A,I2)')
C     +   ' STRING=',STRING(P1:P2),': FORM=',FORM,':L1=',L1,' L2=',L2
C         WRITE(*,*) ' STRING(P2:P2)=',STRING(P2:P2)
         READ(UNIT=STRING(P1:),FMT=FORM,ERR=99) R1
         RVAL=R1
      END IF
      ST=0
      RETURN
C
 99   CONTINUE
      ST=1
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 rdgeni.ftn Daxcad revision 1.8
      SUBROUTINE RDGENI(FILNM)
C     ========================
C
C    A program to translate the content of a GENIO
C    file from the MOSS system into DAXCAD format.
C
      include 'include/ndata.inc'
      include 'include/masti.inc'
      include 'include/lfu.inc'
      include 'include/wtov.inc'
      include 'include/params.inc'
C
      INTEGER*4 IXMIN,IYMIN,IXMAX,IYMAX,IX,IY,ID,IG
      INTEGER*4 GUNIT,NBLOCK,ST,P,NSTRGS
      INTEGER*4 VRFY,NVRFY,NLEN,NLEN1
      INTEGER*4 THISX,THISY,LASTX,LASTY,TFORM
      INTEGER*4 I,J,K,P1,P2,P3,RTYPE,TLEN
      INTEGER*4 NF,SDIM,NUP,RECL,FUNIT
      INTEGER*2 CC,SX1,SY1,SX2,SY2
      INTEGER*2 TJST,TFONT,TLAY,PP,TCOL
      INTEGER*2 GPIDAT(40)
C      REAL XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
      DOUBLE PRECISION XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX
      REAL SCX,SCY,TSLT,ANGC,PI,TSCAL,AZIMUT
      REAL LEVEL,ORGX,ORGY,RDIM,ENDX,ENDY,R(32)
      DOUBLE PRECISION DXMIN,DYMIN,DXMAX,DYMAX,DOXX,DOYY
      DOUBLE PRECISION DD(32)
      LOGICAL OK,CXY,APOPEN,AFLASH,FORMOK,TEXTOK,STEND,DATAN
      LOGICAL SCAN,FPASS,SAME
      CHARACTER*80 BLOCK,FORM1,FILNM*(*),STRING
      CHARACTER CRTYP*3,ISTRNG*10,FORM*64
      CHARACTER*6 FORMS(32),TLNAM*4
      INTEGER*4 FIELD(30),SP
      CHARACTER CH
C
      EXTERNAL GETNUM,NLEN,NLEN1,VRFY,NVRFY,PI,AZIMUT
C
C     set record length for workfile
      SP=1
      RECL=24
      CALL OURSCR(FUNIT,RECL,OK)
C
C     set flag for first pass scan of limits
      SCAN=.TRUE.
      FPASS=.TRUE.
C
C     set text parameters
      NF=0
      TJST=1
      TFONT=1
      TLAY=10
      TCOL=7
      TSLT=0.0
      FORM1='(F10.0)'
C     assume angles provided in radians
      ANGC=1.0
C     set default spline form to ploygon
      TFORM=24+1
C
      NUP=0
      IXMIN=9999
      IXMAX=-9999
      IYMIN=9999
      IYMAX=-9999
      ISTRNG='0123456789'
      FORMOK=.FALSE.
      TEXTOK=.FALSE.
      DATAN=.FALSE.
C     text in MOSS is cm on paper,multiply by 10
      TSCAL=10.0
C
      LASTX=0
      LASTY=0
      THISX=0
      THISY=0
      NSTRGS=0
C
      APOPEN=.FALSE.
C
      CALL FINDU1(GUNIT,OK)
      IF ( .NOT.OK ) THEN
         CALL DEPRNT(668)
      END IF
      OPEN(UNIT=GUNIT,FILE=FILNM,ERR=991)
CIBM
C      LFU(GUNIT)=.TRUE.
CIBM
      REWIND(UNIT=GUNIT)
C
C     ensure limits are initialized well out
C     of normal ranges
      XMIN=+1E10
      XMAX=-1E10
      YMIN=+1E10
      YMAX=-1E10
      ZMIN=+1E10
      ZMAX=-1E10
C
      NBLOCK=0
C
 3    CONTINUE
      BLOCK='
     +                                                         '
      READ(UNIT=GUNIT,FMT='(A)',ERR=990,END=100)BLOCK
      NBLOCK=NBLOCK+1
C     tell the user what is happening every 50 blocks
      IF (MOD(NBLOCK,100).EQ.0) THEN
         STRING=' '
         WRITE(STRING,FMT='(A,I6,A)')'Total of',NBLOCK,' blocks read'
         CALL CPRINT(STRING)
      END IF
C     get the record type in character format
      CRTYP=BLOCK(1:3)
C     Record type must be pure numeric
      P1=NVRFY(CRTYP,ISTRNG)
D      WRITE(10,*) '[RDGENI] BLOCK= ',BLOCK
D      WRITE(10,*) '[RDGENI] P1= ',P1
      DO 620 I=1,NLEN(BLOCK)
          IF(BLOCK(I:I).NE.CHAR(32)) THEN
              IF(BLOCK(I:I).EQ.'0'.AND.DATAN.AND.
     +           BLOCK(I+1:I+1).NE.'.') THEN
                   DATAN=.FALSE.
              ENDIF
              GOTO 610
          ENDIF
 620  CONTINUE
610   IF (P1.EQ.0.OR.DATAN) THEN
         IF (FORMOK) THEN
C           read numeric data with current format
C           **********************************************
C           ***      READ REAL DATA FROM BLOCK         ***
C           **********************************************
C            READ(UNIT=BLOCK,FMT=FORM1,ERR=994)(R(I),I=1,NF)
            READ(UNIT=BLOCK,FMT=FORM1,ERR=994)(DD(I),I=1,NF)
            DO 777 I=1,NF
               R(I)=DD(I)
 777        CONTINUE
            IF (TEXTOK) THEN
C              ********************************************
C              ***        TEXT RECORD                   ***
C              ********************************************
C              next physical record contains text string
               READ(UNIT=GUNIT,FMT='(A)',ERR=990,END=100)BLOCK
               NBLOCK=NBLOCK+1
C              test for end of text data
               STEND=((R(1).EQ.ENDX.AND.R(2).EQ.ENDY) .OR.
     +                (R(3).EQ.ENDX.AND.R(4).EQ.ENDY))
               IF (.NOT.STEND) THEN
C                 convert base angle of text
                  R(4)=R(4)*ANGC
C                 write text to database
C                 offset by origin value
                  R(1)=R(1)+ORGX
                  R(2)=R(2)+ORGY
C                 remember to scale text height
                  R(3)=R(3)*TSCAL
C                 angle is AZIMUTH convert to conventional angle
                  R(4)=AZIMUT(R(4))
                  CALL DEWC85(R(1),R(2),R(3),R(3),R(4),TSLT,
     +                     TJST,TFONT,TCOL,TLAY,BLOCK,PP,OK)
               END IF
            ELSE
C              ********************************************
C              ***        SPLINE RECORD                 ***
C              ********************************************
C              must be normal string
               I=1
 88            CONTINUE
C              test for end of string delimiter values
               STEND=(R(I).EQ.ENDX .AND. R(I+1).EQ.ENDY)
               IF (STEND) THEN
C                 close the string
                  DATAN=.FALSE.
                  IF (NUP.GT.0) THEN
C                    write string to database
                     CALL DEWCF7(FUNIT,NUP,TFORM,TLAY,INT2(P),OK)
                  END IF
C                 clear point count for the string
                  NUP=0
               ELSE
C                 add point to string
                  NUP=NUP+1
C                 update limits of data
                  IF ( R(I)+ORGX.GT.0 ) THEN
                     XMIN=MIN(XMIN,DBLE(R(I)+ORGX))
                     XMAX=MAX(XMAX,DBLE(R(I)+ORGX))
                  END IF
                  IF ( R(I+1)+ORGY.GT.0 ) THEN
                     YMIN=MIN(YMIN,DBLE(R(I+1)+ORGY))
                     YMAX=MAX(YMAX,DBLE(R(I+1)+ORGY))
                  END IF
C
                  IF (SDIM.EQ.2) THEN
C                    2d string
                     WRITE(UNIT=FUNIT,REC=NUP)
     +               R(I)+ORGX,R(I+1)+ORGY,LEVEL,0.0,0.0,0.0
C                    update z limit if necessary
                     IF ( LEVEL.GT.0 ) THEN
                        ZMIN=MIN(ZMIN,DBLE(LEVEL))
                        ZMAX=MAX(ZMAX,DBLE(LEVEL))
                     END IF
                  ELSE IF (SDIM.EQ.3) THEN
C                    3d string
                     WRITE(UNIT=FUNIT,REC=NUP)
     +               R(I)+ORGX,R(I+1)+ORGY,R(I+2),0.0,0.0,0.0
C                    update z limit if necessary
                     IF ( R(I+2).GT.0 ) THEN
                        ZMIN=MIN(ZMIN,DBLE(R(I+2)))
                        ZMAX=MAX(ZMAX,DBLE(R(I+2)))
                     END IF
                  END IF
                  I=I+SDIM
                  IF ((I+SDIM-1).LE.NF) GOTO 88
               END IF
            END IF
         ELSE
C           ********************************************
C           ***        UNSUPPORTED DATA TYPE         ***
C           ********************************************
C           not a numeric field,must be a descriptor
C            WRITE(10,*)'Descriptor:',BLOCK(1:NLEN(BLOCK))
         END IF
      ELSE IF (P1.EQ.1) THEN
C        ********************************************
C        ***       RECORD HEADER                  ***
C        ********************************************
C        must be record header get the type
         READ(UNIT=BLOCK,FMT='(I3)',ERR=992)RTYPE
C        handle the particular record type
         IF (RTYPE.EQ.001) THEN
C           ********************************************
C           ***       FORMAT STATEMENT               ***
C           ********************************************
C           Format statement for the next record
            DATAN=.FALSE.
            P2=INDEX(BLOCK,'FORMAT')
            IF (P2.EQ.0) THEN
               WRITE(10,*)
     +         ' Error in format record - block number:',NBLOCK
               FORMOK=.FALSE.
            ELSE
               FORM=BLOCK(P2+6:NLEN(BLOCK))
               TEXTOK=(INDEX(FORM,'A').GT.0)
C              decode the format statement
               CALL DFORMT(FORM,FORMS,NF,ST)
               IF (ST.NE.0) THEN
C                 error in format
                  WRITE(10,*)' Error in format record - block number:',
     +            NBLOCK
                  FORMOK=.FALSE.
                  WRITE(10,*)' ** ERROR IN FORMAT ** BLOCK=',NBLOCK
               ELSE
C                 formats ok
                  FORM1='('
                  DO 77 I=1,NF
                     P1=NLEN(FORM1)+1
                     FORM1(P1:)=FORMS(I)
                     P1=NLEN(FORM1)+1
                     FORM1(P1:)=','
 77               CONTINUE
                  FORM1(P1:P1)=')'
                  FORMOK=.TRUE.
               END IF
            END IF
         ELSE IF (RTYPE.EQ.080) THEN
C           ********************************************
C           ***       SPLINE HEADER                  ***
C           ********************************************
C           string header
            DATAN=.TRUE.
            NSTRGS=NSTRGS+1
C           write out string label
            TLNAM=BLOCK(4:7)
            K=NLEN(TLNAM)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C     Kirk
C     this searches the current layer names.
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C           if not blank line check against existing names
            IF (K.GT.0) THEN
               J=0
 15            CONTINUE
               IF (J.LT.255) THEN
                  TLEN=NLEN(LNAME(J))
                  IF (TLEN.GT.0) THEN
C                    layer name is not blank.
                     IF (TLNAM(1:1).EQ.LNAME(J)(1:1)) THEN
C                       layer name already exists use it
                        TLAY=J
C                       set points mode if required
                        IF (TLNAM(1:1).EQ.'P') then
C                          set spline-points form
                           TFORM=24+0
                        ELSE
C                          set spline-polygon form
                           TFORM=24+1
                        END IF
                        GOTO 16
                     ELSE
C                       go test next layer
                        J=J+1
                        GOTO 15
                     END IF
                  ELSE
C                    no name attached to layer
C                    so attach this name
                     LNAME(J)=TLNAM
C                    use this layer number
                     TLAY=J
C                    set points mode if required
                     IF (TLNAM(1:1).EQ.'P') then
C                       set spline-points form
                        TFORM=24+0
                     ELSE
C                       set spline-polygon form
                        TFORM=24+1
                     END IF
                  END IF
               END IF
            END IF
C
 16         CONTINUE
C           find control parameters from string header
 
            CALL GETNUM(BLOCK(16:23),LEVEL,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
            CALL GETNUM(BLOCK(24:33),ORGX,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
            CALL GETNUM(BLOCK(34:43),ORGY,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
            CALL GETNUM(BLOCK(44:51),RDIM,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
C           save string dimension
            SDIM=RDIM
            CALL GETNUM(BLOCK(52:61),ENDX,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
            CALL GETNUM(BLOCK(62:71),ENDY,OK,P2,ST)
            IF (ST.NE.0) GOTO 993
C           string header complete
C           initialize string data pointer
            NUP=0
         ELSE IF (RTYPE.EQ.017) THEN
C           ********************************************
C           ***       ANGULAR UNITS                  ***
C           ********************************************
C           Angular units modifier
            DATAN=.FALSE.
            IF (BLOCK(4:7).EQ.'GRAD') THEN
C              angles in grads,convert to degrees
               ANGC=180.0/200.0
            ELSE IF (BLOCK(4:7).EQ.'RADI') THEN
C              angles in radians,convert to degrees
               ANGC=180.0/PI(1.0)
            ELSE IF (BLOCK(4:7).EQ.'DEGR') THEN
C              angles in degrees,no conversion
               ANGC=1.0
            ELSE
C              assume radians
               ANGC=180.0/PI(1.0)
            END IF
C
         ELSE IF (RTYPE.EQ.999) THEN
C           ********************************************
C           ***       END OF FILE                    ***
C           ********************************************
C           EOF marker
C            WRITE(*,*)' EOF detected'
            DATAN=.FALSE.
            GOTO 100
         ELSE
C           ********************************************
C           ***       UNKNOWN RECORD TYPE            ***
C           ********************************************
C           unknown record type
            DATAN=.FALSE.
            WRITE(10,*)' UNKNOWN RECORD TYPE:',RTYPE,' BLOCK=',NBLOCK
         END IF
      END IF
      GOTO 3
C
 100  CONTINUE
 
 200  CONTINUE
C
      WRITE(10,'(A,I5,A)')' Total of ',NBLOCK, ' blocks read'
      WRITE(10,'(A,I5,A)')' Total of ',NSTRGS, ' strings found'
      WRITE(10,*) 'Minimum:',XMIN,YMIN,ZMIN
      WRITE(10,*) 'Maximum:',XMAX,YMAX,ZMAX
      CLOSE(UNIT=GUNIT)
CIBM
C      LFU(GUNIT)=.FALSE.
CIBM
      CLOSE(UNIT=FUNIT)
CIBM
C      LFU(FUNIT)=.FALSE.
CIBM
C     reset display to correct window
      CALL DCPRNT(682)
C     set default work layer to 0
      CLAYER=0
C     move and zoom to view limits of terrain data
      IF(SAME(XMAX-XMIN,0.0)) GOTO 995
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C     set bottom left of paper at new position
      WPORGX=XMIN+DRWSIZ(1)*PAPTOW/2
      WPORGY=YMIN+DRWSIZ(2)*PAPTOW/2
 
C     set new paper limits
      CALL CHGP05()
C     regenerate display
      CALL ZOMEXT()
C     conversion completed
1000  CONTINUE
      CALL DCPRNT(683)
C
      RETURN
C
 990  WRITE (10,*) ' ERROR AT BLOCK ',NBLOCK,' ST=',ST
      WRITE (10,*) ' Reading Record From File'
      RETURN
C
 991  WRITE (10,*) ' ERROR OPENING GENIO FILE:',FILNM
      RETURN
C
 992  WRITE (10,*) ' ERROR AT BLOCK ',NBLOCK,' ST=',ST
      WRITE (10,*) ' Record Type Error'
      RETURN
C
 993  WRITE (10,*) ' ERROR AT BLOCK ',NBLOCK,' ST=',ST
      WRITE (10,*) ' Converting number'
      RETURN
C
 994  WRITE (10,*) ' ERROR AT BLOCK ',NBLOCK,' ST=',ST
      WRITE (10,*) ' Reading Real Data From Block:'
      WRITE (10,*) BLOCK
      RETURN
C
 995  CONTINUE
      CALL DEPRNT(684)
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 redgi0.ftn Daxcad revision 1.8
      SUBROUTINE REDGI0()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    Subroutine REDGI02 is the control routine
C2    for the READ GENIO function.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      INTEGER*4 TMEN,TCELL,ST
      LOGICAL OK,OK2
      CHARACTER*128 FILNM,CC*1
      EXTERNAL RDGENI
C
      CALL FNAME(FILNM,ST)
      IF ( ST.NE.0 ) THEN
         MEN=0
         RETURN
      END IF
C
C     go read the data file
      CALL RDGENI(FILNM)
      CCMD='Q'
C
      END
C
C     --------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 tparen.ftn Daxcad revision 1.8
      SUBROUTINE TPAREN(STRING,ST)
C     ============================
C
C1    vartype            C*(*)  I4
C1    iostatus             I    O
C
C2    Subroutine TPAREN test the passed character string
C2    STRING for complete matching parentheses pairs.
C2    ST is zero if the match is complete,non-zero if
C2    a mis-match exists.
C
      INTEGER*4 ST,P0,P1,P2,P3,P4,SP,STAK(32),NLEV,NPAR
      INTEGER*4 PAREN(4,32),PN,NP
      INTEGER*4 I,J,NLEN,NL
      LOGICAL SWAP
      CHARACTER*(*) STRING
C
      INTRINSIC INDEX
      EXTERNAL NLEN
C
      NL=NLEN(STRING)
      NLEV=0
      NPAR=0
      SP=0
      PN=0
      IF (NL.EQ.0) THEN
C        blank format statement
         ST=1
         RETURN
      END IF
C
      P0=1
      DO 50 I=1,NL
         IF (STRING(I:I).EQ.'(') THEN
C           found opening bracket
            NLEV=NLEV+1
            NP=NP+1
            SP=SP+1
            STAK(SP)=I
         ELSE IF (STRING(I:I).EQ.')') THEN
C           found closing bracket
            PN=PN+1
            PAREN(2,PN)=I
            PAREN(1,PN)=STAK(SP)
            PAREN(3,PN)=NLEV
            NLEV=NLEV-1
            SP=SP-1
         END IF
 50   CONTINUE
C
      IF (NLEV.NE.0) THEN
C        mis-matched parenthesis
         ST=1
         RETURN
      END IF
C
      ST=0
C
      END
C
C     ----------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 value.ftn Daxcad revision 1.8
      DOUBLE PRECISION FUNCTION VALUE(STRING)
C     -----------------------------
C1    VARTYPE                 C*(*)
C1    IOSTATS                   I
C
C2    This routine wil convert a string to double
C2
      CHARACTER*80 STRING,TEST,BUFF
      INTEGER*4 NLEN,I
      TEST='ED'
C
      IF(INDEX(STRING,'.').EQ.0) THEN
          DO 20 I=1,NLEN(STRING)
               IF(INDEX(TEST,STRING(I:I)).NE.0) THEN
                   WRITE(BUFF,'(3A)') STRING(:I-1),'.0',
     +             STRING(I:)
                   STRING=BUFF
                   GOTO 30
               ENDIF
20        CONTINUE
          STRING=STRING(:NLEN(STRING))//'.0'
      ENDIF
30    CONTINUE
 
      IF(NLEN(STRING).GT.0) THEN
 
           READ(UNIT=STRING,FMT='(G50.0)',ERR=10) VALUE
           RETURN
 
      ENDIF
10    CONTINUE
      VALUE = 0.0
      END
C
C     ---------------------------------------------------
C
