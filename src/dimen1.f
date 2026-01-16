C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 dimen1.f   */
C
      SUBROUTINE CLRDOP()
C     ===================
C
C1    vartype          none.
C1    iostatus
C
C2    CALL CLRTAL is a general routine to clear the  real variaBLES
C2    RTALLY.
C
      include 'include/dimendat.inc'
C     clear mode flags
      INTEGER*2 I
      DO 60 I=1,20
         DOPFLG(I)=.FALSE.
 60   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 clrtal.ftn Daxcad revision 1.8
      SUBROUTINE CLRTAL()
C     ===================
C
C1    vartype          none.
C1    iostatus
C
C2    CALL CLRTAL is a general routine to clear the  real variaBLES
C2    RTALLY.
C
      include 'include/dimendat.inc'
      INTEGER*2 I
C
      DO 60 I=1,10
         RTALLY(I)=0
 60   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dima00.ftn Daxcad revision 1.8
      SUBROUTINE DIMA00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    DIMENSION ANGULAR function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL DIMA01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     enter the dim angular function
      CALL DIMA01()
C     ensure  option for angular is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dima01.ftn Daxcad revision 1.8
      SUBROUTINE DIMA01()
C     ===================
C1    no arguments required
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
C
      INTEGER TMEN,TCELL,I
C
      EXTERNAL GTMCLO,CLRPEW,GTCLRM
      EXTERNAL GTMCWT,MNLDM3,GTMCWI,DIMA02
C
      TMEN=MEN
      TCELL=CELLN
C
C     clear mode flags
      CALL CLRDOP()
C     clear the record counters.
      CALL CLRTAL()
C     initialize ANGULAR option menu
      CALL MNLDM3()
C     enter the DIM ANGULAR routine
      CALL DIMA02()
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dima02.ftn Daxcad revision 1.8
      SUBROUTINE DIMA02()
C     ===================
C1       no arguments passed
C
C2    DIMA02 gathers the neccesary dimension point info
C2    including dime line and text position. This then
C2    passed to main dimension generating routine which
C2    is intended as general dimen creation routine.
C
      include   'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/entity.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,
     +     PDAT(2,4),LDAT1(6),LDAT2(6),DCR,DISTXY,PI
      INTEGER*2 MBUFF(1:13)
      INTEGER*2 P,ENT1,ENT2,I,ENT
      LOGICAL OK
      EXTERNAL DIML10,DIMA03
C
      CALL FILL()
      DO 5 I=1,13
         MBUFF(I)=IMBUFF(I)
 5    CONTINUE
C     set the Angular Dimension Flag
      SPCSYM=3
 10   CONTINUE
C     go get dimension reference points
      CALL DIMA10(PDAT,LDAT1,LDAT2,X4,Y4,.FALSE.,OK)
C     test for abort
      IF (.NOT.OK) RETURN
C     go generate dimension data
      CALL DIMA03(PDAT,LDAT1,LDAT2,X4,Y4)
C     draw the dimension on screen
      DO 6 I=1,13
         IMBUFF(I)=MBUFF(I)
 6    CONTINUE
C     store complete dimension in database
      CALL DEWDIM(P,ADIMN,OK)
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C
      ENT=ADIMN
      CALL ALLDRW(ENT,P)
C
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C      CALL DRWDIM()
C     clear any flags from screen
      CALL UNFLAG(.TRUE.)
C     dimension should be complete
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dima10.ftn Daxcad revision 1.8
      SUBROUTINE DIMA10(PDAT,LDAT1,LDAT2,X4,Y4,MEASUR,OK1)
C     ====================================================
C1    i/o stat           O     O     O    O  O  I     O
C1    vartyp            R(2,4) R(6)   R(6) R  R  L     L
C2    Subroutine DIMA10 obtains from the operator
C2    data defining the angular dimension to be
C2    constructed.The hits obtained are returned
C2    in PDAT,line data returned in LDAT1,LDAT2
C2.   entity types in ENT1,ENT2.(usually lines)
C2    MEASUR indicates that routine is used by
C2    Measure Angle so no need for dimension point.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/entity.inc'
C
      REAL XC,YC,X,Y,X1,Y1,X4,Y4,C
      REAL LDAT1(6),LDAT2(6),PDAT(2,4)
C
      INTEGER*4 I
      INTEGER*2 MIP1,MIP2,ENT1,ENT2,FMIPOS
C
      LOGICAL OK,OK1,CHANGE,FOUND,OPTION,QUIT,MEASUR,FIRST
C
      EXTERNAL FINDP0,DIML11,WO2SC,CROSS,FINDL0,DER500
      EXTERNAL UNFLAG,ZSFLAG,INTLAH
C
      OK1=.FALSE.
      FMIPOS=1
      FIRST=.FALSE.
C
 10   CONTINUE
      IF (DOPFLG(1)) THEN
C        point to point mode currently selected
 101     CONTINUE
C        go get centre point
         CALL FINDP0(92,PDAT(1,1),PDAT(2,1),OPTION,QUIT)
         IF (QUIT) RETURN
         IF (OPTION) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 101
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) GOTO 10
            GOTO 101
         END IF
C        Show the hit point on screen
         CALL WO2SC(PDAT(1,1),PDAT(2,1),RWORK1(81),RWORK1(82))
         CALL CROSS(RWORK1(81),RWORK1(82))
C
 102     CONTINUE
C        go get first point
         CALL FINDP0(93,PDAT(1,2),PDAT(2,2),OPTION,QUIT)
         IF (QUIT) RETURN
         IF (OPTION) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 102
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
C              clear flag
               CALL CROSS(RWORK1(81),RWORK1(82))
               GOTO 10
            END IF
            GOTO 102
         END IF
C        trap zero length dimension
         IF ((PDAT(1,1).EQ.PDAT(1,2)).AND.
     +       (PDAT(2,1).EQ.PDAT(2,2))) THEN
            CALL DEPRNT(94)
            GOTO 102
         END IF
C        Show the hit point on screen
         CALL WO2SC(PDAT(1,2),PDAT(2,2),RWORK1(83),RWORK1(84))
         CALL CROSS(RWORK1(83),RWORK1(84))
C
 103     CONTINUE
C        go get second point
         CALL FINDP0(95,PDAT(1,3),PDAT(2,3),OPTION,QUIT)
         IF (QUIT) RETURN
         IF (OPTION) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 103
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
C              clear flags
               CALL CROSS(RWORK1(81),RWORK1(82))
               CALL CROSS(RWORK1(83),RWORK1(84))
               GOTO 10
            END IF
            GOTO 103
         END IF
C
C        trap zero length dimension
         IF ( (PDAT(1,1).EQ.PDAT(1,3).AND.
     +         PDAT(2,1).EQ.PDAT(2,3)).OR.
     1        (PDAT(1,2).EQ.PDAT(1,3).AND.
     2         PDAT(2,2).EQ.PDAT(2,3))       ) THEN
            CALL DEPRNT(94)
            GOTO 103
         END IF
C        Show the hit point on screen
         CALL WO2SC(PDAT(1,3),PDAT(2,3),RWORK1(85),RWORK1(86))
         CALL CROSS(RWORK1(85),RWORK1(86))
C
      ELSE IF (DOPFLG(2)) THEN
C        between line mode currently selected
 201     CONTINUE
C        go get line to dimension
         CALL FINDL0(96,PDAT(1,2),PDAT(2,2),MIP1,OPTION,QUIT)
         IF (QUIT) RETURN
         IF (OPTION) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 201
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
C              clear flags
               CALL UNFLAG(.TRUE.)
               GOTO 10
            END IF
            GOTO 201
         END IF
 203     CONTINUE
C        go get second line
         CALL FINDL0(97,PDAT(1,3),PDAT(2,3),MIP2,OPTION,QUIT)
         IF (QUIT) RETURN
         IF (OPTION) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 203
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
C              clear flags
               CALL UNFLAG(.TRUE.)
               GOTO 10
            END IF
            GOTO 203
         END IF
C
C        test for same line twice
         IF (MIP1.EQ.MIP2) THEN
C           warn him!
            CALL DEPRNT(98)
            CALL ZSFLAG(.TRUE.,OK)
            GOTO 203
         END IF
C        must find intersection of the lines
C
         CALL DER500(MIP1,OK)
         ENT1=IMBUFF(2)
         IF (ENT1.EQ.CENLIN) THEN
C           We got a centre line ... disguise it as a line.
            CALL CLNEND(PDAT(1,2),PDAT(2,2),X1,Y1)
            LDAT1(1) = RDBUFF(1)
            LDAT1(2) = RDBUFF(2)
            LDAT1(3) = 0.0
            LDAT1(4) = X1
            LDAT1(5) = Y1
            LDAT1(6) = 0.0
            ENT1 = LINE
         ELSE
C           Pass the data buffer as is.
            DO 300 I=1,6
               LDAT1(I)=RDBUFF(I)
 300        CONTINUE
         ENDIF
C
         CALL DER500(MIP2,OK)
         ENT2=IMBUFF(2)
         IF (ENT2.EQ.CENLIN) THEN
C           We got a centre line ... disguise it as a line.
            CALL CLNEND(PDAT(1,3),PDAT(2,3),X1,Y1)
            LDAT2(1) = RDBUFF(1)
            LDAT2(2) = RDBUFF(2)
            LDAT2(3) = 0.0
            LDAT2(4) = X1
            LDAT2(5) = Y1
            LDAT2(6) = 0.0
            ENT2 = LINE
         ELSE
C           Pass the data buffer as is.
            DO 301 I=1,6
               LDAT2(I)=RDBUFF(I)
 301        CONTINUE
         ENDIF
C
C        go find the intersection point
         CALL INTLAH(ENT1,LDAT1,ENT2,LDAT2,0.0,0.0,
     +                      PDAT(1,1),PDAT(2,1),OK)
         IF ( .NOT. OK ) THEN
C           no intersection found so must be parallel
C           warn him!
            CALL DEPRNT(317)
            CALL ZSFLAG(.TRUE.,OK)
            GOTO 203
         END IF
C        show the centre point
         CALL WO2SC(PDAT(1,1),PDAT(2,1),RWORK1(81),RWORK1(82))
         CALL CROSS(RWORK1(81),RWORK1(82))
      END IF
C
      IF ( .NOT. MEASUR ) THEN
 302     CONTINUE
C        go get third point so load points menu
         CALL DIML12(ADIMN,PDAT(1,4),PDAT(2,4),X4,Y4,CHANGE)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 302
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) GOTO 10
         END IF
C        Test for quit condition
         IF ((MEN.EQ.2).OR.(CCMD.EQ.'Q'.OR.CCMD.EQ.'q')) THEN
            RETURN
         END IF
C        clear flags from previous op
         CALL CROSS(RWORK1(81),RWORK1(82))
         IF (OPFLAG(1)) THEN
            CALL CROSS(RWORK1(83),RWORK1(84))
            CALL CROSS(RWORK1(85),RWORK1(86))
         END IF
C
      END IF
C
      OK1=.TRUE.
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dima30.ftn Daxcad revision 1.8
      SUBROUTINE DIMA30(ANGLE,DMNSEC)
C     ===============================
C
C1    vartype             R    C*(*)
C1    iostatus            I      O
C
C2    Subroutine DIMA30 converts the real number
C2    ANGLE into a character string equivalent
C2    of degrees,minutes and seconds.
C
      include 'include/dimendat.inc'
C
      REAL ANGLE,MINSEC,ABANG
      INTEGER  IDEGS,MINS,SECS,INT,NINT,I,NLEN1
      CHARACTER*(*) DMNSEC,TEMP*80,TEMP1*80
      INTRINSIC INT,NINT,CHAR
      EXTERNAL CRUNCH,NLEN1
C
C     set absolute value of angle.
      ABANG=ABS(ANGLE)
C     find number of degrees
      IDEGS=INT(ABANG)
      MINSEC=ABANG-IDEGS
      MINS=INT(MINSEC*60)
      SECS=NINT(MINSEC*3600-60*MINS)
      IF ( SECS. EQ. 60 ) THEN
C        set to zero and increment Mins by 1
         SECS=0
         MINS=MINS+1
         IF ( MINS .EQ. 60 ) THEN
C           can update degrees count by 1
            MINS=0
            IDEGS=IDEGS+1
         END IF
      END IF
C     convert to character string
C     CHAR(129) is degree character
      IF ( SPCSYM.EQ.5 ) THEN
C        wants degrees , minutes & seconds.
         WRITE(UNIT=TEMP,FMT='(I3,A,I2,A,I2,A)')IDEGS,CHAR(129),
     +                                        MINS,'''',SECS,'"'
      ELSE IF ( SPCSYM.EQ.4  ) THEN
C        wants degrees , minutes only
         WRITE(UNIT=TEMP,FMT='(I3,A,I2,A)')IDEGS,CHAR(129),
     +                                        MINS,''''
      ELSE
C        wants degrees only.
         WRITE(UNIT=TEMP,FMT='(I3,A)')IDEGS,CHAR(129)
      END IF
C
      IF ( ANGLE .LT. 0 ) THEN
C        add the '-' sign to character sign
         TEMP1='-'//TEMP(1:NLEN1(TEMP))
         TEMP=TEMP1
      END IF
C
      CALL CRUNCH(TEMP)
      DMNSEC=TEMP
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd00.ftn Daxcad revision 1.8
      SUBROUTINE DIMD00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    DIMENSION DIAMETER function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL DIMD01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     enter the dim diameter function
      CALL DIMD01()
C     ensure option for diam is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd01.ftn Daxcad revision 1.8
      SUBROUTINE DIMD01()
C     ===================
C
C1    no arguments required
C
C2    Subroutine DIMD01 sets all conditions to
C2    default status before starting into the
C2    dimension diametral function proper.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
      EXTERNAL MNLDM2,GTCLRM
      EXTERNAL GTHFMC,GTMCWI,MNLPTS,DIMR02,CLRTAL
C     clear mode flags
      CALL CLRDOP()
C     clear the record counters.
      CALL CLRTAL()
C     initialize DIAMETER option menu
      CALL MNLDM4()
C     need the point modes as well
      CALL MNLPTS()
C     enter the DIM DIAM routine
      CALL DIMR02(.TRUE.)
C     disable the point modes
      CALL MNUPTS()
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd13.ftn Daxcad revision 1.8
      SUBROUTINE DIMD13(DIMD,PREC,DIMT)
C     =================================
C
C1    vartype             R   I*4  C*(*)
C1    iostatus            I    I   O
C
C2    Subroutine DIMD13 converts the dimension DIMD
C2    into a character string DIMT with decimal
C2    precision PREC
C2    ANGD passed true if angular dimension type
C
      include 'include/dimendat.inc'
      REAL DIMD
      INTEGER*4 PREC,P,NLEN1,P1
      CHARACTER DIMT(10)*(*),FORM*12,CHAR,TEMP*40
      INTRINSIC NINT,CHAR
C
      EXTERNAL CRUNCH,NLEN1
C
      FORM='(F10.2,A)'
C
      IF (PREC.NE.0) THEN
         FORM(6:6)=CHAR(48+PREC)
C           normal scalar value
            WRITE(UNIT=DIMT(1),FMT=FORM) DIMD,' '
C           check to see if less than unity for leading zero
            IF ( DIMD .LT . 1 .AND. DIMD .GT. -1 ) THEN
C             but if ascii and imperial then supress leading zero
              IF ( ANSI .AND. (.NOT. METRIC) ) THEN
                P=INDEX(DIMT(1),'0')
                P1=INDEX(DIMT(1),'.')
                IF ( P .LT. P1 ) THEN
                  TEMP=DIMT(1)(1:P-1)//DIMT(1)(P+1:NLEN1(DIMT(1)))
                  DIMT(1)=TEMP
                END IF
              END IF
            END IF
      ELSE
         IF (DIMD.LE.1 .AND. DIMD.GE.-1) THEN
             WRITE(UNIT=DIMT(1),FMT='(F4.2)') DIMD
         ELSE
C            normal scalar
             WRITE(UNIT=DIMT(1),FMT='(I7)') NINT(DIMD)
         END IF
      END IF
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd14.ftn Daxcad revision 1.8
      SUBROUTINE DIMD14(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     =================================================
C
C1    vartype             R   I4   R   L   C*(*)
C1    iostatus            I   I    I   I     O
C
C2    Subroutine DIML14 converts DIMD into character
C2    string DIMT with precision PREC,and adds the
C2    lowest set tolerance value TOL.
C2    ANGD passed true if angular dim type
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      REAL DIMD,STRVAL(4,10),BLWD,BLHT,BTOL
      INTEGER*4 NLEN1,I,P,NSTR
      CHARACTER DIMT(10)*(*),TEMP*40,TEMP2*40,CHAR
C
      INTRINSIC CHAR,ABS
C
      EXTERNAL NLEN1,CRUNCH,DIMD13
C
C     calculate the basic dimension.
      CALL DIMD13(DIMD,PREC,TEMP)
C     compress the basic dimension
      CALL CRUNCH(TEMP)
C     Test to see if the basic dimension needs surpressed
      IF ( ANSI .AND. ( .NOT. METRIC ) ) GOTO 5
C     Supress trailing zeroes  if real value given
      IF (INDEX(TEMP,'.') .GT. 0 ) THEN
C        real number so try and supress
         CALL ZSUPP2(TEMP)
      END IF
C     check for period at end of string so redundant.
      P=INDEX(TEMP,'.')
      IF (P .EQ. NLEN1(TEMP) ) THEN
C     Reset string to exclude period
        TEMP2=TEMP(1:P-1)
        TEMP=TEMP2
      END IF
C
 5    CONTINUE
      IF ( SPCSYM.EQ.1 ) THEN
C        diametral sign requested
         DIMT(1)=CHAR(130)//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM.EQ.2 ) THEN
C        diametral sign requested
         DIMT(1)='R'//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM.EQ.3 ) THEN
C        angular dimension might be required.
         DIMT(1)=TEMP(1:NLEN1(TEMP))//CHAR(129)
      ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5 ) THEN
C         degs,mins,seconds asked for
          CALL DIMA30(DIMD,DIMT(1))
      ELSE
          DIMT(1)=TEMP
      END IF
C     go work out lowest tolerance value.
      IF ( LTOL .LE. UTOL ) THEN
         BTOL=LTOL
      ELSE
         BTOL=UTOL
      END IF
C
      CALL DIMD13(ABS(BTOL),PREC,TEMP)
      IF ( SPCSYM.EQ.3 ) THEN
C        angular dimension required.
         TEMP2=TEMP
         TEMP=TEMP2(1:NLEN1(TEMP2))//CHAR(129)
      ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5 ) THEN
C        degs,mins,seconds asked for
         CALL DIMA30(ABS(BTOL),TEMP)
      END IF
C     produce dimension text.
      DIMT(2)=CHAR(128)//TEMP(1:NLEN1(TEMP))
      CALL CRUNCH(DIMT(2))
C     Set the text string attributes
          STRVAL(3,1)=0
          STRVAL(4,1)=0
          STRVAL(3,2)=NLEN1(DIMT(1))*DTWDT
          STRVAL(1,1)=DTHGT
          STRVAL(2,1)=DTWDT
          BLWD=NLEN1(DIMT(1))*DTWDT
          IF ( DIN ) THEN
              STRVAL(4,2)=0.2*DTHGT
              STRVAL(1,2)=DTHGT*0.6
              STRVAL(2,2)=DTWDT*0.6
              BLWD=BLWD+NLEN1(DIMT(2))*0.6*DTWDT
          ELSE
              STRVAL(4,2)=0
              STRVAL(1,2)=DTHGT
              STRVAL(2,2)=DTWDT
              BLWD=BLWD+NLEN1(DIMT(2))*DTWDT
          END IF
          BLHT=DTHGT
          NSTR=2
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd15.ftn Daxcad revision 1.8
      SUBROUTINE DIMD15(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     ==================================================
C
C1    vartype             R   I4   R   L   C(3)*(*)
C1    iostatus            I   I    I   I     O
C
C2    Subroutine DIML15 converts Dimension value DIMD into character
C2    string DIMT. This usess the lower and upper tolerances , LTOL
C2    & UTOL ,to produce tolerance limits.The Basic dimension is held
C2    in DIMT(1) The lower Tolerance value is held
C2    in DIMT(2) , the upper value in DIMT(3).
C2    STRVAL(1,I) holds the dimn text height
C2    STRVAL(2,I) holds the dimn text width
C2    STRVAL(1,I) holds the dimn text x local origin
C2    STRVAL(1,I) holds the dimn text y local origin
C2    BLWD & BLHT contain the Dimn text block extents.
C
C2    Note:- Local origin is bottom left for text and block.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      REAL DIMD,STRVAL(4,10),BLWD,BLHT
      INTEGER*4 NLEN1,I,GTLEN,P,NSTR
      CHARACTER DIMT(10)*(*),TEMP*40,TEMP2*40,CHAR,TEMP3*40
C
      INTRINSIC CHAR
C
      EXTERNAL NLEN1,CRUNCH,DIMD13
C
      I=1
C     Create basic dimension
      CALL DIMD13(DIMD,PREC,TEMP)
C     compress the basic dimension
      CALL CRUNCH(TEMP)
C     check for trailing zeroes.
C     if ANSI and Imperial then keep zeroes
      IF ( (ANSI) .AND. ( .NOT. METRIC ) ) GOTO 15
C     Supress trailing zeroes  if real value given
      IF (INDEX(TEMP,'.') .GT. 0 ) THEN
C        real number so try and supress
         CALL ZSUPP2(TEMP)
      END IF
C     check for period at end of string so redundant.
      P=INDEX(TEMP,'.')
      IF (P .EQ. NLEN1(TEMP) ) THEN
C     Reset string to exclude period
        TEMP2=TEMP(1:P-1)
        TEMP=TEMP2
      END IF
C
 15   CONTINUE
      IF ( SPCSYM.EQ.1 ) THEN
C        diametral sign requested
         DIMT(1)=CHAR(130)//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM.EQ.2 ) THEN
C        radial sign requested
         DIMT(1)='R'//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM.EQ.3 ) THEN
C        angular dimension required.
         DIMT(1)=TEMP(1:NLEN1(TEMP))//CHAR(129)
      ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5) THEN
C        degs,mins,seconds asked for
         CALL DIMA30(DIMD,DIMT(1))
      ELSE
         DIMT(1)=TEMP
      END IF
C     Create upper tolerance text
      CALL DIMD13(UTOL,PREC,DIMT(3))
C     Create lower tolerance text
      CALL DIMD13(LTOL,PREC,DIMT(2))
C     Set tolerance value if zero.
      IF ( (LTOL.EQ.0.AND..NOT.ANSI)
     +   .OR.(LTOL.EQ.0.AND.ANSI.AND.METRIC)  )  THEN
C     Set Tol to single zero.
           DIMT(2)='0'
      END IF
C
C     Set tolerance value if zero.
      IF ( (UTOL.EQ.0.AND..NOT.ANSI)
     +   .OR.(UTOL.EQ.0.AND.ANSI.AND.METRIC)  )  THEN
 
C     Set Tol to single zero.
           DIMT(3)='0'
      END IF
C     Write out values temp.
      IF ( LTOL .GT. 0 .OR. LTOL .LT. 0) THEN
C     add the '+' sign to character sign
C     if less than 0  then - sign will already be assigned
C        Check to see if tolerance needs Angular sign
         IF ( SPCSYM.EQ.3 ) THEN
C           angular dimension required.
            TEMP2=DIMT(2)
            DIMT(2)=TEMP2(1:NLEN1(TEMP2))//CHAR(129)
         ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5 ) THEN
C           degs,mins,seconds asked for
            CALL DIMA30(LTOL,DIMT(2))
         END IF
         IF ( LTOL .GT. 0 ) THEN
C            add the '+' sign to character sign
C            if less than 0  then - sign will already be assigned
             TEMP3='+'//DIMT(2)(1:NLEN1(DIMT(2)))
             DIMT(2)=TEMP3
         END IF
      END IF
C
      IF ( UTOL .GT. 0 .OR. UTOL .LT. 0) THEN
C     add the '+' sign to character sign
C     if less than 0  then - sign will already be assigned
C        Check to see if tolerance needs Angular sign
         IF ( SPCSYM.EQ.3 ) THEN
C           angular dimension required.
            TEMP2=DIMT(3)
            DIMT(3)=TEMP2(1:NLEN1(TEMP2))//CHAR(129)
         ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5 ) THEN
C           degs,mins,seconds asked for
            CALL DIMA30(UTOL,DIMT(3))
         END IF
         IF ( UTOL .GT. 0 ) THEN
C           add the '+' sign to character sign
C           if less than 0  then - sign will already be assigned
            TEMP3='+'//DIMT(3)(1:NLEN1(DIMT(3)))
            DIMT(3)=TEMP3
         END IF
      END IF
 
C       compress the basic dimension
C        CALL CRUNCH(DIMT(1))
c       compress the upper tolerance
        CALL CRUNCH(DIMT(2))
        IF ( LTOL .EQ. 0 ) THEN
C       compress character string so no blanks
            TEMP3=' '//DIMT(2)(1:NLEN1(DIMT(2)))
            DIMT(2)=TEMP3
        END IF
C       compress the lower tolerance
        CALL CRUNCH(DIMT(3))
        IF ( UTOL .EQ. 0 ) THEN
            TEMP3=' '//DIMT(3)(1:NLEN1(DIMT(3)))
            DIMT(3)=TEMP3
        END IF
 
C     now set the control data
      IF ( BS ) THEN
C
         DO 965 I=1,3
           STRVAL(1,I)=DTHGT
           STRVAL(2,I)=DTWDT
 965      CONTINUE
C
          STRVAL(3,1)=0
          STRVAL(4,1)=0
          STRVAL(3,2)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,2)=0
          STRVAL(3,3)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,3)=1.5*DTHGT
C         Which is the greater length string
          GTLEN=MAX( NLEN1(DIMT(2)),NLEN1(DIMT(3)) )
          BLWD=STRVAL(3,2)+GTLEN*DTWDT
          BLHT=2.5*DTHGT
          NSTR=3
C
      ELSE IF ( ANSI ) THEN
C
         DO 195 I=1,3
           STRVAL(1,I)=DTHGT
           STRVAL(2,I)=DTWDT
 195     CONTINUE
C
          STRVAL(3,1)=0
          STRVAL(4,1)=0.8*DTHGT
          STRVAL(3,2)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,2)=0
          STRVAL(3,3)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,3)=1.6*DTHGT
C         Which is the greater length string
          GTLEN=MAX( NLEN1(DIMT(2)),NLEN1(DIMT(3)) )
          BLWD=STRVAL(3,2)+GTLEN*DTWDT
          BLHT=2.5*DTHGT
          NSTR=3
C
      ELSE IF ( DIN ) THEN
C
           STRVAL(1,1)=DTHGT
           STRVAL(2,1)=DTWDT
         DO 295 I=2,3
           STRVAL(1,I)=DTHGT*.6
           STRVAL(2,I)=DTWDT*.6
 295     CONTINUE
C
          STRVAL(3,1)=0
          STRVAL(4,1)=0.4*DTHGT
          STRVAL(3,2)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,2)=0
          STRVAL(3,3)=NLEN1(DIMT(1))*DTWDT
          STRVAL(4,3)=1.2*DTHGT
C         Which is the greater length string
          GTLEN=MAX( NLEN1(DIMT(2)),NLEN1(DIMT(3)) )
          BLWD=STRVAL(3,2)+(GTLEN*DTWDT*0.6)
          BLHT=1.8*DTHGT
          NSTR=3
C
      END IF
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd16.ftn Daxcad revision 1.8
      SUBROUTINE DIMD16(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     ==================================================
C
C1    vartype             R   I4   R   L   C(3)*(*)
C1    iostatus            I   I    I   I     O
C
C2    Subroutine DIML16 converts Dimension value DIMD into character
C2    string DIMT. This applies the lower and upper tolerances , LTOL
C2    & UTOL ,to produce 2 basic dimensions. The lower Value is held
C2    in DIMT(1) , the upper value in DIMT(2).
C2    STRVAL(1,I) holds the dimn text height
C2    STRVAL(2,I) holds the dimn text width
C2    STRVAL(1,I) holds the dimn text x local origin
C2    STRVAL(1,I) holds the dimn text y local origin
C2    BLWD & BLHT contain the Dimn text block extents.
C
C2    Note:- Local origin is bottom left for text and block.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      REAL DIMD,STRVAL(4,10),BLWD,BLHT
      INTEGER*4 NLEN1,I,NSTR
      CHARACTER DIMT(10)*(*),TEMP*40,TEMP2*40,CHAR,TEMP3*40
C
      INTRINSIC CHAR
C
      EXTERNAL NLEN1,CRUNCH,DIMD13
C
C     Create lower basic dimension
      CALL DIMD13(DIMD+LTOL,PREC,DIMT(1))
C     Create upper basic dimension
      CALL DIMD13(DIMD+UTOL,PREC,DIMT(2))
C     Set 3rd element array
      IF (SPCSYM.EQ.1) THEN
         DIMT(3)=CHAR(130)
      ELSE IF ( SPCSYM.EQ.2 ) THEN
         DIMT(3)='R'
      END IF
C
      CALL CRUNCH(DIMT(1))
      CALL CRUNCH(DIMT(2))
C     set up the control data
      DO 95 I=1,3
        STRVAL(1,I)=DTHGT
        STRVAL(2,I)=DTWDT
 95   CONTINUE
C
      IF ( SPCSYM.EQ.3 ) THEN
C        angular dimension required.
         TEMP=DIMT(1)
         DIMT(1)=TEMP(1:NLEN1(TEMP))//CHAR(129)
         TEMP=DIMT(2)
         DIMT(2)=TEMP(1:NLEN1(TEMP))//CHAR(129)
      ELSE IF ( SPCSYM.EQ.4 .OR. SPCSYM.EQ.5) THEN
C            degs,mins,seconds asked for
             CALL DIMA30(DIMD+LTOL,DIMT(1))
             CALL DIMA30(DIMD+UTOL,DIMT(2))
      END IF
C
C
      IF ( SPCSYM.EQ.1 .OR. SPCSYM.EQ.2 ) THEN
C        set 3 strings cos of diameter/radial sign
         STRVAL(3,1)=1.5*DTWDT
         STRVAL(4,1)=0
         STRVAL(3,2)=1.5*DTWDT
         STRVAL(4,2)=1.5*DTHGT
         STRVAL(3,3)=0
         STRVAL(4,3)=0.75*DTHGT
C
         BLWD=NLEN1(DIMT(1))*DTWDT+DTWDT*1.5
         BLHT=2.5*DTHGT
         CALL CRUNCH(DIMT(3))
C
         NSTR=3
      ELSE
C        set only 2 strings
         STRVAL(3,1)=0
         STRVAL(4,1)=0
         STRVAL(3,2)=0
         STRVAL(4,2)=1.5*DTHGT
C
         BLWD=NLEN1(DIMT(1))*DTWDT
         BLHT=2.5*DTHGT
C
         NSTR=2
      END IF
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimd17.ftn Daxcad revision 1.8
      SUBROUTINE DIMD17(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     ==================================================
C
C1    vartype             R   I4   R   L   C(3)*(*)
C1    iostatus            I   I    I   I     O
C
C2    Subroutine DIML17 converts Dimension value DIMD into character
C2    string DIMT. This prefixes the string with character string input
C2    from keyboard.
C2    STRVAL(1,I) holds the dimn text height
C2    STRVAL(2,I) holds the dimn text width
C2    STRVAL(1,I) holds the dimn text x local origin
C2    STRVAL(1,I) holds the dimn text y local origin
C2    BLWD & BLHT contain the Dimn text block extents.
C
C2    Note:- Local origin is bottom left for text and block.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      REAL DIMD,STRVAL(4,10),BLWD,BLHT
      INTEGER*4 NLEN1,I,NSTR
      INTEGER*2 NLEN2,NCHAR
      CHARACTER DIMT(10)*(*),TEMP*40,TEMP2*40,CHAR,INPT*80
C
      INTRINSIC CHAR
C
      EXTERNAL NLEN1,CRUNCH,DIMD13,NLEN2
C
C     Create basic dimension
      CALL DIMD13(DIMD,PREC,TEMP)
C     crunch up dimension
      CALL CRUNCH(TEMP)
C     blank of input string
      CALL BCHAR(INPT)
 55   CONTINUE
      CALL DCPRNT(39)
C     get string from user and hold in INPT
      CALL DPRMXP(90,INPT)
C     check the string is valid.
      NCHAR=NLEN2(INPT)
      IF ( NCHAR .GT. LEN(INPT) ) NCHAR=LEN(INPT)
      IF ( NCHAR .EQ. 1 .AND .INPT(1:1) .EQ. ' ' ) INPT='    '
C
      IF ( SPCSYM.EQ.1 ) THEN
C        diametral sign requested
         DIMT(1)=CHAR(130)//' '//TEMP(1:NLEN1(TEMP))//
     +           INPT(1:NLEN1(INPT))
      ELSE IF ( SPCSYM.EQ.2 ) THEN
C        diametral sign requested
         DIMT(1)='R'//' '//TEMP(1:NLEN1(TEMP))//
     +           INPT(1:NLEN1(INPT))
      ELSE IF (SPCSYM.GE.2 .AND. SPCSYM .LE. 5 ) THEN
C        add angular dim sign & marry the two text strings
         DIMT(1)=TEMP(1:NLEN1(TEMP))//CHAR(129)//INPT(1:NLEN1(INPT))
      ELSE
C        marry the two text strings
         DIMT(1)=TEMP(1:NLEN1(TEMP))//INPT(1:NLEN1(INPT))
      END IF
C     store the text string local origin.
      STRVAL(3,1)=0
      STRVAL(4,1)=0
C     store text string height & width
      STRVAL(1,1)=DTHGT
      STRVAL(2,1)=DTWDT
C     store text block overall height & width
      BLWD=NLEN1(DIMT(1))*DTWDT
      BLHT=DTHGT
C     set number of text strings created
      NSTR=1
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml00.ftn Daxcad revision 1.8
      SUBROUTINE DIML00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    DIMENSION LINEAR function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,DIML01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DIM LINE routine
      CALL DIML01()
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml01.ftn Daxcad revision 1.8
      SUBROUTINE DIML01()
C     ===================
C1    no arguments required
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
      EXTERNAL GTMCLO,CLRPEW,MNLDM1,GTCLRM
      EXTERNAL GTHFMC,GTMCWI,DIML02
C
C
C     clear mode flags
      CALL CLRDOP()
C     initialize LINEAR option menu
      CALL MNLDM1()
C     enter the DIM LINE routine
      CALL DIML02()
C     clear option menu
      CALL GTCLRM(3)
C     clear the error and prompt windows
      CALL CLRPEW
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml02.ftn Daxcad revision 1.8
      SUBROUTINE DIML02()
C     ===================
C1    no arguments passed
C
C2    DIML02 gathers the neccesary dimension point info
C2    including dime line and text position. This then
C2    passed to main dimension generating routine which
C2    is intended as general dimen creation routine.
C
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/entity.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,M(3,3)
C
      INTEGER*2 P,I,ENT
      INTEGER*2 MBUFF(1:13)
C
      LOGICAL OK
C
      EXTERNAL DIML10,DIML03
C
C     set special symbol flag to default of zero , no symbol.
      SPCSYM=0
      CALL FILL()
      DO 5 I=1,13
         MBUFF(I)=IMBUFF(I)
 5    CONTINUE
 10   CONTINUE
C
C     go get dim reference points
      CALL DIML10(X1,Y1,X2,Y2,X3,Y3,X4,Y4,OK)
C     test for abort
      IF (.NOT.OK) RETURN
C
C     go generate dimension data
      CALL DIML03(X1,Y1,X2,Y2,X3,Y3,X4,Y4,.FALSE.,M,OK)
C     check if okay
      IF ( .NOT.OK ) GOTO 10
C     store complete dimension in database
      DO 6 I=1,13
         IMBUFF(I)=MBUFF(I)
 6    CONTINUE
      CALL DEWDIM(P,LDIMN,OK)
C
C     draw the dimension on screen
C      CALL DRWDIM()
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C
      ENT=LDIMN
      CALL ALLDRW(ENT,P)
C
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C     clear any flags from screen
      CALL UNFLAG(.TRUE.)
C
C     dimension should be complete
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml10.ftn Daxcad revision 1.8
      SUBROUTINE DIML10(X1,Y1,X2,Y2,X3,Y3,X4,Y4,OK)
C     =============================================
C1                       R  R  R  R  R  R  R  R  L
C1                       O  O  O  O  O  O  O  O  O
C2    Subroutine DIML10 obtains from the operator
C2    four hits defining the dimension to be
C2    constructed.The hits obtained are returned
C2    in X1Y1,X2Y2,X3Y3,X4,Y4.
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/dimendat.inc'
C
      REAL X,Y,X1,Y1,X2,Y2,X3,Y3,X4,Y4,ABS
C
      INTEGER*4 C
      INTEGER*2 FMIPOS
C
      LOGICAL OK,CHANGE,FOUND,FIRST
C
      INTRINSIC ABS
C
      EXTERNAL GETANS,WO2SC,CROSS,RSWAP,DIML11
      EXTERNAL DSE800,UNFLAG,DIML12,TCURS,NOSRCH,ADSRCH,MNLPTS,MNUPTS
C
      OK=.FALSE.
      FIRST=.FALSE.
      FMIPOS=1
 10   CONTINUE
      IF (DOPFLG(1)) THEN
C        point to point mode currently selected
 101     CONTINUE
C        go get first point
         CALL DCPRNT(102)
         CALL GETANS(C,X1,Y1)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 101
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) GOTO 10
            GOTO 101
         END IF
C        Test for quit condition
         IF (MEN.EQ.2) RETURN
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
C        Show the hit point on screen
         CALL WO2SC(X1,Y1,RWORK1(81),RWORK1(82))
         CALL CROSS(RWORK1(81),RWORK1(82))
C
 102     CONTINUE
C        go get second point
         CALL DCPRNT(103)
         CALL GETANS(C,X2,Y2)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 102
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
C              clear flag
               CALL CROSS(RWORK1(81),RWORK1(82))
               GOTO 10
            END IF
            GOTO 102
         END IF
C        Test for quit condition
         IF (MEN.EQ.2) RETURN
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C        trap zero length dimension
         IF ((X1.EQ.X2).AND.(Y1.EQ.Y2)) THEN
            CALL DEPRNT(104)
            GOTO 101
         END IF
C
C        Show the hit point on screen
         CALL WO2SC(X2,Y2,RWORK1(83),RWORK1(84))
         CALL CROSS(RWORK1(83),RWORK1(84))
C
 103     CONTINUE
C        go get third point
         CALL DIML12(LDIMN,X3,Y3,X4,Y4,CHANGE)
         IF (CHANGE) THEN
C           clear flags
            CALL CROSS(RWORK1(81),RWORK1(82))
            CALL CROSS(RWORK1(83),RWORK1(84))
            GOTO 10
         END IF
C        Test for quit condition
         IF (MEN.EQ.2) RETURN
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
C        clean up by deleting indication crosses
         CALL CROSS(RWORK1(81),RWORK1(82))
         CALL CROSS(RWORK1(83),RWORK1(84))
C
      ELSE IF (DOPFLG(2)) THEN
C        along line mode currently selected
 201     CONTINUE
C        go get line to dimension
         CALL DCPRNT(105)
         CALL TCURS(C,X,Y)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 201
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) GOTO 10
            GOTO 201
         END IF
C        Test for quit condition
         IF (MEN.EQ.2) RETURN
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
         CALL NOSRCH()
         CALL ADSRCH(LINE)
         CALL DSE800(X,Y,FOUND)
         IF (.NOT.FOUND) THEN
            CALL DEPRNT(106)
            GOTO 201
         END IF
C
         X1=RDBUFF(1)
         Y1=RDBUFF(2)
         X2=RDBUFF(4)
         Y2=RDBUFF(5)
 203     CONTINUE
C        go get third point so load points menu
         CALL MNLPTS()
C
 204     CONTINUE
         CALL DIML12(LDIMN,X3,Y3,X4,Y4,CHANGE)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 204
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) GOTO 10
         END IF
C        now unload the points menu
         CALL MNUPTS()
C        Test for quit condition
         IF (MEN.EQ.2) RETURN
         IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
C        clear flag on line
         CALL UNFLAG(.TRUE.)
      END IF
C
C     sort the reference points into ascending order of X or Y
      IF ((X1.GT.X2).OR.(ABS(X1-X2).LT.1E-5).AND.(Y1.GT.Y2)) THEN
C        swap coordinate positions
         CALL RSWAP(X1,X2)
         CALL RSWAP(Y1,Y2)
      END IF
C
C     return with 3 dim ref points in X1Y1,X2Y2,X3Y3
      OK=.TRUE.
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml11.ftn Daxcad revision 1.8
      SUBROUTINE DIML11(CHANGE)
C     =========================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine DIML11 handles the selection
C2    of options and mode changes while within
C2    DIMENSION LINEAR command.The logical flag
C2    CHANGE is returned true if the MODE  of
C2    dimensioning is changed,false otherwise.
C2
      include 'include/movdat.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
      INTEGER*4 NLEN,I
      DOUBLE PRECISION DN
      LOGICAL CHANGE
      CHARACTER*20 TEMP,CCBUFF,TOKEN*1
C
      INTRINSIC INT,REAL
C
      EXTERNAL GTHIMC,GTMCLO,AEXPRN,DIML13,GTMCWI
      EXTERNAL MNLPTS,MNUPTS,GTHFMC,GTMCHI,NLEN,FNDTOK,GTMCWT
C     set text strings for menu output.
C
      CHANGE=.TRUE.
      IF (CCMD.EQ.'P') THEN
C        Point to Point mode selected
C        if point to point set,do nothing
C        but return with CHANGE status to
C        re-initialize the existing mode
         IF (DOPFLG(1)) RETURN
C        if point to point not set,then set it exclusively
         DOPFLG(1)=.TRUE.
         DOPFLG(2)=.FALSE.
C        ensure only one cell hilited
         CALL GTHIMC(3,CCMD,'PA',MNCELL(1,2))
         CALL MNLPTS()
         RETURN
      ELSE IF (CCMD.EQ.'A') THEN
C        along line mode selected
C        if already set do nothing
         IF (DOPFLG(2)) RETURN
C        if not set then select exclusively
         DOPFLG(1)=.FALSE.
         DOPFLG(2)=.TRUE.
C        ensure only one cell hilited
         CALL GTHIMC(3,CCMD,'PA',MNCELL(2,2))
         CALL MNUPTS()
         RETURN
      END IF
C
C     no mode change made,set flag
      CHANGE=.FALSE.
C
C     now test for direction locks etc
      IF (CCMD.EQ.'H') THEN
C        horizontal lock selected
         IF (DOPFLG(3)) THEN
C           must reset it
            DOPFLG(3)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set horizontal lock
            DOPFLG(3)=.TRUE.
            DOPFLG(4)=.FALSE.
C           ensure only one cell hilited
            CALL GTHIMC(3,CCMD,'HV',MNCELL(3,2))
         END IF
      ELSE IF (CCMD.EQ.'V') THEN
C        vertical lock selected
         IF (DOPFLG(4)) THEN
C           must reset it
            DOPFLG(4)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set vertical lock
            DOPFLG(3)=.FALSE.
            DOPFLG(4)=.TRUE.
C           ensure only one cell hilited
            CALL GTHIMC(3,CCMD,'HV',MNCELL(4,2))
         END IF
      ELSE IF (CCMD.EQ.'M') THEN
C        angular mode,set minutes
         IF ( SPCSYM .EQ. 4 .OR. SPCSYM .EQ. 5 ) THEN
C           must reset it
            CALL GTMCLO(MEN,CELLN)
            SPCSYM=3
C           ensure seconds reset as well
            CALL GTHFMC(3,'m',MNCELL(4,2))
            CALL GTMCLO(3,MNCELL(4,2))
         ELSE
C           must set minutes lock
            SPCSYM=4
         END IF
      ELSE IF (CCMD.EQ.'m') THEN
C        angular mode,set seconds
         IF ( SPCSYM .EQ. 5) THEN
C           must reset it
            SPCSYM=4
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set seconds,and minutes lock
            SPCSYM=5
C           ensure both cells hilited
            CALL GTHFMC(3,'M',MNCELL(4,2))
            CALL GTMCHI(3,MNCELL(4,2))
         END IF
      ELSE IF (CCMD.EQ.'B') THEN
C        align with other dimension
         IF (DOPFLG(5)) THEN
C           must reset it
            DOPFLG(5)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set alignment lock
            DOPFLG(5)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'t') THEN
C        major angle requested
         IF (DOPFLG(13)) THEN
C           must reset it
            DOPFLG(13)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set Major Angle.
            DOPFLG(13)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'S') THEN
C        suppress left witness line
         IF (DOPFLG(7)) THEN
C           must reset it
            DOPFLG(7)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set left suppression
            DOPFLG(7)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'s') THEN
C        suppress right witness line
         IF (DOPFLG(8)) THEN
C           must reset it
            DOPFLG(8)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set right suppression
            DOPFLG(8)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'W') THEN
C        suppress left Arrow line
         IF (DOPFLG(17)) THEN
C           must reset it
            DOPFLG(17)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set left suppression
            DOPFLG(17)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'w') THEN
C        suppress right Arrow line
         IF (DOPFLG(18)) THEN
C           must reset it
            DOPFLG(18)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set right suppression
            DOPFLG(18)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'o') THEN
C        force dimension line outside
         IF (DOPFLG(9)) THEN
C           must reset it
            DOPFLG(9)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(9)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'O') THEN
C        force dimension text outside
         IF (DOPFLG(12)) THEN
C           must reset it
            DOPFLG(12)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(12)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'F') THEN
C        offset dimension text
         IF (DOPFLG(11)) THEN
C           must reset it
            DOPFLG(11)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(11)=.TRUE.
         END IF
 
      ELSE IF (CCMD.EQ.'d') THEN
C        diametral dimension flag
         IF ( SPCSYM .EQ. 1 ) THEN
C           must reset it
            CALL GTMCLO(MEN,CELLN)
            SPCSYM=0
         ELSE
C           must set force out
            SPCSYM=1
         END IF
 
      ELSE IF (CCMD.EQ.'p') THEN
C        set new precision level
 111     CALL DPRMXP(111,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*111)
C           Check range of precision is valid
            IF ( INT(DN).GT.6 .OR. INT(DN).LT.0 ) THEN
               CALL DEPRNT(234)
               GOTO 111
            END IF
            PREC=INT(DN)
C           find token for precision:
            CALL FNDTOK(229,TOKEN)
C           write precision to menu cell
            CALL GTMCWI(3,TOKEN,PREC)
C           update the tolerance to show effect of precision
C           find token for tolerance:
            CALL FNDTOK(231,TOKEN)
            IF (TOKEN.NE.' ') THEN
C              found the expected cell ok
               CALL DIML13(LTOL,PREC,.FALSE.,TEMP)
               CALL GTMCWT(3,TOKEN,TEMP)
            END IF
C
            CALL FNDTOK(236,TOKEN)
            IF (TOKEN.NE.' ') THEN
C              found the expected cell ok
               CALL DIML13(UTOL,PREC,.FALSE.,TEMP)
               CALL GTMCWT(3,TOKEN,TEMP)
            END IF
 
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'a') THEN
C        set new lower tolerance level
 112     CALL DPRMXP(112,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*112)
            LTOL=REAL(DN)
            CALL DIML13(LTOL,PREC,.FALSE.,TEMP)
C           find token for tolerance:
            CALL FNDTOK(231,TOKEN)
C           update contents of cell
            CALL GTMCWT(3,TOKEN,TEMP)
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'u') THEN
C        set new lower tolerance level
 113     CALL DPRMXP(112,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*113)
            UTOL=REAL(DN)
            CALL DIML13(UTOL,PREC,.FALSE.,TEMP)
C           find token for tolerance:
            CALL FNDTOK(236,TOKEN)
C           update contents of cell
            CALL GTMCWT(3,TOKEN,TEMP)
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'z') THEN
C        set new tolerance type
         CALL DIMTYP()
C
      END IF
C     return with flags updated
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml12.ftn Daxcad revision 1.8
      SUBROUTINE DIML12(ENT,X,Y,X1,Y1,CHANGE)
C     =======================================
C
C1    vartype           I2  R R R  R   L
C1    iostatus          I   O O O  O   O
C
C2    Subroutine DIML12 handles the selection
C2    of the reference point for placement of
C2    linear dimensions.ENT determines Dimension type.
C2
      include 'include/movdat.inc'
      include 'include/menun.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/dimendat.inc'
C
      INTEGER*2 ENT,FMIPOS
      INTEGER*4 C,I,LCRDAT,I2I4
      REAL X,Y,X1,Y1,D1,D2,DISTXY
      LOGICAL CHANGE,OK,FIRST
      EXTERNAL DIML11,DSE800,DISTXY,TCURS,NOSRCH,ADSRCH,
     +         GETANS
 
C
      FMIPOS=1
      FIRST=.FALSE.
C
 103  CONTINUE
      CHANGE=.FALSE.
      IF (DOPFLG(5)) THEN
C        align with another dimension
         CALL DCPRNT(113)
         CALL TCURS(C,X,Y)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 103
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
               RETURN
            ELSE
               GOTO 103
            END IF
         END IF
C        Test for quit condition
         IF ((MEN.EQ.2).OR.(CCMD.EQ.'Q'.OR.CCMD.EQ.'q')) THEN
            RETURN
         END IF
C
         CALL NOSRCH()
         CALL ADSRCH(ENT)
         CALL DSE800(X,Y,OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(114)
            GOTO 103
         END IF
C        data for dim now in RWORK
         IF ( ENT .EQ. LDIMN ) THEN
C           examine Iwork array to get no. of text strings stored
C           first equate to 4 byte integer
            LCRDAT=I2I4(IWORK(4,1))
            I=MOD(LCRDAT,256)/16
            X=RWORK(1,I+2)
            Y=RWORK(2,I+2)
         ELSE IF ( ENT .EQ. ADIMN ) THEN
C           use original reference point as stored in header
            X=RWORK(4,2)
            Y=RWORK(5,2)
         END IF
      ELSE
C        find the reference point for dim
         CALL DCPRNT(115)
         CALL GETANS(C,X,Y)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 103
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
               RETURN
            ELSE
               GOTO 103
            END IF
         END IF
C        Test for quit condition
         IF ((MEN.EQ.2).OR.(CCMD.EQ.'Q'.OR.CCMD.EQ.'q')) THEN
            RETURN
         END IF
      END IF
C
      IF ( DOPFLG(11) ) THEN
C        must be offset text option so get 4th point
C        find the reference point for dim text
         CALL DCPRNT(217)
         CALL GETANS(C,X1,Y1)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 103
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
               RETURN
            ELSE
               GOTO 103
            END IF
        END IF
C       Test for quit condition
        IF ((MEN.EQ.2).OR.(CCMD.EQ.'Q'.OR.CCMD.EQ.'q')) THEN
           RETURN
        END IF
      ELSE IF ( DOPFLG(12) ) THEN
C        must be outer text option so get 4th point
C        find the reference point for dim text
         CALL DCPRNT(217)
         CALL GETANS(C,X1,Y1)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 103
            END IF
            CALL DIML11(CHANGE)
            IF (CHANGE) THEN
               RETURN
            ELSE
               GOTO 103
            END IF
         END IF
C        Test for quit condition
         IF ((MEN.EQ.2).OR.(CCMD.EQ.'Q'.OR.CCMD.EQ.'q')) THEN
            RETURN
         END IF
      END IF
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 diml13.ftn Daxcad revision 1.8
      SUBROUTINE DIML13(DIMD,PREC,ANGD,DIMT)
C     =====================================
C
C1    vartype             R   I*4  C*(*)
C1    iostatus            I    I   O
C
C2    Subroutine DIML13 converts the dimension DIMD
C2    into a character string DIMT with decimal
C2    precision PREC
C2    ANGD passed true if angular dimension type
C
      REAL DIMD
      INTEGER*4 PREC
      CHARACTER DIMT*(*),FORM*12,CHAR
      INTRINSIC NINT,CHAR,INT
      LOGICAL ANGD
C
      EXTERNAL CRUNCH
C
      FORM='(F10.2,A)'
C
      IF (PREC.NE.0) THEN
         FORM(6:6)=CHAR(48+PREC)
         IF ( ANGD ) THEN
            WRITE(UNIT=DIMT,FMT=FORM) DIMD,CHAR(129)
         ELSE
C           normal scalar value
            WRITE(UNIT=DIMT,FMT=FORM) DIMD,' '
         END IF
      ELSE
C        check to see if less than unity for leading zero
         IF ( DIMD .LE . 1 .AND. DIMD .GE. -1 ) THEN
            WRITE(UNIT=DIMT,FMT='(F4.2)') DIMD
         ELSE
C           normal scalar
            WRITE(UNIT=DIMT,FMT='(I7)') NINT(DIMD)
         END IF
      END IF
C
      CALL CRUNCH(DIMT)
C      WRITE(UNIT=10,FMT=*)'[DIML13] DIMT=',DIMT
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimr00.ftn Daxcad revision 1.8
      SUBROUTINE DIMR00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    DIMENSION RADIAL function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,DIMR01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DIM RADIAL routine
      CALL DIMR01()
C     ensure option for dim radial is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimr01.ftn Daxcad revision 1.8
      SUBROUTINE DIMR01()
C     ===================
C
C1    no arguments required
C
C2    Subroutine DIMD01 sets all conditions to
C2    default status before starting into the
C2    dimension diametral function proper.
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
      EXTERNAL MNLDM2,GTCLRM
      EXTERNAL GTHFMC,GTMCWI,MNLPTS,DIMR02
C
C     clear mode flags
      CALL CLRDOP()
C     clear the record counters.
      CALL CLRTAL()
C     initialize RADIAL option menu
      CALL MNLDM5()
C     need the point modes as well
      CALL MNLPTS()
C     enter the DIM DIAM routine
      CALL DIMR02(.FALSE.)
C     switch off the point modes
      CALL MNUPTS()
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimr02.ftn Daxcad revision 1.8
      SUBROUTINE DIMR02(DIAM)
C     =======================
C1                       L
C1                       I
C
C2    DIMR02 gathers the neccesary dimension point info
C2    for a diametral/radial dimension, logical DIAM choosing
C2    which,  including  and text position. This then
C2    passed to main dimension generating routine which
C2    is intended as general dimen creation routine.
C
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/entity.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG
C
      INTEGER*2 MBUFF(1:13)
      INTEGER*2 P,I,ENT
C
      LOGICAL OK,DIAM
C
      EXTERNAL DIMR10,DIMR03,DEWDIM,DRWDIM,UNFLAG
C
      CALL FILL()
      DO 5 I=1,13
         MBUFF(I)=IMBUFF(I)
 5    CONTINUE
 
 10   CONTINUE
 
C     Clear any logical flags which will be set by routine.
      DOPFLG(16)=.FALSE.
C     set the radial/diametral sign to be used
C     first set the diametral flag .TRUE.
      IF ( DIAM ) THEN
C       diameter requested so use Diametral sign
        SPCSYM=1
      ELSE
C       was a radial sign so set here.
        SPCSYM=2
      END  IF
C     go get entity to be dimensioned
C     and the break point and end point
C     of the leader line.
      CALL DIMR10(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,.FALSE.,OK)
C     test for abort
      IF (.NOT.OK) THEN
         CALL CLRDOP()
         RETURN
      END IF
C     go generate dimension data
      CALL DIMR03(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,OK)
C
      DO 6 I=1,13
         IMBUFF(I)=MBUFF(I)
 6    CONTINUE
      IF ( DIAM ) THEN
C        diametral dim required
         CALL DEWDIM(P,DDIMN,OK)
      ELSE
C        radial dim required
         CALL DEWDIM(P,RDIMN,OK)
      END IF
C     clear any flags from screen
      CALL UNFLAG(.TRUE.)
C     draw the dimension on screen
C      CALL DRWDIM()
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C
      ENT=RDIMN
      CALL ALLDRW(ENT,P)
C
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C     dimension should be complete
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimr10.ftn Daxcad revision 1.8
      SUBROUTINE DIMR10(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,LABEL,OK)
C     ================================================================
C
C1    vartype           R  R  R  R  R  R  R  R  R  R   R    L
C1    iostatus          O  O  O  O  O  O  O  O  O  O   O    O
C
C2    Subroutine DIMR10 obtains from the operator
C2    three hits defining the diam/radial dimension to be
C2    constructed.The hits obtained are returned
C2    in X1Y1,X2Y2,X3Y3.The radius of the arc is
C2    returned in R,centre XC,YC if successful OK returned
C2    true. If label is .TRUE. then a dimension label is requested
C2    so look for point hits only.
C2    if we use offset text then postion is returned in
C     X3 Y3
C
C
C
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/dimendat.inc'
C
      INTEGER*4 C
      INTEGER*2 FMIPOS
C
      REAL X,Y,X1,Y1,X2,Y2,X3,Y3,R,XC,YC,SANG,EANG,PI
C
      LOGICAL OK,LABEL,FIRST,SAME
C
      EXTERNAL DIMR11,DSE800,UNFLAG,NOSRCH,ADSRCH
      EXTERNAL GETANS,WO2SC,CROSS,SAME,PI
C
      OK=.FALSE.
      FIRST=.FALSE.
      FMIPOS=1
 
 101  CONTINUE
C     go get first point
      IF ( LABEL ) THEN
C       ask for any hit point
        CALL DCPRNT(124)
        CALL GETANS(C,X1,Y1)
C       cancel points modes.
      ELSE
C       ask specifically for an arc
        CALL DCPRNT(107)
        CALL GETANS(C,X1,Y1)
      END IF
C     handle menu3 options
      IF (MEN.EQ.3) THEN
         IF ( CCMD.EQ.'c') THEN
            CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
            GOTO 101
         END IF
C        well DIMR11 won't be able to handle insert text label
C        menu three options so abort at this point if called from 
C        INSERT TEXT LABEL 
         IF(LABEL) THEN
            OK = .FALSE.
            RETURN
         ELSE              
            CALL DIMR11(OK)
            IF ( .NOT.OK ) RETURN
            GOTO 101
         ENDIF
      END IF
C     Test for quit condition
      IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         OK=.FALSE.
         RETURN
      END IF
C
C     Show the hit point on screen
      CALL WO2SC(X1,Y1,RWORK1(81),RWORK1(82))
      CALL CROSS(RWORK1(81),RWORK1(82))
C
      IF ( .NOT. LABEL ) THEN
C       is true dimension type.
C       search for arc near hit point
        CALL NOSRCH()
        CALL ADSRCH(ARC)
        CALL ADSRCH(CENLIN)
        CALL DSE800(X1,Y1,OK)
        IF (.NOT.OK) THEN
           CALL DEPRNT(108)
C          erase the cross from screen
           CALL CROSS(RWORK1(81),RWORK1(82))
           GOTO 101
        END IF
C
        IF(SAME(RDBUFF(1),X1).AND.SAME(RDBUFF(2),Y1)) THEN
           CALL DEPRNT(94)
C          erase the cross from screen
           CALL CROSS(RWORK1(81),RWORK1(82))
C          clear entity flag
           CALL UNFLAG(.TRUE.)
           GOTO 101
        END IF
C
        IF ( DOPFLG(11).AND..NOT.DOPFLG(12)) THEN
C           find the reference point for dim text
            CALL DCPRNT(217)
            CALL GETANS(C,X3,Y3)
C           handle menu3 options
            IF (MEN.EQ.3) THEN
                IF ( CCMD.EQ.'c') THEN
                    CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
                    GOTO 101
                END IF
                CALL DIMR11(OK)
                IF ( .NOT.OK ) RETURN
                GOTO 102
            END IF
C           Test for quit condition
            IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
                OK=.FALSE.
                RETURN
            END IF
        ENDIF
C
        IF (IMBUFF(2).EQ.CENLIN) THEN
C          A P.C.D. centre line is also allowed
           IF (IDBUFF(4).EQ.3) THEN
C             Use the curved portion of the PCD as a circle.
              XC=RDBUFF(1)
              YC=RDBUFF(2)
              R =RDBUFF(3)
              SANG=0.0
              EANG=PI(2.0)
           ELSE
C             Wrong kind of centre line.
              CALL DEPRNT(108)
              CALL UNFLAG(.TRUE.)
              GOTO 101
           ENDIF
        ELSE
C          Use the arc as is.
           XC=RDBUFF(1)
           YC=RDBUFF(2)
           R =RDBUFF(4)
           SANG=RDBUFF(5)
           EANG=RDBUFF(6)
        ENDIF
        OK=.FALSE.
      END IF
C     check to see if wants leader type only.
      IF ( DOPFLG(15) ) THEN
C        yes is leader type so 3 hits needed.
 102     CONTINUE
C        go get break point
         CALL DCPRNT(109)
         CALL GETANS(C,X2,Y2)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 102
            END IF
C           well DIMR11 won't be able to handle insert text label
C           menu three options so abort at this point if called from 
C           INSERT TEXT LABEL 
            IF(LABEL) THEN
              OK = .FALSE.
              RETURN
            ELSE              
              CALL DIMR11(OK)
              IF ( .NOT.OK ) RETURN
              GOTO 102
            ENDIF
         END IF
C        Test for quit condition
         IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
            OK=.FALSE.
            RETURN
         END IF
C
C        Show the hit point on screen
         CALL WO2SC(X2,Y2,RWORK1(83),RWORK1(84))
         CALL CROSS(RWORK1(83),RWORK1(84))
C
 103     CONTINUE
         IF ( .NOT. DIN ) THEN
C           go get end point of leader
            CALL DCPRNT(110)
            CALL GETANS(C,X3,Y3)
C           handle menu3 options
            IF (MEN.EQ.3) THEN
               IF ( CCMD.EQ.'c') THEN
                 CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
                 GOTO 103
               END IF
C              well DIMR11 won't be able to handle insert text label
C              menu three options so abort at this point if called from 
C              INSERT TEXT LABEL 
               IF(LABEL) THEN
                 OK = .FALSE.
                 RETURN
               ELSE              
                 CALL DIMR11(OK)
                 IF ( .NOT.OK ) RETURN
                 GOTO 103
               ENDIF
            END IF
C           Test for quit condition
            IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
               OK=.FALSE.
               RETURN
            END IF
C
C           clean up by deleting indication crosses
            CALL CROSS(RWORK1(81),RWORK1(82))
            CALL CROSS(RWORK1(83),RWORK1(84))
C           clear entity flag
            CALL UNFLAG(.TRUE.)
         END IF
      ELSE  IF ( DOPFLG(12) ) THEN
C        is full dimension type
 104     CONTINUE
C        go get end point of leader
         CALL DCPRNT(110)
         CALL GETANS(C,X3,Y3)
C        handle menu3 options
         IF (MEN.EQ.3) THEN
            IF ( CCMD.EQ.'c') THEN
               CALL INSCNL(FMIPOS,LDIMN,1,FIRST,OK)
               GOTO 104
            END IF
            CALL DIMR11(OK)
            IF ( .NOT.OK ) RETURN
            GOTO 104
         END IF
C        Test for quit condition
         IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
            OK=.FALSE.
            RETURN
          END IF
C
C        clean up by deleting indication crosses
         CALL CROSS(RWORK1(81),RWORK1(82))
C        clear entity flag
         CALL UNFLAG(.TRUE.)
      END IF
      OK=.TRUE.
C
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimr11.ftn Daxcad revision 1.8
      SUBROUTINE DIMR11(OK)
C     ===================
C
C1    vartype
C1    iostatus
C
C2    Subroutine DIMR11 handles the selection
C2    of options and mode changes while within
C2    DIMENSION RADIAL command.
C2
      include 'include/movdat.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
C
      REAL REAL
C
      INTEGER*4 NLEN,I,INT
C
      DOUBLE PRECISION DN
C
      CHARACTER*20 TEMP,CCBUFF,TOKEN*1
C
      LOGICAL OK
C
      INTRINSIC INT,REAL
C
      EXTERNAL GTMCLO,AEXPRN,DIML13
      EXTERNAL CRUNCH,NLEN,GTHFMC,FNDTOK,GTMCWT
C
C
      OK=.TRUE.
C
      IF (CCMD.EQ.'W') THEN
C        suppress left Arrow line
         IF (DOPFLG(17)) THEN
C           must reset it
            DOPFLG(17)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set left suppression
            DOPFLG(17)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'w') THEN
C        suppress right Arrow line
         IF (DOPFLG(18)) THEN
C           must reset it
            DOPFLG(18)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set right suppression
            DOPFLG(18)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'F') THEN
C        offset dimension text
         IF (DOPFLG(11)) THEN
C           must reset it
            DOPFLG(11)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(11)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'o') THEN
C        force dimension line outside
         IF (DOPFLG(9)) THEN
C           must reset it
            DOPFLG(9)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(9)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'O') THEN
C        force dimension text outside
         IF (DOPFLG(12)) THEN
C           must reset it
            DOPFLG(12)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set force out
            DOPFLG(12)=.TRUE.
         END IF
      ELSE IF (CCMD.EQ.'d') THEN
C        diametral dimension flag
         IF (SPCSYM .EQ. 1) THEN
C           must reset it
            CALL GTMCLO(MEN,CELLN)
            SPCSYM=0
         ELSE
C           must set force out
            SPCSYM=1
         END IF
 
      ELSE IF (CCMD.EQ.'A') THEN
C        leader type dimension
         IF (DOPFLG(15)) THEN
C           must reset it
            DOPFLG(15)=.FALSE.
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set leader type dimension
            DOPFLG(15)=.TRUE.
         END IF
 
      ELSE IF (CCMD.EQ.'p') THEN
C        set new precision level
 111     CALL DPRMXP(111,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*111)
C           Check range of precision is valid
            IF ( INT(DN).GT.6 .OR. INT(DN).LT.0 ) THEN
               CALL DEPRNT(234)
               GOTO 111
            END IF
            PREC=INT(DN)
C           find token for precision:
            CALL FNDTOK(229,TOKEN)
C           write precision to menu cell
            CALL GTMCWI(3,TOKEN,PREC)
C           update the tolerance to show effect of precision
C           find token for tolerance:
            CALL FNDTOK(231,TOKEN)
            IF (TOKEN.NE.' ') THEN
C              found the expected cell ok
               CALL DIML13(LTOL,PREC,.FALSE.,TEMP)
               CALL GTMCWT(3,TOKEN,TEMP)
            END IF
C
            CALL FNDTOK(236,TOKEN)
            IF (TOKEN.NE.' ') THEN
C              found the expected cell ok
               CALL DIML13(UTOL,PREC,.FALSE.,TEMP)
               CALL GTMCWT(3,TOKEN,TEMP)
            END IF
 
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'a') THEN
C        set new lower tolerance level
 112     CALL DPRMXP(112,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*112)
            LTOL=REAL(DN)
            CALL DIML13(LTOL,PREC,.FALSE.,TEMP)
C           find token for tolerance:
            CALL FNDTOK(231,TOKEN)
C           update contents of cell
            CALL GTMCWT(3,TOKEN,TEMP)
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'u') THEN
C        set new lower tolerance level
 113     CALL DPRMXP(112,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*113)
            UTOL=REAL(DN)
            CALL DIML13(UTOL,PREC,.FALSE.,TEMP)
C           find token for tolerance:
            CALL FNDTOK(236,TOKEN)
C           update contents of cell
            CALL GTMCWT(3,TOKEN,TEMP)
         END IF
         CALL GTMCLO(MEN,CELLN)
C
      ELSE IF (CCMD.EQ.'z') THEN
C        set new tolerance type
         CALL DIMTYP()
C
      ELSE
         OK=.FALSE.
      END IF
C     return with flags updated
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dimtyp.ftn Daxcad revision 1.8
      SUBROUTINE DIMTYP()
C     ===================
C
      include 'include/dimendat.inc'
      include 'include/menun.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C 
      INTEGER*4 TOLTXT(6),MNUM,TCELL
      INTEGER*4 I
      LOGICAL OK
C 
C     set text strings for menu output.
      DATA (TOLTXT(I),I=1,6)/237,430,431,432,433,437/
C 
      MNUM = 4
      TCELL = CELLN
      CALL MENPOP(MNUM,OK)
      CALL GTMCLO(TCELL,3)
      IF (OK) THEN      
         TOLTYP = ICHAR(CCMD)
         GTMULT = .TRUE.
         CALL GTDMWT(429,3,VNOUN(TOLTXT(TOLTYP)))
      ENDIF
C 
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 dmntxt.ftn Daxcad revision 1.8
      SUBROUTINE DMNTXT(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
C     ===================================================
C
C1    vartype             R   I4   R   L   C(3)*(*)
C1    iostatus            I   I    I   I     O
C
C2    Subroutine DMNTXT is the controlling subroutine for Dimension
C2    text creation. It uses common block variable TOLTYP to
C2    call required dimension type routine..The Basic dimension is held
C2    in DIMT(1) The lower Tolerance value is held
C2    in DIMT(2) , the upper value in DIMT(3).
C2    STRVAL(3,I) holds the dimn text height
C2    STRVAL(4,I) holds the dimn text width
C2    STRVAL(1,I) holds the dimn text x local origin
C2    STRVAL(1,I) holds the dimn text y local origin
C2    BLWD & BLHT contain the Dimn text block width and height
C2    extents.
C
C2    Note:- Local origin is bottom left for text and block.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      REAL DIMD,STRVAL(4,10),BLWD,BLHT
      INTEGER*4 I,P,NSTR,NLEN1
      CHARACTER DIMNTX(10)*(*),TEMP*80
      INTRINSIC CHAR
      EXTERNAL NLEN1,CRUNCH,DIMD13
C
C     get the tolerance type required , but check range first.
      IF ( TOLTYP .GT. 6 .OR. TOLTYP . LT . 1 ) GOTO 99
C     check if DOPFLG(6) .TRUE. indicating regeneration of text
C     required by external source so skip this part.
      IF ( DOPFLG(6).AND..NOT.OPFLAG(9) ) THEN
C
C        OPFLAG(9) is used for editing overwritten text
C
C        must reset this flag so not used later
         DOPFLG(6)=.FALSE.
C        must go and set text string parameters
         GOTO 20
      END IF
c     now pass to subroutine
      GOTO (1,2,3,4,5,6) TOLTYP
C
 1    CONTINUE
C     no tolerance required so simply convert string
      CALL DIMD13(DIMD,PREC,DIMNTX)
C     Set basic parameters cos only one line
      CALL CRUNCH(DIMNTX(1))
      NSTR=1
      STRVAL(1,1)=DTHGT
      STRVAL(2,1)=DTWDT
      STRVAL(3,1)=0
      STRVAL(4,1)=0
C     check for diametral sign
      IF ( SPCSYM .EQ. 1 ) THEN
         TEMP=DIMNTX(1)
         DIMNTX(1)=CHAR(130)//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM .EQ. 2 ) THEN
         TEMP=DIMNTX(1)
         DIMNTX(1)='R'//' '//TEMP(1:NLEN1(TEMP))
      ELSE IF ( SPCSYM .EQ. 3 ) THEN
C        angular dimension might be required.
         TEMP=DIMNTX(1)
         DIMNTX(1)=TEMP(1:NLEN1(TEMP))//CHAR(129)
      ELSE IF ( (SPCSYM .EQ. 4) .OR. (SPCSYM .EQ. 5 ) ) THEN
C        degs,mins,seconds asked for
         CALL DIMA30(DIMD,DIMNTX(1))
      END IF
      BLHT=DTHGT
      BLWD=NLEN1(DIMNTX(1))*DTWDT
      GOTO 95
C
 2    CONTINUE
C     bilateral tolerance required , use lower tolerance value.
      CALL DIMD14(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
      GOTO 95
C
 3    CONTINUE
C     tolerance limits to be shown with basic dimension.
      CALL DIMD15(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
      GOTO 95
C
 4    CONTINUE
C     tolerance to be applied to value and basic sizes shown.
      CALL DIMD16(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
      GOTO 95
C
 5    CONTINUE
C     text input from keyboard to be added to dimension.
      CALL DIMD17(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
      GOTO 95
C
 6    CONTINUE
C     text input from keyboard , No dimension value added.
      CALL DIMD18(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
      GOTO 95
C
 20   CONTINUE
C     set text string from common block as contains mixture of
C     dimension and text so cant regenerate it.
      DO 30 I=1,NUMSTR
         DIMNTX(I)=DIMCHR(I)
         STRVAL(1,I)=DTHGT
         STRVAL(2,I)=DTWDT
C        store the text string local origin.
         STRVAL(3,I)=0
         STRVAL(4,I)=-(I-1)*1.5*DTHGT
 30   CONTINUE
      NSTR=NUMSTR
      BLHT=DTHGT
      BLWD=NLEN1(DIMNTX(1))*DTWDT
95    CONTINUE
      IF ( DIN ) THEN
C     need to convert dots to commas
        DO 103 I=1,3
C       Found dot so change to comma
            P=INDEX(DIMNTX(I),'.')
            IF ( P .GT. 0 ) THEN
                DIMNTX(I)(P:P)=','
            END IF
 103    CONTINUE
      END IF
C     Return for more
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 endval.ftn Daxcad revision 1.8
      SUBROUTINE ENDVAL(X1,Y1,X2,Y2,TMPX1,TMPY1,TMPX2,TMPY2,R1,R2)
C     ===========================================================
C1                      R  R  R  R R      R    R      R    R   R
C1                      I  I  I  I I      I    I      I    O   O
C
C2    Subroutine ENDVAL works out the end point of a line furthest
C2    from X1,Y1 along direction X1,Y1 X2,Y2
C2    End point returned in R1,R2
C
      REAL TMPX1,TMPY1,TMPX2,TMPY2,D1,D2,X1,Y1,R1,R2,DISTXY,
     +     L1,L2,L3,M1,M2,M3,X2,Y2,VD0D13
      INTRINSIC ABS
      EXTERNAL DISTXY,VD0D13
C     calculate line direction from X1,Y1,X2,Y2
      CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     get perpendicular lines thro end points
      CALL VV00L6(L1,L2,L3,TMPX1,TMPY1,M1,M2,M3)
C     get distance from start point to this end point
      D1=VD0D13(M1,M2,M3,X1,Y1)
C     get perpendicular lines thro end points
      CALL VV00L6(L1,L2,L3,TMPX2,TMPY2,M1,M2,M3)
C     get distance from start point to this end point
      D2=VD0D13(M1,M2,M3,X1,Y1)
C     set return end point
      IF ( D1 .LT. 0  .OR. D1 .LE. D2 ) THEN
C        second distance was furthest
         R1=TMPX2
         R2=TMPY2
      ELSE
C        First distance was furthest
         R1=TMPX1
         R2=TMPY1
      END IF
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 majdim.ftn Daxcad revision 1.8
      SUBROUTINE MAJDIM()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the DIMENSION mode
C2    of operation is selected from the master menu.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      EXTERNAL DIMD00,DIML00,DIMA00,MNIDIM,GTHFMC,DIMR00,GTCLRM,UNFLAG
      EXTERNAL TCURS,GTMCLO,GTMCHI
      EXTERNAL CLRPEW
C
C     Now activate the DIME major E3/A0M DIoption menu
      CALL MNIDIM
C     set default dimension parameters
      RTMP=2.5*ALNG*PAPTOW
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C     Making single line the default insert text
      MEN=2
C     'L' is the token used by insert line
      CCMD='L'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select DIMENSION type
      CALL TCURS(C,X,Y)
C
C     Making single line the default insert text
      MEN=2
C     'L' is the token used by insert line
      CCMD='L'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     ensure single entity returned for grouped entities
      GSSTAT=1
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'D') THEN
C           DIAMETER DIMENSION option
            CALL DIMD00()
         ELSE IF (CCMD.EQ.'L') THEN
C           DIM LINE option
            CALL DIML00()
         ELSE IF (CCMD.EQ.'A') THEN
C           DIM ANGULAR option
            CALL DIMA00()
         ELSE IF (CCMD.EQ.'R') THEN
C           DIM RADIAL option
            CALL DIMR00()
         ELSE
C           unrecognized dimension option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         CALL UNFLAG(.TRUE.)
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         CALL DEPRNT(91)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
      CALL UNFLAG(.TRUE.)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mnidim.ftn Daxcad revision 1.8
      SUBROUTINE MNIDIM()
C     ===================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2    the INSERT DIMENSION option list.
C2
      EXTERNAL GTDMEN,GTDMHD,GTCLRM
C
      CALL GTCLRM(3)
      CALL GTCLRM(2)
C
C     place menu header for DIMEN and hilite it
      CALL GTDMHD(215,2)
C     Enter the dimension options.
C2    A is the token for ANGULAR
      CALL GTDMEN(217,2)
C2    D is the token for DIAMETRAL
      CALL GTDMEN(216,2)
C2    R is the token for RADIAL
      CALL GTDMEN(218,2)
C2    L is the token for LINEAR
      CALL GTDMEN(219,2)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mnldm1.ftn Daxcad revision 1.8
      SUBROUTINE MNLDM1()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM1 loads the options
C2    for the LINEAR dimension function into
C2    menu no3.
C2
C2
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 I
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTDMEN,GTMCHI,MNLDM2,FNDPOS
C
      I=2
C
C2    A is the token for ALONG LINE
      CALL GTDMEN(220,3)
      DOPFLG(2)=.TRUE.
C     save the menu,cell hilited
      MNCELL(2,1)=3
      CALL FNDPOS(220,MNCELL(2,2))
      CALL GTMCHI(MNCELL(2,1),MNCELL(2,2))
C2    P is the token for POINT TO POINT
      CALL GTDMEN(221,3)
C2    d is the token for DIAMETRAL
      CALL GTDMEN(222,3)
C2    H is the token for HORIZONTAL
      CALL GTDMEN(223,3)
C2    V is the token for VERTICAL
      CALL GTDMEN(224,3)
C2    O is the token for OUTER TEXT
      CALL GTDMEN(225,3)
C2    B is the token for ALIGN
      CALL GTDMEN(226,3)
C2    S is the token for SUPPRESS LEFT WITNESS LINE
      CALL GTDMEN(227,3)
C2    s is the token for SUPPRESS RIGHT WITNESS LINE
      CALL GTDMEN(228,3)
C2    W is the token for SUPPRESS LEFT ARROW
      CALL GTDMEN(238,3)
C2    s is the token for SUPPRESS RIGHT ARROW
      CALL GTDMEN(239,3)
C2    B is the token for OUTER DIMENSION
      CALL GTDMEN(235,3)
C2    B is the token for OFFSET TEXT
      CALL GTDMEN(436,3)
C     Offset text 
C     load tolerance and precision options
      CALL MNLDM2()
C     Cancel
      CALL GTDMEN(99,3)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mnldm2.ftn Daxcad revision 1.8
      SUBROUTINE MNLDM2()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM2 loads the Common options
C2    for the DIAM/RADIAL/LINEAR dimension function
C2    Into menu no3.
C2
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 I,TOLTXT(6)
      CHARACTER CHAR,TOKEN*1,TEMP*12
      INTRINSIC CHAR
      EXTERNAL GTDMEN,FNDTOK,GTMCWT,GTMCWI,DIML13
C
C     set text strings for menu output.
      DATA (TOLTXT(I),I=1,6)/237,430,431,432,433,437/
      I=2
C
C2    p is the token for PRECISION:
      CALL GTDMEN(229,3)
C2    a is the token for LOWER TOLERANCE:
      CALL GTDMEN(231,3)
C2    S is the token for UPPER TOLERANCE:
      CALL GTDMEN(236,3)
C2    s is the token for TOLERANCE TYPE:
      GTMULT = .TRUE.
      CALL GTDMWT(429,3,VNOUN(TOLTXT(TOLTYP)))
C     find token for precision:
      CALL FNDTOK(229,TOKEN)
C     enter precision to menu
      CALL GTMCWI(3,TOKEN,PREC)
C     enter lower tolerance to menu
      CALL DIML13(LTOL,PREC,.FALSE.,TEMP)
C     find token for Lower tolerance:
      CALL FNDTOK(231,TOKEN)
C     update cell contents
      CALL GTMCWT(3,TOKEN,TEMP)
C     enter upper tolerance to menu.
      CALL DIML13(UTOL,PREC,.FALSE.,TEMP)
C     find token for Lower tolerance:
      CALL FNDTOK(236,TOKEN)
C     update cell contents
      CALL GTMCWT(3,TOKEN,TEMP)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mnldm3.ftn Daxcad revision 1.8
      SUBROUTINE MNLDM3()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM3 loads the options
C2    for the ANGULAR dimension function into
C2    menu no3.
C2
C2
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
C
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTDMEN,GTMCHI,MNLDM2,FNDPOS
C
C2    A is the token for BETWEEN LINES
      CALL GTDMEN(232,3)
      DOPFLG(2)=.TRUE.
C     save the menu,cell hilited
      MNCELL(2,1)=3
      CALL FNDPOS(232,MNCELL(2,2))
C     hilite the cell
      CALL GTMCHI(MNCELL(2,1),MNCELL(2,2))
C2    P is the token for POINT TO POINT
      CALL GTDMEN(221,3)
C2    M is the token for MINUTES
      CALL GTDMEN(233,3)
C2    m is the token for SECONDS
      CALL GTDMEN(234,3)
C2    O is the token for OUTER Text.
      CALL GTDMEN(225,3)
C2    B is the token for ALIGN
      CALL GTDMEN(226,3)
C2    O is the token for OUTER Dimension Lines.
      CALL GTDMEN(435,3)
C2    B is the token for MAJOR ANGLE required.
      CALL GTDMEN(434,3)
C2    S is the token for SUPPRESS LEFT WITNESS LINE
      CALL GTDMEN(227,3)
C2    s is the token for SUPPRESS RIGHT WITNESS LINE
      CALL GTDMEN(228,3)
C2    W is the token for SUPPRESS LEFT ARROW
      CALL GTDMEN(238,3)
C2    s is the token for SUPPRESS RIGHT ARROW
      CALL GTDMEN(239,3)
C     load tolerance and precision options
      CALL MNLDM2()
C     Cancel
      CALL GTDMEN(99,3)
C
      END
C
C     ---------------------------------------------------------
C
 
C       @(#)  256.1 date 12/16/89 mnldm4.ftn Daxcad revision 1.8
      SUBROUTINE MNLDM4()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM1 loads the options
C2    for the Diameter/Radial dimension function into
C2    menu no3.
C2
C2
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 I
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTDMEN,GTMCHI,MNLDM2,FNDPOS
C
      I=2
C
C2    A is the token for Leader type dimension.
      CALL GTDMEN(230,3)
C2    O is the token for OUTER TEXT
      CALL GTDMEN(225,3)
C2    W is the token for SUPPRESS LEFT ARROW
      CALL GTDMEN(238,3)
C2    s is the token for SUPPRESS RIGHT ARROW
      CALL GTDMEN(239,3)
C2    B is the token for OUTER DIMENSION
      CALL GTDMEN(235,3)
C     Offset text 
      CALL GTDMEN(436,3)
C     load tolerance and precision options
      CALL MNLDM2()
C     Cancel
      CALL GTDMEN(99,3)
C
      END
C
C     ---------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mnldm5.ftn Daxcad revision 1.8
      SUBROUTINE MNLDM5()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM5 loads the options
C2    for the Diameter/Radial dimension function into
C2    menu no3.
C2
C2
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 I
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTDMEN,GTMCHI,MNLDM2,FNDPOS
C
      I=2
C
C2    A is the token for Leader type dimension.
      CALL GTDMEN(230,3)
C2    O is the token for OUTER TEXT
      CALL GTDMEN(225,3)
C2    W is the token for SUPPRESS LEFT ARROW
C      CALL GTDMEN(238,3)
C2    s is the token for SUPPRESS RIGHT ARROW
C      CALL GTDMEN(239,3)
C2    B is the token for OUTER DIMENSION
      CALL GTDMEN(235,3)
C     load tolerance and precision options
C     Offset text 
      CALL GTDMEN(436,3)
      CALL MNLDM2()
C     Cancel
      CALL GTDMEN(99,3)
C
      END
C
C     ---------------------------------------------------------
C
