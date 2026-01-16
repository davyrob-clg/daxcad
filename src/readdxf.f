C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 readdxf.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE RDXCOS(STLSET,COLSET,ASTYLE,ACOL,DAXSTL,DAXCOL)
C     SUBROUTINE RDXEAC(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXELN(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXESL(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXETC(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXETX(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXF00()
C     SUBROUTINE RDXF01(OUNIT)
C     SUBROUTINE RDXHED(OUNIT,LINEP,EOF,OK)
C     SUBROUTINE RDXGTL (ACNAM,DAXLAY,DAXCOL,DAXSTL)
C     SUBROUTINE RDXPOL(OUNIT,LINEP,GROUPD,GC,GV,OK)
C     SUBROUTINE RDXREN(OUNIT,LINEP,OK)
C     SUBROUTINE RDXRLN(OUNIT,LINEP,GRPCDE,GRPVAL,EOF,OK)
C     SUBROUTINE RDXRTB(OUNIT,LINEP,EOF,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE RDXCOS(STLSET,COLSET,ASTYLE,ACOL,DAXSTL,DAXCOL)
C     ===========================================================
C1    VARYPE               L      L   C*(*)   I4    I2     I2
C1    IOSTAT               I      I     I     I     O      O
C
C2    This routine returns th equvalent daxcad font and color from acad
C
      include 'include/style.inc'
C
      CHARACTER*80 STYLE(10)*10,ASTYLE*(*)
      INTEGER*2 DAXSTL,DAXCOL,ACOL
      INTEGER*2 CURP,TLAY,COLOR(15),TCOL,I
      LOGICAL STLSET,COLSET
C
      DATA STYLE/'CONTINUOUS','HIDDEN','DASHED','DASHED','CENTER',
     +           'CENTER','PHANTOM','DOT','DASHDOT','DIVIDE'/
      DATA COLOR/1,3,5,4,2,6,7,3,11,1,9,5,13,1,8/
C
C     find a good one
      IF(COLSET) THEN
C         set defaults
          DAXCOL = 7
          DO 10 I=1,15
              IF(ACOL.EQ.COLOR(I)) THEN
C                 set extrnal color
                  COLOUR=I
                  DAXCOL = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF
C
20    CONTINUE
      IF(STLSET) THEN
C         set defaults
          DAXSTL = 1
          DO 30 I=1,10
              IF(ASTYLE.EQ.STYLE(I)) THEN
                  DAXSTL = I
                  RETURN
              ENDIF
30        CONTINUE
      ENDIF
C
      END
C
      SUBROUTINE RDXEAC(OUNIT,LINEP,GC,GV,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  L
C1    IOSTAT            I   I     I   O
C
C     This routine will write into the database the relevant
C2    information for entities
C     as it has been read into an 80 character buffer
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV,LYNAM*20
      REAL X,Y,R,ANG1,ANG2,PI,RAD
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL
      LOGICAL OK,EOF,CIRCLE
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH,RAD,PI
C
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
      CIRCLE=.FALSE.
      IF(GV.EQ.'CIRCLE') CIRCLE=.TRUE.
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                 
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.GT.9) CALL CRUNCH(GV)
      IF(GC.EQ.8) THEN
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y
      ELSEIF(GC.EQ.40) THEN
          READ(UNIT=GV,FMT='(F15.0)') R
      ELSEIF(GC.EQ.50) THEN
          READ(UNIT=GV,FMT='(F15.0)') ANG1
          ANG1=RAD(ANG1)
      ELSEIF(GC.EQ.51) THEN
          READ(UNIT=GV,FMT='(F15.0)') ANG2
          ANG2=RAD(ANG2)
      ELSEIF(GC.EQ.62) THEN
          COLSET = .TRUE.
          READ(UNIT=GV,FMT='(I3)') COLNAM
      ELSEIF(GC.EQ.6) THEN
          STLSET=.TRUE.
          STLNAM = GV
      ELSEIF(GC.EQ.0) THEN
C         Write the entity to the database
          IF(CIRCLE) THEN
              ANG1=0
              ANG2=PI(2.0)
          ENDIF
          CALL RDXGTL (LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
          CALL DEWC05(X,Y,R,ANG1,ANG2,TFONT,TLAY,P,OK)
          OK=.TRUE.
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
 
      SUBROUTINE RDXELN(OUNIT,LINEP,GC,GV,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  L
C1    IOSTAT            I   I     I   O
C
C     This routine writes the relevant informatin to the
C2    database for a line entity. This is not part of a component
C2    construct
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV,LYNAM*20
      REAL X1,Y1,X2,Y2
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL
      LOGICAL OK,EOF
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH
C
      COLSET = .FALSE.
      STLSET = .FALSE.
      TFONT=1
      TLAY=1
      TFONT=1
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                 
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.EQ.8) THEN
C         layer name
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X1
      ELSEIF(GC.EQ.11) THEN
          READ(UNIT=GV,FMT='(F15.0)') X2
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y1
      ELSEIF(GC.EQ.21) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y2
      ELSEIF(GC.EQ.62) THEN
          COLSET = .TRUE.
          READ(UNIT=GV,FMT='(I3)') COLNAM
      ELSEIF(GC.EQ.6) THEN
          STLSET=.TRUE.
          STLNAM = GV
      ELSEIF(GC.EQ.0) THEN
C         Write the entity to the database
          CALL RDXGTL (LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
          CALL DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
          OK=.TRUE.
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
C
      SUBROUTINE RDXESL(OUNIT,LINEP,GC,GV,OK)
C     =======================================
C
C1    VARTYP            I4  I4    I4  C*80 L
C1    IOSTAT            I   I     I    I   O
C
C2    This routine wil convert the autocad SOLID entity to a
C2    sort of DAXCAD format. DAXCAD cannot do solid filling
C2    therefor it is a compromise. The entity will be
C2    an outline thus.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV,LYNAM*20
      REAL X1,Y1,X2,Y2,X3,X4,Y3,Y4
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL
      LOGICAL OK,EOF,SAME
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH,SAME
C
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                 
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.EQ.8) THEN
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X1
      ELSEIF(GC.EQ.11) THEN
          READ(UNIT=GV,FMT='(F15.0)') X2
      ELSEIF(GC.EQ.12) THEN
          READ(UNIT=GV,FMT='(F15.0)') X3
      ELSEIF(GC.EQ.13) THEN
          READ(UNIT=GV,FMT='(F15.0)') X4
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y1
      ELSEIF(GC.EQ.21) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y2
      ELSEIF(GC.EQ.22) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y3
      ELSEIF(GC.EQ.23) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y4
      ELSEIF(GC.EQ.62) THEN
          COLSET = .TRUE.
          READ(UNIT=GV,FMT='(I3)') COLNAM
      ELSEIF(GC.EQ.6) THEN
          STLSET=.TRUE.
          STLNAM = GV
      ELSEIF(GC.EQ.0) THEN
          CALL RDXGTL(LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
C         Write the solid deteails out to the data base as lines
          CALL DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
          CALL DEWC03(X2,Y2,X4,Y4,TFONT,TLAY,P,OK)
C         If the last two points are the same then it will
C         have only three sides. Adjust accordingly !
          IF(SAME(X3,X4).AND.SAME(Y3,Y4)) THEN
              CALL DEWC03(X4,Y4,X1,Y1,TFONT,TLAY,P,OK)
          ELSE
              CALL DEWC03(X4,Y4,X3,Y3,TFONT,TLAY,P,OK)
              CALL DEWC03(X3,Y3,X1,Y1,TFONT,TLAY,P,OK)
          ENDIF
C         go back without delay
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
C
      SUBROUTINE RDXETC(OUNIT,LINEP,GC,GV,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  C*80 L
C1    IOSTAT            I   I     I    I   O
C
C2    This routine converts a trace to four lines in standard format
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV,LYNAM*20
      REAL X1,Y1,X2,Y2,X3,X4,Y3,Y4
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL
      LOGICAL OK,EOF
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH
C
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                 
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.EQ.8) THEN
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X1
      ELSEIF(GC.EQ.11) THEN
          READ(UNIT=GV,FMT='(F15.0)') X2
      ELSEIF(GC.EQ.12) THEN
          READ(UNIT=GV,FMT='(F15.0)') X3
      ELSEIF(GC.EQ.13) THEN
          READ(UNIT=GV,FMT='(F15.0)') X4
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y1
      ELSEIF(GC.EQ.21) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y2
      ELSEIF(GC.EQ.22) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y3
      ELSEIF(GC.EQ.23) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y4
      ELSEIF(GC.EQ.0) THEN
          CALL RDXGTL(LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
C         Write the entity to the database
          CALL DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
          CALL DEWC03(X2,Y2,X4,Y4,TFONT,TLAY,P,OK)
          CALL DEWC03(X4,Y4,X3,Y3,TFONT,TLAY,P,OK)
          CALL DEWC03(X3,Y3,X1,Y1,TFONT,TLAY,P,OK)
          OK=.TRUE.
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
 
      SUBROUTINE RDXETX(OUNIT,LINEP,GC,GV,OK)
C     =======================================
C
C1    VARTYP            I4  I4    I4  L
C1    IOSTAT            I   I     I   O
C
C2    This routine will write into the database the relevant
C2    information for text entities.
 
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
C
      CHARACTER*80 GV,TEXTS,LYNAM*20,BUFF,CON
      REAL X,Y,TANG,TSLT,XO,YO,XSF,WID,HGT
      INTEGER*4 GC,LINEP,OUNIT,NLEN1,NLEN,I,P1,P2,DNUM
      INTEGER*4 NUM
      INTEGER*2 P,TFONT,TLAY,JST,NCHAR,TCOL
      LOGICAL OK,EOF
      EXTERNAL CRUNCH,NLEN1,XYNORG,DEWC85
      CHARACTER SPECC*2
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL NLEN
C
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
      TCOL=4
      JST=0
      X=0.0
      Y=0.0
      HGT=1.0
      XSF=1.0
      TANG=0.0
      TSLT=0.0
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      IF(EOF) THEN                                    
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.GT.9) CALL CRUNCH(GV)
      IF(GC.EQ.8) THEN
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.1) THEN
          READ(UNIT=GV,FMT='(A)',ERR=300) TEXTS
          NCHAR=NLEN1(TEXTS)
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) X
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) Y
      ELSEIF(GC.EQ.40) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) HGT
      ELSEIF(GC.EQ.41) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) XSF
      ELSEIF(GC.EQ.50) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) TANG
      ELSEIF(GC.EQ.51) THEN
          READ(UNIT=GV,FMT='(F15.0)',ERR=300) TSLT
      ELSEIF(GC.EQ.72) THEN
          READ(UNIT=GV,FMT='(I2)',ERR=300) JST
      ELSEIF(GC.EQ.62) THEN
          COLSET = .TRUE.
          READ(UNIT=GV,FMT='(I3)') COLNAM
      ELSEIF(GC.EQ.6) THEN
          STLSET=.TRUE.
          STLNAM = GV
      ELSEIF(GC.EQ.0) THEN
C         alter AutoCAD justification to DAXCADS
          IF(JST.EQ.0) THEN
              JST=1
          ELSE IF(JST.EQ.1) THEN
              JST=4
          ELSE IF(JST.EQ.2) THEN
              JST=7
          ENDIF
C         Set the correct text height
          HGT=HGT/PAPTOW
C         Get the width
          WID=XSF*HGT
C         Get the bottom left hand coords of the text
          CALL  XYNORG(X,Y,JST,TANG,TSLT,NCHAR,WID,HGT,XO,YO)
C         make the justification left
          JST=1
C         get the layer details
          CALL RDXGTL (LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
C         Special characters used
          I=1
 400      CONTINUE
          IF(I.GE.NLEN(TEXTS)) GOTO 420
          SPECC=TEXTS(I:I+1)
C           WRITE(10,*) '[DXF] SPECC= ',SPECC
          IF(SPECC.EQ.'%%') THEN
              CALL DBOUND(TEXTS(I:),P1,P2,*420)
C           WRITE(10,*) '[DXF] TEXTS= ',TEXTS(P1+I:P2+I)
              IF(P1+I-1.EQ.I+2) THEN
C                  Valid numeric string
                  WRITE(UNIT=CON,FMT='(A3)') TEXTS(P1+I-1:P2-1+I)
                  READ(UNIT=CON,FMT='(I3)') NUM
C                 special convertion numbers
                  IF(NUM.EQ.127) THEN
                       NUM=129
                  ELSE IF(NUM.EQ.129) THEN
                       NUM=130
                  ENDIF
C           WRITE(10,*) '[DXF] NUM= ',NUM
                  IF(I.EQ.1) THEN
                      WRITE(UNIT=BUFF,FMT='(3A)')
     +                CHAR(NUM),TEXTS(I+P2:)
                      TEXTS=BUFF
                  ELSE
                      WRITE(UNIT=BUFF,FMT='(3A)')
     +                TEXTS(1:MAX(1,I-1)),CHAR(NUM),TEXTS(I+P2:)
                      TEXTS=BUFF
                  ENDIF
C           WRITE(10,*) '[DXF] FINAL TEXTS= ',TEXTS
                  I=I+1
                  GOTO 400
              ENDIF
          ENDIF
          I=I+1
          GOTO 400
 420      CONTINUE
C         Write the entity to the database
          CALL DEWC85(X,Y,WID,HGT,TANG,TSLT,JST,CLFONT,
     +                  TCOL,TLAY,TEXTS,P,OK)
          OK=.TRUE.
          RETURN
      ENDIF
      GOTO 10
300   WRITE(UNIT=GV,FMT='(A,I7)') 'Illegal character in text ',LINEP
      CALL EPRINT(GV)
200   OK=.FALSE.
      END
C
      SUBROUTINE RDXF00()
C     ===================
C
C1    No arguments
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/macro.inc'
      include 'include/lfu.inc'
C
      LOGICAL ENT,DELETE
      REAL M(3,3)
      INTEGER*2 I,CURP
      INTEGER*4 NLEN1,DNUM
      CHARACTER*80 FILNAM,SFILE*80
      CHARACTER*80 TFILE
      CHARACTER EXT*4
      INTEGER*4 OUNIT,ST,NLEN
      LOGICAL OK,EX,QUIT
      EXTERNAL  NLEN1
C
C     Initalise all vars
      EXT = '.dxf'
      CALL DPRMXP(81,FILNAM)
      IF(NLEN(FILNAM).EQ.0) THEN
C         zero lengh name
          GOTO 200
      ENDIF
C     Open up an ASCII file to output the data
      CALL FINDU1(OUNIT,OK)
      IF(.NOT.OK) THEN
C         unable to find free unit. Serious problem
          GOTO 110
      ENDIF
C     add on suffix
      CALL SUFFIX(FILNAM,EXT)
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
C     the file does not exist
      IF ( .NOT.EX ) THEN
          CALL DEPRNT(278)
          GOTO 200
      ENDIF
C     Everything seems ok go ahead and open the file for read.
      OPEN(UNIT=OUNIT,
     +          FILE=FILNAM,
     +          STATUS='OLD',
     +          ACCESS='SEQUENTIAL',
     +          ERR=110)
C     Initaliase all database pointers.
      CALL INITDB()
C     call the main operating routine.
      CALL RDXF01(OUNIT)
C     *********************************************************
C     everything ok close up and go home
C     *********************************************************
C
      CLOSE(UNIT=OUNIT)
C
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
C
      GOTO 200
C     *********************************************************
C     File handling error has occured. Almost certainly the
C     on the apollo the pad is open. On the PC the file
C     be in use from something else.
C     *********************************************************
120   CALL DCPRNT(277)
      GOTO 200
C     *********************************************************
C     The open statement cannot open the file. Something is
C     wrong.
C     *********************************************************
110   DNUM = 675
      CALL DEPRNT(DNUM)
200   CCMD='Q'
      MEN=0
      END
C
 
 
 
C===============================================================================
 
 
 
      SUBROUTINE RDXF01(OUNIT)
C     ========================
C
C1    VARTYPE             I4
C1    IOSTAT              O
C
C2    This routine controls the main reading and decoding of
C2    each line of DXF
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/style.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/props.inc'
C
      CHARACTER*80 GRPVAL,INSNAM,TNAM
      REAL M(3,3),TTM(3,3),TBUFF(6),X,Y
      INTEGER*4 LINEP,GRPCDE,GTYPE,OUNIT,FN,DNUM
      LOGICAL OK,EOF,INSFND,DELETE,COK
      INTEGER*2 CURP,IPRELP,ENT,PMIP,TMIP,MRELP,TTMIP,TTPDP,TTTXP,ILAY
      INTEGER*2 IPDP,ITXP,IMIP,DBLIM,ISTP,TLAY
      INTEGER*2 TIDB1,TIDB2,TIDB3,TIDB4,TDFP,SPA,I
      INTEGER*2 MMIP
C
      OK=.TRUE.
C     set the initial line pointer to 1
      LINEP=1
      FN=1
C     call the line decoding routine
      CALL RDXDEC(OUNIT,LINEP,EOF,OK)
      IF(.NOT.OK) GOTO 200
C     ***********************************
c     master information complete
C     ***********************************
      DNUM = 685
      CALL DCPRNT(DNUM)
      DBLIM=NMIPOS-1
      DO 10 CURP=1,DBLIM
         CALL DIR500(CURP,OK)
         IF(IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.COMPI) THEN
C            save the instance pointers
             IMIP=CURP
             ISTP = IMBUFF(1)
             ILAY = IMBUFF(4)
             IPDP = IMBUFF(7)
             PMIP = IMBUFF(8)
             ITXP = IMBUFF(9)
C            get the part data information about the instance
             CALL DBR500(IPDP,OK)
C            save it in a buffer
             DO 60 I=1,6
                 TBUFF(I)=RDBUFF(I)
C                reset the values to 0
                 RDBUFF(I)=0.0
 60          CONTINUE
C            save the ID information buffers
             TIDB1=IDBUFF(1)
             TIDB2=IDBUFF(2)
             TIDB3=IDBUFF(3)
             TIDB4=IDBUFF(4)
C            get the text name of the instance
             CALL DTR500(ITXP,OK)
             INSNAM=CBUFF
C            save the intasnce name
             TNAM=CBUFF
C            ***********************************
C            Go and look for an internal master
C            ***********************************
             CALL INSC11(INSNAM,FN,MMIP,COK)
             IF(.NOT.COK) GOTO 11
C            save the relation pointer and transfer it to
C            the instance later on
             MRELP=IMBUFF(10)
C            componment has been found
C            read MI data for entity
C            Now create the transformation matrix
C            for this component
C            scaling reqd
             OPFLAG(1)=.TRUE.
C            rotation reqd
             OPFLAG(2)=.TRUE.
C            transformation reqd.
             OPFLAG(4)=.TRUE.
C            block X base point
             REFDAT(1,1)=0.0
C            block Y base point
             REFDAT(1,2)=0.0
C            component X scale factor
             REFDAT(1,3)=TBUFF(3)
C            component Y scale factor
             REFDAT(10,1)=TBUFF(4)
C            block X base point
             REFDAT(2,1)=0.0
C            block Y base point
             REFDAT(2,2)=0.0
C            component rotation angle
             REFDAT(2,3)=TBUFF(5)
C            Translation X distance
             REFDAT(4,1)=TBUFF(1)
C            Translation Y distance
             REFDAT(4,2)=TBUFF(2)
C            create transform for instance
             CALL RDXMTX(M)
C            reset the entity status to what it was earlier
             IMBUFF(1)=ISTP
C            set entity type to Instance type.
             IMBUFF(2)=COMPI
C            set work layer.
             IMBUFF(4)=ILAY
C            copy window limits into instance
C            this time it will work
C            set part data pointer
             IMBUFF(7)=IPDP

             IMBUFF(8)=PMIP
C            set the text pointer for the instance
             IMBUFF(9)=ITXP
             CBUFF=TNAM
C            set relational pointer
             IMBUFF(10)=MRELP
C            detach individual properties
             IMBUFF(11)=0
C            set the ID buffers
             IDBUFF(1)=TIDB1
             IDBUFF(2)=TIDB2
             IDBUFF(3)=TIDB3
             IDBUFF(4)=TIDB4
C            Save the pointers that are necessary
             TTMIP=NMIPOS
             TTPDP=NPDPOS
             TTTXP=NTXPOS
C            kid on we have no more entities after this instance
             NMIPOS=CURP
             NPDPOS=IPDP
             NTXPOS=ITXP
C            now reenter the instance with its new information
             CALL DEW566(M,TMIP,OK)
C            reset the pointers to the real end of the database
             NMIPOS=TTMIP
             NPDPOS=TTPDP
             NTXPOS=TTTXP
          ENDIF
 11       CONTINUE
 10   CONTINUE
      DNUM = 686
      CALL DCPRNT(DNUM)
      DO 50 CURP=1,NMIPOS-1
          CALL DIR500(CURP,OK)
          IF(IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.COMPM) THEN
             CALL DTR500(IMBUFF(9),OK)
C             get the part data information about
C             save the part data pointer of the master
              TNAM=CBUFF
              TTPDP=IMBUFF(7)
C             Get the existing information about the location of
C             the bulid up of geometry
              CALL DBR500(TTPDP,OK)
C             save the ID buffers
              TIDB1=IDBUFF(1)
              TIDB2=IDBUFF(2)
              TIDB3=IDBUFF(3)
              TIDB4=IDBUFF(4)
C             Hold it! store the pointer data in a scratch file.
C             copy the master entities into a scratch file
              NDATA=0
              DO 100 I=INT(RDBUFF(1)),INT(RDBUFF(2))
                  NDATA=NDATA+1
                  WRITE(UNIT=SWINDU,REC=NDATA) I,X,Y,TDFP,SPA
 100          CONTINUE
              IF(NDATA.EQ.0) GOTO 51
C             now move the geometry according to its base point
C             set number of copies
              COPYIT=.FALSE.
              NNCOPY=0
C             disable erase and draw of entities during transformation
              OPFLAG(1)=.FALSE.
              OPFLAG(2)=.FALSE.
              OPFLAG(3)=.FALSE.
C             Translation only required
              OPFLAG(4)=.TRUE.
              OPFLAG(8)=.FALSE.
              OPFLAG(9)=.FALSE.
              OPFLAG(10)=.FALSE.
C             Get the block base point if from auto cad it could be
C             offset. DAXDXF generates only 0,0 origin blocks
              REFDAT(4,1)=-RDBUFF(4)
              REFDAT(4,2)=-RDBUFF(5)
C             Set limits of box to 0
              REFDAT(9,1)=0.0
              REFDAT(9,2)=0.0
              REFDAT(10,1)=0.0
              REFDAT(10,2)=0.0
C             go do the move
              CALL MOVMAS(.TRUE.)
C             place component limits in buffer area
C             minimum diagonal point
              RDBUFF(1) = REFDAT(9,1)
C             minimum X
              RDBUFF(2) = REFDAT(9,2)
C             maximum Y
              RDBUFF(4) = REFDAT(10,1)
C             minimum Y
              RDBUFF(5) = REFDAT(10,2)
              RDBUFF(3) = 0.0
              RDBUFF(6) = 0.0
C              RDBUFF(1)=-1E10
C              RDBUFF(2)=-1E10
C              RDBUFF(3)=0.0
C             maximum diagonal point
C              RDBUFF(4)=1E10
C              RDBUFF(5)=1E10
C              RDBUFF(6)=0.0
C              IF((ABS(RDBUFF(1)-RDBUFF(4))+ABS(RDBUFF(2)-
C     +           RDBUFF(5))).LT.20) THEN
C                  RDBUFF(1)=-100
C                  RDBUFF(4)=100
C                  RDBUFF(2)=-100
C                  RDBUFF(5)=100
C              ENDIF
C             set the ID buffers
              IDBUFF(1)=TIDB1
              IDBUFF(2)=TIDB2
              IDBUFF(3)=TIDB3
              IDBUFF(4)=TIDB4
C             write the part data back to the master
              CALL DBM500(TTPDP,OK)
          ENDIF
 51       CONTINUE
 50   CONTINUE
C     Phew! we now have to copy the window limits into the instances
C     and transform the box according to its location
C      DNUM = 741
C      CALL DCPRNT(DNUM)
      DBLIM=NMIPOS-1
      DO 20 CURP=1,DBLIM
         CALL ALLRD(CURP,ENT,M,DELETE)
         IF(.NOT.DELETE.AND.ENT.EQ.COMPI) THEN
C            save the instance pointers
             TLAY=IMBUFF(4)
             PMIP=IMBUFF(8)
             ISTP=IMBUFF(1)
             IMIP=CURP
             ITXP=IMBUFF(9)
             IPDP=IMBUFF(7)
             MRELP=IMBUFF(10)
             TNAM=CBUFF
C            get the part data information about the instance
C            save the ID infrmation buffers
             TIDB1=IDBUFF(1)
             TIDB2=IDBUFF(2)
             TIDB3=IDBUFF(3)
             TIDB4=IDBUFF(4)
C            get the text name of the instance
C            ***********************************
C            Get the internal master for the current instance
C            ***********************************
             IF(MRELP.GT.0) THEN
C                read relation list
                 CALL DRR950(MRELP,OK)
                 MMIP = RLBUFF(3)
C                now read master box
                 CALL ALLRD(MMIP,ENT,TTM,DELETE)
                 CALL DBR500(IMBUFF(7),OK)
             ELSE
                 GOTO 21
             ENDIF
C            copy box dtails into database
             DO 70 I=1,6
                RWORK(I,1)=RDBUFF(I)
                RWORK(I,2)=RDBUFF(I)
 70          CONTINUE
C            sort second diagonal data
             RWORK(2,2) = RWORK(5,1)
             RWORK(5,2) = RWORK(2,1)
C            transform enclosing box of component
             CALL MV0003(RWORK(1,1),M)
             CALL MV0003(RWORK(1,2),M)
C            reset the entity status to what it was earlier
             IMBUFF(1)=ISTP
C            set entity type to Instance type.
             IMBUFF(2)=COMPI
C            set work layer.
             IMBUFF(4)=TLAY
C            copy window limits into instance
C            this time it will work
C            set part data pointer
             IMBUFF(7)=IPDP
             IMBUFF(8)=PMIP
C            set the text pointer for the instance
             IMBUFF(9)=ITXP
             CBUFF=TNAM
C            set relational pointer
             IMBUFF(10)=MRELP
C            detach individual properties
             IMBUFF(11)=0
C             set the ID buffers
             IDBUFF(1)=TIDB1
             IDBUFF(2)=TIDB2
             IDBUFF(3)=TIDB3
             IDBUFF(4)=TIDB4
C            Save the pointers that are necessary
             TTMIP=NMIPOS
             TTPDP=NPDPOS
             TTTXP=NTXPOS
C            kid on we have no more entities after this instance
             NMIPOS=CURP
             NPDPOS=IPDP
             NTXPOS=ITXP
C            now reenter the instance with its new information
             CALL DEW566(M,TMIP,OK)
C            reset the pointers to the real end of the database
             NMIPOS=TTMIP
             NPDPOS=TTPDP
             NTXPOS=TTTXP
          ENDIF
 21       CONTINUE
 20   CONTINUE
C     Ensure all the components have the .cmp
      CALL DXCPET()
C     do a zoom extents for diplay file purposes
      CALL ZOMEXT()
      RETURN
200   DNUM =  742
      CALL DCPRNT(DNUM)
      END
      SUBROUTINE RDXHED(OUNIT,LINEP,EOF,OK)
C     =====================================
C1    VARYPE             I4    I4    L  L
C1    IOSTAT             I     I     O  O
C
C2    Reads and decodes all header information
C2
C2
C2    Arguments:-
C2
C2    OUNIT       ->          The unit number open
C2    LINEP       ->          The current line pointer.
C2    EOF         ->          End of file flag
C2
C2
C2    Error Returns:
C2
C2
C2
C2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/wtov.inc'
C
      LOGICAL EOF
      LOGICAL OK
      LOGICAL LOCKS(10)
C
C     LOCKS Description             1        Extents min
C                                   2        Extents max
C                                   3-       Spare for further use.
      INTEGER*4 GC
      INTEGER*4 OUNIT
      INTEGER*4 LINEP
      INTEGER*4 DNUM
      REAL XMIN,YMIN
      REAL XMAX,YMAX
      INTEGER*4 I
      CHARACTER*80 GV
C
      DO 5 I = 1,10
          LOCKS(I) = .FALSE.
5     CONTINUE
C
      XMIN = WXMIN
      YMIN = WYMIN
      XMAX = WXMAX
      YMAX = WYMAX
C
      EOF = .FALSE.
      OK = .FALSE.
C
      GC = 0
C
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
C
      CALL CRUNCH(GV)
      IF(EOF) THEN
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
 
      IF(GC.EQ.10) THEN
C         X coordinate descriptiuon
          IF(LOCKS(1) ) THEN
              CALL RVALU(GV,XMIN,OK)
          ELSEIF(LOCKS(2) ) THEN
              CALL RVALU(GV,XMAX,OK)
          ENDIF
      ELSEIF(GC.EQ.20) THEN
C         Y coordinate descriptiuon
          IF(LOCKS(1) ) THEN
              CALL RVALU(GV,YMIN,OK)
          ELSEIF(LOCKS(2) ) THEN
              CALL RVALU(GV,YMAX,OK)
          ENDIF
      ELSEIF(GC.EQ.9) THEN
C         clear locks list
          DO 6 I = 1,10
              LOCKS(I) = .FALSE.
 6        CONTINUE
          IF(GV.EQ.'$EXTMIN') THEN
C             lock in externs min
              LOCKS(1) = .TRUE.
          ELSEIF(GV.EQ.'$EXTMAX') THEN
C             lock in externs maximum
              LOCKS(2) = .TRUE.
          ENDIF
      ELSE IF(GC.EQ.0.AND.GV.EQ.'ENDSEC') THEN
C         next section
          GOTO 300
      ENDIF
C
      GOTO 10
C     error here
200   OK = .FALSE.
      RETURN
300   CONTINUE
C
C     set new world coords for reading into viewport
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C
C
      END
C
C
 
      SUBROUTINE RDXGTL (ACNAM,DAXLAY,DAXCOL,DAXSTL)
C     =============================================
C
C1    VARTYPE           C*20    I2      I2     I2
C1    IOSTAT              I     O        O     O
C
C2    From a layer name this function gets the daxcad layer number
C2    its line style and color
C
      include 'include/masti.inc'
      include 'include/dxf.inc'
      include 'include/style.inc'
C
      CHARACTER*20 ACNAM
      INTEGER*2 DAXLAY,DAXSTL,DAXCOL,I
C     default layer number
      DAXLAY=1
C     search the current set layers
      DO 10 I=1,NUMLAY
          IF(LNAME(I).EQ.ACNAM) THEN
              DAXLAY=I
              DAXCOL=INT(LSTYLE(I)/256)
              COLOUR = DAXCOL
              DAXSTL=LSTYLE(I)-(DAXCOL*256)
              RETURN
          ENDIF
 10   CONTINUE
      END
C
 
 
      SUBROUTINE RDXPOL(OUNIT,LINEP,GROUPD,GC,GV,OK)
C     ==============================================
C
C1    VARTYP            I4  I4    I4    L   C*80 L
C1    IOSTAT            I   I      I    I    I   O
C
C2    This routine will read the DXF POLYLINE entity
C2    It converts it to lines in a group format. Thus enabling
C2    the connectivity in the AUTOCAD POLYLINE but not the restrictions
C2    say of the DAXCAD POLYGON entity
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
C
      CHARACTER*80 GV,LYNAM*20,VLAY*20
      REAL X1,Y1,X2,Y2,X3,X4,Y3,Y4,SX,SY,BX,BY,BULGE
      REAL ANG1, ANG2, RADIUS, WIDTH
      INTEGER*4 GC,LINEP,OUNIT,TYPE,NOV,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL,D1,LFONT
      LOGICAL OK,EOF,SAME,CLOSED,VERTEX,ADD,FIN,LAYSET,START,SEMI
      LOGICAL GROUPD
      EXTERNAL CRUNCH,SAME
C
      TLAY=1
      TFONT=1
      NOV = 0
      START = .FALSE.
      LAYSET = .FALSE.
      CLOSED  =.FALSE.
      VERTEX = .FALSE.
      SEMI = .FALSE.
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                          
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
C
C     Start processing polyline data
C     we are looking for VERTEX and SEQEND and other flags to indicate
C     that the polyline is closed.
C     The best way to do it it is to put the whole reference of each
C     line into the scratch file and then create a new group entity
C
      FIN = .FALSE.
      ADD = .FALSE.
      CALL CRUNCH(GV)
      IF(GC.EQ.8) THEN
C         get the layer name
          IF(.NOT.LAYSET) THEN
             LYNAM=GV(1:20)
             LAYSET = .TRUE.
          ENDIF
      ELSEIF(GC.EQ.70) THEN
C         polyline flag specifies open or closed profile
C         vertex flag specifys other inforrmation - weed out
         READ(UNIT=GV,FMT='(I1)') TYPE
          IF (.NOT.VERTEX) THEN
             CLOSED = TYPE.EQ.1
          ENDIF
      ELSEIF(GC.EQ.42) THEN
C         polyline flag specifies the bulge factor
          READ(UNIT=GV, FMT='(F15.0)') BULGE
          SEMI = BULGE.EQ.1.0
      ELSEIF(GC.EQ.0) THEN
          READ(UNIT=GV,FMT='(A)')
          IF(GV.EQ.'VERTEX') THEN
C             allow for first vertex
              IF(VERTEX) THEN
                  ADD = .TRUE.
                  VLAY = LYNAM
              ELSE
                  START = .TRUE.
                  ADD=.FALSE.
              ENDIF
              VERTEX = .TRUE.
          ELSEIF(GV.EQ.'SEQEND') THEN
              FIN = .TRUE.
              ADD = .TRUE.
          ENDIF
C      ELSEIF(GC.EQ.40) THEN
C         polyline flag specifies the line width
C          READ(UNIT=GV, FMT='(F15.0)') WIDTH
C          LFONT = 
C
      ELSEIF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X1
      ELSEIF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y1
      ENDIF
      IF(ADD) THEN
C         add the line to the data base
          NOV = NOV+1
          IF(NOV.GT.1) THEN
C             we can enter this point and the last into the data base
              CALL RDXGTL (VLAY,TLAY,TCOL,TFONT)
C             enter the line into the data base
              IF(.NOT.(SAME(X1,X2).AND.SAME(Y1,Y2))) THEN
                 IF (SEMI) THEN
                    ANG1 = 0
                    IF (CLOSED) THEN
                       ANG2 = PI(2.0)
                    ELSE IF (BULGE .GT. 0) THEN
                       ANG2 = PI(1.0)
                    ENDIF
                    IF (X1.LT.X2) THEN
                       RADIUS = ABS((X2-X1)/2)
                       X1 = X1 + RADIUS
                    ELSE
                       RADIUS = ABS((X1-X2)/2)
                       X1 = X2 + RADIUS
                    ENDIF
                    CALL DEWC05(X1,Y1,RADIUS,ANG1,ANG2,LFONT,TLAY,
     +                   P,OK)
                    GOTO 200
                 ELSE
                    CALL DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
                 ENDIF
              END IF
C             mark for groupo create later
              CALL SSFLAG(P,BX,BY,DFP,D1,.FALSE.)
          ENDIF
          X2 = X1
          Y2 = Y1
          IF(START) THEN
C             save the start point for closing poly`gons
              SX = X1
              SY = Y1
              START = .FALSE.
          ENDIF
      ENDIF
C
      IF(FIN.AND.CLOSED) THEN
          CALL RDXGTL (VLAY,TLAY,TCOL,TFONT)
C         enter the line into the data base
          IF(.NOT.(SAME(SX,X2).AND.SAME(SY,Y2))) THEN
               CALL DEWC03(SX,SY,X2,Y2,TFONT,TLAY,P,OK)
          END IF
C         mark for groupo create later
          CALL SSFLAG(P,BX,BY,DFP,D1,.FALSE.)
      ENDIF
C
      IF(FIN) THEN
C         create a group out
C         gruoup this polyline unless it is part of a block
          IF(NDATA.GT.0.AND.GROUPD) THEN
              CALL DEW050(P,OK)
          ENDIF
      ELSE
          GOTO 10
      ENDIF
C
200   CONTINUE
C     reset all ents in scratch file
      NDATA = 0
      RETURN
      END
 
 
 
 
      SUBROUTINE RDXREN(OUNIT,LINEP,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  L
C1    IOSTAT            I   I     I   O
C
C     This routine will write into the database the relevant
C2    information for entities
C     as it has been read into an 80 character buffer
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV
      INTEGER*4 GC,LINEP,OUNIT
      LOGICAL OK,EOF
      EXTERNAL CRUNCH
C
20    CONTINUE
      CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      IF(.NOT.OK.OR.EOF) RETURN
10    CONTINUE
      CALL CRUNCH(GV)
      IF ( GC.EQ.0.AND.GV.EQ.'LINE') THEN
          CALL RDXELN(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'ARC') THEN
          CALL RDXEAC(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'CIRCLE') THEN
          CALL RDXEAC(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'TEXT') THEN
          CALL RDXETX(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'TRACE') THEN
          CALL RDXETC(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'SOLID') THEN
          CALL RDXESL(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'INSERT') THEN
          CALL RDXECI(OUNIT,LINEP,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'POLYLINE') THEN
          CALL RDXPOL(OUNIT,LINEP,.TRUE.,GC,GV,OK)
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'POINT') THEN
C         new point entity
          CALL RDXRPT(OUNIT,LINEP,GC,GV,OK)
      ELSEIF( GC.EQ.0.AND.GV.EQ.'ENDSEC') THEN
          RETURN
      ELSE
          GOTO 20
      ENDIF
      GOTO 10
      END

      SUBROUTINE RDXRLN(OUNIT,LINEP,GRPCDE,GRPVAL,EOF,OK)
C     ================================================
C
C1    VARTYPE             I4   I4    I4     C80   L
C1    IOSTAT              I     I     O       O    O
C
C2    This routine reads the DXF code 2 lines at a time .
C2    It also highlights the menu cell for the working banner.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GRPVAL,BUFF
      INTEGER*4 LINEP,GRPCDE,OUNIT,M1,C1
      LOGICAL OK,EOF
C
C     initialise the variables .
      M1=2
      C1=10
      EOF=.FALSE.
      OK=.TRUE.
C     This is a group code
      READ(UNIT=OUNIT,FMT='(I10)',END=100,ERR=200) GRPCDE
C     This is a group value.
      READ(UNIT=OUNIT,FMT='(A)',END=100,ERR=200) GRPVAL
 
C     Highlight the working banner every 200 lines.
      IF(MOD(LINEP-1,500).EQ.0) THEN
           WRITE(UNIT=BUFF,FMT='(I6,A)') LINEP-1,'  DXF LINES '
           CALL CPRINT (BUFF)
      ENDIF
C     increment line pointer.
      LINEP=LINEP+2
      RETURN
C     Error on the input line format
 200  WRITE(UNIT=GRPVAL,FMT='(A,I6)')'DXF format error on line ',LINEP
      CALL EPRINT(GRPVAL)
      OK=.FALSE.
      RETURN
 100  CONTINUE
      EOF=.TRUE.
      END
C
C
      SUBROUTINE RDXRTB(OUNIT,LINEP,EOF,OK)
C     =======================================
C
C1    VARTYP            I4  I4    I4  C*80 L
C1    IOSTAT            I   I     I    I   O
C
C     This routine reads the various table types in a
C2    DXF file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV
      REAL X1,Y1,X2,Y2,X3,X4,Y3,Y4
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P
      LOGICAL OK,EOF,TAB
      EXTERNAL CRUNCH
C
 10   CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
 20   IF(.NOT.OK) GOTO 200
      IF(GC.EQ.0.AND.GV.EQ.'TABLE') THEN
          TAB=.TRUE.
      ELSEIF(GC.EQ.2.AND.GV.EQ.'LAYER') THEN
          CALL RDXRLY(OUNIT,LINEP,GC,GV,OK)
          GOTO 20
      ELSEIF(GC.EQ.0.AND.GV.EQ.'ENDTAB') THEN
          TAB=.FALSE.
      ELSEIF(GC.EQ.0.AND.GV.EQ.'ENDSEC') THEN
          RETURN
      ENDIF
      GOTO 10
 200  OK=.FALSE.
      END
 
