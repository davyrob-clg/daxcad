C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writerc.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE FHERR(FNAM,ERR)
C     SUBROUTINE MNIWR1()
C     SUBROUTINE REDAN1()
C     SUBROUTINE REDHED()
C     SUBROUTINE WRED01()
C     SUBROUTINE WRED02()
C     SUBROUTINE WREDAC()
C     SUBROUTINE WREDOG()
C     SUBROUTINE WRRPCF()
C     SUBROUTINE WRTBD1()
C     SUBROUTINE WRTBD2()
C     SUBROUTINE WRTBOA()
C     SUBROUTINE WRTLB9()
C     SUBROUTINE WRTRED()
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE FHERR(FNAM,ERR)
C     ==========================
C1    vartype          C*(*) I2
C!    iostatus           I   I
C
C2    Signals a file handling error
C2    Primarily intended for MSDOS file
C2    handling routines
C
      INTEGER*2 ERR
      INTEGER NLEN1,L1,L2
      CHARACTER*(*) FNAM,STRING*80,MSG*16
      EXTERNAL NLEN1
C
      L1=LEN(STRING)
      STRING=' '
      GOTO (32,32,33,34,35,36) ERR
 32   CONTINUE
      MSG='No File:'
      GOTO 40
 33   CONTINUE
      MSG='No Path:'
      GOTO 40
 34   CONTINUE
      MSG='No Handle:'
      GOTO 40
 35   CONTINUE
      MSG='No Access:'
      GOTO 40
 36   CONTINUE
      MSG='Wrong Handle:'
      GOTO 40
 40   CONTINUE
      L2=NLEN1(MSG)
      STRING=MSG(1:L2)
      STRING(L2+1:)=FNAM
      CALL DEPRNT(636)
C
      END
C
C     --------------------------------------------------
C
 
      SUBROUTINE MNIWR1()
C     ===================
C
C     initialise minor menu for redac
 
C   326     6   (C)     " Component"
      CALL GTDMEN(326,3)
C   327     8   (B)     " Board"
      CALL GTDMEN(327,3)
C   328    10   (G)     " Off grid"
      CALL GTDMEN(328,3)
C     auto numbering
      CALL GTDMEN(324,3)
 
      END
C
C     --------------------------------------------------
C
      SUBROUTINE REDAN1()
C     ===================
C1    vartype
C1    iostatus
C
C2    automatically number the the component pads
C2    so that they are unique.
C2    this helps over the intial typing.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/entity.inc'
      include 'include/lfu.inc'
C
      LOGICAL OK
      CHARACTER*3 NAME
      INTEGER ONUNIT,I,K
      INTEGER*2 ENTYPE,TMIP,COL,NOPAD,MINP,P,TJST,TFONT,
     +  TCOL,TLAY
      REAL X,Y,SNAPG,GSIZE,TOL,M1(3,3),XMIN,YMIN
      EXTERNAL SNAPG
C
      CALL OURSCR(ONUNIT,8,OK)
      TJST=5
      TFONT=1
      TCOL=5
      TLAY=41
 
      DO 101 COL=2,3
        XMIN=9999.0
        YMIN=9999.0
        NOPAD=0
C       loop round the current display file.
        DO 30 TMIP=1,NMIPOS-1
          CALL DIR500(TMIP,OK)
          IF (  IMBUFF(1).NE.100.AND.IMBUFF(1).EQ.10
     +          .AND.IMBUFF(4).EQ.0.AND.IMBUFF(2).EQ.ARC ) THEN
C           we have valid data.
            CALL ALLRD(TMIP,ENTYPE,M1,OK)
            IF ( IMBUFF(3).EQ.COL )    THEN
              NOPAD=NOPAD+1
              WRITE(UNIT=ONUNIT,REC=NOPAD,ERR=98) RDBUFF(1),RDBUFF(2)
              IF ( RDBUFF(1).LT.XMIN.AND.RDBUFF(2).LT.YMIN) THEN
                XMIN=RDBUFF(1)
                YMIN=RDBUFF(2)
                MINP=NOPAD
              END IF
            ELSE IF ( COL.EQ.2.AND.IMBUFF(3).EQ.4 )    THEN
              NOPAD=NOPAD+1
              WRITE(UNIT=ONUNIT,REC=NOPAD,ERR=98) RDBUFF(1),RDBUFF(2)
              IF ( RDBUFF(1).LT.XMIN.AND.RDBUFF(2).LT.YMIN) THEN
                XMIN=RDBUFF(1)
                YMIN=RDBUFF(2)
                MINP=NOPAD
              END IF
            END IF
          END IF
 30     CONTINUE
C
        IF ( NOPAD.GT.0 ) THEN
C         swap minimum with. first if not first
          IF   ( MINP.NE.1 ) THEN
              READ(UNIT=ONUNIT,REC=1,ERR=97) X,Y
              WRITE(UNIT=ONUNIT,REC=MINP,ERR=98) X,Y
              WRITE(UNIT=ONUNIT,REC=1,ERR=98) XMIN,YMIN
          END IF
          DO 10 I=1,NOPAD-1
            READ(UNIT=ONUNIT,REC=I) XMIN,YMIN
            DO 20 K=I+1,NOPAD
              READ(UNIT=ONUNIT,REC=K) X,Y
              IF ( Y.LT.YMIN ) THEN
                WRITE(UNIT=ONUNIT,REC=I,ERR=98) X,Y
                WRITE(UNIT=ONUNIT,REC=K,ERR=98) XMIN,YMIN
                XMIN=X
                YMIN=Y
              END IF
 20         CONTINUE
 10       CONTINUE
          ENTYPE=TEXT
          DO 99 I=1,NOPAD
            READ(UNIT=ONUNIT,REC=I,ERR=97) X,Y
            WRITE(UNIT=NAME,FMT='(I3)',ERR=98) I
            CALL CRUNCH(NAME)
            CALL DEWC85(X,Y,0.6,0.8,0.0,10.0,
     +      TJST,TFONT,TCOL,TLAY,NAME,P,OK)
            CALL SPCDRW(ENTYPE,P,.FALSE.,M1,.FALSE.)
 99       CONTINUE
        END IF
 101  CONTINUE
C
 100  CONTINUE
C
      LFU(ONUNIT)=.FALSE.
      CLOSE(UNIT=ONUNIT)
      RETURN
C
 96   CONTINUE
C     get here if error writing output file
      I=422
      CALL DEPRNT(I)
      GOTO 100
 97   CONTINUE
C     get here if error reading scratch file
      I=83
      CALL DEPRNT(I)
      CALL DEPRNT(664)
      GOTO 100
 98   CONTINUE
C     get here if error writing scratch file
      I=171
      CALL DEPRNT(I)
      GOTO 100
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE REDHED()
C     ===================
C
      include 'include/redboard.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/params.inc'
      include 'include/swind.inc'
      include 'include/lfu.inc'
C
      CHARACTER*5 NAME
      REAL LIMX,LIMY,M1(3,3),X,Y,GSIZE,TOL,SNAPG,IX,IY,X1,Y1,SCA
      LOGICAL OK,ON,FOUND
      INTEGER NLEN,UNIT,I,K,ONUNIT,OFUNIT
      INTEGER*2 TMIP,ENTYPE,NOGP,NOOP,I2,NODP,II2,OFN,IT
      EXTERNAL NLEN,SNAPG
C
      NAME='    '
      LIMX=0
      LIMY=0
      NOGP=0
      NOOP=0
      NODP=0
      NDATA=1
      OFN=0
C     Tolerance=TOLVAL*INCH_FAC/DBUFAC,where TOLVAL in Inches
C     TOLVAL set to 5 thou (0.127mm)
      TOL=0.001*2.54/DBUFAC
C     set grid size to 25 thou (1 DSU)
C     GSIZE=SIZE*INCH_FAC/DBUFAC where size in inches
      GSIZE=0.025*2.54/DBUFAC
      SCA=10000.0*DBUFAC/25.4
C     open pad file.
      CALL OURSCR(ONUNIT,10,OK)
      CALL OURSCR(OFUNIT,12,OK)
C
C     loop round the current display file.
      DO 30 TMIP=1,NMIPOS-1
        CALL DIR500(TMIP,OK)
        IF (  IMBUFF(1).NE.100.AND.IMBUFF(1).EQ.10 ) THEN
C         we have valid data.
          CALL ALLRD(TMIP,ENTYPE,M1,OK)
          IF ( IMBUFF(4).EQ.0 )      THEN
C           layer 0
C           colour 1 is the component outline.
            IF ( IMBUFF(3).EQ.1 ) THEN
               LIMX=MAX(LIMX,RDBUFF(1),RDBUFF(4))
               LIMY=MAX(LIMY,RDBUFF(2),RDBUFF(5))
C           pad geometry.
            ELSE IF ( IMBUFF(3).EQ.2 ) THEN
C             colour 2 is on grid pads.
              NOGP=NOGP+1
            ELSE IF ( IMBUFF(3).EQ.3 ) THEN
C             colour 3 is off grid pads.
              NOOP=NOOP+1
            ELSE IF ( IMBUFF(3).EQ.4 ) THEN
C             colour 4 is on grid dummy pads
              IF ( IMBUFF(2).EQ.ARC) THEN
                NODP=NODP+1
              ELSE
                WRITE(UNIT=SWINDU,REC=NDATA,ERR=98) TMIP,IMBUFF(2),
     +                   RDBUFF(1),RDBUFF(2)
                NDATA=NDATA+1
              END IF
            END IF
          ELSE IF ( IMBUFF(4).EQ.41 ) THEN
            IF ( IMBUFF(2).EQ.TEXT) THEN
C             I2 is the pad code which we don't know at
C             this time so make the default code 3.
C             Pad number.
              I2=3
C             I is the pad number ,save x,y position.
              X=SNAPG(RDBUFF(1),GSIZE,0.0)-RDBUFF(1)
              Y=SNAPG(RDBUFF(2),GSIZE,0.0)-RDBUFF(2)
              ON=SQRT(X*X+Y*Y).LT.TOL
              CALL IVALU(CBUFF,I,OK)
C              ON=.TRUE.
              IF ( ON ) THEN
                WRITE(UNIT=ONUNIT,REC=I,ERR=98) RDBUFF(1),RDBUFF(2),I2
              ELSE
                OFN=OFN+1
                II2=I
                WRITE(UNIT=OFUNIT,REC=OFN,ERR=98)
     +           II2,RDBUFF(1),RDBUFF(2),I2
              END IF
            END IF
          ELSE IF ( IMBUFF(4).EQ.43 ) THEN
C           text on layer 43 placed at 0,0 is
C           component name.
            IF (RDBUFF(1).EQ.0.0.AND.RDBUFF(2).EQ.0.0
     +          .AND.IMBUFF(2).EQ.TEXT       ) THEN
              I=INDEX(CBUFF,'.')
              IF ( I.EQ.0 ) I=NLEN(CBUFF)+1
              NAME=CBUFF(2:I-1)
            END IF
          END IF
        END IF
 30   CONTINUE
C
      IF ( (NOGP.EQ.0.AND.NODP.EQ.0) ) THEN
        CALL DCPRNT(659)
        CALL DEPRNT(660)
        GOTO 100
      ELSE IF (NODP.NE.NOOP ) THEN
        CALL DCPRNT(661)
        CALL DEPRNT(662)
        GOTO 100
      END IF
 
C     loop round the  file.
      DO 40 TMIP=1,NMIPOS-1
        CALL DIR500(TMIP,OK)
        IF (  IMBUFF(1).NE.100 ) THEN
C         we have valid data.
          CALL ALLRD(TMIP,ENTYPE,M1,OK)
          IF ( IMBUFF(4).EQ.42 .AND.
     +         IMBUFF(2).EQ.TEXT)     THEN
C           text on layer 42 is the pad code number
C           search through
            I=0
 50         I=I+1
            READ(UNIT=ONUNIT,REC=I,ERR=97) X,Y,I2
            IF ( ABS(RDBUFF(1)-X).LT.TOL.AND.
     +           ABS(RDBUFF(2)-Y).LT.TOL) THEN
              CALL IVALU(CBUFF,K,OK)
              I2=K
              WRITE(UNIT=ONUNIT,REC=I,ERR=98) X,Y,I2
            ELSE
              IF ( I.LT.(NOGP+NODP)) GOTO 50
              I=0
 70           I=I+1
              READ(UNIT=OFUNIT,REC=I,ERR=97) II2,X,Y,I2
              IF ( RDBUFF(1).EQ.X.AND.RDBUFF(2).EQ.Y) THEN
                CALL IVALU(CBUFF,K,OK)
                I2=K
                WRITE(UNIT=OFUNIT,REC=I,ERR=98) II2,X,Y,I2
              ELSE
                IF ( I.LT.NODP) GOTO 70
              END IF
            END IF
          END IF
        END IF
 40   CONTINUE
C     if no component name then give default 999
      IF ( NLEN(NAME).EQ.0 ) NAME='999'
      CALL IVALU(NAME,I,OK)
C
      IF ( LIMX.EQ.0.0.OR.LIMY.EQ.0.0 ) THEN
        CALL DEPRNT(663)
      ENDIF
 
      WRITE(RDLINE,FMT='(A,I6,2X,2I5,I4)',ERR=96)
     +  'L',I,NINT(LIMX/DSU),NINT(LIMY/DSU),NOGP+NODP
      CALL WRTRED()
      DO 60 I=1,(NOGP+NODP)
        READ(UNIT=ONUNIT,REC=I,ERR=98) X,Y,I2
        WRITE(UNIT=RDLINE,FMT='(2I7,I5)',ERR=96)
     +  NINT(X/DSU),NINT(Y/DSU),I2
        CALL WRTRED()
 60   CONTINUE
 
C     offgrid pads no to deal with
C
      DO 65 I=1,NOOP
        WRITE(RDLINE,FMT='(A)',ERR=96)'OFF'
        CALL WRTRED()
        READ(UNIT=OFUNIT,REC=I,ERR=97) II2,X,Y,I2
        WRITE(UNIT=RDLINE,FMT='(I7)',ERR=96)II2
        CALL WRTRED()
        READ(UNIT=ONUNIT,REC=II2,ERR=97) IX,IY,IT
        WRITE(UNIT=RDLINE,FMT='(2I7,I5)',ERR=96)
     +  NINT(X*SCA),NINT(Y*SCA),I2
        CALL WRTRED()
C       must now look for any tracks comming off
        K=0
        FOUND=.FALSE.
 68     K=K+1
          READ(UNIT=SWINDU,REC=K,ERR=97) TMIP,ENTYPE,X1,Y1
          IF (TMIP.GT.0 ) THEN
            CALL ALLRD(TMIP,ENTYPE,M1,OK)
C
            IF ( ABS(RDBUFF(1)-IX).LT.TOL/2.0
     +      .AND.ABS(RDBUFF(2)-IY).LT.TOL/2.0) THEN
              WRITE(UNIT=RDLINE,FMT='(2I7)')
     +      NINT((RDBUFF(4)-RDBUFF(1))*SCA),
     1      NINT((RDBUFF(5)-RDBUFF(2))*SCA)
            CALL WRTRED()
              IX=RDBUFF(4)
              IY=RDBUFF(5)
              FOUND=.TRUE.
            ELSE IF ( ABS(RDBUFF(4)-IX).LT.TOL/2.0
     +           .AND.ABS(RDBUFF(5)-IY).LT.TOL/2.0) THEN
              WRITE(UNIT=RDLINE,FMT='(2I7)')
     +      NINT((RDBUFF(1)-RDBUFF(4))*SCA),
     1      NINT((RDBUFF(2)-RDBUFF(5))*SCA)
            CALL WRTRED()
              IX=RDBUFF(1)
              IY=RDBUFF(2)
              FOUND=.TRUE.
            END IF
            IF ( FOUND )
     +      WRITE(UNIT=SWINDU,REC=K,ERR=98) -TMIP,ENTYPE,X1,Y1
          END IF
          IF ( .NOT.FOUND.AND.K.LT.NDATA-1) GOTO 68
        IF ( FOUND ) THEN
          FOUND=.FALSE.
          K=0
          GOTO 68
        END IF
C
 65   CONTINUE
C
 100  CONTINUE
      CLOSE(UNIT=ONUNIT)
      CLOSE(UNIT=OFUNIT)
      LFU(ONUNIT)=.FALSE.
      LFU(OFUNIT)=.FALSE.
      RETURN
C
 96   CONTINUE
C     get here if error writing output file
      I=422
      CALL DEPRNT(I)
      GOTO 100
 97   CONTINUE
C     get here if error reading scratch file
      I=83
      CALL DEPRNT(I)
      GOTO 100
 98   CONTINUE
C     get here if error writing scratch file
      I=171
      CALL DEPRNT(I)
      GOTO 100
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRED01()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    controls operation of the WRITE redac function
C
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/redboard.inc'
      include 'include/lfu.inc'
C
      CHARACTER*3 SEQ,UNF
      LOGICAL YES
      INTEGER TMEN,TCELL
      INTEGER*4 OUNIT,NLEN
      EXTERNAL GTMCLO,WRTCV1,NLEN
C
      TMEN=MEN
      TCELL=CELLN
C     enter the WRITE REDAC FUNCTION
 100  CONTINUE
C     ask for filename for data
      CALL DPRMXP(81,CBUFF)
C     if null file name assume abort required
      IF ( NLEN(CBUFF).GT.0 ) THEN
CAPOLLO|SUN
        CALL OPNFFA(CBUFF,REDUNT,YES)
        IF ( YES )  CALL WRED02()
CAPOLLO|SUN
CIBM
C         REDUNT=0
C        CALL CREATF(CBUFF,REDUNT,ST)
C        IF (ST.EQ.0) CALL WRED02()
CIBM
      END IF
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
      CCMD='q'
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRED02()
C     ===================
C
      include 'include/redboard.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 I
C     output header for component output.
      RDLINE='.PCB'
      CALL WRTRED()
      RDLINE='.ASS'
      CALL WRTRED()
      RDLINE='UNI  40'
      CALL WRTRED()
      RDLINE='.LIB'
      CALL WRTRED()
C
      CALL REDHED()
C     output blank line
      RDLINE=' '
      CALL WRTRED()
C
C     write end of file marker.
      RDLINE='.EOD'
      CALL WRTRED()
C     finally close the file.
CAPOLLO|SUN
      CLOSE(UNIT=REDUNT,ERR=95)
      LFU(REDUNT)=.FALSE.
CAPOLLO|SUN
CIBM
C      CALL CLOSEF(REDUNT,ST)
CIBM
C
      RETURN
C
 95   CONTINUE
      I=82
      CALL DEPRNT(I)
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WREDAC()
C     ===================
C
      include 'include/menun.inc'
      include 'include/redboard.inc'
      include 'include/params.inc'
      include 'include/nbuff.inc'
C
      INTEGER TMEN,TCELL,C
      REAL X,Y
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     take defaults from REDBOARD.PCF file
      CALL WRRPCF()
C
 10   CONTINUE
C     Now activate the write redac minor menu
      CALL MNIWR1()
C     set up dsu value. (25 thou = 1 dsu)
      DSU=0.025*2.54/DBUFAC
      CALL DCPRNT(284)
C     Read a cursor hit to select READ mode
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.3) THEN
C       ensure menu cell is hilited
        CALL GTMCHI(TMEN,TCELL)
        IF       ( CCMD.EQ.'C' ) THEN
C*************************************************************
C         Redboard component output
          CALL WRED01()
        ELSE  IF ( CCMD.EQ.'G' ) THEN
C*************************************************************
C         off/on grid check
          CALL WREDOG()
        ELSE  IF ( CCMD.EQ.'A' ) THEN
C*************************************************************
C         Auto number of component
          CALL REDAN1()
        ELSE  IF ( CCMD.EQ.'B' ) THEN
C*************************************************************
C         Board output
          CALL WRTBD1()
        ELSE
C*************************************************************
C         unrecognized read option
          CALL DEPRNT(8)
C         set MEN to 0 to avoid infinite loop on unassigned cell
          MEN=0
        END IF
        IF (CCMD.EQ.'q') GOTO 99
C       ensure the entry cell is not hilited any more
        CALL GTMCLO(TMEN,TCELL)
C       if another major option,go check it out
        IF (MEN.EQ.2) GOTO 20
C       clear the minor option menu
        CALL GTCLRM(3)
        GOTO 10
      ELSE
         CALL DEPRNT(284)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WREDOG()
C     ===================
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/entity.inc'
C
      LOGICAL ON,OK,DRA
      INTEGER*2 ENTYPE,TMIP
      REAL X,Y,SNAPG,GSIZE,TOL,M1(3,3)
      EXTERNAL SNAPG
C
C     Tolerance=TOLVAL*INCH_FAC/DBUFAC,where TOLVAL in Inches
C     TOLVAL set to 5 thou (0.127mm)
      TOL=0.001*2.54/DBUFAC
C     set grid size to 25 thou (1 DSU)
C     GSIZE=SIZE*INCH_FAC/DBUFAC where size in inches
      GSIZE=0.025*2.54/DBUFAC
C     loop round the current display file.
      DO 30 TMIP=1,NMIPOS-1
        CALL DIR500(TMIP,OK)
        IF (  IMBUFF(1).NE.100.AND.IMBUFF(1).EQ.10 ) THEN
C         we have valid data.
          CALL ALLRD(TMIP,ENTYPE,M1,OK)
          IF ( IMBUFF(4).EQ.0.AND.ENTYPE.EQ.ARC )    THEN
C           layer 0
            X=SNAPG(RDBUFF(1),GSIZE,0.0)-RDBUFF(1)
            Y=SNAPG(RDBUFF(2),GSIZE,0.0)-RDBUFF(2)
            ON=SQRT(X*X+Y*Y).LT.TOL
            DRA=.FALSE.
            IF ( IMBUFF(3).EQ.2 .AND..NOT.ON ) THEN
C             colour 2 is on grid pads.
              IMBUFF(3)=3
              CALL DIM500(TMIP,OK)
              DRA=.TRUE.
            ELSE IF ( IMBUFF(3).EQ.3.AND.ON ) THEN
C             colour 3 is off grid pads.
              IMBUFF(3)=2
              CALL DIM500(TMIP,OK)
              DRA=.TRUE.
            END IF
            IF ( DRA )   CALL SPCDRW(ENTYPE,TMIP,.FALSE.,M1,.FALSE.)
          END IF
        END IF
 30   CONTINUE
 
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRRPCF()
C     ===================
C
C     read content of 'REDBOARD.PCF' and
C     set default conditions.
C
      include 'include/redboard.inc'
      include 'include/library.inc'
C
      INTEGER*4 PCFUNT,L,NLEN,P,N,ST
      LOGICAL OK
      CHARACTER*80 LINE,PARAM*80,PCFNAM
      EXTERNAL IVALU
C
C     test for existance of 'REDBOARD.PCF'
C     and read content
      LIBNAM='pcb.lib'
      AUXNAM='redcmp.cdi'
      NEWCMP=501
CAPOLLO|IBM
      PCFNAM='redboard.pcf'
CAPOLLO|IBM
CSUN
C        PCFNAM=HOME(1:NLEN(HOME))//'/.redboard.pcf'
CSUN
C
      INQUIRE(FILE=PCFNAM,EXIST=OK)
      IF (.NOT.OK) THEN
C       try global path
CAPOLLO|SUN
        PCFNAM='redboard.pcf'
        CALL FPARS(PCFNAM,ST)
CAPOLLO|SUN
CIBM
C        PCFNAM=LIBRARY(1:NLEN(LIBRARY))//'\redboard.pcf'
CIBM
        INQUIRE(FILE=PCFNAM,EXIST=OK)
        IF (.NOT.OK) THEN
          CALL DEPRNT(727)
          RETURN
        END IF
      END IF
C
      CALL OPNOFF(PCFNAM,PCFUNT,OK)
      IF (OK) THEN
C       read from .pcf file
 10     CONTINUE
        READ(UNIT=PCFUNT,FMT='(A)',ERR=91,END=99) LINE
        L=NLEN(LINE)
        IF (L.EQ.0) GOTO 10
        IF (LINE(1:1).EQ.'*') GOTO 10
        P=INDEX(LINE,'=')
        IF (P.LE.0) GOTO 10
        IF (L.GT.P) THEN
C         get parameter
          PARAM=LINE(P+1:L)
        ELSE
          GOTO 10
        END IF
        IF (LINE(1:P-1).EQ.'REDLIB') THEN
C         read library path
          LIBNAM=PARAM
        ELSE IF (LINE(1:P-1).EQ.'REDCMP') THEN
C         read aux component path
          AUXNAM=PARAM
        ELSE IF (LINE(1:P-1).EQ.'AUTOCMP') THEN
C         read start number for auto comp creation
          CALL IVALU(PARAM,N,OK)
          IF (OK) NEWCMP=N
        END IF
        GOTO 10
C
      ELSE
C       return with defaults set
        RETURN
      END IF
C
 99   CONTINUE
C     close file,and exit
      CALL CLOSUN(PCFUNT,.TRUE.,OK)
      RETURN
C
 91   CONTINUE
C     error reading pcf file
      CALL DEPRNT(728)
      GOTO 99
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRTBD1()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    controls operation of the WRITE redac function
C
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/redboard.inc'
      include 'include/lfu.inc'
C
      CHARACTER*3 SEQ,UNF
      LOGICAL YES
      INTEGER TMEN,TCELL,I
      INTEGER*4 OUNIT,NLEN
      INTEGER*2 ST
      EXTERNAL GTMCLO,NLEN
C
      TMEN=MEN
      TCELL=CELLN
C     enter the WRITE REDAC BOARD function
 100  CONTINUE
C     ask for filename for data
      CALL DPRMXP(81,CBUFF)
C     if null file name assume abort required
      IF ( NLEN(CBUFF).GT.0 ) THEN
C     open ascii file and write data to it
CAPOLLO|SUN
        CALL OPNFFA(CBUFF,REDUNT,YES)
        IF ( YES )  CALL WRTBD2()
        CLOSE(UNIT=REDUNT,STATUS='KEEP',ERR=95)
        LFU(REDUNT)=.FALSE.
CAPOLLO|SUN
CIBM
C        REDUNT=0
C        CALL CREATF(CBUFF,REDUNT,ST)
C        IF (ST.EQ.0) CALL WRTBD2()
C        CALL CLOSEF(REDUNT,ST)
CIBM
      END IF
C     ensure option  is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
      CCMD='q'
C
      RETURN
 95   CONTINUE
      I=82
      CALL DEPRNT(I)
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRTBD2()
C     ===================
C1    vartype
C1    iostatus
C
C2    Writes PCB BOARD data to the already opened
C2    PASCII format file connected to global
C2    unit REDUNT.
C2    Writes a .BOA section or sections containing
C2    board outline data,followed by a .LIB section
C2    and .IDX section containing the fixing hole
C2    components used for the board.
C
      include 'include/redboard.inc'
C
C     place .PCB header in PASCII file
      RDLINE='.PCB'
      CALL WRTRED()
      RDLINE='.ASS'
      CALL WRTRED()
      RDLINE='UNI 40'
      CALL WRTRED()
C     place null line in PASCII file
      RDLINE=' '
      CALL WRTRED()
C     write board outline data to PASCII file
      CALL WRTBOA()
C     write library compnent data to PASCII file
      CALL WRTLB9()
C     all done,return to caller
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRTBOA()
C     ===================
C1    vartype
C1    iostatus
C
C2    Writes all geometry found on layer 40
C2    to the open PASCII format file connected
C2    to global unit REDUNT.
C2    Creates multiple .BOA sections for unconnected
C2    geometry elements,each .BOA section being created
C2    from a series of connected line,or arc entities.
C
      include 'include/redboard.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/entity.inc'
      include 'include/lfu.inc'
C
      LOGICAL OK,FOUND,USEIT,STPA,NCD,FULCIR
      CHARACTER*3 NAME
      INTEGER ONUNIT,I,K,J,COUNT,RECN,LC,LASTC,FIRSTP,LASTP
      INTEGER*2 ASEGS,NSEG,USELM,TRECN,IC,STLP,WRSEG,WASEG
      INTEGER*2 ENTYPE,TMIP,P,NOARC,NOELM,NOLINE,NOCH
      REAL LX,LY,TOL,M1(3,3),CHORD,TANG,PI,PI2,R1,X1,Y1,X2,Y2
      REAL RANG,IANG,LDATA(2,2),RR(9),START(2),SANG,TRAD,ANG
      REAL OLDDSU(2),NEWDSU(2)
      EXTERNAL PI
C
C         scratch file structure for board
C No bytes     2   2  4  4  4  4 4 4  4  4  4
C  LINE      MIP,ENT,X1,Y1,X2,Y2,0 0  0  0  0
C  ARC       MIP,ENT, X, Y, R, S,E X1 Y1 X2 Y2
C
      CALL OURSCR(ONUNIT,48,OK)
C
      PI2=PI(2.0)
C     Tolerance=TOLVAL*INCH_FAC/DBUFAC,where TOLVAL in Inches
C     TOLVAL set to 1 thou (0.127mm)
      TOL=0.001*2.54/DBUFAC
C     no lines found
      NOLINE=0
C     no arcs found
      NOARC=0
C     no elements found
      NOELM=0
C     total chordal angle
      TANG=0
C     Total radius length
      TRAD=0
C     number of segments per arc
      NSEG=0
      USELM=0
C     loop round the current data base file.
      DO 30 TMIP=1,NMIPOS-1
        CALL DIR500(TMIP,OK)
        IF (IMBUFF(1).NE.100.AND.IMBUFF(1).EQ.10.AND.IMBUFF(4).EQ.40
     +   .AND.(IMBUFF(2).EQ.ARC.OR.IMBUFF(2).EQ.LINE)  ) THEN
C         we have valid data read the entity
          CALL ALLRD(TMIP,ENTYPE,M1,OK)
          NOELM=NOELM+1
          IF ( ENTYPE.EQ.LINE)  THEN
            NOLINE=NOLINE+1
            WRITE(UNIT=ONUNIT,REC=NOELM,ERR=98) TMIP,ENTYPE,
     +      RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),0.0,0.0,0.0,0.0,0.0
          ELSE IF (ENTYPE.EQ.ARC) THEN
            NOARC=NOARC+1
C           test for end angle zero
            IF (RDBUFF(6).EQ.0) RDBUFF(6)=PI2
C           calculate start and end points of arc
            X1=RDBUFF(1)+RDBUFF(4)*COS(RDBUFF(5))
            Y1=RDBUFF(2)+RDBUFF(4)*SIN(RDBUFF(5))
            X2=RDBUFF(1)+RDBUFF(4)*COS(RDBUFF(6))
            Y2=RDBUFF(2)+RDBUFF(4)*SIN(RDBUFF(6))
            WRITE(UNIT=ONUNIT,REC=NOELM,ERR=98) TMIP,ENTYPE,
     +      RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     +      X1,Y1,X2,Y2
            CHORD=RDBUFF(6)-RDBUFF(5)
            IF ( CHORD .LT.0.0 ) CHORD=PI2+CHORD
            TANG=TANG+CHORD
            TRAD=TRAD+RDBUFF(4)
          END IF
        END IF
 30   CONTINUE
C
C     set first position in scratch file above entity data
      FIRSTP=NOELM+1
      LASTP=FIRSTP
C     cannot limit board with one element
      IF ( NOELM.LT.1 ) GOTO 100
C     cannot create more than 62 segments in boundary
      IF ( NOELM.GT.61) GOTO 100
C
      IF ( NOARC.GT.0 ) THEN
C       calculate interpolation angle
        RANG=TANG/(60-NOLINE)
        RANG=MAX(RANG,PI2/16)
      END IF
C     number of segements that can be allocated to arcs
      ASEGS=62-NOLINE
C     weighted ratio of radius to amount of angle set at 50/50
      WASEG=ASEGS/2
      WRSEG=ASEGS/2
C     *****************************************
C     write board outline header to output file
C     *****************************************
      RDLINE='.BOA'
      CALL WRTRED()
C     set flags for start and direction link
      NCD=.FALSE.
      STPA=.TRUE.
C     set the recornumber back to 1
      RECN=1
92    CONTINUE
C     read the entity from the scratch file
      READ(UNIT=ONUNIT,REC=RECN,ERR=97)TMIP,ENTYPE,(RR(I),I=1,9)
C     If we are on a second pass then set the entity pointer back
      IF(NCD) TMIP=-TMIP
C     save the starting entity number.
      TRECN=RECN
      IF (ENTYPE.EQ.LINE) THEN
          DO 20 I=1,2
C           initialize data buffer
            IF(NCD) THEN
               LDATA(I,1)=RR(I+2)
C              initialize start position
               START(I)=RR(I+2)
            ELSE
               LDATA(I,1)=RR(I)
C              initialize start position
               START(I)=RR(I)
            ENDIF
 20       CONTINUE
          DO 21 I=1,2
            IF(NCD) THEN
C              initialize data buffer
               LDATA(I,2)=RR(I)
            ELSE
C              initialize data buffer
               LDATA(I,2)=RR(I+2)
            ENDIF
 21       CONTINUE
      ELSE IF (ENTYPE.EQ.ARC) THEN
          CHORD=RR(5)-RR(4)
          IF ( CHORD .LT.0.0 ) CHORD=PI2+CHORD
          DO 50 I=1,2
C           initialize data buffer
            IF(NCD) THEN
               SANG=RR(4)
               IANG=1
               LDATA(I,1)=RR(I+5)
               LDATA(I,2)=RR(I+7)
               START(I)=RR(I+5)
C              initialize start position
            ELSE
               SANG=RR(5)
               IANG=-1
               LDATA(I,1)=RR(I+7)
               LDATA(I,2)=RR(I+5)
C              initialize start position
               START(I)=RR(I+7)
            ENDIF
 50       CONTINUE
      ENDIF
C     update points for movement checking
      NEWDSU(1)=NINT(LDATA(1,2)/DSU)
      NEWDSU(2)=NINT(LDATA(2,2)/DSU)
      OLDDSU(1)=NINT(LDATA(1,1)/DSU)
      OLDDSU(2)=NINT(LDATA(2,1)/DSU)
C     discard this element,it has been used
      WRITE(UNIT=ONUNIT,REC=RECN,ERR=98)-TMIP,ENTYPE,(RR(I),I=1,9)
C     write first point to output
      IF(ENTYPE.EQ.LINE) THEN
C         save the pointer in case this has to be rewritten
          STLP=LASTP
          WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +          NINT(START(1)/DSU),NINT(START(2)/DSU)
          LASTP=LASTP+1
          WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +          NINT(LDATA(1,2)/DSU),NINT(LDATA(2,2)/DSU)
          LASTP=LASTP+1
C         update the used element counter
          USELM=USELM+1
C         update buffer position
          DO 22 I=1,2
            LDATA(I,1)=LDATA(I,2)
 22       CONTINUE
      ELSE IF(ENTYPE.EQ.ARC) THEN
C          Write out the first arc to the scratch file
           STLP=LASTP
           WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +          NINT(START(1)/DSU),NINT(START(2)/DSU)
           USELM=USELM+1
           LASTP=LASTP+1
C          calculate the number of segments relating to the
C          size of the angle and the radius.
           NSEG=MAX(1,INT(((RR(3)/TRAD)*WRSEG)+((CHORD/TANG)*WASEG)))
C          angle increment and direction
           ANG=(CHORD/NSEG)*IANG
C          point output loop
           DO 32 K=1,NSEG
C             accumulate angle
              SANG=SANG+ANG
C             calculate the point
              LDATA(1,1)=RR(1)+RR(3)*COS(SANG)
              LDATA(2,1)=RR(2)+RR(3)*SIN(SANG)
C             calculte the point in DSU.
              NEWDSU(1)=NINT(LDATA(1,1)/DSU)
              NEWDSU(2)=NINT(LDATA(2,1)/DSU)
C             has it moved ?
              IF((ABS(NEWDSU(1)-OLDDSU(1)).GT.0).AND.
     +           (ABS(NEWDSU(2)-OLDDSU(2)).GT.0)) THEN
C                 it moved from the last time, write it out
                  WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +            NINT(LDATA(1,1)/DSU),NINT(LDATA(2,1)/DSU)
                  LASTP=LASTP+1
C                 update the DSU points
                  OLDDSU(1)=NEWDSU(1)
                  OLDDSU(2)=NEWDSU(2)
             ENDIF
 32        CONTINUE
C          send last point on arc if needed from saved point
           NEWDSU(1)=NINT(LDATA(1,2)/DSU)
           NEWDSU(2)=NINT(LDATA(2,2)/DSU)
           IF((ABS(NEWDSU(1)-OLDDSU(1)).GT.0).OR.
     +        (ABS(NEWDSU(2)-OLDDSU(2)).GT.0)) THEN
              LDATA(1,1)=LDATA(1,2)
              LDATA(2,1)=LDATA(2,2)
C             write next point to output
              WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +        NINT(LDATA(1,2)/DSU),NINT(LDATA(2,2)/DSU)
              LASTP=LASTP+1
              OLDDSU(1)=NEWDSU(1)
              OLDDSU(2)=NEWDSU(2)
         END IF
      ENDIF
C
 88   CONTINUE
C     now get into main loop of linking data elements
C     set indicator flags
      FOUND=.FALSE.
      USEIT=.FALSE.
C     reset the record number
      RECN=0
 23   CONTINUE
C     increment the number
      RECN=RECN+1
      IF (RECN.LE.NOELM) THEN
        READ(UNIT=ONUNIT,REC=RECN,ERR=98)TMIP,ENTYPE,(RR(I),I=1,9)
C       has this entity been used before if so ignore and move on
        IF (TMIP.LE.0) GOTO 27
C       indicate entity available
        FOUND=.TRUE.
        IF (ENTYPE.EQ.LINE) THEN
C          test for match with last point
           IF ((ABS(RR(1)-LDATA(1,2)).LT.TOL) .AND.
     +         (ABS(RR(2)-LDATA(2,2)).LT.TOL)) THEN
C            have a match,use this entity
             DO 24 I=1,2
               LDATA(I,2)=RR(I+2)
 24          CONTINUE
             USEIT=.TRUE.
           ELSE IF ((ABS(RR(3)-LDATA(1,2)).LT.TOL) .AND.
     +           (ABS(RR(4)-LDATA(2,2)).LT.TOL)) THEN
C            have a match,use this entity
             DO 26 I=1,2
               LDATA(I,2)=RR(I)
 26          CONTINUE
             USEIT=.TRUE.
           END IF
        ELSE IF (ENTYPE.EQ.ARC) THEN
C         arc segment
C          test for match with last point
           IF ((ABS(RR(6)-LDATA(1,2)).LT.TOL) .AND.
     +         (ABS(RR(7)-LDATA(2,2)).LT.TOL)) THEN
C            have a match,use this entity
             IANG=1
C            set start angle for interpolation
             SANG=RR(4)
             CHORD=RR(5)-RR(4)
             IF ( CHORD .LT.0.0 ) CHORD=PI2+CHORD
             USEIT=.TRUE.
C            save end point of interpolation
             LDATA(1,2)=RR(8)
             LDATA(2,2)=RR(9)
           ELSE IF ((ABS(RR(8)-LDATA(1,2)).LT.TOL) .AND.
     +              (ABS(RR(9)-LDATA(2,2)).LT.TOL)) THEN
C            have a match,use this entity
             IANG=-1
C            set start angle for interpolation
             SANG=RR(5)
             CHORD=RR(5)-RR(4)
             IF ( CHORD .LT.0.0 ) CHORD=PI2+CHORD
             USEIT=.TRUE.
C            save end point of interpolation
             LDATA(1,2)=RR(6)
             LDATA(2,2)=RR(7)
           END IF
        ELSE
C         cannot cope with this entity
           GOTO 27
        END IF
        IF (USEIT) THEN
C         use this and discard from scratch file
          USELM=USELM+1
          STPA=.FALSE.
          NCD=.FALSE.
          WRITE(UNIT=ONUNIT,REC=RECN,ERR=98)-TMIP,ENTYPE,(RR(I),I=1,9)
          IF (ENTYPE.EQ.LINE) THEN
C            write next point to output only if it has actualy moved
             NEWDSU(1)=NINT(LDATA(1,2)/DSU)
             NEWDSU(2)=NINT(LDATA(2,2)/DSU)
             IF((ABS(NEWDSU(1)-OLDDSU(1)).GT.0).OR.
     +           (ABS(NEWDSU(2)-OLDDSU(2)).GT.0)) THEN
                 WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +           NINT(LDATA(1,2)/DSU),NINT(LDATA(2,2)/DSU)
                 LASTP=LASTP+1
C                update buffer position
                 DO 25 I=1,2
                     LDATA(I,1)=LDATA(I,2)
 25              CONTINUE
                 OLDDSU(1)=NEWDSU(1)
                 OLDDSU(2)=NEWDSU(2)
             ENDIF
          ELSE
C             must be arc
              NSEG=MAX(1,INT(((RR(3)/TRAD)*WRSEG)+((CHORD/TANG)*WASEG)))
              ANG=(CHORD/NSEG)*IANG
              DO 31 K=1,NSEG
                  SANG=SANG+ANG
                  LDATA(1,1)=RR(1)+RR(3)*COS(SANG)
                  LDATA(2,1)=RR(2)+RR(3)*SIN(SANG)
C                 write next point to output
                  NEWDSU(1)=NINT(LDATA(1,1)/DSU)
                  NEWDSU(2)=NINT(LDATA(2,1)/DSU)
                  IF((ABS(NEWDSU(1)-OLDDSU(1)).GT.0).AND.
     +               (ABS(NEWDSU(2)-OLDDSU(2)).GT.0)) THEN
                      WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +                NINT(LDATA(1,1)/DSU),NINT(LDATA(2,1)/DSU)
                      LASTP=LASTP+1
                      OLDDSU(1)=NEWDSU(1)
                      OLDDSU(2)=NEWDSU(2)
                 ENDIF
 31            CONTINUE
C              send last point on arc if needed from saved point
               NEWDSU(1)=NINT(LDATA(1,2)/DSU)
               NEWDSU(2)=NINT(LDATA(2,2)/DSU)
               IF((ABS(NEWDSU(1)-OLDDSU(1)).GT.0).OR.
     +            (ABS(NEWDSU(2)-OLDDSU(2)).GT.0)) THEN
                  LDATA(1,1)=LDATA(1,2)
                  LDATA(2,1)=LDATA(2,2)
C                 write next point to output
                  WRITE(UNIT=ONUNIT,REC=LASTP,ERR=98)
     +            NINT(LDATA(1,2)/DSU),NINT(LDATA(2,2)/DSU)
                  LASTP=LASTP+1
                  OLDDSU(1)=NEWDSU(1)
                  OLDDSU(2)=NEWDSU(2)
               END IF
          END IF
C         go try next entity
          USEIT=.FALSE.
          FOUND=.FALSE.
          GOTO 88
        ENDIF
 27     CONTINUE
        GOTO 23
      END IF
      IF(STPA) THEN
C         This indicates that the first point chosen cannot be linked
C         however we wil allow on more try
          STPA=.FALSE.
          NCD=.TRUE.
          USELM=USELM-1
          LASTP=STLP
          RECN=TRECN
          GOTO 92
      ENDIF
C     if we havnt used them all up yet keep going and its not
C     we ran of end of scratch file. Must have done all
C     entities or we have come to a discontinuty in the route
C     check for limit of 62 segments and limit to this
      IF (LASTP-FIRSTP.GT.62) LASTP=FIRSTP+61
C     as line string of board outline
      WRITE(UNIT=RDLINE,FMT='(A,I3)',ERR=100) 'L',LASTP-FIRSTP
      CALL WRTRED()
      DO 60 I=FIRSTP,LASTP-1
          READ(UNIT=ONUNIT,REC=I,ERR=97) J,K
          WRITE(UNIT=RDLINE,FMT='(I4,I5)',ERR=96)J,K
          CALL WRTRED()
 60   CONTINUE
C
 100  CONTINUE
C
      RDLINE='   '
      CALL WRTRED()
      LASTP=FIRSTP
C     if there are more elementys then go back and hunt them out
      IF(USELM.LT.NOELM) THEN
C        we will use this one as a start point
         NCD=.FALSE.
         STPA=.TRUE.
         DO 105 IC=1,NOELM
            READ(UNIT=ONUNIT,REC=IC,ERR=98)TMIP,ENTYPE,(RR(I),I=1,9)
            IF(TMIP.GT.0) THEN
               RECN=IC
               GOTO 92
            ENDIF
105      CONTINUE
C        Set the record to obtain start ponts of
      ENDIF
C
      LFU(ONUNIT)=.FALSE.
      CLOSE(UNIT=ONUNIT)
      RETURN
C
 96   CONTINUE
C     get here if error writing output file
      I=422
      CALL DEPRNT(I)
      GOTO 100
 97   CONTINUE
C     get here if error reading scratch file
      I=83
      CALL DEPRNT(I)
      GOTO 100
 98   CONTINUE
C     get here if error writing scratch file
      I=171
      CALL DEPRNT(I)
      GOTO 100
 99   CONTINUE
C     get here if cannot resolve geometry
      GOTO 100
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRTLB9()
C     ========================
C
C1    vartype             I4
C1    iostatus            IO
C
C2    Writes component references to the PASCII
C2    output file connected to REDUNT,if the component
C2    matches the spec of a fixing hole entity,which
C2    must exist on layer 40.If necessary,creates a
C2    unique component name for each incrementally.
C
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
      include 'include/redboard.inc'
C
      INTEGER*2 I,TENT,IX,IY,IOX,IOY,PCBN,LINDEX,N,NN,LI,OPCBN
      INTEGER*2 NEWNUM,NNEW,J,CDATA(4,2),DUMPAD,K,II,PCBN2,ST
      INTEGER*4 CNUM,LOCAL,LREF,LIBN,KK,CCNT,AUXUNT
      INTEGER IOST
      LOGICAL OK,OG
      REAL M(3,3),R(9),OFFX,OFFY
      CHARACTER*80 CNAM,CLINE*12
      EXTERNAL DIR500,DER566,DECODM
C
C     set default for dummy pad
      DUMPAD=1
C     set start number for new component generation
      NEWNUM=NEWCMP
C     open index file
      CALL OURSCR(IDXUNT,88,OK)
D      WRITE(10,*)'[WRTLB9] IDXUNT=',IDXUNT
C     open auxiliary file
      CALL OPNFFF(AUXNAM,AUXUNT,OK)
D      WRITE(10,*)'[WRTLB9] AUXUNT=',AUXUNT
C     write header to aux file
      WRITE(UNIT=AUXUNT,FMT='(A)',ERR=99) '.PCB'
      WRITE(UNIT=AUXUNT,FMT='(A)',ERR=99) '.ASS'
      WRITE(UNIT=AUXUNT,FMT='(A)',ERR=99) 'UNI 40'
      WRITE(UNIT=AUXUNT,FMT='(A)',ERR=99) '.LIB'
      WRITE(UNIT=AUXUNT,FMT='(A)',ERR=99) ' '
C     initialize new component creation number
      NNEW=0
      CCNT=1
D      WRITE(10,*) 'WRITE SWINDU,CCNT',SWINDU,CCNT
      LINDEX=0
      LREF=1
C     initialize component number for unique id
      CNUM=0
C     search for component instance only
      TENT=COMPI
C     search complete database if necessary
      DO 10 I=1,NMIPOS-1
        CALL DIR500(I,OK)
C       test for matching condition
        IF (IMBUFF(1).NE.100 .AND. IMBUFF(2).EQ.TENT
     +      .AND. IMBUFF(4).EQ.40 ) THEN
C         go get all data for this component
          CALL DER566(I,M,OK)
C         decode transform
          CALL DECODM(M,R)
C         write component ref to PASCII
          RDLINE=' '
C         get component position in DSU
          IX=NINT(R(1)/DSU)
          IY=NINT(R(2)/DSU)
C         find off-grid
          OFFX=(R(1)/DSU)-IX
          OFFY=(R(2)/DSU)-IY
C         get offset in thou for off-grid
          IOX=NINT(OFFX*DSU*1000.0)
          IOY=NINT(OFFY*DSU*1000.0)
C
C         extract component id number
          IF (CBUFF(1:1).EQ.'L' .OR. CBUFF(1:1).EQ.'l') THEN
C           get component number
            CALL IVALU(CBUFF(2:),KK,OK)
            IF (.NOT.OK) THEN
C             not a valid redac component
              CALL DEPRNT(710)
              GOTO 9
            END IF
C           copy to 2-byte integer
            PCBN=KK
C           set logical for off-grid component
            OG=IOX.NE.0 .OR. IOY.NE.0
C
            IF (OG) THEN
C             must get new component number
 40           CONTINUE
              OPCBN=PCBN
              PCBN=NEWNUM
              NEWNUM=NEWNUM+1
C             keep count of new components
              NNEW=NNEW+1
            ELSE
C             make related pcb number same as oroginal
              OPCBN=PCBN
            END IF
C
C           test for existance of compin local index
            J=0
 50         CONTINUE
            J=J+1
            IF (J.LE.LINDEX) THEN
C             read index,and test for match
              READ (UNIT=IDXUNT,REC=J,ERR=94,IOSTAT=IOST)OK,N,NN,CNAM
C             keep trying if no match
              IF (N.NE.PCBN) GOTO 50
C             must have found match,set local index pointer
              LI=J
            ELSE
C             does not exist in index
C             add to index file
C             point to next record in index
              LINDEX=LINDEX+1
              WRITE(UNIT=IDXUNT,REC=LINDEX,ERR=96,IOSTAT=IOST)
     +          OG,PCBN,OPCBN,CBUFF
C             save local index
              LI=LINDEX
            END IF
          ELSE
C           not a redac component
            CALL DEPRNT(710)
            GOTO 9
          END IF
C
C         write comp data for examination
D      WRITE(10,*) 'WRITE SWINDU,CCNT',SWINDU,CCNT
          WRITE(UNIT=SWINDU,REC=CCNT,ERR=97,IOSTAT=IOST)
     +      LI,PCBN,IX,IY,IOX,IOY,I
          CCNT=CCNT+1
        END IF
C
  9     CONTINUE
 10   CONTINUE
C
      IF (NNEW.GT.0) THEN
C       tell user new components created
        CNAM=' '
        WRITE(UNIT=CNAM,FMT='(A,I3,A)')'Total of',NNEW,
     +        ' Off-Grid Components created'
        CALL CPRINT(CNAM)
      END IF
 
C     **********************************************
C     ****    WRITE LOCAL LIBRARY DEFINITIONS   ****
C     **********************************************
C     write library section to output
      RDLINE=' '
      CALL WRTRED()
      RDLINE='.LIB'
      CALL WRTRED()
      DO 22 J=1,LINDEX
        READ(UNIT=IDXUNT,REC=J,ERR=94,IOSTAT=IOST)OG,PCBN,OPCBN,CNAM
        IF (.NOT.OG) THEN
C         offset in index by 1
          N=J-1
C         write normal component definition to file
          CALL WRTLB1(N,PCBN,REDUNT,LIBNAM,ST)
        ELSE
C         off-grid construction needed
C         go get original component data
          CALL WRTLB8(OPCBN,CDATA,LIBNAM,ST)
          IF (ST.EQ.0) THEN
C           create comp definition
            RDLINE=' '
            WRITE(UNIT=RDLINE,FMT='(A,I3,2I6,I4)')
     +      'L',J-1,(CDATA(I,1),I=2,4)
            CALL WRTRED()
            RDLINE=' '
            WRITE(UNIT=RDLINE,FMT='(2I6,I4)')
     +      CDATA(1,2),CDATA(2,2),DUMP  AD
            CALL WRTRED()
            RDLINE=' '
            CALL WRTRED()
C           find offset data from swind file
            K=0
 60         CONTINUE
            K=K+1
            IF (K.LE.CCNT) THEN
              READ(UNIT=SWINDU,REC=K,ERR=95,IOSTAT=IOST)
     +                  N,PCBN2,IX,IY,IOX,IOY,II
              IF (PCBN.EQ.PCBN2) THEN
C               write definition to aux file
                WRITE(UNIT=AUXUNT,FMT='(A,I3,2I6,I4)')
     +          'L',PCBN,(CDATA(I,1),I=2,4)
                WRITE(UNIT=AUXUNT,FMT='(2I6,I4)')
     +          CDATA(1,2),CDATA(2,2),DUMPAD
                WRITE(UNIT=AUXUNT,FMT='(A)')'OFF'
                WRITE(UNIT=AUXUNT,FMT='(2I6,I4)')IOX,IOY,CDATA(3,2)
                WRITE(UNIT=AUXUNT,FMT='(A)')' '
              ELSE
C               try next for match
                GOTO 60
              END IF
            ELSE
C             cannot match list to index ,god knows why!
C             better tell him some story,and sneek out
C             the back way
              CALL DEPRNT(711)
            END IF
          ELSE
C           could not find comp in lib file
            CALL DEPRNT(712)
          END IF
        END IF
 22   CONTINUE
C
      WRITE(UNIT=AUXUNT,FMT='(A)')'.EOD'
      WRITE(UNIT=AUXUNT,FMT='(A)')' '
      CALL CLOSUN(AUXUNT,.TRUE.,OK)
C
C     **********************************************
C     ****    WRITE LOCAL LIBRARY INDEX         ****
C     **********************************************
C     write library index to output
      RDLINE=' '
      CALL WRTRED()
      RDLINE='.IDX'
      CALL WRTRED()
      DO 23 J=1,LINDEX
        READ(UNIT=IDXUNT,REC=J,ERR=94,IOSTAT=IOST)OG,PCBN,OPCBN,CNAM
        RDLINE=' '
        IF (.NOT.OG) THEN
C         write normal index record
          WRITE(UNIT=RDLINE,FMT='(A,I4,I6)') 'L',J-1,PCBN
        ELSE
C         off-grid construction needed
          WRITE(UNIT=RDLINE,FMT='(A,I4,I6,A)') 'L',J-1,PCBN,' O'
        END IF
        CALL WRTRED()
 23   CONTINUE
C
C     close index unit now
      CALL CLOSUN(IDXUNT,.FALSE.,OK)
C
C     **********************************************
C     ****  WRITE LOCAL COMPONENT REFERENCE     ****
C     **********************************************
C     write component references to output
      RDLINE=' '
      CALL WRTRED()
      RDLINE='.COM'
      CALL WRTRED()
      DO 24 J=1,CCNT-1
        READ(UNIT=SWINDU,REC=J,ERR=95,IOSTAT=IOST)
     +      N,PCBN,IX,IY,IOX,IOY,I
        CNUM=CNUM+1
        CLINE=' '
        WRITE(UNIT=CLINE,FMT='(A,I3)') 'F',CNUM
        CALL CRUNCH(CLINE)
        WRITE(UNIT=RDLINE,FMT='(A5,3I3,A2,I3,I2,I2,2I6)',ERR=99)
     +        CLINE(1:5),0,0,0,' L',N-1,0,0,IX,IY
        CALL WRTRED()
 24   CONTINUE
C
C     write trailer to pascii file
      RDLINE=' '
      CALL WRTRED()
      RDLINE='.EOD'
      CALL WRTRED()
      RDLINE=' '
      CALL WRTRED()
      RETURN
C
 94   CONTINUE
      CALL DEPRNT(713)
      GOTO 101
 95   CONTINUE
      CALL DEPRNT(714)
      GOTO 101
 96   CONTINUE
      CALL DEPRNT(715)
      GOTO 101
 97   CONTINUE
      CALL DEPRNT(716)
      GOTO 101
 98   CONTINUE
      CALL DEPRNT(717)
      GOTO 101
 99   CONTINUE
      CALL DEPRNT(718)
 101  CONTINUE
      CALL CLOSUN(AUXUNT,.TRUE.,OK)
      CALL CLOSUN(IDXUNT,.FALSE.,OK)
D      WRITE(10,*)'[WRTLB9] IOSTAT=',IOST
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE WRTRED()
C     ===================
C
      include 'include/redboard.inc'
C
      INTEGER NLEN1
      INTEGER*2 ST
      EXTERNAL NLEN1
C
CAPOLLO|SUN
      WRITE(UNIT=REDUNT,FMT='(A)') RDLINE(1:NLEN1(RDLINE))
CAPOLLO|SUN
CIBM
C      CALL WRITFS(RDLINE,REDUNT,ST)
CIBM
C
      END
C
C     --------------------------------------------------
C
