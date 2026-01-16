C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 read.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE  FNAME(FILNM,ST)
C     SUBROUTINE GERBTL(FILNM,SCALE,ST)
C     SUBROUTINE GETVAL(STRING,C,IVAL,ST)
C     SUBROUTINE GPARAM(COD,BEND,FIL,PRE,SUC,ZER,REP
C     SUBROUTINE MAJRED()
C     SUBROUTINE MNIRED()
C     SUBROUTINE NWORD(WORD,WL,DELIM,EOS)
C     SUBROUTINE RDBTST()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE  FNAME(FILNM,ST)
C     ===========================
 
      INTEGER*4 ST,NLEN
      CHARACTER*(*) FILNM
      LOGICAL OK,OK2
      EXTERNAL NLEN
 
      ST=0
 10   CONTINUE
C     go get a file name for reading
      CALL DPRMXP(81,FILNM)
      IF (NLEN(FILNM).EQ.0 ) GOTO 99
      INQUIRE(FILE=FILNM,EXIST=OK)
      CALL CHKNAM(FILNM,OK2)
      OK=OK2.AND.OK
      IF (.NOT.OK) THEN
C        cannot find the file
         CALL DEPRNT(278)
         GOTO 10
      END IF
 
      RETURN
 99   CONTINUE
      ST=1
 
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE GERBTL(FILNM,SCALE,ST)
C     =================================
C
      include'include/gerber.inc'
      include'include/redscl.inc'
      include'include/redboard.inc'
      include'include/lfont.inc'
      include'include/lfu.inc'
C
      CHARACTER*100 RECORD,FILNM*(*),
     +              APSIZE*20,FORM*20,BEND*1,FIL*1
      LOGICAL OK,EOS
      INTEGER*4 ST,II,RECNO,NLEN,GUNIT,END,
     +          INDX,OVER,WL,STAT
      INTEGER*2 COD,PRE,SUC,ZER,REP,CRLF,VAR,BLSZ,CROSS
      REAL SCALE
      EXTERNAL NLEN
C
C     Set the default values for assembler routines
      COD=0
      BEND=CHAR(42)
      FIL=CHAR(0)
      PRE=2
      SUC=3
      ZER=1
      REP=0
      CRLF=1
      VAR=1
      BLSZ=0
      CROSS=0
      AUXSCL=1.0
C
      ST=0
      OVER=0
      DO 5 II=1,100
         TOLSET(II,1)=3
         TOLSET(II,2)=90
         TOLTYP(II)='D'
 5    CONTINUE
C
      CALL FINDU1(GUNIT,OK)
      IF ( .NOT.OK ) THEN
         ST=2
         CALL DEPRNT(637)
      END IF
C      WRITE(10,*) '[GERBTL] FILNM = ',FILNM
      OPEN(UNIT=GUNIT,FILE=FILNM,STATUS='OLD',ERR=99)
CIBM
C      LFU(GUNIT)=.TRUE.
CIBM
      REWIND(UNIT=GUNIT)
 
      RECNO=0
      OPT=0
C
 10   READ( UNIT=GUNIT,FMT='(A)',END=20,ERR=103,IOSTAT=STAT)RDLINE
      CURP=1
      EOFL=NLEN(RDLINE)
      IF(EOFL.EQ.0) GOTO 10
C     now remove the first feild
      CALL NWORD(RECORD,WL,' ',EOS)
C     read through data file to the top of the definition table.
c-----------------------------------------------
      IF ( RECORD(1:5).EQ.'CODES') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'ASCII') COD=0
           IF ( RECORD(1:WL).EQ.'EBCIDIC') COD=1
           IF ( RECORD(1:WL).EQ.'ISO') COD= 2
           IF ( RECORD(1:WL).EQ.'EIA') COD=3
        END IF
c-----------------------------------------------
C     now set PRE with the number of preceding digits
      ELSE IF ( RECORD(1:9).EQ.'PRECEDING') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        CALL IVALU(RECORD,II,OK)
        IF (OK) THEN
            PRE=II
        END IF
c-----------------------------------------------
C     now set SUC with the number of succeeding digits
      ELSE IF ( RECORD(1:10).EQ.'SUCCEEDING') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        CALL IVALU(RECORD,II,OK)
        IF (OK) THEN
            SUC=II
        END IF
c-----------------------------------------------
C     now set ZER with the code for leading zeros
      ELSE IF ( RECORD(1:4).EQ.'ZERO') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'TRAILING') ZER=1
           IF ( RECORD(1:WL).EQ.'LEADING') ZER=2
           IF ( RECORD(1:WL).EQ.'KEEP') ZER=3
        END IF
c-----------------------------------------------
C     now set CRLF with the code for carriage line feed
      ELSE IF ( RECORD(1:4).EQ.'CRLF') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'INCLUDE') CRLF=1
           IF ( RECORD(1:WL).EQ.'OMIT') CRLF=0
        END IF
c-----------------------------------------------
C     now set BEND with the character for an end of line
      ELSE IF ( RECORD(1:8).EQ.'ENDBLOCK') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        CALL IVALU(RECORD,II,OK)
        IF (OK) THEN
            BEND=CHAR(II)
        END IF
c-----------------------------------------------
      ELSE IF ( RECORD(1:11).EQ.'COORDINATES') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'INCREMENTAL') OPT=OPT+2
        END IF
c-----------------------------------------------
      ELSE IF ( RECORD(1:5).EQ.'UNITS') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'METRIC') AUXSCL=25.4
           IF ( RECORD(1:WL).EQ.'IMPERIAL') AUXSCL=1.0
        END IF
c-----------------------------------------------
      ELSE IF ( RECORD(1:7).EQ.'REPEATS') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( RECORD(1:WL).EQ.'OMIT')   REP=0
           IF ( RECORD(1:WL).EQ.'INCLUDE')   REP=1
        END IF
c-----------------------------------------------
      ELSE IF ( RECORD(1:9).EQ.'GCOMMANDS') THEN
        CALL NWORD(RECORD,WL,' ',EOS)
        IF (WL.GT.0) THEN
           IF ( INDEX(RECORD,'OMIT').GT.0) OPT=OPT+4
        END IF
      END IF
C
c===============================================================
c---   Finished reading the format now read tool definition ----
c---------------------------------------------------------------
      IF ( RECNO.EQ.0.AND.
     +     RECORD(1:8).NE.'POSITION' ) THEN
         GOTO 10
      END IF
      RECNO=RECNO+1
      IF ( RECNO.EQ.1 )THEN
           GOTO 10
      END IF
C      IF ( NLEN(RECORD).EQ.0 ) GOTO 20
      IF(RECORD(1:4).EQ.'USER') GOTO 20
C
c     read tool number
      CALL IVALU(RECORD,ST,OK)
      TOOLNO=ST
C     check that it is within range
      IF ( TOOLNO.LT.10.OR.TOOLNO.GT.100 ) GOTO 102
C     read apeture shape 1-Round,2-Square,3-Annular.
      CALL NWORD(RECORD,WL,' ',EOS)
      CALL IVALU(RECORD,II,OK)
      IF ( II.GE.0 )  THEN
         TOLSET(TOOLNO,1)=II
C        store the apeture usage ie. F-flash D-draw B-Both.
         CALL NWORD(RECORD,WL,' ',EOS)
C        save the character in toltyp array
         IF (WL.EQ.1) TOLTYP(TOOLNO)=RECORD(1:1)
C        Extract apeture size in 1000 of inch.
         CALL NWORD(RECORD,WL,' ',EOS)
         IF(WL.GT.0.AND. TOOLNO.GT.0) THEN
           CALL IVALU(RECORD,ST,OK)
           TOLSET(TOOLNO,2)=ST
         END IF
C        only got 99 thickness so don't let it go out of bounds
         IF ( TOOLNO.LT.1.OR.TOOLNO.GT.99 ) THEN
            CALL DEPRNT(638)
            GOTO 10
         END IF
C        remap finger to our square definition.
         IF ( TOLSET(TOOLNO,1).EQ.0 ) END=1
C        remap round to our round definition.
         IF ( TOLSET(TOOLNO,1).EQ.1 ) END=2
C        remap square to our square
         IF ( TOLSET(TOOLNO,1).EQ.2 ) END=1
C        map annular onto round
         IF ( TOLSET(TOOLNO,1).EQ.3 ) END=2
C        creat control word for thickness
C Byte   2        1
C        5432109876543210
C Toolno       **********
C End       ***
C Over   ***
C        3184215216318421
C        261000152426
C        739942268
C        682684
C        84
C
         INDX=OVER*8192+END*1024+TOOLNO
C        store control word for thickness
         PLTHKI(TOOLNO)=INDX
C        store thickness in mm
         LTHKR(1,TOOLNO)=25.4*TOLSET(TOOLNO,2)/1000.0
C        if a flash we use circle so define thickness
C        as negative of actual.
         IF ( TOLTYP(TOOLNO).EQ.'F' )
     +   LTHKR(1,TOOLNO)=-LTHKR(1,TOOLNO)
         LTHKR(1,TOOLNO)=SCALE*LTHKR(1,TOOLNO)
 
C        standard pen diameter
         LTHKR(2,TOOLNO)=0.3
      END IF
      GOTO 10
 20   CONTINUE
c     now call a fortran routine which emulates the PC assembler routine
C     that sets the format for all input and output of gerber files
C     ======   NOT fully implimented ============
      CALL GPARAM(COD,BEND,FIL,PRE,SUC,ZER,REP,CRLF,VAR,BLSZ,CROSS)
      ST=0
      CLOSE(UNIT=GUNIT)
CIBM
C      LFU(GUNIT)=.FALSE.
CIBM
      RETURN
  99  WRITE(10,*) 'Error opening file',FILNM
      ST=1
      RETURN
 100  WRITE(10,'(A)') 'Error reading tool no:',RECNO
      ST=1
      RETURN
 101  WRITE(10,'(A)') 'Error reading tool shape:',RECNO
      ST=1
      RETURN
 102  WRITE(10,'(A)') 'Error reading tool  usage',RECNO
      ST=1
      RETURN
 103  WRITE(10,'(A)') 'Error reading gb6000 line',STAT
      ST=1
      RETURN
      END
 
      SUBROUTINE GETVAL(STRING,C,IVAL,ST)
C     ===================================
C
C1    vartype            C*(*) C1  I4  I4
C1    iostatus             I   I   O   O
C
C2    Subroutine GETVAL tests STRING for existance of
C2    the character C,and extracts if possible the first numeric
C2    value follwing the check character.The integer value is
C2    returned in IVAL and completion status in ST.
C
      INTEGER*4 IVAL,ST,P1,P2,P3,VRFY,IFORM
      CHARACTER*(*) STRING,C*(*),ISTRNG*12,FORM*12,CTMP*20
      PARAMETER (ISTRNG='0123456789+-')
      INTRINSIC INDEX
      EXTERNAL VRFY
C
C     initialize format for reading of integer data
C     find position of check character first
      P1=INDEX(STRING,C)
      IF (P1.EQ.0) THEN
         ST=1
         RETURN
      END IF
C
C     find numeric follwing check character
C     point past check char first
      P2=P1+LEN(C)
      P1=P1+LEN(C)-1
      P3=VRFY(STRING(P2:),ISTRNG)
      IF (P3.EQ.1) THEN
         ST=2
         RETURN
      END IF
C
C     find absolute position of last numeric char
      P3=P1+P3-1
C     now P2:P3 identifies the numeric limits in the string
      IFORM=P3-P2+1
      CTMP=STRING(P2:P3)
      WRITE(UNIT=FORM,FMT='(A,I2,A)')'(I',IFORM,')'
      READ(UNIT=CTMP,FMT=FORM) IVAL
      ST=0
C
      END
C
C     ----------------------------------------------------------
C
      SUBROUTINE GPARAM(COD,BEND,FIL,PRE,SUC,ZER,REP
     +                   ,CRLF,VAR,BLSZ,CROSS)
C     ==============================================
C     subroutine to handle the format of a gerber tool file
c
      include'include/redscl.inc'
      CHARACTER*(*) BEND,FIL
      INTEGER*2 COD,PRE,SUC,ZER,REP,CRLF,VAR,BLSZ,CROSS,NSUC
      REAL RSUC
C     deals with the number of succeeding places
C
      RSUC=SUC
      AUXSCL=AUXSCL*10**RSUC
D      WRITE(10,*) 'AUXSCL= ',AUXSCL,RSUC
      END
 
 
 
      SUBROUTINE MAJRED()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the READ mode
C2    of operation is selected from the master menu.
C2    controls operation of the CHANGE function
C
CIBM
C      INCLUDE '\INCLUDE\VERTYPE.INC'
CIBM
CPC386
C      INCLUDE 'INCLUDE\\VERTYPE.INC'
CPC386
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/viewport.inc'
      include 'include/pwmgr.inc'
      include 'include/krap.inc'
C                               
      CHARACTER*80  RESP
      INTEGER CP,C,TMEN,TCELL,ICHAR
      INTEGER*4  DNUM
      REAL X,Y
      LOGICAL AUTFLG
      EXTERNAL MNIRED,GTCLRM,UNFLAG,CHKPRD
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,DEPRNT
C
CIBM|PC386
C      IF(RFIDF.NE.4.20.OR.RFIDF2.NE.2.46) RETURN
CIBM|PC386
C     Now activate the READ major option menu
      CALL MNIRED()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C                      
C     If viewports active ensure they are turned off
      IF (MVPACT) THEN          
 5        CONTINUE
          DNUM = 768
          CALL DPRMXP(DNUM,RESP)
          IF ((RESP(1:1).EQ.'Y').OR.(RESP(1:1).EQ.'y')) THEN
C             Clear the viewports so only left with the backcloth
              CALL VPCLR()
          ELSE IF ((RESP(1:1).EQ.'N').OR.(RESP(1:1).EQ.'n')) THEN
C             Take user back to main menu
              DNUM = 769
              CALL DEPRNT(DNUM)   
C             Jump to end of routine
              GOTO 99
          ELSE
              GOTO 5
          ENDIF
      ENDIF

 10   CONTINUE
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
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF       ( CCMD.EQ.'S' ) THEN
C*************************************************************
             CALL CHKPRD(MIFCODE, USELEV, AUTFLG)
             IF (AUTFLG .OR. KDEMO) THEN
                 CALL REDMIF()
             ELSE
                 CALL    ERRORDIALOG(
     +           'READ M.I.F. option not authorised')
             MEN=0
             ENDIF
         ELSE  IF ( CCMD.EQ.'T' ) THEN
C*************************************************************
C           Read HPGL plot files
             CALL CHKPRD(HPGCODE, USELEV, AUTFLG)
             IF (AUTFLG .OR. KDEMO) THEN
                 CALL REDHP0()
             ELSE
                 CALL    ERRORDIALOG(
     +           'READ HPGL option not authorised')
             MEN=0
             ENDIF
         ELSE  IF ( CCMD.EQ.'U' ) THEN
C*************************************************************
C           Read Gerber plot files
             CALL CHKPRD(GERCODE, USELEV, AUTFLG)
             IF (AUTFLG .OR. KDEMO) THEN
                 CALL REDHP0()
             ELSE
                 CALL    ERRORDIALOG(
     +           'READ GERBER option not authorised')
             MEN=0
             ENDIF
         ELSE  IF ( CCMD.EQ.'V' ) THEN
C*************************************************************
C           Read line data
            CALL RDBTST()
         ELSE  IF ( CCMD.EQ.'W' ) THEN
C*************************************************************
C           read MOSS data file,
             CALL CHKPRD(MIFCODE, USELEV, AUTFLG)
             IF (AUTFLG .OR. KDEMO) THEN
                 CALL REDGI0()
             ELSE
                 CALL    ERRORDIALOG(
     +           'READ GENIO  option not authorised')
             MEN=0
             ENDIF
         ELSE IF (CCMD.EQ.'D') THEN
C*************************************************************
C           dxf file format output for AutoCAD
             CALL CHKPRD(DXFCODE, USELEV, AUTFLG)
             IF (AUTFLG .OR. KDEMO) THEN
                 CALL RDXF00()
             ELSE
                 CALL    ERRORDIALOG(
     +           'READ DXF  option not authorised')
             MEN=0
             ENDIF
         ELSE
C*************************************************************
C           unrecognized read option
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
         IF (MEN.EQ.2) GOTO 20
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
      RETURN
      END
C
C
      SUBROUTINE MNIRED()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major READ options.
C2
      EXTERNAL GTDMEN,GTCLRM,MNLINS,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C     Clear the major option menu.
      CALL GTCLRM(2)
C     Enter the READ major option.
      CALL GTDMHD(25,2)
C
C     Load the nouns into major option menu.
C     British Marine Technology Splines Data
      CALL GTDMEN(306,2)
C     HPGL plotter file format
      CALL GTDMEN(302,2)
C     GERBER plotter file format
      CALL GTDMEN(303,2)
C     GENIO 'MOSS' system format
      CALL GTDMEN(307,2)
C     DXF output file format
      CALL GTDMEN(473,2)
 
C
      END
 
 
*
      SUBROUTINE NWORD(WORD,WL,DELIM,EOS)
C     ===================================
C1    vartype           C*80 I4 C*(*) L
C1    iostatus           O    O   I    O
C
C2    Subroutine XWORD0 extracts the next word from the current
C2    macro-program line,using the global scan pointer as
C2    the start point for the search,and updating the scan
C2    pointer to the end of the word found.The word is returned
C2    in WORD,it's length in WL,and the logical EOS is TRUE
C2    if the last word in the line.
C2    Spaces are used as delimiters.
C
      include 'include/redboard.inc'
C
      INTEGER*4 WL,P,P1,P2
      LOGICAL EOS
      CHARACTER*(*) WORD,DELIM*(*)
C
C     set dynamic pointer
      P=CURP
      WL=0
      WORD=' '
C
C      WRITE(10,*)'[NWORD] RDLINE,CURP ',RDLINE,CURP
 10   CONTINUE
      EOS=P.GT.EOFL
C     reached end of buffer,and no word
      IF ( EOS ) RETURN
      IF (RDLINE(P:P).EQ.' ') THEN
C        blank,try next position
         P=P+1
         GOTO 10
      END IF
      EOS=P.EQ.EOFL
C     found start of word,maybe
      P1=P
      P2=P
C     test for quotes
 20   CONTINUE
      EOS=P2.GT.EOFL
      IF ( EOS ) GOTO 30
      IF ( RDLINE(P2:P2).NE.DELIM) THEN
C        not a delimiter,try next position
         P2=P2+1
         GOTO 20
      END IF
      EOS=P2.EQ.EOFL
C     point to end of char string not to delimiter
C     return the string found
 30   WORD=RDLINE(P1:P2-1)
      WL=P2-P1
C     set scan pointer to delimiter
      CURP=P2
      END
*
*
C
      SUBROUTINE RDBTST()
C     ===================
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/lfu.inc'
C
      CHARACTER*128 FILNM
      REAL X(2),Y(2),PI
      INTEGER*4  BMUNIT,ST,TYP
      INTEGER*2 TMIP1,LN
      LOGICAL OK
      EXTERNAL PI
C
C     open the IGES data file
      CALL FNAME(FILNM,ST)
      IF ( ST.NE.0 ) THEN
         MEN=0
         RETURN
      END IF
C
      CALL FINDU1(BMUNIT,OK)
      IF ( .NOT.OK ) THEN
         CALL DEPRNT(420)
      END IF
      OPEN(UNIT=BMUNIT,FILE=FILNM,STATUS='OLD')
CIBM
C      LFU(BMUNIT)=.TRUE.
CIBM
 10   CONTINUE
      READ(UNIT=BMUNIT,END=20,FMT=*)
     +   TYP,LN,X(1),Y(1),X(2),Y(2)
      IF( TYP.EQ.3) THEN
         CALL DEWC03(X(1),Y(1),X(2),Y(2),CLFONT,
     +          LN,TMIP1,OK)
      ELSE IF ( TYP.EQ.5 ) THEN
         CALL DEWC05(X(1),Y(1),X(2),0.0,PI(2.0),CLFONT,
     +          LN,TMIP1,OK)
      END IF
      GOTO 10
 20   CONTINUE
      CLOSE(UNIT=BMUNIT)
CIBM
C      LFU(BMUNIT)=.FALSE.
CIBM
 
      END
C
C     ----------------------------------------------------
C
