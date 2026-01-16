C
C     @(#)  412.1 date 6/11/92 xconversion.f 
C
C
C     Filename    : xconversion.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:47:39
C     Last change : 92/06/11 14:44:23
C
C     Copyright : Practical Technology Limited  
C     File :- xconversion.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION PENCOL(KOLOUR)
C     SUBROUTINE LODTOK(LOCKED,FILNAM,ST)
C     SUBROUTINE DMETER(PCENT)
C     SUBROUTINE METERC()
C     SUBROUTINE ALLRD(TMIP,ENT,M,DELETE)
C     SUBROUTINE MAC020(OK)
C     SUBROUTINE MAC082(OK,EOF)
C     SUBROUTINE MAC060(OK)
C     SUBROUTINE CHGC03(ALL,USESW,MIPP,SYMBOL,NAME,CHKSTR,OK)
C     SUBROUTINE FOLDUP(STRING)
C     SUBROUTINE UNFOLD(STRING)
C     SUBROUTINE TCURS(C,X,Y)
C     SUBROUTINE STINIT()
C     SUBROUTINE CLOSWF(OK)
C     SUBROUTINE PAPCHK()
C     SUBROUTINE DELL02(ENT,ALL)
C     SUBROUTINE DIMD18(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     SUBROUTINE ALLDRW(ENT,TMIP)
C     SUBROUTINE CPRINT(STRING)
C     SUBROUTINE EPRINT(STRING)
C     SUBROUTINE CFONT()
C     SUBROUTINE CHANGETEXTPAT(OLDPAT,NEWPAT,ST)
C     SUBROUTINE SETSYSTEMVAR(NAME,VALUE)
C     SUBROUTINE PATREP1(STRING,C1,C2,BEGIN,OK)
C
C     |-----------------------------------------------------------------|
C
C
C
C     This source file contains routines which have been modified
C     for the X conversion for daxcad. At the head of each
C     routine a comment exists to nindicate the source file form whence it
C     was extracted. This file is under sr10 SCCS
C
C
C
C     ******************************************************************
C
C     Extracted from menu.f
C
C     ******************************************************************
C
      SUBROUTINE LODTOK(LOCKED,FILNAM,ST)
C     ===================================
C
C1    vartype            L    C*80  I4
C1    iostatus           I      I    O
C2
C2    Subroutine LODTOK writes the verb/noun token
C2    into the token table from an external file.
C2    ST returns the completion status
C
      include 'include/vntable.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 VNNUM,UNITN,ST,NLIST(30),TLIST,I
      LOGICAL EX,OK,OP,LOCKED
      CHARACTER TOKEN*1,STRING*80,FILNAM*(*)
      EXTERNAL FINDU1,WAIT
C
      DATA TLIST/14/
      DATA (NLIST(ST),ST=1,14)/25,26,272,
     +         301,304,305,306,310,312,313,314,315,316,317/
C
 3    CONTINUE
C     clear all tokens to nulls
      DO 5 VNNUM=1,500
         VNTOKE(VNNUM)=' '
         VNPOS(VNNUM)=0
 5    CONTINUE
C
C		write(*,*) 'TOKEN FILE:',FILNAM
	  INQUIRE(FILE=FILNAM,EXIST=EX)
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +        FORM='UNFORMATTED',IOSTAT=ST,ERR=100)
     
     	
         REWIND(UNIT=UNITN)
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
C
C
C        Read first line for new compatibility.
C        These number are set in menuload program
C
         READ(UNIT=UNITN,END=20)VNNUM,TOKEN
C		READ(UNIT=UNITN,END=20,ERR=30)VNNUM,TOKEN         
         IF ( VNNUM.NE.999999.AND.TOKEN.NE.'X') THEN
              WRITE(*,*) 'Attempt at using previous version token file'
              STOP
         ENDIF
  
   
 10      CONTINUE
         READ(UNIT=UNITN,END=20,ERR=30)VNNUM,TOKEN
C        If we have an unlock system the we don't
C        want them have certain things by accident
         IF ( .NOT. LOCKED ) THEN
            DO 123 I=1,TLIST
               IF ( VNNUM.EQ. NLIST(I) ) TOKEN=' '
 123        CONTINUE
         END IF
C        write token to list
         VNTOKE(VNNUM)=TOKEN
         GOTO 10
 30      CONTINUE
C        set status for read error
         ST=1
C        close the file
         GOTO 25
 20      CONTINUE
         ST=0
 25      CONTINUE
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
      END IF
C
      RETURN
 100  CONTINUE
      ST=1
 
      END
C
C     ******************************************************************
C
C     Extracted from colour81.f
C
C     ******************************************************************
C
      FUNCTION PENCOL(KOLOUR)
C     =======================
C
C1    vartype    I2     I2
C1    iostatus   O      I
C
C2    Pencol is now rewritten. Black is not always 0
C2    X will definatley not allow this. The color supplied is
C2    still modified into a displayable range.
C
      include 'include/daxcolor.inc'
C
      INTEGER*2 PENCOL,KOLOUR
C
      IF (KOLOUR.LT.0) THEN
C         return the background colour
          PENCOL=COLBAK
      ELSE
C         return a colour in the displayable range
          IF ( COLDRG .EQ.2 ) THEN
              IF ( KOLOUR .EQ. COLBAK ) THEN
C                 Background colour only
                  PENCOL = COLBAK
              ELSE
                  PENCOL = COLFOR
              ENDIF
              RETURN
          ENDIF
C         should cope with all eventualities
          PENCOL=MOD(KOLOUR,COLDRG)
      END IF
C
      END

C     ******************************************************************
C
C     Extracted from displayd.f
C
C     ******************************************************************


      SUBROUTINE DMETER(PCENT)
C     ========================
C1    VARTYPE             I4
C1    IOSTAT              I
C
C2    Routine to draw a natty bar for retrive drawing in nounmenu
C2
      include 'include/gpr.ins.inc'
      include 'include/drgbar.inc'
      include 'include/daxcolor.inc'
C
      REAL SIZE
      INTEGER*4 LENGTH,PCENT,ST,VAL
      INTEGER*4 COL
      INTEGER*2 WINDOW(4)
      LOGICAL UNOBSCURED
C
C     first set replace mode
      CALL ROPREP()
C
      LENGTH=BRXMAX-BRXMIN
      SIZE = (LENGTH*PCENT)/100
C
C     get display
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
      WINDOW(1) = BRXMIN
      WINDOW(2) = BRYMIN
      WINDOW(3) = SIZE
      WINDOW(4) = BRYMAX-BRYMIN+1
C
      COL = 1
      CALL TOOLPEN(COL)
      CALL GPR_$RECTANGLE(WINDOW,ST)
C
      CALL GPR_$RELEASE_DISPLAY(ST)
C     flush out the server
      CALL GPR_$SERVER_FLUSH_X(ST)
      END


C     ******************************************************************
C
C     Extracted from displayd.f
C
C     ******************************************************************


      SUBROUTINE METERC()
C     ===================
C
C2    Routine to draw a natty bar for retrive drawing in nounmenu
C2
      include 'include/gpr.ins.inc'
      include 'include/drgbar.inc'
      include 'include/daxcolor.inc'
C
      REAL SIZE
      INTEGER*4 LENGTH,PCENT,ST,VAL
      INTEGER*2 WINDOW(4)
      INTEGER*4 COL
      LOGICAL UNOBSCURED
C
C     first set replace mode
      CALL ROPREP()
C
C     get display
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
      WINDOW(1) = BRXMIN
      WINDOW(2) = BRYMIN
      WINDOW(3) = BRXMAX-BRXMIN
      WINDOW(4) = BRYMAX-BRYMIN+1
C

      COL = COLBAK
      CALL TOOLPEN(COL)
      CALL GPR_$RECTANGLE(WINDOW,ST)
      COL = 1
      CALL TOOLPEN(COL)
C
      CALL GPR_$RELEASE_DISPLAY(ST)
      END

C     ******************************************************************
C
C     Extracted from display.f
C
C     ******************************************************************
C
      SUBROUTINE ALLRD(TMIP,ENT,M,DELETE)
C     ===================================
C1    VARTYPE           I2  I2 R(3,3)  L
C1    IOSTATUS          I   IO    O    O
C
C2    General entity read routine. Returns the entity type any associated
C2    transform and whether the entity is deleted.
C2  
C2  
C2    Arguments:-
C2  
C2    TMIP        ->          Master index pointer.
C2    ENT         ->          Entity type (DAXCAD)
C2    M           ->          Transformation matrix ( depends on entity )
C2    DELETE      ->          Flag .TRUE. indicates entity deleted.
C2  
C2    Error Returns:
C2  
C2    If entity type is 0 then an error occured.
C2  
C
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
C
      REAL M(3,3)
      INTEGER*2 ENT
      INTEGER*2 TMIP
      INTEGER*4 TYPE
      LOGICAL DELETE
      LOGICAL OK
C
C     read Mi data first
      CALL DIR500(TMIP,OK)
      IF ( .NOT.OK ) THEN
C         error in read must set entity type to 0 and return.
          ENT = 0
          GOTO 999
      ENDIF
C
C     ensure global mip is correct
      DELETE=IMBUFF(1).EQ.100
      IF ( DELETE ) THEN
C         deleted proceeed no further.
          GOTO 999
      ENDIF
C
      MIP=TMIP
C     set entity type
      ENT = IMBUFF(2)
C     Must be an I4 goto for certain compilers. ( SUN 386i )
      TYPE = ENT
C
C
C           1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
C          21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
C          41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
C          61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
C          81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
C
      GOTO( 1, 2, 3, 1, 5, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1,30,31, 1,33,33,33,33,33, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1,66, 1,66, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1,85, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     1     ) TYPE
C
 1    CONTINUE
         RETURN
 2    CONTINUE
C        Line to be read
         CALL DBR500(IMBUFF(7),OK)
         RETURN
C
 3    CONTINUE
C        Line to be read
         CALL DBR500(IMBUFF(7),OK)
         RETURN
 5    CONTINUE
C        Arc to be read
         CALL DBR500(IMBUFF(7),OK)
         RETURN
 7    CONTINUE
C        Spline to be read
         CALL DER500(TMIP,OK)
         RETURN
 30   CONTINUE
C        Center Line to be read
         CALL DBR500(IMBUFF(7),OK)
         RETURN
 31   CONTINUE
C        Hatch to be read
         CALL DER500(TMIP,OK)
         RETURN
 33   CONTINUE
C        dimension to be read
         CALL DERDIM(TMIP,OK)
         RETURN
 66   CONTINUE
C        Componet instance to be read
         CALL DER566(TMIP,M,OK)
         RETURN
 85   CONTINUE
C        Text to be read
         CALL DER500(TMIP,OK)
C
999   CONTINUE   
      END
C

C     ******************************************************************
C
C     Extracted from basik2.f
C
C     ******************************************************************


      SUBROUTINE MAC020(OK)
C     =====================
C1    VARYPE            L
C1    IOSTAT            O
C
C2    Subroutine MAC020 processes FOR statements
C2    found in a macro-program line. Any errors will be detected
C2    and printed on DAXCAD. We assume that the macro will be terminated 
C2    if error found.
C
C2    Arguments:-
C2  
C2    NONE
C2  
C2    Error Returns:
C2  
C2    LOGICAL .TRUE. indicates succes of process
C2  
C2  
      include  'include/macro.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 WL,P,VLEN,VP,INDEX,NLEN,NLEN1,LBUFF
      INTEGER*4 P1,P2,P3,STEP
      REAL RTMP
      DOUBLE PRECISION DN,DN1
      LOGICAL OK,EOS
      CHARACTER WORD*80,INIVAL*80,FINVAL*80,INCVAL*80,BUFF*80
      INTEGER*4 NF,FIELD(30),FNEXT
      EXTERNAL XWORD0,FOLDUP,DEPRNT,MAC000,NLEN,NLEN1,DCPRNT
C
      OK=.TRUE.
      STEP=0
      FNEXT=0
      BUFF=MACBUF(MACCP+1:)
      CALL CHARCH(BUFF)
      LBUFF=NLEN1(BUFF)
      P1=INDEX(MACBUF(MACCP+1:),'=')
      P2=INDEX(BUFF,'TO')
      STEP=INDEX(BUFF,'STEP')
      P3=STEP
      IF(STEP.EQ.0) THEN
          P3=LBUFF+1
          INCVAL='1'
      ELSE
          INCVAL=BUFF(P3+4:)
      ENDIF
C     Error check conditions
C     Errors checked
C     ************************
C     Missing =
C        "    TO
C        "    start value
C        "    end value
C     ************************
C     Errors in variables will be checked later in the routien in
C     AEXPRN etc
      IF(P1.LT.2.OR.P2.EQ.0.OR.(P2.EQ.P1+1).OR.(P2+1.EQ.LBUFF))THEN
C         Syntax error chaecked
          CALL DCPRNT(460)
          GOTO 99
      ENDIF
      FORLCV(NFORLP+1)=BUFF(1:P1-1)
      INIVAL=BUFF(P1+1:P2-1)
      FINVAL=BUFF(P2+2:P3-1)
C
C     load initial value to variable processor
      WRITE(UNIT=WORD,FMT='(A,A,A)')
     +     FORLCV(NFORLP+1),'=',INIVAL(1:NLEN1(INIVAL))
      CALL AEXPRN(WORD,DN,*99)
C     save initial value in common
      FORLRV(1,NFORLP+1)=REAL(DN)
C     calculate final value
      CALL AEXPRN(FINVAL,DN1,*999)
C     save final value in common
      FORLRV(2,NFORLP+1)=REAL(DN1)
C     calculate step value
      IF((DN.GT.DN1).AND.STEP.EQ.0) INCVAL='-1'
      CALL AEXPRN(INCVAL,DN,*999)
C     save step value in common
      IF(DN.EQ.0) GOTO 10
      FORLRV(3,NFORLP+1)=REAL(DN)
C     save program line number to start processing
C     loop contents from
      FORLNS(1,NFORLP+1)=MACLIN
C     save stack position of control variable
      CALL RECVAR(FORLCV(NFORLP+1),VLEN,VP)
      FORLNS(2,NFORLP+1)=VP
C
C     test for zero trip loop
      IF (FORLRV(3,NFORLP+1).GT.0) THEN
C        step is positive
         RTMP=FORLRV(2,NFORLP+1)-FORLRV(1,NFORLP+1)
      ELSE
C        step is negative
         RTMP=FORLRV(1,NFORLP+1)-FORLRV(2,NFORLP+1)
      END IF
      IF (RTMP.GE.0.0) THEN
C         loop must be executed
C         set nesting level of loops
          NFORLP=NFORLP+1
          RETURN
      ENDIF
10    CONTINUE
C     zero trip loop,scan for NEXT
      CALL MAC000(OK)
      IF (OK) THEN
C         hit end of program
          CALL DEPRNT(399)
C          WRITE(10,*)'[MAC020] FOR without NEXT error'
          GOTO 99
      ENDIF
C     test for NEXT
      CALL PARSES(MACBUF,FIELD,NF)
      WORD=MACBUF(1:FIELD(1))
      CALL FOLDUP(WORD)
      IF (WORD.EQ.'NEXT') THEN
          IF(FNEXT.EQ.0) THEN
C             return to caller
              OK=.TRUE.
              RETURN
          ELSE
              FNEXT=FNEXT-1
          ENDIF
      ENDIF
      IF (WORD.EQ.'FOR') THEN
          FNEXT=FNEXT+1
      ENDIF
C     go try next line
      GOTO 10
 99   CONTINUE
      OK=.FALSE.
      WRITE (UNIT=WORD, FMT='(A,I3.3,A)') '(A,I3,T',MACCP+8,',A)'
      WRITE (UNIT=FINVAL, FMT=WORD)  'Line:',MACLIN,'^'
C     print out final error and location.
      PRNTS=.TRUE.
      CALL CPRINT(FINVAL)
      RETURN
 999  CONTINUE
C
      END

C     ******************************************************************
C
C     Extracted from basik2.f
C
C     ******************************************************************



      SUBROUTINE MAC082(OK,EOF)
C     =========================
C1    VARYPE            L   L
C1    IOSTAT            O   O
C
C2    Input variables from a file. This routine will input
C2    a variable list from a sequential access file. The values
C2    can omly be read in if the unit number requsted has been
C2    opened. EOF indicated that an end of file has been reached
C2  
C2    Arguments:-
C2  
C2    OK      ->         .TRUE. success
C2    EOF     ->         .TRUE. end of file has been reached.
C2  
C2  
C2    Error Returns:
C2  
C2    OK  as above
C
      include   'include/macro.inc'
C
      INTEGER*4 LUN,NL,NOV,I,IC,NLEN1,VABL,ASSNUM,NLEN,FIELD(30)
      INTEGER*4 VARFD(30),VFP,SSP,VSP,NF,LC,EC,IQ
      DOUBLE PRECISION DLUN,DVAL,VAL(20)
      CHARACTER FILNAM*80,CH
      CHARACTER BUFF*80,DQ*2,QC*2
      CHARACTER INBUF*400,OUTBUF*80,STRING*80,SVAL*80,SVAR*80
      LOGICAL OK,EOS,FO,EOF,SOK
      INTRINSIC INDEX
C
      EXTERNAL AEXPRN,XWORDG,DEPRNT,NLEN1,GETLUN,NLEN
C
C     initialise
      EOF = .FALSE.
      INBUF=' '
      NOV=0
      VABL=0
C     parse the incoming buffer
      STRING=MACBUF(MACCP+1:)
      CALL PARSES(STRING,FIELD,NF)
      IF(NF.EQ.1) THEN
C         No arguments on the input line
          CALL DCPRNT(443)
          GOTO 100
      ENDIF
      BUFF=STRING(1:FIELD(1))
      CALL AEXPRN(BUFF,DVAL,*100)
      CALL CEXPRN(BUFF,SOK)
      IF(SOK) THEN
          CALL DCPRNT(474)
          GOTO 100
      ELSE
          LUN=INT(DVAL)
          CALL GETLUN(LUN,FO,*100)
          IF(.NOT.FO) THEN
              CALL DCPRNT(456)
              GOTO 100
          ENDIF
      ENDIF
C     file is open and waiting to be read
C     read in a buffer full and decode from there
      READ(UNITAR(LUN),'(A)',END=400) INBUF
C     parse the field using commas or spaces as separators
      CALL PARSES(INBUF,VARFD,VFP)
C     if the list of variables to be read is different
C     to that in the file then assign only the smaller
C     of the two
C     initialise the field start pointers
      SSP=FIELD(1)+1
      VSP=1
      ASSNUM=NF-1
      IF(VFP.LT.ASSNUM) ASSNUM=VFP
      I=0
300   I=I+1
      IF(I.GT.ASSNUM) GOTO 200
C     get the desired fields from the macro buffer and the
C     one we just read in.
          SVAR=STRING(SSP:FIELD(I+1))
          SVAL=INBUF(VSP:VARFD(I))
C         get ready to chop off the delimeters
          LC=NLEN1(SVAL)
          EC=NLEN1(SVAR)
          IF(SVAL(LC:LC).EQ.','.OR.SVAL(LC:LC).EQ.';') THEN
              IF(LC.EQ.1) THEN
                  SVAL=' '
              ELSE
                 SVAL=SVAL(:LC-1)
              ENDIF
          ENDIF
          IF(SVAR(EC:EC).EQ.','.OR.SVAR(EC:EC).EQ.';') THEN
              IF(EC.EQ.1) THEN
                  SVAR=' '
              ELSE
                 SVAR=SVAR(:EC-1)
              ENDIF
          ENDIF
C         stick an equals between the two and concatonate
C         assign it by calling the biggy
          EC=NLEN1(SVAR)
          QC='  '
          IF(SVAR(EC:EC).EQ.'$') THEN
              QC='"'
C             now we have a string lets look for any quotes. If we find
C             them then double them up. this is to make aexprn think that
C             they are single quotes.
              ASSNUM=I
              IQ=0
              SVAL=INBUF(VSP:)
              BUFF=SVAL
              EC=NLEN1(SVAL)
500           IQ=IQ+1
              IF(.NOT.(IQ.GT.EC.OR.IQ.GT.LEN(SVAL))) THEN
C                 ok to put a quote char in
                  DQ=SVAL(IQ:IQ)
                  IF(DQ.EQ.'"') THEN
                     BUFF=SVAL(:IQ)//SVAL(IQ:)
                     SVAL=BUFF
                     IQ=IQ+1
                     EC=EC+1
                  ENDIF
                  GOTO 500
              ENDIF
          ELSE
C             normal variable.
              IF(NLEN(SVAL).EQ.0) SVAL='0'
          ENDIF
C
          OUTBUF=SVAR(:NLEN1(SVAR))//
     +           '='//QC(:1)//SVAL(:NLEN1(SVAL))//QC
          CALL AEXPRN(OUTBUF,DLUN,*100)
C         update the field start pointers
          VSP=VARFD(I)+1
          SSP=FIELD(I+1)+1
      GOTO 300
C     *****************************
C     return with successful completion
C     *****************************
200   CONTINUE
      OK=.TRUE.
      RETURN
C     *****************************
C     problem return with an error
C     *****************************
100   CONTINUE
      OK=.FALSE.
      RETURN
C     *****************************
C     tell them they canny go on
C     *****************************
400   CONTINUE
C     an end of file has been signalled. We shall now
C     go to the label specified in the LABEOF global
      MACBUF='GOTO '//LABEOF(:NLEN1(LABEOF))
      MACCP=1
      MACLEN=NLEN1(MACBUF)
      EOF=.TRUE.
      OK=.TRUE.
      END


C     ******************************************************************
C
C     Extracted from basik2.f
C
C     ******************************************************************

      SUBROUTINE MAC060(OK)
C     =====================
C1    VARYPE            L
C1    IOSTAT            O
C
C2    Makes a subroutine call to something via DAXCAD
C2    direct call. Invoked by a CALL statement a function name and some arguments
C2  
C2  
C2    eg CALL SYSTEM("/bin/ls -la")
C2  
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    True or False ie duff argument list
C2  
C2  
C
      include  'include/macro.inc'
C
      DOUBLE PRECISION DN
      INTEGER*4 SL
      INTEGER*4 OLEN
      INTEGER*4 TCHR
      INTEGER*4 NLEN
      INTEGER*4 INDEX
      INTEGER*4 INTG
      INTEGER*4 NVAR
      INTEGER*4 CVAR
      INTEGER*4 NLEN1
      INTEGER*4 I
      INTEGER*4 FIELD(30)
      INTEGER*4 NF
      INTEGER*4 P
      INTEGER*4 SP
      INTEGER*4 ST
      INTEGER*4 LEN1
      INTEGER*4 LEN2
C
      INTEGER*4 II
C
      LOGICAL OK
      LOGICAL EOS
      LOGICAL CVERFY
      LOGICAL COK
      LOGICAL QUIT
      LOGICAL PREF
      LOGICAL YES
      LOGICAL FE
C
      CHARACTER*80 STRING
      CHARACTER*80 FUNCT
      CHARACTER*1 FCHR
      CHARACTER*80 ARGS
      CHARACTER*80 VAR
      CHARACTER*80 SYS
      CHARACTER*80 CARGS(30)
C
      REAL AVAR(10)
C
      EXTERNAL NLEN
C
C
C     Initalisation of variables
C
      OK=.FALSE.
      NVAR=0
      SP=1
C     go find the next valid character
      STRING=MACBUF(MACCP+1:)
      CALL PARSES(STRING,FIELD,NF)
      P=INDEX(STRING,'(')
      IF(P.EQ.0) THEN
          FUNCT=STRING
      ELSEIF(P.EQ.1) THEN
          GOTO 999
      ELSE
          ARGS=STRING(P+1:NLEN1(STRING))
          IF(ARGS(NLEN1(ARGS):).NE.')')THEN
              GOTO 999
          ENDIF
          CALL PARSES(ARGS,FIELD,NVAR)
          FUNCT=STRING(:P-1)
          DO 100 I=1,NVAR
              IF(FIELD(I).EQ.1) THEN
                  GOTO 999
              ENDIF
C             save argument as a character
              CARGS(I) = ARGS(SP:FIELD(I)-1)
              CALL AEXPRN(CARGS(I),DN,*998)
              CALL CEXPRN(CARGS(I),COK)
              IF(.NOT.COK) THEN
C                 set the numeric value of the string
                  AVAR(I)=REAL(DN)
              ELSE
C                 reset as a character string
                  AVAR(I)=0
              ENDIF
              SP=FIELD(I)+1
100       CONTINUE
      ENDIF
 12   CONTINUE
C     system calls
      IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$ZOOMO') THEN
         IF (NVAR.NE.3) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
         CALL ZOOMO(AVAR)
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$REDRAW') THEN
         CALL REDRAW()
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$ZOOMI') THEN
         IF (NVAR.NE.4) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
         CALL ZOOMI(AVAR)
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$PAN') THEN
         IF (NVAR.NE.2) THEN
            CALL DEPRNT(443)
            RETURN
         END IF


         CALL PAN(AVAR)
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$WAIT') THEN
         IF (NVAR.NE.1) THEN
            CALL DEPRNT(443)
            RETURN
         END IF

         CALL DGWAIT(AVAR(1))
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$PREVIOUS') THEN
         CALL ZPVIEW()
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$EXTENTS') THEN
         CALL ZOMEXT()
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'$CLEAR') THEN
         CALL INITDB()
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'SYSTEM') THEN
C
C        External system argument. Important to release display
C
         IF (NVAR.NE.1) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
         LEN1= NLEN(CARGS(1))
         IF ( LEN1.EQ.0) THEN 
            CALL DEPRNT(443)
            RETURN
         ENDIF

         CALL GPR_$FORCE_RELEASE(II,ST)

         WRITE(SYS,FMT='(3A)',ERR=999) '"',CARGS(1)(1:LEN1),'"'
         CALL SHELLP(SYS,ST)
         CALL ACRDIS(II)
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'CHGTXT') THEN
         IF (NVAR.NE.2) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
         CALL GPR_$FORCE_RELEASE(II,ST)
         LEN1= NLEN(CARGS(1))
         LEN2= NLEN(CARGS(2))
         IF ( LEN1.EQ.0.OR.LEN2.EQ.0) THEN 
            CALL DEPRNT(443)
            RETURN
         ENDIF
         CALL CHANGETEXTPAT(CARGS(1)(1:LEN1),CARGS(2)(1:LEN2),ST)
         CALL ACRDIS(II)

      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'INQUIRE') THEN
C
C        Inquire function to test existance of files
C
         IF (NVAR.NE.1) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
C
         CALL GPR_$FORCE_RELEASE(II,ST)
         LEN1= NLEN(CARGS(1))
C
         IF ( LEN1.EQ.0) THEN 
            CALL DEPRNT(443)
            RETURN
         ENDIF

         INQUIRE(FILE=CARGS(1)(1:LEN1),EXIST=FE)
         IF ( FE ) THEN
             CALL SETSYSTEMVAR('@FEXIST$','Y')
         ELSE
             CALL SETSYSTEMVAR('@FEXIST$','N')
         ENDIF
         CALL ACRDIS(II)
      ELSE IF(FUNCT(1:NLEN1(FUNCT)).EQ.'CONFIRM') THEN

         IF (NVAR.NE.2) THEN
            CALL DEPRNT(443)
            RETURN
         END IF
         CALL FOLDUP(CARGS(2))
         PREF = CARGS(2)(1:1).EQ.'Y'
         CALL CONFIRMATION(CARGS(1),PREF,YES)
         IF ( YES ) THEN
	     CALL SETSYSTEMVAR('@YESOK$','Y')
         ELSE
	     CALL SETSYSTEMVAR('@YESOK$','N')
         ENDIF

      ELSE
C         Cannot decode this statement come out
          GOTO 999
      END IF
C     Successfully decoded the function call 
      OK=.TRUE.
      RETURN
999   CONTINUE
C     error tell him about it.
      CALL DCPRNT(460)
998   CONTINUE
C     error here
      OK=.FALSE.
C
      END
C
C     ******************************************************************
C
C     Extracted from change.f
C
C     ******************************************************************

      SUBROUTINE CHGC03(ALL,USESW,MIPP,SYMBOL,NAME,CHKSTR,OK)
C     =======================================================
C1    VARTYPE            L    L    I2    L     L    C*(*)  L
C1    IOSTATUS           I    I    I     I     I      I    O
C        
C2    This routine will check if the selected entity is one
C2    that we want.
C2  
C2    Arguments:-
C2  
C2    ALL              ->             check all entries in the SWINDU file.
C2    USESW            ->             Update SWINDU file if entry is rejected.
C2    MIPP             ->             Master index pointer of entity
C2    SYMBOL           ->             Indicates .TRUE. a symbol ( DAXCAD )
C2    NAME             ->             Name of symbol/component
C2    CHKSTR           ->             Old name
C2
C2  
C2    Error Returns:
C2  
C2    OK .TRUE. success
C2  
C2  

C
      include   'include/masti.inc'
      include   'include/swind.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
C
      LOGICAL ALL,SYMBOL,NAME,OK,USESW
      INTEGER*4 I,INDX,BINDEX,SCOUNT
      INTEGER*2 MIPP,D1,D2,P,TXTPTR,TENT
      CHARACTER CHKSTR*(*),TSTSTR*80     
      REAL BX,BY
C                            
      EXTERNAL BINDEX
C
      P = MIPP
      I = 0
      SCOUNT = 0
      IF (SYMBOL) THEN
         TENT = SYMBI
      ELSE
         TENT = COMPI
      ENDIF 
 10   CONTINUE
         IF (ALL .AND. (I.LT.NDATA)) THEN
            I = I + 1
C           read from SWINDU
            CALL RSCRF(I,P,BX,BY,D1,D2)
         ENDIF

C        read instance from MI.
         CALL DIR500(P,OK)   
C        Is it the right type ?
         IF(IMBUFF(2).EQ.TENT) THEN
C           The right type of entity. Check the name.
            TXTPTR = IMBUFF(9)
            CALL DTR500(TXTPTR,OK)
            IF (NAME) THEN
               TSTSTR = CBUFF
            ELSE                           
C              Get the path opto (but discluding) the last /.
C              Unless, that is, the path IS '/'.
               INDX = BINDEX(CBUFF,'/') - 1
C              Make an empty path a blank.
               IF (INDX.GE.1) THEN 
                  TSTSTR = CBUFF(:INDX)
               ELSE IF (INDX.EQ.0) THEN
C                 The path IS '/'
                  TSTSTR = '/'
               ELSE 
                  TSTSTR = ' '
               ENDIF
            ENDIF
C           Make them lower case.
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C            CALL UNFOLD(CHKSTR)
C            CALL UNFOLD(TSTSTR)
            IF (CHKSTR.NE.TSTSTR) THEN
C              This aint what we are looking for.
               IF (USESW) THEN
                  IF (ALL) THEN                
C                    Unflag it in the database.
                     IF (IMBUFF(1).GT.128) IMBUFF(1)=IMBUFF(1)-128
                     CALL DIM500(P,OK)
C                    Unflag it on the screen.
                     CALL ALLDRW(TENT,P)    
C                    Set SWINDU master index pointer 0 ... change
C                    routine MASCNG will then ignore this entry.
                     P = 0
                     WRITE(UNIT=SWINDU,REC=I) P,BX,BY,D1,D2
                  ELSE 
                     CALL ZSFLAG(.FALSE.,OK)
                  ENDIF
               ENDIF
               OK = .FALSE.
            ELSE
C              Note the position of the last good record,
C              so far, in SWINDU.
               SCOUNT = I
            ENDIF
         ELSE
C           Wrong type of entity.
            IF (USESW) THEN
C              Erk! This shouldn't happen. 
               CALL DEPRNT(61)
C              Throw them all away and return.
               NDATA = 0
               GOTO 99
            ENDIF
            OK = .FALSE.
         ENDIF
      IF (ALL .AND. (I.LT.NDATA)) GOTO 10
      IF (ALL) THEN   
C        Set the length of SWINDU to the last good record.
         NDATA = SCOUNT
      ENDIF
C
 99   CONTINUE
C
      END
C
C
C     ******************************************************************
C
C     Extracted from chrlib.f
C
C     ******************************************************************

      SUBROUTINE FOLDUP(STRING)
C     =========================
C1    VARTYPE           C*(*)
C1    IOSTATUS            IO
C
C2    Subroutine FOLDUP folds all characters in STRING
C2    to upper case.
C
C2    Arguments:-
C2  
C2    STRING      ->      Incoming and outgoung string to be folded.
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      INTEGER*4 L
      INTEGER*4 I
      INTEGER*4 NLEN1
      INTEGER*4 C
      INTEGER*4 C1
      INTEGER*4 C2
      INTEGER*4 Q1
      INTEGER*4 Q2
      CHARACTER*(*) STRING
      LOGICAL TRIP
      EXTERNAL NLEN1
C
C     set limits on folding ( a-z )
      PARAMETER (C1 = 97)
      PARAMETER (C2 = 122)
C     set quote characters for ignoring data
      PARAMETER (Q1 = 34)
      PARAMETER (Q2 = 39)
C
      L=NLEN1(STRING)
      TRIP=.TRUE.
      DO 10 I=1,L
          C=ICHAR(STRING(I:I))
          IF(C.EQ.Q1.OR.C.EQ.Q2) THEN
C             quote character has been found
              TRIP = .NOT.TRIP
          ENDIF
          IF(TRIP) THEN
C             fold if not within quotes
              IF (C.GE.C1 .AND. C.LE.C2) THEN
                  C=C-32
                  STRING(I:I)=CHAR(C)
              END IF
          ENDIF
 10   CONTINUE
C
      END


C     ******************************************************************
C
C     Extracted from chrlib.f
C
C     ******************************************************************

      SUBROUTINE UNFOLD(STRING)
C     =========================
C1    VARTYPE           C*(*)
C1    IOSTATUS            IO
C
C2    Subroutine UNFOLD folds all characters in STRING
C2    to lower case
C
C2    Arguments:-
C2  
C2    STRING      ->      Incoming and outgoung string to be folded down.
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      INTEGER*4 L
      INTEGER*4 I
      INTEGER*4 NLEN1
      INTEGER*4 C
      INTEGER*4 C1
      INTEGER*4 C2
      INTEGER*4 Q1
      INTEGER*4 Q2
      CHARACTER*(*) STRING
      LOGICAL TRIP
      EXTERNAL NLEN1
C
C     set limits on folding ( A-Z )
      PARAMETER (C1 = 65)
      PARAMETER (C2 = 90)
C     set quote characters for ignoring data
      PARAMETER (Q1 = 34)
      PARAMETER (Q2 = 39)
C
      L=NLEN1(STRING)
      TRIP=.TRUE.
      DO 10 I=1,L
          C=ICHAR(STRING(I:I))
          IF(C.EQ.Q1.OR.C.EQ.Q2) THEN
C             quote character has been found
              TRIP = .NOT.TRIP
          ENDIF
          IF(TRIP) THEN
C             fold if not within quotes
              IF (C.GE.C1 .AND. C.LE.C2) THEN
                  C=C+32
                  STRING(I:I)=CHAR(C)
              END IF
          ENDIF
 10   CONTINUE
C
      END

C     ******************************************************************
C
C     Extracted from cursor83.f
C
C     ******************************************************************

      SUBROUTINE TCURS(C,X,Y)
C     =======================
C1    VARYPE          I4 R R 
C1    IOSTAT          O  O O
C
C2    This is the main cursor input routine in DAXCAD
C2    It is called either directly to obtain cursor position from
C2    the user or by GETANS etc. It returns a postion X & Y with
C2    a character hit. Glabal data SOURCE and SKEy are also avaliable.
C2    During MACRO ops this routine will rcall the command processesor
C2    or get a hit from the user ( GINPUT )
C2  
C2    Arguments:-
C2  
C2    C           ->          ASCII value of key hit
C2    X           ->          X coordinate in World Terms
C2    Y           ->          Y coordinate in World Terms
C2  
C2    Relevant Externals
C2
C2    SKEY        ->          Ascii key hit from keyboard.
C2    SOURCE      ->          Type of source.  See APCURS for more details.
C2    APCURX(Y)   ->          Actual screen position.
C2    MACOP       ->          Macro operational.
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/menun.inc'
      include 'include/menug.inc'
      include 'include/apollo.inc'
      include 'include/macro.inc'
      include 'include/dig2wo.inc'
      include 'include/wtov.inc'
      include 'include/comstack.inc'
      include 'include/viewport.inc'
      include 'include/swind.inc'
      include 'include/keydef.inc'
      include 'include/menpop.inc'
      include 'include/journal.inc'
      include 'include/icon.inc'
      include  'include/server.inc'
 
      CHARACTER*1 TOKEN
      CHARACTER*1 KEYMAC
      CHARACTER*256 COORD
      INTEGER*2 XP,YP,NUM
      INTEGER*4 SC,I1,I2,J,C,FC,K,RETM,I,DNUM,UNIT,NLEN1,MNUM
      INTEGER*4 IX,IY
      INTEGER*4 ST
      INTEGER*4 NLEN
      REAL CSX,CSY
      REAL X,Y,SX,SY,CD0D13,DIST,SNAPG,WX1,WY1,WX2,WY2,DISTXY
      LOGICAL OK,MOK,COK,HILITE,LTEMP,WINDOW
      INTRINSIC ICHAR,CHAR
      EXTERNAL MENHIT,SCURS,SUTILS,GTMCHI,GTCLRW,NLEN1,
     +         WNHIT,SC2WO,DISTXY,CD0D13,SNAPG,WRTJRN,NLEN

C     just in case !!!
      X=0.0
      Y=0.0
C     reset all viewport control flags just in case
      VPADD= .FALSE.
      VPMOV= .FALSE.
      VPDEL= .FALSE.
      VPCAN=.FALSE.
      HILITE = .TRUE.
C     set popup command flag (default)
      POPUP = .FALSE.
C
 9    CONTINUE
      TCONT=(GINPUT.AND.GANS)
      IF(MACOP.AND..NOT.GINPUT) THEN
 3          CONTINUE
            CALL MAC101(X,Y,OK)
            IF(ATMAC) RETURN
            IF (MACOP.AND..NOT.GINPUT) THEN
               IF (MEN.EQ.4.AND.CCMD.NE.'q') THEN
C                 set menu cell hiliting in sutils
C                 if necessary
                  IF(CELLN .GT. 0) THEN
                      HILITE = .TRUE.
                  ELSE
                      HILITE = .FALSE.
                  ENDIF
C                 use HILITE to for display POPUPS if being used
                  CALL SUTILS(HILITE)
                  IF(MACOP) GOTO 3
               ELSE
                  RETURN
               END IF
            ENDIF
      END IF
C
C
C     Use this call for SERVER version later.
C

      IF (SERVER) THEN
C
3010      CONTINUE
          WRITE(*,'(A,$)') '[SERVER] Enter World Coordinate: '
          READ(*,'(A)') COORD
          IF ( NLEN(COORD).EQ.0) THEN
               CALL SABORT()
          ENDIF
          READ(COORD,FMT='(2F10.0)',END=1000,ERR=1000) X,Y
          RETURN
3000      CONTINUE
          WRITE(*,'(A)') '[SERVER] Incorrect data supplied'
          GOTO 3010
C
      ENDIF
 
C     fetch a cursor hit
 10   CONTINUE
      POPUP = .FALSE.
C     fetch a cursor hit
      CALL SCURS(C,SX,SY)
C      PRINT*, 'HIT CURSORPOS',SX,SY
C     pop any windows that we find if cursor hit is in its box
      CALL WINCUR(SX,SY,OK)
      IF(OK) GOTO 10
C     test for an icon hit on buttons 1 and 2
C     save cursor postion
      CSX = SX
      CSY = SY
      IF ( SOURCE.EQ.0 ) THEN 
          KEYMAC = CHAR(SKEY)
          CALL RUNKEYMAC(KEYMAC,ST)
          IF ( ST.EQ.0) THEN
C             transfer control back round to macro
              GOTO 9
          ENDIF
      ENDIF
C----------------- digitiser menu control --------------------------------------
C
      IF (MENOK) THEN
C     the digitiser menu is active
         IF ( DIGIT.AND.(C-153).EQ.DIGBUT) THEN
C        the right button has been used - decode co-ordinates
            CALL READMN(SX,SY)
            IF (.NOT.FILEOK) THEN
C            invalid result of some sort
               DNUM=573
               CALL DEPRNT(DNUM)
               GOTO 10
            ELSE
C              must have found a filename so we're finished here
C             bring the window forward
              CALL SETDP(CSX,CSY,.FALSE.)
              GOTO 1000
            END IF
         END IF
      END IF
C
C-------------------------------------------------------------------------------
C
      IF ( DIGIT.AND.(C-153).EQ.DIGBUT) THEN
C        digitiser is running. and he has used it.
         MEN=0
         CCMD=' '
         SOURCE=0
C        SX,SY do not contain screen coordinates if
C        we are here the contain the absolute coordinates
C        from the digitiser.
C        Using the transform already set up
         CALL NEWXY(SX,SY,X,Y,DIGWXY)
C        WRITE(10,*) 'DIGIT COORD:SX,SY',SX,SY
C        write(10,*) 'world before:',x,y
C        check for horizontal line
         WX1=DX
         WY1=DY
         WX2=DX+(WXMAX-WXMIN)
         WY2=DY
         DIST=ABS(CD0D13(WX1,WY1,WX2,WY2,X,Y))
         IF (DIST.LT.WDIGAN) THEN
            Y=DY
         END IF
         WX1=DX
         WY1=DY
         WX2=DX
         WY2=DY+(WXMAX-WXMIN)
         DIST=ABS(CD0D13(WX1,WY1,WX2,WY2,X,Y))
         IF (DIST.LT.WDIGAN) THEN
            X=DX
         END IF
         WX1=X
         WY1=Y
         X=SNAPG(X,WDIGS,0.0)
         Y=SNAPG(Y,WDIGS,0.0)
         DX=X
         DY=Y
C         write(10,*) 'world after:',x,y
         GOTO 1000
      END IF
C
      IF (ERRMSG) THEN
C        clear the message and flag
         CALL GTCLRW(2)
         ERRMSG=.FALSE.
      END IF
C     is the hit in the graphics area?
C
C     test for menu input
      I1=NINT(SX)
      I2=NINT(SY)
C        PRINT*, 'TESTING MENU'
      CALL MENHIT(I1,I2,MEN,CELLN,CTEXT,CCMD,COK)
C     if the source is special popup but in the menus then
C     we must assue priority over multiple cells
      IF(SOURCE.EQ.8.AND..NOT.COK) THEN
C
          CALL SETDP(CSX,CSY,.FALSE.)
C         test for an icon status of this thing 
          IF(CVPN.GT.0) THEN
              IF(ICNUM(CVPN).AND..NOT.SKEY.EQ.99) THEN
                  CALL SETDP(CSX,CSY,.TRUE.)
                  DNUM = 756
                  CALL DEPRNT(DNUM)
C                 go back round
                  GOTO 10
              ENDIF
          ENDIF
              
C         **** DAVYs (and COLINs!) POPUP menus stuff ****
C         load up the correct popup
          XP = SX
          YP = SY                     
C         ******************************************
C         LEAVE VPSMNU,DSPMNU and PTSMNU in as these
C         are to enable PoPuP to be turned off when 
C         required
C         ******************************************
          IF(SKEY.EQ.99.AND.VPSMNU) THEN
C             NOTE we are driving every thing off popups now
C             3rd button hit deactivated
C             Turbo Views
              IF(MAWS) THEN 
                  MEN  = 4
                  MNUM = 2
              ELSE 
                  MEN = 4
                  MNUM = 9
C                 disable some facilites
                  IF(CVPN.EQ.0) MNUM = 10
              ENDIF
          ELSEIF(SKEY.EQ.98.AND.DSPMNU) THEN
C             Display or layer (REVERSE for new popup structure )
              IF(DISLAY) THEN
                  MEN = 4
                  MNUM = 16
              ELSE
                  MEN = 4
                  MNUM = 1
              ENDIF
          ELSEIF(SKEY.EQ.97.AND.PTSMNU) THEN
C             Points Mode
              MEN = 3
              MNUM = 5                
          ELSE
              GOTO 10
          ENDIF
          CALL MENPOP(MNUM,MOK)
C         set flag
          POPUP = MOK
C         get the correct cell asociated with the special popup
          CALL GTHFMC(MEN,CCMD,CELLN)
          CELLN = 0
C         Points mode menu we can go home
          IF(MOK.AND.MNUM.EQ.5) THEN
              MEN = 3
              GOTO 1000
          ENDIF
C         call daxport and maws direct
C         make window current now
          IF(MOK.AND.(MNUM.EQ.9.OR.MNUM.EQ.10.OR.MNUM.EQ.2)) THEN
C             Only call if 3rd menu hit and MAWS or DAXPORTS
              IF ( MAWS) THEN
                 CALL SUTIL1()
              ELSE
                  CALL DEFWIN(OK)
                  IF(CVPN.GT.0) THEN
                      IF(ICNUM(CVPN) ) THEN
C                         set back to backcloth again
                          CALL SETDP(CSX,CSY,.TRUE.)
                      ENDIF
                  ENDIF
              ENDIF
              GOTO 10
          ENDIF
          IF(MOK) THEN
C             pretned that we had a 4 menu hit
              MEN = 4
              IF(DISLAY) THEN
                  CALL SLAYER()
              ELSE
                  CALL SUTIL1()
              ENDIF
C
C             Go back get another screen hit.
C             Test for a quit character first.
              IF ((CCMD.NE.'Q'.AND.CCMD.NE.'q').AND.
     +            (CCMD.NE.CHAR(150).AND.CCMD.NE.CHAR(13))) GOTO 10
          ELSE
              GOTO 10
          ENDIF
      ENDIF          
C     test for an ordinary cursor hit
      CALL WNHIT(SX,SY,OK)
C     increment com buffer pointer
      IF(CSTKP.LT.20) THEN
           CSTKP = CSTKP +1
      ELSE
C         reset for wrap round
          DO 200 I=2,20
              CMDS(I-1) = CMDS(I)
              HITPTS(1,I-1) = HITPTS(1,I)
              HITPTS(2,I-1) = HITPTS(2,I)
200       CONTINUE
C
      ENDIF
C
      IF ( OK ) THEN
C        PRINT*, 'VIEWPORT HIT'
C     graphics area has been hit
C     set current window please with all the trimmings
          CALL SETDP(CSX,CSY,.FALSE.)
          IF(CVPN.GT.0) THEN
              IF(ICNUM(CVPN) ) THEN
                  CALL SETDP(CSX,CSY,.TRUE.)
                  DNUM = 757
                  CALL DEPRNT(DNUM)
C                 go back round
                  GOTO 10
              ENDIF
         ENDIF
         IF(MACOP.AND.GINPUT.AND..NOT.GANS) THEN
C             Invalid hit for macro operation
               CALL SC2WO(SX,SY,X,Y)
               CALL MAC101(X,Y,OK)
               IF (OK) GOTO 9
         END IF
         MEN=0
         CCMD=' '
         CALL SC2WO(SX,SY,X,Y)
         CMDS(CSTKP) = 'Screen hit'
         HITPTS(1,CSTKP) = X
         HITPTS(2,CSTKP) = Y
C----------------- key definition control --------------------------------------
C
         IF (KSTART) THEN
             KCMD='GINPUT'
             CALL GETKEY()
         END IF
C
C-------------------------------------------------------------------------------
C        write out an entry to the journal
C        file if required i.e. if points mode is on then
C        the journal entry will be done from GETANS with
C        the true END, CENTRE etc value
         IF(JOURON .AND. .NOT. PNTMOD) 
     +     CALL WRTJRN(X, Y, 'w', 'dummy', C)
         GOTO 1000
      END IF
C
      X=SX
      Y=SY
C     test for menu input
      I1=NINT(SX)
      I2=NINT(SY)
C     menu input request
C        PRINT*, 'CALL MENHIT ? ',CCMD,OK
      CALL MENHIT(I1,I2,MEN,CELLN,CTEXT,CCMD,OK)

C      PRINT*, 'MENHIT ? ',CCMD,OK
C
C
      IF (OK) THEN
C        update command list
         CMDS(CSTKP) = CTEXT
         IF (CCMD.EQ.' ') THEN
C           ignore the menu hit if space char
            CALL DEPRNT(131)
            GOTO 10
         END IF
C        write out an entry to the journal
C        file if required
         IF(JOURON .AND. MEN .NE. 4 ) 
     +   CALL WRTJRN(X, Y, 'm', 'dummy',C)
C----------------- key definition control --------------------------------------
C
         IF (KSTART) THEN
             KCMD=CCMD
             CALL GETKEY()
         END IF
C
C-------------------------------------------------------------------------------
         IF (MEN.EQ.4) THEN
C           write out an entry to the journal
C           file if required e.g. if the layer menu 
C           is switched on if display menu is on
C           then journaling is done from the specific 
C           subroutines.
            IF(JOURON .AND. .NOT. DISLAY ) 
     +      CALL WRTJRN(X, Y, 'm', 'dummy',C)
C           set drawn flag
            DRAWN = .FALSE.
C           display control menu hit
            IF(CELLN .GT. 0) THEN
               HILITE = .TRUE.
            ELSE
               HILITE = .FALSE.
            ENDIF
            CALL SUTILS(HILITE)
C           Go back get another screen hit.
C           Test for a quit character first.
            IF ((CCMD.NE.'Q'.AND.CCMD.NE.'q').AND.
     +          (CCMD.NE.CHAR(150).AND.CCMD.NE.CHAR(13))) GOTO 10
         END IF
         IF ( MEN.EQ.1 ) THEN
            CALL GTMCHI(MEN,CELLN)
            CALL CALC00()
            CALL GTMCLO(MEN,CELLN)
            GOTO 10
         END IF
         IF(MACOP.AND.GINPUT.AND..NOT.GANS) THEN
C             Invalid hit for macro operation
               CALL MAC101(X,Y,OK)
               IF (OK) GOTO 9
         END IF
 20   CONTINUE
C        ensure menu cell is hilited
         CALL GTMCHI(MEN,CELLN)
         C=ICHAR(CCMD)
         CS=C
         FC=C
      ELSE
C        must have hit no man's land,go get another hit
C        in a valid region of the screen
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
1000  CONTINUE
C     Flush the X server if active
      CALL GPR_$SERVER_FLUSH_X(ST)
      END
C
C     ******************************************************************
C
C     Extracted from data.f
C
C     ******************************************************************
C
      SUBROUTINE STINIT()
C     ===================
C1    VARYPE       NONE     
C1    IOSTAT       NONE     
C
C2    Main initilisation of DAXCAD global data.
C2    Should be called just the once. From the DAXCAD subroutine.
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/wtov.inc'
      include 'include/ndata.inc'
      include 'include/style.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/arcdat.inc'
      include 'include/swind.inc'
      include 'include/params.inc'
      include 'include/lfont.inc'
      include 'include/hdata.inc'
      include 'include/dimendat.inc'
      include 'include/vntable.inc'
      include 'include/dig2wo.inc'
      include 'include/apollo.inc'
      include 'include/menun.inc'
      include 'include/curpos.inc'
      include 'include/macro.inc'
      include 'include/section.inc'
      include 'include/library.inc'
      include 'include/product.inc'
      include 'include/wildc.inc'
      include 'include/pendat.inc'
      include 'include/save.inc'
      include 'include/layer.inc'
      include 'include/ftypes.inc'
      include 'include/redscl.inc'
      include 'include/suffix.inc'
      include 'include/viewport.inc'
      include 'include/clock.inc'
      include 'include/sioport.inc'
      include 'include/journal.inc' 
      include 'include/inscen.inc'
      include 'include/keymac.inc'
C
      INTEGER I          
      INTEGER*4 ST
      INTEGER*4 NLEN
      CHARACTER SUFFIXES(NSUF)*4
C      
      EXTERNAL NLEN
C     
      DATA (SUFFIXES(I),I=1,NSUF)/'.drg',
     +                            '.cmp',
     +                            '.sym',
     +                            '.sec',
     +                            '.prt',
     +                            '.tbl',
     +                            '.rpt',
     +                            '.inf',
     +                            '.im',
     +                            '.igs',
     +                            '.dxf',
     +                            '.bom',
     +                            '.mif',
     +                            '.dmf',
     1                            '.mac'/
C
CC	Use some variables that define where to look.
CC	This makes running development versions easier.
CC	Also, look for user config files in HOME directory. GCU
C
CAPOLLO
      PRNAM='daxcad.4.0'
      PRODUCT='daxcad'
      LIBRARY='daxcad.4.0'

C      PRNAM='cadeasy.1.0'
C      PRODUCT='cadeasy'
C      LIBRARY='/usr/cadeasy.1.0'
      FILCHR='/'
CAPOLLO
C      CALL GETENVC('HOME',HOME)
CSUN
CIBM|PC386
C      PRNAM='@005'
C      PRODUCT='@002'
C      LIBRARY='@006'
C      PROOT=LIBRARY
C      FILCHR='\\'
CIBM|PC386
CPRIME
C      FILCHR='>'
CPRIME
C 
C     Set GRDMNU to FALSE, used by viewports for updating the
C     GRID select menu when changing viewports
      GRDMNU = .FALSE.
C
C
      CALL DIGINT()
C     *************************************************
C     **** Default Viewing Parameters              ****
C     *************************************************
      SCPX=0.0
      SCPY=0.0
      WCPX=0.0
      WCPY=0.0
C     screen mapping limits world,viewport
      VXMIN=0.0
      VXMAX=2023.0
      VYMIN=0.0
      VYMAX=2799.0
      WXMIN=0.0
      WXMAX=2023.0
      WYMIN=0.0
      WYMAX=2799.0
      SCXMIN=0.0
      SCXMAX=2280.0
      SCYMIN=0.0
      SCYMAX=2024.0
C
C     Setting transformation matrices to unity
C     since world coordinates map directly on to
C     the initial viewport
      CALL I3M(WVXY)
      CALL I3M(VWXY)
 
C     default to english
      LANGID=44
      ANSY='Y'
C
      DISLAY=.FALSE.
      CCMD=' '
C
      DO 10 I=1,20
         APCURX(I)=0.0
         APCURY(I)=0.0
 10   CONTINUE
C     no of lines in cursor
      NPOS=4
C     default cursor size.
      CRSIZE=200
C     set default file type to drawing
      FILTYP='DRAWING'
C     set software revision level
      ROOTRV= 4.01
C     set drawing revision level
      DRGREV=1.00
C     set basic drawing parameters to default conditions
C     start with unit conversion factors
      DBFACS(1)=0.1000
      DBFACS(2)=1
      DBFACS(3)=100
      DBFACS(4)=100000
      DBFACS(5)=2.54
      DBFACS(6)=30.48
      DBFACS(7)=91.44
      DBFACS(8)=160934.40
C
C     *************************************************
C     **** Valid List of Units and menu tokens     ****
C     *************************************************
C     load the valid list of units
C     and associated popup menu tokens
      DBULST(1)='mm'
      DBUNTK(1)='a'
      DBULST(2)='cm'
      DBUNTK(2)='b'
      DBULST(3)='m'
      DBUNTK(3)='c'
      DBULST(4)='km'
      DBUNTK(4)='d'
      DBULST(5)='In'
      DBUNTK(5)='e'
      DBULST(6)='ft'
      DBUNTK(6)='f'
      DBULST(7)='yd'
      DBUNTK(7)='g'
      DBULST(8)='Mi'
      DBUNTK(8)='h'
C     *************************************************
C     **** Default Paper Units etc                 ****
C     *************************************************
C     set default units,paper and scale
      DBUNIT='mm'
      DBUFAC=0.1
      PAPUNT='mm'
      PAPFAC=0.1
      DRWSCL=1.0
      DRGSCL='1/1'
C     set section scale
      SECSCL=DRWSCL
C     set paper mapping to world at default
C     size of A3 with scale of 1.0
      WPXMIN=-210
      WPXMAX=210
      WPYMIN=-148.5
      WPYMAX= 148.5
C     origin of paper in world space
      WPORGX=0
      WPORGY=0
C
C     *************************************************
C     **** Default Fillet Parameters               ****
C     *************************************************
      FRAD=10.0
      FLTRIM=3
      FILLET=0
C     *************************************************
C     **** Default Grid Settings                   ****
C     *************************************************
      GRIDOX=0.0
      GRIDOY=0.0
      GRIDSX=10.0
      GRIDSY=10.0
      SETGRD=.FALSE.
C     initialise on a cartesian grid
      GRTYPE = 1
C     *************************************************
C     **** Default ARC construction Parameters     ****
C     *************************************************
      ARCRAD=20.0
      ANGLE=0.0
      RADSET=.FALSE.
      ARCSET=.FALSE.
C     *************************************************
C     **** Default Centre Line construction Params ****
C     *************************************************
      ROTANG = 0.0
      BRDRSZ = 10.0
      CLINEF = 6
C     *************************************************
C     **** Plotting Defaults (GERBER)              ****
C     *************************************************
C     set Gerber to normal resolution scaling
      GRBSCL=1.0
C     set interpolation tolerance to default
      ARCTOL=0.5
      AUXSCL=1.0
C     *************************************************
C     **** Blank all Layer Names                   ****
C     *************************************************
      DO 22 I=0,255
          LNAME(I)='                      '
 22   CONTINUE
C     *************************************************
C     **** Default Dimension Parameters            ****
C     *************************************************
C     Gap length
      GAPL=3.0
C     Witness extension length
      EXTL=1.0
C     Arrow length
      ALNG=4.0
C     Arrow width
      AWDT=1.5
C     Dimension Text height
      DTHGT=2.5
C     Dimension Text width
      DTWDT=1.875
C     Dimension Text offset
      DTOFF=1.0
C     Paper to World scale
      PAPTOW=1.0
C     Precision for display
      PREC=2
C     Nominal Dimension Tolerance
      TOL=0.05
      MFILE=.TRUE.
C     Default Dimension Standard is BS308
      BS=.TRUE.
      DIN=.FALSE.
      ANSI=.FALSE.
C     Default Dimension Mode is Metric
      METRIC=.TRUE.
      IMP=.FALSE.
C     Default Lower Tolerance
      LTOL=0.00
C     Default Upper Tolerance
      UTOL=0.10
C     Default Tolerance Type
      TOLTYP=1
      SPCSYM=0
C     *************************************************
C     **** Default Hatch Parameters                ****
C     *************************************************
C     Default Hatch distance
      HDIST=4.0
C     Hatch Angle
      HANG=45.0
      LANG=0.7853982
C     hatch forms.
      NFORM=6
C     set to form 1
      FORM=1
C     cross hatch angle.
      CHANG=90.0
C     No Cross-Hatching,
      CROSSH=.FALSE.
C     *************************************************
C     **** Default Move/Copy Parameters            ****
C     *************************************************
      COPYIT=.FALSE.
      NNCOPY=0
C     *************************************************
C     **** Default Text Parameters                 ****
C     *************************************************
C     text variables.
      JUST  =1
      TWIDTH=2.5
      THIGT=3.0
      SLANT=0.0
      TANGL=0.0
C     *************************************************
C     **** Default Appearance Parameters           ****
C     *************************************************
      COLOUR=1
      LAYER=1
      SPARE=0
      FONT=1
      STATUS=10
C     set deafult line font to solid
      CLFONT=1
      THICK=0
C
C     Line fonting data
C
C     *************************************************
C     **** Reset Default Database Pointers         ****
C     *************************************************
C     MI pointer
      NMIPOS=1
C     PD pointer
      NPDPOS=1
C     TX pointer
      NTXPOS=1
      CONT=0
      TXTP=1
      PDFP=1
      MIP=1
C     RL pointer
      NRLPOS=1
C     LAY pointer
      NLYPOS=1
C     Number entities in search file
      NDATA=0
C     Spline mode (see datah0.ftn for explanation)
      SMODE=1
      STYPE=24
C     *************************************************
C     **** Default Database Pointer Limits         ****
C     *************************************************
      MIPLIM=32000
      PDPLIM=32000
      TXPLIM=32000
      RLPLIM=5000
      PRILIM=10000
      PRCLIM=10000
C     *************************************************
C     **** Set Entity Numbers                      ****
C     *************************************************
C             Value from following :
C             --------------------
C               2 - Marker
C               3 - Line
C               5 - Arc
C               7 - Spline
C              30 - Centreline
C              31 - Crosshatching
C              33 - Linear  Dimension
C              34 - Angular     "
C              35 - Radial      "
C              36 - General Label
C              37 - Diameter Dimension
C              50 - Group
C              52 - Detail
C              54 - Symbol Master
C              56 - Component Master
C              64 - Symbol Instance
C              66 - Component Instance
C              82 - Text Node
C              85 - Text
C              86 - Text block
C
C ============================================================
C   Things have changed please look at entity.inc which now===
C   uses a parameter statement to set these entity values  ===
C ============================================================
C
C     not strictly an entity,a relation record type
C             200 - MILIST
C
CAPOLLO
      WILDC(1)='?*.drg'
      WILDC(2)='?*.mac'
      WILDC(3)='?*.cmp'
      WILDC(4)='?*.prt'
      WILDC(5)='?*.sym'
      WILDC(6)='  '
      WILDC(7)='?*.bom'
      WILDC(8)='?*.mif'
      WILDC(6)='?*.dmf'
      WILDC(9)='?*.dmf'
      PATHN(9)=LIBRARY(1:NLEN(LIBRARY))//'/DMFDIR/'
CAPOLLO
CIBM|PC386
C      WILDC(1)='*.drg'
C      WILDC(2)='*.mac'
C      WILDC(3)='*.cmp'
C      WILDC(4)='*.prt'
C      WILDC(5)='*.sym'
C      WILDC(6)='  '
CIBM|PC386
CSUN
CC	Note : on the Sun these are in regex(3) (not shell)
CC	format since we do wild card expansions in line
CC	rather than try to fork a shell to do them!
C      WILDC(1)='\.drg$'
C      WILDC(2)='\.mac$'
C      WILDC(3)='\.cmp$'
C      WILDC(4)='\.prt$'
C      WILDC(5)='\.sym$'
C      WILDC(6)='\.im$'
CSUN
C     set up restore file
 
      CALL INMOV(.TRUE.)
      LAYTST=.FALSE.
      CMPLAY=.FALSE.
      CMPDRW=.FALSE.
      CMPERS=.FALSE.
C
      FINTOT=0
      LOADED=.FALSE.
      SHOW=.FALSE.
      HIDE=.FALSE.
      SHOWAL=.FALSE.
      HIDEAL=.FALSE.
C
C     filetypes for opening and creating
C      File type requested
C
      DAXDRG = 0
      DAXPRT = 1
      DAXCMP = 2
      DAXSYM = 3
      DAXPOP = 4
      DAXSEC = 5
      DAXASC = 6
 
CAPOLLO
      MSFOFF = .FALSE.
C     Set viewport details
      FUNC0 = 186
      FUNC1 = 192
      FUNC2 = 193
      FUNC3 = 194
      FUNC4 = 195
      FUNC5 = 196
      MVPACT = .FALSE.
      DO 15 I =1,5
C
         VPNUM(I) = .FALSE.
         VPVIS(I) = .FALSE.
C
15    CONTINUE
      VPADD = .FALSE.
      VPDEL = .FALSE.
      VPDIS  = .FALSE.
      VPCAN = .FALSE.
      VIEWPS(1,1) = WXMIN
      VIEWPS(2,1) = WYMIN
      VIEWPS(3,1) = WXMAX
      VIEWPS(4,1) = WYMAX
CAPOLLO
C      ****************************************
C      * journal control & special data       *
C      ****************************************
       JOURON = .FALSE. 
       JRNCMD(1) = 'GINPUT'
       JRNCMD(2) = '* Screen Hit'
       JRNCMD(3) = 'PICKZONE LARGE'
       JRNCMD(4) = '* DBGON'
       JRNCMD(5) = '* HOLDMAC' 
       JRNCMD(6) = 'CALL $ZOOMI'
       JRNCMD(7) = 'CALL $EXTENTS'
       JRNCMD(8) = 'CALL $REDRAW'
       JRNCMD(9) = 'CALL $PREVIOUS'
       JRNCMD(10) = 'CALL $ZOOMO'
       JRNCMD(11) = 'CALL $PAN'
C
C
      KEYMACACTIVE = .FALSE.
C
C     wee clock
      CFIRST = .TRUE.
C     initialise the sio usage common block
      DO 33 I = 1,3
        SIOUSE(I) = 0
        CALCNT(I) = 0
  33  CONTINUE
 
      DO 30 I=1,NSUF
C         Copy in suffix data into global data space.
          LSUF(I) = SUFFIXES(I)
 30   CONTINUE
C
      END
C
C     ******************************************************************
C
C     Extracted from datah9.f
C
C     ******************************************************************

C
      SUBROUTINE CLOSWF(OK)
C     =====================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine CLOSWF closes all temporary drawing
C2    work files,returning the logical flag Ok TRUE
C2    if all files are successfully closed.All file
C2    pointers,unit numbers etc are contained within
C2    common block MASTI.
C2    
C2    In workstation this does nothing. To compile porperly
C2    This is a stubbed routine
C
      LOGICAL OK
C
C     Stubbed Routine
C
      END
C

C     ******************************************************************
C
C     Extracted from define.f
C
C     ******************************************************************

      SUBROUTINE PAPCHK()
C     ===================
C1    VARYPE       NONE     
C1    IOSTAT       NONE
C
C2    part of CREATE MENU code developed by the gorgeous sexy Mhari Watt??
C2    star of stage and DTP
C2    checks that the paper selected is what the user wants to base
C2    his menu on, also returns the x and y limits and origin for the grid.
C2    created 11/7/88 - mlw
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/params.inc'
      include 'include/vntable.inc'
      include 'include/menug.inc'
C
      INTEGER*4 DNUM
      INTEGER*4 I
      INTEGER*4 NLEN1
      LOGICAL YESOK
      CHARACTER*80 TEMP
      CHARACTER*80 STRING
      CHARACTER*80 ANS*1
C
      EXTERNAL NLEN1
      EXTERNAL YESOK
C
      PAPOK=.FALSE.
      PLIMIT(1)=0.0
      PLIMIT(2)=0.0
C
C     check for DBU=mm and SCALE=1:1
      IF (DBUNIT.NE.'mm'.OR.DRWSCL.NE.1.00) THEN
          DNUM=576
          CALL EPRINT(DICT01(DNUM))
          DNUM=577
          CALL CPRINT(DICT01(DNUM))
          RETURN
       END IF
 
C
C     find the paper size for the drawing
      I=0
50    CONTINUE
      I=MOD(I,5)+1
C     find the pointer to current sheet size
      IF (PAPLST(I).NE.DRWSHT(:2)) GOTO 50
C
      DNUM=551
      TEMP=DICT01(DNUM)
      STRING=TEMP(:NLEN1(TEMP))//DRWSHT(:3)
      CALL CPRINT(STRING)
      DNUM=2
      CALL DPRMXP(DNUM,ANS)
      IF (YESOK(ANS)) THEN
         PAPOK=.TRUE.
      ELSE
         RETURN
      END IF
C
C     set limits for grid based on sheet size
C     test for rotated sheet first
      IF (DRWSHT(3:3).EQ.'R') THEN
C        sheet must be rotated
         PLIMIT(2)=PAPSIZ(1,I)-10
         PLIMIT(1)=PAPSIZ(2,I)-10
      ELSE
C        sheet positioned normally
         PLIMIT(1)=PAPSIZ(1,I)-10
         PLIMIT(2)=PAPSIZ(2,I)-10
      END IF
C
      END


C     ******************************************************************
C
C     Extracted from define.f
C
C     ******************************************************************


      SUBROUTINE DELL02(ENT,ALL)
C     ==========================
C1    VARTYPE            I2   L
C1    IOSTATUS           I    I
C
C2    Subroutine DELL02 handles all of the work
C2    within the delete options.The selection of masks
C2    etc is passed to this routine from the caller
C2    and acted upon.
C2  
C2    Arguments:-
C2  
C2    ENT         ->          The entity mask 
C2    ALL         ->          All entiies to be included.
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  

C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/delstore.inc'
      include 'include/dcross.inc'
C
      REAL HPX1,HPY1,HPX2,HPY2,X,Y,WX2,WY2
C
      INTEGER*4 C,TMEN,TCELL,I,PTXT
      INTEGER*2 MIPP,ENT,TENT
      LOGICAL BETWP,AUTBTW,LL,FORWARD
C
      LOGICAL OK,OPTION,QUIT,ALL,END
C
      EXTERNAL ALSRCH,NOSRCH,ADSRCH,FINDET,DELL03
C
C     Initalise all Variables for use with main delete routine
      DNPDPO = 0
      DNMIPO = 0
      BETWP = .FALSE.
      AUTBTW = .FALSE.
      CALL SETSER(ENT,ALL,PTXT)
C
 10   CONTINUE
C     find and flag an entity at hit point
      CALL FINDET(PTXT,X,Y,MIPP,TENT,OPTION,QUIT)
C
      IF (QUIT) THEN
          GOTO 100
      ENDIF
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
C     This is a special to cope with line or arc
C     delete between points
C
C
20    CONTINUE
C
      IF ( CCMD.EQ.'P' .OR. CCMD.EQ.'p' ) THEN
C
C         Menu Handler Section.
C
          IF ((CCMD.EQ.'P' .AND. .NOT. BETWP ) .OR.
     +        (CCMD.EQ.'p' .AND. .NOT. AUTBTW)) THEN
C
C             Switching into Between points or Auto Between
C
              IF (AUTBTW .OR. BETWP) THEN
C                 Clear current cell hit
                  CALL GTMCLO(TMEN,TCELL)
                  DNMIPO = 0
              ENDIF
C
C             Select new mode.
C
              IF (CCMD.EQ.'p') THEN
C                 Switching 'Auto between" on.
                  BETWP = .FALSE.
                  AUTBTW = .TRUE.
              ELSE
C                 Switching 'Between Points" on.
                  BETWP = .TRUE.
                  AUTBTW = .FALSE.
              ENDIF
C             remove the 'window' option. It's not valid here.
              CALL FNDPOS(210,C)
              CALL GTCLRC(3,C)
C             remove the 'Accept' option. It's not valid here either.
              CALL FNDPOS(198,C)
              CALL GTCLRC(3,C)
C             We must restrict the search to lines and arcs.
              IF (ALL) THEN
                  CALL NOSRCH()
                  CALL ADSRCH(LINE)
                  CALL ADSRCH(ARC)
                  GSSTAT=3
              ENDIF
C
C             Make sure everything is unflagged.
C
              CALL UNFLAG(.TRUE.)
C
          ELSE
C             Switching 'Between Points' or 'Auto Between' off.
              BETWP = .FALSE.
              AUTBTW = .FALSE.
C             Put the search mask back.
              IF (ALL) THEN
                  CALL SETSER(ENT,ALL,PTXT)
              ENDIF
C             Unflag the between points/auto between option.
              CALL GTMCLO(MEN,CELLN)
C             Put the 'window' and 'accept' options back.
              CALL GTDMEN(210,3)
              CALL GTDMEN(198,3)
C             Put the prompts back.
              IF (ALL) THEN
                 PTXT=327
              ELSE IF ( ENT .EQ. ARC ) THEN
                 PTXT=329
              ELSE
                 PTXT=328
              END IF
              GOTO 10
          ENDIF
C
          TMEN=MEN
          TCELL=CELLN
C         set prompt depending on type of entity
          IF (ALL) THEN
              PTXT=386
          ELSE IF ( ENT .EQ. ARC ) THEN
              PTXT=67
          ELSE
              PTXT=191
          END IF
          IF (BETWP) THEN
              CALL GTMCHI(3,2)
          ELSE
              CALL GTMCHI(3,4)
          ENDIF
C         loop back to get an entity
          GOTO 10
      ELSEIF(OPTION) THEN
C
C         ACCEPT WINDOW OR CANCEL options are left to interpret
C
          LL = BETWP .OR. AUTBTW
          CALL DELL03( LL )
          GOTO 10
      ENDIF
C
C     Hit point from the screen here
C

      IF ( BETWP ) THEN
C
C         Between Points - get points.  
C
          CALL MNLPTS()
          CALL FINDP0(250,HPX1,HPY1,OPTION,QUIT)
          IF (QUIT.OR.OPTION) THEN
              CALL ZSFLAG(.FALSE.,OK)
              CALL MNUPTS()
              IF (CCMD.EQ.CHAR(149)) THEN
C                 Cancel Option Clear line selected
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
              ELSE
                  CALL GTMCLO(TMEN,TCELL)
              END IF
              IF ( OPTION ) THEN
C                 loop back to main option control
                  GOTO 20
              ENDIF
C             Must have quit
              GOTO 100
          END IF
C
C         Second point
C
          CALL FINDP0(251,HPX2,HPY2,OPTION,QUIT)
C         Check what been hit
          IF (QUIT.OR.OPTION) THEN
              CALL ZSFLAG(.FALSE.,OK)
              CALL MNUPTS()
              IF (CCMD.EQ.CHAR(149)) THEN
C                 Cancel Option Clear line selected
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
              ELSE
                  CALL GTMCLO(TMEN,TCELL)
              END IF
              IF ( OPTION ) THEN
C                 loop back to main option control
                  GOTO 20
              ENDIF
C             Must have quit
              GOTO 100
          END IF
C         read back entity 
          CALL DER500(MIPP,OK)
          MIP=MIPP
C
C         Time to unload points menu
C     
          CALL MNUPTS()
C
      ELSEIF(AUTBTW) THEN
C         Auto Between - calculate points.
C         Find the nearest intersection point in each direction.
C         ( If there isn't one, the end point is returned.)
C         Get the data into NBUFF.
          CALL DER500(MIPP,OK)
C
C         Block any crosses from appearing at the intersections.
C
          DCROS = .FALSE.
          MIP=MIPP
          FORWARD = .TRUE.
          CALL INTER0(X,Y,FORWARD,HPX1,HPY1,END)
          MIP=MIPP
          FORWARD = .FALSE.
          CALL INTER0(X,Y,FORWARD,HPX2,HPY2,END)
C         Unblock the crosses.
          DCROS = .TRUE.
      ENDIF
C
      IF ( AUTBTW.OR.BETWP ) THEN
C
C         Delete the entity from the screen with the points.
C
C         Unflag the entity now
          CALL ZSFLAG(.FALSE.,OK)
          IF ( TENT .EQ. LINE ) THEN
              CALL DELLBP(HPX1,HPY1,HPX2,HPY2)
          ELSE IF ( TENT.EQ.ARC ) THEN
              CALL DELABP(HPX1,HPY1,HPX2,HPY2,X,Y)
          END IF
      ENDIF
C
C     Loop back to main Collector
C
      GOTO 10
C
 100  CONTINUE
C
C     Ensure the 'window' and 'accept' options are back.
C
      IF (BETWP .OR. AUTBTW) THEN
         CALL GTDMEN(210,3)
         CALL GTDMEN(198,3)
      ENDIF
C
      END

C     ******************************************************************
C
C     Extracted from define.f
C
C     ******************************************************************



      SUBROUTINE DIMD18(DIMD,DIMT,STRVAL,BLWD,BLHT,NSTR)
C     ==================================================
C1    VARTYPE             R   I4   R   L   R     R  C(3)*(*)
C1    IOSTATUS            I   I    I   I   O     O     O
C
C2    Subroutine DIML18 constructs dimension text entirely
C2    from the keyboard.
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
C
      REAL DIMD,STRVAL(4,10),BLWD,BLHT
      INTEGER*4 NLEN1,I,NSTR,NCHAR,J
      INTEGER*4 TWDTH
      CHARACTER DIMT(10)*(*),TEMP*40,TEMP2*40,INPT*80
      CHARACTER*80 TCBUFF
      LOGICAL SINGLE
      INTRINSIC CHAR
      EXTERNAL NLEN1,CRUNCH,DIMD13
C
C       blank off the  input string
        DO 22 I=1,10
           CALL BCHAR(DIMT(I))
 22			DIMT(I) = ''
		CONTINUE
C
      CALL BCHAR(INPT)
      TWDTH=0
C
C     initialize count of text strings
      I=1
C     Marked for editing.  So we dont need to 
C     collect user input.	
C      IF(LEDIT .AND. I .GT. NUMSTR) THEN
C		I = NUMSTR
C        GOTO 88
C      ENDIF 
CDHR  2/12/03 Added this line to always ensure block does not change
C      IF(LEDIT) THEN
C		I = NUMSTR
C        GOTO 88
C      ENDIF 

C     initialize logic for single blank string
      SINGLE=.FALSE.
 55   CONTINUE
      
      IF(LEDIT) THEN
		TCBUFF = DIMCHR(I)
		INPT = DIMCHR(I)
		NCHAR=NLEN1(INPT)	
      ELSE
C		Last move here
		CALL DCPRNT(39)
C		get string from user and hold in INPT
		CALL DPRMX2(90,TCBUFF,INPT)
C		check the string is valid.
		NCHAR=NLEN1(INPT)
        TCBUFF = INPT
      ENDIF
      
	  IF(.NOT. LEDIT) THEN
        IF ( NCHAR .EQ. 1 .AND. INPT(1:1) .EQ. ' ' ) THEN
C          string is blank
           IF ( I .GT. 1 ) THEN
C            current string is null,ognore it
             I=I-1
             GOTO 88
           END IF
C          only one string,and it is blank
C          store as single space
           SINGLE=.TRUE.
        END IF
      ENDIF
C     store the text string local origin.
      STRVAL(3,I)=0
      STRVAL(4,I)=-(I-1)*1.5*DTHGT
C     store text string height & width
      DIMT(I)=INPT(1:NLEN1(INPT))
      STRVAL(1,I)=DTHGT
      STRVAL(2,I)=DTWDT
      TWDTH=MAX(TWDTH,INT(NCHAR*DTWDT))

	  IF (LEDIT) THEN
C		Check to see if we have all the strings already listed
		IF (NUMSTR.EQ.I) THEN
			GOTO 88
		ENDIF
	  ENDIF

      I=I+1

      IF(I.GT.6) THEN
          I = I-1
C          Print out a message if we are on the take
		  IF (.NOT.LEDIT) CALL DEPRNT(635)
          GOTO 88
      ENDIF
      IF ( I .LT. 11 .AND. .NOT.SINGLE) GOTO 55
      I=I-1
C
 88   CONTINUE
      PRINT*, '[DIMD18] I @88',I
	  NSTR=I
C     store text block overall height & width
      BLWD=TWDTH
      BLHT=NSTR*DTHGT+(NSTR-1)*.5*DTHGT
C     set number of text strings created
C
 99   CONTINUE
      END



C     ******************************************************************
C
C     Extracted from display.f
C
C     ******************************************************************


      SUBROUTINE ALLDRW(ENT,TMIP)
C     ===========================
C1    VARYPE            I2   I2
C1    IOSTAT            O    I
C
C2    General purpose draw routine. Will draw the entity at the
C2    specified Master index position. TMIP
C2  
C2    Arguments:-
C2  
C2    ENT     ->          The entity type to be drawn
C2    TMIP    ->          Master index pointer to draw
C2  
C2  
C2    Error Returns:
C2  
C2    If the ENT is 0 then error has occured.
C2  
C2  
C
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/daxcolor.inc'
      include 'include/server.inc'
C
      REAL M(3,3)
      INTEGER*4 I
      INTEGER*4 J
      INTEGER*4 ST
      INTEGER*2 ENT
      INTEGER*2 TMIP
      INTEGER*2 ENTST
      INTEGER*2 II2
      INTEGER*2 ENTCOL
      INTEGER*2 STAT
      LOGICAL OK
      LOGICAL DELETE
      LOGICAL IGNORE
      LOGICAL ERASE
      LOGICAL XOR
      LOGICAL HWF
C
C     Initalise all internal variables.
C
      XOR=.FALSE.
      HWF=.FALSE.
      ST=1
C
      IF ( SERVER ) THEN
          RETURN
      ENDIF
C     read the entity data into buffers
      CALL ALLRD(TMIP,ENT,M,DELETE)
      IF ( ENT.EQ.0 ) THEN
C         error in reading it.
          GOTO 999
      ENDIF
C
      IF (.NOT.DELETE) THEN
C        test status of entity
C        ignore GROUP totally,only a header or Component Master
         STAT = MOD(INT(IMBUFF(1)),128)
         IGNORE=STAT.EQ.COMPM.OR.ENT.EQ.GROUP
     +      .OR.STAT.EQ.SYMBM
         IF (.NOT.IGNORE) THEN
C           test status of entity
            ENTST=IMBUFF(1)
            IF ( IMBUFF(1).GT.128 ) THEN
C              set hardware font for entity flagging
               II2=2
               CALL HARDWF(II2)
               HWF=.TRUE.
C              set raster op to XOR
               XOR=.TRUE.
               CALL ROPXOR()
            END IF
            IF (ENT.EQ.COMPI .OR. ENT.EQ.SYMBI) THEN
C              component/symbol instance to be drawn
               IF(VLAYER(IMBUFF(4))) THEN
C                 Ok to draw the compnent instance.
                  CALL DRW066(M)
               ENDIF
            ELSE
C              Standard enetity type to be drawn  
               CALL SPCDRW(ENT,TMIP,.FALSE.,M,.FALSE.)
            END IF
C           reset hardware font
            IF ( HWF ) THEN
C              reset to solid line
               II2=0
               CALL HARDWF(II2)
            END IF
         END IF
      END IF
C     raster op to replace
      IF (XOR)  CALL ROPREP()
C
999   CONTINUE
      END


C
C     ******************************************************************
C
C     Extracted from error85.f
C
C     ******************************************************************
C


      SUBROUTINE CPRINT(STRING)
C     =========================
C1    VARYPE              C*(*)
C1    IOSTAT               I
C
C2    Prints a string onto DAXCAD'S prompt window
C2    If SERVER mode is active then the stirng will be printed
C2    to standard output.
C2  
C2    Arguments:-
C2  
C2    STRING          ->      Incoming data string
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/macro.inc'
      include 'include/server.inc'
C
      CHARACTER*(*) STRING
      INTEGER*4 WIN
      INTEGER*4 NLEN
      EXTERNAL GTPLIN
      EXTERNAL NLEN
C
      IF (SERVER.AND.(PROMPT.OR.PRNTS)) THEN
C         server mode Write to standard output.
          WRITE(*,'(2A)') '[SERVER] ',STRING
          RETURN
      ENDIF
C     if we are printing from the macro then its ok
      IF(PROMPT.OR.PRNTS) THEN
         WIN=1
         CALL GTPLIN(STRING,WIN)
         PRNTS=.FALSE.
      ENDIF
      CALL PROMPTOUT(STRING,NLEN(STRING))
C
      END

C
C     ******************************************************************
C
C     Extracted from error85.f
C
C     ******************************************************************
C

      SUBROUTINE EPRINT(STRING)
C     =========================
C1    VARYPE              C*(*)
C1    IOSTAT               I
C
C2    Prints a string onto DAXCAD'S prompt window
C2    If SERVER mode is active then the stirng will be printed
C2    to standard output.
C2  
C2    Arguments:-
C2  
C2    STRING          ->      Incoming data string
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/menun.inc'
      include 'include/server.inc'
C
      CHARACTER*(*) STRING
      INTEGER*4 WIN
      INTEGER*4 NLEN
      EXTERNAL GTILIN,BELL,NLEN
C
C
      WIN = 2
      IF (SERVER) THEN
          WRITE(*,'(2A)') '[SERVER ERROR] ',STRING
          CALL BELL
          RETURN
      ELSE
C         set flag to indicate error message present
          CALL GTILIN(STRING,WIN)
          ERRMSG=.TRUE.
      ENDIF
      CALL ERROROUT(STRING,NLEN(STRING))
C     Ding Dong
      CALL BELL()
C
      END




C
C     ******************************************************************
C
C     Extracted from insert.f
C
C     ******************************************************************
C

      SUBROUTINE CFONT()
C     ==================
C1    VARYPE      NONE      
C1    IOSTAT            
C
C2    This routine will change a font round by number
C2    It can get data from a popup or from data input.
C2    Numbers are indexes into a lookup table.
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include  'include/lfont.inc'
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/style.inc'
      include  'include/daxcolor.inc' 
      include  'include/vntable.inc' 
C
      CHARACTER TOK*1
      CHARACTER NAME*20
      INTEGER*4 C,NLEN
      INTEGER*4 ICHAR
      INTEGER*2 TFONT
      INTEGER*2 ENDVAL
      INTEGER*2 MASK
      INTEGER*2 AND_2
C
      DOUBLE PRECISION VAL
C
      LOGICAL TOKN
C
      EXTERNAL NLEN
      EXTERNAL ICHAR
      EXTERNAL AND_2
C
 10   CONTINUE
      IF ( CCMD .EQ. 'f' ) THEN
C        This section gets FONT number fomr popup  
         CALL MENPOP(-1,TOKN)
C        Check for menu hit.
         IF ( TOKN ) THEN
C           Update the global data.
            CLFONT=ICHAR(CCMD)
C           Find the name of the font.
C           Put that name into the menu cell.
            TXFONT = FONTNM(CLFONT)
         END IF
      ELSE IF (CCMD.EQ.'=') THEN
C
C        This section gets THICKNESS from popup or from user input
         CALL MENPOP(-3,TOKN)
C
         IF (  TOKN ) THEN   
            TFONT=ICHAR(CCMD)
C           Is it a call for a thickness not in the popup menu
C           ie an 'OTHER'.
            IF (TFONT.EQ.255) THEN
               CALL DPRMXP(363,NAME)
C              Blank string return without change
               IF ( NLEN(NAME) .EQ. 0 ) THEN
                  CALL GTMCLO(MEN,CELLN)
                  RETURN
               ELSE
                  CALL AEXPRN(NAME,VAL,*10)
                  TFONT=INT(VAL)
               ENDIF
            ENDIF
C           Check within limits for defined fonts.
            IF ( TFONT.LT.0.OR.TFONT.GT.109 ) THEN
               CALL DEPRNT(362)
               GOTO 10
            END IF
C           Update the global data.
            THICK=TFONT
C           Put the number into the menu cell.
            MASK = 15 * 1024
C           Mask is f00. This picks out the line end number.       
            ENDVAL = AND_2(PLTHKI(TFONT),MASK)  / 1024
            WRITE(NAME,'(I3,A,F4.2,A,I2,A)') TFONT,' {',
     +                             LTHKR(1,TFONT),',',ENDVAL,'}'
            TXTHK = NAME
         END IF 
      END IF
C
      END 


      SUBROUTINE CHANGETEXTPAT(OLDPAT,NEWPAT,ST)
C     ==========================================
C1    VARYPE                   C*(*)   C*(*) I4
C1    IOSTAT                     I      I    O
C
C2    Changes a text pattern to be called from a macro call 
C2  
C2  
C2    Arguments:-
C2  
C2    OLDPAT	->	The old pattern
C2    NEWPAT	->	The new pattern
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    0		->	Success
C2    -1	->	Pattnern not found
C2  
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc' 
      include  'include/vntable.inc' 
C
      CHARACTER*(*) OLDPAT
      CHARACTER*(*) NEWPAT
C
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 NLEN

      INTEGER*2 I
      INTEGER*2 MMIP
      INTEGER*2 PDP
      INTEGER*2 TSLA
      INTEGER*2 TJUST
      INTEGER*2 NCHARS

      LOGICAL OK
C
      EXTERNAL NLEN
C
      ST = -1

      DO 10 I=1,NTXPOS-1
C
C         Read text entry and find if it is DAXCAD text entity
C
          CALL DTR500(I,OK)
          MMIP = ICBUFF(1)
          CALL DIR500(MMIP,OK)
          LENGTH = NLEN(CBUFF)
C
C         The length and enity to be checked.
C
          IF ( LENGTH.GT.0.AND.IMBUFF(2).EQ.TEXT) THEN

C
C              Use Kirks pattern replacement routine
C
               IF ( OLDPAT(1:1).EQ.'%') THEN
                   CALL PATREP1(CBUFF,OLDPAT(2:),NEWPAT,.TRUE.,OK)
               ELSE
                   CALL PATREP1(CBUFF,OLDPAT,NEWPAT,.FALSE.,OK)
               ENDIF
               IF ( OK ) THEN
C
C                  modify the database
C
                   CALL DIR500(MMIP,ST)
                   CALL DTM500(I,MMIP,OK)
C                  Get part data pointer for changing number of chars
                   PDP = IMBUFF(7)
                   CALL DBR500(PDP,OK)
C
C                  uncode, change length , recode and store
C
                   CALL UCODET(RDBUFF(6),TSLA,TJUST,NCHARS)
                   NCHARS = NLEN(CBUFF)
                   CALL CODET(TSLA,TJUST,NCHARS,RDBUFF(6))
                   CALL DBM500(PDP,OK)
C
                   ST = 0
               ENDIF
          ENDIF
10    CONTINUE
      END


      SUBROUTINE SETSYSTEMVAR(NAME,VALUE)
C     ===================================
C1    VARYPE                  C*(*) C*(*)
C1    IOSTAT                   I     I
C
C2    Set the contents of a system variable
C2    If it is a character then use the string direct instead
C2    of converting using internal F77 files
C2  
C2  
C2    Arguments:-
C2  
C2    NAME	->	The name of the system varible
C2    VALUE	->	The value to be converted
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      CHARACTER*(*) NAME
      CHARACTER*(*) VALUE
      CHARACTER*80 EXPR
C
      INTEGER*4 NLEN
      INTEGER*4 NLEN1
      INTEGER*4 VLEN
      INTEGER*4 LENGTH
C
      DOUBLE PRECISION DN
C
      EXTERNAL NLEN
      EXTERNAL NLEN1
C
      LENGTH = NLEN(NAME)
      VLEN = NLEN1(VALUE)
C
      IF ( LENGTH.EQ.0) THEN
          RETURN
      ENDIF

      IF ( NAME(LENGTH:LENGTH).EQ.'$') THEN

          WRITE(EXPR,FMT='(4A)',ERR=999)
     +          NAME(1:LENGTH),'="',
     +          VALUE(1:VLEN),'"'
      ELSE

          READ(VALUE,FMT='(G15.0)',ERR=999,END=999) DN

          WRITE(EXPR,FMT='(2A,G15.7)',ERR=999)
     +          NAME(1:LENGTH),'=',DN

      ENDIF

      CALL AEXPRN(EXPR,DN,*999)

999   CONTINUE

      END





      SUBROUTINE PATREP1(STRING,C1,C2,BEGIN,OK)
C     ========================================
C1    vartype            C*(*) C*(*) C*(*) L L
C1    iostatus            IO     I     I   I O
C
C2    Replaces first occurrence of C1 in STRING
C2    If begin is true then use the beginning of the line only
C2    Returns OK true if replacement made.
C
      INTEGER*4 P
      INTEGER*4 LENGTH
      LOGICAL OK
      LOGICAL BEGIN
      CHARACTER*(*) C1
      CHARACTER*(*) C2
      CHARACTER*(*) STRING
      CHARACTER*256 PAT1
C
      P = INDEX(STRING,C1)
C
      IF ( BEGIN.AND.P.NE.1) THEN
          OK = .FALSE.
          RETURN
      ENDIF
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

      SUBROUTINE SABORT()
C     ===================
C1    no arguments required
C
C2    Subroutine  SABORT releases the DAXCAD server system
C2
C
      include 'include/sioport.inc'
C
      INTEGER*2 I
      INTEGER*4 ST
      LOGICAL KILL
C
C     release the display from GPR control
C
C     Check the sio ports to see if there are in use
      DO 10 I = 1,3
        IF(SIOUSE(I).NE.0) THEN 
           CALL RSTSIO(I)
           CALL SIOCLS(I)
        END IF
  10  CONTINUE
      WRITE(*,'(A)' ) '[SERVER] Shutdown Complete.......' 
      STOP

C
      END


