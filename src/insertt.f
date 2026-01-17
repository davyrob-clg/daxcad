C
C     @(#)  412.1 date 6/11/92 insertt.f 
C
C
C     Filename    : insertt.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:39:07
C     Last change : 92/06/11 14:32:37
C
C     Copyright : Practical Technology Limited  
C     File :- insertt.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DIMN02()
C     SUBROUTINE DIMN03(X1,Y1,X2,Y2,X3,Y3,OK)
C     SUBROUTINE INST01()
C     SUBROUTINE INST02()
C     SUBROUTINE INST03()
C     SUBROUTINE INST03()
C     SUBROUTINE INST10()
C     SUBROUTINE MNITXT()
C
C     |-----------------------------------------------------------------|
C


      SUBROUTINE DIMN02()
C     ===================
C1
C1    no arguments required
C
C2    DIMN02 gathers the neccesary dimension point info
C2    for a Label. This is then
C2    passed to main dimension generating routine which
C2    is intended as general dimen creation routine.
C
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include   'include/menun.inc'
      include   'include/viewport.inc'
C
      REAL X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG
C
      INTEGER*4 TMEN,TCELL
      INTEGER*2 P,ENT
C
      LOGICAL OK, CVERFY
      LOGICAL DOPFL6, OPFL9
C
      EXTERNAL DIMR03
C
      TMEN=MEN
      TCELL=CELLN
C    
C     load the points mode
      CALL MNLPTS() 
C
 10   CONTINUE
C
C     set the control data needed for Label.
      SPCSYM=0
      R=25.0
      DOPFLG(15)=.TRUE.
      TOLTYP=6
      CALL DIMR10(X1,Y1,X2,Y2,X3,Y3,
     +            XC,YC,R,SANG,EANG,.TRUE.,OK)
C     test for abort
      IF (.NOT.OK) THEN
          IF (MEN.EQ.3 .AND. CVERFY(CCMD,'KLAHWV')) THEN
C            parameter change required
C            Highlight option that has been selected
             CALL GTHIMC(MEN,CCMD,'KLAHWV',CELLN)
C            go handle the text status change selected
             CALL INST10()
C            Switch off finished with alteration
             CALL GTMCLO(MEN,CELLN)
             GOTO 10
          ELSE IF ( CCMD .EQ. 'k' ) THEN
C           colour
            CALL CCOLOR()
C           load colour control cell
            CALL MNICOL()
            GOTO 10
          ELSE
            DOPFLG(15)=.FALSE.
            TOLTYP=1
            CALL GTMCLO(TMEN,TCELL)
            GOTO 999
          ENDIF
      END IF
C     go generate dimension data
C     defensive stuff again change text label
C     leaves the flags in rags so save the OPFLAGS
C
      DOPFL6 = DOPFLG(6)
      OPFL9  = OPFLAG(9)
      DOPFLG(6) = .FALSE.
      OPFLAG(9) = .FALSE.
C
C     
C     set label edit flag false
      LEDIT = .FALSE.
      CALL DIMN03(X1,Y1,X2,Y2,X3,Y3,OK)
C
      DOPFLG(6) = DOPFL6
      OPFLAG(9) = OPFL9
C
C     radial dim required
      CALL FILL()
 
      VPADD=.TRUE.
      CALL DEWDIM(P,GLABEL,OK)
C     clear any flags from screen
      CALL UNFLAG(.TRUE.)
C     draw the dimension on screen
      ENT = GLABEL
C     draw the thing into place
      CALL ALLDRW(ENT,P)
      VPADD=.FALSE.
C     dimension should be complete
      GOTO 10
C          
 999  CONTINUE
C     exit and unload points mode
      CALL MNUPTS()
      END
*
C       @(#)  256.1 date 12/16/89 dimn03.ftn Daxcad revision 1.8
      SUBROUTINE DIMN03(X1,Y1,X2,Y2,X3,Y3,OK)
C     =======================================
C
C1    vartype           R  R  R  R  R  R  L L
C1    iostatus          I  I  I  I  I  I  I O
C
C2    Subroutine DIMN03 constructs a label dimension
C2    from DOPFLAg logical flags and passed points info
C2    in X1Y1,X2Y2,X3Y3.
c2    This is stored as a radial dimension and uses unrelated
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/movdat.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/tmpdat.inc'
C
      INTEGER*2 JST,RCDCNT
      INTEGER*4 SEGTYP,NSTR
      REAL X1,Y1,X2,Y2,X3,Y3,STRVAL(4,10),BLWD,BLHT,TAGL,
     1     RANG,X4,Y4,DIMD,U1,U2
      LOGICAL OK,ARROIN,TXTOUT,LEFT,SUPLFT,SUPRGT
      CHARACTER*20 DIMNTX(10)*80
      INTRINSIC ABS,CHAR
      EXTERNAL CV0L14,VC00P9,DMNTXT
C
 10   CONTINUE
C     reset all record counter flags to zero
      CALL CLRTAL()
      JST=1
      DIMD=10.0
      CALL DMNTXT(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
C     find unit vector of  dim line
      CALL UCU001(X1,Y1,X2,Y2,U1,U2)
C     call routine to generate a leader dimension.
      CALL DIMLDR(X1,Y1,X2,Y2,X3,Y3,BLWD,BLHT,U1,U2,X4,Y4)
C     must be a full dimension type
C     set dim lines, first set header control data.
      CALL DMWREC(X2,Y2,0.0,X3,Y3,0.0,RTALLY(1),DIMCON,OK)
C     set text angle to be written
C     horizontal cos leader
      RANG=0.0
      TAGL=0.0
C     set the text data.
      CALL TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C     First MIP held data
      CALL ORGDAT()
C     set the header record count
      RCDCNT=1
C     Set the dimension  segment to be a line.
      SEGTYP=0
      ARROIN=.FALSE.
      TXTOUT=.FALSE.
C     next  record of dimension storage
      CALL FRSTRC(RCDCNT,ARROIN,TXTOUT,.FALSE.)
C     next record number of text strings
      CALL TEXREC(RCDCNT)
C     set left arrowhead information using supression state.
      SUPLFT=.FALSE.
      SUPRGT=.TRUE.
      CALL ARRREC(RCDCNT,SUPLFT,SUPRGT)
C     record  Dimension lines first set dimension line supression.
      CALL DIMREC(RCDCNT,SEGTYP,SUPLFT,SUPRGT)
C     next  projection lines,call with 1 for arc projections.
      CALL PRJREC(RCDCNT,SEGTYP,LEFT)
C     Finally write out the trailer record.
      CALL TRALRC(RCDCNT,BLWD,BLHT)
C     That's all for now
C
      END
 
 
C
C       @(#)  256.1 date 12/16/89 inst01.ftn Daxcad revision 1.8
      SUBROUTINE INST01()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT TEXT option list
C2    assumes at entry that the calling menu cell
C2    is hilited
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
C
      INTEGER C,TMEN,TCELL
      REAL X,Y
      LOGICAL CVERFY
      EXTERNAL TCURS,GTMCLO,CVERFY,
     +         INST10,GTHFMC,GTHIMC,INST02,INST03
C
C     Making single line the default insert text
      MEN=3
C     'X' is the token used by single line
      CCMD='X'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
C
      CELLN=TCELL
      GOTO 11
C
 10   CONTINUE
C     Tell him to select a menu option
      CALL DCPRNT(38)
C     find action required
      CALL TCURS(C,X,Y)
C
      TMEN=MEN
      TCELL=CELLN
C
 11   CONTINUE
C     if another major option,return to INSERT control routine
      IF (MEN.EQ.2) GOTO 605
C     select type of text action required
      IF (MEN.EQ.3) THEN
C        make sure caller highlighted
         CALL GTMCHI(MEN,CELLN)
         IF ( CVERFY(CCMD,'KLAHWV=') ) THEN
C           parameter change required
C           Highlight option that has been selected
            CALL GTHIMC(MEN,CCMD,'KLAHWV=',CELLN)
C           go handle the text status change selected
            CALL INST10()
C           Switch off finished with alteration
            CALL GTMCLO(MEN,CELLN)
            GOTO 10
         ELSE IF (CCMD.EQ.'X' ) THEN
C           go put the text in for him
            CALL INST02()
         ELSE IF (CCMD.EQ.'B' ) THEN
C           go put the text in for him
            CALL INST03()
         ELSE IF (CCMD.EQ.'l' ) THEN
C           l is the token for LABEL
            CALL DIMN02()
C           before return,ensure caller menu cell is not hilited
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C
      END IF
C     test for quit character
 
      IF ( MEN.EQ.2.OR.CCMD.EQ.'q') GOTO 605
      IF (MEN.EQ.3) GOTO 11
C     go try again
      GOTO 10
C
 605  CONTINUE
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C       @(#)  256.1 date 12/16/89 inst02.ftn Daxcad revision 1.8
C
      SUBROUTINE INST02()
C     ===================
C2    INSTXT inserts a single line of text where thr indicates
C
      include   'include/ndata.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/style.inc'
      include   'include/viewport.inc'
      include   'include/vntable.inc'
      include   'include/gtxt2.inc'
C
      CHARACTER INPT*80,CLRNAM*16
      REAL X,Y,TEMP
      INTEGER*2 NCHAR,P,FMIPOS,ENT
      INTEGER*4 C,TMEN,TCELL,NLEN
      LOGICAL CVERFY,OK,FIRST
C
      INTRINSIC ICHAR,COS,SIN,TAN
C
      EXTERNAL GETANS,GTHIMC,GTMCLO,CODET,FILL,
     +         CVERFY,INST10,MNLPTS,DRWTXT,
     1         DEW500,MNUPTS,NLEN
C
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
      FMIPOS=NMIPOS
      FIRST=.TRUE.
      ENT = TEXT
C
C     Enter the appropriate menu options for point selection
      CALL MNLPTS()
C
 55   CONTINUE
C
C     propmt for position to place text string
      CALL DCPRNT(217)
      CALL GETANS(C,X,Y)
C SPB 111194 - Now we have to include = too
      IF ( MEN .EQ. 3 ) THEN
         IF ( CVERFY(CCMD,'KLAHWV=') ) THEN
C           He forgot to change the parameters first,idiot!
C           Highlight option that has been selected
            CALL GTHIMC(MEN,CCMD,'KLAHWV=',CELLN)
C           make the parameter changes required
            CALL INST10()
C           Switch off finished with alteration
            CALL GTMCLO(MEN,CELLN)
C           try for text reference point again
            GOTO 55
         ELSE IF ( CCMD .EQ. 'c' ) THEN
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
            GOTO 55
         ELSE IF ( CCMD .EQ. 'k' ) THEN
C           colour
            CALL CCOLOR()
C           load colour control cell
            CALL MNICOL()
            GOTO 55
         ELSE
C           USER has ask for another text option
C           unload the point modes from the option menu
            GOTO 605
         END IF
      ELSE IF ( MEN .NE. 0 .OR.CVERFY(CCMD,'Qq')) THEN

         GOTO 605
      END IF
C
      CALL DPRMXP(39,INPT)
      NCHAR=MIN(NLEN(INPT),LEN(INPT))
      IF ( NCHAR.EQ.0 ) GOTO 55
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C
      CALL DEWC85(X,Y,TWIDTH,THIGT,TANGL,SLANT,JUST,CLFONT,
     +                  COLOUR,CLAYER,INPT,P,OK)
C
      CALL ALLDRW(ENT,P)
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C
      FIRST=.FALSE.
      IF ( OK ) GOTO 55
C
C
 605  CONTINUE
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C

      SUBROUTINE INST03()
C     ===================
C2    INSTXT inserts a single line of text where thr indicates
C
C SPB - 031194 - I think this is for including a file of text ...
C
      include   'include/ndata.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/movdat.inc'
      include   'include/swind.inc'
      include   'include/style.inc'
CAPOLLO:SUN
      include   'include/viewport.inc'
CAPOLLO:SUN
C
      CHARACTER INPT*80
      REAL X,Y,TEMP,RAD
      INTEGER*2 NCHAR,ENTYPE,P,D1,D2,FMIPOS,PDP,TTMIP,
     +          RELP,NXTRLR,NRRECS,NENTS,J,ZERO
      INTEGER*4 I,C,TMEN,TCELL,UNITN,NLEN,NLEN1,
     +          NLIN,MAXC
      LOGICAL CVERFY,OK,FIRST
C
      INTRINSIC ICHAR,COS,SIN,TAN
C
      EXTERNAL GETANS,GTHIMC,GTMCLO,CODET,FILL,RAD,
     +         NLEN1,CVERFY,INST10,MNLPTS,DRWTXT,
     1         DEW500,MNUPTS,NLEN,OPNOFF,CLOSUN,DEPRNT
C
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
      D1=0
      D2=0
      ZERO=0
C
C     Enter the appropriate menu options for point selection
      CALL MNLPTS()
C
      FMIPOS=NMIPOS
      FIRST=.TRUE.
C
 55   CONTINUE
C     prompt for position to place text block
      CALL DCPRNT(217)
      CALL GETANS(C,X,Y)
C
      IF ( CCMD.EQ. 'Q' .OR. CCMD.EQ.'q') GOTO 605
C
      IF ( MEN .EQ. 3 ) THEN
         IF ( CVERFY(CCMD,'KLAHWV') ) THEN
C           He forgot to change the parameters first,idiot!
C           Highlight option that has been selected
            CALL GTHIMC(MEN,CCMD,'KLAHWV',CELLN)
C           make the parameter changes required
            CALL INST10()
C           Switch off finished with alteration
            CALL GTMCLO(MEN,CELLN)
C           try for text reference point again
            GOTO 55
         ELSE IF ( CCMD .EQ. 'c' ) THEN
C           This is to miss out the group entry
C           which will be the last thing in the
C           master index
C           save the number of records,and entities
            TTMIP=NMIPOS-1
            IF ( TTMIP.LE.ZERO.OR.NMIPOS.LE.FMIPOS ) THEN
               GOTO 800
            END IF
            CALL DIR500(TTMIP,OK)
            RELP=IMBUFF(10)
            IF (RELP.LE.ZERO) THEN
               GOTO 800
            END IF
            CALL DRR950(RELP,OK)
            NRRECS=RLBUFF(4)
            PDP=RLBUFF(5)
C           this knocks the relational recored for groups
            NMIPOS=NMIPOS-1
C           The number of entries written.
            DO  20 I=1,PDP
               CALL INSCNL(FMIPOS,TEXT,1,FIRST,OK)
 20         CONTINUE
            GOTO 55
         ELSE IF (CCMD.EQ.'B' ) THEN
            CALL GTMCHI(MEN,CELLN)
            GOTO 55
         ELSE IF ( CCMD .EQ. '+' ) THEN
C           colour
            CALL CCOLOR()
            GOTO 55
         ELSE
C           USER has ask for another text option
C           unload the point modes from the option menu
            GOTO 605
         END IF
      ELSE IF ( MEN .NE. 0 ) THEN
         GOTO 605
      END IF
C
C     prompt for text file name
      CALL DPRMXP(81,INPT)
C
      IF( NLEN(INPT).EQ.0 ) GOTO 605
C
      INQUIRE(FILE=INPT,EXIST=OK)
      IF ( .NOT.OK ) THEN
           CALL DEPRNT(278)
           GOTO 605
      ENDIF
C
      CALL SIZE(INPT,MAXC,NLIN)
 
C      WRITE(UNIT=CBUFF,FMT='(A,2I4)')
C     + 'Line,max',NLIN,MAXC
C      CALL CPRINT(CBUFF)
C     open the text file
      CALL OPNOFF(INPT,UNITN,OK)
      IF (.NOT.OK) THEN
         CALL DEPRNT(278)
         GOTO 605
      END IF
      IF ( JUST.EQ.2.OR.JUST.EQ.5.OR.JUST.EQ.8 ) NLIN=NLIN/2
      IF ( MOD(JUST+0,3).NE.0  )  THEN
         Y=Y+(NLIN*PAPTOW*1.5*THIGT)*COS(RAD(TANGL))
         X=X-(NLIN*PAPTOW*1.5*THIGT)*SIN(RAD(TANGL))
      END IF
C
 45   CONTINUE
      READ(UNIT=UNITN,FMT='(A)',END=65) INPT
      NCHAR=MIN(NLEN(INPT),LEN(INPT))
      IF ( NCHAR .EQ.0  ) THEN
C      Y=Y-(PAPTOW*1.5*THIGT)
         Y=Y-(PAPTOW*1.5*THIGT)*COS(RAD(TANGL))
         X=X+(PAPTOW*1.5*THIGT)*SIN(RAD(TANGL))
         GOTO 45
      END IF
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
      CALL DEWC85(X,Y,TWIDTH,THIGT,TANGL,SLANT,JUST,CLFONT,
     +                  COLOUR,CLAYER,INPT,P,OK)
C
C      WRITE(10,*) 'GROUPING ',P,NMIPOS,OK
      CALL SSFLAG(P,0.0,0.0,D2,D1,.FALSE.)
C     draw the text string on screen
      ENTYPE=TEXT
      CALL ALLDRW(ENTYPE,P)
C      CALL DRWTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),RDBUFF(4),
C     +            RDBUFF(5),RDBUFF(6),CBUFF)
C
C
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
      IF ( .NOT. OK ) THEN
         GOTO 605
      END IF
C
      FIRST=.FALSE.
C      Y=Y-(PAPTOW*1.5*THIGT)
      Y=Y-(PAPTOW*1.5*THIGT)*COS(RAD(TANGL))
      X=X+(PAPTOW*1.5*THIGT)*SIN(RAD(TANGL))
C
      GOTO 45
C
 65   CONTINUE
C      WRITE(10,*) 'NUMBER OF TEXT LINES',NDATA
      IF (NDATA.GT.0) THEN
         CALL DCPRNT(50)
         CALL DEW050(P,OK)
C      WRITE(10,*) 'P,OK',P,OK
         OK=.TRUE.
C        ensure screen flags are cleared before leaving
         VNDATA=NDATA+1
         CALL UNFLAG(.TRUE.)
      ELSE
C        nothing in buffer,tell the idiot
         CALL DEPRNT(34)
      END IF
C
C     close and keep the file
      CALL CLOSUN(UNITN,.TRUE.,OK)
      GOTO 55
C
 605  CONTINUE
C     unload the point modes from the option menu
      CALL MNUPTS()
C     before return,ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      RETURN
C
 700  CONTINUE
      CALL DEPRNT(420)
      GOTO 605
 
 800  CONTINUE
      CALL DEPRNT(33)
      CALL GTMCLO(MEN,CELLN)
      GOTO 55
C
      END
C
      SUBROUTINE INST10()
C     ===================
C2    INST10 has all the variable associated with text input
C2       Height , Width , Angle , Slant , Justification
C SPB 031194 - 
C     And hopefully now with added thickness ...
C
      include   'include/ndata.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/gtxt2.inc'
      include   'include/vntable.inc'
C                                
      INTEGER*4 JMAP(9)
      INTEGER*4 I, TMEN, TCELL
      INTEGER*4 NLEN,OPHPNT(3),OPVPNT(3),MNUM,J
      REAL TTXTH
      DOUBLE PRECISION DTEMP
      INTRINSIC MOD,REAL
      CHARACTER JSTNAM*16       
      LOGICAL TOK
C
      EXTERNAL GTCLRC,GTPMEN,NLEN,GTMCLO,AEXPRN,GTHFMC
C
      DATA (JMAP(I),I=1,9)/3,6,9,2,5,8,1,4,7/
C
      TMEN=MEN
      TCELL=CELLN
C     store dictionary pointers for justifications
C     horizontals first
      OPHPNT(1)=155
      OPHPNT(2)=156
      OPHPNT(3)=157
C     now verticals
      OPVPNT(1)=158
      OPVPNT(2)=159
      OPVPNT(3)=160
C
      IF ( CCMD .EQ. 'K' ) THEN
C***************************************************
C         Change text justification.               *
C***************************************************
         MNUM = 8
         CALL MENPOP(MNUM,TOK)
         IF (TOK) THEN        
            J = ICHAR(CCMD)-64
            JUST = JMAP(J)
            JSTNAM = VNOUN(J+150)
            CALL GTDMEN(160,3)       
            GTMULT=.TRUE.
            CALL GTDMWT(160,3,JSTNAM)
         ENDIF
      ELSE IF ( CCMD .EQ. '=' ) THEN
C***************************************************
C         Change the thickness of the lines        *
C***************************************************
C SPB - 031194 - Add bits in to get the line thickness.
         CALL INSATT(CCMD)
C     Don't forget to un highlight the "Attributes" cell.
         CALL GTMCLO(TMEN, TCELL)                                  
C     CALL CFONT()
C     GTMULT = .TRUE.
C     CALL GTDMWT(409,3,TXTHK)
C     IMBUFF(12)=TXTHK
C SPB - 031194 - 
      ELSE IF ( CCMD .EQ. 'H' ) THEN
C***************************************************************
C              N E W   T E X T   H E I G H T                   *
C***************************************************************
C        user wants to change the text height
C        prompt for new text height and return expression
 111     CONTINUE
         CALL DPRMXP(26,CBUFF)
C
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and
C           put in an empty line return for more input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DTEMP,*111)
            IF ( DTEMP .NE. 0.0 ) THEN
               TTXTH=REAL(DTEMP)
C              Assumption is if user changes height he also
C              wishes the aspect ratio to stay the same
               TWIDTH=TTXTH*(TWIDTH/THIGT)
C              Set new text height
               THIGT=TTXTH
C              show it in cell
               CALL GTDMWR(162,3,THIGT,'(F8.2)')
C              update width also
               CALL GTDMWR(163,3,TWIDTH,'(F8.2)')
            ELSE
C              prompt user that zero height  is illegal
C              and try again
               CALL DEPRNT(27)
               GOTO 111
            END IF
         END IF
C
      ELSE IF ( CCMD .EQ. 'W' ) THEN
C***************************************************************
C              N E W   T E X T   W I D T H                     *
C***************************************************************
C        user wants to change the text width
C        prompt for new text width and return expression
 112     CALL DPRMXP(218,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and
C           put in an empty line return for more input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DTEMP,*112)
            IF ( DTEMP .NE. 0.0 ) THEN
               TWIDTH=REAL(DTEMP)
               CALL GTDMWR(163,3,TWIDTH,'(F8.2)')
            ELSE
C              prompt user that zero width is illegal
C              and try again
               CALL DEPRNT(121)
               GOTO 112
            END IF
         END IF
C
      ELSE IF ( CCMD .EQ. 'A' ) THEN
C***************************************************************
C              N E W   A N G L E  O F  T E X T                 *
C***************************************************************
C        user wants to change the text width
C        prompt for new text width and return expression
 113     CALL DPRMXP(219,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and
C           put in an empty line return for more input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DTEMP,*113)
            TANGL=REAL(DTEMP)
            CALL GTDMWR(164,3,TANGL,'(F8.2)')
         END IF
C
      ELSE IF ( CCMD .EQ. 'V' ) THEN
C***************************************************************
C              N E W   T E X T   S L A N T                     *
C***************************************************************
C        user wants to change the text slant.
C        prompt for new text slant and return expression
 114     CALL DPRMXP(220,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and
C           put in an empty line return for more input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DTEMP,*114)
            IF ( DTEMP.GT. 89.0 .OR. DTEMP .LT. -89.0 ) THEN
               CALL DEPRNT(242)
            ELSE
               SLANT=REAL(DTEMP)
               CALL GTDMWR(161,3,SLANT,'(F8.2)')
            END IF
         END IF
      END IF
 
      END
*
C       @(#)  256.1 date 12/16/89 mnitxt.ftn Daxcad revision 1.8
C
      SUBROUTINE MNITXT()
C     ===================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the INSERT TEXT option list.
C2
C2    Tokens used here are X and (unassigned).
C2
      include 'include/style.inc'
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'

      INTEGER*4 C
      EXTERNAL GTPMEN,GTCLRM,MNLTX1
      CHARACTER*16 CLRNAM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Load the text parameters.
      CALL MNLTX1()
C
C     Define position for last line two options in menu.
C
C2    X is the token for SINGLE LINE.
      CALL GTDMEN(165,3)
C     B is the token for Block
      CALL GTDMEN(166,3)
C     c is token foe cancel
      CALL GTDMEN(168,3)
C2    MULTIPLE LINE is unassigned.
C     CALL GTPMEN('Multiple line.',' ',3,15)
C2    L is the token for LABEL
      CALL GTDMEN(438,3)
C2
C     load colour control cell
      CALL MNICOL()
C
      END
