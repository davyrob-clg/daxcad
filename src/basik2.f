C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 basik2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CLSMAC()
C     SUBROUTINE GETLUN(LUN,FO,*)
C     SUBROUTINE ISCOM(COMAND,CALEN,FOUND)
C     SUBROUTINE ISPOP(VNNUM,COMAND,CALEN,FOUND)
C     SUBROUTINE ISPSP(VNNUM,COMAND,CALEN,FOUND)
C     SUBROUTINE MAC000(ENDMAC)
C     SUBROUTINE MAC010(SCREEN,OK)
C     SUBROUTINE MAC015(OK)
C     SUBROUTINE MAC021(OK)
C     SUBROUTINE MAC030(OK)
C     SUBROUTINE MAC040(OK,OK2)
C     SUBROUTINE MAC080(OK)
C     SUBROUTINE MAC086(OK)
C     SUBROUTINE MAC090(OK)
C     SUBROUTINE MAC099(ILLEX,OK)
C     SUBROUTINE MAC100()
C     SUBROUTINE MAC101(X,Y,OK)
C     SUBROUTINE MACP00(PRMEN,OK)
C     SUBROUTINE MACRUN(FILNAM)
C     SUBROUTINE OPNMAC(MACNAM,OK)
C     SUBROUTINE PAN(AVAR)
C     SUBROUTINE ZOOMI(AVAR)
C     SUBROUTINE ZOOMO(AVAR)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CLSMAC()
C     ===================
C1    no arguments required
C
C2    Subroutine CLSMAC closes the workfiles
C2    used for run-time control of MACRO-programs.
C
      include  'include/menun.inc'
      include  'include/macro.inc'
      include  'include/calc.inc'
      include  'include/lfu.inc'
      include  'include/keymac.inc'
C
      INTEGER*4 I
      LOGICAL OK
C
      MACTOK=.FALSE.
      MACOP=.FALSE.
      PROMPT=.TRUE.
      MSFOFF = .FALSE.
      MENUS=.TRUE.
      IF ( KEYMACACTIVE ) THEN
           KEYMACACTIVE = .FALSE.
C          delete temporay key macro file
           CALL DELETE(KEYMACFIL,OK)
      ENDIF
C     close the program file
      IF (MACUNT.NE.0) THEN
           CLOSE(UNIT=MACUNT)
      ENDIF
CIBM
C      LFU(MACUNT)=.FALSE.
CIBM
C     close the label file
      IF (LABUNT.NE.0) CLOSE(UNIT=LABUNT)
CIBM
C      LFU(LABUNT)=.FALSE.
CIBM
C
C     make sure all file access units are closed
      PROMPT=.TRUE.
      MENUS=.TRUE.
      DO 10 I=1,10
          IF(UNITOP(I:I).EQ.'O') THEN
              CLOSE(UNIT=UNITAR(I))
          ENDIF
10    CONTINUE
      MENHI=.FALSE.
      MACFIL=.FALSE.
      END
C
      SUBROUTINE GETLUN(LUN,FO,*)
C1    VARTYP             I4  L AEP
C2    IOSTAT              I  O
C3    This routine relates directly to the macro file commands
C3    it returns the lun status according to the array status
C3    flaf UNITAR
      include   'include/macro.inc'
      CHARACTER STRNG*80,CH
      INTEGER*4 NL,LUN
      DOUBLE PRECISION DLUN
      LOGICAL FO
      INTRINSIC NINT
C     lets do it
      IF(LUN.GT.10.OR.LUN.LT.1) THEN
          CALL DEPRNT(467)
          GOTO 100
      ENDIF
C     is it open ?
      IF(UNITOP(LUN:LUN).EQ.'O') THEN
          FO=.TRUE.
      ELSE
          FO=.FALSE.
      ENDIF
      RETURN
100   RETURN 1
      END
 
 
      SUBROUTINE ISCOM(COMAND,CALEN,FOUND)
C     ====================================
C      I/O type        C*(*)    L
C      I/O stat          I      O
C      ISCOM is used to compare the COMAND word
C      with the menus 3,1 to see if any of the
C      commands exist. If they do it returns FOUND
C      as true and retruns ccmd,nem,ncell from the
C      GTXT common blocks associated with the command
C      Have put in rotating buffer and also fixed
C      problem in FOR loop (OK not set in KIRKS MAC020)
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
      include 'include/macro.inc'
      INTEGER*4 I,CELIM,J,CNN,FCEL,FMEN,INDEX,NLEN,LENCOM
      INTEGER*4 CALEN,CELIM1,NMLIM1,CCOUNT,CLIM,NLEN1,TCN2,VNUM
      CHARACTER*80 CTXT,COMAND*(*),TEMP*40
      LOGICAL FOUND
      INTRINSIC INDEX
      EXTERNAL FOLDUP,NLEN,NLEN1,GTMNVN,ISPOP
      SAVE CLIM
C
      FOUND=.FALSE.
      TOGGLE=.FALSE.
      CN=0
C      IF(NTOK) THEN
C         NMLIM=3
C      ELSE
C         NMLIM=2
C      END IF
C
      TEMP=GMB(4,0)
C      write(10,*) '[iscomm] quitxt= ',quitxt
      IF(INDEX(QUITXT,COMAND(1:CALEN)).GT.0) THEN
         CCMD='q'
         MEN=4
         CELLN=1
         FOUND=.TRUE.
         RETURN
      END IF
 20   CONTINUE
      IF ( .NOT.TOGGLE ) THEN
         CLIM=0
      END IF
      CN=MOD(CN,3)+1
C     Which menu to search next.
      CNN=CELS(CN)
C     set the position the start searching from to same
C     as when we left.
      CN2=CELP(CNN)
C      WRITE(10,*) '[ISCOM] searching:',CNN,CN2
      CELIM=GML(CNN,6)
C     start by searching the same cell.
C      IF(CN2.GT.0) CN2=CN2-1
      CELIM1=MAX(MOD(CN2,CELIM),0)
C
C      WRITE(10,*)'[ISCOM] CNN,CELIM1,CELIM,CN,COMAND ',
C     +     CNN,CELIM1,CELIM,CN,'"',COMAND(1:CALEN),'"'
C
 30   CONTINUE
C
      IF(CNN.EQ.2.AND.CN2.EQ.0) GOTO 40
C      IF(CNN.EQ.2) CN2=MOD(CN2+1,CELIM+1)
C     This section of program searches the menu
C     number CN for a match with COMAND
C
C      WRITE(10,*)'[ISCOM]1 CN2= ',CN2,CELIM1
      CTXT=GMB(CNN,CN2)
C
      IF ( GMBC(CNN,CN2).EQ.' ' ) GOTO 40
C      IF ( NLEN(CTXT).EQ.0 ) GOTO 40
C     then disregard anything to the right of it.
      I=INDEX(CTXT,':')
      IF (I.GT.0) THEN
         TEMP=CTXT(:I-1)
         CTXT=TEMP
      END IF
C     Get rid of any blanks
      CALL CRUNCH(CTXT)
C     Fold menu text into upper case
      CALL FOLDUP(CTXT)
C     If there is a : in the menu cell
      LENCOM=NLEN1(CTXT)
C      WRITE(10,*)'Cell Text: COMMAND= ',COMAND(1:CALEN)
C      WRITE(10,*)'LENCOM= ',LENCOM,' Cell Text:= ',CTXT
C      WRITE(10,*)'INDEX= ',INDEX(COMAND(1:CALEN),CTXT(1:LENCOM))
      IF (INDEX(CTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1 
     +.AND. .NOT.  MACPOP) THEN
         TCN2 = CN2 + 1
         FOUND=.TRUE.
         CELLN=CN2+1
         MEN=CNN
         CELP(MEN)=CN2
         CCMD=GMBC(CNN,CN2)
C         WRITE(10,*)'[ISCOM] CELLN,MEN,CCMD ',CELLN,MEN,CCMD
         RETURN
      ELSE IF ( GMCMUL(CNN,CN2) .AND. MACPOP) THEN
C        right it is a popup
         TCN2=CN2+1
C        get the vnoun number
         CALL GTMNVN(VNUM,CNN,TCN2)
         CALL ISPOP(VNUM,COMAND,CALEN,FOUND)
         IF(FOUND) THEN
            CELLN=CN2+1
            MEN=CNN
            CELP(MEN)=CELLN
            RETURN
         ENDIF
      END IF
C
 40   CONTINUE
      CN2=MOD(CN2+1,CELIM)
      CLIM=CLIM+1
C      WRITE(10,*)'[ISCOM]2 CN2= ',CLIM,CN2,CELIM1
      IF (CLIM.LT.CELIM) GOTO 30
C     check if any other menu to search.
      IF (CN.LT.NMLIM) GOTO 20
 
 99   CONTINUE
      END
 
      SUBROUTINE ISPOP(VNNUM,COMAND,CALEN,FOUND)
C     ====================================
C1      I/O type          I4  C*(*)    I4    L
C1      I/O stat          I      I      I    O
C
C2      ISPOP does the same type of command processing
C2      as is done in ISCOM but only for the popup menus
C2      VNNUM :-  is the dictionary number of the menu
C2                text in the multiple popup cell being
C2                being tested.
C2      COMAND:-  is the macro command text being checked
C2      CALEN :-  is the length of the text being checked
C2      FOUND :-  TRUE if the text has been found in that
C2                popup
C2      CCMD, VNCCMD are set by this routine with the
C2      menu number MEN, CELLN being set by ISCOM on it's return
C
      include 'include/vntable.inc'
      include 'include/menun.inc'
      include 'include/menpop.inc'
C
         INTEGER*4 VNNUM,VNUM1,POPNO,J,CALEN,LENCOM,NLEN1,START,END,I
         LOGICAL FOUND
         CHARACTER*(*) COMAND
         CHARACTER*80 PTXT,TEMP
         INTRINSIC INDEX
         FOUND = .FALSE.
C
C        right lets get the popup menu number
         POPNO = VNMULT(VNNUM)
C
C        find the number of the popup attached to
C        that menu cell and if there isn't then just return
         IF(POPNO .EQ. 0) GOTO 50
C
C        check to see if it is a special popup
C        i.e. built within the code and not from
C        daxcad.menu.config identifiable from the
C        negative menu code
         IF(POPNO .LT. 0) THEN
           CALL ISPSP(VNNUM,COMAND,CALEN,FOUND)
           GOTO 50
         ENDIF
C
C        get the start and end points of the list of
C        cells in the popup menu
         START = MNPNTR(1,POPNO)
         END   = MNPNTR(2,POPNO)
C
C        check that we have not been input the popup header as
C        a repeat command and go back if this is the case
C
C        get the popup header text
         PTXT = VNOUN(VNNUM)
C        then disregard anything to the right of it.
         J=INDEX(PTXT,':')
         IF (J.GT.0) THEN
            TEMP=PTXT(:J-1)
            PTXT=TEMP
         END IF
C        Get rid of any blanks
         CALL CRUNCH(PTXT)
C        Fold menu text into upper case
         CALL FOLDUP(PTXT)
         LENCOM=NLEN1(PTXT)
         IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
            FOUND=.TRUE.
            VNCCMD = VNNUM
            CCMD   = VNTOKE(VNNUM)
C           successful lets exit
            GOTO 50
         ENDIF
C
C        right lets trundle thro' the list checking
C        if the popup cells match the MACRO command
C
         DO 30 I=START, END
C           get the vnoun number
            VNUM1 = MNPCEL(I)
            IF(VNTOKE(VNUM1).EQ.' ') GOTO 30
C           get the menu text
            PTXT = VNOUN(VNUM1)
C           then disregard anything to the right of it.
            J=INDEX(PTXT,':')
            IF (J.GT.0) THEN
              TEMP=PTXT(:J-1)
              PTXT=TEMP
            END IF
C           Get rid of any blanks
            CALL CRUNCH(PTXT)
C           Fold menu text into upper case
            CALL FOLDUP(PTXT)
C           If there is a : in the menu cell
            LENCOM=NLEN1(PTXT)
            IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
               FOUND=.TRUE.
               VNCCMD = VNUM1
               CCMD   = VNTOKE(VNUM1)
C              successful lets exit
               GOTO 50
            ENDIF
 30      CONTINUE
C
C
 50      CONTINUE
C
         END
C
C--------------------------------------------------------------------
C
 
      SUBROUTINE ISPSP(VNNUM,COMAND,CALEN,FOUND)
C     ====================================
C1      I/O type          I4  C*(*)    I4    L
C1      I/O stat          I      I      I    O
C
C2      ISPSP does the same type of command processing
C2      as is done in ISPOP but only for the popup menus
C2      That are built internally within the code not
C2      from daxcad.menu.config identifiable by their
C2       -ve menu codes
C2      VNNUM :-  is the dictionary number of the menu
C2                text in the multiple popup cell being
C2                being tested.
C2      COMAND:-  is the macro command text being checked
C2      CALEN :-  is the length of the text being checked
C2      FOUND :-  TRUE if the text has been found in that
C2                popup
C2      CCMD, VNCCMD are set by this routine with the
C2      menu number MEN, CELLN being set by ISCOM on it's return
C
      include 'include/vntable.inc'
      include 'include/menun.inc'
      include 'include/menpop.inc'
      include 'include/lfont.inc'
      include 'include/pendat.inc'
      include 'include/thklst.inc'
C
         INTEGER*4 VNNUM,VNUM1,POPNO,J,CALEN,LENCOM,NLEN1,START,END,I,
     +          THKNUM
         INTEGER*2 MASK,ENDVAL,AND_2
         LOGICAL FOUND
         CHARACTER*(*) COMAND
         CHARACTER*80 PTXT,TEMP
         INTRINSIC INDEX
         FOUND = .FALSE.
C
C        right lets get the popup menu number
         POPNO = VNMULT(VNNUM)
C
C        find the number of the popup attached to
C        that menu cell and if there isn't then just return
         IF(POPNO .EQ. 0) GOTO 50
 
C
C        check that we have not been input the popup header as
C        a repeat command and go back if this is the case
C
C        get the popup header text
         PTXT = VNOUN(VNNUM)
C        then disregard anything to the right of it.
         J=INDEX(PTXT,':')
         IF (J.GT.0) THEN
            TEMP=PTXT(:J-1)
            PTXT=TEMP
         END IF
C        Get rid of any blanks
         CALL CRUNCH(PTXT)
C        Fold menu text into upper case
         CALL FOLDUP(PTXT)
         LENCOM=NLEN1(PTXT)
         IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
            FOUND=.TRUE.
            VNCCMD = VNNUM
            CCMD   = VNTOKE(VNNUM)
C           successful lets exit
            GOTO 50
         ENDIF
C        set the dummy VNCCMD in case anyone tries to use it
         VNCCMD = 507
 
         IF(POPNO .EQ. -1) THEN
C        ******************************************************
C                           Line font
C        ******************************************************
C          get the start and end points of the list of
C          cells in the popup menu
           START = 1
           END   = NFONTS
C
C          right lets trundle thro' the list checking
C          if the popup cells match the MACRO command
C
           DO 30 I=START, END
              PTXT = FONTNM(I)
C             then disregard anything to the right of it.
              J=INDEX(PTXT,':')
              IF (J.GT.0) THEN
                TEMP=PTXT(:J-1)
                PTXT=TEMP
              END IF
C             Get rid of any blanks
              CALL CRUNCH(PTXT)
C             Fold menu text into upper case
              CALL FOLDUP(PTXT)
C             If there is a : in the menu cell
              LENCOM=NLEN1(PTXT)
              IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
                 FOUND=.TRUE.
                 CCMD   = CHAR(I)
C                successful lets exit
                 GOTO 50
              ENDIF
 30        CONTINUE
         ELSEIF(POPNO .EQ. -2) THEN
C        ******************************************************
C                        Plotter Type
C        ******************************************************
C          get the start and end points of the list of
C          cells in the popup menu
           START = 1
           END   = MODELS
 
C
C          right lets trundle thro' the list checking
C          if the popup cells match the MACRO command
C
           DO 40 I=START, END
              PTXT = TYPE(I)
C             then disregard anything to the right of it.
              J=INDEX(PTXT,':')
              IF (J.GT.0) THEN
                TEMP=PTXT(:J-1)
                PTXT=TEMP
              END IF
C             Get rid of any blanks
              CALL CRUNCH(PTXT)
C             Fold menu text into upper case
              CALL FOLDUP(PTXT)
C             If there is a : in the menu cell
              LENCOM=NLEN1(PTXT)
              IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
                 FOUND=.TRUE.
                 CCMD   = CHAR(I)
C                successful lets exit
                 GOTO 50
              ENDIF
 40        CONTINUE
         ELSEIF(POPNO .EQ. -3) THEN
C        ******************************************************
C                           Line width
C        ******************************************************
C          get the start and end points of the list of
C          cells in the popup menu
           START = 1
           END   = DFSTOT
           MASK = 15 * 1024
C          right let's check the zero width first
           WRITE(PTXT,'(I2,A,F4.2,A,F4.2,A,I2)') 0,':  ',
     +                    0.00,'   ',0.00,'  ',0
C          then disregard anything to the right of it.
           J=INDEX(PTXT,':')
           IF (J.GT.0) THEN
              TEMP=PTXT(:J-1)
              PTXT=TEMP
           END IF
C          Get rid of any blanks
           CALL CRUNCH(PTXT)
C          Fold menu text into upper case
           CALL FOLDUP(PTXT)
C          If there is a : in the menu cell
           LENCOM=NLEN1(PTXT)
           IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
               FOUND=.TRUE.
               CCMD   = CHAR(0)
C              successful lets exit
               GOTO 50
           ENDIF
 
C          right lets trundle thro' the list checking
C          the usr defined widths in the middle from the file
C          if the popup cells match the MACRO command
 
           DO 60 I=START, END
              THKNUM = FNTDFS(I)
C             Mask is f00. This picks out the line end number.
              ENDVAL = AND_2(PLTHKI(THKNUM),MASK)  / 1024
              WRITE(PTXT,'(I2,A,F4.2,A,F4.2,A,I2)') THKNUM,':  ',
     +                    LTHKR(1,THKNUM),'   ',LTHKR(2,THKNUM),'  ',ENDVAL
C             then disregard anything to the right of it.
              J=INDEX(PTXT,':')
              IF (J.GT.0) THEN
                TEMP=PTXT(:J-1)
                PTXT=TEMP
              END IF
C             Get rid of any blanks
              CALL CRUNCH(PTXT)
C             Fold menu text into upper case
              CALL FOLDUP(PTXT)
C             If there is a : in the menu cell
              LENCOM=NLEN1(PTXT)
              IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
                 FOUND=.TRUE.
                 CCMD   = CHAR(THKNUM)
C                successful lets exit
                 GOTO 50
              ENDIF
 60        CONTINUE
C
C          right let's check the OTHER  option on the bottom
           PTXT = VNOUN(506)
C          Get rid of any blanks
           CALL CRUNCH(PTXT)
C          Fold menu text into upper case
           CALL FOLDUP(PTXT)
           LENCOM=NLEN1(PTXT)
           IF (INDEX(PTXT(1:LENCOM),COMAND(1:CALEN)).EQ.1) THEN
               FOUND=.TRUE.
               CCMD = VNTOKE(506)
C              successful lets exit
               GOTO 50
           ENDIF
C
         ENDIF
C
 50      CONTINUE
C
         END
C
C-------------------------------------------------------------------------
C
      SUBROUTINE MAC000(ENDMAC)
C     =========================
C
C1    vartype              L
C1    iostatus             O
C
C2    Subroutine MAC000 reads the next line of
C2    a previously loaded macro-program into the
C2    buffer MACBUF,the line length into MACLEN.
C2    All comment lines and label lines are ignored
C2    such that only executable lines are returned.
C2    The current program line pointer MACLIN is updated
C2    and points to the line in the buffer.
C
      include  'include/macro.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 NLEN
      LOGICAL ENDMAC
      EXTERNAL NLEN
C
C     CALL CPRINT('[MAC000] ENTERED')
 10   CONTINUE
C     update line pointer
      MACLIN=MACLIN+1
C     test for end of program
      IF (MACLIN.LE.MACSIZ) THEN
C        read the line
         READ(UNIT=MACUNT,REC=MACLIN,ERR=99)MACLEN,MACBUF
C        test for comment
         IF (MACBUF(1:1).EQ.'*') GOTO 10
         IF (MACBUF(1:1).EQ.'&') GOTO 10
C        executable line,set scanning pointer
         MACCP=1
         ENDMAC=.FALSE.
      ELSE
C        end of program
         ENDMAC=.TRUE.
      END IF
C
C     CALL CPRINT('[MAC000] exitted')
      RETURN
C
 99   CONTINUE
C     error detected,abort run
      WRITE(UNIT=MACERR,FMT='(2A,I6)')
     +  DICT01(461)(1:NLEN(DICT01(461))),' ',MACLIN
      CALL EPRINT(MACERR)
      ENDMAC=.TRUE.
      END
C
      SUBROUTINE MAC010(SCREEN,OK)
C     ============================
C1    vartype            L     L
C1    iostatus           I     O
C
C2    This is the main prin routine for both screen and output
C2    files. The incoming variable SCREEN determines the direction
C2    of the output. true for screen and false for lun. The basic
C2    theory is to get the incoming line after the PRINT statement
C2    and parse it using the , and ; field separators
C
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
C
      LOGICAL SOK,OK,SCREEN,FO,PROTMP
      DOUBLE PRECISION DN,DVAL
      CHARACTER*80 OUTS,CSTRNG,STRING,BUFF,EC*2,TXTTMP
      CHARACTER*400 FINBUF,SEPCO*8,SEPSC*2,TFNBUF
      INTEGER*4 FL,SLEN,NL,I,FSEPS,LOOPS,EP,NFIELD,SF
      INTEGER*4 NLEN,FIELD(30),STARTP,LUN,NLEN1,TPOS
      DATA SEPCO,SEPSC/'     ,',';'/
C
C     get the string to print from the main buffer
      STRING=MACBUF(MACCP+1:)
C     crunch it to remove ambiguous spaces
C      CALL CHARCH(STRING)
C     call ther parsing routine which separates the line into
C     fields separated by commas and semicolons
      CALL PARSES(STRING,FIELD,NFIELD)
      IF(NFIELD.GT.29) THEN
          CALL DEPRNT(454)
          GOTO 99
      ENDIF
C     initialise various bits
      FINBUF=' '
      EP=1
      SF=1
      STARTP=1
      LOOPS=0
C     check for where the output is going either screen
C     or file is screen is true then its going to the VDU
C     otherwise its going to a file
      IF(.NOT.SCREEN) THEN
          BUFF=STRING(1:FIELD(1))
          EC=BUFF(NLEN1(BUFF):)
          IF(EC.EQ.','.OR.EC.EQ.';') BUFF=BUFF(:NLEN1(BUFF)-1)
          CALL AEXPRN(BUFF,DVAL,*100)
          CALL CEXPRN(BUFF,SOK)
          IF(SOK) THEN
C             too many fields in the line
              CALL DEPRNT(455)
              GOTO 99
          ELSE
              LUN=INT(DVAL)
              CALL GETLUN(LUN,FO,*100)
              IF(.NOT.FO) THEN
C                 Unit not open
                  CALL DEPRNT(456)
                  GOTO 99
              ENDIF
          ENDIF
          SF=2
          STARTP=FIELD(1)+1
      ENDIF
C     loop round taking each field in turn and sending
C     it via AEXPRN. The value returned either character or
C     numeric is concatonated in a big output buffer
      DO 10 I=SF,NFIELD
         OUTS='       '
         FSEPS=0
C     transfer to workin buffer
         BUFF=STRING(STARTP:FIELD(I))
         FL=NLEN(BUFF)
         IF(FL.EQ.0)GOTO 100
         IF(FL.GE.1) THEN
C     calculate the field separator distance
             FSEPS=INDEX(SEPCO,BUFF(FL:FL))+
     +             INDEX(SEPSC,BUFF(FL:FL))
             IF(FSEPS.GT.0) THEN
                FL=FL-1
             ENDIF
         ENDIF
C        if the only thing in the field was a separator then
C        just concatonate the number of space in the output buffer
         IF(FL.EQ.0)THEN
             SLEN=0
             GOTO 150
         ENDIF
C        strip of the field separator character if there wsa one
         TXTTMP=BUFF(:FL)
         BUFF=TXTTMP
C        Is the field padded with spaces ?
         IF(NLEN(BUFF).EQ.0) THEN
            SLEN=0
C           dont bother evaluating it then
            GOTO 150
         ENDIF
C        go get the value of this expression
         CALL AEXPRN(BUFF,DN,*99)
         CALL CEXPRN(CSTRNG,SOK)
C     is it character
         IF(SOK) THEN
C     if wee are writing to a file enclose all text in quotes
C             IF(SCREEN) THEN
              OUTS=CSTRNG
              SLEN=LENSTR
C     its numeric
          ELSE
              WRITE(OUTS,'(F20.5)') DN
C     clip off trailing zeroes only if integer or going to a file
              NL=NLEN(OUTS)
              IF(SBUF.EQ.'INTEGER'.OR.(.NOT.SCREEN)) THEN
40               IF(OUTS(NL:NL).EQ.'0') THEN
                   NL=NL-1
                   OUTS=OUTS(:NL)
                   GOTO 40
                 ELSEIF(OUTS(NL:NL).EQ.'.') THEN
                     NL=NL-1
                     OUTS=OUTS(:NL)
                 ENDIF
             ENDIF
C     justify the variable to the left
             CALL CRUNCH(OUTS)
             SLEN=NLEN1(OUTS)
         ENDIF
C    ok finished field evaluation concatonate
 150     CONTINUE
C        just a quick hack to avoid crashes
         IF(SLEN .EQ. 0 .AND. FSEPS .EQ. 0) THEN
            TPOS=1                            
         ELSE
            TPOS=SLEN+FSEPS
         ENDIF
         TFNBUF=FINBUF(:EP)//OUTS(:TPOS)
         FINBUF=TFNBUF
         EP=EP+SLEN+FSEPS
         IF(EP.GT.400) THEN
C            The output buffer is full
             CALL DEPRNT(457)
             GOTO 99
         ENDIF
         STARTP=FIELD(I)+1
10    CONTINUE
C     end of the evaluation loop
C     now we can output. But wait, what happens if the line
C     is bigger than 80 chars ?
C     where do we write this output ?
100   IF(SCREEN) THEN
C         override prompt control
          PROTMP=PROMPT
          PROMPT=.TRUE.
          FL=NLEN(FINBUF)
          LOOPS=INT(FL/80)
C     the number of loops determined by the 80 chars factor
C     the subscript values for length are based on the number of loops
          DO 50 I=0,LOOPS
             OUTS=FINBUF(I*80+1:(I+1)*80)
             IF(I.EQ.0) THEN
                 CALL CPRINT(OUTS(2:))
             ELSE
                 CALL CPRINT(OUTS)
             ENDIF
50        CONTINUE
          PROMPT=PROTMP
      ELSE
          IF(FILRW(LUN)) THEN
              WRITE(UNITAR(LUN),'(A)') FINBUF(2:NLEN1(FINBUF))
          ELSE
C             file is read only
              CALL DEPRNT(458)
              GOTO 99
          ENDIF
      ENDIF
      OK=.TRUE.
      RETURN
99    OK=.FALSE.
C
C     WRITE(10,*)'[MAC010] exitted'
      END
 
 
      SUBROUTINE MAC015(OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine MAC015 processes INPUT statements
C2    found in a macro-program line.The current
C2    version only supports the printing of single
C2    text strings,delimited by double quotes.
C
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
      CHARACTER BUFF*80,TEXT*80,INLIN*80,ASSBUF*80
      CHARACTER SVAR*80,SVAL*80,ERRBUF*80,QC*2,DQ*2
      INTEGER FIELD(30),NF,F,NLEN1,VFP,VARFD(30),SSP,VSP,I
      INTEGER*4 EC,LC,SP,IQ
      DOUBLE PRECISION        DVAL
      LOGICAL OK,SOK
      EXTERNAL NLEN1
C     wee flag just to keep delimeters out of the text expression
      F=0
C     parse the incoming line
      BUFF=MACBUF(MACCP+1:)
      CALL PARSES(BUFF,FIELD,NF)
C     Is there a prompt on the the input line
      INLIN=BUFF(1:FIELD(1)-1)
C     if there is a " on the first field then take it as a quote
      IF(INDEX(INLIN,'"').GT.0) THEN
 
C         is it valid ? if not then jump out and finish
          CALL AEXPRN(INLIN,DVAL,*100)
          CALL CEXPRN(INLIN,SOK)
C         If there is a prompt then then we want to ignore the
C         fact that it is there. Thus set an offset value
C         onto the field pointers
          F=1
      ELSE
C         no prompt string is present
          INLIN=' '
          F=0
      ENDIF
C     set the prompt string ( just a ? if no prompt string)
      TEXT= INLIN(:NLEN1(INLIN))//' ? '
C     ok get me the inputs from the user
      PRNTS=.TRUE.
      CALL CPRMXP(TEXT,INLIN)
C     parse the incoming variable line
      CALL PARSES(INLIN,VARFD,VFP)
      ERRBUF=INLIN
C     assign all the variables that have been requested
C     on the INPUT line. If any are missed out then
C     do a redo from start for that variable
      SSP=F*FIELD(1)+1
      VSP=1
      DO 10 I=1,NF-F
C         evaluate the variable name requested
          SVAR=BUFF(SSP:FIELD(I+F))
          EC=NLEN1(SVAR)
          IF(SVAR(EC:EC).EQ.','.OR.SVAR(EC:EC).EQ.';')SVAR=SVAR(:EC-1)
C         if we have not entered the required number of strings then
C         we must ask for a redo
          IF(I.GT.VFP) GOTO 70
C         evaluate the incomin value whether constant or variable
          SVAL=INLIN(VSP:VARFD(I))
          LC=NLEN1(SVAL)
          IF(SVAL(LC:LC).EQ.','.OR.SVAL(LC:LC).EQ.';')SVAL=SVAL(:LC-1)
60        QC=' '
C         Is the variable requested a character string
          IF(INDEX(SVAR,'$') .GT.0) THEN
              QC='"'
              VFP=I
C             now we have a string lets look for any quotes. If we find
C             them then double them up. this is to make aexprn think that
C             they are single quotes.
              IQ=0
              SVAL=INLIN(VSP:)
              BUFF=SVAL
              EC=NLEN1(SVAL)
200           IQ=IQ+1
              IF(IQ.GT.EC.OR.IQ.GT.LEN(SVAL)) GOTO 80
              DQ=SVAL(IQ:IQ)
              IF(DQ.EQ.'"') THEN
                  BUFF=SVAL(:IQ)//SVAL(IQ:)
                  SVAL=BUFF
                  IQ=IQ+1
                  EC=EC+1
              ENDIF
              GOTO 200
          ENDIF
C         concatonate variable name and value and stick in quotes
C         if the variable is a string
80        ASSBUF=SVAR(:NLEN1(SVAR))//'='//QC(:1)//
     +           SVAL(:NLEN1(BUFF))//QC
C         assign the variable name to its input value
          CALL AEXPRN(ASSBUF,DVAL,*70)
C         go down and update the pointers
          GOTO 20
C         duff up has occurred. Tell the duffer and get a new value
70        ERRBUF=DICT01(459)
     +        (1:NLEN1(DICT01(459)))//' '//SVAR(:NLEN1(SVAR))//' ? '
          PRNTS=.TRUE.
          CALL CPRMXP(ERRBUF,SVAL)
          INLIN = SVAL
          GOTO 60
20        CONTINUE
C         update the pointers
          VSP=VARFD(I)+1
          SSP=FIELD(I+F)+1
10    CONTINUE
      OK=.TRUE.
      RETURN
100   OK=.FALSE.
      END
 
 
      SUBROUTINE MAC021(OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine MAC021 processes NEXT statements
C2    found in a macro-program line.
C
      include  'include/macro.inc'
      include  'include/calc.inc'
C
      REAL RTMP
      LOGICAL OK,DONE
C
C     WRITE(10,*)'[MAC021] entered'
C     test for nesting level first
      IF (NFORLP.LT.1) THEN
C        NEXT without FOR
         CALL DEPRNT(399)
C         WRITE(10,*)'[MAC021] NEXT without FOR error'
         OK=.FALSE.
         RETURN
      END IF
C
C     must evaluate new loop control value
      VARD(FORLNS(2,NFORLP))=
     +   VARD(FORLNS(2,NFORLP))+DBLE(FORLRV(3,NFORLP))
      RTMP=REAL(VARD(FORLNS(2,NFORLP)))
      IF (FORLRV(3,NFORLP).GT.0.0) THEN
C        positive step
         DONE=RTMP.GT.FORLRV(2,NFORLP)
      ELSE
C        negative step
         DONE=RTMP.LT.FORLRV(2,NFORLP)
      END IF
      IF (DONE) THEN
C        clear this loop from the stack
         NFORLP=NFORLP-1
      ELSE
C        more to do,reset line number
         MACLIN=FORLNS(1,NFORLP)
      END IF
      OK=.TRUE.
C     WRITE(10,*)'[MAC021] exitted'
C
      END
C
      SUBROUTINE MAC030(OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine MAC030 processes GOTO statements
C2    found in a macro-program line.
C2    The label table is searched for a match with
C2    the given label,and processing redirected to
C2    that line.
C
      include  'include/macro.inc'
C
      INTEGER*4 SL,LN,I,NLEN,NLEN1
      DOUBLE PRECISION DN
      LOGICAL OK,EOS
      CHARACTER STRNG*80,LABEL*20
      EXTERNAL NLEN
C
C      WRITE(10,*)'[MAC030] entered'
C     go find label name given
      STRNG=MACBUF(MACCP:)
      CALL CRUNCH(STRNG)
C     hold on if the label has a dollar in it try and evaluate it
      IF(STRNG(NLEN1(STRNG):).EQ.'$') THEN
          CALL AEXPRN(STRNG,DN,*99)
          CALL CEXPRN(STRNG,OK)
       ENDIF
C     test for match with known labels
      IF (MACLAB.GT.0) THEN
C        set counter for search
         I=0
 10      CONTINUE
         I=I+1
         IF (I.LE.MACLAB) THEN
C           within bounds of table
C           read the label data
            READ(UNIT=LABUNT,REC=I,ERR=99)LN,LABEL
            IF (LABEL(2:20).EQ.STRNG(1:19)) THEN
C              found a match,set target to this one
               MACLIN=LN
               OK=.TRUE.
C     WRITE(*,*)'[MAC030] exitted'
               RETURN
            END IF
C           go try next one
            GOTO 10
         END IF
      END IF
C     got here if no label found
 99   CONTINUE
      CALL DEPRNT(406)
C      WRITE(10,*)'[MAC030] no label found'
      OK=.FALSE.
C
      END
C
      SUBROUTINE MAC040(OK,OK2)
C     =========================
C
C1    vartype            L  L
C1    iostatus           O  O
C
C2    Subroutine MAC040 processes IF statements
C2    found in a macro-program line.
C2    The statement following the condition
C2    is pre-loaded into the buffer if the
C2    condition is satisfied.OK2 is returned
C2    true if a statement pre-load has taken place.
C
      include  'include/macro.inc'
      include  'include/calc.inc'
C
      INTEGER*4 P,P1,P2,P3,P4,P5,IC,L,NLEN,NLEN1,FIELD(30),NF,I
      CHARACTER*80 CR1,CR2
      DOUBLE PRECISION DN
      REAL R1,R2
      LOGICAL OK,OK2,CONDIT,SOK
      CHARACTER COND(6)*4,STRNG*80,INTFIL*80,BUFF*80
      EXTERNAL FOLDUP,CRUNCH,NLEN,MAC000
C
      DATA COND/'.EQ.','.NE.','.GT.','.GE.','.LT.','.LE.'/
C
C     WRITE(10,*)'[MAC040] entered'
C     second expression bounded by P3 and P4
C     evaluate the two expressions
      BUFF=MACBUF(MACCP+1:)
      CALL PARSES (BUFF,FIELD,NF)
      STRNG=BUFF(1:FIELD(1))
      CALL CHARCH(STRNG)
      IF(STRNG(1:1).NE.'('.OR.STRNG(NLEN1(STRNG):).NE.')') GOTO 99
      IF(INDEX(BUFF(FIELD(1):FIELD(2)),'THEN').EQ.0) GOTO 99
C     get the conditional operator
      DO 200 IC=1,6
          P=INDEX(STRNG,COND(IC))
          IF(P.GT.0) THEN
              P2=P-1
              P3=P+4
              P1=2
              P4=NLEN1(STRNG)-1
              P5=FIELD(2)+MACCP+1
              GOTO 210
          ENDIF
200   CONTINUE
      GOTO 99
210   CR1= ' '
      CR2= ' '
      CALL AEXPRN(STRNG(P1:P2),DN,*99)
      CALL CEXPRN(CR1,SOK)
      R1=REAL(DN)
      IF(.NOT.SOK) THEN
          WRITE(INTFIL,'(F12.5)') R1
          READ(INTFIL,'(A)') CR1
      ENDIF
      CALL AEXPRN(STRNG(P3:P4),DN,*99)
      CALL CEXPRN(CR2,SOK)
      R2=REAL(DN)
      IF(.NOT.SOK) THEN
          WRITE(INTFIL,'(F12.5)') R2
          READ(INTFIL,'(A)') CR2
      ENDIF
      CONDIT=.FALSE.
C     go test correct condition
      GOTO (21,22,23,24,25,26) IC
 21   CONTINUE
C     .EQ. condition
      IF (CR1.EQ.CR2) THEN
         CONDIT=.TRUE.
      END IF
      GOTO 30
 22   CONTINUE
C     .NE. condition
      IF (CR1.NE.CR2) THEN
         CONDIT=.TRUE.
      END IF
      GOTO 30
 23   CONTINUE
C     .GT. condition
      IF (R1.GT.R2) THEN
         CONDIT=.TRUE.
      END IF
      GOTO 30
 24   CONTINUE
C     .GE. condition
      IF (R1.GE.R2) THEN
         CONDIT=.TRUE.
      END IF
      GOTO 30
 25   CONTINUE
C     .LT. condition
      IF (R1.LT.R2) THEN
         CONDIT=.TRUE.
      END IF
      GOTO 30
 26   CONTINUE
C     .LE. condition
      IF (R1.LE.R2) THEN
         CONDIT=.TRUE.
      END IF
 30   CONTINUE
C     must now decide if line or block conditional
C     get string after IF THEN
      STRNG=MACBUF(P5:)
      L=NLEN(STRNG)
      IF (L.GT.0) THEN
C        statement to process
         IF (CONDIT) THEN
C           preload the statement to buffer
C           backspace one line
C            MACLIN=MACLIN-1
C           recover line to avoid effects of fold to upper case
C           reload buffer with same line
C           extract statement
C            CALL MAC000(OK)
C            STRNG=MACBUF(P5:)
C            L=NLEN(STRNG)
            MACBUF=STRNG
            MACLEN=L
            MACCP=1
            OK=.TRUE.
C           set flag to indicate preload
            OK2=.TRUE.
         ELSE
C           condition not satisfied
C           go to next line
            OK=.TRUE.
            OK2=.FALSE.
         END IF
      ELSE
C        block IF,cannot process at the moment
         GOTO 99
      END IF
C     WRITE(10,*)'[MAC040] exitted'
      RETURN
C
 99   CONTINUE
C     syntax error
      OK=.FALSE.
      CALL DEPRNT(460)
C     WRITE(10,*)'[MAC040] exitted'
C
      END
C
C
C
      SUBROUTINE MAC080(OK)
C     ====================
C
C1    VARTYP            L
C2    IOSTAT            O
C
C3    This routine processes the macro open statement
C
      include   'include/macro.inc'
      include  'include/calc.inc'
      DOUBLE PRECISION DVAL
      INTEGER*4 LUN,NL,FL,I,NLEN1,ST,NF,FIELD(30)
      INTEGER*4 SP
      CHARACTER FILNAM*80,STRING*80,CSTRNG*80,BUFF*80,EC*2
      LOGICAL RW,FO,FE,OK
      INTRINSIC INDEX
      EXTERNAL NLEN1,DEPRNT,CRUNCH,
     +         GETLUN,FINDU1
C     get the read/write status within quotes
 
      STRING=MACBUF(MACCP+1:)
      CALL PARSES(STRING,FIELD,NF)
      IF(NF.NE.3) THEN
          CALL DEPRNT(443)
          GOTO 100
      ENDIF
      SP=1
      DO 50 I=1,NF
          BUFF=STRING(SP:FIELD(I))
          EC= BUFF(NLEN1(BUFF):NLEN1(BUFF))
          IF(EC.EQ.';'.OR.EC.EQ.';') BUFF=BUFF(:NLEN1(BUFF)-1)
          CALL AEXPRN(BUFF,DVAL,*100)
          CALL CEXPRN(CSTRNG,OK)
          GOTO (1,2,3) I
          CALL DCPRNT(443)
          GOTO 100
C     access code check
1         IF(OK) THEN
              IF(CSTRNG(1:1).EQ.'O') THEN
                  RW=.TRUE.
                  GOTO 60
              ELSEIF(CSTRNG(1:1).EQ.'I') THEN
                  RW=.FALSE.
                  GOTO 60
              ENDIF
          ENDIF
C         access code error in open statemnt
          CALL DCPRNT(462)
          GOTO 100
C     logical unit number check
2         IF(.NOT.OK) THEN
              LUN=INT(DVAL)
              CALL GETLUN(LUN,FO,*100)
              IF(FO) THEN
                  CALL DEPRNT(463)
                  GOTO 100
              ENDIF
          ELSE
              CALL DEPRNT(474)
              GOTO 100
          ENDIF
          GOTO 60
C         get the filename and test it
3         IF(OK) THEN
              FILNAM=CSTRNG
              INQUIRE(FILE=FILNAM,EXIST=FE)
C             if the file is marked for read only and it does not exist
C             then stop the macro. we want a known file name
              IF(.NOT.FE.AND.(.NOT.RW)) THEN
                 CALL DEPRNT(278)
                 GOTO 100
              ENDIF
C             This routine uses stream calls to open_append the the
C             file in question. This will tell whether the file
C             is open on the display manger. If so we can read but not
C             write. Thus the status of the user must be read only if
C             he wants to use it.
              CALL INQFS1(FILNAM,ST,OK)
C             A false return means the file cannot be wriiten to.
              IF ( .NOT. OK.AND.RW.AND.FE )  THEN
C                     The plonker wants to write to it tell him he cant.
                      CALL DEPRNT(277)
                      GOTO  100
              END IF
          ENDIF
60        CONTINUE
          SP=FIELD(I)+1
50    CONTINUE
C     very important. find a unit number.
      CALL FINDU1(UNITAR(LUN),OK)
      IF(.NOT.OK) THEN
         CALL DEPRNT(308)
         GOTO 100
      ENDIF
C     set status arrays
      UNITOP(LUN:LUN)='O'
      FILRW(LUN)=RW
C     all tests passed go ahead and open the file
      OPEN(UNIT=UNITAR(LUN),IOSTAT=ST,ERR=200,FILE=FILNAM,
     +      STATUS='UNKNOWN',ACCESS='SEQUENTIAL')
C     *******************************
C     return with no problems
C     *******************************
      OK=.TRUE.
      RETURN
C     ******************************************
C     error has occurred go back without opening
C     ******************************************
C     special error case the open statement cannot open the file.
C     something is definitly wrong.
200   CONTINUE
      CALL DEPRNT(287)
100   CONTINUE
      OK=.FALSE.
      END
 
 
 
 
      SUBROUTINE MAC086(OK)
C1    VARTYP            L
C2    IOSTAT            O
C3    This routine will close an existing open logical
C3    unit number. If the unit is already closed an error
C3    will be generated
C
      include   'include/calc.inc'
      include   'include/macro.inc'
      INTEGER*4 LUN,NL,NLEN,FIELD(3),NF,I
      DOUBLE PRECISION DLUN
      CHARACTER STRNG*80,FILNAM*80,BUFF*80
      LOGICAL OK,EOS,FO,SOK
      EXTERNAL GETLUN,NLEN1
C     set ok to false and only set true when everything works
      BUFF=MACBUF(MACCP+1:)
      CALL PARSES(BUFF,FIELD,NF)
      IF(NLEN(BUFF).EQ.0) THEN
          DO 10 I=1,10
              IF(UNITOP(I:I).EQ.'O') THEN
                  CLOSE(UNIT=UNITAR(I))
                  UNITOP(I:I)='C'
              ENDIF
 10       CONTINUE
          OK=.TRUE.
          RETURN
      ELSE
          CALL AEXPRN(BUFF,DLUN,*100)
          CALL CEXPRN(BUFF,SOK)
          IF(SOK) THEN
              CALL DCPRNT(474)
          ELSE
              LUN=INT(DLUN)
              CALL GETLUN(LUN,FO,*100)
              IF(.NOT.FO) THEN
                 CALL DCPRNT(456)
              ELSE
                 UNITOP(LUN:LUN)='C'
                 CLOSE (UNIT=UNITAR(LUN))
                 OK=.TRUE.
                 RETURN
              ENDIF
          ENDIF
      ENDIF
100   OK =.FALSE.
      END
 
      SUBROUTINE MAC090(OK)
C     =================
C1    VARTPE             L
C2    IOSTAT             O
C3    This rootine will set the ON ERROR GOTOor the
C3    ON EOF GOTO globallabels
C3    label after the goto. If no label is specified
C3    or is not found then the program will automaticaly
C3    stop
C
      include   'include/macro.inc'
C
      INTEGER*4 NLEN1,IERR,IEOF
      CHARACTER*80 STRNG
      LOGICAL OK
      EXTERNAL NLEN1,CRUNCH
C     lets get the label and assign it to the global
      STRNG=MACBUF(:NLEN1(MACBUF))
      CALL CHARCH(STRNG)
      IERR=INDEX(STRNG,'ONERRORGOTO')
      IEOF=INDEX(STRNG,'ONEOFGOTO')
C     label can be preceeded by & or not
C     select global
      IF(IERR.EQ.1) THEN
          LABERR=STRNG(12:NLEN1(STRNG))
          IF(LABERR(1:1).EQ.'&') LABERR=LABERR(2:)
      ELSEIF(IEOF.EQ.1) THEN
          LABEOF=STRNG(10:NLEN1(STRNG))
          IF(LABEOF(1:1).EQ.'&') LABEOF=LABEOF(2:)
      ELSE
          CALL DEPRNT(466)
          GOTO 100
      ENDIF
      OK=.TRUE.
      RETURN
100   OK=.FALSE.
      END
 
      SUBROUTINE MAC099(ILLEX,OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
      include  'include/macro.inc'
C
      DOUBLE PRECISION DN
      LOGICAL OK,TRIP,ILLEX
      CHARACTER*2 CH,STRING*80
      INTEGER*2 NL,I
      INTEGER*4 NLEN
      EXTERNAL CRUNCH,AEXPRN
C
      OK = .TRUE.
      TRIP = .FALSE.
C     Check that = is no in string
      STRING = MACBUF
      CALL CHARCH(STRING)
      NL = NLEN(STRING)
      DO 500 I=1,NL
          CH=STRING(I:I)
          IF(CH.EQ.'"'.AND..NOT.TRIP) THEN
              TRIP=.TRUE.
              GOTO 500
          ELSEIF(CH.EQ.'"'.AND.TRIP) THEN
              TRIP=.FALSE.
              GOTO 500
          ENDIF
          IF(CH.EQ.'='.AND.TRIP) THEN
              ILLEX  =.TRUE.
              RETURN
          ENDIF
500   CONTINUE
C
C      WRITE(*,*)'[MAC099] entered'
C     try it as arithmetic expression
      CALL AEXPRN(STRING,DN,*99)
      ILLEX = .FALSE.
      OK=.TRUE.
C      WRITE(*,*)'[MAC099 normal exit dn=',DN
      RETURN
 99   CONTINUE
C     WRITE(*,*)'[MAC099] error exit buf=',STRING
      OK=.FALSE.
C
      END
C
C
      SUBROUTINE MAC100()
C     ===================
C
C2    This routine allows selection of predefined macros
C2    to be run. All current macros are run up from a defined
C2    directory file created in the CREATE function. The pathname
C2    is selected in the SELECT function
C
C     Curently in testin mode.
C
      include  'include/menun.inc'
      include  'include/macro.inc'
      include  'include/calc.inc'
      include  'include/gtxt2.inc'
      include  'include/wildc.inc'
      include  'include/product.inc'
      CHARACTER FILNAM*80,SFILE*80
      CHARACTER*80 OUTFIL
      INTEGER FUNIT,RECL,HUNIT
      INTEGER*4 NLEN1,NLEN,PHNUM
      LOGICAL OK,QUIT
      EXTERNAL NLEN1,DISFIL,CRUNCH,MACRUN,MNIMAS,MNICAL,NLEN
C 
C     set flag to show that the call to the MACRO is
C     from menpop ie we're looking for a popup
      MACPOP = .FALSE.
C     If this is being called form the main routine
C     then go straight to MAC100
 
      IF(ATMAC.AND.MSFOFF) THEN
 
 
           FILNAM = MACBUF
           ATMAC = .FALSE.
           CALL CLSMAC()
           GOTO 210
 
 
      ENDIF
      IF(CCMD.NE.'Z') THEN
          FILNAM=PRNAM(1:NLEN(PRNAM))//'.mac'
          CALL MACRUN (FILNAM)
          GOTO 200
      ENDIF
CIBM|PC386
C      SFILE=PATHN(2)(1:NLEN1(PATHN(2)))//'mac.msf'
C      CALL CRUNCH(SFILE)
C      INQUIRE(FILE=SFILE,EXIST=OK)
C      IF ( MFILE ) THEN
C         FILNAM=PATHN(2)(1:NLEN1(PATHN(2)))//WILDC(2)
C         CALL CRUNCH(FILNAM)
C         CALL DIRFIN(SFILE,FILNAM,OK)
C      ENDIF
CIBM|PC386
C     call the display file routine to select a group of macro
C     programs. If the file is not found then it will return with
C     a not ok flag and allow user input
10    PHNUM = 4
      CALL DISFIL(SFILE,FILNAM,PHNUM,OK,QUIT)
      IF(QUIT) GOTO 200
      IF(.NOT.OK) THEN
C         Either the MSF file has not been found or the
C         user has decided to select his own little macro
          CALL DPRMXP(439,FILNAM)
          IF(NLEN(FILNAM).EQ.0) GOTO 200
      ENDIF
210   CONTINUE
      CALL MACRUN(FILNAM)
      IF(.NOT.MACOP) GOTO 10
200   CONTINUE
C     tidy up the .msf file if on auto
      IF ( MFILE ) CALL DELETE(SFILE,OK)
      CALL MNIMAS()
      CALL MNICAL()
      CALL MNIDIS()
      END
 
      SUBROUTINE MAC101(X,Y,OK)
C     =========================
C     This subroutine gets called by TCURS when MACOP is set
C     and interprets the current macro line.
C
      include  'include/macro.inc'
      include  'include/menun.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
      include 'include/curwin.inc'
      include 'include/server.inc'
 
      REAL X,Y
      LOGICAL OK,ENDMAC,OK2,EOS,EOF,SCREEN,PRMEN,ILLEX
      CHARACTER*80 LINES(100),LINBUF,TEMP1,TEMP2,FILNAM
      CHARACTER*30 CGPRMP(22)
      CHARACTER*8 IGLABL,WORD*80,KEY*1,INPL*1
      INTEGER*4 WL,P,NLEN1,FIELD(30),NF,NLEN
      DOUBLE PRECISION DN
      EXTERNAL NLEN1,NLEN
C
C     MACTOK is used to return to the DAXCAD command
C     interpreter because the end of the current
C     line has not been completely read i.e. only
C     not all the tokens for this line have been issued
C     initialiser for token interpreter
C      write(10,*)'[MAC101] MAC101 CALLccmd= ',ichar(ccmd),' men= ',men
 
      IF(GINPUT) THEN
C        reset values
         GINPUT=.FALSE.
         OK=.TRUE.
C        preload variable X,Y with graphical result.
         WRITE(TEMP1,'(A,E14.7)')'X=',X
         CALL AEXPRN(TEMP1,DN,*98)
         WRITE(TEMP1,'(A,E14.7)')'Y=',Y
         CALL AEXPRN(TEMP1,DN,*98)
C========== NEW CODE 1/5/86 (enable layer control)
C        Clear the display option menu.
         CALL GTCLRM(4)
C        Insert Layer option menu
         DISLAY=.FALSE.
         CALL MNIDIS()
C====================================================
         IF(BIGWIN) THEN
             XWIDTH =   8.0
             YWIDTH =   8.0
             WIDTHL =   16.0
         ELSE
             XWIDTH =   0.5
             YWIDTH =   0.5
             WIDTHL =   1.0
         ENDIF
         RETURN
      END IF
      IF(MACTOK.OR.NTOK) THEN
         MFIRST=.FALSE.
         CALL MAC050(X,Y,OK)
C      write(10,*)'[MAC101] CHANGE  ccmd= ',ichar(ccmd),' men= ',men
         IF(.NOT.OK) GOTO 98
         RETURN
      END IF
C
 20   CONTINUE
C     is the HOLD set
      IF(HOLD) THEN
          KEY=CHAR(0)
          CALL KBHOLD(KEY,OK)
C         cancel the HOLDMAC if return or M3
          IF(KEY.EQ.CHAR(150).OR.KEY.EQ.CHAR(99))
     +       HOLD=.FALSE.
C         if space bar next step
          IF(KEY.NE.CHAR(32)) GOTO 20
      ENDIF
C     now start to process the program lines
 26   KEY=CHAR(0)
      CALL KBINTR(KEY,OK)
      IF((KEY.EQ.'E'.OR.KEY.EQ.'e').AND.OK) THEN
C         program aborted
          CALL DEPRNT(451)
          GOTO 99
      ENDIF
      CALL MAC000(ENDMAC)
C     debugger flag
      CALL FOLDUP(MACBUF)
      IF(DBG) THEN
          PRNTS=.TRUE.
          CALL CPRINT('"'//MACBUF(:NLEN1(MACBUF))//' "')
      ENDIF
C     set search window
      IF(BIGWIN) THEN
          XWIDTH =   8.0
          YWIDTH =   8.0
          WIDTHL =   16.0
      ELSE
          XWIDTH =   0.5
          YWIDTH =   0.5
          WIDTHL =   1.0
      ENDIF
C     diagnostic for Server version
      IF (DBG.AND.SERVER) WRITE(*,'(2A)') 
     +      '[DIAG:]', MACBUF(:NLEN1(MACBUF))
 
      IF (ENDMAC) GOTO 99
C     Any blank lines should be ignored directly
      IF(NLEN(MACBUF).EQ.0) GOTO 26
 25   CONTINUE
C     start point for pre-loaded instructions
C     extract first word from line
      CALL PARSES(MACBUF,FIELD,NF)
      WORD=MACBUF(:FIELD(1))
      MACCP=FIELD(1)
      CALL CHARCH(WORD)
C     fold to upper case
C     ***************************************
C     diagnostic output of each control word
C      CALL CPRINT('"'//WORD(1:NLEN1(WORD))//'"')
C     ***************************************
C     test for control word
      IF (WORD(1:1).EQ.'*' .OR. WORD(1:1).EQ.'''') THEN
C        comment line
         OK=.TRUE.
      ELSE IF (WORD(1:1).EQ.'&') THEN
C        label line
         OK=.TRUE.
      ELSE IF (WORD.EQ.'REM') THEN
C        process the print statement
         OK=.TRUE.
      ELSE IF (WORD.EQ.'PRINT'.OR.WORD.EQ.'?') THEN
C        process the print statement
         SCREEN=.TRUE.
         CALL MAC010(SCREEN,OK)
      ELSE IF (WORD.EQ.'ALERT'.OR.WORD.EQ.'?') THEN
C        process the print statement
         SCREEN=.TRUE.
         CALL MAC011(OK)

      ELSE IF (WORD.EQ.'PROMPT') THEN
C        process the print statement
         PRMEN=.TRUE.
         CALL MACP00(PRMEN,OK)
      ELSE IF (WORD.EQ.'MENU') THEN
C        process the print statement
         PRMEN=.FALSE.
         CALL MACP00(PRMEN,OK)
      ELSE IF (WORD.EQ.'INPUT') THEN
C        process the print statement
         CALL MAC015(OK)
      ELSE IF (WORD.EQ.'FOR') THEN
C        process the FOR statement
         CALL MAC020(OK)
      ELSE IF (WORD.EQ.'NEXT') THEN
C        process the NEXT statement
         IF ( OK ) GOTO 99
         CALL MAC021(OK)
      ELSE IF (WORD.EQ.'IF') THEN
C        process the IF statement
         CALL MAC040(OK,OK2)
         IF (OK.AND.OK2) GOTO 25
      ELSE IF (WORD.EQ.'GOTO') THEN
C        process the GOTO statement
         CALL MAC030(OK)
      ELSE IF (WORD.EQ.'CALL') THEN
C        process the CALL statement
         CALL MAC060(OK)
      ELSE IF (WORD.EQ.'OPEN') THEN
C        process the open statement
         CALL MAC080(OK)
      ELSE IF (WORD.EQ.'INPUT#') THEN
C        process the input file statement
         CALL MAC082(OK,EOF)
            IF(OK.AND.EOF) GOTO 25
      ELSE IF (WORD.EQ.'PRINT#') THEN
C        process the print to a file statement
         SCREEN=.FALSE.
         CALL MAC010(SCREEN,OK)
      ELSE IF (WORD.EQ.'GOSUB') THEN
C        process the print to a file statement
         CALL MAC250(OK)
      ELSE IF (WORD.EQ.'RETURN') THEN
C        process the print to a file statement
         CALL MAC260(OK)
      ELSE IF (WORD.EQ.'CLOSE') THEN
C        process the close statement
         CALL MAC086(OK)
      ELSE IF (WORD.EQ.'ON') THEN
C        process the ON ERROR/EOF GOTO statement
         CALL MAC090(OK)
      ELSE IF (WORD.EQ.'FSMON') THEN
C         sets the MSF flag
          MSFOFF = .FALSE.
          OK =.TRUE.
      ELSE IF (WORD.EQ.'FSMOFF') THEN
C         sets the MSF flag
          MSFOFF = .TRUE.
          OK =.TRUE.
      ELSE IF (WORD.EQ.'DBGON') THEN
C         Debugger flag now prints code out
         DBG=.TRUE.
         OK=.TRUE.
      ELSE IF (WORD.EQ.'DBGOFF') THEN
C         Debugger flag now off
         DBG=.FALSE.
         OK=.TRUE.
      ELSE IF (WORD.EQ.'HOLDMAC') THEN
C        The hold flag is set halt execution
         HOLD=.TRUE.
         CALL DCPRNT(452)
         OK=.TRUE.
      ELSE IF (WORD.EQ.'INVOKE') THEN
C         Start running an external program lest hope we come back
          CALL MAC200(OK)
      ELSE IF (WORD.EQ.'REWIND') THEN
C         Start running an external program lest hope we come back
          CALL MAC210(OK)
      ELSE IF (WORD.EQ.'STOP') THEN
C         stop the macro
          WRITE(UNIT=LINBUF,FMT='(A,I4)') 'Macro terminated at line',
     +    MACLIN
          PRNTS=.TRUE.
          CALL CPRINT(LINBUF)
          GOTO 99
      ELSE IF (WORD.EQ.'PICKZONE') THEN
C         allow user control of search window
          CALL MAC220(OK)
      ELSE
C        test for expressions,etc as last resort
         P=INDEX(MACBUF,'=')
         IF (P.GT.0.AND.WORD.NE.'GINPUT') THEN
C           try to evaluate expression
            CALL MAC099(ILLEX,OK)
C           Correct assignment
            IF(.NOT.OK.OR..NOT.ILLEX) GOTO 98
            P=0
C         ENDIF
C         IF(P.EQ.0) THEN
         ELSE
C           cannot recognise the statement
C           It must be a geometry command. ?
            MFIRST=.TRUE.
            NTOK=.FALSE.
            GINPUT=.FALSE.
C           start again (Note set to 0 because NVCHR increments
C           pointer MACCP automatically)
            MACCP=0
            CALL MAC050(X,Y,OK)
            IF(.NOT.OK) GOTO 98
C           is somebody trying to get a new macro started
            IF(CCMD.EQ.'Z'.AND..NOT.NTOK) THEN
                CALL MAC270(OK)
                IF(.NOT.OK) GOTO 98
            ENDIF
            RETURN
C            OK=.FALSE.
         END IF
      END IF
98    IF (.NOT.OK) THEN
C        error detected in program line
         WRITE(UNIT=MACERR,FMT='(A,I6,A,A)')
     +   DICT01(453)(1:NLEN1(DICT01(453))),MACLIN,' ',
     1   MACBUF(:NLEN1(MACBUF))
         PRNTS=.TRUE.
C        error menu
         CALL ERRORDIALOG(MACERR)
         GOTO 99
      END IF
      GOTO 20
 99   CONTINUE
      CALL CLSMAC()
      XWIDTH =   8.0
      YWIDTH =   8.0
      WIDTHL =   16.0
      END
C
C     Macro calls
C
      SUBROUTINE MACP00(PRMEN,OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Dont quite know what this one does but its shorter
C
      include  'include/macro.inc'
C
      LOGICAL OK,PRMEN
      CHARACTER WORD*80
      EXTERNAL CRUNCH,DEPRNT
C
      OK=.TRUE.
      WORD=MACBUF(MACCP+1:)
      CALL CRUNCH(WORD)
      IF(PRMEN) THEN
         IF ( WORD.EQ.'ON' ) THEN
             PROMPT=.TRUE.
          ELSE IF ( WORD.EQ.'OFF' ) THEN
             PROMPT=.FALSE.
          ELSE
             CALL DEPRNT(396)
             OK=.FALSE.
          END IF
      ELSE
          IF ( WORD.EQ.'ON' ) THEN
             MENUS=.TRUE.
          ELSE IF ( WORD.EQ.'OFF' ) THEN
             MENUS=.FALSE.
          ELSE
             CALL DEPRNT(397)
             OK=.FALSE.
          END IF
      ENDIF
C     WRITE(10,*)'[MAC010] exitted'
      END
C
C
      SUBROUTINE MACRUN(FILNAM)
C     ==================
C
      include  'include/menun.inc'
      include  'include/macro.inc'
      include  'include/calc.inc'
      include  'include/gtxt2.inc'
      include 'include/vntable.inc'
      include 'include/curwin.inc'
      include 'include/product.inc'
      include 'include/server.inc'
C
C
      LOGICAL OK,ENDMAC,OK2
      CHARACTER*80 LINES(100),LINBUF,TEMP1,TEMP2,FILNAM
      CHARACTER*30 CGPRMP(22)
      CHARACTER*8 IGLABL
      INTEGER*4 WL,P,NLEN,TMEN,TCELL
      LOGICAL EOS
      CHARACTER*80 WORD
      EXTERNAL NLEN,DEPRNT
C     initialise the file access status
C
      TMEN=MEN
      TCELL=CELLN
 3    CONTINUE
C     initialize calculator
      CALL INCALC()
 
C     IF ( CCMD.NE.'Z' ) THEN
C          CALL DPRMXP(439,FILNAM)
C         IF (NLEN(FILNAM).EQ.0) GOTO 99
C      ELSE
C         FILNAM=PRODUCT(1:NLEN(PRODUCT))//'.mac'
C      END IF
C
      CALL OPNMAC(FILNAM,OK)
      IF (.NOT.OK) THEN
C        error in opening macro file
         CALL DCPRNT(434)
         MACOP=.FALSE.
         GOTO 99
      END IF
C     show number of lines and labels
C      WRITE(UNIT=MACERR,FMT='(A,I6)') DICT01(449)
C     +     (1:NLEN(DICT01(449))),MACSIZ
C      CALL CPRINT(MACERR)
C      WRITE(UNIT=TEMP1,FMT='(A,I6)') DICT01(450)
C     +     (1:NLEN(DICT01(450))),MACLAB
C      CALL CPRINT(TEMP1)
C
C     set the macro in operation to true
      ntok=.FALSE.
      ATMAC=.FALSE.
      GINPUT=.FALSE.
      MACOP=.TRUE.
      MACTOK=.FALSE.
      TOGGLE=.FALSE.
      NOWDS=0
      DBG=.FALSE.
      NMLIM=3
      CN2=0
C========== NEW CODE 1/5/86 (enable layer control)
      IF ( .NOT.SERVER ) THEN
C        server version starts in create menu
         CALL GTHFMC(4,'q',OCEL)
         QUITXT=GMB(4,OCEL-1)
         CALL CRUNCH(QUITXT)
         CALL FOLDUP(QUITXT)
         CALL CRUNCU(QUITXT)
C         WRITE(10,*)'[MAC100] OCEL,QUITXT ',OCEL,QUITXT
C        Clear the display option menu.
         CALL GTCLRM(4)
C        Insert Layer option menu
         DISLAY=.FALSE.
         CALL MNIDIS()
      ENDIF
C====================================================
C     set for loop counter to zero
      NFORLP=0
C     set current line pointer to zero
      MACLIN=0
C     clear line buffer
      MACBUF=' '
      RETURN
C
 99   CONTINUE
      CALL CLSMAC()
      CALL GTMCLO(TMEN,TCELL)
C      IF (FILNAM.NE.'Q') GOTO 3
C      WRITE(*,'(//A)')' END OF MACRO PROGRAM'
      MACOP=.FALSE.
 
C
      END
C
      SUBROUTINE OPNMAC(MACNAM,OK)
C     ============================
C
C1    vartype            C*(*)  L
C1    iostatus             I    O
C
C2    Subroutine OPNMAC opens a macro-program source file
C2    and copies it into an unformated work file connected
C2    to MACUNT,and creates a label look-up table in the
C2    unformatted file connected to LABUNT.If successful
C2    OK returned true.
C
      include  'include/macro.inc'
      include  'include/nbuff.inc'
      include 'include/lfu.inc'
      include 'include/vntable.inc'
      include 'include/gosubs.inc'
C
      INTEGER*4 TUNIT,L,NLEN,I,J
      LOGICAL OK
      CHARACTER*(*) MACNAM,TEMP*20
C
      EXTERNAL OPNOFF,NLEN,DEPRNT
C
C     open the source file
      CALL OPNOFF(MACNAM,TUNIT,OK)
      IF (.NOT.OK) RETURN
C     open work file and label file
      CALL OURSCR(MACUNT,84,OK)
      IF (.NOT.OK) THEN
         CLOSE(UNIT=TUNIT,STATUS='KEEP')
CIBM
C         LFU(TUNIT)=.FALSE.
CIBM
         RETURN
      END IF
      CALL OURSCR(LABUNT,24,OK)
      IF (.NOT.OK) THEN
         CLOSE(UNIT=TUNIT,STATUS='KEEP')
CIBM
C         LFU(TUNIT)=.FALSE.
CIBM
         CLOSE(UNIT=MACUNT)
CIBM
C         LFU(MACUNT)=.FALSE.
CIBM
         RETURN
      END IF
C     now have source file,work file and label file open
C     set global control variables to initial values
C     make sure msfoff are off by default
      MSFOFF = .TRUE.
      GSPNT = 0
      MACSIZ=0
      MACLAB=0
C     set file acces status variable
      UNITOP='CCCCCCCCCC'
      HOLD=.FALSE.
C     status of search window
      BIGWIN=.FALSE.
C     status of print and input statments
      PRNTS=.FALSE.
C     now read program source
C     typed nput flag false
      TYPED = .FALSE.
 10   CONTINUE
      READ(UNIT=TUNIT,FMT='(A)',END=20,ERR=30)MACBUF
C     increment line counter
      MACSIZ=MACSIZ+1
C     write to work file
      L=NLEN(MACBUF)
      WRITE(UNIT=MACUNT,REC=MACSIZ,ERR=30)L,MACBUF
C     write to label file if necessary
      IF(MACBUF(1:1).EQ.'&') THEN
         CALL FOLDUP(MACBUF)
C        got a label line
C        check if label already exists.
         I=0
 5       CONTINUE
         I=I+1
         IF ( I.LE.MACLAB ) THEN
            READ(UNIT=LABUNT,REC=I) J,TEMP
            IF ( TEMP(1:NLEN(TEMP)).EQ.MACBUF(1:NLEN(MACBUF)) ) THEN
               CALL DEPRNT(442)
               WRITE(UNIT=CBUFF,FMT='(I4,A,I4)')
     +         MACSIZ,DICT01(478)(1:NLEN(DICT01(478))),J
               PRNTS=.TRUE.
               CALL CPRINT(CBUFF)
               OK=.FALSE.
               GOTO 31
            ELSE
               GOTO 5
            END IF
         END IF
         MACLAB=MACLAB+1
         WRITE(UNIT=LABUNT,REC=MACLAB,ERR=30)MACSIZ,MACBUF(1:20)
      END IF
      GOTO 10
 20   CONTINUE
C     must have hit end of source file
      CLOSE(UNIT=TUNIT,STATUS='KEEP')
CIBM
C      LFU(TUNIT)=.FALSE.
CIBM
      OK=.TRUE.
      RETURN
C
 30   CONTINUE
C     some kind of error on file access
      CALL DEPRNT(12)
 31   CONTINUE
      CLOSE(UNIT=TUNIT,STATUS='KEEP')
CIBM
C         LFU(TUNIT)=.FALSE.
CIBM
      CLOSE(UNIT=MACUNT)
CIBM
C         LFU(MACUNT)=.FALSE.
CIBM
      CLOSE(UNIT=LABUNT)
CIBM
C         LFU(LABUNT)=.FALSE.
CIBM
      OK=.FALSE.
C
      END
C
C
      SUBROUTINE PAN(AVAR)
C     ======================
C     this subroutine is used to zoom out
C     by a known scale factor
C
      include   'include/wtov.inc'
      include   'include/menun.inc'
      REAL AVAR(10)
C
      AVAR(4)=WXMIN+AVAR(1)
      AVAR(5)=WYMIN+AVAR(2)
      AVAR(6)=WXMAX+AVAR(1)
      AVAR(7)=WYMAX+AVAR(2)
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate new viewport limits
      CALL WORLD(AVAR(4),AVAR(5),AVAR(6),AVAR(7))
C
      CALL REGEND()
      END
C
 
C
      SUBROUTINE ZOOMI(AVAR)
C     ======================
C     this subroutine is used to zoom out
C     by a known scale factor
C
      include   'include/wtov.inc'
      include   'include/menun.inc'
      REAL AVAR(10)
C
C
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate new viewport limits
      CALL WORLD(AVAR(1),AVAR(2),AVAR(3),AVAR(4))
C
      CALL ATGEND()
      END
C
      SUBROUTINE ZOOMO(AVAR)
C     ======================
C     this subroutine is used to zoom out
C     by a known scale factor
C
      include   'include/wtov.inc'
      include   'include/menun.inc'
      include  'include/macro.inc'
      REAL AVAR(10),A(3,3),XMIN,YMIN,XMAX,YMAX,D1
C
      D1=AVAR(3)
      CALL SCAP2D(AVAR(1),AVAR(2),AVAR(3),D1,A)
      CALL NEWXY(WXMIN,WYMIN,XMIN,YMIN,A)
      CALL NEWXY(WXMAX,WYMAX,XMAX,YMAX,A)
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate new viewport limits
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
      CALL REGEND()
      END
C

C
C	  Added by the DPS on March 4th 2002 - Allows a VB Style Alert box
C
      SUBROUTINE MAC011(OK)
C     ============================
C1    vartype            L 
C1    iostatus           O
C
C
      include  'include/macro.inc'
      include  'include/calc.inc'
      include 'include/vntable.inc'
C
      LOGICAL OK
	  CHARACTER*80 STRING
      
C     get the string to print from the main buffer
      STRING=MACBUF(MACCP+1:)
C     crunch it to remove ambiguous spaces
C      CALL CHARCH(STRING)

      CALL INFODIALOG(STRING,1)

      OK=.TRUE.
      RETURN
99    OK=.FALSE.
C
C     WRITE(10,*)'[MAC010] exitted'
      END
