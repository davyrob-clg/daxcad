C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 error85.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CLRPEW()
C     SUBROUTINE CPRMXP(STRING,INPL)
C     SUBROUTINE DCPRN2(DNUM,STRING)
C     SUBROUTINE DCPRNT(DNUM)
C     SUBROUTINE DEPRN3(STRING,DNUM)
C     SUBROUTINE DEPRNT(DNUM)
C     SUBROUTINE DPRMX2(DNUM,BUFF,INPL)
C     SUBROUTINE DPRMX3(DNUM,STRING,INPL)
C     SUBROUTINE DPRMXP(DNUM,INPL)
C     SUBROUTINE DTPMSG(DNUM,WIN)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE CLRPEW()
C     ===================
C1    no arguments required
C
C2    subroutine CLRPEW clears to black
C2    the prompt window on the screen.
C2    Ensures that the scrolling buffer
C2    is cleared to avoid old messages
C2    appearing in the window.
C
      INTEGER*4 PWIN
C
      EXTERNAL GTZAPW
C
      DATA PWIN/1/
C     clear the prompt window
      CALL GTZAPW(PWIN)
C
      END
C
C
      SUBROUTINE CPRMXP(STRING,INPL)
C     ==============================
C1    vartype             C     C
C1    iostatus            I     O
C2
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C2    and waits on the same line for typed
C2    input of a text string which is
C2    returned in INPL.
C
      include 'include/macro.inc'
      include 'include/comstack.inc'
      include 'include/journal.inc'
C
      CHARACTER*(*) STRING,INPL,BLANK*80
      CHARACTER*256 TEXT
      REAL X,Y
      LOGICAL  OK
      INTEGER*4 WIN,I
      INTEGER*4 NLEN
      INTEGER*4 NLEN1
      EXTERNAL GTPMSG,GTRLIN,WRTJRN
      EXTERNAL NLEN
      EXTERNAL NLEN1
C
C     if the prompt varable set then write it out
 
      WIN = 1
c      IF(PROMPT.OR.PRNTS) THEN
c          CALL GTPMSG(STRING,WIN)
c      ELSE
c          BLANK = ' '
c          CALL GTPMSG(BLANK, WIN)
c      ENDIF
      PRNTS=.FALSE.
      IF (MACOP.AND.MACTOK.AND..NOT.TCONT) THEN
             TYPED = .TRUE.
             CALL MAC101(X,Y,OK)
             TYPED = .FALSE.
             IF (TIR) THEN
                INPL=TIS
             ELSE
                IF(OK) THEN
                    CALL GETINPUT(STRING,INPL)
C                    CALL GTRLIN(INPL,WIN)
                ELSE
                    INPL=' '
                ENDIF
             END IF
      ELSE
             CALL GETINPUT(STRING,INPL)
C             CALL GTRLIN(INPL,WIN)
      END IF
      IF(CSTKP.LT.20) THEN
           CSTKP = CSTKP +1
      ELSE
C        reset for wrap round
          DO 200 I=2,20
              CMDS(I-1) = CMDS(I)
              HITPTS(1,I-1) = HITPTS(1,I)
              HITPTS(2,I-1) = HITPTS(2,I)
200       CONTINUE
C

      ENDIF

      TEXT = STRING(1:NLEN1(STRING))//' :: '//INPL(1:NLEN1(INPL))

      CALL PROMPTOUT(TEXT,NLEN(TEXT))
C     Update comm file
      CMDS(CSTKP) = '~'//INPL
C     if JOURNAL is on then write out an entry to the journal
C     file 
      IF(JOURON .AND. .NOT. PNTMOD) CALL WRTJRN(X, Y, 'a', INPL, 0)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DCPRN2(DNUM,STRING)
C     ==============================
C1    vartype             I4  C*(*)
C1    iostatus            I     I
C2
C2    prints the message in dictionary
C2    position DNUM concatenated with
C2    STRING to the prompt window,in normal video mode
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM
      CHARACTER*(*) STRING
      EXTERNAL CPRINT,DTPMSG
C
C     print the dictionary string
      CALL DTPMSG(DNUM,1)
C     print the string
      CALL CPRINT(STRING)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DCPRNT(DNUM)
C     =========================
C1    vartype             C
C1    iostatus            I
C2
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM
      EXTERNAL CPRINT
C
      CALL CPRINT(DICT01(DNUM))
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DEPRN3(STRING,DNUM)
C     ==============================
C1    vartype            C*(*)  I4
C1    iostatus            I     I
C2
C2    prints the message in STRING
C2    concatenated with text from dictionary position
C2    DNUM to the prompt window,in normal video mode
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM,NLEN1
      CHARACTER*(*) STRING,TEMP*100
      EXTERNAL EPRINT,NLEN1
C
C     print the dictionary string.
      WRITE(TEMP,'(3A)') STRING(1:NLEN1(STRING)),' ',
     +     DICT01(DNUM)(1:NLEN1(DICT01(DNUM)))
      CALL EPRINT(TEMP)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DEPRNT(DNUM)
C     =========================
C1    vartype             I4
C1    iostatus            I
C2
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM
      EXTERNAL CPRINT
C
      CALL EPRINT(DICT01(DNUM))
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DPRMX2(DNUM,BUFF,INPL)
C     =================================
C1    vartype             C    C    C
C1    iostatus            I    O    O
C2
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C2    and waits on the same line for typed
C2    input of a text string which is
C2    returned in INPL.
      include 'include/vntable.inc'
      include 'include/typed.inc'
      include 'include/interface.inc'
C
      CHARACTER*(*) INPL,BUFF
      INTEGER*4 DNUM
      EXTERNAL CPRMXP
C
      TMPBUF=BUFF
      LASTBUF=BUFF
C
      CALL CPRMXP(DICT01(DNUM),INPL)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DPRMX3(DNUM,STRING,INPL)
C     ===================================
C1    vartype             I4  C*(*) C*(*)
C1    iostatus            I     I     O
C2
C     prints the message from dictionary
C2    position DNUM,followed by prompt
C2    message in STRING to the
C2    prompt window,in normal video mode
C2    and waits on the same line for typed
C2    input of a text string which is
C2    returned in INPL.
C
      include 'include/vntable.inc'
 
      CHARACTER*(*) INPL,STRING
      INTEGER*4 DNUM
      EXTERNAL CPRMXP,DTPMSG
C
C     print the dictionary string
      CALL DTPMSG(DNUM,1)
C     prompt for input
      CALL CPRMXP(STRING,INPL)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DPRMXP(DNUM,INPL)
C     ==============================
C1    vartype             I4    C
C1    iostatus            I     O
C2
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C2    and waits on the same line for typed
C2    input of a text string which is
C2    returned in INPL.
      include 'include/vntable.inc'
C
      CHARACTER*(*) INPL
      INTEGER*4 DNUM
      EXTERNAL CPRMXP
C
      CALL CPRMXP(DICT01(DNUM),INPL)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE DTPMSG(DNUM,WIN)
C     ===========================
C
C2    prints the message in STRING to the
C2    prompt window,in normal video mode
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM,WIN
      INTEGER*4 NLEN
      INTEGER*4 NLEN1
      EXTERNAL GTPMSG
C
      CALL GTPMSG(DICT01(DNUM),WIN)
      CALL PROMPTOUT(DICT01(DNUM),NLEN1(DICT01(DNUM)))
C
      END
