C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 gtxt84.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE BORDER
C     SUBROUTINE CLEAR()
C     SUBROUTINE GTCLVN(GTWID4, GTCID4)
C     SUBROUTINE GTDCLV(VNNUM,GTWID4)
C     SUBROUTINE GTDHFM(GTWID4,VNUM,GTCID4)
C     SUBROUTINE GTDMCH(VNUM, GTWID4)
C     SUBROUTINE GTDMCL(VNUM, GTWID4)
C     SUBROUTINE GTDMEN(VNNUM,GTWID4)
C     SUBROUTINE GTDMHD(VNNUM,GTWID4)
C     SUBROUTINE GTDMWI(VNNUM,MENU,IDAT)
C     SUBROUTINE GTDMWR(VNNUM,MENU,R,FORM)
C     SUBROUTINE GTDMWT(VNNUM,MENU,STRNG)
C     SUBROUTINE GTDOMN(VNNUM,GTWID4)
C     SUBROUTINE GTHFMC(GTWID4,CMDC,GTCID4)
C     SUBROUTINE GTHIMC(GTWID4,CMDC,CMDS,GTCID4)
C     SUBROUTINE GTILIN(GTSTRG,GTWID4)
C     SUBROUTINE GTMCHI(GTWID4,GTCID4)
C     SUBROUTINE GTMCLO(GTWID4,GTCID4)
C     SUBROUTINE GTMCWI(MENU,TOKEN,I4)
C     SUBROUTINE GTMCWR(MENU,TOKEN,R,FORM)
C     SUBROUTINE GTMCWT(MENU,TOKEN,STRNG)
C     SUBROUTINE GTMNVN(VNNUM,GTWID4, GTCID4)
C     SUBROUTINE GTPLIN(GTSTRG,GTWID4)
C     SUBROUTINE GTPMSG(GTSTRG,GTWID4)
C     SUBROUTINE GTRLIN(GTSTRG,GTWID4)
C     SUBROUTINE GTRSTW(GTWID4)
C     SUBROUTINE GTSAVW(GTWID4)
C     SUBROUTINE GTVNPO(VNUM, GTCID4)
C     SUBROUTINE GTVNTK(VNUM, TOKEN)
C     SUBROUTINE GTZAPW(GTWID4)
C     SUBROUTINE MENHIT(CURSX,CURSY,LMENUN,LCELLN,LCTEXT,LCCMD,OK)
C     SUBROUTINE RSTMEN(GTWID4)
C     SUBROUTINE SAVEMN(GTWID4)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE BORDER
C     =================
C2
C2      Subroutine BORDER draws a border round the current
C2      screen window
C2      Note. no drawing will appear outside this border
C2      due to the drawing being clipped
C1
C
      include 'include/daxcolor.inc'
      include 'include/wtov.inc'
      include 'include/interface.inc'
C
      INTEGER*2 IXMIN,IXMAX,IYMIN,IYMAX
      EXTERNAL DRSREC
C
C     convert to two-byte integers
      IXMIN=VXMIN
      IXMAX=VXMAX
      IYMIN=VYMIN
      IYMAX=VYMAX
 
      CALL TOOLPEN(VIEWF)
      CALL DRSREC(IXMIN,IYMIN,IXMAX,IYMAX)
 
      IXMIN=IXMIN-1
      IYMIN=IYMIN-1
      IXMAX=IXMAX+1
      IYMAX=IYMAX+1
      CALL DRSREC(IXMIN,IYMIN,IXMAX,IYMAX)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE CLEAR()
C     ==================
C1    no arguments needed
C2      Subroutine CLEAR erase the drawing area
C
      include   'include/wtov.inc'
      include   'include/interface.inc'
C
      EXTERNAL BORDER,CLEARW
C
      CALL TOOLPEN(VIEWB)
      CALL CLEARW(VXMIN-1,VYMIN-1,VXMAX+1,VYMAX+1)
      CALL BORDER()
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTCLVN(GTWID4, GTCID4)
C     ==============================+
C1    vartype            I4     I4
C1    iostatus           I      I
C2
C2    Subroutine GTCLVN clears the verb/noun number GMCVNM
C2    in the cell GTCID4 in  the menu number GTWID4.
C
      include 'include/gtxt2.inc'
C
C
      INTEGER*4 GTWID4,GTCID4
C
      GMCVNM(GTWID4, GTCID4-1) = 0
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDCLV(VNNUM,GTWID4)
C     ==============================+
C1    vartype            I4     I4
C1    iostatus           I      I
C2
C2    Subroutine GTDCLV clears the verb/noun number GMCVNM
C2    in the cell VNNUM in  the menu number GTWID4,using
C2    the position read from the command dictionary system.
C2    do you really need to use this function
C
      include 'include/vntable.inc'
C
C
      INTEGER*4 GTWID4,GTCID4,VNNUM
      EXTERNAL  GTCLVN
C
C     read the position for the command in menu
      GTCID4=VNPOS(VNNUM)
C     test for null cell
      IF (GTCID4.GT.0) THEN
C        clear the verb number
         CALL GTCLVN(GTWID4, GTCID4)
      END IF
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDHFM(GTWID4,VNUM,GTCID4)
C     ==========================================
C
C1    vartype             I4    I4     I4
C1    iostatus            I     I       O
C
C2    Finds the cell in menu number GTWID4
C2    with verb noun number vnum 
C2    The cell number is returned in GTCID4.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 VNUM,GTWID4,GTCID4
      CHARACTER CMDC*1
      EXTERNAL GTVNTK, GTHFMC
C     get the token for the vnum
      CALL GTVNTK(VNUM,CMDC)    
C     right so get the menu cell
      CALL GTHFMC(GTWID4,CMDC,GTCID4)
C
      END
C
C-----------------------------------------------------------------
      SUBROUTINE GTDMCH(VNUM, GTWID4)
C     ================================
C1    vartype             I4     I4
C1    iostatus            I      I
C2
C2    ensures that the cell associated with verb/noun No
C2    VNUM in menu No GTWID4 is displayed in reverse video mode.
C2    This is a verb noun driven version of GTMCHI
C
      include  'include/gtxt2.inc'
      include  'include/macro.inc'
C
      INTEGER*4 VNUM, GTWID4, GTCID4
C
      EXTERNAL GTMCHI,GTVNPO
C     get the cell position for that verb/noun No
      CALL GTVNPO(VNUM, GTCID4)
C     hi-light the cell
      CALL GTMCHI(GTWID4, GTCID4)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMCL(VNUM, GTWID4)
C     ================================
C1    vartype             I4     I4
C1    iostatus            I      I
C2
C2    ensures that the cell associated with verb/noun No
C2    VNUM in menu No GTWID4 is displayed in normal video
C2    This is a verb noun driven version of GTMCLO
C
      include  'include/gtxt2.inc'
      include  'include/macro.inc'
C
      INTEGER*4 VNUM, GTWID4, GTCID4
C
      EXTERNAL GTMCLO,GTVNPO
C     get the cell position for that verb/noun No
      CALL GTVNPO(VNUM, GTCID4)
C     hi-light the cell
      CALL GTMCLO(GTWID4, GTCID4)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMEN(VNNUM,GTWID4)
C     ==============================+
C1    vartype            I4     I4
C1    iostatus           I      I
C2
C2    Subroutine GTDMEN writes the verb/noun number VNNUM
C2    into the menu number GTWID4,using the word,token,and
C2    position read from the command dictionary system.
C2    the VNNUM for that cell is also stored.
C
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
C
      INTEGER*4 GTWID4,GTCID4,VNNUM
      CHARACTER WORD*16,TOKEN*1
      EXTERNAL GTPMEN
C
C     read the token for this command
      TOKEN=VNTOKE(VNNUM)
C     read the word for the command
      WORD=VNOUN(VNNUM)
C     read the position for the command in menu
      GTCID4=VNPOS(VNNUM)
C     test for null cell
      WRITE(10,*)'[GTDMEN] Token Cell Word ',TOKEN,GTCID4,WORD
      IF (GTCID4.GT.0) THEN
C        write to menu cell
         CALL GTPMEN(WORD,TOKEN,GTWID4,GTCID4)
C        store the verb number
         GMCVNM(GTWID4, GTCID4-1) = VNNUM
      END IF
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMHD(VNNUM,GTWID4)
C     ==============================+
C1    vartype            I4     I4
C1    iostatus           I      I
C2
C2    Subroutine GTDMHD writes the verb/noun number VNNUM
C2    into the menu number GTWID4,using the word
C2    read from the command dictionary system.
C2    The word is written into position 1 of the menu
C2    with a null token. The VNNUM for that cell is also stored.
C
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*4 GTWID4,GTCID4,VNNUM
      CHARACTER WORD*16,TOKEN*1
      EXTERNAL GTPMEN,GTMCHI
C
C     force token to null
      TOKEN=' '
C     read the word for the command
      WORD=VNOUN(VNNUM)
C     force the position to be 1
      GTCID4=1
C     write to menu cell
      CALL GTPMEN(WORD,TOKEN,GTWID4,GTCID4)
C
C     store the verb number
      GMCVNM(GTWID4, GTCID4-1) = VNNUM
C
C     hilite the header cell
      CALL GTMCHI(GTWID4,GTCID4)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMWI(VNNUM,MENU,IDAT)
C     ====================================
C
C1    vartype           I4     I4   I4
C1    iostatus          I      I    I
C
C2    Subroutine GTDMWI updates the cell position
C2    for the dictionary position VNNUM using the
C2    dictionary data,a colon expected for delimiter.
C2    The cell is reconstructed using the concatenation
C2    of old content and the integer number IDAT.
C2    The VNNUM for that cell is also stored.
C
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*4 MENU,TCELL,INDEX,I,VNNUM,IDAT
C
      CHARACTER*(*) TOKEN*1,CSTRNG*80,TEMP*80
C     INTRINSIC INDEX
C
      EXTERNAL GTCLRC,GTPMEN
C
C     find the token.
      TOKEN=VNTOKE(VNNUM)
C     find the cell number.
      TCELL=VNPOS(VNNUM)
C     now read cell text from dictionary.
      CSTRNG=VNOUN(VNNUM)
      I=INDEX(CSTRNG,':')
C     protect agains no colon.
      IF (I.EQ.0) I=12
C     construct new cell text.
      WRITE(UNIT=TEMP,FMT='(A,I3)') CSTRNG(:I),IDAT
C     now clear old contents.
      CALL GTCLRC(MENU,TCELL)
C     write new data into place.
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C     store the verb number
      GMCVNM(MENU, TCELL-1) = VNNUM
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMWR(VNNUM,MENU,R,FORM)
C     ====================================
C
C1    vartype           I4    I4   R C*(*)
C1    iostatus          I      I   I   I
C
C2    Subroutine GTDMWR updates the cell position
C2    for the dictionary position VNNUM using the
C2    dictionary data,a colon expected for delimiter.
C2    The cell is reconstructed using the concatenation
C2     of old content and the REAL number R with format FORM.

C
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*4 MENU,TCELL,INDEX,I,VNNUM
      REAL R
C
      CHARACTER*(*) TOKEN*1,FORM,CSTRNG*80,TEMP*80,TEMP1*20
C     INTRINSIC INDEX
C
      EXTERNAL GTCLRC,GTPMEN
C
C     find the token
      TOKEN=VNTOKE(VNNUM)
C     find the cell number
      TCELL=VNPOS(VNNUM)
C     now read cell text from dictionary
      CSTRNG=VNOUN(VNNUM)
      I=INDEX(CSTRNG,':')
C     protect agains no colon
      IF (I.EQ.0) I=12
C     construct new cell text
      WRITE(UNIT=TEMP1,FMT=FORM) R
      CALL CRUNCH(TEMP1)
      WRITE(UNIT=TEMP,FMT='(A,A)') CSTRNG(:I),TEMP1
C     now clear old contents
      CALL GTCLRC(MENU,TCELL)
C     write new data into place
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C     store the verb number
      GMCVNM(MENU, TCELL-1) = VNNUM
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDMWT(VNNUM,MENU,STRNG)
C     ===================================
C
C1    vartype           I4      I4  C*(*)
C1    iostatus          I       I     I
C
C2    Subroutine GTDMWT concatenates STRNG with
C2    the exisiting dictionary word for cell
C2    occupied by entry VNNUM in the verb-noun table
C2    A colon is expected as delimiter of the cell contents,
C2    and termination is made at 12 chars if no colon found.
C2    The cell is then cleared,and the cell text
C2    reconstructed using the concatenation of old
C2    content and the string STRNG.
C2    the VNNUM for that cell is also stored.
C
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 MENU,TCELL,INDEX,I,VNNUM
C
      CHARACTER TOKEN*1,STRNG*(*),CSTRNG*80,TEMP*80
C     INTRINSIC INDEX
      EXTERNAL GTCLRC,GTPMEN
C
C     find the token
      TOKEN=VNTOKE(VNNUM)
C     find the cell number
      TCELL=VNPOS(VNNUM)
C     now read cell text from dictionary
      CSTRNG=VNOUN(VNNUM)
      I=INDEX(CSTRNG,':')
C     protect agains no colon
      IF (I.EQ.0) I=12
C     construct new cell text
      WRITE(UNIT=TEMP,FMT='(A,A)') CSTRNG(:I),STRNG
C     now clear old contents
      CALL GTCLRC(MENU,TCELL)
C     write new data into place
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C     store the verb number
      GMCVNM(MENU, TCELL-1) = VNNUM
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTDOMN(VNNUM,GTWID4)
C     ==============================+
C1    vartype            I4     I4
C1    iostatus           I      I
C2
C2    Subroutine GTDOMN writes the verb/noun number VNNUM
C2    into the menu number GTWID4,using the word,token,and
C2    position read from the command dictionary system.
C2    The cell is cleared befor being written,to ensure no
C2    conflicts.
C
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*4 GTWID4,GTCID4,VNNUM
      CHARACTER WORD*16,TOKEN*1
      EXTERNAL GTPMEN,GTCLRC
C
C     read the token for this command
      TOKEN=VNTOKE(VNNUM)
C     read the word for the command
      WORD=VNOUN(VNNUM)
C     read the position for the command in menu
      GTCID4=VNPOS(VNNUM)
C     test for null cell
      IF (GTCID4.GT.0) THEN
C        clear to background first
         CALL GTCLRC(GTWID4,GTCID4)
C        write to menu cell
         CALL GTPMEN(WORD,TOKEN,GTWID4,GTCID4)
C        store the verb number
         GMCVNM(GTWID4, GTCID4-1) = VNNUM
      END IF
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTHFMC(GTWID4,CMDC,GTCID4)
C     ==========================================
C
C1    vartype             I4    C1     I4
C1    iostatus            I     I       O
C
C2    Finds the cell in menu number GTWID4
C2    containing command charcater CMDC
C2    The cell number is returned in GTCID4.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 PL,I,J,GTWID4,GTCID4
      CHARACTER CMDC*1
C
      PL=GML(GTWID4,6)
      DO 30 J=0,PL-1
         IF (GMBC(GTWID4,J).EQ.
     +      CMDC) THEN
C           remember offset from buffer number to cell
C           number of 1.
            GTCID4=J+1
            GOTO 99
         END IF
 30   CONTINUE
C     if got here,then cell not found
      GTCID4=0
 99   CONTINUE
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTHIMC(GTWID4,CMDC,CMDS,GTCID4)
C     ==========================================
C1    vartype             I4    C1   C*    I4
C1    iostatus            I     I    I     O
C2
C2    ensures that the cell in menu number GTWID4
C2    containing command charcater CMDC is displayed
C2    in reverse video mode.The cell number hilited
C2    is returned in GTCID4.All characters in string
C2    CMDS other than CMDC are used to ensure that
C2    the equivalent cells are displayed in normal
C2    video mode.
C2    ie only the cell related to CMDC from the cells
C2    containing CMDS is left in hilited mode.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 NLEN1,PL,I,J,GTWID4,GTCID4
      CHARACTER CMDC,CMDS*(*)
      EXTERNAL GTMCHI,GTMCLO,NLEN1
C
      PL=GML(GTWID4,6)
C
      DO 20 I=1,NLEN1(CMDS)
         DO 30 J=0,PL-1
            IF (GMBC(GTWID4,J).EQ.
     +          CMDS(I:I)) THEN
               IF (CMDS(I:I).EQ.CMDC) THEN
C                 remember offset from buffer number to cell
C                 number of 1.
                  CALL GTMCHI(GTWID4,J+1)
                  GTCID4=J+1
               ELSE
                  CALL GTMCLO(GTWID4,J+1)
               END IF
            END IF
 30   CONTINUE
 20   CONTINUE
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTILIN(GTSTRG,GTWID4)
C     ===============================
C1    vartype             C       I4
C1    iostatus            I       I
C2
C2    Formerly GTXT_PRINT_INV_LINE prints the string 'GTSTRG'
C2    on the current line of window number GTWID4 in inverse
C2    video.Performs a CR,LF sequence.
C
      include 'include/gtxt1.inc'
C
      INTEGER*4 GTWID4,NLEN1
      INTEGER*2 L,CP,LP,TL,GTWID
      CHARACTER GTSTRG*(*)
C
      EXTERNAL GTSCRW,NLEN1
C
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C     Find length of passed string
      L=NLEN1(GTSTRG)
C     get current cursor position on input line
      CP=GWC(GTWID,5)
C     get last useable cursor position
      LP=GWL(GTWID,5)
C     Calculate total length of the line
      TL=CP+L-1
      IF (TL.GT.LP) THEN
C        Set actual length of string for output
         L=L-(TL-LP)
      END IF
C
C     Place the line in the input line of the window
      GWLINE(GTWID)(CP:)=GTSTRG(1:L)
C     scroll the input line into the output area
      CALL GTSCIW(GTWID4)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMCHI(GTWID4,GTCID4)
C     ================================
C1    vartype             I4     I4
C1    iostatus            I      I
C2
C2    ensures that the cell no GTCID4 in menu
C2    no GTWID4 is displayed in reverse video mode.
C
      include  'include/gtxt2.inc'
      include  'include/macro.inc'
C
      INTEGER*4 GTWID4,GTCID4
C
      EXTERNAL GTIVMC
C
C     test for out of range values
      IF (GTWID4.LT.1.OR.GTWID4.GT.4) RETURN
      IF (.NOT.GMCINV(GTWID4,GTCID4-1)) THEN
         IF ( MENUS ) THEN
            CALL GTIVMC(GTWID4,GTCID4)
         END IF
      ELSE
C         WRITE(10,*) '[gtmchi]',GTWID4,GTCID4
      END IF
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMCLO(GTWID4,GTCID4)
C     ================================
C1    vartype             I4     I4
C1    iostatus            I      I
C2
C2    ensures that the cell no GTCID4 in menu
C2    no GTWID4 is displayed in normal video mode.
C
      include  'include/gtxt2.inc'
      include  'include/macro.inc'
C
      INTEGER*4 GTWID4,GTCID4
C
      EXTERNAL GTIVMC
C
C     test for out of range values
      IF (GTWID4.LT.1.OR.GTWID4.GT.4) RETURN
C
      IF (GMCINV(GTWID4,GTCID4-1)) THEN
         IF ( MENUS ) THEN
            CALL GTIVMC(GTWID4,GTCID4)
         END IF
      ELSE
C         WRITE(10,*) '[gtmclo]',GTWID4,GTCID4
      END IF
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMCWI(MENU,TOKEN,I4)
C     ================================
C
C1    vartype           I4    C*1  I4
C1    iostatus          I      I   I
C
C2    Subroutine GTMCWI searches menu number MENU
C2    for the token TOKEN,extracts the text of the
C2    cell up to and including a colon.
C2    the cell is then cleared,and the cell text
C2    reconstructed using the concatenation of old
C2    content and the integer I4.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 MENU,I4,TCELL,INDEX,I
C
      CHARACTER*(*) TOKEN,CSTRNG*80,TEMP*80
C     INTRINSIC INDEX
      EXTERNAL GTHFMC,GTCLRC,GTPMEN
C
C     find the correct cell first
      CALL GTHFMC(MENU,TOKEN,TCELL)
      IF ( TCELL.EQ.0 ) RETURN
C     now read cell text from buffer
      CSTRNG=GMB(MENU,TCELL-1)
      I=INDEX(CSTRNG,':')
C     protect agains no colon
      IF (I.EQ.0) I=12
C     construct new cell text
      WRITE(UNIT=TEMP,FMT='(A,I3)') CSTRNG(:I),I4
C     now clear old contents
      CALL GTCLRC(MENU,TCELL)
C     write new data into place
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMCWR(MENU,TOKEN,R,FORM)
C     ====================================
C
C1    vartype           I4    C*1  I4 C*(*)
C1    iostatus          I      I   I    I
C
C2    Subroutine GTMCWR searches menu number MENU
C2    for the token TOKEN,extracts the text of the
C2    cell up to and including a colon.
C2    the cell is then cleared,and the cell text
C2    reconstructed using the concatenation of old
C2    content and the REAL number R with format FORM.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 MENU,TCELL,INDEX,I
      REAL R
      CHARACTER*(*) TOKEN,FORM,CSTRNG*80,TEMP*80,TEMP1*20
      EXTERNAL GTHFMC,GTCLRC,GTPMEN
C
C     find the correct cell first
      CALL GTHFMC(MENU,TOKEN,TCELL)
      IF ( TCELL.EQ.0 ) RETURN
C     now read cell text from buffer
      CSTRNG=GMB(MENU,TCELL-1)
      I=INDEX(CSTRNG,':')
C     protect agains no colon
      IF (I.EQ.0) I=12
C     construct new cell text
      WRITE(UNIT=TEMP1,FMT=FORM) R
      WRITE(UNIT=TEMP,FMT='(A,A)') CSTRNG(:I),TEMP1
C     now clear old contents
      CALL GTCLRC(MENU,TCELL)
C     write new data into place
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMCWT(MENU,TOKEN,STRNG)
C     ===================================
C
C1    vartype           I4    C*1   C*(*)
C1    iostatus          I      I      I
C
C2    Subroutine GTMCWT searches menu number MENU
C2    for the token TOKEN,extracts the text of the
C2    cell up to and including a colon.
C2    the cell is then cleared,and the cell text
C2    reconstructed using the concatenation of old
C2    content and the string STRNG.
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 MENU,TCELL,INDEX,I,NLEN1
      CHARACTER TOKEN*1,STRNG*(*),CSTRNG*80,TEMP*80
C     INTRINSIC INDEX
      EXTERNAL GTHFMC,GTCLRC,GTPMEN,NLEN1
C
C     find the correct cell first
      CALL GTHFMC(MENU,TOKEN,TCELL)
      IF ( TCELL.EQ.0 ) RETURN
C     now read cell text from buffer
      CSTRNG=GMB(MENU,TCELL-1)
      I=INDEX(CSTRNG,':')
C     protect agains no colon
      IF (I.EQ.0) I=12
C     construct new cell text
      WRITE(UNIT=TEMP,FMT='(A,A)') CSTRNG(:I),STRNG(1:(NLEN1(STRNG)))
C     now clear old contents
      CALL GTCLRC(MENU,TCELL)
C     write new data into place
      CALL GTPMEN(TEMP,TOKEN,MENU,TCELL)
C
C     all done
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTMNVN(VNNUM,GTWID4, GTCID4)
C     ==============================+
C1    vartype            I4      I4     I4
C1    iostatus           O        I      I
C2
C2    This function gets the vnoun dict No
C2    held for the menu cell specified by
C2    by GTWID4, GTCID4
C
      include 'include/gtxt2.inc'
C
C
      INTEGER*4 GTWID4,GTCID4,VNNUM
C
      VNNUM = GMCVNM(GTWID4, GTCID4-1)
C
C
      END
C
C-----------------------------------------------------------------
C
 
      SUBROUTINE GTPLIN(GTSTRG,GTWID4)
C     ===============================
C1    vartype             C      I4
C1    iostatus            I      I
C2
C2    Formerly GTXT_PRINT_LINE prints the character string
C2    GTSTRG on the current line of window number GTWID4.
C2    Performs a carriage return,linefeed sequence after
C2    transmission of the string.
C
      include 'include/gtxt1.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*4 GTWID4,NLEN1
      INTEGER*4 CURFONT
      INTEGER*4 ST
      INTEGER*4 X,Y
      INTEGER*4 NLEN
      INTEGER*4 COL
      INTEGER*2 L,CP,LP,TL,GTWID
      CHARACTER GTSTRG*(*)
      CHARACTER TEMP*256
C
      EXTERNAL GTSCRW,NLEN1,NLEN
C

      COL = COLFOR
      CALL TEXTINWINDOW(1,5,COL,GTSTRG,NLEN1(GTSTRG))
      GWLINE(GTWID4)=GTSTRG
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTPMSG(TEXT,WINDOW)
C     ==============================
C1    vartype             C      I4
C1    iostatus            I       I
C2
C2    Formerly GTXT_PRINT_MSG prints a message on the current line
C2    of window number GTWID4 and leaves the text cursor at the end
C2    of the message string. Multiple calls may be made to GTPMSG
C2    on the one output line of the window.
C
      include 'include/gtxt1.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*4 WINDOW
      INTEGER*4 NLEN1
      INTEGER*4 COL
      CHARACTER TEXT*(*)
C
      EXTERNAL NLEN1
C
      COL = COLFOR
      CALL TEXTINWINDOW(WINDOW,5,COL,TEXT,NLEN1(TEXT))

      GWLINE(WINDOW)=TEXT
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTRLIN(GTSTRG,GTWID4)
C     ===============================
C1    vartype              C      I4
C1    iostatus             O      I
C2
C2    Formerly GTXT_READ_LINE reads keyboard input and
C2    echoes each character on the current line of window
C2    number GTWID4. The input line is terminated by CR,or
C2    ctrl 'M', at which time the text window is scrolled.
C2    The typed string is returned in 'GTSTRG'.
C
      include 'include/gtxt1.inc'
      include 'include/vntable.inc'
C
      INTEGER*2 GTWID,L
      INTEGER*2 CP,LP,TL
      INTEGER*4 ST,GTWID4,NLEN1
      CHARACTER GTSTRG*(*),CHAR*1
      CHARACTER PROMPT*80
C
      INTRINSIC CHAR,LEN,REAL
      EXTERNAL GTSCRW,GTSRLN,NLEN1,STCURP
C
C     Go find a line of text input,and echo to screen
      PROMPT = DICT01(556)
      CALL GETINPUT(PROMPT,GTSTRG)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTRSTW(WINDOW)
C     =========================
C
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine GTRSTW copies the  state of a
C2    screen  window form a safe buffer,to allow
C2    recovery of the previous state.
C
      include 'include/gtxt1.inc'
      include 'include/gtxt3.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*4 WINDOW
      INTEGER*4 COL
C
      EXTERNAL GTSCRW
C
      COL = COLFOR
      GWLINE(WINDOW) = GWBISF(WINDOW)
      CALL TEXTINWINDOW(
     + WINDOW,5,COL,GWLINE(WINDOW),NLEN1(GWLINE(WINDOW)))
C     now scroll the data into the window
      END
C
C-----------------------------------------------------------------
C
 
      SUBROUTINE GTSAVW(WINDOW)
C     =========================
C
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine GTSAVW copies the current state of a
C2    screen  window into a safe buffer,to allow use
C2    of that window for other activity,but enableing
C2    recovery of the previous state.
C
      include 'include/gtxt1.inc'
      include 'include/gtxt3.inc'
C
      INTEGER*4 WINDOW
C
      GWBISF(WINDOW)=GWLINE(WINDOW)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTVNPO(VNUM, GTCID4)
C     ===============================
C
C1    vartype            I4     I4
C1    iostatus           I      O
C
C2    sticks the  cell No for the VNUM  into
C2    GTCID4 so that the cell No for a particular
C2    dictionary number can be found.
C
      include 'include/vntable.inc'
C
      INTEGER*4  VNUM,GTCID4
      GTCID4 = VNPOS(VNUM)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTVNTK(VNUM, TOKEN)
C     ===================
C
C1    vartype            I4     C*1
C1    iostatus           I      O
C
C2    sticks the token for the VNUM  into
C2    the TOKEN so that the token for a particular
C2    dictionary number can be found.
C
      include 'include/vntable.inc'
C
      INTEGER*4  VNUM
      CHARACTER*1 TOKEN
      TOKEN = VNTOKE(VNUM)
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE GTZAPW(GTWID4)
C     ========================
C1    vartype             I4
C1    iostatus            I
C2
C2    This routine clears the text window number GTWID4 to
C2    background value,normally black.Also clears the text
C2    buffer associated with the window.
C
C
      include 'include/gtxt1.inc'
C
      INTEGER*2 GW,I,J
      INTEGER*4 GTWID4
C
      EXTERNAL GTCLRW
C
      GW=GTWID4
C     clear the screen area to black
      CALL GTCLRW(GTWID4)
C     clear the text buffer
      IF (GWA(GW,1)) THEN
C        clear the text buffer area
         J=GWL(GW,6)
         DO 50,I=0,J
            GWB(GW,I)=GNLINE
 50      CONTINUE
C
      END IF
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE MENHIT(CURSX,CURSY,LMENUN,LCELLN,LCTEXT,LCCMD,OK)
C     ========================================================
C
C2     This subroutine returns a logical value OK if the
C2     cursor hit CURSX,CURSY is within a valid menu area
C2     and returns the menu number and cell number hit in
C2     LMENUN and LCELLN. It also sticks the vnoun number 
C2     stored for that cell in VNCCMD
C2     
C
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
C
      INTEGER*4 CURSX,CURSY,LMENUN,LCELLN,MENKEY,N,NLEN1
      LOGICAL OK
      CHARACTER LCTEXT*80,LCCMD*1
      EXTERNAL MENKEY,NLEN1
C
      OK=.FALSE.
      N=0
C     cycle through the available menu list
 100  CONTINUE
      N=N+1
      IF (N.GT.4) GOTO 110
C      print*, 'MENU:',N,'X AND Y',CURSX,CURSY
	  LCELLN=MENKEY(N,CURSX,CURSY)
C      print*, 'CELL:',LCELLN

C     if valid menu hit found,return menu number and cell number
      OK=(LCELLN.GT.0)
      IF (OK) THEN
         LMENUN=N
         LCTEXT=GMB(N,LCELLN-1)
         LCCMD=GMBC(N,LCELLN-1)
         VNCCMD = GMCVNM(N,LCELLN-1)      
C         WRITE(*,'(5A)') 'Command "',LCTEXT(1:NLEN1(LCTEXT)),'"  ',LCCMD,' ',VNCCMD
      ELSE
         GOTO 100
      END IF
 110  CONTINUE
      END
C
C-----------------------------------------------------------------

      SUBROUTINE RSTMEN(GTWID4)
C     =========================
C
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine RSTMEN restores the menu
C2    that was saved by SAVEMN, GTWID4 
C2    controls whether to restore noun
C2    or modifier menu
C
      include 'include/gtxt2.inc'
C
      INTEGER*2 I,GTWID,MENLEN
      INTEGER*4 GTWID4,TCELL
      EXTERNAL GTPMEN, GTMCHI
C
C
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C     check for valid menu I.D.
      IF(GTWID .GT. 0 .AND. GTWID .LE. 4) THEN

C             
C       if it's the noun menu then the title cell with
C       the null token will have to be written out first
        IF(GTWID4 .EQ. 2) THEN
           I = 0
           TCELL = I + 1
           CALL GTPMEN(GMBSAV(GTWID,I),GMBCSV(GTWID,I),GTWID4,TCELL)
C          hilite the cell
           CALL GTMCHI(GTWID4,TCELL)
        ENDIF
C 
C       get the number of menu cells
        MENLEN=GML(GTWID,6) - 1
        DO 50 I=0,MENLEN
           TCELL = I + 1
           IF(GMBCSV(GTWID,I)(1:1) .NE. ' ') THEN
C             ok there is a valid token
              CALL GTPMEN(GMBSAV(GTWID,I),GMBCSV(GTWID,I),GTWID4,TCELL)
C             hilite if necessary
              IF(INVSAV(GTWID,I)) CALL GTMCHI(GTWID4,TCELL)
            ENDIF
 50     CONTINUE
      ENDIF   
C
      END
C
C-----------------------------------------------------------------
C
                
      SUBROUTINE SAVEMN(GTWID4)
C     =========================
C
C1    vartype             I4
C1    iostatus            I
C
C2    Subroutine MENSAV copies the current state of a
C2    menu  into a safe buffer,to allow use
C2    of that menu for other activity,but enableing
C2    recovery of the previous state.  using
C2    RSTMEN. Only the modifier or noun are copied
C2    depending on GTWID4
C
      include 'include/gtxt2.inc'
C
      INTEGER*2 I,GTWID,MENLEN
      INTEGER*4 GTWID4
C
C
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C     
      IF(GTWID .GT. 0 .AND. GTWID .LE. 4) THEN
C       get the number of menu cells
C       unusually for FORTRAN they index from zero
        MENLEN=GML(GTWID,6) - 1
        DO 50 I=0,MENLEN
C          save highlighting
           INVSAV(GTWID,I) = GMCINV(GTWID,I) 
C          save multiple cell tags
           MULSAV(GTWID,I) = GMCMUL(GTWID,I)          
C          save the tokens
           GMBCSV(GTWID,I) = GMBC(GTWID,I)          
C          save the text
           GMBSAV(GTWID,I) = GMB(GTWID,I) 
 50     CONTINUE
      ENDIF
C
      END
C
C
C-----------------------------------------------------------------
C
