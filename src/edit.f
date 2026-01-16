C
C     @(#)  412.1 date 6/11/92 edit.f 
C
C
C     Filename    : edit.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:50
C     Last change : 92/06/11 14:29:12
C
C     Copyright : Practical Technology International Limited  
C     File :- edit.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE EDIT_CLEANUP()
C     =========================
C
C2    Routine will clean up the output buffer 
C2    turn of all input devices and will delete the 
C
      include    'include/edit.inc'
      include    'include/input.inc'
C
      INTEGER*2 BUFLEN
      INTEGER*2 NLEN2,I
C
      EXTERNAL NLEN2
C
C     Buffer claenup
      IF(CINPN.EQ.0) RETURN
      BUFLEN = NLEN2(BUFFER)
      IF(BUFLEN.GT.0.AND.ENDPOS.GT.0) THEN
          DO 10 I=ENDPOS,BUFLEN
C             reset to spaces
              BUFFER(I:I) = CHAR(32)
10        CONTINUE
C
C         Input devices off
      ENDIF
      CALL SET_INPUT_OFF()
      END


*
      SUBROUTINE EDIT_TEXT_CHAR(CHR,DIRECT,ST)
C     ========================================
C1    VARYPE                     I2  I2  I4 
C1    IOSTAT                     I   I   O
C
C2    Routine will update cursor or text one character at a time
C2    it can cope with curor in the middle of text etc
C2 
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/apfont.inc'
      include    'include/input.inc'
C
      LOGICAL BACK,SHIFT,DIRECT
      LOGICAL UNOBSCURED
      CHARACTER CHR
      INTEGER*4 ST
      INTEGER*2 V1(2),V2(2),V3(2),X,Y,TX,POS
      INTEGER*2 DATA(2)
      INTEGER*2 NPOS,LENGTH
      INTEGER*2 NLEN2,N
      INTEGER*2 NEWPOS(2),BAND
      INTEGER*2 INT2
      INTEGER*4 FONTID
      INTEGER*4 ICHAR
C
      EXTERNAL NLEN2,ICHAR
C
C     draw out old triangle
C

      CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
      FONTID = INPFNT(CINPN)
      CALL SET_FONT(FONTID,ST)
      CALL SET_TEXT('X',ST)
      BAND = TEXTW
      ST  = 0
      X = CPRX 
      Y = CPRY 
      IF(ICHAR(CHR).EQ.EATCHR) THEN
C         eat can simulate normal delete by going right and deleting
          IF(CURPOS.EQ.ENDPOS.OR.CURPOS.EQ.1.AND.
     +       .NOT.CURPOS.LT.ENDPOS) THEN
              GOTO 998
          ENDIF
C         move to right
C
          CALL DRAW_EDIT_CURSOR(LOW)
          CHR = BUFFER(CURPOS:CURPOS)
          CURPOS = CURPOS +1
          CALL SET_TEXT(CHR,ST)
C         recover original position
          CPRX = CPRX + TEXTW + TXSPACE
C         recover original position
          BX = X+ABS(TEXTW-TORGX)
          BY = Y+ABS(TEXTH-TORGY)
          CALL DRAW_EDIT_CURSOR(HIGH)
          X = CPRX 
          Y = CPRY 
          CHR = CHAR(DELCHR)
      ENDIF

      IF(ICHAR(CHR).EQ.DELCHR.OR.ICHAR(CHR).EQ.DEL.
     +   OR.ICHAR(CHR).EQ.255) THEN
C         we are we deleteing 
C         out of range
          IF(CURPOS-STRPOS+1.EQ.1) GOTO 998

C         draw out old cursor
          CALL DRAW_EDIT_CURSOR(LOW)
C
C         set flag for now
          BACK = CURPOS.LT.ENDPOS
C         save current position
          TX = X
          SHIFT = CURPOS.EQ.ENDPOS.AND.STRPOS.GT.1
          CURPOS = CURPOS - 1
C         rub out existing character
          CHR = BUFFER(CURPOS:CURPOS)
          CALL SET_TEXT(CHR,ST)
C         recover original position
          CPRX = CPRX - (TEXTW + TXSPACE)
          CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
C
          CALL TOOLS_TEXT(CPRX,CPRY,CHR,INT2(1),ST)
C
          CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
C         decrement edn pointer
          ENDPOS = ENDPOS -1
C         finnaly move string back out
          IF(BACK) THEN
C             string to move
              MVS = BUFFER(CURPOS+1:ENDPOS)
C             move the buffer up please
C             save current character
              N = NLEN2(MVS)
              DATA(1) = CURPOS+1
              DATA(2) = ENDPOS-CURPOS
              NPOS = CURPOS
              CALL BUFMOV(DATA,NPOS,BUFFER)
C             now move the string
              CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
C             draw text string
              CALL DRAW_TEXT(TX,CPRY,MVS,N,NEWPOS,ST)
              CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
              CALL DRAW_TEXT(CPRX,CPRY,MVS,N,NEWPOS,ST)
          ELSEIF(SHIFT) THEN
              DIRECT = .FALSE.
520           CONTINUE
              CALL EDIT_SHIFT(DIRECT,LENGTH,NEWPOS)
              IF(NEWPOS(2)+1.LT.CURPOS) THEN
                  DIRECT = .TRUE.
                  GOTO 520
              ENDIF

              CPRX = NEWPOS(1)
          ENDIF    
          IF(CURPOS.EQ.1) THEN
              CALL EDIT_INIT()
              GOTO 998
          ENDIF
100       CONTINUE
C         now set cursor postion
          CHR = BUFFER(CURPOS-1:CURPOS-1)
C         get last character details
          CALL SET_TEXT(CHR,ST)
          X = CPRX - (TEXTW + TXSPACE)
          BX = X+ABS(TEXTW-TORGX)
          BY = Y+ABS(TEXTH-TORGY)
C         draw in new cursor
          CALL DRAW_EDIT_CURSOR(HIGH)
C
      ELSEIF(ICHAR(CHR).EQ.KEYL) THEN
C         move key to the left
          IF(CURPOS.EQ.1)  GOTO 998
C         draw out old cursor
          CALL DRAW_EDIT_CURSOR(LOW)
          IF(STRPOS.GT.1.AND.STRPOS.EQ.CURPOS) THEN
C             by definition have some to go
              LENGTH = ENDPOS-STRPOS+1
              CALL EDIT_SHIFT(.FALSE.,LENGTH,NEWPOS)
              CURPOS = CURPOS -1
              CALL EDIT_INIT()
              GOTO 998
          ELSE
              CURPOS = CURPOS -1
              POS = CURPOS
              CHR = BUFFER(POS:POS)
              CALL SET_TEXT(CHR,ST)
C             recover original position
              CPRX = CPRX - (TEXTW + TXSPACE)
C             recover original position
          ENDIF
          IF(CURPOS-STRPOS+1.EQ.1) THEN
C             start of diisplay
              CALL EDIT_INIT()
              GOTO 998
          ENDIF
          CHR = BUFFER(CURPOS-1:CURPOS-1)
C         get last character details
          CALL SET_TEXT(CHR,ST)
          X = CPRX - (TEXTW + TXSPACE)
          BX = X+ABS(TEXTW-TORGX)
          BY = Y+ABS(TEXTH-TORGY)
          CALL DRAW_EDIT_CURSOR(HIGH)
      ELSEIF(ICHAR(CHR).EQ.KEYR) THEN
C         move key to the right
          IF(CURPOS.EQ.ENDPOS) GOTO 998
C         draw out old cursor
          CALL DRAW_EDIT_CURSOR(LOW)
C
          CHR = BUFFER(CURPOS:CURPOS)
          CURPOS = CURPOS +1
          CALL SET_TEXT(CHR,ST)
C         recover original position
          CPRX = CPRX + TEXTW + TXSPACE
          IF(CPRX.GT.SPOSX+ELIMIT) THEN
C             recover original position
              LENGTH = ENDPOS-STRPOS+1
510           CONTINUE
              CALL EDIT_SHIFT(.TRUE.,LENGTH,NEWPOS)
C             make sure that we are under the correct 
              IF(CURPOS.GT.NEWPOS(2)+1.AND.CURPOS.LE.ENDPOS) GOTO 510
              CURPOS = NEWPOS(2)+1
              CHR = BUFFER(CURPOS:CURPOS)
              X = NEWPOS(1)
              CALL SET_TEXT(CHR,ST)
              CPRX = X
              X = X - TEXTW - TXSPACE
          ENDIF
          BX = X+ABS(TEXTW-TORGX)
          BY = Y+ABS(TEXTH-TORGY)
          CALL DRAW_EDIT_CURSOR(HIGH)
C
      ELSE
C         moving forward one character only
          IF(ENDPOS.EQ.MAXCHR) THEN
              GOTO 999
          ENDIF
C         draw out old cursor
          CALL DRAW_EDIT_CURSOR(LOW)
          IF(CURPOS.LT.ENDPOS) THEN
C             block text move to the right
C             set temp string fro block move
              MVS = BUFFER(CURPOS:ENDPOS-1)
C             save current character
              N = ENDPOS - CURPOS
C             remenber to add on display offset
              DATA(1) = CURPOS
              DATA(2) = ENDPOS-CURPOS
              NPOS = CURPOS+1
              CALL BUFMOV(DATA,NPOS,BUFFER)
C             now move the string
              CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
C             draw text string
              CALL DRAW_TEXT(CPRX,CPRY,MVS(1:N),N,NEWPOS,ST)
              CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
              CALL SET_TEXT(CHR,ST)
              TX = CPRX + TEXTW + TXSPACE
              CALL DRAW_TEXT(TX,CPRY,MVS(1:N),N,NEWPOS,ST)
              X = CPRX
              CPRX = X
          ENDIF
          BUFFER(CURPOS:CURPOS) = CHR
          CALL SET_TEXT(CHR,ST)
          CPRX = CPRX + TEXTW + TXSPACE
C         mark end of buffer
C         draw this piece of text into the current space
          CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
          IF(CPRX.GT.SPOSX+ELIMIT) THEN
C             we have input out of bounds
C             move text One position to the left
              POS = ENDPOS
              ENDPOS = CURPOS
500           CONTINUE
              CALL EDIT_SHIFT(.TRUE.,LENGTH,NEWPOS)
C             reset postion for drawing
              X = NEWPOS(1)
              CALL SET_TEXT(CHR,ST)
              CPRX = X + TEXTW + TXSPACE
              IF(CPRX.GT.SPOSX+ELIMIT) THEN
C                 shift again till clear
                  GOTO 500
              ENDIF
              CALL TOOLS_TEXT(X,Y,CHR,INT2(1),ST)
              ENDPOS = POS
          ELSE
              CALL TOOLS_TEXT(X,Y,CHR,INT2(1),ST)
          ENDIF
C         increment curpos
          CURPOS = CURPOS + 1
C         set end of string
          ENDPOS = ENDPOS+1
C         set text values
          BX = X+ABS(TEXTW-TORGX)
          BY = Y+ABS(TEXTH-TORGY)
          CALL DRAW_EDIT_CURSOR(HIGH)
C
      ENDIF
C
      RETURN
999   CONTINUE
C     signal some kind of error
      ST = 1
      CALL BELL()

998   CONTINUE

      END
C
C2  
C
      SUBROUTINE DRAW_TEXT(X,Y,TEXT,LENGTH,NEWPOS,ST)
C     ===============================================
C1    VARYPE              I2 I2 C*(*) I2     I2   I4
C1    IOSTAT              I  I   I    I      I    O
C
C2    Loop round text and draw to our own standards
C2    Useful for inserting and deleteing text blocks
C2  
      include    'include/edit.inc'
      include    'include/input.inc'
      include    'include/daxcolor.inc'
C
      CHARACTER*(*) TEXT,CHR*1
      INTEGER*2 X,Y,LENGTH,NLEN2,N,I,TX,NEWPOS(2)
      INTEGER*2 INT2
      INTEGER*4 FONTID
      INTEGER*4 ST
      LOGICAL UNOBSCURED
C
      EXTERNAL NLEN2
C
      TX = X
      N = LEN(TEXT)
C
      IF(CINPN.GT.0) THEN
C         change font if currently active
          FONTID = INPFNT(CINPN)
          CALL SET_FONT(FONTID,ST)
      ENDIF
C     check text length
      IF(N.EQ.0) RETURN
C
      CALL BATCH_ACQUIRE(.TRUE.,ST)
      DO 10 I=1,N
C
          CHR = TEXT(I:I)
          CALL SET_TEXT(CHR,ST)
          IF(TX+TEXTW+TXSPACE.GT.SPOSX+ELIMIT) THEN
C             is next useable position out of range ?
              NEWPOS(1) = TX 
              IF(I.GT.1) THEN
                 NEWPOS(2) = I-1
              ENDIF
              GOTO 20
          ENDIF
          CALL TOOLS_TEXT(TX,Y,CHR,INT2(1),ST)
C         update next postion
          TX = TX + TEXTW + TXSPACE
C         check for limiter
10    CONTINUE
C     send back next usable text postion
      NEWPOS(1) = TX
      IF(I.GT.1) THEN
          NEWPOS(2) = I-1
      ENDIF
20    CONTINUE
      CALL BATCH_ACQUIRE(.FALSE.,ST)
C
      END


      SUBROUTINE BUFMOV(DATA,NPOS,BUFFER)
C     ===================================
C1    VARYPE             I2   I2   C*(*)
C1    IOSTAT             I    I     I/O
C
C2    Routine will move the specified block from one position to another
C2    It assumes limits have been checked
C2  
C
      INTEGER*2 DATA(2)
      INTEGER*2 NPOS,EPOS,CPOS
      INTEGER*2 I,J
      CHARACTER*(*) BUFFER
C
      CPOS = NPOS
      IF(DATA(1).EQ.0.OR.DATA(2).LE.0.OR.NPOS.EQ.DATA(1)) THEN
C         quick check to see if we need to bother
          GOTO 999
      ENDIF
C
C     do the move
      IF(NPOS.LT.DATA(1)) THEN
          DO 10 I=DATA(1),DATA(1)+DATA(2)-1
              BUFFER(CPOS:CPOS) = BUFFER(I:I)
              CPOS = CPOS+1
10        CONTINUE
      ELSE
          CPOS = NPOS+DATA(2)-1
          DO 20 I=DATA(1)+DATA(2)-1,DATA(1),-1
              BUFFER(CPOS:CPOS) = BUFFER(I:I)
              CPOS = CPOS -1
20        CONTINUE
      ENDIF
999   CONTINUE
      END


      SUBROUTINE EDIT_INIT()
C     ======================
C
C2    This routine will initalise the buffer and its 
C2    cursor pointer
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 ST
C
C     set it for a space
      CALL SET_TEXT(' ' ,ST)
C
      BX = SPOSX
      BY = SPOSY+ABS(TEXTH-TORGY)
      CURPOS = STRPOS
C
      CALL DRAW_EDIT_CURSOR(HIGH)
C
      END


      SUBROUTINE DRAW_EDIT_CURSOR(LOHI)
C     =================================
C1    VARYPE                       L
C1    IOSTAT                       I
C
C2    wee routine to draw the editng cursor
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/daxcad_x.inc'
      include    'include/cursor.inc'
C
      INTEGER*4 ST,CFILL
      INTEGER*2 V1(2)
      INTEGER*2 V2(2)
      INTEGER*2 V3(2)
      LOGICAL LOHI
      LOGICAL TMPCUR
      CHARACTER*80 BUFF
C
C     do we draw if allready high lighted
      IF((LOHI.AND.TRCUR).OR.
     +   (.NOT.LOHI.AND..NOT.TRCUR) ) RETURN
C
C     set fill value correctly
      
      TMPCUR = .FALSE.
      IF ( .NOT.XVERSION ) THEN
          IF ( CURSON ) THEN
              CALL CURSOR_ON(.FALSE.)
              TMPCUR = .TRUE.
          ENDIF
      ENDIF
      CALL TOOLPEN(EDCOL)
      CALL ROPXOR()
C     set vertices
      V1(2) = BY+1
      V2(2) = BY+6
      V3(2) = BY+6
      V1(1) = BX
      V2(1) = BX-5
      V3(1) = BX+5
      TRCUR = LOHI
C     Draw the triangle
      CALL TOOLS_TRIANGLE(V1,V2,V3,ST)
      CALL ROPREP()
      IF ( .NOT.XVERSION ) THEN
          IF ( TMPCUR ) THEN
              CALL CURSOR_ON(.TRUE.)
          ENDIF
      ENDIF
C
      END

C
      SUBROUTINE SET_CURSOR(POS,ST)
C     =============================
C1    VARYPE                 I2 I4
C1    IOSTAT                 I  O
C
C2    This routine will set an absolute position on the 
C2    defined by the argument POS 
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*2 POS
      INTEGER*2 TX,I,TY,X,Y,J,NEWPOS(2),N
      INTEGER*4 ST
      CHARACTER CHR
      LOGICAL REDRAW
C
      CALL DRAW_EDIT_CURSOR(LOW)
      CALL SET_TEXT(' ' ,ST)
      TY = SPOSY+ABS(TEXTH-TORGY)
      TX = SPOSX
      X = SPOSX
      Y = SPOSY 
      DO 20 I=STRPOS,POS-1
          CHR = BUFFER(I:I)
          CALL SET_TEXT(CHR ,ST)
          TX = X+ABS(TEXTW-TORGX)
          TY = Y+ABS(TEXTH-TORGY)
          X = X + TEXTW + TXSPACE
 20   CONTINUE
C
C     check ranges for drawing on the screen
      REDRAW= .FALSE.
      IF(X.GT.SPOSX+ELIMIT) THEN
C         outeide must back track to right hand extents
C         set starting value
          X = SPOSX+ELIMIT
          DO 200 I=ENDPOS-1,1,-1
              CHR= BUFFER(I:I)
              CALL SET_TEXT(CHR,ST)
              X = X - TEXTW- TXSPACE
              IF(X.LT.SPOSX) THEN
C                 save starting postion for text in buffer
C                 must be previous character
                  J = I+1
                  REDRAW = .TRUE.
                  GOTO 300
              ENDIF
200       CONTINUE
      ENDIF
      IF(POS.LT.STRPOS) THEN
C         implicit restart
          J = 1
          REDRAW = .TRUE.
      ENDIF
300   CONTINUE
      IF(REDRAW) THEN
C         start by taking out existing string
          CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
          MVS = BUFFER(STRPOS:ENDPOS-1)
          N = ENDPOS -  STRPOS
          CALL DRAW_TEXT(SPOSX,SPOSY,MVS(1:N),N,NEWPOS,ST)
          CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
          STRPOS = J
          MVS = BUFFER(STRPOS:ENDPOS-1)
          N = ENDPOS -  STRPOS
          CALL DRAW_TEXT(SPOSX,SPOSY,MVS(1:N),N,NEWPOS,ST)
          IF(POS.EQ.1) THEN
C             set higlight and next postion at start
              BX = SPOSX
              BY = SPOSY
              CPRX = SPOSX
          ELSE
C             set higlight and next postion at end
              CPRX = NEWPOS(1)
              CHR = BUFFER(ENDPOS-1:ENDPOS-1)
              CALL SET_TEXT(CHR,ST)
              X = CPRX - TEXTW -TXSPACE
C             get cursor postion
              BX = X+ABS(TEXTW-TORGX)
              BY = Y+ABS(TEXTH-TORGY)
          ENDIF
      ELSE
          BX = TX
          BY = TY
          CPRX = X
      ENDIF

      CURPOS = POS
C     draw the cursor
      CALL DRAW_EDIT_CURSOR(HIGH)
      END
C
      SUBROUTINE EDIT_LINE_DEL()
C     ==========================
C
C2    This routine deletes the current line of text
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
C
      INTEGER*2 POS,N,NEWPOS(2)
      INTEGER*4 ST
      INTEGER*4 FONTID
C
      IF(ENDPOS.EQ.1) RETURN
C
      CALL DRAW_EDIT_CURSOR(LOW)
C
      CPRX = SPOSX
      N = ENDPOS - 1
      MVS = BUFFER(STRPOS:ENDPOS-1)
C     set erase colour
      CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
C     set font
      FONTID = INPFNT(CINPN)
      CALL SET_FONT(FONTID,ST)
C     draw text string
      CALL DRAW_TEXT(CPRX,CPRY,MVS,N,NEWPOS,ST)
      CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
      BUFFER = ' '
      CURPOS = 1
      ENDPOS = 1
      STRPOS = 1
      CALL EDIT_INIT()
      POS = 1
      CALL SET_CURSOR(POS,ST)
      END
C
      SUBROUTINE CURSOR_LEFT()
C     ========================
C
C2    This routine moves the cursor to the start of the line
C2    by a key press
C  
      include    'include/edit.inc'
C
      INTEGER*2 POS
      INTEGER*4 ST
C
      POS = 1
      CURPOS = 1
      CALL SET_CURSOR(POS,ST)

      END
C
      SUBROUTINE CURSOR_RIGHT()
C     ==========================
C
C2    This routine moves the cursor to the end of the line
C2    by a key press
C  
      include    'include/edit.inc'
C
      INTEGER*2 POS
      INTEGER*4 ST
C
      POS = ENDPOS
      CALL SET_CURSOR(POS,ST)
      CURPOS = POS

      END
C
C
C-------------------------------------------------------------
      SUBROUTINE EDIT_SHIFT(DIR,LENGTH,NEWPOS)
C     =================================
C1    VARYPE                 L     I2
C1    IOSTAT                 I     O
C
C2    Shifts current display left or right 
C2    if left or right exceeds pixel display limit
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 NOC,ST,COL
      CHARACTER*256 TEXT
      INTEGER*2 X,Y,LENGTH,NEWPOS(2)
      LOGICAL DIR
C
      X = SPOSX
      Y = SPOSY
      LENGTH = ENDPOS
      IF(STRPOS.LT.ENDPOS) THEN
C         some text is available undraw before the shift
          TEXT = BUFFER(STRPOS:ENDPOS-1)
          CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
          CALL DRAW_TEXT(X,Y,TEXT(1:LENGTH),LENGTH,NEWPOS,ST)
      ENDIF
C     get direction of shift
      IF (DIR) THEN
          STRPOS = STRPOS + 1
      ELSE
          STRPOS = STRPOS - 1
      ENDIF
      TEXT = BUFFER(STRPOS:ENDPOS-1)
      CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
      LENGTH = ENDPOS - STRPOS
      CALL DRAW_TEXT(X,Y,TEXT(1:LENGTH),LENGTH,NEWPOS,ST)
      NEWPOS(2) = STRPOS + NEWPOS(2) -1
C
      END
C
      SUBROUTINE EDIT_PASTE(ST)
C     ========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Pastes into the edit devuce currently active
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
C
      CHARACTER*256 TEXT
      INTEGER*4 N,ST
      INTEGER*4 FONTID
      INTEGER*2 NLEN2
      INTEGER*2 DATA(2)
      INTEGER*2 X,Y,I,J,K
      INTEGER*2 NPOS,LENGTH,NEWPOS(2)
      CHARACTER*4 ILLEG
C
      EXTERNAL NLEN2
C
C     set up illegal characters
      ILLEG(1:1) = CHAR(150)
      ILLEG(2:2) = CHAR(0)
      ILLEG(3:3) = CHAR(10)
      ILLEG(4:4) = CHAR(13)
      TEXT = ' '
      CALL GET_PASTE_FILE(TEXT,ST)
      N = NLEN2(TEXT)
C     strip out illegal chars
      K = 0
      DO 10 I=1,N
          J = INDEX(ILLEG,TEXT(I:I))
          IF(J.EQ.0) THEN
              K = K + 1
              MVS(K:K) = TEXT(I:I)
          ENDIF
10    CONTINUE
C     save new length
      TEXT = MVS
      N = K
      IF(ST.EQ.0.AND.N.GT.0) THEN
C         set font
          FONTID = INPFNT(CINPN)
          CALL SET_FONT(FONTID,ST)
          CALL TOOLPEN_TEXT(EDBACK,EDBACK,.TRUE.,ST)
C         get current positon
          X = CPRX
          Y = CPRY
          IF(ENDPOS.GT.1.AND.CURPOS.LT.ENDPOS) THEN
              NPOS = CURPOS + N
              IF(NPOS.GT.MAXCHR) THEN
                  GOTO 999
              ENDIF
              MVS = BUFFER(CURPOS:ENDPOS-1)
              LENGTH = NLEN2(MVS)
              IF(ENDPOS+N.GT.MAXCHR) THEN
                  GOTO 999
              ENDIF
              CALL DRAW_TEXT(X,Y,MVS(1:LENGTH),LENGTH,NEWPOS,ST)
              DATA(1) = CURPOS
              DATA(2) = ENDPOS-CURPOS
              MVS = BUFFER(CURPOS:ENDPOS-1)
              CALL BUFMOV(DATA,NPOS,BUFFER)
          ENDIF
          IF(ENDPOS+N.GT.MAXCHR) THEN
              GOTO 999
          ENDIF
          ENDPOS = ENDPOS+ N
          BUFFER(CURPOS:CURPOS+N-1) = TEXT
          CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
          CALL DRAW_TEXT(X,Y,BUFFER(CURPOS:ENDPOS),LENGTH,NEWPOS,ST)
      ENDIF
      ST = 0
      RETURN
999   CONTINUE
      CALL BELL()
      ST = 1
      END
C

      SUBROUTINE CALC_SHIFT(TEXT,NOC,POS,ST)
C     ======================================
C1    VARYPE                C*(*) I2  I2 I4 
C1    IOSTAT                 I    O   O  O
C
C     test how many characters will be drawn
C2  
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
C
      CHARACTER*(*) TEXT,CHR*1
      INTEGER*2 X,Y,LENGTH,NLEN2,N,I,TX,POS,NOC
      INTEGER*4 ST
C
      EXTERNAL NLEN2
C
      TX = SPOSX
      N = LEN(TEXT)
      
C     check text length
      IF(N.EQ.0) RETURN
C
      DO 10 I=1,N
C
          CHR = TEXT(I:I)
          CALL SET_TEXT(CHR,ST)
          IF(TX+TEXTW+TXSPACE.GT.SPOSX+ELIMIT) THEN
C             is next useable position out of range ?
              POS = TX 
              IF(I.GT.1) THEN
                 NOC = I-1
              ENDIF
              GOTO 20
          ENDIF
C         update next postion
          TX = TX + TEXTW + TXSPACE
C         check for limiter
10    CONTINUE
C     send back next usable text postion
      POS = TX
      IF(I.GT.1) THEN
          NOC = I-1
      ENDIF
20    CONTINUE
C
      END



