C
C     @(#)  412.1 date 6/11/92 input.f 
C
C
C     Filename    : input.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:57
C     Last change : 92/06/11 14:31:57
C
C     Copyright : Practical Technology International Limited  
C     File :- input.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INPUT_INIT(ST)
C     SUBROUTINE INPUT_MAKE(X,Y,INPNUM,COL,BCOL,FONT,LIMIT,
C     SUBROUTINE INPUT_DRAW(INPNUM,DRAW,ALL,ST)
C     SUBROUTINE SET_INPUT_DEVICE(INPNUM,ST)
C     SUBROUTINE SET_INPUT_OFF()
C     SUBROUTINE SET_INPUT(SCRPOS,INPNUM,ST)
C     SUBROUTINE SET_CURSOR_POS(SCRPOS,INPNUM,MAXPOS,ST)
C     SUBROUTINE INPUT_RESET(INPNUM,ST)
C     SUBROUTINE INPUT_MODIFY(INPNUM,TEXT,BUFLEN,ST)
C     SUBROUTINE INPDRW(INPNUM,DISP,ST)
C     SUBROUTINE INPACC(INPNUM,ST)
C     SUBROUTINE INPACC_POS(INPNUM,POS,ST)
C     SUBROUTINE INPUT_GET_POS(INPNUM,POS,ST)
C     SUBROUTINE INPUT_GET_TEXT(INPNUM,BUFF,BUFLEN,ST)
C     SUBROUTINE INPGTEXT(INPNUM,BUFF,INLEN,BUFLEN,ST)
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE INPUTINIT(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Inialise input strings and set type of font to be used
C2    This number appiles to DAXCAD font number which holds 
C2    apollo font identifiers
C
      include    'include/edit.inc'
      include    'include/apfont.inc'
      include    'include/input.inc'
C
      INTEGER*4 I
      INTEGER*4 ST
C
      DO 10 I=1,INPMAX
C         set active flag
          INPACT(I) = .FALSE.
          INPDIS(I) = .FALSE.
          INPCUR(I) = 1
          INPEND(I) = 1

C         reset text ascociated with it
          INPBUF(I) = ' ' 
10    CONTINUE
C
C     set curwent count
      TRCUR = .FALSE.
      BUFFER = ' '
      CURPOS = 1
      ENDPOS = 1
C     set input device to 0
      CINPN = 0
      END
C
      SUBROUTINE INPUT_MAKE(X,Y,INPNUM,COL,BCOL,FONT,LIMIT,TYPE,ST)
C     =============================================================
C1    VARYPE                I4 I4 I2    I4  I4    I4   I4   I4  I4
C1    IOSTAT                 I I   I    I   I     I    I    I   O
C
C2    This routine is called when the user wants to place 
C2    a input on the screen. A input number is returned
C2    as the identifier which will be returned on any hits
C2    made on the button when displayed
C
      include    'include/input.inc'
      include    'include/apfont.inc'
      include    'include/edit.inc'
C
      INTEGER*4 FONTID
      INTEGER*4 LIMIT
      INTEGER*4 X,Y
      INTEGER*4 INPNUM,I,FONT
      INTEGER*4 ST,COL,TYPE,LABEL
      INTEGER*4 BCOL
      INTEGER*4 SIZE
      INTEGER*4 NLEN,LENGTH
      INTEGER*4 XMIN,YMIN,XMAX,YMAX
      CHARACTER TEXT*256
C
      EXTERNAL NLEN
C
      IF(INPNUM.EQ.0) THEN
          DO 10 I=1,INPMAX
              IF(.NOT.INPACT(I)) THEN
                  INPNUM = I
                  GOTO 20
              ENDIF
 10       CONTINUE
      ENDIF
C
20    CONTINUE
C
      INPACT(INPNUM) = .TRUE.
      INPDIS(INPNUM) = .FALSE.
      INPPOS(1,INPNUM) = X
      INPPOS(2,INPNUM) = Y
      INPCTR(1,INPNUM) = X
      INPCTR(2,INPNUM) = Y
      INPCTX(1,INPNUM) = X
      INPCTX(2,INPNUM) = Y
      INPCOL(INPNUM) = COL
      INPBCL(INPNUM) = BCOL
      INPLIM(INPNUM) = LIMIT
      INPSTR(INPNUM) = 1
      INPTYP(INPNUM) = TYPE
      INPLAB(INPNUM) = LABEL
      IF(FONT.LE.0.OR.FONT.GT.100) THEN
C         set as current loaded font
          CALL INQ_FONT(FONT,ST)
      ENDIF
C     set text width dummy
      TEXTW = 0
      INPFNT(INPNUM) = FONT
C
C     this is the frame mod
C
      CALL SET_FONT(FONT,ST)
      CALL GET_TEXT_SIZE(SIZE,ST)
C
      IF(INPTYP(INPNUM).EQ.1) THEN
          XMIN = X-CURSW
          YMIN = Y-CURSH-SIZE
          XMAX = XMIN + LIMIT +(2*CURSW) + TEXTW
          YMAX = YMIN + SIZE + (2*CURSH) + 6
      ELSE
          XMIN = X
          YMIN = Y-SIZE
          XMAX = XMIN + LIMIT + CURSW + TEXTW
          YMAX = YMIN + SIZE + CURSH
      ENDIF
C
      INPFRM(1,INPNUM) = XMIN
      INPFRM(2,INPNUM) = YMIN
      INPFRM(3,INPNUM) = XMAX
      INPFRM(4,INPNUM) = YMAX
      ST = 0
C
      END

C
      SUBROUTINE INPUT_DRAW(INPNUM,DRAW,ALL,ST)
C     =========================================
C1    VARYPE                  I4    L   L   I4
C1    IOSTAT                  I     I   I   O
C
C
C2    This routine will draw the litte edge on side of 
C2    the main text input device. It draws it as inactive
C2    and any text which exists in the buffers.
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
      include    'include/apfont.inc'
      include    'include/dialog.inc'
C
      INTEGER*4 INPNUM
      INTEGER*4 NEWPOS(2)
      INTEGER*4 LABNUM
      INTEGER*4 ST,A,I,NLEN,LENGTH,LX,LY,X,Y
      INTEGER*4 FONTID
      INTEGER*2 SX,SY
      CHARACTER TEXT*256
      INTEGER*2 XMIN,YMIN,XMAX,YMAX
      INTEGER*2 RECT(4)
      LOGICAL UNOBSCURED
      LOGICAL DRAW
      LOGICAL ALL
C
      EXTERNAL NLEN
C
C
C     draw in any label and modify starting position
C     draw the frame box
      INPDIA(INPNUM) = DIAGCR
      CALL ROPREP()
      LENGTH = NLEN(INPBUF(INPNUM))
C     set font of font
      FONTID = INPFNT(INPNUM)
      CALL SET_FONT(FONTID,ST)
C
      BX = INPCTR(1,INPNUM)
      BY = INPCTR(2,INPNUM) 
      CPRX = INPCTX(1,INPNUM)
      CPRY = INPCTX(2,INPNUM)
C
      IF(INPTYP(INPNUM).EQ.1) THEN
C
          IF ( DRAW ) THEN
              CALL TOOLPEN(INPBCL(INPNUM))
              RECT(1) = INPFRM(1,INPNUM)
              RECT(2) = INPFRM(2,INPNUM)
              RECT(3) = INPFRM(3,INPNUM)-INPFRM(1,INPNUM)+1
              RECT(4) = INPFRM(4,INPNUM)-INPFRM(2,INPNUM)+1
              CALL TOOLS_RECTANGLE(RECT,ST)
              CALL TOOLPEN(INPCOL(INPNUM))
          ENDIF
          XMIN = INPFRM(1,INPNUM)
          YMIN = INPFRM(2,INPNUM)
          XMAX = INPFRM(3,INPNUM)
          YMAX = INPFRM(4,INPNUM)
C         draw the frame box
          CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
C
      ENDIF
      IF( LENGTH .GT.0) THEN
C         Draw in old text if nesessary
          CURPOS = INPCUR(INPNUM)
          ENDPOS = INPEND(INPNUM)
          BUFFER = INPBUF(INPNUM)
          SPOSX = INPPOS(1,INPNUM)
          SPOSY = INPPOS(2,INPNUM)
          STRPOS = INPSTR(INPNUM)
          EDCOL = INPCOL(INPNUM)
          EDBACK = INPBCL(INPNUM)
          ELIMIT = INPLIM(INPNUM)
C
          SX = SPOSX
          SY = SPOSY
C
          IF(.NOT.DRAW.AND..NOT.ALL)  THEN
C             clear the frame bit with text
              RECT(1) = SX
              RECT(2) = INPFRM(2,INPNUM)
              RECT(3) = ELIMIT
              RECT(4) = INPFRM(4,INPNUM)-INPFRM(2,INPNUM)+1
              CALL TOOLS_RECTANGLE(RECT,ST)
              GOTO 999
          ENDIF
          CALL TOOLPEN_TEXT(EDCOL,EDBACK,.TRUE.,ST)
          CALL DRAW_TEXT(SX,SY,BUFFER(STRPOS:),LENGTH,NEWPOS,ST)
C
      ENDIF
C
C     draw frame or marker at the moment
      IF(INPTYP(INPNUM).EQ.1) THEN
C
          XMIN = INPFRM(1,INPNUM)
          YMIN = INPFRM(2,INPNUM)
          XMAX = INPFRM(3,INPNUM)
          YMAX = INPFRM(4,INPNUM)
C         draw the frame box
          CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
C
      ENDIF
999   CONTINUE
      END
C
C
      SUBROUTINE SET_INPUT_DEVICE(INPNUM,ST)
C     ======================================
C1    VARYPE                        I4   I4
C1    IOSTAT                        I    O
C
C2    This routine will copy the contents of the device buffers
C2    into the main editor buffer. If another device is currently in 
C2    use then the editor buffer will be copied back It sets the device
C2    active on the screen. It set the font for the input text.
C
      include    'include/edit.inc'
      include    'include/input.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 INPNUM
      INTEGER*4 ST
      INTEGER*4 FONTID
      INTEGER*2 SX,SY
C

      IF(CINPN.EQ.INPNUM ) THEN
C         we are allready active or not displayed or out of range
          ST = 2
          RETURN
      ENDIF
C
C     save current fontid
      IF(CINPN.GT.0) THEN
C         turn of cursor from this one
          CALL DRAW_EDIT_CURSOR(LOW)
C         copy back all information to his buffer
          INPCUR(CINPN) = CURPOS
          INPEND(CINPN) = ENDPOS
          INPCTR(1,CINPN) = BX
          INPCTR(2,CINPN) = BY
          INPCTX(1,CINPN) = CPRX
          INPCTX(2,CINPN) = CPRY
          IF(ENDPOS.GT.1) THEN
              INPBUF(CINPN) = BUFFER(1:ENDPOS-1)
          ELSE
              INPBUF(CINPN) = ' '
          ENDIF
          INPSTR(CINPN) = STRPOS
      ENDIF
C     set as activtivity flag
      CINPN = INPNUM
C
      IF(INPNUM.EQ.0) GOTO 900
C
C     load up new buffers and pointers
      CURPOS = INPCUR(CINPN)
      ENDPOS = INPEND(CINPN)
      BX = INPCTR(1,CINPN)
      BY = INPCTR(2,CINPN) 
      CPRX = INPCTX(1,CINPN)
      CPRY = INPCTX(2,CINPN)
      BUFFER = INPBUF(CINPN)
      SPOSX = INPPOS(1,CINPN)
      SPOSY = INPPOS(2,CINPN)
      STRPOS = INPSTR(CINPN)
      EDCOL = INPCOL(CINPN)
      EDBACK = INPBCL(CINPN)
      ELIMIT = INPLIM(CINPN)
C     Now set marker as here
      FONTID = INPFNT(CINPN)
C     set font for this device
      CALL SET_FONT(FONTID,ST)
900   CONTINUE
      END
C
      SUBROUTINE SET_INPUT_OFF()
C     ==========================
C
C2    This routine will turn off input devices
C
      include    'include/edit.inc'
      include    'include/input.inc'
      INTEGER*4 INPNUM
      INTEGER*4 ST
C
C     no need to continue if none active
      IF(CINPN.EQ.0) RETURN
      INPNUM = 0
C
      CALL SET_INPUT_DEVICE(INPNUM,ST)
C     low marker cursor please
      CALL DRAW_EDIT_CURSOR(LOW)
C
      END


      SUBROUTINE SET_INPUT(SCRPOS,INPNUM,ST)
C     ======================================
C1    VARYPE                 I2      I4  I4
C1    IOSTAT                 I       O/I   O
C
C2    This is the main routine for activating an input device should the 
C2    the cursor be in the vicinity of the input device text
C2    This routine calls the device activator and this then does the 
C2    necessary highlighting
C
      include    'include/edit.inc'
      include    'include/input.inc'
      include    'include/daxcolor.inc'
      INTEGER*2 SCRPOS(2)
      INTEGER*4 INPNUM
      INTEGER*2 MAXPOS
      INTEGER*4 ST
C
      INPNUM = 0
      ST = 1
      IF( CINPN.GT.0 ) THEN
C         copy back all information to his buffer
          INPCUR(CINPN) = CURPOS
          INPEND(CINPN) = ENDPOS
          INPCTR(1,CINPN) = BX
          INPCTR(2,CINPN) = BY
          INPCTX(1,CINPN) = CPRX
          INPCTX(2,CINPN) = CPRY
          INPBUF(CINPN) = BUFFER
          INPSTR(CINPN) = STRPOS
      ENDIF
C
      CALL SET_CURSOR_POS(SCRPOS,INPNUM,MAXPOS,ST)
C
      IF(MAXPOS.GT.0.AND.INPNUM.GT.0) THEN
           ST = 0
           CALL SET_INPUT_DEVICE(INPNUM,ST)
           CALL SET_CURSOR(MAXPOS,ST)
      ELSE
C         we have no input signaled
          ST = 1
      ENDIF
C
      END
C-------------------------------------------------------------
C
      SUBROUTINE SET_CURSOR_POS(SCRPOS,INPNUM,MAXPOS,ST)
C     ================================================
C1    VARYPE                     I2      I4    I2   I4
C1    IOSTAT                     I       O      O   O
C
C2    This routine wil return the closest cursor postion and the closest
C2    input device when called. No cursor is drawn here this must be done else
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
      include    'include/dialog.inc'
C
      INTEGER*2 SCRPOS(2)
      INTEGER*4 INPNUM
      INTEGER*2 MAXPIX
      INTEGER*2 MAXPOS
      INTEGER*4 FONTID
      INTEGER*4 DIST
      INTEGER*2 I,X,Y,J
      INTEGER*4 TX,TY
      INTEGER*4 ST
      INTEGER*4 XMIN,YMIN,XMAX,YMAX
      INTEGER*4 XP,YP
      INTEGER*4 ELEM,N,A
      LOGICAL OK
      CHARACTER CHR,STR*128
      LOGICAL PNTBOX
C
      EXTERNAL PNTBOX
C
C     set maximum value here
      MAXPIX = 1024
      MAXPOS = 0
      OK = .TRUE.
C
C     save current font 
C     do a box check first 
      DO 20 J=1,INPMAX
          IF(INPDIS(J).AND.INPDIA(J).EQ.DIAGCR ) THEN
               XMIN = INPFRM(1,J)
               YMIN = INPFRM(2,J)
               XMAX = INPFRM(3,J)
               YMAX = INPFRM(4,J)
               XP = SCRPOS(1)
               YP = SCRPOS(2)
C              if not in box text next one
               IF(.NOT.PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)) THEN
                   GOTO 30
               ENDIF
               TX = INPPOS(1,J)
               TY = INPPOS(2,J)
               FONTID = INPFNT(J)
C              set font for measuring ops
               CALL SET_FONT(FONTID,ST)
C              text along exsting string
               DO 10 I=INPSTR(J),INPEND(J)
C                  
                   DIST = ((SCRPOS(1)-TX)**2)+((SCRPOS(2)-TY)**2)
                   IF(DIST.GT.0) THEN
                       DIST= SQRT(REAL(DIST))
                   ENDIF
                   IF(DIST.LT.MAXPIX) THEN
                       MAXPIX = DIST
                       MAXPOS = I
                       INPNUM = J
                   ENDIF
C    
                   CHR = INPBUF(J)(I:I)
                   CALL SET_TEXT(CHR,ST)
C                  increment next value
                   TX = TX + TEXTW + TXSPACE
  10           CONTINUE
          ENDIF
 30   CONTINUE
 20   CONTINUE
      
C
 99   CONTINUE
      END
C
      SUBROUTINE INPUT_RESET(INPNUM,ST)
C     =================================
C1    VARYPE                   I4   I4
C1    IOSTAT                   I    O
C
C2    Routine will reset input device number by clearng line
C2    from screen and reseting buffers 
C2 
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
C
      INTEGER*4 INPNUM
      INTEGER*2 SX,SY
      INTEGER*4 ST
C
      IF(INPNUM.GT.0) THEN
C
          CALL SET_INPUT_DEVICE(INPNUM,ST)
          CALL EDIT_LINE_DEL()
          CALL SET_INPUT_OFF()
C
          SX = INPPOS(1,INPNUM)
          SY = INPPOS(2,INPNUM)
          INPCUR(INPNUM) = 1
          INPEND(INPNUM) = 1
          INPCTR(1,INPNUM) = SX
          INPCTR(2,INPNUM) = SY
          INPCTX(1,INPNUM) = SX
          INPCTX(2,INPNUM) = SY
          INPBUF(INPNUM) = ' '
          INPSTR(INPNUM) = 1

      ENDIF


      END
      SUBROUTINE INPUT_MODIFY(INPNUM,TEXT,BUFLEN,ST)
C     ==============================================
C1    VARYPE                    I4   C*(*)  I4   I4
C1    IOSTAT                    I    I      I     O
C
C2    This routine will modify a piece of text currently 
C2    in the input number to the one being  passed
C     If the text is modified during an active session then this will 
C2    not save the current session this one only so beware makesure its off first
C  
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
C
      INTEGER*4 INPNUM
      INTEGER*2 SX,SY
      INTEGER*4 ST,NLEN,BUFLEN
      CHARACTER*256 TEXT
      LOGICAL REDRAW
C
      EXTERNAL NLEN
C
      IF(BUFLEN.LT.0.OR.BUFLEN.GT.MAXCHR) GOTO 999

      IF(INPNUM.GT.0 ) THEN
C
          REDRAW = .FALSE.
          IF(INPDIS(INPNUM)) THEN
C             erase the current cell
              CALL SET_INPUT_OFF()
              CALL TOOLPEN(INPBCL(INPNUM))
              CALL INPUT_DRAW(INPNUM,.FALSE.,.FALSE.,ST)
              REDRAW = .TRUE.
          ENDIF
C         reset full buffers
          SX = INPPOS(1,INPNUM)
          SY = INPPOS(2,INPNUM)
          INPCUR(INPNUM) = 1
          INPEND(INPNUM) = BUFLEN+1
          INPCTR(1,INPNUM) = SX
          INPCTR(2,INPNUM) = SY
          INPCTX(1,INPNUM) = SX
          INPCTX(2,INPNUM) = SY
          IF(BUFLEN.EQ.0) THEN
              INPBUF(INPNUM) = ' '
          ELSE
              INPBUF(INPNUM) = TEXT(1:BUFLEN)
          ENDIF
          INPSTR(INPNUM) = 1
          IF(REDRAW) THEN
              CALL TOOLPEN(INPCOL(INPNUM))
              CALL INPUT_DRAW(INPNUM,.TRUE.,.FALSE.,ST)
          ENDIF

      ELSE
          GOTO 999
      ENDIF
      ST = 0
C
      RETURN
999   CONTINUE
      ST = 1
      END
C      
C
C
C
      SUBROUTINE INPDRW(INPNUM,DISP,ST)
C     =================================
C1    VARYPE              I4    L   I4
C1    IOSTAT              I     I   O
C
C2    User driven routine to draw on and off input
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
C
      INTEGER*4 INPNUM,LABNUM
      INTEGER*4 ST
      LOGICAL DISP
C
      ST = 0
      IF(INPNUM.LT.0.OR.INPNUM.GT.INPMAX) GOTO 999
      IF(DISP.AND..NOT.INPDIS(INPNUM)) THEN
          CALL TOOLPEN(INPCOL(INPNUM))
          INPDIS(INPNUM) = .TRUE.
      ELSEIF(.NOT.DISP.AND.INPDIS(INPNUM)) THEN
          INPDIS(INPNUM) = .FALSE.
          CALL SET_INPUT_OFF()
          CALL TOOLPEN(INPBCL(INPNUM))
      ELSE
          RETURN
      ENDIF
C     draw or erase the text
      CALL INPUT_DRAW(INPNUM,DISP,.TRUE.,ST)
      RETURN
999   ST = 1

      END
      SUBROUTINE INPACC(INPNUM,ST)
C     ============================
C1    VARYPE              I4   I4
C1    IOSTAT              I    O
C
C2    Routine will activate the specified at the current 
C2    Triangular postion
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 ST
      INTEGER*2 MAXPOS
      INTEGER*4 FONTID
      INTEGER*4 INPNUM
C
C     set font for this one
      FONTID = INPFNT(INPNUM)
      CALL SET_FONT(FONTID,ST)
C
C
      IF(CINPN.GT.0) THEN
          CALL SET_INPUT_OFF()
      ENDIF
      IF(INPNUM.GT.0) THEN
          CALL SET_INPUT_DEVICE(INPNUM,ST)
          MAXPOS = ENDPOS
          CALL SET_CURSOR(MAXPOS,ST)
      ENDIF
C
      END
C
      SUBROUTINE INPACC_POS(INPNUM,POS,ST)
C     ====================================
C1    VARYPE                I4     I4  I4
C1    IOSTAT                I      I   O
C
C2    Routine will activate the specified at the SPECIFIED
C2    cursor position
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 POS
      INTEGER*4 ST
      INTEGER*2 MAXPOS
      INTEGER*4 FONTID
      INTEGER*4 INPNUM
C
C     set font for this one
      FONTID = INPFNT(INPNUM)
      CALL SET_FONT(FONTID,ST)
C
C
      IF(CINPN.GT.0) THEN
          CALL SET_INPUT_OFF()
      ENDIF
      IF(INPNUM.GT.0) THEN
          CALL SET_INPUT_DEVICE(INPNUM,ST)
C         set postion requred
          IF ( POS.GT.INPEND(INPNUM))  THEN
              MAXPOS = INPEND(INPNUM)
          ELSEIF ( POS.LT.1 )THEN
              MAXPOS = 1
          ELSE
              MAXPOS = POS
          ENDIF
          
          CALL SET_CURSOR(MAXPOS,ST)
C         store value on the way out 
          POS = MAXPOS
      ENDIF
C
      END
C
      SUBROUTINE INPUT_GET_POS(INPNUM,POS,ST)
C     =======================================
C1    VARYPE                    I4     I4  I4
C1    IOSTAT                     I      O   O
C
C2    Gets the current cursor position of the triangular cursor.
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/input.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 POS
      INTEGER*4 ST
      INTEGER*2 MAXPOS
      INTEGER*4 FONTID
      INTEGER*4 INPNUM
C
      IF(INPNUM.GT.0.AND.INPNUM.LT.INPMAX) THEN

C         Get the postion
          POS = INPCUR(INPNUM)
          ST = 0

      ELSE 
C         out of range error
          ST = 1
      ENDIF
C
      END
C
C
      SUBROUTINE INPUT_GET_TEXT(INPNUM,BUFF,BUFLEN,ST)
C     ================================================
C1    VARYPE                      I4    C    I4   I4
C1    IOSTAT                      I    I/O   O    O
C
C2    This routine reads the input string currently in INPNUM (OLD version )
C  
      INTEGER*4 ST
      INTEGER*4 BUFLEN
      INTEGER*4 INPNUM
      INTEGER*4 INLEN
      INTEGER*4 LENGTH
      CHARACTER*256 BUFF
C
C     for old compatibilty use this one instead
      INLEN = BUFLEN
C
      CALL INPGTEXT(INPNUM,BUFF,INLEN,BUFLEN,ST)
C
      END
C
C
      SUBROUTINE INPGTEXT(INPNUM,BUFF,INLEN,BUFLEN,ST)
C     ================================================
C1    VARYPE               I4    C     I4    I4   I4
C1    IOSTAT               I    I/O     I     O    O
C
C2    This routine reads the input sttring currently in INPNUM
C  
      include    'include/edit.inc'
      include    'include/input.inc'
C
      INTEGER*4 ST
      INTEGER*4 BUFLEN
      INTEGER*4 INPNUM
      INTEGER*4 INLEN
      INTEGER*4 LENGTH
      CHARACTER*256 BUFF
C
      EXTERNAL NLEN
C
C     range check
      IF(INPNUM.LE.0.OR.INPNUM.GT.INPMAX) THEN
          ST = 1
          GOTO 999
      ENDIF

      IF(INLEN.LE.0 ) THEN
          ST = 2
          GOTO 999
      ENDIF

      IF(INLEN.GT.LEN(BUFF)) THEN
C         set length as buffer length must chop
          LENGTH = LEN(BUFF)
      ELSE
C         set length as incoming buffer size
          LENGTH = INLEN
      ENDIF
C
C     set buffer please
      BUFF(1:LENGTH) = INPBUF(INPNUM)(1:LENGTH)
C     set outgoing buffer length as his current postion please 
      BUFLEN = INPEND(INPNUM) - 1
      ST =0 
      RETURN
999   CONTINUE
      ST = 1
      END

