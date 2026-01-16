C
C     @(#)  412.1 date 6/11/92 label.f 
C
C
C     Filename    : label.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:24
C     Last change : 92/06/11 14:33:18
C
C     Copyright : Practical Technology International Limited  
C     File :- label.f
C
C     DAXCAD FORTRAN 77 Source file
C
C
      SUBROUTINE LABELINIT(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                I
C
C2    This routine will inialise all label code in daxtools
C
      include    'include/edit.inc'
      include    'include/label.inc'
C
      INTEGER*4 ST,I
C
      DO 10 I=1,LABMAX
C
          LABACT(I) = .FALSE.
          LABTXT(I) = ' '
          LABFNT(I) = 0 
C
 10   CONTINUE
C
      END
C
      SUBROUTINE LABEL_MAKE(X,Y,JUST,FONT,COL,BCOL,TEXT,LENGTH,
     +                      FILE,INVERT,LABNUM,ST)
C     ===================================================================
C1    VARYPE               I4 I4  I4    I4  I4    C      I4    L   L   I4  I4 
C1    IOSTAT               I  I   I     I   I     I      I     I    I   I  O
C
C2    Sets up label for displaying on the screen
C
      include    'include/edit.inc'
      include    'include/label.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 X,Y,JUST,LENGTH,LABNUM,FONT
      INTEGER*2 XP,YP
      INTEGER*4 FONTID
      INTEGER*2 BOXH,BOXW,CFID
      INTEGER*4 ST,I,COL,BCOL
      LOGICAL FILE,INVERT
      CHARACTER*(*) TEXT
C
      IF(LABNUM.EQ.0) THEN
          DO 10 I=1,LABMAX
             IF(.NOT.LABACT(I)) THEN
                  LABNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
20        CONTINUE
      ENDIF
C
C     save current font
      IF(FONT.LE.0.OR.FONT.GT.100) THEN
C         set as current loaded font
          CALL INQ_FONT(FONT,ST)
      ENDIF
C
      LABFNT(LABNUM) = FONT
      LABCOL(1,LABNUM) = COL
      LABCOL(2,LABNUM) = BCOL
C
C     save text
      IF(LENGTH.GT.0) LABTXT(LABNUM) = TEXT(1:LENGTH)
C     add on a null if not a filename
      IF(.NOT.FILE) LABTXT(LABNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
C
C     get current font
      FONTID = LABFNT(LABNUM)
      CALL SET_FONT(FONTID,ST)
C     get details for this text
      CALL SET_TEXT(LABTXT(LABNUM),ST)
C
      BOXH = TEXTH
      BOXW = TEXTW
C     get apollo justification
      XP = X + TORGX
      YP = Y + TORGY
C
      IF(JUST.EQ.1) THEN
          XP = XP - BOXW/2
      ELSEIF(JUST.EQ.2) THEN
          XP = XP - BOXW
      ELSEIF(JUST.EQ.3) THEN
          YP = YP - BOXH/2
      ELSEIF(JUST.EQ.4) THEN
          XP = XP - BOXW/2
          YP = YP - BOXH/2
      ELSEIF(JUST.EQ.5) THEN
          XP = XP - BOXW
          YP = YP - BOXH/2
      ELSEIF(JUST.EQ.6) THEN
          YP = YP - BOXH
      ELSEIF(JUST.EQ.7) THEN
          YP = YP - BOXH
          XP = XP - BOXW/2
      ELSEIF(JUST.EQ.8) THEN
          YP = YP - BOXH
          XP = XP - BOXW
      ENDIF
C
C     store plottable point
      LABPOS(1,LABNUM) = XP
      LABPOS(2,LABNUM) = YP
C     set whether file is used to get text
      LABFIL(LABNUM) = FILE
C
      LABACT(LABNUM) = .TRUE.
C     set invert status
      LABINV(LABNUM) = INVERT
      LABDIS(LABNUM) = .FALSE.
C
      END
C
C
      SUBROUTINE LABEL_DRAW(LABNUM,COL,BCOL,ST)
C     =========================================
C1    VARYPE                  I4    I4  I4  I4
C1    IOSTAT                  I     I   I    O
C
C2    This routine will draw the label on the screen
C
      include    'include/edit.inc'
      include    'include/label.inc'
      include    'include/apfont.inc'
      include    'include/dialog.inc'
C
      INTEGER*4 LABNUM
      INTEGER*2 X,Y,NL,NLEN2
      INTEGER*4 FONTID
      INTEGER*4 RECT(4)
      INTEGER*4 ST,UNITS,COL,BCOL
      LOGICAL OK,EX
      CHARACTER*200 FILNAM
C
      EXTERNAL NLEN2
C
      LABDIA(LABNUM) = DIAGCR
C
      X = LABPOS(1,LABNUM)
      Y = LABPOS(2,LABNUM)
C
      FONTID = LABFNT(LABNUM)
      CALL SET_FONT(FONTID,ST)
C
      CALL ROPREP()
      IF(LABFIL(LABNUM) ) THEN
C         set fielname first
          FILNAM = LABTXT(LABNUM)
C         use file to get info from. We need to open it
          INQUIRE(FILE=FILNAM,EXIST=EX)
C
          IF(.NOT.EX) THEN
C             file does not exist
              ST = 1
              GOTO 999
          ENDIF
C         get a unit number
          CALL FINDU1(UNITS,OK)
          IF(.NOT.OK) THEN
              ST = 2
              GOTO 999
          ENDIF
C         open the file please
          OPEN( UNIT=UNITS,
     +          FILE=FILNAM,
     +          ACCESS='SEQUENTIAL',
     +          STATUS='OLD',
     +          IOSTAT=ST,
     +          ERR=999)
100       CONTINUE
C         loop round and write the text onto the screen
          READ(UNIT=UNITS,FMT='(A)',END=200,ERR=999) FILNAM
          NL = NLEN2(FILNAM)
C         first draw in the background window
          IF(LABINV(LABNUM) ) THEN
              CALL SET_TEXT(FILNAM,ST)
              RECT(1) = LABPOS(1,LABNUM)-1
              RECT(2) = LABPOS(2,LABNUM)-TORGY-1
              RECT(3) = TEXTW+2
              RECT(4) = TEXTH+2
              CALL TOOLPEN(COL)
              CALL TOOLS_RECTANGLE(RECT,ST)
              CALL TOOLPEN(BCOL)
          ENDIF
          CALL TOOLPEN_TEXT(COL,BCOL,.FALSE.,ST)
          CALL TOOLS_TEXT(X,Y,FILNAM,NL,ST)
          Y = Y+TEXTH
          GOTO 100
200       CONTINUE
C         close the file
          CLOSE(UNITS)
      ELSE
C         just normal font
          NL = NLEN2(LABTXT(LABNUM))
          IF(LABTXT(LABNUM)(NL:NL).EQ.CHAR(0)) NL = NL-1
C
          IF(LABINV(LABNUM) ) THEN
              CALL SET_TEXT(LABTXT(LABNUM)(1:NL),ST)
              RECT(1) = LABPOS(1,LABNUM)-1
              RECT(2) = LABPOS(2,LABNUM)-TORGY-1
              RECT(3) = TEXTW+2
              RECT(4) = TEXTH+2
              CALL TOOLPEN(COL)
              CALL TOOLS_RECTANGLE(RECT,ST)
              CALL TOOLPEN(BCOL)
          ENDIF
          CALL TOOLPEN_TEXT(COL,BCOL,.FALSE.,ST)
          CALL TOOLS_TEXT(X,Y,LABTXT(LABNUM),NL,ST)
      ENDIF
C
      RETURN
999   CONTINUE
      ST = 1
      END
C
      SUBROUTINE LABEL_GET_TEXT(LABNUM,BUFF,INLEN,BUFLEN,ST)
C     ======================================================
C1    VARYPE                      I4    C    I4    I4   I4
C1    IOSTAT                       I    O    I     O     O
C
C2    The routine will return the text associated eith the number
C
      include    'include/label.inc'
C
      CHARACTER*200 BUFF
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      INTEGER*4 ST,LABNUM
      INTEGER*4 BUFLEN
      INTEGER*4 INLEN
      INTEGER*4 USELEN
C
      EXTERNAL NLEN
C
      BUFLEN = 0
      ST = 0
C
      IF(LABNUM.EQ.0) THEN
          ST  = 1
          GOTO 999
      ENDIF
      IF(INLEN.LE.0) THEN
C         duff length
          ST = 4
          GOTO 999
      ENDIF

      IF(LABACT(LABNUM)) THEN 
C         Get the text and strip off the null

          LENGTH = NLEN( LABTXT(LABNUM) )
          IF(LENGTH.LE.1) THEN
C             empty label
              BUFLEN = 0
              ST = 3
              GOTO 999
          ENDIF
C         only modify if not filename
          IF(.NOT.LABFIL(LABNUM) )THEN
C             check lengths for nulls
              IF(INLEN.GE.LENGTH )  THEN
C                 assign the buffer cos it has more bytes than needed
                  BUFF(1:LENGTH) = LABTXT(LABNUM)(1:LENGTH-1)
                  BUFLEN = LENGTH - 1
              ELSE
C                 not enough bytes need to chop
                  BUFF(1:INLEN) = LABTXT(LABNUM)(1:INLEN)
                  BUFLEN = LENGTH 
              ENDIF
          ELSE
              BUFLEN = LENGTH
          ENDIF
      ELSE
          ST = 2
          GOTO 999
      ENDIF
      ST = 0
C
999   CONTINUE
      END


      SUBROUTINE LABEL_MODIFY_TEXT(LABNUM,BUFF,BUFLEN,ST)
C     ===================================================
C1    VARYPE                         I4    C     I4   I4
C1    IOSTAT                         I     I     I    O
C
C2    The routine will modify nthe text associated eith the number
C
      include    'include/label.inc'
C
      CHARACTER*(*) BUFF
      INTEGER*2 NLEN2,LENGTH
      INTEGER*4 LABNUM
      INTEGER*4 ST,BUFLEN
C
      EXTERNAL NLEN2
C
      ST = 0
      IF(LABNUM.EQ.0) GOTO 999
      IF(BUFLEN.LE.0) GOTO 999
      IF(LABACT(LABNUM)) THEN 
C         Get the text and strip off the null
          LABTXT(LABNUM) = BUFF(1:BUFLEN)
          LENGTH = BUFLEN
          IF(LENGTH.EQ.0) GOTO 999
C         append the null please
          IF(.NOT.LABFIL(LABNUM))THEN
             LABTXT(LABNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
          ENDIF
      ENDIF
      RETURN
C
999   ST = 1
      END
C
      SUBROUTINE LABDRW(LABNUM,DISP,ST)
C     =================================
C1    VARYPE              I4     L  I4
C1    IOSTAT              I      I  I
C
C2    User routine to draw or erase a label
C 
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/label.inc'
C
      INTEGER*4 LABNUM
      INTEGER*4 ST,COL,BCOL
      LOGICAL DISP
C
      IF(LABNUM.EQ.0) RETURN
      IF(DISP.AND..NOT.LABDIS(LABNUM)) THEN
          COL = LABCOL(1,LABNUM)
          BCOL = LABCOL(2,LABNUM)
          LABDIS(LABNUM) = .TRUE.
      ELSEIF(.NOT.DISP.AND.LABDIS(LABNUM)) THEN
          LABDIS(LABNUM) = .FALSE.
          COL = LABCOL(2,LABNUM)
          BCOL = LABCOL(2,LABNUM)
      ELSE
          RETURN
      ENDIF
C     draw or erase the text
      CALL LABEL_DRAW(LABNUM,COL,BCOL,ST)
      END
C
      SUBROUTINE LABEL_MODIFY(LABNUM,COL,BCOL,FONT,ST)
C     ====================================================
C1    VARYPE                    I4   I4   I4   I4  I4
C1    IOSTAT                    I    I    I    I   O
C
C2    modifys current data in label in LABNUM
C
      include    'include/edit.inc'
      include    'include/daxcolor.inc'
      include    'include/label.inc'
      include    'include/apfont.inc'
C
      INTEGER*4 ST,COL,BCOL
      INTEGER*2 LABNUM
      INTEGER*4 FONT
      INTEGER*4 X,Y
C
      ST = 0
      IF(LABNUM.LT.1.OR.LABNUM.GT.LABMAX) THEN
          ST = 1
          RETURN
      ENDIF
C
      LABCOL(1,LABNUM) = COL
      LABCOL(2,LABNUM) = BCOL
      IF(FONT.GT.0.AND.FONT.LT.100) THEN
C         modify only if valid font number
          LABFNT(LABNUM) = FONT
      ENDIF
C
      END
C
      SUBROUTINE LABMOD(LABNUM,X,Y,BUFF,BUFLEN,ST)
C     ============================================
C1    VARYPE               I4    C     I4   I4  I4 
C1    IOSTAT               I     I     I    I   O
C
C2    The routine will modify nthe text associated eith the number
C
      include    'include/label.inc'
C
      CHARACTER*201 BUFF
      INTEGER*2 NLEN2,LENGTH
      INTEGER*4 LABNUM
      INTEGER*4 X,Y
      INTEGER*4 BLEN
      INTEGER*4 DIANUM
      INTEGER*4 ST,BUFLEN,LL
      LOGICAL REDRAW
C
      EXTERNAL NLEN2
C
      IF ( BUFLEN.GT.LEN(BUFF)-1 ) THEN
          BLEN = LEN(BUFF)-1
      ELSE
          BLEN = BUFLEN
      ENDIF
C
      ST = 0
      IF(LABNUM.EQ.0) GOTO 999
      REDRAW = LABDIS(LABNUM)
C     modify new position
      LABPOS(1,LABNUM) = X
      LABPOS(2,LABNUM) = Y
      IF(LABACT(LABNUM)) THEN 

          IF(REDRAW) CALL LABDRW(LABNUM,.FALSE.,ST)
C         Get the text and strip off the null
          IF(BUFLEN.GT.0) THEN
              LABTXT(LABNUM) = ' '
              LABTXT(LABNUM) = BUFF(1:BLEN)
              LENGTH = BLEN
              IF(LENGTH.EQ.0) GOTO 999
C             append the null please
              IF(.NOT.LABFIL(LABNUM))THEN
                  LL = LEN(LABTXT(LABNUM)) 
                  IF(LL.LE.LENGTH) LENGTH = LENGTH -1
C                 clear it first
                  LABTXT(LABNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
              ENDIF
          ELSE
C             he wants nout
              LABTXT(LABNUM) = ' '
              LABTXT(LABNUM)(1:1) = CHAR(0)

          ENDIF
          IF(REDRAW) CALL LABDRW(LABNUM,.TRUE.,ST)
      ENDIF
      RETURN
C
999   ST = 1
      END

      SUBROUTINE LABMOD_AP(LABNUM,BUFF,BUFLEN,ST)
C     ========================================
C1    VARYPE               I4    C     I4   I4
C1    IOSTAT               I     I     I    O
C
C2    The routine will modify nthe text associated eith the number
C
      include    'include/label.inc'
C
      CHARACTER*201 BUFF
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      INTEGER*4 LABNUM
      INTEGER*4 POS
      INTEGER*4 BLEN
      INTEGER*4 ST,BUFLEN,LL
      LOGICAL REDRAW
C
      EXTERNAL NLEN
C
      ST = 0
C
      IF ( BUFLEN.GT.LEN(BUFF)-1 ) THEN
          BLEN = LEN(BUFF)-1
      ELSE
          BLEN = BUFLEN
      ENDIF
C
      IF(LABNUM.EQ.0) THEN
          GOTO 999
      ENDIF
      REDRAW = LABDIS(LABNUM)
      IF(LABACT(LABNUM)) THEN 

          IF(REDRAW) CALL LABDRW(LABNUM,.FALSE.,ST)
C         Get the text and strip off the null
          IF(BLEN.GT.0) THEN
              IF(.NOT.LABFIL(LABNUM)) THEN
                  POS = INDEX(LABTXT(LABNUM),':')
                  IF(POS.GT.0) THEN
                      LABTXT(LABNUM)(POS+2:) = BUFF(1:BLEN)
                      LENGTH = NLEN(LABTXT(LABNUM))
                      LL = LEN(LABTXT(LABNUM))
                      IF(LL.LE.LENGTH) THEN
                          LENGTH = LENGTH -1
                      ENDIF
                      LABTXT(LABNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
                  ELSE 
                      GOTO 999
                  ENDIF
              ENDIF
              IF(LENGTH.EQ.0) GOTO 999
C             append the null please
              IF(.NOT.LABFIL(LABNUM))THEN
              ENDIF
          ENDIF
          IF(REDRAW) CALL LABDRW(LABNUM,.TRUE.,ST)
      ENDIF
      RETURN
C
999   ST = 1
      END


      SUBROUTINE LABEL_SET_ATTR(LABNUM,TOOL_ATTRIBUTES,TOOL_TEXT,ST)
C     ===============================================================
C1    VARYPE                       I4       STRUCT         STRUCT I4
C1    IOSTAT                       I          I              I    O
C
C2    This routine sets attributes of a button
C2  
C2  
      include    'include/label.inc'
      include    'include/attributes.inc'
C
      INTEGER*4 LABNUM
      INTEGER*4 LENGTH
      INTEGER*4 ST
      LOGICAL REDRAW
C
      IF(LABNUM.LE.0.OR.LABNUM.GT.LABMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.LABACT(LABNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
      IF(TOOL_ATTRIBUTES(1).NE.1) THEN
C         attributres array not been set
          ST = 3
          GOTO 999
      ENDIF
C
      REDRAW = LABDIS(LABNUM)
C
      IF(REDRAW) THEN
          CALL LABDRW(LABNUM,.FALSE.,ST)
      ENDIF
C
C     x and y postion
      LABPOS(1,LABNUM) = TOOL_ATTRIBUTES(2)
      LABPOS(2,LABNUM) = TOOL_ATTRIBUTES(3)
C     font
      LABFNT(LABNUM) =   TOOL_ATTRIBUTES(6)
C     color
      LABCOL(1,LABNUM) = TOOL_ATTRIBUTES(7)
      LABCOL(2,LABNUM) = TOOL_ATTRIBUTES(8)
C     type
      LABFIL(LABNUM) = TOOL_ATTRIBUTES(11).GT.0
      LABINV(LABNUM) = TOOL_ATTRIBUTES(12).GT.0
C     now dependant coding
C
C     text now
      LENGTH = TOOL_ATTRIBUTES(10)
      IF(LENGTH.GT.0) THEN
          LENGTH = MIN(LENGTH,LEN(LABTXT(LABNUM))-1)
          LABTXT(LABNUM)(1:LENGTH) = TOOL_TEXT(1:LENGTH)
C         put in a null
          LABTXT(LABNUM)(LENGTH+1:LENGTH+1) = CHAR(0)
      ENDIF

      IF(REDRAW) THEN
          CALL LABDRW(LABNUM,.TRUE.,ST)
      ENDIF
      ST = 0
999   CONTINUE
      END
C
      SUBROUTINE LABEL_GET_ATTR(LABNUM,TOOL_ATTRIBUTES,TOOL_TEXT,ST)
C     ===============================================================
C1    VARYPE                       I4       STRUCT         STRUCT I4
C1    IOSTAT                       I          O              O    O
C
C2    This routine Gets attributes of a label
C2  
C2  
      include    'include/label.inc'
      include    'include/attributes.inc'
C
      INTEGER*4 LABNUM
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      INTEGER*4 ST
      LOGICAL REDRAW
C
      EXTERNAL NLEN
C
      IF(LABNUM.LE.0.OR.LABNUM.GT.LABMAX) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      IF(.NOT.LABACT(LABNUM)) THEN
C         not active
          ST = 2
          GOTO 999
      ENDIF
C
C     set header 
      TOOL_ATTRIBUTES(1) = 1
C
C     save all attributes
      TOOL_ATTRIBUTES(2)= LABPOS(1,LABNUM) 
      TOOL_ATTRIBUTES(3)= LABPOS(2,LABNUM) 
      TOOL_ATTRIBUTES(6)= LABFNT(LABNUM) 
      TOOL_ATTRIBUTES(7)= LABCOL(1,LABNUM) 
      TOOL_ATTRIBUTES(8)= LABCOL(2,LABNUM) 
      IF(LABFIL(LABNUM)) THEN
          TOOL_ATTRIBUTES(11)= 1
      ELSE
          TOOL_ATTRIBUTES(11)= 0
      ENDIF
      IF(LABINV(LABNUM)) THEN
          TOOL_ATTRIBUTES(12)= 1
      ELSE
          TOOL_ATTRIBUTES(12)= 0
      ENDIF
C
C     text now
      LENGTH = NLEN(LABTXT(LABNUM))-1
      IF(LENGTH.GT.1) THEN
C         set text please
          TOOL_ATTRIBUTES(10) = LENGTH
          TOOL_TEXT(1:LENGTH) = LABTXT(LABNUM)(1:LENGTH)
      ELSE
C         nothing in theis button
          LENGTH = 0
      ENDIF
      ST = 0
999   CONTINUE
      END




