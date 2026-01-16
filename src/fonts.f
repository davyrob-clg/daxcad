C
C     @(#)  412.1 date 6/11/92 fonts.f 
C
C
C     Filename    : fonts.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:45
C     Last change : 92/06/11 14:30:12
C
C     Copyright : Practical Technology International Limited  
C     File :- fonts.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE FONT_INIT(ST)
C     SUBROUTINE SET_FONT(FONT,ST)
C     SUBROUTINE LOAD_FONT(FNAME,BUFLEN,FONT,ST)
C     SUBROUTINE GET_TEXT_INFO(TEXT,BUFLEN,TSIZE,TORG,ST)
C     SUBROUTINE INQ_FONT(FONT,ST)
C     SUBROUTINE GET_TEXT_SIZE(SIZE,ST)
C     SUBROUTINE SET_TEXT(CHR,ST)
C     SUBROUTINE FONT_COPY(FONT_OLD,FONT_NEW,ST)
C     SUBROUTINE FONT_SET_SPACING(FONT,SPACE,ST)
C     SUBROUTINE FONT_INQ_MAX(FONT,HEIGHT,ST)
C     SUBROUTINE FONT_INQ_WID(FONT,SIZE,ST)
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE FONTINIT(ST)
C     =======================
C1    VARYPE              I4
C1    IOSTAT              O
C
C2    Initalise all fonts
C
      include    'include/apfont.inc'
C
      INTEGER*4 ST,I
      ST =0 
      DO 10 I=1,MAXFNT
          APFACT(I) = .FALSE.
          APFNTS(I) = 0
10    CONTINUE
C     current font 
      CURRENT_FONT = 0
      END


      SUBROUTINE SET_FONT(FONT,ST)
C     ============================
C1    VARYPE              I4   I4
C1    IOSTAT              I    O
C
C2    Sets the DAXTOOLS font number
C
      INTEGER*4 ST,FONT
      INTEGER*2 FONTID
C
      include    'include/apfont.inc'
C
      IF(FONT.GT.100.OR.FONT.LT.1) THEN
          ST = 1
          GOTO 999
      ENDIF
C
      FONTID = APFNTS(FONT)
C
C     set the font now
      CALL GPR_$SET_TEXT_FONT(FONTID,ST)
      CURRENT_FONT = FONT
      RETURN
999   CONTINUE
C     error condition ST is set allready
      RETURN
C
      END
C
C
C
C
      SUBROUTINE LOAD_FONT(FNAME,BUFLEN,FONT,ST)
C     ==========================================
C1    VARYPE                C*(*)  I4    I4  I4
C1    IOSTAT                  I    I     I/O   O
C
C2    Loads an APOLLO font file with the identifier supplied
C2  
C2  
C
      include    'include/apfont.inc'
C
      CHARACTER*(*) FNAME
      INTEGER*4 FONT,BUFLEN,ST,I
      INTEGER*2 FONTID         
      INTEGER*2 BUF2         
C
C     get a font number if none supplied
	  
      IF(FONT.LT.0.OR.FONT.GT.MAXFNT) THEN
          ST = 2
          GOTO 999
      ENDIF
      IF(FONT.EQ.0)  THEN
          DO 10 I=1,MAXFNT
              IF(.NOT.APFACT(I)) THEN
                  FONT = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF
C
20    CONTINUE
C     check for name
      IF(BUFLEN.LE.0) THEN
          ST = 1
          GOTO 999
      ENDIF
C
C     load up the font
      BUF2 = buflen
      CALL GPR_$LOAD_FONT_FILE(FNAME,
     +                         BUF2,
     +                         FONTID,
     +                         ST)

      IF(ST.EQ.0) THEN
          APFACT(FONT) = .TRUE.
          APFNTS(FONT) = FONTID
      ELSE
          GOTO 999
      ENDIF
C     all ok go home with new font
      ST=0
      RETURN
999   CONTINUE
C     error condition
      END
C
      SUBROUTINE GET_TEXT_INFO(TEXT,BUFLEN,TSIZE,TORG,ST)
C     ===================================================
C1    VARYPE                   C*(*)   I4   I42   I42 I4
C1    IOSTAT                     I     I     O     O  O 
C
C2    Gets text extents and origin Usefull for formatting tools etc
C2    Direction is RIGHT User will have to used other if using other directions
C
      include    'include/gpr.ins.inc'
      include    'include/apfont.inc'
C
      INTEGER*2 XYEND(2),FONTID,SIZE(2)
      INTEGER*4 ST,TSIZE(2),TORG(2),BUFLEN
      CHARACTER TEXT*(*)
C
      IF(BUFLEN.LE.0) THEN
          GOTO 999
      ENDIF
C
C     get extents
      CALL GPR_$INQ_TEXT_EXTENT(TEXT,INT2(BUFLEN),SIZE,ST)
C
      IF(ST.EQ.0) THEN
          TSIZE(1) = SIZE(1)
          TSIZE(2) = SIZE(2)
      ELSE
          GOTO 999
      ENDIF
C     get offsets of text
      CALL GPR_$INQ_TEXT_OFFSET(TEXT,INT2(BUFLEN),SIZE,XYEND,ST)

      IF(ST.EQ.0) THEN
C
          TORG(1) = SIZE(1)
          TORG(2) = SIZE(2)
      ELSE
          GOTO 999
C    
      ENDIF
      ST =0
      RETURN
999   CONTINUE

C
      END
C
C
C
      SUBROUTINE INQ_FONT(FONT,ST)
C     ============================
C1    VARYPE               I4  I4
C1    IOSTAT               O   O
C
C2    Gets current text font loaded
C
C
      include    'include/gpr.ins.inc'
      include    'include/apfont.inc'
C

      INTEGER*4 I,FONT,ST
      INTEGER*2 FONTID,DIRECT
C
      CALL GPR_$INQ_TEXT(FONTID,DIRECT,ST)
      IF(ST.NE.0) THEN
          GOTO 999
      ENDIF
C
      DO 10 I=1,MAXFNT
          IF(APFNTS(I).EQ.FONTID) THEN
              FONT = I
              ST = 0
              RETURN
          ENDIF
 10   CONTINUE
      FONT = 0
      ST = 1
      RETURN
999   CONTINUE
      END
C
      SUBROUTINE GET_TEXT_SIZE(SIZE,ST)
C     =================================
C1    VARYPE                   I4  I4
C1    IOSTAT                   O   O
C
C2    Gets the maximum text heigth for the currently loaded font
C
      include    'include/apfont.inc'

      INTEGER*4 I,ST,SIZE,A
      INTEGER*4 BUFLEN
      INTEGER*4 NLEN
      INTEGER*4 TORG(2)
      INTEGER*4 TSIZE(2)
      CHARACTER*120 CHR
C
C     inialise stirng
      CHR = ' ' 
C     check range here
      DO 50 I=32,128
          A = I-31
          CHR(A:A) = CHAR(I)
50    CONTINUE
      BUFLEN = 105
      CALL GET_TEXT_INFO(CHR,BUFLEN,TSIZE,TORG,ST)
C     set y size of text
      SIZE = TSIZE(2)


99    CONTINUE
      END
C
      SUBROUTINE SET_TEXT(CHR,ST)
C     ==========================
C1    VARYPE              C   I4
C1    IOSTAT              I   O
C
C2    Routine sets global text charactristics be fore drawing etc
C
      include    'include/gpr.ins.inc'
      include    'include/edit.inc'
      include    'include/apfont.inc'
C
      INTEGER*2 DIRECT
      INTEGER*2 SIZE(2),XYEND(2),FONTID,NLEN2,NL,INTER
      INTEGER*4 ST
      CHARACTER CHR*(*)
      CHARACTER*20 ARG1
C
      EXTERNAL NLEN2
C
      NL = NLEN2(CHR)

      IF(NL.EQ.0) NL = 1
      IF(CHR(NL:NL).EQ.CHAR(0)) NL=NL-1
      IF(NL.EQ.0) RETURN
C
      CALL GPR_$INQ_TEXT(FONTID,DIRECT,ST)
C     get spacing of font
      CALL GPR_$INQ_HORIZONTAL_SPACING(FONTID,TXSPACE,ST)
C      TXSPACE = TXSPACE+1
C
      CALL GPR_$INQ_TEXT_EXTENT(CHR,NL,SIZE,ST)
C
      IF(ST.EQ.0) THEN
          TEXTW = SIZE(1)
          TEXTH = SIZE(2)
      ENDIF
      CALL GPR_$INQ_TEXT_OFFSET(CHR,NL,SIZE,XYEND,ST)

      IF(ST.EQ.0) THEN
C
          TORGX = SIZE(1)
          TORGY = SIZE(2)
C    
      ENDIF
      END
C
      SUBROUTINE FONT_COPY(FONT_OLD,FONT_NEW,ST)
C     ==========================================
C1    VARYPE                   I4      I4    I4
C1    IOSTAT                   I       I     O
C
C2    Copy a font. The copied font can be modified
C
      include    'include/apfont.inc'
C
      INTEGER*4 FONT_OLD
      INTEGER*4 FONT_NEW
      INTEGER*4 FONT
      INTEGER*4 ST
      INTEGER*4 I
      INTEGER*2 APFONTID
      INTEGER*2 NEW_APFONTID
C
      IF(FONT_OLD.LT.0.OR.FONT_OLD.GT.MAXFNT) THEN
          ST = 2
          GOTO 999
      ENDIF
      DO 10 I=1,MAXFNT
          IF(.NOT.APFACT(I)) THEN
              FONT = I
              GOTO 20
          ENDIF
10    CONTINUE
C     ran out of fonts
      ST = 3
      GOTO 999
C
20    CONTINUE
C
C     get apollo font id of old font
      APFONTID = APFNTS(FONT_OLD)
C
C     replicate the font
C
      CALL GPR_$REPLICATE_FONT(APFONTID,NEW_APFONTID,ST)
      IF(ST.GT.0) THEN
C         system error
          ST = 1
          GOTO 999
      ENDIF
C
      APFNTS(FONT) = NEW_APFONTID
      FONT_NEW = FONT
      ST = 0
      RETURN
C     error here
999   CONTINUE
      END
C
      SUBROUTINE FONT_SET_SPACING(FONT,SPACE,ST)
C     ===========================================
C1    VARYPE                       I4   I4   I4
C1    IOSTAT                       I    I    O
C
C2    Sets the spacing for the specified font
C  
      include    'include/apfont.inc'
C
      INTEGER*4 ST
      INTEGER*4 FONT
      INTEGER*4 SPACE
      INTEGER*2 FONTID                        
      INTEGER*2 INT2
C
      IF(FONT.LT.0.OR.FONT.GT.MAXFNT) THEN
C         font out of range
          ST = 2
          GOTO 999
      ENDIF
C
      IF(SPACE.LT.0.OR.SPACE.GT.127) THEN
C         space is too big
          ST = 3
          GOTO 999
      ENDIF
C
      FONTID = APFNTS(FONT)
C
      CALL GPR_$SET_HORIZONTAL_SPACING(FONTID,INT2(SPACE),ST)
      IF(ST.GT.0) THEN
          ST = 1
          GOTO 999
      ENDIF
      ST = 0 
      RETURN
999   CONTINUE
C
      END
C
      SUBROUTINE FONT_INQ_MAX(FONT,HEIGHT,ST)
C     =======================================
C1    VARYPE                   I4    I4   I4
C1    IOSTAT                   I     O    O
C
C2    Returns the maximum font size for this font
C  
      include    'include/apfont.inc'
C
      INTEGER*4 FONT
      INTEGER*4 HEIGHT
      INTEGER*4 ST
      INTEGER*4 FONTID
C
C     Initalise the network
C
      CALL SET_FONT(FONT,ST)
      IF(ST.EQ.0) THEN
          CALL GET_TEXT_SIZE(HEIGHT,ST)
          ST = 0
      ENDIF
      END


      SUBROUTINE FONT_INQ_WID(FONT,SIZE,ST)
C     =====================================
C1    VARYPE                   I4  I4  I4
C1    IOSTAT                   I   O   O
C
C2    Gets the maximum text width for 1 character loaded
C
      include    'include/apfont.inc'

      INTEGER*4 FONT
      INTEGER*4 I,ST,SIZE,A
      INTEGER*4 BUFLEN
      INTEGER*4 NLEN
      INTEGER*4 TORG(2)
      INTEGER*4 TSIZE(2)
      CHARACTER*120 CHR
C
      CALL SET_FONT(FONT,ST)
      IF (ST.GT.0) THEN
          ST = 1
          GOTO 999
      ENDIF
      BUFLEN = 1
      SIZE = 0
C     check range here
      DO 50 I=32,128
          A = I-31
          CHR(A:A) = CHAR(I)
          CALL GET_TEXT_INFO(CHR,BUFLEN,TSIZE,TORG,ST)
          IF (TSIZE(1).GT.SIZE) THEN
              SIZE = TSIZE(1)
          ENDIF
50    CONTINUE
C
999   CONTINUE
C
      END






