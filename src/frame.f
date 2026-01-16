C
C     @(#)  412.1 date 6/11/92 frame.f 
C
C
C     Filename    : frame.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:50:21
C     Last change : 92/06/11 14:30:14
C
C     Copyright : Practical Technology International Limited  
C     File :- frame.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE FRAME_INIT(ST)
C     SUBROUTINE FRAME_DEFINE(FRMNUM,RECT,INVERT,OUTLIN,THICK,COL,
C     SUBROUTINE FRAME_DRAW(FRMNUM,ST)
C     SUBROUTINE FRMDRW(FRMNUM,DISP,ST)
C     SUBROUTINE FRAME_CURSOR_DEPENDANT(SCRPOS,CURS,ST)
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE FRAMEINIT(ST)
C     =========================
C1    VARYPE                 I4
C1    IOSTAT                 O
C
C2    Frame configuration
C  
      include    'include/frames.inc'
C
      INTEGER*4 ST,I
C
      DO 10 I=1,MAXFRM
C         initalise all
          FRMBOX(1,I) = 0
          FRMBOX(2,I) = 0
          FRMBOX(3,I) = 0
          FRMBOX(4,I) = 0
          FRMDIS(I) = .FALSE.
          FRMACT(I) = .FALSE.
          FRMCUR(I) = 0
10    CONTINUE
      END
C
      SUBROUTINE FRAME_DEFINE(FRMNUM,RECT,THICK,COL,BCOL,CURS,ST)
C     ===========================================================
C1    VARYPE                    I4    I4    I4   I4  I4   I4  I4  
C1    IOSTAT                    o     I     I    I   I    I   O
C
C2  
C2    Arguments:-
C2  
C2    FRMNUM          ->          Frame number 
C2    RECT            ->          Frawe size
C2    THICK           ->          Thickness
C2    COL             ->          Color
C2    CURS            ->          dependant cursor ID
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    -1      ->      No more frames
C2  
C2  

C  
      include    'include/frames.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 FRMNUM
      INTEGER*4 RECT(4)
      INTEGER*4 CURS
      INTEGER*4 ST
      INTEGER*4 COL
      INTEGER*4 BCOL
      INTEGER*4 THICK
      INTEGER*4 I
C
      ST = 0
      IF(FRMNUM.EQ.0)  THEN
          DO 10 I=1,MAXFRM
              IF(.NOT.FRMACT(I)) THEN
                  FRMNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF
C
      IF ( FRMNUM.EQ.0 ) THEN
          ST = -1
          RETURN 
      ENDIF

20    CONTINUE
C
      FRMBOX(1,FRMNUM) = RECT(1)
      FRMBOX(2,FRMNUM) = RECT(2)
      FRMBOX(3,FRMNUM) = RECT(3)
      FRMBOX(4,FRMNUM) = RECT(4)
C
      FRMCOL(1,FRMNUM) = COL
      FRMCOL(2,FRMNUM) = BCOL
      FRMTHK(FRMNUM) = THICK
      FRMDIS(FRMNUM) = .FALSE.
      FRMACT(FRMNUM) = .TRUE.
      FRMCUR(FRMNUM) = CURS
C
      END
C
      SUBROUTINE FRMDRW(FRMNUM,DISP,ST)
C     =================================
C1    VARYPE                 I4    I4
C1    IOSTAT                 I     O
C
C2    Draws the frame whatever color must be set outside
C
      include    'include/frames.inc'
      include    'include/daxcolor.inc'
      include    'include/dialog.inc'
C
      INTEGER*4 ST,INC,I
      INTEGER*4 FRMNUM
      INTEGER*2 RECT(4)
      INTEGER*2 XMIN,YMIN,XMAX,YMAX
      LOGICAL DISP
C
      IF(DISP.AND..NOT.FRMDIS(FRMNUM)) THEN
          FRMDIS(FRMNUM) = .TRUE.
      ELSEIF(.NOT.DISP.AND.FRMDIS(FRMNUM)) THEN
          FRMDIS(FRMNUM) = .FALSE.
      ELSE
          RETURN
      ENDIF
C
      FRMDIA(FRMNUM) = DIAGCR
C
C     define the frame drawing op
C     get thicknes incremet
      INC = SIGN(1,FRMTHK(FRMNUM) )
C     get box
      RECT(1) = FRMBOX(1,FRMNUM)
      RECT(2) = FRMBOX(2,FRMNUM)
      RECT(3) = FRMBOX(3,FRMNUM)
      RECT(4) = FRMBOX(4,FRMNUM)

      IF ( .NOT.DISP ) THEN
          CALL TOOLPEN(INT4(COLBAK))
          RECT(1) = FRMBOX(1,FRMNUM) - INC
          RECT(2) = FRMBOX(2,FRMNUM) - INC
          RECT(3) = FRMBOX(3,FRMNUM) + 2*INC
          RECT(4) = FRMBOX(4,FRMNUM) + 2*INC
          CALL TOOLS_RECTANGLE(RECT,ST)
      ELSE

          CALL TOOLPEN(FRMCOL(2,FRMNUM))
          CALL TOOLS_RECTANGLE(RECT,ST)
          CALL TOOLPEN(FRMCOL(1,FRMNUM))

          DO 10 I=1,FRMTHK(FRMNUM),INC
C             consider thickness here
              XMIN = RECT(1)
              YMIN = RECT(2)
              XMAX = XMIN + RECT(3)
              YMAX = YMIN + RECT(4)
              CALL TOOLS_BOX(XMIN,YMIN,XMAX,YMAX,ST)
              RECT(1) = RECT(1)-INC
              RECT(2) = RECT(2)-INC
              RECT(3) = RECT(3)+(2*INC)
              RECT(4) = RECT(4)+(2*INC)
10        CONTINUE
      ENDIF
      CALL TOOLPEN(INT4(COLFOR))
      END
C
C
      SUBROUTINE FRAME_CURSOR_DEPENDANT(SCRPOS,CURS,ST)
C     =================================================
C1    VARYPE                             I2     I4  I4
C1    IOSTAT                             I      O   O
C
C2    Cursor dependancy on the frame if the hit point resides.
C  
      include    'include/frames.inc'
      include    'include/daxcolor.inc'
      include    'include/cursor.inc'
      include    'include/dialog.inc'
C
      INTEGER*2 SCRPOS(2)
      INTEGER*4 ST,I,CNT,J,CURS
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,XP,YP
      LOGICAL OK,PNTBOX,INBOX
C
      EXTERNAL PNTBOX
C
      ST =0
      OK = .TRUE.
C     set position
      XP = SCRPOS(1)
      YP = SCRPOS(2)
      DO 10 I=1,MAXFRM
          IF(FRMDIS(I) ) THEN
C
C             if cursor dependant
              IF(FRMCUR(I).EQ.0) GOTO 10
C         
              XMIN = FRMBOX(1,I)
              YMIN = FRMBOX(2,I)
              XMAX = XMIN + FRMBOX(3,I)
              YMAX = YMIN + FRMBOX(4,I)
C
              INBOX = PNTBOX(XMIN,YMIN,XMAX,YMAX,XP,YP)
C
              IF(INBOX) THEN
C              
                  CURS = FRMCUR(I)
                  ST = 0
                  RETURN
             ENDIF

          ENDIF
C
10    CONTINUE
      ST = 1
      END

      SUBROUTINE FRAME_CLEAR(FRMNUM,ST)
C     ================================
C1    VARYPE                 I4    I4
C1    IOSTAT                 I     O
C
C2    clears the inside of the frame
C
      include    'include/frames.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 ST,I
      INTEGER*4 FRMNUM
      INTEGER*2 RECT(4)
      INTEGER*2 XMIN,YMIN,XMAX,YMAX
      LOGICAL UNOBSCURED
C
C     define the frame drawing op
C     get thicknes incremet
C     get box
      CALL ROPREP()
      RECT(1) = FRMBOX(1,FRMNUM)+1
      RECT(2) = FRMBOX(2,FRMNUM)+1
      RECT(3) = FRMBOX(3,FRMNUM)-1
      RECT(4) = FRMBOX(4,FRMNUM)-1
      CALL TOOLPEN(FRMCOL(2,FRMNUM))
      CALL TOOLS_RECTANGLE(RECT,ST)
      CALL TOOLPEN(INT4(COLFOR))

      END



