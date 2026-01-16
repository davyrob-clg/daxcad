C
C     @(#)  412.1 date 6/11/92 colors.f 
C
C
C     Filename    : colors.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:31
C     Last change : 92/06/11 14:25:27
C
C     Copyright : Practical Technology International Limited  
C     File :- colors.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE MOD_COLORMAP(COLNUM,COL,R,G,B,ST)
C     SUBROUTINE INQ_COLORMAP(COLNUM,COL,R,G,B,ST)
C     SUBROUTINE LOCK_COLORMAP(ST)
C     SUBROUTINE UNLOCK_COLORMAP(ST)
C     SUBROUTINE TOOLPEN(COLOR4)
C     SUBROUTINE TOOLPEN_TEXT(FORE,BACK,SAME,ST)
C
C     |-----------------------------------------------------------------|
C

C     
      SUBROUTINE MOD_COLORMAP(COLNUM,COL,R,G,B,ST)
C     ============================================
C1    VARYPE                    I4   I4  I4I4I4I4
C1    IOSTAT                     I   I   I I I O
C
C2    Modifies a colormap tabele with the vector given
C
      INTEGER*4 COLNUM
      INTEGER*4 COL
      INTEGER*4 R,G,B
      INTEGER*4 ST
      INTEGER*4 PLANES
C
      include  'include/apollo.inc'
      include  'include/color.inc'
C
      IF(HIPLAN.EQ.0) THEN
          ST = 0
          RETURN
      ENDIF
      IF(COLNUM.LT.0.OR.COLNUM.GT.MAXTAB) THEN
          ST = 1
          GOTO 999
      ENDIF
C
      IF(COLNUM.GT.0) THEN
C         color table is not active
          IF(.NOT.COLOR_ACTIVE(COLNUM) ) THEN
              GOTO 999
          ENDIF
      ENDIF
C     get type
      PLANES = 2**(HIPLAN+1)

      IF(COL.GT.PLANES) THEN
C         color number is out of reange for this machine
          ST = 2
      ENDIF

      COLOR_TABLE(1,COL,COLNUM) = R
      COLOR_TABLE(2,COL,COLNUM) = G
      COLOR_TABLE(3,COL,COLNUM) = B
C
      ST = 0
999   CONTINUE
C
      END


      SUBROUTINE INQ_COLORMAP(COLNUM,COL,R,G,B,ST)
C     ============================================
C1    VARYPE                    I4   I4  I4I4I4I4
C1    IOSTAT                     I   I   O O O O
C
C2    Returns the current loaded colormap value for the table
C
      INTEGER*4 COLNUM
      INTEGER*4 COL
      INTEGER*4 R,G,B
      INTEGER*4 ST
      INTEGER*4 PLANES
C
      include  'include/color.inc'
      include  'include/apollo.inc'
C
      IF(HIPLAN.EQ.0) THEN
          ST = 0
          RETURN
      ENDIF
      IF(COLNUM.LT.0.OR.COLNUM.GT.MAXTAB) THEN
          ST = 1
          GOTO 999
      ENDIF

      IF(COLNUM.GT.0) THEN
C         color table is not active
          IF(.NOT.COLOR_ACTIVE(COLNUM) ) THEN
              GOTO 999
          ENDIF
      ENDIF


      PLANES = 2**(HIPLAN+1)

      IF(COL.GT.PLANES) THEN
C         color number is out of reange for this machine
          ST = 2
      ENDIF

      R = COLOR_TABLE(1,COL,COLNUM) 
      G = COLOR_TABLE(2,COL,COLNUM) 
      B = COLOR_TABLE(3,COL,COLNUM) 
C
      ST = 0
C
999   CONTINUE
      END


      SUBROUTINE LOCK_COLORMAP(ST)
C     ============================
C1    VARYPE                   I4
C1    IOSTAT                   O
C
C2    Locks current color map into program
C2    You can only change by calling GPR direct
C  
      include  'include/color.inc'
C
      INTEGER*4 ST
C
      COLOR_LOCKED = .TRUE.
C
      END
C
      SUBROUTINE UNLOCK_COLORMAP(ST)
C     ==============================
C1    VARYPE                     I4
C1    IOSTAT                     O
C
C2    UNLocks current color map into program
C  
      include  'include/color.inc'
C
      INTEGER*4 ST
C
      COLOR_LOCKED = .FALSE.
C
      END
C
      SUBROUTINE TOOLPEN(COLOR)
C     =========================
C1                         I4
C1                         I
C2    SubroutineTOOL PEN changes the data level to the passed
C2    colour number COLOR,range limit of 16 currently.
C2    also for mono machines and fill values of 0 
C2    we make 
C2         0 = Black
C2        15 = White
C2        256 = Shadow of some description
C
      include  'include/gpr.ins.inc'
C
      include  'include/daxcolor.inc'
      include  'include/shadow.inc'
      include  'include/apollo.inc'
C
      INTEGER*4 ST
      INTEGER*4 COLS
      INTEGER*4 COLOR
      INTEGER*4 COLOR4
      INTEGER*4 XOR_MASK
      LOGICAL COLOUR
C
C     get conditions
      IF(COLOR.EQ.256) THEN
	  COLOR4 = 7
      ELSE
          COLOR4 = COLOR
      ENDIF
      COLOUR = HIPLAN.GT.0
C     save current drawing color regardless
      TOOLPEN_COLOR = COLOR4
      IF(COLOUR) THEN
C         remap for X etc
          COLS = COLOR_MAP(COLOR4)
      ELSE
C         make sure color is only drawn
          IF(COLOR4.GT.0) THEN
             COLS = COLFOR
          ELSE
             COLS = COLBAK
          ENDIF
      ENDIF
C     set  new XOR colour if required
      IF(RASTER_OP_FLAG .EQ. XOR_FLAG) THEN
C        set XOR color if not already set to XOR
         CALL DAX_XOR(COLOR_MAP(0), COLS, 
     +                     XOR_MASK)
         COLS = XOR_MASK  
      ENDIF

C	  WRITE(*,*) 'TOOLPEN SET:',COLOR,COLS
C
C     normal draw opertions
      CALL GPR_$SET_DRAW_VALUE(COLS,ST)
      CALL GPR_$SET_FILL_VALUE(COLS,ST)
C     set fill value to white always
      CALL GPR_$SET_TEXT_VALUE(COLS,ST)
C     set to current background value

      COLS = COLBAK
 
      CALL GPR_$SET_TEXT_BACKGROUND_VALUE(COLS,ST)
C 
      END
C

      SUBROUTINE TOOLPEN_TEXT(FORE,BACK,SAME,ST)
C     ==========================================
C1    VARYPE                   I4   I4   L   I4
C1    IOSTAT                   I    I    I   O
C
C2    Sets colors for text only
C2
C2         0 = Black
C2        15 = White
C2        256 = Shadow of some description
C
      include  'include/gpr.ins.inc'
      include  'include/daxcolor.inc'
      include  'include/apollo.inc'
C
      INTEGER*4 FORE
      INTEGER*4 BACK
      INTEGER*4 FORET
      INTEGER*4 BACKT
      INTEGER*4 ST
      LOGICAL SAME
      LOGICAL SHADOW
      LOGICAL COLOUR
      INTEGER*2 BMSIZ(2)
C
C     get entry from look up table
      
      COLOUR = HIPLAN.GT.0
C
      IF(COLOUR) THEN
C         remap for X etc
          FORET = COLOR_MAP(FORE)
          BACKT = COLOR_MAP(BACK)
      ELSE
C         make sure color is only drawn
          IF(FORE.GT.0) THEN
             FORET = COLFOR
          ELSE
             FORET = COLBAK
          ENDIF
          IF(BACK.GT.0) THEN
             BACKT = COLFOR
          ELSE
             BACKT = COLBAK
          ENDIF
      ENDIF
C     normal draw opertions 
      CALL GPR_$SET_TEXT_VALUE(FORET,ST)
      CALL GPR_$SET_TEXT_BACKGROUND_VALUE(BACKT,ST)
C
      END
C




  

