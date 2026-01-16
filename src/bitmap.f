C
C     @(#)  412.1 date 6/11/92 bitmap.f 
C
C
C     Filename    : bitmap.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:35
C     Last change : 92/06/11 14:24:12
C
C     Copyright : Practical Technology International Limited  
C     File :- bitmap.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INIT_BITMAP(ST)
C     SUBROUTINE MAKE_BITMAP(BITNUM,BMSIZE,PLANES,ST)
C     SUBROUTINE BITMAP_KILL(BITNUM,ST)
C     SUBROUTINE BITMAP_COPY(SOURCE,TARGET,WINDOW,ORIGIN,ST)
C     SUBROUTINE SET_BITMAP(BITNUM,ST)
C     SUBROUTINE BITMAP_CLEAR(BITNUM,COLOR,ST)C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE INITBITMAP(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Initalise bitmapping stuff
C
      include  'include/bitmap.inc'
      include  'include/obswin.inc'
      include  'include/apollo.inc'
C
      INTEGER*4 ST
      INTEGER*4 I
C
      DO 10 I = 0,BITMAX
C         reset all descripters
          BITDES(1,I) = 0
10    CONTINUE
C
      BITDES(1,0) = DISPDE
C     attribute block is 0
      BITDES(2,0) = 0
      BITDES(3,0) = ROOTW(1)
      BITDES(4,0) = ROOTW(2)
      BITDES(5,0) = HIPLAN
C
      BATCH_AQ = .FALSE.
      GR_LOCK =  .FALSE.
C      
C
      END
C
      SUBROUTINE MAKE_BITMAP(BITNUM,BMSIZE,PLANES,ST)
C     ==============================================
C1    VARYPE                  I4      I4    I4   I4
C1    IOSTAT                  I       I     I    I
C
C2    This routine makes a bitmap. If any values
C2    are 0 then they will inherit from the canvas
C2  
      include  'include/bitmap.inc'
C
      INTEGER*4 BITNUM
      INTEGER*4 BMSIZE(2)
      INTEGER*4 PLANES
      INTEGER*4 ST
      INTEGER*4 BITMAP
      INTEGER*4 ATTR
      INTEGER*2 SIZE(2)
      INTEGER*2 HIPLAN
      INTEGER*4 I
C
      IF(BITNUM.EQ.0)  THEN
          DO 10 I=1,BITMAX
              IF(BITDES(1,I).EQ.0) THEN
                  BITNUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
C         ran out of descripters
          ST = 1
          GOTO 999
      ENDIF
C     
20    CONTINUE
C
C     ok go ahead and do the buisness with bitmaps 
C     
      IF(PLANES.EQ.0) THEN
          HIPLAN = BITDES(5,0)
      ELSE
          HIPLAN = PLANES
          BITDES(5,BITNUM) = PLANES
      ENDIF
C     bitmap size allocation
      IF(BMSIZE(1).LE.0) THEN
          SIZE(1) = BITDES(3,0)
      ELSE
          SIZE(1) = BMSIZE(1)
      ENDIF
      IF(BMSIZE(2).LE.0) THEN
          SIZE(2) = BITDES(3,0)
      ELSE
          SIZE(2) = BMSIZE(2)
      ENDIF
C     allocate attribute block
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(ATTR,ST)
      IF(ST.GT.0) THEN
          ST = 2
          GOTO 999
      ENDIF
C     Allocate a bit map in memory
      CALL GPR_$ALLOCATE_BITMAP(SIZE,HIPLAN,ATTR,BITMAP,ST)
      IF(ST.GT.0) THEN
          ST = 3
          GOTO 999
      ENDIF
C     ok save all details
      BITDES(1,BITNUM) = BITMAP
      BITDES(2,BITNUM) = ATTR
      BITDES(3,BITNUM) = SIZE(1)
      BITDES(4,BITNUM) = SIZE(2)
      BITDES(5,BITNUM) = HIPLAN
      ST  =0
      RETURN
999   CONTINUE
      END
C
      SUBROUTINE BITMAP_KILL(BITNUM,ST)
C     =================================
C1    VARYPE                   I4   I4
C1    IOSTAT                   I    O
C
C2    This routine destroys a made bitmap
C  
      include  'include/bitmap.inc'
C
      INTEGER*4 BITNUM
      INTEGER*4 ST
      INTEGER*4 BITMAP
      INTEGER*4 ATTR
C
      IF(BITNUM.LT.1.OR.BITNUM.GT.BITMAX) THEN
C         error range of bitmap number
          ST = 1
          GOTO 999
      ENDIF
C
      IF( BITDES(1,BITNUM).GT.0) THEN
C         load details
          BITMAP = BITDES(1,BITNUM)
          ATTR  =  BITDES(2,BITNUM)
C         deallocate all
          CALL GPR_$DEALLOCATE_BITMAP (BITMAP,ST)
          IF(ST.GT.0) THEN
              ST = 2
              GOTO 999
          ENDIF
          CALL GPR_$DEALLOCATE_ATTRIBUTE_BLOCK (ATTR,ST)
          IF(ST.GT.0) THEN
              ST = 3
              GOTO 999
          ENDIF
          BITDES(1,BITNUM) = 0
      ELSE
          ST = 4
          GOTO 999
      ENDIF
C     ok here
      ST = 0
      RETURN
999   CONTINUE
      END
C
      SUBROUTINE BITMAP_COPY(SOURCE,TARGET,WINDOW,ORIGIN,ST)
C     ======================================================
C1    VARYPE                  I4      I4     I4     I4   I4
C1    IOSTAT                  I       I      I      I    O
C
C2    copy a bitmap window to a target bitmap 
C  
      INTEGER*4 SOURCE
      INTEGER*4 TARGET
      INTEGER*4 ORIGIN(2)
      INTEGER*4 WINDOW(4)
      INTEGER*4 ST
      INTEGER*4 SRC
      INTEGER*4 TRG
      INTEGER*4 I
      INTEGER*2 SOURCE_I2
      INTEGER*2 TARGET_I2
      INTEGER*2 ORIGIN_I2(2)
      INTEGER*2 WINDOW_I2(4)
C
      include  'include/bitmap.inc'
C
      IF(SOURCE.LT.0.OR.SOURCE.GT.BITMAX) THEN
C         error range of bitmap number
          ST = 1
          GOTO 999
      ENDIF
C     target
      IF(TARGET.LT.0.OR.TARGET.GT.BITMAX) THEN
C         error range of bitmap number
          ST = 1
          GOTO 999
      ENDIF
C
      DO 10 I=1,4
         WINDOW_I2(I) = WINDOW(I)
10    CONTINUE
      DO 20 I=1,2
         ORIGIN_I2(I) = ORIGIN(I)
20    CONTINUE
C
      IF(WINDOW_I2(1).LT.0) THEN
          WINDOW_I2(3) = WINDOW_I2(3) + WINDOW_I2(1)
      ENDIF
      IF(WINDOW_I2(2).LT.0) THEN
          WINDOW_I2(4) = WINDOW_I2(4) + WINDOW_I2(4)
      ENDIF
      SRC = BITDES(1,SOURCE)
      TRG = BITDES(1,TARGET)
      CALL TOOLS_PIXEL_BLT(SRC,TRG,WINDOW_I2,ORIGIN_I2,ST)
C
      ST = 0
      RETURN
999   CONTINUE
      END
C
      SUBROUTINE SET_BITMAP(BITNUM,ST)
C     ================================
C1    VARYPE                  I4   I4
C1    IOSTAT                  I    O
C
C2    sets working bitmap
C
      include  'include/bitmap.inc'
C
      INTEGER*4 BITNUM
      INTEGER*4 ST
      INTEGER*4 BITMAP
C
      IF(BITNUM.LT.0.OR.BITNUM.GT.BITMAX) THEN
C         error range of bitmap number
          ST = 1
          GOTO 999
      ENDIF
C
      BITMAP = BITDES(1,BITNUM)
      CALL GPR_$SET_BITMAP(BITMAP,ST)
      IF(ST.GT.0) THEN
          ST = 2
          GOTO 999
      ENDIF
C     ok here
      ST = 0
      RETURN
999   CONTINUE
      END
C
      SUBROUTINE BITMAP_CLEAR(BITNUM,COLOR,ST)
C     ========================================
C1    VARYPE                    I4     I4  I4
C1    IOSTAT                    I      I   O
C
C2    Clear the bitmap 
C  
      include  'include/bitmap.inc'
C
      INTEGER*4 BITNUM
      INTEGER*4 ST
      INTEGER*4 BITMAP
      INTEGER*4 CURBIT
      INTEGER*4 COLOR
      INTEGER*2 RECT(4)
C
      IF(BITNUM.LT.0.OR.BITNUM.GT.BITMAX) THEN
C         error range of bitmap number
          ST = 1
          GOTO 999
      ENDIF
C
      CALL GPR_$INQ_BITMAP(CURBIT,ST)
C
      RECT(1) = 0
      RECT(2) = 0
      RECT(3) = BITDES(3,BITNUM)
      RECT(4) = BITDES(4,BITNUM)
      IF(BITNUM.EQ.0) THEN
C         main canvas bitmap
          BITMAP = BITDES(1,BITNUM)
          CALL GPR_$SET_BITMAP(BITMAP,ST)
          CALL TOOLPEN(COLOR)
          CALL TOOLS_RECTANGLE(RECT,ST)
      ELSE
          BITMAP = BITDES(1,BITNUM)
          IF(BITMAP.GT.0) THEN
              CALL GPR_$SET_BITMAP(BITMAP,ST)
              CALL TOOLPEN(COLOR)
              CALL GPR_$RECTANGLE(RECT,ST)
          ENDIF
      ENDIF
C
C     ok here
      ST = 0
      CALL GPR_$SET_BITMAP(CURBIT,ST)
      RETURN
999   CONTINUE
      END




