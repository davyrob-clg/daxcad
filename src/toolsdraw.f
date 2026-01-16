C
C     @(#)  412.1 date 6/11/92 toolsdraw.f 
C
C
C     Filename    : toolsdraw.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:38
C     Last change : 92/06/11 14:42:21
C
C     Copyright : Practical Technology International Limited  
C     File :- toolsdraw.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE TOOLS_LINE(X1,Y1,X2,Y2,ST)
C     SUBROUTINE TOOLS_BOX(X1,Y1,X2,Y2,ST)
C     SUBROUTINE TOOLS_MULTILINE(CURX,CURY,POS,ST)
C     SUBROUTINE TOOLS_POLYLINE(CURX,CURY,POS,ST)
C     SUBROUTINE TOOLS_RECTANGLE(RECT,ST)
C     SUBROUTINE TOOLS_TEXT(X,Y,TEXT,TLEN,ST)
C     SUBROUTINE TOOLS_TEXT_AP( TEXT,TLEN,ST )
C     SUBROUTINE TOOLS_CIRCLE(CENTER,RADIUS,ST)
C     SUBROUTINE TOOLS_CIRCLE_FILLED(CENTER,RADIUS,ST)
C     SUBROUTINE TOOLS_POLYFILL(NX,NY,NOE,ST)
C     SUBROUTINE TOOLS_TRIANGLE(V1,V2,V3,ST)
C     SUBROUTINE TOOLS_PIXEL_BLT(SOURCE,TARGET,WIN,ORIGIN,ST)C
C
C     |-----------------------------------------------------------------|
C


      SUBROUTINE TOOLS_LINE(X1,Y1,X2,Y2,ST)
C     =====================================
C1    VARTYPE               I2 I2 I2 I2 I4
C1    IOSTST                I  I  I  I  O
C
C     This routine will output text to the screen
C
C
      INTEGER*4 ST
      INTEGER*4 INDEX
      INTEGER*4 J,X,Y
      INTEGER*4 I
      INTEGER*2 X1,Y1,X2,Y2
      INTEGER*2 WINDOW(4)
C
C     normal drawing ops
      CALL GPR_$MOVE(X1,Y1,ST)
      CALL GPR_$LINE(X2,Y2,ST)
C
      END
C
      SUBROUTINE TOOLS_BOX(X1,Y1,X2,Y2,ST)
C     =====================================
C1    VARTYPE              I2 I2 I2 I2 I4
C1    IOSTST               I  I  I  I  O
C
C     This routine will output box to the screen
C
C
      INTEGER*4 ST
      INTEGER*4 INDEX
      INTEGER*4 J,X,Y
      INTEGER*4 I
      INTEGER*2 X1,Y1,X2,Y2
      INTEGER*2 WINDOW(4)
C
      CALL GPR_$DRAW_BOX(X1,Y1,X2,Y2,ST)
C
      END

      SUBROUTINE TOOLS_MULTILINE(CURX,CURY,POS,ST)
C     ============================================
C1    VARTYPE                     I2   I2  I2  I4
C1    IOSTST                      I    I   I   O
C
C2    The routine outputs a multline to the screen
C
      INTEGER*4 ST,INDEX,J,X,Y
      INTEGER*2 POS
      INTEGER*2 X1,Y1,X2,Y2,I
      INTEGER*2 WINDOW(4)
      INTEGER*2 CURX(POS)
      INTEGER*2 CURY(POS)
C
      CALL GPR_$MULTILINE(CURX,CURY,POS,ST)
C
      END

      SUBROUTINE TOOLS_POLYLINE(CURX,CURY,POS,ST)
C     ===========================================
C1    VARTYPE                    I2   I2  I2  I4
C1    IOSTST                     I    I   I   O
C
C2    This routine outputs a polyline to the screen
C
      INTEGER*4 ST
      INTEGER*2 POS
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 CURX(POS)
      INTEGER*2 CURY(POS)
C
      CALL GPR_$POLYLINE(CURX,CURY,POS,ST)
C
      END
C
      SUBROUTINE TOOLS_RECTANGLE(RECT,ST)
C     ===================================
C1    VARTYPE                    I2  I4
C1    IOSTST                     I   O
C
C2    This routine will output a filled rectangle to
C2    the screen
C
      INTEGER*4 ST
      INTEGER*2 POS,RECT(4)
      INTEGER*2 WINDOW(4)
      INTEGER*2 I
C
      CALL GPR_$RECTANGLE(RECT,ST)
C
      END
C

      SUBROUTINE TOOLS_TEXT(X,Y,TEXT,TLEN,ST)
C     =======================================
C1    VARTYPE           I2 I2 C*(*) I2 I4
C1    IOSTST             I  I   I   I  O
C
C2    This routine will output text to the screen
C
C
      INTEGER*4 ST
      INTEGER*2 TLEN
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 X,Y
      CHARACTER*(*) TEXT
C
C     range check
      IF(TLEN.LE.0.OR.TLEN.GT.32768) RETURN
C
      CALL GPR_$MOVE(X,Y,ST)
      ST = 0
      CALL GPR_$TEXT(TEXT,TLEN,ST)
      END
C
      SUBROUTINE TOOLS_TEXT_AP( TEXT,TLEN,ST )
C     ========================================
C1    VARTYPE                    C*(*) I2 I4
C1    IOSTST                       I   I  O
C
C2    This routine will output text to the screen
C2    at the current position. ie the the current 
C2    draw postion. Usefull for appending text after
C2    a draw.
C
      INTEGER*4 ST
      INTEGER*2 TLEN
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 X,Y
      CHARACTER*(*) TEXT
C
C     range check
      IF(TLEN.LE.0.OR.TLEN.GT.32768) RETURN
C
C     get current position
      CALL GPR_$INQ_CP(X,Y,ST)
C     draw text
      CALL GPR_$TEXT(TEXT,TLEN,ST)
C
      END

      SUBROUTINE TOOLS_CIRCLE(CENTER,RADIUS,ST)
C     ========================================
C1    VARTYPE                  I2(4)    I2  I4
C1    IOSTST                     I      I   O
C
C2    This routine will output a circle
C
      INTEGER*4 ST
      INTEGER*2 TLEN
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 CENTER
      INTEGER*2 RADIUS
C
C     draw circle
      CALL GPR_$CIRCLE(CENTER,RADIUS,ST)
C
      END
C
      SUBROUTINE TOOLS_CIRCLE_FILLED(CENTER,RADIUS,ST)
C     ================================================
C1    VARTYPE                         I2(4)    I2  I4
C1    IOSTST                            I      I   O
C
C2    This routine will output a filled circle
C
C
      INTEGER*4 ST
      INTEGER*2 TLEN
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 CENTER
      INTEGER*2 RADIUS
C
C     normal draw ops
      CALL GPR_$CIRCLE_FILLED(CENTER,RADIUS,ST)
C
      END
C
      SUBROUTINE TOOLS_POLYFILL(NX,NY,NOE,ST)
C     =======================================
C1    VARTYPE                  I2  I2  I2  I4
C1    IOSTST                   I   I   I   O
C
C2    This routine will output a filled circle
C
      INTEGER*4 ST
      INTEGER*4 BUTSET(8)
      INTEGER*2 WINDOW(4)
      INTEGER*2 NOE
      INTEGER*2 NX(NOE),NY(NOE)
      INTEGER*2 I
C
C     normal draw ops
      CALL GPR_$START_PGON(NX(1),NY(1),ST)
      CALL GPR_$PGON_POLYLINE(NX,NY,NOE,ST)
      CALL GPR_$CLOSE_FILL_PGON(ST)
C
      END
C
      SUBROUTINE TOOLS_TRIANGLE(V1,V2,V3,ST)
C     ======================================
C1    VARTYPE                   I2 I2 I2 I4
C1    IOSTST                    I  I  I  O
C
C2    This routine will output a triangle to the screen
C
      INTEGER*4 ST
      INTEGER*2 TLEN
      INTEGER*2 I
      INTEGER*2 WINDOW(4)
      INTEGER*2 V1(2)
      INTEGER*2 V2(2)
      INTEGER*2 V3(2)
	  INTEGER*2 X,Y
	  INTEGER*2 NX(2),NY(2),NOE

C
      X=0
	  Y=0
	  NX(1) = V2(1)
	  NY(1) = V2(2)
      NX(2) = V3(1)
	  NY(2) = V3(2)
	  NOE = 2


C
C     DPS -> This works with Xwin32 but not Xfree86, so add a PGON thing
C

      CALL GPR_$START_PGON(V1(1),V1(2),ST)
      CALL GPR_$PGON_POLYLINE(NX,NY,NOE,ST)
      CALL GPR_$CLOSE_FILL_PGON(ST)

C      CALL GPR_$TRIANGLE(V1,V2,V3,ST)
C	  CALL GPR_$LINE(X,Y,V1(1),V1(2),ST)
C
      END

      SUBROUTINE TOOLS_PIXEL_BLT(SOURCE,TARGET,WIN,ORIGIN,ST)
C     ======================================================
C1    VARTYPE                      I4     I4     I2   I2 I4
C1    IOSTST                       I      I      I    I  I
C
C2    This routine will pixel blit
C
      include  'include/bitmap.inc'
C
      INTEGER*4 ST
      INTEGER*4 SOURCE
      INTEGER*4 TARGET
      INTEGER*4 DISPLAY
      INTEGER*2 WIN(4)
      INTEGER*2 ORIGIN(2)
C
      INTEGER*2 I
      INTEGER*4 CURBIT
      INTEGER*2 WINDOW(4)

C
C     set current bitmap
      CALL GPR_$INQ_BITMAP(CURBIT,ST)
C     set target bitmap
      CALL GPR_$SET_BITMAP(TARGET,ST)
C
      DISPLAY = BITDES(1,0)

      CALL GPR_$PIXEL_BLT(SOURCE,WIN,ORIGIN,ST)
C
      CALL GPR_$SET_BITMAP(CURBIT,ST)
C
      END
C
C
