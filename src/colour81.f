C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 colour81.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION COLORE(RED,GREEN,BLUE)
C     SUBROUTINE PENDRW()
C     SUBROUTINE PENERS()
C     SUBROUTINE PENFIL()
C     SUBROUTINE SETAPN(KOLOUR)
C     SUBROUTINE SETDRW(KOLOUR)
C     SUBROUTINE SETERS(KOLOUR)
C     SUBROUTINE SETFIL(KOLOUR)
C
C
C     |-----------------------------------------------------------------|
C

      FUNCTION COLORE(RED,GREEN,BLUE)
C     ===============================
      INTEGER*4 RED,GREEN,BLUE,COLORE,I,J
 
      I=65536
      J=256
      COLORE = I*RED + J*GREEN + BLUE
      END
C
C-----------------------------------------------------------------
C
 
C
C------------------------------------------------------------
C
      SUBROUTINE PENDRW()
C     ===================
C
C2    Subroutine PENDRW loads the pen with the
C2    current draw colour.
C
      include 'include/daxcolor.inc'
      include 'include/style.inc'
C
      EXTERNAL PEN
C
C     set pen to current draw colour
      CALL PEN(COLDRW)
C     save active pen colour
      COLPEN=COLDRW
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE PENERS()
C     ===================
C
C2    Subroutine PENERS loads the pen with the
C2    current erase colour.
C
      include 'include/daxcolor.inc'
C
      EXTERNAL PEN
C
C     set pen to current erase colour
      CALL PEN(COLERS)
C     save active pen colour
      COLPEN=COLERS
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE PENFIL()
C     ===================
C
C2    Subroutine PENFIL loads the pen with the
C2    current fill colour.
C
      include 'include/daxcolor.inc'
C
      EXTERNAL PEN
C
C     set pen to current fill colour
      CALL PEN(COLFIL)
C     save active pen colour
      COLPEN=COLFIL
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE SETAPN(KOLOUR)
C     =========================
C
C1    vartype             I2
C1    iostatus            I
C
C2    Subroutine SETAPN sets the active colour to KOLOUR
C2    and loads the pen with the active colour.
C
      include 'include/daxcolor.inc'
C
      INTEGER*2 KOLOUR,PENCOL
      EXTERNAL PEN,PENCOL
C
C     ensure colour is displayable
      COLPEN=PENCOL(KOLOUR)
C     set pen to current active colour
      CALL PEN(COLPEN)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE SETDRW(KOLOUR)
C     =========================
C
C1    vartype             I2
C1    iostatus            I
C
C2    Subroutine SETDRW sets the draw colour to KOLOUR
C2    and loads the pen with the current draw colour.
C
      INTEGER*2 KOLOUR,PENCOL
      EXTERNAL PEN,PENCOL
C
C     set current draw colour
      KOLOUR=PENCOL(KOLOUR)
C     set pen to current draw colour
      CALL PEN(KOLOUR)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE SETERS(KOLOUR)
C     =========================
C
C1    vartype             I2
C1    iostatus            I
C
C2    Subroutine SETERS sets the erase colour to KOLOUR
C2    and loads the pen with the current erase colour.
C
      include 'include/daxcolor.inc'
C
      INTEGER*2 KOLOUR,PENCOL,PENC
      EXTERNAL PEN,PENCOL
C
C     ensure colour is displayable
      PENC=PENCOL(KOLOUR)
C     set current erase colour
      COLERS=PENC
C     save active pen colour
      COLPEN=COLERS
C     set pen to current draw colour
      CALL PEN(COLERS)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE SETFIL(KOLOUR)
C     =========================
C
C1    vartype             I2
C1    iostatus            I
C
C2    Subroutine SETFIL sets the fill colour to KOLOUR
C2    and loads the pen with the current fill colour.
C
      include 'include/daxcolor.inc'
C
      INTEGER*2 KOLOUR,PENCOL,PENC
      EXTERNAL PEN,PENCOL
C
C     ensure colour is displayable
      PENC=PENCOL(KOLOUR)
C     set current fill colour
      COLFIL=PENC
C     save active pen colour
      COLPEN=COLFIL
C     set pen to current fill colour
      CALL PEN(COLDRW)
C
      END
C
C------------------------------------------------------------
C
