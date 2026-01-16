C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 colour84.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ROPOR()
C     SUBROUTINE ROPREP()
C     SUBROUTINE ROPXOR()
C
C
C     |-----------------------------------------------------------------|
C

C
      SUBROUTINE ROPOR()
C     ===================
C
C2    Subroutine ROPXOR sets the current raster operation
C2    to exclusive-OR  mode.
C
      include   'include/viewport.inc'
      INTEGER*4 ST,CBITM,I
      INTEGER*4 ROPT
      EXTERNAL RASTER
C
      ROPT=7
      CALL RASTER(ROPT)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE ROPREP()
C     ===================
C
C2    Subroutine ROPREP sets the current raster operation
C2    to replace mode.
C
      include   'include/viewport.inc'
      INTEGER*4 ST,CBITM,I
      INTEGER*4 ROPT
      EXTERNAL RASTER
C
      ROPT=3
      CALL RASTER(ROPT)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE ROPXOR()
C     ===================
C
C2    Subroutine ROPXOR sets the current raster operation
C2    to exclusive-OR  mode.
C
      include   'include/viewport.inc'
      INTEGER*4 ST,CBITM,I
      INTEGER*4 ROPT
      EXTERNAL RASTER
C
      ROPT=6
      CALL RASTER(ROPT)
C
      END
C
C------------------------------------------------------------
C
      SUBROUTINE ROPAND()
C     ===================
C
C2    Subroutine ROPXOR sets the current raster operation
C2    to exclusive-OR  mode.
C
      include   'include/viewport.inc'
      INTEGER*4 ST,CBITM,I
      INTEGER*4 ROPT
      EXTERNAL RASTER
C
      ROPT=1
      CALL RASTER(ROPT)
C
      END

