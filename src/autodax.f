C
C     @(#)  412.1 date 6/11/92 autodax.f 
C
C
C     Filename    : autodax.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:27
C     Last change : 92/06/11 14:23:55
C
C     Copyright : Practical Technology Limited  
C     File :- autodax.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GENACAD()   start and run acad from within daxcad
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE GENACAD()

      include 'include/params.inc'

      CHARACTER*80 COM
      CHARACTER*80 FILNAM

      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      INTEGER*4 ST

      EXTERNAL NLEN


      LENGTH = NLEN(DRGNAM)


      FILNAM = DRGNAM(:LENGTH-4)


      CALL WRDXF0(.TRUE.)
      WRITE(COM,FMT='(4A)') 
     +'acad ',FILNAM(1:NLEN(FILNAM)),'.dwg',' dxfin'

      CALL SHELLP(COM,ST)

      END

