
C
C     @(#)  412.1 date 6/11/92 browsex11.f 
C
C
C     Filename    : browsex11.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:51:15
C     Last change : 92/06/11 14:24:30
C
C     Copyright : Practical Technology International Limited  
C     File :- browsex11.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     X windows dependant browse code
C
C
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ALLOCBITFILE(NAME,ST)
C     ================================
C1    VARTYPE                  C   L
C
C2    Allocate a bitmap descripter for drawing into then
C2    copying onto disk as a file. 
C2  
C2  
C2    Arguments:-
C2  
C2    NAME	->	The filename to be use for the bitmap
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C2  
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/dtext.inc'
      include 'include/browse.inc'
      include 'include/interface.inc'
C
      INTEGER*4 ST,NLEN
      INTEGER*2 WINDOW(4),BOFF(2),L,BMSIZ(2),PLANE
      LOGICAL LWAIT,UNOBSCURED,OK,CREATED
      CHARACTER NAME*(*)
C
      EXTERNAL NLEN
C
C     get length of current name
      L = NLEN(NAME)
      IF(L.EQ.0) THEN
          OK = .FALSE.
          GOTO 999
      ENDIF
C     set up varaibles
      SIZE(1) = BRSBX
      SIZE(2) = BRSBY
C     allocate the storage bitmap
      BMSIZ(1) = BRSBX
      BMSIZ(2) = BRSBY
C
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(GENATT,ST)
C     only one plane please
      PLANE = 0
      CALL GPR_$ALLOCATE_BITMAP
     +     (BMSIZ,PLANE,GENATT,GENBIT,ST)
C
      IF(ST.GT.0) THEN
          OK = .FALSE.
          GOTO 999
      ENDIF
C     set ok flag and go home
      OK = .TRUE.
999   CONTINUE
      END
C

C

      SUBROUTINE BROWSELOADIMAGE(X,Y,NAME,ST)
C     =======================================
C1    VARTYPE                     I2 I2 C*(*)L
C1    IOSTAT                      I  I   I   O
C
C2    This routine will read out a bitmap file
C2    of the specified name. It uses GPR_$READONLY
C2    to read the file
C2    it will then blt the contents of the bitmap set up
C2    specially to cope with load and save browse
C
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/browse.inc'
      include 'include/interface.inc'
C
      CHARACTER*(*) NAME
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 XO,YO
      INTEGER*2 PLANE
      INTEGER*2 X,Y
      INTEGER*2 BMSIZ(2)
      INTEGER*2 WINDOW(4)
      INTEGER*2 ORGW(2)
      INTEGER*4 F,B

      EXTERNAL NLEN

      XO = X
      YO = Y
      BMSIZ(1) = BRSBX
      BMSIZ(2) = BRSBY
      PLANE = HIPLAN
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(GENATT,ST)

      CALL GPR_$ALLOCATE_BITMAP
     +     (BMSIZ,PLANE,GENATT,GENBIT,ST)

      CALL GPR_$SET_BITMAP(GENBIT,ST)
      CALL ROPREP()
C      CALL TOOLPEN(MENUF)
C      WINDOW(1) = 0
C      WINDOW(2) = 0
C      WINDOW(3) = BRSBX-1
C      WINDOW(4) = BRSBY-1
C      CALL GPR_$RECTANGLE(WINDOW,ST)
C      CALL TOOLPEN(MENUF)

      CALL GPR_$SET_FILL_VALUE(1,ST)
      CALL GPR_$SET_FILL_BACKGROUND_VALUE(0,ST)

C      CALL HACK_GET_FB(F,B)
C      IF (HIPLAN .EQ. 0) THEN
C        CALL GPR_$SET_FILL_VALUE(1,ST)
C        CALL GPR_$SET_FILL_BACKGROUND_VALUE(0,ST)
C      ELSE
C        WRITE(*,*) 'Setting F,B:', F, B
C        CALL GPR_$SET_FILL_VALUE(F,ST)
C        CALL GPR_$SET_FILL_BACKGROUND_VALUE(B,ST)
C      ENDIF

      CALL GPR_$READ_BITMAP(GENBIT,0,0,NAME,NLEN(NAME),ST)
      CALL GPR_$SET_BITMAP(DISPDE,ST)
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = BRSBX-1
      WINDOW(4) = BRSBY-1
      ORGW(1) = X+1
      ORGW(2) = Y+1
      CALL GPR_$PIXEL_BLT(GENBIT,WINDOW,ORGW,ST)

      CALL GPR_$DEALLOCATE_BITMAP(GENBIT,ST)

      END

