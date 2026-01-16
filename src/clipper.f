C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 clipper.f   */
C
C
      FUNCTION ISTCLP(VAL,MIN,MAX)
C     ==============================
C1       I               I4 , I4 , I4
C1       O               I , I , I
C
C2        Function SETCLP is set according to
C2        the following conditions :
C2
C2         -1 if VAL < MIN
C2          0 if VAL >= MIN .or. VAL <= MAX
C2         +1 if VAL > MAX
C2
      INTEGER*4 VAL,MIN,MAX
      INTEGER*4 ISTCLP
C
      IF( VAL .LT. MIN ) THEN
         ISTCLP=-1
      ELSE IF( VAL .GT. MAX ) THEN
         ISTCLP=1
      ELSE
         ISTCLP=0
      END IF
C
      END
C
C
      FUNCTION NEWCLP(A,B,C,D,VAR)
C     ============================
C1    VARYPE         I4I4I4I4 I4   
C1    IOSTAT         I  I  I  I
C
C2    Linear interpolation equation used for clipping
C2    lines to limit of viewport
C2  
C2  
C2    Arguments:-
C2  
C2    A           ->
C2    B           ->
C2    C           ->
C2    D           ->
C2    VAR         ->
C2  
C2  
C2    Error Returns:  NONE
C2  
C2  
C2  
C2  
      INTEGER*4 NEWCLP
      INTEGER*4 A
      INTEGER*4 B
      INTEGER*4 C
      INTEGER*4 D
      INTEGER*4 VAR
C 
      NEWCLP = A+((B-A)*(VAR-C)/(D-C))
C
      END
 
 
      SUBROUTINE VCLIP(X1,Y1,X2,Y2,XMIN,YMIN,XMAX,YMAX,VISAB,CODE)
C     ============================================================
C1    VARTPYE          I4 I4 I4 I4  I4    I4   I4   I4   L    I4
C1    IOSTST           I  I  I  I   I     I    I    I    O    O
C
C2    This clipper will clip and modify coords
C2    The return code works as follows
C2    0 nothing visable
C2    1 both points inside the window
C2    2 both points outside the window
C2    3 first point outside
C2    4 second point outside
C2
C2
C
      include   'include/viewport.inc'
C
      LOGICAL VISAB,FPOINT,SPOINT
      INTEGER*4 XMAX,XMIN,YMAX,YMIN,CODE
      INTEGER*4 IX1,IX2,IY1,IY2,IX,IY,CEN,LEFT,RIGHT,TOP,BOTTOM,ISTCLP
C
      PARAMETER(CEN=0,LEFT=-1,BOTTOM=-1,RIGHT=1,TOP=1)
C
      INTEGER*4 X,Y,X1,X2,Y1,Y2,A,B,C,D,VAR,NEWCLP
C
C
      EXTERNAL ISTCLP
      EXTERNAL NEWCLP
C
C
C      NEW(A,B,C,D,VAR)=A+((B-A)*(VAR-C)/(D-C))
C
C       Check position of points on viewport
C        X coordinate       -1 off left
C                            0 centre
C                           +1 off right
C        Y coordinate       -1 off bottom
C                            0 centre
C                           +1 off top
C
      IX1=ISTCLP(X1,XMIN,XMAX)
      IY1=ISTCLP(Y1,YMIN,YMAX)
      IX2=ISTCLP(X2,XMIN,XMAX)
      IY2=ISTCLP(Y2,YMIN,YMAX)
C
C       Indicates if line exists on the screen
      FPOINT = IX1.EQ.CEN .AND. IY1.EQ.CEN
      SPOINT = IX2.EQ.CEN .AND. IY2.EQ.CEN
      IF( FPOINT .AND. SPOINT ) THEN
          CODE = 1
      ELSEIF(FPOINT) THEN
          CODE = 3
      ELSEIF(SPOINT) THEN
          CODE = 4
      ELSE
          CODE = 2
      ENDIF
      VISAB=.FALSE.
C
 10   IF((IX1*IX2).EQ.1 .OR. (IY1*IY2).EQ.1 )  THEN
          CODE = 0
          RETURN
      ENDIF
C
C     If Fpoint .true. then first point is inside window
      FPOINT = IX1.EQ.CEN .AND. IY1.EQ.CEN
      SPOINT = IX2.EQ.CEN .AND. IY2.EQ.CEN
C
C     If VISAB .true. then both end points are inside window
C
      IF( FPOINT .AND. SPOINT ) THEN
          VISAB = .TRUE.
      ENDIF
C
      IF ( VISAB ) THEN
C         make the code both outside but clipped
          RETURN
      ENDIF
C
      IF ( FPOINT ) THEN
C
C       Start with first point of line
         IX=IX2
         IY=IY2
      ELSE
C
C       If first point totally inside viewport start
C       on second point
         IX=IX1
         IY=IY1
C
      END IF
C
      IF ( IX .EQ. LEFT ) THEN
C
C       If X coordinate off left of window interpolate to
C       left edge of viewport X coordinate = left boundary
         Y=NEWCLP(Y1,Y2,X1,X2,XMIN)
         X=XMIN
C
      ELSE IF ( IX .EQ. RIGHT ) THEN
C
C       If X coordinate off right of window interpolate to
C       right edge of viewport X coordinate = right boundary
         Y=NEWCLP(Y1,Y2,X1,X2,XMAX )
         X=XMAX
C
      ELSE IF ( IY .EQ. TOP ) THEN
C
C       If Y coordinate off top of window interpolate to
C       top edge of viewport Y coordinate = top boundary
         X=NEWCLP(X1,X2,Y1,Y2,YMAX)
         Y=YMAX
C
      ELSE IF ( IY .EQ. BOTTOM ) THEN
C
C       If Y coordinate off bottom of window interpolate to
C       bottom edge of viewport Y coordinate = bottom boundary
         X=NEWCLP(X1,X2,Y1,Y2,YMIN)
         Y=YMIN
C
      END IF
C
      IF ( IX .EQ. IX1 .AND. IY .EQ. IY1 ) THEN
C
C       If first point check transfer new points to X1,Y1
         X1=X
         Y1=Y
         IX1=ISTCLP(X1,XMIN,XMAX)
         IY1=ISTCLP(Y1,YMIN,YMAX)
C
      ELSE
C
C       If second point check transfer new points to X2,Y2
         X2=X
         Y2=Y
         IX2=ISTCLP(X2,XMIN,XMAX)
         IY2=ISTCLP(Y2,YMIN,YMAX)
C
      END IF
C
      GOTO 10
C
      END
*
 
