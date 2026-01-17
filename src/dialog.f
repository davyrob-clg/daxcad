C
C     @(#)  412.1 date 6/11/92 dialog.f 
C
C
C     Filename    : dialog.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:42
C     Last change : 92/06/11 14:28:20
C
C     Copyright : Practical Technology International Limited  
C     File :- dialog.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DIAGINIT(ST)
C     SUBROUTINE DIAG_PAINT(DIANUM,PAINT,ST)
C     SUBROUTINE DIAG_DEFINE(DIANUM,RECT,COL,BCOL,ST)
C     SUBROUTINE DIAG_ALLOCATE(DIANUM,OK)
C     SUBROUTINE DIAGDR(DIANUM,DISP,ST)
C
C     |-----------------------------------------------------------------|
C
C
C     Source for dialog boxes requirments
C     Must be used on an existing canvas
C     Uses existing label input and button definitions
C     Must be brought up within notifier returning
C     Dialog event type and 
C
      SUBROUTINE DIAGINIT(ST)
C     =========================
C1    VARYPE                 I4      
C1    IOSTAT                 I      
C
C2    Configures dialog boxes
C
      include    'include/dialog.inc'
C
      INTEGER*4 ST,I,J,K
C
      DO 10 I=1,MAXDIA
C
          DIAGAC(I) = .FALSE.
C
 10   CONTINUE
C
      DIAGCR = 0 
C
      END
      SUBROUTINE DIAG_PAINT(DIANUM,PAINT,ST)
C     ======================================
C1    VARYPE                 I4      L   I4
C1    IOSTAT                 I       I   O
C
C2    Paints in or draws out the dialog box.
C2    Called from within the notifier it handles
C2    everythig else. It looks into the dialog 
C2    structure and draws all associated entities
C
      include    'include/dialog.inc'
      include    'include/menpop.inc'
      include    'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*4 DIANUM
      LOGICAL PAINT,REDCUR
      INTEGER*4 WINDOW(4),ORG(2)
      INTEGER*2 XP,YP
      INTEGER*2 WIN(4)
      INTEGER*2 DBOX(4),BOX(4)
      INTEGER*4 I,J,COL,CURBM,ELEM
      INTEGER*2 II2
      LOGICAL REPAINT
      LOGICAL OK

C
C     if repainting is in progress then cannot erase
C     copy all 
      DO 100 I=1,4
          DBOX(I) = DIAGBX(I,DIANUM)
100   CONTINUE
      XP = DBOX(1)
      YP = DBOX(2)
C     check for any other dialog on display
      IF(PAINT) THEN
C         check for the same dialog box being drawn
          IF(DIAGCR.EQ.DIANUM) THEN
C             dont bother me
              ST = 2
              GOTO 999
          ENDIF
C         increment stack pointer
          DIAG_POINT = DIAG_POINT + 1
          DIAG_STACK(DIAG_POINT) = DIANUM
      ELSE
C         stack underflow here no boxes are on screen
          IF(DIAG_POINT.EQ.0) THEN
              ST = 1
              GOTO 999
          ENDIF
C         check order makeing sure that he
C         wants to remove the last on on the list
          IF(DIANUM.NE.DIAG_STACK(DIAG_POINT)) THEN
              ST = 3
              GOTO 999
          ENDIF
      ENDIF
          
      IF(.NOT.PAINT) THEN
C         NOTE: This is removing the box and should be the second call
          WINDOW(1) = 0
          WINDOW(2) = 0
          WINDOW(3) = DBOX(3) + SHADOW + 1
          WINDOW(4) = DBOX(4) + SHADOW + 1
          ORG(1) = DBOX(1)
          ORG(2) = DBOX(2)
          CALL ROPREP()
C         recover underneath
          CALL BITMAP_COPY(DIAGBT(DIANUM),0,WINDOW,ORG,ST)
C         dealaocate bitmp
          CALL BITMAP_KILL(DIAGBT(DIANUM),ST)
C         decrement stack pointer 
          DIAG_POINT = DIAG_POINT - 1
C         set current dialog box
          DIAGCR = DIAG_STACK(DIAG_POINT)
          ST = 0
          GOTO 999
      ENDIF
      CALL DIAG_ALLOCATE(DIANUM,OK)
C     set location and size of portion of main bitmap to be saved
      WINDOW(1) = DBOX(1)
      WINDOW(2) = DBOX(2)
      WINDOW(3) = DBOX(3) + SHADOW + 1
      WINDOW(4) = DBOX(4) + SHADOW + 1
      ORG(1) = 0
      ORG(2) = 0
C
C     save the current configuration
      CALL BITMAP_COPY(0,DIAGBT(DIANUM),WINDOW,ORG,ST)
C
C     set raster ops for drawing
      CALL ROPREP()
C     set draw value for background filling
      CALL TOOLPEN(DIAGCL(2,DIANUM) )
C     set current back ground to that of dialog box
C     clear area
      CALL TOOLS_RECTANGLE(DBOX,ST)
C
C     set line round box
C
      CALL TOOLPEN(DIAGCL(1,DIANUM))
C
      BOX(1)=DBOX(1)
      BOX(2)=DBOX(2)
      BOX(3)=BOX(1)+DBOX(3)-1
      BOX(4)=BOX(2)+DBOX(4)-1
C
      DO 50 I=1,4
         CALL TOOLS_BOX(BOX(1),BOX(2),BOX(3),BOX(4),ST)
         BOX(1)=BOX(1)+1
         BOX(2)=BOX(2)+1
         BOX(3)=BOX(3)-1
         BOX(4)=BOX(4)-1
 50   CONTINUE
C     Next ... The shadow.
      II2 = 1
C     set for black drawing
      COL = COLFOR
C     make raster ops are set
      CALL ROPREP()
      CALL TOOLPEN(COL)
      CALL TOOLS_SET_SHADOW(.TRUE.)
C     reset box postion
      BOX(1)=DBOX(1)
      BOX(2)=DBOX(2)
      BOX(3)=BOX(1)+DBOX(3)
      BOX(4)=BOX(2)+DBOX(4)
      WIN(1) = BOX(3)
      WIN(2) = BOX(2) + SHADOW
      WIN(3) = SHADOW - 3
      WIN(4) = BOX(4) - BOX(2) - 3
      CALL TOOLS_RECTANGLE(WIN,ST)
      WIN(1) = BOX(1) + SHADOW
      WIN(2) = BOX(4)
      WIN(3) = BOX(3) - BOX(1) -3 
      WIN(4) = SHADOW - 3
      CALL TOOLS_RECTANGLE(WIN,ST)
C
      CALL TOOLS_SET_SHADOW(.FALSE.)
      ORG(1) = DIAGBX(1,DIANUM)
      ORG(2) = DIAGBX(2,DIANUM)
C     Now ... paint the box with elements
      DIAGCR = DIANUM
C     now repaint the cursor if it was on
      ST = 0
999   CONTINUE
      END
C
C
      SUBROUTINE DIAG_DEFINE(DIANUM,RECT,COL,BCOL,ST)
C     ===============================================
C1    VARYPE                   I4   I4(4)  I4 I4  I4
C1    IOSTAT                   I     I     I   I  O
C
C2    This will define a dialog box for drawing
C2    The frame defined will be saved and repainted
C2    The frame and shdow will draw inwards remember  
C2    this wehen defining the dialog box itself
C
      include    'include/dialog.inc'
C
      INTEGER*4 DIANUM,RECT(4)
      INTEGER*4 COL,BCOL,ST,I,J
C
      IF(DIANUM.EQ.0)  THEN
          DO 10 I=1,MAXDIA
              IF(.NOT.DIAGAC(I)) THEN
                  DIANUM = I
                  GOTO 20
              ENDIF
10        CONTINUE
      ENDIF

20    CONTINUE
C     check limits here
      IF(DIANUM.GT.MAXDIA) THEN
          PRINT*, 'No more Dialog boxes (MAX) ',MAXDIA
          ST = 1
          RETURN
      ENDIF
C     copy in the size of box please
      DO 50 I=1,4
          DIAGBX(I,DIANUM) = RECT(I)
50    CONTINUE
C     set flags active
      DIAGAC(DIANUM) = .TRUE.
      DIAGCL(1,DIANUM) = COL
      DIAGCL(2,DIANUM) = BCOL
      DIADIS(DIANUM) = .FALSE.
C
      END
C
      SUBROUTINE DIAG_ALLOCATE(DIANUM,OK)
C     =====================================
C1    VARTYPE                     I4   L
C1    IOSTAT                      I    O
C
C     alloactes a bitmap for swapping in dialog box
C2
      include  'include/apollo.inc'
      include  'include/daxcolor.inc'
      include  'include/dialog.inc'
      include  'include/menpop.inc'
C
      LOGICAL OK,EX
      INTEGER*4 BMSIZ(2)
      INTEGER*4 ST,COL
      INTEGER*4 DIANUM
C
C     allocate a bitmap for the menu and for saving current
      BMSIZ(1) = DIAGBX(3,DIANUM) + SHADOW +2
      BMSIZ(2) = DIAGBX(4,DIANUM) + SHADOW +2
C     Allocate a bit map in memory
      DIAGBT(DIANUM) = 0
      CALL MAKE_BITMAP(DIAGBT(DIANUM),BMSIZ,0,ST)
C     Allocate an attribute block for the bitmap
      COL = DIAGCL(2,DIANUM) 

C     white it out
      CALL BITMAP_CLEAR(DIAGBT(DIANUM),COL,ST)
C 
      END

C
      SUBROUTINE DIAGDR(DIANUM,DISP,ST)
C     =================================
C1    VARYPE             I4     L   I4
C1    IOSTAT             I      I   O
C
C2    Draws or erases a dialog box
C  
      include  'include/dialog.inc'
C
      INTEGER*4 DIANUM
      INTEGER*4 ST
      LOGICAL DISP
C
      IF(DIANUM.LT.1.OR.DIANUM.GT.MAXDIA) THEN
C         out of range error
          ST = 6
          GOTO 999
      ENDIF
      IF(.NOT.DIAGAC(DIANUM)) THEN
C         currently not displayed
          ST = 7
          GOTO 999
      ENDIF

      IF(DISP.AND..NOT.DIADIS(DIANUM)) THEN
          DIADIS(DIANUM) = .TRUE.
      ELSEIF(.NOT.DISP.AND.DIADIS(DIANUM)) THEN
          DIADIS(DIANUM) = .FALSE.
      ELSE
          ST = 8
          GOTO 999
      ENDIF
C     draw or erase the text
      CALL DIAG_PAINT(DIANUM,DISP,ST)
      CALL GPR_$SERVER_FLUSH_X(ST)

999   CONTINUE
      END
C

