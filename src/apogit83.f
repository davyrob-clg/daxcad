C
C     @(#)  412.1 date 6/11/92 apogit83.f 
C
C
C     Filename    : apogit83.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:29:33
C     Last change : 92/06/11 14:23:17
C
C     Copyright : Practical Technology Limited  
C     File :- apogit83.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GTIMEN(GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4,HOR)
C     SUBROUTINE GTIWIN(GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4,GTWOO)C
C
C     |-----------------------------------------------------------------|
C
C
C
      SUBROUTINE GTIMEN(GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4,HOR)
C     =========================================================
C1    vartype             I4     I4     I4     I4     I4    L
C1    iostatus            I      I      I      I      I     I
C1
C2    Formerly GTXT_INIT_MENU,this routine initializes a menu area
C2    on the graphics screen,positioned with top left corner at apollo
C2    screen coords GTWOX4,GTWOY4.The identifier for the menu to be
C2    created is GTWID, an integer in the range 1-4 at present. The
C2    number of chatracters in each line is to be GTWLL4,with a page
C2    length of GTWPL4 lines.HOR is a logical flag paseed as .TRUE.
C2    if a horizontal menu is to be initialized.
C2  
C2    Arguments:-
C2  
C2    GTWID4          ->          Menu number being created
C2    GTWOX4          ->          Top left X origin of menu
C2    GTWOY4          ->          Top left Y origin of menu
C2    GTWLL4          ->          number of chars in each line (COLUMNS)
C2    GTWPL4          ->          Number of lines in the page (ROWS)
C2    HOR             ->          Indicates hotizontal if .TRUE.
C2
C2    Error Returns:NONE
C2  
C2  
C
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/drgbar.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
C
      REAL LX1,LY1,LX2,LY2
      INTEGER*2 GTWID,GTWOX,GTWOY,
     +          GTWLL,GTWPL,PLMASK,
     1          LX(1:8),LY(1:8),
     2          X1,Y1,X2,Y2,I,BMSIZ(2),
     3          CELLDX,CELLDY,MENDX,MENDY,
     4          I2
      INTEGER*2 TWLIMS(4),BMWIND(2,2),DISPOS(2),WIN(4)
      INTEGER*4 ST,ADESC,MDESC,GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4,COL
      INTEGER*4 GW
      LOGICAL UNOBSCURED,GTWOO,HOR,ACT
C
C     convert all passed arguments to i*2 for apollo use
      CALL ROPREP()
      GTWID=GTWID4
      GTWOX=GTWOX4
      GTWOY=GTWOY4
      GTWLL=GTWLL4
      GTWPL=GTWPL4
C     Menu window can only be output
      GTWOO=.TRUE.
C
C     Set parameters for horizontal or vertical
      IF (HOR) THEN
          CELLDX=GTWLL*TEXW
          CELLDY=0
          MENDX=GTWPL*(CELLDX+1)
          MENDY=TEXH+5
      ELSE
          CELLDX=0
          CELLDY=TEXH+5
          MENDX=GTWLL*TEXW+1
          MENDY=GTWPL*(TEXH+5)+1
      END IF
C
C     Set the window limits on screen
      GML(GTWID,1)=GTWOX
      GML(GTWID,2)=GTWOY
      GML(GTWID,3)=MENDX
      GML(GTWID,4)=MENDY
      GML(GTWID,5)=GTWLL
      GML(GTWID,6)=GTWPL
C
C
C     Set the cursor position to start of input line in window
      GMC(GTWID,1)=GML(GTWID,1)
      GMC(GTWID,2)=GTWOY+3
      GMC(GTWID,3)=GML(GTWID,5)*TEXW-1
      GMC(GTWID,4)=TEXH+5
      GMC(GTWID,5)=CELLDY
      GMC(GTWID,6)=CELLDX
C
C     Set up the cell bitmap for raster ops on the menu
C
C     Set the bitmap size
      BMSIZ(1)=GML(GTWID,5)*TEXW+2
      BMSIZ(2)=TEXH+4
C     Save bitmap size in common area
      GTCBMS(GTWID,1)=BMSIZ(1)
      GTCBMS(GTWID,2)=BMSIZ(2)
C
C
C     Clear the window area
C
      GW = GTWID
C     paint in the menu and frame
      CALL DAXMPAINT(GW)
C
C     Enable the window
      GMA(GTWID)=.TRUE.
C
      RETURN
 
 999  CALL ERROR_$PRINT(ST)
      STOP
 
      END
C
      SUBROUTINE GTIWIN(GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4,GTWOO)
C     ===========================================================
C1    vartype             I4     I4     I4     I4     I4     L
C1    iostatus            I      I      I      I      I      I
C2
C2    Formerly GTXT_INIT_WIN,this routine initializes a text window
C2    on the graphics screen,positioned with top left corner at apollo
C2    screen coords GTWOX4,GTWOY4.The identifier for the window to be
C2    created is GTWID, an integer in the range 1-4 at present. The
C2    number of chatracters in each line is to be GTWLL4,with a page
C2    length of GTWPL4 lines. The final argument GTWOO is a logical
C2    flag set to .TRUE. if the window should be configured for output
C2    only,set to .FALSE. if a normal input/output window is required.
C2
C
      include 'include/gpr.ins.inc'
      include 'include/gtxt1.inc'
      include 'include/dtext.inc'
      include 'include/interface.inc'
C
      INTEGER*2 GTWID,GTWOX,GTWOY,
     +          GTWLL,GTWPL,
     +          LX(1:8),LY(1:8), ONE

      REAL LX1,LY1,LX2,LY2
      INTEGER*4 ST,GTWID4,GTWOX4,GTWOY4,GTWLL4,GTWPL4
      LOGICAL UNOBSCURED,GTWOO
C
C     convert all passed integers to I*2
      ONE = 1
      GTWID=GTWID4
      GTWOX=GTWOX4
      GTWOY=GTWOY4
      GTWLL=GTWLL4
      GTWPL=GTWPL4
C     Set the window limits on screen
      GWL(GTWID,1)=GTWOX
      GWL(GTWID,2)=GTWOY
      GWL(GTWID,3)=GTWLL*TEXW-1
      GWL(GTWID,4)=GTWPL*(TEXH+2)+9
      GWL(GTWID,5)=MIN(INT(GTWLL),LEN(GWLINE(GTWID)))
      GWL(GTWID,6)=GTWPL
C
C     Set the position for drawing of the window border
      LX(1)=GWL(GTWID,1)-3
      LY(1)=GWL(GTWID,2)-(TEXH+3)
      LX(2)=LX(1)+GWL(GTWID,3)+3
      LY(2)=LY(1)+GWL(GTWID,4)+8
C
C     Set the cursor position to start of input line in window
      GWC(GTWID,1)= GWL(GTWID,1)
      GWC(GTWID,2)= (GTWPL-1)*(TEXH+3)+GTWOY
      GWC(GTWID,3)= GWL(GTWID,1)
      GWC(GTWID,4)= GWC(GTWID,2)
      GWC(GTWID,5)= 1
C
C     Set pointer to start of buffer
      GWBP(GTWID)=0
C
C     Clear input buffer
      GWLINE(GTWID)=GNLINE
C
C     Clear the window area
CCCCC      CALL GTCLRW(GTWID4)
C
      CALL TOOLPEN(MENUF)
      CALL DRSREC(LX(1),LY(1),LX(2),LY(2))
      CALL DRSREC(LX(1)-ONE,LY(1)-ONE,LX(2)+ONE,LY(2)+ONE)
C
C     Enable the window
      GWA(GTWID,1)=.TRUE.
      GWA(GTWID,2)=GTWOO
C
      END
C
C-----------------------------------------------------------------


      SUBROUTINE DAXMPAINT(GTWID)
C     ===========================
C1    VARYPE                I4  
C1    IOSTAT                I 
C
C2    Paint a daxcad menu ( noun/verb etc ) into position
C2    All values previously calculated in GTIMEN
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    GTWID	->	Window number 
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/drgbar.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
C
      INTEGER*2  LX(8),LY(8)
      INTEGER*2  WIN(4)
      INTEGER*4 GTWID
      INTEGER*4 ST
      INTEGER*4 i
      LOGICAL UNOBSCURED
C
C     Set the position for drawing of the window border
C
      LX(1)=GML(GTWID,1)-2
      LY(1)=GML(GTWID,2)-TEXH+1
      LX(2)=LX(1)+GML(GTWID,3)+2
      LY(2)=LY(1)+GML(GTWID,4)+1

      IF(GTWID.EQ.2) THEN
C         save menu 2 coords for repainting progress bar
          BRXMAX=LX(2)-1
          BRXMIN=LX(1)+1
          BRYMAX=LY(2)-1
          BRYMIN=LY(1)+1
      ENDIF
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)

      CALL TOOLPEN(MENUB)
      WIN(1) = LX(1)
      WIN(2) = LY(1)
      WIN(3) = LX(2)-LX(1)+1
      WIN(4) = LY(2)-LY(1)+1
      CALL GPR_$RECTANGLE(WIN,ST)

      CALL TOOLPEN(MENUF)
C     Set up to draw frame around menu area
      DO 10 I = 1,2
          CALL DRSREC(LX(1),LY(1),LX(2),LY(2))
          LX(1) = LX(1) - 1
          LY(1) = LY(1) - 1
          LX(2) = LX(2) + 1
          LY(2) = LY(2) + 1
 10   CONTINUE 
C
      CALL GPR_$RELEASE_DISPLAY(ST)

      END

