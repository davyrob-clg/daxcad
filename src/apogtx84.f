C
C     @(#)  412.1 date 6/11/92 apogtx84.f 
C
C
C     Filename    : apogtx84.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:29:37
C     Last change : 92/06/11 14:23:20
C
C     Copyright : Practical Technology International Limited  
C     File :- apogtx84.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GTCLRC(GTWID4,GTCID4)
C     SUBROUTINE GTCLRC1(GTWID4,GTCID4)
C     SUBROUTINE GTCLRM(GTWID4)
C     SUBROUTINE GTCLRW(GTWID4)
C     SUBROUTINE GTIVMC(GTWID4,GTCID4)
C     SUBROUTINE GTSCIW(GTWID4)
C     SUBROUTINE GTSCRW(GTWID4)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE GTCLRC(GTWID4,GTCID4)
C     ================================
C1    vartype              I4     I4
C1    iostatus             I      I
C2    Formerly GTXT_CLEAR_MENU_CELL clears cell number
C2    GTCID4 in menu number GTWID4 to background colour,
C2    normally black.Also clears the logical flag in the
C2    associated array to indicate video status.
C
      include 'include/gtxt1.inc'
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/macro.inc'
      include 'include/interface.inc'
C
      REAL X1,Y1,X2,Y2,TMP
      INTEGER*2 BMWIND(2,2),DISPOS(2)
      INTEGER*4 ST,ADESC,MDESC,GTWID4,GTCID4,OPT,GTCID,ONE
      EXTERNAL CLEARW
C
C     convert from passed
C     I*4 to internal I*2 for apollo use
C
      CALL TOOLPEN(MENUB)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
      ONE=1
      GTCID=GTCID4-ONE
      IF ( MENUS ) THEN
C         DISPOS(1)=GML(GTWID4,1)+(GTCID4-1)*GMC(GTWID4,6)-1
C         DISPOS(2)=(GTCID4-1)*GMC(GTWID4,5)+GML(GTWID4,2)-(TEXH-3)
         X1=GMC(GTWID4,1)-ONE+(GTCID)*GMC(GTWID4,6)
         Y1=GMC(GTWID4,2)+(GTCID)*GMC(GTWID4,5)+4
         X2=X1+GMC(GTWID4,3)+3
         Y2=Y1-GMC(GTWID4,4)
C        clear cell,remember to reverse order of y-values
         CALL CLEARW(X1,Y2,X2,Y1)
C        set raster op to complement of source
C        ensure menu text buffer is cleared in the correct
C        position. note the offset of 1 between screen cell
C        and buffer cell.
 
      END IF
C
      GMB(GTWID4,GTCID)=GNLINE
      GMBC(GTWID4,GTCID)=' '
C     set flag in common block to indicate inverse status
      GMCINV(GTWID4,GTCID)=.FALSE.
 
C     reset multiple cell flag.
      GMCMUL(GTWID4,GTCID)=.FALSE.
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 gtclrc1.ftn Daxcad revision 1.19
      SUBROUTINE GTCLRC1(GTWID4,GTCID4)
C     ================================
C1    vartype              I4     I4
C1    iostatus             I      I
C2    Formerly GTXT_CLEAR_MENU_CELL clears cell number
C2    GTCID4 in menu number GTWID4 to background colour,
C2    normally black.Also clears the logical flag in the
C2    associated array to indicate video status.
C
      include 'include/gtxt1.inc'
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/macro.inc'
      include 'include/interface.inc'
C
      REAL X1,Y1,X2,Y2,TMP
      INTEGER*2 BMWIND(2,2),DISPOS(2)
      INTEGER*4 ST,ADESC,MDESC,GTWID4,GTCID4,OPT,GTCID,ONE
      EXTERNAL CLEARW
C
C     convert from passed
C     I*4 to internal I*2 for apollo use
C
      CALL TOOLPEN(MENUB)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
      ONE=1
      GTCID=GTCID4-ONE
      IF ( MENUS ) THEN
C           find the descriptor for bitmap
            MDESC=GTCMDS(GTWID4)
C           Set the cell size for BLT operation
            BMWIND(1,1)=0
            BMWIND(2,1)=0
            BMWIND(1,2)=GTCBMS(GTWID4,1)
            BMWIND(2,2)=GTCBMS(GTWID4,2)
C           set the origin in the display for the BLT
C           find position for bitmap in display
            DISPOS(1)=GML(GTWID4,1)+(GTCID-1)*GMC(GTWID4,6)-1
            DISPOS(2)=(GTCID-1)*GMC(GTWID4,5)+GML(GTWID4,2)-(TEXH-3)
C           set raster op to exclusive or
            OPT=0
            CALL RASTER(OPT)
C           do the BLT
            CALL BITMAP(MDESC,BMWIND,DISPOS)
C           Reset raster op to default
            CALL ROPREP()
C           set flag in common block to indicate inverse status
      END IF
C
      GMB(GTWID4,GTCID)=GNLINE
      GMBC(GTWID4,GTCID)=' '
C     set flag in common block to indicate inverse status
      GMCINV(GTWID4,GTCID)=.FALSE.
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 gtclrm.ftn Daxcad revision 1.19
      SUBROUTINE GTCLRM(GTWID4)
C     ========================
C1    vartype             I4
C1    iostatus            I
C2
C2    Formerly GTXT_CLEAR_MENU.
C2    This routine clears the menu number GTWID4 to
C2    background value,normally black.
C
C
      include 'include/gtxt2.inc'
C
      INTEGER*4 GTWID4,I,PL,IZERO
C
      EXTERNAL GTCLRC, GTCLVN
C
C     test for range of menu ID number
      IZERO=0
      I=4
      IF (GTWID4.GT.IZERO .AND. GTWID4.LE.I) THEN
C     test for active menu
         IF (GMA(GTWID4)) THEN
C     find the page length
            PL=GML(GTWID4,6)
C     clear each cell in turn
            DO 20 I=1,PL
               CALL GTCLRC(GTWID4,I)
C              clear the verb number storage cells
               CALL GTCLVN(GTWID4, I)                
 20         CONTINUE
         END IF
      END IF
C
      END
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 gtclrw.ftn Daxcad revision 1.19
      SUBROUTINE GTCLRW(GTWID4)
C     ========================
C1    vartype             I4
C1    iostatus            I
C2
C2    Formerly GTXT_CLEAR_WIN.
C2    This routine clears the text window number GTWID4 to
C2    background value,normally black.
C
C
      include 'include/gtxt1.inc'
      include 'include/dtext.inc'
      include 'include/interface.inc'
C
      INTEGER*2 TWLIMS(4)
      INTEGER*2 GW
      INTEGER*4 ST,GTWID4
C
      EXTERNAL CLEARW
C
      CALL TOOLPEN(MENUB)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
      GW=GTWID4
      IF (GWA(GW,1)) THEN
C        set the window limits for block fill
         TWLIMS(1)=GWL(GW,1)-1
         TWLIMS(2)=GWL(GW,2)-TEXH
         TWLIMS(3)=TWLIMS(1)+GWL(GW,3)+1
         TWLIMS(4)=TWLIMS(2)+GWL(GW,4)
C        Clear the text buffer area of the screen
         CALL CLEARW(REAL(TWLIMS(1)),REAL(TWLIMS(2)),
     +               REAL(TWLIMS(3)),REAL(TWLIMS(4)))
C
      END IF
C
      END
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 gtivmc.ftn Daxcad revision 1.19
      SUBROUTINE GTIVMC(GTWID4,GTCID4)
C     ================================
C1    vartype              I4     I4
C1    iostatus             I      I
C2
C2    Formerly GTXT_MENU_INV_CELL inverts the contents
C2    of cell number GTCID4 of menu number GTWID4.
C2    Also writes the logical flag into the correct position of the
C2    logical array associated with the menu,to show the video status
C2    of the cell.
C
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
      include 'include/apollo.inc'
C
      REAL X1,Y1,X2,Y2
      INTEGER*2 GTWID,GTCID
      INTEGER*2 WIN(4)
      INTEGER*2 BMWIND(2,2),DISPOS(2)
      INTEGER*4 ST,GTWID4,GTCID4,OPT
      INTEGER*4 COL
      EXTERNAL ROPXOR,ROPREP
C
      CALL TOOLPEN(MENUF)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
C     convert from passed I*4 to internal i*2 for apollo use
      GTWID=GTWID4
      GTCID=GTCID4
C     check for valid arguments
      IF (GTWID.GT.4 .OR. GTWID.LE.0) RETURN
      IF (GMA(GTWID)) THEN
         IF (GML(GTWID,6).GE.GTCID .AND.
     +       GTCID.GT.0) THEN
C
            WIN(3) = GTCBMS(GTWID,1)
            WIN(4) = GTCBMS(GTWID,2) + 1
C           set the origin in the display for the BLT
C           find position for bitmap in display
            WIN(1) = GML(GTWID,1)+(GTCID-1)*GMC(GTWID,6)-1
            WIN(2) = (GTCID-1)*GMC(GTWID,5)+GML(GTWID,2)-(TEXH-3) - 1
C           set raster op to exclusive or
            IF ( .NOT.(GMCMUL(GTWID,GTCID-1).AND.HIPLAN.GT.0) ) THEN
                CALL ROPXOR()
C               do the BLT
                CALL GPR_$RECTANGLE(WIN,ST)
C               Reset raster op to default
                CALL ROPREP()
            ENDIF
C           set flag in common block to indicate inverse status
            GMCINV(GTWID,GTCID-1)=.NOT.GMCINV(GTWID,GTCID-1)
C
         END IF
      END IF
C
      END
C
C-----------------------------------------------------------------
C
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 gtsciw.ftn Daxcad revision 1.19
      SUBROUTINE GTSCIW(GTWID4)
C     ========================
C1    vartype              I4
C1    iostatus             I
C2
C2    Formerly GTXT_SCROLL_INV_WIN.
C2    This routine scrolls window number GTWID4 in inverse video.
C
      include 'include/gtxt1.inc'
      include 'include/interface.inc'
C
      REAL X,Y
      INTEGER*2 PAGLEN,LINLEN2,NEWLIN,CURPOS(2),MOD,I,J,OLDLIN
      INTEGER*2 GTWID
      INTEGER*4 GTWID4,J4
      INTEGER*4 ST
      LOGICAL UNOBSCURED
      EXTERNAL GTSPLN
C
      CALL TOOLPEN(MENUF)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C     Set the basic working parameters
C
      OLDLIN=GWBP(GTWID)
      PAGLEN=GWL(GTWID,6)
      NEWLIN=MOD(INT(OLDLIN+1),INT(PAGLEN))
      GWBP(GTWID)=NEWLIN
      NEWLIN=GWBP(GTWID)
C
      CALL GTCLRW(GTWID4)
C
C     Set the cursor position to start of top line of window
      X=REAL(GWL(GTWID,1))
      Y=REAL(GWL(GTWID,2))
C
      IF (GWA(GTWID,2)) THEN
C
C        Copy input line into circular buffer
         GWB(GTWID,NEWLIN)=GWLINE(GTWID)
C        The window is set for output only
      ELSE
C
C     Copy input line into circular buffer
         GWB(GTWID,OLDLIN)=GWLINE(GTWID)
         GWB(GTWID,NEWLIN)=GNLINE
      END IF
C
C     Write the buffer to the screen
      DO 200 I=1,PAGLEN-1
         J=MOD(NEWLIN+I,PAGLEN)
         J4=J
         CALL GTSPLN(X,Y,GWB(GTWID4,J4),.FALSE.)
 200  CONTINUE
C
      J=MOD(NEWLIN+I,PAGLEN)
      J4=J
      CALL GTSPLN(X,Y,GWB(GTWID4,J4),.TRUE.)
C
C     Set the cursor position to start of input line of window
C
C     Retrieve starting position
      CURPOS(1)=GWC(GTWID,3)
      CURPOS(2)=GWC(GTWID,4)
C     Update current position
      GWC(GTWID,1)=CURPOS(1)
      GWC(GTWID,2)=CURPOS(2)
C     Reset character pointer
      GWC(GTWID,5)=1
C     Now actually set cursor in proper position
C
      END
C
C-----------------------------------------------------------------
C       @(#)  256.1 date 12/16/89 gtscrw.ftn Daxcad revision 1.19
      SUBROUTINE GTSCRW(GTWID4)
C     ========================
C1    vartype             I4
C1    iostatus            I
C2
C2    Formerly GTXT_SCROLL_WIN scrolls the text in window number GTWID4
C2    up by one line.
C
      include 'include/gtxt1.inc'
      include 'include/interface.inc'
C
C
      REAL X,Y
      INTEGER*2 PAGLEN,LINLEN2,NEWLIN,CURPOS(2),MOD,I,J,OLDLIN
      INTEGER*2 GTWID
      INTEGER*4 GTWID4,J4
      INTEGER*4 ST
      EXTERNAL GTSPLN,MOVES,STCURP,GTCLRW
C
      CALL TOOLPEN(MENUF)
      CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
C     convert passed integers to i*2 for apollo
      GTWID=GTWID4
C     Set the basic working parameters
C
      OLDLIN=GWBP(GTWID)
      PAGLEN=GWL(GTWID,6)
      NEWLIN=MOD(INT(OLDLIN+1),INT(PAGLEN))
      GWBP(GTWID)=NEWLIN
      NEWLIN=GWBP(GTWID)
C
C
      CALL GTCLRW(GTWID4)
C
C     Set the cursor position to start of top line of window
      X=REAL(GWL(GTWID,1))
      Y=REAL(GWL(GTWID,2))
C
      IF (GWA(GTWID,2)) THEN
C        Copy input line into circular buffer
         GWB(GTWID,NEWLIN)=GWLINE(GTWID)
C        The window is set for output only
      ELSE
C        Copy input line into circular buffer
         GWB(GTWID,OLDLIN)=GWLINE(GTWID)
         GWB(GTWID,NEWLIN)=GNLINE
      END IF
C
C     Write the buffer to the screen
          DO 200 I=1,PAGLEN
             J=MOD(NEWLIN+I,PAGLEN)
             J4=J
             CALL GTSPLN(X,Y,GWB(GTWID4,J4),.FALSE.)
 200       CONTINUE
C
C     Set the cursor position to start of input line of window
C
C     Retrieve starting position
      CURPOS(1)=GWC(GTWID,3)
      CURPOS(2)=GWC(GTWID,4)
C     Update current position
      GWC(GTWID,1)=CURPOS(1)
      GWC(GTWID,2)=CURPOS(2)
C     Reset character pointer
      GWC(GTWID,5)=1
C     Now actually set cursor in proper position
C
      END
C
C-----------------------------------------------------------------
C
