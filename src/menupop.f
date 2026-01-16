C
C     @(#)  412.1 date 6/11/92 menupop.f 
C
C
C     Filename    : menupop.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:40:33
C     Last change : 92/06/11 14:35:18
C
C     Copyright : Practical Technology International Limited  
C     File :- menupop.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CURSOR_POINTER(APX,APY)
C     SUBROUTINE DRAWPT(APX,APY)
C     SUBROUTINE INTPOP()
C     SUBROUTINE MENPOP(MNCODE,OK)
C     SUBROUTINE MENPOP_CELL(XP,YP,CELL,POS)
C     SUBROUTINE MENPOP_CONFIG(FILNAM, ST)
C     SUBROUTINE MENPOP_INIT_GR(REPAINT,ST)
C     SUBROUTINE MENPOP_HIGHLIGHT(TOKEN,OK)
C     SUBROUTINE MENPOP_LOAD(CODE,OK)
C     SUBROUTINE MENPOP_PAINT(MNCODE,X,Y,PAINT)
C     SUBROUTINE MENPOP_SET_BACKGROUND(GRY)
C     SUBROUTINE PAINT_LINE(X1,Y1,X2,Y2,INDX)
C
C     |-----------------------------------------------------------------|
C
C
C
C
      SUBROUTINE DRAWPT(APX,APY)
C     ==========================
C1    VARTPYE            I2 I2
C1    IOSTAT             I  I
C
C2    Draws a point at specified pixel ( XVERSION COMPATIBLE )
C
C
      include  'include/daxcad_x.inc'
C
      INTEGER*2 APX,APY
      INTEGER*4 ST
C
      IF(XVERSION) THEN
C         draw a single point into pixmap
          CALL GPR_$POINT(APX,APY,ST)
      ELSE
C         do normal gpr move/line draw
          CALL GPR_$MOVE(APX,APY,ST)
          CALL GPR_$LINE(APX,APY,ST)
      ENDIF
      END
C
C-----------------------------------------------------------------
 

      SUBROUTINE INTPOP()
C     =====================
C1    VARYPE       NONE
C1    IOSTAT            
C
C2    function calls MENPOP_CONFIG to load
C2    the popup menu data from the file daxcad.menu
C2    looking first in the home directory and then
C2    in the daxcad directory
C2
C2    Arguments:-NONE
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C
      include'include/product.inc'
      include'include/library.inc'
      include'include/menpop.inc'
C
      INTEGER*4 ST,NLEN
      CHARACTER*80 FILNAM
      EXTERNAL MENPOP_CONFIG, EXIT
C
C
C*****************************************************
C            POPUP MENUS FILE
C*****************************************************
C
      FILNAM=PRNAM(1:NLEN(PRNAM))//'.menu.config'
      CALL FPARS(FILNAM,ST)
C
      CALL  MENPOP_CONFIG(FILNAM, ST)
      IF (ST.NE.0) THEN
C        failure in loading token file
         WRITE(UNIT=*,FMT=*)' Cannot load popup system file '
         CALL PGM_$EXIT()
      END IF
C
C     addition for repainting. Stores last popup number
      LSTMNC = 0
C
      END
C
C--------------------------------------------------------------
C
      SUBROUTINE MENPOP(MNCODE,OK)
C     ============================
C1    VARTYPE            I4   L
C1    IOSTAT             I    O
C
C2    This rotine will control the POPUP menus from DAXCAD.
C2    called from TCURS when a MULTIPLE cell is called and
C2    returns the TOKEN from the POPPED cell
C
      include 'include/apollo.inc'
      include 'include/menun.inc'
      include 'include/menpop.inc'
      include 'include/macro.inc'
      include 'include/journal.inc'
C
      LOGICAL OK
      REAL X,Y
      INTEGER*2  XP,YP
      INTEGER*4 MNCODE,TCELL,TMEN
      CHARACTER TCCMD*1,TOKEN*1
      EXTERNAL WRTJRN,MAC101
C
C 
      IF(MACOP .AND. MNCODE .NE. 5) THEN
C       right the MACRO is running and the man
C       does not want the points mode popup
C       Flag the macro that it is being called
C       from the popup code
 10     CONTINUE
        MACPOP = .TRUE.
        CALL MAC101(X,Y,OK)
C       turn off the MACRO  popup flag
        MACPOP = .FALSE.
C       let's just check that we got a popup menu cell
C       and not the repeat of the header cell i.e. declined
C       to hit a popup menu cell while journaling then issued
C       the same command again
        IF(POPHDR(MNCODE) .EQ. VNCCMD) GOTO 10
C
C       the MACRO may wish to go again so check
        IF(CCMD .EQ. 'q') OK = .FALSE.        
        RETURN
      ENDIF
C     set coords as I2 from global cursor position
      XP = APCURX(1)
      YP = APCURY(3)
C     save the current hit values
      TCELL =CELLN
      TMEN = MEN
      TCCMD = CCMD
C
C     load up the popup and wait for a return
      CALL MENPOP_LOAD(MNCODE,OK)
      IF(OK)THEN
C        User definable cell s possible
         LSTMNC = MNCODE
         CALL MENPOP_PAINT(MNCODE,XP,YP,.TRUE.)
C        get menu hit
         CALL MENPOP_HIGHLIGHT(TOKEN,OK)
         CALL MENPOP_PAINT(MNCODE,XP,YP,.FALSE.)
         LSTMNC = 0
C        Last cell and noun variable clear
         LSTCEL = 0
         LSTNOU = 0
         IF(OK) THEN
           CCMD =  TOKEN
           LSTCEL = CELLN-1
           IF ( MNCODE.GT.0) THEN
C             Some popups donthave attached nouns
              LSTNOU = MNPNTR(1,MNCODE)+LSTCEL-1
              LSTNOU = MNPCEL(LSTNOU)
           ENDIF
C          stick out a journaling entry if required
           IF(JOURON) THEN
C
             IF(TMEN .EQ. 3) THEN
               CALL WRTJRN(0.0,0.0,'p',DAXMNU(CELLN),0)
             ENDIF
C
             IF(TMEN .EQ. 4 .AND. DISLAY) THEN
               CALL WRTJRN(0.0,0.0,'p',DAXMNU(CELLN),0)
             ENDIF
C
           ENDIF
           CELLN = TCELL
           MEN = TMEN
         ENDIF
      ENDIF
 
      END
C
      SUBROUTINE MENPOP_CELL(XP,YP,CELL,POS)
C     ======================================
C1    VARTYPE                I2 I2  I4   I2(2)
C1    STATUS                 I   I  O    O
C
C2    Fuctions return the cell number of the current menu
C2    based on a current postion passed
C2  
C2    Arguments:-
C2  
C2    XP          ->          Current X cursor position 
C2    YP          ->          Current Y cursor position 
C2    CELL        ->          The cell number that the cursor occupies
C2    POS         ->          The actual X,Y position of the cell. Height
C2                            and Width are calculated elsewhere
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  

C
      include  'include/menpop.inc'
C
      REAL FACTX,FACTY
      INTEGER*2 XP,YP,TEMPX,TEMPY,POS(2),COLS,ROWS
      INTEGER*4 CELL
C
      TEMPY = YP-STARTY
      FACTY = (REAL(TEMPY)/REAL(BITMH-(2*BLKBDR)-SHADOW))
      IF(NUMCOL.EQ.0) THEN
C        A simple, 1 column menu.
         CELL = FACTY*MNTOT
         TEMPX = 0
         TEMPY = (CELL*(TEXTH+LNSPC)) + 1
      ELSE
C        A multiple column menu.
         TEMPX = XP-STARTX
         FACTX = (REAL(TEMPX)/REAL(BITMW-(2*BLKBDR)-SHADOW))
         COLS = 1 + FACTX*NUMCOL
C        Number of rows AFTER the banner.
         ROWS = 1 + FACTY*( ((MNTOT-1)/NUMCOL) + 1)
C        Cell number.
         CELL =((ROWS-2)*NUMCOL) + COLS
C        Find the bottom left hand corner of the cell.
         TEMPX = (COLS-1)*((TEXTW*MAXLN)+LGAP+RGAP)
         TEMPY = (ROWS-1)*(TEXTH+LNSPC) + TGAP
      ENDIF
      POS(1) = STARTX+TEMPX
      POS(2) = STARTY+TEMPY
      END
C
      SUBROUTINE MENPOP_CONFIG(FILNAM, ST)
C     =====================================
C1    VARTYPE                   C*(*)  I*4    
C1    IOSTAT                     I      O       
C
C2    Load up the popup menu configurations
C2
      include 'include/gpr.ins.inc'
C
      include 'include/apollo.inc'
      include 'include/dtext.inc'
      include 'include/vntable.inc'
      include 'include/style.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
      include 'include/menpop.inc'
C
      LOGICAL OK,EX,UNOBSCURED
      INTEGER*2 SORG(2),BMSIZ(2),ASCI,NUMB,TNUM
      INTEGER*4 MENUNT,I,ST,ADESC,AMDESC,BDESC,CDESC,TOT,NLEN,NUM
      CHARACTER*80 FILNAM*(*),INBUF*25,TOKEN*2,STRING*20,INTFIL
      CHARACTER*80 TEMPS*80
      INTEGER*4 CODE,STARTP,ENDP,SP,GREY,TEMPI,COL
C
      EXTERNAL NLEN
C
C
C     set varibles
      OK = .FALSE.
      I = 0
      MAXCOL = 1
C
C     set tot values for each menu
      MNTOT=0
C     initialise VNMULT which should contain 0
C     if the menu cell has no popup and popup
C     code if it has
      DO 30 I=1,VNMAX
        VNMULT(I)=0
 30   CONTINUE
C     initialise the list of popup header menu
C     cells
      DO 40 I= -POPLIM,POPLIM
        POPHDR(I) = 0
 40   CONTINUE
C     *****************************
C      special popup data
C     *****************************
C
C     line thickness header
      POPHDR(-3) = 409
      VNMULT(409) = -3
C     line font  header
      POPHDR(-1) = 407
      VNMULT(407) = -1
C     Plotter type  header
      POPHDR(-2) = 311
      VNMULT(311) = -2
C      
C     test existance of menu files
      INQUIRE(FILE=FILNAM,EXIST=EX)
C
      IF(.NOT.EX) THEN
          ST = 2
          GOTO 999
      ENDIF
      CALL FINDU1(MENUNT,OK)
      IF(.NOT. OK) THEN
C         set status to indicate no units available
          ST=3
          RETURN
      END IF
C
      ST = 1
      OPEN(UNIT=MENUNT,FILE=FILNAM,STATUS='OLD',
     +     ACCESS='SEQUENTIAL',ERR=998)
 20   CONTINUE
C     get a line from the file
      READ(UNIT=MENUNT,FMT='(A)',END=10, ERR=999) INBUF
C
C     Blank line we dont need
      IF(NLEN(INBUF).EQ.0) THEN
          GOTO 20
      ENDIF
      TEMPS = INBUF
      CALL CRUNCH(INBUF)
C     Deal With all comments
      ASCI = INDEX(INBUF,'*')
      IF(ASCI.GT.1) THEN
C         Comment at the end of a line
          STRING=INBUF(:ASCI-1)
      ELSEIF(ASCI.EQ.0) THEN
C         no comment
          STRING=INBUF
      ELSE
C         coment line ignore it
          GOTO 20
      ENDIF
C     folup the string to avoid confusion
      CALL FOLDUP(STRING)
C     start of string then mark it
      IF(STRING(1:5).EQ.'BEGIN') THEN
C
          CALL IVALU(STRING(6:),CODE,OK)
          IF(.NOT.OK) THEN
              GOTO 999
          ENDIF
C
          IF(CODE.LT.1.OR.CODE.GT. POPLIM) THEN
             GOTO 999
          ENDIF
          STARTP = MNTOT+1
          MNPNTR(1,CODE) = STARTP
          MNPNTR(3,CODE) = 0
          DAXBNR(CODE) = ' '
C
      ELSEIF(STRING(1:3).EQ.'END') THEN
C         end of the string mark the end values
          MNPNTR(2,CODE) = MNTOT
C
      ELSEIF(STRING(1:6).EQ.'BANNER') THEN
C         Banner for Head of POPUP
          CALL IVALU(STRING(7:),TEMPI,OK)
          IF(.NOT.OK) THEN
              GOTO 999
          ENDIF
C
          IF(TEMPI.LT.1 .OR. TEMPI.GT.VNMAX) THEN
              GOTO 999
          ENDIF
          DAXBNR(CODE) = VNOUN(TEMPI)
C
      ELSEIF(STRING(1:7).EQ.'COLUMNS') THEN
C         Number of columns to split cells into
C         NOTE: Order is      Cell1   Cell2
C                             Cell3   Cell4
          CALL IVALU(STRING(8:),TEMPI,OK)
          IF(.NOT.OK) THEN
              GOTO 999
          ENDIF
          IF(TEMPI.LT.0.OR.CODE.GT.MAXCEL) THEN
              GOTO 999
          ENDIF
          MNPNTR(3,CODE) = TEMPI
          IF(TEMPI.GT.MAXCOL) MAXCOL=TEMPI
C
      ELSEIF(STRING(1:6).EQ.'HEADER') THEN
C         get the vnum of the header menu cell
          CALL IVALU(STRING(7:),TEMPI,OK)
          IF(.NOT.OK) THEN
              GOTO 999
          ENDIF
          IF(TEMPI.LT.1.OR.TEMPI.GT.VNMAX) THEN
              GOTO 999
          ENDIF
          VNMULT(TEMPI) = CODE
C         stick the header in the list
          POPHDR(CODE) = TEMPI          
C
      ELSE
C         an ordinary number
          CALL IVALU(STRING,NUM,OK)
          IF(NUM.LT.1.OR.NUM.GT.VNMAX) THEN
              GOTO 999
          ENDIF
          MNTOT = MNTOT+1
          MNPCEL(MNTOT) = NUM
C         attach popup code to the menu cell
          VNMULT(NUM) = CODE          
C
      ENDIF
C     loop back
      GOTO 20

 10   CONTINUE
C     close the file
C     ************************
C     End of POPUP CONGURATION
C     ************************
C
C     Initalise GR code
      CALL MENPOP_INIT_GR(.FALSE.,ST)
C
      ST = 0
      GOTO 999

998   CONTINUE
C     File is not open
      ST = 1
      RETURN
999   CONTINUE
      CLOSE(UNIT=MENUNT, STATUS = 'KEEP')

      END
C
      SUBROUTINE MENPOP_INIT_GR(REPAINT,ST)
C     =====================================
C1    VARYPE                       L    I4
C1    IOSTAT                       I    O
C
C2    Initalise the Graphics side of popups. Needed for Repaint of DAXCAD
C2    Where all bitmaps must be allocated and dealocated
C2  
C2    Arguments:-
C2  
C2    REPAINT     ->      Indicates that a repaint must takeplace
C2  
C2    Error Returns:
C2  
C2  
      include 'include/gpr.ins.inc'
      include 'include/apollo.inc'
      include 'include/dtext.inc'
      include 'include/menpop.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
      include 'include/interface.inc'
C
      INTEGER*4 ST
      LOGICAL REPAINT
      LOGICAL UNOBSCURED
      INTEGER*2 SORG(2)
      INTEGER*2 BMSIZ(2)
      INTEGER*2 WINDOW(4)
      INTEGER*2 ASCI
      INTEGER*2 NUMB
      INTEGER*2 TNUM
      INTEGER*4 ADESC
      INTEGER*4 AMDESC
      INTEGER*4 BDESC
      INTEGER*4 CDESC
      INTEGER*4 GREY
      INTEGER*4 COL
C
C     get text extents
      IF ( REPAINT ) THEN
C         Dealocate if repainting
          CALL GPR_$DEALLOCATE_BITMAP(SWBITM,ST)
          CALL GPR_$DEALLOCATE_BITMAP(MUBITM,ST)
      ELSE
C         grey background only occurs once.
          GREY = 3
          CALL MENPOP_SET_BACKGROUND(GREY)
      ENDIF
C
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
      TNUM =1
      CALL GPR_$INQ_TEXT_EXTENT('M',TNUM,SORG,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
      TEXTW=SORG(1)+2
      TEXTH=SORG(2)
C
C     make sure maximun bitmap heights are used
      BMSIZ(1) = (TEXTW*MAXCHR*MAXCOL) + HBORDR
      BMSIZ(2) = (TEXTH+LNSPC)*MAXCEL + VBORDR
C     allocate a bitmap for the menu and for saving current
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(ADESC,ST)
C       
C     Allocate a bit map in memory
      CALL GPR_$ALLOCATE_BITMAP(BMSIZ,HIPLN,ADESC,SWBITM,ST)
C
C     set and allocate the shaded marker for toggle cells
C     defining a bitmap for the cells cos kirks wont do
      BMSIZ(1)=MAXCOL*TEXTW*MAXCHR
      BMSIZ(2)=TEXH+LNSPC
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY (ST)
C     Allocate an attribute block for the bitmap
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(AMDESC,ST)
C
C     Allocate a bit map in memory
      CALL GPR_$ALLOCATE_BITMAP(BMSIZ,HIPLN,AMDESC,MUBITM,ST)
C     set curent bitmap for clearing
      CALL GPR_$SET_BITMAP(MUBITM,ST)
C
      CALL ROPREP()
      CALL TOOLPEN(MENUF)
C
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = BMSIZE(1)
      WINDOW(4) = BMSIZE(2)
C     white it out
      CALL GPR_$RECTANGLE(WINDOW,ST)
C     reset bitmap to main display
      CALL GPR_$SET_BITMAP(DISPDE,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      END
C                
C---------------------------------------------------------------
C
      SUBROUTINE MENPOP_HIGHLIGHT(TOKEN,OK)
C     =====================================
C1    VARYPE                       C    L
C1    IOSTAT                       O    O
C
C2    This routine will higlight the popup menu and return
C2    the token associated  with this cell
C2    This routine should operate in the same fashion as APCURS
C2  
C2    Arguments:-
C2  
C2    TOKEN       ->          The token formt the cell.
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK .TRUE. means a cell was hit.
C2  
C2  
C
      include 'include/gpr.ins.inc'
C
      include 'include/apollo.inc'
      include 'include/gtxt2.inc'
      include 'include/dtext.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/datatype.inc'
      include 'include/menpop.inc'
      include 'include/dig2wo.inc'
      include 'include/macro.inc'
      include 'include/menun.inc'
      include 'include/curwin.inc'
      include 'include/daxcad_x.inc'
      include 'include/interface.inc'
C
      REAL FACT
      INTEGER*2 BMWIND(2,2),DISPOS(2),CURSX,CURSY
      INTEGER*4 ADESC,MDESC,GTWID4,GTCID4,OPT
      INTEGER*4 X,Y,XC,YC,ABX,ABY,TCELL,TMEN,MENKEY
      INTEGER*4 MEN1,CELLN1,CELL,OCELL
      INTEGER*2 SCRPOS(2),EVTYPE,I,ICHAR,SCPOSN(2)
      INTEGER*4 ST,LONE,LTWO,ONE,TWO,KEY,C
      INTEGER*4 SYSCUR
      INTEGER*2 TIMEC(3),SORCE,DELC,BAKC,DGB
      INTEGER*2 OLDX,OLDY
      INTEGER*2 WINDOW(4)
      LOGICAL UNOBSCURED,LWAIT,OK,MENOK,LOC,OUT,HORFLG,VERFLG
      INTEGER*2 CURSP(2),POS(2)
      CHARACTER*1 EVDATA,HITKEY,TOKEN
C
      EXTERNAL ROPXOR,ROPREP
C
C     find out current cursor postion

      CALL TOOLPEN(MENUF)
      IF ( XVERSION ) THEN
C         Turn on X wait cursor
          CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
          CALL GPR_$SET_AUTO_UPDATE(.FALSE.,ST)
      ENDIF
C     system cursor
      CALL NEW_CURSOR(0,ST)
C     cursor always starts outside popup
      OUT = .TRUE.
      OLDX = STARTX-BLKBDR
      OLDY = STARTY-BLKBDR
C     set cursor position
      CALL ROPXOR()
C     set size of cell window
      BMWIND(1,1)=0
      BMWIND(2,1)=0
      BMWIND(1,2)=(TEXTW*MAXLN)+LGAP + RGAP
      BMWIND(2,2)=TEXTH+LNSPC
      CALL TOOLPEN(MENUF)
C
      OK = .FALSE.
      CELL =-1
      OCELL =-1
 222  CONTINUE
C

      IF ( DIGIT ) THEN
        LWAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
        IF ( EVTYPE.EQ.GPR_$NO_EVENT ) THEN
           CALL MOUSE(EVTYPE,DGB,C,EVDATA,X,Y,ABX,ABY)
          IF ( EVTYPE.EQ.GPR_$NO_EVENT ) GOTO 222
C      write(10,*) 'coord',x,y,abx,aby
          SCRPOS(1)=X
          SCRPOS(2)=LIMY-Y
        END IF
      ELSE
         DGB=-1
         LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
      END IF
      IF (EVTYPE.EQ.GPR_$LOCATOR) THEN
          CALL ROPXOR()
C         find location of cursor
          CURSX=SCRPOS(1)
          CURSY=SCRPOS(2)
C         is cursor out with our little menu
          HORFLG = CURSX.LT.STARTX .OR.
     +             CURSX.GT.(STARTX+BITMW - 2*BLKBDR - SHADOW - 1)
          VERFLG = CURSY.LT.(STARTY+TEXTH + TGAP + LNSPC) .OR.
     +             CURSY.GT.(STARTY+BITMH - 2*BLKBDR - SHADOW - 1)
C         NOTE: Vertically ... we must miss the first (banner) cell.
C
          IF(HORFLG.OR.VERFLG) THEN
C             NOTE: CURSY.LT. - Misses out the top menu cell.
C                               This is assumed to be a banner.
C             if already outside then draw out old cursor
              IF(.NOT.OUT) THEN
C                 paint out the old cell
                  WINDOW(1) = DISPOS(1)
                  WINDOW(2) = DISPOS(2)
                  WINDOW(3) = BMWIND(1,2)
                  WINDOW(4) = BMWIND(2,2)
                  CALL TOOLS_RECT_INV(WINDOW,ST)
                  CELL = -1
                  OCELL = -1
                  CALL CURSOR_ON(.TRUE.)
              ENDIF
C             draw in new cursor
              CALL CURSOR_UPDATE(SCRPOS,ST)
              CALL UPDACP(SCRPOS(1),SCRPOS(2))
              OUT = .TRUE.
C
          ELSE
C             get the menu cell in the
              CALL MENPOP_CELL(CURSX,CURSY,CELL,POS)
              IF(OUT) THEN
                   CALL CURSOR_ON(.FALSE.)
              ENDIF
C             draw in new cursor
              OUT = .FALSE.
C             has the cell changed ? or is it in cell 0
              IF(CELL.NE.OCELL.AND.CELL.NE.0) THEN
C                 yup so erase the old one
                  IF(OCELL.GE.0) THEN
                      WINDOW(1) = DISPOS(1)
                      WINDOW(2) = DISPOS(2)
                      WINDOW(3) = BMWIND(1,2)
                      WINDOW(4) = BMWIND(2,2)
                      CALL TOOLS_RECT_INV(WINDOW,ST)
                  ENDIF
C                 draw the new one
                  WINDOW(1) = POS(1)
                  WINDOW(2) = POS(2)
                  WINDOW(3) = BMWIND(1,2)
                  WINDOW(4) = BMWIND(2,2)
                  CALL TOOLS_RECT_INV(WINDOW,ST)
                  DISPOS(1) = POS(1)
                  DISPOS(2) = POS(2)
                  OCELL = CELL
              ENDIF
          ENDIF
C         save current postion
          OLDX = CURSX
          OLDY = CURSY
          GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$BUTTONS) THEN
C         any buttons should take us out
          I=ICHAR(EVDATA)
C         If we get an UPSTROKE ignore it
          IF ( I.GE.65 .AND. I.LE.68 .AND.SOURCE.NE.8) GOTO 222
          IF(OUT) CALL CURSOR_ON(.FALSE.)
          IF(CELL.GT.0) THEN
              CELLN = CELL+1
              MEN = 5
              TOKEN = DAXTOK(CELLN)
              OK = .TRUE.
          ENDIF
          CALL UPDACP(CURSX,CURSY)
      ELSE
          GOTO 222
      ENDIF
C
      IF ( XVERSION ) THEN
C         Turn cursor back into normal DAXCAD cursor
          CALL GPR_$SET_CURSOR_ACTIVE(.FALSE.,ST)
          CALL GPR_$SET_AUTO_UPDATE(.TRUE.,ST)
      ENDIF
      END
C
      SUBROUTINE MENPOP_LOAD(CODE,OK)
C     ===============================
C1    VARTYPE                 I   L
C1    IOSTATUS                I4  O
C
C2    The MEnu loader based on CODE
C
      include 'include/menpop.inc'
      include 'include/vntable.inc'
      include 'include/lfont.inc'
      include 'include/pendat.inc'
      include 'include/thklst.inc'
C
      LOGICAL OK
      INTEGER*4 CODE,START,END,BUFFER,NLEN,I,THKNUM,DOLIM,INDVAL
      INTEGER*2 MASK,ENDVAL,AND_2,CELS
      CHARACTER TXT*20
C
      EXTERNAL NLEN,AND_2
C     reset totalisers
      MNTOT = 1
      OK = .FALSE.
 
      IF (CODE.EQ.-1) THEN
C        ***********************************
C        Menu -1 is the line font selection.
C        ***********************************
         DAXMNU(1)='Font Selection'
         DAXTOK(1)=' '
         NUMCOL= 0
         IF (NFONTS.LT.MAXCEL) THEN
            DOLIM = NFONTS
         ELSE
            DOLIM = MAXCEL - 1
         ENDIF
         DO 20,MNTOT=1,DOLIM
            DAXMNU(MNTOT+1)=FONTNM(MNTOT)
            DAXTOK(MNTOT+1)=CHAR(MNTOT)
 20      CONTINUE
      ELSE IF (CODE.EQ.-2) THEN
C        **************************************
C        Menu -2 is the plotter type selection.
C        **************************************
         DAXMNU(1)='Plotter Type'
         DAXTOK(1)=' '
         NUMCOL= 0
         IF (MODELS.LT.MAXCEL) THEN
            DOLIM = MODELS
         ELSE
            DOLIM = MAXCEL - 1
         ENDIF
         DO 30,MNTOT=1,DOLIM
            DAXMNU(MNTOT+1)=TYPE(MNTOT)
            DAXTOK(MNTOT+1)=CHAR(MNTOT)
 30      CONTINUE
      ELSE IF (CODE.EQ.-3) THEN
C        ************************************
C        Menu -3 is the line width selection.
C        ************************************
         DAXMNU(1)='     Thick  Pen  End'
         DAXTOK(1)=' '
         NUMCOL= 0
         MNTOT = 1
         MASK = 15 * 1024
C        Put the '0' option on the top.
         MNTOT = MNTOT + 1
         DAXMNU(MNTOT) = ' 0:  0.00   0.00   0'
         DAXTOK(MNTOT) = CHAR(0)
C        Put all the ones from the file in the middle.
         IF (DFSTOT.LE.(MAXCEL-3)) THEN
            DOLIM = DFSTOT
         ELSE
            DOLIM = MAXCEL - 3
         ENDIF
         DO 40,THKNUM=1,DOLIM
            MNTOT = MNTOT + 1
C           Get the thickness number from the list.
            I = FNTDFS(THKNUM)
C           Mask is f00. This picks out the line end number.
            ENDVAL = AND_2(PLTHKI(I),MASK)  / 1024
            WRITE(TXT,'(I2,A,F4.2,A,F4.2,A,I2)') I,':  ',
     +                    LTHKR(1,I),'   ',LTHKR(2,I),'  ',ENDVAL
            DAXMNU(MNTOT)=TXT
            DAXTOK(MNTOT)=CHAR(I)
 40      CONTINUE
C        Put the OTHER option on the bottom
         MNTOT = MNTOT + 1
         DAXMNU(MNTOT) = VNOUN(506)
         DAXTOK(MNTOT) = VNTOKE(506) 
      ELSE
         IF(CODE.LT.1.OR.CODE.GT.501) THEN
             CALL DEPRNT(655)
             RETURN
         ENDIF
 
C        *************************
C        The rest are normal menus
C        *************************
         START = MNPNTR(1,CODE)
         END = MNPNTR(2,CODE)
         NUMCOL = MNPNTR(3,CODE)
C
C        Load banner
         IF(NLEN(DAXBNR(CODE)).GT.0) THEN
             DAXMNU(MNTOT)=DAXBNR(CODE)
             DAXTOK(MNTOT)=' '
         ELSE
             DAXMNU(MNTOT)='Daxcad Popup'
             DAXTOK(MNTOT)=' '
         ENDIF
         IF ((1+END-START).LT.MAXCEL) THEN
            DOLIM = END
         ELSE
            DOLIM = START + MAXCEL - 2
         ENDIF
         DO 10 MNSTK=START,DOLIM
             MNTOT=MNTOT+1
             INDVAL = MNPCEL(MNSTK)
             IF (INDVAL.GT.0) THEN
                DAXMNU(MNTOT)=VNOUN(INDVAL)
                DAXTOK(MNTOT)=VNTOKE(INDVAL)
             ENDIF
10       CONTINUE
C
      ENDIF
      OK =.TRUE.
      END
C
C
      SUBROUTINE MENPOP_PAINT(MNCODE,X,Y,PAINT)
C     =========================================
C1    VARTYPE                   I4  I2 I2  L
C1    IOSTATUS                  I   I  I   I
C
C2    This rouitne will paint in the menu values from a defined
C2    array held in the arrays in MENPOP.INC. 
C2
C2    MNCODE = Menu number.
C2    X,Y = Top left corner of menu. 
C2    PAINT = Drawing or erasing menu?
C2                .TRUE.  - Draw menu.
C2                .FALSE. - Erase menu.
C2
      include 'include/gpr.ins.inc'
C
      include 'include/gtxt2.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/menpop.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
      include 'include/shadow.inc'
C
      CHARACTER STRING*20
      INTEGER*2 BMWIND(2,2),DISPOS(2)
      INTEGER*2 X,Y ,II2
      INTEGER*2 SCALE
      REAL TX,TY,XMAX,XMIN,YMAX,YMIN, APX,APY,IX(2),IY(2)
      INTEGER*2 COUNT,I,J,RLEN, TEMPX,TEMPY,XSTEP,YSTEP,PENCOL
      INTEGER*2 WINDOW(4),ORG(2),BOX(4)
      INTEGER*4 MNCODE,XP,YP,CURBM,ST,NLEN,LN,MDESC,COLR,COL
      INTEGER*4 INDX
      INTEGER*4 TEMPC
      LOGICAL UNOBSCURED,PAINT,OK
      EXTERNAL DRAWLS,PENCOL
C
      XP = X
      YP = Y
C
C--------------------------------------------------------------------- 
C     Calculate the size of the new menu.    
C--------------------------------------------------------------------- 
C
C Calculate the cell width.
C
C     The Banner first.
      IF (NUMCOL.EQ.0) THEN
C        Single column.
         MAXLN = NLEN(DAXMNU(1))
      ELSE
C        Multiple columns. Banner stretches across all.
         MAXLN = (NLEN(DAXMNU(1))) / NUMCOL
      ENDIF
C     Now the rest of the cells.
      DO 100 I =2,MNTOT
         LN = NLEN(DAXMNU(I))
         IF(LN.GT.MAXLN) MAXLN = LN
 100  CONTINUE
C
C Calculate the size in pixels.
C
      IF (NUMCOL.EQ.0) THEN
C        A simple, 1 column menu.
C        width = (width of text box * No of chars) + borders
         BITMW = (TEXTW * MAXLN) + HBORDR
C
C        height = (height of text box * No of cells) + borders
C        NOTE: banner is included in VBORDER.
         BITMH = (TEXTH + LNSPC)*MNTOT + VBORDR
C
C        Menu -3 is the line width. Must leave room for examples.
         IF (MNCODE.EQ.-3) BITMW = BITMW + 110
      ELSE
C        A multiple column menu.
C        width = (width of text box * No of chars * No of columns) +
C                borders + (space between the columns.)
         BITMW = (TEXTW*MAXLN*NUMCOL)+HBORDR+((NUMCOL-1)*(LGAP+RGAP))
C
C        height = (height of text box * No of rows) + borders
         BITMH = ((TEXTH + LNSPC)*(((MNTOT-1)/NUMCOL)+1)) + VBORDR
      ENDIF
C
C--------------------------------------------------------------------- 
C     Ensure that the menu position is fully on the screen.
C--------------------------------------------------------------------- 
C
C     Right hand side of screen.
      IF(XP.GT.BMSIZE(1)-BITMW-VERGE) XP = BMSIZE(1)-BITMW-VERGE
C     Bottom of screen.
      IF(YP.GT.BMSIZE(2)-BITMH-VERGE) YP = BMSIZE(2)-BITMH-VERGE
C     Left hand side of screen.
      IF(XP.LT.VERGE) XP =VERGE
C     Top of screen.
      IF(YP.LT.VERGE) YP =VERGE
C
C     And the exact position of the menu is ...
      BOX(1) = XP - 1
      BOX(2) = YP - 1
      BOX(3) = BOX(1)+BITMW - (2*BLKBDR) - SHADOW
      BOX(4) = BOX(2)+BITMH - (2*BLKBDR) - SHADOW
C
C--------------------------------------------------------------------- 
C     Got size and position ... Paint in or out ?
C--------------------------------------------------------------------- 
C
      IF(.NOT.PAINT) THEN
C         NOTE: This is removing the menu and should be the second call
C         to MENPOP_PAINT for this menu.
          WINDOW(1) = 0
          WINDOW(2) = 0
          WINDOW(3) = BITMW
          WINDOW(4) = BITMH
          ORG(1) = STARTX-BLKBDR
          ORG(2) = STARTY-BLKBDR
          CALL ROPREP()
C         acquire display NOW !!!
          UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
          CALL GPR_$PIXEL_BLT(SWBITM,WINDOW,ORG,ST)
C
          CALL GPR_$RELEASE_DISPLAY(ST)
          CALL ROPREP()
          RETURN
      ENDIF
C
C--------------------------------------------------------------------- 
C     Store the bit map under the menu then clear the area.
C--------------------------------------------------------------------- 
C
C     get current bitmap
      CALL GPR_$INQ_BITMAP(CURBM,ST)
C     XP and YP now contain the modified origin of the menu
C     set location and size of portion of main bitmap to be saved
      WINDOW(1) = XP-BLKBDR
      WINDOW(2) = YP-BLKBDR
      WINDOW(3) = BITMW
      WINDOW(4) = BITMH
      ORG(1) = 0
      ORG(2) = 0
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
C
C     save the current configuration
      CALL GPR_$SET_BITMAP(SWBITM,ST)
      CALL GPR_$PIXEL_BLT(CURBM,WINDOW,ORG,ST)
      CALL GPR_$SET_BITMAP(CURBM,ST)
C
      CALL GPR_$RELEASE_DISPLAY(ST)
C
      CALL TOOLPEN(MENUB)
C     remember to clear the menu area SCREEN
      XMIN = BOX(1)
      YMIN = BOX(2)
      XMAX = BOX(3)
      YMAX = BOX(4)
      CALL CLEARW(XMIN,YMIN,XMAX,YMAX)
      CALL TOOLPEN(MENUF)
C
C--------------------------------------------------------------------- 
C     Draw a nice thick line round the menu
C--------------------------------------------------------------------- 
C
      DO 50 I=1,2
         CALL DRSREC(BOX(1),BOX(2),BOX(3),BOX(4) )
         BOX(1)=BOX(1)-1
         BOX(2)=BOX(2)-1
         BOX(3)=BOX(3)+1
         BOX(4)=BOX(4)+1
 50   CONTINUE
C
C--------------------------------------------------------------------- 
C     If multiple columns ... divide them by a verticle line
C--------------------------------------------------------------------- 
C
      IF (NUMCOL.GT.0) THEN
         UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
         DO 40,I=1,NUMCOL-1
C           Miss the top (banner) line.
            TEMPX = XP + (((TEXTW * MAXLN) + LGAP + RGAP) * I)
            TEMPY = YP + TEXTH + LNSPC
            CALL GPR_$MOVE(TEMPX,TEMPY,ST)
            TEMPY = YP + BITMH - (2*BLKBDR + SHADOW)
            CALL GPR_$LINE(TEMPX,TEMPY,ST)
 40      CONTINUE
         CALL GPR_$RELEASE_DISPLAY(ST)
      ENDIF
C
C--------------------------------------------------------------------- 
C     Next ... The shadow.
C--------------------------------------------------------------------- 
C
      CALL TOOLS_SET_SHADOW(.TRUE.)
      CALL TOOLPEN(MENUF)
      CALL ROPREP()
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      WINDOW(1) = BOX(3)
      WINDOW(2) = BOX(2) + SHADOW
      WINDOW(3) = SHADOW - 1
      WINDOW(4) = BOX(4) - BOX(2) - 1
      CALL GPR_$RECTANGLE(WINDOW,ST)
      WINDOW(1) = BOX(1) + SHADOW
      WINDOW(2) = BOX(4)
      WINDOW(3) = BOX(3) - BOX(1) - 1
      WINDOW(4) = SHADOW - 1
      CALL GPR_$RECTANGLE(WINDOW,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
      CALL TOOLS_SET_SHADOW(.FALSE.)
C
C--------------------------------------------------------------------- 
C     Now ... paint the cell with text
C--------------------------------------------------------------------- 
C
C     set coords for starting postion in COMMON
      STARTX = XP
      STARTY = YP
C     make sure raster op
      CALL ROPREP()
      IF (NUMCOL.EQ.0) THEN
C---------------------------------------
C        A simple, 1 column menu.
C---------------------------------------
         COUNT = MNTOT
         IF (MNCODE.EQ.6) THEN
C
C           Menu 6 is colour menu. Write each colour in that colour.
            TX = XP + LGAP
            TY = YP + TEXTH + TGAP + LNSPC/2 -3
C           First the banner in white.
            STRING = DAXMNU(1)
            CALL GTSPMG(TX,TY,STRING,.FALSE.)
            DO 10 I = 2,COUNT
C              Text positon in cell
               TX = XP + LGAP
               TY = TY + TEXTH + LNSPC
C              This is where the menu is
               STRING = DAXMNU(I)
C              Pick the colour.
               II2 = I - 1
               TEMPC = MENUF
               COLR = II2
               MENUF = COLR
C              paint the bastard in
               CALL GTSPMG(TX,TY,STRING,.FALSE.)
               MENUF = TEMPC
 10         CONTINUE
C           Put the white text colour back.
            I = COLFOR
            CALL TOOLPEN(COLR)
C
         ELSE
C
C           generate first point for the text
            TY = YP + TEXTH + TGAP + LNSPC/2 -3
C           step calculations for thick line popup.
            XSTEP = 90 + HBORDR
            YSTEP = TEXTH / 2
            DO 60 I = 1,COUNT
C               Text positon in cell
                TX = XP + LGAP
C               This is where the menu is
                STRING = DAXMNU(I)
C               paint the bastard in
                CALL GTSPMG(TX,TY,STRING,.FALSE.)
C               Menu -3 is the line width menu. Must draw  examples.
                IF (MNCODE.EQ.-3) THEN
C                  Miss the top (banner) and bottom (OTHER) lines.
                   IF (I.GT.1 .AND. I.LT.COUNT) THEN
                      IX(1) = REAL(XP + BITMW - XSTEP)
                      IY(1) = REAL(TY - YSTEP)
                      IX(2) = IX(1) + 91.0
                      IY(2) = IY(1)
C                     calculate thick index
                      INDX = I-2
C                     Paint in thick line
                      CALL PAINT_LINE(IX(1),IY(1),IX(2),IY(2),INDX)
                   ENDIF
C
                ENDIF
C               Increment to next position which is text height + line spacing
                TY = TY + TEXTH + LNSPC
 60         CONTINUE
C
         ENDIF
      ELSE
C---------------------------------------
C        A multiple column menu.
C---------------------------------------
         COUNT = 1
C        Banner
C        Text positon in cell
         TX = XP + LGAP
         TY = YP + TEXTH + TGAP + LNSPC/2
         STRING = DAXMNU(COUNT)
C        paint the bastard in
         CALL GTSPMG(TX,TY,STRING,.FALSE.)
         TY = TY + TEXTH + LNSPC
         RLEN = (MNTOT-1)/NUMCOL
         XSTEP = (TEXTW*MAXLN)+LGAP+RGAP
         DO 20 I=1,RLEN
            DO 30,J=1,NUMCOL
               COUNT = COUNT + 1
               TX = XP + LGAP + (XSTEP * (J-1))
C              This is where the menu is
               STRING = DAXMNU(COUNT)
C              paint the bastard in
               CALL GTSPMG(TX,TY,STRING,.FALSE.)
 30         CONTINUE
            TY = TY + TEXTH + LNSPC
 20      CONTINUE
      ENDIF
C
C--------------------------------------------------------------------- 
C     Highlight the BANNER cell
C--------------------------------------------------------------------- 
C
C     NOTE: We leave a wee white border between the highlight and the
C     menu border.
      CALL TOOLPEN(MENUF)
      BMWIND(1,1)=XP +2
      BMWIND(2,1)=YP+TGAP
      BMWIND(1,2)=(BITMW - 2*BLKBDR - SHADOW) -5
      BMWIND(2,2)=TEXTH+LNSPC
C     highlight it
c      write(10,*) '[PAINT] DAXMNU(1)= ',DAXMNU(1)
      CALL ROPXOR()
      CALL GPR_$RECTANGLE(BMWIND,ST)
      CALL ROPREP()
C
      END
C
C
      SUBROUTINE MENPOP_SET_BACKGROUND(GRY)
C     =====================================
C1    VARTYP                            I4
C1    IOSTAT                            I
C
C2    This routine allocates a bitmap for POPUP menu MARKING
C
      include 'include/gpr.ins.inc'
C
      include 'include/menpop.inc'
      include 'include/apollo.inc'
      include 'include/fill_pattern.inc'
      include 'include/daxcolor.inc'
      include 'include/interface.inc'
C
      INTEGER*4 STATUS,AB_FILL,AB_BACK,ST,BM_DESC,COL,GRY
      INTEGER*2 WINDOW(4),BMSIZE(2),PLMASK,X,Y
      INTEGER*2 FILL_VALUE,CENTER(2),RAD
      INTEGER*2 BACK_VALUE,SCALE,DENSTY,I,J,SP,O,P
      INTEGER*2 GREY(4,4,4)
      LOGICAL UNOBSCURED
C
C     Initialise grey patterns
C     grey ( columns, rows, shades)
C     shade 1  = 25%
C     shade 1  = root
C     shade 1  = 50%
C     shade 1  = 75%
      DATA GREY / 1,0,0,0,
     +            0,1,0,0,
     +            0,0,0,1,
     +            0,0,1,0, 1,0,0,0,
     +                     1,0,0,0,
     +                     0,0,1,0,
     +                     0,0,1,0, 1,0,1,0,
     +                              0,1,0,1,
     +                              1,0,1,0,
     +                              0,1,0,1, 0,1,1,1,
     +                                       1,0,1,1,
     +                                       1,1,1,0,
     +                                       1,1,0,1/
C
C     set fill bitmap size
      BMSIZE(1) = 450
      BMSIZE(2) = 30
      PLMASK = 15
      SCALE = 1
C
C     acquire display now
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(STATUS)
C
C     allocate an attributre block for each bitmap
C
      CALL GPR_$ALLOCATE_ATTRIBUTE_BLOCK(AB_FILL,STATUS)
C
C
      CALL GPR_$ALLOCATE_BITMAP
     +     (BMSIZE,HIPLAN,AB_FILL,MNBITM,STATUS)
C
C
C     set backing bitmap
      CALL GPR_$SET_BITMAP(MNBITM,ST)
C     were gonna fill it
C     white on both mono an colour
      COL = COLBAK
      CALL TOOLPEN(COL)
C
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = BMSIZE(1)
      WINDOW(4) = BMSIZE(2)
C
      CALL GPR_$RECTANGLE(WINDOW,ST)
      COL = MENUF
      CALL TOOLPEN(COL)
C
C     write patterns into bitmap
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(STATUS)
      DO 10 O=1,32,4
          DO 20 P=1,350,4
              DO 30 I=1,4
                  DO 40 J= 1,4
                      X=I+P-1
                      Y=J+O-1
                      IF(GREY(j,i,GRY).EQ.1) THEN
                          CALL DRAWPT(X,Y)
                      ENDIF
 40               CONTINUE
 30           CONTINUE
 20       CONTINUE
 10   CONTINUE
      CALL GPR_$RELEASE_DISPLAY(ST)
C
C     reset current bitmap
C
      CALL GPR_$SET_BITMAP(DISPDE,STATUS)
C
      CALL GPR_$RELEASE_DISPLAY(ST)
C
C
      END
C
      SUBROUTINE PAINT_LINE(X1,Y1,X2,Y2,INDX)
C     =======================================
C1    VARTYPE               R  R  R  R    I4
C1    IOSTAT                I  I  I  I    I
C
C2    This routine will draw a line for the 'line thickness'
C2    menu (number -3).
C2    NOTE: This routine is a cut down version of DRAWLT with
C2    all the world to screen conversion removed.
C
      include 'include/lfont.inc'
      include 'include/thklst.inc'
C
      REAL X1,Y1,X2,Y2,CONST,TOT,EXT,XT1,YT1,XT2,YT2,
     +     VX1,VY1,VX2,VY2,L1,L2,L3,N1(2),N2(2),N3(2),
     +     MINMAX
      INTEGER*2 CENT(2),SRAD,IX(4),IY(4),N
      INTEGER*4 THK,END
      INTEGER*4 STATUS
      INTEGER*4 INDX
C
      EXTERNAL MINMAX
C
      CONST = 32000.0
      EXT = 0.0
      IF (INDX.GT.0) THEN
         THK = FNTDFS(INDX)
         THK = PLTHKI(THK)
      ELSE
         THK = 0
      ENDIF
C
      CALL PENDRW()
C
      XT1 = X1
      XT2 = X2
      YT1 = Y1
      YT2 = Y2
C     check for line thickness
      IF ( THK.GT.0 ) THEN
         TOT=ABS(6.0*LTHKR(1,MOD(THK,1024)))
         IF (TOT.GT.8.0) TOT=8.0
         END=MOD(THK,8192)/1024
         CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
         IF ( END.EQ.4 ) THEN
            EXT=TOT
            CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
            CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
         END IF
         IF(END.EQ.2) THEN
C           Draw a rounded end.
            CENT(1)=NINT(X1)
            CENT(2)=NINT(Y1)
            SRAD=NINT(TOT) - 1
            CALL GPR_$CIRCLE_FILLED(CENT,SRAD,STATUS)
         ENDIF
         CALL VV0L15(L1,L2,L3,TOT,N1(1),N2(1),N3(1),
     +                               N1(2),N2(2),N3(2))
         CALL VC00P4(X1,Y1,-EXT,L1,L2,L3,XT1,YT1)
         CALL VC00P4(X2,Y2, EXT,L1,L2,L3,XT2,YT2)
         CALL VC0PLP(N1(1),N2(1),N3(1),XT1,YT1,VX1,VY1)
         CALL VC0PLP(N1(1),N2(1),N3(1),XT2,YT2,VX2,VY2)
         IX(1)=NINT(MINMAX(VX1,CONST))
         IY(1)=NINT(MINMAX(VY1,CONST))
         IX(4)=IX(1)
         IY(4)=IY(1)
         CALL GPR_$START_PGON(IX(1),IY(1),STATUS)
         IX(1)=NINT(MINMAX(VX2,CONST))
         IY(1)=NINT(MINMAX(VY2,CONST))
         CALL VC0PLP(N1(2),N2(2),N3(2),XT2,YT2,VX2,VY2)
         CALL VC0PLP(N1(2),N2(2),N3(2),XT1,YT1,VX1,VY1)
         IX(2)=NINT(MINMAX(VX2,CONST))
         IY(2)=NINT(MINMAX(VY2,CONST))
         IX(3)=NINT(MINMAX(VX1,CONST))
         IY(3)=NINT(MINMAX(VY1,CONST))
         N=4
         CALL GPR_$PGON_POLYLINE(IX,IY,N,STATUS)
         CALL GPR_$CLOSE_FILL_PGON(STATUS)
C
         IF(END.EQ.2) THEN
C           Draw a rounded end.
            CENT(1)=NINT(X2)
            CENT(2)=NINT(Y2)
            SRAD=NINT(TOT) - 1
            CALL GPR_$CIRCLE_FILLED(CENT,SRAD,STATUS)
         ENDIF
      ELSE
C        now we have screen coords VX1,VY1,VX2,VY2
C        must draw line with correct fonting
         CALL DRAWLS(XT1,YT1,XT2,YT2)
      END IF
C
      END
C
C
