C
C     @(#)  412.1 date 6/11/92 browse.f 
C
C
C     Filename    : browse.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:51:03
C     Last change : 92/06/11 14:24:19
C
C     Copyright : Practical Technology Int Limited  
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C

C     ********************************************************************
C
C                     B R O W S I N G   S Y S T E M
C
C      This is the machine independant browsing code.  We init and
C      then draw system.
C
C     ********************************************************************
C

      SUBROUTINE BRMAIN(DEFAULT,FILNAM,ST)
C     ====================================
C1    VARYPE              I4      C*  I4
C1    IOSTAT              I       O   O
C
C2    Main browser control. Controls cursor and buttons
C2    Will return a selected file for use by the file selector if needed
C2  
C2    Arguments:-
C2  
C2    DEFAULT	->	Default types
C2    FILNAM	->	The file selcted for loading
C2  
C2  
C2    Error Returns:
C2  
C2    0		->	File has been selected
C2   -1		->	Nothing selected or a quit
C2  
C2  
C2  
      include    'include/event.inc'
      include    'include/interface.inc'
C
      INTEGER*4 DEFAULT
      INTEGER*4 EVTYPE
      INTEGER*4 EVNUM
      INTEGER*4 X,Y
      INTEGER*4 BUTTON
      INTEGER*4 ST
      INTEGER*4 BOXN
      INTEGER*4 PAGE
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      INTEGER*4 STATUS
      CHARACTER*(*) FILNAM
      CHARACTER*256 FILE
      CHARACTER*300 TEXT
      CHARACTER*256 NUMBER
      CHARACTER*256 PROMPT
C 
      EXTERNAL NLEN
C
      FILNAM = ' '
      FILE = ' '
      CALL BROWSEINIT(ST)
      CALL BROWSELOAD(DEFAULT,ST)
C     load up first page
      IF ( BRPICS.GT.0) THEN
          PAGE = 1
          CALL BROWSEPAGE(PAGE,ST)
      ENDIF
      CALL CURSOR_ON(.TRUE.)
C
1000  CONTINUE
C
      CALL TOOLS_NOTIFY(X,Y,EVTYPE,BUTTON,EVNUM,ST)
C
      IF ( EVTYPE.EQ.DAX_EVENT_BUTTON ) THEN

           IF ( EVNUM.EQ.19) THEN
C              Selection now made
               CALL BUTTON_LOW(EVNUM,ST)
               IF ( NLEN(FILE).GT.0 ) THEN
C                  strip .pic 
                   LENGTH = NLEN(FILE)
                   FILNAM = FILE(1:LENGTH-4)
                   ST = 0
               ELSE
                   ST = -1
               ENDIF
               GOTO 999
           ELSEIF ( EVNUM.EQ.20) THEN
C              Clear the buffer 
               CALL BUTTON_LOW(EVNUM,ST)
               ST = -1
               GOTO 999
           ELSEIF ( EVNUM.EQ.16) THEN
               PAGE = BRPAGE + 1
               CALL BROWSEPAGE(PAGE,ST)
               CALL BUTTON_LOW(EVNUM,ST)
           ELSEIF ( EVNUM.EQ.17) THEN
C              Again the text
               PAGE = BRPAGE - 1
               CALL BROWSEPAGE(PAGE,ST)
               CALL BUTTON_LOW(EVNUM,ST)
           ELSEIF ( EVNUM.EQ.18) THEN
C              select a page number from the dialog box
               PAGE = BRPAGE
               PROMPT = 'Enter new page number'
               CALL GETINPUT(PROMPT,NUMBER)
               CALL CRUNCH(NUMBER)
               READ(NUMBER,FMT='(I2)',ERR=100,END=100) PAGE
100            CONTINUE
               CALL BROWSEPAGE(PAGE,ST)
               CALL BUTTON_LOW(EVNUM,ST)
           ELSEIF ( EVNUM.EQ.0) THEN
               CALL BROWSETEST(X,Y,BOXN,ST)
               IF ( BOXN.GT.0) THEN
                   CALL GETFILEFROMBOX(BOXN,FILE,ST)
                   IF ( ST.EQ.0) THEN
                       TEXT = 'Current File:  '//FILE(1:NLEN(FILE)-4)
                       LENGTH = NLEN(TEXT)
                       CALL TEXTINWINDOW(3,1,BRFCOL,TEXT,LENGTH)
                       CALL TOOLPEN(BRFCOL)
                   ELSE
                       FILE = ' '
                       TEXT = 'Current File:  Nothing Selected'
                       LENGTH = NLEN(TEXT)
                       CALL TEXTINWINDOW(3,1,BRFCOL,TEXT,LENGTH)
                       CALL TOOLPEN(BRFCOL)
                   ENDIF
               ENDIF


           ELSE
               CALL BUTTON_LOW(EVNUM,ST)
           ENDIF
C
      ENDIF
      GOTO 1000
C
999   CONTINUE
      CALL CURSOR_ON(.FALSE.)
      CALL BROWSEEXIT(STATUS)
      END



      SUBROUTINE BROWSEINIT(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Initalise the browser dialog and draw it onto the DAXCAD screen
C2    Do all calcs on the picture frames.
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C
      include   'include/daxcolor.inc'
      include   'include/interface.inc'
C
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 TSIZE(2)
      INTEGER*4 TORG(2)
      INTEGER*4 LENGTH
      INTEGER*4 I4LEN
      INTEGER*4 XP, YP

      INTEGER*2 XT,YT
      INTEGER*2 I2LEN

      CHARACTER*256 STRING

      EXTERNAL NLEN
C
C     set frame positions
      CALL DIAGDR(5,.TRUE.,ST)
      CALL BUTDRW(16,.TRUE.,ST)
      CALL BUTDRW(17,.TRUE.,ST)
      CALL BUTDRW(18,.TRUE.,ST)
      CALL BUTDRW(19,.TRUE.,ST)
      CALL BUTDRW(20,.TRUE.,ST)
      CALL FRMDRW(3,.TRUE.,ST)

      CALL SET_FONT(4,ST)
      CALL TOOLS_SET_TEXT()


      STRING = 'GRAPHICAL BROWSING DIALOG'
      CALL GET_TEXT_INFO(STRING,NLEN(STRING),TSIZE,TORG,ST)

      XP = DIALOG5(1) + DIALOG5(3)/2 - TSIZE(1)/2
      YP = DIALOG5(2) + 20

C     set the label Number to six, why six ?? 
C     cos the  fileselector has used 5
      I4LEN = NLEN(STRING)
      CALL LABMOD(6,XP,YP,STRING,I4LEN,ST)
      CALL LABDRW(6,.TRUE.,ST)

      CALL SET_FONT(1,ST)

C     Set u some size s for drawing
C
      NUMBX = 9
      NUMBY = 5
      BRFSX = (NUMBX*100)+((NUMBX+1)*BRSPX)
      BRFSY = (NUMBY*100)+((NUMBY+1)*BRSPY)
C     page number
      BRPAGE = 1
C     Frame number selected
      BRSELECT = 0
      BRBCOL = DIALOGB
      BRFCOL = COLFOR
      BRSOX = 5 + DIALOG5(1)
      BRSOY = 40 + DIALOG5(2)
      CALL BRDRAWPAGE(ST)

C
C     Tell im what hes got
C
      STRING = 'Current File: Nothing Selected'
      LENGTH = NLEN(STRING)
      CALL TEXTINWINDOW(3,1,BRFCOL,STRING,LENGTH)

      
C
      
      END
C
      SUBROUTINE BROWSEEXIT(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                O
C
C2    Shut down browser and undraw all objects
C2
C2
C
      include   'include/interface.inc'
C
      INTEGER*4 ST
C
      CALL BUTDRW(16,.FALSE.,ST)
      CALL BUTDRW(17,.FALSE.,ST)
      CALL BUTDRW(18,.FALSE.,ST)
      CALL BUTDRW(19,.FALSE.,ST)
      CALL BUTDRW(20,.FALSE.,ST)
      CALL FRMDRW(3,.FALSE.,ST)
      CALL DIAGDR(5,.FALSE.,ST)
      IF ( BRUNIT.GT.0) THEN
          CLOSE(BRUNIT)
      ENDIF
      CALL TMPCLEANUP(BRFID,ST)

C
      END

      SUBROUTINE BRCLEAR(ST)
C     =======================
C1    VARYPE             I4
C1    IOSTAT             O
C
C2    This routine will clear just the browse frame contaiing
C2    each of the pictures
C
      include   'include/apollo.inc'
      include   'include/daxcolor.inc'
      include   'include/interface.inc'
C
      INTEGER*4 ST
      INTEGER*2 RECT(4),LX,LY
      LOGICAL UNOBSCURED
C
C
C     border size is 2 pixels
      RECT(1) = BRSOX
      RECT(2) = BRSOY
      RECT(3) = NUMBX*(BRSBX + BRSPX) + BRSPX
      RECT(4) = NUMBY*(BRSBY + BRSPY) + BRSPY
C     set background color
      CALL TOOLPEN(BRBCOL)
      CALL TOOLS_RECTANGLE(RECT,ST)
C
      END
C
C     -----------------------------------------------------
C
      SUBROUTINE BRDRAWPAGE(ST)
C     =========================
C1    VARYPE                I4
C1    IOSTAT                I
C
C2    This draws the box frames before all the pictures are drawn into
C2    PLACE
C
      include   'include/daxcolor.inc'
      include   'include/interface.inc'
C
      INTEGER*4 XMIN,YMIN,XMAX,YMAX
      INTEGER*4 I,J,ST
      INTEGER*2 LY
      INTEGER*2 INT2
C
      CALL TOOLPEN(BRFCOL)
      DO 10 I=1,NUMBX
          DO 20 J=1,NUMBY
              XMIN = BRSOX + ((I-1)*100)+(I*10)
              YMIN = BRSOY +((J-1)*100)+((J-1)*10)+BRSPY
              XMAX = XMIN+BRSBX
              YMAX = YMIN+BRSBY
              CALL TOOLS_BOX(INT2(XMIN),INT2(YMIN),
     +              INT2(XMAX),INT2(YMAX) ,ST)


20    CONTINUE
10    CONTINUE
      END
C
      SUBROUTINE BROWSETEST(X,Y,BOXN,ST)
C     ==================================
C1    VARYPE               I4 I4 I4  I4
C1    IOSTAT               I  I  O   O
C
C2    Getss cursor hit and will transform that hit into
C2    a box number and a current 
C2  
C2  
C2    Arguments:-
C2  
C2    X,Y	->	Cursor hit point
C2    BOXN	->      Box number hit (NUMBX*NUMBY) 0 no hit
C2  
C2  
C2    Error Returns:
C2
C2    NONE
C2
C2
C
      include   'include/daxcolor.inc'
      include   'include/interface.inc'
C
      INTEGER*4 X,Y
      INTEGER*4 ST
      INTEGER*4 BOXN
      INTEGER*4 SC
      INTEGER*4 XMIN,YMIN
      INTEGER*4 XMAX,YMAX
      INTEGER*4 INT4
      INTEGER*4 I
      INTEGER*4 J
      INTEGER*4 INC
C
      INTEGER*2 XS,YS
      INTEGER*2 XF,YF
C
      LOGICAL PNTBOX
C
      EXTERNAL PNTBOX
      EXTERNAL INT4
C
      SC = 0
      DO 20 J=1,NUMBY
          DO 10 I=1,NUMBX

              SC = SC+1
              XMIN = BRSOX+((I-1)*100)+(I*10)
              YMIN = BRSOY+ ((J-1)*100)+((J-1)*10)+BRSPY
              XMAX = XMIN+BRSBX
              YMAX = YMIN+BRSBY
C
              IF(PNTBOX(XMIN,YMIN,XMAX,YMAX,X,Y)) THEN
C                 de highlight current box
                  IF ( BRSELECT.GT.0) THEN
                      CALL TOOLPEN(BRBCOL)
                      DO 30 INC=1,4
                          XS = BRRECT(1)-INC
                          YS = BRRECT(2)-INC
                          XF = BRRECT(3)+INC
                          YF = BRRECT(4)+INC
                          CALL TOOLS_BOX(XS,YS,XF,YF,ST)
 30                   CONTINUE
                  ENDIF
C
C                 Save current hit params
C
                  BRSELECT = SC
                  BOXN = SC
                  BRRECT(1) = XMIN
                  BRRECT(2) = YMIN
                  BRRECT(3) = XMIN + BRSBX
                  BRRECT(4) = YMIN + BRSBY
                  CALL TOOLPEN(BRFCOL)
C                 hightlight it
                  DO 40 INC=1,4
                      XS = BRRECT(1)-INC
                      YS = BRRECT(2)-INC
                      XF = BRRECT(3)+INC
                      YF = BRRECT(4)+INC
                      CALL TOOLS_BOX(XS,YS,XF,YF,ST)
 40               CONTINUE
                  GOTO 2000
              ENDIF
C         continue looping for test
 10       CONTINUE
 20   CONTINUE
2000  CONTINUE
      END
C

      SUBROUTINE BROWSELOAD(DEFAULT,ST)
C     =================================
C1    VARYPE                  I4    I4
C1    IOSTAT                  I     O 
C
C2    Load the pic files in the specified directory
C2    into the temp file and use open the file in ready for loading up
C2    the pictures from the file
C2  
C2    Arguments:-
C2  
C2    DEFAULT	->	The daxcad type of file
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    0  OK
C2    -1 FAILURE
C2  
      include   'include/interface.inc'
      include   'include/macro.inc'
C
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 DEFAULT
      INTEGER*4 NLEN
      INTEGER*4 DIRS
      INTEGER*4 MATCHNO
      INTEGER*4 UNITS
      INTEGER*4 PLEN

      LOGICAL OK

      CHARACTER*256 TMPFIL
      CHARACTER*256 WILD
      CHARACTER*256 PATH
      CHARACTER*256 CWD

      EXTERNAL NLEN

      ST = 0
      BRPICS = 0
      BRUNIT = 0
      CALL GETTMPFILE(TMPFIL,LENGTH,BRFID,ST)
C     set wild card and targets

      IF ( DEFAULT.EQ.-1 ) THEN
C          Defaults to any file type
           CALL TOOLSGETCWD(CWD,PLEN,ST)
           PATH = CWD(1:PLEN)
           WILD = '.*.pic$'
      ELSEIF ( DEFAULT.EQ.0 ) THEN
C          Defaults to any file type
           WILD = '.*.drg.pic$'
           PATH = PATHN(1)
      ELSEIF ( DEFAULT.EQ.1 ) THEN
C          Defaults to daxcad parts
           WILD = '.*.prt.pic$'
           PATH = PATHN(4)
      ELSEIF ( DEFAULT.EQ.2 ) THEN
C          Defaults to daxcad components
           WILD = '.*.cmp.pic$'
           PATH = PATHN(3)
      ELSEIF ( DEFAULT.EQ.3 ) THEN
C          Defaults to daxcad symbols
           WILD = '.*.sym.pic$'
           PATH = PATHN(3)
      ELSEIF ( DEFAULT.EQ.4 ) THEN
C          Defaults to daxcad symbols
           WILD = '.*.mac.pic$'
           PATH = PATHN(2)
      ELSEIF ( DEFAULT.EQ.5 ) THEN
C          Defaults to daxcad symbols
           WILD = '.*.dxf.pic$'
           PATH = PATHN(2)
      ELSE
           CALL TOOLSGETCWD(CWD,PLEN,ST)
           PATH = CWD(1:PLEN)
           WILD = '.*.pic$'

      ENDIF

      CALL GET_DIRS_WILD(PATH,
     +                   NLEN(PATH),
     +                   0,
     +                   WILD,
     +                   NLEN(WILD),
     +                   TMPFIL,
     +                   LENGTH,
     +                   MATCHNO,
     +                   ST)
C
C     set maximum for for current selection
C
      IF ( MATCHNO.EQ.0) THEN
          CALL TMPCLEANUP(BRFID,ST)
          GOTO 999
      ENDIF
      BRPICS = MATCHNO
C
      CALL TOOLSSETCWD(PATH,NLEN(PATH),ST)
      CALL FINDU1(BRUNIT,OK)
C
      OPEN(UNIT=BRUNIT,
     +              FILE=TMPFIL(1:LENGTH),
     +              ERR=998)
C
C
      RETURN
999   CONTINUE
      RETURN
998   CONTINUE
      ST = -1
      END


      SUBROUTINE BROWSEPAGE(PAGENO,ST)
C     ================================
C1    VARYPE                 I4    I4
C1    IOSTAT                 I     O
C
C2    Loads the specified page number
C2  
C2  
C2    Arguments:-
C2  
C2    PAGENO	->	Page number
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    0   SUCCESS
C2    -1  Illegal page number
C2  
C
      include   'include/interface.inc'
C
      INTEGER*4 ST
      INTEGER*4 PAGENO
      INTEGER*4 NOP
      INTEGER*4 OFFSET
      INTEGER*4 NLEN
      INTEGER*4 I
      INTEGER*4 TMP
      INTEGER*4 X,Y
      INTEGER*4 LENGTH

      CHARACTER*256 FILNAM
      CHARACTER*256 TEXT

      EXTERNAL NLEN
C
      IF ( BRPICS.EQ.0) THEN
           GOTO 999
      ENDIF
      
      REWIND(BRUNIT)


      NOP = BRPICS/(NUMBX*NUMBY) + 1

      IF ( PAGENO.GT.NOP) PAGENO = NOP
      IF ( PAGENO.EQ.0) PAGENO = 1

      TMP = BRPAGE
      BRPAGE = PAGENO

      CALL BRCLEAR(ST)
      CALL BRDRAWPAGE(ST)

      OFFSET = NUMBX*NUMBY*(PAGENO-1)

      DO 10 I=1,OFFSET
          READ(BRUNIT,FMT='(A)',END=999,ERR=999) FILNAM
 10   CONTINUE


      DO 20 I=1,NUMBX*NUMBY

          READ(BRUNIT,FMT='(A)',END=998,ERR=999) FILNAM
          CALL BRLOADPIC(FILNAM,I,ST)

20    CONTINUE

998   CONTINUE
      WRITE(TEXT,FMT='(I2,A,I2)') TMP,' of ',NOP
      LENGTH = NLEN(TEXT)
      X = BRPAGX
      Y = BRPAGY
C
      CALL SET_FONT(1,ST)
      CALL TOOLPEN_TEXT(BRBCOL,BRBCOL,.TRUE.,ST)
C
      CALL LABMOD(7,X,Y,TEXT,LENGTH,ST)
      CALL LABDRW(7,.TRUE.,ST)
C
      WRITE(TEXT,FMT='(I2,A,I2)') PAGENO,' of ',NOP
      LENGTH = NLEN(TEXT)
      CALL TOOLPEN_TEXT(BRFCOL,BRBCOL,.TRUE.,ST)
C
      CALL LABMOD(7,X,Y,TEXT,LENGTH,ST)
      CALL LABDRW(7,.TRUE.,ST)

999   CONTINUE
      END

      SUBROUTINE BRLOADPIC(FILE,PICNUM,ST)
C     ====================================
C1    VARYPE               C*(*)  I4   I4
C1    IOSTAT                I     I    O
C
C2    Load a picture into the specified location
C2    on the local screen at location PICNUM
C2  
C2    Arguments:-
C2  
C2    FILE	->	The pic file
C2    PICNUM	->	The picture box number
C2  
C2  
C2    Error Returns:
C2  
C2    0   SUCCESS
C2    -1  Cannot load picture file
C2  
      include   'include/interface.inc'

      INTEGER*4 ST
      INTEGER*4 PICNUM
      INTEGER*4 SC
      INTEGER*4 XMIN,YMIN
      INTEGER*4 XMAX,YMAX
      INTEGER*4 NLEN
      INTEGER*4 I,J
      INTEGER*2 X,Y
      INTEGER*2 LENGTH
C
      CHARACTER*(*) FILE
      CHARACTER*256 TEXT
C
      EXTERNAL NLEN
C
      ST = 0
      SC = 0
      DO 20 J=1,NUMBY
          DO 10 I=1,NUMBX

              SC = SC+1
              XMIN = BRSOX+((I-1)*100)+(I*10)
              YMIN = BRSOY+ ((J-1)*100)+((J-1)*10)+BRSPY
              XMAX = XMIN+BRSBX
              YMAX = YMIN+BRSBY

              IF ( PICNUM.EQ.SC) THEN

                   LENGTH = NLEN(FILE)
                   CALL SET_FONT(2,ST)
                   X = XMIN 
                   Y = YMIN
                   CALL BROWSELOADIMAGE(X,Y,FILE,ST)
                   GOTO 999
              ENDIF
 10       CONTINUE
 20   CONTINUE

999   CONTINUE
      END



      SUBROUTINE CREATEBROWSEIMAGE(NAME,TYPE,ST)
C     ==========================================
C1    VARYPE                      C*(*)  I4  I4
C1    IOSTAT                        I    I   O
C
C2    This routine will create a specific type of image for the browser
C2    It can do a full database drawing or read from the scratch file
C2    containing certain the ents
C2  
C2    Arguments:-
C2  
C2    NAME	->	Pic file name
C2    TYPE	->	DAXCAD type ( DRG CMP SYM PRT )
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  

C
      include   'include/browse.inc'
      include   'include/daxcolor.inc'
      include   'include/interface.inc'
      include   'include/wtov.inc'
      include   'include/filunit.inc'
      include   'include/viewport.inc'
      include   'include/masti.inc'
      include   'include/apollo.inc'
      include   'include/ftypes.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/movdat.inc'
      include   'include/swind.inc'
      include   'include/server.inc'
      include   'include/daxcad_x.inc'

C
      INTEGER*2 D1,TDFP,VPN
      INTEGER*2 INSMIP,TMIP,ENT,DFPK,RELP,ENTPNT
C
      REAL BX,BY,XMIN,YMIN,XMAX,YMAX,XDIST,YDIST
      INTEGER*2 WINDOW(4),ORGW(2)
      INTEGER*4 ST,TYPE,I,TDATA,NLEN,TEMPL, NAMEL
      INTEGER*4 TCOL
      INTEGER*4 TFCOL
      INTEGER*4 TBCOL 
	  INTEGER*4 BLEN
      INTEGER*2 TPLANE
      REAL VX1,VX2,VY1,VY2
      REAL WX1,WX2,WY1,WY2
      CHARACTER*(*) NAME
      CHARACTER*256 TEMP
      LOGICAL OK,FINISH
C
      EXTERNAL NLEN
C
C     allocate a bitmap for bitmap                    

      IF ( SERVER ) THEN
          ST = 0
          RETURN
      ENDIF
      TEMP = NAME(1:NLEN(NAME))//'.pic'
      CALL ALLOCBITFILE(TEMP,ST)
      IF(ST.NE.0) THEN
          GOTO 999
      ENDIF
C
C     save current limits
      VX1 = VXMIN
      VY1 = VYMIN
      VX2 = VXMAX
      VY2 = VYMAX
C     save current world limits
      WX1 = WXMIN
      WY1 = WYMIN
      WX2 = WXMAX
      WY2 = WYMAX
C     No do the create
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = BRSBX
      WINDOW(4) = BRSBY
      IF(TYPE.EQ.DAXDRG) THEN
C         set up as mono Set all values to explicit 
C         1 or 0 for bitmap file only
          TFCOL = COLFOR
          TBCOL = COLBAK
          TCOL = COLDRG
          COLDRG = 2
          TPLANE = HIPLAN
          HIPLAN = 0
          COLFOR = 1
          COLBAK = 0
C         simple drawing picture
C         do world change
          VX1 = VXMIN
          VY1 = VYMIN
          VX2 = VXMAX
          VY2 = VYMAX
          VXMIN = 0
          VYMIN = 0
          VXMAX = BRSBX
          VYMAX = BRSBY
          CALL SETW2V()
C SPB - 161194 - Always write out a drg file.
C     According to Dave Roberts of Intelliscan the commented-out 
C     line of code below is just to not bother writing a picture
C     file if there's no content to the drawing.   Unfortunately
C     this is always set and so it doesn't allocate the bitmaps
C     and then goes on to XwriteBitmapFile which then barfs with
C     a BadMatch error because the pixmap was not big enough ...
C
C          IF( LDFILE(CVPN) .EQ.1 ) GOTO 20
C
C         set file buffer bitmap
          CALL GPR_$SET_BITMAP(GENBIT,ST)
	  CALL GPR_$SET_FILL_VALUE(0,ST)
	  CALL GPR_$RECTANGLE(WINDOW,ST)
          DFPK=0
 10       CONTINUE
          DFPK=DFPK+1
          CALL RDISPF(DFPK,ENT,TMIP,OK)
          IF ( TMIP.GT.0 .AND. TMIP.LT.NMIPOS) CALL ALLDRW(ENT,TMIP)
          IF ( DFPK .LT. LDFILE(CVPN)-1 ) GOTO 10
C         Now do a pixel transfer from here to the file
C
C         dealocate all
 20       CONTINUE
      ELSEIF(TYPE.EQ.DAXCMP.OR.TYPE.EQ.DAXSYM.OR.TYPE.EQ.DAXPRT) THEN
C         Symbol or component to be drawn
C         check for data befor using
          IF(NDATA.EQ.0) GOTO 400
C         extract wrold coords
          COPYIT=.FALSE.
C         disable erase and draw of entities during transformation
          OPFLAG(1)=.FALSE.
          OPFLAG(2)=.FALSE.
          OPFLAG(3)=.FALSE.
          OPFLAG(4)=.FALSE.
          OPFLAG(8)=.FALSE.
          OPFLAG(9)=.FALSE.
          OPFLAG(10)=.FALSE.
C         go do the move to get the extents
C         recover ectents of this baby
          TDATA = NDATA
          CALL MOVMAS(.FALSE.)
          NDATA = TDATA
          XMAX = REFDAT(10,1)
          XMIN = REFDAT(9,1)
          YMAX = REFDAT(10,2)
          YMIN = REFDAT(9,2)
C         set world 
          XDIST = ABS(XMAX-XMIN)*0.03
          YDIST = ABS(YMAX-YMIN)*0.03
          WXMIN = XMIN-XDIST
          WYMIN = YMIN-YDIST
          WXMAX = XMAX+XDIST
          WYMAX = YMAX+YDIST
C         allow at least 1 in main area
          IF(XDIST.EQ.0.0) THEN
              WXMAX = WXMIN+WYMAX
          ELSEIF(YDIST.EQ.0.0) THEN
              WYMAX = WYMIN+WXMAX
          ENDIF
C         set bitmap dimensionns
          VXMIN = 0
          VYMIN = 0
          VXMAX = BRSBX
          VYMAX = BRSBY
C         set world
          CALL SETW2V()
C         get entities to draw from scratch file
          CALL GPR_$SET_BITMAP(GENBIT,ST)
	  CALL GPR_$SET_FILL_VALUE(0,ST)
	  CALL GPR_$RECTANGLE(WINDOW,ST)
          VPN = 0
C         set up as mono Set all values to explicit 
C         1 or 0 for bitmap file only
          TFCOL = COLFOR
          TBCOL = COLBAK
          TCOL = COLDRG
          COLDRG = 2
          TPLANE = HIPLAN
          HIPLAN = 0
          COLFOR = 1
          COLBAK = 0
C         switch on all layers
          CALL SAVLAY(VPN,OK)
          DO 200 I=0,255
              VLAYER(I) = .TRUE.
200       CONTINUE
          DO 100 I=1,NDATA
              CALL RSCRF(I,TMIP,BX,BY,TDFP,D1)
              IF ( TMIP .GT. 0 ) CALL ALLDRW(ENT,TMIP)
100       CONTINUE
         CALL GETLAY(VPN,OK)
      ENDIF
C     reset world please
      WXMIN = WX1
      WYMIN = WY1
      WXMAX = WX2
      WYMAX = WY2
      VXMIN = VX1
      VYMIN = VY1
      VXMAX = VX2
      VYMAX = VY2
      CALL SETW2V()
C
      WINDOW(1) = 0
      WINDOW(2) = 0
      WINDOW(3) = BRSBX
      WINDOW(4) = BRSBY
      ORGW(1) =0
      ORGW(2) =0
      COLDRG = TCOL
      COLFOR = TFCOL
      COLBAK = TBCOL
      HIPLAN = TPLANE
C     Use GPRX externsion to write out bitmap
	  BLEN=NLEN(TEMP)
      CALL GPR_$SET_BITMAP(GENBIT,ST)

      CALL GPR_$WRITE_BITMAP(GENBIT,TEMP,BLEN,ST)
400   CONTINUE
C
C
      CALL GPR_$SET_BITMAP(DISPDE,ST)
      CALL GPR_$DEALLOCATE_ATTRIBUTE_BLOCK(GENATT,ST)
      CALL GPR_$DEALLOCATE_BITMAP ( GENBIT,ST )
C                     
      RETURN
999   CONTINUE
      CALL DEPRNT(628)
C
      END
C
      SUBROUTINE GETFILEFROMBOX(BOXN,FILE,ST)
C     =======================================
C1    VARYPE                     I4  C*(*)I4
C1    IOSTAT                     I    O   O
C
C2    Recover a file from the current loded browser system.
C2  
C2  
C2    Arguments:-
C2  
C2    BOXN	->	Box number from browser current page
C2    FILE	->	Outgoing file name
C2  
C2  
C2    Error Returns:
C2  
C2    0		->	Success file in arg FILE
C2    -1	->	No file could be selected
C2  
      include   'include/interface.inc'
C
      INTEGER*4 BOXN
      INTEGER*4 ST
      INTEGER*4 NOP
      INTEGER*4 OFFSET
      INTEGER*4 I
      
      CHARACTER*(*) FILE
      CHARACTER*256 FILNAM

      ST = -1 
      IF ( BRPICS.EQ.0) THEN
C          Nothing loaded nothing could be detected
           GOTO 999
      ENDIF

      REWIND(BRUNIT)


      NOP = BRPICS/(NUMBX*NUMBY) + 1

      OFFSET = NUMBX*NUMBY*(BRPAGE-1)

      DO 10 I=1,OFFSET
          READ(BRUNIT,FMT='(A)',END=999,ERR=999) FILNAM
 10   CONTINUE


      DO 20 I=1,BOXN

          READ(BRUNIT,FMT='(A)',END=998,ERR=999) FILNAM

20    CONTINUE
      ST = 0
      FILE = FILNAM

998   CONTINUE
999   CONTINUE
      END

      SUBROUTINE DELETE_PIC(FILE,OK)
C     ==============================
C1    VARYPE               C*()  L
C1    IOSTAT                I    O
C
C2    This deletes the pic file asociated with the drawing
C2  
C2  
C2    Arguments:-
C2  
C2    FILE	->	The file with the pic extensions
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    TRUE	->	Deleted file OK
C2    FALSE	->	Either file does not exist or it cannot be deleted
C2  
      INCLUDE 'include/ftypes.inc'
C
      CHARACTER*(*) FILE
      CHARACTER*256 TEMP
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      LOGICAL OK
C
      EXTERNAL NLEN
C
      LENGTH = NLEN(FILE)
C
      IF ( LENGTH.EQ.0) THEN
           OK = .TRUE.
      ELSE
           TEMP = FILE(1:LENGTH)//'.pic'
           CALL DELETE(TEMP,OK)
      ENDIF

C
      END

      SUBROUTINE GENC(OK)
C     ===================
C1    VARYPE          L  
C1    IOSTAT          O  
C
C2    Convert file specifications to pic files
C2    Uses fileselector to get filespecs and types
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      include 'include/ftypes.inc'
      include 'include/swind.inc'

      LOGICAL OK
      LOGICAL QUIT
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      INTEGER*4 ID
      INTEGER*4 UNITS
      INTEGER*4 PHNUM
      INTEGER*4 TYPE
      CHARACTER*256 SELECTFILE
      CHARACTER*256 TMPFILE
      CHARACTER*256 BUFF
C
      EXTERNAL NLEN
C
      CALL FILESELECTOR(0,SELECTFILE,256,LENGTH,ST)
      IF ( ST.EQ.-1) THEN
          OK = .FALSE.
          QUIT= .TRUE.
          RETURN
      ENDIF
      IF ( ST.EQ.0.AND.NLEN(SELECTFILE).GT.0) THEN
          OK = .TRUE.
          QUIT = .FALSE.
      ELSE
          OK = .FALSE.
          QUIT= .FALSE.
          RETURN
      ENDIF

      CALL GETTMPFILE(TMPFILE,LENGTH,ID,ST)
      IF ( LENGTH.EQ.0) THEN
C         possible fault
          RETURN
      ENDIF
      CALL DIRFIN(TMPFILE(1:LENGTH),SELECTFILE,OK)

      CALL  FINDU1(UNITS,OK)

      OPEN(UNIT=UNITS,FILE=TMPFILE(1:LENGTH),ERR=999,IOSTAT=ST)

1000  CONTINUE
 
      READ(UNIT=UNITS,FMT='(A)',ERR=2000,END=2000) BUFF

      CALL CPRINT('Converting: '//BUFF)

      CALL INITDB()
      NDATA = 0
      TYPE = DAXTYP
      IF(TYPE.EQ.DAXDRG) THEN
          CALL LOAD_DRG(BUFF)
      ELSEIF(TYPE.EQ.DAXPRT) THEN
          CALL LOAD_PRT(BUFF)
      ELSEIF(TYPE.EQ.DAXCMP.OR.TYPE.EQ.DAXSYM) THEN
          TYPE = DAXTYP
          CALL LOAD_CMP(BUFF,TYPE)
      ENDIF
      GOTO 1000


2000  CONTINUE
      CLOSE(UNIT=UNITS,STATUS='KEEP')
      CALL INITDB()
      NDATA = 0


999   CONTINUE
      CALL TMPCLEANUP(ID,ST)
      END


C

      SUBROUTINE LOAD_CMP(FILNM,TYPE)
C     ===============================
C1    VARYPE             C*(*)    I4
C1    IOSTAT               I      I
C
C2    Converts either a DAXCAD symbol or component into PIC format
C2  
C2  
C2    Arguments:-
C2  
C2    FILNAM	->	Name of component or symbol
C2    TYPE	->	Type specifier DAXCMP or DAXSYM
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include    'include/masti.inc'
      include    'include/nbuff.inc'
      include    'include/filunit.inc'
      include    'include/ftypes.inc'
      include    'include/entity.inc'
      include    'include/swind.inc'
      include    'include/movdat.inc'
      include    'include/ndata.inc'
      include   'include/props.inc'
C
      LOGICAL OK
      CHARACTER*(*) FILNM,FILETP*80
      INTEGER*4 FN,TYPE,ST,WUN,TDATA,I
      INTEGER*2 TTMIP
      INTEGER*2 TTPDP
      INTEGER*2 TTTXP
      INTEGER*2 TTRLP
      INTEGER*2 TTPRP
      INTEGER*2 TTPCP,II
C
      WUN = 1
      IF(TYPE.EQ.DAXCMP)THEN
         FILETP='COMPM'
         FN = 1
      ELSEIF(TYPE.EQ.DAXSYM)THEN 
         FN = 2
         FILETP='SYMBM'
      END IF
C
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
      IF(.NOT.OK) RETURN
C
C     save the current record pointers
      TTMIP=NMIPOS
      TTPDP=NPDPOS
      TTTXP=NTXPOS
      TTRLP=NRLPOS
      TTPRP=NPRPOS
      TTPCP=NPCPOS
C     must go get the file and enter to database

      CALL INSP05(FILNM,FILETP,
     +       TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
C
C     master data being transformed to databae units
      DO 41 II=TTMIP,NMIPOS-WUN
C        save the MI pointer in work file.
         CALL DIR500(II,OK)
         IF((IMBUFF(1).EQ.SYMBM.OR.IMBUFF(1).EQ.COMPM)
     +   .OR.(IMBUFF(1).EQ.10.AND.(IMBUFF(2).EQ.COMPM.OR
     +   .IMBUFF(2).EQ.SYMBM)) ) THEN
C            Condition chosen if data is entity informnation
C            which is master but not instance
 
             IMBUFF(1)  = 10
C             modify to ordinary data
             CALL DIM500(II,OK)
             NDATA=NDATA+1
             WRITE(UNIT=SWINDU,REC=NDATA) II,0.0,0.0,0.0
C            master marked for move cannot do again
 
         ENDIF
 41   CONTINUE
C
      COPYIT= .FALSE.
C
      DO 10 I=1,10
          OPFLAG(I) = .FALSE.
10    CONTINUE
C
      TDATA = NDATA
C
      NDATA = TDATA
      DO 400 I=0,255
          VLAYER(I)= .TRUE.
400   CONTINUE
      CALL CREATEBROWSEIMAGE(FILNM,TYPE,ST)
99    CONTINUE

      END

      SUBROUTINE LOAD_PRT(FILNM)
C     ==========================
C1    VARYPE              C*(*)
C1    IOSTAT                I
C
C2    Convert a DAXCAD part file into PIC format
C2  
C2  
C2    Arguments:-
C2  
C2    FILNAM	->	Part filename
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include    'include/masti.inc'
      include    'include/nbuff.inc'
      include    'include/filunit.inc'
      include    'include/ftypes.inc'
      include    'include/entity.inc'
      include    'include/swind.inc'
      include    'include/movdat.inc'
      include    'include/ndata.inc'
      include    'include/props.inc'
C
      LOGICAL OK
      CHARACTER*(*) FILNM,FILETP*80
      INTEGER*4 FN,TYPE,ST,WUN,TDATA,I
      INTEGER*2 TTMIP
      INTEGER*2 TTPDP
      INTEGER*2 TTTXP
      INTEGER*2 TTRLP
      INTEGER*2 TTPRP
      INTEGER*2 TTPCP,II
C
      WUN = 1
      TYPE = DAXPRT
      FILETP='PART'
      CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
C
C
C     save the current record pointers
      TTMIP=NMIPOS
      TTPDP=NPDPOS
      TTTXP=NTXPOS
      TTRLP=NRLPOS
      TTPRP=NPRPOS
      TTPCP=NPCPOS
C     must go get the file and enter to database

      CALL INSP05(FILNM,FILETP,
     +       TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
C
C     master data being transformed to databae units
      DO 40 II=TTMIP,NMIPOS-WUN
C        save the MI pointer in work file.
         CALL DIR500(II,OK)
         IF(IMBUFF(1).EQ.10.AND..NOT.(IMBUFF(2)
     +    .EQ.COMPM.OR.IMBUFF(2).EQ.SYMBM).OR.IMBUFF(1).EQ.GROUP) THEN
C            Condition chosen if data is entity informnation
C            which is anything but master also instance
             NDATA=NDATA+1
             WRITE(UNIT=SWINDU,REC=NDATA) II,0.0,0.0,0.0
         ENDIF
C
40    CONTINUE
      COPYIT= .FALSE.
      DO 10 I=1,10
          OPFLAG(I) = .FALSE.
10    CONTINUE
      TDATA = NDATA
C
      NDATA = TDATA
C     set all layers on anyway
      DO 400 I=0,255
          VLAYER(I)= .TRUE.
400    CONTINUE
      CALL CREATEBROWSEIMAGE(FILNM,TYPE,ST)
99    CONTINUE

      END

      SUBROUTINE LOAD_DRG(TEMP)
C     =========================
C1    VARYPE               C*(*)
C1    IOSTAT                I
C
C2    Load and convert a drawing to pic format
C2  
C2  
C2    Arguments:-
C2  
C2    TEMP	->	Name of drawing
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include    'include/masti.inc'
      include    'include/nbuff.inc'
      include    'include/ndata.inc'
      include    'include/menun.inc'
      include    'include/entity.inc'
      include    'include/ftypes.inc'
      include    'include/filunit.inc'
C
      CHARACTER*(*) TEMP
      INTEGER*4 ST,TYPE,I
      INTEGER*2 II
      LOGICAL OK
C
C
      DAXTYP = DAXDRG
      CALL OPENNM(TEMP,PARFUN,.TRUE.,OK)
      CALL RSTALL(OK)
C     set all layers on please
      DO 400 I=0,255
          IF(TLAYER(I).GT.0) VLAYER(I)= .TRUE.
400    CONTINUE
C
      CALL REGEND()
C
      CALL CLEAR()
      TYPE = DAXDRG
      CALL CREATEBROWSEIMAGE(TEMP,TYPE,ST)
C
      END
C

