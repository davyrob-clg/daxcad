C
C         SCCS id Keywords             @(#)  412.1 date 6/11/92 inserth.f   
C
C
      SUBROUTINE CHKHLW(X1,Y1,X2,Y2,NUM,FORMI,FONT,YES)
C     ============================================
C
C1    vartype            R, R, R, R, I4,  I2, I2   L
C1    iostatus           I, I, I, I,  I    I   O   O
C
      include  'include/hdata.inc'
      include  'include/masti.inc'
      include  'include/ndata.inc'
      include  'include/nbuff.inc'
C
      REAL X1,Y1,X2,Y2,OX1,OY1,OX2,OY2,CD0D13
      INTEGER*4 NUM,POS,FORMD(8,2)
      INTEGER*2 FORMI,FONT
      LOGICAL YES
      EXTERNAL CD0D13
C
C     save last vector coords for matching next time
      SAVE OX1,OY1,OX2,OY2,POS
C     form control for hatch pattern within this array
      DATA FORMD/1,3,2,5,2,1,0,0,1,0,2,0,6,6,0,3/
C
C     test for old format hatch,which contains
C     no proper form control,must draw all vectors.
      YES=FORMI.EQ.0.OR.FORMI.EQ.1
      IF ( YES ) THEN
C        old hatch, get font and return
         FONT=IMBUFF(6)
         RETURN
      END IF
C
      IF ( NUM.EQ.1 ) THEN
C        first hatch line,save for test on next
         OX1=X1
         OY1=Y1
         OX2=X2
         OY2=Y2
         POS=1
         FONT=1
         YES=.TRUE.
         RETURN
      END IF
C
C     find distance between last vector and this one
C     to establish if this is part of the same vector
      IF ( CD0D13(OX1,OY1,OX2,OY2,X1,Y1).GT.(PAPTOW*HDIST/2.0)) THEN
C        save this vector for next test
         OX1=X1
         OY1=Y1
         OX2=X2
         OY2=Y2
         POS=POS+1
      END IF
C
C     test for validity of line draw within
C     the current hatching form
      YES=MOD(POS,FORMD(FORMI,1)).NE.0
C
      IF ( YES ) THEN
C        this is a non-specific line to be drawn
         FONT=1
      ELSE
C        this is a specific line to be drawn
C        using the definition stored in FORMD
         FONT=FORMD(FORMI,2)
         YES=FONT.GT.0
      END IF
C
      END
C
C     ---------------------------------------------------
C
 
      SUBROUTINE DRWHLW(X1,Y1,X2,Y2,NUM,FORM)
C     ============================================
C
C1    vartype            R, R, R  R, I4, I2
C1    iostatus           I  I  I  I  I   I
C
C2    Subroutine DRWHLW draws a hatch line using
C2    the control defined by the FORM of hatching
C2    to decide whether or not to display this
C2    particular line, and the font with which it
C2    should be displayed. NUM passes the line number
C2    within the hatch of this particular line.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
 
      REAL X1,Y1,X2,Y2
      INTEGER*4 NUM,I4
      INTEGER*2 FORM,FONT
      LOGICAL YES
C
C     check if line should be drawn within pattern
      CALL CHKHLW(X1,Y1,X2,Y2,NUM,FORM,FONT,YES)
      IF ( YES ) THEN
C         go draw with correct font
         I4=FONT
         CALL DRWFLW(X1,Y1,X2,Y2,I4)
      END IF
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE EHKHLW(NUM,FORM,FONT,YES)
C     ====================================
C1                       I4,  I2, I2   L
C1                        I    I   O   O
C2    draws a hatch line according to the following
C2    conditions
C2    The form type may vary
 
      include 'include/masti.inc'
 
      REAL X1,Y1,X2,Y2
      INTEGER*4 NUM
      INTEGER*2 FORM,FONT
      LOGICAL YES
 
      FONT=LSFONT
      YES=.TRUE.
 
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE ERSHLW(X1,Y1,X2,Y2,NUM,FORM,LFONT)
C     ============================================
C1                       R, R, R  R, I4, I2   I2
C2    draws a hatch line according to the following
C2    conditions
C2    The form type may vary
      include 'include/masti.inc'
 
      REAL X1,Y1,X2,Y2
      INTEGER*4 NUM,TFONT
      INTEGER*2 FORM,FONT,LFONT
      LOGICAL YES
 
      IF ( FORM .EQ. 1 ) THEN
         TFONT=LFONT
         CALL DRWFLW(X1,Y1,X2,Y2,TFONT)
      ELSE
         CALL EHKHLW(NUM,FORM,FONT,YES)
         IF ( YES ) THEN
            TFONT=CLFONT
            CALL DRWFLW(X1,Y1,X2,Y2,TFONT)
         END IF
      END IF
 
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE HATCHB
C     =================
C
C     Main hatching routine
C
C
C1    No Arguments used
C
C2     Open temporary direct access file for storing
C2     the boundary which has been indicated by the user
C2     The storage within the file is as follows
C2
C2      2 Bytes   2 Bytes    4 Bytes    4 Bytes
C2      =======   =======    =======    =======
C2        MIP     Entity     X-coord    Y-coord
C2                 Type      of flags   of flags
C2                            world  coordidates
C2
C2      Next  20 Bytes store the modified boundary
C2      Next  20 Bytes store the original boundary
C2
C2      Storage for a line
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      X1coord   Y1coord   X2coord   Y2coord     0.0
C2
C2      Storage for a arc
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      X coord   Y coord   Radius     Start     End
C2      centre    center               Angle     Angle
C2
C2    
C2    ********************
C2    VIEWPORT INFORMATION
C2    ********************
C2    
C2    This section handles the canceling of views as well as drawing
C2    Initially the hatch is drawn into the current display and not into
C2    into any storage bitmaps 
C2    If the user changes viewport before accepting then the hatch will
C2    not be deleted This may affect the screen but the over head of doing this
C2    is too much. In MAWS it wont matter anyway. The entity will drawn into all
C2    views at ACCEPT time
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/hdata.inc'
      include   'include/ndata.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/lfont.inc'
      include   'include/viewport.inc'
      include   'include/gtxt2.inc'
      include   'include/lfu.inc'
      include   'include/vntable.inc'
C
      INTEGER*2 TMIP,TFONT,LFORM,FN,TVPN,
     +          TNMIP,TNPDF,PD,P3,ENT,TLDFIL,HVPN
      REAL      X,Y,X1,DISTXY,RAD,M(3,3),SX,SY
      REAL BUFF1(6),BUFF2(6)
      INTEGER*4 C,HK,REC,I,TCELL,TMEN,DMEN,DCELL,I1,TOTAL
      LOGICAL   OK,CHANGE,YESOK,VISAB,ADDED,GO,SAME,ABORT 
      LOGICAL   CONFRM
      CHARACTER*20 ANS*3
      INTEGER*2 DFP1
      INTEGER*4 ST
C
      EXTERNAL DISTXY,RAD,ERSFLW,DRWFLW,FNDPOS,FNDTOK,GTDMEN,YESOK
C
      ABORT = .FALSE. 
      CONFRM = .FALSE.
C
C     Open temporary file needed
      DMEN = 3
      DCELL = 20
3000  CONTINUE
      REC=52
      CALL OURSCR(HUNIT,REC,OK)
      IF ( .NOT. OK ) THEN
C        This would be a strange thing but it could happen
         CALL DCPRNT(143)
         CALL DEPRNT(90)
         RETURN
      END IF
C
 11   CONTINUE
C
C     set number of ents in scractch file
      NO=0
C     daxport flag
      ADDED=.FALSE.
C     fault handler
      CALL ENTDAT(.TRUE.)
      TNMIP=NMIPOS
      TNPDF=NPDPOS
      TLDFIL=LDFILE(CVPN)
C     ensure current hatch values in menu
C
C     Form type
      I=FORM-1
      GTMULT = .TRUE.
      CALL GTDMWT(128,3,VNOUN(I+531))
C     Find menu cell which has been set for NEW BOUNDARY
      CALL GTDMEN(122,3)
C     Find menu cell which has been set for AREA
      CALL GTDMEN(120,3)
C     Find menu cell which has been set for CANCEL
      CALL GTDMEN(121,3)
C     Find menu cell which has been set for HATCH DISTANCE
      CALL GTDMWR(123,3,HDIST,'(F8.2)')
C     Find menu cell which has been set for HATCH ANGLE
      CALL GTDMWR(124,3,HANG,'(F8.2)')
C     Find menu cell which has been set for CROSS HATCHING
      CALL FNDPOS(125,C)
      IF ( CROSSH ) THEN
         CALL GTMCHI(3,C)
      ELSE
         CALL GTMCLO(3,C)
      END IF
C
      CALL GTDMWR(127,3,CHANG,'(F8.2)')
CC
 3    CONTINUE
      HK=1
      FN=1
C     go get hatch boundary
      CALL PULENT(FN)
C     store current viewport hatch is being drawn into
      HVPN = CVPN
C
      IF ( NO.EQ.0.OR.MEN.EQ.2 .OR.
     +     CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C           Get rid of temporary scratch files
C           erase the existing profile
            CALL ERSCRS(.TRUE.)
            CALL ERSPRF()
C           make sure we cannot draw again
            NO = 0
C           reset master index pointers
            NMIPOS=TNMIP
            NPDPOS=TNPDF
C           reset display file pointers unlesss view changed
            CLOSE(UNIT=HUNIT)
CIBM
C            LFU(HUNIT)=.FALSE.
CIBM
            DDCODE = 0
            RETURN
      END IF
C
C     find the limits of the hatch area
      CALL MAXMIN()
C
      NMIPOS=TNMIP
      NPDPOS=TNPDF
C     Storage of Hatching lines
      CALL FILL()
      IMBUFF(2)=  HATCH
      IMBUFF(5)=  FORM
C     ensure current line font applied
      IMBUFF(6)=  CLFONT
      IMBUFF(12)= THICK
      IDBUFF(1)=  HATCH
      IDBUFF(2)=  NMIPOS-1
      IDBUFF(4)=  SPARE
      RDBUFF(1)=  XMIN
      RDBUFF(2)=  YMIN
      RDBUFF(3)=  0.0
      RDBUFF(4)=  XMAX
      RDBUFF(5)=  YMAX
      RDBUFF(6)=  0.0
C     display file gets incremented
      CALL DEW501(TMIP,OK)
C
 5    CONTINUE
      NPDPOS=TNPDF+1
C     Number of hatch lines stored
      HK=1
C
C
C     check that there is a sensible box for the hatch to use
C     ie the box could be a point at a two circle tangency point
C
C     set the flag that it is ok to go and hatch
      GO =.TRUE.
      IF( SAME(XMIN,XMAX) .OR. SAME(YMIN,YMAX)) THEN
C        Oh well no hatch lines stored
         GO=.FALSE.
      ELSE
C        Right so we've got a hatchable area
C
C        check if he is going to hatch something off the screen and if so
C        ask him if he`s sure about it.
         CALL CHKBOX(XMIN,YMIN,XMAX,YMAX,TOTAL,OK)
         IF(TOTAL .NE. 0) THEN
C          oh well some of the hatch is off screen
C          ask him if he still wants to do it
           CALL DPRMXP(614,ANS)
           IF(.NOT.(YESOK(ANS))) THEN
C          no hatch lines stored
               GO=.FALSE.
           ENDIF
         ENDIF
C
C        right do the hatch if ok
         IF(GO) THEN
           IF(MVPACT.AND..NOT.MAWS) THEN
               DDCODE = 2
           ENDIF
           CALL HBLINE(HK)
           IF ( CROSSH ) THEN
              IF ( HANG+CHANG .GT. 180 ) THEN
                X1=-CHANG
              ELSE
                X1=CHANG
              END IF
              HANG=HANG+X1
              HK=HK+1
              CALL HBLINE(HK)
              HANG=HANG-X1
           END IF
           DDCODE = 0
         ENDIF
      ENDIF
C 
C     well if we did not do the hatch set the hatch vector count to zero
C
      IF(.NOT. GO) HK=0
C
C     modify the changes to last point
      IF ( HK .GT. 0 ) THEN
          PD=NPDPOS-1
          IDBUFF(3)=0
          CALL DBM500(PD,OK)
      ENDIF
      ENT=HATCH
C
C     Clear Boundary option from menu.
      CALL FNDPOS(122,C)
      CALL FNDTOK(122,ANS)
      CALL GTCLRC(3,C)
C
C     Clear AREA option from menu.
      CALL FNDPOS(120,C)
      CALL FNDTOK(120,ANS)
      CALL GTCLRC(3,C)
C
C     Clear cancel.
C      CALL FNDPOS(121,C)
C      CALL FNDTOK(121,ANS)
C      CALL GTCLRC(3,C)
C
      CHANGE=.FALSE.
      TFONT=CLFONT
      LFORM=FORM
C
 15   CONTINUE
C
      CALL DCPRNT(145)
 16   CONTINUE
C
C     delete hatch
C      CALL GTDMEN(454,3)
      CALL TCURS(C,X,Y)
      TCELL = CELLN
      TMEN = MEN
C
 
c      CALL WO2SC(X,Y,SX,SY)
c      CALL WRKHIT(SX,SY,OK)
c c     IF(OK) THEN
c          CALL DEPRNT(117)
c          GOTO 15
c      ENDIF
      IF ( MEN .NE. 3 ) THEN
	 CALL CONFIRMATION(DICT01(146),.FALSE.,CONFRM)
C        
         IF (CONFRM) THEN
C           make sure we cannot draw again
            NO = 0
C           use cancel entity routine to cancel out of that viewport
            CALL ENTCAN(TMIP,ST)
            IF(HVPN.EQ.CVPN) THEN
C               erase the existing profile
                CALL ERSCRS(.TRUE.)
                CALL ERSPRF()
C               draw out of curent bitmap
                IF(MVPACT.AND..NOT.MAWS) THEN
                    DDCODE = 2
                ENDIF
                CALL PENERS()
                CALL ALLDRW(ENT,TMIP)
                CALL PENDRW()
            ENDIF
C           User does not want to save hatching
C           Get rid of temporary scratch files
            NMIPOS=TNMIP
            NPDPOS=TNPDF
C           reset viewport please
            CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
 
C           give us another hatch
            CALL GTMCLO(TMEN,TCELL)
            IF(MEN .EQ. 0) THEN
C              if the guy did a screen hit to get here then run round again
               GOTO 3000
            ELSE
C              back for a new insert option
               DDCODE = 0
               RETURN
            ENDIF
         END IF
         CALL GTMCLO(MEN,CELLN)
         GOTO 15
      END IF
C
      IF ( MEN .EQ. 3 ) THEN
C        Store this because they might change
C        the font as an option
         CALL HATCHM(OK)
         IF ( OK ) then
            CHANGE=.TRUE.
            GOTO 15
         END IF
         IF ( CCMD.EQ.CHAR(150).AND.CHANGE) THEN
            IF(HVPN.EQ.CVPN) THEN
C               draw out of curent bitmap
                IF(MVPACT.AND..NOT.MAWS) THEN
                    DDCODE = 2
                ENDIF
                CALL PENERS()
                CALL ALLDRW(ENT,TMIP)
                CALL PENDRW()
            ENDIF
            CALL GTMCLO(MEN,CELLN)
            OK=.FALSE.
C           Check for changes
            IF (   FORM.NE.IMBUFF(5)  ) THEN
               IMBUFF(5)=FORM
               OK=.TRUE.
            END IF
            IF (  COLOUR.NE.IMBUFF(3) ) THEN
               IMBUFF(3)=COLOUR
               OK=.TRUE.
            END IF
            IF (  CLFONT.NE.IMBUFF(6) ) THEN
               IMBUFF(6)=CLFONT
               OK=.TRUE.
            END IF
            IF (  THICK.NE.IMBUFF(12) ) THEN
               IMBUFF(12)=THICK
               OK=.TRUE.
            END IF
            IF (  CLAYER.NE.IMBUFF(4) ) THEN
               IMBUFF(4)=CLAYER
               OK=.TRUE.
            END IF
            IF ( OK )  CALL DIM500(TMIP,OK)
            GOTO 5
      ELSE IF (CCMD .EQ.CHAR(149) ) THEN
C           use cancel entity routine to cancel out of that viewport
            CALL ENTCAN(TMIP,ST)
C           draw out the old hatch
            IF(HVPN.EQ.CVPN) THEN
C               erase profiles
                CALL ERSCRS(.TRUE.)
                CALL ERSPRF()
C               draw out of curent bitmap
                IF(MVPACT.AND..NOT.MAWS) THEN
                    DDCODE = 2
                ENDIF
                CALL PENERS()
                CALL ALLDRW(ENT,TMIP)
                CALL PENDRW()
            ENDIF
C           Get rid of temporary scratch files
            NMIPOS=TNMIP
            NPDPOS=TNPDF
            DMEN = TMEN
            DCELL = TCELL
            CALL GTCLRC(TMEN,TCELL)
C           go back and try again
            GOTO 11
         ELSE IF ( .NOT.(CCMD.EQ.CHAR(150).AND..NOT.CHANGE)) THEN
            CALL DEPRNT(131)
            CALL GTMCLO(MEN,CELLN)
            CHANGE=.FALSE.
            GOTO 16
         END IF
      END IF
C
      IF ( HK .EQ. 0 ) THEN
         CALL GTMCLO(MEN,CELLN)
         CALL DEPRNT(210)
         GOTO 5
      END IF
C     Time to add hatch to any other viewports Small overhead
C     in that it draws owver the old hatch but there we are
      IF(HVPN.NE.CVPN) THEN
C         cancel element out of display file
          TVPN = CVPN
          CVPN = HVPN
          CALL ENTCAN(TMIP,ST)
          CVPN = TVPN
C         add the to the new current display file
          CALL ADDISP(TMIP,ENT,DFP1,OK)
      ENDIF
C
      VPADD = .TRUE.
      CALL ALLDRW(ENT,TMIP)
      VPADD = .FALSE.
      CALL GTCLRC(DMEN,DCELL)
      CALL GTMCLO(MEN,CELLN)
C     Go for another hatch
C     erase the existing profile
      CALL ERSCRS(.TRUE.)
      CALL ERSPRF()
C     make sure we cannot draw again
      NO = 0
C     fault handler
      CALL ENTDAT(.FALSE.)
      GOTO 11
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE HATCHM(OK)
C     =====================
C2    No arguments used
C
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/lfont.inc'
      include  'include/nbuff.inc'
      include  'include/hdata.inc'
      include  'include/style.inc'
      include  'include/offset.inc'
      include   'include/gtxt2.inc'
      include   'include/vntable.inc'
C
      LOGICAL OK,QUIT,MOK,CVERFY
      DOUBLE PRECISION DIST,DN
      INTEGER*4 NLEN,I4,MNUM,TMEN,TCELL,I
      CHARACTER ANS*1,STRING*80
      INTRINSIC REAL,MOD
C
      EXTERNAL NLEN,GTMCLO,AEXPRN,GTMCWI,GTCLRC,GTPMEN
     +         CVERFY
C
      OK=.TRUE.
      TMEN = MEN
      TCELL = CELLN      
C
      IF ( CCMD .EQ. 'X') THEN
         IF ( CROSSH ) THEN
            CALL GTMCLO(MEN,CELLN)
            CROSSH=.FALSE.
         ELSE
            CROSSH=.TRUE.
         END IF
C
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      ELSE IF (CVERFY(CCMD,'=fk')) THEN
C     ****************************
C       Change line attributes.  
C     ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(TMEN, TCELL)                                  
C
      ELSE IF (CCMD.EQ.'F') THEN
C******************************************************
C                Hatch Form                           *
C******************************************************
            LSFORM=FORM
C           Get the new line font number
            MNUM = 19
            CALL MENPOP(MNUM,OK)
            IF (OK) THEN
               FORM = ICHAR(CCMD)
C              update the font number in the cell
               I=FORM-1
               GTMULT = .TRUE.
               CALL GTDMWT(128,3,VNOUN(I+531))
            ELSE
               OK = .TRUE.
               CALL GTMCLO(TMEN,TCELL)
            ENDIF
C           end of line font option control block
C
      ELSE IF ( CCMD .EQ. 'H' ) THEN
 111     CONTINUE
         CALL DPRMXP(163,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C          evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DIST,*111)
            IF ( DIST .GT. 0.14 ) THEN
               HDIST=REAL(DIST)
C              Find menu cell which has been set for HATCH DISTANCE
               CALL FNDTOK(123,ANS)
               CALL GTMCWR(3,ANS,HDIST,'(F8.2)')
            ELSE
C              prompt user that zero radius is illegal
C              and try again
               CALL DEPRNT(121)
               GOTO 111
            END IF
C
         END IF
      ELSE IF ( CCMD .EQ. 'V' ) THEN
C        relative angle for crosshatching
 113     CONTINUE
         CALL DPRMXP(188,CBUFF)
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C          evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DIST,*113)
            IF ( DIST .LT. 180.AND.DIST.GT.0.0 ) THEN
               CHANG=REAL(DIST)
C              Find menu cell which has been set for HATCH DISTANCE
               CALL GTDMWR(127,3,CHANG,'(F8.2)')
            ELSE
C              prompt user that zero radius is illegal
C              and try again
               CALL DEPRNT(211)
               GOTO 113
            END IF
C
         END IF
C
      ELSE IF ( CCMD .EQ. 'A' ) THEN
C
 112     CONTINUE
         CALL DPRMXP(188,CBUFF)
C
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DIST,*112)
            HANG=REAL(DIST)
            IF ( HANG.LT.0 ) THEN
               CALL DCPRNT(165)
               HANG=180+HANG
            ELSE IF ( HANG .GE. 180 ) THEN
               CALL DCPRNT(166)
               HANG=HANG-180
            END IF
C           Find menu cell which has been set for HATCH ANGLE
            CALL FNDTOK(124,ANS)
            CALL GTMCWR(3,ANS,HANG,'(F8.2)')
         END IF
C
      ELSEIF(CCMD.EQ.'O') THEN
C         Used for offseter
2010      CONTINUE
          CALL DPRMXP(529,STRING)
          IF(NLEN(STRING).EQ.0) THEN
              GOTO 2000
          ELSE
C             calculate the offsets again
              CALL CRTOF3(NUMOFF,NUMARR,NUMP,
     +                        MULTP,STRING,OK,QUIT)
          ENDIF
          CALL GTMCLO(MEN,CELLN)
          IF(.NOT.OK) GOTO 2010
 2000    CONTINUE
      ELSEIF(CCMD.EQ.'L') THEN
C         target layer control for offseter or anything elae you like
          OFFLAY = .NOT.OFFLAY
          IF(OFFLAY) THEN
 222          CALL DPRMXP(265,STRING)
              IF (NLEN(STRING).EQ.0)THEN
                  TARGLY = CLAYER
                  OFFLAY = .FALSE.
              ELSE
                  CALL AEXPRN(STRING,DN,*222)
C                 save the target layer number
                  TARGLY = DN
                  IF ( TARGLY .LT.0.OR.TARGLY.GT.255 ) THEN
                      CALL DEPRNT(134)
                      GOTO 222
                  ELSEIF(TARGLY.EQ.CLAYER) THEN
                      OFFLAY = .FALSE.
                  END IF
              ENDIF
          ELSE
C             set work target layer to work layer
              TARGLY = CLAYER
          ENDIF
          CALL GTMCWI(3,CCMD,TARGLY)
C         ensure cell hilited
          IF(OFFLAY) THEN
             CALL GTMCHI(MEN,CELLN)
          ELSE
C            cell loaded if no good
             CALL GTMCLO(MEN,CELLN)
          ENDIF
      ELSE
C        set OK .false. to indicate that HATCHM
C        does not process this CCMD
         OK=.FALSE.
      END IF
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE HBLINE(HK)
C     =====================
C1
C2
C2     The storage within the file is as follows
C2     this is modified since the modified boundary
C2     is all that is used now
C2     The first 10 Bytes stay as they are
C2
C2      2 Bytes   2 Bytes    4 Bytes    4 Bytes
C2      =======   =======    =======    =======
C2        MIP     Entity     X-coord    Y-coord
C2                 Type      of flags   of flags
C2                            world  coordidates
C2
C2      The next 20 Bytes which held the original
C2      boundary is now used by HBLINE to hold
C2      the line vectors of each individual boundary
C2      line arcs have to be used in Cartesian form
C2      so it does not change thier values
C2      Storage for a line vector
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      L1        L2        L3
C2      Storage for a arc
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      X coord   Y coord   Radius     Start     End
C2      centre    centre               Angle     Angle
C2
C2      Next  20 Bytes store the modified boundary
C2      Storage for a line
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      X1coord   Y1coord   X2coord   Y2coord    0.0
C2      Storage for a arc
C2      4 Bytes   4 Bytes   4 Bytes   4 Bytes   4 Bytes
C2      =======   =======   =======   =======   =======
C2      X coord   Y coord   Radius     Start     End
C2      centre    center               Angle     Angle
C
      include  'include/masti.inc'
      include  'include/hdata.inc'
      include  'include/ndata.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      REAL X1,Y1,X2,Y2,XYP(1:400,1:2),RAD,BX,BY,X,Y,REAL
C
      DOUBLE PRECISION DHDIST,L1,L2,L3,M1,M2,M3,DIST,D1,D2,D3,
     1      C1,C2,C3,N1,N2,N3,DYMIN,DYMAX,DXMIN,DXMAX,XP,YP,
     2      DVDD13,DRAD,XP1,YP1,XF,YF,DBLE
      LOGICAL STORIT,OK,ARCTPT,SAME,CHKLN,HDIR,DSTOP
      INTEGER*2 TMIP,TPDP,DP
      INTEGER*4 HK,HK1,K,I,J,CT,P,P1,SUM
      INTRINSIC DBLE,REAL
      EXTERNAL DVDD13,ARCTPT,RAD,DRWFLW,SAME,
     +         DCVL14,DVV0L5,DVVL15,DVC0P5,DVC0P9,RSWAP,DSTOP
C
C
      TPDP = NPDPOS
      HDIR=.TRUE.
      DXMIN=DBLE(XMIN)
      DYMIN=DBLE(YMIN)
      DXMAX=DBLE(XMAX)
      DYMAX=DBLE(YMAX)
      XF=DYMAX
C
C     Condition no. 4
C     Creating horizontal line
      CALL DCVL14(DXMIN,DYMAX,DXMAX,XF,L1,L2,L3)
      XF=DXMAX
      YF=DYMIN
      XP1 = 0.0
      XP = 0.0
      YP1 = 0.0
      YP = 0.0
C
      CALL DVV0L5(L1,L2,L3,DXMIN,DYMAX,DBLE(RAD(HANG)),M1,M2,M3)
      DIST=DVDD13(M1,M2,M3,DXMIN,DYMIN)
C     Condition no. 5
      IF ( DIST .LT. 0 ) THEN
C     Origin is bottom left therefore finish top right
         XF=DXMAX
         YF=DYMAX
         CALL DVV0L5(L1,L2,L3,DXMIN,DYMIN,DBLE(RAD(HANG)),M1,M2,M3)
      END IF
      DHDIST=DBLE(PAPTOW*HDIST)
C
 30   CONTINUE
C     Keyboard stoppeer
      IF(DSTOP('Q',500) ) RETURN
C     Condition No. 6
C                 original              right    left
C                 vector                vector   vector
      CALL DVVL15(M1,M2,M3,DHDIST,C1,C2,C3,N1,N2,N3)
C     Condition no. 7
      DIST=DVDD13(C1,C2,C3,XF,YF)
C      Condition no. 9
      IF ( DIST .LT. 0.0 ) THEN
C     Hatch counter is set to next availible postion so
C     HK=HK-1 is the actual number of hatch lines stored
         HK=HK-1
         RETURN
      END IF
C     Number of intersection points stored
      K=1
      DO 40 I=1,NO,1
         READ(UNIT=HUNIT,REC=I) MIP1,ENT1,BX,BY,
     +    XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1    XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
         IF (  ENT1  .EQ.  LINE  ) THEN
C     Condition no. 3
C     Generate line in vector form
            CALL DCVL14(DBLE(XY(1,1)),DBLE(XY(2,1)),
     +                  DBLE(XY(3,1)),DBLE(XY(4,1)),D1,D2,D3)
C         Condition no. 8a
            CALL DVC0P5(C1,C2,C3,D1,D2,D3,XP,YP,OK)
            IF ( OK ) THEN
               X=REAL(XP)
               Y=REAL(YP)
C         Condition no. 8b
C         Test for legitimate point
               STORIT=CHKLN(XY(1,1),XY(3,1),X).AND.
     +                CHKLN(XY(2,1),XY(4,1),Y)
C         Condition no. 8c
               IF ( STORIT ) THEN
                     XYP(K,1)=X
                     XYP(K,2)=Y
                     K=K+1
                     DO 1020 DP=1,K -2
                        IF(SAME(XYP(DP,1),X).AND.
     +                      SAME(XYP(DP,2),Y)) THEN
C                            take the last point off we dont want it
                             K = K-1
                        ENDIF
 1020                CONTINUE
               ENDIF
            END IF
         ELSE IF ( ENT1  .EQ. ARC ) THEN
            D1=DBLE(XY(1,2))
            D2=DBLE(XY(2,2))
            D3=DBLE(XY(3,2))
            CALL DVC0P9(D1,D2,D3,C1,C2,C3,XP,YP,XP1,YP1,OK)
C      WRITE(10,*) '******HBLINE******************************'
C      WRITE(10,*) 'XC,YC,RAD',XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
C      WRITE(10,*) 'OK,POINTS',OK,XP,YP,XP1,YP1
C
 
            IF(OK) THEN
               OK=.NOT.(SAME(REAL(XP),REAL(XP1)).AND.SAME(REAL(YP),
     +         REAL(YP1)))
            ENDIF
 
            IF ( OK ) THEN
               X=REAL(XP)
               Y=REAL(YP)
               IF ( ARCTPT(XY(1,1),XY(2,1),XY(4,1),XY(5,1),
     +                     X,Y)) THEN
                  XYP(K,1)=X
                  XYP(K,2)=Y
                  K=K+1
                  DO 1000 DP=1,K -2
                      IF(SAME(XYP(DP,1),X).AND.
     +                   SAME(XYP(DP,2),Y)) THEN
C                         take the last point off we dont want it
                          K = K-1
                  ENDIF
 1000             CONTINUE
               END IF
               X=REAL(XP1)
               Y=REAL(YP1)
               IF ( ARCTPT(XY(1,1),XY(2,1),XY(4,1),XY(5,1),
     +                           X,Y)) THEN
                  XYP(K,1)=X
                  XYP(K,2)=Y
                  K=K+1
                  DO 1010 DP=1,K -2
                      IF(SAME(XYP(DP,1),X).AND.
     +                   SAME(XYP(DP,2),Y)) THEN
C                         take the last point off we dont want it
                          K = K-1
                  ENDIF
 1010             CONTINUE
               END IF
            END IF
         END IF
C        test for duplicate point
 40   CONTINUE
C
      IF ( K .GT. 1 ) THEN
C
C     Condition no. 8d
         IF ( HANG .LT. 90 ) THEN
C     Sort by x coordinate
C
            DO 50 I=1,K-2
               DO 60 J=I+1,K-1
                  IF ( XYP(J,1) .LT. XYP(I,1) ) THEN
                     CALL RSWAP(XYP(J,1),XYP(I,1))
                     CALL RSWAP(XYP(J,2),XYP(I,2))
                  END IF
 60            CONTINUE
 50         CONTINUE
C
         ELSE
C
            DO 80 I=1,K-2
               DO 90 J=I+1,K-1
                  IF ( XYP(J,2) .LT. XYP(I,2) ) THEN
                     CALL RSWAP(XYP(J,1),XYP(I,1))
                     CALL RSWAP(XYP(J,2),XYP(I,2))
                  END IF
 90            CONTINUE
 80         CONTINUE
C
         END IF
C        Number of new hatch lines to be stored
         HK1=((K-1)/2)
C
         IF ( HDIR ) THEN
C           this will store the hatch line
C           in reverse order
            J=1
            SUM=2
         ELSE
            J=HK1*2-1
            SUM=-2
         END IF
         HDIR=.NOT.HDIR
C        Condition no. 8e
         P=HK
         P1=0
         DO 70 I=HK,HK+HK1-1,1
            IF ( .NOT. ( SAME( XYP(J,1),XYP(J+1,1) ) .AND.
     +                   SAME( XYP(J,2),XYP(J+1,2) ) ) )  THEN
               RDBUFF(1)=XYP(J,1)
               RDBUFF(2)=XYP(J,2)
               RDBUFF(4)=XYP(J+1,1)
               RDBUFF(5)=XYP(J+1,2)
               CALL DBW501(TMIP,OK)
               IF(.NOT.OK) THEN
                   NPDPOS = TPDP
                   HK = 0
                   RETURN
               ENDIF
               CALL DRWHLW(XYP(J,1),XYP(J,2),XYP(J+1,1),
     +                     XYP(J+1,2),I,FORM)
               P=P+1
               P1=P1+1
            END IF
            J=J+SUM
 70      CONTINUE
C        New total number of hatch lines (+1)
         HK=HK+P1
      END IF
C
C     Condition no. 9
C     New line is tranfered for
      M1=C1
      M2=C2
      M3=C3
C
      GOTO 30
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE INTERS(ENT1,X1,X2,X3,X4,X5,X6,
     +                  ENT2,Y1,Y2,Y3,Y4,Y5,Y6,
     1                  Z1, Z2, Z3, Z4, Z5, Z6,Z7,Z8,ST, OK)
C     =====================================
C
      include   'include/entity.inc'
C
      REAL X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8
      REAL X5,X6,Y5,Y6
      DOUBLE PRECISION  DX1,DX2,DX3,DX4,DY1,DY2,DY3,DY4,
     +                  XP,YP,XP1,YP1,
     +                  DX5,DX6,DY5,DY6,
     +                  XP2,YP2,XP3,YP3
      LOGICAL OK
      INTEGER*2 ENT1,ENT2
      INTEGER*4 ST
      INTRINSIC REAL,DBLE
C
      EXTERNAL DCC0P5,DCC0P9,DCCP19
C
      DX1=DBLE(X1)
      DX2=DBLE(X2)
      DX3=DBLE(X3)
C
      DY1=DBLE(Y1)
      DY2=DBLE(Y2)
      DY3=DBLE(Y3)
C
C     assignment of unitialised variables was causing
C     a crash so!
      XP  = 0.0
      YP  = 0.0
      XP1 = 0.0
      YP1 = 0.0
      XP2 = 0.0
      YP2 = 0.0
      XP3 = 0.0
      YP3 = 0.0
C
      IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. LINE ) THEN
C        First and Second  is a line.
         DX4=DBLE(X4)
         DY4=DBLE(Y4)
         CALL DCC0P5(DX1,DX2,DX3,DX4,DY1,DY2,DY3,DY4,XP,YP
     +      ,OK)
         ST = 1
      ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. LINE ) THEN
         DY4=DBLE(Y4)
         DX5=DBLE(X5)
         DX6=DBLE(X6)
         CALL MCC0P9(DX1,DX2,DX3,DX5,DX6,DY1,DY2,DY3,DY4,
     +      XP, YP, XP1,YP1, OK )
         IF ( OK ) THEN
            ST = 2
            Z3=REAL(XP1)
            Z4=REAL(YP1)
         END IF
      ELSE IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. ARC ) THEN
         DX4=DBLE(X4)
         DY5=DBLE(Y5)
         DY6=DBLE(Y6)
         CALL MCC0P9(DY1,DY2,DY3,DY5,DY6,DX1,DX2,DX3,DX4,
     +      XP, YP, XP1,YP1, OK )
         IF ( OK ) THEN
            ST = 2
            Z3=REAL(XP1)
            Z4=REAL(YP1)
         END IF
      ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. ARC ) THEN
         DX5=DBLE(X5)
         DX6=DBLE(X6)
         DY5=DBLE(Y5)
         DY6=DBLE(Y6)
         CALL MCCP19(DX1,DX2,DX3,DX5,DX6,DY1,DY2,DY3,
     +      DY5,DY6,XP,YP,XP1,YP1,XP2,YP2,XP3,YP3,ST,OK)
         IF ( OK ) THEN
            Z3=REAL(XP1)
            Z4=REAL(YP1)
            Z5=REAL(XP2)
            Z6=REAL(YP2)
            Z7=REAL(XP3)
            Z8=REAL(YP3)
         END IF
      END IF
      IF ( OK ) THEN
         Z1=REAL(XP)
         Z2=REAL(YP)
      END IF
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE MAXMIN()
C     ===================
C
      include   'include/hdata.inc'
      include   'include/entity.inc'
C
      INTEGER*2 I
      DOUBLE PRECISION D1,D2,D3
      REAL  BX,BY,X1,Y1,X2,Y2,XYB(5,2)
      EXTERNAL RSWAP,ARCBOX,UNFLAG
C
      READ(UNIT=HUNIT,REC=1) MIP1,ENT1,BX,BY,
     +    XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1    XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
C
C      WRITE(UNIT=10,FMT='(A,I5)') 'Boundaries stored ',NO
      IF ( ENT1  .EQ. LINE ) THEN
C        Line
         XMAX=XY(1,1)
         YMAX=XY(2,1)
         XMIN=XY(3,1)
         YMIN=XY(4,1)
         IF ( XMIN .GT. XMAX ) THEN
            CALL RSWAP(XMIN,XMAX)
         END IF
         IF ( YMIN .GT. YMAX ) THEN
            CALL RSWAP(YMIN,YMAX)
         END IF
      ELSE IF ( ENT1 .EQ. ARC ) THEN
C        Arc
         CALL  ARCBOX(XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     +                   XMIN,YMIN,XMAX,YMAX)
C
      END IF
C
C     WRITE(UNIT=10,FMT=*)' BOUNDARY STORED'
      DO 10 I=1,NO,1
         READ(UNIT=HUNIT,REC=I) MIP1,ENT1,BX,BY,
     +    XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1    XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
C
C      WRITE(UNIT=10,FMT='(2I5,2F8.3,2(/,5F10.5))')
C     +    MIP1,ENT1,BX,BY,
C     +    XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
C     1    XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
C
         IF ( ENT1  .EQ. LINE ) THEN
C           Condition no. 2
            IF ( XY(1,1) .GT. XY(3,1) ) THEN
               CALL RSWAP(XY(1,1),XY(3,1))
               CALL RSWAP(XY(2,1),XY(4,1))
            END IF
            X1=XY(1,1)
            Y1=XY(2,1)
            X2=XY(3,1)
            Y2=XY(4,1)
            IF ( Y2 .LT. Y1 ) THEN
               CALL RSWAP(Y1,Y2)
            END IF
         ELSE IF ( ENT1  .EQ. ARC ) THEN
            CALL  ARCBOX(XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     +                      X1,Y1,X2,Y2)
         END IF
C
C     Condition No. 1
C
C      WRITE(UNIT=10,FMT=*) 'ENT LIMS',X1,Y1,X2,Y2
         XMIN=MIN(XMIN,X1)
         YMIN=MIN(YMIN,Y1)
         XMAX=MAX(XMAX,X2)
         YMAX=MAX(YMAX,Y2)
C        Store just in case the line has been
C        turned round
         WRITE(UNIT=HUNIT,REC=I) MIP1,ENT1,BX,BY,
     +    XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1    XY(1,2),XY(2,2),XY(3,2),XY(4,2),XY(5,2)
C
C         WRITE(UNIT=10,FMT='(2I4,2F5.1,/5F10.3))') MIP1,ENT1,BX,BY,
C     +    XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1)
C
 10   CONTINUE
C
C      WRITE(UNIT=10,FMT=*)' BOUNDARY STORED',XMIN,YMIN,XMAX,YMAX
C
      CALL UNFLAG(.TRUE.)
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE MNIHAT
C     =================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2    the INSERT HATCH option list.
C2
C2    Tokens used here are B and CHAR(150)
C2
      INTEGER*4 I
      INTRINSIC CHAR
      EXTERNAL GTPMEN,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Enter the hatching options.
      I=4
C
C2    W is the token for INSIDE AREA.
C      CALL GTDMEN(120,3)
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(121,3)
C2    H is the token for PITCH
C      CALL GTDMEN(123,3)
C2    A is the token for ANGLE
C      CALL GTDMEN(124,3)
C2    X is the token for CROSS HATCH.
      CALL GTDMEN(125,3)
C2    CHAR(150) is the token for ACCEPT.
      CALL GTDMEN(126,3)
      CALL GTDMEN(469,3)
C2
C
      END
C
C     ---------------------------------------------------
C
 
      SUBROUTINE PULENT(FN)
C     ===================
C1    No arguments needed
C
C2    PULENT retrieves a line or arc from the data base indicating
C2    which have been found storing them in a temporary file
C2    the user hits RETURN.
C2    If at any time the user hit 'Q' or 'q' then the operation is
C2    abandonded
C2    If Backspace is hit this removes the last line or arc
C2    only and this is indicated by the box at the centre
C2    being removed
C2    FN defines the mode of use of PULENT which may be
C2       FN = 1   Find a boundary for hatching purposes
C2       FN = 2   Define a profile for NC purposes
C2       FN = 3   Define boundary for area measurement
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/hdata.inc'
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/swind.inc'
      include   'include/save.inc'
      include   'include/curwin.inc'
      include   'include/profile.inc'
      include   'include/viewport.inc'
C
      REAL      X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ANG,XP,YP,SX,SY,
     +          BX(2),BY(2),XYB(5,2),PI,REAL,SSRAD,SINC,
     1          CXP,CYP,DISTXY,CANG,XLAST,YLAST,STANG,
     2          SRAD,SXC,SYC,ETANG,SHITX,SHITY,AUX(2,2),
     3          XP1,YP1,XP2,YP2,TANG,LIM,DIST,AX1,AX2,XY1,AY2,NX,NY
      REAL      NEWANG,EANG,SANG,XY41,XY51
      DOUBLE PRECISION  DX1,DY1,DX2,DY2,DX3,DY3,DC1,DC2,DVDD14,
     +                  NL1,NL2,NL3,DHX1,DHY1,DCANG
C
      INTEGER*2  ENTP,I,J,BDP,POINT,SBDP(50),NBDP,ACHK,FN,I2,I1,SFP
      INTEGER*2  TENT,D1,ENTYPE,TEMPNT,NOENT
C
      INTEGER*4 C,BMEN,BCELL,II,DNUM,TMEN,TCELL,LOGIC,ST,STARAY(5),
     +            ST1,CNCL
C
      LOGICAL CHKLN,START,NEWBD,ARCTPT,CLKWS,SAME,WOK,
     +        BCLOCK,ABND,OKP1,OKP2,DISTOK,OK,AREA,WINB,
     +        OCLOCK,CPLBD,FIRST,FPOINT,DSAME
C
      INTRINSIC CHAR,REAL
C
      EXTERNAL NOSRCH,ADSRCH,TCURS,UNFLAG,HATCHM,SAME,
     +         WINDOW,RSCRF,DER500,GTMCLO,BELL,
     1         DBOX,DSE801,INTERS,CHKLN,WO2SC,BCROSS,DISTXY,
     2         CANG,CLKWS,ARCTPT,LOGIC,NEWANG,DSAME,DCANG
C
      CALL OURSCR(CNCL,8,OK)
      IF(.NOT.OK) GOTO 80
      IF ( FN.EQ.1 ) THEN
C        339    "Indicate boundary of area(s) to be hatched"
         DNUM=339
      ELSE IF ( FN.EQ.2 ) THEN
C        70    "Indicate profile"
         DNUM=70
      ELSE IF ( FN.EQ.3 ) THEN
C        148    "Indicate boundary to be measured"
         DNUM=148
      END IF
C
      WINB = .FALSE.
      ENT1 = 0
      BDP =1
      SFP =1
      NBDP=1
      ENTP=1
      TEMPNT = 2
      SBDP(NBDP)=ENTP
      NEWBD=.FALSE.
      AREA=.FALSE.
      FIRST = .FALSE.
      CALL NOSRCH()
      CALL ADSRCH(LINE)
      CALL ADSRCH(ARC)
      IF(FN.EQ.1) THEN
          CALL ADSRCH(ELIPSE)
      END IF
C
 10   CONTINUE
C
      IF (NEWBD) THEN
        NEWBD = .FALSE.
        ENTP = TEMPNT - 1
      END IF
C     tell the user what to do
      CALL DCPRNT(DNUM)
C
      IF ( SFP .NE. ENTP ) THEN
        CALL ZSFLAG(.TRUE.,OK)
        SFP =ENTP
      ENDIF

      XLAST = X
      YLAST = Y
C     go get cursor hit
      CALL TCURS(C,X,Y)
C
C     keep reseting draiwng code if using daxports
      IF(MVPACT.AND..NOT.MAWS) THEN
          DDCODE = 2
      ENDIF
C
 9    CONTINUE
C     save menu,and cell numbers
      TMEN = MEN
      TCELL = CELLN
C     get hit in screen coords
      CALL WO2SC(X,Y,SX,SY)
C     test for hit in work area
      CALL WRKHIT(SX,SY,WOK)
C     Test for quit condition
      IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         CALL UNFLAG(.TRUE.)
         GOTO 80
      END IF
C     Check for display control.
 30   CONTINUE
      IF ( MEN .EQ. 3 ) THEN
C        HATCH command modifier 
         CALL HATCHM(OKP1)
         IF ( OKP1 ) GOTO 10
         IF(CCMD.EQ.'N') THEN
C           used for a new profile ie restart let the caller handle it
            GOTO 80
         ENDIF
      END IF
C
      IF ( CCMD .EQ. 'Z' ) THEN
C         autoprofile
          CALL SPCG00(ENTP)
C         if we hit something else then profile not selected
          IF(MEN.EQ.0) THEN
C             if auto profile has gened an open profile then carry on as normal
              IF(OPENP) THEN
C                 low the cell and set currrent number
                  SFP = ENTP
                  CALL GTMCLO(TMEN,TCELL)
                  NO = ENTP -1
                  GOTO 10
              ENDIF
              AREA=.TRUE.
              ACHK=ENTP
C             store the postion to read from the display file
              SFP = ENTP
              BDP = ENTP
              NBDP = NBDP + 1
              SBDP(NBDP) = -ENTP
              CALL GTMCLO(TMEN,TCELL)
              NO = ENTP -1
              GOTO 10
          ELSE
C             something else has been hit do a test
              IF(CCMD.EQ.'Z') GOTO 10
              GOTO 9
          ENDIF
C
      ENDIF
C
      IF ( CCMD .EQ. 'W' ) THEN
         IF(ABS(SBDP(NBDP)) .NE. ENTP ) THEN
            DNUM = 754
            CALL DEPRNT(DNUM)
            CALL GTMCLO(MEN,CELLN)
            GOTO 10
         ENDIF
         CALL DCPRNT(88)
         CALL WINDOW(.TRUE.)
         DO 111 II= SFP ,NDATA
            CALL RSCRF(II,MIP1,BX(1),BY(1),I,J)
            CALL DER500(MIP1,OK)
            IF ( .NOT. OK ) THEN
               CALL UNFLAG(.TRUE.)
               GOTO 80
            END IF
C      WRITE(10,*) '[HATCH] ENTP WRITE= ',ENTP,MIP1
            ENTYPE = IMBUFF(2)
            IF (ENTYPE .EQ.ELIPSE ) THEN
                 ENTYPE = ARC
            END IF
            WRITE(UNIT=HUNIT,REC=ENTP) MIP1,ENTYPE,BX(1),BY(1),
     +        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1        RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
            ENTP=ENTP+1
 111     CONTINUE
         AREA=.TRUE.
         ACHK=ENTP
C        store the postion to read from the display file
         SFP = ENTP
         BDP = ENTP
         NBDP = NBDP + 1
         SBDP(NBDP) = -ENTP
         CALL GTMCLO(MEN,CELLN)
C      WRITE(10,*)'[PULENT]AREA;ENTP,BDP,NBDP',ENTP,' ',BDP,' ',NBDP
         GOTO 10
      ELSE
          WINB = .FALSE.
      END IF
C      WRITE(10,*) 'PULENT',CCMD,AREA,ACHK,ENTP
C
      IF ( CCMD.EQ.'P' ) THEN
C        profile option been chosen
C        get rid of what we have here before
C        going back.
         NO = 0
         NO=ENTP-1
         GOTO 80
      ELSE IF ( CCMD.EQ.'p' ) THEN
C        Points option been chosen
C        get rid of what we have here before
C        going to start again.
         NO = 0
         NO=ENTP-1
         GOTO 80
      END IF
C
c     ********************************  NEW BOUNDRY OR ACCEPT  ******************************
      IF ( CCMD .EQ. 'B' .OR. CCMD .EQ. CHAR(150) ) THEN
C        NEWBD set .true. indicates a new boundary is being
C        defined
         IF ( FN.EQ.2.AND.ENTP.GT.0 ) THEN
C           next boundary in gnc
            IF (ENT1.EQ. ARC) THEN
C              ==== If last entity was an arc then let us find the ====
C              ==== best start and end point                       ====
C              call routine to find the two end points of the arc
c              NOTE:- from the original boundry definition
               CALL NEWPT1(XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),X1,X2)
               CALL NEWPT1(XYB(1,1),XYB(2,1),XYB(3,1),XYB(5,1),X3,X4)
c                    
C              call routine to find the the old old intersection point
               CALL NEWPT1(XY(1,1),XY(2,1),XYB(3,1),XY(4,1),XP2,YP2)
c
               ST = 2
C              now calulate the best intersection point .i.e.start
C              or end . Given last true intersection point and hit point 
               CALL SRTAR4(XY(1,1),XY(2,1),XP2,YP2,BX(1),BY(1),
     +                     X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,STARAY)
c              now let us call the routine to calulate the start and
c              end angle from the start point end point and one hit point
               CALL SRT3PT(XP2,YP2,XP,YP,BX(1),BY(1),
     +                     XY(1,1),XY(2,1),XY(4,1),XY(5,1))
               WRITE(UNIT=HUNIT,REC=ENTP-1) MIP1,ENT1,BX(1),BY(1),
     +         XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1         XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)

c   
            ELSE IF (ENT1.EQ.LINE) THEN
C               Make sure the last line is correct way
C               round for an open boundry i.e. that the end point is correct
                IF (SAME(XYB(3,1),XY(1,1)).AND.
     +              SAME(XYB(4,1),XY(2,1))) THEN
                   XP2 = XYB(1,1)
                   YP2 = XYB(2,1)
                ELSE
                   XP2 = XYB(3,1)
                   YP2 = XYB(4,1)
                END IF
                    
                CALL SRTEND(XY(1,1),XY(2,1),XP2,YP2,BX(1),BY(1),
     +            XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XP,YP)
                XY(3,1) = XP
                XY(4,1) = YP
C
                WRITE(UNIT=HUNIT,REC=ENTP-1) MIP1,ENT1,BX(1),BY(1),
     +          XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1          XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)

            END IF 
            IF(BDP .EQ. ENTP.AND.ENTP.GT.1) BDP = ABS(SBDP(NBDP-1))
            IF(ENTP.GT.1) THEN
               READ(UNIT=HUNIT,REC=BDP) MIP1,ENT1,BX(1),BY(1),
     +          XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1         XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
               IF (ENT1.EQ. ARC) THEN
C                 **** Same as above but for First entity is arc ***
C                 ==== If last entity was an arc then let us find the ====
C                 ==== best start and end point                       ====
C                 call routine to find the nearest intersection point
                  CALL NEWPT1(XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),X1,X2)
                  CALL NEWPT1(XYB(1,1),XYB(2,1),XYB(3,1),XYB(5,1),X3,X4)
C                 call routine to find the the old old intersection point
                  CALL NEWPT1(XY(1,1),XY(2,1),XYB(3,1),XY(4,1),XP2,YP2)
                  ST = 2
C                 now calulate the best intersection point .i.e.start
C                 or end . Given last true intersection point and hit point 
                  CALL SRTAR4(XY(1,1),XY(2,1),XP2,YP2,BX(1),BY(1),
     +                       X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,STARAY)
c                 now let us call the routine to calulate the start and
c                 end angle from the start point end point and one hit point
                  CALL SRT3PT(XP2,YP2,XP,YP,BX(1),BY(1),
     +                        XY(1,1),XY(2,1),XY(4,1),XY(5,1))
                  WRITE(UNIT=HUNIT,REC=BDP) MIP1,ENT1,BX(1),BY(1),
     +            XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1            XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
c   
c   
               ELSE IF (ENT1.EQ.LINE) THEN
C                  Make sure the last line is correct way
C                  round for an open boundry i.e. that the end point is correct
                   IF (SAME(XYB(1,1),XY(3,1)).AND.
     +                 SAME(XYB(2,1),XY(4,1))) THEN
                      XP2 = XYB(3,1)
                      YP2 = XYB(4,1)
                   ELSE
                      XP2 = XYB(1,1)
                      YP2 = XYB(2,1)
                   END IF
                    
                   CALL SRTEND(XY(3,1),XY(4,1),XP2,YP2,BX(1),BY(1),
     +               XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XP,YP)
                   XY(1,1) = XP
                   XY(2,1) = YP
C
                   WRITE(UNIT=HUNIT,REC=BDP) MIP1,ENT1,BX(1),BY(1),
     +             XY(1,1),XY(2,1),XY(3,1),XY(4,1),XY(5,1),
     1             XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)

               END IF
            END IF 
            NO=ENTP-1
            CALL GTMCLO(MEN,CELLN)
            GOTO 80
         END IF
C
         NEWBD=.TRUE.
         BMEN=MEN
         BCELL=CELLN
C
C        if we have already defined a complete boundary then go home
         CPLBD = AREA .AND. ACHK.EQ.ENTP 
C
C
c     ********************************  BACK SPACE  ******************************
      ELSE IF ( CCMD .EQ. CHAR(149)  ) THEN
C        Backspace has been hit
C            write(10,*) '[hatch] cancel sbdp entp ',sbdp(nbdp),entp
         IF(SBDP(NBDP) . LT.0 .AND.ABS(SBDP(NBDP)) .EQ. ENTP) THEN
            WINB=.TRUE.
         ELSE
            WINB=.FALSE.
         ENDIF
         IF ( ENTP .EQ. 1 ) THEN
C           ENTP is set at 1 there is nothing to unflag
            CALL DEPRNT(33)
         ELSE IF (WINB) THEN
C
C           is this a window boundary
C
C           decremet boundary pointer 
            NBDP = NBDP - 1
C           reset scratch file
            SFP = ABS(SBDP(NBDP))
C
            IF(SFP.GT.NDATA) THEN
C               This one has nothing in the data file
                DO 201 II = ABS(SBDP( NBDP)),ABS(SBDP(NBDP+1))-1
                READ(UNIT=HUNIT,REC=II) MIP1,ENT1,X,Y,
     +           XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1           XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
                    CALL DER500(MIP1,OK)
C                   remove the attention flag.
                    CALL LABNFG(MIP1)
                    IF(MVPACT.AND..NOT.MAWS) THEN
                         DDCODE = 2
                    ENDIF
                    CALL ALLDRW(IMBUFF(2),MIP1)
201             CONTINUE
                ENTP = SFP
                BDP = ENTP
                WINB = .FALSE.
C                
            ELSE
                DO 200 II = SFP , NDATA
                    CALL RSCRF(II,MIP1,BX(1),BY(1),I,J)
                    CALL DER500(MIP1,OK)
C                   remove the attention flag.
                    CALL LABNFG(MIP1)
                    IF(MVPACT.AND..NOT.MAWS) THEN
                        DDCODE = 2
                    ENDIF
                    CALL ALLDRW(IMBUFF(2),MIP1)
200             CONTINUE
                NDATA = SFP -1
                ENTP = SFP
                BDP = ENTP
                WINB = .FALSE.
            ENDIF
         ELSE
            ENTP=ENTP-1
C           Recover last store boundary from scratch file
           READ(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,BX(1),BY(1),
     +      XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1     XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
C      WRITE(UNIT=10,FMT=*)'[PULENT] CANCEL;ENTP,BDP,NBDP',ENTP,
C     +                     ' ',BDP,' ',NBDP

C            CALL ALLDRW(ENT1,MIP1)
C
            CALL ZSFLAG(.TRUE.,OK)
            SFP = SFP - 1
            IF ( BDP .GT. ENTP ) THEN
               NBDP=NBDP-1
               BDP=ABS(SBDP(NBDP))
            END IF
         END IF
         NO=ENTP-1
          IF (NO.GT.1) THEN
C             This code resets the xy41 value to its original state
C             so that arcs will store their previous positions
              READ(UNIT=HUNIT,REC=NO) MIP1,ENT1,BX(1),BY(1),
     +         XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1        XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
C           now restore the previous start end angle
            IF(WINB) THEN
C               note if this errors then just use the current xy (5,1),(4,1) values
                READ(UNIT=CNCL,REC=NO,ERR = 220) XY41,XY51
                XY(4,1) = XY41
                XY(5,1) = XY51
            END IF
 220        CONTINUE
            WRITE(UNIT=HUNIT,REC=NO) MIP1,ENT1,BX(1),BY(1),
     +       XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1       XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
         END IF
         CALL GTMCLO(MEN,CELLN)
         GOTO 10
      END IF
c
C     ****************************************************
C     *********  ALL MENU OPTIONS DEALT WITH  ************
C     ****************************************************
      NOENT = ENTP - BDP
c     *******************************************
C     ******* O R D I N A R Y  B O U N D R Y*****
c     *******************************************
      IF ( .NOT. NEWBD ) THEN
          IF(.NOT.WOK) THEN
              CALL DEPRNT(8)
              CALL GTMCLO(TMEN,TCELL)
              GOTO 10
         ENDIF
C        Search for line,or Arc
         AREA = .FALSE.
         CALL DSE801(X,Y,OKP1)
         BX(1) = X
         BY(1) = Y
C
         IF ( .NOT. OKP1 ) THEN
C           Nothing found therefore tell the user and
C           go for another try
            CALL DEPRNT(142)
            GOTO 10
         END IF
         IF((ENTP-BDP+1).EQ.1) THEN
C            valid point store the first one
             SHITX = X
             SHITY = Y
         ENDIF
C        do not pass common block element,use II instead
C         II=NDATA
C         CALL RSCRF(II,MIP1,BX(1),BY(1),I,J)
C         WRITE(UNIT=10,FMT='(A,I4)') 'WRITE Entp is ',ENTP
C         WRITE(UNIT=10,FMT='(2I5,2F8.3,2(/,5F10.5))')
C     +           MIP1,IMBUFF(2),BX(1),BY(1),
C     +     RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
C     1     RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
C
         MIP1=IDBUFF(2)
         D1=0
C        put this entity into the scratch file
         CALL SSFLAG(MIP1,BX(1),BY(1),DFP,D1,.FALSE.)
C        store the current flag
C      WRITE(10,*)'[PULENT]AREA;ENTP,BDP,NBDP',ENTP,' ',BDP,' ',NBDP
         SFP = SFP + 1
         ENTYPE = IMBUFF(2)
         IF (ENTYPE .EQ.ELIPSE ) THEN
              ENTYPE = ARC
         END IF
         WRITE(UNIT=HUNIT,REC=ENTP) MIP1,ENTYPE,BX(1),BY(1),
     +     RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1     RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
         POINT=ENTP-1
      ELSE
c     ****************************************
C     ******* N E W  B O U N D R Y ***********
c     ****************************************
C         NOENT = NOENT -1
C        TEST for valid boundaries before contining
C        Pointer set back one to finish old boundary
         POINT=ENTP-1
         IF ( ENTP  .LE. BDP .AND. .NOT. CPLBD) THEN
            CALL DEPRNT(152)
            CALL DCPRNT(90)
            CALL GTMCLO(MEN,CELLN)
c            ENTP=ENTP+1
            NEWBD=.FALSE.
            GOTO 10
         END IF
         IF ( NOENT.EQ.2 ) THEN
            IF ( ENT1.EQ.LINE .AND. ENT2.EQ.LINE ) THEN
C              only two entities and both are lines
C              156    "Only two lines selected"
               CALL DEPRNT(156)
               NEWBD=.FALSE.
c               ENTP=ENTP+1
               CALL GTMCLO(BMEN,BCELL)
               GOTO 10
            END IF
         END IF
         IF ( NOENT .EQ. 1 ) THEN
            READ(UNIT=HUNIT,REC=POINT) MIP1,ENT1,BX(1),BY(1),
     +      XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1     XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
            IF (  ENT1 .EQ. ARC )  THEN
C           Is the single boundary a full circle
               IF ( XY(4,1) .LT. 0.01 .AND.
     +              XY(5,1) .GE. PI(2.0)-0.01 ) THEN
C                 Yes it is so we will allow that
                  BDP=ENTP
C                 Next boundary pointer must be updated
                  NBDP=NBDP+1
C                 Store the boundary pointer in case
C                 user backsteps through the boundary
                  SBDP(NBDP)=BDP
C                 Update the entity pointer now
c                  ENTP=ENTP+1
                  CALL GTMCLO(BMEN,BCELL)
                  IF ( CCMD .EQ. CHAR(150) ) THEN
                     NO=ENTP-1
                     GOTO 80
                  ELSE
C                    go round again 
                     NEWBD=.FALSE.
                  END IF
                  GOTO 10
               ELSE
                  CALL DEPRNT(154)
                  CALL DCPRNT(90)
                  CALL GTMCLO(BMEN,BCELL)
                  GOTO 10
               END IF
            ELSE
               CALL DEPRNT(154)
               CALL DCPRNT(90)
               CALL GTMCLO(BMEN,BCELL)
               GOTO 10
            END IF
         END IF
      END IF
C     if a complete boundary has been specified then go home
      IF ( CCMD .EQ. CHAR(150).AND.CPLBD ) THEN
          NO=ENTP - 1
          CALL GTMCLO(MEN,CELLN)
          GOTO 80
      ELSEIF(CCMD.EQ.'B'.AND.CPLBD) THEN
          NEWBD = .FALSE.
          CALL DEPRNT(152)
          GOTO 10 
      END IF
C     Indicates if that two points have been stored
C     on the boundary
      IF (NOENT .EQ. 0) THEN
C        First of boundary point nothing to do
         ABND=IMBUFF(2).EQ.ARC.OR.IMBUFF(2).EQ.ELIPSE
C        If first boundary is an arc then we have to store
C        hit point for future use
C      write(10,*) '[hatch] start point x= ',x
C      write(10,*) '[hatch] start point y= ',y
         FIRST = .TRUE.
         CXP=X
         CYP=Y
         ENTP=ENTP+1
         GOTO 10
      END IF
      IF (NEWBD) THEN
         TEMPNT = ENTP
         POINT = ENTP-1
         ENTP = BDP
      END IF
C     Recover the last stored boundary from scratch file.
      READ(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,BX(1),BY(1),
     +    XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1   XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
      IF (ENT1.EQ.ARC.OR.ENT1.EQ.ELIPSE) THEN
         CALL ARCAUX(MIP1,AUX(1,1),AUX(2,1),OK)
      END IF
C
C         WRITE(UNIT=10,FMT='(A,I4)') 'READ Entp is ',ENTP
C         WRITE(UNIT=10,FMT='(2I5,2F8.3,2(/,5F10.5))')
C     +    MIP1,ENT1,BX(1),BY(1),
C     +    XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
C     1   XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
C
C     Recover the start or previously defined boundary.
      READ(UNIT=HUNIT,REC=POINT) MIP2,ENT2,BX(2),BY(2),
     +    XY(1,2), XY(2,2), XY(3,2), XY(4,2), XY(5,2),
     1   XYB(1,2),XYB(2,2),XYB(3,2),XYB(4,2),XYB(5,2)
      IF (ENT2.EQ.ARC.OR.ENT2.EQ.ELIPSE) THEN
         CALL ARCAUX(MIP2,AUX(1,2),AUX(2,2),OK)
      END IF
C
C         WRITE(UNIT=10,FMT='(A,I4)') 'READ Point is ',POINT
C         WRITE(UNIT=10,FMT='(2I5,2F8.3,2(/,5F10.5))')
C     +       MIP2,ENT2,BX(2),BY(2),
C     +    XY(1,2), XY(2,2), XY(3,2), XY(4,2), XY(5,2),
C     1   XYB(1,2),XYB(2,2),XYB(3,2),XYB(4,2),XYB(5,2)
C
C     check for same entity hit twice.
      IF (MIP1.EQ.MIP2) THEN
C        cannot allow this.
C        98    "Duplicate entity illegal"
         CALL DEPRNT(98)
C        go try again.
         GOTO 10
      END IF
C
C
C     get intersection between entities
      CALL INTERS(ENT1,XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),
     +            AUX(1,1),AUX(2,1),
     +            ENT2,XYB(1,2),XYB(2,2),XYB(3,2),XYB(4,2),
     +            AUX(1,2),AUX(2,2),
     1            X1,X2,X3,X4,X5,X6,X7,X8,ST,OKP1)
C
C ---------------------------------------------------------------------------
C      WRITE(10,*) '[HATCH] X1 X2 = ',X1,X2
C      WRITE(10,*) '[HATCH] X3 X4 = ',X3,X4
      IF ( .NOT. OKP1 ) THEN
C        Entities did not intersect at all
C
         IF(NEWBD) THEN
              NEWBD = .FALSE.
              CALL GTMCLO(BMEN,BCELL)
         ENDIF

         GOTO (11,22,33) (ENT1+ENT2-4)/2
C
C        Must be parallel lines
 11      CONTINUE
C        157    "Entities do not intersect"
         CALL DEPRNT(157)
         GOTO 44
C        Line did not pass through the circle at all
 22      CONTINUE
C        157    "Entities do not intersect"
         CALL DEPRNT(157)
         GOTO 44
C        Arcs did not intersect
 33      CONTINUE
C        157    "Entities do not intersect"
         CALL DEPRNT(157)
C
 44      CONTINUE
         CALL DCPRNT(90)
C        Remove flag of most recent boundary added to the file
          IF(MVPACT.AND..NOT.MAWS) THEN
             DDCODE = 2
         ENDIF
         CALL ALLDRW(ENT1,MIP1)
C
         GOTO 10
      END IF
c
C *******************************************************************************
C ********   THIS CODE DEALS WITH THE START CONDITIONS AND STORES    ************
C ********        INFORMATION SUITABLE FOR THE END CONDITION         ************
C *******************************************************************************
      IF(MVPACT.AND..NOT.MAWS) THEN
          DDCODE = 2
      ENDIF
C
      IF(FIRST) THEN
         IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. LINE )  THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D   L I N E              *
C***********************************************************************
C            Check intersection is not virtual
C            NOTE same as normal
C         
C              Alter start line of boundary
               XY(3,2)=X1
               XY(4,2)=X2
C              and finish line of boundary
               XY(1,1)=X1
               XY(2,1)=X2
C
            CALL DRWCRS(X1,X2,1)
C
C
         ELSE IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D    A R C               *
C***********************************************************************
C
C                   Line and Arc
c           This routine works by calculating the nearest intersection point 
C           to the current hit point.  Then stores the new intersection point
C           for processing later.  
c           
C                                                               
C
C           call routine to find the nearest intersection point
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)
c   
c
c           store the intersection point of the first 
C           arc segment to be processed by the end condition
c
            XY(4,2) = REAL(DCANG(DBLE(XY(1,2)),DBLE(XY(2,2)),
     +                           DBLE(XP),DBLE(YP)))
C
C           modify the line end point to the intersection point
            XY(1,1) = XP
            XY(2,1) = YP
            CALL DRWCRS(XP,YP,1)
C        
         ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. LINE ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A   L I N E              *
C***********************************************************************
C
C                Arc and Line
c
c
c           This routine works by calculating the nearest intersection point 
C           to the current hit point.  Then stores the new intersection point
C           for processing later.  
           
C           call routine to find the nearest intersection point
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)

c
c
C           store the new intesection point untill we decide what
C           segment that is required
            XY(4,1) = REAL(DCANG(DBLE(XY(1,1)),DBLE(XY(2,1)),
     +                     DBLE(XP),DBLE(YP)))
C
C           store the new intesection point as the new line end point
            XY(3,2) = XP
            XY(4,2) = YP         
C
            CALL DRWCRS(XP,YP,1)
C
c
c
C
          ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A R C                    *
C***********************************************************************
C
C              Arc  and  Arc
c
c           This routine works by calculating the nearest intersection point 
C           to the current hit point.  Then stores the new intersection point
C           for processing later.  
c         
c
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)
C
C           store the new intesection point until we decide what
C           segment that is required
            XY(4,1) = REAL(DCANG(DBLE(XY(1,1)),DBLE(XY(2,1)),
     +                           DBLE(XP),DBLE(YP)))
C
c           store the intersection point of the first 
C           arc segment to be processed by the end condition
            XY(4,2) = REAL(DCANG(DBLE(XY(1,2)),DBLE(XY(2,2)),
     +                           DBLE(XP),DBLE(YP)))
C
            CALL DRWCRS(XP,YP,1)
     
         END IF
      END IF   





C *******************************************************************************
C *******************************************************************************
C ********         N E W   B O U N D R Y   C O D E   O N L Y ********************
C *******************************************************************************
C *******************************************************************************
      IF(NEWBD) THEN
         IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. LINE )  THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D   L I N E              *
C***********************************************************************
C            Check intersection is not virtual
C         
C              Alter start line of boundary
               XY(3,2)=X1
               XY(4,2)=X2
C              and finish line of boundary
               XY(1,1)=X1
               XY(2,1)=X2
C
            CALL DRWCRS(X1,X2,1)
C
C
         ELSE IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D    A R C               *
C***********************************************************************
C
C                   Line and Arc
C
C
            CALL NEWPT1(XY(1,2),XY(2,2),XY(3,2),XY(4,2),XP2,YP2)

C           call routine to find the nearest intersection point
            CALL SRTAR4(XY(1,2),XY(2,2),XP2,YP2,BX(2),BY(2),X1,X2,
     +                  X3,X4,X5,X6,X7,X8,ST,XP,YP,STARAY)
           
c   
c
c           Now we need to calculate the which portion of the
C           arc.  This is done from the previous hit point on the
C           arc.
c
C                 
            CALL SRT3PT(XP2,YP2,XP,YP,BX(2),BY(2),
     +                  XY(1,2),XY(2,2),XY(4,2),XY(5,2))
C        
            XY(1,1) = XP
            XY(2,1) = YP
            CALL DRWCRS(XP,YP,1)
C        
         ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. LINE ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A   L I N E              *
C***********************************************************************
C

C                Arc and Line
C
c           This routine works by calculating the closest intersection
C           point to the vector passing through the previous intersection
C           point and the previous hit point
C           
C           ------------------------------------------------
C           This section is equivalent to the call to NEREST            
            CALL SRTEND(XY(1,2),XY(2,2),XY(3,2),XY(4,2),BX(2),BY(2),
     +                  X1,X2,X3,X4,XP,YP)
C          ------------------------------------------------------
c
C           store the new intesection point as the new line end point
            XY(3,2) = XP
            XY(4,2) = YP         
C
            CALL DRWCRS(XP,YP,1)
c
c
            CALL NEWPT1(XY(1,1),XY(2,1),XY(3,1),XY(4,1),XP2,YP2)
            CALL SRT3PT(XP,YP,XP2,YP2,BX(1),BY(1),
     +                  XY(1,1),XY(2,1),XY(4,1),XY(5,1))
            
c   
c
C
          ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A R C                    *
C***********************************************************************
C
C              Arc  and  Arc
c
C           This routine is split into 2 : -
c
C              1 it calculates the new intersection point of the new arc
C              2 calculates the old arc segment from the previously stored
C                intersection point
c         
c
            CALL NEWPT1(XY(1,2),XY(2,2),XY(3,2),XY(4,2),XP2,YP2)
C
C           call routine to find the nearest intersection point
            CALL SRTAR4(XY(1,2),XY(2,2),XP2,YP2,BX(2),BY(2),X1,X2,
     +                  X3,X4,X5,X6,X7,X8,ST,XP,YP,STARAY)
c
            CALL DRWCRS(XP,YP,1)
C
C           now let us calculate the new start angle and end angle
C           with our sorted points
            CALL SRT3PT(XP2,YP2,XP,YP,BX(2),BY(2),
     +                  XY(1,2),XY(2,2),XY(4,2),XY(5,2))
c   
c           Now we need to calculate the which portion of the
C           arc.  This is done from the previous hit point on the
C           arc.
c
            CALL NEWPT1(XY(1,1),XY(2,1),XY(3,1),XY(4,1),XP2,YP2)
           
            CALL SRT3PT(XP,YP,XP2,YP2,BX(1),BY(1),
     +                  XY(1,1),XY(2,1),XY(4,1),XY(5,1))
C 
c   
         END IF

C *******************************************************************************
C *******************************************************************************
C ********     O R D I N A R Y   B O U N D R Y       O N L Y ********************
C *******************************************************************************
C *******************************************************************************

      ELSE IF (.NOT.FIRST) THEN
         IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. LINE )  THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D   L I N E              *
C***********************************************************************
C            Check intersection is not virtual
C         
C
C              Alter the end points of previous line
               XY(3,2)=X1
               XY(4,2)=X2
C              and also the start point of the current line
               XY(1,1)=X1
               XY(2,1)=X2
C
C
C      WRITE(UNIT=10,FMT='(A,2F10.4)') 'LINE INTERSECTION POINT',X1,X2
C
            CALL DRWCRS(X1,X2,1)
C
C
         ELSE IF ( ENT1 .EQ. LINE .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C               N E W   L I N E   A N D   O L D    A R C               *
C***********************************************************************
C
C                   Line and Arc
c          This routine works by calculating the nearest intersection point 
C           to the current hit point.  Then calcuales from previous the hit point
C           if the segment.   It then
C           modifies the start and end angle of the arc to ensure it
C           is on the corrent segments.
C                                                               
C
C           call routine to find the nearest intersection point
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)
c   
c
c           Now we need to calculate the which portion of the
C           arc.  This is done from the previous hit point on the
C           arc.
c
c           previous code should have set radius = -ve if Clockwise 

            CALL NEWPT1(XY(1,2),XY(2,2),XY(3,2),XY(4,2),XP2,YP2)
         
            CALL SRT3PT(XP2,YP2,XP,YP,BX(2),BY(2),
     +                   XY(1,2),XY(2,2),XY(4,2),XY(5,2))
C 
c   
C        
            XY(1,1) = XP
            XY(2,1) = YP
            CALL DRWCRS(XP,YP,1)
C        
         ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. LINE ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A   L I N E              *
C***********************************************************************
C
C                Arc and Line
c
c           This routine works by calculating the nearest intersection point 
C           to the current hit point.  Then stores the new intersection point
C           for processing later.  
           
C           call routine to find the nearest intersection point
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)
c
c
c
C           store the new intesection point until we decide what
C           segment that is required
            XY(4,1) = REAL(DCANG(DBLE(XY(1,1)),DBLE(XY(2,1)),
     +                            DBLE(XP),DBLE(YP)))
C
C           store the new intesection point as the new line end point
            XY(3,2) = XP
            XY(4,2) = YP         
C
            CALL DRWCRS(XP,YP,1)
C

c
c
c
C
          ELSE IF ( ENT1 .EQ. ARC .AND. ENT2 .EQ. ARC ) THEN
C***********************************************************************
C           N E W     A R C   A N D   O L D   A R C                    *
C***********************************************************************
C
C              Arc  and  Arc
c
C           This routine is split into 2 : -
c
C              1 it calculates the new intersection point of the new arc
C              2 calculates the old arc segment from the previously stored
C                intersection point
c         
c
            CALL NEREST(X,Y,X1,X2,X3,X4,X5,X6,X7,X8,ST,XP,YP,ST1)
C
C           store the new intesection point until we decide what
C           segment that is required
            XY(4,1) = REAL(DCANG(DBLE(XY(1,1)),DBLE(XY(2,1)),
     +                            DBLE(XP),DBLE(YP)))
            CALL NEWPT1(XY(1,1),XY(2,1),XY(3,1),XY(4,1),NX,NY)
C
c           Now we need to calculate the which portion of the
C           arc.  This is done from the previous hit point on the
C           arc.
c
c           previous code should have set radius = -ve if Clockwise 
            CALL NEWPT1(XY(1,2),XY(2,2),XY(3,2),XY(4,2),XP2,YP2)
           
            CALL SRT3PT(XP2,YP2,XP,YP,BX(2),BY(2),
     +                  XY(1,2),XY(2,2),XY(4,2),XY(5,2))
C 
            CALL DRWCRS(XP,YP,1)
          END IF
     
      END IF






c
 1020 CONTINUE
C
C          modify the first and last ents

      IF(MVPACT.AND..NOT.MAWS) THEN
          DDCODE = 2
      ENDIF
      IF ( (ENTP-BDP+1).GT.2 ) THEN
         IF ( ENT2.EQ.LINE ) THEN
            IF ( SAME(XY(1,2),XY(3,2)).AND.
     +           SAME(XY(2,2),XY(4,2))        ) THEN
               DNUM = 755
               CALL DEPRNT(DNUM)
               CALL ALLDRW(ENT1,MIP1)
               GOTO 10
            END IF
         ELSE IF ( ENT2.EQ.ARC ) THEN
            IF ( SAME(XY(5,2),XY(4,2)) ) THEN
               DNUM = 755
               CALL DEPRNT(DNUM)
               CALL ALLDRW(ENT1,MIP1)
               GOTO 10
            END IF
         END IF
      END IF
C
C     Store the last stored boundary from scratch file
      XY41 = XY(4,1)
      XY51 = XY(5,1)
      WRITE(UNIT=CNCL,REC=ENTP) XY41,XY51
      WRITE(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,BX(1),BY(1),
     +    XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
     1   XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
C
C      WRITE(UNIT=10,FMT='(A,I4)') 'WRITE STORE Entp is ',ENTP
C      WRITE(UNIT=10,FMT='(2I6,2F8.3,2(/,5F10.5))')
C     +    MIP1,ENT1,BX(1),BY(1),
C     +    XY(1,1), XY(2,1), XY(3,1), XY(4,1), XY(5,1),
C     1   XYB(1,1),XYB(2,1),XYB(3,1),XYB(4,1),XYB(5,1)
C
C
C     Store the start or previously defined boundary
      WRITE(UNIT=HUNIT,REC=POINT) MIP2,ENT2,BX(2),BY(2),
     +    XY(1,2), XY(2,2), XY(3,2), XY(4,2), XY(5,2),
     1   XYB(1,2),XYB(2,2),XYB(3,2),XYB(4,2),XYB(5,2)
C
C      WRITE(UNIT=10,FMT='(A,I4)') 'WRITE STORE Point is ',POINT
C      WRITE(UNIT=10,FMT='(2I6,2F8.3,2(/,5F10.5))')
C     +       MIP2,ENT2,BX(2),BY(2),
C     +    XY(1,2), XY(2,2), XY(3,2), XY(4,2), XY(5,2),
C     1   XYB(1,2),XYB(2,2),XYB(3,2),XYB(4,2),XYB(5,2)
C
C
      IF ( NEWBD  ) THEN
         ENTP = TEMPNT-1
         BDP=ENTP+1
         NBDP=NBDP+1
         SBDP(NBDP)=BDP
         NEWBD=.FALSE.
         CALL GTMCLO(BMEN,BCELL)
         AREA = .TRUE.
         ACHK = ENTP + 1
      END IF
C
C     Increment store for potential boundaries
C      WRITE(UNIT=10,FMT='(A,I5)' ) 
C    +     'Number of Boundarys stored : ',ENTP
      ENTP=ENTP+1
      FIRST = .FALSE.
C
C     Draw out the cross which was indicating to the user where
C     the hit was taken
C
C     If not accept that was hit then continue
C     then it must have been new boundary
      NO=ENTP-1
      IF ( CCMD .NE. CHAR(150) ) GOTO 10
C
C     NO contains the number of actual boundaries found
 80   CONTINUE
C     reset drawing code
      DDCODE = 0
      CLOSE(UNIT=CNCL)
C
      END
C
C     ---------------------------------------------------
C

      SUBROUTINE SRTARC(X1,Y1,X2,Y2,X3,Y3)
C     ====================================
C1    VARTYP            R  R  R  R  R  R
C1    IOSTAT            IO IO IO IO I  I
C
C2    This routine will sort a start and finish coordinate
C2    of an arc  COUNTERCLOCKWISE
C2    X2 and Y2 will be the start angle what ever happens
C
      REAL X1,Y1,X2,Y2,X3,Y3,V1,V2,V3
      REAL L1,L2,L3,P1,P2,P3,C1,C2,C3
      REAL U1,U2,U3
      REAL MX1,MX2,MY1,MY2
C
C     Generate 2 vectors
      CALL CV0L14(X1,Y1,X3,Y3,L1,L2,L3)
      CALL CV0L14(X2,Y2,X3,Y3,P1,P2,P3)
C     Mid points of vectors
      MX1 = (X1+X3)/2
      MX2 = (X2+X3)/2
      MY1 = (Y1+Y3)/2
      MY2 = (Y2+Y3)/2
C
C     gen two vectors perp
      CALL VV00L6 (L1,L2,L3,MX1,MY1,V1,V2,V3)
      CALL VV00L6 (P1,P2,P3,MX2,MY2,U1,U2,U3)
C
      CALL CRSPRD(V1,V2,V3,U1,U2,U3,C1,C2,C3)
C     swop coords if necessary
      IF(C3.LT.0.0) THEN
C
          CALL RSWAP(X1,X2)
          CALL RSWAP(Y1,Y2)
C
      ENDIF
C
 
      END
C
C     ---------------------------------------------------
C
