C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah0.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ADSRCH(ETYP)
C     SUBROUTINE ALSRCH()
C     SUBROUTINE CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
C     SUBROUTINE FILL()
C     SUBROUTINE INFLAG()
C     SUBROUTINE NOSRCH()
C     SUBROUTINE SSFLAG(MIPP,SX,SY,TDFP,P2,FLAG)
C     SUBROUTINE UNFLAG(CLEAR)
C     SUBROUTINE ZRFLAG()
C     SUBROUTINE ZSFLAG(ONLY1,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE ADSRCH(ETYP)
C     =======================
C1    no arguments required
C
C2    subroutine ADSRCH enables search status
C2    for entity type ETYP
C
C
C
C
C
C
C
C
******************************************************************
********                                                **********
********             D A T A      B A S E               **********
********                                                **********
********             S T R U C T U R E                  **********
********                                                **********
******************************************************************
C
C        MASTER(1) = STATUS
C             Value from following :
C             --------------------
C           10 - Normal    Editable  Entity
C           20 - Delete    Protected    "
C           40 - Component Construct    "
C           40 - Symbol    Construct    "
C           50 - Group                  "
C          100 - Deleted                "
C         >128 - Entity found on search (temporary state)
C
C
C        MASTER(2) = ENTYPE
C
C             Value from following :
C             --------------------
C               2 - Marker
C               3 - Line
C               5 - Arc
C               6 - Ellipse
C               7 - Spline
C              30 - Centreline
C              31 - Crosshatching
C              33 - Linear   Dimension
C              34 - Angular      "
C              35 - Radial       "
C              36 - General  Label
C              37 - Diameter Dimension
C              50 - Group
C              51 - Detail
C              55 - Symbol    master
C              56 - Component master
C              65 - Symbol    instance
C              66 - Component instance
C              82 - Text Node
C              85 - Text
C
C
C        MASTER(3) = Colour
C        MASTER(4) = Layer
C        MASTER(5) = Form of polygon fit
C        Add from below to find which from to display
C                   +0  Points only
C                   +1  Polygon fit
C                   +2  Curve fit
C                   +3  Polygon and points
C                 Value
C                   4   Points
C                   8   Polygon
C                  12   Quad
C                  16   Hermite
C                  20   Bezier
C                  24   B-Spline
C
C        MASTER(6) = Font
C        MASTER(7) = Part data file pointer
C        MASTER(8) = Parent pointer ( if group )
C        MASTER(9) = Text pointer
C        MASTER(10)= Relation pointer ( groups )
C        MASTER(11)= Property pointer
C        MASTER(12)= Thickness
C        MASTER(13)= registration serial number
C
C        PART INDEX(1)=  Entity type
C        PART INDEX(2)=  Master index pointer
C        PART INDEX(3)=  Continuation pointer
C        PART INDEX(4)=  Spare
C
C        CBUFF   Text
C        TEXT INDEX(1)  Master index pointer
C        TEXT INDEX(2)  Continuation pointer
C
C        REC1(1...6) data on entity see below for detail
C        of each particular type
C
C LINE   *****************
C        PART REC1(1)= X coordinate of first endpoint
C        PART REC1(2)= Y     "      "    "       "
C        PART REC1(3)= -
C        PART REC1(4)= X coordinate of second endpoint
C        PART REC1(5)= Y     "      "     "      "
C        PART REC1(6)= -
C
C ARC    *****************
C        PART REC1(1)= X coordinate of centre of arc
C        PART REC1(2)= Y     "       "   "    "   "
C        PART REC1(3)= -
C        PART REC1(4)= Radius of the arc
C        PART REC1(5)= Start angle (radians) from horizontal
C        PART REC1(6)= Encluded angle of the arc
C
C ELIPSE *****************
C        PART REC1(1)= X coordinate of centre of arc
C        PART REC1(2)= Y     "       "   "    "   "
C        PART REC1(3)= -
C        PART REC1(4)= Major Radius of the arc
C        PART REC1(5)= Start angle (radians) from horizontal
C        PART REC1(6)= Encluded angle of the arc
C
C        PART REC2(1)= Minor radius
C        PART REC2(2)= Incrimental angle
C        PART REC2(3)= -
C        PART REC2(4)= -
C        PART REC2(5)= -
C        PART REC2(6)= -
C TEXT   *****************
C        PART REC1(1)= X defined origin of the text
C        PART REC1(2)= Y    "      "    "   "   "
C        PART REC1(3)= Width  of characters
C        PART REC1(4)= Height "       "
C        PART REC1(5)= Angle  "       "
C        PART REC1(6)= Number of characters,Slant,Justification
C
C HATCH  *****************
C        PART INDEX(3)=  CONTINUATION POINTER to next line in hatch
C        PART REC1(1)= X coordinate lower left bound of hatch
C        PART REC1(2)= Y     "        "    "     "    "   "
C        PART REC1(3)= -
C        PART REC1(4)= X coordinate upper right bound of hatch
C        PART REC1(5)= Y     "        "     "     "    "   "
C        PART REC1(6)= -
C
C        PART INDEX(3)=  CONTINUATION POINTER to next line in hatch
C        PART REC2(1)= X coordinate of first endpoint
C        PART REC2(2)= Y     "      "    "       "
C        PART REC2(3)= -
C        PART REC2(4)= X coordinate of second endpoint
C        PART REC2(5)= Y     "      "     "      "
C        PART REC2(6)= -
C
C SPLINE *****************
C        PART REC1(1)= X coordinate of first point
C        PART REC1(2)= Y     "      "    "     "
C        PART REC1(3)= Z     "      "    "     "
C        PART REC1(4)= X lock for above point (eg Hermit etc,)
C        PART REC1(5)= Y   "   "    "     "         "
C        PART REC1(6)= Z   "   "    "     "         "
C
C        PART REC2(1)= X coordinate of next point
C        PART REC2(2)= Y     "      "    "    "
C        PART REC2(3)= Z     "      "    "    "
C        PART REC2(4)= X lock for above point  (eg Hermit etc,)
C        PART REC2(5)= Y   "   "    "     "          "
C        PART REC2(6)= Z   "   "    "     "          "
C
C
C LINEAR DIMENSION *******
C        PART REC1(1)= Reference point1  X
C        PART REC1(2)=     "       "     Y
C        PART REC1(3)= Dimension point1  X
C        PART REC1(4)=     "        "    Y
C        PART REC1(5)= dimension (1) line length
C        PART REC1(6)= witness   (1) line length
C
C        PART REC2(1)= Reference point2  X
C        PART REC2(2)=     "       "     Y
C        PART REC2(3)= Dimension point2  X
C        PART REC2(4)=     "       "     Y
C        PART REC2(5)= dimension (2) line length
C        PART REC2(6)= witness   (2) line length
C
C        PART REC3(1)= Unit vector U1
C        PART REC3(2)=  "      "   U2
C        PART REC3(3)= Gap length
C        PART REC3(4)= Extension length
C        PART REC3(5)= Arrow length
C        PART REC3(6)=   "   width
C
C RADIAL DIMENSION *******
C        PART REC1(1)= Leader start point x ( at arrowhead )
C        PART REC1(2)=     "       "      Y       "
C        PART REC1(3)= Leader knee bend  X
C        PART REC1(4)=     "        "    Y
C        PART REC1(5)= Leader end point X ( at text )
C        PART REC1(6)= Leader end point Y
C
C        PART REC2(1)= Arrow Apex point X
C        PART REC2(2)=   "     "   "    Y
C        PART REC2(3)= Unit vector U1
C        PART REC2(4)=  "      "   U2
C        PART REC2(5)= Arrow length
C        PART REC2(6)=   "   width
C
C DIAMETRAL DIMENSION *******
C        PART REC1(1)= Leader start point x ( at arrowhead )
C        PART REC1(2)=     "       "      Y       "
C        PART REC1(3)= Leader knee bend  X
C        PART REC1(4)=     "        "    Y
C        PART REC1(5)= Leader end point X ( at text )
C        PART REC1(6)= Leader end point Y
C
C        PART REC3(1)= Arrow Apex point X
C        PART REC3(2)=   "     "   "    Y
C        PART REC3(3)= Unit vector U1
C        PART REC3(4)=  "      "   U2
C        PART REC3(5)= Arrow length
C        PART REC3(6)=   "   width
C
C ANGULAR DIMENSION *******
C        PART REC1(1)= Dimension Arc 1 Centre X
C        PART REC1(2)=     "       "      "   Y
C        PART REC1(3)=     "       "      "   Z
C        PART REC1(4)=     "       "  Radius
C        PART REC1(5)=     "       "  Start Angle
C        PART REC1(6)=     "       "  End Angle
C
C        PART REC2(1)= Arrow 1 Apex point X
C        PART REC2(2)=   "       "   "    Y
C        PART REC2(3)= Arrow length
C        PART REC2(4)=   "   width
C        PART REC2(5)= Unit vector U1
C        PART REC2(6)=  "      "   U2
C
C        PART REC3(1)= Projection Line 1 X coord
C        PART REC3(2)=     "       "     y coord
C        PART REC3(3)=     "       "     Z coord
C        PART REC3(4)=     "       "     Gap length
C        PART REC3(5)=     "       "     Extension
C        PART REC3(6)=     "       "     Line Length
c
C        PART REC4(1)= Dimension Arc 2 Centre X
C        PART REC4(2)=     "       "      "   Y
C        PART REC4(3)=     "       "      "   Z
C        PART REC4(4)=     "       "  Radius
C        PART REC4(5)=     "       "  Start Angle
C        PART REC4(6)=     "       "  End Angle
C
C        PART REC5(1)= Arrow 2 Apex point X
C        PART REC5(2)=   "       "   "    Y
C        PART REC5(3)= Arrow length
C        PART REC5(4)=   "   width
C        PART REC5(5)= Unit vector U1
C        PART REC5(6)=  "      "   U2
C
C        PART REC6(1)= Projection Line 2 X coord
C        PART REC6(2)=     "       "     y coord
C        PART REC6(3)=     "       "     Z coord
C        PART REC6(4)=     "       "     Gap length
C        PART REC6(5)=     "       "     Extension
C        PART REC6(6)=     "       "     Line Length
C
C
C
      include  'include/masti.inc'
C
      INTEGER*2 ETYP
C
      ENSRCH(ETYP)=.TRUE.
C
      END
C
      SUBROUTINE ALSRCH()
C     ===================
C1    no arguments required
C
C2    subroutine ALSRCH enables search status
C2    for all entity types.
C
      include  'include/masti.inc'
C
      INTEGER*2 I
C
      DO 50 I=0,127
         ENSRCH(I)=.TRUE.
 50   CONTINUE
C
      END
C
C
      SUBROUTINE CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
C     =======================================
C1     vartype           R, R, R, R, L,  L
C1     iostatus          B, B, B, B, O,  O
C
C2    NO COMMENTS SUPPLIED!!!!!!!!
      include   'include/curwin.inc'
C
      LOGICAL OK,FPOINT,TOTAL
      REAL     X1, X2, Y1, Y2, A, B, C, D, VAR, NEW
      REAL XLOW,XUP,YLOW,YUP,X,Y
      INTEGER IX1,IX2,IY1,IY2,IX,IY,CEN,LEFT,RIGHT,TOP,BOTTOM,SETCLP
C
      PARAMETER (CEN=0,LEFT=-1,RIGHT=1,TOP=1,BOTTOM=-1)
C
      EXTERNAL SETCLP
C
C        Linear interpolation equation used for clipping
C        line to limit of cursor window
C
      NEW(A,B,C,D,VAR)=A+((B-A)*(VAR-C)/(D-C))
C
C        Check position of points on window
C           X coordinates    -1 off left
C                             0 centre
C                             1 off right
C           Y cordinates     -1 off bottom
C                             0 centre
C                             1 off top
      XLOW=XCURS-XWIDTH
      XUP =XCURS+XWIDTH
      YLOW=YCURS-YWIDTH
      YUP =YCURS+YWIDTH
C
      IX1=SETCLP(X1,XLOW,XUP)
      IY1=SETCLP(Y1,YLOW,YUP)
      IX2=SETCLP(X2,XLOW,XUP)
      IY2=SETCLP(Y2,YLOW,YUP)
C
      OK = .FALSE.
      TOTAL=.TRUE.
C
C     If product is 1 then line totally outside window
 10   IF((IX1*IX2).EQ.1 .OR. (IY1*IY2).EQ.1 )  RETURN
C
C     If Fpoint .true. then first point is inside window
      FPOINT = IX1.EQ.CEN .AND. IY1.EQ.CEN
C
C     If OK .true. then both end points are inside window
      OK  = FPOINT .AND. IX2.EQ.CEN .AND. IY2.EQ.CEN
C
C     Therefore return
      IF ( OK ) RETURN
C
C     line was not totally inside window
      TOTAL=.FALSE.
C
C     If first .true. then clip second point
      IF ( FPOINT ) THEN
         IX=IX2
         IY=IY2
      ELSE
         IX=IX1
         IY=IY1
      END IF
C
      IF ( IX .EQ. LEFT ) THEN
         Y=NEW(Y1,Y2,X1,X2,XLOW)
         X=XLOW
      ELSE IF ( IX .EQ. RIGHT ) THEN
         Y=NEW(Y1,Y2,X1,X2,XUP )
         X=XUP
      ELSE IF ( IY .EQ. TOP ) THEN
         X=NEW(X1,X2,Y1,Y2,YUP)
         Y=YUP
      ELSE IF ( IY .EQ. BOTTOM ) THEN
         X=NEW(X1,X2,Y1,Y2,YLOW)
         Y=YLOW
      END IF
C
      IF ( IX.EQ.IX1.AND.IY.EQ.IY1 ) THEN
         X1=X
         Y1=Y
         IX1=SETCLP(X1,XLOW,XUP)
         IY1=SETCLP(Y1,YLOW,YUP)
      ELSE
         X2=X
         Y2=Y
         IX2=SETCLP(X2,XLOW,XUP)
         IY2=SETCLP(Y2,YLOW,YUP)
      END IF
C
      GOTO 10
C
      END
*
      SUBROUTINE FILL()
C     =================
C1    no arguments required
C
C2    Subroutine FILL fills the MI record buffer
C2    with the currently set default conditions
C2    of entity status,layer,colour,type etc,and
C2    uses the current pointer values for PDF,MIP.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/style.inc'
C
C
C     MI data allocated as folows
         IMBUFF(1)=  STATUS
         IMBUFF(3)=  COLOUR
         IMBUFF(4)=  CLAYER
         IMBUFF(5)=  SPARE
         IMBUFF(6)=  FONT
         IMBUFF(7)=  PDFP
         IMBUFF(8)=  SPARE
         IMBUFF(9)=  TXTP
         IMBUFF(10)= SPARE
         IMBUFF(11)= SPARE
         IMBUFF(12)= SPARE
         IMBUFF(13)= SPARE
C
      END
C
      SUBROUTINE INFLAG()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    subroutine INFLAG sets the valid entity count
C2    beyond the end of the current buffer,to prevent
C2    unnecessary attempts to unflag the entities.
C
      include  'include/swind.inc'
      include  'include/nbuff.inc'
C
C     set valid flag to end of buffer
      VNDATA=NDATA+1
C
      END
C
C----------------------------------------------------------
C
      SUBROUTINE NOSRCH()
C     ===================
C1    no arguments required
C
C2    subroutine NOSRCH disables search status
C2    for all entity types.
C
      INTEGER*2 I
C
      include  'include/masti.inc'
C
      DO 50 I=0,127
         ENSRCH(I)=.FALSE.
 50   CONTINUE
C
      END
C
C
      SUBROUTINE SSFLAG(MIPP,SX,SY,TDFP,P2,FLAG)
C     ==========================================
C
C1    vartype           I2    R  R   I2 I2   L
C1    iostatus           I    I  I    I  I   I
C
C2    Subroutine SSFLAG sets an attention flag on the
C2    screen at the position SX,SY and stores the screen
C2    position and MI pointer MIPP in the workfile
C2    attached to SWINDU.
C
      include  'include/swind.inc'
      include  'include/nbuff.inc'
      include  'include/viewport.inc'
C
      REAL SX,SY
C
      INTEGER*2 MIPP,TDFP,P2,TENT
      LOGICAL FLAG
C
      EXTERNAL LABFLG
C
      NDATA=NDATA+1
      WRITE(UNIT=SWINDU,REC=NDATA) MIPP,SX,SY,TDFP,P2
      IF ( .NOT.FLAG ) RETURN
C
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set drawing code for any viewports that are active
          DDCODE = 2
      ENDIF
      CALL LABFLG(MIPP)
      TENT=IMBUFF(2)
      CALL ALLDRW(TENT,MIPP)
C     reset drawing code
      DDCODE = 0
C
      END
C
C
      SUBROUTINE UNFLAG(CLEAR)
C     ========================
C
C1    vartype             L
C1    iostatus            I
C
C2    subroutine UNFLAG recovers all temporarily
C2    flagged entities form the workfile,and
C2    removes the attention flags from the screen
C2    If the logical flag CLEAR is true,then the
C2    scratch file pointer is cleared to zero.
C
      include  'include/swind.inc'
      include  'include/nbuff.inc'
      include  'include/viewport.inc'
C
      INTEGER*4 I
      INTEGER*2 MIP,MIPP,TENT
      REAL BX,BY
      LOGICAL CLEAR
C
      EXTERNAL DBOX,LABNFG
C
C     test for existance of flags first.
      IF (NDATA.GT.0) THEN
         DO 50 I=1,NDATA
C           find the flag position from workfile.
            READ(UNIT=SWINDU,REC=I) MIP,BX,BY
C           Check its valid.
            IF (MIP.GT.0) THEN
C              remove the attention flag.
               CALL LABNFG(MIP)
               IF (I.GE.VNDATA) THEN
                   IF(MVPACT.AND..NOT.MAWS) THEN
C                     viewport drawing code
                      DDCODE = 2
                   ENDIF
C                  CALL DBOX(BX,BY)
                   MIPP=MIP
                   CALL ALLDRW(TENT,MIPP)
C                  reset
                   DDCODE = 0
               END IF
            END IF
 50      CONTINUE
      END IF
C     recovery now complete
      IF (CLEAR) THEN
C        set NDATA back to zero
         NDATA=0
         VNDATA=1
      ELSE
C        set valid flag to end of buffer
         VNDATA=NDATA+1
      END IF
C     everything ok,return as normal
C
      END
C
C----------------------------------------------------------
C
      SUBROUTINE ZRFLAG()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    subroutine ZRFLAG resets the entity counts to zero
C2    which effectively clears the scratch file of entities.
C
      include  'include/swind.inc'
      include  'include/nbuff.inc'
C
C     clear entity count
      NDATA=0
C     set valid flag number
      VNDATA=1
C
      END
C
C----------------------------------------------------------
C
 
C
      SUBROUTINE ZSFLAG(ONLY1,OK)
C     ===========================
C1    vartype             L   L
C1    iostatus            I   O
C
C2    Subroutine ZSFLAG clears the screen flag
C2    for the last entity stored in the entity
C2    select buffer file 'SWINDU',and corrects
C2    the pointer to the end of that buffer.
C2    OK returned .TRUE. if flag cleared,if
C2    .FALSE. then nothing in buffer.
C2    If ONLY1 is true then only the last entry in
C2    the scratch file is removed and unflagged.
C
      include  'include/ndata.inc'
      include  'include/swind.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 MIP,TDFP,P2,LPAR,MIPP,TENT
C
      REAL BX,BY
C
      LOGICAL OK,FIRST,ONLY1
C
      EXTERNAL DBOX,DIM500,DIR500
C
      IMBUFF(8)=-1
      FIRST=.TRUE.
C
10    CONTINUE
C
      IF ( IMBUFF(8) .NE.0  ) LPAR=IMBUFF(8)
C     beter make sure that the buffer contains something
      IF (NDATA.GT.0 ) THEN
C        remove last entity from list
         READ(UNIT=SWINDU,REC=NDATA) MIPP,BX,BY
C        test for existance of flags first.
         IF (MIPP.GT.0) THEN
            CALL DIR500(MIPP,OK)
            IF ( FIRST.OR.IMBUFF(8).EQ.LPAR ) THEN
               IF ( IMBUFF(1) .GT. 128 ) THEN
                  IMBUFF(1)=IMBUFF(1)-128
                  TENT=IMBUFF(2)
                  MIP=MIPP
                  CALL DIM500(MIPP,OK)
               END IF
C        
               IF (NDATA.GE.VNDATA) THEN
                   IF(MVPACT.AND..NOT.MAWS) THEN
C                      set drawing code for DAXPORTS
                       DDCODE = 2
                   ENDIF
C                  CALL DBOX(BX,BY)
                   CALL ALLDRW(TENT,MIPP)
                   DDCODE = 0
               ELSE
C                 valid flag must be next one to be drawn
C                 set equal since main pointer will be decremented
                  VNDATA=NDATA
               END IF
C              decrement data pointer
               NDATA=NDATA-1
C              recovery now complete
               OK=.TRUE.
C              only remove one symbol or component
               IF (TENT.EQ.COMPI.OR.TENT.EQ.SYMBI) THEN
                  CALL DIR500(MIPP,OK)
               END IF
C              only remove last entry
               IF (FIRST.AND.ONLY1) RETURN
               FIRST=.FALSE.
               IF ( NDATA.GT.0 )GOTO 10
            END IF
         ELSE
C           if a dud master index was found, drop it and
C           go around again
            NDATA = NDATA - 1
            IF (NDATA.GT.0) GOTO 10
         END IF
      ELSE
C        no entity to recover
         OK=.FALSE.
      END IF
C
      END
C
C----------------------------------------------------------
C
