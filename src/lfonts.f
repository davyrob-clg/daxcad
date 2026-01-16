C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 lfonts.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DRWFAW(X,Y,WRAD,SANG,EANG,FONT)
C     SUBROUTINE DRWFLW(X1,Y1,X2,Y2,FONT)
C     SUBROUTINE ERSFAW(X,Y,R,SANG,EANG,FONT)
C     SUBROUTINE ERSFLW(X1,Y1,X2,Y2,FONT)
C     SUBROUTINE LFSETS()
C     SUBROUTINE PAP2SC(PAPL,SCRL)
C     SUBROUTINE SETVIS(VISIBL)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DRWFAW(X,Y,WRAD,SANG,EANG,FONT)
C     ==========================================
C
C1    vartype           R R   R    R   R    I4
C1    iostatus          I I   I    I   I    I
C
C2       Subroutine ARCCT draw a arc  centre X,Y
C2       with radius WRAD drawing anti-clockwise
C2       using line font FONT
C
      include 'include/lfont.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER*4 FONT,NFPATS,I,J,LFSEGP
C
      REAL X,Y,WRAD,SANG,EANG,ANGLE
      REAL PI,SRAD,ANG1,ANG2,PATLEN,WFONTL
C
      LOGICAL TOTAL,OK,VISIBL,PENDWN
      INTRINSIC ABS,INT
C
      EXTERNAL WTOSVX,ARCCT,ARCCHK,SETVIS
C
C
      DISPV=.FALSE.
C     test actual screen size of arc
      CALL WTOSVX(WRAD,SRAD)
      IF (SRAD.LT. 2.0) THEN
C        too small to draw an arc,a point will do
         CALL WO2SC(X,Y,ANG1,ANG2)
         CALL WRKHIT(ANG1,ANG2,DISPV)
         IF ( DISPV ) CALL POINTS(ANG1,ANG2)
         GOTO 99
      END IF
C
C     we are going to have to draw something now
      IF (FONT.EQ.1.OR.LFONTR(2,1).LT.0.75) THEN
C        solid line construction wil do
         CALL ARCCT(X,Y,WRAD,SANG,EANG)
      ELSE
C        fonting is required,must do some work
C        find pointer to font segment data
         LFSEGP=LFONTI(1,FONT)
C        find world length for complete font pattern in radians
         PATLEN=PAPTOW*LFONTR(1,FONT)/WRAD
C        find included angle of arc
         ANGLE=ABS(EANG-SANG)
         IF (EANG.LT.SANG) ANGLE=PI(2.0)-ANGLE
C        now find number of complete patterns
         NFPATS=INT(ANGLE/PATLEN)
C        trap for NFPATS=0
         IF (NFPATS.LT.1) THEN
C           cannot draw a proper font pattern
C           so just draw solid
            CALL ARCCT(X,Y,WRAD,SANG,EANG)
            GOTO 99
         END IF
C        now calculate actual font length
         WFONTL=ANGLE/NFPATS
C        update incremental seg lengths for
C        line font in use here,should save
C        some time during drawing of arc
C        cycle through pattern segments
         DO 50 I=0,LFONTI(2,FONT)-1
C           save incremental angle for this segment
            LFTAB2(2,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*WFONTL
 50      CONTINUE
C        all pattern segment increments up to date
C        set first position on arc
         ANG1=SANG
C        now go and draw the fonted arc
C        draw the required number of patterns
         VISIBL=.FALSE.
         DO 70 I=1,NFPATS
C           draw a single font pattern
C           assume starts with solid segment
            PENDWN=.TRUE.
            DO 60 J=0,LFONTI(2,FONT)-1
C              draw a single segment
               ANG2=ANG1+LFTAB2(2,LFSEGP+J)
               IF (PENDWN) CALL ARCCT(X,Y,WRAD,ANG1,ANG2)
               IF ( DISPV ) VISIBL=.TRUE.
C              update start point for next segment
               ANG1=ANG2
C              toggle pen status
               PENDWN=.NOT.PENDWN
 60         CONTINUE
 70      CONTINUE
         DISPV=VISIBL
      END IF
 99   CONTINUE
C     set visibility flag
      VISIBL=DISPV
      CALL SETVIS(VISIBL)
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DRWFLW(X1,Y1,X2,Y2,FONT)
C     ===================================
C
C1                       R, R, R, R  I4
C1                       I, I, I, I  I
C
C2    Subroutine DRWFLW draws a line between (X1,Y1)
C2    and (X2,Y2) using the line font FONT
C2    if it exists on the current viewport
C2    X1,Y1,X2,Y2 are in world coordinates
C
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      INTEGER*4 FONT,NFPATS,LFSEGP,I,J,NINT,BIT,st
C
      REAL  X1, Y1, X2, Y2,
     +     WX1,WY1,WX2,WY2,
     1     VX1,VY1,VX2,VY2
      REAL WL,LL,LSTSEG,DISTXY,SFONTL,SFDX,SFDY
      REAL SX1,SY1,SX2,SY2,WSL
C
      LOGICAL DISPV,PENDWN,DSTOP
C
      COMMON /WCURSR/WCPX,WCPY
      REAL WCPX,WCPY
C
      INTRINSIC ABS,NINT
C
      EXTERNAL CLIP,WO2SC,DRAWLS,DISTXY,SETVIS,DRAWLW
C
C     Transfer of values so that the end points are not
C     corrupted by clipping subroutine
      WX1=X1
      WY1=Y1
      WX2=X2
      WY2=Y2
C
      FONT=MAX(FONT,1)
 
C     Clipping routine will supply new line to be drawn
C     If visible logical flag DISPV returned .TRUE.
      CALL CLIP(WX1,WY1,WX2,WY2,DISPV)
      IF (DISPV) THEN
C        if font 1 (solid) no tests needed or
C        current viewscale may mean fonting not visible
C        either case means use solid line
C        the limit 0.75 will need adjusted for
C        different display systems
         IF (FONT.EQ.1.OR.LFONTR(2,1).LT.0.75.OR.FONT.GT.NFONTS) THEN
C           Font has no definition.
C           Too small to display properly
C           solid line construction
            CALL DRAWLT(WX1,WY1,WX2,WY2)
         ELSE
C           not a solid line,got some work to do
C           find pointer to segment data
            LFSEGP=LFONTI(1,FONT)
C           must do some calcs to allow fonting
C           find line length in world units
            WL=DISTXY(WX1,WY1,WX2,WY2)
C           find world length for complete font patterns
C           WSL is the world segment length
            WSL=LFONTR(1,FONT)*PAPTOW
C           allow for solid segment at end of line
C           save length of end segment
            LSTSEG=LFTAB2(1,LFSEGP)*WSL
            LL=WL-LSTSEG
C           now find number of complete patterns
            NFPATS=NINT(LL/WSL)
C           trap for NFPATS=0
            IF (NFPATS.LT.1) THEN
C              cannot draw a proper font pattern
C              so just draw solid
               CALL DRAWLT(WX1,WY1,WX2,WY2)
               GOTO 99
            END IF
C           now calculate actual font length
            SFONTL=LL/NFPATS
C           find incremental world coords for
C           one pattern length
            SFDX=SFONTL*(WX2-WX1)/WL
            SFDY=SFONTL*(WY2-WY1)/WL
C           update incremental seg lengths for
C           line font in use here,should save
C           some time during drawing of line
C           cycle through pattern segments
            DO 50 I=0,LFONTI(2,FONT)-1
C              save world increments for this segment
               LFTAB2(2,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*SFDX
               LFTAB2(3,LFSEGP+I)=LFTAB2(1,LFSEGP+I)*SFDY
 50         CONTINUE
C           all pattern segment increments up to date
C
C           set first position on line
            SX1=WX1
            SY1=WY1
C           now go and draw the fonted line
C           draw the required number of patterns
            DO 70 I=1,NFPATS
C              assume starts with solid segment
               PENDWN=.TRUE.
               DO 60 J=0,LFONTI(2,FONT)-1
C                 draw a single segment
                  SX2=SX1+LFTAB2(2,LFSEGP+J)
                  SY2=SY1+LFTAB2(3,LFSEGP+J)
                  IF (PENDWN) THEN
C                    must draw line with correct fonting
                     CALL DRAWLT(SX1,SY1,SX2,SY2)
                  END IF
C                 update start point for next segment
                  SX1=SX2
                  SY1=SY2
C                 toggle pen status
                  PENDWN=.NOT.PENDWN
 60            CONTINUE
 70         CONTINUE
C           bust out here on DSTOP
 80         CONTINUE
C
C           now draw the last segment
            IF (PENDWN) CALL DRAWLT(SX1,SY1,WX2,WY2)
         END IF
 99      CONTINUE
C        update cursor position to end of
C        line just drawn.
         WCPX=WX2
         WCPY=WY2
      END IF
C
C     set visibility flag
      CALL SETVIS(DISPV)
C
      END
C
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSFAW(X,Y,R,SANG,EANG,FONT)
C     ======================================
C
C1                      R R R  R    R    I4
C1                      I I I  I    I    I
C
C2    Subroutine ERSFAW erases an arc centre X,Y
C2    radius R starting angle SANG,end angle EANG
C2     using the line font FONT
C2    if it exists on the current viewport
C2    X,Y,R are in world coordinates
C
      INTEGER*4 FONT
      REAL X,Y,R,SANG,EANG
C
      EXTERNAL PENERS,PENDRW,DRWFAW
C
      CALL PENERS()
      CALL DRWFAW(X,Y,R,SANG,EANG,FONT)
      CALL PENDRW()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSFLW(X1,Y1,X2,Y2,FONT)
C     ===================================
C1                       R, R, R, R  I4
C1                       I, I, I, I  I
C
C2    Subroutine ERSFLW erases a line between (X1,Y1)
C2    and  (X2,Y2) using the line font FONT
C2    if it exists on the current viewport
C2    X1,Y1,X2,Y2 are in world coordinates
C
      INTEGER*4 FONT
      REAL X1,Y1,X2,Y2
C
      EXTERNAL PENERS,PENDRW,DRWFLW
C
      CALL PENERS()
      CALL DRWFLW(X1,Y1,X2,Y2,FONT)
      CALL PENDRW()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE LFSETS()
C     ===================
C
C1    no arguments required
C
C2    Subroutine LFSETS updates the line font
C2    table to contain the current view font parameter
C2    of screen length for each of the line font patterns.
C
      include 'include/lfont.inc'
C
      INTEGER*4 I
C
      EXTERNAL PAP2SC
C
C     cycle through all line fonts
      DO 50 I=1,NFONTS
C        convert paper length to screen length
         CALL PAP2SC(LFONTR(1,I),LFONTR(2,I))
 50   CONTINUE
C     all done
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE PAP2SC(PAPL,SCRL)
C     ============================
C
C1    vartype            R    R
C1    iostatus           I    O
C
C2    Subroutine PAP2SC converts the distance PAPL
C2    supplied in paper units to an equivalent length
C2    in screen units (pixels) and returns in SCRL.
C2    Due to the differential resolutions often found
C2    between X and Y axes on a graphics screen,this
C2    routine returns the average of the equivalent
C2    pixel lengths on X and Y.
C
      include 'include/ndata.inc'
C
      REAL PAPL,SCRL,L1,L2
C
      EXTERNAL WTOSVX,WTOSVY
C
C     find equivalent length on X axis
      CALL WTOSVX(PAPL*PAPTOW,L1)
C     find equivalent length on Y axis
      CALL WTOSVY(PAPL*PAPTOW,L2)
C     find average of these two lengths
      SCRL=(L1+L2)/2
C
C     return pixel length in SCRL
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SETVIS(VISIBL)
C     =========================
C
C1    vartype             L
C1    iostatus            I
C
C2    Subroutine SETVIS sets the common block
C2    variable DISPV to the same state as the
C2    passed argument VISIBL.Ensures visibility
C2    of entities is maintained during display
C2    file manipulation.
C
      include 'include/masti.inc'
C
      LOGICAL VISIBL
C
      DISPV=VISIBL
C
      END
C
C-------------------------------------------------------------
C
