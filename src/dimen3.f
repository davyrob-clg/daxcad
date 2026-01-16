C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 dimen3.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ANSDIM(NX1,NY1,NX2,NY2,NX3,NY3,BLWD,BLHT,X4,Y4,ENT
C     SUBROUTINE ARRREC(RCDCNT,SUPLFT,SUPRGT)
C     SUBROUTINE CHKARR(DIMD,RALNG,BLWD,TXTOUT,ARROIN)
C     SUBROUTINE CHKLFT(X3,Y3,X4,Y4,P1,P2,U1,U2,LEFT)
C     SUBROUTINE CHKORD(X1,Y1,X2,Y2,SWAPED)
C     SUBROUTINE DIMF33(X1,Y1,X2,Y2,X3,Y3,X4,Y4)
C     SUBROUTINE DIMF35(DIMTYP,X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG)
C     SUBROUTINE DIML03(X1,Y1,X2,Y2,X3,Y3,X4,Y4,REGEN,M,OK)
C     SUBROUTINE DIML04(NX1,NY1,NX2,NY2,CPX,CPY,U1,U2,L1,L2,L3,BLWD,
C     SUBROUTINE DIMLDR(X1,Y1,X2,Y2,X3,Y3,BLWD,BLHT,U1,U2,X4,Y4)
C     SUBROUTINE DIMORG()
C     SUBROUTINE DIMR03(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,OK)
C     SUBROUTINE DIMREC(RCDCNT,SEGTYP,SUPLFT,SUPRGT)
C     SUBROUTINE DMARRC(RCDCNT,DIMLIN)
C     SUBROUTINE DMFSRC(RCDCNT,DIMTYP,GAPEXT)
C     SUBROUTINE DMMREC(X1,X2,X3,X4,X5,X6,TALLY,RELDAT,OK)
C     SUBROUTINE DMSGRC(RCDCNT,DIMTYP,GAPEXT)
C     SUBROUTINE DMTRRC(RCDCNT,BLWD,BLHT)
C     SUBROUTINE DMTXCK(DIMLEN,BLWD,BLHT,TXTOUT)
C     SUBROUTINE DMTXRC(RCDCNT,DIMTYP)
C     SUBROUTINE DMWREC(X1,X2,X3,X4,X5,X6,TALLY,RELDAT,OK)
C     SUBROUTINE FRSTRC(RCDCNT,ARROIN,TXTOUT,DIMTYP)
C     SUBROUTINE ORGDAT()
C     SUBROUTINE PRJREC(RCDCNT,SEGTYP,LEFT)
C     SUBROUTINE SETGAP(GAPEXT,TGAPL,TEXTL)
C     SUBROUTINE TEXBOR(X4,Y4,BLWD,BLHT,X1,Y1)
C     SUBROUTINE TEXREC(RCDCNT)
C     SUBROUTINE TRALRC(RCDCNT,BLWD,BLHT)
C     SUBROUTINE TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C     SUBROUTINE WITREC(SUPLEF,SUPRIT,RCDCNT)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ANSDIM(NX1,NY1,NX2,NY2,NX3,NY3,BLWD,BLHT,X4,Y4,ENT
     +                                                    ,PDAT,OK)
C     =============================================================
C1    vartype           R   R   R   R   R    R    R  R  I2  R    L
C1    iostat            I   I   I   I   I    I    I  I  I   O    O
C
C2    Subroutine ANSDIM calculates the intersects of a dimension text
C2    box with a line or arc.
C2    NX1,NY1 is Line start coord , Arc centre
C2    NX2,NY2 is Line end coord , Arc start point
C2    NX3,NY3 is Arc end point
C2    Blwd & Blht is text block sizes and X4,Y4 the BL origin.
C2    Ent decides if Arc or Line.
C2    PDAT is intersect coords.
C
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
C
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,X4,Y4,U1,U2,NX1,NY1,X11(2),
     +     NX2,NY2,CPX,CPY,BLWD,BLHT,TX4,TY4,DIMENX,DIMENY,PDAT(2,2),
     1     Y11(2),X,Y,NX3,NY3,RADIUS,DISTXY,TX,TY
      INTEGER*2 K,I,ENT
      INTEGER*4 NLEN1
      LOGICAL LEFT,ARROIN,TXTOUT,OK,CHKLN,CLKWS,SWAPED
      EXTERNAL CHKLN,CC00P5,CHKORD,TEXBOR,CLKWS,DISTXY
C
      K=1
C     create offset box around text block,BL & TR coords in PDAT
      CALL TEXBOR(X4,Y4,BLWD,BLHT,X11,Y11)
      IF ( ENT .EQ. LDIMN ) THEN
C      calculate intersects of dimension line and each border line.
       DO 10 I=1,2
         TX=X11(I)
         CALL CC00P5(NX1,NY1,NX2,NY2,X11(I),Y11(1),TX,Y11(2),X,Y,OK)
C        calculate if intersect is within the active line length.
         IF ( OK ) THEN
          IF ( CHKLN(Y11(1),Y11(2),Y) .AND. CHKLN(X11(I),TX,X) ) THEN
C             Is within active length so store as intersect point.
              PDAT(K,1)=X
              PDAT(K,2)=Y
              K=K+1
           END IF
         END IF
         TY=Y11(I)
         CALL CC00P5(NX1,NY1,NX2,NY2,X11(1),Y11(I),X11(2),TY,X,Y,OK)
C        calculate if intersect is within the active line length.
         IF ( OK ) THEN
            IF ( CHKLN(Y11(I),TY,Y).AND.CHKLN(X11(1),X11(2),X) ) THEN
C              Is within active length so store as intersect point.
               PDAT(K,1)=X
               PDAT(K,2)=Y
               K=K+1
            END IF
         END IF
 10     CONTINUE
C       Check order of the intesrects so that LH one is in PDAT(1).
        IF ( K .GT. 1 ) THEN
C          Intersect found
           CALL CHKORD(PDAT(1,1),PDAT(1,2),PDAT(2,1),PDAT(2,2),SWAPED)
           OK=.TRUE.
        ELSE
C          no intersect found.
           OK=.FALSE.
        END IF
      ELSE
C       must be angular dimension so use arc intersection points.
C       calculate arc radius
        RADIUS=DISTXY(NX1,NY1,NX2,NY2)
        DO 20 I=1,2
          TX=X11(I)
C         calculate line vector
          CALL CV0L14(X11(I),Y11(1),TX,Y11(2),L1,L2,L3)
C         calculate intersects of arc and line.
          CALL VC00P9(NX1,NY1,RADIUS,L1,L2,L3,X1,Y1,X2,Y2,OK)
C         calculate if intersect is within the active line length.
          IF ( OK ) THEN
            IF ( CHKLN(Y11(1),Y11(2),Y1).AND.CHKLN(X11(I),TX,X1))THEN
C              Is within active length so store as intersect point.
               PDAT(K,1)=X1
               PDAT(K,2)=Y1
               K=K+1
            END IF
            IF ( CHKLN(Y11(1),Y11(2),Y2).AND.CHKLN(X11(I),TX,X2))THEN
C              Is within active length so store as intersect point.
               PDAT(K,1)=X2
               PDAT(K,2)=Y2
               K=K+1
            END IF
          END IF
         TY=Y11(I)
C        calculate line vector
         CALL CV0L14(X11(1),Y11(I),X11(2),TY,L1,L2,L3)
C        calculate intersects of arc and line.
         CALL VC00P9(NX1,NY1,RADIUS,L1,L2,L3,X1,Y1,X2,Y2,OK)
C        calculate if intersect is within the active line length.
         IF ( OK ) THEN
           IF ( CHKLN(Y11(I),TY,Y1).AND.CHKLN(X11(1),X11(2),X1)) THEN
C             Is within active length so store as intersect point.
              PDAT(K,1)=X1
              PDAT(K,2)=Y1
              K=K+1
           END IF
           IF ( CHKLN(Y11(I),TY,Y2).AND.CHKLN(X11(1),X11(2),X2)) THEN
C             Is within active length so store as intersect point.
              PDAT(K,1)=X2
              PDAT(K,2)=Y2
              K=K+1
           END IF
         END IF
 20      CONTINUE
C        sort into anticlockwise order
         IF ( K .GT. 1 ) THEN
C           intersect was found
            IF ( CLKWS(NX1,NY1,PDAT(1,1),PDAT(1,2),
     +                     PDAT(2,1),PDAT(2,2)) ) THEN
C             2nd angle c/wise to first so swap arguments.
C             swap the angles and intersect points
              CALL RSWAP(PDAT(1,1),PDAT(2,1))
              CALL RSWAP(PDAT(1,2),PDAT(2,2))
            END IF
            OK=.TRUE.
         ELSE
C           no intersect found
            OK=.FALSE.
         END IF
       END IF
C      Return to calling Routine.
       END
C
C     ---------------------------------------------------------
C
      SUBROUTINE ARRREC(RCDCNT,SUPLFT,SUPRGT)
C     =======================================
C
C1    vartype            I2    L      L
C1    iostatus           I/O   I      I
C
C2    TXTREC is a  routine to set the  text records
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10)
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K
      LOGICAL SUPLFT,SUPRGT
C
C     set left arrowhead information
C     clear scratch array
      DO 703 I=1,3
         IWORK2(I)=0
 703  CONTINUE
C     number of arrow records
      IWORK2(1)=RTALLY(3)
C     Form of arrowhead used according to IGES standard.
C     i.e Unfilled closed arrow.
      IWORK2(2)=2
      IF ( SUPLFT ) THEN
c         supression of left arrowhead occured
          LCRDAT(4)=IWORK2(1)+IWORK2(2)*16+32768
      ELSE
C         no left suppression found
          LCRDAT(4)=IWORK2(1)+IWORK2(2)*16
      END IF
C     write out data to ist arrowhead integer record
      IWORK(4,RCDCNT+1)=I4I2(LCRDAT(4))
      IWORK(1,RCDCNT+1)=TERMIN
       IF (  SUPRGT ) THEN
c         supression of right arrowhead occured
          LCRDAT(4)=IWORK2(1)+IWORK2(2)*16+32768
      ELSE
C         no suppression found
          LCRDAT(4)=IWORK2(1)+IWORK2(2)*16
      END IF
C     write out data to 2nd arrowhead integer record
      IWORK(4,RCDCNT+2)=I4I2(LCRDAT(4))
      IWORK(1,RCDCNT+2)=TERMIN
      K=0
      DO 303 I=RCDCNT+1,RCDCNT+RTALLY(3)
C       set local counter
        K=K+1
C       set real data record
        DO 509 J=1,6
           RWORK(J,I)=DIMARR(J,K)
 509    CONTINUE
 303  CONTINUE
      RCDCNT=RCDCNT+RTALLY(3)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CHKARR(DIMD,RALNG,BLWD,TXTOUT,ARROIN)
C     ==============================================
C
C1    vartype           R    R    R    L      L
C1    iostatus          I    I    I    I      O
C
C2    CHKARR is a  routine to check whether arrows will
C2    fit in dimension or need forced outer.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
      REAL DIMD,RALNG,BLWD
C
      LOGICAL ARROIN,TXTOUT
C     Check to see if dimensions inner and allowed
      ARROIN=.TRUE.
      IF ( .NOT.DOPFLG(9) ) THEN
C        dimension lines inner but check if can be done
         IF ( TXTOUT ) THEN
C           text is outer so check arrow length only
            ARROIN=( RALNG*PAPTOW*2.5 .LT. DIMD )
         ELSE
C           Text must be inner so has to be considered
            ARROIN=( (BLWD*PAPTOW+RALNG*PAPTOW*2.5) .LT. DIMD )
         END IF
      END IF
C     override to force outer dimension if necessary
      IF (DOPFLG(9)) ARROIN=.FALSE.
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CHKLFT(X3,Y3,X4,Y4,P1,P2,U1,U2,LEFT)
C     ===============================================
C
C1    vartype           R  R  R  R  R  R  R  R  L
C1    iostatus          I  I  I  I  I  I  I  I  O
C
C2    TXTREC is a  routine to check the position of
C2    either hit point X3,Y3/X4,Y4 against point P1,P2
C2    and returns with left .TRUE. if valid.
C2    X3,Y3/X4,Y4 are selected according to whether asked
C2    for or forced outer.
C
      include 'include/dimendat.inc'
C
      REAL X3,Y3,X4,Y4,P1,P2,U1,U2,RW1,RW2
C
      LOGICAL LEFT
C     check the hit point to use.
      IF ( DOPFLG(12) ) THEN
C          was asked for so use hit point X4,Y4
           RW1=X4
           RW2=Y4
      ELSE
C          was forced so use hit point X3,Y3
           RW1=X3
           RW2=Y3
      END IF
C     now check whether to left of point.
      IF (U2.EQ.0) THEN
C          dimension is vertical
          LEFT=( .NOT. (RW2.GT.P2) )
      ELSE
C          dimension not vertical
          LEFT=( .NOT. (RW1.GT.P1) )
      END IF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CHKORD(X1,Y1,X2,Y2,SWAPED)
C     ====================================
C
C1    vartype           R  R  R  R  L
C1    iostatus          I  I  I  I  O
C
C2    CHKORD is a general routine to order the  4 real variables
C2    X1-X4 in ascending order of X then Y.SWAPED indicates a change
C2    of order was required.
C2    This is for use by the Dimension module.
C
C
      REAL X1,Y1,X2,Y2
      LOGICAL SWAPED
      EXTERNAL RSWAP
C
      SWAPED=.FALSE.
      IF ((X1.GT.X2).OR.(ABS(X1-X2)
     +     .LT.1E-5).AND.(Y1.GT.Y2)) THEN
C        swap coordinate positions
         CALL RSWAP(X1,X2)
         CALL RSWAP(Y1,Y2)
         SWAPED=.TRUE.
      END IF
 
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMF33(X1,Y1,X2,Y2,X3,Y3,X4,Y4)
C     =========================================
C1                      R  R  R  R  R  R  R  R
C1                      O  O  O  O  O  O  O  O
C1    X1Y1=1st Dimension Control point
C1    X2Y2=2nd Dimension Control point
C1    X3Y3=Dimension Line centre
C1    X4Y4=Text origin point
C2    Sub DIMF33 analyses the control data held in Common array
C2    IWORK & IMBUFF. This decodes the parameters stored with a
C2    linear dimension and sets the following flags and variables :
C2
C2         DOPFLG(1-20) True or False depending on control data
C2
C2         RTALLY(1-10) Which holds the number of each type of
C2                      dimension element e.g. Left Witness line.
C
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/entity.inc'
      include   'include/tmpdat.inc'
C
      INTEGER*4 RCDCNT,I
      REAL DX,DY,X1,Y1,X2,Y2,X3,Y3,X4,Y4,BLWD,BLHT,TANG,
     +     GAPEXT(3,4),RAD,DM(2,2)
      DOUBLE PRECISION DXP,DYP
C
C     Mip held data
      CALL DIMORG()
C     next First record of dimension storage
      CALL DMFSRC(RCDCNT,LDIMN,GAPEXT)
C     next record number of text strings
      CALL DMTXRC(RCDCNT,LDIMN)
C     next record number of arrowheads
      CALL DMARRC(RCDCNT,DM)
C     next record left dimension/witness/projection/lines
      CALL DMSGRC(RCDCNT,LDIMN,GAPEXT)
C     next record original data
      CALL DMTRRC(RCDCNT,BLWD,BLHT)
C     set the line definition points.
      X1=RWORK(1,1)
      Y1=RWORK(2,1)
      X2=RWORK(4,1)
      Y2=RWORK(5,1)
C     Set arrow point for GAPL,EXTL regen.
      I=RTALLY(1)+RTALLY(2)+1
      GAPEXT(1,2)=RWORK(1,I)
      GAPEXT(2,2)=RWORK(2,I)
C     set the dimension line referencre point
      IF ( DOPFLG(12)) THEN
C        outer text
C        must be left so use lH arrow point
         I=RTALLY(1)+RTALLY(2)+RTALLY(3)+2
         X3=RWORK(1,I)
         Y3=RWORK(2,I)
C        must offset by text width to get true origin
         X4=RWORK(1,RTALLY(1)+1)
         Y4=RWORK(2,RTALLY(1)+1)
         IF ( DOPFLG(16) ) THEN
            TANG=RAD(RWORK(5,RTALLY(1)+1))
            X4=X4+BLWD*PAPTOW*COS(TANG)
            Y4=Y4+BLWD*PAPTOW*SIN(TANG)
         END IF
      ELSE
C       inner text.
         X3=RWORK(1,RTALLY(1)+1)
         Y3=RWORK(2,RTALLY(1)+1)
C        Use text origin as is.
         X4=RWORK(1,RTALLY(1)+1)
         Y4=RWORK(2,RTALLY(1)+1)
      END IF

C     reset the dimension line origin using text posn
C     and nearest point on dimension line.
      CALL DCCPLP(DBLE(DM(1,1)),DBLE(DM(1,2)),DBLE(DM(2,1)),
     +            DBLE(DM(2,2)),DBLE(X4),DBLE(Y4),DXP,DYP)
C     set the dimension origin.
      X3=REAL(DXP)
      Y3=REAL(DYP)
C
C     set original record count
      RTALLY(10)=1
C     calculate the witness line Extension and Gap lengths.
      CALL SETGAP(GAPEXT,GAPL,EXTL)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMF35(DIMTYP,X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG)
C     ===========================================================
C1                      I2     R  R  R  R  R  R  R  R  R R    R
C1                      I      O  O  O  O  O  O  O  O  O O    O
C
C2    Sub DIMF35 analyses the control data held in Common array
C2    IWORK & IMBUFF. This decodes the parameters stored with a
C2    rad/diam dimension and sets the following flags and variables :
C2
C2         DOPFLG(1-20) True or False depending on control data
C2
C2         RTALLY(1-10) Which holds the number of each type of
C2                      dimension element e.g. Left Witness line.
C
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
      include   'include/entity.inc'
C
      INTEGER*2 DIMTYP
      INTEGER*4 I,RCDCNT
      REAL DX,DY,X1,Y1,X2,Y2,X3,Y3,XC,YC,R,BLWD,BLHT,SANG,EANG,
     +     TANG,RAD,GAPEXT(3,4),DIMLIN(2,2)
      EXTERNAL CLRDOP
c     clear the logical flags.
      CALL CLRDOP()
C     First MIP held data
      CALL DIMORG()
C     next First record of dimension storage
      CALL DMFSRC(RCDCNT,RDIMN,GAPEXT)
C     next record number of text strings
      CALL DMTXRC(RCDCNT,RDIMN)
C     next record number of arrowheads
      CALL DMARRC(RCDCNT,DIMLIN)
C     next record left dimension/witness/projection/lines
      CALL DMSGRC(RCDCNT,RDIMN,GAPEXT)
C     next record original data
      CALL DMTRRC(RCDCNT,BLWD,BLHT)
C     set the position parameters properly
      IF ( DIMTYP.NE.GLABEL)  THEN
C        set the arc definition data
         XC=RWORK(1,1)
         YC=RWORK(2,1)
         R=RWORK(4,1)
         SANG=RWORK(5,1)
         EANG=RWORK(6,1)
      END IF
      IF ( DOPFLG(15) .OR. (DIMTYP.EQ.GLABEL) ) THEN
C        leader type so use stored left arrow  data.
         X1=RWORK(1,RTALLY(1)+RTALLY(2)+1)
         Y1=RWORK(2,RTALLY(1)+RTALLY(2)+1)
         IF ( DOPFLG(15) ) THEN
C           use dimension record for knee bend and leader end points.
            X2=RWORK(1,RTALLY(1)+RTALLY(2)+RTALLY(3)+2)
            Y2=RWORK(2,RTALLY(1)+RTALLY(2)+RTALLY(3)+2)
            X3=RWORK(4,RTALLY(1)+RTALLY(2)+RTALLY(3)+2)
            Y3=RWORK(5,RTALLY(1)+RTALLY(2)+RTALLY(3)+2)
         ELSE
C           use header record for knee bend and leader end points.
            X2=RWORK(1,1)
            Y2=RWORK(2,1)
            X3=RWORK(4,1)
            Y3=RWORK(5,1)
         END IF
      ELSE
C        normal dimension type.
C        set text origin.
         IF ( DOPFLG(11) ) THEN 
C	    offset text so use the original hitpoint and not 
C           the transformed version or the text will wander
C           when CHANGING tolerance or MOVING
            X3=RWORK(4,RTALLY(1)+RTALLY(2)+RTALLY(3)+1)
            Y3=RWORK(5,RTALLY(1)+RTALLY(2)+RTALLY(3)+1)
         ELSE
            X3=RWORK(1,RTALLY(1)+1)
            Y3=RWORK(2,RTALLY(1)+1)
         ENDIF
         IF ( DOPFLG(16) ) THEN
C           left side dimension so use left arrow for org point.
            X1=RWORK(1,RTALLY(1)+RTALLY(2)+1)
            Y1=RWORK(2,RTALLY(1)+RTALLY(2)+1)
	    IF (.NOT. DOPFLG(11)) THEN 
C              only if NOT offset text.
C              must offset by block width to set origin.
               TANG=RAD(RWORK(5,RTALLY(1)+1))
               X3=X3+BLWD*PAPTOW*COS(TANG)
               Y3=Y3+BLWD*PAPTOW*SIN(TANG)
            ENDIF
         ELSE
C           Right side dimension so use right arrow for org point.
            X1=RWORK(1,RTALLY(1)+RTALLY(2)+2)
            Y1=RWORK(2,RTALLY(1)+RTALLY(2)+2)
         END IF
      END IF
C     reset any flags which could cause false supression
      IF ( DOPFLG(15).OR.(DIMTYP.EQ.GLABEL).OR.(SPCSYM .EQ.2) ) THEN
        DOPFLG(7)=.FALSE.
        DOPFLG(8)=.FALSE.
        DOPFLG(17)=.FALSE.
        DOPFLG(18)=.FALSE.
      END IF
C     set no. of original records.
      RTALLY(10)=1
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIML03(X1,Y1,X2,Y2,X3,Y3,X4,Y4,REGEN,M,OK)
C     ====================================================
C1                      R  R  R  R  R  R  R  R  L     R L
C1                      I  I  I  I  I  I  I  I  I     I O
C2    Subroutine DIML03 generates a dimension using
C2    minimum of three hits defining the dimension to be
C2    constructed.The hits required are in X1Y1,X2Y2,X3Y3,X4Y4.
C2    X1Y1,X2Y2 are the original control points , X3Y3 are the
C2    point for the dimension line to pass through and X4Y4 are
C2    outer text origin , if outer text was requested.
C2    Logical Flags for Tolerance type, Int Standard, Text and Dim
C2    position,upper & lower tolerance values , supression states
C2    etc are retreived from common block, Dimendat.Inc.
C2
C2    N.B. This is intended as general dimension construct
C2         routine to be used by Change,Move etc.
C2         By using Hit points passed through arg list and logical
C2         flags set in Common , new dimension can be constructed.
C
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
      include   'include/entity.inc'
CC
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,U1,U2,TGAPL
     +     ,NX1,NY1,NX2,NY2,WX1,WY1,WX2,WY2,WX3,WY3,WX4,WY4
     1     ,CPX,CPY,DISTXY,ABS,STRVAL(4,10),BLWD,BLHT,TAGL,MAT(3,3)
     2     ,RANG,CANG,DEG,DIMD,TWX1,TWY1,PI,TX1,TY1,TX2,TY2,M(3,3)
     3     ,DEG90,TX3,TY3,RDAT(2,2),NX3,NY3
      INTEGER*2 NCHAR,NLEN2,JST,P,I4I2,RCDCNT,J,K
      INTEGER*4 NLEN1,NSTR,I,TSPSYM,SEGTYP,C
      CHARACTER DIMNTX(10)*80
      LOGICAL SAME,LEFT,ARROIN,TXTOUT,OK,REGEN,TMPLOG,SWAPED,CHKLN,OK1
      LOGICAL LINDIM
C
      EXTERNAL CLRTAL,SAME,DEG,GETANS
C



C     Set any neccesary parameters
      DEG90=90.0
      RTMP=2.5*ALNG*PAPTOW
      LINDIM=.TRUE.
 10   CONTINUE
C     reset all record counter flags to zero
      CALL CLRTAL()
      TX1=X1
      TY1=Y1
      TX2=X2
      TY2=Y2
C     test for direction locks on the dimension
      IF (DOPFLG(3)) THEN
C        horizontal lock set
C        create horizontal vector
         TY2=TY1
      ELSE IF (DOPFLG(4)) THEN
C        vertical lock set
C        create vertical vector
         TX2=TX1
      END IF
C     sort the reference points into ascending order of X or Y
      CALL CHKORD(TX1,TY1,TX2,TY2,SWAPED)
      IF ( SWAPED ) THEN
C        had to swap so swap original points
         CALL RSWAP(X1,X2)
         CALL RSWAP(Y1,Y2)
      END IF
C     test for zero length dimension,reject if necessary
      IF ( SAME(TX1,TX2).AND. SAME(TY1,TY2) ) THEN
C        Check to see if a regenerated dimension.
         IF ( REGEN ) THEN
C           check for condition found.
            IF ( SAME(DEG90,DEG(ACOS(M(1,1)))) ) THEN
C              90 Degrees rotation so flip logicals
               TMPLOG=DOPFLG(3)
               DOPFLG(3)=DOPFLG(4)
               DOPFLG(4)=TMPLOG
               GOTO 10
            ELSE
C              rotation has caused a Zero dimension but Continue.
               CALL DEPRNT(100)
C              set points for use.
               NX1=X3
               NX2=X3
               CPX=X3
               NY1=Y3
               NY2=Y3
               CPY=Y3
               IF ( DOPFLG(4) ) THEN
C                 Vertical line requested so set accordingly
                  NY1=TY1
                  NY2=TY1
                  CPY=TY1
                  CALL CV00L4(TX1,TY1,TX1,TY1+10.0,X3,Y3,L1,L2,L3)
                  CALL UCU001(NX1,NY1,NX1,NY1+10.0,U1,U2)
               ELSE
C                 Horizontal line requested so set accordingly
                  NX1=TX1
                  NX2=TX1
                  CPX=TX1
                  CALL CV00L4(TX1,TY1,TX1+10.0,TY1,X3,Y3,L1,L2,L3)
                  CALL UCU001(NX1,NY1,NX1+10.0,NY1,U1,U2)
               END IF
               DIMD=0.0
               GOTO 100
            END IF
         ELSE
C            at original creation stage so can abort
             CALL DEPRNT(100)
             OK=.FALSE.
             RETURN
         END IF
      END IF
C     find the vector through X3,Y3 which is parallel
C     to TX1,TY1,TX2,TY2
      CALL CV00L4(TX1,TY1,TX2,TY2,X3,Y3,L1,L2,L3)
C     Find the endpoints of the dimension line passing through
C     X3,Y3 parallel to Line L1,L2,L3.
      CALL VC00L1(L1,L2,L3,TX1,TY1,TX2,TY2,X3,Y3,NX1,NY1,NX2,NY2)
C     find mid-point of dim line
      CALL CC00P3(NX1,NY1,NX2,NY2,CPX,CPY)
C     find unit vector of  dim line
      CALL UCU001(NX1,NY1,NX2,NY2,U1,U2)
C     find dimension distance
      DIMD=DISTXY(NX1,NY1,NX2,NY2)
C3    call text generation routine,origin text all at bottom left
 100  CONTINUE
      JST=1
      CALL DMNTXT(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
C     create witness line detail Cos indepndant of text/dim line
      CALL DIMWL4(X1,Y1,NX1,NY1,GAPL*PAPTOW,EXTL*PAPTOW,
     +                                               WX1,WY1,WX2,WY2)
C     find the 2nd witness line .
      CALL DIMWL4(X2,Y2,NX2,NY2,GAPL*PAPTOW,EXTL*PAPTOW,
     +                                               WX3,WY3,WX4,WY4)
C     Check text can fit inside the dimension lines
      IF ( ANSI ) THEN
         CALL ANSDIM(WX1,WY1,WX2,WY2,NX3,NY3,BLWD,BLHT,CPX,CPY,LDIMN
     +                                                    ,RDAT,OK1)
         CALL ANSDIM(WX3,WY3,WX4,WY4,NX3,NY3,BLWD,BLHT,CPX,CPY,LDIMN
     +                                                     ,RDAT,OK)
         TXTOUT=( DOPFLG(12) .OR. ( OK ) .OR. (  OK1 ) )
      ELSE
         CALL DMTXCK(DIMD,BLWD,BLHT,TXTOUT)
      END IF
      IF (  TXTOUT ) THEN
C       check to see if Aligned and so dimension text needs placed
        IF ( DOPFLG(5) ) THEN
          IF ( .NOT. REGEN ) THEN
C             aligned text so will have to check for posn
              CALL DCPRNT(101)
              CALL GETANS(C,X4,Y4)
              TX3=X4
              TY3=Y4
C             set a flag to indicate side if outer text found
              CALL CHKLFT(TX3,TY3,X4,Y4,CPX,CPY,U1,U2,LEFT)
          END IF
        ELSE
C         set a Tflag to indicate side if outer text found
          CALL CHKLFT(X3,Y3,X4,Y4,CPX,CPY,U1,U2,LEFT)
        END IF
C       If text is found to be outer but was forced then must reset
C       X4 and Y4 accordingly.Also check for hit within length of
C       dimrnsion line
        IF ( .NOT. DOPFLG(12) .OR. CHKLN(NX1,NX2,X4) .OR.
     +       ( U2 .EQ. 0 .AND. CHKLN(NX1,NX2,X4) )) THEN
              IF ( LEFT ) THEN
C                 set text position relative to left arrow apex
                  CALL UC00P4(NX1,NY1,-RTMP,U1,U2,X4,Y4)
                  IF ( ( ANSI ) .AND. ( U1 .NE. 0 ) ) X4=X4-RTMP
              ELSE
C                 set text position relative to right arrow apex
                  CALL UC00P4(NX2,NY2,RTMP,U1,U2,X4,Y4)
                  IF ( ( ANSI ) .AND. ( U1 .NE. 0 ) ) X4=X4+RTMP
              END IF
         END IF
      END IF
C     Check to see if dimensions inner and allowed
      CALL CHKARR(DIMD,ALNG,BLWD,TXTOUT,ARROIN)
C     set text angle to be written
      IF ( ANSI ) THEN
C         must always be horizontal
          RANG=0.0
          TAGL=0.0
      ELSE
C         might be at an angle
          RANG=CANG(NX1,NY1,NX2,NY2)
          TAGL=DEG(RANG)
      END IF
C     set dim lines, first set Control data & witness lines.
      CALL DMWREC(X1,Y1,0.0,X2,Y2,0.0,RTALLY(1),DIMCON,OK)
      CALL DMWREC(WX1,WY1,0.0,WX2,WY2,0.0,RTALLY(6),DIMWTL,OK)
      CALL DMWREC(WX3,WY3,0.0,WX4,WY4,0.0,RTALLY(7),DIMWTR,OK)
C     call the single dimension generatiuon routine.

      CALL DIML04(NX1,NY1,NX2,NY2,CPX,CPY,U1,U2,L1,L2,L3,BLWD,
     +                          BLHT,LEFT,TXTOUT,ARROIN,X4,Y4,LINDIM)
C     If DIN then draw extension between external dim lines
      IF ( DIN ) THEN
         CALL DMWREC(NX1,NY1,0.0,NX2,NY2,0.0,RTALLY(8),DIMEXL,OK)
      END IF
C     set the text data.
      CALL TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C
C     set control data for storage purposes
C
C     Check projection line status for Dimension draw routine
      IF ( DOPFLG(17) .OR. DOPFLG(18) ) THEN
C        must be a dimension  Line surpressed so check proj line.
         IF ( LEFT .AND. DOPFLG(17) ) THEN
C           proj line on same side as supressed dimension line
            DOPFLG(20)=.TRUE.
         ELSE IF ( .NOT.LEFT .AND. DOPFLG(18) ) THEN
C           proj line on same side as supressed dimension line
            DOPFLG(20)=.TRUE.
         END IF
      END IF
      IF ( LEFT ) THEN
C        Set flag indicating Whether outer text left of dimension
         DOPFLG(16)=.TRUE.
      END IF
C     first MIP held data


      CALL ORGDAT()
C     set the header record count
      RCDCNT=1
C     set segment type to show linear dimension & projection lines
      SEGTYP=0
C     next First record of dimension storage
      CALL FRSTRC(RCDCNT,(.NOT.DOPFLG(9)),DOPFLG(12),LDIMN)
C     next record number of text strings
      CALL TEXREC(RCDCNT)
C     set left arrowhead information
      CALL ARRREC(RCDCNT,DOPFLG(17),DOPFLG(18))
C     record  Dimension lines
      CALL DIMREC(RCDCNT,SEGTYP,DOPFLG(17),DOPFLG(18))
C     next  Witness lines
      CALL WITREC(DOPFLG(8),DOPFLG(7),RCDCNT)
C     next  projection lines,call with 0 for linear projection lines.
      CALL PRJREC(RCDCNT,SEGTYP,LEFT)
C     Finally write out the trailer record.
      CALL TRALRC(RCDCNT,BLWD,BLHT)
 
C     That's all for now
      OK=.TRUE.
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIML04(NX1,NY1,NX2,NY2,CPX,CPY,U1,U2,L1,L2,L3,BLWD,
     +                                BLHT,LEFT,TXTOUT,ARROIN,X4,Y4,
     1                                  LINDIM)
C     ==============================================================
C1    vartype           R   R   R   R   R   R   R  R  R  R  R  R
C1                                    R    L    L      L      R  R
C1                                       L
C1    iostat            I   I   I   I   I   I   I  I  I  I  I  I
C1                                    I    I    I      I      I/O I/O
C1                                       I
C
      include   'include/dimendat.inc'
      include   'include/entity.inc'
      include   'include/ndata.inc'
C
      REAL L1,L2,L3,X1,Y1,X2,Y2,X3,Y3,X4,Y4,U1,U2,NX1,NY1,
     +     NX2,NY2,CPX,CPY,BLWD,BLHT,TX4,TY4,DIMENX,DIMENY,PDAT(2,2),
     1     NDIMNX,NDIMNY,T1,T2
      INTEGER*2 K
      INTEGER*4 Q,QUADA
      LOGICAL LEFT,ARROIN,TXTOUT,OK,CHKLN,LINDIM
      EXTERNAL CHKLN
C
      IF ( ARROIN ) THEN
C       arrowheads are inside
C       First set arrowhead info
        CALL DMWREC(NX1,NY1,-U1,-U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
        CALL DMWREC(NX2,NY2,U1,U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
        IF ( .NOT.TXTOUT ) THEN
C         Inner arrows and inner text.
          IF ( ANSI ) THEN
C           must break dimension lines by text.
            IF ( DOPFLG(11)) THEN
C              offset text requested
               CALL VC0PLP(L1,L2,L3,X4,Y4,TX4,TY4)
               IF ( CHKLN(NX1,NX2,TX4).AND.CHKLN(NY1,NY2,TY4)) THEN
C                 is actually on line so use
                  X4=TX4-(BLWD/2)*PAPTOW
                  Y4=TY4
               END IF
            ELSE
C              use dimension centre as offset point.
               X4=CPX-(BLWD/2)*PAPTOW
               IF ( TOLTYP .EQ. 6 ) THEN
C                 overwritten text so could be long string
                  Y4=CPY-(DTHGT/2)*PAPTOW
               ELSE
                  Y4=CPY-(BLHT/2)*PAPTOW
               END IF
            END IF
            CALL ANSDIM(NX1,NY1,NX2,NY2,T1,T2,BLWD,BLHT,X4,Y4,
     +                                                LDIMN,PDAT,OK)
            CALL DMWREC(NX1,NY1,0.0,PDAT(1,1),PDAT(1,2),0.0,RTALLY(4)
     +                                                    ,DIMLNL,OK)
            CALL DMWREC(PDAT(2,1),PDAT(2,2),0.0,NX2,NY2,0.0,RTALLY(5)
     +                                                    ,DIMLNR,OK)
          ELSE
C           text parallel to dimension lines.
            CALL DMWREC(NX1,NY1,0.0,CPX,CPY,0.0,RTALLY(4),DIMLNL,OK)
            CALL DMWREC(CPX,CPY,0.0,NX2,NY2,0.0,RTALLY(5),DIMLNR,OK)
            IF ( DOPFLG(11)) THEN
C              offset text requested
               IF (LINDIM) THEN
C
                  CALL VC0PLP(L1,L2,L3,X4,Y4,TX4,TY4)
C
                  IF(.NOT.(CHKLN(NX1,NX2,TX4)
     +            .AND.CHKLN(NY1,NY2,TY4))) THEN
                      CALL UC00P4(CPX,CPY,-BLWD*PAPTOW/2,U1,U2,TX4,TY4)
                  END IF
C
               ELSE
                  CALL UC00P4(CPX,CPY,-BLWD*PAPTOW/2,U1,U2,TX4,TY4)
               END IF
            ELSE
C              offset from centre point of dimension
               CALL UC00P4(CPX,CPY,-BLWD*PAPTOW/2,U1,U2,TX4,TY4)
            END IF
            CALL UC00P4(TX4,TY4,DTOFF*PAPTOW,-U2,U1,X4,Y4)
          END IF
        ELSE
C         inner arrows & outer text position so projection needed.
C         write out dimension end points.
          CALL DMWREC(NX1,NY1,0.0,CPX,CPY,0.0,RTALLY(4),DIMLNL,OK)
          CALL DMWREC(CPX,CPY,0.0,NX2,NY2,0.0,RTALLY(5),DIMLNR,OK)
          IF ( ANSI ) THEN
            IF ( U1.EQ.0 ) THEN
C             Dimension is horizontal so one proj line only
              IF ( LEFT ) THEN
                CALL DMWREC(X4,NY1,0.0,NX1,NY1,0.0,RTALLY(8),
     +                                             DIMEXL,OK)
C                CALL DMWREC(NX2,NY2,0.0,NX2+RTMP,NY1,0.0,RTALLY(9)
C     +                                                 ,DIMEXR,OK)
              ELSE
                CALL DMWREC(NX2,NY2,0.0,X4,NY2,0.0,RTALLY(9),
     +                                             DIMEXR,OK)
C                CALL DMWREC(NX1-RTMP,NY1,0.0,NX1,NY1,0.0,RTALLY(8)
C     +                                                 ,DIMEXL,OK)
              END IF
              Y4=NY1
            ELSE
C             must have a knee bent projection.
C             take nearest point on line to hit
              CALL VC00P6(L1,L2,L3,X4,Y4,DIMENX,DIMENY)
              IF ( LEFT ) THEN
C               text to left of dimension.
                CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(8)
     +                                                   ,DIMEXL,OK)
              ELSE
C               text to right of dimension.
                CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(9)
     +                                                   ,DIMEXR,OK)
              END IF
              IF ( DIMENX .GT. X4 ) THEN
C               text to left of dimension horiz leg
                CALL DMWREC(X4,DIMENY,0.0,DIMENX,DIMENY,0.0,RTALLY(8)
     +                                                     ,DIMEXL,OK)
                LEFT=.TRUE.
              ELSE
C               text to right of dimension horiz leg
                CALL DMWREC(DIMENX,DIMENY,0.0,X4,DIMENY,0.0,RTALLY(8)
     +                                                     ,DIMEXL,OK)
                LEFT=.FALSE.
              END IF
              Y4=DIMENY
            END IF
            IF ( LEFT ) THEN
C             set text position as offset from the projection end
              X4=X4-(BLWD+DTOFF)*PAPTOW
            ELSE
              X4=X4+DTOFF*PAPTOW
            END IF
            Y4=Y4-(BLHT/2)*PAPTOW
          ELSE
C           text parallel to dimension and straight projection.
C           take nearest point on line to hit
            CALL VC00P6(L1,L2,L3,X4,Y4,DIMENX,DIMENY)
            IF ( LEFT ) THEN
C             text to left of dimension.
              CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(8)
     +                                                 ,DIMEXL,OK)
              CALL  UC00P4(DIMENX,DIMENY,-(BLWD+DTOFF)*PAPTOW,
     +                                               U1,U2,X4,Y4)
            ELSE
C             text to right of dimension.
              CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(9)
     +                                                 ,DIMEXR,OK)
              CALL  UC00P4(DIMENX,DIMENY,DTOFF*PAPTOW,
     +                                               U1,U2,X4,Y4)
C
C             WRITE(10,*)'[DIML04] DIMENX,DIMENX',DIMENX,DIMENY
C             WRITE(10,*)'[DIML04] U1,U2,X4,Y4',U1,U2,X4,Y4
            END IF
          END IF
        END IF
      ELSE
C       arrowheads are outer.
C       set arrowhead info
        CALL DMWREC(NX1,NY1,U1,U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
        CALL DMWREC(NX2,NY2,-U1,-U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
        IF ( .NOT.TXTOUT ) THEN
C         outer arrows & inner text
          IF ( ANSI ) THEN
C           set the left side  dimension line
            CALL UC00P4(NX1,NY1,-RTMP,U1,U2,DIMENX,DIMENY)
C           set the new dim line end points
            CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                              ,DIMLNL,OK)
C           set the right side  dimension line
            CALL UC00P4(NX2,NY2,RTMP,U1,U2,DIMENX,DIMENY)
C           set the new dim line end points
            CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(5)
     +                                              ,DIMLNR,OK)
C           set text origin along dim line
            X4=CPX-(BLWD/2)*PAPTOW
            Y4=CPY-(BLHT/2)*PAPTOW
          ELSE
C           parallel text and standard arrow.
C           set the left side  dimension line
            CALL UC00P4(NX1,NY1,-RTMP,U1,U2,DIMENX,DIMENY)
C           set the new dim line end points
            CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                              ,DIMLNL,OK)
C           set the right side  dimension line
            CALL UC00P4(NX2,NY2,RTMP,U1,U2,DIMENX,DIMENY)
C           set the new dim line end points
            CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(5)
     +                                              ,DIMLNR,OK)
C           set text origin along dim line
            CALL UC00P4(CPX,CPY,-BLWD*PAPTOW/2,U1,U2,TX4,TY4)
            CALL UC00P4(TX4,TY4,DTOFF*PAPTOW,-U2,U1,X4,Y4)
          END IF
        ELSE
C         outer arrows & outer text position
          IF ( ANSI ) THEN
C           must have a knee bent projection.
C           horizontal text and standard arrow.
            IF ( U1.EQ.0 ) THEN
C             Dimension is horizontal so one proj line only
              IF ( LEFT ) THEN
                CALL DMWREC(X4,NY1,0.0,NX1,NY1,0.0,RTALLY(4),
     +                                             DIMLNL,OK)
                CALL DMWREC(NX2,NY2,0.0,NX2+RTMP,NY1,0.0,RTALLY(5)
     +                                                 ,DIMLNR,OK)
              ELSE
                CALL DMWREC(NX2,NY2,0.0,X4,NY2,0.0,RTALLY(5),
     +                                             DIMLNR,OK)
                CALL DMWREC(NX1-RTMP,NY1,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                                 ,DIMLNL,OK)
              END IF
              Y4=NY1
            ELSE
C             must have a knee bent projection.
C             take nearest point on line to hit
              CALL VC00P6(L1,L2,L3,X4,Y4,DIMENX,DIMENY)
              IF ( LEFT ) THEN
C               text to left of dimension.
                CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                                   ,DIMLNL,OK)
C               set the right side  dimension line
                CALL UC00P4(NX2,NY2,RTMP,U1,U2,NDIMNX,NDIMNY)
C               set the new dim line end points
                CALL DMWREC(NX2,NY2,0.0,NDIMNX,NDIMNY,0.0,RTALLY(5)
     +                                                ,DIMLNR,OK)
              ELSE
C               text to right of dimension.
                CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(5)
     +                                                   ,DIMLNR,OK)
C               set the left side  dimension line
                CALL UC00P4(NX1,NY1,-RTMP,U1,U2,NDIMNX,NDIMNY)
C               set the new dim line end points
                CALL DMWREC(NDIMNX,NDIMNY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                                  ,DIMLNL,OK)
              END IF
              IF ( DIMENX .GT. X4 ) THEN
C               text to left of dimension horiz leg
                CALL DMWREC(X4,DIMENY,0.0,DIMENX,DIMENY,0.0,RTALLY(4)
     +                                                    ,DIMLNL,OK)
              ELSE
C               text to right of dimension horiz leg
                CALL DMWREC(DIMENX,DIMENY,0.0,X4,DIMENY,0.0,RTALLY(4)
     +                                                    ,DIMLNL,OK)
              END IF
              Y4=DIMENY
            END IF
            IF ( LEFT ) THEN
C             set text position as offset from the projection end
              X4=X4-(BLWD+DTOFF)*PAPTOW
            ELSE
              X4=X4+DTOFF*PAPTOW
            END IF
            Y4=Y4-(BLHT/2)*PAPTOW
          ELSE
C           text parallel to dimension and straight projection.
C           take nearest point on line to hit
            CALL VC00P6(L1,L2,L3,X4,Y4,DIMENX,DIMENY)
            IF ( LEFT ) THEN
C             text to left of dimension.
              CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                                 ,DIMLNL,OK)
              CALL  UC00P4(DIMENX,DIMENY,-(BLWD+DTOFF)*PAPTOW,
     +                                                U1,U2,X4,Y4)
C             set the right side  dimension line
              CALL UC00P4(NX2,NY2,RTMP,U1,U2,DIMENX,DIMENY)
C             set the new dim line end points
              CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(5)
     +                                                ,DIMLNR,OK)
            ELSE
C             text to right of dimension.
              CALL DMWREC(NX2,NY2,0.0,DIMENX,DIMENY,0.0,RTALLY(5)
     +                                                 ,DIMLNR,OK)
              CALL  UC00P4(DIMENX,DIMENY,DTOFF*PAPTOW,
     +                                               U1,U2,X4,Y4)
C             set the left side  dimension line
              CALL UC00P4(NX1,NY1,-RTMP,U1,U2,DIMENX,DIMENY)
C             set the new dim line end points
              CALL DMWREC(DIMENX,DIMENY,0.0,NX1,NY1,0.0,RTALLY(4)
     +                                                ,DIMLNL,OK)
            END IF
          END IF
        END IF
      END IF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMLDR(X1,Y1,X2,Y2,X3,Y3,BLWD,BLHT,U1,U2,X4,Y4)
C     =========================================================
C
C1    vartype           R  R  R  R  R  R  R    R    R  R  R   L
C1    iostatus          I  I  I  I  I  I  I    I    I  I  I   O
C
C2    Subroutine DIMLDR constructs a diametral/radial dimension leader
C2    from DOPFLAg logical flags and passed points info
C2    in X2Y2,X3Y3.
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/movdat.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/tmpdat.inc'
C
      INTEGER*4 QUADA
      REAL L1,L2,L3,X,Y,X1,Y1,X2,Y2,X3,Y3,BLWD,BLHT,
     +     X4,Y4,DIMENX,DIMENY,U1,U2,CANG,ANG
      LOGICAL OK
      EXTERNAL CV0L14,VC00P9
C
C        Check to see if single line or knee bend needed
         IF ( DIN ) THEN
C           must point toward reference point
            ANG=CANG(X1,Y1,X2,Y2)
            IF ( QUADA(ANG) .EQ. 3 .OR. QUADA(ANG) .EQ. 4 ) THEN
               CALL UCU001(X2,Y2,X1,Y1,U1,U2)
            ELSE
               CALL UCU001(X1,Y1,X2,Y2,U1,U2)
            END IF
C           single line needed.
            DOPFLG(16)=( .NOT. ( X2 .GT. X1 ) )
C           find the text origin and save it
            IF ( DOPFLG(16) ) THEN
C              text is to left of dimension
               CALL UC00P4(X2,Y2,-((BLWD+DTOFF)*PAPTOW),U1,
     1                                      U2,X4,Y4)
            ELSE
C              text is to right of dimension so find dist along line.
               CALL UC00P4(X2,Y2,(DTOFF*PAPTOW),U1,U2,X4,Y4)
            END IF
            CALL DMWREC(X1,Y1,0.0,X2,Y2,0.0,RTALLY(4),DIMLNL,OK)
         ELSE
C           generate horizontal vector L thru X2,Y2
            CALL CV0L14(X2,Y2,X2+10.0,Y2,L1,L2,L3)
C           take nearest point on line to hit
            CALL VC00P6(L1,L2,L3,X3,Y3,DIMENX,DIMENY)
C           find the text origin and save it
            IF ( DIMENX .GT. X2 ) THEN
C              text is to right of dimension
               X4=DIMENX+PAPTOW*DTOFF
               Y4=Y2
            ELSE
C              text is to Left of dimension
               X4=DIMENX-(BLWD+DTOFF)*PAPTOW
               Y4=Y2
               DOPFLG(16)=.TRUE.
            END IF
C           Set the text position correctly in Y axis.
            Y4=Y4-(DTHGT/2)*PAPTOW
            CALL DMWREC(X1,Y1,0.0,X2,Y2,0.0,RTALLY(4),DIMLNL,OK)
            CALL DMWREC(X2,Y2,0.0,DIMENX,DIMENY,0.0,RTALLY(4),
     +                                              DIMLNL,OK)
         END IF
C        find the unit vector of the arrow
C        must point toward reference point
         CALL UCU001(X2,Y2,X1,Y1,U1,U2)
C        store arrowhead only
         CALL DMWREC(X1,Y1,U1,U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMORG()
C     ==================
      include   'include/wrkdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
CC
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      EXTERNAL I2I4
C     examine Imbuff array to get general parameters
C     first equate to 4 byte integer
      LCRDAT(1)=I2I4(IMBUFF(5))
C     clear scratch array
      DO 700 I=1,4
         IWORK2(I)=0
 700  CONTINUE
C     0 for unrelated dimension, 1 for related dimension
      IWORK2(1)=MOD(LCRDAT(1),2)
      DOPFLG(6)=( IWORK2(1) .LT. 1 )
C
C		DHR ADDED 30/10/03
C
	  DOPFLG(6) = .FALSE.
C     0 for Unscaled dimension , 1 for scaled dimension
      IWORK2(2)=MOD(LCRDAT(1),4)/2
C      IF (  IWORK2(2) .LT. 1 ) THEN
C      END IF
C     0 for untoleranced dimension, 1 for toleranced dimension
      IWORK2(3)=MOD(LCRDAT(1),8)/4
C      IF (  IWORK2(3) .LT. 1 ) THEN
C      END IF
C     International standard used
      IWORK2(4)=MOD(LCRDAT(1),128)/8
      BS= ( IWORK2(4) .EQ. 0 )
      DIN= ( IWORK2(4) .EQ. 1)
      ANSI= ( IWORK2(4) .EQ. 2 )
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMR03(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,OK)
C     =========================================================
C
C1    vartype           R  R  R  R  R  R  R  R  R  L
C1    iostatus          I  I  I  I  I  I  I  I  I  O
C
C2    Subroutine DIMR03 constructs a diametral/radial dimension
C2    from DOPFLAg logical flags and passed points info
C2    in X1Y1,X2Y2,X3Y3.The radius of the arc is
C2    in R,centre XC,YC if successful OK returned
C2    true.
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/movdat.inc'
      include 'include/nbuff.inc'
      include 'include/dimendat.inc'
      include 'include/tmpdat.inc'
      include 'include/entity.inc'
C
      INTEGER*2 NCHAR,NLEN2,JST,P,I4I2,RCDCNT,J,K
C
      INTEGER*4 NLEN1,NSTR,I,TSPSYM,SEGTYP
C
      REAL TMPX1,TMPY1,TMPX2,TMPY2,D1,D2,L1,L2,L3,DISTXY,ABS,
     +               X,Y,X1,Y1,X2,Y2,X3,Y3,R,XC,YC,SANG,EANG,
     1               STRVAL(4,10),BLWD,BLHT,TAGL,MAT(3,3),RANG,
     2               X4,Y4,PDAT(2,4),WX1,WY1,DIMENX,DIMENY,
     3               CANG,DEG,DIMD,TWX1,TWY1,PI,NX1,NY1,NX2,NY2,
     4               U1,U2,CPX,CPY,CHKVAL
      REAL DD1,DD2,DD3
      REAL TX1,TY1
C
      LOGICAL OK,ARROIN,TXTOUT,LEFT,SUPRDL,SUPRDR,SUPLFT,SUPRGT,SWAPED
     +        ,CHKLN,LINDIM
C
      CHARACTER DIMNTX(10)*80
C
      INTRINSIC ABS,CHAR
C
      EXTERNAL CV0L14,VC00P9,DMNTXT,DEG,CANG,PI,CHKLN,DISTXY
C
 10   CONTINUE
C
      LINDIM=.FALSE.
      RTMP=2.5*ALNG*PAPTOW
      SUPRDR=.FALSE.
      SUPRDL=.FALSE.
c     reset all record counter flags to zero
      CALL CLRTAL()
C     generate vector L thru X1,Y1 and XC,YC
      CALL CV0L14(X1,Y1,XC,YC,L1,L2,L3)
C     find intersection point on arc
      CALL VC00P9(XC,YC,R,L1,L2,L3,NX1,NY1,NX2,NY2,OK)
      CALL CHKORD(NX1,NY1,NX2,NY2,SWAPED)
C     set dimension value according to Dimension type.
      IF ( SPCSYM .EQ. 1 ) THEN
C       use Diametral Value For dimension and text check value
        CHKVAL=2*R
        DIMD=2*R
      ELSE
C       use radius value for Dim value but diameter for Check value.
        CHKVAL=2*R
        DIMD=R
      END  IF
      TSPSYM=SPCSYM
C     if DIN and full arc then dont show dia. sign.
      IF ( DIN ) THEN
         IF (( SANG .LT. 0.01 ) .AND. ( EANG .GT. PI(2.)-0.01 )
     +               .AND. ( SPCSYM .EQ. 1 )) THEN
             SPCSYM=0
         END IF
      END IF
      JST=1
      CALL DMNTXT(DIMD,DIMNTX,STRVAL,BLWD,BLHT,NSTR)
c     reset the special symbol  flag
      SPCSYM=TSPSYM
      IF ( DOPFLG(15) ) THEN
C        Leader asked for
C        take nearest point on Arc to hit
C         CALL NRSVAL(X1,Y1,NX1,NY1,NX2,NY2,TMPX1,TMPY1)
         IF ( DISTXY(X1,Y1,NX1,NY1).LE.
     +        DISTXY(X1,Y1,NX2,NY2)      ) THEN
            TMPX1=NX1
            TMPY1=NY1
         ELSE
            TMPX1=NX2
            TMPY1=NY2
         END IF
         CALL VC00P6(L1,L2,L3,X2,Y2,DIMENX,DIMENY)
C        call routine to generate a leader dimension.
         CALL DIMLDR(TMPX1,TMPY1,DIMENX,DIMENY,X3,Y3,BLWD,BLHT,U1,U2
     +                                                       ,X4,Y4)
C        jump to Control data storage section.
         GOTO 199
      END IF
C     must be a full dimension type
C     find mid-point of dim line
      IF ( DOPFLG(11) ) THEN
          CALL CV0L14(NX1,NY1,NX2,NY2,DD1,DD2,DD3)
          CALL VC0PLP(DD1,DD2,DD3,X3,Y3,CPX,CPY)
      ELSE
          CALL CC00P3(NX1,NY1,NX2,NY2,CPX,CPY)
      ENDIF
C     find unit vector of  dim line
      CALL UCU001(NX1,NY1,NX2,NY2,U1,U2)
C     set logical flags For text and arrow position.
      ARROIN=.TRUE.
      TXTOUT=.FALSE.
C     Check text fits inside dimension lines,by using check value.
      CALL DMTXCK(CHKVAL,BLWD,BLHT,TXTOUT)
C     set a flag to indicate side of Text relative to centre
      CALL CHKLFT(X1,Y1,X3,Y3,CPX,CPY,U1,U2,LEFT)
      IF (  TXTOUT ) THEN
C        If text is found to be outer but was forced then must reset
C        X4 and Y4 accordingly.
         IF ( .NOT. DOPFLG(12).OR.CHKLN(NX1,NX2,X3) ) THEN
            IF ( LEFT ) THEN
C              set text position relative to left arrow apex
               RTMP=-RTMP
            END IF
            CALL UC00P4(NX1,NY1,RTMP,U1,U2,X4,Y4)
            IF ( ANSI) X4=X4+RTMP
         ELSE
C           was not forced so use user given points X3,Y3.
            X4=X3
            Y4=Y3
         END IF
      END IF
C     Check to see if dimensions inner and allowed
      CALL CHKARR(CHKVAL,ALNG,BLWD,TXTOUT,ARROIN)
C     Test to see if  radial Dimension so supress one dim line.
      IF ( SPCSYM .EQ. 2 ) THEN
         SUPRDR=LEFT
         SUPRDL=.NOT.LEFT
      END IF
C     call the single dimension generatiuon routine.
      CALL DIML04(NX1,NY1,NX2,NY2,CPX,CPY,U1,U2,L1,L2,L3,BLWD,
     +                          BLHT,LEFT,TXTOUT,ARROIN,X4,Y4,LINDIM)
      IF ( .NOT. ARROIN ) THEN
         IF ( SPCSYM .EQ. 1 ) THEN
C           If DIN then draw extension between external dim lines
            IF ( DIN ) THEN
C              Draw line between full diameter arrows.
               CALL DMWREC(NX1,NY1,0.0,NX2,NY2,0.0,
     +                          RTALLY(8),DIMEXL,OK)
            END IF
         ELSE
            IF ( LEFT ) THEN
               CALL DMWREC(NX1,NY1,0.0,CPX,CPY,0.0,
     +                         RTALLY(8),DIMEXL,OK)
            ELSE
               CALL DMWREC(CPX,CPY,0.0,NX2,NY2,0.0,
     +                         RTALLY(9),DIMEXR,OK)
            END IF
        END IF
      END IF
 199  CONTINUE
C     set dim lines, first set Control data .
      CALL DMWREC(XC,YC,0.0,R,SANG,EANG,RTALLY(1),DIMCON,OK)
C     set text angle to be written
      IF ( ANSI .OR. (DOPFLG(15).AND..NOT.DIN)  ) THEN
C         horizontal cos Ansi or leader which is not DIN
          RANG=0.0
          TAGL=0.0
      ELSE
C         angled including DIN leader type diension.
          RANG=CANG(NX1,NY1,NX2,NY2)
          TAGL=DEG(RANG)
      END IF
C     set the text data.
      CALL TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C
C     set control data for storage purposes
C     Set flag indicating Whether outer text left of dimension
      IF ( LEFT ) DOPFLG(16)=.TRUE.
C     First MIP held data
      CALL ORGDAT()
C     set the header record count
      RCDCNT=1
C     Set the dimension  segment to be a line.
      SEGTYP=0
C     next  record of dimension storage
      CALL FRSTRC(RCDCNT,ARROIN,TXTOUT,RDIMN)
C     next record number of text strings
      CALL TEXREC(RCDCNT)
C     set left arrowhead information using supression state.
      SUPLFT=((DOPFLG(17).AND..NOT.DOPFLG(15)).OR.SUPRDL)
      SUPRGT= ( DOPFLG(18).OR.SUPRDR)
      CALL ARRREC(RCDCNT,SUPLFT,SUPRGT)
C     record  Dimension lines first set dimension line supression.
      SUPLFT=((DOPFLG(17).AND..NOT.DOPFLG(15)) .OR. SUPRDL )
      SUPRGT= ( DOPFLG(18).OR.SUPRDR )
      CALL DIMREC(RCDCNT,SEGTYP,SUPLFT,SUPRGT)
C     next  projection lines,call with 1 for arc projections.
      CALL PRJREC(RCDCNT,SEGTYP,LEFT)
C     Finally write out the trailer record.
      CALL TRALRC(RCDCNT,BLWD,BLHT)
C     That's all for now
C     Reset any variable set by leder type dimension
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMREC(RCDCNT,SEGTYP,SUPLFT,SUPRGT)
C     ==============================================
C
C1    vartype            I2    I4     L      L
C1    iostatus           I/O   I      I      I
C
C2    TXTREC is a  routine to set the  text records
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10),SEGTYP
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K,LINARC
      LOGICAL SUPLFT,SUPRGT
C
C     set the segment type i.e arc or line.
      IF ( SEGTYP .EQ. 0 ) THEN
C        was a line type so set accordingly
         LINARC=LINSEG
      ELSE
C        was an Arc segment
         LINARC=ARCSEG
      END IF
C     record LEFT Dimension line
C     clear scratch array
      DO 704 I=1,4
         IWORK2(I)=0
 704  CONTINUE
C     number of segment records  following
      IWORK2(1)=RTALLY(4)+RTALLY(5)+RTALLY(6)+RTALLY(7)+
     +                               RTALLY(8)+RTALLY(9)
C     Form of line/arc  used
      IWORK2(2)=0
C     Form of segment i.e. 0 for line or 1 for arc  used
      IWORK2(3)=SEGTYP
C     set left Dimension state
      IWORK2(4)=0
      IF ( SUPLFT ) THEN
C        left dimension line supression asked for
         LCRDAT(5)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                       +IWORK2(4)*4096+32768
      ELSE
C        no dimension line supression
         LCRDAT(5)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                                   +IWORK2(4)*4096
      END IF
      K=0
      DO 304 I=RCDCNT+1,RCDCNT+RTALLY(4)
C       set local counter
        K=K+1
C       set integer control data
        IWORK(4,I)=I4I2(LCRDAT(5))
        IWORK(1,I)=LINARC
        DO 513 J=1,6
           RWORK(J,I)=DIMLNL(J,K)
 513    CONTINUE
 304  CONTINUE
      RCDCNT=RCDCNT+RTALLY(4)
C
C     record RIGHT Dimension line
C     clear scratch array
      DO 705 I=1,4
         IWORK2(I)=0
 705  CONTINUE
C
C     number of segment records  following
      IWORK2(1)=RTALLY(4)+RTALLY(5)+RTALLY(6)+RTALLY(7)+
     +                               RTALLY(8)+RTALLY(9)
C     Form of line/arc  used i.e Dimension line
      IWORK2(2)=0
C     Form of segment i.e. 0 for line or 1 for arc  used
      IWORK2(3)=SEGTYP
C     set RIGHT Dimension state
      IWORK2(4)=1
      IF ( SUPRGT ) THEN
C        right dimension line supression asked for
         LCRDAT(6)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                       +IWORK2(4)*4096+32768
      ELSE
C        no dimension line supression
         LCRDAT(6)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                                   +IWORK2(4)*4096
      END IF
      K=0
      DO 305 I=RCDCNT+1,RCDCNT+RTALLY(5)
C       set local counter
        K=K+1
C       set integer control data
        IWORK(4,I)=I4I2(LCRDAT(6))
        IWORK(1,I)=LINARC
        DO 517 J=1,6
           RWORK(J,I)=DIMLNR(J,K)
 517    CONTINUE
 305  CONTINUE
      RCDCNT=RCDCNT+RTALLY(5)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMARRC(RCDCNT,DIMLIN)
C     ================================
C2                       I/O    O
C2                       I4     R(2,2)
C2    Array DIMLIN returns the arrow apex points
C2    of current dimension.
C2
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      REAL DIMLIN(2,2)
      EXTERNAL I2I4,CLRDOP
C     next record number of arrowheads
C     examine Iwork array to get arrowhead information
C     first equate to 4 byte integer
      LCRDAT(3)=I2I4(IWORK(4,RCDCNT+1))
C     clear scratch array
      DO 703 I=1,3
         IWORK2(I)=0
 703  CONTINUE
C     number of arrow records
      RTALLY(3)=MOD(LCRDAT(3),16)
C     Form of arrowhead used
      IWORK2(2)=MOD(LCRDAT(3),256)/16
C     read the dimension arrow apex points.
      DIMLIN(1,1)=RWORK(1,RCDCNT+1)
      DIMLIN(1,2)=RWORK(2,RCDCNT+1)
      DIMLIN(2,1)=RWORK(1,RCDCNT+2)
      DIMLIN(2,2)=RWORK(2,RCDCNT+2)
C     set the record position counter
      RCDCNT=RCDCNT+RTALLY(3)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMFSRC(RCDCNT,DIMTYP,GAPEXT)
C     =======================================
C2
C2
      include   'include/wrkdat.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
      include   'include/entity.inc'
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      INTEGER*2 DIMTYP
      REAL GAPEXT(3,4)
      EXTERNAL RAD,I2I4,CLRDOP
C     next First record of dimension storage
C     examine Iwork array to get basic dim info
C     first equate to 4 byte integer
C
C     DHR - Modified LCRDAT from 1 to 2
C
	  LCRDAT(2)=I2I4(IWORK(4,1))
      IF ( DIMTYP .EQ. LDIMN ) THEN
C         set the refernce point to be used for GAPL,EXTL regen.
          GAPEXT(1,1)=RWORK(1,1)
          GAPEXT(2,1)=RWORK(2,1)
      ELSE IF ( DIMTYP .EQ. ADIMN ) THEN
          GAPEXT(1,1)=RWORK(1,2)
          GAPEXT(2,1)=RWORK(2,2)
      END IF
C     clear scratch array
      DO 701 I=1,6
         IWORK2(I)=0
 701  CONTINUE
C     text position : 0 for inner or 1 for outer
      IWORK2(1)=MOD(LCRDAT(2),2)
      DOPFLG(12)= (  IWORK2(1) .GE. 1 )
C     dimension/arrow position: 0 for inner or 1 for outer
      IWORK2(2)=MOD(LCRDAT(2),4)/2
      DOPFLG(9)= (IWORK2(2) .EQ. 1)
C     direction lock of dimension
      IWORK2(3)=MOD(LCRDAT(2),16)/4
      IF ( DIMTYP .EQ. LDIMN ) THEN
         DOPFLG(3)=( IWORK2(3) .EQ. 1 )
         DOPFLG(4)=( IWORK2(3) .EQ. 2 )
      ELSE IF ( DIMTYP .EQ. RDIMN ) THEN
         DOPFLG(15)=( IWORK2(3) .EQ. 1 )
      ELSE IF ( DIMTYP .EQ. ADIMN ) THEN
         DOPFLG(13)=( IWORK2(3) .EQ. 1 )
      END IF
C     number of text records following
      IF ( DIMTYP .EQ. ADIMN ) THEN
         RTALLY(1)=2
         RCDCNT=2
      ELSE
         RTALLY(1)=1
         RCDCNT=1
      END IF
C
C     DHR	Modified from 1 to 2
C
      RTALLY(2)=MOD(LCRDAT(2),256)/16
C     number of Arrow/ dimension/ extension records following
      IWORK2(5)=MOD(LCRDAT(2),16384)/256
C     number of original data records following
      RTALLY(10)=LCRDAT(2)/16384
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMMREC(X1,X2,X3,X4,X5,X6,TALLY,RELDAT,OK)
C     ===================================================
C
C1    vartype           R  R  R  R  R  R  R     R(6,6)  L
C1    iostatus          I  I  I  I  I  I  I     O      O
C
C2    DMMREC is a general routine to write the  6 real variables
C2    X1-X6 into the next free position in the array RELDAT.
C2    Returns OK with .TRUE. if successful.
C2    This is for use by the Dimension module.
C
C
      INTEGER TALLY
      REAL X1,X2,X3,X4,X5,X6,RELDAT(6,6)
      LOGICAL OK
C     set logical to default condition.
      OK=.FALSE.
C     Write to the correct record  of the pased real array.
      RELDAT(1,TALLY)=X1
      RELDAT(2,TALLY)=X2
      RELDAT(3,TALLY)=X3
      RELDAT(4,TALLY)=X4
      RELDAT(5,TALLY)=X5
      RELDAT(6,TALLY)=X6
C     set logical to clear condition.
      OK=.TRUE.
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMSGRC(RCDCNT,DIMTYP,GAPEXT)
C     =======================================
C2
C2
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
      include   'include/entity.inc'
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      INTEGER*2 DIMTYP
      REAL GAPEXT(3,4)
      EXTERNAL I2I4
C
C     next record left dimension/witness/projection/lines
C     examine Iwork array to get no. of segments following.
C     first equate to 4 byte integer
      LCRDAT(4)=I2I4(IWORK(4,RCDCNT+1))
C     clear scratch array
      DO 704 I=1,4
         IWORK2(I)=0
 704  CONTINUE
C     clear Record position counters
      DO 604 I=4,10
         RTALLY(I)=0
 604  CONTINUE
C     find number of segment records following
      IWORK2(1)=MOD(LCRDAT(4),64)
C     loop for the number of segments coming  and decode data.
      DO 901 I=RCDCNT+1,RCDCNT+IWORK2(1)
C         read in the integer data
          LCRDAT(4)=I2I4(IWORK(4,I))
C         Form of line/arc  used i.e dimension/witness/projection.
          IWORK2(2)=MOD(LCRDAT(4),256)/64
C         fInd whether right or left dimension
          IWORK2(3)=MOD(LCRDAT(4),8192)/4096
C         take action on whether dimension/witness/projection.
          IF ( IWORK2(2) .EQ. 0 ) THEN
C             must have been a dimension line
                 IF ( IWORK2(3) .EQ. 0 ) THEN
C                    dim must be to left so check suppress
                     DOPFLG(17)=IWORK(4,I).LT.0
C                    update left dimension counter
                     RTALLY(6)=RTALLY(6)+1
                 ELSE
C                    dim must be to Right so check suppress
                     DOPFLG(18)=IWORK(4,I).LT.0
C                    update right dimension counter
                     RTALLY(7)=RTALLY(7)+1
                 END IF
          ELSE IF ( IWORK2(2) .EQ. 1 ) THEN
C             must have been a witness line
                 IF ( IWORK2(3) .EQ. 0 ) THEN
C                    dim must be to left so check suppress
                     IF (DIMTYP.EQ.LDIMN .OR. DIMTYP.EQ.ADIMN) THEN
C                       first store the points for GAPL,EXTL regen.
                        GAPEXT(1,3)=RWORK(1,I)
                        GAPEXT(2,3)=RWORK(2,I)
                        GAPEXT(1,4)=RWORK(4,I)
                        GAPEXT(2,4)=RWORK(5,I)
                     END IF
                     DOPFLG(7)=IWORK(4,I).LT.0
C                    update left witness counter
                     RTALLY(4)=RTALLY(4)+1
                 ELSE
C                    dim must be to Right so check suppress
                     DOPFLG(8)=IWORK(4,I).LT.0
C                    update right witness counter
                     RTALLY(5)=RTALLY(5)+1
                 END IF
          ELSE
C             must have been a projection line
C             dim must be to left so check suppress
              DOPFLG(20)=IWORK(4,I).LT.0
              IF ( IWORK2(3) .EQ. 0 ) THEN
C                 update left projection counter
                  RTALLY(8)=RTALLY(8)+1
              ELSE
C                 update right projection counter
                  RTALLY(9)=RTALLY(9)+1
              END IF
          END IF
 901  CONTINUE
C     set the record position counter
      RCDCNT=RCDCNT+IWORK2(1)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMTRRC(RCDCNT,BLWD,BLHT)
C     ===================================
C2
C2
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      EXTERNAL I2I4
      REAL BLWD,BLHT
C     next record original data
C     examine Iwork array to get original data info
C     first equate to 4 byte integer
      LCRDAT(5)=I2I4(IWORK(4,RCDCNT+1))
C     clear scratch array
      DO 705 I=1,3
         IWORK2(I)=0
 705  CONTINUE
C     form of dimension tolerance used
      IWORK2(1)=MOD(LCRDAT(5),16)
      TOLTYP=IWORK2(1)+1
C     Form of special character used with dimension.
      IWORK2(2)=MOD(LCRDAT(5),256)/16
C     diametral sign used ???
      SPCSYM=IWORK2(2)
C     units used for dimension
      METRIC=( IWORK(4,RCDCNT+1) .GE. 0 )
C     reset the text block width and height etc.
      DTOFF=RWORK(1,RCDCNT+1)
      UTOL=RWORK(2,RCDCNT+1)
      LTOL=RWORK(3,RCDCNT+1)
      PREC=RWORK(4,RCDCNT+1)
      BLWD=RWORK(5,RCDCNT+1)
      BLHT=RWORK(6,RCDCNT+1)
C     reset dimension text parameters
      DTHGT=RWORK(4,RTALLY(1)+1)
      DTWDT=RWORK(3,RTALLY(1)+1)
C     reset the arrow parameters.
      ALNG=RWORK(5,RTALLY(1)+RTALLY(2)+1)
      AWDT=RWORK(6,RTALLY(1)+RTALLY(2)+1)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMTXCK(DIMLEN,BLWD,BLHT,TXTOUT)
C     =========================================
C1                      R      R    R     L
C1                      I      I    I     O
C2    Subroutine DMTXCK checks dimension text can fit
C2    inside the dimension lines.
C2     DIMLEN=Dimension value
C2     BLWD= Dimension text block width
C2     BLHT=  Dimension text block height
C2     TXTOUT= True if Text is outside dimension lines.
C
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
CC
      REAL BLWD,BLHT,DIMLEN
C
      LOGICAL TXTOUT
C
      TXTOUT=.FALSE.
      IF ( .NOT.DOPFLG(12) ) THEN
         IF ( ANSI ) THEN
           TXTOUT=.FALSE.
C           Ansi standard , special case.
         ELSE
C          was not ANSI i.e DIN,BS etc
           TXTOUT=( BLWD*PAPTOW .GT. DIMLEN )
         END IF
      END IF
C     Force outer status depending on user controiied flags.
      IF ( DOPFLG(12) ) TXTOUT=.TRUE.
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMTXRC(RCDCNT,DIMTYP)
C     ================================
C2
C2
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/tmpdat.inc'
      include   'include/entity.inc'
C
      INTEGER*4 I2I4,LCRDAT(10),I,RCDCNT,IWORK2(10)
      INTEGER*2 DIMTYP
      EXTERNAL I2I4
 
C     next record number of text strings
C     examine Iwork array to get Text information
C     first equate to 4 byte integer
      LCRDAT(2)=I2I4(IWORK(4,RCDCNT+1))
C     clear scratch array
      DO 702 I=1,2
         IWORK2(I)=0
 702  CONTINUE
C     number of text records
      IWORK2(1)=MOD(LCRDAT(2),16)
      NUMSTR=MOD(LCRDAT(2),16)
      RTALLY(2)=NUMSTR
C     Supression state of text
      DOPFLG(19)= ( IWORK(4,RCDCNT+1) .LT. 0 )
C     set text position left or right of dimension
      IWORK2(2)=MOD(LCRDAT(2),32)/16
      DOPFLG(16)= ( IWORK2(2) .EQ. 0 )
      DOPFLG(16)=.FALSE.
	  IF ( DIMTYP .EQ. LDIMN .OR. IMBUFF(2).EQ.RDIMN 
     +    .OR. IMBUFF(2).EQ.DDIMN) THEN
C        check for offset text position.
         IWORK2(4)=MOD(LCRDAT(2),64)/32
         DOPFLG(11)=( IWORK2(4) .EQ. 1 )
      END IF
C     set the record position counter
      RCDCNT=RCDCNT+RTALLY(2)
C
      END
C
C     ---------------------------------------------------------
C`
      SUBROUTINE DMWREC(X1,X2,X3,X4,X5,X6,TALLY,RELDAT,OK)
C     ===================================================
C
C1    vartype           R  R  R  R  R  R  R     R(6,10)L
C1    iostatus          I  I  I  I  I  I  I     O      O
C
C2    DIMREC is a general routine to write the  6 real variables
C2    X1-X6 into the next free position in the array RELDAT.
C2    Returns OK with .TRUE. if successful.
C2    This is for use by the Dimension module.
C
C
      INTEGER*4 TALLY
      REAL X1,X2,X3,X4,X5,X6,RELDAT(6,10)
      LOGICAL OK
C     set logical to default condition.
      OK=.FALSE.
C     increment to next record position
      TALLY=TALLY+1
C     Write to the correct record  of the pased real array.
      RELDAT(1,TALLY)=X1
      RELDAT(2,TALLY)=X2
      RELDAT(3,TALLY)=X3
      RELDAT(4,TALLY)=X4
      RELDAT(5,TALLY)=X5
      RELDAT(6,TALLY)=X6
C     set logical to clear condition.
      OK=.TRUE.
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE FRSTRC(RCDCNT,ARROIN,TXTOUT,DIMTYP)
C     ============================================
C
C1    vartype            I2    L      L      I2
C1    iostatus           I/O   I      I      I
C
C2    FRSTRC is a  routine to set the  First record
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10)
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K,DIMTYP
      LOGICAL ARROIN,TXTOUT
C
C     First record of dimension storage
C     clear scratch array
      DO 701 I=1,6
         IWORK2(I)=0
 701  CONTINUE
C     text position : 0 for inner or 1 for outer
      IF ( TXTOUT ) IWORK2(1)=1
C     dimension/arrow position: 0 for inner or 1 for outer
      IF ( .NOT.ARROIN ) IWORK2(2)=1
      IF ( DIMTYP.EQ.LDIMN ) THEN
C        set for linear dimension.
C        direction lock of dimension
         IF ( DOPFLG(3) ) THEN
C           horizontal lock used
            IWORK2(3)=1
         ELSE IF ( DOPFLG(4) ) THEN
C           vertical lock used
            IWORK2(3)=2
         END IF
      ELSE IF ( DIMTYP.EQ.RDIMN ) THEN
C        set for Radial type
         IF ( DOPFLG(15) ) THEN
C           leader type dimension
            IWORK2(3)=1
         END IF
      ELSE IF ( DIMTYP.EQ.ADIMN ) THEN
        IF ( DOPFLG(13) ) THEN
C          major angle required
           IWORK2(3)=1
        ELSE
C          minor angle  required.
           IWORK2(3)=2
        END IF
      END IF
C     number of text records following
      IWORK2(4)=RTALLY(2)
C     number of Arrow/ dimension/ extension records following
      IWORK2(5)=RTALLY(3)+RTALLY(4)+RTALLY(5)+RTALLY(6)+
     +          RTALLY(7)+RTALLY(8)+RTALLY(9)
C
C     number of original data records following
      IWORK2(6)=RTALLY(10)
      LCRDAT(2)=IWORK2(1)+IWORK2(2)*2+IWORK2(3)*4+IWORK2(4)*16+
     +                            IWORK2(5)*256+IWORK2(6)*16384
      IWORK(4,1)=I4I2(LCRDAT(2))
C
      IF ( DIMTYP .EQ. ADIMN ) THEN
C        two header records to be written
         IWORK(1,2)=HEADER
         DO 502 I=1,6
             RWORK(I,1)=DIMCON(I,1)
             RWORK(I,2)=DIMCON(I,2)
 502     CONTINUE
      ELSE
C        only one header record to be written
         DO 501 I=1,6
            RWORK(I,1)=DIMCON(I,1)
 501     CONTINUE
C
      END IF
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE ORGDAT()
C     ===================
C
C1    vartype          none.
C1    iostatus
C
C2    CLRTAL is a general routine to set the  control variaBLES
C2    stored in Master Index section of Dimension data.
C
      include 'include/dimendat.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10)
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K
C
C     Set MIP held data
C     clear scratch array
      DO 700 I=1,5
         IWORK2(I)=0
 700  CONTINUE
C     0 for unrelated dimension, 1 for related dimension
      IF ( TOLTYP .GT. 0 .AND. TOLTYP .LT. 5 ) IWORK2(1)=1
C     0 for Unscaled dimension , 1 for scaled dimension
      IWORK2(2)=0
C     0 for untoleranced dimension, 1 for toleranced dimension
      IF ( TOLTYP .GT. 1 .AND. TOLTYP .LT. 5 ) IWORK2(3)=1
C     International standard used
      IF ( DIN ) THEN
         IWORK2(4)=1
      ELSE IF  ( ANSI ) THEN
         IWORK2(4)=2
C      ELSE IF  ( JIS ) THEN
C         IWORK2(4)=3
C     Nos. 4-15 are spare
      END IF
C
      LCRDAT(1)=IWORK2(1)+IWORK2(2)*2+IWORK2(3)*4+
     +          IWORK2(4)*8
      
	  CTRDAT(1)=I4I2(LCRDAT(1))

C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE PRJREC(RCDCNT,SEGTYP,LEFT)
C     ====================================
C
C1    vartype            I2
C1    iostatus           I/O
C
C2    TXTREC is a  routine to set the  text records
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10),SEGTYP
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K,LINARC
      LOGICAL LEFT
C
C     next record projection line data
      IF ( RTALLY(9) .GT. 0 .OR. RTALLY(8) .GT. 0 ) THEN
C        must have had projection lines.
C        set the segment type i.e arc or line.
         IF ( SEGTYP .EQ. 0 ) THEN
C          was a line type so set accordingly
           LINARC=LINSEG
         ELSE
C          was an Arc segment
           LINARC=ARCSEG
         END IF
C        clear scratch array
         DO 708 I=1,4
           IWORK2(I)=0
 708     CONTINUE
C        number of segment records  following
         IWORK2(1)=RTALLY(4)+RTALLY(5)+RTALLY(6)+RTALLY(7)+
     +                               RTALLY(8)+RTALLY(9)
C        Form of line/arc  used i.e. Projection line
         IWORK2(2)=2
C        Form of segment i.e. 0 for line or 1 for arc  used
         IWORK2(3)=SEGTYP
         IF ( LEFT ) THEN
C           set left projection line state
            IWORK2(4)=0
         ELSE
C           set Right projection line state
            IWORK2(4)=1
         END IF
         IF ( DOPFLG(20) ) THEN
C           projection line supression asked for
            LCRDAT(9)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                       +IWORK2(4)*4096+32768
         ELSE
C           no projection line supression
            LCRDAT(9)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                                   +IWORK2(4)*4096
         END IF
         K=0
         IF ( RTALLY(8) .GT. 0 ) THEN
            CONTINUE
            DO 308 I=RCDCNT+1,RCDCNT+RTALLY(8)
C              set local counter
               K=K+1
C              set integer control data
               IWORK(4,I)=I4I2(LCRDAT(9))
               IWORK(1,I)=LINARC
               DO 529 J=1,6
                  RWORK(J,I)=DIMEXL(J,K)
 529           CONTINUE
 308        CONTINUE
            RCDCNT=RCDCNT+RTALLY(8)
         END IF
         K=0
         IF ( RTALLY(9) .GT. 0 ) THEN
            DO 309 I=RCDCNT+1,RCDCNT+RTALLY(9)
C             set local counter
              K=K+1
C             set integer control data
              IWORK(4,I)=I4I2(LCRDAT(9))
              IWORK(1,I)=LINARC
              DO 533 J=1,6
                 RWORK(J,I)=DIMEXR(J,K)
 533          CONTINUE
 309        CONTINUE
            RCDCNT=RCDCNT+RTALLY(9)
         END IF
      END IF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE SETGAP(GAPEXT,TGAPL,TEXTL)
C     ====================================
C1                      R      R     R
C1                      I      O     O
C2    Sub SETGAP reconstructs the dimension Gap length and Extension
C2    length. This uses the witness line points , reference point and
C2    arrow apex point and returns  the distances in TGAPL & TEXTL.
C2
      include   'include/ndata.inc'
      include   'include/params.inc'
      include   'include/dhead.inc'
      REAL GAPEXT(3,4),TGAPL,TEXTL,SCA
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,DVDD13
      LOGICAL SAME
      EXTERNAL DVDD13,SAME
 
C     calculate the witness line line vector.
      IF (  SAME(GAPEXT(1,3),GAPEXT(1,4)).AND.
     +      SAME(GAPEXT(2,3),GAPEXT(2,4))      ) THEN
         TGAPL=0.0
         TEXTL=0.0
         RETURN
      END IF
*
      CALL DCVL14(DBLE(GAPEXT(1,3)),DBLE(GAPEXT(2,3)),
     +            DBLE(GAPEXT(1,4)),DBLE(GAPEXT(2,4)),
     1                                       L1,L2,L3)
C     Generate perpindicular line vector thro reference point.
      CALL DVV0L6(L1,L2,L3,DBLE(GAPEXT(1,1)),DBLE(GAPEXT(2,1))
     +                                              ,M1,M2,M3)
      SCA=(PAPTOW/RDHEAD(3))/PAPTOW
C      WRITE(10,'(A,/5F8.3)') 'SCA,PAPTOW,RDHEAD(3),RDHEAD(1),DBUFAC',
C     +             SCA,PAPTOW,RDHEAD(3),RDHEAD(1),DBUFAC
C     get sign and distance of the extension line length
C     convert the distance to paper units
      TGAPL=SCA*ABS(DVDD13(M1,M2,M3,DBLE(GAPEXT(1,3)),
     +                            DBLE(GAPEXT(2,3))))
C     Generate perpindicular line vector thro arrow point.
      CALL DVV0L6(L1,L2,L3,DBLE(GAPEXT(1,2)),DBLE(GAPEXT(2,2))
     +                                              ,M1,M2,M3)
C     get sign and distance of the extension line length
C     convert the distance to paper units
      TEXTL=SCA*ABS(DVDD13(M1,M2,M3,DBLE(GAPEXT(1,4)),
     +                            DBLE(GAPEXT(2,4))))
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE TEXBOR(X4,Y4,BLWD,BLHT,X1,Y1)
C     =======================================
C1    vartype           R   R   R   R   R  R
C1    iostat            I   I   I   I   I  I
C
      include   'include/ndata.inc'
C
      REAL X4,Y4,BLWD,BLHT,X1(2),Y1(2)
      LOGICAL OK
C     set  coords as (Origin-Text offset) & (Origin+Text width=offset)
      X1(1)=X4-DTOFF*PAPTOW
      X1(2)=X4+(BLWD+DTOFF)*PAPTOW
      Y1(1)=Y4-DTOFF*PAPTOW
      Y1(2)=Y4+(BLHT+DTOFF)*PAPTOW
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE TEXREC(RCDCNT)
C     =========================
C
C1    vartype            I2
C1    iostatus           I/O
C
C2    TEXREC is a  routine to set the  text records
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10)
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K
      LOGICAL OK
C
C     record number of text strings
C     clear scratch array
      DO 702 I=1,4
         IWORK2(I)=0
 702  CONTINUE
C     number of text records
      IWORK2(1)=RTALLY(2)
C     text suppression state
      IF ( DOPFLG(16) ) THEN
C        set flag to show left text
         IWORK2(3)=0
      ELSE
C        show text to be to right of dimension.
         IWORK2(3)=1
      END IF
      IF ( DOPFLG(11) ) THEN
C        Offset text asked for.
         IWORK2(4)=1
      END IF
      IF ( DOPFLG(19) ) THEN
C        set the sign bit 15 to show text surpressed
         LCRDAT(3)=IWORK2(1)+IWORK2(3)*16+IWORK2(4)*32+32768
      ELSE
C        dont affect any sign bit
         LCRDAT(3)=IWORK2(1)+IWORK2(3)*16+IWORK2(4)*32
      END IF
      K=0
      DO 301 I=RCDCNT+1,RCDCNT+RTALLY(2)
C       Set local counter
        K=K+1
C       set integer control data
        IWORK(4,I)=I4I2(LCRDAT(3))
        IWORK(1,I)=TEXSEG
C       set real part data.
        DO 505 J=1,6
           RWORK(J,I)=DIMTXT(J,K)
 505    CONTINUE
 301  CONTINUE
C     update record counter
      RCDCNT=RCDCNT+RTALLY(2)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE TRALRC(RCDCNT,BLWD,BLHT)
C     ==================================
C
C1    vartype           I2     R    R
C1    iostatus          I      I    I
C
C2    FRSTRC is a  routine to set the  TRAILER record
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER*4 LCRDAT(10),I,IWORK2(10)
      INTEGER*2 I4I2,RCDCNT
      REAL BLWD,BLHT
C
C     clear scratch array
      DO 709 I=1,3
         IWORK2(I)=0
 709  CONTINUE
C     form of dimension tolerance used
      IWORK2(1)=TOLTYP-1
C     Form of special character used with dimension.
      IWORK2(2)=SPCSYM
C     units used for dimension
      IF ( .NOT.METRIC ) THEN
C        imperial units used
         LCRDAT(10)=IWORK2(1)+IWORK2(2)*16+32768
      ELSE
         LCRDAT(10)=IWORK2(1)+IWORK2(2)*16
      END IF
      IWORK(4,RCDCNT+1)=I4I2(LCRDAT(10))
      IWORK(1,RCDCNT+1)=TRAILR
C     set the real data section
      RWORK(1,RCDCNT+1)=DTOFF
      RWORK(2,RCDCNT+1)=UTOL
      RWORK(3,RCDCNT+1)=LTOL
      RWORK(4,RCDCNT+1)=PREC
      RWORK(5,RCDCNT+1)=BLWD
      RWORK(6,RCDCNT+1)=BLHT
C     set original record count
      RTALLY(10)=1
C     set total record counter
      RECCNT(1)=RCDCNT+1
C     That's all for now
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C     =========================================================
C
C1    vartype           R  R  R  R  R  R  R     R      L
C1    iostatus          I  I  I  I  I  I  I     O      O
C
C2    TXTREC is a general routine to write the  6 real variables
C2    X1-X6 into the next free position in the array RELDAT.
C2    Returns OK with .TRUE. if successful.
C2    This is for use by the Dimension module.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
      INTEGER*2 JST,NCHAR
      INTEGER*4 NSTR,I,NLEN1
      REAL X4,Y4,RANG,TAGL,STRVAL(4,10),MAT(3,3),NX4,NY4,CODED
      CHARACTER DIMNTX(10)*80
      LOGICAL OK
      EXTERNAL ROTP2D,CODET,NEWXY
C
C     define any rotation matrix if required
      CALL ROTP2D(X4,Y4,RANG,MAT)
      DO 101 I=1,NSTR
         NCHAR=NLEN1(DIMNTX(I))
C        encode text parameters.
         CALL CODET(0.0,JST,NCHAR,CODED)
C        calculate new origin if required.
         CALL NEWXY(X4+STRVAL(3,I)*PAPTOW,Y4+STRVAL(4,I)*PAPTOW,
     +                                              NX4,NY4,MAT)
C        Set the text String ,height,width & angle
         DIMCHR(I)=DIMNTX(I)
         CALL DMWREC(NX4,NY4,STRVAL(2,I),STRVAL(1,I),TAGL,CODED,
     +                                      RTALLY(2),DIMTXT,OK)
 101  CONTINUE
C
 99   CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE WITREC(SUPLEF,SUPRIT,RCDCNT)
C     ======================================
C
C1    vartype           L      L       I2
C1    iostatus          I      I       I/O
C
C2    TXTREC is a  routine to set the  text records
C2    of Dimension data.
C
      include 'include/dimendat.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN1,LCRDAT(10),C,I,IWORK2(10)
      INTEGER*2 JST,NLEN2,NCHAR,P,I4I2,RCDCNT,J,K
      LOGICAL SUPLEF,SUPRIT
C
C     next record left Witness line
C     clear scratch array
      DO 706 I=1,4
         IWORK2(I)=0
 706  CONTINUE
C     number of segment records  following
      IWORK2(1)=RTALLY(4)+RTALLY(5)+RTALLY(6)+RTALLY(7)+
     +                               RTALLY(8)+RTALLY(9)
C
C     Form of line/arc  used i.e. Witness line
      IWORK2(2)=1
C     Form of segment i.e. 0 for line or 1 for arc  used
      IWORK2(3)=0
C     set left witness state
      IWORK2(4)=0
      IF ( SUPRIT ) THEN
C        left dimension line supression asked for
         LCRDAT(7)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                       +IWORK2(4)*4096+32768
      ELSE
C        no dimension line supression
         LCRDAT(7)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                                   +IWORK2(4)*4096
      END IF
      K=0
      DO 306 I=RCDCNT+1,RCDCNT+RTALLY(6)
C       set local counter
        K=K+1
C       set integer control data
        IWORK(4,I)=I4I2(LCRDAT(7))
        IWORK(1,I)=LINSEG
        DO 521 J=1,6
           RWORK(J,I)=DIMWTL(J,K)
 521    CONTINUE
 306  CONTINUE
      RCDCNT=RCDCNT+RTALLY(6)
C     next record RIGHT witness line
C     clear scratch array
      DO 707 I=1,4
         IWORK2(I)=0
 707  CONTINUE
C     number of segment records  following
      IWORK2(1)=RTALLY(4)+RTALLY(5)+RTALLY(6)+RTALLY(7)+
     +                               RTALLY(8)+RTALLY(9)
C     Form of line/arc  used i.e witness line
      IWORK2(2)=1
C     Form of segment i.e. 0 for line or 1 for arc  used
      IWORK2(3)=0
C     set RIGHT Dimension state
      IWORK2(4)=1
      IF ( SUPLEF ) THEN
C        right dimension line supression asked for
         LCRDAT(8)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                       +IWORK2(4)*4096+32768
      ELSE
C        no dimension line supression
         LCRDAT(8)=IWORK2(1)+IWORK2(2)*64+IWORK2(3)*256
     +                                   +IWORK2(4)*4096
      END IF
      K=0
      DO 307 I=RCDCNT+1,RCDCNT+RTALLY(7)
C       set local counter
        K=K+1
C       set integer control data
        IWORK(4,I)=I4I2(LCRDAT(8))
        IWORK(1,I)=LINSEG
        DO 525 J=1,6
           RWORK(J,I)=DIMWTR(J,K)
 525    CONTINUE
 307  CONTINUE
      RCDCNT=RCDCNT+RTALLY(7)
C
      END
C
C     ---------------------------------------------------------
C
