C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 dimen2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DATXCK(BLWD,BLHT,X4,Y4,TXTOUT)
C     SUBROUTINE DIMA03(PDAT,LDAT1,LDAT2,X4,Y4)
C     SUBROUTINE DIMA04(CDAT,X3,Y3,X4,Y4,X5,Y5,TXTOUT,ARROIN,LEFT,
C     SUBROUTINE DIMF34(PDAT,LDAT1,LDAT2,X4,Y4)
C     SUBROUTINE DINTXT(XC,YC,RAD,BLWD,TXTOUT,LEFT,X4,Y4,RANG)
C     SUBROUTINE PRDVEC(X1,Y1,X2,Y2,U1,U2)
C     SUBROUTINE QADSET(ANGLE,WX1,WY1,BLWD,BLHT,X4,Y4)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DATXCK(BLWD,BLHT,X4,Y4,TXTOUT)
C     ===========================================
C1                      R    R    R   R   L
C1                      I    I    I   I   O
C2    Subroutine DAXCK checks dimension text can fit
C2    inside the Angular dimension lines.
C2
C2     BLWD= Dimension text block width
C2     BLHT=  Dimension text block height
C2     WX1=   Dimension line midpoint in X
C2     WY1=   Dimension line midpoint in Y
C2     TXTOUT= True if Text is outside dimension lines.
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
      REAL BLWD,BLHT,X4,Y4,PDAT(2,2),TX,TY,DISTXY
C
      LOGICAL OK,TXTOUT,TOUT
C
      EXTERNAL CC00P5,CHKLN,DISTXY
C
      TXTOUT=.FALSE.
      IF ( .NOT.DOPFLG(12) ) THEN
C        text flag is inner but check if can be done
         IF ( ANSI .OR. BS ) THEN
C           Dimension text is horizontal so do check for fit.
            CALL ANSDIM(DIMWTL(1,1),DIMWTL(2,1),DIMWTL(4,1),
     +                    DIMWTL(5,1),TX,TY,BLWD,BLHT,X4,Y4,
     1                                        LDIMN,PDAT,OK)
C           if OK True then intersect found.
            TOUT=OK
            CALL ANSDIM(DIMWTR(1,1),DIMWTR(2,1),DIMWTR(4,1),
     +                    DIMWTR(5,1),TX,TY,BLWD,BLHT,X4,Y4,
     1                                        LDIMN,PDAT,OK)
            TXTOUT=( OK .OR. TOUT )
         ELSE
C           text parallel to dim Arc so use Chordal length
            TXTOUT=( BLWD*PAPTOW .GT. DISTXY(DIMWTL(4,1),DIMWTL(5,1),
     +               DIMWTR(4,1),DIMWTR(5,1) ) )
         END IF
      END IF
C     Force outer status depending on user controiied flags.
      IF ( DOPFLG(12) ) TXTOUT=.TRUE.
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMA03(PDAT,LDAT1,LDAT2,X4,Y4)
C     =========================================
C1                      R    R     R     R  R
C1                      I    I     I     I  I
C2    Subroutine DIMA03 constructs an Angular  dimension
C2    from DOPFLAg logical flags and passed points info
C2    in PDAT,LDAT1,LDAT2 .
C2    The Angular dimension s constructed on the basis of
C2    a circle with portions or part portions of the circle
C2    forming the dimension lines.
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
      REAL PDAT(2,4),LDAT1(6),LDAT2(6),DCR,DISTXY,PI,MAT(3,3)
      REAL CDAT(6),X3,Y3,X4,Y4,X5,Y5,A1,A2,A3,CANG,MANG,U1,U2
      REAL X6,Y6,X7,Y7,DEG,TAGL,RANG,TX4,TY4,NX3,NY3,RDAT(2,2)
      REAL T1,TOX,TOY,ANGLE,COS,SIN,TA1,TA2,TMPANG
      REAL STRVAL(4,10),BLWD,BLHT,WX1,WY1,DIMD,TMPX1,TMPY1,TMPX2,TMPY2
      INTEGER*2 I4I2,RCDCNT,J,K
      INTEGER*4 NLEN1,NSTR,I
      INTEGER*2 ENT1,ENT2,NCHAR,NLEN2,P,JST
      INTEGER*4 QUADA,Q1,Q2,Q,P1,P2,P3,MOD,INDEX,SEGTYP
      LOGICAL OK,CHKLN,TXTOUT,LEFT,ARROIN,CLKWS,INVARR,OK1
     +        ,WITLEF,WITRIT
      CHARACTER DIMNTX(10)*80
      INTRINSIC MOD,CHAR,INDEX,COS,SIN
      EXTERNAL DISTXY,CANG,QUADA,NLEN2,DEG,
     +         DIMA10,INTLAH,RSWAP,UCU001,CHKLN,DIMA30,NLEN1,
     +         DIML13,CODET
C
C     reset all record counter flags to zero
      CALL CLRTAL()
      RTMP=2.5*ALNG*PAPTOW
C
      WITLEF=.FALSE.
      WITRIT=.FALSE.
      TX4=X4
      TY4=Y4
C     find dimension circle radius
      DCR=DISTXY(PDAT(1,1),PDAT(2,1),PDAT(1,4),PDAT(2,4))
C     create temporary data array for dim circle
      CDAT(1)=PDAT(1,1)
      CDAT(2)=PDAT(2,1)
      CDAT(3)=0.0
      CDAT(4)=DCR
      CDAT(5)=0.0
      CDAT(6)=PI(2.0)
C
C     store the line data for use by witness lines.
      IF (DOPFLG(1)) THEN
C        between points mode
C        put data into first array
         LDAT1(1)=PDAT(1,1)
         LDAT1(2)=PDAT(2,1)
         LDAT1(3)=0
         LDAT1(4)=PDAT(1,2)
         LDAT1(5)=PDAT(2,2)
         LDAT1(6)=0
C        put data into second array
         LDAT2(1)=PDAT(1,1)
         LDAT2(2)=PDAT(2,1)
         LDAT2(3)=0
         LDAT2(4)=PDAT(1,3)
         LDAT2(5)=PDAT(2,3)
         LDAT2(6)=0
      END IF
      ENT1=ARC
      ENT2=LINE
C     find  intersect of dimension arc and first line.
      CALL INTLAH(ENT1,CDAT,ENT2,LDAT1,PDAT(1,2),PDAT(2,2),X3,Y3,OK)
C     find angle of this intersection
      A1=CANG(PDAT(1,1),PDAT(2,1),X3,Y3)
C     find intersect of dimension arc and second  line.
      CALL INTLAH(ENT1,CDAT,ENT2,LDAT2,PDAT(1,3),PDAT(2,3),X5,Y5,OK)
C     find angle of this intersection
      A2=CANG(PDAT(1,1),PDAT(2,1),X5,Y5)
C     find angle of Text hit position
      A3=CANG(PDAT(1,1),PDAT(2,1),PDAT(1,4),PDAT(2,4))
C     Must store original data points in control block.
      IF (DOPFLG(2)) THEN
C        must store line end point
         CALL ENDVAL(PDAT(1,1),PDAT(2,1),X3,Y3,LDAT1(1),LDAT1(2),
     +               LDAT1(4),LDAT1(5),TMPX1,TMPY1)
         CALL ENDVAL(PDAT(1,1),PDAT(2,1),X5,Y5,LDAT2(1),LDAT2(2),
     +               LDAT2(4),LDAT2(5),TMPX2,TMPY2)
C        Now store the actual data away
         CALL DMWREC(PDAT(1,1),PDAT(2,1),0.0,TMPX1,TMPY1,0.0,
     +               RTALLY(1),DIMCON,OK)
         CALL DMWREC(TMPX2,TMPY2,0.0,PDAT(1,4),PDAT(2,4),0.0,
     +               RTALLY(1),DIMCON,OK)
      ELSE
         CALL DMWREC(PDAT(1,1),PDAT(2,1),0.0,PDAT(1,2),PDAT(2,2),0.0,
     +               RTALLY(1),DIMCON,OK)
         CALL DMWREC(PDAT(1,3),PDAT(2,3),0.0,PDAT(1,4),PDAT(2,4),0.0,
     +               RTALLY(1),DIMCON,OK)
      END IF
C     test angles to check that in ascending order a/clockwise.
      IF ( CLKWS(PDAT(1,1),PDAT(2,1),X5,Y5,X3,Y3) ) THEN
C        2nd angle c/wise to first so swap arguments.
C        swap the angles and intersect points
         CALL RSWAP(A1,A2)
         CALL RSWAP(X3,X5)
         CALL RSWAP(Y3,Y5)
C        swap the line end points
         DO 91 I=1,6
            CALL RSWAP(LDAT1(I),LDAT2(I))
 91      CONTINUE
C        swap the control data line end points
         DO 92 I=1,3
            CALL RSWAP(DIMCON(I+3,1),DIMCON(I,2))
 92      CONTINUE
      END IF
C     Set the true angle.
      Q1=QUADA(A1)
      Q2=QUADA(A2)
C     Check for quadrant fit.
      IF ( (Q1.EQ.3.OR.Q1.EQ.4 ).AND.(Q2.EQ.1) .OR.
     +     (Q1.EQ.4).AND.(Q2.EQ.2) ) THEN
C         danger quadrants.
          ANGLE=REAL(DBLE(PI(2.0))-(ABS(DBLE(A1)-DBLE(A2))))
      ELSE
          ANGLE=REAL(DBLE(A2)-DBLE(A1))
      END IF
C     set angle mid point
      MANG=A1+(ANGLE/2)
C     set dimension angle points accordingly.
      CDAT(5)=A1
      CDAT(6)=A2
      IF ( DOPFLG(13) )  THEN
C        major angle required so must be complemented
C        ensure mid-angle is in 0-2PI range
         MANG=MOD(MANG+PI(1.0),PI(2.0))
      END IF
C     confirm whether angle required is minor or major one.
      IF ( DOPFLG(13) .AND. ( ANGLE .LT. PI(1.0) ) )
     +                             ANGLE=PI(2.0)-ABS(ANGLE)
C     call text generation routine,origin text all at bottom left
 944  CONTINUE
      JST=1
      CALL DMNTXT(DEG(ANGLE),DIMNTX,STRVAL,BLWD,BLHT,NSTR)
C     set dimension arc centre point
      WX1=CDAT(1)+(DCR*COS(MANG))
      WY1=CDAT(2)+(DCR*SIN(MANG))
C     now check text hit point against mid angle
      IF ( DOPFLG(12) ) THEN
C        use user defined hit external hit point
         LEFT=CLKWS(CDAT(1),CDAT(2),WX1,WY1,X4,Y4)
      ELSE
C        use point to define dimension arc reference
         LEFT=CLKWS(CDAT(1),CDAT(2),WX1,WY1,PDAT(1,4),PDAT(2,4))
      END IF
C     generate witness line detail for use by text checking.
C     First do sorting of end points :-
      RWORK1(101)=DISTXY(LDAT1(1),LDAT1(2),X3,Y3)
      RWORK1(102)=DISTXY(LDAT1(4),LDAT1(5),X3,Y3)
         IF (RWORK1(101).LE.RWORK1(102)) THEN
C           first line endpoint is closest
            RWORK1(101)=LDAT1(1)
            RWORK1(102)=LDAT1(2)
         ELSE
C           second line endpoint must be closest
            RWORK1(101)=LDAT1(4)
            RWORK1(102)=LDAT1(5)
         END IF
 
      CALL DIMWL4(RWORK1(101),RWORK1(102),X3,Y3,
     +            GAPL*PAPTOW,EXTL*PAPTOW,
     +            RWORK1(101),RWORK1(102),RWORK1(103),RWORK1(104))
      CALL DMWREC(RWORK1(101),RWORK1(102),0.0,
     +            RWORK1(103),RWORK1(104),0.0,
     +            RTALLY(7),DIMWTR,OK)
C
C
      RWORK1(105)=DISTXY(LDAT2(1),LDAT2(2),X5,Y5)
      RWORK1(106)=DISTXY(LDAT2(4),LDAT2(5),X5,Y5)
         IF (RWORK1(105).LE.RWORK1(106)) THEN
C           first line endpoint is closest
            RWORK1(105)=LDAT2(1)
            RWORK1(106)=LDAT2(2)
         ELSE
C           second line endpoint must be closest
            RWORK1(105)=LDAT2(4)
            RWORK1(106)=LDAT2(5)
         END IF
      CALL DIMWL4(RWORK1(105),RWORK1(106),X5,Y5,
     +            GAPL*PAPTOW,EXTL*PAPTOW,
     +            RWORK1(105),RWORK1(106),RWORK1(107),RWORK1(108))
      CALL DMWREC(RWORK1(105),RWORK1(106),0.0,
     +            RWORK1(107),RWORK1(108),0.0,
     +            RTALLY(6),DIMWTL,OK)
C
C     must automatically suppress witness lines
C     if between line mode,and arrowhead inside bounds
C     of line
      IF (.NOT.DOPFLG(1)) THEN
C
         WITLEF=(CHKLN(LDAT1(1),LDAT1(4),DIMWTR(4,1)) .AND.
     +       CHKLN(LDAT1(2),LDAT1(5),DIMWTR(5,1)) .OR.
     1       DOPFLG(8) )
         WITRIT=(CHKLN(LDAT2(1),LDAT2(4),DIMWTL(4,1)) .AND.
     +       CHKLN(LDAT2(2),LDAT2(5),DIMWTL(5,1)) .OR.
     1       DOPFLG(7) )
      ELSE
C        between points and supress asked for.
         WITLEF=DOPFLG(8)
         WITRIT=DOPFLG(7)
      END IF
C     if Major angle required then dim lines and text outside.
      IF ( .NOT. DOPFLG(13) ) THEN
C         Check text can fit inside the dimension lines
         IF ( ANSI .OR. BS ) THEN
           CALL ANSDIM(RWORK1(101),RWORK1(102),RWORK1(103),RWORK1(104)
     1                ,NX3,NY3,BLWD,BLHT,WX1,WY1,LDIMN,RDAT,OK1)
           CALL ANSDIM(RWORK1(105),RWORK1(106),RWORK1(107),RWORK1(108)
     1                ,NX3,NY3,BLWD,BLHT,WX1,WY1,LDIMN,RDAT,OK)
C
C
           TXTOUT=( DOPFLG(12) .OR. ( OK ) .OR. (  OK1 ) )
 
         ELSE
C          text parallel to dim Arc so use Chordal length
           TXTOUT=( BLWD*PAPTOW .GT. DISTXY(DIMWTL(4,1),DIMWTL(5,1),
     +               DIMWTR(4,1),DIMWTR(5,1) ) )
C           CALL DATXCK(BLWD,BLHT,WX1,WY1,TXTOUT)
         END IF
         IF ( TXTOUT ) THEN
C            check to see if asked for
C            get intersection of witness lines and the arc
             INVARR=(CLKWS(CDAT(1),CDAT(2),X3,Y3,X4,Y4) .AND.
     +              CLKWS(CDAT(1),CDAT(2),X4,Y4,X5,Y5) )
             IF ( .NOT. DOPFLG(12) .OR.  INVARR ) THEN
                IF ( LEFT ) THEN
C                  not asked for so set X4,Y4.
                   CALL PRDVEC(CDAT(1),CDAT(2),X5,Y5,U1,U2)
                   CALL UC00P4(X5,Y5,-RTMP,U1,U2,X4,Y4)
                ELSE
C                  not asked for so set X4,Y4.
                   CALL PRDVEC(CDAT(1),CDAT(2),X3,Y3,U1,U2)
                   CALL UC00P4(X3,Y3,RTMP,U1,U2,X4,Y4)
                END IF
             END IF
          END IF
C         check that arrows can fit inside angle, approx using chord.
          ARROIN=(.NOT.( RTMP .GT. DISTXY(X3,Y3,X5,Y5)))
          IF ( DOPFLG(9)) ARROIN=.FALSE.
      ELSE
          ARROIN=.FALSE.
          TXTOUT=.FALSE.
      END IF
C     now call routines to do dimension generation
      CALL DIMA04(CDAT,X3,Y3,X4,Y4,X5,Y5,TXTOUT,ARROIN,LEFT,WX1,WY1,
     +                                         MANG,RANG,BLWD,BLHT )
C     set the text angle and if DIN the position.
      IF ( DIN ) THEN
          TAGL=DEG(RANG)
      ELSE
C         set text angle always to be zero.
          RANG=0
          TAGL=0
      END IF
C
C     set the text data.
      CALL TXTREC(X4,Y4,RANG,TAGL,STRVAL,JST,NSTR,DIMNTX)
C     set control data for storage purposes
      IF ( LEFT ) THEN
C        Set flag indicating Whether outer text left of dimension
         DOPFLG(16)=.TRUE.
      END IF
C     write out the Master index data.
      CALL ORGDAT()
C     set the header record count
      RCDCNT=2
C     next First record of dimension storage
      CALL FRSTRC(RCDCNT,ARROIN,TXTOUT,ADIMN)
C     set segment type to show arc dimension & projections.
      SEGTYP=1
C     next record number of text strings
      CALL TEXREC(RCDCNT)
C     set left arrowhead information
      CALL ARRREC(RCDCNT,DOPFLG(17),DOPFLG(18))
C     record  Dimension lines
      CALL DIMREC(RCDCNT,SEGTYP,DOPFLG(17),DOPFLG(18))
C     next  Witness lines
      CALL WITREC(WITLEF,WITRIT,RCDCNT)
C     next  projection lines,call with 0 for linear projection lines.
      CALL PRJREC(RCDCNT,SEGTYP,LEFT)
C     Finally write out the trailer record.
      CALL TRALRC(RCDCNT,BLWD,BLHT)
C     That's all for now
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMA04(CDAT,X3,Y3,X4,Y4,X5,Y5,TXTOUT,ARROIN,LEFT,
     +                  WX1,WY1,MANG,RANG,BLWD,BLHT )
C     ============================================================
C
      include  'include/dimendat.inc'
      include  'include/ndata.inc'
      include  'include/entity.inc'
C
      REAL L1,L2,L3,PDAT(2,2),CDAT(6),X3,Y3,X4,Y4,X5,Y5,
     +     MANG,BLWD,BLHT,RANG,U1,U2,ANG1,ANG2,ANGLE,CANG,
     1     WX1,WY1,RAD,TX1,TY1,TX2,TY2,DISTXY
      INTEGER*4 NLEN1,QUADA,Q
      LOGICAL LEFT,ARROIN,TXTOUT,OK,CLKWS
      EXTERNAL CANG,RAD,CLKWS,DISTXY
C
      IF ( ARROIN ) THEN
C        arrowheads are inside
C        find unit vector of first arrowhead
         CALL PRDVEC(CDAT(1),CDAT(2),X5,Y5,U1,U2)
         CALL DMWREC(X5,Y5,-U1,-U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
         CALL PRDVEC(CDAT(1),CDAT(2),X3,Y3,U1,U2)
         CALL DMWREC(X3,Y3,U1,U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
         IF ( .NOT.TXTOUT ) THEN
C           Inner arrows and inner text.
            IF ( ANSI ) THEN
              X4=WX1-(BLWD/2*PAPTOW)
              Y4=WY1-(BLHT/2*PAPTOW)
              CALL ANSDIM(CDAT(1),CDAT(2),X3,Y3,X5,Y5,BLWD,BLHT,
     +                                     X4,Y4,ADIMN,PDAT,OK)
              IF ( OK ) THEN
C                intersects found
                 ANG2=CANG(CDAT(1),CDAT(2),PDAT(2,1),PDAT(2,2))
                 ANG1=CANG(CDAT(1),CDAT(2),PDAT(1,1),PDAT(1,2))
              ELSE
C                no intersects found so set default condition.
                 ANG1=MANG
                 ANG2=MANG
              END IF
              CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),ANG1,
     +                                CDAT(6),RTALLY(4),DIMLNL,OK)
              CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5),
     +                                   ANG2,RTALLY(5),DIMLNR,OK)
            ELSE IF ( BS .OR. DIN ) THEN
              CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),MANG,
     +                             CDAT(6),RTALLY(4),DIMLNL,OK)
              CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5),
     +                                MANG,RTALLY(5),DIMLNR,OK)
              IF ( DIN ) THEN
                 X4=WX1
                 Y4=WY1
                 CALL DINTXT(CDAT(1),CDAT(2),CDAT(4),BLWD,TXTOUT,
     +                                        LEFT,X4,Y4,RANG)
              ELSE
C                text is inner so set according to quadrant
                 CALL QADSET(MANG,WX1,WY1,BLWD,BLHT,X4,Y4)
              END IF
            END IF
         ELSE
C           inner arrows & outer text position so projection needed.
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),MANG,
     +                            CDAT(6),RTALLY(4),DIMLNL,OK)
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5),
     +                                 MANG,RTALLY(5),DIMLNR,OK)
C           set the projection arc
            ANGLE=CANG(CDAT(1),CDAT(2),X4,Y4)
            IF ( LEFT ) THEN
C               use arc start point as start point
                CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6),
     +                                ANGLE,RTALLY(8),DIMEXL,OK)
            ELSE
C               use arc end point as start point
                CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),ANGLE,
     +                                 CDAT(5),RTALLY(9),DIMEXR,OK)
            END IF
C           set text position according to quadrant.
C           generate vector L thru X4,Y4 and XC,YC
            CALL CV0L14(X4,Y4,CDAT(1),CDAT(2),L1,L2,L3)
C           find intersection point on arc
            CALL VC00P9(CDAT(1),CDAT(2),CDAT(4),L1,L2,L3,
     +                     TX1,TY1,TX2,TY2,OK)
C           take nearest point on Arc to hit
            IF ( DISTXY(X4,Y4,TX1,TY1)    .LE.
     +           DISTXY(X4,Y4,TX2,TY2)  ) THEN
               WX1=TX1
               WY1=TY1
            ELSE
               WX1=TX2
               WY1=TY2
            END IF
            IF ( DIN ) THEN
                 X4=WX1
                 Y4=WY1
                 CALL DINTXT(CDAT(1),CDAT(2),CDAT(4),
     +                 BLWD,TXTOUT,LEFT,X4,Y4,RANG)
            ELSE
C               find quadrant of dimension mid-angle
                CALL QADSET(ANGLE,WX1,WY1,BLWD,BLHT,X4,Y4)
            END IF
         END IF
      ELSE
C        arrowheads are outer.
C        find unit vector of first arrowhead
         CALL PRDVEC(CDAT(1),CDAT(2),X5,Y5,U1,U2)
         CALL DMWREC(X5,Y5,U1,U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
         CALL PRDVEC(CDAT(1),CDAT(2),X3,Y3,U1,U2)
         CALL DMWREC(X3,Y3,-U1,-U2,ALNG,AWDT,RTALLY(3),DIMARR,OK)
         IF ( .NOT.TXTOUT ) THEN
C         outer arrows & inner text
          IF ( DOPFLG(13)) THEN
            IF (BS .OR. DIN ) THEN
C           store Left dimension arc data in array
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),MANG,
     +                   CDAT(5),RTALLY(4),DIMLNL,OK)
C           store Right dimension arc data in array
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6)
     +                  ,MANG,RTALLY(5),DIMLNR,OK)
            ELSE
             X4=WX1-(BLWD/2*PAPTOW)
             Y4=WY1-(BLHT/2*PAPTOW)
             CALL ANSDIM(CDAT(1),CDAT(2),X3,Y3,X5,Y5,BLWD,BLHT,
     +                                     X4,Y4,ADIMN,PDAT,OK)
             IF ( OK ) THEN
C                intersects found
                 ANG2=CANG(CDAT(1),CDAT(2),PDAT(2,1),PDAT(2,2))
                 ANG1=CANG(CDAT(1),CDAT(2),PDAT(1,1),PDAT(1,2))
             ELSE
C                no intersects found so set default condition.
                 ANG1=MANG
                 ANG2=MANG
             END IF
             CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),ANG1,
     +                                CDAT(5),RTALLY(4),DIMLNL,OK)
             CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6),
     +                                   ANG2,RTALLY(5),DIMLNR,OK)
            END IF
            IF (  BS ) THEN
C               find quadrant of dimension mid-angle
                CALL QADSET(MANG,WX1,WY1,BLWD,BLHT,X4,Y4)
            ELSE IF ( DIN ) THEN
                X4=WX1
                Y4=WY1
                CALL DINTXT(CDAT(1),CDAT(2),CDAT(4),
     +              BLWD,TXTOUT,LEFT,X4,Y4,RANG)
            END IF
          ELSE
C           store Left dimension arc data in array
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6),
     +                   CDAT(6)+RTMP/CDAT(4),RTALLY(4),DIMLNL,OK)
C           store Right dimension arc data in array
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5)-
     +                  RTMP/CDAT(4),CDAT(5),RTALLY(5),DIMLNR,OK)
            IF ( ANSI ) THEN
              X4=WX1-(BLWD/2*PAPTOW)
              Y4=WY1-(BLHT/2*PAPTOW)
            ELSE IF (  BS ) THEN
C               find quadrant of dimension mid-angle
                CALL QADSET(MANG,WX1,WY1,BLWD,BLHT,X4,Y4)
            ELSE IF ( DIN ) THEN
                X4=WX1
                Y4=WY1
                CALL DINTXT(CDAT(1),CDAT(2),CDAT(4),
     +              BLWD,TXTOUT,LEFT,X4,Y4,RANG)
            END IF
          END IF
         ELSE
C           outer arrows & outer text position
            ANGLE=CANG(CDAT(1),CDAT(2),X4,Y4)
C           check position of text relative to dimension
            IF ( LEFT ) THEN
C              store Right dimension arc data in array
               CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5)-
     +                    RTMP/CDAT(4),CDAT(5),RTALLY(5),DIMLNR,OK)
               CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6),
     +                                   ANGLE,RTALLY(4),DIMLNL,OK)
            ELSE
C              use arc end point as start point
C              store Left dimension arc data in array
               CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(6),
     +                   CDAT(6)+RTMP/CDAT(4),RTALLY(4),DIMLNL,OK)
               CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),ANGLE,
     +                                  CDAT(5),RTALLY(5),DIMLNR,OK)
            END IF
C           generate vector L thru X4,Y4 and XC,YC
            CALL CV0L14(X4,Y4,CDAT(1),CDAT(2),L1,L2,L3)
C           find intersection point on arc
            CALL VC00P9(CDAT(1),CDAT(2),CDAT(4),L1,L2,L3,
     +                     TX1,TY1,TX2,TY2,OK)
C           take nearest point on Arc to hit
            IF ( DISTXY(X4,Y4,TX1,TY1)    .LE.
     +           DISTXY(X4,Y4,TX2,TY2)  ) THEN
               WX1=TX1
               WY1=TY1
            ELSE
               WX1=TX2
               WY1=TY2
            END IF
C           find quadrant of dimension mid-angle
            IF ( DIN ) THEN
               X4=WX1
               Y4=WY1
               CALL DINTXT(CDAT(1),CDAT(2),CDAT(4),
     +                 BLWD,TXTOUT,LEFT,X4,Y4,RANG)
            ELSE
               CALL QADSET(ANGLE,WX1,WY1,BLWD,BLHT,X4,Y4)
            END IF
         END IF
C        If DIN then draw extension between external dim lines
         IF ( DIN ) THEN
            CALL DMWREC(CDAT(1),CDAT(2),CDAT(3),CDAT(4),CDAT(5),
     +                              CDAT(6),RTALLY(8),DIMEXL,OK)
         END IF
      END IF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DIMF34(PDAT,LDAT1,LDAT2,X4,Y4)
C     =========================================
C1                      R    R     R     R  R
C1                      I    I     I     I  I
C1    X1Y1=1st Dimension Control point
C1    X2Y2=2nd Dimension Control point
C1    X3Y3=Dimension Line centre
C1    X4Y4=Text origin point
C2    Sub DIMF34 analyses the control data held in Common array
C2    IWORK & IMBUFF. This decodes the parameters stored with a
C2    linear dimension and sets the following flags and variables :
C2
C2         DOPFLG(1-20) True or False depending on control data
C2
C2         RTALLY(1-10) Which holds the number of each type of
C2                      dimension element e.g. Left Witness line.
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
      INTEGER*4 RCDCNT
      REAL PDAT(2,4),LDAT1(6),LDAT2(6),X3,Y3,X4,Y4,X5,Y5,
     +     X1,X2,Y1,Y2,BLWD,BLHT,GAPEXT(3,4),DIMLIN(2,2)
C
C     Mip held data
      CALL DIMORG()
C     next First record of dimension storage
      CALL DMFSRC(RCDCNT,ADIMN,GAPEXT)
C     next record number of text strings
      CALL DMTXRC(RCDCNT,ADIMN)
C     next record number of arrowheads
      CALL DMARRC(RCDCNT,DIMLIN)
C     next record left dimension/witness/projection/lines
      CALL DMSGRC(RCDCNT,ADIMN,GAPEXT)
C     next record original data
      CALL DMTRRC(RCDCNT,BLWD,BLHT)
C     set the original control data points.
      PDAT(1,1)=RWORK(1,1)
      PDAT(2,1)=RWORK(2,1)
      PDAT(1,2)=RWORK(4,1)
      PDAT(2,2)=RWORK(5,1)
      PDAT(1,3)=RWORK(1,2)
      PDAT(2,3)=RWORK(2,2)
      PDAT(1,4)=RWORK(4,2)
      PDAT(2,4)=RWORK(5,2)
C     set the text origin to original hit point.
      X4=RWORK(1,3)
      Y4=RWORK(2,3)
      DOPFLG(1)=.TRUE.
C     set line data array
C     put data into first array
      LDAT1(1)=PDAT(1,1)
      LDAT1(2)=PDAT(2,1)
      LDAT1(3)=0
      LDAT1(4)=PDAT(1,2)
      LDAT1(5)=PDAT(2,2)
      LDAT1(6)=0
C     put data into second array
      LDAT2(1)=PDAT(1,1)
      LDAT2(2)=PDAT(2,1)
      LDAT2(3)=0
      LDAT2(4)=PDAT(1,3)
      LDAT2(5)=PDAT(2,3)
      LDAT2(6)=0
C     Set arrow point for GAPL,EXTL regen.
      GAPEXT(1,2)=RWORK(1,RTALLY(1)+RTALLY(2)+1)
      GAPEXT(2,2)=RWORK(2,RTALLY(1)+RTALLY(2)+1)
C     set original record count
      RTALLY(10)=1
C     calculate the witness line Extension and Gap lengths.
      CALL SETGAP(GAPEXT,GAPL,EXTL)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DINTXT(XC,YC,RAD,BLWD,TXTOUT,LEFT,X4,Y4,RANG)
C     ============================================================
C1                      R  R  R   R    R    L      L    R  R  R
C1                      I  I  I   I    I    I      I    I  I  O
C                                                      /O  /O
C2    DINTXT takes in the centre point (xc,yc) , radius (rad) ,
C2    and Angle (mang) of an arc and gives the point (X1,Y1) which is
C2    a distance ( Dist ) along the line perpendicular to that
C2    one defined as XC,YC-Mang(x,y). RANG is the angle of the
C2    perpendicular line in Rads.
C1
C
      include 'include/ndata.inc'
      INTEGER*4 Q,QUADA
      REAL XC,YC,L1,L2,L3,PL1,PL2,PL3,ANGLE,TX4,TY4
     +     ,X1,Y1,U1,U2,BLWD,RANG,CANG,X4,Y4,RAD
      LOGICAL TXTOUT,LEFT
      EXTERNAL CANG,QUADA
C
C     save origin points for later use
      X1=X4
      Y1=Y4
C     first calculate line vector from centre/reference point
      CALL CV0L14(XC,YC,X1,Y1,L1,L2,L3)
C     find perpendicular line thro reference point.
      CALL VV00L6(L1,L2,L3,X1,Y1,PL1,PL2,PL3)
C
      IF ( TXTOUT ) THEN
C        check side of dimension to place text
         IF ( LEFT ) THEN
C           offset from reference point by block width
             ANGLE=CANG(XC,YC,X4,Y4)
             GOTO (1,1,2,2) QUADA(ANGLE)
 1           CONTINUE
             CALL VC00P4(X1,Y1,-(BLWD+DTOFF)*PAPTOW,PL1,PL2,PL3,X4,Y4)
             RANG=CANG(X4,Y4,X1,Y1)
             GOTO 99
 2           CONTINUE
             CALL VC00P4(X1,Y1,-DTOFF*PAPTOW,PL1,PL2,PL3,X4,Y4)
             RANG=CANG(X1,Y1,X4,Y4)
 99          CONTINUE
         ELSE
             ANGLE=CANG(XC,YC,X4,Y4)
             GOTO (11,11,12,12) QUADA(ANGLE)
 11          CONTINUE
             CALL VC00P4(X1,Y1,DTOFF*PAPTOW,PL1,PL2,PL3,X4,Y4)
             RANG=CANG(X1,Y1,X4,Y4)
             GOTO 199
 12          CONTINUE
             CALL VC00P4(X1,Y1,(BLWD+DTOFF)*PAPTOW,PL1,PL2,PL3,X4,Y4)
             RANG=CANG(X4,Y4,X1,Y1)
 199         CONTINUE
         END IF
      ELSE
C        offset from reference point by half block width
         ANGLE=CANG(XC,YC,X4,Y4)
         GOTO (21,21,22,22) QUADA(ANGLE)
 21      CONTINUE
         CALL VC00P4(X1,Y1,-(BLWD/2)*PAPTOW,PL1,PL2,PL3,TX4,TY4)
         GOTO 299
 22      CONTINUE
         CALL VC00P4(X1,Y1,(BLWD/2)*PAPTOW,PL1,PL2,PL3,TX4,TY4)
 299     CONTINUE
         RANG=CANG(TX4,TY4,X1,Y1)
C        offset text from dimension line by DTOFF
         CALL UCU001(TX4,TY4,X1,Y1,U1,U2)
         CALL UC00P4(TX4,TY4,DTOFF*PAPTOW,-U2,U1,X4,Y4)
      END IF
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE PRDVEC(X1,Y1,X2,Y2,U1,U2)
C     ===================================
C1                      R  R  R  R  R  R
C1                      I  I  I  I  O  O
C2    Subroutine PRDVEC calculates the dX & dY direction
C2    vectors for line  perpendicular to X1Y1 & X2Y2 ,
C2    passing through point X2Y2. The dY value returned in
C2    U1 , the dX value in U2.
C
      REAL X1,Y1,X2,Y2,X3,Y3,L1,L2,L3,PL1,PL2,PL3,U1,U2
C
      EXTERNAL CV0L14,VV00L6,VC00P4,UCU001
C
C     first calculate line vector.
      CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     find perpendicular line thro arrow vertice
      CALL VV00L6(L1,L2,L3,X2,Y2,PL1,PL2,PL3)
C     calculate a point a distance along this vector.
      CALL VC00P4(X2,Y2,10.0,PL1,PL2,PL3,X3,Y3)
C     now generate the dX & dY values for these 2 points.
      CALL UCU001(X2,Y2,X3,Y3,U1,U2)
C
      END
C
C     ---------------------------------------------------------
C
 
      SUBROUTINE QADSET(ANGLE,WX1,WY1,BLWD,BLHT,X4,Y4)
C     ================================================
C1    vartype           R     R   R   R    R    R  R
C1    iostat            I     I   I   I    I    O  O
C
C2    Sub QADSET sets text offset position  from a point
C2    according to the quadrant it is in.
C
      include   'include/ndata.inc'
C
      REAL X4,Y4,BLWD,BLHT,ANGLE,WX1,WY1
      INTEGER*4 QUADA
      EXTERNAL QUADA
C
C     find quadrant of dimension mid-angle
      GOTO ( 21,22,23,24) QUADA(ANGLE)
C     set justification for text
 21   CONTINUE
C     no change to text origin
      X4=WX1+DTOFF*PAPTOW
      Y4=WY1+DTOFF*PAPTOW
      GOTO 299
 22   CONTINUE
C     offset text oriigin in x axis Only
      X4=WX1-(BLWD+DTOFF)*PAPTOW
      Y4=WY1+DTOFF*PAPTOW
      GOTO 299
 23   CONTINUE
C     offset text oriigin in x and y axis
      X4=WX1-(BLWD+DTOFF)*PAPTOW
      Y4=WY1-(BLHT+DTOFF)*PAPTOW
      GOTO 299
 24   CONTINUE
C     offset text oriigin in Y axis only.
      X4=WX1+DTOFF*PAPTOW
      Y4=WY1-(BLHT+DTOFF)*PAPTOW
 299  CONTINUE
C
      END
C
C     ---------------------------------------------------------
C
