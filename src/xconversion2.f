C
C     @(#)  412.1 date 6/11/92 xconversion2.f 
C
C
C     Filename    : xconversion2.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:47:55
C     Last change : 92/06/11 14:44:27
C
C     Copyright : Practical Technology International Limited  
C     File :- xconversion2.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
C
C     This source file contains routines which have been modified
C     for the X conversion for daxcad. At the head of each
C     routine a comment exists to nindicate the source file form whence it
C     was extracted. This file is under sr10 SCCS
C
C
C     ******************************************************************
C
C     Extracted from insert1.f
C
C     ******************************************************************
C
      SUBROUTINE GETANS(C,X,Y)
C     ========================
C1                      I,R,R
C1                      4,R,R
C1                      O,O,O
C
C2       GETANS will be used by the user in the construction
C2       of the drawing
C
C2       GETANS'S job is to catch point definitions :
C2       'T'   Typed input            X,Y
C2       'E'   End           of       line,arc,text
C2       'C'   Centre        of       arc,circle
C2       'C'   Midpoint      of       line
C2       'I'   Intersection  of       line-line,line-arc,arc-arc
C2       'N'   Nearest point on       line,arc
C2       'D'   Reference     to       End,Centre,Mid,Inter,Nearest
C2       'Y'   Distance      along    line,arc,circle
C
C2       Getans uses TCURS for user input
C
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/ndata.inc'
      include   'include/apollo.inc'
      include   'include/macro.inc'
      include   'include/abs.inc'
      include   'include/vntable.inc'
      include   'include/journal.inc'
C
C
      INTEGER*2 TMIP(2),ENT(2)
      CHARACTER*40 INPL
C
      REAL X,Y,Z,XT1,YT1,XT2,YT2,DISTXY,DIST,DIST2,CANG,
     +      XI,YI,ZI,Q(2,2),R(2,2),RXP1,RYP1,RXP2,RYP2,
     +      SX1,SY1,SX2,SY2,AUX1,AUX2,AUX3,AUX4,RAD,RANG,INCANG,PI
      INTEGER*4 IST
      DOUBLE PRECISION DX1,DY1,DX2,DY2,DX3,DY3,DX4,DY4,ANG,
     +                 DRADUS,XP,YP,XP1,YP1,DXI,DYI,DRAD
      INTEGER*2 NOENT,LEN,I1,I2
      INTEGER*4 NCH,I,C,CP,TMEN,TCELL,RMEN,RCELL,NLEN
      LOGICAL OK,REF,CVERFY,OK1,SAME
      INTRINSIC REAL,DBLE,COS,SIN
C
      EXTERNAL TCURS,SNAPXY,ZSFLAG,GTMCLO,GTMCHI,
     +         AEXPRN,CVERFY,GTHIMC,DSE801,DEPRNT,DISTXY,
     1         DCCPAP,DCCPLP,DRAD,DCC0P4,DCC0P3,INTERS,NLEN,
     2         WRTJRN,CANG,SAME,PI
C
C      X = 0.0
C      Y = 0.0
      GANS=.TRUE.
      REF=.FALSE.
      NOENT=0
C     set the journaling points mode flag on 
      PNTMOD = .TRUE.
C
 10   CONTINUE
C
      CALL TCURS(C,X,Y)
C      WRITE(10,*) '[GETANS] CCMD= ',CCMD
C      WRITE(10,*) '[GETANS] MEN= ',MEN
C      WRITE(10,*) '[GETANS] SOURCE= ',SOURCE
C      WRITE(10,*) '[GETANS] C= ',C
C
 11   CONTINUE
C
      IF (SOURCE.EQ.1.AND.MEN.EQ.0) THEN
         IF ( SKEY.EQ.98 ) THEN
            MEN=3
            CCMD='C'
C           Highlight the 'CENTRE' cell.
            TCELL = VNPOS(43)
            CALL GTMCHI(3,TCELL)
         ELSE IF ( SKEY.EQ.99 ) THEN
            MEN=3
            CCMD='E'
C           Highlight the 'END' cell.
            TCELL = VNPOS(42)
            CALL GTMCHI(3,TCELL)
         ELSE
            CCMD=' '
         END IF
      ELSE
C        Don't reset source if it is 8
C        so that hits on popup header 
C        cells have same result if hit from TCURS
         IF(SOURCE .NE. 8) SOURCE=0
      END IF
C
      IF ( CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C            CALL ZSFLAG(.TRUE.,OK1)
            PNTMOD=.FALSE.
            GANS=.FALSE.
            RETURN
      END IF
C
      IF ( CCMD.EQ.' '.AND.SETGRD ) THEN
         CALL SNAPXY(X,Y,XI,YI)
         X=XI
         Y=YI
      END IF
C
      IF ( MEN .NE. 3 ) GOTO 100
C
      TMEN=MEN
      TCELL=CELLN
C
C *************************************************************
C     R E F E R E N C E  T O
C *************************************************************
C
C     'D' Hit   'Reference to point.'
C
      IF ( CCMD.EQ.'D') THEN
         IF ( REF ) THEN
            TCELL = VNPOS(40)
            CALL GTMCLO(3,TCELL)
            REF=.FALSE.
         ELSE
            REF=.TRUE.
            RMEN=MEN
            RCELL=CELLN
C           Tell him to hit reference point.
            CALL DCPRNT(124)
         END IF
C
         GOTO 10
      END IF
C
      IF ( CCMD .EQ. 'T' ) THEN
C *************************************************************
C        T Y P E D  I N P U T
C *************************************************************
C
C        'T' Hit   'Typed input'
C
  211    CONTINUE
C        Prompt for coord and return expression.
         CALL DPRMXP(283,INPL)
         IF ( NLEN(INPL).EQ. 0 ) THEN
C           Switch off light now we're finished
            TCELL = VNPOS(46)
            CALL GTMCLO(3,TCELL)
            GOTO 10
         END IF
         CALL XYCOOR(INPL,X,Y,Z,OK)
         IF ( .NOT. OK ) GOTO 211
         CCMD=' '
C        Switch off light now we're finished
         TCELL = VNPOS(46)
         CALL GTMCLO(3,TCELL)
         MEN=0
         GOTO 100
C
      END IF
C
      IF ( CVERFY(CCMD,'ENCIY') ) THEN
         TMEN=3
C         CALL GTHIMC(TMEN,CCMD,'ENCIY',TCELL)
C
         IF (CCMD.EQ.'E') THEN
C
C *************************************************************
C           E N D  O F  L I N E  O R  A R C  S U B - M E N U
C *************************************************************
C
C           'E' Hit. 'End of Entity Arc or Line.'
C
C           Tell him to hit entity.
 31         CONTINUE
            IF ( SOURCE .NE. 1 ) THEN
               CALL DCPRNT(230)
               CALL TCURS(C,X,Y)
C
C              User  hit cell again. Switch off. Return to start.
               IF ( CCMD .EQ. 'E' ) THEN
                   TCELL = VNPOS(42)
                   CALL GTMCLO(MEN,TCELL)
                   GOTO 10
               END IF
C              If user  hit another menu cell return to check lines.
               IF ( MEN .NE. 0 ) THEN
                   TCELL = VNPOS(42)
                   CALL GTMCLO(3,TCELL)
                   GOTO 11
               END IF
            ELSE
               MEN=0
            END IF
C
            CALL DSE801(X,Y,OK)
C
            IF ( .NOT. OK ) THEN
               CALL DEPRNT(142)
               IF ( SOURCE .EQ. 1 ) SOURCE=0
               GOTO 31
            END IF
C
C           Got the entity. Now find the end point.
            NOENT=1
            IF ( IMBUFF(2) .EQ. ARC ) THEN
C              Xcoord   + Radius  *Cos(Sang)
               XT1=RDBUFF(1)+RDBUFF(4)*COS(RDBUFF(5))
C              Ycoord   + Radius  *Sin(Sang)
               YT1=RDBUFF(2)+RDBUFF(4)*SIN(RDBUFF(5))
C              Xcoord   + Radius  *Cos(Eang)
               XT2=RDBUFF(1)+RDBUFF(4)*COS(RDBUFF(6))
C              Ycoord   + Radius  *Sin(Eang)
               YT2=RDBUFF(2)+RDBUFF(4)*SIN(RDBUFF(6))
C
               DIST=DISTXY(XT1,YT1,X,Y)
               DIST2=DISTXY(XT2,YT2,X,Y)
C
               IF ( DIST2 .LT. DIST ) THEN
                  X=XT2
                  Y=YT2
               ELSE
                  X=XT1
                  Y=YT1
               END IF
C
            ELSE IF (IMBUFF(2) .EQ. CENLIN) THEN
C              Center line found.
               CALL CLNEND(X,Y,XT1,YT1)
               X = XT1
               Y = YT1
            ELSE
C              Line found.
               XT1=RDBUFF(1)
               YT1=RDBUFF(2)
               XT2=RDBUFF(4)
               YT2=RDBUFF(5)
C
               DIST=DISTXY(XT1,YT1,X,Y)
               DIST2=DISTXY(XT2,YT2,X,Y)
C
               IF ( DIST2 .LT. DIST ) THEN
                  X=XT2
                  Y=YT2
               ELSE
                  X=XT1
                  Y=YT1
               END IF
C
            END IF
C           Switch off light now we're finished
            TCELL = VNPOS(42)
            CALL GTMCLO(3,TCELL)
C                                                           
C           write out an entry to the journal
C           file if required
C            IF(JOURON) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C
         ELSE IF (CCMD.EQ.'N') THEN
C
C *************************************************************
C           N E A R E S T     S U B - M E N U
C *************************************************************
C
C           'N' Hit
C           Nearest Entity Arc or Line
C           Tell him to hit reference point.
 21         CONTINUE
            CALL DCPRNT(124)
            CALL TCURS(C,X,Y)
C
C           User has hit the cell again swicth off and return to start
            IF ( CCMD .EQ. 'N' ) THEN
               TCELL = VNPOS(45)
               CALL GTMCLO(3,TCELL)
               GOTO 10
            END IF
C
C           If user has hit another menu cell return to check lines
            IF ( MEN .NE. 0 ) THEN
               TCELL = VNPOS(45)
               CALL GTMCLO(3,TCELL)
               GOTO 11
            END IF

C           Line
            CALL DSE801(X,Y,OK)
            IF ( .NOT. OK ) THEN
               CALL DEPRNT(142)
               GOTO 21
            END IF
            NOENT=1
            IF ( IMBUFF(2) .EQ. ARC) THEN
C              arc found
               DX1=DBLE(X)
               DY1=DBLE(Y)
               XP=DBLE(RDBUFF(1))
               YP=DBLE(RDBUFF(2))
               DRADUS=DBLE(RDBUFF(4))
               CALL DCCPAP(XP,YP,DRADUS,DX1,DY1,DX2,DY2)
               X=REAL(DX2)
               Y=REAL(DY2)
            ELSE IF (IMBUFF(2).EQ.CENLIN) THEN
C              Centre line found.
               CALL CLNEND(X,Y,XT1,YT1)
               DX1 = DBLE(XT1)
               DY1 = DBLE(YT1)
               IF (IDBUFF(4).EQ.3) THEN
C                 PCD ... But the curved portion or not?
                  RANG = MOD(RDBUFF(5),PI(1.0))
                  INCANG = MOD(CANG(RDBUFF(1),RDBUFF(2),XT1,YT1),
     +                     PI(1.0))
                  IF (.NOT. SAME(RANG,INCANG)) THEN
C                    PCD curved bit.
                     DX1=DBLE(X)
                     DY1=DBLE(Y)
                     DX2=DBLE(RDBUFF(1))
                     DY2=DBLE(RDBUFF(2))
                     DRADUS=DBLE(RDBUFF(3))
                     CALL DCCPAP(DX2,DY2,DRADUS,DX1,DY1,XP,YP)
                  ELSE
C                    PCD straight bit.
                     RAD = RDBUFF(3)
                     INCANG = 0
                     CALL NEWPNT(RDBUFF(1),RDBUFF(2),RAD,RDBUFF(3),
     +                           INCANG,RDBUFF(5),XT2,YT2)
                     DX2 = DBLE(XT2)
                     DY2 = DBLE(YT2)
                     CALL DCCPLP(DX1,DY1,DX2,DY2,DBLE(X),DBLE(Y),XP,YP)
                  ENDIF
               ELSE
C                 Cross or single centre line.
                  DX2 = DBLE(RDBUFF(1))
                  DY2 = DBLE(RDBUFF(2))
                  CALL DCCPLP(DX1,DY1,DX2,DY2,DBLE(X),DBLE(Y),XP,YP)
               ENDIF
               X=REAL(XP)
               Y=REAL(YP)
            ELSE
               DX1=DBLE(RDBUFF(1))
               DY1=DBLE(RDBUFF(2))
               DX2=DBLE(RDBUFF(4))
               DY2=DBLE(RDBUFF(5))
               CALL DCCPLP(DX1,DY1,DX2,DY2,DBLE(X),DBLE(Y),XP,YP)
               X=REAL(XP)
               Y=REAL(YP)
            END IF
C           Switch off light now we're finished
            TCELL = VNPOS(45)
            CALL GTMCLO(3,TCELL)
C                        
C           write out an entry to the journal
C           file if required
C            IF(JOURON) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C
         ELSE IF (CCMD.EQ.'Y') THEN
C
C *************************************************************
C           D I S T A N C E  A L O N G   S U B - M E N U
C *************************************************************
C
C           'Y' Hit
C           Distance along  Entity Arc or Line
C           Tell him to hit entitiy.
 20         CONTINUE
            CALL DCPRNT(230)
            CALL TCURS(C,X,Y)
C           write(10,*) '[GETANS] AFTER TCURS X= ',X,' Y= ',Y
C
C           User has hit the cell again switch off
C           and return to start
            IF ( CCMD .EQ. 'Y' ) THEN
               TCELL = VNPOS(41)
               CALL GTMCLO(3,TCELL)
               GOTO 10
            END IF
C
C           If user has hit another menu cell return to check lines
            IF ( MEN .NE. 0 ) THEN
               TCELL = VNPOS(41)
               CALL GTMCLO(3,TCELL)
               GOTO 11
            END IF
C
C           Line
            CALL DSE801(X,Y,OK)
            IF ( .NOT. OK ) THEN
                  CALL DEPRNT(142)
                  GOTO 20
            END IF
C
C           write(10,*) '[GETANS] ENTITY FOUND= ',OK,' TYPE= ',IMBUFF(2)

            NOENT=1
            IF ( IMBUFF(2).EQ. ARC ) THEN
C              Arc found.
               GOTO 223
  221          CONTINUE
               CALL DEPRNT(211)
C              prompt for Angle round arc and return expression
  223          CALL DPRMXP(213,INPL)
               IF(NLEN(INPL).EQ.0) THEN
C               IF ( INPL(1:1) .EQ. ' ' ) THEN
C                  CALL GTMCLO(TMEN,TCELL)
                  GOTO 10
               END IF
C              evaluate an arithmetic expression from the keyboard
C              Alternative return 221 for illegal input
               CALL AEXPRN(INPL,ANG,*221)
C              ANG is in degrees remember  ??????
C              Recover arc from data base
C
C              Radius of arc
               DRADUS=DBLE(RDBUFF(4))
C
               ANG= DRAD(ANG)
C
               X=RDBUFF(1) + REAL ( DRADUS*COS(ANG) )
               Y=RDBUFF(2) + REAL ( DRADUS*SIN(ANG) )
            ELSE IF (IMBUFF(2).EQ.CENLIN) THEN
C              Center line found.
               CALL CLNEND(X,Y,XT1,YT1)
               DX1 = DBLE(XT1)
               DY1 = DBLE(YT1)
               IF (IDBUFF(4).EQ.3) THEN
                  RAD = RDBUFF(3)
                  INCANG = 0
                  CALL NEWPNT(RDBUFF(1),RDBUFF(2),RAD,RDBUFF(3),
     +                        INCANG,RDBUFF(5),XT2,YT2)
                  DX2 = DBLE(XT2)
                  DY2 = DBLE(YT2)
               ELSE
                  DX2 = DBLE(RDBUFF(1))
                  DY2 = DBLE(RDBUFF(2))
               ENDIF
               GOTO 232
 231           CONTINUE
               CALL DEPRNT(231)
 232           CONTINUE
               IF (IDBUFF(4).EQ.3) THEN
C                 PCD ... But the curved portion or not?
                  RANG = MOD(RDBUFF(5),PI(1.0))
                  INCANG = MOD(CANG(RDBUFF(1),RDBUFF(2),XT1,YT1),
     +                     PI(1.0))
                  IF (.NOT. SAME(RANG,INCANG)) THEN
C                    Curved portion of a PCD
                     CALL DPRMXP(213,INPL)
                     IF(NLEN(INPL).EQ.0) THEN
                        GOTO 10
                     END IF
                     CALL AEXPRN(INPL,ANG,*231)
                     DRADUS=DBLE(RDBUFF(3))
                     ANG= DRAD(ANG)
                     X=RDBUFF(1) + REAL ( DRADUS*COS(ANG) )
                     Y=RDBUFF(2) + REAL ( DRADUS*SIN(ANG) )
                  ELSE
C                    Tell him to give distance along line: return exp.
                     CALL DPRMXP(232,INPL)
                     IF(NLEN(INPL).EQ.0) THEN
                        GOTO 10
                     END IF
                     CALL AEXPRN(INPL,ANG,*231)
                     CALL DCC0P4(DBLE(XT1),DBLE(YT1),ANG,
     +                           DX1,DY1,DX2,DY2,DX4,DY4)
                     X=REAL(DX4)
                     Y=REAL(DY4)
                  ENDIF
               ELSE
C                 Tell him to give distance along line: return exp.
                  CALL DPRMXP(232,INPL)
                  IF(NLEN(INPL).EQ.0) THEN
                     GOTO 10
                  END IF
                  CALL AEXPRN(INPL,ANG,*231)
                  CALL DCC0P4(DBLE(XT1),DBLE(YT1),ANG,
     +                        DX1,DY1,DX2,DY2,DX4,DY4)
                  X=REAL(DX4)
                  Y=REAL(DY4)
               ENDIF
            ELSE
C              Line found.
               DIST=DISTXY(RDBUFF(1),RDBUFF(2),X,Y)
               DIST2=DISTXY(RDBUFF(4),RDBUFF(5),X,Y)
C
               IF ( DIST2 .LT. DIST ) THEN
                  DX1=DBLE(RDBUFF(4))
                  DY1=DBLE(RDBUFF(5))
                  DX2=DBLE(RDBUFF(1))
                  DY2=DBLE(RDBUFF(2))
                  X=RDBUFF(4)
                  Y=RDBUFF(5)
               ELSE
                  DX1=DBLE(RDBUFF(1))
                  DY1=DBLE(RDBUFF(2))
                  DX2=DBLE(RDBUFF(4))
                  DY2=DBLE(RDBUFF(5))
                  X=RDBUFF(1)
                  Y=RDBUFF(2)
               END IF
               GOTO 224
C
  222          CONTINUE
               CALL DEPRNT(231)
C              Tell him to give distance along line: return exp.
  224          CALL DPRMXP(232,INPL)
               IF(NLEN(INPL).EQ.0) THEN
                  GOTO 10
               END IF
C              Evaluate an arithmetic expression from the keyboard.
C              Alternative return 222 for illegal input
               CALL AEXPRN(INPL,ANG,*222)
               CALL DCC0P4(DBLE(X),DBLE(Y),ANG,
     +                     DX1,DY1,DX2,DY2,DX4,DY4)
               X=REAL(DX4)
               Y=REAL(DY4)
            END IF
C           Switch off light now we're finished
            TCELL = VNPOS(41)
            CALL GTMCLO(3,TCELL)
C                        
C           write out an entry to the journal
C           file if required
C            IF(JOURON) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C
         ELSE IF (CCMD.EQ.'C') THEN
C
C *************************************************************
C           C E N T R E , M I D - P O I N T   S U B - M E N U
C *************************************************************
C
C           'C' Hit  'Centre, mid-point sub menu'
C
C           Tell him to hit reference entity.
 41         CONTINUE
            IF ( SOURCE .NE. 1 ) THEN
               CALL DCPRNT(230)
               CALL TCURS(C,X,Y)
C              User has hit the cell again; swicth off 
C              and return to start.
               IF ( CCMD .EQ. 'C' ) THEN
                  TCELL = VNPOS(43)
                  CALL GTMCLO(3,TCELL)
                  GOTO 10
               END IF
C              If user has hit another menu cell,
C              return to check lines.
               IF ( MEN .NE. 0 ) THEN
                  TCELL = VNPOS(43)
                  CALL GTMCLO(3,TCELL)
                  GOTO 11
               END IF
            ELSE
               MEN=0
            END IF
C
C
            CALL DSE801(X,Y,OK)
C
            IF ( .NOT. OK ) THEN
               CALL DEPRNT(142)
               IF ( SOURCE .EQ. 1 ) SOURCE=0
               GOTO 41
            END IF
            NOENT=1
            IF ( IMBUFF(2) .EQ. LINE ) THEN
C              Line found.
               DX1=DBLE(RDBUFF(1))
               DY1=DBLE(RDBUFF(2))
               DX2=DBLE(RDBUFF(4))
               DY2=DBLE(RDBUFF(5))
               CALL DCC0P3(DX1,DY1,DX2,DY2,XP,YP)
               X=REAL(XP)
               Y=REAL(YP)
            ELSE
C              arc or center line found.
               IF (IDBUFF(4).EQ.3) THEN
C                 A PCD center line is a little more complex.
                  RAD = RDBUFF(3)
                  INCANG = 0
                  CALL NEWPNT(RDBUFF(1),RDBUFF(2),RAD,RDBUFF(3),
     +                        INCANG,RDBUFF(5),X,Y)
               ELSE
                  X=RDBUFF(1)
                  Y=RDBUFF(2)
               ENDIF
            END IF
C
C           Switch off light now we're finished
            TCELL = VNPOS(43)
            CALL GTMCLO(3,TCELL)
C                        
C           write out an entry to the journal
C           file if required
C            IF(JOURON) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C
C
C*************************************************************
C****       START    OF    INTERSECTION    CODING         ****
C*************************************************************
C
         ELSE IF (CCMD.EQ.'I') THEN
C
C *************************************************************
C           I N T E R S E C T I O N   S U B - M E N U
C *************************************************************
C
C           'I' Hit
C           Intersection of Arc or Line
C
            NOENT=0
C           Tell him to hit entity.
C
 51         CONTINUE
            IF ( NOENT .EQ. 0 ) THEN
               CALL DCPRNT(222)
            ELSE
               CALL DCPRNT(223)
            END IF
            CALL TCURS(C,X,Y)
C           User  hit  cell again; swicth off; return to start.
            IF ( CCMD .EQ. 'I' ) THEN
               TCELL = VNPOS(44)
               CALL GTMCLO(3,TCELL)
               GOTO 10
            END IF
C           If user  hit other menu cell, return to check lines.
            IF ( MEN .NE. 0 ) THEN
               TCELL = VNPOS(44)
               CALL GTMCLO(3,TCELL)
               GOTO 11
            END IF
C           Entity search.
            IF ( NOENT.EQ.1) CALL RESET()
            CALL DSE801(X,Y,OK)
            IF ( .NOT. OK ) THEN
                  CALL DEPRNT(142)
                  GOTO 51
            END IF
C
            NOENT=NOENT+1
C           Note master index and entity type.
            TMIP(NOENT)=MIP
            ENT(NOENT)=IMBUFF(2)
C
C           Store the arc center point / line start point.
            Q(NOENT,1)=RDBUFF(1)
            R(NOENT,1)=RDBUFF(2)
C
            IF ( IMBUFF(2) .EQ.ARC ) THEN
C              Arc found.
C              Store arc hit for future use
               XT1=X
               YT1=Y
C              Store the arc radius.
               Q(NOENT,2)=RDBUFF(4)
            ELSE IF ( ENT(NOENT).EQ.CENLIN) THEN
C              Centre line found.
C              Pick an end point.
               CALL CLNEND(X,Y,XT2,YT2)
               RANG = MOD(RDBUFF(5),PI(1.0))
               INCANG = MOD(CANG(RDBUFF(1),RDBUFF(2),XT2,YT2),PI(1.0))
               IF ( IDBUFF(4).LT.3 .OR. SAME(RANG,INCANG) ) THEN
C                 Anything but the curved portion of a PCD,  turn into
C                 a line from the centre point to the end point picked
C                 and use that in the intersection calculation.
                  ENT(NOENT) = LINE
                  Q(NOENT,2) = XT2
                  R(NOENT,2) = YT2
               ELSE
C                 PCD curved bit. Turn this into an arc and use that in
C                 the intersection calculation.
                  XT1=X
                  YT1=Y
                  ENT(NOENT) = ARC
                  Q(NOENT,2) = RDBUFF(3)
               ENDIF
            ELSE
C              Store the line end point.
               Q(NOENT,2)=RDBUFF(4)
               R(NOENT,2)=RDBUFF(5)
            END IF
C
            IF ( NOENT .LT. 2 ) GOTO 51
            IF ( TMIP(1).EQ.TMIP(2) ) THEN
               CALL DEPRNT(98)
               NOENT=NOENT-1
               GOTO 51
            END IF
C
            AUX1 = Q(1,2)
            AUX2 = 0.0
            AUX3 = Q(2,2)
            AUX4 = 0.0
            CALL INTERS(ENT(1),Q(1,1),R(1,1),Q(1,2),R(1,2),AUX1,AUX2,
     +                  ENT(2),Q(2,1),R(2,1),Q(2,2),R(2,2),AUX3,AUX4,
     1                  RXP1,RYP1,RXP2,RYP2,SX1,SY1,SX2,SY2,IST,OK)
C
            IF ( .NOT. OK ) THEN
               NOENT=NOENT-1
               CALL DEPRNT(157)
C              cancel flag on second entity
C               CALL ZSFLAG(.TRUE.,OK1)
C              Go find second entity.
               GOTO 51
            END IF
C
            IF ( ENT(1).EQ.LINE .AND. ENT(2).EQ.LINE ) THEN
               X=RXP1
               Y=RYP1
            ELSE IF ( (ENT(1).EQ.LINE .AND. ENT(2).EQ.ARC) .OR.
     +                (ENT(1).EQ.ARC  .AND. ENT(2).EQ.LINE).OR.
     1                (ENT(1).EQ.ARC  .AND. ENT(2).EQ.ARC) ) THEN
               DIST = DISTXY(XT1,YT1,RXP1,RYP1)
               DIST2= DISTXY(XT1,YT1,RXP2,RYP2)
               IF ( DIST .LT. DIST2 ) THEN
                  X=RXP1
                  Y=RYP1
               ELSE
                  X=RXP2
                  Y=RYP2
               END IF
C
            END IF
C                   
C           Switch off light now we're finished
            TCELL = VNPOS(44)
            CALL GTMCLO(3,TCELL)
C                        
C           write out an entry to the journal
C           file if required
C            IF(JOURON) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C
C*************************************************************
C*****      END OF INTERSECTION CODING                   *****
C*************************************************************
         END IF
C
         CCMD=' '
      END IF
C
 100  CONTINUE
      IF ( MEN .NE. 0 ) THEN
         IF ( REF ) THEN
            TCELL = VNPOS(40)
            CALL GTMCLO(3,TCELL)
         END IF
      ELSE
C
         IF ( REF ) THEN
C
            XT1=ABSX
            YT1=ABSY
            ABSX=X
            ABSY=Y
C
 212        CONTINUE
            CALL DPRMXP(283,INPL)
            IF ( NLEN(INPL).EQ. 0 ) THEN
               CALL GTMCLO(RMEN,RCELL)
               REF=.FALSE.
               ABSX=XT1
               ABSY=YT1
               GOTO 10
            END IF
C
            CALL XYCOOR(INPL,XI,YI,ZI,OK)
            IF ( .NOT. OK ) GOTO 212
            REF = .FALSE.
C
C           Increment the position as a reference to.
            X=XI
            Y=YI
C           Switch off Ref to light now that user has input values
            TCELL = VNPOS(40)
            CALL GTMCLO(3,TCELL)
         END IF
C
      END IF
C
      IF (NOENT.GE.1) CALL RESET()

C      ensure second entity flag erased if necessary
C      IF (NOENT.EQ.2) CALL ZSFLAG(.TRUE.,OK1)
C
      IF(MEN .NE. 3 .AND. VNCCMD .NE. 92) THEN
C     
C        Don't reset global X, Y  if CONTINUOUS  menu hit
C
C        what is this pish you may ask ??????
C        well lot's of pre SR10 macros relied on 
C        the semi static nature of x, and y so 
C        so don't reset ABSX, ABSY in certain cases.
C        Unfortunately A.K. used this bug as a feature in his various 
C        MACROS, eg fastfax-1,  so patch and burn commander!!!!!
         ABSX=X
         ABSY=Y
      ENDIF
C     punt out a journal entry
C     but only if it is a graphic hit
C     if it is a menu hit the command
C     will have been journaled in TCURS
      IF(JOURON .AND. MEN .EQ. 0) CALL WRTJRN(X, Y, 'w', 'dummy', C)
C     preload variable X,Y with graphical result.
      WRITE(INPL,'(A,E14.7)')'X=',X
      CALL AEXPRN(INPL,DXI,*99)
      WRITE(INPL,'(A,E14.7)')'Y=',Y
      CALL AEXPRN(INPL,DXI,*99)
C
      IF(MACOP.AND.GINPUT) THEN
          C= 113
          GINPUT= .FALSE.
          GOTO 10
      ENDIF
      GINPUT= .FALSE.
      GANS=.FALSE.
C     set the journaling points mode flag off
      PNTMOD = .FALSE.
C
 99   CONTINUE
C
      END

C
C     ******************************************************************
C
C     Extracted from inserta.f
C
C     ******************************************************************
C
      SUBROUTINE INSCON()
C     ===================
C1    VARYPE            
C1    IOSTAT            
C
C2    This subroutine draws arcs concentric to an existing
C2    one selected by the user.
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C
      include  'include/masti.inc'
      include  'include/entity.inc'
      include  'include/menun.inc'
      include  'include/nbuff.inc'
C
      INTEGER*2 FMIPOS,SIDE,ENT
      INTEGER*4 TMEN,TCELL,C,ICOM,IMUL,INDX,CRCLS,ST,EN,I,JMP,MNUM
      REAL X1,Y1,CENTRX,CENTRY,RADIUS,SANG,EANG,RADINC,DIST
      DOUBLE PRECISION DLENGT
      CHARACTER*80 INPL
      LOGICAL OK,FIRST,MOK,CVERFY
C
      INTEGER*4 NLEN
      REAL DISTXY
      INTRINSIC REAL,INT
      EXTERNAL GTMCHI,DCPRNT,TCURS,NOSRCH,ADSRCH,DSE801,DIR500,
     +         DPRMXP,NLEN,DISTXY,CVERFY,INSATT
C
C     save pointers to menu cell which was the caller
      TMEN=MEN
      TCELL=CELLN
C                              
      ENT = ARC
      FMIPOS=NMIPOS
      FIRST=.TRUE.
C
C     ensure the caller cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C     enter the point modes to the option menu
      CALL MNLPTS()
C
C     Get the reference arc.
 10   CONTINUE
      CALL UNFLAG(.TRUE.)
      CALL DCPRNT(201)
      CALL GETANS(C,X1,Y1)
C     Hit another menu go find out what
      IF ( MEN .NE. 0 ) THEN
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel.
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
            GOTO 10
         END IF
         JMP = 1
         GOTO 300
      ENDIF
      CALL NOSRCH()
      CALL ADSRCH(ARC)
      CALL DSE800(X1,Y1,OK)
      IF ( .NOT.OK ) THEN
C         Couldn,t find any arc. Try again.
          CALL DEPRNT(108)
          GOTO 10
      ENDIF
C     Recover from data base
      CALL DIR500(MIP,OK)
      CALL DBR500(IMBUFF(7),OK)
      CENTRX = RDBUFF(1)
      CENTRY = RDBUFF(2)
      RADIUS = RDBUFF(4)
      SANG = RDBUFF(5)
      EANG = RDBUFF(6)
C            
 20   CONTINUE
C     Which side of the arc.
      CALL DCPRNT(207)
      CALL GETANS(C,X1,Y1)
C     Hit another menu go find out what
      IF ( MEN .NE. 0 ) THEN
         IF (MEN.EQ.3 .AND. CCMD.EQ.'c' ) THEN
C           Cancel.
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
            CALL ALLDRW(ENT,MIP)
            GOTO 10
         END IF
         JMP = 2
         GOTO 300
      ENDIF
C     Is the point inside or outside the arc.
      SIDE = 1
      DIST = DISTXY(CENTRX,CENTRY,X1,Y1)
      if( DIST .LT. RADIUS) SIDE = -1
C
C     Now get the distances.
100   CONTINUE
      CALL DPRMXP(208,INPL)
      CALL ALLDRW(ENT,MIP)
      IF ( NLEN(INPL).EQ.0) THEN
C        switch off since user has returned zero length
         CALL GTMCLO(TMEN,TCELL)
         GOTO 350
      END IF
C
C     Multpile circles?
      INDX = 1
50    CONTINUE
      ICOM=INDEX(INPL(INDX:),',')
      IMUL=INDEX(INPL(INDX:),'@')
      IF( (ICOM.GT.0) .AND. ((ICOM.LT.IMUL) .OR. (IMUL.EQ.0)) )THEN
C
C        Single increment then comma.
         EN = INDX + ICOM - 1
         CALL AEXPRN(INPL(INDX:(EN-1)),DLENGT,*200)
         RADIUS = RADIUS + (REAL(DLENGT) * SIDE)
         CALL INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
         FIRST = .FALSE.
         INDX = EN + 1
         GOTO 50
C
      ELSE IF((IMUL.GT.0).AND.((IMUL.LT.ICOM) .OR. (ICOM.EQ.0)))THEN
C
C        Multiple circles at same increment.
         EN = INDX + IMUL - 1
C        Get the number of circles.
         CALL AEXPRN(INPL(INDX:(EN-1)),DLENGT,*200)
         CRCLS = INT(DLENGT)
C        Get the increment.
         ST = EN+1
         EN = INDX + ICOM - 1
         IF (ICOM.LE.0) EN = NLEN(INPL) +1
         CALL AEXPRN(INPL(ST:(EN-1)),DLENGT,*200)
         RADINC = REAL(DLENGT) * SIDE
C        Draw them.
         DO 60, I=1,CRCLS
            RADIUS = RADIUS + RADINC
            CALL INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
 60      CONTINUE
         FIRST = .FALSE.
         INDX = EN + 1
         IF (ICOM.GT.0) GOTO 50
      ELSE
C
C        Single new arc.
         CALL AEXPRN(INPL(INDX:),DLENGT,*200)
         RADIUS = RADIUS + (REAL(DLENGT) * SIDE)
         CALL INSDRW(CENTRX,CENTRY,RADIUS,SANG,EANG)
         FIRST = .FALSE.
C
      ENDIF
C
      GOTO 10
C
 200  CONTINUE
      CALL DEPRNT(209)
      GOTO 100
C
 300  CONTINUE
C        process sub options in here 
C
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      IF (CVERFY(CCMD,'=fk')) THEN
C     ****************************
C        Change line attributes.  
C     ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(MEN, CELLN)                                  
         IF(JMP .EQ. 1) GOTO 10
         IF(JMP .EQ. 2) GOTO 20
      END IF
C
 350  CONTINUE
      CALL UNFLAG(.TRUE.)
      CALL GTMCLO(TMEN,TCELL)
      END



C
C     ******************************************************************
C
C     Extracted from krlib2.f
C
C     ******************************************************************
C

      FUNCTION HEX2D(C)
C     =================
C1    VARYPE       C*(*)   
C1    IOSTAT         I   
C
C2    Function HEX2D returns the decimal
C2    integer equivalent of the passed
C2    hexadecimal character C.
C2    If the character C is outwith the
C2    hex range (0-9,A-F) then, the function
C2    returns zero
C2  
C2    Arguments:-
C2  
C2    C       ->      The character string in Hexadecimal format.
C2    HEX2D   ->      The decimal number converted.
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      INTEGER*4 HEX2D
      INTEGER*4 L1
      INTEGER*4 L2
      INTEGER*4 L3
      INTEGER*4 L4
      INTEGER*4 IC
      INTEGER*4 N
C
      CHARACTER*1 C
C
C     character limits in ascii for
C     hex character range
C
      PARAMETER ( L1 = 48  )
      PARAMETER ( L2 = 57 )
      PARAMETER ( L3 = 65 ) 
      PARAMETER ( L4 = 70 ) 
C
C     get ascii value
      IC=ICHAR(C)
      IF (IC.GE.L1 .AND. IC.LE.L2) THEN
C        character is 0-9
         N=IC-L1
      ELSE IF (IC.GE.L3 .AND. IC.LE.L4) THEN
C        character is A-F
         N=IC-L3+10
      ELSE
C        out of range
         N=0
      END IF
C     return result
      HEX2D=N
C
      END
C


C
C     ******************************************************************
C
C     Extracted from plot.f
C
C     ******************************************************************
C

      SUBROUTINE OPENPF(AUTO,FOK)
C     ===========================
C1    VARYPE             L    L
C1    IOSTAT             O    O
C
C
C2    OPENPF open a plot file either
C2    supplied by the user or by
C2    creating a plot file with a unique name
C2    and number
C2    OK is returned .true. if it was successful
C2    It either sets PLTUNT to a free unit numer if
C2    it is using normal FORTRAN I/O or if a
C2    BENSON/CALCOMP which may need to be written as
C
C2
C2    Arguments:-
C2  
C2    AUTO    ->      .TRUE. Indicates that file should be spooled by external spooler
C2    FOK     ->      Indicates return state .TRUE. success
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    as FOK
C2  
C2  
      include 'include/pendat.inc'
      include 'include/vntable.inc'
C
      CHARACTER PLOUT*80
      CHARACTER TNUM*2
      INTEGER*2 NLEN2
      INTEGER*4 PLCONT
      INTEGER*4 L
      INTEGER*4 NLEN
      INTEGER*4 ST

      INTEGER*2 WMODE
      INTEGER*2 LEN
      INTEGER*2 SMODE


      LOGICAL AUTO
      LOGICAL FOK
      LOGICAL OK
      LOGICAL EX
      LOGICAL YESOK
      LOGICAL YESNO
      LOGICAL YES
      LOGICAL MAKEOK
C
      EXTERNAL NLEN
      EXTERNAL NLEN2
      EXTERNAL YESOK
      EXTERNAL MAKEOK
C 
      FOK=.FALSE.
      PLCONT=1
 
 222  CONTINUE
C     *****************************************************
C     SET SCALE FOR PLOTTER OUTPUT TO CONVERT TO INTEGER
C     *****************************************************
C
      PLOTD=.FALSE.
      IF (PLOMOD.NE.17) THEN
         CALL CONFIRMATION(DICT01(285),.TRUE.,YESNO)
      ELSE
         PLOUT = 'N'
      ENDIF
C
      IF ( .NOT.YESNO ) THEN
C
C         Plot to a named file.
C
          AUTO = .FALSE.
 221      CONTINUE
C         get the Plot File name
          CALL DPRMXP(81,PLTFIL)
          L=NLEN(PLTFIL)
          IF( L.EQ.0 ) THEN
C             Null plot name must return at once.
              GOTO 999
          ENDIF
          IF (PLOMOD.NE.17) THEN
C             Normal plot file append sheet size
              PLOUT=PLTFIL(1:L)//'.'//PLTSHT(1:2)
          ELSE
C             Plotting Interleaf, put a different ending on
              PLOUT=PLTFIL(1:L)//'.doc'
          ENDIF

		  IF (PLOMOD.EQ.14) THEN
C
C		  DPS - Add in a .PS please.  Its the way this are now
C
              L=NLEN(PLOUT)
	          PLOUT=PLOUT(1:L)//'.ps'
		  ENDIF

C          make sure name is lower case
          CALL UNFOLD(PLOUT)
C         check if plot file already exists.
          INQUIRE(FILE=PLOUT,EXIST=EX)
          IF ( EX ) THEN
              IF(MAKEOK()) THEN
C                 ok to delete the old plot file
                  CALL DELETE(PLOUT(1:NLEN2(PLOUT)),OK)
              ELSE
C                 go back to get another name 
                  GOTO 221
              ENDIF
          ENDIF
C
          IF (PLOMOD.EQ.10) THEN
C             This for Off line plotting of Benson or Calcomp Plotters
              SMODE = 0
              WMODE = 6
              LEN = NLEN(PLOUT)
              CALL STREAM_OPEN(PLOUT,LEN,WMODE,SMODE,STRID,ST)
          ELSE
C             valid filename so go get unit number.
C             go find a unit for the file.
              CALL FINDU2(PLTUNT,PLOUT,OK)
              IF ( .NOT. OK ) THEN
                  CALL DEPRNT(287)
                  GOTO 999
              ENDIF
C             Open a sequential access file using default values.
              OPEN(UNIT=PLTUNT,
     +             FILE=PLOUT,
     +             STATUS='NEW',
     +             FORM='FORMATTED',
     +             IOSTAT=ST,
     +             ERR=100)
          ENDIF
      ELSE
C
C         Plot to a file in the plotter.pcf file
C
C
          AUTO = .TRUE.
C
 232      CONTINUE
          L=NLEN(PLTDIR)
          IF (L.EQ.0) THEN
C             modify plotfile name
              PLTDIR = './daxcad_plot_file_'
          ENDIF

          WRITE(PLOUT,'(A,I2)') PLTDIR(1:NLEN(PLTDIR)),PLTNO
		  if (PLOMOD.EQ.14) THEN
C
C		  DPS - Add in a .PS please.  Its the way this are now
C
              L=NLEN(PLOUT)
	          PLOUT=PLOUT(1:L)//'.ps'
		  ENDIF
C         modify name to lower case
          CALL CRUNCH(PLOUT)
          CALL UNFOLD(PLOUT)
C
C         get next plot number
          PLTNO=MOD(PLTNO,99)+1
C
C
          IF ( PLCONT .LT. 99 ) THEN
C             check if plot file already exists.
              INQUIRE(FILE=PLOUT,EXIST=EX)
              IF ( EX ) THEN
C                 Plot file allready exists try again
                  PLCONT=PLCONT+1
                  GOTO 232
              ENDIF
C             go find a unit for the file.
              CALL FINDU2(PLTUNT,PLOUT,OK)
              IF ( .NOT. OK ) THEN
C                 cannot find a unit number
                  CALL DEPRNT(287)
                  GOTO 999
              END IF
              IF (PLOMOD.NE.10) THEN
C                 Open a sequential access file using default values
                  OPEN(UNIT = PLTUNT,
     +                 FILE=PLOUT,
     +                 STATUS='NEW',
     +                 IOSTAT=ST,
     +                 ERR=100)
              ELSE
C                 set plotd so that the benson routines use stream calls
C                 to output the data instead of fortran writes.
                  PLOTD=.TRUE.
                  SMODE = 0
                  WMODE = 6
                  LEN = NLEN(PLOUT)
                  CALL STREAM_OPEN(PLOUT,LEN,WMODE,SMODE,STRID,ST)
              ENDIF
          ELSE
              CALL DEPRNT(288)
              GOTO 999
          ENDIF
      ENDIF
 233  CONTINUE
C
C     save global plot file
      PLTFIL = PLOUT
C     return with status ok to continue plotting.
      FOK=.TRUE.
C
      RETURN
 100  CONTINUE
C     Plot File open error
      CALL DEPRNT(290)
999   CONTINUE
      FOK=.FALSE.

C
      END
C

C
C     ******************************************************************
C
C     NEW ROUTINE
C
C     ******************************************************************
C
      SUBROUTINE DAXPLOT(PLOTFILE,PLOTNUMBER,PLOTCOPIES, ST)
C     ==========================================
C1    VARYPE               C*(*)     I4      I4     I4
C1    IOSTAT                 I       I       I4     O
C
C2    Invokes the DAXCAD plotter script. This allows the particular system
C2    in operation to invoke the required plot spooler.
C2  
C2    Arguments:-
C2  
C2    PLOTFILE        ->          The full pathname of the file
C2    PLOTNUMBER       ->         The current plotting mode. See INTPLT for more info
C2    PLOTCOPIES       ->         Number of copies
C2  
C2  
C2    Error Returns:
C2  
C2    1       ->          Could not find the invoke script.
C2    2       ->          Error generated during invoke
C2  
C
      INTEGER*4 ST
      INTEGER*4 PLOTNUMBER
      INTEGER*4 PLOTCOPIES
      INTEGER*4 LENGTH
      INTEGER*4 STATUS
      INTEGER*4 NLEN
      CHARACTER PLOTFILE*(*)
      CHARACTER EXEC*1024
C
      EXTERNAL NLEN
C
      ST = 0
C
      WRITE(EXEC,FMT='(3A,I2,A,I2,A)') 
     +                     '"/daxcad/daxcad_plotter_script ',
     +                     PLOTFILE(1:NLEN(PLOTFILE)),' ',
     +                     PLOTNUMBER,' ',
     +                     PLOTCOPIES,' "'
      PRINT*, 'DAXCAD Plotter script invoking ',EXEC(1:NLEN(EXEC))
      CALL SHELLP(EXEC,STATUS)

      IF ( STATUS.GT.0) THEN
          ST = 2 
      ENDIF
      END

C
C     ******************************************************************
C
C     Extracted from plot1.f
C
C     ******************************************************************



      SUBROUTINE PLTTXT(X1,X2,X3,X4,X5,X6,TEXT)
C     =========================================
C1    VARYPE            R  R  R  R  R  R   C*(*)
C1    IOSTAT            I  I  I  I  I  I    I
C
C2     This routine uses the passed Info
C2     to write to the plot file a text string.
C2  
C2    Arguments:-
C2  
C2    X1,X2              ->          The text origin
C2    X3                 ->          The text width
C2    X4                 ->          The text height
C2    X5                 ->          The angle of text
C2    X6                 ->          The slant,justification & no chars
C2    TEXT               ->          Actual text string
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include   'include/pendat.inc'
      include   'include/params.inc'
      include   'include/ndata.inc'
      include   'include/wtov.inc'
      include   'include/wrtinlf.inc'
      INCLUDE   'include/nbuff.inc'

C
      CHARACTER OLINE*120
      CHARACTER TEXT2*120
      CHARACTER OLIN1*80
      CHARACTER TEXT*80
      CHARACTER TEMP1*1
      CHARACTER TEMP2*1
      CHARACTER TEXT1*80
C
      INTEGER*4 NCHAR4
      INTEGER*4 I
      INTEGER*4 II
      INTEGER*4 NLEN
      INTEGER*4 NLEN1
      INTEGER*4 SPECC
      INTEGER*4 ITEXT(80)
      INTEGER*4 NUM1
      INTEGER*4 NUM2
      INTEGER*4 NUM3
      INTEGER*4 NUM4
      INTEGER*4 CRC
      INTEGER*4 I4
      INTEGER*4 LOOP
C
      INTEGER*2 TJUST
      INTEGER*2 NCHAR
      INTEGER*2 TSLT
      INTEGER*2 TMIP1
      INTEGER*2 POINT
      INTEGER*2 J
      INTEGER*2 INTEQ
C
      REAL  X1,X2,X3,X4,X5,X6
      REAL  DIST1,DIST2
      REAL  DISTXY
      REAL  XT1,YT1,XT2,YT2
      REAL  HT,WD
      REAL  ANGX,ANGY
      REAL  ANG
      REAL  ASPECT
      REAL  SCAL
      REAL  SLTR
      REAL  TX1,TY1
      REAL  RAD
      REAL  XO,YO
      REAL  XX1,YY1
      REAL  RNUM
C
      LOGICAL OK
      LOGICAL PLUMIN
      LOGICAL LTEMP
C
      EXTERNAL NLEN
      EXTERNAL NLEN1
      EXTERNAL DISTXY
C
C
C---  ANGY=SIN(ANG) , ANGX=COS(ANG) WHERE ANG IS ANGLE
C---  FROM HORIZONTAL. DEFAULT CONDITION IS HORIZONTAL
C---  TEXT , AS DETAILED ABOVE
C
          
C     save hard or soft text mode
      LTEMP=HARD
C
CGCU !      IF ( PLOMOD.EQ.14 .OR.PLOWIN) THEN
      IF ( PLOWIN ) THEN
C         force software text under these condiditons
          HARD=.FALSE.
      ENDIF
C 
C         get DAXCAD text information
C
      CALL UCODET(X6,SLTR,TJUST,NCHAR)
C         
      IF ( HARD ) THEN
C
C         Hardware Text to be generated
C
C
222       CONTINUE
C         set temp string to find any CR characters
          TEXT1 = TEXT
C
C         This is for CR inbedded in string. We will loop back 
C         to continue plotting the string. In the meantime we simply calculate 
C         the new position of the string
C
          CRC=INDEX(TEXT1,CHAR(13))
          IF ( CRC.EQ.1 ) THEN
C             CR is first character.
              X2=X2-(PAPTOW*1.5*COS(RAD(X5+TEXANG))*X4)
              X1=X1+(PAPTOW*1.5*SIN(RAD(X5+TEXANG))*X4)
              TEXT = TEXT1(CRC+1:)
              GOTO 222
          ELSE IF ( CRC .GT. 1 ) THEN
C             strip of text to be displayed
              TEXT = TEXT1(:CRC-1)
              NCHAR=CRC
          END IF
C         Find the bottom left corner of the text
          CALL WO2PL(X1,X2,TX1,TY1)
          CALL XYNORG(TX1,TY1,TJUST,X5+TEXANG,SLTR,NCHAR,X3,X4,XO,YO)
C         calculate origin
          XO=TX1+(XO-TX1)*FACT*PAPSCL
          YO=TY1+(YO-TY1)*FACT*PAPSCL
C
C         divide units by 10 because they are specified in cm on the
C         plotter but we store in mm
          WD=PAPSCL*(X3/10.0)
          HT=PAPSCL*(X4/10.0)
C
          IF ( ABS(HT) .LT. LIM ) THEN
C             Text cannot be seen so come out
              GOTO 999
          ENDIF
C
C         clear special text field
          TEXT1 = ' '
C         check to see if special character exists
C         first set the special character flags to false
          DO 10 I=1,NLEN1(TEXT)
              SPECC = MOD( ICHAR(TEXT(I:I))+256,256 )
              IF(SPECC.GT.127) THEN
                  TEXT1(I:I)=TEXT(I:I)
                  TEXT(I:I)=' '
              END IF
 10       CONTINUE
C
C         Determine plotter type and output text.
C
C                1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
          GOTO ( 1,1,1,2,1,1,3,1,1,4,5,1,1,8,1,6,7,1) PLOMOD
C
 1        CONTINUE
C         H.P. 7475,7885B,GOULD 6320 plotters
            SLTR=RAD(SLTR)
            SLTR=TAN(SLTR)
            ANGX=COS(RAD(X5+TEXANG))
            ANGY=SIN(RAD(X5+TEXANG))
C---        NOW OUTPUT STRING OF TEXT TO LINE
            WRITE (OLINE,'(5(A,F9.6),A,2(A,I7),A)' )
     +     'SI',WD*2.0/3.0,',',HT,';DI',ANGX,',',ANGY,
     1     'SL',SLTR,';','PU',NINT(XO),',',NINT(YO),';'
            CALL OUTOLL(.TRUE.,OLINE)
            WRITE (PLTUNT,'(4A) ')
     2     'LB',TEXT(1:NLEN1(TEXT)),CHAR(3),';'
            GOTO 100
 2        CONTINUE
C*****************************************************************
C                                               TIM100   ** BENSON
C*****************************************************************
C           Benson 1625-S plotter
            IF ( NCHAR.LT.40 )    TEXT(NCHAR+1:)='
     +               '
            LOOP=4*(NCHAR/4)+1
            LOOP=MIN(LOOP,80)
            J=1
            DO 23 I=1,LOOP,4
               TEMP1=TEXT(I:I)
               NUM1=ICHAR(TEMP1)
               TEMP1=TEXT(I+1:I+1)
               NUM2=ICHAR(TEMP1)
               TEMP1=TEXT(I+2:I+2)
               NUM3=ICHAR(TEMP1)
               TEMP1=TEXT(I+3:I+3)
               NUM4=ICHAR(TEMP1)
               ITEXT(J)=NUM1*2**24+NUM2*2**16+NUM3*2**8+NUM4
               J=J+1
 23         CONTINUE
            ASPECT=WD/HT
            CALL TPARMB(ASPECT,SLTR)
            I4=NCHAR
            CALL SYMBOB(XO,YO,HT,ITEXT,X5+TEXANG,I4)
            GOTO 100
C
 4       CONTINUE
C*****************************************************************
C                                              ISDP      ** BENSON
C*****************************************************************
C           Benson 1625-S plotter
            IF ( NCHAR.LT.40 )    TEXT(NCHAR+1:)='
     +               '
            LOOP=4*(NCHAR/4)+1
            LOOP=MIN(LOOP,80)
            J=1
            DO 24 I=1,LOOP,4
               TEMP1=TEXT(I:I)
               NUM1=ICHAR(TEMP1)
               TEMP1=TEXT(I+1:I+1)
               NUM2=ICHAR(TEMP1)
               TEMP1=TEXT(I+2:I+2)
               NUM3=ICHAR(TEMP1)
               TEMP1=TEXT(I+3:I+3)
               NUM4=ICHAR(TEMP1)
               ITEXT(J)=NUM1*2**24+NUM2*2**16+NUM3*2**8+NUM4
               J=J+1
 24         CONTINUE
            ASPECT=WD/HT
            CALL TPARM(ASPECT,SLTR)
            I4=NCHAR
            CALL SYMBOL(XO,YO,HT,ITEXT,X5+TEXANG,I4)
            GOTO 100
C
 3       CONTINUE
C*****************************************************************
C                                                       ** CALCOMP
C*****************************************************************
C           CALCOMP 1044 plotter
            IF ( NCHAR.LT.80 )    TEXT(NCHAR+1:)='
     +               '
            J=1
            DO 25 I=1,NCHAR,2
               TEMP1=TEXT(I:I)
               TEXT(I:I)=TEXT(I+1:I+1)
               TEXT(I+1:I+1)=TEMP1
 25         CONTINUE
            ASPECT=WD/HT
            CALL TPARMC(HT,ASPECT,SLTR)
            CALL SYMBOC(XO,YO,ASPECT,TEXT,INTEQ,X5,NCHAR)
            GOTO 100
 5       CONTINUE
C*****************************************************************
C                                                        ** GERBER
C*****************************************************************
          CALL FOLDUP(TEXT)
          WRITE(OLINE,FMT='(A,I4.4,3A)')
     +    'G56W',NINT(X5),'000D10',TEXT(1:NLEN(TEXT)),'*'
          IF (OLINE(5:5).EQ.' ') OLINE(5:5)='0'
          WRITE(PLTUNT,FMT='(A)') OLINE(1:NLEN(OLINE))
          GOTO 100
 6       CONTINUE
C*****************************************************************
C                                                   ** HOUSTON DMP
C*****************************************************************
C         Set the plotter pen to the beginning of
C         the text string
          WRITE(OLINE,FMT='(2(A,I12))')
     +      'U',NINT(XO),',',NINT(YO)
          CALL OUTOLL(.FALSE.,OLINE)
c         Set height and width
          WRITE(OLINE,FMT='(2(A,I3),3(A))')
     +         'S(S',NINT(HT*10*FACT/7),',W',
     +               NINT(WD*10*FACT/6),')',
     +        TEXT(1:NLEN1(TEXT)),CHAR(3)
          WRITE(PLTUNT,FMT='(A)') OLINE(1:NLEN(OLINE))
 
          GOTO 100
 7       CONTINUE
C*****************************************************************
C                                                     ** Interleaf
C*****************************************************************
C         NOTE: Interleaf has 3 forms of text 
C       "Text" string - INTFLG = 'T'
C                       Can only rotate by 90 degree steps (The
C                       rotation angle does not need to be 90,
C                       Interleaf will round it.)
C"Convert-to-Outline" - INTFLG = 'O'
C                       The most acurate representation (except the
C                       aspect ratio) but the text cannot be changed
C                       within Interleaf.
C     "Microdocument" - INTFLG = 'M'
C                       The least accurate representation. Text does
C                       not rotate or scale to size, but it is fully
C                       editable within Interleaf.
          NUM1=NLEN(TEXT)
          IF (NUM1.LT.1) GOTO 100
          X5 = X5 + ROTTXT
          IF (INTFLG.EQ.'M') THEN
C            Creating A Micro Document
C            The first line, with all the numbers.
CCCCC             XX1 = XO - (WD * NUM1/3)
             XX1 = XO
             YY1 = 0 - (YO + (HT/2))
             WRITE(OLINE,FMT='(2(A,F8.2),A)') '(T12,1,12,',XX1,',',
     +                    YY1,',5,127,5,7,127,8,0,3,'
             CALL CRUNCH(OLINE)
             WRITE(PLTUNT,FMT='(A)') OLINE(1:NLEN(OLINE))
C            The second line, with page sizes.
             XX1 = WD * NUM1
             YY1 = HT
             WRITE(OLINE,FMT='(2(A,F8.2),A)') '<!Page, Width = ',XX1,
     +                    ' inches, Height = ',YY1,' inches>'
             WRITE(PLTUNT,FMT='(A)') OLINE(1:NLEN(OLINE))
C            The next two lines are constant.
             WRITE(PLTUNT,FMT='(A)') '<para>'
C            Then output the text string iteslf
             WRITE(PLTUNT,FMT='(A)') TEXT(1:NLEN(TEXT))
C            close document
             WRITE(PLTUNT,FMT='(A)') '<End Text>)'
          ELSE
C            Text strings or outline text.
C            First, prefix all special characters with back slashes.
             II=1
             TEXT2 = ' '
             NUM2 = INT(HT * 40)
             IF (NUM2.LE.0) NUM2 = 1
             DO 77 I=1,NUM1
C               Special characters are < > ( ) , \ and SPACE.
                IF ( TEXT(I:I).EQ.'<' .OR. TEXT(I:I).EQ.'>' .OR.
     +               TEXT(I:I).EQ.'(' .OR. TEXT(I:I).EQ.')' .OR.
     +               TEXT(I:I).EQ.' ' .OR. TEXT(I:I).EQ.',' .OR.
     +               TEXT(I:I).EQ.'\\' ) THEN
                   TEXT2(II:II) = '\\'
                   II=II+1
                   IF (II.EQ.120) GOTO 777
                ENDIF
                TEXT2(II:II) = TEXT(I:I)
                II=II+1
                IF (II.EQ.120) GOTO 777
 77          CONTINUE
C            Now build the output string.
 777         CONTINUE
             IF (INTFLG.EQ.'T') then
C               Text Strings
                ANG=90*INT(X5/90)
                IF ((X5-ANG).GE.45.0) ANG=ANG+90
                XO=XO+(COS(RAD(ANG))*WD*NUM1/6.0)
                YO=YO+(SIN(RAD(ANG))*WD*NUM1/6.0)
                WRITE(OLIN1,FMT='(3(A,F8.2),A,I3,A)') '(t14,1,0,',XO,
     +                        ',',-YO,',1,7,0,',-X5,
     +                        ',,wst:swiss',NUM2,','
             ELSE
C               Convert-to-Outline.
                WRITE(OLIN1,FMT='(3(A,F8.2),A,I3,A)') '(o4,1,12,',XO,
     +                     ',',-YO,',',RAD(-X5),',wst:swiss',NUM2,','
             ENDIF
             CALL CRUNCH(OLIN1)
             WRITE(OLINE,FMT='(3(A))') OLIN1(1:NLEN(OLIN1)),
     +                                 TEXT2(1:NLEN(TEXT2)),')'
C            And finally, write it out.
             WRITE(PLTUNT,FMT='(A)') OLINE(1:NLEN(OLINE))
          ENDIF
          GOTO 100
C
 8        CONTINUE
C*****************************************************************
C	PostScript
C*****************************************************************
            I=IMBUFF(12)
            CALL CHGTHK(I)
            SLTR=RAD(SLTR)
            SLTR=TAN(SLTR)
C Change font if necessary
C
C		DPS - This might work woth a printer but not Acrobat.
C		Force the hardware font to be Courier - it makes sense
C
	    IF (X3.NE.LPTX3 .OR. X4.NE.LPTX4 .OR. SLTR.NE.LPTSLT) THEN
		WRITE (OLINE,'(A,3(F9.2,A))') '/Courier_findfont_[',
     +		X3,'_0_',X4*SLTR,'_',X4,'_0_0]_makefont_setfont'
		CALL CRUNCH(OLINE)
		CALL STRIPO(OLINE,'_',' ')
		WRITE (PLTUNT,'(A) ')  OLINE(1:NLEN1(OLINE))
		LPTX3=X3
		LPTX4=X4
		LPTSLT=SLTR
	    END IF
C---        NOW OUTPUT STRING OF TEXT TO LINE
            WRITE (OLINE,'(2(F9.2,A))') XO,'_',YO,'_moveto'
            CALL CRUNCH(OLINE)
            CALL STRIPO(OLINE,'_',' ')
            WRITE (PLTUNT,'(A) ')  OLINE(1:NLEN1(OLINE))
	    IF (ABS(X5+TEXANG).GT.1.0D-12) THEN
		WRITE (PLTUNT,'(A)') 'gsave'
		WRITE (PLTUNT,'(F9.2,A)') X5+TEXANG,' rotate'
	    END IF
            WRITE (PLTUNT,'(3A)') '(',TEXT(1:NLEN1(TEXT)),') show'
	    IF (ABS(X5+TEXANG).GT.1.0D-12) THEN
	            WRITE (PLTUNT,'(A)') 'grestore'
	    END IF
            GOTO 100


 100      CONTINUE
C
C                              SPECIAL CHARACTERS
C         Were any special characters encountered Have to generate them ourselves
C
          IF(NLEN(TEXT1).GT.0) THEN
C             software text overwrite of existing plotfile.
              CALL SFFTXT(.FALSE.,1,X1,X2,X3,X4,X5,X6,TEXT1)
         END IF
C
         IF ( CRC .NE. 0 ) THEN
C          must loop back and get the rest of the string with the CR chars
           X2=X2-(PAPTOW*1.5*COS(RAD(X5+TEXANG))*X4)
           X1=X1+(PAPTOW*1.5*SIN(RAD(X5+TEXANG))*X4)
           TEXT=TEXT1(CRC+1:)
           GOTO 222
         END IF
      ELSE
C
C       Software text as requested by user or constrained by plotter type
C
        CALL SFFTXT(.FALSE.,1,X1,X2,X3,X4,X5,X6,TEXT)
      END IF
      CALL WO2PL(X1,X2,TX1,TY1)
      CALL TORGBR(TX1,TY1,TJUST,X5+TEXANG,SLTR,NCHAR,X3,X4,XO,YO)
      XO=TX1+(XO-TX1)*FACT
      YO=TY1+(YO-TY1)*FACT
C
999   CONTINUE
C     set current plotting origin and reset hard mode if constrained by DAXCAD
      AXP=XO
      AYP=YO
      HARD=LTEMP
C
      END

C
C     ******************************************************************
C
C     Extracted from polyclip.f
C
C     ******************************************************************


      SUBROUTINE POLY_CIRCLE(RADIUS,CX,CY,NX,NY,NOE)
C     =============================================
C1    VARTYPE                I4    I4 I4  I4  I4 I4
C1    IOSTAT                 I     I  I   O   O  O
C
C2    This routine will gen a circular polygon for thick lines
C
C2  
C2    Arguments:-
C2  
C2    RADIUS          ->          radius of the circle
C2    CX CY           ->          Center point of the circle
C2    NX,NY           ->          Polygon X and Y 
C2    NOE             ->          Number of segment in the polygon
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  

      INTEGER*2 NX(200),NY(200)
      INTEGER*2 NOE
C
      INTEGER*4 CY,CX
      INTEGER*4 COUNT
      INTEGER*4 IDIST
      INTEGER*4 I
C
      REAL SRAD
      REAL RAD
      REAL RADS
      REAL RADIUS
      REAL ANG
C
      COUNT = MIN((0.4*RADIUS),200.0)
C     find included angle.
      RADS = 6.28318/COUNT
      DO 60 I=1,COUNT
          ANG = ANG+RADS
          NX(I) = COS(ANG)*RADIUS+CX
          NY(I) = SIN(ANG)*RADIUS+CY
60    CONTINUE
      NOE = COUNT
      END

C
C     ******************************************************************
C
C     Extracted from polyclip.f
C
C     ******************************************************************



      FUNCTION POLY_SIDE(XP,YP,XMIN,YMIN,XMAX,YMAX)
C     =============================================
C1    VARTYPE            I4 I4  I4   I4   I4   I4
C1    IOST               I  I   I    I    I    I
C
C2    The side number of the point is returned in this routine
C2  
C2                        2
C2            --------------------------
C2            |                        |
C2            |                        |
C2            |                        |
C2            |                        |
C2        1   |                        |  3
C2            |                        |
C2            |                        |
C2            |                        |
C2            |                        |
C2            |                        |
C2            --------------------------
C2                        4
C2  
C2    Arguments:-
C2  
C2    XP,YP                           ->      Point on the box
C2    XMIN,YMIN,XMAX,YMAX             ->      the box
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    If the point does not lie on the box the function returns 0
C2  
C2  
C
      INTEGER*4 POLY_SIDE
      INTEGER*4 XP,YP
      INTEGER*4 XMIN,XMAX
      INTEGER*4 YMIN,YMAX
      INTEGER*4 SIDEN
C
      IF(XMIN.EQ.XP) THEN
          SIDEN = 1
      ELSEIF(XMAX.EQ.XP) THEN
          SIDEN = 3
      ELSEIF(YMIN.EQ.YP) THEN
          SIDEN = 4
      ELSEIF(YMAX.EQ.YP) THEN
          SIDEN = 2
      ELSE
          SIDEN = 0
      ENDIF
C
      POLY_SIDE = SIDEN
 
      END

C
C     ******************************************************************
C
C     Extracted from polyclip.f
C
C     ******************************************************************
C

      SUBROUTINE RDXRLY(OUNIT,LINEP,GC,GV,OK)
C     =======================================
C1    VARTYP            I4  I4    I4  C*80 L
C1    IOSTAT            I   I     I    I   O
C
C2    This routine reads the layers and bulids up a layer
C2    array of names. The name will corespond to the subscript
C2    value of the array and this will be the nunber of the daxcad
C2    layer
C2  
C2    Arguments:-
C2  
C2    OUNIT           ->          Current unit number of dxf file
C2    LINEP           ->          Current logical line pointer of file 
C2    GC              ->          Graphics code
C2    GV              ->          Graphics value
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK   .TRUE.     Read layer table properly
C2  
C2  
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
      include 'include/style.inc'
      include 'include/wtov.inc'
      include 'include/dxf.inc'
C
      CHARACTER GV*80
      CHARACTER STYLE(10)*10
      CHARACTER LYNAM*20
      CHARACTER TLS*10
      CHARACTER TEMP*10
      INTEGER*4 LINEP
      INTEGER*4 OUNIT
      INTEGER*4 FN
      INTEGER*4 GC
      INTEGER*4 DNUM
      LOGICAL OK
      LOGICAL EOF
      LOGICAL DELETE
      LOGICAL NEWLAY
      LOGICAL CSET
      LOGICAL LSET
      INTEGER*2 CURP
      INTEGER*2 TLAY
      INTEGER*2 TCOL
      INTEGER*2 I
      INTEGER*2 TCOLOR(15)
C
C     set data for program
      DATA STYLE/'CONTINUOUS','HIDDEN','DASHED','DASHED','CENTER',
     +           'CENTER','PHANTOM','DOT','DASHDOT','DIVIDE'/
C
      DATA TCOLOR/1,3,5,4,2,6,7,3,11,1,9,5,13,1,8/
C
      TCOL = 1
      LSET = .FALSE.
      CSET = .FALSE.
      NEWLAY=.FALSE.
C
C     set default layer linestyle and color just in case
C
      TLS=STYLE(1)
20    CONTINUE
      CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(.NOT.OK) RETURN
      IF(EOF) THEN                    
          DNUM = 672
          CALL DEPRNT(DNUM)
          RETURN
      ENDIF
 60   CONTINUE
      IF ( GC.EQ.0.AND.GV.EQ.'LAYER'.AND.(.NOT.NEWLAY)) THEN
          NEWLAY=.TRUE.
      ELSEIF( GC.EQ.2.AND.NEWLAY) THEN
          LYNAM=GV
      ELSEIF( GC.EQ.62.AND.NEWLAY) THEN
          READ(UNIT=GV,FMT='(I10)',ERR=300) TCOL
          TCOL = ABS(TCOL)
      ELSEIF( GC.EQ.6.AND.NEWLAY) THEN
          TLS=GV(1:10)
      ELSEIF( GC.EQ.0) THEN
C         if a layer has not been signalled go and stop the read
C         increment the number of layers
C         set the global line name
          LNAME(NUMLAY)=LYNAM
C         set the default syle
          LSTYLE(NUMLAY)=1
C         get the line style name
          DO 50 I=1,10
             IF(STYLE(I).EQ.TLS) THEN
C                set the subscript value of the line style
                 LSTYLE(NUMLAY)=I
                 LSET = .TRUE.
                 GOTO 95
             ENDIF
 50       CONTINUE
C         set the color values also
C         The default color is 7 (white)
 95       CONTINUE
C         code the color and line style into the same
C         number. The LS byte is the line style and the MS byte is
C         surprisingly the color.
          DO 70 I=1,15
 
             IF(TCOL.EQ.TCOLOR(I)) THEN
 
                 TCOL = I
                 TCOL=TCOL*256
                 CSET = .TRUE.
                 LSTYLE(NUMLAY)=LSTYLE(NUMLAY)+TCOL
 
             ENDIF
 
 70       CONTINUE
 
          IF(.NOT.CSET) THEN
              LSTYLE(NUMLAY)=LSTYLE(NUMLAY) + TCOL*256
          ENDIF
C         increment the layer number
          NUMLAY=NUMLAY+1
          IF(NUMLAY.GT.255) THEN
C            If there are more than 255 layers the scrap it.
             DNUM = 681
             CALL DEPRNT(DNUM)
             NUMLAY=255
             RETURN
          ENDIF
          IF(GV.EQ.'ENDTAB') THEN
             OK=.TRUE.
C            decremnt it since we started on layer 1
             NUMLAY=NUMLAY-1
             RETURN
          ELSE
             NEWLAY=.FALSE.
C            reset default line style
             TLS=STYLE(1)
             GOTO 60
          ENDIF
      ENDIF
      GOTO 20
300   WRITE(UNIT=GV,FMT='(A,I7)') 'Wrong layer control format',LINEP
      CALL EPRINT(GV)
      OK=.FALSE.
      END

      SUBROUTINE DIAG(ST)
C     ===================
C1    VARYPE          I4
C1    IOSTAT          O
C
C2    Creates and opens a diagnostic file for this session
C2    The file is opened and deleted when the system closes
C2
C2  
C2    Arguments:-NONE
C2  
C2  
C2    Error Returns:
C2  
C2    0		->	Success
C2    -1	-> 	No diagnotic file opened
C2  
C
C
      INTEGER*4 ST
      INTEGER*4 TIM(6)
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      CHARACTER*82 DIAGNM
      CHARACTER*82 TEMP
      include 'include/product.inc'
C
      EXTERNAL NLEN
C
C
C     Get tempoery file
C
      CALL GETMPFILE(DIAGNM,80,LENGTH,ST)
C
      IF ( ST.NE.0 ) THEN
C
C	   Error
C
           RETURN
C
      ENDIF
C
      TEMP = DIAGNM(1:LENGTH)
      OPEN(UNIT=10,FILE=TEMP,STATUS='NEW',
     +             IOSTAT=ST,ERR=101)
      WRITE(10,'(A)',ERR=101,IOSTAT=ST) 
     + 'Practical Technology International Ltd 1991'
      WRITE(10,'(A)',ERR=101,IOSTAT=ST) 'System Diagnostics File'
      WRITE(10,'(2A)',ERR=101,IOSTAT=ST) 
     +      'Product: ',PRODUCT(1:NLEN(PRODUCT))
      CALL LOCALT(TIM)
      WRITE(10,'(2(A,2(I2.2,A),I4))')
     +     'DATE: ',TIM(3),'/',TIM(2),'/',TIM(1),
     + '    TIME: ',TIM(4),':',TIM(5),':',TIM(6)
      ST = 0
      RETURN
101   CONTINUE
      END

      SUBROUTINE GTINGT(REPAINT,ST)
C     =============================
C1    VARYPE              L     I4
C1    IOSTAT              O     O
C
C2    Formerly GTXT_INIT_GTXT initializes the GTXT system of
C2    APOLLO graphics text handlers. This routine must be
C2    called before calling any of the GTXT operations. All
C2    system variables are initialized within this sub-routine.
C2        All working data space for the GTXT library is declared
C2    in the two common blocks referenced in the following INCLUDE
C2    statements.Neither of these common blocks are required by
C2    programs which call GTXT routines in the normal way.
C2  
C2    Arguments:-
C2  
C2    REPAINT     ->      Specifies that a repaint is taking place
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include 'include/gtxt1.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*2 I,J
      INTEGER*4 Z
      INTEGER*4 ADESC
      INTEGER*4 MDESC
      INTEGER*4 ST
C
      LOGICAL REPAINT
C
C     Clear all variables to default conditions
C
      Z=0
C
      GNLINE = ' '
C
      DO 100 I=1,4
C         Initalise all 4 windows and menus 
C
          IF( .NOT.REPAINT ) THEN
C             These variablers do not get reset during repaint
              DO 120 J=0,31
                  GWB(I,J) = GNLINE
                  GMB(I,J) = GNLINE
                  GMBC(I,J) = GNLINE
                  GMCINV(I,J)=.FALSE.
 120          CONTINUE
          ENDIF
C         set to zero 
          GTCMDS(I) = 0
          GTCADS(I) = 0
C
          GWBP(I) = Z
          GWLINE(I)=GNLINE
          GMA(I) = .FALSE.
          DO 110 J=1,6
              GWL(I,J)=0
              GML(I,J)=0
 110      CONTINUE
          DO 140 J=1,5
              GWC(I,J)=0
              GMC(I,J)=0
 140      CONTINUE
          DO 150 J=1,2
              GWA(I,J)=.FALSE.
 150      CONTINUE
 100  CONTINUE
C
      END



