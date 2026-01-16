C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insertcf.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INS1CF()
C     SUBROUTINE INS2CF()
C     SUBROUTINE INSCF0()
C     SUBROUTINE INSCF5()
C     SUBROUTINE INSCFC()
C     SUBROUTINE INSLCF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
C     SUBROUTINE MNICFL
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE INS1CF()
C     ===================
C2    No arguments needed.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/fillet.inc'
      include 'include/chamfer.inc'
C
      INTEGER*4 TMEN,TCELL,NLEN,MNUM,i
      DOUBLE PRECISION   DN
      REAL REAL
      LOGICAL OK,CVERFY
      CHARACTER*40 NVAL
      INTRINSIC REAL,MOD
      EXTERNAL CVERFY,GTMCLO,NLEN,GTMCWI,
     +         AEXPRN,GTCLRC,GTPMEN,GTHIMC,INSATT
C
      TMEN=MEN
      TCELL=CELLN
C
      IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C            Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attribues" cell.
            CALL GTMCLO(TMEN,TCELL)                                  
         RETURN
      ELSE IF (CCMD.EQ.'c') THEN
C        cancel last fillet option
         CALL INSCFC()
         CALL GTMCLO(MEN,CELLN)
      END IF
C
      IF ( CVERFY(CCMD,'FSBL') ) THEN
         CALL GTHIMC(TMEN,CCMD,'FSBL',TCELL)
         IF (      CCMD .EQ. 'L' ) THEN
            FLTRIM=0
         ELSE IF ( CCMD .EQ. 'F' ) THEN
            FLTRIM=1
         ELSE IF ( CCMD .EQ. 'S') THEN
            FLTRIM=2
         ELSE IF ( CCMD .EQ. 'B' ) THEN
            FLTRIM=3
         END IF
C        Go get another hit.
         RETURN
      END IF
 99   CONTINUE
      IF ( CVERFY(CCMD,'ias') ) THEN
         IF(CCMD.NE.'i') THEN
            CALL GTHIMC(TMEN,CCMD,'as',TCELL)
         END IF
C***************************************************************
C              N E W   F I R S T  L I N E  D I S T A N C E     *
C***************************************************************
         IF ( CCMD .EQ. 'i' ) THEN
            FILLET=0
            CALL DPRMXP(208,NVAL)
            IF(NLEN(NVAL).NE.0) THEN
               CALL AEXPRN(NVAL,DN,*99)
               IF(REAL(DN).GT.0.0) THEN
                  DFDIST = DN
C                 update cell contents for First Line:
                  CALL GTDMWR(517,3,REAL(DFDIST),'(F12.3)')
                  CALL FNDPOS(517,I)
                  CALL GTMCHI(3,I)
               END IF
            END IF
C           Highlight second distance.
            CALL FNDPOS(518,I)
            CALL GTMCHI(3,I)
C           Unhighlight angle.
            CALL FNDPOS(519,I)
            CALL GTMCLO(3,I)
C***************************************************************
C            N E W   F I X E D  A N G L E                      *
C***************************************************************
         ELSE IF ( CCMD .EQ. 'a' ) THEN
            FILLET=1
            CALL DPRMXP(188,NVAL)
            IF(NLEN(NVAL).NE.0) THEN
               CALL AEXPRN(NVAL,DN,*99)
               IF(REAL(DN).GT.0.0) THEN
                  DANGL = DN
C                 update cell contents for Angle:
                  CALL GTDMWR(519,3,REAL(DANGL),'(F12.3)')
                  CALL GTHIMC(TMEN,CCMD,'as',TCELL)
               END IF
            END IF
C***************************************************************
C            N E W   S E C O N D  L I N E  D I S T A N C E     *
C***************************************************************
         ELSE IF ( CCMD .EQ. 's') THEN
            FILLET=2
            CALL DPRMXP(208,NVAL)
            IF(NLEN(NVAL).NE.0) THEN
               CALL AEXPRN(NVAL,DN,*99)
               IF(REAL(DN).GT.0.0) THEN
                  DSDIST = DN
C                 update cell contents for second line:
                  CALL GTDMWR(518,3,REAL(DSDIST),'(F12.3)')
                  CALL GTHIMC(TMEN,CCMD,'as',TCELL)
               END IF
            END IF
         END IF
         RETURN
      END IF
      END
 
      SUBROUTINE INS2CF()
C     ===================
C2    INS2F5 set the menu cells with the values stored
C2    in the common block putting them in the correct
C2    menu cells by using GTHFMC to locate their cell number.
C
      include  'include/ndata.inc'
      include  'include/chamfer.inc'
C
      INTEGER*4 TMEN,TCELL,I
      CHARACTER TEMP*12,STR*1
C
      EXTERNAL GTHFMC,GTHIMC,GTPMEN
C
      DFDIST=DBLE(10.0)
      DSDIST=DBLE(10.0)
      DANGL=DBLE(45.0)
      FILLET = 1
C     We know they are on menu 3
      TMEN=3
C     set cell for RAD:
      CALL GTDMWR(517,3,REAL(DFDIST),'(F12.3)')
      CALL GTDMWR(518,3,REAL(DSDIST),'(F12.3)')
      CALL GTDMWR(519,3,REAL(DANGL),'(F12.3)')
C     Highlight first length.
      CALL FNDPOS(517,I)
      CALL GTMCHI(3,I)
C     Highlight angle.
      CALL FNDPOS(519,I)
      CALL GTMCHI(3,I)
 
C
      END
C
      SUBROUTINE INSCF0()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT FILLET option
C
      include 'include/menun.inc'
      include 'include/masti.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL MNIFIL,INS0F5,GTMCLO,CLRPEW,GTMCWI,MNICOL,MNISTD
C
      TMEN=MEN
      TCELL=CELLN
C     ensure cell is hilited
      CALL GTMCHI(TMEN,TCELL)
C     initialize INSERT FILLET option menu
      CALL MNICFL()
C     enter style option
      CALL MNISTD()
C
      CALL INSCF5()
C     ensure FILLET option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
      SUBROUTINE INSCF5()
C     ===================
C
C1    no arguments required
C2
C2    provides secondary support to the INSERT CHAMFER option
C2    this routine normally called by INSCF0 supervisor
C
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/fillet.inc'
C
C        TYPE OF TRIMMING var (FLTRIM)
C        ------
C        0 = no lines or arcs trimmed
C        1 = first  line or arc indicated trimmed
C        2 = second   "  "   "      "        "
C        3 = both lines or arcs  trimmed
C
C        TYPE OF FILLET  var (FILLET)
C        ------
C        0 = minor arc
C        1 = major arc
C        2 = full circle
C
      INTEGER*2 TMIP(2),NPTS,ENTYP(2)
C
      INTEGER*4 POINT,C,I,INDEX,TMEN,TCELL,NUM
      INTEGER*4 TFILLET
      REAL HX(2),HY(2),ANG,X3,X(5,2),DIST,
     +     X1,Y1,X2,Y2,XM,YM,XMT,YMT,DISTXY,
     2     XP,YP,XP1,YP1,CD0D13,TDBUFF(6,2)
C
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,
     1     XC1,YC1,XC2,YC2,XC3,YC3,XC4,YC4,
     2     VAL,DVDD13,DC(1:4),DDSTXY,XC,YC,
     3     XS1,YS1,XS2,YS2,XT1,YT1,XT2,YT2,DCANG,
     4     DX(2),DY(2),T1,T2,T3,S1,S2,S3,DFRAD
      LOGICAL OK,CVERFY
      INTRINSIC DBLE,ABS,REAL,ATAN,INDEX
C
      EXTERNAL INS2F5,TCURS,INS1F5,NOSRCH,ADSRCH,DSE801,
     +         DCVL14,DVCPLP,DCCPAP,CC00P5,DBOX,INSLCF,CD0D13,
     1         INSLAF,DISTXY,INSAAF,UNFLAG
C
C
      CFMIP(1)=0
      CFMIP(2)=0
      NPTS=1
      TFILLET = FILLET
      CALL INS2CF()
C
C     Get a cursor hit;find out what he wants to do.
 10   CONTINUE
      IF ( NPTS .EQ. 1 ) THEN
C        prompt for hit on first entity
         CALL DCPRNT(222)
      ELSE
C        prompt for hit on second entity
         CALL DCPRNT(223)
      END IF
C
      CALL TCURS(C,HX(NPTS),HY(NPTS))
C
      IF (MEN.EQ.2 .OR. CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         CALL UNFLAG(.TRUE.)
C        reset fillet to previous value to prevent fillet option being corrupted
         FILLET = TFILLET
         RETURN
      END IF
      TMEN=MEN
      TCELL=CELLN
      IF ( MEN .EQ. 3 ) THEN
         CALL INS1CF()
         GOTO 10
      END IF
C
      CALL NOSRCH()
C     add lines only to the chamfer control
      CALL ADSRCH(LINE)
C
      CALL DSE800(HX(NPTS),HY(NPTS),OK)
C
      IF ( .NOT. OK ) THEN
C     Did not find a line close enough ask again
         CALL DEPRNT(142)
         GOTO 10
      END IF
C
      TMIP(NPTS)=MIP
C
C     test for same entity hit twice
      IF (NPTS.EQ.2) THEN
         IF (TMIP(1).EQ.TMIP(2)) THEN
            CALL DEPRNT(224)
            GOTO 10
         END IF
      END IF
C
      ENTYP(NPTS)=IMBUFF(2)
C
      IF ( IMBUFF(2) .EQ. LINE ) THEN
C
         X(1,NPTS)=RDBUFF(1)
         X(2,NPTS)=RDBUFF(2)
         X(3,NPTS)=RDBUFF(4)
         X(4,NPTS)=RDBUFF(5)
 
C
      ELSE  IF ( IMBUFF(2) .EQ. ARC) THEN
C
         X(1,NPTS)=RDBUFF(1)
         X(2,NPTS)=RDBUFF(2)
         X(3,NPTS)=RDBUFF(4)
         X(4,NPTS)=RDBUFF(5)
         X(5,NPTS)=RDBUFF(6)
C
      END IF
 
      DO 105 I=1,6
105     TDBUFF(I,NPTS)=RDBUFF(I)
 
C
      IF ( ENTYP(NPTS) .EQ. LINE ) THEN
         CALL DCVL14(DBLE(X(1,NPTS)),DBLE(X(2,NPTS)),DBLE(X(3,NPTS)),
     +               DBLE(X(4,NPTS)),L1,L2,L3)
C
         CALL DVCPLP(L1,L2,L3,DBLE(HX(NPTS)),DBLE(HY(NPTS)),
     +                         DX(NPTS),DY(NPTS) )
      ELSE
         CALL DCCPAP(DBLE(X(1,NPTS)),DBLE(X(2,NPTS)),DBLE(X(3,NPTS)),
     +             DBLE(HX(NPTS)),DBLE(HY(NPTS)),DX(NPTS),DY(NPTS))
      END IF
C
      NPTS=NPTS+1
C
      IF ( NPTS .EQ. 2 ) GOTO 10
C
      CALL UNFLAG(.TRUE.)
C
      IF ( ENTYP(1) .EQ. LINE .AND. ENTYP(2) .EQ. LINE ) THEN
         CALL CC00P5(X(1,1),X(2,1),X(3,1),X(4,1),
     +               X(1,2),X(2,2),X(3,2),X(4,2),XM,YM,OK)
         IF ( .NOT. OK ) THEN
            CALL DEPRNT(157)
            NPTS=2
            GOTO 10
         ELSE
          CALL INSLCF(TMIP(1),X(1,1),X(2,1),X(3,1),X(4,1),DX(1),DY(1),
     1                TMIP(2),X(1,2),X(2,2),X(3,2),X(4,2),DX(2),DY(2),
     2                             OK)
         END IF
C
      END IF
C
C     good fillet we can save it
      IF(OK) THEN
 
          CALL DER500(TMIP(1),OK)
          DO 100 I = 1 ,6
 
 100        BUFF(I)=TDBUFF(I,1)
 
          CALL DER500(TMIP(2),OK)
 
          DO 101 I = 1 ,6
 
 101        BUFF(I+6)=TDBUFF(I,2)
 
          CFMIP(1)=TMIP(1)
          CFMIP(2)=TMIP(2)
 
      ENDIF
      NPTS=1
 
C
      GOTO 10
C
      END
*
      SUBROUTINE INSCFC()
C     ===================
C     This routine wil cancel the last fillet
C
      include  'include/ndata.inc'
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/fillet.inc'
      include  'include/viewport.inc'
C
      INTEGER*2 CURENT,DFPT,ENT,I
      LOGICAL OK
C
      IF(CFMIP(1).EQ.0.OR.CFMIP(2).EQ.0) THEN
          CALL DEPRNT(241)
          RETURN
      ENDIF
 
C     WRITE(10,*) '[INS1F6] CFMIP(1)= ',CFMIP(1),CFMIP(2)
C     erase all the nasties
      CURENT = NMIPOS -1
      VPMOV = .TRUE.
      CALL PENERS()
      CALL DIR500(CFMIP(1),OK)
      CALL ALLDRW(IMBUFF(2),CFMIP(1) )
      CALL DIR500(CFMIP(2),OK)
      CALL ALLDRW(IMBUFF(2),CFMIP(2) )
      VPMOV = .FALSE.
      ENT=ARC
C     set cancel flag
      VPCAN = .TRUE.
      CALL PENERS()
C     take fillet off data base
      CALL ALLDRW(ENT,CURENT )
      IF(DISPV) LDFILE(CVPN) = LDFILE(CVPN) -1
      CALL PENDRW()
      VPCAN = .FALSE.
      VPMOV=.TRUE.
      CALL DER500(CFMIP(1),OK)
 
      DO 10 I=1,6
 10     RDBUFF(I)=BUFF(I)
 
      CALL DBM500(IMBUFF(7),OK)
 
      CALL DER500(CFMIP(2),OK)
 
      DO 20 I=1,6
 20     RDBUFF(I)=BUFF(I+6)
 
      CALL DBM500(IMBUFF(7),OK)
 
      CALL PENDRW()
      CALL ALLDRW(ENT,CFMIP(1) )
      CALL ALLDRW(ENT,CFMIP(2) )
 
C     reset mov flag
      VPMOV = .FALSE.
      CFMIP(1)=0
      CFMIP(2)=0
 
C     reset database pointers
      NMIPOS=NMIPOS-1
      NPDPOS=NPDPOS-1
C     take off layer monitor
      CALL DELMON(CLAYER,.FALSE.)
      END
C
      SUBROUTINE INSLCF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
     +                  TMIP2,XG1,YG1,XG2,YG2,DX2,DY2,
     1                                             OK)
C     ================================================
C2    This routine is used to create the chamfer fillet
C2    between two lines.  These are calculated from the
C2    two line coordinates and master index pointers provided
C
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
      include  'include/chamfer.inc'
CAPOLLO:SUN
      include   'include/viewport.inc'
CAPOLLO:SUN
C
      INTEGER*2 TMIP1,TMIP2,P,ENT,ENTTMP
      INTEGER*4 I4
C
 
      REAL XF1,YF1,XF2,YF2,XG1,YG1,XG2,YG2,SANG,EANG,ANG,PI,ATAN
     +     REAL,ABS
      LOGICAL OK,SAME
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,X,Y,
     1     CF1,CF2,CF3,TXP1,TYP1,TXP2,TYP2,TXP3,TYP3,
     2     NL1,NL2,NL3,TDANGL,
     3     VAL,DVDD13,DC(1:4),DIST,DDSTXY,XC,YC,
     4     XS1,YS1,XS2,YS2,XT1,YT1,XT2,YT2,DCANG,DRAD,
     5     DX1,DY1,DX2,DY2,T1,T2,T3,S1,S2,S3,DFRAD,DBLE,DVDD14
C
      INTRINSIC ATAN,DBLE,REAL,ABS
C
      EXTERNAL DCVL14,DVC0P5,DVC0C8,DVDD13,DDSTXY,
     +         DVCPLP,DCANG,RSWAP,DER500,ERSFLW,
     1         DRWFLW,DEM500,DEWC05,DRWFAW,DRAD,DVDD14
C
C
      XT1=DBLE(XF1)
      YT1=DBLE(YF1)
      XT2=DBLE(XF2)
      YT2=DBLE(YF2)
C
      XS1=DBLE(XG1)
      YS1=DBLE(YG1)
      XS2=DBLE(XG2)
      YS2=DBLE(YG2)
 
C
C     Create line vector for first line
      CALL DCVL14(XT1,YT1,XT2,YT2,L1,L2,L3)
C     Create line vector for second line
      CALL DCVL14(XS1,YS1,XS2,YS2,M1,M2,M3)
C
C     X,Y are the intersection of the two lines
      CALL DVC0P5(L1,L2,L3,M1,M2,M3,X,Y,OK)
C
C     If not OK then lines did not intersect ie. PARALLEL
      IF ( .NOT. OK ) THEN
C     Warn user
         CALL DEPRNT(157)
         RETURN
      END IF
C
c     Let us create the vector from the intersection point to nearest
      CALL DCVL14(X,Y,DBLE(DX1),DBLE(DY1),NL1,NL2,NL3)
C     now lets find the point FDIST along the vector
      CALL DVC0P4(X,Y,DFDIST,NL1,NL2,NL3,TXP1,TYP1)
C     now we have the trim point for the first line
C     We must find out which intersection poit is to be trimmed
C     to do this we will establish only 2 distance. i.e. the
C     distance between the end/intesection point and end/trim
C     point.  NOTE this distance MUST be greater between end and
C     trim point and end and intesection point.
C
      IF ( FLTRIM .EQ. 1 .OR. FLTRIM .EQ. 3) THEN
C     Trim the first line indicated back to the fillet
C        let us erase the line in question
         CALL PENERS()
         ENT=LINE
         CALL ALLDRW(ENT,TMIP1)
         CALL PENDRW()
C
C        first let us do database read
         CALL DER500(TMIP1,OK)
C
         DC(1)=DVDD14(X,Y,NL1,NL2,NL3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
         DC(2)=DVDD14(X,Y,NL1,NL2,NL3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
C
C        check to see if this is the trim end
         IF(DC(1).LT.DC(2)) THEN
c           correct end
C           must modify the data in buffers
            RDBUFF(1) = REAL(TXP1)
            RDBUFF(2) = REAL(TYP1)
         ELSE
c           must be the other end
            RDBUFF(4) = REAL(TXP1)
            RDBUFF(5) = REAL(TYP1)
         END IF
C        now write the data
         CALL DEM500(TMIP1,OK)
c        let us redraw modified entity
         ENT=LINE
         CALL ALLDRW(ENT,TMIP1)
      END IF
C
      IF(FILLET.EQ.1) THEN
C        let us find a vector throught trim point at angle to
C        start vector
C        now it is difficult to calculate which quad etc
C        so becuase I'm lazy I'll calculate both and the
C        closest to the second hit point wins
         TDANGL = DRAD(DANGL)
         CALL DVV0L5(L1,L2,L3,TXP1,TYP1,TDANGL,CF1,CF2,CF3)
C        now let us find the intersection point to the other line vector
         CALL DVC0P5(CF1,CF2,CF3,M1,M2,M3,TXP2,TYP2,OK)
C        If not OK then lines did not intersect ie. PARALLEL
         IF ( .NOT. OK ) THEN
C        Warn user
            CALL DEPRNT(157)
            RETURN
         END IF
         CALL DVV0L5(L1,L2,L3,TXP1,TYP1,-TDANGL,CF1,CF2,CF3)
C        now let us find the intersection point to the other line vector
         CALL DVC0P5(CF1,CF2,CF3,M1,M2,M3,TXP3,TYP3,OK)
C        If not OK then lines did not intersect ie. PARALLEL
         IF ( .NOT. OK ) THEN
C        Warn user
            CALL DEPRNT(157)
            RETURN
         END IF
         DC(1)=DDSTXY(DBLE(DX2),DBLE(DY2),TXP2,TYP2)
         DC(2)=DDSTXY(DBLE(DX2),DBLE(DY2),TXP3,TYP3)
         IF(DC(2).LT.DC(1)) THEN
            TXP2=TXP3
            TYP2=TYP3
         END IF
c        Let us create the vector from the intersection point to nearest
C        we will need it to find out correct end to trim (vector mag)
         CALL DCVL14(X,Y,TXP2,TYP2,NL1,NL2,NL3)
C
      ELSE
C        must be using some fixed distance alone second vector
c        Let us create the vector from the intersection point to nearest
         CALL DCVL14(X,Y,DBLE(DX2),DBLE(DY2),NL1,NL2,NL3)
C        now lets find the point DSDIST along the vector
         CALL DVC0P4(X,Y,DSDIST,NL1,NL2,NL3,TXP2,TYP2)
c
      END IF
C
C     now we have the trim point for the second line
C     We must find out which intersection poit is to be trimmed
C     to do this we will establish only 2 distance. i.e. the
C     distance between the end/intesection point and end/trim
C     point.  NOTE this distance MUST be greater between end and
C     trim point and end and intesection point.
C
      IF ( FLTRIM .EQ. 2 .OR. FLTRIM .EQ. 3) THEN
C
C        let us erase the line in question
         CALL PENERS()
         ENT=LINE
         CALL ALLDRW(ENT,TMIP2)
         CALL PENDRW()
C        Trim the second line back to fillet
C        first let us do database read
         CALL DER500(TMIP2,OK)
C
         DC(1)=DVDD14(X,Y,NL1,NL2,NL3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
         DC(2)=DVDD14(X,Y,NL1,NL2,NL3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
C
C        check to see if this is the trim end
         IF(DC(1).LT.DC(2)) THEN
c           correct end
C           must modify the data in buffers
            RDBUFF(1) = TXP2
            RDBUFF(2) = TYP2
         ELSE
c           must be the other end
            RDBUFF(4) = TXP2
            RDBUFF(5) = TYP2
         END IF
C        now write the data
         CALL DEM500(TMIP2,OK)
c        let us redraw modified entity
         ENT=LINE
         CALL ALLDRW(ENT,TMIP2)
      END IF
C
C     Now create the chamfer line
c
      CALL DEWC03(REAL(TXP1),REAL(TYP1),REAL(TXP2),REAL(TYP2),CLFONT,
     +            CLAYER,P,OK)
C
C     Finally display arc chosen
      ENT=LINE
      CALL ALLDRW(ENT,P)
C     Return for another shot
C
CAPOLLO|SUN
C     This will draw the line withot diaply file interference
      VPADD = .FALSE.
CAPOLLO|SUN
      END
*
      SUBROUTINE MNICFL
C     =================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2    the INSERT FILLET option list.
C2
C2    Tokens used here are L,F,S,B,O,J and U.
C2
      include 'include/ndata.inc'
C
      INTEGER*4 I
      EXTERNAL GTPMEN,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Initialise menu cell datum.
      FLOFF=10
C
C     Load INSERT FILLET option list
C
C2    R is the token for Change Fillet Radius
c      CALL GTDMEN(130,3)
C2    L is the token for NO TRIMMING.
      CALL GTDMEN(131,3)
C2    F is the token for FIRST ONLY.
      CALL GTDMEN(132,3)
C2    S is the token for SECOND ONLY.
      CALL GTDMEN(133,3)
C2    B is the token for BOTH TRIMMED.
      CALL GTDMEN(134,3)
C     hilite both as default
      CALL FNDPOS(134,I)
      CALL GTMCHI(3,I)
C2    O is the token for MINOR ARC.
      CALL GTDMEN(517,3)
C     hilite minor arc as default
C2    J is the token for MAJOR ARC.
      CALL GTDMEN(518,3)
C2    U is the token for FULL ARC.
      CALL GTDMEN(519,3)
C     cancel option
      CALL GTDMEN(99,3)
C2
      END
C
C
C
