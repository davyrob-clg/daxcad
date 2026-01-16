C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insertf.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INS0F5()
C     SUBROUTINE INS1F5()
C     SUBROUTINE INS1F6()
C     SUBROUTINE INS2F5()
C     SUBROUTINE INSAAF(TMIP1,XF1,YF1,XF2,DX1,DY1,
C     SUBROUTINE INSLAF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
C     SUBROUTINE INSLLF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
C     SUBROUTINE MNIFIL
C     SUBROUTINE WCROSS(WX,WY)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE INS0F5()
C     ===================
C
C1    no arguments required
C2
C2    provides secondary support to the INSERT FILLET option
C2    this routine normally called by INSF00 supervisor
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
     +         DCVL14,DVCPLP,DCCPAP,CC00P5,DBOX,INSLLF,CD0D13,
     1         INSLAF,DISTXY,INSAAF,UNFLAG
C
C
      CFMIP(1)=0
      CFMIP(2)=0
      NPTS=1
      CALL INS2F5()
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
         RETURN
      END IF
      TMEN=MEN
      TCELL=CELLN
      IF ( MEN .EQ. 3 ) THEN
         CALL INS1F5()
         GOTO 10
      END IF
C
      CALL NOSRCH()
      CALL ADSRCH(LINE)
      CALL ADSRCH(ARC)
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
          CALL INSLLF(TMIP(1),X(1,1),X(2,1),X(3,1),X(4,1),DX(1),DY(1),
     1                TMIP(2),X(1,2),X(2,2),X(3,2),X(4,2),DX(2),DY(2),
     2                             OK)
         END IF
C
      ELSE IF ( ENTYP(1) .EQ. LINE .AND. ENTYP(2) .EQ. ARC ) THEN
C
         DIST=CD0D13(X(1,1),X(2,1),X(3,1),X(4,1),X(1,2),X(2,2))
         IF ( ABS(DIST) .GT. (X(3,2)+2.0*FRAD)) THEN
            CALL DEPRNT(225)
            NPTS=2
            GOTO 10
         ELSE
          CALL INSLAF(TMIP(1),X(1,1),X(2,1),X(3,1),X(4,1),DX(1),DY(1),
     1                TMIP(2),X(1,2),X(2,2),X(3,2),DX(2),DY(2),
     2                   .FALSE. ,OK)
         END IF
C
      ELSE IF ( ENTYP(1) .EQ. ARC  .AND. ENTYP(2) .EQ. LINE ) THEN
C
         DIST=CD0D13(X(1,2),X(2,2),X(3,2),X(4,2),X(1,1),X(2,1))
         IF ( ABS(DIST) .GT. (X(3,1)+2.0*FRAD)) THEN
            CALL DEPRNT(225)
            NPTS=2
            GOTO 10
         ELSE
          CALL INSLAF(TMIP(2),X(1,2),X(2,2),X(3,2),X(4,2),DX(2),DY(2),
     1                TMIP(1),X(1,1),X(2,1),X(3,1),DX(1),DY(1),
     2                   .TRUE. ,OK)
C
         END IF
      ELSE IF ( ENTYP(1) .EQ. ARC  .AND. ENTYP(2) .EQ. ARC ) THEN
C
C         WRITE(UNIT=10,FMT=*)'[INS0F5] CALLING DISTXY',
C     +         X(1,2),' ',X(2,2),' ',X(1,1),' ',X(2,1)
         DIST=DISTXY(X(1,2),X(2,2),X(1,1),X(2,1))
         IF ( ABS(DIST) .GT. (X(3,1)+X(3,2)+2.0*FRAD)) THEN
            CALL DEPRNT(225)
            NPTS=2
            GOTO 10
         ELSE
          CALL INSAAF(TMIP(1),X(1,1),X(2,1),X(3,1),DX(1),DY(1),
     1                TMIP(2),X(1,2),X(2,2),X(3,2),DX(2),DY(2),OK)
C
         END IF
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
      SUBROUTINE INS1F5()
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
C
      INTEGER*4 TMEN,TCELL,NLEN,MNUM
      DOUBLE PRECISION   DFRAD
      REAL REAL
      LOGICAL OK,CVERFY,MOK
      INTRINSIC REAL,MOD
      EXTERNAL CVERFY,GTMCLO,NLEN,GTMCWI,
     +         AEXPRN,GTCLRC,GTPMEN,GTHIMC,INSATT
C
      TMEN=MEN
      TCELL=CELLN
C
C
      IF ( CCMD .EQ. 'R' ) THEN
C***************************************************************
C              N E W   F I L L E T   R A D I U S               *
C***************************************************************
C        user wants to change the fillet radius size
C        prompt for new radius and return expression
 111     CONTINUE
         CALL DPRMXP(186,CBUFF)
C
         IF ( NLEN(CBUFF) .EQ. 0 ) THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CBUFF,DFRAD,*111)
            IF ( DFRAD .GT. 0.0 ) THEN
               FRAD=REAL(DFRAD)
C              update cell contents for RAD:
               CALL GTDMWR(130,3,FRAD,'(F12.3)')
            ELSE
C              prompt user that zero radius is illegal
C              and try again
               CALL DEPRNT(187)
               GOTO 111
            END IF
         END IF
         CALL GTMCLO(MEN,CELLN)
         RETURN
      END IF
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      IF (CVERFY(CCMD,'=fk')) THEN
C     ****************************
C       Change line attributes.  
C     ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(TMEN, TCELL)                                  
         RETURN
C
      ELSE IF (CCMD.EQ.'c') THEN
C        cancel last fillet option
         CALL INS1F6()
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
      IF ( CVERFY(CCMD,'JOU') ) THEN
         CALL GTHIMC(TMEN,CCMD,'JOU',TCELL)
         IF ( CCMD .EQ. 'O' ) THEN
            FILLET=0
         ELSE IF ( CCMD .EQ. 'J' ) THEN
            FILLET=1
         ELSE IF ( CCMD .EQ. 'U') THEN
            FILLET=2
         END IF
         RETURN
      END IF
      END
 
      SUBROUTINE INS1F6()
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
      SUBROUTINE INS2F5()
C     ===================
C2    INS2F5 set the menu cells with the values stored
C2    in the common block putting them in the correct
C2    menu cells by using GTHFMC to locate their cell number.
C
      include  'include/ndata.inc'
C
      INTEGER*4 TMEN,TCELL
      CHARACTER TEMP*12,STR*1
C
      EXTERNAL GTHFMC,GTHIMC,GTPMEN
C
C     We know they are on menu 3
      TMEN=3
C     set cell for RAD:
      CALL GTDMWR(130,3,FRAD,'(F12.3)')
C     Find out what fillet trimming has been saved
C     then use GTHIMC to highlite only one of them
      TEMP='LFSB         '
      STR=TEMP(FLTRIM+1:FLTRIM+1)
      CALL GTHIMC(TMEN,STR,'FSBL',TCELL)
C     Find out what fillet type has been saved
C     then use GTHIMC to highlite only one of them
      TEMP='OJU '
      STR=TEMP(FILLET+1:FILLET+1)
      CALL GTHIMC(TMEN,STR,'JOU',TCELL)
C
      END
C
      SUBROUTINE INSAAF(TMIP1,XF1,YF1,XF2,DX1,DY1,
     +                  TMIP2,XG1,YG1,XG2,DX2,DY2,
     1                                         OK)
C     ============================================
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/entity.inc'
      include   'include/style.inc'
CAPOLLO:SUN
      include   'include/viewport.inc'
CAPOLLO:SUN
C
      INTEGER*2 TMIP1,TMIP2,P,I,ENT
      INTEGER*4 NUM,IN,I4
C
      REAL XF1,YF1,XF2,YF2,XG1,YG1,XG2,YG2,ZG1,ZF1,TOTAL,REAL,
     1    HX1,HY1,HX2,HY2,SANG,EANG,ANG,EANG1,SANG1,PI,ATAN,ABS
      LOGICAL OK,CLOCK1,CLOCK2,DCLKWS,CLOCK,OKP(4),SAME
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,X,Y,
     1     XC(8),YC(8),YC4,XP1,YP1,XP,YP,
     2     VAL,DVDD13,DC(1:4),DIST,DDSTXY,DIST1,
     3     XS1,YS1,XS2,YS2,XT1,YT1,XT2,YT2,DCANG,
     4     DX1,DY1,DX2,DY2,T1,T2,T3,S1,S2,S3,DFRAD,DBLE
C
      INTRINSIC ATAN,DBLE,REAL,ABS
      EXTERNAL DCC0C9,DCCP19,DDSTXY,DCLKWS,DCCPAP,
     +         DCANG,RSWAP,DER500,ERSFAW,DRWFAW,DEM500,DEWC05
C
      XT1=DBLE(XF1)
      YT1=DBLE(YF1)
      XT2=DBLE(XF2)
C
      XS1=DBLE(XG1)
      YS1=DBLE(YG1)
      XS2=DBLE(XG2)

C
C     Added for bug in UNIX version
C
      DO 110 I=1,8
         XC(I) = 0.0
         YC(I) = 0.0
110   CONTINUE
C
      CALL DCC0C9(XT1,YT1,XT2,XS1,YS1,XS2,DBLE(FRAD),XC,YC,OKP)
C
      P=1
      NUM=0
      DO 10 I=1,4
         IF ( OKP(I) ) NUM=NUM+1
 10   CONTINUE
C
      IF(NUM.EQ.0) THEN
C        no fillet centers available
         CALL DEPRNT(108)
         OK=.FALSE.
         RETURN
      ENDIF
C
c      WRITE(10,*) '[INSAAF] NUM= ',NUM
C
C      WRITE(UNIT=10,FMT=*)'[INSAAF] CENTRES FOUND=',
C     +   OKP(1),OKP(2),OKP(3),OKP(4),NUM
C      WRITE(UNIT=10,FMT=*)'[INSAAF] 2  XC4,YC4 ',XC(4),' ',YC(4)
      CALL DCCP19(XT1,YT1,XT2,XS1,YS1,XS2,XP,YP,XP1,YP1,OK)
C
C     Original arcs do not intersection therefore only two fillets
C     exist.
c      DO 201 I=1,NUM
c         CALL DRWFAW(REAL(XC(I)),REAL(YC(I)),FRAD,0.0,6.28,INT4(2))
c         CALL DRWFAW(REAL(XC(I+4)),REAL(YC(I+4)),FRAD,0.0,6.28,INT4(3))
c 201  CONTINUE
C
 
      IF ( OKP(1) .AND. NUM .EQ. 1 ) THEN
         XP=(DX1+DX2)/2.0
         YP=(DY1+DY2)/2.0
         XC(4)=XC(5)
         YC(4)=YC(5)
C      WRITE(UNIT=10,FMT=*)'[INSAAF] 3  XC4,YC4 ',XC(4),' ',YC(4)
         XP=(XT1+XS1)/2.0
         YP=(YT1+YS1)/2.0
         NUM=4
      ELSE IF ( OKP(2) .AND. NUM .EQ. 1 ) THEN
 
C
      ELSE IF ( OKP(3) .AND. NUM .EQ. 1 ) THEN
C
      ELSE
         DIST=DDSTXY(XC(1),YC(1),DX1,DY1)+DDSTXY(XC(1),YC(1),DX2,DY2)
         DIST1=DDSTXY(XC(5),YC(5),DX1,DY1)+DDSTXY(XC(5),YC(5),DX2,DY2)
         IF ( DIST1 .LT. DIST ) THEN
            DO 5 P=5,8
               XC(P-4)=XC(P)
               YC(P-4)=YC(P)
 5          CONTINUE
            XP=XP1
            YP=YP1
C      WRITE(UNIT=10,FMT=*)'[INSAAF] 4  XC4,YC4 ',XC(4),' ',YC(4)
         END IF
      END IF
C
      CLOCK1=DCLKWS(XT1,YT1,DX1,DY1,XP,YP)
      CLOCK2=DCLKWS(XS1,YS1,DX2,DY2,XP,YP)
C
      CLOCK=.FALSE.
      IF ( CLOCK1.AND.CLOCK2 ) CLOCK =.TRUE.
      IF ( .NOT. CLOCK1 .AND..NOT.CLOCK2 ) CLOCK=.TRUE.
C
C      WRITE(10,*) '[INSAAF] CLOCK',CLOCK,CLOCK1,CLOCK2
C
      IF ( CLOCK ) THEN
         IF ( NUM .EQ. 3 ) THEN
            DIST=DDSTXY(XC(2),YC(2),DX1,DY1)+
     +           DDSTXY(XC(2),YC(2),DX2,DY2)
            DIST1=DDSTXY(XC(3),YC(3),DX1,DY1)+
     +            DDSTXY(XC(3),YC(3),DX2,DY2)
            IF ( DIST .LT. DIST1 ) THEN
               P=2
            ELSE
               P=3
            END IF
         ELSE
            IF ( OKP(2) ) THEN
               DIST=DDSTXY(XC(2),YC(2),DX1,DY1)+
     +              DDSTXY(XC(2),YC(2),DX2,DY2)
               DIST1=DDSTXY(XC(6),YC(6),DX1,DY1)+
     +               DDSTXY(XC(6),YC(6),DX2,DY2)
               IF ( DIST .LT. DIST1 ) THEN
                  P=2
               ELSE
                  P=6
               END IF
            ELSE
               P=0
            END IF
         END IF
      ELSE
         IF ( NUM .EQ. 4 ) THEN
C      write(10,*)'[insaaf] calculating dist'
C      write(10,*)XC(1),' ',YC(1),' ',DX1,' ',DY1,' ',
C     +                     DX2,' ',DY2
            DIST=DDSTXY(XC(1),YC(1),DX1,DY1)+
     +           DDSTXY(XC(1),YC(1),DX2,DY2)
C      write(10,*)'[insaaf] calculating dist1'
C      write(10,*)XC(4),' ',YC(4),' ',DX1,' ',DY1,' ',
C     +                     DX2,' ',DY2
            DIST1=DDSTXY(XC(4),YC(4),DX1,DY1)+
     +            DDSTXY(XC(4),YC(4),DX2,DY2)
            IF ( DIST .LT. DIST1 ) THEN
               P=1
            ELSE
               P=4
            END IF
         ELSE
            IF ( OKP(1) ) THEN
               P=1
            ELSE IF ( OKP(2) ) THEN
               DIST=DDSTXY(XC(2),YC(2),DX1,DY1)+
     +              DDSTXY(XC(2),YC(2),DX2,DY2)
               DIST1=DDSTXY(XC(6),YC(6),DX1,DY1)+
     +               DDSTXY(XC(6),YC(6),DX2,DY2)
               IF ( DIST .LT. DIST1 ) THEN
                  P=2
               ELSE
                  P=6
               END IF
            ELSE IF ( OKP(3) ) THEN
               DIST=DDSTXY(XC(3),YC(3),DX1,DY1)+
     +              DDSTXY(XC(3),YC(3),DX2,DY2)
               DIST1=DDSTXY(XC(7),YC(7),DX1,DY1)+
     +               DDSTXY(XC(7),YC(7),DX2,DY2)
               IF ( DIST .LT. DIST1 ) THEN
                  P=2
               ELSE
                  P=6
               END IF
            ELSE
               P=0
            END IF
         END IF
      END IF
C
c      WRITE(10,*) '[INSAAF] P NUM = ',P,NUM
      IF ( P .GT. NUM .OR.P.EQ.0) THEN
         CALL DEPRNT(108)
         OK=.FALSE.
         RETURN
      END IF
C
      CALL DCCPAP(XT1,YT1,XT2,XC(P),YC(P),T1,T2)
      CALL DCCPAP(XS1,YS1,XS2,XC(P),YC(P),S1,S2)
C
      IF  ( FILLET .EQ. 2 ) THEN
         SANG1=0.0
         EANG1=PI(2.0)
      ELSE
C
         SANG1=REAL(DCANG(XC(P),YC(P),T1,T2))
         EANG1=REAL(DCANG(XC(P),YC(P),S1,S2))
C
         ANG=ABS(EANG1-SANG1)
         IF ( EANG1 .LT. SANG1 ) THEN
            ANG=PI(2.0)-ANG
         END IF
C
         IF ( FILLET .EQ. 0 ) THEN
            IF ( ANG .GT. PI(1.0) ) THEN
               CALL RSWAP(SANG1,EANG1)
            END IF
         ELSE IF ( FILLET .EQ. 1 ) THEN
            IF ( ANG .LT. PI(1.0) ) THEN
               CALL RSWAP(SANG1,EANG1)
            END IF
         END IF
      END IF
C
      IF ( FLTRIM .EQ. 0 ) GOTO 100
C
      IF ( FLTRIM .EQ. 1 .OR. FLTRIM .EQ. 3 ) THEN
C     Trim the first BACK indicated back to the fillet
C
         CALL DER500(TMIP1,OK)
         I4=IMBUFF(6)
CSUN:APOLLO
         VPMOV=.TRUE.
CSUN:APOLLO
         CALL PENERS()
         ENT=ARC
         CALL ALLDRW(ENT,TMIP1)
         CALL PENDRW()
C
C         CALL ERSFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
C     +               RDBUFF(6),I4)
C
         ANG=REAL(DCANG(XT1,YT1,T1,T2))
         IF (  CLOCK1 ) THEN
            IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
               RDBUFF(6)=ANG
               ANG=REAL(DCANG(XT1,YT1,XP,YP))
               RDBUFF(5)=ANG
            ELSE
               RDBUFF(6)=ANG
            END IF
         ELSE
            IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
               RDBUFF(5)=ANG
               ANG=REAL(DCANG(XT1,YT1,XP,YP))
               RDBUFF(6)=ANG
            ELSE
               RDBUFF(5)=ANG
            END IF
         END IF
         IF ( SAME(RDBUFF(5),RDBUFF(6))  ) THEN
            CALL DEPRNT(226)
         ELSE
            MIP=TMIP1
            CALL DEM500(TMIP1,OK)
         END IF
         ENT=ARC
         CALL ALLDRW(ENT,TMIP1)
      END IF
C
      IF ( FLTRIM .EQ. 2 .OR. FLTRIM .EQ. 3 ) THEN
C     Trim the first BACK indicated back to the fillet
C
         CALL DER500(TMIP2,OK)
         I4=IMBUFF(6)
CSUN:APOLLO
         VPMOV=.TRUE.
CSUN:APOLLO
         CALL PENERS()
         ENT=ARC
         CALL ALLDRW(ENT,TMIP2)
         CALL PENDRW()
C
C         CALL ERSFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
C     +               RDBUFF(6),I4)
C
         ANG=REAL(DCANG(XS1,YS1,S1,S2))
         IF ( CLOCK2 ) THEN
            IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
               RDBUFF(6)=ANG
               ANG=REAL(DCANG(XS1,YS1,XP,YP))
               RDBUFF(5)=ANG
            ELSE
               RDBUFF(6)=ANG
            END IF
         ELSE
            IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
               RDBUFF(5)=ANG
               ANG=REAL(DCANG(XS1,YS1,XP,YP))
               RDBUFF(6)=ANG
            ELSE
               RDBUFF(5)=ANG
            END IF
         END IF
         IF ( SAME(RDBUFF(5),RDBUFF(6))  ) THEN
            CALL DEPRNT(226)
         ELSE
            MIP=TMIP2
            CALL DEM500(TMIP2,OK)
         END IF
         ENT=ARC
         CALL ALLDRW(ENT,TMIP2)
C
      END IF
C
 100  CONTINUE
C
CSUN:APOLLO
      VPMOV=.FALSE.
      VPADD=.TRUE.
CSUN:APOLLO
C     store the new arc in database
      CALL DEWC05(REAL(XC(P)),REAL(YC(P)),FRAD,
     +            SANG1,EANG1,CLFONT,CLAYER,P,OK)
C     Finally display arc chosen
      I4=IMBUFF(6)
      ENT=ARC
      CALL ALLDRW(ENT,P)
C      CALL DRWFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
C     +            RDBUFF(6),I4)
C
      END
C
      SUBROUTINE INSLAF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
     +                  TMIP2,XG1,YG1,XG2,DX2,DY2,FIRST,OK)
C     =====================================================
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/entity.inc'
      include   'include/style.inc'
CAPOLLO:SUN
      include   'include/viewport.inc'
CAPOLLO:SUN
C
      INTEGER*2 TMIP1,TMIP2,P,ENT
      INTEGER*4 NUM,I4,TFONT,I
      REAL XF1,YF1,XF2,YF2,XG1,YG1,XG2,PI,REAL,ABS,
     1     SANG,EANG,ANG,SANG1,EANG1,ATAN
      LOGICAL OK,DCLKWS,CLOCK,FIRST,SAME
      DOUBLE PRECISION L1,L2,L3,M1,
     1     XC(8),YC(8),XP,YP,XP1,YP1,
     2     DVDD13,DCDD13,DC(3),DIST,DDSTXY,DIST1,
     3     XS1,YS1,XS2,XT1,YT1,XT2,YT2,DCANG,
     4     DX1,DY1,DX2,DY2,T1,T2,S1,S2,DBLE
      INTRINSIC ATAN,DBLE,REAL,ABS
C
      EXTERNAL DCC0C7,DCC0P9,DDSTXY,CD0D13,DCLKWS,DCCPLP,
     +         DCCPAP,DCANG,RSWAP,DER500,ERSFAW,DCDD13,
     1         DRWFAW,DEM500,DCVL14,DVDD13,ERSFLW,DRWFLW,DEWC05
C
      DIST1 = 0
      DIST = 0
      TFONT =1
      XT1=DBLE(XF1)
      YT1=DBLE(YF1)
      XT2=DBLE(XF2)
      YT2=DBLE(YF2)
C
      XS1=DBLE(XG1)
      YS1=DBLE(YG1)
      XS2=DBLE(XG2)
C
C     Added for bug in UNIX version
C
      DO 10 I=1,8
         XC(I) = 0.0
         YC(I) = 0.0
10    CONTINUE
C
C
      CALL DCC0C7(XT1,YT1,XT2,YT2,XS1,YS1,XS2,DBLE(FRAD),XC,YC,NUM,OK)
C
      CALL DCC0P9(XS1,YS1,XS2,XT1,YT1,XT2,YT2,XP,YP,XP1,YP1,OK)
C
      IF ( .NOT. OK ) THEN
         XP=(DX1+DX2)/2.0
         YP=(DY1+DY2)/2.0
         CALL DCCPLP(XT1,YT1,XT2,YT2,XS1,YS1,XP1,YP1)
         DIST1=DCDD13(XS1,YS1,XP1,YP1,DX1,DY1)
         OK=DIST1.LT.0.0
      ELSE
          DIST = 0.0
          DO 300 I =1,4
              DIST=DIST+DDSTXY(XC(I),YC(I),DX1,DY1)+
     +        DDSTXY(XC(I),YC(I),DX2,DY2)
300       CONTINUE
          DIST1 = 0.0
          DO 310 I =5,8
              DIST1=DIST1+DDSTXY(XC(I),YC(I),DX1,DY1)+
     +        DDSTXY(XC(I),YC(I),DX2,DY2)
310       CONTINUE
C         DIST=DDSTXY(XP,YP,DX1,DY1)+DDSTXY(XP,YP,DX2,DY2)
C         DIST1=DDSTXY(XP1,YP1,DX1,DY1)+DDSTXY(XP1,YP1,DX2,DY2)
         OK=DIST1 .GE. DIST
      END IF
C     Work out which four fillet centres are
C     Closer to the two hit points
      IF ( .NOT.OK ) THEN
         DO 5 P=5,8
            XC(P-4)=XC(P)
            YC(P-4)=YC(P)
    5    CONTINUE
         XP=XP1
         YP=YP1
      END IF
C
C
C      DEBUG FOR FINDING FILLET CENTRES
C      CALL DRWFAW(REAL(XC(1)),REAL(YC(1)),FRAD,0.0,6.28,TFONT)
C      CALL DRWFAW(REAL(XC(2)),REAL(YC(2)),FRAD,0.0,6.28,TFONT)
C      CALL DRWFAW(REAL(XC(3)),REAL(YC(3)),FRAD,0.0,6.28,TFONT)
C      CALL DRWFAW(REAL(XC(4)),REAL(YC(4)),FRAD,0.0,6.28,TFONT)
 
      DIST=DDSTXY(XS1,YS1,DX1,DY1)-DDSTXY(XS1,YS1,XP,YP)
      DIST1=DCDD13(XT1,YT1,XT2,YT2,DX2,DY2)
      CLOCK=DCLKWS(XS1,YS1,DX2,DY2,XP,YP)
C
      IF ( DIST .GT. 0.0 ) THEN
         DIST=DDSTXY(XC(1),YC(1),DX2,DY2)
         DIST1=DDSTXY(XC(2),YC(2),DX2,DY2)
         IF ( DIST1.GT.DIST ) THEN
            P=1
         ELSE
            P=2
         END IF
      ELSE
         DIST=DDSTXY(XC(3),YC(3),DX2,DY2)
         DIST1=DDSTXY(XC(4),YC(4),DX2,DY2)
         IF ( DIST1.GT.DIST) THEN
            P=3
         ELSE
            P=4
         END IF
      END IF
C     P now contains the
      CALL DCCPLP(XT1,YT1,XT2,YT2,XC(P),YC(P),T1,T2)
      CALL DCCPAP(XS1,YS1,XS2,XC(P),YC(P),S1,S2)
C
      IF  ( FILLET .EQ. 2 ) THEN
         SANG1=0.0
         EANG1=PI(2.0)
      ELSE
C
         SANG1=REAL(DCANG(XC(P),YC(P),T1,T2))
         EANG1=REAL(DCANG(XC(P),YC(P),S1,S2))
C
         ANG=ABS(EANG1-SANG1)
         IF ( EANG1 .LT. SANG1 ) THEN
            ANG=PI(2.0)-ANG
         END IF
C
         IF ( FILLET .EQ. 0 ) THEN
            IF ( ANG .GT. PI(1.0) ) THEN
               CALL RSWAP(SANG1,EANG1)
            END IF
         ELSE IF ( FILLET .EQ. 1 ) THEN
            IF ( ANG .LT. PI(1.0) ) THEN
               CALL RSWAP(SANG1,EANG1)
            END IF
         END IF
      END IF
C
C
      IF ( FLTRIM .EQ. 0 ) GOTO 100
C
      IF ( FLTRIM .EQ. 1 .OR. FLTRIM .EQ. 3 ) THEN
C     Trim the first BACK indicated back to the fillet
C
C
         IF ( FIRST ) THEN
C     ARC WAS FIRST
            CALL DER500(TMIP2,OK)
            I4=IMBUFF(6)
CAPOLLO:SUN
            VPMOV = .TRUE.
CAPOLLO:SUN
            CALL PENERS()
            ENT=ARC
            CALL ALLDRW(ENT,TMIP2)
            CALL PENDRW()
C            CALL ERSFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
C     +               RDBUFF(6),I4)
            ANG=REAL(DCANG(XS1,YS1,S1,S2))
            IF ( CLOCK ) THEN
               IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
                  RDBUFF(6)=ANG
                  ANG=REAL(DCANG(XS1,YS1,XP,YP))
                  RDBUFF(5)=ANG
               ELSE
                  RDBUFF(6)=ANG
               END IF
            ELSE
               IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
                  RDBUFF(5)=ANG
                  ANG=REAL(DCANG(XS1,YS1,XP,YP))
                  RDBUFF(6)=ANG
               ELSE
                  RDBUFF(5)=ANG
               END IF
            END IF
            IF ( SAME(RDBUFF(5),RDBUFF(6))  ) THEN
               CALL DEPRNT(226)
            ELSE
               MIP=TMIP2
               CALL DEM500(TMIP2,OK)
            END IF
            ENT=ARC
            CALL ALLDRW(ENT,TMIP2)
         ELSE
            CALL DCVL14(XC(P),YC(P),XP,YP,L1,L2,L3)
            CALL DER500(TMIP1,OK)
            DC(1)=DVDD13(L1,L2,L3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
            DC(2)=DVDD13(L1,L2,L3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
            DC(3)=DVDD13(L1,L2,L3,DX1,DY1)
            I4=IMBUFF(6)
 
CAPOLLO:SUN
            VPMOV = .TRUE.
CAPOLLO:SUN
            CALL PENERS()
            ENT=LINE
            CALL ALLDRW(ENT,TMIP1)
            CALL PENDRW()
C            CALL ERSFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
C     +                  RDBUFF(5),I4)
            IF ( DC(1)*DC(2) .LT. 0.0 ) THEN
               IF ( DC(1)*DC(3) .LT. 0.0 ) THEN
                  RDBUFF(1)=T1
                  RDBUFF(2)=T2
               ELSE
                  RDBUFF(4)=T1
                  RDBUFF(5)=T2
               END IF
            ELSE
               DC(1)=DDSTXY(XP,YP,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
               DC(2)=DDSTXY(XP,YP,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
               IF ( DC(1) .LT. DC(2)  ) THEN
                  RDBUFF(1)=T1
                  RDBUFF(2)=T2
               ELSE
                  RDBUFF(4)=T1
                  RDBUFF(5)=T2
               END IF
            END IF
            IF ( SAME(RDBUFF(1),RDBUFF(4)) .AND.
     +           SAME(RDBUFF(2),RDBUFF(5)) ) THEN
               CALL DEPRNT(196)
            ELSE
               MIP=TMIP1
               CALL DEM500(TMIP1,OK)
            END IF
            ENT=LINE
            CALL ALLDRW(ENT,TMIP1)
         END IF
      END IF
      IF ( FLTRIM .EQ. 2 .OR. FLTRIM .EQ. 3 ) THEN
C     Trim the first BACK indicated back to the fillet
C
         IF ( .NOT. FIRST ) THEN
C     ARC WAS FIRST
C
            CALL DER500(TMIP2,OK)
            I4=IMBUFF(6)
CAPOLLO:SUN
            VPMOV = .TRUE.
CAPOLLO:SUN
            CALL PENERS()
            ENT=ARC
            CALL ALLDRW(ENT,TMIP2)
            CALL PENDRW()
C            CALL ERSFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
C     +               RDBUFF(6),I4)
            ANG=REAL(DCANG(XS1,YS1,S1,S2))
            IF ( CLOCK ) THEN
               IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
                  RDBUFF(6)=ANG
                  ANG=REAL(DCANG(XS1,YS1,XP,YP))
                  RDBUFF(5)=ANG
               ELSE
                  RDBUFF(6)=ANG
               END IF
            ELSE
               IF(ABS(RDBUFF(6)-RDBUFF(5)-PI(2.0)).LT.1E-5)THEN
                  RDBUFF(5)=ANG
                  ANG=REAL(DCANG(XS1,YS1,XP,YP))
                  RDBUFF(6)=ANG
               ELSE
                  RDBUFF(5)=ANG
               END IF
            END IF
            IF ( SAME(RDBUFF(5),RDBUFF(6))  ) THEN
               CALL DEPRNT(226)
            ELSE
               MIP=TMIP2
               CALL DEM500(TMIP2,OK)
            END IF
            ENT=ARC
            CALL ALLDRW(ENT,TMIP2)
         ELSE
            CALL DCVL14(XC(P),YC(P),XP,YP,L1,L2,L3)
            CALL DER500(TMIP1,OK)
            DC(1)=DVDD13(L1,L2,L3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
            DC(2)=DVDD13(L1,L2,L3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
            DC(3)=DVDD13(L1,L2,L3,DX1,DY1)
            I4=IMBUFF(6)
CAPOLLO:SUN
            VPMOV = .TRUE.
CAPOLLO:SUN
            CALL PENERS()
            ENT=LINE
            CALL ALLDRW(ENT,TMIP1)
            CALL PENDRW()
            CALL ERSFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +                  RDBUFF(5),I4)
            IF ( DC(1)*DC(2) .LT. 0.0 ) THEN
               IF ( DC(1)*DC(3) .LT. 0.0 ) THEN
                  RDBUFF(1)=T1
                  RDBUFF(2)=T2
               ELSE
                  RDBUFF(4)=T1
                  RDBUFF(5)=T2
               END IF
            ELSE
               DC(1)=DDSTXY(XP,YP,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
               DC(2)=DDSTXY(XP,YP,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
               IF ( DC(1) .LT. DC(2)  ) THEN
                  RDBUFF(1)=T1
                  RDBUFF(2)=T2
               ELSE
                  RDBUFF(4)=T1
                  RDBUFF(5)=T2
               END IF
            END IF
            IF ( SAME(RDBUFF(1),RDBUFF(4)) .AND.
     +           SAME(RDBUFF(2),RDBUFF(5)) ) THEN
               CALL DEPRNT(196)
            ELSE
               MIP=TMIP1
               CALL DEM500(TMIP1,OK)
            END IF
            ENT=LINE
            CALL ALLDRW(ENT,TMIP1)
         END IF
      END IF
C
 100  CONTINUE
CAPOLLO:SUN
            VPMOV = .FALSE.
            VPADD = .TRUE.
CAPOLLO:SUN
C     store the arc in data base
      CALL DEWC05(REAL(XC(P)),REAL(YC(P)),FRAD,
     +        SANG1,EANG1,CLFONT,CLAYER,P,OK)
C    Finally display arc chosen
      I4=CLFONT
      ENT=ARC
      CALL ALLDRW(ENT,P)
C      CALL DRWFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
C     +            RDBUFF(5),RDBUFF(6),I4)
C
CAPOLLO:SUN
            VPMOV = .FALSE.
            VPADD = .TRUE.
CAPOLLO:SUN
      END
C
      SUBROUTINE INSLLF(TMIP1,XF1,YF1,XF2,YF2,DX1,DY1,
     +                  TMIP2,XG1,YG1,XG2,YG2,DX2,DY2,
     1                                             OK)
C     ================================================
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
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
     1     XC1,YC1,XC2,YC2,XC3,YC3,XC4,YC4,
     2     VAL,DVDD13,DC(1:4),DIST,DDSTXY,XC,YC,
     3     XS1,YS1,XS2,YS2,XT1,YT1,XT2,YT2,DCANG,
     4     DX1,DY1,DX2,DY2,T1,T2,T3,S1,S2,S3,DFRAD,DBLE
C
      INTRINSIC ATAN,DBLE,REAL,ABS
C
      EXTERNAL DCVL14,DVC0P5,DVC0C8,DVDD13,DDSTXY,
     +         DVCPLP,DCANG,RSWAP,DER500,ERSFLW,
     1         DRWFLW,DEM500,DEWC05,DRWFAW
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
      CALL  DVC0C8(L1,L2,L3,M1,M2,M3,DBLE(FRAD),
     +             XC1,YC1,XC2,YC2,XC3,YC3,XC4,YC4,OK)
      CALL DCVL14(X,Y,XC1,YC1,T1,T2,T3)
      VAL=DVDD13(T1,T2,T3,DX1,DY1)*DVDD13(T1,T2,T3,DX2,DY2)
      IF ( VAL .GT. 0.0 ) THEN
         DC(1)=(DDSTXY(DX1,DY1,XC2,YC2)
     1         +DDSTXY(DX2,DY2,XC2,YC2))
         DC(2)=(DDSTXY(DX1,DY1,XC3,YC3)
     1         +DDSTXY(DX2,DY2,XC3,YC3))
         IF ( DC(1) .LT. DC(2) ) THEN
            XC=XC2
            YC=YC2
         ELSE
            XC=XC3
            YC=YC3
         END IF
      ELSE
         DC(1)=(DDSTXY(DX1,DY1,XC1,YC1)
     1         +DDSTXY(DX2,DY2,XC1,YC1))
         DC(2)=(DDSTXY(DX1,DY1,XC4,YC4)
     1         +DDSTXY(DX2,DY2,XC4,YC4))
         IF ( DC(1) .LT. DC(2) ) THEN
            XC=XC1
            YC=YC1
         ELSE
            XC=XC4
            YC=YC4
         END IF
      END IF
C
 305  CONTINUE
      CALL DVCPLP(L1,L2,L3,XC,YC,XT1,YT1)
      CALL DVCPLP(M1,M2,M3,XC,YC,XT2,YT2)
C
      IF  ( FILLET .EQ. 2 ) THEN
         SANG=0.0
         EANG=PI(2.0)
      ELSE
         SANG=REAL(DCANG(XC,YC,XT1,YT1))
         EANG=REAL(DCANG(XC,YC,XT2,YT2))
         ANG=ABS(EANG-SANG)
         IF ( EANG .LT. SANG ) THEN
            ANG=PI(2.0)-ANG
         END IF
         IF ( FILLET .EQ. 0 ) THEN
            IF ( ANG .GT. PI(1.0) ) THEN
               CALL RSWAP(SANG,EANG)
            END IF
         ELSE IF ( FILLET .EQ. 1 ) THEN
            IF ( ANG .LT. PI(1.0) ) THEN
               CALL RSWAP(SANG,EANG)
            END IF
         END IF
      END IF
C
      IF  ( FLTRIM .EQ. 0 ) GOTO 100
      CALL DCVL14(XC,YC,X,Y,L1,L2,L3)
      IF ( FLTRIM .EQ. 1 .OR. FLTRIM .EQ. 3) THEN
C     Trim the first line indicated back to the fillet
C
         CALL DER500(TMIP1,OK)
         DC(1)=DVDD13(L1,L2,L3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
         DC(2)=DVDD13(L1,L2,L3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
         DC(3)=DVDD13(L1,L2,L3,DX1,DY1)
         I4=IMBUFF(6)
CAPOLLO|SUN
C        This will draw the line withot diaply file interference
         VPMOV = .TRUE.
CAPOLLO|SUN
C        erase this line
         CALL PENERS()
         ENT=LINE
         CALL ALLDRW(ENT,TMIP1)
         CALL PENDRW()
C         CALL ERSFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
C     +               RDBUFF(5),I4)
         IF ( DC(1)*DC(2) .LT. 0.0 ) THEN
            IF ( DC(1)*DC(3) .LT. 0.0 ) THEN
               RDBUFF(1)=XT1
               RDBUFF(2)=YT1
            ELSE
               RDBUFF(4)=XT1
               RDBUFF(5)=YT1
            END IF
         ELSE
            DC(1)=DDSTXY(X,Y,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
            DC(2)=DDSTXY(X,Y,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
            IF ( DC(1) .LT. DC(2)  ) THEN
               RDBUFF(1)=XT1
               RDBUFF(2)=YT1
            ELSE
               RDBUFF(4)=XT1
               RDBUFF(5)=YT1
            END IF
         END IF
         IF ( SAME(RDBUFF(1),RDBUFF(4)) .AND.
     +        SAME(RDBUFF(2),RDBUFF(5)) ) THEN
            CALL DEPRNT(196)
         ELSE
            MIP=TMIP1
            CALL DEM500(TMIP1,OK)
         END IF
         ENT=LINE
         CALL ALLDRW(ENT,TMIP1)
      END IF
      IF ( FLTRIM .EQ. 2 .OR. FLTRIM .EQ. 3) THEN
C     Trim the second line back to fillet
         CALL DER500(TMIP2,OK)
C     Calculate which end is nearer to fillet intersection
         DC(1)=DVDD13(L1,L2,L3,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
         DC(2)=DVDD13(L1,L2,L3,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
         DC(3)=DVDD13(L1,L2,L3,DX2,DY2)
         I4=IMBUFF(6)
C        Erase the line or arc
         CALL PENERS()
         ENT=LINE
         CALL ALLDRW(ENT,TMIP2)
         CALL PENDRW()
C         CALL ERSFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
C     +               RDBUFF(5),I4)
         IF ( DC(1)*DC(2) .LT. 0 ) THEN
            IF ( DC(1)*DC(3) .LT. 0.0  ) THEN
               RDBUFF(1)=XT2
               RDBUFF(2)=YT2
            ELSE
               RDBUFF(4)=XT2
               RDBUFF(5)=YT2
            END IF
         ELSE
            DC(1)=DDSTXY(X,Y,DBLE(RDBUFF(1)),DBLE(RDBUFF(2)))
            DC(2)=DDSTXY(X,Y,DBLE(RDBUFF(4)),DBLE(RDBUFF(5)))
            IF ( DC(1) .LT. DC(2)  ) THEN
               RDBUFF(1)=XT2
               RDBUFF(2)=YT2
            ELSE
               RDBUFF(4)=XT2
               RDBUFF(5)=YT2
            END IF
         END IF
         IF ( SAME(RDBUFF(1),RDBUFF(4)) .AND.
     +        SAME(RDBUFF(2),RDBUFF(5)) ) THEN
            CALL DEPRNT(196)
         ELSE
            MIP=TMIP2
            CALL DEM500(TMIP2,OK)
         END IF
         ENT=LINE
         CALL ALLDRW(ENT,TMIP2)
      END IF
C
 
 100  CONTINUE
C
CAPOLLO|SUN
C     reset view flags
      VPMOV = .FALSE.
      VPADD = .TRUE.
CAPOLLO|SUN
C     store as new arc in data base
      CALL DEWC05(REAL(XC),REAL(YC),FRAD,SANG,EANG,CLFONT,CLAYER,P,OK)
C     Finally display arc chosen
      ENT=ARC
      CALL ALLDRW(ENT,P)
C     Return for another shot
C
CAPOLLO|SUN
C     This will draw the line withot diaply file interference
      VPADD = .FALSE.
CAPOLLO|SUN
      END
*
      SUBROUTINE MNIFIL
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
      CALL GTDMEN(130,3)
C2    L is the token for NO TRIMMING.
      CALL GTDMEN(131,3)
C2    F is the token for FIRST ONLY.
      CALL GTDMEN(132,3)
C2    S is the token for SECOND ONLY.
      CALL GTDMEN(133,3)
C2    B is the token for BOTH TRIMMED.
      CALL GTDMEN(134,3)
C     hilite both as default
C      CALL FNDPOS(134,I)
C      CALL GTMCHI(3,I)
C2    O is the token for MINOR ARC.
      CALL GTDMEN(135,3)
C     hilite minor arc as default
C      CALL FNDPOS(135,I)
C      CALL GTMCHI(3,I)
C2    J is the token for MAJOR ARC.
      CALL GTDMEN(136,3)
C2    U is the token for FULL ARC.
      CALL GTDMEN(137,3)
C     cancel option
      CALL GTDMEN(99,3)
C2
      END
C
C
C
      SUBROUTINE WCROSS(WX,WY)
C     ========================
C1    vartype           R  R
C1    iostatus          I  I
C2
C2    draws a cross using XOR raster op on the screen
C2    at the world coords WX,WY.
C
      REAL WX,WY,SX,SY
C
      EXTERNAL WO2SC,CROSS
C
C     convert world to screen coords
      CALL WO2SC(WX,WY,SX,SY)
C     draw the cross on screen
      CALL BCROSS(SX,SY)
C
      END
C
 
C
