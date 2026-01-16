C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 insertl.f   */
C

C
      SUBROUTINE INS0L3()
C     ===================
C1       No arguments passed.
C1       Common blocks used are STYLE,MASTI,MASTC
C2
C2    controls INSERT LINE mode of operation,the appropriate
C2    cell in the major option menu is hilited before entry to
C2    this routine.
C
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/ndata.inc'
      include   'include/nbuff.inc'
      include   'include/lfont.inc'
      include   'include/entity.inc'
      include   'include/gtxt2.inc'
      include   'include/abs.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 TMIP
      INTEGER*2 ENTYPE,TMIP1,FMIPOS,I,ENTYP,TPNT
      INTEGER*2 REFMIP(2),REFENT(2)
      INTEGER*4 C,NPOINT,TMEN,TCELL,TCELL2,MNCODE,MNUM,TMPTYP,
     +   LENGC,LENGM,ANGLTM,ANGLTC,PERPM,PERPC,PARLLM,PARLLC,MOD,
     1   LOCKHC,LOCKHM,LOCKVM,LOCKVC,TANG2M,TANG2C,CP1,MP1,
     2   ANGLAC,ANGLAM,TANG1M,TANG1C,NLEN,ICOM,IMUL,NO,MLEN,CPOS
      REAL SX1,SY1,SX2,SY2,AUX1,AUX2,AUX3,AUX4
      INTEGER*4 IST,IARRAY(30),CPNT,CPNT1,CPNT2
      REAL RARRAY(30),RTOT
      REAL ZERO,DISTXY,X(1:3),Y(1:3),X1,Y1,X2,Y2,
     +     LENGTH,CANG,ANG,SCX,SCY,XP1,YP1,XP2,YP2,PRLDIS,DAT(4)
C
      REAL SXMIN,SYMIN,SXMAX,SYMAX
      REAL XMIN,YMIN,XMAX,YMAX
C
      DOUBLE PRECISION DX1,DY1,DX2,DY2,LL(3),L1,L2,L3,XP,YP,DLENGT,
     +      DRAD,DANGT,DXC1,DYC1,DHTX1,DHTY1,DHTX2,DHTY2,DCANG,
     1      DRADS1,DRADS2,DXC2,DYC2,M1,M2,M3,DDSTXY,DCDD13
      CHARACTER*80 INPL,MLINE
C
      INTEGER*4 HUNIT,I1,I2,ENTPNT,CONJUS,LINVAL,NCNT,LTOT,LTOT1
      INTEGER*2 MIP1,MIP2
      REAL TBUFF(6),BX(2),BY(2),PARSTX,PARSTY
      LOGICAL OK1,CONPAR,RESPON,SAME,LINFST
      DOUBLE PRECISION DV1,DV2,CONDIS,NX1,NY1,NX2,NY2,DDIST
C
      LOGICAL LOCKH,LOCKV,PARLL,PARLD,PERP,TANC1,ANGLTO,LENG,OK,MULTI,
     +        CONTIN,CVERFY,LINDIR,ANGLAB,TANC2,INTOF,FIRST,TFIRST,
     +        MOK,OK2,EXTRA
C
      INTRINSIC REAL,DBLE,COS,SIN,MOD
C
      SAVE DX1,DY1,DX2,DY2
C
      EXTERNAL GTHIMC,WO2SC,CROSS,INSL09,GETANS,UNFLAG,
     +         GTMCWI,GTMCLO,GTMCHI,NLEN,DCDD13,DCANG,
     1         AEXPRN,TCURS,NOSRCH,ADSRCH,DVV0L5,DRAD,DVCPLP,
     2         DCV0L5,DCV0L6,DCV0L4,INTERS,DISTXY,DER500,CVERFY,DCVL15,
     3         DDSTXY,DCVL22,DCC0L2,DCV1L8,CANG,ZERO,DEWC03,DRWFLW,
     4         GTDMCH, GTDMCL, INSATT,SAME
C
C
C
C     set the distance of projection used within intof option
      CONPAR = .FALSE.
      DDIST = DBLE(1000.0)
C     clear temporary menu pointers
      TMEN=3
      TCELL=0
C     set default condition to separate line
C     and highlight menu cell
      CONTIN=.FALSE.
      CALL GTDMCH(91,3)
      TMEN=0
      TCELL=0
      FMIPOS=NMIPOS
C
      INTOF=.FALSE.
      FIRST=.TRUE.
      MULTI=.FALSE.
C
C     start main processing loop-only leave
C     this loop if exitting INSERT LINE function
      REFMIP(1) =0
      REFMIP(2) =0
 5    CONTINUE
C     set searching options
      CALL NOSRCH()
      CALL ADSRCH(LINE)
      CALL ADSRCH(CENLIN)
      CALL ADSRCH(SYMBI)
      CALL ADSRCH(COMPI)
      CALL UNFLAG(.TRUE.)
C     set the point counter to zero to start new line
      NPOINT=0
C
C     In continous second point becomes first for next line
      IF ( CONTIN ) THEN
         X(1)=X(2)
         Y(1)=Y(2)
         NPOINT=1
C        show the first point by a cross
         CALL WO2SC(X(1),Y(1),SCX,SCY)
         CALL CROSS(SCX,SCY)
      END IF
C     clear all line locks
      IF ( ANGLTO ) THEN
         CALL GTMCLO(ANGLTM,ANGLTC)
      ELSE IF ( PARLL ) THEN
        CALL GTMCLO(PARLLM,PARLLC)
      ELSE IF ( LOCKH ) THEN
         CALL GTMCLO(LOCKHM,LOCKHC)
      ELSE IF ( LOCKV ) THEN
         CALL GTMCLO(LOCKVM,LOCKVC)
      ELSE IF ( PARLD ) THEN
        CALL GTMCLO(PARLLM,PARLLC)
      ELSE IF ( PERP ) THEN
         CALL GTMCLO(PERPM,PERPC)
      ELSE IF ( ANGLAB ) THEN
         CALL GTMCLO(ANGLAM,ANGLAC)
      ELSE IF ( TANC2 ) THEN
         CALL GTMCLO(TANG2M,TANG2C)
      ELSE IF ( TANC1 ) THEN
         CALL GTMCLO(TANG1M,TANG1C)
      END IF
      CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,PARLL,PARLD,PERP)
C     no length defined
      LENG=.FALSE.
C     no tangency condition defined
      TANC1=.FALSE.
      TANC2=.FALSE.
C
 10   CONTINUE
C
      IF ( NPOINT .EQ. 0 ) THEN
C        first point of new line
         CALL DCPRNT(250)
      ELSE
C        second point of line
         CALL DCPRNT(192)
      END IF
C
 11   CONTINUE
C     fetch cursor info
      CALL GETANS(C,X1,Y1)
C
 12   CONTINUE
C
C     Menu2 indicates selection of new insert option
C     so return to caller(normally INS00)
C     test for quit character return to caller if quit found
      IF ( MEN .EQ. 2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         IF ( NPOINT .GT. 0 ) THEN
C           Hide the first point by a cross
            CALL WO2SC(X(1),Y(1),SCX,SCY)
            CALL CROSS(SCX,SCY)
         END IF
         IF(CONPAR) CLOSE(UNIT = HUNIT)
         RETURN
      END IF
C
C     set flag to indicate directional properties of line
      LINDIR=ANGLAB .OR. ANGLTO .OR. PERP .OR.
     +        PARLL .OR.  PARLD .OR. LOCKH.OR. LOCKV
C
C******************************************************
C******************************************************
C******************************************************
C                INSERT LINE SUB-OPTIONS              *
C******************************************************
C******************************************************
C******************************************************
C     Test for selection of INSERT LINE sub option menu entry
      IF (MEN.EQ.3) THEN
C        save the menu and cell which brought us here
         TMEN=MEN
         TCELL=CELLN
C******************************************************
C               SIMPLE BOX                            *
C******************************************************
         IF ( CCMD.EQ.'Z') THEN

             IF ( NPOINT.EQ.0) THEN

                 CALL GTDMCL(92,3)
                 CALL GTDMCL(91,3)

1010             CONTINUE
                 CALL GTMCHI(TMEN, TCELL)
                 CALL DCPRNT(843)
                 CALL GETANS(C,SXMIN,SYMIN)
                 IF ( MEN .NE. 0 ) THEN
                       CALL GTMCLO(TMEN, TCELL)
                       GOTO 12
                 ENDIF
                 CALL WO2SC(SXMIN,SYMIN,SCX,SCY)
                 CALL CROSS(SCX,SCY)

                 CALL DCPRNT(842)
                 CALL GETANS(C,SXMAX,SYMAX)
                 CALL CROSS(SCX,SCY)
                 IF ( MEN .NE. 0 ) THEN
                       CALL GTMCLO(TMEN, TCELL)
                       GOTO 12
                 ENDIF
C                abort this function if no valid area returned
                 IF(SXMIN .EQ. SXMAX .OR. SYMIN .EQ. SYMAX) THEN
                   CALL DEPRNT(841)
                   GOTO 1010
                 ENDIF
C
                 ENTYPE=LINE
C                create a box please
                 XMIN = SXMIN + (SXMAX - SXMIN)
                 VPADD = .TRUE.
                 CALL DEWC03(SXMIN,SYMIN,XMIN,SYMIN,CLFONT,
     +                       CLAYER,TMIP1,OK)
C                draw the entity
                 CALL ALLDRW(ENTYPE,TMIP1)
                 CALL DEWC03(XMIN,SYMIN,SXMAX,SYMAX,CLFONT,
     +                       CLAYER,TMIP1,OK)
                 CALL ALLDRW(ENTYPE,TMIP1)
                 YMIN = SYMIN + (SYMAX - SYMIN)
                 CALL DEWC03(SXMAX,SYMAX,SXMIN,YMIN,CLFONT,
     +                       CLAYER,TMIP1,OK)
                 CALL ALLDRW(ENTYPE,TMIP1)
                 CALL DEWC03(SXMIN,YMIN,SXMIN,SYMIN,CLFONT,
     +                       CLAYER,TMIP1,OK)
                 CALL ALLDRW(ENTYPE,TMIP1)

                 CALL GTMCLO(TMEN, TCELL)                                  
C                flag false
                 FIRST=.FALSE.
                 VPADD = .FALSE.

                 GOTO 1010

             ELSE
                 CALL DEPRNT(844)
                 CALL GTMCLO(TMEN, TCELL)                                  
             ENDIF

             GOTO 5

C******************************************************
C               CANCEL LINE                           *
C******************************************************
         ELSEIF ( CCMD.EQ.'c' ) THEN
C           cancel last entity created.
            IF(CONPAR.AND.ENTPNT.GT.1) THEN
                  DO 225 I1 = 1,LTOT
                     CALL INSCNL(FMIPOS,LINE,1,FIRST,OK)
                     IF(ENTPNT.GT.0) ENTPNT = ENTPNT -1
  225             CONTINUE
               IF(ENTPNT.GT.LTOT) THEN
                  DO 227 I1 = 1,LTOT
                     READ(UNIT=HUNIT,REC=ENTPNT-I1,ERR=99)
     +                     MIP1,ENTYPE,BX(1),BY(1),
     +                     TBUFF(1),TBUFF(2),TBUFF(3),
     +                     TBUFF(4),TBUFF(5),TBUFF(6)
                     X(2) = BX(1)
                     Y(2) = BY(1)
C                    Reset the last used position for typed input.
                     ABSX=X(2)
                     ABSY=Y(2)
                     CALL PENERS()
                     CALL ALLDRW(ENTYPE,MIP1)
                     CALL PENDRW()
                     CALL DIR500(MIP1,OK)
C                    Must modify the correct master index data so point to part data
                     TPNT = IMBUFF(7)
                     CALL DBR500(TPNT,OK)
                     DO 229 I2 = 1,6
                        RDBUFF(I2) = TBUFF(I2)
  229                CONTINUE
                     CALL DBM500(TPNT,OK)
                     CALL ALLDRW(ENTYPE,MIP1)
  227             CONTINUE
                  X(1)=BX(1)
                  Y(1)=BY(1)
                  NPOINT=1
               ELSE
                     READ(UNIT=HUNIT,REC=1,ERR=99)
     +                     MIP1,ENTYPE,BX(1),BY(1),
     +                     TBUFF(1),TBUFF(2),TBUFF(3),
     +                     TBUFF(4),TBUFF(5),TBUFF(6)
                     X(2) = BX(1)
                     Y(2) = BY(1)
               END IF
               
            ELSE
               TFIRST=FIRST
               CALL INSCNL(FMIPOS,LINE,1,FIRST,OK)
               IF ( .NOT.TFIRST.AND.CONTIN ) THEN
C                 This will be the point to go from now.
                  X(2)=RDBUFF(1)
                  Y(2)=RDBUFF(2)
C                 Reset the last used position for typed input.
                  ABSX=X(2)
                  ABSY=Y(2)
                  CALL WO2SC(X(1),Y(1),SCX,SCY)
                  CALL CROSS(SCX,SCY)
               END IF
            END IF
            IF ( OK ) GOTO 10
            GOTO 5
*
         END IF
C
C        process all INSERT LINE sub options in here
C
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
         IF (CVERFY(CCMD,'=fk')) THEN
C           ****************************
C           Change line attributes.  
C           ****************************
            CALL INSATT(CCMD)
C           Don't forget to un highlight the "Attributes" cell.
            CALL GTMCLO(TMEN, TCELL)                                  
            GOTO 10
         END IF
C
C******************************************************
C                CONTINUOUS                           *
C******************************************************
         IF (CCMD.EQ.'X'.OR.CCMD.EQ.'x') THEN
C           'x' is for continuous
C           'X' is for between points 
C           CONTINUOUS line option selected
            NPOINT=0
            IF(CONTIN) THEN
               IF(CCMD .EQ. 'X') THEN
                 CALL GTDMCL(92,3)
                 CALL GTDMCH(91,3)
                 CONTIN = .FALSE.
               ENDIF
            ELSE
               IF(CCMD .EQ. 'x') THEN
                 CALL GTDMCL(91,3)
                 CALL GTDMCH(92,3)
                 CONTIN = .TRUE.
               ENDIF 
            ENDIF
            GOTO 10
C           end of continuous line option control block
         END IF
C
C******************************************************
C                L e n g t h                          *
C******************************************************
C
         IF ( CCMD.EQ.'L') THEN
            IF (LENG) THEN
c              switch off since user has hit again
               CALL GTMCLO(MEN,CELLN)
               LENG=.FALSE.
            ELSE
C              set logical
               LENG=.TRUE.
C              Hilite cell
               CALL GTMCHI(MEN,CELLN)
C              Store men,cell for when finished with it
               LENGC=CELLN
               LENGM=MEN
C
C              Prompt for length and return expression.
               GOTO 119
  111          CONTINUE
               CALL DEPRNT(194)
  119          CALL DPRMXP(195,INPL)
C              evaluate an arithmetic expression from the keyboard
               IF ( NLEN(INPL).EQ.0) THEN
c              switch off since user has returned zero length
                  CALL GTMCLO(MEN,CELLN)
                  LENG=.FALSE.
                  GOTO 11
               END IF
               CALL AEXPRN(INPL,DLENGT,*111)
               LENGTH=REAL(DLENGT)
               IF ( LENGTH .EQ.  0.0 ) THEN
                  CALL DEPRNT(196)
                  GOTO 119
               END IF
C
            END IF
            GOTO 10
         END IF
C
C******************************************************
C                I n t o f                            *
C******************************************************
         IF ( CCMD .EQ. 'i' ) THEN
            IF ( INTOF ) THEN
               CALL GTMCLO(MEN,CELLN)
               INTOF=.FALSE.
            ELSE
               IF ( .NOT. LINDIR  ) THEN
                  CALL DEPRNT(197)
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               ELSE IF ( NPOINT .EQ. 0 ) THEN
c                 do not allow parallel at a distance YET
                  CALL DEPRNT(198)
                  CALL GTMCLO(MEN,CELLN)
                  GOTO 10
               ELSE
 101              CONTINUE
C                 get a hit close to the required arc
                  CALL DCPRNT(199)
                  CALL MNUPTS()
                  CALL TCURS(C,X1,Y1)
C                 Load the point modes.
                  CALL MNLPTS()
                  IF ( MEN .NE. 0 ) GOTO 12
C                 got a cursor hit alright
C                 go find the arc for tangency
                  CALL ADSRCH(ARC)
                  CALL DSE801(X1,Y1,OK)
C
                  IF (  OK ) THEN
C
C                    save the mip and entity type to draw out
                     REFMIP(1) = MIP
                     REFENT(1) = IMBUFF(2)
C                    Have to calculate second point on the line
                     IF ( LOCKH ) THEN
                        X(2)=X(1)+100.0
                        Y(2)=Y(1)
                     ELSE IF ( LOCKV ) THEN
                        Y(2)=Y(1)+100.0
                        X(2)=X(1)
                     ELSE IF ( ANGLAB ) THEN
C                       At an angle to an existing line
                        DX1=0.0
                        DY1=1.0
                        DX2=0.0
c                       calculate vector through point X1 Y1
                        CALL DVV0L5(DX1,DY1,DX2,DBLE(X(1)),DBLE(Y(1)),
     +                          DRAD(DANGT),L1,L2,L3)
c                       Now let us calculate a second point along this vector to be used later in inters
                        CALL DVC0P4(DBLE(X(1)),DBLE(Y(1)),DDIST,
     +                               L1,L2,L3,XP,YP)
                        X(2)=REAL(XP)
                        Y(2)=REAL(YP)
                     ELSE IF ( ANGLTO ) THEN
C                       At an angle to an existing line
                        CALL DCV0L5(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                          DBLE(Y(1)),DRAD(DANGT),L1,L2,L3)
                        X1=DRAD(DCANG(DX1,DY1,DX2,DY2)+DANGT)
                        X(2)=X(1)+100.0*COS(X1)
                        Y(2)=Y(1)+100.0*SIN(X1)
                        CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),
     +                               XP,YP)
                        X(2)=REAL(XP)
                        Y(2)=REAL(YP)
                     ELSE IF ( PERP ) THEN
C                       Perpendicular to an existing line
c                       calculate vector through point X1 Y1
                        CALL DCV0L6(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                     DBLE(Y(1)),L1,L2,L3 )
c                       Now let us calculate a second point along this vector to be used later in inters
                        CALL DVC0P4(DBLE(X(1)),DBLE(Y(1)),DDIST,L1,L2,
     +                               L3,XP,YP)
                        X(2)=REAL(XP)
                        Y(2)=REAL(YP)
                     ELSE IF ( PARLL ) THEN
C                       Parallel to an existing line
c                       calculate vector through point X1 Y1
                        CALL DCV0L4(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                           DBLE(Y(1)),L1,L2,L3)
c                       Now let us calculate a second point along this vector to be used later in inters
                        CALL DVC0P4(DBLE(X(1)),DBLE(Y(1)),DDIST,L1,L2,L3
     +                               ,XP,YP)
                        X(2)=REAL(XP)
                        Y(2)=REAL(YP)
                     ELSE IF ( PARLD.OR.CONPAR ) THEN
C                       Parallel at a distance to an existing line
                        CALL DCCPLP(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                              DBLE(Y(1)),XP,YP)
                        X(1)=REAL(XP)
                        Y(1)=REAL(YP)
                        CALL DCV0L6(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                     DBLE(Y(1)),L1,L2,L3 )
                        CALL DVC0P4(DBLE(X(1)),DBLE(Y(1)),
     +                     DBLE(RARRAY(1)),L1,L2,L3,XP,YP)
                        X(1)=REAL(XP)
                        Y(1)=REAL(YP)
                        CALL DCV0L4(DX1,DY1,DX2,DY2,DBLE(X(1)),
     +                           DBLE(Y(1)),L1,L2,L3)
                        CALL DVC0P4(DBLE(X(1)),DBLE(Y(1)),DDIST,
     +                     L1,L2,L3,XP,YP)
                        X(2)=REAL(XP)
                        Y(2)=REAL(YP)
                     END IF
C        
                    IF (IMBUFF(2).EQ.CENLIN) THEN
C                      Entity is center line, pretend it is a line.
                       CALL CLNEND(X1,Y1,X2,Y2)
                       DAT(1)=RDBUFF(1)
                       DAT(2)=RDBUFF(2)
                       DAT(3)=X2
                       DAT(4)=Y2
                       ENTYP=LINE
                    ELSE
C                      Entity is line.
                       DAT(1)=RDBUFF(1)
                       DAT(2)=RDBUFF(2)
                       DAT(3)=RDBUFF(4)
                       DAT(4)=RDBUFF(5)
                       ENTYP=IMBUFF(2)
                    ENDIF
                    AUX1 = X(2)
                    AUX2 = 0.0
                    AUX3 = DAT(3)
                    AUX4 = 0.0
                    CALL INTERS(LINE,X(1),Y(1),X(2),Y(2),AUX1,AUX2,
     +                 ENTYP,DAT(1),DAT(2),DAT(3),DAT(4),AUX3,AUX4,
     +                 XP1,YP1,XP2,YP2,SX1,SY1,SX2,SY2,IST,OK)
C
                     IF ( .NOT. OK ) THEN
                        CALL DEPRNT(157)
                        GOTO 101
                     END IF
C
                     IF ( IMBUFF(2).EQ.ARC ) THEN
                        IF ( DISTXY(X1,Y1,XP1,YP1) .LT.
     +                       DISTXY(X1,Y1,XP2,YP2) ) THEN
                           X(2)=XP1
                           Y(2)=YP1
                        ELSE
                           X(2)=XP2
                           Y(2)=YP2
                        END IF
                     ELSE
                        X(2)=XP1
                        Y(2)=YP1
                     END IF
                     CALL GTMCLO(TMEN,TCELL)
                     GOTO 13
                  ELSE
                     CALL DEPRNT(142)
                     GOTO 101
                  END IF
               END IF
            END IF
         END IF
C
C******************************************************
C                T A N G E N T   1  a r c s           *
C******************************************************
         IF ( CCMD.EQ.'F'  ) THEN
C           tangency condition requested
            IF ( TANC1 ) THEN
C              cancel previous selection
               CALL GTMCLO(MEN,CELLN)
               TANC1=.FALSE.
            ELSE
C
               TANC1=.TRUE.
               TANG1C=CELLN
               TANG1M=MEN
 21            CONTINUE
C              get a hit close to the required arc
               CALL DCPRNT(201)
               CALL MNUPTS()
               CALL TCURS(C,X1,Y1)
C              Load the point modes.
               CALL MNLPTS()
               IF ( MEN .NE. 0 ) THEN
C                 Hit another menu must cancel this option
C                 and process the new one
                  TANC1=.FALSE.
                  CALL GTMCLO(TANG1M,TANG1C)
                  CALL GTMCLO(MEN,CELLN)
                  IF ( MEN.EQ.3.AND.CCMD.EQ.'F') THEN
                     GOTO 10
                  ELSE
                     GOTO 12
                  END IF
               END IF
C              got a cursor hit alright
C              save the hit point for future reference
               DHTX1=DBLE(X1)
               DHTY1=DBLE(Y1)
               CALL ADSRCH(ARC)
C              go find the arc for tangency
               CALL DSE801(X1,Y1,OK)
C
               IF (  OK.AND.IMBUFF(2).EQ.ARC ) THEN
C
C                 save the mip and entity type to draw out
                  REFMIP(1) = MIP
                  REFENT(1) = IMBUFF(2)
C                 Recover ARC from data base
C                  CALL DER500(MIP,OK)
                  DXC1=DBLE(RDBUFF(1))
                  DYC1=DBLE(RDBUFF(2))
                  DRADS1=DBLE(RDBUFF(4))
C                 If not lindir then have to store hit point
C                 to ensure finding correct tangent
                  X(3)=X1
                  Y(3)=Y1
               ELSE
                  CALL DEPRNT(142)
                  GOTO 21
               END IF
            END IF
            GOTO 10
         END IF
C
C******************************************************
C                T A N G E N T   2  a r c s           *
C******************************************************
         IF ( CCMD.EQ.'G'  ) THEN
C           code added to unhighlight any entity that
C           was previously highlighted MM 20/07/88
            IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
            IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
C           tangency condition requested
            IF ( TANC2 ) THEN
C              cancel previous selection
               CALL GTMCLO(MEN,CELLN)
               TANC2=.FALSE.
            ELSE
               IF ( LINDIR ) THEN
                  CALL GTHIMC(TMEN,' ','ABVHpPR',TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                PARLL,PARLD,PERP)
               END IF
               TANC2=.TRUE.
               TANG2C=CELLN
               TANG2M=MEN
 23            CONTINUE
C              get a hit close to the required arc
               CALL DCPRNT(203)
               CALL MNUPTS()
               CALL TCURS(C,X1,Y1)
C              Load the point modes.
               CALL MNLPTS()
               IF ( MEN .NE. 0 ) THEN
C                 Hit another menu must cancel this option
C                 and process the new one
                  TANC2=.FALSE.
                  CALL GTMCLO(TANG2M,TANG2C)
                  CALL GTMCLO(MEN,CELLN)
                  IF ( MEN.EQ.3.AND.CCMD.EQ.'G') THEN
                     GOTO 10
                  ELSE
                     GOTO 12
                  END IF
               END IF
C              got a cursor hit alright
C              save the hit point for future reference
               DHTX1=DBLE(X1)
               DHTY1=DBLE(Y1)
C              go find the arc for tangency
               CALL ADSRCH(ARC)
               CALL DSE801(X1,Y1,OK)
C
               IF (  OK.AND.IMBUFF(2).EQ.ARC  ) THEN
C
C                 save the mip and entity type to draw out
                  REFMIP(1) = MIP
                  REFENT(1) = IMBUFF(2)
C                 Recover ARC from data base
                  DXC1=DBLE(RDBUFF(1))
                  DYC1=DBLE(RDBUFF(2))
                  DRADS1=DBLE(RDBUFF(4))
                  CALL DIR500(MIP,OK)
                  IF(IMBUFF(2).EQ.SYMBI.OR.IMBUFF(2).EQ.COMPI) THEN
                     TMIP1 =IDBUFF(2)
                  ELSE
                     TMIP1=MIP
                  ENDIF
               ELSE
                  CALL DEPRNT(142)
                  GOTO 23
               END IF
C
 24            CONTINUE
C              get a hit close to the required arc
               CALL DCPRNT(204)
               CALL MNUPTS()
               CALL TCURS(C,X1,Y1)
C              Load the point modes.
               CALL MNLPTS()
               IF ( MEN .NE. 0 ) THEN
C                 Hit another menu must cancel this option
C                 and process the new one
                  TANC2=.FALSE.
                  CALL GTMCLO(TANG2M,TANG2C)
                  CALL GTMCLO(MEN,CELLN)
                  IF ( MEN.EQ.3.AND.CCMD.EQ.'G') THEN
                     GOTO 10
                  ELSE
                     GOTO 12
                  END IF
               END IF
C              got a cursor hit alright
C              save the hit point for future reference
               DHTX2=DBLE(X1)
               DHTY2=DBLE(Y1)
C              go find the arc for tangency
               CALL DSE801(X1,Y1,OK)
C
               IF (  OK.AND.IMBUFF(2).EQ.ARC ) THEN
C                 Recover ARC from data base
                  CALL DIR500(MIP,OK)
                  IF(IMBUFF(2).EQ.SYMBI.OR.IMBUFF(2).EQ.COMPI) THEN
                     IF ( IDBUFF(2) .EQ. TMIP1 ) THEN
                        CALL DEPRNT(224)
                        GOTO 24
                     ENDIF
                  ELSE IF ( MIP .EQ. TMIP1 ) THEN
                     CALL DEPRNT(224)
                     GOTO 24
                  END IF
C                 save the mip and entity type to draw out
                  REFMIP(2) = MIP
                  REFENT(2) = IMBUFF(2)
                  DXC2=DBLE(RDBUFF(1))
                  DYC2=DBLE(RDBUFF(2))
                  DRADS2=DBLE(RDBUFF(4))
               ELSE
                  CALL DEPRNT(142)
                  GOTO 24
               END IF
            END IF
            GOTO 10
         END IF
C
C************************************************************
C************************************************************
C*       START OF THE LINE DIRECTION SUB-MENU               *
C************************************************************
C************************************************************
         TMEN=MEN
         TCELL=CELLN
C
         IF (CVERFY(CCMD,'ABVHpPR')) THEN
C           ensure only one cell is highlighted
            CALL GTHIMC(TMEN,CCMD,'ABVHpPR',TCELL)
C******************************************************
C                H o r i z o n t a l                  *
C******************************************************
            IF (CCMD.EQ.'H') THEN
C
               IF (LOCKH) THEN
C                 switch off horizontal hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                PARLL,PARLD,PERP)
               ELSE
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                PARLL,PARLD,PERP)
                  LOCKH=.TRUE.
                  CALL GTMCHI(TMEN,TCELL)
                  LOCKHC=TCELL
                  LOCKHM=TMEN
C                 leave knowing that cell is hilited
               END IF
               GOTO 10
C******************************************************
C                V e r t i c a l  l i n e             *
C******************************************************
            ELSE IF (CCMD.EQ.'V') THEN
               IF (LOCKV) THEN
C                 switch off vertical hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               ELSE
C                 set them all to .false. because only 1 of the 5
C                 can be lit at any one time
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  LOCKV=.TRUE.
                  CALL GTMCHI(TMEN,TCELL)
                  LOCKVC=TCELL
                  LOCKVM=TMEN
C                 leave knowing that cell is hilited
               END IF
               GOTO 10
C******************************************************
C                P a r a l l e l  l i n e             *
C******************************************************
            ELSE IF ( CCMD .EQ. 'P' ) THEN
C              code added to unhighlight any entity that
C              was previously highlighted MM 20/07/88
               IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
               IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
C
               IF (PARLL) THEN
C                 switch off parallel hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               ELSE
c                 set them all to .false. because only 1 of the 5
C                 can be lit at any one time
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  PARLL=.TRUE.
C                 hilight that cell since it's set
                  CALL GTMCHI(TMEN,TCELL)
                  PARLLC=TCELL
                  PARLLM=TMEN
                  CALL DCPRNT(205)
 14               CALL GETANS(C,X1,Y1)
C
C                 Hit another menu go find out what
                  IF ( MEN .NE. 0 ) THEN
C                    Hit another menu must cancel this option
C                    and process the new one
                     PARLL=.FALSE.
                     CALL GTMCLO(PARLLM,PARLLC)
                     IF ( MEN.EQ.3.AND.CCMD.EQ.'P') THEN
                        GOTO 10
                     ELSE
                        GOTO 12
                     END IF
                  END IF
C                 go find the line for reference
                  CALL NOSRCH()
                  CALL ADSRCH(LINE)
                  CALL ADSRCH(CENLIN)
                  CALL ADSRCH(SYMBI)
                  CALL ADSRCH(COMPI)
                  CALL DSE801(X1,Y1,OK)
                  IF (  OK ) THEN
                     TMPTYP = IMBUFF(2)
C                    Recover from data base
                     CALL DIR500(MIP,OK)
C                    only read the part data if not comp master
                     IF(IMBUFF(2).NE.COMPI.AND.IMBUFF(2).NE.SYMBI) THEN
                         CALL DBR500(IMBUFF(7),OK)
                     ENDIF
C
C                    save the mip and entity type to draw out
                     REFMIP(1) = MIP
                     REFENT(1) = IMBUFF(2)
                     IF (TMPTYP.EQ.CENLIN) THEN
C                       Entity is center line, pretend it is a line.
                        CALL CLNEND(X1,Y1,X2,Y2)
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(X2)
                        DY2=DBLE(Y2)
                     ELSE
C                       Entity is line.
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(RDBUFF(4))
                        DY2=DBLE(RDBUFF(5))
                     ENDIF
                  ELSE
C                    line not found go back and ask for another hit
C                    if the user hit parallel at this point this will
C                    take him out of the option.
                     CALL DEPRNT(106)
                     GOTO 14
                  END IF
               END IF
               GOTO 10
C******************************************************
C               C o n t i n u o u s   P a r a l l e l *
C******************************************************
            ELSE IF ( CCMD .EQ. 'p'.AND.CONTIN ) THEN
C        
               NPOINT = 0
               IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
               IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
C
               CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               IF (CONPAR) THEN
C                 switch off parallel hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
C                 reset logical 
                  CONPAR =.FALSE.
                  CLOSE(UNIT=HUNIT)
               ELSE
 223                 CONTINUE
                     CALL DPRMXP(208,INPL)
C                    evaluate an arithmetic expression from
C                    the keyboard
                     IF ( NLEN(INPL).EQ.0) THEN
C                       switch off since user has returned zero length
                        CALL GTMCLO(TMEN,TCELL)
                        CONPAR=.FALSE.
                        GOTO 11
                     END IF
C                    evaluate an arithmetic expression from
C                    the keyboard
                     CALL SEPAR(INPL,IARRAY,RARRAY,CPNT,LTOT,RTOT,OK2)
                     IF ( CPNT.EQ.0) THEN
C                       switch off since user has returned zero length
                        CALL GTMCLO(PARLLM,PARLLC)
                        PARLD=.FALSE.
                        GOTO 11
                     END IF
                     ENTPNT=52
                     CALL OURSCR(HUNIT,ENTPNT,OK)
                     IF ( .NOT. OK ) THEN
C                       This would be a strange thing but it could happen
                        CALL DCPRNT(143)
                        CALL DEPRNT(90)
                        RETURN
                     END IF
                     ENTPNT = 1
                     LINFST = .TRUE.
                     CONPAR = .TRUE.
                     CONTIN = .TRUE.
                     PARLD = .TRUE.
                     CALL DPRMXP(827,INPL)
                     CALL CRUNCH(INPL)
C                    default left justified
                     IF(RESPON(INPL(1:1),17)) THEN
                       DO  333  I1 = 1,CPNT
c                      also loop round the the number time for every separation 3@4 etc
                            RARRAY(I1) = -RARRAY(I1)
 333                   CONTINUE                      
C                    -----------------------------------
                     ELSE  IF(RESPON(INPL(1:1),19)) THEN
c
c                      CPNT1 points to start CPNT2 points to end
c                      CPNT points to the current pointer
c
                       CPNT1 = 1
                       CPNT2 = CPNT
                       IF(LTOT.GT.0) THEN
C                        ignore a zero distance
                         IF(SAME(0.0,RARRAY(1))) THEN
                           CPNT1 = CPNT1+1
                         END IF
c                        if there is more than one line
                         IF(LTOT.GT.0) THEN
                           CPNT = CPNT+1
                           RARRAY(CPNT) = -RARRAY(CPNT1) - RTOT
                           IARRAY(CPNT) =  1
                           LTOT = LTOT + 1
                           EXTRA = .TRUE.
c                          if there was more than one of this length
                           IF(IARRAY(CPNT1).EQ.1) THEN
                              CPNT1 = CPNT1+1
                              EXTRA = .FALSE.
                           END IF
C
                           DO  334  I1 = CPNT1,CPNT2
c                          also loop round the the number time for every separation 3@4 etc
                                CPNT = CPNT+1
                                RARRAY(CPNT) = -RARRAY(I1)
C                               if there was an extra elemnt then decrement
                                IF(EXTRA) THEN
                                   IARRAY(CPNT) =  IARRAY(I1) -1
                                   EXTRA = .FALSE.
                                ELSE
                                   IARRAY(CPNT) =  IARRAY(I1)
                                END IF
                                LTOT=LTOT+IARRAY(CPNT)
 334                       CONTINUE
                         END IF
                       END IF    
                     END IF
               END IF      
               GOTO 11
C******************************************************
C                P a r a l l e l  D i s t a n c e     *
C******************************************************
            ELSE IF ( CCMD .EQ. 'p'.AND..NOT.CONTIN ) THEN
C              code added to unhighlight any entity that
C              was previously highlighted MM 20/07/88
               IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
               IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
C
               IF (PARLD) THEN
C                 switch off parallel hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
C                 reset multiple line option.
                  PARLD=.FALSE.
               ELSE
C                 set them all to .false. because only 1 of the 5
C                 can be lit at any one time
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  PARLD=.TRUE.
C                 hilight that cell since it's set
                  CALL GTMCHI(TMEN,TCELL)
                  PARLLC=TCELL
                  PARLLM=TMEN
                  CALL DCPRNT(205)
 41               CONTINUE
                  CALL MNUPTS()
                  CALL TCURS(C,X1,Y1)
C                 Load the point modes.
                  CALL MNLPTS()
C
C                 Hit another menu go find out what
                  IF ( MEN .NE. 0 ) THEN
C                    Hit another menu must cancel this option
C                    and process the new one
                     MULTI=.FALSE.
                     PARLL=.FALSE.
                     PARLD=.FALSE.
                     CALL GTMCLO(PARLLM,PARLLC)
                     CALL GTMCLO(MEN,CELLN)
                     IF ( MEN.EQ.3.AND.CCMD.EQ.'p') THEN
                        GOTO 10
                     ELSE
                        GOTO 12
                     END IF
                  END IF
C                 go find the line for reference
                  CALL NOSRCH()
                  CALL ADSRCH(LINE)
                  CALL ADSRCH(CENLIN)
                  CALL ADSRCH(SYMBI)
                  CALL ADSRCH(COMPI)
                  CALL DSE801(X1,Y1,OK)
                  IF (  OK ) THEN
                     TMPTYP = IMBUFF(2)
C                    Recover from data base
                     CALL DIR500(MIP,OK)
C                    only read the part data if not comp instance
                     IF(IMBUFF(2).NE.COMPI.AND.IMBUFF(2).NE.SYMBI) THEN
                         CALL DBR500(IMBUFF(7),OK)
                     ENDIF
C                    save the mip and entity type to draw out
C
                     REFMIP(1) = MIP
                     REFENT(1) = IMBUFF(2)
                     IF (TMPTYP.EQ.CENLIN) THEN
C                       Entity is center line, pretend it is a line.
                        CALL CLNEND(X1,Y1,X2,Y2)
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(X2)
                        DY2=DBLE(Y2)
                     ELSE
C                       Entity is line.
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(RDBUFF(4))
                        DY2=DBLE(RDBUFF(5))
                     ENDIF
C
                     CALL DCPRNT(207)
 42                  CONTINUE
                     CALL MNUPTS()
                     CALL TCURS(C,X1,Y1)
C                    Load the point modes.
                     CALL MNLPTS()
c 42                  CALL GETANS(C,X1,Y1)
C                    Hit another menu go find out what
                     IF ( MEN .NE. 0 ) THEN
C                      Hit another menu must cancel this option
C                      and process the new one
                       PARLD=.FALSE.
                       CALL GTMCLO(PARLLM,PARLLC)
                       IF ( MEN.EQ.3.AND.CCMD.EQ.'p') THEN
                         GOTO 10
                       ELSE
                         GOTO 12
                       END IF
                     END IF
                     DHTX1=DBLE(X1)
                     DHTY1=DBLE(Y1)
C
 203                 CONTINUE
                     CALL DPRMXP(208,INPL)
C                    evaluate an arithmetic expression from
C                    the keyboard
                     CALL SEPAR(INPL,IARRAY,RARRAY,CPNT,LTOT,RTOT,OK2)
                     IF ( CPNT.EQ.0) THEN
C                       switch off since user has returned zero length
                        CALL GTMCLO(PARLLM,PARLLC)
                        PARLD=.FALSE.
                        GOTO 11
                     END IF
                     PARLD = (CPNT.GT.0)
                     IF (DCDD13(DX1,DY1,DX2,DY2,DHTX1,DHTY1)
     +                                               .LT.0 ) THEN
                           CALL DSWAP(DX1,DX2)
                           CALL DSWAP(DY1,DY2)
                     END IF 
                  ELSE
C                    line not found go back and ask for another hit
C                    if the user hit parallel at this point this will
C                    take him out of the option.
                     CALL DEPRNT(106)
                     GOTO 41
                  END IF
               END IF
               GOTO 10
C******************************************************
C              P e r p e n d i c u l a r              *
C******************************************************
            ELSE IF ( CCMD.EQ.'R') THEN
C              code added to unhighlight any entity that
C              was previously highlighted MM 20/07/88
               IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
               IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
C
               IF (PERP) THEN
C                 switch off perpendicular hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               ELSE
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  PERP=.TRUE.
C                 hilight that cell since it's set
                  CALL GTMCHI(TMEN,TCELL)
                  PERPC=TCELL
                  PERPM=TMEN
C
 15               CONTINUE
                  CALL DCPRNT(205)
                  CALL GETANS(C,X1,Y1)
C
C                 Hit another menu go find out what
                  IF ( MEN .NE. 0 ) GOTO 12
                  CALL NOSRCH()
                  CALL ADSRCH(LINE)
                  CALL ADSRCH(CENLIN)
                  CALL ADSRCH(SYMBI)
                  CALL ADSRCH(COMPI)
                  CALL DSE801(X1,Y1,OK)
                  IF (  OK ) THEN
C                    Recover from data base
                     CALL DIR500(MIP,OK)
C                    only read the part data if not comp instance
                     IF(IMBUFF(2).NE.COMPI.AND.IMBUFF(2).NE.SYMBI) THEN
                         CALL DBR500(IMBUFF(7),OK)
                     ENDIF
C                    Recover from data base
                     IF (IMBUFF(2).EQ.CENLIN) THEN
C                       Entity is center line, pretend it is a LINE.
                        CALL CLNEND(X1,Y1,X2,Y2)
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(X2)
                        DY2=DBLE(Y2)
                     ELSE
C                       Entity is line.
                        DX1=DBLE(RDBUFF(1))
                        DY1=DBLE(RDBUFF(2))
                        DX2=DBLE(RDBUFF(4))
                        DY2=DBLE(RDBUFF(5))
                     ENDIF
C
C                    save the mip and entity type to draw out
                     REFMIP(1) = MIP
                     REFENT(1) = IMBUFF(2)
C                    cancel flag on line
                  ELSE
C                    line not found go back and ask for another hit
C                    if the user hit perpendicular at this point
C                    this will take him out of the option.
                     CALL DEPRNT(106)
                     GOTO 15
                  END IF
               END IF
               GOTO 10
C******************************************************
C            A n g l e   R e l a t i v e               *
C******************************************************
            ELSE IF ( CCMD.EQ.'A') THEN
C              code added to unhighlight any entity that
C              was previously highlighted MM 20/07/88
               IF(REFMIP(1).GT.0) CALL ALLDRW(REFENT(1),REFMIP(1))
               IF(REFMIP(2).GT.0) CALL ALLDRW(REFENT(2),REFMIP(2))
               IF (ANGLTO) THEN
C                 switch off angle hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               ELSE
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  ANGLTO=.TRUE.
C                 hilight that cell since it's set
                  CALL GTMCHI(TMEN,TCELL)
                  ANGLTC=TCELL
                  ANGLTM=TMEN
 16               CONTINUE
                  CALL DCPRNT(205)
                  CALL GETANS(C,X1,Y1)
C
C                 if hit another insert option,go find out which
                  IF ( MEN .NE. 0 ) GOTO 12
C                 search for a line close to the hit point
                  CALL NOSRCH()
                  CALL ADSRCH(LINE)
                  CALL ADSRCH(CENLIN)
                  CALL ADSRCH(SYMBI)
                  CALL ADSRCH(COMPI)
                  CALL DSE801(X1,Y1,OK)
                  IF (  OK ) THEN
C                    Recover from data base
                     CALL DIR500(MIP,OK)
C                    only read the part data if not comp instance
                     IF(IMBUFF(2).NE.COMPI.AND.IMBUFF(2).NE.SYMBI) THEN
                         CALL DBR500(IMBUFF(7),OK)
                     ENDIF
C                   Recover from data base
                    IF (IMBUFF(2).EQ.CENLIN) THEN
C                      Entity is center line, pretend it is a LINE.
                       CALL CLNEND(X1,Y1,X2,Y2)
                       DX1=DBLE(RDBUFF(1))
                       DY1=DBLE(RDBUFF(2))
                       DX2=DBLE(X2)
                       DY2=DBLE(Y2)
                    ELSE
C                      Entity is line.
                       DX1=DBLE(RDBUFF(1))
                       DY1=DBLE(RDBUFF(2))
                       DX2=DBLE(RDBUFF(4))
                       DY2=DBLE(RDBUFF(5))
                    ENDIF
C
C                   save the mip and entity type to draw out
                    REFMIP(1) = MIP
                    REFENT(1) = IMBUFF(2)
C                   now got the reference line data.
                    GOTO 120
 112                CONTINUE
                    CALL DEPRNT(211)
C                   go get the angle required
 120                CALL DPRMXP(212,INPL)
C                   evaluate an arithmetic expression from the keyboard
                    IF ( NLEN(INPL).EQ.0) THEN
C                      switch off since user has returned zero length
                       CALL GTMCLO(ANGLTM,ANGLTC)
                       ANGLTO=.FALSE.
                       GOTO 11
                    END IF
                    CALL AEXPRN(INPL,DANGT,*112)
C                   cancel flag on line
                 ELSE
C                   line not found go back and ask for another hit
C                   if the user hit angle at this point this will
C                   take him out of the option.
                    CALL DEPRNT(106)
                    GOTO 16
                  END IF
               END IF
C
               GOTO 10
C******************************************************
C            A n g l e   A b s o l u t e              *
C******************************************************
            ELSE IF ( CCMD.EQ.'B') THEN
               IF (ANGLAB) THEN
C                 switch off angle hilight since user
C                 has hit twice in a row
                  CALL GTMCLO(TMEN,TCELL)
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
               ELSE
                  CALL INSL09(ANGLTO,ANGLAB,LOCKV,LOCKH,
     +                                 PARLL,PARLD,PERP)
                  ANGLAB=.TRUE.
C                 hilight that cell since it's set
                  CALL GTMCHI(TMEN,TCELL)
                  ANGLAC=TCELL
                  ANGLAM=TMEN
C                 go get the angle required
 113              CALL DPRMXP(213,INPL)
C                 evaluate an arithmetic expression from the keyboard
                  IF ( NLEN(INPL).EQ.0) THEN
C                    switch off since user has returned zero length
                     CALL GTMCLO(ANGLAM,ANGLAC)
                     ANGLAB=.FALSE.
                     GOTO 11
                  END IF
                  CALL AEXPRN(INPL,DANGT,*113)
C
               END IF
C
               GOTO 10
            END IF
C           end of menu2 block
         END IF
C************************************************************
C************************************************************
C        End of line direction control block                *
C************************************************************
C************************************************************
      END IF
C************************************************************
C************************************************************
C************************************************************
C       END OF INSERT LINE LINE SUB-OPTION CONTROL          *
C************************************************************
C************************************************************
C************************************************************
C
      NPOINT=NPOINT+1
C
      X(NPOINT) = X1
      Y(NPOINT) = Y1
C
      IF (NPOINT.EQ.1) THEN
C        show the hit on screen by a small cross
         CALL WO2SC(X(1),Y(1),SCX,SCY)
         CALL CROSS(SCX,SCY)
      ELSE
C        clear the cross for the start point
         CALL WO2SC(X(1),Y(1),SCX,SCY)
         CALL CROSS(SCX,SCY)
      END IF
C
      IF (NPOINT.LT.2) GOTO 10
C
 13   CONTINUE
C
C
C************************************************************
C************************************************************
C************************************************************
C****      ALL LINE DEFINITION COMPLETE                  ****
C****      START OF DATABASE AND DISPLAY UPDATE          ****
C************************************************************
C************************************************************
C************************************************************
      IF ( LOCKH ) THEN
C        Horizontal line
         CALL GTMCLO(LOCKHM,LOCKHC)
         Y(2)=Y(1)
      ELSE IF ( LOCKV ) THEN
C        Vertical line
         CALL GTMCLO(LOCKVM,LOCKVC)
         X(2)=X(1)
      ELSE IF ( PARLL.OR.(PARLD.AND..NOT.CONPAR) ) THEN
C        Parallel to an existing line
         CALL GTMCLO(PARLLM,PARLLC)
         CALL DCV0L4(DX1,DY1,DX2,DY2,DBLE(X(1)),DBLE(Y(1)),
     +                  L1,L2,L3 )
         CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
         X(2)=REAL(XP)
         Y(2)=REAL(YP)
C
      ELSE IF ( PERP  ) THEN
C        Perpendicular to an existing line
         CALL GTMCLO(PERPM,PERPC)
         CALL DCV0L6(DX1,DY1,DX2,DY2,DBLE(X(1)),DBLE(Y(1)),
     +                  L1,L2,L3 )
         CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
         X(2)=REAL(XP)
         Y(2)=REAL(YP)
      ELSE IF ( ANGLTO ) THEN
C        At an angle to an existing line
         CALL GTMCLO(ANGLTM,ANGLTC)
         CALL DCV0L5(DX1,DY1,DX2,DY2,DBLE(X(1)),DBLE(Y(1)),
     +                DRAD(DANGT),L1,L2,L3)
         CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
         X(2)=REAL(XP)
         Y(2)=REAL(YP)
      ELSE IF ( ANGLAB ) THEN
C        At an angle to an existing line
         CALL GTMCLO(ANGLAM,ANGLAC)
         DX1=0.0
         DY1=1.0
         DX2=0.0
C      WRITE(10,*) '[INSERL] DANGT= ',DRAD(DANGT)
C      WRITE(10,*) '[INSERL] REAL X(1) Y(1)= ',X(1),Y(1)
         CALL DVV0L5(DX1,DY1,DX2,DBLE(X(1)),DBLE(Y(1)),
     +                DRAD(DANGT),L1,L2,L3)
         CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
C      WRITE(10,*) '[INSERL] XP YP =',xp,yp
         X(2)=REAL(XP)
         Y(2)=REAL(YP)
C      WRITE(10,*) '[INSERL] X(2) Y(2)= ',X(2),Y(2)
C
C
      ELSE IF ( TANC2 ) THEN
         CALL GTMCLO(TANG2M,TANG2C)
         CALL  DCVL22(DXC1,DYC1,DRADS1,DXC2,DYC2,DRADS2,
     +                  DHTX1,DHTY1,DHTX2,DHTY2,LL,OK)
         IF ( OK ) THEN
            CALL DVCPLP(LL(1),LL(2),LL(3),DBLE(X(1)),DBLE(Y(1)),XP,YP)
            X(1)=REAL(XP)
            Y(1)=REAL(YP)
            CALL DVCPLP(LL(1),LL(2),LL(3),DBLE(X(2)),DBLE(Y(2)),XP,YP)
            X(2)=REAL(XP)
            Y(2)=REAL(YP)
         ELSE
            GOTO 5
         END IF
C
      END IF
C
      IF (TANC1) THEN
C        Tangent to an existing circle
C
C        ensure menu cell switched off
         CALL GTMCLO(TANG1M,TANG1C)
         IF (LINDIR) THEN
C           directional control already set for the line
C           must adhere to that condition
C           generate the tangent line
            CALL DCC0L2(DBLE(X(1)),DBLE(Y(1)),DBLE(X(2)),DBLE(Y(2)),
     +      DXC1,DYC1,DRADS1,DHTX1,DHTY1,DX1,DY1,DX2,DY2)
C           save end points
            X(1)=REAL(DX1)
            X(2)=REAL(DX2)
            Y(1)=REAL(DY1)
            Y(2)=REAL(DY2)
         ELSE
C           Stored hit point X(3),Y(3)
C
            CALL DCV1L8(DBLE(X(1)),DBLE(Y(1)),DBLE(X(3)),DBLE(Y(3)),
     +                   DXC1,DYC1,DRADS1,L1,L2,L3,OK)
            IF ( OK ) THEN
               CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
               X(2)=REAL(XP)
               Y(2)=REAL(YP)
            ELSE
C
               CALL DEPRNT(214)
               GOTO  5
            END IF
         END IF
      END IF
C
C     Trim the line to length if necessary
      IF (LENG) THEN
         LENG=.FALSE.
         CALL GTMCLO(LENGM,LENGC)
         ANG=CANG(X(1),Y(1),X(2),Y(2))
         X(2)=X(1)+LENGTH*COS(ANG)
         Y(2)=Y(1)+LENGTH*SIN(ANG)
      END IF
C
      IF ( ZERO(DISTXY(X(1),Y(1),X(2),Y(2))) .EQ. 0.0 ) THEN
         NPOINT=NPOINT-1
         CALL DEPRNT(94)
         GOTO 10
      END IF
C
      IF(CONPAR) THEN
        DX1 = DBLE(X(1))
        DY1 = DBLE(Y(1))
        DX2 = DBLE(X(2))
        DY2 = DBLE(Y(2))
      END IF
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     This is the section that builds up parallel lines at a distance
      IF (PARLD.OR.CONPAR) THEN
         PRLDIS = 0.0
c        loop round the number of different line separations
         DO  444  I1 = 1,CPNT
c           also loop round the the number time for every separation 3@4 etc
            DO  445  I2 = 1,IARRAY(I1)
C              the first distance has already been calculated so only add
C              on after the first time through
               PRLDIS=PRLDIS + RARRAY(I1)
c              Now let us calculate the lines parallel
C               (Note DX1,DY1 and DX2,DY2 have already been sorted
c                so that L1,L2,L3 is the correct vector to use)
               CALL DCVL15(DX1,DY1,DX2,DY2,DBLE(PRLDIS),
     +                     L1,L2,L3,M1,M2,M3)
c              we already know L1,L2,L3 is the correct vector so let us calculate 
C              the new start point
               CALL DVCPLP(L1,L2,L3,DBLE(X(1)),DBLE(Y(1)),XP,YP)
               X(1)=XP
               Y(1)=YP
c              we already know L1,L2,L3 is the correct vector so let us calculate 
C              the new finish point
               CALL DVCPLP(L1,L2,L3,DBLE(X(2)),DBLE(Y(2)),XP,YP)
               X(2)=XP
               Y(2)=YP
C              store the newly constructed line.
               CALL DEWC03(X(1),Y(1),X(2),Y(2),CLFONT,
     +          CLAYER,TMIP1,OK)
C              draw the line which has just been stored
               ENTYPE=LINE
C              draw the entity
               CALL ALLDRW(ENTYPE,TMIP1)
               IF (CONPAR) THEN 
                  TMIP = NMIPOS - 1
                  WRITE(UNIT=HUNIT,REC=ENTPNT,ERR=99) TMIP,
     +              ENTYPE,
     +              REAL(DX2),REAL(DY2),
     +              RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +              RDBUFF(4),RDBUFF(5),RDBUFF(6)
                  ENTPNT = ENTPNT +1
               END IF
 445        CONTINUE
 444     CONTINUE
CC        have finished the constuction so reset logical flag
C         PARLD = .FALSE.
Cc        clear cell
C         CALL GTMCLO(TMEN,TCELL)
      ELSE
C        It was not parallel at a distance so we will just create one line
C        store the newly constructed line.
         CALL DEWC03(X(1),Y(1),X(2),Y(2),CLFONT,
     +          CLAYER,TMIP1,OK)
      END IF
C
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(215)
         IF(CONPAR) CLOSE(UNIT = HUNIT)
         RETURN
      END IF
C     draw the line which has just been stored
      ENTYPE=LINE
 
C     draw the entity
      CALL ALLDRW(ENTYPE,TMIP1)
 
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C     Mutiple lines being inserted by parallel distance.
C      CONPAR = CONTIN.AND.PARLD
      IF(CONPAR) THEN               
         IF (ENTPNT.GT.LTOT+1) THEN
         NCNT = 1
         DO  544  I1 = 1,CPNT
c           also loop round the the number time for every separation 3@4 etc
            DO  545  I2 = 1,IARRAY(I1)
              READ(UNIT=HUNIT,REC=ENTPNT-NCNT,ERR=99) MIP2,ENTYPE,
     +           BX(2),BY(2),
     +           TBUFF(1),TBUFF(2),TBUFF(3),
     +           TBUFF(4),TBUFF(5),TBUFF(6)
              READ(UNIT=HUNIT,REC=ENTPNT-NCNT-LTOT,ERR=99) MIP1,ENTYPE,
     +           BX(1),BY(1),
     +           TBUFF(1),TBUFF(2),TBUFF(3),
     +           TBUFF(4),TBUFF(5),TBUFF(6)
C             erase the old arc
              CALL PENERS()
              CALL ALLDRW(ENTYPE,MIP2)
              CALL PENDRW()
              CALL DIR500(MIP2,OK)
C             Must modify the correct master index data so point to part data
              TPNT = IMBUFF(7)
              CALL DBR500(TPNT,OK)
              IF(OK) THEN
                CALL DCC0P5(DBLE(TBUFF(1)),DBLE(TBUFF(2)),
     +                      DBLE(TBUFF(4)),DBLE(TBUFF(5)),
     +                      DBLE(RDBUFF(1)),DBLE(RDBUFF(2)),
     +                      DBLE(RDBUFF(4)),DBLE(RDBUFF(5)),
     +                      DV1,DV2,OK1)
                 IF(OK1) THEN
                    RDBUFF(1) = REAL(DV1)
                    RDBUFF(2) = REAL(DV2)
                    CALL DBM500(TPNT,OK)
                    CALL ALLDRW(ENTYPE,MIP2)
                 END IF
              END IF
              WRITE(UNIT=HUNIT,REC=ENTPNT-NCNT,ERR=99) MIP2,ENTYPE,
     +           BX(2),BY(2),
     +           RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +           RDBUFF(4),RDBUFF(5),RDBUFF(6)
C             erase the old arc
              CALL PENERS()
              CALL ALLDRW(ENTYPE,MIP1)
              CALL PENDRW()
              CALL DIR500(MIP1,OK)
C             Must modify the correct master index data so point to part data
              TPNT = IMBUFF(7)
              CALL DBR500(TPNT,OK)
              IF(OK.AND.OK1) THEN
                 RDBUFF(4) = REAL(DV1)
                 RDBUFF(5) = REAL(DV2)
                 CALL DBM500(TPNT,OK)
                 CALL ALLDRW(ENTYPE,MIP1)
              END IF
             NCNT = NCNT +1
  545        CONTINUE
  544      CONTINUE
         END IF
         IF(LINFST) THEN
            PARSTX = REAL(DX1)
            PARSTY = REAL(DY1)
            LINFST = .FALSE.
         END IF
         IF (SAME(REAL(DX2),PARSTX)
     +             .AND.SAME(REAL(DY2),PARSTY)) THEN
C ==========================================================
C            Must be the start point coincident
C            so we will trim to start and end point
C ==========================================================
            NCNT = 1
            DO  644  I1 = 1,CPNT
c           also loop round the the number time for every separation 3@4 etc
            DO  645  I2 = 1,IARRAY(I1)
              READ(UNIT=HUNIT,REC=ENTPNT-NCNT,ERR=99) MIP2,ENTYPE,
     +           BX(2),BY(2),
     +           TBUFF(1),TBUFF(2),TBUFF(3),
     +           TBUFF(4),TBUFF(5),TBUFF(6)
              READ(UNIT=HUNIT,REC=LTOT-NCNT+1,ERR=99) MIP1,ENTYPE,
     +           BX(1),BY(1),
     +           TBUFF(1),TBUFF(2),TBUFF(3),
     +           TBUFF(4),TBUFF(5),TBUFF(6)
C             erase the old arc
              CALL PENERS()
              CALL ALLDRW(ENTYPE,MIP2)
              CALL PENDRW()
              CALL DIR500(MIP2,OK)
C             Must modify the correct master index data so point to part data
              TPNT = IMBUFF(7)
              CALL DBR500(TPNT,OK)
              IF(OK) THEN
                CALL DCC0P5(DBLE(TBUFF(1)),DBLE(TBUFF(2)),
     +                      DBLE(TBUFF(4)),DBLE(TBUFF(5)),
     +                      DBLE(RDBUFF(1)),DBLE(RDBUFF(2)),
     +                      DBLE(RDBUFF(4)),DBLE(RDBUFF(5)),
     +                      DV1,DV2,OK1)
                 IF(OK1) THEN
                    RDBUFF(4) = REAL(DV1)
                    RDBUFF(5) = REAL(DV2)
C                   Must modify the correct master index data so point to part data
                    TPNT = IMBUFF(7)
                    CALL DBM500(TPNT,OK)
                    CALL ALLDRW(ENTYPE,MIP2)
                 END IF
              END IF
              WRITE(UNIT=HUNIT,REC=ENTPNT-NCNT,ERR=99) MIP2,
     +           ENTYPE,
     +           BX(2),BY(2),
     +           RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +           RDBUFF(4),RDBUFF(5),RDBUFF(6)
C             erase the old arc
              CALL PENERS()
              CALL ALLDRW(ENTYPE,MIP1)
              CALL PENDRW()
              CALL DIR500(MIP1,OK)
C             Must modify the correct master index data so point to part data
              TPNT = IMBUFF(7)
              CALL DBR500(TPNT,OK)
              IF(OK.AND.OK1) THEN
                 RDBUFF(1) = REAL(DV1)
                 RDBUFF(2) = REAL(DV2)
C                Must modify the correct master index data so point to part data
                 TPNT = IMBUFF(7)
                 CALL DBM500(TPNT,OK)
                 CALL ALLDRW(ENTYPE,MIP1)
              END IF
              NCNT = NCNT + 1
  645        CONTINUE
  644      CONTINUE
           ENTPNT = 1
           NPOINT = 0
           LINFST = .TRUE.
           GOTO 10
         END IF
         X(1) = REAL(DX1)
         Y(1) = REAL(DY1)
         X(2) = REAL(DX2)
         Y(2) = REAL(DY2)
      END IF
C     draw out any flagged entities
      DO 8 I=1,2
          IF(REFMIP(I).GT.0) THEN
              CALL ALLDRW(REFENT(I),REFMIP(I) )
          ENDIF
 8    CONTINUE
      REFMIP(1) =0
      REFMIP(2) =0
      FIRST=.FALSE.
      ABSX=X(2)
      ABSY=Y(2)
C
      GOTO 5
 99   CONTINUE
      CALL ERRORDIALOG('Error in read/write on CONTINUOUS PARALLEL')
C
      END
C
C
      SUBROUTINE INSL09(ANGLAB,ANGLTO,LOCKV,LOCKH,PARLL,PARLD,PERP)
C     =======================================================
C
C1    vartype             L       L     L     L     L     L    L
C1    iostatus            IO      IO    IO   IO    IO    IO   IO
C
C2    sets all the logical variables passed to .FALSE.
C2    intended for use in  INSERT LINE mode to clear
C2    all line locks between events.
C
      LOGICAL ANGLTO,ANGLAB,LOCKV,LOCKH,PARLL,PARLD,PERP
C
      ANGLTO=.FALSE.
      ANGLAB=.FALSE.
      LOCKV=.FALSE.
      LOCKH=.FALSE.
      PARLL=.FALSE.
      PARLD=.FALSE.
      PERP=.FALSE.
C
      END
C
      SUBROUTINE MNILIN
C     =================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the INSERT LINE option list.
C2
C2    Tokens used here are X,H,V,P,R,A,F,G and L.
C2
      include   'include/gtxt2.inc'
C
      EXTERNAL GTPMEN,MNLPTS
C
C     CALL GTCLRM(3)
C
C     Define position for last line option in menu.
C      I=14
C
C2    Z is the token for BOX
      CALL GTDMEN(628,3)
C2    X is the token for BETWEEN POINTS
      CALL GTDMEN(91,3)
C2    x is the token for CONTINUOUS
      CALL GTDMEN(92,3)
C2    H is the token for HORIZONTAL.
      CALL GTDMEN(93,3)
C2    V is the token for VERTICAL.
      CALL GTDMEN(94,3)
C2    P is the token for PARALLEL.
      CALL GTDMEN(95,3)
C2    p is the token for PARALLEL DISTANCE.
      CALL GTDMEN(96,3)
C2    R is the token for PERPENDICULAR.
      CALL GTDMEN(97,3)
C2    i is the token for INTOF.
      CALL GTDMEN(98,3)
C2    c is the token for CANCEL.
      CALL GTDMEN(99,3)
C2    L is the token for LENGTH.
      CALL GTDMEN(100,3)
C2    B is the token for ANGLE ABS.
      CALL GTDMEN(101,3)
C2    A is the token for ANGLE REL.
      CALL GTDMEN(102,3)
C2    F is the token for TAN(1 ARC).
      CALL GTDMEN(103,3)
C2    G is the token for TAN(2 ARCS).
      CALL GTDMEN(104,3)
C2
C     Load the point modes.
      CALL MNLPTS()
C
      END
C
C
C
        SUBROUTINE SEPAR(TXT,IARRAY,RARRAY,CPNT,LTOT,DTOT,OK)
c       ================================================
c       iotype            c*(*) i4(30) r(30) I4   I4   R  L
c       iostat            I    O       O     O    O    O  O
C
C
        CHARACTER*80 TXT,TXT2
        INTEGER*4 FIELD(30),FN,I,NLEN1,PNT1,IARRAY(30),NLEN,ATVAL,
     +                     CPNT,LTOT
        REAL RARRAY(30),DTOT
        DOUBLE PRECISION DN
        LOGICAL OK
 
        CALL PARSES(TXT,FIELD,FN)
        TXT2 = '                                                       '
        PNT1 = 0
        CPNT = 0
        LTOT = 0
        DTOT = 0.0
        DO 10 I = 1,FN
           OK = .FALSE.
           IF(FN.EQ.I) THEN
              TXT2 = TXT(PNT1+1:)
              OK = .TRUE.
           ELSE
              IF((FIELD(I)-PNT1).GT.1) THEN
                 TXT2 = TXT(PNT1+1:FIELD(I)-1)
                 OK = .TRUE.
              END IF
           END IF
           PNT1 = FIELD(I)
           ATVAL = INDEX(TXT2,'@')
           IF(OK.AND.NLEN(TXT2).GT.0) THEN
              CPNT = CPNT +1
              IF(ATVAL.GT.1.AND.ATVAL.LT.NLEN(TXT2)) THEN
                 CALL AEXPRN(TXT2(1:ATVAL-1),DN,*20)
                 IARRAY(CPNT) = NINT(DN)
                 CALL AEXPRN(TXT2(ATVAL+1:),DN,*20)
                 RARRAY(CPNT) = REAL(DN)
              ELSE
                 IARRAY(CPNT) = 1
                 CALL AEXPRN(TXT2,DN,*20)
                 RARRAY(CPNT) = REAL(DN)
             END IF
             DTOT = DTOT + IARRAY(CPNT)*RARRAY(CPNT)
             LTOT = LTOT + IARRAY(CPNT)
           END IF
   10   CONTINUE
        RETURN
   20   CONTINUE
        OK = .FALSE.
        END
 
 
 
