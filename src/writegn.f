C
C        SCCS id Keywords             @(#)  412.1 date 6/11/92 writegn.f 
C
      SUBROUTINE WRTG01()
C     ====================
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/hdata.inc'
      include 'include/lfu.inc'
      include 'include/params.inc'
C
      DOUBLE PRECISION DN
      CHARACTER*80 OLIN,SEQ*20,UNF*20,DBU*2
      INTEGER*4 I,REC,TYPE,SKNO,INCK,NLEN,OUNIT,TMEN,TCELL
      INTEGER*2 FN
      REAL ORGX,ORGY
      LOGICAL OK,YES,OPTION,QUIT
C
      EXTERNAL OURSCR,DCPRNT,DEPRNT,PULENT,MEASA3,NLEN
C
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU.NE.'MM' .AND. DBU.NE.'IN' ) THEN
C       wrong units for GNC input
         I=448
         CALL DEPRNT(I)
C        abort process
         MEN=0
         RETURN
      END IF
      REC=52
      CALL OURSCR(HUNIT,REC,OK)
      IF ( .NOT. OK ) THEN
C        This would be a strange thing but it could happen
         I=143
         CALL DCPRNT(I)
         I=90
         CALL DEPRNT(I)
         GOTO 999
      END IF
C
 100  CONTINUE
      I=380
      CALL DPRMXP(I,OLIN)
      IF ( NLEN(OLIN).EQ.0 ) THEN
C      WRITE(10,*) '[WRTG01] SETTING M TO 0'
         MEN=0
         GOTO 997
      END IF
      CALL AEXPRN(OLIN,DN,*100)
      SKNO=MAX(1,INT(DN))
 
 101  CONTINUE
C   236    "Type in increment:"
      CALL DPRMXP(236,OLIN)
      IF ( NLEN(OLIN).EQ.0 ) THEN
         MEN=0
         GOTO 997
      END IF
      CALL AEXPRN(OLIN,DN,*101)
      INCK=MAX(1,INT(DN))
      CALL DPRMXP(81,OLIN)
C     if null file name assume abort required
      IF ( NLEN(OLIN).EQ.0 ) THEN
         MEN=0
         GOTO 997
      END IF
      INQUIRE(FILE=OLIN,EXIST=YES)
CAPOLLO
      INQUIRE(FILE=OLIN,SEQUENTIAL=SEQ,UNFORMATTED=UNF)
CAPOLLO
C
      IF ( YES ) THEN
C     WRITE(10,*) 'GNC',SEQ,UNF
CAPOLLO
C         IF ( SEQ.NE.'YES'.OR.UNF.NE.'NO' ) THEN
C            CALL DEPRNT(11)
C            GOTO 997
C         END IF
CAPOLLO
         CALL DELETE(OLIN,YES)
C     WRITE(10,*) 'GNC File deletion was ',YES
         IF ( .NOT. YES  ) THEN
            CALL DEPRNT(719)
            GOTO 997
         END IF
      END IF
      CALL FINDU2(OUNIT,OLIN,YES)
      IF ( .NOT. YES ) THEN
         RETURN
      END IF
      OPEN(UNIT=OUNIT,FILE=OLIN,STATUS='NEW',ERR=400)
CIBM
C      LFU(OUNIT)=.TRUE.
CIBM
699   CONTINUE
C     find origin for GNC data
      CALL MNLPTS()
C     prompt for origin point
      I=44
      CALL FINDP0(I,ORGX,ORGY,OPTION,QUIT)
      CALL WCROSS(ORGX,ORGY)
      CALL MNUPTS()
      IF (QUIT) GOTO 998
      IF (OPTION) GOTO 699
      WRITE(OUNIT,'(A)') '$GNX'
C     default the span curve type
      TYPE=1
C     Find menu cell which has been set for Profile
      CALL FNDPOS(301,I)
      CALL GTMCHI(3,I)
      TMEN=3
      TCELL=I
 10   CONTINUE
      IF ( TYPE.EQ.1 )  THEN
C        profile curve.
         FN=2
         CALL PULENT(FN)
         I=NO
      ELSE IF ( TYPE.EQ.2 ) THEN
C        points kcurve.
         I=0
         CALL MNLPTS()
         CALL WRTG03(HUNIT,I)
         CALL MNUPTS()
      ELSE IF ( TYPE.EQ.3 ) THEN
C        curve entity.
         I=0
         CALL WRTG04(HUNIT,I)
      END IF
C     switch off profile/points cell
      CALL GTMCLO(TMEN,TCELL)
      IF ( MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C        Get rid of temporary scratch files
         GOTO 998
      END IF
C
      IF ( CCMD.EQ.'P'.OR.CCMD.EQ.'p' ) THEN
         TMEN=MEN
         TCELL=CELLN
      END IF
C
      IF ( I.GT.0 ) THEN
         CALL WRTG02(HUNIT,OUNIT,TYPE,I,SKNO,INCK,ORGX,ORGY)
      END IF
C     make sure nothing left in buffer.
      CALL UNFLAG(.TRUE.)
C
      IF ( MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C        Get rid of temporary scratch files
         GOTO 998
      END IF
C
      IF ( CCMD.EQ.'P' ) THEN
C        profile option been chosen
C        get rid of what we have here before
C        going back.
         TYPE=1
      ELSE IF ( CCMD.EQ.'p' ) THEN
C        Points option been chosen
C        get rid of what we have here before
C        going to start again.
         TYPE=2
      ELSE IF (CCMD.EQ.'Y') THEN
C        User wants to use a curve
C        Start again with this option.
         TYPE = 3
      END IF
 
      IF ( CCMD.NE.CHAR(150) ) GOTO 10
C
      WRITE(OUNIT,'(A)') '$GDP'
      WRITE(OUNIT,'(A)') 'EOF'
C
      CLOSE(UNIT=OUNIT)
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
C
      CALL POPPD1(OLIN,673,120,350,667)
C      CALL POPPD1(OLIN,100,200,350,350)
C
C     show the user what he has created
C     ********************************************
C     return normally with files closed
C     ********************************************
      RETURN
C
 997  CONTINUE
C     ********************************************
C     return and close scratch file
C     ********************************************
      CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
      RETURN
C     ********************************************
C     return and close both scratch and work files
C     ********************************************
 998  CONTINUE
      CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
      CLOSE(UNIT=OUNIT)
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
      RETURN
C
C     ********************************************
C     return with no action
C     ********************************************
 999  CONTINUE
 400  CONTINUE
C     error in filename will not open
      CALL DEPRNT(276)
C
      END
C
C     ----------------------------------------------
C
      SUBROUTINE WRTG02(FUNIT,OUNIT,TYPE,N,SKNO,INCK,ORGX,ORGY)
C     ==========================================================
C        vartyp           I4   I4,I4
C        iostat            I    I  I
C
      include 'include/menun.inc'
      include 'include/wtov.inc'
      include 'include/entity.inc'
      include 'include/params.inc'
C
      CHARACTER*20 FORMAT,OLIN*80,DBU*2
      LOGICAL SAMEPT,YES,OP,HOLD,QUIT,OPTION,FULCIR,SANGL,SAME
      INTEGER*2 MIP1,ENT,ENTH,MIP2,ENT1,TERMP
      INTEGER*4 N,I,NP,FUNIT,OUNIT,ON,NLEN,TYPE,SKNO,INCK
      INTEGER*4 TCELL,TMEN
      DOUBLE PRECISION DX1,DY1
      REAL ORGX,ORGY
      REAL X1,X2,Y1,Y2,Z1,BX,BY,OX,OY,XH1,YH1,XH2,YH2,ZH1,XP1,YP1,
     +     T1,T2,T3,CD0D13,DISTXY,PI
      REAL J1,J2,J3,J4,J5,TX1,TY1,TX2,TY2,TZ1,XP,YP,XP2,YP2,CANG,
     +     TXP,TYP,TXP2,TYP2,TOY,TOX,XP11,YP11
      EXTERNAL SAMEPT,DPI,NLEN,CD0D13,DISTXY,PI,CANG,SAME
C
C
      IF ( CCMD.NE.'P'.OR.CCMD.NE.'p' ) THEN
         CALL GTMCLO(MEN,CELLN)
      END IF
C     use different format for database units.
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU .EQ. 'MM' ) THEN
         FORMAT='(A,2F12.4,F10.5)'
      ELSE  IF ( DBU .EQ. 'IN' ) THEN
         FORMAT='(A,2F12.5,F10.6)'
      END IF
 100  CONTINUE
C
C     Write out Kcurve no. and
      WRITE(OLIN,'(2(A,I6))') 'KCU/',SKNO,',',TYPE
      SKNO=SKNO+INCK
      CALL CRUNCH(OLIN)
      WRITE(OUNIT,'(A)') OLIN
C
      IF ( TYPE.EQ.1 ) THEN
         READ(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY, X1, Y1, X2, Y2, Z1,
     +                                          XH1,YH1,XH2,YH2,ZH1
         IF (.NOT.(ENT.EQ.ARC.AND.N.EQ.1) ) THEN
C           **************************************************
C           ******   profile needs start and end point   *****
C           **************************************************
            TERMP = 0
            CALL MNLPTS()
            CALL  FINDP0(381,XP1,YP1,OPTION,QUIT)
            CALL MNUPTS()
C           Termp stores the number of termination points
            IF(OPTION.AND.CCMD.EQ.CHAR(150)) THEN
                TMEN=MEN
                TCELL=CELLN
C               ensure the entry cell is not hilited any more
                CALL GTMCLO(TMEN,TCELL)
                TERMP = 2
            ELSE  IF ( QUIT.OR.OPTION ) THEN
                RETURN
            ELSE
                CALL WCROSS(XP1,YP1)
C               profile needs end point.
C               go find an point to put it through
                CALL MNLPTS()
                CALL  FINDP0(382,XP11,YP11,OPTION,QUIT)
                CALL MNUPTS()
                IF(OPTION.AND.CCMD.EQ.CHAR(150)) THEN
                   TMEN=MEN
                   TCELL=CELLN
C                  ensure the entry cell is not hilited any more
                   CALL GTMCLO(TMEN,TCELL)
                    TERMP = 1
                ELSE  IF ( QUIT.OR.OPTION ) THEN
                   RETURN
                ELSE
                   CALL WCROSS(XP11,YP11)
                END IF
            END IF
C
C           **************************************************
C           ******  START POINT is handled here          *****
C           **************************************************
C           This code has a look ahead to the next entity to
C           calculate which end of the first entity is used.
C           Then the first entity is modified

            IF ( ENT .EQ. ARC) THEN
c              ====================================
C              ======       was it an arc   =======
c              ====================================
C              
c
C              arc found so calculate the nearest point on arc to hit point
C              NOTE :- only do this calculation if hit point was supplied
C                   i.e. TERMP LT 2
               IF(TERMP.LT.2) THEN
                  CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +            DBLE(XP1),DBLE(YP1),DX1,DY1)
                  OX=REAL(DX1)
                  OY=REAL(DY1)
               END IF
c              let us read the next entity
               READ(UNIT=FUNIT,REC=2) MIP2,ENT1,BX,BY,TX1,TY1,
     +                             TX2,TY2,TZ1,J1,J2,J3,J4,J5
c              let us calculate the two points on the next entity
C              from the start angle end angle
               IF(ENT1.EQ.ARC) THEN
C                 if arc the two points for the next entity is found
C                 from the start angle end angle by newpt1
                  CALL NEWPT1(TX1,TY1,TX2,TY2,TXP,TYP)
                  CALL NEWPT1(TX1,TY1,TX2,TZ1,TXP2,TYP2)
               ELSE 
C                 if next entity is line then it is easy to find the two points
C                 no  calulation necessary
                  TXP = TX1
                  TYP = TY1
                  TXP2 = TX2
                  TYP2 = TY2
               END IF
c              now let us calculate the the two points on the start entity
C              from the start angle end angle by newpt1
               CALL NEWPT1(X1,Y1,X2,Y2,XP,YP)
               CALL NEWPT1(X1,Y1,X2,Z1,XP2,YP2)
c
C              the logical SANGL is used to find if the start angle is the
C              shared with either of the end points of the next entity
               SANGL=(SAMEPT(XP,YP,TXP,TYP).OR.SAMEPT(XP,YP,TXP2,TYP2))
c
c              Now a decision if the start point was specified
               IF(TERMP.LT.2) THEN
C                 start point was specified so modifiy start or end angle
C                 with newly calculated start point on first arc
                  IF(SANGL) THEN
                     Z1 = CANG(X1,Y1,OX,OY)
                  ELSE 
                     Y2 = CANG(X1,Y1,OX,OY)
                  END IF                   
c                 now modify the boundry definition
                  WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,
     +            X1,Y1, X2, Y2, Z1,XH1,YH1,XH2,YH2,ZH1
               ELSE 
C                 No start point was specified so we only have to
C                 set the first point which is one of the two existing
C                 points
                  IF(SANGL) THEN
                    OX = XP2
                    OY = YP2
                  ELSE
                    OX = XP
                    OY = YP
                  END IF
               END IF
            ELSE     
c              ====================================
C              ======       was it an line  =======
c              ====================================
c
C              line found so calculate the nearest point on line to hit point
C              NOTE :- only do this calculation if hit point was supplied
C                   i.e. TERMP LT 2
               IF(TERMP.LT.2) THEN
                  CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +            DBLE(XP1),DBLE(YP1),DX1,DY1)
                  OX=REAL(DX1)
                  OY=REAL(DY1)
                  WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,
     +            OX, OY, X2, Y2, Z1,XH1,YH1,XH2,YH2,ZH1
               ELSE
C                 
C                 No start point was specified so let us see which
C                 ent to modify
                  READ(UNIT=FUNIT,REC=2) MIP2,ENT1,BX,BY,TX1,TY1,
     +                             TX2,TY2,TZ1,J1,J2,J3,J4,J5
                  IF(ENT1.EQ.ARC) THEN
c                    let us calculate the two points on the next entity
C                    from the start angle end angle
                     CALL NEWPT1(TX1,TY1,TX2,TY2,TXP,TYP)
                     CALL NEWPT1(TX1,TY1,TX2,TZ1,TXP2,TYP2)
                  ELSE 
C                    if next entity is line then it is easy to find the two points
                     TXP = TX1
                     TYP = TY1
                     TXP2 = TX2
                     TYP2 = TY2
                  END IF
c                 now let us calculate the the two points on the start entity
                  CALL NEWPT1(X1,Y1,X2,Y2,XP,YP)
                  CALL NEWPT1(X1,Y1,X2,Z1,XP2,YP2)
C                 the logical SANGL is used to find if the start point is the
C                 shared with either of the end points of the next entity
                  SANGL=(SAMEPT(XP,YP,TXP,TYP).OR.
     +                   SAMEPT(XP,YP,TXP2,TYP2))
C                 All we only have to is set the first point which
C                 is one of the two existing points
                  IF(SANGL) THEN
                    OX = XP2
                    OY = YP2
                  ELSE
                    OX = XP
                    OY = YP
                  END IF
               END IF
            END IF
         ELSE
C           Fool the scratch file into having two entries for a complete circle
            IF(SAME(Y2,Z1)) THEN
               Z1 = Z1 + PI(2.0)
            END IF
            IF(SAME((Z1-Y2),PI(2.0))) THEN
               N = N + 1
               WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY, X1, Y1, X2,
     +                            (Z1-Y2)/2.0,Z1,XH1,YH1,XH2,YH2,ZH1
               WRITE(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY, X1, Y1, X2, Y2,
     +                            (Z1-Y2)/2.0,XH1,YH1,XH2,YH2,ZH1
            END IF
            OX = X1+X2*COS(Y2)
            OY = Y1+X2*SIN(Y2)
            TERMP = 2
         END IF
C
      END IF
*
c
c
c
C     *********************************************************
C     ******  LOOP ROUND ALL THE ENTITIES ON THE PROFILE  *****
C     *********************************************************
      DO 20 I=1,N
         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
C        WRITE(UNIT=10,FMT=*) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
         IF(N.EQ.I.AND.ENT.EQ.ARC) THEN
C           **************************************************
C           ******  FINISH CONDITION is handled here     *****
C           **************************************************
C           for a finish we only need to modify the arc at this stage
C           so that we can calculate the correct bulge factor for the current 
C           point.  
c           NOTE this is only done if a finish point has been specified
C                   i.e. TERMP EQ 0
C
            IF (N.GT.1.AND.TERMP.EQ.0) THEN
C              Let us calculate the nearest point on arc to hit point
               CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +         DBLE(XP11),DBLE(YP11),DX1,DY1)
               TOX=REAL(DX1)
               TOY=REAL(DY1)
c              let us calculate the two points on the current entity
C              from the start angle end angle
               CALL NEWPT1(X1,Y1,X2,Y2,XP,YP)
               CALL NEWPT1(X1,Y1,X2,Z1,XP2,YP2)
c
C              the logical SANGL is used to find if the previous  point is
C              shared with either of the end points of the current entity
               SANGL = (SAMEPT(XP,YP,OX,OY))
               IF(SANGL) THEN
                  Z1 = CANG(X1,Y1,TOX,TOY)
               ELSE 
                  Y2 = CANG(X1,Y1,TOX,TOY)
               END IF 
            END IF 
         END IF
           
         FULCIR=.FALSE.
         IF ( ENT.EQ. ARC ) THEN
C           Is the single boundary a full circle
            IF ( Y2.LT.0.01.AND.
     +           Z1.GE.PI(2.0)-0.01.AND.N.EQ.1 ) THEN
C                Yes it is so we will allow that
               ZH1=Z1
               Z1=Z1/2.0
               FULCIR=.TRUE.
            END IF
 33         CONTINUE
            XH1=X1+X2*COS(Z1)
            YH1=Y1+X2*SIN(Z1)
            XH2=X1+X2*COS(Y2)
            YH2=Y1+X2*SIN(Y2)
C           calculate mid point of arc
            T3=Z1-Y2
C      WRITE(10,*) 'ANGLES',T3,Y2,Z1
            IF ( T3 .LT. 0.0 ) THEN
               T3=((PI(2.0)+T3)/2.0)+Y2
            ELSE
               T3=(Z1+Y2)/2.0
            END IF
C      WRITE(10,*) 'ANGLES',T3,Y2,Z1
            XP1=X1+X2*COS(T3)
            YP1=Y1+X2*SIN(T3)
C         WRITE(10,*) 'old x,y',OX,OY
C         WRITE(10,*) 'End angle',XH1,YH1,
C         WRITE(10,*) 'Start angle',XH2,YH2
C         WRITE(10,*) 'Mid-point',XP1,YP1
 
            IF ( SAMEPT(OX,OY,XH1,YH1) ) THEN
               T3=CD0D13(XH1,YH1,XH2,YH2,XP1,YP1)
               OX=XH2
               OY=YH2
               T1=XH1
               T2=YH1
C               WRITE(10,*) 'End point used'
            ELSE IF ( SAMEPT(OX,OY,XH2,YH2)) THEN
               T3=CD0D13(XH2,YH2,XH1,YH1,XP1,YP1)
               OX=XH1
               OY=YH1
               T1=XH2
               T2=YH2
C               WRITE(10,*) 'Start point used'
            ELSE
C               WRITE(10,*) '************* ERRoR ************'
            END IF
C      WRITE(10,*) 'Perp dist',T3,DISTXY(XH1,YH1,XH2,YH2)
               T3=SIGN(1.0,T3)*
     +         (ABS(T3)/(DISTXY(XH1,YH1,XH2,YH2)/2.0))
            WRITE(OUNIT,FORMAT) 'SPA',T1-ORGX,T2-ORGY,T3
            IF ( FULCIR ) THEN
               FULCIR=.FALSE.
               Y2=Z1
               Z1=ZH1
               GOTO 33
            END IF
C
         ELSE IF ( ENT .EQ. LINE ) THEN
            WRITE(OUNIT,FORMAT) 'SPA',X1-ORGX,Y1-ORGY,0.0
            OX=X2
            OY=Y2
         ELSE IF ( ENT .EQ. 2 ) THEN
            WRITE(OUNIT,FORMAT) 'SPA',X1-ORGX,Y1-ORGY,0.0
            OX=X1
            OY=Y1
         END IF
 
 20   CONTINUE
C
C     write out the last span of the profile.
C      WRITE(OUNIT,FORMAT) 'SPA',OX,OY,0.0
C
      IF ( TYPE.EQ.1 ) THEN
C       **************************************************
C       **********   FINISH POINT handled here    ********
C       **************************************************
        IF (.NOT.(ENT.EQ.ARC.AND.N.EQ.1) ) THEN
            READ(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                                     XH1,YH1,XH2,YH2,ZH1
c        calcuate a point on the arc or line closest to the
C        hit point.  This is used as the termination point
C        NOTE :- for arcs this was already done when calculating the
C                bulge factor
         IF ( ENT .EQ. ARC.AND.TERMP.EQ.0) THEN
C           arc found
            CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +      DBLE(XP11),DBLE(YP11),DX1,DY1)
            OX=REAL(DX1)
            OY=REAL(DY1)
         ELSE  IF (TERMP.EQ.0) THEN
            CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +      DBLE(XP11),DBLE(YP11),DX1,DY1)
            OX=REAL(DX1)
            OY=REAL(DY1)
         END IF
C
         END IF
         WRITE(OUNIT,FORMAT) 'SPA',OX-ORGX,OY-ORGY,0.0
      END IF
      CCMD=CHAR(32)
*
C
      END
C
C     ----------------------------------------------------
C                                a
      SUBROUTINE WRTG03(FUNIT,NP)
C     ===========================
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
C
      INTEGER*4 NP,FUNIT,II
      INTEGER*2 P,MD,ED,I,J
      LOGICAL OPTION,QUIT,OK
      REAL FP(3),HPX,HPY,RD1,RD2
C
      RD1=0.0
      RD2=0.0
      ED=2
      MD=0
C
 5    CONTINUE
C
C     go find an point to put it through
      CALL  FINDP0(164,HPX,HPY,OPTION,QUIT)
C
      IF ( QUIT.OR.(OPTION.AND.MEN.EQ.2) ) RETURN
C
      IF ( OPTION ) THEN
C
         IF ( CCMD .EQ. CHAR(150) ) THEN
            IF ( NP .GT. 1 ) THEN
               CCMD='p'
               RETURN
            ELSE IF ( NP .EQ. 1 ) THEN
               CALL DEPRNT(155)
               CALL GTMCLO(MEN,CELLN)
            ELSE
               RETURN
            END IF
         ELSE IF ( CCMD .EQ. CHAR(149) ) THEN
            IF ( NP .GT. 0 ) THEN
               READ(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,
     +         FP(1),FP(2),FP(3),RD1,RD2
               CALL WO2SC(FP(1),FP(2),HPX,HPY)
               CALL BCROSS(HPX,HPY)
               NP=NP-1
               CALL GTMCLO(MEN,CELLN)
            ELSE
               CALL DEPRNT(724)
            END IF
         ELSE IF ( CCMD .EQ. 'W' ) THEN
C           Window
            CALL DCPRNT(88)
            CALL WINDOW(.FALSE.)
            DO 111 II=1,NDATA
               CALL RSCRF(II,MD,RD1,RD2,I,J)
               CALL DER500(MD,OK)
               IF ( .NOT. OK ) THEN
                  CALL UNFLAG(.TRUE.)
                  RETURN
               END IF
               NP=NP+1
               WRITE(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,
     +           RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1           RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),RDBUFF(6)
 111        CONTINUE
            CALL GTMCLO(MEN,CELLN)
         ELSE IF ( CCMD.EQ.'B' ) THEN
C           Next Bounary option
C           going back.
            RETURN
         ELSE IF ( CCMD.EQ.'P' ) THEN
C           profile option been chosen
C           get rid of what we have here before
C           going back.
            RETURN
         ELSE IF ( CCMD.EQ.'p' ) THEN
C           Points option been chosen
C           get rid of what we have here before
C           going to start again.
            RETURN
         ELSE IF ( CCMD.EQ.'C' ) THEN
C           Curve Entity option been chosen
C           going to start again.
            RETURN
         ELSE
            CALL DEPRNT(8)
            CALL GTMCLO(MEN,CELLN)
         END IF
 
         GOTO 5
C
      END IF
C
      NP=NP+1
      FP(1)=HPX
      FP(2)=HPY
      FP(3)=0.0
C         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
      WRITE(UNIT=FUNIT,REC=NP) MD,ED,RD1,RD2,FP(1),FP(2),FP(3),0.0,0.0
      CALL WO2SC(FP(1),FP(2),HPX,HPY)
      CALL BCROSS(HPX,HPY)
C
      GOTO 5
C
      END
C
C     ---------------------------------------------------
C

      SUBROUTINE WRTG04(SCRFIL,NP)
C     ============================
C1    VARTYPE             I4   I4
C1    IOSTATUS            I    O
C2
C2    This routine prompts for a curve then interpolates it for
C2    k_curve output, into the scratch file SCRFIL.
C2
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/drwspl.inc'
C
      DOUBLE PRECISION DN
      INTEGER*4 FUNIT,II,SCRFIL,NP,NLEN
      INTEGER*2 P,I,J,ENT
      LOGICAL OPTION,QUIT,OK
      REAL HPX,HPY,M(3,3)
      CHARACTER NUMSTR*5,INSTR*80      
C
      EXTERNAL NLEN
C
C     Put up the resolution menu cell.
      CALL GTDMEN(352,3)
      WRITE(NUMSTR,'(F4.1)') RESLTN
      CALL GTDMWT(352,3,NUMSTR)
C
 5    CONTINUE
C
C     Get a point close to the required curve.
      CALL  FINDP0(444,HPX,HPY,OPTION,QUIT)
C
C     Does the user want to change anything?
      IF ( QUIT.OR.(OPTION.AND.MEN.EQ.2) ) RETURN
      IF ( OPTION ) THEN
        IF ( CCMD .EQ. CHAR(150) ) THEN
           IF ( NP .GT. 1 ) THEN
              CCMD='C'
              RETURN
           ELSE IF ( NP .EQ. 1 ) THEN
              CALL DEPRNT(155)
              CALL GTMCLO(MEN,CELLN)
           ELSE
              RETURN
           END IF
        ELSE IF (CCMD.EQ.'B'.OR.CCMD.EQ.'P'.OR.CCMD.EQ.'p'
     +                            .OR.CCMD.EQ.'C') THEN
C           B = Next Bounary option,
C           P = profile option, 
C           p = points option.
C           Thay all = go back.
            RETURN
         ELSE IF (CCMD.EQ.'r') THEN
C           Change the resolution value.
            II = 431
 50         CONTINUE
            CALL DPRMXP(II,INSTR)
            IF ( NLEN(INSTR).NE.0 ) THEN
               CALL AEXPRN(INSTR,DN,*100)
               IF (REAL(DN).LT.1.0) THEN 
                  CALL DEPRNT(209)
                  GOTO 50
               ELSE
                  RESLTN = REAL(DN)
C                 Update the resolution menu cell.
                  CALL GTDMEN(352,3)
                  WRITE(NUMSTR,'(F4.1)') RESLTN
                  CALL GTDMWT(352,3,NUMSTR)
               ENDIF
            ENDIF
 100        CONTINUE
         ELSE
C           Nothing else makes any sense here.
            CALL DEPRNT(8)
            CALL GTMCLO(MEN,CELLN)
         END IF
         GOTO 5
      END IF
C                                           
C     Can we find a curve near the hit point.
      CALL NOSRCH()
      ENT = SPLINE
      CALL ADSRCH(ENT)
      CALL DSE800(HPX,HPY,OK)
      IF (.NOT.OK) THEN
         CALL DEPRNT(142)
         GOTO 5
      ENDIF
C
      CALL DCPRNT(379)
C     Got the curve. Now interpolate it.
      NPSPL = 0
      CALL I3M(M)
      CALL DRWSPL(MIP,.FALSE.,M,SCRFIL)
      NP = NPSPL
C
C     Remove the resolution menu cell.
      CALL GTMCLO(3,8)
      CALL GTCLRC(3,8)
C
      END
C
C     ---------------------------------------------------
C
