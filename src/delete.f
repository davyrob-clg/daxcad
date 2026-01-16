C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 delete.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DELABP(HPX1,HPY1,HPX2,HPY2,REFX,REFY)
C     SUBROUTINE DELL03(BETWP)
C     SUBROUTINE DELLBP(HPX1,HPY1,HPX2,HPY2)
C     SUBROUTINE DLRCVR()
C     SUBROUTINE INTER0(X,Y,FORWRD,RESX,RESY,END)
C     SUBROUTINE MAJDL1()
C     SUBROUTINE MASDEL()
C     SUBROUTINE MNIDEL
C     SUBROUTINE MNIDL0()
C     SUBROUTINE MNLD11()
C     SUBROUTINE MNUDL0()
C     SUBROUTINE SETSER(ENT,ALL,PTXT)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DELABP(HPX1,HPY1,HPX2,HPY2,REFX,REFY)
C     ================================================
C1                        R,   R,   R,   R    R    R
C1                        I,   I,   I,   I    I    I
C
C2        This routine erases the arc in the NBUFF buffers, between the
C2    points  (HPX1,HPY1) ,  (HPX2,HPY2).  Unlike a line, however.  The
C2    word BETWEEN is ambiguous.  Annother point is required to specify
C2    which of the two possible arcs (major or minor) is to be removed.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/viewport.inc'
      include 'include/entity.inc'
      include 'include/menun.inc'
      include 'include/delstore.inc'
C
      INTEGER*4 COL
      INTEGER*2 ENT,P,I
      REAL HPX1,HPY1,HPX2,HPY2,REFX,REFY,SANG,EANG,OLDSTA,OLDEND,
     +     FULCRC,PI,CENX,CENY,RAD
      REAL XS,YS
      REAL XF,YF    
      REAL DIST1,DIST2
      REAL ANG1,ANG2
      REAL CANG
      LOGICAL OK,SAME,OPTION,QUIT,FULCIR
      LOGICAL SAMEA
      LOGICAL ARCTPT

      EXTERNAL PI,SAME
      EXTERNAL SAMEA
      EXTERNAL CANG
      EXTERNAL ARCTPT
C
      FULCRC = PI(2.0)
      ENT = IMBUFF(2)
C
C     Store the original data ... just in case the twit wants to cancel.
      DNMIPO = NMIPOS
      DNPDPO = NPDPOS
      OLDMIP = MIP
      NEWMIP = 0
      COL  = IMBUFF(3)
      DO 5,I=1,4
         DIDBUF(I)=IDBUFF(I)
 5    CONTINUE
      DO 6,I=1,6
         DRDBUF(I)=RDBUFF(I)
 6    CONTINUE
      DO 7,I=1,13
         DIMBUF(I)=IMBUFF(I)
 7    CONTINUE
C        
      OLDSTA = RDBUFF(5)
      OLDEND = RDBUFF(6)
C
C     get start and end of existing arc.
      CALL ARCSE(DRDBUF,XS,YS,XF,YF,FULCIR)
C
      IF(FULCIR) THEN
C         Turn the hit points into angles if in full circle.
          CALL SRT3PT(HPX1,HPY1,
     +                HPX2,HPY2,
     +                REFX,REFY,
     +                RDBUFF(1),RDBUFF(2),
     +                SANG,EANG)

      ELSE
C         Arc portion use the two hit points only
          IF ( .NOT.(
     +        ARCTPT(RDBUFF(1),RDBUFF(2),OLDSTA,OLDEND,HPX1,HPY1)
     +        .AND.
     +        ARCTPT(RDBUFF(1),RDBUFF(2),OLDSTA,OLDEND,HPX2,HPY2)
     +        ))THEN

              CALL EPRINT('Error: Both points must lie on the arc')
              GOTO 999
          ENDIF
C         Both lie in arc.
          ANG1 = CANG(RDBUFF(1),RDBUFF(2),HPX1,HPY1)
          ANG2 = CANG(RDBUFF(1),RDBUFF(2),HPX2,HPY2)
C
          CALL CANG2A(OLDSTA,ANG1,DIST1)
          CALL CANG2A(OLDSTA,ANG2,DIST2)
C
          IF ( DIST1.GT.DIST2) THEN
C
              SANG = ANG2
              EANG = ANG1
C
          ELSE
C
              SANG = ANG1
              EANG = ANG2
C
          ENDIF
C


C     modify an end point of an arc to be 2*PI to keep consstancy
      IF (SAMEA(0.0,EANG)) THEN
          EANG = FULCRC
      ENDIF

C
      ENDIF
C
      IF (SAMEA(SANG,EANG)) THEN
C        same point cannot really do this.
         CALL DEPRNT(94)
         GOTO 999
      ENDIF
C
C     Erase the old arc.
      VPMOV = .TRUE.
      CALL PENERS()
      CALL ALLDRW(ENT,MIP)
      CALL PENDRW()
      VPMOV = .FALSE.
C
      IF (FULCIR) THEN
C         Was a full circle. No need to split, just swap new end points
C         and use them.
          RDBUFF(5) = EANG
          RDBUFF(6) = SANG
C         Modify the database.
          CALL DEM500(MIP,OK)
C         now draw the new arc with correct colour
          VPMOV = .TRUE.
          CALL ALLDRW(ENT,MIP)
          VPMOV = .FALSE.     
      ELSE
C
C         Arc does not pass go, but erase region does.
          IF ( SAMEA(OLDEND,EANG).AND.SAMEA(OLDSTA,SANG)) THEN
C             Wants the complete arc deleted both end points same
              VPMOV = .TRUE.
              CALL PENERS()
              CALL ALLDRW(ENT,MIP)
              VPMOV = .FALSE.
C             set delete status 
              IMBUFF(1)=100
C             write back to storage
              CALL DIM500(MIP,OK)
          ELSE IF (SAMEA(OLDSTA,SANG)) THEN
C             Chop end  angle back to hit point
              RDBUFF(5) = EANG
C             Modify the database.
              CALL DEM500(MIP,OK)
C             Draw the modified arc.
              VPMOV = .TRUE.
              CALL ALLDRW(ENT,MIP)
              VPMOV = .FALSE.
          ELSE IF (SAMEA(OLDEND,EANG)) THEN
C             Chop start angle to hit point
              RDBUFF(6) = SANG
C             Modify the database.
              CALL DEM500(MIP,OK)
C             Draw the modified arc.
              VPMOV = .TRUE.
              CALL ALLDRW(ENT,MIP)
              VPMOV = .FALSE.
          ELSE
C             Take out the middle of the arc. Splitting it in two.
              RDBUFF(6) = SANG
C             Modify the database.
              CALL DEM500(MIP,OK)
C             now draw the modified old arc.
              VPMOV = .TRUE.
              CALL ALLDRW(ENT,MIP)
              VPMOV = .FALSE.     
C             store the new arc
              CENX = RDBUFF(1)
              CENY = RDBUFF(2)
              RAD  = RDBUFF(4)
              CALL DEWC05(CENX,CENY,RAD,EANG,OLDEND,CLFONT,CLAYER,
     +                        P,OK)
              IF(.NOT.OK) THEN
C                 cannot add to database
                  CALL ALLDRW(ENT,P)
                  GOTO 999
              ENDIF
C             make sure new color is inherited.
              IMBUFF(3) = COL
C             now draw the new arc.
              CALL DEM500(P,OK)
              NEWMIP = P
              VPADD = .TRUE.
              CALL ALLDRW(ENT,P)
              VPADD = .FALSE.
          ENDIF
      ENDIF
C
 999  CONTINUE
C       
      END
C
      SUBROUTINE DELL03(BETWP)
C     ========================
C       VARTYPE           L
C       IOSTATUS          I
C
      include 'include/ndata.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/delstore.inc'
      include 'include/viewport.inc'
C
      REAL WX1,WY1,WX2,WY2
C
      INTEGER*4 TMEN,TCELL,C,I
      INTEGER*2 ENT
C
      LOGICAL OK,BETWP
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL WINDOW,MASDEL,ZSFLAG,GTMCLO
C
C
C     start option processinig here
 20   CONTINUE
C     process the independants first
C
C     process the normal options now
      TMEN=MEN
      TCELL=CELLN
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        use window for selection of entities
         CALL DCPRNT(88)
         CALL WINDOW(.TRUE.)
      ELSE IF (CCMD.EQ.CHAR(150)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
            CALL DCPRNT(89)
C           go do the delete
            CALL MASDEL()
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
C
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C                OOPS OPTION                                   *
C***************************************************************
C     if backspace char,redelete last entity from delete list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.FALSE.,OK)
         IF ((.NOT.OK) .AND. BETWP .AND. (DNMIPO.NE.0)) THEN
C           Between points - cancel ... restore the original data.
C           Erase from screen.
            ENT = IMBUFF(2)
            CALL PENERS()
            VPMOV = .TRUE.
            CALL ALLDRW(ENT,OLDMIP)
            VPMOV = .FALSE.
            IF (NEWMIP.GT.0) THEN
               VPDEL = .TRUE.
               CALL ALLDRW(ENT,NEWMIP)
               VPDEL = .FALSE.
            ENDIF
            CALL PENDRW()
C
            DO 5,I=1,4
               IDBUFF(I)=DIDBUF(I)
 5          CONTINUE
            DO 6,I=1,6
               RDBUFF(I)=DRDBUF(I)
 6          CONTINUE
            DO 7,I=1,13
               IMBUFF(I)=DIMBUF(I)
 7          CONTINUE
C
C           Modify the database.
            CALL DEM500(OLDMIP,OK)
C           Draw the modified arc.
            VPMOV = .TRUE.
            CALL ALLDRW(ENT,OLDMIP)
            VPMOV = .FALSE.
C
            NMIPOS = DNMIPO
            NPDPOS = DNPDPO
         ELSE IF (.NOT.OK) THEN
            CALL DEPRNT(33)
         ENDIF
C        Block cancel between points until next delete is complete.
         DNMIPO = 0
      ELSE
         CALL DEPRNT(8)
      END IF
C
C        ensure menu cell is switched off
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE DELLBP(HPX1,HPY1,HPX2,HPY2)
C     ======================================
C1                         R,   R,   R,   R
C1                         I,   I,   I,   I
C2
C2    Subroutine DELLBP will split a single line into
C2    Two seperate lines if need be or just modifiy the existing
C2    line if one of the points used coincides with one of the end
C2    points of the line being altered.
C
 
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/viewport.inc'
      include 'include/entity.inc'
      include 'include/delstore.inc'
 
      LOGICAL OPTION,QUIT,SAME,OK,SWAP
      INTEGER*2 MP,TMIP,ENT,I
      INTEGER*4 P1END,P2END,I4
      REAL HPX1,HPY1,HPX2,HPY2,DISTXY,DIST1,DIST2
      DOUBLE PRECISION L1,L2,L3,X1,Y1
      EXTERNAL SAME,DISTXY,DRWFLW,ERSFLW
C
C
C     Store the original data ... just in case the twit wants to cancel.
      DNMIPO = NMIPOS
      DNPDPO = NPDPOS
      OLDMIP = MIP
      NEWMIP = 0
      DO 5,I=1,4
         DIDBUF(I)=IDBUFF(I)
 5    CONTINUE
      DO 6,I=1,6
         DRDBUF(I)=RDBUFF(I)
 6    CONTINUE
      DO 7,I=1,13
         DIMBUF(I)=IMBUFF(I)
 7    CONTINUE
C
      IF ( DISTXY(HPX1,HPY1,RDBUFF(1),RDBUFF(2)).GT.
     +     DISTXY(HPX2,HPY2,RDBUFF(1),RDBUFF(2))     ) THEN
            CALL RSWAP(HPX1,HPX2)
            CALL RSWAP(HPY1,HPY2)
      END IF
C     Create a line vector through the line since
C     we may use it twice
      CALL DCVL14(DBLE(RDBUFF(1)),DBLE(RDBUFF(2)),
     +            DBLE(RDBUFF(4)),DBLE(RDBUFF(5)),L1,L2,L3)
C     check if hit point one is on an end point
      IF ( SAME(HPX1,RDBUFF(1)).AND.SAME(HPY1,RDBUFF(2)) ) THEN
         P1END=1
      ELSE IF (SAME(HPX1,RDBUFF(4)).AND.SAME(HPY1,RDBUFF(5))) THEN
         P1END=-1
      ELSE
         P1END=0
      END IF
C     If not an end point snap onto the line
      IF ( P1END.EQ.0 ) THEN
         CALL DVCPLP(L1,L2,L3,DBLE(HPX1),DBLE(HPY1),X1,Y1)
C        reset the hit points
         HPX1=REAL(X1)
         HPY1=REAL(Y1)
      END IF
C     check if hit point two is on an end point
      IF ( SAME(HPX2,RDBUFF(1)) .AND.SAME(HPY2,RDBUFF(2)) ) THEN
         P2END=1
      ELSE IF ( SAME(HPX2,RDBUFF(4)).AND.SAME(HPY2,RDBUFF(5)) ) THEN
         P2END=-1
      ELSE
         P2END=0
      END IF
C     If not an end point snap onto the line
      IF ( P2END.EQ.0 ) THEN
         CALL DVCPLP(L1,L2,L3,DBLE(HPX2),DBLE(HPY2),X1,Y1)
         HPX2=REAL(X1)
         HPY2=REAL(Y1)
      END IF
      IF ( P1END.NE.0.AND.P2END.NE.0 ) THEN
C        Both points snap to end points.
         IF (P1END.NE.P2END) THEN
C           The whole line has been indicated. This is a bit of a 
C           roundabout way to go about deleting a line. Still, better
C           do it anyway.
            ENT = LINE
            VPDEL = .TRUE.
            CALL PENERS()
C           erase the old line
            CALL ALLDRW(ENT,OLDMIP)
            CALL PENDRW()
            VPDEL = .FALSE.
C           Set status to delete
            IMBUFF(1)=100
C           write back to storage
            CALL DIM500(OLDMIP,OK)
         ELSE
C           If this occurs then nit wit has indicated the same 
C           end twice ( stupid twit?)
            CALL DEPRNT(366)
         ENDIF
         RETURN
      END IF
 
C     create a line of standard status.
      IMBUFF(1)=10
      I4=IMBUFF(6)
      TMIP = MIP
C
      ENT = LINE
      VPMOV = .TRUE.
      CALL PENERS()
C     erase the old line
      CALL ALLDRW(ENT,TMIP)
      CALL PENDRW()
      VPMOV = .FALSE.
C
C      CALL ERSFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I4)
 
      IF ( P1END.NE.0  ) THEN
C        Only altering the one end
C        use point two
         IF ( P1END .GT. 0 ) THEN
            CALL RSWAP(RDBUFF(1),HPX2)
            CALL RSWAP(RDBUFF(2),HPY2)
         ELSE
            CALL RSWAP(RDBUFF(4),HPX2)
            CALL RSWAP(RDBUFF(5),HPY2)
         END IF
C        Enter as modified line in data base
         VPMOV = .TRUE.
         CALL DEM500(MP,OK)
         CALL ALLDRW(ENT,MP)
C         CALL DRWFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I4)
         VPMOV = .FALSE.
 
      ELSE  IF ( P2END.NE.0  ) THEN
C     Only altering the one end
         IF ( P2END .GT. 0 ) THEN
            CALL RSWAP(RDBUFF(1),HPX1)
            CALL RSWAP(RDBUFF(2),HPY1)
         ELSE
            CALL RSWAP(RDBUFF(4),HPX1)
            CALL RSWAP(RDBUFF(5),HPY1)
         END IF
C        Enter as modified line in data base
         VPMOV = .TRUE.
         CALL DEM500(MP,OK)
         CALL ALLDRW(ENT,MP)
C         CALL DRWFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I4)
         VPMOV = .FALSE.
      ELSE
C        set end of new line to first hit point
         CALL RSWAP(RDBUFF(4),HPX1)
         CALL RSWAP(RDBUFF(5),HPY1)
C        Enter as modified line in data base
         CALL DEM500(MP,OK)
         VPMOV = .TRUE.
         CALL ALLDRW(ENT,MP)
         VPMOV = .FALSE.
         RDBUFF(1)=HPX1
         RDBUFF(2)=HPY1
         RDBUFF(4)=HPX2
         RDBUFF(5)=HPY2
C        add new line to data base.
         VPADD = .TRUE.
         CALL DEW500(MP,OK)
         CALL ALLDRW(ENT,MP)
         NEWMIP = MP
         VPADD = .FALSE.
C         CALL DRWFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I4)
 
      END IF
 
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE DLRCVR()
C     ===================
C1    no arguments required
C
C2    subroutine DLRCVR recovers all temporarily
C2    deleted entities form the workfile,and
C2    removes the attention flags from the screen
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/swind.inc'
C
      INTEGER*4 I
      INTEGER*2 D1,D2,P,TENT
      REAL BX,BY
      LOGICAL OK
C
      EXTERNAL RSCRF,DIR500,DIM500,DBOX
C
      IF (NDATA.GT.0) THEN
         DO 50 I=1,NDATA
C           remove last entity from list
            CALL RSCRF(I,P,BX,BY,D1,D2)
            CALL DIR500(P,OK)
C           check if entity has delete status
C           if not,no need to write back
            IF (IMBUFF(1).GE.100) THEN
C              set status back to default
               IMBUFF(1)=10
C              write data back to storage
               CALL DIM500(P,OK)
            END IF
C           remove the attention flag
C            IF (I.GE.VNDATA) THEN
C               CALL DBOX(BX,BY)
C            END IF
            TENT=IMBUFF(2)
            CALL ALLDRW(TENT,P)
 50      CONTINUE
      END IF
C
C     recovery now complete
C     set NDATA back to zero
      NDATA=0
      VNDATA=1
C     everything ok,return as normal
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE INTER0(X,Y,FORWRD,RESX,RESY,END)
C     ===========================================
C1    VARTYPE           R R   L     R    R    L
C1    IOSTATUS          I I   I     O    O    O
C
C2    Find the nearest intersection to the point x,y on the line
C2    or arc in NBUFF.INC. If no intersection exists, the end point 
C2    is returned. NOTE ... The search only goes in one direction 
C2    allong the line/arc.
C2
C2    FORWARD: This is the direction of the search. If true then the 
C2             search moves toward the second end point of the line,
C2             or end angle of the arc.
C
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
C
      INTEGER*2 MIPP
      REAL XS,YS,XF,YF,X,Y,RESX,RESY,CENX,CENY,RADIUS,RR,IANG,
     +     P1,P2,P3
      LOGICAL FORWRD,OK,END,ISLINE
C
C     **********************************************
C                 MAIN AREA INITIALISAION
C     **********************************************
C     save current autoprofile cell
      CURVEC = 1
      IPCNT = 0
      RETRY = .FALSE.
      CALL MULTND()
      LINSTD = .FALSE.
      ARCSTD = .FALSE.
      PRVARC = .FALSE.
      CURARC = .FALSE.
C
      MIPP = MIP
      PCLOCK = .NOT.FORWRD
      ISLINE = IMBUFF(2).EQ.LINE
      IF (ISLINE) THEN
C        Find the intersection with a line.
         XS = RDBUFF(1)
         YS = RDBUFF(2)
         XF = RDBUFF(4)
         YF = RDBUFF(5)
C        Get the line's vector.
         IF (FORWRD) THEN
            CALL CV0L14 (XF,YF,XS,YS,L1,L2,L3)
         ELSE
            CALL CV0L14 (XS,YS,XF,YF,L1,L2,L3)
         ENDIF
C        Get nearest point on vector to the hit point.
         CALL VC0PLP(L1,L2,L3,X,Y,OX,OY)
      ELSE
C        Find the intersection with an arc.
         CENX = RDBUFF(1)
         CENY = RDBUFF(2)
         RADIUS = RDBUFF(4)
C
         RR = RADIUS
         IANG = 0
         CALL NEWPNT(CENX,CENY,RADIUS,RR,IANG,RDBUFF(5),XS,YS)
         CALL NEWPNT(CENX,CENY,RADIUS,RR,IANG,RDBUFF(6),XF,YF)
C
C        generate the start vector
         CALL CV0L14 (CENX,CENY,X,Y,P1,P2,P3 )
         CALL VV00L6 (P1,P2,P3,X,Y,L1,L2,L3)
C        Get nearest point on vector to the hit point.
         CALL VC00P4( CENX,CENY,RADIUS,P1,P2,P3,OX,OY )
      ENDIF
C
      PRMIP = IDBUFF(2)
C     Get the nearest intersection.
      CALL PASS1(OK)
      IF (OK) THEN
         RESX = XYI(1,1)
         RESY = XYI(1,2)
      ELSE
         IF (FORWRD) THEN
            RESX = XF
            RESY = YF
         ELSE
            RESX = XS
            RESY = YS
         ENDIF
      ENDIF
      END = .NOT. OK
C
C     Get the data back into NBUFF since PASS1 corrupts it.
      CALL DER500(MIPP,OK)
      MIP = MIPP
C
      END
      SUBROUTINE MAJDL1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the DELETE mode
C2    of operation is selected from the master menu.
C2    controls operation of the DELETE function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      INTEGER*2 ENT
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      CHARACTER*16 TEMP,OLIN
C
      EXTERNAL MNIDEL,CLRPEW,MNLD11,GTHFMC,GTMCHI,TCURS,
     +         GTMCLO,DLRCVR,GTCLRM
C
C     Now activate the DELETE major option menu
      CALL MNIDEL()
C
C     clear the error and prompt windows
      CALL CLRPEW()
C     Read the major option menu to find out what he wants to
C     delete. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     initialize DELETE OPTION  menu
      CALL MNLD11()
C     prompt for option select
C     Making single line the default insert text
      MEN=2
C     'a' is the token used by insert line
      CCMD='a'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select DELETE mode
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'A') THEN
C           DELETE ARC option
            CALL GTDMEN(221,3)
            CALL GTDMEN(214,3)
            CALL DELL02(ARC,.FALSE.)
            CALL FNDPOS(221,C)
            CALL GTCLRC(3,C)
            CALL FNDPOS(214,C)
            CALL GTCLRC(3,C)
         ELSE IF (CCMD.EQ.'a') THEN
C           DELETE ALL option
            CALL GTDMEN(221,3)
            CALL GTDMEN(214,3)
            CALL DELL02(ENT,.TRUE.)
            CALL FNDPOS(221,C)
            CALL GTCLRC(3,C)
            CALL FNDPOS(214,C)
            CALL GTCLRC(3,C)
         ELSE IF (CCMD.EQ.'l') THEN
C           DELETE CENTER LINE option
            CALL DELL02(CENLIN,.FALSE.)
         ELSE IF (CCMD.EQ.'H') THEN
C           DELETE HATCH option
            CALL DELL02(HATCH,.FALSE.)
         ELSE IF (CCMD.EQ.'T') THEN
C           DELETE TEXT option
            CALL DELL02(TEXT,.FALSE.)
         ELSE IF (CCMD.EQ.'L') THEN
C           DELETE LINE option
            CALL GTDMEN(221,3)
            CALL GTDMEN(214,3)
            CALL DELL02(LINE,.FALSE.)
            CALL FNDPOS(221,C)
            CALL GTCLRC(3,C)
            CALL FNDPOS(214,C)
            CALL GTCLRC(3,C)
         ELSE IF (CCMD.EQ.'D') THEN
C           DELETE DIMENSION option
            CALL DELL02(LDIMN,.FALSE.)
         ELSE IF (CCMD.EQ.'C') THEN
C           DELETE CURVE option
            CALL DELL02(SPLINE,.FALSE.)
         ELSE IF (CCMD.EQ.'c') THEN
C           DELETE Component option
            CALL DELL02(COMPI,.FALSE.)
         ELSE IF (CCMD.EQ.'s') THEN
C           DELETE symbol option
            CALL DELL02(SYMBI,.FALSE.)
         ELSE
C           unrecognized delete option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the error and prompt windows
         CALL CLRPEW()
C
         IF (CCMD.EQ.'q') GOTO 99
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         IF (MEN.EQ.3) CALL GTMCLO(MEN,CELLN)
         CALL DEPRNT(87)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
C     clear the minor option menu
      CALL GTCLRM(3)
C
      RETURN
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE MASDEL()
C     ===================
C
C2    subroutine MASDEL deletes all entities
C2    whose MI pointers are stored in the scratch
C2    workfile attached to unit SWINDU.
C2    The entity counter NDATA is reset to zero
C2    on exit from this routine.
C
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/swind.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/save.inc'
      include   'include/viewport.inc'
C
C
      INTEGER*4 I
      INTEGER*2 P,ENT,D1,D2,LPMIP,PMIP,TENT
      REAL      BX,BY,M(3,3)
C
      LOGICAL OK,GRP
      EXTERNAL UNFLAG,RSCRF,DIR500,ALLDRW,DIM500,DELMON
      EXTERNAL PENERS,PENDRW,ZRFLAG
C
      CALL I3M(M)
      IF (NDATA.EQ.0) RETURN
C
C     entities to be erased,no point in taking time
C     to properly unflag them,set valid flag pointer
C     past end of buffer to prevent this.
C      WRITE(10,*)'[MASDEL] calling UNFLAG'
      VNDATA=NDATA+1
      CALL UNFLAG(.FALSE.)
C
      LPMIP=-1
C     switch pen to erase colour
      CALL PENERS()
C
      DO 10 I=1,NDATA,1
C
C        read from workfile
         CALL RSCRF(I,P,BX,BY,D1,D2)
C
         CALL DIR500(P,OK)
C        test for visibility of any of the layers
C        save entity type being erased
         TENT=IMBUFF(2)
C         WRITE(10,*)'[MASDEL] calling ALLDRW MIP=',P
C         WRITE(10,*) '[MASDEL] P= ',P,' TENT= ',TENT
         VPDEL = .TRUE.
         CALL PENERS()
         CALL ALLDRW(TENT,P)
         CALL PENDRW()
         VPDEL = .FALSE.
C        test current status
         IF ( IMBUFF(1) .EQ.100 ) THEN
C           already marked as deleted
C           only need to delete the entity from the screen
            GRP=.FALSE.
         ELSE
C           test for grouped status
            IF (IMBUFF(1).EQ.GROUP) THEN
C              get Parent MIP
               PMIP=IMBUFF(8)
               GRP=.TRUE.
            ELSE
               GRP=.FALSE.
            END IF
            IF (TENT.EQ.COMPI.OR.TENT.EQ.SYMBI) THEN
C              must recover primary record of entity
               CALL DIR500(P,OK)
            END IF
C           Set status to delete
            IMBUFF(1)=100
C           write back to storage
            CALL DIM500(P,OK)
         END IF
C
C        Subtract one entity from layer monitor
         CALL DELMON(IMBUFF(4),.TRUE.)
C
C     must delete header for grouped entities
C     only need to do it once for each group
      IF (GRP.AND.(PMIP.NE.LPMIP)) THEN
C        read MI of Parent
         CALL DIR500(PMIP,OK)
C        Set status to delete
         IMBUFF(1)=100
C        write back to storage
         CALL DIM500(PMIP,OK)
         LPMIP=PMIP
      END IF
C
 20   CONTINUE
 10   CONTINUE
C
C     switch pen back to draw colour
      CALL PENDRW()
C     reset entity counter to zero before return.
      CALL ZRFLAG()
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE MNIDEL
C     =================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the ERASE major options.
C2
C
      EXTERNAL GTDMEN,GTDMHD,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the ERASE major options.
C      CALL GTPMEN('ERASE',' ',2,1)
      CALL GTDMHD(20,2)
C
C     Load the entity descriptors.
 
C2    H is the token for HATCH.
      CALL GTDMEN(201,2)
C2    T is the token for TEXT.
      CALL GTDMEN(202,2)
C2    A is the token for ARC.
      CALL GTDMEN(203,2)
C2    L is the token for LINE.
      CALL GTDMEN(204,2)
C2    D is the token for DIMENSION
      CALL GTDMEN(205,2)
C2    'a' is the token for ANY
      CALL GTDMEN(206,2)
C2        is the token for SYMBOL
      CALL GTDMEN(207,2)
C2        is the token for COMP
      CALL GTDMEN(208,2)
C2        is the token for CURVE
      CALL GTDMEN(209,2)
C2    'l' is the token for Center Line
      CALL GTDMEN(82,2)
C
      END
C
C     ------------------------------------------------------
C

      SUBROUTINE MNIDL0()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNIDL0 load the between points cell
C2    which may be used by arc or line
C
C     we are actually using dimension between points
      CALL GTDMEN(221,3)
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE MNLD11()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLD11 loads the AREA
C2    and ACCEPT portions of the option
C2    menu no3.
C2
C2    Tokens used here are W and CHAR(150).
C2
      CHARACTER CHAR
C
      INTRINSIC CHAR
C
      EXTERNAL GTDMEN
C
C     CHAR(149) is the token for CANCEL
C      CALL GTPMEN('Cancel',CHAR(149),3,16)
      CALL GTDMEN(197,3)
C2    e is the token for EXCLUDE
C      CALL GTPMEN('Exclude','e',3,2)
C2    W is the token for INSIDE AREA.
C      CALL GTPMEN('Inside  area','W',3,1)
      CALL GTDMEN(210,3)
C2    CHAR(150) is token for ACCEPT.
C      CALL GTPMEN('Accept',CHAR(150),3,26)
      CALL GTDMEN(198,3)
C2
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE MNUDL0()
C     ===================
C1    No arguments required.
C
      INTEGER*4 C
C2    Subroutine MNIDL0 load the between points cell
C2    which may be used by arc or line
C
C     we are actually using dimension between points
      CALL FNDPOS(221,C)
      CALL GTCLRC(3,C)
C
      END
C
C     ------------------------------------------------------
C
      SUBROUTINE SETSER(ENT,ALL,PTXT)
C     ===============================
C2    VARTYPE           I2   L   I4
C2    IOSTATUS          I    I   O
C
C2    This routine sets the search mask for delete.
C
      include 'include/masti.inc'   
      include 'include/entity.inc'
C
      INTEGER*2 ENT
      INTEGER*4 PTXT
      LOGICAL ALL
C
      CALL NOSRCH()
C     Allow deletion of groups
      GSSTAT=3
C
C     set search mask to correct entity types
      IF (ALL) THEN
C        allow searching for any entity type
         CALL ALSRCH()
         GSSTAT=2
         PTXT=327
      ELSE IF (ENT.EQ.LINE) THEN
C        enable line searching only
         CALL ADSRCH(LINE)
         PTXT=328
      ELSE IF (ENT.EQ.ARC) THEN
C        enable arc searching only
         CALL ADSRCH(ARC)
         PTXT=329
      ELSE IF (ENT.EQ.HATCH) THEN
C        enable hatch searching only
         CALL ADSRCH(HATCH)
         PTXT=330
      ELSE IF (ENT.EQ.CENLIN) THEN
C        enable hatch searching only
         CALL ADSRCH(CENLIN)
         PTXT=328
      ELSE IF (ENT.EQ.LDIMN) THEN
C        enable dimension searching only
         CALL ADSRCH(LDIMN)
         CALL ADSRCH(RDIMN)
         CALL ADSRCH(DDIMN)
         CALL ADSRCH(ADIMN)
         PTXT=331
      ELSE IF (ENT.EQ.TEXT) THEN
C        enable text searching only
         CALL ADSRCH(TEXT)
         PTXT=332
      ELSE IF (ENT.EQ.SPLINE) THEN
C        enable text searching only
         CALL ADSRCH(SPLINE)
         PTXT=327
      ELSE IF (ENT.EQ.COMPI) THEN
C        enable text searching only
         CALL ADSRCH(COMPI)
         PTXT=327
      ELSE IF (ENT.EQ.SYMBI) THEN
C        enable text searching only
         CALL ADSRCH(SYMBI)
         PTXT=327
      END IF
C
      END
C
