C
C     @(#)  412.1 date 6/11/92 display.f 
C
C
C     Filename    : display.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:35:01
C     Last change : 92/06/11 14:28:42
C
C     Copyright : Practical Technology Limited  
C     File :- display.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION DSTOP(KEYVAL,PROMPT)
C     FUNCTION   SNAPG(X,GSIZE,ORG)
C     SUBROUTINE ADDLAY(LAYDAT,NLAY)
C     SUBROUTINE ADDMON(LAY)
C     SUBROUTINE ADDNUM(NUM,LP,INPL)
C     SUBROUTINE ALLTFM(ENT,M)
C     SUBROUTINE AROHED(X,Y,L,W,U1,U2,X1,Y1,X2,Y2)
C     SUBROUTINE ATGEND()
C     SUBROUTINE BLOAD1(GEOM)
C     SUBROUTINE BLOAD2(GEOM)
C     SUBROUTINE BSPLIN(GEOM,MATR,OX,OY,DIST,VISBAS,DRWTYP)
C     SUBROUTINE CHKCMP(HX,HY,OK)
C     SUBROUTINE CHKTEX(HX,HY,X1,X2,X3,X4,X5,X6,OK)
C     SUBROUTINE CMPERA(TTMIP,LAYD,NLAY)
C     SUBROUTINE CMPRED(TTMIP,LAYD,NLAY)
C     SUBROUTINE CMPVIS(TTMIP,OK)
C     SUBROUTINE CODET(SLA,JUST,NCHAR,TEMP)
C     SUBROUTINE DELLAY(LAYDAT,NLAY)
C     SUBROUTINE DELMON(LAY,DEL)
C     SUBROUTINE DIML25(ARRAY1,ARRAY2,PAPTOW,ARRAY3)
C     SUBROUTINE DIMWL4(X1,Y1,X2,Y2,GAPL,EXTL,X3,Y3,X4,Y4)
C     SUBROUTINE DLAYER(STATUS)
C     SUBROUTINE DPOLY(POINT,USET,M,DRWTYP,OK)
C     SUBROUTINE DPTS(POINT,USET,M,OK)
C     SUBROUTINE DRW066(M)
C     SUBROUTINE DRWBUF(TFM)
C     SUBROUTINE DRWDIM()
C     SUBROUTINE DRWHT(TMIP,USET,M)
C     SUBROUTINE DRWMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     SUBROUTINE DRWPRF()
C     SUBROUTINE DRWSPL(TMIP,USET,M,DRWTYP)
C     SUBROUTINE DRWTBX(X1,X2,X3,X4,X5,X6)
C     SUBROUTINE DRWTXT(X,Y,TW,TH,TANG,Z6,TEXT)
C     SUBROUTINE EGRID()
C     SUBROUTINE ERS066(M)
C     SUBROUTINE ERSDIM()
C     SUBROUTINE ERSHT(TMIP)
C     SUBROUTINE ERSMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     SUBROUTINE ERSPRF()
C     SUBROUTINE ERSSPL(TMIP,USET,M)
C     SUBROUTINE ERSTXT(X1,X2,X3,X4,X5,X6,CBUFF)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE ADDLAY(LAYDAT,NLAY)
C     ===================
C1    no arguments required
C2
C2    clears the graphics screen,and redraws
C2    based upon a complete regeneration of the
C2    display from the contents of the database.
C
      include   'include/viewport.inc'
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
      include   'include/wtov.inc'
      include   'include/swind.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 TMIP,TMIP1,P3,ENT
      REAL      RAD,X,Y,SLT
      REAL X1,Y1,X2,Y2,XC,YC,RADIUS,SANG,EANG
      INTEGER*4 I,J,K,LAYDAT(256),NLAY
      INTEGER*4 ST
      INTEGER*2 VPN
      INTEGER*2 ENTYPE,NCHAR
      LOGICAL OK,TOTAL,ADDIT,REDRAW
C
      EXTERNAL GRID1,DIR500,ALLDRW,ADDISP
C
C     set redraw flag
      REDRAW=.FALSE.
      IF ( NMIPOS .EQ. 1 ) THEN
         IF ( SETGRD ) THEN
             CALL GRID1()
         END IF
         RETURN
      END IF
C
      TMIP=0
C     if any viewports are being used then set bitmaps for drawing
      IF(MVPACT) THEN
C         we have a single daxport as the current view
          VPN = CVPN
C         set this view to draw into
          CALL VPDRAW(VPN,.TRUE.,ST)
          IF(COLPEN.EQ.COLERS) THEN
              CALL SETAPN(COLERS)
          ELSE
              CALL SETAPN(COLFOR)
          ENDIF
      ENDIF
C
 10   TMIP=TMIP+1
C
      CALL DIR500(TMIP,OK)
C
      IF (  IMBUFF(1) .EQ. 10 .OR. IMBUFF(1).EQ.GROUP ) THEN
C
         ADDIT=.FALSE.
         DO 20 K=1,NLAY
            IF(IMBUFF(4).EQ.LAYDAT(K)) THEN
C              if this entity lies on the current layer to be shown then add it
C              the display file
               ADDIT=.TRUE.
            END IF
 20      CONTINUE
         IF ( (IMBUFF(2).EQ.SYMBI.OR.IMBUFF(2).EQ.COMPI).AND.
     +       VLAYER(IMBUFF(4)) ) THEN
C           check if this instance has any of the current layers but dont
C            add it to the dispaly file just redraw it
                CALL CMPRED(TMIP,LAYDAT,NLAY)
         ENDIF
C
         IF ( ADDIT )  THEN
C
            ENT=IMBUFF(2)
            CALL ALLDRW(ENT,TMIP)
C
            IF ( DISPV  ) THEN
               CALL ADDISP(TMIP,ENT,P3,OK)
C         Pointer P3 is set to current position
C        in the array DFILE(2,****)
C
            END IF
         END IF
      END IF
C
      IF ( TMIP .LT. (NMIPOS-1) ) GOTO 10
C
C     reset drawing ops
      CALL VPRES()
      IF ( SETGRD ) THEN
         CALL GRID1()
      END IF
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ADDMON(LAY)
C     ======================
C
      include   'include/masti.inc'
C
      INTEGER*2 LAY
C
      TLAYER(LAY)=TLAYER(LAY)+1
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ADDNUM(NUM,LP,INPL)
C     =============================
C1                       I4  C*(*)
C1                        I   IO
 
      include   'include/masti.inc'
C
      INTEGER*4 NUM,LP,NLEN,LENT,MAXLEN
      CHARACTER*(*) INPL*100,STR*3,LINE*20
      EXTERNAL NLEN,CRUNCH
C
      WRITE(UNIT=STR,FMT='(I3)' ) NUM
C     standard output
      MAXLEN = 80
      INPL(LP:LP+2)=STR
      LENT=NLEN(LNAME(NUM))
      IF ( LENT .GT. 0 ) THEN
C        add name to list
         WRITE(UNIT=LINE,FMT='(3A)') '(',LNAME(NUM)(1:LENT),')'
C        add name in brackets to string.
         IF(LP+3.LE.MAXLEN) INPL(LP+3:)=LINE
C        remove blanks
         CALL CRUNCH(INPL)
C        reset pointer.
         LP=NLEN(INPL)
C
      END IF
C     add comma
      IF(LP+3.LE.MAXLEN)  INPL(LP+3:LP+3)=','
C     remove all the blanks
      CALL CRUNCH(INPL)
 
C     reset line pointer
      LP=NLEN(INPL)+2
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ALLTFM(ENT,M)
C     ========================
C
C1    vartype           I2 R(3,3)
C1    iostatus          I    I
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
C
C2    Subroutine ALLTFM transforms the entity
C2    data currently in the buffers by the transformation
C2    matrix defined in M.The data is not moved from the buffers
C2    therefore the actual stored entity data is not affected.
C2    This routine intended only for display purposes.
C
      REAL M(3,3),PARS(9),TREFDT(10,3)
      INTEGER*2 ENT,I,II
      LOGICAL OK
C
      EXTERNAL MV0003,MV0005,MV0033,MV0034,MV0035,MV0085
C
C     save the refdat common block
      DO 10 I = 1,10
        DO 20 II = 1,3
           TREFDT(I,II) = REFDAT(I,II) 
  20    CONTINUE
  10  CONTINUE
      CALL DECODM(M,PARS)
C     scale setting
      REFDAT(7,3) = PARS(4)
      
      IF (ENT.EQ.MARKER) THEN
         CALL MV0002(RDBUFF,M)
      ELSE IF (ENT.EQ.LINE) THEN
         CALL MV0003(RDBUFF,M)
      ELSE IF (ENT.EQ.ARC) THEN
         CALL MV0005(RDBUFF,M)
      ELSE IF (ENT.EQ.TEXT) THEN
         CALL MV0085(RDBUFF,M)
      ELSE IF (ENT.EQ.CENLIN) THEN
         CALL MV0030(RDBUFF,M)
      ELSE IF (ENT.EQ.LDIMN) THEN
         CALL MV0DIM(ENT,M)
      ELSE IF (ENT.EQ.RDIMN.OR.ENT.EQ.DDIMN.OR.ENT.EQ.GLABEL) THEN
         CALL MV0DIM(ENT,M)
      ELSE IF (ENT.EQ.ADIMN) THEN
         CALL MV0DIM(ENT,M)
      END IF
C
      DO 30 I = 1,10
        DO 40 II = 1,3
           REFDAT(I,II) = TREFDT(I,II) 
  40    CONTINUE
  30  CONTINUE
      END
C
C-------------------------------------------------------------
C
C
      SUBROUTINE AROHED(X,Y,L,W,U1,U2,X1,Y1,X2,Y2)
C     ============================================
C
C2    Subroutine AROHED returns in X1Y1,X2Y2 the vertices of an
C2    arrowhead with point at XY, pointing in the direction of
C2    the unit vector U1U2. The arrowhead length and width are
C2    passed in L and W.
C
      REAL X,Y,L,W,U1,U2,X1,Y1,X2,Y2
      EXTERNAL UC00P4
C
C     Find construction point for the vertices of the arrowhead.
C     This point will be in the reverse direction along the vector
C     from the reference point XY.
      CALL UC00P4(X,Y,L,-U1,-U2,X2,Y2)
C
C     Find first vertice point. This will be along a vector perpend.
C     to the original supplied vector.
      CALL UC00P4(X2,Y2,W/2,-U2,U1,X1,Y1)
C
C     Find second vertice point. This will be along a reverse
C     perpend. vector to the original vector, distance W from X1Y1.
      CALL UC00P4(X1,Y1,W,U2,-U1,X2,Y2)
C
C     Return with complete arrowhead defined by XY,X1Y1,X2Y2
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ATGEND()
C     ===================
C1    no arguments required
C
C2    Subroutine ATGEND clears and generates the
C2    graphics screen based on the contents of
C2    the display file.Ensures maximum performance
C2    on sequential zoom operations
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/wtov.inc'
      include   'include/swind.inc'
      include   'include/cross.inc'
      include   'include/viewport.inc'
C
      INTEGER*2 TMIP,ENT,P3,TDFPT1,TDFPT2
      INTEGER*2 POINT,I,NCHAR,TVPN,TDFP
      REAL      X,Y,RAD,SLT
      INTEGER*4 BIT,ST
      REAL X1,Y1,X2,Y2,XC,YC,RADIUS,SANG,EANG
      INTEGER*2 ENTYPE
      LOGICAL OK,TOTAL
C
      EXTERNAL CLEAR,CHKBOX,DRAWLW,DISFRA,LFSETS,DRWFLW,
     +          BELL,RDISPF,RAD,ADDISP
      EXTERNAL DRWFAW
C
      CALL SNAQR()
C     save current world coordinates back to storage
      CALL TRNDP()
C     reset cross file pointer
      CRSPTR = 1
C     clear the graphics screen
      CALL  CLEAR()
C     reset line font parameters
      CALL LFSETS()
C     show the drawing frame
      CALL DISFRA()
C
C     quick check for anything in the database first
      IF ( LDFILE(CVPN).EQ.1 ) GOTO 89
C     Reset display file pointer to one
      VPDFIL = .FALSE.
C     save current display file pointer
      TDFP = LDFILE(CVPN)-1
      LDFILE(CVPN) = 1
C
      POINT=0
C     find the new display pointer
C     reset the pointer for the new display file
C
 10   POINT=POINT+1
C
C     read the display file
      CALL RDISPF(POINT,ENT,TMIP,OK)
C
C     test for abort of redraw from keyboard
C     is this a valid enty 
      IF ( TMIP .GT. 0 ) THEN
         CALL DIR500(TMIP,OK)
         IF ( VLAYER(IMBUFF(4)) ) THEN
C             draw it
              CALL ALLDRW(ENT,TMIP)
C             dispv will be set if any of the entity
C             was visible
              IF ( DISPV ) THEN
C                store the current display file pointer
                 CALL ADDISP(TMIP,ENT,P3,OK)
C                reset to current display file
              END IF
         END IF
C
      END IF
C
C     test to end of file
      IF ( POINT.LT.TDFP ) GOTO 10
C
99     CONTINUE
C
 89   CONTINUE
      DRAWN=.TRUE.
C     check if grid is set.
      IF ( SETGRD ) CALL GRID1()
C     Draw in any profiles
      CALL DRWPRF()
C     Release display and update
      CALL SNREL()
C
C     viewporting flags
      VPDFIL = .FALSE.
      END

      SUBROUTINE BLOAD1(GEOM)
C     ==========================
      REAL FP(3,4) ,GEOM(3,4)
      INTEGER*4 I,J
C
      DO 20 I=2,4
         DO 20 J=1,3
            GEOM(J,I-1)=GEOM(J,I)
 20   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE BLOAD2(GEOM)
C     ==========================
      REAL FP(3,4),GEOM(3,4)
      INTEGER*4 I,J
C
      DO 20 I=2,3
         DO 20 J=1,3
            GEOM(J,I-1)=GEOM(J,I)
 20   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 bsplin.ftn Daxcad revision 1.8
      SUBROUTINE BSPLIN(GEOM,MATR,OX,OY,DIST,VISBAS,DRWTYP)
C     =====================================================
C
C2      If  DRWTYP =  0 ... to the screen.
C2                   -1 ... to the plot file.
C2                   >0 ... to the "write" file.
C2      If DRWTYP>0, it is the scratch file unit number.
C 
      include 'include/save.inc'
      include 'include/masti.inc'
      include 'include/pendat.inc'
      include 'include/drwspl.inc'
C
      REAL INC,GEOM(3,4),MATR(4,4),COEFF(3,4),TP,T,T0,T1,T2,T3,
     +     OX,OY,P(3),X1,Y1,X2,Y2,NX,NY,DIST,RD1,RD2,FP(3)
      INTEGER K,J,I
      INTEGER*4 FONT,DRWTYP
      INTEGER*2 TYP,MD
      LOGICAL VISBAS,SAME
      EXTERNAL SAME
C                     
      FONT = 1
      RD1=0.0
      RD2=0.0
      TYP=2
      MD=0
      FP(3)=0.0
C
C     calculate the distance between
      IF (DRWTYP.EQ.0) THEN
C        Drawing.
         CALL WTOSVX(DIST,INC)
         IF(.NOT.SAME(0.0,INC)) INC=15.0/INC
      ELSE IF (DRWTYP.EQ.-1) THEN
C        Plotting.
         CALL WTOPVX(DIST,INC)
         INC=INC/FACT
         IF(.NOT.SAME(INC,0.0) ) INC=2.0/INC
      ELSE        
C        Draw to a scratch file for "write"
         INC = RESLTN/DIST
C
         IF (FRSTPT) THEN
C           Got to output the start point.
            FP(1)=OX
            FP(2)=OY
            NPSPL = NPSPL + 1
            WRITE(UNIT=DRWTYP,REC=NPSPL) MD,TYP,RD1,RD2,FP(1),FP(2),
     +                                FP(3),0.0,0.0
            FRSTPT = .FALSE.
         ENDIF
      ENDIF
C      WRITE(10,*) '[BSPLIN] INCREMENT INC= ',inc
      INC=MAX(INC,0.01)
      INC=MIN(INC,1.0)
      DO 10 K=1,3
         DO 10 J=1,4
            COEFF(K,J)=0.0
            DO 10 I=1,4
               COEFF(K,J)=COEFF(K,J)+MATR(I,J)*GEOM(K,I)
 10   CONTINUE
C
      DO 40 T=0.0,1.0,INC
         T0=1.0
         T1=T
         T2=T*T
         T3=T*T2
         DO 50 I=1,3
            P(I)=T3*COEFF(I,1)+
     +           T2*COEFF(I,2)+
     1           T1*COEFF(I,3)+
     2           T0*COEFF(I,4)
 50      CONTINUE
C
         IF (DRWTYP.EQ.0) THEN
C           Drawing.
            CALL WO2SC(OX,OY,X1,Y1)
            CALL WO2SC(P(1),P(2),X2,Y2)
            IF (.NOT.(SAME(X1,X2).AND.SAME(Y1,Y2))) THEN
               CALL DRAWLT(OX,OY,P(1),P(2))
C              lock visibility
               IF(DISPV) VISBAS=.TRUE.
            ENDIF
         ELSE IF (DRWTYP.EQ.-1) THEN
C           Plotting.
            CALL PLTFLW(OX,OY,P(1),P(2),FONT)
         ELSE        
C           Draw to a scratch file for "write"
            FP(1)=P(1)
            FP(2)=P(2)
            NPSPL = NPSPL + 1
            WRITE(UNIT=DRWTYP,REC=NPSPL) MD,TYP,RD1,RD2,FP(1),FP(2),
     +                                FP(3),0.0,0.0
         ENDIF
C
         OX=P(1)
         OY=P(2)
C
 40   CONTINUE             
C
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE CHKCMP(HX,HY,OK)
C     ==========================
C1                       R, R,  L
C1                       I, I,  O
      include 'include/wrkdat.inc'
C
      REAL X(4),Y(4),HX,HY
      LOGICAL OK
C
C     simple check to see if a point is within a box
      X(1) = RWORK(1,1)
      Y(1) = RWORK(2,1)
      X(2) = RWORK(1,1)
      Y(2) = RWORK(2,1)

      X(1) = MIN(X(1),RWORK(4,1))
      Y(1) = MIN(Y(1),RWORK(5,1))
      X(2) = MAX(X(2),RWORK(4,1))
      Y(2) = MAX(Y(2),RWORK(5,1))

      X(1) = MIN(X(1),RWORK(4,2))
      Y(1) = MIN(Y(1),RWORK(5,2))
      X(2) = MAX(X(2),RWORK(4,2))
      Y(2) = MAX(Y(2),RWORK(5,2))

      X(1) = MIN(X(1),RWORK(1,2))
      Y(1) = MIN(Y(1),RWORK(2,2))
      X(2) = MAX(X(2),RWORK(1,2))
      Y(2) = MAX(Y(2),RWORK(2,2))

      OK = HX.GT.X(1).AND.
     +     HY.GT.Y(1).AND.
     +     HX.LT.X(2).AND.
     +     HY.LT.Y(2)
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE CHKTEX(HX,HY,X1,X2,X3,X4,X5,X6,OK)
C     =============================================
C1                       R, R, R, R, R, R, R, R, L
C1                       I, I, I, I, I, I, I, I, O
C
      include 'include/nbuff.inc'
C
      REAL X(4),Y(4),X1,X2,X3,X4,X5,X6,SCA,A(3,3),
     +    XMIN,XMAX,YMIN,YMAX,SLA,HX,HY,XC,YC,XC1,YC1
      INTEGER*2 NCHAR,JST,I
      LOGICAL OK
      EXTERNAL TORGTL,TORGTR,TORGBR,TXTORG,UCODET
C
      SCA=1.08
      CALL UCODET(X6,SLA,JST,NCHAR)
C
      DO 10 I=1,4
        CALL TXTORG(I,X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(I),Y(I))
 10   CONTINUE
C
      XC=(X(1)+X(2))/2.0
      YC=(Y(1)+Y(2))/2.0
      CALL SCAP2D(XC,YC,SCA,SCA,A)
      XC1=(X(3)+X(4))/2.0
      YC1=(Y(3)+Y(4))/2.0
      DO 20 I=3,4
         CALL NEWXY(X(I),Y(I),XC,YC,A)
         X(I)=XC
         Y(I)=YC
 20   CONTINUE
      CALL SCAP2D(XC1,YC1,SCA,SCA,A)
      DO 30 I=1,2
         CALL NEWXY(X(I),Y(I),XC,YC,A)
         X(I)=XC
         Y(I)=YC
 30   CONTINUE
C
      CALL POLYWN(HX,HY,X,Y,4,OK)
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE CMPERA(TTMIP,LAYD,NLAY)
C     ====================================
C
C1    vartype            I2   I4*256 I4
C1    iostatus           I      I     I
C
C2    This routine will erase any entity on the layer array
C2
 
      include 'include/save.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
 
      REAL M(3,3)
      INTEGER*4 LAYD(256),NLAY,I
      INTEGER*2 TTMIP,ENT
      LOGICAL OK
 
 
C     copy data for drw066
      CALL I3M(M)
      DO 10 I=1,NLAY
          LAYDAT(I)=LAYD(I)
10    CONTINUE
      LAYTOT=NLAY
C      WRITE(10,*) '[CMPERA] ERASE A PORTION OF AN INSTANCE PERHAPS'
 
C     get transform of matrix
      CALL PENERS()
      CALL ALLRD(TTMIP,ENT,M,OK)
      CMPERS=.TRUE.
      CALL DRW066(M)
      CMPERS=.FALSE.
      CALL PENDRW()
      END
 
 
 
      SUBROUTINE CMPRED(TTMIP,LAYD,NLAY)
C    ======== =========================
C
C1    vartype            I2   I4*256   I4
C1    iostatus           I      I       I
C
C2    This routine checks the whether any entity lies on the requested layer
C2
 
      include 'include/save.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
 
      REAL M(3,3)
      INTEGER*4 LAYD(256),NLAY,I
      INTEGER*2 TTMIP,ENT
      LOGICAL OK
 
 
C     copy data for drw066
      DO 10 I=1,NLAY
          LAYDAT(I)=LAYD(I)
10    CONTINUE
      LAYTOT=NLAY
 
      CALL I3M(M)
      CALL DIR500(TTMIP,OK)
 
      OK=.FALSE.
      IF(IMBUFF(2).EQ.COMPI.OR.IMBUFF(2).EQ.SYMBI) THEN
 
          CMPDRW=.TRUE.
          CALL ALLRD(TTMIP,ENT,M,OK)
          CALL DRW066(M)
 
      ENDIF
      CMPDRW=.FALSE.
      END
 
      SUBROUTINE CMPVIS(TTMIP,OK)
C     ===========================
C
C1    vartype            I2   L
C1    iostatus           I    O
C
C2    This routine checks the visibily of a component or symbol
C2
 
      include 'include/save.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
 
      REAL M(3,3)
      INTEGER*2 TTMIP
      LOGICAL OK
 
C     get entity information
      CALL I3M(M)
      CALL DIR500(TTMIP,OK)
 
      OK=.FALSE.
      IF(IMBUFF(2).EQ.COMPI.OR.IMBUFF(2).EQ.SYMBI) THEN
          LAYTST=.TRUE.
          CALL DRW066(M)
          OK = .NOT.LAYTST
      ENDIF
      LAYTST = .FALSE.
      END
 
C      SUBROUTINE CODET(SLA,JUST,NCHAR,TEMP)
CC     ======================================
CC1    vartype           R ,  I2, I2  ,  R
CC1    iostatus          I    I   I      O
CC
CC2    Subroutine CODET encodes the SLANT of a text
CC2    string,together with JUSTIFICATION and character
CC2    count in a REAL variable.
CC2    The acceptable justification range is 1 to 9.
CC2    The acceptable SLANT range is -90.0 to +90.0 degrees
CC2    Positive slant is clockwise from vertical.
CC2    The max no chars is 255.
CC2    Encoded form of data as follows:
CC2          High Word   Low Word
CC2        |JUST |SLANT|  NCHAR   |
CC2    The encoding assumes Motorola storage convention.
CC2    ie 4-byte word stored in ascending memory
CC2    as byte4,byte3,byte2,byte1.
CC
C      REAL TEMP,SLA,X,S
C      INTEGER*2 I(2),JUST,NCHAR,J1
C      EQUIVALENCE(I,X)
CC
CC     ensure justification is in range 1-9
C      J1=MAX(INT(1),INT(JUST))
C      J1=MIN(INT(9),INT(J1))
CC     ensure SLANT within range
C      S=MOD(SLA,90.0)
C      I(1)=((256*J1)+INT(S+90))
C      I(2)=NCHAR
CCSUN386
CC      I(2)=((256*J1)+INT(S+90))
CC      I(1)=NCHAR
CCSUN386
C      TEMP=X
CC
C      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DELLAY(LAYDAT,NLAY)
C     ===================
C1    no arguments required
C
C2    Subroutine DELLAY erases layer data from the
C2    graphics screen based on the contents of
C2    the display file.
C
      include   'include/viewport.inc'
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/wtov.inc'
      include   'include/swind.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 TMIP,ENT,P3,TENT,MIP1
      INTEGER*4 K,LAYDAT(256),NLAY
      INTEGER*4 ST
      INTEGER*2 POINT,I,NCHAR
      REAL      TEMP,X,Y,RAD,SLT
      REAL X1,Y1,X2,Y2,XC,YC,RADIUS,SANG,EANG
      INTEGER*2 ENTYPE,I1(2)
      INTEGER*2 VPN
      LOGICAL OK,TOTAL,DELIT,REDRAW
      EQUIVALENCE(TEMP,I1)
C
      EXTERNAL GRID1,RDISPF,DIR500,ALLDRW,WDISPF
      EXTERNAL PENERS,PENDRW
C
C
C     jump past everything if nothing in display file
      IF ( LDFILE(CVPN) .EQ. 1 ) GOTO 89
C
C     select erase colour
      CALL PENERS()
C
      IF(MVPACT) THEN
C         we have a single edaxport as the current view
          VPN = CVPN
C         set this view to draw into
          CALL VPDRAW(VPN,.TRUE.,ST)
          IF(COLPEN.EQ.COLERS) THEN
              CALL SETAPN(COLERS)
          ELSE
              CALL SETAPN(COLFOR)
          ENDIF
      ENDIF
      POINT=0
C
 10   POINT=POINT+1
C
      CALL RDISPF(POINT,ENT,TMIP,OK)
      TENT=ENT
      MIP1=TMIP
C
      IF ( TMIP .GT. 0 ) THEN
C
          CALL DIR500(TMIP,OK)
C
          IF (  IMBUFF(1) .EQ. 10. OR.IMBUFF(1).EQ.GROUP ) THEN
C
            DELIT=.FALSE.
C
             DO 20 K=1,NLAY
                IF(IMBUFF(4).EQ.LAYDAT(K)
     +             .AND.IMBUFF(4).NE.CLAYER    ) THEN
                   DELIT=.TRUE.
                END IF
C            WRITE(10,*) 'LAYER,ENT,LAYER,DELETE',
C     +      LAYDAT(K),IMBUFF(2),IMBUFF(4),DELIT
 20          CONTINUE
C
C            WRITE(10,*) 'LAYER,ENT,LAYER',
C     +      LAYDAT(K),IMBUFF(2),IMBUFF(4)
 
             ENTYPE=IMBUFF(2)
C
             IF ( DELIT ) THEN
C               draw it out
 
 
                CALL ALLDRW(ENTYPE,TMIP)
C               ensure redraw does not pick up this entity
                CALL WDISPF(POINT,TENT,-MIP1,OK)
             ELSE IF ( IMBUFF(2).EQ.SYMBI.OR.IMBUFF(2).EQ.COMPI ) THEN
C               if this component has any layers on it that hav to be erased
                CALL CMPERA(TMIP,LAYDAT,NLAY)
                CALL PENERS()
             ENDIF
C
          END IF
C
      END IF
C
      IF ( POINT .LT. LDFILE(CVPN)-1 ) GOTO 10
C
C     select draw colour
      CALL PENDRW()
C
 89   CONTINUE
C     reset all displays
      CALL VPRES()
      IF ( SETGRD ) CALL GRID1()
C
      END
C
C-------------------------------------------------------------
C
C
      SUBROUTINE DELMON(LAY,DEL)
C     ======================
C
      include   'include/masti.inc'
C
      INTEGER*2 LAY
      LOGICAL DEL
C
C     delete from monitor of layer LAY.
      TLAYER(LAY)=TLAYER(LAY)-1
C     if entity has been delete add the delete count.
      IF ( DEL ) DELENT=DELENT+1
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DIML25(ARRAY1,ARRAY2,PAPTOW,ARRAY3)
C     ==============================================
C
C1    vartype            R(6)   R(6)    R     R(10)
C1    iostatus            I      I      I       O
C
C2    Subroutine DIML25 returns the world co-ords. of a dim. line
C2    end point and arrowhead vertices. The origin of the dim. line
C2    is assumed XY and the dimension line length L.
C2    The unit vector U1U2 is the dimension witness line unit
C2    vector.
C2    Arrowhead size is defined by ALNG and AWDT.
C2    X1Y1 returns the dim. line end point, and the arrowhead is
C2    completely defined by XY,X2Y2,X3Y3.
C2    All dtat passed in arrays.
C
      REAL ARRAY1(6),ARRAY2(6),ARRAY3(10),PAPTOW
      REAL U3,U4
C
      EXTERNAL UC00P4,AROHED
C
C     Must ensure correct rotation of unit vector to
C     create dim. line vector.
C     If incY<0 rotate ccw.
C     If incY=0 and incX>0, rotate ccw.
C
      IF (ARRAY2(1).EQ.0) THEN
C        horizontal witness requires vertical dimension vector
         U3=1
         U4=0
C
      ELSE IF (ARRAY2(1).LT.0) THEN
C        Rotate ccw.
         U3=-ARRAY2(2)
         U4=ARRAY2(1)
      ELSE
C        Rotate cw.
         U3=ARRAY2(2)
         U4=-ARRAY2(1)
      ENDIF
C
C     copy start point of dim line to output
      ARRAY3(3)=ARRAY1(3)
      ARRAY3(4)=ARRAY1(4)
C     same point is point of arrow,copy it to output
      ARRAY3(9)=ARRAY3(3)
      ARRAY3(10)=ARRAY3(4)
C     Find dim. line end point and put in output.
      CALL UC00P4(ARRAY1(3),ARRAY1(4),ARRAY1(5),
     +            U3,U4,ARRAY3(1),ARRAY3(2))
C
      IF (ARRAY1(5).LT.0) THEN
C        vector direction is correct as it stands
C        generate arrowhead coords.
         CALL AROHED(ARRAY1(3),ARRAY1(4),
     +               ARRAY2(5)*PAPTOW,ARRAY2(6)*PAPTOW,
     1               U3,U4,ARRAY3(5),ARRAY3(6),ARRAY3(7),ARRAY3(8))
      ELSE
C        Arrowhead vector always opposite direction.
C        Generate arrowhead co-ords.
         CALL AROHED(ARRAY1(3),ARRAY1(4),
     +               ARRAY2(5)*PAPTOW,ARRAY2(6)*PAPTOW,
     1            -U3,-U4,ARRAY3(5),ARRAY3(6),ARRAY3(7),ARRAY3(8))
      END IF
C
C     Return with vertices in ARRAY3(1-10).
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DIMWL4(X1,Y1,X2,Y2,GAPL,EXTL,X3,Y3,X4,Y4)
C     ====================================================
C1    vartype           R  R  R  R   R    R   R  R  R  R
C1    iostatus          I  I  I  I   I    I   O  O  O  O
C2
C2    A single precision version of DIMWL1
C2    This routine returns the end points of the dimension
C2    witness line drawn from reference point X1,Y1 to the
C2    end of dimension line at X2,Y2 using the gap length
C2    GAPL and the extension length EXTL.The witness line
C2    coords are returned in X3,Y3,X4,Y4
C
C
      REAL X1,Y1,X2,Y2,X3,Y3,X4,Y4,GAPL,EXTL,L1,L2,L3
      LOGICAL SAME
C
      EXTERNAL CV0L14,VC00P4,SAME
C
C     test for zero length witness line
      IF ( SAME(X1,X2) .AND. SAME(Y1,Y2) ) THEN
C        return end points of witness line equal
         X3=X1
         X4=X1
         Y3=Y1
         Y4=Y1
         RETURN
      END IF
C     find the vector from X1,Y1 to X2,Y2
      CALL CV0L14(X1,Y1,X2,Y2,L1,L2,L3)
C     Find the point X3,Y3 which is GAPL along the
C     vector from X1,Y1.
      CALL VC00P4(X1,Y1,GAPL,L1,L2,L3,X3,Y3)
C     Find the point X4,Y4 which is EXTL along the
C     vector from X2,Y2.
      CALL VC00P4(X2,Y2,EXTL,L1,L2,L3,X4,Y4)
C
C     That's all folks.....
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DLAYER(STATUS)
C     =========================
C1    VARTYPE             L
C1    IOSTATUS            I
C
C2    This routine will delete either hidden or shown layers
      include 'include/layer.inc'
 
       LOGICAL STATUS
       INTEGER*4 I,J
 
      J=0
      DO 10 I=1,FINTOT
 
          IF ((FINLAY(I).LT.256.AND..NOT.STATUS)
     +      .OR.(FINLAY(I).GT.255.AND.STATUS) ) THEN
              J=J+1
              FINLAY(J)=FINLAY(I)
          ENDIF
 
10    CONTINUE
 
      FINTOT=J
C      WRITE(10,*) '[DLAYER] FINLAY= ',(FINLAY(I),I=1,FINTOT )
      END
C
C     ------------------------------------------------------------
C
      SUBROUTINE DPOLY(POINT,USET,M,DRWTYP,OK)
C     ========================================
C
C2      If  DRWTYP =  0 ... to the screen.
C2                   -1 ... to the plot file.
C2                   >0 ... to the "write" file.
C2      If DRWTYP>0, the absolute of DRWTYP is the file unit number.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/drwspl.inc'
C
      REAL M(3,3),BUFFER(6,2),XP,YP,XP1,YP1,FP(3),RD1,RD2
      INTEGER*4 I4,DRWTYP
      INTEGER*2 POINT,ED,MD
      LOGICAL OK,USET,SAME,VISBAS,STRT
      EXTERNAL SAME
C
      VISBAS=.FALSE.
      STRT = .TRUE.
      RD1=0.0
      RD2=0.0
      ED=2
      MD=0
      FP(3)=0.0
C
      IF ( USET ) THEN
         CALL NEWXY(RDBUFF(1),RDBUFF(2),XP,YP,M)
         RDBUFF(1)=XP
         RDBUFF(2)=YP
      END IF
C
      CALL MLOAD6(RDBUFF,BUFFER(1,2))
      IF ( POINT .NE. 0 ) THEN
         CALL DBR500(POINT,OK)
         IF ( .NOT. OK ) RETURN
         POINT=IDBUFF(3)
      END IF
C
 10   CONTINUE
C
      CALL MLOAD6(BUFFER(1,2),BUFFER(1,1))
      IF ( USET ) THEN
         CALL NEWXY(RDBUFF(1),RDBUFF(2),XP,YP,M)
         RDBUFF(1)=XP
         RDBUFF(2)=YP
      END IF
      CALL MLOAD6(RDBUFF,BUFFER(1,2))
C
      IF (DRWTYP.EQ.0) THEN
C        Draw to the screen.
         IF(.NOT.(SAME(BUFFER(1,1),BUFFER(1,2)).
     +     AND.SAME(BUFFER(2,1),BUFFER(2,2)))) THEN
             CALL DRAWLT(BUFFER(1,1),BUFFER(2,1),
     +             BUFFER(1,2),BUFFER(2,2))
C            lock the visibility
             IF(DISPV) VISBAS=.TRUE.
         ENDIF
      ELSE IF (DRWTYP.EQ.-1) THEN
C        Draw to a plot file.
         I4=IMBUFF(6)
         CALL PLTFLW(BUFFER(1,1),BUFFER(2,1),BUFFER(1,2),
     +         BUFFER(2,2),I4)
      ELSE
C        Draw to a scratch file for "write"
         IF (STRT) THEN
C           Musn't forget the start point.
            FP(1)=BUFFER(1,1)
            FP(2)=BUFFER(2,1)
            NPSPL = NPSPL + 1
            WRITE(UNIT=DRWTYP,REC=NPSPL) MD,ED,RD1,RD2,FP(1),FP(2),
     +                                FP(3),0.0,0.0
            STRT = .FALSE.
         ENDIF                            
C        Note the end of the line segment.
         FP(1)=BUFFER(1,2)
         FP(2)=BUFFER(2,2)
         NPSPL = NPSPL + 1
         WRITE(UNIT=DRWTYP,REC=NPSPL) MD,ED,RD1,RD2,FP(1),FP(2),FP(3),
     +                                0.0,0.0
      ENDIF
C
      IF ( POINT .NE. 0 ) THEN
         CALL DBR500(POINT,OK)
         IF ( .NOT. OK ) RETURN
         POINT=IDBUFF(3)
         GOTO 10
      END IF
C
C     set visibility flag
      CALL SETVIS(VISBAS)
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DPTS(POINT,USET,M,OK)
C     ================================
C1                      I2
C1                      I
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
      REAL M(3,3),XP,YP,XP1,YP1
      REAL X(5),Y(5)
      INTEGER*2 POINT
      LOGICAL OK,USET,VISBAS
C
      VISBAS=.FALSE.
 10   CONTINUE
C
      IF ( USET ) THEN
         CALL NEWXY(RDBUFF(1),RDBUFF(2),XP,YP,M)
      ELSE
         XP=RDBUFF(1)
         YP=RDBUFF(2)
      END IF
      CALL DCROSS(XP,YP)
      CALL WO2SC(XP,YP,XP1,YP1)
      CALL WRKHIT(XP1,YP1,OK)
C     lock visability
      IF(OK)  VISBAS=.TRUE.
C
      IF ( POINT .NE. 0 ) THEN
         CALL DBR500(POINT,OK)
         IF ( .NOT. OK ) RETURN
         POINT=IDBUFF(3)
         GOTO 10
      END IF
C     set visibility flag
      CALL SETVIS(VISBAS)
C
      END
C
C
C-----------------------------------------------------------
C
      SUBROUTINE DRW066(M)
C     ====================
C
C1    vartype        R(3,3)
C1    iostatus         I
C
C2    This is a brilliant new component draw routine
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/wtov.inc'
      include 'include/masti.inc'
      include 'include/compkr.inc'
      include 'include/save.inc'
      include 'include/viewport.inc'
C
      CHARACTER*2 INPL
      INTEGER*4 DI,TOT
      INTEGER*2 TTMIP,ENTPNT,RELP,STKP,VP
      INTEGER*2 ENT,I,J,CMPMIP,CMPENT
      LOGICAL OK,FINISH,DELETE,CMPVP(0:MAXVP)
      LOGICAL L0,L1,L2,L3,L4,VISBAS
      LOGICAL HWF
 
      EXTERNAL SPCDRW
C     start relation list
 
C
C      CALL HFONT(.TRUE.)
C      CALL DRAWLW(RWORK(1,1),RWORK(2,1),RWORK(1,2),RWORK(2,2))
C      CALL DRAWLW(RWORK(1,2),RWORK(2,2),RWORK(4,1),RWORK(5,1))
C      CALL DRAWLW(RWORK(4,1),RWORK(5,1),RWORK(4,2),RWORK(5,2))
C      CALL DRAWLW(RWORK(1,1),RWORK(2,1),RWORK(4,2),RWORK(5,2))
C      CALL HFONT(.FALSE.)
      CALL CHKBOX(RWORK(1,1),RWORK(2,1),RWORK(4,1),RWORK(5,1),TOT,OK)
      IF(.NOT.OK) THEN
          DISPV = .FALSE.
          RETURN
      END IF
C     save the current ents
      CMPMIP = MIP
      CMPENT = IMBUFF(2)
C     save logical flags
      L0=OPFLAG(1)
      L1=OPFLAG(6)
      L2=OPFLAG(7)
      L3=OPFLAG(3)
      L4=OPFLAG(11)
C     set opflags to ensure scale done properly
      OPFLAG(1)=.TRUE.
      OPFLAG(6)=.TRUE.
      OPFLAG(7)=.TRUE.
      OPFLAG(3)=.FALSE.
      OPFLAG(11)=.FALSE.
C     save incoming matrix
      CALL I3M(IDM)
      CALL I3M(IMM)
      CALL MULT3M (M,IDM,CDT)
C     set variables
      VISBAS = .FALSE.
      STKP=0
      RELP = IMBUFF(10)
      FINISH = .FALSE.
      ENTPNT = 4
C     viewport locking visiblity flags
      DO 310 VP = 0,MAXVP
          CMPVP(VP)=.FALSE.
310   CONTINUE
C
C     if relation pointer is undefined then dinna draw
      IF ( RELP.LE.0 ) GOTO 200
 
100   CONTINUE
C     read the first record and continuation if nec
 
      CALL DRR950(RELP,OK)
 
      IF(RLBUFF(1).NE.200.OR.ENTPNT.EQ.11) THEN
 
          RELP=RLBUFF(2)
C         test for last line
          IF ( RELP.LE.0 ) THEN
              FINISH = .TRUE.
          ELSE
              CALL DRR950(RELP,OK)
C             reset subscript count
              ENTPNT = 4
 
          END IF
      ENDIF
 
      IF ( .NOT.FINISH ) THEN
 
C         save entity to be read
          TTMIP = RLBUFF(ENTPNT)
 
C         are we finished.
          IF (TTMIP.LE.0) THEN
 
               ENTPNT = 11
               GOTO 100
C
          ENDIF
 
          ENTPNT = ENTPNT + 1
 
C         get current entity
          CALL ALLRD ( TTMIP,ENT,ME,DELETE)
 
          IF ( ENT.EQ.COMPI.OR.ENT.EQ.SYMBI ) THEN
C             Its an instance go and draw it but
C             first save the current pointers and transform
 
C             check nesting level and whether instance resolved
C             This variable is set in the include file COMPKR
              IF(STKP.GE.NSTLIM.OR.IMBUFF(10).LE.0) GOTO 100
 
C             increment stack pointer
              STKP = STKP + 1
 
C             save array
              DO 20 I=1,3
                 DO 20 J=1,3
 
 20                STKTFM(J,I,STKP) = CDT(J,I)
 
 
C             save pointers
              RELMIP(STKP) = RELP
              RELSUB(STKP) = ENTPNT
 
C             recover this instance details and start reading it
              FINISH = .FALSE.
              RELP = IMBUFF(10)
              ENTPNT = 4
 
C             update the current drawing transform
              CALL MULT3M(ME,CDT,IMM)
              CALL MULT3M(IMM,IDM,CDT)
 
          ELSE
 
C             make sure the data is transformed for the applications
              CALL ALLTFM(ENT,CDT)
 
C     ***********************************************
C                APPLICATIONS SECTION
C     ***********************************************
 
c      WRITE(10,*) '[DRW066] ',CMPLAY,LAYTST,CMPDRW,CMPERS
              IF ( CMPLAY ) THEN
 
C                 This one swithces the layers of the master entitys
                  VLAYER(IMBUFF(4))=.TRUE.
 
              ELSE IF ( LAYTST ) THEN
 
C                 this one tests the complete visibility of a component
                  CALL DIR500(TTMIP,OK)
                  IF(VLAYER(IMBUFF(4)) )THEN
                       LAYTST=.FALSE.
                        GOTO 200
                  ENDIF
 
               ELSE IF ( CMPDRW ) THEN
 
C                 check if this entity lies on any of the layers to
C                 be shown
                  DO 2000 DI=1,LAYTOT
                     IF ( LAYDAT(DI) .EQ. IMBUFF(4) ) THEN
                         CALL SPCDRW(ENT,TTMIP,.TRUE.,CDT,.TRUE.)
                         IF(DISPV) VISBAS = .TRUE.
                     ENDIF
2000              CONTINUE
 
              ELSE IF ( CMPERS ) THEN
 
                  DO 2010 DI=1,LAYTOT
 
                     IF ( LAYDAT(DI) .EQ. IMBUFF(4) .AND.
     +                  (LAYDAT(DI).NE.CLAYER) ) THEN
                        CALL SPCDRW(ENT,TTMIP,.TRUE.,CDT,.TRUE.)
                        GOTO 2020
                     ENDIF
2010              CONTINUE
2020              CONTINUE
 
              ELSE
 
C                 Bog standard just draw it
C                 Use this call if you want to see what happens
C
C                  CALL CPRMXP('cr:',INPL)
C
                  HWF=.FALSE.
                  IF(IMBUFF(1).GT.128) THEN
                      CALL HFONT(.TRUE.)
                      CALL ROPXOR()
                      HWF = .TRUE.
                  ENDIF
 
                  CALL SPCDRW(ENT,TTMIP,.TRUE.,CDT,.TRUE.)
                  IF(HWF) THEN
                      CALL HFONT(.FALSE.)
                      CALL ROPREP()
                  ENDIF
                  IF(DISPV) VISBAS = .TRUE.
C                 viewport locking for visibilty
                  DO 300 VP = 0,MAXVP
                      IF(VPVIS(VP)) CMPVP(VP)=.TRUE.
300               CONTINUE
 
              ENDIF
 
          ENDIF
 
 
      ELSE
 
C         recover if there is any thing on the stack
 
 
          IF(STKP.EQ.0) THEN
C             nothin on stack go home
              GOTO 200
          ELSE
 
              DO 30 I=1,3
                 DO 30 J=1,3
 30                  CDT(J,I) = STKTFM(J,I,STKP)
 
 
              FINISH=.FALSE.
              RELP = RELMIP(STKP)
 
              ENTPNT = RELSUB(STKP)
              STKP = STKP - 1
 
 
         ENDIF
 
      ENDIF
 
C     go read the next entity whatever it may be
      GOTO 100
 
200   CONTINUE
 
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
      OPFLAG(3)=L3
      OPFLAG(11)=L4
C
C     visibility test
      CALL SETVIS(VISBAS)
      DISPV = VISBAS
C     update any viewports
      IF(.NOT.MVPACT) RETURN
C     get buffers with instance data
      CALL ALLRD(CMPMIP,CMPENT,ME,DELETE)
C     update any viewports
      DO 320 VP = 0,MAXVP
          IF(CMPVP(VP).AND.VPADD.AND.VP.NE.CVPN) THEN
C             Update the display file only for adding only
C             cancel is done elsewhere
              CALL UPDDF(CMPMIP,CMPENT,VP,CMPVP(VP))
          ENDIF
320   CONTINUE
      RETURN
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DRWBUF(TFM)
C     ======================
C
C1    vartype         R(3,3)
C1    iostatus          I
C
C2    Subroutine DRWBUF draws the entities currently referenced
C2    in the SWIND scratch file,transforming each by TFM.
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/swind.inc'
C
      REAL R1,R2,TFM(3,3),M1(3,3),M2(3,3)
      INTEGER*4 I
      INTEGER*2 I21,I22,ENT,TMIP
      LOGICAL OK,L0,L1,L2,TOTAL,DELETE
      EXTERNAL ALLRD,SPCDRW,ALLTFM,I3M,MULT3M
C
C     ensure text scaled proportionaly during display
C     save opflags
      L0=OPFLAG(1)
      L1=OPFLAG(6)
      L2=OPFLAG(7)
C     set opflags to ensure scale done properly
      OPFLAG(1)=.TRUE.
      OPFLAG(6)=.TRUE.
      OPFLAG(7)=.TRUE.
C
C     read the list of entities
      DO 10 I=1,NDATA
         CALL RSCRF(I,TMIP,R1,R2,I21,I22)
         ENDCOD=I22
         IF (TMIP.GT.0) THEN
C           read the entity and draw it in position
            CALL ALLRD(TMIP,ENT,M1,DELETE)
            IF (ENT.EQ.COMPI .OR. ENT.EQ.SYMBI) THEN
C              found an instance
C              concatenate current position transform
C              with instance transform
               CALL MULT3M(M1,TFM,M2)
C              draw the instance
               CALL DRW066(M2)
            ELSE
C              transform and draw the entity
C              transform the entity
               CALL ALLTFM(ENT,TFM)
C              draw the transformed entity
               CALL SPCDRW(ENT,TMIP,.TRUE.,TFM,.TRUE.)
            END IF
         END IF
 10   CONTINUE
C
C     recover opflags
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
C
 20   CONTINUE
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE DRWDIM()
C     ======================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine DRWDIM is a general routine for drawing of
C2    any  dimension type.
C
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc'
      include   'include/dimendat.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/masti.inc'
C
      REAL PDAT(2,2)
      INTEGER*4 I,K
      LOGICAL OK
      EXTERNAL DRWTXT,DRAWLW,AROHED,ARCCT
C
      OK=.FALSE.
C     set local counter for text
      K=0
C     Loop through the stored dimension records.
      DO 101 I=1,RECCNT(1)
C     Check supression state of sub-record entity
         IF ( IWORK(4,I) .GE. 0 ) THEN
C        was not surpressed so continue
            IF ( IWORK(1,I) .EQ. TEXSEG ) THEN
C              was a text record so use text buffer as well
               K=K+1
               CALL DRWTXT(RWORK(1,I),RWORK(2,I),RWORK(3,I),
     +                     RWORK(4,I),RWORK(5,I),RWORK(6,I),DIMCHR(K))
               IF ( DISPV ) OK=.TRUE.
            ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
 
C              was arrowhead so must decode the arrowhead parameters.
               CALL AROHED(RWORK(1,I),RWORK(2,I),PAPTOW*RWORK(5,I),
     +                     PAPTOW*RWORK(6,I),RWORK(3,I),RWORK(4,I),
     1                     PDAT(1,1),PDAT(2,1),PDAT(1,2),PDAT(2,2))
               CALL DRAWLW(PDAT(1,1),PDAT(2,1),PDAT(1,2),PDAT(2,2))
               IF ( DISPV ) OK=.TRUE.
               CALL DRAWLW(RWORK(1,I),RWORK(2,I),PDAT(1,2),PDAT(2,2))
               IF ( DISPV ) OK=.TRUE.
               CALL DRAWLW(RWORK(1,I),RWORK(2,I),PDAT(1,1),PDAT(2,1))
               IF ( DISPV ) OK=.TRUE.
            ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
C              Was a line so use data direct
               CALL DRAWLW(RWORK(1,I),RWORK(2,I),RWORK(4,I),RWORK(5,I))
               IF ( DISPV ) OK=.TRUE.
            ELSE IF ( IWORK(1,I) .EQ. ARCSEG ) THEN
C              Arc found so draw direct.
C              was an arc type segment
               CALL ARCCT(RWORK(1,I),RWORK(2,I),RWORK(4,I),
     +                               RWORK(5,I),RWORK(6,I))
               IF ( DISPV ) OK=.TRUE.
C           ELSE
C              Unrecognised subrecord type.
C              WRITE(10,*)'[DRWDIM] Non Geometric Sub-Record ',IWORK(1,I)
            END IF
         END IF
 101  CONTINUE
C     set visibility flag
      CALL SETVIS(OK)
C     end of draw routine.
 
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DRWHT(TMIP,USET,M)
C     =============================
C
C1    vartype           I*2  L  R(3,3)
C1    iostatus           I   I    I
C
C2    Subroutine DRWHT draws the HATCH entity
C2    pointed to by TMIP. If the logical USET is
C2    true,then the hatch data will be transformed
C2    using the transform M.
C2    The form and font of the hatch pattern are
C2    implied from the FONT and FORM values
C2    within the MI record for the entity.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
C
      REAL M(3,3),THDIST,L1,L2,L3,DIST,VD0D13,TBUFF(6),DISTXY
      INTEGER*4 I4,I,TOTAL
      INTEGER*2 POINT,TMIP,TFONT,TPDFP
      LOGICAL OK,USET
C
      EXTERNAL DER500, CHKBOX,DBR500,DRWFLW,VD0D13
C
      THDIST = HDIST
      I4 = 1
      CALL DER500(TMIP,OK)
C     transform hatchbox to current instance position
C     if transform to be used
      IF ( USET ) CALL MV0003(RDBUFF,M)
C     DRAW BOUNDING BOX
CD      CALL HFONT(.TRUE.)
CD      CALL DRAWLW(RDBUFF(1),RDBUFF(2),RDBUFF(1),RDBUFF(5))
CD      CALL DRAWLW(RDBUFF(1),RDBUFF(5),RDBUFF(4),RDBUFF(5))
CD      CALL DRAWLW(RDBUFF(4),RDBUFF(5),RDBUFF(4),RDBUFF(2))
CD      CALL DRAWLW(RDBUFF(4),RDBUFF(2),RDBUFF(1),RDBUFF(2))
CD      CALL HFONT(.FALSE.)
C
C     test for visibility of complete hatch
      CALL CHKBOX(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),TOTAL,DISPV)
      IF ( .NOT. DISPV ) RETURN
C     get part data pointer
      TPDFP = IDBUFF(3)
      IF ( TPDFP .EQ. NPDPOS.OR.TPDFP .EQ. 0) THEN
          DISPV = .FALSE.
          RETURN
      ENDIF
C     are there any hatch lines stored
      CALL DBR500(TPDFP,OK)
C     get first hatch line as a vector
      CALL CV0L14(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     +               L1,L2,L3)
      TPDFP=IDBUFF(3)
C
      IF(TPDFP.GT.0) THEN
C        Read the next hatch line.
         CALL DBR500(TPDFP,OK)
C        get Pitch between lines (Paper Units)
         DIST=VD0D13(L1,L2,L3,RDBUFF(1),RDBUFF(2))/PAPTOW
         IF (USET) THEN
C           transform in use, must
C           rework hatch spacing for vector test
C           setup distance vector along X axis
            TBUFF(1)=0.0
            TBUFF(2)=0.0
            TBUFF(3)=0.0
            TBUFF(4)=DIST
            TBUFF(5)=0.0
            TBUFF(6)=0.0
C           transform the distance vector
            CALL MV0003(TBUFF,M)
C           get new length
            DIST=DISTXY(TBUFF(1),TBUFF(2),TBUFF(4),TBUFF(5))
         END IF
C        store current hatch pitch
         HDIST = DIST
      ENDIF
C     re-read to reset to start of hatch
      CALL DER500(TMIP,OK)
      I=0
C     loop through and draw all hatch lines
 33   CONTINUE
      I=I+1
      POINT=IDBUFF(3)
      IF(POINT.EQ.0) GOTO 50
      CALL DBR500(POINT,OK)
C     ensure fonted lines are shown properly by transforming line
      IF ( USET ) CALL MV0003(RDBUFF,M)
C     go draw the line using current parameters
      CALL DRWHLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),I,IMBUFF(5))
C     end of loop when no continuation pointer
      IF (IDBUFF(3).NE.0) GOTO 33
C
 50   CONTINUE
C     restore global hatching distance
      HDIST = THDIST
C     reset visibility flag to default
      DISPV=.TRUE.
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DRWMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     ===================================================
C1    VARTYPE           R  R    R     R     R    I*2  I*2
C1    IOSTAT            I  I    I     I     I     I    I
C
C2    This routine is used to draw a marker entity at the
c2    point X1,Y1 at orientation ANGLE with scale in x
C2    SCALX and scale in y SCALY.  The form refers to the
C2    marker index number into the preloaded marker table
c
C
      include 'include/marker.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include  'include/wtov.inc'
      REAL X1,Y1,ANGLE,SCALX,SCALY,STX,STY,A(3,3),B(3,3),C(3,3)
      REAL NSX1,NSY1,NFX1,NFY1,BX1,BX2,BX3,BX4,BY1,BY2,BY3,BY4
      REAL BLX,BLY,TRX,TRY,D(3,3)
      INTEGER*2 FORM,FONT,MIND,I
      INTEGER*4 TOTAL,FONT4, ENTCOL
      LOGICAL NEWBLK
C
      MIND=MRKIND(FORM)
C
      IF(MIND.EQ.0) THEN
C         look if there is a marker at no. 1
          MIND=MRKIND(1)
          IF ( MIND.EQ.0) THEN
              CALL EPRINT('No marker definitions')
              RETURN
          ENDIF
      END IF
C 
C     scale the point arround the world origin
      CALL SCAL2D(SCALX,SCALY,A)
C     rotate the point arround the world origin
      CALL ROT2D(ANGLE,B)
C     Concatenate result
      CALL MULT3M(A,B,C)
c     translate the point arround the world origin
      CALL TRAN2D(X1,Y1,B)
C     Concatenate result
      CALL MULT3M(C,B,A)
 
      CALL MULT3M(A,WVXY,C)
C     let us transform the bounding box
      CALL NEWXY(MRKR(MIND,1),MRKR(MIND,2),BX1,BY1,A)
      CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND+1,2),BX2,BY2,A)
      CALL NEWXY(MRKR(MIND,1),MRKR(MIND+1,2),BX3,BY3,A)
      CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND,2),BX4,BY4,A)
C     This is a check to see if it is worth drawing the
C     marker or not by checking its surounding box
C
      CALL CHKBOX(MIN(BX1,BX2,BX3,BX4),MIN(BY1,BY2,BY3,BY4),
     +           MAX(BX1,BX2,BX3,BX4),MAX(BY1,BY2,BY3,BY4),
     +           TOTAL,DISPV)
 
      IF(.NOT.DISPV) THEN
        RETURN
      END IF
      NEWBLK = .FALSE.
      STX=MRKR(MIND+2,1)
      STY=MRKR(MIND+2,2)
         IF(FONT.EQ.1.AND.TOTAL.EQ.0.AND.IMBUFF(12).EQ.0) THEN
               IF(.NOT.NEWBLK) THEN
                  CALL NEWXY(MRKR(MIND+2,1),MRKR(MIND+2,2),NSX1,NSY1,C)
               END IF  
               I =  MIND+3
  10           CONTINUE
                  IF(.NOT.NEWBLK) THEN
                     CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,C)
                  END IF
                  IF(MRKI(I).EQ.1) THEN
                     CALL DRAWLS(NSX1,NSY1,NFX1,NFY1)
C            WRITE(10,*) '[DRWMRK] QUICK  ',NSX1,NSY1,NFX1,NFY1,FONT
                  END IF
                  IF (MRKI(I).EQ.99) THEN
                     NEWBLK = .FALSE.
                     IF (MRKR(I,1) .GT. 0.0 ) THEN
                         IMBUFF(12) = INT(MRKR(I,1))
                         NEWBLK = .TRUE.
                     ELSE IF (MRKR(I,2) .GT. 1.0 ) THEN
                         FONT4 = INT(MRKR(I,2))
                         NEWBLK = .TRUE.
                     END IF
                     I = I +1
                     IF (MRKR(I,1) .GT.0.0) THEN
                         ENTCOL=INT(MRKR(I,1))
                         CALL SETAPN(ENTCOL)
                     END IF
                     IF(NEWBLK.AND.I.GT.1) THEN
                       CALL NEWXY(MRKR(I-1,1),MRKR(I-1,2),NFX1,NFY1,A)
                     ELSE
                       CALL NEWXY(MRKR(I-1,1),MRKR(I-1,2),NFX1,NFY1,C)
                     END IF
                  ELSE IF (NEWBLK) THEN
                     CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,A)
                     IF (MRKI(I).EQ.1) THEN
                        CALL DRWFLW(NSX1,NSY1,NFX1,NFY1,FONT4)
                     END IF
                  END IF
                  NSX1=NFX1
                  NSY1=NFY1
                  I = I + 1
               IF(I.LT.MIND+MRKI(MIND)) GOTO 10
         ELSE
            CALL NEWXY(MRKR(MIND+2,1),MRKR(MIND+2,2),NSX1,NSY1,A)
            I = MIND+3
  20        CONTINUE 
               IF (MRKI(I) .EQ. 99) THEN 
                   IF (MRKR(I,1) .GT. 0.0 ) THEN
                      IMBUFF(12) = INT(MRKR(I,1))
                   ELSE IF (MRKR(I,2) .GT. 1.0 ) THEN
                       FONT4 = INT(MRKR(I,2))
                   END IF
                   I = I + 1
                   IF (MRKR(I,1) .GT.0.0) THEN
                       ENTCOL=INT(MRKR(I,1))
                       CALL SETAPN(ENTCOL)
                   END IF
               END IF
               CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,A)
               IF(MRKI(I).EQ.1) THEN
                  CALL DRWFLW(NSX1,NSY1,NFX1,NFY1,FONT4)
C                 WRITE(10,*) '[DRWMRK] ',NSX1,NSY1,NFX1,NFY1
               END IF
               NSX1=NFX1
               NSY1=NFY1
            I = I + 1
            IF ( I .LE.MIND+MRKI(MIND)) GOTO 20
         END IF
      DISPV=.TRUE.
      END
 
 
      SUBROUTINE DRWPRF()
C     ===================
C1    NO ARGS
C
C2    This routine will draw a profile on the current contents of the
C2    scratch file
C
      include    'include/hdata.inc'
      LOGICAL OP
C
      INTEGER*2 I1
      IF (NO.GT.0) THEN
C         If the file is opened the draw the profile
          INQUIRE(UNIT=HUNIT,OPENED=OP)
          IF(OP) THEN
              DO 100 I1 =1,NO
                  CALL PRODRW(I1)
 100          CONTINUE
          ENDIF
      ENDIF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE DRWSPL(TMIP,USET,M,DRWTYP)
C     =====================================
C
C1                      I*2   L R(3,3) I*4
C1                       I    I   I     I
C2      Subroutine DRWSPL  draws a spline
C2      If  DRWTYP =  0 ... to the screen.
C2                   -1 ... to the plot file.
C2                   >0 ... to the "write" file.
C2      If DRWTYP>0, the absolute of DRWTYP is the file unit number.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/drwspl.inc'
C
      REAL M(3,3),XP,YP,TW
      INTEGER*4 I,DRWTYP
      INTEGER*2 POINT,TMIP,TYPE,MODE,POINT1
      LOGICAL OK,USET,SP
C
      EXTERNAL DER500,CLIP,DBR500,DRWFLW
C
      FRSTPT = .TRUE.
C
      CALL DER500(TMIP,OK)
C
      MODE=MOD(IMBUFF(5)+0,4)
      TYPE=IMBUFF(5)-MODE
C     special draw
      SP=.FALSE.
      IF(INT(TYPE/128).GT.0) THEN
C         Curve is flagged. Correct the entity type.
          TYPE=TYPE-128
C         If we are drawing, we need a special font.
          IF (DRWTYP.EQ.0) THEN
             SP=.TRUE.
          ENDIF
      ENDIF
C     draw bounding box of spline
C      CALL DBR500(IDBUFF(3),OK)
C      CALL HFONT(.TRUE.)
C      CALL DRAWLT(RDBUFF(1),RDBUFF(2),RDBUFF(1),RDBUFF(5))
C      CALL DRAWLT(RDBUFF(1),RDBUFF(5),RDBUFF(4),RDBUFF(5))
C      CALL DRAWLT(RDBUFF(4),RDBUFF(5),RDBUFF(4),RDBUFF(2))
C      CALL DRAWLT(RDBUFF(4),RDBUFF(2),RDBUFF(1),RDBUFF(2))
C      CALL HFONT(.FALSE.)
C      CALL DER500(TMIP,OK)
C     load first two we know they are there or else
C     this is a funny spline
 
      GOTO ( 10,20,30,40 ) MODE+1
 10   CONTINUE
C        ****************************************
C        ***      POINTS STRING               ***
C        ****************************************
         POINT=IDBUFF(3)
C        Read the first two records cos they contain
C        the end constraints.
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         IF(POINT.EQ.0) GOTO 50
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         CALL DPTS(POINT,USET,M,OK)
         GOTO 50
 20   CONTINUE
C        ****************************************
C        ***    POLY-LINE STRING              ***
C        ****************************************
         POINT=IDBUFF(3)
C        Read the past first two records cos they contain
C        the end constraints
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         POINT1=POINT
         IF(POINT.EQ.0) GOTO 50
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
C        Set the continuation pointer
         CALL DPOLY(POINT,USET,M,DRWTYP,OK)
         IF(SP)  THEN
             CALL DBR500(POINT1,OK)
             POINT=IDBUFF(3)
             CALL DPTS(POINT,USET,M,OK)
         ENDIF
         GOTO 50
C
 30   CONTINUE
C
      GOTO (31,32,33,34,35,36 ) TYPE/4
C
 31   CONTINUE
C        Points only mode not there yet oops !
         GOTO 10
 32   CONTINUE
C        Polynomial fit not there yet oops !
         GOTO 50
 33   CONTINUE
C        Quadratic fit not there yet oops !
         GOTO 50
 34   CONTINUE
C        ****************************************
C        ***    HERMITE CURVE                ***
C        ****************************************
         POINT=IDBUFF(3)
C        Read the first two records cos they contain
C        the end constraints
         TW=RDBUFF(6)
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         POINT1=POINT
         IF(POINT.EQ.0) GOTO 50
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
C        Set the continuation pointer
         CALL POLY16(POINT,TW,USET,M,DRWTYP,OK)
         IF(SP) THEN
              CALL DBR500(POINT1,OK)
              POINT=IDBUFF(3)
              CALL DPTS(POINT,USET,M,OK)
              IF(MODE.EQ.2) THEN
                  CALL DBR500(POINT1,OK)
                  POINT=IDBUFF(3)
                  CALL DPOLY(POINT,USET,M,DRWTYP,OK)
              ENDIF
         ENDIF
         GOTO 50
 35   CONTINUE
C        Bezier  not there yet oops !
         GOTO 50
 36   CONTINUE
 
C        ****************************************
C        ***    B-SPLINE CURVE                ***
C        ****************************************
C        B-Spline
         POINT=IDBUFF(3)
C        Read the first two records cos they contain
C        the end constraints
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         POINT1=POINT
         IF(POINT.EQ.0) GOTO 50
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
C        Set the continuation pointer
         CALL POLY24(POINT,USET,M,DRWTYP,OK)
         IF(SP) THEN
              CALL DBR500(POINT1,OK)
              POINT=IDBUFF(3)
              CALL DPTS(POINT,USET,M,OK)
              IF(MODE.EQ.2) THEN
                  CALL DBR500(POINT1,OK)
                  POINT=IDBUFF(3)
                  CALL DPOLY(POINT,USET,M,DRWTYP,OK)
              ENDIF
         ENDIF
         GOTO 50
C
 40   CONTINUE
         POINT=IDBUFF(3)
C        Read the first two records cos they contain
C        the end constraints
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
C        Set the continuation pointer
         CALL DPOLY(POINT,USET,M,DRWTYP,OK)
C        Reread the master index header to find
C        the continuation pointer for data.
         CALL DER500(TMIP,OK)
         GOTO 30
 50   CONTINUE
      IF(SP) TYPE=TYPE+256
C
      END
C
C
C-------------------------------------------------------------
C
      SUBROUTINE DRWTBX(X1,X2,X3,X4,X5,X6)
C     ====================================
C1                       R, R, R, R, R, R
C1                       I, I, I, I, I, I
C
C2    Subroutine DRWTBX draws a box surrounding a
C2    text item described by the dtat record passed
C2    in X1-X6.
C
C
      include 'include/nbuff.inc'
C
      REAL X(4),Y(4),X1,X2,X3,X4,X5,X6,SLA,SCA
      INTEGER*2 NCHAR,JST,I
      EXTERNAL UCODET,TORGTL,TORGTR,TORGBR,TXTORG,DRAWLW
C
C     decode the text params
      CALL UCODET(X6,SLA,JST,NCHAR)
C     find the limits of the text
      DO 10 I=1,4
        CALL TXTORG(I,X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(I),Y(I))
 10   CONTINUE
C     draw the bounding box around the text
      DO 20 I=1,4
        JST=MOD(I+0,4)+1
 20     CALL DRAWLW(X(I),Y(I),X(JST),Y(JST))
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE DRWTXT(X,Y,TW,TH,TANG,Z6,TEXT)
C     ===========================================
C1                      R,R, R, R,  R,   R, C*(*)
C1                      I,I, I, I,  I,   I,  I
C     READ FONT FILE AND OUTPUT TO SCREEN
C
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
      include 'include/vntable.inc'
C
      CHARACTER LET*1,TEXT*(*),TEXT1*120
      REAL X1,Y1,X,Y,XT,YT,TEMPX,TEMPY,TW,TH,RAD,Q,R,TMPH,TMPW,
     +     SLA,TAGL,TEXTA(3,3),C1,C2,C3,XO,YO,TANG,Z6,STH
      INTEGER*2 T(2),I,J1,J2,I1(2),JST,NCHAR,CRC
      INTEGER*4 I4,K,C,IP,I3,LC,TOTAL
      INTRINSIC ICHAR,COS,SIN,TAN,CHAR,REAL
      EQUIVALENCE  (IP,T)
C
      EXTERNAL TEXBOX,CHKBOX,UCODET,WTOSVY,DRWTBX,XYNORG,RAD,ROTP2D,
     +         NEWXY,DRAWLW
C
C     use temporary storage for text height,width in world coords
      TMPH=TH*PAPTOW
      TMPW=TW*PAPTOW
      CALL  TEXBOX(X,Y,TMPW,TMPH,TANG,Z6,X1,Y1,TEMPX,TEMPY)
      CALL CHKBOX(X1,Y1,TEMPX,TEMPY,TOTAL,DISPV)
      IF ( .NOT. DISPV ) RETURN
      CALL UCODET(Z6,SLA,JST,NCHAR)
      CALL WTOSVY(TMPH,STH)
      IF ( STH .LT. 6.0 ) THEN
C        draw a box where text should be
C        print*, 'TEXT BOX ONLY'
         CALL DRWTBX(X,Y,TMPW,TMPH,TANG,Z6)
         RETURN
      END IF
 222  CONTINUE
      TEXT1=TEXT
C     This is for CR inbedded in files by CV
      CRC=INDEX(TEXT1,CHAR(13))
C     avoid negative length of string if CR first char
      IF ( CRC .GT. 1 ) THEN
         TEXT=TEXT1(:CRC-1)
         NCHAR=CRC
      END IF
C     Find the bottom left corner of the text
      I=1
      CALL  TXTORG(I,X,Y,JST,TANG,SLA,NCHAR,TMPW,TMPH,XO,YO)
C3    All Characters are defined on a 30x30 square
      C1=TMPH/30.0
C3    Slant angle is stored in Degrees therefore
C3    have to convert
      C2=TAN(RAD(SLA))*C1
      C3=TMPW/30.0
C3    TANG is the angle at which the text is tobe written
C     This is in degrees have to convert to radians
      TAGL=RAD(TANG)
      IF (NCHAR.GT.80) NCHAR=80
      DO 10  K=1,NCHAR
         LET=TEXT(K:K)
         CALL ROTP2D(XO,YO,TAGL,TEXTA)
C3       If the letter is greater than ASCII 32 then the
C3       character is a valid character
         LC = MOD(ICHAR(LET)+256,256)
         IF (  LC .GT. 32 ) THEN
C            print*, 'TEXT VALUE',LET
C3          Take the ASCII value of the character then offset
C3          this by one
            I4=LC + 1
C3          IP has the pointer value to where the character definition
C3          is stored
            IP=FONT1(I4)
C            READ (UNIT=20,REC=I4) T(1),T(2)
            I4=T(2)

C            print*,'FONT DETAILS',IP,I4
            IF (I4.GT.0) THEN
C3            IP now has the ASCII number and the length of the
C3            definition using the equivalence with the variable T
C3               T(1)   contains the ASCII number
C3               T(2)     "       "  length of the definition
               IP=FONT1(I4)
C                      print*,'FONT DETAILS 2',IP,I4
C               READ(UNIT=20,REC=I4) T(1),T(2)
               DO 20 I3=1,T(2)
C                  Read definition from Font file
                  IP=FONT1(I4+I3)
C                  print*,'FONT DETAILS 2',IP,I4
C                 READ(UNIT=20,REC=I4+I3) T(1),T(2)
C                 If X coordinate exceeds 100 then a line has to
                  IF ( T(1) .GE. 100 ) THEN
                     TEMPX=XO+ C2*REAL(T(2)) + C3*(REAL(T(1))-100)
                     TEMPY=YO+ C1*REAL(T(2))
                     CALL NEWXY(TEMPX,TEMPY,X1,Y1,TEXTA)
                     CALL DRAWLT(XT,YT,X1,Y1)
                  ELSE
                     TEMPX=XO+ C2*REAL(T(2)) + C3*REAL(T(1))
                     TEMPY=YO+ C1*REAL(T(2))
                     CALL NEWXY(TEMPX,TEMPY,X1,Y1,TEXTA)
                  END IF
                  XT=X1
                  YT=Y1
 20            CONTINUE
            ELSE
               WRITE(10,*) 'Illegal character at',K,'value',LC
               WRITE(10,*) 'IMBUFF',(IMBUFF(I),I=1,13)
               WRITE(10,*) 'IDBUFF',(IDBUFF(I),I=1,4)
               WRITE(10,*) 'CBUFF',TEXT
            END IF
         END IF
C
         XO=XO+TMPW*COS(TAGL)
         YO=YO+TMPW*SIN(TAGL)
 10   CONTINUE
C
      IF ( CRC .NE. 0 ) THEN
         Y=Y-1.5*COS(TAGL)*TH
         X=X+1.5*SIN(TAGL)*TH
         TEXT=TEXT1(CRC+1:)
         GOTO 222
      END IF
 
      DISPV=.TRUE.
C
      END
C
C---------------------------------------------------------------------
C
      FUNCTION DSTOP(KEYVAL,PROMPT)
C     =============================
C1    VARTYPE          I4    I4
C1    IOSTAT           I     I
C
C2    This function is used to stop a draw
C2    call it when you want to get a result from
C2    the key 'E'
C
      LOGICAL OK,DSTOP
      INTEGER*4 D,KVAL,PROMPT
      CHARACTER KEY,KEYVAL
C
C
      DSTOP=.FALSE.
      KEY=CHAR(0)
C     get the value
      CALL KBINTR(KEY,OK)
      CALL FOLDUP(KEY)
C      write(10,*) '[dstop] key =',ichar(key),' ok= ',ok
      IF(KEY.EQ.KEYVAL.AND.OK) THEN
C         program aborted
          D=PROMPT
          CALL DEPRNT (D)
          DSTOP=.TRUE.
      ENDIF
      END
C
C---------------------------------------------------------------------
C
 
      SUBROUTINE EGRID()
C     ==================
C
      include 'include/ndata.inc'
C1    noarguments required
C
      EXTERNAL PENERS,PENDRW,GRID1
C
C     select erase colour
      CALL PENERS()
      ISGRID=.TRUE.
      CALL GRID1()
      ISGRID=.FALSE.
C     select draw colour
      CALL PENDRW()
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE ERS066(M)
C     ========================
C
C1    vartype          R(3,3) L
C1    iostatus            I   O
C
C2    Subroutine ERS066 erases the component instance
C2    based on the data currently stored in the
C2    array RWORK.
C2    The flag OK is returned true if successful.
C
      REAL M(3,3)
C
      EXTERNAL DRW066,PENERS,PENDRW
C
      CALL PENERS()
      CALL DRW066(M)
      CALL PENDRW()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSDIM()
C     ====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine ERSDIM erases  any  dimension
C2    based on the data currently stored in the
C2    array RWORK.
C2    The flag OK is returned true if successful.
C
C
      EXTERNAL DRWDIM,PENERS,PENDRW
C
      CALL PENERS()
      CALL DRWDIM()
      CALL PENDRW()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSHT(TMIP)
C     ======================
C
C1                      I*2
C1                      I
C2      Subroutine ERSHT erases a hatch
      INTEGER*2 TMIP
      REAL M(3,3)
      EXTERNAL DRWHT,PENERS,PENDRW
      CALL PENERS()
      CALL DRWHT(TMIP,.FALSE.,M)
      CALL PENDRW()
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
C     ===================================================
C1    VARTYPE           R  R    R     R     R    I*2  I*2
C1    IOSTAT            I  I    I     I     I     I    I
C
C2    This routine is used to erase a marker entity at the
c2    point X1,Y1 at orientation ANGLE with scale in x
C2    SCALX and scale in y SCALY.  The form refers to the
C2    marker index number into the preloaded marker table
c
      REAL X1,Y1,ANGLE,SCALX,SCALY
      INTEGER*2 FORM,FONT
      EXTERNAL DRWHT,PENERS,PENDRW
      CALL PENERS()
      CALL DRWMRK(X1,Y1,ANGLE,SCALX,SCALY,FORM,FONT)
      CALL PENDRW()
      END
 
      SUBROUTINE ERSPRF()
C     ===================
C1    NO ARGS
C
C2    This routine will draw a profile on the current contents of the
C2    scratch file
C
      include    'include/hdata.inc'
      LOGICAL OP
C
      INTEGER*2  I1
      IF (NO.GT.0) THEN
C         If the file is opened the draw the profile
          INQUIRE(UNIT=HUNIT,OPENED=OP)
          IF(OP) THEN
              DO 100 I1 =1,NO
C                 send it a negative argument for erase
                  CALL PRODRW(-I1)
 100          CONTINUE
          ENDIF
      ENDIF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE ERSSPL(TMIP,USET,M)
C     ======================
C
C1                      I*2
C1                      I
C2      Subroutine ERSHT erases a hatch
      INTEGER*2 TMIP
      INTEGER*4 DRWTYP
      LOGICAL USET
      REAL M(3,3)
      EXTERNAL DRWSPL,PENERS,PENDRW
      DRWTYP = 0
      CALL PENERS()
      CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
      CALL PENDRW()
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ERSTXT(X1,X2,X3,X4,X5,X6,CBUFF)
C     ==========================================
C
C1                      R,R,   R,   R,   R
C1                      I,I,   I,   I,   I
C2      Subroutine EARCCT erases an arc
C2      centre       X,Y
C2      radius       WRAD
C2      start angle  SANG
C2      end   angle  EANG
C
      REAL X1,X2,X3,X4,X5,X6
      CHARACTER*(*) CBUFF
      EXTERNAL DRWTXT,PENERS,PENDRW
C
      CALL PENERS()
      CALL DRWTXT(X1,X2,X3,X4,X5,X6,CBUFF)
      CALL PENDRW()
C
      END
C
C-------------------------------------------------------------
C

