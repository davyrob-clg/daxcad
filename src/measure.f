C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 measure.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE APERM(RAD,SANG,EANG,TPERM)
C     SUBROUTINE ARCCAL(X,Y,WRAD,SANG,EANG,
C     SUBROUTINE AREA(X1,Y1,X2,Y2,TAREA)
C     SUBROUTINE CENT(X1,Y1,X2,Y2,GX,GY,I2MX,I2MY,IXYX)
C     SUBROUTINE DI3M(A)
C     SUBROUTINE DMULT3( A , B , C )
C     SUBROUTINE DROT2D(THETA,A)
C     SUBROUTINE DROTP2(X,Y,THETA,A)
C     SUBROUTINE DTRAN2(TX,TY,A)
C     SUBROUTINE MAJME1()
C     SUBROUTINE MEAS1()
C     SUBROUTINE MEAS2()
C     SUBROUTINE MEASA1()
C     SUBROUTINE MEASA2()
C     SUBROUTINE MEASA3(FUNIT,N)
C     SUBROUTINE MEASP1()
C     SUBROUTINE MEASP2()
C     SUBROUTINE MESAG1()
C     SUBROUTINE MESAG2()
C     SUBROUTINE MESE00()
C     SUBROUTINE MESME2()
C     SUBROUTINE MESP00()
C     SUBROUTINE MESP01()
C     SUBROUTINE MNIARE()
C     SUBROUTINE MNIME1()
C     SUBROUTINE MNIME2()
C     SUBROUTINE MNIMES()
C     SUBROUTINE MNLAG3()
C     SUBROUTINE MNMEAS()
C     SUBROUTINE PERIM(X1,Y1,X2,Y2,TPERM)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE APERM(RAD,SANG,EANG,TPERM)
C     =====================================
      DOUBLE PRECISION ANG,RAD,SANG,EANG,TPERM,DPI
      EXTERNAL DPI
C
      ANG=ABS(EANG-SANG)
      IF ( EANG .LT. SANG ) ANG=DPI(2.0D0)-ANG
      TPERM=TPERM+ABS(RAD)*ANG
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE ARCCAL(X,Y,WRAD,SANG,EANG,
     +                  TAREA,GX,GY,I2MX,I2MY,IXYX, START)
C     =============================================
C2
C2    START = TRUE if start of the arc is the first point
C2    in this part of the profile
C2    START = FALSE if end of the arc is the first point
C2    in this part of the profile
 
      INTEGER*2 II2
      INTEGER COUNT,I,MAX,NUM
      CHARACTER*3 ANS
      DOUBLE PRECISION TAREA,GX,GY,I2MX,I2MY,IXYX
      DOUBLE PRECISION  RADS,X,Y,WRAD,NX,NY,TRS(3,3),OX,OY,PIXELS,
     +                  SRAD,DPI,SANG,EANG,ANGLE
      LOGICAL RESPON, START
      EXTERNAL DPI,RESPON
 
 
      PIXELS=0.04
C     Calculate the angle in radians which will give a
C     smooth curve for the size of radius
      RADS=PIXELS/SQRT(ABS(WRAD))
C
C     find included angle
      ANGLE=ABS(EANG-SANG)
      IF ( EANG .LT. SANG ) ANGLE=DPI(2.0D0)-ANGLE
C
C     calculate nearest whole number of intervals needed
      COUNT=1+(ANGLE/RADS)
C
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc
      RADS=DBLE(ANGLE/DBLE(COUNT))
C
C     Obtain the transformation matrice which rotates about
C     the centre of the circle X,Y by the angle RADS


      IF ( ABS(EANG-SANG).GT.DPI(2.0D0)-.01) THEN
C        well it's a complete circle
         II2=2
         CALL HARDWF(II2)
         CALL ROPXOR()
         CALL ARCCT(REAL(X),REAL(Y),REAL(WRAD),
     +                  REAL(SANG),REAL(EANG))
C        reset now !!!!!!!!
         II2=0
         CALL HARDWF(II2)
         CALL ROPREP()
         CALL DPRMXP(441,ANS)
         RADS=ABS(RADS)
         NUM = 12
C        is it a hole ?
         IF( RESPON(ANS,NUM)) THEN
             RADS = -RADS
         END IF
C        draw the arc back
         CALL ARCCT(REAL(X),REAL(Y),REAL(WRAD),
     +                  REAL(SANG),REAL(EANG))
C        starting point of the drawing of the circle
         OX=X+ABS(WRAD)*COS(SANG)
         OY=Y+ABS(WRAD)*SIN(SANG)
      ELSE
C        well it's not a complete circle         

         IF(START) THEN
C          starting point of the drawing of the arc
C          if the profile continues at the start of the arc
           OX=X+ABS(WRAD)*COS(SANG)
           OY=Y+ABS(WRAD)*SIN(SANG)
           RADS = ABS(RADS)
         ELSE
C          starting point of the drawing of the arc
C          if the profile continues at the end of the arc
           OX=X+ABS(WRAD)*COS(EANG)
           OY=Y+ABS(WRAD)*SIN(EANG)
           RADS = -ABS(RADS)
         ENDIF  
      END IF
C
      CALL DROTP2(X,Y,RADS,TRS)
C
C
C     loop for the whole number calculated above
      DO 10 I=1,COUNT,1
C
C        transform OX to NX by transformation matrice
C        and also OY to NY
         CALL DNEWXY(OX,OY,NX,NY,TRS)
C        Draw between old point and new
         CALL AREA(OX,OY,NX,NY,TAREA)
         CALL CENT(OX,OY,NX,NY,GX,GY,I2MX,I2MY,IXYX)
C        old now new point on the circumference
         OX=NX
         OY=NY
 10   CONTINUE
C     Arc exists
 
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE AREA(X1,Y1,X2,Y2,TAREA)
C     ==================================

      include 'include/area.inc'

      DOUBLE PRECISION  X1,Y1,X2,Y2,TAREA
      TAREA=TAREA+(X1-X2)*(Y2+Y1)/2.0    

C     
      IF (X1 .LT. YXMIN) THEN
        YXMIN = X1
      ELSEIF (X1 .GT. YXMAX) THEN
        YXMAX = X1
      ENDIF

      IF (X2 .LT. YXMIN) THEN
        YXMIN = X2
      ELSEIF (X2 .GT. YXMAX) THEN
        YXMAX = X2
      ENDIF

      IF (Y1 .LT. YYMIN) THEN
        YYMIN = Y1
      ELSEIF (Y1 .GT. YYMAX) THEN
        YYMAX = Y1
      ENDIF

      IF (Y2 .LT. YYMIN) THEN
        YYMIN = Y2
      ELSEIF (Y2 .GT. YYMAX) THEN
        YYMAX = Y2
      ENDIF

      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE CENT(X1,Y1,X2,Y2,GX,GY,I2MX,I2MY,IXYX)
C     ==================================
 
      DOUBLE PRECISION X1,Y1,X2,Y2,GX,GY,I2MX,I2MY,IXYX
      DOUBLE PRECISION ASQR1,ATRI1,CNSQR1,CNTRI1,ASQR2,ATRI2,
     +                 CNSQR2,CNTRI2
 
C      WRITE(10,*) 'ME CENT X1,Y1,X2,Y2:',X1,Y1,X2,Y2
      ASQR1=(Y2-Y1)*X2
      CNSQR1=X2/2.0
      ATRI1=(X1-X2)*(Y2-Y1)/2.0
      CNTRI1=(X2+(X1-X2)/3.0)
      GX=GX+(ASQR1*CNSQR1+ATRI1*CNTRI1)
 
      ASQR2=(X1-X2)*Y1
      CNSQR2=Y1/2.0
      ATRI2=(X1-X2)*(Y2-Y1)/2.0
      CNTRI2=(Y1+(Y2-Y1)/3.0)
      GY=GY+(ASQR2*CNSQR2+ATRI2*CNTRI2)
 
      I2MX=I2MX+(1.0/3.0*ASQR2*Y1**2)+
     +    ATRI2*(1.0/18.0*(Y2-Y1)**2+CNTRI2**2)
      I2MY=I2MY+(1.0/3.0*ASQR1*X2**2)+
     +    ATRI1*(1.0/18.0*(X1-X2)**2+CNTRI1**2)
 
      IXYX=IXYX+(ASQR2*CNSQR2*((X1+X2)/2.0))
     +         +(-1.0/18.0*ATRI2**2+ATRI2*CNTRI1*CNTRI2)
 
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DI3M(A)
C     =================
C1                 R(3*3)
C1                   O
C2      Subroutine I3M set matrice A(3*3) to the
C2      identity matrice I(3*3)
C
      DOUBLE PRECISION A(3,3),I(3,3)
      INTEGER K,J
C
C       Set I to a 3 by 3 indentity matrice
      DATA I/1.0,0.0,0.0,
     A       0.0,1.0,0.0,
     B       0.0,0.0,1.0/
C
      DO 10 J=1,3
         DO 10 K=1,3
            A(J,K)=I(J,K)
 10   CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DMULT3( A , B , C )
C     ==============================
C1                        3*R(3*3)
C1                       I , I , O
C2       Subroutine MULT3M multiplies matrices A and B
C2       and returns the values in matrice C
C
      DOUBLE PRECISION A(3,3),B(3,3),C(3,3)
      INTEGER I,J,K
      DO 1 I=1,3
         DO 2 J=1,3
            C(I,J)=0.0
            DO 3 K=1,3
               C(I,J)=C(I,J)+A(I,K)*B(K,J)
 3          CONTINUE
 2       CONTINUE
 1    CONTINUE
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DROT2D(THETA,A)
C     =========================
C1                       R ,R(3*3)
C1                       I , O
C2       Subroutine ROT2D rotates a point about the origin
C2       by the theta (radians) returning the transformation
C2       matrice A
C
      DOUBLE PRECISION A(3,3),THETA
      INTRINSIC DSIN,DCOS
      EXTERNAL I3M
C       set to identity matrice
      CALL DI3M(A)
C       set the rotation sin,cos
      A(1,1)=DCOS(THETA)
      A(2,2)=A(1,1)
      A(1,2)=DSIN(THETA)
      A(2,1)=-A(1,2)
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DROTP2(X,Y,THETA,A)
C     ==============================
C1                      R,R,  R  ,R(3*3)
C1                      I,I,  I  ,O
C2       Subroutine ROTP2D sets A the 3*3 matrice to rotate
C2       a point by the value of theta
C
      DOUBLE PRECISION X,Y,THETA,A(3,3),B(3,3),C(3,3)
      EXTERNAL TRAN2D,ROT2D,MULT3M
C        translates back to origin 0,0
      CALL DTRAN2(-X,-Y,A)
C        rotate about 0,0
      CALL DROT2D(THETA,B)
      CALL DMULT3(A,B,C)
C        translate back to original position X,Y
      CALL DTRAN2(X,Y,B)
      CALL DMULT3(C,B,A)
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE DTRAN2(TX,TY,A)
C     ==========================
C1                       R, R,R(3,3)
C1                       I, I,O
C
C2       Subroutine TRAN2D sets array A to translate a
C2       point by the values of TX,TY
C
      DOUBLE PRECISION A(3,3),TX,TY
      EXTERNAL I3M
C
C       set to indentity matrice
      CALL DI3M(A)
C        set translation values
      A(3,1)=TX
      A(3,2)=TY
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MAJME1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the MEASURE mode
C2    of operation is selected from the master menu.
C
      include 'include/menun.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
      REAL X,Y
      EXTERNAL MNIMES,CLRPEW,GTHFMC,GTMCHI,TCURS,
     +         MEAS1,GTMCLO,GTCLRM
C
C     Clear major and minor menus, and enter the
C     major MEASURE options.
      CALL MNIMES()
C
C     clear the error and prompt windows.
      CALL CLRPEW
C
C     Making single line the default measure.
      MEN=2
C     'A' is the token used by measure entity.
      CCMD='A'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select input mode.
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit.
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited.
         CALL GTMCHI(TMEN,TCELL)
C        major MEASURE option selected.
         IF (CCMD.EQ.'A') THEN
C           set search status to return single entity.
            GSSTAT=1
C           MEASURE entity option.
            CALL MESE00()
         ELSE IF (CCMD.EQ.'P') THEN
C           set search status to return group headers.
            GSSTAT=4
C           MEASURE PROPERTY option
            CALL MESP00()
         ELSE IF (CCMD.EQ.'K') THEN
C           set search status to return single entity.
            GSSTAT=1
C           MEASURE DISTANCE option
            CALL MEAS1()
         ELSE IF (CCMD.EQ.'p') THEN
C           set search status to return single entity.
            GSSTAT=1
C           MEASURE POSITION option
            CALL MEASP1()
         ELSE IF (CCMD.EQ.'B') THEN
C           set search status to return single entity.
            GSSTAT=1
C           MEASURE AREA  option.
            CALL MEASA1()
         ELSE IF (CCMD.EQ.'a') THEN
C           set search status to return single entity.
            GSSTAT=1
C           MEASURE ANGLE  option
            CALL MESAG1()
         ELSE
C           UNRECOGNISED option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell.
            MEN=0
         END IF
         IF (CCMD.EQ.'q') RETURN
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         CALL DEPRNT(176)
      END IF
      GOTO 10
C
 99   CONTINUE
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEAS1()
C     ==================
C1    no arguments required
C2
C2    controls operation of the Measure Distance function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,CLRPEW,MNLPTS,MEAS2,MNUPTS
C
      TMEN=MEN
      TCELL=CELLN
C     initialize points option menu
      CALL MNLPTS()
C     Add precision
      CALL MNIME2()
C     enter the Measure Distance routine
      CALL MEAS2()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C     clear point mode
      CALL MNUPTS()
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEAS2()
C     ==================
C
C2    Subroutine MEAS2 controls the
C2    MEASURE DISTANCE function.
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
C
      REAL DEG,SCX1,SCY1,SCX2,SCY2,
     +     DIST,DISTXY,X1,Y1,X2,Y2,VAL
      CHARACTER*80 OLIN,FORM*30,STR*10
      INTEGER*4 I,NLEN1,RES
      LOGICAL OK,FIRST,OPTION,QUIT
      INTRINSIC MAX
      EXTERNAL FINDP0,DISTXY,NOSRCH,WO2SC,CROSS,NLEN1
C
C     enable no searching
      CALL NOSRCH()
      FIRST=.TRUE.
 10   CONTINUE
C     go get a point
      CALL FINDP0(250,X1,Y1,OPTION,QUIT)
      IF (QUIT) RETURN
      IF (OPTION) THEN
C       handle modifier
        CALL MNMEAS()
        GOTO 10
      END IF
C     put cross on screen at hit point
      IF ( . NOT. FIRST ) THEN
         CALL CROSS(SCX1,SCY1)
      END IF
C     convert to screen coords
      CALL WO2SC(X1,Y1,SCX1,SCY1)
      CALL CROSS(SCX1,SCY1)
C
 20   CONTINUE
C     go get a point
      CALL FINDP0(251,X2,Y2,OPTION,QUIT)
      IF (QUIT) RETURN
      IF (OPTION) GOTO 20
C     put cross on screen
      IF ( . NOT. FIRST ) THEN
         CALL CROSS(SCX2,SCY2)
      END IF
C     convert to screen coords
      CALL WO2SC(X2,Y2,SCX2,SCY2)
      CALL CROSS(SCX2,SCY2)
C     calc distance between points
      DIST=DISTXY(X1,Y1,X2,Y2)
C
      VAL=MAX(DIST,ABS(X2-X1),ABS(Y2-Y1))
      RES=4
      IF ( VAL .GT. 1.0 ) THEN
         RES=INT(LOG10(VAL))+RES
      END IF
C
      WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
      CALL CRUNCH(STR)
      WRITE(UNIT=FORM,FMT='(5A)')
     +  '(A,',STR(1:NLEN1(STR)),',SP,2(A,',STR(1:NLEN1(STR)),'))'
C
      WRITE(UNIT=OLIN,FMT=FORM)
     +    'Distance is :',DIST,'  Xincr',X2-X1,'  Yincr',Y2-Y1
      CALL ERRORDIALOG(OLIN)
      CALL CPRINT(OLIN)
      FIRST=.FALSE.
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEASA1()
C     ====================
C1    no arguments required
C2
C2    controls operation of the Measure Area function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     Initialise menu 3 with control cells
      CALL MNIARE()
C     enter the Measure Area routine
      CALL MEASA2()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEASA2()
C     ===================
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/hdata.inc'
      include 'include/lfu.inc'
C
      INTEGER*2 FN
      INTEGER*4 I,REC
      LOGICAL OK
      EXTERNAL OURSCR,DCPRNT,DEPRNT,PULENT,MEASA3
C
      REC=52
      CALL OURSCR(HUNIT,REC,OK)
      IF ( .NOT. OK ) THEN
C        This would be a strange thing but it could happen
         CALL DCPRNT(143)
         CALL DEPRNT(90)
         RETURN
      END IF
C
 10   CONTINUE
 
      FN=3
 
      CALL PULENT(FN)
 
      IF ( NO .EQ. 0 .OR. MEN .EQ. 2 .OR.
     +     CCMD .EQ. 'Q' .OR. CCMD .EQ. 'q' ) THEN
C           Get rid of temporary scratch files
            CLOSE(UNIT=HUNIT)
CIBM
C            LFU(HUNIT)=.FALSE.
CIBM
            RETURN
      END IF
C
      I=NO
      CALL MEASA3(HUNIT,I)
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEASA3(FUNIT,N)
C     ==========================
C
      include 'include/wtov.inc'
      include 'include/entity.inc'
      include 'include/area.inc'                       
      include 'include/viewport.inc'                       
 
      CHARACTER*80 FILNM,FORMAT,SEQ*20,UNF*20
      LOGICAL SAME,YES,OP,HOLD,OK,CLOCK,START
      INTEGER*2 MIP1,ENT,ENTH
      INTEGER*4 N,I,NP,FUNIT,OUNIT,ON,NLEN
      REAL X1,X2,Y1,Y2,Z1,BX,BY,OX,OY
      DOUBLE PRECISION TAREA,TPERM,GX,GY,I2MX,I2MY,DPI,
     +                 IXYX,IXYY,IXYXG,I2MGX,I2MGY,THETA,IM,IT
      DOUBLE PRECISION L1,L2,L3,M1,M2,M3,XP,YP,PRANG
      DOUBLE PRECISION IU,IV,KXX,KYY,SMODX,SMODY,IXX,IYY,TTHETA

      EXTERNAL SAME,DPI,NLEN,OPNFFA,CLOSUN

      INTRINSIC COS
C     Temporary path of section modulus 
      SMODX = 0.0
      SMODY = 0.0
                           
      ON=0
      GX=0.0
      GY=0.0
      I2MX=0.0
      I2MY=0.0
      IXYX=0.0
      IXYY=0.0
      TAREA=0.0
      TPERM=0.0
C     initialise in case of problems
      OX=0.0
      OY=0.0
C     get the starting coords
      CALL SORTSC(OX,OY,CLOCK)
C     
C     Initialise the limits of the boundary
      YXMAX = -1E+38
      YXMIN =  1E+38
      YYMAX = -1E+38
      YYMIN =  1E+38

C
C
      DO 20 I=1,N
         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1
         IF(ENT .EQ. LINE) THEN
            CALL PERIM(DBLE(X1),DBLE(Y1),DBLE(X2),DBLE(Y2),TPERM)
            CALL AREA(DBLE(X1),DBLE(Y1),DBLE(X2),DBLE(Y2),TAREA)
            CALL CENT(DBLE(X1),DBLE(Y1),DBLE(X2),DBLE(Y2),
     +                GX,GY,I2MX,I2MY,IXYX)
            OX=X2
            OY=Y2
         ELSEIF(ENT .EQ. ARC) THEN
C           right check whether the next part of the profile
C           is connected to the start or the end of the arc
            IF (SAME(OX,X1+ABS(X2)*COS(Y2))
     +      .AND. SAME(OY,Y1+ABS(X2)*SIN(Y2)))THEN
C              ok it's connected to the start
               START = .TRUE.
C              reset next position to the end
               OX = X1 + ABS(X2)*COS(Z1)                     
               OY = Y1 + ABS(X2)*SIN(Z1)
            ELSE
C              ok it's connected to the end
               START = .FALSE.
C              reset next position to the start
               OX = X1 + ABS(X2)*COS(Y2)                     
               OY = Y1 + ABS(X2)*SIN(Y2)
            ENDIF
            CALL APERM(DBLE(X2),DBLE(Y2),DBLE(Z1),TPERM)
            CALL ARCCAL(DBLE(X1),DBLE(Y1),DBLE(X2),DBLE(Y2),
     +                   DBLE(Z1),TAREA,GX,GY,I2MX,I2MY,IXYX,START)
         ENDIF    
 20   CONTINUE


C
      IF ( ABS(TAREA) .GT. 0.0 ) THEN
         GX=GX/TAREA
         GY=GY/TAREA
      END IF
C
 30   CONTINUE
C    
C     do a polygon fill on the area
C     only if either no viewports defined or in a sub viewport
C     ie not the main one
C     else inform user of why 
      IF ((.NOT.MVPACT).OR.((MVPACT).AND.(CVPN.NE.0))) THEN
         CALL POLDRW(OK)                       
         IF(.NOT. OK) THEN
            CALL DEPRNT(618)
         ENDIF
      ELSE
         CALL DEPRNT(617)
      ENDIF
C
      CALL DPRMXP(81,FILNM)
C     if null file name assume abort required
      IF (NLEN(FILNM).EQ.0) RETURN
C     open the file,ask the user before over-writing
      CALL OPNFFA(FILNM,OUNIT,OK)
      IF (.NOT.OK) GOTO 30
C
C     write data to output file
      FORMAT='(A,1P,G14.6)'
      WRITE(OUNIT,FORMAT) '          Perimeter :',TPERM
      WRITE(OUNIT,FORMAT) '                Area:',TAREA
      WRITE(OUNIT,FORMAT) 'Centre of Gravity  X:',GX
      WRITE(OUNIT,FORMAT) '   "    "    "     Y:',GY
      WRITE(OUNIT,'(A)') ' '
      WRITE(OUNIT,FORMAT)
     +    'Moment of Inertia about X Axis         :',I2MX
      WRITE(OUNIT,FORMAT)
     +    '                        Y Axis         :',I2MY
      WRITE(OUNIT,'(A)') ' '
      I2MGX=I2MX-TAREA*GY**2
      I2MGY=I2MY-TAREA*GX**2
      IXX = I2MGX
      IYY = I2MGY                                                 

C     Radius of Gyration about Horizontal Centroid Axis
C     is Momemt of Inertia about Centroid X / Area 
      KXX = SQRT(ABS(IXX / TAREA))
C     Calculate section modulus about Centroid X 
      IF (ABS(YYMAX - GY) .GE. ABS(YYMIN - GY)) THEN
        SMODX = IXX / (ABS(YYMAX - GY))
      ELSE
        SMODX = IXX / (ABS(YYMIN - GY))
      ENDIF

      WRITE(OUNIT,'(A)')
     +    'Moment of Inertia about Horizontal'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                     Ixx:',I2MGX
      WRITE(OUNIT,'(A)')
     +    'Minimum Section Modulus about Horizontal'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                      Zx:',SMODX
      WRITE(OUNIT,'(A)')
     +    'Radius of Gyration about Horizontal'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                     Kxx:',KXX
      WRITE(OUNIT,'(A)') ' '

C     Radius of Gyration about Horizontal Centroid Axis
C     is Momemt of Inertia about Centroid Y / Area 
      KYY = SQRT(ABS(I2MGY / TAREA))
C     Calculate section modulus about Centroid Y 
      IF (ABS(YXMAX - GX) .GE. ABS(YXMIN - GX)) THEN
        SMODY = IYY / (ABS(YXMAX - GX))
      ELSE
        SMODY = IYY / (ABS(YXMIN - GX))
      ENDIF

      WRITE(OUNIT,'(A)')
     +    'Moment of Inertia about Vertical'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                     Iyy:',I2MGY
      WRITE(OUNIT,'(A)')
     +    'Minimum Section Modulus about Vertical'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                      Zy:',SMODY
      WRITE(OUNIT,'(A)')
     +    'Radius of Gyration about Vertical'
      WRITE(OUNIT,FORMAT)
     +    'Centroidal Axis                     Kyy:',KYY
      WRITE(OUNIT,'(A)') ' '                      

      WRITE(OUNIT,FORMAT)
     +    'Product of Inertia about origin        :',IXYX

      IXYXG=IXYX-TAREA*GX*GY

C     check for symmetry of the shape
C     and if it is symmetrical i.e. Ixy relatively
C     near to zero then set principal axis angles
C     and Ixy to zero
      IF(ABS(IXYXG)/(ABS(I2MGX)+ABS(I2MGY)) .LT. 1.0/200.0) THEN
         PRANG = 0.0
         THETA = 0.0
         IXYXG = 0.0
C
      ELSE
          IF ( SAME(REAL(I2MGX),REAL(I2MGY)) ) THEN
              THETA=DPI(0.25D0)
          ELSE
             THETA=ATAN(-(2*IXYXG/(I2MGX-I2MGY)))/2.0
          END IF
                       
          PRANG = THETA*180.0D0/DPI(1.0D0)
          IF (PRANG .LT. 0.0) THEN
             PRANG = PRANG + 90.0
          ENDIF 
      ENDIF
      WRITE(OUNIT,FORMAT)
     +    'Product of Inertia about Centroid   Ixy:',IXYXG

      WRITE(OUNIT,FORMAT)
     +    'Angle of Principal Axis (degs)         :',PRANG
               
C     This is to make the equations more readable see
C     E.J.Hearn Vol2 Mechanics of Materials 2nd Edition          
C     The SEC = 1/COS
      TTHETA = 2.0 * THETA 
      IU = 0.5 * (IXX + IYY) + 0.5 * (IXX - IYY) * (1.0/COS(TTHETA))
      IV = 0.5 * (IXX + IYY) - 0.5 * (IXX - IYY) * (1.0/COS(TTHETA))
      WRITE(OUNIT,FORMAT)
     +    'Moment about Principal Axis          Iu:',IU
      WRITE(OUNIT,FORMAT)
     +    '                                     Iv:',IV

      WRITE(OUNIT,FORMAT)
     +    'Second Polar Moment about Centroid    J:',IXX+IYY
      WRITE(OUNIT,FORMAT)
     +    'Radius of gyration about Centroid   Kzz:',
     1 SQRT(ABS((IXX+IYY)/ TAREA))      
C
C      IT=SQRT(((0.5*(I2MGX-I2MGY))**2)+IXYXG**2)
C      IM=0.5*(I2MGX+I2MGY)
C      WRITE(OUNIT,FORMAT)
C     +    'Maximum Inertia                        :',IM+IT
C      WRITE(OUNIT,FORMAT)                  
C     +    'Minimum Inertia                        :',IM-IT
C
C     close the output file
      CALL CLOSUN(OUNIT,.TRUE.,OK)
C
      IT=GY
      IM=GX+DBLE(WXMAX)
      CALL DCVL14(GX,GY,IM,IT,L1,L2,L3)
      CALL DRAWT(L1,L2,L3,4)
      IT=GY+DBLE(WYMAX)
      IM=GX
      CALL DCVL14(GX,GY,IM,IT,L1,L2,L3)
      CALL DRAWT(L1,L2,L3,4)
      IT=GY
      IM=GX+DBLE(WXMAX-WXMIN)
      CALL DCVL14(GX,GY,IM,IT,L1,L2,L3)
      CALL DVV0L5(L1,L2,L3,GX,GY,THETA,M1,M2,M3)
      CALL DRAWT(M1,M2,M3,2)
      CALL DVV0L6(M1,M2,M3,GX,GY,L1,L2,L3 )
      CALL DRAWT(L1,L2,L3,2)
C     pop the results onto the screen was 350,250 at end 
      CALL POPPD1(FILNM,100,200,450,450)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEASP1()
C     ==================
C1    no arguments required
C2
C2    controls operation of the Measure Distance function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,CLRPEW,MNLPTS,MEAS2,MNUPTS
C
      TMEN=MEN
      TCELL=CELLN
C     initialize points option menu
      CALL MNLPTS()
C     Add precision
      CALL MNIME2()
C     enter the Measure Distance routine
      CALL MEASP2()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C     clear point mode
      CALL MNUPTS()
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MEASP2()
C     ==================
C
C1    Measure position
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
C
      REAL DEG,SCX1,SCY1,
     +     DIST,DISTXY,X1,Y1,VAL
      CHARACTER*80 OLIN,FORM*30,STR*10,NGR
      INTEGER*4 I,NLEN1,RES
      LOGICAL OK,FIRST,OPTION,QUIT
      INTRINSIC MAX
      EXTERNAL FINDP0,DISTXY,NOSRCH,WO2SC,CROSS,NLEN1
C
C     enable no searching
      CALL NOSRCH()
      FIRST=.TRUE.
C
 10   CONTINUE
C     go get a point
      CALL FINDP0(250,X1,Y1,OPTION,QUIT)
      IF (QUIT) RETURN
      IF (OPTION) THEN
C       handle modifier
        CALL MNMEAS()
        GOTO 10
      END IF
C     put cross on screen.
      IF ( . NOT. FIRST ) THEN
C       erase existing cross from screen
        CALL CROSS(SCX1,SCY1)
      END IF
C     transform to screen coords
      CALL WO2SC(X1,Y1,SCX1,SCY1)
      CALL CROSS(SCX1,SCY1)
 
      OK=.FALSE.
      IF (DRWSHT(1:1).EQ.'M' .OR. DRWSHT(1:2).EQ.'AA') THEN
C	hack way of doing a test for NGR calculation
C       M is a map sheet. AA is merged together, therefore probably maps
       CALL GETNGR(X1,Y1,NGR,OK)
       OLIN='NGR = '//NGR
      END IF
 
      IF (.NOT.OK ) THEN
C       either not a map or map calc failed
        VAL=MAX(ABS(X1),ABS(Y1))
        RES=4
        IF ( VAL .GT. 1.0 ) THEN
           RES=INT(LOG10(VAL))+RES
        END IF
C
        WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
        CALL CRUNCH(STR)
        WRITE(UNIT=FORM,FMT='(5A)')
     +  '(A,',STR(1:NLEN1(STR)),',SP,A,',STR(1:NLEN1(STR)),')'
C
        WRITE(UNIT=OLIN,FMT=FORM)
     +     'X,Y is :',X1,',',Y1
      END IF
 
      CALL ERRORDIALOG(OLIN)
      CALL CPRINT(OLIN)
      FIRST=.FALSE.
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESAG1()
C     ==================
C1    no arguments required
C2
C2    controls operation of the Measure ANGLEfunction
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,CLRPEW,MNLPTS,MNUPTS
C
      TMEN=MEN
      TCELL=CELLN
C     Add precision
      CALL MNIME2()
C     initialize Between line/points option
      CALL MNLAG3()
C     enter the Measure Angle routine
      CALL MESAG2()
C     ensure insert option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C     clear point mode
      CALL MNUPTS()
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESAG2()
C     ==================
C
C2    Subroutine MESAG2 controls MEASURE ANGLE function.
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include 'include/dimendat.inc'
C
      REAL DEG,SCX1,SCY1,SCX2,SCY2,PDAT(2,4),X4,Y4,X3,Y3,CANG,
     +     DIST,DISTXY,X1,Y1,X2,Y2,VAL,LDAT1(6),LDAT2(6),X5,Y5,
     1     A1,A2,PI,CDAT(6),DCR
      CHARACTER*80 OLIN,FORM*30,STR*10
      INTEGER*2 ENT1,ENT2
      INTEGER*4 C,TMEN,TCELL,I,NLEN1,RES,Q1,Q2,QUADA
      LOGICAL OK,FIRST,CLKWS
      INTRINSIC MAX
      EXTERNAL GETANS,CPRINT,DISTXY,NOSRCH,WO2SC,CROSS,NLEN1,
     +         CANG,CLKWS,PI
C
C     enable no searching
      CALL NOSRCH()
      CALL ADSRCH(LINE)
C     set to default between lines mode.
C
 10   CONTINUE
C     go get dimension reference points
      CALL DIMA10(PDAT,LDAT1,LDAT2,X4,Y4,.TRUE.,OK)
C     test for quit character
      IF (MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         GOTO 10
      END IF
C
C     find dimension circle radius
      DCR=DISTXY(PDAT(1,1),PDAT(2,1),PDAT(1,3),PDAT(2,3))
C     create temporary data array for dim circle
      CDAT(1)=PDAT(1,1)
      CDAT(2)=PDAT(2,1)
      CDAT(3)=0.0
      CDAT(4)=DCR
      CDAT(5)=0.0
      CDAT(6)=PI(2.0)
      IF (DOPFLG(1)) THEN
C        between points mode
C        put data into first array
         LDAT1(1)=PDAT(1,1)
         LDAT1(2)=PDAT(2,1)
         LDAT1(3)=0.0
         LDAT1(4)=PDAT(1,2)
         LDAT1(5)=PDAT(2,2)
         LDAT1(6)=0.0
C        put data into second array
         LDAT2(1)=PDAT(1,1)
         LDAT2(2)=PDAT(2,1)
         LDAT2(3)=0.0
         LDAT2(4)=PDAT(1,3)
         LDAT2(5)=PDAT(2,3)
         LDAT2(6)=0.0
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
      END IF
C     Set the true angle.
      Q1=QUADA(A1)
      Q2=QUADA(A2)
C     Check for quadrant fit.
      IF ( (Q1.EQ.3.OR.Q1.EQ.4 ).AND.(Q2.EQ.1) .OR.
     +     (Q1.EQ.4).AND.(Q2.EQ.2) ) THEN
C         danger quadrants.
          VAL=PI(2.0)-(ABS(A1-A2))
      ELSE
          VAL=A2-A1
      END IF
      RES=4
      IF ( VAL .GT. 1.0 ) THEN
         RES=INT(LOG10(VAL))+RES
      END IF
      WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
      CALL CRUNCH(STR)
      WRITE(UNIT=FORM,FMT='(3A)')'(A,2(2X,',STR(1:NLEN1(STR)),'))'
      WRITE(UNIT=OLIN,FMT=FORM) 'Angle is :',DEG(VAL),360.0-DEG(VAL)
      CALL ERRORDIALOG(OLIN)
      CALL CPRINT(OLIN)
      CALL UNFLAG(.TRUE.)
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESE00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the Measure ENTITY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C
C     load sub menu with entities which
      CALL MNIME1()
C     enter the Measure Entity routine
      CALL MESME2()
C     Clear menu 3 cos we might not need it no more.
      CALL GTCLRM(3)
C
C     ensure measure option for arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESME2()
C     ===================
C
C2    Subroutine MESME2 is the MEASURE ENTITY function
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      REAL X,Y,DEG,M(3,3)
      CHARACTER  ANS*3
      INTEGER*2 ENT,MIPP
      INTEGER*4 C,TMEN,TCELL,I,TC
      LOGICAL OK,OPTION,QUIT
      INTRINSIC CHAR
      EXTERNAL NOSRCH,ADSRCH,CPRINT,TCURS,UNFLAG,DSE800,DEG,
     +         CRUNCH,GTPMSG
C
C     allow searching for all entities
      CALL ALSRCH()
 10   CONTINUE
      CALL FINDET(386,X,Y,MIPP,ENT,OPTION,QUIT)
C     since we used all 3 line the last line for displaying
C     data is put there by GTPMSG so call CPRINT will scroll
C     the line up for the next data if there is any.
      CALL DCPRNT(90)
C     test for quit character
      IF (QUIT) THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
C
      IF (OPTION) THEN
         CALL MNMEAS()
         GOTO 10
      END IF
C     read entity data
      CALL ALLRD(MIP,ENT,M,OPTION)
C     write entity data to screen
      CALL OTDATA()
C     clear scren flags
      CALL UNFLAG(.TRUE.)
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESP00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the MEASURE PROPERTY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MESP01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     enter the MEASURE PROPERTY routine
      CALL MESP01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MESP01()
C     ===================
C
C2    Subroutine MESP01 handles the control of
C2    MEASURE PROPERTY function,which allows
C2    access to property data attached to entities
C2    within the database.
C
      include 'include/nbuff.inc'
      include 'include/props.inc'
      include 'include/product.inc'
C
      REAL X,Y
      INTEGER*4 C,TMEN,TCELL,NP,NLEN
      INTEGER*2 MIPP,ENT
      LOGICAL OK,OPTION,QUIT
      CHARACTER*80 OLIN,ANS*1
      INTRINSIC CHAR
      EXTERNAL ALSRCH,CRUNCH,
     +         DSE800,FINDET,UNFLAG,NLEN
C
C     enable searching for all entities
      CALL ALSRCH()
 10   CONTINUE
      CALL FINDET(222,X,Y,MIPP,ENT,OPTION,QUIT)
 20   CONTINUE
C     test for quit character
      IF (QUIT) THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (OPTION) THEN
C
         GOTO 10
      END IF
C
C***************************************************************
C                  PROPERTY EXTRACT ROUTINE                    *
C***************************************************************
      IF (IMBUFF(11).LE.0) THEN
C        no properties attached
C        "No properties attached"
         CALL DEPRNT(343)
C         WRITE(10,*)' [MESP01] no property link present mip=',mipp
C         WRITE(10,*)' ENTITY=',ENT
      ELSE
C        property link present
C        place list of properties in buffer ENTPRP
         CALL EXPR03(MIPP,NP,OK)
         IF (.NOT.OK) THEN
C           cannot extract list of properties
C           "No properties attached"
            CALL DEPRNT(343)
C         WRITE(10,*)' [MESP01] canot extract properties mip=',mipp
C         WRITE(10,*)' ENTITY=',ENT
            GOTO 10
         END IF
C        write them to file for display
         CALL EXPR93(NP,PRNAM(1:NLEN(PRNAM))//'.pro',OK)
         IF (OK) THEN
C           pop to screen for display
C           "Exit properties file before proceeding"
            CALL DCPRNT(206)
            CALL POPPD1(PRNAM(1:NLEN(PRNAM))//'.pro',774,120,250,668)
         ELSE
C           must be file error
C           "Error storing data"
            CALL DEPRNT(149)
         END IF
      END IF
C
      CALL UNFLAG(.TRUE.)
      GOTO 10
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNIARE()
C     =================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2    Tokens used here are B and CHAR(150)
C2
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTPMEN,GTCLRM
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C2    CHAR(149) is the token for CANCEL
      CALL GTDMEN(121,3)
C2    B is the token for NEXT BOUNDARY.
      CALL GTDMEN(122,3)
 
C2    CHAR(150) is the token for ACCEPT.
      CALL GTDMEN(126,3)
C     this is autoprofile command
      CALL GTDMEN(469,3)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNIME1()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major INSERT options.
C2
      include 'include/movdat.inc'
 
      CHARACTER*1 TOKEN
      EXTERNAL GTCLRM,GTMCHI,GTDMHD,GTDMEN
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Load the entities types into major option menu.
C
C2    H is the token for HATCH.
      CALL GTDMEN(390,3)
C2    T is the token for TEXT.
      CALL GTDMEN(391,3)
C2    A is the token for ARC.
      CALL GTDMEN(392,3)
C2    L is the token for LINE.
      CALL GTDMEN(393,3)
C2    E is the token for CENLIN.
      CALL GTDMEN(389,3)
C
C     Set Any as default
      CALL GTDMEN(394,3)
      CALL FNDTOK(394,TOKEN)
      MNCELL(1,1)=3
      CALL GTHFMC(MNCELL(1,1),TOKEN,MNCELL(1,2))
      CALL GTMCHI(MNCELL(1,1),MNCELL(1,2))
 
C     [S]     /Spline/
      CALL GTDMEN(395,3)
C     [D]     /Dimension/
      CALL GTDMEN(396,3)
C     [c]     /Component/
      CALL GTDMEN(397,3)
C     [s]     /Symbol/
      CALL GTDMEN(398,3)
C
      CALL MNIME2()
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNIME2()
C     ===================
C1    No arguments required.
C
      include 'include/movdat.inc'
      include 'include/ndata.inc'
C
      CHARACTER*1 TOKEN
C
C2    p is the token for Precision:
      CALL GTDMEN(229,3)
C     find token for precision:
      CALL FNDTOK(229,TOKEN)
C     enter precision to menu
      CALL GTMCWI(3,TOKEN,PREC)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNIMES()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major INSERT options.
C2
C
      EXTERNAL GTCLRM,GTMCHI,GTDMHD,GTDMEN
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the MEASURE major option.
      CALL GTDMHD(22,2)
C
C     Load the entities types into major option menu.
C
C      CALL GTDMEN(173,2)
C2    K is the token for DISTANCE
      CALL GTDMEN(174,2)
C2    a is the token for ANGLE
      CALL GTDMEN(175,2)
C2    B is the token for AREA
      CALL GTDMEN(176,2)
C2    A is the token for Entity
      CALL GTDMEN(177,2)
C     P is the token for Property
      CALL GTDMEN(178,2)
C     p is the token for Position
      CALL GTDMEN(179,2)
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE MNLAG3()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLAGM3 loads the options
C2    for the MEASURE ANGULAR  function into
C2    menu no3.
C2
C2
      include 'include/dimendat.inc'
      include 'include/movdat.inc'
C
      EXTERNAL GTDMEN,GTMCHI,MNLDM2,FNDPOS
C
C2    A is the token for BETWEEN LINES
      CALL GTDMEN(232,3)
      DOPFLG(2)=.TRUE.
      DOPFLG(1)=.FALSE.
C     save the menu,cell hilited
      MNCELL(2,1)=3
      CALL FNDPOS(232,MNCELL(2,2))
C     hilite the cell
      CALL GTMCHI(MNCELL(2,1),MNCELL(2,2))
C2    P is the token for POINT TO POINT
      CALL GTDMEN(221,3)
C
      END
      SUBROUTINE MNMEAS()
C     ===================
C1    No arguments required.
C
C2    MNMEAS handles any menu 3 actions
C     for the measure option
C
      include 'include/movdat.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
C
      INTEGER*4 NLEN,I,INT
      DOUBLE PRECISION DN
      REAL REAL
      LOGICAL CHANGE
      CHARACTER*20 TEMP,OLIN,CCBUFF,TOKEN*1
      INTRINSIC INT,REAL
C
      IF ( CCMD .NE.'p') THEN
         CALL GTMCLO(MNCELL(1,1),MNCELL(1,2))
      END IF
C
      IF (CCMD.EQ.'p') THEN
C        set new precision level
 111     CONTINUE
         CALL DPRMXP(111,CCBUFF)
C
         IF (NLEN(CCBUFF).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(CCBUFF,DN,*111)
C           Check range of precision is valid
            IF ( INT(DN).GT.6.OR.INT(DN).LT.0 ) THEN
               CALL DEPRNT(234)
               GOTO 111
            END IF
            PREC=INT(DN)
C           find token for precision:
            CALL FNDTOK(229,TOKEN)
C           write precision to menu cell
            CALL GTMCWI(3,TOKEN,PREC)
            CALL GTMCLO(MEN,CELLN)
         END IF
         RETURN
      ELSE IF ( CCMD .EQ. 'A' ) THEN
C        Search for anything
         CALL ALSRCH()
      ELSE IF ( CCMD .EQ. 'L' ) THEN
C        Only lines please
         CALL NOSRCH()
         CALL ADSRCH(LINE)
      ELSE IF ( CCMD .EQ. 'E' ) THEN
C        Only centre lines please
         CALL NOSRCH()
         CALL ADSRCH(CENLIN)
      ELSE IF ( CCMD .EQ. 'T' ) THEN
C        Only text please
         CALL NOSRCH()
         CALL ADSRCH(TEXT)
      ELSE IF ( CCMD .EQ. 'C' ) THEN
C        Only Arc please
         CALL NOSRCH()
         CALL ADSRCH(ARC)
      ELSE IF ( CCMD .EQ. 'H' ) THEN
C        Only Hatch please
         CALL NOSRCH()
         CALL ADSRCH(HATCH)
      ELSE IF ( CCMD .EQ. 'S' ) THEN
C        Only spline please
         CALL NOSRCH()
         CALL ADSRCH(SPLINE)
      ELSE IF ( CCMD .EQ. 'c' ) THEN
C        Only components please
         CALL NOSRCH()
         CALL ADSRCH(COMPI)
      ELSE IF ( CCMD .EQ. 's' ) THEN
C        Only components please
         CALL NOSRCH()
         CALL ADSRCH(SYMBI)
      ELSE IF ( CCMD .EQ. 'D' ) THEN
C        Only Dimensions please
         CALL NOSRCH()
         CALL ADSRCH(LDIMN)
         CALL ADSRCH(RDIMN)
         CALL ADSRCH(DDIMN)
         CALL ADSRCH(ADIMN)
      END IF
C
      MNCELL(1,1)=MEN
      MNCELL(1,2)=CELLN
C
      END
C
C     ---------------------------------------------------------
C
      SUBROUTINE PERIM(X1,Y1,X2,Y2,TPERM)
C     ===================================
C
      DOUBLE PRECISION X1,Y1,X2,Y2,TPERM,DDSTXY
      EXTERNAL DDSTXY
C
      TPERM=TPERM+DDSTXY(X1,Y1,X2,Y2)
C
      END
C
C     ---------------------------------------------------------
C
