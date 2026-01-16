C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 polyfil.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE POLARC(X,Y,WRAD,SANG,EANG,CLOCK,NOX,NOY,NOE)
C     SUBROUTINE SORTPR(ENTP,OX,OY,CLOCK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE POLARC(X,Y,WRAD,SANG,EANG,CLOCK,NOX,NOY,NOE)
C     ==========================================
C
C1    vartype          R R  R    R    R
C1    iostatus         I I  I    I    I
C
C2       Subroutine ARCCT draw a arc  centre X,Y
C2       with radius WRAD drawing anti-clockwise
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/viewport.inc'
C
C
      INTEGER*2 NOX(6000),NOY(6000),NOE
      INTEGER COUNT,I,MAXN
      LOGICAL OK,CLOCK
      REAL RADS,X,Y,WRAD,NX,NY,TRS(3,3),OX,OY,PIXELS,
     +  SRAD,PI,SANG,EANG,ANGLE,VX1,VY1,VX2,VY2
      PARAMETER( PIXELS=1.8 , MAXN = 1000 )
      INTRINSIC ABS,SQRT,DBLE,REAL,COS,SIN
      EXTERNAL WTOSVX,ROTP2D,DRAWLW,NEWXY,ARCCHK
      CALL WTOSVX(WRAD,SRAD)
C     Calculate the angle in radians which will give a
C     smooth curve for the size of radius.
      RADS=PIXELS/MAX(1.0,SQRT(SRAD))
C     find included angle.
      ANGLE=ABS(EANG-SANG)
      IF ( EANG .LT. SANG ) ANGLE=PI(2.0)-ANGLE
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc.
      COUNT=MIN(1+ABS(NINT(ANGLE/RADS)),MAXN)
C     Recalculate the angle to obtain an whole value which
C     will draw a complete arc.
      RADS=DBLE(ANGLE/REAL(COUNT))
      IF(.NOT.CLOCK) RADS = -DBLE(ANGLE/REAL(COUNT))
C     Obtain the transformation matrice which rotates about
C     the centre of the circle X,Y by the angle RADS.
      CALL ROTP2D(X,Y,RADS,TRS)
C     starting point of the drawing of the circle.
      IF(CLOCK) THEN
          OX=X+WRAD*COS(SANG)
          OY=Y+WRAD*SIN(SANG)
      ELSE
          OX=X+WRAD*COS(EANG)
          OY=Y+WRAD*SIN(EANG)
      ENDIF
      CALL WO2SC(OX,OY,VX1,VY1)               
C     Ensure that the screen limits are okay 
      IF (VX1 .GT. 32000.0) VX1 = 32000.0
      IF (VX1 .LT. -32000.0) VX1 = -32000.0
      IF (VY1 .GT. 32000.0) VY1 = 32000.0
      IF (VY1 .LT. -32000.0) VY1 = -32000.0    
C         
      NOE = NOE+1
      NOX(NOE) = VX1
      NOY(NOE) = VY1
C     loop for the whole number calculated above.
      OK=.FALSE.
         DO 10 I=1,COUNT,1
C           transform OX to NX by transformation matrice
C           and also OY to NY.
            CALL NEWXY(OX,OY,NX,NY,TRS)
C           Draw between old point and new.
            CALL WO2SC(OX,OY,VX1,VY1)
C           Ensure that the screen limits are okay 
            IF (VX1 .GT. 32000.0) VX1 = 32000.0
            IF (VX1 .LT. -32000.0) VX1 = -32000.0
            IF (VY1 .GT. 32000.0) VY1 = 32000.0
            IF (VY1 .LT. -32000.0) VY1 = -32000.0    
C         
            CALL WO2SC(NX,NY,VX2,VY2)
C           Ensure that the screen limits are okay 
            IF (VX2 .GT. 32000.0) VX2 = 32000.0
            IF (VX2 .LT. -32000.0) VX2 = -32000.0
            IF (VY2 .GT. 32000.0) VY2 = 32000.0
            IF (VY2 .LT. -32000.0) VY2 = -32000.0    
C         
C           now we have screen coords VX1,VY1,VX2,VY2
C           must draw line with correct fonting
            NOE = NOE+1
            NOX(NOE) = VX2
            NOY(NOE) = VY2
C           old now new point on the circumference
            OX=NX
            OY=NY
 10      CONTINUE
      END
 
 
 
      SUBROUTINE SORTPR(ENTP,OX,OY,CLOCK)
C     ===================================
C1    VARTYPE            I4  R  R   L
C1    IOSTATUS           I   O  O   O
C
C2    Purpose to sort an NC profile from pulent. I blame
C2    Neil for this.
C
      include 'include/hdata.inc'
      include 'include/entity.inc'
 
      INTEGER*2 LAST
      INTEGER*4 ENTP
      REAL OX,OY,XF,YF
      LOGICAL CLOCK,SAMEPT
      REAL BX,BY,XS1,YS1,XF1,YF1,ZH1,X1,Y1,X2,Y2,Z1
      REAL XS2,YS2,XF2,YF2,ZH2,XAF,YAF,XAS,YAS
      REAL XH1,YH1,XH2,YH2,ZZ1
 
C
      EXTERNAL SAMEPT
C
      OX=0.0
      OY=0.0
C
      READ(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,BX,BY,XS1,YS1,XF1,YF1,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
C
C     set special case
      IF(NO.EQ.1.AND.ENT1.EQ.ARC) THEN
C     full circle so set to start end
          CLOCK = .FALSE.
          OX = XH1 + ABS(XH2) * COS(YH2)
          OY = YH1
          RETURN
      ENDIF
      READ(UNIT=HUNIT,REC=ENTP+1,ERR=99) MIP2,ENT2,BX,BY,
     +                       XS2,YS2,XF2,YF2,ZH2,
     +                       X1,Y1,X2,Y2,Z1
C
      IF(ENT2.EQ.ARC) THEN
C
C         start
          XAS=XS2+XF2*COS(YF2)
          YAS=YS2+XF2*SIN(YF2)
C         end
          XAF=XS2+XF2*COS(ZH2)
          YAF=YS2+XF2*SIN(ZH2)
C         set points
          XS2 = XAS
          YS2 = YAS
          XF2 = XAF
          YF2 = YAF
 
C
      ENDIF
      IF(ENT1.EQ.ARC) THEN
C
C         start
          XAS=XS1+XF1*COS(YF1)
          YAS=YS1+XF1*SIN(YF1)
C         end
          XAF=XS1+XF1*COS(ZH1)
          YAF=YS1+XF1*SIN(ZH1)
C         set points
          XS1 = XAS
          YS1 = YAS
          XF1 = XAF
          YF1 = YAF
 
      ENDIF
C
      IF(ENT1.EQ.LINE) THEN
          IF(SAMEPT(XS1,YS1,XF1,YF1)) THEN
              XS1 = XH2
              YS1 = YH2
          ENDIF
      ENDIF
      IF(SAMEPT(XS1,YS1,XS2,YS2).OR.SAMEPT(XS1,YS1,XF2,YF2)) THEN
          CLOCK = .TRUE.
          OX = XF1
          OY = YF1
          XF = XS1
          YF = YS1
      ELSE
          CLOCK = .FALSE.
          OX = XS1
          OY = YS1
          XF = XF1
          YF = YF1
      ENDIF
      IF(ENT1.EQ.LINE) THEN
          WRITE(UNIT=HUNIT,REC=ENTP) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
     +                       X1,Y1,X2,Y2,Z1
C          WRITE(10,*) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
C
      ENDIF
      LAST =NO
 
C     sort last entity if its a line
      READ(UNIT=HUNIT,REC=LAST) MIP1,ENT1,BX,BY,XS1,YS1,XF1,YF1,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
      READ(UNIT=HUNIT,REC=LAST-1) MIP2,ENT2,BX,BY,XS2,YS2,XF2,YF2,ZH2,
     +                       X1,Y1,X2,Y2,Z1
C
      IF(ENT2.EQ.ARC) THEN
C
C         start
          XAS=XS2+XF2*COS(YF2)
          YAS=YS2+XF2*SIN(YF2)
C         end
          XAF=XS2+XF2*COS(ZH2)
          YAF=YS2+XF2*SIN(ZH2)
C         set points
          XS2 = XAS
          YS2 = YAS
          XF2 = XAF
          YF2 = YAF
 
C
 
      ENDIF
C     sort it if its a line
      IF(ENT1.EQ.LINE) THEN
          IF(SAMEPT(XS1,YS1,XF1,YF1)) THEN
              X1 = XS1
              Y1 = YS1
              X2 = XH1
              Y2 = YH1
              WRITE(UNIT=HUNIT,REC=LAST) MIP1,ENT1,BX,BY,
     +                       X1,Y1,X2,Y2,ZH1,
     +                       XH1,YH1,XH2,YH2,ZZ1
          ENDIF
      ENDIF
C          WRITE(10,*) MIP1,ENT1,BX,BY,OX,OY,XF,YF,ZH1,
C
99    CONTINUE
      END
 
