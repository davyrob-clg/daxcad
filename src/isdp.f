C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 isdp.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION BENSGN(X,Y)
C     SUBROUTINE AXIS(XK,YK,IBCD,NAANT,AXLEN,THETA,STARV,DVAL)
C     SUBROUTINE BENDEF(IBITS)
C     SUBROUTINE BENFRM(IVECT,IPC,IARR)
C     SUBROUTINE BENOUT(ICD)
C     SUBROUTINE BENPAK(I1,I2,I3,I4,NBYTES)
C     SUBROUTINE BENPUT(I1,I2,I3,I4,ICD)
C     SUBROUTINE BENUPK(NA,MA)
C     SUBROUTINE CIRCLE(X,Y,J,R,BANG,SANG)
C     SUBROUTINE CVAS(X1,Y1,X2,Y2)
C     SUBROUTINE CVSA(X1,Y1,X2,Y2)
C     SUBROUTINE CVSB(X1,Y1,X2,Y2,J)
C     SUBROUTINE ECHEL(E1,E2,X1,X2)
C     SUBROUTINE FACTOR(F)
C     SUBROUTINE IBENA(I1,I2,I3)
C     SUBROUTINE LINE(X,Y,N,IDIS,LSI,NRSYM)
C     SUBROUTINE NEWPEN(NPEN)
C     SUBROUTINE NOMBA(X,Y,J,FNP,NC,HX,HY,COSA,SINA)
C     SUBROUTINE NOMBS(X,Y,J,FNP,NC,HX,HY,COSA,SINA)
C     SUBROUTINE NUMBER(XK,YK,SIZE,GETAL,THETA,NDEC)
C     SUBROUTINE OFFSET(X0,XFACT,Y0,YFACT)
C     SUBROUTINE PCARA(X,Y,M,NLIS,NC,HX,HY,COSA,SINA)
C     SUBROUTINE PCARS(X,Y,J,NLIS,NC,HX,HY,COSA,SINA)
C     SUBROUTINE PLOT(X,Y,IPEN)
C     SUBROUTINE PLOTS(IBUF,LBUF,ND)
C     SUBROUTINE PLUMA(JPEN)
C     SUBROUTINE PNUMA(X1,Y1,IBLO,X2,Y2)
C     SUBROUTINE POSA(X,Y)
C     SUBROUTINE POSS(A1,A2,B1,B2,C1,C2)
C     SUBROUTINE SCALE(X,XLEN,N,IDIS)
C     SUBROUTINE SPESYM(X,Y,J,N,HX,HY,COSA,SINA)
C     SUBROUTINE SYMBOL(X,Y,SIZE,IBCD,THETA,N)
C     SUBROUTINE TODRAW(X,Y,Z,I,T,N,K)
C     SUBROUTINE TPARM(X,Y)
C     SUBROUTINE TRAA(X,Y,M)
C     SUBROUTINE TRAS(X,Y,M)
C     SUBROUTINE WHERE(X,Y,FACT)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE AXIS(XK,YK,IBCD,NAANT,AXLEN,THETA,STARV,DVAL)
      DIMENSION IBCD(1)
C     UNIT INCHES
C     DATA COF1,COF2,COF3,COF4,COF5,COF6,COF7,COF8,COF9,COF10,COF11,ICF
C    1     /1.0,-0.1,0.11,-0.07,0.325,0.075,0.14,0.07,0.07,0.05,3.,1/
C     UNIT CMS
      DATA COF1,COF2,COF3,COF4,COF5,COF6,COF7,COF8,COF9,COF10,COF11,ICF
     1    /2.,-0.254,.266,-0.07,0.405,0.17,0.35,0.17,0.088,0.127,1.5,2/
C
C     (XK,YK)      STARTING POINT OF AXIS IN CM
C     IBCD         AXIS TITLE
C     NAANT        NUMBER OF CHARACTERS IN TITLE , - IF UNDER AXIS
C     AXLEN        LENGTH OF AXIS IN CM
C     THETA        ANGLE OF AXIS FROM X-DIRECTION IN DEGREES
C     STARV        VALUE AT THE FIRST TIC MARK
C     DVAL         CHANGE IN VALUE BETWEEN TIC MARKS
C
      KN=NAANT
      A=COF1
C
C     FIX PLACE OF TEXT AND TIC MARKS
C
      IF(KN)1,2,2
   1  A=-A
      KN=-KN
C
C     FIND EXPONENT
C
   2  EX=0.0
      ADV=ABS(DVAL)*COF1
      IF(ADV)3,7,3
   3  IF(ADV-99.0)6,4,4
   4  ADV=ADV*0.1
      EX=EX+1.
      GO TO 3
   5  ADV=ADV*10.0
      EX=EX-1.
   6  IF(ADV-0.01)5,7,7
   7  XVAL=STARV*10.0**(-EX)
      ADV=COF1*DVAL*10.0**(-EX)
      STH=THETA*0.0174533
      CTH=COS(STH)
      STH=SIN(STH)
      DXB=COF2
      DYB=COF10*(COF11*A-1.)
      XN=XK+DXB*CTH-DYB*STH
      YN=YK+DYB*CTH+DXB*STH
      NTIC=AXLEN+1.0
      NT=NTIC/2
      IS=1
      NK=2
      STP=AMOD(XVAL,ADV)
      STP=ABS(STP/ADV)
      IF(STP.LT.0.25.OR.STP.GT.0.75) GO TO 8
       IS=2
      XVAL=XVAL+ADV*0.5
      XN=XN+CTH
      YN=YN+STH
      NT=NT-1
   8  IDEC=ADV*100.0+0.5
      IF(MOD(IDEC,10)) 11,9,11
   9  NK=1
      IF(MOD(IDEC,100)) 11,10,11
  10  NK=-NK
C
C     WRITE TEXT AND VALUES
C
   11 DO 20 I=IS,NTIC,ICF
      CALL NUMBER(XN,YN,COF3,XVAL,THETA,NK)
      XVAL=XVAL+ADV
      XN=XN+COF1*CTH
      YN=YN+COF1*STH
      IF(NT.GT.0) GO TO 20
      NT=NTIC
      Z=KN
      IF(EX)12,13,12
  12  Z=Z+7.0
   13 DXB=COF4*Z+AXLEN*0.5
      DYB=COF5*A-COF6
      XR=XK+DXB*CTH-DYB*STH
      YR=YK+DXB*STH+DYB*CTH
      CALL SYMBOL(XR,YR,COF7,IBCD(1),THETA,KN)
      IF(EX) 14,20,14
   14 Z=Z-5.0
      XR=XR+Z*CTH*COF7
      YR=YR+Z*STH*COF7
      CALL SYMBOL(XR,YR,COF7,3H*10,THETA,3)
      XR=XR+(3.0*CTH-0.8*STH)*COF7
      YR=YR+(3.0*STH+0.8*CTH)*COF7
      CALL NUMBER(XR,YR,COF8,EX,THETA,-1)
   20 NT=NT-ICF
C
C        PLOT AXIS AND TIC-MARKS
C
      CALL PLOT(XK+AXLEN*CTH,YK+AXLEN*STH,3)
      DXB=-COF9*A*STH
      DYB=COF9*A*CTH
      A=NTIC-1
      XN=XK+A*CTH
      YN=YK+A*STH
      DO 30 I=1,NTIC
      CALL PLOT(XN,YN,2)
      CALL PLOT(XN+DXB,YN+DYB,1)
      CALL PLOT(XN,YN,1)
      XN=XN-CTH
      YN=YN-STH
   30 CONTINUE
      RETURN
      END
      SUBROUTINE BENDEF(IBITS)
C+
C     Definitions file for Fortran Level 1 software.
C-
      DIMENSION IBITS(4)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1          IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2          ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3          I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4          IBIAS,IDICT,IE2022,ICORD,IOUT(4),MAXPEN,IES(512)
C
C     Plotter device name.
C
      DATA JDEV/2HTT/
C
C     Unit number (default).
C
      DATA JUNIT/0/
C
C     Bit lengths: IBITS(1) = word length,
C                       (2) = character length,
C                       (3) = output frame length,
C                       (4) = output byte length.
C
      IBITS(1) = 32
      IBITS(2) = 8
      IBITS(3) = 8
      IBITS(4) = 8
C
C     Online/Offline connection switch: 0 = offline,
C					1 = online.
C
      IONOFF = 1
C
C     Direct/Spooled switch: 0 = direct,
C			     1 = spooled.
C
      ISPOOL = 0
C
C     Save device and unit in common.
C
      IDEV = JDEV
      IUNIT = JUNIT
C
C     Set minimum for biased systems.
C
      IBIAS = 0
C
C     Buffer End Control: 0 = FF200000,
C			  2 = FF220202.
C
      IE2022 = 2
C
C     Increments per centimetre.
C
      PASX = 200.0
      PASY = 200.0
C
C     Dictionaries required: 0 = none (software characters),
C			     1 = standard 64 characters,
C			     2 = extended 128 characters.
C
      IDICT = 0
C
C     Character order (1 for PDP, -1 for others).
C
      ICORD = -1
C
C     Maximum pen number.
C
      MAXPEN = 3
C
C     Initialise output channel.
C
      CALL BENOUT(1)
      RETURN
      END
*
      SUBROUTINE BENFRM(IVECT,IPC,IARR)
C+
C     SET BENSON VECTOR FORMAT INTO IARR.
C     IVECT IS THE VECTOR VALUE AND IPC IS THE PEN UP/DOWN CODE.
C-
      DIMENSION IARR(2)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
 
      ISIGN = 0
      JVECT = IVECT
      IF (IVECT .GE. 0) GOTO 10
      ISIGN = 1
      JVECT = - IVECT
   10 IARR(2) = JVECT / (JSHFT / 4)
      IARR(1) = JVECT - IARR(2) * (JSHFT / 4)
      IARR(1) = IARR(1) * 4 + ISIGN + IPC
      RETURN
      END
      SUBROUTINE BENOUT(ICD)
C+
C     Write buffer to output device: online or offline.
C-
      include 'include/pendat.inc'
 
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1          IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2          ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3          I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4          IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      EQUIVALENCE (CHOUT,IES(1))
      CHARACTER*512 CHOUT
      INTEGER*2 STREAM_ID
      INTEGER*4 STATUS,RETPTR,BUFLEN,RETLEN,KEY(3)
      INTEGER*4 SEEK_KEY(3)
      INTEGER*4 BUFFLEN
      INTEGER*4 ICD
      CHARACTER*9 PATHNAME
      CHARACTER*9 SIOS(3)
      CHARACTER*10 NULL
      DATA SIOS/'/dev/sio1','/dev/sio2','/dev/sio3'/
C     First pass: assign device.
C
	goto (10,20,40) icd
 10   CONTINUE
C      PATHNAME(1:9)=SIOS(NTAPE)(1:9)
C      CALL STREAM_$OPEN  (PATHNAME,INT2(9),STREAM_$OVERWRITE,
C     1 STREAM_$NO_CONC_WRITE,STREAM_ID,STATUS)
C      IF(STATUS.NE.0) GOTO 1000
C      CALL SIO_$CONTROL (STREAM_ID,SIO_$SPEED,SIO_$9600,STATUS)
C      IF(STATUS.NE.0)GOTO 1000
C      CALL SIO_$CONTROL (STREAM_ID,SIO_$INPUT_SYNC,.TRUE.,STATUS)
C      IF(STATUS.NE.0) GOTO 1000
C      CALL SIO_$CONTROL(STREAM_ID,SIO_$RAW,.TRUE.,STATUS)
C      IF(STATUS.NE.0) GOTO 1000
      GOTO 100
C
C      OUTPUT BUFFER
C
 20   CONTINUE
C      WRITE(10,*) 'BUFFER SIZE :',MBUF
C      DO 30 I=1,MBUF
C         II=I*4-3
C         II=I
C         WRITE(10,'(I12,4X,3A,I5)')
C     +   IES(I),'"',CHOUT(II:II),'"',ICHAR(CHOUT(II:II))
C         CHOUT(I:I)=CHOUT(II:II)
C   30 CONTINUE
C
C     Write to device.
C
C******************************************************************
C     This addition was done for Paisley Colleges' 1302 which     *
C     only has a 128 byte buffer and combined with using          *
C     DC1/DC3 for handshaking it is too slow to stop the host     *
C     send to many bytes. So we pack the last 8 bytes with nulls  *
C     just to keep the plotter happy                              *
C     Pack out with .nulls.                                       *
C     Note packet length for vaild data was reduced to 120 bytes  *
      DO 22 J=1,8
         NULL=CHAR(0)
 22   CONTINUE
      CHOUT(MBUF+1:MBUF+1)=NULL(1:8)
      BUFFLEN= MBUF+8
C******************************************************************
C     Note I have change the Stream_ID name
         CALL STREAM_PUT_CHR(STRID,CHOUT,BUFFLEN,
     1   STATUS)
         IF(STATUS.NE.0) GOTO 1000
 
      GOTO 100
C
C     close stream
C
C     Note I have change the Stream_ID name
 40   CONTINUE
      CALL STREAM_CLOSE(STRID,STATUS)
      IF (STATUS.NE.0) GOTO 1000
100   RETURN
C
C     REPORT ERROR
C
1000  CALL ERROR_$PRINT (STATUS)
      STOP
      END
*
      SUBROUTINE BENPAK(I1,I2,I3,I4,NBYTES)
C+
C     PUT NBYTES DATA ITEMS INTO THE OUTPUT BUFFER
C-
      DIMENSION II(4)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
C
C     SAVE PARAMETERS IN ARRAY II.
C
      II(1) = I1
      II(2) = I2
      II(3) = I3
      II(4) = I4
C
C     LOOP FOR NBYTES BYTES
C
      DO 40 I = 1,NBYTES
C
C     APPLY BIAS
C
      IF (II(I) .LT. IBIAS) II(I) = II(I) + 64
C
C     GET POSITION POINTERS
C
      KBUF = (MBUF + IFPW - 1) / IFPW
      IPC = MOD(MBUF - 1,IFPW)+1
      IF (IPC .GT. 1) GOTO 20
C
C     TOP BYTE IN OUTPUT WORD: TAKE CARE OF SIGN BIT.
C
      KSHFT = ISHFT ** (IFPW - 1)
      NS = 1
      IF (II(I) .LT. IBMAX) GOTO 10
      NS = - IMAX
      II(I) = II(I) - IBMAX
C
C     SET OUTPUT WORD
C
   10 IES(KBUF) = II(I) * KSHFT + NS - 1
      GOTO 30
C
C     ADD INTO OUTPUT WORD
C
   20 IES(KBUF) = IES(KBUF) + II(I) * KSHFT
C
C     RESET OUTPUT SHIFT CONTROL AND BYTE COUNTER.
C
   30 KSHFT = KSHFT / ISHFT
      MBUF = MBUF + 1
   40 IFREE = IFREE - 1
      RETURN
      END
 
      SUBROUTINE BENPUT(I1,I2,I3,I4,ICD)
C+
C     MOVE I1, I2, I3 AND I4 INTO THE BUFFER
C     CHECK FOR BUFFER FULL - CALL BENOUT IF IT IS, AND SET NEXT BUFFER
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
C
C     SKIP TO RELEVANT SECTION TO PROCESS DATA.
C
      IF (ICD) 30,20,10
C
C     ADD TO BUFFER
C
   10 CALL BENPAK(I1,I2,I3,I4,ICD)
C
C     CHECK FOR BUFFER FULL
C
      IF (IFREE .GT. 0) GOTO 60
C
C     BUFFER FULL: SEND END BLOCK.
C
   20 CALL BENPAK(I77,IE2022+32,IE2022,IE2022,4)
C
C     SEND THE BUFFER
C
   30 MBUF = MBUF - 1
      CALL BENOUT(2)
C
C     SET NEXT BUFFER START
C
   40 DO 50 I = 1,NBUF
   50 IES(I) = 0
      IFREE = IBUF - 4
      MBUF = 1
      INTER = 0
C
C     RETURN
C
   60 RETURN
      END
      FUNCTION BENSGN(X,Y)
C+
C     RETURN THE VALUE OF X WITH THE SIGN OF Y IN BENSGN
C-
      IF (Y) 10,20,30
   10 BENSGN = -X
      RETURN
   20 BENSGN = 0.0
      RETURN
   30 BENSGN = X
      RETURN
      END
      SUBROUTINE BENUPK(NA,MA)
C+
C     UNPACK THE WORD NA, CONTAINING ICPW CHARACTERS, INTO THE
C     ARRAY MA.
C-
      DIMENSION MA(1)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      LA = NA
      IF (LA .GE. 0) GOTO 10
      LA = LA + IMAX + 1
   10 J = 1
      IF (ICORD .NE. 1) J = ICPW
      DO 20 I = 1,ICPW
      IA = LA / LSHIFT
      MA(J) = LA - IA * LSHIFT
      J = J + ICORD
   20 LA = IA
      IF (NA .GE. 0) GOTO 30
      MA(1) = MA(1) + LSHIFT / 2
   30 RETURN
      END
      SUBROUTINE CIRCLE(X,Y,J,R,BANG,SANG)
C+
C     I0 SOFTWARE - CIRCULATION INTERPOLATION COMMAND.
C-
      DIMENSION MA(4)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1              IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2              ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3              I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4              IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
C
C     CONVERT ANGLES FROM DEGREES TO RADIANS.
C
      ANG = BANG * 3.14159 / 180.0
      DANG = SANG * 3.14159 / 180.0
C
C     GET COORDINATES OF CENTRE OF CIRCLE.
C
      XC = X * PASX + BENSGN(0.5,X)
      XC = XC - AMOD(XC,1.0)
      YC = Y * PASY + BENSGN(0.5,Y)
      YC = YC - AMOD(YC,1.0)
C
C     ADJUST IF RELATIVE MOVE (J > 1)
C
      IF (J .GT. 1) XC = XC + RX
      IF (J .GT. 1) YC = YC + RY
C
C     GET MOVEMENT TO START OF ARC IN EXACT UNITS.
C
      XS = X + R * COS(ANG)
      XM = XS * PASX + BENSGN(0.5,XS)
      XM = XM - AMOD(XM,1.0) - XC
      IXM = IFIX(XM)
      CALL BENFRM(IXM,0,MA(1))
      YS = Y + R * SIN(ANG)
      YM = YS * PASY + BENSGN(0.5,YS)
      YM = YM - AMOD(YM,1.0) - YC
      IYM = IFIX(YM)
      CALL BENFRM(IYM,0,MA(3))
C
C     MOVE TO START POINT OF ARC.
C
      CALL TRAA(XS,YS,J)
C
C     SEND CIRCULAR INTERPOLATION COMMAND.
C     EMPTY BUFFER IF NOT ENOUGH ROOM.
C
      IF (IFREE .LT. 16) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,27,IPEN,0,4)
C
C     SEND X AND Y START.
C
      CALL BENPUT(MA(1),MA(2),MA(3),MA(4),4)
C
C     GET MOVEMENT TO END OF ARC IN EXACT UNITS,
C     OVERWRITE THE COMMON CURRENT PEN POISTION PARAMETERS
C     TO THIS POINT.
C
      XP = X + R * COS(ANG + DANG)
      RX = XP * PASX + BENSGN(0.5,XE)
      RX = RX - AMOD(RX,1.0)
      XM = RX - XC
      IXM = IFIX(XM)
      CALL BENFRM(IXM,0,MA(1))
      YP = Y + R * SIN(ANG + DANG)
      RY = YP * PASY + BENSGN(0.5,YE)
      RY = RY - AMOD(RY,1.0)
      YM = RY - YC
      IYM = IFIX(YM)
      CALL BENFRM(IYM,0,MA(3))
C
C     SEND X AND Y END.
C
      CALL BENPUT(MA(1),MA(2),MA(3),MA(4),4)
C
C     GET RADIUS IN EXACT UNITS, AND SUBTENDED ANGLE IN REQUIRED FORM.
C
      RS = ABS(R) * PASX + 0.5
      RS = RS - AMOD(RS,1.0)
      IR = IFIX(RS)
      CALL BENFRM(IR,0,MA(1))
      RANG = SANG
      RANG = RANG - AMOD(RANG,1.0)
      IANG = IFIX(RANG)
      CALL BENFRM(IANG,0,MA(3))
      CALL BENPUT(MA(1),MA(2),MA(3),MA(4),4)
      INTER = 0
      RETURN
      END
      SUBROUTINE CVAS(X1,Y1,X2,Y2)
      CALL POSS (X,Y,EX,EY,XA,YA)
      X2 = (X1 - XA) / EX
      Y2 = (Y1 - YA) / EY
      RETURN
      END
      SUBROUTINE CVSA(X1,Y1,X2,Y2)
      CALL POSS(X,Y,EX,EY,XA,YA)
      X2 = X1 * EX + XA
      Y2 = Y1 * EY + YA
      RETURN
      END
      SUBROUTINE CVSB(X1,Y1,X2,Y2,J)
      CALL POSS(X,Y,EX,EY,XA,YA)
      K = J + 1
      X2 = X1 * EX
      Y2 = Y1 * EY
      IF (K .GT. 2) GOTO 10
      X2 = X2 + XA
      Y2 = Y2 + YA
   10 RETURN
      END
      SUBROUTINE ECHEL(E1,E2,X1,X2)
C+
C
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
 
      EX = E1
      EY = E2
      XO = X1
      YO = X2
      RETURN
      END
      SUBROUTINE FACTOR(F)
      CALL TODRAW(F,B,B,IB,B,IB,5)
      RETURN
      END
      SUBROUTINE IBENA(I1,I2,I3)
C+
C     INITIALISATION ROUTINE
C
C     I1      - DUMMY PARAMETER.
C     I2      - DUMMY PARAMETER.
C     I3      - LOGICAL UNIT NUMBER OF OUTPUT DEVICE
C-
      DIMENSION IBITS(4),IDIC(64,2),IDIC1(64),IDIC2(64)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      EQUIVALENCE (IBITS(1),IBPW),(IBITS(2),IBPC),
     1 (IBITS(3),IBPF),(IBITS(4),IBPB)
      EQUIVALENCE (IDIC(1,1),IDIC1(1)),(IDIC(1,2),IDIC2(1))
C
C     FIRST DICTIONARY.
C     64 ASCII CHARACTERS.
C
      DATA IDIC1/
     1        32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
     2        42, 43, 44, 45, 46, 47, 48, 49, 50, 51,
     3        52, 53, 54, 55, 56, 57, 58, 59, 60, 61,
     4        62, 63, 64, 65, 66, 67, 68, 69, 70, 71,
     5        72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
     6        82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
     7        92, 93, 94, 95/
C
C     SECOND DICTIONARY.
C     32 BLANKS AND
C     32 EXTENDED ASCII CHARACTERS.
C
      DATA IDIC2/
     1        32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     2        32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     3        32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     4        32, 32, 96, 97, 98, 99,100,101,102,103,
     5       104,105,106,107,108,109,110,111,112,113,
     6       114,115,116,117,118,119,120,121,122,123,
     7       124,125,126,127/
C
C     SET COMMON VARIABLES
C
      NTAPE = I3
      XP = 0.0
      YP = 0.0
      NPOS = 0
      EX = 1.0
      EY = 1.0
      RX = 0.0
      RY = 0.0
C
C     GET VARIABLE DEFINITIONS SET WHEN SYSTEM GENERATED.
C
      CALL BENDEF(IBITS)
C
C     BUFFER CONTROL CONSTANTS
C
      ICPW = IBPW / IBPC
      IFPW = IBPW / IBPF
      ISHFT = 2 ** IBPF
      JSHFT = 2 ** IBPB
      LSHIFT = 2 ** IBPC
      IBMAX = ISHFT / 2
      IMAX = (2 ** (IBPW - 2) - 1) * 2 + 1
      N2MAX = 2 ** (IBPB * 2 - 2) - 1
C
      MBUF = 1
C     we have shorted this to 120 for the F8 board
C     in Paisley College
C      IBUF = 128
      IBUF = 120
      IF (IONOFF + ISPOOL .EQ. 0) IBUF = 1024
      NBUF = IBUF  / 2
      IFREE = IBUF - 4
      INTER = 0
      IPEN = 0
C
C     CONTROL CONSTANTS
C
      I77 = 2 ** IBPB - 1
      IF (IONOFF .NE. 0) GOTO 10
C
C     SEND FIRST ADDRESS BLOCK
C
      IBLOC = 0
      CALL PNUMA(0.0,0.0,IBLOC,0.0,0.0)
C
C     SEND DUMMY BUFFER A TOTAL OF 4 TIMES
C
   10 DO 15 II=1,1
      CALL BENPUT(I77,2,IPEN,0,4)
      CALL BENPUT(0,0,0,0,0)
   15 CONTINUE
C
C     DICTIONARIES.
C     IF IDICT = 0, SOFTWARE CHARACTERS ARE GENERATED, AND THE
C		    DICTIONARIES ARE NOT REQUIRED,
C     IF IDICT = 1, THE SECOND DICTIONARY IS FILLED WITH NULLS
C		    (STANDARD CHARACTER SET),
C     IF IDICT = 2, BOTH DICTIONARIES ARE REQUIRED
C		    (EXTENDED CHARACTER SET).
C
      IF (IDICT .EQ. 0) GOTO 60
      IF (IDICT .EQ. 2) GOTO 30
C
C     STANDARD CHARACTER SET.
C     BLANK OUT SECOND DICTIONARY.
C
      DO 20 I = 2,64
   20 IDIC2(I) = IDIC2(1)
C
C     SEND BOTH DICTIONARIES.
C
C     CHANGED TO SEND DICTIONARY TWICE
C
   30 CONTINUE
C      DO 55 K=1,2
      DO 50 I = 1,2
      CALL BENPUT(I77,6,0,0,2)
      DO 40 J = 1,64,4
      ICH1 = IDIC(J,I)
      ICH2 = IDIC(J + 1,I)
      ICH3 = IDIC(J + 2,I)
      ICH4 = IDIC(J + 3,I)
   40 CALL BENPUT(ICH1,ICH2,ICH3,ICH4,4)
      CALL BENPUT(I77,2,IPEN,0,4)
   50 CALL BENPUT(0,0,0,0,0)
C   55 CONTINUE
C
C     SET CHARACTER EQUIVALENTS FOR NUMERICAL SYMBOLS (USED IN NOMBA).
C
   60 DO 70 I = 1,10
   70 INCH(I) = IDIC1(I + 16)
      INCH(11) = IDIC1(14)
      INCH(12) = IDIC1(15)
      INCH(13) = IDIC1(38)
      RETURN
      END
      SUBROUTINE LINE(X,Y,N,IDIS,LSI,NRSYM)
      DIMENSION X(1),Y(1)
C     INCHES
C     DATA HCARAC/0.08/
C     CMS
      DATA HCARAC/.2/
C
C     (X,Y)   POINTS TO BE PLOTTED
C     N       NUMBER OF POINTS TO BE PLOTTED
C     LSI     LINE-SYMBOL-INDICATOR,LINE,SYMBOLS, OR COMBINED PLOT
C     NRSYM   EQUIVALENT OF THE SYMBOL TO BE USED
      ITR=N*IDIS+1
      ISC=ITR+IDIS
      IEND=ITR-IDIS
      XTR=X(ITR)
      YTR=Y(ITR)
      XSC=ABS(X(ISC))
      YSC=ABS(Y(ISC))
C
C     FIND SHORTEST DISTANCE
C
      CALL WHERE(AX,AY,AF)
       AX=AX*XSC+XTR
      AY=AY*YSC+YTR
      VISA=AMAX1(ABS(X(1)-AX),ABS(Y(1)-AY))
      VERSA=AMAX1(ABS(X(IEND)-AX),ABS(Y(IEND)-AY))
      IPEN=3
      ICODE=-1
C
C     SET INDICES FOR PLOT-DIRECTION AND SYMBOL-PLOTTING
C
      ML=IABS(LSI)
      IF(LSI)2,1,2
   1  ML=1
   2  IF(VISA-VERSA)4,4,3
   3  N1=IEND
      NSYMB=((N-1)/ML)*ML+ML-N+1
      IPL=-IDIS
      GO TO 5
   4  N1=1
      NSYMB=ML
      IPL=IDIS
   5  IF(LSI)6,7,8
   6  IPENL=3
      IODEL=-1
      ILS=1
      GO TO 9
   7  NSYMB=ISC
   8  IPENL=2
      IODEL=-2
      ILS=0
   9  DO 15 I=1,N
      XP=(X(N1)-XTR)/XSC
      YP=(Y(N1)-YTR)/YSC
      IF(NSYMB-ML)11,10,12
C
C     PLOT SYMBOL
C
  10  CALL SYMBOL(XP,YP,HCARAC,NRSYM,0.0,ICODE)
      NSYMB=1
      GO TO 14
  11  IF(ILS)13,12,13
  12  CALL PLOT(XP,YP,IPEN)
C
C     PLOT LINE
C
  13  NSYMB=NSYMB+1
  14  N1=N1+IPL
      ICODE=IODEL
  15  IPEN=IPENL
      RETURN
      END
      SUBROUTINE NEWPEN(NPEN)
      NPLUM=NPEN-1
      CALL PLUMA(NPLUM)
      RETURN
      END
      SUBROUTINE NOMBA(X,Y,J,FNP,NC,HX,HY,COSA,SINA)
C+
C     DRAW A REAL NUMBER.
C     THE NUMBER IS CONVERTED TO A CHARACTER STRING AND SUPPLIED TO
C     PCARA IN ARRAY ICR, ONE CHARACTER PER WORD.
C-
      DIMENSION ICR(20)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      A10 = 1./ALOG(10.)
      FPV = ABS(FNP)
      NNC = 0
      N = NC
      MAX = 9
      IF (N .GT. MAX) N = MAX
      IF (N .LT. - MAX) N = - MAX
      IF (FNP) 20,10,30
   10 NNC = NNC + 1
      ICR(NNC) = INCH(1)
      IF (N .EQ. - 1) GOTO 110
      NNC = NNC + 1
      ICR(NNC) = INCH(12)
      GOTO 110
   20 NNC = NNC + 1
      ICR(NNC) = INCH(11)
   30 Z = 0.5
      IF (N + 1) 70,50,40
   40 Z = 0.5 * 10.0 ** ( - N)
   50 FPV = FPV + Z
      Z = ALOG(FPV) * A10
      I = Z + 1.0
      IF (I .LT. 1) I = 1
      ILP = I + N
      IF (N .LT. 0) ILP =ILP + 1
      DO 60 K = 1,ILP
      U = 10.0 ** (I - K)
      L = FPV / U
      IF (L .GT. 9) L = 9
      NNC = NNC + 1
      ICR(NNC) = INCH(L + 1)
      FPV = FPV - FLOAT(L) * U
      IF (K .NE. I)  GOTO 60
      IF (N .LT. 0) GOTO 60
      NNC = NNC + 1
      ICR(NNC) = INCH(12)
   60 CONTINUE
      GOTO 110
   70 NNC = NNC + 1
      ICR(NNC) = INCH(1)
      NNC = NNC + 1
      ICR(NNC) = INCH(12)
      N = - N
      V = ALOG(FPV) * A10
      L = V + 1.0
      IF (L .LT. 0) L = L - 1
   80 W = FPV + 0.5 * 10.0 ** (L - N)
      Z = ALOG(W) * A10
      I = L
      L = Z + 1.0
      IF (L .LT. 0) L = L - 1
      IF (I .NE. L) GOTO 80
      FPV = W
      DO 90  K = 1,N
      U = 10.0 ** (I - K)
      L = FPV / U
      IF (L .GT. 9) L = 9
      NNC = NNC + 1
      ICR(NNC) = INCH(L + 1)
   90 FPV = FPV - FLOAT(L) * U
      NNC = NNC + 1
      ICR(NNC) = INCH(13)
      IF (I.GE. 0) GOTO 100
      NNC = NNC + 1
      ICR(NNC) = INCH(11)
  100 L = IABS(I) / 10
      NNC = NNC + 1
      ICR(NNC) = INCH(L + 1)
      L = IABS(I) - L * 10
      NNC = NNC + 1
      ICR(NNC) = INCH(L + 1)
  110 JP =  J - 10
      CALL PCARA(X,Y,JP,ICR,NNC,HX,HY,COSA,SINA)
      RETURN
      END
      SUBROUTINE NOMBS(X,Y,J,FNP,NC,HX,HY,COSA,SINA)
C+
C
C-
      CALL CVSB(X,Y,XA,YA,J)
      CALL NOMBA(XA,YA,J,FNP,NC,HX,HY,COSA,SINA)
      RETURN
      END
      SUBROUTINE NUMBER(XK,YK,SIZE,GETAL,THETA,NDEC)
C
C
C        (XK,YK)   LOWER LEFT-HAND CORNER OF THE FIRST DIGIT OR SIGN
C        SIZE      HEIGHT OF THE DIGIT(S)
C        GETAL     FLOATING POINT NUMBER , WHICH IS TO BE PLOTTED
C        THETA     ANGLE OF THE NUMBER WITH THE POSITIVE ABSCISSA
C                  (IN DEGREES)
C        NDEC%0    NUMBER OF DIGITS TO THE RIGHT-HAND OF THE DECIMAL
C                  POINT , WHICH ARE TO BE PLOTTED
C        NDEC=0    ONLY INTEGER PORTION AND DECIMAL POINT ARE PLOTTED
C        NDEC=-1   ONLY INTEGER PORTION IS PLOTTED
C        NDEC^-1   ABS(NDEC)-1 DIGITS ARE TRUNCATED FROM THE INTEGER
C                  PORTION
C
      DIMENSION IDA(10)
      DATA IDA/1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9/
      DATA MS/1H-/,MP/1H./
      X=XK
      Y=YK
      SP=999.0
      GTL=GETAL
      N=NDEC
C
C        CHECK@NUMBER OF DIGITS LESS THAN 10
C
      MAXD=9
      IF(N.GT.MAXD) N=MAXD
      IF(-N.GT.MAXD) N=-MAXD
      IF(GTL.GE.0.0) GO TO 10
C
C        PLOT MINUS ,
C
C     CALL SYMBOL(X,Y,SIZE,1H-,THETA,1)
      CALL SYMBOL(X,Y,SIZE,MS ,THETA,1)
      X=SP
      Y=SP
   10 M=-N
      IF(N.GE.0) GO TO 20
      M=M-1
   20 GTL=ABS(GTL)+0.5*10.0**M
      IP=ALOG10(GTL)+1.0
      ILP=IP
      IF(N.GE.-1) GO TO 30
      ILP=ILP+N+1
   30 IF(ILP.GT.0) GO TO 40
C
C        PLOT ZERO , IF ABS(GETAL)^1
C
      CALL SYMBOL(X,Y,SIZE,IDA(1),THETA,1)
      X=SP
      Y=SP
      GO TO 60
   40 DO 50  JJ=1,ILP
      KK=GTL*10.0**(JJ-IP)
C      KA=KK +   RANG DU ZERO DANS LA TABLE DES CARACTERES
      KA=IDA(KK+1)
C
C        PLOT DIGITS AT THE LEFT-HAND OF THE DECIMAL POINT
C
      CALL SYMBOL(X,Y,SIZE,KA,THETA,1)
      GTL=GTL-FLOAT(KK*10**(IP-JJ))
      X=SP
   50 Y=SP
   60 IF(N.LT.0) RETURN
C
C        PLOT DECIMAL POINT
C
      CALL SYMBOL(X,Y,SIZE,MP ,THETA,1)
      IF(N.LE.0) RETURN
      DO 70  JJ=1,N
      GTL=GTL*10.0
      KK=GTL
      KA=IDA(KK+1)
C
C        PLOT DIGITS AT THE RIGHT-HAND OF THE DECIMAL POINT
C
      CALL SYMBOL(X,Y,SIZE,KA,THETA,1)
   70 GTL=GTL-FLOAT(KK)
      RETURN
      END
      SUBROUTINE OFFSET(X0,XFACT,Y0,YFACT)
      CALL TODRAW(X0,XFACT,Y0,I,YFACT,IB,3)
      RETURN
      END
      SUBROUTINE PCARA(X,Y,M,NLIS,NC,HX,HY,COSA,SINA)
C+
C     HARDWARE CHARACTER GENERATION.
C-
      DIMENSION NLIS(1),MA(8),LA(4),OLD(4),XA(4)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1              IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2              ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3              I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4              IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      DATA OLD/4*1000.0/
      N  =  M
      ICAR  =  ICPW
C
C     IS CALL FROM NOMBA ?
C
      IF (M .GE. 0) GOTO 10
C
C     YES.
C
      N = 10 + M
      ICAR = 1
C
C     MOVE PEN TO POINT (X,Y).
C
   10 CALL TRAA(X,Y,N)
      NW = (NC + ICAR - 1) / ICAR
C
C     ANY CHARACTERS TO DRAW ?
C
      IF (NC .LE. 0) GOTO 150
C
C     YES: CHECK FOR CHANGE IN SIZE AND/OR ORIENTATION.
C
      XCM = HX * COSA
      YCM = HX * SINA
      XD = PASX * XCM
      YD = PASY * YCM
      IF (HX .NE. OLD(1)) GOTO 20
      IF (HY .NE. OLD(2)) GOTO 20
      IF (COSA .NE. OLD(3)) GOTO 20
      IF (SINA .NE. OLD(4)) GOTO 20
      GOTO 60
C
C     THEY'VE CHANGED. CHECK IF THE NEW VALUES ARE VALID.
C
   20 XA(1) = COSA * HX * PASX
      XA(2) = - SINA * HY * PASX
      XA(3) = SINA * HX * PASX
      XA(4) = COSA * HY * PASX
      DO 30 I = 1,4
      A = XA(I) * 4.0 / 3.0
      IA = IFIX(A + BENSGN(0.5,A))
      IF (IABS(IA) .GT. N2MAX) GOTO 40
      J = I * 2 - 1
   30 CALL BENFRM(IA,0,MA(J))
      OLD(1) = HX
      OLD(2) = HY
      OLD(3) = COSA
      OLD(4) = SINA
      GOTO 50
C
C     CHARACTERS ARE TOO BIG. MOVE TO END OF STRING.
C
   40 CALL TRAA(XCM * NC,YCM * NC,0)
      GOTO 150
C
C     SEND HEIGHT COMMAND, EMPTY BUFFER IF INSUFFICIENT ROOM.
C
   50 IF (IFREE .LT. 12) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,3,IPEN,0,4)
      CALL BENPUT(MA(1),MA(2),MA(3),MA(4),4)
      CALL BENPUT(MA(5),MA(6),MA(7),MA(8),4)
C
C     SET MAXIMUM NUMBER OF CHARACTERS ALLOWED IN EACH STRING.
C
   60 IMX = -1
      IMY = -1
      IF(ABS(XD) .LT. 1.0) IMX = N2MAX
      IF(ABS(YD) .LT. 1.0) IMY = N2MAX
      IF(ABS(XD) .GE. 1.0) IMX = IABS(N2MAX / IFIX(XD))
      IF(ABS(YD) .GE. 1.0) IMY = IABS(N2MAX / IFIX(YD))
      IF (IMX .EQ. -1) IMX = IMY
      IF (IMY .EQ. -1) IMY = IMX
C
C     SEND CHARACTER COMMAND, EMPTY BUFFER IF INSUFFICIENT ROOM.
C
      IF (IFREE .LT. 8) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,4,IPEN,0,4)
      JMAX = MIN0(IMX,IMY,IFREE)
      ISENT = 0
      ILA = 0
      IW = 0
      IC = 0
C
C     LOOP FOR CHARACTERS.
C
      DO 140 I = 1,NW
      J = 1
      IW = IW + 1
      NA = NLIS(IW)
      MA(1) = NA
      IF (ICAR .EQ. 1) GOTO 80
      J = I * ICPW
      IF (J .GT. NC) J = NC
      J = MOD(J + ICPW - 1,ICPW) + 1
      CALL BENUPK(NA,MA)
   80 DO 130 K = 1,J
      ILA = ILA + 1
      LA(ILA) = MA(K)
      ISENT = ISENT + 1
      IC = IC + 1
      IF (IC .GE. NC) GOTO 90
      IF (ISENT .EQ. JMAX) GOTO 90
      IF (ILA .LT. 4) GOTO 130
   90 CALL BENPUT(LA(1),LA(2),LA(3),LA(4),ILA)
      ILA = 0
      IF (MBUF .EQ. 1) GOTO 100
      IF (IC .GE. NC) GOTO 100
      IF (ISENT .EQ. JMAX) GOTO 100
      GOTO 130
C
C     BUFFER SENT, SET AND SEND ADJUSTMENT VECTOR.
C
  100 LA(1) = IFIX(XD * FLOAT(ISENT) + BENSGN(0.5,XD))
      RX = RX + FLOAT(LA(1))
      XP = XP + XCM * FLOAT(ISENT)
      LA(3) = IFIX(YD * FLOAT(ISENT) + BENSGN(0.5,XD))
      RY = RY + FLOAT(LA(3))
      YP = YP + YCM * FLOAT(ISENT)
      CALL BENFRM(LA(1),0,LA(1))
      CALL BENFRM(LA(3),0,LA(3))
      IF (IFREE .LT. 8) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,7,IPEN,0,4)
      CALL BENPUT(LA(1),LA(2),LA(3),LA(4),4)
      ISENT = 0
      IF (IC .GE. NC) GOTO 130
C
C     SEND CHARACTER COMMAND, EMPTY BUFFER IF INSUFFICIENT ROOM.
C
      IF (IFREE .LT. 8) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,4,IPEN,0,4)
      JMAX = MIN0(IMX,IMY,IFREE)
  130 CONTINUE
  140 CONTINUE
C
C     SET VECTOR SWITCH.
C
      INTER = 0
  150 RETURN
      END
 
      SUBROUTINE PCARS(X,Y,J,NLIS,NC,HX,HY,COSA,SINA)
C+
C
C-
      DIMENSION NLIS(1)
      CALL CVSB(X,Y,XA,YA,J)
      CALL PCARA(XA,YA,J,NLIS,NC,HX,HY,COSA,SINA)
      RETURN
      END
      SUBROUTINE PLOT(X,Y,IPEN)
      CALL TODRAW(X,Y,BID,IBID,BID,IPEN,2)
      RETURN
      END
      SUBROUTINE PLOTS(IBUF,LBUF,ND)
      CALL IBENA(0,0,ND)
      NB=0
      CALL PNUMA(0.,0.,NB,0.,0.)
      CALL TODRAW(B,B,B,IB,B,IB,1)
      RETURN
      END
      SUBROUTINE PLUMA(JPEN)
C+
C     CHANGE THE PEN.
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      IF (JPEN .EQ. IPEN) GOTO 10
      IF (JPEN .LT. 0) GOTO 10
      IF (JPEN .GT. IOUT(5)) GOTO 10
      IPEN = JPEN
      IF (IFREE .LT. 8) CALL BENPUT(0,0,0,0,0)
      CALL BENPUT(I77,2,IPEN,0,4)
      INTER = 1
   10 RETURN
      END
      SUBROUTINE PNUMA(X1,Y1,IBLO,X2,Y2)
C+
C     REDEFINITION OF ORIGIN
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
C
C     MOVE PEN TO NEW ORIGIN POINT
C
      CALL TRAA(X1,Y1,0)
C
C     SET COMMON VARIABLES
C
      XP = X2
      YP = Y2
      RX = XP * PASX
      RY = YP * PASY
      NPOS = 0
      IF (MBUF .LE. 1) GOTO 10
C
C     EMPTY CURRENT BUFFER.
C     OFFLINE: ALWAYS AS REQUESTED, ONLINE: ONLY IF IBLO = 9999.
C
      IF (IONOFF .NE. 0 .AND. IBLO .NE. 9999) GOTO 30
      CALL BENPUT(0,0,0,0,0)
   10 IF (IONOFF .NE. 0) GOTO 20
C
C     SEND ADDRESS BLOCK.
C
      MBUF = 1
      IK = IBLOC
      IF (IBLO .EQ. 9999) IK = 9999
      I1 = IK / 1000
      IK = IK - I1 * 1000
      I2 = IK / 100
      IK = IK - I2 * 100
      I3 = IK / 10
      I4 = IK - I3 * 10
      CALL BENPUT(I77,48,48,48,4)
      CALL BENPUT(I1,I2,I3,I4,4)
      CALL BENPUT(I4,I3,I2,I1,4)
      CALL BENPUT(48,48,48,I77,4)
      CALL BENPUT(0,0,0,0,-1)
C
C     INCREMENT BLOCK COUNTER.
C
      IBLOC = IBLOC + 1
   20 IF (IBLO .NE. 9999) GOTO 30
C
C     CLOSE PLOT DEVICE.
C
      CALL BENOUT(3)
   30 RETURN
      END
      SUBROUTINE POSA(X,Y)
C+
C     FIND CURRENT PEN POSITION - RETURN IN (X,Y)
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
      X = XP
      Y = YP
      RETURN
      END
      SUBROUTINE POSS(A1,A2,B1,B2,C1,C2)
C+
C     FIND CURRENT PEN POSITION.
C-
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
 
      A1 = (XP - XO) / EX
      A2 = (YP - YO) / EY
      B1 = EX
      B2 = EY
      C1 = XO
      C2 = YO
      RETURN
      END
       SUBROUTINE SCALE(X,XLEN,N,IDIS)
      DIMENSION X(1),SCVAL(7)
      DATA SCVAL/1.0,2.0,4.0,5.0,8.0,10.,20./
C
C     X       ARRAY TO BE SCALED
C     XLEN    LENGHTINCM AVAILABLE
C     N       NUMBER OF POINTS TO BE SCALED
C     IDIS    SUBSCRIPT INCREMENT
      TRUNC=0.01
      IPL=IABS(IDIS)
      NEND=N*IPL
      YN=X(1)
      YX=YN
C
C     FIND MIN AND MAX VALUES
C
      DO 20 I=1,NEND,IPL
      Y=X(I)
      IF(YN-Y)10,20,5
  5   YN=Y
      GO TO 20
  10   IF(YX-Y)15,20,20
  15  YX=Y
  20  CONTINUE
      IF(YN)25,30,30
  25  TRUNC=TRUNC-1.0
C
C     FIND SCALING FACTOR
C
  30  DX=(YX-YN)/XLEN
      IF(DX)95,90,35
  35  I=ALOG10(DX)+1000.0
      EXP=10.0**(I-1000)
       DX=DX/EXP-0.01
      DO 40 I=1,6
      IA=I
      IF(SCVAL(I)-DX)40,45,45
 40   CONTINUE
  45  DX=SCVAL(IA)*EXP
      YIN=DX*AINT(YN/DX+TRUNC)
      XVPOS=YIN+(XLEN+0.01)*DX
C
C     CHECK  PLOTLENGTH
C
      IF(XVPOS-YX)50,60,60
  50  YIN=EXP*AINT(YN/EXP+TRUNC)
      XVPOS=YIN+(XLEN+0.01)*DX
      IF(XVPOS-YX)55,60,60
  55  IA=IA+1
      GO TO 45
  60  YIN=YIN-AINT((XLEN+(YIN-YX)/DX)*0.5)*DX
      IF(YN*YIN)65,65,70
  65  YIN=0.0
  70  IF(IDIS)75,75,80
  75  YIN=YIN+AINT(XLEN+0.5)*DX
      DX=-DX
  80  NEND=NEND+1
      X(NEND)=YIN
      NEND=NEND+IPL
      X(NEND)=DX
      RETURN
  90  DX=ABS((2.0*YN)/XLEN)+1.0
      GO TO 35
  95  IF(IDIS)100,100,105
 100  YIN=YX
      GO TO 80
 105  DX=-DX
      YIN=YN
      GO TO 80
      END
       SUBROUTINE SPESYM(X,Y,J,N,HX,HY,COSA,SINA)
C
C   14  CARACTERES CENTRES
C
      DIMENSION IDC( 93),IOC(14),EPD(7)
      DATA  IOC          /1,8,19,24,29,34,40,46,51,58,63,76,85,91/
      DATA    EPD       /0.,0.166667,0.333333,0.5,-0.1666667,-0.33333,
     1-0.5/
      DATA    IDC         /103,163,166,136,133,103,0,
     1                     103,143,161,164,146,116,134,131,113,103,0,
     2                     103,165,135,103,0,
     3                     3,106,30,160,0,
     4                     63,136,33,166,0,
     5                     103,160,106,130,103,0,
     6                     6,103,160,130,103,0,
     7                     36,163,133,166,0,
     8                     40,110,36,166,133,163,0,
     9                     133,63,100,106,0,
     A                     133,22,152,163,52,155,166,55,125,136,25,122,
     B0,
     C                     33,166,6,103,63,136,30,160,0,
     D                     33,166,136,163,133,0,
     E                     103,106,0/
       CALL TRAA(X,Y,J)
      IND=N+1
      IF(IND.LE.0) GO TO 24
      IF(IND.GT.14)GO TO 24
      CALL POSA(XA,YA)
   15 IPD=IOC(IND)
   17 IDP=IDC(IPD)
      IND=IDP
      IF(IDP-99)20,20,22
   22 IP=1
      IDP=IDP-100
      GOTO21
   20 IP=0
   21 NX=IDP/10
      NY=IDP-NX*10
      FX=EPD(NX+1)*HX
      FY=EPD(NY+1)*HY
      X1=XA+FX*COSA-FY*SINA
      Y1=YA+FX*SINA+FY*COSA
      CALL TRAA(X1,Y1,IP)
       IF(IND)23,24,23
   23 IPD=IPD+1
      GO TO 17
  24    RETURN
      END
 
C**********************************************************************C
C                                                      								        C
C     BENSON ELECTRONICS LIMITED,			                           	       C
C     Techno House,					                                  	       C
C     Redcliffe Way,			                                  			       C
C     Bristol BS1 6NH,                                    						       C
C     England.                                              				       C
C     Telephone: (0272) 211501                                         C
C     Telex:	 444597 Benson                              					       C
C								                                            C
C**********************************************************************C
C**********************************************************************C
C			                                               					       C
C		     PROPRIETARY INFORMATION                        			       C
C                                                                      C
C     The contents of this software and listings are proprietary,      C
C     and are not to be disclosed or released to other parties         C
C     without the written consent of Benson Electronics Limited.       C
C                                                                      C
C**********************************************************************C
C**********************************************************************C
C                                                                      C
C     BENSON STANDARD DRAFTING PACKAGE (BSDP) - LEVEL 1        	       C
C                                                                      C
C**********************************************************************C
      SUBROUTINE SYMBOL(X,Y,SIZE,IBCD,THETA,N)
      DIMENSION IBCD(1)
      CALL TODRAW (X,Y,SIZE,IBCD,THETA,N,6)
      RETURN
      END
      SUBROUTINE TODRAW(X,Y,Z,I,T,N,K)
      DIMENSION I(1)
      save pouce,fact,nbloc,fa,xoff,yoff,xfact,yfact
      IM = I(1)
      GO TO (31,32,33,34,35,36),K
C    K=2   PLOT
   32 IKOD=IABS(N)
      IF(IKOD-10)2,10,10
    2 CONTINUE
      IF(IKOD-2)3,4,5
    3 CONTINUE
      MOJ=NPOS
      GO TO 6
    4 MOJ=1
      GO TO 6
    5 MOJ=0
    6 XA=X*FA
      YA=Y*FA
   61 NPOS=MOJ
      CALL TRAA(XA,YA,MOJ)
    7 IF(N)8,9,9
   8  NBIDON=NBLOC
      IF(NBIDON.EQ.999) NBIDON=9999
      GO TO 68
 9     RETURN
   10 IF(IKOD-20)11,20,20
   11 IF(IKOD-12)12,13,14
   12 MOJ=NPOS
      GO TO 15
   13 MOJ=1
      GO TO 15
   14 MOJ=0
   15 XA=((X-XOFF)/XFACT)*FA
      YA=((Y-YOFF)/YFACT)*FA
      GO TO 61
   20 IF(IKOD-30)21,30,30
   21 IF(IKOD-22)22,23,24
   22 MOJ=NPOS
      GO TO 25
   23 MOJ=1
      GO TO 25
   24 MOJ=0
   25 XA=X*FA
      YA=Y*FA
      NPOS=MOJ
      CALL TRAA (XA,YA,MOJ)
C     NBIDON=-1000
      NBIDON=0
  68  CALL POSA (XR,YR)
      CALL PNUMA(XR,YR,NBIDON,0.,0.)
      NBLOC=NBIDON
      RETURN
   30 NBLOC=IKOD
      MOJ=0
      XA=X*FA
      YA=Y*FA
      NPOS=MOJ
      CALL TRAA (XA,YA,MOJ)
      GO TO 8
C     K=1   PLOTS
   31 CONTINUE
      NPOS=0
C      POUCE=2.54
      POUCE=1.0
      FACT=1.
      NBLOC=1
      FA=POUCE*FACT
      XOFF=0.
      YOFF=0.
      XFACT=1.
      YFACT=1.
      RETURN
C      K=4   WHERE
   34 CALL POSA (X,Y)
      X=X/FA
      Y=Y/FA
      Z=FACT
      RETURN
C     K=5   FACTOR
   35 FACT=X
      FA=FACT*POUCE
      RETURN
C     K=3    OFFSET
   33 XOFF=X
      XFACT=Y
      YOFF=Z
      YFACT=T
      RETURN
C      K=6    SYMBOL
   36 TPI=T*3.1415927/180.
      SINA=SIN(TPI)
      COSA=COS(TPI)
      CALL POSA(XD,YD)
      IF(X-999.)42,41,42
   42 IF(Y-999.)44,41,44
  44  XD=X*FA
      YD=Y*FA
  41  HX=Z*FA*6./7.
      HY=Z*FA
      IF(N)46,60,45
   45 CALL PCARA(XD,YD,0,I,N,HX,HY,COSA,SINA)
      RETURN
   46 IF(N+1)48,47,47
   47 J=0
      GO TO 49
   48 J=1
   49 CALL SPESYM(XD,YD,J,I,HY,HY,COSA,SINA)
      RETURN
C     N=0   DESSINER UN CARACTERE  (CADRE A DROITE DANS LE MOT)
 60   CONTINUE
      CALL PCARA(XD,YD,0,IM,1,HX,HY,COSA,SINA)
      RETURN
      END
      SUBROUTINE TPARM(X,Y)
C     This routine is there to dummy for the one in
C     TIM100 which this is a direct replacment of
 
      REAL X,Y
      END
*
      SUBROUTINE TRAA(X,Y,M)
C+
C     MOVE THE PEN TO POINT X,Y WITH PEN MODE M
C-
      DIMENSION LA(4)
      COMMON/ZZZBEN/XP,YP,RX,RY,PASX,PASY,EX,EY,XO,YO,INTER,
     1 IBUF,MBUF,NBUF,IBLOC,NTAPE,IPEN,IFPW,ICPW,
     2 ISHFT,JSHFT,KSHFT,LSHIFT,IMAX,IBMAX,N2MAX,
     3 I77,IFREE,INCH(13),IONOFF,ISPOOL,IDEV,IUNIT,
     4 IBIAS,IDICT,IE2022,ICORD,IOUT(5),IES(512)
C
C     SAVE PARAMETERS.
C
      XX = X
      YY = Y
      J = M
      IF (J .LE. 1) GOTO 10
C
C     RELATIVE MOVE.
C
      XP = XP + XX
      YP = YP + YY
      J = J - 2
      GOTO 20
C
C     ABSOLUTE MOVE.
C
   10 XP = XX
      YP = YY
C
C     SET PEN CODE:
C     NPOS = 0: PEN DOWN
C     NPOS = 2: PEN UP
C
   20 NPOS = 0
      IF (J .NE. 0) NPOS = 2
C
C     SET NEW POSITION AND MOVEMENT:
C
C     1) RXN (RYN) IS SET TO THE NEW ABSOLUTE POSITION IN STEPS,
C     2) RJX (RJY) IS SET TO THE MOVE TO THE NEW POSITION IN STEPS,
C     3) RX (RY) IS RESET TO THE NEW ABSOLUTE POSITION IN STEPS.
C
      RXN = XP * PASX + BENSGN(0.5,XP)
      RXN = RXN - MOD(RXN,1.0)
      RJX = RXN - RX
      RX = RXN
      RYN = YP * PASY + BENSGN(0.5,YP)
      RYN = RYN - MOD(RYN,1.0)
      RJY = RYN - RY
      RY = RYN
C
C     MAKE RJX MAXIMUM MOVE, RJY MINIMUM MOVE.
C
      IXX = 1
      IYY = 3
      NXX = 0
      NYY = NPOS
      IF (ABS(RJX) .GE. ABS(RJY)) GOTO 50
      R = RJX
      RJX = RJY
      RJY = R
      IXX = 3
      IYY = 1
      NXX = NPOS
      NYY = 0
C
C     HOW MANY MAXIMUM VECTORS ARE REQUIRED FOR THE MOVE ?
C
   50 IF (RJX .EQ. 0.0) GOTO 100
      IVECT = IFIX(RJX / FLOAT(N2MAX))
      INEND = RJX - FLOAT(N2MAX) * FLOAT(IVECT)
      IVECT = IABS(IVECT)
      IF (INEND .NE. 0) IVECT = IVECT + 1
C
C     SET THIS MOVE.
C
   60 NTX = IFIX(RJX / FLOAT(IVECT))
      NTY = IFIX(RJY / FLOAT(IVECT))
C
C     GET IN BENSON FORMAT.
C
      CALL BENFRM(NTX,NXX,LA(IXX))
      CALL BENFRM(NTY,NYY,LA(IYY))
C
C     VECTORS SWITCHED ON ?
C
      IF (INTER .NE. 0) GOTO 90
C
C     NO, WE MUST SEND COMMAND.
C
      IF (IFREE .GE. 8) GOTO 80
C
C     NOT ENOUGH ROOM: EMPTY BUFFER.
C
   70 CALL BENPUT(0,0,0,0,0)
C
C     SEND VECTOR COMMAND.
C
   80  CONTINUE
       CALL BENPUT(I77,2,IPEN,0,4)
      INTER = 1
C
C     SEND THIS MOVE.
C
   90 IF (IFREE .LT. 4) GOTO 70
      CALL BENPUT(LA(1),LA(2),LA(3),LA(4),4)
C
C     CHECK IF VECTOR COMPLETE: IF NOT, REPEAT FOR NEXT SEGMENT.
C
      IVECT = IVECT - 1
      IF (IVECT .EQ. 0) GOTO 100
C
C     REPEAT VECTORS: RESET REMAINING MOVEMENT.
C
      RJX = RJX - FLOAT(NTX)
      RJY = RJY - FLOAT(NTY)
      GOTO 60
  100 CONTINUE
      END
      SUBROUTINE TRAS(X,Y,M)
C+
C
C-
      J = M
      CALL CVSB(X,Y,XA,YA,J)
      CALL TRAA(XA,YA,J)
      RETURN
      END
      SUBROUTINE WHERE(X,Y,FACT)
      CALL TODRAW (X,Y,FACT,IB,B,IB,4)
      RETURN
      END
