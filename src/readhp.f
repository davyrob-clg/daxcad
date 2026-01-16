C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 readhp.f   */
C
C     This source file contains routines which have been modified
C     for the X conversion for daxcad. At the head of each
C     routine a comment exists to nindicate the source file form whence it
C     was extracted. This file is under sr10 SCCS
C
      SUBROUTINE FILIN(FILID,BUFFER,BUFLEN,ST)
C     ========================================
C1    VARYPE            I4    C*(*)   I4   I4
C1    IOSTAT            I       O     O    O
C
C2    Reads data from an HP plot file
C2  
C2  
C2    Arguments:-
C2  
C2    FILID               ->              F77 unit number
C2    BUFFER              ->              Buffer supplied to put data into
C2    BUFLEN              ->              Amount of data read
C2  
C2  
C2    Error Returns:
C2  
C2    1           ->          Error in reading plot file
C2  
C2  



      INTEGER*4 ST,BUFLEN,NLEN
      INTEGER*4 FILID
      CHARACTER*(*) BUFFER
      EXTERNAL NLEN

 10   CONTINUE
C
      READ(UNIT=FILID,FMT='(A)',END=999) BUFFER
      BUFLEN=NLEN(BUFFER)
      ST=0
C
      RETURN
 999  CONTINUE
      ST=1
      RETURN
      END
cC
      SUBROUTINE HLINE(PD,PA,IX,IY,OK)
C     ================================
C1      iostat         I, I, I, I
C1                     L  L  I4 I4
      include 'include/pendat.inc'
      include 'include/masti.inc'
C
      REAL DISTXY,TX,TY
      LOGICAL PD,PA,OK
      INTEGER*2 TMIP1,TFONT
      INTEGER*4 IX,IY
      EXTERNAL DISTXY
 
      IF ( .NOT.PD ) THEN
        IF ( PA ) THEN
          AXP=IX
          AYP=IY
        ELSE
          AXP=AXP+IX
          AYP=AYP+IY
        END IF
        OK=.TRUE.
      ELSE
        IF ( PA ) THEN
          TX=IX
          TY=IY
        ELSE
          TX=AXP+IX
          TY=AYP+IY
        END IF
        TFONT=1
        IF ( DISTXY(AXP,AYP,TX,TY).GT.0.0 ) THEN
          CALL DEWC03(AXP*SCA,AYP*SCA,TX*SCA,TY*SCA,
     +                TFONT,CLAYER,TMIP1,OK)
        END IF
        AXP=TX
        AYP=TY
      ENDIF
 
      END
 
      SUBROUTINE HPLINE(IX,IY,OK)
C     ===========================
 
      include 'include/redboard.inc'
C
      INTEGER*4  IX,IY
      LOGICAL OK
 
      CALL HPNUM(IX,OK)
      IF ( .NOT.OK ) RETURN
      CALL HPNUM(IY,OK)
C
      END
*
 
      SUBROUTINE HPNUM(IX,OK)
C     ===========================
 
      include 'include/redboard.inc'
C
      INTEGER*2 TP,IC
      INTEGER*4  IX
      CHARACTER*1 CHR,VAL*20
      LOGICAL OK
 
      VAL='-+0123456789'
      OK=.TRUE.
      TP=CURP+1
      OK=TP.LE.EOFL
      IF ( .NOT.OK ) THEN
        CURP=CURP+1
        RETURN
      END IF
 301  IF ( CURP.LT.EOFL ) THEN
        CURP=CURP+1
        CHR=RDLINE(CURP:CURP)
        IF ( INDEX(VAL(1:12),CHR).GT.0) GOTO 301
       OK=.NOT.(CURP-TP.LT.1)
      ELSE IF ( CURP.EQ.EOFL ) THEN
        OK=(CURP.GE.TP)
      END IF
C
      IF ( .NOT.OK ) RETURN
      IC=CURP
      IF ( CURP.LT.EOFL ) IC=IC-1
      CALL IVALU(RDLINE(TP:IC),IX,OK)
C
      END
 
      SUBROUTINE HPNUMR(RX,OK)
C     ===========================
 
      include 'include/redboard.inc'
C
      INTEGER*2 TP,IC
      REAL RX
      CHARACTER*1 CHR,VAL*20
      LOGICAL OK
 
      VAL='.-+0123456789'
      OK=.TRUE.
      TP=CURP+1
      OK=TP.LE.EOFL
      IF ( .NOT.OK ) THEN
        CURP=CURP+1
        RETURN
      END IF
 301  IF ( CURP.LT.EOFL ) THEN
        CURP=CURP+1
        CHR=RDLINE(CURP:CURP)
        IF ( INDEX(VAL(1:13),CHR).GT.0) GOTO 301
        OK=.NOT.(CURP-TP.LT.1)
      ELSE IF ( CURP.EQ.EOFL ) THEN
        OK=(CURP.GE.TP)
      END IF
      IF ( .NOT.OK ) RETURN
      IC=CURP
      IF ( CURP.LT.EOFL ) IC=IC-1
 
      CALL RVALU(RDLINE(TP:IC),RX,OK)
C
      END
*
*
      SUBROUTINE RDHPGL(GFILE)
C     ========================
C
C    A program to translate the content of a Gerber
C    photo-plotter file into DAXCAD format.
C
      include 'include/redboard.inc'
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 IX,IY,IA,ID
      INTEGER*4 SUNIT
      INTEGER*4 GUNIT,ST,I,J,WL,NLEN
      INTEGER*2 TMIP1,P,TLAY,TFONT,
     +          N,PP,TP,LL,IDUM,TCOL,TJST
      REAL X(2),Y(2),CANG,SANG,RAD,RADIUS,EANG,
     +     PI,DISTXY,RA,RX,RY,DEG
      LOGICAL PDOWN,PABS,OK,EOS,TOK
      CHARACTER*80 BLOCK,GFILE*(*),VAL*20,CHR*1,CMMD*20,ESTRG
      EXTERNAL CANG,RAD,PI,DISTXY,DEG,NLEN
C
      SCA=0.001*2.5/DBUFAC
      PDOWN=.FALSE.
      PABS=.TRUE.
      TFONT=1
      TLAY=0
      TCOL=7
      TJST=1
      AXP=-32000
      AYP=-32000
      VAL='-+0123456789'
C           123456789
      CMMD=';PACLSDIV'//CHAR(27)
      CALL SOPEN(GFILE,SUNIT)
C
      N=1
      ST=0
      CURP=1
      EOFL=0
 101  CONTINUE
      IF ( CURP.GT.EOFL ) THEN
        IF ( ST.EQ.0 ) THEN
          RDLINE='
     +                    '
          CALL  FILIN(SUNIT,BLOCK,EOFL,ST)
          IF ( ST.GT.0) GOTO 200
          CURP=1
          RDLINE(1:EOFL)=BLOCK(1:EOFL)
          CHR=RDLINE(CURP:CURP)
          CURP=CURP+1
        END IF
      ELSE
        IF ( ST.EQ.-1 ) THEN
          IF ( EOFL-CURP.LT.19) THEN
            RDLINE(1:(EOFL-CURP+1))=RDLINE(CURP:EOFL)
            RDLINE(CURP:)='                          '
            CURP=EOFL-CURP+2
            CALL  FILIN(SUNIT,BLOCK,EOFL,ST)
            IF ( ST.GT.0) GOTO 200
            RDLINE(CURP:CURP+EOFL)=BLOCK(1:EOFL)
            EOFL=EOFL+CURP-1
            CURP=1
          END IF
        END IF
        CHR=RDLINE(CURP:CURP)
        CURP=CURP+1
        IF ( CURP.GT.EOFL ) GOTO 101
      END IF
C             1   2   3   4   5   6   7   8   9  10
      GOTO (101,410,420,430,440,450,460,470,470,480)
     +                           INDEX(CMMD(1:10),CHR)
 
      GOTO 101
C
 410  CHR=RDLINE(CURP:CURP)
C*******************************                P*
      GOTO (411,412,413,414) INDEX('UADR',CHR)
C       error country.
        ESTRG='P error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C*****************************                  PU;
 411      PDOWN=.FALSE.
          CALL HPLINE(IX,IY,OK)
          IF ( .NOT.OK ) GOTO 101
          CALL HLINE(PDOWN,PABS,IX,IY,OK)
          IF(.NOT.OK) GOTO 200
      GOTO 101
C*********************************              PA;
 412      PABS=.TRUE.
          CALL HPLINE(IX,IY,OK)
          IF ( .NOT.OK ) GOTO 101
          CALL HLINE(PDOWN,PABS,IX,IY,OK)
          IF(.NOT.OK) GOTO 200
      GOTO 101
C*********************************              PD;
 413      PDOWN=.TRUE.
          CALL HPLINE(IX,IY,OK)
          IF ( .NOT.OK ) GOTO 101
          CALL HLINE(PDOWN,PABS,IX,IY,OK)
          IF(.NOT.OK) GOTO 200
      GOTO 101
C*********************************              PR;
 414      PABS=.FALSE.
          CALL HPLINE(IX,IY,OK)
          IF ( .NOT.OK ) GOTO 101
          CALL HLINE(PDOWN,PABS,IX,IY,OK)
          IF(.NOT.OK) GOTO 200
      GOTO 101
C
 420  CHR=RDLINE(CURP:CURP)
      GOTO (421,422) INDEX('AP',CHR)
        ESTRG='A error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C*******************************                AA;
 421    CALL HPNUM(IX,OK)
        TOK=OK
        CALL HPNUM(IY,OK)
        TOK=TOK.AND.OK
        CALL HPNUMR(RA,OK)
        TOK=TOK.AND.OK
        CALL HPNUM(ID,OK)
        IF ( .NOT.TOK) GOTO 101
        PDOWN=.TRUE.
        RADIUS=DISTXY(REAL(IX),REAL(IY),AXP,AYP)
C      write(10,*) '[readhp1] ra ix iy= ',ra,ix,iy
C
        IF (RA.EQ.360) THEN
          SANG=0.0
          EANG=PI(2.0)
        ELSE
          IF (RA.LT.0) THEN
            EANG=CANG(REAL(IX),REAL(IY),AXP,AYP)
            SANG=MOD(EANG+RAD(RA)+PI(2.0),PI(2.0))
            IF ( SANG.LT.0.0 ) SANG=PI(2.0)-SANG
            AXP=IX+RADIUS*COS(SANG)
            AYP=IY+RADIUS*SIN(SANG)
C      write(10,*) '[readhp1] sang eang ',sang,eang
          ELSE
            SANG=CANG(REAL(IX),REAL(IY),AXP,AYP)
            EANG=MOD(SANG+RAD(RA),PI(2.0))
            IF ( EANG.LT.0.0 ) EANG=PI(2.0)-EANG
            AXP=IX+RADIUS*COS(EANG)
            AYP=IY+RADIUS*SIN(EANG)
C      write(10,*) '[readhp1] sang eang ',sang,eang
          END IF
        END IF
C
        CALL DEWC05(REAL(IX)*SCA,REAL(IY)*SCA,RADIUS*SCA,
     +              SANG,EANG,TFONT,CLAYER,TMIP1,OK)
        IF(.NOT.OK) GOTO 200
      GOTO 101
C*******************************                AP;
 422      CALL HPNUM(ID,OK)
          IF ( .NOT.OK ) GOTO 101
      GOTO 101
C
 430  CHR=RDLINE(CURP:CURP)
      GOTO (431) INDEX('I',CHR)
        ESTRG='C error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C*******************************                CI;
 431    CHR=RDLINE(CURP+1:CURP+1)
        IF (ICHAR(CHR).LT.45.OR.ICHAR(CHR).GT.58 ) THEN
           IF(CURP.LT.NLEN(RDLINE)) THEN
C              WRITE(10,*)'[REDHP ,CHR = ',CHR
              CURP=CURP+1
              GOTO 431
           END IF
        END IF
        CALL HPNUM(IA,OK)
C        WRITE(10,*) '[REDHP] IA ',IA
C
        CALL HPNUM(ID,TOK)
        IF ( .NOT.OK) GOTO 101
        RADIUS=IA
        SANG=0.0
        EANG=PI(2.0)
        CALL DEWC05(AXP*SCA,AYP*SCA,RADIUS*SCA,SANG,EANG,
     +              TFONT,CLAYER,TMIP1,OK)
        IF(.NOT.OK) GOTO 200
      GOTO 101
C
 440  CHR=RDLINE(CURP:CURP)
      GOTO (441,442) INDEX('BT',CHR)
        ESTRG='L error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C*******************************                LB;
 441    CONTINUE
 306    CURP=CURP+1
        TP=INDEX(RDLINE(CURP:),CHAR(3))+CURP-2
        IF ( TP.GT.CURP-2 ) THEN
          CALL DEWC85(AXP*SCA,AYP*SCA,TWIDTH,THIGT,TANGL,SLANT,
     +    TJST,TFONT,TCOL,CLAYER,RDLINE(CURP:TP),TMIP1,OK)
          IF(.NOT.OK) GOTO 200
          CURP=TP+2
        END IF
      GOTO 101
 442  CONTINUE
        CALL HPNUM(IX,OK)
      GOTO 101
C
 450  CHR=RDLINE(CURP:CURP)
      GOTO (451,452,453,454) INDEX('PILC',CHR)
        ESTRG='S error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C*******************************                SP;
 451      CALL HPNUM(ID,OK)
          IF ( .NOT.OK ) GOTO 101
          CLAYER=ID
      GOTO 101
C*********************************              SI;
 452    CALL HPNUMR(RX,OK)
        TOK=OK
        CALL HPNUMR(RY,OK)
        TOK=TOK.AND.OK
        IF ( .NOT.TOK ) GOTO 101
        TWIDTH =10.0*RX*1.5
        THIGT  =10.0*RY
      GOTO 101
C*********************************              SL;
 453    CALL HPNUMR(RX,OK)
        IF ( .NOT.OK ) GOTO 101
        SLANT=ATAN(RX)
      GOTO 101
 454    CALL HPNUM(IX,OK)
        IF ( OK ) GOTO 454
      GOTO 101
C
 460  CHR=RDLINE(CURP:CURP)
C*******************************                D*
      GOTO (461) INDEX('I',CHR)
        ESTRG='D error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C
C*****************************                  DI;
 461    CALL HPNUMR(RX,OK)
        TOK=OK
        CALL HPNUMR(RY,OK)
        TOK=TOK.AND.OK
        IF ( .NOT.TOK ) GOTO 101
        TANGL=DEG(ACOS(RX))
      GOTO 101
C
 470  CHR=RDLINE(CURP:CURP)
        CURP=CURP+1
        IF (CHR.NE.';'.AND.CURP.LT.EOFL) GOTO 470
      GOTO 101
 
 480  CHR=RDLINE(CURP:CURP)
C*******************************                V*
      GOTO (481) INDEX('S',CHR)
        ESTRG='V error'//RDLINE(CURP-1:CURP)
        CALL EPRINT(ESTRG)
      GOTO 101
C
C*****************************                  VS;
 481    CALL HPNUM(IX,OK)
      GOTO 101
 
 200  CONTINUE
C
      CLOSE(SUNIT)
      LFU(SUNIT)=.FALSE.
      RETURN
C
 990  CALL DEPRNT(688)
      RETURN
 991  CALL DEPRNT(689)
C
      END
*
*
*
C       @(#)  256.1 date 12/16/89 redhp0.ftn Daxcad revision 1.8
      SUBROUTINE REDHP0()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    Subroutine REDGI02 is the control routine
C2    for the READ HPGL function.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      INTEGER*4 ST
      CHARACTER*128 FILNM
      EXTERNAL RDHPGL
C
      CALL FNAME(FILNM,ST)
      IF ( ST.NE.0 ) THEN
         MEN=0
         RETURN
      END IF
C
C     go read the data file
      CALL RDHPGL(FILNM)
C
      END
 
      SUBROUTINE SOPEN(FILNM,FILID)
C     =============================

      CHARACTER*(*) FILNM
      LOGICAL OK
      INTEGER*4 FILID
      INTEGER*4 NLEN,ST
      INTEGER*4 FNLEN

      FILID=0
      FNLEN=NLEN(FILNM)
      IF ( FNLEN.EQ.0 ) RETURN
      CALL FINDU1(ST,OK)
      IF (.NOT.OK ) RETURN
      FILID=ST
      OPEN(UNIT=FILID,FILE=FILNM,ERR=99)
      RETURN
 99   FILID=0
      END
 
 
