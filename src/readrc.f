C
C
C     @(#)  412.1 date 6/11/92 readrc.f 
C
C
C     Filename    : readrc.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:44:00
C     Last change : 92/06/11 14:39:53
C
C     Copyright : Practical Technology Limited  
C     File :- readrc.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE REDASS()
C     SUBROUTINE REDBOA()
C     SUBROUTINE REDCMP(LIB)
C     SUBROUTINE REDCOD()
C     SUBROUTINE REDCOM()
C     SUBROUTINE REDCOP()
C     SUBROUTINE REDDRA()
C     SUBROUTINE REDERR()
C     SUBROUTINE REDIDX()
C     SUBROUTINE REDIFL()
C     SUBROUTINE REDLB2(LIB)
C     SUBROUTINE REDLIB()
C     SUBROUTINE REDLIN(N)
C     SUBROUTINE REDRD()
C     SUBROUTINE REDREM()
C     SUBROUTINE REDROU()
C     SUBROUTINE REDTEX()
C     SUBROUTINE RREDAC()
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE REDASS()
C     ===================
      include 'include/redboard.inc'
      include 'include/gerber.inc'
      include 'include/lfont.inc'
      include 'include/params.inc'
C
C     reads the assigments part of the initial
C     data file.
      CHARACTER*40 CMDS
      INTEGER ICMD,II,DRILL,LAY,ORI,PADN,SIZE,TYPE,
     +        PLATE,FINGL,TRAKN,END
C
      CMDS='MAR CMD MAX UNI PAD TRA TEX TTS TPS PPS'
      ICMD=((INDEX(CMDS,RDLINE(1:3))-1)/4)+1
      II=INDEX(RDLINE,'-')
      IF ( II.NE.0 ) RDLINE(II:II)='0'
      GOTO (1,1,1,4,5,6,7,1,1,1 ) ICMD
 
 1    CONTINUE
      RETURN
C2    CONTINUE
C     RETURN
C3    CONTINUE
C     RETURN
 4    CONTINUE
      READ(UNIT=RDLINE(4:),FMT='(I4)') II
C     set up dsu value. (25 thou = 1 dsu)
      DSU=(1.0/REAL(II))*2.54/DBUFAC
 
      RETURN
 5    CONTINUE
         READ(UNIT=RDLINE(4:),FMT='(2I3,I4,I2,I5,I2)',
     +     ERR=99)    PADN,LAY,SIZE,TYPE,DRILL,PLATE
C          PADN=PADN+1
          IF ( TYPE.EQ.0 ) THEN
C           PLATE is not the plated through hole value
C           it is in fact the .orietation
            ORI=PLATE
            READ(UNIT=RDLINE(23:),FMT='(I5,I2)',ERR=99)
     +        FINGL,PLATE
            IF ( FINGL.GT.0 ) THEN
              IF ( ORI.EQ.1 ) FINGL=-FINGL
            END IF
            TOLSET(PADN,2)=25.4*FINGL/1000.0
        END IF
C       finger pad.
        IF ( TYPE.EQ.0 ) END=2
        IF ( TYPE.EQ.1 ) END=2
        IF ( TYPE.EQ.2 ) END=1
        IF ( TYPE.EQ.3 ) END=2
C     Changed by MM 11/8/87 for BOB (Racal) such that pads are all
C     zero index and pad data is not maintained
C        PLTHKI(PADN)=END*1024+PADN
C        LTHKR(1,PADN)=-25.4*SIZE/1000.0
C        LTHKR(2,PADN)=25.4*DRILL/1000.0
C      The following code is used to temerarily hold the pad
C      radius in the line definition table. Note line thickness is used
C      for the first 16 pads and pen thickness for the last 16 this is
C      keep everthing into the 127 range of line thickness
C      Sorry neil its a cracker don't you think
        IF(PADN.LE.16) THEN
            LTHKR(1,PADN+110)=-25.4*SIZE/1000.0
        ELSE
            LTHKR(2,PADN+94)=-25.4*SIZE/1000.0
        END IF
        TOLSET(PADN,1)=PLATE*255+TYPE
      RETURN
C
 6    CONTINUE
        READ(UNIT=RDLINE(4:),FMT='(I2,I3,I5)',ERR=99) TRAKN,LAY,SIZE
C       offset track data
C     Changed by MM 11/8/87 for BOB (Racal) such that index to tracks
C     is +100
        TRAKN=TRAKN+100
        END=2
        LTHKR(1,TRAKN)=25.4*SIZE/1000.0
        PLTHKI(TRAKN)=END*1024+TRAKN
      RETURN
C
 7    CONTINUE
        READ(UNIT=RDLINE(4:),FMT='(I2,2I4)',ERR=99) PADN,SIZE,DRILL
C        PADN=PADN+50
        END=2
C     Changed by MM 11/8/87 for BOB (Racal) such that pads are all
C     zero index and pad data is not maintained
C        LTHKR(1,PADN)=25.4*DRILL/1000.0
C        LTHKR(2,PADN)=0.3
C        PLTHKI(PADN)=END*1024+PADN
C        TOLSET(PADN,2)=SIZE
        TOLSET(PADN+50,2)=SIZE
      RETURN
C
C8    CONTINUE
C     RETURN
C9    CONTINUE
C     RETURN
C10   CONTINUE
C     RETURN
 
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 redboa.ftn Daxcad revision 1.8
      SUBROUTINE REDBOA()
C     ===================
      include 'include/redboard.inc'
      include 'include/style.inc'
C
      LOGICAL OK
      INTEGER*2 TLAY,TFONT,P,TTHK
      REAL IX(2,2)
      INTEGER*4 LINES,K,N,I,J,L
C
C     check for correct 'L'
      IF ( RDLINE(1:1).NE.'L' ) GOTO 99
C     This for variable field definition
C     jump past the letter 'L'
      CURP=2
C     get the number of entries in definition.
      CALL REDLIN(1)
      IF ( ERRCOD.NE.0 ) GOTO 99
      LINES=NANS(1)
      N=1
C     set fixed layer for board outline.
      TLAY=40
C     set to solid line.
      COLOUR=2
      TFONT=1
      TTHK=THICK
      THICK=0
      DO 10 K=1,LINES
         CURP=1
         CALL REDRD()
         IF ( ERRCOD.NE.0 ) GOTO 99
         CALL REDLIN(2)
         IF ( ERRCOD.NE.0 ) GOTO 99
         IX(1,N)=NANS(1)*DSU
         IX(2,N)=NANS(2)*DSU
         IF ( K.GT.1 ) THEN
            I=MOD(N,2)+1
            J=MOD(N+1,2)+1
            CALL DEWC03(IX(1,I),IX(2,I),IX(1,J),IX(2,J),
     +                  TFONT,TLAY,P,OK)
         END IF
         N=MOD(N,2)+1
 10   CONTINUE
      THICK=TTHK
      RETURN
C
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 redcmp.ftn Daxcad revision 1.8
      SUBROUTINE REDCMP(LIB)
C     ======================
C
      include 'include/redboard.inc'
      include 'include/filunit.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/compd.inc'
      include 'include/entity.inc'
      include 'include/props.inc'
      include 'include/params.inc'
      include 'include/wrkdat.inc'
      include 'include/movdat.inc'
      include 'include/style.inc'
      include 'include/lfont.inc'
      include 'include/gerber.inc'
      include 'include/ndata.inc'
      include 'include/lfu.inc'
C
      LOGICAL OK,DELETE,FIRST
      REAL PADX,PADY,XMIN,YMIN,M(3,3),PI,XMD,YMD,WD,HT,BESTLN,ORGLN,
     +     CHKVAL
      INTEGER LIB
      INTEGER*2 P,TJST,TFONT,TCOL,TLAY,TMIP,TTHK
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP,TLDFIL,
     +          TP2,ENT,D1,D2,PMIP
      INTEGER*4 FN,NLEN,I,J,UNIT,TUNIT,LOCAL,TLNUM,TYPE
      CHARACTER*40 FILNM,NAME*4
      EXTERNAL NLEN,PI
CAPOLLO|SUN
      WRITE(UNIT=FILNM,FMT='(A,I5,A)',ERR=99) '/comp2d/l',lib,'.cmp'
CAPOLLO|SUN
CIBM
C      WRITE(UNIT=FILNM,FMT='(A,I5,A)',ERR=99) '\comp2d\l',lib,'.cmp'
CIBM
      CALL CRUNCH(FILNM)
      CALL CPRINT(FILNM)
C     This for variable field definition
      NAME=RDLINE(1:4)
C
C     set parameters for component master to be added.
      TYPE=1
      FN=1
      FILTYP='COMPM'
      TJST=5
      TFONT=1
      TCOL=7
      TLAY=45
C     test for existance of master within database
      CALL INSC10(FILNM,FN,PMIP,OK)
      IF (.NOT.OK) THEN
C       does not already exist in the drawing
C       test existance of external component file
        INQUIRE(FILE=FILNM,EXIST=OK)
        IF (.NOT.OK) THEN
C         cannot find the file
C         this is where we show the symbol from the ascii file.
          CALL OPNOFF ('redac.$$$',UNIT,OK)
          LOCAL=NANS(5)
          TLNUM=LNUM
101       CONTINUE
          READ(UNIT=UNIT,FMT='(A)',ERR=99,END=99) RDLINE
          EOFL=NLEN(RDLINE)
          IF ( EOFL.EQ.0 ) GOTO 101
          IF ( INDEX(RDLINE,'L').EQ.0) GOTO 101
          CURP=2
          CALL REDLIN(4)
          IF ( ERRCOD.NE.0 ) RETURN
          IF (NANS(1).EQ.LOCAL ) THEN
            TUNIT=REDUNT
            REDUNT=UNIT
            TLNUM=LNUM
            CALL REDLB2(LIB)
            IF ( ERRCOD.NE.0 ) GOTO 99
            LNUM=TLNUM
            REDUNT=TUNIT
          ELSE
            GOTO 101
          END IF
          CLOSE(UNIT=UNIT)
CIBM
C         LFU(UNIT)=.FALSE.
CIBM
C         showed the data indicate that by making reference negative.
          WRITE(UNIT=IDXUNT,REC=LOCAL+1,ERR=99) -LIB
          RETURN
        ELSE
          CALL OPENNM(FILNM,PARFUN,.TRUE.,OK)
C         must go get the file and enter to database
C         save the current record pointers
          TTMIP=NMIPOS
          TTPDP=NPDPOS
          TTTXP=NTXPOS
          TTRLP=NRLPOS
          TTPRP=NPRPOS
          TTPCP=NPCPOS
          CALL INSP05(FILNM,'COMPM',
     +    TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP)
          CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=99)
CIBM
C          LFU(PARFUN)=.FALSE.
CIBM
C         find the MIP for the component master
C         read it from the first entity of the master
          CALL DIR500(TTMIP,OK)
C         save parent pointer to header
          D1=IMBUFF(8)
C         read header MI entry
          CALL DIR500(D1,OK)
C         now read relation header entry for master
          CALL DRR950(IMBUFF(10),OK)
C         save MI pointer to component master
          PMIP=RLBUFF(3)
C         read text of comp/symb master name
          CALL DIR500(PMIP,OK)
          TP2=IMBUFF(9)
          CALL DTR500(TP2,OK)
C         ensure master name is corrected to filename
C         used for reference,prevents file-name changes
C         conflicting with internal comp names.
          CBUFF=FILNM
          CALL DTM500(TP2,PMIP,OK)
C         must correct master data for current database unit
          CALL CLROPF()
C
          CALL INSP07(FN,TTMIP,TYPE)
        END IF
      END IF
C
      CALL DER500(PMIP,OK)
C
C      write(10,*) '   '
C      write(10,*) 'Com name:',CBUFF
C      write(10,*) 'nans(7)',nans(7)
C     find relation header from instance data
      RELP=IMBUFF(10)
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
C     test for valid component relation
      IF (RLBUFF(1).NE.COMPM)  GOTO 99
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
C     first pad
      FIRST=.TRUE.
      DO 15 J=1,NRRECS
        NXTRLR=RLBUFF(2)
C       read the list of entities
        CALL DRR950(NXTRLR,OK)
        DO 10 I=4,10
          TTMIP=RLBUFF(I)
          IF (TTMIP.GT.0) THEN
C           read the entity and draw it in position.
            CALL ALLRD(TTMIP,ENT,M1,DELETE)
            IF ( .NOT.(ENT.EQ.COMPI)) THEN
C             this is a pad of the symbol.
              IF ( ENT.EQ.ARC.AND.IMBUFF(3).NE.3 ) THEN
                IF ( FIRST ) THEN
                  ORGLN=REAL(NANS(8)*DSU)+REAL(NANS(8)*DSU)
                  GOTO ( 91,92,93,94 ) NANS(7)+1
 91               CONTINUE
                  BESTLN=ORGLN+RDBUFF(1)+RDBUFF(2)
                  XMIN=RDBUFF(1)
                  YMIN=RDBUFF(2)
                  GOTO 30
 92               CONTINUE
                  BESTLN=ORGLN+RDBUFF(1)-RDBUFF(2)
                  XMIN=RDBUFF(1)
                  YMIN=RDBUFF(2)
c                  XMIN=RDBUFF(2)
c                  YMIN=RDBUFF(1)
                  GOTO 30
 93               CONTINUE
                  BESTLN=ORGLN-RDBUFF(1)-RDBUFF(2)
                  XMIN=RDBUFF(1)
                  YMIN=RDBUFF(2)
c                  XMIN=RDBUFF(2)
c                  YMIN=RDBUFF(1)
                  GOTO 30
 94               CONTINUE
                  BESTLN=ORGLN-RDBUFF(1)+RDBUFF(2)
                  XMIN=RDBUFF(1)
                  YMIN=RDBUFF(2)
c                  XMIN=RDBUFF(2)
c                  YMIN=RDBUFF(1)
 30               CONTINUE
                  FIRST=.FALSE.
                ELSE
                  GOTO ( 1,2,3,4 ) NANS(7)+1
 1                CHKVAL=ORGLN+RDBUFF(1)+RDBUFF(2)
                  GOTO 5
 2                CHKVAL=ORGLN+RDBUFF(1)-RDBUFF(2)
                  GOTO 5
 3                CHKVAL=ORGLN-RDBUFF(1)-RDBUFF(2)
                  GOTO 5
 4                CHKVAL=ORGLN-RDBUFF(1)+RDBUFF(2)
 5                CONTINUE
                  OK=CHKVAL.LT.BESTLN
                  IF ( CHKVAL.LT.BESTLN ) THEN
                     XMIN=RDBUFF(1)
                     YMIN=RDBUFF(2)
                     BESTLN=CHKVAL
                  ENDIF
                END IF
              ELSE  IF ( ENT.EQ.LINE ) THEN
                XMD=MAX(XMD,RDBUFF(1),RDBUFF(4))
                YMD=MAX(YMD,RDBUFF(2),RDBUFF(5))
              END IF
 
            END IF
          END IF
 10     CONTINUE
 15   CONTINUE
C
      XMD=REAL(NANS(2)*DSU)+XMD/2.0
      YMD=REAL(NANS(3)*DSU)+YMD/2.0
 
 
      GOTO ( 11,22,33,44 ) NANS(7)+1
 11   REFDAT(2,3)=0.0
      OPFLAG(2)=.FALSE.
      REFDAT(4,1)=REAL(NANS(8)*DSU)-XMIN
      REFDAT(4,2)=REAL(NANS(9)*DSU)-YMIN
      PADX=REFDAT(4,1)+XMD
      PADY=REFDAT(4,2)+YMD
      GOTO 55
 22   REFDAT(2,3)=PI(0.5)
      OPFLAG(2)=.TRUE.
      REFDAT(4,1)=REAL(NANS(8)*DSU)+YMIN
      REFDAT(4,2)=REAL(NANS(9)*DSU)-XMIN
      PADX=REFDAT(4,1)-YMD
      PADY=REFDAT(4,2)+XMD
      GOTO 55
 33   REFDAT(2,3)=PI(1.0)
      OPFLAG(2)=.TRUE.
      REFDAT(4,1)=REAL(NANS(8)*DSU)+XMIN
      REFDAT(4,2)=REAL(NANS(9)*DSU)+YMIN
      PADX=REFDAT(4,1)-XMD
      PADY=REFDAT(4,2)-YMD
      GOTO 55
 44   REFDAT(2,3)=PI(1.5)
      OPFLAG(2)=.TRUE.
      REFDAT(4,1)=REAL(NANS(8)*DSU)-YMIN
      REFDAT(4,2)=REAL(NANS(9)*DSU)+XMIN
      PADX=REFDAT(4,1)+YMD
      PADY=REFDAT(4,2)-XMD
      GOTO 55
 55   CONTINUE
C
      TTHK=THICK
      THICK=0
      HT=TOLSET(50,2)*DSU/PAPTOW
      WD=HT
      CALL DEWC85(PADX,PADY,HT,WD,0.0,0.0,
     +      TJST,TFONT,TCOL,TLAY,NAME,P,OK)
      THICK=TTHK
C
C     translate if required
      OPFLAG(4)=.TRUE.
C     set rotate to origin of symbol.
      REFDAT(2,1)=0.0
      REFDAT(2,2)=0.0
C     create transform for instance
      CALL SETTFM(M)
C     read master data entry
      CALL DER500(PMIP,OK)
C     create an instance
C     set entity type to Instance type.
      IMBUFF(2)=COMPI
C     ensure pointer to parent entry is zero.
      IMBUFF(8)=0
C     set work layer.
      IMBUFF(4)=CLAYER
C     copy window limits into instance
      DO 50 I=1,6
         RWORK(I,1)=RDBUFF(I)
         RWORK(I,2)=RDBUFF(I)
 50   CONTINUE
C     sort second diagonal data
      RWORK(2,2)=RWORK(5,1)
      RWORK(5,2)=RWORK(2,1)
C     transform enclosing box of component
      CALL MV0003(RWORK(1,1),M)
      CALL MV0003(RWORK(1,2),M)
C     write instance to database
      IF ( IMBUFF(10).LE.0) THEN
         CALL DEPRNT(418)
      END IF
      CALL DEW566(M,TMIP,OK)
C     draw the component in place
      CALL DRW066(M)
C     test for assigned properties,and update them
      CALL UPR066(TMIP,OK)
 
      RETURN
 
 99   CONTINUE
      ERRCOD=1
 
      END
 
C       @(#)  256.1 date 12/16/89 redcod.ftn Daxcad revision 1.8
      SUBROUTINE REDCOD()
C     ===================
      include 'include/redboard.inc'
      include 'include/style.inc'
 
C     This for variable field definition
      CURP=5
      CALL REDLIN(1)
      IF (ERRCOD.NE.0 ) GOTO 99
      CCODE=NANS(1)
C     off set track thickness to start at 100-107
      THICK=CCODE+100
      WRITE(10,'(A,2I4)') 'REDCOD:',CCODE,THICK
      RETURN
 99   ERRCOD=1
 
      END
*
C       @(#)  256.1 date 12/16/89 redcom.ftn Daxcad revision 1.8
      SUBROUTINE REDCOM()
C     ===================
      include 'include/redboard.inc'
 
      CHARACTER*4 NAME
      INTEGER  LIB
C
      CURP=5
      CALL REDLIN(9)
      IF (ERRCOD.NE.0 ) GOTO 99
C     read component number from index
      READ(UNIT=IDXUNT,REC=NANS(5)+1,ERR=99) LIB
      IF ( LIB.LT.0 ) RETURN
      CALL REDCMP(LIB)
C
      RETURN
 99   ERRCOD=1
C
      END
*
*
C       @(#)  256.1 date 12/16/89 redcop.ftn Daxcad revision 1.8
      SUBROUTINE REDCOP()
C     ===================
      include 'include/redboard.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/style.inc'
C
      LOGICAL OK
      INTEGER*2 TLAY,TFONT,P,FP,II
      REAL IX(2,2)
      INTEGER*4 K,N,I,J,L,NN
 
      N=1
C     set fixed layer for board outline.
      TLAY=50
C     set to solid line.
      FP=NMIPOS
      TFONT=1
      COLOUR=7
      THICK=0
      K=1
      NN=4
      CURP=1
      IF ( INDEX(RDLINE,'L').EQ.0 ) NN=2
      CALL REDLIN(NN)
      IF ( ERRCOD.NE.0 ) GOTO 99
      IX(1,N)=NANS(1)*DSU
      IX(2,N)=NANS(2)*DSU
      N=MOD(N,2)+1
C
 10   CONTINUE
C
        CURP=1
        CALL REDRD()
        IF ( ERRCOD.NE.0 ) GOTO 99
        NN=4
        IF ( INDEX(RDLINE,'L').EQ.0 ) NN=2
        CALL REDLIN(NN)
        IF ( ERRCOD.NE.0 ) GOTO 99
        IX(1,N)=NANS(1)*DSU
        IX(2,N)=NANS(2)*DSU
        I=MOD(N,2)+1
        J=MOD(N+1,2)+1
        CALL DEWC03(IX(1,I),IX(2,I),IX(1,J),IX(2,J),
     +                  TFONT,TLAY,P,OK)
        N=MOD(N,2)+1
C
      IF ( NN.EQ.2 )    GOTO 10
 
      TLAY=NANS(4)
      CALL DER500(FP,OK)
      IX(1,N)=RDBUFF(1)
      IX(2,N)=RDBUFF(2)
      I=MOD(N,2)+1
      J=MOD(N+1,2)+1
          CALL DEWC03(IX(1,I),IX(2,I),IX(1,J),IX(2,J),
     +                  TFONT,TLAY,P,OK)
 
      DO 20 II=FP,P-1
        CALL DIR500(II,OK)
        IMBUFF(4)=TLAY
        CALL DIM500(II,OK)
 20   CONTINUE
 
      RETURN
C
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 reddra.ftn Daxcad revision 1.8
      SUBROUTINE REDDRA()
C     ===================
      include 'include/redboard.inc'
      include 'include/params.inc'
      include 'include/lfu.inc'
C
      CHARACTER*100 CLIST
      INTEGER L,I,CWORD,OWORD
C              1   2   3   4   5   6   7   8   9   0   1   2   3   4   5
      CLIST='.REM.IFL.PCB.ASS.BOA.LIB.IDX.COM.CON.TEX.COD.COP.ROU.EOD.RE
     +F'
C
      ERRCOD=0
      CWORD=0
      OWORD=0
      LNUM=0
      IDXUNT=0
      SX=0.0
C
C     set up dsu value. (25 thou = 1 dsu)
      DSU=0.025*2.54/DBUFAC
C
 10   CONTINUE
      IF ( ERRCOD.NE.0 ) THEN
         CALL REDERR()
         GOTO 999
      END IF
C     read the next line from the pascii input file.
      CALL REDRD()
C
 101  CONTINUE
      IF ( ERRCOD.NE.0 ) THEN
         CALL REDERR()
         GOTO 999
      END IF
      IF ( RDLINE(1:1).EQ.'.' ) THEN
C        control word found ---  non-modal control
         IF ( CWORD.NE. 1.AND.
     +        CWORD.NE.11.AND.
     1        CWORD.NE.15) OWORD=CWORD
         CWORD=((INDEX(CLIST,RDLINE(1:4))-1)/4)+1
         IF( CWORD.EQ.1 ) THEN
            CALL REDREM()
            CWORD=OWORD
         ELSE IF ( CWORD.EQ.11 ) THEN
            CALL REDCOD()
            CWORD=OWORD
         ELSE IF ( CWORD.EQ.15 ) THEN
            CWORD=OWORD
         END IF
         IF ( CWORD.EQ.14 ) GOTO 999
      ELSE
C               1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
         GOTO (11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) CWORD
 11   CONTINUE
        CWORD=OWORD
        GOTO 10
 12   CONTINUE
C       PRINT'(2A)','IFL:',RDLINE(1:EOFL)
        CALL REDIFL()
        GOTO 10
 13   CONTINUE
C       PRINT'(2A)','PCB:',RDLINE(1:EOFL)
        GOTO 10
 14   CONTINUE
        CALL REDASS()
        GOTO 10
 15   CONTINUE
        CALL REDBOA()
        GOTO 10
 16   CONTINUE
        CALL REDLIB()
        GOTO 10
 17   CONTINUE
        CALL REDIDX()
        GOTO 10
 18   CONTINUE
        CALL REDCOM()
        GOTO 10
 19   CONTINUE
C       PRINT'(2A)','CON:',RDLINE(1:EOFL)
        GOTO 10
 20   CONTINUE
        CALL REDTEX()
        GOTO 10
 21   CONTINUE
        CALL REDCOD()
        CWORD=OWORD
        GOTO 10
 22   CONTINUE
        CALL REDCOP()
        GOTO 10
 23   CONTINUE
        CALL REDROU()
        GOTO 101
 24   CONTINUE
        GOTO 10
 25   CONTINUE
C        WRITE(10,'(2A)') 'REF:',RDLINE(1:EOFL)
        CWORD=OWORD
        GOTO 10
      END IF
C-----------------------------------------     main loop
      GOTO 10
 999  CONTINUE
      CLOSE(UNIT=REDUNT)
CIBM
C      LFU(REDUNT)=.FALSE.
CIBM
      IF ( IDXUNT.NE.0 ) THEN
CIBM
C      LFU(IDXUNT)=.FALSE.
CIBM
          CLOSE(UNIT=IDXUNT)
      END IF
      RETURN
C
 100  CALL DEPRNT(666)
      END
*
*
C       @(#)  256.1 date 12/16/89 rederr.ftn Daxcad revision 1.8
      SUBROUTINE REDERR()
C     ===================
      include 'include/redboard.inc'
C
      CALL DEPRNT(677)
      EOFL=MAX(1,EOFL)
      CALL CPRINT(RDLINE(1:EOFL))
      WRITE(10,'(3A,I5)')'"',RDLINE(1:EOFL),'"',LNUM
      WRITE(RDLINE,FMT='(A,I3)')
     +   'Error in REDBOARD file line',LNUM
      WRITE(10,*)'LINE =',LNUM
      CALL CPRINT(RDLINE)
      END
*
*
C       @(#)  256.1 date 12/16/89 redidx.ftn Daxcad revision 1.8
      SUBROUTINE REDIDX()
C     ===================
      include 'include/redboard.inc'
      LOGICAL OK
 
      IF ( IDXUNT.EQ.0 ) THEN
C       open scratch file for index
        CALL OURSCR(IDXUNT,4,OK)
        IF ( .NOT.OK ) GOTO 99
      END IF
C     This for variable field definition
      CURP=2
      CALL REDLIN(2)
      IF (ERRCOD.NE.0 ) GOTO 99
      WRITE(UNIT=IDXUNT,REC=NANS(1)+1,ERR=99) NANS(2)
      RETURN
 99   ERRCOD=1
 
      END
*
*
C       @(#)  256.1 date 12/16/89 redifl.ftn Daxcad revision 1.8
      SUBROUTINE REDIFL()
C     ===================
      include 'include/redboard.inc'
 
 
 10   CONTINUE
      CALL REDRD()
      IF ( ERRCOD.NE.0 ) GOTO 99
C     special check  allowed here because '.EOD'
C     will always be there.
      IF ( RDLINE(1:1).EQ.'.'.AND.
     +     RDLINE(1:4).NE.'.EOD') GOTO 99
 
      IF ( RDLINE(1:4).NE.'.EOD') GOTO 10
 
      RETURN
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 redlb2.ftn Daxcad revision 1.8
      SUBROUTINE REDLB2(LIB)
C     ===================
      include 'include/redboard.inc'
      include 'include/style.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      CHARACTER*20 NAME
      REAL PI,IX(2,2),RAD
      LOGICAL EOS,OK
      INTEGER*2 TLAY,TFONT,P,PAD,TTHK,TCOL,TJST
      INTEGER*4 LINES,K,N,I,J,L,UNIT,LIB
      EXTERNAL PI
C
C     check for correct 'L'
      IF ( RDLINE(1:1).NE.'L' ) GOTO 99
C     L is the library reference number
C     IX(1,1),IX(2,1) are the dimension of the
C     component
C     LINES is the number of pads on the component
      CURP=2
      N=4
      CALL REDLIN(N)
      IF ( ERRCOD.NE.0 ) RETURN
      L=NANS(1)
      IX(1,1)=NANS(2)*DSU
      IX(2,1)=NANS(3)*DSU
 
      LINES=NANS(4)
C
      TFONT=1
      TLAY=0
      TCOL=7
      TJST=6
      COLOUR=7
      TTHK=THICK
      THICK=0
      WRITE(UNIT=NAME,FMT='(A,I6)',ERR=99) 'L',L
      CALL CRUNCH(NAME)
      CALL DEWC85(SX+IX(1,1)/2.0,-10.0*PAPTOW,2.0,2.0,
     +     0.0,10.0,TJST,TFONT,TCOL,TLAY,NAME,P,OK)
      WRITE(UNIT=NAME,FMT='(A,I6)',ERR=99) 'L',LIB
      CALL CRUNCH(NAME)
      CALL DEWC85(SX+IX(1,1)/2.0,-20.0*PAPTOW,2.0,2.0,
     +     0.0,10.0,TJST,TFONT,TCOL,TLAY,NAME,P,OK)
C     border of component.
      THICK=0
      CALL DEWC03(SX+0.0,0.0,SX+IX(1,1),0.0,
     +                                TFONT,TLAY,P,OK)
      CALL DEWC03(SX+IX(1,1),0.0,SX+IX(1,1),IX(2,1),
     1                                TFONT,TLAY,P,OK)
      CALL DEWC03(SX+IX(1,1),IX(2,1),SX+0.0,IX(2,1),
     1                                TFONT,TLAY,P,OK)
      CALL DEWC03(SX+0.0,IX(2,1),SX+0.0,0.0,
     1                                TFONT,TLAY,P,OK)
      N=3
      DO 10 K=1,LINES
        CURP=1
        CALL REDRD()
        IF ( ERRCOD.NE.0 ) RETURN
        CALL REDLIN(N)
        IF ( ERRCOD.NE.0 ) RETURN
C     Changed by MM 11/8/87 for BOB (Racal) such that pads are all
C     zero index and pad data is not maintained
C        THICK=NANS(3)+1
        IX(1,2)=SX+NANS(1)*DSU
        IX(2,2)=   NANS(2)*DSU
C       pick up the pad size from the definition table.
C      Changed by MM 11/9/87 for racal
C      The following code is used to temerarily hold the pad
C      radius in the line definition table. Note line thickness is used
C      for the first 16 pads and pen thickness for the last 16 this is
C      keep everthing into the 127 range of line thickness
        IF(NANS(3).LE.16) THEN
            RAD=PAPTOW*ABS(LTHKR(1,NANS(3)+110)/2.0)
        ELSE
            RAD=PAPTOW*ABS(LTHKR(2,NANS(3)+94)/2.0)
        END IF
c        RAD=PAPTOW*ABS(LTHKR(1,THICK)/2.0)
        CALL DEWC05(IX(1,2),IX(2,2),
     +     RAD,0.0,PI(2.0),TFONT,TLAY,P,OK)
 10   CONTINUE
      THICK=TTHK
C
      SX=SX+REAL(IX(1,1))+(10.0*PAPTOW)
 
      RETURN
 99   ERRCOD=1
 
      END
 
C       @(#)  256.1 date 12/16/89 redlib.ftn Daxcad revision 1.8
      SUBROUTINE REDLIB()
C     ===================
      include 'include/redboard.inc'
      include 'include/style.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/lfu.inc'
C
      CHARACTER*20 NAME
      CHARACTER*1024 BUFF
      REAL PI,IX(2,2),RAD
      LOGICAL EOS,OK
      INTEGER*2 TLAY,TFONT,P,PAD,TTHK,TCOL,TJST
      INTEGER*4 LINES,K,N,I,J,L,UNIT
      EXTERNAL PI
C
C     check for correct 'L'
      IF ( RDLINE(1:1).NE.'L' ) GOTO 99
C     L is the library reference number
C     IX(1,1),IX(2,1) are the dimension of the
C     component
C     LINES is the number of pads on the component
      CURP=2
      N=4
      CALL REDLIN(N)
      IF ( ERRCOD.NE.0 ) RETURN
C     library reference no.
      L=NANS(1)
      IX(1,1)=NANS(2)*DSU
      IX(2,1)=NANS(3)*DSU
C     no of pads in the definition
      LINES=NANS(4)
C
      INQUIRE(FILE='REDAC.$$$',EXIST=OK)
      IF ( OK ) THEN
        CALL FINDU1(UNIT,OK)
        OPEN(UNIT=UNIT,FILE='REDAC.$$$',STATUS='UNKNOWN',
     +                            ACCESS='SEQUENTIAL',ERR=99)
      ELSE
        CALL OPNFFF ('redac.$$$',UNIT,OK)
        IF ( .NOT.OK ) GOTO 99
      END IF
      WRITE(UNIT=UNIT,FMT='(A)',ERR=98) RDLINE(1:EOFL)
C
C     This has been added cos STATUS='APPEND' is not F77 Complient!!!!
C
C     Read until end of file
C
 30   CONTINUE
      READ(UNIT=UNIT,FMT='(A)',END=20) BUFF
      GOTO 30
 20   CONTINUE
C
C     Do whatever comes next
C
      DO 10 K=1,LINES
        CURP=1
        CALL REDRD()
        IF ( ERRCOD.NE.0 ) RETURN
        WRITE(UNIT=UNIT,FMT='(A)',ERR=98) RDLINE(1:EOFL)
 
 10   CONTINUE
 98   CONTINUE
      CLOSE(UNIT=UNIT)
      RETURN
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 redlin.ftn Daxcad revision 1.8
      SUBROUTINE REDLIN(N)
C     ====================
      include 'include/redboard.inc'
C
      CHARACTER*10 NUM
      INTEGER*4 N,I
      LOGICAL OK
 
      ERRCOD=0
      I=0
 10   I=I+1
      CALL NWORD(NUM,NANS(I),' ',OK)
      IF ( NANS(I).EQ.0 ) GOTO 99
      IF ( NUM(1:1).NE.'L') THEN
         CALL IVALU(NUM,NANS(I),OK)
         IF ( .NOT.OK ) GOTO 99
      ELSE
         CALL IVALU(NUM(2:),NANS(I),OK)
         IF(.NOT.OK) NANS(I)=0
      END IF
      IF( I.LT.N ) GOTO 10
      RETURN
 99   ERRCOD=1
      END
*
C       @(#)  256.1 date 12/16/89 redrd.ftn Daxcad revision 1.8
      SUBROUTINE REDRD()
C     ==================
      include 'include/redboard.inc'
      include 'include/style.inc'
C
      INTEGER NLEN
      EXTERNAL NLEN
C
 10   READ(UNIT=REDUNT,FMT='(A)',ERR=99,END=99) RDLINE
      EOFL=NLEN(RDLINE)
      LNUM=LNUM+1
      IF ( EOFL.EQ.0 ) GOTO 10
c      IF ( LNUM.GT.200 ) THEN
c      WRITE(10,FMT='(A,3I4)') 'REARD:',REDUNT,THICK,LNUM
c      WRITE(10,'(3A)')'REARD:"',RDLINE(1:EOFL),'"'
c      END IF
      RETURN
 99   ERRCOD=1
      END
*
*
C       @(#)  256.1 date 12/16/89 redrem.ftn Daxcad revision 1.8
      SUBROUTINE REDREM()
C     ===================
      include 'include/redboard.inc'
C      PRINT'(3A)','Rem:"',RDLINE(5:EOFL),'"'
      END
*
*
C       @(#)  256.1 date 12/16/89 redrou.ftn Daxcad revision 1.8
      SUBROUTINE REDROU()
C     ===================
      include 'include/redboard.inc'
      include 'include/style.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      REAL IX(2,2),RAD,PI
      LOGICAL EOS,OK
      INTEGER*2 TLAY,TFONT,P,LAY,TTHK
      INTEGER*4 LINES,K,N,I,J,L,II,NO
      EXTERNAL PI
 
      TLAY=0
      LAY=0
      TFONT=1
      K=1
      N=1
      NO=4
 5    CONTINUE
      IF ( RDLINE(1:1).EQ.'.') THEN
         II=(((INDEX('    .COD.REM',RDLINE(1:4)) )-1)/4)+1
      ELSE
         II=4
      END IF
      GOTO( 40,20,30,50 ) II
 50   CONTINUE
        CURP=1
        NO=4
C       if no layer no then only 2 number in the list.
        IF ( INDEX(RDLINE,'L').EQ.0 ) NO=2
        CALL REDLIN(NO)
        IF(ERRCOD.NE.0 ) RETURN
        IX(1,N)=NANS(1)*DSU
        IX(2,N)=NANS(2)*DSU
        LAY=0
        IF (NO.EQ.4) LAY=NANS(4)
        IF ( K.GT.1 ) THEN
           I=MOD(N,2)+1
           J=MOD(N+1,2)+1
           IF (.NOT.(IX(1,I).EQ.IX(1,J).AND.
     +               IX(2,I).EQ.IX(2,J)) ) THEN
              CALL DEWC03(IX(1,I),IX(2,I),IX(1,J),IX(2,J),
     +                    TFONT,TLAY,P,OK)
              IF(LAY.NE.TLAY.AND.LAY.NE.0 ) THEN
                 TTHK=THICK
                 THICK=0
                 RAD=PAPTOW*ABS(LTHKR(1,110)/2.0)
                 CALL DEWC05(IX(1,N),IX(2,N),
     +              RAD,0.0,PI(2.0),TFONT,TLAY,P,OK)
                 THICK=TTHK
              END IF
              IF(LAY.EQ.0 ) THEN
C                end of this  line.
                 K=0
              ELSE
                TLAY=LAY
                COLOUR=TLAY
              END IF
           END IF
        ELSE
           TLAY=LAY
           COLOUR=TLAY
        END IF
        K=K+1
        N=MOD(N,2)+1
      GOTO 10
 20      CALL REDCOD()
         IF(ERRCOD.NE.0 ) RETURN
         K=1
      GOTO 10
 30      CALL REDREM()
 10      CALL REDRD()
         IF(ERRCOD.NE.0 ) RETURN
      GOTO 5
 40   RETURN
C
      END
*
*
C       @(#)  256.1 date 12/16/89 redtex.ftn Daxcad revision 1.8
      SUBROUTINE REDTEX()
C     ===================
      include 'include/redboard.inc'
      include 'include/gerber.inc'
      include 'include/style.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      CHARACTER*80 NAME
      REAL ANG,PADX,PADY,WD,HT
      LOGICAL EOS,OK
      INTEGER*2 TLAY,TFONT,P,LAY,TTHK,TCOL,TJST
      INTEGER*4 N,I,J,L,II,NO
      EXTERNAL PI
 
      TLAY=0
      LAY=0
      TFONT=1
      TCOL=7
      TJST=1
      N=1
      NO=5
      IF ( RDLINE(1:1).EQ.'.') RETURN
C     read the text
      NAME=RDLINE(1:EOFL)
      CALL REDRD()
      IF ( ERRCOD.NE.0 ) RETURN
      CURP=1
      CALL REDLIN(NO)
      ANG=0.0
 
      GOTO (105,102,103,104) NANS(2)+1
 102  ANG=90.0
      GOTO 105
 103  ANG=180.0
      GOTO 105
 104  ANG=270.0
 105  CONTINUE
C
      TLAY=NANS(1)
C
C      IF ( THICK.LT.50 )THICK=THICK+10
      HT=TOLSET(THICK-50,2)*DSU/PAPTOW
      NO=THICK
      THICK=0
      WD=HT
      IF ( NANS(3).EQ.1 ) WD=-WD
      PADX=NANS(4)*DSU
      PADY=NANS(5)*DSU
      CALL DEWC85(PADX,PADY,WD,HT,ANG,0.0,
     +        TJST,TFONT,TCOL,TLAY,NAME,P,OK)
      THICK=NO
C
      END
*
 
C       @(#)  256.1 date 12/16/89 rredac.ftn Daxcad revision 1.8
      SUBROUTINE RREDAC()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    Subroutine REDGE0 is the control routine
C2    for the READ GERBER function.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/redboard.inc'
      include 'include/lfu.inc'
      include 'include/style.inc'
C
      INTEGER*4 ST
      LOGICAL OK
      CHARACTER*128 FILNM
      EXTERNAL RDGERB,FNAME
C
      CALL FNAME(FILNM,ST)
      IF ( ST.NE.0 ) THEN
         MEN=0
         RETURN
      END IF
 
      CALL FINDU1(REDUNT,OK)
      IF ( .NOT.OK ) THEN
         CALL DEPRNT(665)
        GOTO 99
      END IF
 
      OPEN(UNIT=REDUNT,FILE=FILNM)
CIBM
C      LFU(REDUNT)=.TRUE.
CIBM
      REWIND(UNIT=REDUNT)
 
      ST=THICK
C     go read the data file
      CALL REDDRA()
      THICK=ST
C
      INQUIRE(FILE='redac.$$$',EXIST=OK)
      IF ( OK ) THEN
         OPEN(UNIT=REDUNT,FILE='REDAC.$$$')
         CLOSE(UNIT=REDUNT,STATUS='DELETE')
      END IF
 
 99   CCMD='q'
C
      END
*
*
