C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 oldatah.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION GPONLY()
C     SUBROUTINE BSE066(TYPE,M,X,Y,OK)
C     SUBROUTINE DSE066(TYPE,M,X,Y,FOUND,LIM,LIMSD)
C     SUBROUTINE DSE800(X,Y,OK)
C     SUBROUTINE DSE801(X,Y,OK)
C     SUBROUTINE DSE880(ENT,X,Y,M,USIT,OK,INST,MMIP,LIM,LIMSD)
C     SUBROUTINE DSE881(ENT,X,Y,OK,INST,MMIP)
C     SUBROUTINE INTSPL(GEOM,MATR,OX,OY,DIST,X,Y,FIRSTS,HIT)
C     SUBROUTINE SRCHWN(XMIN,YMIN,XMAX,YMAX,FLAG)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE BSE066(TYPE,M,X,Y,OK)
C     ================================
C
C1    vartype           I2,R(3,3),R,R,L
C1    iostatus           I,  I    I,I,O
C
      include 'include/masti.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/compd.inc'
C
      REAL M(3,3),X,Y,WX(4),WY(4),DM(3,3)
      INTEGER*4 I,J,II,JJ,KK
      INTEGER*2 ENT,TTMIP,TYPE,MMIP,ZERO
      LOGICAL OK,DELETE,L0,L1,L2,TOTAL,NOFIND,L3,L4
C
      EXTERNAL DRR950,ALLRD,SPCDRW,ALLTFM,I3M,MULT3M
C
      CALL I3M(M1)
      CALL I3M(DM)
      CALL MULT3M(M,M1,MM)
      CNEST=0
      NINSTS=0
      NOFIND=.FALSE.
C
C     save instance master index.
      MMIP=MIP
C     WRITE(10,*) '[DSE066]',MIP,IDBUFF(2)
      CALL CHKCMP(X,Y,OK)
C     WRITE(10,*) 'DID IT FIT',X,Y,OK
      IF (.NOT. OK ) RETURN
C
C     ensure text scaled proportionaly during display
C     save opflags
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
C
 5    CONTINUE
C     find relation header from instance data
      RELP=IMBUFF(10)
C      WRITE(UNIT=10,FMT='(I6,2A)')RELP,' ',CBUFF
C      WRITE(UNIT=10,FMT=*)'[DSE066] Start of Component'
C      WRITE(UNIT=10,FMT=*)'Nest=',CNEST,' Ninsts=',NINSTS
C      write(unit=10,fmt=*)' Current Transform'
C      WRITE(UNIT=10,FMT='(3F8.3)')((MM(II,JJ),JJ=1,3),II=1,3)
C
C     if null relation pointer,then unresolved instance
      IF (RELP.LE.0) THEN
C        write to diag
         WRITE(UNIT=10,FMT=*)
     +   '*** Unresolved COMPONENT instance [DSE066]',CBUFF
         NOFIND=.TRUE.
      END IF
      IF (NOFIND) GOTO 20
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
C     test for valid component relation
C     WRITE(10,*) 'RLBUFF(1)',RLBUFF(1)
      IF (RLBUFF(1).NE.COMPM.AND.RLBUFF(1).NE.SYMBM) THEN
         GOTO 20
      END IF
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
      NENTS=RLBUFF(5)
C
C     WRITE(10,*) 'NENT,NREC',NENTS,NRRECS
      DO 15 J=1,NRRECS
         NXTRLR=RLBUFF(2)
C        read the list of entities
         CALL DRR950(NXTRLR,OK)
         DO 10 I=4,10
            TTMIP=RLBUFF(I)
            IF (TTMIP.GT.0) THEN
C              read the entity and draw it in position
               CALL ALLRD(TTMIP,ENT,M1,DELETE)
               IF (ENT.EQ.COMPI.OR.ENT.EQ.SYMBI) THEN
C                 check if nested component has been hit.
                  CALL MV0066(MM,DM)
                  CALL CHKCMP(X,Y,OK)
                  IF ( OK ) THEN
C                    found a nested instance
C                    save reference
                     NINSTS=NINSTS+1
C                    set nest level for this instance,must be 1 greater
C                    than the current level.
                     NSTLEV(NINSTS)=CNEST+1
C                    save pointer to instance
                     NSTMIP(NINSTS)=TTMIP
C                    save instance name
                     NSTNAM(NINSTS)=CBUFF
C                    calculate nested transformation
                     CALL MULT3M(M1,MM,M2)
C                    save the transform
                     KK=0
                     DO 30 JJ=1,3
                        DO 31 II=1,3
                           KK=KK+1
                           NSTTFM(KK,NINSTS)=M2(II,JJ)
 31                     CONTINUE
 30                  CONTINUE
C                     WRITE(UNIT=10,FMT=*)'[DSE066] Nested Component'
C                     WRITE(UNIT=10,FMT=*)'[DSE066] Nest level=',
C    +               CNEST+1,' MIP=',TTMIP,' Name=',CBUFF
C              WRITE(UNIT=10,FMT='(3F8.3)')((M2(II,JJ),JJ=1,3),II=1,3)
                  END IF
               ELSE
C                 transform and draw the entity
C                 transform the entity
                  CALL ALLTFM(ENT,MM)
C                 draw the transformed entity
C                 quick layer test dont bother checking if its invisible
                  IF(VLAYER(IMBUFF(4)) ) THEN
                      IF ( TYPE.EQ.1 ) THEN
                         CALL DSE880(ENT,X,Y,MM,.TRUE.,OK,.TRUE.,MMIP)
                      ELSE
                         CALL DSE881(ENT,X,Y,OK,.TRUE.,MMIP)
                      END IF
                      IF ( OK ) GOTO 999
                  ENDIF
               END IF
            END IF
 10      CONTINUE
 15   CONTINUE
C
 20   CONTINUE
C      WRITE(UNIT=10,FMT=*)'[DSE066] end of Component'
C     handle the nested instances if required
      IF (NINSTS.GT.0) THEN
C        must read back the next instance to expand
         TTMIP=NSTMIP(NINSTS)
         CNEST=NSTLEV(NINSTS)
C         WRITE(UNIT=10,FMT=*)'Expanding ',NSTNAM(NINSTS)
         CALL ALLRD(TTMIP,ENT,M1,DELETE)
C        retrieve transform
         KK=0
         DO 32 JJ=1,3
            DO 33 II=1,3
               KK=KK+1
               MM(II,JJ)=NSTTFM(KK,NINSTS)
 33         CONTINUE
 32      CONTINUE
C        decrement count of instances to be expanded
         NINSTS=NINSTS-1
C        go process this instance
         GOTO 5
      END IF
C
 999  CONTINUE
 
C     recover opflags
      OPFLAG(1)=L0
      OPFLAG(6)=L1
      OPFLAG(7)=L2
      OPFLAG(3)=L3
      OPFLAG(11)=L4
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE DSE066(TYPE,M,X,Y,FOUND,LIM,LIMSD)
C     =============================================
C
C1    vartype           I2,R(3,3),R,R,L
C1    iostatus           I,  I    I,I,O
C
C2
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
      REAL X,Y
      CHARACTER*2 INPL
      INTEGER*2 TTMIP,ENTPNT,RELP,STKP,MMIP,TYPE
      INTEGER*2 ENT,I,J,DI
      REAL LIM,LIMSD
      LOGICAL OK,FINISH,DELETE,FOUND
      LOGICAL L0,L1,L2,L3,L4
C     start relation list
C     set found status
      FOUND = .FALSE.
C     save instance master index.
      MMIP=MIP
 
C     check the box limits
      CALL CHKCMP(X,Y,OK)
      IF (.NOT. OK ) RETURN
 
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
      STKP=0
      RELP = IMBUFF(10)
      FINISH = .FALSE.
      ENTPNT = 4
C
C     if relation pointer is undefined then dinna draw
      IF ( RELP.LE.0 ) GOTO 200
 
100   CONTINUE
C     read the first record and continuation if nec
      CALL DRR950(RELP,OK)
      IF(RLBUFF(1).NE.200.OR.ENTPNT.EQ.11) THEN
          RELP=RLBUFF(2)
C         test for last line
          IF ( RELP.EQ.0 ) THEN
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
 
          ENDIF
 
          ENTPNT = ENTPNT + 1
 
C         get current entity
          CALL ALLRD ( TTMIP,ENT,ME,DELETE)
 
          IF (ENT.EQ.COMPI.OR.ENT.EQ.SYMBI) THEN
C             check current nesting level
              IF(STKP.GE.NSTLIM.OR.IMBUFF(10).LE.0) GOTO 100
C             check thast his point within this box
              CALL MV0066(CDT,IMM)
              CALL CHKCMP(X,Y,OK)
              IF ( .NOT.OK ) GOTO 100
C             Its an instance go and draw it but
C             first save the current pointers and
C             transform
 
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
C             search for the entity
              CALL ALLTFM(ENT,CDT)
              IF(VLAYER(IMBUFF(4)) ) THEN
 
                  IF ( TYPE.EQ.1 ) THEN
                    CALL DSE880(ENT,X,Y,CDT,.TRUE.,FOUND,.TRUE.
     +                          ,MMIP,LIM,LIMSD)
                  ELSE
                     CALL DSE881(ENT,X,Y,FOUND,.TRUE.,MMIP)
                  END IF
                  IF ( FOUND ) GOTO 200
 
              END IF
 
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
      RETURN
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE DSE800(X,Y,OK)
C     =========================
C1      vartype         R R  L
C1      iostatus        I I  O
C1
C
C2    Subroutine DSE800 carries out a search for entities
C2    within a region close to the point in world space defined
C2    by X,Y. The  first entity found in the search zone
C2    is flagged by the MI pointer being stored
C2    in the scratch file attached to unit SWINDU
C2    available from common block SWIND.
C2    Only entity types enabled for searching purposes will be
C2    considered.
C2    NOTE: this routine searches only the contents of the
C2    display file.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/entity.inc'
      include   'include/apollo.inc'
      include   'include/curwin.inc'
      include   'include/keymap.inc'
      include  'include/viewport.inc'
C
      REAL X,Y,X1,Y1,X2,Y2,MPX,MPY,BX,BY,DISTXY,DIST,CD0D13
      REAL M(3,3),REAL,LIM,LIMSD,XP,YP
C
      DOUBLE PRECISION DMPX,DMPY,DBLE
      INTEGER*2 POINT,ENT,TMIP,DFPK,D1,GPMI,STPNT,TYPE
      INTEGER*4 MOD,DICTN,I,IX,IY,HITX,HITY
      LOGICAL OK,TOTAL,CHKLN,ARCTPT,GROUPF,SAME,DELETE,GPONLY,ENTHIT
C
      INTRINSIC ATAN,ABS,MOD,DBLE,REAL
C
      EXTERNAL RDISPF,STOWVX,DIR500,DBR500,CD0D13,CHKLN,DCCPLP
      EXTERNAL ARCTPT,DISTXY,TEXBOX,SAME,
     +         WO2SC,SSFLAG,GETGRP,GPONLY
C
      CALL STOWVX(XWIDTH,LIM)
      LIMSD = LIM**2
      OK=.FALSE.
C     set size of search zone around hit point
      TYPE=1
C
      IF ( LDFILE(CVPN) .EQ. 1 ) RETURN
C     cycle through the display file
      DFPK=0
C
 10   CONTINUE
      DFP=MOD(DFP+0,LDFILE(CVPN)-1)+DFPINC
C
      DFPK=DFPK+1
C
C     read next display file entry
      CALL RDISPF(DFP,ENT,TMIP,OK)
C     check for error reading data.
      IF (.NOT.OK) RETURN
C
C      WRITE(10,*) '[DSE800]DFP,ENT,TMIP,ENSRCH(ENT) GSSTAT',
C     +                     DFP,ENT,TMIP,ENSRCH(ENT),GSSTAT
C++++++++++++++++++++++++++++++++++++++++
      IF ((ENSRCH(ENT).OR.ENSRCH(GROUP)).AND.TMIP.GT.0) THEN
C1111111111111111111111111111111111111111
         CALL ALLRD(TMIP,ENT,M,DELETE)
         IF(ENSRCH(GROUP).AND.IMBUFF(1).NE.GROUP.AND.
     +      GPONLY()) GOTO 20
C        Search is valid
C        test for delete status
         MIP=TMIP
C        if deleted or found already.
C        component master then ignore.
C        symbol master then ignore.
C        test for group status condition
C        ignore grouped entities entirely if required
         IF( IMBUFF(1).GT.99     .OR.
     +       IMBUFF(1).EQ.COMPM  .OR.
     1       IMBUFF(1).EQ.SYMBM  .OR.
     2    (IMBUFF(1).EQ.GROUP.AND.GSSTAT.EQ.0)     ) GOTO 20
C
C        test for group status condition
         GROUPF=(IMBUFF(1).EQ.GROUP)
C        save pointer to group parent MI
         GPMI=IMBUFF(8)
C        now get into the search proper
C        ensure correct entity type being used in search
         ENT=IMBUFF(2)
C
         IF (ENT.EQ.COMPI .OR. ENT.EQ.SYMBI) THEN
C22222222222222222222222222222222
C           save index record for instance
            CALL DSE066(TYPE,M,X,Y,OK,LIM,LIMSD)
            IF (.NOT.OK) GOTO 840
            CALL ALLRD(TMIP,ENT,M,DELETE)
            MIP=TMIP
            IF (GROUPF) GOTO (810,820,830) GSSTAT
 810        CONTINUE
            GOTO 840
 820        CONTINUE
C           the complete group should be returned
C           cancel compi from scartch file
            CALL ZSFLAG(.TRUE.,OK)
            CALL GETGRP(GPMI,OK)
            GOTO 840
 830        CONTINUE
C           grouped entity not allowed
            DICTN=60
            CALL DEPRNT(DICTN)
 840        CONTINUE
C           recover index record for instance
         ELSE
            CALL DSE880(ENT,X,Y,M,.FALSE.,OK,.FALSE.,D1,LIM,LIMSD)
C22222222222222
         END IF
C      WRITE(10,*) '[DSE800] HIT:',OK
C        check if search found anything.
         IF ( OK ) THEN
            RETURN
         ENDIF
C
C11111111111
      END IF
C+++++++++++
 20   IF (DFPK.LT.(LDFILE(CVPN)-1)) GOTO 10
C
      OK=.FALSE.
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE DSE801(X,Y,OK)
C     =========================
C1      vartype         R R  L
C1      iostatus        I I  O
C1
C
C2    Subroutine DSE801 carries out a search for entities
C2    within a region close to the point in world space defined
C2    by X,Y. The  first entity found in the search zone
C2    is flagged by the MI pointer being stored
C2    in the scratch file attached to unit SWINDU
C2    available from common block SWIND.
C2    Only entity types enabled for searching purposes will be
C2    considered.
C2    Searhes only for LINES or ARCS
C2    NOTE: this routine searches only the contents of the
C2    display file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/curwin.inc'
      include  'include/viewport.inc'
C
      REAL X,Y,X1,Y1,X2,Y2,MPX,MPY,BX,BY,LIM,
     +     DISTXY,DIST,CD0D13,M(3,3)
C
      INTEGER*2 ENT,TMIP,DFPK,D1,GPMI,TYPE
      INTEGER*4 MOD,DICTN
      LOGICAL OK,TOTAL,CHKLN,ARCTPT,DELETE,GROUPF
	  REAL LIMSD
C
      INTRINSIC ATAN,ABS,MOD
C
      EXTERNAL RDISPF,STOWVX,DIR500,DBR500,CD0D13,CHKLN,SSFLAG
      EXTERNAL ARCTPT,DISTXY,TEXBOX
C
      OK=.FALSE.
C
      IF ( LDFILE(CVPN) .EQ. 1 ) RETURN
C     set size of search zone around hit point
      CALL STOWVX(XWIDTH,LIM)
C
      TYPE=2
C     cycle through the display file
      DFPK=0
C
 10   CONTINUE
      DFP=MOD(DFP+0,LDFILE(CVPN)-1)+DFPINC
C
      DFPK=DFPK+1
C
C     read next display file entry
      CALL RDISPF(DFP,ENT,TMIP,OK)
      IF (.NOT.OK) THEN
         RETURN
      ENDIF
C
C      WRITE(10,*) '[DSE801]DFP,ENT,TMIP,ENSRCH(ENT)',
C     +                     DFP,ENT,TMIP,ENSRCH(ENT)
C
      IF ( (ENT.EQ.LINE   .OR. ENT.EQ.ARC   .OR. ENT.EQ.CENLIN .OR.
     +      ENT.EQ.SPLINE .OR. ENT.EQ.COMPI .OR. ENT.EQ.SYMBI  .OR.
     +      ENT.EQ.MARKER .OR. ENT.EQ.ELIPSE)
     1              .AND.TMIP.GT.0       ) THEN
C        Search is valid
C        test for delete status
C      WRITE(10,*) 'Calling ALLRD :TMIP,ENT',TMIP,ENT
         CALL ALLRD(TMIP,ENT,M,DELETE)
C      WRITE(10,*) 'Left ALLRD: TMIP,ENT',TMIP,ENT,DELETE
C
C        test for delete or construct status
C        if deleted or component master then ignore.
C        ignore grouped entities entirely if required
         IF ( DELETE )GOTO 20
C        test for group status condition
         GROUPF=(IMBUFF(1).EQ.GROUP)
C        save pointer to group parent MI
         GPMI=IMBUFF(8)
C        now get into the search proper
         IF ( ENT.EQ.COMPI.OR.ENT.EQ.SYMBI ) THEN
			CALL DSE066(TYPE,M,X,Y,OK,LIM,LIMSD)
           
         ELSE
            CALL DSE881(ENT,X,Y,OK,.FALSE.,D1)
         END IF
C        check if search successful
         IF ( OK ) RETURN
C
C     *****************************************
C           end of search
C     *****************************************
      END IF
C
 20   CONTINUE
      IF (DFPK.LT.(LDFILE(CVPN)-1)) GOTO 10
      OK=.FALSE.
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE DSE880(ENT,X,Y,M,USIT,OK,INST,MMIP,LIM,LIMSD)
C     ========================================================
C1     vartyp           I2  R R  L  L    I2
C1     iostatus          I  I I  O  I    I
 
C2    Subroutine DSE800 carries out a search for entities
C2    within a region close to the point in world space defined
C2    by X,Y. The  first entity found in the search zone
C2    is flagged by the MI pointer being stored
C2    in the scratch file attached to unit SWINDU
C2    available from common block SWIND.
C2    Only entity types enabled for searching purposes will be
C2    considered.
C2    NOTE: this routine searches only the contents of the
C2    display file.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/entity.inc'
      include   'include/curwin.inc'
      include   'include/marker.inc'
      include   'include/movdat.inc'
C
C
      INTEGER*2 ENT,POINT,STPNT,TMIP,D1,GPMI,MMIP,I,J,L,TPDP(2),
     +          LPDP,FPDP,PNTC,SEQP,SPDP,STP,K,MPDP,MIND,
     +          MODE,TYPE,TTMIP,
     +          IBUF1(4),CLTYPE
      INTEGER*4 LCRDAT,I2I4,DICTN,LOGIC,ST
      REAL X,Y,X1,Y1,BX,BY,LIM,CD0D13,MPX,MPY,M(3,3),
     +     DISTXY,DIST,DIST1,MAXDIS,XP1,YP1,HPX,HPY,
     +     A(3,3),B(3,3),C(3,3),NFX1,NFY1,NSX1,NSY1,
     +     BX1,BX2,BX3,BX4,BY1,BY2,BY3,BY4,LIMSD,PI,
     +     CENX,CENY,RAD,INCANG,ANG,BRDR,DUMMY,X2,Y2,ENANG,CANG,RCX,RCY,
     +     RARAY1(6),
     +     BUFFER(6,4),XP,YP,TW,R1,R2,CDSD13,
     +     MATRIX(4,4),GEOM(3,4),OX,OY,INC,NX,NY,OPX,OPY,
     +     SEND,SSTA,CX,CY,RRAD
      LOGICAL OK,CHKLN,GROUPF,SAME,ARCTPT,INST,USIT,
     +        FLAGIT,L1,CFLAG,ZERO,LOK,
     +        HIT,USET,LAST,FIRSTS,FIRST
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,INCA,STA,ENDA,
     +                 DX1,DY1,DX2,DY2,DDIST,DXV1,DYV1,DPI,DXX1,DYY1,
     +                 DMPX,DMPY
C
      EXTERNAL STOWVX,CHKLN,CD0D13,SAME,DISTXY,ARCTPT,LOGIC,DPI,PI,CANG
C
C     set size of search zone around hit point
      FLAGIT=.FALSE.
      CFLAG=.FALSE.
C     set end code to enclosed as default
      ENDCOD=0
C
      GROUPF=(IMBUFF(1).EQ.GROUP)
C     save pointer to group parent MI
      GPMI=IMBUFF(8)
C
C      WRITE(10,*) '[DSE880]',ENT,X,Y,INST,MMIP
C     ****************************************
      IF ( ENT .EQ. LINE ) THEN
C     ****************************************
C        Searching for a line.
         IF ( ABS(RDBUFF(1)-RDBUFF(4)).LT.
     +       ABS(RDBUFF(2)-RDBUFF(5))     ) THEN
            OK=CHKLN(RDBUFF(2),RDBUFF(5),Y)
         ELSE
            OK=CHKLN(RDBUFF(1),RDBUFF(4),X)
         END IF
C
         LOK = SAME( RDBUFF(1),RDBUFF(4)).AND.
     +         SAME( RDBUFF(2),RDBUFF(5))
 
         IF(.NOT.OK.OR.LOK) THEN
             OK = .FALSE.
             RETURN
         ENDIF
         IF (  ABS(CDSD13(RDBUFF(1),RDBUFF(2),
     1         RDBUFF(4),RDBUFF(5),X,Y)).LE.LIMSD) THEN
C           line has been hit.
C           set code for end hit,if line
C           find closest end point
            R1=DISTXY(X,Y,RDBUFF(1),RDBUFF(2))
            R2=DISTXY(X,Y,RDBUFF(4),RDBUFF(5))
            IF (R1.LE.R2) THEN
C              set first point nearest
               ENDCOD=1
            ELSE
C              set 2nd point nearest
               ENDCOD=2
            END IF
C           ensure line is flagged
            FLAGIT=.TRUE.
         END IF
C
C     ****************************************
      ELSE IF ( ENT.EQ.MARKER ) THEN
C     ****************************************
 
C           Searching for marker
C           Copy data since clipping alters end point values
            X1=RDBUFF(1)
            Y1=RDBUFF(2)
            MIND=MRKIND(IMBUFF(5))
            IF (MIND.EQ.0) THEN
               IF (DISTXY(X1,Y1,X,Y).LT.LIM ) THEN
                  FLAGIT = .TRUE.
                  OK = FLAGIT
               END IF
            ELSE
C
C              scale the point
               CALL SCAL2D(RDBUFF(5),RDBUFF(6),A)
C              rotate the point
               CALL ROT2D(RDBUFF(4),B)
C              Concatenate result
               CALL MULT3M(A,B,C)
c              translate the point
               CALL TRAN2D(X1,Y1,B)
C              Concatenate result
               CALL MULT3M(C,B,A)
               CALL NEWXY(MRKR(MIND,1),MRKR(MIND,2),BX1,BY1,A)
               CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND+1,2),BX2,BY2,A)
               CALL NEWXY(MRKR(MIND,1),MRKR(MIND+1,2),BX3,BY3,A)
               CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND,2),BX4,BY4,A)
D         WRITE(10,*) '[SRCHWN] BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4 ',
D     +                BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4
C      write(10,*) '[SRCHWN] XMIN,YMIN,XMAX,YMAX',XMIN,YMIN,XMAX,YMAX
C     This is a check to see if it is worth drawing the
C     marker or not by checking its surounding box
               IF(X.GT.MIN(BX1,BX2,BX3,BX4).AND.
     +            Y.GT.MIN(BY1,BY2,BY3,BY4).AND.
     +            X.LT.MAX(BX1,BX2,BX3,BX4).AND.
     +            Y.LT.MAX(BY1,BY2,BY3,BY4)) THEN
C                 hit inside area
               CALL NEWXY(MRKR(MIND+2,1),MRKR(MIND+2,2),NSX1,NSY1,A)
               DO 20 I = MIND+3,MIND+MRKI(MIND)+2
                  CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,A)
                  IF(MRKI(I).EQ.1) THEN
C                     OK=(CHKLN(NSX1,NFX1,X).AND.CHKLN(NSY1,NFY1,Y))
C                     WRITE(10,*) '[DSE880] OK = ',OK,NSX1,NFX1,X,
C     +               NSY1,NFY1,Y
C                     IF ( OK .AND.
                      IF(ABS(CD0D13(NSX1,NSY1,NFX1,NFY1,
     2                      X,Y)).LT.LIM) THEN
C
                      FLAGIT=.TRUE.
                      OK=.TRUE.
                      GOTO 21
                     END IF
                  END IF
                  NSX1=NFX1
                  NSY1=NFY1
  20           CONTINUE
  21           CONTINUE
            END IF
         END IF
C     ****************************************
      ELSE IF ( ENT.EQ.ARC ) THEN
C     ****************************************
C        Searching for arcs
C        test for hit within radial limit of centre
         DIST=ABS(DISTXY(RDBUFF(1),RDBUFF(2),
     +               X,Y)-RDBUFF(4))
         IF (DIST.GT.LIM) GOTO 2000
C        test for hit close to circumference
         OK = DIST.LE.LIM .AND.
     +        ARCTPT(RDBUFF(1),RDBUFF(2),
     1               RDBUFF(5),RDBUFF(6),X,Y)
 
         IF ( OK ) THEN
C           ensure entity is flagged
            FLAGIT=.TRUE.
         END IF
C     ****************************************
      ELSE IF ( ENT.EQ.ELIPSE ) THEN  
C     ****************************************
C        Searching for ellipse
C        test for hit within radial limit of centre
          CX = RDBUFF(1)
          CY=  RDBUFF(2)
          RRAD = RDBUFF(4)
          SSTA = RDBUFF(5)
          SEND = RDBUFF(6)
                                     
          TTMIP = IDBUFF(3)
          CALL DBR550(TTMIP,IBUF1,RARAY1,OK)

          BX1 = CX-MAX(RRAD,RARAY1(1))
          BY1 = CY-MAX(RRAD,RARAY1(1))
          BX2 = CX+MAX(RRAD,RARAY1(1))
          BY2 = CY+MAX(RRAD,RARAY1(1))
         IF(X.GT.BX1.OR.X.LT.BX2.OR.Y.GT.BY1.OR.Y.LT.BY2) GOTO 2000
          
C        test for hit close to circumference
         XC1 = DBLE(CX)
         YC1 = DBLE(CY)
         RAD1 = DBLE(RRAD)
         STA = DBLE(SSTA)
         ENDA = DBLE(SEND)
         IF (SAME(SSTA,PI(2.0))) ENDA = DPI(2.0D0)
         RAD2 = DBLE(RARAY1(1))
         INCA = DBLE(RARAY1(2))
         DX1 = DBLE(X)
         DY1 = DBLE(Y)
         CALL DDNTE(XC1,YC1,RAD1,RAD2,INCA,STA,ENDA,
     +            DX1,DY1,DDIST,DXV1,DYV1,DXX1,DYY1,ST)
         OK = (ST.GT.0.AND. REAL(DDIST).LE.LIM )
         IF ( OK ) THEN
            DDIST = DABS(DDIST)
C           ensure entity is flagged
            FLAGIT=.TRUE.
         END IF
C     ****************************************
      ELSE IF ( ENT .EQ. SPLINE) THEN
C     ****************************************
C        Searching for B_Spline
C        Get the mode of the curve type either
C        0 points 1 polygon 2 curve 3 curve+ploygon
         MODE=MOD(IMBUFF(5)+0,4)
         TYPE=IMBUFF(5)-MODE
C        thunb weigth stored in the first point
         TW=RDBUFF(6)
C        set the main find flag to false
C         WRITE(10,*) '[SPLINE CALLED MODE= ',MODE
C         WRITE(10,*) '[SPLINE THUMB WEIGHT= ',TW
         FLAGIT=.FALSE.
C        current point counter
         PNTC=3
C        temorary part data pointer arrays
         TPDP(1)=0
         TPDP(2)=0
C        maximum distance from the hull allowable
C        this is defined as the box limits
         MAXDIS=MAX(ABS(RDBUFF(2)-RDBUFF(4)),
     +         ABS(RDBUFF(2)-RDBUFF(5)) )
         IF(SAME(MAXDIS,0.0)) CALL STOWVX(500.0,MAXDIS)
C        read past the first 2 control points
 109     CONTINUE
C        save the first part data pointer
         MPDP=IMBUFF(7)
C        next point to read
         POINT=IDBUFF(3)
         CALL DBR500(POINT,OK)
C        time to check box limits unless we are using a tranlation
         IF(.NOT.USIT) THEN
C           could this be an old curve ( all zero )
             IF(.NOT.(SAME(RDBUFF(1),0.0).AND.
     +          SAME(RDBUFF(2),0.0).AND.
     +          SAME(RDBUFF(4),0.0).AND.
     +          SAME(RDBUFF(5),0.0)) ) THEN
C                  are we out of box limits
                IF((X.LT.RDBUFF(1).AND.X.GT.RDBUFF(4)).AND.
     +             (Y.LT.RDBUFF(2).AND.Y.GT.RDBUFF(5)) ) THEN
                      GOTO 115
                ENDIF
             ENDIF
         ENDIF
 
         POINT=IDBUFF(3)
         CALL DBR500(POINT,OK)
         IF ( USIT ) CALL MV0003(RDBUFF,M)
C        load the first point
         X1=RDBUFF(1)
         Y1=RDBUFF(2)
C        save the first point
         FPDP=POINT
 111     CONTINUE
C        main search loop for all types
         LPDP=POINT
         POINT=IDBUFF(3)
C        increment point count
         PNTC=PNTC+1
C        Get the next value. What if this is a one point curve
         IF(POINT.GT.0) THEN
             CALL DBR500(POINT,OK)
             IF ( USIT ) CALL MV0003(RDBUFF,M)
C            is it the same point
             OK=(SAME(RDBUFF(1),X1).AND.SAME(RDBUFF(2),Y1))
         ELSE
             OK=.FALSE.
         ENDIF
         IF(.NOT.OK) THEN
C            what mode are we
             IF(MODE.EQ.0) THEN
C               points only
                XP1=RDBUFF(1)
                YP1=RDBUFF(2)
C               does point lie in cursor hit box ?
                OK=(CHKLN(X+LIM,X-LIM,XP1).AND.
     +          CHKLN(Y+LIM,Y-LIM,YP1)).OR.(CHKLN(X+LIM,X-LIM,X1).AND.
     +          CHKLN(Y+LIM,Y-LIM,Y1))
                IF (OK) THEN
C                   yup go and flag
                    FLAGIT=.TRUE.
                    GOTO 115
                ENDIF
             ELSE
                IF(MODE.EQ.2.OR.MODE.EQ.3) THEN
C                   This is either curve or polygon and curve
                    DIST=ABS(CD0D13(RDBUFF(1),RDBUFF(2),X1,Y1,X,Y))
c                   Set the distance from the current hull line
                    IF(DIST.LT.MAXDIS) THEN
C                       Is the hit point within the limits of the hull
                        IF ( SAME(RDBUFF(1),X1)   ) THEN
                           OK=CHKLN(RDBUFF(2),Y1,Y)
                        ELSE IF ( SAME(RDBUFF(2),Y1) ) THEN
                           OK=CHKLN(RDBUFF(1),X1,X)
                         ELSE
                           OK=CHKLN(RDBUFF(1),X1,X).OR.
     +                      CHKLN(RDBUFF(2),Y1,Y)
                        END IF
                        IF(OK) THEN
C                          we have a valid minimum distance from
C                          the hit point to the hull
                           MAXDIS=DIST
C                          store the sequence number of this point
                           SEQP=PNTC-1
C                          store the current part data pointers
                           TPDP(1)=LPDP
                           TPDP(2)=POINT
                        ENDIF
                    END IF
                ENDIF
C               Tis is either a polygon or polygon and curve
                IF(MODE.EQ.1.OR.MODE.EQ.3) THEN
C                  check hull limits
                   IF ( ABS(RDBUFF(1)-X1).LT.
     +             ABS(RDBUFF(2)-Y1)) THEN
                       OK=CHKLN(RDBUFF(2),Y1,Y)
                   ELSE
                       OK=CHKLN(RDBUFF(1),X1,X)
                   END IF
C
                   IF ( OK .AND.
     +                ABS(CD0D13(RDBUFF(1),RDBUFF(2),
     1                X1,Y1,X,Y)).LE.LIM) THEN
C                     line has been hit.
                       FLAGIT=.TRUE.
                       GOTO 115
                   END IF
                ENDIF
             ENDIF
         ENDIF
C        If we are at the end ofthe run then start to locate the spline
c        update the old point
         X1=RDBUFF(1)
         Y1=RDBUFF(2)
 
         IF (IDBUFF(3).NE.0) GOTO 111
         IF(MODE.EQ.0.OR.MODE.EQ.1) GOTO 115
C        Have we got any point which migth be in the vicinity
         IF(TPDP(1).EQ.0) THEN
             FLAGIT=.FALSE.
             GOTO 115
         ENDIF
C        possible do a check on the for points which
         CALL DBR500(TPDP(1),OK)
         IF ( USIT ) CALL MV0003(RDBUFF,M)
         DIST=DISTXY(RDBUFF(1),RDBUFF(2),X,Y)
         CALL DBR500(TPDP(2),OK)
         IF ( USIT ) CALL MV0003(RDBUFF,M)
         DIST1=DISTXY(RDBUFF(1),RDBUFF(2),X,Y)
C        the smallest distance to the point will be the
C        point we use for interpolation.
         SEQP=SEQP+LOGIC(DIST.GT.DIST1)
C        Obtain the starting point for interpolation
         STP=((SEQP-1)*LOGIC(SEQP.GT.4.AND.SEQP.LT.(PNTC-1)))+
     +      (3*LOGIC(SEQP.LE.4))+((PNTC-2)*LOGIC(SEQP.GE.(PNTC-1)))
C        draw four crosses to see if we are right
         PNTC=1
C        do we have the points needed
         FIRSTS=.TRUE.
         HIT=.FALSE.
         POINT=MPDP
118      CONTINUE
         CALL DBR500(POINT,OK)
         IF ( USIT ) CALL MV0003(RDBUFF,M)
         IF(PNTC.EQ.STP-1.AND.TYPE.EQ.24) THEN
C            We have two segments to interpolate
             CALL SETM24(MATRIX)
C            WRITE(10,*) '[POLY24] INCREMENT 0.3:',INC
C            start at the first point
C            load up the buffers
             IF(STP.EQ.3) THEN
                POINT=IDBUFF(3)
                CALL DBR500(POINT,OK)
             ENDIF
             IF ( USIT ) CALL MV0003(RDBUFF,M)
             POINT=IDBUFF(3)
             CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,1),OK)
             IF(PNTC.EQ.2) THEN
                 FIRSTS=.FALSE.
                 CALL MLOAD6(BUFFER(1,1),BUFFER(1,2))
             ELSE
                 CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,2),OK)
             ENDIF
             CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,3),OK)
             CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,4),OK)
             OX=BUFFER(1,2)
             OY=BUFFER(2,2)
             OPX=OX
             OPY=OY
             NX=BUFFER(1,3)
             NY=BUFFER(2,3)
C            load up the geometric info
             CALL LOADG(GEOM,BUFFER)
C            Draw the special case first
             DIST=DISTXY(NX,NY,OPX,OPY)
             CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
             IF(FLAGIT) GOTO 115
C            Move the values one up to accomodate
C            the new one to be read.
             DO 1005 I=1,3
                CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
1005         CONTINUE
C
             IF(POINT.EQ.0) THEN
                 CALL MLOAD6(BUFFER(1,3),BUFFER(1,4))
                 CALL LOADG(GEOM,BUFFER)
                 OPX=BUFFER(1,2)
                 OPY=BUFFER(2,2)
                 NX=BUFFER(1,4)
                 NY=BUFFER(2,4)
                 DIST=DISTXY(NX,NY,OPX,OPY)
                 CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
                 IF(FLAGIT) GOTO 115
                 CALL BLOAD1(GEOM)
                 CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
                 IF(FLAGIT) GOTO 115
                 CALL BLOAD2(GEOM)
                 CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
                 IF(FLAGIT) GOTO 115
             ELSE
                 CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,4),OK)
C
                 CALL LOADG(GEOM,BUFFER)
                 NX=BUFFER(1,3)
                 NY=BUFFER(2,3)
                 OPX=BUFFER(1,2)
                 OPY=BUFFER(2,2)
                 DIST=DISTXY(NX,NY,OPX,OPY)
                 CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
                 IF(FLAGIT) GOTO 115
             ENDIF
             GOTO 115
         ELSE IF(PNTC.EQ.STP.AND.TYPE.EQ.16) THEN
C           ******************
C           HERMITE CURVE PICK
C           ******************
            LAST=.FALSE.
C      WRITE(10,*) '[DSE881]  NEW CURVE'
            CALL SETM16(MATRIX)
C           load up the first four points
            CALL DBR500(POINT,OK)
            IF ( USIT ) CALL MV0003(RDBUFF,M)
            POINT=IDBUFF(3)
            CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,1),OK)
            CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,2),OK)
C           last point perhaps is being asked of as major point
            L1 = (POINT.EQ.0)
            CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,3),OK)
C           get increment
            CALL LOADGH(TW,GEOM,BUFFER,LAST)
            OX=BUFFER(1,1)
            OY=BUFFER(2,1)
            OPX=OX
            OPY=OY
            NX=BUFFER(1,2)
            NY=BUFFER(2,2)
            DIST=DISTXY(NX,NY,OPX,OPY)
            CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
C           load up for the first point
            DO 2006 I=1,2
               CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
2006        CONTINUE
            CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,3),OK)
            CALL LOADGH(TW,GEOM,BUFFER,LAST)
            OPX=BUFFER(1,1)
            OPY=BUFFER(2,1)
            NX=BUFFER(1,2)
            NY=BUFFER(2,2)
            DIST=DISTXY(NX,NY,OPX,OPY)
            CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
            IF(FLAGIT) GOTO 115
C
            DO 2005 I=1,2
               CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
2005        CONTINUE
C
            CALL PREL(POINT,USIT,M,RDBUFF,BUFFER(1,3),OK)
C           if the last point has been loaded then tell em
            LAST=L1
            CALL LOADGH(TW,GEOM,BUFFER,LAST)
            OPX=BUFFER(1,1)
            OPY=BUFFER(2,1)
            NX=BUFFER(1,2)
            NY=BUFFER(2,2)
            DIST=DISTXY(NX,NY,OPX,OPY)
            CALL INTSPL(GEOM,MATRIX,OX,OY,DIST,X,Y,FIRSTS,FLAGIT)
            GOTO 115
         ENDIF
         PNTC=PNTC+1
         POINT=IDBUFF(3)
         GOTO 118
115      CONTINUE
C        set end code to enclosed
         ENDCOD=0
C
C     ****************************************
      ELSE IF ( ENT .EQ. CENLIN ) THEN
C     ****************************************
C        Searching for a Center Line.
         CENX = RDBUFF(1)
         CENY = RDBUFF(2)
         RAD = RDBUFF(3)
         INCANG = 0
         ANG = RDBUFF(5)
C        Get the extension and convert it from paper to world.
         CALL PAP2SC(RDBUFF(6),DIST)
         CALL SC2WO(DIST,0.0,BRDR,DUMMY)
         CALL SC2WO(0.0,0.0,DUMMY,DIST)
         BRDR = BRDR - DUMMY
         CLTYPE = IDBUFF(4)
C
         IF (CLTYPE.EQ.3) THEN
C           PCD straight line section.
            DUMMY = RAD
C           Find the referance arc center point.
            CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,RCX,RCY)
            CENX = RCX
            CENY = RCY
            RAD = RDBUFF(4)
         ENDIF
C
         FIRST = .TRUE.
 50      CONTINUE
C           Firstly ... Find The Line.
C           Find finish point.
            RAD = RAD + BRDR
            DUMMY = RAD
            CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,X2,Y2)
C           and start point is the same dist again,  other direction.
            X1 = CENX - (X2 - CENX)
            Y1 = CENY - (Y2 - CENY)
C
C           Now Look For A Hit.
            IF ( ABS(X2-X1) .LT. ABS(Y2-Y1) ) THEN
               OK=CHKLN(Y1,Y2,Y)
            ELSE
               OK=CHKLN(X1,X2,X)
            END IF
C        
            IF (OK .AND. (ABS(CDSD13(X1,Y1,X2,Y2,X,Y)).LE.LIMSD) ) THEN
C              line has been hit.
C              set code for end hit,if line
C              find closest end point
               R1=DISTXY(X,Y,X1,Y1)
               R2=DISTXY(X,Y,X2,Y2)
               IF (R1.LE.R2) THEN
C                 set first point nearest
                  ENDCOD=1
               ELSE
C                 set 2nd point nearest
                  ENDCOD=2
               END IF
C              ensure line is flagged
               FLAGIT=.TRUE.
            END IF
C
C        First line dealt with, how about a second?
         IF (FIRST .AND. (CLTYPE.EQ.1)) THEN
C           Second line of a cross center line.
            RAD = RDBUFF(4)
            ANG = ANG + PI(0.5)
            FIRST = .FALSE.
            GOTO 50              
         ELSE IF (CLTYPE.EQ.3 .AND. (.NOT.FLAGIT)) THEN
C           PCD ... the curved bit. (Miss this bit if already found.)
C           test for hit within radial limit of centre
            DIST=ABS(DISTXY(RDBUFF(1),RDBUFF(2),
     +                  X,Y)-RDBUFF(3))
            IF (DIST.GT.LIM) GOTO 2000
C           Find start and finish point.
            XC1 = DBLE(RDBUFF(1))
            YC1 = DBLE(RDBUFF(2))
            RAD1 = DBLE(RDBUFF(3))
            XC2 = DBLE(CENX)
            YC2 = DBLE(CENY)
            RAD2 = DBLE(RAD)
            CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2,OK)
            X1 = REAL(DX1)
            Y1 = REAL(DY1)
            X2 = REAL(DX2)
            Y2 = REAL(DY2)
C           Convert the points to angles,
            ANG = CANG(RDBUFF(1),RDBUFF(2),X1,Y1)
            ENANG = CANG(RDBUFF(1),RDBUFF(2),X2,Y2)
C           test for hit close to circumference
            OK = DIST.LE.LIM .AND.
     +           ARCTPT(RDBUFF(1),RDBUFF(2),
     1                  ANG,ENANG,X,Y)
            IF ( OK ) THEN
C              ensure entity is flagged
               FLAGIT=.TRUE.
            END IF
         ENDIF
c
         IF (.NOT.OK) RETURN
C
C     ****************************************
      ELSE IF ( ENT .EQ. HATCH ) THEN
C     ****************************************
C        Searching for hatch
         IF ( USIT ) CALL MV0003(RDBUFF,M)
         OK =X.GT.RDBUFF(1)  .AND.
     +       X.LT.RDBUFF(4)  .AND.
     1       Y.GT.RDBUFF(2)  .AND.
     2       Y.LT.RDBUFF(5)
         IF ( OK ) THEN
C              hit inside area
 22            CONTINUE
               IF ( IDBUFF(3) .NE. 0 ) THEN
                  POINT = IDBUFF(3)
                  CALL DBR500(POINT,OK)
                  IF ( USIT ) CALL MV0003(RDBUFF,M)
                  IF ( ABS(RDBUFF(1)-RDBUFF(4)) .LT.
     +                 ABS(RDBUFF(2)-RDBUFF(5)) ) THEN
                     OK=CHKLN(RDBUFF(2),RDBUFF(5),Y)
                  ELSE
                     OK=CHKLN(RDBUFF(1),RDBUFF(4),X)
                  END IF
C
               IF ( OK .AND.
     +           ABS(CD0D13(RDBUFF(1),RDBUFF(2),
     1                      RDBUFF(4),RDBUFF(5),
     2                   X,Y)).LT.LIM) THEN
C
                   FLAGIT=.TRUE.
               ELSE
                  GOTO 22
               END IF
C
            ELSE
               OK=.FALSE.
            END IF
         END IF
C     *****************************************************
      ELSE IF (ENT.EQ.LDIMN.OR.ENT.EQ.ADIMN.OR.ENT.EQ.RDIMN
     +    .OR. ENT.EQ.DDIMN.OR.ENT.EQ.GLABEL) THEN
C     *****************************************************
C           examine Iwork array to get no. of text strings stored
C           first equate to 4 byte integer
            LCRDAT=I2I4(IWORK(4,1))
            I=MOD(LCRDAT,256)/16
C           set header record offset
            IF ( ENT .EQ. ADIMN ) THEN
               J=2
               L=3
            ELSE
               J=1
               L=2
            END IF
 405        CONTINUE
            J=J+1
            CALL CHKTEX(X,Y,RWORK(1,J),RWORK(2,J),PAPTOW*RWORK(3,J),
     +                  PAPTOW*RWORK(4,J),RWORK(5,J),
     +                  RWORK(6,J),OK)
C           check for hit on another dimension text string
            IF ( (.NOT.OK) .AND. (J-1 .LT. I ) ) GOTO 405
C
            IF ( OK ) THEN
               CALL TEXBOX(RWORK(1,L),RWORK(2,L),PAPTOW*RWORK(3,L),
     +                  PAPTOW*RWORK(4,L),RWORK(5,L),
     +                  RWORK(6,L),MPX,MPY,BX,BY)
               FLAGIT=.TRUE.
               CFLAG=.TRUE.
            END IF
C
C     ****************************************
         ELSE IF ( ENT .EQ. TEXT ) THEN
C     ****************************************
C           Searching for text
C           find enclosing box
            CALL CHKTEX(X,Y,RDBUFF(1),RDBUFF(2),PAPTOW*RDBUFF(3),
     +                  PAPTOW*RDBUFF(4),RDBUFF(5),RDBUFF(6),OK)
            IF ( OK ) THEN
               FLAGIT=.TRUE.
            END IF
C     ****************************************
       END IF
C     ****************************************
C
        IF(FLAGIT) THEN
           IF (GROUPF) GOTO (110,120,130,140) GSSTAT
C
 110        CONTINUE
C           found a line,save the MI pointer,flag and return
            IF ( INST ) THEN
               MIP=MMIP
            ELSE
               MIP=IDBUFF(2)
               IF (CFLAG) MIP = IWORK(2,1)
            END IF
C           flag the screen location
            TMIP=MIP
            BX=0.0
            BY=0.0
C           ensure correct end code used
            D1=ENDCOD
            CALL SSFLAG(TMIP,BX,BY,DFP,D1,.TRUE.)
C           return with data in buffer
            GOTO 150
 120        CONTINUE
C           the complete group should be returned
            DICTN=76
            CALL DCPRNT(DICTN)
            CALL GETGRP(GPMI,OK)
            GOTO 150
 130        CONTINUE
C           grouped entity not allowed
            DICTN=60
            CALL DEPRNT(DICTN)
            CALL ZSFLAG(.TRUE.,OK)
            GOTO 2000
 140        CONTINUE
C           the complete group should be flagged,
C           and the header returned
            DICTN=76
            CALL DCPRNT(DICTN)
            CALL GETGRP(GPMI,OK)
            IF(.NOT.OK) RETURN
C           read the MI data before return
            CALL DIR500(GPMI,OK)
C           all finished entity has been flagged
 150        RETURN
      ENDIF
C     no entity has been flagged tell 'em about it
 2000 CONTINUE
      OK=.FALSE.
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE DSE881(ENT,X,Y,OK,INST,MMIP)
C     =======================================
C1     vartyp           I2  R R  L  L    I2
C1     iostatus          I  I I  O  I    I
 
C2    Subroutine DSE800 carries out a search for entities
C2    within a region close to the point in world space defined
C2    by X,Y. The  first entity found in the search zone
C2    is flagged by the MI pointer being stored
C2    in the scratch file attached to unit SWINDU
C2    available from common block SWIND.
C2    Only entity types enabled for searching purposes will be
C2    considered.
C2    NOTE: this routine searches only the contents of the
C2    display file.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/entity.inc'
      include   'include/curwin.inc'
      include   'include/viewport.inc'
      include   'include/marker.inc'
 
      INTEGER*2 ENT,POINT,STPNT,TMIP,D1,GPMI,MMIP,MODE,PNTC,I,MIND,
     +          CLTYPE
      REAL X,Y,X1,Y1,X2,Y2,BX,BY,LIM,CD0D13,MPX,MPY,M(3,3),DISTXY,DIST
      REAL A(3,3),B(3,3),C(3,3),NFX1,NFY1,NSX1,NSY1,
     +        BX1,BX2,BX3,BX4,BY1,BY2,BY3,BY4, ZERO, ENANG,
     +        ANG,DUMMY,RAD, RCX,RCY, INCANG,CANG,PI, BRDR, CENX,CENY
      DOUBLE PRECISION DMPX,DMPY, XC1,YC1,RAD1, XC2,YC2,RAD2,
     +                 DX1,DY1, DX2,DY2
      LOGICAL OK,CHKLN,GROUPF,SAME,ARCTPT,INST,FOUND,TVPA,FIRST
C
      INTRINSIC ABS
C
      EXTERNAL STOWVX,CHKLN,CD0D13,SAME,DISTXY,ARCTPT,CANG,PI
C
C     set size of search zone around hit point
      TVPA = VPADD
      VPADD = .FALSE.
      CALL STOWVX(XWIDTH,LIM)
C
      GROUPF=(IMBUFF(1).EQ.GROUP)
C     save pointer to group parent MI
      GPMI=IMBUFF(8)
C
      OK=.FALSE.
      FOUND=OK
C
C     ****************************************
      IF ( ENT .EQ. LINE ) THEN
C     ****************************************
C
C        Searching for a line
         IF ( ABS(RDBUFF(1)-RDBUFF(4)).LT.
     +      ABS(RDBUFF(2)-RDBUFF(5))     ) THEN
            OK=CHKLN(RDBUFF(2),RDBUFF(5),Y)
         ELSE
            OK=CHKLN(RDBUFF(1),RDBUFF(4),X)
         END IF
         IF ( SAME(DISTXY(RDBUFF(1),RDBUFF(2),
     +                         RDBUFF(4),RDBUFF(5) )
     +             , ZERO(0.0)) )THEN
             OK = .FALSE.
             RETURN
         ENDIF
 
         IF (OK.AND.
     +      ABS(CD0D13(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     2                   X,Y)).LT.LIM) THEN
C           found a line,save the MI pointer
            IF ( INST ) MIP=MMIP
C           save the MI pointer and flag position in work file
            TMIP=MIP
C           return with data in buffer
         ELSE
            OK=.FALSE.
         END IF
C
C     ****************************************
      ELSE IF ( ENT.EQ.MARKER ) THEN
C     ****************************************
 
C           Searching for marker
C           Copy data since clipping alters end point values
            X1=RDBUFF(1)
            Y1=RDBUFF(2)
            MIND=MRKIND(IMBUFF(5))
C
C           scale the point
            CALL SCAL2D(RDBUFF(5),RDBUFF(6),A)
C           rotate the point
            CALL ROT2D(RDBUFF(4),B)
C           Concatenate result
            CALL MULT3M(A,B,C)
c           translate the point
            CALL TRAN2D(X1,Y1,B)
C           Concatenate result
            CALL MULT3M(C,B,A)
            CALL NEWXY(MRKR(MIND,1),MRKR(MIND,2),BX1,BY1,A)
            CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND+1,2),BX2,BY2,A)
            CALL NEWXY(MRKR(MIND,1),MRKR(MIND+1,2),BX3,BY3,A)
            CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND,2),BX4,BY4,A)
C      WRITE(10,*) '[SRCHWN] BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4 ',
C     +             BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4
C      write(10,*) '[SRCHWN] XMIN,YMIN,XMAX,YMAX',XMIN,YMIN,XMAX,YMAX
C     This is a check to see if it is worth drawing the
C     marker or not by checking its surounding box
            IF(X.GT.MIN(BX1,BX2,BX3,BX4).AND.
     +         Y.GT.MIN(BY1,BY2,BY3,BY4).AND.
     +         X.LT.MAX(BX1,BX2,BX3,BX4).AND.
     +         Y.LT.MAX(BY1,BY2,BY3,BY4)) THEN
C              hit inside area
            CALL NEWXY(MRKR(MIND+2,1),MRKR(MIND+2,2),NSX1,NSY1,A)
            DO 20 I = MIND+3,MIND+MRKI(MIND)+2
               CALL NEWXY(MRKR(I,1),MRKR(I,2),NFX1,NFY1,A)
               IF(MRKI(I).EQ.1) THEN
C                  OK=(CHKLN(NSX1,NFX1,X).AND.CHKLN(NSY1,NFY1,Y))
C                  IF ( OK .AND.
                   IF(ABS(CD0D13(NSX1,NSY1,NFX1,NFY1,
     2                   X,Y)).LT.LIM) THEN
                   OK=.TRUE.
C
                   IF ( INST ) MIP=MMIP
C                   save the MI pointer and flag position in work file
                   TMIP=MIP
C                  return with data in buffer
                   GOTO 21
                  END IF
               END IF
               NSX1=NFX1
               NSY1=NFY1
  20        CONTINUE
            OK=.FALSE.
  21        CONTINUE
         END IF
C
C     ****************************************
      ELSE IF ( ENT .EQ. CENLIN ) THEN
C     ****************************************
C        Searching for a Center Line.
         CENX = RDBUFF(1)
         CENY = RDBUFF(2)
         RAD = RDBUFF(3)
         INCANG = 0
         ANG = RDBUFF(5)
C        Get the extension and convert it from paper to world.
         CALL PAP2SC(RDBUFF(6),DIST)
         CALL SC2WO(DIST,0.0,BRDR,DUMMY)
         CALL SC2WO(0.0,0.0,DUMMY,DIST)
         BRDR = BRDR - DUMMY
         CLTYPE = IDBUFF(4)
C
         IF (CLTYPE.EQ.3) THEN
C           PCD straight line section.
            DUMMY = RAD
C           Find the referance arc center point.
            CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,RCX,RCY)
            CENX = RCX
            CENY = RCY
            RAD = RDBUFF(4)
         ENDIF
C
         FIRST = .TRUE.
 50      CONTINUE
C           Firstly ... Find The Line.
C           Find finish point.
            RAD = RAD + BRDR
            DUMMY = RAD
            CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,X2,Y2)
C           and start point is the same dist again,  other direction.
            X1 = CENX - (X2 - CENX)
            Y1 = CENY - (Y2 - CENY)
C
C           Now Look For A Hit.
            IF ( ABS(X2-X1) .LT. ABS(Y2-Y1) ) THEN
               OK=CHKLN(Y1,Y2,Y)
            ELSE
               OK=CHKLN(X1,X2,X)
            END IF
            IF (OK.AND.
     +         ABS(CD0D13(X1,Y1,X2,Y2,X,Y)).LT.LIM) THEN
C              found a line,save the MI pointer
               IF ( INST ) MIP=MMIP
C              save the MI pointer and flag position in work file
               TMIP=MIP
C              return with data in buffer
            ELSE
               OK=.FALSE.
            END IF
C        
C        First line dealt with, how about a second?
         IF (FIRST .AND. (CLTYPE.EQ.1) .AND. .NOT.OK) THEN
C           Second line of a cross center line.
            RAD = RDBUFF(4)
            ANG = ANG + PI(0.5)
            FIRST = .FALSE.
            GOTO 50              
         ELSE IF (CLTYPE.EQ.3 .AND. (.NOT.OK)) THEN
C           PCD ... the curved bit. (Miss this bit if already found.)
C           test for hit within radial limit of centre
            DIST=ABS(DISTXY(RDBUFF(1),RDBUFF(2),
     +                  X,Y)-RDBUFF(3))
            IF (DIST.GT.LIM) THEN
               OK = .FALSE.
               GOTO 200
            ENDIF
C           Find start and finish point.
            XC1 = DBLE(RDBUFF(1))
            YC1 = DBLE(RDBUFF(2))
            RAD1 = DBLE(RDBUFF(3))
            XC2 = DBLE(CENX)
            YC2 = DBLE(CENY)
            RAD2 = DBLE(RAD)
            CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2,OK)
            X1 = REAL(DX1)
            Y1 = REAL(DY1)
            X2 = REAL(DX2)
            Y2 = REAL(DY2)
C           Convert the points to angles,
            ANG = CANG(RDBUFF(1),RDBUFF(2),X1,Y1)
            ENANG = CANG(RDBUFF(1),RDBUFF(2),X2,Y2)
C           test for hit close to circumference
            OK = DIST.LE.LIM .AND.
     +           ARCTPT(RDBUFF(1),RDBUFF(2),ANG,ENANG,X,Y)
            IF (OK) THEN
               IF ( INST ) MIP=MMIP
               TMIP=MIP
            ENDIF
         ENDIF
 200     CONTINUE
c
         IF (.NOT.OK) RETURN
C     ****************************************
      ELSE IF ( ENT.EQ.ARC ) THEN
C     ****************************************
C        Searching for arcs
C        test for hit within radial limit of centre
         DIST=ABS(DISTXY(RDBUFF(1),RDBUFF(2),
     +            X,Y)-RDBUFF(4))
C        test for hit close to circumference
         OK = DIST.LT.LIM .AND.
     +       ARCTPT(RDBUFF(1),RDBUFF(2),
     1       RDBUFF(5),RDBUFF(6),X,Y)
         IF ( OK ) THEN
C           hit the arc,save the MI pointer
            IF ( INST ) MIP=MMIP
C           save the MI pointer and flag position in work file
            TMIP=MIP
C           return with data in buffer
         ELSE
            OK=.FALSE.
         END IF
C     ****************************************
      ELSE IF ( ENT.EQ.SPLINE ) THEN
C     ****************************************
C
          MODE=MOD(IMBUFF(5)+0,4)
C         next point to read
          POINT=IDBUFF(3)
          CALL DBR500(POINT,OK)
          POINT=IDBUFF(3)
          CALL DBR500(POINT,OK)
C         load the first point
          X1=RDBUFF(1)
          Y1=RDBUFF(2)
C         save the first point
 100      CONTINUE
          POINT=IDBUFF(3)
C         main search loop for all types
C         increment point count
C         get the value
          CALL DBR500(POINT,OK)
C         is it the same point
          OK=(SAME(RDBUFF(1),X1).AND.SAME(RDBUFF(2),Y1))
          IF(OK) GOTO 130
C         Searching for the spline
C         check hull limits
          IF(MODE.EQ.0) THEN
C             points only
C             does point lie in cursor hit box ?
              OK=(CHKLN(X+LIM,X-LIM,X1).AND.CHKLN(Y+LIM,Y-LIM,Y1))
              IF (OK) THEN
C                 we have one set the data buffers with info
                  FOUND=.TRUE.
                  RDBUFF(1)=X1
                  RDBUFF(2)=Y1
                  RDBUFF(4)=X1
                  RDBUFF(5)=Y1
                  GOTO 110
              ENDIF
              IF(IDBUFF(3).EQ.0) THEN
C                 last point on the list check it
                  OK=(CHKLN(X+LIM,X-LIM,RDBUFF(1))
     +             .AND.CHKLN(Y+LIM,Y-LIM,RDBUFF(2)))
                  IF (OK) THEN
C                     fill buffers
                      FOUND=.TRUE.
                      RDBUFF(4)=RDBUFF(1)
                      RDBUFF(5)=RDBUFF(2)
                      GOTO 110
                  ENDIF
              ENDIF
          ELSE
C             This is a genuine polygon curve
              IF ( ABS(RDBUFF(1)-X1).LT.
     +           ABS(RDBUFF(2)-Y1)) THEN
                 OK=CHKLN(RDBUFF(2),Y1,Y)
              ELSE
                 OK=CHKLN(RDBUFF(1),X1,X)
              END IF
C
              IF ( OK .AND.
     +            ABS(CD0D13(RDBUFF(1),RDBUFF(2),
     1                   X1,Y1,X,Y)).LE.LIM) THEN
C                 line has been hit.
C                 Kid on its a line
                  FOUND=.TRUE.
                  RDBUFF(4)=X1
                  RDBUFF(5)=Y1
                  GOTO 110
              ENDIF
          ENDIF
C         update point buffer
130       CONTINUE
          X1=RDBUFF(1)
          Y1=RDBUFF(2)
C         is this the last one ?
          IF(IDBUFF(3).NE.0) GOTO 100
110       CONTINUE
          IF(FOUND) THEN
C             kid on we have a line
              IMBUFF(2)=LINE
              ENT=LINE
          ENDIF
          OK=FOUND
      END IF
C
      IF ( OK ) THEN
C        set hardware font for entity flagging
         D1=2
         CALL HARDWF(D1)
         CALL ROPXOR()
         IF(MVPACT.AND..NOT.MAWS) THEN
C             set viewport drawing code for marking
              DDCODE = 2
         ENDIF
         CALL SPCDRW(ENT,TMIP,.FALSE.,M,.FALSE.)
C        reset to solid line
         D1=0
         CALL HARDWF(D1)
         CALL ROPREP()
C        reset drawing code
         DDCODE =0
      END IF
      VPADD = TVPA
C
      END
C
C     ---------------------------------------------------
C
      LOGICAL FUNCTION GPONLY()
C     =========================
C
C2    TThsi function returns a true value
C2    if only a grouped entity is required
C
      include 'include/masti.inc'
      include 'include/entity.inc'
C
      INTEGER*4 I
C
C     set flag
      GPONLY=.FALSE.
C
      DO 10 I=1,127
        IF(ENSRCH(I).AND.I.NE.GROUP) RETURN
10    CONTINUE
C
C     set flag if only group required
      IF(ENSRCH(GROUP))  GPONLY=.TRUE.
      END
C
C     ---------------------------------------------------
C
 
 
      SUBROUTINE INTSPL(GEOM,MATR,OX,OY,DIST,X,Y,FIRSTS,HIT)
C     ===========================================================
C1    VARTYPE            R   R    R    R  R  R  R R R    L    L
C1    IOSTAT             I   I    I    I  I  I  I I I    I    O
C
C2    This routine interpolatioons a segemnt of a spline
C2    curve and determines whether the supplied hit point
C2    is in the vicinity of the individual segments.
C2    The forst segment is ignored as the first point
C2    does not lie on the curve. The FIRSTS logical signals
C2    when the first segment is being sent and it not checked
C2    The rest is simple.
C
      include   'include/curwin.inc'
      include   'include/ndata.inc'
      REAL INC,GEOM(3,4),MATR(4,4),COEFF(3,4),
     +     TP,T,T0,T1,T2,T3,OX,OY,P(3),X,Y,LIM
      REAL LIMX,LIMY,NLIMX,NLIMY,LIMI,LIMB,CD0D13
      REAL NX,NY,DIST
      INTEGER K,J,I
      LOGICAL HIT,OK,SAME,CHKLN,FIRSTS
      EXTERNAL CD0D13,SAME
C     get the line vicinty limit
      CALL STOWVX(XWIDTH,LIM)
C
      CALL WTOSVX(DIST,INC)
      IF(.NOT.SAME(0.0,INC)) INC=15.0/INC
      INC=MAX(INC,0.01)
      INC=MIN(1.0,INC)
C
C     calculte segment point
      DO 10 K=1,3
         DO 10 J=1,4
            COEFF(K,J)=0.0
            DO 10 I=1,4
               COEFF(K,J)=COEFF(K,J)+MATR(I,J)*GEOM(K,I)
 10   CONTINUE
C
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
 
C        ignore the first segment
 
C      CALL HFONT(.TRUE.)
         IF(FIRSTS.OR.(SAME(P(1),OX).AND.SAME(P(2),OY))) THEN
             FIRSTS=.FALSE.
             GOTO 60
         ENDIF
C         CALL DRAWLT(OX,OY,P(1),P(2))
C        does the hit point lie in the segment vicinity ?
         IF ( ABS(OX-P(1)).LT.
     +       ABS(OY-P(2))     ) THEN
            OK=CHKLN(OY,P(2),Y)
         ELSE
            OK=CHKLN(OX,P(1),X)
         END IF
         IF ( OK .AND.
     +        ABS(CD0D13(OX,OY,P(1),P(2),X,Y)).LE.LIM) THEN
C           line has been hit.
C           yes it does tell em about it
            HIT=.TRUE.
C      CALL HFONT(.FALSE.)
            RETURN
         END IF
C
60       CONTINUE
C        Update the segment
         OX=P(1)
         OY=P(2)
C
 40   CONTINUE
C      CALL HFONT(.FALSE.)
C
      END
C
C     ---------------------------------------------------
C
      SUBROUTINE SRCHWN(XMIN,YMIN,XMAX,YMAX,FLAG)
C     ==========================================
C1      vartype           R  ,  R ,  R ,  R L
C1      iostatus          I  ,  I ,  I ,  I O
C1
C
C2    Subroutine SRCHWN carries out a search for entities
C2    within a rectangular window defined in world space
C2    by the points XMIN,YMIN and XMAX,YMAX. The entities
C2    found are flagged by their MI pointers being stored
C2    in the scratch file attached to unit SWINDU and their
C2    number being stored in variable NDATA,both these are
C2    available from common block SWIND.
C2    NOTE: this routine searches only the contents of the
C2    display file.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/wrkdat.inc'
      include   'include/entity.inc'
      include   'include/movdat.inc'
      include   'include/dimendat.inc'
      include   'include/curwin.inc'
      include   'include/swind.inc'
      include   'include/marker.inc'
      include  'include/viewport.inc'
C
C*************************************************************
C***  NOTE this subroutine only finds lines which are      ***
C***  totally inside the window arc is another matter !!!  ***
C*************************************************************
C
      REAL XMIN,YMIN,XMAX,YMAX,TXWIDTH,TYWIDTH,DISTXY,DIST,
     +     X1,Y1,X2,Y2,MPX,MPY,BX,BY,M(3,3),
     +     PDAT(2,4),A(3,3),B(3,3),C(3,3),
     +     BX1,BX2,BX3,BX4,BY1,BY2,BY3,BY4,
     +     CENX,CENY,RAD,INCANG,ANG,BRDR,DUMMY,ENANG,CANG,RCX,RCY,PI
      INTEGER*2 ENT,TMIP,TMIP2,DFPK,D1,J,I,K,POINT,CURGRP,MIND,CLTYPE
      INTEGER*4 MOD,DICTN,NUMENT
      LOGICAL OK,OK1,TOTAL,GROUPF,FLAG,SAME,DELETE,DTOT,DOK,
     +        FLAGIT,GPONLY,FIRST
      DOUBLE PRECISION XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2
C
      INTRINSIC ATAN,ABS,MOD
C
      EXTERNAL RDISPF,DIR500,DBR500,CURCLP,
     +         SSFLAG,GETGRP,ARCBOX,WO2SC,TEXBOX,
     +         DERDIM,SAME,GPONLY,PI,CANG
C
C
      TXWIDTH=XWIDTH
      TYWIDTH=YWIDTH
C
C     Set current group pointer
      CURGRP=-32768
C     set flag to show wholly contained
      D1=0
      NUMENT=NDATA
C
      XCURS=(XMAX+XMIN)/2.0
      YCURS=(YMAX+YMIN)/2.0
C
      XWIDTH=(XMAX-XMIN)/2.0
      YWIDTH=(YMAX-YMIN)/2.0
C
C     cycle through the display file.
      DFPK=0
C     no entities in the display file.
      IF ( LDFILE(CVPN) .EQ. 1 ) GOTO 99
C
 10   CONTINUE
      DFP=MOD(DFP+0,LDFILE(CVPN)-1)+DFPINC
C     set flag logical to true read later
      FLAGIT=.FALSE.
C
      DFPK=DFPK+1
C
C     read next display file entry.
      CALL RDISPF(DFP,ENT,TMIP,OK)
C
      IF (.NOT.OK) GOTO 99
C
C      WRITE(10,*) '[SRCHWN]DFP,ENT,TMIP,OK',DFP,ENT,TMIP,OK
C     ++++++++++++++++++++++++++++++++++++++++++++++
      IF ((ENSRCH(ENT).OR.ENSRCH(GROUP)).AND.TMIP.GT.0) THEN
C     ++++++++++++++++++++++++++++++++++++++++++++++
C        Search is valid.
C        test for delete status.
         CALL ALLRD(TMIP,ENT,M,DELETE)
         IF (.NOT.OK) GOTO 99
         IF(ENSRCH(GROUP).AND.IMBUFF(1).NE.GROUP.AND.
     +      GPONLY()) GOTO 20
C        if deleted or found already.
C        component master then ignore.
C        symbol master then ignore.
C        test for group status condition.
C        ignore grouped entities entirely if required.
C         WRITE(10,*) '[SRCHWN] GROUP:',IMBUFF(1),GROUP,GSSTAT,IMBUFF(7)
         IF( IMBUFF(1).GT.99     .OR.
     +       IMBUFF(1).EQ.COMPM  .OR.
     1       IMBUFF(1).EQ.SYMBM  .OR.
     2    (IMBUFF(1).EQ.GROUP.AND.GSSTAT.EQ.0)     ) GOTO 20
C        test for group status condition
         GROUPF=(IMBUFF(1).EQ.GROUP)
C        get the parent MI of group
         TMIP2=IMBUFF(8)
C        have we already encountered this group
         IF( GROUPF.AND.IMBUFF(8).EQ.CURGRP ) GOTO 20
C
C        ensure correct entity type being used in search
         ENT=IMBUFF(2)
C        ************************************************
         IF ( ENT .EQ. LINE ) THEN
C        ************************************************
C           Searching for lines
C           Copy data since clipping alters end point values
            X1=RDBUFF(1)
            Y1=RDBUFF(2)
            X2=RDBUFF(4)
            Y2=RDBUFF(5)
            CALL CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
            IF ( OK ) THEN
C              part of the line exists within the window
               IF ( TOTAL ) THEN
C                 all of the line exists inside the window
C                  !!  STORE IT !!
C                 set end code to indicate line wholly contained
                  D1=0
               ELSE IF ( OPFLAG(11) ) THEN
C                 STRETCH mode enabled, need to know which
C                 end of the line to stretch
                  IF ( SAME(RDBUFF(1),X1).AND.SAME(RDBUFF(2),Y1) ) D1=1
                  IF ( SAME(RDBUFF(4),X2).AND.SAME(RDBUFF(5),Y2) ) D1=2
               ELSE
                  GOTO 141
               END IF
C              flag entity and enter to scratch file
               FLAGIT=.TRUE.
            END IF
 141        CONTINUE
C        ************************************************
         ELSE IF ( ENT .EQ. MARKER ) THEN
C        ************************************************
C           Searching for lines
C           Copy data since clipping alters end point values
            X1=RDBUFF(1)
            Y1=RDBUFF(2)
            MIND=MRKIND(IMBUFF(5))
            IF (MIND.EQ.0) THEN
                IF (X1.GT.XMIN.AND.X1.LT.XMAX.AND.
     +              Y1.GT.YMIN.AND.Y1.LT.YMAX) THEN
                   FLAGIT= .TRUE.
                   D1 = 0
                END IF
            ELSE
C
C              scale the point
               CALL SCAL2D(RDBUFF(5),RDBUFF(6),A)
C              rotate the point
               CALL ROT2D(RDBUFF(4),B)
C              Concatenate result
               CALL MULT3M(A,B,C)
c              translate the point
               CALL TRAN2D(X1,Y1,B)
C              Concatenate result
               CALL MULT3M(C,B,A)
               CALL NEWXY(MRKR(MIND,1),MRKR(MIND,2),BX1,BY1,A)
               CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND+1,2),BX2,BY2,A)
               CALL NEWXY(MRKR(MIND,1),MRKR(MIND+1,2),BX3,BY3,A)
               CALL NEWXY(MRKR(MIND+1,1),MRKR(MIND,2),BX4,BY4,A)
C      WRITE(10,*) '[SRCHWN] BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4 ',
C     +             BX1,BY1,BX2,BY2,BX3,BY3,BX4,BY4
C      write(10,*) '[SRCHWN] XMIN,YMIN,XMAX,YMAX',XMIN,YMIN,XMAX,YMAX
C     This is a check to see if it is worth drawing the
C     marker or not by checking its surounding box
C      CALL CHKBOX(MIN(BX1,BX2,BX3,BX4),MIN(BY1,BY2,BY3,BY4),
C     +           MAX(BX1,BX2,BX3,BX4),MAX(BY1,BY2,BY3,BY4),
C     +           TOTAL,DISPV)
               IF(MIN(BX1,BX2,BX3,BX4).GT.XMIN.AND.
     +            MIN(BY1,BY2,BY3,BY4).GT.YMIN.AND.
     +            MAX(BX1,BX2,BX3,BX4).LT.XMAX.AND.
     +            MAX(BY1,BY2,BY3,BY4).LT.YMAX) THEN
C                 part of the line exists within the window
                  FLAGIT=.TRUE.
                  D1=0
               END IF
            END IF
C        ************************************************
         ELSE IF ( ENT.EQ.ARC ) THEN
C        ************************************************
C           Searching for arcs
            CALL ARCBOX(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),RDBUFF(6),X1,Y1,X2,Y2)
C
            IF( X1.GT.XMIN.AND.X2.LT.XMAX.AND.
     1          Y1.GT.YMIN.AND.Y2.LT.YMAX  ) THEN
C              !!  STORE IT !!
               FLAGIT=.TRUE.
C              set end code to indicate arc wholly contained
               D1=0
            END IF
C
C        ************************************************
         ELSE IF ( ENT .EQ. CENLIN ) THEN
C        ************************************************
C           Searching for a Center Line.
            CENX = RDBUFF(1)
            CENY = RDBUFF(2)
            RAD = RDBUFF(3)
            INCANG = 0
            ANG = RDBUFF(5)
C           Get the extension and convert it from paper to world.
            CALL PAP2SC(RDBUFF(6),DIST)
            CALL SC2WO(DIST,0.0,BRDR,DUMMY)
            CALL SC2WO(0.0,0.0,DUMMY,DIST)
            BRDR = BRDR - DUMMY
            CLTYPE = IDBUFF(4)
C
            IF (CLTYPE.EQ.3) THEN
C              PCD straight line section.
               DUMMY = RAD
C              Find the referance arc center point.
               CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,RCX,RCY)
               CENX = RCX
               CENY = RCY
               RAD = RDBUFF(4)
            ENDIF
C
            FIRST = .TRUE.
C           If d1=0 ... we assume cross is fully contained until proved
C           otherwise.
            D1 = 0
 50         CONTINUE
C              Firstly ... Find The Line.
C              Find finish point.
               RAD = RAD + BRDR
               DUMMY = RAD
               CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,ANG,X2,Y2)
C              and start point is the same dist again,  other direction.
               X1 = CENX - (X2 - CENX)
               Y1 = CENY - (Y2 - CENY)
C
               CALL CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
               IF ( OK ) THEN
C                 part of the line exists within the window
                  IF ( (.NOT.TOTAL) .AND. OPFLAG(11) ) THEN
C                    STRETCH mode enabled, need to know which
C                    end of the line to stretch
                     IF (SAME(RDBUFF(1),X1).AND.SAME(RDBUFF(2),Y1)) D1=1
                     IF (SAME(RDBUFF(4),X2).AND.SAME(RDBUFF(5),Y2)) D1=2
                  ELSE IF (.NOT.TOTAL) THEN
C                    Nope. It aint there.
                     GOTO 241
                  END IF
C                 flag entity and enter to scratch file
                  FLAGIT=.TRUE.
               END IF
C                
 241        CONTINUE
C           First line dealt with, how about a second?
            IF (FIRST .AND. (CLTYPE.EQ.1)) THEN
C              Second line of a cross center line.
               RAD = RDBUFF(4)
               ANG = ANG + PI(0.5)
               FIRST = .FALSE.
               GOTO 50              
            ELSE IF (CLTYPE.EQ.3) THEN
C              PCD ... the curved bit.
C              test for hit within radial limit of centre
C              Find start and finish point.
               XC1 = DBLE(RDBUFF(1))
               YC1 = DBLE(RDBUFF(2))
               RAD1 = DBLE(RDBUFF(3))
               XC2 = DBLE(CENX)
               YC2 = DBLE(CENY)
               RAD2 = DBLE(RAD)
               CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,DX2,DY2,OK)
               X1 = REAL(DX1)
               Y1 = REAL(DY1)
               X2 = REAL(DX2)
               Y2 = REAL(DY2)
C              Convert the points to angles,
               ANG = CANG(RDBUFF(1),RDBUFF(2),X1,Y1)
               ENANG = CANG(RDBUFF(1),RDBUFF(2),X2,Y2)
C              test for arc being inside window.
               CALL ARCBOX(RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +               ANG,ENANG,X1,Y1,X2,Y2)
C
               IF( X1.GT.XMIN.AND.X2.LT.XMAX.AND.
     1          Y1.GT.YMIN.AND.Y2.LT.YMAX  ) THEN
C                 !!  STORE IT !!
                  FLAGIT=.TRUE.
               END IF
            ENDIF
C
C        ************************************************
         ELSE IF ( ENT .EQ. HATCH ) THEN
C        ************************************************
C           Searching for hatch
            IF( RDBUFF(1).GT.XMIN.AND.RDBUFF(4).LT.XMAX.AND.
     1          RDBUFF(2).GT.YMIN.AND.RDBUFF(5).LT.YMAX  ) THEN
C              !!  STORE IT !!
               FLAGIT=.TRUE.
C              set end code to indicate hatch wholly contained
               D1=0
            END IF
C        ****************************************
         ELSE IF ( ENT .EQ. SPLINE) THEN
C        ****************************************
C           Searching for B_Spline
C           read past the two control blocks
            POINT=IDBUFF(3)
            CALL DBR500(POINT,OK)
            POINT=IDBUFF(3)
 111        CONTINUE
C
            CALL DBR500(POINT,OK)
            POINT=IDBUFF(3)
            OK= RDBUFF(1).GT.XMIN.AND.RDBUFF(1).LT.XMAX.AND.
     1          RDBUFF(2).GT.YMIN.AND.RDBUFF(2).LT.YMAX
            IF ( .NOT.OK ) GOTO 112
            IF(POINT.NE.0) GOTO 111
C
            IF (OK) THEN
                FLAGIT=.TRUE.
C              set end code to indicate  wholly contained
               D1=0
            END IF
112         CONTINUE
C
C           ************************************************
            ELSE IF ( ENT.EQ.LDIMN ) THEN
C           ************************************************
C           Searching for LINEAR DIM
C           read the data for the dimension entity
C           now test for hit wthin text area
C
C           switch pen to erase colour
            OK=.FALSE.
            DOK=.FALSE.
            DTOT=.TRUE.
            DOK=(RWORK(1,1).GT.XMIN.AND.RWORK(1,1).LT.XMAX
     +     .AND.RWORK(2,1).GT.YMIN.AND.RWORK(2,1).LT.YMAX ).OR.
     1         (RWORK(4,1).GT.XMIN.AND.RWORK(4,1).LT.XMAX
     +        .AND. RWORK(5,1).GT.YMIN.AND.RWORK(5,1).LT.YMAX)
C           set local counter for text
            K=0
C           Loop through the stored dimension records.
            DO 101 I=1,RECCNT(1)
C              Check supression state of sub-record entity
               IF ( IWORK(4,I) .GE. 0 ) THEN
C                 was not surpressed so continue
               IF ( IWORK(1,I) .EQ. LDIMN ) THEN
                  X1=RWORK(1,I)
                  Y1=RWORK(2,I)
                  X2=RWORK(4,I)
                  Y2=RWORK(5,I)
                  CALL CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
                  DOK=OK.OR.DOK
               ELSE IF (IWORK(1,I) .EQ. TEXSEG ) THEN
C                 was a text record so use text buffer as well
                  K=K+1
                 CALL TEXBOX(RWORK(1,I),RWORK(2,I),PAPTOW*RWORK(3,I),
     +               PAPTOW*RWORK(4,I),RWORK(5,I),RWORK(6,I),
     1               X1,Y1,X2,Y2)
                  OK=X1.GT.XMIN.AND.X2.LT.XMAX.AND.
     1               Y1.GT.YMIN.AND.Y2.LT.YMAX
                  DOK =DOK.OR.OK
                  DTOT=OK.AND.DTOT
               ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
C                 was arrowhead so must decode the arrowhead parameters.
 
C
               ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
C                 Was a line so use data direct
                  X1=RWORK(1,I)
                  Y1=RWORK(2,I)
                  X2=RWORK(4,I)
                  Y2=RWORK(5,I)
                  CALL CURCLP(X1,Y1,X2,Y2,OK,TOTAL)
                  DOK=OK.OR.DOK
                  DTOT=DTOT.AND.TOTAL
C               ELSE
C                 Unrecognised subrecord type.
C                 WRITE(10,*)
C    +            '[DRWDIM] Non Geometric Sub-Record ',IWORK(1,I)
               END IF
            END IF
 101     CONTINUE
C        switch pen back to draw colour
         IF ( DOK ) THEN
           IF ( OPFLAG(11).AND..NOT.DTOT ) THEN
               IF ( RWORK(1,1).GT.XMIN.AND.RWORK(1,1).LT.XMAX
     +     .AND.RWORK(2,1).GT.YMIN.AND.RWORK(2,1).LT.YMAX ) THEN
C                 set flag to show first end contained
                  D1=1
                  CALL WO2SC(RWORK(1,1),RWORK(2,1),X1,Y1)
               ELSE IF ( RWORK(4,1).GT.XMIN.AND.RWORK(4,1).LT.XMAX
     +            .AND.RWORK(5,1).GT.YMIN.AND.RWORK(5,1).LT.YMAX ) THEN
C                 set flag to show second end contained
                  D1=2
                  CALL WO2SC(RWORK(4,1),RWORK(5,1),X1,Y1)
               ELSE
C                 set flag to show wholly contained
                  D1=0
               END IF
            ELSE
               IF ( .NOT.DTOT) GOTO 440
C              set flag to show wholly contained
               D1=0
            END IF
         ELSE
            GOTO 440
         END IF
 
         IF ( DOK )  THEN
C           !!  STORE IT !!
            FLAGIT=.TRUE.
         END IF
 440     CONTINUE
C     ************************************************
         ELSE IF (ENT.EQ.ADIMN.OR.ENT.EQ.RDIMN.OR.ENT.EQ.LDIMN
     +       .OR. ENT.EQ.DDIMN.OR.ENT.EQ.GLABEL) THEN
C     ************************************************
C        Searching for LINEAR DIM
C        read the data for the dimension entity
C        now test for hit wthin text area
         IF ( ENT.EQ.ADIMN) THEN
C           offset header records
            J=3
         ELSE
            J=2
         END IF
C        find enclosing box using first dimension record
         CALL TEXBOX(RWORK(1,J),RWORK(2,J),PAPTOW*RWORK(3,J),
     +        PAPTOW*RWORK(4,J),RWORK(5,J),
     +        RWORK(6,J),X1,Y1,X2,Y2)
C
         IF( X1 .GT. XMIN .AND.
     +       X2 .LT. XMAX .AND.
     1       Y1 .GT. YMIN .AND.
     2       Y2 .LT. YMAX  ) THEN
C           !!  STORE IT !!
            FLAGIT=.TRUE.
         END IF
C     ************************************************
      ELSE IF ( ENT .EQ. TEXT ) THEN
C     ************************************************
C        Searching for text
C        test for text box contained in window
         CALL TEXBOX(RDBUFF(1),RDBUFF(2),PAPTOW*RDBUFF(3),
     +               PAPTOW*RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1               X1,Y1,X2,Y2)
C
         IF( X1 .GT. XMIN .AND.
     +       X2 .LT. XMAX .AND.
     1       Y1 .GT. YMIN .AND.
     2       Y2 .LT. YMAX  ) THEN
C           !!  STORE IT !!
            FLAGIT=.TRUE.
         END IF
C     ************************************************
      ELSE IF ( ENT.EQ.COMPI.OR.ENT.EQ.SYMBI ) THEN
C     ************************************************
C        Searching for component instance
C
C     CALL DRAWLW(XMIN,YMIN,XMAX,YMAX)
C     CALL WAIT(2.0)
C     CALL DRAWLW(RWORK(1,1),RWORK(2,1),RWORK(4,1),RWORK(5,1))
C     CALL WAIT(2.0)
C     CALL DRAWLW(RWORK(1,2),RWORK(2,2),RWORK(4,2),RWORK(5,2))
C     CALL WAIT(2.0)
 
 
C     has this component visibility
      CALL CMPVIS(TMIP,OK)
      IF(OK) THEN
             CALL ALLRD(TMIP,ENT,M,DELETE)
             IF( RWORK(1,1).GT.XMIN.AND.RWORK(4,1).LT.XMAX.AND.
     1           RWORK(2,1).GT.YMIN.AND.RWORK(5,1).LT.YMAX  ) THEN
C               !!  STORE IT !!
                FLAGIT=.TRUE.
             END IF
      ENDIF
C     ************************************************
      ELSE IF ( ENT.EQ.SYMBI ) THEN
C     ************************************************
C        Searching for component instance
C
C      WRITE(10,*) 'RDBUFF',RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5)
C      WRITE(10,*) 'WINDOW',XMIN,YMIN,XMAX,YMAX
         IF( RDBUFF(1) .GT. XMIN .AND.
     +       RDBUFF(4) .LT. XMAX .AND.
     1       RDBUFF(2) .GT. YMIN .AND.
     2       RDBUFF(5) .LT. YMAX  ) THEN
C           !!  STORE IT !!
            FLAGIT=.TRUE.
         END IF
C     ************************************************
      END IF
C     ************************************************
C
C     ++++++++++++++++++++++++++++++++++++++++++++++
      END IF
C     flag entity if within window
      IF (FLAGIT) THEN
         IF (GROUPF) GOTO (110,120,130,140) GSSTAT
 110     CONTINUE
C        flag the entity found
         BX=0.0
         BY=0.0
C        save in work file
         CALL SSFLAG(TMIP,BX,BY,DFP,D1,FLAG)
         GOTO 150
 120     CONTINUE
C        the complete group should be returned
         CALL GETGRP(TMIP2,OK)
         GOTO 150
 130     CONTINUE
C        grouped entity not allowed
         DICTN=60
         CALL DEPRNT(DICTN)
         NUMENT=NUMENT+1
         CURGRP = IMBUFF(8)
         GOTO 150
 140     CONTINUE
C        the complete group should be flagged,
C        and the header returned
         DICTN=76
         CALL DCPRNT(DICTN)
         CALL GETGRP(TMIP2,OK)
C        read the MI data before return
         CALL DIR500(TMIP2,OK)
 150     CONTINUE
      END IF
C     ++++++++++++++++++++++++++++++++++++++++++++++
 20   CONTINUE
      IF (DFPK.LT.(LDFILE(CVPN)-1)) GOTO 10
C     recover the stored window widths
 99   CONTINUE
 
      IF(NDATA.EQ.NUMENT) THEN
          DICTN=34
          CALL DEPRNT(DICTN)
      ENDIF
      XWIDTH=TXWIDTH
      YWIDTH=TYWIDTH
C
      END
C
C     ---------------------------------------------------
C
