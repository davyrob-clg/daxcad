C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writegt.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE WRTGT1(FN)
C     SUBROUTINE WRTGT2(FUNIT,OUNIT,TYPE,FN,N,PFNUM,SNUM,LNUM,CNUM,
C     SUBROUTINE WRTGT3(FUNIT,NP)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE WRTGT1(FN)
C     ====================
C
C1    vartype           I4
C1    iostatus          I
c
C2    This routine is designed to create an ascii output file
C2    containing GTL geometry definitions suitable for use
C2    within the OLIVETTI GTL : NC programming system.
C2    The function number FN=1 indicates GTL3 format
C2    and FN=2 indicates GTLT format for output data.
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/hdata.inc'
      include 'include/lfu.inc'
      include 'include/params.inc'
      include 'include/ftypes.inc'
C
      DOUBLE PRECISION DN
      CHARACTER*80 OLIN,FNAME,SEQ*20,UNF*20,DBU*2
      INTEGER*4 I,REC,TYPE,SKNO,INCK,NLEN,OUNIT,TMEN,TCELL,FCODE
      INTEGER*4 PFNUM,SNUM,CNUM,LNUM,PNUM,NLEN1,L,STARTP,ENDP,FN
      INTEGER*4 DAXNAM,DAXST
      INTEGER*2 FFN
      REAL ORGX,ORGY,VXS,VXL,VYS,VYL,GTLOX,GTLOY
      LOGICAL OK,YES,METRIC,OPTION,QUIT,MAKEOK
      CHARACTER*13 FORM1(2)
 
      CHARACTER*8 CCXS(2)
      CHARACTER*4 CCYS(2)
      CHARACTER*4 CCXL(2),CCYL(2)
C
      EXTERNAL OURSCR,DCPRNT,DEPRNT,PULENT,MEASA3,NLEN,NLEN1
      EXTERNAL WWINDO,DAXNAM,MAKEOK
C
      DATA FORM1/'(4(A,F10.4))','(4(A,F11.5))'/
      DATA CCXS,CCYS/'SCA,XS ','SCA,ZS ',',YS ',',XS '/
      DATA CCXL,CCYL/',XL ',',ZL ',',YL ',',XL '/
C
C     initialize geometry numbering controls
C     Profile Number
      PFNUM=1
C     Line Number
      LNUM=1
C     Circle Number
      CNUM=1
C     Point Number
      PNUM=1
C     Set Number
      SNUM=1
C
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU.NE.'MM' .AND. DBU.NE.'IN' ) THEN
C       wrong units for GTL input
         I=448
         CALL DEPRNT(I)
C        abort process
         MEN=0
         GOTO 999
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
      DAXTYP = 7
 100  CONTINUE
C     ask for filename for data
      CALL DPRMXP(81,FNAME)
C     if null file name assume abort required
      IF ( NLEN(FNAME).EQ.0 ) THEN
         GOTO 997
      END IF
C
      DAXST = DAXNAM(FNAME)
 
      IF (DAXST .NE. 3 ) THEN
 
         IF (DAXST .NE. 2 ) THEN
 
             CALL DEPRNT(341)
             GOTO 100
 
         ELSE
 
             IF ( MAKEOK() ) THEN
                 CALL DELETE(FNAME,YES)
                 IF ( .NOT. YES  ) THEN
                    CALL DEPRNT(719)
                    GOTO 997
                 END IF
             ELSE
 
               GOTO 100
 
            END IF
 
         ENDIF
 
      ENDIF
      CALL FINDU2(OUNIT,FNAME,YES)
      IF ( .NOT. YES ) GOTO 997
C
      CALL OPNFFF(FNAME,OUNIT,OK)
      IF(.NOT.OK) GOTO 997
CIBM
C      LFU(OUNIT)=.TRUE.
CIBM
C     ******************************************
C     **   setup GTL origin and limits        **
C     ******************************************
 699  CONTINUE
C     find origin for GTL data
      CALL MNIGNC()
      CALL MNLPTS()
C     prompt for origin point
      I=44
      CALL FINDP0(I,GTLOX,GTLOY,OPTION,QUIT)
      CALL MNUPTS()
      IF (QUIT) GOTO 998
      IF (OPTION) GOTO 699
C     get rectangular window for GTL view
 700  CONTINUE
C     go get rectangle for GTL screen mapping
      CALL WWINDO(VXS,VYS,VXL,VYL,OK)
      IF (.NOT.OK) GOTO 700
C
C     ******************************************
C     **   write file header                  **
C     ******************************************
      WRITE(UNIT=OUNIT,FMT='(A)')
     +    ''' *** DAXCAD-GTL Geometry Interface ***'
C     tell the NC programmer where the data came form
      WRITE(UNIT=OUNIT,FMT='(3A)') 'ID="',DRGNAM(1:NLEN1(DRGNAM)),'"'
C     start file with the units in use
      IF (DBU.EQ.'MM') THEN
C        UNIT1 is MM for GTL
         WRITE(UNIT=OUNIT,FMT='(A)') 'UNIT1'
C        set run-time format code
         FCODE=1
      ELSE
C        UNIT2 is IN for GTL
         WRITE(UNIT=OUNIT,FMT='(A)') 'UNIT2'
C        set run-time format code
         FCODE=2
      END IF
C     write TURNING switch to file if necessary
      IF (FN.EQ.2) WRITE(UNIT=OUNIT,FMT='(A)') 'XR'
C
      OLIN=' '
C     offset view limits to GTL origin
      VXS=VXS-GTLOX
      VXL=VXL-GTLOX
      VYS=VYS-GTLOY
      VYL=VYL-GTLOY
C     write view scale data for GTL
      WRITE(UNIT=OLIN,FMT=FORM1(FCODE))
     +      CCXS(FN),VXS,CCYS(FN),VYS,
     +      CCXL(FN),VXL,CCYL(FN),VYL
      CALL CRUNCH(OLIN)
      L=NLEN1(OLIN)
      WRITE(UNIT=OUNIT,FMT='(A)') OLIN(1:L)
C     draw the axes
      WRITE(UNIT=OUNIT,FMT='(A)') 'AXES'
C
c     write call to standard GTL command file
      WRITE(UNIT=OUNIT,FMT='(A)') 'CALL DAXGTL'
      WRITE(UNIT=OUNIT,FMT='(A)') ''' *** Geometry Definition ***'
C
C
C     ******************************************
C     **   start geometry definition          **
C     ******************************************
C     default the curve type to PROFILE
      TYPE=1
C     Find menu cell which has been set for Profile
      CALL FNDPOS(301,I)
      CALL GTMCHI(3,I)
      TMEN=3
      TCELL=I
 10   CONTINUE
      IF ( TYPE.EQ.1 )  THEN
C        profile curve.
         FFN=2
         CALL PULENT(FFN)
         I=NO
      ELSE IF ( TYPE.EQ.2 ) THEN
C        points kcurve.
         I=0
         CALL MNLPTS()
         CALL WRTGT3(HUNIT,I)
         CALL MNUPTS()
      END IF
C     switch off profile/points cell
C      CALL GTMCLO(TMEN,TCELL)
      IF ( MEN.EQ.2.OR.CCMD.EQ.'Q'.OR.CCMD.EQ.'q' ) THEN
C        Get rid of temporary scratch files
C        close work file also
         GOTO 998
      END IF
C
      IF ( CCMD.EQ.'P'.OR.CCMD.EQ.'p' ) THEN
C        switch off profile/points cell
         CALL GTMCLO(TMEN,TCELL)
         TMEN=MEN
         TCELL=CELLN
         CALL GTMCHI(TMEN,TCELL)
      ELSE
         CALL GTMCLO(MEN,CELLN)
      END IF
C
C     **********************************************
C     **   write geometry definitions to output   **
C     **********************************************
      IF ( I.GT.0 ) THEN
         CALL WRTGT2(HUNIT,OUNIT,TYPE,FN,I,PFNUM,SNUM,LNUM,CNUM,PNUM,
     +               GTLOX,GTLOY)
C        **********************************************
C        **      write post-script to geometry       **
C        **********************************************
         WRITE(UNIT=OUNIT,FMT='(A,/,A,/,A)') 'CLS','PLOT,PF','AXES'
      END IF
C
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
      END IF
 
      IF ( CCMD.NE.CHAR(150) ) GOTO 10
C
C     **********************************************
C     **           terminate the data file        **
C     **********************************************
C     write EOF marker to file
      WRITE(UNIT=OUNIT,FMT='(A)') ''' *** End of Geometry Data ***'
C
C     ********************************************
C     close both scratch and work files
C     ********************************************
      CLOSE(UNIT=HUNIT)
CIBM
C      LFU(HUNIT)=.FALSE.
CIBM
      CLOSE(UNIT=OUNIT)
CIBM
C      LFU(OUNIT)=.FALSE.
CIBM
C
C     show the user what he has created
      CALL POPPD1(FNAME,673,120,350,667)
c
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
C
      END
C
C     ----------------------------------------------
C
      SUBROUTINE WRTGT2(FUNIT,OUNIT,TYPE,FN,N,PFNUM,SNUM,LNUM,CNUM,
     +                  PNUM,GTLOX,GTLOY)
C     ===============================================================
C        vartyp           I4   I4    I4  I4  I4   I4   I4    I4  I4
C        iostat            I    I    I   I   IO   IO   IO    IO  IO
C
C2    The function number FN=1 indicates GTL3 format
C2    and FN=2 indicates GTLT format for output data.
C
      include 'include/menun.inc'
      include 'include/wtov.inc'
      include 'include/entity.inc'
      include 'include/params.inc'
C
      CHARACTER*20 OLIN*80,PBUF*90
      CHARACTER*80 KCURVE(10)
      LOGICAL SAME,YES,OP,HOLD,QUIT,OPTION,FULCIR,TNGENT
      LOGICAL TAN1,TAN2,FFARC,FFILET
      INTEGER*2 MIP1,ENT,ENTH,MIP0,ENT0
      INTEGER*4 N,I,NP,FUNIT,OUNIT,ON,NLEN,TYPE,SKNO,INCK,FCODE
      INTEGER*4 PFNUM,SNUM,LNUM,CNUM,PNUM,INUM,L,NLEN1,PBUFP,IP
      INTEGER*4 KCURVL,J,INUM2,IN00,IN01,FIRSTP,LASTP,STARTP,ENDP,FN
      INTEGER*4 ENTS,ENTF
      DOUBLE PRECISION DX1,DY1,DX2,DY2
      REAL X1,X2,Y1,Y2,Z1,BX,BY,OX,OY,XH1,YH1,XH2,YH2,ZH1,XP1,YP1,
     +     T1,T2,T3,CD0D13,DISTXY,PI,CANG,DEG,ANG,GTLOX,GTLOY
      REAL X01,X02,Y01,Y02,OXE,OYE,STARTX,STARTY,ENDX,ENDY
      REAL XVAL(2),YVAL(2),XS1,YS1,XS2,YS2,XF1,YF1,XF2,YF2
      REAL ENDX1,ENDY1,ENDX2,ENDY2
      CHARACTER*3 CIRCC,FILLC,DBU*2
      CHARACTER*17 FORM1(2),FORM2(2)
      CHARACTER*9 FORM3(2)
      CHARACTER*3 CCX(2),CCY(2)
      CHARACTER*18 FORM4(2)
C
      EXTERNAL SAME,DPI,NLEN,CD0D13,DISTXY,PI,NLEN1,CANG,DEG,TNGENT
C
      DATA FORM1,FORM2/'(A,I6,3(A,F10.4))','(A,I6,3(A,F11.5))',
     +                  '(A,I6,2(A,F10.4))','(A,I6,2(A,F11.5))'/
C
      DATA FORM3/'(A,F10.4)','(A,F11.5)'/
C
      DATA FORM4/'(A,I6,4(A,F10.4))','(A,I6,4(A,F11.5))'/
C
      DATA CCX,CCY/',X ',',Z ',',Y ',',X '/
C
C     initialize profile buffer
      DO 3 J=1,10
         KCURVE(J)=' '
 3    CONTINUE
      KCURVL=1
      PBUF=' '
      PBUFP=1
      IF ( CCMD.NE.'P'.OR.CCMD.NE.'p' ) THEN
         CALL GTMCLO(MEN,CELLN)
      END IF
C     use different format for database units.
      DBU=DBUNIT
      CALL FOLDUP(DBU)
      IF ( DBU .EQ. 'MM' ) THEN
C        set format code
         FCODE=1
      ELSE  IF ( DBU .EQ. 'IN' ) THEN
C        set format code
         FCODE=2
      END IF
 100  CONTINUE
C     close profiled or open profile. If a close profile take
C     the end point as the nearest to the start and end entity
C     also do a wee check to see if the start point is on the
C     second entity if it is then start on the second entity
C     cos otherwise by our present logic we will end up defining
C     a xero length entity and that not a good thing
 
C
C     **************************************************
C     ****        Find start and end points         ****
C     **************************************************
C
      IF ( TYPE.EQ.1 ) THEN
C        we want end points for all cases except an arc
C        on its own then itis just defined positive
C        go find start point for profile
         CALL MNLPTS()
         CALL  FINDP0(381,XP1,YP1,OPTION,QUIT)
         CALL MNUPTS()
         IF ( QUIT.OR.OPTION ) RETURN
C        save start point
         STARTX=XP1
         STARTY=YP1
C        get me an end point
         CALL MNLPTS()
         CALL  FINDP0(382,XP1,YP1,OPTION,QUIT)
         CALL MNUPTS()
         IF ( QUIT.OR.OPTION ) RETURN
C        save it for later use
         ENDX=XP1
         ENDY=YP1
      ENDIF
C     alter the start point of the start entity to suit direction
C     coincident point as the start
      READ(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                       XH1,YH1,XH2,YH2,ZH1
      ENTS=ENT
      IF(SAME(X1,X2).AND.SAME(Y1,Y2)) THEN
          IF(SAME(X1,XH1).AND.(.NOT.SAME(Y1,YH1))) THEN
              X1=XH1
              Y1=YH1
          ELSE
              X1=XH2
              Y1=YH2
          ENDIF
          WRITE(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                            XH1,YH1,XH2,YH2,ZH1
C         the ox and oy are the old points when it comes to
C         the next vector
          OX=X1
          OY=Y1
C         Get the 'OLD' point for a first ARC
      ELSEIF (ENT.EQ.ARC.AND.N.GT.1) THEN
C         start and end points on the starting arc
C         start
          XS1=X1+X2*COS(Y2)
          YS1=Y1+X2*SIN(Y2)
C         end
          XS2=X1+X2*COS(Z1)
          YS2=Y1+X2*SIN(Z1)
C         get info on the next entity
          READ(UNIT=FUNIT,REC=2) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                           XH1,YH1,XH2,YH2,ZH1
          IF(ENT.EQ.LINE) THEN
              XF1=X1
              YF1=Y1
              XF2=X2
              YF2=Y2
          ELSE
              XF1=X1+X2*COS(Y2)
              YF1=Y1+X2*SIN(Y2)
              XF2=X1+X2*COS(Z1)
              YF2=Y1+X2*SIN(Z1)
          ENDIF
C         find the starting point of the first arc by comparing
C         the end angle with the other entity points.
          IF((SAME(XS2,XF1).AND.SAME(YS2,YF1)).OR.(SAME(XS2,XF2).
     +       AND.SAME(YS2,YF2))) THEN
              OX=XS1
              OY=YS1
          ELSE
              OX=XS2
              OY=YS2
          ENDIF
      ENDIF
C     alter the end information
      IF(ENT.EQ.LINE) THEN
          READ(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                           XH1,YH1,XH2,YH2,ZH1
          ENTF=ENT
          IF(SAME(X1,X2).AND.SAME(Y1,Y2)) THEN
              IF(SAME(X2,XH2).AND.(.NOT.SAME(Y2,YH2))) THEN
                  X2=XH2
                  Y2=YH2
              ELSE
                  X2=XH1
                  Y2=YH1
              ENDIF
              WRITE(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                                XH1,YH1,XH2,YH2,ZH1
          ENDIF
      ENDIF
C
C     **************************************************
C     ****        Pass Number 1 Creating Geometry   ****
C     **************************************************
C
C     save first point number
      FIRSTP=PNUM
      DO 10 I=1,N
         OLIN=' '
         READ(UNIT=FUNIT,REC=I) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                                     XH1,YH1,XH2,YH2,ZH1
D         WRITE(UNIT=10,FMT=*) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
D     +                                     XH1,YH1,XH2,YH2,ZH1
C
         IF ( ENT.EQ. ARC ) THEN
C           *****************************************
C           **         Define Circular ARC         **
C           *****************************************
            WRITE(UNIT=FUNIT,REC=I)
     +           MIP1,ENT,CNUM,BY,X1,Y1,X2,Y2,Z1,
     +                        XH1,YH1,XH2,YH2,ZH1
            WRITE(OLIN,FMT=FORM1(FCODE))
     +      'C',CNUM,CCX(FN),X1-GTLOX,CCY(FN),Y1-GTLOY,',R',X2
C           update current CIRCLE number
            CNUM=CNUM+1
         ELSE IF ( ENT .EQ. LINE ) THEN
C           *****************************************
C           **         Define LINE                 **
C           *****************************************
            WRITE(UNIT=FUNIT,REC=I)
     +           MIP1,ENT,LNUM,PNUM,X1,Y1,X2,Y2,Z1,
     +                        XH1,YH1,XH2,YH2,ZH1
C           If this is the first entity then modify the start
C           points of the line.
C           define line by start point and angle
            WRITE(OLIN,FMT=FORM4(FCODE))
     +      'L',LNUM,CCX(FN),X1-GTLOX,CCY(FN),Y1-GTLOY,
     1      CCX(FN),X2-GTLOX,CCY(FN),Y2-GTLOY
C           update current line number
            LNUM=LNUM+1
         ELSE IF ( ENT .EQ. 2 ) THEN
C           *****************************************
C           **         Define POINT                **
C           *****************************************
C           add geometry number to scratch file
            WRITE(UNIT=FUNIT,REC=I)
     +           MIP1,ENT,LNUM,BY,X1,Y1,X2,Y2,Z1,
     +                        XH1,YH1,XH2,YH2,ZH1
            WRITE(OLIN,FMT=FORM2(FCODE))
     +      'P',PNUM,CCX(FN),X1-GTLOX,CCY(FN),Y1-GTLOY
C           update current point number
            PNUM=PNUM+1
         END IF
C        eliminate spaces
         CALL CRUNCH(OLIN)
         L=NLEN1(OLIN)
C        write to output file
         WRITE(OUNIT,'(A)') OLIN(1:L)
 10   CONTINUE
C     save last point number for a points set only
      LASTP=PNUM-1
      STARTP=PNUM
      ENDP=PNUM+1
      PNUM=PNUM+2
C
C     clear line buffer
      OLIN=' '
C
C     **************************************************
C     ****        Create Profile Header             ****
C     **************************************************
C
      IF (TYPE.EQ.1) THEN
C        Write out PROFILE NUMBER and start point
         WRITE(OLIN,'(2(A,I6))') 'PF',PFNUM,',P',STARTP
         PFNUM=PFNUM+1
      ELSE
C        write out SET number
         WRITE(OLIN,'(A,I6,A)') 'SET',SNUM,',SUM,'
         SNUM=SNUM+1
      END IF
      CALL CRUNCH(OLIN)
      L=NLEN1(OLIN)
C     place in profile buffer string
      PBUF=OLIN(1:L)
      PBUFP=PBUFP+L
C
C     **************************************************
C     ****        Pass Number 2 Creating Profile    ****
C     **************************************************
C
      IF (TYPE.EQ.1) THEN
C        **************************************************
C        ****           Creating Profile Curve         ****
C        **************************************************
C
C        clear element flags
         FFARC=.FALSE.
         FFILET=.FALSE.
C
         DO 20 I=1,N
            READ(UNIT=FUNIT,REC=I) MIP1,ENT,INUM,INUM2,X1,Y1,X2,Y2,Z1
C           WRITE(UNIT=10,FMT=*) MIP1,ENT,INUM,INUM2,X1,Y1,X2,Y2,Z1
            FULCIR=.FALSE.
            OLIN=' '
            IF ( ENT.EQ. ARC ) THEN
C              Is the single boundary a full circle
               IF ( Y2.LT.0.01.AND.
     +            Z1.GE.PI(2.0)-0.01.AND.N.EQ.1 ) THEN
C                 Yes it is so we will allow that
                  ZH1=Z1
                  Z1=Z1/2.0
                  FULCIR=.TRUE.
               END IF
 33            CONTINUE
               XH1=X1+X2*COS(Z1)
               YH1=Y1+X2*SIN(Z1)
               XH2=X1+X2*COS(Y2)
               YH2=Y1+X2*SIN(Y2)
C              calculate mid point of arc
               T3=Z1-Y2
C              WRITE(10,*) 'ANGLES',T3,Y2,Z1
               IF ( T3 .LT. 0.0 ) THEN
                  T3=((PI(2.0)+T3)/2.0)+Y2
               ELSE
                  T3=(Z1+Y2)/2.0
               END IF
C              WRITE(10,*) 'ANGLES',T3,Y2,Z1
               XP1=X1+X2*COS(T3)
               YP1=Y1+X2*SIN(T3)
C              WRITE(10,*) 'old x,y',OX,OY
C              WRITE(10,*) 'End angle',XH1,YH1,
C              WRITE(10,*) 'Start angle',XH2,YH2
C              WRITE(10,*) 'Mid-point',XP1,YP1
 
               IF ( SAME(OX,XH1).AND.SAME(OY,YH1) ) THEN
C                 ARC direction is wrong,reverse it
                  T3=CD0D13(XH1,YH1,XH2,YH2,XP1,YP1)
                  OX=XH2
                  OY=YH2
C                 T1,T2 is end point of arc
                  T1=XH1
                  T2=YH1
C                 set negative direction for circle or fillet
                  CIRCC=',-C'
                  FILLC=',R-'
C                  WRITE(OLIN,'(A,I6)')
C     +            ',-C',INUM
C                 WRITE(10,*) 'End point used'
               ELSE IF ( SAME(OX,XH2).AND.SAME(OY,YH2) ) THEN
C                 ARC direction is correct,use as is
                  T3=CD0D13(XH2,YH2,XH1,YH1,XP1,YP1)
                  OX=XH1
                  OY=YH1
C                 T1,T2 is end point of arc
                  T1=XH2
                  T2=YH2
C                 set positive direction for circle or fillet
                  CIRCC=',C'
                  FILLC=',R'
C                 WRITE(OLIN,'(A,I6)')
C     +            ',C',INUM
C                WRITE(10,*) 'Start point used'
               ELSE
C                 set positive direction for circle or fillet
                  CIRCC=',C'
                  FILLC=',R'
C                 WRITE(10,*) '** ERROR creating GTL profile **'
               END IF
C              test for tangency conditions
C              test for tangency with previous entity
              TAN1=.FALSE.
              TAN2=.FALSE.
              IF(I.GT.1) THEN
                  READ(UNIT=FUNIT,REC=I-1)
     +            MIP0,ENT0,IN00,IN01,X01,Y01,X02,Y02
                  TAN1=TNGENT(ENT0,X01,Y01,X02,Y02,ENT,X1,Y1,X2,Y2)
C                 test for second tangency
              ENDIF
              IF(I.LT.N) THEN
                  READ(UNIT=FUNIT,REC=I+1)
     +            MIP0,ENT0,IN00,IN01,X01,Y01,X02,Y02
                  TAN2=TNGENT(ENT,X1,Y1,X2,Y2,ENT0,X01,Y01,X02,Y02)
              ENDIF
              IF (TAN1.AND.TAN2.AND.(.NOT.FFILET)) THEN
C                fillet may be created
                 WRITE(OLIN,FMT=FORM3(FCODE)) FILLC,X2
                 FFILET=.TRUE.
                 FFARC=.FALSE.
              ELSE
C                must create circle element
                 WRITE(OLIN,'(A,I6)') CIRCC,INUM
                 FFILET=.FALSE.
                 FFARC=.TRUE.
              END IF
C
C              WRITE(10,*) 'Perp dist',T3,DISTXY(XH1,YH1,XH2,YH2)
               T3=SIGN(1.0,T3)*
     +          (ABS(T3)/(DISTXY(XH1,YH1,XH2,YH2)/2.0))
C              write arc section to profile
C
               IF ( FULCIR ) THEN
                  FULCIR=.FALSE.
                  Y2=Z1
                  Z1=ZH1
                  GOTO 33
               END IF
C
            ELSE IF ( ENT .EQ. LINE ) THEN
C              write line section to profile
               IF (FFARC) THEN
C                 last element was ARC
C                 is it tangent
                  IF (TAN2) THEN
C                    tangency between last arc and this line
                     WRITE(OLIN,'(A,I6)') ',L',INUM
                  ELSE
C                    must ensure 2nd intersection used
                     WRITE(OLIN,'(A,I6)') ',I2,L',INUM
                  END IF
               ELSE
C                 last element was either LINE or FILLET
C                 so create normal line element
                  WRITE(OLIN,'(A,I6)') ',L',INUM
               END IF
               FFARC=.FALSE.
               FFILET=.FALSE.
               OX=X2
               OY=Y2
            ELSE IF ( ENT .EQ. 2 ) THEN
C              this is a point entity
               OX=X1
               OY=Y1
               OLIN=' '
            END IF
C
            CALL CRUNCH(OLIN)
            L=NLEN1(OLIN)
            PBUF(PBUFP:)=OLIN(1:L)
            PBUFP=PBUFP+L
            PBUFP=NLEN1(PBUF)+1
            OLIN=' '
C
            IF (PBUFP.GE.72) THEN
C              write line to file with continuation
               PBUF(PBUFP:)='$'
C              place line in buffer
               KCURVE(KCURVL)= PBUF(1:PBUFP)
               KCURVL=MIN(10,KCURVL+1)
               PBUF=' '
               PBUFP=1
            END IF
C
 20      CONTINUE
C
C        logic. The start point must relate to the first entity.
C        the end point can relate to the start or end entity
C        the closest will determine the snap point.
 
      IF(TYPE.EQ.1) THEN
      READ(UNIT=FUNIT,REC=1) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                       XH1,YH1,XH2,YH2,ZH1
      IF ( ENTS .EQ. ARC) THEN
          CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +                DBLE(STARTX),DBLE(STARTY),DX1,DY1)
          CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +                DBLE(ENDX),DBLE(ENDY),DX2,DY2)
      ELSE
          CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +                DBLE(STARTX),DBLE(STARTY),DX1,DY1)
          CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +                DBLE(ENDX),DBLE(ENDY),DX2,DY2)
      ENDIF
      STARTX=REAL(DX1)
      STARTY=REAL(DY1)
C     get the end nearestn points
      READ(UNIT=FUNIT,REC=N) MIP1,ENT,BX,BY,X1,Y1,X2,Y2,Z1,
     +                       XH1,YH1,XH2,YH2,ZH1
      IF ( ENTS .EQ. ARC) THEN
          CALL DCCPAP(DBLE(X1),DBLE(Y1),DBLE(X2),
     +                DBLE(ENDX),DBLE(ENDY),DX1,DY1)
      ELSE
          CALL DCCPLP(DBLE(XH1),DBLE(YH1),DBLE(XH2),DBLE(YH2),
     +                DBLE(ENDX),DBLE(ENDY),DX1,DY1)
      ENDIF
      ENDX1=REAL(DX1)
      ENDY1=REAL(DY1)
      ENDX2=REAL(DX2)
      ENDY2=REAL(DY2)
      IF(DISTXY(ENDX1,ENDY1,ENDX,ENDY).GT.
     +   DISTXY(ENDX2,ENDY2,ENDX,ENDY)) THEN
          ENDX=ENDX2
          ENDY=ENDY2
      ELSE
          ENDX=ENDX1
          ENDY=ENDY1
      ENDIF
      WRITE(OLIN,FMT=FORM2(FCODE))
     +    'P',STARTP,CCX(FN),STARTX-GTLOX,CCY(FN),STARTY-GTLOY
C        eliminate spaces
         CALL CRUNCH(OLIN)
         L=NLEN1(OLIN)
C        write to output file
         WRITE(UNIT=OUNIT,FMT='(A)') OLIN(1:L)
D      WRITE(10,*) '[WRITEGT] STARYT AND END POINTS'
D         WRITE(UNIT=10,FMT='(A)') OLIN(1:L)
      WRITE(OLIN,FMT=FORM2(FCODE))
     +    'P',ENDP,CCX(FN),ENDX-GTLOX,CCY(FN),ENDY-GTLOY
C        eliminate spaces
         CALL CRUNCH(OLIN)
         L=NLEN1(OLIN)
C        write to output file
         WRITE(UNIT=OUNIT,FMT='(A)') OLIN(1:L)
D         WRITE(UNIT=10,FMT='(A)') OLIN(1:L)
      ENDIF
C
         OLIN=' '
C        add end point to kurve definition
         WRITE(OLIN,'(A,I6)')
     +     ',P',ENDP
         CALL CRUNCH(OLIN)
         L=NLEN1(OLIN)
         PBUF(PBUFP:)=OLIN(1:L)
         PBUFP=PBUFP+L
         PBUFP=NLEN1(PBUF)+1
         OLIN=' '
C
      ELSE
C        **************************************************
C        ****            Creating Points Curve         ****
C        **************************************************
         OLIN=' '
C        create a SET of points
         WRITE(OLIN,'(A,I6,A,I6)')
     +        'P',FIRSTP,'>',LASTP
         CALL CRUNCH(OLIN)
         L=NLEN1(OLIN)
         PBUF(PBUFP:)=OLIN(1:L)
         PBUFP=PBUFP+L
         PBUFP=NLEN1(PBUF)+1
         OLIN=' '
      END IF
C
C     ensure record is written to buffer
      KCURVE(KCURVL)=PBUF(1:PBUFP)
      KCURVL=MIN(10,KCURVL+1)
C     write kcurve definition to ascii file
      DO 80 I=1,KCURVL-1
         L=NLEN1(KCURVE(I))
         WRITE(OUNIT,FMT='(A)') KCURVE(I)(1:L)
 80   CONTINUE
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE WRTGT3(FUNIT,NP)
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
            IF ( NP.GT.1 ) THEN
C      if 2 or more points have been selected then make
C      sure that we can go round again
               CCMD='p'
               RETURN
            ELSEIF ( NP .EQ.1 ) THEN
               CALL DEPRNT(155)
               CALL GTMCLO(MEN,CELLN)
            ELSEIF (NP.EQ.0) THEN
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
               CALL DEPRNT(706)
            END IF
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
         ELSE
            CALL DEPRNT(8)
            CALL GTMCLO(MEN,CELLN)
         END IF
C
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
