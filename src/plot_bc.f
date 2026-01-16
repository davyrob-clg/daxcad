C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 plot.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ADDENT(ALL,MI)
C     SUBROUTINE ADDINS(MI)
C     SUBROUTINE ADDSIG(MI)
C     SUBROUTINE AXISCG()
C     SUBROUTINE CLIPP(X1,Y1,X2,Y2,VISAB)
C     SUBROUTINE CLIPPS(X1,Y1,X2,Y2,VISAB)
C     SUBROUTINE CLWFPN()
C     SUBROUTINE GROUPE(ALL)
C     SUBROUTINE INTPLT()
C     SUBROUTINE LDPEND(FILNAM,ST)
C     SUBROUTINE MAJPLT()
C     SUBROUTINE MNIPLT()
C     SUBROUTINE MNLPD1()
C     SUBROUTINE MNLPW1()
C     SUBROUTINE OUTOLL(FLSH,LINE)
C     SUBROUTINE PARCHI()
C     SUBROUTINE PL2WO(VX,VY,WX,WY)
C     SUBROUTINE PLOTMN()
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE ADDENT(ALL,MI)
C     =====================
C1                      I2
C1                       I
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*4 LAYER,OPUT,TMPUNT
      INTEGER*2 MI
      LOGICAL OK,ALL
 
C     read the MIP file at the current set MIP position.
C     data will be returned in IMBUFF() in common block
      CALL DIR500(MI,OK)
C     Check to see if Layer is acive on screen . If yes then
C     legitimate to plot , no then don't plot.
      IF (.NOT.ALL.AND.(IMBUFF(2).EQ.SYMBI.OR.IMBUFF(2).EQ.COMPI))THEN
         CALL ADDINS(MI)
      ELSE
C        must be valid entity so write out pointers
C        first set the correct unit number of the temp file
         CALL ADDSIG(MI)
      END IF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE ADDINS(MI)
C     =====================
C
C1    ADDINS scans a symbol or component for entries
C1    on layers for plotting.
C
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/compd.inc'
      include 'include/wrkdat.inc'
      include 'include/wtov.inc'
 
      LOGICAL OK,NOFIND,ADD
      INTEGER*2 TTMIP,ZERO,MI,ENT,LASTLY,STAT,LAYLST(1:256),LLIST
      INTEGER*4 I,J,K
 
      EXTERNAL DRR950,ALLRD,SPCDRW,ALLTFM,I3M,MULT3M
 
C     MM is drawing transform
      ZERO=0
      CNEST=0
      NINSTS=0
      NOFIND=.FALSE.
      ENT=IMBUFF(2)
      STAT=IMBUFF(1)
      LASTLY=-1
      LLIST=0
C
 5    CONTINUE
C     find relation header from instance data
      RELP=IMBUFF(10)
      IF(.NOT.VLAYER(IMBUFF(4))) then
        RETURN
      else
        CALL ADDSIG(MI)
        return
      end if
        
C     if null relation pointer,then unresolved instance
      IF (RELP.LE.ZERO) then
C        write to diag
         WRITE(UNIT=10,FMT=*)
     +   '*** Unresolved COMPONENT instance [DRW066]',CBUFF
         NOFIND=.TRUE.
      END IF
      IF (NOFIND) GOTO 20
 
C     read the relation header
      CALL DRR950(RELP,OK)
C     header data now in buffer
C     test for valid component relation
      IF (RLBUFF(1).NE.COMPM.AND.RLBUFF(1).NE.SYMBM) THEN
         GOTO 20
      END IF
 
C     save the number of records,and entities
      NRRECS=RLBUFF(4)
      NENTS=RLBUFF(5)
C
      DO 15 J=1,NRRECS
         NXTRLR=RLBUFF(2)
C        read the list of entities
         CALL DRR950(NXTRLR,OK)
         DO 10 I=4,10
            TTMIP=RLBUFF(I)
C           data will be returned in IMBUFF() in common block
            IF ( TTMIP.GT.0 ) THEN
               CALL DIR500(TTMIP,OK)
               IF (IMBUFF(2).EQ.COMPI.OR.IMBUFF(2).EQ.SYMBI) THEN
C                 found a nested instance
C                 save reference
                  NINSTS=NINSTS+1
C                 set nest level for this instance,must be 1 greater
C                 than the current level.
                  NSTLEV(NINSTS)=CNEST+1
C                 save pointer to instance
                  NSTMIP(NINSTS)=TTMIP
C                 save instance name
                  NSTNAM(NINSTS)=CBUFF
               ELSE
C                 transform and draw the entity
                  IMBUFF(1)=STAT
                  IMBUFF(2)=ENT
                  ADD=.TRUE.
                  DO 111 K=1,LLIST
                     IF (IMBUFF(4).EQ.LAYLST(K)) ADD=.FALSE.
 111              CONTINUE
C      WRITE(10,*) 'MI,LLIST,ADD,IMBUFF(4):',MI,LLIST,ADD,IMBUFF(4)
                  IF ( ADD ) THEN
                     LLIST=LLIST+1
                     LAYLST(LLIST)=IMBUFF(4)
                     CALL ADDSIG(MI)
                  END IF
               END IF
            END IF
 10      CONTINUE
 15   CONTINUE
 
 20   CONTINUE
C      WRITE(UNIT=10,FMT=*)'[DRW066] end of Component'
C     handle the nested instances if required
      IF (NINSTS.GT.0) THEN
C        must read back the next instance to expand
         TTMIP=NSTMIP(NINSTS)
         CNEST=NSTLEV(NINSTS)
C         WRITE(UNIT=10,FMT=*)'Expanding ',NSTNAM(NINSTS)
         CALL DIR500(TTMIP,OK)
C        decrement count of instances to be expanded
         NINSTS=NINSTS-1
C        go process this instance
         GOTO 5
      END IF
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE ADDSIG(MI)
C     ==================
 
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
 
      INTEGER*2 MI,P
 
C      WRITE(10,*) '[ADDSIG] MI,IMBUFF(1),IMBUFF(2),IMBUFF(4)',
C     +                      MI,IMBUFF(1),IMBUFF(2),IMBUFF(4)
C
      IF ( VLAYER(IMBUFF(4)).AND.IMBUFF(1).LT.100.AND.
     +      .NOT.(IMBUFF(1).EQ.COMPM.OR.IMBUFF(1).EQ.SYMBM).AND.
     1      .NOT.(IMBUFF(2).EQ.COMPM.OR.IMBUFF(2).EQ.SYMBM) )  THEN
C         WRITE(10,*) 'Add to list Unit,No.',
C     +   PENNOS(LAYERN(IMBUFF(4)),1),PENNOS(LAYERN(IMBUFF(4)),2)
C        P=0
C 10     P=P+1
C        READ(PENNOS(LAYERN(IMBUFF(4)),1),REC=P) T
C        and actually write out the pointers
         WRITE(PENNOS(LAYERN(IMBUFF(4)),1),
     +     REC=(PENNOS(LAYERN(IMBUFF(4)),2)+1)) MI,IMBUFF(2)
C        now update the temp file position record
         PENNOS(LAYERN(IMBUFF(4)),2)=PENNOS(LAYERN(IMBUFF(4)),2)+1
C
      END IF
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE AXISCG()
C     ===================
      include 'include/params.inc'
      include 'include/pendat.inc'
C
C           1  2  3  4  5  6  7  8 9  0  1  2  3  4  5  6  7  8
      GOTO (1,10,10,10,10,10,10,10,3,10,10,10,10,10,10,10,10,10 )PLOMOD
C
 1    CONTINUE
C
      IF ( DRWSHT(1:2).NE.'a4') RETURN
      PLBOR(1)=10.0
      PLBOR(2)=7.0
      PLBOR(3)=16.0
      PLBOR(4)=10.0
C
      RETURN
 3    CONTINUE
C
      IF ( DRWSHT(1:2).NE.'a4') RETURN
      PLBOR(1)=16.0
      PLBOR(2)=25.0
      PLBOR(3)=15.0
      PLBOR(4)=10.5
      PAPLIM(5,2)= 0
      PAPLIM(5,1)= 0
      PAPLIM(5,3)=10800
      PAPLIM(5,4)=6800
C
 10   CONTINUE
C
      END
C
C     -----------------------------------------------------------
C
      SUBROUTINE CLIPP(X1,Y1,X2,Y2,VISAB)
C     ===================================
C1                     R, R, R, R,    L
C1                     B, B, B, B,    O
C2       Subroutine CLIP checks whether the line between
C2       X1,Y1 and X2,Y2 invisible on the cuurrent viewport
C2         If it is then VISAB will be set .TRUE. else it
C2       will be set at .FALSE.
C
      LOGICAL VISAB,ALTER
      INTEGER IX1,IX2,IY1,IY2,IX,IY,CEN,LEFT,RIGHT,TOP,BOTTOM,SETCLP
C
      PARAMETER(CEN=0,LEFT=-1,BOTTOM=-1,RIGHT=1,TOP=1)
C
      REAL X,Y,X1,X2,Y1,Y2,A,B,C,D,VAR,NEW
C
      include  'include/pendat.inc'
C
      EXTERNAL SETCLP
C
C        Linear interpolation eqution used for clipping
C        lines to limit of viewport
C
      NEW(A,B,C,D,VAR)=A+((B-A)*(VAR-C)/(D-C))
C
C       Check position of points on viewport
C        X coordinate       -1 off left
C                            0 centre
C                           +1 off right
C        Y coordinate       -1 off bottom
C                            0 centre
C                           +1 off top
C
      IX1=SETCLP(X1,PWXMIN,PWXMAX)
      IY1=SETCLP(Y1,PWYMIN,PWYMAX)
      IX2=SETCLP(X2,PWXMIN,PWXMAX)
      IY2=SETCLP(Y2,PWYMIN,PWYMAX)
C
C     Indicates if line exists on the screen
      VISAB=.FALSE.
C
 10   IF((IX1*IX2).EQ.1 .OR. (IY1*IY2).EQ.1 )  RETURN
C
      VISAB=IX1.EQ.CEN.AND.IY1.EQ.CEN.AND.IX2.EQ.CEN.AND.IY2.EQ.CEN
C
      IF ( VISAB ) RETURN
C
      IF ( IX1.EQ.CEN .AND. IY1.EQ.CEN ) THEN
C
C       Start with first point of line
         IX=IX2
         IY=IY2
      ELSE
C
C       If first point totally inside viewport start
C       on second point
         IX=IX1
         IY=IY1
C
      END IF
C
      IF ( IX .EQ. LEFT ) THEN
C
C       If X coordinate off left of window interpolate to
C       left edge of viewport X coordinate = left boundary
         Y=NEW(Y1,Y2,X1,X2,PWXMIN)
         X=PWXMIN
C
      ELSE IF ( IX .EQ. RIGHT ) THEN
C
C       If X coordinate off right of window interpolate to
C       right edge of viewport X coordinate = right boundary
         Y=NEW(Y1,Y2,X1,X2,PWXMAX )
         X=PWXMAX
C
      ELSE IF ( IY .EQ. TOP ) THEN
C
C       If Y coordinate off top of window interpolate to
C       top edge of viewport Y coordinate = top boundary
         X=NEW(X1,X2,Y1,Y2,PWYMAX)
         Y=PWYMAX
C
      ELSE IF ( IY .EQ. BOTTOM ) THEN
C
C       If Y coordinate off bottom of window interpolate to
C       bottom edge of viewport Y coordinate = bottom boundary
         X=NEW(X1,X2,Y1,Y2,PWYMIN)
         Y=PWYMIN
C
      END IF
C
      IF ( IX .EQ. IX1 .AND. IY .EQ. IY1 ) THEN
C
C       If first point check transfer new points to X1,Y1
         X1=X
         Y1=Y
         IX1=SETCLP(X1,PWXMIN,PWXMAX)
         IY1=SETCLP(Y1,PWYMIN,PWYMAX)
C
      ELSE
C
C       If second point check transfer new points to X2,Y2
         X2=X
         Y2=Y
         IX2=SETCLP(X2,PWXMIN,PWXMAX)
         IY2=SETCLP(Y2,PWYMIN,PWYMAX)
C
      END IF
C
      GOTO 10
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE CLIPPS(X1,Y1,X2,Y2,VISAB)
C1                     R, R, R, R,    L
C1                     B, B, B, B,    O
C2       Subroutine CLIP checks whether the line between
C2       X1,Y1 and X2,Y2 invisible on the cuurrent viewport
C2         If it is then VISAB will be set .TRUE. else it
C2       will be set at .FALSE.
C
      LOGICAL VISAB,ALTER
      INTEGER IX1,IX2,IY1,IY2,IX,IY,CEN,LEFT,RIGHT,TOP,BOTTOM,SETCLP
C
      PARAMETER(CEN=0,LEFT=-1,BOTTOM=-1,RIGHT=1,TOP=1)
C
      REAL X,Y,X1,X2,Y1,Y2,A,B,C,D,VAR,NEW
C
      include  'include/pendat.inc'
C
      EXTERNAL SETCLP
C
C        Linear interpolation eqution used for clipping
C        lines to limit of viewport
C
      NEW(A,B,C,D,VAR)=A+((B-A)*(VAR-C)/(D-C))
C
C       Check position of points on viewport
C        X coordinate       -1 off left
C                            0 centre
C                           +1 off right
C        Y coordinate       -1 off bottom
C                            0 centre
C                           +1 off top
C
      IX1=SETCLP(X1,PVXMIN,PVXMAX)
      IY1=SETCLP(Y1,PVYMIN,PVYMAX)
      IX2=SETCLP(X2,PVXMIN,PVXMAX)
      IY2=SETCLP(Y2,PVYMIN,PVYMAX)
C
C     Indicates if line exists on the screen
      VISAB=.FALSE.
C
 10   IF((IX1*IX2).EQ.1 .OR. (IY1*IY2).EQ.1 )  RETURN
C
      VISAB=IX1.EQ.CEN.AND.IY1.EQ.CEN.AND.IX2.EQ.CEN.AND.IY2.EQ.CEN
C
      IF ( VISAB ) RETURN
C
      IF ( IX1.EQ.CEN .AND. IY1.EQ.CEN ) THEN
C
C       Start with first point of line
         IX=IX2
         IY=IY2
      ELSE
C
C       If first point totally inside viewport start
C       on second point
         IX=IX1
         IY=IY1
C
      END IF
C
      IF ( IX .EQ. LEFT ) THEN
C
C       If X coordinate off left of window interpolate to
C       left edge of viewport X coordinate = left boundary
         Y=NEW(Y1,Y2,X1,X2,PVXMIN)
         X=PVXMIN
C
      ELSE IF ( IX .EQ. RIGHT ) THEN
C
C       If X coordinate off right of window interpolate to
C       right edge of viewport X coordinate = right boundary
         Y=NEW(Y1,Y2,X1,X2,PVXMAX )
         X=PVXMAX
C
      ELSE IF ( IY .EQ. TOP ) THEN
C
C       If Y coordinate off top of window interpolate to
C       top edge of viewport Y coordinate = top boundary
         X=NEW(X1,X2,Y1,Y2,PVYMAX)
         Y=PVYMAX
C
      ELSE IF ( IY .EQ. BOTTOM ) THEN
C
C       If Y coordinate off bottom of window interpolate to
C       bottom edge of viewport Y coordinate = bottom boundary
         X=NEW(X1,X2,Y1,Y2,PVYMIN)
         Y=PVYMIN
C
      END IF
C
      IF ( IX .EQ. IX1 .AND. IY .EQ. IY1 ) THEN
C
C       If first point check transfer new points to X1,Y1
         X1=X
         Y1=Y
         IX1=SETCLP(X1,PVXMIN,PVXMAX)
         IY1=SETCLP(Y1,PVYMIN,PVYMAX)
C
      ELSE
C
C       If second point check transfer new points to X2,Y2
         X2=X
         Y2=Y
         IX2=SETCLP(X2,PVXMIN,PVXMAX)
         IY2=SETCLP(Y2,PVYMIN,PVYMAX)
C
      END IF
C
      GOTO 10
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE CLWFPN()
C     ===================
C1    no arguments needed
C
C2    This routine closes all the temporary
C2    workfiles which were opened to hold the
C2    MIP & ENT data for each pen .
C2    The unit number is held in array PENNOS
C2    held in common block PENDAT.
C
      include 'include/pendat.inc'
C
      INTEGER*4 I,PNUNIT
C
      DO 10 I=1,MAXPEN
C
         IF(  PENN(I) ) THEN
C          this pen was set so file opened before
C          get the unit number from array
           PNUNIT=PENNOS(I,1)
C          ..now close it.
           CLOSE(PNUNIT)
         END IF
C
 10   CONTINUE
c
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE GROUPE(ALL)
C     ===================
C1    no arguments needed
C2                  SUBROUTINE GROUPE
C2     READ DATA FILE AND GROUP ENTITIES BY SELECTED PEN
C2     ASSIGNMENTS ACCORDING TO LAYER NUMBER.STORE IN TEMP
C2     FILES the MIP and Entity types.
C
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include  'include/viewport.inc'
C
      CHARACTER*40 TEXT*40
      INTEGER*4 LAYER,OPUT,TMPUNT
      INTEGER*2 ENTYPE,MI,I,J,DFPK
      LOGICAL OK,ALL
      EXTERNAL OURSCR,DIR500
C
C      clear array values to 0
       DO 17 J=1,2
           DO 15 I=1,MAXPEN
             PENNOS(I,J) = 0
 15        CONTINUE
 17    CONTINUE
C
C     open the neccessary scratch files according to
C     the number of pens being used.
      DO 10 I=1,MAXPEN
C     check within limits of valid number pens
         IF ( PENN(I) ) THEN
C           get a free unit number for this scratch file
C           And open with record length = 4 Bytes.
            CALL OURSCR(TMPUNT,4,OK)
C           store this unit number in array for later use
            PENNOS(I,1) = TMPUNT
         END IF
C
 10    CONTINUE
C
C      Now do a read of the data file and store MIP and
C      ENTITY pointers only in the temp files according
C       to their layer number .i.e for each layer a specific
C      pen will be set
C
      IF ( PLOWIN ) THEN
C        only need to scan display file
         DO 30 DFPK=1,LDFILE(CVPN)-1
            CALL RDISPF(DFPK,ENTYPE,MI,OK)
            IF ( MI.GT.0 ) THEN
               CALL ADDENT(ALL,MI)
            END IF
 30      CONTINUE
      ELSE
C        Read the MIP file so set the Do loop to its length
         DO 20 MI=1,NMIPOS-1
            CALL ADDENT(ALL,MI)
 20      CONTINUE
      END IF
C     now test to see if all the files opened were used .
      DO 33 I=1,MAXPEN
        IF ( PENN(I) .AND. (PENNOS(I,2).EQ.0)) THEN
C       Pen number was set active but no data was written
C       against this so close the opened file and set logical
C       status to show not used.
             CLOSE(PENNOS(I,1))
             PENN(I)=.FALSE.
        END IF
 33   CONTINUE
C
      END
C
C--------------------------------------------------------------------
C
      SUBROUTINE INTPLT()
C     ===================
C
C2    INPLT intialises the parameters for the plotter to used.
C2    given the model that has been set.
C
      include  'include/pendat.inc'
      include 'include/params.inc'
      include 'include/redscl.inc'
      include   'include/wrtinlf.inc'
C
      INTEGER*4 I
      REAL PROPP, PLWID, PLLEN
C     Check if Plotter definition not found.
      IF ( MODELS .EQ. 0 ) GOTO 222
C
      PWXMIN=0
      PWYMIN=0
      PWXMAX=1
      PWYMAX=1
      PVXMIN=0
      PVYMIN=0
      PVXMAX=1
      PVYMAX=1
      PLXMIN=0
      PLYMIN=0
      PLXMAX=1
      PLYMAX=1
C
      CALL I3M(PLWVXY)
      CALL I3M(PLVWXY)
C
      PLTNO=0
C     reset the buffer point
      CLEN=1
      PENSPC=.FALSE.
C
C     find out the model numer for use
C     in the rest of the program.
      PLOMOD=MODLST(CURPLT)
C
C     get the plot directory to use in creatopn of plot file.
      PLTDIR=PLTNAM(CURPLT)
 
C     get name of plotter for display purposes.
      PLNAME=TYPE(CURPLT)
C
C      MODEL  NO       NAME
C              1      'HP 7475'.
C              2      'HP 7585B'.
C              3      'Gould 6320'.
C              4      'Benson 1625/45 (Z80) (VDF,TIM100)'.
C              5      'HP 7550'.
C              6      'ROLAND A2'.
C              7      'Calcomp '.
C     ref 2    8      'HP 7580B'.
C              9      'Hitachi A3/A4'.
C             10      'Benson 1302  (ISDP)'.
C             11      'Gerber Photoplotter'.
C     ref 2   12      'HP 7586B'.
C             13      'metric advent'.
C             14      'postscript (300 dpi)'.
C             15      'Roland  DPX 3300'
C             16      'Houston DMP language'
C             17      'Interleaf'
C             19      'Epson GPR Bitmap'
C
 
C          01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18
      GOTO(10,11,12,13,14,15,16,11,18,19,20,11,21,22,23,24,25,26,27)
     +     PLOMOD
C
 10   CONTINUE
C****************************************************
C                                    H P   7 4 7 5  *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=6
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=16.0
      PLBOR(2)=10.0
      PLBOR(3)=10.0
      PLBOR(4)=7.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=46680
      PAPLIM(2,3)=32958
      PAPLIM(3,3)=16158
      PAPLIM(4,3)=16158
      PAPLIM(5,3)=11040
C
      PAPLIM(1,4)=32958
      PAPLIM(2,4)=22920
      PAPLIM(3,4)=22920
      PAPLIM(4,4)=11040
      PAPLIM(5,4)=7721
C
      GOTO 222
 
 11   CONTINUE
C****************************************************
C                                   H P   7 5 8 5 B *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=8
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=28.0
      PLBOR(2)=5.0
      PLBOR(3)=5.0
      PLBOR(4)=3.0
C
      PAPLIM(1,1)=-23160
      PAPLIM(2,1)=-16200
      PAPLIM(3,1)=-7780
      PAPLIM(4,1)=-7780
      PAPLIM(5,1)=-3580
C
      PAPLIM(1,2)=-16620
      PAPLIM(2,2)=-11680
      PAPLIM(3,2)=-11680
      PAPLIM(4,2)=-5740
      PAPLIM(5,2)=-5740
C
      PAPLIM(1,3)=23160
      PAPLIM(2,3)=16200
      PAPLIM(3,3)=7780
      PAPLIM(4,3)=7780
      PAPLIM(5,3)=3580
C
      PAPLIM(1,4)=16620
      PAPLIM(2,4)=11680
      PAPLIM(3,4)=11680
      PAPLIM(4,4)=5740
      PAPLIM(5,4)=5740
C
      GOTO 222
 12   CONTINUE
C****************************************************
C                              G O U L D   6 3 2 0  *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=10
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=28.0
      PLBOR(2)=3.0
      PLBOR(3)=3.0
      PLBOR(4)=5.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=-15520
      PAPLIM(3,1)=-10680
      PAPLIM(4,1)=-7180
      PAPLIM(5,1)=-3090
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=-11160
      PAPLIM(3,2)=-7660
      PAPLIM(4,2)=-5240
      PAPLIM(5,2)=-5230
C
      PAPLIM(1,3)=0
      PAPLIM(2,3)=15520
      PAPLIM(3,3)=10680
      PAPLIM(4,3)=7180
      PAPLIM(5,3)=3090
C
      PAPLIM(1,4)=0
      PAPLIM(2,4)=11160
      PAPLIM(3,4)=7660
      PAPLIM(4,4)=5240
      PAPLIM(5,4)=5230
C
      GOTO 222
C
 13   CONTINUE
C****************************************************
C                             B E N S O N  1 6 2 5  *
C****************************************************
C     Benson plotters
      HARD=.FALSE.
      FACT=0.1
      MAXPEN=8
      MINPEN=1
      LIM=0.02
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0.0
      PAPLIM(2,1)=0.0
      PAPLIM(3,1)=0.0
      PAPLIM(4,1)=0.0
      PAPLIM(5,1)=0.0
C
      PAPLIM(1,2)=0.0
      PAPLIM(2,2)=0.0
      PAPLIM(3,2)=0.0
      PAPLIM(4,2)=0.0
      PAPLIM(5,2)=0.0
C
      PAPLIM(1,3)=118.9
      PAPLIM(2,3)=84.1
      PAPLIM(3,3)=59.4
      PAPLIM(4,3)=42.0
      PAPLIM(5,3)=21.0
C
      PAPLIM(1,4)=84.1
      PAPLIM(2,4)=59.4
      PAPLIM(3,4)=42.0
      PAPLIM(4,4)=29.7
      PAPLIM(5,4)=29.7
C
      GOTO 222
C
 14   CONTINUE
C
C****************************************************
C                                    H P   7 5 5 0  *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=8
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=16.0
      PLBOR(2)=10.0
      PLBOR(3)=10.0
      PLBOR(4)=7.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=46680
      PAPLIM(2,3)=32958
      PAPLIM(3,3)=16158
      PAPLIM(4,3)=16158
      PAPLIM(5,3)=11040
C
      PAPLIM(1,4)=32958
      PAPLIM(2,4)=22920
      PAPLIM(3,4)=22920
      PAPLIM(4,4)=11040
      PAPLIM(5,4)=7721
C
      GOTO 222
C
 15   CONTINUE
C****************************************************
C                                  R O L A N D  A 2 *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=8
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=-22590
      PAPLIM(2,1)=-15520
      PAPLIM(3,1)=-12280
      PAPLIM(4,1)=-12280
      PAPLIM(5,1)=-12280
C
      PAPLIM(1,2)=-16145
      PAPLIM(2,2)=-11160
      PAPLIM(3,2)=-8640
      PAPLIM(4,2)=-8640
      PAPLIM(5,2)=-8640
C
      PAPLIM(1,3)=22590
      PAPLIM(2,3)=15520
      PAPLIM(3,3)=11480
      PAPLIM(4,3)=4520
      PAPLIM(5,3)=-400
C
      PAPLIM(1,4)=16145
      PAPLIM(2,4)=11160
      PAPLIM(3,4)=8640
      PAPLIM(4,4)=3240
      PAPLIM(5,4)=-240
C
      GOTO 222
 16   CONTINUE
C****************************************************
C                             C A L C O M P 906/907 *
C****************************************************
C     Benson plotters
      HARD=.FALSE.
      FACT=0.1
CDHR  pens for this plotter should be 8
C      MAXPEN=4
      MAXPEN=8
      MINPEN=1
      LIM=0.02
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0.0
      PAPLIM(2,1)=0.0
      PAPLIM(3,1)=0.0
      PAPLIM(4,1)=0.0
      PAPLIM(5,1)=0.0
C
      PAPLIM(1,2)=0.0
      PAPLIM(2,2)=0.0
      PAPLIM(3,2)=0.0
      PAPLIM(4,2)=0.0
      PAPLIM(5,2)=0.0
C
      PAPLIM(1,3)=118.9
      PAPLIM(2,3)=84.1
      PAPLIM(3,3)=59.4
      PAPLIM(4,3)=42.0
      PAPLIM(5,3)=21.0
C
      PAPLIM(1,4)=84.1
      PAPLIM(2,4)=59.4
      PAPLIM(3,4)=42.0
      PAPLIM(4,4)=29.7
      PAPLIM(5,4)=29.7
C
      GOTO 222
 18   CONTINUE
C****************************************************
C                                   H I T A C H I   *
C****************************************************
      HARD=.TRUE.
      FACT=1000/25.4
      MAXPEN=4
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=14.5
      PLBOR(2)=15.0
      PLBOR(3)=12.0
      PLBOR(4)=27.0
C
      PAPLIM(1,1)=-23160
      PAPLIM(2,1)=-16200
      PAPLIM(3,1)=-11260
      PAPLIM(4,1)= 0
      PAPLIM(5,1)= 0
C
      PAPLIM(1,2)=-16620
      PAPLIM(2,2)=-11680
      PAPLIM(3,2)=-8200
      PAPLIM(4,2)= 0
      PAPLIM(5,2)= 0
C
      PAPLIM(1,3)=23160
      PAPLIM(2,3)=16200
      PAPLIM(3,3)=11260
      PAPLIM(4,3)=15140
      PAPLIM(5,3)=10800
C
      PAPLIM(1,4)=16620
      PAPLIM(2,4)=11680
      PAPLIM(3,4)=8200
      PAPLIM(4,4)=10800
      PAPLIM(5,4)=6740
C
      GOTO 222
C
 19   CONTINUE
C****************************************************
C                             B E N S O N  1 6 2 5  *
C****************************************************
C     Benson plotters
      HARD=.FALSE.
      FACT=0.1
      MAXPEN=4
      MINPEN=1
      LIM=0.02
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0.0
      PAPLIM(2,1)=0.0
      PAPLIM(3,1)=0.0
      PAPLIM(4,1)=0.0
      PAPLIM(5,1)=0.0
C
      PAPLIM(1,2)=0.0
      PAPLIM(2,2)=0.0
      PAPLIM(3,2)=0.0
      PAPLIM(4,2)=0.0
      PAPLIM(5,2)=0.0
C
      PAPLIM(1,3)=118.9
      PAPLIM(2,3)=84.1
      PAPLIM(3,3)=59.4
      PAPLIM(4,3)=42.0
      PAPLIM(5,3)=21.0
C
      PAPLIM(1,4)=84.1
      PAPLIM(2,4)=59.4
      PAPLIM(3,4)=42.0
      PAPLIM(4,4)=29.7
      PAPLIM(5,4)=29.7
C
      GOTO 222
C
 20   CONTINUE
C****************************************************
C                               GERBER PHOTOPLOTTER *
C****************************************************
C     Gerber Photoplotter output
C     Uses the global GRBSCL to set parameters
C     for use in Hi-Res mode of plotter,thus
C     allowing for 1/10th Thou resolution plotting.
C
      HARD=.FALSE.
      FACT=GRBSCL*1000.0/25.4
d      WRITE(10,*) '[INTPLT] GRBSCL AUXSCL ',GRBSCL,AUXSCL
      MAXPEN=1
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=AUXSCL/1000.0*GRBSCL*46811
      PAPLIM(2,3)=AUXSCL/1000.0*GRBSCL*33110
      PAPLIM(3,3)=AUXSCL/1000.0*GRBSCL*23386
      PAPLIM(4,3)=AUXSCL/1000.0*GRBSCL*16535
      PAPLIM(5,3)=AUXSCL/1000.0*GRBSCL*8268
C
      PAPLIM(1,4)=AUXSCL/1000.0*GRBSCL*33110
      PAPLIM(2,4)=AUXSCL/1000.0*GRBSCL*23386
      PAPLIM(3,4)=AUXSCL/1000.0*GRBSCL*16535
      PAPLIM(4,4)=AUXSCL/1000.0*GRBSCL*11693
      PAPLIM(5,4)=AUXSCL/1000.0*GRBSCL*11693
C
      GOTO 222
C
 21   CONTINUE
C****************************************************
C                             mm. output vector     *
C****************************************************
C     Benson plotters
      HARD=.FALSE.
      FACT=1.0
      MAXPEN=1
      MINPEN=1
      LIM=0.02
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0.0
      PAPLIM(2,1)=0.0
      PAPLIM(3,1)=0.0
      PAPLIM(4,1)=0.0
      PAPLIM(5,1)=0.0
C
      PAPLIM(1,2)=0.0
      PAPLIM(2,2)=0.0
      PAPLIM(3,2)=0.0
      PAPLIM(4,2)=0.0
      PAPLIM(5,2)=0.0
C
      PAPLIM(1,3)=1189
      PAPLIM(2,3)=841
      PAPLIM(3,3)=594
      PAPLIM(4,3)=420
      PAPLIM(5,3)=210
C
      PAPLIM(1,4)=841
      PAPLIM(2,4)=594
      PAPLIM(3,4)=420
      PAPLIM(4,4)=297
      PAPLIM(5,4)=297
 
      GOTO 222
 
 22   CONTINUE
C****************************************************
C                                postscript output  *
C****************************************************
      HARD=.TRUE.
      FACT=72/25.4
      MAXPEN=1
      MINPEN=1
      LIM=0.02
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0.0
      PAPLIM(2,1)=0.0
      PAPLIM(3,1)=0.0
      PAPLIM(4,1)=0.0
      PAPLIM(5,1)=0.0
C
      PAPLIM(1,2)=0.0
      PAPLIM(2,2)=0.0
      PAPLIM(3,2)=0.0
      PAPLIM(4,2)=0.0
      PAPLIM(5,2)=0.0
C
      PAPLIM(1,3)=1189
      PAPLIM(2,3)=841
      PAPLIM(3,3)=594
      PAPLIM(4,3)=420
      PAPLIM(5,3)=210
C
      PAPLIM(1,4)=841
      PAPLIM(2,4)=594
      PAPLIM(3,4)=420
      PAPLIM(4,4)=297
      PAPLIM(5,4)=297
C
      GOTO 222
 
 23   CONTINUE
C****************************************************
C                       R O L A N D   D P X 3 3 0 0 *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=8
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=-22590
      PAPLIM(2,1)=-17300
      PAPLIM(3,1)=-17300
      PAPLIM(4,1)=-17300
      PAPLIM(5,1)=-17300
C
      PAPLIM(1,2)=-16145
      PAPLIM(2,2)=-11880
      PAPLIM(3,2)=-11880
      PAPLIM(4,2)=-11880
      PAPLIM(5,2)=-11880
C
      PAPLIM(1,3)=22590
      PAPLIM(2,3)=16340
C      PAPLIM(2,3)=16300
      PAPLIM(3,3)=6450
      PAPLIM(4,3)=-500
      PAPLIM(5,3)=-5420
C
      PAPLIM(1,4)=16145
      PAPLIM(2,4)=11880
c      PAPLIM(2,4)=12000
      PAPLIM(3,4)=4920
      PAPLIM(4,4)=0
      PAPLIM(5,4)=-3480
 
      GOTO 222
 
 24   CONTINUE
C****************************************************
C                             H O U S T O N   D M P *
C****************************************************
C
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=6
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=22.0
      PLBOR(2)=15.5
      PLBOR(3)=15.5
      PLBOR(4)=18.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=46320
      PAPLIM(2,3)=32400
      PAPLIM(3,3)=22520
      PAPLIM(4,3)=15560
      PAPLIM(5,3)=7160
C
      PAPLIM(1,4)=32400
      PAPLIM(2,4)=22520
      PAPLIM(3,4)=15560
      PAPLIM(4,4)=10640
      PAPLIM(5,4)=10640
C
C
      GOTO 222
C
 25   CONTINUE
C****************************************************
C                                I n t e r l e a f  *
C****************************************************
C
      HARD=.TRUE.
 259  CONTINUE
      FACT=1/25.4
      MAXPEN=1
      MINPEN=1
      LIM=0.01
C
      PLBOR(1)=0.0
      PLBOR(2)=0.0
      PLBOR(3)=0.0
      PLBOR(4)=0.0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PROPP = DRWSIZ(1) / DRWSIZ(2)
      IF (PROPP.LT.1) THEN
         PLWID = 7
         PLLEN = 9.5
      ELSE
         PLWID = 7
         PLLEN = 4.75
      ENDIF
C
      PAPLIM(1,3) = PLWID
      PAPLIM(2,3) = PLWID
      PAPLIM(3,3) = PLWID
      PAPLIM(4,3) = PLWID
      PAPLIM(5,3) = PLWID
C
      PAPLIM(1,4) = PLLEN
      PAPLIM(2,4) = PLLEN
      PAPLIM(3,4) = PLLEN
      PAPLIM(4,4) = PLLEN
      PAPLIM(5,4) = PLLEN
C
      GOTO 222
 26   CONTINUE
C****************************************************
C                                    H P   7 4 7 5  *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=6
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=0
      PLBOR(2)=.63
      PLBOR(3)=.63
      PLBOR(4)=0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=46680
      PAPLIM(2,3)=32958
      PAPLIM(3,3)=16158
      PAPLIM(4,3)=16800
      PAPLIM(5,3)=11835
C
      PAPLIM(1,4)=32958
      PAPLIM(2,4)=22920
      PAPLIM(3,4)=22920
      PAPLIM(4,4)=11835
      PAPLIM(5,4)=8400
C
      GOTO 222
 27   CONTINUE
C****************************************************
C                                    EPSON          *
C****************************************************
      HARD=.TRUE.
      FACT=40.0
      MAXPEN=1
      MINPEN=1
      LIM=0.1
C
      PLBOR(1)=0
      PLBOR(2)=.63
      PLBOR(3)=.63
      PLBOR(4)=0
C
      PAPLIM(1,1)=0
      PAPLIM(2,1)=0
      PAPLIM(3,1)=0
      PAPLIM(4,1)=0
      PAPLIM(5,1)=0
C
      PAPLIM(1,2)=0
      PAPLIM(2,2)=0
      PAPLIM(3,2)=0
      PAPLIM(4,2)=0
      PAPLIM(5,2)=0
C
      PAPLIM(1,3)=46680
      PAPLIM(2,3)=32958
      PAPLIM(3,3)=16158
      PAPLIM(4,3)=16800
      PAPLIM(5,3)=11835
C
      PAPLIM(1,4)=32958
      PAPLIM(2,4)=22920
      PAPLIM(3,4)=22920
      PAPLIM(4,4)=11835
      PAPLIM(5,4)=8400
C
      GOTO 222
 
C
C****************************************************
C
 222  CONTINUE
      CALL AXISCG()
C
C****************************************************
C
      END
C
C     -----------------------------------------------------------
C
      SUBROUTINE LDPEND(FILNAM,ST)
C     ============================
C
      include 'include/vntable.inc'
      include 'include/pendat.inc'
      include 'include/lfu.inc'
 
      INTEGER*4 ST,UNITN,P1,P2,NLEN,LENGTH,TMP,
     +    POINT,TPOINT,SPOINT,FONTN,SET(4),P
      LOGICAL OP,OK,EX,NUM
      DOUBLE PRECISION DANS
      REAL VAL,TOTAL,TEMVAL(20)
      CHARACTER*(*) FILNAM,STRNG2*100,NAME*20
      EXTERNAL NLEN,DEPRNT
C
 5    CONTINUE
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
C     wait for access to file if necessary
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +        FORM='FORMATTED')
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
C
C
 10      CONTINUE
C
 21      CONTINUE
C        we are not reading a number first.
         SET(1)=-1
         SET(2)=-1
         SET(3)=-1
         SET(4)=-1
C
         NUM=.FALSE.
         READ(UNIT=UNITN,FMT='(A)',END=20,ERR=30)STRNG2
C        save active length of string.
         LENGTH=NLEN(STRNG2)
C        check for blank lines ignore them.
         IF ( LENGTH.EQ.0 ) GOTO 21
C        correct into upper case.
         CALL FOLDUP(STRNG2)
C        find the first quote
         P1=0
 
 35      CONTINUE
C
 22      CONTINUE
         P1=P1+1
         IF ( STRNG2(P1:P1).EQ.' '.AND.P1.LT.LENGTH) GOTO 22
         P2=P1
 23      CONTINUE
         P2=P2+1
         IF ( STRNG2(P2:P2).NE.' '.AND.P2.LT.LENGTH) GOTO 23
C
         IF ( P1 .GT. LENGTH ) THEN
            IF (      SET(1).LT.0 ) THEN
               CALL DEPRNT(424)
            ELSE IF ( SET(2).LT.0.AND.
     +                SET(3).LT.0.AND.
     1                SET(4).LT.0 )      THEN
               CALL DEPRNT(425)
               WRITE(NAME,FMT='(I4)') SET(1)
               CALL CPRINT(DICT01(532)(1:NLEN(DICT01(532)))//NAME(1:4))
            ELSE
               PENSD(SET(1),1)=SET(2)
               PENSD(SET(1),2)=SET(3)
               PENSD(SET(1),3)=SET(4)
            END IF
            GOTO 21
         END IF
C
         IF ( .NOT.NUM ) THEN
            IF ( STRNG2(P1:P2) .EQ.'PEN' ) THEN
               P=1
            ELSE IF ( STRNG2(P1:P2) .EQ.'VELOCITY' ) THEN
               P=2
            ELSE IF ( STRNG2(P1:P2) .EQ.'ACCELERATION' ) THEN
               P=3
            ELSE IF ( STRNG2(P1:P2) .EQ.'FORCE' ) THEN
               P=4
            ELSE
               CALL CPRINT('UNKNOWN:"'//STRNG2(P1:P2)//'"')
               GOTO 30
            END IF
            NUM=.TRUE.
         ELSE
            CALL AEXPRN(STRNG2(P1:P2),DANS,*30)
            SET(P)=INT(DANS)
            NUM=.FALSE.
         END IF
 
         P1=P2
C
         GOTO 35
C
 30      CONTINUE
C        set status for read error
         ST=1
C        close the file
         GOTO 25
 20      CONTINUE
C
         ST=0
 25      CONTINUE
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
      END IF
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE MAJPLT()
C     ===================
C1    no arguments required
C2    Subroutine MAJPLT is the entry point
C2    for the PLOT option from the master menu
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/gerber.inc'
CIBM
C          INCLUDE '\INCLUDE\VERTYPE.INC'
CIBM
CPC386
C          INCLUDE '\INCLUDE\\VERTYPE.INC'
CPC386
C
      INTEGER*4 CP,C,TMEN,TCELL,I
      CHARACTER*40 TEMP
      REAL X,Y
C
      EXTERNAL MNIPLT,PLTD00,TCURS,GTMCLO,INTPLT,
     +         MNLPD1,CLRPEW,GTMCHI,PLTW00,PLOTMN
C
      IF ( MODELS.EQ.0  ) RETURN
CIBM|PC386
C      IF(RFIDF.LE.0.0.OR.RFIDF2.LE.0.0) RETURN
CIBM|PC386
C
C     initialize common variables, updated from des40 ja
      NANNOT=0
      PLOSCL=DRGSCL
      PLTSCL=DRWSCL
C
      CALL INTPLT()
C     Now activate the PLOT major option menu.
      CALL MNIPLT()
C     initialize PLOT DRAWING option menu.
      CALL MNLPD1()
C     clear the error and prompt windows.
      CALL CLRPEW
C
      CURTL=0
C
      PLTSHT=DRWSHT
C     Read the major option menu to find out what he wants to
C     plot. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C     Making single line the default insert text
 
 10   CONTINUE
C     Read a cursor hit to select PLOT type
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') THEN
         RETURN
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'D') THEN
C           PLOT DRAWING option
            CALL PLTD00()
         ELSE IF (CCMD.EQ.'W') THEN
C           PLOT DRAWING option
            CALL PLTW00()
         ELSE IF (CCMD.EQ.'S') THEN
C           PLOT DRAWING option
            CALL PLTS00()
         ELSE IF (CCMD.EQ.'w') THEN
C           PLOT DRAWING option
            CALL PLTSW0()
         ELSE
C           unrecognized dimension option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q' ) THEN
            RETURN
         END IF
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
         GOTO 10
      ELSE IF ( MEN .EQ. 3 ) THEN
         CALL PLOTMN()
      ELSE
         CALL DEPRNT(284)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE MNIPLT()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major PLOT option
C2
      EXTERNAL GTDMEN,GTCLRM,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the PLOT major option.
C     write header to menu 2
      CALL GTDMHD(27,2)
C
C     enter DRAWING option
      CALL GTDMEN(315,2)
C
C     enter WINDOW option
      CALL GTDMEN(316,2)
C
C     SPB - Don't allow the PLOT->Screen button to be displayed
C     enter SCREEN DUMP option
      CALL GTDMEN(317,2)
 
C     enter window plot option
      CALL GTDMEN(357,2)
C
      END
C
C     ----------------------------------------------------
C
C
      SUBROUTINE MNLPD1()
C     ===================
C
C1    No arguments required.
C
C2    Loads the LEVEL1 MENU options and
C
      include 'include/pendat.inc'
      include 'include/params.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 TCELL,TMEN
      INTEGER I
      CHARACTER TOKEN*1
      EXTERNAL GTPMEN,GTCLRM,GTDOMN,GTDMWT,GTDMEN,FNDPOS,FNDTOK
     +         GTDMWT
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C2    S is the token for Set paper size
C     show current sheet size, and it is
C     a popup so set the flag for multiple
      GTMULT = .TRUE.
      CALL GTDMWT(310,3,DRWSHT)
C     punt out the paper name in the cell below.
      TOKEN = ' '
      TMEN = 3
      TCELL=VNPOS(310) + 1
      CALL GTPMEN(PAPNAM(CURPNO),TOKEN,TMEN,TCELL)
C
C2    P is the token for PLOTTER TYPE
      GTMULT = .TRUE.
      CALL GTDMWT(311,3,TYPE(CURPLT))
C
C2    T is the token for H/S text
      IF ( HARD ) THEN
         CALL GTDOMN(313,3)
C        Hardware text
      ELSE
C        Software text
         CALL GTDOMN(314,3)
      END IF
C
      CALL GTDMEN(318,3)
CGCU
C     plot scale, token is 's'
      CALL GTDMEN(700,3)
C     show current scale
      CALL FNDTOK(700,TOKEN)
      CALL GTMCWT(3,TOKEN,PLOSCL)

C     plot copies, token is 'C'
      CALL GTDMEN(701,3)
C     set copies = 1
      PLTCOP=1
      CALL FNDTOK(701,TOKEN)
      CALL GTMCWI(3,TOKEN,PLTCOP)

CGCU

C
C2    c is the token for Clipping of data
      CALL GTDMEN(375,3)
      CLIPIT=.TRUE.
c
C     enable HIRES and ARCTOL if required
      CALL PARCHI()
C
      END
C
C     ----------------------------------------------------
C

      SUBROUTINE MNLPW1()
C     ===================
C1    No arguments required.
 
      include 'include/pendat.inc'
      include 'include/params.inc'
C
      INTEGER I
      CHARACTER TOKEN*1
C
C2    Loads the LEVEL1 MENU options and
C
      EXTERNAL GTDMEN
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE OUTOLL(FLSH,LINE)
C     ============================
C1                        L  C*(*)
C1                        I    I
C
C2     This routine outputs a character string
C2     to the current opened plot file.
C
      include 'include/pendat.inc'
C
      CHARACTER*(*) LINE
      CHARACTER*255 PBUF
      LOGICAL FLSH
      INTEGER*4 NLEN1,LEN
      EXTERNAL NLEN1
      SAVE PBUF
C     crush the file.
      CALL CRUNCH(LINE)
C      LEN=NLEN1(LINE)
C      IF ( LEN+CLEN.GT.255.OR.FLSH ) THEN
C        WRITE(UNIT=PLTUNT,FMT='(A)') PBUF(1:CLEN-1)
C        CLEN=1
C      END IF
C      PBUF(CLEN:)=LINE(1:LEN)
C      CLEN=CLEN+LEN
 
      WRITE(UNIT=PLTUNT,FMT='(A)') LINE(1:NLEN1(LINE))
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PARCHI()
C     ===================
C
C1    no arguments required
C
C2    Subroutine PARCHI is used to show the
C2    HIRES and ARCTOL options for use during
C2    GERBER plotting.
C2    This routine loads the menu cells for both
C2    options and shows the current state of the
C2    settings.
C
      include 'include/pendat.inc'
C
      INTEGER*4 I
      CHARACTER TOKEN*1
C
C     if plotter model is GERBER (PLOMOD=11)
C     then must enable options for ARCTOL and HIRES
      IF (PLOMOD.EQ.11) THEN
C        load options for ARCTOL and HIRES
C        load Hires option
          CALL GTDMEN(354,3)
         IF (GRBSCL.GT.1.0) THEN
C           hilite hires mode
            CALL FNDPOS(354,I)
            CALL GTMCHI(3,I)
         END IF
C        load ARCTOL option
         CALL FNDTOK(355,TOKEN)
C        write current ARCTOL to cell
         CALL GTDMWR(355,3,ARCTOL,'(F5.3)')
      ELSE
C        ensure Hires and Arctol options removed
         CALL FNDPOS(354,I)
         CALL GTCLRC(3,I)
         CALL FNDPOS(355,I)
         CALL GTCLRC(3,I)
      END IF
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PL2WO(VX,VY,WX,WY)
C     =============================
C1                      R, R, R, R
C1                      I, I, O, O
C2      Subroutine PL2WO converts VX,VY (plotters coordinates)
C2      into world coordinates WX,WY
C
      include  'include/pendat.inc'
      REAL VX,VY,WX,WY
C
      EXTERNAL NEWXY
C
C       Using plotter to world transformation PLVWXY
      CALL NEWXY(VX,VY,WX,WY,PLVWXY)
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE PLOTMN()
C     ===================
C
C2    PLOTMN sets the modifier options
C2    for plotting functions.
C
      include 'include/pendat.inc'
      include 'include/menun.inc'
      include 'include/params.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C
      CHARACTER TOKEN*1,FILNM*40,INPUT*80,TEMP*80
      INTEGER*4 I, MOD, ST, NLEN, POPMEN,TMEN,TCELL,OLDPLT
      LOGICAL OK
      INTRINSIC  MOD
      DOUBLE PRECISION DN, DN1
      EXTERNAL GTCLRC,GTPMEN,GTDMWT,GTDOMN,FNDPOS,FNDTOK,NLEN,
     +         CHGP07
C
      TMEN = MEN
      TCELL = CELLN
      IF ( CCMD .EQ. 'S' ) THEN
C       set the paper size for the drawing
        POPMEN = 20
        CALL MENPOP(POPMEN, OK)
        IF(OK) THEN
C         see which paper has been selected
          CALL CHGP07(I, CCMD)  
C         save the sheet descriptor
          PLTSHT(:2)=PAPLST(I)
C         set the paper size for the drawing
C         show current selection in cell
          GTMULT = .TRUE.
          CALL GTDMWT(310,3,PLTSHT(:2))
C         punt out the paper name in the cell below.
          TOKEN = ' '
          TCELL=VNPOS(310) + 1
          CALL GTCLRC(TMEN,TCELL)
          CALL GTPMEN(PAPNAM(I),TOKEN,TMEN,TCELL)
          CALL AXISCG()                            
        ENDIF
        CALL GTMCLO(TMEN,TCELL)  
      ELSE IF ( CCMD .EQ. 'P' ) THEN
C        show current plotter model
         POPMEN = -2
         CALL MENPOP(POPMEN,OK)
C        NOTE: Plotter 17 is Interleaf and must ONLY
C        be accessable through the WRITE menu.
         OLDPLT = CURPLT
         IF(OK) THEN
            CURPLT = ICHAR(CCMD)
         ENDIF
         IF (MODLST(CURPLT).EQ.17) CURPLT = OLDPLT
         CALL GTMCLO(TMEN,TCELL)
         GTMULT = .TRUE.
         CALL GTDMWT(311,3,TYPE(CURPLT))
         CALL INTPLT()
C2       T is the token for H/S text
         IF ( HARD ) THEN
             CALL FNDPOS(313,I)
             CALL GTCLRC(3,I)
             CALL GTDOMN(313,3)
C            Hardware text
         ELSE
C            Software text
             CALL FNDPOS(314,I)
             CALL GTCLRC(3,I)
             CALL GTDOMN(314,3)
         END IF
C        enable options for HIRES and ARCTOL for
C        GERBER if required
         CALL PARCHI()
      ELSE IF ( CCMD .EQ. 'T' ) THEN
C        set HardWare Text/Interpolation mode
         HARD=.NOT.HARD
         CALL GTCLRC(MEN,CELLN)
         IF ( HARD ) THEN
C           hardware text
            CALL GTDOMN(313,3)
         ELSE
C           set SoftWare Text/Interpolation mode
            CALL GTDOMN(314,3)
         END IF
      ELSE IF ( CCMD.EQ.'D' ) THEN
C        loading pen specification.
C        Ask user for filename
         CALL DPRMXP(81,FILNM)
C        if null file name assume abort required
         IF (NLEN(FILNM).EQ.0) THEN
            PENSPC=.FALSE.
            RETURN
         END IF
         CALL LDPEND(FILNM,ST)
         PENSPC=ST.EQ.0
      ELSE IF (CCMD.EQ.'A') THEN
C        ARCTOL to be set
 111     CALL DPRMXP(497,FILNM)
         IF (NLEN(FILNM).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
            RETURN
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(FILNM,DN,*111)
            ARCTOL=REAL(DN)
C           update cell content
            CALL GTDMWR(355,3,ARCTOL,'(F5.3)')
         END IF
      ELSE IF (CCMD.EQ.'H') THEN
C        Hiresolution to be toggled
         IF (GRBSCL.EQ.1.0) THEN
C           set to hires
            GRBSCL=10.0
C           reset plotter parameters
            CALL INTPLT()
C           leave cell hilited
         ELSE
C           reset to lores
            GRBSCL=1.0
C           reset plotter parameters
            CALL INTPLT()
C           ensure cell is not hilited
            CALL GTMCLO(MEN,CELLN)
         END IF
      ELSE IF (CCMD.EQ.'c') THEN
C        this is used to toggle the clipp option
         IF(CLIPIT) THEN
           CLIPIT=.FALSE.
         ELSE
           CALL GTMCLO(MEN,CELLN)
           CLIPIT=.TRUE.
         END IF
CGCU
      ELSE IF (CCMD.EQ.'s') THEN
C        Plot scale
 500     CONTINUE
         CALL DPRMXP(845,INPUT)
C
         IF ( NLEN(INPUT).EQ.0 ) THEN
C           zero length string returned,abort this option
C           ensure cell no longer highlighted
            CALL GTMCLO(MEN,CELLN)
         ELSE
            CALL AEXPRN(INPUT,DN1,*500)
C           set drawscale to new value
            IF (REAL(DN1).EQ.0.0) THEN
               CALL DEPRNT(160)
               GOTO 500
            ENDIF
            PLTSCL=REAL(DN1)
C           save the scale as character string
            IF (PLTSCL.LE.1) THEN
C              set new character string for scale
               X=1/PLTSCL
               WRITE(UNIT=TEMP,FMT='(F10.3)') X
               INPUT='1/'//TEMP(1:NLEN1(TEMP))
            ELSE
               X=PLTSCL
               WRITE(UNIT=TEMP,FMT='(F10.3)') X
               INPUT=TEMP(1:NLEN1(TEMP))//'/1'
            END IF
            CALL CRUNCH(INPUT)
            PLOSCL=INPUT(1:NLEN1(INPUT))
C           show scale selected
            CALL FNDTOK(700,TOKEN)
            CALL GTMCWT(3,TOKEN,PLOSCL)
C           clear any error messages
            CALL CLRPEW()
         END IF
      ELSE IF ( CCMD .EQ. 'C' ) THEN
C***************************************************************
C              NUMBER  OF COPIES OPTION                        *
C***************************************************************
C        user wants to change the number of copies
C        prompt for new number and return expression
 501     CALL DPRMXP(264,INPUT)
C
         IF (NLEN(INPUT).EQ.0 )THEN
C           user has returned zero length string
C           assume that he has change his mind and return for input
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           evaluate an arithmetic expression from the keyboard
            CALL AEXPRN(INPUT,DN,*501)
            PLTCOP=INT(DN)
C           ensure copy status is correct
            IF(PLTCOP.LT.1) PLTCOP=1
            CALL GTMCWI(3,CCMD,PLTCOP)
         END IF
CGCU
      END IF
C
      END
C
C     ----------------------------------------------------
C

