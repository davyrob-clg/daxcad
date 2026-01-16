C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 readgr.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE RDGERB(GFILE)
C     SUBROUTINE REDGE0()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE RDGERB(GFILE)
C     ========================
C
C    A program to translate the content of a Gerber
C    photo-plotter file into DAXCAD format.
C
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/gerber.inc'
      include 'include/lfu.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/style.inc'
      include 'include/redscl.inc'
      include 'include/wtov.inc'
      include 'include/ndata.inc'
C
      INTEGER*4 IXMIN,IYMIN,IXMAX,IYMAX,IX,IY,ID,IG,DIV
      INTEGER*4 GUNIT,DUNIT,NBLOCK,ST
      INTEGER*4 THISX,THISY,LASTX,LASTY
      INTEGER*2 CC,SX1,SY1,SX2,SY2
      INTEGER*2 TMIP1
      REAL SCA,X(2),Y(2),SANG,EANG,RAD1,PI,RAD,SCALE
      LOGICAL OK,CXY,APOPEN,AFLASH,RDCHK
      CHARACTER*80 BLOCK,GFILE*(*)
C
      EXTERNAL DEWC05,DEWC03,PI
C
      LASTX=0
      LASTY=0
      THISX=0
      THISY=0
      RAD=1
      SCALE=DRWSCL
C     set limits backwards.
      IXMIN=9999
      IXMAX=-9999
      IYMIN=9999
      IYMAX=-9999
      IG=0
      ID=0
      IX=0
      IY=0
C
      COLOUR=CLAYER
C
c      IF ( DBUNIT.EQ.'mm' ) THEN
c         SCA=25.4/1000
c      ELSE IF ( DBUNIT.EQ.'In' ) THEN
c         SCA=1/1000.0
c      END IF
      SCA=2.54/DBUFAC
      AUXSCL=1.0
C      SCA=SCA*SCALE
C
      APOPEN=.FALSE.
      AFLASH=.FALSE.
C     default plotter definition file.
      BLOCK='gb6000.usr'
      CALL GERBTL(BLOCK,SCALE,ST)
      IF ( ST.NE.0 ) THEN
         BLOCK='\reddraw\gb6000.usr'
         CALL GERBTL(BLOCK,SCALE,ST)
         IF ( ST.NE.0 ) THEN
            CALL DEPRNT(667)
            RETURN
         END IF
      END IF
D      WRITE(10,*) 'SCA,AUXSCL = ',SCA,AUXSCL
      SCA=SCA/AUXSCL
C
      CALL FINDU1(GUNIT,OK)
      IF ( .NOT.OK ) THEN
         CALL DEPRNT(668)
      END IF
      OPEN(UNIT=GUNIT,FILE=GFILE,ERR=991)
CIBM
C      LFU(GUNIT)=.TRUE.
CIBM
      NBLOCK=0
C
 100  CONTINUE
C
C     scan of data file now complete,use the data limits
C     to fit to screen
C
C     now draw the data on screen
      REWIND(UNIT=GUNIT)
C
      NBLOCK=0
C
 5    CONTINUE
      RDCHK=.FALSE.
      READ(UNIT=GUNIT,FMT='(A)',ERR=990,END=200)BLOCK
C     end of block mark
      IF ( BLOCK.EQ.'M02*' ) GOTO 200
      NBLOCK=NBLOCK+1
C     find EOB position
C     skip block if no end marker
      IF (INDEX(BLOCK,'*').LE.1) GOTO 5
C     look for G function in block
      CALL GETVAL(BLOCK,'G',IG,ST)
      RDCHK=RDCHK.OR.ST.EQ.0
C     look for D function in block
      CALL GETVAL(BLOCK,'D',ID,ST)
      RDCHK=RDCHK.OR.ST.EQ.0
      IF (ST.EQ.0) THEN
D         WRITE(10,*)'ID = ',ID
C        D-code found
C        open or close aperture
         IF (ID.EQ.01) THEN
            APOPEN=.TRUE.
            AFLASH=.FALSE.
         ELSE IF (ID.EQ.02) THEN
            APOPEN=.FALSE.
            AFLASH=.FALSE.
         ELSE IF (ID.EQ.03) THEN
            AFLASH=.TRUE.
            APOPEN=.FALSE.
         ELSE IF ( IG.EQ.54 ) THEN
C           gerber index starts at 10
            TOOLNO=ID
            IF ( TOOLNO.LE.99 ) THEN
               THICK=TOOLNO
               RAD=REAL(TOLSET(TOOLNO,2))/2.0
            ELSE
               WRITE(10,*)  'Tool definition no:',TOOLNO
            END IF
         ELSE IF ( ID.GE.10.AND.ID.LE.99 ) THEN
C           gerber index starts at 10
            TOOLNO=ID
            IF ( TOOLNO.LE.99 ) THEN
               THICK=TOOLNO
               RAD=REAL(TOLSET(TOOLNO,2))/2.0
            ELSE
               WRITE(10,*)  'Tool definition no:',TOOLNO
            END IF
         ELSE
            WRITE(10,*)'Unknown control:',ID
         END IF
      END IF
C     look for X coord in block
      CALL GETVAL(BLOCK,'X',IX,ST)
      RDCHK=RDCHK.OR.ST.EQ.0
      IF (ST.EQ.0) THEN
         IXMIN=MIN(IX,IXMIN)
         IXMAX=MAX(IX,IXMAX)
         CXY=.TRUE.
      ELSE
         IX =THISX
         CXY=.FALSE.
      END IF
C     look for Y coord in block
      CALL GETVAL(BLOCK,'Y',IY,ST)
      RDCHK=RDCHK.OR.ST.EQ.0
      IF (ST.EQ.0) THEN
         IYMIN=MIN(IY,IYMIN)
         IYMAX=MAX(IY,IYMAX)
         CXY=.TRUE.
      ELSE
         IY =THISY
         CXY=CXY.OR..FALSE.
      END IF
C
      IF ( .NOT.RDCHK ) THEN
         CALL DEPRNT(669)
         CALL CPRINT(BLOCK)
         GOTO 5
      END IF
C
      IF ( CXY ) THEN
         LASTY=THISY
         THISY=IY
         LASTX=THISX
         THISX=IX
      END IF
C
      IF (APOPEN.AND.CXY) THEN
         X(1)=LASTX*SCA
         Y(1)=LASTY*SCA
         X(2)=THISX*SCA
         Y(2)=THISY*SCA
D         WRITE(10,*) 'LINE = ',X(1),Y(1),X(2),Y(2)
         IF(.NOT.(X(1).EQ.X(2).AND.Y(1).EQ.Y(2)))
     +         CALL DEWC03(X(1),Y(1),X(2),Y(2),CLFONT,
     +       CLAYER,TMIP1,OK)
         CXY=.FALSE.
      ELSE IF ( AFLASH ) THEN
         X(1)=THISX*SCA
         Y(1)=THISY*SCA
C         RAD1=RAD*SCA
C        changed to always read in thou
        RAD1=RAD*.00254/DBUFAC
         SANG=0.0
         EANG=PI(2.0)
D         WRITE(10,*) 'ARC  = ',X(1),Y(1),RAD1
         CALL DEWC05(X(1),Y(1),RAD1,SANG,EANG,
     +                CLFONT,CLAYER,TMIP1,OK)
         CXY=.FALSE.
c         AFLASH=.FALSE.
      END IF
C
      GOTO 5
C
 200  CONTINUE
C     Change paper position to x0,y0
C     set bottom left of paper at new position
      WPORGX=0.0+DRWSIZ(1)*PAPTOW/2
      WPORGY=0.0+DRWSIZ(2)*PAPTOW/2
C     call change paper position routine
      CALL CHGP05()
C     now show the new data
      CALL ZOMEXT()
C
C
      CLOSE(UNIT=GUNIT)
CIBM
C      LFU(GUNIT)=.FALSE.
CIBM
 
      RETURN
C
 990  CALL DEPRNT(670)
      RETURN
 991  CALL DEPRNT(671)
C
      END
C
C     ----------------------------------------------------------
 
      SUBROUTINE REDGE0()
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
      include 'include/style.inc'
C
      INTEGER*4 ST,TCOL
      CHARACTER*128 FILNM
      EXTERNAL RDGERB,FNAME
C
      CALL FNAME(FILNM,ST)
      IF ( ST.NE.0 ) THEN
         MEN=0
         RETURN
      END IF
C     go read the data file
      TCOL=COLOUR
      CALL RDGERB(FILNM)
      COLOUR=TCOL
C
      CCMD='q'
C
      END
C
C     --------------------------------------------------
C
