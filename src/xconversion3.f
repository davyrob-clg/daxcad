C
C     @(#)  412.1 date 6/11/92 xconversion3.f 
C
C
C     Filename    : xconversion3.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:48:06
C     Last change : 92/06/11 14:44:30
C
C     Copyright : Practical Technology Limited  
C     File :- xconversion3.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     FUNCTION INT2(I4)
C     FUNCTION INT4(I2)
C     SUBROUTINE DXFMRKBLOCK(OUNIT,NUM,OK)
C     SUBROUTINE DXFSTL(OUNIT,OK)
C     SUBROUTINE DXFWCI(OUNIT,M)
C     SUBROUTINE DWPL01(TMIP,PRIPNT)
C     SUBROUTINE GTPMEN(GTSTRG,GTSTG1,GTWID4,GTCID4)
C     SUBROUTINE AUTODR(OK,CLOCK,QUIT,L1,L2,L3)
C     SUBROUTINE SCROSS(APX,APY)
C     SUBROUTINE PLTD02(PSCALE)
C     SUBROUTINE CLOSPF(AUTO,QUIT,ST)
C     SUBROUTINE POLDRW(OK)
C
C     |-----------------------------------------------------------------|
C
C     This source file contains routines which have been modified
C     for the X conversion for daxcad. At the head of each
C     routine a comment exists to nindicate the source file form whence it
C     was extracted. This file is under sr10 SCCS
C

C     ******************************************************************
C
C     Extracted from writedxf.f
C
C     ******************************************************************


      SUBROUTINE DXFMRKBLOCK(OUNIT,NUM,OK)
C     =====================================
C1    VARYPE                   I4    I4 L
C1    IOSTAT                   I     I  O
C
C2    Writes out the block header for a marker defintion
C2  
C2    Arguments:-
C2  
C2    OUNIT           ->          F77 file unit number for writing
C2    NUM             ->          Marker number (DAXCAD)
C2  
C2  
C2    Error Returns:
C2  
C2    OK  .TRUE.  If success
C2  
C2  
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
      include 'include/marker.inc'
      include 'include/product.inc'
C
      INTEGER*4 OUNIT
      INTEGER*4 NUM
      INTEGER*4 NLEN
      LOGICAL OK
C
      EXTERNAL NLEN
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'BLOCK'
C     layer number
      CALL DXFWLN(OUNIT)
C     Block name
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
C     block name will be marker number
C
      WRITE(UNIT=CBUFF,FMT='(A,I3,A)',ERR=999)
     +          PRODUCT(1:NLEN(PRODUCT)),
     +          NUM,
     +          'MARKER'
C
      CALL CRUNCH(CBUFF)
      CALL FOLDUP(CBUFF)
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) CBUFF(1:NLEN(CBUFF))
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C     block type flags (Block has attribiutes)
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
 
      OK = .TRUE.
C     ok go home
      RETURN
C
999   CONTINUE
C     error from write here
C
      OK = .FALSE.
      END
C
C     ******************************************************************
C
C     Extracted from writedxf.f
C
C     ******************************************************************

      SUBROUTINE DXFSTL(OUNIT,OK)
C     ===========================
C1    VARTYPE            I4    L
C1    IOSTAUS             I    O
C
C2    Writes out the layer if it has somthing on it
C2    codes the name with a prefix of the product name
C2  
C2  
C2    Arguments:-
C2  
C2    OUNIT           ->          F77 file unit number for writing
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK  .TRUE.  If success
C2  
C2  
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
      include 'include/product.inc'
C
      INTEGER*4 I
      INTEGER*4 NLEN
      INTEGER*4 OUNIT
      INTEGER*4 ST
      INTEGER*4 TCOLOR(15)
      CHARACTER TEMP*80
      LOGICAL OK
C
      EXTERNAL NLEN
C
      DATA TCOLOR/1,3,5,4,2,6,7,2,3,2,1,2,5,2,1/
C
C     load up colors
      DO 100 I=1,15
          COLOR(I) = TCOLOR(I)
100   CONTINUE
C
C     this section is layer releated
C
C     must be at least one layer with somting on it
      WRITE(UNIT=OUNIT,FMT='(A)') '0'
      WRITE(UNIT=OUNIT,FMT='(A)') 'TABLE'
      WRITE(UNIT=OUNIT,FMT='(A)') '2'
      WRITE(UNIT=OUNIT,FMT='(A)') 'LAYER'
      WRITE(UNIT=OUNIT,FMT='(A)') '70'
      WRITE(UNIT=OUNIT,FMT='(A)') '255'
C     lopp round for layers
      DO 10 I=0,255
C         this layer has something on it must be defined
          IF(TLAYER(I).GT.0) THEN
C             layer header
              WRITE(UNIT=OUNIT,FMT='(A)') '0'
              WRITE(UNIT=OUNIT,FMT='(A)') 'LAYER'
C             layer name
              WRITE(UNIT=OUNIT,FMT='(A)') '2'
C             write out daxcad name with this one
              WRITE(UNIT=TEMP,FMT='(A,I3,A)')
     +                PRODUCT,
     +                I,
     +                LNAME(I)
C             take out all ILLEGAL character please
              CALL DXFILTER(TEMP,ST)
C             no need to do this but do it any way just incase DAXFILTER changes
              CALL CRUNCH(TEMP)
C             must be in uppercase for some reason (ACAD)
              CALL FOLDUP(TEMP)
              WRITE(UNIT=OUNIT,FMT='(A)') TEMP(:NLEN(TEMP))
C             Table flags layer is not frozen
              WRITE(UNIT=OUNIT,FMT='(A)') '70'
              WRITE(UNIT=OUNIT,FMT='(A)') '0'
C             color value
              WRITE(UNIT=OUNIT,FMT='(A)') '62'
C             if the color is negative then the layer is on
              IF(VLAYER(I)) THEN
                  WRITE(UNIT=OUNIT,FMT='(I3)') 7
              ELSE
                  WRITE(UNIT=OUNIT,FMT='(I3)') -7
              ENDIF
              WRITE(UNIT=OUNIT,FMT='(A)') '6'
C             write out a line style of daxcad font 1
              WRITE(UNIT=OUNIT,FMT='(A)')
     +                            LINESN(1)
     +                            (:NLEN(LINESN(1)))
          ENDIF
C
10    CONTINUE
C     all ok
      OK = .TRUE.
      END
C
C     ******************************************************************
C
C     Extracted from writedxf.f
C
C     ******************************************************************
 
      SUBROUTINE DXFWCI(OUNIT,M)
C     ==========================
C1    VARYPE            I4   R3*3
C1    IOSTAT            I     I
C
C2    This routine writes out a symbol or
C2    instance with the rotation angle and
C     translation point
C2  
C2  
C2    Arguments:-
C2  
C2    OUNIT           ->          F77 file unit number for writing
C2    M               ->          Transform of symbol/comp
C2  
C2  
C2    Error Returns:
C2  
C2    
C2    NONE
C2  
C2  
C
      include  'include/product.inc'
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/dxf.inc'
      include  'include/wildc.inc'
C
      REAL M(3,3)
      REAL PARS(9)
      REAL DEG
C
      INTEGER*4 OUNIT
      INTEGER*4 NLEN,I
      INTEGER*4 POS
      INTEGER*4 POINT
      INTEGER*4 BINDEX
      INTEGER*4 TMIP
      INTEGER*4 ST
      INTEGER*2 RELP
      LOGICAL OK
      CHARACTER*100 TEMP
      CHARACTER*80 NAME
C
      EXTERNAL NLEN
      EXTERNAL DECODM
      EXTERNAL DEG
      EXTERNAL BINDEX
C
C
      IF(IMBUFF(2).EQ.GROUP) THEN
          NOG=NOG+1
          WRITE(UNIT=TEMP,FMT='(2A,I4)')
     +             PRODUCT(1:NLEN(PRODUCT)),
     +             'GROUP',
     +             NOG
          POS = 1
          CALL I3M(M)
      ELSE
C         re-format the name into daxcad specific names. no that name will be ov
          POS = BINDEX(CBUFF,FILCHR)+1
          POINT = BINDEX(CBUFF,'.')
 
C         look for sym or cmp extension.
          IF(POINT.LE.1) THEN
C             strip .cmp or .sym name
              POINT = NLEN(CBUFF)
          ELSE
C             increment past point
              POINT = POINT - 1
          ENDIF
C
C         set the name of the component
          NAME = CBUFF(POS:POINT)
          CALL SUBSTC(NAME,'.',' ')
          CALL CRUNCH(NAME)
C         save current mip for unique ID
          RELP=IMBUFF(10)
          IF(RELP.LE.0) THEN
C             cannot read this instance record
              GOTO 999
          ENDIF
C         if null relation pointer,then unresolved instance
          CALL DRR950(RELP,OK)
          TMIP = RLBUFF(3)
C         filter out vile stuff
          CALL DXFILTER(NAME,ST)
          IF ( ST.EQ.2 ) THEN
C             set this to a value which is still unique but indicates an error
              NAME = 'ILLEGALNAME'
          ENDIF
          WRITE(UNIT=TEMP,FMT='(A,I5,A)',ERR=999)
     +             PRODUCT(1:NLEN(PRODUCT)),
     +             TMIP,
     +             NAME(1:NLEN(NAME))
      ENDIF
C     cruch and fold to upper case this one
      CALL CRUNCH(TEMP)
      CALL FOLDUP(TEMP)
C
C     decode the transformation matrix for this instance.
      CALL DECODM(M,PARS)
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'INSERT'
C     layer number
      CALL DXFWLN(OUNIT)
C     Instance name
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
C     If the instance name has a . then it must not be allowed.
      WRITE(OUNIT,'(A)',ERR=999) TEMP(:NLEN(TEMP))
C     TRanslation from the existing origin
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) PARS(1)
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) PARS(2)
C     scaling factors in the X and Y axis
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) PARS(4)
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) PARS(5)
C     rotaton angle
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '50'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) DEG(PARS(9))
      OK= .TRUE.
      RETURN
999   CONTINUE
      OK= .FALSE.
      END
 
 

C     ******************************************************************
C
C     Extracted from define.f
C
C     ******************************************************************

      SUBROUTINE DWPL01(TMIP,PRIPNT)
C     ===============================
C
C1    vartype             I2    I2
C1    iostatus            I     O
C
C2    Subroutine DWPL01 writes a property
C2    link structure to file,returning the
C2    record number occupied by the link in PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT
      INTEGER*2 TMIP
C
C     point to next record
      PRIPNT=NPRPOS
      NPRPOS=NPRPOS+1
C
C     point to continuation
      PRLBUF(4,1)=NPRPOS
C     write link record
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (PRLBUF(I,1),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=PRLBUF(I,1)
 500  CONTINUE
CAPOLLO|SUN|PC386
C     write prop list
CIBM
C      WRITE(PRIFLU,REC=NPRPOS,ERR=999) (PRLBUF(I,2),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 501 I=1,8
         PRIFIL(I,NPRPOS)=PRLBUF(I,2)
 501  CONTINUE
CAPOLLO|SUN|PC386
C
      NPRPOS=NPRPOS+1
 999  CONTINUE
C
      END



C     ******************************************************************
C
C     Extracted from apogtx84.f
C
C     ******************************************************************


      SUBROUTINE GTPMEN(GTSTRG,GTSTG1,GTWID4,GTCID4)
C     ==============================================
C1    vartype             C      C       I4     I4
C1    iostatus            I      I       I      I
C2
C2    Formerly GTXT_PRINT_MENU_CELL prints the character string
C2    GTSTRG on cell number GTCID4 of menu number GTWID4.
C2    Also writes the string into the correct position of the
C2    text buffer associated with the menu,and enters the
C2    command character 'GTSTG1' into the command buffer
C2    associated with the menu.
C
      include 'include/gpr.ins.inc'
C
      include 'include/dtext.inc'
      include 'include/gtxt2.inc'
      include 'include/menpop.inc'
      include 'include/macro.inc'
      include 'include/interface.inc'
      include 'include/apollo.inc'
C
      INTEGER*2 BMWIND(2,2),DISPOS(2),WINDOW(4)
      INTEGER*2 GTWID,GTCID
      INTEGER*2 L,CP,LP,TL,CURPOS(1:2),CURROP(8),CURORG(2)
      INTEGER*4 ST,CURPAT,GTWID4,GTCID4,NLEN1,NLEN,COL,ORMODE
      INTEGER*4 TMP
      REAL X,Y
      REAL X1,Y1,X2,Y2
      LOGICAL CURACT,UNOBSCURED
      CHARACTER GTSTRG*(*),GTSTG1*(*)
C
      EXTERNAL NLEN1,GTSPMG
C
C     convert from passed I*4 to internal i*2 for apollo use
      GTWID=GTWID4
      GTCID=GTCID4
C     Find length of passed string
      L=NLEN1(GTSTRG)
C     Get last useable character position
      LP=GML(GTWID,5)
      IF (L.GT.LP) THEN
C        Set actual length of string for output
         L=LP
      END IF

      TMP = MENUB
      IF(GTMULT.AND.MENUS.AND.HIPLAN.GT.0) THEN
           MENUB = POPMRK
           CALL TOOLPEN(POPMRK)
           GTCID=GTCID4-1
           X1=GMC(GTWID4,1)-1+(GTCID)*GMC(GTWID4,6)
           Y1=GMC(GTWID4,2)+(GTCID)*GMC(GTWID4,5)+4
           X2=X1+GMC(GTWID4,3)+3
           Y2=Y1-GMC(GTWID4,4)
C          clear cell,remember to reverse order of y-values
           CALL CLEARW(X1,Y2,X2,Y1)
           CALL TOOLPEN_TEXT(MENUF,POPMRK,.FALSE.,ST)
           GTCID=GTCID4
      ELSE
           CALL TOOLPEN_TEXT(MENUF,MENUB,.FALSE.,ST)
      ENDIF
C
C     write the text into the menu text buffer at the
C     correct position.note the offset of 1 between
C     buffer and screen position
C     save menu text.
      GMB(GTWID,GTCID-1)=GTSTRG
C     save menu token.
      GMBC(GTWID,GTCID-1)=GTSTG1
C     Is it a multiple.
      GMCMUL(GTWID,GTCID-1)=GTMULT
      IF ( MENUS ) THEN
         X=GMC(GTWID,1)+(GTCID-1)*GMC(GTWID,6)+1
         Y=GMC(GTWID,2)+(GTCID-1)*GMC(GTWID,5)-1
C        WRITE(*,*)'[GTPMEN] L,STRING,TOKEN=',L,GTSTRG,GTSTG1
C        shuff out the text.
         CALL GTSPMG(X,Y,GTSTRG(1:L),.FALSE.)
      END IF
C     if this is a multiple cell then REVERSE it out
      IF(GTMULT.AND.MENUS) THEN
C         load up the cell with a clever marker
          BMWIND(1,1)=0
          BMWIND(2,1)=0
          BMWIND(1,2)=GTCBMS(GTWID,1)
          BMWIND(2,2)=GTCBMS(GTWID,2)+1
C         set the origin in the display for the BLT
C         find position for bitmap in display
          DISPOS(1)=GML(GTWID,1)+(GTCID-1)*GMC(GTWID,6)-1
          DISPOS(2)=(GTCID-1)*GMC(GTWID,5)+GML(GTWID,2)-(TEXH-3)-1
C         set raster op to exclusive or
          IF ( HIPLAN.EQ.0) THEN
              CALL TOOLPEN(MENUF)
              CALL ROPOR()
              CALL BITMAP(MNBITM,BMWIND,DISPOS)
          ENDIF
          CALL ROPREP()
      ENDIF
C
      GTMULT=.FALSE.
      MENUB = TMP
C
      END


C     ******************************************************************
C
C     Extracted from autop3.f
C
C     ******************************************************************



      SUBROUTINE AUTODR(OK,CLOCK,QUIT,L1,L2,L3)
C     ==========================================
C1    VARYPE            L   L     L   R  R  R
C1    IOSTAT            I   I     O   O  O  O
C
C2    Will get a direction vector from the keyboard 
C2
C2
C2
C2  
C2  
C2    Arguments:-
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  

C
      include  'include/gpr.ins.inc'
C
      include  'include/apollo.inc'
      include  'include/dig2wo.inc'
      include  'include/datatype.inc'
      include  'include/macro.inc'
      include  'include/menun.inc'
      include  'include/curwin.inc'
      include  'include/wtov.inc'
C
      INTEGER*4 X,Y,ABX,ABY,TCELL,TMEN,MENKEY,TFONT
      INTEGER*4 CURSX,CURSY,MEN1,CELLN1
      INTEGER*2 SCRPOS(2),EVTYPE,I,ICHAR,SCPOSN(2)
      INTEGER*4 ST,LONE,LTWO,ONE,TWO,KEY,C
      INTEGER*2 TIMEC(3),SORCE,DELC,BAKC,DGB
      LOGICAL UNOBSCURED,LWAIT,OK,MENOK
      CHARACTER*1 EVDATA,HITKEY
      INTEGER*2 APX,APY,COUNT
      REAL  POINT(2,15),X1,X2,X3,Y1,Y2,Y3,CD0D13
      REAL L1,L2,L3,X4,Y4,SANG,EANG,P1,P2,P3,DISTXY,DIST
      REAL XC,YC,RAD,CANG,ARCRAD,CENX,CENY,ANG
      DOUBLE PRECISION DXC,DYC,DRAD
      LOGICAL CLOCK,QUIT,BUTTON,DRAW,CLKWS
C
C
      EXTERNAL MENHIT,MOUSE,CD0D13
C
C     Time to acquire the display now.
      UNOBSCURED=GPR_$ACQUIRE_DISPLAY(ST)
C
      TFONT =1
      COUNT = 0
      BUTTON = .FALSE.
      OK =.FALSE.
      QUIT = .FALSE.
C     set exclusive or raster op.
      CALL ROPXOR()
      CALL STCURP(REAL(APCURX(1)),REAL(APCURY(3)))
C     Draw the graphics cursor in the default start position.
 
C     undraw the cursor
C      CALL GPR_$MULTILINE(APCURX,APCURY,NPOS,ST)
C     save the current cursor position
 
      APX = APCURX(1)
      APY = APCURY(3)
C     draw a cross in the postion
      CALL SCROSS(APX,APY)
C
C     if the cursor is in the menu are do not draw
C      write(10,*) 'apcurx,apcury:',apcurx(1),apcurx(3)
C
C
C
 222  CONTINUE
C
      LWAIT=GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
C
C     If the arrow keys have been used then come back to
C     this routine to move the cursor as normal
 
 225  CONTINUE
      IF (EVTYPE.EQ.GPR_$LOCATOR) THEN
C         update the cursor position.
          CURSX=SCRPOS(1)
          CURSY=SCRPOS(2)
          CALL WRKHIT(REAL(CURSX),REAL(CURSY),OK)
C
          IF(.NOT.OK) THEN
             QUIT = .TRUE.
             GOTO 400
          ENDIF
          IF(BUTTON) THEN
              COUNT =COUNT +1
              IF(COUNT.LE.5) THEN
                  CALL SC2WO(REAL(CURSX),REAL(CURSY),
     +                      POINT(1,COUNT), POINT(2,COUNT) )
              ELSE
                  COUNT = 5
              ENDIF
              CALL SCROSS( APX , APY )
              APX = CURSX
              APY = CURSY
              CALL SCROSS(APX,APY)
          ELSE
              CALL SCROSS( APX,APY)
              APX = CURSX
              APY = CURSY
              CALL SCROSS(APX,APY)
          ENDIF
          GOTO 222
      ELSE IF (EVTYPE.EQ.GPR_$BUTTONS ) THEN
            I = ICHAR(EVDATA)
C           discard first upstroke of button
            IF (I .GE. 65.AND.I.LT.68) THEN
                IF( BUTTON ) THEN
                     COUNT = MIN(INT(COUNT),15)
                     COUNT = MAX(INT(COUNT),1)
                     CALL SC2WO(REAL(CURSX),REAL(CURSY),
     +                      POINT(1,COUNT), POINT(2,COUNT) )
                    GOTO 500
                ENDIF
            ELSE
C               first down stroke
                BUTTON = .TRUE.
            ENDIF
            GOTO 222
      ELSE
            GOTO 222
      ENDIF
500   CONTINUE
      IF(COUNT.LT.4) THEN
         OK = .FALSE.
         QUIT = .FALSE.
         GOTO 400
      ENDIF
C     get three points on the arc
      COUNT = MIN(INT(COUNT),5)
      X1 = POINT(1,1)
      X2 = POINT(1,COUNT/2)
      X3 = POINT(1,COUNT)
      Y1 = POINT(2,1)
      Y2 = POINT(2,COUNT/2)
      Y3 = POINT(2,COUNT)
 
 
 
      CLOCK = ( CD0D13(X1,Y1,X3,Y3,X2,Y2) .LE. 0.0)
 
 
      CALL CV0L14 (X1,Y1,X3,Y3,L1,L2,L3)
 
      XC = (X1+X3)/2
      YC = (Y1+Y3)/2
 
      IF ( CLKWS(XC,YC,X2,Y2,X1,Y1) ) THEN
 
          SANG=CANG(XC,YC,X3,Y3)
          EANG=CANG(XC,YC,X1,Y1)
 
      ELSE
 
          SANG=CANG(XC,YC,X1,Y1)
          EANG=CANG(XC,YC,X3,Y3)
 
      ENDIF
      RAD = DISTXY(X1,Y1,X3,Y3)/2
      CALL DRWFAW(XC,YC,RAD,SANG,EANG,TFONT)
C      CALL DRWARC(XC,YC,RAD,SANG,EANG)
      CALL DRAWLT(X1,Y1,X3,Y3)
      EANG = CANG(XC,YC,X3,Y3)
      IF (CLOCK) THEN
          ANG = 0.17453
      ELSE
          ANG = -0.17453
      ENDIF
      EANG = EANG + ANG
      X4 = XC + RAD*COS(EANG)
      Y4 = YC + RAD*SIN(EANG)
 
C     do the arraow calculateions
      DIST = DISTXY(X1,Y1,X3,Y3)
      CALL CV0L14 (X3,Y3,X4,Y4,P1,P2,P3)
      CALL ARROW(X3,Y3,P1,P2,P3,DIST)
      CALL CV0L14 (X3,Y3,X1,Y1,P1,P2,P3)
      DIST = DISTXY(X1,Y1,X3,Y3)
      CALL ARROW(X3,Y3,P1,P2,P3,DIST)
      OK = .TRUE.
C     reset mein cursor
400   CONTINUE
      CALL SCROSS(APX,APY)
      CALL UPDACP(APX,APY)
C     If the msf are running then low the cell
C     undraw the cursor
C
C     Now set raster op back to default
      CALL ROPREP()
C
      CALL GPR_$RELEASE_DISPLAY(ST)
C
C
      END
 

C     ******************************************************************
C
C     Extracted from autop3.f
C
C     ******************************************************************


      SUBROUTINE SCROSS(APX,APY)
C     ==========================
C1    VARYPE            I2  I2
C1    IOSTAT            I   I
C
C2    This routine draws a cross
C2  
C2  
C2    Arguments:-
C2  
C2    APX             ->              X coordinate
C2    APY             ->              Y coordinate
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
C 
      include 'include/gpr.ins.inc'
C 
      INTEGER*2 APX
      INTEGER*2 APY
      INTEGER*2 X
      INTEGER*2 Y
      INTEGER*4 ST
      LOGICAL UNOBSCURED
C 
      CALL GPR_$MOVE(APX,APY,ST)
      X = APX+20
      Y = APY-20
      CALL GPR_$LINE(X,Y,ST)
C
      X = APX+8
      Y = APY-12
      CALL GPR_$LINE(X,Y,ST)

      X = APX+12
      Y = APY-8
      CALL GPR_$LINE(X,Y,ST)

      X = APX+20
      Y = APY-20
      CALL GPR_$LINE(X,Y,ST)
C 
      END



C     ******************************************************************
C
C     Extracted from plot.f
C
C     ******************************************************************

      SUBROUTINE PLTD02(PSCALE)
C     =========================
C
C     ******************************************************
C1    This SUBROUTINE converts coordinate data from an array
C1    data file into plotter commands suitable for hp7475
C1    OR HP7585.Works on assumption of a spooler type
C1    environment.User must input :-
C1       1)Filename of Data file
C1       2)Pens to be used during plot
C1       3)Layers associated with particular pens
C1       4)type of plotter used is assumed i.E.7585
C1    Default pen is pen 1 and unasigned  layers  are
C1    plotted with this pen.
C1    An initial search is done by active pen numbers and
C1    data held in temp files.Then it is read to master
C1    plot file and optimisation checks done i.e.
C1       1)End old line = start new line then keep pen
C1          down - INCLUDEs search on BOTH new ends
C1       2)Arc/Circle chord length is relative to radius
C1          and sets accordingly
C1    ****************************************************
C
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/pendat.inc'
      include 'include/masti.inc'
      include 'include/params.inc'
      include 'include/ndata.inc'
      include 'include/wtov.inc'
      include 'include/redscl.inc'
      include 'include/gerber.inc'
      include 'include/library.inc'
      include 'include/viewport.inc'
 
      CHARACTER*80  OLINE,CSIZE,TEMP*40
      CHARACTER PLOUT*80
      LOGICAL FPEN,DEL,IGNORE,PSCALE
      LOGICAL   EX
      INTEGER*2 ENTYPE,TMIP1,NLEN2
      INTEGER*2 NUM,TI
      INTEGER*4 LNUM
      INTEGER*4 L
      INTEGER*4 LAYER,I,DATA(1:255),NDATA,ST
     +          ,LKOUNT,OPUT,TXP1,TYP1,NLEN1
     1          ,TXP2,TYP2,II,MAXREC,NLEN,MOD
      REAL PXS,PYS,PXL,PYL,ORGX,ORGY
      REAL  ANG,DIST1,DIST2,DIST,M(3,3),
     1      XT1,YT1,XT2,YT2,RATIO,TRAT,SCALE
      INTRINSIC MOD
      EXTERNAL NLEN,FINDU2,SETPEN,SETLAY,SETPAP,GROUPE,
     +         FILHED,NEWPEN,CPRINT,CLWFPN,FILEND,NLEN1
C
      LOGICAL OK,ALL,QUIT,AUTO
      DOUBLE PRECISION DSIZE
C
      AUTO = .FALSE.
C
      IF (PLOMOD.EQ.19) THEN
C
C         Epson printer output APOLLO ONLY
C
          CALL DPRMXP(81,PLTFIL)
          L=NLEN(PLTFIL)
          IF( L.EQ.0 ) THEN
C             Null plot name must return at once.
              RETURN
          ENDIF
          PLOUT=PLTFIL(1:L)//'_'//PLTSHT(1:2)
		  if (PLOMOD.EQ.14) THEN
C
C		  DPS - Add in a .PS please.  Its the way this are now
C
              L=NLEN(PLOUT)
	          PLOUT=PLOUT(1:L)//'.'//'.ps'
		  ENDIF
             
C         make sure name is lower case
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C          CALL UNFOLD(PLOUT)
C         check if plot file already exists.
          INQUIRE(FILE=PLOUT,EXIST=EX)
          IF ( EX ) THEN
              CALL OVERWRITE(PLOUT,OK)
              IF (.NOT.OK) THEN
                  RETURN
              ENDIF
          ENDIF
          CALL PLOTEPSON(PLOUT,ST)
          RETURN
      ENDIF
C
      IF (PLOMOD.EQ.11) THEN
        SCALE=1.0
        OLINE='gb6000.usr'
        CALL GERBTL(OLINE,SCALE,ST)
        IF ( ST.NE.0 ) THEN
CIBM
C          OLINE='\reddraw\gb6000.usr'
CIBM
CPC386
C          OLINE='\\reddraw\\gb6000.usr'
CPC386
CAPOLLO|SUN
          OLINE=LIBRARY(1:NLEN1(LIBRARY))//'/gb6000.usr'
CAPOLLO|SUN
          CALL GERBTL(OLINE,SCALE,ST)
          CALL INTPLT()
          IF ( ST.NE.0 ) THEN
             CALL DEPRNT(658)
          END IF
         END IF
      END IF
 
CC      WRITE(10,'(////A)') '***********   PLOT ***********'
C      WRITE(10,*) '[pltd02]WIN XP1,YP1,XP2,YP2:',XP1,YP1,XP2,YP2
      CALL SETPAP(PSCALE,TXP1,TYP1,TXP2,TYP2,OK)
      IF ( .NOT. OK ) GOTO 203
C      WRITE(10,*)'[pltd02]PLOT WIN PWXMIN:',PWXMIN,PWYMIN,PWXMAX,PWYMAX
C      WRITE(10,*)'[pltd02]PLOT WIN PVXMIN:',PVXMIN,PVYMIN,PVXMAX,PVYMAX
C      WRITE(10,*)'[pltd02]PLOTTXP1,TYP1,TXP2,TYP2:',TXP1,TYP1,TXP2,TYP2
C
      IF ( PSCALE ) THEN
 700    CONTINUE
C       go get rectangle for  screen mapping
        CALL WWINDO(PXS,PYS,PXL,PYL,OK)
        IF (.NOT.OK) GOTO 700
C
        ORGX=PXS-WPXMIN
        ORGY=PYS-WPYMIN
        ORGX=INT(ORGX/PAPTOW)
        ORGY=INT(ORGY/PAPTOW)
 
        PXS=INT(PXS/PAPTOW)
        PYS=INT(PYS/PAPTOW)
        PXL=INT(PXL/PAPTOW)
        PYL=INT(PYL/PAPTOW)
 
C       save values in common block for use in FILHED
        VPORGX = ORGX
        VPORGY = ORGY
 
        WRITE(UNIT=CBUFF,FMT='(A,I8,A,I8)')
     +    'Paper_size:',INT(PXL-PXS),',',INT(PYL-PYS)
        CALL CRUNCH(CBUFF)
        CALL CPRINT(CBUFF)
      ELSE
        VPORGX = 0.0
        VPORGY = 0.0
      END IF
 
C     check to see if any entities in drawing.
      IF ( LDFILE(CVPN).LT.2.AND.TLAYER(255).EQ.0) THEN
C        no entities !!
         RETURN
      END IF
 
      CALL  OPENPF(AUTO,OK)
      IF ( .NOT. OK ) RETURN
C
C
C     *******************************************************
C     Prompt for Pen numbers to be used and set arrays i.e.
C     for I=1.MAXPEN PENN(I) is .true. if pen requested else .false.
C     For I=0,255 LAYERN(I) holds default pen number 1
C     *******************************************************
C
      CALL SETPEN(ALL,QUIT)
      IF ( QUIT ) GOTO 203
C
C     *******************************************************
C     Prompt for layers used with pens chosen and set array
C     For I=0,255 ,  LAYERN(I) holds associated pen number
C     *******************************************************
C
      CALL SETLAY(ALL,QUIT)
      IF ( QUIT ) GOTO 203
C
C     *******************************************************
C     Using data From scan , set paper extents of plotter
C     according to drawing ratio XP1,XP2,YP1,YP2 are the drawing
C     extents ; TXP1,TYP1,TXP2,TYP2 are the new point extents
C     in plotter coordinates.
C     *******************************************************
C
C
      IF ( PSCALE ) THEN
 
        PLTVPT=.TRUE.
        PVXMIN=ORGX
        PVYMIN=ORGY
        PVXMAX=(PXL-PXS)+ORGX
        PVYMAX=(PYL-PYS)+ORGY
 
      ELSE
 
        PLTVPT=.FALSE.
 
      END IF
 
 
C     *******************************************************
C     Read data file and group entities by selected pen
C     assignments according to later no. stored in layer(N).
C     temp files called PEN(I) where I is the pen number
C     used as temporary storage
C     *******************************************************
C
C     *******************************************************
C     output header block to plotfile. consists of xon-xoff
C     initiator , scale command and resets p1 & p2 if needed.
C     xp1,xp2,yp1,yp2 are the drawing extents ; txp1,typ1,txp2
C      ,typ2 are the  extents in plotter coordinates.
C     *******************************************************
 
C     *******************************************************
C     output header block to plotfile. consists of xon-xoff
C     initiator , scale command and resets p1 & p2 if needed.
C     xp1,xp2,yp1,yp2 are the drawing extents ; txp1,typ1,txp2
C      ,typ2 are the  extents in plotter coordinates.
C     *******************************************************
C
      CALL FILHED(TXP1,TYP1,TXP2,TYP2)
C
      CALL RELDIS(NUM)
 
      FPEN=.TRUE.
C
      DO 201 I=1,MAXPEN
C
         IF(PENN(I)) THEN
C           set current pen for layer checking.
            CURPEN=I
C
            CALL CHGPEN(I)
C           set the pen position to some probable unused value
            AXP=-123456
            AYP=-123456
            DO 206 II=1,NMIPOS-1
C               read the file at correct record posn , the MIP held
C               in TMIP1 and Entity type in ENTYPE
                TI=II
                CALL DIR500(TI,OK)
C               Check if entity is visible
                IF (IMBUFF(1).GE.100) THEN
                     GOTO 206
                ENDIF
                LNUM = IMBUFF(4)
                ENTYPE=IMBUFF(2)
                IF ( VLAYER(LNUM).AND.LAYERN(LNUM).EQ.I.OR.
     +               ((ENTYPE.EQ.COMPI.OR.ENTYPE.EQ.SYMBI).AND.
     +               VLAYER(LNUM)) ) THEN

                    TMIP1=TI
C                   now check to see if valid entity types
                    CALL ALLRD(TMIP1,ENTYPE,M,DEL)
 
C                   test status of entity
C                   ignore GROUP totally,only a header or Component Master
                    IGNORE=IMBUFF(1).EQ.COMPM.OR.IMBUFF(1).EQ.SYMBM
     +             .OR.ENTYPE.EQ.GROUP.OR.ENTYPE.EQ.COMPM.OR.
     +              ENTYPE.EQ.SYMBM
                    IF (.NOT.IGNORE) THEN
                         IF (ENTYPE.EQ.COMPI.OR.ENTYPE.EQ.SYMBI) THEN
C                         component instance to be drawn
                              CALL PLT066(M)
                          ELSE
C                            Line,Arc,Hatch,Text to be drawn
C                            Linear,Radial,Diametral,Angular dimension to be drawn.
                             CALL PLTDRW(ENTYPE,TMIP1,.FALSE.,M)
                          ENDIF
                    END IF
                 END IF
206         CONTINUE
         END IF
C
C
201   CONTINUE
C
C     *******************************************************
C     OUTPUT RESET CODE TO PLOTTER AT FILE END
C     *******************************************************
      CALL FILEND()
C
C     *******************************************************
C     AND FINISH .......
C     *******************************************************
C
20    CONTINUE
      CALL CLOSPF(AUTO,QUIT,ST)
      CALL ACRDIS(NUM)
      RETURN
C
 203  CONTINUE
C
      IF(.NOT.QUIT) CALL CLWFPN()
      CALL CLOSPF(AUTO,QUIT,ST)
      RETURN
C
      END

C

C     ******************************************************************
C
C     Extracted from plot.f
C
C     ******************************************************************

      SUBROUTINE CLOSPF(AUTO,QUIT,ST)
C     ===============================
C1    VARYPE             L    L   I4
C1    IOSTAT             I    I   O
C
C2    Close the current plot file and spool it if required.
C2  
C2  
C2    Arguments:-
C2  
C2    AUTO            ->          Close the file and spool immedialtly
C2    QUIT            ->          Close and delete the file.
C2  
C2  
C2    Error Returns:
C2  
C2    Error from iostat on close
C2  
C2  
C
      include 'include/pendat.inc'
      include 'include/product.inc'
C
      LOGICAL QUIT
      LOGICAL AUTO
      INTEGER*4 ST
      INTEGER*4 NLEN
      CHARACTER*80 SPLR
C
      EXTERNAL NLEN
C
      IF ( QUIT ) THEN
C         he want to delete it
          CLOSE(UNIT=PLTUNT,STATUS='DELETE',IOSTAT=ST)
      ELSE
C         close file and keep it
          CLOSE(UNIT=PLTUNT,STATUS='KEEP',IOSTAT=ST)
          IF ( AUTO ) THEN
C             fire off special script to invoke spooler server
              CALL DAXPLOT(PLTFIL(1:NLEN(PLTFIL)),PLOMOD,PLTCOP,ST)
          ENDIF
      END IF
C
      END


C     ******************************************************************
C
C     Extracted from polyfil.f
C
C     ******************************************************************



      SUBROUTINE POLDRW(OK)
C     =======================
C1    VARTYPE            L
C1    IOSTAT             O
C
C2    This routine will draw a portion of the profile
C2    which is stored in the scratch file
C2    OK will be set false if the profile
C2    is partially in the viewport and
C2    the fill will be abandoned
C
      include 'include/gpr.ins.inc'
      include 'include/hdata.inc'
      include 'include/entity.inc'
      include 'include/daxcolor.inc'
C
      LOGICAL ERASE,FIRST,OK
      REAL BUFF1(6),BUFF2(6),X,Y,X1,Y1,X2,Y2,BUFF3(6)
      REAL AX1,AY1,AX2,AY2,PX,PY
      INTEGER*2 TMIP,ENT,ENTP,SX,SY,FX,FY,XP1,YP1,XP2,YP2
      INTEGER*2 NX(6000),NY(6000),NOE,LASTE,OX,OY
      INTEGER*2 AXP1,AYP1,AXP2,AYP2
      INTEGER*4 TFONT,ST,I,J,TOTAL
      LOGICAL UNOBSCURED,REDARC,CLOCK
C
      REDARC = .FALSE.
      FIRST = .TRUE.
      NOE = 0
C     sort start and end points
 
C
C     set ok true
      OK = .TRUE.
C      IF(TOTAL .NE. 0) THEN
C        OK =.FALSE.
C        RETURN
C      ENDIF
C     get data
      DO 10 I=1,NO
          READ(UNIT=HUNIT,REC=I,ERR=99) TMIP,ENT,X,Y,
     +        BUFF1(1),BUFF1(2),BUFF1(4),BUFF1(5),BUFF1(6),
     1        BUFF2(1),BUFF2(2),BUFF2(4),BUFF2(5),BUFF2(6)
 
 
C         higligh the profile
C
C         draw the entity
          IF(ENT.EQ.LINE) THEN
C
              IF(FIRST) THEN
                  CALL SORTPR(I,PX,PY,CLOCK)
                  CALL WO2SC(PX,PY,X1,Y1)
C                 Ensure that the screen limits are okay 
                  IF (X1 .GT. 32000.0) X1 = 32000.0
                  IF (X1 .LT. -32000.0) X1 = -32000.0
                  IF (Y1 .GT. 32000.0) Y1 = 32000.0
                  IF (Y1 .LT. -32000.0) Y1 = -32000.0    
C         
                  OX = X1
                  OY = Y1
                  SX = OX
                  SY = OY
                  FIRST = .FALSE.
              ENDIF
              CALL WO2SC(BUFF1(1),BUFF1(2),X1,Y1)
              CALL WO2SC(BUFF1(4),BUFF1(5),X2,Y2)
C             Ensure that the screen limits are okay 
              IF (X1 .GT. 32000.0) X1 = 32000.0
              IF (X1 .LT. -32000.0) X1 = -32000.0
              IF (Y1 .GT. 32000.0) Y1 = 32000.0
              IF (Y1 .LT. -32000.0) Y1 = -32000.0    
C         
C             Ensure that the screen limits are okay 
              IF (X2 .GT. 32000.0) X2 = 32000.0
              IF (X2 .LT. -32000.0) X2 = -32000.0
              IF (Y2 .GT. 32000.0) Y2 = 32000.0
              IF (Y2 .LT. -32000.0) Y2 = -32000.0    
C         

              XP1 = X1
              YP1 = Y1
              XP2 = X2
              YP2 = Y2
C
              NOE = NOE+1
              NX(NOE) = OX
              NY(NOE) = OY
C             set start of current ent
              OX = XP2
              OY = YP2
              IF(OX.EQ.SX.AND.OY.EQ.SY) THEN
C                 complete boundary found
                  NOE = NOE + 1
                  NX(NOE) = OX
                  NY(NOE) = OY
                  NOE = NOE + 1
                  NX(NOE) = NX(1)
                  NY(NOE) = NY(1)
                  FIRST = .TRUE.
              ENDIF
C
          ELSE IF(ENT.EQ.ARC) THEN
C
              IF(FIRST) THEN
                  CALL SORTPR(I,PX,PY,CLOCK)
                  CALL WO2SC(PX,PY,X1,Y1)
C                 Ensure that the screen limits are okay 
                  IF (X1 .GT. 32000.0) X1 = 32000.0
                  IF (X1 .LT. -32000.0) X1 = -32000.0
                  IF (Y1 .GT. 32000.0) Y1 = 32000.0
                  IF (Y1 .LT. -32000.0) Y1 = -32000.0    
C         
                  OX = X1
                  OY = Y1
                  SX = OX
                  SY = OY
                  FIRST = .FALSE.
              ENDIF
C             start and end points assume cc direction
              X1=BUFF1(1)+BUFF1(4)*COS(BUFF1(5))
              Y1=BUFF1(2)+BUFF1(4)*SIN(BUFF1(5))
              X2=BUFF1(1)+BUFF1(4)*COS(BUFF1(6))
              Y2=BUFF1(2)+BUFF1(4)*SIN(BUFF1(6))
C
              CALL WO2SC(X1,Y1,AX1,AY1)
              CALL WO2SC(X2,Y2,AX2,AY2)
C
C             Ensure that the screen limits are okay 
              IF (AX1 .GT. 32000.0) AX1 = 32000.0
              IF (AX1 .LT. -32000.0) AX1 = -32000.0
              IF (AY1 .GT. 32000.0) AY1 = 32000.0
              IF (AY1 .LT. -32000.0) AY1 = -32000.0    
C         
C             Ensure that the screen limits are okay 
              IF (AX2 .GT. 32000.0) AX2 = 32000.0
              IF (AX2 .LT. -32000.0) AX2 = -32000.0
              IF (AY2 .GT. 32000.0) AY2 = 32000.0
              IF (AY2 .LT. -32000.0) AY2 = -32000.0    
C         
              AXP1 = AX1
              AYP1 = AY1
              AXP2 = AX2
              AYP2 = AY2
C
              IF((OX.EQ.AXP1).AND.(OY.EQ.AYP1)) THEN
                  CLOCK = .TRUE.
                  OX = AXP2
                  OY = AYP2
               CALL POLARC(BUFF1(1),BUFF1(2),BUFF1(4),BUFF1(5),BUFF1(6),
     +                    CLOCK,NX,NY,NOE)
              ELSE
                  CLOCK = .FALSE.
                  OX = AXP1
                  OY = AYP1
              CALL POLARC(BUFF1(1),BUFF1(2),BUFF1(4),BUFF1(5),BUFF1(6),
     +                    CLOCK,NX,NY,NOE)
              ENDIF
              IF(OX.EQ.SX.AND.OY.EQ.SY) THEN
C                 complete boundary found
                  NOE = NOE + 1
                  NX(NOE) = OX
                  NY(NOE) = OY
                  NOE = NOE + 1
                  NX(NOE) = NX(1)
                  NY(NOE) = NY(1)
                  FIRST = .TRUE.
              ENDIF
          ENDIF
 20   CONTINUE
 10   CONTINUE

C
C     fill the polygon
C
      CALL ROPXOR()
      CALL HFONT(.TRUE.)
      CALL SETAPN(COLFOR)
      CALL GPR_$SET_CLIPPING_ACTIVE(.TRUE.,ST)
      UNOBSCURED = GPR_$ACQUIRE_DISPLAY(ST)
      CALL GPR_$START_PGON(NX(1),NY(1),ST)
      CALL GPR_$PGON_POLYLINE(NX,NY,NOE,ST)
      CALL GPR_$CLOSE_FILL_PGON(ST)
      CALL GPR_$SET_CLIPPING_ACTIVE(.FALSE.,ST)
      CALL GPR_$RELEASE_DISPLAY(ST)
C
99    CONTINUE
C     reset all factors
      CALL HFONT(.FALSE.)
      CALL ROPREP()
 
      END


      FUNCTION INT2(I4)
C     =================
C1    VARYPE        I4    
C1    IOSTAT        I    
C
C2    Converts I4 to I2
C2  
C2  
C2    Arguments:-
C2  
C2    I4      ->         Integer*4 argument (MUST BE )
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      INTEGER*2 INT2
      INTEGER*4 I4

      INT2 = I4

      RETURN

      END


      FUNCTION INT4(I2)
C     =================
C1    VARYPE        I4    
C1    IOSTAT        I    
C
C2    Converts I2 to I4
C2  
C2  
C2    Arguments:-
C2  
C2    I2      ->         Integer*2 argument (MUST BE )
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      INTEGER*4 INT4
      INTEGER*2 I2

      INT4 = I2

      RETURN

      END


