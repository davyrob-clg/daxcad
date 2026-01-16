C
C     @(#)  412.1 date 6/11/92 plot1.f 
C
C
C     Filename    : plot1.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:42:33
C     Last change : 92/06/11 14:37:52
C
C     Copyright : Practical Technology Limited  
C     File :- plot1.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE CHGPEN(PN)
C     =====================
C2
C     write to internalfile "Select Pen No I " command line
C
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
      INTEGER*4 PN
      INTEGER*2 PN2
      CHARACTER*80 OLINE
      EXTERNAL NEWPEB
C
C            1 2 3 4 5 6 7 8 9 0  1 2  3  4 5 6  7 8
      GOTO ( 1,1,1,2,1,1,3,1,1,4,10,1,10,10,1,5,10,1) PLOMOD
  4   CONTINUE
         CALL NEWPEN(PN)
      GOTO 10
  2   CONTINUE
         CALL NEWPEB(PN)
      GOTO 10
  3   CONTINUE
         PN2=PN
         CALL NEWPEC(PN2)
      GOTO 10
  1   CONTINUE
         IF ( PENSPC ) THEN
            IF (PENSD(PN,1).NE.0 ) THEN
               WRITE(OLINE,'(2(A,I4),A)')
     +         'VS,',PENSD(PN,1),',',PN,';'
C              actually write out the line to Plot File
               CALL OUTOLL(.FALSE.,OLINE)
            END IF
            IF (PENSD(PN,2).NE.0 ) THEN
               WRITE(OLINE,'(2(A,I4),A)')
     +         'AS,',PENSD(PN,2),',',PN,';'
C              actually write out the line to Plot File
               CALL OUTOLL(.FALSE.,OLINE)
            END IF
            IF (PENSD(PN,3).NE.0 ) THEN
               WRITE(OLINE,'(2(A,I4),A)')
     +         'FS,',PENSD(PN,3),',',PN,';'
C              actually write out the line to Plot File
               CALL OUTOLL(.FALSE.,OLINE)
            END IF
         END IF
C
         WRITE(OLINE,'(A,I4,A)') 'SP',PN,';'
C        actually write out the line to Plot File
         CALL OUTOLL(.FALSE.,OLINE)
      GOTO 10
 5    CONTINUE
         WRITE(OLINE,'(A,I4)') 'P',PN
C        actually write out the line to Plot File
         CALL OUTOLL(.FALSE.,OLINE)
C
 10   CONTINUE
C
      END
C
C       @(#)  256.1 date 4/19/90 chgthk.ftn Daxcad revision 1.8
      SUBROUTINE CHGTHK(PN)
C     =====================
C2
CC    This routine has been added to handle
C     plotters with width capability e.g. gerber,postscipt
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
      include 'include/lfont.inc'
      include 'include/gerber.inc'
      include 'include/ndata.inc'
      REAL PT
      INTEGER*4 PN,THK,LTAB(0:4),END,NLEN1
      CHARACTER*80 OLINE
      EXTERNAL NLEN1
      DATA LTAB/0,0,1,1,2/
C
C
C
C     PN is used to indicate back to the control code
C     that the plotter model has the change thick
C     capability.  i.e. PN=0 implies capability
C                       PN unchanged implies no capabilty
 
C            1  2  3  4  5  6  7  8  9  0 1  2  3 4  5  6 7   8
      GOTO (10,10,10,10,10,10,10,10,10,10,1,10, 3,2,10,10,10,10) PLOMOD
  1   CONTINUE
C******************************************************
C                                       G e r b e r   *
C******************************************************
C        WRITE(10,*)'PN,CURTL,IMBUFF(12)',PN,CURTL,IMBUFF(12)
        IF (PN.NE.CURTL ) THEN
          IF ( PN.LT.10.OR.PN.GT.99 ) THEN
            IF (MOD(OPT/4,2).EQ.1) THEN
               WRITE(UNIT=OLINE,FMT='(A,I2.2,A)')
     +         'D',INT(LTHKR(2,127)),'*'
            ELSE
               WRITE(UNIT=OLINE,FMT='(A,I2.2,A)')
     +                   'G54D',INT(LTHKR(2,127)),'*'
            END IF
          ELSE
            IF (MOD(OPT/4,2).EQ.1) THEN
              WRITE(UNIT=OLINE,FMT='(A,I2.2,A)') 'D',PN,'*'
            ELSE
              WRITE(UNIT=OLINE,FMT='(A,I2.2,A)')
     +                'G54D',PN,'*'
            END IF
          END IF
          CALL OUTOLL(.FALSE.,OLINE)
        END IF
        PN=0
      GOTO 10
 
  2   CONTINUE
C******************************************************
C                               P o s t s c r i p t   *
C******************************************************
        IF (PN.NE.CURTL ) THEN
          THK=PLTHKI(PN)
C        WRITE(10,*) 'IMBUFF(12),CURTL',IMBUFF(12),CURTL,
C     +            LTHKR(1,MOD(THK,64)),MOD(THK,64),THK
          PT=PAPSCL*LTHKR(1,MOD(THK,64))*FACT
C      write(10,*) 'PAPSCL:',PAPSCL,PAPTOW,PT,LTHKR(1,MOD(THK,64))*FACT
          END=LTAB(MOD(THK,8192)/1024)
          WRITE(UNIT=OLINE,FMT='(F9.2,A,I2,A)')
     +    PT,'_setlinewidth_',END,'_setlinecap'
          CALL CRUNCH(OLINE)
          CALL STRIPO(OLINE,'_',' ')
          WRITE(UNIT=PLTUNT,FMT='(A)')OLINE(1:NLEN1(OLINE))
        END IF
        PN=0
      GOTO 10
  3   CONTINUE
C******************************************************
C       M e t r i c  O u t p u t  f o r  A d v e n t  *
C******************************************************
       PN=0
 
 10   CONTINUE
      CURTL=IMBUFF(12)
C
      END
C
C       @(#)  256.1 date 6/7/90 filend.ftn Daxcad revision 1.8
      SUBROUTINE FILEND()
C     ===================
C1    no arguments.
C2    FILEND output end block to plotfile.
C
C     write to plot file a command string to reset plotter
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
      include 'include/params.inc'
C
      INTEGER*2 I2,TSIZE
      EXTERNAL OUTOLL,PLOTB
C           1 2 3 4 5 6 7 8 9 0 1 2 3 4 5  6  7 8
      GOTO (1,1,1,2,7,1,4,1,1,5,6,7,8,9,1,10,11,12) PLOMOD
 1    CONTINUE
C       HP7475 7585B
        CALL OUTOLL(.TRUE.,'SP0;')
        RETURN
 2    CONTINUE
C       BENSON 1625-S
        CALL PLOTB(PLXMAX+2.0,PLYMIN,999)
      RETURN
 4    CONTINUE
C       Calcomp 104* series
        I2=999
        CALL PLOTC(PLXMAX+2.0,PLYMIN,I2)
      RETURN
 5    CONTINUE
        CALL CHGPEN(1)
        CALL PLOT(PLXMAX+2.0,PLYMIN,999)
      RETURN
 6    CONTINUE
        CALL OUTOLL(.FALSE.,'X0Y0D02*')
        CALL OUTOLL(.FALSE.,'M02*')
      RETURN
 7    CONTINUE
C       HP7586
        CBUFF = 'AH;'
        TSIZE=INDEX('A0 A1 A2 A3 A4 ',PLTSHT(:2))
        IF ( TSIZE.GT.0 ) THEN
          TSIZE=1+((TSIZE-1)/3)
        END IF
        IF (TSIZE.LT.3) CBUFF = 'AF;'
        CALL OUTOLL(.TRUE.,CBUFF)
        RETURN
C
 8    CONTINUE
 
      RETURN
 
 9    CONTINUE
C     finish off by ejecting the page.
      CALL OUTOLL(.FALSE.,'stroke')
      CALL OUTOLL(.FALSE.,'showpage')
CGCU
C Don't output ctrl-D: it breaks the PostScript rules
C      WRITE(PLTUNT,FMT='(A)') CHAR(4)
CGCU 
      RETURN
 
 10   CONTINUE
C        Select pen 0 to return current pen to holder for houston
         WRITE(CBUFF,'(A)') 'P0'
C        actually write out the line to Plot File
         CALL OUTOLL(.FALSE.,CBUFF)
      RETURN
C
 11   CONTINUE
        CALL OUTOLL(.FALSE.,')')
        CALL OUTOLL(.FALSE.,'>')
      RETURN
 12   CONTINUE
         CALL OUTOLL(.FALSE.,'PG;')
      RETURN
C
      END
C
C     -------------------------------------------------------------
C
      SUBROUTINE FILHED(TXP1,TYP1,TXP2,TYP2)
C     ======================================
C     *******************************************************
C     FILHED()
C     output header block to plotfile. consists of xon-xoff
C     initiator , scale command and resets p1 & p2 if needed.
C     *******************************************************
      include 'include/pendat.inc'
      include 'include/product.inc'
      include 'include/params.inc'
      include 'include/gerber.inc'
      include 'include/library.inc'
C
      CHARACTER  OLINE*80,ESC*1,TEMP*80
      INTEGER*2 I2
      INTEGER*2 ZERO2
      INTEGER*4 TXP1,TYP1,TXP2,TYP2,ST,NLEN,GETFD
      REAL SCALE
      REAL PSFACT
      INTRINSIC CHAR
      INTEGER NUNIT,IR1,IR2,IR3,IR4
      LOGICAL EX,OK
      REAL SCALEX,SCALEY,R1,R2,R3,R4
      EXTERNAL CRUNCH,OUTOLL,PLOTSB,NLEN
      EXTERNAL STRMFL
      EXTERNAL GETFD
C     PLTVER variable ja
      INTEGER*4 PLTVER
      CHARACTER*24 FDATE
C 
C Metric Plot version number ja
      PARAMETER (PLTVER=1)
C
C     set the ESC character for use in output to plotter
C            1 2 3 4 5 6 7 8 9 0 1  2 3 4  5 6 7 8
      GOTO ( 1,3,1,2,1,3,4,3,1,5,6,12,7,8,10,1,9,1 ) PLOMOD
 1    CONTINUE
C        HP 7475,Hitachi,Gould Plotter,HP 7550 plotters
         ESC=CHAR(27)
C        now write to plot file the XON/XOFF command line
CAPOLLO|SUN
         WRITE(OLINE,'(5A)')
     +     'IN;',ESC,'.I81;;17:',ESC,'.N;19:'
         CALL OUTOLL(.FALSE.,OLINE)
CAPOLLO|SUN
C        now write to plot file the plotter window extents
         WRITE(OLINE,'(4(A,I7),A)' )
     +    'IP',TXP1,',',TYP1,',',TXP2,',',TYP2,';AP3;PA;'
         CALL OUTOLL(.FALSE.,OLINE)
      RETURN
 3       CONTINUE
C        NP 7585B 7580B
         ESC=CHAR(27)
C        now write to plot file the XON/XOFF command line
CAPOLLO|SUN
         WRITE(OLINE,'(5A) ')
     +     'IN;',ESC,'.I81;;17:',ESC,'.N;19:'
         CALL OUTOLL(.FALSE.,OLINE)
CAPOLLO|SUN
C        now write to plot file the plotter window extents
         WRITE(OLINE,'(4(A,I7),A)')
     +    'IP',TXP1,',',TYP1,',',TXP2,',',TYP2,';AP3;PA;NR;'
         CALL OUTOLL(.FALSE.,OLINE)
         RETURN
 12      CONTINUE
C        NP 7585B 7580B
         ESC=CHAR(27)
C        now write to plot file the XON/XOFF command line
CAPOLLO|SUN
         WRITE(OLINE,'(5A) ')
     +     'IN;',ESC,'.I81;;17:',ESC,'.N;19:'
         CALL OUTOLL(.FALSE.,OLINE)
CAPOLLO|SUN
C        now write to plot file the plotter window extents
         WRITE(OLINE,'(4(A,I7),A)')
     +    'IP',TXP1,',',TYP1,',',TXP2,',',TYP2,';AP3;PA;'
         CALL OUTOLL(.FALSE.,OLINE)
         RETURN
 2    CONTINUE
C        Benson 1625-S 1645,
CAPOLLO|SUN
C         CALL PLOTSB(0,0,1)
CAPOLLO|SUN
CIBM|PC386
         CALL PLOTSB(0,0,PLTUNT)
CIBM|PC386
        RETURN
 4    CONTINUE
C        Calcomp 104* series
         I2=PLTUNT
         ZERO2=0
         CALL PLOTSC(ZERO2,ZERO2,I2)
         CALL FONTC(2.0)
      RETURN
 5    CONTINUE
C        5342,1302,1313 plotter
CAPOLLO|SUN
         CALL PLOTS(0,0,1)
CAPOLLO|SUN
CIBM|PC386
C         CALL PLOTS(0,0,PLTUNT)
CIBM|PC386
        RETURN
 6    CONTINUE
C*****************   g e r b e r *************
         CALL OUTOLL(.FALSE.,'*')
         CURTL=0
         RETURN
 7    CONTINUE
C        advent hole, metric format section updated ja
C***************** Advent metric format **************
C
C       Write paper size and raster scale for raster backcloth
C       Format is:
C               V <version number of plot file> (new at V 1)
C               PaperW PaperH                   in mm
C               ViewportX ViewPortY             vector viewport offset in mm
C               ScalingFactor                   to apply to plot (new at V 1)
C               Name X Y Raster-Scale-Factor    raster offsets in mm and scale
C                       .
C                       .
C                       .
C               <blank line>
C               Annotation
C                       .
C               <blank line>
C       Version number
         WRITE(PLTUNT,'(A,I3)') 'V ',PLTVER
C paper size
C
         WRITE(OLINE,'(F15.5,A,F15.5)') PVXMAX-PVXMIN,',',PVYMAX-PVYMIN
         CALL OUTOLL(.FALSE.,OLINE)
C viewport
         WRITE(OLINE,'(F15.5,A,F15.5)') VPORGX,',',VPORGY
         CALL OUTOLL(.FALSE.,OLINE)
C plot scale
         WRITE(OLINE,'(F15.5)') PLTSCL/DRWSCL
         CALL OUTOLL(.FALSE.,OLINE)
C
C raster records.
C
         CALL OSPLOT()
C
C blank line
C
         CALL OUTOLL(.FALSE.,' ')
C Annotation
C         DO 77 I=1,NANNOT
C           WRITE(PLTUNT,'(A)') ANNOT(I)(1:NLEN(ANNOT(I)))
C 77      CONTINUE
C
C blank line
C
         CALL OUTOLL(.FALSE.,' ')
         RETURN
 8    CONTINUE
C        ****************  postscript plot output  ****************
C	 initialise last text angle etc.
	 LPTX3=0.0
	 LPTX4=0.0
	 LPTSLT=0.0

         WRITE(PLTUNT,FMT='(A)') '%!PS-Adobe-1.0'
         WRITE(PLTUNT,FMT='(3A,F4.2)') '%%Creator: ',
     +         PRODUCT(1:NLEN(PRODUCT)),' V',ROOTRV
         WRITE(PLTUNT,FMT='(2A)') '%%CreationDate: ',FDATE()
         WRITE(PLTUNT,FMT='(2A)') '%%Title: Plot from ',
     +                             DRGNAM(1:NLEN(DRGNAM))
	 PSFACT=(72/25.4)*(PLTSCL/DRWSCL)
C Calculate bounding box
         ILLX=PVXMIN*PSFACT
	 ILLY=PVYMIN*PSFACT
         IURX=PVXMAX*PSFACT
	 IURY=PVYMAX*PSFACT
C PostScript bounding box is at bottom left of the paper
	 IXMARGIN=18
	 IYMARGIN=18
	 IBBLLX=IXMARGIN
	 IBBLLY=IYMARGIN
	 IBBURX=IURX-ILLX+IXMARGIN
	 IBBURY=IURY-ILLY+IYMARGIN

	 WRITE(PLTUNT,FMT='(A,4I7)') '%%BoundingBox: ',
     +		IBBLLX,IBBLLY,IBBURX,IBBURY
         WRITE(PLTUNT,FMT='(A)') '%%EndComments'

         WRITE(PLTUNT,FMT='(A)') '% Clip to Bounding Box'
	 WRITE(PLTUNT,FMT='(2I7,A)') IBBLLX,IBBLLY,' moveto'
	 WRITE(PLTUNT,FMT='(2I7,A)') IBBURX,IBBLLY,' lineto'
	 WRITE(PLTUNT,FMT='(2I7,A)') IBBURX,IBBURY,' lineto'
	 WRITE(PLTUNT,FMT='(2I7,A)') IBBLLX,IBBURY,' lineto'
	 WRITE(PLTUNT,FMT='(A)') 'closepath clip newpath'
         WRITE(PLTUNT,FMT='(2I7,A)')
     +		IXMARGIN-ILLX,IYMARGIN-ILLY,' translate'

C Flush the Fortran I/O stream prior to using C I/O
         CALL FLUSH(PLTUNT)
	 TEMP=LIBRARY(1:NLEN(LIBRARY))//'/postscript.prolog'
	 CALL STRMFL(GETFD(PLTUNT),TEMP(1:NLEN(TEMP)))
         WRITE(PLTUNT,FMT='(A)') '%%EndProlog'
	 WRITE(PLTUNT,FMT='(2F8.4,A)') PSFACT,PSFACT,' scale'
         WRITE(PLTUNT,FMT='(A)') '%%StartRaster'
	 CALL PSBACK(PLTUNT)
         WRITE(PLTUNT,FMT='(A)') '%%EndRaster'
         WRITE(PLTUNT,FMT='(A)') '0 setlinewidth'
         WRITE(PLTUNT,FMT='(A)') 'newpath'
         RETURN
C
 9    CONTINUE
C        *****************   I n t e r l e a f *************
         WRITE(PLTUNT,FMT='(A)')'<!Document,'
         WRITE(PLTUNT,FMT='(A)')'    Final Output Device = "ps",'
         WRITE(PLTUNT,FMT='(A)')'    Default Printer = "sx2">'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Font Definitions,'
         WRITE(PLTUNT,FMT='(A)')'    F20 = Times 10>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Page,'
         WRITE(PLTUNT,FMT='(A)')'    Height =           11.692 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Width =             8.267 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Top Margin =        0.984 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Bottom Margin =     0.984 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Left Margin =       0.984 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Right Margin =      0.984 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Starting Page # =        Inherit,'
         WRITE(PLTUNT,FMT='(A)')'    Consecutive Hyphens =    3,'
         WRITE(PLTUNT,FMT='(A)')'    Revision Bar Placement = Left,'
         WRITE(PLTUNT,FMT='(A)')'    Feathering =             off>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Autonumber Stream, List, 1>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Class, micro:caption,'
         WRITE(PLTUNT,FMT='(A)')'    Alignment = Left,'
         WRITE(PLTUNT,FMT='(A)')'    Font = F20>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Class, para,'
         WRITE(PLTUNT,FMT='(A)')'    Font = F20>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<!Master Frame,'
         WRITE(PLTUNT,FMT='(A)')'    Name = "At Anchor",'
         WRITE(PLTUNT,FMT='(A)')'    Placement = At Anchor,'
         WRITE(PLTUNT,FMT='(A)')'    Width =0.41 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Height =0.137 inches,'
         WRITE(PLTUNT,FMT='(A)')
     C        '    Vertical Alignment =0.03 inches,'
         WRITE(PLTUNT,FMT='(A)')'    Diagram ='
         WRITE(PLTUNT,FMT='(A)')'V4,'
         WRITE(PLTUNT,FMT='(A)')'(g9,32767,0)>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<para>'
         WRITE(PLTUNT,FMT='(A)')' '
         WRITE(PLTUNT,FMT='(A)')'<Frame,'
         WRITE(PLTUNT,FMT='(A)')
     C        '    Name ="At Anchor",'
         WRITE(PLTUNT,FMT='(A)')'   Placement = At Anchor,'
         WRITE(PLTUNT,FMT='(A)')'   Width =Contents,'
         WRITE(PLTUNT,FMT='(A)')'   Height =Contents,'
         WRITE(PLTUNT,FMT='(A)')
     C        '   Vertical Alignment =         Center,'
         WRITE(PLTUNT,FMT='(A)')'   Diagram ='
         WRITE(PLTUNT,FMT='(A)')'V5,'
         WRITE(PLTUNT,FMT='(A)')'(g9,1,0'
         CURTL=0
      RETURN

 10     CONTINUE
        OLINE = 'SCALE.PCF'
        SCALEX = 1.0
        SCALEY = 1.0
        INQUIRE(FILE = OLINE,EXIST = EX)
        IF(.NOT.EX) THEN
           OLINE =LIBRARY(1:NLEN(LIBRARY))//'/scale.pcf'
           INQUIRE(FILE = OLINE,EXIST = EX)
        END IF
        IF(EX) THEN
           CALL FINDU2(NUNIT,OLINE,OK)
           IF ( .NOT. OK ) THEN
             CALL DEPRNT(287)
             RETURN           
           END IF
C            Open a sequential access file using default values.
           OPEN(UNIT = NUNIT,FILE = OLINE,STATUS='OLD',
     +     ERR=99)
           READ(NUNIT,*,ERR = 99) SCALEX,SCALEY
           CLOSE(NUNIT)
        END IF
  99     CONTINUE
C        NP 7585B 7580B
         ESC=CHAR(27)
C        now write to plot file the XON/XOFF command line
CAPOLLO|SUN
         WRITE(OLINE,'(5A) ')
     +     'IN;',ESC,'.I81;;17:',ESC,'.N;19:'
         CALL OUTOLL(.FALSE.,OLINE)
CAPOLLO|SUN
C        now write to plot file the plotter window extents
         
         R1 = TXP1/SCALEX
         R2 = TYP1/SCALEY
         R3 = TXP2/SCALEX
         R4 = TYP2/SCALEY
         IR1 = R1
         IR2 = R2
         IR3 = R3
         IR4 = R4
         WRITE(OLINE,'(4(A,I7),A)' )
     +    'IP',TXP1,',',TYP1,',',TXP2,',',TYP2,';AP3;PA;NR;'
         CALL OUTOLL(.FALSE.,OLINE)
         WRITE(OLINE,'(4(A,I7),A)')
     +    'SC',IR1,',',IR3,',',
     +         IR2,',',IR4,';'
         CALL OUTOLL(.FALSE.,OLINE)
         RETURN
           

      END
C
C     --------------------------------------------------------------------------
C
C       @(#)  256.1 date 4/19/90 hararc.ftn Daxcad revision 1.8
      SUBROUTINE HARARC(X1,X2,X4,X5,X6)
C     =================================
C
C     Draw a hardware arc.
C 
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
 
      REAL X1,X2,X4,X5,X6,AXP1,AYP1,UU,P,PRAD,XT1,YT1,DEG,HARLIM,
     +     ALPH,AP,CON,TRS(3,3),NX,NY,RADS,RAD,PI,TWOPI,MIDANG,
     +     XT5,XT6,AXP2,AYP2,DISTXY,DEL1,DEL2,XY(2,6)
      CHARACTER*140 OLINE
      INTEGER*4 I,COUNT,NLEN1
      LOGICAL OK,SAME
      INTRINSIC NINT,COS,SIN,ATAN,ABS,SQRT,INT,DBLE,REAL
      EXTERNAL WO2PL,WTOPVX,OUTOLL,ROTP2D,NEWXY,DISTXY,NLEN1,SAME
C
      TWOPI=PI(2.0)
      HARLIM=LIM
      IF (PLOMOD.EQ.17) HARLIM = HARLIM / 10
C
      CALL WO2PL(X1,X2,XT1,YT1)
      CALL WTOPVX(X4,PRAD)
C 
      IF ( PRAD .LT. HARLIM ) RETURN
C
      IF ( ABS(X6-X5).LT.TWOPI-0.01) THEN
        XT5=MOD(X5+RAD(TEXANG+360.0),TWOPI)
        XT6=MOD(X6+RAD(TEXANG+360.0),TWOPI)
      ELSE
        XT5=X5
        XT6=X6
      END IF
C
      AXP1=XT1+PRAD*COS(XT5)
      AYP1=YT1+PRAD*SIN(XT5)
C
      AXP2=XT1+PRAD*COS(XT6)
      AYP2=YT1+PRAD*SIN(XT6)
C
      AP=ABS(XT6-XT5)
      IF ( XT6.LT.XT5 ) AP=TWOPI-AP
C     Convert to Degrees
      AP=DEG(AP)
C
      IF ( DISTXY(AXP,AYP,AXP2,AYP2).LT.
     +     DISTXY(AXP,AYP,AXP1,AYP1)     ) THEN
        AXP1=AXP2
        AYP1=AYP2
        AP  =-AP
      END IF
C           1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
      GOTO (1,1,1,1,1,1,1,1,1,1,2,1,1,3,1,4,5,6) PLOMOD
C
 1    CONTINUE
        ALPH=SQRT((3.6)/PRAD)
        ALPH=DEG(ALPH)
        ALPH=MAX(1.0,ALPH)
C  ***************  HP 7475,7585 ********************
        IF (AXP.EQ.AXP1.AND.AYP.EQ.AYP1) THEN
          WRITE (OLINE,'(2(A,I12),2(A,I4),A)' )
     +      'AA',NINT(XT1),',',NINT(YT1),
     1       ',',NINT(AP),',',NINT(ALPH),';'
        ELSE
          WRITE (OLINE,'(4(A,I12),A,F8.2,A,I3,A)' )
     +    'PU',NINT(AXP1),',',NINT(AYP1),
     1    ';PD;AA',NINT(XT1),',',NINT(YT1),
     2    ',',AP,',',NINT(ALPH),';'
        END IF
C
        CALL OUTOLL(.FALSE.,OLINE)
C
        GOTO 100
C
 2      CONTINUE
C   ************  GERBER PLOTTER *********************
        IF ( IMBUFF(12).NE.0 ) THEN
            WRITE (OLINE,'(2(A,I12),A)' )
     +      'X',NINT(XT1),'Y',NINT(YT1),'D03*'
        ELSE
          IF (.NOT.(AXP.EQ.AXP1.AND.AYP.EQ.AYP1) ) THEN
C           need to move
            WRITE (OLINE,'(2(A,I12),A)' )
     +      'X',NINT(AXP1),'Y',NINT(AYP1),'D02*'
            CALL OUTOLL(.FALSE.,OLINE)
          END IF
          WRITE (OLINE,'(4(A,SP,I12),A)' )
     +    'G75X',NINT(AXP2),'Y',NINT(AYP2),
     +    'I',NINT(XT1),'J',NINT(YT1),'*'
        END IF
        CALL OUTOLL(.FALSE.,OLINE)
C
      GOTO 100
 
 3    CONTINUE
C
        XT5=DEG(XT5)
        XT6=DEG(XT6)
        IF (.NOT.(AXP.EQ.AXP1.AND.AYP.EQ.AYP1)) THEN
            AXP1=XT1+PRAD*COS(RAD(XT5))
            AYP1=YT1+PRAD*SIN(RAD(XT5))
            WRITE (OLINE,'(2(F9.1,A))' )
     +      AXP1,'_',AYP1,'_moveto'
            CALL CRUNCH(OLINE)
            CALL STRIPO(OLINE,'_',' ')
            WRITE (PLTUNT,'(A) ')
     +      OLINE(1:NLEN1(OLINE))
        END IF
C
        WRITE (OLINE,'(5(F15.5,A))' )
     +  XT1,'_',YT1,'_',PRAD,'_',XT5,'_',XT6,'_arc_stroke'
        CALL CRUNCH(OLINE)
        CALL STRIPO(OLINE,'_',' ')
        WRITE (PLTUNT,'(A) ')
     +  OLINE(1:NLEN1(OLINE))
 
      GOTO 100
 
 4    CONTINUE
          AXP1=XT1+PRAD*COS(RAD(XT5))
          AYP1=YT1+PRAD*SIN(RAD(XT5))
C     ************************************ Houston DMP
          WRITE (OLINE,'(5(A,I12) )')
     +      'U ',NINT(AXP1),',',NINT(AYP1),
     1      'CA ',NINT(XT1),',',NINT(YT1),
     2      ',',NINT(AP)
          CALL OUTOLL(.FALSE.,OLINE)
C     GOTO 100
C
 5    CONTINUE
C        Writing Interleaf.
C        First two points - a tangent line
C        coming into the arc.
C        NOTE: The angle is already in radians
C              This seems to be inconsistent with the rest of the routine.
         IF (SAME(MOD((XT6-XT5),TWOPI),0.0)) THEN
C           For a full circle we use adifferent method.
C           We draw an elipse.
            XY(1,1)=XT1-PRAD
            XY(2,1)=YT1-PRAD
            XY(1,2)=XT1+PRAD
            XY(2,2)=YT1-PRAD
            XY(1,3)=XT1-PRAD
            XY(2,3)=YT1+PRAD
            DO 58 I=1,3
               XY(2,I) = -XY(2,I)
 58         CONTINUE
            WRITE (OLINE,'(12(A,F8.2),A)' )
     +            '(e8,1,0,',XY(1,1),',',XY(2,1),
     +                   ',',XY(1,2),',',XY(2,2),
     +                   ',',XY(1,3),',',XY(2,3),',0,127,5,7,0,2,0)'
         ELSE
            DEL1=PRAD*COS(XT5)
            DEL2=PRAD*SIN(XT5)
            XY(1,1)=XT1+DEL1-DEL2
            XY(2,1)=YT1+DEL1+DEL2
            XY(1,2)=XT1+DEL1
            XY(2,2)=YT1+DEL2
C           Middle point - a point on the arc.
            MIDANG=(XT5+XT6)/2.0
            IF (XT6.LT.XT5) THEN
C               Which arc are we looking for
                MIDANG = MIDANG + PI(1.0)
            ENDIF
            DEL1=PRAD*SIN(MIDANG)
            DEL2=PRAD*COS(MIDANG)
            XY(1,3)=XT1+DEL2
            XY(2,3)=YT1+DEL1
C           End points - a tangent line
C           coming out of the arc.
            DEL1=PRAD*SIN(XT6)
            DEL2=PRAD*COS(XT6)
            XY(1,4)=XT1+DEL2
            XY(2,4)=YT1+DEL1
            XY(1,5)=XT1+DEL1+DEL2
            XY(2,5)=YT1+DEL1-DEL2
C           Last point - The end point of the arc.
            XY(1,6)=XY(1,4)
            XY(2,6)=XY(2,4)
C           Must negate all Y values for interleaf.
            DO 59 I=1,6
               XY(2,I) = -XY(2,I)
 59         CONTINUE
            WRITE (OLINE,'(12(A,F8.2),A)' )
     +           '(a8,1,0,1,',XY(1,1),',',XY(2,1),
     +                    ',',XY(1,2),',',XY(2,2),
     +                    ',',XY(1,3),',',XY(2,3),
     +                    ',',XY(1,4),',',XY(2,4),
     +                    ',',XY(1,5),',',XY(2,5),
     +                    ',',XY(1,6),',',XY(2,6),',5,7,127,7,0,2,0)'
         ENDIF
         CALL OUTOLL(.FALSE.,OLINE)
      GOTO 100 

 6    CONTINUE
        ALPH=SQRT((3.6)/PRAD)
        ALPH=DEG(ALPH)
        ALPH=MAX(1.0,ALPH)
C  ***************  HP 7475,7585 ********************
        IF (AXP.EQ.AXP1.AND.AYP.EQ.AYP1) THEN
          WRITE (OLINE,'(2(A,I12),A,I4,A)' )
     +      'AA',NINT(XT1),',',NINT(YT1),
     1       ',',NINT(AP),';'
        ELSE
          WRITE (OLINE,'(4(A,I12),A,F8.2,A)' )
     +    'PU',NINT(AXP1),',',NINT(AYP1),
     1    ';PD;AA',NINT(XT1),',',NINT(YT1),
     2    ',',AP,';'
        END IF
C
        CALL OUTOLL(.FALSE.,OLINE)
C
        GOTO 100
C 
 100  CONTINUE
 
C---  DEFINE END POINT OF ARC
      IF ( AP.LT.0 ) THEN
        AXP=XT1+PRAD*COS(XT5)
        AYP=YT1+PRAD*SIN(XT5)
      ELSE
        AXP=XT1+PRAD*COS(XT6)
        AYP=YT1+PRAD*SIN(XT6)
      END IF
C
      END
*
*
C       @(#)  256.1 date 4/19/90 outlin.ftn Daxcad revision 1.8
      SUBROUTINE OUTLIN(X1,Y1,X2,Y2)
C     ==============================
C1                       R  R  R  R
C1                       I  I  I  I
C
C2     OUTLIN writes to the plot file the command string
C2     for the plotter to draw line from XT1,YT1 To XT2,YT2.
C
      include 'include/pendat.inc'
      include 'include/nbuff.inc'
      include 'include/gerber.inc'
      include 'include/lfont.inc'
      include 'include/ndata.inc'
C
      REAL XT1,YT1,XT2,YT2,DISTXY,DIST1,DIST2,X1,Y1,X2,Y2,ABS,TOT
      INTEGER INT,END
      INTEGER*4 THK,NLEN1
C**   Benson
      INTEGER*4 UP,DOWN,IX1,IY1,IX2,IY2,THISX,THISY
      INTEGER*4 EOBL
      INTEGER*2 UPC,DOWNC
      LOGICAL VIS
      CHARACTER*120  OLINE
      CHARACTER*8 EOB
      INTRINSIC ABS,INT
      EXTERNAL CRUNCH,DISTXY,RSWAP,OUTOLL,PLOTB,NLEN1
C
C     clear buffer string
      OLINE=' '
      XT1=X1
      YT1=Y1
      XT2=X2
      YT2=Y2
C     get current integer position
      THISX=NINT(AXP)
      THISY=NINT(AYP)
C
      IF (CLIPIT) THEN
         CALL CLIPPS(XT1,YT1,XT2,YT2,VIS)
      ELSE
           VIS=.TRUE.
      END IF
      IF ( .NOT. VIS ) RETURN
C     If difference bewtween two end points
C     is less than 0.2 mm why plot it
C
C	  DPS - Why we plot it is postscript actually.  If you dont plot it you
C           Dont even get a dot on a zero width line.
C
      IF (PLOMOD.NE.11.and.PLOMOD.NE.14) THEN
         IF ( ABS(XT1-XT2)+ABS(YT1-YT2) .LT. 0.025*FACT )  RETURN
      END IF
C
      IF ( PLOMOD.NE.11 ) THEN
        DIST1=DISTXY(XT1,YT1,AXP,AYP)
        DIST2=DISTXY(XT2,YT2,AXP,AYP)
        IF ( DIST1 .GT .DIST2 ) THEN
           CALL RSWAP(XT1,XT2)
           CALL RSWAP(YT1,YT2)
        END IF
      END IF
C      write out the text string to internal file.
C            1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8
      GOTO ( 1,1,1,2,1,1,3,1,1,4,5,1,6,7,1,8,9,1 ) PLOMOD
C
C     ***********************************************************
C     *****    HP 7475/7885B                                 ****
C     ***********************************************************
 1    CONTINUE
C        Supports two H.P. plotters 7475 & 7885B
C        If the start of the new line is within 0.2 mm
C        of the last position of the pen then don't move
C        with pen just draw to other end point
         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .LT. 0.05*FACT ) THEN
            WRITE (OLINE,'(2(A,I12),A)' )
     1          'PD',NINT(XT2),',',NINT(YT2),';'
         ELSE
            WRITE (OLINE,'(4(A,I12),A)' )
     +      'PU',NINT(XT1),',',NINT(YT1),
     1      ';PD',NINT(XT2),',',NINT(YT2),';'
         END IF
C        call routine to output direct to plot file.
         CALL OUTOLL(.FALSE.,OLINE)
         GOTO 10
C     ***********************************************************
C     *****    BENSON 1625-S                                 ****
C     ***********************************************************
 2    CONTINUE
C        Benson 1625-S plotter works in cm
         UP=3
         DOWN=2
C        If the start of the new line is within 0.02 cm
C        of the last position of the pen then don't move
C        with pen just draw to other end point
C      WRITE(10,*) '[OUTLIN]',XT1,YT1,XT2,YT2
         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .LT. 0.05*FACT) THEN
            CALL PLOTB(XT2,YT2,DOWN)
         ELSE
            CALL PLOTB(XT1,YT1,UP)
            CALL PLOTB(XT2,YT2,DOWN)
         END IF
         GOTO 10
C     ***********************************************************
C     *****    CALCOMP 906 SERIES                            ****
C     ***********************************************************
 3    CONTINUE
C        CALCOMP 906 contoller series plotters works in cm
         UPC=3
         DOWNC=2
C        If the start of the new line is within 0.02 cm
C        of the last position of the pen then don't move
C        with pen just draw to other end point
         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .LT. 0.05*FACT) THEN
            CALL PLOTC(XT2,YT2,DOWNC)
         ELSE
            CALL PLOTC(XT1,YT1,UPC)
            CALL PLOTC(XT2,YT2,DOWNC)
         END IF
         GOTO 10
C
C     ***********************************************************
C     *****                                                  ****
C     ***********************************************************
 4    CONTINUE
C        Benson 1625-S plotter works in cm
         UP=3
         DOWN=2
C        If the start of the new line is within 0.02 cm
C        of the last position of the pen then don't move
C        with pen just draw to other end point
C      WRITE(10,*) '[OUTLIN]',XT1,YT1,XT2,YT2
         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .LT. 0.05*FACT) THEN
            CALL PLOT(XT2,YT2,DOWN)
         ELSE
            CALL PLOT(XT1,YT1,UP)
            CALL PLOT(XT2,YT2,DOWN)
         END IF
         GOTO 10
C     ***********************************************************
C     *****    GERBER PHOTOPLOTTER OUTPUT                    ****
C     ***********************************************************
 5    CONTINUE
C        get coords in integer form
         IX1=NINT(XT1)
         IY1=NINT(YT1)
         IX2=NINT(XT2)
         IY2=NINT(YT2)
         OLINE=' '
         IF (IX1.NE.THISX .OR. IY1.NE.THISY) THEN
C          have to move to start position of line with aperture closed
C          write control string to file
           WRITE(UNIT=OLINE,FMT='(2(A,I12),A)')
     +      'X',IX1,'Y',IY1,'D02*'
C          send to output
           CALL OUTOLL(.FALSE.,OLINE)
C          update current position
           THISX=IX1
           THISY=IY1
C          set End Of Block characters
           EOB='D01*'
           EOBL=4
        ELSE
C          continuation of last vector
C          set End Of Block chars
           EOB='*'
           EOBL=1
        END IF
C       deal with second point of vector
        OLINE=' '
C       test conditions for 2nd point output
        IF (MOD(OPT+0,2).EQ.1) THEN
C          MODAL coords in effect,output only changed coord
           IF (IX2.NE.THISX) THEN
              IF (IY2.NE.THISY) THEN
C                output both coords since both change
                 WRITE(UNIT=OLINE,FMT='(2(A,I12),A)')
     +             'X',IX2,'Y',IY2,EOB(1:EOBL)
              ELSE
C                output X coord only
                 WRITE(UNIT=OLINE,FMT='(A,I12,A)')
     +             'X',IX2,EOB(1:EOBL)
              END IF
           ELSE IF (IY2.NE.THISY) THEN
C             only Y coord has changed
C             output Y coord only
              WRITE(UNIT=OLINE,FMT='(A,I12,A)')
     +           'Y',IY2,EOB(1:EOBL)
          ELSE
C            no change at all (zero length line)
             OLINE=' '
          END IF
        ELSE
C          non-modal coords,have to output both
           IF (IX2.NE.THISX .OR. IY2.NE.THISY) THEN
C             output both coords
              WRITE(UNIT=OLINE,FMT='(2(A,I12),A)')
     +           'X',IX2,'Y',IY2,EOB(1:EOBL)
           ELSE
C            no change
             OLINE=' '
           END IF
        END IF
C
C       output line if required
        IF (OLINE.NE.' ') CALL OUTOLL(.FALSE.,OLINE)
        GOTO 10
C     ***********************************************************
C     *****  ADVENT                                          ****
C     ***********************************************************
 6    CONTINUE
         THK=PLTHKI(IMBUFF(12))
         IF ( THK.GT.0 ) THEN
            TOT=ABS(LTHKR(1,MOD(THK,64)))
            END=MOD(THK,8192)/1024
         ELSE
            TOT=0.0
            END=0
         END IF
         WRITE (OLINE,'(A,I4,A,F8.2,A,I4,4(A,F12.5))' )
     +   '3,',IMBUFF(3),',',TOT,',',END,',',XT1,',',YT1,',',XT2,',',YT2
C        call routine to output direct to plot file.
         CALL OUTOLL(.FALSE.,OLINE)
         GOTO 10
 
 7    CONTINUE
C     ***********************************************************
C     ******                            postscript output.   ****
C     ***********************************************************
C         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .GT. 0.005*FACT ) THEN
            WRITE (OLINE,'(4(F15.5,A))' )
     +      XT1,'_',YT1,'_moveto_',XT2,'_',YT2,'_lineto_stroke'
C         ELSE
C           WRITE (OLINE,'(2(I12,A))' )
C     1     NINT(XT2),'_',NINT(YT2),'_lineto_stroke'
C         END IF
C        call routine to output direct to plot file.
         CALL CRUNCH(OLINE)
         CALL STRIPO(OLINE,'_',' ')
         WRITE (PLTUNT,'(A) ')  OLINE(1:NLEN1(OLINE))
 
         GOTO 10
  8   CONTINUE
C     ***********************************************************
C     ***** Houston DM/PL plotter                            ****
C     ***********************************************************
C        If the start of the new line is within 0.2 mm
C        of the last position of the pen then don't move
C        with pen just draw to other end point
         IF ( ABS(AXP-XT1)+ABS(AYP-YT1) .LT. 0.05*FACT ) THEN
            WRITE (OLINE,'(2(A,I12))')
     1          'D ',NINT(XT2),',',NINT(YT2)
         ELSE
            WRITE (OLINE,'(4(A,I12))' )
     +      'U ',NINT(XT1),',',NINT(YT1),
     1      ',D ',NINT(XT2),',',NINT(YT2)
         END IF
C        call routine to output direct to plot file.
         CALL OUTOLL(.FALSE.,OLINE)
         GOTO 10
C     ***********************************************************
C     *****              INTERLEAF OUTPUT                    ****
C     ***********************************************************
 9    CONTINUE
         OLINE=' '
         WRITE(UNIT=OLINE,FMT='(A,F8.2,A,F8.2,A,F8.2,A,F8.2,A)')
     +   '(v6,1,0,',XT1,',',0.0-YT1,',',XT2,',',0.0-YT2,',7,0,2,0)'
C        output line
         CALL OUTOLL(.FALSE.,OLINE)
         GOTO 10
C
 10   CONTINUE
C
C      WRITE(10,*) '[OUTLIN]',XT1,YT1,XT2,YT2,AXP,AYP
      AXP=XT2
      AYP=YT2
C
      END
*
*
C       @(#)  256.1 date 4/19/90 plttxt.ftn Daxcad revision 1.8
*
 
C       @(#)  256.1 date 12/16/89 sfftxt.ftn Daxcad revision 1.8
      SUBROUTINE SFFTXT(SING,POS,X,Y,TW,TH,TANG,Z6,TEXT)
C     ==================================================
C1                      R,R, R, R,  R,   R,C*(*)
C1                      I,I, I, I,  I,   I,   I
C     Read font file and output to plot file.
C
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
      include 'include/vntable.inc'
      include 'include/pendat.inc'
C
      LOGICAL TOTAL,SING
      CHARACTER LET*1,TEXT*(*),TEXT1*120
      REAL X1,Y1,X,Y,XT,YT,TEMPX,TEMPY,TW,TH,RAD,Q,R,TMPH,TMPW,
     +     SLA,TAGL,TEXTA(3,3),C1,C2,C3,XO,YO,TANG,Z6,STH
      INTEGER*2 T(2),I,J1,J2,I1(2),JST,NCHAR,CRC
      INTEGER*4 I4,K,C,IP,I3,POS,LC
      EQUIVALENCE  (IP,T)
C
      EXTERNAL TEXBOX,CHKBOX,UCODET,WTOSVY,DRWTBX,XYNORG,RAD,ROTP2D,
     +         NEWXY,DRAWLW
C
C     use temporary storage for text height,width in world coords.
      TMPH=TH*PAPTOW
      TMPW=TW*PAPTOW
C
C      write(10,*) 'sfftxt:',SING,POS,X,Y,TW,TH,TANG,Z6,ICHAR(TEXT)
      CALL UCODET(Z6,SLA,JST,NCHAR)
C      write(10,*) '[sfftxt] ',SLA,JST,NCHAR
C      IF ( SING ) NCHAR=1
C
 222  CONTINUE
      TEXT1=TEXT
C     This is for CR inbedded in files by CV
      CRC=INDEX(TEXT1,CHAR(13))
C     avoid negative length of string if CR first char.
      IF ( CRC .GT. 1 ) THEN
         TEXT=TEXT1(:CRC-1)
         NCHAR=CRC
      END IF
C     Find the bottom left corner of the text.
      CALL  XYNORG(X,Y,JST,TANG,SLA,NCHAR,TMPW,TMPH,XO,YO)
 
C3    TANG is the angle at which the text is tobe written
C     This is in degrees have to convert to radians
      TAGL=RAD(TANG)
 
C      IF ( SING ) THEN
CC       set the character position as the text origin plus
CC       the position-1 times the text width.
CC        WRITE(10,*) 'PLOT:',POS-1,XO,TW,PAPTOW,PAPSCL*FACT,COS(TAGL)
CC              123     3              3    321
C        XO=XO+(POS-1)*TMPW*COS(TAGL)
CC        XO=XO+ (((POS-1)*TW*PAPTOW*COS(TAGL)))*FACT
CC        WRITE(10,*) '[SFFTXT] PLOT:',XO
C      END IF
C
C3    All Characters are defined on a 30x30 square.
      C1=TMPH/30.0
C3    Slant angle is stored in Degrees therefore.
C3    have to convert
      C2=TAN(RAD(SLA))*C1
      C3=TMPW/30.0
      NCHAR=MIN(INT(80),INT(NCHAR))
C      IF (NCHAR.GT.80) NCHAR=80
 
      DO 10  K=1,NCHAR
        LET=TEXT(K:K)
        CALL ROTP2D(XO,YO,TAGL,TEXTA)
C3      If the letter is greater than ASCII 32 then the
C3      character is a valid character
C       specially for the sun
        LC = MOD( ICHAR(LET)+256,256)
        IF ( LC .GT. 32 ) THEN
C3        Take the ASCII value of the character then offset
C3        this by one
          I4= LC + 1
C3        IP has the pointer value to where the character definition
C3        is stored
          IP=FONT1(I4)
C         READ (UNIT=20,REC=I4) T(1),T(2)
          I4=T(2)
          IF (I4.GT.0) THEN
C3          IP now has the ASCII number and the length of the
C3          definition using the equivalence with the variable T
C3             T(1)   contains the ASCII number
C3             T(2)     "       "  length of the definition.
            IP=FONT1(I4)
C           READ(UNIT=20,REC=T2) T(1),T(2)
            DO 20 I3=1,T(2)
C             Read definition from Font file
              IP=FONT1(I4+I3)
C             READ(UNIT=20,REC=I4+I3) T(1),T(2)
C             If X coordinate exceeds 100 then a line has to
              IF ( T(1) .GE. 100 ) THEN
                TEMPX=XO+ C2*REAL(T(2)) + C3*(REAL(T(1))-100)
                TEMPY=YO+ C1*REAL(T(2))
                CALL NEWXY(TEMPX,TEMPY,X1,Y1,TEXTA)
                CALL PLTL03(XT,YT,X1,Y1)
              ELSE
                TEMPX=XO+ C2*REAL(T(2)) + C3*REAL(T(1))
                TEMPY=YO+ C1*REAL(T(2))
                CALL NEWXY(TEMPX,TEMPY,X1,Y1,TEXTA)
              END IF
              XT=X1
              YT=Y1
 20         CONTINUE
          ELSE
C      WRITE(10,*) '[SFFTXT]Illegal character at',K,'value',LC
C      WRITE(10,*) '[SFFTXT]IMBUFF',(IMBUFF(I),I=1,13)
C      WRITE(10,*) '[SFFTXT]IDBUFF',(IDBUFF(I),I=1,4)
C      WRITE(10,*) '[SFFTXT]CBUFF',TEXT
          END IF
        END IF
        XO=XO+TMPW*COS(TAGL)
        YO=YO+TMPW*SIN(TAGL)
 10   CONTINUE
C
      IF ( CRC .NE. 0 ) THEN
         Y=Y-1.5*COS(TAGL)*TH
         X=X+1.5*SIN(TAGL)*TH
         TEXT=TEXT1(CRC+1:)
         GOTO 222
      END IF
      END
C
C
 
