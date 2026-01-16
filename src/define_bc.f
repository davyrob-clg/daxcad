C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 define.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CHGTXT(X,Y,SLNT,JST,MTEXT,NCHAR,TMIP,OK)
C     SUBROUTINE DCPR05(PRPNT,PFIELD,PTEXT,ST)
C     SUBROUTINE DEFB00()
C     SUBROUTINE DEFB01()
C     SUBROUTINE DEFC01()
C     SUBROUTINE DEFG01()
C     SUBROUTINE DEFG02()
C     SUBROUTINE DEFG03()
C     SUBROUTINE DEFM00()
C     SUBROUTINE DEFP00()
C     SUBROUTINE DEFP02()
C     SUBROUTINE DEFP03()
C     SUBROUTINE DEFS00()
C     SUBROUTINE DEFS01()
C     SUBROUTINE DEFT00()
C     SUBROUTINE DEFT01()
C     SUBROUTINE DEFT02()
C     SUBROUTINE DEFT03(CN)
C     SUBROUTINE DEFT10(FJUST)
C     SUBROUTINE DEFT97(TNAME,OK)
C     SUBROUTINE DEFT98(TNAME,OK)
C     SUBROUTINE DEFVP0()
C     SUBROUTINE DMPL01(TMIP,PRIPNT)
C     SUBROUTINE DMPRC0(PRPNT,PFIELD,PPTEXT,PRCPNT,OK)
C     SUBROUTINE DMPRI0(PRIPNT,OK)
C     SUBROUTINE DRPL01(PRIPNT,NRECS,NPROPS,OK)
C     SUBROUTINE DRPR01(PRPNT,OK)
C     SUBROUTINE DRPRC0(OK)
C     SUBROUTINE DRPRC1(PCPNT,IPCBUF,PTEXT,OK)
C     SUBROUTINE DRPRI0(PRIPNT,OK)
C     SUBROUTINE DRPRI5(PRIPNT,IPBUF1,OK)
C     SUBROUTINE DSPR01(PNAME,PRPNT,FOUND)
C     SUBROUTINE DWPL02(TMIP,PRIPNT,OK)
C     SUBROUTINE DWPL10(TMIP,PRIPNT,OK)
C     SUBROUTINE DWPL11(TMIP,OK)
C     SUBROUTINE DWPL20(PRIP,TMIP,OK)
C     SUBROUTINE DWPR01(PRPNT,OK)
C     SUBROUTINE DWPRC0(PRPNT,PFIELD,PPTEXT,PRCPNT,OK)
C     SUBROUTINE DWPRI0(PRIPNT,OK)
C     SUBROUTINE DWPRI5(IPBUF,PRIPNT,OK)
C     SUBROUTINE DWPRI6(IPBUF,PRIPNT,OK)
C     SUBROUTINE INPLNK()
C     SUBROUTINE MAJDF1()
C     SUBROUTINE MNIDEF()
C     SUBROUTINE MNLDF0()
C     SUBROUTINE MNLDKD()
C     SUBROUTINE MNLDMN()
C     SUBROUTINE MNLDT0()
C     SUBROUTINE MNLVP0()
C     SUBROUTINE MODMEN()
C     SUBROUTINE SAVMEN(ST)
C     SUBROUTINE UPR001(TMIP,OK)
C     SUBROUTINE UPR066(TMIP,OK)
C     SUBROUTINE UPR100(P1,OK)
C     SUBROUTINE UPR101(OK)
C     SUBROUTINE VERC01()
C     
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CHGTXT(X,Y,SLNT,JST,MTEXT,NCHAR,TMIP,OK)
C     ===================================================
C     changes the text entered in the cell where it has been modified
C     rather than entered fresh.
C     part of the CREATE MENU code
C     created 25/7/88 - mlw
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 P,TMIP,ENTYPE,N,JST,NCHAR
      CHARACTER*80 MTEXT
      REAL X,Y,CODE,SLNT
      LOGICAL OK
C
      OK=.FALSE.
C
C     disable searching for all entity types except text
      CALL NOSRCH()
      CALL ADSRCH(TEXT)
C     find the MI pointer to the text in that cell
C     (this also reads the MI data and flags the entity)
      CALL DSE800(X,Y,OK)
      IF (.NOT.OK) GOTO 99
      TMIP=MIP
C      write(10,*) 'returned from DSE800 - tmip : ',tmip
C
C     read the data at that position
      CALL DER500(TMIP,OK)
      IF (.NOT.OK) GOTO 99
      P=IMBUFF(9)
C
C     erase the existing text
      IF (IMBUFF(2).EQ.TEXT) THEN
          CALL ERSTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +                RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1                CBUFF)
      END IF
C
C     alter the data
      CALL CODET(SLNT,JST,NCHAR,CODE)
      RDBUFF(6)=CODE
c      WRITE(10,*) 'CODE,RDBUFF(6) : ',CODE,RDBUFF(6)
      CBUFF=MTEXT(1:NCHAR)
      IMBUFF(1)=MOD(IMBUFF(1)+0,128)
c      WRITE(10,*) ' text before change : ',TXFILE(P)
      CALL DEM500(TMIP,OK)
c      write(10,*) 'returned from DEM500 - tmip : ',tmip
c      WRITE(10,*) 'text after change : ',TXFILE(P)
      IF (.NOT.OK) GOTO 99
C
      RETURN
C
  99  CONTINUE
      write(10,*) '******* ERROR modifying menu cell *******'
C
      END
C
      SUBROUTINE DCPR05(PRPNT,PFIELD,PTEXT,ST)
C     ========================================
C
C1    vartype            I2     I2    C*80 I4
C1    iostatus           I      I      I   O
C
C2    subroutine DCPR05 reads the property record number
C2    PRPNT,and if it is a valid attached property record,
C2    tests data field PFIELD for a match with the text PTEXT.
C2    COMPLETION STATUS RETURNED IN ST.
C2          ST=0   match found
C2          ST=1   no match
C2          ST=2   no data in field
C2          ST=3   not an attached property
C2          ST=4   record pointer out of range
C2          ST=5   field number out of range
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,PFIELD,IPBUF(8),PCPNT,IPCBUF(2)
      INTEGER*4 ST
      LOGICAL OK
      CHARACTER*80 PTEXT,PTEXT1
      EXTERNAL DRPRI5,DRPRC1
C
C     read the property record
      CALL DRPRI5(PRPNT,IPBUF,OK)
      IF (.NOT.OK) THEN
         ST=4
         RETURN
      END IF
C     got the index block
C     set the data pointer
      PCPNT=IPBUF(PFIELD)
      IF (PCPNT.GT.0) THEN
C        read the data field into PPTEXT
         CALL DRPRC1(PCPNT,IPCBUF,PTEXT1,OK)
         IF (.NOT.OK) THEN
            ST=5
            RETURN
         END IF
C        test for match
         IF (PTEXT.EQ.PTEXT1) THEN
C           data is same
            ST=0
            RETURN
         ELSE
C           data does not match
            ST=1
            RETURN
         END IF
      ELSE
C        no data in this field
         ST=2
         RETURN
      END IF
C
C
      END
C
C
      SUBROUTINE DEFB00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the DEFINE BACKCLOTH function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,DEFB01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DEFINE BACKCLOTH routine
      CALL DEFB01()
C     ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFB01()
C     ===================
C1    no arguments required
C
      include 'include/params.inc'
      include 'include/section.inc'
      include 'include/macro.inc'
      include 'include/menun.inc'
      include 'include/wildc.inc'
C
CSUN
      INTEGER*4 DNUM,NLEN,ST,MEN3,MEN4
      REAL II(4)
      REAL SCALE
      LOGICAL CREBAK,OK,QUIT,OK1
      CHARACTER*80 SCNAME,BACNAM,PAP*3,SFILE,WILD
      EXTERNAL GTCLRM,NLEN,NLEN1,CREBAK
C
CC     enter the DEFINE BACKCLOTH routine
CC
CC     prompt for backcloth name
CC      SFILE=PATHN(6)(:NLEN1(PATHN(6)))//'Back.msf'
CC      CALL CRUNCH(SFILE)
CC   experiment with generating the file on the fly
CC      WILD=PATHN(6)(:NLEN1(PATHN(6)))//WILDC(6)
CC      CALL CRUNCH(WILD)
CC      CALL DIRFIN(SFILE,WILD,OK)
CC   end of generation
C SPB - 031194 - Prompt the user for a filename
      CALL DISFIL(SFILE,BACNAM,6,OK,QUIT)
CC     tidy up the .msf file
CC      CALL DELETE(SFILE,OK1)
      IF (QUIT) THEN
        RETURN
      END IF
      IF (.NOT.OK) THEN
CC         Either the MSF file has not been found or the
CC         user has decided to select his own name
         CALL DPRMXP(484,BACNAM)
      ENDIF
      IF (NLEN(BACNAM).GT.0) THEN
C SPB - 031194 - Don't force a suffix on the user ...
C        CALL SUFFIX(BACNAM,'.im')
CC       First, free up any existing backcloth.
        CALL OSCLRM()
CC       make call for backcloth
        CALL CRUNCH(BACNAM)
        OK=CREBAK(BACNAM,SCALE,PAP,II(1),II(2),II(3),II(4))
        IF (OK) THEN
          SCNAME=DRGNAM
          CALL SUFFIX(SCNAME,'.sec')
CC         get local name to store in section record
C SPB 031194 - This is cack - it goes and tries to
C              get the image from the same directory
          CALL LOCNAM(BACNAM)
CC         backcloth found,use return data
          CALL OSAMAP(SCNAME,BACNAM,II,ST)
CC         set scale as returned
          SECSCL=SCALE
CC         set sheet key
          SHTKEY=PAP
CC         setup section block
          CALL OSETMM()
CC	   load the bitmap
          CALL OSLODA()
CC         zoom extents cos origin has changed
          CALL ZOMEXT()
        END IF
      END IF
CSUN
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFC01()
C     ===================
C1    no arguments required
C2    used to define the text string associated with each selected cell
C2    part of the CREATE MENU code
C2    created 11/7/88 - mlw
C
      include 'include/vntable.inc'
      include 'include/menug.inc'
      include 'include/menun.inc'
      include 'include/style.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
C
      CHARACTER*80 TBUFF,ANS*1,C*1,ENTRY*20
      REAL X,Y,WDTH,HGHT,SLNT,ANGLE
      INTEGER*4 DNUM
      INTEGER*2 TMIP1,ENTYPE,JST,NLEN2,LNGTH
      LOGICAL YESOK,OK,CHTEXT,OUTR
C
      EXTERNAL YESOK,NLEN2
C
      WDTH=INCX/20.0
      HGHT=THIGT*(WDTH/TWIDTH)
      JST=4
      SLNT=0.0
      ANGLE=0.0
C
10    CONTINUE
      CHTEXT=.FALSE.
      DNUM=553
      CALL CPRINT(DICT01(DNUM))
      CALL TCURS(C,X,Y)
C
C     check for another menu request
      IF (MEN.EQ.3.OR.MEN.EQ.2) RETURN
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
C     get cell which has been hit
      CELLX=INT((X-GLIMIT(1))/INCX)+1
      CELLY=INT((Y-GLIMIT(2))/INCY)+1
C     Check to see if out of range if so tell him and go
C     back round
      OUTR = .FALSE.
      IF ((CELLX.LT.1).OR.(CELLX.GT.XN)) THEN        
          OUTR = .TRUE.
      ENDIF                       
      IF ((CELLY.LT.1).OR.(CELLY.GT.YN)) THEN        
          OUTR = .TRUE.
      ENDIF                          
      IF (OUTR) THEN
          DNUM = 773 
          CALL DEPRNT(DNUM)
          GOTO 10
      ENDIF

C     check if he's trying to overwrite...and tell him what he's up to
      IF (DMENU(CELLX,CELLY).NE.'undefined') THEN
         DNUM=561
         CALL DCPRN2(DNUM,DMENU(CELLX,CELLY))
         DNUM=437
         CALL DPRMXP(DNUM,ANS)
C        changed his mind again...so have another go
         IF (.NOT.YESOK(ANS)) GOTO 10
         CHTEXT=.TRUE.
      END IF
C     get text which is to be associated with selected cell
      DNUM=556
      CALL DPRMXP(DNUM,ENTRY)
C     null entry indicates he's changed his mind
      IF (ENTRY.EQ.' ') RETURN
C     save it!
      DMENU(CELLX,CELLY)=ENTRY
C     display it!
      X=GLIMIT(1)+(INCX*(CELLX-1))+(INCX/2.0)
      Y=GLIMIT(2)+(INCY*(CELLY-1))+(INCY/8.0)
      IF (CHTEXT) THEN
          LNGTH=NLEN2(ENTRY)
          CALL CHGTXT(X,Y,SLNT,JST,ENTRY,LNGTH,TMIP1,OK)
      ELSE
          CALL DEWC85(X,Y,WDTH,HGHT,ANGLE,SLNT,JST,FONT,COLOUR,
     +                LAYER,ENTRY,TMIP1,OK)
      END IF
C
      ENTYPE=TEXT
c      write(10,*) 'calling ALLDRW - TMIP : ',tmip1
      CALL ALLDRW(ENTYPE,TMIP1)
C
C     flag the fact that a part definition exists
      MENDEF=.TRUE.
C     go get more
      GOTO 10
C
      END
C
      SUBROUTINE DEFG01()
C     ===================
C1    no arguments required
C2    part of the CREATE MENU code
C2    controls the option to define the grid layout on which the menu
C2    is to be based
C2    created 11/7/88 - mlw
C
      include 'include/vntable.inc'
      include 'include/menug.inc'
      include 'include/menun.inc'
C
      CHARACTER*80 TBUFF,ANS*1,C*1
      DOUBLE PRECISION DN
      REAL XNUM,YNUM,TLIMIT,X,Y
      INTEGER*4 NLEN,DNUM
      INTEGER*2 I,J
      LOGICAL YESOK
C
      EXTERNAL NLEN,YESOK
C
C     check if half-way through definition and warn the guy he's going
C     to lose the lot
      IF (MENDEF) THEN
          DNUM=557
          CALL EPRINT(DICT01(DNUM))
          DNUM=558
          CALL CPRINT(DICT01(DNUM))
          DNUM=16
          CALL DPRMXP(DNUM,ANS)
          IF (.NOT.YESOK(ANS)) RETURN
       END IF
 
C      he's quite happy...so initialise the buffer
       DO 5 J=1,50
          DO 6 I=1,50
             DMENU(J,I)='undefined'
6         CONTINUE
5      CONTINUE
C
C     check grid is to be based on correct paper size and rotation
      CALL PAPCHK(PAPOK,PLIMIT)
      IF (.NOT.PAPOK) RETURN
C
11    CONTINUE
C     load points modes
      CALL MNLPTS()
C     get extents of grid
      DNUM=552
      CALL CPRINT(DICT01(DNUM))
      CALL GETANS(C,GLIMIT(1),GLIMIT(2))
      IF (MEN.NE.0) THEN
          DNUM=117
          CALL EPRINT(DICT01(DNUM))
          GOTO 11
      END IF
      CALL GETANS(C,GLIMIT(3),GLIMIT(4))
      IF (MEN.NE.0) THEN
          DNUM=117
          CALL EPRINT(DICT01(DNUM))
          GOTO 11
      END IF
C
C     unload points modes
      CALL MNUPTS()
C
12    CONTINUE
C     get No. of cells in x-plane
      CALL DPRMXP(548,TBUFF)
      IF (NLEN(TBUFF).EQ.0 ) THEN
         RETURN
      ELSE
C        evaluate an arithmetic expression from the keyboard
         CALL AEXPRN(TBUFF,DN,*99)
         XNUM=REAL(DN)
      END IF
C
C     get No. of cells in y-plane
      CALL DPRMXP(549,TBUFF)
      IF (NLEN(TBUFF).EQ.0 ) THEN
         RETURN
      ELSE
C        evaluate an arithmetic expression from the keyboard
         CALL AEXPRN(TBUFF,DN,*99)
         YNUM=REAL(DN)
      END IF
C
      IF(GLIMIT(1).GT.GLIMIT(3)) THEN
         TLIMIT=GLIMIT(1)
         GLIMIT(1)=GLIMIT(3)
         GLIMIT(3)=TLIMIT
      END IF
      IF(GLIMIT(2).GT.GLIMIT(4)) THEN
         TLIMIT=GLIMIT(2)
         GLIMIT(2)=GLIMIT(4)
         GLIMIT(4)=TLIMIT
      END IF
C
      INCX=(GLIMIT(3)-GLIMIT(1))/XNUM
      INCY=(GLIMIT(4)-GLIMIT(2))/YNUM
      XN=NINT(XNUM)
      YN=NINT(YNUM)
C
      IF (INCX.LT.10.0.OR.INCY.LT.10.0) THEN
          DNUM=550
          CALL EPRINT(DICT01(DNUM))
          GOTO 12
      END IF
C
      CALL DEFG02()
C
      DNUM=2
      CALL DPRMXP(DNUM,ANS)
      IF (.NOT.YESOK(ANS)) THEN
          CALL REDRAW()
          RETURN
      END IF
C     clear temporary grid display
      CALL REDRAW()
C     display grid permanently and save in database
      GRIDOK=.TRUE.
      CALL DEFG03()
      MENDEF=.TRUE.
C
99    CONTINUE
      END
C
      SUBROUTINE DEFG02()
C     ================== =
C2    part of CREATE MENU CODE
C2    displays grid on screen for verification - not stored in
C2    database
C
      include 'include/menug.inc'
C
      REAL X1,Y1,X2,Y2
      INTEGER*2 N
      INTEGER*4 DNUM
C
C     display vertical lines first
      X1=GLIMIT(1)
      Y1=GLIMIT(2)
      X2=X1
      Y2=GLIMIT(4)
      DO 20 N=0,XN
         CALL DRAWLW(X1,Y1,X2,Y2)
         X1=X1+INCX
         X2=X1
20    CONTINUE
C
 
C     now display horizontal lines
      X1=GLIMIT(1)
      Y1=GLIMIT(2)
      X2=GLIMIT(3)
      Y2=Y1
      DO 30 N=0,YN
         CALL DRAWLW(X1,Y1,X2,Y2)
         Y1=Y1+INCY
         Y2=Y1
30    CONTINUE
C
      END
C
      SUBROUTINE DEFG03()
C     ===================
C2    part of CREATE MENU CODE
C2    displays grid on screen permanently and stores in database
C     also adds markers and title box
C
      include 'include/style.inc'
      include 'include/entity.inc'
      include 'include/menug.inc'
C
      REAL X1,Y1,X2,Y2,RADIUS,EANG,SANG
      INTEGER*2 N,TMIP1,ENTYPE
      INTEGER*4 DNUM
      LOGICAL OK
 
      LAYER=1
      FONT=1
      ENTYPE=LINE
C     display vertical lines first
      X1=GLIMIT(1)
      Y1=GLIMIT(2)
      X2=X1
      Y2=GLIMIT(4)
      DO 20 N=0,XN
C        store the newly constructed line.
         CALL DEWC03(X1,Y1,X2,Y2,FONT,LAYER,TMIP1,OK)
C        draw the line which has just been stored
         CALL ALLDRW(ENTYPE,TMIP1)
         X1=X1+INCX
         X2=X1
20    CONTINUE
C
 
C     now display horizontal lines
      X1=GLIMIT(1)
      Y1=GLIMIT(2)
      X2=GLIMIT(3)
      Y2=Y1
      DO 30 N=0,YN
         CALL DEWC03(X1,Y1,X2,Y2,FONT,LAYER,TMIP1,OK)
C        draw the line which has just been stored
         CALL ALLDRW(ENTYPE,TMIP1)
         Y1=Y1+INCY
         Y2=Y1
30    CONTINUE
C
C     draw the box for the menu title
      X1=GLIMIT(1)
      Y1=GLIMIT(4)
      X2=X1
      Y2=Y1+10
      CALL DEWC03(X1,Y1,X2,Y2,FONT,LAYER,TMIP1,OK)
C     draw the line which has just been stored
      CALL ALLDRW(ENTYPE,TMIP1)
      Y1=Y2
      X2=GLIMIT(3)
      CALL DEWC03(X1,Y1,X2,Y2,FONT,LAYER,TMIP1,OK)
C     draw the line which has just been stored
      CALL ALLDRW(ENTYPE,TMIP1)
      X1=X2
      Y2=GLIMIT(4)
      CALL DEWC03(X1,Y1,X2,Y2,FONT,LAYER,TMIP1,OK)
C     draw the line which has just been stored
      CALL ALLDRW(ENTYPE,TMIP1)
C
C     mark the corners with an arc
      ENTYPE=ARC
      RADIUS=2.0
      SANG=0.0
      EANG=SANG
      X1=GLIMIT(1)
      Y1=GLIMIT(2)
      X2=GLIMIT(3)
      Y2=GLIMIT(4)
C
C     store the new arc
      CALL DEWC05(X1,Y1,RADIUS,SANG,EANG,FONT,LAYER,TMIP1,OK)
C     draw the arc
      CALL ALLDRW(ENTYPE,TMIP1)
C     store the new arc
      CALL DEWC05(X2,Y2,RADIUS,SANG,EANG,FONT,LAYER,TMIP1,OK)
C     draw the arc
      CALL ALLDRW(ENTYPE,TMIP1)
C
      END
C
 
      SUBROUTINE DEFM00()
C     ===================
C1    no arguments required
C
C2    Controls the CREATE MENU OPTION
C
      include 'include/menun.inc'
      include 'include/menug.inc'
      include 'include/macro.inc'
      include 'include/wildc.inc'
      include 'include/ndata.inc'
      include 'include/vntable.inc'
C
      CHARACTER TEXT*80,TBUFF*80,ANS*1
      DOUBLE PRECISION DN
      INTEGER*4 C,ACTCEL,LPATH,LFILE,NLEN1,DNUM,ST
      LOGICAL OK,FOK,YESOK
      REAL X,Y
C
      EXTERNAL NLEN1,YESOK
C
C     load up the menu cells on menu 3
      CALL MNLDMN()
C
C     clear all the flags
      GRIDOK=.FALSE.
      PAPOK=.FALSE.
      MENDEF=.FALSE.
C
C     this stops a floating value that upsets gtmclo
      ACTCEL=1
C
10    CONTINUE
      DNUM=554
      CALL CPRINT(DICT01(DNUM))
      CALL TCURS(C,X,Y)
15    CONTINUE
      IF(CCMD.EQ.'Q'.OR.CCMD.EQ.'q'.OR.MEN.EQ.2) THEN
          IF (MENDEF) THEN
             DNUM=557
             CALL EPRINT(DICT01(DNUM))
             DNUM=558
             CALL CPRINT(DICT01(DNUM))
             DNUM=16
             CALL DPRMXP(DNUM,ANS)
             IF (YESOK(ANS)) RETURN
          ELSE
             RETURN
          END IF
          CALL GTMCLO(MEN,CELLN)
          GOTO 10
       END IF
C
      IF(MEN.EQ.3.AND.(CELLN.GE.1.AND.CELLN.LT.(1+(3*NPATH)) )) THEN
C        hilight chosen cell
         IF (CELLN.NE.ACTCEL) THEN
            CALL GTMCLO(3,ACTCEL)
            ACTCEL=CELLN
            CALL GTMCHI(3,ACTCEL)
         END IF
C
C        decipher option request
         IF (CCMD.EQ.'d') THEN
             CALL DEFG01()
             CALL GTMCLO(3,ACTCEL)
        ELSE IF (CCMD.EQ.'D') THEN
             IF(PAPOK.AND.GRIDOK) THEN
                CALL DEFC01()
                GOTO 15
             ELSE
                DNUM=555
                CALL EPRINT(DICT01(DNUM))
             END IF
         ELSE IF (CCMD.EQ.'V') THEN
             IF(MENDEF) THEN
                CALL VERC01()
                GOTO 15
             ELSE
                DNUM=559
                CALL EPRINT(DICT01(DNUM))
             END IF
         ELSE IF (CCMD.EQ.'S') THEN
             IF(MENDEF) THEN
                CALL SAVMEN(ST)
                IF (ST.EQ.1) THEN
C                 User failed to specify file so go back round
                  DNUM = 772
                  CALL DEPRNT(DNUM)                                
                ENDIF
             ELSE
                DNUM=559
                CALL EPRINT(DICT01(DNUM))
             END IF
         ELSE IF (CCMD.EQ.'M') THEN
             IF (MENDEF) THEN
                DNUM=557
                CALL EPRINT(DICT01(DNUM))
                DNUM=558
                CALL CPRINT(DICT01(DNUM))
                DNUM=16
                CALL DPRMXP(DNUM,ANS)
                IF (YESOK(ANS)) CALL MODMEN()
             ELSE
                CALL MODMEN()
             END IF
         ELSE
             DNUM=2
             CALL EPRINT(DICT01(DNUM))
         END IF
       END IF
C
C      clear the active cell, and return to get next option
       CALL GTMCLO(3,ACTCEL)
       GOTO 10
C
      END
C
      SUBROUTINE DEFP00()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
 
      EXTERNAL GTCLRM,DEFP02,MNLDF0,UNFLAG
C
      TMEN=MEN
      TCELL=CELLN
C     initialize DEFINE PROPERTY menu
      CALL MNLDF0()
C     enter the DEFINE PROPERTY routine
      CALL DEFP02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTMCLO(TMEN,TCELL)
 
      CALL GTCLRM(3)
C
      END
C
      SUBROUTINE DEFP02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine DEFP02 handles the definition
C2    of properties for attachment later to
C2    entities in the graphical database.
C
      include 'include/menun.inc'
      include 'include/props.inc'
C
      INTEGER*4 C,GTCELL,NLEN,I
      INTEGER*2 PRCPNT,PRIPNT,PRPNT,I2
      REAL X,Y
      LOGICAL FOUND,OK
      CHARACTER*80 PNAME,PPROMT
      EXTERNAL GTHIMC,DCPRNT,DEPRNT,TCURS,DEFP03,NLEN
      EXTERNAL DSPR01,FNDTOK,FNDPOS,GTMCHI
C
C     set the default data type for the property
C     set default to text
      PDATYP=8
C     set default action code to KB input
      PACODE=1
C     ensure menu shows current state
C     only TEXT hilited
      CALL GTHIMC(3,'T','IRT',GTCELL)
C
C     goto default entry point by asking for
C     name of property to change
C     load token for "Name" option
C      CALL FNDTOK(343,CCMD)
C     ensure "Name" is hilited
C      MEN=3
C      CALL FNDPOS(343,CELLN)
C      CALL GTMCHI(MEN,CELLN)
C      GOTO 21
C
 10   CONTINUE
C     print prompt for user
C     "Select an option from the MINOR menu"
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
C
 21   CONTINUE
C
      IF (MEN.EQ.3) THEN
         IF (CCMD.EQ.'N') THEN
C           NAME selected
C           ask him for it
C          "Enter name of property:"
            CALL DPRMXP(281,PNAME)
            IF (NLEN(PNAME).EQ.0) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
C           fold name to upper case,to avoid confusion
            CALL FOLDUP(PNAME)
C           go check for previous definition of same name
            CALL DSPR01(PNAME,PRPNT,FOUND)
            IF (FOUND) THEN
C              already exists
C             "Property of this name already defined"
               CALL DEPRNT(340)
C              reset caller menu cell
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
C          "Enter prompt for data:"
            CALL DPRMXP(300,PPROMT)
C           reset caller menu cell
            CALL GTMCLO(MEN,CELLN)
C           set up property definition in files
C           clear buffer first
            DO 500 I=1,8
               PRIBUF(I)=0
 500        CONTINUE
C           set record type to property definition
            PRIBUF(1)=1
C           set property type to current status
            PRIBUF(2)=PDATYP*256+PACODE
C           write property name to file
            I2=5
            CALL DWPRC0(NPRPOS,I2,PNAME,PRCPNT,OK)
C           store pointer in control block
            PRIBUF(5)=PRCPNT
C           write prompt to file
            IF (NLEN(PPROMT).GT.0) THEN
               I2=6
               CALL DWPRC0(NPRPOS,I2,PPROMT,PRCPNT,OK)
C              store pointer in control block
               PRIBUF(6)=PRCPNT
            END IF
C           write the control block to file
            CALL DWPRI0(PRIPNT,OK)
C
            GOTO 10
         ELSE
C           go handle options
            CALL DEFP03()
            GOTO 10
         END IF
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') THEN
         RETURN
      ELSE
C       "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFP03()
C     ===================
C
C1    no arguments required
C
C2    Subroutine DEFP03 sets the correct
C2    data type for the property on the basis of
C2    a hit in menu 3.
C
      include 'include/menun.inc'
      include 'include/props.inc'
C
      INTEGER*4 TMEN,TCELL,GTCELL
      EXTERNAL GTHIMC,GTMCLO
C
      TMEN=MEN
      TCELL=CELLN
      IF (CCMD.EQ.'I') THEN
C        set data type to INTEGER
         PDATYP=2
C        hilite integer cell
         CALL GTHIMC(3,'I','IRT',GTCELL)
      ELSE IF (CCMD.EQ.'R') THEN
C        set data type to REAL
         PDATYP=4
C        hilite integer cell
         CALL GTHIMC(3,'R','IRT',GTCELL)
      ELSE IF (CCMD.EQ.'T') THEN
C        set data type to TEXT
         PDATYP=8
C        hilite integer cell
         CALL GTHIMC(3,'T','IRT',GTCELL)
      ELSE
C        clear entry cell
         CALL GTMCLO(TMEN,TCELL)
      END IF
C
      END
C
C
      SUBROUTINE DEFS00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the DEFINE SEARCH-KEY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,DEFS01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DEFINE SEARCH-KEY routine
      CALL DEFS01()
C     ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
      SUBROUTINE DEFS01()
C     ===================
C1    no arguments required
C
      include 'include/psearch.inc'
C
      INTEGER*4 DNUM
      EXTERNAL DPRMXP,FOLDUP
C
C     prompt for search-key
C     483    "Enter Property for use as search key:"
      DNUM=483
      CALL DPRMXP(DNUM,SRCKEY)
      CALL FOLDUP(SRCKEY)
C     reset search pointer
      NPSRCH=1
C
      END
C
      SUBROUTINE DEFT00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the DEFINE TABLE function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,DEFT01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DEFINE PROPERTY routine
      CALL DEFT01()
C     clear caller menu cell
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFT01()
C     ===================
C1    no arguments required
C
      include 'include/propt.inc'
C
      EXTERNAL GTCLRM,DEFT02,MNLDT0
C
C     set default colum width
      COLW=10
C     set default justification
      FJUST=1
C     set default position
      COLPOS=2
C     set default significant figures
      SIGFIG=2
C     initialize DEFINE TABLE menu
      CALL MNLDT0()
C     enter the DEFINE TABLE routine
      CALL DEFT02()
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFT02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine DEFT02 handles the definition
C2    of properties tables for extract of data from
C2    entities in the graphical database.
C
      include 'include/menun.inc'
      include 'include/props.inc'
      include 'include/propt.inc'
      include 'include/vntable.inc'
      include 'include/library.inc'
      include 'include/ftypes.inc'
C
      INTEGER*4 C,GTCELL,NLEN,I,J,N,NLEN1
      INTEGER*2 PRCPNT,PRIPNT,PRPNT
      REAL X,Y
      LOGICAL FOUND,OK,EX,MAKEOK
      CHARACTER*80 PNAME,PPROMT,TNAME,TMPT*4,DIRNAM,TBFNAM
      EXTERNAL GTHIMC,DCPRNT,DEPRNT,TCURS,DEFP03,NLEN
      EXTERNAL DSPR01,VRFINT,DPRMX3,ASKINT,FOLDUP,MAKEOK
C
C
C     check for the existance of the valid directory
C     dont bother letting him do anything until it is
C     created
 
C     set the global filtype
      DAXTYP = DAXPOP
CIBM
C      DIRNAM=LIBRARY(1:NLEN(LIBRARY))//'\ptabl\'
CIBM
CPC386
C      DIRNAM=LIBRARY(1:NLEN(LIBRARY))//'\\ptabl\\'
CPC386
CAPOLLO|SUN
      DIRNAM=LIBRARY(1:NLEN(LIBRARY))//'/ptabl/'
      CALL VALDIR(DIRNAM,OK)
CAPOLLO|SUN
      IF(.NOT.OK) THEN
          PNAME=DIRNAM(1:NLEN(DIRNAM)+1)//DICT01(280)
          CALL EPRINT(PNAME)
          MEN=0
          RETURN
      ENDIF
      FOUND=.FALSE.
C     set default left margin position
      COLPOS=2
C     set default column width
      COLW=10
C     set default compress action to null
      DO 5 I=1,20
         COMACT(I)=0
 5    CONTINUE
C
 10   CONTINUE
C     print prompt for user
C     "Select an option from the MINOR menu"
      CALL DCPRNT(38)
      CALL TCURS(C,X,Y)
C
      IF (MEN.EQ.3) THEN
         IF (CCMD.EQ.'N') THEN
C           NAME selected
C           ask him for it
C          "Enter name of extract table:"
 100        CONTINUE
            CALL DPRMXP(216,TNAME)
            IF (NLEN(TNAME).EQ.0) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 10
            END IF
CIBM
C           TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'\ptabl\'//tname
CIBM
CPC386
C           TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'\\ptabl\\'//tname
CPC386
CSUN|APOLLO
            TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'/ptabl/'//TNAME
CSUN|APOLLO
C
 
C           go check for previous definition of same name
            INQUIRE(FILE=TBFNAM,EXIST=EX)
            IF(EX) THEN
                IF(.NOT.MAKEOK()) GOTO 100
            ENDIF
 
C     **********************************************
C     ***        TABLE HEADER DEFINITION         ***
C     **********************************************
C
C           set default page size
C           page width and length
            PAGDAT(1)=80
            PAGDAT(2)=66
C           left and right margins
            PAGDAT(3)=2
            PAGDAT(4)=72
            PAGDAT(5)=0
            PAGDAT(6)=0
            PAGDAT(7)=0
            PAGDAT(8)=0
 
 300        CONTINUE
C           "Enter number of lines in table header:"
            CALL ASKINT(233,.TRUE.,1,NHHLIN)
            IF (NHHLIN.GT.20) THEN
C              too many lines in header
C              "Only 20 lines allowed in header"
               CALL DEPRNT(158)
               GOTO 300
            END IF
C           have valid number of lines
C           read the header data into buffer
C           "Type the table header"
            CALL DCPRNT(161)
C           promt each line with it's number
            DO 301 I=1,NHHLIN
               WRITE(UNIT=TMPT,FMT='(I3,A)') I,'>'
               CALL DPRMX3(279,TMPT,THEADR(I))
 301        CONTINUE
C
C     **********************************************
C     ***          TABLE COLUMN LAYOUT           ***
C     **********************************************
 302        CONTINUE
C           ask how many columns required
C           prompt for number of columns
C           "Enter number of columns in table:"
            CALL ASKINT(177,.FALSE.,1,NCOLS)
            IF (NCOLS.GT.20) THEN
C              too many columns in table
C               "Only 20 columns allowed in table"
               CALL DEPRNT(130)
               GOTO 302
            END IF
C           have valid number of columns
C     **********************************************
C     ***       COLUMN HEADER DEFINITION         ***
C     **********************************************
 
 305        CONTINUE
C          "Enter number of lines in column header:"
           CALL ASKINT(202,.TRUE.,1,NCHLIN)
           IF (NCHLIN.GT.10) THEN
C             too many lines in header
C             "Only 10 lines allowed in header"
              CALL DEPRNT(200)
              GOTO 305
           END IF
C          have valid number of lines
C     **********************************************
C     ***        TABLE COLUMN DEFINITION         ***
C     **********************************************
C
            DO 303 I=1,NCOLS
C              go get column definition
               CALL DEFT03(I)
 303        CONTINUE
C
C           end of column definition
C           save header data block
            HEDDAT(1)=NHHLIN
            HEDDAT(2)=NCHLIN
            HEDDAT(3)=NCOLS
C           save last column position
            HEDDAT(4)=COLPOS-1
            HEDDAT(5)=0
            HEDDAT(6)=0
            HEDDAT(7)=0
            HEDDAT(8)=0
C
C
 401        CONTINUE
C           save the table definition
            CALL DEFT98(TNAME,OK)
            IF (.NOT.OK) THEN
C              cannot create the table file
               CALL DEPRNT(447)
C             "Enter name of extract table:"
               CALL DPRMXP(216,TNAME)
               IF (NLEN(TNAME).NE.0) GOTO 401
            END IF
C
C           initialize DEFINE TABLE menu
            CALL GTCLRM(3)
            CALL MNLDT0()
            GOTO 10
C           end of table definition function
         END IF
      ELSE IF (MEN.EQ.2) THEN
C        return to previous level
         RETURN
      ELSE IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') THEN
         RETURN
      ELSE
C       "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      END IF
C
 99   CONTINUE
C
C
      END
C
C     ---------------------------------------------
C
      SUBROUTINE DEFT03(CN)
C     =====================
C
C1    vartype           I4
C1    iostatus          I
C
C2    Subroutine DEFT03 handles the definition
C2    of properties columns for extract of data from
C2    entities in the graphical database.
C2    Current column number passed in CN.
C
      include 'include/menun.inc'
      include 'include/props.inc'
      include 'include/propt.inc'
C
      INTEGER*4 C,GTCELL,NLEN,I,TMEN,TCELL,CN,TCOLW
      INTEGER*2 PRCPNT,PRIPNT,PRPNT
      REAL X,Y
      DOUBLE PRECISION DN
      LOGICAL FOUND,OK,RESPON
      CHARACTER*80 PNAME,TNAME,STRING,TMPT*3,TMPT2*20,TMPT4
      EXTERNAL DCPRNT,DEPRNT,NLEN,FOLDUP,RESPON,
     +         DSPR01,DCPRN2,AEXPRN,ASKINT
C
C
 10   CONTINUE
C     save previous column width
      TCOLW=COLW
C     print prompt for user
      WRITE(UNIT=TMPT,FMT='(I3)') CN
C     "Define parameters for column "
      CALL DCPRN2(150,TMPT)
C     **********************************************
C     ****          COLUMN WIDTH                ****
C     **********************************************
C     column width
 302  CONTINUE
C     prompt for width of column
C     "Enter column width:"
      CALL ASKINT(153,.TRUE.,TCOLW,COLW)
C     have valid column width in COLW
C     **********************************************
C     ****          COLUMN HEADER               ****
C     **********************************************
C     define the column header data
C     read the header data into buffer
C     "Type the column header"
      CALL DCPRNT(151)
C     prompt each line with it's number
      DO 301 I=1,NCHLIN
 307     CONTINUE
         WRITE(UNIT=TMPT4,FMT='(I3,A)') I,'>'
         CALL DPRMX3(279,TMPT4,COLHED(I,CN))
C        must check width against column width
         IF (NLEN(COLHED(I,CN)).GT.COLW) THEN
C           "Column width exceeded"
            CALL DEPRNT(138)
            GOTO 307
         END IF
 301  CONTINUE
C
C     **********************************************
C     ****            DATA SOURCE               ****
C     **********************************************
C     ask for data source
C     "Give source of data - Property or Calculate or Tally (P/C/T)"
 304  CONTINUE
      CALL DPRMXP(40,STRING)
      IF (NLEN(STRING).EQ.0) GOTO 304
      IF( RESPON(STRING(1:1),6) )THEN
C        source is a property
C        ask for property name
         SORCEC=1
C        "Enter name of property to be read:"
 305     CONTINUE
         CALL DPRMXP(297,STRING)
         IF (NLEN(STRING).EQ.0) GOTO 305
C        fold name to upper case
         CALL FOLDUP(STRING)
C        set data source flag to property
         COLDAT(4,CN)=1
C        force compress action to combine
         COMACT(CN)=1
      ELSE IF ( RESPON(STRING(1:1),7) )THEN
C        source is a calculation
         SORCEC=2
 306     CONTINUE
C        "Enter expression:"
         CALL DPRMXP(249,STRING)
         IF (NLEN(STRING).EQ.0) GOTO 306
C        validate the expression
         CALL AEXPRN(STRING,DN,*306)
C        force compress action to null
         COMACT(CN)=0
      ELSE IF ( RESPON(STRING(1:1),8) )THEN
C        Tally column required
         STRING=' '
C        save code for tally
         SORCEC=3
C        force data type to integer
         PDATYP=2
C        force compress action to add
         COMACT(CN)=2
      ELSE
C        not a valid response
C        "Respond with 'P' or 'C' or 'T' "
         CALL DEPRNT(263)
         GOTO 304
      END IF
C     save source name
      COLCON(1,CN)=STRING
 310  CONTINUE
      IF (SORCEC.NE.3) THEN
C        **********************************************
C        ****            DATA TYPE                 ****
C        **********************************************
C        ask for data source
C        "Give type of data -  Text,Integer,Real (T/I/R) "
 308     CONTINUE
         CALL DPRMXP(69,STRING)
         IF (NLEN(STRING).EQ.0) GOTO 308
         IF( RESPON(STRING(1:1),9) )THEN
C           data type is TEXT
            PDATYP=8
         ELSE IF ( RESPON(STRING(1:1),10) ) THEN
C           data type is INTEGER
            PDATYP=2
         ELSE IF ( RESPON(STRING(1:1),11) ) THEN
C           data type is REAL
            PDATYP=4
         ELSE
C           not a valid response
C           "Respond with 'T' or 'I' or 'R'"
            CALL DEPRNT(68)
            GOTO 308
         END IF
      END IF
C     **********************************************
C     ****          DECIMAL PLACES              ****
C     **********************************************
      IF (PDATYP.EQ.2 .OR. PDATYP.EQ.4) THEN
C        number of places of decimal
         IF (PDATYP.EQ.2) THEN
C           INTEGER data
C           prompt for number of digits,default to 1
C           "Enter mandatory number of digits to show:"
 401        CONTINUE
            CALL ASKINT(91,.TRUE.,1,SIGFIG)
            IF (SIGFIG.GT.COLW) THEN
C              cannot fit column
               CALL DEPRNT(446)
               GOTO 401
            END IF
C           have valid number of digits
         ELSE IF (PDATYP.EQ.4) THEN
C           REAL data
C           prompt for number of digits,default to 2
C           "Enter number of decimal places to show:"
 402        CONTINUE
            CALL ASKINT(175,.TRUE.,2,SIGFIG)
            IF (SIGFIG.GT.COLW-2 .OR. SIGFIG.GT.6) THEN
C              cannot fit column
               CALL DEPRNT(446)
               GOTO 402
            END IF
C           have valid number of digits
         END IF
      END IF
C     **********************************************
C     ****      FIELD JUSTIFICATION             ****
C     **********************************************
C     ask for justification
C     "Field justification Left,Centre,Right (L/C/R)? "
 309  CONTINUE
      CALL DPRMXP(73,STRING)
      IF (NLEN(STRING).EQ.0) GOTO 310
      IF( RESPON(STRING(1:1),3) ) THEN
C        justify Left
         FJUST=1
      ELSE IF ( RESPON(STRING(1:1),4) ) THEN
C        justify Centre
         FJUST=4
      ELSE IF ( RESPON(STRING(1:1),5) ) THEN
C        justify Right
         FJUST=7
      ELSE
C        not a valid response
C        "Respond with 'L' or 'C' or 'R'"
         CALL DEPRNT(52)
         GOTO 309
      END IF
C
C     accept settings
C     "Column definition complete"
      CALL DCPRNT(159)
C     column width
      COLDAT(1,CN)=COLW
C     column position
      COLDAT(2,CN)=COLPOS
C     update column position
      COLPOS=COLPOS+COLW+1
C     field justification
      COLDAT(3,CN)=FJUST
C     data source descriptor
      COLDAT(4,CN)=SORCEC
C     data type
      COLDAT(5,CN)=PDATYP
      COLDAT(7,CN)=0
      COLDAT(8,CN)=0
      IF (PDATYP.EQ.2 .OR. PDATYP.EQ.4) THEN
C        integer or real data
C        number of significant figures
         COLDAT(6,CN)=SIGFIG
C        preload the column number in variable form
         WRITE(UNIT=TMPT2,FMT='(A,I3,A)')
     +         '@',CN,'=1'
         CALL CRUNCH(TMPT2)
         CALL AEXPRN(TMPT2,DN,*333)
 333     CONTINUE
         IF (PDATYP.EQ.2) THEN
C           format for integer
            TMPT2='(I'
         ELSE
C           format for real
            TMPT2='(F'
         END IF
C        construct format specifier for output
         WRITE(UNIT=STRING,FMT='(A,I3,A,I3,A)')
     +        TMPT2,COLDAT(1,CN),'.',COLDAT(6,CN),')'
         CALL CRUNCH(STRING)
         COLCON(2,CN)=STRING
      ELSE
C        number of significant characters
         COLDAT(6,CN)=COLDAT(1,CN)
C        format for text output
         COLCON(2,CN)='(A)'
      END IF
C     clear default output for now
      COLCON(3,CN)='   '
C
      END
C
C
      SUBROUTINE DEFT10(FJUST)
C     =========================
C
C1    vartype            I4
C1    iostatus           IO
C
C2    Subroutine DEFT10 controls field justification
C2    for data display in property table columns.
C2    The same justifications are used as those in TEXT.
C
      INTEGER*4 J,JMAP(3,3),VJST,HJST,
     +          LHJST(9),LVJST(9)
      INTEGER*4 OPHPNT(3),OPVPNT(3),I,K,MOD,FJUST
      INTRINSIC MOD
C
      EXTERNAL GTDOMN
C
C
      DATA     ((JMAP(J,K),K=1,3),J=1,3)/1,2,3,4,5,6,7,8,9/,
     1     (LHJST(I),I=1,9)/1,2,3,1,2,3,1,2,3/,
     2     (LVJST(I),I=1,9)/1,1,1,2,2,2,3,3,3/
C
C
C     store dictionary pointers for justifications
C     horizontals only
      OPVPNT(1)=158
      OPVPNT(2)=159
      OPVPNT(3)=160
C
C     Find out the horizontal justification which
C     should be set
      HJST=LHJST(FJUST)
C     Find out the vertical justification which
C     should be set
      VJST=LVJST(FJUST)
C
      VJST=MOD(VJST,3)+1
      FJUST=JMAP(VJST,HJST)
C     overwrite vertical justification with new
      CALL GTDOMN(OPVPNT(VJST),3)
C
C
      END
C
C
      SUBROUTINE DEFT97(TNAME,OK)
C     ===========================
C
C1    vartype            C*(*) L
C1    iostatus             I   O
C
C2    Subroutine DEFT97 reads a property table
C2    definition from file under the directory
C2    /daxcad/ptabl/.
C2    OK returned true if successful.
C
      include 'include/propt.inc'
      include 'include/lfu.inc'
      include 'include/library.inc'
      include 'include/filunit.inc'
      include  'include/fhead.inc'
      include  'include/params.inc'
      include  'include/ftypes.inc'
C
      INTEGER*4 DUMMY(8),I,TBUNIT,J,NLEN1
      LOGICAL EX,OP,OK
      CHARACTER*80 SEQ*8,UNF*8,TNAME*(*)
      EXTERNAL NLEN1
C
C
C      WRITE(10,*) '[DEFT97] Table name:',TBFNAM
      OK=.FALSE.
      DO 20 I=1,8
         DUMMY(I)=0
 20   CONTINUE
C
 10   CONTINUE
 
      FILTYP='PTABLE'
      DAXTYP = 4
C
      CALL OPENNM(TNAME,PARFUN,.TRUE.,OK)
      TBUNIT=PARFUN
      IF(.NOT.OK) RETURN
C###########################################################################
 
C     restore the file header
      CALL RSTFHD(OK)
 
C     file now open and ready for reading
C     read page data block
 
      READ(UNIT=TBUNIT) (PAGDAT(J),J=1,8)
C
C     read dummy data block
      READ(UNIT=TBUNIT) (DUMMY(J),J=1,8)
C
C     read header data block
      READ(UNIT=TBUNIT) (HEDDAT(J),J=1,8)
      NHHLIN=HEDDAT(1)
      NCHLIN=HEDDAT(2)
      NCOLS=HEDDAT(3)
C
C     read dummy data block
      READ(UNIT=TBUNIT) (DUMMY(J),J=1,8)
C
C     read HEADER data
      DO 336 I=1,NHHLIN
         READ(UNIT=TBUNIT) THEADR(I)
 336  CONTINUE
C
C     read column header data
      DO 333 I=1,NCOLS
         DO 338 J=1,NCHLIN
            READ(UNIT=TBUNIT) COLHED(J,I)
 338     CONTINUE
 333  CONTINUE
C
C     read column compression control data
      READ(UNIT=TBUNIT) (COMACT(J),J=1,NCOLS)
C
C     read column data
      DO 334 I=1,NCOLS
         READ(UNIT=TBUNIT) (COLDAT(J,I),J=1,8)
 334  CONTINUE
C
C     read column text data
      DO 335 I=1,NCOLS
         DO 337 J=1,6
            READ(UNIT=TBUNIT) COLCON(J,I)
 337     CONTINUE
 335  CONTINUE
C
      CLOSE(UNIT=TBUNIT,STATUS='KEEP')
CIBM
C      LFU(TBUNIT)=.FALSE.
CIBM
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE DEFT98(TNAME,OK)
C     ===========================
C
C1    vartype            C*(*) L
C1    iostatus             I   O
C
C2    Subroutine DEFT98 writes a property table
C2    definition to file under the directory
C2    /daxcad/ptabl/.
C2    OK returned true if successful.
C
      include 'include/propt.inc'
      include 'include/lfu.inc'
      include 'include/library.inc'
      include 'include/filunit.inc'
      include  'include/params.inc'
C
      INTEGER*4 DUMMY(8),I,TBUNIT,J,NLEN1
      LOGICAL EX,OP,OK
      CHARACTER*80 TBFNAM,SEQ*8,UNF*8,TNAME*(*),FORM*20,EXT*4
C
      EXTERNAL NLEN1
C
      OK=.FALSE.
C     test to see if the directory exists
CSUN|APOLLO
      TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'/ptabl/'//TNAME
CSUN|APOLLO
CIBM
C      TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'\ptabl\'//tname
CIBM
CPC386
C      TBFNAM=LIBRARY(1:NLEN1(LIBRARY))//'\\ptabl\\'//tname
CPC386
C
      DO 20 I=1,8
         DUMMY(I)=0
 20   CONTINUE
C
 10   CONTINUE
      INQUIRE(FILE=TBFNAM,EXIST=EX)
 
      IF (EX) THEN
C        test for access method
         CALL DELETE(TBFNAM,EX)
      ENDIF
 
      CALL FINDU1(TBUNIT,OK)
 
      OPEN(UNIT=TBUNIT,FILE=TBFNAM,ACCESS='SEQUENTIAL',
     +         FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=99)
CIBM
C      LFU(TBUNIT)=.TRUE.
CIBM
C
C##################################################################
C     This is the mod for writing a properties table with a header.
      PARFUN=TBUNIT
      FILTYP='PTABLE'
      CALL MAKFHD()
      CALL SAVFHD(OK)
      IF (.NOT.OK ) RETURN
C###########################################################################
 
 
C     file now open and ready for writing
C     write page data block
      WRITE(UNIT=TBUNIT) (PAGDAT(J),J=1,8)
D      WRITE(UNIT=10,FMT='(8I3)')(PAGDAT(J),J=1,8)
C
C     write dummy data block
      WRITE(UNIT=TBUNIT) (DUMMY(J),J=1,8)
D      WRITE(UNIT=10,FMT='(8I3)')(DUMMY(J),J=1,8)
C
C     write header data block
      WRITE(UNIT=TBUNIT) (HEDDAT(J),J=1,8)
D      WRITE(UNIT=10,FMT='(8I3)')(HEDDAT(J),J=1,8)
C
C     write dummy data block
      WRITE(UNIT=TBUNIT) (DUMMY(J),J=1,8)
D      WRITE(UNIT=10,FMT='(8I3)')(DUMMY(J),J=1,8)
C
C     write HEADER data
      DO 336 I=1,NHHLIN
         WRITE(UNIT=TBUNIT) THEADR(I)
D         WRITE(UNIT=10,FMT=*)THEADR(I)(1:NLEN1(THEADR(I)))
 336  CONTINUE
C
C     write column header data
      DO 333 I=1,NCOLS
         DO 338 J=1,NCHLIN
            WRITE(UNIT=TBUNIT)   COLHED(J,I)
D            WRITE(UNIT=10,FMT=*) COLHED(J,I)(1:NLEN1(COLHED(J,I)))
 338     CONTINUE
 333  CONTINUE
C
C     write column compression control data
      WRITE(UNIT=TBUNIT) (COMACT(J),J=1,NCOLS)
      WRITE(UNIT=10,FMT=*) (COMACT(J),J=1,NCOLS)
C
C     write column data
      DO 334 I=1,NCOLS
         WRITE(UNIT=TBUNIT) (COLDAT(J,I),J=1,8)
D         WRITE(UNIT=10,FMT='(8I3)')(COLDAT(J,I),J=1,8)
 334  CONTINUE
C
C     write column text data
      DO 335 I=1,NCOLS
C           clear unused variables
            COLCON(4,I)=' '
            COLCON(5,I)=' '
            COLCON(6,I)=' '
         DO 337 J=1,6
            WRITE(UNIT=TBUNIT)    COLCON(J,I)
D            WRITE(UNIT=10,FMT=*)  COLCON(J,I)(1:NLEN1(COLCON(J,I)))
 337     CONTINUE
 335  CONTINUE
C
      CLOSE(UNIT=TBUNIT,STATUS='KEEP')
CIBM
C      LFU(TBUNIT)=.FALSE.
CIBM
      OK=.TRUE.
      RETURN
C
 99   CONTINUE
      OK=.FALSE.
C
      END
C
C
      SUBROUTINE DEFVP0()
C     ===================
C1    no arguments required
C2
C2    controls operation of the DEFINE SEARCH-KEY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,DEFS01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the DEFINE viewport routine
      CALL MNLVP0()
      CALL DEFVP1()
C     ensure caller menu cell is not hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE DMPL01(TMIP,PRIPNT)
C     ===============================
C
C1    vartype             I2    I2
C1    iostatus            I     I
C
C2    Subroutine DMPL01 writes a modified property
C2    link structure to file,starting at the
C2    record number PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I,RP,P
      INTEGER*2 PRIPNT,TMIP
C
C     write link record
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (PRLBUF(I,1),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=PRLBUF(I,1)
 500  CONTINUE
CAPOLLO|SUN|PC386
C     set record pointer to current record
      RP=2
 10   CONTINUE
C     get position to write next record
      P=PRLBUF(4,RP-1)
      IF (P.GT.0) THEN
C        write prop list
CIBM
C      WRITE(PRIFLU,REC=P,ERR=999) (PRLBUF(I,RP),I=1,8)
CIBM
CAPOLLO|SUN|PC386
         DO 501 I=1,8
            PRIFIL(I,P)=PRLBUF(I,RP)
 501     CONTINUE
CAPOLLO|SUN|PC386
         RP=RP+1
C        loop until done
         GOTO 10
      END IF
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DMPRC0(PRPNT,PFIELD,PPTEXT,PRCPNT,OK)
C     ================================================
C
C1    vartype            I2      I2    C*(*)   I2   L
C1    iostatus           I       I       I     I    O
C
C2    Subroutine DMPRC0 modifies a property text record
C2    in the property text file,PRPNT passes the related
C2    property control record,PFIELD indicates
C2    the field number referenced in the control block,
C2    the text is contained in PPTEXT,and the text record
C2    written is returned in PRCPNT.
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,PRCPNT,PFIELD
      LOGICAL OK
      CHARACTER*(*) PPTEXT
C
      OK=.FALSE.
CIBM
C      WRITE(PRCFLU,REC=PRCPNT,ERR=999) PRPNT,PFIELD,PPTEXT
CIBM
CAPOLLO|SUN|PC386
CC     store return pointer
      PRCFII(1,PRCPNT)=PRPNT
CC     store field number
      PRCFII(2,PRCPNT)=PFIELD
CC     store the text
      PRCFIL(PRCPNT)=PPTEXT
CAPOLLO|SUN|PC386
      OK=.TRUE.
 999  CONTINUE
C
      END
C
      SUBROUTINE DMPRI0(PRIPNT,OK)
C     ============================
C
C1    vartype             I2    L
C1    iostatus            O     O
C
C2    Subroutine DMPRI0 modifies a property
C2    control record ,pointed to by PRIPNT
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT
      LOGICAL OK
C
      OK=.FALSE.
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (PRIBUF(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=PRIBUF(I)
 500  CONTINUE
CAPOLLO|SUN|PC386
 999  CONTINUE
      OK=.TRUE.
C
      END
C
C
      SUBROUTINE DRPL01(PRIPNT,NRECS,NPROPS,OK)
C     =========================================
C
C1    vartype             I2     I4    I4   L
C1    iostatus            I      O     O    O
C
C2    Subroutine DRPL01 reads a property
C2    link structure from file,using the
C2    record number occupied by the link in PRIPNT.
C2    The number of records in the link are returned
C2    in NRECS,and the number of properties in NPROPS,
C2    OK true if successful.
C
      include 'include/props.inc'
C
      INTEGER*4 I,NRECS,NPROPS,N
      INTEGER*2 PRIPNT,TPRPNT
      LOGICAL OK
C
      OK=.FALSE.
C     read link record
CIBM
C      READ(PRIFLU,REC=PRIPNT,ERR=999) (PRLBUF(I,1),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRLBUF(I,1)=PRIFIL(I,PRIPNT)
 500  CONTINUE
CAPOLLO|SUN|PC386
C     find out how many records and properties
      NRECS=PRLBUF(2,1)/256
      NPROPS=MOD(PRLBUF(2,1)+0,256)
C     find pointer to property list
      TPRPNT=PRLBUF(4,1)
C     read prop list records
      DO 502 N=2,NRECS
CIBM
C      READ(PRIFLU,REC=TPRPNT,ERR=999) (PRLBUF(I,N),I=1,8)
CIBM
CAPOLLO|SUN|PC386
         DO 501 I=1,8
            PRLBUF(I,N)=PRIFIL(I,TPRPNT)
 501     CONTINUE
CAPOLLO|SUN|PC386
         TPRPNT=PRLBUF(4,N)
 502  CONTINUE
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DRPR01(PRPNT,OK)
C     ===========================
C
C1    vartype            I2   L
C1    iostatus           I    O
C
C2    Subroutine DRPR01 reads the property entry
C2    pointed to by PRPNT into the buffers,and returns
C2    completion status in OK.
C2    Only returns master property,or attached property.
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,I2A,I2B
      LOGICAL OK
      EXTERNAL DRPRI0,DRPRC0
C
C     read property index block
      CALL DRPRI0(PRPNT,OK)
      IF (OK) THEN
         I2A=1
         I2B=5
         IF (PRIBUF(1).EQ.I2A .OR. PRIBUF(1).EQ.I2B) THEN
C           got a master or attached property
            CALL DRPRC0(OK)
         ELSE
C           error in data pointer
            OK=.FALSE.
         END IF
      END IF
C
      END
C
C
      SUBROUTINE DRPRC0(OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine DRPRC0 reads the character data
C2    associated with the current property index block
C2    into the buffers.
C
      include 'include/props.inc'
C
      INTEGER*4 J
      LOGICAL OK
C
      OK=.FALSE.
C     read the NAME record
      J=PRIBUF(5)
      IF (J.GT.0) THEN
CIBM
C      READ(PRCFLU,REC=J,ERR=999) PRCBI(1,1),PRCBI(2,1),PRCBUF(1)
CIBM
CAPOLLO|SUN|PC386
         PRCBI(1,1)=PRCFII(1,J)
         PRCBI(2,1)=PRCFII(1,J)
         PRCBUF(1)=PRCFIL(J)
CAPOLLO|SUN|PC386
      ELSE
         RETURN
      END IF
C     read the prompt record
      J=PRIBUF(6)
      IF (J.GT.0) THEN
CIBM
C      READ(PRCFLU,REC=J,ERR=999) PRCBI(1,2),PRCBI(2,2),PRCBUF(2)
CIBM
CAPOLLO|SUN|PC386
         PRCBI(1,2)=PRCFII(1,J)
         PRCBI(2,2)=PRCFII(2,J)
         PRCBUF(2)=PRCFIL(J)
CAPOLLO|SUN|PC386
      ELSE
         PRCBI(1,2)=0
         PRCBI(2,2)=0
         PRCBUF(2)='               '
      END IF
C     read the data record
      J=PRIBUF(7)
      IF (J.GT.0) THEN
CIBM
C      READ(PRCFLU,REC=J,ERR=999) PRCBI(1,3),PRCBI(2,3),PRCBUF(3)
CIBM
CAPOLLO|SUN|PC386
         PRCBI(1,3)=PRCFII(1,J)
         PRCBI(2,3)=PRCFII(2,J)
         PRCBUF(3)=PRCFIL(J)
CAPOLLO|SUN|PC386
      ELSE
         PRCBI(1,3)=0
         PRCBI(2,3)=0
         PRCBUF(3)='               '
      END IF
C     read the spare record
      J=PRIBUF(8)
      IF (J.GT.0) THEN
CIBM
C      READ(PRCFLU,REC=J,ERR=999) PRCBI(1,4),PRCBI(2,4),PRCBUF(4)
CIBM
CAPOLLO|SUN|PC386
         PRCBI(1,4)=PRCFII(1,J)
         PRCBI(2,4)=PRCFII(2,J)
         PRCBUF(4)=PRCFIL(J)
CAPOLLO|SUN|PC386
      ELSE
         PRCBI(1,4)=0
         PRCBI(2,4)=0
         PRCBUF(4)='               '
      END IF
C
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DRPRC1(PCPNT,IPCBUF,PTEXT,OK)
C     ========================================
C
C1    vartype            I2   I2(2)  C*80   L
C1    iostatus           I      O     O     O
C
C2    Subroutine DRPRC1 reads the property text data
C2    from position PCPNT in the property text data file
C2    returning the data in PTEXT,and the pointer elements
C2    in IPCBUF. OK is returned true if the read is
C2    successful.
C
      include 'include/props.inc'
C
      INTEGER*2 PCPNT,IPCBUF(2)
      LOGICAL OK
      CHARACTER*80 PTEXT
C
      OK=.FALSE.
      IF (PCPNT.LT.NPCPOS) THEN
CIBM
C      READ(PRCFLU,REC=PCPNT,ERR=999) IPCBUF(1),IPCBUF(2),PTEXT
CIBM
CAPOLLO|SUN|PC386
C        read the return pointer data
         IPCBUF(1)=PRCFII(1,PCPNT)
         IPCBUF(2)=PRCFII(2,PCPNT)
C        read the text data
         PTEXT=PRCFIL(PCPNT)
CAPOLLO|SUN|PC386
         OK=.TRUE.
      ELSE
C        pointer out of range
         OK=.FALSE.
         WRITE(UNIT=10,FMT=*)'[DRPRC1] Pointer error P=',PCPNT
      END IF
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DRPRI0(PRIPNT,OK)
C     ============================
C
C1    vartype             I2    L
C1    iostatus            O     O
C
C2    Subroutine DRPRI0 reads a property
C2    control record from file,from the
C2    record number PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT
      LOGICAL OK
C
      OK=.FALSE.
C     test for valid record
      IF (PRIPNT.GT.0 .AND. PRIPNT.LT.NPRPOS) THEN
C        read the index block
CIBM
C      READ(PRIFLU,REC=PRIPNT,ERR=999) (PRIBUF(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
         DO 500 I=1,8
             PRIBUF(I)=PRIFIL(I,PRIPNT)
 500     CONTINUE
CAPOLLO|SUN|PC386
         OK=.TRUE.
      ELSE
C        out of range
         WRITE(UNIT=10,FMT=*)'[DRPRI0] Pointer error P=',PRIPNT
         OK=.FALSE.
      END IF
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DRPRI5(PRIPNT,IPBUF1,OK)
C     ===================================
C
C1    vartype             I2    I2(8) L
C1    iostatus            I       O   O
C
C2    Subroutine DRPRI5 reads a property
C2    control record from file,from the
C2    record number PRIPNT,into IPBUF1
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT,IPBUF1(8)
      LOGICAL OK
C
      OK=.FALSE.
C     test for valid record
      IF (PRIPNT.GT.0 .AND. PRIPNT.LT.NPRPOS) THEN
C        read the index block
CIBM
C      READ(PRIFLU,REC=PRIPNT,ERR=999) (IPBUF1(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
         DO 500 I=1,8
             IPBUF1(I)=PRIFIL(I,PRIPNT)
 500     CONTINUE
CAPOLLO|SUN|PC386
         OK=.TRUE.
      ELSE
C        out of range
         WRITE(UNIT=10,FMT=*)'[DRPRI5] Pointer error P=',PRIPNT
         OK=.FALSE.
      END IF
 999  CONTINUE
C
      END
C
      SUBROUTINE DSPR01(PNAME,PRPNT,FOUND)
C     ====================================
C
C1    vartype           C*(*)  I2     L
C1    iostatus            I    O      O
C
C2    Subroutine DSPR01 searches the properties
C2    file system for a master property definition
C2    of the name PNAME.If a match is found
C2    the data for the definition is loaded into
c2    the buffers,and the pointer to the property
C2    index is returned in PRPNT.FOUND indicates the
C2    success of the search.
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,I
      INTEGER*4 J
      LOGICAL FOUND,OK
      CHARACTER*(*) PNAME
      EXTERNAL DRPR01
C
      I=1
 10   CONTINUE
      IF (I.LT.NPRPOS) THEN
C        read the property master data
         CALL DRPR01(I,OK)
         IF (OK) THEN
C           test for correct name
            IF (PRCBUF(1).EQ.PNAME) THEN
               FOUND=.TRUE.
               PRPNT=I
               RETURN
            END IF
         END IF
C        no match found
         I=I+1
         GOTO 10
      END IF
C
      FOUND=.FALSE.
C
      END
C
C
C
      SUBROUTINE DWPL02(TMIP,PRIPNT,OK)
C     =================================
C
C1    vartype             I2    I2   L
C1    iostatus            I     O    O
C
C2    Subroutine DWPL02 writes a property
C2    link structure to file,copying it from
C2    the link currently contained in the buffers,
C2    and associating it with entity TMIP.Returning the
C2    record number occupied by the link in PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I,N
      INTEGER*2 PRIPNT,TPRPNT,TMIP
      LOGICAL OK
      EXTERNAL DWPRI5,DWPRI6
C
C     find number of records in link
      N=PRLBUF(2,1)/256
C     write link records
      DO 500 I=1,N-1
C        set MIP for record
         PRLBUF(3,I)=TMIP
         CALL DWPRI6(PRLBUF(1,I),TPRPNT,OK)
         IF (I.EQ.1) PRIPNT=TPRPNT
 500  CONTINUE
C
C     write last record
      PRLBUF(3,N)=TMIP
      CALL DWPRI5(PRLBUF(1,N),TPRPNT,OK)
C
      END
C
C
      SUBROUTINE DWPL10(TMIP,PRIPNT,OK)
C     =================================
C
C1    vartype             I2    I2   L
C1    iostatus            I     O    O
C
C2    Subroutine DWPL10 copies a property
C2    link structure with all associated properties from
C2    the link currently contained in the buffers,
C2    and associating it with entity TMIP.Returning the
C2    record number occupied by the link in PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*2 PRIPNT,TMIP,I2
      LOGICAL OK
      EXTERNAL DWPL02,DWPL11,DIM501
C
C     copy all the properties
      CALL DWPL11(TMIP,OK)
C     copy the link structure
      CALL DWPL02(TMIP,PRIPNT,OK)
C     set link pointer in the entity
      I2=11
      CALL DIM501(TMIP,I2,PRIPNT,OK)
C
      END
C
C
      SUBROUTINE DWPL11(TMIP,OK)
C     ==========================
C
C1    vartype             I2  L
C1    iostatus            I   O
C
C2    Subroutine DWPL11 copies all properties within a
C2    link structure currently contained in the buffers,
C2    and associating it with entity TMIP.Returning the
C2    completion status in OK.
C
      include 'include/props.inc'
C
      INTEGER*4 NR,NP,NRECS,NPROPS,LP
      INTEGER*2 PRIPN,TMIP,TPRPNT
      LOGICAL OK
      EXTERNAL DWPL02,DIM501,DRPR01,DWPR01
C
C     copy all the properties
C     start to process the link list
C     point to first list record in buffer
      NR=2
      NP=0
C     find number of records and properties
      NRECS=PRLBUF(2,1)/256
      NPROPS=MOD(PRLBUF(2,1)+0,256)
 100  CONTINUE
C     process the current record
      IF (NR.LE.NRECS) THEN
C        process the list
C        initialize pointer into record
         LP=5
 110     CONTINUE
         IF (NP.LT.NPROPS) THEN
            IF (LP.LE.8) THEN
C              read pointer from list
               TPRPNT=PRLBUF(LP,NR)
               IF (TPRPNT.GT.0) THEN
C                 process the property
C                 read the property data
                  CALL DRPR01(TPRPNT,OK)
C                 set MI for this entity
                  PRIBUF(3)=TMIP
C                 write the property data to file
                  CALL DWPR01(TPRPNT,OK)
C                 change link list value to new pointer
                  PRLBUF(LP,NR)=TPRPNT
C                 update count of props copied
                  NP=NP+1
               END IF
C              update pointer into list
               LP=LP+1
C              go try next in list
               GOTO 110
            ELSE
C              go try next record
               NR=NR+1
               GOTO 100
            END IF
         END IF
C        all props have now been copied
      END IF
C     all records have been tested
C
      END
C
C
      SUBROUTINE DWPL20(PRIP,TMIP,OK)
C     ===============================
C
C1    vartype             I2    I2  L
C1    iostatus            I     I   O
C
C2    Subroutine DWPL20 copies a property
C2    link structure as defined at property index position
C2    PRIP with all associated properties
C2    and associating it with entity TMIP.Returning the
C2    completion status in OK.
C
      include 'include/props.inc'
C
      INTEGER*4 NRECS,NPROPS
      INTEGER*2 PRIP,PRIP2,TMIP
      LOGICAL OK
      EXTERNAL DRPL01,DWPL10
C
C     read the link structure to buffers
      CALL DRPL01(PRIP,NRECS,NPROPS,OK)
      IF (OK) THEN
C        copy complete link and data into entity
         CALL DWPL10(TMIP,PRIP2,OK)
      END IF
C
      END
C
      SUBROUTINE DWPR01(PRPNT,OK)
C     ===========================
C
C1    vartype            I2   L
C1    iostatus           O    O
C
C2    Subroutine DWPR01 writes the property entry
C2    pointed to by PRPNT into file from the buffers,
C2    and returns completion status in OK.
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,I2
      INTEGER*4 NLEN,I
      LOGICAL OK
      EXTERNAL DWPRI0,DWPRC0,NLEN
C
C     point to next property index record
      PRPNT=NPRPOS
C     write all character data first
      DO 50 I=1,4
C        write text data to file
         IF (NLEN(PRCBUF(I)).GT.0) THEN
C           write each record to file
            I2=I+4
            CALL DWPRC0(PRPNT,I2,PRCBUF(I),PRIBUF(I+4),OK)
         END IF
 50   CONTINUE
C
C     write control block to file
      CALL DWPRI0(PRPNT,OK)
C
      END
C
C
      SUBROUTINE DWPRC0(PRPNT,PFIELD,PPTEXT,PRCPNT,OK)
C     ================================================
C
C1    vartype            I2      I2    C*(*)   I2   L
C1    iostatus           I       I       I     O    O
C
C2    Subroutine DWPRC0 writes a property text record
C2    into the property text file,PRPNT passes the related
C2    property control record,PFIELD indicates
C2    the field number referenced in the control block,
C2    the text is contained in PPTEXT,and the text record
C2    written is returned in PRCPNT.
C
      include 'include/props.inc'
C
      INTEGER*2 PRPNT,PRCPNT,PFIELD
      LOGICAL OK
      CHARACTER*(*) PPTEXT
C
      OK=.FALSE.
      PRCPNT=NPCPOS
C     store return pointer
CIBM
C      WRITE(PRCFLU,REC=PRCPNT,ERR=999) PRPNT,PFIELD,PPTEXT
CIBM
CAPOLLO|SUN|PC386
      PRCFII(1,PRCPNT)=PRPNT
C     store field number
      PRCFII(2,PRCPNT)=PFIELD
C     store the text
      PRCFIL(PRCPNT)=PPTEXT
CAPOLLO|SUN|PC386
C     update the next position pointer
      NPCPOS=NPCPOS+1
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DWPRI0(PRIPNT,OK)
C     ============================
C
C1    vartype             I2    L
C1    iostatus            O     O
C
C2    Subroutine DWPRI0 writes a property
C2    control record to file,returning the
C2    record number occupied in PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT
      LOGICAL OK
C
C     point to next record
      PRIPNT=NPRPOS
      OK=.FALSE.
C
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (PRIBUF(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=PRIBUF(I)
 500  CONTINUE
CAPOLLO|SUN|PC386
      NPRPOS=NPRPOS+1
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DWPRI5(IPBUF,PRIPNT,OK)
C     ==================================
C
C1    vartype           I2(8)   I2   L
C1    iostatus            I     O    O
C
C2    Subroutine DWPRI5 writes a property
C2    control record to file,returning the
C2    record number occupied in PRIPNT.
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT,IPBUF(8)
      LOGICAL OK
C
      OK=.FALSE.
C     point to next record
      PRIPNT=NPRPOS
C
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (IPBUF(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=IPBUF(I)
 500  CONTINUE
CAPOLLO|SUN|PC386
      NPRPOS=NPRPOS+1
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE DWPRI6(IPBUF,PRIPNT,OK)
C     ==================================
C
C1    vartype           I2(8)   I2   L
C1    iostatus            I     O    O
C
C2    Subroutine DWPRI6 writes a property
C2    control record to file,returning the
C2    record number occupied in PRIPNT.
C2    Acontinuation pointer is set to point
C2    to the next free space in the index file.
C
      include 'include/props.inc'
C
      INTEGER*4 I
      INTEGER*2 PRIPNT,IPBUF(8)
      LOGICAL OK
C
      OK=.FALSE.
C     point to next record
      PRIPNT=NPRPOS
C
C     set continuation pointer
      IPBUF(4)=NPRPOS+1
CIBM
C      WRITE(PRIFLU,REC=PRIPNT,ERR=999) (IPBUF(I),I=1,8)
CIBM
CAPOLLO|SUN|PC386
      DO 500 I=1,8
         PRIFIL(I,PRIPNT)=IPBUF(I)
 500  CONTINUE
CAPOLLO|SUN|PC386
C
      NPRPOS=NPRPOS+1
      OK=.TRUE.
 999  CONTINUE
C
      END
C
C
      SUBROUTINE INPLNK()
C     ===================
C
C1    no arguments required
C
C2    Subroutine INPLNK clears the PROPERTY
C2    LINK buffer space to zeroes.
C
      include 'include/props.inc'
C
      INTEGER*4 I,J
C
      DO 50 I=1,20
         DO 60 J=1,8
            PRLBUF(J,I)=0
 60      CONTINUE
 50   CONTINUE
C
      PRLNAM='                    '
C
      END
C
C
      SUBROUTINE MAJDF1()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the DEFINE mode
C2    of operation is selected from the master menu.
C2    controls operation of the DEFINE function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
      EXTERNAL MNIDEF,DEFP00,GTCLRM,DEFT00,FNDPOS,FNDTOK
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,UNFLAG
C
C     Now activate the DEFINE major option menu
      CALL MNIDEF()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     attach. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C
C     goto default entry point by asking for
C     name of property
C     load token for "Property" option
C      CALL FNDTOK(320,CCMD)
C      MEN=2
C     hilite the "Property" cell
C      CALL FNDPOS(320,CELLN)
C      CALL GTMCHI(MEN,CELLN)
C      GOTO 20
C
 10   CONTINUE
C     tell him what to do
C     "select an option from the minor menu"
      CALL DCPRNT(284)
C     Read a cursor hit to select DEFINE mode
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
         IF (CCMD.EQ.'P') THEN
C           DEFINE PROPERTY option
            CALL DEFP00()
         ELSE IF (CCMD.EQ.'S') THEN
C           DEFINE SEARCH KEY option
            CALL DEFS00()
            MEN=0
         ELSE IF (CCMD.EQ.'T') THEN
C           DEFINE TABLE option
            CALL DEFT00()
         ELSE IF (CCMD.EQ.'B') THEN
C           DEFINE BACKCLOTH option
            CALL DEFB00()
            MEN=0
         ELSE IF (CCMD.EQ.'V') THEN
C           DEFINE viewports option
            CALL DEFVP0()
            MEN=0
         ELSE IF (CCMD.EQ.'M') THEN
C           DEFINE MENU option
            CALL DEFM00()
         ELSE
C           unrecognized DEFINE option
C           "option not yet available"
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
C        "No MAJOR menu option has been selected"
         CALL DEPRNT(24)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
C
      RETURN
      END
C
C
      SUBROUTINE MNIDEF()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the DEFINE major options.
C2
C
      EXTERNAL GTCLRM,GTDMEN,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the DEFINE major options.
C     Hilite the option header
      CALL GTDMHD(32,2)
C
C     Load the options for define
 
C2    P is the token for DEFINE PROPERTY
      CALL GTDMEN(330,2)
 
C2    T is the token for DEFINE TABLE
      CALL GTDMEN(331,2)
 
C2    S is the token for DEFINE SEARCH-KEY
      CALL GTDMEN(335,2)
 
C2    B is the token for DEFINE BACKCLOTH
      CALL GTDMEN(332,2)
C2    V is the token for DEFINE VIEWPORT
      CALL GTDMEN(453,2)
C     M is the token for DEFINE MENU
      CALL GTDMEN(480,2)
C
 
      END
C
C
      SUBROUTINE MNLDF0()
C     ===================
C1    no arguments requierd
C
C2    Enters the options for DEFINE PROPERTY
C2    to menu number 3.
C
      EXTERNAL GTDMEN
C
C     load the data type options
C     "Integer"
      CALL GTDMEN(340,3)
C     "Real"
      CALL GTDMEN(341,3)
C     "Text"
      CALL GTDMEN(342,3)
C     load the name option
C     "Name"
      CALL GTDMEN(343,3)
C
      END
C
C
      SUBROUTINE MNLDKD()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLCKD loads the MODIFY KEYDEF, DEFINE KEY
C2    VERIFY KEY and SAVE KEY options into menu No.3
C2    Intended for use in CREATE KEY DEFINITION
C2
C2    Tokens used here are M, D, V, and S
C2
C     [created 1/8/88 - mlw]
C
      EXTERNAL GTDMEN
C
C2    D is the token for DEFINE KEY
      CALL GTDMEN(502,3)
C2
      END
C
      SUBROUTINE MNLDMN()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLCMN loads the MODIFY MENU, DEFINE GRID,
C2    DEFINE CELL, VERIFY CELL and SAVE MENU options into menu No.3
C2    Intended for use in CREATE MENU
C2
C2    Tokens used here are m, d, D, V, and S
C2
C     [created 7/7/88 - mlw]
C
      EXTERNAL GTDMEN
C
C     M is the token for MODIFY MENU
      CALL GTDMEN(482,3)
C     d is the token for DEFINE GRID
      CALL GTDMEN(483,3)
C2    D is the token for DEFINE CELL.
      CALL GTDMEN(484,3)
C2    V is the token for VERIFY CELL
      CALL GTDMEN(485,3)
C2    S is token for SAVE CELL
      CALL GTDMEN(486,3)
C2
      END
C
 
      SUBROUTINE MNLDT0()
C     ===================
C1    no arguments required
C
C2    Enters the options for DEFINE TABLE
C2    to menu number 3.
C
      EXTERNAL GTDMEN
C
C     load the name option
C     "Name"
      CALL GTDMEN(343,3)
C
      END
C
      SUBROUTINE MNLVP0()
C     ===================
C1    NO ARGS
C
C2    Controls select system menu
C     MAWS and Daxports
      include  'include/viewport.inc'

      CHARACTER TOKEN
      INTEGER*4 TCELL
 
      CALL GTDMEN(516,3)
      CALL GTDMEN(345,3)
 
      IF(MAWS) THEN
          TOKEN = 'M'
      ELSE
          TOKEN = 'D'
      ENDIF
      CALL GTHFMC(3,TOKEN,TCELL)
      CALL GTMCHI(3,TCELL)
      END
C
      SUBROUTINE MODMEN()
C     ===================
C1    no arguments required
C2    modifies an existing menu definition
C2    part of the CREATE MENU code
C2    created 25/7/88 - mlw
C
      include 'include/menug.inc'
      include 'include/entity.inc'
      include 'include/style.inc'
      include 'include/ndata.inc'
 
C
      REAL WDTH,HGHT,SLNT,ANGLE,X,Y
      INTEGER*2 JST,I,J,TMIP,ENTYPE,N
      LOGICAL OK
C
      EXTERNAL MENLOD,DEFG03
C
      CALL PAPCHK(PAPOK,PLIMIT)
      IF (.NOT.PAPOK) RETURN
C
10    CONTINUE
C
C     first load up menu to be modified
      CALL MENLOD()
      CALL MNLDMN()
C
C     then display it
      CALL DEFG03()
C     display the text strings
      WDTH=(INCX/20.0)
      HGHT=THIGT*(WDTH/TWIDTH)
      JST=4
      SLNT=0.0
      ANGLE=0.0
      ENTYPE=TEXT
C
      DO 15 I=1,XN
         DO 20 J=1,YN
            IF (DMENU(I,J).NE.'undefined') THEN
               X=GLIMIT(1)+(INCX*(I-1))+(INCX/2.0)
               Y=GLIMIT(2)+(INCY*(J-1))+(INCY/8.0)
               CALL DEWC85(X,Y,WDTH,HGHT,ANGLE,SLNT,JST,
     +           FONT,COLOUR,LAYER,DMENU(I,J),TMIP,OK)
               CALL ALLDRW(ENTYPE,TMIP)
            END IF
20       CONTINUE
15    CONTINUE
C
C     indicate a partial definition
      PAPOK=.TRUE.
      GRIDOK=.TRUE.
      MENDEF=.TRUE.
C
C     go on as if it was a new one
      END
C
C
      SUBROUTINE SAVMEN(ST)
C     ===================
C1    no arguments required
C2    handles saving the current menu
C2    part of the CREATE MENU code
C2    created 11/7/88 - mlw
C
      include 'include/vntable.inc'
      include 'include/menug.inc'
      include 'include/style.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
      include 'include/library.inc'
C
      CHARACTER*80 TBUFF,MNAME
      REAL X,Y,HGHT,WDTH,SLNT,ANGLE,MAXWID,C(3,3)
      INTEGER*4 DNUM,UNIT,NLEN1,ST
      LOGICAL YESOK,OK,KEEP
      INTEGER*2 I,J,JST,TMIP1,ENTYPE
C
      EXTERNAL YESOK,NLEN1
C                                 
      ST = 0
10    CONTINUE
C     get the name of a file to store it in
      DNUM=58
      CALL DPRMXP(DNUM,MNAME)
      IF (MNAME.EQ.' ') THEN
C         Fall out as no menu specified
          ST = 1
          RETURN
      ENDIF
C     open the file
      CALL SUFFIX(MNAME,'.DMF')
      TBUFF = LIBRARY(1:NLEN1(LIBRARY))//'/dmfdir/'//MNAME
      CALL OPNFFA(TBUFF,UNIT,OK)
      IF (.NOT.OK) GOTO 10
C     null entry indicates he wants to forget it
      IF (MNAME.EQ.' ') RETURN
C
C     display the name in the title box
      HGHT=5.0
      WDTH=TWIDTH*(HGHT/THIGT)
      MAXWID=WDTH*(NLEN1(MNAME))
      IF (MAXWID.GT.(INCX*XN)) THEN
          WDTH=((INCX*XN)-4.0)/(NLEN1(MNAME))
          HGHT=THIGT*(WDTH/TWIDTH)
      END IF
      JST=4
      SLNT=0.0
      ANGLE=0.0
      X=GLIMIT(1)+(XN*INCX/2.0)
      Y=GLIMIT(4)+(5.0-(HGHT/2.0))
      CALL DEWC85(X,Y,WDTH,HGHT,ANGLE,SLNT,JST,FONT,COLOUR,
     +            LAYER,MNAME,TMIP1,OK)
      ENTYPE=TEXT
      CALL ALLDRW(ENTYPE,TMIP1)
C
C     save the number of cells in each axis
      WRITE(UNIT,*) XN,YN
C     save cell increments
      WRITE(UNIT,'(2(F10.3))')INCX,INCY
C     save the origin and extents of the grid
      WRITE(UNIT,'(4(F10.3))') GLIMIT(1),GLIMIT(2),GLIMIT(3),GLIMIT(4)
C
      CALL I3M(C)
      C(1,1) = 1.0
      C(2,2) = C(1,1)
      C(3,3) = C(1,1)
C     save transformation
      DO 12 I=1,3
         WRITE(UNIT,'(4(F10.3))',ERR=99) C(I,1),C(I,2),
     +                                   C(I,3)
 12   CONTINUE
C     save the text strings
      DO 15 I=1,XN
         DO 20 J=1,YN
            WRITE(UNIT,'(A)') DMENU(I,J)
20       CONTINUE
15    CONTINUE
C
C     close the file and keep it
      KEEP=.TRUE.
      CALL CLOSUN(UNIT,KEEP,OK)
      IF (OK) MENDEF=.FALSE.
C
C      reset the buffer for next menu
       DO 5 J=1,50
          DO 6 I=1,50
             DMENU(J,I)='undefined'
6         CONTINUE
5      CONTINUE
 99   CONTINUE
      END
C
      SUBROUTINE UPR001(TMIP,OK)
C     ==========================
C
C1    vartype            I2  L
C1    iostatus           I   O
C
C2    Subroutine UPR001 updates the property link
C2    attached to entity TMIP,by testing the return
C2    pointer for a match,and copying all property
C2    data over to the entity,and creating
C2    a new link if necessary.If no properties attached,
C2    then no action is taken.
C2    Completion status returned in OK
C
      include 'include/props.inc'
      include 'include/nbuff.inc'
C
      INTEGER*2 TMIP,TMIP2,PRPNT
      LOGICAL OK
      EXTERNAL DIR500,DRPRI0,DWPL20
C
C     read MI data for entity
      CALL DIR500(TMIP,OK)
C     find link pointer
      PRPNT=IMBUFF(11)
      IF (PRPNT.GT.0) THEN
C        go check out the return pointer
C        read link control record
         CALL DRPRI0(PRPNT,OK)
C        get return pointer
         TMIP2=PRIBUF(3)
         IF (TMIP2.GT.0 .AND. TMIP2.NE.TMIP) THEN
C           must copy properties from TMIP2 to TMIP
            CALL DWPL20(PRPNT,TMIP,OK)
         END IF
      END IF
C
      END
C
C
      SUBROUTINE UPR066(TMIP,OK)
C     ==========================
C
C1    vartype            I2  L
C1    iostatus           I   O
C
C2    Subroutine UPR066 tests for a property link
C2    on the entity pointed to by TMIP,and if
C2    one exists,updates each of the blank properties
C2    by taking the action defined within the property definition.
C2    OK returned true if successful.
C
      include 'include/propt.inc'
      include 'include/props.inc'
C
      INTEGER*2 TMIP,TPRPNT,NUM
      INTEGER*4 I,DTYPE,ACTION,NLEN,NP
      LOGICAL OK,NONE
      EXTERNAL EXPR01,UPR001,EXPR02,EXPR03,ATTP51,DWPRC0
      EXTERNAL DWPRI0,DMPRC0,NLEN

		NUM=7
C
C     test for existance of property link
      CALL EXPR01(TMIP,NONE,OK)
      IF (.NOT.NONE) THEN
C        properties are attached to entity,ensure
C        they are wholly owned.
         CALL UPR001(TMIP,OK)
C        now test for required data
         CALL EXPR03(TMIP,NP,OK)
C        NP contains the number of properties
C        ENTPRP contains the list of props,and data
C        from include block PROPT.INC
C
         DO 20 I=1,NP
            IF (NLEN(ENTPRP(2,I)).EQ.0) THEN
C              no data in field
C              search for named property
               CALL EXPR02(ENTPRP(1,I),ENTPRP(2,I),
     +                     DTYPE,TPRPNT,NONE,OK)
C              prompt for new value
               CALL ATTP51(DTYPE,ACTION,OK)
C              write text record,or modify existing
               IF (OK.AND.NONE) THEN
C                 no data present add text record
                  CALL DWPRC0(TPRPNT,INT2(7),PRCBUF(3),PRIBUF(7),OK)
C                 modify index record for property
                  CALL DMPRI0(TPRPNT,OK)
               ELSE IF (OK) THEN
C                 write modified text to file
                  CALL DMPRC0(TPRPNT,INT2(7),PRCBUF(3),PRIBUF(7),OK)
               END IF
            END IF
 20      CONTINUE
      END IF
C
      END
C
C
      SUBROUTINE UPR100(P1,OK)
C     ========================
C
C1    vartype            I2 L
C1    iostatus           I  O
C
C2    Subroutine UPR100 updates the property links
C2    attached to all entities starting from position P1
C2    in the MI file,by testing the return
C2    pointer for a match,and copying all property
C2    data over to the entity,and creating
C2    a new link if necessary.If no properties attached,
C2    then no action is taken.
C2    Completion status returned in OK
C
      include 'include/masti.inc'
C
      INTEGER*2 TMIP,P1
      LOGICAL OK
      EXTERNAL UPR001
C
C     cycle through the contents of the database
C     from position P1.
      DO 50 TMIP=P1,NMIPOS-1
C        update properties on the entity
         CALL UPR001(TMIP,OK)
 50   CONTINUE
C
      END
C
C
      SUBROUTINE UPR101(OK)
C     =====================
C
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine UPR101 updates the property links
C2    attached to all entities currently identified
C2    in the select scratch file.
C2    By testing the return pointer in the PR file
C2    for a match,and copying all property
C2    data over to the entity,and creating
C2    a new link if necessary.If no properties attached,
C2    then no action is taken.
C2    Completion status returned in OK
C
      include 'include/masti.inc'
      include 'include/swind.inc'
C
      INTEGER*2 TMIP,DFPP,SPA
      INTEGER*4 I
      REAL X,Y
      LOGICAL OK
      EXTERNAL UPR001,RSCRF
C
C     cycle through the contents of the scratch file
      DO 50 I=1,NDATA
C        read from scratch file
         CALL RSCRF(I,TMIP,X,Y,DFPP,SPA)
C        update properties on the entity
         CALL UPR001(TMIP,OK)
 50   CONTINUE
C
      END
C
C
      SUBROUTINE VERC01()
C     ===================
C1    no arguments required
C2    used if user wants to verify the string assigned to a cell
C2    part of the CREATE MENU code
C2    created 11/7/88 - mlw
C
      include 'include/vntable.inc'
      include 'include/menug.inc'
      include 'include/menun.inc'
C
      CHARACTER*80 TBUFF,ANS*1,C*1,ENTRY*20
      REAL X,Y
      INTEGER*4 DNUM
C
10    CONTINUE
      DNUM=553
      CALL CPRINT(DICT01(DNUM))
      CALL TCURS(C,X,Y)
C
C     check for another menu request
      IF (MEN.EQ.3.OR.MEN.EQ.2) RETURN
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') RETURN
C
      CELLX=INT((X-GLIMIT(1))/INCX)+1
      CELLY=INT((Y-GLIMIT(2))/INCY)+1
      IF(X.LT.GLIMIT(1) .OR. X .GT. GLIMIT(3) 
     +  .OR. Y .LT. GLIMIT(2) .OR. Y.GT.GLIMIT(4)) THEN 
          RETURN
      END IF
      ENTRY=DMENU(CELLX,CELLY)
      IF (ENTRY.EQ.'undefined') THEN
           DNUM=560
           CALL EPRINT(DICT01(DNUM))
      ELSE
           DNUM=562
           CALL DCPRN2(DNUM,ENTRY)
      END IF
C
C     this one's modal
      GOTO 10
C
      END
C
