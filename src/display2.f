C
C     @(#)  412.1 date 6/11/92 display2.f 
C
C
C     Filename    : display2.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:35:38
C     Last change : 92/06/11 14:28:46
C
C     Copyright : Practical Technology Limited  
C     File :- display2.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GETWIN(SX1,SY1,SX2,SY2,OK)
C     SUBROUTINE ISOSNP(XW,YW,XS,YS)
C     SUBROUTINE LOADG(GEOM,FP)
C     SUBROUTINE LOADGH(TW,GEOM,FP,LAST)
C     SUBROUTINE MLAYER(DATA,NDATA,STATUS)
C     SUBROUTINE MLOAD6(AR1,AR2)
C     SUBROUTINE MNIDIS
C     SUBROUTINE MWIN()
C     SUBROUTINE MWIN1(WXC,WYC)
C     SUBROUTINE MWIN2(OK)
C     SUBROUTINE OLDVPT()
C     SUBROUTINE POLY16(POINT,TW,USET,M,DRWTYP,OK)
C     SUBROUTINE POLY24(POINT,USET,M,DRWTYP,OK)
C     SUBROUTINE POLYWN(HPX,HPY,X,Y,N,OK)
C     SUBROUTINE PREL(POINT,USET,M,BUFF1,BUFFER,OK)
C     SUBROUTINE PRODRW(ENTP)
C     SUBROUTINE REDRAW()
C     SUBROUTINE REGEND()
C     SUBROUTINE RSCRF(POS,TMIP,X,Y,DFP,SPA)
C     SUBROUTINE SCALEW()
C     SUBROUTINE SETM16(MATRIX)
C     SUBROUTINE SETM24(MATRIX)
C     SUBROUTINE SHOPAP()
C     SUBROUTINE SLAYER()
C     SUBROUTINE SNAPXY(XW,YW,XS,YS)
C     SUBROUTINE TEXBOX(X1,X2,X3,X4,X5,X6,XMIN,YMIN,XMAX,YMAX)
C     SUBROUTINE TORGBR(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     SUBROUTINE TORGTL(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     SUBROUTINE TORGTR(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     SUBROUTINE TXTORG(FUNC,X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     SUBROUTINE UCODET(TEMP,SLA,JUST,NCHAR)
C     SUBROUTINE XYNORG(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     SUBROUTINE ZOMEXT()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE GETWIN(SX1,SY1,SX2,SY2,OK)
C     =====================================
C1    vartype            R   R   R   R  L
C1    iostatus           O   O   O   O  O
C
C2    subroutine GETWIN returns the diagonal
C2    coords of a window on the screen,obtained from
C2    user input by generation of a rubber banded
C2    rectangle on the screen.
C2    If the operation has been successful the logical
C2    flag OK is returned TRUE.
C                          
      include 'include/gpr.ins.inc'
      include 'include/viewport.inc'

      REAL SX1,SY1,SX2,SY2
C                            
      INTEGER*2 CX,CY,CURPOS(2)
      INTEGER*4 AX1,AY1,AX2,AY2,C,MENUN,CELLN,SOURCE
C
      LOGICAL OK,ST
C
      CHARACTER CTEXT*80,CCMD*1
C                                   
      EXTERNAL WRKHIT,MENHIT,SCURS,APRREC
C
C     tell the user what to do
 10   CONTINUE
      CALL DCPRNT(88)
      CALL SCURS(C,SX1,SY1)
C     Save the cureent cursor pos before doing the viepwort update
C     in case the menu is updated and move the cursor
C      CALL GPR_$INQ_CP(CX,CY,ST)
      CX = INT(SX1)
      CY = INT(SY1)
C     set the current viewport
      CALL SETDP(SX1,SY1,.FALSE.)
C     Now restore the cursor position in case it was updated
      CALL GPR_$MOVE(CX,CY,ST)                
      CURPOS(1) = CX
      CURPOS(2) = CY
      CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
C     test for hit in wohome
      CALL WNHIT(SX1,SY1,OK)
      IF (.NOT.OK) THEN
C        test for menu cell hit
         CALL MENHIT(INT(SX1),INT(SY1),MENUN,CELLN,CTEXT,CCMD,OK)
         IF (OK) THEN
C           another menu hit,must exit this function
            OK=.FALSE.
            RETURN
         ELSE
C           must be invalid area of screen
            CALL DEPRNT(117)
            GOTO 10
         END IF
      END IF
C
C     go let him define the screen window
      CALL APRREC(SX1,SY1,SX2,SY2,CCMD,SOURCE,OK)
C     return with appropriate status
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE ISOSNP(XW,YW,XS,YS)
C     ==============================
C
C1    vartype           R  R  R  R
C1    iostatus          I  I  O  O
C
C     snap to point on the isometric grid
C     what a pity it almost works
      include 'include/ndata.inc'
      REAL XW,YW,XS,YS,BLEFTX,BLEFTY,
     +X1,Y1,X2,Y2,L1,L2
      INTEGER XSIGN, YSIGN      
      LOGICAL XODD, YODD  
      INTRINSIC MOD,NINT,ABS,INT
C
C     set the direction of the hit point 
C     from the GRID origin
C
C     first the X direction
C
      IF(XW .LT. GRIDOX) THEN
        XSIGN = -1
      ELSE
        XSIGN = 1
      ENDIF
C
C     now the Y direction
  
      IF(YW .LT. GRIDOY) THEN
        YSIGN = -1
      ELSE
        YSIGN = 1
      ENDIF
C
C     get the position of the box containing the hit point
C
C     get the bottom left x co-ord
      BLEFTX = GRIDOX + INT((XW - GRIDOX)/HISOSX)*HISOSX
C     get the bottom left y co-ord
      BLEFTY = GRIDOY + INT((YW - GRIDOY)/HISOSY)*HISOSY
C
C     find if the base point of this box is on a node point
C     or on the centre of a tessellation diamond 
C
C
      YODD = (ABS(MOD((INT((YW-GRIDOY)/HISOSY)),2)) .EQ. 1) 
      XODD = (ABS(MOD((INT((XW-GRIDOX)/HISOSX)),2)) .EQ. 1) 

      IF(XODD .AND . YODD .OR. .NOT. XODD .AND. .NOT. YODD) THEN 
C       right we've hit a grid point o.k.
        X1 = BLEFTX
        Y1 = BLEFTY
        X2 = BLEFTX + XSIGN * HISOSX
        Y2 = BLEFTY + YSIGN * HISOSY
C       get the square of distance between the points
        L1 = (XW-X1)*(XW-X1)+(YW-Y1)*(YW-Y1)        
        L2 = (XW-X2)*(XW-X2)+(YW-Y2)*(YW-Y2)        
        IF(L1 .GT. L2) THEN 
          XS = X2
          YS = Y2
        ELSE
          XS = X1
          YS = Y1
        ENDIF
      ELSE
C       right we've hit the centre of a tessellation diamond 
        X1 = BLEFTX
        Y1 = BLEFTY + YSIGN * HISOSY
        X2 = BLEFTX + XSIGN * HISOSX
        Y2 = BLEFTY
C       get the square of distance between the points
        L1 = (XW-X1)*(XW-X1)+(YW-Y1)*(YW-Y1)        
        L2 = (XW-X2)*(XW-X2)+(YW-Y2)*(YW-Y2)        
        IF(L1 .GT. L2) THEN 
          XS = X2
          YS = Y2
        ELSE
          XS = X1
          YS = Y1
        ENDIF
      ENDIF
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE LOADG(GEOM,FP)
C     =========================
      REAL FP(6,4),GEOM(3,4)
      INTEGER I,J
C
      DO 10 I=1,4
         DO 10 J=1,3
            GEOM(J,I)=FP(J,I)
 10   CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE LOADGH(TW,GEOM,FP,LAST)
C     ==================================
      REAL FP(6,4),GEOM(3,4),TW
      INTEGER I,J
      LOGICAL LAST
C
      DO 10 J=1,3
          GEOM(J,1)=FP(J,1)
          GEOM(J,2)=FP(J,2)
          GEOM(J,3)=TW*(GEOM(J,2)-GEOM(J,1))
C         the last segment is different
          IF (LAST) THEN
              GEOM(J,4)=GEOM(J,3)
          ELSE
              GEOM(J,4)=TW*(FP(J,3)-GEOM(J,2))
          ENDIF
 10   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE MLAYER(DATA,NDATA,STATUS)
C     ====================================
C
C
 
      include 'include/layer.inc'
      include 'include/masti.inc'
 
      INTEGER*4 DATA(1:256),NDATA,I,J,STATUS
 
C      WRITE(10,*) '[MLAYER] STATUS= ',STATUS
 
C      WRITE(10,*) '[MLAYER] DATA= ',(DATA(I),I=1,NDATA)
C      WRITE(10,*) '[MLAYER] NDATA= ',NDATA
      DO 10 I=1,NDATA
 
 
 
          DO 20 J=1,FINTOT
 
              IF(MOD(FINLAY(J),256).EQ.DATA(I) ) THEN
 
C                 overwrite the current layer to be hidden or shown
 
                  FINLAY(J) = DATA(I)+STATUS
 
                  GOTO 10
 
              ENDIF
 
20        CONTINUE
 
          IF(CLAYER.NE.DATA(I) ) THEN
 
 
              FINTOT=FINTOT+1
 
              FINLAY(FINTOT)=DATA(I)+STATUS
 
          ENDIF
 
 10   CONTINUE
 
C      WRITE(10,*) '[MLAYER] FINLAY= ',(FINLAY(I),I=1,FINTOT )
 
      END
 
 
      SUBROUTINE MLOAD6(AR1,AR2)
C     =========================
C1                    R(6),R(6)
      INTEGER*4 I
      REAL AR1(6),AR2(6)
C
      DO 10 I=1,6
         AR2(I)=AR1(I)
 10   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE MNIDIS
C     =================
C1    No arguments required.
C
C2    Initializes and fills the display control menu.
C2
C2    Tokens used here are CHAR(149),q,R,M,Z,S and U.
C2
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/menun.inc'
      include 'include/layer.inc'
      include 'include/viewport.inc'
      include 'include/gtxt2.inc'
C
      INTEGER*4 I4,TCELL
      INTRINSIC CHAR
C
      EXTERNAL GTDMEN,GTCLRM,GTDMCH
C
C     Clear the display control menu area.
      CALL GTCLRM(4)
C
C     Load the display control menu.
C
      IF ( DISLAY ) THEN
C2       q is the token for MASTER MENU.
         CALL GTDMEN(50,4)
C2       R is the token for REDRAW.
         CALL GTDMEN(51,4)
C2       M is the token for PAN WINDOW.
         CALL GTDMEN(52,4)
C2       Z is the token for ZOOM IN.
         CALL GTDMEN(53,4)
C2       S is the token for ZOOM OUT.
         CALL GTDMEN(54,4)
C2       P is the token for ZOOM PREVIOUS
         CALL GTDMEN(55,4)
C2       E is the token for ZOOM EXTENTS
         CALL GTDMEN(56,4)
C2       V is the token for Layer Control
         CALL GTDMEN(57,4)
C2       U is the token for GRID Option
         CALL GTDMEN(59,4)
         IF (SETGRD) CALL GTDMCH(59,4)
C
      ELSE
C        write header for layer control.
C         CALL GTDMHD(57,4)
         CALL GTDMEN(50,4)
C        show current work layer.
         I4=CLAYER
         CALL GTDMWI(60,4,I4)
C        write name of layer directly.
         IF(LOADED) THEN
             CALL GTDMEN(336,4)
         ELSE
             CALL GTPMEN(LNAME(CLAYER),'N',4,2)
         ENDIF
C        List command.
         CALL GTDMEN(61,4)
C        Show command.
         CALL GTDMEN(62,4)
C        Show all command.
         CALL GTDMEN(63,4)
C        Hide command.
         CALL GTDMEN(64,4)
C        Hide all command.
         CALL GTDMEN(65,4)
C        Display menu return command.
         CALL GTDMEN(66,4)
C        set the logical to tell layer is active
         IF (SHOW) THEN
             CALL GTHFMC(4,'S',TCELL)
             CALL GTMCHI(4,TCELL)
         ENDIF
         IF (SHOWAL) THEN
             CALL GTHFMC(4,'T',TCELL)
             CALL GTMCHI(4,TCELL)
         ENDIF
         IF (HIDE) THEN
             CALL GTHFMC(4,'H',TCELL)
             CALL GTMCHI(4,TCELL)
         ENDIF
         IF (HIDEAL) THEN
             CALL GTHFMC(4,'I',TCELL)
             CALL GTMCHI(4,TCELL)
         ENDIF
      END IF
C
      END
C
C
C-------------------------------------------------------------
C
 
      SUBROUTINE MWIN()
C     =================
C1       No arguments needed
C
C2       MWIN moves the current viewport by the vector
C2       indicated by the user
C
      include   'include/wtov.inc'
      include   'include/menun.inc'
      include   'include/journal.inc'
C
      REAL SXS,SYS,SXF,SYF,WXS,WYS,WXF,WYF,XT,YT,
     +     XMIN,XMAX,YMIN,YMAX,SXT,SYT
      INTEGER*4 C,MENUN,NLEN
      CHARACTER BUFF*84,RUNFMT*84
      LOGICAL OK
C
      EXTERNAL WORLD,REGEND,WRKHIT,OLDVPT,WRTJRN
C
C     go try pan by property first
C      CALL MWIN2(OK)
C      IF (OK) RETURN
C     Go ask for the first PAN reference point.
      CALL DCPRNT(124)
 5    CALL SCURS(C,SXS,SYS)
C
      IF ( C.EQ.156 ) THEN
        CALL MWIN2(OK)
        IF (OK) RETURN
        GOTO 5
      ELSE
C     Check inside work area
      CALL WRKHIT(SXS,SYS,OK)
      IF ( .NOT. OK ) THEN
C        Did  not hit  workspace may be menu
         CALL MENHIT(INT(SXS),INT(SYS),MENUN,CELLN,CTEXT,CCMD,OK)
C        But it did hit a menu option
         IF ( OK ) THEN
            RETURN
         ELSE
            CALL DEPRNT(117)
            GOTO 5
         END IF
      END IF
C
      CALL SC2WO(SXS,SYS,WXS,WYS)
C     indicate with a cross
      CALL CROSS(SXS,SYS)
C
C     Go ask for target point for move.
      CALL DCPRNT(125)
 10   CALL SCURS(C,SXF,SYF)
C
C     Check inside work area,if not abort this function
      CALL WRKHIT(SXF,SYF,OK)
      IF ( .NOT. OK ) THEN
C        Did  not hit  workspace may be menu
         CALL MENHIT(INT(SXF),INT(SYF),MENUN,CELLN,CTEXT,CCMD,OK)
C        But it did hit a menu option
         IF ( OK ) THEN
C           Get rid off first cross
            CALL CROSS(SXS,SYS)
            RETURN
         ELSE
            CALL DEPRNT(117)
            GOTO 10
         END IF
      END IF
C
      CALL SC2WO(SXF,SYF,WXF,WYF)
C     indicate with a cross
      CALL CROSS(SXF,SYF)
C
C
      XT=WXS-WXF
      YT=WYS-WYF
C
      XMIN=WXMIN+XT
      YMIN=WYMIN+YT
      XMAX=WXMAX+XT
      YMAX=WYMAX+YT
C
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate the new world to screen transformation system
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C
C     erase the two reference crosses
      CALL CROSS(SXS,SYS)
      CALL CROSS(SXF,SYF)
C
C     regenerate the display
      CALL REGEND()
      IF(JOURON) THEN
C       write a journal entry if required
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A45)',ERR=99)
     +  '(A', NLEN(JRNCMD(11)),
     1  ',''('',F14.6,'','',F14.6,'')'')'
        WRITE(UNIT=BUFF,FMT=RUNFMT,ERR=99) JRNCMD(11),WXS-WXF,WYS-WYF
        CALL WRTJRN(0.0,0.0,'m',BUFF,0)
C
      ENDIF
      ENDIF
 99   CONTINUE
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE MWIN1(WXC,WYC)
C     =========================
C
C1    vartype           R   R
C1    iostatus          I   I
C
C2    MWIN1 places the centre of the viewport
C2    on the point WXC,WYC,maintaining the current
C2    view scale
C
      include   'include/wtov.inc'
C
      REAL WXC,WYC,XX,YY,XMIN,XMAX,YMIN,YMAX
      EXTERNAL WORLD,REGEND,OLDVPT
C
C     calculate new window limits
      XX=WXMAX-WXMIN
      YY=WYMAX-WYMIN
      XMIN=WXC-XX/2
      YMIN=WYC-YY/2
      XMAX=WXC+XX/2
      YMAX=WYC+YY/2
C
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate the new world to screen transformation system
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
C
C     regenerate the display
      CALL REGEND()
C
 99   CONTINUE
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE MWIN2(OK)
C     ====================
C
C2    Special function 3 is used
C2    for the FIND & PAN function
C
      include 'include/psearch.inc'
      include 'include/props.inc'
      include 'include/entity.inc'
      include 'include/wrkdat.inc'
      include 'include/nbuff.inc'
C
      INTEGER*2 IPCBUF(2),IPIBUF(8),P1,P2,P3,PMIP,TENT
      INTEGER*4 NLEN,DNUM
      REAL  M(3,3),WXC,WYC
      LOGICAL OK,DELETE
      CHARACTER*80 PTEXT
      EXTERNAL NLEN
C
C     ask the user for the data match
C     to use in searching
      DNUM=486
C      "Enter search data:"
      CALL DPRMXP(DNUM,SRCDAT)
      CALL FOLDUP(SRCDAT)
      IF (NLEN(SRCDAT).EQ.0) THEN
        OK=.FALSE.
        RETURN
      END IF
C
C     get start point for search
      P1=MOD(NPSRCH,NPCPOS)+1
      NPSRCH=P1
 5    CONTINUE
C     read property text data record
      CALL DRPRC1(NPSRCH,IPCBUF,PTEXT,OK)
      IF (OK) THEN
C       compare in upper case
        CALL FOLDUP(PTEXT)
        IF (SRCDAT.EQ.PTEXT) THEN
C         data matches,check it is attached
          P2=IPCBUF(1)
C         read the PMI record
          CALL DRPRI5(P2,IPIBUF,OK)
          IF (IPIBUF(1).EQ.5) THEN
C           attached property ok
C           test that match is with data
            IF (NPSRCH.EQ.IPIBUF(7)) THEN
C             it is a data match
C             check field name also matches
              P3=IPIBUF(5)
              CALL DRPRC1(P3,IPCBUF,PTEXT,OK)
              CALL FOLDUP(PTEXT)
              IF (SRCKEY.EQ.PTEXT) THEN
C               found a match,show this one
                PMIP=IPIBUF(3)
                GOTO 55
              END IF
            END IF
          END IF
        END IF
      END IF
C     point to next record
      NPSRCH=MOD(NPSRCH,NPCPOS)+1
C     if not seen before,go do next one
      IF (NPSRCH.NE.P1) GOTO 5
C     get here if no match found for request
      CALL DEPRNT(654)
      CALL CPRINT(SRCKEY)
      CALL CPRINT(SRCDAT)
      OK=.TRUE.
      RETURN
C
 55   CONTINUE
C     PMIP points to matching entity,
c     go get it,and establish viewing position for it
      CALL ALLRD(PMIP,TENT,M,DELETE)
C     skip deleted entities
      IF (DELETE) THEN
         NPSRCH=MOD(NPSRCH,NPCPOS)+1
         GOTO 5
      ENDIF
C     find out what kind of entity,and centre it
C     on screen at the current view scale.
      IF (TENT.EQ.LINE) THEN
C       use mid-point of line
        WXC=(RDBUFF(1)+RDBUFF(4))/2
        WYC=(RDBUFF(2)+RDBUFF(5))/2
      ELSE IF (TENT.EQ.ARC) THEN
C       use centre of arc
        WXC=RDBUFF(1)
        WYC=RDBUFF(2)
      ELSE IF (TENT.EQ.SYMBI) THEN
C       use centre of symbol
        WXC=(RWORK(1,1)+RWORK(4,1))/2
        WYC=(RWORK(2,1)+RWORK(5,1))/2
      ELSE IF (TENT.EQ.COMPI) THEN
C       use centre of component
        WXC=(RWORK(1,1)+RWORK(4,1))/2
        WYC=(RWORK(2,1)+RWORK(5,1))/2
      ELSE
C       no action
        DNUM=487
C     'Cannot Display matching entity'
        CALL DEPRNT(DNUM)
        OK=.TRUE.
        RETURN
      END IF
C     must now have a view centre to apply
      CALL MWIN1(WXC,WYC)
      OK=.TRUE.
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE OLDVPT()
C     ===================
C
C2    Subroutine OLDVPT swaps the
C2    viewport limts of the current
C2    view and those of the previous view
C
      include 'include/wtov.inc'
C
      EXTERNAL RSWAP
C
      CALL RSWAP(WXMIN,LWXMIN)
      CALL RSWAP(WYMIN,LWYMIN)
      CALL RSWAP(WXMAX,LWXMAX)
      CALL RSWAP(WYMAX,LWYMAX)
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE POLY16(POINT,TW,USET,M,DRWTYP,OK)
C     ============================================
C
C2      Subroutine DRWSPL  draws a spline
C2      If  DRWTYP =  0 ... to the screen.
C2                   -1 ... to the plot file.
C2                   >0 ... to the "write" file.
C2      If DRWTYP>0, the absolute of DRWTYP is the file unit number.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
C
      REAL M(3,3),BUFFER(6,4),XP,YP,XP1,YP1,
     +     MATRIX(4,4),GEOM(3,4),OX,OY,INC,TBUF(6)
      REAL TW,NX,NY,DISTXY,DIST,OPX,OPY
      DOUBLE PRECISION DN
      CHARACTER*80 STRING,INPL
      INTEGER*2 POINT
      INTEGER*4 I,K,NLEN,DRWTYP
      LOGICAL OK,USET,LAST,VISBAS
      EXTERNAL DISTXY
C
C
      LAST=.FALSE.
      VISBAS=LAST
      CALL SETM16(MATRIX)
C
C     load up the first four points
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,1),OK)
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,2),OK)
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,3),OK)
C     set the start point for plotting
      OX=BUFFER(1,1)
      OY=BUFFER(2,1)
      OPX=OX
      OPY=OY
C     load up for the first point
      CALL LOADGH(TW,GEOM,BUFFER,LAST)
      NX=BUFFER(1,2)
      NY=BUFFER(2,2)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
 10   CONTINUE
C
C     Move the values one up to accomodate
C     the new one to be read.
      DO 5 I=1,2
         CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
 5    CONTINUE
C
 
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,3),OK)
      CALL LOADGH(TW,GEOM,BUFFER,LAST)
C
      OPX=BUFFER(1,1)
      OPY=BUFFER(2,1)
      NX=BUFFER(1,2)
      NY=BUFFER(2,2)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
      IF ( POINT .NE. 0 ) GOTO 10
C
C
      DO 6 I=1,2
         CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
 6    CONTINUE
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,3),OK)
      CALL LOADGH(TW,GEOM,BUFFER,LAST)
      OPX=BUFFER(1,1)
      OPY=BUFFER(2,1)
      NX=BUFFER(1,2)
      NY=BUFFER(2,2)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
      DO 7 I=1,2
         CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
 7    CONTINUE
      LAST=.TRUE.
      CALL LOADGH(TW,GEOM,BUFFER,LAST)
      OPX=BUFFER(1,1)
      OPY=BUFFER(2,1)
      NX=BUFFER(1,2)
      NY=BUFFER(2,2)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
C     set visibility flag
      CALL SETVIS(VISBAS)
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE POLY24(POINT,USET,M,DRWTYP,OK)
C     =========================================
C
C2      If  DRWTYP =  0 ... to the screen.
C2                   -1 ... to the plot file.
C2                   >0 ... to the "write" file.
C2      If DRWTYP>0, it is the scratch file unit number.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
C
      REAL M(3,3),BUFFER(6,4),XP,YP,XP1,YP1,
     +     MATRIX(4,4),GEOM(3,4),OX,OY,INC,TBUF(6),sinc
      REAL DISTXY,NX,NY,DIST,OPX,OPY
      INTEGER*2 POINT
      INTEGER*2 OPOINT
      INTEGER*4 I,K,DRWTYP
      CHARACTER*80 INPL
      LOGICAL OK,USET,VISBAS
      LOGICAL USET1
      EXTERNAL DISTXY
C
      VISBAS=.FALSE.
      CALL SETM24(MATRIX)
C
C     This is done to ensure the the B-spline
C     start at the first point
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,1),OK)
C     The weighting is done by using the first point
C     twice to ensure it passes through it
      CALL MLOAD6(BUFFER(1,1),BUFFER(1,2))
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,3),OK)
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,4),OK)
C
C     get increment in screen coords
C
      CALL LOADG(GEOM,BUFFER)
      OX=BUFFER(1,1)
      OY=BUFFER(2,1)
      OPX=OX
      OPY=OY
C     Draw the special case first
      NX=BUFFER(1,3)
      NY=BUFFER(2,3)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
 10   CONTINUE
C
C     Move the values one up to accomodate
C     the new one to be read.
      DO 5 I=1,3
         CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
 5    CONTINUE
C
      OPOINT = POINT 
      CALL PREL(POINT,USET,M,RDBUFF,BUFFER(1,4),OK)
      CALL LOADG(GEOM,BUFFER)
C
      OPX=BUFFER(1,2)
      OPY=BUFFER(2,2)
      NX=BUFFER(1,3)
      NY=BUFFER(2,3)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
      IF ( POINT .NE. 0 ) GOTO 10
C
      DO 6 I=1,3
         CALL MLOAD6(BUFFER(1,I+1),BUFFER(1,I))
 6    CONTINUE
C     load the last point up to do complete draw.

CDHR
C     PREL does not use USET to modify the point since rdbuff
C     allready contains this modified point Thus always false
C     on the last one.
CDHR
      USET1 = OPOINT.GT.4.AND.USET
      CALL PREL(POINT,USET1,M,RDBUFF,BUFFER(1,4),OK)
      CALL LOADG(GEOM,BUFFER)
      OPX=BUFFER(1,2)
      OPY=BUFFER(2,2)
      NX=BUFFER(1,3)
      NY=BUFFER(2,3)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
      CALL BLOAD1(GEOM)
      OPX=BUFFER(1,2)
      OPY=BUFFER(2,2)
      NX=BUFFER(1,3)
      NY=BUFFER(2,3)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
      CALL BLOAD2(GEOM)
      OPX=BUFFER(1,3)
      OPY=BUFFER(2,3)
      NX=BUFFER(1,4)
      NY=BUFFER(2,4)
      DIST=DISTXY(NX,NY,OPX,OPY)
C     draw the spline in
      CALL BSPLIN(GEOM,MATRIX,OX,OY,DIST,VISBAS,DRWTYP)
C
C     set visibility flag
      CALL SETVIS(VISBAS)
C
      END
      SUBROUTINE POLYWN(HPX,HPY,X,Y,N,OK)
C     ===================================
C1                       R   R R(N),I L
C1                       I   I  I I I O
C
C2    POLYWN checks the point HPX,HPY and confirms if the point
C2    lies within the polygon window which is held in
C2    array X(N),Y(N) where N gives the number of point which
C2    define the window
C2    OK is returned .true. is the point does lie within the
C2    polyogn window else .false.
C
      INTEGER*4 N,K,NP,I,J,HK1,ST
      REAL HPX,HPY,X(N),Y(N),X1,Y1
      REAL XYP(50,2)
      DOUBLE PRECISION C1,C2,C3,D1,D2,D3,XP,YP
      LOGICAL OK,CHKLN,SAME
C
      INTRINSIC DBLE
      EXTERNAL CHKLN,SAME
C
 
C     Creating horizontal line through the point in question
      CALL DCVL14(DBLE(HPX),DBLE(HPY),DBLE(HPX+100.0),DBLE(HPY),
     +      C1,C2,C3)
C
      K=1
C
C     Loop for the number of lines (equal to the number of points)
C     in the window
      DO 40 I=1,N,1
C
C        NP set to the next point to be used for the window
         NP=MOD(I,N)+1
C
C        Generate line in vector form
         CALL DCVL14(DBLE(X(I)),DBLE(Y(I)),
     +               DBLE(X(NP)),DBLE(Y(NP)),D1,D2,D3)
C        find the intersection with main line
         CALL DVC0P5(C1,C2,C3,D1,D2,D3,XP,YP,OK)
C        found an intersection
         IF ( OK ) THEN
            X1=REAL(XP)
            Y1=REAL(YP)
C           Test for legitimate point ie between end-points
            IF (CHKLN(X(I),X(NP),X1).AND.
     +          CHKLN(Y(I),Y(NP),Y1) ) THEN
               XYP(K,1)=X1
               XYP(K,2)=Y1
               K=K+1
            END IF
         END IF
 40   CONTINUE
C
      IF ( K .GT. 1 ) THEN
C
C     Sort by x coordinate
            DO 50 I=1,K-2
               DO 60 J=I+1,K-1
                  IF ( XYP(J,1) .LT. XYP(I,1) ) THEN
                     CALL RSWAP(XYP(J,1),XYP(I,1))
                     CALL RSWAP(XYP(J,2),XYP(I,2))
                  END IF
 60            CONTINUE
 50         CONTINUE
C
C        Number of lines which would be generated
         HK1=((K-1)/2)
C
         J=1
         OK=.FALSE.
C        check the point against all the generated lines
         DO 70 I=1,HK1,1
            IF ( CHKLN(XYP(J,1),XYP(J+1,1),HPX) .AND.
     +           CHKLN(XYP(J,2),XYP(J+1,2),HPY) ) THEN
                  OK=.TRUE.
            END IF
            J=J+2
C
 70      CONTINUE
C
      ELSE
C        No lines genrated therefore no intersection
C        and not within the boundary.
         OK=.FALSE.
C
      END IF
C
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE PREL(POINT,USET,M,BUFF1,BUFFER,OK)
C     ==============================================
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
C
      REAL M(3,3),BUFFER(6),XP,YP,BUFF1(6)
      INTEGER*2 POINT
      LOGICAL OK,USET
C
C      WRITE(10,*) 'POINT,BUFF1(1),BUFF1(2)',
C     +             POINT,BUFF1(1),BUFF1(2)
      IF ( USET ) THEN
         CALL NEWXY(BUFF1(1),BUFF1(2),XP,YP,M)
         BUFF1(1)=XP
         BUFF1(2)=YP
      END IF
C
      CALL MLOAD6(BUFF1,BUFFER)
C
      IF ( POINT .NE. 0 ) THEN
         CALL DBR500(POINT,OK)
         POINT=IDBUFF(3)
         IF ( .NOT. OK ) RETURN
      END IF
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE PRODRW(ENTP)
C     =======================
C1    VARTYPE            I2
C1    IOSTAT             I
C
C2    This routine will draw a portion of the profile
C2    which is stored in the scratch file
C
      include 'include/hdata.inc'
      include 'include/entity.inc'
      include 'include/viewport.inc'
C
      LOGICAL ERASE
      REAL BUFF1(6),BUFF2(6),X,Y
      INTEGER*2 TMIP,ENT,ENTP
      INTEGER*4 TFONT
 
      ERASE = .FALSE.
      IF(ENTP.LT.0) THEN
          ENTP = -ENTP
          ERASE = .TRUE.
      ENDIF
      TFONT = 1
C     get data
      READ(UNIT=HUNIT,REC=ENTP,ERR=99) TMIP,ENT,X,Y,
     +        BUFF1(1),BUFF1(2),BUFF1(4),BUFF1(5),BUFF1(6),
     1        BUFF2(1),BUFF2(2),BUFF2(4),BUFF2(5),BUFF2(6)
 
 
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set viewporting draw code please
          DDCODE = 2
      ENDIF
      IF(.NOT.ERASE) THEN
C         higligh the profile
          CALL ROPXOR()
          CALL HFONT(.TRUE.)
C
C         draw the entity

          IF(ENT.EQ.LINE) THEN
 
              CALL DRWFLW(BUFF1(1),BUFF1(2),BUFF1(4),BUFF1(5),TFONT)
 
          ELSE IF(ENT.EQ.ARC) THEN
 
               CALL DRWFAW(BUFF1(1),BUFF1(2),BUFF1(4),
     +                    BUFF1(5),BUFF1(6),TFONT)
 
          ENDIF
      ELSE
C         draw out the highligted lines
          CALL ALLDRW(ENT,TMIP)
      ENDIF
 
99    CONTINUE
C     reset all factors
      CALL HFONT(.FALSE.)
      CALL ROPREP()
      DDCODE = 0
 
      END
 
C
C---------------------------------------------------------------------
C
      SUBROUTINE REDRAW()
C     ===================
C1    no arguments required
C2
C2    clears and redraws the current display
C2    based upon what is stored in the display file
C2    using the currently set view scale,and view port
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/entity.inc'
      include   'include/wtov.inc'
      include   'include/swind.inc'
      include   'include/viewport.inc'
      include   'include/cross.inc'
      include   'include/journal.inc'
C
      INTEGER*2 TMIP,NCHAR,ENT,DFPK
      INTEGER*2 DPF,I1
      REAL      X,Y,RAD,X3,Y3,Z(4),W(4)
      REAL X1,Y1,X2,Y2,XC,YC,RADIUS,SANG,EANG,SLT
      LOGICAL OK,OP,DSTOP
C
      EXTERNAL CLEAR,BELL,DRAWLW,RDISPF,RAD
      EXTERNAL DISFRA,DRWFLW,DRWFAW,WRTJRN
C
C     clear the graphics screen
      CALL SNAQR()
C     reset cross file pointer
      CRSPTR = 1
      CALL CLEAR()
C     draw the drawing frame
      CALL DISFRA()
C     set pointer to valid screen flags,since all have been erased
C      VNDATA=NDATA+1
C
      IF( LDFILE(CVPN) .GT. 1 ) THEN
         DFPK=0
 10      CONTINUE
            DFPK=DFPK+1
            CALL RDISPF(DFPK,ENT,TMIP,OK)
C 
            IF ( TMIP .GT. 0 )    CALL ALLDRW(ENT,TMIP)
C
            IF(MOD(DFPK,100).EQ.0) THEN
C               check keyboard stopper.
                IF(REDRAW_INTERRUPT) THEN
                    IF(DSTOP('Q',493)) GOTO 99
                ENDIF
            ENDIF
C
         IF ( DFPK .LT. LDFILE(CVPN)-1 ) GOTO 10
      END IF
C
99    CONTINUE
      DRAWN=.TRUE.
      IF ( SETGRD ) CALL GRID1()
C     draw anything in the hatch file
 
 
C     Draw in any profiles
      CALL DRWPRF()
C     draw the little view on the thing
      IF(CVPN.GT.0) CALL DRWVPD()
      CALL SNREL()
C     write out a journal entry
      IF(JOURON) CALL WRTJRN(0.0,0.0,'m',JRNCMD(8),0)
C
      END
 
 
 
C
C======================================================================
C
C
 
 
      SUBROUTINE REGEND()
C     ===================
C1    no arguments required
C2
C2    clears the graphics screen,and redraws
C2    based upon a complete regeneration of the
C2    display from the contents of the database.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/ndata.inc'
      include   'include/menun.inc'
      include   'include/entity.inc'
      include   'include/wtov.inc'
      include   'include/swind.inc'
      include   'include/viewport.inc'
      include   'include/cross.inc'
C
      INTEGER*2 TMIP,P3,ENT
      REAL      RAD,X,Y,SLT
      REAL X1,Y1,X2,Y2,XC,YC,RADIUS,SANG,EANG
      INTEGER*4 I,J,K
      INTEGER*2 ENTYPE,NCHAR,TVPN
      LOGICAL OK, DSTOP
C
      EXTERNAL CLEAR,LFSETS,DISFRA,GRID1,DIR500,ALLDRW,ADDISP
C
      CALL SNAQR()
C     save current world coordinates back to storage
      CALL TRNDP()
C     reset cross file pointer
      CRSPTR = 1
      CALL  CLEAR()
C     reset the line font parameters
      CALL LFSETS()
C     show the drawing frame
      CALL DISFRA()
C
      IF ( NMIPOS .EQ. 1 ) GOTO 89
C     Reset display file pointer to one
      VPDFIL = .FALSE.
      LDFILE(CVPN)=1
C     Set master index position to one
      TMIP=0
 10   TMIP=TMIP+1
      CALL DIR500(TMIP,OK)
      IF ( VLAYER(IMBUFF(4)) ) THEN
C         check that it is not a deleted entity,
C         or a component master.
          IF (IMBUFF(1).EQ.100) GOTO 11
          IF (IMBUFF(1).EQ.COMPM .OR. IMBUFF(1).EQ.SYMBM) GOTO 11
          ENT=IMBUFF(2)
          CALL ALLDRW(ENT,TMIP)
C         if visable add to display file
          IF ( DISPV ) THEN
C             add it now
              CALL ADDISP(TMIP,ENT,P3,OK)
          ENDIF
          IF(MOD(TMIP,100).EQ.0) THEN
C             check keyboard stopper.
              IF(REDRAW_INTERRUPT) THEN
                  IF(DSTOP('Q',493)) GOTO 99
             ENDIF
          ENDIF
 11       CONTINUE
      END IF
      IF ( TMIP .LT. (NMIPOS-1) ) GOTO 10
C 
99    CONTINUE
C
89    CONTINUE
      DRAWN=.TRUE.
      IF ( SETGRD ) CALL GRID1()
C     Draw in any profiles
      CALL DRWPRF()
C     Viewporting falgs
      VPDFIL = .FALSE.
      CALL SNREL()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE RSCRF(POS,TMIP,X,Y,DFP,SPA)
C     ======================================
C1    vartype          I4   I2  R R I2  I2
C1    iostatus         I    O   O O O   O
C
      include 'include/swind.inc'
C
      REAL X,Y
      INTEGER*4 POS
      INTEGER*2 TMIP,DFP,SPA
C
      IF ( POS .LE. NDATA ) THEN
         READ(UNIT=SWINDU,REC=POS) TMIP,X,Y,DFP,SPA
      END IF
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SCALEW()
C     =================
C1       No arguments needed
 
C2       SCALE moves the current viewport by the vector
C2       indicated by the user
C
      include   'include/wtov.inc'
      include   'include/menun.inc'
      include   'include/journal.inc'
C
C
      REAL SXS,SYS,WXS,WYS,SCAL,
     +     XMIN,XMAX,YMIN,YMAX,A(1:3,1:3)
      INTEGER*4 C,NLEN,MENUN
      LOGICAL OK
      CHARACTER*20 INPT,BUFF*84,RUNFMT*84
      DOUBLE PRECISION DSCAL
      INTRINSIC REAL
C
      EXTERNAL SCURS,WRKHIT,MENHIT,CROSS,NLEN,AEXPRN,SC2WO,
     +         SCAP2D,NEWXY,OLDVPT,WORLD,REGEND,WRTJRN
C
C     go ask him for centre of zoom area
      CALL DCPRNT(124)
 5    CALL SCURS(C,SXS,SYS)
      CALL WRKHIT(SXS,SYS,OK)
      IF ( .NOT. OK ) THEN
C        Did  not hit  workspace may be menu
         CALL MENHIT(INT(SXS),INT(SYS),MENUN,CELLN,CTEXT,CCMD,OK)
C        But it did hit a menu option
         IF ( OK ) THEN
C           Get rid off first cross
            RETURN
         ELSE
            CALL DEPRNT(117)
            GOTO 5
         END IF
C
      END IF
      CALL CROSS(SXS,SYS)
C
C
 10   CONTINUE
      CALL DPRMXP(126,INPT)
C
      IF ( NLEN(INPT).EQ.0 ) THEN
         SCAL=3.0
      ELSE
         CALL AEXPRN(INPT,DSCAL,*10)
         IF ( DSCAL .LT. 0.0 ) THEN
            RETURN
         END IF
         SCAL=REAL(DSCAL)
      END IF
C
      CALL SC2WO(SXS,SYS,WXS,WYS)
      CALL SCAP2D(WXS,WYS,SCAL,SCAL,A)
      CALL NEWXY(WXMIN,WYMIN,XMIN,YMIN,A)
      CALL NEWXY(WXMAX,WYMAX,XMAX,YMAX,A)
C     save current viewport limits
      CALL OLDVPT()
C     set extents flag
      ZOMLIM=.FALSE.
C     generate new viewport limits
      CALL WORLD(XMIN,YMIN,XMAX,YMAX)
      CALL CROSS(SXS,SYS)
C
      CALL REGEND()
C
      IF(JOURON) THEN
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A45)',ERR=99)
     +  '(A', NLEN(JRNCMD(10)),
     1  ',''('',F14.6,'','',F14.6,'','',F14.6,'')'')'
         WRITE(UNIT=BUFF,FMT=RUNFMT,ERR=99) JRNCMD(10),WXS, WYS, SCAL 
         CALL WRTJRN(0.0,0.0,'m',BUFF,0)
       ENDIF
C
 99   CONTINUE
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SETM16(MATRIX)
C     =========================
      REAL MATRIX(4,4)
      INTEGER*4 I,J
C
      MATRIX(1,1)= 2.0
      MATRIX(2,1)=-2.0
      MATRIX(3,1)= 1.0
      MATRIX(4,1)= 1.0
C
      MATRIX(1,2)=-3.0
      MATRIX(2,2)= 3.0
      MATRIX(3,2)=-2.0
      MATRIX(4,2)=-1.0
C
      MATRIX(1,3)= 0.0
      MATRIX(2,3)= 0.0
      MATRIX(3,3)= 1.0
      MATRIX(4,3)= 0.0
C
      MATRIX(1,4)= 1.0
      MATRIX(2,4)= 0.0
      MATRIX(3,4)= 0.0
      MATRIX(4,4)= 0.0
C
C      DO 1 I=1,4
C         DO 1 J=1,4
C            MATRIX(I,J)=MATRIX(I,J)/6.0
C 1    CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SETM24(MATRIX)
C     =========================
      REAL MATRIX(4,4)
      INTEGER*4 I,J
C
      MATRIX(1,1)=-1.0
      MATRIX(2,1)= 3.0
      MATRIX(3,1)=-3.0
      MATRIX(4,1)= 1.0
C
      MATRIX(1,2)= 3.0
      MATRIX(2,2)=-6.0
      MATRIX(3,2)= 3.0
      MATRIX(4,2)= 0.0
C
      MATRIX(1,3)=-3.0
      MATRIX(2,3)= 0.0
      MATRIX(3,3)= 3.0
      MATRIX(4,3)= 0.0
C
      MATRIX(1,4)= 1.0
      MATRIX(2,4)= 4.0
      MATRIX(3,4)= 1.0
      MATRIX(4,4)= 0.0
C
      DO 1 I=1,4
         DO 1 J=1,4
            MATRIX(I,J)=MATRIX(I,J)/6.0
 1    CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SHOPAP()
C     ===================
C
C1    no arguments required
C
C2    Subroutine SHOPAP is a utility used
C2    by the CREATE DRAWING function for
C2    display of the currently set drawing
C2    sheet.
C
      EXTERNAL ZOMEXT,CHGP05
C
C     set new paper to world scale,and limits
      CALL CHGP05()
C     show the new paper position
      CALL ZOMEXT()
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE SLAYER()
C     ================
C1    no arguments required
C2
C2    provides the layer control facility,by selecting
C2    visible layers of the drawing.
C
      include   'include/masti.inc'
      include   'include/style.inc'
      include   'include/menun.inc'
      include   'include/macro.inc'
      include   'include/layer.inc'
      include   'include/viewport.inc'
      include   'include/menpop.inc'
C
      INTEGER*4 C,K,TMEN,TCELL,IX,IY,DATA(1:256),NDATA,I,NLEN,LP,
     +          LENG,LEN,LENGTH,LASTLY,FIRLAY,J,XCELL
      INTEGER*4 SDATA(1:255),SNDATA
      INTEGER*2 TCVPN
      INTEGER*2 TPVPN

      INTEGER*4 LCOUNT
      REAL X,Y
      CHARACTER*14 OLIN,NUM*3,INPL*100,COMMA*1

      CHARACTER*50 TEXTO(256)
      CHARACTER*20 NAME

      LOGICAL OK,REG,FIRST,BATCH,CVERFY
      LOGICAL LOOP
      INTRINSIC LEN
C
      EXTERNAL SCURS,GTCLRW,WRKHIT,DEPRNT,MENHIT,CPRINT,GTMCHI,
     +         CPRMXP,NLEN,STRNUM,ADDLAY,GTCLRC,GTPMEN,DELLAY,
     +         REGEND,BCHAR,CRUNCH,GTMCLO
C
      LOOP = .FALSE.
      BATCH=.FALSE.
      NDATA=0
 10   CONTINUE
C      IF(.NOT.MACOP.OR.GINPUT) THEN
C         CALL DCPRNT(392)
C         CALL SCURS(C,X,Y)
CC
C         IF (ERRMSG) THEN
CC           clear the message and flag
C            CALL GTCLRW(2)
C            ERRMSG=.FALSE.
C         END IF
CC
C         CALL WRKHIT(X,Y,OK)
C
C         IF ( OK ) THEN
C            CALL DEPRNT(129)
C            GOTO 10
C         END IF
CC
C         IX=INT(X+0.5)
C         IY=INT(Y+0.5)
C         CALL MENHIT(IX,IY,TMEN,TCELL,CTEXT,CCMD,OK)
C         IF ( .NOT. OK ) THEN
C            CALL DEPRNT(117)
C            GOTO 10
C         END IF
CC        User has hit another menu so we have to leave thsi one
CC        fast
C         IF ( TMEN .NE. 4 ) THEN
C            CALL DEPRNT(128)
C            CALL DCPRNT(129)
C            GOTO 10
C         END IF
CC
C         IF  ( CCMD .EQ. ' ' ) THEN
C            CALL DEPRNT(131)
C            GOTO 10
C         END IF
CC
C      ELSE
CC        macro set standard menu & cell numbers.
         TMEN=MEN
         TCELL=CELLN
C      END IF
C
C         CALL GTMCHI(TMEN,TCELL)
 
      IF ( CVERFY(CCMD,'SHIT') ) THEN
 
 
          IF(POPUP) THEN
C             set to loop and reexecute
              LOOP = .TRUE.
          ELSE
              BATCH=.TRUE.
              IF(.NOT.LOADED) THEN
                  CALL GTCLRC(4,2)
C                 load with accept
                  CALL GTDMEN(336,4)
                 LOADED=.TRUE.
              ENDIF
          ENDIF
 
      ENDIF
 13   CONTINUE
      IF ( CCMD .EQ. 'N' ) THEN
C        work layer by name.
 11      CONTINUE
         CALL DPRMXP(438,INPL)
C        blank line quit.
         IF ( NLEN(INPL) .EQ. 0 ) GOTO 98
C        set into upper case.
         CALL FOLDUP(INPL)
C        send active length.
         LENGTH=NLEN(INPL)
C        get layer number.
         CALL FINDLN(INPL(1:LENGTH),DATA(1),OK)
C        Coundn't find layer number for given name.
         IF ( .NOT. OK ) THEN
            CALL DEPRNT(414)
            GOTO 11
         END IF
C        set contruction layer to same.
         CLAYER=DATA(1)
C
C        check if this is visible.
         IF ( .NOT. VLAYER(CLAYER) )THEN
            VLAYER(CLAYER)=.TRUE.
            IF ( TLAYER(CLAYER) .GT. 0 ) THEN
               NDATA=1
               CALL ADDLAY(DATA,NDATA)
            END IF
         END IF
C
         CCMD = 'N'
         CALL GTCLRC(TMEN,TCELL)
C        show current work layer
         I=CLAYER
         CALL GTDMWI(60,4,I)
         CALL GTCLRC(TMEN,TCELL)
         CALL GTPMEN(LNAME(CLAYER),'N',TMEN,TCELL)
         GOTO 98
C
      ELSE IF ( CCMD .EQ. 'C' ) THEN
C        New working layer being selected by number.
         IF(MVPACT.AND.CVPN.GT.0) THEN
C             draw out current work layer number
              CALL DRWVPD()
         ENDIF
C        main loop
 20      CONTINUE
         CALL DPRMXP(132,INPL)
C        abort option
         IF ( NLEN(INPL).EQ.0 ) GOTO 98
C
         CALL STRNUM(INPL,DATA,NDATA)
C        no data returned ?
         IF ( NDATA .EQ. 0) THEN
            CALL DEPRNT(134)
            GOTO 20
         END IF
C        check range of layer.
         IF ( DATA(1) .LT. 0 .OR. DATA(1) .GT. 255 ) THEN
            CALL DEPRNT(134)
            GOTO 20
         END IF
C        set work layer to first valid layer.
         CLAYER=DATA(1)
C
         IF ( .NOT. VLAYER(CLAYER) )THEN
            VLAYER(CLAYER)=.TRUE.
            IF ( TLAYER(CLAYER) .GT. 0 ) THEN
               NDATA=1
C              Ensure that we update before calling addlay
               CALL SAVLAY(CVPN,OK) 
               CALL ADDLAY(DATA,NDATA)
            END IF
         END IF
C
         IF(POPUP) THEN
C           if command came from popup then update
            CCMD = 'C'
            CALL GTHFMC(4,CCMD,TCELL)
            TMEN = 4
         ENDIF
C        
         IF(TCELL.GT.0) THEN
C             clear them cell
              CALL GTCLRC(TMEN,TCELL)
         ENDIF
C        show current work layer, if the display menu is up
         IF(.NOT. DISLAY) THEN
            I=CLAYER
            CALL GTDMWI(60,4,I)
            IF(.NOT.LOADED ) THEN
               CALL GTCLRC(TMEN,TCELL-1)
               CALL GTPMEN(LNAME(CLAYER),'N',TMEN,TCELL-1)
            ENDIF
         ENDIF
         GOTO 98
 
      ELSE IF ( CCMD .EQ. 'H' ) THEN
C        Hide selected layers.
C
         CALL DLAYER(.FALSE.)
         IF(.NOT.LOOP) THEN
C            is this from a popup or not
             IF( HIDEAL ) THEN
                  CALL GTHFMC(4,'I',TCELL)
                  CALL GTMCLO(4,TCELL)
                  HIDEAL=.FALSE.
              ELSE IF (HIDE) THEN
                  CALL GTHFMC(4,'H',TCELL)
                  CALL GTMCLO(4,TCELL)
                  HIDE=.FALSE.
                  GOTO 99
             ENDIF
         ENDIF
 30      CALL DPRMXP(135,INPL)
C
         IF ( NLEN(INPL).EQ.0 ) THEN
             IF (TCELL.GT.0) CALL GTMCLO(TMEN,TCELL)
             GOTO 99
         ENDIF
         CALL STRNUM(INPL,DATA,NDATA)
         IF ( NDATA .EQ. 0) THEN
            CALL DEPRNT(134)
            GOTO 30
         END IF
         DO 35 I=1,NDATA
            IF ( DATA(I) .LT. 0 .OR. DATA(I) .GT. 255 ) THEN
               CALL DEPRNT(134)
               GOTO 30
            END IF
 35      CONTINUE
C        set work layer.
         CALL MLAYER (DATA,NDATA,256)
         HIDE=.TRUE.
 
      ELSE IF ( CCMD .EQ. 'S' ) THEN
C        Add selected layers
C
         CALL DLAYER(.TRUE.)
         IF(.NOT.LOOP) THEN
              IF( SHOWAL ) THEN
                  CALL GTHFMC(4,'T',XCELL)
                  CALL GTMCLO(4,XCELL)
                  SHOWAL=.FALSE.
              ELSE IF (SHOW) THEN
                  CALL GTHFMC(4,'S',XCELL)
                  CALL GTMCLO(4,XCELL)
                  SHOW=.FALSE.
                  GOTO 99
              ENDIF
         ENDIF
C 
 40      CONTINUE
         CALL DPRMXP(136,INPL)
         IF ( NLEN(INPL).EQ.0 ) THEN
              IF (TCELL.GT.0) CALL GTMCLO(TMEN,TCELL)
              GOTO 99
         ENDIF
         CALL STRNUM(INPL,DATA,NDATA)
C
         IF ( NDATA .EQ. 0) THEN
            CALL DEPRNT(134)
            GOTO 40
         END IF
         DO 45 I=1,NDATA
            IF ( DATA(I) .LT. 0 .OR. DATA(I) .GT. 255 ) THEN
               CALL DEPRNT(134)
               GOTO 40
            END IF
 45      CONTINUE
         CALL MLAYER (DATA,NDATA,0)
         SHOW=.TRUE.
C 
C 
      ELSE IF ( CCMD .EQ. 'T' ) THEN
C        show all layers
C 
         CALL DLAYER(.TRUE.)
         IF( SHOWAL) THEN
             IF(.NOT.LOOP) THEN
C                only do this for layer accept
                 CALL GTMCLO(4,TCELL)
                 SHOWAL=.FALSE.
             ENDIF
         ELSE
             IF(.NOT.LOOP) THEN
C                only do this for layer accept
                 IF( HIDEAL) THEN
                    CALL GTHFMC(4,'I',XCELL)
                    CALL GTMCLO(4,XCELL)
                    HIDEAL=.FALSE.
                 ENDIF
                 IF( SHOW) THEN
                     CALL GTHFMC(4,'S',XCELL)
                     CALL GTMCLO(4,XCELL)
                     SHOW=.FALSE.
                 ENDIF
             ENDIF
C            set showal for immediate execution
             SHOWAL=.TRUE.
             J=0
C            build up layers to be shown
             DO 202 I=0,255
               IF ( TLAYER(I).GT.0 ) THEN
                   J=J+1
                   DATA(J)=I
                ENDIF
 202         CONTINUE
             CALL MLAYER(DATA,J,0)
         ENDIF
 
      ELSE IF ( CCMD .EQ. 'I' ) THEN
C        hide all layers
         CALL DLAYER(.FALSE.)
         IF (HIDEAL) THEN
             IF(.NOT.LOOP) THEN
C                only do this for layer accept
                 CALL GTMCLO(4,TCELL)
                 HIDEAL = .FALSE.
             ENDIF
         ELSE
             IF(.NOT.LOOP) THEN
C                only do this for layer accept
                 IF( SHOWAL) THEN
                     CALL GTHFMC(4,'T',XCELL)
                     CALL GTMCLO(4,XCELL)
                     SHOWAL=.FALSE.
                 ENDIF
                 IF( HIDE) THEN
                     CALL GTHFMC(4,'H',XCELL)
                     CALL GTMCLO(4,XCELL)
                     HIDE=.FALSE.
                 ENDIF
              ENDIF
C             set layers for hiding please
              J=0
              DO 200 I=0,255
                  IF ( VLAYER(I) ) THEN
                      J=J+1
                      DATA(J)=I
                  ENDIF
 200          CONTINUE
C             add to list
              CALL MLAYER(DATA,J,256)
C             set flag
              HIDEAL=.TRUE.
         ENDIF
C        Set all layers off
      ELSE IF ( CCMD .EQ. 'L' ) THEN
C        List layer which are switch on
         CALL BCHAR(INPL)
         LENG = 63
         LP=1
         COMMA=','
         CALL DCPRNT(137)
C        set last beyond possible range to start with.
         LASTLY=-2
         FIRST=.TRUE.
C        loop for layers possible
         LCOUNT = 0
         DO 60 I=0,255
            IF ( VLAYER(I) ) THEN
C
               LCOUNT = LCOUNT + 1
               IF ( NLEN(LNAME(I)).GT.0) THEN
                   NAME = LNAME(I)
               ELSE
                   NAME = 'No Name Attached'
               ENDIF

               IF ( I.EQ.CLAYER ) THEN

                   WRITE(TEXTO(LCOUNT),FMT='(A,I3,2A)' ) 
     + 'Working Layer: ',I,' : ',NAME(1:NLEN(NAME))
               ELSE
                   WRITE(TEXTO(LCOUNT),FMT='(A,I3,2A)' ) 
     + 'Visible Layer: ',I,' : ',NAME(1:NLEN(NAME))
               END IF
            ENDIF
 60      CONTINUE

         CALL INFODIALOG(TEXTO,LCOUNT)
         IF(.NOT.POPUP) THEN
C             if from a popup then dont highlight
              CALL GTMCLO(TMEN,TCELL)
         ENDIF
C
      ELSE IF ( CCMD. EQ. 'E' ) THEN
C        Clear the display option menu.
         CALL GTCLRM(4)
C        Insert Layer option menu
         DISLAY=.NOT.DISLAY
         CALL MNIDIS()
         RETURN
      ELSE IF (CCMD.EQ.'q') THEN
C        Quit key hit (MASTER MENU).
         RETURN
      ELSE IF (CCMD.EQ.CHAR(150) ) THEN
 
         CCMD=' '
C 
         NDATA=0
         SNDATA=0
C 
         IF(SHOWAL) THEN
             DO 400 I=0,255
                IF(TLAYER(I).GT.0) VLAYER(I)= .TRUE.
400          CONTINUE
             CALL CMPTST()
             IF(HIDE) THEN
                 DO 401 I=1,FINTOT
                    IF(FINLAY(I).GT.255) VLAYER(FINLAY(I)-256)=.FALSE.
401              CONTINUE
             ENDIF
C            redraw everything
             CALL REGEND()
C            should we reset ?
             IF(MVPACT) THEN
C                save current picture
                 IF (MAWS) THEN                
C                   Must be MAWS so save everything
                    CALL SAVPIC()
                 ELSE 
C                   Must be viewports so call this to save viewport
                    CALL WNSVBT()
                 ENDIF
             ENDIF
C            repaint any windows if showing the background
             CALL WNPNTA()
         ELSE IF(HIDEAL) THEN
             DO 403 I=0,255
                VLAYER(I)= .FALSE.
403          CONTINUE
             VLAYER(CLAYER)=.TRUE.
             IF (SHOW) THEN
                DO 404 I=1,FINTOT
                IF(FINLAY(I).LT.256) VLAYER(FINLAY(I))=.TRUE.
404             CONTINUE
             ENDIF
C            redraw everything.
             CALL REGEND()
             IF(MVPACT) THEN
C                save current picture
                 IF (MAWS) THEN
C                   Must be MAWS so save everything
                    CALL SAVPIC()
                 ELSE 
C                   Must be viewports so call this to save viewport
                    CALL WNSVBT()
                 ENDIF
             ENDIF
C            repaint any windows if showing the background
             CALL WNPNTA()
         ENDIF
C 
         IF (SHOW) THEN
            DO 100 I=1,FINTOT
                IF(FINLAY(I).LT.256) THEN
C                   is this layer to be hidden
                    DO 500 J=1,FINTOT
C                       yes
                        IF(FINLAY(J)-256.EQ.FINLAY(I) ) GOTO 100
 500                CONTINUE
                    SNDATA=SNDATA+1
                    SDATA(SNDATA)=FINLAY(I)
                ENDIF
 100        CONTINUE
         ENDIF
         IF (HIDE) THEN
            DO 101 I=1,FINTOT
 
                IF(FINLAY(I).GT.255) THEN
C                   is this layer to be shown
C                    DO 501 J=1,FINTOT
C                       yes
C                        IF(FINLAY(I)-256.EQ.FINLAY(J) ) GOTO 101
C 501                CONTINUE
                    NDATA=NDATA+1
                    DATA(NDATA)=FINLAY(I)-256
                ENDIF
 
 101        CONTINUE
 
         ENDIF
 
         IF(SNDATA.GT.0) THEN
             DO 300 I=1,SNDATA
                VLAYER(SDATA(I))=.TRUE.
 300         CONTINUE
C            save current configuartion now before drawing
             CALL SAVLAY(CVPN,OK)
             CALL ADDLAY(SDATA,SNDATA)
         ENDIF
         IF(NDATA.GT.0) THEN
C            save current configuartion now before drawing
             CALL SAVLAY(CVPN,OK)
             CALL DELLAY(DATA,NDATA)
             DO 301 I=1,NDATA
                VLAYER(DATA(I))=.FALSE.
301         CONTINUE
         ENDIF
C 
C        reset everything
         FINTOT=0
         LOADED=.FALSE.
         SHOW=.FALSE.
         HIDE=.FALSE.
         HIDEAL=.FALSE.
         SHOWAL=.FALSE.
C        save all layer conditions please
         CALL SAVLAY(CVPN,OK) 
C        update banners on the viewports
         IF(CVPN.GT.0.AND.MVPACT) THEN
             CALL DRWVPD()
         ENDIF
C        update the layer menu 
         CALL MNIDIS()
         RETURN
 
      ELSE
         CALL DEPRNT(8)
      END IF
 99   CONTINUE
C     if looping the execute the command NOW
      IF(LOOP) THEN
          CCMD = CHAR(150)
          LOOP = .FALSE.  
          GOTO 13
      ENDIF
 
      IF (.NOT.(SHOW.OR.HIDE.OR.HIDEAL.OR.SHOWAL).AND.LOADED.
     +    AND.BATCH) THEN
         CALL GTHFMC(4,CHAR(150),TCELL)
         LOADED = .FALSE.
         CALL GTCLRC(4,TCELL)
         CALL GTPMEN(LNAME(CLAYER),'N',4,TCELL)
      ENDIF
      RETURN
C     low the cell only here
 98   CONTINUE
C     save all layer conditions please
      CALL SAVLAY(CVPN,OK) 
C     update banners on the viewports
      IF(CVPN.GT.0.AND.MVPACT) THEN
          CALL DRWVPD()
      ENDIF
      IF(.NOT.POPUP) THEN
          CALL GTMCLO(TMEN,TCELL)
      ENDIF
 
      END
C-------------------------------------------------------------
C
 
 
      FUNCTION   SNAPG(X,GSIZE,ORG)
C     =============================
C
C1    vartype      R   R   R    R
C1    iostatus     O   I   I    I
C
      REAL SNAPG,X,GSIZE,ORG,TMP1
      TMP1=NINT((X-ORG)/GSIZE)
      SNAPG=(TMP1*GSIZE)+ORG
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE SNAPXY(XW,YW,XS,YS)
C     ==============================
C
C1    vartype           R  R  R  R
C1    iostatus          I  I  O  O
C
      include 'include/ndata.inc'
      REAL XW,YW,XS,YS,SNAPG
      EXTERNAL SNAPG,ISOSNP
C
      IF(GRTYPE .EQ. 2) THEN
C       isometric grid
        CALL ISOSNP(XW,YW,XS,YS)
      ELSE
C       cartesian grid
        XS=SNAPG(XW,GRIDSX,GRIDOX)
        YS=SNAPG(YW,GRIDSY,GRIDOY)
      ENDIF
C  
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE TEXBOX(X1,X2,X3,X4,X5,X6,XMIN,YMIN,XMAX,YMAX)
C     ========================================================
C1                       R, R, R, R, R, R,  R ,  R ,  R ,  R
C1                       I, I, I, I, I, I,  O ,  O ,  O ,  O
C
C
      include 'include/nbuff.inc'
C
      REAL X(4),Y(4),X1,X2,X3,X4,X5,X6,
     +    XMIN,XMAX,YMIN,YMAX,SLA
      INTEGER*2 NCHAR,JST,I
      EXTERNAL TORGTL,TORGTR,TORGBR,TXTORG,UCODET
C
      CALL UCODET(X6,SLA,JST,NCHAR)
C
      XMIN= 1.0E30
      YMIN= 1.0E30
      XMAX=-1.0E30
      YMAX=-1.0E30
 
      DO 10 I=1,4
        CALL TXTORG(I,X1,X2,JST,X5,SLA,NCHAR,X3,X4,X(I),Y(I))
        XMIN=MIN(X(I),XMIN)
        XMAX=MAX(X(I),XMAX)
        YMIN=MIN(Y(I),YMIN)
        YMAX=MAX(Y(I),YMAX)
 10   CONTINUE
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE TORGBR(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     ========================================================
C1                      R,R,I*2,  R , R , I*2 , R , R  , R, O
C1                      I,I, I ,  I , I ,  I  , I , I  , O, O
C
C
      REAL X,Y,TAGL,SLA,TWD,THGT,XO,YO,RAD,DIST
      INTEGER*2 NCHAR,JST
      INTRINSIC COS,SIN
      EXTERNAL RAD
C
C       Left , Centre and Right justifications
C
      GOTO ( 10,10,10,20,20,20,30,30,30 ) JST
C        Left justification
 10   CONTINUE
         XO=X + TWD*NCHAR*COS(RAD(TAGL))
         YO=Y + TWD*NCHAR*SIN(RAD(TAGL))
         GOTO 40
 20   CONTINUE
C        Centre justification
         XO=X + TWD*(NCHAR/2.0)*COS(RAD(TAGL))
         YO=Y + TWD*(NCHAR/2.0)*SIN(RAD(TAGL))
         GOTO 40
 30   CONTINUE
C        No need to change Right justification
         XO=X
         YO=Y
 40   CONTINUE
C
C     Top  , Middle and Bottom justifications
C
      GOTO ( 50,60,70,50,60,70,50,60,70 ) JST
 50   CONTINUE
C        No need to change Bottom justification
         XO=XO
         YO=YO
         GOTO 80
 60   CONTINUE
C        Middle justification
         DIST=  (THGT/2.0)/COS(RAD(SLA))
         XO=XO - DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO - DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 70   CONTINUE
C        Top justification
         DIST=  THGT/COS(RAD(SLA))
         XO=XO - DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO - DIST*SIN(RAD(TAGL+90.0-SLA))
C
 80   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE TORGTL(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     ========================================================
C1                      R,R,I*2,  R , R , I*2 , R , R  , R, O
C1                      I,I, I ,  I , I ,  I  , I , I  , O, O
C
C
      REAL X,Y,TAGL,SLA,TWD,THGT,XO,YO,RAD,DIST
      INTEGER*2 NCHAR,JST
      INTRINSIC COS,SIN
      EXTERNAL RAD
C
C       Left , Centre and Right justifications
C
      GOTO ( 10,10,10,20,20,20,30,30,30 ) JST
C     No need to change left justification
 10   CONTINUE
         XO=X
         YO=Y
         GOTO 40
 20   CONTINUE
C        Centre justification
         XO=X - TWD*(NCHAR/2.0)*COS(RAD(TAGL))
         YO=Y - TWD*(NCHAR/2.0)*SIN(RAD(TAGL))
         GOTO 40
 30   CONTINUE
C        Right justification
         XO=X - TWD*NCHAR*COS(RAD(TAGL))
         YO=Y - TWD*NCHAR*SIN(RAD(TAGL))
 40   CONTINUE
C
C     Top , Middle and Bottom justifications
C
      GOTO ( 50,60,70,50,60,70,50,60,70 ) JST
 50   CONTINUE
C        Bottom justification
         DIST=  THGT/COS(RAD(SLA))
         XO=XO + DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO + DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 60   CONTINUE
C        Middle justification
         DIST=  (THGT/2.0)/COS(RAD(SLA))
         XO=XO + DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO + DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 70   CONTINUE
C        No need to change top justification
         XO=XO
         YO=YO
C
 80   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE TORGTR(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     ========================================================
C1                      R,R,I*2,  R , R , I*2 , R , R  , R, O
C1                      I,I, I ,  I , I ,  I  , I , I  , O, O
C
C
      REAL X,Y,TAGL,SLA,TWD,THGT,XO,YO,RAD,DIST
      INTEGER*2 NCHAR,JST
      INTRINSIC COS,SIN
      EXTERNAL RAD
C
C       Left , Centre and Right justifications
C
      GOTO ( 10,10,10,20,20,20,30,30,30 ) JST
C        Left justification
 10   CONTINUE
         XO=X + TWD*NCHAR*COS(RAD(TAGL))
         YO=Y + TWD*NCHAR*SIN(RAD(TAGL))
         GOTO 40
 20   CONTINUE
C        Centre justification
         XO=X + TWD*(NCHAR/2.0)*COS(RAD(TAGL))
         YO=Y + TWD*(NCHAR/2.0)*SIN(RAD(TAGL))
         GOTO 40
 30   CONTINUE
C        No need to change Right justification
         XO=X
         YO=Y
 40   CONTINUE
C
C     Top  , Middle and Bottom justifications
C
      GOTO ( 50,60,70,50,60,70,50,60,70 ) JST
 50   CONTINUE
C        Bottom justification
         DIST=  THGT/COS(RAD(SLA))
         XO=XO + DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO + DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 60   CONTINUE
C        Middle justification
         DIST=  (THGT/2.0)/COS(RAD(SLA))
         XO=XO + DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO + DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 70   CONTINUE
C        No need to change top justification
         XO=XO
         YO=YO
C
 80   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE TXTORG(FUNC,X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     =============================================================
C1                      I,R,R,I*2,  R , R , I*2 , R , R  , R, O
C1                      I,I,I, I ,  I , I ,  I  , I , I  , O, O
C
      REAL X,Y,TAGL,SLA,TWD,THGT,XO,YO,RAD,DIST
      INTEGER*2 NCHAR,JST,I,FTABL(9,8),CONST,FUNC
      INTRINSIC COS,SIN
      EXTERNAL RAD
C
      DATA FTABL/0, 0, 0,-1,-1,-1,-2,-2,-2,
     2           0, 0, 0,-1,-1,-1,-2,-2,-2,
     3           2, 2, 2, 1, 1, 1, 0, 0, 0,
     4           2, 2, 2, 1, 1, 1, 0, 0, 0,
     1           0,-1,-2, 0,-1,-2, 0,-1,-2,
     2           2, 1, 0, 2, 1, 0, 2, 1, 0,
     3           2, 1, 0, 2, 1, 0, 2, 1, 0,
     4           0,-1,-2, 0,-1,-2, 0,-1,-2/
C
C     Left , Centre and Right justifications
      CONST=FTABL(JST,FUNC)
      DIST=(TWD*NCHAR)/2.0
C     DIST=(TWD*NCHAR-(TWD/3.0))/2.0
      XO=X + CONST*DIST*COS(RAD(TAGL))
      YO=Y + CONST*(TWD*NCHAR-(TWD/3.0))/2.0*SIN(RAD(TAGL))
C     Top , Middle and Bottom justifications
      DIST=(THGT/2.0)/COS(RAD(SLA))
      CONST=FTABL(JST,FUNC+4)
      XO=XO + CONST*DIST*COS(RAD(TAGL+90.0-SLA))
      YO=YO + CONST*DIST*SIN(RAD(TAGL+90.0-SLA))
C
      END
C
CC---------------------------------------------------------------------
CC   moved this to equivhack.f cos of misalign problem on sparcs
CC
CC
C      SUBROUTINE UCODET(TEMP,SLA,JUST,NCHAR)
CC     ======================================
CC1    vartype             R , R ,  I2, I2
CC1    iostatus            I   O    O   O
CC
C      REAL TEMP,Y,SLA
C      INTEGER*2 NCHAR,JUST,I(2)
C      EQUIVALENCE(Y,I)
C      INTRINSIC MOD,INT
CC
C      Y=TEMP
C      NCHAR=I(2)
C      SLA=MOD(I(1)+0,256)-90
C      JUST=INT(I(1)/256)
CCSUN386
CC      NCHAR=I(1)
CC      SLA=MOD(I(2)+0,256)-90
CC      JUST=INT(I(2)/256)
CCSUN386
CC
C      END
C
C-------------------------------------------------------------
C
      SUBROUTINE XYNORG(X,Y,JST,TAGL,SLA,NCHAR,TWD,THGT,XO,YO)
C     ========================================================
C1                      R,R,I*2,  R , R , I*2 , R , R  , R, O
C1                      I,I, I ,  I , I ,  I  , I , I  , O, O
C
C
      REAL X,Y,TAGL,SLA,TWD,THGT,XO,YO,RAD,DIST
      INTEGER*2 NCHAR,JST
      INTRINSIC COS,SIN
      EXTERNAL RAD
C
C       Left , Centre and Right justifications
C
      DIST=TWD*NCHAR-(TWD/3.0)
      GOTO ( 10,10,10,20,20,20,30,30,30 ) JST
 10   CONTINUE
C        No need to change Left justification
         XO=X
         YO=Y
         GOTO 40
 20   CONTINUE
C        Centre justification
         XO=X - DIST/2.0*COS(RAD(TAGL))
         YO=Y - DIST/2.0*SIN(RAD(TAGL))
         GOTO 40
 30   CONTINUE
C        Right justification
         XO=X - DIST*COS(RAD(TAGL))
         YO=Y - DIST*SIN(RAD(TAGL))
 40   CONTINUE
C
      GOTO ( 50,60,70,50,60,70,50,60,70 ) JST
C
 50   CONTINUE
C        No need to change bottom justification
         XO=XO
         YO=YO
         GOTO 80
 60   CONTINUE
C        Middle justification
         DIST=  (THGT/2.0)/COS(RAD(SLA))
         XO=XO - DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO - DIST*SIN(RAD(TAGL+90.0-SLA))
         GOTO 80
 70   CONTINUE
C        Top justification
         DIST=  THGT/COS(RAD(SLA))
         XO=XO - DIST*COS(RAD(TAGL+90.0-SLA))
         YO=YO - DIST*SIN(RAD(TAGL+90.0-SLA))
C
 80   CONTINUE
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE ZOMEXT()
C     ===================
C
C1    Subroutine ZOMEXT produces a screen display
C1    using the limits of the world mapping to the
C1    drawing sheet.
C
      include 'include/wtov.inc'
      include 'include/journal.inc'
      include 'include/menun.inc'
C
      REAL TX,TY
      EXTERNAL OLDVPT,WORLD,REGEND,WRTJRN
C
C     nothing to do if already at extents
C      IF (.NOT.ZOMLIM) THEN
C        save current viewport limits
         CALL OLDVPT()
C        find length and width of paper mapping
         TX=WPXMAX-WPXMIN
         TY=WPYMAX-WPYMIN
C        set view limits to 3% greater than world extents
         CALL WORLD(WPXMIN-0.015*TX,WPYMIN-0.015*TY,
     +           WPXMAX+0.015*TX,WPYMAX+0.015*TY)
C
C        generate the new display
         CALL REGEND()
         ZOMLIM=.TRUE.
C        write out a journal entry
         IF(JOURON .AND. MEN .EQ. 4) THEN
C           only punt out a journaling command if 
C           the function has been called by the user
C           from the display menu and not from a DAXCAD
C           funtion like SHOPAP.
            CALL WRTJRN(0.0,0.0,'m',JRNCMD(7),0)
         ENDIF
C      END IF
C
      END
C
C-------------------------------------------------------------
C
