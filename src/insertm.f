C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insertm.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE INSM00()
C     SUBROUTINE INSM01()
C     SUBROUTINE MNIMRK
C     SUBROUTINE SCLMRK(MNUM,HEIGHT,WIDTH,SCALX,SCALY,ST)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE INSM00()
C     ===================
C1    no arguments required
C2
C2    controls operation of the INSERT CURVE function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL MNICUR,INSD01,GTMCLO,CLRPEW
C
      TMEN=MEN
      TCELL=CELLN
C     initialize INSERT CURVE option menu
      CALL MNIMRK()
C     enter the INSERT CURVE routine
      CALL INSM01()
C     unload the points mode
      CALL MNUPTS()
C     ensure insert option for curve is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW()
C
      END
C
C--------------------------------------------------------
C
      SUBROUTINE INSM01()
C     =============================
C
      include 'include/marker.inc'
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/save.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 SPDP
      INTEGER*2 FMIPOS
      INTEGER*2 P,ENT,TFORM,TFONT,TLAY
      INTEGER*4 NLEN,C
      DOUBLE PRECISION DN
      LOGICAL OPTION,QUIT,OK,DELETE,CVERFY,FIRST
      REAL FP(3),HPX,HPY,M(3,3),TW
      REAL XMAX,XMIN,YMAX,YMIN,SCALX,SCALY,ANGLE
      CHARACTER*20 NAME,TOKEN*1
      EXTERNAL NLEN,CVERFY,INSATT
C
C     save current masterindex pointers
 
      FMIPOS=NMIPOS
 
      ENT=MARKER
C
      SCALX=1.0
      SCALY=1.0
      ANGLE=0.0
 1    CONTINUE
 
C     save current database pointrs
      CALL INSDS2(.TRUE.)
 
 5    CONTINUE
C
C     go find an point to put it through
      CALL  FINDP0(164,HPX,HPY,OPTION,QUIT)
C
      IF ((OPTION.AND.MEN.EQ.2).OR.
     +    QUIT) THEN
C         reset flags
          RETURN
      ENDIF
 
 
      IF ( OPTION ) THEN
C
C******************************************************
C                CANCEL MARKER                        *
C******************************************************
         IF ( CCMD.EQ.'c') THEN
            FIRST=.FALSE.
            CALL INSCNL(FMIPOS,ENT,1,FIRST,OK)
C******************************************************
C                LINE FONTING                         *
C******************************************************
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
         ELSE IF (CVERFY(CCMD,'=fk')) THEN
C                ****************************
C                 Change line attributes.  
C                ****************************
                 CALL INSATT(CCMD)
C                Don't forget to un highlight the "Attribues" cell.
                 CALL GTMCLO(MEN,CELLN)                                  
C******************************************************
C                SCALE IN X                           *
C******************************************************
         ELSE IF(CCMD.EQ.'X') THEN
 10         CONTINUE
            CALL DPRMXP(245,NAME)
C           Blank string return without change
            IF ( NLEN(NAME) .EQ. 0 ) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 5
            ELSE
               CALL AEXPRN(NAME,DN,*10)
C              Check within limits for defined fonts.
               IF ( DN.LT.0.0 ) THEN
                  CALL DEPRNT(362)
                  GOTO 10
               END IF
               SCALX=DN
           END IF
C          find token for X Scale:
           CALL FNDTOK(501,TOKEN)
C          update cell content
           CALL GTMCWR(3,TOKEN,SCALX,'(F8.2)')
C******************************************************
C                SCALE IN Y                           *
C******************************************************
         ELSE IF(CCMD.EQ.'y') THEN
 20         CONTINUE
            CALL DPRMXP(245,NAME)
C           Blank string return without change
            IF ( NLEN(NAME) .EQ. 0 ) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 5
            ELSE
               CALL AEXPRN(NAME,DN,*20)
C              Check within limits for defined fonts.
               IF ( DN.LT.0.0 ) THEN
                  CALL DEPRNT(362)
                  GOTO 20
               END IF
               SCALY=DN
           END IF
C          find token for Y Scale:
           CALL FNDTOK(502,TOKEN)
C          update cell content
           CALL GTMCWR(3,TOKEN,SCALY,'(F8.2)')
C******************************************************
C                rotation angle                       *
C******************************************************
         ELSE IF(CCMD.EQ.'r') THEN
 30         CONTINUE
            CALL DPRMXP(246,NAME)
C           Blank string return without change
            IF ( NLEN(NAME) .EQ. 0 ) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 5
            ELSE
               CALL AEXPRN(NAME,DN,*30)
C              Check within limits for defined fonts.
               IF ( DN.LT.0.0 ) THEN
                  CALL DEPRNT(362)
                  GOTO 30
               END IF
               ANGLE=DN
           END IF
C          find token for Rotation:
           CALL FNDTOK(505,TOKEN)
C          update cell content
           CALL GTMCWR(3,TOKEN,ANGLE,'(F8.2)')
C******************************************************
C                MARKER INDEX NUMBER                  *
C******************************************************
         ELSE IF(CCMD.EQ.'i') THEN
 40         CONTINUE
            CALL DPRMXP(583,NAME)
C           Blank string return without change
            IF ( NLEN(NAME) .EQ. 0 ) THEN
               CALL GTMCLO(MEN,CELLN)
               GOTO 5
            ELSE
               CALL AEXPRN(NAME,DN,*40)
C              Check within limits for defined fonts.
               IF ( DN.LT.1.OR.DN.GT.MRKIMX ) THEN
                  CALL DEPRNT(362)
                  GOTO 40
               END IF
C              find token for Index value
               CALL FNDTOK(504,TOKEN)
c
               MRKNUM=INT(DN)
               C=MRKNUM
               CALL GTMCWI(3,TOKEN,C)
           END IF
 
         END IF
         GOTO 5
C
      END IF
C     It was not an option so it must be raw data
C     so let us create a marker with the current values
C
      TFORM=MRKNUM
      TFONT=CLFONT
      TLAY=CLAYER
      CALL DEWC02(HPX,HPY,ANGLE,SCALX,SCALY,TFORM,TFONT,TLAY,SPDP,OK)
 
      VPADD = .TRUE.
      ENT=MARKER
C     draw the entity
      CALL ALLDRW(ENT,SPDP)
      VPADD = .FALSE.
 
 
C
      GOTO 5
C
      END
*
 
C
      SUBROUTINE MNIMRK
C     =================
C1    no arguments required
C
C2    Clears the minor option menu and loads
C2    the INSERT CURVE option list.
C2
C2
      include 'include/ndata.inc'
      include 'include/marker.inc'
C
      INTEGER*4 MODES(4),C
      CHARACTER*1 TOKEN
      EXTERNAL GTDMEN,GTCLRM,GTDOMN
C
C     Clear minor option menu.
      CALL GTCLRM(3)
      CALL MNISTD()
C2
      CALL GTDMEN(501,3)
      CALL GTDMEN(502,3)
      CALL GTDMEN(505,3)
C     find token for X Scale:
      CALL FNDTOK(501,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,1.0,'(F8.2)')
C
C     find token for Y Scale:
      CALL FNDTOK(502,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,1.0,'(F8.2)')
C     find token for rotation angle:
      CALL FNDTOK(505,TOKEN)
C     update cell content
      CALL GTMCWR(3,TOKEN,0.0,'(F8.2)')
C
C      cancel cell
      CALL GTDMEN(503,3)
C     and accept
      CALL GTDMEN(504,3)
C     find token for Index value
      CALL FNDTOK(504,TOKEN)
c
      C=MRKNUM
      CALL GTMCWI(3,TOKEN,C)
C     Load the point modes.
      CALL MNLPTS()
C
      END
*
 
      SUBROUTINE SCLMRK(MNUM,HEIGHT,WIDTH,SCALX,SCALY,ST)
C     =====================
C
      include 'include/marker.inc'
 
      DOUBLE PRECISION HEIGHT,WIDTH
      REAL MAXX,MAXY,SCALX,SCALY
      INTEGER*2 MIND
      INTEGER*4 ST,MNUM
 
      MIND=MRKIND(MNUM)
 
C     Set the scale factors to a default of one
      SCALX = 1
      SCALY = 1
 
C     Zero is undefined so do not do anything but flag it
      IF (MIND.EQ.0) THEN
          ST = 1
          RETURN
      ENDIF
 
C     Now work out maxx and maxy
C     Top right of bounding box is MRKR(MIND+1)
C     Bottom left MRKR(MIND)
      MAXX = MRKR(MIND+1,1) - MRKR(MIND,1)
      MAXY = MRKR(MIND+1,2) - MRKR(MIND,2)
 
 
C     Now work out the scale factors
      IF (MAXX.NE.0) THEN
         SCALX = HEIGHT / MAXX
      ENDIF
      IF (MAXY.NE.0) THEN
         SCALY = WIDTH  / MAXY
      ENDIF
 
C     Set st = 0 to show success
      ST = 0
      END
 
 
 
 
