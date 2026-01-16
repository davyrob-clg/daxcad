C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 vpselect.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     LOGICAL FUNCTION CONFRM()
C     SUBROUTINE SELV00()
C     SUBROUTINE SELV01()
C     SUBROUTINE TRNRST(VPN)
C     SUBROUTINE TRNSAV(VPN)
C
C     |-----------------------------------------------------------------|
C
      LOGICAL FUNCTION CONFRM()
C     =========================
C1    NO ARGS
C
C2    This function will bring up a popup in the middle of the screen
C2    and ask to confirm the current state
C
      include 'include/viewport.inc'
      include 'include/menpop.inc'
      include 'include/menun.inc'
      INTEGER*4 MNUM
      LOGICAL MOK
C
      MNUM = 15
      CCMD = ' '
      CALL MENPOP(MNUM,MOK)
C
      CONFRM = CCMD.EQ.'C'
      END
C
      SUBROUTINE SELV00()
C     ===================
C
C1    vartype
C1    iostatus
C
C2
C2    controls the SELECT DIMENSION STANDARD function
C                           
      include 'include/viewport.inc'
      include 'include/menun.inc'
C                       
      CHARACTER TOKEN,UTOKEN
      INTEGER*4 TCELL,TMEN
C
      EXTERNAL GTMCLO,SELS02
C                   
      TMEN=MEN
      TCELL=CELLN
C     initialize SELECT DIMENSION menu
      CALL MNLVP0()
C     call the select dim routine
      CALL SELV01()                

      END
*

                          

      SUBROUTINE SELV01()
C     ===================
C1    no arguments required
C
C2    Controls select viewport system
C2    should allow instream change of viewing system
C
      include 'include/masti.inc'
      include 'include/viewport.inc'
      include 'include/menun.inc'
C
      CHARACTER TOKEN,UTOKEN
      CHARACTER*80 INPL*1
      REAL X,Y
      INTEGER*4 TCELL,TMEN
      INTEGER*4 C,NLEN,DNUM
      INTEGER*4 I
      INTEGER*2 LDFTMP
      LOGICAL RESET,YESOK,CVERFY,SYS
      LOGICAL   OLDMWS
C
      EXTERNAL GTMCLO,SELS02
      EXTERNAL NLEN,YESOK
C                   
      TMEN=MEN
      TCELL=CELLN
C                                           
10    CONTINUE
      DNUM = 743
      CALL DCPRNT(DNUM)
      CALL TCURS(C,X,Y)           
C     Remember last display state ie either maws or daxports
      OLDMWS = MAWS                                        
C
C     If graphic screen then go back round after telling user
      IF ( MEN.EQ.0 ) THEN
C     117    "Invalid area of screen"
         CALL DEPRNT(117)
         GOTO 10
      ENDIF

C     If not our menu then exit
      IF (MEN.NE.3) GOTO 1000
C
      IF(CVERFY(CCMD,'MD')) THEN
          IF(CCMD.EQ.'M') THEN
              DNUM = 744
              SYS = .TRUE.
          ELSE
              DNUM = 745
              SYS = .FALSE.
          ENDIF
          CALL DCPRNT(DNUM)
          IF(MVPACT) THEN     
              DNUM = 746
              CALL DPRMXP(DNUM,INPL)
              IF(NLEN(INPL).EQ.0) GOTO 1000
              RESET = YESOK(INPL)            
              IF(.NOT.RESET) GOTO 1000
C             Confirm he wants it 
              DNUM = 5
              CALL DPRMXP(DNUM,INPL)
              IF(NLEN(INPL).EQ.0) GOTO 1000
              RESET = YESOK(INPL)            
              IF(.NOT.RESET) GOTO 1000
C             save current display file pointers on backcloth only
              LDFTMP = LDFILE(0)
C             ok go ahaead and reset the system
              CALL INITVP()
C             reset current display file pointers
              LDFILE(0) = LDFTMP
          ENDIF
C         set system variable
          MAWS = SYS
C         Now handle the highlight of the menu
          IF (MAWS.NEQV.OLDMWS) THEN
              IF(MAWS) THEN
                 TOKEN = 'M'
                 UTOKEN = 'D'
              ELSE
                 TOKEN = 'D'
                 UTOKEN = 'M'
              ENDIF
C             Do the unhighlighting
              CALL GTHFMC(3,UTOKEN,TCELL)
              CALL GTMCLO(3,TCELL)
C             Now do the highlighting
              CALL GTHFMC(3,TOKEN,TCELL)
              CALL GTMCHI(3,TCELL)
          ENDIF
      ENDIF  
C     Go back round and see what the next input brings
      GOTO 10                            

1000  CONTINUE
      END
 
      SUBROUTINE TRNRST(VPN)
C1    =====================
C2    IOTYPE            I
C2    IOSTAT            I
C2    This routine is used to restore the
C2    world to screen and screen to world transforms
C2    from the previously stored viewport number VPN.
C2    This saves time later on when in unneccassary
C2    recalculation of the viewport number
C
      include   'include/wtov.inc'
      include   'include/vptrans.inc'
      include   'include/viewport.inc'
      INTEGER*2 VPN,I,J
C
 
C     recover clipping limits please
      WXMIN = VIEWPS(1,VPN+1)
      WYMIN=  VIEWPS(2,VPN+1)
      WXMAX = VIEWPS(3,VPN+1)
      WYMAX = VIEWPS(4,VPN+1)
 
      DO 10 I= 1,3
         DO 20 J = 1,3
           WVXY(I,J) = VPWVXY(VPN+1,I,J)
           VWXY(I,J) = VPVWXY(VPN+1,I,J)
 20      CONTINUE
 10   CONTINUE
      END
      SUBROUTINE TRNSAV(VPN)
C1    =====================
C2    IOTYPE            I
C2    IOSTAT            I
C2    This routine is used to save the current
C2    world to screens and screen to world transforms
C2    against the current viewport number VPN.
C2    This saves time later on when in unneccassary
C2    recalculation of the viewport number
C
      include   'include/wtov.inc'
      include   'include/vptrans.inc'
      INTEGER*2 VPN,I,J
C
      DO 10 I= 1,3
         DO 20 J = 1,3
           VPWVXY(VPN+1,I,J) = WVXY(I,J)
           VPVWXY(VPN+1,I,J) = VWXY(I,J)
 20      CONTINUE
 10   CONTINUE
      END
C
C-----------------------------------------------------------------------
C
 
 
 
 
 
 
