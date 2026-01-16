C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 vpdraw.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE VPCLR()
C     SUBROUTINE VPCOL(ST)
C     SUBROUTINE VPDRAW(VPN,DISPLAY,ST)
C     SUBROUTINE VPRES()
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE VPCLR()
C     ===================
C1    no arguments required
C2
C2    this routine removes any viewports that are present
C2    and causes only the backcloth to remain
C
      include 'include/masti.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 LDTMP
C
C     Save the backcloth pointers
      LDTMP = LDFILE(0)
C     Initailise the viewports
      CALL INITVP()
C     Restore the backcloth pointers
      LDFILE(0) = LDTMP
 
      END
 
      SUBROUTINE VPCOL(ST)
C     ====================
C1    VARYPE           I4
C1    IOSTAT           I
C
C2    sets the color of the current bitmap to that of the
C2    parent bitmap color allready set Uses daxcad to do it
C
      include   'include/daxcolor.inc'
C
      INTEGER*4 ST
      CALL PEN(COLPEN)
      ST = 0
      END
 
 
 
 
      SUBROUTINE VPDRAW(VPN,DISPLAY,ST)
C     =================================
C1    VARYPE            I2     L  I4
C1    IOSTAT            I      I  O
C
C2    sets up a viewport for drawing into. If diplsay is set to true
C2    then both screen and hidden display will be updated. If false
C2    the only the hidden bitmap will be updated
C
      include   'include/viewport.inc'
      include   'include/ddcode.inc'
C
      LOGICAL DISPLAY
      LOGICAL CREATE
      INTEGER*4 ST
      INTEGER*2 VPN
C
C
C     get viewport number to use
      ST = 0
      IF(.NOT.MVPACT.OR.(VPN.LT.0.OR.VPN.GT.MAXVP)) THEN
          ST = 1
          GOTO 999
      ENDIF
C     are we creating 
      CALL PORT_CREATE_INQ(CREATE,ST)
      IF(CREATE) THEN
C         force an update cos vp not yet on display
          CALL PORT_DC_SET(DC_PORT_NORMAL_HIDDEN,ST)
      ELSE
          IF(DISPLAY) THEN
              CALL PORT_DC_SET(DC_PORT_AND_SCREEN,ST)
          ELSE
              CALL PORT_DC_SET(DC_PORT_NORMAL_HIDDEN,ST)
          ENDIF
      ENDIF
999   CONTINUE
      END
C
C
 
      SUBROUTINE VPRES()
C     ===================
C2    NO ARGUMENTS
C
C2    Resets the viewport bitmap to main display
C2    and ensures draiwng code reset
C
      include   'include/viewport.inc'
      include   'include/daxcolor.inc'
      include   'include/ddcode.inc'
C
      INTEGER*4 ST
C
      ST = 0
      IF(.NOT.MVPACT) THEN
C         no viewport then forget it
          ST = 1
          GOTO 999
      ENDIF
C     force an update cos vp not yet on display
      CALL PORT_DC_SET(DC_NORMAL,ST)
      CALL SETAPN(COLPEN)
C
999   CONTINUE
      END
C
