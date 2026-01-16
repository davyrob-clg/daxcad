C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 vp_support.f   */
C

      SUBROUTINE PORT_ACTIVE(DPNUM,MODE,ST)
C     ====================================
C1    VARYPE                  I2    L   I
C1    IOSTAT                  I     I   O
C
C2    This routine will activate or deactivate the system based on
C2    any of the flags being set
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      LOGICAL MODE
      LOGICAL TRIP
      INTEGER*4 ST
      INTEGER*4 I
C
      ST = 0
C
      IF(DPNUM.LT.1.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      VPNUM(DPNUM) = MODE
      TRIP = .TRUE.
      DO 10 I=1,MAXVP
          IF(VPNUM(I)) THEN
              TRIP = .FALSE.
          ENDIF
10    CONTINUE
      IF(TRIP) THEN
C         if no more left then turn off system
          MVPACT = .FALSE.
      ELSE
C         make sure system is on
          MVPACT = .TRUE.
      ENDIF
C
999   CONTINUE
      END
C
C
C
C      @(#)  256.1 date 12/16/89 port_create_inq.ftn 
      SUBROUTINE PORT_CREATE_INQ(MODE,ST)
C     ==================================
C1    VARYPE                     L   I4
C1    IOSTAT                     I    O
C
C2    Sets the port create mode
C
      include 'include/viewport.inc'
C
      LOGICAL MODE
      INTEGER*4 ST
C
      ST = 0
C
      MODE = VIEWPORT_CREATE
C
      RETURN
999   CONTINUE
C
      END
 
C
C      @(#)  256.1 date 12/16/89 port_create_set.ftn 
      SUBROUTINE PORT_CREATE_SET(MODE,ST)
C     ==================================
C1    VARYPE                     L   I4
C1    IOSTAT                     I    O
C
C2    Sets the port create mode
C
      include 'include/viewport.inc'
C
      LOGICAL MODE
      INTEGER*4 ST
C
      ST = 0
C
      VIEWPORT_CREATE = MODE
C
C
      RETURN
999   CONTINUE
C
      END
C
C      @(#)  256.1 date 12/16/89 port_dc_set.ftn 
      SUBROUTINE PORT_DC_SET(CODE,ST)
C     ===============================
C1    VARYPE                  I4  I4
C1    IOSTAT                  I   O
C
C2    sets drawing code for viewport system
C2    when any of the low level graphics routines
C2    are used. This is modal This does not affect the transforms
C
      include 'include/viewport.inc'
      include 'include/daxcolor.inc'
      include 'include/ddcode.inc'
C
      INTEGER*4 ST
      INTEGER*4 CODE
      INTEGER*2 NUM
      INTEGER*2 DPNUM
C
      ST = 0
C
      IF(CODE.LT.0.OR.CODE.GT.3) THEN
          ST = 1
          GOTO 999
      ENDIF
      IF(MAWS) THEN
C         set up all for maws
          IF(CODE.EQ.DC_NORMAL) THEN
C             set display bitmap
              DPNUM = 0
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_NORMAL
          ELSEIF(CODE.EQ.DC_PORT_AND_SCREEN) THEN
              CALL PORT_INQ(DPNUM,ST)
              IF(DPNUM.EQ.0) THEN
C                 no backloth for maws
                  CALL PORT_DRAW(DPNUM,ST)
                  DDCODE = DC_NORMAL
              ELSE
                  DPNUM = DPNUM +  1
                  CALL PORT_DRAW(DPNUM,ST)
                  DDCODE = DC_PORT_AND_SCREEN
              ENDIF
          ELSEIF(CODE.EQ.DC_NORMAL_CLIPPED_TO_PORT) THEN
C             for maws can only haver the one anyway
              DPNUM = 0
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_NORMAL
          ELSEIF(CODE.EQ.DC_PORT_NORMAL_HIDDEN) THEN
              CALL PORT_INQ(DPNUM,ST)
              IF(DPNUM.EQ.0) THEN
C                 no backloth for maws
                  CALL PORT_DRAW(DPNUM,ST)
                  DDCODE = DC_NORMAL
                  ST = 2
                  GOTO 999
              ELSE
                  DPNUM = DPNUM +  1
                  CALL PORT_DRAW(DPNUM,ST)
                  DDCODE = DC_NORMAL
              ENDIF
          ENDIF
      ELSE
C         set up all for maws
          IF(CODE.EQ.DC_NORMAL) THEN
C             set display bitmap
              DPNUM = 0
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_NORMAL
          ELSEIF(CODE.EQ.DC_PORT_AND_SCREEN) THEN
C             update both hidden and screen
              CALL PORT_INQ(DPNUM,ST)
              DPNUM = DPNUM +  1
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_PORT_AND_SCREEN
          ELSEIF(CODE.EQ.DC_NORMAL_CLIPPED_TO_PORT) THEN
C             draw into the current display current only the backcloth
              DPNUM = 0
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_NORMAL_CLIPPED_TO_PORT
          ELSEIF(CODE.EQ.DC_PORT_NORMAL_HIDDEN) THEN
C             draw into the hidden daxport without updating screen
              CALL PORT_INQ(DPNUM,ST)
              DPNUM = DPNUM +  1
              CALL PORT_DRAW(DPNUM,ST)
              DDCODE = DC_NORMAL
          ENDIF
 
      ENDIF
 
 
 
      RETURN
999   CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 port_draw.ftn 
      SUBROUTINE PORT_DRAW(DPNUM,ST)
C     ===============================
C1    VARYPE               I2    I4
C1    IOSTAT               I     O
C
C2    Sets the port ready to draw into. This is always
C2    hidden but depending on the drawing code the screen
C2    can also be updated. The type of operation is set
C2    by PORT_OP_SET These options are not modem and must
C2    be set before calling this routine
C
      include 'include/viewport.inc'
      include 'include/vpops.inc'
      include 'include/daxcolor.inc'
C
      INTEGER*4 ST
      INTEGER*2 DPNUM
      INTEGER*2 NUM
      INTEGER*4 OPTYPE
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP+1) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      CALL PORT_OP_SET(VP_OP_MOVE,ST)
C
C     set up correct port
      CALL UPDBTM(DPNUM)
C
      CALL PORT_OP_SET(VP_OP_NORMAL,ST)
C
      RETURN
C
999   CONTINUE
C
      END
 
 
C      @(#)  256.1 date 12/16/89 port_inq.ftn 
      SUBROUTINE PORT_INQ(DPNUM,ST)
C     =============================
C1    VARYPE               I2   I4
C1    IOSTAT               O    O
C
C2    reutrn current port number
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
C
      ST = 0
C
      IF(.NOT.MVPACT) THEN
          ST = 1
          DPNUM = 0
          RETURN
      ENDIF
      DPNUM = CVPN
C
      RETURN
C
      END
C      @(#)  256.1 date 12/16/89 port_op_set.ftn 
      SUBROUTINE PORT_OP_SET(OPTYPE,ST)
C     ================================
C1    VARYPE                  I4    I4
C1    IOSTAT                  I     O
C
C2    sets the optype for the next viewport drawing op
C2    relates specically to display lists
C
      include 'include/vpops.inc'
      include 'include/viewport.inc'
C
      INTEGER*4 ST
      INTEGER*4 OPTYPE
C
C
      IF(OPTYPE.LT.0.OR.OPTYPE.GT.4) THEN
          ST = 1
          GOTO 999
      ENDIF
C
C     set all modes off
      VPADD = .FALSE.
      VPMOV = .FALSE.
      VPCAN = .FALSE.
C
      IF(OPTYPE.EQ.VP_OP_ADD) THEN
          VPADD = .TRUE.
      ELSEIF(OPTYPE.EQ.VP_OP_MOVE) THEN
          VPMOV = .TRUE.
      ELSEIF(OPTYPE.EQ.VP_OP_DRAW) THEN
          VPMOV = .TRUE.
      ELSEIF(OPTYPE.EQ.VP_OP_CANCEL) THEN
          VPCAN = .TRUE.
      ENDIF
      VP_OPTYPE = OPTYPE
      ST =0
      RETURN
C
999   CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 port_reset_name.ftn 
      SUBROUTINE PORT_RESET_NAME(DPNUM,ST)
C     ===================================
C1    VARYPE                      I2  I4
C1    IOSTAT                      I   O
C
C2    restes the current name of the viewport
C
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/vntable.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 ADDRES
      INTEGER*4 NLEN
      CHARACTER*20 VIEW
C
      EXTERNAL NLEN
C
      ST = 0
      VIEW = VNOUN(496)
      IF(DPNUM.LT.1.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      ADDRES = 438+DPNUM
      IF(NLEN(VPUDEF).EQ.0) THEN
C         set just in case
          VPUDEF = 'Undefined'
      ENDIF
C     save into main storage
      VPNAME(DPNUM) = VPUDEF
C     save name into noun for popup
      VNOUN(ADDRES) = VIEW(1:NLEN(VIEW))//' '//VPUDEF
C
999   CONTINUE
      END
C
C
C      @(#)  256.1 date 12/16/89 port_screen_extents_inq.ftn 
      SUBROUTINE PORT_SCREEN_EXTENTS_INQ(DPNUM,XMIN,YMIN,XMAX,YMAX,ST)
C     ================================================================
C1    VARYPE                               I2   R    R     R   R   I4
C1    IOSTAT                               I    O    O     O   O   O
C
C2    Returns the extents of the requested viewport can be anything
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      REAL XMIN,YMIN
      REAL XMAX,YMAX
      INTEGER*4 ST
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      IF(DPNUM.EQ.0.OR.MAWS) THEN
          XMIN = VPXMIN
          YMIN = VPYMIN
          XMAX = VPXMAX
          YMAX = VPYMAX
      ELSE
          XMIN = VIEWEX(1,DPNUM)
          YMIN = VIEWEX(2,DPNUM)
          XMAX = VIEWEX(3,DPNUM)
          YMAX = VIEWEX(4,DPNUM)
      ENDIF
C
999   CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 port_screen_extents_set.ftn 
      SUBROUTINE PORT_SCREEN_EXTENTS_SET(DPNUM,XMIN,YMIN,XMAX,YMAX,ST)
C     ================================================================
C1    VARYPE                               I2   R    R     R   R   I4
C1    IOSTAT                               I    I    I     I   I   O
C
C2    Sets the sreen extents of a daxport. If MAWS is active then the values
C2    cannot be overwritten currently and error is returned. This routine
C2    does not set transforms only limits
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      REAL XMIN,YMIN
      REAL XMAX,YMAX
      INTEGER*4 ST
C
      ST = 0
C
      IF(MAWS) THEN
C         cannot alter maws
          ST = 3
          GOTO 999
      ENDIF
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ELSEIF(DPNUM.EQ.0) THEN
C         cannot alter main backcloth
          ST = 2
          GOTO 999
      ENDIF
C
      VIEWEX(1,DPNUM) = XMIN
      VIEWEX(2,DPNUM) = YMIN
      VIEWEX(3,DPNUM) = XMAX
      VIEWEX(4,DPNUM) = YMAX
C
999   CONTINUE
C
      END
C
C      @(#)  256.1 date 12/16/89 port_screen_load.ftn 
      SUBROUTINE PORT_SCREEN_LOAD(DPNUM,ST)
C     ====================================
C1    VARYPE                      I2   I4
C     IOSTAT                      I    O
C
C2    Loads the screen coord held into DAXCAD
C2    coordinates into viepwort
C2    Use SAVE to save them
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 I
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      IF(DPNUM.EQ.0.OR.MAWS) THEN
          VXMIN = VPXMIN
          VYMIN = VPYMIN
          VXMAX = VPXMAX
          VYMAX = VPYMAX
      ELSE
          VXMIN = VIEWEX(1,DPNUM)
          VYMIN = VIEWEX(2,DPNUM)
          VXMAX = VIEWEX(3,DPNUM)
          VYMAX = VIEWEX(4,DPNUM)
      ENDIF
C
      RETURN
C
999   CONTINUE
      END
 
 
 
C      @(#)  256.1 date 12/16/89 port_screen_save.ftn 
      SUBROUTINE PORT_SCREEN_SAVE(DPNUM,ST)
C     ===================================
C1    VARYPE                     I2   I4
C     IOSTAT                     I    O
C
C2    Saves the screen coordinates into viepwort
C2    Use LOAD to recover them. If naws is used then you cannot set
C2    screen extents anyway
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 I
C
      IF(MAWS) THEN
C         cannot alter maws
          ST = 3
          GOTO 999
      ENDIF
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ELSEIF(DPNUM.EQ.0) THEN
C         cannot alter main backcloth
          ST = 2
          GOTO 999
      ENDIF
C
      VIEWEX(1,DPNUM) = VXMIN
      VIEWEX(2,DPNUM) = VYMIN
      VIEWEX(3,DPNUM) = VXMAX
      VIEWEX(4,DPNUM) = VYMAX
C
999   CONTINUE
C
C
C
      END
 
C      @(#)  256.1 date 12/16/89 port_screen_to_hidden.ftn 
      SUBROUTINE PORT_SCREEN_TO_HIDDEN(DPNUM,ST)
C     ==========================================
C1    VARYPE                            I2   I4
C1    IOSTAT                            I    O
C
C2    Saves the current drawn screen to a hidden bitmap
C
      include 'include/viewport.inc'
      include 'include/icon.inc'
C
      INTEGER*2 DPNUM
      LOGICAL OK
      INTEGER*4 ST
C
      CALL ROPREP()
      IF(DPNUM.EQ.0) THEN
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
C         save the current screen full of data
          CALL SAVE_BITMAP(DPNUM,OK)
      ELSEIF(DPNUM.GT.0.AND.DPNUM.LE.MAXVP) THEN
          IF(.NOT.ICNUM(DPNUM)) THEN
              ORIGIN(1) = 0
              ORIGIN(2) = 0
              WINBLT(1) = VIEWEX(1,DPNUM)
              WINBLT(2) = VIEWEX(2,DPNUM)
              WINBLT(3) = VIEWPO(1,DPNUM)+1
              WINBLT(4) = VIEWPO(2,DPNUM)+1
              CALL SAVE_BITMAP(DPNUM,OK)
          ELSE
              ST = 2
              GOTO 999
          ENDIF
      ELSE
          ST = 1
          GOTO 999
      ENDIF
C     ST = 0
      RETURN
C
999   CONTINUE
C
      END
C
C      @(#)  256.1 date 12/16/89 port_set.ftn 
      SUBROUTINE PORT_SET(DPNUM,ST)
C     =============================
C1    VARYPE               I2   I4
C1    IOSTAT               I    O
C
C2    Sets port number
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
C
      ST = 0
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      CVPN = DPNUM
C
      RETURN
999   CONTINUE
C
      END
C
C      @(#)  256.1 date 12/16/89 port_set_name.ftn 
      SUBROUTINE PORT_SET_NAME(DPNUM,NAME,ST)
C     =======================================
C1    VARYPE                    I2 C*(*)I4
C1    IOSTAT                    I   I   O
C
C2    Sets the current viewport name
C
      include 'include/viewport.inc'
      include 'include/vnames.inc'
      include 'include/vntable.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 ADDRES
      INTEGER*4 NLEN
      INTEGER*4 NLEN1
      CHARACTER*(*) NAME
      CHARACTER*20 VIEW
C
      EXTERNAL NLEN
      EXTERNAL NLEN1
C
      ST = 0
      VIEW = VNOUN(496)
      IF(DPNUM.LT.1.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
      ADDRES = 438+DPNUM
C
C     save into main storage
      VPNAME(DPNUM) = NAME
C     save name into noun for popup
      VNOUN(ADDRES) = VIEW(1:NLEN(VIEW))//
     +                ' '//VPNAME(DPNUM)(1:NLEN1(VPNAME(DPNUM)))
 
999   CONTINUE
      END
C
C
C
C      @(#)  256.1 date 12/16/89 port_world_extents_inq.ftn 
      SUBROUTINE PORT_WORLD_EXTENTS_INQ(DPNUM,XMIN,YMIN,XMAX,YMAX,ST)
C     ================================================================
C1    VARYPE                               I2   R    R     R   R   I4
C1    IOSTAT                               I    O    O     O   O   O
C
C2    Sets the World extents of a portt.
C2    This routine  does not set transforms only limits
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      REAL XMIN,YMIN
      REAL XMAX,YMAX
      INTEGER*4 ST
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
C     Load up all values
C
      XMIN = VIEWPS(1,DPNUM+1)
      YMIN = VIEWPS(2,DPNUM+1)
      XMAX = VIEWPS(3,DPNUM+1)
      YMAX = VIEWPS(4,DPNUM+1)
C
999   CONTINUE
C
      END
C
C
C      @(#)  256.1 date 12/16/89 port_world_extents_set.ftn 
      SUBROUTINE PORT_WORLD_EXTENTS_SET(DPNUM,XMIN,YMIN,XMAX,YMAX,ST)
C     ================================================================
C1    VARYPE                               I2   R    R     R   R   I4
C1    IOSTAT                               I    I    I     I   I   O
C
C2    Sets the World extents of a portt.
C2    This routine  does not set transforms only limits
C
      include 'include/viewport.inc'
C
      INTEGER*2 DPNUM
      REAL XMIN,YMIN
      REAL XMAX,YMAX
      INTEGER*4 ST
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
C     Load up all values
C
      VIEWPS(1,DPNUM+1) = XMIN
      VIEWPS(2,DPNUM+1) = YMIN
      VIEWPS(3,DPNUM+1) = XMAX
      VIEWPS(4,DPNUM+1) = YMAX
C
999   CONTINUE
C
      END
C
C      @(#)  256.1 date 12/16/89 port_world_load.ftn 
      SUBROUTINE PORT_WORLD_LOAD(DPNUM,ST)
C     ====================================
C1    VARYPE                      I2   I4
C     IOSTAT                      I    O
C
C2    Loads the world coordinates into viepwort
C2    Use SAVE to save them
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 I
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      WXMIN = VIEWPS(1,DPNUM+1)
      WYMIN = VIEWPS(2,DPNUM+1)
      WXMAX = VIEWPS(3,DPNUM+1)
      WYMAX = VIEWPS(4,DPNUM+1)
C
      RETURN
C
999   CONTINUE
      END
C
C      @(#)  256.1 date 12/16/89 port_world_save.ftn 
      SUBROUTINE PORT_WORLD_SAVE(DPNUM,ST)
C     ===================================
C1    VARYPE                     I2   I4
C     IOSTAT                     I    O
C
C2    Saves the world coordinates into viepwort
C2    Use LOAD to recover them
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      INTEGER*2 DPNUM
      INTEGER*4 ST
      INTEGER*4 I
C
      ST = 0
C
      IF(DPNUM.LT.0.OR.DPNUM.GT.MAXVP) THEN
C         range error
          ST = 1
          GOTO 999
      ENDIF
C
      VIEWPS(1,DPNUM+1) = WXMIN
      VIEWPS(2,DPNUM+1) = WYMIN
      VIEWPS(3,DPNUM+1) = WXMAX
      VIEWPS(4,DPNUM+1) = WYMAX
C
      RETURN
C
999   CONTINUE
      END
 
