C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 displayd.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE SPCDRW(ENT,TMIP,USET,M,FDRAW)
C     SUBROUTINE ZPVIEW()
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE SPCDRW(ENT,TMIP,USET,M,FDRAW)
C     ========================================
C1    vartype           I2   I2   L R(3,3) L
C1    iostatus          I    I    I   I    I
C
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/daxcolor.inc'
      include 'include/viewport.inc'
      include 'include/server.inc'
C
      INTEGER*4 TFONT
      INTEGER*4 DRWTYP
      INTEGER*2 ENT,TMIP,ENTCOL,NUM,I,VPN
      REAL M(3,3)
      LOGICAL OK,USET,FDRAW,ERASE,UPDATE,SPCVP
C
      EXTERNAL DIR500,DBR500,DTR500,DRWFLW,DRWFAW,DRWHT,
     +         DRWTXT,DRWDIM,PENDRW,SETAPN
C
C
C     ***************************************************
C         Section 1 to bring in the correct bitmap
C         for drawing into 
C     ***************************************************
C
      IF ( SERVER ) THEN
          RETURN
      ENDIF
      UPDATE=.FALSE.
      NUM=-1
C     if no display ops are being done the dont look a bitmap code
      IF(.NOT.VPADD.AND..NOT.VPMOV.AND..NOT.
     +   VPDEL.AND..NOT.VPCAN) GOTO 20
C
      IF(MVPACT) THEN
C         set counter
          NUM = MAXVP+1
C         we will draw into other views
          SPCVP = .TRUE.
      ELSE
C         we will leave other views alone
          SPCVP = .FALSE.
          GOTO 20
      ENDIF
10    CONTINUE
C     if view is active then do it
      IF(NUM.GT.1) THEN
C         All viewport update
          IF(VPNUM(NUM-1)) THEN
             VPN = NUM-1
C            set appropriate bitmaps for the draw
             CALL UPDBTM(NUM)
             UPDATE=.TRUE.
          ELSE
             NUM=NUM-1
C            try again since view was not active
             GOTO 10
          ENDIF
C         loop if this bitmap is not active
      ELSEIF(NUM.EQ.1) THEN
C         recover state of play
C         Update the swapper if necessary only in daxports
          IF(.NOT.MAWS) THEN
C             allways update the maws
              CALL UPDBTM(NUM)
          ELSE
              NUM=NUM-1
              GOTO 10
          ENDIF
      ELSEIF(NUM.EQ.0) THEN
          CALL UPDBTM(NUM)
C         if viewports dont draw into viewport
          IF(.NOT.MAWS) GOTO 999
      ENDIF
C     lets do the draw
 20   CONTINUE
C
C     ***************************************************
C         Section 2 Draw the entity into the bitmap
C     ***************************************************
C
C     set visibily flags befor draw
      DISPV=.FALSE.
C     entity erase in progress if active pencolor
C     is the erase colour
      ERASE=COLPEN.EQ.COLERS
C
      IF ( .NOT.ERASE.AND..NOT.VLAYER(IMBUFF(4)) ) THEN
          IF(NUM.GE.0) THEN
             GOTO 10
          ENDIF
          RETURN
      ENDIF
C
C     get font for drawing
      TFONT=IMBUFF(6)
C
      IF (.NOT.ERASE) THEN
C        get colour for entity
         ENTCOL=IMBUFF(3)
C        set the pen colour, this is  RUBBISH !!!!
C        cos we cannot rely on the current pen colour, we
C        have to set it explictly, this increases redraw time
C        by 10 - 15% (apollo GPR).
         CALL SETAPN(ENTCOL)
      END IF
C           1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
C          21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40
C          41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60
C          61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
C          81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100
      GOTO( 1, 2, 3, 1, 5, 1, 7, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 30,31, 1,33,33,33,33,33, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     +    , 1, 1, 1, 1,85, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
     1     ) ENT
C
 1    CONTINUE
         GOTO 999
 2    CONTINUE
C        marker drawing routine
         CALL DRWMRK(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),RDBUFF(6),IMBUFF(5),IMBUFF(6))
         GOTO 999
C
 3    CONTINUE
C        Line to be drawn
         CALL DRWFLW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),TFONT)
         GOTO 999
 5    CONTINUE
C        Arc to be drawn
         CALL DRWFAW(RDBUFF(1),RDBUFF(2),RDBUFF(4),
     +               RDBUFF(5),RDBUFF(6),TFONT)
         GOTO 999
 7    CONTINUE
C        Spline to be drawn            
         DRWTYP = 0
         CALL DRWSPL(TMIP,USET,M,DRWTYP)
         GOTO 999
 30   CONTINUE
C        Center Line to be drawn
         CALL DRWCEN(.TRUE.)
         GOTO 999
 31   CONTINUE
C        Hatch to be drawn
         CALL DRWHT(TMIP,USET,M)
         GOTO 999
 33   CONTINUE
C        dimension to be drawn
         CALL DRWDIM()
         GOTO 999
 85   CONTINUE
C        Text to be drawn
         CALL  DRWTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),
     +                RDBUFF(4),RDBUFF(5),RDBUFF(6),
     1                CBUFF)
         GOTO 999
 999  CONTINUE
C
C     ***************************************************
C         Section 3 Update any display files now
C     ***************************************************
C
C     are we using viewporting adding
      IF(VPADD.OR.VPMOV.OR.VPDEL.OR.VPCAN) THEN
C         reset default drawing code
          DDCODE = 0
C         reset segemnet control flag
          VPSEGF = .FALSE.
          CALL UPDDF(TMIP,ENT,NUM,DISPV)
C         Go back again if you need to do it again
          IF(NUM.GE.0) GOTO 10
C         get layer conditions
          CALL GETLAY(CVPN,OK)
      ENDIF
C
400   CONTINUE
C
      END
      SUBROUTINE ZPVIEW()
C     ===================
C
C1    Subroutine ZPVIEW produces a screen display
C1    using the limits of the previous viewport
C
      include 'include/wtov.inc'
      include 'include/viewport.inc'
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/journal.inc'
C
      LOGICAL OK
      EXTERNAL REGEND,OLDVPT,SETW2V
C
C     get the old details
      CALL OLDVPT()
      CALL SETW2V()
C     generate the new display
      VIEWPS(1,CVPN+1) = WXMIN
      VIEWPS(2,CVPN+1) = WYMIN
      VIEWPS(3,CVPN+1) = WXMAX
      VIEWPS(4,CVPN+1) = WYMAX
      CALL REGEND()
C     journaling bit for macro
      IF(JOURON)  CALL WRTJRN(0.0,0.0,'m',JRNCMD(9),0)
C
      END
C
