
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 segment.f   */


C
      SUBROUTINE SEG_BUILD(NUM,X1,Y1,X2,Y2,SEGLST,NUMSEG)
C     ===================================================
C1    VARTYPE               I2   I2(4)  ARR    I2
C1    IOSTS                 I    I      O      O
C
C2    This routine will return a list of segments to be drawn
C2    into the viewport NUM. It looks at the obscured list
C2    and splits the line segment into various bits. This
C2    can then be draw directly onto the screen. The segment coords
C2    must be in screen coords. This is determined by adding on the viewport
C2    offset postion
C
      include   'include/gpr.ins.inc'
      include   'include/viewport.inc'
      include   'include/apollo.inc'
      include   'include/icon.inc'
      include   'include/daxcolor.inc'
C
      INTEGER*2 SEGLST(4,20)
      INTEGER*2 NUMSEG
      INTEGER*4 I,J,ST
      INTEGER*4 STACK(5,20)
      INTEGER*4 STKP
      INTEGER*4 LIST(MAXVP)
      INTEGER*4 X1,Y1,X2,Y2
      INTEGER*4 CODE
      INTEGER*4 WINCLP,CLP
      LOGICAL VISAB
      INTEGER*4 OX1,OY1,OX2,OY2
      INTEGER*4 XMIN,YMIN,XMAX,YMAX,CBITM,DRAWV
      INTEGER*2 NUM
C
C     if an icon the dont bother
      IF(NUM.GT.0) THEN
           IF(ICNUM(NUM)) RETURN
      ENDIF
C     set number of segments
      NUMSEG = 0
C     segment has allready been clipped into place thus no need to clip
C     our own polygon
C     set current drawing bitmap
      CALL GPR_$INQ_BITMAP(CBITM,ST)
C     inquire about the draw value before setting
      DRAWV = TOOLPEN_COLOR
      CALL GPR_$SET_BITMAP(DISPDE,ST)
C     make sure both drawing correctly
      CALL TOOLPEN(DRAWV)
      J = 0
      IF(NUM.EQ.0) THEN
C         build a list of all viable windows to clip and do it
          DO 30 I=1,MAXVP
              IF(VPNUM(I)) THEN
                  J = J + 1
                  LIST(J) = I
              ENDIF
 30       CONTINUE
      ELSE
         DO 20 I=1,MAXVP
C     
C             build a list of polygons to clip segment
              IF(OBSLST(NUM,I).AND.I.NE.NUM) THEN
                 J = J + 1
                 LIST(J) = I
             ENDIF
 20      CONTINUE
      ENDIF
C
C     now clip the segment with the other polygons
      IF(J.EQ.0) THEN
C         special case no clipping reqd
          CALL GPR_$MOVE(INT2(X1),INT2(Y1),ST)
          CALL GPR_$LINE(INT2(X2),INT2(Y2),ST)
C         set current drawing bitmap
          CALL GPR_$SET_BITMAP(CBITM,ST)
          RETURN
      ENDIF
 
C
      STKP = 0
      CLP = 1
C     set segment values
      OX1 = X1
      OY1 = Y1
      OX2 = X2
      OY2 = Y2
 10   CONTINUE
C
C
C     determine clip window
      WINCLP = LIST(CLP)
      XMIN = VIEWEX(1,WINCLP) - 2
      YMIN = VIEWEX(2,WINCLP) - 2 
      XMAX = VIEWEX(3,WINCLP) + 2
      YMAX = VIEWEX(4,WINCLP) + 2
      OX1 = X1
      OY1 = Y1
      OX2 = X2
      OY2 = Y2
C     clippit
      CALL VCLIP(X1,Y1,X2,Y2,XMIN,YMIN,XMAX,YMAX,VISAB,CODE)
C     we are interested in a partial clip
C
 40   CONTINUE
      IF(CLP.EQ.J) THEN
C         segment has become fully visable and all windows have ben used
          IF(CODE.EQ.0) THEN
C
              CALL GPR_$MOVE(INT2(OX1),INT2(OY1),ST)
              CALL GPR_$LINE(INT2(OX2),INT2(OY2),ST)
          ELSEIF(CODE.EQ.2) THEN
C             2 segments to be drawn
              CALL GPR_$MOVE(INT2(OX1),INT2(OY1),ST)
              CALL GPR_$LINE(INT2(X1),INT2(Y1),ST)
              CALL GPR_$MOVE(INT2(X2),INT2(Y2),ST)
              CALL GPR_$LINE(INT2(OX2),INT2(OY2),ST)
          ELSEIF(CODE.EQ.4) THEN
C
              CALL GPR_$MOVE(INT2(OX1),INT2(OY1),ST)
              CALL GPR_$LINE(INT2(X1),INT2(Y1),ST)
          ELSEIF(CODE.EQ.3) THEN
C
              CALL GPR_$MOVE(INT2(OX2),INT2(OY2),ST)
              CALL GPR_$LINE(INT2(X2),INT2(Y2),ST)
          ENDIF
C         anythign on the stack
          IF(STKP.EQ.0) THEN
             CALL GPR_$SET_BITMAP(CBITM,ST)
             RETURN
          ENDIF
C         take of the stack and consider next segment
          X1 = STACK(1,STKP)
          Y1 = STACK(2,STKP)
          X2 = STACK(3,STKP)
          Y2 = STACK(4,STKP)
          CLP = STACK(5,STKP)
          STKP = STKP -1
          CLP = CLP +1
          GOTO 10
      ELSE
C         modfiye clipping segment and go
          IF(CODE.EQ.3) THEN
              X1 = X2
              Y1 = Y2
              X2 = OX2
              Y2 = OY2
          ELSEIF(CODE.EQ.4) THEN
              X2 = X1
              Y2 = Y1
              X1 = OX1
              Y1 = OY1
 
          ENDIF
      ENDIF
      IF(CODE.EQ.2) THEN
C         two segments to consider here
C         what the hell do we do with them
          STKP = STKP + 1
C         save other segment an clip it later
          STACK(1,STKP) = OX1
          STACK(2,STKP) = OY1
          STACK(3,STKP) = X1
          STACK(4,STKP) = Y1
C         save the window number and restart from here when
C         we come back to do this segment
          STACK(5,STKP) = CLP
C         new segment to be clipped
          X1 = X2
          Y1 = Y2
          X2 = OX2
          Y2 = OY2
C         incremrn
      ELSEIF(CODE.EQ.1) THEN
C         This is the end of this segment
          IF(STKP.GT.0) THEN
              CLP = J
              GOTO 40
          ELSE
C            reset bitmap and return
             CALL GPR_$SET_BITMAP(CBITM,ST)
             RETURN
          ENDIF
      ELSEIF(CODE.EQ.0) THEN
C         Segment now being reset
          X1 = OX1
          Y1 = OY1
          X2 = OX2
          Y2 = OY2
      ENDIF
      CLP = CLP + 1
      GOTO 10
      END
