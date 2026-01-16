C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah5.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DEW500(P,OK)
C     SUBROUTINE DEW501(P,OK)
C     SUBROUTINE DEW533(P,OK)
C     SUBROUTINE DEW535(P,OK)
C     SUBROUTINE DEW537(P,OK)
C     SUBROUTINE DEW566(M,P,OK)
C     SUBROUTINE DEWC02(X1,Y1,ANGLE,SCALX,SCALY,FORM,TFONT,TLAY,P,OK)
C     SUBROUTINE DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
C     SUBROUTINE DEWC05(X,Y,R,ANG1,ANG2,TFONT,TLAY,P,OK)
C     SUBROUTINE DEWC07(FORM,TLAY,P,PD,OK)
C     SUBROUTINE DEWC08(FORM,TLAY,TFONT,P,PD,OK)
C     SUBROUTINE DEWC30(CENX,CENY, MAJRAD,MINRAD, ROTANG, BRDRSZ,
C     SUBROUTINE DEWC85(X,Y,TW,TH,TANG,TSLT,TJST,TFONT,
C     SUBROUTINE DEWDIM(P,DIMTYP,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DEW500(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEW500 writes the entity data to next position
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    The next entry pointers are updated before return,so that
C2    sequential calls may be made with the same data,to create
C2    multiple entities.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P,P1,P2,P3
      LOGICAL OK
C
      EXTERNAL DIW500,DBW500,DTW500,ADDISP,ADDMON
C
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C
C     take care of text in here
      IF ( IMBUFF(2) .EQ. TEXT ) THEN
C        set text pointer to next free space
         IMBUFF(9)= NTXPOS
      ELSE
C        should be no text pointer
         IMBUFF(9)=0
      END IF
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
      IF ( IMBUFF(9) .NE. 0 ) THEN
         CALL DTW500(P1,OK)
      END IF
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=MIP
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(IDBUFF(2),IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
      END
*
      SUBROUTINE DEW501(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEW500 writes the entity data to next position
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    The connectivity pointer
C2    is set to the next available record after the current one.
C2    The next entry pointers are updated before return,so that
C2    sequential calls may be made with the same data,to create
C2    multiple entities.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P,P2,P3
      LOGICAL OK
C
      EXTERNAL DIW500,DTW500,ADDISP,DBW501,ADDMON
C
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     take care of text in here
      IF ( IMBUFF(2) .EQ. TEXT ) THEN
         IMBUFF(9)= NTXPOS
         CALL DTW500(P,OK)
      ELSE
C        should be no text pointer
         IMBUFF(9)=0
      END IF
C
C     write the MI data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C     now write the PD data
      CALL DBW501(P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 holds the Display File Pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
      END
C
C
      SUBROUTINE DEW533(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEW533 writes the part data for a Linear
C2    Dimension into the master index file. This calls the
C2    General Dimension Database write routine , DEWDIM.
C
      include  'include/entity.inc'
C
      INTEGER*2 P
      LOGICAL OK
C
      EXTERNAL DEWDIM
C
C     Call general dimension storage routine.
      CALL DEWDIM(P,LDIMN,OK)
      END
C
**
      SUBROUTINE DEW535(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEW535 writes the part data for a radial
C2    Dimension into the master index file. This calls the
C2    General Dimension Database write routine , DEWDIM.
C
      include  'include/entity.inc'
C
      INTEGER*2 P
      LOGICAL OK
C
      EXTERNAL DEWDIM
C
C     Call general dimension storage routine.
      CALL DEWDIM(P,RDIMN,OK)
C
      END
*
      SUBROUTINE DEW537(P,OK)
C     =======================
C1    vartype           I2 L
C1    iostatus          O  O
C
C2    Subroutine DEW537 writes the part data for a Diametral
C2    Dimension into the master index file. This calls the
C2    General Dimension Database write routine , DEWDIM.
C
      include  'include/entity.inc'
C
      INTEGER*2 P
      LOGICAL OK
C
      EXTERNAL DEWDIM
C
C     Call general dimension storage routine.
      CALL DEWDIM(P,DDIMN,OK)
C
      END
**
      SUBROUTINE DEW566(M,P,OK)
C     =========================
C1    vartype        R(3,3) I2 L
C1    iostatus         I    O  O
C
C2    Subroutine DEW566 writes the entity data to next position
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    Intended for use wtih COMPONENT/SYMBOL INSTANCES
C2    The next entry pointers are updated before return,so that
C2    sequential calls may be made with the same data,to create
C2    multiple entities.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/wrkdat.inc'
C
      REAL M(3,3)
      INTEGER*4 I
      INTEGER*2 P,P2,P3
      LOGICAL OK
C
      EXTERNAL DIW500,DBW551,DBW550,DTW500,ADDISP
C
C     fault handler
      CALL ENTDAT(.TRUE.)
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C
      IMBUFF(9)= NTXPOS
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
      CALL DTW500(P3,OK)
C
      DO 50 I=1,5
C        ensure that entity type is same in both references
C        if one wrong,best make both wrong!!! (or correct?)
         IWORK(1,I)=IMBUFF(2)
C        ensure that MI pointer in PDF is correct
         IWORK(2,I)=P
C        clear spare pointer in pd
         IWORK(4,I)=0
C
  50  CONTINUE
C
C     write first row of transform matrix
      DO 20 I=1,3
         RWORK(I,3)=M(1,I)
         RWORK(I+3,3)=0.0
 20   CONTINUE
C     write second row of transform matrix
      DO 21 I=1,3
         RWORK(I,4)=M(2,I)
         RWORK(I+3,4)=0.0
 21   CONTINUE
C     write third row of transform matrix
      DO 22 I=1,3
         RWORK(I,5)=M(3,I)
         RWORK(I+3,5)=0.0
 22   CONTINUE
C     now write the PD data with continuation
      CALL DBW551(IWORK(1,1),RWORK(1,1),P2,OK)
      CALL DBW551(IWORK(1,2),RWORK(1,2),P2,OK)
      CALL DBW551(IWORK(1,3),RWORK(1,3),P2,OK)
      CALL DBW551(IWORK(1,4),RWORK(1,4),P2,OK)
C     write last record to pdf
      CALL DBW550(IWORK(1,5),RWORK(1,5),P2,OK)
C
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
C
C
      SUBROUTINE DEWC02(X1,Y1,ANGLE,SCALX,SCALY,FORM,TFONT,TLAY,P,OK)
C     =============================================================
C1    vartype           R  R    R     R     R    I2   I2   I2 I2 L
C1    iostatus          I  I    I     I     I    I    I    I  O  O
C
C2    Subroutine DEWC02 writes the entity data for the marker
C2    at the point X1,Y1 and angle ANGLE with scale SCALX,SCALY
c2    and marker number index FORM in font FONT
C2    in the master index file and enters all data in the
C2    part data file.
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,FORM,TLAY
      REAL X1,Y1,ANGLE,SCALX,SCALY
      LOGICAL OK,SAME
C
      EXTERNAL FILL,DIW500,DBW500,ADDMON,ADDISP,SAME
C     fault handler
      CALL ENTDAT(.TRUE.)
      OK=.FALSE.
C     Check to see if it is a zero length line
C        initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=MARKER
C     set the form index number
      IMBUFF(5)=FORM
C     current line thickness
C SPB - 031194 
      IMBUFF(12)=THICK
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     should be no text pointer
      IMBUFF(9)=0
C
      IMBUFF(4)=  TLAY
C
C     ensure current construction line font is used
      IMBUFF(6)=  TFONT
C     put the line data into buffer
      RDBUFF(1)=  X1
      RDBUFF(2)=  Y1
      RDBUFF(3)=  0.0
      RDBUFF(4)=  ANGLE
      RDBUFF(5)=  SCALX
      RDBUFF(6)=  SCALY
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C****************************************************
C****************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C****************************************************
C****************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
 
      SUBROUTINE DEWC03(X1,Y1,X2,Y2,TFONT,TLAY,P,OK)
C     ==============================================
C1    vartype           R  R  R  R  I2    I2   I2  L
C1    iostatus          I  I  I  I  I     I     O  O
C
C2    Subroutine DEWC03 writes the entity data for the line
C2    between the points X1,Y1 and X2,Y2 with current font
C2    in the master index file and enters all data in the
C2    part data file.
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,TLAY
      REAL X1,Y1,X2,Y2
      LOGICAL OK,SAME
C
      EXTERNAL FILL,DIW500,DBW500,ADDMON,ADDISP,SAME
C     fault handler
      CALL ENTDAT(.TRUE.)
      OK=.FALSE.
C     Check to see if it is a zero length line
      IF(.NOT.(SAME(X1,X2).AND.SAME(Y1,Y2))) THEN
C        initialize the MI buffer record
         CALL FILL()
C        set entity type component of MI record
         IMBUFF(2)=LINE
C        current line thickness
         IMBUFF(12)=THICK
C        ensure PD pointer in MI file is correct
         IMBUFF(7)=NPDPOS
C        should be no text pointer
         IMBUFF(9)=0
C
         IMBUFF(4)=  TLAY
C
C        ensure current construction line font is used
         IMBUFF(6)=  TFONT
C        put the line data into buffer
         RDBUFF(1)=  X1
         RDBUFF(2)=  Y1
         RDBUFF(3)=  0.0
         RDBUFF(4)=  X2
         RDBUFF(5)=  Y2
         RDBUFF(6)=  0.0
C
C        write the M I data from buffer
         CALL DIW500(P,OK)
         IF (.NOT.OK) RETURN
C        P now contains MI position used
C        ensure MIP points to correct entry for
C        this entity
         MIP=P
C
C        ensure that entity type is same in both references
C        if one wrong,best make both wrong!!! (or correct?)
         IDBUFF(1)=IMBUFF(2)
C        ensure that MI pointer in PDF is correct
         IDBUFF(2)=P
C        now write the PD data
         CALL DBW500(P2,OK)
C        P2 returns the PDF position used
C        no need to do any more,OK flag
C        will reflect success or not.
C
C****************************************************
C****************************************************
C        Add entity to display file
C        This should be done elsewhere,but is needed
C        here at the moment.
         CALL ADDISP(P,IMBUFF(2),P3,OK)
C        P3 returns the Display File pointer
         CALL ADDMON(IMBUFF(4))
C****************************************************
C****************************************************
      END IF
C     fault handler
      CALL ENTDAT(.FALSE.)
C
      END
C
C
      SUBROUTINE DEWC05(X,Y,R,ANG1,ANG2,TFONT,TLAY,P,OK)
C     ==================================================
C1    vartype           R R R  R    R  I2 L
C1    iostatus          I I I  I    I  O  O
C
C2    Subroutine DEWC05 writes the entity data for the arc
C2    centred on X,Y radius R,start angle ANG1,end angle ANG2
C2    using current line font
C2    in the master index file and enters all data in the
C2    part data file.
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,TLAY
      REAL X,Y,R,ANG1,ANG2,PI,ZERO,TWO
      LOGICAL OK,SAME
C
      EXTERNAL FILL,DIW500,DBW500,ADDISP,ADDMON,SAME,PI
C
C     fault handler
      CALL ENTDAT(.TRUE.)
      ZERO = 0.0
      TWO = 2.0
C     initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=ARC
C      WRITE(10,*) '[DEWC05] ENT= ',IMBUFF(2),ARC
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C
      IMBUFF(4)=  TLAY
C
C     current line thickness
      IMBUFF(12)=THICK
C     ensure current construction line font is used
      IMBUFF(6)=TFONT
C     put the line data into buffer
      RDBUFF(1)=  X
      RDBUFF(2)=  Y
      RDBUFF(3)=  0.0
      RDBUFF(4)=  R
      RDBUFF(5)=  ANG1
      IF ( SAME(ZERO,ANG2) ) THEN
          ANG2 = PI(TWO)
      ENDIF
 
      RDBUFF(6)=  ANG2
C
C     should be no text pointer
      IMBUFF(9)=0
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(IDBUFF(2),IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
C
      SUBROUTINE DEWC07(FORM,TLAY,P,PD,OK)
C     ====================================
C1    vartype           I      I2 I2 I2  I2
C1    iostatus          I      I  O  O   O
C
C2    Subroutine DEWC07 writes a header for a Spline Curve
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,TLAY,PD
      INTEGER*4 NP,FUNIT,I,FORM
      REAL X1,Y1,X2,Y2
      LOGICAL OK
C
      EXTERNAL FILL,DIW500,DBW500,ADDMON,ADDISP
C
C     fault handler
      CALL ENTDAT(.TRUE.)
C     initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=SPLINE
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     current line thickness
      IMBUFF(12)=THICK
C     should be no text pointer
      IMBUFF(9)=0
C
      IMBUFF(4)=  TLAY
C
C     Store the type of spline we have
      IMBUFF(5)=FORM
C
      TFONT=1
      IMBUFF(6)=  TFONT
C     put the line data into buffer
C
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C
C     now write the PD data
C
      RDBUFF(1)=0.0
      RDBUFF(2)=0.0
      RDBUFF(3)=0.0
      RDBUFF(4)=0.0
      RDBUFF(5)=0.0
      RDBUFF(6)=0.0
C     write out two header records for storing
C     end point locks
      CALL DBW501(P2,OK)
      CALL DBW500(PD,OK)
 
C      DO 30 I=1,NP-1
C         READ(UNIT=FUNIT,REC=I) RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                          RDBUFF(4),RDBUFF(5),RDBUFF(6)
C         CALL DBW501(P2,OK)
C         IF ( .NOT. OK ) THEN
C         END IF
C 30   CONTINUE
C      READ(UNIT=FUNIT,REC=NP) RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                        RDBUFF(4),RDBUFF(5),RDBUFF(6)
C      CALL DBW500(P2,OK)
C
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
C
C     --------------------------------------------------
C
      SUBROUTINE DEWC08(FORM,TLAY,TFONT,P,PD,OK)
C     ====================================
C1    vartype           I      I2 I2 I2  I2
C1    iostatus          I      I  O  O   O
C
C2    Subroutine DEWC07 writes a header for a Spline Curve
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      INTEGER*2 P,P2,P3,TFONT,TLAY,PD
      INTEGER*4 NP,FUNIT,I,FORM
      REAL X1,Y1,X2,Y2
      LOGICAL OK
C
      EXTERNAL FILL,DIW500,DBW500,ADDMON,ADDISP
C
C     fault handler
      CALL ENTDAT(.TRUE.)
C     initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=SPLINE
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     current line thickness
C SPB - 031194 
      IMBUFF(12)=THICK
C     should be no text pointer
      IMBUFF(9)=0
C
      IMBUFF(4)=  TLAY
C
C     Store the type of spline we have
      IMBUFF(5)=FORM
C
      IMBUFF(6)=  TFONT
C     put the line data into buffer
C
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
C
C     now write the PD data
C
      RDBUFF(1)=0.0
      RDBUFF(2)=0.0
      RDBUFF(3)=0.0
      RDBUFF(4)=0.0
      RDBUFF(5)=0.0
      RDBUFF(6)=0.0
C     write out two header records for storing
C     end point locks
      CALL DBW501(P2,OK)
      CALL DBW500(PD,OK)
 
C      DO 30 I=1,NP-1
C         READ(UNIT=FUNIT,REC=I) RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                          RDBUFF(4),RDBUFF(5),RDBUFF(6)
C         CALL DBW501(P2,OK)
C         IF ( .NOT. OK ) THEN
C         END IF
C 30   CONTINUE
C      READ(UNIT=FUNIT,REC=NP) RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                        RDBUFF(4),RDBUFF(5),RDBUFF(6)
C      CALL DBW500(P2,OK)
C
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
C
C     --------------------------------------------------
C
      SUBROUTINE DEWC30(CENX,CENY, MAJRAD,MINRAD, ROTANG, BRDRSZ,
C     ===========================================================
C1    VARTYPE            R    R      R      R       R       R    
C1    IOSTATUS           I    I      I      I       I       I    
     +                  CLTYPE,CLINEF,RETP)
C                       ===================
C1    VARTYPE             I2     I2    I2
C1    IOSTATUS            I      I     O
C
C2    Write a 'Center_line' record to the database using the data
C2    passed.
C
      include 'include/masti.inc'
      include 'include/style.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
C
      REAL CENX,CENY, MAJRAD,MINRAD, ROTANG, BRDRSZ
      INTEGER*2 P,P2,P3,RETP,CLTYPE,CLINEF
      LOGICAL OK
C                     
      IF (MAJRAD.GT.0.0) THEN
C        initialize the MI buffer record
         CALL FILL()
C        set entity type component of MI record
         IMBUFF(2) = CENLIN
C        Use current layer.
         IMBUFF(4) = CLAYER
C        ensure center_line font is used
         IMBUFF(6) = CLINEF
C        ensure PD pointer in MI file is correct
         IMBUFF(7) = NPDPOS
C        should be no text pointer
         IMBUFF(9) = 0
C        No thickness permitted.
C SPB - 031194
         IMBUFF(12) = THICK
C        put the line data into buffer
         RDBUFF(1) = CENX
         RDBUFF(2) = CENY
         RDBUFF(3) = MAJRAD
         RDBUFF(4) = MINRAD
         RDBUFF(5) = ROTANG
         RDBUFF(6) = BRDRSZ
C
C        write the M I data from buffer
         CALL DIW500(P,OK)
         RETP = P
         IF (.NOT.OK) RETURN
C        P now contains MI position used
C        ensure MIP points to correct entry for
C        this entity
         MIP=P
C
C        ensure that entity type is same in both references
C        if one wrong,best make both wrong!!! (or correct?)
         IDBUFF(1)=IMBUFF(2)
C        ensure that MI pointer in PDF is correct
         IDBUFF(2)=P
C        and the type of center line.
         IDBUFF(4)=CLTYPE
C        now write the PD data
         CALL DBW500(P2,OK)
C        P2 returns the PDF position used
C        no need to do any more,OK flag
C        will reflect success or not.
C
C****************************************************
C****************************************************
C        Add entity to display file
C        This should be done elsewhere,but is needed
C        here at the moment.
         CALL ADDISP(P,IMBUFF(2),P3,OK)
C        P3 returns the Display File pointer
         CALL ADDMON(IMBUFF(4))
C****************************************************
C****************************************************
      ENDIF
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DEWC85(X,Y,TW,TH,TANG,TSLT,TJST,TFONT,
     +                  TCOL,TLAY,TEXTS,P,OK)
C     ==============================================================
C1    vartype           R R R  R    R    R    I2  I2 I2 I2 C*(*) I2 L
C1    iostatus          I I I  I    I    I    I   I  I  I   I    O  O
C
C2    Subroutine DEWC85 writes the entity data to next position
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    the common block. P is returned with the MI destination used.
C2    MASTI which may be found in INCLUDE file
C2    'MASTI.INC'.Logical flag OK is returned TRUE
C2    if the operation was successful.
C2    The next entry pointers are updated before return,so that
C2    sequential calls may be made with the same data,to create
C2    multiple entities.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
      include  'include/style.inc'
C
      REAL X,Y,TW,TH,TANG,TSLT,TCODE
      INTEGER*2 P,P2,P3,TLAY,TFONT,NCHAR,TJST,TCOL
      INTEGER*4 NLEN1
      LOGICAL OK
      CHARACTER*(*) TEXTS
C
      EXTERNAL DIW500,DBW500,DTW500,ADDISP,ADDMON,NLEN1
C
C     fault handler
      CALL ENTDAT(.TRUE.)
      CALL FILL()
C
      IMBUFF(7)=NPDPOS
C
C     take care of text in here
      IMBUFF(2)=TEXT
      IMBUFF(3)=TCOL
      IMBUFF(4)=TLAY
C     current line thickness
C SPB - 031194
      IMBUFF(12)=THICK
      IMBUFF(6)=TFONT
      IMBUFF(9)=NTXPOS
C
C     write the M I data from buffer
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=P
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=P
      CBUFF=TEXTS
      CALL DTW500(P,OK)
C     ensure that entity type is same in both references
C     if one wrong,best make both wrong!!! (or correct?)
      IDBUFF(1)=IMBUFF(2)
      RDBUFF(1)=X
      RDBUFF(2)=Y
      RDBUFF(3)=TW
      RDBUFF(4)=TH
C     ensure Base Angle is in range 0-360 degrees
      RDBUFF(5)=MOD(TANG,360.0)
C     encode the slant and number of characters
      NCHAR=NLEN1(TEXTS)
      CALL CODET(TSLT,TJST,NCHAR,TCODE)
      RDBUFF(6)=TCODE
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(IDBUFF(2),IMBUFF(2),P3,OK)
C     P3 returns the Display File pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C      WRITE(10,*) '********* DEWC85 *****************'
C      WRITE(10,*) X,Y,TW,TH,TANG,TCODE,TLAY,P
C      WRITE(10,*) '"',TEXTS,'"'
C      WRITE(10,*) '**********************************'
      P=MIP
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
C
C     --------------------------------------------------
C
      SUBROUTINE DEWDIM(P,DIMTYP,OK)
C     ==============================
C1    vartype           I2 I2    L
C1    iostatus          O  I2    O
C
C2    Subroutine DEWDIM writes the entity data for any dimension
C2    in the master index file and enters all data in the
C2    part data file,the origin of the data for writing being
C2    RWORK and MIBUFF. Dimension type passed in DIMTYP
C2    P is returned with the MI destination used.
C2    Logical flag OK is returned TRUE if the operation was successful
C2    The next entry pointers are updated before return,so that
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/wrkdat.inc'
      include  'include/entity.inc'
      include  'include/dimendat.inc'
C
      INTEGER*2 P,P2,I,P3,J,DIMTYP
      LOGICAL OK
C
      EXTERNAL DIW500,DBW550,DBW551,ADDMON,DTW550,DTW551,ADDISP,FILL

C
C
C     fault handler
      CALL ENTDAT(.TRUE.)
C     initialize the MI buffer record
      CALL FILL()
C     set entity type component of MI record
      IMBUFF(2)=DIMTYP
C     point to text entry for dimension
      IMBUFF(9)=NTXPOS
C     ensure PD pointer in MI file is correct
      IMBUFF(7)=NPDPOS
C     store dimension control data
      IMBUFF(5)=CTRDAT(1)
C     ensure MIP points to correct entry for
C     this entity
      MIP=NMIPOS
C     write the MI data from buffer to next free position
      CALL DIW500(P,OK)
      IF (.NOT.OK) RETURN
C     P now contains MI position used
C     write the DIMENSION TEXT to text file
      IF ( RTALLY(2) .GT. 1  ) THEN
C        more than one text record so need continuation pointers.
         DO 55 I=1,RTALLY(2)-1
C           write out from scratch array
            CALL DTW551(DIMCHR(I),P2,OK)
 55      CONTINUE
C        now write out last text with no continuation ponter
         CALL DTW550(DIMCHR(RTALLY(2)),P2,OK)
      ELSE
C        Only one string ,write text with no continuation ponter
         CALL DTW550(DIMCHR(RTALLY(2)),P2,OK)
      END IF
C     Write out the DIMENSION PART DATA to PD file
C     sum the total number of records used
      I=RECCNT(1)
C      I=RTALLY(1)+RTALLY(2)+RTALLY(3)+RTALLY(4)+RTALLY(5)+RTALLY(6)
C     +                    +RTALLY(7)+RTALLY(8)+RTALLY(9)+RTALLY(10)
C     Set original data records to indicate dimension type
      IWORK(1,1)=DIMTYP
C     ensure that MI pointer in PDF is correct
      DO 70 J=1,I
C        ensure that MI pointer in PDF is correct
         IWORK(2,J)=P
 70   CONTINUE
C     now write the PD data with continuation
      DO 80 J=1,I-1
         CALL DBW551(IWORK(1,J),RWORK(1,J),P2,OK)
 80   CONTINUE
C     write last record to pdf without continuation.
      CALL DBW550(IWORK(1,I),RWORK(1,I),P2,OK)
C     P2 returns the PDF position used
C     no need to do any more,OK flag
C     will reflect success or not.
C
C*************************************************
C*************************************************
C     Add entity to display file
C     This should be done elsewhere,but is needed
C     here at the moment.
      CALL ADDISP(P,IMBUFF(2),P3,OK)
C     P3 holds the Display File Pointer
      CALL ADDMON(IMBUFF(4))
C*************************************************
C*************************************************
C
C     fault handler
      CALL ENTDAT(.FALSE.)
      END
**
