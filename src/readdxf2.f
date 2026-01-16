C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 readdxf2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DEWD56(FN,TTMIP,LOCAL,PMIP,OK)
C     SUBROUTINE DXCPET()
C     SUBROUTINE INSC11(CMPNAM,FN,PMIP,OK)
C     SUBROUTINE RDXDEC(OUNIT,LINEP,EOF,OK)
C     SUBROUTINE RDXECI(OUNIT,LINEP,GC,GV,OK)
C     SUBROUTINE RDXMTX(M)
C     SUBROUTINE RDXRBK(OUNIT,LINEP,EOF,OK)
C     SUBROUTINE RDXRPT(OUNIT,LINEP,GC,GV,OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE DEWD56(FN,TTMIP,LOCAL,PMIP,OK)
C     =========================================
C
C1    vartype            I4 I2     L    I2   L
C1    iostatus           I  I      I    O    O
C
C2    Subroutine DEW056 writes to the database
C2    the definition of a COMPONENT of entities
C2    using the relation file entry to define the
C2    group from that currently in workfiles starting
C2    at MI position TTMIP.
C2    FN = 1 COMPONENT
C2    FN = 2 SYMBOL
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
      include 'include/movdat.inc'
C
      INTEGER*2 I2,P,P2,PMIP,TTMIP,LTTMIP,COMSYM
      INTEGER*2 TMIP
      INTEGER*4 FN,NRRECS,NENTS,NXTENT,I,J,II
      LOGICAL OK,LOCAL
C
      EXTERNAL FILL,DIW500,DRW950,DRW951,DIR500,DIM500
      EXTERNAL DTW500,DBW500
C
C     save pointer to last entity in list
      LTTMIP=NMIPOS-1
C
      IF (.NOT.LOCAL) THEN
C        clear MI buffer to defaults
         CALL FILL()
      END IF
C     set correct entity type
      IF (FN.EQ.1) THEN
         COMSYM=COMPM
      ELSE
         COMSYM=SYMBM
      END IF
      IMBUFF(2)=COMSYM
C     ensure font is zero
      IMBUFF(6)=0
C     ensure PDP is next free space
      IMBUFF(7)=NPDPOS
C     ensure TXTP is next free space for name
      IMBUFF(9)=NTXPOS
C     set pointer to relation header
      IMBUFF(10)=NRLPOS
C     now write header to MI file
      CALL DIW500(PMIP,OK)
C     PMIP now contains MI position used
C     ensure MIP points to correct entry for
C     this entity
      MIP=PMIP
C     write text to file
      CALL DTW500(P,OK)
C     ensure entity types agree
      IDBUFF(1)=IMBUFF(2)
C     ensure that MI pointer in PDF is correct
      IDBUFF(2)=MIP
C     now write the PD data
      CALL DBW500(P2,OK)
C     P2 contains pointer to entry in PDF
C     save the pointers
C
C     *********************************************
C     *********************************************
C
C     create relation structure for entities between
C     TTMIP and LTTMIP
C
C     set the entity status to component master
      DO 50 I2=TTMIP,LTTMIP
C        read MI data for entity
         CALL DIR500(I2,OK)
C        set entity status
         IMBUFF(1)=COMSYM
C        set PMIP for entity
         IMBUFF(8)=PMIP
C        detach individual properties
         IMBUFF(11)=0
C        write MI data back to file
         CALL DIM500(I2,OK)
 50   CONTINUE
C
C     reset the pointers
C     now fill a relation record with the
C     header data for the relation data
C     set relation record type
      RLBUFF(1)=COMSYM
C     save parent MI in relation
      RLBUFF(3)=PMIP
C     clear relation buffer
      DO 10 I=4,10
         RLBUFF(I)=0
 10   CONTINUE
C
C     find number of entities in group
      NENTS=(LTTMIP-TTMIP)+1
C     find number of relation records required
      NRRECS=NENTS/7
      IF (NENTS.GT.NRRECS*7) NRRECS=NRRECS+1
C     save number of records
      RLBUFF(4)=NRRECS
C     save number of entities
      RLBUFF(5)=NENTS
C     write relation record
      CALL DRW951(P,OK)
C
C     now write MILIST to relation
      RLBUFF(1)=MILIST
C
      NXTENT=TTMIP
      IF (NRRECS.EQ.1) GOTO 40
      DO 30 I=1,NRRECS-1
C        fill the record with MI pointers
         DO 35 J=4,10
C           place the MI in the relation list
            RLBUFF(J)=NXTENT
            NXTENT=NXTENT+1
 35      CONTINUE
C        write the record to relation file
         CALL DRW951(P,OK)
 30   CONTINUE
C
 40   CONTINUE
C     last relation record to be written
C     clear relation buffer
      DO 20 I=4,10
         RLBUFF(I)=0
 20   CONTINUE
C     fill the record with MI pointers
      I=4
      DO 31 I2=NXTENT,LTTMIP
         RLBUFF(I)=I2
         I=I+1
 31   CONTINUE
C     write the last record to relation file
      CALL DRW950(P,OK)
C
      END
C
 
      SUBROUTINE DXCPET()
C     =======================
C1    vartype
C1    iostatus
C
C2    Subroutine DXCPET runs through the datebase
C2    looking for any component masters or
C2    component instances. If any are found it then
C2    checks to see if they have a .CMP extension
C2    on them and if not modifies the database entry
C2    so that they do this will allow us to regenerate
C2    if we wish after saving the drawing
 
C     Note :- This is done after readdxf has resolved
C             everything out.
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
 
      INTEGER*2  P,TMIP
      INTEGER*4  INDEX,IN,I,LS,NLEN
      LOGICAL    OK
 
      EXTERNAL  NLEN
      INTRINSIC INDEX
 
C     For all the masters search the database NMIPOS ?
 
      DO 10 P = 1, NMIPOS-1
C         Do a DIR500 to read it
          CALL DIR500(P,OK)
 
C         Do a DTR500 only if IMBUFF(2) = COMPM or COMPI
          IF ((IMBUFF(2).EQ.COMPM) .OR.
     +        (IMBUFF(2).EQ.COMPI)) THEN
C             IMBUFF(9) Gives the text pointer
              TMIP = IMBUFF(9)
              CALL DTR500(TMIP,OK)
 
C             Get the length and then remove any _cmp
              LS = NLEN(CBUFF)
              IN = 0
              DO 15 I = LS , 1, -1
                 IF (CBUFF(I:I).EQ.'_') THEN
                    IN = INDEX(CBUFF(I:),'_cmp')
C                   If not zero add on rest on string
                    IF (IN.NE.0) THEN
                       IN = IN + I - 1
                    ENDIF
                    GOTO 30
                 ENDIF
 15           CONTINUE
 30           CONTINUE
 
              IF (IN.NE.0) THEN
                  DO 20 I = IN, LS
                      CBUFF(I:I) = ' '
 20               CONTINUE
              ENDIF
 
C             Add the extension if not there
              CALL SUFFIX(CBUFF,'.cmp')
 
C             Write the changes back
              CALL DTM500(TMIP,P,OK)
          ENDIF
 10   CONTINUE
 
      END
      SUBROUTINE INSC11(CMPNAM,FN,PMIP,OK)
C     =======================================
C
C1    vartype           C*(*)   I4   I2  L
C1    iostatus            I     I    O   O
C
C2    Subroutine INSC10 searches within the current
C2    database for an entity of type SYMBM or COMPM
C2    based on the value of FN, and name CMPNAM
C2    returning the PMIP of the master if found
C2    OK indicates master found within existing file.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*4 FN,NLEN1,I
      INTEGER*2 PMIP,P2,TENT
      LOGICAL OK
      CHARACTER*(*) CMPNAM
      EXTERNAL NLEN1
C
      TENT=COMPM
C
      OK=.FALSE.
      P2=1
 10   CONTINUE
      IF (P2.GE.NTXPOS) RETURN
      CALL DTR500(P2,OK)
      IF ( CBUFF(1:NLEN1(CBUFF)) .EQ.
     +    CMPNAM(1:NLEN1(CMPNAM))) THEN
C        found correct name,test it
         PMIP=ICBUFF(1)
         CALL DIR500(PMIP,OK)
         IF (IMBUFF(1).NE.100.AND.IMBUFF(2).EQ.COMPM) THEN
C           correct type,return this one
            OK=.TRUE.
            RETURN
         END IF
      END IF
C     increment to next
      P2=P2+1
      OK=.FALSE.
      GOTO 10
C
      END
      SUBROUTINE RDXDEC(OUNIT,LINEP,EOF,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  L
C1    IOSTAT            I   I     I   O
C
C     This routine will decode and activate the DXF group code
C     as it has been read into an 80 character buffer
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      CHARACTER*80 GV
      INTEGER*4 GC,LINEP,OUNIT,I
      LOGICAL OK,EOF
      EXTERNAL CRUNCH
C
C     set default layer settings just in case the DXF file
C     doesnt have any and we have to put it all onto
C     layer 1 (the DAXCAD default layer)
      GC=0
      GV='SECTION'
      NUMLAY=1
      LSTYLE(1)=1793
C     Group codes initial filtering
      OK=.TRUE.
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      IF(EOF) RETURN
      CALL CRUNCH(GV)
      IF(GC.EQ.0.AND.GV.EQ.'SECTION') THEN
          CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
          IF(GC.EQ.2.AND.GV.EQ.'ENTITIES') THEN
              CALL RDXREN(OUNIT,LINEP,EOF,OK)
          ELSEIF(GC.EQ.2.AND.GV.EQ.'BLOCKS') THEN
              CALL RDXRBK(OUNIT,LINEP,EOF,OK)
          ELSEIF(GC.EQ.2.AND.GV.EQ.'TABLES') THEN
              CALL RDXRTB(OUNIT,LINEP,EOF,OK)
          ELSEIF(GC.EQ.2.AND.GV.EQ.'HEADER') THEN
              CALL RDXHED(OUNIT,LINEP,EOF,OK)
          ENDIF
          IF(EOF) RETURN
          IF(.NOT.OK) RETURN
      ENDIF
      GOTO 10
      END
 
C
C======================================================================
C
 
 
 
      SUBROUTINE RDXECI(OUNIT,LINEP,GC,GV,OK)
C     =======================================
C
C1    VARTYP            I4  I4    I4  C*80 L
C1    IOSTAT            I   I      I   I   O
C
C2    This routine collects the information from the INSERT
C2    group code and builds a transformation matrix for
C2    component instance which will be writyten to the database.
C2    It is not resolved at this stage and thus the parent data
C2    pointer will be a zero or negative number.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/style.inc'
      include 'include/movdat.inc'
C
C
      CHARACTER*80 GV,BLKNAM,TEMP,LYNAM*20
      REAL X,Y,ROTANG,XSF,YSF
      REAL RAD,M(3,3)
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TMIP,TCOL
      LOGICAL OK,EOF
      LOGICAL COLSET,STLSET
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH,RAD,RDXRLN
      EXTERNAL INSP04,DEW566,RDXMTX,MV0003,DRW066,DEPRNT
C
C     set defaults
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
      X=0.0
      Y=0.0
      ROTANG=0.0
      YSF=1.0
      XSF=1.0
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN                 
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.EQ.8) THEN
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.2) THEN
          BLKNAM=GV
C SPB - 031194 - NEEB Don't like lowercase filenames
C          CALL UNFOLD(BLKNAM)
      ELSE IF(GC.EQ.10) THEN
          READ(UNIT=GV,FMT='(F15.0)') X
      ELSE IF(GC.EQ.20) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y
      ELSE IF(GC.EQ.41) THEN
          READ(UNIT=GV,FMT='(F15.0)') XSF
      ELSE IF(GC.EQ.42) THEN
          READ(UNIT=GV,FMT='(F15.0)') YSF
      ELSE IF(GC.EQ.50) THEN
          READ(UNIT=GV,FMT='(F15.0)') ROTANG
          ROTANG=RAD(ROTANG)
      ELSE IF(GC.EQ.0) THEN
C         get the layer for this component
          CALL RDXGTL(LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
C         Write the entity to the database
C         set status for normal
          IMBUFF(1)=10
C         set entity type to Instance type.
          IMBUFF(2)=COMPI
C         ensure pointer to parent entry is zero.
          IMBUFF(8)=0
C         set work layer.
          IMBUFF(4)=TLAY
C         set font
          RWORK(1,1)=X
          RWORK(2,1)=Y
          RWORK(3,1)=XSF
          RWORK(4,1)=YSF
          RWORK(5,1)=ROTANG
          RWORK(6,1)=0.0
C         sort second diagonal data
          RWORK(2,2)=RWORK(5,1)
          RWORK(5,2)=RWORK(2,1)
C         write instance to database
          CBUFF=BLKNAM
          CALL DEW566(M,TMIP,OK)
          IF(.NOT.OK) THEN 
              DNUM = 680
              CALL DEPRNT(DNUM)
              GOTO 200
          ENDIF
C         return to the decoder routine
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
 
      SUBROUTINE RDXMTX(M)
C     ====================
C
C1    vartype         R(3,3)
C1    iostatus          O
C
C2    Subroutine SETTFM sets and returns a 3x3
C2    matrix defining a geometric transformation
C2    on the basis of data in the MOVDAT work variables.
C
      include 'include/movdat.inc'
C
      REAL SM(3,3),RM(3,3),TM(3,3),M(3,3),M2(3,3),RDUM
      EXTERNAL I3M,SCAP2D,ROTP2D,TRAN2D,MULT3M
C
C     initialize the matrices
      CALL I3M(SM)
      CALL I3M(RM)
      CALL I3M(TM)
C
C     scale if required
      IF (OPFLAG(1)) THEN
C        set the Y scale factor held in REFDAT(3,1)
         CALL SCAP2D(REFDAT(1,1),REFDAT(1,2),
     +               REFDAT(1,3),REFDAT(10,1),SM)
      END IF
C     rotate if required
      IF (OPFLAG(2)) THEN
         CALL ROTP2D(REFDAT(2,1),REFDAT(2,2),REFDAT(2,3),RM)
      END IF
C     translate if required
      IF (OPFLAG(4)) THEN
         CALL TRAN2D(REFDAT(4,1),REFDAT(4,2),TM)
      END IF
C     concatenate the transforms
      CALL MULT3M(SM,RM,M2)
      CALL MULT3M(M2,TM,M)
C     M now contains the working transform
C
      END
C
 
      SUBROUTINE RDXRBK(OUNIT,LINEP,EOF,OK)
C     ===================================
C
C1    VARTYP            I4  I4    I4  L  L
C1    IOSTAT            I   I     I   O  O
C
C2    This routine will write blocks into the database.
C     They are coded as symbols.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/style.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/props.inc'
      include 'include/lfu.inc'
 
C
      REAL X,Y,M(3,3),BPX,BPY
      INTEGER*2 SPA,TMIP,PMIP,I,TFONT,TCOL,TLAY
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP,A,B,C
      CHARACTER*80 GV,BLKNAM,TEMP,LYNAM*20
      INTEGER*4 GC,LINEP,OUNIT,FN,TPOS,DNUM
      LOGICAL OK,EOF,BLOC
      LOGICAL ENT,DELETE
      LOGICAL COLSET,STLSET,BX,BY
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH
C
C     We are into the blocks section
      BX = .FALSE.
      BY = .FALSE.
      COLSET = .FALSE.
      STLSET = .FALSE.
      TLAY=1
      TFONT=1
C     This poistion is the start of the block data
      X=0.0
      Y=0.0
      BPX=0.0
      BPY=0.0
      FN=1
C     save current starting position.
      TMIP = NMIPOS
C
C     Main loop for reading block data into DAXCAD database.
C
 
10    CONTINUE
C
C     set the pointer for the scratch file information
C     set the scratch file software
C     reset the scratch file pointer
C     read the intial line to make sure its a block or
C     an end of section.
      CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      IF(EOF) THEN
          DNUM = 672
          CALL DEPRNT(DNUM)
          RETURN
      ENDIF
      IF(.NOT.OK) RETURN
      CALL CRUNCH(GV)
 20   CONTINUE
      IF(GC.EQ.8)THEN
C         layer definition
          LYNAM=GV(1:20)
          GOTO 10
      ELSEIF(GC.EQ.0.AND.GV.EQ.'BLOCK') THEN
C         We have a block defintion .
          BLOC=.TRUE.
          GOTO 10
      ELSE IF(GC.EQ.0.AND.GV.EQ.'ENDSEC') THEN
C         Block section now written. Go back and do the
C         next section.
          RETURN
      ELSEIF(GC.EQ.0.AND.GV.EQ.'ATTDEF') THEN
          BLOC = .FALSE.
          GOTO 10
      ELSEIF ( GC.EQ.2.AND.BLOC ) THEN
C         Block name found
          BLKNAM=GV
C SPB - 031194 - NEEB Don't like lowercase filenames
C          CALL UNFOLD(BLKNAM)
          GOTO 10
      ELSEIF ( GC.EQ.10.AND.BLOC.AND..NOT.BX ) THEN
C         This is the X block base point
          READ(UNIT=GV,FMT='(F15.0)') BPX
          BX = .TRUE.
          GOTO 10
      ELSEIF ( GC.EQ.20.AND.BLOC.AND..NOT.BY ) THEN
C         This is the  Y block base point
          READ(UNIT=GV,FMT='(F15.0)') BPY
          BY = .TRUE.
          GOTO 10
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'LINE') THEN
C
C         write out a line to the database and store it
C
          CALL RDXELN(OUNIT,LINEP,GC,GV,OK)
C
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'ARC') THEN
C
C         write out an ARC to the database and store it
C
          CALL RDXEAC(OUNIT,LINEP,GC,GV,OK)
C
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'CIRCLE') THEN
C
C         write out an CIRCLE (ARC) to the database and store it
C
          CALL RDXEAC(OUNIT,LINEP,GC,GV,OK)
C
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'TEXT') THEN
C
C         write out TEXT to the database and store it
C
          CALL RDXETX(OUNIT,LINEP,GC,GV,OK)
C
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'TRACE') THEN
C
C         Write out a trace entity (four single lines)
C
          CALL RDXETC(OUNIT,LINEP,GC,GV,OK)
C
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'SOLID') THEN
C
C         Write out a SOLID entity (four or three single lines)
C
          CALL RDXESL(OUNIT,LINEP,GC,GV,OK)
C
      ELSEIF ( GC.EQ.0.AND.GV.EQ.'POLYLINE') THEN
C
C         Write out a Polyline entity
C
          CALL RDXPOL(OUNIT,LINEP,.FALSE.,GC,GV,OK)
C
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'INSERT') THEN
C
C         A nested instance is done here
C
          CALL RDXECI(OUNIT,LINEP,GC,GV,OK)
C
      ELSE IF ( GC.EQ.0.AND.GV.EQ.'POINT') THEN
C
C         A point entity
C
          CALL RDXRPT(OUNIT,LINEP,GC,GV,OK)
C
      ELSE IF( GC.EQ.0.AND.GV.EQ.'ENDBLK') THEN
C         get layer information for this master
          IF(TMIP.EQ.NMIPOS) THEN
C             Nothing dont bother to save it here
              TMIP = NMIPOS
C             loop back
              GOTO 10
          ENDIF
          CALL RDXGTL(LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base
          IMBUFF(4)=TLAY
C         Write the block out to the database using the
C         extracted information from above
C         start of master data
          RDBUFF(1)=REAL(TMIP)
C         End of master data
          RDBUFF(2)=REAL(NMIPOS-1)
C         spare
          RDBUFF(3)=0.0
C         base point X
          RDBUFF(4)=BPX
C         base point Y
          RDBUFF(5)=BPY
C         spare
          RDBUFF(6)=0.0
C         place name in text buffer
          CBUFF=BLKNAM
C         the work files,and create relation list etc
          CALL DEW056(FN,TMIP,.FALSE.,PMIP,OK)
C         reset all pointer to the end of the list
          OK=.TRUE.
C         reset the scratch file pointer
          TMIP = NMIPOS
C         reset base pointers
          BX = .FALSE.
          BY = .FALSE.
          GOTO 10
      ELSE
C         some unknown command found ignore it and read another line
          GOTO 10
      ENDIF
C     Allready loaded values.Only need to decode them
      GOTO 20
      END
 
      SUBROUTINE RDXRPT(OUNIT,LINEP,GC,GV,OK)
C     =======================================
C
C1    VARTYP            I4      I4    I4  L
C1    IOSTAT            I       I     I   O
C
C2    Reads next entity for POINT. Actially used marker entity no 1 as a point
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      CHARACTER*80 GV,LYNAM*20
      REAL ANGLE
      REAL SX,SY
      REAL X1,Y1,X2,Y2
      INTEGER*2 FORM
      INTEGER*4 GC,LINEP,OUNIT,DNUM
      INTEGER*2 P,TFONT,TLAY,TCOL
      LOGICAL OK,EOF
      LOGICAL COLSET,STLSET
      LOGICAL X,Y
      CHARACTER*80 STLNAM
      INTEGER*2 COLNAM
C
      EXTERNAL CRUNCH
C
      X = .FALSE.
      Y = .FALSE.
      SX = 1.0
      SY = 1.0
      ANGLE = 0.0
      COLSET = .FALSE.
      STLSET = .FALSE.
      FORM = 1
      TFONT=1
      TLAY=1
      TFONT=1
10    CALL RDXRLN(OUNIT,LINEP,GC,GV,EOF,OK)
      CALL CRUNCH(GV)
      IF(EOF) THEN
          DNUM = 672
          CALL DEPRNT(DNUM)
          GOTO 200
      ENDIF
      IF(.NOT.OK) GOTO 200
      IF(GC.EQ.8) THEN
C         layer name
          LYNAM=GV(1:20)
      ELSEIF(GC.EQ.10.AND..NOT.X) THEN
          READ(UNIT=GV,FMT='(F15.0)') X1
          X = .TRUE.
      ELSEIF(GC.EQ.20.AND..NOT.Y) THEN
          READ(UNIT=GV,FMT='(F15.0)') Y1
          Y = .TRUE.
      ELSEIF(GC.EQ.62) THEN
          COLSET = .TRUE.
          READ(UNIT=GV,FMT='(I3)') COLNAM
      ELSEIF(GC.EQ.6) THEN
          STLSET=.TRUE.
          STLNAM = GV
      ELSEIF(GC.EQ.0) THEN
C         Write the entity to the database
          CALL RDXGTL (LYNAM,TLAY,TCOL,TFONT)
C         get local conditions
          CALL RDXCOS(STLSET,COLSET,STLNAM,COLNAM,TFONT,TCOL)
C         add to data base the new MARKER
          CALL DEWC02(X1,Y1,ANGLE,SX,SY,FORM,TFONT,TLAY,P,OK)
          RETURN
      ENDIF
      GOTO 10
200   OK=.FALSE.
      END
C
 
