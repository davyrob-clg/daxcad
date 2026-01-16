C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 readmif.f   */
C
C
      SUBROUTINE GETMIF(MIFNAM,ST)
C     ============================
C
C1    vartype            C*(*)  I
C1    iostatus             I    O
C
C2    Subroutine GETMIF reads data in MOSS-MIF format from
C2    the file MIFNAM, and stores the contained drawing
C2    in internal DAXCAD format.
C
      include 'include/style.inc'
      include 'include/moss_mif.inc'
C           
      INTEGER*2  OLDCLR
      INTEGER*4  DNUM
      INTEGER ST,NLEN1,NLEN
      CHARACTER*(*) MIFNAM
      LOGICAL OK
      CHARACTER*80 STRING
      EXTERNAL NLEN1,NLEN
C
C     set dbu factors for mm and cm
C
      DRAWFC(1) = 1.0
      DRAWFC(2) = 10.0
C     Build the marker file name for future use if required
      MRKNAM = MIFNAM(1:NLEN(MIFNAM))//'.mrk'

C     open MIF file
      CALL FINDU1(MIFUNT,OK)
      IF (.NOT.OK) THEN
         ST=1
         GOTO 999
      END IF           

C     Save current colour as we change it as we go
      OLDCLR = COLOUR
           
                                                 
C     Now open mif file which must exist else error
      OPEN(UNIT=MIFUNT,FILE=MIFNAM,STATUS='OLD',ERR=99)
      REWIND(UNIT=MIFUNT)                          

C     start processing data block
 10   CONTINUE
C     read next line from file,and get data block type
      CALL MIF001(ST)
      IF (ST.EQ.2) THEN
C        valid end of file
         ST=0
C        Now move the paper to fit over the moss drawing
         CALL MIFPAP()
         GOTO 999
      ELSE IF (ST.NE.0) THEN
C        return error condition
         GOTO 999
      END IF
C     select option for data block type
      IF (BLKNAM.EQ.'MOSS') THEN
C        general MOSS header
         ST=0
      ELSE IF (BLKNAM.EQ.'MIFHEADER') THEN
C        MIF header block
         CALL MIFHED(ST)
      ELSE IF (BLKNAM.EQ.'DRAW') THEN
C        MIF drawing header information block
         CALL MIFDRW(ST)
      ELSE IF (BLKNAM.EQ.'COLOUR') THEN
C        MIF COLOUR table definition
         CALL MIFCOL(ST)
      ELSE IF (BLKNAM.EQ.'THICK') THEN
C        MIF THICKNESS table definition
         CALL MIFTHK(ST)
      ELSE IF (BLKNAM.EQ.'FONT') THEN
C        MIF FONT table definition
         CALL MIFONT(ST)
      ELSE IF (BLKNAM.EQ.'LTYPE') THEN
C        MIF LINE TYPE table definition
         CALL MIFTYP(ST)
      ELSE IF (BLKNAM.EQ.'MSYMBL') THEN
C        MIF SYMBOL table definition
         CALL MIFSYM(ST)
      ELSE IF (BLKNAM.EQ.'CFTYPE') THEN
C        MIF CURVE FITTING definition
         CALL MIFCFT(ST)
      ELSE IF (BLKNAM.EQ.'SHEET') THEN
C        MIF SHEET definition
         CALL MIFSHT(ST)
      ELSE IF (BLKNAM.EQ.'WINDOW') THEN
C        MIF WINDOW definition
         CALL MIFWIN(ST)
      ELSE IF (BLKNAM.EQ.'MODEL') THEN
C        MIF MODEL definition
         CALL MIFMOD(ST)
      ELSE IF (BLKNAM.EQ.'OBJECT') THEN
C        MIF OBJECT definition
         CALL MIFOBJ(ST)
      ELSE IF (BLKNAM.EQ.'ELEMENT') THEN
C        MIF ELEMENT definition
         CALL MIFELM(ST)
      ELSE IF (BLKNAM.EQ.'POLYLINE') THEN
C        MIF POLYLINE definition
         CALL MIFPOL(ST)
      ELSE IF (BLKNAM.EQ.'TEXT') THEN
C        MIF TEXT definition
         CALL MIFTXT(ST)
      ELSE IF (BLKNAM.EQ.'ARC') THEN
C        MIF ARC definition
         CALL MIFARC(ST)
      ELSE IF (BLKNAM.EQ.'PIPMARK') THEN
C        MIF PIPMARK definition
         CALL MIFPIP(ST)
      ELSE IF (BLKNAM.EQ.'CENTRED') THEN
C        MIF CENTRED instance definition
         CALL MIFCEN(ST)
      ELSE IF (BLKNAM.EQ.'SYMBOL') THEN
C        MIF SYMBOL instance definition
         CALL MIFCEN(ST)
      ELSE IF (BLKNAM.EQ.'VECTORS') THEN
C        MIF VECTORS definition
         CALL MIFVEC(ST)
      ELSE IF (BLKNAM.EQ.'HATCH') THEN
C        MIF HATCH definition
         CALL MIFHAT(ST)
      ELSE IF (BLKNAM.EQ.'ENDPOLYLINE') THEN
C        MIF Polyline terminator
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDARC') THEN
C        MIF Arc terminator
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDPIPMARK') THEN
C        MIF Pipmark terminator
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDCENTRED') THEN
C        MIF Centred terminator
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDSYMBOL') THEN
C        MIF Symbol terminator
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDHATCH') THEN
C        MIF HATCH definition
C        clear track value
         INENTY=INONE
      ELSE IF (BLKNAM.EQ.'ENDELEMENT') THEN
C        MIF Element terminator
         CELEM=' '
      ELSE IF (BLKNAM.EQ.'ENDOBJECT') THEN
C        MIF Object terminator
         COBJ=' '
      ELSE IF (BLKNAM.EQ.'ENDMODEL') THEN
C        MIF Model terminator
         CMODEL=' '
      ELSE IF (BLKNAM.EQ.'ENDWINDOW') THEN
C        MIF Window terminator
         CWINDO=0
      ELSE IF (BLKNAM.EQ.'ENDSHEET') THEN
C        MIF Sheet terminator
         CSHEET=0
      ELSE IF (BLKNAM.EQ.'EOF') THEN
C        MIF File terminator
         STRING=' '
         WRITE(STRING,FMT='(A)')'*************End of File*************'
         CALL CPRINT(STRING)
      ELSE
C        unknown block type
         STRING=' '
         WRITE(STRING,FMT='(3A,I6)')'Data Block "',
     +        BLKNAM(1:NLEN1(BLKNAM)),
     +        '" not known at line number ',NMIFLN
         CALL EPRINT(STRING)
         ST=999
         GOTO 999
      END IF
      IF (ST.EQ.0) GOTO 10
C     return status
      GOTO 999
C
 99   CONTINUE
C     Inform user failed to find mif file
      DNUM = 760
      CALL DEPRNT(DNUM)
      ST=2
      GOTO 999

 999  CONTINUE
C     Exit point to restore colour 
      COLOUR = OLDCLR
      RETURN
C
      END
C
C     ---------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mif000.ftn Daxcad revision 1.8
      SUBROUTINE MIF000(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST
C
C     read next line from MIF data file
      READ(UNIT=MIFUNT,FMT='(A)',ERR=98,END=99) MIFBUF
C     update current line count
      NMIFLN=NMIFLN+1
C     update block record count
      BLKRN=BLKRN+1
      ST=0
      RETURN
 98   CONTINUE
      ST=1
      RETURN
 99   CONTINUE
      ST=2
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mif001.ftn Daxcad revision 1.8
      SUBROUTINE MIF001(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST
C
C     read next line from MIF data file
      READ(UNIT=MIFUNT,FMT='(A)',ERR=98,END=99) MIFBUF
C     update current line count
      NMIFLN=NMIFLN+1
C     reset block record number
      BLKRN=1
C     get first field in upper case,left justified
      BLKNAM=MIFBUF(1:12)
      CALL CRUNCH(BLKNAM)
      CALL FOLDUP(BLKNAM)
      ST=0
      RETURN
 98   CONTINUE
C     return with READ FILE ERROR status
      ST=1
      RETURN
 99   CONTINUE
C     return with END OF FILE status
      ST=2
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mif002.ftn Daxcad revision 1.8
      SUBROUTINE MIF002(FN,FP)
C     ========================
C
C1    vartype           I  I
C1    iostatus          I  O
C
      include 'include/moss_mif.inc'
C
      INTEGER FN,ST,FP,RN,NF
      DOUBLE PRECISION R
C
C     find record number in which field exists
      RN=INT(FN/6)
      IF (MOD(FN,6).GT.0) RN=RN+1
C     check which record of the block is in buffer
      IF (RN.GT.BLKRN) THEN
C        read the next record into buffer
         CALL MIF000(ST)
      END IF
C     find field number in record
      NF=MOD(FN,6)
      IF (NF.EQ.0) NF=6
C     get pointer to field in buffer
      FP=(NF-1)*13 + 1
C     return with pointer
      END
C
C     ------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mif003.ftn Daxcad revision 1.8
      SUBROUTINE MIF003(FN,R,ST)
C     ==========================
C
C1    vartype           I  D I
C1    iostatus          I  O O
C
      include 'include/moss_mif.inc'
C
      INTEGER FN,ST,FP,FP2
      DOUBLE PRECISION R
C
C     find pointer to requested field
      CALL MIF002(FN,FP)
      FP2=FP+11
C     read data from field with REAL format
      READ(UNIT=MIFBUF(FP:FP2),FMT='(F12.5)',ERR=99) R
      ST=0
      RETURN
 99   CONTINUE
      ST=9999
      END
C
C     ------------------------------------------------
C
 
C       @(#)  256.1 date 12/16/89 mif004.ftn Daxcad revision 1.8
      SUBROUTINE MIF004(FN,I,ST)
C     ==========================
C
C1    vartype           I  I I
C1    iostatus          I  O O
C
      include 'include/moss_mif.inc'
C
      INTEGER FN,I,ST,FP,FP2
C
C     find pointer to requested field
      CALL MIF002(FN,FP)
      FP2=FP+11
C     read data from field with INTEGER format
      READ(UNIT=MIFBUF(FP:FP2),FMT='(I12)',ERR=99) I
      ST=0
      RETURN
 99   CONTINUE
      ST=9999
      END
C
C     ------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mif005.ftn Daxcad revision 1.8
      SUBROUTINE MIF005(FN,C,ST)
C     ==========================
C
C1    vartype           I C*12 I
C1    iostatus          I  O   O
C
      include 'include/moss_mif.inc'
C
      INTEGER FN,ST,FP,FP2
      CHARACTER*12 C
C
C     find pointer to requested field
      CALL MIF002(FN,FP)
      FP2=FP+11
C     read data from field with CHARACTER format
      C=MIFBUF(FP:FP2)
C     remove leading spaces
      CALL LSSUPP(C)
      ST=0
      RETURN
 99   CONTINUE
      ST=9999
      END
C
C     ------------------------------------------------
C
C
C     ------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 mif006.ftn 
      SUBROUTINE MIF006(FN,C,ST)
C     ==========================
C
C1    vartype           I C*12 I
C1    iostatus          I  O   O
C
      include 'include/moss_mif.inc'
C
      INTEGER FN,ST,FP,FP2
      CHARACTER*12 C
C
C     Do not remove leading spaces in this one.
C     find pointer to requested field
      CALL MIF002(FN,FP)
      FP2=FP+11
C     read data from field with CHARACTER format
      C=MIFBUF(FP:FP2)
      ST=0
      RETURN
 99   CONTINUE
      ST=9999
      END
C
C       @(#)  256.1 date 12/16/89 mifarc.ftn Daxcad revision 1.8
      SUBROUTINE MIFARC(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,FN,I,LTYP,COLI,THCKI,CFT,NPTS
      INTEGER*2 TF,TL,P
      DOUBLE PRECISION CX,CY,RAD,SANG,EANG
      LOGICAL OK
C
C     set track value
      INENTY=IARC
C     get LINETYPE index
      CALL MIF004(2,LTYP,ST)
C     get COLOUR index
      CALL MIF004(3,COLI,ST)
C     get THICKNESS index
      CALL MIF004(4,THCKI,ST)
C     get ARC centre X (drawing units)
      CALL MIF003(5,CX,ST)
C     get ARC centre Y (drawing units)
      CALL MIF003(6,CY,ST)
C     get radius (drawing units)
      CALL MIF003(7,RAD,ST)
C     get start angle (radians)
      CALL MIF003(8,SANG,ST)
C     get end angle (radians)
      CALL MIF003(9,EANG,ST)
C     must scale to paper units
      CX=CX*DRAWFC(DRAWUN)  + SHOFTX
      CY=CY*DRAWFC(DRAWUN)  + SHOFTY
      RAD=RAD*DRAWFC(DRAWUN)
                     
C     Handle the change of the line type if required
      IF (ONCLLY) THEN
          TF = WLINTP
      ELSE
          TF =LTYP
      ENDIF                                         

C     Handle the change of the line type and colour if tying
C     layers to colour
      IF (LABCLR) THEN
          TF = WLINTP
C          TCOL = COLOUR
      ENDIF                                         

C     Get the layer to place the entity on
      IF (LYPRCL) THEN
          TL = WLAYER + COLI - 1
      ELSE
          TL=WLAYER
      ENDIF

C     write arc to database
      CALL DEWC05(REAL(CX),REAL(CY),REAL(RAD),
     +            REAL(SANG),REAL(EANG),TF,TL,P,OK)
      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 4/19/90 mifcen.ftn Daxcad revision 1.8
      SUBROUTINE MIFCEN(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/masti.inc'
      include 'include/moss_mif.inc'
C
      INTEGER*2 TFONT,TLAY,SPDP,SYMI2
      INTEGER ST,SYMI,COLI,THCKI       
      LOGICAL OK,SAME
      DOUBLE PRECISION SX,SY,SA,SH,SW
      REAL   SCALX,SCALY,SRX,SRY,SRA
      CHARACTER*12 STRING
C
C     set track value
      INENTY=ICEN
C     get SYMBOL index
      CALL MIF004(2,SYMI,ST)
C     get COLOUR index
      CALL MIF004(3,COLI,ST)
C     get THICKNESS index
      CALL MIF004(4,THCKI,ST)
C     get X coordinate (drawing units)
      CALL MIF003(5,SX,ST)
C     get Y coordinate (drawing units)
      CALL MIF003(6,SY,ST)
C     get angle of symbol (radians)
      CALL MIF003(7,SA,ST)
C     get height of symbol (drawing units)
      CALL MIF003(8,SH,ST)
C     get width of symbol (drawing units)
      CALL MIF003(9,SW,ST)
C     read next field for 'ENDVECTORS'
      CALL MIF005(10,STRING,ST)                     
C     If not symbol to markers then just return
      IF (.NOT.SYMMRK) THEN
          ST = 0 
          RETURN
      ENDIF
C     Now call drwmrk so that it is shown, first call sclmrk to get
C     the scale
      CALL SCLMRK(SYMI,SW,SH,SCALX,SCALY,ST)

C     Handle the change of the line type if required
      IF (ONCLLY) THEN
          TFONT = WLINTP
      ELSE
          TFONT= CLFONT
      ENDIF                                         

C     Handle the change of the line type and colour if tying
C     layers to colour
      IF (LABCLR) THEN
          TFONT = WLINTP
C          TCOL = COLOUR
      ENDIF                                         

C     Get the layer to place the entity on
      IF (LYPRCL) THEN
          TLAY = WLAYER + COLI - 1
      ELSE
          TLAY = WLAYER
      ENDIF

C     Convert doubles to reals
      SRX = SX
      SRY = SY              
      SRA = -SA                
      SYMI2 = SYMI                    
C     Now make to correct scale
      SRX = SRX * DRAWFC(DRAWUN)  + SHOFTX
      SRY = SRY * DRAWFC(DRAWUN)  + SHOFTY

      IF(SAME(SCALX,0.0)) SCALX = SCALY
      IF(SAME(SCALY,0.0)) SCALY = SCALX

      SCALX = SCALX *  DRAWFC(DRAWUN)
      SCALY = SCALY *  DRAWFC(DRAWUN)

      CALL DEWC02(SRX,SRY,SRA,SCALX,SCALY,SYMI2,TFONT,TLAY,SPDP,OK)

C     return clear status
      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifcft.ftn Daxcad revision 1.8
      SUBROUTINE MIFCFT(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      DOUBLE PRECISION CFTOL
      INTEGER ST,FN,NCFTYP,CFTYPI,I,NL
      CHARACTER*12 CFNAM
C
C     get number of defined curve fit types
      CALL MIF004(2,NCFTYP,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NCFTYP
C        get index
         CALL MIF004(FN+1,CFTYPI,ST)
C        get CF Tolerance
         CALL MIF003(FN+2,CFTOL,ST)
C        get length of name
         CALL MIF004(FN+3,NL,ST)
C        get CF name
         CALL MIF005(FN+4,CFNAM,ST)
         FN=FN+6
 10   CONTINUE
C     read next record for ENDCFTYPE
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDCFTYPE') THEN
         ST=0
      ELSE
         ST=1
      END IF
      END
C
C     -----------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 mifclr.ftn 
      SUBROUTINE MIFCLR()
C     =====================
C
C
      include 'include/style.inc'
      include 'include/moss_mif.inc'
C
      INTEGER*4 MOD
      INTRINSIC MOD
 
C     If first time then do not increment colour index
      IF (.NOT.FIRCLR) THEN
          COLOUR = COLOUR + 1
      ENDIF
 
C     Increment collay and handle the wraparound to keep the colour
C     index within the range of 1 - 15
      FIRCLR = .FALSE.
      COLOUR = MOD(INT(COLOUR),16)
      IF (COLOUR.EQ.0) THEN
C         Handle colour going to zero index
          COLOUR = 1
      ENDIF
 
      END
C      @(#)  256.1 date 12/16/89 mifcly.ftn 
      SUBROUTINE MIFCLY(NAME)
C     =====================
C
C     This routine will take the element name and
C     Work out the required line style and colour for
C     it
C
      include 'include/style.inc'
      include 'include/moss_mif.inc'
C
C     Assuming the following line styles
C     1 -> SOLID
C     2 -> SHORT DASH
C     3 -> LONG DASH
C     5 -> LONG CENTRE
C     6 -> SINGLE CENTRE
C
C     And Assuming the following colours
C     1 - Red
C     2 - Green
C     3 - Blue
C     4 - Cyan
C     5 - Yellow
C     6 - Purple
C     7 - White
C
      CHARACTER*(*)  NAME
 
      IF (NAME(1:1).EQ.'D') THEN
C         Ditch  - Long dash , Blue
          COLOUR = 3
          WLINTP = 3
      ELSE IF (NAME(1:1).EQ.'F') THEN
C         Fence - Long dash.dot white (single centre?)
          COLOUR = 7
          WLINTP = 6
      ELSE IF (NAME(1:1).EQ.'H') THEN
C         Hedge - Chain dash, green (long centre)
          COLOUR = 2
          WLINTP = 5
      ELSE IF (NAME(1:1).EQ.'I') THEN
C         Interface - Chain dash,red (long centre)
          COLOUR = 1
          WLINTP = 5
      ELSE IF (NAME(1:1).EQ.'L') THEN
C         Levels - Solid, green
          COLOUR = 2
          WLINTP = 1
      ELSE IF (NAME(1:1).EQ.'M') THEN
C         Alignment - Chain dash ,red (long centre)
          COLOUR = 1
          WLINTP = 5
      ELSE IF (NAME(1:1).EQ.'V') THEN
C         Verge - Short dash, green
          COLOUR = 2
          WLINTP = 2
      ELSE IF ((NAME(1:1).EQ.'C').OR.(NAME(1:1).EQ.'K')) THEN
C         Road - Solid , Red
          COLOUR = 1
          WLINTP = 1
      ELSE IF ((NAME(1:1).GE.'0').AND.(NAME(1:1).LE.'9')) THEN
C         Other - Solid, yellow
          COLOUR = 5
          WLINTP = 1
      ELSE IF ((NAME(1:1).GE.'A').AND.(NAME(1:1).LE.'Z')) THEN
C         other alpha - Solid, white
          COLOUR = 7
          WLINTP = 1
      ENDIF
 
      END
C       @(#)  256.1 date 12/16/89 mifcol.ftn Daxcad revision 1.8
      SUBROUTINE MIFCOL(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
      include 'include/masti.inc'
C
      DOUBLE PRECISION R,G,B
      INTEGER ST,NCOLI,COLI,L,FN,I
      CHARACTER*12 COLNM
C
C     get number of defined colours
      CALL MIF004(2,NCOLI,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NCOLI
C        get colour index
         CALL MIF004(FN+1,COLI,ST)
C        get RED component
         CALL MIF003(FN+2,R,ST)
         COLMIX(1,I)=R
C        get GREEN component
         CALL MIF003(FN+3,G,ST)
         COLMIX(2,I)=G
C        get BLUE component
         CALL MIF003(FN+4,B,ST)
         COLMIX(3,I)=B
C        get length of colour name
         CALL MIF004(FN+5,L,ST)
C        get colour NAME
         CALL MIF005(FN+6,COLNM,ST)
         COLNAM(I)=COLNM
         FN=FN+6
C        Now if one layer is to only hold one colour then
C        assign the colour names to the layers to be
C        nice and consistent and helpful
         IF (LYPRCL) THEN
            LNAME(WLAYER + COLI - 1) = COLNAM(I)
         ENDIF
 10   CONTINUE
C     store number of colours
      NCOLRS=NCOLI     
C     read next record for ENDCOLOUR
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDCOLOUR') THEN
         ST=0
      ELSE
         ST=1
      END IF
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 4/19/90 mifdrw.ftn Daxcad revision 1.8
      SUBROUTINE MIFDRW(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST
C
C     get MOSS revision number
      CALL MIF003(4,MOSREV,ST)
C     get MIF revision number
      CALL MIF003(5,MIFREV,ST)
C     get drawing units flag 1=mm, 2=cm
      CALL MIF004(6,DRAWUN,ST)
C     get number of sheets
      CALL MIF004(7,NSHEET,ST)
C     get drawing sheet size in drawing units
C      CALL MIF003(8,SHEETX,ST)
C      CALL MIF003(9,SHEETY,ST)
C
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifelm.ftn Daxcad revision 1.8
      SUBROUTINE MIFELM(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,NL,MOSMIN
C
C     get MOSS minor option number
      CALL MIF004(2,MOSMIN,ST)
C     get length of name
      CALL MIF004(3,NL,ST)
C     get ELEMENT name
      CALL MIF005(4,CELEM,ST)
C     ensure entity track values clear
      INENTY=INONE
      INVECT=INONE
           
C     Tie elements to a certain colour and line style if TRUE            
      IF (LABCLR) THEN 
         CALL MIFCLY(CELEM)
      ENDIF

      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 4/20/90 mifhat.ftn 
      SUBROUTINE MIFHAT(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,BOUND,NLEN1
      DOUBLE PRECISION X1,X2
      LOGICAL FIRST,OK,DRAW
      CHARACTER*12 BLKTYP
C
C     set track value
      INENTY=IHAT
C     read hatch angle flag
      CALL MIF003(2,X1,ST)
c     read hatchspacing flag
      CALL MIF003(3,X2,ST)
c     read boundry paint flag
      CALL MIF004(4,BOUND,ST)
      
      IF(BOUND.EQ.-1) THEN
C       read next line from file,and get data block type
        CALL MIF001(ST)
        BLKTYP = 'END'//BLKNAM(1:NLEN1(BLKNAM))
C       skip over any boundry definition
 10     CONTINUE
C       read next line from file,and get data block type
        CALL MIF001(ST)
        IF (BLKNAM.NE.BLKTYP) GOTO 10
      END IF
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifhed.ftn Daxcad revision 1.8
      SUBROUTINE MIFHED(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
C2    Subroutine MIFHED reads the MOSS-MIF header
C2    block, and interprets Date and time of creation.
C
      include 'include/moss_mif.inc'
C
      INTEGER ST
C
C      WRITE(*,'(A,/A)') ' MIFHEADER',MIFBUF
      ST=0
      END
C
C     -----------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 miflay.ftn 
      SUBROUTINE MIFLAY()
C     =====================
C
C
      include 'include/moss_mif.inc'
C
      INTEGER*4 MOD
      INTRINSIC  MOD
 
C     If first time then do not increment layer number
      IF (.NOT.FIRLAY) THEN
          WLAYER = WLAYER + 1
      ENDIF
      FIRLAY = .FALSE.
 
      WLAYER = MOD(WLAYER,256)
 
      END
C       @(#)  256.1 date 12/16/89 miflay.ftn 
C       @(#)  256.1 date 12/16/89 mifmod.ftn Daxcad revision 1.8
      SUBROUTINE MIFMOD(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,NL,FN,NLEN1
      CHARACTER*80 STRING
      EXTERNAL NLEN1
C
C     get number of objects in model
      CALL MIF004(2,NMOBJS,ST)
C     get length of name
      CALL MIF004(3,NL,ST)
C     set field number for model name
      FN=4
C     get MODEL name
      CALL MIFTX0(NL,FN,STRING)
      CMODEL=STRING(1:NL)
C     Now if MODFLG is TRUE generate a new layer
      IF (MODFLG) THEN
          CALL MIFLAY()
C         If one colour per layer take a new colour as well
          IF (ONCLLY) THEN
              CALL MIFCLR()
          ENDIF
      ENDIF
      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifobj.ftn Daxcad revision 1.8
      SUBROUTINE MIFOBJ(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,NL,L1,L2,NLEN1
      CHARACTER*80 STRING
C
C     get number of elements in object
      CALL MIF004(2,NOELMS,ST)
C     get length of name
      CALL MIF004(3,NL,ST)
C     get OBJECT name
      CALL MIF005(4,COBJ,ST)
C     tell user what is being processed
      L1=NLEN1(CMODEL)
      L2=NLEN1(COBJ)
      STRING=' '
      WRITE(STRING,FMT='(5A)')
     + 'Processing model "',CMODEL(1:L1),'" Object "',COBJ(1:L2),'"'
      CALL CPRINT(STRING)

C     If OBJFLG set generate a new layer for each model
      IF (OBJFLG) THEN 
C         Check to see that it does not start with a /
          IF (COBJ(1:1).NE.'/') THEN
              CALL MIFLAY()
C             If one colour per layer take a new colour as well
              IF (ONCLLY) THEN
                  CALL MIFCLR()
              ENDIF
          ENDIF
      ENDIF

      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifont.ftn Daxcad revision 1.8
      SUBROUTINE MIFONT(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      DOUBLE PRECISION HEIGHT,HWRAT,SPACE,CANGLE,CTHICK,DDEG
      INTEGER ST,FONTI,SL,FL,FN,NDEC,I
      CHARACTER*12 SNAME,FNAME
      EXTERNAL DDEG
C
C     get number of defined colours
      CALL MIF004(2,NTFONT,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NTFONT
C        get FONT index
         CALL MIF004(FN+1,FONTI,ST)
C        store reference index
         TFNTID(I)=FONTI
C        get length of style name
         CALL MIF004(FN+2,SL,ST)
C        get style name
         CALL MIF005(FN+3,SNAME,ST)
C        store style name
         TSNAME(I)=SNAME
C        get length of FONT name
         CALL MIF004(FN+4,FL,ST)
C        get FONT name
         CALL MIF005(FN+5,FNAME,ST)
C        store font name
         TFNAME(I)=FNAME
C        get character HEIGHT
         CALL MIF003(FN+6,HEIGHT,ST)
C        scale height by dbu factor
         HEIGHT=HEIGHT*DRAWFC(DRAWUN)
C        get HEIGHT/WIDTH ratio
         CALL MIF003(FN+7,HWRAT,ST)
C        get character SPACING
         CALL MIF003(FN+8,SPACE,ST)
C        get character ANGLE
         CALL MIF003(FN+9,CANGLE,ST)
C        convert angle to slant in degrees
         CANGLE=90.0-DDEG(CANGLE)
C        get character THICKNESS
         CALL MIF003(FN+10,CTHICK,ST)
C        scale thickness by dbu factor
         CTHICK=CTHICK*DRAWFC(DRAWUN)
C        get number decimal places
         CALL MIF004(FN+11,NDEC,ST)
C        store height
         TXPARS(1,I)=HEIGHT
C        store width
         TXPARS(2,I)=HWRAT*HEIGHT
C        store spacing
         TXPARS(3,I)=SPACE
C        store angle
         TXPARS(4,I)=CANGLE
C        store thickness
         TXPARS(5,I)=CTHICK
         FN=FN+12
 10   CONTINUE
C     read next record for ENDFONT
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDFONT') THEN
         ST=0
      ELSE
         ST=1
      END IF
      END
C
C     -----------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 mifpap.ftn 
      SUBROUTINE MIFPAP()
C     ============================
C
C
C     This routine rescales the mif paper to within the
C     zoom extents
C
      include 'include/wtov.inc'
      include 'include/ndata.inc'
      include 'include/params.inc'
 
C     Change paper position to x0,y0
C     set bottom left of paper at new position
      WPORGX=0.0+DRWSIZ(1)*PAPTOW/2
      WPORGY=0.0+DRWSIZ(2)*PAPTOW/2
C     call change paper position routine
      CALL CHGP05()
C     now show the new data
      CALL ZOMEXT()
 
      END
C       @(#)  256.1 date 12/16/89 mifpap.ftn 
C      @(#)  256.1 date 12/16/89 mifpic.ftn 
      SUBROUTINE MIFPIC(FPICK,FIRST,SEC,FTOK,STOK,HMEN,UMEN)
C     ===================
 
      LOGICAL FPICK,FIRST,SEC
      CHARACTER*1  FTOK,STOK,HMEN,UMEN
 
C     IF FPick is true it is the first that has been picked
C     IF FPick is false it is the second that has been picked
C     IF first is true then it is highlighted else unhighlighted
C     IF second is true then it is highlighted else unhighlighted
C     FTOK and STOK are the tokens used for the cells
C     HMEN and UMEN handle the hightlight and unhighlight
 
      HMEN = ' '
      UMEN = ' '
 
      IF (FPICK) THEN
C         User has selected the first one
          IF (FIRST) THEN
              FIRST = .FALSE.
              UMEN = FTOK
          ELSE IF (SEC) THEN
              FIRST = .TRUE.
              SEC = .FALSE.
              UMEN = STOK
              HMEN = FTOK
          ELSE
              FIRST = .TRUE.
              HMEN = FTOK
          ENDIF
      ELSE
C         User has selected second one
          IF (SEC) THEN
              SEC = .FALSE.
              UMEN = STOK
          ELSE IF (FIRST) THEN
              SEC = .TRUE.
              FIRST = .FALSE.
              UMEN = FTOK
              HMEN = STOK
          ELSE
              SEC = .TRUE.
              HMEN = STOK
          ENDIF
      ENDIF
 
      END
C       @(#)  256.1 date 12/16/89 mifpip.ftn Daxcad revision 1.8
      SUBROUTINE MIFPIP(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,FN,I,LTYP,COLI,THCKI,CFT,NPTS
      DOUBLE PRECISION DRWLEN,DEPPAT,XX,YY
C
C     set track value
      INENTY=IPIP
C     get COLOUR index
      CALL MIF004(2,COLI,ST)
C     get THICKNESS index
      CALL MIF004(3,THCKI,ST)
C     get number of points
      CALL MIF004(4,NPTS,ST)
      FN=5
C     go get points of pipmark
C      WRITE(*,*)'Pipmark:'
      DO 10 I=1,NPTS
         CALL MIF003(FN,XX,ST)
         CALL MIF003(FN+1,YY,ST)
         FN=FN+2
C         WRITE(*,*)'Pip point:',XX,' ',YY
10    CONTINUE
      ST=0
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifpol.ftn Daxcad revision 1.8
      SUBROUTINE MIFPOL(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C  
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/save.inc'
      include 'include/style.inc'
      include 'include/gtxt2.inc'  
      include 'include/vntable.inc'  
      include 'include/moss_mif.inc'
C                                                      
      INTEGER*2  TF,TL,P,PD,TMIP,SMIPOS,SPDP
      INTEGER    ST,FN,I,LTYP,COLI,THCKI,CFT,NPTS,J
      INTEGER*4  TYPE
      DOUBLE PRECISION DRWLEN,DEPPAT,XX,YY,X1,Y1,X2,Y2
      LOGICAL    FIRST,OK
      REAL       TW
C
C     set track value
      INENTY=IPOLY
C     get LINETYPE index
      CALL MIF004(2,LTYP,ST)
C     get COLOUR index
      CALL MIF004(3,COLI,ST)
C     get THICKNESS index
      CALL MIF004(4,THCKI,ST)
C     get CURVE FIT TYPE
      CALL MIF004(5,CFT,ST)
C     get drawn length of pattern
      CALL MIF003(6,DRWLEN,ST)
C     get depth of pattern
      CALL MIF003(7,DEPPAT,ST)
C     get number of points in string
      CALL MIF004(8,NPTS,ST)
      FN=9

C     Set up the necessary values for font,layer,colour
C     Handle the change of the line type if required
      IF (ONCLLY) THEN
          TF  = WLINTP
      ELSE
          TF = 1
      ENDIF                                         

C     Handle the change of the line type and colour if tying
C     layers to colour
      IF (LABCLR) THEN
          TF = WLINTP
C          TCOL = COLOUR
      ENDIF                                         

C     Set up the work layer
      IF (LYPRCL) THEN
          TL = WLAYER + COLI - 1
      ELSE
          TL=WLAYER
      ENDIF

C     go get points of polyline
C      WRITE(*,*)'Polyline:'

      IF (LINLIN) THEN
C     Represent the polyline as a sequence of lines
          FIRST = .TRUE.
          DO 10 I=1,NPTS
             IF (FIRST) THEN
                CALL MIF003(FN,X1,ST)
                CALL MIF003(FN+1,Y1,ST)
                FN=FN+2
                FIRST=.FALSE.
C               vectors are given already scaled
C               must scale back to paper units
                X1=X1*DRAWFC(DRAWUN) + SHOFTX
                Y1=Y1*DRAWFC(DRAWUN) + SHOFTY
             ELSE
                CALL MIF003(FN,X2,ST)
                CALL MIF003(FN+1,Y2,ST)
                FN=FN+2
C               vectors are given already scaled
C               scale to paper units
                X2=X2*DRAWFC(DRAWUN) + SHOFTX
                Y2=Y2*DRAWFC(DRAWUN) + SHOFTY
C               Draw the line
                CALL DEWC03(REAL(X1),REAL(Y1),REAL(X2),REAL(Y2),
     +                      TF,TL,P,OK)
                X1=X2
                Y1=Y2
             END IF
10        CONTINUE
      ELSE IF (PLYLIN) THEN
C         Treat polyline as an actual polyline but we will be unable
C         to font it
          IF (NPTS.EQ.2) THEN
C             Its a line so just write out as one
              CALL MIF003(FN,XX,ST)
              CALL MIF003(FN+1,YY,ST)
              FN=FN+2

              CALL MIF003(FN,X1,ST)
              CALL MIF003(FN+1,Y1,ST)
              FN=FN+2

C             I always forget to do this. Now scale the points
C             by the drawing scale
              XX = XX * DRAWFC(DRAWUN) + SHOFTX
              YY = YY * DRAWFC(DRAWUN) + SHOFTY
              X1 = X1 * DRAWFC(DRAWUN) + SHOFTX
              Y1 = Y1 * DRAWFC(DRAWUN) + SHOFTY

              CALL DEWC03(REAL(XX),REAL(YY),
     +              REAL(X1),REAL(Y1),TF,TL,P,OK)

              ST=0
              RETURN
           ENDIF
C     ******* Handle polyline def ********                      
C     Write header of spline curve
C     The spline polygon value

C     See datah0.ftn  Polygon with polygon fit
         TYPE = 25
         CALL DEWC08(TYPE,TL,TF,SMIPOS,SPDP,OK)

         DO 20 I=1,NPTS
             CALL MIF003(FN,XX,ST)
             CALL MIF003(FN+1,YY,ST)
             FN=FN+2

             DO 40 J=1,6
                 RDBUFF(J)=0.0
 40          CONTINUE

             RDBUFF(1)=XX * DRAWFC(DRAWUN) + SHOFTX
             RDBUFF(2)=YY * DRAWFC(DRAWUN) + SHOFTY
             IDBUFF(1)=SPLINE
             IDBUFF(2)=SMIPOS

             CALL DBW500(P,OK)
             CALL DBR500(SPDP,OK)
             IDBUFF(3)=P
             CALL DBM500(SPDP,OK)
             SPDP=P
 20      CONTINUE

C        Modify the last entity to lose the chain link
         PD=NPDPOS-1
C        Do a read before modifying
         CALL DBR500(PD,OK)
         IDBUFF(3)=0       
C        Now modify it 
         CALL DBM500(PD,OK)
C        Now change the type of the spline to a ploygon line
         CALL DIR500(SMIPOS,OK)
         IMBUFF(5) = 25
         CALL DIM500(SMIPOS,OK)
C        Now calculate Thumb weight
         DO 50 I=1,6
            RDBUFF(I)=0.0
 50      CONTINUE
         CALL DBR500(IMBUFF(7),OK)
         TW = 0.25
         RDBUFF(6)=TW
         CALL DBM500(IMBUFF(7),OK)
 
C        update limits
         CALL CHGE40(SMIPOS)
      ELSE                                
C     ******* If not line or polyline just read in and ignore ******
      DO 30 I=1,NPTS
         CALL MIF003(FN,XX,ST)
         CALL MIF003(FN+1,YY,ST)
         FN=FN+2
C         WRITE(*,*)'Poly point:',XX,' ',YY
 30   CONTINUE
      ENDIF

      ST=0
      END
C
C     -----------------------------------------------------
C
C      @(#)  256.1 date 12/16/89 mifset.ftn 
      SUBROUTINE MIFSET(PLAY,MODEL,OBJECT,COLLAY,POLY,LABCOL,LINES,
     +                   SYMARK,SHEET,LCOLS,ST)
C     =====================
C
C1    vartype           I      L      L    L      L    L      L   I
C1    iostatus          I      I      I    I      I    I      I   O
C
      include 'include/style.inc'
      include 'include/masti.inc'
      include 'include/moss_mif.inc'
 
C
      INTEGER*4 PLAY,ST,DNUM
      LOGICAL   MODEL,OBJECT,COLLAY,POLY,LABCOL,LINES,SYMARK
      LOGICAL   SHEET,LCOLS
 
C     If play is not equal to 0 then set new start layer to that
C     else use the current work layer the code checks for this but
C     check for valid range first of all
      IF ((PLAY.LT.0).OR.(PLAY.GT.255)) THEN
          IF (PLAY.NE.1000) THEN
             DNUM = 763
             CALL DEPRNT(DNUM)
             ST = 1
             RETURN
          ENDIF
      ENDIF
 
C     Set WLAYER to the required layer if -1 the code
C     will use the Current Layer ie CLAYER
      WLAYER = PLAY

C     IF user specifies to place one colour per layer then we
C     have to ensure all the rest of the layer stuff is off
C     like model_layer or object_layer
      IF (LCOLS) THEN
          IF ((MODEL).OR.(OBJECT)) THEN
              LCOLS = .FALSE.
              GOTO 99
          ENDIF
          LYPRCL = .TRUE.
      ELSE
          LYPRCL = .FALSE.
      ENDIF
                
C     If MODEL then for each new model then take a new layer
      IF (MODEL) THEN
          IF ((OBJECT).OR.(LYPRCL)) THEN
             MODEL = .FALSE.
             GOTO 99
          ENDIF
          MODFLG = .TRUE.
      ELSE
          MODFLG = .FALSE.
      ENDIF
 
C     If OBJECT then for each new object then take a new layer
      IF (OBJECT) THEN
          IF ((MODEL).OR.(LYPRCL)) THEN
             OBJECT = .FALSE.
             GOTO 99
          ENDIF
          OBJFLG = .TRUE.
      ELSE
          OBJFLG = .FALSE.
      ENDIF
 
C     If COLLAY then we will only take one colour per layer
      IF (COLLAY) THEN
          IF (LABCOL) THEN
              COLLAY = .FALSE.
              GOTO 99
          ENDIF
          ONCLLY = .TRUE.
      ELSE
          ONCLLY = .FALSE.
      ENDIF

C     Does user want colours to labels
      IF (LABCOL) THEN
C         If ONCLLY already set then error
          IF (COLLAY) THEN
              LABCOL = .FALSE.
              GOTO 99
          ENDIF
          LABCLR = .TRUE.
      ELSE
          LABCLR = .FALSE.
      ENDIF
 
C     If POLY then the user wants the lines converted into poly lines
C     this must be set so that LABCOL can be set and that is mutually
C     exclusive with one col per layer
      IF (POLY) THEN
          IF (LINES) THEN
             POLY = .FALSE.
             GOTO 99
          ENDIF
          PLYLIN = .TRUE.
      ELSE
          PLYLIN = .FALSE.
      ENDIF                     

      IF (LINES) THEN
          IF (POLY) THEN
             LINES = .FALSE.
             GOTO 99
          ENDIF
          LINLIN = .TRUE.
      ELSE
          LINLIN = .FALSE.
      ENDIF                                                 
                
C     Does user wish moss symbols turned into daxcad markers ?
      IF (SYMARK) THEN
          SYMMRK = .TRUE.
      ELSE
          SYMMRK = .FALSE.
      ENDIF          
                                                              
C     If multiple sheets does user want them split and placed
C     off to the right 
      IF (SHEET) THEN
          MULSHT = .TRUE.
      ELSE
          MULSHT = .FALSE.
      ENDIF
 
98    CONTINUE
      ST = 0
      RETURN
 
99    CONTINUE
      DNUM = 762
      CALL DEPRNT(DNUM)
      ST = 1
      END
C       @(#)  256.1 date 4/19/90 mifsht.ftn Daxcad revision 1.8
      SUBROUTINE MIFSHT(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
      include 'include/params.inc'
C
      INTEGER ST
C
C     get SHEET number
      CALL MIF004(2,CSHEET,ST)
C     get drawing sheet size in drawing units
      CALL MIF003(3,SHEETX,ST)
      CALL MIF003(4,SHEETY,ST)
C     get number of WINDOWS on SHEET
      CALL MIF004(5,NWINDS,ST)

      IF (MULSHT) THEN                                   
C         Only do this if user has selected multiple sheets
          IF (FIRSHT) THEN
C             First time so do not do anything
              FIRSHT = .FALSE.
          ELSE                                
C             Add the offset in to move the next stuff
C             add in a small offset to ensure a gap between
C             sheets
              SHOFTX = SHOFTX + DRWSIZ(1) + 50.0
              SHOFTY = 0.0
          ENDIF
      ENDIF

      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifsym.ftn Daxcad revision 1.8
      SUBROUTINE MIFSYM(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C                  
      INTEGER*4 MAXLIN,DNUM
      PARAMETER (MAXLIN = 3000)

      INTEGER XMRK(1:MAXLIN),YMRK(1:MAXLIN),PENMRK(1:MAXLIN)
      INTEGER ST,NSYMBI,SYMBI,SYMTYP,NL,FN,NLINS,NREFS
      INTEGER I,J,K,IX,IY,IPEN,MRKCNT
      CHARACTER*12 SNAM


C     Only generate file if Symbol to marker specified
      IF (SYMMRK) THEN
C        Attempt to open marker file if fail error
         OPEN(UNIT=MRKUNT,FILE=MRKNAM,STATUS='UNKNOWN',ERR=98)
         REWIND(UNIT=MRKUNT)                             
      ENDIF
C
C     get number of defined SYMBOLS
      CALL MIF004(2,NSYMBI,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NSYMBI
C        Set count of lines to 0 so that we do not write out
C        any spurious numbers
         MRKCNT = 0
C        get SYMBOL index
         CALL MIF004(FN+1,SYMBI,ST)
C        get SYMBOL type
         CALL MIF004(FN+2,SYMTYP,ST)
C        get length of symbol name
         CALL MIF004(FN+3,NL,ST)
C        get symbol name
         CALL MIF005(FN+4,SNAM,ST)
C         write(*,*)'SYMBOL:',SNAM(1:NL)
C        get number of lines in the definition
         CALL MIF004(FN+5,NLINS,ST)
C        get number of reference points
         CALL MIF004(FN+6,NREFS,ST)
C        update pointer to next field
         FN=FN+6                                            
C        enter a loop to interpret all line strings
C        within the definition
C        set auxiliary pointer
         K=1
         DO 9 J=1,NLINS
C           start new line string
C            write(*,*)'Starting new symbol string'
C           get 3 integers
 7          CONTINUE
            CALL MIF004(FN+K,IX,ST)
            CALL MIF004(FN+K+1,IY,ST)
            CALL MIF004(FN+K+2,IPEN,ST)
            K=K+3
            IF (K.EQ.7) THEN
               FN=FN+6
               K=1
            END IF
            IF (IX.NE.-1 .OR. IY.NE.-1 .OR.IPEN.NE.-1) THEN
C               add point to line string
C               write(*,*)'ix=',IX,' iy=',IY,' ipen=',IPEN
C               Change the pen commands to daxad marker format
                IF (IPEN.EQ.3) IPEN = 0
                IF (IPEN.EQ.2) IPEN = 1                       
C               Now write the co-ordinates <x> <y> <pen> into
C               a temp array so we can count the lines.
                MRKCNT = MRKCNT + 1
                XMRK(MRKCNT) = IX
                YMRK(MRKCNT) = IY
                PENMRK(MRKCNT) = IPEN
C               Jump back round
                GOTO 7
            END IF
 9       CONTINUE              
         IF (K.GT.1) FN=FN+6

C     If not symbols to markers then just discard         
      IF (SYMMRK) THEN
C     
C         Having reached here we can write out the whole thing into the
C         file
C         Write the number of the marker and the number of lines
          WRITE(UNIT = MRKUNT,FMT='(2I11)',ERR = 99) SYMBI,MRKCNT
C         Now write out the symbol name
          WRITE(UNIT = MRKUNT,FMT='(1A)',ERR = 99) SNAM(1:NL)
C         Now write out the symbol info
          DO 11 J=1,MRKCNT
             WRITE(UNIT = MRKUNT,FMT='(3I11)',ERR = 99)
     +      XMRK(J),YMRK(J),PENMRK(J)
 11       CONTINUE
      ENDIF
 10   CONTINUE
C     read next record for ENDSYMBL
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDMSYMBL') THEN
          ST=0
          IF (SYMMRK) THEN
C            At end of symbol definition so close file and cause daxcad
C            to load it
             CLOSE(UNIT = MRKUNT,ERR = 99)
             CALL LDLMRK(MRKNAM,ST)
          ENDIF
      ELSE
         ST=1
      ENDIF
      RETURN           
 99   CONTINUE
      ST = 1
      RETURN
 98   CONTINUE
C     Inform user failed to create mif marker file
      DNUM = 761
      CALL DEPRNT(DNUM)
      ST=1
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifthk.ftn Daxcad revision 1.8
      SUBROUTINE MIFTHK(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      DOUBLE PRECISION RTHICK
      INTEGER ST,FN,NTHIKI,THICKI,I
C
C     get number of defined thicknesses
      CALL MIF004(2,NTHICK,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NTHICK
C        get index
         CALL MIF004(FN+1,THICKI,ST)
         THKIND(I)=THICKI
C        get THICKNESS
         CALL MIF003(FN+2,RTHICK,ST)
C        store in table (scale by dbu factor)
         THKTAB(I)=RTHICK*DRAWFC(DRAWUN)
         FN=FN+6
 10   CONTINUE
C     read next record for ENDTHICK
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDTHICK') THEN
         ST=0
      ELSE
         ST=1
      END IF
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 miftx0.ftn Daxcad revision 1.8
      SUBROUTINE MIFTX0(NCH,FN,STRING)
C     ================================
C
C1    vartype            I  I   C*80
C1    iostatus           I  IO   O
C
      include 'include/moss_mif.inc'
C
      INTEGER NCH,FN
      INTEGER N,N1,P,I,NN,NLEN1,S,ST
      CHARACTER*80 STRING,TEXT*12
      EXTERNAL NLEN1
C
C     find number of text fields used
      N=INT(NCH/12)
      N1=MOD(NCH,12)
      IF (N1.NE.0) N=N+1
C     clear string
      STRING=' '
      P=1
C     get text fields and concatenate
      DO 10 I=1,N
C        get text field,MIF006 is same as MIF005 except does not strip
C        leading spaces.
         CALL MIF006(FN,TEXT,ST)
         IF (I.EQ.N) THEN
C            If the last field take the spec field width which is 12
C            and subtract this from N1 the actual field width and then
C            justify by that amount to the left if non zero.
             IF (N1.NE.0) THEN
                 CALL SHIFT(TEXT,N1 - 12)
             ENDIF
         ENDIF

C        place text field in string
         STRING(P:)=TEXT
C        Add twelve as we now guarantee that we will not strip any 
C        spaces off the start of the string.
         P = P + 12
         FN=FN+1
 10   CONTINUE
C
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 miftxt.ftn Daxcad revision 1.8
      SUBROUTINE MIFTXT(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/masti.inc'
      include 'include/style.inc'
      include 'include/moss_mif.inc'
C
      INTEGER ST,FN,I,TFONTI,COLI,THCKI,NCH,N,P,N1
      INTEGER NLEN1,S,NN
      INTEGER*2 TJ,TFONT,TCOL,TLAY
      DOUBLE PRECISION TOX,TOY,TA
      REAL TX,TY,TW,TH,TANG,TSLT,R,AZIMUT,DEG
      CHARACTER STRING*80,TEXT*12
      LOGICAL OK
      EXTERNAL NLEN1,AZIMUT,DEG
C
C     set track value
      INENTY=ITXT
C     get TEXT FONT index
      CALL MIF004(2,TFONTI,ST)
C     get COLOUR index
      CALL MIF004(3,COLI,ST)
C     get THICKNESS index
      CALL MIF004(4,THCKI,ST)
C     get bottom left coordinate of text string
      CALL MIF003(5,TOX,ST)
      CALL MIF003(6,TOY,ST)
C     scale position to paper units
      TOX=TOX*DRAWFC(DRAWUN) + SHOFTX
      TOY=TOY*DRAWFC(DRAWUN) + SHOFTY
C     get ANGLE of text (radians)
      CALL MIF003(7,TA,ST)
C     convert radians to degrees
      R=TA
      R=DEG(R)
      TA=AZIMUT(R)
C     get number of chars in text string
      CALL MIF004(8,NCH,ST)
C     set field number to first field of text
      FN=9
C     go read text string
      CALL MIFTX0(NCH,FN,STRING)
 
C     write string to database
C     Set as defaults 
      TFONT = CLFONT
      TCOL = COLI

C     Handle the change of the line type and colour if tying
C     layers to colour
      IF (LABCLR) THEN
          TFONT = WLINTP
          TCOL = COLOUR
      ENDIF                                         

C     Get the layer to place the entity on
C     Set up the work layer
      IF (LYPRCL) THEN
          TLAY = WLAYER + COLI - 1
      ELSE
          TLAY=WLAYER
      ENDIF

C     Handle the colour now if one colour per layer
      IF (ONCLLY) THEN
         TCOL = COLOUR
      ENDIF                                        

      TX=TOX
      TY=TOY
      TANG=TA
      TSLT=0
      TJ=1
      TH=TXPARS(1,TFONTI)
      TW=TXPARS(2,TFONTI)
      CALL DEWC85(TX,TY,TW,TH,TANG,TSLT,TJ,TFONT,TCOL,TLAY,STRING,P,OK)
C     read next field for 'ENDTEXT'
      CALL MIF005(FN,TEXT,ST)
      IF (TEXT.EQ.'ENDTEXT') THEN
         ST=0
      ELSE
         ST=998
      END IF
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 miftyp.ftn Daxcad revision 1.8
      SUBROUTINE MIFTYP(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,NLTYPI,LTYPI,LTGRUP,NL,FN,NLINS
      INTEGER HWLTYP,I,J,K,IX,IY,IPEN
      CHARACTER*12 LTNAM
C
C     get number of defined LINE TYPES
      CALL MIF004(2,NLTYPI,ST)
C     set initial field count
      FN=6
      DO 10 I=1,NLTYPI
C        get LTYPE index
         CALL MIF004(FN+1,LTYPI,ST)
C        get GROUP type
         CALL MIF004(FN+2,LTGRUP,ST)
         IF (LTGRUP.EQ.1) THEN
C           MOSS hardware line type
            CALL MIF004(FN+3,HWLTYP,ST)
C            write(*,*)'MOSS Line Type:',HWLTYP
C           update pointer to next field
            FN=FN+6
         ELSE
C           user defined line type
C           get length of line type name
            CALL MIF004(FN+3,NL,ST)
C           get line type name
            CALL MIF005(FN+4,LTNAM,ST)
C            write(*,*)'USER Line Type:',LTNAM(1:NL)
C           get number of lines in the definition
            CALL MIF004(FN+5,NLINS,ST)
C           update pointer to next field
            FN=FN+6
C           enter a loop to interpret all line strings
C           within the definition
C           set auxiliary pointer
            K=1
            DO 9 J=1,NLINS
C              start new line string
C               write(*,*)'Starting new line type string'
C              get 3 integers
 7             CONTINUE
               CALL MIF004(FN+K,IX,ST)
               CALL MIF004(FN+K+1,IY,ST)
               CALL MIF004(FN+K+2,IPEN,ST)
               K=K+3
               IF (K.EQ.7) THEN
                  FN=FN+6
                  K=1
               END IF
               IF (IX.NE.-1 .OR. IY.NE.-1 .OR.IPEN.NE.-1) THEN
C                 add point to line string
C                  write(*,*)'ix=',IX,' iy=',IY,' ipen=',IPEN
                  GOTO 7
               END IF
 9          CONTINUE
            IF (K.GT.1) FN=FN+6
         END IF
 10   CONTINUE
C     read next record for ENDLTYPE
      CALL MIF001(ST)
      IF (BLKNAM.EQ.'ENDLTYPE') THEN
         ST=0
      ELSE
         ST=1
      END IF
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifvec.ftn Daxcad revision 1.8
      SUBROUTINE MIFVEC(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,FN,I,COLI,THCKI,NPTS
      DOUBLE PRECISION X1,Y1,X2,Y2
      INTEGER*2 TF,TL,P
      LOGICAL FIRST,OK,DRAW
      CHARACTER*12 STRING
C
C     set track value
      INVECT=IVEC
C     get COLOUR index
      CALL MIF004(2,COLI,ST)
C     get THICKNESS index
      CALL MIF004(3,THCKI,ST)
C     get number of points in string
      CALL MIF004(4,NPTS,ST)
      FN=5
C     go get points of polyline
      FIRST=.TRUE.

C     Set up the necessary values
C     Handle the change of the line type if required
      IF (ONCLLY) THEN
          TF  = WLINTP
      ELSE
          TF = 1
      ENDIF                                         

C     Handle the change of the line type and colour if tying
C     layers to colour
      IF (LABCLR) THEN
          TF = WLINTP
C          TCOL = COLOUR
      ENDIF                                         

C     Set up the work layer
      IF (LYPRCL) THEN
          TL = WLAYER + COLI - 1
      ELSE
          TL=WLAYER
      ENDIF

      DO 10 I=1,NPTS
         IF (FIRST) THEN
            CALL MIF003(FN,X1,ST)
            CALL MIF003(FN+1,Y1,ST)
            FN=FN+2
            FIRST=.FALSE.
C           vectors are given already scaled
C           must scale back to paper units
            X1=X1*DRAWFC(DRAWUN) + SHOFTX
            Y1=Y1*DRAWFC(DRAWUN) + SHOFTY
         ELSE
            CALL MIF003(FN,X2,ST)
            CALL MIF003(FN+1,Y2,ST)
            FN=FN+2
C           vectors are given already scaled
C           scale to paper units
            X2=X2*DRAWFC(DRAWUN) + SHOFTX
            Y2=Y2*DRAWFC(DRAWUN) + SHOFTY
            DRAW = .TRUE.
            IF (INENTY.EQ.IARC) DRAW = .FALSE.
            IF ((PLYLIN).AND.(INENTY.EQ.IPOLY)) DRAW = .FALSE.
            IF ((LINLIN).AND.(INENTY.EQ.IPOLY)) DRAW = .FALSE.
            IF ((SYMMRK).AND.(INENTY.EQ.ICEN)) DRAW = .FALSE.
                                                             
            IF (DRAW) THEN
C              ignore vectors within ARC or symbols or if lines
C              are lines or polylines
               CALL DEWC03(REAL(X1),REAL(Y1),REAL(X2),REAL(Y2),
     +                      TF,TL,P,OK)
            ENDIF
            X1=X2
            Y1=Y2
         END IF
10    CONTINUE
C     read next field for 'ENDVECTORS'
      CALL MIF005(FN,STRING,ST)
      IF (STRING.EQ.'ENDVECTORS') THEN
C        clear track value
         INVECT=INONE
         ST=0
      ELSE
         ST=998
      END IF
C
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 mifwin.ftn Daxcad revision 1.8
      SUBROUTINE MIFWIN(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
      include 'include/moss_mif.inc'
C
      INTEGER ST,FN,WSHETN
C
C     get SHEET number
      CALL MIF004(2,WSHETN,ST)
C     get WINDOW number on SHEET
      CALL MIF004(3,CWINDO,ST)
C     get type of drawing,1=plan,2=long section,3=cross section
      CALL MIF004(4,DRWTYP,ST)
C     get number of models in window
      CALL MIF004(5,NWMODS,ST)
C     update field number
      FN=6
C     get X size in drawing units
      CALL MIF003(FN+1,WINDOX,ST)
      CALL MIF003(FN+2,WINDOY,ST)
C     get margins in drawing units
      CALL MIF003(FN+3,LMARGN,ST)
      CALL MIF003(FN+4,BMARGN,ST)
      CALL MIF003(FN+5,RMARGN,ST)
      CALL MIF003(FN+6,TMARGN,ST)
      FN=FN+6
C     get bottom left point in world coords
      CALL MIF003(FN+2,WRLDOX,ST)
      CALL MIF003(FN+4,WRLDOY,ST)
C     get page rotation (radians)
      CALL MIF003(FN+5,PAGROT,ST)
      FN=FN+6
C     get horizontal scale
      CALL MIF003(FN+1,HSCALE,ST)
C     get vertical scale
      CALL MIF003(FN+1,VSCALE,ST)
      END
C
C     -----------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 redmif.ftn Daxcad revision 1.8
      SUBROUTINE REDMIF()
C     ===================
C
C1    vartype
C1    iostatus
C
C2    Subroutine REDGI02 is the control routine
C2    for the READ GENIO function.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/layer.inc'
      include 'include/vntable.inc'
      include 'include/moss_mif.inc'

C                             
      DOUBLE PRECISION AVA
      REAL X,Y
      INTEGER*4 TMEN,TCELL,ST,C,SSLY,DNUM,NLEN,I4,NINT
      INTEGER*4 DATA(1:256),NDATA
      LOGICAL OK,OK2,OKAY
      LOGICAL FILE,MNMODE,CWL,OCL,LTOC,MODLAY,OBJLAY,SPECLY
      LOGICAL POLYLN,ACCPT,CVERFY,LINES,SYMARK,DUMMY,MSHETS
      LOGICAL LAYCOL
      CHARACTER*128 FILNM,CC*1
      CHARACTER*80  MIFNAM,BUF,LYNAM
      CHARACTER*1   HM,UM,DUMCHR
      EXTERNAL RDGENI,NLEN,AEXPRN                     
      INTRINSIC NINT
                                       
C     Initialise all the menu logicals
      SSLY = 1000
      MNMODE = .FALSE.                                     
      SPECLY = .FALSE.
      CWL = .TRUE.
      OCL = .FALSE.
      LTOC = .FALSE.
      MODLAY = .FALSE.
      OBJLAY = .FALSE.
      FILE = .FALSE.  
      ACCPT = .FALSE.
      POLYLN = .FALSE.
      LINES = .FALSE.
      SYMARK = .FALSE.                
      MSHETS = .FALSE.
      LAYCOL = .FALSE.
      DUMCHR = ' '
C
C     Initialise the menu with all the options
      CALL GTDMEN(569,3)
      CALL GTDMEN(570,3)
      CALL GTDMEN(571,3)
      CALL GTDMEN(572,3)
      CALL GTDMEN(573,3)
      CALL GTDMEN(574,3)
      CALL GTDMEN(575,3)
      CALL GTDMEN(576,3)
      CALL GTDMEN(577,3)                  
      CALL GTDMEN(578,3)                  
      CALL GTDMEN(579,3)                  
      CALL GTDMEN(580,3)                  
      CALL GTDMEN(581,3)                  

C     Highlight CWL menu 571   
      TCELL = 4
      CALL GTMCHI(3,TCELL)

C     Ensure all global variables are set
      CALL MIFSET(SSLY,MODLAY,OBJLAY,OCL,POLYLN,LTOC,LINES,SYMARK,
     +            MSHETS,LAYCOL,ST)
C     
C     Handle the input 
10    CONTINUE            
C     Prompt user for it
      DNUM = 764
      CALL DCPRNT(DNUM)
      CALL TCURS(C,X,Y)

      IF (MEN.EQ.0) THEN
C     117 "Invalid area of the screen"
          CALL DEPRNT(117)
          GOTO 10
      ENDIF
                             
C     If not our menu then exit
      IF (MEN.NE.3) GOTO 1000
              
C     Else what is it
      IF (CVERFY(CCMD,'F')) THEN
C         Handle Filename                          
          DNUM = 676
          CALL DPRMXP(DNUM,MIFNAM)
          IF (NLEN(MIFNAM).NE.0) THEN
C             Only highlight if not highlit already
              IF (.NOT.FILE) THEN
                  CALL MIFPIC(.TRUE.,FILE,DUMMY,'F',DUMCHR,HM,UM)
              ENDIF
          ELSE 
              FILE = .FALSE.
              HM = ' '
              UM = 'F'
          END IF
      ELSE IF (CVERFY(CCMD,'o')) THEN
C         One colour per layer       
          CALL MIFPIC(.TRUE.,OCL,LTOC,'o','l',HM,UM)
      ELSE IF (CVERFY(CCMD,'l')) THEN
C         Labels to colours
          CALL MIFPIC(.FALSE.,OCL,LTOC,'o','l',HM,UM)
      ELSE IF (CVERFY(CCMD,'M')) THEN
C         Every new model take a new layer 
          CALL MIFPIC(.TRUE.,MODLAY,OBJLAY,'M','O',HM,UM)
      ELSE IF (CVERFY(CCMD,'O')) THEN
C         Every new object take a new layer 
          CALL MIFPIC(.FALSE.,MODLAY,OBJLAY,'M','O',HM,UM)
      ELSE IF (CVERFY(CCMD,'P')) THEN
C         handle Polylines as Polylines not as individual
          CALL MIFPIC(.TRUE.,POLYLN,LINES,'P','L',HM,UM)
      ELSE IF (CVERFY(CCMD,'L')) THEN
C         handle Polylines as lines not individual vectors
          CALL MIFPIC(.FALSE.,POLYLN,LINES,'P','L',HM,UM)
      ELSE IF (CVERFY(CCMD,'s')) THEN
C         handle Symbols as markers not vectors
          CALL MIFPIC(.TRUE.,SYMARK,DUMMY,'s',DUMCHR,HM,UM)
      ELSE IF (CVERFY(CCMD,'m')) THEN
C         handle mutliple sheets offset to the right 
          CALL MIFPIC(.TRUE.,MSHETS,DUMMY,'m',DUMCHR,HM,UM)
      ELSE IF (CVERFY(CCMD,'C')) THEN
C         Start from current work layer
          CALL MIFPIC(.FALSE.,SPECLY,CWL,'S','C',HM,UM)
      ELSE IF (CVERFY(CCMD,'Z')) THEN
C         Start from current work layer
          CALL MIFPIC(.TRUE.,LAYCOL,DUMMY,'Z',DUMCHR,HM,UM)
          IF (OBJLAY) THEN
              OBJLAY = .FALSE.
              CALL GTHFMC(3,'O',TCELL)
              CALL GTMCLO(3,TCELL)
          ENDIF
          IF (MODLAY) THEN
              MODLAY = .FALSE.
              CALL GTHFMC(3,'M',TCELL)
              CALL GTMCLO(3,TCELL)
          ENDIF                                     
      ELSE IF (CVERFY(CCMD,'S')) THEN
C         Handle Start layer defined  
          IF (SPECLY) THEN 
C             lose the Number and redo the menu cell
              BUF = VNOUN(570)
              I4=INDEX(BUF,':')
C             protect agains no colon.
              IF (I4.EQ.0) I4=13

              WRITE(UNIT=VNOUN(570),FMT='(A)',ERR=98) BUF(1:I4)  
98            CONTINUE
C             Clear the menu cell first 
              CALL GTHFMC(3,CCMD,TCELL)
              CALL GTMCLO(3,TCELL)
              CALL GTCLRC(3,TCELL)        
C             Write into the menu cell the modified layer
              CALL GTDMEN(570,3)  
              SSLY = 1000
          ELSE
C             Ask for the number and then fill the menu cell
              DNUM = 766
              CALL DPRMXP(DNUM,LYNAM)
              IF( NLEN(LYNAM).EQ.0 ) THEN
                  GOTO 102  
              ENDIF                 
C             Check layer is valid
              CALL STRNUM(LYNAM,DATA,NDATA)
C             no data returned ?
              IF ( NDATA .EQ. 0) THEN
                 CALL DEPRNT(134)
                 GOTO 102
              END IF
C             check range of layer.
              IF ( DATA(1) .LT. 0 .OR. DATA(1) .GT. 255 ) THEN
                 CALL DEPRNT(134)
                 GOTO 102
              END IF
C             Set up the start layer number
              SSLY = DATA(1)
C             Now fill the menu cell
              BUF = VNOUN(570)
              I4=INDEX(BUF,':')
C             protect agains no colon.
              IF (I4.EQ.0) I4=13

              WRITE(UNIT=VNOUN(570),FMT='(A,I3)',ERR=99) BUF(1:I4),
     +                          SSLY  
99            CONTINUE         
C             Clear the menu cell first 
              CALL GTHFMC(3,CCMD,TCELL)
              CALL GTMCLO(3,TCELL)
              CALL GTCLRC(3,TCELL)        
C             Write into the menu cell the modified layer
              CALL GTDMEN(570,3)  
              CALL GTMCHI(3,TCELL)
101           CONTINUE
          ENDIF          
C         Handle if successful
          CALL MIFPIC(.TRUE.,SPECLY,CWL,'S','C',HM,UM)
          GOTO 103
102       CONTINUE
C         If error unhighlight this one
          UM = 'S'
          HM = ' '
103       CONTINUE
      ELSE IF (CVERFY(CCMD,'A')) THEN
C         Go for it, but check that the filename has been
C         specified and also the layers has been.         
          ST = 1
          OKAY = .FALSE.         
          IF (SPECLY) THEN
              OKAY = .TRUE.
          ELSE IF (CWL) THEN
              OKAY = .TRUE.
          ENDIF
          IF (.NOT.OKAY) THEN
              DNUM = 767
              CALL DEPRNT(DNUM)
          ELSE IF (.NOT.FILE) THEN 
C            Inform user that file must be specified
             DNUM = 765
             CALL DEPRNT(DNUM)                           
             OKAY = .FALSE.         
          ELSE
             OKAY = .TRUE.
          ENDIF

          IF (OKAY) THEN
C             Show accept is okay
              CALL MIFPIC(.TRUE.,ACCPT,DUMMY,'A',DUMCHR,HM,UM)
          ELSE            
              HM = ' '
              UM = 'A'
          ENDIF
      ELSE
C         STUFFED
      ENDIF
                      
901   CONTINUE                             
C     If any of these two are on turn off the laycol one 
      IF ((OBJLAY).OR.(MODLAY)) THEN
          IF (LAYCOL) THEN
              LAYCOL = .FALSE.
              CALL GTHFMC(3,'Z',TCELL)
              CALL GTMCLO(3,TCELL)
          ENDIF               
      ENDIF

      CALL MIFSET(SSLY,MODLAY,OBJLAY,OCL,POLYLN,LTOC,LINES,SYMARK,
     +               MSHETS,LAYCOL,ST)
                                                                 
902   CONTINUE
      IF (HM.NE.' ') THEN
          CALL GTHFMC(3,HM,TCELL)
          CALL GTMCHI(3,TCELL)
      ENDIF

      IF (UM.NE.' ') THEN
          CALL GTHFMC(3,UM,TCELL)
          CALL GTMCLO(3,TCELL)
      ENDIF

C     If accept then go and read file
      IF (ACCPT) THEN
          GOTO 900
      ENDIF
C     Go back round
      GOTO 10

900   CONTINUE
C     If CWL set WLAYER then use current layer
      IF (CWL) THEN
          WLAYER = CLAYER
      ENDIF

C     Set the to globals to TRUE for the first time
C     into miflay and mifclr which ensure that we do
C     not jump a layer or a colour when we start off.
C
      FIRLAY = .TRUE.
      FIRCLR = .TRUE. 
      FIRSHT = .TRUE. 
C     Set up the global offsets for the sheet to zero
      SHOFTX = 0.0
      SHOFTY = 0.0

C     go read the data file
      CALL GETMIF(MIFNAM,ST)               

C     Show accept to be false so that we can go back round
      ACCPT = .FALSE.
C     Unhighlight the accept menu cell Token 'A'
      UM = 'A'
      IF (UM.NE.' ') THEN
          CALL GTHFMC(3,UM,TCELL)
          CALL GTMCLO(3,TCELL)
      ENDIF
C     Go back round 
      GOTO 10

C                              
1000  CONTINUE
      END
C
C     --------------------------------------------------
C
