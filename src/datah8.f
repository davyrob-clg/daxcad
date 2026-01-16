C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 datah8.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE GETDHD()
C     SUBROUTINE GETFHD()
C     SUBROUTINE GETNHD()
C     SUBROUTINE GETPHD()
C     SUBROUTINE GETVHD()
C     SUBROUTINE MAKDHD()
C     SUBROUTINE MAKFHD()
C     SUBROUTINE MAKNHD()
C     SUBROUTINE MAKPHD()
C     SUBROUTINE MAKVHD()
C     SUBROUTINE RSTDHD(OK)
C     SUBROUTINE RSTFHD(OK)
C     SUBROUTINE RSTNHD(OK)
C     SUBROUTINE RSTPHD(OK)
C     SUBROUTINE RSTSEC(OK)
C     SUBROUTINE RSTSSC(OK)
C     SUBROUTINE RSTVHD(OK)
C     SUBROUTINE SAVDHD(OK)
C     SUBROUTINE SAVFHD(OK)
C     SUBROUTINE SAVNHD(OK)
C     SUBROUTINE SAVPHD(OK)
C     SUBROUTINE SAVSEC(OK)
C     SUBROUTINE SAVSSC(OK)
C     SUBROUTINE SAVVHD(OK)
C     
C     |-----------------------------------------------------------------|
C
      SUBROUTINE GETDHD()
C     ===================
C
C1    No arguments required
C
C2    subroutine GETDHD retrieves a drawing header block from
C2    within the file header storage area,passsing it tothe current
C2    set of parameters available from the system data structure.
C
      include  'include/wtov.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/dhead.inc'
C
      DOUBLE PRECISION DN
      REAL REAL
      INTRINSIC REAL
C
      EXTERNAL AEXPRN
C
C*********************************
C     retrieve drawing header block
C*********************************
C     save database units and factor
      DBUNIT=CDHEAD(1)
      DBUFAC=RDHEAD(1)
C     save the paper units and factor
      PAPUNT=CDHEAD(2)
      PAPFAC=RDHEAD(2)
C     write the paper to world scale ratio
      PAPTOW=RDHEAD(3)
C     save the drawscale text form
      DRGSCL=CDHEAD(3)
C     fix for old drawings,(avoids zero drawscale)
      IF (RDHEAD(12).NE.0) THEN
C        save scale in numeric form
         DRWSCL=RDHEAD(12)
      ELSE
C        have to calculate drawscale
         CALL AEXPRN(DRGSCL,DN,*100)
         DRWSCL=REAL(DN)
 100     CONTINUE
      END IF
C     save the drawing sheet descriptor
      DRWSHT=CDHEAD(4)
C     save the  drawing size
      DRWSIZ(1)=RDHEAD(4)
      DRWSIZ(2)=RDHEAD(5)
C     write the extents to the file in database units
      WPXMIN=RDHEAD(6)
      WPYMIN=RDHEAD(7)
      WPXMAX=RDHEAD(8)
      WPYMAX=RDHEAD(9)
C     retrieve the paper origin position
      WPORGX=RDHEAD(10)
      WPORGY=RDHEAD(11)
C
      END
C
C
      SUBROUTINE GETFHD()
C     ===================
C
C1    No arguments rerquired
C
C2    Subroutine GETFHD retrieves data from the
C2    file header storage area passing it to the current set
C2    of parameters in the system data structure.
C
      include  'include/params.inc'
      include  'include/fhead.inc'
C
C******************************
C     retrieve file header block
C******************************
C
C     note,not all data is read back to parameter block
C     get the drawing name
      DRGNAM=CFHEAD(2)
C     get software revision number of drawing creation
      OROTRV=RFHEAD(1)
C     get drawing revision number here
      DRGREV=RFHEAD(2)
C     file header complete at this point
C
      END
C
C
      SUBROUTINE GETNHD()
C     ===================
C
C1    No arguments required
C
C2    Subroutine GETNHD retrieves a database header block
C2    containing the numbers of records stored of all the
C2    data types kept within the data structure.All the
C2    data types are stored sequentially in random length
C2    records in the file,immediately following the file
C2    header block.
C
      include  'include/masti.inc'
      include  'include/nhead.inc'
      include  'include/props.inc'
C
      INTEGER*4 I
C
C     ***************************************
C     retrieve database header block
C     ***************************************
C
C     get number of MI records
      NMIPOS=NHEADI(1)
C     get number of PD records
      NPDPOS=NHEADI(2)
C     get number of TX records
      NTXPOS=NHEADI(3)
C     get number of RL records
      NRLPOS=NHEADI(4)
C     get number of PI records
      NPRPOS=NHEADI(5)
C     get number of PC records
      NPCPOS=NHEADI(6)
C     get number of PC records
      NLYPOS=NHEADI(7)
C
      END
C
C
      SUBROUTINE GETPHD()
C     ===================
C
C1    No arguments required
C
C2    Subroutine GETPHD retrieves a parameter header block
C2    within the file header storage area,passing it to
C2    the system data structure.
C
      include  'include/masti.inc'
      include  'include/params.inc'
      include  'include/ndata.inc'
      include  'include/style.inc'
      include  'include/arcdat.inc'
      include  'include/dpars.inc'
C
C
C     ***************************************
C     retrieve drawing parameters header block
C     ***************************************
C
C     save dimension parameters
      DTHGT=RPARMS(1)
      DTWDT=RPARMS(2)
      DTOFF=RPARMS(3)
      GAPL=RPARMS(4)
      EXTL=RPARMS(5)
      ALNG=RPARMS(6)
      AWDT=RPARMS(7)
      TOL=RPARMS(8)
      PREC=IPARMS(1)
C     save hatching parameters
      HANG=RPARMS(9)
      LANG=RPARMS(10)
      HDIST=RPARMS(11)
      CROSSH=LPARMS(1)
C     save text parameters
      TWIDTH=RPARMS(15)
      THIGT=RPARMS(16)
      SLANT=RPARMS(17)
      TANGL=RPARMS(18)
C     save fillet parameters
      FRAD=RPARMS(12)
      FLTRIM=IPARMS(2)
      FILLET=IPARMS(3)
C     save grid parameters
      GRIDOX=RPARMS(21)
      GRIDOY=RPARMS(22)
      GRIDSX=RPARMS(23)
      GRIDSY=RPARMS(24)
      SETGRD=LPARMS(3)
C     save copy conditions
      NNCOPY=IPARMS(5)
      COPYIT=LPARMS(2)
C     save arc construction parameters
      ARCRAD=RPARMS(13)
      ANGLE=RPARMS(14)
      RADSET=LPARMS(4)
      ARCSET=LPARMS(5)
C     save current line font
      CLFONT=IPARMS(6)
C     save the current construction layer
      CLAYER=IPARMS(7)
C     save the default entity status
      COLOUR=IPARMS(8)
      LAYER=IPARMS(9)
      SPARE=IPARMS(10)
      FONT=IPARMS(11)
      STATUS=IPARMS(12)
      GRTYPE=IPARMS(13)
C
C     end of parameter block
C
      END
C
C
      SUBROUTINE GETVHD()
C     ===================
C
C1    no arguments requierd
C
C2    Subroutine GETVHD retrieves the viewing header block
C2    within the file header block storage area,passsing it
C2    to the system data structure.
C
      include  'include/wtov.inc'
      include  'include/vhead.inc'
C
      INTEGER*4 I,J,K
C
C*********************************
C     retrieve viewing header block
C*********************************
C     save extents of last view used
      WXMIN=RVHED1(1,2)
      WYMIN=RVHED1(2,2)
      WXMAX=RVHED1(4,2)
      WYMAX=RVHED1(5,2)
C     save extents of paper on world
      WPXMIN=RVHED1(1,3)
      WPYMIN=RVHED1(2,3)
      WPXMAX=RVHED1(4,3)
      WPYMAX=RVHED1(5,3)
C     save last world to view transform
      K=-2
      DO 50 J=0,2
         K=K+3
         DO 60 I=0,2
            WVXY(I+1,J+1)=RVHED2(K+I,3)
 60      CONTINUE
 50   CONTINUE
C
C     save last view to world transform
      K=-2
      DO 51 J=0,2
         K=K+3
         DO 61 I=0,2
            VWXY(I+1,J+1)=RVHED2(K+I,4)
 61      CONTINUE
 51   CONTINUE
C
C     end of viewing header block
C
      END
C
C
      SUBROUTINE MAKDHD()
C     ===================
C
C1    No arguments required
C
C2    subroutine MAKDHD creates a drawing header block
C2    within the file header storage area,using the current
C2    set of parameters available from the system data structure.
C
      include  'include/wtov.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/dhead.inc'
C
      INTEGER*4 I
C
C*********************************
C     create drawing header block
C*********************************
C     clear the character data
      DO 50 I=1,10
         CDHEAD(I)='        '
         RDHEAD(I)=0.0
         RDHEAD(I+10)=0.0
 50   CONTINUE
C     save database units and factor
      CDHEAD(1)=DBUNIT
      RDHEAD(1)=DBUFAC
C     save the paper units and factor
      CDHEAD(2)=PAPUNT
      RDHEAD(2)=PAPFAC
C     write the paper to world scale ratio
      RDHEAD(3)=PAPTOW
C     save the drawscale character string
      CDHEAD(3)=DRGSCL
C     save the drawscale value
      RDHEAD(12)=DRWSCL
C     save the drawing sheet descriptor
      CDHEAD(4)=DRWSHT
C     save the  drawing size
      RDHEAD(4)=DRWSIZ(1)
      RDHEAD(5)=DRWSIZ(2)
C     write the extents to the file in database units
      RDHEAD(6)=WPXMIN
      RDHEAD(7)=WPYMIN
      RDHEAD(8)=WPXMAX
      RDHEAD(9)=WPYMAX
C     write the paper origin to the file in database units
      RDHEAD(10)=WPORGX
      RDHEAD(11)=WPORGY
C
      END
C
C
      SUBROUTINE MAKFHD()
C     ===================
C
C1    No arguments rerquired
C
C2    Subroutine MAKFHD creates a file header in the
C2    file header storage area using the current set
C2    of parameters available from the system data structure.
C
      include  'include/params.inc'
      include  'include/fhead.inc'
CIBM
C          INCLUDE '\INCLUDE\VERTYPE.INC'
CIBM
CPC386
C          INCLUDE 'INCLUDE\\VERTYPE.INC'
CPC386
C
      INTEGER*4 I
C
C******************************
C     create file header block
C******************************
C     clear the character data
      DO 50 I=1,5
         CFHEAD(I)='      '
         RFHEAD(I)=0.0
 50   CONTINUE
C     identify the type of file being written
      CFHEAD(1)= FILTYP
C     place drawing name here
      CFHEAD(2)=DRGNAM
C     store software revision number
      RFHEAD(1)=ROOTRV
C     write drawing revision number here
      RFHEAD(2)=DRGREV+0.01
C     file header complete at this point
CIBM|PC386
C      CFHEAD(3)='FULL'
C      IF(RFIDF.LT.0.0) CFHEAD(3)='DEMO'
C      RFHEAD(3)=RFIDF
C      RFHEAD(4)=RFIDF2
CIBM|PC386
C
      END
C
C
      SUBROUTINE MAKNHD()
C     ===================
C
C1    No arguments required
C
C2    Subroutine MAKNHD creates a database header block
C2    containing the numbers of records stored of all the
C2    data types kept within the data structure.All the
C2    data types are stored sequentially in random length
C2    records in the file,immediately following the file
C2    header block.
C
      include  'include/masti.inc'
      include  'include/nhead.inc'
      include  'include/props.inc'
C
      INTEGER*4 I,NLEN
      INTEGER*2 J
      EXTERNAL NLEN
C
C     ***************************************
C     create database header block
C     ***************************************
C
C     save number of MI records
      NHEADI(1)=NMIPOS-1
C      WRITE(10,*) 'NHEADI(1),NMIPOS',NHEADI(1),NMIPOS
 
C     save number of PD records
      NHEADI(2)=NPDPOS-1
C     save number of TX records
      NHEADI(3)=NTXPOS-1
C     save number of relation records
      NHEADI(4)=NRLPOS-1
C     save number of property index records.
      NHEADI(5)=NPRPOS-1
C     save number of property character records.
      NHEADI(6)=NPCPOS-1
C     number of layer entries which have names aginst them.
 
C     find out how many layer have been given names.
      NLYPOS=1
      DO 10 J=0,255
         IF ( NLEN(LNAME(J)).NE.0 ) THEN
            NLYPOS=NLYPOS+1
         END IF
 10   CONTINUE
C
      NHEADI(7)=NLYPOS-1
C     save record length of MI (2-byte integers).
      NHEADS(1)=10
C
C     save record length of PD (4-byte reals).
      NHEADS(2)=6
C     save record length of TX (characters).
      NHEADS(3)=80
C     save record length of RL (2-byte integers).
      NHEADS(4)=10
C     save record length of PI (2-byte integers).
      NHEADS(5)=8
C     save record length of PC (ASCII characters).
      NHEADS(6)=80
C     save record length of layer name ( number,name).
      NHEADS(7)=22
C
      DO 52 I=8,12
         NHEADI(I)=0
         NHEADS(I)=0
 52   CONTINUE
C
      END
C
C
      SUBROUTINE MAKPHD()
C     ===================
C
C1    No arguments required
C
C2    Subroutine MAKPHD creates a parameter header block
C2    within the file header storage area,using the current
C2    parameters found in the system data structure.
C
      include  'include/masti.inc'
      include  'include/params.inc'
      include  'include/ndata.inc'
      include  'include/style.inc'
      include  'include/arcdat.inc'
      include  'include/dpars.inc'
C
      INTEGER*4 I
C
C     ***************************************
C     create drawing parameters header block
C     ***************************************
C
C     clear parameter buffers
      DO 20 I=1,50
         RPARMS(I)=0.0
         IPARMS(I)=0
 20   CONTINUE
      DO 21 I=1,10
         LPARMS(I)=.FALSE.
 21   CONTINUE
C
C     save dimension parameters
      RPARMS(1)=DTHGT
      RPARMS(2)=DTWDT
      RPARMS(3)=DTOFF
      RPARMS(4)=GAPL
      RPARMS(5)=EXTL
      RPARMS(6)=ALNG
      RPARMS(7)=AWDT
      RPARMS(8)=TOL
      IPARMS(1)=PREC
C     save hatching parameters
      RPARMS(9)=HANG
      RPARMS(10)=LANG
      RPARMS(11)=HDIST
      LPARMS(1)=CROSSH
C     save text parameters
      RPARMS(15)=TWIDTH
      RPARMS(16)=THIGT
      RPARMS(17)=SLANT
      RPARMS(18)=TANGL
C     save fillet parameters
      RPARMS(12)=FRAD
      IPARMS(2)=FLTRIM
      IPARMS(3)=FILLET
C     save grid parameters
      RPARMS(21)=GRIDOX
      RPARMS(22)=GRIDOY
      RPARMS(23)=GRIDSX
      RPARMS(24)=GRIDSY
      LPARMS(3)=SETGRD
C     save copy conditions
      IPARMS(5)=NNCOPY
      LPARMS(2)=COPYIT
C     save arc construction parameters
      RPARMS(13)=ARCRAD
      RPARMS(14)=ANGLE
      LPARMS(4)=RADSET
      LPARMS(5)=ARCSET
C     save current line font
      IPARMS(6)=CLFONT
C     save the current construction layer
      IPARMS(7)=CLAYER
C     save the default entity status
      IPARMS(8)=COLOUR
      IPARMS(9)=LAYER
      IPARMS(10)=SPARE
      IPARMS(11)=FONT
      IPARMS(12)=STATUS
      IPARMS(13)=GRTYPE
C
C     end of parameter block
      END
C
C
      SUBROUTINE MAKVHD()
C     ===================
C
C1    no arguments requierd
C
C2    Subroutine MAKVHD creates a viewing header block
C2    within the file header block storage area,using the
C2    data currently available from the system data structure.
C
      include  'include/wtov.inc'
      include  'include/vhead.inc'
C
      INTEGER*4 I,J,K
C
C*********************************
C     create viewing header block
C*********************************
C     Clear the spare character spaces
      DO 52 I=1,10
         CVHEAD(I)='        '
 52   CONTINUE
C     clear real variable work space
      DO 40 I=1,6
         DO 41 J=1,6
            RVHED1(J,I)=0.0
 41      CONTINUE
 40   CONTINUE
      DO 42 I=1,6
         DO 43 J=1,9
            RVHED2(J,I)=0.0
 43      CONTINUE
 42   CONTINUE
      DO 44 I=1,12
         DO 45 J=1,16
            RVHED3(J,I)=0.0
 45      CONTINUE
 44   CONTINUE
C
C     save extents of last view used
      RVHED1(1,2)=WXMIN
      RVHED1(2,2)=WYMIN
      RVHED1(3,2)=0
      RVHED1(4,2)=WXMAX
      RVHED1(5,2)=WYMAX
      RVHED1(6,2)=0
C     save extents of paper on world
      RVHED1(1,3)=WPXMIN
      RVHED1(2,3)=WPYMIN
      RVHED1(3,3)=0
      RVHED1(4,3)=WPXMAX
      RVHED1(5,3)=WPYMAX
      RVHED1(6,3)=0
C     save last world to view transform
      K=-2
      DO 50 J=0,2
         K=K+3
         DO 60 I=0,2
            RVHED2(K+I,3)=WVXY(I+1,J+1)
 60      CONTINUE
 50   CONTINUE
C
C     save last view to world transform
      K=-2
      DO 51 J=0,2
         K=K+3
         DO 61 I=0,2
            RVHED2(K+I,4)=VWXY(I+1,J+1)
 61      CONTINUE
 51   CONTINUE
C
C     end of viewing header block
C
      END
C
C
      SUBROUTINE RSTDHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTDHD reads the drawing header
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/dhead.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C     read character info first
      DO 50 I=1,10
         READ(UNIT=PARFUN,ERR=99) CDHEAD(I)
 50   CONTINUE
C
C     read the real data now
      DO 51 I=1,20
         READ(UNIT=PARFUN,ERR=99) RDHEAD(I)
 51   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
C
C
      SUBROUTINE RSTFHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTFHD reads the file header
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/fhead.inc'
CIBM
C          INCLUDE '\INCLUDE\VERTYPE.INC'
CIBM
CPC386
C          INCLUDE 'INCLUDE\\VERTYPE.INC'
CPC386
 
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C     read character info first
      DO 50 I=1,5
C        clear the data space
         CFHEAD(I)=' '
         READ(UNIT=PARFUN,ERR=99) CFHEAD(I)
 50   CONTINUE
C
C     read the real data now
      DO 51 I=1,5
         READ(UNIT=PARFUN,ERR=99) RFHEAD(I)
 51   CONTINUE
C
C     now read the time of last modification
      DO 52 I=1,6
         READ(UNIT=PARFUN,ERR=99) IFHEAD(I)
 52   CONTINUE
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
CIBM|PC386
C      RFIDFC=RFHEAD(3)
C      RFIDF1=RFHEAD(4)
C      IF(RFIDF.GT.0.0.OR.RFIDF2.GT.0.0) THEN
C         IF(RFHEAD(3).LT.0.0.OR.RFHEAD(4).LT.0.0) OK=.FALSE.
C      END IF
CIBM|PC386
C
      END
C
C
      SUBROUTINE RSTNHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTNHD reads the database header
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/nhead.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C
C     read the numbers of records used
      DO 51 I=1,12
         READ(UNIT=PARFUN,ERR=99) NHEADI(I)
 51   CONTINUE
C
C     read record lengths used
      DO 52 I=1,12
         READ(UNIT=PARFUN,ERR=99) NHEADS(I)
 52   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
 
      SUBROUTINE RSTPHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTPHD reads the parameters header
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/dpars.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C     read the real info first
      DO 50 I=1,50
         READ(UNIT=PARFUN,ERR=99) RPARMS(I)
 50   CONTINUE
C
C     read the integer data now
      DO 51 I=1,50
         READ(UNIT=PARFUN,ERR=99) IPARMS(I)
 51   CONTINUE
C
C     now read the logicals from file
      DO 52 I=1,10
         READ(UNIT=PARFUN,ERR=99) LPARMS(I)
 52   CONTINUE
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE RSTSEC(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTSEC reads the section block
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/section.inc'
      include  'include/params.inc'
C
      INTEGER*4 I,J
CAPOLLO|PC386
      INTEGER*4 IOSGXY(4)
CAPOLLO|PC386
CSUN
C      REAL IOSGXY(4)
CSUN
      LOGICAL OK
C
      OK=.FALSE.
C     read section sheet definition first
C     drawing scale
      READ(UNIT=PARFUN,ERR=99) SECSCL
C     C*3 key for sheet
      READ(UNIT=PARFUN,ERR=99) SHTKEY
C     C*14 name for sheet
      READ(UNIT=PARFUN,ERR=99) SHTNAM
C     2xR for sheet size in mm
      READ(UNIT=PARFUN,ERR=99) SHTSIZ(1),SHTSIZ(2)
C
C     read the section global coords now
      DO 51 I=0,16
C        read the section flag
         READ(UNIT=PARFUN,ERR=99) SECTOK(I)
C        read the section name
         READ(UNIT=PARFUN,ERR=99) SECNAM(1,I)
C        read the bitmap name
         READ(UNIT=PARFUN,ERR=99) SECNAM(2,I)
C        read the section global coords
         IF (OROTRV.LT.2.202) THEN
C         this check needed because of
C         change to double precision during
C         BETA test of version 2.2 of s/w
C         retrieve as integer(apollo) or real(sun)
C         data
          READ(UNIT=PARFUN,ERR=99) (IOSGXY(J),J=1,4)
          DO 5 J=1,4
           OSGXY(J,I)=IOSGXY(J)
 5        CONTINUE
         ELSE
C         retrieve as double precision
          READ(UNIT=PARFUN,ERR=99) (OSGXY(J,I),J=1,4)
         END IF
C        read the section local coords
         READ(UNIT=PARFUN,ERR=99) (WWGXY(J,I),J=1,4)
 51   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE RSTSSC(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTSSC reads the section data
C2    for the current section number
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/section.inc'
      include  'include/params.inc'
C
      INTEGER*4 I,J
      LOGICAL OK
CAPOLLO|PC386
      INTEGER*4 IOSGXY(4)
CAPOLLO|PC386
CSUN
C      REAL IOSGXY(4)
CSUN
C
      OK=.FALSE.
C     read section sheet definition first
C     drawing scale
      READ(UNIT=PARFUN,ERR=99) SECSCL
C     C*3 key for sheet
      READ(UNIT=PARFUN,ERR=99) SHTKEY
C     C*14 name for sheet
      READ(UNIT=PARFUN,ERR=99) SHTNAM
C     2xR for sheet size in mm
      READ(UNIT=PARFUN,ERR=99) SHTSIZ(1),SHTSIZ(2)
C
c      WRITE(10,*)'[RSTSSC] restoring to:',SECNUM
C     use current section number for load
      I=SECNUM
C     read the section global coords
C     read the section flag
      READ(UNIT=PARFUN,ERR=99) SECTOK(I)
C     read the section name
      READ(UNIT=PARFUN,ERR=99) SECNAM(1,I)
C     read the bitmap name
      READ(UNIT=PARFUN,ERR=99) SECNAM(2,I)
C     read the section global coords
      IF (OROTRV.LT.2.202) THEN
C      this check needed because of
C      change to double precision during
C      BETA test of version 2.2 of s/w
C      retrieve as integer(apollo) or real(sun)
C      data
       READ(UNIT=PARFUN,ERR=99) (IOSGXY(J),J=1,4)
       DO 5 J=1,4
        OSGXY(J,I)=IOSGXY(J)
 5     CONTINUE
      ELSE
C      retrieve as double precision
       READ(UNIT=PARFUN,ERR=99) (OSGXY(J,I),J=1,4)
      END IF
 
C     read the section local coords
      READ(UNIT=PARFUN,ERR=99) (WWGXY(J,I),J=1,4)
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C     --------------------------------------------------
C
 
      SUBROUTINE RSTVHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine RSTVHD reads the viewing header
C2    from the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/vhead.inc'
C
      INTEGER*4 I,J
C
      LOGICAL OK
C
      OK=.FALSE.
C     read character info first
      DO 50 I=1,10
         READ(UNIT=PARFUN,ERR=99) CVHEAD(I)
 50   CONTINUE
C
C     read the real data now (RVHED1)
      DO 51 J=1,6
         READ(UNIT=PARFUN,ERR=99) (RVHED1(I,J),I=1,6)
 51   CONTINUE
C
C     read the real data now (RVHED2)
      DO 52 J=1,6
         READ(UNIT=PARFUN,ERR=99) (RVHED2(I,J),I=1,9)
 52   CONTINUE
C
C     read the real data now (RVHED3)
      DO 53 J=1,12
         READ(UNIT=PARFUN,ERR=99) (RVHED3(I,J),I=1,16)
 53   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
      SUBROUTINE SAVDHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVDHD writes the drawing header
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/dhead.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C     write character info first
      DO 50 I=1,10
         WRITE(UNIT=PARFUN,ERR=99) CDHEAD(I)
 50   CONTINUE
C
C     write the real data now
      DO 51 I=1,20
         WRITE(UNIT=PARFUN,ERR=99) RDHEAD(I)
 51   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SAVFHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVFHD writes the file header
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/fhead.inc'
C
      INTEGER*4 I
      INTEGER*4 TIMDAT(6)
C
      LOGICAL OK
      EXTERNAL LOCALT
C
      OK=.FALSE.
C
C     write character info first
      DO 50 I=1,5
         WRITE(UNIT=PARFUN,ERR=99) CFHEAD(I)
 50   CONTINUE
C
C     write the real data now
      DO 51 I=1,5
         WRITE(UNIT=PARFUN,ERR=99) RFHEAD(I)
 51   CONTINUE
C
C     find what the time is
      CALL LOCALT(TIMDAT)
C     now write the time to file
      DO 52 I=1,6
         IFHEAD(I)=TIMDAT(I)
         WRITE(UNIT=PARFUN,ERR=99) IFHEAD(I)
 52   CONTINUE
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SAVNHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVNHD writes the database header
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/nhead.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C
C     write the numbers of records used
      DO 51 I=1,12
C         WRITE(UNIT=10,FMT=*,ERR=99) I,NHEADI(I)
         WRITE(UNIT=PARFUN,ERR=99) NHEADI(I)
 51   CONTINUE
C
C     write the record lengths used
      DO 52 I=1,12
         WRITE(UNIT=PARFUN,ERR=99) NHEADS(I)
 52   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SAVPHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVPHD writes the parameters header
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/dpars.inc'
C
      INTEGER*4 I
C
      LOGICAL OK
C
      OK=.FALSE.
C     write the real info first
      DO 50 I=1,50
         WRITE(UNIT=PARFUN,ERR=99) RPARMS(I)
 50   CONTINUE
C
C     write the integer data now
      DO 51 I=1,50
         WRITE(UNIT=PARFUN,ERR=99) IPARMS(I)
 51   CONTINUE
C
C     now write the logicals to file
      DO 52 I=1,10
         WRITE(UNIT=PARFUN,ERR=99) LPARMS(I)
 52   CONTINUE
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE SAVSEC(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVSEC writes the section block
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/section.inc'
      include  'include/params.inc'
C
      INTEGER*4 I,J
      LOGICAL OK
C
      OK=.FALSE.
C     write section sheet definition first
C     drawing scale
      WRITE(UNIT=PARFUN,ERR=99) DRWSCL
C     C*3 key for sheet
      WRITE(UNIT=PARFUN,ERR=99) SHTKEY
C     C*14 name for sheet
      WRITE(UNIT=PARFUN,ERR=99) SHTNAM
C     2xR for sheet size in mm
      WRITE(UNIT=PARFUN,ERR=99) SHTSIZ(1),SHTSIZ(2)
C
C     write the section global coords now
      DO 51 I=0,16
C        write the section flag
         WRITE(UNIT=PARFUN,ERR=99) SECTOK(I)
C        write the section name
         WRITE(UNIT=PARFUN,ERR=99) SECNAM(1,I)
C        write the bitmap name
         WRITE(UNIT=PARFUN,ERR=99) SECNAM(2,I)
C        write the section global coords
         WRITE(UNIT=PARFUN,ERR=99) (OSGXY(J,I),J=1,4)
C        write the section local coords
         WRITE(UNIT=PARFUN,ERR=99) (WWGXY(J,I),J=1,4)
 51   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE SAVSSC(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVSSC writes the section data
C2    for the current section number
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/section.inc'
      include  'include/params.inc'
C
      INTEGER*4 I,J
      LOGICAL OK
C
      OK=.FALSE.
C     write section sheet definition first
C     drawing scale
      WRITE(UNIT=PARFUN,ERR=99) DRWSCL
C     C*3 key for sheet
      WRITE(UNIT=PARFUN,ERR=99) SHTKEY
C     C*14 name for sheet
      WRITE(UNIT=PARFUN,ERR=99) SHTNAM
C     2xR for sheet size in mm
      WRITE(UNIT=PARFUN,ERR=99) SHTSIZ(1),SHTSIZ(2)
C
C     use current section number for save
      I=SECNUM
C     write the section global coords
C     write the section flag
      WRITE(UNIT=PARFUN,ERR=99) SECTOK(I)
C     write the section name
      WRITE(UNIT=PARFUN,ERR=99) SECNAM(1,I)
C     write the bitmap name
      WRITE(UNIT=PARFUN,ERR=99) SECNAM(2,I)
C     write the section global coords
      WRITE(UNIT=PARFUN,ERR=99) (OSGXY(J,I),J=1,4)
C     write the section local coords
      WRITE(UNIT=PARFUN,ERR=99) (WWGXY(J,I),J=1,4)
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE SAVVHD(OK)
C     =====================
C
C1    vartype           L
C1    iostatus          O
C
C2    Subroutine SAVVHD writes the viewing header
C2    to the file currently connected to unit
C2    number PARFUN.Ok returns the status of the
C2    operation.
C
      include  'include/filunit.inc'
      include  'include/vhead.inc'
C
      INTEGER*4 I,J
C
      LOGICAL OK
C
      OK=.FALSE.
C     write character info first
      DO 50 I=1,10
         WRITE(UNIT=PARFUN,ERR=99) CVHEAD(I)
 50   CONTINUE
C
C     write the real data now (RVHED1)
      DO 51 J=1,6
         WRITE(UNIT=PARFUN,ERR=99) (RVHED1(I,J),I=1,6)
 51   CONTINUE
C
C     write the real data now (RVHED2)
      DO 52 J=1,6
         WRITE(UNIT=PARFUN,ERR=99) (RVHED2(I,J),I=1,9)
 52   CONTINUE
C
C     write the real data now (RVHED3)
      DO 53 J=1,12
         WRITE(UNIT=PARFUN,ERR=99) (RVHED3(I,J),I=1,16)
 53   CONTINUE
C
C     set status to ok
      OK=.TRUE.
C
 99   CONTINUE
C
      END
C
C
