C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 data.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DIGINT()
C     SUBROUTINE INATTX()
C     SUBROUTINE INITDB()
C     SUBROUTINE INMAC()
C     SUBROUTINE INTPAP(STATUS)
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE DIGINT()
C     ===================
C
      include 'include/datatype.inc'
      include 'include/dig2wo.inc'
      include 'include/menug.inc'
 
C     digitise control data.
      STRIDO=0
      STRIDI=0
      DIGIT=.FALSE.
      DX=0.0
      DY=0.0
      DIGSNA=1.0
      DIGANG=2.0
      WDIGS=1.0
      WDIGAN=2.0
C
      WRLDX=0.0
      WRLDY=0.0
      DIGX=0.0
      DIGY=0.0
      SCADIG=1.0
      ANGDIG=0.0
C
      CALL I3M(DIGWXY)
C     disable digitser menus
      MENOK=.FALSE.
C
      END
C
C     ----------------------------------------------------------------
C
      SUBROUTINE INATTX()
C     ===================
C
C1    No   Args
C
C2    Subroutine INATTX initialises the attribute text
C2    that goes into the attribute menu cells after the colon
C2    the cells COLOUR, FONT: and THICK: i.e. it initialises
C2    text RED in COLOUR:RED
C2
C
      include 'include/style.inc'
      include 'include/lfont.inc'
      include 'include/masti.inc'
      include 'include/vntable.inc'
      include 'include/inscen.inc'
C
      INTEGER*2 TFONT,ENDVAL,MASK,AND_2
C
C     right set the menu text entry for COLOUR
      TXCLR = VNOUN(413+COLOUR)
C     right set the menu text entry for FONT
      TXFONT = FONTNM(CLFONT)
C     right set the menu text entry for centre line FONT
      CLINET = FONTNM(CLINEF)
C     right set the menu text entry for THICK
      TFONT = THICK
      MASK = 15 * 1024
C     Mask is f00. This picks out the line end number.
      ENDVAL = AND_2(PLTHKI(TFONT),MASK)  / 1024
      WRITE(TXTHK,'(I3,A,F4.2,A,I2,A)',ERR=90) TFONT,' {',
     +                             LTHKR(1,TFONT),',',ENDVAL,'}'
C
      GOTO 100
 90   CONTINUE
      WRITE(10,*)'[INATTX] Error in write ',TXCLR,TXCLR,TXFONT
C
 100  CONTINUE
C
      END
      SUBROUTINE INITDB()
C     ===================
C
C2    Subroutine INITDB clears all database
C2    control variables.
C
      include  'include/style.inc'
      include  'include/masti.inc'
      include  'include/props.inc'
      include  'include/menun.inc'
      include  'include/swind.inc'
      include  'include/nbuff.inc'
      include  'include/wtov.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/filunit.inc'
      include  'include/lfont.inc'
      include  'include/apollo.inc'
      include  'include/vntable.inc'
      include  'include/macro.inc'
      include  'include/curwin.inc'
      include  'include/movdat.inc'
      include  'include/viewport.inc'
C
      INTEGER*4 I
      INTEGER*4 ST
      LOGICAL OK
C
      XWIDTH =   8.0
      YWIDTH =   8.0
      WIDTHL =   16.0
      ASCII=.FALSE.
      DFPINC=1
      DELENT=0
      DFP=1
      CMOVE=4
      DFPNT=1
      IDBUFF(4)=0
 
C     clear the data pointers to zero
      DELENT=0
      DFP=1
      DFPNT=1
      NMIPOS=1
      NPDPOS=1
      NTXPOS=1
      NRLPOS=1
      NPRPOS=1
      NPCPOS=1
      LDFILE(CVPN)=1
C     Grid should be off when we start
      SETGRD=.FALSE.
C
C     set drawing rev to start
      DRGREV=0.99
      FILTYP='DRAWING'
C
C     clear layer visibility
      DO 35,I=0,255
         TLAYER(I)=0
         VLAYER(I)=.FALSE.
         LNAME(I)='                                '
 35   CONTINUE
C
C     set default construction layer
C     Default is layer 0
      VLAYER(1)=.TRUE.
      CLAYER=1
C
C     set the workspace size for transformation
C     make it 3% bigger then the paper mapping
C     ensure previous view is a valid one
      LWXMIN=WXMIN
      LWYMIN=WYMIN
      LWXMAX=WXMAX
      LWYMAX=WYMAX
C
C     ensure section data block is properly initialized
      CALL OSCLRM()
C
C     Set Cursor flags
      MENHI=.FALSE.
      MACFIL=.FALSE.
C     set scaling flags now used for text operations
      REFDAT(1,3) = 1.000
      REFDAT(7,3) = 1.000
C
      CALL INITVP()
C
C     Initialise new MMU
C
      CALL DATABASEINIT(ST)

      END
C
C     ----------------------------------------------------------------
C
 
      SUBROUTINE INMAC()
C     ==================
C
      include  'include/macro.inc'
      INTEGER I
C
      DO 10 I=1,4
         CELP(I)=0
         CELS(I)=1
 10   CONTINUE
C
      CELS(1)=3
      CELS(2)=2
      CELS(3)=4
      MACOP=.FALSE.
      NTOK=.FALSE.
      GINPUT=.FALSE.
      PROMPT=.TRUE.
      MENUS=.TRUE.
      TCONT=.FALSE.
      GANS=.FALSE.
      MACPOP = .FALSE. 
      END
C
C     ----------------------------------------------------------------
C
      SUBROUTINE INTPAP(STATUS)
C     ===================
C
C1    vartype            I*4
C1    iostatus            I
C
C2    initialises the paper list and the paper names
C2    to the menu text read in from the dictionary file
C2    also initialises the paper/vnoun table which keeps
C2    each papers dictionary entry
C
C2    STATUS is the status returned by LDLPAP
C2    0 if paper sizes defined by paper.pcf
C2    non zero if paper sizes have to be
C2    defined by the program
C
C
      include 'include/vntable.inc'
      include 'include/params.inc'
      include 'include/section.inc'
C
      INTEGER*4  STATUS

      IF(STATUS .EQ. 0) THEN
C        Store the paper descriptors.
         VNOUN(364)=PAPNAM(1)
         VNOUN(365)=PAPNAM(2)
         VNOUN(366)=PAPNAM(3)
         VNOUN(367)=PAPNAM(4)
         VNOUN(368)=PAPNAM(5)
      ELSE
C
C       The paper sizes are not user
C       defined so set them up from the
C       the  verb/noun table
C       now load the drawing sheet sizes
C       and blank the paper descriptors
C
C       A4 size
        PAPLST(1)=VNOUN(364)(:2)
        PAPNAM(1)=' '
        PAPSIZ(1,1)=210
        PAPSIZ(2,1)=297
C       A3 size
        PAPLST(2)=VNOUN(365)(:2)
        PAPNAM(2)=' '
        PAPSIZ(1,2)=420
        PAPSIZ(2,2)=297
C       A2 size
        PAPLST(3)=VNOUN(366)(:2)
        PAPNAM(3)=' '
        PAPSIZ(1,3)=594
        PAPSIZ(2,3)=420
C       A1 size
        PAPLST(4)=VNOUN(367)(:2)
        PAPNAM(4)=' '
        PAPSIZ(1,4)=841
        PAPSIZ(2,4)=594
C       A0 size
        PAPLST(5)=VNOUN(368)(:2)
        PAPNAM(5)=' '
        PAPSIZ(1,5)=1189
        PAPSIZ(2,5)=841
      ENDIF
C 
C     set up the papersize/vnoun
C     look up table
      PAPVNO(1) = 364
      PAPVNO(2) = 365
      PAPVNO(3) = 366
      PAPVNO(4) = 367
      PAPVNO(5) = 368
C
C     set default drawing sheet
      DRWSHT=PAPLST(2)
      DRWSIZ(1)=PAPSIZ(1,2)
      DRWSIZ(2)=PAPSIZ(2,2)
      CURPNO = 2
C
C
C     set default section sheet
      SHTKEY=DRWSHT
      SHTNAM=DRWSHT
      SHTSIZ(1)=DRWSIZ(1)
      SHTSIZ(2)=DRWSIZ(2)
C
C
      END
