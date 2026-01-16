C
C     @(#)  412.1 date 6/11/92 moss_mif.inc 
C
C
C     Filename    : moss_mif.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:25
C     Last change : 92/06/11 14:35:49
C
C     Copyright : Practical Technology Limited  
C     File :- moss_mif.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C2    This include file contains all global definitions
C2    required for processing of MOSS SYSTEMS LTD
C2    MIF format drawing files.
C2
C2    CSHEET contains the number of the current SHEET
C2    NWINDS contains the number of WINDOWS on the current SHEET
C2    CWINDO contains the number of the current WINDOW
C2    NWMODS contains the number of MODELS in the current WINDOW
C2    CMODEL contains the name of the current MODEL
C2    NMOBJS contains the number of OBJECTS in the current MODEL
C2    COBJ   contains the name of the current OBJECT
C2    NOELMS contains the number of ELEMENTS in the current OBJECT
C2    CELEM  contains the name of the current ELEMENT
C2    INENTY contains the track value for the entity being processed
C2    INVECT contains a non-zero track value if vectors are being processed
C2    MOSREV contains the revision number of MOSS used to create the MIF file
C2    MIFREV contains the revision number of the MOSS-MIF output translator
C2    DRAWUN contains the drawing units identifier (1=mm,2=cm)
C2    DRAWFC contains the factors for each of the drawing units
C2    SHEETX contains draw sheet X size (drawing units)
C2    SHEETY contains draw sheet Y size (drawing units)
C2    COLMIX contains the RGB mix for each defined colour
C2    COLNAM contains the names of defined colours
C2    NCOLRS contains total number of defined colours
C2    NTHICK contains total number of defined thickbesses
C2    THKTAB contains the thicknesses defined
C2    THKIND contains the thickness reference index
C2    NTFONT contains the total number of Text Font definitions
C2    TFNTID contains the Text Font reference index
C2    TSNAME contains the Text Style name
C2    TFNAME contains the Text Font name
C2    TXPARS contains the text parameters (Height,Width,Spacing,Angle,Thickness)
C2    DRWTYP contains the type of MOSS drawing (1=plan,2=long section,3=cross section)
C2    WINDOX contains window X size (drawing units)
C2    WINDOY contains window Y size (drawing units)
C2    RMARGN contains the right margin size (drawing units)
C2    LMARGN contains the left margin size (drawing units)
C2    TMARGN contains the top margin size (drawing units)
C2    BMARGN contains the bottom margin size (drawing units)
C2    WRLDOX contains bottom left corner X of window (world coords)
C2    WRLDOY contains bottom left corner Y of window (world coords)
C2    HSCALE contains X scale of window
C2    VSCALE contains Y scale of window
C2    PAGROT contains Page rotation (radians)
C2    WLAYER contains the layer to place the model/object on
C2           if zero use the current work layer.
C2    WLINTP contains the current line type if doing colour/fonts to labels
C2    MODFLG if TRUE take a new layer for each MODEL in mif
C2    OBJFLG if TRUE take a new layer for each OBJECT in mif
C2    ONCLLY if TRUE assign one colour per layer to mif data
C2    PLYLIN if TRUE then convert lines into polylines not vectors
C2    LABCLR if TRUE then assign label colours to each object type
C2    FIRCLR if TRUE then do not increment colour the first time
C2    FIRLAY if TRUE then do not increment layer the first time
C2    LINLIN if TRUE then convert lines into polylines not vectors
C2    SYMMRK if TRUE then convert symbols into markers
C2    MULSHT if TRUE then for each sheet move to the right by sheet size
C2    LYPRCL if TRUE then put each colour on one layer
C2    SHOFTX holds the current sheet offset in the X direction 
C2    SHOFTY holds the current sheet offset in the Y direction 
C                                       
      LOGICAL MODFLG,OBJFLG,ONCLLY,PLYLIN,LABCLR,FIRCLR,FIRLAY,LINLIN
      LOGICAL SYMMRK,MULSHT,FIRSHT,LYPRCL
      INTEGER MIFUNT,MRKUNT,NMIFLN,BLKRN
      INTEGER DRAWUN,NSHEET,SHEETN,NWINDS,DRWTYP,NWMODS,NMOBJS,NOELMS
      INTEGER CSHEET,CWINDO,INENTY,INVECT,NCOLRS,NTHICK,THKIND(16)
      INTEGER NTFONT,TFNTID(16),WLAYER,WLINTP
      DOUBLE PRECISION MOSREV,MIFREV,SHEETX,SHEETY,COLMIX(3,16)
      DOUBLE PRECISION THKTAB(16),TXPARS(5,16),DRAWFC(2)
      DOUBLE PRECISION WINDOX,WINDOY,LMARGN,BMARGN,RMARGN,TMARGN
      DOUBLE PRECISION WRLDOX,WRLDOY,PAGROT,HSCALE,VSCALE
      DOUBLE PRECISION SHOFTX,SHOFTY
      CHARACTER*80 MIFBUF,BLKNAM*12,MRKNAM
      CHARACTER*12 CMODEL*40,COBJ,CELEM,COLNAM(16),TSNAME(16),TFNAME(16)
      COMMON /MIF1/MIFUNT,MRKUNT,NMIFLN,BLKRN,DRAWUN,NSHEET,
     +             MOSREV,MIFREV,SHEETX,SHEETY,
     +             SHEETN,NWINDS,DRWTYP,NWMODS,NMOBJS,NOELMS,
     +             WINDOX,WINDOY,LMARGN,RMARGN,BMARGN,TMARGN,
     +             WRLDOX,WRLDOY,PAGROT,HSCALE,VSCALE,
     +             CSHEET,CWINDO,INENTY,INVECT,NCOLRS,NTHICK,THKIND,
     +             COLMIX,THKTAB,NTFONT,TFNTID,WLAYER,WLINTP

      COMMON /MIF2/MIFBUF,BLKNAM,CMODEL,COBJ,CELEM,COLNAM,TSNAME,TFNAME,
     +             MRKNAM                         
      COMMON /MIF3/MODFLG,OBJFLG,ONCLLY,PLYLIN,LABCLR,FIRCLR,FIRLAY,
     +              LINLIN,SYMMRK,MULSHT,FIRSHT,LYPRCL 
      COMMON/MIF4/SHOFTX,SHOFTY,TXPARS,DRAWFC

C2    The folowing block defines control parameters for
C2    tracking entities during processing.

      INTEGER IPOLY,IARC,ISYM,ICEN,IPIP,ITXT,IHAT,IVEC,INONE
      PARAMETER (
     +            IPOLY=1,
     +            IARC =2,
     +            ISYM =3,
     +            ICEN =4,
     +            IPIP =5,
     +            ITXT =6,
     +            IHAT =7,
     +            IVEC =1,
     +            INONE=0 )


