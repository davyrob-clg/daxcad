C
C     @(#)  412.1 date 6/11/92 params.inc 
C
C
C     Filename    : params.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:31
C     Last change : 92/06/11 14:37:16
C
C     Copyright : Practical Technology Limited  
C     File :- params.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    this include file describes the format
C2    of the working parameters for creation
C2    of a drawing.
C
      REAL DBUFAC,PAPFAC,DRWSCL,DRWSIZ(2)
      REAL PAPSIZ(2,6),DBFACS(8),ROOTRV,OROTRV,DRGREV
C
      CHARACTER DBUNIT*2,PAPUNT*2,DRWSHT*3,PAPLST(6)*2,DBULST(8)*2
      CHARACTER DRGSCL*12,DRGNAM*80,FILTYP*80,PAPNAM(6)*14
      CHARACTER DBUNTK(8)*1 
C
      INTEGER*4 PAPVNO(6),CURPNO
C
C2    DBUFAC is the conversion factor for current DB units
C2    PAPFAC is the conversion factor for current paper units
C2    DRWSCL is the current drawing scale
C2    DRWSIZ is the size of the current drawing sheet (paper units)
C2    PAPSIZ contains the size in MM of standard drawing sheets
C2    DBFACS contains conversion factors for all units
C2
C2    DBUNIT is the current DB units descriptor
C2    PAPUNT is the current paper units descriptor
C2    DRWSHT is the current drawing sheet descriptor
C2    PAPLST contains the list of valid sheet descriptors
C2    DBULST contains the valid list of DB unit descriptors
C2    DRGSCL contains the scale in character string form
C2    DRGNAM is the current drawing name
C2    FILTYP is the description of the file format 
C2    ROOTRV is the current revision number of DAXCAD
C2    OROTRV is the revision of DAXCAD used to create drawing
C2    DRGREV is the revision number of the drawing
C2    DBUNTK is the table of tokens for the units popup
C2    PAPVNO is the table of verb/noun number for the paper
C2           to allow access to the PAPER menu text and
C2           associated menu tokens. 
C
      COMMON /RPARAM/ DBUFAC,PAPFAC,DRWSCL,DRWSIZ,PAPSIZ,
     +                DBFACS,ROOTRV,OROTRV,DRGREV
C
      COMMON /CPARAM/ DBUNIT,PAPUNT,DRWSHT,PAPLST,DBULST,DRGSCL,
     +                DRGNAM,FILTYP,PAPNAM
C
      COMMON /MNTOKK/ DBUNTK,PAPVNO,CURPNO
