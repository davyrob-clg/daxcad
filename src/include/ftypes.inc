C
C     @(#)  412.1 date 6/11/92 ftypes.inc 
C
C
C     Filename    : ftypes.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:14
C     Last change : 92/06/11 14:30:19
C
C     Copyright : Practical Technology Limited  
C     File :- ftypes.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C       File type requested
C       DAXDRG  Daxcad drawing file
C       DAXPRT    "    part file
C       DAXCMP    "    component file
C       DAXSYM    "    symbol file
C       DAXSEC    "    property
C       DAXSEC    "    sectiom
C       DAXASC    "    binary file
C       DAXBOM    "    bill of materials 
C       DAXMAC    "    macro
C       DAXMIF    "    mif file
C       DAXGEN    "    genio
C       DAXDMF    "    digitiser menu file
C       DAXDXF    "    dxf AUTOCRUD
C       DAXIGS    "    IGES file
C
      INTEGER*4 DAXDRG
      INTEGER*4 DAXPRT
      INTEGER*4 DAXCMP
      INTEGER*4 DAXSYM
      INTEGER*4 DAXPOP
      INTEGER*4 DAXSEC
      INTEGER*4 DAXASC
      INTEGER*4 DAXBOM
      INTEGER*4 DAXMAC
      INTEGER*4 DAXMIF
      INTEGER*4 DAXGEN
      INTEGER*4 DAXDMF
      INTEGER*4 DAXDXF
      INTEGER*4 DAXIGS
      INTEGER*4 DAXTYP
C
      COMMON /FTYPE/DAXDRG,DAXPRT,DAXCMP,DAXSYM,DAXASC,DAXPOP,
     +        DAXSEC,DAXTYP,DAXBOM,DAXMAC,DAXMIF,DAXGEN,DAXDMF,
     1        DAXDXF,DAXIGS
