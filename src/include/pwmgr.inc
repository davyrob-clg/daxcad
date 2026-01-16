C     @(#)  412.1 date 6/11/92 pwmgr.inc 
C
C
C     Filename    : pwmgr.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:16:02
C     Last change : 92/06/11 14:39:13
C
C     Copyright : Practical Technology Limited  
C     File :- pwmgr.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Description this file contains the constants for control 
C     of the passwd mangr software.
C
C     
C     NORCODE -> normal autorisation (no extra prods)
C     DXFCODE -> authorisation code for DXF
C     IGSCODE -> authorisation code for IGES
C     INTCODE -> authorisation code for INTERLEAF
C     HPGCODE -> authorisation for HPGL
C     MIFCODE -> authorisation for MIF/GENIO
C     PEPCODE -> authorisation code for PEPS NC
C     GNCCODE -> authorisation code for GNC SPANS
C     CVXCODE -> authorisation code for CV-EXEC
C     PATCODE -> authorisation code for PATHTRACE
C     GERBER  -> authorisation code for GERBER PHOTOPLOTTER
C     PRDCNT  -> Total number of the above products
C     
C     
C      
      INTEGER*4 DXFCODE
      INTEGER*4 IGSCODE
      INTEGER*4 INTCODE
      INTEGER*4 HPGCODE
      INTEGER*4 MIFCODE
      INTEGER*4 PEPCODE 
      INTEGER*4 GNCCODE
      INTEGER*4 CVXCODE
      INTEGER*4 PATCODE
      INTEGER*4 GERCODE  
      INTEGER*4 PRDCNT 
      INTEGER*4 YEARZERO
C
      CHARACTER*3 DXFNAME
      CHARACTER*3 IGSNAME
      CHARACTER*3 INTNAME
      CHARACTER*3 HPGNAME
      CHARACTER*3 MIFNAME
      CHARACTER*3 PEPNAME
      CHARACTER*3 GNCNAME
      CHARACTER*3 CVXNAME
      CHARACTER*3 PATNAME
      CHARACTER*3 GERNAME
C
      PARAMETER(DXFNAME='DXF')
      PARAMETER(IGSNAME='IGS')
      PARAMETER(INTNAME='INT')
      PARAMETER(HPGNAME='HPG')
      PARAMETER(MIFNAME='MIF')
      PARAMETER(PEPNAME='PEP')
      PARAMETER(GNCNAME='GNC')
      PARAMETER(CVXNAME='CVX')
      PARAMETER(PATNAME='PAT')
      PARAMETER(GERNAME='GER')


      PARAMETER(DXFCODE = 1)
      PARAMETER(IGSCODE = 2)
      PARAMETER(INTCODE = 4)
      PARAMETER(HPGCODE = 8)
      PARAMETER(MIFCODE = 16)
      PARAMETER(PEPCODE = 32)
      PARAMETER(GNCCODE = 64)
      PARAMETER(CVXCODE = 128)
      PARAMETER(PATCODE = 256)
      PARAMETER(GERCODE = 512) 
C
C     count of the above
      PARAMETER(PRDCNT   = 10) 
      PARAMETER(YEARZERO = 1992) 
C
C
