C
C     @(#)  412.1 date 6/11/92 fhead.inc 
C
C
C     Filename    : fhead.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:12
C     Last change : 92/06/11 14:29:51
C
C     Copyright : Practical Technology Limited  
C     File :- fhead.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This include file contains the declarations of
C2    all variables required to construct a file header
C2    block for use with ROOTS drawings,parts,symbols.
C
      REAL RFHEAD(5)
C
      INTEGER*4 IFHEAD(6)
C
      CHARACTER*80 CFHEAD(5)
C
      COMMON /FHEAD1/CFHEAD
C
      COMMON /FHEAD2/RFHEAD,IFHEAD
C
C2    CFHEAD(1) = the file type
C2    CFHEAD(2) = the file or drawing name
C2    CFHEAD(3-5) spare
C
C2    RFHEAD(1) = DAXCAD revision number
C2    RFHEAD(2) = drawing revision number
C2    RFHEAD(3-5) spare
C
C2    IFHEAD(1) = year of last file modification
C2    IFHEAD(2) = month of last file modification
C2    IFHEAD(3) = day of last file modification
C2    IFHEAD(4) = hour of last file modification
C2    IFHEAD(5) = minute of last file modification
C2    IFHEAD(6) = second of last file modification
C
C
