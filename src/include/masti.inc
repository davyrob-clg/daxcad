C
C     @(#)  412.1 date 6/11/92 masti.inc 
C
C
C     Filename    : masti.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:23
C     Last change : 92/06/11 14:34:49
C
C     Copyright : Practical Technology Limited  
C     File :- masti.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    CLAYER is the current construction layer
C2    CLFONT is the current construction line font
C2    LSFONT is the previous font style
C
CAPOLLO
      INTEGER*2 CMIP(1:2,1:32000),DFILE(1:2,1:32000,0:5),
     +          RLFILE(1:10,1:5000)
C                                
      CHARACTER*80 TXFILE(32000)
C
CAPOLLO
CSUN
C      INTEGER*2 MIFILE(1:10,1:10000),PDFI(1:4,1:10000),
C     1          CMIP(1:2,1:10000),DFILE(1:2,1:10000,1:2),
C     2          RLFILE(1:10,1:5000)
CC                                
C      CHARACTER*80 TXFILE(10000)
CC
C      REAL      PDFR(1:6,1:10000),T2FILE(1:9,1:5000)
CSUN
CIBM
C      INTEGER*2 MIFILE,PDFI,
C     1          CMIP,DFILE,
C     2          RLFILE
CC                                
C      CHARACTER*80 TXFILE
CC
C      REAL      PDFR,T2FILE
CIBM
C
      INTEGER*2 NMIPOS,NPDPOS,NTXPOS,NRLPOS,NT2POS,MIP,PDFP,TXTP,
     +          NLYPOS,CONT,SP2,RECCNT(2),DFP,DFPINC,DFPNT,LDFILE(0:5),
     2          CLAYER,TLAYER(0:255),DELENT,
     3          MIPLIM,PDPLIM,TXPLIM,RLPLIM,T2PLIM,CLFONT,LSFONT,
     4          PRILIM,PRCLIM
C
C2    MIPLIM max number of MI records
C2    PDPLIM max number of PD records
C2    TXPLIM max number of TX records
C2    RLPLIM max number of RL records
C2    PRILIM max number of PR records
C2    PRCLIM max number of PC records
C
      INTEGER*2 MIFILU,PDFILU,TXFILU,PRFILU,DDFILU(2),FFILU,
     +          RLFILU,T2FILU
      INTEGER*2 DDFLIM
C
      INTEGER*4 GSSTAT
C2       GSSTAT is the GROUP search status
C      
      LOGICAL DISPV,VLAYER(0:255),ENSRCH(0:127)
C
      CHARACTER*20 LNAME(0:255)

      COMMON /MASTI/ NMIPOS,NPDPOS,NTXPOS,NRLPOS,NT2POS,
     1               MIPLIM,PDPLIM,TXPLIM,RLPLIM,T2PLIM,PRILIM,PRCLIM,
     2               MIP,PDFP,TXTP,CONT,SP2,DFP,DFPINC,NLYPOS,
     3               DFPNT,LDFILE,RECCNT,
     4               CLAYER,CLFONT,LSFONT,TLAYER,DELENT,
     5               CMIP,
     6               MIFILU,PDFILU,TXFILU,PRFILU,DDFILU,FFILU,
     7               RLFILU,T2FILU,
     8               DDFLIM
C
      COMMON /MASTI1/RLFILE,DFILE

      COMMON /MASTI2/DISPV,GSSTAT,VLAYER,ENSRCH

      COMMON /MASTC/ TXFILE,LNAME
