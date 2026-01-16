C
C     @(#)  412.1 date 6/11/92 rcv1.inc 
C
C
C     Filename    : rcv1.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:39
C     Last change : 92/06/11 14:39:25
C
C     Copyright : Practical Technology Limited  
C     File :- rcv1.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This common block contains all the major data storage
C2    elements required to allow detailed analysis of the
C2    content of a computervision CADDS3/4 database file.
C
      INTEGER*2 MIDAT(1:500*256),PDFDAT(1:2500*256),TB4DAT(1:300*256)
      INTEGER*2 GMIDAT(1:500*256),GDFDAT(1:1000*256)
C      should be ones above
C      INTEGER*2 MIDAT(1:5*256),PDFDAT(1:2*256),TB4DAT(1:3*256)
C      INTEGER*2 GMIDAT(1:5*256),GDFDAT(1:1*256)
      INTEGER*2 IDREC(1:256),TETEAR(1:256)
      INTEGER*4 MIPOS,PDFPOS,TB4POS,MIBLKP,PDBLKP,TB4BLP,GMIBLP,GDFBLP
      INTEGER*4 GMIPOS,GDFPOS,IAVGMI,IAVGDF
      INTEGER*4 MIALH,IAVMI,IAVPDF,IAVTB4,MIBSIZ,GMIBSZ,GDFBSZ
      INTEGER*4 TB4BSZ,MISECS,PDSECS,TB4SEC,GMISEC,GDFSEC
      INTEGER*4 GCFENT,GCSTAT,GDFPNT,GMI5,GMI6,GMI7,GMI8
      INTEGER*4 CVLAYR,CVENTY,CVSTAT,PDFPNT,TB4PNT,OUTTYP
      INTEGER*4 MODTOT,DRWTOT
      INTEGER*4 OUTUN,CVFONT,MIWRD6
      LOGICAL CVMODL,CBLANK,CVISBL,EXTFND,SCLFND
      LOGICAL CVALOW(0:255)
      REAL CVDBU,CV4SX,CV4SY,CV4SZ,CVEXTS(6),CVSCLS(16)
C
      COMMON/RCV1/MIPOS,PDFPOS,TB4POS,
     +            MIBLKP,PDBLKP,TB4BLP,GMIBLP,GDFBLP,
     +            GMIPOS,GDFPOS,IAVGMI,IAVGDF,
     +            MIALH,IAVMI,IAVPDF,IAVTB4,MIBSIZ,GMIBSZ,GDFBSZ,
     +            TB4BSZ,MISECS,PDSECS,TB4SEC,GMISEC,GDFSEC,
     +            GCFENT,GCSTAT,GDFPNT,GMI5,GMI6,GMI7,GMI8,
     +            CVLAYR,CVENTY,CVSTAT,PDFPNT,TB4PNT,OUTTYP,
     +            MODTOT,DRWTOT,
     +            OUTUN,CVFONT,MIWRD6,
     +            CVMODL,CBLANK,CVISBL,EXTFND,SCLFND,
     +            CVALOW,
     +            CVDBU,CV4SX,CV4SY,CV4SZ,CVEXTS,CVSCLS,
     +            MIDAT,PDFDAT,TB4DAT,GMIDAT,GDFDAT,IDREC,TETEAR
C
C     end of this common block
C
