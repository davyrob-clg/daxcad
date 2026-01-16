C
C     @(#)  412.1 date 6/11/92 mast3d.inc 
C
C
C     Filename    : mast3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:23
C     Last change : 92/06/11 14:34:46
C
C     Copyright : Practical Technology Limited  
C     File :- mast3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This common block holds the data storage variables used
C     by the 3D module.
C
C Master Index Section:-
C ======================
C
C     MIFILE is the Master index data array.
C     MIPLIM is the Master Index array Pointer limit.
C     NMIPOS is the next free position in the Master Index array.
C     MIP is the Master Index Pointer Value in Current use by System.
C     IMBUFF is a Data buffer holding Current Master index Information
C
C Part Data Section:-
C ===================
C
C     PDFI is the part data integer array.
C     PDFR is the part data real array.
C     NPDPOS is the next free position in the Part Data array.
C     PDPLIM is the Part Data array pointer limit.
C     IDBUFF is integer scratch array for part data
C     RDBUFF is real scratch array for part data
c
C Text Section:-
C ==================
C
C     NTXPOS is the next free record in the text array.
C
C
C Relational Pointer Section:-
C ==================
C
C     NRLPOS is the next free record in the Relational array.
C
C Display Section:-
C ==================
C
C     DFILE is the integer storage section of the display file.
C     RDFILE is the real storage section of the display file.
C     LDFILE is the display file pointer array.
C     DFPNT is the current Display File position looked at.
C     DFP
C
C Auxilliary Section:-
C ==================
C
C     DELENT  is number of deleted entities in drawing
C     CLAYER  is Current work layer
C     DFPINC
C     VLAYER  is logical to show if layer visible
C     TLAYER
C     LNAME   is name assigned to layer.
C
C  Layer Section :-
C  =================
C
C     DISPV
C     ENSRCH
C

      INTEGER MIFILE(1:11,1:1000),MIPLIM,NMIPOS,MIP,IMBUFF(1:14),
     +          PDFI(1:6,1:1000),PDPLIM,NPDPOS,DFILE(1:6,1:1000,1:2)
     1          ,LDFILE(1:2),DFPNT,DFP,NTXPOS,NRLPOS,
     2           IDBUFF(6,100),BMIP
C        
      INTEGER DELENT,CLAYER,DFPINC,TLAYER(0:255)
C
      REAL PDFR(1:6,1:1000),RDFILE(1:6,1:1000),R3BUFF(6,100)
      LOGICAL DISPV,VLAYER(0:255),ENSRCH(0:127)
C
      CHARACTER*20 LNAME(0:255),CBUFF*80
C
      COMMON /MIP3D/  MIFILE,MIPLIM,NMIPOS,MIP,IMBUFF,BMIP
      COMMON /PDAT3D/ PDFI,PDFR,PDPLIM,NPDPOS,IDBUFF,R3BUFF
      COMMON /DISP3D/ DFILE,RDFILE,LDFILE,DFPNT,DFP
      COMMON /TEXT3D/ NTXPOS
      COMMON /RELN3D/ NRLPOS
      COMMON /AUXI3D/ DELENT,CLAYER,DFPINC,TLAYER,VLAYER
      COMMON /CHAR3D/ LNAME
      COMMON /CHRBUF/ CBUFF
      COMMON /LAYRBF/ DISPV,ENSRCH

C

