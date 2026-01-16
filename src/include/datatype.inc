C
C     @(#)  412.1 date 6/11/92 datatype.inc 
C
C
C     Filename    : datatype.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:03
C     Last change : 92/06/11 14:26:49
C
C     Copyright : Practical Technology Limited  
C     File :- datatype.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      CHARACTER*255 CHR
      REAL LIM,TIME,ALIM,SCAX,SCAY
      INTEGER*4 XO,YO,AX,AY,LIMX,LIMY,ORGX,ORGY
      INTEGER*4 BUFLEN,CURPOS,ABSOL,IDNUM,OUTPOS
      INTEGER*4 NUL,SOH,STX,ETX,EOT,ENQ,ACK,BEL,BS,
     +          HT,LF,VT,FF,CR,SO,SI,DLE,DC1,DC2,DC3,
     1          DC4,NAK,SYN,ETB,CAN,EM,SUB,ESC,FS,GS,RS,US
      INTEGER*2 DIGDEV,STRIDO,STRIDI,FILID,CHKB,NBYTES,PNTDEV

      COMMON /TYPE2/IDNUM,LIM,TIME,ALIM,ABSOL,
     +              XO,YO,AX,AY,LIMX,LIMY,SCAX,SCAY,ORGX,ORGY,
     1              STRIDI,STRIDO,FILID,DIGDEV,PNTDEV,NBYTES,CHKB  

      COMMON /CONTRL/NUL,SOH,STX,ETX,EOT,ENQ,ACK,BEL,BS,
     +                HT,LF,VT,FF,CR,SO,SI,DLE,DC1,DC2,DC3,
     1                DC4,NAK,SYN,ETB,CAN,EM,SUB,ESC,FS,GS,RS,US
      COMMON /LINE1/CHR
      COMMON /LINE2/BUFLEN,CURPOS,OUTPOS

