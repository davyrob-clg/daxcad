C
C     @(#)  412.1 date 6/11/92 edit.inc 
C
C
C     Filename    : edit.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:10
C     Last change : 92/06/11 14:29:14
C
C     Copyright : Practical Technology International Limited  
C     File :- edit.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C     Functions and subroutines index:-
C
C
C
C
C     TEXTH       Current text height
C     TEXTW       Current text width
C     CPRX        Next X position available for character
C     CPRY        Next Y position available for character
C     BX,BY       Current cursosr positon based on last character
C                 In fact 1 pixel to right of character extents
C     SPOSX       starting postion for text. In fact the cursor will point
C                 directly on this position initial and after deletes and things
C     TRCUR       Triangular cursor is on the screen at this moment
C     STRPOS      starting pos for text displayed
C     STREND      End pos for text displayed
C     EDCOL       Color for editing.
C     EDBACK      Background Color for editing.
C

      INTEGER*2 TEXTH,TEXTW,TXSPACE,TXFID,TXTERM,CPRX,CPRY
      INTEGER*2 SPOSX,SPOSY,CURPOS,TORGX,TORGY,BX,BY
      INTEGER*2 PASTEC,LINEDL,LEFEXT,RIGEXT
      INTEGER*2 CURSW,CURSH,DELCHR,KEYL,KEYR,ENDPOS,MAXCHR,EATCHR
      INTEGER*2 DEL
      INTEGER*2 ELIMIT,STRPOS,STREND
      INTEGER*2 UPCHR,DNCHR
C
      CHARACTER BUFFER*257,MVS*257
C
      INTEGER*4 EDCOL
      INTEGER*4 EDBACK
      LOGICAL HIGH,LOW,TRCUR

      COMMON/EDITI2/TEXTH,TEXTW,TXSPACE,TXFID,
     +              CPRX,CPRY,SPOSX,SPOSY,CURPOS,
     +              TORGX,TORGY,BX,BY,ENDPOS,
     +              ELIMIT,STRPOS,STREND,EDCOL,EDBACK
C
      COMMON/EDITLG/TRCUR
C
      COMMON/EDITC/BUFFER,MVS
C
C     key defintions
      PARAMETER (CURSW=8,
     +           CURSH=5,
     +           KEYL=138,
     +           KEYR=140,
     +           DELCHR=149,
     +           DEL=127,
     +           EATCHR=131,
     +           TXTERM=150,
     +           PASTEC=233,
     +           LEFEXT=132,
     +           RIGEXT=134,
     +           LINEDL=130,
     +           UPCHR = 136,
     +           DNCHR = 142)
C     events and other paramters
      PARAMETER (LOW=.FALSE.,
     +           HIGH=.TRUE.,
     +           MAXCHR=255)
