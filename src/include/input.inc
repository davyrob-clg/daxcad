C
C     @(#)  412.1 date 6/11/92 input.inc 
C
C
C     Filename    : input.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:18
C     Last change : 92/06/11 14:32:00
C
C     Copyright : Practical Technology International Limited  
C     File :- input.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C
C     MAXINP .... maximum number of input strings allowed here
C     NNPBUF .... Storage of buffers used for all input lines
C     INPCUR .... Storage of current cursor position
C     INPCTR .... Storage of triangular cursor position
C     INPCPX .... Storage of next text position
C     INPEND .... STorage of current end position
C     INPSPO .... STorage of current start points
C     INPLIM .... Storage of max pixel width of input
C     INPFNT .... Fonts for each device
C     INPCOL .... Color of each device
C     INPBCL .... Background color of each device
C     INPSTR .... Starting position for drawing on each
C                 buffer line. Each time a character
C                 exceeds the limit it increments
C                 and if a character is deleted then
C                 this number is checked and decrement
C                 and redrawn
C     INPTYP ...  Type of frame or enclosure to be draw
C                 round the input text
C
C     INPLAB ...  Prompt for the input device. If specified then this label is used
C                 at the start of the input devuce for a prompt. Thus its coordintaes
C                 do not have to be specified
C     INPFRM      frame size round the text;
C     INPDIA      Input dialog control
C
C
C
      INTEGER*2 INPMAX
      PARAMETER(INPMAX = 5)
C     
      CHARACTER*256 INPBUF(INPMAX)
C
      INTEGER*2 INPCUR(INPMAX)
      INTEGER*2 INPCTR(2,INPMAX)
      INTEGER*2 INPCTX(2,INPMAX)
      INTEGER*2 INPPOS(2,INPMAX)
      INTEGER*2 INPEND(INPMAX)
      INTEGER*2 CINPN
      INTEGER*2 INPFRM(4,INPMAX)
      INTEGER*2 INPLIM(INPMAX)
      INTEGER*2 INPFNT(INPMAX)
      INTEGER*4 INPCOL(INPMAX),INPSTR(INPMAX)
      INTEGER*4 INPTYP(INPMAX)
      INTEGER*4 INPLAB(INPMAX)
      INTEGER*4 INPBCL(INPMAX)
      INTEGER*4 INPDIA(INPMAX)
C
      LOGICAL INPACT(INPMAX)
      LOGICAL INPDIS(INPMAX)
C
      COMMON/INPI2/INPCUR,INPCTR,INPCTX,INPPOS,INPEND,CINPN,INPFNT,
     +             INPLIM,INPFRM
      COMMON/INPI4/INPCOL,INPSTR,INPTYP,INPLAB,INPBCL,INPDIA
      COMMON/INPLG/INPACT,INPDIS
      COMMON/INPCH/INPBUF
