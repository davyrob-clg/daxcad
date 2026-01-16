C
C     @(#)  412.1 date 6/11/92 frames.inc 
C
C
C     Filename    : frames.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:13
C     Last change : 92/06/11 14:30:17
C
C     Copyright : Practical Technology International Limited  
C     File :- frames.inc
C
C     DAXCAD FORTRAN 77 Include file
C

C     Include file for frames please
C
C     MAXFRM ... Maximum number of frames 
C     FRMBOX ... Frame dimensions
C     FRMTYP ... Type to be drawn. 1st element OUTLINE
C                or FILLED. 2nd element INVERTED on
C                source
C     FRMACT ... Frame is active 
C     FRMDIS ... Frame is currently displayed
C     FRMTHK ... Thicknes of frame
C     FRMCUR ... Cursor depoendancy on a particular frame. If this is a valid cursor
C                indentifier then the notifier will automaticaly set the cursor in 
C                that frame
C     FRMDIA ... Dialog control for frames


      INTEGER*4 MAXFRM
      PARAMETER (MAXFRM = 10)
C
      INTEGER*2 FRMCUR(MAXFRM)
      INTEGER*2 FRMBOX(4,MAXFRM)
      LOGICAL FRMACT(MAXFRM),FRMDIS(MAXFRM)
      INTEGER*4 FRMCOL(2,MAXFRM),FRMTHK(MAXFRM)
      INTEGER*4 FRMDIA(MAXFRM)
      CHARACTER*256 FRMTXT(MAXFRM)
      COMMON/FRMI4/FRMCOL,FRMTHK,FRMDIA
      COMMON/FRMI2/FRMBOX,FRMCUR
      COMMON/FRMLG/FRMACT,FRMDIS
      COMMON/FRMTX/FRMTXT

