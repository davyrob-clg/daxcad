C
C     @(#)  412.1 date 6/11/92 cursor.inc 
C
C
C     Filename    : cursor.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:02
C     Last change : 92/06/11 14:26:01
C
C     Copyright : Practical Technology International Limited  
C     File :- cursor.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C     Source file for cursor patterns
C
C
C     CURSOR_XHAIR   Cross hairs pattern. Cursor goes across screen fully
C     CURSOR_CROSS   Simple cross pattern. length can be defined
C     CURSOR_X       an X pattern of 45 degrees from the cross
C     CURSOR_CHAR    A character from an icon or character file of up to
C                    100x100 pixels
C
      INTEGER*4 CURSOR_XHAIR
      INTEGER*4 CURSOR_CROSS
      INTEGER*4 CURSOR_SCROSS
      INTEGER*4 CURSOR_X
      INTEGER*4 CURSOR_CHAR   
C
      PARAMETER(CURSOR_XHAIR = 1,  
     +          CURSOR_CROSS = 2, 
     +          CURSOR_X     = 3, 
     +          CURSOR_CHAR  = 4,
     +          CURSOR_SCROSS= 5) 
C
C
C
      INTEGER*4 MAXCUR
      PARAMETER(MAXCUR=10)
C
C     CURSXY ... Current curosir postion when last drawn
C     CURSLN ... X Y lenght of the cursor or frame in which cursor is used
C                only for XHAIR currently
C     CURSBT ... Bitmap descriper for user designed cursors
C     CUSRAT ... Atrribute block for the aforesaid bitmap
C     CURSTY ... Current cursor type
C     CURSCO ... Cursor color
C     CURSBD ... The bitmap is used for all user defined cursors
C     CURSFN ... Font file idetifier for the user defined cursor
C     CURSEV ... If set then the cursor will be active during locator movements
C                this also defines the system cursor
C     CURSNO ... Cursor type defined
C     CURSON ... The cursor is swiched on or off
C     INFRAM ... Currently inside a frame 
C     CUROLD ... The value of the cursor identifier before we wnt into a frame
C     DAXCURS... The type of DAXCAD viewport cursor. Either 
C                a cross hair or a simple cross Subscript 1
C                is cursor number. 2 a,d 3 X and Y sizes
C
C     DAXTRACK   Tracking of DAXCAD viewport cursor is allowed
C
      INTEGER*2 CURSXY(2)
      INTEGER*2 CURSFN(MAXCUR)
      INTEGER*2 CURSLN(4,MAXCUR)
      INTEGER*4 CURSBT
      INTEGER*4 CURSTY
      INTEGER*4 CURSCO(MAXCUR)
      LOGICAL   CURSBD
      LOGICAL   CURSDF(MAXCUR)
      LOGICAL   CURSEV(0:MAXCUR)
      INTEGER*4 CURSNO(MAXCUR)
      LOGICAL   CURSON
      LOGICAL   INFRAM
      INTEGER*2 CUROLD
      INTEGER*4 CURBIT,CURATT
      INTEGER*4 DAXCURS(3)
      LOGICAL   DAXTRACK
C
      CHARACTER CURSCH(MAXCUR)
C
      COMMON/CURSI2/CURSXY,CURSLN,CURSFN,CUROLD
      COMMON/CURSI4/CURSBT,CURSTY,CURSCO,CURSNO,DAXCURS
      COMMON/CURSCH/CURSCH
      COMMON/CURSLG/CURSBD,CURSDF,CURSEV,CURSON,INFRAM,DAXTRACK
      COMMON/CPSYS/CURBIT,CURATT
C                   
