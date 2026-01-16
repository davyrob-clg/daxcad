C
C     @(#)  412.1 date 6/11/92 gtxt1.inc 
C
C
C     Filename    : gtxt1.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:15
C     Last change : 92/06/11 14:31:21
C
C     Copyright : Practical Technology Limited  
C     File :- gtxt1.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This common block contains all the necessary
C     variables for execution of the GTXT library
C     of text handlers for text I/O on the Apollo
C     graphics screen.
C
      INTEGER*4 GWBP(1:4)
C
C       GWBP contains the pointer to the current line
C       in the text buffer associated with the window being referenced.
C
      INTEGER*2 GWL(1:4,1:6),GWC(1:4,1:5)
C
C       GWL(GTWID,1)=upper left x-coord of window
C       GWL(GTWID,2)=upper left y-coord of window
C       GWL(GTWID,3)=extent of window in x
C       GWL(GTWID,4)=extent of window in y
C       GWL(GTWID,5)=line length in characters
C       GWL(GTWID,6)=page length in lines of text
C
C       GWC(1,1)=X-coord of current cursor on input line
C       GWC(1,2)=Y-coord of current cursor on input line
C       GWC(1,3)=X-coord of cursor origin on input line
C       GWC(1,4)=Y-coord of cursor origin on input line
C       GWC(1,5)=Character position in input buffer
C
      LOGICAL GWA(1:4,1:2)
C
C       GWA is a logical array,which indicates the
C       condition of a window. .TRUE. indicates that the window
C       in question has been initialized, and is available for use.
C
      COMMON /GD1/GWBP,GWA,GWL,GWC
C
C
CAPOLLO|SUN
      CHARACTER*80 GWB(1:4,0:31),GWLINE(1:4),GNLINE
CAPOLLO|SUN
CIBM
C      CHARACTER*80 GWB(1:2,0:4),GWLINE(1:4),GNLINE
CIBM
C
C       GWB is a text buffer area for storage of text data
C       for display in the associated window. The data is normally displayed
C       in order starting from the current line as defined by the value
C       in GWBP,with a page length defined in the array
C       GWL(GTWID,6).
C
C       GWLINE (GTWID) contains the current input line 
C       associated with the referenced window
C
C       GNL is an empty line of length 80 characters
C       used to guarantee all postions in a text string are set to
C       nulls,since APOLLO FORTRAN does not clear character variables
C       properly.
C
      COMMON /GD2/GWB,GWLINE,GNLINE
C

     
C
