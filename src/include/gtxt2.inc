C
C     @(#)  412.1 date 6/11/92 gtxt2.inc 
C
C
C     Filename    : gtxt2.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:16
C     Last change : 92/06/11 14:31:23
C
C     Copyright : Practical Technology Limited  
C     File :- gtxt2.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C     This common block contains all the necessary
C     variables for execution of the GTXT library
C     of menu handlers for text I/O on the Apollo
C     graphics screen.
C
C
      INTEGER*2 GML(1:4,1:6),GMC(1:4,1:6)
C
C       GML(GTWID,1)=upper left x-coord of window
C       GML(GTWID,2)=upper left y-coord of window
C       GML(GTWID,3)=extent of window in x
C       GML(GTWID,4)=extent of window in y
C       GML(GTWID,5)=line length in characters
C       GML(GTWID,6)=page length in lines of text
C
C       GMC(GTWID,1)=X-coord of first menu cell
C       GMC(GTWID,2)=Y-coord of first menu cell
C       GMC(GTWID,3)=extent of menu cell in X
C       GMC(GTWID,4)=extent of menu cell in Y
C       GMC(GTWID,5)=pitch in Y between menu cells
C       GMC(GTWID,6)=pitch in X between menu cells
C
      INTEGER*2 GTCBMS(4,2)
C
C       GTCBMS(GTWID,1)=extents in x of cell bitmap
C       GTCBMS(GTWID,2)=extents in y of cell bitmap
C
      INTEGER*4 GTCADS(4),GTCMDS(4)
C
C       GTCADS(GTWID)=attribute identifier for cell bitmap
C       GTCMDS(GTWID)=bitmap identifier for cell bitmap
C
      LOGICAL GMA(1:4),GMCINV(1:4,0:31),INVSAV(1:4,0:31),
     +        GMCMUL(1:4,0:31), MULSAV(1:4,0:31),GTMULT
C
      INTEGER*4 GMCVNM(1:4, 0:31)      
C
C       GMA is a logical array,which indicates the
C       condition of a window. .TRUE. indicates that the window
C       in question has been initialized, and is available for use.
C
C       GMCINV is a logical array, which indicates the display status
C       of each of the menu cells. A true value indicates that the menu cell
C       in question is currently shown in inverse video.
C       
C       INVSAV saves the state of GMCINV to allow the insertion of a temporary
C       menu
C       
C
C       GMCMUL is a logical array which indicates if the currently loaded
C       cell is a multiple text cell.
C
C       MULSAV saves the state of GMCMUL to allow the insertion of a temporary
C       menu
C                                                    
C       GMCVNM is an array which contains the verb number for each of the menu
C              cells
C
      COMMON /GM1/GTCADS,GTCMDS,
     +       GMA,GMCINV,GMCMUL,GTMULT,GML,GMC,GTCBMS,GMCVNM
      COMMON /GM1SAV/ INVSAV,MULSAV
C
C
CAPOLLO|SUN
      CHARACTER*80 GMB(1:4,0:31), GMBSAV(1:4,0:31)
CAPOLLO|SUN
CIBM
C      CHARACTER*17 GMB(1:4,0:31), GMBSAV(1:4,0:31)
CIBM
      CHARACTER*1 GMBC (1:4,0:31), GMBCSV(1:4,0:31)
C
C       GMB is a text buffer area for storage of text data
C       for display in the associated window. The data is normally displayed
C       in order starting from the current line as defined by the value
C       in GMB_PNT,with a page length defined in the array
C       GML(GTWID,6).
C
C       GMBSAV saves the state of GMB to allow the insertion of a temporary
C       menu
C
C       GMBC is a text buffer for storage of the single key
C       command code to be passed back to the main program as a result
C       of a hit in the menu area.C
C
C       GMBCSV saves the state of GMBC to allow the insertion of a temporary
C       menu
C
      COMMON /GM2/GMB,GMBC
      COMMON /GM2SAV/ GMBSAV, GMBCSV
C
C
