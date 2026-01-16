C
C     @(#)  412.1 date 6/11/92 propt.inc 
C
C
C     Filename    : propt.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:37
C     Last change : 92/06/11 14:38:51
C
C     Copyright : Practical Technology Limited  
C     File :- propt.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C2    This include file contains the
C2    definitions required for PROPERTIES TABLE
C2    control and processing.
C
      REAL RTOTS(20)
      INTEGER*4 HEDDAT(8),COLDAT(8,20),NCOLS,COLW,FJUST,COLPOS,SIGFIG
      INTEGER*4 NHHLIN,NCHLIN,SORCEC,PAGDAT(8),COMFLD(20),CALFLD(20)
      INTEGER*4 COMACT(20)
      LOGICAL TOTCOL(20)
C
CAPOLLO|SUN
      CHARACTER*80 THEADR(20),COLHED(10,20),TABLNM,TABLIN*255
      CHARACTER*80 COLCON(6,20),ENTPRP(2,100)
CAPOLLO|SUN
CIBM
C      CHARACTER*30 THEADR(20),COLHED(10,20),TABLNM,TABLIN*255
C      CHARACTER*30 COLCON(6,20),ENTPRP(2,100)
CIBM
C
      EQUIVALENCE (COLHED,ENTPRP)
C
      COMMON /PROPT1/HEDDAT,COLDAT,NCOLS,COLW,FJUST,COLPOS,SIGFIG,
     +               NHHLIN,NCHLIN,SORCEC,PAGDAT,COMFLD,CALFLD,COMACT,
     1               TOTCOL,RTOTS
C
      COMMON /PROPT2/THEADR,COLCON,COLHED,TABLNM,TABLIN
C
C2    HEDDAT contains the control data for the table header
C2    COLDAT contains the control data for the columns in the table
C2    COMACT contains the actions to take during compression
C2    TOTCOL contains the column numbers for totalling
C2    NCOLS is the number of columns in the table
C2    COLW is the field width of the current column
C2    FJUST is the field justification of the current column
C2    COLPOS is the output position for the current column
C2    SIGFIG is the number of decimal digits for the current column
C2    NHHLIN is the number of lines in the header
C2    NCHLIN is the number of lines in the column header
C2    SORCEC is the source code for the current data
C2    ENTPRP is a buffer which can be used to form a list
C2           of all properties attached to an entity.This
C2           buffer may not be used during full property
C2           extract procedures,since it is required for
C2           column header data.(Equivalenced with COLHED)
C
