C
C     @(#)  412.1 date 6/11/92 marker.inc 
C
C
C     Filename    : marker.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:23
C     Last change : 92/06/11 14:34:41
C
C     Copyright : Practical Technology Limited  
C     File :- marker.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This common block contains all the information neaded
C     to support the marker definition table and index system
C     the real array MRKR contains the real infirmation associated
C     with the marker and MRKI the integer data.  The following
C     attempts to describe the structure of the marker definition :-
C     
C     MRKR             MRKI          AUXDAT                   TXTPNT                TXTDAT
C     ====             ====          ======                   ======                ======
C   XMIN  YMIN    DEFINITION CNT <---DEFINITION  <---DEFINITION     AUX RETURN      MARKER NAME
C   XMAX  YMAX    AUX POINTER --->  RETURN PTR       RETURN PTR      POINTER
C    -     -      SEGMENT CNT(N)     TEXT PTR
C   XPNT1  YPNT1  OPERATION DESC  
C   XPNT2  YPNT2  OPERATION DESC  
C   XPNT3  YPNT3  OPERATION DESC  
C    .........................
C    .........................
C   XPNTN  YPNTN  OPERATION DESC
C THICKNESS STYLE DESC
C   COLOUR   -        
C   XPNT1  YPNT1  OPERATION DESC  
C   XPNT2  YPNT2  OPERATION DESC  
C   XPNT3  YPNT3  OPERATION DESC  
C    .........................
C    .........................
C   XPNTN  YPNTN  OPERATION DESC
C                                   <---------- START + DEFINITION CNT           
C
C
C
C     where DESC :-
C                   0 = pen up vector move
C                   1 = pen down vector move
C                   5 = arc description follows (centrex, centrey,5)
C                                               (radius )
C                                               (start ang. end ang.,segments)
c                  99 = parameter block         (Thickness, style,99)
C                                               (colour)
C
C
C     The counter limits  are as follows:-
c      MRKRCT    current real array limit
c      MRKICT    current index array limit
c      AUXDCT    current aux data limit
c      TXTCNT    current text data limit
c
C     The current pointers are as follows:-
c      MRKRCR    current real array pointer
c      MRKICR    current index array pointer
c      AUXDCR    current aux data pointer
c      TXTCNR    current text data pointer
c      MRKNUM    current marker number
C
C     
      INTEGER*2 MRKRMX,MRKIMX,AUXDMX,TXTCMX
      PARAMETER (MRKRMX=600,MRKIMX=50,AUXDMX=200,TXTCMX=50)
C
C
      INTEGER*2 MRKI(0:600),MRKIND(0:50),AUXDAT(0:200),TXTPNT(0:50,2)
      INTEGER*2 MRKRCT,MRKICT,AUXDCT,TXTCNT
      INTEGER*2 MRKRCR,MRKICR,AUXDCR,TXTCNR,MRKNUM

      REAL MRKR(0:600,2)
      CHARACTER*16 TXTDAT(0:50)
C
      COMMON /MRKDTI/MRKI,MRKIND,AUXDAT,TXTPNT,
     +           MRKRCT,MRKICT,AUXDCT,TXTCNT,
     +           MRKRCR,MRKICR,AUXDCR,TXTCNR,MRKNUM
                 
      COMMON /MRKDTR/MRKR
      COMMON /MKRDTT/TXTDAT
      
