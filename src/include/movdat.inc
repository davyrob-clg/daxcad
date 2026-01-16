C     This common block contains the data required
C     @(#)  412.1 date 6/11/92 movdat.inc 
C
C
C     Filename    : movdat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:27
C     Last change : 92/06/11 14:36:07
C
C     Copyright : Practical Technology Limited  
C     File :- movdat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     during a move operation.
C
C2    meanings-  OPFLAG(1)=scaling enabled
C2               OPFLAG(2)=rotation enabled
C2               OPFLAG(3)=mirroring enabled
C2               OPFLAG(4)=translation enabled
C2               OPFLAG(5)=target layer enabled
C2               OPFLAG(6)=scale text in proportion to geometry
C2               OPFLAG(7)=scale paper related data (text arrows etc)
C2               OPFLAG(8)=add entity to display file
C2               OPFLAG(9)=erase entities from old position
C2               OPFLAG(10)=draw entities in new position
C2               OPFLAG(11)=used for stretch.
C2               OPFLAG(12)=currently changing master data
C2               ENDCOD is used to indicate which end of a line
C2               to stretch
C2    The menu cells currently hilited are also
C2    stored in the array MNCELL in the following
C2    manner-----MNCELL(1,1),(1,2)=scaling menu,cell
C2               MNCELL(2,1),(2,2)=rotation menu,cell
C2               MNCELL(3,1),(3,2)=mirroring menu,cell
C2               MNCELL(4,1),(4,2)=translation menu,cell
C2               MNCELL(5,1)=target layer number
C2               MNCELL(5,2)=Current contruction layer
C2               MNCELL(6,1),(6,2)=array menu, cell
C
C2    All reference data required for the geometric
C2    transformations is held in array REFDAT
C2         REFDAT(1,1),(1,2),(1,3)=scale origin,scale factor
C2         REFDAT(2,1),(2,2),(2,3)=rotate origin,rotate angle
C2         REFDAT(3,1),(3,2),(3,3)=mirror line origin,angle
C2         REFDAT(4,1),(4,2)      =translate deltax,deltay
C2
C2        REFDAT(9,1),(9,2)=xmin.ymin of window around entities
C2        REFDAT(10,1),(10,2)=xmax,ymax of window around entities
C2
C2          CURSCL current overall scaling factor,usually
C2                 calculated by concatenation of transforms
C2                 during multiple copy transformations.
C2          ENDCOD defines the end the entity to modify
C2                 in a STRETCH operation, 1=first point
C2                 2=second point of line
C
C
C
C
C2  The following data controls the  ARRAY option
C2  in  MOVE and is contained in the COMMON block ARRAY
C2
C2
C2     RADIAL = TRUE if  array is circular, FALSE for 
C2     rectangle 
C2     BORDER = TRUE if array is a rectangular BORDER
C2     ROTAT  = TRUE if elements in circular are rotated
C2     ARRTYP = TRUE if array type controls have been set
C2     ARRAD  = radius of a radial array
C2     ARRPOS = position of the ARRAY, centre for 
C2     circular array or corner element of a rectangle
C2     ARRXD  = distance between columns
C2     ARRYD  = distance between rows
C2     ARRNO  = No of elements in a circular array
C2     ROWS   = No of ROWS in a rect ARRAY 
C2     COLS   = No of COLS in a rect ARRAY
C2     ARRON  = set TRUE if an array is to be made
C2     ELECEN(1) = x position of instanced element
C2     ELECEN(2) = y position of instanced element 
C2     MEMBER = TRUE if the chosen elements are to
C2              part of the array
C2     STANG  = start angle for a circular array in radians
C2
      REAL REFDAT(10,3),CURSCL,ARRPOS(2),ARRXD,ARRYD,
     +ARRAD, ELECEN(2), STANG 
C      
      INTEGER*4 MNCELL(10,2), ARRNO, ROWS, COLS
C
      INTEGER*2 ENDCOD
C
      LOGICAL OPFLAG(12), RADIAL, ROTAT, ARRTYP,
     +ARRON, BORDER, MEMBER
C
      COMMON /MOVDAT/REFDAT,CURSCL,OPFLAG,MNCELL,ENDCOD
C
      COMMON /ARRAY/RADIAL,ROTAT,ARRTYP,ARRPOS,ARRXD,
     +ARRYD,ARRNO,ARRON,ROWS,COLS,ARRAD,ELECEN,
     1BORDER,MEMBER,STANG


