C
C     @(#)  412.1 date 6/11/92 movd3d.inc 
C
C
C     Filename    : movd3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:26
C     Last change : 92/06/11 14:36:04
C
C     Copyright : Practical Technology Limited  
C     File :- movd3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This common block contains the data required
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
C2    The menu cells currently hilited are also
C2    stored in the array MNCELL in the following
C2    manner-----MNCELL(1,1),(1,2)=scaling menu,cell
C2               MNCELL(2,1),(2,2)=rotation menu,cell
C2               MNCELL(3,1),(3,2)=mirroring menu,cell
C2               MNCELL(4,1),(4,2)=translation menu,cell
C2               MNCELL(5,1)=target layer number
C2               MNCELL(5,2)=Current contruction layer
C
C2    All reference data required for the geometric
C2    transformations is held in array REFDAT
C2         REFDAT(1,1-6)=scale origin,scale factor
C2         REFDAT(2,1-6)=rotate origin,rotate angle
C2         REFDAT(3,1-6)=mirror line origin,angle
C2         REFDAT(4,1-6) =translate deltax,deltay,deltaz
C2
C2        REFDAT(9,1),(9,2)=xmin.ymin of window around entities
C2        REFDAT(10,1),(10,2)=xmax,ymax of window around entities
C2       
C2          CURSCL current overall scaling factor,usually
C2                 calculated by concatenation of transforms
C2                 during multiple copy transformations.
C
      REAL REFDAT(10,6),CURSCL
      INTEGER*4 MNCELL(10,2)
      LOGICAL OPFLAG(10)
C
      COMMON /MOVDAT/REFDAT,CURSCL,MNCELL,OPFLAG
C
