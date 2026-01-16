C
C     @(#)  412.1 date 6/11/92 krap.inc 
C
C
C     Filename    : krap.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:20
C     Last change : 92/06/11 14:33:12
C
C     Copyright : Practical Technology Limited  
C     File :- krap.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
      INTEGER*4 PASUNT
      INTEGER*4 NODEID
      INTEGER*4 LICEN2,NNODES,DXXUNT,USELEV,UNODE,HNODE
      LOGICAL   KDEMO
C
      COMMON /KRAP/NODEID,PASUNT,LICEN2,
     +             NNODES,DXXUNT,USELEV,UNODE,HNODE
C
      COMMON /KRAPL/KDEMO

C
C2    Common block KRAP contains the node protection data
C2    and control information
C2
C2    PASUNT contains the unit number of the password file
C2    LICPNT contains the pointer to the LICENCE number record
C2    CCODE  contains the default country code
C2    LICENC contains the LICENCE number as defined within the software package
C2    LICEN2 contains the licence number as read from the password file
C2    NNODES contains the number of authorized nodes
C2    CURNOD contains the index number of the current node record
C2    DXXUNT contains the unit number of the DAXCAD executable image
C2    USELEV contains the user access level given by the licence
C2    UNODE  contains the number of the user node
C2    HNODE  contains the number of the node hosting the user
C
C2    Common block KRAPL contains the following info
C2
C2    KDEMO  TRUE if DEMO version

