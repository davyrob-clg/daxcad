C
C     @(#)  412.1 date 6/11/92 dat3d.inc 
C
C
C     Filename    : dat3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:02
C     Last change : 92/06/11 14:26:16
C
C     Copyright : Practical Technology Limited  
C     File :- dat3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C1    A data storage block for use in
C2    3d viewing applications.Only supports
C2    simple facetted structures at present
C
      INTEGER MAXVRT,MAXENT
      PARAMETER (MAXVRT=1000,MAXENT=100)
      INTEGER MIDAT(3,MAXENT),MIP,VRP,VRBP,CURENT
      REAL VERTS(3,MAXVRT),VRTBUF(3,100)
      COMMON /MODEL1/MIDAT,VERTS,VRTBUF,MIP,VRP,VRBP,
     +               CURENT
C
C2    MAXVRT is the max no vertices allowed
C2    MAXENT is the max no ents allowed
C2    MIDAT contains the MI data
C2    VERTS contains model verex data
C2    VRTBUF is a buffer for avtive vertex processing
C2    MIP is current entity pointer
C2    VRP is current vertex pointer
C2    VRBP is current vertex buffer pointer
C2    CURENT is the current entity type in buffer
