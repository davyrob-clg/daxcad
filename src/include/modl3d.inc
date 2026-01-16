C
C     @(#)  412.1 date 6/11/92 modl3d.inc 
C
C
C     Filename    : modl3d.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:25
C     Last change : 92/06/11 14:35:47
C
C     Copyright : Practical Technology Limited  
C     File :- modl3d.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C Model Definition data:-
C ======================
C 
C     NVERTS  is no. of Vertices in Model.
C     VERTIC  is definition of vertice record
C     EXTREC  is definition of world extents record
C     BODREC  is definition of body centre and no. faces record
C     ORGREC  is definition of world origin & length record
C     MODMIP  is Master Index pointer for model data in buffer.
C     MODRCS  is number of records model occupies in array.    
C     NFACES  is number of faces on Model.
C     NSEGS   is the number of segments on model.
C
      INTEGER NVERTS,VERTIC,EXTREC,BODREC,ORGREC,MODMIP,
     +        MODRCS,NFACES,NSEGS
C        
C
      REAL BODYCR(3),MINPTS(3),MAXPTS(3)
C
      COMMON /MODINF/ NVERTS,VERTIC,EXTREC,BODREC,ORGREC,MODMIP,
     +                MODRCS,NFACES,NSEGS,BODYCR,MINPTS,MAXPTS
C
