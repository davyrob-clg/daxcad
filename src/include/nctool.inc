C
C     @(#)  412.1 date 6/11/92 nctool.inc 
C
C
C     Filename    : nctool.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:30
C     Last change : 92/06/11 14:36:29
C
C     Copyright : Practical Technology Limited  
C     File :- nctool.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      REAL MTIME,PTIME,TTIME,HITIME,MOVXY,RAPIDF,PARTT,TCHANG
     +    ,TSETUP,PAPSET,HITRAT,MATLOD,MATUNL,NIBINC,LNBINC
      REAL TURRET(1:20,1:4)
      INTEGER TPOSN,TURLIM
      LOGICAL CCW,OK
      CHARACTER*80 TLDESC
C2        ARRAY TURRET contains all tool and die sizes
C2        1=TLDIAM(MM) 2=TLDIAM(IN) 3=DIEDIAM(IN) 4=DIEDIAM(MM)
C2        TPOSN  -  Station position of current tool
C2        TLDESC -     Description   of current tool
C2        TURLIM - Max no. of tools in turret.
      COMMON /NCD5/MTIME,PTIME,TTIME,MOVXY,RAPIDF,PARTT,TCHANG,CCW,
     +     TURRET,HITIME,TSETUP,PAPSET,HITRAT,MATLOD,MATUNL,
     1     NIBINC,LNBINC,TPOSN,TURLIM
C
      COMMON /NCD10/TLDESC
C
