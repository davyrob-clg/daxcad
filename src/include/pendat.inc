C
C     @(#)  412.1 date 6/11/92 pendat.inc 
C
C
C     Filename    : pendat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:32
C     Last change : 92/06/11 14:37:43
C
C     Copyright : Practical Technology Limited  
C     File :- pendat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This holds the variables used to produce plotter
C     output data for a range of plotters.
C
C     PLTUNT=The unit number of the Plot File opened.
C
C     STRID =The stream ID being used.
C
C     PLOTD =Logical to indicate whether plotting direct
C            to the plotter or to a temporary file.
C     LAYERN(0:255)= Record of Pen no. used for each layer
C     PLOMOD= The plotter model as a number i.e
C                     1 = HP7475
C                     2 = HP7585B
C     SIZE = Paper size in A format . All paper sizes offset
C             by 1 i.e A0 is 1 , A4 is 5
C     MAXPEN = Max no of pens for that model
C
C     PENNOS(8,2)= The unit numbers of the temp files used to
C                  segragate the data by Pen numbers,and the
C                  no of records in that file. 
C     PENN(8) = Logical showing whether a pen is being used
C                i.e True indicates being used , False not used
C     PLTFIL = Name of the Output Plot File
C
C     HARD indicates wether the plotter is generating the text or
C          the software
C     TEXANG holds the temporary direction of text which
C     may be different if a rotated plot is being done.
C
C     PLTNAM contains a list of directories for each plotter
C     in the list each plotter has its own plot directory wihch
C     can be the same or different from the rest in the list.
C
C     PLTDIR store the directory for the plot file to be open in 
C     if the plot file is to be plotted .
C
C     MODLST  contains a list of model number which can be used
C     with this version of the program.
C
C     TYPE stores in a list the plotter types name which can 
C     be used.
C
C     PLNAME is the currently selected plotter name.
C
C     CURPLT point into the MODLST array to the currently
C     selected  plotter 
C
C     GRBSCL is the plotter units scale for GERBER plotters
C     set to 1.0 for normal 1 Thou resolution plotting.
C     set to 10.0 for hi-res (1/10th Thou) resolution plotting.
C
C     ARCTOL is the tolerance for use in ARC interpolation
C     ARCTOL is expressed in plotter units.
C     set to default of 0.5, can be changed by user.
C     
C     MODELS contains the maximum number of different plotters
C     which can be used.
C
C     PENSD contains data about each pen 
C     PENSD(?,1) velocity of pen ?
C     PENSD(?,2) acceleration of pen ?
C     PENSD(?,3) force of pen ?
C
C     VPORGX The X offset of the viewport (for plot viewport)
C     VPORGY The Y offset of the viewport (for plot viewport)
C     PLTVPT Indictaes that the viewport plotting is active
C
C     PLOSCL : plot scale as a string
C     PLTSCL : plot scale
C     PLTCOP : number of copies

      INTEGER*4 LAYERN(0:255),PLOMOD,SIZE,PENNOS(10,2)
     +            ,PLTUNT,PLTNO,MAXPEN,MINPEN,MODELS,CURPEN
      INTEGER*4 MODLST(10),PENSD(10,3),CURPLT
      INTEGER*2 STRID,CLEN
      REAL PAPLIM(5,4),PLBOR(4)
      REAL XP1,YP1,XP2,YP2,AXP,AYP,SCA,LIM,
     +      ROTARY(3,3),FACT,PAPSCL,TEXANG,GRBSCL,ARCTOL
C
      LOGICAL PENN(10),PLOWIN,ROTATE,HARD,PLOTD,PENSPC,PLTVPT,CLIPIT
C
      CHARACTER*40 TYPE(10)*15,PLTSHT*3,
     +             PLTNAM(10)*80,PLTDIR*100,PLNAME*15
      CHARACTER*80 PLTFIL

      COMMON /PLTDAT/ PLTUNT,LAYERN,PLOMOD,SIZE,ROTATE,PENSPC,CURPEN,
     +                MAXPEN,MINPEN,LIM,MODELS,ROTARY,MODLST,PENSD,
     1                PENNOS,XP1,YP1,XP2,YP2,SCA,AXP,AYP,CURPLT,
     2                PENN,PLOWIN,PLTNO,FACT,PAPSCL,GRBSCL,ARCTOL,
     3                PAPLIM,PLBOR,HARD,PLOTD,TEXANG,
     4                PLTVPT,CLIPIT,STRID,CLEN

      COMMON/PLTC/TYPE,PLTFIL,PLTSHT,PLTNAM,PLTDIR,PLNAME

      REAL PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1   PLXMIN,PLXMAX,PLYMIN,PLYMAX,
     2   PVXMIN,PVXMAX,PVYMIN,PVYMAX,PLWVXY(3,3),PLVWXY(3,3)
C
      COMMON /WTOP/PWXMIN,PWXMAX,PWYMIN,PWYMAX,
     1       PLXMIN,PLXMAX,PLYMIN,PLYMAX,
     2       PVXMIN,PVXMAX,PVYMIN,PVYMAX,PLWVXY,PLVWXY

      REAL VPORGX,VPORGY
      COMMON /PLTVP/ VPORGX,VPORGY

      REAL PLTSCL
      CHARACTER*40 PLOSCL
      COMMON /PLTSCA/ PLTSCL,PLOSCL

C     stuff for advent's postscript output, last text angle and size
      REAL  LPTX3,LPTX4,LPTSLT
      COMMON /PTXTA/LPTX3,LPTX4,LPTSLT

      INTEGER*4 PLTCOP
      COMMON /PLTCP/ PLTCOP
