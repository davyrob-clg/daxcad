C
C     @(#)  412.1 date 6/11/92 viewport.inc 
C
C
C     Filename    : viewport.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:53
C     Last change : 92/06/11 14:43:02
C
C     Copyright : Practical Technology Limited  
C     File :- viewport.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C                     VARIABLE DESCRIPTION
C                     ====================
C     
C         MAWSMX      Maximum number of views. not common parameter only
C         MVPACT      The viewport system is active. At least 
C                     one view has been defined.
C         VPNUM(MAXVP)    The array of logicals defining which viewports
C                     are actually defined.
C
C         VPVIS(MAXVP)    Array of logicals which define the visiblity
C                     of an entity after it has been drawin in each
C                     viewport. 
C         VPADD       First of three flags which determine how an
C                     entity is to be drawn into ech viewport.
C                     VPADD specifies that the entity must be drawn
C                     into each view and the display files updated.
C         VPMOV       Specifies that the entity must be drawn but 
C                     display files are not to be altered. This is
C                     slightly inefficient as any entities which are
C                     drawn out of view will still be in the display 
C                     file.
C         VPDEL       The entity must be drawn and removed from the 
C                     display file. This is done by modifing the entry
C                     in the file as negative.
C         VPDIS       This indicates to routine MNIDIS that the 
C                     viewport memu is loaded. If the layer menu is
C                     selected then when toggled back.
C         DRG2VP      This flag tell ZPVIEW that he must recover the 
C                     the bitmap that was saved when the FIRST view
C                     was selected. This gives the user 5 1/2 veiwports
C         VIEWPS(4,6) This array holds the world limits of each view 
C                     that has been defined. When the view is popped
C                     the world limits are taken from this. The first
C                     limits are the main display coords, ie the 
C                     actual world at any time before a view pop is 
C                     requested.
C         VPBITM(5)   The viewport bitmap identifiers.
C         
C         MAINBM      The bitmap for the main view which is saved when
C                     view is popped.
C         CVPN        The current viewport number. Range from 1-5.
C                     if main display the CVPN must be 0
C         PVPN        The previous viewport number. 
C
C         VPLIM       Used to store the main display display file
C                     limit. The viewport display file is then 
C                     mapped into DAXCAD and treated as the mein file.
C                     if any entities are added or canceled then this
C                     must be altered. It is saved and recovered in
C                     routine SET_DISPLAY_FILE.
C         VPGRID      The grid status used for zoom previous. Normally grid
C                     is turned off when a view is entered. The current 
C                     grid status of the main drawing is stored in this variable.
C
C         MAWS        Are MAWS or DAXPORTS active ?
C
C         VIEWEX      Extents of each view that is being used
C
C         VIEWPO      origin of each of the viewports
C         OBSLST      obscured list
C
C         VPSEGF      The segement manager is now to be used with offset
C                     This means that the line will be updated in other viewports
C
C         VPSEGN      The number of the viewport to clip into
C         VPOX        Origin offset for segment
C         VPOY
C         VPDFIL      Display operations are being done in a viewport
C         VPXMAX      }
C         VPYMAX      } The current viewport limits
C         VPXMIN      }
C         VPYMIN      }
C         VPBANN      The height in pixels of the banner. The font is vt100s
C 
C         GRDMNU      If TRUE getgrd can update the setgrd menu to keep us in
C                     sync
C
C         DDCODE      For drawing display code. used in DRAWLS to determine the 
C                     drawing code. They are as follows
C                     0 ... Normal drawing op into to current bitmap (DEFAULT)
C                     1 ... Draw into viewport and into its displayed offset
C                     2 ... Draw into viewport and clip all other viewports bu
C                           do not update hidden bitmap
C
C         THATS ALL FOLKS !!!!!!!!!!!!
C


C     This include file copes with the bitmap swapping
C
      LOGICAL MAWS,GRDMNU
      INTEGER*4 MAXVP
      INTEGER*4 VPBANN
      PARAMETER (MAXVP = 5)
      PARAMETER (VPBANN = 17)
      INTEGER*4 VPXMAX,VPYMAX,VPYMIN,VPXMIN
C
      INTEGER*4 VIEWEX(4,MAXVP)
      INTEGER*4 VIEWPO(2,MAXVP)
      INTEGER*4 VP_OPTYPE
      INTEGER*2 VPOX,VPOY
      LOGICAL OBSLST(MAXVP,MAXVP),VPSEGF,VPDFIL
      LOGICAL VIEWPORT_CREATE

      REAL VIEWPS(4,MAXVP+1)
      LOGICAL MVPACT,MAINSVD,VPNUM(MAXVP),VPVIS(0:MAXVP),VPADD,VPDIS,
     +        VPDEL,VPMOV,VPREC,VPCAN,VPGRID(0:MAXVP)
      INTEGER*4 VPBITM(MAXVP),ATTRB(MAXVP),MAINBM,ATTM
      INTEGER*2 CVPN,FUNC0,FUNC1,FUNC2,FUNC3,FUNC4,FUNC5,PVPN
      INTEGER*2 HIPLN,BMSIZE(2),ORIGIN(2),WINBLT(4),VPLIM,LCOM
      INTEGER*4 DDCODE
      INTEGER*2 VPLAY(258,0:MAXVP),VPCLAY,VPSEGN
C
      COMMON /VIEWPR/ VIEWPS
      COMMON /VIEWPL/ MVPACT,MAINSVD,VPNUM,VPVIS,VPADD,VPDIS,
     +                VPDEL,VPMOV,VPREC,VPCAN,VPGRID,MAWS,
     +                OBSLST,VPSEGF,VPDFIL,GRDMNU
      COMMON /VIEWP2/ CVPN,FUNC0,FUNC1,FUNC2,FUNC3,FUNC4,FUNC5,
     +                HIPLN,BMSIZE,ORIGIN,WINBLT,VPLIM,PVPN,LCOM,
     +                VPLAY,VPCLAY,VPSEGN,VPOX,VPOY
      COMMON /VIEWPI/ VPBITM,ATTRB,MAINBM,ATTM,VIEWEX,VIEWPO,
     +                VPXMAX,VPYMAX,VPYMIN,VPXMIN
      COMMON /VIWPL2/ DDCODE
      COMMON/VIEWCR/  VIEWPORT_CREATE,VP_OPTYPE
C



