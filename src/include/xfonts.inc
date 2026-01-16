c
c     SCCS id Keywords             @(#)  412.1 date 6/11/92 xfonts.inc   
c
c
c     This file is for external x font definitions     
c     
c
c
c     xfontsmax ...  The maximum number of fonts that can be defined
c
      integer*4 xfontsmax
      parameter(xfontsmax=10)
c
      integer*4 xfontsdim(2,xfontsmax)
      character*80 xfonts(xfontsmax)
c
      common/daxfontsc/xfonts
      common/daxfontsi/xfontsdim

