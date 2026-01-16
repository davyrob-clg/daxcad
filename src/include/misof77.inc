c
C     @(#)  412.1 date 6/11/92 misof77.inc 
C
C
C     Filename    : misof77.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:25
C     Last change : 92/06/11 14:35:30
C
C     Copyright : Practical Technology Limited  
C     File :- misof77.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This include file is for F77 integeration routines for 
c	nc ( misomex ) INTERFACE
c     
c     
c
c
      integer*4 mxcontrollayer
      integer*4 mxoriginmarker
c
      parameter(mxoriginmarker = 7)
c
      integer*4 laydat(256)
      integer*4 oldlay
      integer*4 nlay
      common/misomex/laydat,nlay,oldlay,mxcontrollayer

