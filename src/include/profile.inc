C
C     @(#)  412.1 date 6/11/92 profile.inc 
C
C
C     Filename    : profile.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:36
C     Last change : 92/06/11 14:38:43
C
C     Copyright : Practical Technology Limited  
C     File :- profile.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     Common area for autoprofile routines to use
C
C     L1 L2 L3 current direction vector
C     PRMIP previous mi pointer to entity
C     CRMIP current mi pointer to entity
c     ACLOCK The current arc direction on needed if last entity was an arc
C     VCOR a correct vector in the corrwect sense has already found
C     ANGC an angle in the correct sense
C     ANGW an angle in the wrong sense
C     ENBUFF the modified vector buffer with start point 
C     XYI intersection point
C     IPUNIT fortran unit for storing intersection points
C     prevents rentry
C     NODEC VECTOT clever try again variables
C     RETRY this flag signals a retry
C     PCLOCK last arc direction status
C     ARCCLK direction of current arc being tested
C     RADC RADW a radius producing min ar max area
C     ARCRAD current arc radius
C     CURARC the current ent is an arc
C     ARCTSD valid arc has been stored at current intersection (tangency only)
C     LINTSD valid line has been stored at current intersection (tangency only)
C
      REAL L1,L2,L3,OX,OY,DISTI,ENBUFF(6),XYI(1,2)
      REAL ANGC,ANGW,REDIST,PROX,PROY
      INTEGER*2 PRMIP,CRMIP,VECTOT,REMIP
      INTEGER*4 IPUNIT,IPCNT,CURVEC
      LOGICAL ACLOCK,CLOCK,VCOR,OPENP,RETRY,NODEC(200),PCLOCK
      LOGICAL ARCCLK,CURARC,ARCSTD,LINSTD,PRVARC
      REAL ARCRAD,RADC,RADW

      COMMON/AUTOPR/ L1,L2,L3,OX,OY,DISTI,ANGC,ANGW,
     +               ENBUFF,XYI,REDIST,PROX,PROY,ARCRAD,RADC,RADW


      COMMON/AUTOPI/ PRMIP,CRMIP,VECTOT,REMIP

      COMMON/AUTOPF/ IPUNIT,IPCNT,CURVEC

      COMMON/AUTOPL/ ACLOCK,CLOCK,VCOR,OPENP,RETRY,NODEC,PCLOCK,
     +               ARCCLK,CURARC,ARCSTD,LINSTD,PRVARC

