C
C     @(#)  412.1 date 6/11/92 faults.inc 
C
C
C     Filename    : faults.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:12
C     Last change : 92/06/11 14:29:48
C
C     Copyright : Practical Technology Limited  
C     File :- faults.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Include file to enable a fault handler 
C
      INTEGER*2 TTMIP
      INTEGER*2 TTPDP
      INTEGER*2 TTTXP
      INTEGER*2 TTRLP
      INTEGER*2 TTPRP
      INTEGER*2 TTPCP

      LOGICAL CRASHF
      LOGICAL DAXWRT
      LOGICAL ENTITY_WRITE
      LOGICAL DAXCAD_FAULT_OCCURED
      COMMON/FAULTS/DAXWRT,DAXCAD_FAULT_OCCURED,CRASHF,ENTITY_WRITE
      COMMON/FAULTI/TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP

