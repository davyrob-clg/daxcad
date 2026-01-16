C
C     @(#)  412.1 date 6/11/92 psearch.inc 
C
C
C     Filename    : psearch.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:38
C     Last change : 92/06/11 14:38:56
C
C     Copyright : Practical Technology Limited  
C     File :- psearch.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This include file defines the
C2    variable storage required for
C2    searches on keyed non-graphic
C2    data.
C
C2    NPSRCH is the pointer to the next
C2    item to search in the property data set.
C2    SRCKEY is the KEY upon which the data search
C2    should be executed
C2    SRCDAT  is the data to be matched from the
C2    key field during the search.
C
      INTEGER*2 NPSRCH
      COMMON /PSRCH1/NPSRCH
C
      CHARACTER*80 SRCKEY,SRCDAT
      COMMON /PSRCH2/SRCKEY,SRCDAT
C
