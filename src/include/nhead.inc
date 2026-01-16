C
C     @(#)  412.1 date 6/11/92 nhead.inc 
C
C
C     Filename    : nhead.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:30
C     Last change : 92/06/11 14:36:37
C
C     Copyright : Practical Technology Limited  
C     File :- nhead.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This include file contains the declarations required
C2    for the creation of a database file header for the
C2    ROOTS system
C
      INTEGER*4 NHEADI(12),NHEADS(12)
C
      COMMON /NHEAD/NHEADI,NHEADS
C
C2    NHEADI(1) = number of MI records
C2    NHEADI(2) = number of PD records
C2    NHEADI(3) = number of TX records
C2    NHEADI(4) = number of PR records
C2    NHEADI(5) = number of SY records
C2    NHEADI(6) = number of TF2 records
C2    NHEADI(7) = number of TF3 records
C2    NHEADI(8-10) = spare
C2    NHEADI(12) = number of Named Layers records
C
C2    NHEADS(1) = record length of MI records
C2    NHEADS(2) = record length of PD records
C2    NHEADS(3) = record length of TX records
C2    NHEADS(4) = record length of PR records
C2    NHEADS(5) = record length of SY records
C2    NHEADS(6) = record length of TF2 records
C2    NHEADS(7) = record length of TF3 records
C2    NHEADS(8-10) = spare
C2    NHEADS(12) = record length of Named Layers records

