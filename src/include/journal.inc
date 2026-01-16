C
C     @(#)  412.1 date 6/11/92 journal.inc 
C
C
C     Filename    : journal.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:19
C     Last change : 92/06/11 14:32:52
C
C     Copyright : Practical Technology Limited  
C     File :- journal.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     This file contains the data used by the program journaling
C     other data is also contained in the command stacking include
C     file commstack.inc
C
C     This data is initialised in stinit
C
C     JOURNU = jornal output file logical unit
C     JOURON = TRUE to show that journal output is on
C              and the journal file has been successfully
C     PNTMOD = TRUE if the calls to TCURS WRTJRN has been made from GETANS
C              so that the journaling outputs the the END or the CENTRE
C              of a line from GETANS or the screen hitpoint if the call
C              to TCURS did not come through GETANS
C     PREHIT = 'm' if last hit was a menu
C     PREHIT = 'w' if last hit was in the graphics window
C     PREHIT = 'a' if last hit was an answer to a prompt 
C
      CHARACTER*1 PREHIT, BUFFER*84, JRNCMD(20)*20
      INTEGER*4 JOURNU
      LOGICAL JOURON, PNTMOD
C     
C     JRNCMD contains all the character data necessary to
C     to produce a macro file
C
C     JRNCMD(1) = 'GINPUT'
C     JRNCMD(2) = '* Screen Hit'
C     JRNCMD(3) = 'PICKZONE LARGE'
C     JRNCMD(4) = '* DBGON'
C     JRNCMD(5) = '* HOLDMAC' 
C     JRNCMD(6) = 'CALL $ZOOMI'
C     JRNCMD(7) = 'CALL $EXTENTS'
      COMMON /JOURNL/  JOURNU,JOURON,PNTMOD       
      COMMON /JRNCMD/ JRNCMD,BUFFER, PREHIT


