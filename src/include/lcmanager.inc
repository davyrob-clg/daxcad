C
C     @(#)  412.1 date 6/11/92 lcmanager.inc 
C
C
C     Filename    : lcmanager.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:21
C     Last change : 92/06/11 14:33:26
C
C     Copyright : Practical Technology Limited  
C     File :- lcmanager.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     This include file is for the DAXCAD locator manager.
C
C     The LM will allow a group of entities to be drawn with the
C     in a dynamic animated form.
C
C
C     LMLDRW      ->      Last position in scratch file that was drawn to.
C     LMPOS       ->      Current cursor position detected.
C     LMBUTTON    ->      Key pressed during interupt
C     LMEVTYPE     ->     Current eventype
C     LMPENDING    ->     Must act up interup event
C
C
      INTEGER*4 LMLDRW
      INTEGER*4 LMPOS(2)
      INTEGER*4 LMBUTTON
      INTEGER*4 LMEVTYPE
      LOGICAL LMPENDING
C
      COMMON/DAXLM4/LMLDRW,
     +              LMPOS,
     +              LMBUTTON,
     +              LMEVTYPE,
     +              LMPENDING
