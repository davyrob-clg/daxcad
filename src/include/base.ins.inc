C
C     @(#)  412.1 date 6/11/92 base.ins.ftn 
C
C
C     Filename    : base.ins.ftn
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:59
C     Last change : 92/06/11 14:24:06
C
C     Copyright : Practical Technology Limited  
C     File :- base.ins.ftn
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C   BASE.INS.FTN, /sys/ins, jrw, 07/08/81
C
C   CHANGES:
C     88/07/11  leduc  added declaration for pathname constants
C     88/01/07  garyf  removed errin definition
C     87/12/31  garyf  change errout & add stderr
C     87/02/10  mishkin Added ios_$stdin, etc.
C     86/07/24  geiser IADDR is now an intrinsic.
C     85/03/26  RpS    Added lib_$member_of_set declaration
C     81/09/17  CAS    added declaration of IADDR
C
C   base include file for fortran customers
                                               
C---System defined stream id's:

       integer*2
     +     stream_$stdin, stream_$stdout, stream_$errout, 
     +     stream_$stderr

       parameter (
     +     stream_$stdin   = 0,
     +     stream_$stdout  = 1,
     +     stream_$errout  = 2,
     +     stream_$stderr  = 2)

       integer*2
     +     ios_$stdin, ios_$stdout, ios_$errout, ios_$stderr

       parameter (
     +     ios_$stdin   = 0,
     +     ios_$stdout  = 1,
     +     ios_$errout  = 2,
     +     ios_$stderr  = 2)


C---Status "ok" definition:

       integer*4
     +     status_$ok

       parameter (
     +     status_$ok = 0)


C   Constant declarations for pathname/component name length:
C   The long versions of the maxes should be used as name_$pnamlen_max 
C   and name_$complen_max are obsolete.

       integer*4
     +     name_$long_complen_max, name_$long_pnamlen_max,
     +     name_$pnamlen_max, name_$complen_max


       parameter (
     +     name_$long_complen_max = 255,
     +     name_$long_pnamlen_max = 1023,
     +     name_$pnamlen_max = 256,
     +     name_$complen_max = 32)



C---Functions:

       logical
     +     lib_$member_of_set
