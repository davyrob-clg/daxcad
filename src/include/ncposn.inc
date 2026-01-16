C
C     @(#)  412.1 date 6/11/92 ncposn.inc 
C
C
C     Filename    : ncposn.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:29
C     Last change : 92/06/11 14:36:22
C
C     Copyright : Practical Technology Limited  
C     File :- ncposn.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
      REAL CLDATA(1:245)
      INTEGER*2 NOWRDS,ICLDAT(1:2,1:245)
      EQUIVALENCE (ICLDAT,CLDATA)
C
      COMMON /NCD1/CLDATA,NOWRDS
C
C
      REAL THISX,THISY,LASTX,LASTY,TRANX,TRANY,CLAMPS(70:71),
     +     CENTRX,CENTRY,RADIUS,THISI,THISJ,SHFTY
C
      COMMON /NCD2/THISX,THISY,LASTX,LASTY,
     +              TRANX,TRANY,CLAMPS,CENTRX,CENTRY,
     +              RADIUS,THISI,THISJ,SHFTY
C
C
      REAL TLDIAM,LASTF,THISF,NEXTF,THISP
      INTEGER GCHARS,TCHARS,DCHARS,ECHARS,MCHARS,PCHARS
      INTEGER LMOTN,LCIRQ,LDIMS
      LOGICAL GFUN,TFUN,DFUN,EFUN,MFUN,IJFUN,PFUN,RFUN,
     +        FEDON,CIRCON,RAPON,PNCHON,FFXTON,TRCHON,SCRBON,SCRBTR,
     1        TAPON,PLMODE,PUMODE,OPSKP,REVY,NIMODE,NIBBON
C
C2       TLDIAM - Diameter of current tool
C2       LASTF  - Feedrate last used
C2       THISF  - Feedrate currently in use
C2       NEXTF  - Feedrate for next move
C2       TRCHON - True if PLASMA torch on
C2       SCRBON - True if Scribe Head active
C2       NIMODE - True if nibble mode selected
C2       NIBBON - True if nibble Head active
C2       TAPON  - True if Tapping Head active
C2       PLMODE - True if PLASMA mode selected
C2       PUMODE - True if PUNCH mode selected
C2       PNCHON - True if PUNCH head active
C2       PFUN   - True if PARAMETER to be set
C2       PCHARS - Contains parameter number to be set
C2       THISP  - Contains current parameter value
C
      COMMON /NCD3/TLDIAM,LASTF,THISF,NEXTF,THISP
     +       GFUN,TFUN,DFUN,EFUN,MFUN,IJFUN,PFUN,RFUN,
     +       FEDON,CIRCON,RAPON,PNCHON,FFXTON,TRCHON,SCRBON,SCRBTR,
     +       TAPON,PLMODE,PUMODE,NIMODE,OPSKP,REVY,NIBBON
     +       GCHARS,TCHARS,DCHARS,ECHARS,MCHARS,PCHARS,
     +       LMOTN,LCIRQ,LDIMS

      INTEGER CCCHAR
      LOGICAL CCON,TRANS1,ARAPID,DCVON,CCHOLD,TRHOLD,NIHOLD
C
C2       CCON   - True if CUTCOM on
C2       TRANS1 - True if TRANSLATION required
C2       ARAPID - True if RAPID motion allowed
C2       DCVON  - True if DUST COVER loaded
C2       CCCHAR - Contains current CUTCOM value (41 or 42)
C2       CCHOLD - Cutcom is on hold,switch when motion detected
C2       TRHOLD - Torch is on hold,switch when motion detected
C2       NIHOLD - Nibble is on hold,switch when motion detected.
C
      COMMON /NCD7/CCON,TRANS1,ARAPID,DCVON,CCHOLD,TRHOLD,
     +             ,NIHOLD,CCCHAR
C
C
C     curent GNC command buffer
C     contains GNC command currently being processed
      CHARACTER*12 GNCCMD
      COMMON /NCD8/GNCCMD
C
C
C2       GNCSEQ - contains sequence number of GNC command
C2       ECHOIT - non-zero,means echo control data to screen.
      INTEGER GNCSEQ,ECHOIT
      COMMON /NCD9/GNCSEQ,ECHOIT
C
