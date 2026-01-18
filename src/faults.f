C
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 faults.f   */
C

      SUBROUTINE ENTDAT(WRITING)
C     ==========================
C1    VARTYPE              L
C1    IOSTAT               I
C  
C2  
C2    Arguments:-
C2  
C2    WRITING       ->        Flag .TRUE. indicates that an entity is being written
C2                            and is not complete
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  

C
C2    Routine which saves the current database pointers
C2    and uses the input flag to tell whether a succesfull
C2    write to the data base has been succesfull
C
      include  'include/faults.inc'
      include  'include/masti.inc'
      include 'include/props.inc'
C
      LOGICAL WRITING
C
      IF(WRITING) THEN
          ENTITY_WRITE = .TRUE.
C         save pointers
          TTMIP=NMIPOS
          TTPDP=NPDPOS
          TTTXP=NTXPOS
          TTRLP=NRLPOS
          TTPRP=NPRPOS
          TTPCP=NPCPOS
      ELSE
C         recover pointers
          ENTITY_WRITE = .FALSE.
      ENDIF
C
      END
C
      INTEGER FUNCTION FAULTHANDLER()
C     =================================
C1    NO ARGS
C
C2    This is the main fault handler routine for daxcad
C
      include  'include/masti.inc'
      include  'include/faults.inc'
      include  'include/props.inc'
      include  'include/ftypes.inc'
      include  'include/product.inc'
      include  'include/apollo.inc'
      include  'include/krap.inc'
C
C
      INTEGER*2 I
      INTEGER*4 ST
      LOGICAL KILL
      INTEGER*2 P
      CHARACTER TEMP*20
      LOGICAL OK
      INTEGER COMUN,NLEN,DNUM
      EXTERNAL NLEN
C     Initial test for recursive faults
      IF(DAXCAD_FAULT_OCCURED) GOTO 99
C     set flag
      DAXCAD_FAULT_OCCURED = .TRUE.
C
C     tell the user what has happened
      DNUM = 523
      CALL DEPRNT(523)
C
C     check whether in mid write
      IF(ENTITY_WRITE) THEN
C         recover last pointers
          NMIPOS=TTMIP
          NPDPOS=TTPDP
          NTXPOS=TTTXP
          NRLPOS=TTRLP
          NPRPOS=TTPRP
          NPCPOS=TTPCP
      ENDIF
C     save the drawing if not a demo
      IF(KDEMO) THEN
C        well fancy that, too bad cos it's a demo copy 
         CALL DEPRNT(840)
      ELSE
C         save the drawing if anything init
          IF(NMIPOS.GT.1) THEN
             DNUM = 522
             CALL DCPRNT(522)
C            set file types
             DAXTYP = DAXDRG
C            make sure all flagging is all write
             DO 200 P=1,NMIPOS-1
                 CALL LABNFG(P)
 200         CONTINUE
C            save the drawing
             CALL STORE2(.TRUE.)
C            write out command buffer
             CALL WRCMDS()
          ENDIF          
      ENDIF
C     close diagnostsic unit
      CLOSE(UNIT=10)
C     make sure all units closed
C     bring down cleanly
C
C     try to delete the bitmap
      KILL=.FALSE.
C      CALL GPR_$TERMINATE(KILL,ST)
      RETURN
99    CONTINUE
C
C
      END
C
      SUBROUTINE WRCMDS()
C     ===================
C1    NO ARGS
C
C2    this routine will write out the command file DAXCAD.COMMANDS
C
      include  'include/product.inc'
      include  'include/comstack.inc'
C
      CHARACTER*80 FILNAM
      INTEGER*4 COMUN,I,NLEN
      LOGICAL OK
C
      EXTERNAL NLEN
C
      CALL FINDU1(COMUN,OK)
C     Write out command structure
C     set command filename
      FILNAM = PRNAM(1:NLEN(PRNAM))//'.commands'
      IF(OK) THEN
          OPEN(UNIT=COMUN,FILE=FILNAM,STATUS='UNKNOWN'
     +             ,ACCESS='SEQUENTIAL',ERR=200)
          DO 30 I=1,CSTKP
              WRITE(UNIT=COMUN,FMT='(A20,'' '',F12.7,F12.7)',ERR=200)
     +           CMDS(I),HITPTS(1,I),HITPTS(2,I)
 30       CONTINUE
      ENDIF
      RETURN
200   CONTINUE
      CALL DEPRNT(734)
      END
 
 
