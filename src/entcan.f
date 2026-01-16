C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 entcan.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ENTCAN(TMIP,ST)
C
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE ENTCAN(TMIP,ST)
C     ==========================
C1    VARYPE            I2   I4
C1    IOSTAT            I    O
C
C2    Cancels an entity from the display list
C2    It looks for it at the top of the display file
C2    And if its it not there it searches thru anyway
C2    This ensures that the element can be properly canceled
C2    It also works only on the one viewport the current one
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 ST
      INTEGER*2 TMIP
      INTEGER*2 DFP1
      INTEGER*2 RETMIP
      INTEGER*2 DFINDX
      INTEGER*2 ENT
      LOGICAL OK
C
C     set status
      ST = 0
C     set display file pointer
      DFP1 = LDFILE(CVPN)-1
C     find out if we can still see the ent to be cancelled
      CALL RDISPF(DFP1,ENT,RETMIP,OK)
      IF(TMIP.EQ.RETMIP) THEN
C         Is it ent on the end of the disp' file, where we put it
          LDFILE(CVPN)=LDFILE(CVPN)-1
C         set status an go home
          ST = 0
          GOTO 999
       ELSE
C         A zoom or pan might have moved it, go looking.
          DFINDX=1
C         Main loop round current display list looking for the MIP
 10       CONTINUE
              CALL RDISPF(DFINDX,ENT,RETMIP,OK)
              IF (TMIP.EQ.RETMIP) THEN
C                 GOTCHA! Mark it as deleted.
                  RETMIP = -RETMIP
                  CALL WDISPF(DFINDX,ENT,RETMIP,OK)
                  ST = 0
                  GOTO 999
               ELSE
C                  NO? Oh well, try the next.
                   DFINDX = DFINDX + 1
                   IF (DFINDX.LT.DFP1) GOTO 10
               ENDIF
C
      ENDIF
C     not found at all nothing done about it
      ST = 1
      RETURN
C     safe exit
999   CONTINUE
      END
 
