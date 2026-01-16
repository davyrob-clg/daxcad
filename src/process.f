C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 process.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE SPAWNPROC(PROCNAME,RETCODE,ST)
C     SUBROUTINE SETPROCSTREAMS(CONNV,ST)
C     SUBROUTINE RESETPROCSTREAMS(ST)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE SPAWNPROC(PROCNAME,RETCODE,ST)
C     ==========================================
C1    VARYPE                 C*(*)    I4     I4
C1    IOSTAT                    I     O      O
C
C2    Runs a sub process. Waits for child to complete
C2    and returns a status with a code wich is either
C2    a signal or an exitcode.
C2
C2    Arguments:-
C2  
C2    PROCNAME   ->  Process name
C2    RETCODE     ->  Eith signal or exit code number
C2  
C2  
C2    Error Returns:
C2  
C2    STATUS RETURNS ARE IN PROCESSES.INC
C2  
C2  
      include  'include/process.inc'
C
      INTEGER*4 ST
      INTEGER*4 PROCNUM
      INTEGER*4 MODEL
      INTEGER*4 WAITMODE
      INTEGER*4 NLEN
      INTEGER*4 PID
      INTEGER*4 LENGTH
      INTEGER*4 RETCODE
      INTEGER*2 AQCNT
      CHARACTER*(*) PROCNAME
      EXTERNAL NLEN
C
C
C     use shellp to run process in wait mode
C     so that it appears like a subroutine to
C     the application programmer.

      ST = 0
      RETCODE = 0
      MODEL = MODELAEGIS
      WAITMODE = WAITPROC
      LENGTH = NLEN(PROCNAME)
C     release display in case of  faux pas
      CALL RELDIS(AQCNT)
      CALL spawnprocess(STREAMCONV,
     +              PROCNAME,
     +              LENGTH,
     +              PID,
     +              ST)
      CALL ACRDIS(AQCNT)
      IF(ST.NE.PROCOK) THEN
C         set return code
          RETCODE = PID
      ENDIF
      
C
      END
C
C     ------------------------------------------------


      SUBROUTINE SETPROCSTREAMS(CONNV,ST)
C     ====================================
C1    VARYPE                        I4  I4
C1    IOSTAT                        I     O
C
C2    Sets up a connection vector for passing to remote processes
C2    from within DAXTOOLS
C2    There are 3 possible streams ST_IN STD_OUT ERR_OUT
C  
      include  'include/process.inc'
C
      INTEGER*4 CONNV(3),ST
C
C
      STREAMCONV(1) = CONNV(1)
      STREAMCONV(2) = CONNV(2)
      STREAMCONV(3) = CONNV(3)
      ST = 0
C
      END
C
      SUBROUTINE RESETPROCSTREAMS(ST)
C     =================================
C1    VARYPE                        I4   
C1    IOSTAT                        O
C
C2    Restets all connection streams
C  
      include  'include/process.inc'
C
      INTEGER*4 ST
      STREAMCONV(1) = -1
      STREAMCONV(2) = -1
      STREAMCONV(3) = -1
      ST = 0
C
      END

