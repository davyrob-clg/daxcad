C
C     @(#)  412.1 date 6/11/92 aposerv.f 
C
C
C     Filename    : aposerv.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:30:15
C     Last change : 92/06/11 14:23:34
C
C     Copyright : Practical Technology Limited  
C     File :- aposerv.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE SHELLP(COMLIN,ST)
C     SUBROUTINE CHILD(N,CMD,ARGC,ST)
C     SUBROUTINE COPYF(SOURCE,TARGET)
C     SUBROUTINE DELETE(FILE,OK)
C     SUBROUTINE DIRFIN(PATHN,FILES,OK)
C     SUBROUTINE INQFS1(FILNAM,ST,OK)
C     SUBROUTINE SHELL(CMD,N,FN)
C     SUBROUTINE SIZE(FILE,LMAX,LCOUNT)
C     SUBROUTINE VALDIR(PATHN,OK)
C     SUBROUTINE EVENT_TIMEOUT(TIMEO,OK)
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE SHELLP(COMLIN,ST)
C     ============================
C1    VARYPE            C*(*)  I4
C1    IOSTAT              I    O
C
C2    Invokes a shell program. It call the C routine shellpc_
C2    This looks at a number of shells to use before invoking
C2    This is an interface routine between the F77 and the C
C2  
C2    Arguments:-
C2  
C2    COMLIN      ->      Command string to be executed. 
C2                        Should not contain any reference to a shell.
C2                        shellpc will split and invoke the program
C2  
C2  
C2    Error Returns:
C2  
C2    Because of limited arguments ST will be set if the program
C2    exited from a signal or from an exit code. If it was exit(x)
C2    the st will be +ve. If it was a signal then ST will be -ve
C2    
C2    1024            ->          Length is invalid
C2
      CHARACTER*(*) COMLIN
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
C
      EXTERNAL NLEN
C
      ST = 0
C     get active length.
      LENGTH = NLEN(COMLIN)
C
      IF (LENGTH.GT.1024.OR.LENGTH.LT.0) THEN
          ST = 1024
          GOTO 999
      ENDIF
C
C     Invoke the program
      CALL shellpc(COMLIN,LENGTH,ST)
C
999   CONTINUE
      RETURN

      END

      SUBROUTINE CHILD(N,CMD,ARGC,ST)
C     ===============================
C1    VARYPE          I4 C    C   I4
C1    IOSTAT          I  I    I   O
C
C2    Subroutine CHILD invokes an external
C2    program,and waits for completion of it
C2    before return.
C2    At entry N contains the number of
C2    arguments passed in character array
C2    ARGC for use in the invocation of the
C2    program,including the program name
C2    which must be passed as the first
C2    element of the array ARGC
C2    At exit ST contains the completion status
C2    of the child process.
C2  
C2  
C2    Arguments:-
C2  
C2    N               ->          Number of args
C2    CMD             ->          The command to executed full path name 
C2    ARGC            ->          All the arguments including the first
C2
C2  
C2  
C2    Error Returns:
C2  
C2    Because of limited arguments ST will be set if the program
C2    exited from a signal or from an exit code. If it was exit(x)
C2    the st will be +ve. If it was a signal then ST will be -ve
C2    
C2    1024            ->          Length is invalid
C2  
C2  
C2  
      INTEGER*4 N
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 STREAMS(3)
      INTEGER*4 LENGTH
      INTEGER*4 PID
      INTEGER*4 STATUS
      INTEGER*4 I
C
      CHARACTER*40 ARGC(10),CMD
      CHARACTER*1024 PROC
      CHARACTER*10 FORM
C
      EXTERNAL NLEN
C
      IF ( N.LE.0 ) THEN
          ST = 1024
          GOTO 999
      ENDIF

      WRITE(FORM,FMT='(2A,I2,A)' ) 'A,'' '',',N,'(A,'' '')'
      WRITE(PROC,FORM) CMD(1:NLEN(CMD)),( ARGC(I),I=2,N )

      CALL spawnprocess(STREAMS,PROC,LENGTH,PID,STATUS)

999   CONTINUE
      RETURN


C
      END
 
      SUBROUTINE COPYF(SOURCE,TARGET)
C     ================================
C1    VARYPE              C       C
C1    IOSTAT              I       I
C
C2    This routine will copy the target filel to the source file.
C2    It will use the best copy facility it has. At present it 
C2    invokes a unix /bin/cp or AEGIS /com/sh 
C2    Until such times as we can copy using file escripters
C2    we will have to lkeave it like this
C2    Thsis routine calls copyfc_ see this routine for more details.
C2
C2    Arguments:-
C2  
C2    SOURCE       ->          The file source
C2    TARGET       ->          The target file
C2    
C2  
C2  
C2    Error Returns:
C2  
C2    NONE as the are no provision for arguments. 
C2  
C2  
C
      CHARACTER*(*) SOURCE
      CHARACTER*(*) TARGET
      INTEGER*4 LENGTH1
      INTEGER*4 LENGTH2
      INTEGER*4 NLEN
      INTEGER*4 ST
      INTEGER*2 AQCNT
      EXTERNAL NLEN
C
      LENGTH1 = NLEN(SOURCE)
      LENGTH2 = NLEN(TARGET)
C
      IF ( LENGTH1.LE.0.OR.LENGTH2.LE.0 ) THEN
           RETURN
      ENDIF
C
      CALL RELDIS(AQCNT)
      CALL copyfc(SOURCE,LENGTH1,TARGET,LENGTH2,ST)
      CALL ACRDIS(AQCNT)
C
      END
C
C-------------------------------------------------------------
C
      SUBROUTINE DELETE(FILE,OK)
C     ==========================
C1    VARYPE             C   L
C1    IOSTAT             I   O
C
C2    This routine deletes a DISK file in UNXY dependant way
C2    This routine interfaces onto deletec_ which will use the system
C2    call unlink.
C2  
C2    Arguments:-
C2  
C2    FILE     ->      The file to be deleted
C2  
C2  
C2    Error Returns:
C2  
C2    OK       ->      Straight logical if succesfull .TRUE. otherwise .FALSE.
C2  

C
      CHARACTER*(*) FILE
      LOGICAL OK
      LOGICAL EX
      INTEGER*4 ST
      INTEGER*4 LINE
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
C
      EXTERNAL NLEN
C
      LENGTH = NLEN(FILE)
      IF (LENGTH.LE.0.OR.LENGTH.GT.1024) THEN
          OK = .FALSE.
          RETURN
      ENDIF

C     Use F77 inqure to determine the file existance
      INQUIRE(FILE=FILE,EXIST=EX)
C
      IF ( EX ) THEN
          CALL deletec(FILE,LENGTH,ST)
          OK = ST .EQ. 0
      ELSE
          OK = .FALSE.
      ENDIF
C
      END
C
C
C
      SUBROUTINE DIRFIN(PATHN,FILES,OK)
C     =================================
C1    VARYPE              C     C    L
C1    IOSTAT              I    I/O   O
C
C2    This routine will search the directory with the wildcard
C2    FILES and under the pathname output file PATHN. Any errors
C2    generated during this rumpus will be sent to a straem file
C2    read back to a buffer which is flagged true or false
C2  
C2    Arguments:-
C2  
C2    PATHN           ->          The pathname that contains the wildcard and the
C2                                directory to search.
C2  
C2    FILES           ->          The output filename
C2  
C2    Error Returns:
C2  
C2    OK       ->      Straight logical if succesfull .TRUE. otherwise .FALSE.
C2  
C
     
      CHARACTER PATHN*(*)
      CHARACTER FILES*(*)
      CHARACTER TPATH*1026
      INTEGER*4 NLEN
      INTEGER*4 LENGTH1
      INTEGER*4 LENGTH2
      INTEGER*4 ST
      INTEGER*4 POS
      LOGICAL OK
C
      EXTERNAL NLEN
C
C     look for a valid path here
      POS = INDEX(FILES,'/')
      IF ( POS.EQ.0 ) THEN
C         prefix a directory
          TPATH = './'//FILES
      ELSE
          TPATH = FILES
      ENDIF
      LENGTH1 = NLEN(PATHN)
      LENGTH2 = NLEN(TPATH)
C
      CALL dirfinc(PATHN,LENGTH1,TPATH,LENGTH2,ST)
C
      OK = ST.EQ.0
C
      END


C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 inqfs1.ftn Daxcad revision 1.19
      SUBROUTINE INQFS1(FILNAM,ST,OK)
C     ===============================
C1    VARYPE              C    I4 L
C1    IOSTAT              I    O  O
C
C2    This routine chechs the valididty of a file to be written to or not
C2    For comapatibilty The Apollo error code was returned. This
C2    is not longer used. This routine calls inqfs1c which opens a file for
C2    append. A system error can be obtained by using daxgeterror_()
C2
C2
C2    Arguments:-
C2  
C2    FILNAM      ->      The file to be checked
C2    ST          ->      No longer used Returns error code from C based code.
C2  
C2  
C2    Error Returns:
C2  
C2    OK       ->      Straight logical if succesfull .TRUE. otherwise .FALSE.
C2  
C2  
C2  
      INTEGER*4 ST
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
      LOGICAL OK

      CHARACTER*(*) FILNAM
      EXTERNAL NLEN
C
      LENGTH = NLEN(FILNAM)
      CALL inqfs1c(FILNAM,LENGTH,ST)

      OK = ST.EQ.0

      END
C
C
C-----------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 shell.ftn Daxcad revision 1.19
      SUBROUTINE SHELL(CMD,N,FN)
C     =========================
C1    VARYPE           C   I4 C
C1    IOSTAT            
C
C2    This routine does exactly the same as CHILD but does not have a status
C2    return arg Thus we call child directly.
C2  
C2    Arguments:-
C2  
C2  
C2    N               ->          Number of args
C2    CMD             ->          The command to executed full path name 
C2    FN              ->          All the arguments including the first
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  

      CHARACTER FN(10)*(*)
      CHARACTER CMD*(*)
      INTEGER*4  ST
      INTEGER*4  N
C
      CALL CHILD(N,CMD,FN,ST)
C
      END


      SUBROUTINE SIZE(FILE,LMAX,LCOUNT)
C     =================================
C1    VARYPE            C   I4    I4
C1    IOSTAT            I    O    O
C
C2    This routine checks the size of a file
C2    It looks a the biggest line within the file (LMAX)
C2    and returns the line count. Should provide a kind of box
C2  
C2    Arguments:-
C2  
C2    FILNM       ->      The file to be checked.
C2    LMAX        ->      The maximum line size not including CR
C2    LCOUNT      ->      The maximum line count
C2  
C2    Error Returns:
C2  
C2    NONE ?
C2  
C2  
      CHARACTER FILE*(*)
      INTEGER*4 LMAX
      INTEGER*4 LCOUNT
      INTEGER*4 LENGTH
      INTEGER*4 ST
      INTEGER*4 NLEN
C
      EXTERNAL NLEN
C
      LENGTH = NLEN(FILE)
C 
      CALL sizec(FILE,LENGTH,LMAX,LCOUNT,ST)
C
      END
C
C 
      SUBROUTINE VALDIR(PATHN,OK)
C     ===========================
C1    VARYPE              C   L
C1    IOSTAT              I   O
C
C2    Checks the path of the name PATHN. It can a checks that
C2    the name is a valid DIRECTORY.
C2  
C2    Arguments:-
C2  
C2    PATHN   ->  The directory path
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK       ->      Straight logical if succesfull .TRUE. otherwise .FALSE.
C2  
C2  
C
      CHARACTER PATHN*(*)
      INTEGER*4 ST
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      LOGICAL OK
C
      EXTERNAL NLEN
C
      LENGTH = NLEN(PATHN)
C
      CALL valdirc(PATHN,LENGTH,ST)
C
      OK  = ST .EQ. 0
C
      END

      
      SUBROUTINE EVENT_TIMEOUT(TIMEO,OK)
C     ==================================
C1    VARTYPE                   R   L
C1    IOSTAT                    I    O
C
C2    The event to signal this cancel is an Upstoke of a mouse key
C2    If the upstroke comes before the timeout then OK is set true
C2    If an upstroke is signelled before timeout then OK false
C2  
C2    Arguments:-
C2  
C2    TIMEO        ->      the number of seconds to  wait.
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    OK .TRUE.  means that the routine did timeout without a button being released.
C2  
C2  
C                                                  
      include 'include/apollo.inc'
      include 'include/gpr.ins.inc'
C
      INTEGER*2 EVTYPE
      INTEGER*2 SCRPOS(2)
      INTEGER*2 CURPOS(2)
      INTEGER*4 ST
      CHARACTER EVDATA
      DOUBLE PRECISION SEX
      DOUBLE PRECISION SEX2
      DOUBLE PRECISION ELAP
      REAL TIMEO
      LOGICAL OK
      LOGICAL LWAIT
C
C     get the starting time
      ELAP = 0
      CALL UNIXTIME(SEX)
C     start of loop
10    CONTINUE
C     Ensure cursor is in the daxcad pad so that we will see the event of the
C     button coming up before the timeout
      CURPOS(1) = APCURX(1)
      CURPOS(2) = APCURY(3)
      CALL GPR_$SET_CURSOR_POSITION(CURPOS,ST)
C     Now do the move to ensure everything is resynced properly
      CALL GPR_$MOVE(APCURX(1),APCURY(3),ST)
C     Now check for any events 
C     get an event if possible
      LWAIT=GPR_$COND_EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
C
C     sample the time after polling
      CALL UNIXTIME(SEX2)
C                                            
      IF(EVTYPE.EQ.GPR_$LEFT_WINDOW) THEN
C     Wait for the cursor to come back in and the return false
11       CONTINUE
         LWAIT = GPR_$EVENT_WAIT(EVTYPE,EVDATA,SCRPOS,ST)
         IF(EVTYPE.NE.GPR_$ENTERED_WINDOW) THEN
            GOTO 11                                   
         ELSE
            OK = .FALSE.
            RETURN
         ENDIF 
      ENDIF

C     ealapsed time calculation in apollo jiffies
      ELAP = SEX2-SEX
      IF(ELAP.GT.TIMEO) THEN
          OK = .TRUE.
          RETURN
      ENDIF
      IF(EVTYPE.EQ.GPR_$BUTTONS) THEN
          OK = .FALSE.
          RETURN
      ENDIF
      GOTO 10
      END


