C
C     @(#)  412.1 date 6/11/92 process.inc 
C
C
C     Filename    : process.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:35
C     Last change : 92/06/11 14:38:37
C
C     Copyright : Practical Technology Limited  
C     File :- process.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C     Include file for process event control within Fortran
C
      INTEGER*4 WAITPROC
      INTEGER*4 CHILDPROC

      INTEGER*4 MODELAEGIS
      INTEGER*4 MODELUNIX

      INTEGER*4 PROCOK
      INTEGER*4 CANNOTFORK
      INTEGER*4 PROCESSFAILED
      INTEGER*4 NOMOREPROCS
      INTEGER*4 CANNOTEXEC
      INTEGER*4  PROCEXIT      
      INTEGER*4  PROCSIG       
      INTEGER*4  NOSHELL

      INTEGER*4  NOPROCACTIVE  
      INTEGER*4  PROCEVENT
      INTEGER*4  NOPROCEVENT

      INTEGER*4 MAXPROC      

      PARAMETER( MAXPROC    = 10 )


      PARAMETER( MODELAEGIS   = 0 )
      PARAMETER( MODELUNIX  = 1 )

      PARAMETER( WAITPROC   = 0 )
      PARAMETER( CHILDPROC  = 1 )

      PARAMETER( PROCOK     = 0 )
      PARAMETER( CANNOTFORK = 1 )
      PARAMETER( PROCESSFAILED = 2)
      PARAMETER( NOMOREPROCS = 3 )
      PARAMETER( CANNOTEXEC  = 4 )
      PARAMETER(  PROCEXIT     = 5 )
      PARAMETER(  PROCSIG      = 6 )
      PARAMETER(  NOSHELL      = 7 )

      PARAMETER(  NOPROCACTIVE = 0 )
      PARAMETER(  PROCEVENT  = 1 )
      PARAMETER(  NOPROCEVENT  = 2 )


C      SIGHUP  1       { /* hangup */
C      SIGINT  2       { /* interrupt (rubout) */
C      SIGQUIT 3       { /* quit (ASCII FS) */
C      SIGILL  4       { /* illegal instruction (not reset when caught)*/
C      SIGTRAP 5       { /* trace trap (not reset when caught) */
C      SIGIOT  6       { /* IOT instruction */
C      SIGEMT  7       { /* EMT instruction */
C      SIGFPE  8       { /* floating point exception */
C      SIGKILL 9       { /* kill (cannot be caught or ignored) */
C      SIGBUS  10      { /* bus error */
C      SIGSEGV 11      { /* segmentation violation */
C      SIGSYS  12      { /* bad argument to system call */
C      SIGPIPE 13      { /* write on a pipe with no one to read it */
C      SIGALRM 14      { /* alarm clock */
C      SIGTERM 15      { /* software termination signal from kill */
C      SIGUSR1 16      { /* user defined signal 1 */
C      SIGUSR2 17      { /* user defined signal 2 */
C      SIGCLD  18      { /* death of a child */
C      SIGAPOLLO  19   { /* Apollo-specific fault */
C      SIGSTOP 20      { /* stop, cannot be caught, held, or ignored */
C      SIGTSTP 21      { /* stop signal generated from keyboard */
C      SIGCONT 22      { /* continue after stop */
C      SIGCHLD 23      { /* child status has changed */
C      SIGTTIN 24      { /* background read attempted from control terminal */
C      SIGTTOU 25      { /* background write attempted to control terminal */
C      SIGIO   26	{ /* i/o is possible on a descriptor */
C      SIGTINT 26      { /* input record is available at control terminal */
C      SIGXCPU 27      { /* cpu time limit exceeded */
C      SIGXFSZ 28      { /* file size limit exceeded */
C      SIGVTALRM 29    { /* virtual time alarm */
C      SIGPROF 30      { /* profiling timer alarm */
C      SIGURG  31      { /* urgent condition present on socket */

      INTEGER*4 SIGHUP 
      INTEGER*4 SIGINT  
      INTEGER*4 SIGQUIT 
      INTEGER*4 SIGILL  
      INTEGER*4 SIGTRAP 
      INTEGER*4 SIGIOT  
      INTEGER*4 SIGEMT  
      INTEGER*4 SIGFPE  
      INTEGER*4 SIGKILL 
      INTEGER*4 SIGBUS  
      INTEGER*4 SIGSEGV 
      INTEGER*4 SIGSYS  
      INTEGER*4 SIGPIPE 
      INTEGER*4 SIGALRM 
      INTEGER*4 SIGTERM 
      INTEGER*4 SIGUSR1 
      INTEGER*4 SIGUSR2 
      INTEGER*4 SIGCLD  
      INTEGER*4 SIGAPOLLO
      INTEGER*4 SIGSTOP 
      INTEGER*4 SIGTSTP 
      INTEGER*4 SIGCONT 
      INTEGER*4 SIGCHLD 
      INTEGER*4 SIGTTIN 
      INTEGER*4 SIGTTOU 
      INTEGER*4 SIGIO   
      INTEGER*4 SIGTINT 
      INTEGER*4 SIGXCPU 
      INTEGER*4 SIGXFSZ 
      INTEGER*4 SIGVTALRM 
      INTEGER*4 SIGPROF 
      INTEGER*4 SIGURG  

      PARAMETER(  SIGHUP  = 1  )
      PARAMETER(  SIGINT  = 2  )
      PARAMETER(  SIGQUIT = 3  )
      PARAMETER(  SIGILL  = 4  )
      PARAMETER(  SIGTRAP = 5  )
      PARAMETER(  SIGIOT  = 6  )
      PARAMETER(  SIGEMT  = 7  )
      PARAMETER(  SIGFPE  = 8  )
      PARAMETER(  SIGKILL = 9  )
      PARAMETER(  SIGBUS  = 10 )
      PARAMETER(  SIGSEGV = 11 )
      PARAMETER(  SIGSYS  = 12 )
      PARAMETER(  SIGPIPE = 13 )
      PARAMETER(  SIGALRM = 14 )
      PARAMETER(  SIGTERM = 15 )
      PARAMETER(  SIGUSR1 = 16 )
      PARAMETER(  SIGUSR2 = 17 )
      PARAMETER(  SIGCLD  = 18   )
      PARAMETER(  SIGAPOLLO  = 19 )
      PARAMETER(  SIGSTOP = 20  )
      PARAMETER(  SIGTSTP = 21  )
      PARAMETER(  SIGCONT = 22  )
      PARAMETER(  SIGCHLD = 23  )
      PARAMETER(  SIGTTIN = 24  )
      PARAMETER(  SIGTTOU = 25  )
      PARAMETER(  SIGIO   = 26  )
      PARAMETER(  SIGTINT = 26  )
      PARAMETER(  SIGXCPU = 27  )
      PARAMETER(  SIGXFSZ = 28  )
      PARAMETER(  SIGVTALRM = 29 )
      PARAMETER(  SIGPROF = 30  )
      PARAMETER(  SIGURG  = 31  )

C
C     Connecvtion vector include file for remote processes
C
C
      INTEGER*4 STREAMCONV(3)
      COMMON/STREAMS/STREAMCONV


