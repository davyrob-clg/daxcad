

/* SCCS id Keywords             @(#)  412.1 date 6/11/92 daxproc.c   */

/*
 *
 *
 *        Filename    : daxproc.c
 *        Version     : 1.1
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Contains all UNIX dependant routines for DAXCAD process control
 *
 *        This file contains the following routines:
 *        
 *        spawnprocess_
 *        setdaxsignals
 *        BuildArgv
 *
 *
 *
 *
*/


#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include "xlang.h"

#include "daxcad_functions.h"




#define WAITPROC 0     /* parent wil wait for child */
#define CHILDPROC 1    /*   will fork and not wait will contact the parent through a signal  */

#define PROCOK        0    /* procees has  completed ok */
#define CANNOTFORK    1    /* procees cannot be forked go away */
#define PROCESSFAILED 2    /* procees has not completed */
#define NOMOREPROCS   3    /* cannot complete a new process  */
#define CANNOTEXEC    4    /* cannot exec the forked process for some reason  */
#define PROCEXIT      5   /* process has stopped by an exit code */
#define PROCSIG       6   /* Process has been signalled to stop */
#define NOSHELL       7   /* no shell to run the process ( Unlikely )*/

#define NOPROCACTIVE  0   /* no process are active */
#define PROCEVENT     1   /* child process event clear */
#define NOPROCEVENT   2   /* process are active but no events are currently queued */

#define MAXNAMESIZE 256    /* the maximum process allowed */
#define MAXPROC 10        /* the maximum number of process allowed */
#define MAXEVENT 100   /* the maximum number of process events allowed to occur 
                          after this eventing will always use the last entry */

#define NULLSIGNAL 0   /* thenull signal */


#define MAXTOOLSARGS  256
#define SUCCESS 0
#define INVALIDPATH 1
#define TOOMANYARGS 2

char *strrchr();


extern void DAXCADFault();            /* the tools base fault handler */

static int ToolsArgc = 0;      /* global argument count */
static char *ToolsArgv[256];       /* argument vector */

static char ProcName[MAXNAMESIZE];     /* main progarm buffer */





SPAWNPROCESS(Streams,ProcLocal,Length,Pid,st)

  /* Description   :- Invoke a process under the DAXTOOLS environment
  *                  
  * 
  *
  * Return status :- 
  *                  
  * Notes         :- The routine will try to invoke a program directly
  *                  without the need for a shell. Thus only object
  *                  models can be invoked.
  *                  
  *
  */

int Streams[3];      /* connection streams */

char *ProcLocal;  /* program name */
int *Length;       /* length of name */
int *Pid;          /* the pid or exit code depending on the waiting mode */
int *st;       /* return status described above */

{

	/* Must do a system herte */

}


setdaxsignals()   
                         /* Description   :- set all signals for DAXCAD
                         *                   MACHINE will have to be altered
                         * 
                         *
                         * Return status :- Should be ok 0
                         *                  
                         *   
                         *
                         */

{

  signal(SIGINT  ,DAXCADFault );
  signal(SIGILL  ,DAXCADFault );
  signal(SIGFPE  ,DAXCADFault );



   signal(SIGHUP  ,DAXCADFault );

   signal(SIGQUIT ,DAXCADFault );
 
   signal(SIGTRAP ,DAXCADFault );
   signal(SIGIOT  ,DAXCADFault );
   //signal(SIGEMT  ,DAXCADFault );

   signal(SIGBUS  ,DAXCADFault );
   signal(SIGSEGV ,DAXCADFault ); 
   signal(SIGSYS  ,DAXCADFault ); 
   signal(SIGPIPE ,DAXCADFault );

   signal(SIGTERM ,DAXCADFault );
   signal(SIGUSR1 ,DAXCADFault );
   signal(SIGUSR2 ,DAXCADFault );

   signal(SIGTSTP ,DAXCADFault );
   signal(SIGCONT ,DAXCADFault );


   signal(SIGTTIN ,DAXCADFault );
   signal(SIGTTOU ,DAXCADFault );
   signal(SIGIO   ,DAXCADFault );

   signal(SIGXCPU ,DAXCADFault );


   signal(SIGXFSZ ,DAXCADFault );

   

}


BuildArgv(Path,Argc,Argv,NewPath)       

                        /* Description   :- build an argument list for invoking
                              programs. Holds malloced in main storage
                              and is used one at a time 
                         * 
                         * 
                         *
                         * Return status :-  -1 Path name is 0 length.
                         *                   -2 Number of args execceded
                         *
                         *
                         *   
                         * Notes         :- Keeps 2 values in main
                         *                  storage. When called these
                         *                  values are freed.
                         *
                         */




char *Path;        /* the full path of the prgram including arguments */
int *Argc;         /* the number of arguments included in the list. 
                      This does not include the final null 
                   */
char **Argv;        /* the returned pointer to the argument list */
char **NewPath;     /* The new path to be executed */

{

int len;
int i;
char *path;
char arg[MAXNAMESIZE];
char buff[MAXNAMESIZE];
int j;
int trip;
char *pos;


   trip = 0;

   if ( ToolsArgc ) {
       for ( i=0;i<ToolsArgc;i++)
           free( ToolsArgv[i] );   /* free the currently used area */
       ToolsArgc = 0;
   }

   len = strlen(Path);     /* check length of path */

   if (!len ) {
       return INVALIDPATH;      /* length is invalid */
   }

   j = 0;
   i = 0;
   path = Path;

   for (  j; j<len ; j++ ) {       /* go round string */

       if ( *path != ' ' )  {       /* char found */

           while ( *path != ' ' && *path )   {
               if ( *path == '\"' || *path == '\'' ) {
                   trip = !trip;
                   *path++;
               }
               else
                    arg[i++] = *path++;
               j++;
           }

           if ( !trip ) {

               ToolsArgc++;        /*  increment argument count */

               if ( ToolsArgc == MAXTOOLSARGS )
                   return TOOMANYARGS;

               arg[i] = '\0';
  
               if ( ToolsArgc == 1 ) /* alter the path */
               {
                   strcpy(NewPath,arg);
                   pos = strrchr(arg,'/');
                   if ( pos ) {
                       strcpy(buff,++pos);
                       strcpy(arg,buff);   
                   }
  
                }

               ToolsArgv[ToolsArgc-1] = (char*)malloc(strlen(arg)+1); /* get space for argument vector */
               strcpy(ToolsArgv[ToolsArgc-1],arg);
               i = 0;
           }
           else    {
               arg[i++] = *path;
           }

       }

       path++;             /* increment pointer */
   }

   ToolsArgv[ToolsArgc] = NULL;

   *Argv = (char*)ToolsArgv;
   *Argc = ToolsArgc;

   return SUCCESS;



}


