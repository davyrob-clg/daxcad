
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 handler.c   */

/*
 *
 *
 *        Filename    : handler.c
 *        Version     : 412.1
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Contains faulthandler routine for DAXCAD	
 *
 *        This file contains the following routines:
 *
 *        DAXCADFault
*/

#include <stdio.h>
#include "daxcad_functions.h"
#include "signal.h"

#ifdef APOLLO

#include "/sys/ins/fault.ins.c"

#endif

static int FaultOccured=0;   /* a fault has occured must quit again */


DAXCADFault(Sig,Code)       /* genaeral fault handler for the tools */

int Sig;
int Code;

{

int retcod;
int status;
int cleanid;
int apcode;
int st;

   retcod = 0;
#ifdef APOLLO
   apcode = fault_$quit;           /* fault quit */
#endif
   if ( FaultOccured )
       exit(1);                    /* quit out now to be safe */


   FAULTHANDLER();
   cleanid = 0;			/* allows all files to be closed */
   TMPCLEANUP(&cleanid,&st);	/* clean up temp files */

   fprintf(stderr,"\n");
   fprintf(stderr,"*************************************************************\n");
   fprintf(stderr,"**                                                         **\n");
   fprintf(stderr,"**              S Y S T E M   T E R M I N A T E D          **\n");
   fprintf(stderr,"**                                                         **\n");
   fprintf(stderr,"*************************************************************\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"        DAXCAD Cannot continue execution from here\n");
   fprintf(stderr,"             However a crash file called:\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"                daxcad.4.0.save.drg\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"      Has been created with the drawing preserved.\n");
   fprintf(stderr,"  Run DAXCAD again to recover the contents of the drawing\n\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"*************************************************************\n");
   fprintf(stderr,"**                                                         **\n");
   fprintf(stderr,"**      S Y S T E M   F A U L T   I N F O R M A T I O N    **\n");
   fprintf(stderr,"**                                                         **\n");
   fprintf(stderr,"*************************************************************\n");
   fprintf(stderr,"\n");

#ifdef APOLLO

   psignal(Sig,"Program Faulted");

   if ( Sig == SIGFPE )	{

        switch ( Code )	{

             case FPE_INTOVF_TRAP :    
                 printf("Specific Fault : integer overflow\n");
		 apcode = fault_$fp_ovrflo;
                 break;
             case FPE_INTDIV_TRAP :    
                 printf("Specific Fault : integer division by zero \n");
		 apcode = fault_$fp_div_zero;
                 break;
             case FPE_FLTOVF_TRAP :    
                 printf("Specific Fault : floating overflow\n");
		 apcode = fault_$fp_ovrflo;
                 break;
             case FPE_FLTDIV_TRAP :         
                 printf("Specific Fault : floating/decimal division by zero\n");
		 apcode = fault_$fp_div_zero;
                 break;
             case FPE_FLTUND_TRAP :         
                 printf("Specific Fault : floating underflow\n");
		 apcode = fault_$fp_undflo;
                 break;
             case FPE_DECOVF_TRAP :         
                 printf("Specific Fault : decimal overflow\n");
		 apcode = fault_$fp_ovrflo;
                 break;
             case FPE_SUBRNG_TRAP :         
                 printf("Specific Fault : subscript range\n");
		 apcode = fault_$access_violation ;
                 break;
             case FPE_FLTOVF_FAULT :        
                 printf("Specific Fault : floating overflow\n");
		 apcode = fault_$fp_ovrflo;
                 break;
             case FPE_FLTDIV_FAULT :        
                 printf("Specific Fault : floating divide by zero\n");
		 apcode = fault_$fp_div_zero;
                 break;
             case FPE_FLTUND_FAULT :        
                 printf("Specific Fault : floating underflow\n");
		 apcode = fault_$fp_undflo;
                 break;
             default :
		 apcode = fault_$access_violation ;
                 break;
             
            
        }

        Code = apcode;
   }
   if ( Code > 255 ) {
       fprintf(stderr,"Apollo Specific: ");
       error_$print(&Code);
   }
   else {
       fprintf(stderr,"Non Apollo Code: ");
       error_$print(&Code);
   }

#endif
#ifdef SUN
   fprintf(stderr,"\n");
   psignal(Sig,"Program Faulted");
   fprintf(stderr,"\n");
#endif

#ifdef HP700
   fprintf(stderr,"Program Faulted sig %d external code %d\n\n",Sig,Code);
#endif

#ifdef APOLLO 

   fprintf(stderr,"\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"**                                                        **\n");
   fprintf(stderr,"**            Please use the command tb (traceback)       **\n");
   fprintf(stderr,"**        for a full trace of where the error occcured    **\n");
   fprintf(stderr,"**                                                        **\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"\n");

#else

   fprintf(stderr,"\n");
/*
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"**                                                        **\n");
   fprintf(stderr,"**           If neccessary report the crash to the        **\n");
   fprintf(stderr,"**              P.T.I. Customer Support Desk              **\n");
   fprintf(stderr,"**                                                        **\n");
   fprintf(stderr,"**                   Tel: 041 204 2221                    **\n");
   fprintf(stderr,"**                   Fax: 041 204 4959                    **\n");
   fprintf(stderr,"**                                                        **\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"************************************************************\n");
   fprintf(stderr,"\n");
*/

#endif

   FaultOccured = 1;               /* set if we come backagin */

#ifdef APOLLO


   if ( Code > 255 ) {
       pfm_$error_trap(&Code);        /* generate a trace back */
   }
   else	{
       pfm_$error_trap(&apcode);        /* generate a trace back */
   }
       	


#endif
   exit(1);                        /* finaly shut down */
}












