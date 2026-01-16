/*
     @(#)  412.1 date 6/11/92 tempfiles.c 


     Filename    : tempfiles.c
     Version     : 412.1
     Retrieved   : 92/06/12 15:51:02
     Last change : 92/06/11 14:41:38

     Copyright : Practical Technology Limited  
     File :- tempfiles.c

     Temorary file control for tools
*/


#include<stdio.h>

#include "daxcad_functions.h"

/*

      Creates temporty files for use by DAXTOOLS

      The system will keep a record of all temporary

      files created. During a tools exit the files will

      be deleted.

*/

#define FILESIZE 20        /* size of each temp file */
#define TEMPFILES 100      /* the maximum number allowed */
#define TMPDIR "/tmp"      /* temporty direcotry */
#define PREFIX "pt$"       /* prefix for our own id */

static char *PtTempFiles[TEMPFILES];   /* pointers to files created */


GETTMPFILE(TempFile,Length,Id,Status) /* get the temp file */

char *TempFile;        /* the string to copy into */
int *Length;           /* the returned length */
int *Id;               /* the counter. Should allow
                          files to be deleted individualy
                       */
int *Status;           /* status return 

                       1   ->  Count exceeded 
                       2   ->  could not create a tmp
                       */


{

char *tmp;
int len;
char *new_mem;
int nextfree;

   *Status = 0;
   *Id = -1;

   for(nextfree=0;nextfree<TEMPFILES;nextfree++)  {
       if ( !PtTempFiles[nextfree] ) 
           break;
   }
   if ( nextfree == TEMPFILES ) {
       *Status = 1;
       return;
   }

   tmp = (char*)tempnam(TMPDIR,PREFIX);

   if ( !tmp ) {
       *Status = 2;
       return;
   }

   len = strlen(tmp)+1;  /* need some mem to alloc */


   PtTempFiles[nextfree] = (char*) malloc(len); 

   strcpy(PtTempFiles[nextfree],tmp);   /* ok need to copy */

   strcpy(TempFile,tmp);   /* get local copy back */
   *Length = len-1;        /* set length */
   *Id = nextfree+1;

}

TMPCLEANUP(Id,Status) /*  cleans up all temporay files 
                          or a specific file */

int *Id;           /* id reqwuired */
int *Status;       /*  return status 
   
                       1   ->  nothing to clean up 
                   */


{

int i;     /* for loop var */
int id;
char *file;

   *Status = 0;
   id = *Id - 1;       /* get id */

   if ( id >= 0 ) {                /* wants do deledte a file */
       file = PtTempFiles[id];
       if (file) {
           unlink(file);   
           free(file);
           PtTempFiles[id] = NULL;
       }
       return;
    }
    else   {
       for ( i=0;i<TEMPFILES;i++)   {

           file = PtTempFiles[i];

           if (file) {
               unlink(file);
               free(file);
               PtTempFiles[i] = NULL;
           }
       }
   }
}






