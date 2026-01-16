/* SCCS id Keywords             @(#)  412.1 date 6/11/92 defaults.c   */

/*
 *
 *
 *        Filename    : defaults.c
 *        Version     : 412.1
 *
 *        Copyright : Practical Technology Limited
 *
 *        This source file contains all main and F77 interfacing routines
 *        for the DAXCAD misomex plotter driver module.
 *
 *        This file contains the following routines:
 *
*/

#include <stdio.h>
#include <math.h>
#include <sys/file.h>
#include <time.h>
#include <errno.h>

#include "xlang.h"    /* Cross language refernce file must be updated if
                         any functions added to in this file
                      */

#include "defaults.h"

int
OpenDefaults()

      /* Description   :- Open the defaults file and keep it open
       * 		  If another open is detected then the function will
       *                  return an error
       *
       * Return status :- 	allready open
       *                      Cannot open
       *   
       *                  
       * Notes         :- The getdefaults can be used with this function for multiple reads
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */
{

	if ( DefaultsFp )	{

		return DEFAULTS_ALLREADY_OPEN;
	}


        if ( DefaultFile )      {
                DefaultsFp = fopen(DefaultFile,"r");
        }
        else    {
                DefaultsFp = fopen("~/daxcad-defaults","r");
                if ( !DefaultsFp )      {

                        DefaultsFp = fopen("/daxcad/daxcad-defaults","r");
                }
        }


        if ( !DefaultsFp )      {
                perror("Cannot open daxcad-defaults");
                return DEFAULTS_CANNOT_OPEN;
        }

	DefaultsOpened = 1;
	return DEFAULTS_OK;


}

getdefaults(Name,Type,Value,st)

      /* Description   :- Reads defaults file to obtain values from DAXCAD defaults file
       * 
       * 
       *
       * Return status :-     Name not found.
       *                      Name has no args                 
       *                      Cannot open defaults file   
       *                      Name found but type undefined
       *  
       *   
       *                  
       * Notes         :- Reads a predefined defaults file name either coded or set
       *                  The name refrs to the propety name. The type is one of the following:-
       *                  
       *                  
       *                  DAX_INTEGER
       *                  DAX_FLOATING POINT
       *                  DAX_LOGICAL
       *                  DAX_STRING
       *
       *
       *
       */

char *Name;		/*	<i>	Name of property */
int Type;		/*	<i>	Type of property to assign */
int *Value;		/*	<o>	The value Note this must be of the correct size to hold whatever
					is requested */
int *st;


{


char name[MAXPROPNAME];
int type;
int len;
char value[MAXVALUE];
char buffer[MAXBUF];
char *s;
char *p;
char *end;
double db;
int running;
int op;
int ret;


	op = DefaultsOpened;

	if ( !op ) 	{
		ret = OpenDefaults();
		if ( ret == DEFAULTS_CANNOT_OPEN )	{
			*st = ret;
			return;
		}
	}

	*st = DEFAULTS_NO_NAME;

	rewind(DefaultsFp);
	running = 1;

	while ( running && (s =fgets(buffer,MAXBUF,DefaultsFp)))	{	/* read in buffers */

		sscanf(buffer,"%s",name);		/* scanf internaly */

		if ( strcmp(name,Name) == NULL )	{	/* look for name */

			len = strlen(Name);
			
			p = (char*)&buffer[len];

			while ( *p == ' ' && *p )		/* take out white space */
				p++;
	
			if ( ! *p )	{
				*st = DEFAULTS_NO_ARGS;
				break;
			}

			switch ( Type )	{

				case DAX_INTEGER :

					len = atoi(p);
					memcpy(Value,(char*)&len,sizeof(int));
					*st = 0;
					break;

				case DAX_FLOATING :

					db = atof(p);
					memcpy(Value,(char*)&db,sizeof(double));
					*st = 0;
					break;

				case DAX_LOGICAL :

					if ( strncmp ( p,"true",4 ) == NULL )	{

						*Value = (int)-1;
					}

					else	{
						*Value = (int)0;

					}
					*st = 0;
					break;

				case DAX_STRING :
					len = strlen(p)-1;
					p[len] = '\0';
                                        end = p+len-1;
			                while (isspace(*end) && end > p ) /* take out trailing blanks */
				              end--;
                                        *(end + 1) = '\0';
					strcpy (Value,p);
					*st = 0;
					break;
				default :
					*st = DEFAULTS_NO_TYPE;
					break;
			}
			running = 0;
		}

	}
	
	if ( !op ) 
		CloseDefaults();
	

}

CloseDefaults()

      /* Description   :- Closes defaults file
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- This close file opend by OpenDefaults
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{

	if ( DefaultsFp )
		fclose(DefaultsFp);

	DefaultsOpened = 0;
	DefaultsFp = NULL;
}
