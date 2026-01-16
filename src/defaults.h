/* SCCS id Keywords             @(#)  412.1 date 6/11/92 defaults.h   */

/*

	Include file for DAXCAD-DEFAULTS definitions 

*/


#define MAXPROPNAME 100			/* maximum size of propety name */
#define MAXFILE 1024			
#define MAXVALUE 1024
#define MAXBUF 1125

#define DAX_INTEGER		0
#define DAX_FLOATING		1
#define DAX_LOGICAL		2
#define DAX_STRING		3

static char *DefaultFile = NULL;
static FILE *DefaultsFp;
static DefaultsOpened=NULL;

/*	Error defintions */


#define DEFAULTS_OK		0	/* success */
#define DEFAULTS_CANNOT_OPEN	-1	/* Cannot open defaults file */
#define DEFAULTS_ALLREADY_OPEN	-2	/* File open allready */
#define DEFAULTS_NO_TYPE	-3	/* Undefined DAXCAD type */
#define DEFAULTS_NO_NAME	-4	/* Name not found */
#define DEFAULTS_NO_ARGS	-5	/* No argument to name */





