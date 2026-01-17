
/*

   DAXCAD   Generic startup GPR and X11

   Practical Technology Int'l 1991

   Public domain 2000-2017

   Main start up file for DAXCAD based applications

 */

#include <stdio.h>
#include <errno.h>

#include "daxcad_functions.h"
#include "xlang.h"
#include "version.h"

#ifndef GPR
extern char *Xdisplay;
#endif

#define F77false ((int)00)
#define F77true ((int)-1)

typedef int LOGICAL;
LOGICAL WantMyDrawingsBackedUp; /* flag to backup drawings */
LOGICAL server;					/* are we running -server ?*/
static char *sid = "5.1.0 date 17/01/26";

LOGICAL doabu_(wantBackups) int *wantBackups;
{
	/* the trailing underscore is especially for RSTALL:datah9.f */
	/* returns F77True if the user really does want files bu'ed  */
	*wantBackups = (int)WantMyDrawingsBackedUp;
}

main(argc, argv) int argc;
char **argv;
{
	static int debug = 1;
	int st;
	int status;
	int c;
	int opt;
	short type;
	char data;
	int rep;
	char display[256];
	char buff[256];
	int DAXCADREPAINT();
	LOGICAL version;
	LOGICAL demo;
	LOGICAL track;			  /* cursor tracking mode     */
	LOGICAL redraw_interrupt; /* disable redraw interrupt */
	char ServerMacro[1024];
	int mlen;
	int i;
	int cursor[3]; /* current cursor defs */
	extern int DAXCADXFault();
	/* extern int daxerrorhandler() ; */

	strcpy(display, "localhost:0"); /* set the display as null */
	demo = F77false;
	server = F77false;
	WantMyDrawingsBackedUp = F77true;
	redraw_interrupt = F77true;
	version = F77false; /* X version */
	cursor[0] = 1;		/* cross hair selected */
	cursor[1] = 100;	/* default sizes */
	cursor[2] = 100;

#ifdef GPR
	track = F77true; /* tracking is true */
#else
	track = F77false; /* tracking is false */
#endif

	for (i = 1; i < argc; ++i)
	{
		if (!strcmp(argv[i], "-demo")) /* demo mode */
		{
			demo = F77true;
		}
		else if (!strcmp(argv[i], "-display")) /* set sio line 1, 2 or 3 */
		{
			i++;
			if (i == argc || argv[i][0] == '-')
				ErrorAndAbort("Missing or misplaced argument for -display");
			else
			{
				strcpy(display, argv[i]); /* copy into the display */
			}
		}
		else if (!strcmp(argv[i], "-server")) /* delay before startup */
		{
			i++;
			if (i == argc || argv[i][0] == '-')
				ErrorAndAbort("Missing or misplaced argument for -server");
			else
			{
				server = F77true;
				strcpy(ServerMacro, argv[i]);
				mlen = strlen(ServerMacro);
			}
		}
		else if (!strcmp(argv[i], "-cross")) /* delay before startup */
		{
			i++;
			if ((i == argc || argv[i][0] == '-') || (i + 1 == argc || argv[i + 1][0] == '-'))
				ErrorAndAbort("Missing or misplaced argument for -cross");
			else
			{
				cursor[0] = 2;
				cursor[1] = atoi(argv[i++]);
				cursor[2] = atoi(argv[i]); /* get size */
			}
		}
		else if (!strcmp(argv[i], "-nocross")) /* no cursor */
		{
			cursor[0] = 0;
		}
		else if (!strcmp(argv[i], "-nobackup")) /* no backup */
		{
			WantMyDrawingsBackedUp = F77false;
		}
		else if (!strcmp(argv[i], "-notrack")) /* cursor tracking  off */
		{
			track = F77false;
		}
		else if (!strcmp(argv[i], "-track")) /* cursor tracking  on */
		{
			track = F77true;
		}
		else if (!strcmp(argv[i], "-nointerrupt")) /* cursor tracking  on */
		{
			redraw_interrupt = F77false;
		}
		else if (!strcmp(argv[i], "-help")) /* cursor tracking  on */
		{
			sprintf(buff, " ");
			ErrorAndAbort(buff);
		}
		else
		{ /* jobs in rags chief */
			sprintf(buff, "Invalid Argument (%d) %s ", i, argv[i]);
			ErrorAndAbort(buff);
		}
	}

#ifndef GPR

	/* Setup the X error handler */
	/*	XSetErrorHandler(daxerrorhandler) ;*/

	XSetIOErrorHandler(DAXCADXFault); /* xerror handler */

	GPR_$OPEN_X_DISPLAY(display, &st); /* open the display */

	if (st)
	{
		puts("Error opening X server display ");
		exit(1);
	}
	/*       XSynchronize(Xdisplay,1);*/
#endif

	PTICopyRight(); /* display copyright */

	setdaxsignals(); /* set all signals for fault handling */

	printf("\n");
	printf("%s\n", "DAXCAD 1986, 2020 - Release Version 5.0 - X86 64");

#if 0
	printf("\n%s\n", sid);
#endif
	printf("\n");

#ifdef APOLLOX
	printf("[GRAPHICS/OS] Apollo SR10.x (XApollo)\n");
#endif

#ifdef SUN
	printf("[GRAPHICS/OS] SUNOS SYS5R4 (X11R5)\n");
#endif

#ifdef HP700
	printf("[GRAPHICS/OS] HPUX (Motif X11R4)\n");
#endif

#ifdef APOLLO10
	printf("[GRAPHICS/OS] Apollo SR10.x (GPR)\n");
#endif

#ifdef APOLLO97
	printf("[GRAPHICS/OS] Apollo SR9.7.5 (GPR)\n");
#endif
#ifdef DAXWIN32
	printf("[GRAPHICS/OS] Windows Cygwin (X11)\n");
#endif
#ifdef DAXLINUX
	printf("[GRAPHICS/OS] Windows and Linux (X11)\n");
#endif

#ifndef GPR
	version = F77true; /* X version */
#endif

	daxcad_(&version, &demo, &server, ServerMacro, &mlen, cursor, &track, &redraw_interrupt); /* start daxcad */
}

ErrorAndAbort(ErrorMessage)
	/* Description   :- Boy has foobarred the command so
 *                  give him error message usage and exit
 *
 * Return status :- Does not return
 *
 *
 *
 * Externals     :-
 *
 * Notes         :-
 *
 */

	char *ErrorMessage; /* <i> meaningful error message         */
{

	fprintf(stderr, " %s \n Usage: daxcad [-display host:screen [-server macro.mac| [-demo]\\\n", ErrorMessage);
	fprintf(stderr, "       [-cross width height] [-nocross] [-nobackup] [-track] [-notrack]\n");

	fprintf(stderr, "\nwhere options include:\n");
	fprintf(stderr, "   -display host:screen      X server to use\n");
	fprintf(stderr, "   -server macro.mac         No Graphics Mode running macro.mac\n");
	fprintf(stderr, "   -demo                     Demonstration Mode. No save or plot\n");
	fprintf(stderr, "   -cross width height       Cross type cursor of height and width\n");
	fprintf(stderr, "   -nocross                  No cursor in viewport\n");
	fprintf(stderr, "   -nobackup                 No backup drawing files\n");
	fprintf(stderr, "   -track                    Cursor tracking enabled\n");
	fprintf(stderr, "   -notrack                  Cursor tracking disabled\n");
	fprintf(stderr, "   -nointerrupt              Disable redraw interrupt (Increases X performance)\n");
	fprintf(stderr, "   -help                     This page\n");

	fprintf(stderr, "\n\nDefault values depend on graphics version\n");

	exit(-1);
}

PTICopyRight()
/* Description   :- Display a copy right message
 * Return status :-
 * Notes         :-
 */
{

	//printf("%c", 12);
	puts("  ------------------------------------------------------");
	puts(" |                                                      |");
	puts(" |                                                      |");
	puts(" |                                                      |");
	puts(" |          DAXCAD for Windows & Linux 2026             |");
	puts(" |                                                      |");
	puts(" |           Built with CYGWIN 64 DLL                   |");
	puts(" |                                                      |");
	puts(" |                                                      |");
	puts(" |       This software was originally written by        |");
	puts(" |         Practical Technology Ltd in Glasgow          |");
	puts(" |                                                      |");
	puts(" |         https://github.com/davy-clg/daxcad           |");
	puts(" |                                                      |");
	puts(" |       NO WARRANTY APPLIES NOT THAT IT EVER DID       |");
	puts(" |                                                      |");
	puts(" |    *************UPDATE INFORMATION*************      |");
	puts(" |                                                      |");
	puts(" |    7/11/03:Label Fix Patch Move label now works      |");
	puts(" |    2/12/03:Label Move fix.  Move label corrupted     |");
	puts(" |    11/11/13:Updated for Windows 8.1                  |");
	puts(" |    06/03/17:New Make with Cygwin and eclipse         |");
	puts(" |    20/05/20:Ported to 64 Bit and Linux               |");
	puts(" |    17/01/26:Recovered from bitbucket                 |");
	puts(" |                                                      |");
	puts(" |                                                      |");
	puts("  ------------------------------------------------------");
}

PTIOldCopyRight()
/* Description   :- Display a copy right message - just for nostaliga!
 * Return status :-
 * Notes         :-
 */
{

	//printf("%c", 12);
	puts("  ------------------------------------------------------");
	puts(" |                                                      |");
	puts(" |                  C O P Y R I G H T                   |");
	puts(" | Practical Technology International (UK) Limited 1992 |");
	puts(" |                                                      |");
	puts(" |                125 Buchanan Street                   |");
	puts(" |                  Glasgow G1 2JA                      |");
	puts(" |                     Scotland                         |");
	puts(" |                  United Kingdom                      |");
	puts(" |                                                      |");
	puts(" |                DAXCAD Revison 4.0                    |");
	puts(" |                                                      |");
	puts(" |                   TERMS OF USE                       |");
	puts(" |                                                      |");
	puts(" |          This software is licensed by:               |");
	puts(" |      Practical Technology International Limited      |");
	puts(" |                                                      |");
	puts(" |   Any use by any other company or persons other than |");
	puts(" |        the licencee is STRICTLY PROHIBITED           |");
	puts(" |                                                      |");
	puts(" |   Acceptance of these conditions is not negotiable   |");
	puts(" |               and is protected by law.               |");
	puts(" |                                                      |");
	puts("  ------------------------------------------------------");
}
