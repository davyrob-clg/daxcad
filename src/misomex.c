/* SCCS id Keywords             @(#)  412.1 date 6/11/92 misomex.c   */

/*
 *
 *
 *        Filename    : misomex.c
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
#include <string.h>

#include "xlang.h"    /* Cross language refernce file must be updated if
                         any functions added to in this file 
                      */

#include "misomex.h"
#include "daxcad_functions.h"
#include "defaults.h"


mxinit(status)


      /* Description   :- Initalise MISOMEX plotter interface.
       * 
       * 
       *
       * Return status :- -1	Cannot initalise system.
       *                  
       *   
       *                  
       * Notes         :-	This will allocate memory and clear 
       *			data structures for usage.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

int *status;


{

DAXCADINT ent;
unsigned int bytes;

	if ( MisomexInit )	{
	
		RelDaxDisplay();
		printf("Warning NC interface is still open \n");
		AqDaxDisplay();
		
		NcShut();
	}

	
	CurrentMode = VIEW_NORMAL;	/* set mode flags */
	EditMode = MAINMODE;

	MisomexInit = 1;	/* set inial flag */

	bytes = DAXCADTEXT;
	GlobalEntity.text = (char*)malloc(bytes);	/* make space for global buffer */

	NOSRCH();	/* clear search masks */
	ent = LINE;
	ALSRCH(&ent);	/* enable searching lines only */
	ent = ARC;
	ALSRCH(&ent);	/* enable searching lines only */

	MISOLAYER(&MisomexLayer,&LayerInit);
	MISOLAYER(&MisomexLayer,&LayerOn);

	NcInitProperties();

}

NcShut()

      /* Description   :- Shut down Misomex system
       * 
       * 
       *
       * Return status :-
       *                  
       *   
       *                  
       * Notes         :-
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{

int i;

	free(GlobalEntity.text);	/* free up text */

	for ( i=0;i<NcPropCount;i++)	{	/* free up proerty data */
		free((char*)NcStdProps[i]);
	}

	NcPropCount = 0;		/* set property counter */
	MisomexInit = 0;		/* set inial flag */
}

WRMX00()

      /* Description   :- This routine is the main driver for the MISOMEX 
       *                  plotter interface routine with DAXCAD
       * 
       * 
       *
       * Return status :-NONE
       *                  
       *   
       *                  
       * Notes         :-Controls all main menu routines.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{

float hpx;
float hpy;
int status;
int running;
int ch;			/* character returned from TCURS routine */
int daxcell;
int daxmenu;
int daxnum;
DAXCADLOGICAL ok;
DAXCADINT mipp;
DAXCADINT textmip;
int seqnum;
DAXCADLOGICAL option;
DAXCADLOGICAL quit;
int use_tcurs;
int pnoun;
int tmode;


	mxinit(&status);	/* initalise misomex routines */
	MxMenu(MISOMEX_MAIN,MENU_ON);		/* load up main control menus */
	running = 1;	
	use_tcurs = 1;

	while( running )	{

		if ( use_tcurs ) {
			daxnum = 0;
			TCURS(&ch,&hpx,&hpy);		/* get a cursor hit from the screen */
		}

		GETCELL(&daxcell,&daxmenu,&daxnum,&pnoun);

		use_tcurs = 1;	/* No modal search */

		if ( daxmenu != VERB_MENU && daxmenu != WORKSPACE )	{	/* break out of This menu strucure */
			GTMCLO(&daxmenu,&daxcell);	/* clear other optios cell and go out */
			running = 0;
			daxnum = 0;
			break;
		}
		if ( daxcell == 0 )
			daxnum = 0;

		switch(daxnum)	{

			case CREATE_SEQ :	/* create a new sequence profile and add to the existing system */

				GTMCLO(&daxmenu,&daxcell);		/* clear befor going into subordinate */

				if ( CurrentMode == VIEW_LAYER )	{
					MISOLAYER(&MisomexLayer,&LayerBack);
				}
				EditMode = CREATING;
				if ( CreateSequence() == EXIT_MENU )	{
					running = 0;
					break;
				}

				if ( CurrentMode == VIEW_LAYER )	{
					MISOLAYER(&MisomexLayer,&LayerOff);	/* switch of all other layers ( CELL Highlighted in MxMenu */
				}
				break;

			case VIEW_PROFILES :
				ProfileCell = daxcell;	/* save this cell for later use in menus */
				switch(CurrentMode )	{
					case VIEW_NORMAL :
						MISOLAYER(&MisomexLayer,&LayerOff);
						CurrentMode = VIEW_LAYER;
						GTMCHI(&daxmenu,&daxcell);
						break;
					case VIEW_LAYER :
						MISOLAYER(&MisomexLayer,&LayerBack);
						CurrentMode = VIEW_NORMAL;
						GTMCLO(&daxmenu,&daxcell);
						break;
				}
				break;
			case EDIT_SEQ :	/* create a new sequence profile and add to the existing system */

				GTMCLO(&daxmenu,&daxcell);		/* clear befor going into subordinate */


				EditMode = EDITING;

				if ( EditSequence() == EXIT_MENU )	{
					running = 0;
					break;
				}
				break;

			case SET_DATUM :	/* Get a datum point */

				GTMCLO(&daxmenu,&daxcell);
				EditMode = SETDATUM;
				if ( MxGetDatum(DATUM_LOCATE) == EXIT_MENU )
					running = 0;
				break;

			case PROPERTIES :	/*  Generic Properties */


					MENPOP(&PropertiesPopup,&ok);
					GTMCLO(&daxmenu,&daxcell);
					GETCELL(&daxcell,&daxmenu,&daxnum,&pnoun);


					if ( pnoun == 0 )
						break;

					MXPICKPROFILE(&mipp,&textmip,&seqnum,&option,&quit);

					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						running = 0;
						break;
					}
					if ( mipp )
						NcAttachProperty(mipp,pnoun);
					else
						DEPRNT(&NoProfileFound);		/* cannot find any thing */
					break;

			case OUTPUT_DATA:
				NcOutput();
				GTMCLO(&daxmenu,&daxcell);
				break;
				
			case 0:			/* break out of cell  */
				DEPRNT(&NoNcOption);
				break;

			default:
				GTMCLO(&daxmenu,&daxcell);
		}
		
	}

	GTCLRM(&VerbMenu);	/* clear verbmenu before exiting software */
	EditMode = MAINMODE;
	NcShut();				/* shut down all systems */

}


int
MxGetDatum(Mode)

      /* Description   :- Locate a datum for output only during output.
       * 
       * 
       *
       * Return status :- Menu Status Control
       *                  
       *   
       *                  
       * Notes         :- This routine will create a marker of standard 
       *                  number 7 which is an origin. This must
       *                  be located in layer misolayer to be a valid origin.
       *                  
       *                  
       *                  
       *
       */


int Mode;	/* <i> Mode indicates whether locate or load global dataums */


{
DAXCADINT nmipos;
DAXCADINT i;
DAXCADINT markermip;
DAXCADLOGICAL option;
DAXCADLOGICAL quit;
DAXCADREAL hpsx;
DAXCADREAL hpsy;
int st;
int exitcode;



	exitcode = MAIN_MENU;				/* override on option menu */
	markermip = 0;

	GETNMIPOS(&nmipos);	/* get DAXCAD nmipos */

	for ( i = 1; i<nmipos;i++)	{	/* loop tru and look for MISO layer */

		daxcadread(i,&st);

		if (st == 0 && 
			GlobalEntity.mip[DXSTATUS] == NORMAL &&
			GlobalEntity.mip[DXENTITY] == MARKER &&
			GlobalEntity.mip[DXFORM] == MARKER_ORIGIN &&
			GlobalEntity.mip[DXLAYER] ==  MisomexLayer )	{	/* element found on layer */
				
                  	markermip = i;	/* found an origin marker */
				break;
		}

	}


	if ( Mode == DATUM_RETURN ) {	/* just set current dataum */
		if ( markermip )	{
			MxDatum[0] = GlobalEntity.pdp[XVAL1];
			MxDatum[1] = GlobalEntity.pdp[YVAL1];
		}
		else	{
			MxDatum[0] = 0.0;
			MxDatum[1] = 0.0;
		}
		return exitcode;
	}
	MxMenu(MISOMEX_POINTS,MENU_ON);			/* turn on points mode */

	FINDP0(&IndicateDatum,&hpsx,&hpsy,&option,&quit);
        
	if  ( !option )	{	/* ok to create or modify marker */
	
		MxDatum[0] = hpsx;
		MxDatum[1] = hpsy;
	
		MXMARKER(&markermip,&hpsx,&hpsy);	/* either mod or create origin marker */

	}

	


	MxMenu(MISOMEX_POINTS,MENU_OFF);			/* turn on points mode */
	MxMenu(MISOMEX_MAIN,MENU_ON);				/* reload main menu*/

	if ( quit ) 
			exitcode = EXIT_MENU;
	if ( option )
			exitcode = MAIN_MENU;				/* override on option menu */


	return exitcode;							/* return to normal */


}



daxcadread(Mip,st)


      /* Description   :- Reads an element from DAXCADs database. It requires the master index
       *                  Pointer and will return the data in the entity structure.
       * 
       * 
       *
       * Return status :-
       *			-1	->	If the MIP number does not refer to a valid number 
       *                  	-2	->	Entity is deleted
       *   
       *                  
       * Notes         :-	This interfaces with a F77 routine to give all information to fill 
       *			The special structure for DAXCAD enetity
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */


DAXCADINT Mip;				/*	<i>	The Master index pointer */
int *st;				/*	<o>	return status */

{

DAXCADINT mip[MAXMIPBUFF];
DAXCADINT pdi[MAXPDIBUFF];
double pdp[MAXPDPBUFF];
Matrix m;
char text[DAXCADTEXT];
int i;
int status;

	*st = 0;

	FULLREAD(&Mip,mip,pdp,pdi,m,text,&status);

	if ( status ) {
		*st = -1;	/* cannot read this element */
		return;
	}

	if ( mip[DXSTATUS] != DELETED ) 	{
		for (i=0;i<MAXMIPBUFF;i++)
			GlobalEntity.mip[i] = mip[i];
		for (i=0;i<MAXPDPBUFF;i++)
			GlobalEntity.pdp[i] = pdp[i];
		for (i=0;i<MAXPDIBUFF;i++)
			GlobalEntity.pdi[i] = pdi[i];
		for ( i=0;i<9;i++)
			GlobalEntity.m[i] = m[i];	/* get matrix */
		
		if ( mip[DXTEXT] )	{	/* get text with this element */
			strcpy(GlobalEntity.text,text);
		}
	}
	else	{
		*st = -2;
	}

}
	
DisplayFile(Dfp,Mip,Ent,st)


      /* Description   :- This routine gets a master index and entity type from the current
       *		  Display file In daxcad.
       * 
       * 
       *
       * Return status :-
       *			-1	->	Dfp is out of range.
       *			-2	->	
       *                  
       *   
       *                  
       * Notes         :-	
       *                  If Mip is returned -ve then this means that element has been removed 
       *                  from the display file.
       *                  
       *                  
       *                  
       *
       */

DAXCADINT Dfp;	/*	<i>	The requested Display file pointer */
DAXCADINT *Mip;	/*	<o>	Master index pointer */
DAXCADINT *Ent;	/*	<o>	Entity type */
DAXCADINT *st;	/*	<o>	Status */
{

DAXCADINT dfp1;		/*	Display file pointer for daxcad */
DAXCADINT ent;		/* 	Entity type */
DAXCADINT mipp;		/*	master index pointer */

DAXCADLOGICAL	ok;	/*	Logical ok flag */

	dfp1 = Dfp;

	RDISPF(&dfp1,&ent,&mipp,&ok);


	if ( !ok ) {
		*st = -1;
		return;
	}

	*Mip = mipp;
	*Ent = ent;

	*st = 0;


}


MaxDisplay(MaxDfp,Cvpn)


      /* Description   :- Returns the current display file pointer for the current viewport in use.
       * 
       * 
       *
       * Return status :-NONE
       *                  
       *   
       *                  
       * Notes         :-The value returned is the next usable value. If empty it will b e a value 0
       *		 and thus cannot be used within C arrays. It is a logical value for the DAXCAD database.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */
DAXCADINT *MaxDfp;	/*	<o>	The maximum value for display file */
DAXCADINT *Cvpn;	/*	<o>	The current viwport number */

{
	
DAXCADINT dfp;		/* the display file pointer */
DAXCADINT cvpn;		/* current viewport number */

	MAXDISPLAY(&dfp,&cvpn);		/* call f77 routine to get values */

	*MaxDfp = dfp;
	*Cvpn = cvpn;

}




MxMenu(Menu,State)


      /* Description   :- Sets up current menu cells for MISOMEX plotter inteface
       * 
       * 
       *
       * Return status :-NONE
       *                  
       *   
       *                  
       * Notes         :-Place all new cells into this routine.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

int Menu;	/*	<i>	Menu number defined in include file */
int State;	/*	<i>	Controls all cells. 0 means allow header only */
{


	GTCLRM(&VerbMenu);	/* clear verbmenu first */

	switch(Menu)	{
		case MISOMEX_MAIN:			/* main menu here */
			GTDMHD(&MainHeader,&VerbMenu);
			if ( State )	{
				GTDMEN(&CreateSeq,&VerbMenu);
				GTDMEN(&EditSeq,&VerbMenu);
				GTDMEN(&SetDatum,&VerbMenu);
				GTDMEN(&OutPut,&VerbMenu);
				GTDMEN(&ViewProfiles,&VerbMenu);
				GTDMEN(&Properties,&VerbMenu);

				if ( CurrentMode == VIEW_LAYER )	{
					GTMCHI(&VerbMenu,&ProfileCell);	/* hightlight any cells */
				}
			}
			break;
		case MISOMEX_CREATE_SEQ:			/* main menu here */
			GTDMHD(&CreateHeader,&VerbMenu);
			if ( State ) 	{
				GTDMEN(&RestartProfile,&VerbMenu);
				GTDMEN(&AutoProfile,&VerbMenu);
				GTDMEN(&QuitCreate,&VerbMenu);
				GTDMEN(&DrillCreate,&VerbMenu);
				GTDMEN(&Accept,&VerbMenu);
				GTDMEN(&Cancel,&VerbMenu);
			}
			break;
		case MISOMEX_EDIT_SEQ:			/* Edit sequence menu here */

			GTDMHD(&EditHeader,&VerbMenu);
			if ( State ) 	{
				GTDMEN(&EditInsert,&VerbMenu);
				GTDMEN(&EditDelete,&VerbMenu);
				GTDMEN(&EditSwap,&VerbMenu);
				GTDMEN(&EditReseq,&VerbMenu);
				GTDMEN(&EditProp,&VerbMenu);
				GTDMEN(&EditQuit,&VerbMenu);
			}
			break;

		case MISOMEX_POINTS:			/* main menu here */
			if ( State ) {
				MNLPTS();			/* load points menu */
				GTDMEN(&QuitPoints,&VerbMenu);
			}
			else	{
				MNUPTS();			/* load points menu */
			}
			break;
		case MISOMEX_DRILL:			/* Drill pattern of windows */
			if ( State ) {
				MNLPTS();			/* load points menu */
				GTDMEN(&DrillWindow,&VerbMenu);
				GTDMEN(&QuitPoints,&VerbMenu);
			}
			else	{
				MNUPTS();			/* load points menu */
			}

	}

}


int
CreateSequence()


      /* Description   :- This routine will allow a single profile sequence of a defined finite number
       * 			of DAXCAD entites to be added together to form a special MISOMEX profile.
       * 
       *
       * Return status :-NONE
       *                  
       *   
       *                  
       * Notes         :-
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{

int daxmenu;	/* tcurs needed loacals */
int daxnum;
int daxcell;
DAXCADLOGICAL ok;
float hpsx,hpsy;		/* start and finish point values */
int running;
int i;
int st;
DAXCADINT tmip;
DAXCADINT p2,dfp;
int exitcode;
int seq;
int pnoun;

	exitcode = MAIN_MENU;

	running = 1;	/* set while flag loop control */


	MxMenu(MISOMEX_CREATE_SEQ,MENU_ON);	/* load sequence menu */

	while(running)	{

		GETPROFILE(&NumData,&st);		/* get profile needed */
		GETCELL(&daxcell,&daxmenu,&daxnum,&pnoun);
		if ( daxmenu != VerbMenu && daxmenu != WORKSPACE)	{
			exitcode = EXIT_MENU;
			break;
		}
		switch ( daxnum ) 	{

			case ACCEPT :			/* wants profile ok to get start and end points */

				if ( !NumData )	{		/* nothing defined */
					DEPRNT(&NoProfileDefined);
				}
				else	{			/* profile accepted ok */

					if ( GetNextSequence(&seq) == EXIT_MENU )	{	/* get the next available sequence number */
						running = 0;
						exitcode = EXIT_MENU;
						break;
					}
					NextSeq = seq;			/* set next sequence please */

					if ( BuildNewProfile() == EXIT_MENU ) 	{
						exitcode = EXIT_MENU;
						running = 0;
					}
					else	{
						MxMenu(MISOMEX_CREATE_SEQ,MENU_ON);	/* load sequence menu */
					}
				}
				ZSFLAG(&F77False,&ok);		/* clear profile from screen as well as in scracth file*/
				break;

			case QUIT_CREATE :		/* wants to quit this menu completely but not system out */
				running = 0;
				exitcode = MAIN_MENU;
				break;
			default :			/* option other means quit or worse */
				running = 0;
				exitcode = EXIT_MENU;
				break;
		}

		if ( !st )	{
			for ( i=1;i<=NumData;i++)	{
				RSCRF(&i,&tmip,&hpsx,&hpsy,&dfp,&p2);
				daxcadread(tmip,&st);
			}
		}
	}

		
	MxMenu(MISOMEX_MAIN,MENU_ON);	/* reload main menu*/
	ZSFLAG(&F77False,&ok);		/* clear profile from screen */

	return exitcode;

}

int
GetNextSequence(Seqnum)

      /* Description   :- This routine gets a next valid sequnce for CreateSequence 
       *                  routines. It uses the MxBuildList to obtain a 
       *                  valid set of profiles.
       *                
       *                
       *                
       * Return status :- If Seqnum is 0 then none avaliable.
       *                
       *                
       *                
       * Notes         :- This uses a simple algorithim to determine whether
       *                  the susbscript value of EditList is less than
       *                  the sequence value it contains
       *                  
       *                  
       *                  
       *
       */

int *Seqnum;		/*	<o>	Sequence number that is obtained */
{

int count;
int i;
int exitcode;

	*Seqnum = 0;		/* validate sequence number */
	count = 0;

	exitcode = MxBuildList();		/* build a new editing list unsorted */
	
	if ( exitcode == EXIT_MENU )	{
		return exitcode;
	}
		
	if ( !EditElements )	{		/* nothing to build so must return 1 */
		*Seqnum = 1;
		return SUCCESS;
	}

	MxMainSort();						/* sort the list */

	for (i=0; i<EditElements; i++)	{

		count++;					/* increment counter */

		if ( count < EditList[i].seqno ) {
			*Seqnum = count;
			break;
		}
	}

	if ( ! *Seqnum )	{
		*Seqnum = EditList[EditElements-1].seqno + 1;	/* must set up the number of elements */
	}

	free((char*)EditList);		/* free up editlist */

	return SUCCESS;

}


int
BuildNewProfile()

      /* Description   :- Build a new profile from data allready stored in the scratch file in DAXCAD
       * 
       * 
       *
       * Return status :-
       *                  
       *   
       *                  
       * Notes         :-
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{
DAXCADINT firstent;	/* Contain mip of first entity in profile list */
DAXCADINT lastent;
DAXCADINT data;
DAXCADINT dfp;
DAXCADINT p2;
DAXCADINT retp;
Marker markdef;		/* marker defintions */
float x;
float y;
int pos;
int endpos;
DAXCADLOGICAL option;
DAXCADLOGICAL quit;
int st;
int layer;
int seq;
int points;			/* the number of hit points needed */
float hpsx,hpsy;
float hpfx,hpfy;

double msang;
double meang;
int clocks;
int clocke;

	msang = 0.0;
	meang = 0.0;
	points = 0;			/* automatic selection being used */
	data = NumData;			/* save total number of entities for profile only */
	markdef.scalex = 1.0;		/* set scales in x direction */
	markdef.scaley = 1.0;		/* set scales in y direction */
	markdef.rotang = 0.0;		/* set rotation angle */
	markdef.font= 1;		/* set for solid code for drawing  */

	WORKLAYER(&markdef.layer);	/* set working layer */
	
	/* try to establish direction for markers */

	pos = 1;
	RSCRF(&pos,&firstent,&x,&y,&dfp,&p2);

	if ( data > 1 )	{

		pos = data;
		RSCRF(&pos,&lastent,&x,&y,&dfp,&p2);

	}
	if ( data == 1 )	{	/* determine type of sing entity */

		daxcadread(firstent,&st);

		MxMenu(MISOMEX_POINTS,MENU_ON);	/* load points menu */

		switch(GlobalEntity.mip[DXENTITY])	{

			case LINE :		/* a single line as part of profile */

				points = 2;
				FINDP0(&IndicateStart,&hpsx,&hpsy,&option,&quit);
				if ( option )
					return MAIN_MENU;
				if ( quit ) 
					return EXIT_MENU;


				FINDP0(&IndicateFinish,&hpfx,&hpfy,&option,&quit);
				if ( option )
					return MAIN_MENU;
				if ( quit ) 
					return EXIT_MENU;
				break;


			case ARC :		/* a single arc could be full circle or split */

				if ( FullCircle(firstent) )	{
					points = 1;
					FINDP0(&IndicateStart,&hpsx,&hpsy,&option,&quit);
					if ( option )
						return MAIN_MENU;
					if ( quit ) 
						return EXIT_MENU;


				}
				else	{
					points = 2;
					FINDP0(&IndicateStart,&hpsx,&hpsy,&option,&quit);
					if ( option )
						return MAIN_MENU;
					if ( quit ) 
						return EXIT_MENU;
					FINDP0(&IndicateFinish,&hpfx,&hpfy,&option,&quit);
					if ( option )
						return MAIN_MENU;
					if ( quit ) 
						return EXIT_MENU;
				}
				break;
		}

		MxMenu(MISOMEX_POINTS,MENU_OFF);	/* unload points menu */

	}
	else	{		/* automatic collection of start and end points */
		GetAutoEnd(&points,&hpsx,&hpsy,&hpfx,&hpfy);
	}
	
	/* create all markers now */
	
	pos = 1;endpos=data;
	MxMarkerAngles(pos,endpos,hpsx,hpsy,hpfx,hpfy,&msang,&meang,&clocks,&clocke);

	
	markdef.point.x = hpsx;
	markdef.point.y = hpsy;
	if ( points == 2 ) 	{
		markdef.rotang = msang;
		markdef.num = MARKER_START;
		markdef.direction = clocks;
		CreateMarker(&markdef,&retp);
		markdef.point.x = hpfx;
		markdef.point.y = hpfy;
		markdef.num = MARKER_END;
		markdef.rotang = meang;
		markdef.direction = clocke;
		CreateMarker(&markdef,&retp);
	}
	else	{
		markdef.rotang = msang;			/* direction */
		markdef.direction = clocks;
		markdef.num = MARKER_OPEN;
		CreateMarker(&markdef,&retp);
	}
	
	CreateTextSequence(&retp);		/* do seq number text */

	seq = NextSeq;
	layer = MisomexLayer;

	CREATEPROFILE(&seq,&layer);

	NextSeq++;				/* clear next sequence */
	
	return MAIN_MENU;


}

CreateTextSequence(Pointer)

      /* Description   :- Create a text string containing the sequence number
       * 
       * 
       *
       * Return status :-Only the newly created text
       *                  
       *   
       *                  
       * Notes         :- I have made the sequnce number postion start at the end/start of the
       *                  profile. This effectivly places the text to the right of the end marker.
       *                  The sequence number comes from the global value.
       *                  
       *                  
       *                  
       *
       */

DAXCADINT *Pointer;

{

double x,y;
float xp,yp;
char string[DAXLEN];
DAXCADINT tdfp;
DAXCADINT p2;
	
	xp = x = LastMarker.x + 10;
	yp = y = LastMarker.y;

	sprintf(string,"SEQ %d",NextSeq);

	CREATETEXT(&x,&y,string,Pointer);	/* creata and draw the text */

	SSFLAG(Pointer,&xp,&yp,&tdfp,&p2,&F77False);	/* add to main list for creating a group */

}

CreateMarker(MarkDef,Pointer)


      /* Description   :- This routine creates a marker at the specified location
       *                  for start and finish locations on a profile 
       * 
       * 
       *
       * Return status :-	-1	->	Cannot createa a new marker into database
       *                  
       *   
       *                  
       * Notes         :-	The Marker structure is a Marker buffer 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */
Marker *MarkDef;		/* structure containing marker definitions */
DAXCADINT *Pointer;		/* returned DAXCAD database pointer */


{
DAXCADLOGICAL ok;
DAXCADINT tlay;
DAXCADINT tfont;
DAXCADINT form;
DAXCADINT ent;
DAXCADINT retp;
DAXCADINT p2;
DAXCADINT tdfp;
DAXCADREAL x,y;
DAXCADREAL angle;
DAXCADREAL scalex;
DAXCADREAL scaley;

	LastMarker.x = x = MarkDef->point.x;
	LastMarker.y = y = MarkDef->point.y;

	scalex =  MarkDef->scalex;
	scaley =  MarkDef->scaley;
	angle = MarkDef->rotang;			/* set angle */
	form = MarkDef->num;
	tfont = MarkDef->font;
	tlay = MarkDef->layer;
	retp = 0;
	
	DEWC02(&x,&y,&angle,&scalex,&scaley,&form,&tfont,&tlay,&retp,&ok);	/* add marker to the data base */

	if ( !ok )	{

		return -1;
	}

	*Pointer = retp;			/* set retrning MIP for new marker */

	MXMODMRK(&retp,&MarkDef->direction);

	ent = MARKER;
	VALLDRW(&ent,&retp);

	SSFLAG(&retp,&x,&y,&tdfp,&p2,&F77False);	/* add to main list for creating a group */

	return 0;
	
}

int
FullCircle(Mip)


      /* Description   :- This routine determines whether the mip 
       *		will be a fill circle. 
       * 
       *
       * Return status :-
       *		1	->	Full circle
       *		2	->	Nothing
       *                  
       * Notes         :-NONE
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */
DAXCADINT Mip;	/*	<i>	The master index ref */

{

#define ARCTOL 1e-4
int st;

        daxcadread(Mip,&st);

	if ( GlobalEntity.mip[DXENTITY] != ARC )
		return 0;

	if ( fabs(GlobalEntity.pdp[SANG] ) < ARCTOL && fabs(GlobalEntity.pdp[EANG]-(M_PI*2.0)) < ARCTOL )
		return 1;
	else
		return 0;

}



EntityConnect(Mip1,Mip2,Connect1X,Connect1Y,Connect2X,Connect2Y,Connection)



      /* Description   :- Will return Connection as 1 if the 2 mip pointers connect
       * 		  the specified connection point will be passed back in ConnectX/Y
       * 
       *
       * Return status :-NONE
       *                  
       *   
       *                  
       * Notes         :-Currently only does LINE/LINE LINE/ARC ARC/ARC connections
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXCADINT Mip1;			/*	<i>	Mip pointer to 1st entity */
DAXCADINT Mip2;			/*	<i>	Mip pointer to 2nd entity */
double *Connect1X;		/*	<o>	Connection point X */
double *Connect1Y;		/*	<o>	Connection point Y */
double *Connect2X;		/*	<o>	Connection point X */
double *Connect2Y;		/*	<o>	Connection point Y */
int *Connection;		/*	<o>	Connection flag control */

{
int st;
struct EntityBuffer entity1;
struct EntityBuffer entity2;
double xs1,ys1;			/* first set of end points */
double xs2,ys2;
double xf1,yf1;			/* second set of end points */
double xf2,yf2;

	daxcadread(Mip1,&st);
	memcpy((char*)&entity1,(char*)&GlobalEntity,sizeof(struct EntityBuffer));
	daxcadread(Mip2,&st);
	memcpy((char*)&entity2,(char*)&GlobalEntity,sizeof(struct EntityBuffer));

	if ( entity1.mip[DXENTITY] == LINE && entity2.mip[DXENTITY] == LINE )	{

		xs1 = entity1.pdp[XVAL1];	/* start line */
		ys1 = entity1.pdp[YVAL1]; 
		xs2 = entity1.pdp[XVAL2];
		ys2 = entity1.pdp[YVAL2]; 

		xf1 = entity2.pdp[XVAL1];	/* second line */
		yf1 = entity2.pdp[YVAL1]; 
		xf2 = entity2.pdp[XVAL2];
		yf2 = entity2.pdp[YVAL2]; 
	}

	if ( entity1.mip[DXENTITY] == LINE && entity2.mip[DXENTITY] == ARC )    {

                xs1 = entity1.pdp[XVAL1];       /* start line */  
                ys1 = entity1.pdp[YVAL1]; 
                xs2 = entity1.pdp[XVAL2];
                ys2 = entity1.pdp[YVAL2];

		xf1 = entity2.pdp[CENX] + entity2.pdp[RADIUS]*cos(entity2.pdp[SANG]);
		yf1 = entity2.pdp[CENY] + entity2.pdp[RADIUS]*sin(entity2.pdp[SANG]);
		xf2 = entity2.pdp[CENX] + entity2.pdp[RADIUS]*cos(entity2.pdp[EANG]);
		yf2 = entity2.pdp[CENY] + entity2.pdp[RADIUS]*sin(entity2.pdp[EANG]);

	}
        if ( entity2.mip[DXENTITY] == LINE && entity1.mip[DXENTITY] == ARC )    {

                xs1 = entity2.pdp[XVAL1];       /* start line */
                ys1 = entity2.pdp[YVAL1];
                xs2 = entity2.pdp[XVAL2];
                ys2 = entity2.pdp[YVAL2];

                xf1 = entity1.pdp[CENX] + entity1.pdp[RADIUS]*cos(entity1.pdp[SANG]);
                yf1 = entity1.pdp[CENY] + entity1.pdp[RADIUS]*sin(entity1.pdp[SANG]);
                xf2 = entity1.pdp[CENX] + entity1.pdp[RADIUS]*cos(entity1.pdp[EANG]);
                yf2 = entity1.pdp[CENY] + entity1.pdp[RADIUS]*sin(entity1.pdp[EANG]);

        }

        if ( entity1.mip[DXENTITY] == ARC && entity2.mip[DXENTITY] == ARC )    {


                xs1 = entity2.pdp[CENX] + entity2.pdp[RADIUS]*cos(entity2.pdp[SANG]);
                ys1 = entity2.pdp[CENY] + entity2.pdp[RADIUS]*sin(entity2.pdp[SANG]);
                xs2 = entity2.pdp[CENX] + entity2.pdp[RADIUS]*cos(entity2.pdp[EANG]);
                ys2 = entity2.pdp[CENY] + entity2.pdp[RADIUS]*sin(entity2.pdp[EANG]);


                xf1 = entity1.pdp[CENX] + entity1.pdp[RADIUS]*cos(entity1.pdp[SANG]);
                yf1 = entity1.pdp[CENY] + entity1.pdp[RADIUS]*sin(entity1.pdp[SANG]);
                xf2 = entity1.pdp[CENX] + entity1.pdp[RADIUS]*cos(entity1.pdp[EANG]);
                yf2 = entity1.pdp[CENY] + entity1.pdp[RADIUS]*sin(entity1.pdp[EANG]);

        }

	*Connection = CONNECT_NONE;
	
	*Connect1X = (double)0.0;
	*Connect1Y = (double)0.0;
	*Connect2X = (double)0.0;
	*Connect2Y = (double)0.0;


	if ( PTOL(&xs1,&ys1,&xf1,&yf1) || PTOL(&xs1,&ys1,&xf2,&yf2))	{
		*Connection = CONNECT_ONE;
		*Connect1X = xs1;
		*Connect1Y = ys1;
	}

        if ( PTOL(&xs2,&ys2,&xf1,&yf1) || PTOL(&xs2,&ys2,&xf2,&yf2))	{
		if ( *Connection == CONNECT_ONE)	{
	                *Connection = CONNECT_TWO;
	                *Connect2X = xs2;
	                *Connect2Y = ys2;
		}
		else	{
	                *Connection = CONNECT_ONE;
	                *Connect1X = xs2;
	                *Connect1Y = ys2;
		}
        }

	

}



GetAutoEnd(Points,Hpsx,Hpsy,Hpfx,Hpfy)


      /* Description   :- Gets the end points of a selected profile. 
       * 
       * 
       *
       * Return status :-
       *			-1	->	Profile error mismatch element missing
       *                  	-2	->	Only one element
       *   
       *                  
       * Notes         :-
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

int *Points;		/*	<o>	The number of points used */
float *Hpsx;		/*	<o>	End Points start and finish */
float *Hpsy;            /*      <o>     End Points start and finish */
float *Hpfx;            /*      <o>     End Points start and finish */
float *Hpfy;            /*      <o>     End Points start and finish */

{
DAXCADINT firstent;
DAXCADINT lastent;
DAXCADINT nextent;
DAXCADINT prevent;
DAXCADINT dfp,p2;
DAXCADINT ent1;
DAXCADINT ent2;
int pos;
int st;
float x,y;
double cx,cy;
double cx2,cy2;
double xs,ys;
double xf,yf;
int connect;

	pos = 1;
        RSCRF(&pos,&firstent,&x,&y,&dfp,&p2);
	daxcadread(firstent,&st);
	ent1 = GlobalEntity.mip[DXENTITY];

	pos = NumData;
	RSCRF(&pos,&lastent,&x,&y,&dfp,&p2);
	daxcadread(lastent,&st);
	ent2 = GlobalEntity.mip[DXENTITY];


	if ( ent1 == LINE && ent2 == LINE && NumData == 2 )	{	/* special case cannot connect 2 lines */
		connect = CONNECT_NONE;
	}
	else	{
		EntityConnect(firstent,lastent,&cx,&cy,&cx2,&cy2,&connect);
	}

	if ( connect == CONNECT_ONE || connect == CONNECT_TWO) 	{
		*Points = 1;
	}
	else	{
		*Points = 2;
	}
        
	daxcadread(firstent,&st);

	switch( GlobalEntity.mip[DXENTITY] )      {

		case LINE :

			*Hpsx = GlobalEntity.pdp[XVAL1];
			*Hpsy = GlobalEntity.pdp[YVAL1];

			break;

		case ARC :

			xs = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[SANG]);
			ys = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[SANG]);
			xf = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[EANG]);
			yf = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[EANG]);

			pos = 2;
			RSCRF(&pos,&nextent,&x,&y,&dfp,&p2);
			EntityConnect(firstent,nextent,&cx,&cy,&cx2,&cy2,&connect);

			if ( PTOL(&xf,&yf,&cx,&cy)) {

				*Hpsx = xs;
				*Hpsy = ys;
			}
			else	{
				*Hpsx = xf;
				*Hpsy = yf;
			}
			break;
					
	}

	if ( *Points == 1 )
		return;			/* only one point required */


	daxcadread(lastent,&st);

	switch(GlobalEntity.mip[DXENTITY])      {

		case LINE :

			*Hpfx = GlobalEntity.pdp[XVAL2];
			*Hpfy = GlobalEntity.pdp[YVAL2];
			break;

		case ARC :

			xs = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[SANG]);
			ys = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[SANG]);
			xf = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[EANG]);
			yf = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[EANG]);

			pos = NumData-1;
			RSCRF(&pos,&prevent,&x,&y,&dfp,&p2);
			EntityConnect(lastent,prevent,&cx,&cy,&cx2,&cy2,&connect);

			if ( PTOL(&xf,&yf,&cx,&cy)) {

				*Hpfx = xs;
				*Hpfy = ys;
			}
			else	{
				*Hpfx = xf;
				*Hpfy = yf;
			}
			break;
					
	}

}

int
EditSequence()


      /* Description   :- This routine will handle all editing functions including bringing
       *                  up menus
       * 
       *
       * Return status :- Returns the menu code status eithe back to main menu or out exiting
       *                  
       *   
       *                  
       * Notes         :- Must build an initial list of 2 columns containing sequnce number and
       *                  mip code. This will be manupulated and if required saved on exiting from
       *                  the edit sequence command
       *                  
       *                  
       *                  
       *
       */

/*	NO ARGUMENTS */

{

DAXCADINT mipp;
DAXCADINT mip1;			/* swapped mips */
DAXCADINT mip2;
DAXCADINT textmip;
DAXCADINT textmip2;
DAXCADLOGICAL ok;
DAXCADLOGICAL option;
DAXCADLOGICAL quit;
int seqnum;
int use_tcurs;

int daxmenu;			/* GUI interfaces */
int daxcell;
int daxnum;

int exitcode;
int ch;
float hpx;
float hpy;

int running;			/* main loop control flag */
int pnoun;


	use_tcurs = 1;
	running = 1;
	
	if ( exitcode = MxBuildList() )	{	/* build a new editing list unsorted */
		return exitcode;
	}

	MxMainSort();				/* sort list now */

	exitcode = MAIN_MENU;

	MxMenu(MISOMEX_EDIT_SEQ,MENU_ON);		/* display create menu */

	while (running)	{	/* main GUI loop */

		if ( use_tcurs)	{
			TCURS(&ch,&hpx,&hpy);           /* get a cursor hit from the screen */
		}
		else
			use_tcurs = 1;

		GETCELL(&daxcell,&daxmenu,&daxnum,&pnoun);

		if ( daxmenu != VERB_MENU && daxmenu != WORKSPACE )	{	/* break out of This menu strucure */
			running = 0;
			daxnum = 0;
			exitcode = EXIT_MENU;
		}
		if ( daxmenu == VerbMenu ) 	{

			switch(daxnum)	{	/* get options */

				case EDIT_QUIT :			/* quit menu */

					exitcode = MAIN_MENU;
					running = 0;
					break;

				case EDIT_RESEQ :			/* Resequence all values */

					if ( CONFRM() == F77True )	{
						MxResequence();		/* do reseq */
						MxWriteChanges();	/* shwo changes to user */
					}

					break;


				case EDIT_DELETE :			/* Delete a profile  */

					MXPICKPROFILE(&mipp,&textmip,&seqnum,&option,&quit);

					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						exitcode = EXIT_MENU;
						running = 0;
						break;
					}
					if ( mipp )	{	/* has he picked a profile for deleteion */

						MxMarkProfile(textmip,1);
						if ( CONFRM() == F77True )	{

							MxMarkProfile(textmip,0);
							MxDelete(mipp);		/* do the deletion */
							MxWriteChanges();	/* show changes to user */

						}
						else	{
							MxMarkProfile(textmip,0);	/* mark it back in */
						}

					}
					else	{

						DEPRNT(&NoProfileFound);		/* cannot find any thing */

					}
					break;



				case EDIT_INSERT :			/* isert a sequence hole menu */

					MXPICKPROFILE(&mipp,&textmip,&seqnum,&option,&quit);
					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						exitcode = EXIT_MENU;
						running = 0;
						break;
					}

					if ( mipp )	{	/* has he picked */

						MxInsert(mipp);	/* recalculate editlist */
						MxWriteChanges();	/* shwo changes to user */

					}
					else	{

						DEPRNT(&NoProfileFound);		/* cannot find any thing */

					}

					break;


				case EDIT_SWAP :			/* Swap 2 profiles */

					MXPICKPROFILE(&mip1,&textmip,&seqnum,&option,&quit);
					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						exitcode = EXIT_MENU;
						running = 0;
						break;
					}

					if ( !mip1 )	{	/* has he picked */
						DEPRNT(&NoProfileFound);		/* cannot find any thing */
						break;
					}

					MxMarkProfile(textmip,1);

					MXPICKPROFILE(&mip2,&textmip2,&seqnum,&option,&quit);

					MxMarkProfile(textmip,0);

					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						exitcode = EXIT_MENU;
						running = 0;
						break;
					}

					if ( !mip2 )	{	/* has he picked */
						DEPRNT(&NoProfileFound);		/* cannot find any thing */
						break;
					}

					MxSwap(mip1,mip2);
					MxWriteChanges();	/* show changes to user */
					break;
				
				case EDIT_PROP :		/* change some properyy */

					MENPOP(&PropertiesEditPopup,&ok);
					GTMCLO(&daxmenu,&daxcell);
					GETCELL(&daxcell,&daxmenu,&daxnum,&pnoun);


					if ( pnoun == 0 )
						break;

					MXPICKPROFILE(&mipp,&textmip,&seqnum,&option,&quit);

					if ( option )	{
						use_tcurs = 0;
						break;
					}
					if ( quit )	{
						running = 0;
						break;
					}
					if ( mipp )
						NcEditProperty(mipp,pnoun);
					else
						DEPRNT(&NoProfileFound);		/* cannot find any thing */
					break;


				default :

					break;

			}
			if ( daxnum )	{
				GTMCLO(&VerbMenu,&daxcell);	/* clear  before returning */
			}
		}
		else	{
			DEPRNT(&NoNcOption);
		}

		daxcell = 0;
		daxmenu = 0;

	}

	free((char*)EditList);			/* free up the list */

	MxMenu(MISOMEX_MAIN,MENU_ON);   /* reload main menu*/
	ZSFLAG(&F77False,&ok);          /* clear profile from screen */

	return exitcode;		/* what menu to go to */



}


MxMainSort()

      /* Description   :- Sorts eelements in EditList into order by seqno
       * 
       * 
       *
       * Return status :-  -1	If nothing in list
       *                  
       *   
       *                  
       * Notes         :- Simple bubble sort here based on global EditElements.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

/*	NO ARGUMENTS */
{
int i,j;
int n1,n2;	/* local sequence numbers */

	for ( i=0;i<EditElements;i++)	{
		for ( j=0;j<EditElements-1;j++)	{

			n1 = EditList[j].seqno;
			n2 = EditList[j+1].seqno;
			if ( n1 > n2 )	{
				MxSortSwap(j,(int)j+1);
			}
		}
	}
}

MxSortSwap(Elem1,Elem2)

      /* Description   :- Swaps 2 elements of Editlist
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- PArt of bubble sort mechanisim
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

int Elem1;	/* <i>	First elements to be swapped */
int Elem2;	/* <i>	Second elements to be swapped */

{

int seqno;
int mipp;

	seqno = EditList[Elem1].seqno;	/* save first elemnts */
	mipp = EditList[Elem1].mipp;

	EditList[Elem1].seqno = EditList[Elem2].seqno;	/* swap */
	EditList[Elem1].mipp = EditList[Elem2].mipp;

	EditList[Elem2].seqno = seqno;				/* recover */
	EditList[Elem2].mipp = mipp;

}

int
MxBuildList()

      /* Description   :- Builds Edit LIst for editiing of profiles list
       * 
       * 
       *
       * Return status :- -1 If no profiles
       *                  
       *   
       *                  
       * Notes         :- This is used for editing functions as well as finding next sequences etc.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{


DAXCADINT nmipos;		/* master index position */
DAXCADINT i;
int elements;
int st;
int pass;
unsigned bytes;

	EditList = NULL;			/* Pointer to the list itself */
	EditMalloc = 0;				/* number of currentyly allocated elements ( inc only ) */
	EditElements= 0;			/* global number of elements being edited */

	elements = 0;

	GETNMIPOS(&nmipos);	/* get DAXCAD nmipos */

	for ( pass=0; pass<2; pass++)	{

		elements = 0;

		for ( i = 1; i<nmipos;i++)	{	/* loop tru and look for MISO layer */

			daxcadread(i,&st);

			if (	st == 0 &&
				GlobalEntity.mip[DXSTATUS] == NORMAL && 
				GlobalEntity.mip[DXENTITY] == GROUP &&
				GlobalEntity.mip[DXFORM] &&
				GlobalEntity.mip[DXLAYER] ==  MisomexLayer )	{	/* element found on layer */

				elements++;	/* increment elements found */

				if ( pass == 1 )	{	/* Add elementst to list */

					EditList[elements-1].mipp = i;				/* store values */
					EditList[elements-1].seqno = GlobalEntity.mip[DXFORM];
				}
			}
			
		}

		if ( pass == 0 )	{	/* counted all allocate an edit list */

			if ( !elements )	{		/* error no profiles on layer */

				if ( EditMode == EDITING )
					DEPRNT(&NoProfileDefined);			/* change this eror prompot */

				return MAIN_MENU;
			}

			bytes = elements*sizeof(EditProfile);      		/* get bytes */
			EditList = (EditProfile*)malloc(bytes);

			if ( !EditList )	{

                  RelDaxDisplay();
				perror("[EditSequence] Cannot alloc memory EditList ");
				AqDaxDisplay();
				DEPRNT(&NoAlloc);
				return EXIT_MENU;
			}
		}
	}

		

	EditElements = elements;		/* set global values */
	EditMalloc = elements;			/* should be same initialy */
	return SUCCESS;				/* return with succes code */

}

MxWriteChanges()

      /* Description   :- Write all changes to DAXCAD database from EditList global
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- Takes each element from EditList in ascending order 
       *                  and write new sequence number againts MIP number
       *                  calls a bit of F77 to integerate with DAXCAD database
       *                  
       *                  
       *                  
       *
       */

/* NO ARGUMENTS */


{

int i;
DAXCADINT mipp;
int seqno;

	for ( i=0;i<EditElements;i++)	{	/* loop round list */

		mipp = EditList[i].mipp;	/* mip and seq number */
		seqno = EditList[i].seqno;

		if ( seqno )	{	/* delete status */
			MXWRITECHANGE(&mipp,&seqno);	/* call F77 integeragtion routine */
        
		}
	}
}






int
MxInsert(MipInsert)

      /* Description   :- Inserst a sequence number at the specified mip
       * 
       * 
       *
       * Return status :- -1 if MipInsert not found in Current EditList
       *                  
       *   
       *                  
       * Notes         :- This routine looks for the mip requested nameley MipInsert
       *                  and increments all sequence numbers from that point onward
       *                  
       *                  
       *                  
       *                  
       *
       */
DAXCADINT MipInsert;	/*	<i) The group mip of the picked profile */

{
int increment;
int i;
	increment = 0;

	for ( i=0;i<EditElements;i++)	{

		if ( EditList[i].mipp )	{	/* check delete status */

			if ( increment )	{

				EditList[i].seqno = EditList[i].seqno + 1;
			}
			else	{

				if ( EditList[i].mipp == MipInsert )	{
					EditList[i].seqno = EditList[i].seqno + 1;
					increment = 1;
				}
			}
			
		}
	}

	if ( !increment ) 
		return -1;
	else
		return SUCCESS;
}


int
MxSwap(MipSwap1,MipSwap2)

      /* Description   :- Swaps 2 Mips around. Must be supplied ( MXPRICKPROFILE )
       * 
       * 
       *
       * Return status :- -1 if MipSwapx not found in Current EditList
       *                  
       *   
       *                  
       * Notes         :- Simple swap mechanisim. Only swaps Sequence numbers
       *                  not mips as in MxMainSort
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXCADINT MipSwap1;	/*	<i) The 1st group mip of the picked profile */
DAXCADINT MipSwap2;	/*	<i) The 2nd group mip of the picked profile */

{
int found1;		/* located first elem */
int found2;		/* located 2nd element */
int seq;		/* temp sequence number */
int i;


	found1 = -1;	/* initalise */
	found2 = -1;

	for ( i=0;i<EditElements;i++)	{

		if ( EditList[i].mipp )	{	/* check delete status */

			if ( MipSwap1 == EditList[i].mipp && found1 == -1 )	{
				found1 = i;
			}
			if ( MipSwap2 == EditList[i].mipp && found2 == -1)	{
				found2 = i;
			}
			
		}
	}

	if ( found1 == -1 && found2 == -1  ) 	/* found nothing to swap */
		return -1;



	seq = EditList[found1].seqno;			/* swap */
	EditList[found1].seqno = EditList[found2].seqno;
	EditList[found2].seqno = seq;

	return SUCCESS;

}


int 
MxDelete(MipDelete)

      /* Description   :-Deletes the group from the sequence list.
       *                
       *                
       *                
       * Return status :-  -1 if not found
       *                
       *                
       *                
       * Notes         :- This routine will DAXCAD explode the group and
       *                  return the geometry to the layer from whence it came
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXCADINT MipDelete;	/*	<i> The group mip of the picked profile */

{


int i;

	for ( i=0;i<EditElements;i++)	{

		if ( EditList[i].mipp == MipDelete )	{
			MXDELETE(&MipDelete);
			EditList[i].seqno = 0;	/* set deleted */
			EditList[i].mipp = 0;	/* set deleted */
			return SUCCESS;
		}
			
	}

	return -1;

}

MxResequence()

      /* Description   :- Resequence the current edit list
       *               
       *                
       *                
       * Return status :- NONE
       *                
       *                
       *                
       * Notes         :- Simply resets sequence numbers to that of 
       *                  the a incrementel counter
       *                  
       *                  
       *                  
       *                  
       *
       */

/*	NO ARGS		*/

{


int i;
int count;

	count =1;
	for ( i=0;i<EditElements;i++)	{

		if ( EditList[i].seqno )	{
			EditList[i].seqno = count++;	/* set count number */
		}

	}

}


MxMarkProfile(Mipp,State)

      /* Description   :- Marks a profile while in use by highlighting text
       *                  of that profile
       * 
       *
       * Return status :-NONE 
       *                  
       *   
       *                  
       * Notes         :- Simply for knowing wich profile has been hit
       *                  we mark the text cos its a better bet for controlling
       *                  
       *                  
       *                  
       *                  
       *
       */
DAXCADINT Mipp;		/*	<i>	Text master index for marking */
int State;		/*	<i>	Marker Up or Down */

{

int ent;

	if ( State )	{

		LABFLG(&Mipp);		/* mark for use */
		ALLDRW(&ent,&Mipp);
	}
	else	{
		LABNFG(&Mipp);		/* unmark it */
		ALLDRW(&ent,&Mipp);
	}
		

}

MxMarkerAngles(StartPos,EndPos,Spx,Spy,Fpx,Fpy,SAngle,EAngle,ClockS,ClockE)

      /* Description   :- Calculates the start and end rotation angles for each marker
       * 		  on a sequence. The direction of the arc either CW or CCW
       *                  is returned.
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- Should be automatic calculations for any combinations of 
       *                  elements
       *                  
       *                  
       *                  <<<<----EXPLANATION of angle calcs---->>>>
       *                  
       *                  LINE            p2 (End)
       *                                 *
       *                                *
       *                               *
       *                              *
       *                             *  angle for marker
       *                            *   
       *                           *---------
       *                           p1 ( Start )
       *                  
       *                  
       *                  
       *                                  *
       *                                    *  
       *                                     *
       *                                      *
       *                       O               *   ARC
       *                        #   Vector 1   *  /
       *                         # /           *
       *                          #            *
       *                           #          *
       *                            #        *          Vector 1 is from Start or end point on arc
       *                             #      *           Vector 2 is perpendicular to vector 1 thru start/end point
       *                              # * *             Calculate angle of perp vector
       *              Vector 2       ##  
       *                      \    ##
       *                         ##
       *                       ##
       *                  
       *                             
       *  
      */                      

int StartPos;	/*	<i>	Starting postion in DAXCAD  Display file */
int EndPos;	/*	<i>	end postion in DAXCAD  Display file */
float Spx;	/*	<i>	Starting point for a single ent only */
float Spy;	/*	<i>	Starting point for a single ent only */
float Fpx;	/*	<i>	Finish point for a single ent only */
float Fpy;	/*	<i>	Finish point for a single ent only */
double *SAngle;	/*	<o>	Angle in radians of starting marker */
double *EAngle;	/*	<o>	Angle in radians of starting marker */
int *ClockS;	/*	<o>	Whether the start arc is clockwise 0 CW 1 CCW */
int *ClockE;	/*	<o>	Whether the end arc is clockwise 0 CW 1 CCW */
{

DAXCADINT firstent;		/* MIP of first entity */
DAXCADINT lastent;		/* MIP of second entity */
DAXCADINT nextent;		/* MIP of next entity in scratch file */
DAXCADINT dfp;			/* Display file pointer */
DAXCADINT pp2;			/* Spare on scratch file */
DAXCADINT ent1;
DAXCADINT ent2;
float x;			/* hit point on entity */
float y;

double cx;			/* connection points */
double cy;
double cx2;			/* connection points */
double cy2;

double DCANG();			/* DAXCAD function to return angle of a line */
double DVN0D6();		/* function to return angle between 2 vectors */

double xp,yp;			/* point to use for vector generation */
double xp1,yp1;			/* point to use for vector generation */
double xs,ys;
double xf,yf;
double l1,l2,l3;		/* standard vector */
double p1,p2,p3;		/* Perp vector */
double cenx,ceny;		/* centre point */
double radius;			/* radius */

int pos;
int profconnect;
int connect;
int st;


	if ( StartPos < EndPos )	{	/* check for single entity */

		RSCRF(&StartPos,&firstent,&x,&y,&dfp,&pp2);	/* obtain MIP values */
		RSCRF(&EndPos,&lastent,&x,&y,&dfp,&pp2);

		daxcadread(firstent,&st);
		ent1 = GlobalEntity.mip[DXENTITY];
		daxcadread(lastent,&st);
		ent2 = GlobalEntity.mip[DXENTITY];

				
		if ( ent1 == LINE && ent2 == LINE && NumData == 2 )	{	/* special case cannot connect 2 lines */
			profconnect = CONNECT_NONE;
		}
		else	{
			EntityConnect(firstent,lastent,&cx,&cy,&cx2,&cy2,&profconnect);
		}

		daxcadread(firstent,&st);

		switch ( GlobalEntity.mip[DXENTITY] )	{

			case LINE :		/* Line is first element */

				*SAngle = DCANG(&GlobalEntity.pdp[XVAL1],	/* straight angle calc */
						&GlobalEntity.pdp[YVAL1],
						&GlobalEntity.pdp[XVAL2],
						&GlobalEntity.pdp[YVAL2] );
				break;

			case ARC :		/* Arc is first element */

				
				xs = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[SANG]);
				ys = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[SANG]);
				xf = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[EANG]);
				yf = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[EANG]);

				cenx = GlobalEntity.pdp[CENX];
				ceny = GlobalEntity.pdp[CENY];
				radius = GlobalEntity.pdp[RADIUS];

				pos = 2;					/* check next ent for connection */
				RSCRF(&pos,&nextent,&x,&y,&dfp,&pp2);			
				EntityConnect(firstent,nextent,&cx,&cy,&cx2,&cy2,&connect);


				if ( PTOL(&xf,&yf,&cx,&cy)) {

					xp = xs;
					yp = ys;
					*ClockS = 1;	/* CCW default direction established */
					DCVL14(&xp,&yp,&cenx,&ceny,&l1,&l2,&l3); /* gen vector along radis */
				}
				else	{
					xp = xf;
					yp = yf;
					*ClockS = 0;	/* CW  direction established */
					DCVL14(&cenx,&ceny,&xp,&yp,&l1,&l2,&l3); /* gen vector along radis */
				}

				DVV0L6 (&l1,&l2,&l3,&xp,&yp,&p1,&p2,&p3);	/* perp vector */
				DVC0P4(&xp,&yp,&radius,&p1,&p2,&p3,&xp1,&yp1);	/* get a new point along perp vector of size radius */

				*SAngle = DCANG(&xp,&yp,&xp1,&yp1);

				break;

		}


		if ( !profconnect ) {

			daxcadread(lastent,&st);

			switch ( GlobalEntity.mip[DXENTITY] )	{

				case LINE :		/* Line is first element */

					*EAngle = DCANG(&GlobalEntity.pdp[XVAL1],
							&GlobalEntity.pdp[YVAL1],
							&GlobalEntity.pdp[XVAL2],
							&GlobalEntity.pdp[YVAL2] );
					break;

				case ARC :		/* Arc is first element */

				xs = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[SANG]);
				ys = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[SANG]);
				xf = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[EANG]);
				yf = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[EANG]);

				cenx = GlobalEntity.pdp[CENX];
				ceny = GlobalEntity.pdp[CENY];
				radius = GlobalEntity.pdp[RADIUS];

				pos = EndPos-1;					/* check prev ent for connection */
				RSCRF(&pos,&nextent,&x,&y,&dfp,&pp2);
				EntityConnect(lastent,nextent,&cx,&cy,&cx2,&cy2,&connect);


				if ( PTOL(&xf,&yf,&cx,&cy)) {

					xp = xs;
					yp = ys;
					*ClockE = 0;	/* CW default direction established */
					DCVL14(&cenx,&ceny,&xp,&yp,&l1,&l2,&l3); /* gen vector along radis */
				}
				else	{
					xp = xf;
					yp = yf;
					*ClockE = 1;	/* CCW  direction established */
					DCVL14(&xp,&yp,&cenx,&ceny,&l1,&l2,&l3); /* gen vector along radis */
				}

				DVV0L6 (&l1,&l2,&l3,&xp,&yp,&p1,&p2,&p3);	/* perp vector */
				DVC0P4(&xp,&yp,&radius,&p1,&p2,&p3,&xp1,&yp1);

				*EAngle = DCANG(&xp,&yp,&xp1,&yp1);

				break;
			}
		}
	}
	else	{		/* only one piece of data thus use hit points */

		RSCRF(&StartPos,&firstent,&x,&y,&dfp,&pp2);	/* obtain MIP values */

		daxcadread(firstent,&st);

		switch ( GlobalEntity.mip[DXENTITY] )	{

			case LINE :		/* Line is only element */

				xs = Spx;	/* hit points are float ( DAXCAD ) */
				ys = Spy;
				xf = Fpx;
				yf = Fpy;

				*SAngle = DCANG(&xs,&ys,&xf,&yf);	/* calculate 2 angles based on hit point */
				*EAngle = DCANG(&xf,&yf,&xs,&ys);

				break;

			case ARC :		/* Arc is first element */

				xs = GlobalEntity.pdp[CENX]+GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[SANG]);
				ys = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[SANG]);
				xf = GlobalEntity.pdp[CENX] + GlobalEntity.pdp[RADIUS]*cos(GlobalEntity.pdp[EANG]);
				yf = GlobalEntity.pdp[CENY] + GlobalEntity.pdp[RADIUS]*sin(GlobalEntity.pdp[EANG]);
				cenx = GlobalEntity.pdp[CENX];
				ceny = GlobalEntity.pdp[CENY];
				radius = GlobalEntity.pdp[RADIUS];

				xp = Spx;yp=Spy;
				if ( PTOL(&xs,&ys,&xp,&yp)) {

					*ClockS = 1;	/* CCW default direction established */
					*ClockE = 1;
					xp = xs;
					yp = ys;
				}
				else	{
					*ClockS = 0;	/* CW  direction established */
					*ClockE = 0;
					xp = xf;
					yp = yf;
				}
				if ( FullCircle(firstent) )	{	/* full circle */
					*ClockS = 1;
				}

				if ( *ClockS == 1 )	{
					DCVL14(&xp,&yp,&cenx,&ceny,&l1,&l2,&l3); /* gen vector along radis */
				}
				else	{
					DCVL14(&cenx,&ceny,&xp,&yp,&l1,&l2,&l3); /* gen vector along radis */
				}
				DVV0L6 (&l1,&l2,&l3,&xp,&yp,&p1,&p2,&p3);	/* perp vector */
				DVC0P4(&xp,&yp,&radius,&p1,&p2,&p3,&xp1,&yp1);
				*SAngle = DCANG(&xp,&yp,&xp1,&yp1);

				if ( *ClockE ) {	/* end is also CCW */

					xp = xf;
					yp = yf;
				}
				else	{
					xp = xs;
					yp = ys;
				}
				if ( *ClockE == 1 )	{
					DCVL14(&xp,&yp,&cenx,&ceny,&l1,&l2,&l3); /* gen vector along radis */
				}
				else	{
					DCVL14(&cenx,&ceny,&xp,&yp,&l1,&l2,&l3); /* gen vector along radis */
				}
				DVV0L6 (&l1,&l2,&l3,&xp,&yp,&p1,&p2,&p3);	/* perp vector */
				DVC0P4(&xp,&yp,&radius,&p1,&p2,&p3,&xp1,&yp1);
				*EAngle = DCANG(&xp,&yp,&xp1,&yp1);

				break;
		}
	}




}





int
NcInitProperties()

      /* Description   :- Initialise NC propeties by reading DAXCAD_DEFAULTS file
       *                  and setting the proerties found in that
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- We do this by setiing in the defaults file a value for 
       *                  The number od properties and then getting all values like PROP1 PROP2 etc
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{

int st;
int numprop;
int i;
char prop[MAXPROPNAME];

char pname[81];
char prompt[81];
int type;
int noun;
DAXCADINT mip;


	getdefaults("NCPROPS",DAX_INTEGER,&numprop,&st);
	
	if ( st )	{

		DEPRNT(&NoPropsSet);
		printf("[NcInitProperties] Error in reading defaults file");
		return -1;
	}

	
	for ( i=0; i<numprop;i++)	{	/* get proerties */

		
		sprintf(prop,"PROPNAME%.d",(int)i+1);
		getdefaults(prop,DAX_STRING,pname,&st);

		sprintf(prop,"PROPPROMPT%.d",(int)i+1);
		getdefaults(prop,DAX_STRING,prompt,&st);

		sprintf(prop,"PROPTYPE%.d",(int)i+1);
		getdefaults(prop,DAX_INTEGER,&type,&st);

		sprintf(prop,"PROPNOUN%.d",(int)i+1);
		getdefaults(prop,DAX_INTEGER,&noun,&st);


		NCPROP(pname,prompt,&type,&mip,&st);

		NcStdProps[i] = (NcProperty*)malloc(sizeof(NcProperty));
		strcpy(NcStdProps[i]->name,pname);
		strcpy(NcStdProps[i]->prompt,prompt);
		NcStdProps[i]->type = type;
		NcStdProps[i]->mip = mip;
		NcStdProps[i]->noun = noun;

		NcPropCount++;
		
	}
	return SUCCESS;
}


NcAttachProperty(Mipp,Noun)

      /* Description   :-Attach a property onto a Profile
       *                
       *                
       *                
       * Return status :-NONE
       *                
       *                
       *                
       * Notes         :- We must use the popup to control all 
       *                  ops on this one. I have used 2 control
       *                  variables to indicate which cell 
       *                  is for measuring a profile and which is for
       *                  an undefined proerty of the users choice
       *                  We use existing prop code for this. The 
       *                  popup should be constructed thus
       *
       *                   --------------- 
       *                  |Properties     |
       *                  |Defined        |#
       *                  |Defined        |#
       *                  |Defined        |#
       *                  | ....          |#
       *                  |Other Property |#
       *                  |MEASURE        |#
       *                   --------------- #
       *                    ################
       *
       *
       */

DAXCADINT Mipp;		/*	<i>	Group master index */
int Noun;		/*	<i>	DAXCAD popup noun number  */

{

int measure;
int other;
char name[81];
DAXCADINT pmip;
int st;
int mode;
int i;


	mode = 0;
	getdefaults("NCPROPOTHER",DAX_INTEGER,&other,&st);
	getdefaults("NCPROPMEASURE",DAX_INTEGER,&measure,&st);

	
	if ( Noun == other )	{	/* user defined property */

		NCATTACH(&mode,name,&F77True,&Mipp,&pmip,&st);
	}

	if ( Noun == measure )	{

		mode = 1;
		NCATTACH(&mode,name,&F77False,&Mipp,&pmip,&st);

	}

	else	{		/* must be an existing prop name */


		for ( i=0;i<NcPropCount;i++)	{


			if ( Noun == NcStdProps[i]->noun )	{	/* got noun that relates to cell hit */
				strcpy(name,NcStdProps[i]->name);
				pmip = NcStdProps[i]->mip;

				NCATTACH(&mode,name,&F77False,&Mipp,&pmip,&st);	/* attach the property */
				break;
			}

		}


	}

}




NcEditProperty(Mipp,Noun)

      /* Description   :-Edit a property onto a Profile
       *                
       *                
       *                
       * Return status :-NONE
       *                
       *                
       *                
       * Notes         :- Sames method as NcAttcahProperty
       *                  
       *                 
       *                
       *               
       *              
       *             
       *
       *
       */

DAXCADINT Mipp;		/*	<i>	Group master index */
int Noun;		/*	<i>	DAXCAD popup noun number  */

{

char name[81];
int i;

	for ( i=0;i<NcPropCount;i++)	{


		if ( Noun == NcStdProps[i]->noun )	{	/* got noun that relates to cell hit */
			strcpy(name,NcStdProps[i]->name);

			NCEDIT(&Mipp,name);	/* Edit the property */
			break;
		}

	}


}

NcOutput()

      /* Description   :- This outputs the data from a drawing into a file which relates
       *                  to the drawing by name. The output file will be called 
       *                  drgnam.igf
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- The full desrciption of the output file is as floows
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */
/* NO ARGS */

{

char drgnam[1025];
char outnam[1025];
char *suffix;
char units[4];
DAXCADLOGICAL MAKEOK();
DAXCADINT mip;
DAXCADINT propmip;	/* property master index */
DAXCADINT profmip;
DAXCADINT rlp;
int i;
int length;
int pntr;
int st;
int exitcode;
double xpo,ypo,xpo1,ypo1;		/* adjusted x and y values */
double xp,yp;
double drgscale;
double dbufac;
double tolerance;
char *curdate;
int clock;

      time(&clock);
      curdate = ctime(&clock);

	GETDRAWINGNAME(drgnam,&length);
	GETDRAWINGPARAMS(&drgscale,&dbufac,units);
	NCTOL(&tolerance);

	strcpy(outnam,drgnam);

	suffix = rindex(drgnam,'.');

	sprintf(suffix,".igf");

	OutputFP = fopen(drgnam,"r");

	if ( OutputFP ) {

		fclose(OutputFP);
		if (MAKEOK() == F77False)	{
			return;
		}
	}

	OutputFP = fopen(drgnam,"w");
	if ( !OutputFP ) {
		RelDaxDisplay();
		perror("Cannot open NC output file");
		AqDaxDisplay();
		DEPRNT(&NoNcOpen);
		return;
	}

	exitcode = MxGetDatum(DATUM_RETURN);	/* get datum if there is one and load globals */

	MxBuildList();	/* get a list of elements and sort */
	MxMainSort();

	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#     Practical Technology 1990 \n");
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#              DAXCAD\n");
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#     Generic Numerical Control\n");
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#     %s\n",curdate);
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"# From DAXCAD drawing %s \n",outnam);
	fprintf(OutputFP,"# Sequences on Layer %d\n",MisomexLayer);
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#\n");

	fprintf(OutputFP,"%%HEADER DRAWING %s\n",outnam);
	fprintf(OutputFP,"%%HEADER LAYER %d\n",MisomexLayer);
	fprintf(OutputFP,"%%HEADER SCALE %lf\n",drgscale);
	fprintf(OutputFP,"%%HEADER UNITS %s\n",units);
	fprintf(OutputFP,"%%HEADER FACTOR %lf\n",dbufac);
	fprintf(OutputFP,"%%HEADER TOLERANCE %lf\n",tolerance);
	fprintf(OutputFP,"#\n");
	fprintf(OutputFP,"#\n");


	for ( i=0;i<EditElements;i++)	{

		
		fprintf(OutputFP,"#\n");
		fprintf(OutputFP,"%%BEGINSEQ %d\n",EditList[i].seqno);

		profmip = EditList[i].mipp;

		daxcadread(profmip,&st);

		propmip = GlobalEntity.mip[DXPROP];	/* get prfile group header */

		if ( propmip ) {		/* Master prop attached */

			NCEXTRACTPROPERTY(&profmip);	/* get properties from F77 */
		}

		NcGetPoint(profmip,MARKER_START,&xp,&yp);

		xpo = GlobalEntity.pdp[XVAL1] - MxDatum[0];	/* get datum */
		ypo = GlobalEntity.pdp[YVAL1] - MxDatum[1];
		fprintf(OutputFP,"&SP %lf %lf\n",xpo,ypo);

		daxcadread(profmip,&st);

		rlp = GlobalEntity.mip[DXRELATION];

		pntr = 0;
		do {

			GETGROUP(&rlp,&pntr,&mip);
			if ( pntr )	{

				daxcadread(mip,&st);

				switch(GlobalEntity.mip[DXENTITY])	{

				case LINE :

					xpo = GlobalEntity.pdp[XVAL1] - MxDatum[0];	/* get datum */
					ypo = GlobalEntity.pdp[YVAL1] - MxDatum[1];
					xpo1 = GlobalEntity.pdp[XVAL2] - MxDatum[0];	/* get datum */
					ypo1 = GlobalEntity.pdp[YVAL2] - MxDatum[1];
					fprintf(OutputFP,"&L %lf %lf %lf %lf\n",xpo,ypo,xpo1,ypo1);
					break;
										
				case ARC :

					xpo = GlobalEntity.pdp[CENX] - MxDatum[0];	/* get datum */
					ypo = GlobalEntity.pdp[CENY] - MxDatum[1];
					fprintf(OutputFP,"&A %lf %lf %lf %lf %lf\n",xpo,ypo,
									GlobalEntity.pdp[RADIUS],
									GlobalEntity.pdp[SANG],
									GlobalEntity.pdp[EANG]);
					break;

				}

			}
		}
		while( pntr );

		NcGetPoint(profmip,MARKER_END,&xp,&yp);
		xp -= MxDatum[0];
		yp -= MxDatum[1];
		fprintf(OutputFP,"&EP %lf %lf\n",xp,yp);
		
		fprintf(OutputFP,"%%ENDSEQ\n");
		
	}

	fprintf(OutputFP,"%%ENDFILE\n");
	fclose(OutputFP);
	free((char*)EditList);		/* free up editlist */

}

int 
NcArcDir(Xp,Yp,Entity)

      /* Description   :- This takes a hit point which MUST lie on either start or end of the arc
       *                  and determines the directon of the arc wrt the hit point
       * 
       *
       * Return status :- -1 CW 1 CCW 0 Hit point not on either end
       *                  
       *   
       *                  
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

double Xp;			/* <i>	Hit point X */
double Yp;			/* <i>	Hit point x */
struct EntityBuffer Entity;	/* <i>  Entity buffer from DAXCAD */

{
double x1;
double y1;
double x2;
double y2;



        if ( fabs(Entity.pdp[SANG] ) < ARCTOL && fabs(Entity.pdp[EANG]-(M_PI*2.0)) < ARCTOL )	{
		return 1;	/* always return CCW for a full circle */

	}

	x1 = Entity.pdp[CENX] + Entity.pdp[RADIUS]*cos(Entity.pdp[SANG]);	/* get end point */
	y1 = Entity.pdp[CENY] + Entity.pdp[RADIUS]*sin(Entity.pdp[SANG]);
	x2 = Entity.pdp[CENX] + Entity.pdp[RADIUS]*cos(Entity.pdp[EANG]);
	y2 = Entity.pdp[CENY] + Entity.pdp[RADIUS]*sin(Entity.pdp[EANG]);


	if ( PTOL(&x1,&y1,&Xp,&Yp))	
		return 1;				/* CCW direction as DAXCAD */
	if ( PTOL(&x2,&y2,&Xp,&Yp))	
		return 1;				/* CW direction as DAXCAD */

	return 0;	/* hit point not on arc */
}

NcGetPoint(Mip,Point,Xp,Yp)

      /* Description   :- This routine is part of the Output code. It gets the
       *                  start point of a profile defined by a MARKER entity 
       * 
       *
       * Return status :- -1 If the list has not been generated
       *                  
       *   
       *                  
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXCADINT Mip;		/* <i>	DAXCAD group entity MIP */
int Point;		/* <i>  Indicates either a start or an end point */
double *Xp;		/* <o>	Hit point X */
double *Yp;		/* <o>	Hit point X */

{

int st;
DAXCADINT mip; 
DAXCADINT rlp; 
int pntr;


	daxcadread(Mip,&st);			/* read group header */

	rlp = GlobalEntity.mip[DXRELATION];	/* get relation header */

	pntr = 0;
	do {

		GETGROUP(&rlp,&pntr,&mip);	/* get next element in groupo list */
		if ( pntr )     {

			daxcadread(mip,&st);

			if ( GlobalEntity.mip[DXENTITY] == MARKER )	{
				if ( Point == MARKER_START && (GlobalEntity.mip[DXFORM] == MARKER_START || 
					GlobalEntity.mip[DXFORM] == MARKER_OPEN ))	{
						*Xp = GlobalEntity.pdp[XVAL1];
						*Yp = GlobalEntity.pdp[YVAL1];
						return 0;
				}
				if ( Point == MARKER_END && (GlobalEntity.mip[DXFORM] == MARKER_END || 
					GlobalEntity.mip[DXFORM] == MARKER_OPEN ))	{
						*Xp = GlobalEntity.pdp[XVAL1];
						*Yp = GlobalEntity.pdp[YVAL1];
						return 0;
				}
			}
		}
	}
	while ( pntr);


	return -1;

}

NCWRITEPROPERTY(Property,Type,Value)

      /* Description   :- Writes the property NAME and value to the current output file
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- Currently has to be called from F77
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

char *Property;	/*	<i>	Property name */
int *Type;	/*	<i>	DAXCAD type */
char *Value;	/*	<i>	Value in text for */

{


	switch (*Type)	{

	case DAXCAD_INT :

		fprintf(OutputFP,"&P %s INT %s\n",Property,Value);
		break;

	case DAXCAD_REAL :

		fprintf(OutputFP,"&P %s REAL %s\n",Property,Value);
		break;

	case DAXCAD_TEXT :

		fprintf(OutputFP,"&P %s TEXT %s\n",Property,Value);
		break;
	
	}


}








