/*
     @(#)  412.1 date 6/11/92 toolkit_main.h 

     Filename    : toolkit_main.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:42:20
     Last change : 92/06/11 14:42:19

     Copyright : Practical Technology Limited  
     File :- toolkit_main.h

     C include file

     Main toolkit include file containing globals to be included in Main program

*/

int TkCurrentInputWindow = -1;		/* current window resource where input events are comming from */
int TkCurrentOutputWindow = -1;		/* current drawing window */
char TkNextWindowId='A';		/* next id */

struct TkWindow *TkWindows[MAXWINDOWS];			/* windows that are defined */

struct TkTextWindow  *TkTextWindows[MAXTEXTWINDOWS];	/* text windows that are defined */

struct TkScrollbar *TkScrollbars[MAXSCROLLBARS];		/* scroll bars */


struct TkFont *TkFonts[MAXFONTS];

int TkSystemDepth=1;	/* default system depth */

unsigned int GprLibSet[8] = {    -1,	/* Library set */
                                  -1, 
                                  -1, 
                                  -1, 
                                  -1, 
                                  -1, 
                                  -1, 
                                  -1};


struct TkButton *TkButtons[MAXBUTTONS];	/* The button resource array */

int TkBulletinStack[MAXBULLETINS];			/* temporary bulletin stack */
int TkBulletinSp = -1;							/* stack pointer */


int TkForeGround = 0;	/* default foreground and background colors */
int TkBackGround = 1;


int TkInputState=TK_NORMAL_INPUT;		/* state of mouse input */
int TkGrabbedObject = 0;		/* object has been grabbed */

int TkParentResource=0;				/* current resource for getting objects */

int TkEventing=0;				/* notifier loop control */

