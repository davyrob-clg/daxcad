/*
     @(#)  412.1 date 6/11/92 toolkit_extern.h 

     Filename    : toolkit_extern.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:42:18
     Last change : 92/06/11 14:42:17

     Copyright : Practical Technology Limited  
     File :- toolkit_extern.h

     C include file

     Main toolkit include file containing external references to globals

*/

extern int TkCurrentInputWindow;		/* current window resource where input events are comming from */
extern int TkCurrentOutputWindow;		/* output window */
extern char TkNextWindowId;			/* next id */

extern struct TkWindow *TkWindows[MAXWINDOWS];	/* windows that are defined */
extern struct TkTextWindow *TkTextWindows[MAXTEXTWINDOWS];	/* text windows that are defined */

extern struct TkScrollbar *TkScrollbars[MAXSCROLLBARS];		/* scroll bars */

extern struct TkFont *TkFonts[MAXFONTS];

extern int TkSystemDepth;	/* default system depth */

extern unsigned int GprLibSet[8];
                    
extern struct TkButton *TkButtons[MAXBUTTONS];	/* The button resource array */

extern int TkBulletinStack[MAXBULLETINS];			/* temporary bulletin stack */
extern int TkBulletinSp;							/* stack pointer */

extern int TkForeGround;	/* default foreground and background colors */
extern int TkBackGround;

extern TkInputState;		/* state of mouse input */
extern int TkGrabbedObject;		/* object has been grabbed */

extern int TkParentResource;	/* current resource for getting objects */

extern int TkEventing;				/* notifier loop control */

