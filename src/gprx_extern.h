
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_extern.h   */

/*

   External include file referebce for all gprx routines.
   Based on gprx.h


*/

#ifndef GPRX_EXTERN_H_
#define GPRX_EXTERN_H_

extern int *RasterOpTable[];


char *strdupn();

extern char *getenv();             /* define function */

extern Display *Xdisplay;                 /* current X display open */

extern int BitmapCount;               /* number of bitmaps currently open */

extern GprBitmap *Bitmaps[];    /* pointers to a lot of bitmaps */

extern GprBitmapDesc CurrentBitmapId;     /* The ID of the current bitmap being used */

extern short *GprWindowOrg;             /* default window positions */

extern GContext LocalGcDraw;                  /* local GC for the current bitmap */
extern GContext LocalGcText;                  /* local GC for the current bitmap */
extern GContext LocalGcFill;                  /* local GC for the current bitmap */

extern Drawable LocalDrawable;            /* drawble for local OP */

extern GprBitmapDesc EventInputDesc;      /* the window being used for event input */

extern Drawable LocalAutoRefresh;
extern int GprAutoRefresh;
extern int GprAutoUpdate;


extern Colormap SystemColormap;           /* GPRs colormap */
extern int SystemDepth;                   /* the number of planes avaliable */
extern int SystemScreen;                  /* the screen definition */

extern int (*GprWindowPaintProc)();       /* refersh procesdure */

extern Cursor GprBlankCursor;        /* set the id for no cursor */


extern int PolygonOpen;              /* flag indicates polygon is open Defines number of vertices being used */
extern int PolygonFirst0;              /* flag indicates which is the starting vertex */

extern XPoint *Points;                    /* pointer to a list of points being  created */


extern Screen screen;

extern GprFontStruct  *GprFonts[];            /* list of pointers */

extern GprBitmap *GprEnvSaved;        /* environment variable pointer */

extern XSizeHints InitialWindowHints;

extern char DefaultWindowName[260];
extern char DefaultIconName[260];
extern Pixmap IconPixmap;

extern int GprAutoSwitch;             /* allows automatic switching of windows during cursor movemtn */


extern unsigned int EventMask;

extern Colormap GprColormapId;


extern int GprWhitePixel;            /* Gpr white and black pixels */
extern int GprBlackPixel;

/*extern int TrueColor;*/
#endif // GPRX_EXTERN_H_