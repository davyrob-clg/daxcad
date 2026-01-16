/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_lib.h   */

/*

   Defintions for X library code not for clients not using X defintions 

*/



typedef struct {                   /* the descripter of an individual bitmap */

               INT drawable;           /* is the bitmap on screen or not */
               INT clipping;           /* is clipping active or not */
               short clipwindow[4];    /* the current clip window size */
               GprOrigin curpos;       /* the current postion for GR ops */
               GprCursor cursorpos;    /* the current cursor position on the bitmap */

               GprColor draw;          /* current color setting for drawing  */
               GprColor fill_f;          /* current color setting for drawing  */
               GprColor fill_b;          /* current color setting for drawing  */
               GprColor text_f;          /* current color setting for drawing  */
               GprColor text_b;          /* current color setting for drawing  */

               GprRaster rasterop;           /* current raster op on the bitmap */


               Window WindowId;        /* X window ID for the window */

               GContext  XgcText;      /* X graphics context for this bitmap */
               GContext  XgcFills;     /* X graphics context for this bitmap */
               GContext  XgcDraw;      /* X graphics context for this bitmap */
               Pixmap PixmapId;        /* The pixmap id if a hidden bitmap */
               Drawable DrawId;        /* extra item for drawble */

               Region clipregion;      /* X region id */

               short planemask;        /* current plane mask */

               Cursor currentcursor;          /* current cursor defintions */
               Cursor fontcursor;          /* current cursor defintions */
               Cursor blank;           /* The blank cursor defintiion */
               Cursor wait;            /* The waiting cursor defintion */

               int xfont;              /* GPR font id loaded earlier ( Not the fo */
               Font fid;               /* quick store of X font id also stored in GC */
               
               short gprfid;           /* a lookback to the gpr index file */

               unsigned int eventmask; /* the event mask for this event */

               GprBitmapDesc fillpat;  /* the gpr descripter currently used for filling */

               int solid;              /* Flag indicates last type of op */

               int auto_update;        /* If true then backing bitmap will be updated */
               int auto_refresh;       /* True indicates that auto refresh is active */
               Pixmap backingstore;    /* backing store flag is set */


               }   GprBitmap;


typedef struct  {
           short stream;                /* the stream id code for this session */
           GprBitmapDesc desc;          /* the descripter of main screen bitmap */
           INT (*WindowProcedure)();      /* pointer to a function for refreshing routines */
           INT (*DispMemProc)();          /* pointer to a function for refreshing routines */

        }  GprInit;


typedef struct {

                   XFontStruct *XGPRFont;      /* font structure */
                   char *fontname;             /* name of font */

               } GprFontStruct;


GprFontStruct  *GprFonts[MAXFONTS];            /* list of pointers */

struct {

         unsigned char windowid;    /* the window id used by gpr */
         Window xwin;      /* the X window id */
   
       } GprWindowId[MAXBITMAPS];  /* sub reference for eventing */

