
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx.h   */

/*

           Include file for GPR X convertor library 
           Must have X11 include files installed before it can be used.


*/






int RasterOpTable[16] = {              /* truth table ( It maps directly any way */
                           GXclear,
                           GXand,
                           GXandReverse,
                           GXcopy,
                           GXandInverted,
                           GXnoop,
                           GXxor,
                           GXor,
                           GXnor,
                           GXequiv,
                           GXinvert,
                           GXorReverse,
                           GXcopyInverted,
                           GXorInverted,
                           GXnand,
                           GXset
                        };
                                           



char *getenv();                    /* define function */

Display *Xdisplay;                 /* current X display open */

int BitmapCount = 0;               /* number of bitmaps currently open */

GprBitmap *Bitmaps[MAXBITMAPS];    /* pointers to a lot of bitmaps */

GprBitmapDesc CurrentBitmapId;     /* The ID of the current bitmap being used */

short GprWindowOrg[2];             /* default window positions */

GContext LocalGcText;              /* local GC for the current bitmap */
GContext LocalGcFill;             /* local GC for the current bitmap */
GContext LocalGcDraw;              /* local GC for the current bitmap */

Drawable LocalDrawable;            /* drawble for local OP */

Drawable LocalAutoRefresh;         /* the auto refresh flags */
int GprAutoRefresh;
int GprAutoUpdate;


GprBitmapDesc EventInputDesc;      /* the window being used for event input */

Colormap SystemColormap;           /* GPRs colormap */
int SystemDepth;                   /* the number of planes avaliable */
int SystemScreen;                  /* the screen definition */

int (*GprWindowPaintProc)();       /* refersh procesdure */

Cursor GprBlankCursor=None;        /* set the id for no cursor */

int GprAutoSwitch = 0;             /* allows automatic switching of windows during cursor movemtn */



int PolygonOpen =  0;              /* flag indicates polygon is open Defines number of vertices being used */
int PolygonFirst = 0;              /* flag indicates which is the starting vertex */

XPoint *Points;                    /* pointer to a list of points being  created */


Screen screen;


GprFontStruct  *GprFonts[MAXFONTS];            /* list of pointers */

#define SHIFT_P   1     /* All lock modes defined and ignored by the user */
#define CTRL_P    2
#define CAPS_L    4
#define SHIFT_L   8

static int KeyLockModes;        /* all key locking defintions */


#define XButton1 1     /* button number */
#define XButton2 2     
#define XButton3 3


GprBitmap *GprEnvSaved = 0;        /* environment variable pointer */



static char *XeventNames[] = {

    "",
    "",
    "KeyPress",
    "KeyRelease",
    "ButtonPress",
    "ButtonRelease",
    "MotionNotify",
    "EnterNotify",
    "LeaveNotify",
    "FocusIn",
    "FocusOut",
    "KeyMapNotify",
    "Expose",
    "GraphicsExpose",
    "NoExpose",
    "VisiblityNotify",
    "CreateNotify",
    "DestroyNotify",
    "UnmapNotify",
    "MapNotify",
    "MapRequest",
    "ReparentNotify",
    "ConfigureNotify",
    "ConfigureRequest",
    "GavityNotify",
    "ResizeRequest",
    "CirculateNotify",
    "CirculateRequest",
    "PropertyNotify",
    "SelectionClear",
    "SelectionRequest",
    "SelectionNotify",
    "ColormapNotify",
    "ClientMessage",
    "MappingNotify"
};


#define XAllEvents 0x7FFFFF    /* the mask for all events for X */

unsigned int EventMask;       /* event mask for events used */


/*         Default Window Property definitins        */
XSizeHints InitialWindowHints;
char DefaultWindowName[260];
char DefaultIconName[260];
Pixmap IconPixmap;
Colormap GprColormapId=0;	/* Global colormap for use by GPRX */

/* int GprWhitePixel=1; int GprBlackPixel=0; */
/* Gpr white and black pixels */
int GprWhitePixel, GprBlackPixel ;
