
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx2.c   */

/*

             Practical Technology 1990 

       Continued libray routines based library for Apollo GPR to X library 

*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include "gprx_types.h"
#include "gprx_lib.h"
#include "gprx_extern.h"
#include "gprx_errors.h"

#include "xlang.h"

static currentbitmap; /* for uses by the environment saver */

INT
    GPR_$INQ_BITMAP(Bitmap, st)

    /* Description   :- returns the current bitmap identifier to the user
       *
       * Return status :- None assumes gpr initalised
       *                  
       * Notes         :- 
       *
       */

    GprBitmapDesc *Bitmap; /*  <o>     The descripter for bitmap ther bitmap */
GprStatus *st;
{
    *Bitmap = CurrentBitmapId;
    *st = 0;
}

INT
    GPR_$SET_FILL_PATTERN(Desc, Scale, st)

    /* Description   :- Sets the pixmap to  be used for all fills in the
       *
       * Return status :- The descripter is not a pixmap
       *                  
       * Notes         :- 
       *                  
       */

    GprBitmapDesc *Desc; /*  <i> Bitmap descripter should lead to a pixmap */
short *Scale;            /*  <i> scale for pixels only can be 1 */
GprStatus *st;           /*  <o> status */
{
    XGCValues xgcv; /* Graphics context values  */
    uint32_t mask;  /* variable mask */

    *st = 0;

    if (!ValidDesc((int)*Desc))
    {
        *st = GPR_BadDescripter;
        return;
    }

    if (!Bitmaps[*Desc]->PixmapId && *Desc)
    { /* not a zero defintion and not a pixmap */
        *st = GPR_NotPixmap;
        return;
    }

    if (!*Desc)
    { /* wants nothing for his descripter must be solid fills */
        xgcv.fill_style = FillSolid;
        mask = GCFillStyle;
    }
    else
    {
        xgcv.tile = Bitmaps[*Desc]->PixmapId;
        xgcv.fill_style = FillTiled;
        mask = GCFillStyle | GCTile; /* change to new pattern */
    }

    /*      xgcv.fill_style = FillSolid;
       mask = GCFillStyle;
*/

    XChangeGC(Xdisplay, Bitmaps[CurrentBitmapId]->XgcFills, mask, &xgcv);

    Bitmaps[CurrentBitmapId]->fillpat = *Desc; /* set current gpr setting */
}

INT
    GPR_$SET_ATTRIBUTE_BLOCK(Ablock, st)
    /* Description   :- Asociates an attribute block with a bitmap
       *                  Not currently used to any effect 
       * 
       * Return status :- None
       *                  
       * Notes         :- Atribute blocks do exist within GPRX but only
       *                  as an internal block of data wich cannot be interchanged
       *                  I suppose the Graphics Context is the best use of this
       *                  At a later date attribute blocks could be defined
       *                  as GC`s
       */
    int *Ablock; /*  <i>     Attribute block descripter */
GprStatus *st;   /*  Never guess */
{
    *st = 0;
}

INT
    GPR_$INQ_CP(X, Y, st)
    /* Description   :- Returns current position in current bitmap 
       * 
       * Return status :- None
       *                  
       * Notes         :- 
       *                  
       */
    Gprcoord *X; /*  <o> The x coordinate */
Gprcoord *Y;     /*  <o> The y coordinate */
GprStatus *st;
{
    *st = 0;

    *X = Bitmaps[CurrentBitmapId]->curpos[0];
    *Y = Bitmaps[CurrentBitmapId]->curpos[1];
}

INT
    GPR_$INQ_DRAW_VALUE(Index, st)
    /* Description   :- Returns cuurent drawing color in the current bitmap.
       *                  This routine applies to all drawinf routines including
       *                  Fills, Text and primitives
       *
       * Return status :- None
       *                  
       * Notes         :- 
       *
       */
    GprColor *Index; /*  <o>     The color map index */
GprStatus *st;
{
    *st = 0;
    *Index = Bitmaps[CurrentBitmapId]->draw;
}

INT
    GPR_$INQ_FILL_VALUE(Index, st)
    /* Description   :- Returns cuurent drawing color in the current bitmap.
       *                  This routine applies to all drawinf routines including
       *                  Fills, Text and primitives
       *
       * Return status :- None
       *                  
       * Notes         :- 
       *                  
       */
    GprColor *Index; /*  <o>     The color map index */
GprStatus *st;
{
    *st = 0;
    *Index = Bitmaps[CurrentBitmapId]->fill_f;
}

INT
    GPR_$INQ_FILL_BACKGROUND_VALUE(Index, st)
    /* Description   :- Returns cuurent drawing color in the current bitmap.
       *                  This routine applies to all drawinf routines including
       *                  Fills, Text and primitives
       *
       * Return status :- None
       *                  
       * Notes         :- 
       *
       */
    GprColor *Index; /*  <o>     The color map index */
GprStatus *st;
{
    *st = 0;
    *Index = Bitmaps[CurrentBitmapId]->fill_b;
}

INT
    GPR_$INQ_TEXT_VALUES(IndexF, IndexB, st)
    /* Description   :- Returns cuurent drawing color in the current bitmap
       *                  This routine applies to all drawinf routines including
       *                  Fills, Text and primitives
       *
       * Return status :- None
       *                  
       * Notes         :- 
       *
       */
    GprColor *IndexF; /*  <o>     The color map index */
GprColor *IndexB;     /*  <o>     The color map index */
GprStatus *st;
{
    *st = 0;
    *IndexF = Bitmaps[CurrentBitmapId]->text_f;
    *IndexB = Bitmaps[CurrentBitmapId]->text_b;
}

INT
    GPR_$CIRCLE(Center, Radius, st)
    /* Description   :- Draws a full circle about the point with the radius
       * 
       * Return status :- None 
       *                  
       * Notes         :- 
       *
       */
    GprOrigin Center; /*  <i> The center of the circle */
short *Radius;        /*  <i> The radius of the circle */
GprStatus *st;
{
    int x, y;
    unsigned int width, height;
    int angle1, angle2;

    x = Center[0] - *Radius; /* calculte upper left hand corner */
    y = Center[1] - *Radius;

    width = *Radius * 2;
    height = *Radius * 2;

    angle1 = 0;
    angle2 = 23040; /* number of 64ths in 360 degrees */

    XDrawArc(Xdisplay, LocalDrawable, LocalGcDraw, x, y, width, height,
             angle1, angle2);

    if (GprAutoUpdate)
    {
        XDrawArc(Xdisplay, LocalAutoRefresh,
                 LocalGcDraw, x, y, width, height, angle1, angle2);
    }
}

INT
    GPR_$CIRCLE_FILLED(Center, Radius, st)
    /* Description   :- Draws a full circle about the point with the radius
       *                  and fills it with current settings.
       * 
       * Return status :- None 
       *                  
       * Notes         :- 
       *                  
       */
    GprOrigin Center; /*  <i> The center of the circle */
short *Radius;        /*  <i> The radius of the circle */
GprStatus *st;
{
#define NIL (0) // A name for the void pointer
    int x, y;
    unsigned int width, height;
    int angle1, angle2;

    x = Center[0] - *Radius; /* calculte upper left hand corner */
    y = Center[1] - *Radius;

    width = *Radius * 2;
    height = *Radius * 2;

    angle1 = 0;
    angle2 = 23040; /* number of 64ths in 360 degrees */

    XFillArc(Xdisplay, LocalDrawable, LocalGcFill, x, y, width, height,
             angle1, angle2);

    if (GprAutoUpdate)
    {
        XFillArc(Xdisplay, LocalAutoRefresh, LocalGcDraw, x, y,
                 width, height, angle1, angle2);
    }

    //assert(Xdisplay);
    XFlush(Xdisplay);

    return;
    int blackColor = BlackPixel(Xdisplay, DefaultScreen(Xdisplay));
    int whiteColor = WhitePixel(Xdisplay, DefaultScreen(Xdisplay));

    // Create the window

    Window w = XCreateSimpleWindow(Xdisplay, DefaultRootWindow(Xdisplay), 0, 0,
                                   200, 100, 0, blackColor, blackColor);

    // We want to get MapNotify events

    XSelectInput(Xdisplay, w, StructureNotifyMask);

    // "Map" the window (that is, make it appear on the screen)

    XMapWindow(Xdisplay, w);

    // Create a "Graphics Context"

    GC gc = XCreateGC(Xdisplay, w, 0, NIL);

    // Tell the GC we draw using the white color

    XSetForeground(Xdisplay, gc, whiteColor);

    // Wait for the MapNotify event

    for (;;)
    {
        XEvent e;
        XNextEvent(Xdisplay, &e);
        if (e.type == MapNotify)
            break;
    }

    // Draw the line

    XDrawLine(Xdisplay, w, gc, 10, 60, 180, 20);

    // Send the "DrawLine" request to the server

    XFlush(Xdisplay);

    // Wait for 10 seconds

    //sleep(10);
}

INT
    GPR_$INQ_BITMAP_DIMENSIONS(Desc, Size, Hiplane, st)
    /* Description   :- Returns the size and number of planes of a bitmap
       * 
       * Return status :- 
       *                  
       * Notes         :- 
       *                  
       */
    GprBitmapDesc *Desc; /*  <i> descripeter of bitmap */
GprBitmapSize Size;      /*  <o> width and height */
GprPlanes *Hiplane;      /*  <o> Higest plane id */
GprStatus *st;
{
    Window root;                /* root window ID */
    int x, y;                   /* location */
    unsigned int width, height; /* what do you think */
    unsigned int border_width;  /* what do you think */
    unsigned int depth;         /* depth of window */
    Drawable d;
    Status status;

    d = Bitmaps[CurrentBitmapId]->DrawId;

    *st = 0;

    if (!ValidDesc(*Desc))
    {
        *st = GPR_BadDescripter;
        return;
    }

    /* get gemetry of window or pixmap  */
    XGetGeometry(Xdisplay, d, &root,
                 &x, &y, &width, &height, &border_width, &depth);

    Size[0] = width;
    Size[1] = height;
    *Hiplane = depth - 1;
}

INT
    GPR_$INQ_BITMAP_START(Desc, Pos, st)

    /* Description   :- Returns the positon of the current bitmap window
       *                  relative to the X rootwindow
       *
       * Return status :- NONE
       *                  
       * Notes         :- EXTENSION:  Return the pos of the window in the
       *                              rootwindow workspace
       *
       */
    GprBitmapDesc *Desc; /*  <i> descripeter of bitmap */
GprOrigin Pos;           /*  <o> position */
GprStatus *st;
{
    Window root;                /* root window ID */
    int x, y;                   /* location */
    unsigned int width, height; /* what do you think */
    unsigned int border_width;  /* what do you think */
    unsigned int depth;         /* depth of window */

    if (!ValidDesc(*Desc))
    {
        *st = GPR_BadDescripter;
        return;
    }

    /* get gemetry of window or pixmap  */
    XGetGeometry(Xdisplay, Bitmaps[CurrentBitmapId]->DrawId, &root,
                 &x, &y, &width, &height, &border_width, &depth);

    Pos[0] = x;
    Pos[1] = y;
}

GPR_$SET_BANNER(Desc, String, Length, st)
/* Description   :- Extension for GPRX. Sets the title banner for a 
       *                  window.
       *
       * Return status :- Length too big, not a visible window etc.
       *                  
       * Notes         :- This can only be used where motif or some other
       *                  window manager is running that will display the 
       *                  details of the window.
       *
       */
GprBitmapDesc *Desc; /* <i>  the bitmap descripter for the window */
char *String;        /* <i>  Title string */
short *Length;       /* <i>  Length of string */
GprStatus *st;
{
    int len;

    *st = 0;
    if (*Length <= 0)
    {
        return;
    }
    len = *Length;

    if (!Bitmaps[*Desc]->WindowId)
    {
        *st = GPR_BitmapIsNotWindow;
        return;
    }

    XChangeProperty(Xdisplay, Bitmaps[*Desc]->WindowId,
                    XA_WM_NAME, XA_STRING, 8,      /* 8 bit info */
                    PropModeReplace, String, len); /* data */
}

GPRXError(Func, Error, Code)
    /* Description   :- Prints out an ascii value if debugging mode 
       * 
       * Return status :- NONE
       *                  
       * Notes         :- This will print to the current output pad any
       *                  errors encounted in the system. It will not stop 
       *                  the system.
       *
       */
    char *Func; /*  <i>     Function name */
char *Error;    /*  <i>     Error string */
int Code;       /*  <i>     The error code */
{
    printf("*** Warning ***\n");
    printf(" GPRX error message: %s\n", Error);
    printf(" Received from : %s\n", Func);
}

GPR_$SET_BITMAP_WINDOW(Desc, Win, st)
/* Description   :- Sets the postion of the Window bitmap to 
       *                  the postion on the screen with dimensions
       *
       * Return status :- NONE
       *                  
       * Notes         :- EXTENSION:  Allows the window to be reconfigured
       *                              to the exact size set.
       *
       */
GprBitmapDesc *Desc; /* <i>  the bitmap descripter for the window */
GprWindow Win;       /*  <i> The window sizes */
GprStatus *st;
{
    XWindowChanges values;
    Window win;
    GprStatus status;
    int count;

    *st = 0;

    if (!ValidDesc(*Desc))
    {
        *st = GPR_BadDescripter;
        return;
    }

    if (Bitmaps[*Desc]->PixmapId)
    {
        *st = GPR_BitmapIsNotWindow; /* not a window */
        return;
    }

    values.x = Win[0];
    values.y = Win[1];

    values.width = Win[2];
    values.height = Win[3];

    XResizeWindow(Xdisplay, Bitmaps[*Desc]->WindowId, Win[2], Win[3]);
    GPR_$COLLECT_EXPOSURES(&count, &status);
    XFlush(Xdisplay);
}

WaitForConfigure()
/* Description   :- Waits for a configure event generated by ourself
       * 
       * Return status :- 
       *                  
       * Notes         :- This is used by the set bitmap window routine and others to
       *                  wait for the window manager or server to return a ConfigureNotify
       *                  event what was just generated.
       *
       */
{
    XEvent event;
    int waiting;

    waiting = 1;
    while (waiting)
    {
        XNextEvent(Xdisplay, &event); /* get all expose events */
        if (event.type == ConfigureNotify)
        {
            waiting = 0;
        }
    }
}

GPR_$COLLECT_EXPOSURES(Count, st)
/* Description   :- Collects exposure events from self generated window events
       *
       * Return status :- NONE
       *                  
       * Notes         :- EXTENSION:   If a window is resized by the client
       *                               ie us, then exposure events will be generated
       *                               Normally you dont want these because
       *                               they will be stacked by the server and sent
       *                               back to you again Effectivly it clears the
       *                               buffer of exposures.
       *
       */
int *Count; /*  <o> The nunmber of exposrues tha were collected */
GprStatus *st;
{
    XEvent event;
    XExposeEvent *ee; /* pointer to expose requset */
    int count;

    *st = 0;
    *Count = 0;
    count = XPending(Xdisplay); /* check pending events and flush */

    while (count)
    {
        XNextEvent(Xdisplay, &event); /* get all expose events */
        count--;
        if (event.type == Expose)
        {
            (*Count)++;                  /* inc exposure count */
            ee = (XExposeEvent *)&event; /* get exposure event count */
            while (ee->count)
            {
                XNextEvent(Xdisplay, &event); /* get all expose events */
                count--;
                ee = (XExposeEvent *)&event;
                (*Count)++;
            }
        }
    }
}

GPR_$INQ_RASTER_OP(Ops, st)
/* Description   :- Returns current raster operations for current
       *                  bitmap.
       *
       * Return status :- NONE
       *                  
       * Notes         :- This will return the same raster op for all 
       *                  planes currently
       *                  
       */
short Ops[8];
GprStatus *st;
{
    int i;

    for (i = 0; i < 8; i++)
    {
        Ops[i] = Bitmaps[CurrentBitmapId]->rasterop;
    }
}

GPR_$SAVE_ENV_X(st)
/* Description   :- Saves the current GPRX bitmap settings
       *
       * Return status :- NONE
       *                  
       * Notes         :- Will save rasters colours and current bitmap
       *
       */
GprStatus *st;
{
    *st = 0;
    if (!GprEnvSaved)
    {
        GprEnvSaved = (GprBitmap *)malloc(sizeof(GprBitmap)); /* allocate memeory */
    }

    memcpy(GprEnvSaved, Bitmaps[CurrentBitmapId], sizeof(GprBitmap)); /* wow copy in whole struct */

    currentbitmap = CurrentBitmapId; /* save ID of bitmap */
}

GPR_$RESTORE_ENV_X(st)
/* Description   :- Restores the current bitmap GPRX settings 
       * 
       * Return status :- GPR_NoEnvironment if not saved 
       *                  
       * Notes         :- Will restore rasters colours and current bitmap
       */
GprStatus *st;
{
    GprStatus status;
    GprPlanes plane;
    GprFontid fid;

    static currentbitmap;

    if (!GprEnvSaved)
    {
        *st = GPR_NoEnvironment;
        return;
    }

    CurrentBitmapId = currentbitmap;
    memcpy(Bitmaps[CurrentBitmapId], GprEnvSaved, sizeof(GprBitmap)); /* wow copy in whole struct */
    SetLocalDrawable();                                               /* restore external settings */

    plane = (unsigned int)-1; /* stop optimser comlaining */
    GPR_$SET_RASTER_OP(plane, &Bitmaps[CurrentBitmapId]->rasterop, &status);
    TOOLPEN(&Bitmaps[CurrentBitmapId]->draw);
    GPR_$SET_TEXT_VALUE(&Bitmaps[CurrentBitmapId]->text_f, &status);
    GPR_$SET_TEXT_BACKGROUND_VALUE(&Bitmaps[CurrentBitmapId]->text_b, &status);
    TOOLPEN(&Bitmaps[CurrentBitmapId]->fill_f);
    GPR_$SET_FILL_BACKGROUND_VALUE(&Bitmaps[CurrentBitmapId]->fill_b, &status);

    free(GprEnvSaved); /* free up memory */
    GprEnvSaved = 0;   /* clear pointer */
}

INT
    SetProperty(Win)
    /* Description   :- Sets standard properties for the window just defined
       *                  Not yet mapped. Some values can be changed before mapping
       *                  such as the name of the window etc.
       *
       * Return status :- 
       *                  
       * Notes         :- 
       *
       */
    Window Win; /* <i>   window id */
{
    int len;

    strcpy(DefaultWindowName, "DAXCAD ");
    len = strlen(DefaultWindowName); /* length of default window name */

    XChangeProperty(Xdisplay, Win, XA_WM_NAME, XA_STRING, 8,  /* 8 bit info */
                    PropModeReplace, DefaultWindowName, len); /* data */

    InitialWindowHints.flags = PPosition | PSize | PMinSize | PMaxSize | PAspect;
    XSetSizeHints(Xdisplay, Win, &InitialWindowHints, XA_WM_SIZE_HINTS);
    /*	if ( Xdisplay->screens->save_unders)	{
		printf("Backing store is enabled on Server\n");
	}
*/
}

INT
    InterceptExposures(EventStruct)
    /* Description   :- Gets the events associated with an Expose event just
       *                  generated 
       *                  
       * Return status :- NONE
       *                  
       * Notes         :- NONE
       */
    XEvent *EventStruct; /*  <i>     Event structure from main event code */
{
    XExposeEvent *ee; /* pointer to expose requset */
    XEvent event;
    GprWindow rect;
    GprOrigin org;
    GprStatus status;
    GprBitmapDesc source;
    GprBitmapDesc current;
    GprBitmapDesc desc;
    Window win;
    LOGICAL clip;
    LOGICAL f;

    ee = (XExposeEvent *)EventStruct;

    win = ee->window; /* need this bit to handle exposures of other windows */
    WindowToBitmap(win, &desc);
    current = CurrentBitmapId;
    GPR_$SET_BITMAP(&desc, &status);

    if (!GprAutoRefresh)
    {
        GPR_$SET_BITMAP(&current, &status);
        return -1; /* cannot return exposure */
    }

    if (Bitmaps[CurrentBitmapId]->clipping)
        clip = F77true;
    else
        clip = F77false;

    f = F77false;

    GPR_$SET_CLIPPING_ACTIVE(&f, &status);

    GprAutoUpdate = F77false;
    source = Bitmaps[CurrentBitmapId]->backingstore;

    org[0] = ee->x;
    org[1] = ee->y;

    rect[0] = ee->x;
    rect[1] = ee->y;
    rect[2] = ee->width;
    rect[3] = ee->height;

    GPR_$PIXEL_BLT(&source, rect, org, &status);
    while (ee->count)
    {

        XNextEvent(Xdisplay, &event); /* get all expose events */
        ee = (XExposeEvent *)&event;
        org[0] = ee->x;
        org[1] = ee->y;

        rect[0] = ee->x;
        rect[1] = ee->y;
        rect[2] = ee->width;
        rect[3] = ee->height;

        GPR_$PIXEL_BLT(&source, rect, org, &status);
    }

    GPR_$SET_CLIPPING_ACTIVE(&clip, &status);
    GprAutoUpdate = F77true;
    XFlush(Xdisplay);                   /* clear buffer */
    GPR_$SET_BITMAP(&current, &status); /* rest to normal bitmap */
}

WindowToBitmap(Win, Desc)
    /* Description   :- Returns the GPRX bitmap descripter from
       *                  the X window Atom
       * Return status :- Not found -1
       *                  
       * Notes         :- This simply loops round all bitmaps descripters and
       *                  looks for the descripter 
       *
       */
    Window Win;      /*  <i> This is the window id */
GprBitmapDesc *Desc; /*  <o> The returned descripter */
{
    int i;

    if (!Win)
        return -1; /* bad window */

    for (i = 0; i < MAXBITMAPS; i++)
    { /* look round all bitmaps */

        if (Bitmaps[i])
        {
            if (Win == Bitmaps[i]->WindowId)
            {
                *Desc = i;
                return 0;
            }
        }
    }

    return -1; /* if we get here then a disaster  */
}

INT
    GPR_$SET_AUTO_SWITCH(Auto, st)
    /* Description   :- Sets automatic witching as cursor moves fomr one window to 
       *                  another.
       * 
       * Return status :- 
       *                  
       * Notes         :- EXTENSION: The window id event is still sent
       *                             But action does not have to be taken
       *                             if this routine is called;
       *
       */
    LOGICAL *Auto; /*  <i>     Logical to indicate mode */
GprStatus *st;
{
    *st = 0;
    GprAutoSwitch = *Auto;
}

INT
    GPR_$SET_AUTO_UPDATE(Auto, st)
    /* Description   :- Allows graphics to be updated or not in the hidden bitmap
       *                  management system. If set the data will be updated into
       *                  the hidden bitmap 
       *
       * Return status :- GPR_NoAutoRefresh if mode not set
       *                  
       * Notes         :-EXTENSION: This routine is used in conjunction with
       *                            gpr_$set_auto_refresh and the hidden bitmap 
       *                            management system. It should be used when
       *                            you dont want the hidden bitmap to be updated
       *                            This should be outdated when X cursor are used. 
       *                            Warning use with care because when
       *                            not in use then hidden bitmap will not be updated 
       *                            so external windows could cause problems
       */
    LOGICAL *Auto; /*  <i>     Logical to indicate mode */
GprStatus *st;
{
    *st = 0;

    if (!GprAutoRefresh)
    {
        *st = GPR_NoAutoRefresh;
        return;
    }
    Bitmaps[CurrentBitmapId]->auto_update = *Auto;

    SetLocalDrawable();
}

INT
    GPR_$FORCE_HIDDEN_UPDATE(st)
    /* Description   :- Forces a hidden update of the bitmap by copying the 
       *                  whole screen.
       * 
       * Return status :- GPR_NoAutoRefresh if not active
       *                  
       * Notes         :- EXTENSION:   This is in place to counter performance
       *                               problems. Call this to force a hidden update
       *                               You may want to call this if you turn the 
       *                               updating off.
       */

    GprStatus *st;
{
    GprBitmapDesc source;
    GprBitmapDesc desc;
    GprBitmapSize size;
    GprOrigin org;
    GprWindow window;
    GprPlanes planes;
    GprStatus status;

    *st = 0;

    if (!GprAutoRefresh)
    {
        *st = GPR_NoAutoRefresh;
        return;
    }

    source = CurrentBitmapId; /* Save current bitmap ID  */

    GPR_$INQ_BITMAP_DIMENSIONS(&source, size, &planes, &status);

    desc = Bitmaps[source]->backingstore; /* hidden bitmap */

    GPR_$SET_BITMAP(&desc, &status); /* set to new bitmap */

    org[0] = 0; /* set up origin */
    org[1] = 0;
    window[0] = 0;
    window[1] = 0;
    window[2] = size[0];
    window[3] = size[1];

    GPR_$PIXEL_BLT(&source, window, org, &status); /* copy from window */

    GPR_$SET_BITMAP(&source, &status); /*  set to old bitmap */
}

GPR_$GET_SYSTEM_COLORMAP(Pixels, NoPixels, st)
/* Description   :- Gets an array of pixels which 
       *                  map indexes to a colormap
       *                
       * Return status :- 0 Success
       *                 -1 To many entries
       *                
       * Notes         :- 
       *
       */
int *Pixels;   /* <i> Pixel array */
int *NoPixels; /* <i> Pixel array size */
int *st;
{
    int i;
    int indexes;
    XColor xcolorcell; /* The colormap cell for X windows */
    int ret;
    int planemask[256];

    *st = 0;

    if (SystemDepth == 1)
    { /* monochrome */
        *st = 0;
        return;
    }

    ret = XAllocColorCells(Xdisplay, SystemColormap, True, planemask, 0, Pixels, *NoPixels);

    if (ret == 0)
    {
        *st = -1; /* cannot allocate entries */
    }
}

int
    GPR_$SET_COLOR_MAP(StartIndex, N_Entries, Values, st)
    /* Description   :- Established new values for the color map
       * 
       * Return status :-NONE
       *                  
       * Notes         :-This call uses the same formats as gpr as in X
       *                  
       */
    GprColor *StartIndex; /*	Index of first color array */
short *N_Entries;
int Values[256];
int *st;
{
    int col;
    Colormap cmap;                /* default colormap id */
    XColor xcolorcell;            /* The colormap cell for X windows */
    GprColorVector *gprcolorcell; /* The gpr colormap values */
    Display *display;
    int R, G, B;
    char spec[20];
    int ret;

    cmap = GprColormapId;

    for (col = 0; col < *N_Entries; col++)
    {

        RGB(Values[col], &R, &G, &B);

        xcolorcell.red = R * 257;
        xcolorcell.green = G * 257;
        xcolorcell.blue = B * 257;

        xcolorcell.pixel = col + (*StartIndex);
        xcolorcell.flags = DoRed | DoBlue | DoGreen;
        XStoreColor(Xdisplay, GprColormapId, &xcolorcell);
    }
}

RGB(Value, R, G, B)
/* Description   :- Established new values for the color map
       *
       * Return status :-NONE
       *                  
       * Notes         :-This call uses the same formats as gpr as in X
       *                  
       */
int Value; /* <i> Incoming coded GPR value */
int *R;    /* <o> Red */
int *G;    /* <o> Green */
int *B;    /* <o> Blue */
{
    *R = Value / 65536;
    *G = (Value - (*R * 65536)) / 256;
    *B = (Value - (*R * 65536) - (*G * 256));
    return;
}

GPRX_XBELL()
/* Description   :- Rings the bell based on the X server bell
       * Return status :-NONE
       * Notes         :-A simple bell ringer that is X portable
       */

{
    char *ptr;
    int percent;

    ptr = getenv("DAXCAD_BELL_VOLUME");
    if (ptr && *ptr)
    {
        percent = atoi(ptr);
        if (percent < 0 || percent > 100)
            percent = 0;
    }
    else
        percent = 100;

    XBell(Xdisplay, percent);
}

GPR_$WRITE_BITMAP(Bitmap, File, Length, st)
/* Description   :- Write a GPR bitmap to a file
       *                
       * Return status :- 0 Succces
       *                  error ( unix )
       *                
       * Notes         :- 
       *
       */
GprBitmapDesc *Bitmap; /*  <o>     The descripter for bitmap ther bitmap */
char *File;            /*  <i>     File name to write */
int *Length;           /*  <i>     File name length */
GprStatus *st;
{
    char *f;
    GprBitmapSize size; /*  <o> width and height */
    GprPlanes hiplane;  /*  <o> Higest plane id */
    int width;
    int height;
    Pixmap bitmap;

    *st = 0;

    f = strdupn(File, *Length);

    GPR_$INQ_BITMAP_DIMENSIONS(Bitmap, size, &hiplane, st);

    bitmap = Bitmaps[*Bitmap]->PixmapId;

    width = size[0];
    height = size[1];

    XWriteBitmapFile(Xdisplay, f, bitmap, width, height, -1, -1);

    free(f);
}

GPR_$READ_BITMAP(Bitmap, Dest_x, Dest_y, File, Length, st)
/* Description   :- Reads a bitmap file into a supplied GPR bitmap 
       *                
       * Return status :- 0 Succces
       *                  error ( unix )
       *                
       * Notes         :- 
       *
       */
GprBitmapDesc *Bitmap; /*  <i/o>     The descripter for bitmap ther bitmap */
int *Dest_x;           /*  <i>     Location of bitmap */
int *Dest_y;           /*  <i>     Location of bitmap */
char *File;            /*  <i>     File name to write */
int *Length;           /*  <i>     File name length */
GprStatus *st;
{
    char *f;
    GprBitmapSize size; /*  <o> width and height */
    GprPlanes hiplane;  /*  <o> Higest plane id */
    int width;
    int height;
    Pixmap bitmap;
    Drawable window;
    Pixmap tbitmap;
    int x_hot, y_hot;
    int src_x, src_y;
    int dest_x, dest_y;
    int col;
    int ret;

    *st = 0;

    f = strdupn(File, *Length);

    bitmap = Bitmaps[*Bitmap]->PixmapId;

    ret = XReadBitmapFile(Xdisplay, bitmap, f, &width, &height, &tbitmap, &x_hot, &y_hot);

    if (ret != BitmapSuccess)
    {
        *st = ret;
        return;
    }

    dest_x = *Dest_x;
    dest_y = *Dest_y;
    src_x = 0;
    src_y = 0;

    XCopyPlane(Xdisplay, tbitmap, bitmap, LocalGcDraw,
               src_x, src_y, width, height, dest_x, dest_y, 1);

    XFreePixmap(Xdisplay, tbitmap);
    free(f);
}

int DAXCADXFault(display)
    Display *display;
{
    fprintf(stderr, "\n");
    fprintf(stderr, "******************************\n");
    fprintf(stderr, "**                         ** \n");
    fprintf(stderr, "**     X windows Faulted   ** \n");
    fprintf(stderr, "**                         ** \n");
    fprintf(stderr, "******************************\n");
    fprintf(stderr, "\n");
    DAXCADFault(0, 0);
}

#if 0
/* SPB - 111194 - Added in an Xerrorhandler to calm things down */
int daxerrorhandler (dpy, theError)
    Display *dpy ;
    XErrorEvent *theError ;
{
/*    static int fortranIsCrap = 1 ; */
/*    extern void infodialog_() ; */
    char msg[80];
    XGetErrorText (dpy, theError->error_code, msg, 80) ;
    fprintf (stderr, "Error code %s\n", msg) ;
/*    infodialog_(msg, &fortranIsCrap) ; */
}
#endif
