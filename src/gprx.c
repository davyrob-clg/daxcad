/*
 *
 *        Filename    : gprx.c
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Main routines for GPRX Library.
 *
 *
*/

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>

#define XK_MISCELLANY

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include "gprx_types.h"
#include "gprx_lib.h"
#include "gprx.h" /* SPB 181194 - This is a most cack way of doing things ! */
#include "gprx_errors.h"

#include "xlang.h"

KeySym MapToKeypad(); /* allow keypad to function correctly */

void KillAutoRefresh(Desc);
void GPR_$SET_BITMAP(Desc, st);
void GPR_$ALLOCATE_BITMAP(Size, HiPlane, AttributeBlock, BitmapDesc, st);
void GPR_$DEALLOCATE_BITMAP(Desc, st);
void GPR_$DEALLOCATE_BITMAP_X(Desc, st);
INT GPR_$EVENT_WAIT_X(EventType, EventData, ScrPos, st);
INT GPR_$COND_EVENT_WAIT(EventType, EventData, ScrPos, st);
INT GPR_$COND_EVENT_WAIT_X(EventType, EventData, ScrPos, st);
void GPR_$UNLOAD_FONT_FILE(FontId, st);
INT AllocPoint();
INT GprWordBoundary(Num, Word);

INT ValidDesc();

/*

   Error codes. Not the same as Apollo GPR 

*/

INT
    gpralloccolor_(R, B, G, Pixel)

        int32_t *R;
int32_t *G;
int32_t *B;
int32_t *Pixel;

{

    XColor xcolorcell;
    Window root; /* root window ID */
    Window win;
    int x, y;                   /* location */
    unsigned int width, height; /* what do you think */
    unsigned int border_width;  /* what do you think */
    unsigned int depth;         /* depth of window */
    GprColor temp;              /* temp color */
    GprStatus status;
    int32_t factor;

    win = RootWindow(Xdisplay, DefaultScreen(Xdisplay));

    status = XGetGeometry(Xdisplay, /* get gemetry of window */
                          win,
                          &root,
                          &x, &y,
                          &width, &height,
                          &border_width,
                          &depth);

    factor = 256;

    xcolorcell.red = *R * factor;
    xcolorcell.green = *G * factor;
    xcolorcell.blue = *B * factor;

    xcolorcell.flags = DoRed | DoBlue | DoGreen;

    XAllocColor(Xdisplay, SystemColormap, &xcolorcell);

    *Pixel = xcolorcell.pixel;
}

INT testx_()

{

    Window root; /* root window ID */
    Window win;
    int x, y;                   /* location */
    unsigned int width, height; /* what do you think */
    unsigned int border_width;  /* what do you think */
    unsigned int depth;         /* depth of window */
    GprColor temp;              /* temp color */
    GprStatus status;

    win = RootWindow(Xdisplay, DefaultScreen(Xdisplay));

    status = XGetGeometry(Xdisplay, /* get gemetry of window */
                          win,
                          &root,
                          &x, &y,
                          &width, &height,
                          &border_width,
                          &depth);

    return 0;
}

INT SetLocalDrawable()

/* Description   :- Sets the drawable variables for local use
       * 
       * 
       *
       * Return status :- Nothing assume all ok
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

    Window win;
    Pixmap pixmap;
    GprBitmapDesc desc;

    LocalGcText = Bitmaps[CurrentBitmapId]->XgcText;
    LocalGcFill = Bitmaps[CurrentBitmapId]->XgcFills;
    LocalGcDraw = Bitmaps[CurrentBitmapId]->XgcDraw;

    win = Bitmaps[CurrentBitmapId]->WindowId;
    pixmap = Bitmaps[CurrentBitmapId]->PixmapId;

    if (pixmap)
    {
        LocalDrawable = pixmap;
        GprAutoRefresh = False;
        GprAutoUpdate = False;
    }
    else
    {
        LocalDrawable = win;
        GprAutoRefresh = Bitmaps[CurrentBitmapId]->auto_refresh; /* auto refrsh falsg */
        GprAutoUpdate = Bitmaps[CurrentBitmapId]->auto_update;   /* auto refrsh falsg */
        if (GprAutoRefresh)
        {
            desc = Bitmaps[CurrentBitmapId]->backingstore;
            LocalAutoRefresh = Bitmaps[desc]->PixmapId;
        }
    }
}

/*

   =========================================================================

*/

INT
    AllocateBitmap(Size, HiPlane, ScreenMapped, BitmapDesc)

    /* Description   :- Allocates a bitmap and/or an onscreen window.
       *                  This will malloc some area of memory 
       *                  for a the bitmap. Declare GC and Attributes based 
       *                  on pre definewd constants.
       *
       * Return status :- All error codes defined below. A return status of 0
       *                  indicates success.
       *   
       *                  
       * Notes         :- Called by gpr_$init and gpr_allocate_bitmap etc.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprBitmapSize Size;    /*  <i>  Size of bitmap to be defined */
GprPlanes HiPlane;         /*  <i>  Depth of window */
int ScreenMapped;          /*  <i>  Indicates whether hidden or not */
GprBitmapDesc *BitmapDesc; /*  <o>  ID of bitmap to be used */

{

    int i;
    int desc;        /* descripter id */
    GprBitmap *temp; /*  Pointer to a struct */

    int winx, winy;            /* window origins */
    int winw, winh;            /* window size */
    XGCValues xgcv;            /* Graphics context values  */
    XSetWindowAttributes xswa; /* Window attributes */
    Pixmap pixmap;             /* pix map drawble needed for hidden bitmaps */
    Window win;                /* pixmap window ( InputOnly ) */
    Window pixwin;             /* pixmap window ( InputOnly ) */
    GContext gcdraw;           /* The GC for the bitmap */
    GContext gctext;           /* The GC for the bitmap */
    GContext gcfill;           /* The GC for the bitmap */
    Visual visual;             /* visual id */
    XEvent pe;                 /* ready to get events */
    XExposeEvent *ee;          /* event exposure */
    GprStatus status;          /* local status */
    GprWindow window;          /* the local window for clipping */
    int exposing;              /* colect flag for expose events */
    int out;
    GprBitmapDesc tempdesc; /* temp descripter */
    int planes;
    int depth;

    depth = DefaultDepth(Xdisplay, DefaultScreen(Xdisplay)),
    /*HiPLane = depth;*/

        i = 0;

    if (!Xdisplay)
    {
        return GPR_DisplayNotOpen;
    }

    while (i < MAXBITMAPS)
    { /* loop round to find first bitmap free space */

        if (!Bitmaps[i])
            break;
        else
            i++;
    }

    if (i == MAXBITMAPS) /* no more to define */

        return GPR_NoBitmap;

    else

        desc = i; /* the descripter id */

    if ((Size[0] < 0 || Size[0] > MAXBITMAPX) || (Size[1] < 0 || Size[1] > MAXBITMAPY))

        return GPR_Size;

    if (HiPlane < 0 || HiPlane > HIPLANE) /* plane size out of range */

        return GPR_Planes;

    temp = (GprBitmap *)malloc((unsigned)sizeof(GprBitmap)); /* allocate block of mem */

    memset(temp, '\0', sizeof(GprBitmap)); /* clear memory structure after allocation */

    Bitmaps[desc] = temp; /* store the pointer */

    *BitmapDesc = desc; /* set return descripter */

    /* save all GPR specific settings */

    Bitmaps[desc]->draw = GprWhitePixel;
    Bitmaps[desc]->text_f = GprWhitePixel;
    Bitmaps[desc]->text_b = GprBlackPixel;

    Bitmaps[desc]->fill_f = GprWhitePixel;
    Bitmaps[desc]->fill_b = GprBlackPixel;

    Bitmaps[desc]->backingstore = NotUseful; /* backing store */

    Bitmaps[desc]->clipping = False; /* clipping off */

    Bitmaps[desc]->clipwindow[0] = 0;
    Bitmaps[desc]->clipwindow[1] = 0;
    Bitmaps[desc]->clipwindow[2] = Size[0]; /* set clip window sizes as full */
    Bitmaps[desc]->clipwindow[3] = Size[1];

    Bitmaps[desc]->planemask = AllPlanes; /* set all planes */
    Bitmaps[desc]->fillpat = 0;           /* set current gpr setting */

    Bitmaps[desc]->solid = FillSolid; /* solid default */

    Bitmaps[desc]->rasterop = gpr_rop_src; /* solid default */

    Bitmaps[desc]->auto_refresh = False;
    Bitmaps[desc]->auto_update = False;

    /*


     ---------------------  X Section ----------------------


*/

    winx = GprWindowOrg[0]; /* taken from default settings */
    winy = GprWindowOrg[1];
    winw = Size[0]; /* width and height */
    winh = Size[1];

    /*  Store local GPR information */

    if (ScreenMapped)
    { /* this window will be visable */

        xswa.event_mask = ExposureMask | StructureNotifyMask;
        xswa.background_pixel = GprBlackPixel;
        xswa.border_pixel = GprWhitePixel;
        xswa.backing_store = NotUseful;
        xswa.bit_gravity = StaticGravity;
        xswa.override_redirect = False;

        visual.visualid = CopyFromParent;

        win = XCreateWindow(Xdisplay,
                            RootWindow(Xdisplay, DefaultScreen(Xdisplay)),
                            winx, winy, winw, winh, 4 /*Border Width*/,
                            DefaultDepth(Xdisplay, DefaultScreen(Xdisplay)),
                            InputOutput,
                            &visual,
                            CWEventMask | CWBorderPixel | CWBackPixel | CWBitGravity,
                            &xswa);

        SetProperty(win); /* set up the standard property for this new window */

        XMapWindow(Xdisplay, win); /* display the window now */

        xgcv.function = GXcopy;
        xgcv.fill_style = FillSolid;
        xgcv.line_style = LineSolid;
        /* SPB - 080894 - The following two lines make those mini */
        /* drawings come out the correct colours on all terminals */
        xgcv.foreground = GprWhitePixel;
        xgcv.background = GprBlackPixel;

        xgcv.plane_mask = AllPlanes;

        for (i = 0; i <= HiPlane; i++)
            xgcv.plane_mask |= (1L << i); /* set the number of planes */

        xgcv.plane_mask = AllPlanes;

        gcdraw = (GContext)XCreateGC(Xdisplay, win,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);
        gctext = (GContext)XCreateGC(Xdisplay, win,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);
        gcfill = (GContext)XCreateGC(Xdisplay, win,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);

        Bitmaps[desc]->WindowId = win; /* store window id */
        Bitmaps[desc]->DrawId = win;   /* Indicates ON screen window */

        Bitmaps[desc]->XgcDraw = gcdraw;  /* store GC */
        Bitmaps[desc]->XgcFills = gcfill; /* store GC */
        Bitmaps[desc]->XgcText = gctext;  /* store GC */

        Bitmaps[desc]->PixmapId = 0; /* Indicates ON screen window */

        Bitmaps[desc]->eventmask = 0; /* make sure event mask is cleared */

        CurrentBitmapId = desc; /* make sure current bitmap */

        CreateBlankCursor(CurrentBitmapId);
        CreateWaitCursor(CurrentBitmapId);

        SetLocalDrawable(); /* set up drawables */

        GprWindowId[desc].windowid = 'A'; /* save for eventing codes */
        GprWindowId[desc].xwin = win;

        EventMask = ExposureMask | VisibilityChangeMask | StructureNotifyMask;
        Bitmaps[desc]->eventmask = EventMask;   /* save emask */
        XSelectInput(Xdisplay, win, EventMask); /* ok to enable events */

        EventInputDesc = desc; /* save descripter */

        exposing = 1;
        while (exposing)
        {

            XNextEvent(Xdisplay, &pe); /* this should collect first exposure event */
            if (pe.type == Expose)
            {
                ee = (XExposeEvent *)&pe;
                while (ee->count)
                {
                    XNextEvent(Xdisplay, &pe); /* get all expose events */
                    ee = (XExposeEvent *)&pe;
                }
                exposing = 0; /* clear flag */
            }
        }
    }

    else
    { /* not screen mapped HIDDEN bitmap */

        visual.visualid = CopyFromParent;

        pixwin = XCreateWindow(Xdisplay, /* create a dummy window */
                               RootWindow(Xdisplay, DefaultScreen(Xdisplay)),
                               winx, winy, winw, winh, 0,
                               0, InputOnly,
                               &visual,
                               0,
                               &xswa);

        planes = HiPlane + 1;

        pixmap = XCreatePixmap(Xdisplay,
                               pixwin,
                               winw,
                               winh,
                               planes);

        xgcv.function = GXcopy;

        xgcv.fill_style = FillSolid;
        xgcv.line_style = LineSolid;
        xgcv.foreground = GprBlackPixel;
        xgcv.background = GprWhitePixel;

        for (i = 0; i <= HiPlane; i++)
            xgcv.plane_mask |= (1L << i); /* set the number of planes */

        gcdraw = (GContext)XCreateGC(Xdisplay, pixmap,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);
        gctext = (GContext)XCreateGC(Xdisplay, pixmap,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);

        xgcv.foreground = GprBlackPixel;
        xgcv.background = GprBlackPixel;

        gcfill = (GContext)XCreateGC(Xdisplay, pixmap,
                                     GCFunction | GCFillStyle | GCPlaneMask | GCLineStyle | GCBackground | GCForeground, &xgcv);

        XFillRectangle(Xdisplay, /* clear the space */
                       pixmap,
                       gcfill,
                       (int)0, (int)0,
                       winw, winh);

        xgcv.foreground = GprBlackPixel;
        xgcv.background = GprWhitePixel;

        XChangeGC(Xdisplay,
                  gcfill,
                  GCForeground | GCBackground,
                  &xgcv);

        Bitmaps[desc]->WindowId = pixwin; /* store window id */

        Bitmaps[desc]->XgcDraw = gcdraw;  /* store GC */
        Bitmaps[desc]->XgcFills = gcfill; /* store GC */
        Bitmaps[desc]->XgcText = gctext;  /* store GC */

        Bitmaps[desc]->PixmapId = pixmap; /* Indicates Hidden */
        Bitmaps[desc]->DrawId = pixmap;   /* Actual drawble item */

        GprWindowId[desc].windowid = (unsigned char)255; /* this window cannot be used for eventing */
        GprWindowId[desc].xwin = 0;
    }

    /* must be calls here that use the GC after bitmap created */

    if (!ScreenMapped)
    { /* set up for hidden */
        tempdesc = CurrentBitmapId;
        GPR_$SET_BITMAP(&desc, &status);
    }

    window[0] = 0; /* set up default clip window */
    window[1] = 0;
    window[2] = Size[0];
    window[3] = Size[1];
    GPR_$SET_CLIP_WINDOW(window, &status);

    if (!ScreenMapped)
    { /* reset */
        GPR_$SET_BITMAP(&tempdesc, &status);
    }

    return 0;
}

/*

===========================================================================================

*/

void
    GPR_$OPEN_X_DISPLAY(XDisplayName, st)
    /* Description   :- Opens X display for use by GPR. Can be 
       *                  network based.
       * Return status :- st = 0 for success
       * Notes         :- 
       */
    char *XDisplayName; /*  <i> Display name */
GprStatus *st;          /*  <o> status */
{
    char *envdisplay;
    Font cfont;
    XColor w;
    short len;
    short id;
    GprStatus status;

    int width;
    int depth;
    int height;
    int border_width;
    int x, y;
    Window root;

    *st = 0;

    if (XDisplayName)
    {
        Xdisplay = XOpenDisplay(XDisplayName);
    }
    else
    {
        envdisplay = getenv("DISPLAY");
        if (!envdisplay)
        {
            envdisplay = getenv("display");
            if (!envdisplay)
                Xdisplay = XOpenDisplay(":0");
            else
                Xdisplay = XOpenDisplay(envdisplay);
        }
        else
            Xdisplay = XOpenDisplay(envdisplay);
    }

    if (!Xdisplay)
    {
        *st = GPR_CannotOpenDisplay;
        return;
    }

    SystemScreen = DefaultScreen(Xdisplay);
    SystemColormap = DefaultColormap(Xdisplay, SystemScreen);
    SystemDepth = DefaultDepth(Xdisplay, SystemScreen);

    DeterminePixels(); /* determine what black */
}

/*

===========================================================================================

*/

void
    GPR_$SET_AUTO_REFRESH(Auto, st)

    /* Description   :- Set the auto refresh of the current displayable
       *                  bitmap. Only main memory displayable window bitmaps
       *                  can be displayed.
       *
       * Return status :- The current bitmap is not displayable
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

    LOGICAL *Auto; /*  <i>  Logical to indicate whether to auto refresh */
GprStatus *st;     /*  <o>  return */

{

    XSetWindowAttributes xswa; /* Window attributes */
    int screen;
    Screen *scr;
    int supported;         /* Is this backing store supported */
    GprBitmapDesc desc;    /* new descripter for the auto refresh */
    GprBitmapDesc current; /* current descriipter for the window getting refresh */
    GprPlanes planes;      /* Highest planes Id */
    GprBitmapSize size;    /* The actual size eto be created */
    GprAttribute ablock;   /* attribute block not used */
    GprStatus status;
    GprBitmapDesc source;
    GprBitmapDesc dest;
    GprWindow window;
    GprOrigin org;

    *st = 0;

    if (Bitmaps[CurrentBitmapId]->PixmapId)
    {

        *st = GPR_BitmapIsNotWindow;
        return;
    }

    scr = ScreenOfDisplay(Xdisplay, DefaultScreen(Xdisplay));
    supported = DoesBackingStore(scr);

    if (supported)
    {
        if (*Auto)
        {
            xswa.backing_store = Always;
        }
        else
        {
            xswa.backing_store = NotUseful;
        }

        XChangeWindowAttributes(Xdisplay, /* change backing store as requseted */
                                Bitmaps[CurrentBitmapId]->WindowId,
                                CWBackingStore,
                                &xswa);
    }
    else
    { /* Not supported in this machine Must be rubbish */

        if (*Auto)
        {
            if (Bitmaps[CurrentBitmapId]->auto_refresh)
            { /* Allready has been set thus must free old one */
                KillAutoRefresh(CurrentBitmapId);
            }
            current = CurrentBitmapId;

            GPR_$INQ_BITMAP_DIMENSIONS(&current, size, &planes, &status);
            GPR_$ALLOCATE_BITMAP(size, &planes, &ablock, &desc, &status);

            if (status)
            {
                *st = GPR_CannotAllocateRefresh; /* cannot allocate the bitmap */
                return;
            }
            Bitmaps[CurrentBitmapId]->auto_refresh = F77true; /* store new backinjg store variables */
            Bitmaps[CurrentBitmapId]->auto_update = F77true;  /* update by default */
            Bitmaps[CurrentBitmapId]->backingstore = desc;

            source = CurrentBitmapId;        /* Save current bitmap ID  */
            GPR_$SET_BITMAP(&desc, &status); /* set to new bitmap */
            org[0] = 0;                      /* set up origin */
            org[1] = 0;
            window[0] = 0;
            window[1] = 0;
            window[2] = size[0];
            window[3] = size[1];
            GPR_$PIXEL_BLT(&source, window, org, &status); /* copy from window */
            GPR_$SET_BITMAP(&source, &status);             /*  set to old bitmap */
        }
        else
        {

            if (Bitmaps[CurrentBitmapId]->auto_refresh)
            { /* Allready has been set thus must free old one */
                KillAutoRefresh(CurrentBitmapId);
            }
        }
        SetLocalDrawable(); /* make sure all parameters updated */
    }
}

void
    KillAutoRefresh(Desc)

    /* Description   :- This routine is internal to GPRXLIB. It kills the auto refrsh bitmap
       * 
       * 
       *
       * Return status :- The status from gpr_deallocate_bitmap
       *                  
       *   
       *                  
       * Notes         :- This routine is quite simpple as it uses existing code.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprBitmapDesc Desc; /*	<i>	The descripter for the  window concerend */

{
    GprStatus status;
    GprBitmapDesc desc;

    desc = Bitmaps[Desc]->backingstore;

    GPR_$DEALLOCATE_BITMAP(&desc, &status); /* free the bitmap */
    Bitmaps[Desc]->auto_refresh = False;
    Bitmaps[Desc]->auto_update = False;
    Bitmaps[Desc]->backingstore = 0;
    GprAutoRefresh = False;
    GprAutoUpdate = False;
}

/*

===========================================================================================

*/

void
    GPR_$SET_BITMAP(Desc, st)

    /* Description   :- Sets X current drawbles and graphics context
       * 
       * 
       *
       * Return status :- st = 0 for success
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

    GprBitmapDesc *Desc; /* <i>  Bitmap descripter from gpr */
GprStatus *st;           /* <o>  return status */

{

    *st = 0;

    CurrentBitmapId = *Desc;

    if (!Bitmaps[*Desc])
    {
        *st = GPR_BadDescripter;
        return;
    }

    SetLocalDrawable(); /* set up drawables */
}

/*

===========================================================================================

*/

INT
    GPR_$LINE(X, Y, st)

    /* Description   :- This is the routine to draw line from the current GPR
       *                  position to the specified pos
       * 
       *
       * Return status :- St codes below. 0 for succcess
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

    Gprcoord *X; /*  <i> X coordinate */
Gprcoord *Y;     /*  <i> Y coordinate */
GprStatus *st;   /*  <o> status */

{

    int x1, y1, x2, y2;

    x2 = *X; /* get old pos */
    y2 = *Y;

    x1 = Bitmaps[CurrentBitmapId]->curpos[0];
    y1 = Bitmaps[CurrentBitmapId]->curpos[1];

    XDrawLine(Xdisplay, LocalDrawable, LocalGcDraw, x1, y1, x2, y2); /* draw the line */

    Bitmaps[CurrentBitmapId]->curpos[0] = x2; /* save new pos */
    Bitmaps[CurrentBitmapId]->curpos[1] = y2;

    if (GprAutoUpdate)
    {

        XDrawLine(Xdisplay, LocalAutoRefresh, LocalGcDraw, x1, y1, x2, y2); /* draw the line */
    }
}
/*

===========================================================================================

*/
INT
    GPR_$POINT(X, Y, st)

    /* Description   :- This is the routine to draw a PIXEL point a destination
       *                  It updates current bitmap position
       * 
       *
       * Return status :- St codes below. 0 for succcess
       *                  
       *   
       *                  
       * Notes         :- EXTENSION: Due to problems with 6000 X server
       *                             it seems impossible to draw points with a move
       *                             and line command into a HIDDEN bitmap. Thus
       *                             gpr_$point is used. 
       *                  
       *                  
       *                  
       *
       */

    Gprcoord *X; /*  <i> X coordinate */
Gprcoord *Y;     /*  <i> Y coordinate */
GprStatus *st;   /*  <o> status */

{

    int x1, y1;

    x1 = *X;
    y1 = *Y;

    XDrawPoint(Xdisplay, LocalDrawable, LocalGcDraw, x1, y1); /* draw the line */

    Bitmaps[CurrentBitmapId]->curpos[0] = x1; /* save new pos */
    Bitmaps[CurrentBitmapId]->curpos[1] = y1;

    if (GprAutoUpdate)
    {

        XDrawPoint(Xdisplay, LocalAutoRefresh, LocalGcDraw, x1, y1); /* draw the line */
    }
}
/*

===========================================================================================

*/

INT
    GPR_$MOVE(X, Y, st)

    /* Description   :- This moves the current postion within the bitmap
       *                  for GR ops
       * 
       *
       * Return status :- St codes below. 0 for succcess
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

    Gprcoord *X; /*  <i> X coordinate */
Gprcoord *Y;     /*  <i> Y coordinate */
GprStatus *st;   /*  <o> status */

{

    *st = 0;

    Bitmaps[CurrentBitmapId]->curpos[0] = *X; /* save new pos */
    Bitmaps[CurrentBitmapId]->curpos[1] = *Y;
}

/*

===========================================================================================

*/

void
    GPR_$ALLOCATE_BITMAP(Size, HiPlane, AttributeBlock, BitmapDesc, st)

    /* Description   :- Allocates a bitmap in X. This will be done as a hidden
       *                  bitmap descripter.
       * 
       *
       * Return status :- 
       *                  
       *   
       *                  void
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprBitmapSize Size;       /*  <i>  Size of bitmap to be defined */
GprPlanes *HiPlane;           /*  <i>  Depth of window */
GprAttribute *AttributeBlock; /*  <o>  Attribute block descripter */
GprBitmapDesc *BitmapDesc;    /*  <o>  ID of bitmap to be used */
GprStatus *st;                /*  <o>  Return status */

{

    *st = 0;

    *st = AllocateBitmap(Size, *HiPlane, False, BitmapDesc);

    return;
}

GPR_$INIT(OpMode, Unit, Size, HiPlane, BitmapDesc, st)

/* Description   :- Allocates a bitmap in X. This will be done as a hidden
       *                  bitmap descripter.
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

GprDisplayMode *OpMode;
short *Unit;

GprBitmapSize Size;        /*  <i>  Size of bitmap to be defined */
GprPlanes *HiPlane;        /*  <i>  Depth of window */
GprBitmapDesc *BitmapDesc; /*  <o>  ID of bitmap to be used */
GprStatus *st;             /*  <o>  Return status */

{

    int status;

    if (*OpMode != gpr_direct)
    {
        *st = GPR_ModeNotSupported;
        return;
    }

    *st = AllocateBitmap(Size, *HiPlane, True, BitmapDesc);

    GprColormapId = SystemColormap; /* set the colormap */
}

GPR_$PIXEL_BLT(SourceDesc, SourceWindow, DestOrigin, st)

/* Description   :- Performs a pixel block transfer from
       *                  the source bitmap to the current bitmap
       *                  
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- We use XCopyArea for this. Thus we must
       *                  have each bitmap as the same number of 
       *                  planes. Use bit_blt for indivual plane
       *                  copies
       *                  
       *                  
       *
       */

GprBitmapDesc *SourceDesc; /*  <i>  ID of bitmap to be used as the source */
GprWindow SourceWindow;    /*  <i>  Size of window of source */
GprOrigin DestOrigin;      /*  <i>  The origin of the new one */
GprStatus *st;             /*  <o>  Return status */

{

    Drawable sourcedrw;
    Drawable destdrw;
    int src_x;
    int src_y;
    unsigned int width;
    unsigned int height;
    int dest_x;
    int dest_y;

    *st = 0;

    sourcedrw = Bitmaps[*SourceDesc]->DrawId;   /* get source bitmap */
    destdrw = Bitmaps[CurrentBitmapId]->DrawId; /* get dest bitmap */

    src_x = SourceWindow[0];
    src_y = SourceWindow[1];
    width = SourceWindow[2];
    height = SourceWindow[3];

    dest_x = DestOrigin[0];
    dest_y = DestOrigin[1];

    XCopyArea(Xdisplay,
              sourcedrw,
              destdrw,
              LocalGcFill, /* local GC should be used here */
              src_x, src_y,
              width, height,
              dest_x, dest_y);

    if (GprAutoUpdate)
    { /* local auto refresh set here */
        XCopyArea(Xdisplay,
                  sourcedrw,
                  LocalAutoRefresh, /* update hidden bitmap */
                  LocalGcFill,      /* local GC should be used here */
                  src_x, src_y,
                  width, height,
                  dest_x, dest_y);
    }
}

void
    GPR_$DEALLOCATE_BITMAP(Desc, st)

    /* Description   :- Frees up the bitmap descripter. If a pixmap then
       *                  simply frees it with it GC . If a window then frees 
       *                  GC and deletes the window. Checks for auto refresh 
       *                  In the window and frees that one as well
       *
       * Return status :- Checks for valid desc
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

    GprBitmapDesc *Desc; /* <i>  The descripter */
GprStatus *st;

{

    GprBitmapDesc desc;
    GprStatus status;

    *st = 0;
    desc = 0;

    if (Bitmaps[*Desc]->auto_refresh)
    {
        desc = Bitmaps[*Desc]->backingstore;
    }

    GPR_$DEALLOCATE_BITMAP_X(Desc, &status);

    if (desc)
        GPR_$DEALLOCATE_BITMAP_X(&desc, &status); /* deallocate the pixmap */

    *st = status;
}

void
    GPR_$DEALLOCATE_BITMAP_X(Desc, st)

    /* Description   :- Frees up the bitmap descripter. If a pixmap then
       *                  simply frees it with it GC . If a window then frees 
       *                  GC and deletes the window.
       *
       * Return status :- Checks for valid desc
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

    GprBitmapDesc *Desc;
GprStatus *st;

{

    *st = 0;

    if (!ValidDesc(*Desc))
    {
        *st = GPR_BadDescripter;
        return;
    }

    if (Bitmaps[*Desc]->PixmapId)
    { /* just a pixmap to  be freed */

        XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcDraw);      /* GC */
        XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcFills);     /* GC */
        XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcText);      /* GC */
        XFreePixmap(Xdisplay, Bitmaps[*Desc]->PixmapId); /* main bitmap */
        free((char *)Bitmaps[*Desc]);
        Bitmaps[*Desc] = 0;
        return;
    }

    XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcDraw);  /* GC */
    XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcFills); /* GC */
    XFreeGC(Xdisplay, Bitmaps[*Desc]->XgcText);  /* GC */
    XDestroyWindow(Xdisplay, Bitmaps[*Desc]->WindowId);
    free((char *)Bitmaps[*Desc]);
    Bitmaps[*Desc] = 0;
}

INT
    ValidDesc(Desc)

    /* Description   :- returns True if the Bitmaps descripter has been defined 
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

    GprBitmapDesc Desc; /*  <i> Bitmap descripter */

{
    if (Desc < 0 || Desc >= MAXBITMAPS)
    {
        return False;
    }

    if (!Bitmaps[Desc])
    {
        return False;
    }

    return True;
}

GPR_$TERMINATE(Dummy, st)
/* Description   :- Terminates all windows and shuts down display
       * Return status :- None.
       * Notes         :- 
       */
int *Dummy;    /*  <i> Dummy for leaving frame */
GprStatus *st; /*  <o> GprStatus shpuld be ok */
{

    GprBitmapDesc i; /* descripter */
    GprStatus status;
    short f;

    /* SPB - 181194 - I've had to include this bit here to offset
a problem in gprx.c handling fonts in server macro mode ..... */
    extern LOGICAL server; /* Stored in calldax.c */
    if (server)
    {
        return;
    }

    if (!Xdisplay)
    { /* check to see if still open */
        *st = GPR_XdisplayNotOpen;
        return;
    }

    for (i = 0; i < MAXBITMAPS; i++)
    {
        if (Bitmaps[i])
        {
            GPR_$DEALLOCATE_BITMAP(&i, &status); /* shut down all windows */
        }
    }

    for (f = 0; f < MAXFONTS; f++)
    {
        if (GprFonts[f])
        {
            GPR_$UNLOAD_FONT_FILE(&f, &status); /* shut down all windows */
        }
    }

    if (PolygonOpen)
    {
        free((char *)Points); /* free all polygon data */
    }

    XCloseDisplay(Xdisplay); /* close display connection */
    Xdisplay = (Display *)0; /* set pointers clear */
}

/*

     =================================================


                   X    EVENTING SECTION


     =================================================

*/

void
    GPR_$ENABLE_INPUT(EventType, KeySet, st)

    /* Description   :- Enables different types of events to be
       *                  enabled. Each GPR type event is mapped
       *                  onto an equvalent x event.
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- Other X events will be enabled as extensions
       *                  to the GPR events
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprEventType *EventType; /*  <i> Event ype to be enabled */
GprKeyset KeySet;            /*  <i> Keyset not used here */
GprStatus *st;               /*  <o> GprStatus return */

{

    *st = 0; /*set status for correct args */

    EventMask = Bitmaps[CurrentBitmapId]->eventmask;

    switch (*EventType)
    {

    case gpr_keystroke: /* kepress events */

        EventMask |= KeyPressMask | KeyReleaseMask;
        break;

    case gpr_buttons: /* button events */

        EventMask |= ButtonPressMask | ButtonReleaseMask;
        break;

    case gpr_locator_update: /* updating as well */
    case gpr_locator:        /* locator events */

        EventMask |= PointerMotionMask | ButtonMotionMask;
        break;

    case gpr_entered_window: /* cursor enter window */

        EventMask |= EnterWindowMask;
        break;

    case gpr_left_window: /* cursor leaves window */

        EventMask |= LeaveWindowMask;
        break;

    case gpr_all_events: /* all events */

        EventMask = XAllEvents;
        break;

    case gpr_locator_stop: /* all other events not masked yet */
    case gpr_no_event:
    case gpr_dial:
    case gpr_coded_keys:
    case gpr_function_keys:
    case gpr_pfk:
    case gpr_physical_keys:
    case gpr_kbd_entered_window:
    case gpr_kbd_left_window:
    default:
        *st = GPR_EventNotSupported;
        return;
    }

    XSelectInput(Xdisplay, LocalDrawable, EventMask); /* ok to enable events */
    Bitmaps[CurrentBitmapId]->eventmask = EventMask;
}

GPR_$DISABLE_INPUT(EventType, st)

/* Description   :- Enables different types of events to be
       *                  disabled. Each GPR type event is mapped
       *                  onto an equvalent x event.
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- Other X events will be enabled as extensions
       *                  to the GPR events
       *                  
       *                  
       *                  
       *                  
       *
       */

GprEventType *EventType; /*  <i> Event ype to be enabled */
GprStatus *st;           /*  <o> GprStatus return */

{

    *st = 0; /*set status for correct args */

    EventMask = Bitmaps[CurrentBitmapId]->eventmask;

    switch (*EventType)
    {

    case gpr_keystroke: /* kepress events */

        EventMask &= ~KeyPressMask & ~KeyReleaseMask;
        break;

    case gpr_buttons: /* button events */

        EventMask &= ~ButtonPressMask & ~ButtonReleaseMask;
        break;

    case gpr_locator_update: /* updating as well */
    case gpr_locator:        /* locator events */

        EventMask &= ~PointerMotionMask & ~ButtonMotionMask;
        break;

    case gpr_entered_window: /* cursor enter window */

        EventMask &= ~EnterWindowMask;
        break;

    case gpr_left_window: /* cursor leaves window */

        EventMask &= ~LeaveWindowMask;
        break;

    case gpr_all_events: /* all events */

        EventMask = ~XAllEvents;
        break;

    case gpr_locator_stop: /* all other events not masked yet */
    case gpr_no_event:
    case gpr_dial:
    case gpr_coded_keys:
    case gpr_function_keys:
    case gpr_pfk:
    case gpr_physical_keys:
    case gpr_kbd_entered_window:
    case gpr_kbd_left_window:
    default:
        *st = GPR_EventNotSupported;
        return;
    }

    XSelectInput(Xdisplay, LocalDrawable, EventMask); /* ok to enable events */
    Bitmaps[CurrentBitmapId]->eventmask = EventMask;
}

INT
    GPR_$EVENT_WAIT(EventType, EventData, ScrPos, st)

    /* Description   :-  Waits for an event to occur. Return
       *                   Gpr event map as best it can ( Calles X version )
       *                 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :-This function should return a value to 
       *                 indicate that the window is unobscured.
       *                 Since X takes care of obscured windows we    
       *                 will return a value of true. This is F77 
       *                 true
       *                  
       *
       */

    GprEventType *EventType; /*  <o> Output event type */
char *EventData;             /*  <o> Output character data */
GprCursor ScrPos;            /*  <o> Current cursor position */
GprStatus *st;               /*  <o> Return state */

{

    int32_t data;

    *st = 0;
    GPR_$EVENT_WAIT_X(EventType, &data, ScrPos, st);

    *EventData = (char)data;

    return F77true; /* window is marked unobscured */
}

INT
    GPR_$EVENT_WAIT_X(EventType, EventData, ScrPos, st)

    /* Description   :-  Waits for an event to occur. Return
       *                   Gpr event map as best it can
       *                 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :-This function should return a value to 
       *                 indicate that the window is unobscured.
       *                 Since X takes care of obscured windows we    
       *                 will return a value of true. This is F77 
       *                 true
       *                  
       *
       */

    GprEventType *EventType; /*  <o> Output event type */
int32_t *EventData;          /*  <o> Output character data */
GprCursor ScrPos;            /*  <o> Current cursor position */
GprStatus *st;               /*  <o> Return state */

{

    XEvent event;                   /* main event union */
    XButtonEvent *button_event;     /* pointer to button pressed/released event */
    XKeyEvent *key_event;           /* pointer to keypress/release event */
    XMotionEvent *motion_event;     /* Pointer has moved */
    XCrossingEvent *crossing_event; /* Pointer entered/left window */
    XExposeEvent *ee;               /* pointer to expose requset */
    XConfigureEvent *cn;            /* pointer to expose requset */
    XConfigureRequestEvent *cr;     /* Confugre refresh */
    KeySym symbol;
    char buffer[10];
    XComposeStatus status;
    int ret;
    int i;
    int rect[4];
    int count;
    GprStatus gpr_status;
    GprBitmapDesc current;
    GprBitmapDesc desc;
    Window win;
    LOGICAL autorefresh;

    XNextEvent(Xdisplay, &event); /* get any event */

    /*   printf("Event %s\n",XeventNames[event.type]); */
    switch (event.type)
    {

    case KeyRelease: /* key is release */

        key_event = (XKeyEvent *)&event;

        ret = XLookupString(key_event,
                            buffer,
                            9,
                            &symbol,
                            &status);

        if (symbol == XK_Shift_L || /* special keys for locking */
            symbol == XK_Shift_R ||
            symbol == XK_Control_R ||
            symbol == XK_Control_L ||
            symbol == XK_Caps_Lock ||
            symbol == XK_Shift_Lock)
        {

            SetsKeyLocked(symbol, 0); /* set locks */
            *EventData = symbol;
            *EventType = gpr_physical_keys; /* not normal keyboard event number */
        }

        else
        {
            *EventData = symbol;
            *EventType = gpr_keystroke_up;
        }
        ScrPos[0] = key_event->x;
        ScrPos[1] = key_event->y;
        break;

    case KeyPress: /* key is pressed */

        key_event = (XKeyEvent *)&event;

        ret = XLookupString(key_event,
                            buffer,
                            9,
                            &symbol,
                            &status);

        if (symbol == XK_Shift_L || /* special keys for locking */
            symbol == XK_Shift_R ||
            symbol == XK_Control_R ||
            symbol == XK_Control_L ||
            symbol == XK_Caps_Lock ||
            symbol == XK_Shift_Lock)
        {

            SetsKeyLocked(symbol, 1);
            *EventData = symbol;
            *EventType = gpr_physical_keys; /* not normal keyboard event number */
        }

        else if (symbol == XK_Escape)
        {
            *EventData = 99;
            *EventType = gpr_buttons; /* not normal keyboard event number */
        }

        else
        {

            symbol = MapToKeypad(symbol);

            *EventData = symbol;
            *EventType = gpr_keystroke;
        }

        ScrPos[0] = key_event->x;
        ScrPos[1] = key_event->y;
        break;

    case ButtonPress: /* Button is pressed */

        button_event = (XButtonEvent *)&event;
        *EventType = gpr_buttons;

        ScrPos[0] = button_event->x;
        ScrPos[1] = button_event->y;

        switch (button_event->button)
        {

        case Button1:
            *EventData = 'a';
            break;
        case Button2:
            *EventData = 'b';
            break;
        case Button3:
            *EventData = 'c';
            break;
        case Button4:
            *EventData = 'd';
            break;
        }
        break;

    case ButtonRelease: /* Button is release */

        button_event = (XButtonEvent *)&event;
        *EventType = gpr_buttons;

        ScrPos[0] = button_event->x;
        ScrPos[1] = button_event->y;
        switch (button_event->button)
        {

        case Button1:
            *EventData = 'A';
            break;
        case Button2:
            *EventData = 'B';
            break;
        case Button3:
            *EventData = 'C';
            break;
        case Button4:
            *EventData = 'D';
            break;
        }

        break;

    case MotionNotify: /* cursor movement */

        motion_event = (XMotionEvent *)&event;

        *EventType = gpr_locator;
        ScrPos[0] = motion_event->x;
        ScrPos[1] = motion_event->y;
        *EventData = '\0';
        break;

    case EnterNotify: /* cursor enters the window */

        crossing_event = (XCrossingEvent *)&event;

        *EventType = gpr_entered_window;

        *EventData = 'A'; /* set a default window id */

        for (i = 0; i < MAXBITMAPS; i++)
        { /* tell him what window */

            if (GprWindowId[i].xwin == crossing_event->window)
            {
                *EventData = GprWindowId[i].windowid;
                desc = i;
                break;
            }
        }

        ScrPos[0] = crossing_event->x; /* set pos in new window */
        ScrPos[1] = crossing_event->y;
        if (GprAutoSwitch)
        {
            GPR_$SET_BITMAP(&desc, &gpr_status); /* automatically switch in */
        }

        break;

    case LeaveNotify: /* cursor enters the window */

        KeyLockModes = 0; /* clear the key locking modes */

        crossing_event = (XCrossingEvent *)&event;

        *EventType = gpr_left_window;

        *EventData = 'A'; /* set a default window id */

        for (i = 0; i < MAXBITMAPS; i++)
        { /* tell him what window */

            if (GprWindowId[i].xwin == crossing_event->window)
            {
                *EventData = GprWindowId[i].windowid;
            }
        }

        ScrPos[0] = crossing_event->x; /* set pos in new window */
        ScrPos[1] = crossing_event->y;
        break;

    case ConfigureNotify: /* Window has been moved or resized */

        *EventType = gpr_no_event;
        break;

    case Expose:

        if (GprWindowPaintProc)
            GprWindowPaintProc(&gpr_status);

        *EventType = gpr_no_event;

        break;

    default: /*  Unknown type of event */

        *EventType = gpr_no_event;
    }

    //	printf("Event Type %d - Data %d\n",*EventType,*EventData);

    return F77true; /* window is marked unobscured */
}

IsKeyLocked(Key)

    /* Description   :- Checks a a special key lock mode to make sure
       *                  that control characters and shifted characters are
       *                  returned.
       *
       * Return status :- True if that key is locked
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

    int Key; /*  <i> The GPRX key code number */

{

    return KeyLockModes & Key; /* return if the bit is set */
}

SetsKeyLocked(Key, Pressed)

    /* Description   :- Locks up a special key mixed modes are possible
       *                  
       *                  returned.
       *
       * Return status :- True if that key is locked
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

    int Key; /*  <i> The X key code number */
int Pressed; /*  <i> Indicates true for pressed and false for released */

{

    if (Pressed)
    {

        switch (Key)
        {

        case XK_Shift_L:
        case XK_Shift_R:

            KeyLockModes |= SHIFT_P;
            break;

        case XK_Control_L:
        case XK_Control_R:

            KeyLockModes |= CTRL_P;
            break;

        case XK_Caps_Lock:
            KeyLockModes |= CAPS_L;
            break;
        case XK_Shift_Lock:
            KeyLockModes |= SHIFT_L;
            break;
        }
    }

    else
    { /* clear any set locks */

        switch (Key)
        {

        case XK_Shift_L:
        case XK_Shift_R:

            KeyLockModes &= ~SHIFT_P;
            break;

        case XK_Control_L:
        case XK_Control_R:

            KeyLockModes &= ~CTRL_P;
            break;

        case XK_Caps_Lock:
            KeyLockModes &= ~CAPS_L;
            break;
        case XK_Shift_Lock:
            KeyLockModes &= ~SHIFT_L;
            break;
        }
    }
}

INT
    GPR_$COND_EVENT_WAIT(EventType, EventData, ScrPos, st)

    /* Description   :-  Checks for for an event to occur. Return immediatly
       *                   Gpr event map as best it can ( Calles X version )
       *                 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :-This function should return a value to 
       *                 indicate that the window is unobscured.
       *                 Since X takes care of obscured windows we    
       *                 will return a value of true. This is F77 
       *                 true
       *                  
       *
       */

    GprEventType *EventType; /*  <o> Output event type */
char *EventData;             /*  <o> Output character data */
GprCursor ScrPos;            /*  <o> Current cursor position */
GprStatus *st;               /*  <o> Return state */

{

    int32_t data;

    *st = 0;

    GPR_$COND_EVENT_WAIT_X(EventType, &data, ScrPos, st);

    *EventData = (char)data;

    return F77true; /* window is marked unobscured */
}

INT
    GPR_$COND_EVENT_WAIT_X(EventType, EventData, ScrPos, st)

    /* Description   :-  Checks for for an event to occur. Return immediatly
       *                   Gpr event map as best it can.
       *                 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :-This function should return a value to 
       *                 indicate that the window is unobscured.
       *                 Since X takes care of obscured windows we    
       *                 will return a value of true. This is F77 
       *                 true
       *                  
       *
       */

    GprEventType *EventType; /*  <o> Output event type */
int32_t *EventData;          /*  <o> Output character data */
GprCursor ScrPos;            /*  <o> Current cursor position */
GprStatus *st;               /*  <o> Return state */

{

    int count;

    *st = 0;

    count = XPending(Xdisplay);

    if (count)
        GPR_$EVENT_WAIT_X(EventType, EventData, ScrPos, st);
    else
        *EventType = gpr_no_event;
}

INT
    GPR_$CLEAR(Index, st)

    /* Description   :- Sets all pixels in the current bitmap to 
       *                  the given color index number.
       *                 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :-This routine will TEMPORARILY set the GC for
       *                 the bitmap and reset after clearing.
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprColor *Index; /*  <i> Pixel map value to set all colormap */
GprStatus *st;       /*  <o> Return status */

{

    Window root;                /* root window ID */
    int x, y;                   /* location */
    unsigned int width, height; /* what do you think */
    unsigned int border_width;  /* what do you think */
    unsigned int depth;         /* depth of window */
    GprColor temp;              /* temp color */
    GprStatus status;

    *st = 0;

    XGetGeometry(Xdisplay, /* get gemetry of window */
                 Bitmaps[CurrentBitmapId]->DrawId,
                 &root,
                 &x, &y,
                 &width, &height,
                 &border_width,
                 &depth);

    temp = Bitmaps[CurrentBitmapId]->draw;

    TOOLPEN(Index);

    x = 0; /* size is window relative not root */
    y = 0;

    XFillRectangle(Xdisplay,
                   LocalDrawable,
                   LocalGcDraw,
                   (int)x, (int)y,
                   width, height);

    if (GprAutoUpdate)
    { /* update hidden */

        XFillRectangle(Xdisplay,
                       LocalAutoRefresh,
                       LocalGcDraw,
                       (int)x, (int)y,
                       width, height);
    }

    TOOLPEN(&temp);
}

INT
    GPR_$SET_TEXT_VALUE(Index, st)

    /* Description   :- Sets current global text drawing colour
       *                  on the bitmap to the specified index value
       *                 
       *
       * Return status :- Only range control
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

    GprColor *Index; /*  <i> Pixel map value to set all colormap */
GprStatus *st;       /*  <o> Return status */

{
    XGCValues gv;
    int index;
    uint32_t pixel;

    index = *Index;
    *st = 0;
    if (*Index == -2)
    {
        gv.foreground = GprWhitePixel; /* set default background vals */
        index = GprWhitePixel;
    }
    else
        gv.foreground = index & Bitmaps[CurrentBitmapId]->planemask; /* mask up all values */

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcText,
              GCForeground,
              &gv);

    pixel = *Index;
    XSetForeground(Xdisplay, Bitmaps[CurrentBitmapId]->XgcText, pixel);

    Bitmaps[CurrentBitmapId]->text_f = *Index; /* save color */
}

GPR_$SET_TEXT_BACKGROUND_VALUE(Index, st)

/* Description   :- Sets the background colour for text. 
       *                  
       * 
       *
       * Return status :- NONE
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

GprColor *Index;
GprStatus *st;

{

    XGCValues gv;
    int index;
    uint32_t pixel;

    *st = 0;
    index = *Index;

    if (*Index == -2 || *Index == -1)
    {
        gv.background = GprBlackPixel;
        Bitmaps[CurrentBitmapId]->text_b = gv.background; /* save color */
    }
    else
    {
        gv.background = index & Bitmaps[CurrentBitmapId]->planemask; /* mask up all values */
        Bitmaps[CurrentBitmapId]->text_b = *Index;                   /* save color */
    }

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcText,
              GCBackground,
              &gv);

    pixel = *Index;
    XSetBackground(Xdisplay, Bitmaps[CurrentBitmapId]->XgcText, pixel);
}

INT
    GPR_$SET_FILL_VALUE(Index, st)

    /* Description   :- Sets current global fill drawing colour
       *                  on the bitmap to the specified index value
       *                 
       *
       * Return status :- Only range control
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

    GprColor *Index; /*  <i> Pixel map value to set all colormap */
GprStatus *st;       /*  <o> Return status */

{

    XGCValues gv;
    int index;
    uint32_t pixel;

    *st = 0;

    index = *Index;
    if (*Index == -2)
    {
        gv.foreground = GprWhitePixel; /* set default background vals */
        index = GprWhitePixel;
    }
    else
        gv.foreground = index & Bitmaps[CurrentBitmapId]->planemask; /* mask up all values */

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcFills,
              GCForeground,
              &LocalGcFill);

    pixel = *Index;

    XSetForeground(Xdisplay, Bitmaps[CurrentBitmapId]->XgcFills, pixel);

    Bitmaps[CurrentBitmapId]->fill_f = *Index; /* save color */
}

INT
    GPR_$SET_FILL_BACKGROUND_VALUE(Index, st)

    /* Description   :- Sets current global drawing background colour
       *                  on the bitmap to the specified index value
       *                 
       *
       * Return status :- Only range control
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

    GprColor *Index; /*  <i> Pixel map value to set all colormap */
GprStatus *st;       /*  <o> Return status */

{

    XGCValues gv;
    int index;
    uint32_t pixel;

    *st = 0;

    index = *Index;

    if (*Index == -2)
    {
        gv.background = GprWhitePixel; /* set default background vals */
        index = GprWhitePixel;
    }
    else
        gv.background = index & Bitmaps[CurrentBitmapId]->planemask; /* mask up all values */

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcFills,
              GCBackground,
              &gv);

    pixel = *Index;
    XSetBackground(Xdisplay, Bitmaps[CurrentBitmapId]->XgcFills, pixel);

    Bitmaps[CurrentBitmapId]->fill_b = *Index; /* save color */
}

INT
    GPR_$SET_DRAW_VALUE(Index, st)

    /* Description   :- Sets current global drawing color
       *                  on the bitmap to the specified index value
       *                 
       *
       * Return status :- Only range control
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

    GprColor *Index; /*  <i> Pixel map value to set all colormap */
GprStatus *st;       /*  <o> Return status */

{

    XGCValues gv;
    int index;
    uint32_t pixel;

    *st = 0;
    index = *Index;

    if (*Index == -2)
    {
        gv.foreground = GprWhitePixel; /* set default background vals */
        index = GprWhitePixel;
    }
    else
        gv.foreground = (index & Bitmaps[CurrentBitmapId]->planemask); /* mask up all values */

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcDraw,
              GCForeground,
              &gv);

    pixel = *Index;
    XSetForeground(Xdisplay, Bitmaps[CurrentBitmapId]->XgcDraw, pixel);

    Bitmaps[CurrentBitmapId]->draw = *Index; /* save color */
}

void
    GPR_$SET_WINDOW_ID(Id, st)

    /* Description   :- Sets the input descripter for the current
       *                  bitmaps window. Cannot be a pixmap
       *                 
       *
       * Return status :- IF not pixmap then error generated
       *                  
       *   
       *                  
       * Notes         :-Exactly maps the Gpr action
       *                  
       *                  
       *                  
       *                  
       *
       */

    char *Id;
GprStatus *st;

{

    *st = 0;

    if (Bitmaps[CurrentBitmapId]->PixmapId)
    {
        *st = GPR_NotInputWindow;
        return;
    }

    GprWindowId[CurrentBitmapId].windowid = *Id; /* indicate the character */
}

INT
    GPR_$SET_WINDOW_START(Pos, st)

    /* Description   :- Extension to gpr to allow window to be defined
       *                  at a particular origin.
       * 
       *
       * Return status :- Coordinate could be out of range 
       *                  
       *   
       *                  
       * Notes         :- This routine will allow actual window postions to be 
       *                  specified. This is a modal command and is called before
       *                  each gpr_$init.
       *                  
       *                  
       *                  
       *
       */

    GprOrigin Pos; /* <i>  Coordinate position of window */
GprStatus *st;     /* <o>  return status */

{

    *st = 0;

    GprWindowOrg[0] = Pos[0]; /* save to globals */
    GprWindowOrg[1] = Pos[1];
}

INT
    GPR_$SET_LINE_WIDTH(Width, EndStyle, st)

    /* Description   :- (Extension) Sets the line draiwng width in pixels
       *                  Also allows an end shape to be defined.
       * 
       *
       * Return status :- Style Not recognised.
       *                  
       *   
       *                  
       * Notes         :- This is a good extension to gpr as it allows lines
       *                  to be drawn with thickness as well as an end style
       *                  
       *                  
       *                  
       *                  
       *
       */

    int *Width;
GprEndStyle *EndStyle;
GprStatus *st;

{

    XGCValues gv; /* GC values */

    *st = 0;

    gv.line_width = *Width;
    gv.cap_style = *EndStyle;

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcDraw,
              GCLineWidth | GCCapStyle,
              &gv);

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcFills,
              GCLineWidth | GCCapStyle,
              &gv);
}

INT
    GPR_$SET_LINESTYLE(Style, Scale, st)

    /* Description   :- Sets the line-style attribute of current drawable
       *                  
       * 
       *
       * Return status :- Should be ok any scale is mapped to 1 if outsize a good range.
       *                  
       *   
       *                  
       * Notes         :- I think this does what GPR does.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprLineStyle *Style; /*  <i> Stye of line */
short *Scale;            /*  <i> scale of pattern */
GprStatus *st;           /*  <o> return */

{

    XGCValues gv; /* GC values */

    char dotted[2];
    int scale;

    *st = 0;

    if (*Style == gpr_solid)
    {

        gv.line_style = LineSolid;
        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcDraw,
                  GCLineStyle,
                  &gv);
    }
    else
    {

        if (*Scale <= 0)
            scale = 1;
        else
            scale = *Scale;

        gv.line_style = LineOnOffDash;

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcDraw,
                  GCLineStyle,
                  &gv);

        sprintf(dotted, "%c%c", (char)scale, (char)scale);

        XSetDashes(Xdisplay,
                   Bitmaps[CurrentBitmapId]->XgcDraw,
                   0,
                   dotted,
                   2);
    }
}

void
    GPR_$SET_RASTER_OP(PlaneId, RasterOp, st)

    /* Description   :- Sets the raster op for all GPR ops.
       * 
       * 
       *
       * Return status :- If planes outwith current limits.
       *                  
       *   
       *                  
       * Notes         :- This one could set planes individualy but its
       *                  not going to. We want all planes to be set a once.
       *                  
       *                  
       *                  
       *                  
       *
       */

    GprPlanes *PlaneId; /*  <i> Plane number ( not used currently ) */
GprRasterOp *RasterOp;  /*  <i> raster op. Maps directly onto X */
GprStatus *st;

{

    XGCValues gv; /* GC values */
    int draw;
    int fill;

    *st = 0;

    if (*RasterOp < 0 || *RasterOp > 15)
    {

        *st = GPR_XRasterOpInvalid;
        return;
    }

    gv.plane_mask = (uint32_t)(-1);
    gv.function = *RasterOp; /* set raster op now */

    /* make sure X sees it */
    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcDraw,
              GCFunction | GCPlaneMask,
              &gv);

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcFills,
              GCFunction | GCPlaneMask,
              &gv);

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcText,
              GCFunction | GCPlaneMask,
              &gv);

    Bitmaps[CurrentBitmapId]->rasterop = *RasterOp; /* save into struct */
}

GPR_$LOAD_FONT_FILE(FontName, Length, FontId, st)

/* Description   :- Loads an X font file.
       * 
       * 
       *
       * Return status :- Unable to load fonts
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

char *FontName; /*  <i> The font name. Must ne X name          */
short *Length;  /*  <i> Length of name                         */
short *FontId;  /*  <o> Output fontid. Reference to GPR array  */
GprStatus *st;  /*  <o> status                                 */

{

    int i;
    int length;
    int used;
    char *stemp;
    XFontStruct *xfont;

    //printf("size of long %d", sizeof(*st));

    *st = 0;

    *FontId = 0;

    length = *Length;
    used = -1;

    if (*Length == 0 || *Length > 1024)
    {
        *st = GPR_LengthNotValid;
        return;
    }

    stemp = (char *)malloc((unsigned)length + 1); /* get local space for font name */
    strncpy(stemp, FontName, length);
    stemp[length] = '\0'; /* nullyfy */

    for (i = 0; i < MAXFONTS; i++)
    {

        if (GprFonts[i])
        {
            if (!strcmp(GprFonts[i]->fontname, stemp))
            {
                *st = 0;
                free((char *)stemp); /* deallocate temp space */
                *FontId = i;         /* return the font id that is allready loaded */
                return;
            }
        }
        else if (used == -1)
            used = i; /* first unused one in the list */
    }

    /* load font */

    if (!(xfont = XLoadQueryFont(Xdisplay, stemp)))
    { /* try to load the font */

        free((char *)stemp); /* deallocate temp space */
        *st = GPR_CannotLoadFont;
        return;
    }

    GprFonts[used] = (GprFontStruct *)malloc((unsigned)sizeof(GprFontStruct)); /* malloc area for font storage */

    GprFonts[used]->fontname = (char *)malloc((unsigned)length + 1); /* font name */

    strncpy(GprFonts[used]->fontname, FontName, length); /* copy it */

    GprFonts[used]->XGPRFont = xfont; /* set pointer */

    *FontId = used;

    free((char *)stemp); /* deallocate temp space */
}

void
    GPR_$UNLOAD_FONT_FILE(FontId, st)

    /* Description   :- Unloads a previously loaded font file.
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

    short *FontId; /*  <i> fontid. Reference to GPR array         */
GprStatus *st;     /*  <o> status                                 */

{

    int font;
    Font xfont;

    *st = 0;
    font = *FontId;

    if (font < 0 || font >= MAXFONTS)
    {

        *st = GPR_FontidOutOfRange;
        return;
    }

    if (!GprFonts[font])
    {

        *st = GPR_FontNotLoaded;
        return;
    }

    xfont = GprFonts[font]->XGPRFont->fid;

    XUnloadFont(Xdisplay, xfont);

    free((char *)GprFonts[font]->fontname); /* free named area */
    free((char *)GprFonts[font]);           /* free this one */
    GprFonts[font] = 0;                     /* set pointers back */
}

void
    GPR_$SET_TEXT_FONT(FontId, st)

    /* Description   :- Loads the text font for the current drawble bitmap 
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

    short *FontId; /*  <i> fontid. Reference to GPR array         */
GprStatus *st;     /*  <o> status                                 */

{

    int font;
    Font xfont;
    XGCValues gv;

    *st = 0;

    font = *FontId;

    if (font < 0 || font >= MAXFONTS)
    {

        *st = GPR_FontidOutOfRange;
        return;
    }

    if (!GprFonts[font])
    {

        *st = GPR_FontNotLoaded;
        return;
    }

    xfont = GprFonts[font]->XGPRFont->fid;

    Bitmaps[CurrentBitmapId]->fid = xfont; /* store local copy */

    gv.font = xfont;

    Bitmaps[CurrentBitmapId]->gprfid = font; /* save current loaded descripter */

    XChangeGC(Xdisplay,
              Bitmaps[CurrentBitmapId]->XgcText,
              GCFont,
              &gv);
}

GPR_$TEXT(String, Length, st)

/* Description   :- Draws text string with current GC values.
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

char *String;  /*  <i> The text string to be drawn */
short *Length; /*  <i> The length */
GprStatus *st;

{
    int length;       /* local length */
    int x, y;         /* position */
    short size[2];    /* ne postion after draw */
    GprStatus status; /* local status */

    length = *Length;

    x = Bitmaps[CurrentBitmapId]->curpos[0];
    y = Bitmaps[CurrentBitmapId]->curpos[1];

    XDrawImageString(Xdisplay, LocalDrawable, LocalGcText, x, y, String, length); /* draw the text */

    if (GprAutoUpdate)
    {

        XDrawImageString(Xdisplay, LocalAutoRefresh, LocalGcText, x, y, String, length); /* draw the text */
    }

    GPR_$INQ_TEXT_EXTENT(String, Length, size, &status); /* get new position */

    Bitmaps[CurrentBitmapId]->curpos[0] += size[0]; /* update next drawing postion */
}

void GPR_$SET_CLIPPING_ACTIVE(ClipIt, st)

    /* Description   :- Sets clipping active on the current bitmap
       * 
       * 
       *
       * Return status :- Allways sets it unless a region has not been defined
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

    LOGICAL *ClipIt; /*  <i> indicates whether to clip or not */
GprStatus *st;

{

    XGCValues gv; /* Graphics context values  */

    *st = 0;
    if (!Bitmaps[CurrentBitmapId]->clipregion)
    { /* no region defined */
        *st = GPR_NoRegion;
        GPRXError("gpr_$set_clipping_active",
                  "No region has been defined", 0);
        return;
    }

    if (*ClipIt)
    {

        XSetRegion(Xdisplay,
                   Bitmaps[CurrentBitmapId]->XgcDraw,
                   Bitmaps[CurrentBitmapId]->clipregion);

        XSetRegion(Xdisplay,
                   Bitmaps[CurrentBitmapId]->XgcText,
                   Bitmaps[CurrentBitmapId]->clipregion);

        XSetRegion(Xdisplay,
                   Bitmaps[CurrentBitmapId]->XgcFills,
                   Bitmaps[CurrentBitmapId]->clipregion);

        gv.clip_x_origin = Bitmaps[CurrentBitmapId]->clipwindow[0];
        gv.clip_y_origin = Bitmaps[CurrentBitmapId]->clipwindow[1];

        Bitmaps[CurrentBitmapId]->clipping = True;

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcDraw,
                  GCClipXOrigin | GCClipYOrigin,
                  &gv);

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcFills,
                  GCClipXOrigin | GCClipYOrigin,
                  &gv);

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcText,
                  GCClipXOrigin | GCClipYOrigin,
                  &gv);
    }
    else
    {

        gv.clip_mask = 0;

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcDraw,
                  GCClipMask,
                  &gv);

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcText,
                  GCClipMask,
                  &gv);

        XChangeGC(Xdisplay,
                  Bitmaps[CurrentBitmapId]->XgcFills,
                  GCClipMask,
                  &gv);
        Bitmaps[CurrentBitmapId]->clipping = False;
    }
}

GPR_$SET_CLIP_WINDOW(ClipWindow, st)

/* Description   :- Defines a clip region for the current bitmap
       *                  but does not actually set it. It uses set clipping active
       * 
       *
       * Return status :- Window too big;
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

short ClipWindow[4]; /*  <i> The window size and origin */
GprStatus *st;

{

    Region r;
    XRectangle rect;

    if (Bitmaps[CurrentBitmapId]->clipregion)
    {
        r = Bitmaps[CurrentBitmapId]->clipregion; /* get current region */
        XDestroyRegion(r);
    }

    if (!(r = XCreateRegion()))
    { /* try to create a new region */
        *st = GPR_NoRegion;
        GPRXError("gpr_$set_clip_window",
                  "Cannot create region", 0);
        return;
    }

    Bitmaps[CurrentBitmapId]->clipregion = r;

    Bitmaps[CurrentBitmapId]->clipwindow[0] = ClipWindow[0];
    Bitmaps[CurrentBitmapId]->clipwindow[1] = ClipWindow[1];
    Bitmaps[CurrentBitmapId]->clipwindow[2] = ClipWindow[2];
    Bitmaps[CurrentBitmapId]->clipwindow[3] = ClipWindow[3];

    rect.x = 0;
    rect.y = 0;
    rect.width = (unsigned short)ClipWindow[2];
    rect.height = (unsigned short)ClipWindow[3];

    XUnionRectWithRegion(&rect, r, r);

    if (Bitmaps[CurrentBitmapId]->clipping)
    {

        GPR_$SET_CLIPPING_ACTIVE(&Bitmaps[CurrentBitmapId]->clipping, &st);
    }
}

GPR_$RECTANGLE(Rect, st)

/* Description   :- Draws a filled rectangle of the current color
       * 
       * 
       *
       * Return status :- None
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

short Rect[4]; /*  <i>     The rectange to be filled */
GprStatus *st; /*  <o>     nothing */

{
    int x, y;
    int width;
    int height;

    *st = 0;

    x = Rect[0];
    y = Rect[1];
    width = Rect[2];
    height = Rect[3];

    XFillRectangle(Xdisplay,
                   LocalDrawable,
                   LocalGcFill,
                   (int)x, (int)y,
                   width, height);

    if (GprAutoUpdate)
    { /* update hidden bitmap */

        XFillRectangle(Xdisplay,
                       LocalAutoRefresh,
                       LocalGcFill,
                       (int)x, (int)y,
                       width, height);
    }
}

INT
    GPR_$DRAW_BOX(X1, Y1, X2, Y2, st)

    /* Description   :- Draws a box based on dimension supplied.
       * 
       * 
       *
       * Return status :- None
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

    Gprcoord *X1; /*  <i> Left edge */
Gprcoord *Y1;     /*  <i> Top edge */
Gprcoord *X2;     /*  <i> Right edge */
Gprcoord *Y2;     /*  <i> Bottom edge */
GprStatus *st;

{

    int x, y;
    unsigned int width;
    unsigned int height;

    *st = 0;

    if (*X1 > *X2)
    { /* swap cos some x systems dont like it */
        x = *X1;
        *X1 = *X2;
        *X2 = x;
    }
    if (*Y1 > *Y2)
    {
        y = *Y1;
        *Y1 = *Y2;
        *Y2 = y;
    }
    x = *X1;
    y = *Y1;
    width = *X2 - x;
    height = *Y2 - y;

    XDrawRectangle(Xdisplay,
                   LocalDrawable,
                   LocalGcDraw,
                   (int)x, (int)y,
                   width, height);

    if (GprAutoUpdate)
    { /* update hidden bitmap */

        XDrawRectangle(Xdisplay,
                       LocalAutoRefresh,
                       LocalGcDraw,
                       (int)x, (int)y,
                       width, height);
    }
}

void GPR_$MULTILINE(Xarray, Yarray, Npositions, st)

    /* Description   :- Draws a seriaes of disconnected lines
       * 
       * 
       *
       * Return status :- None
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

    Gprcoord *Xarray; /* <i>  List of X coords */
Gprcoord *Yarray;     /* <i>  List of Y coords */
Gprcoord *Npositions; /* <i>  Number of coords */
GprStatus *st;

{

    int i;
    Gprcoord x1, y1;
    int pos;
    int move;

    *st = 0;

    if (!*Npositions)
        return;

    move = 1; /* first a move */
    pos = (int)*Npositions;

    for (i = 0; i < pos; i++)
    {

        x1 = Xarray[i];
        y1 = Yarray[i];

        if (move)
            GPR_$MOVE(&x1, &y1, &st);
        else
            GPR_$LINE(&x1, &y1, &st);

        move = !move;
    }
}

void
    GPR_$POLYLINE(Xarray, Yarray, Npositions, st)

    /* Description   :- Draws a seriaes of connected lines
       * 
       * 
       *
       * Return status :- None
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

    Gprcoord *Xarray; /* <i>  List of X coords */
Gprcoord *Yarray;     /* <i>  List of Y coords */
Gprcoord *Npositions; /* <i>  Number of coords */
GprStatus *st;

{

    int i;
    Gprcoord x1, y1;
    int pos;
    int move;

    *st = 0;

    if (!*Npositions)
        return;

    move = 1; /* first a move */
    pos = (int)*Npositions;

    x1 = Xarray[0];
    y1 = Yarray[0];
    GPR_$MOVE(&x1, &y1, &st);

    for (i = 1; i < pos; i++)
    {

        x1 = Xarray[i];
        y1 = Yarray[i];

        GPR_$LINE(&x1, &y1, &st);
    }
}

/*

   =========================================================================

*/

INT
    GPR_$SET_REFRESH_ENTRY(WindowPaintProc, WindowRefreshEntry, st)

    /* Description   :- Sets up functions that will be called 
       *                  when refreshing of screen is called.
       * 
       *
       * Return status :- None
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

    int (*WindowPaintProc)(); /* <i>  called when change in visiblity */
int (*WindowRefreshEntry)();  /* <i>  called when display is aquired Not supported yet */
GprStatus *st;

{

    *st = 0;
    GprWindowPaintProc = WindowPaintProc;
}

/*

   =========================================================================

*/

INT
    GPR_$SET_PLANE_MASK(PlaneMask, st)

    /* Description   :- Sets the plane mask of the current bitmap Nothing will 
       *                  come of this execpt the routine gpr_$inq_constraints
       * 
       *
       * Return status :- Nothing
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

    short *PlaneMask; /*  <i> The mask of whiuch planes to use */
GprStatus *st;

{

    *st = 0;
    Bitmaps[CurrentBitmapId]->planemask = *PlaneMask;
    Bitmaps[CurrentBitmapId]->planemask = (unsigned int)-1;
}

/*

   =========================================================================

*/

INT
    GPR_$INQ_CONSTRAINTS(ClipWindow, Active, PlaneMask, st)

    /* Description   :- Returns clipping information about current bitmap
       * 
       * 
       *
       * Return status :- Nothing
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

    short ClipWindow[4];
LOGICAL *Active;
short *PlaneMask;
GprStatus *st;

{

    *st = 0;

    ClipWindow[0] = Bitmaps[CurrentBitmapId]->clipwindow[0]; /* clip window */
    ClipWindow[1] = Bitmaps[CurrentBitmapId]->clipwindow[1];
    ClipWindow[2] = Bitmaps[CurrentBitmapId]->clipwindow[2];
    ClipWindow[3] = Bitmaps[CurrentBitmapId]->clipwindow[3];

    if (Bitmaps[CurrentBitmapId]->clipping) /* is clipping active at all */
        *Active = F77true;
    else
        *Active = F77false;

    *PlaneMask = Bitmaps[CurrentBitmapId]->planemask; /* the plane mask */
}

/*

   =========================================================================

*/

void
    GPR_$SET_X_CURSOR(CursorPattern, st)

    /* Description   :- Defines and draws an X cursor on the screen. The numbers
       *                  releate to the standard cursor list.
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

    int *CursorPattern; /*  <i> The X cursor pattern number */
GprStatus *st;

{

    Cursor curs;

    if (Bitmaps[CurrentBitmapId]->PixmapId)
    {

        *st = GPR_BitmapIsNotWindow;
        return;
    }

    curs = Bitmaps[CurrentBitmapId]->fontcursor;

    if (curs)
    { /* Check cursor needed  */

        XFreeCursor(Xdisplay, curs); /* undefine it now */
    }

    curs = XCreateFontCursor(Xdisplay, *CursorPattern);

    if (!curs)
    {

        *st = GPR_NoCursor;
        return;
    }

    Bitmaps[CurrentBitmapId]->fontcursor = curs;    /* save new cursor ID pattern */
    Bitmaps[CurrentBitmapId]->currentcursor = curs; /* set this cursor as the current one */
}

/*

   =========================================================================

*/

GPR_$SET_WAIT_CURSOR(st)

/* Description   :- This routine sets the system wait cursor
       *                  It does not set it immediatly. You must call 
       *                  gpr_$set_cursor_active to invoke it 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- EXTENSION: This will simply set the predined wait style cursor
       *                             Since X has good cursor facilitues we took advantage of this
       *                             to edit the cursor use the bitmap program on the file:-
       *                             GPRX_wait_cursor.h to modify the bits etc. Recompile and you
       *                             will have a new wait cursor.
       *                  
       *                  
       *                  
       *
       */

GprStatus *st;

{

    *st = 0;
    Bitmaps[CurrentBitmapId]->currentcursor = Bitmaps[CurrentBitmapId]->wait;
}

INT
    GPR_$SET_CURSOR_ACTIVE(Active, st)

    /* Description   :- Specifies whether X cursor is displayed 
       * 
       * 
       *
       * Return status :- None
       *                  
       *   
       *                  
       * Notes         :- Mimic gpr action. Use gpr_$set_x_cursor to set 
       *                  X defined cursor fonts.
       *                  
       *                  
       *                  
       *                  
       *
       */

    LOGICAL *Active; /* <i>  Logical assumes that X cursor will be displayed */
GprStatus *st;       /* <o>  Clear */

{
    Cursor curs;

    if (*Active)
    { /* turn on */

        curs = Bitmaps[CurrentBitmapId]->currentcursor;

        if (!curs)
            curs = None;

        XDefineCursor(Xdisplay,
                      Bitmaps[CurrentBitmapId]->WindowId,
                      curs);
    }

    else
    {

        curs = Bitmaps[CurrentBitmapId]->blank;
        XDefineCursor(Xdisplay,
                      Bitmaps[CurrentBitmapId]->WindowId,
                      curs);
    }
}

/*

   =========================================================================

*/

INT
    GPR_$START_PGON(X, Y, st)

    /* Description   :- Stars Gpr polygon list. Also closes any currently open list
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

    Gprcoord *X; /*  <i>  The starting X coordinate of the polygon */
Gprcoord *Y;     /*  <i>  The starting Y coordinate of the polygon */
GprStatus *st;

{

    short x;
    short y;
    XPoint *p;
    XPoint *f;

    *st = 0;

    x = *X;
    y = *Y;

    if (PolygonFirst)
    {

        AllocPoint(); /* allocate some ememomory for next point */

        p = &Points[PolygonOpen];
        f = &Points[PolygonFirst];

        p->x = f->x;
        p->y = f->y;

        PolygonOpen++;
    }

    AllocPoint(); /* allocate some ememomory for next point */

    p = &Points[PolygonOpen];
    p->x = x;
    p->y = y;

    PolygonFirst = PolygonOpen;

    PolygonFirst = PolygonOpen;
    PolygonOpen++;
}

void
    GPR_$PGON_POLYLINE(X, Y, Npositions, st)

    /* Description   :- Defines a series of line segements fomring part of 
       *                  a polygon boundary.
       * 
       *
       * Return status :- Maximum number of points is 32768 ?
       *                  If polygon not started.
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

    Gprcoord *X;            /*  <i> Pointer to number of points */
Gprcoord *Y;                /*  <i> Pointer to number of points */
unsigned short *Npositions; /*  <i> Number of points */
GprStatus *st;

{

    XPoint *p;
    int pos;
    int i;

    *st = 0;

    if (!PolygonOpen)
    {
        *st = GPR_PolyNotStarted; /* must use gpr_$start_pgon */
        return;
    }

    if (*Npositions > 32767)
    { /* too many opoints */
        *st = GPR_PolyPoints;
        return;
    }

    pos = *Npositions;

    for (i = 0; i < pos; i++)
    {

        AllocPoint(); /* allocate mem */
        p = &Points[PolygonOpen];
        p->x = X[i];
        p->y = Y[i];
        PolygonOpen++;
    }
}

GPR_$CLOSE_FILL_PGON(st)

/* Description   :- Closes and fills polygon 
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

GprStatus *st;

{

    XPoint *p;
    XPoint *f;

    if (!PolygonOpen)
    {
        *st = GPR_PolyNotStarted; /* must use gpr_$start_pgon */
        return;
    }

    AllocPoint(); /* allocate some ememomory for next point */

    p = &Points[PolygonOpen];
    f = &Points[PolygonFirst];

    p->x = f->x;
    p->y = f->y;

    PolygonOpen++;

    /* Do the X fill */

    XFillPolygon(Xdisplay,
                 LocalDrawable,
                 LocalGcFill,
                 Points,
                 PolygonOpen,
                 Complex,
                 CoordModeOrigin);

    if (GprAutoUpdate)
    {

        XFillPolygon(Xdisplay,
                     LocalAutoRefresh,
                     LocalGcFill,
                     Points,
                     PolygonOpen,
                     Complex,
                     CoordModeOrigin);
    }

    PolygonOpen = 0;
    PolygonFirst = 0;

    free((char *)Points);
}

INT AllocPoint()

/* Description   :- Allocates a number of points depending on the next
       *                  word to be used.
       * 
       *
       * Return status :- Nothing
       *                  
       *   
       *                  
       * Notes         :- Should allocate only the number of poinit defined 
       *                  in POLYALLOC
       *                  
       *                  
       *                  
       *                  
       *
       */

{

    if (GprWordBoundary(PolygonOpen, POLYALLOC))
    {

        if (!PolygonOpen)
            Points = (XPoint *)malloc((unsigned)sizeof(XPoint) * POLYALLOC);
        else
            Points = (XPoint *)realloc(Points, sizeof(XPoint) * (PolygonOpen + POLYALLOC));
    }
}

INT
    GprWordBoundary(Num, Word)

    /* Description   :- This routine calculates if the number supplied lies in
       *                  the range also supplied. eg if the number does then
       *                  it returns 1 otherwise 0
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

    int Word; /*  <i>  the word boundary value */
int Num;      /*  <i   the number supplied */

{

    return Num - (Word * (Num / Word)) ? 0 : 1; /* do a quick calc */
}

void GPR_$INQ_TEXT_OFFSET(String, StringLength, StartOffset, XYend, st)

    /* Description   :-         Returns the x- and y-offsets from the top left pixel
       *                          of a string to the origin of the string's first character.
       *                          This routine also returns the x- or y-offset to the pixel
       *                          which is the new current position after the text is written
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

    char *String;     /*  <i> The string to be written */
short *StringLength;  /*  <i> The length */
short StartOffset[2]; /*  <o> X and Y offsets from the top left hand corner of the text */
short *XYend;         /*  <o> New position for text ( total width ) */
GprStatus *st;

{

    Font fid; /* local font id */
    int len;
    int direction;
    int ascent;
    int descent;
    int pixels;
    XCharStruct overall;
    XFontStruct *fontstruct;

    *st = 0;

    fid = Bitmaps[CurrentBitmapId]->gprfid; /* save font */

    fontstruct = GprFonts[fid]->XGPRFont;

    len = (int)*StringLength;

    pixels = XTextExtents(fontstruct,
                          String,
                          len,
                          &direction,
                          &ascent,
                          &descent,
                          &overall);

    StartOffset[0] = overall.lbearing; /* pos from top left hand corner of text */
    StartOffset[1] = overall.ascent;

    *XYend = overall.width;
}

void GPR_$INQ_TEXT_EXTENT(String, StringLength, Size, st)

    /* Description   :-   Returns the x- and y-offsets a string spans when written by
       *                    GPR_$TEXT.
       * Return status :- 
       *                  
       * Notes         :- 
       *                  
       */
    char *String;    /*  <i> The string to be written */
short *StringLength; /*  <i> The length */
short Size[2];       /* the size of the returnd text */
GprStatus *st;
{

    Font fid; /* local font id */
    int len;
    int direction;
    int ascent;
    int descent;
    int pixels;
    XCharStruct overall;
    XFontStruct *fontstruct;

    fid = Bitmaps[CurrentBitmapId]->gprfid; /* save font */
    /* SPB - 171194 - We're getting 0 here if the user has written a macro like:

CREATE
NAME "test"
ACCEPT "y"
INSERT LINE 
GINPUT X(0), Y(0)
GINPUT X(100), Y(100)
VERB
FINISH "Y"
EXIT

*/
    /* Let's try ... */
    if (GprFonts[fid] == NULL)
    {
        *st = 1;
        return;
    }

    *st = 0;
    fontstruct = GprFonts[fid]->XGPRFont;

    len = (int)*StringLength;

    pixels = XTextExtents(fontstruct,
                          String,
                          len,
                          &direction,
                          &ascent,
                          &descent,
                          &overall);

    Size[0] = overall.width;

    Size[1] = overall.ascent + overall.descent;
}

void DeterminePixels()

/* Description   :- This routine will determine what GPR will use for 
       *                  black and white pixels. It should only apply for MONO systems
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- The background behind this call is as follows:-
       *                  Most X servers that are running on 4 or 8 plane systems
       *                  will return a BlackPixel() value of 0. WhitePixel() values
       *                  will be whatever the default colormap defines as white ( Usually)
       *                  However most X servers return running on single plane systems return
       *                  BlackPixel() value of 1 which means that WhitePixel() value is 0 and
       *                  using the above would mean a drawing color of 0. RASTEROPS work in
       *                  the reverse order from this and the whole thing gets mixed up
       *                  Thus for GPR emulation this routine will return what it things a
       *                  BlackPixel() (Background) and a WhitePixel() should be.
       *                  
       *
       */

{
#if 0
int planes;
	planes = DefaultDepth(Xdisplay,DefaultScreen(Xdisplay));	/* what is MONO */
	if ( planes > 1 )	{
		GprWhitePixel = WhitePixel(Xdisplay, DefaultScreen(Xdisplay));	/* should be drawing color */
		GprBlackPixel = BlackPixel(Xdisplay, DefaultScreen(Xdisplay));	/* should be background color */
	}
	else	{
		GprWhitePixel = 1;	/* Ignore server and use what GPR needs */
		GprBlackPixel = 0;
	}
#else
    GprWhitePixel = WhitePixel(Xdisplay, DefaultScreen(Xdisplay)); /* drawing color */
    GprBlackPixel = BlackPixel(Xdisplay, DefaultScreen(Xdisplay)); /* background color */

    //printf("Determine Pixels: %d %d\n", GprWhitePixel, GprBlackPixel);

    //GprWhitePixel = 1;
    //GprBlackPixel = 0;

#endif
}

GPR_$TRIANGLE(V1, V2, V3, st)

/* Description   :- Closes and fills polygon 
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

short V1[2]; /* <i> Vertex */
short V2[2]; /* <i> Vertex */
short V3[2]; /* <i> Vertex */
GprStatus *st;

{

    XPoint tri[3];
    int val;

    tri[0].x = V1[0];
    tri[0].y = V1[1];

    tri[1].x = V2[0];
    tri[1].y = V2[1];

    tri[2].x = V3[0];
    tri[2].y = V3[1];

    /* Do the X fill */

    XFillPolygon(Xdisplay,
                 LocalDrawable,
                 LocalGcDraw,
                 tri,
                 (int)3,
                 Complex,
                 CoordModeOrigin);

    if (GprAutoUpdate)
    {

        XFillPolygon(Xdisplay,
                     LocalDrawable,
                     LocalGcFill,
                     tri,
                     (int)3,
                     Complex,
                     CoordModeOrigin);
    }
}

GPR_$INQ_TEXT(Fontid, Direct, st)

/* Description   :- Returns the current font id of the current bitmap
       *                
       *                
       *                
       * Return status :-
       *                
       *                
       *                
       * Notes         :-  Return GPR id not X id
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

short *Fontid; /* <o> Fontid */
short *Direct;
GprStatus *st;

{
    *Fontid = Bitmaps[CurrentBitmapId]->gprfid; /* get current loaded descripter */

    *Direct = 0;
    *st = 0;
}

GPR_$SET_CURSOR_POSITION(Curpos, st)

/* Description   :-  Moves cursor to new postion 
       *                
       * 
       *
       * Return status :- none
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

short Curpos[2];
GprStatus *st;

{

    Window dest_w;
    Window src_w;
    int dest_x, dest_y;
    GprEventType event;
    GprKeyset keyset;
    int count;
    int i;
    XEvent xevent;

    *st = 0;

    return; /* leave original code in for future reference 
  
    dest_x = Curpos[0];
    dest_y = Curpos[1];
    dest_w =  Bitmaps[CurrentBitmapId]->WindowId;

    event = gpr_locator;
    GPR_$DISABLE_INPUT(&event,st);

    XWarpPointer(Xdisplay,None,dest_w,0,0,0,0,dest_x,dest_y);	

    count = XPending(Xdisplay);	

    GPR_$ENABLE_INPUT(&event,keyset,st);	

    for(i=0;i<count;i++)	{
	XNextEvent(Xdisplay,&xevent);
	if ( xevent.type != MotionNotify )
		XPutBackEvent(Xdisplay,&xevent);

    }

*/
}

KeySym
    MapToKeypad(Symbol)

    /* Description   :- Reads the the current symbol. Checks for a keypad 
       *                  definition and returns the chosen normal ascii char
       *                
       *                
       * Return status :- The symbol that was passed if not a keypad
       *                
       *                
       *                
       * Notes         :- This routine should be taken out if actual keypad 
       *                  symbols are needed
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    KeySym Symbol; /* <i> Incoming symbol recieved from key press */
{

    //	printf("Keyboard Press %d\n",Symbol);
    switch (Symbol)
    {
    case XK_KP_Space: /* space ( no more comments after this ) */
        return XK_space;
        break;
    case XK_KP_Tab:
        return XK_Tab;
        break;
    case XK_KP_Enter:
        return XK_Return;
        break;
    case XK_KP_Equal:
        return XK_equal;
        break;
    case XK_KP_Multiply:
        return XK_asterisk;
        break;
    case XK_KP_Add:
        return XK_plus;
        break;
    case XK_KP_Separator:
        return XK_comma;
        break;
    case XK_KP_Subtract:
        return XK_minus;
        break;
    case XK_KP_Divide:
        return XK_slash;
        break;
    case XK_KP_Decimal:
        return XK_period;
        break;
    case XK_KP_0:
        return XK_0;
        break;
    case XK_KP_1:
        return XK_1;
        break;
    case XK_KP_2:
        return XK_2;
        break;
    case XK_KP_3:
        return XK_3;
        break;
    case XK_KP_4:
        return XK_4;
        break;
    case XK_KP_5:
        return XK_5;
        break;
    case XK_KP_6:
        return XK_6;
        break;
    case XK_KP_7:
        return XK_7;
        break;
    case XK_KP_8:
        return XK_8;
        break;
    case XK_KP_9:
        return XK_9;
        break;
    case XK_Left:
        return 138; /* some additional GPR mappings */
        break;
    case XK_Right:
        return 140;
        break;
    case XK_BackSpace:
        return XK_Delete;
        break;
    default:
        return Symbol;
        break;
    }
}
