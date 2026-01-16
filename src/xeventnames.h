
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 xeventnames.h   */

/*

       Practical Tech 1990

       C include file for GPR x lib 

       All X events as names 

*/


#define XButton1 1     /* button number */
#define XButton2 2     
#define XButton3 3


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
