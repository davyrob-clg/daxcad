
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_glue.c   */

/*

             Practical Technology 1990 

       Continued libray routines based library for Apollo GPR to X library 
       Used to glue Existing DAXCAD routines together.

*/

#include <stdio.h>
#include <errno.h>
#include <string.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include "gprx_types.h"
#include "gprx_lib.h"
#include "gprx_extern.h"
#include "gprx_errors.h"

#include "xlang.h"
#include "daxcad_functions.h"

char *Strdupn();

GPR_$INQ_MONO_PIXELS(WPixel, BPixel, st)
/* Description   :- Returns the value of White and Black pixels
       *                  under the current X environent.
       *
       * Return status :- None will work under Color systems as well
       *                  
       * Notes         :- EXTENSION A Black pixel could be considered the drawing
       *                            pixel and the White a background one It doesn matter
       *                            its only convention.
       */
GprColor *WPixel; /*  <o>     The set White pixel color */
GprColor *BPixel; /*  <o>     The set black pixel */
{
   *WPixel = GprWhitePixel;
   *BPixel = GprBlackPixel;
}

GPR_$INQ_ROOTWINDOW(Size, BitmapDepth, WPixel, BPixel, st)
/* Description   :- returns information about the root window
       *                  Used to determine maximum dimensions etc
       *
       * Return status :- None
       *                  
       * Notes         :- Used to obtain information about the current
       *                  display being used.
       *
       */
GprBitmapSize Size;     /*  <o>  the size of the root window */
GprPlanes *BitmapDepth; /*  <o>  NUmber of active planes */
GprColor *WPixel;       /*  <o>     The set White pixel color */
GprColor *BPixel;       /*  <o>     The set black pixel */
GprStatus *st;
{
   XWindowAttributes attr;
   Window win;
   Window root;                /* root window ID */
   int x, y;                   /* location */
   unsigned int width, height; /* what do you think */
   unsigned int border_width;  /* what do you think */
   unsigned int depth;         /* depth of window */
   Drawable d;
   Status status;

   win = RootWindow(Xdisplay, DefaultScreen(Xdisplay));

   /* get gemetry of window or pixmap  */
   XGetGeometry(Xdisplay, win, &root,
                &x, &y, &width, &height, &border_width, &depth);

   XGetWindowAttributes(Xdisplay, win, &attr);

   /* get gemetry of window or pixmap  */
   XGetGeometry(Xdisplay, win, &root,
                &x, &y, &width, &height, &border_width, &depth);

   //printf("Size of Size[0] %d\n",sizeof(Size[0]));
   //printf("Size of GprColor %d\n",sizeof(GprColor));
   Size[0] = attr.width;
   Size[1] = attr.height;
   *WPixel = GprWhitePixel;
   *BPixel = GprBlackPixel;
   *BitmapDepth = attr.depth - 1;
}

GPR_$SET_ICON_NAME(IconName, Length, st)

/* Description   :- Sets the name of the icon that is displayed.
       * 
       * 
       *
       * Return status :- 1 The name length is geater than 255
       *                  
       *   
       *                  
       * Notes         :- This sets the size hints data for the 
       *                  current bitmap.
       *                  
       *                  
       *                  
       *                  
       *
       */

char *IconName; /*  <i>     The name of the icon you want */
int *Length;    /*  <i>     LEngth */
GprStatus *st;

{

   Window win;
   char *p;

   if (*Length < 0 || *Length > 255)
   {
      *st = GPR_LengthNotValid;
      return;
   }

   win = Bitmaps[CurrentBitmapId]->WindowId;

   if (!win)
   {
      *st = GPR_BitmapIsNotWindow; /* is not a displayable window */
      return;
   }

   p = (char *)Strdupn(IconName, *Length);

   XSetIconName(Xdisplay, win, p);

   free(p);
}

GPR_$SERVER_FLUSH_X(st)

/* Description   :- Flushes the server output queue on the current 
       *                  display. Does an XFlush 
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- EXTENSION: This is designed as an interface onto 
       *                             the XFlush call.
       *                  
       *                  
       *                  
       *                  
       *
       */

GprStatus *st;

{

   XFlush(Xdisplay);
}

char *Strdupn(String, Length)

    /* Description   :- This routine will duplicate a string. It uses 
       *                  malloc to get memory. Primary use is for converting
       *                  f77 stirngs into something usefull
       *
       * Return status :- Null if malloc was not succes otherwise it returns
       *                  a pointer to the new string.
       *   
       *                  
       * Notes         :- Nothing to speak of.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    char *String; /*  <i>     String input */
int Length;       /*  <i>     Number of bytes to malloc */

{

   char *p; /* temp pointer */

   if (!Length)
   {
      return (char *)NULL; /* come out */
   }

   if (!(p = (char *)malloc(Length + 1)))
   {

      return (char *)NULL; /* come out */
   }

   strncpy(p, String, Length);

   p[Length] = '\0';

   return p;
}

GPR_$SET_INPUT_FOCUS(st)

/* Description   :- EXTENSION for GPRX allows focus to be set on the current bitmap window
       *                  if the window manager doesnt do it for you!!!
       * 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- EXTENSION only for X based programs
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

GprStatus *st;

{

   Window focus;
   Time focustime;
   int vis;

   focus = Bitmaps[CurrentBitmapId]->WindowId;

   IsWindowVisable(focus, &vis); /* check for visibility */

   if (!vis)
      return;

   focustime = CurrentTime;

   XSetInputFocus(Xdisplay, focus, RevertToParent, focustime);
}

IsWindowVisable(Win, Visable)

    /* Description   :- Returns the visbale or map status of a 
       *                  window
       *                
       *                
       * Return status :-Visable 0 unmapped
       *                 Visable 1 mapped
       *                
       *                
       * Notes         :- Usefull for testing window icon state
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    Window Win; /* <i> X window id */
int *Visable;   /* <o> Visable flag */

{

   XWindowAttributes attr;

   XGetWindowAttributes(Xdisplay, Win, &attr); /* this will return map states */

   if (attr.map_state)
      *Visable = 1;
   else
      *Visable = 0;
}
