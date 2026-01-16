


/* SCCS id Keywords             @(#)  412.1 date 6/11/92 


/*

             Practical Technology 1990 

     GPRX library routines to control and create cursors

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

#include "GPRX_blank_cursor.h"
#include "GPRX_wait_cursor.h"
#include "GPRX_full_pixmap.h"

#include "xlang.h"
#include "daxcad_functions.h"


CreateBlankCursor(Desc)


      /* Description   :- Creates a blank cursor for the window requested
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- This is designed for GPR compatibility with
       *                  the ability to switch of the screen cursor
       *                  We do this by defining a blank pixmap and assigning
       *                  that to the cursor
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */


GprBitmapDesc Desc;       /* <i> descripter for a window. Must be visible */


{

Pixmap cursorpixmap;
Pixmap fullpixmap;
Drawable win;
Cursor blank;
XColor fg;
XColor bg;


    win = Bitmaps[CurrentBitmapId]->WindowId;

    cursorpixmap = XCreatePixmapFromBitmapData(Xdisplay,
                                               win,
                                               GPRX_blank_cursor_bits,
                                               GPRX_blank_cursor_width,
                                               GPRX_blank_cursor_height,
                                               0,
                                               0,
                                               1);


    fullpixmap = XCreatePixmapFromBitmapData(Xdisplay,
                                               win,
                                               GPRX_blank_mask_bits,
                                               GPRX_blank_cursor_width,
                                               GPRX_blank_cursor_height,
                                               0,
                                               0,
                                               1);
    fg.pixel = 0;
    bg.pixel = 0;

    bg.red = 0;
    bg.blue= 0;
    bg.green= 0;

    fg.red = 0;
    fg.blue= 0;
    fg.green=0;

    fg.flags = DoRed | DoGreen | DoBlue;
    bg.flags = DoRed | DoGreen | DoBlue;

    blank = XCreatePixmapCursor(Xdisplay,
                                cursorpixmap,
                                fullpixmap,
                                &fg,
                                &bg,
                                8,8);

    Bitmaps[Desc]->blank = blank;
    XFreePixmap(Xdisplay,cursorpixmap);	/* free up the pixmap */
    XFreePixmap(Xdisplay,fullpixmap);	/* free up the pixmap */
}







CreateWaitCursor(Desc)


      /* Description   :- Creates a waiting cursor for the window requested
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- This creates a wait state cursor for any use.
       *                  It will be turned on by the routine 
       *
       *                  gpr_$set_wait_cursor
       *                 
       *                
       *               
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */


GprBitmapDesc Desc;       /* <i> descripter for a window. Must be visible */


{

Pixmap cursorpixmap;
Pixmap fullpixmap;
Drawable win;
Cursor wait;
XColor fg;
XColor bg;


    win = Bitmaps[CurrentBitmapId]->WindowId;

    cursorpixmap = XCreatePixmapFromBitmapData(Xdisplay,
                                               win,
                                               GPRX_wait_cursor_bits,
                                               GPRX_wait_cursor_width,
                                               GPRX_wait_cursor_height,
                                               GprWhitePixel,
                                               GprBlackPixel,
                                               1);


    fullpixmap = XCreatePixmapFromBitmapData(Xdisplay,
                                               win,
                                               GPRX_full_pixmap_bits,
                                               GPRX_full_pixmap_width,
                                               GPRX_full_pixmap_height,
                                               GprWhitePixel,
                                               GprBlackPixel,
                                               1);

    fg.pixel = 0;
    bg.pixel = 0;

	if ( SystemDepth == 1 )	{	/* Mono systems may use this */

		bg.red = 65535;
		bg.blue= 65535;
		bg.green= 65535;
		fg.red = 0;
		fg.blue= 0;
		fg.green= 0;

	}
	else	{
		bg.red = 0;
		bg.blue= 0;
		bg.green= 0;

		fg.red = 65535;
		fg.blue= 65535;
		fg.green= 65535;
	}

    fg.flags = DoRed | DoGreen | DoBlue;
    bg.flags = DoRed | DoGreen | DoBlue;

    wait = XCreatePixmapCursor(Xdisplay,
                                cursorpixmap,
                                fullpixmap,
                                &fg,
                                &bg,
                                GPRX_wait_cursor_x_hot,
                                GPRX_wait_cursor_y_hot);

   Bitmaps[Desc]->wait = wait;

   XFreePixmap(Xdisplay,cursorpixmap);	/* free up the pixmap */
   XFreePixmap(Xdisplay,fullpixmap);	/* free up the pixmap */
}







GPR_$INQ_CURSOR()

      /* Description   :- Returns Cursor information
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
       *
       *
       *
       *
       *
       *
       */


{
}
