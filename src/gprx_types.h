
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_types.h   */

/*

       GPR x type defines 

*/

#include <stdint.h>
#define MAXBITMAPS 100 /* the number of bitmaps that can be defined */
#define MAXPLANES 8	   /* number of RGB planes */
#define HIPLANE 64	   /* higest avaliable plane no */
#define MAXFONTS 100   /*Maximum number of fonts */

#define MAXBITMAPX 32000 /* largest bitmap */
#define MAXBITMAPY 32000

#define POLYALLOC 256 /* number of points alllocated at one time */

#define EventNotSupported -1 /* the GPR event type is not X mappable */

#define CHECK_BITS(X, Y)  ((( X & Y )  ==  Y)

#define F77false ((int)00)
#define F77true ((int)-1)
typedef int LOGICAL;

typedef short GprEndStyle;
typedef short GprDisplayMode;
typedef short GprLineStyle;
typedef short GprEventType;
typedef short GprRasterOp;

#define gpr_capnotlast 0
#define gpr_capbutt 1
#define gpr_capround 2
#define gpr_capprojectin 3

#define gpr_solid 0
#define gpr_dotted 1

#define gpr_borrow 0
#define gpr_frame 1
#define gpr_no_display 2
#define gpr_direct 3
#define gpr_borrow_nc 4
#define gpr_direct_rgb 5
#define gpr_borrow_rgb 6
#define gpr_borrow_rgb_nc 7

#define gpr_keystroke 0
#define gpr_buttons 1
#define gpr_locator 2
#define gpr_entered_window 3
#define gpr_left_window 4
#define gpr_locator_stop 5
#define gpr_no_event 6
#define gpr_locator_update 7
#define gpr_dial 8
#define gpr_coded_keys 9
#define gpr_function_keys 10
#define gpr_pfk 11
#define gpr_physical_keys 12
#define gpr_kbd_entered_window 13
#define gpr_kbd_left_window 14
#define gpr_keystroke_up 15
#define gpr_all_events 16

#define gpr_rop_zeros 0
#define gpr_rop_src_and_dst 1
#define gpr_rop_src_and_not_dst 2
#define gpr_rop_src 3
#define gpr_rop_not_src_and_dst 4
#define gpr_rop_dst 5
#define gpr_rop_src_xor_dst 6
#define gpr_rop_src_or_dst 7
#define gpr_rop_not_src_and_not_dst 8
#define gpr_rop_src_equiv_dst 9
#define gpr_rop_not_dst 10
#define gpr_rop_src_or_not_dst 11
#define gpr_rop_not_src 12
#define gpr_rop_not_src_or_dst 13
#define gpr_rop_not_src_or_not_dst 14
#define gpr_rop_ones 15

typedef unsigned char GprKeyset[256]; /* this is supposed to be set of char */

typedef int32_t INT;
typedef int32_t INT_4;

typedef short GprWindow[4]; /* A window size */

typedef short GprBitmapSize[2]; /* the Apollo bitmap size descripter */

typedef short GprPlanes; /* number of planes available */

typedef INT GprBitmapDesc; /* apollo bitmap descripter */

typedef INT GprColor; /* color index descripter */

typedef INT GprAttribute; /* attribute block descripter */

typedef short GprPlaneMask; /* plane mask for active planes on a bitmap */

typedef short GprOrigin[2]; /* coordinate origin system */

typedef short GprCursor[2]; /* cursor position */

typedef short GprRasterOps[8]; /* raster ops on each plane */

typedef short GprRaster; /* raster op type */

typedef short GprFontid;  /* fontid code */
typedef short GprTextDir; /* text direction */

typedef INT GprTextVal[2]; /* text values ( draw and background ) */

typedef short Gprcoord; /* individual coordinated ( X or Y ) */

typedef uint32_t GprIndex;

typedef int32_t GprStatus;

typedef struct ColorVector
{						 /* color vector type for GPR only */
	unsigned char dummy; /* used to pad out structure */
	unsigned char Red;
	unsigned char Green;
	unsigned char Blue;

} GprColorVector;
