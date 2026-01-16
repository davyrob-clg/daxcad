/*
     @(#)  412.1 date 6/11/92 toolkit.h 

     Filename    : toolkit.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:42:13
     Last change : 92/06/11 14:42:12

     Copyright : Practical Technology Limited  
     File :- toolkit.h

     C include file

     Main toolkit include file containing macros and typedefs

*/

#define false           0
#define true            -1

static int True = -1;		/* variables able to be passed */
static int False = 0;

/* Resource code flags */

#define CHECK_BITS(X,Y)  (((Y) == ((X) & (Y))) ? 1 : 0)

#define TK_WINDOW_RESOURCE      256	/* window based resource */ 
#define TK_BUTTON_RESOURCE      512	/* button based resource */
#define TK_FONT_RESOURCE        1024	/* Font resource */
#define TK_BULLETIN_RESOURCE    2048	/* Bulletin board resource */
#define TK_TEXTWINDOW_RESOURCE  4096	/* text window resource */
#define TK_SCROLLBAR_RESOURCE   8192	/* scrollbar/slider resource */

#define MAXWINDOWS 20				/* maximum number windows */
#define MAXTEXTWINDOWS 20			/* maximum number of text windows */
#define MAXFONTS   20				/* maximum number of fonts loaded */
#define MAXSTRINGLENGTH 1024		/* maximum string length for text drawing */
#define MAXSCROLLBARS 20			/* maximum number of scroll bars */
#define MAXBUTTONS      200		/* The maximum number of buttons */
#define ILLEGAL_RESOURCE 0		/* illegal resource */
#define MAXBULLETINS     20		/* maximum number of bulletins on the screen */
#define TK_DISPLAY_OBJECT  1		/* Generic display object */
#define TK_ERASE_OBJECT    0		/* Generic erase object */


struct TkWindow {			/* window definition size */
			int x;
			int y;
			int xsize;
			int ysize;
			int bitmap;		/* the bitmap id for graphics */
			int attr;		/* the attribute block id for graphics */
			short stream;	/* apollo pad stream only */
			short planes;	/* planes */
			char hidden;	/* indicates whether hidden or visable */
			char windowid;	/* Window id */
			int color_b;	/* background color index */
			int cursor;		/* state of cursor on this window */

		};

typedef struct TkWindow TkWindow;

struct TkWindowCreate {
			int x;
			int y;
			int xsize;
			int ysize;
			int color_b;	/* background color of window (INDEX) */
		};	/* used for creating windows */

typedef struct TkWindowCreate TkWindowCreate;

struct TkRect {
			int x;
			int y;
			int xsize;
			int ysize;
		};

typedef struct TkRect TkRect;


struct TkPos {
					int x;
					int y;
				} ;

typedef struct TkPos TkPos;



struct	TkFont {
					char *name;
					short fontid;
				};

typedef struct TkFont TkFont;




struct TkEvent{
					int type; 			/* event type */
					int keyhit;			/* key that was hit */
					int object;			/* object that was hit */
					int mousebutton;	/* mouse button that was hit */
					int x;				/* position */
					int y;
				};

typedef struct TkEvent TkEvent;

                                        

struct TkButton {

			char *string;		/* string with the button */
			int font;			/* toolkit font id */
			int color_t;		/* text color */
			int color_b;		/* background color */
			int color_bord;		/* border color */
			int border;			/* border types */
			int border_width;	/* the width of the border */
			int pos[2];			/* position of button */
			int win;			/* parent resource */
			int select;			/* select mode */
			int select_button;	/* Button which causes button to be hightlit */
			int select_event;	/* events which allow selection */
			int state;			/* state of buttons (SELECTED) */
			int visible;		/* visibility mode */
			int menu;		/* menu resource to comon if selected */
			int menu_button;	/* button to control menu */
			int select_box[4];	/* select box dimensions */

		};

typedef struct TkButton TkButton;

struct TkTextLine {					/* structure for number of lines */
					char *text;			/* text pointer */
					char highlit;		/* text is currently hightlit */
					struct TkTextLine *nextline;
				};

typedef struct TkTextLine TkTextLine;


struct TkTextWindow {

					int color_b;		/* background color */
					int color_text;		/* the color of the text */
					int color_bord;		/* the border color */
					int columns;		/* the number of columns */
					int rows;		/* the number of rows */
					int fontid;		/* toolkit font ( MUST be fixed width ) */
					int pos[2];		/* position on the screen */
					TkTextLine *text;		/* pointer to text lines */
					int linecount;		/* current number of lines on the virtual list */
					int maxlines;		/* the maximum number of virtual lines to hold */
					int currentline;	/* Current line number in the virtual list */
					int windowline;		/* current top line that the text window looks at in th3e virtual list */
					int currentcol;		/* Current column number for horizontal scrolls */
					int border_dist;	/* distance between max text and border */
					int border_thick;	/* thickness of border */
					int font_offset[2];	/* Amount to add to get top left pixel of font */
					int select_box[4];	/* the actual select box size */
					int parent;		/* parent window id */
					int visible;		/* Visibility flag */
					int text_height;	/* overall text height */
					int text_width;		/* overall text width */
					int wrapround;		/* text will wrap */
					int appending;		/* No new line has been added to the window 
                   						   The next line will be appeded */
					int scrollbar;		/* the attached scroll bar */

					};

typedef struct TkTextWindow TkTextWindow;




/*	

               A small note on the textwindow coordinate system
               ================================================

                C1 C2 C3 C4 C5 C6 C7 C8 C9 C0

               o------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R1
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R2
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R3
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R4
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R5
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R6
               -------------------------------  
               |  |  |  |  |  |  |  |  |  |  |    R7
               -------------------------------  

		The Column/Row coordinate system can address any character in
		the system The top left character is 0,0. This relates to x and y



		Additional notes on the window/virtual list

		The virtual list is the malloced list of text elements to
		be displayed. The variable currentline points to the current text line
		logical line position, 1 is the first.

		The text window is a box of row/column size whci looks at that 
		virtual list at any time and can be controlled by scrollbar objects
		the variable windowline looks at the top line of the virtual list
		thus when adding text the text window position will be the same posiiton
		as the currentline




*/
		

struct TkScrollbar {
					int pos[2];			/* position on the screen */
					int width;			/* pixel width */
					int height;			/* pixel size of whole box */
					int virtual;		/* virtual Y size of scroll bar */
					int pixel;			/* pixel height of wire */
					int side;			/* Side to place scrollbar ( see macro defs ) */
					int border;			/* distance between object */
					int vertical;		/* vertiacal or horizontal */
					int color_wire;		/* wire color */
					int color_frame;	/* frame color includeing arrows and box */
					int color_b;		/* background color including internal and external box */
					int parent;			/* parent window */
					int object;			/* attached object id ( If any ) */
					int visible;		/* visibility */
					int currentpos;		/* current virtual position */
					int screenpos;		/* current screen pos */
					int top_box[4];		/* top box dimensions */
					int mid_box[4];		/* middle box dimensions */
					int bot_box[4];		/* bottom box dimensions */
					int wire[2]			/* Central wire position */

		} ;

typedef struct TkScrollbar TkScrollbar;


struct TkObject {
					int objectid;
					struct TkObject *nextobject;
				};

typedef struct TkObject TkObject;

struct TkBulletinBoard{					/* Object bulletin board */

			int color_b;		/* background color */
			int color_bord;		/* border color */
			int temporary;		/* Temporary bulleting board flag */
			int bitmap;			/* a bitmap object containing the hidden portion */
			TkObject *object_list;	/* list of identifiers on the bulleting board */
			int select_box[4];	/* select box dimensions */
			int visible;		/* is the board currently on the screen */

				} ;
typedef struct TkBulletinBoard TkBulletinBoard;

#define TK_BUTTON_BORDER_NONE        0	/* definitions for box type */
#define TK_BUTTON_BORDER_BOX         1
#define TK_BUTTON_BORDER_ROUNDED     2
#define TK_BUTTON_BORDER_SHADOW      256

#define TK_BUTTON_HIGHLIGHT          0	/* wil mark when selected */
#define TK_BUTTON_NO_HIGHLIGHT       1

#define TK_BUTTON_SELECT             0	/* select the button */
#define TK_BUTTON_DESELECT           1	/* select the button */
#define TK_BUTTON_DISPLAY            2	/* draw onto resource */
#define TK_BUTTON_ERASE              3	/* disappear the button */

#define TK_SHADOW_INSET              2 /* the pixel distance to give shadow effect */


/* Scrolling textwindow definitions */

#define TK_SCROLL_LEFT          0	/* scrolling directions */
#define TK_SCROLL_UP	        1
#define TK_SCROLL_DOWN          2
#define TK_SCROLL_RIGHT         3

#define TK_ARROW_LEFT          0	/* scrolling Arrow directions */
#define TK_ARROW_UP	        1
#define TK_ARROW_DOWN          2
#define TK_ARROW_RIGHT         3

#define TK_APPEND_TEXT		0	/* text add modes on text windows */
#define TK_NEWLINE_TEXT		1

#define TK_TOP_WINDOW           -1
#define TK_BOTTOM_WINDOW        -2

#define TK_FULL_PAGE            0	/* page display options */
#define TK_PARTIAL_PAGE         1


#define TK_SCROLL_DETACHED   0		/* scroll bar create options */

#define TK_SCROLLBAR_LEFT       0		/* where top place scroll bar */
#define TK_SCROLLBAR_RIGHT      1	
#define TK_SCROLLBAR_TOP        2	
#define TK_SCROLLBAR_BOTTOM     3	

#define TK_SCROLLBAR_VERTICAL     true
#define TK_SCROLLBAR_HORIZONTAL   false

#define TK_SCROLLBAR_WIDTH      41		/* standard textwindow size */
#define TK_SCROLLBAR_BOX        60		/* standard box height ( FIXED ) */
#define TK_SCROLLBAR_WIRE       5      /* wire border */

/* EVENTING SECTION */

#define TK_MOUSE_1_DOWN    'a'		/* mouse buttons states */
#define TK_MOUSE_2_DOWN    'b'
#define TK_MOUSE_3_DOWN    'c'
#define TK_MOUSE_1_UP      'A'
#define TK_MOUSE_2_UP      'B'
#define TK_MOUSE_3_UP      'C'

#define TK_M1_D             1	/* returnd mouse button events */
#define TK_M2_D             2
#define TK_M3_D             4
#define TK_M1_U             8
#define TK_M2_U             16
#define TK_M3_U             32



#define TK_NORMAL_INPUT         0		/* input state */
#define TK_GRABBED_INPUT        1





/* Text justification factors */

#define TK_TEXT_LEFT        0
#define TK_TEXT_MIDDLE      1
#define TK_TEXT_RIGHT       2

#define gpr_keystroke               0		/* gpr events translated */
#define gpr_buttons                 1
#define gpr_locator                 2
#define gpr_entered_window          3
#define gpr_left_window             4
#define gpr_locator_stop            5
#define gpr_no_event                6
#define gpr_locator_update          7
#define gpr_dial                    8
#define gpr_coded_keys              9
#define gpr_function_keys           10
#define gpr_pfk                     11
#define gpr_physical_keys           12
#define gpr_kbd_entered_window      13
#define gpr_kbd_left_window         14
#define gpr_keystroke_up            15
#define gpr_all_events              16


typedef struct {
        short controller_type;    /* type of graphics controller */
        short accelerator_type;  /* type of graphics accelerator */ 
        short x_window_origin;           /* x origin of window screen area in pixels */ 
        short y_window_origin;           /* y origin of window screen area in pixels */ 
        short x_window_size;             /* x dimension of window screen area in pixels */ 
        short y_window_size;             /* y dimension of window screen area in pixels */ 
        short x_visible_size;            /* x dimension of visible screen area in pixels */ 
        short y_visible_size;            /* y dimension of visible screen area in pixels */ 
        short x_extension_size;          /* x dimension of maximum extended bitmap size in pixels */ 
        short y_extension_size;          /* y dimension of maximum extended bitmap size in pixels */ 
        short x_total_size;              /* x dimension of total buffer area in pixels */ 
        short y_total_size;              /* y dimension of total buffer area in pixels */ 
        short x_pixels_per_cm;           /* number of pixels in x dimension per centimeter */ 
        short y_pixels_per_cm;           /* number of pixels in y dimension per centimeter */ 
        short n_planes;                  /* number of planes available */ 
        short n_buffers;                 /* number of display buffers available */ 
        short delta_x_per_buffer;        /* relative displacement of buffers in x */ 
        short delta_y_per_buffer;        /* relative displacement of buffers in y */ 
        short delta_planes_per_buffer;   /* relative displacement of buffers in depth */ 
        unsigned short mem_overlaps;     /* set of overlaps among classes of buffer memory */ 
        short x_zoom_max;                /* maximum pixel-replication zoom factor for x */ 
        short y_zoom_max;                /* maximum pixel-replication zoom factor for y */ 
        short video_refresh_rate;        /* refresh rate in hz */ 
        short n_primaries;               /* number of primary colors (1 -> monochrome; 3 -> color */ 
        short lut_width_per_primary;     /* number of bits in possible intensity values per primary */ 
        unsigned short avail_formats;    /* set of available interactive/imaging formats */ 
        unsigned short avail_access;     /* set of available pixel sizes for direct access */ 
        short access_address_space;      /* number of 1kb pages of address space available for direct access */ 
        short invert;                    /* INVert implemention */
        short num_lookup_tables;         /* Number of color lookup tables */
        short rgb_color;                 /* Modes for separate values for RGB */
        short default_cursor_mode;       /* type of cursor - software/hardware */
        short avail_cursor_modes;        /* cursors supported by device */
        short n_mult_clips;              /* Number of multiple clip rectangles supported */
} gpr_$disp_char_t;


#ifndef M_E 

#define M_E        2.7182818284590452354E0  /*Hex  2^ 0 * 1.5bf0a8b145769 */
#define M_LOG2E    1.4426950408889633870E0  /*Hex  2^ 0 * 1.71547652B82FE */
#define M_LOG10E   4.3429448190325181667E-1 /*Hex  2^-2 * 1.BCB7B1526E50E */
#define M_LN2      6.9314718055994530942E-1 /*Hex  2^-1 * 1.62E42FEFA39EF */
#define M_LN10     2.3025850929940456840E0  /*Hex  2^ 1 * 1.26bb1bbb55516 */
#define M_PI       3.1415926535897931160E0  /*Hex  2^ 1 * 1.921FB54442D18 */
#define M_PI_2     1.5707963267948965580E0  /*Hex  2^ 0 * 1.921FB54442D18 */
#define M_PI_4     7.8539816339744827900E-1 /*Hex  2^-1 * 1.921FB54442D18 */
#define M_1_PI     3.1830988618379067154E-1 /*Hex  2^-2 * 1.45f306dc9c883 */
#define M_2_PI     6.3661977236758134308E-1 /*Hex  2^-1 * 1.45f306dc9c883 */
#define M_2_SQRTPI 1.1283791670955125739E0  /*Hex  2^ 0 * 1.20dd750429b6d */

#endif


