/* SCCS id Keywords             @(#)  412.1 date 6/11/92 vt.h   */


/*
 *
 *
 *        Filename    : vt.h
 *        Version     : 1.0
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Include file for VTWINDOWS code.
 *
 *        
*/

#define ESCAPE 27
#define VT_CLEAR_SCREEN "[2J"
#define MAXWIN 20

#define VT_DISABLED 0
#define VT_ENABLED  1

#define ROOTWINDOW 0

static int CurrentWindow = -1;      /* current window active */

char *malloc();


#define VT_BORDER 1
#define VT_DEFAULT_COL 80          /* default sizes if not defined */
#define VT_DEFAULT_ROW 24

#define VT_PIXEL_VM_NORMAL  0      /* video mode is normal */
#define VT_PIXEL_VM_REVERSE 0x20   /* video mode is reverse */
#define VT_PIXEL_VM_BOLD    0x40   /* video mode is bold */

#define VT_PIXEL_CS_NORMAL  0      /* character set is normal */
#define VT_PIXEL_CS_GRAPHIC 0x10   /* character set is graphical */


#define VT_PIXEL_CS_BIT 0x10       /* bits that define state of character mode */
#define VT_PIXEL_VM_BIT 0x20
#define VT_PIXEL_BM_BIT 0x40

#define VT_CLIPPING_ON      1      /* CLipping control flags */
#define VT_CLIPPING_OFF     0

#define VT_BACKING_ON       1      /* CLipping control flags */
#define VT_BACKING_OFF      0

struct WindowPixel {
               
                       char byte;      /* character value stored */
                       char pixel;     /* byte field for other information */

                    };

static struct Window {

                int col;           /* column size */
                int row;           /* row size */
                int offx;          /* offset position in root window */
                int offy;          
                int border;        /* Border type */
                int state;         /* enabled state */
                int x;             /* current x pos */
                int y;             /* current y pos */
                int videomode;     /* current video mode */
                int cset;          /* current character set */
                int clipping;      /* clipping is active on text within window */
                int updating;      /* Backing store will be updated */
                int bytes;         /* Number of bytes that are stored for window*/
                struct WindowPixel *pixel;      /* pointer to array of elements on the screen */
                char *title;       /* name of window */

               } VTWindows[MAXWIN];

static int VT_STDOUT = 1;     /* standard io */
static int VT_STDIN  = 0;


#define VT_SHIFT_LEFT    1     /* Shift either way */
#define VT_SHIFT_RIGHT   2


#define VT_TEXT_LEFT     0     /* text justification values */
#define VT_TEXT_MIDDLE   1
#define VT_TEXT_RIGHT    2



/*     Error codes definitions */

#define VTOK                    0
#define VT_CANNOT_INITALISE     1
#define VT_CANNOT_SHUT          2
#define VT_TOO_MANY_WINDOWS     3
#define VT_ROOTWINDOW_ILLEGAL   4
#define VT_CANNOT_SHUT_WINDOW   5
#define VT_WINDOW_ENABLED       6


/* Special VTCHAR definitions for read inputs */

#define MAXREAD 10        /* maximum sequentiol reads */


#define VT100_ARROW_UP      "[A"        /* all vt100 defintions */
#define VT100_ARROW_DOWN    "[B"     
#define VT100_ARROW_LEFT    "[D"     
#define VT100_ARROW_RIGHT   "[C"     

#define VT_ESCAPE      (unsigned char)27
#define VT_BRACKET     (unsigned char)91
#define VT_BACKSPACE   (unsigned char)8
#define VT_DEL         (unsigned char)127
#define VT_CR          (unsigned char)13
#define VT_LF          (unsigned char)10
#define VT_C_UP        (unsigned char)130     /* cursor defintions */
#define VT_C_DOWN      (unsigned char)131
#define VT_C_LEFT      (unsigned char)132
#define VT_C_RIGHT     (unsigned char)133



/*     VT Graphic codes */

#define MAXTERM 2  /* the number of supported terminals */

#define VT_TERM_TL 0       /* subsript defintions for box */
#define VT_TERM_V  1
#define VT_TERM_BL 2
#define VT_TERM_TR 3
#define VT_TERM_H  4
#define VT_TERM_BR 5

#define VT100     0
#define AIXTERM   1


#define MAXSEQUENCE  2

#define VT_ES_GMODE_NORMAL 0      /* gr mode normal ES */
#define VT_ES_GMODE_CHAR   1      /* gr mode character ES */




static char *TermsES[MAXTERM][MAXSEQUENCE] = {
                                               "\033)A\016",
                                               "\033)0\016",
                                               "",
                                               "" };




static unsigned char Terms[MAXTERM][6] = {
                                          108,120,109,107,113,106,
                                          218,179,192,191,196,217};



