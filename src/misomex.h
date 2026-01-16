/* SCCS id Keywords             @(#)  412.1 date 6/11/92 misomex.h   */


double dcang();			/* DAXCAD function defintions */
double ddistxy();
double GetAng();


#define XVAL1		0	/* part data buffer subscript values */
#define YVAL1		1
#define XVAL2		3
#define YVAL2		4

#define CENX		0
#define CENY		1
#define RADIUS		3
#define SANG		4
#define EANG		5

#define DAXLEN		81

#define WORKSPACE	0

#define DAXCADTEXT	81	/* the current size of DAXCAD text entity */
#define MAXPROFILES	100	/* maximum number of profiles available to MISOMEX interface */


#define DELETED		100	/* entity deleted status */
#define NORMAL		10	/* normal status */
#define DELETEPROTECT	20	/* cannot be deleted */
#define GROUP		50
#define CMASTER		56	/* component master */
#define SMASTER		55	/* symbol master */
#define MARKED		128	/* 8 bit set to indicate flagged */

#define MARKER		2
#define LINE		3	/* Daxcad entity type defintions */
#define ARC		5
#define ELLIPSE		6
#define SPLINE		7
#define CENTERLINE	30
#define HATCH		31
#define LDIM		33
#define ADIM		34
#define RDIM		35
#define DDIM		36
#define GROUP		50
#define DETAIL		51
#define SINST		65
#define CINST		66
#define TEXT		85

#define DXSTATUS	0	/* locations of elememtns within DAXCAD MIP buffer structure */
#define DXENTITY	1
#define DXCOLOR		2
#define DXLAYER		3
#define DXFORM		4
#define DXFONT		5
#define DXPDP		6
#define DXPARENT	7
#define DXTEXT		8
#define DXRELATION	9
#define DXPROP		10
#define DXTHICK		11
#define DXREG		12

#define MAXMIPBUFF 	13	/* DAXCAD buffer sizes */
#define MAXPDPBUFF 	6
#define MAXPDIBUFF 	4

#define MARKER_OPEN	8	/* marker symbol definitions must not be overwritten !!! */
#define MARKER_START	9
#define MARKER_END	10

#define ENTITY_IS_INVALID	-1	/* read write status conditions */
#define ENTITY_IS_DELETED 	-2
#define ENTITY_IS_NORMAL 	0

#define CONNECT_NONE	0		/* for routine Entity connect */
#define CONNECT_ONE	1		/* 1 connection points */
#define CONNECT_TWO	2		/* 2 connection points */

typedef double Matrix[9];		/* DAXCAD transform matrix type */

typedef short DAXCADINT;
typedef int DAXCADLOGICAL;
typedef float DAXCADREAL;

int MisomexInit = 0;	/* indicates system initalised */
int MxProfileNum = 0;	/* the maximum number of profiles defined */

typedef struct {	/* definitions of a DAXCAD world coordinate */
		
			double x;
			double y;
		}WorldXY;
typedef struct	{
			WorldXY point;	/* startng point in the world */
			double scalex;	/* x and y scale factors */
			double scaley;		
			double rotang;	/* rotation angle in radans */
			int num;	/* marker number to use */
			int layer;	/* layer to create on */
			int font;	/* font to create lines */
			int direction;	/* If marker relates to an arc this indicates the arcs direction from the marker */
			} Marker;

struct EntityBuffer	{				/* this contain information about the DAXCAD entity	*/

				DAXCADINT mip[MAXMIPBUFF];	/* Master index buffer */
				DAXCADINT pdi[MAXPDIBUFF];	/* part data index buffer */
				double pdp[MAXPDPBUFF];	/* actual geometric data */
				char *text;		/* possible text data */
				Matrix m;		/* transform asociated with this element */

			};

struct ProfileElem 	{
				struct EntityBuffer entity;		/* element information */
				struct ProfileElem *next;		/* pointer to next element */

			};


struct ProfileElem *Profiles[MAXPROFILES];	/* contains the pointers to new profile sequences */

struct EntityBuffer GlobalEntity;		/* the buffer to use during reads or writes to daxcad database  */ 


#define SUCCESS		0			/* succes code from functions with tcurs options */
#define MAIN_MENU	1			/* specifies normal return from subordinate menu */
#define EXIT_MENU	-1			/* return state controls what main manu will do when comming out of subordinate menus */

#define MISOMEX_MAIN		1		/* DAXCAD menu defitions for MxMenu */
#define MISOMEX_CREATE_SEQ	2
#define MISOMEX_EDIT_SEQ	3
#define MISOMEX_SET_DATUM	4
#define MISOMEX_CLEAR_ALL	5
#define MISOMEX_POINTS		6
#define MISOMEX_DRILL		7

#define NOUN_MENU	2
#define VERB_MENU	3
#define CALC_MENU	1
#define DISP_MENU	4

#define MAIN_HEADER		589
#define CREATE_SEQ		590
#define EDIT_SEQ		591
#define SET_DATUM		592
#define OUTPUT_DATA		593
#define VIEW_PROFILES		595
#define PROPERTIES		607

#define CREATE_HEADER		594
#define RESTART_PROFILE		408
#define QUIT_POINTS		597
#define QUIT_CREATE		598
#define DRILL_WINDOW		613
#define DRILL_CREATE		614

#define EDIT_HEADER		599
#define EDIT_INSERT		600
#define EDIT_DELETE		601
#define EDIT_SWAP		603
#define EDIT_RESEQ		604
#define EDIT_PROP		605
#define EDIT_QUIT		606


#define ACCEPT			126
#define CANCEL			121
#define AUTO_PROFILE		469

#define MENU_ON			1		/* Mxmenu control flags */
#define MENU_OFF		0


#define SINGLE_ARC		0		/* single entity cases */
#define SINGLE_CIRCLE		1
#define SINGLE_LINE		2

static int CalcMenu = CALC_MENU;		/* varous DAXCAD menu number codes */
static int NounMenu = NOUN_MENU;
static int VerbMenu = VERB_MENU;
static int DisplayMenu = DISP_MENU;

static int MainHeader =	MAIN_HEADER;		/* all MISOEX interface cell number defineitions */
static int CreateSeq =	CREATE_SEQ;
static int EditSeq =	EDIT_SEQ;
static int SetDatum =	SET_DATUM;
static int OutPut =	OUTPUT_DATA;		/* post process data */
static int ViewProfiles= VIEW_PROFILES;
static int Properties = PROPERTIES;


static int CreateHeader = CREATE_HEADER;	/* Create Seq sub menu */
static int RestartProfile = RESTART_PROFILE;
static int AutoProfile = AUTO_PROFILE;
static int QuitCreate = QUIT_CREATE;
static int QuitPoints = QUIT_POINTS;
static int DrillWindow= DRILL_WINDOW;
static int DrillCreate= DRILL_CREATE;
static int Accept = ACCEPT;
static int Cancel = CANCEL;

static int EditHeader = EDIT_HEADER;		/* edit sequence sub menu */
static int EditInsert = EDIT_INSERT;
static int EditDelete = EDIT_DELETE;
static int EditSwap = EDIT_SWAP;
static int EditReseq = EDIT_RESEQ;
static int EditProp = EDIT_PROP;
static int EditQuit = EDIT_QUIT;

static WorldXY LastMarker;			/* Last position of marker */

static int ProfileCell;				/* cell in main menu */

int NextSeq = 1;				/* the next sequence to be used */
int NumData = 0;				/* number of items in current swindu */

static unsigned int F77True = -1;		/* F77 logical definitions */
static unsigned int F77False= 0;

static int LayerInit = 0;			/* initalise  */
static int LayerOn = 1;				/* switch on special layer */
static int LayerOff = 2;			/* turn off all layers for viewing */
static int LayerBack = 3;			/* bring back all layers */

static int MisomexLayer = 255;			/* special layer can be changed in config file */

#define VIEW_NORMAL		0
#define VIEW_LAYER		1		/* specifies that special layer is being viewed */

#define MAINMODE 			0		/* Main menu */
#define EDITING 			1		/* Edit mode */
#define CREATING			2
#define SETDATUM			3

static int CurrentMode = VIEW_NORMAL;
static int EditMode = EDITING;

/* Error Prompts etc defintions here */

static int NoProfileDefined = 142;		/* used if he tries to accept an empty profile */
static int IndicateStart = 381;			/* used for start point on profile */
static int IndicateFinish = 382;			/* used for finsh point on profile */
static int IndicateGroup = 828;			/* used for finsh point on profile */
static int NoAlloc= 829;					/* Editlist cannot be allocated */
static int IndicateDatum = 831;			/* indicate a dataum point */
static int NoProfileFound = 832;			/* profile has not been found */
static int NoNcOption = 834;			/* in main menu no options set */
static int NoNcOpen = 835;			/* cannot open NC file */



/*	Profile editing globals and defintions */

typedef struct	{			/* editing profile structure need to malloc and realloc */

			int seqno;	/* store sequnce number for undoing */
			int mipp;	/* master index pointer value of group */

		} EditProfile;

static EditProfile *EditList;		/* main list for current editing list */

static int EditMalloc;			/* current number of elements in editlist */
static int EditElements;		/* the acutal number of sequenial elements to be edited */


/*	Global Datum */

#define MARKER_ORIGIN 7		/* marker is an origin */
#define DATUM_LOCATE  0
#define DATUM_RETURN  1

static int MxDatum[2];		/* datum values ( Xand Y ) */


/*	Generic Properties codes */

#define PROP_POPUP	22
#define PROP_EDITPOPUP	23
static int PropertiesPopup = PROP_POPUP;
static int PropertiesEditPopup = PROP_EDITPOPUP;

#define DAXCAD_INT	2	/* DAXCAD property types */
#define DAXCAD_REAL	4
#define DAXCAD_TEXT	8


static int NoPropsSet = 833;	/* No properties in daxcad-defalts file */

#define MAXPROPS 10

typedef struct {	/* Local property strucure definitions */
		char name[81];			/* prop name */
		char prompt[81];		/* prompt */
		int type;			/* DAXCAD type */
		DAXCADINT mip;			/* Master index of property */
		int noun;			/* noun number of popup cell */
		} NcProperty;
	
NcProperty *NcStdProps[MAXPROPS];

static int NcPropCount = 0;		/* Current number of properties defined */

FILE *OutputFP;				/* Output file descripter */

/*

 *      Useful mathmatical constants:
 *
 * M_E          - e
 * M_LOG2E      - log2(e)
 * M_LOG10E     - log10(e)
 * M_LN2        - ln(2)
 * M_PI         - pi
 * M_PI_2       - pi/2
 * M_PI_4       - pi/4
 * M_1_PI       - 1/pi
 * M_2_PI       - 2/pi
 * M_2_SQRTPI   - 2/(sqrt(pi)
 * M_SQRT2      - sqrt(2)
 * M_SQRT_2     - 1/sqrt(2)
*/

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

