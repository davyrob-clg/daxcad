

/* SCCS id Keywords             @(#)  412.1 date 6/11/92 ncpost.h   */

/*
		Include file for ncpost

*/

static char Infile[1024];	/* input file name */
static char Outfile[1024];	/* output file name */

static char DrgName[1024];		/* drawing name */
static double DrgScale=1.0;		/* drawing scale */
static double DbuFactor=1.0;		/* database unit factor */
static double DbuTol=1.0;		/* relative tolernce factor */
static char DrgUnits[3];		/* units char representation */

#define RELTOL 1e-5			/* relative tolerance accuracy */

static int Dbunit=0;		/* base unit type */
static int NcType=0;		/* NC output type */

static FILE *Ifp=stdin;			/* File pointers */
static FILE *Ofp=stdout;		/* set as default output */

#define NC_MISOMEX	1	/* plotter defines */
#define NC_GNC		2


#define MAXBUFF		1024

#define END_OF_SEQ	0
#define END_OF_FILE	-1


static struct MisomexParameters {		/* Struc defintions for NC paramaters */

					double factor;
					int dwell1;
					int dwell2;
					int linetype;
					int velocity;
					int pen;
					int cutang;	/* maximum cutting angle */
				} Misomex;

char Buffer[1025];

static double NcGlobal[2];

#define XVAL	0
#define YVAL	1
					
static int NcSeq=0;	/* flag to indicate sequnce counter reset at every new sequence */
static int NcSeqnum=0;	/* sequence number */

static int NcDebug=0;	/* debug flag set from arg list */
