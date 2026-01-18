
/*      @(#)  412.1 date 6/11/92 aposerv_c.c 
 *
 *
 *        Filename    : aposerv_c.c
 *        Version     : 412.1
 *        Retrieved   : 92/06/12 15:47:23
 *        Last change : 92/06/11 14:23:36
 *
 *        Copyright : Practical Technology Limited  
 *        File :- aposerv_c.c
 *
 *        Machine independant/dependant routines
 *
 *
*/

#include <stdio.h>
#include <sys/file.h>

#include <time.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>


#include "daxcad_functions.h"
#include "xlang.h" /* Cross language refernce file must be updated if
                         any functions added to in this file 
                      */

#define MAXCOMLEN 1024
#define MAXFILE 1025 /* Number of bytes to store */

#define True 0xFFFF
#define False 0

#define MICROSECOND (double)0.000001

static char *EnvDefaultShells[] = {"DAXCAD_SHELL",
                                   "SHELL",
                                   "shell",
                                   "\0"};

static char *SystemShells[] = {"/bin/sh",
                               "/com/sh",
                               "/bsd4.2/bin/sh",
                               "/bsd4.3/bin/sh",
                               "/sys5/bin/sh",
                               "/sys5.3/bin/sh",
                               "\0"};

static char *SystemCopy[] = {"/bin/cp",
                             "/com/cpf",
                             "\0"};

static int streams[] = {-1, -1, -1}; /* stream control */

char localcom[MAXFILE]; /*  local command string */

char *strdupn();

static char ErrorMessage[MAXFILE]; /* the current system error message */
static int curerrno;               /* currebt error message number */

struct mitag
{
    unsigned char c1,
        c2,
        c3,
        c4,
        c5,
        c6;
    short r7,
        r8,
        r9,
        r10,
        r11,
        r12,
        r13;
};

struct pditag
{
    short r1, r2, r3, r4;
};
struct pdrtag
{
    float r1, r2, r3, r4, r5, r6;
};

typedef short ent_type;

void SHELLPC(Command, Comlen, st)

    /* Description   :- Invokes a program in the command line 
       *                  It must use a shell in order to invoke
       * 
       *
       * Return status :- 1    ->    Length is invalid 
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

    char *Command; /*  <i>     The command line to exec */
int *Comlen;       /*  <i>     The length of the command string */
int *st;

{

    //char *getenv();
    char *shell;
    int i;
    int len;
    int status;
    int retcode;
    char *directinvoke;

    if (*Comlen <= 0 || *Comlen > MAXCOMLEN)
    {

        *st = 1;
        return;
    }

    i = 0; /* initalise the i */

    shell = (char *)NULL;

    while (*EnvDefaultShells[i])
    { /* look for an environment  shell */

        shell = getenv(EnvDefaultShells[i++]);
        if (shell)
        {
            if (*shell)
                break;
        }
    }

    if (!shell)
    { /* no env look for actuacl shell */

        i = 0;
        while (SystemShells[i])
        {

            shell = SystemShells[i++];
            if (!access(shell, F_OK)) /* got something valid */
                break;
        }
    }

    directinvoke = getenv("DAXCAD_NOSHELL"); /* if this is set then dont use it */

    if (shell && !directinvoke)
    { /* ok build up a command string */

        if (*shell)
        {
            strcpy(localcom, shell); /* use a shell */
            strcat(localcom, " -c ");
            strncat(localcom, Command, *Comlen);
        }
    }
    else
    {

        strncpy(localcom, Command, *Comlen);
    }

    len = strlen(localcom);

    SPAWNPROCESS(streams, localcom, &len, &retcode, &status); /* spawn the process */

    *st = status;
}

void
    DELETEC(File, Length, st)

    /* Description   :- This will delete the file specified. It uses
       *                  the unix lib unlink() to do the deletion.
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

    char *File; /*  <i>  File name to be deleted */
int *Length;    /*  <i>  Length of file */
int *st;        /*  <o>  return flag */

{

    int len;
    char *f;

    *st = 0;

    if (*Length <= 0 || *Length > MAXFILE)
    {
        *st = 1;
        return;
    }

    f = strdupn(File, *Length);

    if (unlink(f))
    {

        RelDaxDisplay();
        perror("[deletec]"); /* system error */
        AqDaxDisplay();
        daxseterror(); /* set daxcad error message */
        free(f);
        return;
    }

    free(f);
}

void f77strcpy(string1, string2, lens2) /* f77 string copy */

    /* Description   :- Copies a f77 string into something c can do with it
       *                  bu appending a null at the last active character
       * 
       *
       * Return status :- None
       *                  
       *   
       *                  
       * Notes         :- NOT called from F77
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    char *string1; /*  <o>  The outgounf new string (Must be supplied ) */
char *string2;     /*  <i>  F77 type string */
int lens2;         /*  <i>  Length of f77 stirng */
{
    int i;
    for (i = 0; i < lens2; i++)
        string1[i] = string2[i];
    string1[lens2] = '\0';
}

char *strdupn(String, Length)

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

COPYFC(Target, Tlen, Source, Slen, st)

/* Description   :- Copies one file to another file. Uses a system utiliity
       *                  and uses spawn process to do the copy. If Apollo decide to 
       *                  do it prperly then we can to a proper copy. 
       *
       *
       * Return status :- 1    ->      No external copy will exist.
       *                  <0   ->      Some error from the invoke caused the problem
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

char *Target; /*  <i>     Target file for copy */
int *Tlen;    /*  <i>     Length */
char *Source; /*  <i>     Source file for copy */
int *Slen;    /*  <i>     length */
int *st;

{

    int i;
    char *cp;
    char *f1;
    char *f2;
    int len;
    int status;
    int retcode;

    *st = 0;

    i = 0;

    while (SystemCopy[i])
    {

        cp = SystemCopy[i++];
        if (!access(cp, F_OK)) /* got something valid */
            break;
    }

    if (!cp)
    {

        *st = 1;
        return;
    }

    f1 = strdupn(Target, *Tlen); /* get local stirng */
    f2 = strdupn(Source, *Slen);

    sprintf(localcom, "%s %s %s ", cp, f1, f2); /* build command line */

    len = strlen(localcom);
    SPAWNPROCESS(streams, localcom, &len, &retcode, &status); /* spawn the process */

    free(f1); /* free up strings */
    free(f2);

    if (retcode)
        *st = -retcode; /* an error came form the invoker */
}

DIRFINC(Pathname, Plen, SearchFiles, Slen, st)

/* Description   :- Searches the SearcFiles path and wild card
       *                  putting the output into Pathname
       * 
       *
       * Return status :- 1    ->      Incomplete path and wild card
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

char *Pathname;    /*  <i>     output pathname */
int *Plen;         /*  <i>     Length */
char *SearchFiles; /*  <i>     Searchfile pattern and directory */
int *Slen;         /*  <i>     Length */
int *st;

{

    char *path;
    char *wildc;
    char dir[MAXFILE];
    char *search;
    int match;
    int status;
    int typed;
    int len1, len2, len3;
    int len;

    *st = 0;
    len = *Slen + 1; /* need one more for end of string */
    typed = 0;

    search = (char *)strdupn(SearchFiles, len);
    path = (char *)strdupn(Pathname, *Plen);

    wildc = (char *)rindex(search, '/');

    if (!wildc)
    {
        *st = 1;
        return;
    }
    else
    { /* null the string at the / and increment wildc */

        *wildc = '\0';
        wildc++;
    }

    len = strlen(wildc) - 1;
    wildc[len] = '$'; /* append end of character */

    len1 = strlen(search); /* length of directory to search */
    len2 = strlen(wildc);
    len3 = strlen(path);

    /*  tools_get_directory(search, &len1,
                       wildc,  &len2,
                       path,   &len3,
                       &typed,&match, &status);

*/
    if (status)
        *st = status;

    free(search);
    free(path);
}

DIRFINC1(Pathname, Plen, SearchFiles, Slen, st)

/* Description   :- Searches the SearcFiles path and wild card
       *                  putting the output into Pathname
       * 
       *
       * Return status :- 1    ->      Incomplete path and wild card
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

char *Pathname;    /*  <i>     output pathname */
int *Plen;         /*  <i>     Length */
char *SearchFiles; /*  <i>     Searchfile pattern and directory */
int *Slen;         /*  <i>     Length */
int *st;

{

    char *path;
    char *wildc;
    char dir[MAXFILE];
    char *search;
    int match;
    int status;
    int typed;
    int len1, len2, len3;
    int len;

    *st = 0;
    len = *Slen + 1; /* need one more for end of string */
    typed = 0;

    search = (char *)strdupn(SearchFiles, len);
    path = (char *)strdupn(Pathname, *Plen);

    wildc = (char *)rindex(search, '/');

    if (!wildc)
    {
        *st = 1;
        return;
    }
    else
    { /* null the string at the / and increment wildc */

        *wildc = '\0';
        wildc++;
    }

    len = strlen(wildc) - 1;
    wildc[len] = '$'; /* append end of character */

    len1 = strlen(search); /* length of directory to search */
    len2 = strlen(wildc);
    len3 = strlen(path);

    GET_DIRS_WILD(search, &len1,
                  wildc, &len2,
                  path, &len3,
                  &match, &status);

    if (status)
        *st = status;

    free(search);
    free(path);
}

daxseterror()

/* Description   :- Sets the current error message from the system
       *                  I hope its machine indep.
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

{

    if (errno) {
        int errnum = errno;
        // Print error number
        printf("Error number: %d\n", errnum);

        // Get human-readable error message
        printf("Error message: %s\n", strerror(errnum));

        sprintf(ErrorMessage, "%d : %s", errno, strerror(errnum));
    }
    curerrno = errno;
}

DAXGETERROR(String, Passed, Length, st)

/* Description   :- Gets the current error message set.
       *                  String is assumed to have at least the Passed
       *                  length
       *
       * Return status :- 1        ->      length is truncated
       *                  2        ->      No message is set.
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

char *String; /*  <o>     The string to be set */
int *Passed;  /*  <i>     The maximum number of bytes in the string */
int *Length;  /*  <o>     The number of bytes to be set */
int *st;

{

    int l;

    if (!curerrno)
    {
        *st = 2; /* nothing to be set */
        return;
    }

    l = strlen(ErrorMessage);

    if (l > *Passed)
        l = *Passed;

    strncpy(String, ErrorMessage, l);

    *Length = l;
}

PGM_$EXIT()

/* Description   :- Exits from F77 cleanly
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- Historical I think
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{
    /* SPB - 230994 - Added bits to remove temporary files */
    char tryit[80];
    /* Do a little tidying up - remove tmp.????pid files */
    sprintf(tryit, "/bin/rm -f tmp.[A-z][A-z][A-z][A-z][A-z]%05d", getpid());
    system(tryit);
    /* SPB - 230994 - Added bits to remove temporary files */

    /* WTF is this for eh ? */
    sleep(2); /* wait */
    exit(0);  /* shut */
}

INQFS1C(File, Length, st)

/* Description   :- This routine will return the file status requested present
       *                  state within the system. 
       *                  
       *                
       * Return status :  1  File cannot be opned for writing
       *                
       *   
       *                  
       * Notes         :- Uses open for append Should be portable.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

char *File;  /*  <i>     the file to be checked */
int *Length; /*  <i>     Length */
int *st;     /*  <i>     Apollo error code (Not applicable )*/

{

    FILE *fp;
    char *f;
    int len;

    *st = 0;

    f = strdupn(File, *Length); /* get local copy */

    fp = fopen(f, "a");

    if (!fp)
    {
        daxseterror();
        *st = 1;
    }
    else
    {

        fclose(fp);
    }

    free(f);
}

LOCALT(TimeData)

/* Description   :- Gets the local time format
       * 
       *       (1)   year
       *       (2)   month
       *       (3)   day
       *       (4)   hour
       *       (5)   min
       *       (6)   sec
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

int TimeData[6]; /* <o>  Array containg time data */

{

    struct tm *data;
    int clock;

    time(&clock);

    data = localtime(&clock); /* get the struct */

    TimeData[0] = data->tm_year;    /* Apollo modify time ?????*/
    TimeData[1] = data->tm_mon + 1; /* set cos 11 is december */
    TimeData[2] = data->tm_mday;
    TimeData[3] = data->tm_hour;
    TimeData[4] = data->tm_min;
    TimeData[5] = data->tm_sec;
}

SIZEC(File, Length, LineMax, LineCount, st)

/* Description   :- This function will return the number of lines
       *                  and the biggest line in the file. It uses
       *                  file descripters for speed.
       *
       * Return status :- 1    ->      Could not open the file.
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

char *File;     /*  <i>  Filename */
int *Length;    /*  <i>  Length */
int *LineMax;   /*  <o>  Maximum line length */
int *LineCount; /*  <o>  Total line count */
int *st;

{

    char *f;
    int fd;
    int lm;
    int lc;
    int n; /* return number of chars read */
    int bc;
    char buff;

    *st = 0;
    *LineMax = 0;
    *LineCount = 0;

    lm = 0;
    lc = 0;
    bc = 0;

    f = strdupn(File, *Length); /* get local copy */

    fd = open(f, O_RDONLY); /* open file for buffering */

    if (!fd)
    {
        *st = 1;
        daxseterror();
    }
    else
    {

        while (n = read(fd, &buff, 1))
        { /* read 1 byte */

            bc++; /* byte count */
            if (buff == '\n')
            {
                lc++; /* inc line counter */
                if (bc > lm)
                    lm = bc - 1;
                bc = 0;
            }
        }
        close(fd); /* close file */
    }

    *LineMax = lm;
    *LineCount = lc;

    free(f); /* free up file name and go home */
}

TIMEWAIT(Time)

/* Description   :- Waits for a period of time
       * 
       * 
       *
       * Return status :- None
       *                  
       *   
       *                  
       * Notes         :- The minimum time is 1 second
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

float *Time; /*  <i>     Time to wait */

{

    int w;

    if (*Time < 1.0 || *Time > 32700.0) /* a reasonable time to wait */
        return;

    w = *Time;
    sleep(w);
}

UNIXTIME(CurrentTime)

/* Description   :- This routine will return the current unix clock time
       *                  in seconds as a double preciosn time of seconds and
       *                  micro seconds.
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

double *CurrentTime; /*  <o> Current time as DP number */

{

    time_t tp;       /* timeval strcture */
    double fraction; /* the fraction of time in mircoseconds */

    /*gettimeofday(&tp,&tzp);*/ /* get actual time from system */

    *CurrentTime = time(&tp); /* get first portion of time */

    fraction = (double)tp * MICROSECOND;

    *CurrentTime += fraction; /* add on float time */
}

split_i2(in, out1, out2) short *in;
short *out1, *out2;
{
    unsigned char *temp;
    temp = (unsigned char *)in;
    *out1 = *temp;
    *out2 = *(++temp);
}

MIRGETOLD(mifile, p, imbuff)

/* Description   :- Part data record decoder. Called from F77 it decodes       
       *                  a packed record into usefule data.
       *                                                                             
       *                                                                             
       * Return status :- None                                                       
       *                                                                             
       *                                                                             
       *                                                                             
       * Notes         :- This was designed to make the job of decoding faster       
       *                  The record required is passwd as a pointer from f77 and    
       *                  decoded in the template structure mitag
       *                  
       *                  
       *                  
       *
       */

struct mitag *mifile; /*  <i>     Pointer to MI record */
short *p;             /*  <i>     record number */
short imbuff[13];     /*  <o>     filled record buffer */
{

    mifile += (*p - 1);
    imbuff[0] = (unsigned char)mifile->c1;
    imbuff[1] = (unsigned char)mifile->c2;
    imbuff[2] = (unsigned char)mifile->c3;
    imbuff[3] = (unsigned char)mifile->c4;
    imbuff[4] = (unsigned char)mifile->c5;
    imbuff[5] = (unsigned char)mifile->c6;
    imbuff[6] = mifile->r7;
    imbuff[7] = mifile->r8;
    imbuff[8] = mifile->r9;
    imbuff[9] = mifile->r10;
    imbuff[10] = mifile->r11;
    imbuff[11] = mifile->r12;
    imbuff[12] = mifile->r13;
}

PDRGETOLD(pdfi, pdfr, p, idbuff, rdbuff)

/* Description   :- Part data record decoder. Called from F77 it decodes
       *                  a packed record into usefule data.
       * 
       *
       * Return status :- None 
       *                  
       *   
       *                  
       * Notes         :- This was designed to make the job of decoding faster
       *                  The record required is passwd as a pointer from f77 and
       *                  decoded in the template structure pdutag and pdrtag        
       *                  
       *                  
       *                  
       *
       */

struct pditag *pdfi; /*  <i>  Pointer to structure */
struct pdrtag *pdfr; /*  <i>  Pointer to structure */
short *p;            /*  <i>  part data record */
short idbuff[4];     /*  <o>  Out going filled buffer */
float rdbuff[6];     /*  <o>  Out going filled buffer */
{
    pdfi += (*p - 1);
    pdfr += (*p - 1);
    idbuff[0] = pdfi->r1;
    idbuff[1] = pdfi->r2;
    idbuff[2] = pdfi->r3;
    idbuff[3] = pdfi->r4;
    rdbuff[0] = pdfr->r1;
    rdbuff[1] = pdfr->r2;
    rdbuff[2] = pdfr->r3;
    rdbuff[3] = pdfr->r4;
    rdbuff[4] = pdfr->r5;
    rdbuff[5] = pdfr->r6;
}

short
    AND_2(in1, in2)

    /* Description   :- Simples and function used for F77 functions extension
       * 
       * 
       *
       * Return status :- None returns the anded result
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

    short *in1; /*  <i> args 1 */
short *in2;     /*  <i> args 2 */

{
    short temp;

    temp = *in1 & *in2;
    return temp;
}

/*


   ========================================================

                  GPRX/DAXCAD support routines 

   ========================================================

*/

#include "gprx_types.h"
#include "gprx_errors.h"

#define WINDOW_FONT "6x10" /* window font can be redifined */
#define LINE_SPACING 2     /* Line spacing of text */
#define Y_BORDER 5         /* y postion to start drawing text */
#define X_BORDER 5         /* x postion to start drawing text */

static FILE *fp;                 /* file pointer to read file */
static GprBitmapDesc WindowDesc; /* returned descripter */

POPAWINDOW(File, Length, Rect, st)

/* Description   :- Pop an X window on the screen with the contents
       *                  of the file File on the window
       * 
       * 
       *
       * Return status :- The file does not exist
       *                  
       *   
       *                  
       * Notes         :- This simply paints a window on the screen and waits
       *                  for any button hit to clear it.
       *                  it uses GPRX lib calls to make the job simpler.
       *                  
       *                  
       *                  
       *                  
       *
       */
char *File;     /*	<i>	The input file to be used */
int *Length;    /*  <i> Length of filename */
GprWindow Rect; /*	<i>	The size of the window to be displayed */
GprStatus *st;

{

    short opmode;        /* GPR op mode must be 3 to give window */
    GprBitmapSize size;  /* Size of window to be drawn */
    GprPlanes hiplane;   /* number of pleanes */
    short unit;          /* UNIT number not used */
    GprColor white;      /* White color defined from server */
    GprColor black;      /* Black color defined from server */
    GprStatus status;    /* status of GPR calls */
    GprOrigin pos;       /* actual window pos in real space */
    GprBitmapDesc cdesc; /* current descripter */
    int waiting;         /* event loop control */
    short fontid;        /* fontid of text font */
    short len;           /* local length */
    short x, y;          /* currennt poisiotn of text */
    short i;
    short evtype; /* event type needed */
    short keyset[8];
    char evdata;          /* event data */
    char asciitext[1024]; /* string to hold all characters for extents */
    char *string;
    char localname[255];

    f77strcpy(localname, File, *Length);

    fp = fopen(localname, "r"); /* open file for reading */

    if (!fp)
    {
        *st = 1;
        return;
    }

    GPR_$INQ_ROOTWINDOW(size, &hiplane, &white, &black, st); /* get the size of the root window */

    size[0] = Rect[2];
    size[1] = Rect[3];

    pos[0] = Rect[0];
    pos[1] = Rect[1];

    GPR_$SET_WINDOW_START(size, &status); /* set the starting position for the window */

    opmode = 3;
    GPR_$INIT(&opmode, &unit, size, &hiplane, &WindowDesc, &status);

    evtype = gpr_keystroke; /* eventing */
    GPR_$ENABLE_INPUT(&evtype, keyset, st);
    evtype = gpr_buttons;
    GPR_$ENABLE_INPUT(&evtype, keyset, st);
    evtype = gpr_locator;
    GPR_$ENABLE_INPUT(&evtype, keyset, st);

    len = strlen(WINDOW_FONT);
    GPR_$LOAD_FONT_FILE(WINDOW_FONT, &len, &fontid, &status); /* load font to be used */

    GPR_$SET_TEXT_FONT(&fontid, &status); /* set text font on canvas */

    DrawText();

    GPR_$SERVER_FLUSH_X(&status);

    waiting = 1;

    while (waiting)
    {

        GPR_$EVENT_WAIT(&evtype, &evdata, pos, &status);

        waiting = !(evtype == gpr_keystroke || evtype == gpr_buttons);
    }

    fclose(fp);
    GPR_$DEALLOCATE_BITMAP(&WindowDesc, &status);

    cdesc = 0;

    GPR_$SET_BITMAP(&cdesc, &status);
}

DrawText()
/* Description   :- Paints in the text at the start of the window
       * Return status :- NONE
       * Notes         :- 
       */
{
    GprBitmapSize size; /* Size of window to be drawn */
    GprBitmapSize window;
    GprStatus status;
    GprPlanes hiplane;
    GprColor white; /* White color defined from server */
    GprColor black; /* Black color defined from server */
    short len;
    short x, y;
    int running;
    char asciitext[1024]; /* string to hold all characters for extents */
    char *string;

    GPR_$INQ_ROOTWINDOW(size, &hiplane, &white, &black, &status); /* get the size of the root window */
    GPR_$CLEAR(&black, &status);                                  /* clear for repaints */
    GPR_$INQ_BITMAP_DIMENSIONS(&WindowDesc, window, &hiplane, &status);

    rewind(fp);

    len = 1;
    GPR_$INQ_TEXT_EXTENT("M", &len, size, &status);

    x = X_BORDER;
    y = size[1] + Y_BORDER + LINE_SPACING;

    GPR_$MOVE(&x, &y, &status); /* move to first text postion */

    running = 1;

    while ((int)(string = (char *)fgets(asciitext, 1022, fp)) && running)
    {

        len = strlen(asciitext) - 1; /* strip of CR character */
        GPR_$TEXT(asciitext, &len, &status);
        y += size[1] + LINE_SPACING;
        GPR_$MOVE(&x, &y, &status);
        if (y > window[1])
        {
            running = 0;
        }
    }
}

POPPEDWINDOW()
/* Description   :- Repaints popped window This will be called by some
       *                  other repaint routine. It assumes to repaint just
       *                  the size.
       *
       * Return status :- 
       * Notes         :- 
       */
{
    DrawText();
}

GETMPFILE(Name, Supplied, Length, St)

/* Description   :- Return a tempory file name for use by the system
       *                  Intended for F77 use
       *                
       * Return status :- 0 Success
       *                 -1 Supplied length no good
       *                
       * Notes         :- 
       */
char *Name;    /* <o> Name of file */
int *Supplied; /* <i> Supplied buffer length */
int *Length;   /* <o> returned length */
int *St;       /* status */
{

    char temp[1024]; /* maximum size of file name */
    int len;

    *St = 0;

    tmpnam(temp); /* temp file for script to exec */

    len = strlen(temp);

    if (*Supplied < len)
    {
        *St = -1;
        return;
    }

    *Length = len;
    strcpy(Name, temp);
}

GETNODEID(Id)

/* Description   :- Gets a hostid in a machine independant format
       *                  
       * Return status :- NONE should return something
       *                  
       * Notes         :- 1 .. Apollo uses environ to get nodeid
       *                  2 .. Sun uses simple hostid call
       */
int *Id; /* <o> Hostid id */
{

    char *nodeid;
    int nodenum;
#ifdef HP700
#include <sys/utsname.h>
    struct utsname name;
#endif

#ifdef APOLLO
    nodeid = getenv("NODEID");
    sscanf(nodeid, "%x", &nodenum);
    *Id = nodenum;
#endif
#ifdef SUN
    *Id = gethostid();
#endif

#ifdef HP700
    uname(&name);
    nodenum = atoi(name.idnumber);
    *Id = nodenum;
#endif
}

DAXGETSIZE(Size)

/* Description   :- Gets a description of the size of DAXCAD
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- 0 No size variable detected
       *                  1 Large   1280x1024
       *                  2 Medium  1152x900
       *                  3 Small   1024x800
       *                  4 Special 1024x768
       *                  5 micro   (dingky)
       *
       */

int *Size; /* <o> Size variable */

{
    char *var;

    *Size = 0;
    var = getenv("DAXCAD_SCREENSIZE"); /* if this is set then dont use it */

    if (var)
    {

        if (strcmp(var, "large") == 0)
            *Size = 1;
        else if (strcmp(var, "medium") == 0)
            *Size = 2;
        else if (strcmp(var, "small") == 0)
            *Size = 3;
        else if (strcmp(var, "special") == 0)
            *Size = 4;
        else if (strcmp(var, "micro") == 0)
            *Size = 5;
        else if (strcmp(var, "screen") == 0)
            *Size = 0;
    }
}

SETHOME()

/* Description   :- Sets users home directory
       * 
       * 
       *
       * Return status :- NONE
       *                  
       *   
       *                  
       * Notes         :- No return. Simply sets users home direcory
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

{
    char *var;

    var = getenv("HOME"); /* get home directory */

    if (var)
    {
        chdir(var); /* set it anyway */
    }
}

ICHAR(Chr)

/* Description   :- For systems that return a -ve number > 127
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
char *Chr; /* <i> Cahracter variable */

{

    unsigned char chr;

    chr = (unsigned char)*Chr;
    return chr;
}

DAX_XOR(Arg1, Arg2, Output)

/* Description   :- XORs Arg1 and Arg2 and return value
       *                  in Output.
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

unsigned int *Arg1;   /* <i>  first value to be XORed  */
unsigned int *Arg2;   /* <i>  first value to be XORed  */
unsigned int *Output; /* <o>  Resultant                */
{
    *Output = *Arg1 ^ *Arg2;
}

int
    IsBitSet(bit, mask)

    /* Description   :- checks if bit is set in mask
       *                  
       *                
       *                
       * Return status :-
       *                
       *                
       *                
       * Notes         :- returns true if bit set 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

    unsigned int bit; /* <i>  bit to be checked        */
unsigned int mask;    /* <i>  mask to be checked       */
{
    return 1 << bit & mask;
}
