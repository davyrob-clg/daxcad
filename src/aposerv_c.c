
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
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>


#include "daxcad_functions.h"
#include "xlang.h" /* Cross language refernce file must be updated if
                         any functions added to in this file 
                      */

/* External display lock helpers are defined elsewhere in the graphics layer. */
extern void RelDaxDisplay(void);
extern void AqDaxDisplay(void);
extern void SPAWNPROCESS(int *streams, char *command, int *length, int *retcode, int *status);
extern void get_dirs_wild_(char *search, int *search_len,
                                    char *wild, int *wild_len,
                                    char *path, int *path_len,
                                    int *match, int *status);
extern int GPR_$INQ_ROOTWINDOW();
extern int GPR_$SET_WINDOW_START();
extern int GPR_$INIT();
extern int GPR_$ENABLE_INPUT();
extern int GPR_$LOAD_FONT_FILE();
extern int GPR_$SET_TEXT_FONT();
extern int GPR_$SERVER_FLUSH_X();
extern int GPR_$EVENT_WAIT();
extern int GPR_$DEALLOCATE_BITMAP();
extern int GPR_$SET_BITMAP();
extern int GPR_$CLEAR();
extern int GPR_$INQ_BITMAP_DIMENSIONS();
extern int GPR_$INQ_TEXT_EXTENT();
extern int GPR_$MOVE();
extern int GPR_$TEXT();

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

char *strdupn(const char *String, int Length);
static void f77strcpy(char *string1, char *string2, int lens2);
static void DrawText(void);
void daxseterror(void);

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

void SHELLPC(char *Command, int *Comlen, int *st)

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

        shell = (char *)NULL;
        i = 0;
        while (SystemShells[i])
        {

            char *candidate = SystemShells[i++];
            if (!access(candidate, F_OK)) /* got something valid */
            {
                shell = candidate;
                break;
            }
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
        localcom[*Comlen] = '\0';
    }

    len = strlen(localcom);

    SPAWNPROCESS(streams, localcom, &len, &retcode, &status); /* spawn the process */

    *st = status;
}

void
    DELETEC(char *File, int *Length, int *st)

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

static void f77strcpy(char *string1, char *string2, int lens2) /* f77 string copy */

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

{
    int i;
    for (i = 0; i < lens2; i++)
        string1[i] = string2[i];
    string1[lens2] = '\0';
}

char *strdupn(const char *String, int Length)

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

void COPYFC(char *Target, int *Tlen, char *Source, int *Slen, int *st)

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
    cp = (char *)NULL;

    while (SystemCopy[i])
    {

        char *candidate = SystemCopy[i++];
        if (!access(candidate, F_OK)) /* got something valid */
        {
            cp = candidate;
            break;
        }
    }

    if (!cp)
    {

        *st = 1;
        return;
    }

    f1 = strdupn(Target, *Tlen); /* get local stirng */
    f2 = strdupn(Source, *Slen);

    snprintf(localcom, sizeof(localcom), "%s %s %s ", cp, f1, f2); /* build command line */

    len = strlen(localcom);
    SPAWNPROCESS(streams, localcom, &len, &retcode, &status); /* spawn the process */

    free(f1); /* free up strings */
    free(f2);

    if (retcode)
        *st = -retcode; /* an error came form the invoker */
}

void DIRFINC(char *Pathname, int *Plen, char *SearchFiles, int *Slen, int *st)

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

    wildc = (char *)strrchr(search, '/');

    if (!wildc)
    {
        *st = 1;
        free(search);
        free(path);
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
    status = 0;
    if (status)
        *st = status;

    free(search);
    free(path);
}

void DIRFINC1(char *Pathname, int *Plen, char *SearchFiles, int *Slen, int *st)

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

    wildc = (char *)strrchr(search, '/');

    if (!wildc)
    {
        *st = 1;
        free(search);
        free(path);
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

void daxseterror(void)

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

void DAXGETERROR(char *String, int *Passed, int *Length, int *st)

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

void PGM_$EXIT(void)

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
    snprintf(tryit, sizeof(tryit), "/bin/rm -f tmp.[A-z][A-z][A-z][A-z][A-z]%05d", getpid());
    system(tryit);
    /* SPB - 230994 - Added bits to remove temporary files */

    /* WTF is this for eh ? */
    sleep(2); /* wait */
    exit(0);  /* shut */
}

void INQFS1C(char *File, int *Length, int *st)

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

void LOCALT(int TimeData[6])

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

{

    struct tm *data;
    time_t clock;

    time(&clock);

    data = localtime(&clock); /* get the struct */

    TimeData[0] = data->tm_year;    /* Apollo modify time ?????*/
    TimeData[1] = data->tm_mon + 1; /* set cos 11 is december */
    TimeData[2] = data->tm_mday;
    TimeData[3] = data->tm_hour;
    TimeData[4] = data->tm_min;
    TimeData[5] = data->tm_sec;
}

void SIZEC(char *File, int *Length, int *LineMax, int *LineCount, int *st)

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

    if (fd < 0)
    {
        *st = 1;
        daxseterror();
    }
    else
    {

        while ((n = read(fd, &buff, 1)) > 0)
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

void TIMEWAIT(float *Time)

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

{

    int w;

    if (*Time < 1.0 || *Time > 32700.0) /* a reasonable time to wait */
        return;

    w = *Time;
    sleep(w);
}

void UNIXTIME(double *CurrentTime)

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

{

    time_t tp;       /* timeval strcture */
    double fraction; /* the fraction of time in mircoseconds */

    /*gettimeofday(&tp,&tzp);*/ /* get actual time from system */

    *CurrentTime = time(&tp); /* get first portion of time */

    fraction = (double)tp * MICROSECOND;

    *CurrentTime += fraction; /* add on float time */
}

void split_i2(short *in, short *out1, short *out2)
{
    unsigned char *temp;
    temp = (unsigned char *)in;
    *out1 = *temp;
    *out2 = *(++temp);
}

void MIRGETOLD(struct mitag *mifile, short *p, short imbuff[13])

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

void PDRGETOLD(struct pditag *pdfi, struct pdrtag *pdfr, short *p, short idbuff[4], float rdbuff[6])

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

void POPAWINDOW(char *File, int *Length, GprWindow Rect, GprStatus *st)

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

static void DrawText(void)
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

void POPPEDWINDOW(void)
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

void GETMPFILE(char *Name, int *Supplied, int *Length, int *St)

/* Description   :- Return a tempory file name for use by the system
       *                  Intended for F77 use
       *                
       * Return status :- 0 Success
       *                 -1 Supplied length no good
       *                
       * Notes         :- 
       */

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

void GETNODEID(int *Id)

/* Description   :- Gets a hostid in a machine independant format
       *                  
       * Return status :- NONE should return something
       *                  
       * Notes         :- 1 .. Apollo uses environ to get nodeid
       *                  2 .. Sun uses simple hostid call
       */

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

void DAXGETSIZE(int *Size)

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

void SETHOME(void)

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

int ICHAR(char *Chr)

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
{

    unsigned char chr;

    chr = (unsigned char)*Chr;
    return chr;
}

void DAX_XOR(unsigned int *Arg1, unsigned int *Arg2, unsigned int *Output)

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

{
    *Output = *Arg1 ^ *Arg2;
}

int IsBitSet(unsigned int bit, unsigned int mask)

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

{
    return 1 << bit & mask;
}
