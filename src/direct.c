

/* SCCS id Keywords             @(#)  403.1 date 3/2/92 direct.c   */

/*
 *
 *
 *        Filename    : direct.c
 *        Version     : 1.0
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Contains all routines for directory searching 
 *
 *        This file contains the following routines:
 *
 *        
 *        tools_swap
 *        tools_qswap
 *        tools_qsort
 *        tools_get_directory
 *        tools_search_this_directory
 *        tools_search_this_directory_raw
 *        valdir_
 *        existpath_
 *        toolssetcwd
 *        toolsgetcwd
 *  
 * 
 *   
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "xlang.h"
#include "daxcad_functions.h"

#define CHECK_BITS(X, Y) (((Y) == ((X) & (Y))) ? 1 : 0)

#define PATHSIZE 1024
#define TOOLSPATH 256

#define OUTPUT_FILE_ERROR -1
#define DIRECTORY_OPEN_ERROR -2

#define BUFFSIZE 1024
#define NLINES 1024
#define SUCCESS 0

#define OUTPUT_FILE_ERROR -1
#define TOO_MANY_LINES -2
#define EMPTY_FILE -3

typedef int INT_4;
typedef short INT_2;
typedef INT_4 LOGICAL;

extern void tools_sort();
extern void tools_qsort();
extern void tools_qswap();
extern void tools_search_wild();
extern char *strdup();

/* main(){ INT_4 len =11, status; sort("daxcad.test",&len,&status);} */

/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_sort(filename, filength, status)
    /* Description   :- Sort a file containing lines
                         *                  terminated by newlines
                         *
                         * Return status :- 0 SUCCESS
                         *                  NON 0 ERROR CODE
                         *                 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *filename; /* <i> sort file              */
INT_4 *filength;    /* <i> length of file name    */
INT_4 *status;      /* <o> return status          */
{

    FILE *fp;

    char *fnam, buffer[BUFFSIZE], *table[NLINES];

    INT_4 i, line_count = 0;

    *status = SUCCESS; /* set status ok */

    /* right lets copy this scabby FORTRAN filename */

    fnam = (char *)malloc(*filength + 1);
    strncpy(fnam, filename, *filength);

    /* null terminate them */

    fnam[*filength] = '\0'; /* null terminate it */

    if (!(fp = fopen(fnam, "r+")))
    {
        *status = OUTPUT_FILE_ERROR;
    }
    else
    {
        while (fgets(buffer, sizeof(buffer), fp) && line_count < NLINES)
        {
            table[line_count] = (char *)malloc(strlen(buffer) + 1);
            strcpy(table[line_count], buffer);
            line_count++;
        }

        if (line_count >= 0 && line_count < NLINES)
        {
            tools_qsort(table, 0, line_count - 1);

            rewind(fp); /* back to the start */

            for (i = 0; i < line_count; ++i) /* punt out lines and free mem */
            {
                fputs(table[i], fp);
                free(table[i]);
            }
        }
        else
        {
            if (line_count <= 0)
                *status = EMPTY_FILE;
            else
                *status = TOO_MANY_LINES;
        }
        fclose(fp);
    }

    free(fnam);
}

/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_qsort(v, left, right)
    /* Description   :- p 110 K & R 2nd Edition
                         *
                         * Return status :-
                         *                 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *v[]; /* <i> table of strings to sort */
INT_4 left;    /* <i> left most position       */
INT_4 right;   /* <i> right most position      */
{
    INT_4 i, last;

    if (left >= right) /* less than two elements so abort */
        return;

    tools_qswap(v, left, (left + right) / 2);
    last = left;

    for (i = left + 1; i <= right; i++)
        if (strcmp(v[i], v[left]) < 0)
            tools_qswap(v, ++last, i);

    tools_qswap(v, left, last);
    tools_qsort(v, left, last - 1);
    tools_qsort(v, last + 1, right);
}
/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_qswap(v, i, j)
    /* Description   :- p 110 K & R 2nd Edition
                         *                  interchange v[i] & v[j]
                         * Return status :-
                         *                 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *v[]; /* <i> table of strings to interchange */
INT_4 i;       /* <i> 1st element      */
INT_4 j;       /* <i> 2nd element      */
{
    char *temp;

    temp = v[i];
    v[i] = v[j];
    v[j] = temp;
}

/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_get_directory(path, path_len, match, match_len,
                        filename, flnm_len, Typed, match_no, status)
    /* Description   :- This function scans a directory and optionally
                         *                  it's subdirectories for any files that fufill
                         *                  the regular expression match.
                         *                  if tree is TRUE
                         *                  will be searched. otherwise just the
                         *                  directory.
                         *                  filename will contain an output list of 
                         *                  of matching filenames.
                         *                  math_no will contain the number of matching
                         *                  filenames.
                         *                  status 0 if ok.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *path;   /* <i> Search  directory */
INT_4 *path_len;  /* <i> length of path    */
char *match;      /* <i> Regular expression to be expanded */
INT_4 *match_len; /* <i> length of regular expression to be matched */
char *filename;   /* <i> Regular expression to be expanded */
INT_4 *flnm_len;  /* <i> length of regular expression to be matched */
INT_4 *Typed;     /* <i> Typing of files required */
INT_4 *match_no;  /* <o> No of files  to be matched */
INT_4 *status;    /* <i> length of regular expression to be matched */

{

    char t_path[PATHSIZE],
        *t_match,
        *t_filename;

    INT_4 typed;
    INT_4 startpos; /* starting postion values */
    FILE *fp;

    void tools_makpat(), tools_search_this_directory();

    *status = 0; /* set status ok */

    typed = *Typed;

    *match_no = 0; /* initialise no of tools_matchs */

    /* right lets copy these  FORTRAN strings */

    strncpy(t_path, path, *path_len);

    t_match = (char *)malloc(*match_len + 1);
    strncpy(t_match, match, *match_len);

    t_filename = (char *)malloc(*flnm_len + 1);
    strncpy(t_filename, filename, *flnm_len);

    /* null terminate them */

    t_path[*path_len] = '\0';
    t_filename[*flnm_len] = '\0';
    t_match[*match_len] = '\0';

    if (!(fp = fopen(t_filename, "w")))
    {
        *status = OUTPUT_FILE_ERROR;
    }
    else
    { /* output file opened ok */

        rewind(fp); /* back to the start of the file */

        tools_makpat(t_match, match_len); /* make a pattern template */

        tools_search_this_directory(t_path, typed, fp, match_no, status);
    }

    /* free up any memory used */

    if (*status == 0)
        tools_sort(filename, flnm_len, status);

    free(t_match);
    free(t_filename);

    fclose(fp);

    return;
}

/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_search_this_directory(path, typed, fp, count, status)
    /* Description   :- This function scans a directory for any files
                         *                  that match a preset regular expression that 
                         *                  has been set with makpat() in a calling routine 
                         *                  The matched filenames are output to the stream
                         *                  fp.
                         *                  If tree is true then subdirectories are
                         *                  searched.
                         *                  count records the number of filenames found
                         *                  status is set if any problems are hit
                         * Return status :- 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *path; /* <i> The search directory */
INT_4 typed;    /* <i> Flag to enable typing to occur */
FILE *fp;       /* <i> output file stream */
INT_4 *count;   /* <o> number of files matched */
INT_4 *status;  /* <o> status code             */

{

    char buf[PATHSIZE];
    DIR *dirp;
    INT_4 index, end, length, buf_len, ent_len;
    INT_2 dir_len, max_count = 256, read_count;
    char entry_type[10];

#ifdef BSD
    struct direct *dp;
#else
    struct dirent *dp;
#endif
    struct stat st;
    char dot[2];

    if (!(dirp = opendir(path)))
    {
        *status = DIRECTORY_OPEN_ERROR;
        fputs("Invalid Directory\n", fp);
        daxseterror();
        ++*count;
    }
    else
    {
        for (dp = readdir(dirp); (dp != NULL); dp = readdir(dirp))
        {

            length = strlen(dp->d_name);
            end = 0;
            tools_match(dp->d_name, &length, &end, &index);
            if (index > 0)
            {
                fprintf(fp, "%s/%s\n", path, dp->d_name);
                ++*count;
            }
        }
        closedir(dirp); /* Helps if we shut it */
    }
}

void
    GET_DIRS_WILD(path, path_len, dirs, match, match_len,
                  filename, flnm_len, match_no, status)
    /* Description   :- This routine is similar to that 
                         *                  of the above. It gets the 
                         *                  match files but also puts
                         *                  in any directories and links
                         *                  to enable selection to the 
                         *                  above direcories or belows
                         *                  
                         *                  
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *path;   /* <i> Search  directory */
INT_4 *path_len;  /* <i> length of path    */
INT_4 *dirs;      /* <i> Directoies only */
char *match;      /* <i> Regular expression to be expanded */
INT_4 *match_len; /* <i> length of regular expression to be matched */
char *filename;   /* <i> Regular expression to be expanded */
INT_4 *flnm_len;  /* <i> length of regular expression to be matched */
INT_4 *match_no;  /* <o> No of files  to be matched */
INT_4 *status;    /* <i> length of regular expression to be matched */

{

    int len;
    char t_path[PATHSIZE],
        *t_match,
        *t_filename;

    char buff[PATHSIZE];
    INT_4 typed;
    INT_4 startpos; /* starting postion values */
    int size;
    FILE *fp;

    void tools_makpat(), tools_search_this_directory();

    *status = 0; /* set status ok */

    *match_no = 0; /* initialise no of tools_matchs */

    /* right lets copy these  FORTRAN strings */

    len = *path_len;

    if (len == 0)
    {
        len = 2;
        strcpy(t_path, "./");
    }
    else
        strncpy(t_path, path, len);

    t_match = (char *)malloc(*match_len + 1);
    strncpy(t_match, match, *match_len);

    t_filename = (char *)malloc(*flnm_len + 1);
    strncpy(t_filename, filename, *flnm_len);

    /* null terminate them */

    t_path[len] = '\0';
    t_filename[*flnm_len] = '\0';
    t_match[*match_len] = '\0';

    if (!(fp = fopen(t_filename, "w")))
    {
        *status = OUTPUT_FILE_ERROR;
    }
    else
    { /* output file opened ok */

        rewind(fp); /* back to the start of the file */

        tools_makpat(t_match, match_len); /* make a pattern template */

        tools_search_wild(t_path, *dirs, fp, match_no, status);
    }

    /* free up any memory used */

    if (*status == 0)
    {

        tools_sort(filename, flnm_len, status);
    }
    free(t_match);
    free(t_filename);

    fclose(fp);

    return;
}

/*
 *===================================================================================*
 *
 *                                                            Date   :: 3 May 1989
 *
 */
void
    tools_search_wild(path, dirs, fp, count, status)
    /* Description   :- This function scans a directory for any files
                         *                  that match a preset regular expression that 
                         *                  has been set with makpat() in a calling routine 
                         *                  The matched filenames are output to the stream
                         *                  fp.
                         *                  If tree is true then subdirectories are
                         *                  searched.
                         *                  count records the number of filenames found
                         *                  status is set if any problems are hit
                         * Return status :- 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *path; /* <i> The search directory */
INT_4 dirs;     /* <i> Only search for directories */
FILE *fp;       /* <i> output file stream */
INT_4 *count;   /* <o> number of files matched */
INT_4 *status;  /* <o> status code             */

{

    char buf[PATHSIZE];
    DIR *dirp;
    INT_4 index, end, length, buf_len, ent_len;
    INT_4 size;
    INT_2 dir_len, max_count = 256, read_count;
    char entry_type[10];
    char newbuf[PATHSIZE];
    int cc;
    int len;

#ifdef BSD
    struct direct *dp;
#else
    struct dirent *dp;
#endif

    int ok;
    struct stat st;

    if (!(dirp = opendir(path)))
    {
        *status = DIRECTORY_OPEN_ERROR;
        fputs("Invalid Directory\n", fp);
        ++*count;
    }
    else
    {

        for (dp = readdir(dirp); (dp != NULL); dp = readdir(dirp))
        {

            sprintf(buf, "%s/%s", path, dp->d_name);

            index = 0;
            ok = 0;
            if (dirs)
            {
                if (!stat(buf, &st))
                {
                    if (CHECK_BITS(st.st_mode, S_IFDIR))
                        ok = 1;
                }
            }
            else
            {
                length = strlen(dp->d_name);
                end = 0;
                tools_match(dp->d_name, &length, &end, &index);
                ok = index > 0;
            }

            if (ok)
            {
                if (strncmp(dp->d_name, ".", 1))
                {
                    fprintf(fp, "%s\n", dp->d_name);
                    ++*count;
                }
            }
        }
        closedir(dirp); /* Helps if we shut it */
    }
}

VALDIRC(Path, Length, st)

/* Description   :- Checks the validity of the path sent
       * 
       * 
       *
       * Return status :- 1        ->      Directory cannot be found 
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

char *Path;  /*  <i>  The directory path */
int *Length; /*  <i>  length */
int *st;

{

    char *p;
    int ret;
    struct stat buff; /* stats buffer */

    *st = 0;

    p = (char *)strdupn(Path, *Length);

    ret = stat(p, &buff);

    *st = !CHECK_BITS(buff.st_mode, S_IFDIR);
    daxseterror();

    free(p);
}

tools_search_this_directory_raw(path, fp, count, status)

    /* Description   :- This function scans a directory for any files
                         *                  that match a preset regular expression that 
                         *                  has been set with makpat() in a calling routine 
                         *                  The matched filenames are output to the stream
                         *                  fp.
                         *                  If tree is true then subdirectories are
                         *                  searched.
                         *                  count records the number of filenames found
                         *                  status is set if any problems are hit
                         * Return status :- 
                         *   
                         * Externals     :-
                         *
                         * Notes         :-
                         *
                         */
    char *path; /* <i> The search directory */
FILE *fp;       /* <i> output file stream */
INT_4 *count;   /* <o> number of files matched */
INT_4 *status;  /* <o> status code             */

{
#define LINK 1
#define DIRECTORY 2
    char buf[PATHSIZE];
    char access_txt[11];
    DIR *dirp;
    INT_4 index, end, length, buf_len, ent_len;
    INT_2 dir_len, read_count;

#ifdef BSD
    struct direct *dp;
#else
    struct dirent *dp;
#endif

    struct stat st;
    int aread;
    int awrite;
    int aexec;
    int acnt;

    *status = 0;
    if (!(dirp = opendir(path)))
    {
        *status = DIRECTORY_OPEN_ERROR;
        puts("Invalid Directory\n");
    }
    else
    {
        for (dp = readdir(dirp); (dp != NULL); dp = readdir(dirp))
        {
            fputs(dp->d_name, fp);
            fputs("\n", fp);
        }
        closedir(dirp); /* Helps if we shut it */
    }
}

EXISTPATH(Path, Length, st) /* checks existance of the file link or dir */

/* Description   :- Checks the existance of a path for validity 
       *                  Can be aither file or directory.
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

char *Path;  /*  <i>     Path being checked */
int *Length; /*  <i>     Length */
int *st;

{

    struct stat buff;
    char *p;

    *st = 0;

    p = (char *)strdupn(Path, *Length);

    *st = stat(p, &buff);

    daxseterror();

    free(p);
}

TOOLSGETCWD(Path, Size, Status)

/* Description   :- Gets a current working directory.
       * 
       * 
       *
       * Return status :- 1   ->  Invalid Pathname.
       *                  
       *   
       *                  
       * Notes         :- Used for F77 mainly.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

char *Path;  /*  <o> The path that will be set */
int *Size;   /*  <o> The length of the path */
int *Status; /*  <o> Status return */

{

    int ret;

    *Status = 0;

    ret = getcwd(Path, TOOLSPATH + 2);
    if (!ret)
        *Status = 1;
    else
        *Size = strlen(Path);
}

TOOLSSETCWD(Path, Size, Status)

/* Description   :- Sets a working directory.
       * 
       * 
       *
       * Return status :- 1   ->  Invalid Directory Not Set.
       *                  
       *   
       *                  
       * Notes         :- Used for F77 mainly.
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

char *Path;  /*  <i> The path that will be set */
int *Size;   /*  <i> The length of the path */
int *Status; /*  <o> Status return */

{

    char localpath[TOOLSPATH];

    *Status = 0;
    strncpy(localpath, Path, *Size);

    localpath[*Size] = '\0';

    if (chdir(localpath))
        *Status = 1;
}
