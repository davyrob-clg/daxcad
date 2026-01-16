/* SCCS id Keywords             @(#)  412.1 date 6/11/92 local.h   */

/*

    Include file full of manifest constants 
    For general use

*/

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef MAX
#define MAX(a,b) ((a) <  (b)) ? (b) : (a)
#endif

#ifndef MIN
#define MIN(a,b) ((a) <  (b)) ? (a) : (b)
#endif

typedef int INT4;
typedef short DAXINT;	     	/* current daxcad integer defintion */
typedef float DAXFLOAT;		/* current daxcad floating  defintion */
