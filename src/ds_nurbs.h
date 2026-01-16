/*      @(#)  412.1 date 6/11/92 ds_nurbs.h 
 *
 *
 *        Filename    : ds_nurbs.h
 *        Version     : 412.1
 *        Retrieved   : 92/06/11 14:29:03
 *        Last change : 92/06/11 14:29:01
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurbs.h
 *
 *        Last change : 89/12/16 20:50:47
 *
 *        Copyright : Practical Technology Limited  
 *
 *        Solid Modeller Procedural Interface file                
 *        Copyright : Practical Technology Limited  
 *
 *        This file contains the variable declarations and external
 *        routine declarations that are useable from other files. 
 *
-*/

#include  <math.h>
#include  <stdio.h>

#define     FORTRAN_ERROR -1              /* Signal to fortran an error in C */
#define     FORTRAN_OKAY   0              /* Signal to fortran no errors from C */

#define     void          int
#define     TRUE          1               /* The .true. condition           */
#define     FALSE         0               /* The .false. condition          */
                     
#define     TAGLESS       -1              /* The tagless condition of a tag */

#define     YES           TRUE            /* The 'yes' answer               */
#define     NO            FALSE           /* The 'no'  answer               */

#define     CART          3               /* Cartesian model space          */
#define     HOMO          4               /* Homogeneous model space        */

#define     OPEN           FALSE          /* Spline or surface is OPEN */
#define     CLOSED         TRUE           /* Spline or surface is CLOSED */

#define     MAX_INT       2147483647      /* Maximum integer magnitude      */   
#define     MAX_FLOAT     1.7014e38       /* Maximum double magnitude       */
#define     MAX_EXPONENT  2147480000      /* Maximum exponent magnitude     */
#define     MAX_NORM_MANT 1.7041          /* Maximum normalised mantissa    */
#define     MAX_NORM_EXP  38              /* Maximum normalised exponent    */
#define     MIN_FLOAT     1.0e-10         /* Near zero                      */

#define     TOL           1.0e-5          /* A small tolerance value        */

#define     REG           register        /* Register variable              */
#define     STATUS        int             /* Status type variable           */
#define     BOOLEAN       STATUS          /* Boolean type variable          */
#define     SIDE          BOOLEAN         /* Side of entity                 */
#define     END           BOOLEAN         /* End of entity                  */
#define     FLAG          BOOLEAN         /* Flag type variable             */
#define     BITS          unsigned int  /* Bits type variable             */
#define     TAG           int             /* Tag type variable              */
#define     MEM           char *          /* Dynamic memory pointer         */

#define     EOS           '\0'            /* End of string character        */
#define     TAB           '\011'          /* Tab control character          */

#define     SUCCESS       TRUE            /* A successful state             */
#define     FAILED        FALSE           /* An unsuccessful state          */

#define     STR_SIZE      255             /* Maximum string length          */

#define     STR_DEC       STR_SIZE+1      /* ... and its declaration        */

/*
 *
 *    Constants
 *    ~~~~~~~~~
 *
 *
 */
#define     C$PIBY6         5.2359877559829833e-01
#define     C$PIBY4         7.8539816339744750e-01
#define     C$PIBY2         1.5707963267948950e+00
#define     C$PI            3.1415926535897900e+00
#define     C$3PIBY2        4.7123889803846850e+00
#define     C$2PI           6.2831853071795800e+00

#define     MM_TO_INCH    3.9370078740157481e-02
#define     MM_TO_FEET    3.2808398950131234e-03
#define     MM_TO_YARD    1.0936132983377078e-03
#define     MM_TO_MILE    6.2137119223733398e-07

#define     IN_TO_MM      2.5400000000000000e+01
#define     FEET_TO_MM    3.0480000000000000e+02
#define     YARD_TO_MM    9.1440000000000000e+02

#define     MILE_TO_MM    1.6093440000000000e+06

#define     R_TO_D        5.7295779513082380e+01
#define     R_TO_G        6.3661977236758200e+01
#define     D_TO_R        1.7453292519943278e-02
#define     G_TO_R        1.5707962367948950e-02

/*
 *
 *    Solid modeller MACROS
 * 
 *
 */ 
#define     ABS(A)                          ((A) >= 0.0 ? (A) : (-A))
#define     TOL_FAILURE(A)                  (ABS(A) < TOL ? TRUE : FALSE)                      
#define     TOL_WITHIN(A,T)                 (ABS(A) < T ? TRUE : FALSE)  

#define     MAX( A, B, OUT )                (OUT = ((A) > (B)) ? (A) : (B))
#define     MIN( A, B, OUT )                (OUT = ((A) < (B)) ? (A) : (B))

#define     POINTH_TO_3D(IN,OUT)            if (IN[H] != 1.0)\
                                            {\
                                               OUT[X] = IN[X]/IN[H];\
                                               OUT[Y] = IN[Y]/IN[H];\
                                               OUT[Z] = IN[Z]/IN[H];\
                                               OUT[H] = 1.0;\
                                            }

#define     POINT3D_TO_H(IN,WEIGHT,OUT)     OUT[X] = IN[X] * (WEIGHT);\
                                            OUT[Y] = IN[Y] * (WEIGHT);\
                                            OUT[Z] = IN[Z] * (WEIGHT);\
                                            OUT[H] = (WEIGHT)

#define     CHR_TOUPPER(C)                      ((C)-'a'+'A')
#define     CHR_TOLOWER(C)                      ((C)-'A'+'a')
#define     CHR_ISLOWER(C)                      (((C) >= 'a') && ((C) <= 'z'))
#define     CHR_ISUPPER(C)                      (((C) >= 'A') && ((C) <= 'Z'))
#define     CHR_ISDIGIT(C)                      (((C) >= '0') && ((C) <= '9'))
#define     CHR_ISSPACE(C)                      ((C) == ' ')
#define     CHR_ISTAB(C)                        ((C) == '\t')

#define  USER_KNOTS     FALSE      /* Use user supplied knots */
#define  DEF_KNOTS      TRUE       /* Generate default knots */
   
#define  STR_LIN_ORD    2          /* Default order of a straight line */
#define  STR_LIN_PNTS   2          /* Number of points to define a straight line */
                              
#define  DEF_ARC_ANGLE  C$PIBY2    /* The default arc angle, ie The number of arcs that */
                                   /* make up a full circle */
#define  DEF_SPLINE_ORDER 3        /* Default spline order */

                         
#define     PUT(ptr)                (ds_free((char *)(ptr)))
/*
 *
 *  Node memory allocation
 *  ~~~~ ~~~~~~ ~~~~~~~~~~
 *
 */
#define     GET_TRANSFORM        ((DS_TRANSFORM_PTR)ds_alloc(sizeof(struct trans)))
#define     GET_KNOT( num )      ((double *)ds_alloc(sizeof(double) * (num)))
#define     GET_ARRAY(row, col ) ((double *)ds_alloc(sizeof(double) * (row) * (col)))
#define     GET_POINT( num )     ((PTR_POINT)ds_alloc(sizeof(POINT) * (num)))
#define     GET_MESH( row, col ) ((PTR_POINT)ds_alloc(sizeof(POINT) * (row) * (col)))
#define     GET_NURBSPLINE()     ((DS_NURB_SPLINE_PTR)ds_alloc(sizeof(struct nurb_spline)))
#define     GET_NURBSURFACE()    ((DS_NURB_SURFACE_PTR)ds_alloc(sizeof(struct nurb_surface)))
#define     GET_INT( num )       ((INT *)ds_alloc(sizeof(INT) * (num) ))

/*
 *
 *    Solid modeller globals
 *
 *
 *
 */
#define     X             0               /* Co-ordinate indices */
#define     Y             1
#define     Z             2
#define     H             3   
                            
typedef     int           INT;                  /* Portable integer variable         */

typedef     double        POINT_2D[2];          /* Two dimensional co-ordinate       */
typedef     double        VECTOR_2D[2];         /* Two dimensional vector            */
typedef     double        (*PTR_POINT_2D)[2];   /* Pointer to a co-ordinate          */
typedef     double        (*PTR_VECTOR_2D)[2];  /* Pointer to a vector               */

typedef     double        POINT[4];             /* Homogeneous co-ordinate              */
typedef     double        VECTOR[4];            /* Homogeneous vector                   */
typedef     double        (*PTR_POINT)[4];      /* Pointer to a homogeneous co-ordinate */
typedef     double        (*PTR_VECTOR)[4];     /* Pointer to a homogeneous vector      */

typedef     double        MATRIX_3BY3[3][3];    /* 3 by 3 Matrices               */
typedef     double        MATRIX_4BY4[4][4];    /* 4 by 4 Matrices               */

typedef     double        BOX[6];               /* Bounding box co-ordinates     */
typedef     double        (*PTR_BOX)[6];        /* Pointer to a bounding box     */
 
/*
 *
 *        Structure templates
 *
 */

typedef struct nurb_spline              /* NURB SPLINE                              */
{
      INT         type;                 /* Type of geometry stored                  */
      INT         order;                /* The order of the spline                  */
      INT         n;                    /* The number of control points             */
      BOOLEAN     closed;               /* Flags closure                            */
      double      *knots;               /* Pointer to the knot vector               */
      POINT       *points;              /* Pointer to the control polygon           */
} DS_NURB_SPLINE, *DS_NURB_SPLINE_PTR;

typedef struct nurb_surface             /* NURB SURFACE                             */
{
      INT         type;                 /* Type of geometry stored                  */
      INT         order_u;              /* The order of the surface in U            */
      INT         order_v;              /* The order of the surface in V            */
      INT         m;                    /* The number of control points in U        */
      INT         n;                    /* The number of control points in V        */
      BOOLEAN     closed_u;             /* Flags closure in U                       */
      BOOLEAN     closed_v;             /* Flags closure in V                       */
      double      *knots_u;             /* Pointer to the knot vector in U          */
      double      *knots_v;             /* Pointer to the knot vector in V          */
      POINT       *points;              /* Pointer to the control polyhedron        */
} DS_NURB_SURFACE, *DS_NURB_SURFACE_PTR;

/*
 *
 *        Routine definitions
 *
 */
extern BOOLEAN   vecnrm();
extern BOOLEAN   vecclr();
extern double    veclen();
extern double    veclensq();
extern double    vecdp();
extern BOOLEAN   vecccp();
extern BOOLEAN   vecsum();
extern BOOLEAN   vecdif();
extern BOOLEAN   vecdir();
extern BOOLEAN   vecneg();
extern BOOLEAN   veccpy();
extern BOOLEAN   vecsca();
extern BOOLEAN   vecang();
extern BOOLEAN   vecswap();

DS_NURB_SURFACE_PTR     create_cylinder();
DS_NURB_SURFACE_PTR     create_cone();
DS_NURB_SURFACE_PTR     create_plane();
DS_NURB_SURFACE_PTR     create_sphere();
DS_NURB_SURFACE_PTR     create_torus();
DS_NURB_SURFACE_PTR     create_disp_spline();  
DS_NURB_SURFACE_PTR     create_spin_spline();
DS_NURB_SURFACE_PTR     copy_nurbsurface();
DS_NURB_SURFACE_PTR     create_nurbsurface(); 
DS_NURB_SPLINE_PTR      create_nurbspline();
DS_NURB_SPLINE_PTR      create_nurb_arc();        
DS_NURB_SPLINE_PTR      create_nurb_circle(); 
DS_NURB_SPLINE_PTR      create_nurb_line();
DS_NURB_SPLINE_PTR      copy_nurbspline();
DS_NURB_SPLINE_PTR     create_ellipse();

extern double os_mid_knt();
