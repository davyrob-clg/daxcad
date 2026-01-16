
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 nurbs.c   */



/*

           GPRX daxcad conversion Notes 


           2 routines have been altered to take out the cont qualifier


           spline_span_length

           cofg_of_span

*/


#include <stdint.h>
#include "xlang.h"

/*+
 *
 *
 *        Filename    : axes.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:09
 *        Last change : 89/12/16 19:20:29
 *
 *        Copyright : Practical Technology Limited  
 *        File :- axes.c
 *
 *        This file contains the following routines:
 *
 *              axperp
 *              axperpl
 *              axperpsql
 *
 *
 *
 *
 *
 *
*/
char *
ds_alloc();
#include "ds_nurbs.h"
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
axperp2D( pnt, pnt_on_axis, axis_dir, outvec, outpoint )
                        /* This routine calculates a perpendicular vector from the
                         * given axis to the given point that is offset from the axis.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double pnt[];           /* <i> The point, offset from the axis */
double pnt_on_axis[];   /* <i> Point on the axis */
double axis_dir[];      /* <i> The direction of the axis */
double outvec[];        /* <o> The vector from the axis to the point */
double outpoint[];      /* <o> The point of intersection on the axis */
{
      double  norm[3];
      double  u2[3];
      double  disp[3];
      double  point[3];
      double  point_on_axis[3];
      double  dir[3];
                 
      point[2] = 0.0;
      point_on_axis[2] = 0.0;
      dir[2] = 0.0;
      veccpy(pnt,2,point);
      veccpy(pnt_on_axis,2,point_on_axis);  
      veccpy(axis_dir,2,dir);
                                                   
      vecdif( point, point_on_axis, 3, u2 );
      vecnrm( dir, 3, TOL ,norm );
      vecsca( vecdp( u2, norm, 3), norm, 3, disp );
      vecsum( point_on_axis, disp, 3, outpoint );
      vecdif( u2, disp, 3, outvec );

      return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
axperp( point, point_on_axis, dir, outvec, outpoint )
                        /* This routine calculates a perpendicular vector from the
                         * given axis to the given point that is offset from the axis.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double point[];         /* <i> The point, offset from the axis */
double point_on_axis[];    /* <i> Point on the axis */
double dir[];           /* <i> The direction of the axis */
double outvec[];        /* <o> The vector from the axis to the point */
double outpoint[];      /* <o> The point of intersection on the axis */
{
      double  norm[3];
      double  u2[3];
      double  disp[3];
                                                   
      vecdif( point, point_on_axis, 3, u2 );
      vecnrm( dir, 3, TOL ,norm );
      vecsca( vecdp( u2, norm, 3), norm, 3, disp );
      vecsum( point_on_axis, disp, 3, outpoint );
      vecdif( u2, disp, 3, outvec );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 Nov 1988
 *
 */
BOOLEAN
axperpl2D( point2, point_axis2, dir2, displacement )
                        /* This routine calculates the length of the perpendicular vector
                         * from the axis to the given point.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double point2[];         /* <i> The point, offset from the axis */
double point_axis2[];    /* <i> Point on the axis */
double dir2[];           /* <i> The direction of the axis */
double *displacement;   /* <o> The resultant distance */
{                                                                                  
    double  point[3];
    double  point_axis[3];
    double  dir[3];
    
    
    veccpy(point2,2,point);
    point[2] = 0.0;

    veccpy(point_axis2,2,point_axis);
    point_axis[2] = 0.0;

    veccpy(dir2,2,dir);
    dir[2] = 0.0;

      if( !axperpsql( point, point_axis, dir, displacement ))
      {
            return( FALSE );
      }
      *displacement = sqrt( *displacement );

      return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
axperpl( point, point_axis, dir, displacement )
                        /* This routine calculates the length of the perpendicular vector
                         * from the axis to the given point.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double point[];         /* <i> The point, offset from the axis */
double point_axis[];    /* <i> Point on the axis */
double dir[];           /* <i> The direction of the axis */
double *displacement;   /* <o> The resultant distance */
{
      if( !axperpsql( point, point_axis, dir, displacement ))
      {
            return( FALSE );
      }
      *displacement = sqrt( *displacement );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
axperpsql( point, point_axis, dir, displacement )
                        /* This routine calculates the square of the length of the 
                         * perpendicular vector from the axis to the given point.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double point[];         /* <i> The point, offset from the axis */
double point_axis[];    /* <i> Point on the axis */
double dir[];           /* <i> The direction of the axis, assumed non-zero */
double *displacement;   /* <o> The resultant squared distance */
{
      double  denom;
      double  x[3];
      double  f[3];
      double  v[3];

      denom = vecdp( dir, dir, 3 );
      vecdif( point, point_axis, 3, x );

      f[0] = dir[0] * x[1] - dir[1] * x[0];
      f[1] = dir[0] * x[2] - dir[2] * x[0];
      f[2] = dir[1] * x[2] - dir[2] * x[1];

      v[0]  =  dir[1] * f[0]  - dir[2] * f[1];
      v[1]  =  dir[2] * f[2]  - dir[0] * f[0];
      v[2]  = -( dir[0] * f[1]) - dir[1] * f[2];

      *displacement = (vecdp( v, v, 3 ))/( denom * denom );

      return( TRUE );
}
/*      @(#)  256.1 date 12/16/89 bsplines.c  */
/*+
 *
 *        Filename    : bsplines.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:11
 *        Last change : 89/12/16 19:20:39
 *
 *        Copyright : Practical Technology Limited  
 *        File :- bsplines.c
 *
 *        This file contains the following routines:
 *
 *        bsp_point_on_spline
 *        bsp_point_on_surface
 *        bsp_basis
 *        bsp_find_u
 *        bsp_find_v
 *        bsp_get_mu_u
 *        bsp_get_mu_v
 *        bsp_set_mu_u
 *        bsp_set_mu_v
 *        bsp_deriv_surf_u
 *        bsp_deriv_surf_v
 *        bsp_deriv_surf_uv
 *        bsp_derivatives
 *        bsp_span_derivatives
 *
 *
 *
 *
 *
 *
-*/                                                              
  

static      INT         so$mu_u;            /* Current mu in U */
static      INT         so$mu_v;            /* Current mu in V */
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_point_on_spline( spline, u, pnt, fdt, fddt)
                        /* Description   :- This function calculates:
                         *
                         *                  1. A point on the given spline for a given u.
                         *                  2. The tangent to the spline at that point.
                         *                  3. The second derivative at that point.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- bsp_set_mu_u must be called before marching
                         *                  along the spline.               
                         *
                         *                  See Faux and Pratt pg 142 for explanation
                         *                  of Cartesian calculation.
                         *
                         */
DS_NURB_SPLINE_PTR spline;  /* <i> Pointer to the spline */
double             u;       /* <i> The parameter */
POINT            pnt;     /* <o> Point on the spline */
POINT            fdt;     /* <o> First derivative */
POINT            fddt;    /* <o> Second derivative */
{
    INT         mu;
    INT         num_derivatives;
    PTR_POINT   results;
    POINT       temp1;
    POINT       temp2;
    POINT       temp3;

    if( u < 0.0 || u > 1.0 )
    {                                   /* Ouside valid parameter range */
        return(FALSE);
    }
    switch (spline->order) {
    
    case 2:
        num_derivatives = 1;     
        break;
    default:
        num_derivatives = 2;
        break;
    }

    if (!(results = GET_POINT(spline->order)))
        return(FALSE);
    mu = bsp_find_u( u, spline->n, spline->knots );
    bsp_derivatives( spline->points, spline->knots, u, mu, spline->order, num_derivatives, results );    /* Calculate all the derivatives */

    pntextract(results,0,pnt);  /* Point on spline in 4D     */
    pntextract(results,1,fdt);  /* 1st deriv, tangent  in 4D */
    pntextract(results,2,fddt); /* 2nd Deriv in 4D           */

    vecsca( fdt[H]/pnt[H], pnt, HOMO, temp1 );  /* Convert 1st derivative to Cartesian space */
    vecdif( fdt, temp1, HOMO, temp1 );
    vecsca( 1.0/pnt[H], temp1, HOMO, temp1 );

    vecsca( 2.0*fdt[H], temp1, HOMO, temp2 );   /* Convert 2nd derivative to Cartesian space */
    vecsca( fddt[H]/pnt[H], pnt, HOMO, temp3 );
    vecdif( fddt, temp3, HOMO, temp3 );
    vecdif( temp3, temp2, HOMO, temp3 );
    vecsca( 1.0/pnt[H], temp3, HOMO, fddt );

    veccpy( temp1, HOMO, fdt );

    POINTH_TO_3D(pnt,pnt);     /* Convert point on spline from homogeneous to 3d */
    
    if( spline->order == 2 )
    {
        vecclr( fddt, 4 );     /* Ensure no second derivative */
    }

    PUT(results);

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_point_on_surface( surface, u, v, pnt, fdtu, fdtv, fddtu, fddtv)
                        /* Description   :- This function calculates:
                         *
                         *                  1. A point on the given surface.
                         *                  2. The tangent to the surface in U.
                         *                  2. The tangent to the surface in V.
                         *                  3. The second derivative in U.
                         *                  3. The second derivative in V.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- bsp_set_mu_u and bsp_set_mu_v must be 
                         *                  called before each march across the surface
                         *
                         */
DS_NURB_SURFACE_PTR surface;/* <i> Pointer to the surface */
double              u;      /* <i> The parameter in U */
double              v;      /* <i> The parameter in V */
POINT             pnt;    /* <o> Point on surface */
POINT             fdtu;   /* <o> Tangent to surface in U */
POINT             fdtv;   /* <o> Tangent to surface in V */
POINT             fddtu;  /* <o> 2nd derivative in U     */
POINT             fddtv;  /* <o> 2nd derivative in V     */
{
    PTR_POINT     temp1;
    PTR_POINT     temp2;
    POINT         vec1;
    POINT         vec2;
    POINT         vec3;

    if( u < 0.0 || u > 1.0 )
    {                                   /* Ouside valid parameter range */
        return(FALSE);
    }
    if( v < 0.0 || v > 1.0 )
    {                                   /* Ouside valid parameter range */
        return(FALSE);
    }

    temp1 = GET_POINT( 3 );                      /* Allocate working memory */
    temp2 = GET_POINT( 3 );                      /* Allocate working memory */

    bsp_deriv_surf_u( surface, u, v, temp1 );    /* Calculate the derivatives in U */
    bsp_deriv_surf_v( surface, u, v, temp2 );    /* Calculate the derivatives in V */

    pntextract( temp1, 0, pnt );            /* The surface point in 4D          */
    pntextract( temp1, 1, fdtu );           /* The first derivative in U in 4D  */
    pntextract( temp2, 1, fdtv );           /* The first derivative in V in 4D  */
    pntextract( temp1, 2, fddtu );          /* The second derivative in U in 4D */
    pntextract( temp2, 2, fddtv );          /* The second derivative in V in 4D */
          
    vecsca( fdtu[H]/pnt[H], pnt, HOMO, vec1 ); /* Convert 1st derivative in U to Cartesian space */
    vecdif( fdtu, vec1, HOMO, vec1 );
    vecsca( 1.0/pnt[H], vec1, HOMO, vec1 );

    vecsca( 2.0*fdtu[H], vec1, HOMO, vec2 );   /* Convert 2nd derivative in U to Cartesian space */
    vecsca( fddtu[H]/pnt[H], pnt, HOMO, vec3 );
    vecdif( fddtu, vec3, HOMO, vec3 );
    vecdif( vec3, vec2, HOMO, vec3 );
    vecsca( 1.0/pnt[H], vec3, HOMO, fddtu );

    veccpy( vec1, HOMO, fdtu );

    vecsca( fdtv[H]/pnt[H], pnt, HOMO, vec1 ); /* Convert 1st derivative in V to Cartesian space */
    vecdif( fdtv, vec1, HOMO, vec1 );
    vecsca( 1.0/pnt[H], vec1, HOMO, vec1 );

    vecsca( 2.0*fdtv[H], vec1, HOMO, vec2 );   /* Convert 2nd derivative in V to Cartesian space */
    vecsca( fddtv[H]/pnt[H], pnt, HOMO, vec3 );
    vecdif( fddtv, vec3, HOMO, vec3 );
    vecdif( vec3, vec2, HOMO, vec3 );
    vecsca( 1.0/pnt[H], vec3, HOMO, fddtv );

    veccpy( vec1, HOMO, fdtv );

    POINTH_TO_3D(pnt,pnt);         /* Convert point on surface from homogeneous to 3d */

    if( surface->order_u == 2 )
    {
        vecclr( fddtu, 4 );     /* Ensure no second derivative */
    }
    if( surface->order_v == 2 )
    {
        vecclr( fddtv, 4 );     /* Ensure no second derivative */
    }
    PUT( temp2 );                           /* De-allocate working memory */
    PUT( temp1 );                           /* De-allocate working memory */

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_basis( k, n, tau, parameter, uv_flag, basis )
                        /* Description   :- This function calculates the B-spline basis
                         *                  for a particular parametric value. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  This is the Cox-deBoor Algorithm.
                         *
                         */
INT    k;               /* <i> The order of the B-spline curve */
INT    n;               /* <i> The number of polygon vertices */
double tau[];           /* <i> The knot vector */ 
double parameter;       /* <i> The parametric value */
BOOLEAN uv_flag;        /* <i> Flags which basis to calculate  */
double basis[];         /* <o> Array in which to deposit the k basis values */
{
        double  e1;
        double  e2;
        double  e3;
        double  temp1;
        double  temp2;
        double  temp3;

        INT     i;
        INT     j;
        INT     jmu;

        if( uv_flag )
        {
            jmu = bsp_find_u( parameter, n, tau );  /* Calculate which knot element to consider */
        }
        else
        {
            jmu = bsp_find_v( parameter, n, tau );  /* Calculate which knot element to consider */
        }
        jmu++;

        temp1 = tau[jmu];
        temp2 = tau[jmu-1];

        e1 = parameter-temp2;
        e2 = temp1-parameter;
        temp3 = temp1-temp2;

        basis[0] = 0.0;
        if( temp3 > 0.0 )
        {
            basis[0] = 1.0/temp3;
        }
        for( i=2; i < k ; i++ )
        {
            temp3 = tau[jmu-1+i] - temp2;
            basis[i-1] = 0.0;
            if( temp3 > 0.0 )
            {
                basis[i-1] = e1 * basis[i-2]/temp3;
            }
        }
        basis[k-1] = e1 * basis[k-2];
        for( i = 1; i < k-1; i++ )         
        {
            e3 = parameter - tau[jmu-1-i];
            temp3 = temp1 - tau[jmu-1-i];
            if( temp3 > 0.0 )
            {
                basis[0] = e2 * basis[0]/temp3;
            }
            else
            {
                basis[0] = 0.0;
            }
            for( j = 2; j < k-i; ++j)
            {
                temp3 = tau[jmu-1+j] - tau[jmu-1-i];
                if( temp3 > 0.0 )
                {
                    basis[j-1] = (e3 * basis[j-2] + (tau[jmu-1+j]-parameter) * basis[j-1])/temp3;
                }
                else
                {
                    basis[j-1] = e3 * basis[j-2];
                }
            }
            basis[k-i-1] = e3 * basis[k-i-2] + (tau[jmu-1+k-i]-parameter) * basis[k-i-1];
        }
        basis[0] = e2 * basis[0];

        return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
INT
bsp_find_u( u, n, tau )
                        /* Description   :- This function calculates the unique mu such
                         *                  that:
                         *                 
                         *                        tau(mu) <= u < tau(mu+1)
                         *
                         * Return status :- so$mu_u
                         *   
                         * Externals     :-  so$mu_u Used/Modified
                         *               
                         * Notes         :-  
                         *
                         */
double u;               /* <i> The parametric value */
INT    n;               /* <i> The number of polygon vertices */
double tau[];           /* <i> The knot vector */ 
{
    double  diff;
    INT     i;

    for( i = so$mu_u; i < n ; i++ )
    {
        diff = u - tau[i];
        if( ( diff < 0.0 ) && !TOL_FAILURE(diff))
        {
            if( i != so$mu_u )
            {
                so$mu_u = i - 1;
            }
            return( so$mu_u );
        }
    }
    so$mu_u = n-1;
    return(so$mu_u);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
INT
bsp_find_v( v, n, tau )
                        /* Description   :- This function calculates the unique mu such
                         *                  that:
                         *                 
                         *                        tau(mu) <= v < tau(mu+1)
                         *
                         * Return status :- so$mu_v
                         *   
                         * Externals     :-  so$mu_v Used/Modified
                         *               
                         * Notes         :-  
                         *
                         */
double v;               /* <i> The parametric value */
INT    n;               /* <i> The number of polygon vertices */
double tau[];           /* <i> The knot vector */ 
{
    double  diff;
    INT     i;

    for( i = so$mu_v; i < n ; i++ )
    {
        diff = v - tau[i];
        if( ( diff < 0.0 ) && !TOL_FAILURE(diff))
        {
            if( i != so$mu_v )
            {
                so$mu_v = i - 1;
            }
            return( so$mu_v );
        }
    }
    so$mu_v = n-1;
    return(so$mu_v);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
INT
bsp_get_mu_u( )
                        /* Description   :- This function calculates the current mu in U.
                         *
                         * Return status :- so$mu_u
                         *   
                         * Externals     :-  so$mu_u Used.
                         *               
                         * Notes         :-  
                         *
                         */
{
    return(so$mu_u);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
INT
bsp_get_mu_v( )
                        /* Description   :- This function calculates the current mu in V.
                         *
                         * Return status :- so$mu_v
                         *   
                         * Externals     :-  so$mu_v Used.
                         *               
                         * Notes         :-  
                         *
                         */
{
    return(so$mu_v);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_set_mu_u( mu )
                        /* Description   :- This function sets the current mu in U.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  so$mu_u Used.
                         *               
                         * Notes         :- This routine must be called before using
                         *                  bsp_find_u. 
                         *
                         */
INT     mu;             /* <i> The new mu */
{
    so$mu_u = mu;
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_set_mu_v( mu )
                        /* Description   :- This function sets the current mu in V.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  so$mu_u Used.
                         *               
                         * Notes         :- This routine must be called before using
                         *                  bsp_find_v. 
                         *
                         */
INT     mu;             /* <i> The new mu */
{
    so$mu_v = mu;
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_deriv_surf_u( surface, u, v, results )
                        /* Description   :- This function calculates:
                         *
                         *                  1. A point on the given surface.
                         *                  2. The tangent to the surface in U.
                         *                  3. The second derivative in U.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- 
                         *
                         */
DS_NURB_SURFACE_PTR surface;/* <i> Pointer to the surface */
double u;               /* <i> The parameter in U */
double v;               /* <i> The parameter in V */
PTR_POINT results;      /* <o> The point, first and second derivatives in U */
{
    POINT     pnt1;
    POINT     pnt2;

    PTR_POINT pg;
    PTR_POINT q;

    double      *basis;

    INT         mu;
    INT         mv;
    INT         i;
    INT         j;
    INT         k;
    INT         num_derivatives;

    switch (surface->order_u ) {
    
    case 2:
        num_derivatives = 1;     
        break;
    default:
        num_derivatives = 2;
        break;
    }

    basis = GET_KNOT( surface->order_v );      /* Allocate working memory */
    pg    = GET_POINT( surface->m );           /* Allocate working memory */
    q     = GET_POINT( surface->order_u );     /* Allocate working memory */
    
    mu = bsp_find_u( u, surface->m, surface->knots_u );
    mv = bsp_find_v( v, surface->n, surface->knots_v );

    bsp_basis(surface->order_v, surface->n, surface->knots_v, v, FALSE, basis );

    pntclear( results, 3 );

    j = mv - (surface->order_v - 1);
    for( i = 0; i < surface->order_v; i++, j++ )
    {
        mshcolpnt(surface->points, j, surface->m, surface->n, pg );
        bsp_derivatives( pg, surface->knots_u, u, mu, surface->order_u, num_derivatives, q );
                 
        for( k = 0; k < 3; k++ )
        {
            pntextract( q, k, pnt1);
            vecsca( basis[i], pnt1, HOMO, pnt1);
            pntextract( results, k, pnt2 );
            vecsum( pnt2 , pnt1, HOMO, pnt2 );
            pntdeposit( results, k, pnt2 );
        }
    }
    PUT( q );                                /* De=allocate working memory */
    PUT( pg );                               /* De=allocate working memory */
    PUT( basis );                            /* De=allocate working memory */

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_deriv_surf_v( surface, u, v, results )
                        /* Description   :- This function calculates:
                         *
                         *                  1. A point on the given surface.
                         *                  2. The tangent to the surface in V.
                         *                  3. The second derivative in V.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- 
                         *
                         */
DS_NURB_SURFACE_PTR surface;/* <i> Pointer to the surface */
double u;               /* <i> The parameter in U */
double v;               /* <i> The parameter in V */
PTR_POINT results;      /* <o> The point, first and second derivatives in V */
{
    POINT     pnt1;
    POINT     pnt2;

    PTR_POINT pg;
    PTR_POINT q;

    double      *basis;

    INT         mu;
    INT         mv;
    INT         i;
    INT         j;
    INT         k;
    INT         num_derivatives;

    switch (surface->order_v ) {
    
    case 2:
        num_derivatives = 1;     
        break;
    default:
        num_derivatives = 2;
        break;
    }


    basis = GET_KNOT( surface->order_u );      /* Allocate working memory */
    pg    = GET_POINT( surface->n );           /* Allocate working memory */
    q     = GET_POINT( surface->order_v );     /* Allocate working memory */
    
    mu = bsp_find_u( u, surface->m, surface->knots_u );
    mv = bsp_find_v( v, surface->n, surface->knots_v );

    bsp_basis(surface->order_u, surface->m, surface->knots_u, u, TRUE, basis );

    pntclear( results, 3 );

    j = mu - (surface->order_u - 1);
    for( i = 0; i < surface->order_u; i++, j++ )
    {
        mshrowpnt(surface->points, j, surface->n, pg );
        bsp_derivatives( pg, surface->knots_v, v, mv, surface->order_v, num_derivatives, q );
                 
        for( k = 0; k < 3; k++ )
        {
            pntextract( q, k, pnt1);
            vecsca( basis[i], pnt1, HOMO, pnt1);
            pntextract( results, k, pnt2 );
            vecsum( pnt2 , pnt1, HOMO, pnt2 );
            pntdeposit( results, k, pnt2 );
        }
    }
    PUT( q );                                /* De=allocate working memory */
    PUT( pg );                               /* De=allocate working memory */
    PUT( basis );                            /* De=allocate working memory */

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_derivatives( polygon, tau, x, r, k, d, q )
                        /* Description   :- This function calculates all the derivatives
                         *                  of a B-spline at a given x.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- See Computing 29, pgs 365 - 371 (1982)
                         *                  "A simplified B-spline computation routine"
                         *                  by E.T.Y. Lee.
                         *                  
                         *                  Point extracts are explicit to save time
                         *                  on function calls in such a CPU intensive
                         *                  task.
                         */
PTR_POINT polygon;      /* <i> Pointer to the polygon */
double      *tau;       /* <i> Pointer to the knot vector */
double  x;              /* <i> The parameter */
INT     r;              /* <i> tau[r] <= x < tau[r+1] */
double  k;              /* <i> The order of the B-spline */
INT     d;              /* <i> The number of derivatives to be calculated */
PTR_POINT q;            /* <o> The resultant derivatives */
{
    INT       i;
    INT       j;
    INT       mu;
    INT       offset;
    double    const1;
    double    const2;
    double    const3;
    double    *a;
    double    *b;

    PTR_POINT inc1;
    PTR_POINT inc2;

    mu = k - 1;
    offset = 0;
    if( r > mu )
    {
        offset = r - k + 1;
    }
    a = GET_KNOT( k );       /* Allocate working memory */
    b = GET_KNOT( k );       /* Allocate working memory */

    for( i=1; i <= mu; i++)
    {
        a[i] = x - tau[i+offset];
        b[i] = tau[mu+i+offset]-x;
    }
    if( b[1] > a[mu] )                  /* From the right */
    {
        for( i=0; i <=mu; i++ )
        {
	    inc1 = q+i;
     	    inc2 = polygon + i + offset;
            (*(inc1))[X] = (*(inc2))[X];
            (*(inc1))[Y] = (*(inc2))[Y];
            (*(inc1))[Z] = (*(inc2))[Z];
            (*(inc1))[H] = (*(inc2))[H];
        }
        for( j=1; j <=mu; j++ )
        {
            for( i = 0; i <= mu-j; i++)
            {
                const1 = a[i+j];
                const2 = b[i+1];
                const3 = const1+const2;
		inc1 = q+i;
		inc2 = inc1 + 1;

                (*(inc1))[X] = ( const1 * (*(inc2))[X] + const2 * (*(inc1))[X])/const3;
                (*(inc1))[Y] = ( const1 * (*(inc2))[Y] + const2 * (*(inc1))[Y])/const3;
                (*(inc1))[Z] = ( const1 * (*(inc2))[Z] + const2 * (*(inc1))[Z])/const3;
                (*(inc1))[H] = ( const1 * (*(inc2))[H] + const2 * (*(inc1))[H])/const3;
            }
        }
        for( j = 1; j <= d; j++ )
        {
            for( i = d; i >= j; i-- )
            {
                const1 = b[i-j+1]/(mu-j+1);
		inc1 = q+i;
		inc2 = inc1 - 1;

                (*(inc1))[X] = ((*(inc1))[X] - (*(inc2))[X])/const1;
                (*(inc1))[Y] = ((*(inc1))[Y] - (*(inc2))[Y])/const1;
                (*(inc1))[Z] = ((*(inc1))[Z] - (*(inc2))[Z])/const1;
                (*(inc1))[H] = ((*(inc1))[H] - (*(inc2))[H])/const1;
            }
        }
    }
    else
    {                               /* From the left */
        for( i=0; i <= mu; i++ )
        {
	    inc1 = q + i;
	    inc2 = polygon + mu - i + offset;
            (*(inc1))[X] = (*(inc2))[X];
            (*(inc1))[Y] = (*(inc2))[Y];
            (*(inc1))[Z] = (*(inc2))[Z];
            (*(inc1))[H] = (*(inc2))[H];
        }
        for( j=1; j <=mu; j++ )
        {
            for( i = 0; i <= mu-j; i++)
            {
                const1 = b[mu-j+1-i];
                const2 = a[mu-i];
                const3 = const1+const2;
		inc1 = q+i;
		inc2 = inc1 + 1;

                (*(inc1))[X] = ( const1 * (*(inc2))[X] + const2 * (*(inc1))[X])/const3;
                (*(inc1))[Y] = ( const1 * (*(inc2))[Y] + const2 * (*(inc1))[Y])/const3;
                (*(inc1))[Z] = ( const1 * (*(inc2))[Z] + const2 * (*(inc1))[Z])/const3;
                (*(inc1))[H] = ( const1 * (*(inc2))[H] + const2 * (*(inc1))[H])/const3;
            }
        }
        for( j = 1; j <= d; j++ )
        {
            for( i = d; i >= j; i-- )
            {
                const1 = -1.0 * a[mu-i+j]/(mu-j+1);
		inc1 = q + i;
		inc2 = inc1 - 1;

                (*(inc1))[X] = ((*(inc1))[X] - (*(inc2))[X])/const1;
                (*(inc1))[Y] = ((*(inc1))[Y] - (*(inc2))[Y])/const1;
                (*(inc1))[Z] = ((*(inc1))[Z] - (*(inc2))[Z])/const1;
                (*(inc1))[H] = ((*(inc1))[H] - (*(inc2))[H])/const1;
            }
        }
    }
    const1 = tau[r+1] - tau[r];
    for( i=1; i <= d; i++)
    {
	inc1 = q + i;
        (*(inc1))[X] *= const1;
        (*(inc1))[Y] *= const1;
        (*(inc1))[Z] *= const1;
        (*(inc1))[H] *= const1;
        const1 *= const1;
    } 
    PUT(b);      /* De-allocate working memory */
    PUT(a);      /* De-allocate working memory */

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 9th-May-1988
 *
 */
BOOLEAN
bsp_span_derivatives( polygon, tau, x, r, d, q )
                        /* Description   :- This function calculates all the derivatives
                         *                  of a B-spline span at a given x.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise.
                         *   
                         * Externals     :-  Used/modified.
                         *               
                         * Notes         :- See Computing 29, pgs 365 - 371 (1982)
                         *                  "A simplified B-spline computation routine"
                         *                  by E.T.Y. Lee.
                         *
                         *                  Point extracts are explicit to save time
                         *                  on function calls in such a CPU intensive
                         *                  task.
                         */
PTR_POINT polygon;      /* <i> Pointer to the (mu+1) control polygon vertices */
double      *tau;       /* <i> Pointer to the 2 * (mu+1) knot elements */
double  x;              /* <i> The parameter */
INT     r;              /* <i> tau[r] <= x < tau[r+1] */
INT     d;              /* <i> The number of derivatives to be calculated */
PTR_POINT q;            /* <o> The resultant derivatives */
{
    INT       i;
    INT       j;
    double    const1;
    double    const2;
    double    const3;
    double    *a;
    double    *b;

    a = GET_KNOT( r + 1 );       /* Allocate working memory */
    b = GET_KNOT( r + 1 );       /* Allocate working memory */

    for( i=1; i <= r; i++)
    {
        a[i] = x - tau[i];
        b[i] = tau[r+i]-x;
    }
    if( b[1] > a[r] )                  /* From the right */
    {
        for( i=0; i <=r; i++ )
        {
            (*(q+i))[X] = (*(polygon+i))[X];
            (*(q+i))[Y] = (*(polygon+i))[Y];
            (*(q+i))[Z] = (*(polygon+i))[Z];
            (*(q+i))[H] = (*(polygon+i))[H];
        }
        for( j=1; j <=r; j++ )
        {
            for( i = 0; i <= r-j; i++)
            {
                const1 = a[i+j];
                const2 = b[i+1];
                const3 = const1+const2;

                (*(q+i))[X] = ( const1 * (*(q+i+1))[X] + const2 * (*(q+i))[X])/const3;
                (*(q+i))[Y] = ( const1 * (*(q+i+1))[Y] + const2 * (*(q+i))[Y])/const3;
                (*(q+i))[Z] = ( const1 * (*(q+i+1))[Z] + const2 * (*(q+i))[Z])/const3;
                (*(q+i))[H] = ( const1 * (*(q+i+1))[H] + const2 * (*(q+i))[H])/const3;
            }
        }
        for( j = 1; j <= d; j++ )
        {
            for( i = d; i >= j; i-- )
            {
                const1 = b[i-j+1]/(r-j+1);

                (*(q+i))[X] = ((*(q+i))[X] - (*(q+i-1))[X])/const1;
                (*(q+i))[Y] = ((*(q+i))[Y] - (*(q+i-1))[Y])/const1;
                (*(q+i))[Z] = ((*(q+i))[Z] - (*(q+i-1))[Z])/const1;
                (*(q+i))[H] = ((*(q+i))[H] - (*(q+i-1))[H])/const1;
            }
        }
    }
    else
    {                               /* From the left */
        for( i=0; i <= r; i++ )
        {
            (*(q+i))[X] = (*(polygon+r-i))[X];
            (*(q+i))[Y] = (*(polygon+r-i))[Y];
            (*(q+i))[Z] = (*(polygon+r-i))[Z];
            (*(q+i))[H] = (*(polygon+r-i))[H];
        }
        for( j=1; j <=r; j++ )
        {
            for( i = 0; i <= r-j; i++)
            {
                const1 = b[r-j+1-i];
                const2 = a[r-i];
                const3 = const1+const2;

                (*(q+i))[X] = ( const1 * (*(q+i+1))[X] + const2 * (*(q+i))[X])/const3;
                (*(q+i))[Y] = ( const1 * (*(q+i+1))[Y] + const2 * (*(q+i))[Y])/const3;
                (*(q+i))[Z] = ( const1 * (*(q+i+1))[Z] + const2 * (*(q+i))[Z])/const3;
                (*(q+i))[H] = ( const1 * (*(q+i+1))[H] + const2 * (*(q+i))[H])/const3;
            }
        }
        for( j = 1; j <= d; j++ )
        {
            for( i = d; i >= j; i-- )
            {
                const1 = -1.0 * a[r-i+j]/(r-j+1);

                (*(q+i))[X] = ((*(q+i))[X] - (*(q+i-1))[X])/const1;
                (*(q+i))[Y] = ((*(q+i))[Y] - (*(q+i-1))[Y])/const1;
                (*(q+i))[Z] = ((*(q+i))[Z] - (*(q+i-1))[Z])/const1;
                (*(q+i))[H] = ((*(q+i))[H] - (*(q+i-1))[H])/const1;
            }
        }
    }
    const1 = tau[r+1] - tau[r];
    for( i=1; i <= d; i++)
    {
        (*(q + i))[X] = (*(q + i))[X] * const1;
        (*(q + i))[Y] = (*(q + i))[Y] * const1;
        (*(q + i))[Z] = (*(q + i))[Z] * const1;
        (*(q + i))[H] = (*(q + i))[H] * const1;
        const1 = const1 * const1;
    }
    PUT(b);      /* De-allocate working memory */
    PUT(a);      /* De-allocate working memory */

    return(TRUE);
}

/*      @(#)  256.1 date 12/16/89 ds_knots.c  */
/*+
 *
 *        Filename    : ds_knots.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:18
 *        Last change : 89/12/16 19:20:51
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_knots.c
 *
 *        This file contains the following routines:
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */                                         
BOOLEAN                                    
os_mod_knt(knots,numknots,new,numnew,newknots)
                        /* Description   :- This function modifies a knot vector to
                         *                  include new elements.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- 1 - The original knot vector must be sorted
                         *                  2 - The new values lie within the old knots
                         *                  3 - No check is made if a new element is
                         *                      repeated many times.
                         *
                         */                                         
double     knots[];       /* <i> Knot vector */
INT        numknots;      /* <i> Number of knots in knot vector */
double     new[];       /* <i> Pointer to new knots to be added */
INT        numnew;      /* <i> Number of new knots */
double     *newknots[];   /* <o> Knot vector containg refined knots */
{
    double *wrk1;
    double *wrk2;
    int    i,j,k,mu;
    int    sum;

    sum = numknots + numnew;
    /* Create workspace chains as temp and allocate the memory for newknots */
    if (!(wrk1 =  GET_KNOT(sum + numnew)))
        return(FALSE);
    
    if (!(wrk2 = GET_KNOT(sum + numnew)))
        return(FALSE);

    if (!(*newknots = GET_KNOT(sum)))
        return(FALSE);

    knt_copy(knots,numknots,wrk1);  /* Copy to work space */

    mu = 0;
    for (i = 0; i < numnew; i++)
    {
        mu = os_find(mu,wrk1,new,numknots,i);
        /* Copy the source chain into the destination chain, up to that point */
        for (j = 0; j <= mu; j++)
            wrk2[j] = wrk1[j];

        wrk2[mu+1] = new[i];        /* Add in the new value */
        /* Copy the remainder of the source into the destination */
        k = mu + 2;
        for (j = mu+1; j < numknots; j++, k++)
            wrk2[k] = wrk1[j];

        /* If further iterations have to be processed copy the dest back to the source*/
        if ((i + 1) < numnew)
        {
            knt_copy(wrk2,numknots+i+1,wrk1);
            numknots = numknots + 1;
        }
    }
                                         
    knt_copy(wrk2,numknots+1,*newknots);    /* Copy the knot vector */

    /* Return work space chains */
    PUT(wrk1);
    PUT(wrk2);

    return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
BOOLEAN                                                                        
os_def_knt(n,k,closed,newknots)
                        /* Description   :- This function calculates the default knot
                         *                  vector for a spline.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                
INT     n;              /* <i> Number of control points */
INT     k;              /* <i> Order of spline */
BOOLEAN closed;         /* <i> Whether spline is closed */
double  *newknots[];    /* <o> Knot vector for spline */
{
    double  j;
    INT     i;   
    INT     num_knots;      /* Number of knots to generate */
                                                             
    num_knots = n + k;

    if (!(*newknots = GET_KNOT(num_knots)))
        return(FALSE);

    if (closed)
    {       /* Closed polygon generate a uniform knot vector */
        for (i = 0,j = 0.0; i < num_knots; i++,j++)
            (*newknots)[i] = j;
    }                 
    else
    {   /* Open polygon */
        for ( i = 0; i < k ; i++)
            (*newknots)[i] = 0.0;         /* Set first k elements to 0.0 */

        for (i = k, j = 1.0; i < n; i++,j++)
            (*newknots)[i] = j;                 /* Increment through intermediate elements */

        for (i = n; i < num_knots; i++)
            (*newknots)[i] = j;                 /* Set last k elements to j */
    }

    os_knt_norm(*newknots,num_knots,closed,k,*newknots);   /* Normalise the knot vector */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
BOOLEAN                                                                        
os_knt_rev(knots,n)
                        /* Description   :- This function reverses the knot vector
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                       
double  knots[];          /* <i> The knot vector */
INT     n;              /* <i> Number of knots */
{
    double *wrk1;
    INT     i,j;

    if (!(wrk1 = GET_KNOT(n)))
        return(FALSE);

    for (i = 0; i < (n -1); i++)
        wrk1[i] = knots[i+1] - knots[i];    /* Calculate the differences between elements*/

    for (i = 1, j = n - 2; i < n; i++, j--)
        knots[i] = knots[i-1] + wrk1[j];    /* Reverse knots and convert to ascending */

    PUT(wrk1);       /* Put back working memory */

    return( TRUE );
}                       
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */                                                                            
BOOLEAN
os_knt_revclosed(knots,n,k)
                        /* Description   :- This function reverses a closed knot vector
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                            
double    knots[];        /* <i> Knot vector */
INT       n;            /* <i> Number of knot elements */
INT       k;            /* <i> Order of spline */
{
    double *wrk1;
    INT    i,j;

    if (!(wrk1 = GET_KNOT(n)))
        return(FALSE);

    for (i = 0; i < (n -1); i++)    /* Calculate the differencies between elements*/
        wrk1[i] = knots[i+1] - knots[i];

    for ( i = k, j = n - (2 * k - 1); i <= n-k; i++, j--)   /* Reverse knot and convert */
        knots[i] = knots[i-1] + wrk1[j];                      /* to ascending order */

    os_knt_closed(knots,n,k);     /* Re-calculate the first and last (k -1) elements */
    PUT(wrk1);

    return( TRUE );
}                                                                                    
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
BOOLEAN                                                                        
os_iterate_knt(new,numnew,knots,numknots,max_rept,t,strtpos)
                        /* Description   :- This function copies a knot vector to a 
                         *                  refined knot vector and inserts a knot
                         *                  element numnew times into the refined 
                         *                  knot vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                             
double      new;        /* <i> The new element to be inserted */
INT         numnew;     /* <i> The number of times new is to be repeated */
double      knots[];      /* <i> Pointer to original knot vector */
INT         numknots;     /* <i> Number of knots in the knot vector */     
INT         max_rept;   /* <i> Max times a knot can be repeated */
double      *t[];       /* <o> New knot vector */
INT         *strtpos;   /* <o> The position of the first repeated knot */
{
    INT     i,j;
    INT     mu;

    if (!(*t = GET_KNOT(numknots + numnew)))
        return(FALSE);

    for ( i = 0; i < numknots - 1; i++)   /* FInd where to insert the knot */
    {
        if (new >= knots[i])
        {
            mu = i;
        }
    } 
    for (i = 0; i <= mu; i++)
        (*t)[i] = knots[i];       /* Copy up to that point */

    for (i = mu + 1; i <= mu + numnew; i++)
        (*t)[i] = new;          /* Copy new element numnew times */

    for (i = mu + numnew + 1, j = mu + 1; j < numknots; i++, j++) /* Copy rest */
        (*t)[i] = knots[j];

    return(os_rept_knt(new,*t,numknots + numnew, max_rept , strtpos)); /* Check no more than k occ */
                                                         /* of a knot in the knots   */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
BOOLEAN                                                                        
os_iterate_closed(new,numnew,knots,numknots,mult_max,order,t,strtpos)
                        /* Description   :- This function inserts a new knot element
                         *                  numnew times. When the new knot lies 
                         *                  outside the valid range, its proper index
                         *                  is calculated
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                              
double     new;         /* <i> The new element to be inserted */
INT        numnew;      /* <i> Number of times new is to be repeated */
double     knots[];       /* <i> Original knot vector */
INT        numknots;      /* <i> Number of elements in knots */
INT        mult_max;    /* <i> Maximum number of times a knot can be repeated */
INT        order;       /* <i> Order of splines under consideration */
double     *t[];        /* <o> Pointer to refined knot vector */
INT        *strtpos;    /* <o> Position of the first multiple in refined knot vector */
{                                                        
    INT    i;
    INT    mu;
    INT    new_mu;

    for (i = 0; i < numknots - 1; i++)
    {               /* Find where to insert new knot vector */
        if (new >= knots[i])
            mu = i;
    }

    if (mu < (order - 1))       /* In first k - 1 elements */
    {
        new_mu = numknots - order - (order - 1 - mu);
        new = knots[new_mu] + (new - knots[mu]);
    }
    else if (mu > (numknots - order))
    {
        new_mu = order - 1 + ( mu - numknots - order);
        new = knots[new_mu] + ( new - knots[mu]);
    }
    return(os_iterate_knt(new,numnew,knots,numknots,mult_max,t,strtpos));
}                         
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
INT                                                                        
os_rept_knt(new,knots,numt,kapa,strtpos)
                        /* Description   :- This function ensures that a knot does not
                         *                  exist any more than kapa times in a knot
                         *                  vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                        
double     new;         /* <i> New knot element to be checked */
double     knots[];     /* <i> Knot vector */
INT        numt;        /* <i> Number of knots */
INT        kapa;        /* <i> Number of occurrences allowed */
INT        *strtpos;    /* <o> Position of first element */
{
    double *knt;

    INT     fo;
    INT     lo;
    INT     i;
    INT     dif;
    INT     sum;

    sum = 0;

    for (i = 0, fo = -1; ((i < numt) && (fo == -1)) ; i++) /* Walk along whole knot vector until first */
    {                                   /* occurrence found */
        if (fabs(new - knots[i]) < TOL)
            fo = i;
    }                                        
    *strtpos = fo;         /* Record the first occurrence of the multiple knot */

    for (i = numt -1, lo = -1; ((i > 0) && (lo == -1)); i--)
    { /* Step from the end to the start and find the last occurrence */
        if (fabs(new - knots[i]) < TOL)
            lo = i;
    }
    dif = lo - fo;

    if (dif < kapa)
        return(numt);

    if (!(knt = GET_KNOT( 2 * numt))) /* Too many occurrences */
        return(FALSE);          /* Failed to get memory */

    knt_copy(knots,fo+kapa,knt);    /* Copy refined knot chain to work space */

    if (lo < (numt - 1))    /* Watch for multiple knots at end of vector */
    {
        for (i = lo + 1,sum = fo + kapa; i < numt; i++, sum++)
            knt[sum] = knots[i];
    }

    knt_copy(knt,sum,knots);      /* Copy work space to output */

    PUT(knt);    /* Give back temp */

    return(sum);
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 31 May 1988
 *
 */
double                                                                        
os_mid_knt(knots,num_knots, order)
                        /* Description   :- This function returns the mid-point of a
                         *                  knot vector
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                            
double     knots[];     /* <i> Knot vector */
INT        num_knots;   /* <i> Number of knots */
INT        order;       /* <i> Order of spline */
{
    return(knots[order - 1] + (knots[num_knots - order] - knots[order - 1])/2.0);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
void                                                                        
os_knt_norm(knotsin,numknots,closed,order,knotsout)
                        /* Description   :- This function normalises a knot vector
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- When a spline/surface is open the vector 
                         *                  lies  in the range 0.0 <= range <= 1.0
                         *
                         *                  When the spline/surface is closed the
                         *                  vector lies in the range 0.0 <= x <= 1.0
                         *                  between the (k-1)th and the (n)th elements.
                         *
                         */                                                            
double      knotsin[];  /* <i> The un-normalised knot vector */
INT         numknots;   /* <i> Number of knots in the knot vector */
BOOLEAN     closed;     /* <i> Whether the spline/surface is closed */
INT         order;      /* <i> The order of the spline/surface */
double      knotsout[]; /* <o> The normalised knot vector */
{
    double  max;
    double  min;
    INT     i;

    if (closed)     /* Closed */
    {               /* Yes .... */
        max = knotsin[numknots - order] - knotsin[order - 1];
        min = knotsin[order - 1];

        for (i = order - 1; i <= (numknots - order); i++)
            knotsout[i] = (knotsin[i] - min)/max;

        os_knt_closed(knotsout,numknots,order);
    }
    else
    {
        max = knotsin[numknots - 1] - knotsin[0];
        min = knotsin[0];

        for (i = 0; i < numknots; i++)
            knotsout[i] = (knotsin[i] - min)/max;
    }
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 * 
 */
void                                                                       
os_knt_closed(knots,numknots,order)
                        /* Description   :- This function processes a knot vector for a
                         *                  closed polygon and ensures that it 
                         *                  represents a parametric ring.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                              
double     knots[];     /* <i> Knot vector */
INT        numknots;    /* <i> Number of knots in knot vector */
INT        order;       /* <i> Order of spline/surface */
{
    INT     i;
    INT     ub;
    INT     lb;

    lb = order - 1;
    ub = numknots - order;

    /* Copy knot differencies of active range to end of knot vector */
    for (i = 0; i < (order - 1); i++,lb++,ub++)
        knots[ub + 1] = knots[ub] + (knots[lb + 1] - knots[lb]);

    lb = order - 1;
    ub = numknots - order;

    /* Copy knot differencies of active range to start of knot vector */
    for (i = 0; i < order - 1; i++,lb--,ub--)
        knots[lb - 1] = knots[lb] - (knots[ub] - knots[ub - 1]);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
void
os_knt_at_vertex(knots,n,order,vertex_map)                                                                        
                        /* Description   :- This function calculates the relationship
                         *                  between the knot vector and the control
                         *                  vertices of a polygon.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double    knots[];      /* <i> Knot vector */
INT       n;            /* <i> Number of control vertices */
INT       order;        /* <i> Order of spline/surface */
double    vertex_map[]; /* <o> Map of the vertices */
{
    INT     i;
    INT     j;

    for (i = 0; i < n; i++)
    {
        vertex_map[i] = 0.0;
        for ( j = i + 1; j < (i + order); j++)
            vertex_map[i] += knots[j];

        vertex_map[i] /= (order - 1);
    }

}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */                                                                            
void
knt_copy(knotsin,numknots,knotsout)
                        /* Description   :- This function copies one knot vector to
                         *                  another.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double     knotsin[];   /* <i> Source knot vector */
INT        numknots;    /* <i> Number of knots */
double     knotsout[];  /* <i> Destination knot vector */
{
    while (--numknots >= 0)
        knotsout[numknots] = knotsin[numknots];
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
INT
os_find(start,knots,refknots,numknots,j)
                        /* Description   :- This function finds the unique mu such
                         *                  that knots[mu] <= refknots[j] < knots[mu+1]
                         *
                         * Return status :- mu
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
INT     start;          /* <i> Start search from here */
double  knots[];        /* <i> Knot vector */
double  refknots[];     /* <i> Refined knot vector */
INT     numknots;       /* <i> Number of knots */
int     j;              /* <i> A counter */
{
    double     knotsj;
    double     diff;
    int        i;

    knotsj = knots[j];

    for (i = start; i < (numknots - 1); i++)
    {
        diff = knotsj - knots[i];
        if ((diff < 0.0) && (!TOL_FAILURE(diff)))
            return(i - 1);
    }
    return(numknots - 1);
}



   












/*      @(#)  256.1 date 12/16/89 ds_mem_maint.c  */
/*+
 *
 *        Filename    : ds_mem_maint.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:23
 *        Last change : 89/12/16 19:21:01
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_mem_maint.c
 *
 *  This file contains the solid modeller dataspace initialisation
 *  and maintenance routines:
 *
 *                 ds_mem_init
 *                 ds_alloc
 *                 ds_free
 *                 ds_more_core
 *
-*/


union       soheader{             /* Free block header */
                    struct {
                              union   soheader    *ptr;   /* Pointer to next free block */
                              unsigned            size;   /* Size of the free block in  */
                                                          /* multiples of header size */
                           }                      s;
                    int xxx;                          /* Forces alignment of header */
                    };
typedef     union     soheader      SOHEADER;

static      SOHEADER                base;                 /* Empty free list to get */
                                                          /* started */
static      SOHEADER                *allocds = NULL;      /* Last allocated block */
#define     SONALLOC                8192                  /* Number of units allocated */
                                                          /* at once */
static      SOHEADER                *ds_more_core();      /* Requests system for more */
                                                          /* memory */
extern      BOOLEAN                 ds_free();            /* Free memory previously */
                                                          /* allocated */


/*
 *========================================================================================*
 *
 *                                                              Author :: Jim Mulhearn
 *                                                              Date   :: 29th-March-1988
 *
 */
BOOLEAN
ds_mem_init( nbytes )
                        /* This routine allocates dataspace memory for the solid modeller
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         *  Note : This routine grabs a large chunk of memory when called.
                         *         If this routine is not called, the 
                         *         allocation/de-allocation will still operate.
                         */
unsigned  nbytes;       /* <i> The number of bytes to allocate */
{
      register    int           nunits;

      nunits = 1 + ( nbytes + sizeof( SOHEADER ) - 1 ) / ( sizeof( SOHEADER ));
                                                                 /* Round allocation up */
                                                                 /* to a multiple of */
                                                                 /* header size */
      if( allocds == NULL )
      {
            base.s.ptr = allocds = &base;
            base.s.size = 0;
      }
      if(ds_more_core( nunits) == NULL )
      {
            return( FALSE );
      }
      return( TRUE );
} 

/*
 *========================================================================================*
 *
 *                                                              Author :: Jim Mulhearn
 *                                                              Date   :: 29th-March-1988
 *
 */
char *
ds_alloc( nbytes )
                        /* This routine alocates memory from the solid modeller dataspace.
                         *
                         *      = Pointer to start of memory
                         *
                         */
unsigned    nbytes;     /* <i> The number of characters to allocate */
{
      register    SOHEADER      *p;
      register    SOHEADER      *q;
      register    int           nunits;
                 
      nunits = 1 + ( nbytes + sizeof( SOHEADER ) - 1 ) / ( sizeof( SOHEADER )); 
                                                                  /* Round allocation up */
                                                                  /* to a multiple of */
                                                                  /* header size */
      if(( q = allocds ) == NULL )
      {
            base.s.ptr = allocds = q = &base;
            base.s.size = 0;
      }
/*
              |           |
              |           |
           q->|-----------|
              |///////////|
              |///////////|
              |///////////|
              |-----------|
              |           |
              |           |
           p->|-----------|
              |///////////|
              |///////////|
              |///////////|
              |-----------|
              |           |
*/
      for( p = q -> s.ptr ;; q = p, p = p -> s.ptr )
      {
            if( p -> s.size >= nunits )                       /* Found enough space */
            {
                  if( p -> s.size == nunits )
                  {
                        q -> s.ptr = p -> s.ptr;              /* Fits exactly, remove */
                  }                                            /* from free list */
                  else
                  {
                        p -> s.size -= nunits;                /* Allocate the tail end */
                        p           += p -> s.size;                                   
                        p -> s.size  = nunits;                /* Record size of block */
                  }                                           /* allocated */
                  allocds = q;
                  
                  return(( char * )( p + 1 ));
            }
            if( p == allocds )                                /* Failed to allocate */
                                                              /* enough space */
            {                                                 /* Watch out for */
                                                              /* wrap-around */
                  if(( p = ds_more_core( nunits )) == NULL )  /* Allocate a new piece */
                  {                                           /* memory */
                        return( NULL );                       /* No memory left at all */
                  }
            }                                                                         
        }
}
/*                     
 *========================================================================================*
 *
 *                                                              Author :: Jim Mulhearn
 *                                                              Date   :: 29th-March-1988
 *
 */
BOOLEAN
ds_free( ap )                                                                          
                        /* This routine returns memory previously allocated, back onto 
                         * the free list structure.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
char *ap;               /* <i> Pointer to memory */
{
      register    SOHEADER    *p;
      register    SOHEADER    *q;
                                            
      p = ( SOHEADER * )ap - 1;                                      /* Point to header */

      for( q = allocds; !( p > q && p < q -> s.ptr ); q = q -> s.ptr )
      {                                          /* Look for place to insert free block */
            if( q >= q -> s.ptr && ( p > q || p < q -> s.ptr ))      /* Watch for */
            {                                                        /* memory at start */
                  break;                                             /* or end of limits */
            }
      }
      if( p + p -> s.size == q -> s.ptr )                           /* Coalesce with */
      {                                                             /* lower neighbour */
            p -> s.size += q -> s.ptr -> s.size;
            p -> s.ptr   = q -> s.ptr -> s.ptr;
      }
      else
      {
            p -> s.ptr = q -> s.ptr;
      }

      if( q + q -> s.size == p )                                    /* Coalesce with */
      {                                                             /* upper neighbour */
            q -> s.size += p -> s.size;
            q -> s.ptr   = p -> s.ptr;
      }
      else
      {
            q -> s.ptr = p;
      }
      allocds = q;

      return( TRUE );
}
/*
 *========================================================================================*
 *
 *                                                              Author :: Jim Mulhearn
                                                                Date   :: 29th-March-1988
 *
 */
static SOHEADER *
ds_more_core( nu )
                        /* This routine requests memory from the system.
                         *
                         *
                         */
unsigned nu;            /* <i> The number of units requested  */
{
      register    char     * cp;
      register    SOHEADER * up;
      register    int        rnu;
                           
      rnu = SONALLOC * (( nu + SONALLOC - 1 ) / SONALLOC );
      if(( cp = (char*)malloc( rnu * sizeof( SOHEADER ))) == NULL )      /* No space at all */
      {
            return( NULL );
      }
      up = ( SOHEADER * )cp;
      up -> s.size = rnu;

      ds_free(( char * ) ( up + 1 ));

      return( allocds );
}
/*      @(#)  256.1 date 12/16/89 ds_mesh.c  */
/*+
 *
 *        Filename    : ds_mesh.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:27
 *        Last change : 89/12/16 19:21:11
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_mesh.c
 *
 *        This file contains the following routines:
 *
 *             mshextract
 *             mshdeposit
 *             mshcopy
 *             mshclear
 *             mshmax
 *             mshmin
 *             mshminmax
 *             mshminmaxindex
 *             mshrowcol
 *             mshrevrowcol
 *             mshcolrow
 *             mshrevcolrow
 *             mshrowrev
 *             mshcolrev
 *             mshmove
 *
 *
 *
 *
-*/



/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshextract( ptr, i_index, j_index, columns, point )
                        /* Description   :- This function extracts a point from
                         *                  a homogeneous point mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr;        /* <i> Pointer to the point mesh */
int         i_index;    /* <i> The row index into the mesh */
int         j_index;    /* <i> The row index into the mesh */
int         columns;    /* <i> The number of columns in the mesh */
POINT     point;      /* <o> The resultant homogeneous point */
{
      pntextract( ptr, j_index + i_index * columns, point );
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshdeposit( ptr, i_index, j_index, columns, point )
                        /* Description   :- This function deposits a point into
                         *                  a homogeneous point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr;        /* <i> Pointer to the point mesh */
int         i_index;    /* <i> The row index into the mesh */
int         j_index;    /* <i> The row index into the mesh */
int         columns;    /* <i> The number of columns in the mesh */
POINT     point;      /* <i> The homogeneous point to be deposited */
{
      pntdeposit( ptr, j_index + i_index * columns, point );
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshcopy( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one homogeneous point
                         *                  mesh to another
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <i> Pointer to the output point mesh */
{                                         
    POINT point;
    int     i;

    for( i = 0; i < rows*columns; i++ )
    {
        pntextract( ptr_in,  i, point );
        pntdeposit( ptr_out, i, point );
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshclear( ptr_in, rows, columns )
                        /* Description   :- This function clears a homogeneous point
                         *                  mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
{                                         
    POINT point;
    int     i;

    vecclr( point, 4 );

    for( i = 0; i < rows*columns; i++ )
    {
            pntdeposit( ptr_in, i, point );
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshmax( ptr_in, rows, columns, max_point )
                        /* Description   :- This function calculates the maximum 
                         *                  X, Y, Z and H values of the given mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
POINT     max_point;  /* <o> The maximum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, max_point );
    POINTH_TO_3D(max_point,max_point);

    for( i = 1; i < rows*columns ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] > max_point[j] )
            {
                max_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshmin( ptr_in, rows, columns, min_point )
                        /* Description   :- This function calculates the miniimum 
                         *                  X, Y, Z and H values of the given mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
POINT     min_point;  /* <o> The minimum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, min_point );
    POINTH_TO_3D(min_point,min_point);


    for( i = 1; i < rows*columns ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] < min_point[j] )
            {
                min_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshminmax( ptr_in, rows, columns, min_point, max_point )
                        /* Description   :- This function calculates the minimum 
                         *                  and maximum X, Y, Z and H values of the 
                         *                 given mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
POINT     min_point;  /* <o> The minimum X, Y, Z and H values */
POINT     max_point;  /* <o> The maximum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, min_point );
    POINTH_TO_3D(min_point,min_point);
    pntextract( ptr_in, 0, max_point );
    POINTH_TO_3D(max_point,max_point);

    for( i = 1; i < rows*columns ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] < min_point[j] )
            {
                min_point[j] = point[j];
             }
            else
            if( point[j] > max_point[j] )
            {
                max_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshminmaxindex( ptr_in, index, rows, columns, min_point, max_point )
                        /* Description   :- This function calculates the minimum 
                         *                  and maximum values of the index ie X or Y
                         *                  or Z or H for the given mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;       /* <i> Pointer to the input point mesh */
INT       index;        /* <i> The actual point to check for */
int       rows;         /* <i> The number of rows in the mesh */
int       columns;      /* <i> The number of columns in the mesh */
double    *min_point;   /* <o> The minimum value */
double    *max_point;   /* <o> The maximum value */
{                                         
    POINT point;
    int     i;

    pntextract( ptr_in, 0, point );
    POINTH_TO_3D(point,point);

    *min_point = point[index];
    *max_point = *min_point;

    for( i = 1; i < rows*columns ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        if( point[index] < *min_point )
        {
            *min_point = point[index];
        }
        else if( point[index] > *max_point )
        {
            *max_point = point[index];
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshrowcol( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Rows are copied to columns. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The output mesh should be declared
                         *                   ( n X m )
                         *                                                       
                         *        n                        m
                         *   ------------          -------------------
                         *   |----1---->|          || |              |
                         *   |----2---->|          || |              |
                         * m |          |        n |1 2              |
                         *   |          |          || |              |
                         *   |          |          |V V              |
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT points;
    int     j;

    points = GET_POINT( columns );      /* Allocate working memory */
    
    for( j = 0; j < rows; j++ )
    {
        mshrowpnt( ptr_in, j, columns, points );
        pntcolmsh( points, j, columns, rows, ptr_out );
    }

    PUT( points );                      /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshrevrowcol( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Rows are copied to columns in reverse. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The output mesh should be declared
                         *                   ( n X m )
                         *                                                       
                         *        n                        m
                         *   ------------          -------------------
                         *   |----1---->|          |              | ||
                         *   |----2---->|          |              | ||
                         * m |          |        n |              2 1|
                         *   |          |          |              | ||
                         *   |          |          |              V V|
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT points;
    int     i;

    points = GET_POINT( columns );      /* Allocate working memory */
    
    for( i = 0; i < rows; i++ )
    {
        mshrowpnt( ptr_in, i, columns, points );
        pntcolmsh( points, rows - i - 1, columns, rows, ptr_out );
    }

    PUT( points );                      /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshcolrow( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Columns are copied to rows. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The output mesh should be declared
                         *                   ( n X m )
                         *                                                       
                         *        n                        m
                         *   ------------          -------------------
                         *   || |       |          |-------1-------->|
                         *   || |       |          |-------2-------->|
                         * m |1 2       |        n |                 |
                         *   || |       |          |                 |
                         *   |V V       |          |                 |
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT points;
    int     i;

    points = GET_POINT( rows );      /* Allocate working memory */
    
    for( i = 0; i < columns; i++ )
    {
        mshcolpnt( ptr_in, i, rows, columns, points );
        pntrowmsh( points, i, rows, ptr_out );
    }

    PUT( points );                      /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshrevcolrow( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Columns are copied to rows in reverse. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The output mesh should be declared
                         *                   ( n X m )
                         *                                                       
                         *        n                        m
                         *   ------------          -------------------
                         *   || |       |          |                 |
                         *   || |       |          |                 |
                         * m |1 2       |        n |                 |
                         *   || |       |          |-------2-------->|
                         *   |V V       |          |-------1-------->|
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT points;
    int     i;

    points = GET_POINT( rows );      /* Allocate working memory */
    
    for( i = 0; i < columns; i++ )
    {
        mshcolpnt( ptr_in, i, rows, columns, points );
        pntrowmsh( points, rows - i - 1, columns, ptr_out );
    }
    PUT( points );                      /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshrowrev( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Rows are reversed in the  copy. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  
                         *               
                         *                                                       
                         *        n                        n
                         *   ------------          -------------------
                         *   |----1---->|          |                 |
                         *   |----2---->|          |                 |
                         * m |          |        m |                 |
                         *   |          |          |-------2-------->|
                         *   |          |          |-------1-------->|
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT mesh;
    PTR_POINT points;
    int     i;

    mesh   = GET_MESH( rows, columns );      /* Allocate working memory */
    points = GET_POINT( columns );           /* Allocate working memory */
    
    for( i = 0; i < rows; i++ )
    {
        mshrowpnt( ptr_in, i, columns, points );
        pntrowmsh( points, i, rows, mesh );
    }

    mshcopy( mesh, rows, columns, ptr_out );              /* Copy working mesh to output */

    PUT( points );                           /* De-allocate working memory */
    PUT( mesh );                             /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshcolrev( ptr_in, rows, columns, ptr_out )
                        /* Description   :- This function copies one mesh to another.
                         *                  Columns are reversed in the  copy. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  
                         *               
                         *                                                       
                         *        n                        n
                         *   ------------          -------------------
                         *   || |       |          |             | | |
                         *   || |       |          |             | | |
                         * m |1 2       |        m |             2 1 |
                         *   || |       |          |             | | |
                         *   |V V       |          |             V V |
                         *   ------------          -------------------
                         *
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_out;    /* <o> The output mesh */
{                                         
    PTR_POINT mesh;
    PTR_POINT points;
    int     i;

    mesh   = GET_MESH( rows, columns );      /* Allocate working memory */
    points = GET_POINT( rows );              /* Allocate working memory */
    
    for( i = 0; i < columns; i++ )
    {
        mshcolpnt( ptr_in, i, rows, columns, points );
        pntcolmsh( points, i, columns, rows, mesh );
    }
    mshcopy( mesh, rows, columns, ptr_out );              /* Copy working mesh to output */

    PUT( points );                           /* De-allocate working memory */
    PUT( mesh );                             /* De-allocate working memory */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshmove( ptr_in, rows, columns, vector, ptr_out )
                        /* Description   :- This function moves all the elements of
                         *                  a point mesh by a displacement vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
VECTOR      vector;     /* <i> The displacement vector */
PTR_POINT ptr_out;    /* <o> Pointer to the resultant point mesh */
{                                         
    POINT point;
    int     i;

    for( i = 0; i < rows*columns; i++ )
    {
        pntextract( ptr_in, i, point );
        vecsum( point, vector, 3, point );
        pntdeposit( ptr_out, i, point );
    }
      return( TRUE );
}
/*      @(#)  256.1 date 12/16/89 ds_mesh_point.c  */
/*+
 *
 *        Filename    : ds_mesh_point.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:32
 *        Last change : 89/12/16 19:21:22
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_mesh_point.c
 *
 *        This file contains the following routines:
 *
 *             mshrowpnt
 *             mshcolpnt
 *             pntrowmsh
 *             pntcolmsh
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshrowpnt( ptr_mesh, i_index, columns, ptr_point )
                        /* Description   :- This function copies a row of points
                         *                  from a mesh to a point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_mesh;   /* <i> Pointer to the point mesh */
int         i_index;    /* <i> The row index into the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_point;  /* <o> The homogeneous point array */
{       
        POINT   point;
        int     j;

        for( j = 0; j < columns; j++ )
        {
            mshextract( ptr_mesh, i_index, j, columns, point );
            pntdeposit( ptr_point, j, point );
        }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
mshcolpnt( ptr_mesh, j_index, rows, columns, ptr_point )
                        /* Description   :- This function copies a column of points
                         *                  from a mesh to a point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_mesh;   /* <i> Pointer to the point mesh */
int         j_index;    /* <i> The column index into the mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_point;      /* <o> The homogeneous point array */
{       
        POINT   point;
        int     i;

        for( i = 0; i < rows; i++ )
        {
            mshextract( ptr_mesh, i, j_index, columns, point );
            pntdeposit( ptr_point, i, point );
        }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntrowmsh( ptr_point, i_index, columns, ptr_mesh )
                        /* Description   :- This function copies a point
                         *                  array to a row of a mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_point;  /* <i> The homogeneous point array */
int         i_index;    /* <i> The row index into the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_mesh;   /* <o> Pointer to the point mesh */
{       
        POINT   point;
        int     j;

        for( j = 0; j < columns; j++ )
        {
            pntextract( ptr_point, j, point );
            mshdeposit( ptr_mesh, i_index, j, columns, point );
        }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntcolmsh( ptr_point, j_index, rows, columns, ptr_mesh )
                        /* Description   :- This function copies a point
                         *                  array to a column of a mesh.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_point;  /* <i> The homogeneous point array */
int         j_index;    /* <i> The column index into the mesh */
int         rows;       /* <i> The number of rows in the mesh */
int         columns;    /* <i> The number of columns in the mesh */
PTR_POINT ptr_mesh;   /* <o> Pointer to the point mesh */
{       
        POINT   point;
        int     i;

        for( i = 0; i < rows; i++ )
        {
            pntextract( ptr_point, i, point );
            mshdeposit( ptr_mesh, i, j_index, columns, point );
        }
      return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 ds_newton.c  */
/*+
 *
 *        Filename    : ds_newton.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:37
 *        Last change : 89/12/16 19:21:32
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_newton.c
 *
 *        This file contains the following routines:
 *
 *        new_pnt_spln
 *        new_spln_spln
 *        new_spln_surf
 *        new_pnt_surf
 *        new_surf_surf
 *
-*/


#define     MAX_ITERATIONS  10      /* The maximum number of iterations to attempt */

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 1st-July-1988
 *
 */
BOOLEAN
new_pnt_spln( point, curve, ustart,  tol, uout, curve_pnt )
                        /* Description   :- This function performs a Newton-Raphson
                         *                  iteration to calculate the point on the
                         *                  spline nearest to the given point.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The curve must have at least first degree 
                         *                  continuity.
                         *
                         *                  The start parameters must be set to a 
                         *                  known local point if  possible.
                         *                  Otherwise the algorithm may converge to the
                         *                  wrong point.
                         *
                         *                  See page 308 of "Geometric Modelling" by
                         *                  Michael Mortenson for an explanation of
                         *                  the mathematics.
                         *
                         */
POINT              point;     /* <i> THe given point */
DS_NURB_SPLINE_PTR curve;     /* <i> Pointer to the spline curve */
double             ustart;    /* <i> An estimated start parameter */
double            tol;        /* <i> The tolerance within which two points */
                              /*     will be considered coincident */
double            *uout;      /* <o> The resultant parameter on the curve */
POINT             curve_pnt;  /* <o> The point on the curve */
{
    VECTOR  fdu;
    VECTOR  fddu;

    VECTOR  disp;
    double  u;
    double  dp;
    INT     i;
    INT     mu_u;

    double  uconst;
                       
    vecneg(point,CART,disp);
    if(!move_nurbspline(curve,disp))        /* Displace spline origin to point */
    {
        return(FALSE);
    }
    u = ustart;

    for( i = 0; i < MAX_ITERATIONS; i++ )
    {
        bsp_set_mu_u(0);
        if(!bsp_point_on_spline( curve, u, curve_pnt, fdu, fddu ))
        {
           move_nurbspline(curve,point);   /* Re-locate spline */
           return(FALSE);
       }
       mu_u = bsp_get_mu_u();
       uconst = (curve->knots[mu_u + 1]) - (curve->knots[mu_u]);
       dp = vecdp(curve_pnt,fdu,CART);
       if( fabs(dp) <= tol )
       {
            *uout = u;
            move_nurbspline(curve,point);            /* Re-locate spline */
            vecsum(curve_pnt,point,CART,curve_pnt);  /* Re-locate point on spline */
            return( TRUE );
       }
       u = u - uconst * dp/ (vecdp(curve_pnt,fddu,CART) + vecdp(fdu,fdu,CART));

       if(u < 0.0 )                            /* Stop at spline boundaries */
       {
           u = 0.0;
       }
       else if( u > 1.0 )
       {
            u = 1.0;
       }
    }
    *uout = u;
    if(!move_nurbspline(curve,point))          /* Re-locate spline */
    {
        return(FALSE);
    }
    vecsum(curve_pnt,point,CART,curve_pnt);    /* Re-locate point on spline */
    return(TRUE);           /* Point does not lie on spline */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 28th-June-1988
 *
 */
BOOLEAN
new_spln_spln( curve1, curve2, u1start, u2start,  tol, u1out, curve1_pnt, u2out , curve2_pnt)
                        /* Description   :- This function performs a Newton-Raphson
                         *                  iteration to calculate the point of
                         *                  intersection between two curves.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The curves must both have at least first degree 
                         *                  continuity.
                         *
                         *                  The start parameters must be set to a 
                         *                  known local point if  possible.
                         *                  Otherwise the algorithm may converge to the
                         *                  wrong point.
                         *
                         *                  See page 340 of "Geometric Modelling" by
                         *                  Michael Mortenson for an explanation of
                         *                  the mathematics.
                         *
                         *                  The algorithm will fail if spline tangents
                         *                  are parallel or orthogonal to each other at
                         *                  any iteration.
                         *
                         */
DS_NURB_SPLINE_PTR curve1;    /* <i> Pointer to the first curve */
DS_NURB_SPLINE_PTR curve2;    /* <i> Pointer to the second curve */
double            u1start;    /* <i> An estimated start parameter */
double            u2start;    /* <i> An estimated start parameter */
double            tol;        /* <i> The tolerance within which two points */
                              /*     will be considered coincident */
double            *u1out;     /* <o> The resultant parameter on curve1 */
POINT             curve1_pnt; /* <o> The resultant point on curve1 */
double            *u2out;     /* <o> The resultant parameter on curve2 */
POINT             curve2_pnt; /* <o> The resultant point on curve2 */
{
    VECTOR  fdu1;
    VECTOR  fdu2;
    VECTOR  fddu1;
    VECTOR  fddu2;

    VECTOR  diff_vec;
    POINT   r;
    POINT   rinc;
    double  denom_x;
    double  denom_y;
    double  denom_z;
    double  u1;
    double  u2;
    double  u1inc;
    double  u2inc;
    INT     i;
    INT     mu_u1;
    INT     mu_u2;

    double  u1const;
    double  u2const;
    
    u1 = u1start;
    u2 = u2start;

    bsp_set_mu_u(0);
    if(!bsp_point_on_spline( curve1, u1, curve1_pnt, fdu1, fddu1 ))
    {
             return(FALSE);
    }
    mu_u1 = bsp_get_mu_u();
    u1const = (curve1->knots[mu_u1 + 1]) - (curve1->knots[mu_u1]);
    bsp_set_mu_u(0);
    if(!bsp_point_on_spline( curve2, u2, curve2_pnt, fdu2, fddu2 ))
    {
             return(FALSE);
    }
    mu_u2 = bsp_get_mu_u();
    u2const = (curve2->knots[mu_u2 + 2]) - (curve2->knots[mu_u2]);

    vecdif(curve1_pnt, curve2_pnt, 3, r );
     
    for( i = 0; i < MAX_ITERATIONS; i++ )
    {
       denom_x =  (fdu1[Y]*fdu2[Z] - fdu1[Z]*fdu2[Y]);   /* Assuming X constant */
       denom_y = -(fdu1[X]*fdu2[Z] - fdu1[Z]*fdu2[X]);   /* Assuming Y constant */
       denom_z =  (fdu1[X]*fdu2[Y] - fdu1[Y]*fdu2[X]);   /* Assuming Z constant */

       if( fabs(denom_x) >= fabs(denom_y))                /* Select largest denominator */
       {
            if( fabs(denom_x) >= fabs(denom_z))
            {
                if(TOL_FAILURE(denom_x))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                u1inc = u1 + u1const * (fdu2[Y]*r[Z] - fdu2[Z]*r[Y])/denom_x;
                u2inc = u2 + u2const * (fdu1[Y]*r[Z] - fdu1[Z]*r[Y])/denom_x;
            }
            else
            {
                if(TOL_FAILURE(denom_z))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                u1inc = u1 + u1const * (fdu2[X]*r[Y] - fdu2[Y]*r[X])/denom_z;
                u2inc = u2 + u2const * (fdu1[X]*r[Y] - fdu1[Y]*r[X])/denom_z;
            }
       }
       else
       {
            if( fabs(denom_y) >= fabs(denom_z))
            {
                if(TOL_FAILURE(denom_y))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                u1inc = u1 + u1const * (fdu2[Z]*r[X] - fdu2[X]*r[Z])/denom_y;
                u2inc = u2 + u2const * (fdu1[Z]*r[X] - fdu1[X]*r[Z])/denom_y;
            }
            else
            {
                if(TOL_FAILURE(denom_z))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                u1inc = u1 + u1const * (fdu2[X]*r[Y] - fdu2[Y]*r[X])/denom_z;
                u2inc = u2 + u2const * (fdu1[X]*r[Y] - fdu1[Y]*r[X])/denom_z;
            }
       }
       if(u1inc < 0.0 )
       {
            u1inc = 0.0;
       }
       else if( u1inc > 1.0 )
       {
            u1inc = 1.0;
       }
       if(u2inc < 0.0 )
       {
            u2inc = 0.0;
       }
       else if( u2inc > 1.0 )
       {
            u2inc = 1.0;
       }
                   
       bsp_set_mu_u(0);
       if(!bsp_point_on_spline( curve1, u1inc, curve1_pnt, fdu1, fddu1 ))
       {
                return(FALSE);
       }
       mu_u1 = bsp_get_mu_u();
       u1const = (curve1->knots[mu_u1 + 1]) - (curve1->knots[mu_u1]);
       bsp_set_mu_u(0);
       if(!bsp_point_on_spline( curve2, u2inc, curve2_pnt, fdu2, fddu2 ))
       {
                return(FALSE);
       }
       mu_u2 = bsp_get_mu_u();
       u2const = (curve2->knots[mu_u2 + 2]) - (curve2->knots[mu_u2]);

       vecdif(curve1_pnt, curve2_pnt, 3, rinc );
       vecdif(rinc, r, 3, diff_vec );
        
       if((veclen( diff_vec, 3)) <= tol )
       {
            *u1out = u1inc;
            *u2out = u2inc;

            return( TRUE );
       }

       u1 = u1inc;
       u2 = u2inc;
       veccpy(rinc, 3, r );

    }
    *u1out = u1inc;
    *u2out = u2inc;

    return(TRUE);           /* Point does not lie on both splines */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 1st-July-1988
 *
 */
BOOLEAN
new_spln_surf( spln, surf, c_param, s_params, tol, out_c_param, c_pnt, out_s_params, surf_pnt  )
                        /* Description   :- This function performs a Newton-Raphson
                         *                  iteration to calculate a point of 
                         *                  intersection between a spline curve and
                         *                  a surface.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The surface and spline must have at least
                         *                  first degree continuity in all parametric
                         *                  directions.
                         *
                         *                  The start parameters must be local to the
                         *                  actual common point. Otherwise the algorithm
                         *                  may jump to another solution or fail to
                         *                  terminate successfully.
                         *
                         *                  See page 331 of Geometric Modelling by
                         *                  Michael Mortenson for an explanation of
                         *                  the mathematics.
                         *
                         *                  If the spline and surface do not intersect,
                         *                  the algorithm will fail.
                         *
                         *
                         */
DS_NURB_SPLINE_PTR spln;     /* <i> Pointer to the spline */
DS_NURB_SURFACE_PTR surf;   /* <i> Pointer to the surface */
double    c_param;           /* <i> The estimated start parameters for spline */
double    s_params[2];       /* <i> The estimated start parameters for surface */
double    tol;               /* <i> The tolerance within which two points */
                             /*     will be considered coincident */
double    *out_c_param;      /* <i> The resultant parameter for the curve */
POINT     c_pnt;             /* <o> The resultant point on the curve */
double    out_s_params[2];   /* <i> The resultant parameters for the surface */
POINT     surf_pnt;          /* <o> The resultant point on the surface */
{
    VECTOR  fdu;
    VECTOR  fddu;

    VECTOR  fdtu;
    VECTOR  fdtv;
    VECTOR  fddtu;
    VECTOR  fddtv;

    POINT   cp;
    VECTOR  r;
    VECTOR  rinc;
    VECTOR  diff_vec;
    double  du;
    double  num;
    double  uc_const;
    double  u1const;
    double  v1const;
    INT     i;
    INT     mu_c;
    INT     mu_u;
    INT     mu_v;
    

    bsp_set_mu_u(0);
    if(!bsp_point_on_spline( spln, c_param, c_pnt, fdu, fddu ))
    {
             return(FALSE);
    }

    mu_c     = bsp_get_mu_u();
    uc_const = (spln->knots[mu_c + 1]) - (spln->knots[mu_c]);
 
    bsp_set_mu_u(0);
    bsp_set_mu_v(0);
    if(!bsp_point_on_surface( surf, s_params[0], s_params[1], surf_pnt, fdtu, fdtv, fddtu, fddtv ))
    {
             return(FALSE);
    }
    mu_u = bsp_get_mu_u();
    u1const = (surf->knots_u[mu_u + 1]) - (surf->knots_u[mu_u]);
    mu_v = bsp_get_mu_v();
    v1const = (surf->knots_v[mu_v + 1]) - (surf->knots_v[mu_v]);
    
    *out_c_param  = c_param;
    out_s_params[0] = s_params[0];
    out_s_params[1] = s_params[1];
       
    vecdif( c_pnt, surf_pnt, 3, r );

    for( i = 0; i < MAX_ITERATIONS; i++ )
    {
       veccp(fdtu, fdtv, cp );
       du = vecdp(fdu, cp, 3);

       if(TOL_FAILURE(du))
       {
             return(FALSE);          /* Tangential dis-continuity */
       }
       veccp( fdtv, r, cp);
       num = vecdp( fdtu, cp, 3 );
       *out_c_param = *out_c_param - uc_const * num/du;

       veccp( fdu, r, cp);
       num = vecdp( fdtv, cp, 3 );
       out_s_params[0] = out_s_params[0] + u1const * num/du;

       veccp( fdtu, r, cp);
       num = vecdp( fdu, cp, 3 );
       out_s_params[1] = out_s_params[1] + v1const * num/du;

       if(*out_c_param < 0.0 )            /* Stop at curve boundary */
       {
            *out_c_param = 0.0;
       }
       else if( *out_c_param > 1.0 )
       {
            *out_c_param = 1.0;
       }
       if(out_s_params[0] < 0.0 )       /* Stop at surface boundary */
       {
            out_s_params[0] = 0.0;
       }
       else if( out_s_params[0] > 1.0 )
       {
            out_s_params[0] = 1.0;
       }
       if(out_s_params[1] < 0.0 )
       {
            out_s_params[1] = 0.0;
       }
       else if( out_s_params[1] > 1.0 )
       {
            out_s_params[1] = 1.0;
       }

       bsp_set_mu_u(0);
       if(!bsp_point_on_spline( spln, *out_c_param, c_pnt, fdu, fddu ))
       {
                return(FALSE);
       }

       mu_c     = bsp_get_mu_u();
       uc_const = (spln->knots[mu_c + 1]) - (spln->knots[mu_c]);
 
       bsp_set_mu_u(0);
       bsp_set_mu_v(0);
       if(!bsp_point_on_surface( surf, out_s_params[0], out_s_params[1], surf_pnt, fdtu, fdtv, fddtu, fddtv ))
       {
                return(FALSE);
       }
       mu_u = bsp_get_mu_u();
       u1const = (surf->knots_u[mu_u + 1]) - (surf->knots_u[mu_u]);
       mu_v = bsp_get_mu_v();
       v1const = (surf->knots_v[mu_v + 1]) - (surf->knots_v[mu_v]);

       vecdif(c_pnt, surf_pnt, 3, rinc );    

       vecdif(rinc, r, 3, diff_vec);                   
       if((veclen( diff_vec, 3)) <= tol )
       {
            return( TRUE );
       }
       veccpy(rinc, 3, r);
    }
    return(TRUE);           /* Surfaces do not intersect */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 28th-June-1988
 *
 */
BOOLEAN
new_pnt_surf( point, surface, ustart, vstart,  tol, uout, vout, surf_pnt  )
                        /* Description   :- This function performs a Newton-Raphson
                         *                  iteration to minimise the distance
                         *                  between a Cartesian point and a surface.
                         *                  If the point lies on the surface the
                         *                  algorithm will return the parameter values.
                         *                  If the point lies off the surface the 
                         *                  algorithm will return the parameter values
                         *                  of the nearest point on the surface.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The surface must have at least first degree 
                         *                  continuity in both parametric directions.
                         *
                         *                  The start parameters must be set to a 
                         *                  known local point if  possible.
                         *                  Otherwise the algorithm may converge to the
                         *                  wrong point, particularly if the surface
                         *                  doubles back on itself.
                         *
                         *                  See page 340 of "Geometric Modelling" by
                         *                  Michael Mortenson for an explanation of
                         *                  the mathematics.
                         *
                         */
POINT               point;          /* <i> The Cartesian point  */
DS_NURB_SURFACE_PTR surface;        /* <i> Pointer to the surface */
double              ustart;         /* <i> An estimated start parameter */
double              vstart;         /* <i> An estimated start parameter */
double              tol;            /* <i> The tolerance within which two points */
                                    /*     will be considered coincident */
double              *uout;          /* <o> The resultant u parameter */
double              *vout;          /* <o> The resultant v parameter */ 
POINT               surf_pnt;       /* <o> The resultant point on the surface */
{
    VECTOR  fdtu;
    VECTOR  fdtv;
    VECTOR  fddtu;
    VECTOR  fddtv;

    VECTOR  diff_vec;
    POINT   r;
    POINT   rinc;
    double  denom_x;
    double  denom_y;
    double  denom_z;
    double  u;
    double  v;
    double  uinc;
    double  vinc;
    INT     i;
    INT     mu_u;
    INT     mu_v;

    double  uconst;
    double  vconst;
    
    u = ustart;
    v = vstart;

    bsp_set_mu_u(0);
    bsp_set_mu_v(0);
    if(!bsp_point_on_surface( surface, u, v, surf_pnt, fdtu, fdtv, fddtu, fddtv ))
    {
             return(FALSE);
    }
    vecdif(surf_pnt, point, 3, r );

    for( i = 0; i < MAX_ITERATIONS; i++ )
    {
       denom_x =  (fdtu[Y]*fdtv[Z] - fdtu[Z]*fdtv[Y]);   /* Assuming X constant */
       denom_y = -(fdtu[X]*fdtv[Z] - fdtu[Z]*fdtv[X]);   /* Assuming Y constant */
       denom_z =  (fdtu[X]*fdtv[Y] - fdtu[Y]*fdtv[X]);   /* Assuming Z constant */

       mu_u = bsp_get_mu_u();
       uconst = (surface->knots_u[mu_u + 1]) - (surface->knots_u[mu_u]);
       mu_v = bsp_get_mu_v();
       vconst = (surface->knots_v[mu_v + 1]) - (surface->knots_v[mu_v]);

       if( fabs(denom_x) >= fabs(denom_y))                /* Select largest denominator */
       {
            if( fabs(denom_x) >= fabs(denom_z))
            {
                if(TOL_FAILURE(denom_x))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                uinc = u + uconst * (fdtv[Y]*r[Z] - fdtv[Z]*r[Y])/denom_x;
                vinc = v - vconst * (fdtu[Y]*r[Z] - fdtu[Z]*r[Y])/denom_x;
            }
            else
            {
                if(TOL_FAILURE(denom_z))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                uinc = u + uconst * (fdtv[X]*r[Y] - fdtv[Y]*r[X])/denom_z;
                vinc = v - vconst * (fdtu[X]*r[Y] - fdtu[Y]*r[X])/denom_z;
            }
       }
       else
       {
            if( fabs(denom_y) >= fabs(denom_z))
            {
                if(TOL_FAILURE(denom_y))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                uinc = u + uconst * (fdtv[Z]*r[X] - fdtv[X]*r[Z])/denom_y;
                vinc = v - vconst * (fdtu[Z]*r[X] - fdtu[X]*r[Z])/denom_y;
            }
            else
            {
                if(TOL_FAILURE(denom_z))
                {
                    return(FALSE);          /* Tangential dis-continuity */
                }
                uinc = u + uconst * (fdtv[X]*r[Y] - fdtv[Y]*r[X])/denom_z;
                vinc = v - vconst * (fdtu[X]*r[Y] - fdtu[Y]*r[X])/denom_z;
            }
       }
       if(uinc < 0.0 )
       {
            uinc = 0.0;
       }
       else if( uinc > 1.0 )
       {
            uinc = 1.0;
       }
       if(vinc < 0.0 )
       {
            vinc = 0.0;
       }
       else if( vinc > 1.0 )
       {
            vinc = 1.0;
       }
                   
       bsp_set_mu_u(0);
       bsp_set_mu_v(0);
       if(!bsp_point_on_surface( surface, uinc, vinc, surf_pnt, fdtu, fdtv, fddtu, fddtv ))
       {
                return(FALSE);
       }
       vecdif(surf_pnt, point, 3, rinc );
       vecdif(rinc, r, 3, diff_vec );
        
       if((veclen( diff_vec, 3)) <= tol )
       {
            *uout = uinc;
            *vout = vinc;

            return( TRUE );
       }

       u = uinc;
       v = vinc;
       veccpy(rinc, 3, r );

    }
    *uout = uinc;
    *vout = vinc;

    return(TRUE);           /* Point does not lie on the surface */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 29th-June-1988
 *
 */
BOOLEAN
new_surf_surf( surf1, surf2, in_params1, in_params2, tol, out_params1, surf_pnt1,
                          out_params2, surf_pnt2 )
                        /* Description   :- This function performs a Newton-Raphson
                         *                  iteration to calculate a point, common
                         *                  to both surfaces.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- Both surfaces must have at least first
                         *                  degree continuity in both parametric
                         *                  directions.
                         *
                         *                  The start parameters must be local to the
                         *                  actual common point. Otherwise the algorithm
                         *                  may jump to another solution or fail to
                         *                  terminate successfully.
                         *
                         *                  See page 341 of Geometric Modelling by
                         *                  Michael Mortenson for an explanation of
                         *                  the mathematics.
                         *
                         *                  If surfaces do not intersect, the algorithm
                         *                  will fail.
                         *
                         *                  If surface normals are locally parallel on
                         *                  any iteration this algorithm will fail.
                         *
                         */
DS_NURB_SURFACE_PTR surf1;   /* <i> Pointer to the first surface */
DS_NURB_SURFACE_PTR surf2;   /* <i> Pointer to the second surface */
double    in_params1[2];     /* <i> The estimated start parameters for first surface */
double    in_params2[2];     /* <i> The estimated start parameters for second surface */
double              tol;     /* <i> The tolerance within which two points */
                             /*     will be considered coincident */
double    out_params1[2];    /* <o> The resultant parameters for first surface */
POINT     surf_pnt1;         /* <o> The resultant point for the first surface */
double    out_params2[2];    /* <o> The resultant parameters for second surface */
POINT     surf_pnt2;         /* <o> The resultant point for the second surface */
{
    VECTOR  fdtu1;
    VECTOR  fdtv1;
    VECTOR  fddtu1;
    VECTOR  fddtv1;

    VECTOR  fdtu2;
    VECTOR  fdtv2;
    VECTOR  fddtu2;
    VECTOR  fddtv2;

    POINT   cp;
    VECTOR  r;
    VECTOR  rinc;
    VECTOR  diff_vec;
    double  du1;
    double  dv1;
    double  du2;
    double  dv2;
    double  num;
    double  u1const;
    double  u2const;
    double  v1const;
    double  v2const;
    INT     i;
    INT     mu1_u;
    INT     mu2_u;
    INT     mu1_v;
    INT     mu2_v;
    

    bsp_set_mu_u(0);
    bsp_set_mu_v(0);
    if(!bsp_point_on_surface( surf1, in_params1[0], in_params1[1], surf_pnt1, fdtu1, fdtv1, fddtu1, fddtv1 ))
    {
             return(FALSE);
    }

    mu1_u = bsp_get_mu_u();
    u1const = (surf1->knots_u[mu1_u + 1]) - (surf1->knots_u[mu1_u]);
    mu1_v = bsp_get_mu_v();
    v1const = (surf1->knots_v[mu1_v + 1]) - (surf1->knots_v[mu1_v]);
 
    bsp_set_mu_u(0);
    bsp_set_mu_v(0);
    if(!bsp_point_on_surface( surf2, in_params2[0], in_params2[1], surf_pnt2, fdtu2, fdtv2, fddtu2, fddtv2 ))
    {
             return(FALSE);
    }
    mu2_u = bsp_get_mu_u();
    u2const = (surf2->knots_u[mu2_u + 2]) - (surf2->knots_u[mu2_u]);
    mu2_v = bsp_get_mu_v();
    v2const = (surf2->knots_v[mu2_v + 2]) - (surf2->knots_v[mu2_v]);
    
    out_params1[0] = in_params1[0];
    out_params1[1] = in_params1[1];
    out_params2[0] = in_params2[0];
    out_params2[1] = in_params2[1];
       
    vecdif( surf_pnt1, surf_pnt2, 3, r );

    for( i = 0; i < MAX_ITERATIONS; i++ )
    {
       veccp(fdtu2, fdtv2, cp );
       du1 = vecdp(fdtv1, cp, 3);

       dv1 = vecdp(fdtu1, cp, 3);

       veccp(fdtu1, fdtv1, cp );
       du2 = -1.0 * vecdp(fdtv2, cp, 3);

       dv2 = -1.0 * vecdp(fdtu2, cp, 3);

       if( fabs(du1) >= fabs(dv1))                /* Select largest denominator */
       {
            if( fabs(du1) >= fabs(du2))
            {
                if(fabs(du1) >= fabs(dv2))
                {                                /* du1 is largest */
                    if(TOL_FAILURE(du1))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtv2, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[1] = out_params1[1] - v1const * num/du1;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtv2, cp, 3 );
                    out_params2[0] = out_params2[0] + u2const * num/du1;

                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params2[1] = out_params2[1] + v2const * num/du1;
                }
                else
                {                                /* dv2 is largest */
                    if(TOL_FAILURE(dv2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/dv2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/dv2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[0] = out_params2[0] - u2const * num/dv2;
                }
            }
            else
            {
                if(fabs(du2) >= fabs(dv2))
                {                                /* du2 is largest */
                    if(TOL_FAILURE(du2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtv2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/du2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtv2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/du2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[1] = out_params2[1] - v2const * num/du2;
                }
                else
                {                                /* dv2 is largest */
                    if(TOL_FAILURE(dv2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/dv2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/dv2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[0] = out_params2[0] - u2const * num/dv2;
                }
            }
       }
       else
       {
            if( fabs(dv1) >= fabs(du2))
            {
                if(fabs(dv1) >= fabs(dv2))
                {                                /* dv1 is largest */
                    if(TOL_FAILURE(dv1))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtv2, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[0] = out_params1[0] - u1const * num/dv1;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtv2, cp, 3 );
                    out_params2[0] = out_params2[0] + u2const * num/dv1;

                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[1] = out_params2[1] + v2const * num/dv1;
                }
                else
                {                                /* dv2 is largest */
                    if(TOL_FAILURE(dv2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/dv2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/dv2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[0] = out_params2[0] - u2const * num/dv2;
                }
            }
            else
            {
                if(fabs(du2) >= fabs(dv2))
                {                                /* du2 is largest */
                    if(TOL_FAILURE(du2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtv2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/du2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtv2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/du2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[1] = out_params2[1] - v2const * num/du2;
                }
                else
                {                                /* dv2 is largest */
                    if(TOL_FAILURE(dv2))
                    {
                        return(FALSE);          /* Tangential dis-continuity */
                    }
                    veccp( fdtu2, r, cp);
                    num = vecdp( fdtv1, cp, 3 );
                    out_params1[0] = out_params1[0] + u1const * num/dv2;

                    veccp( fdtu1, r, cp);
                    num = vecdp( fdtu2, cp, 3 );
                    out_params1[1] = out_params1[1] + v1const * num/dv2;

                    veccp( fdtv1, r, cp);
                    num = vecdp( fdtu1, cp, 3 );
                    out_params2[0] = out_params2[0] - u2const * num/dv2;
                }
            }
       }
       if(out_params1[0] < 0.0 )            /* Stop at surface boundaries */
       {
            out_params1[0] = 0.0;
       }
       else if( out_params1[0] > 1.0 )
       {
            out_params1[0] = 1.0;
       }
       if(out_params1[1] < 0.0 )
       {
            out_params1[1] = 0.0;
       }
       else if( out_params1[1] > 1.0 )
       {
            out_params1[1] = 1.0;
       }

       if(out_params2[0] < 0.0 )            /* Stop at surface boundaries */
       {
            out_params2[0] = 0.0;
       }
       else if( out_params2[0] > 1.0 )
       {
            out_params2[0] = 1.0;
       }
       if(out_params2[1] < 0.0 )
       {
            out_params2[1] = 0.0;
       }
       else if( out_params2[1] > 1.0 )
       {
            out_params2[1] = 1.0;
       }

                   
       bsp_set_mu_u(0);
       bsp_set_mu_v(0);
       if(!bsp_point_on_surface( surf1, out_params1[0], out_params1[1], surf_pnt1, fdtu1, fdtv1, fddtu1, fddtv1 ))
       {
                return(FALSE);
       }
       mu1_u = bsp_get_mu_u();
       u1const = (surf1->knots_u[mu1_u + 1]) - (surf1->knots_u[mu1_u]);
       mu1_v = bsp_get_mu_v();
       v1const = (surf1->knots_v[mu1_v + 1]) - (surf1->knots_v[mu1_v]);

       bsp_set_mu_u(0);
       bsp_set_mu_v(0);
       if(!bsp_point_on_surface( surf2, out_params2[0], out_params2[1], surf_pnt2, fdtu2, fdtv2, fddtu2, fddtv2 ))
       {
                return(FALSE);
       }
       mu2_u = bsp_get_mu_u();
       u2const = (surf2->knots_u[mu2_u + 2]) - (surf2->knots_u[mu2_u]);
       mu2_v = bsp_get_mu_v();
       v2const = (surf2->knots_v[mu2_v + 2]) - (surf2->knots_v[mu2_v]);

       vecdif(surf_pnt1, surf_pnt2, 3, rinc );    

       vecdif(rinc, r, 3, diff_vec);                   
       if((veclen( diff_vec, 3)) <= tol )
       {
            return( TRUE );
       }
       veccpy(rinc, 3, r);
    }
    return(TRUE);           /* Surfaces do not intersect */
}

/*      @(#)  256.1 date 12/16/89 ds_nurb_anal.c  */
/*+
 *
 *        Filename    : ds_nurb_anal.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:44
 *        Last change : 89/12/16 19:21:45
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_anal.c
 *
 *        This file contains the following routines:
 *
 *        create_arc_knots
 *        create_calc_arc_mid_cntrl_pnt
 *        create_disp_spline
 *        create_nurb_arc
 *        create_nurb_circle
 *        create_nurb_line  
 *        create_spin_spline
 *
-*/
           

#define ELLIPSE_POINTS   9
#define NO_ERRORS        0
#define POINT_TO_2D(A)   A[Z] = 0.0; A[H] = 1.0
#define CUR_CUR_BLOCK    7      
#define SUBDIV_TOL       1.0E-02
                
extern DS_NURB_SPLINE_PTR create_ellipse();


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 28 Sept 1988
 *
 */
void
box_and_line(spline,tol,lstart,lend)
                        /* Description   :- This function takes a spline and a line
                         *                  and calculates the new control points for
                         *                  the line so that it will intersect the
                         *                  spline at the required points.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;  /* <i> The spline to have its minmax box calculated */
double              tol;     /* <i> The tolerance to work to */
POINT               lstart;  /* <i/o> The start point of the line */
POINT               lend;    /* <i/o> The end point of the line */
{                                         
    POINT      min_point;
    POINT      max_point;     
    POINT      ldir;
    POINT      dir;                                    
    POINT      ipoint,ipoint1,ipoint2,ipoint3,ipoint4;
    double     s1,t1,s2,t2,s3,t3,s4,t4;
    double     max_dist;                 
    double     tol_lower,tol_upper;

    tol_lower = -tol;
    tol_upper = 1.0 + tol;


    vecdif(lend,lstart,CART,ldir);      /* Get the direction of the line */

    /* Calculate the min max box for the spline */
    pntminmax( spline->points, spline->n, min_point, max_point );

    /* Intersect the line with the four sides and see how many intersects we get */
    vecclr(dir,CART);
    dir[X] = max_point[X] - min_point[X];
    if (!line_x_line2D(lstart,ldir,min_point,dir,tol,ipoint1,&s1,&t1))
        t1 = 2.0;   /* Invalid value */

    /* The second intersect */
    dir[X] = -dir[X];
    if (!line_x_line2D(lstart,ldir,max_point,dir,tol,ipoint2,&s2,&t2))
        t2 = 2.0;   /* Invalid value */

    /* The third intersect */
    vecclr(dir,CART);
    dir[Y] = max_point[Y] - min_point[Y];
    if (!line_x_line2D(lstart,ldir,min_point,dir,tol,ipoint3,&s3,&t3))
        t3 = 2.0;   /* Invalid value */

    /* The fourth intersect */
    dir[Y] = -dir[Y];
    if (!line_x_line2D(lstart,ldir,max_point,dir,tol,ipoint4,&s4,&t4))
        t4 = 2.0;   /* Invalid value */

    /* Now work out the new start and end values for the line */
    /* Calculate the new start value */
    if ((s1 < 0.0) || (s2 < 0.0) || (s3 < 0.0) || (s4 < 0.0))
    {   /* New start value required */
        max_dist = 1.0;
        if ((s1 < max_dist) && (t1 >= tol_lower) && (t1 <= tol_upper))
        {
            max_dist = s1;
            veccpy(ipoint1,CART,ipoint);
        }
        if ((s2 < max_dist) && (t2 >= tol_lower) && (t2 <= tol_upper))
        {
            max_dist = s2;
            veccpy(ipoint2,CART,ipoint);
        }
        if ((s3 < max_dist) && (t3 >= tol_lower) && (t3 <= tol_upper))
        {
            max_dist = s3;
            veccpy(ipoint3,CART,ipoint);
        }
        if ((s4 < max_dist) && (t4 >= tol_lower) && (t4 <= tol_upper))
        {
            max_dist = s4;
            veccpy(ipoint4,CART,ipoint);
        }                                        
        if (max_dist != 1.0)
           veccpy(ipoint,CART,lstart);
    }
    if ((s1 > 1.0) || (s2 > 1.0) || (s3 > 1.0) || (s4 > 1.0))
    {   /* New end value required */
        max_dist = 0.0;
        if ((s1 > max_dist) && (t1 >= tol_lower) && (t1 <= tol_upper))
        {
           max_dist = s1;
           veccpy(ipoint1,CART,ipoint);
        }
        if ((s2 > max_dist) && (t2 >= tol_lower) && (t2 <= tol_upper))
        {
            max_dist = s2;
            veccpy(ipoint2,CART,ipoint);
        }
        if ((s3 > max_dist) && (t3 >= tol_lower) && (t3 <= tol_upper))
        {
            max_dist = s3;
            veccpy(ipoint3,CART,ipoint);
        }
        if ((s4 > max_dist) && (t4 >= tol_lower) && (t4 <= tol_upper))
        {
            max_dist = s4;
            veccpy(ipoint4,CART,ipoint);
        }      
        if (max_dist != 0.0)
           veccpy(ipoint,CART,lend);
    }                              
    lstart[Z] = 0.0;        /* Ensure both are on the XY plane */
    lstart[H] = 1.0;
    lend[Z] = 0.0;  
    lend[H] = 1.0;

}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
ellipse_trim(ecentre,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,spoint,senable,
                epoint,eenable,start_angle,end_angle,status)
                        /* Description   :- This function returns the point that is
                         *                  a given distance along the ellipse from
                         *                  a given point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
POINT   ecentre;         /* <i> The centre point of the first ellipse */
double  *emaj_rad;       /* <i> The major radius of the first ellipse */
double  *emin_rad;       /* <i> The minor radius of the first ellipse */
double  *einc_ang;       /* <i> The included angle for the first ellipse */
double  *estart_ang;     /* <i> The start angle of the first ellipse */
double  *eend_ang;       /* <i> The end angle of the first ellipse */             
POINT   spoint;          /* <i> New start point for trim if enabled */
BOOLEAN *senable;        /* <i> Enable flag for trimming to start */
POINT   epoint;          /* <i> New end point for trim if enabled */
BOOLEAN *eenable;        /* <i> Enable flag for trimming to end */
double  *start_angle;    /* <i> New computed start angle */
double  *end_angle;      /* <i> New computed end angle */
int32_t    *status;         /* <o> The status of the result */
                         /*     -ve     Error occurred */
                         /*     0       No errors      */
{                                                                           
    double               curve_parm;
    DS_NURB_SPLINE_PTR   ellipse;
    POINT                curve_pnt;               

    /* Generate the two ellipses as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }
                                 
    if (*senable)
    {   /* Trimming the start point */
       if (!point_on_spline(ellipse,spoint,curve_pnt,&curve_parm))
       {
           *status = FORTRAN_ERROR;
           return;
       }                                    
       *start_angle = curve_parm / 1.0 * C$2PI;
    }                                          

    if (*eenable)
    {   /* Trimming the end point */
       if (!point_on_spline(ellipse,epoint,curve_pnt,&curve_parm))
       {
           *status = FORTRAN_ERROR;
           return;
       }                                    
       *end_angle = curve_parm / 1.0 * C$2PI;
    }                                          

    *status = NO_ERRORS;

}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 12 Sept 1988
 *
 */                                                 
void                                                                        
dvtep(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,
                px1,py1,tang1x,tang1y,tang2x,tang2y,status)
                        /* Description   :- This function calculates the two tangents
                         *                  of the ellipse that pass through the given
                         *                  point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The ellipse is assumed to be centred around
                         *                  the origin.
                         *
                         */
double  *ecx;        /* <i> The centre point of the ellipse */
double  *ecy;        /* <i> The centre point of the ellipse */
double  *emaj_rad;      /* <i> The major radius of the ellipse */
double  *emin_rad;      /* <i> The minor radius of the ellipse */
double  *einc_ang;      /* <i> The included angle for the ellipse */
double  *estart_ang;    /* <i> The start angle of the ellipse */
double  *eend_ang;      /* <i> The end angle of the ellipse */
double  *px1;           /* <i> The X Co-ord of the point the tangents are to pass */
double  *py1;           /* <i> The Y Co-ord of the point the tangents are to pass */
double  *tang1x;        /* <i> The X Vector of the first tangent */
double  *tang1y;        /* <i> The Y Vector of the first tangent */
double  *tang2x;        /* <o> The X Vector of the second tangent */
double  *tang2y;        /* <o> The Y Vector of the second tangent */
int32_t    *status;        /* <o> Error indicator */
                        /*     -ve Error occurred */
                        /*     0   No errors */
{   
    BOOLEAN    flip_flag = FALSE;
    double     denom;
    double     numer;
    double     xsq;     /* The square of point[X] */
    double     pxy;     /* point_trans[X] * point_trans[Y] */
    double     sqmaj;   /* The square of the major radius */
    double     m1;
    double     m2;               
    POINT      point_trans;
    POINT      origin;
    VECTOR     z_axis;
    POINT      ecentre;
    POINT      point;
    VECTOR     tangent1;
    VECTOR     tangent2;
                     
    vecclr(point,CART);
    vecclr(ecentre,CART);
    vecclr(origin,CART);
    vecclr(z_axis,CART);
    vecclr(tangent1,CART);      /* Ensure the tangents are reset */
    vecclr(tangent2,CART);
    z_axis[Z] = 1.0;     
    ecentre[X] = *ecx;
    ecentre[Y] = *ecy; 
    point[X] = *px1;
    point[Y] = *py1;
                          
    /* Treat the ellipse as if centred about the origin so translate the point */
    /* if required */
    vecdif(point,ecentre,CART,point_trans);

    /* If the ellipse has been inclined we have to rotate the point back */
    calc_2Dtransformation(origin,z_axis,-(*einc_ang));
    rot_point(point_trans,point_trans);

                      
    xsq = point_trans[X] * point_trans[X];
    sqmaj = *emaj_rad * *emaj_rad;                             
    pxy = point_trans[X] * point_trans[Y];

    denom = 2 * (xsq - sqmaj);
    if (TOL_FAILURE(denom))
    {                        
        /* Rotate the point by 90 degrees and the ellipse as well */
        flip_flag = TRUE;       /* Signal a flip */
        calc_2Dtransformation(origin,z_axis,C$PIBY2);
        rot_point(point_trans,point_trans);

        xsq = point_trans[X] * point_trans[X];
        sqmaj = *emin_rad * *emin_rad;                             
        pxy = point_trans[X] * point_trans[Y];

        denom = 2 * (xsq - sqmaj);
        if (TOL_FAILURE(denom))
        {   /* The two tangent vectors are at right angles so work out the vectors */
            if (point_trans[X] >= 0)
            {
                if (point_trans[Y] >= 0)
                {
                    tangent1[X] = -1.0;
                    tangent1[Y] =  0.0;
                    tangent2[X] =  0.0;
                    tangent2[Y] = -1.0;
                }
                else
                {
                    tangent1[X] = 1.0;
                    tangent1[Y] = 0.0;
                    tangent2[X] = 0.0;
                    tangent2[Y] = 1.0;
                }                     
            }
            else
            {
                if (point_trans[Y] >= 0)
                {
                    tangent1[X] =  1.0;
                    tangent1[Y] =  0.0;
                    tangent2[X] =  0.0;
                    tangent2[Y] = -1.0;
                }
                else
                {
                    tangent1[X] = 1.0;
                    tangent1[Y] = 0.0;
                    tangent2[X] = 0.0;
                    tangent2[Y] = 1.0;
                }                     
            }      
            calc_2Dtransformation(origin,z_axis,-C$PIBY2);
            rot_point(tangent1,tangent1);
            rot_point(tangent2,tangent2);
            /* If the ellipse has been inclined we have to rotate the point back */
            calc_2Dtransformation(origin,z_axis,*einc_ang);
            rot_point(tangent1,tangent1);
            rot_point(tangent2,tangent2);
            *status = NO_ERRORS;
            *tang1x = tangent1[X];
            *tang1y = tangent1[Y];
            *tang2x = tangent2[X];
            *tang2y = tangent2[Y];
            return;
        }                                  
    }                             


    numer = (2 * pxy);
    numer *= numer;                         
    if (flip_flag)
        numer -= 4 * (xsq - sqmaj) * (point_trans[Y] * point_trans[Y] - *emaj_rad * *emaj_rad);
    else 
        numer -= 4 * (xsq - sqmaj) * (point_trans[Y] * point_trans[Y] - *emin_rad * *emin_rad);
    if (numer < 0.0)
    {
        *status = FORTRAN_ERROR;
        return;
    }
    numer = sqrt(numer);

    m1 = (2 * pxy - numer) / denom;
    m2 = (2 * pxy + numer) / denom;

    
    tangent1[X] = 1.0;
    tangent1[Y] = m1;

    tangent2[X] = 1.0;
    tangent2[Y] = m2;

    if (flip_flag)
    {   /* rotate the tangents back */
        calc_2Dtransformation(origin,z_axis,-C$PIBY2);
        rot_point(tangent1,tangent1);
        rot_point(tangent2,tangent2);
        rot_point(point_trans,point_trans);
    }   
    /* If the ellipse has been inclined we have to rotate the point back */
    calc_2Dtransformation(origin,z_axis,*einc_ang);
    rot_point(tangent1,tangent1);
    rot_point(tangent2,tangent2);
    rot_point(point_trans,point_trans);
                                                                        
    if ((point_trans[X] == *emaj_rad) && (point_trans[Y] < 0.0))
    {
        vecneg(tangent1,Z,tangent1);
        vecneg(tangent2,Z,tangent2);
    }
    else if ((point_trans[X] == -(*emaj_rad)) && (point_trans[Y] < 0.0))
    {
        vecneg(tangent1,Z,tangent1);
        vecneg(tangent2,Z,tangent2);
    }
    else if (point_trans[X] > *emaj_rad)
    {
        vecneg(tangent1,Z,tangent1);
        vecneg(tangent2,Z,tangent2);
    }                                       
    else if ((point_trans[X] < *emaj_rad) && (point_trans[X] > -(*emaj_rad)))
    {        
        if (point_trans[Y] > 0.0)
            vecneg(tangent1,Z,tangent1);
        else
            vecneg(tangent2,Z,tangent2);
    }
    *tang1x = tangent1[X];
    *tang1y = tangent1[Y];
    *tang2x = tangent2[X];
    *tang2y = tangent2[Y];

    *status = NO_ERRORS;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */
void
ellipse_circumference(spline,maj_rad,minor_rad,tol,length,status)
                        /* Description   :- This function calculates the circumference
                         *                  of an ellipse using the series method.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The tolerance must not be smaller than
                         *                  1.0E-10 as the factorial effect will
                         *                  overflow even the doubles as we compute.
                         *
                         */
DS_NURB_SPLINE_PTR   spline;    /* <i> The ellipse to have its circumference calc */
double               maj_rad;   /* <i> The major radius */
double               minor_rad; /* <i> The minor radius */
double               tol;       /* <i> The tolerance to work the circumference out to */
double               *length;   /* <o> The length of the circumference */
int32_t                 *status;   /* <o> The error status */
                                /*     -ve      Error occurred */
                                /*     0 or +ve Success        */
{   
    double    edenom;
    double    numerator;
    double    denominator;
    double    result;
    double    current_circ;     /* The current circumference */
    double    new_circ;         /* The newly calculated circumference */
    double    e;
    double    esquared;
    double    epower;                      
                                                                                    

    if (tol < 1.0E-10)
    {
        *status = FORTRAN_ERROR;
        return;
    }

    /* Calculate the eccentricity of the ellipse */
    e = (maj_rad * maj_rad - minor_rad * minor_rad);
    e = sqrt(e);
    e /= maj_rad;
    esquared = e * e;
    epower = esquared;
                                  
    new_circ = 1 - 0.5 * 0.5 * esquared;

    edenom = 1.0;     
    numerator = 1.0;
    denominator = 2.0;
    result = numerator / denominator;
                                             
    do 
    {   /* Circumferences differ by too much so keep going */
        current_circ = new_circ;
        epower *= esquared;
        edenom += 2.0;
        denominator += 2.0;
        numerator += 2.0;
                                    
        result *= numerator / denominator;

        new_circ -= (result * result) * (epower / edenom);
    }
    while (ABS(current_circ - new_circ) > tol);

    new_circ *= 2.0 * C$PI * maj_rad; 
                
    *length = new_circ;
    *status = NO_ERRORS;

}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
fred_dei0le(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,lsx,lsy,lex,ley,
                ipoint1x,ipoint1y,ipoint2x,ipoint2y,status)
                        /* Description   :- This function intersects a line and an 
                         *                  ellipse.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The line is treated as an unbounded line 
                         *                  for the intersection.
                         *
                         */
double  *ecx;        /* <i> The centre point of the ellipse */
double  *ecy;        /* <i> The centre point of the ellipse */
double  *emaj_rad;      /* <i> The major radius of the ellipse */
double  *emin_rad;      /* <i> The minor radius of the ellipse */
double  *einc_ang;      /* <i> The included angle for the ellipse */
double  *estart_ang;    /* <i> The start angle of the ellipse */
double  *eend_ang;      /* <i> The end angle of the ellipse */
double  *lsx;
double  *lsy;
double  *lex;
double  *ley;
double  *ipoint1x;
double  *ipoint1y;
double  *ipoint2x;
double  *ipoint2y;
int32_t    *status;        /* <o> The status of the result */
                        /*     -ve   Error occurred */
                        /*     0     No intersections */
                        /*     1 & 2 Number of intersections */
{
    DS_NURB_SPLINE_PTR   ellipse;
    DS_NURB_SPLINE_PTR   line;
    int32_t                 num;
    double               *intersects;          
    double               rot_angle;
    POINT                ecentre;
    POINT                lstart;
    POINT                lend;
    VECTOR               intersect_line;
    VECTOR               X_axis;
    VECTOR               rot_axis;
         
    vecclr(ecentre,CART);
    vecclr(lstart,CART);
    vecclr(lend,CART);
    ecentre[X] = *ecx;
    ecentre[Y] = *ecy;          
    lstart[X] = *lsx;
    lstart[Y] = *lsy;
    lend[X] = *lex;
    lend[Y] = *ley;
                    
    /* Set up the X_axis */
    X_axis[X] = 1.0;
    X_axis[Y] = 0.0;
    X_axis[Z] = 0.0;                
    rot_axis[X] = 0.0;
    rot_axis[Y] = 0.0;
    rot_axis[Z] = 1.0;

    /* Generate the ellipse and the line as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }

    /* Set up the intersect line */
    vecdif(lstart,lend,CART,intersect_line);

    /* Now calculate the angle */
    vecang(X_axis,intersect_line,CART,TOL,&rot_angle);

    /* Build the rotation matrix and rotate the intersect line */
    calc_2Dtransformation(ecentre,rot_axis,rot_angle);
    rot_2Dpoint(lstart,lstart);
    rot_2Dpoint(lend,lend);
    /* Calculate the new limits for the line to be intersected with the ellipse */
    /* Before generating the spline representing the line */
    box_and_line(ellipse,TOL,lstart,lend);

    if (!(line = create_nurb_line(lstart,lend)))
    {
       *status = FORTRAN_ERROR;                      
        return;
    }

    /* Now do the intersection of two nurbs */  
    if (!ds_nurb_curve_int(line,ellipse,SUBDIV_TOL,&num,&intersects))
    {   
        *status = FORTRAN_ERROR;
        return;
    }
                                          
    /* Generate the reverse rotation matrix */
    calc_2Dtransformation(ecentre,rot_axis,-rot_angle);

    /* Now pass back the intersection points */
    if (num >= 1)
    {   /* One intersection point */
        rot_2Dpoint(&intersects[1+X],&intersects[1+X]);
        *ipoint1x = intersects[1 + X];
        *ipoint1y = intersects[1 + Y];
    }

    if (num == 2)
    {   /* Second intersection point */                
        rot_2Dpoint(&intersects[1+X+CUR_CUR_BLOCK],&intersects[1+X+CUR_CUR_BLOCK]);
        *ipoint2x = intersects[1 + X + CUR_CUR_BLOCK];
        *ipoint2y = intersects[1 + Y + CUR_CUR_BLOCK];
    }

    *status = num;       /* Return number of intersections */

    delete_nurbspline(&ellipse);    /* Free ellipse memory */
    delete_nurbspline(&line);       /* Free line memory    */
    if (num != 0)
       ds_free(intersects);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
dei0ce(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,acx,acy,aradius,
                astart_ang,aend_ang,ip1x,ip1y,ip2x,ip2y,ip3x,ip3y,ip4x,ip4y,status)
                        /* Description   :- This function intersects an arc and an 
                         *                  ellipse.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                
double  *ecx;           /* <i> The X co-ord of the ellipse centre */
double  *ecy;           /* <i> The Y co-ord of the ellipse centre */
double  *emaj_rad;      /* <i> The major radius of the ellipse */
double  *emin_rad;      /* <i> The minor radius of the ellipse */
double  *einc_ang;      /* <i> The included angle for the ellipse */
double  *estart_ang;    /* <i> The start angle of the ellipse */
double  *eend_ang;      /* <i> The end angle of the ellipse */             
double  *acx;           /* <i> The X co-ord of the arc centre */
double  *acy;           /* <i> The Y co-ord of the arc centre */
double  *aradius;       /* <i> The radius of the arc */
double  *astart_ang;    /* <i> The start angle of the arc */
double  *aend_ang;      /* <i> The end angle of the arc */      
double  *ip1x;          /* <i> The First intersection */
double  *ip1y;          /* <i> The First intersection */
double  *ip2x;          /* <i> The Second intersection */
double  *ip2y;          /* <i> The Second intersection */
double  *ip3x;          /* <i> The Third intersection */
double  *ip3y;          /* <i> The Third intersection */
double  *ip4x;          /* <i> The Fourth intersection */
double  *ip4y;          /* <i> The Fourth intersection */
int32_t    *status;        /* <o> The status of the result */
                        /*     -ve   Error occurred */
                        /*     0     No intersections */
                        /*     1 & 2 Number of intersections */
{
    DS_NURB_SPLINE_PTR   ellipse;
    DS_NURB_SPLINE_PTR   arc;
    int32_t                 num;
    double               *intersects;
    double               len;
    POINT                ecentre;   /* Centre of the ellipse */
    POINT                acentre;   /* Centre of the arc */
    POINT                ipoint1;
    POINT                ipoint2;
    VECTOR               result;

    vecclr(ecentre,CART);
    vecclr(acentre,CART);

    ecentre[X] = *ecx;
    ecentre[Y] = *ecy;
    acentre[X] = *acx;
    acentre[Y] = *acy;

    /* Generate the ellipse and the arc as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }
                                       
    if (!(arc = create_nurb_arc(acentre,*aradius,*astart_ang,*aend_ang)))
    {
       *status = FORTRAN_ERROR;                      
        return;
    }

    /* Check for ellipse and arc being identical including incline angle */
    vecdif(ecentre,acentre,CART,result);
    len = veclen(result,CART);
    if ((*emaj_rad == *aradius) && (*emin_rad == *aradius)
        && (TOL_WITHIN(len,TOL)))
    {
        identical_ellipses(ellipse,arc,*estart_ang,*eend_ang,*einc_ang,
                           *astart_ang,*aend_ang,0,ipoint1,ipoint2,&num);

        *ip1x = ipoint1[X];
        *ip1y = ipoint1[Y];
        *ip2x = ipoint2[X];
        *ip2y = ipoint2[Y];
    }
    else
    {

        /* Now do the intersection of two nurbs */  
       if (!ds_nurb_curve_int(arc,ellipse,SUBDIV_TOL,&num,&intersects))
       {   
           *status = FORTRAN_ERROR;
           return;
       }

       /* Now pass back the intersection points */
       if (num >= 1)
       {   /* One intersection point */
           *ip1x = intersects[1 + X];
           *ip1y = intersects[1 + Y];
       }

       if (num >= 2)
       {   /* Second intersection point */
           *ip2x = intersects[1 + X + CUR_CUR_BLOCK];
           *ip2y = intersects[1 + Y + CUR_CUR_BLOCK];
       }                     

       if (num >= 3)
       {   /* Third intersection point */
           *ip3x = intersects[1 + X + 2 * CUR_CUR_BLOCK];
           *ip3y = intersects[1 + Y + 2 * CUR_CUR_BLOCK];
       }                     

       if (num == 4)
       {   /* Fourth intersection point */
           *ip4x = intersects[1 + X + 3 * CUR_CUR_BLOCK];
           *ip4y = intersects[1 + Y + 3 * CUR_CUR_BLOCK];
       }
    }                 

    *status = num;       /* Return number of intersections */

    delete_nurbspline(&ellipse);    /* Free ellipse memory */
    delete_nurbspline(&arc);       /* Free arc memory     */
    if (num != 0)
       ds_free(intersects);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
dei0ee(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,ecx1,ecy1,
                emaj_rad1,emin_rad1,einc_ang1,estart_ang1,eend_ang1,ip1x,ip1y,ip2x,
                ip2y,ip3x,ip3y,ip4x,ip4y,status)
                        /* Description   :- This function intersects an ellipse and an 
                         *                  ellipse.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */             
double  *ecx;           /* <i> The X co-ord of the ellipse centre */
double  *ecy;           /* <i> The Y co-ord of the ellipse centre */
double  *emaj_rad;      /* <i> The major radius of the first ellipse */
double  *emin_rad;      /* <i> The minor radius of the first ellipse */
double  *einc_ang;      /* <i> The included angle for the first ellipse */
double  *estart_ang;    /* <i> The start angle of the first ellipse */
double  *eend_ang;      /* <i> The end angle of the first ellipse */             
double  *ecx1;          /* <i> The X co-ord of the second ellipse centre */
double  *ecy1;          /* <i> The Y co-ord of the second ellipse centre */
double  *emaj_rad1;     /* <i> The major radius of the second ellipse */
double  *emin_rad1;     /* <i> The minor radius of the second ellipse */
double  *einc_ang1;     /* <i> The included angle for the second ellipse */
double  *estart_ang1;   /* <i> The start angle of the second ellipse */
double  *eend_ang1;     /* <i> The end angle of the second ellipse */             
double  *ip1x;          /* <i> The First intersection */
double  *ip1y;          /* <i> The First intersection */
double  *ip2x;          /* <i> The Second intersection */
double  *ip2y;          /* <i> The Second intersection */
double  *ip3x;          /* <i> The Third intersection */
double  *ip3y;          /* <i> The Third intersection */
double  *ip4x;          /* <i> The Fourth intersection */
double  *ip4y;          /* <i> The Fourth intersection */
int32_t    *status;        /* <o> The status of the result */
                        /*     -ve     Error occurred */
                        /*     0       No intersections */
                        /*     1 to 4  Number of intersections */
{
    DS_NURB_SPLINE_PTR   ellipse;
    DS_NURB_SPLINE_PTR   ellipse1;
    int32_t                 num;
    double               *intersects;                              
    double               len;
    POINT                result;
    POINT                ecentre;
    POINT                ecentre1;
    POINT                ipoint1;
    POINT                ipoint2;

    vecclr(ecentre,CART);
    vecclr(ecentre1,CART);

    ecentre[X] = *ecx;
    ecentre[Y] = *ecy;
    ecentre1[X] = *ecx1;
    ecentre1[Y] = *ecy1;

    /* Generate the two ellipses as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }
                                       
    if (!(ellipse1 = create_ellipse(ecentre1,*emin_rad1,*emaj_rad1,*estart_ang1,*eend_ang1,
                                    *einc_ang1)))
    {
        *status = FORTRAN_ERROR;
        return;
    }

    /* Check for ellipses being identical and being circles or part of circles */
    /* Or the incline angle being equal */
    vecdif(ecentre,ecentre1,CART,result);
    len = veclen(result,CART);
    if ((*emaj_rad == *emaj_rad1) && (*emin_rad == *emin_rad1)
        && ((*emaj_rad == *emin_rad) || (*einc_ang == *einc_ang1))
        && (TOL_WITHIN(len,TOL)))
    {
        identical_ellipses(ellipse,ellipse1,*estart_ang,*eend_ang,*einc_ang,
                           *estart_ang1,*eend_ang1,*einc_ang1,ipoint1,ipoint2,&num);

        *ip1x = ipoint1[X];
        *ip1y = ipoint1[Y];
        *ip2x = ipoint2[X];
        *ip2y = ipoint2[Y];
    }
    else                                
    {
        /* Now do the intersection of two nurbs */  
        if (!ds_nurb_curve_int(ellipse,ellipse1,SUBDIV_TOL,&num,&intersects))
        {   
             *status = FORTRAN_ERROR;
             return;
        }

        /* Now pass back the intersection points */
        if (num >= 1)
        {   /* One intersection point */
            *ip1x = intersects[1 + X];
            *ip1y = intersects[1 + Y];
        }

        if (num >= 2)
        {   /* Second intersection point */
            *ip2x = intersects[1 + X + CUR_CUR_BLOCK];
            *ip2y = intersects[1 + Y + CUR_CUR_BLOCK];
        }                     

        if (num >= 3)
        {   /* Third intersection point */
            *ip3x = intersects[1 + X + 2 * CUR_CUR_BLOCK];
            *ip3y = intersects[1 + Y + 2 * CUR_CUR_BLOCK];
        }                     

        if (num == 4)
        {   /* Fourth intersection point */
            *ip4x = intersects[1 + X + 3 * CUR_CUR_BLOCK];
            *ip4y = intersects[1 + Y + 3 * CUR_CUR_BLOCK];
        }
        if (num != 0)
           ds_free(intersects);
    }

    *status = num;       /* Return number of intersections */

    delete_nurbspline(&ellipse);    /* Free first ellipse memory  */
    delete_nurbspline(&ellipse1);   /* Free second ellipse memory */
}  
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
DDNTE(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,px,py,
                distance,normx,normy,ex,ey,status)
                        /* Description   :- This function returns the distance and
                         *                  the vector normal from the ellipse to a
                         *                  point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  *ecx;           /* <i> The X co-ord of the ellipse centre */
double  *ecy;           /* <i> The Y co-ord of the ellipse centre */
double  *emaj_rad;      /* <i> The major radius of the first ellipse */
double  *emin_rad;      /* <i> The minor radius of the first ellipse */
double  *einc_ang;      /* <i> The included angle for the first ellipse */
double  *estart_ang;    /* <i> The start angle of the first ellipse */
double  *eend_ang;      /* <i> The end angle of the first ellipse */             
double  *px;            /* <i> The point that the distance is to be measured from */
double  *py;            /* <i> The point the the distance is to be measured from */
double  *distance;      /* <o> The normal distance from the point to the ellipse */
double  *normx;         /* <o> The X direction of the normal from the ellipse to point*/
double  *normy;         /* <o> The Y direction of the normal from the ellipse to point*/
double  *ex;            /* <o> The X Co-ord on the ellipse */
double  *ey;            /* <o> The Y Co-ord on the ellipse */
int32_t    *status;        /* <o> The status of the result */
                        /*     -ve     Error occurred */
                        /*     0       No intersections */
                        /*     1       Number of intersections */
{
    double               uout;
    DS_NURB_SPLINE_PTR   ellipse;  
    POINT                curve_pnt;
    POINT                ecentre;
    POINT                point;
    VECTOR               normal;

    vecclr(point,CART);
    vecclr(ecentre,CART);

    ecentre[X] = *ecx;
    ecentre[Y] = *ecy; 
    point[X] = *px;
    point[Y] = *py;
    
    /* Generate the two ellipses as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }

    point_on_spline(ellipse,point,curve_pnt,&uout);

    /* Calculate the vector from the ellipse point to the point */
    vecdif(curve_pnt,point,CART,normal);

    /* Calculate the distance */
    *distance = veclen(normal,CART);

    *normx = normal[X];
    *normy = normal[Y];               
    *ex    = curve_pnt[X];
    *ey    = curve_pnt[Y];
                                             
    delete_nurbspline(&ellipse);
    *status = 1;
} 
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
dpdae(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,gpx,gpy,
                gdistance,opx,opy,status)
                        /* Description   :- This function returns the point that is
                         *                  a given distance along the ellipse from
                         *                  a given point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  *ecx;           /* <i> The X co-ord of the ellipse centre */
double  *ecy;           /* <i> The Y co-ord of the ellipse centre */
double  *emaj_rad;      /* <i> The major radius of the first ellipse */
double  *emin_rad;      /* <i> The minor radius of the first ellipse */
double  *einc_ang;      /* <i> The included angle for the first ellipse */
double  *estart_ang;    /* <i> The start angle of the first ellipse */
double  *eend_ang;      /* <i> The end angle of the first ellipse */             
double  *gpx;           /* <i> X co-ord of point distance is to be measured from */
double  *gpy;           /* <i> Y co-ord of point distance is to be measured from */
double  *gdistance;     /* <i> The distance from the point */
double  *opx;           /* <o> X co-ord of the new point */
double  *opy;           /* <o> Y co-ord of the new point */
int32_t    *status;        /* <o> The status of the result */
                        /*     -ve     Error occurred */
                        /*     0       No errors      */
{                                                                           
    double               new_parm;
    double               delta_parm;
    double               circumference;
    double               curve_parm;
    DS_NURB_SPLINE_PTR   ellipse;
    POINT                curve_pnt;               
    VECTOR               fdt;
    VECTOR               fddt;
    POINT                ecentre;
    POINT                gpoint;
    POINT                opoint;

    vecclr(gpoint,CART);
    vecclr(ecentre,CART);

    ecentre[X] = *ecx;
    ecentre[Y] = *ecy; 
    gpoint[X] = *gpx;
    gpoint[Y] = *gpy;

    /* Generate the two ellipses as nurbs */
    if (!(ellipse = create_ellipse(ecentre,*emin_rad,*emaj_rad,*estart_ang,*eend_ang,
                                    *einc_ang)))
    {
        *status = FORTRAN_ERROR;
        return;
    }

    /* Now calculate how far along in parameter space the given point is */
    if (!point_on_spline(ellipse,gpoint,curve_pnt,&curve_parm))
    {
        *status = FORTRAN_ERROR;
        return;
    }                               

    /* Work out the circumference of the ellipse */
    ellipse_circumference(ellipse,*emaj_rad,*emin_rad,1.0E-5,&circumference,status);
    if (*status != 0)
        return;     /* Error working out the circumference of the ellipse */

    /* Divide the distance by the circumference to get the change in parmater */
    delta_parm = *gdistance / circumference;

    new_parm = curve_parm + delta_parm;     /* Calculate new parmater value */

    /* Now find point at that paramter value */                             
    bsp_set_mu_u(0);
    bsp_point_on_spline( ellipse, new_parm, opoint, fdt, fddt);
    
    *opx = opoint[X];
    *opy = opoint[Y];

    *status = NO_ERRORS;

}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 12 Sept 1988
 *
 */
BOOLEAN                                                                         
point_on_spline(spline,point,curve_pnt,curve_parm)
                        /* Description   :- This function takes a point and a spline 
                         *                  and returns the point on the spline that
                         *                  is closest to it and the parmater value
                         *                  on the spline.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;       /* <i> The spline */
POINT               point;        /* <i> The point */                     
POINT               curve_pnt;    /* <o> The point on the spline closest to the point */
double              *curve_parm;  /* <o> The paramter value at the point on the spline */
{
    INT                  i;
    INT                  index;
    double               len;
    double               max_len;
    double               *vertex_map;
    POINT                cntrl_pnt;
    VECTOR               vec;

    max_len = 1.0E200;      /* A big number */

    /* Find the closest control point to the point and then work out the parm value */
    for (i = 0; i < spline->n ; i++)
    {
        pntextract(spline->points,i,cntrl_pnt);
        POINTH_TO_3D(cntrl_pnt,cntrl_pnt);
        vecdif(cntrl_pnt,point,CART,vec);     
        len = veclensq(vec,CART);      
        if (len < max_len)
        {
            max_len = len;
            index = i;
        }
    }                                
    
    if (!(vertex_map = GET_KNOT(spline->n)))
        return(FALSE);

    /* Generate a knot map corresponding to each vertex */
    os_knt_at_vertex(spline->knots,spline->n,spline->order,vertex_map);

    /* Now use the parm value at that control point to get the point on the spline */
    if (!new_pnt_spln(point,spline,vertex_map[index],1.0E-6,curve_parm,curve_pnt))
        return(FALSE);

    PUT(vertex_map);    /* Hand back the memory */

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 6 Sept 1988
 *
 */
DS_NURB_SPLINE_PTR                                                                        
create_ellipse(centrep,min_rad,maj_rad,start_angle,end_angle,incline_angle)
                        /* Description   :- This function creates an ellipse by 
                         *                  generating a nurb representation of it.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  All the angles are assumed to be in 
                         *                   radians.
                         *
                         */      
POINT   centrep;        /* <i> The centre point of the ellipse */
double  min_rad;        /* <i> The minimum radius */
double  maj_rad;        /* <i> The major radius */                         
double  start_angle;    /* <i> The start angle of the ellipse */
double  end_angle;      /* <i> The end angle of the ellipse */
double  incline_angle;  /* <i> The incline angle for rotating the ellipse off */
                        /*     the X-axis */
{     
    DS_NURB_SPLINE_PTR   spline;  
    DS_NURB_SPLINE_PTR   junk_spline;
    DS_NURB_SPLINE_PTR   ellipse_spline;
    INT                  i;
    double               *knot_ellipses;
    double               parm;
    double               angle;
    int32_t                 num_knots;
    POINT                point;
    VECTOR               rot_axis;    
    double               alpha;
    double               beta;
    double               root;
    double               c;
    double               factor;
    double               s;
    double               t;
    POINT                spoint;
    POINT                epoint;
    POINT                mpoint;
    POINT                tangp[9];
    VECTOR               tangd[9];
                            
    if ((TOL_FAILURE(min_rad)) || (TOL_FAILURE(maj_rad)))
        return(NULL);       /* Invalid radius sizes */

    /* Generate the knot vector with repeated knots */
    if (!create_arc_knots(ELLIPSE_POINTS,DEF_SPLINE_ORDER,4,&knot_ellipses,&num_knots))
        return(FALSE);

    if (!(spline = (DS_NURB_SPLINE_PTR) create_nurbspline(DEF_SPLINE_ORDER,
              ELLIPSE_POINTS,OPEN,USER_KNOTS,knot_ellipses,num_knots)))
        return(NULL);      /* Failed to create spline */
    
    PUT(knot_ellipses);

    /* Start the ellipse at the start angle required */
    for (i = 0, angle = start_angle; i < ELLIPSE_POINTS; i += 2)
    {       
       
       point[X] = maj_rad * cos(angle) + centrep[X];
       point[Y] = min_rad * sin(angle) + centrep[Y];
       point[Z] = 0.0;
       point[H] = 1.0;          
       if (!add_nspline_pnt(spline,point,i)) /* Add point point[i] */
           return(NULL);                                          
       angle += C$PIBY2;
    }
     
    /* Now calculate the mid points by calculating the tangents to the points and */
    /* intersecting them */
    for (i = 0, angle = start_angle; i < ELLIPSE_POINTS; i += 2)                     
    {                              
        alpha = cos(angle) / maj_rad;
        beta  = sin(angle) / min_rad;
        c = -1;
        angle += C$PIBY2;       /* Increase by 90 degrees */
        
        /* Convert to parametric form */
        root =  1.0 / ( alpha * alpha + beta * beta );
        factor = -c * root;
        tangp[i][X] = alpha * factor + centrep[X];
        tangp[i][Y] = beta * factor + centrep[Y];
        root = sqrt(root);
        tangd[i][X] = beta * root;
        tangd[i][Y] = -alpha * root;
    }                                                       

    for (i = 1; i < ELLIPSE_POINTS; i+= 2)
    {   /* Now calculate midpoints */ 
        if (!line_x_line2D(tangp[i - 1],tangd[i - 1],tangp[i + 1],tangd[i + 1],TOL,
                mpoint,&s,&t))
            return(FALSE);
       
        pntextract(spline->points,i-1,spoint);
        pntextract(spline->points,i+1,epoint);
        ellipse_midpoint(maj_rad,min_rad,centrep,spoint,epoint,mpoint);
        POINT3D_TO_H(mpoint,mpoint[H],mpoint);
        if (!add_nspline_pnt(spline,mpoint,i)) /* Add point point[i] */
            return(NULL);                                                     
    }

                                  
    if (!TOL_WITHIN((end_angle - C$2PI),TOL))
    {   /* Not 360 degrees */
        if (end_angle < start_angle)
            end_angle = C$2PI + end_angle - start_angle;
        else 
            end_angle = end_angle - start_angle;
    }
    else
        end_angle = end_angle - start_angle;

    /* We have now got a full ellipse now work out the start and end parameters */
    /* from the angles that are specified and then split the ellipse if required */
    if (!TOL_WITHIN((end_angle - C$2PI),TOL))
    {   /* Split the ellipse at the end */
        parm = end_angle / C$2PI;

        if (!oslo_split(spline,parm,&ellipse_spline,&junk_spline))
            return(FALSE);          /* Failed to split spline at parameter value */

        delete_nurbspline(&junk_spline);    /* Free the other split part of the spline */
        delete_nurbspline(&spline);         /* Free the original spline */
        spline = ellipse_spline;
    }

    /* If incline angle not zero then rotate spline by required amount */
    if (!TOL_WITHIN(incline_angle,TOL))
    {   /* Rotate spline by required amount */                
        /* Set up the rotation axis as the Z-axis */
        rot_axis[X] = 0.0;
        rot_axis[Y] = 0.0;
        rot_axis[Z] = 1.0;
        calc_2Dtransformation(centrep,rot_axis,incline_angle);

        for (i = 0; i < spline->n; i++)
        {
            pntextract(spline->points,i,point);     /* Extract the point */
            rot_h_point(point,point);               /* Rotate the point */
            pntdeposit(spline->points,i,point);     /* Deposit it back */
        }
    }

    return( spline );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 6 Sept 1988
 *
 */
BOOLEAN                                                                        
ellipse_midpoint(maj_rad,min_rad,centre,spoint,epoint,mpoint)
                        /* Description   :- This function calculates the midpoint of
                         *                  an ellipse arc.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                               
double        maj_rad;  /* <i> The major radius */
double        min_rad;  /* <i> The minor radius */
POINT         centre;   /* <i> Centre point */        
POINT         spoint;   /* <i> Start point */
POINT         epoint;   /* <i> End point */
POINT         mpoint;   /* <o> The middle point */
{   
    double    theta;
    double    lms;
    double    lsp;
    VECTOR    ms;
    VECTOR    sp;
    VECTOR    spcp;
    VECTOR    epcp;       
    POINT     s;             
    POINT     m;
            
    theta = C$PIBY4;

    /* Calculate the shoulder point */
    s[X] = maj_rad * cos(theta) + centre[X];
    s[Y] = min_rad * sin(theta) + centre[Y];
    s[Z] = 0.0;

    vecdif(spoint,centre,CART,spcp);
    vecdif(epoint,centre,CART,epcp);

    vecsca(0.5,spcp,CART,spcp);
    vecsca(0.5,epcp,CART,epcp);
    
    vecsum(spcp,epcp,CART,m);                        
    vecsum(m,centre,CART,m);                                        
    
    mpoint[Z] = 0.0;

    vecdif(s,m,CART,ms);
    lms = veclen(ms,CART);

    vecdif(mpoint,s,CART,sp);
    lsp = veclen(sp,CART);
                 
    mpoint[H] = lms / lsp;

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 10 June 1988
 *
 */
BOOLEAN
create_arc_knots(n,order,spans,knots,numknots)
                        /* Description   :- This function allocated and passes back the
                         *                  knot vector for a circular nurb. The values
                         *                  in the knot vector depend on the number
                         *                  of spans in the arc.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                     
INT     n;              /* <i> Number of points in spline */
INT     order;          /* <i> Order of the spline */
INT     spans;          /* <i> Number of spans that go to make up the arc */
double  *knots[];       /* <o> The resultant knot vector */
INT     *numknots;      /* <o> The number of knots in the knot vector */
{   
    double  knot_inc;
    double  new_knot;                         
    INT     num_knots;      /* Number of knots in the knot vector */
    INT     i;

    if ((n < order) || (order < 3) || (spans == 0))
        return(FALSE);

    num_knots = n + order;

    if (!(*knots = GET_KNOT(num_knots)))
        return(FALSE);
     
    knot_inc = 1.0 / (double) spans;
    new_knot = knot_inc;

    for (i = 0; i < order; i++)
        (*knots)[i] = 0.0;      /* Set first k elements to 0.0 */

    for (i = order ; i < n ; i += 2)
    {
        (*knots)[i] = new_knot;
        (*knots)[i + 1] = new_knot;
        new_knot += knot_inc;
    }                                                            
                             
    for (i = n; i < num_knots; i++)
         (*knots)[i] = 1.0;     /* Set last k elements to 1.0 */
                                         
    *numknots = num_knots; /* Return to calling routine number of knots */

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 8 June 1988
 *
 */
BOOLEAN                                                                        
create_calc_arc_mid_cntrl_pnt(sp,cp,ep,midpnt)
                        /* Description   :- This function takes an arc and calculates
                         *                  the midpoint of the two defining vectors.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The arc must not be greater than 90deg
                         *                  and the direction of the two points are
                         *                  not important so we do not have to
                         *                  specify the direction of it.
                         *
                         */                                                          
POINT    sp;         /* <i> Start point of the arc */
POINT    cp;         /* <i> Centre point of the arc */
POINT    ep;         /* <i> End point of the arc */
POINT    midpnt;     /* <o> The homogeneous point with the weighting factor */
{
    VECTOR    vec1;
    VECTOR    vec2;
    VECTOR    vec1cp2;       /* The cross product of v1 and v2 */
    VECTOR    vec1cpcp;      /* The cross product of vec1 and vec1cp2 */
    VECTOR    result;        /* The resultant vector */
    double    anglev1v2;     /* The angle between v1v2 */
                  
    /* Calculate the two vectors, check they are the same length and calc the crossp */
    vecdif(sp,cp,CART,vec1);
    vecdif(ep,cp,CART,vec2);                                                              

    if (!TOL_FAILURE(veclensq(vec1,CART)  - veclensq(vec2,CART)))
        return(FALSE);    /* Vectors must be the same length , they are not so error */

    veccp(vec1,vec2,vec1cp2);       /* Cross product */

    veccp(vec1cp2,vec1,vec1cpcp);   /* Cross product the cross product */

    if (!vecnrm(vec1cpcp,CART,TOL,vec1cpcp))    /* Normalise it to unit length */
    {   /* Return the start point */
        veccpy(sp,CART,midpnt);
        midpnt[H] = cos(C$2PI / ((2.0 * C$2PI) /  DEF_ARC_ANGLE));
        return(TRUE);
    }

    /* Calculate the angle between the two vectors */
    if (!vecang(vec1,vec2,CART,TOL,&anglev1v2))
    {   /* Points are coincident so return the start point */
        veccpy(sp,CART,midpnt);
        midpnt[H] = cos(C$2PI / ((2.0 * C$2PI) /  DEF_ARC_ANGLE));
        return(TRUE);
    }
                      
    /* Now work out the actual length of the vector by length = ((2 * angle)/pi) * rad) */
    vecsca(((2 * anglev1v2)/C$PI) * veclen(vec1,CART),vec1cpcp,CART,vec1cpcp);

    /* Add them together to get the resultant vector */
    vecsum(vec1cpcp,vec1,CART,result);

    /* Now add the resultant vector to the centre point to get the actual point */
    vecsum(cp,result,CART,midpnt);                               

    /* Work out the homogeneous value */
    midpnt[H] = cos(anglev1v2 / 2.0);

    return( TRUE );
}                       
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
DS_NURB_SURFACE_PTR                                                                        
create_disp_spline(spline,displacement,distance)
                        /* Description   :- This function lifts a spline through a 
                         *                  displacement vector by a specified distance
                         *
                         * Return status :- Surface ptr if successful
                         *                  NULL otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                 
DS_NURB_SPLINE_PTR spline;       /* <i> The spline to be used in creation of the surface */
VECTOR             displacement; /* <i> The displacemnt vector */
double             distance;     /* <i> The distance to be traversed */
{                   
    DS_NURB_SURFACE_PTR  surface;   /* The generated surface */
    VECTOR      disp_norm;      /* Displacemnt vector normalised */
    VECTOR      disp_vect;      /* Temporary displacemnt vector */
    VECTOR      disp_vec_h;     /* Displacemnt vector in homogeneous space */
    double      disp_dist;      /* The current distance to offset the spline */
    INT         numpnts;        /* The number of control pnts of the spline */
    INT         i,j;
    POINT       point;

    if (!spline)
        return(NULL);          /* No spline so return with error */

    if (TOL_WITHIN(veclen(displacement,CART),TOL))
        return(NULL);          /* Zero length vector of displacement */ 

    if (TOL_WITHIN(distance,TOL))
        return(NULL);          /* Zero distance */

    if (!enq_nurbspline(spline,&numpnts))
        return(NULL);          /* Could not enquire number of control points */

    /* Direction vector is V paramter space and spline is U parameter space */
    if (!(surface = create_nurbsurface(spline->order,STR_LIN_ORD,numpnts,STR_LIN_PNTS,
                spline->closed,OPEN,USER_KNOTS,DEF_KNOTS,spline->knots,NULL,
                enq_knotnum(spline->n,spline->order),0)))
        return(NULL);              

    vecnrm(displacement,CART,TOL,disp_norm);  /* Ensure it is normalised */

    for (j = 0,disp_dist = 0.0;j < STR_LIN_PNTS; j++)       /* Now generate the surface */
    {
        vecsca(disp_dist,disp_norm,CART,disp_vect);         /* Work out the offset */

        for (i = 0; i < numpnts; i++)
        {   /* Walk round spline extracting point and adding displacemnt vector */
            pntextract(spline->points,i,point);

            POINT3D_TO_H(disp_vect,point[H],disp_vec_h);    /* Convert to homo' space */

            vecsum(point,disp_vec_h,CART,point);            /* Generate the new displaced point */
            if (!add_nsurface_pnt(surface,point,i,j))
                return(NULL);                              /* Failed to add surface point */
        }
        disp_dist += (distance / (STR_LIN_PNTS - 1));
    }

    return( surface );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 8 June 1988
 *
 */
DS_NURB_SPLINE_PTR
create_nurb_arc(centre,radius,start_angle,end_angle)
                        /* Description   :- This function creates a spline that 
                         *                  represents an arc.
                         *
                         * Return status :- Spline Pointer if spline created
                         *                  Null pointer otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  
                         *
                         *
                         */  
POINT      centre;      /* <i> Centre point of the arc */
double     radius;      /* <i> Radius of the arc */
double     start_angle; /* <i> Start angle of arc */
double     end_angle;   /* <i> End angle of the arc */
{
    DS_NURB_SPLINE_PTR spline;
    DS_NURB_SPLINE_PTR arc_spline;
    DS_NURB_SPLINE_PTR junk_spline;
    VECTOR             z_axis;
    POINT              spoint;
    double             parm;

    vecclr(z_axis,CART);
    z_axis[Z] = 1.0;
    /* Work out where start point should be */
    spoint[X] = radius * cos(start_angle) + centre[X];
    spoint[Y] = radius * sin(start_angle) + centre[Y];                              
    spoint[Z] = 0.0;
    spoint[H] = 1.0;
    /* Generate a circle and then Split it at the required parameter value */
    if (!(spline = (DS_NURB_SPLINE_PTR) create_nurb_circle(centre,radius,z_axis,spoint)))
        return(FALSE);

    if (!TOL_WITHIN((end_angle - C$2PI),TOL))
    {   /* Not 360 degrees */
        if (end_angle < start_angle)
            end_angle = C$2PI - end_angle;
        else 
            end_angle = end_angle - start_angle;
    }
    else
        end_angle = end_angle - start_angle;

    if (!TOL_WITHIN((end_angle - C$2PI),TOL))
    {   /* Not a full circle so split it */
        parm = ABS(end_angle) / C$2PI;
        if (!oslo_split(spline,parm,&arc_spline,&junk_spline))
            return(FALSE);          /* Failed to split spline at parameter value */

        delete_nurbspline(&junk_spline);    /* Free the other split part of the spline */
        delete_nurbspline(&spline);         /* Free the original spline */
    }   
    else
        arc_spline = spline;

    return( arc_spline );    /* Return the arc spline */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 8 June 1988
 *
 */
DS_NURB_SPLINE_PTR
create_nurb_circle(cp,rad,axis,usp)
                        /* Description   :- This function creates a spline that 
                         *                  represents a circle
                         *
                         * Return status :- Spline Pointer if spline created
                         *                  Null pointer otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
POINT       cp;         /* <i> Centre point of the circle */
double      rad;        /* <i> Radius of the circle  */
VECTOR      axis;       /* <i> Axis of circle */
POINT       usp;        /* <i> User specifiable start point for circle            */
                        /*     Will really be called from inside create_nurb_arc with */
                        /*     this parm specified else passed as NULL.           */
{                   
    DS_NURB_SPLINE_PTR  spline;
    VECTOR       perpaxis;
    double       *knot_arcs;
    INT          whole_arcs;
    INT          i;
    INT          next_point;
    INT          num_knots;
    POINT        sp;
    POINT        midpnt;
    POINT        prev_point;
    POINT        rot_point; 
    POINT        point;
    PTR_POINT    pntchain;

    whole_arcs = (int) (C$2PI / DEF_ARC_ANGLE); 
    if (!(pntchain = GET_POINT((whole_arcs + 2) * 2)))
        return(NULL);
                    
    /* Calculate a point on the circumference of the circle */  
    if (!perp_vector(axis,perpaxis))
        return(FALSE);          /* Could not calculate the perpendicualr axis */
           
    if (!usp)   /* User not specified a start point for the circle */
    {           /* Calculate one ourself */
       vecsca(rad,perpaxis,CART,perpaxis); /* Calculate the axis times the radius */
       vecsum(perpaxis,cp,CART,sp);        /* We have now calculated a point on the circle*/
    }
    else
        veccpy(usp,3,sp);                  /* Copy in the User supplied point */

    POINT3D_TO_H(sp,1.0,rot_point);
    POINT3D_TO_H(sp,1.0,prev_point);

    for (i = 0,next_point = 0; i < whole_arcs; i++)
    {
        pntdeposit(pntchain,next_point++,prev_point); /* Put the first point in */
        /* Calculate the rotation matrix, taking the cp of v1 x v2 to get the axis of rot */
        calc_2Dtransformation(cp,axis,DEF_ARC_ANGLE);
        rot_h_point(rot_point,rot_point);       /* Rotate the point */
                                                                  
        /* Calculate the mid control point of the arc */
        create_calc_arc_mid_cntrl_pnt(prev_point,cp,rot_point,midpnt); 
                                                                   
        pntdeposit(pntchain,next_point++,midpnt);

        veccpy(rot_point,HOMO,prev_point);     /* Make curr point the prev point */
    } 

    POINT3D_TO_H(sp,1.0,point);
    pntdeposit(pntchain,next_point++,point); /* Put last point in */
          
    /* Generate the knot vector for the circle */
    if (!create_arc_knots(next_point,DEF_SPLINE_ORDER,whole_arcs,&knot_arcs,&num_knots))
        return(FALSE);
    /* Now create spline and add the points and free the point chain */
    if (!(spline = (DS_NURB_SPLINE_PTR) create_nurbspline(DEF_SPLINE_ORDER,next_point,OPEN,USER_KNOTS,knot_arcs,num_knots)))
        return(NULL);      /* Failed to create spline */

    PUT(knot_arcs);
                    
    for (i = 0; i < next_point; i++)
    {
        pntextract(pntchain,i,point);
        POINT3D_TO_H(point,point[H],point);
        if (!add_nspline_pnt(spline,point,i))
            return(NULL);           
    }

    PUT(pntchain);  /* Free the memory */

    return( spline );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 8 June 1988
 *
 */ 
DS_NURB_SPLINE_PTR
create_nurb_line(sp,ep)
                        /* Description   :- This function defines a straight line
                         *                  defined by a start point and end point as
                         *                  a nurb
                         *
                         * Return status :- Pointer to spline if created
                         *                  NULL pointer otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                       
POINT       sp;         /* <i> Start point of line */
POINT       ep;         /* <i> End point of line   */
{
    DS_NURB_SPLINE_PTR   spline;                                    
    POINT                hspoint;       /* Start point with homogeneous co-ord */
    POINT                hepoint;       /* End   point with homogeneous co-ord */


    if (!(spline = create_nurbspline(STR_LIN_ORD,STR_LIN_PNTS,OPEN,DEF_KNOTS,NULL,0)))
        return(NULL);
                                         
    POINT3D_TO_H(sp,1.0,hspoint);
    if (!add_nspline_pnt(spline,hspoint,0))     /* Add the start point */         
        return(NULL);

    POINT3D_TO_H(ep,1.0,hepoint);
    if (!add_nspline_pnt(spline,hepoint,1))     /* Add the end point */         
        return(NULL);
              
    return( spline );
}                                                        
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
DS_NURB_SURFACE_PTR                                                                        
create_spin_spline(spline,axis,point_on_axis,angle)
                        /* Description   :- This function spins a spline round a 
                         *                  specified axis and point by a specified
                         *                  number of degrees.
                         *
                         * Return status :- Surface ptr if successful
                         *                  NULL otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                    
DS_NURB_SPLINE_PTR spline;        /* <i> The spline to be spun */
VECTOR             axis;          /* <i> The axis the spline is to be spun around */
POINT              point_on_axis; /* <i> A point which the axis passes through */
double             angle;         /* <i> The angle the spline it to be spun */
{
    DS_NURB_SURFACE_PTR surface;        /* The newly generated surface */
    DS_NURB_SPLINE_PTR tmp_spline;      /* Spline of one row */
    POINT              point; 
    INT                numpnts;
    INT                tnumpnts;
    INT                i,j;

    if (!spline)
        return(NULL);      /* No spline so return error */

    if (TOL_WITHIN(veclen(axis,3),TOL))
        return(NULL);      /* Axis almost zero so error */

    if (TOL_WITHIN(angle,TOL))
        return(NULL);      /* Angle is basically zero */

    if (!enq_nurbspline(spline,&numpnts))
        return(NULL);          /* Could not enquire number of control points */

    pntextract(spline->points,0,point);
    POINTH_TO_3D(point,point);  /* Convert back to 3d */
    /* We need to generate a spline to get the knot vector for the V direction */
    /* of the surface */
    if (!(tmp_spline = create_nurb_arc(point_on_axis,axis,point,angle)))
        return(NULL);                                         

    if (!enq_nurbspline(tmp_spline,&tnumpnts))
        return(NULL);          /* Could not enquire number of control points */

    /* Spin around axis is V paramter space and spline is U parameter space */
    if (!(surface = create_nurbsurface(spline->order,tmp_spline->order,numpnts,tnumpnts,
                spline->closed,tmp_spline->closed,USER_KNOTS,USER_KNOTS,spline->knots,
                tmp_spline->knots, enq_knotnum(spline->n,spline->order),
                enq_knotnum(tmp_spline->n,tmp_spline->order))))
        return(NULL);          
                                 
    /* Walk down the spline generating an nurb arc at each one, copying the points in*/
    /* and then freeing the spline */
    for (i = 0; i < numpnts; i++)
    {                                
        pntextract(spline->points,i,point);
        /* We generate an arc spun round the axis to get the points for the V dir */
        /* of the surface */
        POINTH_TO_3D(point,point);  /* Convert back to 3d */
        if (!(tmp_spline = create_nurb_arc(point_on_axis,axis,point,angle)))
            return(NULL);                                         

        for (j = 0; j < tnumpnts; j++)
        {
            pntextract(tmp_spline->points,j,point);      /* Get the point */
            if (!add_nsurface_pnt(surface,point,i,j))
                return(NULL);          /* Could not add point to surface */
        } 
        delete_nurbspline(&tmp_spline);        /* Free the spline */
    }
    return( surface );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */
BOOLEAN                                                                        
spline_length(spline,length)
                        /* Description   :- This function returns the length of the 
                         *                  spline. The method used is similar to the
                         *                  area method except it caclulates the 
                         *                  length of each patch.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;   /* <i> The spline to be measured */
double              *length;  /* <o> The length of the spline */
{   
    INT       i;
    INT       j;
    INT       num_spans;
    POINT     sum;         
    BOOLEAN   success;
    POINT     point;       
    double    ustart;
    double    uend;
    double    length_of_span;
    double    total_length;                 
    double    h;



    vecclr(sum,CART);
    for (i = 0; i < spline->n; i++)
    {
        pntextract(spline->points,i,point); 
        POINTH_TO_3D(point,point);
        vecsum(sum,point,CART,sum);
    }
    vecsca(1.0/(double)spline->n, sum, CART, sum);   /* Calculate a local origin */

    for (i = 0; i < spline->n; i++)
    {
        pntextract(spline->points,i,point);
        h = point[H];
        POINTH_TO_3D(point,point);
        vecdif(point,sum,CART,point);
        POINT3D_TO_H(point,h,point);
        pntdeposit(spline->points,i,point);
    }
                                                             
    /* Calculate the number of spans */
    if ((num_spans = spline->n - (spline->order - 1)) < 1)   
        return(FALSE);

    ustart = spline->knots[spline->order -1];
    uend = spline->knots[spline->order];
 
    success = TRUE;     
    total_length = 0.0;

    for (i = 0, j = spline->order + 1; i < num_spans && success; i++, j++) /* For each span */
    {
        if (!spline_span_length(spline,ustart,uend,&length_of_span))
            success = FALSE;
        else
        {
            /* Add the length of the span */
            total_length += length_of_span;
        }                                   
        ustart = uend;
        uend   = spline->knots[j];
    }

    *length = total_length;

    for (i = 0; i < spline->n; i++)
    {
        pntextract(spline->points,i,point);
        h = point[H];
        POINTH_TO_3D(point,point);
        vecsum(sum,point,CART,point);
        POINT3D_TO_H(point,h,point);
        pntdeposit(spline->points,i,point);
    }

    return(success);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */
BOOLEAN                                                                        
spline_span_length(spline,ustart,uend,span_length)
                        /* Description   :- This function calculates the length of one
                         *                  of the spans.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                              
DS_NURB_SPLINE_PTR  spline;  /* <i> The spline to be measured */
double              ustart;  /* <i> Start parameter of the span */
double              uend;    /* <i> End parameter of the span */
double              *span_length; /* <o> The length of the span */
{
    double      diff;
    double      weight;
    double      mid_point;
    double      const1;
    double      u0;
    double      u1;
    double      u2;
    double      lfdu0;
    double      lfdu1;
    double      lfdu2;
    VECTOR      fu0;
    VECTOR      fu1;
    VECTOR      fu2;
    VECTOR      fdu0;
    VECTOR      fdu1;
    VECTOR      fdu2;
    VECTOR      fddu;

    diff = uend - ustart;
    weight = diff / 2.0;
    mid_point = ustart + weight;
    const1 = sqrt(3.0 / 5.0);

    u0 = mid_point - weight * const1;
    u1 = mid_point;
    u2 = mid_point + weight * const1;

    bsp_set_mu_u(0);

    bsp_point_on_spline(spline,u0,fu0,fdu0,fddu);    /* Calculate the value at u0 */
    bsp_point_on_spline(spline,u1,fu1,fdu1,fddu);    /* Calculate the value at u1 */
    bsp_point_on_spline(spline,u2,fu2,fdu2,fddu);    /* Calculate the value at u2 */

    lfdu0 = veclen(fdu0,CART);
    lfdu1 = veclen(fdu1,CART);
    lfdu2 = veclen(fdu2,CART);

    if (diff > 0.0)
        *span_length = (weight / (9.0 * diff)) * (5.0 * (lfdu0 + lfdu2)  + 8.0 * lfdu1);
    else
        *span_length = 0.0;

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 20 Sept 1988
 *
 */                                                                             
BOOLEAN
identical_ellipses(ellipse1,ellipse2,e1start,e1end,e1inc,e2start,e2end,e2inc,ipoint1,ipoint2,num)
                        /* Description   :- This function works out the intersection
                         *                  of the two ellipses when they lie on top
                         *                  of each other.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                               
DS_NURB_SPLINE_PTR ellipse1;    /* <i> The first ellipse */
DS_NURB_SPLINE_PTR ellipse2;    /* <i> The second ellipse */     
double             e1start;     /* <i> */
double             e1end;       /* <i> */                                    
double             e1inc;       /* <i> */
double             e2start;     /* <i> */
double             e2end;       /* <i> */
double             e2inc;       /* <i> */
POINT              ipoint1;     /* <o> The first intersection point */
POINT              ipoint2;     /* <o> The second intersection point */
int32_t               *num;        /* <o> The number of intersections */
{
                                         
    e1start += e1inc;
    e1end += e1inc;
    e2start += e2inc;
    e2end += e2inc;

    if (e1end < e1start)
        e1end += C$2PI;
    if (e2end < e2start)
        e2end += C$2PI;

    *num = 0;            
    if (e1start >= e2start && e1start <= e2end)
    {   /* One intersection */
        if (*num == 0)      
            pntextract(ellipse1->points,0,ipoint1);
        else 
            pntextract(ellipse1->points,0,ipoint2);
        (*num)++;
    }
    if (e1end >= e2start && e1end <= e2end)
    {   /* Another intersection */   
        if (*num == 0)      
            pntextract(ellipse1->points,ellipse1->n-1,ipoint1);
        else 
            pntextract(ellipse1->points,ellipse1->n-1,ipoint2);
        (*num)++;
    }
    if (e2start >= e1start && !TOL_WITHIN((e2start - e1start),TOL)
           && e2start <= e1end && *num <= 1)
    {   /* Another intersection */             
        if (*num == 0)      
            pntextract(ellipse2->points,0,ipoint1);
        else 
            pntextract(ellipse2->points,0,ipoint2);
        (*num)++;
    }
    if (e2end >= e1start && e2end <= e1end && *num <= 1)
    {   /* Another intersection */
        if (*num == 0)      
            pntextract(ellipse2->points,ellipse2->n-1,ipoint1);
        else 
            pntextract(ellipse2->points,ellipse2->n-1,ipoint2);
        (*num)++;
    }                                   
                                        
    if (*num >= 1)
        POINTH_TO_3D(ipoint1,ipoint1);
    if (*num == 2)
        POINTH_TO_3D(ipoint2,ipoint2);
}
/*
 *===================================================================================*
 *
 *                                                           Author :: Jim Mulhearn
 *                                                           Date   :: 11th-October-1988
 *
 */
BOOLEAN
cofg_of_spline( spline, c_of_g )
                        /* Description   :- This function calculates the centre of
                         *                  gravity of a spline.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The resultant area is positive when the
                         *                  spline is defined clockwise.
                         *
                         *                  The CofG of open splines is calculated by
                         *                  subtending the spline to the origin.
                         *
                         *                  Gaussian Quadrature is used to calculate
                         *                  the numerical integration.
                         *
                         */
DS_NURB_SPLINE_PTR  spline;  /* <i> The spline pointer */
POINT               c_of_g;  /* <o> The resultant centre of gravity */
{
    INT       i;
    INT       j;
    INT       num_spans;
    POINT     sum;         
    BOOLEAN   success;
    POINT     point;       
    POINT     cofg_span;       
    double    ustart;
    double    uend;
    double    area_of_span;
    double    total_area;                 
    double    h;
    VECTOR    temp;
    VECTOR    temp1;

    vecclr(sum,CART);                                /* Defaults */
    vecclr(c_of_g,CART);
    total_area = 0.0;
    success = TRUE;     

    if((num_spans = spline->n - (spline->order - 1)) < 1) /* Calculate the number of */
    {                                                     /* spans */
        return(FALSE);
    }
    for (i = 0; i < spline->n; i++)
    {
        pntextract(spline->points,i,point); 
        POINTH_TO_3D(point,point);
        vecsum(sum,point,CART,sum);
    }
    vecsca(1.0/(double)spline->n, sum, CART, sum);   /* Calculate a local origin */

    for (i = 0; i < spline->n; i++)
    {                                                /* Move control vertices relative */
        pntextract(spline->points,i,point);          /* to the origin */
        h = point[H];
        POINTH_TO_3D(point,point);
        vecdif(point,sum,CART,point);
        POINT3D_TO_H(point,h,point);
        pntdeposit(spline->points,i,point);
    }
    ustart = spline->knots[spline->order -1];
    uend = spline->knots[spline->order];

    for (i = 0, j = spline->order + 1; i < num_spans && success; i++, j++) /* For each span */
    {
        if (!cofg_of_span(spline,ustart,uend,&area_of_span,cofg_span))
        {
            success = FALSE;
        }
        else
        {
            vecsca( total_area, c_of_g, CART, temp );
            vecsca( area_of_span, cofg_span, CART, temp1 );
            vecdif( temp, temp1, CART, temp );

            total_area -= area_of_span;     /* Add negative areas, subtract positive */
        }                                   /* areas */
        if( TOL_FAILURE(total_area))
        {
            success = FALSE;
        }
        else
        {
            vecsca( 1.0/total_area, temp, CART, c_of_g );
        }

        ustart = uend;
        uend   = spline->knots[j];
    }
    for (i = 0; i < spline->n; i++)
    {
        pntextract(spline->points,i,point);
        h = point[H];
        POINTH_TO_3D(point,point);
        vecsum(sum,point,CART,point);
        POINT3D_TO_H(point,h,point);
        pntdeposit(spline->points,i,point);
    }
    vecsum(sum,c_of_g,CART,c_of_g);         /* Relocate C_of_g to absolute origin */

    return(success);
}
/*
 *===================================================================================*
 *
 *                                                           Author :: Jim Mulhearn
 *                                                           Date   :: 12th-October-1988
 *
 */
BOOLEAN                                                                        
cofg_of_span(spline,ustart,uend,span_area,local_cofg)
                        /* Description   :- This function calculates the c_of_g of
                         *                  a span.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                              
DS_NURB_SPLINE_PTR  spline;     /* <i> The spline to be measured */
double              ustart;     /* <i> Start parameter of the span */
double              uend;       /* <i> End parameter of the span */
double              *span_area; /* <o> The area subtended under the span */
POINT               local_cofg; /* <o> The C_of_G of the span */
{
    double      diff;
    double      weight;
    double      mid_point;
    double      const1;
    double      u0;
    double      u1;
    double      u2;
    VECTOR      fu0;
    VECTOR      fu1;
    VECTOR      fu2;
    VECTOR      fdu0;
    VECTOR      fdu1;
    VECTOR      fdu2;
    VECTOR      fddu;
    VECTOR      f_of_u0;
    VECTOR      f_of_u1;
    VECTOR      f_of_u2;
    VECTOR      cross_u0;
    VECTOR      cross_u1;
    VECTOR      cross_u2;
    VECTOR      sum;
    VECTOR      temp;

    diff = uend - ustart;
    weight = diff / 2.0;
    mid_point = ustart + weight;
    const1 = sqrt(3.0 / 5.0);

    u0 = mid_point - weight * const1;
    u1 = mid_point;
    u2 = mid_point + weight * const1;

    bsp_set_mu_u(0);
    bsp_point_on_spline(spline,u0,fu0,fdu0,fddu);    /* Calculate the value at u0 */
    bsp_point_on_spline(spline,u1,fu1,fdu1,fddu);    /* Calculate the value at u1 */
    bsp_point_on_spline(spline,u2,fu2,fdu2,fddu);    /* Calculate the value at u2 */

    veccp( fu0, fdu0, cross_u0 );
    veccp( fu1, fdu1, cross_u1 );
    veccp( fu2, fdu2, cross_u2 );

    vecsca( 5.0, cross_u0, CART, f_of_u0 );
    vecsca( 8.0, cross_u1, CART, f_of_u1 );
    vecsca( 5.0, cross_u2, CART, f_of_u2 );

    vecclr(sum, CART );
    vecsum( f_of_u0, f_of_u1, CART, sum );
    vecsum( sum, f_of_u2, CART, sum );

    vecsca( weight/18.0, sum, CART, temp );         /* 18.0 from 9.0 and the 1/2 on the */
                                                    /* integral */
    *span_area = temp[2];

    vecsca( 5.0 * cross_u0[Z], fu0, CART, f_of_u0 );/* Now calculate the C_of_G */
    vecsca( 8.0 * cross_u1[Z], fu1, CART, f_of_u1 );
    vecsca( 5.0 * cross_u2[Z], fu2, CART, f_of_u2 );

    vecclr(sum, CART );
    vecsum( f_of_u0, f_of_u1, CART, sum );
    vecsum( sum, f_of_u2, CART, sum );

    vecsca( weight/9.0, sum, CART, sum ); 

    const1 = 3.0 * (*span_area);

    if(TOL_FAILURE(*span_area))                     /* If there is no area use the mid */
    {                                               /* point of the span to calculate */
        vecsca( 2.0/3.0, fu1, CART, local_cofg );   /* the C_of_G */
    }
    else
    {
        vecsca( 1.0/const1, sum, CART, local_cofg );
    }
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 9 Sept 1988
 *
 */                                                 
void                                                                        
dei0le(ecx,ecy,emaj_rad,emin_rad,einc_ang,estart_ang,eend_ang,lsx,lsy,lex,ley,
                ipoint1x,ipoint1y,ipoint2x,ipoint2y,status)
                        /* Description   :- This function intersects a line and an 
                         *                  ellipse by the analytic method. .
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The line is treated as an unbounded line 
                         *                  for the intersection.           
                         *                  The method is described in the book
                         *                  "Practical geometry & graphics "
                         *                  by Harrison and Baxandall which can be
                         *                  found in the Andersonian Library in
                         *                  the Non-dewey classification. 
                         *                  Page 96
                         *
                         */
double  *ecx;           /* <i> The centre point of the ellipse */
double  *ecy;           /* <i> The centre point of the ellipse */
double  *emaj_rad;      /* <i> The major radius of the ellipse */
double  *emin_rad;      /* <i> The minor radius of the ellipse */
double  *einc_ang;      /* <i> The included angle for the ellipse */
double  *estart_ang;    /* <i> The start angle of the ellipse     */
double  *eend_ang;      /* <i> The end angle of the ellipse       */
double  *lsx;           /* <i> Start X of line */
double  *lsy;           /* <i> Start Y of line */
double  *lex;           /* <i> End X of line   */
double  *ley;           /* <i> End Y of line   */                            
double  *ipoint1x;      /* <o> The X Co-ord of the first intersection point  */
double  *ipoint1y;      /* <o> The Y Co-ord of the first intersection point  */
double  *ipoint2x;      /* <o> The X Co-ord of the second intersection point */
double  *ipoint2y;      /* <o> The Y Co-ord of the second intersection point */
int32_t    *status;        /* <o> The status of the result      */
                        /*     -ve   Error occurred          */
                        /*     0     No intersections        */
                        /*     1 & 2 Number of intersections */
{
    POINT      circle;
    POINT      ldir;
    POINT      lstart,lend;                                  
    POINT      l,m,l1,m1,L,M,L1,M1,P,Q,P1,Q1,D,D0,D1,H0,H1,E,E0,E1,K0,K1;
    POINT      x_dir,y_dir,z_dir,len,len1,vec;
    POINT      spoint,epoint;
    double     s,t,detx,dety,equad,squad,pquad,qquad;

               
    *status = 0;        /* Show no intersections */
    lstart[X] = *lsx;
    lstart[Y] = *lsy;      
    lend[X] = *lex;
    lend[Y] = *ley;
    circle[X] = *ecx;
    circle[Y] = *ecy;

    vecclr(x_dir,HOMO);
    x_dir[X] = 1.0;

    vecclr(y_dir,HOMO);
    y_dir[Y] = 1.0;                    

    vecclr(z_dir,HOMO);
    z_dir[Z] = 1.0;
        
    /* Check to see if ellipse is inclined if so then rotate the line by the */
    /* negative amount */
    if (*einc_ang != 0.0)
    {   /* Rotate line */
        calc_2Dtransformation(circle,z_dir,-(*einc_ang));
        rot_2Dpoint(lstart,lstart);
        rot_2Dpoint(lend,lend);
    }

    /* Work out the direction of the line first of all */
    vecdif(lend,lstart,2,ldir);
       
    /* Check to see if the line is parallel to either the major or minor axes */
    /* First of all check to see if parallel to the X axis */
    detx = (ldir[X] * y_dir[Y]) - (y_dir[X] * ldir[Y]);
    dety = (ldir[X] * x_dir[Y]) - (x_dir[X] * ldir[Y]);
    if (ABS(detx) < TOL)
    {   /* Line is parallel to X-axis, therefore perpindicular to Y-axis */
        /* Work out H */
        if (!line_x_circle2D(lstart,ldir,circle,*emaj_rad,TOL,H0,H1))
            return;

        /* Join C H and get D0 (intersection point of minor circle */
        vecdif(H0,circle,2,ldir);
        if (!line_x_circle2D(circle,ldir,circle,*emin_rad,TOL,D0,D1))
            return;                                                

        vecdif(H0,D0,2,len);
        vecdif(H0,D1,2,len1);

        if (veclensq(len,2) > veclensq(len1,2))
            veccpy(D1,2,D0); /* Make D0 the shorter one */

        /* Get the points of intersection */
        if (!line_x_line2D(D0,x_dir,H0,y_dir,TOL,D,&s,&t))
            return;                     
        
        /* Get the perp axis from the intersection point to the Major axis */      
        D[Z] = 0.0; 
        D[H] = 1.0;
        circle[Z] = 0.0;
        circle[H] = 1.0;
        axperp(D,circle,x_dir,vec,D1);                                       
        vecneg(vec,2,vec);
        vecsum(D1,vec,2,D1);
                                                            
        /* Copy the two intersection points into P and Q */
        veccpy(D,2,P);
        veccpy(D1,2,Q);
    } 
    else if (ABS(dety) < TOL)
    {   /* Line is parallel to Y-axis, therefore perpindicular to X-axis */
        /* Work out K */
        if (!line_x_circle2D(lstart,ldir,circle,*emin_rad,TOL,K0,K1))
            return;

        /* Join C K and get D0 (intersection point on major circle */
        vecdif(K0,circle,2,ldir);
        if (!line_x_circle2D(circle,ldir,circle,*emaj_rad,TOL,E0,E1))
            return;                                                

        vecdif(K0,E0,2,len);
        vecdif(K0,E1,2,len1);

        if (veclensq(len,2) > veclensq(len1,2))
            veccpy(E1,2,E0); /* Make D0 the shorter one */

        /* Get the points of intersection */
        if (!line_x_line2D(K0,x_dir,E0,y_dir,TOL,E,&s,&t))
            return;                     
        
        /* Get the perp axis from the intersection point to the Major axis */      
        E[Z] = 0.0; 
        E[H] = 1.0;
        circle[Z] = 0.0;
        circle[H] = 1.0;
        axperp(E,circle,y_dir,vec,E1);                                       
        vecneg(vec,2,vec);
        vecsum(E1,vec,2,E1);

        /* Copy the two intersection points into P and Q */
        veccpy(E,2,P);
        veccpy(E1,2,Q);
    }
    else
    {   /* Intersection line is not parallel */
       /* Work out L and M */
       if (!line_x_circle2D(lstart,ldir,circle,*emaj_rad,TOL,L,M))
           return;              

       /* Work out l and m */                                    
       vecdif(L,circle,2,ldir);
       if (!line_x_circle2D(circle,ldir,circle,*emin_rad,TOL,l,l1))
           return;

       vecdif(M,circle,2,ldir);
       if (!line_x_circle2D(circle,ldir,circle,*emin_rad,TOL,m,m1))
           return;
            
       vecdif(L,l,2,len);
       vecdif(L,l1,2,len1);

       if (veclensq(len,2) > veclensq(len1,2))
           veccpy(l1,2,l); /* Make l the shorter one */

       vecdif(M,m,2,len);
       vecdif(M,m1,2,len1);
       if (veclensq(len,2) > veclensq(len1,2))
           veccpy(m1,2,m); /* make m the shorter one */

       /* Intersect the a horizontal line through L with a vertical line through l */
       if (!line_x_line2D(L,x_dir,l,y_dir,TOL,L1,&s,&t))
           return;

       if (!line_x_line2D(M,x_dir,m,y_dir,TOL,M1,&s,&t))
           return;

       /* Now intersect the line L1 M1 with the minor circle */
       vecdif(L1,M1,2,ldir);
       if (!line_x_circle2D(L1,ldir,circle,*emin_rad,TOL,P1,Q1))
           return;              

       /* Now get the points of intersection */
       vecdif(L,M,2,ldir);
       if (!line_x_line2D(P1,x_dir,L,ldir,TOL,P,&s,&t))
           return;

       if (!line_x_line2D(Q1,x_dir,L,ldir,TOL,Q,&s,&t))
           return;
    }
     
    /* At this point no matter which method has been used to calculate the */
    /* intersections the points are in P and Q */
    /* If ellipse is inclined rotate the intersection points back by the incline angle */
    if (*einc_ang != 0.0)
    {   /* Rotate line */
        calc_2Dtransformation(circle,z_dir,(*einc_ang));
        rot_2Dpoint(Q,Q);
        rot_2Dpoint(P,P);
    }
                                                          
    /* If its a full ellipse then we return both points */
    if ((TOL_WITHIN(*estart_ang,1.0E-04))
           && (TOL_WITHIN(((*eend_ang) - C$2PI),1.0E-04)))
    {
           *ipoint1x = Q[X];
           *ipoint1y = Q[Y];
           *ipoint2x = P[X];
           *ipoint2y = P[Y];
           *status = 2;
    }
    else
    {  /* Check to see which intersection points are valid */
       spoint[X] = circle[X] + *emaj_rad * cos(*estart_ang + (*einc_ang));
       spoint[Y] = circle[Y] + *emin_rad * sin(*estart_ang + (*einc_ang));
       epoint[X] = circle[X] + *emaj_rad * cos(*eend_ang   + (*einc_ang));
       epoint[Y] = circle[Y] + *emin_rad * sin(*eend_ang   + (*einc_ang));

       point_quadrant(circle,P,TOL,&pquad);
       point_quadrant(circle,Q,TOL,&qquad);
       point_quadrant(circle,spoint,TOL,&squad);
       point_quadrant(circle,epoint,TOL,&equad);

       if (squad < equad)
       {
           if ((pquad >= squad) && (pquad <= equad))
           {
               *ipoint1x = P[X];
               *ipoint1y = P[Y];
               (*status)++;
           }
           if ((qquad >= squad) && (qquad <= equad))
           {
               if (*status == 0)
               {
                   *ipoint1x = Q[X];
                   *ipoint1y = Q[Y];
                   (*status)++;
               }
               else
               {
                   *ipoint2x = Q[X];
                   *ipoint2y = Q[Y];
                   (*status)++;
               }
           }                
       }
       else
       {
           if ((pquad >= equad) && (pquad <= squad))
           {
               *ipoint1x = P[X];
               *ipoint1y = P[Y];
               (*status)++;
           }
           if ((qquad >= equad) && (qquad <= squad))
           {               
               if (*status == 0)
               {
                   *ipoint1x = Q[X];
                   *ipoint1y = Q[Y];
                   (*status)++;
               }
               else
               {
                   *ipoint2x = Q[X];
                   *ipoint2y = Q[Y];
                   (*status)++;
               }
           }                
       }
    }
}
        
/*      @(#)  256.1 date 12/16/89 ds_nurb_crea.c  */
/*+
 *
 *        Filename    : ds_nurb_crea.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:48:56
 *        Last change : 89/12/16 19:22:01
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_crea.c
 *
 *        This file contains the following routines:
 *                               
 *          add_nspline_pnt
 *          add_nsurface_pnt
 *          copy_nurbspline
 *          copy_nurbsurface
 *          create_nurbspline
 *          create_nurbsurface
-*/
           

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
BOOLEAN                                                                        
add_nspline_pnt(spline,pnt,index)
                        /* Description   :- This function adds a control point into the
                         *                  spline at the specified index.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE if index outwith range
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                             
DS_NURB_SPLINE_PTR  spline; /* <i> Spline */
POINT         pnt;    /* <i> Point to be added */
INT             index;  /* <i> Index of point */
{
    if (!spline)
        return(FALSE);          /* No spline */

    if ((index < 0) || (index >= spline->n) || ((index >= (spline->n - (spline->order -1)))
             && (spline->closed)))
        return(FALSE);          /* Invalid range */

    pntdeposit(spline->points,index,pnt);      /* Deposit point into spline */

    if ((spline->closed) && (index < (spline->order - 1)))   /* Is spline closed */
    {                                          /* Yes so repeat point */
        if ((index + (spline->n - (spline->order - 1))) >= spline->n)
            return(FALSE);

        pntdeposit(spline->points,index + (spline->n - (spline->order - 1)),pnt);
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
BOOLEAN                                                                        
add_nsurface_pnt(surface,pnt,index_u,index_v)
                        /* Description   :- This function adds a control point into the
                         *                  spline at the specified row and column it
                         *                  also automatically handles the repeating
                         *                  of points.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                           
DS_NURB_SURFACE_PTR surface;/* <i> Surface */
POINT           pnt;        /* <i> Point to be added */
INT             index_u;    /* <i> Row to be added at */
INT             index_v;    /* <i> Column to be added at */
{
    if (!surface)
        return(FALSE);      /* No surface so return error */
                                                            
    /* Are indexes within limits */
    if ((index_u < 0) || (index_u >= surface->m) || ((surface->closed_u)
            && (index_u >= (surface->m - (surface->order_u - 1)))))
        return(FALSE);              

    if ((index_v < 0) || (index_v >= surface->n) || ((surface->closed_v)
            && (index_v >= (surface->n - (surface->order_v - 1)))))
        return(FALSE);             

    /* Deposit point into mesh */
    mshdeposit(surface->points,index_u,index_v,surface->n,pnt);

    /* Is surface closed in U direction and can we repeat the point */
    if ((surface->closed_u) && (index_u < (surface->order_u - 1)))
    {   
        if ((index_u + (surface->m - (surface->order_u -1))) >= surface->m)
            return(FALSE);  /* Invalid index */

        mshdeposit(surface->points,index_u + (surface->m - (surface->order_u - 1)),
                        index_v,surface->n,pnt);
    }
                                                 
    /* Is surface closed in V direction and can we repeat the point */
    if ((surface->closed_v) && (index_v < (surface->order_v - 1)))
    {   
        if ((index_v + (surface->n - (surface->order_v -1))) >= surface->n)
            return(FALSE);  /* Invalid index */

        mshdeposit(surface->points,index_u,index_v + (surface->n - (surface->order_v - 1)),
                        surface->n,pnt);                           
        
        /* If surface is closed in U and V then repeat the point into the corner */
        if ((surface->closed_u) && (index_u < (surface->order_u - 1)))
            mshdeposit(surface->points,index_u + (surface->m - (surface->order_u -1)),
                    index_v + (surface->n - (surface->order_v -1)),surface->n,pnt);
    }
    return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 24th-August-1988
 *
 */
DS_NURB_SPLINE_PTR
copy_nurbspline(source)
                        /* Description   :- This function copies a spline
                         *
                         * Return status :- Pointer to new spline if successful
                         *                  NULL otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR source;   /* <i> Pointer to the source spline */
{
    DS_NURB_SPLINE_PTR  spline;
    POINT               point;
    INT                 numknots;
    INT                 i;

    if(source==NULL)
    {
        return(NULL);
    }

    if (!(spline = GET_NURBSPLINE()))        /* Get spline node */
    {
        return(NULL);
    }
    spline->type   = source->type;           /* Spline type */
    spline->order  = source->order;          /* Order of spline */
    spline->n      = source->n;              /* Number of control points */
    spline->closed = source->closed;         /* Whether spline is closed */
                      
    if (!(spline->points = GET_POINT(source->n)))    /* Get point block */
    {
        return(NULL);          /* Return error if no memory */
    }

    numknots = enq_knotnum(source->n, source->order);

    if (!(spline->knots = GET_KNOT(numknots)))
    {
        return(NULL);          /* Return error if no memory */
    }

    knt_copy(source->knots,numknots,spline->knots);     /* Copy knots */

    for( i = 0; i < source->n; i++ )
    {                                                   /* Copy control polygon */
        pntextract( source->points,i,point);
        pntdeposit( spline->points,i,point);
    }

    return( spline );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 24th-August-1988
 *
 */
DS_NURB_SURFACE_PTR
copy_nurbsurface(source)
                        /* Description   :- This function copies a nurb surface
                         *
                         * Return status :- Pointer to new surface geometry if successful
                         *                  NULL otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SURFACE_PTR source;   /* <i> Pointer to the source surface */
{
    DS_NURB_SURFACE_PTR  surface;
    POINT               point;
    INT                 numknots_u;
    INT                 numknots_v;
    INT                 i;

    if(source==NULL)
    {
        return(NULL);
    }

    if (!(surface = GET_NURBSURFACE()))        /* Get surface geometry node */
    {
        return(NULL);
    }
    surface->type      = source->type;         /* Surface type */
    surface->order_u   = source->order_u;
    surface->order_v   = source->order_v;
    surface->m         = source->m;
    surface->n         = source->n;
    surface->closed_u  = source->closed_u;
    surface->closed_v  = source->closed_v;

    if (!(surface->points = GET_MESH(source->m,source->n))) /* Get point block */
    {
        return(NULL);                                           
    }

    numknots_u = enq_knotnum(surface->m, surface->order_u);
    if (!(surface->knots_u = GET_KNOT(numknots_u)))
    {
        return(NULL);
    }
    knt_copy(source->knots_u,numknots_u,surface->knots_u);  /* Copy knots in U */

    numknots_v = enq_knotnum(surface->n, surface->order_v);
    if (!(surface->knots_v = GET_KNOT(numknots_v)))
    {
        return(NULL);
    }
    knt_copy(source->knots_v,numknots_v,surface->knots_v);  /* Copy knots in V */

    for( i = 0; i < (source->m * source->n); i++ )
    {                                                   /* Copy control polygon */
        pntextract( source->points,i,point);
        pntdeposit( surface->points,i,point);
    }

    return( surface );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
DS_NURB_SPLINE_PTR
create_nurbspline(order,n,closed,def_knots,knots,numknots)
                        /* Description   :- This function creates a nurb spline,
                         *                  sets the order and the closed flags,
                         *                  and sets the number of control points
                         *                  according to the closed flag.
                         *                  If def_knots is TRUE then a default
                         *                  knot vector is calculated else the
                         *                  user supplied one is used. A copy is taken
                         *                  of it.
                         *                  The points have to be added by calling
                         *                  add_pntspline.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                  
INT     order;          /* <i> Order of the spline */
INT     n;              /* <i> Number of control points in the spline */
BOOLEAN closed;         /* <i> Whether the spline is closed or not */
BOOLEAN def_knots;      /* <i> Whether to calculate default knots or use the supplied*/
double  knots[];        /* <i> User supplied knot vector if applicable */
INT     numknots;       /* <i> Number of knots in the users knot vector */
{                                                      
    DS_NURB_SPLINE_PTR    spline;

    if (n < order)
        return(FALSE);              /* Number of points must be >= order of spline */

    if (!(spline = GET_NURBSPLINE()))   /* Get spline block */
        return(NULL);

    if (closed)                     /* Spline closed */
        n += order - 1;             /* Yes so increase number of points in it */

    if (!(spline->points = GET_POINT(n)))   /* Get point block */
        return(NULL);                                           

    /* Set up spline data */
    spline->order  = order;          /* Order of spline */
    spline->n      = n;              /* Number of control points */
    spline->closed = closed;         /* Whether spline is closed */
                      

    if (def_knots)                  /* Generate default knots ?  */
    {                               /* Yes ... */
        if (!os_def_knt(spline->n,spline->order,spline->closed,&spline->knots))
            return(NULL);          /* Failed to generate spline knot vector */
    }
    else
    {                               /* Use user supplied knots */
        if (closed)
            return(NULL);          /* User is not allowed to specify on closed */
        
        if (numknots != enq_knotnum(spline->n, spline->order))
            return(NULL);          /* Return error if number of knots is wrong */

        if (!(spline->knots = GET_KNOT(numknots)))
            return(NULL);          
        knt_copy(knots,numknots,spline->knots);    
    }
    return( spline);
}
                                                                  
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 Jun 1988
 *
 */
DS_NURB_SURFACE_PTR
create_nurbsurface(order_u,order_v,m,n,closed_u,closed_v,def_knots_u,def_knots_v,
                    knots_u,knots_v,numknots_u,numknots_v)
                        /* Description   :- This function creates a nurb surface 
                         *                  of the specifed orders and rows and
                         *                  columns with if necessary the user
                         *                  supplied knot vectors.
                         *
                         * Return status :- Surface if successful
                         *                  NULL otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                     
INT     order_u;        /* <i> Order in U direction */
INT     order_v;        /* <i> Order in V direction */
INT     m;              /* <i> Number of rows in the U direction */
INT     n;              /* <i> Number of columns in the V direction */
BOOLEAN closed_u;       /* <i> Surface closed in U direction */
BOOLEAN closed_v;       /* <i> Surface closed in V direction */
BOOLEAN def_knots_u;    /* <i> Use user supplied knot vector for U direction ? */
BOOLEAN def_knots_v;    /* <i> Use user supplies knot vector for V direction ? */
double  knots_u[];      /* <i> User supplied knot vector for U direction */
double  knots_v[];      /* <i> User supplied knot vector for V direction */
INT     numknots_u;     /* <i> Number of knots in user supplied U knot vector */
INT     numknots_v;     /* <i> Number of knots in user supplied V knot vector */
{
    DS_NURB_SURFACE_PTR  surface;

    if ((m < order_u) || (n < order_v))
        return(FALSE);              /* Number of points must be greater than order */

    if (!(surface = GET_NURBSURFACE()))   /* Get surface block */
        return(NULL);

    /* Increase number of required points if surface is closed in U or V */
    if (closed_u)
        m += (order_u - 1);

    if (closed_v)
        n += (order_v - 1);

    if (!(surface->points = GET_MESH(m,n))) /* Get point block */
        return(NULL);                                           

    /* Now set up surface block */
    surface->order_u   = order_u;
    surface->order_v   = order_v;
    surface->m         = m;
    surface->n         = n;
    surface->closed_u  = closed_u;
    surface->closed_v  = closed_v;


    if (def_knots_u)        /* Default knots vector in U direction */
    {                       /* Yes ....*/
        if (!os_def_knt(surface->m,surface->order_u,surface->closed_u,&surface->knots_u))
            return(NULL);      /* Failed to generate U knot vector */
    }
    else
    {                       /* No ... Use user supplied ones */
        if (closed_u)
            return(NULL);  /* Do not allow if closed in U */
    
        if (numknots_u != enq_knotnum(surface->m, surface->order_u))
            return(NULL);       /* Number of knots is wrong */

        if (!(surface->knots_u = GET_KNOT(numknots_u)))
            return(NULL);

        knt_copy(knots_u,numknots_u,surface->knots_u);  /* Copy knots in */
    }

    if (def_knots_v)        /* Default knots vector in V direction */
    {                       /* Yes .... */
        if (!os_def_knt(surface->n,surface->order_v,surface->closed_v,&surface->knots_v))
            return(NULL);
    }
    else
    {                       /* No .... Use user supplied ones */
        if (closed_v)
            return(NULL);  /* Do not allow if closed in V direction */

        if (numknots_v != enq_knotnum(surface->n, surface->order_v))
            return(NULL);       /* Number of knots is wrong */

        if (!(surface->knots_v = GET_KNOT(numknots_v)))
            return(NULL);       /* Cannot allocate surface knot vector */

        knt_copy(knots_v,numknots_v,surface->knots_v);  /* Copy knots in */
    }
      return( surface );
} 






                               


/*      @(#)  256.1 date 12/16/89 ds_nurb_del.c  */
/*+
 *
 *        Filename    : ds_nurb_del.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:02
 *        Last change : 89/12/16 19:22:12
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_del.c
 *
 *        This file contains the following routines:
 *
 *        delete_nurbspline
 *        delete_nurbsurface
 *
-*/
 

static char *idstring = "@(#) ds_nurb_del.c 1.1 Retrieved - 88/08/16 08:41:27 Changed - 88/06/17 15:05:02";
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
BOOLEAN                 
delete_nurbspline(spline)
                        /* Description   :- This function frees all the memory a spline
                         *                  has been allocated
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The spline pointer is nulled before return
                         *
                         */
DS_NURB_SPLINE_PTR  *spline;  /* <i/o> The spline pointer */
{
    if ((!spline) || (!*spline))
        return(FALSE);      /* No spline to free */

    PUT((*spline)->knots);    /* Free the knots */
    PUT((*spline)->points);   /* Free the points */
    PUT(*spline);           /* Free the spline itself */

    *spline = NULL;         /* Clobber the pointer to NULL for integrity */
    
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
BOOLEAN                                                                        
delete_nurbsurface(surface)
                        /* Description   :- This function frees all the memory a 
                         *                  surface has had allocated to it 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :- Used/Modified
                         *
                         * Notes         :- The surface pointer is nulled before return
                         *
                         */                                                            
DS_NURB_SURFACE_PTR *surface;   /* <i/o> The surface pointer */
{                                      
    if ((!surface) || (!*surface))
        return(FALSE);       /* No surface to free */

    PUT((*surface)->knots_u);   /* Free the U knot vector */
    PUT((*surface)->knots_v);   /* Free the V knot vector */
    PUT((*surface)->points);   /* Free the control polyhedron */
    PUT(*surface);           /* Free the surface itself */

    *surface = NULL;         /* Clobber the pointer to NULL for integrity */

    return( TRUE );
}



/*      @(#)  256.1 date 12/16/89 ds_nurb_enq.c  */
/*+
 *
 *        Filename    : ds_nurb_enq.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:06
 *        Last change : 89/12/16 19:22:20
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_enq.c
 *
 *        This file contains the following routines:
 *                                 
 *          enq_nurbspline
 *          enq_nurbsurface
 *          enq_knotnum
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */
BOOLEAN                                                                        
enq_nurbspline(spline,npoints)
                        /* Description   :- This function returns the number of points
                         *                  in the spline subtracting if necessary the
                         *                  ones that are added for a closed spline.
                         *
                         * Return status :- TRUE if enquiry successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;   /* <i> The spline */
INT             *npoints; /* <o> The number of control points */
{                        
    if (!spline)
        return(FALSE);

    if (spline->closed)
        *npoints = spline->n - (spline->order -1 );
    else
        *npoints = spline->n;

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 June 1988
 *
 */                                                                            
BOOLEAN
enq_nurbsurface(surface,mpoints,npoints)
                        /* Description   :- This function returns the number of points
                         *                  in the U direction (mpoints) and the 
                         *                  V direction (npoints).
                         *
                         * Return status :- TRUE if enquiry successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                 
DS_NURB_SURFACE_PTR surface;    /* <i> The surface */
INT             *mpoints;   /* <o> Number of control points in U */
INT             *npoints;   /* <o> Number of control points in V */
{
    if (!surface)
        return(FALSE);

    if (surface->closed_u)
        *mpoints = surface->m - (surface->order_u - 1);
    else
        *mpoints = surface->m;

    if (surface->closed_v)
        *npoints = surface->n - (surface->order_v - 1);
    else
        *npoints = surface->n;

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
INT                                                                        
enq_knotnum(num_of_pnts,order)
                        /* Description   :- This function returns the number of knots
                         *                  that there should be for a spline or surface
                         *
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                 
INT     num_of_pnts;    /* <i> The number of control points */
INT     order;          /* <i> The order of the spline/surface */
{
    return(num_of_pnts + order);
}
/*      @(#)  256.1 date 12/16/89 ds_nurb_int.c  */
/*+
 *
 *
 *        Filename    : ds_nurb_int.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:09
 *        Last change : 89/12/16 19:22:31
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_int.c
 *
 *        This file contains the following routines:
 *                            
 *        ds_nurb_curve_int
 *        ds_nurb_curve_x_surf
-*/


#define INTERSECT_SIZE   20      /* Number of intersects to be allocated to start with*/
#define CUR_CUR_BLOCK     7      /* The number of doubles one curve - curve intersect takes */
#define CUR_SURF_BLOCK    8      /* The number of doubles one curve - surface takes */
#define U                 0      /* The U parm */
#define V                 1      /* The V parm */
#define PARALLEL         1.0     /* The intersection is termed parallel */
#define INTERSECTION     2.0     /* The intersection is termed an actual intersection*/



/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 Aug 1988
 *
 */
BOOLEAN                                                                       
ds_nurb_curve_int(spline1,spline2,tol,num_of_intersects,intersects)
                        /* Description   :- This function recursively subdivides 
                         *                  two curves and returns all the points of
                         *                  intersection plus the parameter values
                         *                  on each curve at that intersection pnt.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline1;   /* <i> First of two curves to be intersected */
DS_NURB_SPLINE_PTR  spline2;   /* <i> Second of the curve to be intersect */  
double              tol;       /* <i> The flatness tolerance */
INT       *num_of_intersects;  /* <i/o> Number of intersects so far */
double    **intersects;        /* <o> Array holding the list of intersects */
                               /*     The 3d point and the two parm values */
{   
    *num_of_intersects = 0;
    *intersects = NULL;
    if (!ds_nurb_curve_rec(spline1,spline2,tol,num_of_intersects,intersects))
        return(FALSE);
                             
/*
    quicksort_dim(0,(*num_of_intersects) - 1,CUR_CUR_BLOCK,4,&((*intersects)[1]));
                                                                 
    remove_parallel_intersects(&((*intersects)[1]),num_of_intersects,CUR_CUR_BLOCK);
*/
                     
    return(TRUE);
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 1 Aug 1988
 *
 */
BOOLEAN                                                                       
ds_nurb_curve_rec(spline1,spline2,tol,num_of_intersects,intersects)
                        /* Description   :- This function recursively subdivides 
                         *                  two curves and returns all the points of
                         *                  intersection plus the parameter values
                         *                  on each curve at that intersection pnt.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline1;   /* <i> First of two curves to be intersected */
DS_NURB_SPLINE_PTR  spline2;   /* <i> Second of the curve to be intersect */  
double              tol;       /* <i> The flatness tolerance */
INT       *num_of_intersects;  /* <i/o> Number of intersects so far */
double    **intersects;        /* <o> Array holding the list of intersects */
                               /*     The 3d point and the two parm values */
{   
    INT      i;
    BOOLEAN  flat1;
    BOOLEAN  flat2;               
    double   dist;
    double   parm1;
    double   parm2;                              
    double   sp1_parm;
    double   sp2_parm;
    double   new_u1;
    double   new_u2;      
    double   *new_points;        
    double   type;
    POINT    spoint;
    POINT    epoint;
    POINT    min1;
    POINT    max1;
    POINT    min2;
    POINT    max2;
    POINT    point1;
    POINT    point2;
    POINT    p1_start;
    VECTOR   p1_dir;
    POINT    p2_start;
    VECTOR   p2_dir;  
    POINT    ipoint;              
    int32_t     index;                                    
    int32_t     new_size;
    INT      num_intersects;                   
    BOOLEAN  parallel;
    DS_NURB_SPLINE_PTR   spline1_l;
    DS_NURB_SPLINE_PTR   spline1_r;
    DS_NURB_SPLINE_PTR   spline2_l;
    DS_NURB_SPLINE_PTR   spline2_r;
                     
    /* Calculate the bounding boxes and whether they intersect */
    pntminmax(spline1->points,spline1->n,min1,max1);
    pntminmax(spline2->points,spline2->n,min2,max2);

    if (((max1[X] < min2[X]) && (!TOL_WITHIN((max1[X] - min2[X]),TOL)))
          || ((max1[Y] < min2[Y]) && (!TOL_WITHIN((max1[Y] - min2[Y]),TOL))))
    {
        return(TRUE);  /* Bounding boxes do not touch */
    }

    if (((min1[X] > max2[X]) && (!TOL_WITHIN((min1[X] - max2[X]),TOL)))
          || ((min1[Y] > max2[Y]) && (!TOL_WITHIN((min1[Y] - max2[Y]),TOL))))
    {
        return(TRUE);  /* Bounding boxes do not touch */
    }

    /* Calculate whether the splines are flat or not */  
    pntextract(spline1->points,0,spoint);
    pntextract(spline1->points,spline1->n-1,epoint);
    vecdif(epoint,spoint,HOMO,p1_dir);
    flat1 = TRUE;
    if (veclensq(p1_dir,HOMO) < TOL)
        flat1 = FALSE;      /* Cannot be flat */

    for (i = 1; ((i < spline1->n) && (flat1)); i++)
    {
        pntextract(spline1->points,i,point1);
        axperpsql(point1,spoint,p1_dir,&dist);  /* Get the distance squared */
                               
        flat1 = (dist < tol) ? TRUE : FALSE;    /* Either flat or not */
    }

    pntextract(spline2->points,0,spoint);
    pntextract(spline2->points,spline2->n-1,epoint);
    vecdif(epoint,spoint,HOMO,p2_dir);
    flat2 = TRUE;

    if (veclensq(p2_dir,HOMO) < TOL)
        flat2 = FALSE;      /* Cannot be flat */

    for (i = 1; ((i < spline2->n) && (flat2)); i++)
    {
        pntextract(spline2->points,i,point2);
        axperpsql(point2,spoint,p2_dir,&dist);  /* Get the distance squared */

        flat2 = (dist < tol) ? TRUE : FALSE;    /* Either flat or not */
    }

    /* Now split the splines depending if they are not flat or not */
    if ((!flat1) && (!flat2))
    {
        if (!oslo_split(spline1,os_mid_knt(spline1->knots,spline1->n+spline1->order,
                         spline1->order),&spline1_l,&spline1_r))
            return(FALSE);
        if (!oslo_split(spline2,os_mid_knt(spline2->knots,spline2->n+spline2->order,
                         spline2->order),&spline2_l,&spline2_r))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_l,spline2_l,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_l,spline2_r,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_r,spline2_r,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_r,spline2_l,tol,num_of_intersects,intersects))
            return(FALSE);              
        delete_nurbspline(&spline1_l);
        delete_nurbspline(&spline1_r);
        delete_nurbspline(&spline2_l);
        delete_nurbspline(&spline2_r);
    }
    else if ((flat1) && (!flat2))
    {
        if (!oslo_split(spline2,os_mid_knt(spline2->knots,spline2->n+spline2->order,
                         spline2->order),&spline2_l,&spline2_r))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1,spline2_l,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1,spline2_r,tol,num_of_intersects,intersects))
            return(FALSE);  
        delete_nurbspline(&spline2_l);
        delete_nurbspline(&spline2_r);
    }
    else if ((!flat1) && (flat2))
    {
        if (!oslo_split(spline1,os_mid_knt(spline1->knots,spline1->n+spline1->order,
                         spline1->order),&spline1_l,&spline1_r))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_l,spline2,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_rec(spline1_r,spline2,tol,num_of_intersects,intersects))
            return(FALSE);
        delete_nurbspline(&spline1_l);
        delete_nurbspline(&spline1_r);
    }
    else 
    {   /* Both must be flat so try for intersect */
        pntextract(spline1->points,0,p1_start);
        pntextract(spline1->points,spline1->n-1,point1);
        POINTH_TO_3D(p1_start,p1_start);
        POINTH_TO_3D(point1,point1);
        vecdif(point1,p1_start,CART,p1_dir);

        pntextract(spline2->points,0,p2_start);
        pntextract(spline2->points,spline2->n-1,point2);
        POINTH_TO_3D(p2_start,p2_start);
        POINTH_TO_3D(point2,point2);
        vecdif(point2,p2_start,CART,p2_dir);
        
        if (line_x_line3DF(p1_start,p1_dir,p2_start,p2_dir,TOL,ipoint,&parm1,&parm2,
                                &parallel))
        {   /* Got intersections so work out parameters as they would be on the spline*/
            /* Then pass into the newton-raphson routine to get the actual point */
            /* on the spline */             
            ipoint[H] = 1.0;        /* Ensure homogenous */
                                                       

            sp1_parm = (spline1->knots[spline1->n + spline1->order - 1]
                            - spline1->knots[spline1->order-1])
                            * parm1 + spline1->knots[spline1->order-1];
            sp2_parm = (spline2->knots[spline2->n + spline2->order - 1]
                            - spline2->knots[spline2->order-1])
                            * parm2 + spline2->knots[spline2->order-1];
            /* Now use the Newton-Raphson to get the actual point */
            if (!new_spln_spln(spline1,spline2,sp1_parm,sp2_parm,TOL,&new_u1,point1,
                        &new_u2,point2))
            {   /* Tangents are similar */
                new_u1 = sp1_parm;
                new_u2 = sp2_parm;
                veccpy(ipoint,HOMO,point1);
            }
            num_intersects = 1;
            type = INTERSECTION;
        }
        else                   
        {
            if ((parallel) && (curve_line_parl(p1_start,p1_dir,p2_start,p2_dir,TOL,
                                  point1,&new_u1,&new_u2,point2,&parm1,&parm2)))
            {
                /* We have the parameter values on the line - line so now */
                /* convert it to the parameter values on the splines      */
                point1[H] = 1.0;
                point2[H] = 1.0;
                new_u1 = (spline1->knots[spline1->n + spline1->order - 1]
                            - spline1->knots[spline1->order-1])
                            * new_u1 + spline1->knots[spline1->order-1];
                new_u2 = (spline2->knots[spline2->n + spline2->order - 1]
                            - spline2->knots[spline2->order-1])
                            * new_u2 + spline2->knots[spline2->order-1];
                parm1  = (spline1->knots[spline1->n + spline1->order - 1]
                            - spline1->knots[spline1->order-1])
                            * parm1 + spline1->knots[spline1->order-1];
                parm2  = (spline2->knots[spline2->n + spline2->order - 1]
                            - spline2->knots[spline2->order-1])
                            * parm2 + spline2->knots[spline2->order-1];
                num_intersects = 2;
                type = PARALLEL;
            }
            else
                num_intersects = 0;
        }

        for ( i = 0; i < num_intersects; i++)
        {
            if (i == 1)
            {
                veccpy(point2,CART,point1);
                new_u1 = parm1;
                new_u2 = parm2;
            }
            /* Have we enough space in array, The array is doubles and each intersect takes up */
            /* six doubles. We store in the zero index of the array the number of intersects */
            /* that the array can handle. */
            
            if ((!*intersects) || ( (*num_of_intersects) >= (INT) (*intersects)[0]))
            {   /* Allocate memory for the intersect list */
                if (*intersects)
                   new_size = (int32_t) (*intersects)[0] * 2 ;
                else
                   new_size = INTERSECT_SIZE ;

                if (!(new_points = (double *) ds_alloc(new_size * sizeof(double) 
                                           * CUR_CUR_BLOCK)))
                    return(FALSE);
                else
                {
                    if (*intersects)
                    {   /* If not NULL then copy and free */  
                       veccpy(*intersects,(int) ((*intersects)[0] * CUR_CUR_BLOCK) + 1,
                                       new_points);
                       ds_free(*intersects);
                    }
                    *intersects = new_points;  
                    (*intersects)[0] = (double) new_size  - 1;
                }
            }                        
            /* Deposit point and two parmater values */ 
            index = (int32_t) (*num_of_intersects) * CUR_CUR_BLOCK + 1;
            (*intersects)[index++] = point1[X];
            (*intersects)[index++] = point1[Y];
            (*intersects)[index++] = point1[Z];
            (*intersects)[index++] = point1[H];
            (*intersects)[index++] = new_u1;
            (*intersects)[index++] = new_u2;
            (*intersects)[index]   = type;      /* Store the intersection type */
            (*num_of_intersects)++;
        }
    }

    return(TRUE);
}



/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 19 Aug 1988
 *
 */
BOOLEAN                                                                       
ds_nurb_curve_x_surf(spline,surf,tol,num_of_intersects,intersects)
                        /* Description   :- This function recursively subdivides a
                         *                  curve and a surface and returns all the 
                         *                  points of intersection plus the parameter 
                         *                  values on each curve at that intersection.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;   /* <i> Curve to be intersected              */
DS_NURB_SURFACE_PTR   surf;   /* <i> Surface to be intersected            */  
double                 tol;   /* <i> The flatness tolerance               */
INT     *num_of_intersects;   /* <i/o> Number of intersects so far        */
double        **intersects;   /* <o> Array holding the list of intersects */
                              /*     The 3d point and the two parm values */
                              /*     The format is                        */
                              /*     [X,Y,Z,H,splineu,surfaceu,surfacev]  */
{
    if (!ds_nurb_curve_x_surf_rec(spline,surf,tol,num_of_intersects,intersects))
        return(FALSE);
         

    quicksort_dim(0,(*num_of_intersects) - 1, CUR_SURF_BLOCK,4,&((*intersects)[1]));

    remove_parallel_intersects(&((*intersects)[1]),num_of_intersects,CUR_SURF_BLOCK);

    return(TRUE);

}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 19 Aug 1988
 *
 */
BOOLEAN                                                                       
ds_nurb_curve_x_surf_rec(spline,surf,tol,num_of_intersects,intersects)
                        /* Description   :- This function recursively subdivides a
                         *                  curve and a surface and returns all the 
                         *                  points of intersection plus the parameter 
                         *                  values on each curve at that intersection.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;   /* <i> Curve to be intersected              */
DS_NURB_SURFACE_PTR   surf;   /* <i> Surface to be intersected            */  
double                 tol;   /* <i> The flatness tolerance               */
INT     *num_of_intersects;   /* <i/o> Number of intersects so far        */
double        **intersects;   /* <o> Array holding the list of intersects */
                              /*     The 3d point and the two parm values */
                              /*     The format is                        */
                              /*     [X,Y,Z,H,splineu,surfaceu,surfacev]  */
{   
    INT      i;
    INT      num_intersects;
    BOOLEAN  flat_spline;
    BOOLEAN  flat_surface;                                                            
    BOOLEAN  parallel;
    double   dist;
    double   *new_points;                                                        
    double   type;
    POINT    spoint;
    POINT    epoint;
    POINT    min1;
    POINT    max1;
    POINT    min2;
    POINT    max2;
    POINT    plane[3];
    VECTOR   p1_dir;
    POINT    ipoint;              
    POINT    l_start;
    VECTOR   l_dir;
    POINT    point;   
    POINT    point00;
    POINT    pointm0;
    POINT    point0n; 
    double   surf_u;
    double   surf_v;
    double   spline_u;
    double   spline_parm;
    double   surf_parm[2];
    double   surface_parms[2];
    double   curve_parm;
    double   curve_parm1;
    double   surface_parms1[2];
    POINT    ipoint1;
    POINT    curve_pnt;        
    POINT    surface_pnt;
    int32_t     index;                                    
    int32_t     new_size;
    DS_NURB_SPLINE_PTR   spline_l;
    DS_NURB_SPLINE_PTR   spline_r;
    DS_NURB_SURFACE_PTR     surf1;
    DS_NURB_SURFACE_PTR     surf2;
    DS_NURB_SURFACE_PTR     surf3;
    DS_NURB_SURFACE_PTR     surf4;
                                         

    /* Calculate the bounding boxes and whether they intersect */
    pntminmax(spline->points,spline->n,min1,max1);
    mshminmax(surf->points,surf->m,surf->n,min2,max2);

    if ((max1[X] < min2[X]) || (max1[Y] < min2[Y])
            || (max1[Z] < min2[Z]) || (max1[H] < min2[H]))
    {
        return(TRUE);  /* Bounding boxes do not touch */
    }

    if ((min1[X] > max2[X]) || (min1[Y] > max2[Y])
            || (min1[Z] > max2[Z]) || (min1[H] > max2[H]))
    {
        return(TRUE);  /* Bounding boxes do not touch */
    }

    /* Calculate whether the spline is flat or not */  
    pntextract(spline->points,0,spoint);
    pntextract(spline->points,spline->n-1,epoint);
    vecdif(epoint,spoint,HOMO,p1_dir);
    flat_spline = TRUE;
    if (veclensq(p1_dir,HOMO) < TOL)
        flat_spline = FALSE;      /* Cannot be flat */

    for (i = 1; ((i < spline->n) && (flat_spline)); i++)
    {
        pntextract(spline->points,i,point);
        axperpsql(point,spoint,p1_dir,&dist);  /* Get the distance squared */
        
        flat_spline = (dist < tol) ? TRUE : FALSE;    /* Either flat or not */
    }
                         
    /* Calculate whether the surface is flat */ 
    flat_surface = surf_flat(surf,tol);

    /* Now split the spline and the surface depending if they are not flat or not */
    if ((!flat_spline) && (!flat_surface))
    {
        if (!oslo_split(spline,os_mid_knt(spline->knots,spline->n+spline->order,
                         spline->order),&spline_l,&spline_r))
            return(FALSE);
        if (!surf_split(surf,&surf1,&surf2,&surf3,&surf4))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf1,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf2,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf3,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf4,tol,num_of_intersects,intersects))
            return(FALSE);                                                         
        if (!ds_nurb_curve_x_surf_rec(spline_r,surf1,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_r,surf2,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_r,surf3,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_r,surf4,tol,num_of_intersects,intersects))
            return(FALSE);                                                         

    }
    else if ((flat_spline) && (!flat_surface))
    {
        if (!surf_split(surf,&surf1,&surf2,&surf3,&surf4))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline,surf1,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline,surf2,tol,num_of_intersects,intersects))
            return(FALSE);                    
        if (!ds_nurb_curve_x_surf_rec(spline,surf3,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline,surf4,tol,num_of_intersects,intersects))
            return(FALSE);                    
    }
    else if ((!flat_spline) && (flat_surface))
    {
        if (!oslo_split(spline,os_mid_knt(spline->knots,spline->n+spline->order,
                         spline->order),&spline_l,&spline_r))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf1,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf2,tol,num_of_intersects,intersects))
            return(FALSE);                                                         
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf3,tol,num_of_intersects,intersects))
            return(FALSE);
        if (!ds_nurb_curve_x_surf_rec(spline_l,surf4,tol,num_of_intersects,intersects))
            return(FALSE);                                                         
    }
    else 
    {   /* Both must be flat so try for intersect */
        pntextract(spline->points,0,l_start);
        pntextract(spline->points,spline->n-1,point);
        POINTH_TO_3D(l_start,l_start);
        POINTH_TO_3D(point,point);
        vecdif(point,l_start,CART,l_dir);
                                      
        mshextract(surf->points,0,0,surf->n,point00);
        mshextract(surf->points,surf->m-1,0,surf->n,pointm0);
        mshextract(surf->points,0,surf->n-1,surf->n,point0n);
        POINTH_TO_3D(point00,point00);
        POINTH_TO_3D(pointm0,pointm0);
        POINTH_TO_3D(point0n,point0n);
        pntdeposit(plane,0,point00);
        pntdeposit(plane,1,pointm0);
        pntdeposit(plane,2,point0n);
        
        if (blin_bpln_x(plane,l_start,l_dir,ipoint,&surf_u,&surf_v,&spline_u,&parallel))
        {   /* Got intersections so work out parameters as they would be on the spline*/
            /* Then pass into the newton-raphson routine to get the actual point */
            /* on the spline */
            spline_parm = (spline->knots[spline->n + spline->order - 1]
                            - spline->knots[spline->order-1])
                            * spline_u + spline->knots[spline->order - 1];
            surf_parm[U] = (surf->knots_u[surf->m + surf->order_u - 1]
                            - surf->knots_u[surf->order_u-1])
                            * surf_u + surf->knots_u[surf->order_u - 1];
            surf_parm[V] = (surf->knots_v[surf->n + surf->order_v - 1]
                            - surf->knots_v[surf->order_v-1])
                            * surf_v + surf->knots_v[surf->order_v - 1];
                                                                     
            /* Now use the Newton-Raphson to get the actual point */
            if (!new_spln_surf(spline,surf,spline_parm,surf_parm,TOL,
                                 &curve_parm,curve_pnt,surface_parms,surface_pnt))
            {       /* Tangents are similar */
                    surface_parms[U] = surf_parm[U];
                    surface_parms[V] = surf_parm[V];
                    curve_parm = spline_parm;
                    veccpy(ipoint,HOMO,curve_pnt);
            }
            num_intersects = 1;
            type = INTERSECTION;
        }
        else 
        {
            if ((parallel) && (curve_pln_parl(plane,l_start,l_dir,curve_pnt,
                    surface_parms,&curve_parm,ipoint1,surface_parms1,&curve_parm1)))
            {   /* We have the local parameter values so now convert to the global */
                /* parameter values . */
                
            curve_parm = (spline->knots[spline->n + spline->order - 1]
                            - spline->knots[spline->order-1])
                            * curve_parm + spline->knots[spline->order - 1];
            surface_parms[U] = (surf->knots_u[surf->m + surf->order_u - 1]
                            - surf->knots_u[surf->order_u-1])
                            * surface_parms[U] + surf->knots_u[surf->order_u - 1];
            surface_parms[V] = (surf->knots_v[surf->n + surf->order_v - 1]
                            - surf->knots_v[surf->order_v-1])
                            * surface_parms[V] + surf->knots_v[surf->order_v - 1];
                                                                     
                
            curve_parm1 = (spline->knots[spline->n + spline->order - 1]
                            - spline->knots[spline->order-1])
                            * curve_parm1 + spline->knots[spline->order - 1];
            surface_parms1[U] = (surf->knots_u[surf->m + surf->order_u - 1]
                            - surf->knots_u[surf->order_u-1])
                            * surface_parms1[U] + surf->knots_u[surf->order_u - 1];
            surface_parms1[V] = (surf->knots_v[surf->n + surf->order_v - 1]
                            - surf->knots_v[surf->order_v-1])
                            * surface_parms1[V] + surf->knots_v[surf->order_v - 1];

               num_intersects = 2;
               type = PARALLEL;
            }                     
            else
               num_intersects = 0;
        }


        for (i = 0; i < num_intersects; i++)
        {
            if (i == 1)
            {   /* Copy in the second set of values */
                veccpy(ipoint1,HOMO,curve_pnt);
                veccpy(surface_parms1,2,surface_parms);
                curve_parm = curve_parm1;
            }

            if ((!*intersects) || ( (*num_of_intersects) >= (INT) (*intersects)[0]))
            {   /* Allocate memory for the intersect list */
                if (*intersects)
                   new_size = (int32_t) (*intersects)[0] * 2 ;
                else
                   new_size = INTERSECT_SIZE ;

                if (!(new_points = (double *) ds_alloc(new_size * sizeof(double) 
                                           * CUR_SURF_BLOCK)))
                    return(FALSE);
                else
                {
                    if (*intersects)
                    {   /* If not NULL then copy and free */  
                       veccpy(*intersects,(int) ((*intersects)[0] * CUR_SURF_BLOCK) + 1,
                                   new_points);
                       ds_free(*intersects);
                    }
                    *intersects = new_points;  
                    (*intersects)[0] = (double) new_size  - 1;
                }
            }                        
            /* Deposit point and two parmater values */            
            index = (int32_t) (*num_of_intersects) * CUR_SURF_BLOCK + 1;
            (*intersects)[index++] = curve_pnt[X];
            (*intersects)[index++] = curve_pnt[Y];
            (*intersects)[index++] = curve_pnt[Z];
            (*intersects)[index++] = curve_pnt[H];    
            (*intersects)[index++] = curve_parm;
            (*intersects)[index++] = surface_parms[U];
            (*intersects)[index++] = surface_parms[V];
            (*intersects)[index]   = type;
            (*num_of_intersects)++;    /* Increment number of intersects */
        }
    }
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 23 Aug 1988
 *
 */
BOOLEAN                                                                        
curve_line_parl(p1_start,p1_dir,p2_start,p2_dir,tol,inter1,parm1,
                             parm2,inter2,parm3,parm4)
                        /* Description   :- This function takes two lines that are
                         *                  parallel and sees if they intersect.
                         *                  ie If one lies ontop of of the other one
                         *                  and if so returns the intersection points.
                         *
                         * Return status :- TRUE if lines touch
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                    
POINT     p1_start;     /* <i> The start point of the first line */
VECTOR    p1_dir;       /* <i> The direction of the first line */
POINT     p2_start;     /* <i> The start point of the second line */
VECTOR    p2_dir;       /* <i> The direction of the second line */
double    tol;          /* <i> The tolerance to be used in the calculation */
POINT     inter1;       /* <o> The first intersect point */
double    *parm1;       /* <o> The parameter of the intersection */
double    *parm2;       /* <o> The parameter of the intersection */
POINT     inter2;       /* <o> The second intersect point */
double    *parm3;       /* <o> The parameter of the intersection */
double    *parm4;       /* <o> The parameter of the intersection */
{   
    VECTOR   vector;
    POINT    p2_end;     
    POINT    p1_end;                     
    
    POINT    intersection[4];
    VECTOR   vect[4];
    INT      intersect = 0;

    /* Have to check that the lines are actually parallel */
    
    vecsum(p1_start,p1_dir,CART,p1_end);
    vecsum(p2_start,p2_dir,CART,p2_end);     

    if (point_on_line(p1_start,p2_start,p1_end))                               
    {
        axperp(p2_start,p1_start,p1_dir,vect[intersect],intersection[intersect]);
        intersect++;
    }

    if (point_on_line(p1_start,p2_end,p1_end))                                
    {
        axperp(p2_end,p1_start,p1_dir,vect[intersect],intersection[intersect]);
        intersect++;
    }
                     
    if ((intersect != 2) && point_on_line(p2_start,p1_start,p2_end))
    {
        axperp(p1_start,p2_start,p2_dir,vect[intersect],intersection[intersect]);
        intersect++;
    }

    if ((intersect != 2) && point_on_line(p2_start,p1_end,p2_end))
    {
        axperp(p1_end,p2_start,p2_dir,vect[intersect],intersection[intersect]);
    }

    if ((intersect == 0) || ((!TOL_WITHIN(veclensq(vect[0],CART),TOL))
        && (!TOL_WITHIN(veclensq(vect[1],CART),TOL))))
        return(FALSE);      /* Lines are not close enough together */
    

    vecdif(intersection[0],p1_start,CART,vector);
    *parm1 = veclen(vector,CART) / veclen(p1_dir,CART);  /* Classify the first intersection */
                                                         /* Against the first line */

    vecdif(intersection[0],p2_start,CART,vector);
    *parm2 = veclen(vector,CART) / veclen(p2_dir,CART);   /* Classify the first intersection */
                                                          /* Against the second line */

    vecdif(intersection[1],p1_start,CART,vector);
    *parm3 = veclen(vector,CART) / veclen(p1_dir,CART);   /* Classify the second intersection */
                                                          /* Against the first line */
                                          
    vecdif(intersection[1],p2_start,CART,vector);
    *parm4 = veclen(vector,CART) / veclen(p2_dir,CART);   /* Classify the second intersection */
                                                          /* Against the second line */
    /* Copy the two intersections to the parameters */
    veccpy(intersection[0],CART,inter1);
    veccpy(intersection[1],CART,inter2);

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 29 Aug 1988
 *
 */
BOOLEAN                                                                        
curve_pln_parl(plane,l_start,l_dir,ipoint,surface_parm,curve_parm,ipoint1,
                     surface_parm1,curve_parm1)
                        /* Description   :- This function takes a plane and a line that
                         *                  are deemed to be parallel and checks to see
                         *                  if they lie within a tolerance and if so
                         *                  then returns the two points of intersection
                         *                  and the parameter values at each 
                         *                  intersection for the surface and the curve.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                            
PTR_POINT    plane;     /* <i> The three co-ords defining the plane */
POINT        l_start;   /* <i> The start point of the line */
VECTOR       l_dir;     /* <i> The direction of the line */
POINT        ipoint;    /* <o> The first intersection point between the plane and line*/
double       surface_parm[]; /* <o> The two parameter values on the surface at the */
                            /*     first intersection point */
double       *curve_parm;   /* <o> The paramter value on the line for the first */
                            /*     intersection point */                       
POINT        ipoint1;       /* <o> The second intersection point between the plane and line*/
double       surface_parm1[]; /* <o> The two parameter values on the surface at the */
                            /*     second intersection point */
double       *curve_parm1;  /* <o> The paramter value on the line for the second */
                            /*     intersection point */                       
{
    BOOLEAN     parallel;       /* Whether the intersection failed due to parallelism */
    INT         i;
    POINT       plane_equ;      /* The equation of the plane */
    POINT       l_end;          /* The end point of the line */
    POINT       point1;         /* The points that allow us to walk round the plane */
    POINT       point2;         /* boundary doing parametric intersections */
    POINT       point3;
    POINT       point4;
    POINT       point;
    POINT       int1_start;
    POINT       int2_start;
    POINT       parm_start;     /* The start of the line in parameter space */
    POINT       parm_dir;       /* The direction of the line in parameter space */
    VECTOR      dir1;           /* The directionss that allow us to walk round the plane */
    VECTOR      dir2;           /* boundary doing parametric intersections */
    VECTOR      dir3;
    VECTOR      dir4;
    VECTOR      pvec_start;     /* The vector that drops the start point onto the plane*/
    VECTOR      pvec_end;       /* The vector that drops the end point onto the plane */
    double      s;
    double      t;         
    double      u1_start;
    double      v1_start;                              
    double      t1_start;
    double      u2_end;
    double      v2_end;  
    double      t2_end;

    point1[X] = 0.0; point1[Y] = 0.0; point1[Z] = 0.0; point1[H] = 1.0;
    point2[X] = 0.0; point2[Y] = 1.0; point2[Z] = 0.0; point2[H] = 1.0;
    point3[X] = 1.0; point3[Y] = 0.0; point3[Z] = 0.0; point3[H] = 0.0;
    point4[X] = 0.0; point4[Y] = 0.0; point4[Z] = 0.0; point4[H] = 1.0;
    dir1[X] = 0.0; dir1[Y] = 1.0; dir1[Z] = 0.0; dir1[H] = 1.0;
    dir2[X] = 1.0; dir2[Y] = 0.0; dir2[Z] = 0.0; dir2[H] = 1.0;
    dir3[X] = 0.0; dir3[Y] = 1.0; dir3[Z] = 0.0; dir3[H] = 1.0;
    dir4[X] = 1.0; dir4[Y] = 0.0; dir4[Z] = 0.0; dir4[H] = 1.0;
           
    /* Calculate the plane equation */
    pln3p(plane[0],plane[1],plane[2],TOL,plane_equ);
                                      
    /* Calculate the vector from the point to the plane */
    plnperp(plane_equ,l_start,pvec_start);  

    if (veclensq(pvec_start,CART) > TOL)
        return(FALSE);      /* Point not close enough */

    vecsum(l_start,l_dir,CART,l_end);
    plnperp(plane_equ,l_end,pvec_end);

    if (veclensq(pvec_end,CART) > TOL)
        return(FALSE);      /* Point not close enough */
                                                               
    /* Points are close enough to be termed parallel */
    /* Now calculate their intersection points & parameters when dropped onto the plane */
    if (!ublin_ubpln_x(plane,l_start,plane_equ,int1_start,&u1_start,&v1_start,
                       &t1_start,&parallel))
        return(FALSE);

    if (!ublin_ubpln_x(plane,l_end,plane_equ,int2_start,&u2_end,&v2_end,
                       &t2_end,&parallel))
        return(FALSE);

   parm_start[U] = u1_start; parm_start[V] = v1_start; parm_start[Z] = 0.0;
   parm_start[H] = 1.0;                     
   parm_dir[U] = u2_end - parm_start[U];
   parm_dir[V] = v2_end - parm_start[V];
   parm_dir[Z] = 0.0;
   parm_dir[H] = 1.0;


   if ((u1_start >= 0.0) && (u1_start <= 1.0) && (v1_start >= 0.0) && (v1_start <= 1.0))
   {    /* Start point is within plane boundary so return its intersection point */
        surface_parm[U] = u1_start;
        surface_parm[V] = v1_start;
        *curve_parm = t1_start;
        veccpy(int1_start,CART,ipoint);
   }
   else
   {    /* Start point is outwith plane boundary so return its intersection point */
        /* with the plane boundary */
        if (!line_x_line3DF(parm_start,parm_dir,point1,dir1,TOL,point,&s,&t,&parallel))
            if (!line_x_line3DF(parm_start,parm_dir,point2,dir2,TOL,point,&s,&t,&parallel))
                if (!line_x_line3DF(parm_start,parm_dir,point3,dir3,TOL,point,&s,&t,&parallel))
                    if (!line_x_line3DF(parm_start,parm_dir,point4,dir4,TOL,point,&s,&t,&parallel))
                        return(FALSE);                                                 
        surface_parm[U] = point[U];
        surface_parm[V] = point[V];    
        *curve_parm = s;      
        for (i = 0; i < 3; i++)
           ipoint[i] = l_start[i] + l_dir[i] * s;
        ipoint[H] = 1.0;
   }
  
   if ((u2_end >= 0.0) && (u2_end <= 1.0) && (v2_end >= 0.0) && (v2_end <= 1.0))
   {    /* End point is within plane boundary so return its intersection point */
        surface_parm1[U] = u2_end;
        surface_parm1[V] = v2_end;
        *curve_parm1 = t2_end;
        veccpy(int2_start,CART,ipoint1);
   }
   else
   {    /* End point is outwith plane boundary so return its intersection point */
        /* with the plane boundary this intersection call is the reverse of the above*/
        /* so that we would find the other intersection first */
        if (!line_x_line3DF(parm_start,parm_dir,point4,dir4,TOL,point,&s,&t,&parallel))
            if (!line_x_line3DF(parm_start,parm_dir,point3,dir3,TOL,point,&s,&t,&parallel))
                if (!line_x_line3DF(parm_start,parm_dir,point2,dir2,TOL,point,&s,&t,&parallel))
                    if (!line_x_line3DF(parm_start,parm_dir,point1,dir1,TOL,point,&s,&t,&parallel))
                        return(FALSE);                          
        surface_parm1[U] = point[U];
        surface_parm1[V] = point[V];
        *curve_parm1 = s;
        for (i = 0; i < 3; i++)
           ipoint1[i] = l_start[i] + l_dir[i] * s;
        ipoint1[H] = 1.0;
       
   }
   return( TRUE );
}


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 26 Aug 1988
 *
 */
BOOLEAN
remove_parallel_intersects(intersects,num_of_intersects,dim)
                        /* Description   :- This function scans the list of intersects
                         *                  which is assumed to have been sorted and
                         *                  removes any multiple intersects.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                    
double    intersects[];        /* <i> Array holding the list of intersects */
                               /*     The 3d point and the two parm values */
INT       *num_of_intersects;  /* <i/o> Number of intersects so far */
INT       dim;                 /* <i> Size of block for each intersect */
{
    BOOLEAN match;                        
    INT     j;
    INT     next_check;
    INT     num_of_parallel;
    INT     source;

    source = -1;
                                                              
    if (*num_of_intersects <= 1)
        return(TRUE);

    for (next_check = 0; next_check < (*num_of_intersects - 1); next_check++)
    {                                                               
        for (match = TRUE, j = 0; ((j < (dim - 1)) && (match)); j++)
            if (fabs(intersects[next_check * dim + j] - 
                    intersects[(next_check + 1) * dim + j]) > TOL)
                match = FALSE;                          
        if (!match)
        {
            source++;
            if (source != next_check)
            {   /* Check that we won't copy it onto itself needlessly */
                for (j = 0; j < dim; j++)
                   intersects[source * dim + j] = intersects[next_check * dim + j];
            }
        }
        else
        {   /* We have a match of two or possibly more */
            num_of_parallel = 0;
            for (; match && next_check < (*num_of_intersects - 1); )
            {                 
               if (intersects[next_check * dim + (dim -1)] == PARALLEL)
                    num_of_parallel++;

                for (match = TRUE, j = 0; ((j < (dim - 1)) && (match)); j++)
                {
                   if (!TOL_WITHIN((intersects[(next_check + 1) * dim + j] - 
                              intersects[next_check * dim + j]),TOL))
                       match = FALSE;                                         
                }
                if (match)
                     next_check++;
            }
            if (num_of_parallel < 2)
            {   /* One of the intersects was the result of an actual intersection */
                /* so ensure we save this and do not remove it */
                source++;       
                if (source != next_check)
                {   /* Check that we won't copy it onto itself needlessly */
                    for (j = 0; j < dim; j++)
                        intersects[source * dim + j] = intersects[next_check * dim + j];
                }
            }
        }
    }            
    /* This piece of code catches the last one if required */              
    next_check = (*num_of_intersects - 2);
    for (match = TRUE, j = 0; ((j < (dim - 1)) && (match)); j++)
        if (fabs(intersects[next_check * dim + j] - 
                    intersects[(next_check + 1) * dim + j]) > TOL)
            match = FALSE;               
    if (!match)
    {
         source++;     
         next_check++;
         if (source != next_check)
         {   /* Check that we won't copy it onto itself needlessly */
             for (j = 0; j < dim; j++)
                intersects[source * dim + j] = intersects[next_check * dim + j];
         }
     }

    *num_of_intersects = source + 1;  /* Update the new number of intersects */
    return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 ds_nurb_oslo.c  */
/*+
 *
 *
 *        Filename    : ds_nurb_oslo.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:17
 *        Last change : 89/12/16 19:22:43
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_oslo.c
 *
 *        This file contains the following routines:
 *
-*/
 
                                
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 21 Jun 1988
 *
 */
INT
oslo_find(start, knots, ref_knots , num_knots, index)
                        /* Description   :- This function finds the unique mu such that
                         *                  knots(mu) <= ref_knots(index) <= knots(mu+1)
                         *
                         * Return status :- mu
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
INT       start;        /* <i> The point to start the search aint32_t the knot vector */
double    knots[];      /* <i> The knot vector */
double    ref_knots[];  /* <i> The refined knot vector */
INT       num_knots;    /* <i> The number of knots in the knot vector */
INT       index;        /* <i> Index into the refined knot vector array */
{
    double    tj;
    double    diff;
    INT       i;

    tj = ref_knots[index];

    for (i = start; i < num_knots - 1; i++)
    {
        diff = tj - knots[i];
        if ((diff < 0.0) && (!TOL_FAILURE(diff)))
        {
            return(i-1);
        }
    }

    return(num_knots - 1);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 21 Jun 1988
 *
 */                                                                            
int 
oslo_loop(spline,ref_knots,num_refknots,ref_polygon)
                        /* Description   :- This function calculates the new vertices
                         *                  of a control polygon given a refined knot
                         *                  vector using algorithm 2 of the Oslo
                         *                  algorithms. 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The version will work for any knot vector.
                         *
                         */                                                           
DS_NURB_SPLINE_PTR spline;       /* <i> The input spline or part of a surface treated as one*/
double             ref_knots[];  /* <i> The refined knot vector */
INT                num_refknots; /* <i> The number of knots in the refined knot vector*/
PTR_POINT          ref_polygon;  /* <o> Pointer to refined point chain */
{
    INT     j;
    INT     mu; 
    INT     num_knots;
    POINT   point;
                      
    num_knots = spline->n + spline->order;

    for (mu = 0,j = 0; j < num_refknots - spline->order; j++)
    {
        mu = oslo_find(mu, spline->knots, ref_knots, num_knots, j);
        
        oslo_recursive_subdiv(spline->order,spline->knots,ref_knots,spline->points,
                                 spline->order,mu,j,point);

        pntdeposit(ref_polygon,j,point);
    }

    return(num_refknots - spline->order);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 Jun 1988
 *
 */
BOOLEAN                                                                        
oslo_split(spline,cutpoint,spline1,spline2)
                        /* Description   :- This function splits a spline polygon
                         *                  into two.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                          
DS_NURB_SPLINE_PTR  spline;   /* <i> The spline to be split */
double              cutpoint; /* <i> The parametric cut point */
DS_NURB_SPLINE_PTR  *spline1; /* <o> The first split spline */
DS_NURB_SPLINE_PTR  *spline2; /* <o> The second split spline */   
{
    if (!spline->closed)
        return(oslo_intsplit(spline,cutpoint,spline1,spline2));
    else
        return(oslo_intclosplit(spline,cutpoint,spline1,spline2));
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 Jun 1988
 *
 */
BOOLEAN                                                                        
oslo_intsplit(spline,cutpoint,spline1,spline2)
                        /* Description   :- This function splits a spline polygon
                         *                  into two.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  Only to be INTERNALLY called
                         *
                         */                          
DS_NURB_SPLINE_PTR  spline;   /* <i> The spline to be split */
double              cutpoint; /* <i> The parametric cut point */
DS_NURB_SPLINE_PTR  *spline1; /* <o> The first split spline */
DS_NURB_SPLINE_PTR  *spline2; /* <o> The second split spline */
{
    double       *knots;
    POINT        point;
    PTR_POINT    point_chain;
    INT          lent;
    INT          startpos;
    INT          i,j;

    lent = spline->n + spline->order;
                                    
/*
    if (!(knots = GET_KNOT(spline->n + (6 * spline->order))))
        return(FALSE);
*/    
    lent = os_iterate_knt(cutpoint,spline->order,spline->knots,lent,spline->order,&knots,
                    &startpos);     /* Duplicate knots */

    if (!(point_chain = GET_POINT(lent - spline->order)))
        return(FALSE);
    
    oslo_loop(spline,knots,lent,point_chain); /* Calc new cntrl points */
                       
    /* Create the two splines */                             
    if (!(*spline1 = create_nurbspline(spline->order,startpos,OPEN,USER_KNOTS,knots,
                            startpos + spline->order)))
        return(FALSE);

    if (!(*spline2 = create_nurbspline(spline->order,lent - startpos - spline->order,OPEN,
                    USER_KNOTS,&knots[startpos],lent - startpos)))
        return(FALSE);

    for (i = 0; i < (*spline1)->n; i++)       /* Separate the two polygons */
    {
        pntextract(point_chain,i,point);    /* Do first polygon */
        pntdeposit((*spline1)->points,i,point);
    }

    /* Do second polygon */
    for (i = (*spline1)->n, j = 0; i < (*spline1)->n + (*spline2)->n; i++,j++)
    {
        pntextract(point_chain,i,point);
        pntdeposit((*spline2)->points,j,point);
    }

    PUT(knots);
    PUT(point_chain);

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 June 1988
 *
 */
BOOLEAN                                                                         
oslo_intclosplit(spline,cutpoint,spline1,spline2)
                        /* Description   :- This function splits a closed spline into
                         *                  two. It first of all opens it at 0.5 then splits 
                         *                  it.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR  spline;     /* <i> The spline to be split */
double              cutpoint;   /* <i> The parametric cut point */
DS_NURB_SPLINE_PTR  *spline1;   /* <o> The first part of the spline */
DS_NURB_SPLINE_PTR  *spline2;   /* <o> The second part of the spline */
{                                                           
    DS_NURB_SPLINE_PTR tspline1;
    DS_NURB_SPLINE_PTR tspline2;
    DS_NURB_SPLINE     tmp_spline;
    POINT              point;
    PTR_POINT          point_chain;
    double             *knots;

    INT         i,j;
    INT         num_points;
                                                                     
    /* Split the polygon */
    oslo_intsplit(spline,cutpoint,&tspline1,&tspline2);  

    num_points = tspline1->n + tspline2->n - spline->order + 1;
    
    if (!(point_chain = GET_POINT(num_points)))
        return(FALSE);

    for (i = 0; i < tspline2->n - 1; i++)       /* Re-order the polygon chain */
    {
        pntextract(tspline2->points,i,point);
        pntdeposit(point_chain,i,point);
    }

    for (i = spline->order - 2, j = tspline2->n - 1; i < tspline1->n; i++, j++)
    {
        pntextract(tspline1->points,i,point);
        pntdeposit(point_chain,j,point);
    }

    if (!(knots = GET_KNOT(num_points + spline->order)))
        return(FALSE);

    for (i = 0; i <= tspline2->n; i++)              /* Re-organize the knot vector */
        knots[i] = tspline2->knots[i] - tspline2->knots[0];       /* Cause it to start at zero */
                                                                               
    for (i = spline->order, j = tspline2->n+1; i < tspline1->n + spline->order; i++,j++)
        knots[j] = knots[j - 1] + (tspline1->knots[i] - tspline1->knots[i-1]);

    cutpoint = os_mid_knt(knots,num_points + spline->order, spline->order);
                                                                   
    /* Now split revised polygon */
    tmp_spline.order = spline->order;
    tmp_spline.knots = knots;
    tmp_spline.n = num_points;
    tmp_spline.points = point_chain;
    tmp_spline.closed = FALSE;

    oslo_intsplit(&tmp_spline,cutpoint,spline1,spline2);

    PUT(knots);
    PUT(point_chain);
    
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 June 1988
 *
 */
void                                                                         
oslo_recursive_subdiv(order,knots,ref_knots,point_chain,cur_order,i,j,points)
                        /* Description   :- This function is the recursive version of
                         *                  the Oslo algorithm subdivision routine
                         *                  algorithm 2 which outputs points [rp1]
                         *                  where points => Di,j
                         *
                         *                  Despite the recursion, the recusrsive
                         *                  version of this routine is more 
                         *                  efficient than the iterative as it performs
                         *                  less calculations due to the test for 
                         *                  P1 & P2 = 0.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
INT        order;       /* <i> Order of the b-splines in use */
double     knots[];     /* <i> The knot vector */
double     ref_knots[]; /* <i> The refined knot vector */
PTR_POINT  point_chain; /* <i> Point chain pointer */
INT        cur_order;   /* <i> Order of the current spline */
INT        i;           /* <i> Points to knot vectot elements */
INT        j;           /* <i> Loop index */
POINT      points;      /* <o> See description */
{                                                
    POINT       pp1;
    POINT       pp2;
    double      p1;
    double      p2;
    double      div;                                   
    INT         lorder;                        
    INT         jt;

    cur_order--;

    if (cur_order > 0)
    {
        lorder = order - cur_order;
        jt = j + lorder;
        p1 = ref_knots[ jt ] - knots[i];
        p2 = knots[ i + lorder] - ref_knots[ jt];

        if (p1 != 0.0)
            oslo_recursive_subdiv(order,knots,ref_knots,point_chain,cur_order,i,j,pp1);

        if (p2 != 0.0)
            oslo_recursive_subdiv(order,knots,ref_knots,point_chain,cur_order,i-1,j,pp2);
        
        if (p1 == 0.0)
        {
           points[X] = pp2[X];
           points[Y] = pp2[Y];
           points[Z] = pp2[Z];
           points[H] = pp2[H];
        }                     
        else if (p2 == 0.0)
        {
           points[X] = pp1[X];
           points[Y] = pp1[Y];
           points[Z] = pp1[Z];
           points[H] = pp1[H];
        }                 
        else
        {
           div = p1 + p2;
           points[X] = (p1 * pp1[X] + p2 * pp2[X]) / div;
           points[Y] = (p1 * pp1[Y] + p2 * pp2[Y]) / div;
           points[Z] = (p1 * pp1[Z] + p2 * pp2[Z]) / div;
           points[H] = (p1 * pp1[H] + p2 * pp2[H]) / div;
        }
        return;
    }
    pntextract(point_chain,i,points);
}






/*      @(#)  256.1 date 12/16/89 ds_nurb_split.c  */
/*+
 *
 *        Filename    : ds_nurb_split.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:22
 *        Last change : 89/12/16 19:22:53
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_split.c
 *
 *        This file contains the following routines:
 *        
 *        surf_flat
 *        surf_split
 *        surf_subdiv
 *        
-*/

     
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 June 1988
 *
 */
BOOLEAN                                                                         
surf_flat(surface,tol)
                        /* Description   :- This function checks to see if the surface
                         *                  is within the tolerance specified
                         *
                         * Return status :- TRUE if within tol
                         *                  FALSE outwith specified tolerance
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                  
DS_NURB_SURFACE_PTR surface; /* <i> The surface to check for flatness */
double              tol;     /* <i> The tolerance the surface has to be within */
{   
    POINT     plane;        /* The plane equation of the surface patch */
    POINT     point;        /* Point of surface control polyhedron */
    POINT     point00;      /* Point at u = 0.0, v = 0.0 */
    POINT     pointm0;      /* Point at u = 1.0, v = 0.0 */
    POINT     point0n;      /* Point at u = 0.0, v = 1.0 */
    POINT     vector;
    double    dist;
    INT       i,j;

    mshextract(surface->points,0,0,surface->n,point00);
    mshextract(surface->points,surface->m-1,0,surface->n,pointm0);
    mshextract(surface->points,0,surface->n-1,surface->n,point0n);

    if (!pln3p(point00,pointm0,point0n,TOL,plane))
        return(FALSE);      /* Failed to generate plane equation */
                   
    tol *= tol;     /* Square the tol as we compare it with the square of the dist */
              
    for (i = 0; i < surface->m; i++)
        for (j = 0; j < surface->n; j++)
        {
            mshextract(surface->points,i,j,surface->n,point);   /* Get a point */
            plnperp(plane,point,vector);        /* Get distance point is from plane */
            dist = veclensq(vector,3);          /* Get actual distance */
            if (!TOL_WITHIN(dist,tol))
                 return(FALSE);         /* Surface patch not flat */
        }

    return(TRUE);      /* Surface is flat so return TRUE */
}

                            
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 22 June 1988
 *
 */           
BOOLEAN
surf_split(surface,tsurface,usurface,vsurface,wsurface)
                        /* Description   :- This function  splits a controlling
                         *                  polyhedron into four sub-polyhedra.
                         *                  It extracts each row and column individually
                         *                  and splits it. 
                         *
                         * Return status :- nothing
                         *   
                         * Externals     :- Used/Modified
                         *
                         * Notes         :-
                         *
                         */                 
DS_NURB_SURFACE_PTR surface;   /* <i> The surface to be split */
DS_NURB_SURFACE_PTR *tsurface;  /* <o> The surface to be split */
DS_NURB_SURFACE_PTR *usurface;  /* <o> The surface to be split */
DS_NURB_SURFACE_PTR *vsurface;  /* <o> The surface to be split */
DS_NURB_SURFACE_PTR *wsurface;  /* <o> The surface to be split */
{   
/*               
 *            V ---------->
 *          U   --------------------------------------------
 *          |   |                    |                     |
 *          |   |         t          |         u           |
 *          |   |                    |                     |
 *          \/  --------------------------------------------
 *              |                    |                     |
 *              |         v          |          w          |
 *              |                    |                     |
 *              --------------------------------------------
 * 
 * 
 *              ----------------------
 *          u   | t & u   | v & w    |
 *              |         |          |
 *              ----------------------
 *              | t & v   | u & w    |
 *          v   |         |          |
 *              ----------------------
 *
 */
    DS_NURB_SPLINE      tmp_spline;                   
    DS_NURB_SPLINE_PTR  spline1;
    DS_NURB_SPLINE_PTR  spline2;
    DS_NURB_SPLINE_PTR  spline3;
    DS_NURB_SPLINE_PTR  spline4;
    DS_NURB_SPLINE_PTR  spline5;
    DS_NURB_SPLINE_PTR  spline6;

    PTR_POINT   tslab;
    PTR_POINT   tout1;
    PTR_POINT   tout2;
    PTR_POINT   ttmp;
    PTR_POINT   vtmp;

    double      mid;

    int         max_mn;
    int         i;

    if (!(tslab = GET_POINT(surface->m)))       /* Create a point chain as workspace */
        return(FALSE);
    
    MAX(surface->m,surface->n,max_mn);
               
    /* Create point chain for result of curve split */
    if (!(tout1 = GET_POINT(max_mn)))
        return(FALSE);

    if (!(tout2 = GET_POINT(max_mn)))
        return(FALSE);                                

    /* Create working meshes */
    if (!(ttmp = GET_MESH(surface->m,surface->n)))
        return(FALSE);

    if (!(vtmp = GET_MESH(surface->m,surface->n)))
        return(FALSE);

    if (!(*tsurface = GET_NURBSURFACE()))
        return(FALSE);              /* Failed to get memory for surface t */

    if (!(*usurface = GET_NURBSURFACE()))
        return(FALSE);              /* Failed to get memory for surface u */

    if (!(*vsurface = GET_NURBSURFACE()))
        return(FALSE);              /* Failed to get memory for surface v */

    if (!(*wsurface = GET_NURBSURFACE()))
        return(FALSE);              /* Failed to get memory for surface w */

/*
 *   Split this patch in the U direction
 *   This results in two controlling polyhedra
 *
 *   V ------>
 *   U
 *   |  -----------------------------
 *   |  |                           |
 *   |  |        ttmp               |
 *   |  |                           |
 *  \/  -----------------------------
 *      |                           |
 *      |        vtmp               |
 *      |                           |
 *      -----------------------------
 *
 *
 *
 */
      
    mid = os_mid_knt(surface->knots_u,surface->m + surface->order_u, surface->order_u);

    for (i = 0; i < surface->n; i++)
    {
        mshcolpnt(surface->points,i,surface->m,surface->n,tslab);

        tmp_spline.order  = surface->order_u;
        tmp_spline.n      = surface->m;
        tmp_spline.closed = surface->closed_u;
        tmp_spline.knots  = surface->knots_u;
        tmp_spline.points = tslab;

        oslo_split(&tmp_spline,mid,&spline1,&spline2);

        pntcolmsh(spline1->points,i,spline1->n,surface->n,ttmp);  /* Copy tout1 into the meshes t and v*/
        pntcolmsh(spline2->points,i,spline1->n,surface->n,vtmp);
                                           
     }

/* Split the first of these patches in v 
 * This results in two controlling polygons t and u 
 *
 *   V ------>
 *   U
 *   |  -----------------------------
 *   |  |            |              |
 *   |  |     t      |      u       |
 *   |  |            |              |
 *  \/  -----------------------------
 *      |                           |
 *      |        vtmp               |
 *      |                           |
 *      -----------------------------
 *
 *
 *
 */
                             
    mid = os_mid_knt(surface->knots_v,surface->n + surface->order_v, surface->order_v);

    for (i = 0; i < spline1->n; i++)
    {
        tmp_spline.order  = surface->order_v;
        tmp_spline.n      = surface->n;
        tmp_spline.closed = surface->closed_v;
        tmp_spline.knots  = surface->knots_v;
        tmp_spline.points = ttmp + (i * surface->n);

        oslo_split(&tmp_spline,mid,&spline3,&spline4);

        if (i == 0)
        {   /* Create meshes of the correct size, first time around */
            if (!((*tsurface)->points = GET_MESH(spline1->n,spline3->n)))
                return(FALSE);

            if (!((*usurface)->points = GET_MESH(spline1->n,spline4->n)))
                return(FALSE);
        }
        /* Copy rows tout1 and tout2 in the polyhedron mesh */
        pntrowmsh(spline3->points,i,spline3->n,(*tsurface)->points);
        pntrowmsh(spline4->points,i,spline4->n,(*usurface)->points);
    }                

/* Split the second in the v direction
 * This results in two controlling polyhedrons v and w 
 *
 *   V ------>
 *   U
 *   |  -----------------------------
 *   |  |            |              |
 *   |  |     t      |      u       |
 *   |  |            |              |
 *  \/  -----------------------------
 *      |            |              |
 *      |     v      |      w       |
 *      |            |              |
 *      -----------------------------
 *
 */

    for (i = 0; i < spline2->n; i++)
    {                                         
        tmp_spline.order  = surface->order_v;
        tmp_spline.n      = surface->n;
        tmp_spline.closed = surface->closed_v;
        tmp_spline.knots  = surface->knots_v;
        tmp_spline.points = vtmp + (i * surface->n);

        oslo_split(&tmp_spline,mid,&spline5,&spline6);

        if (i == 0)
        {
            if (!((*vsurface)->points = GET_MESH(spline1->n,spline5->n)))
                return(FALSE);
            if (!((*wsurface)->points = GET_MESH(spline2->n,spline6->n)))
                return(FALSE);
        }

        pntrowmsh(spline5->points,i,spline5->n,(*vsurface)->points);
        pntrowmsh(spline6->points,i,spline6->n,(*wsurface)->points);
    }                         

    (*tsurface)->order_u  = spline1->order;
    (*tsurface)->order_v  = spline3->order;
    (*usurface)->order_u  = spline1->order;
    (*usurface)->order_v  = spline4->order;
    (*vsurface)->order_u  = spline2->order;
    (*vsurface)->order_v  = spline5->order;
    (*wsurface)->order_u  = spline2->order;
    (*wsurface)->order_v  = spline6->order;

    (*tsurface)->closed_u = OPEN;
    (*tsurface)->closed_v = OPEN;
    (*usurface)->closed_u = OPEN;
    (*usurface)->closed_v = OPEN;
    (*vsurface)->closed_u = OPEN;
    (*vsurface)->closed_v = OPEN;
    (*wsurface)->closed_u = OPEN;
    (*wsurface)->closed_v = OPEN;

    (*tsurface)->m = spline1->n;
    (*tsurface)->n = spline3->n;
    (*usurface)->m = spline1->n;
    (*usurface)->n = spline4->n;
    (*vsurface)->m = spline2->n;
    (*vsurface)->n = spline5->n;
    (*wsurface)->m = spline2->n;
    (*wsurface)->n = spline6->n;

    if (!((*tsurface)->knots_u = GET_KNOT((*tsurface)->m + (*tsurface)->order_u)))
        return(FALSE);

    if (!((*tsurface)->knots_v = GET_KNOT((*tsurface)->n + (*tsurface)->order_v)))
        return(FALSE);

    if (!((*usurface)->knots_u = GET_KNOT((*usurface)->m + (*usurface)->order_u)))
        return(FALSE);

    if (!((*usurface)->knots_v = GET_KNOT((*usurface)->n + (*usurface)->order_v)))
        return(FALSE);

    if (!((*vsurface)->knots_u = GET_KNOT((*vsurface)->m + (*vsurface)->order_u)))
        return(FALSE);

    if (!((*vsurface)->knots_v = GET_KNOT((*vsurface)->n + (*vsurface)->order_v)))
        return(FALSE);

    if (!((*wsurface)->knots_u = GET_KNOT((*wsurface)->m + (*wsurface)->order_u)))
        return(FALSE);

    if (!((*wsurface)->knots_v = GET_KNOT((*wsurface)->n + (*wsurface)->order_v)))
        return(FALSE);                           

    knt_copy(spline1->knots,(*tsurface)->m + (*tsurface)->order_u,(*tsurface)->knots_u);
    knt_copy(spline5->knots,(*tsurface)->n + (*tsurface)->order_v,(*tsurface)->knots_v);

    knt_copy(spline1->knots,(*usurface)->m + (*usurface)->order_u,(*usurface)->knots_u);
    knt_copy(spline6->knots,(*usurface)->n + (*usurface)->order_v,(*usurface)->knots_v);

    knt_copy(spline2->knots,(*vsurface)->m + (*vsurface)->order_u,(*vsurface)->knots_u);
    knt_copy(spline5->knots,(*vsurface)->n + (*vsurface)->order_v,(*vsurface)->knots_v);

    knt_copy(spline1->knots,(*wsurface)->m + (*wsurface)->order_u,(*wsurface)->knots_u);
    knt_copy(spline6->knots,(*wsurface)->n + (*wsurface)->order_v,(*wsurface)->knots_v);
              
    /* Free the working memory */
    PUT(tslab);
    PUT(vtmp);
    PUT(ttmp);
    PUT(tout2);
    PUT(tout1);                                     
    PUT(spline1->knots);
    PUT(spline1->points);
    PUT(spline2->knots);
    PUT(spline2->points);
    PUT(spline3->knots);
    PUT(spline3->points);
    PUT(spline4->knots);
    PUT(spline4->points);
    PUT(spline5->knots);
    PUT(spline5->points);
    PUT(spline6->knots);
    PUT(spline6->points);

    return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 23 June 1988
 *
 */
BOOLEAN                                                                         
surf_subdiv(surface,tol)
                        /* Description   :- This function actaully subdivides a surface
                         *                  by calling surf_split which does most of
                         *                  the work 
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                         
DS_NURB_SURFACE_PTR  surface;  /* <i> The surface to be split */
double               tol;      /* <i> The flatness tolerance of the surface */
{      
    DS_NURB_SURFACE_PTR  tsurface;
    DS_NURB_SURFACE_PTR  usurface;
    DS_NURB_SURFACE_PTR  vsurface;
    DS_NURB_SURFACE_PTR  wsurface;
                          
    if (!surf_split(surface,&tsurface,&usurface,&vsurface,&wsurface))
        return(FALSE);   /* Error occurred so return causing us to pop */

    if (!surf_flat(tsurface,tol))
    {
        if (!surf_subdiv(tsurface,tol))
            return(FALSE);
    }                     
    else
    {                                    
        delete_nurbsurface(&tsurface);
    }

    if (!surf_flat(usurface,tol))
    {
        if (!surf_subdiv(usurface,tol))
            return(FALSE);
    }
    else
    {
        delete_nurbsurface(&usurface);
    }

    if (!surf_flat(vsurface,tol))
    {
        if (!surf_subdiv(vsurface,tol))
            return(FALSE);
    }
    else
    {                   
        delete_nurbsurface(&vsurface);
    }

    if (!surf_flat(wsurface,tol))
    {
        if (!surf_subdiv(wsurface,tol))
            return(FALSE);
    }
    else
    {
        delete_nurbsurface(&wsurface);
    }

    return( TRUE );
}                       



/*      @(#)  256.1 date 12/16/89 ds_nurb_trans.c  */
/*+
 *
 *        Filename    : ds_nurb_trans.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:28
 *        Last change : 89/12/16 19:23:03
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_nurb_trans.c
 *
 *        This file contains the following routines:
 *
 *        move_nurbspline
 *        rotate_nurbspline
 *        move_nurbsurface
 *        rotate_nurbsurface
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th-August-1988
 *
 */
BOOLEAN
move_nurbspline( spline, disp )
                        /* Description   :- This function moves a spline by a 
                         *                  displacement vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR spline; /* <i> Pointer to the spline */
VECTOR             disp;   /* <i> The displacement vector */
{
      POINT  point;
      VECTOR disp_h;
      INT    i;

      if(!spline)
      {                                                /* No spline! */
            return(FALSE);
      }
      for( i = 0; i < spline->n; i++)
      {
            pntextract( spline->points, i, point);
            POINT3D_TO_H( disp, point[H], disp_h);     /* Convert Displacement to  */
            vecsum( point, disp_h, CART, point );      /* homogeneous space */
            pntdeposit( spline->points, i, point);
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th-August-1988
 *
 */
BOOLEAN
rotate_nurbspline( spline, disp, axis, angle )
                        /* Description   :- This function rotates a spline about an axis
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SPLINE_PTR spline; /* <i> Pointer to the spline */
POINT              disp;   /* <i> The displacement vector */
VECTOR             axis;   /* <i> The axis about which the spline is to be rotated */
double             angle;  /* <i> The angle of rotation in radians */
{
      POINT  point;
      INT    i;

      if(!spline)
      {                                                /* No spline! */
            return(FALSE);
      }
      if(TOL_WITHIN(veclen(axis,CART),TOL))
      {                                                /* No axis! */
            return(FALSE);
      }

      calc_transformation( disp, axis, angle );        /* Calculate rotation matrix */

      for( i = 0; i < spline->n; i++)
      {
            pntextract( spline->points, i, point);
            rot_h_point( point, point);
            pntdeposit( spline->points, i, point);
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th-August-1988
 *
 */
BOOLEAN
move_nurbsurface( surface, disp )
                        /* Description   :- This function moves a surface by a 
                         *                  displacement vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SURFACE_PTR surface; /* <i> Pointer to the surface */
VECTOR              disp;    /* <i> The displacement vector */
{
      POINT  point;
      VECTOR disp_h;
      INT    i;

      if(!surface)
      {                                                /* No surface! */
            return(FALSE);
      }
      for( i = 0; i < (surface->m * surface->n); i++)
      {
            pntextract( surface->points, i, point);
            POINT3D_TO_H( disp, point[H], disp_h);     /* Convert Displacement to  */
            vecsum( point, disp_h, CART, point );      /* homogeneous space */
            pntdeposit( surface->points, i, point);
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th-August-1988
 *
 */
BOOLEAN
rotate_nurbsurface( surface, disp, axis, angle )
                        /* Description   :- This function rotates a surface about an axis
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
DS_NURB_SURFACE_PTR surface; /* <i> Pointer to the surface */
POINT               disp;   /* <i> The displacement vector */
VECTOR              axis;   /* <i> The axis about which the surface is to be rotated */
double              angle;  /* <i> The angle of rotation in radians */
{
      POINT  point;
      INT    i;

      if(!surface)
      {                                                /* No surface! */
            return(FALSE);
      }
      if(TOL_WITHIN(veclen(axis,CART),TOL))
      {                                                /* No axis! */
            return(FALSE);
      }

      calc_transformation( disp, axis, angle );        /* Calculate rotation matrix */

      for( i = 0; i < (surface->m * surface->n); i++)
      {
            pntextract( surface->points, i, point);
            rot_h_point( point, point);
            pntdeposit( surface->points, i, point);
      }
      return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 ds_point.c  */
/*+
 *
 *
 *        Filename    : ds_point.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:34
 *        Last change : 89/12/16 19:23:13
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ds_point.c
 *
 *        This file contains the following routines:
 *
 *             pntextract
 *             pntdeposit
 *             pntcopy
 *             pntclear
 *             pntmax
 *             pntmin
 *             pntminmax
 *             pntrev
 *             pntmove
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
void
pntextract( ptr, index, point )
                        /* Description   :- This function extracts a point from
                         *                  a homogeneous point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT   ptr;        /* <i> Pointer to the point array */
int         index;      /* <i> The index into the array */
POINT       point;      /* <o> The resultant homogeneous point */
{
      point[X] = (*(ptr + index))[X];
      point[Y] = (*(ptr + index))[Y];
      point[Z] = (*(ptr + index))[Z];
      point[H] = (*(ptr + index))[H];
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntdeposit( ptr, index, point )
                        /* Description   :- This function deposits a point into
                         *                  a homogeneous point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr;        /* <i> Pointer to the point array */
int         index;      /* <i> The index into the array */
POINT     point;      /* <i> The homogeneous point to be deposited */
{                       
      (*(ptr + index))[X] = point[X];
      (*(ptr + index))[Y] = point[Y];
      (*(ptr + index))[Z] = point[Z];
      (*(ptr + index))[H] = point[H];
  
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntcopy( ptr_in, length, ptr_out )
                        /* Description   :- This function copies one homogeneous point
                         *                  array to another
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The length of the array */
PTR_POINT ptr_out;    /* <i> Pointer to the output point array */
{                                         
    POINT point;
    int     i;

    for( i = 0; i < length; i++ )
    {
        pntextract( ptr_in, i, point );
        pntdeposit( ptr_out, i, point );
    }

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntclear( ptr_in, length )
                        /* Description   :- This function clears a homogeneous point
                         *                  array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The length of the array */
{                                         
    POINT point;
    int     i;

    vecclr( point, 4 );

    for( i = 0; i < length; i++ )
    {
        pntdeposit( ptr_in, i, point );
    }

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntmax( ptr_in, length, max_point )
                        /* Description   :- This function calculates the maximum 
                         *                  X, Y, Z and H values of the given point
                         *                  array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The number of points in the array */
POINT     max_point;  /* <o> The maximum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, max_point );
    POINTH_TO_3D(max_point,max_point);

    for( i = 1; i < length ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] > max_point[j] )
            {
                max_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntmin( ptr_in, length, min_point )
                        /* Description   :- This function calculates the miniimum 
                         *                  X, Y, Z and H values of the given point
                         *                  array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The number of points in the array */
POINT     min_point;  /* <o> The minimum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, min_point );
    POINTH_TO_3D(min_point,min_point);

    for( i = 1; i < length ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] < min_point[j] )
            {
                min_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntminmax( ptr_in, length, min_point, max_point )
                        /* Description   :- This function calculates the minimum 
                         *                  and maximum X, Y, Z and H values of the 
                         *                  given point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The number of points in the array */
POINT     min_point;  /* <o> The minimum X, Y, Z and H values */
POINT     max_point;  /* <o> The maximum X, Y, Z and H values */
{                                         
    POINT point;
    int     i;
    int     j;

    pntextract( ptr_in, 0, min_point );
    pntextract( ptr_in, 0, max_point );
    POINTH_TO_3D(min_point,min_point);
    POINTH_TO_3D(max_point,max_point);

    for( i = 1; i < length ; i++ )
    {
        pntextract( ptr_in, i, point );
        POINTH_TO_3D(point,point);

        for( j = 0; j < 4; j++ )
        {
            if( point[j] < min_point[j] )
            {
                min_point[j] = point[j];
             }
            else
            if( point[j] > max_point[j] )
            {
                max_point[j] = point[j];
             }
        }
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntrev( ptr_in, length, ptr_out )
                        /* Description   :- This function reverses the sequence of 
                         *                  elements in a point array.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The number of points in the array */
PTR_POINT ptr_out;    /* <o> Pointer to the resultant point chain */
{                                         
    POINT point1;
    POINT point2;
    int     i;
    int     j;
    int     half;

    half = length/2;

    for( i = 0, j = length - 1; i < half; i++, j-- )
    {
        pntextract( ptr_in, i, point1 );
        pntextract( ptr_in, j, point2 );
        pntdeposit( ptr_out, i, point2 );
        pntdeposit( ptr_out, j, point1 );
    }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 10th-May--1988
 *
 */
BOOLEAN
pntmove( ptr_in, length, vector, ptr_out )
                        /* Description   :- This function moves all the elements of
                         *                  a point array by a displacement vector.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */
PTR_POINT ptr_in;     /* <i> Pointer to the input point array */
int         length;     /* <i> The number of points in the array */
VECTOR      vector;     /* <i> The displacement vector */
PTR_POINT ptr_out;    /* <o> Pointer to the resultant point chain */
{                                         
    POINT point;
    int     i;

    for( i = 0; i < length; i++ )
    {
        pntextract( ptr_in, i, point );
        vecsum( point, vector, 3, point );
        pntdeposit( ptr_out, i, point );
    }
      return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 ellipse_user.c  */

/*
 *
 *        Filename    : ellipse_user.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:49:41
 *        Last change : 89/12/16 19:23:23
 *
 *        Copyright : Practical Technology Limited  
 *        File :- ellipse_user.c
 *
 *        This file contains the following routines 
 *
 *
 *
 */


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 11 Nov 1988
 *
 */
void
e2and4(cx,cy,ax,ay,angle,ecx,ecy,maj_rad,min_rad,inc_ang,sta_ang,end_ang,status)
                        /* Description   :- This function takes a centre point and a
                         *                  point at the end of the axis and an angle 
                         *                  to rotate the circle around the axis by to
                         *                  generate an ellipse.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                     
double   *cx;           /* <i> X co-ord of centre point */
double   *cy;           /* <i> Y co-ord of centre point */
double   *ax;           /* <i> X co-ord of axis point   */
double   *ay;           /* <i> Y co-ord of axis point   */
double   *angle;        /* <i> The angle to rotate the circle by */
double   *ecx;          /* <o> The X co-ord centre point of the ellipse */
double   *ecy;          /* <o> The Y co-ord centre point of the ellipse */
double   *maj_rad;      /* <o> The major radius */
double   *min_rad;      /* <o> The minor radius */
double   *inc_ang;      /* <o> The increment angle */
double   *sta_ang;      /* <o> The start angle  */
double   *end_ang;      /* <o> The end angle    */
int32_t     *status;       /* <o> The status of the call */
{                     
    POINT_2D  centre;
    POINT_2D  axis_end;  
    POINT_2D  axis_dir; 
    POINT_2D  vec;  
    POINT     axis_rot;
    POINT     x_axis;
    POINT     paxis_dir;  
    POINT     point;
    double    radius;

    centre[X] = *cx;
    centre[Y] = *cy;
    axis_end[X] = *ax;
    axis_end[Y] = *ay;

    vecdif(centre,axis_end,2,axis_dir);
    radius = veclen(axis_dir,2);    /* Get the radius of the circle */

    veccpy(axis_dir,2,axis_rot);    /* Copy to a 3d axis */
    axis_rot[Z] = 0;                /* Make a z axis rot of zero */

    /* Calculate the perpindicular vector so we can rotate it */
    perp_vector(axis_rot,paxis_dir);

    vecclr(point,3);
    vecnrm(paxis_dir,3,TOL,paxis_dir);      /* Normalise the vector */
    vecsca(radius,paxis_dir,3,paxis_dir);  /* Scale the vector */
    vecsum(centre,paxis_dir,2,point);

    /* Set up the rotation and the calculate it */
    calc_transformation(centre,axis_rot,*angle);
    rot_point(point,point);             /* Rotate the point */

    /* Now drop the point onto the xy plane to get the length of the minor radius */
    vecdif(centre,point,2,vec);
    *min_rad = veclen(vec,2);                                                
                                                                                    
    /* Get the length of the major radius */
    vecdif(centre,axis_end,2,vec);              
    *maj_rad = veclen(vec,2);

    /* Now calculate the incline angle */
    vecclr(x_axis,3);
    x_axis[X] = 1.0;
    vecang(x_axis,axis_dir,2,TOL,inc_ang);
    if (axis_dir[Y] < 0.0)
       *inc_ang = C$2PI - (*inc_ang);

    /* Deposit the centre of the ellipse */
    *ecx = centre[X];
    *ecy = centre[Y];

    *status = FORTRAN_OKAY;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 11 Nov 1988
 *
 */
void
e3(cx,cy,majx,majy,minx,miny,ecx,ecy,maj_rad,min_rad,
           inc_ang,sta_ang,end_ang,status)
                        /* Description   :- This function generates an ellipse from the
                         *                  centre point and the two points for the
                         *                  end of the major and minor axes.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double   *cx;           /* <i> X co-ord of centre point */
double   *cy;           /* <i> Y co-ord of centre point */
double   *majx;         /* <i> X co-ord of major axis point */
double   *majy;         /* <i> Y co-ord of major axis point */
double   *minx;         /* <i> X co-ord of minor axis point */
double   *miny;         /* <i> Y co-ord of minor axis point */
double   *ecx;          /* <o> The X co-ord centre point of the ellipse */
double   *ecy;          /* <o> The Y co-ord centre point of the ellipse */
double   *maj_rad;      /* <o> The major radius */
double   *min_rad;      /* <o> The minor radius */
double   *inc_ang;      /* <o> The increment angle */
double   *sta_ang;      /* <o> The start angle  */
double   *end_ang;      /* <o> The end angle    */
int32_t     *status;       /* <o> The status of the call */
{
    POINT_2D    centre;
    POINT_2D    max;
    POINT_2D    min;
    POINT_2D    vec1;
    POINT_2D    vec2;  
    POINT_2D    x_axis;

    double      pangle;

    centre[X] = *cx;
    centre[Y] = *cy;
    max[X] = *majx;
    max[Y] = *majy;
    min[X] = *minx;
    min[Y] = *miny;

    vecdif(max,centre,2,vec1);
    vecdif(min,centre,2,vec2);
    /* Check to see that the two points are at right angle to each other */
    vecang(vec1,vec2,2,TOL,&pangle);

    if (!TOL_WITHIN((pangle - C$PIBY2),TOL))
        *status = FORTRAN_ERROR;
    else
    {                     
        /* Calculate the angle between the x-axis and the major axis */
        x_axis[X] = 1.0;
        x_axis[Y] = 0.0;
        vecang(vec1,x_axis,2,TOL,inc_ang);  
        if (vec1[Y] < 0.0)
            *inc_ang = C$2PI - (*inc_ang);
        *maj_rad = veclen(vec1,2);
        *min_rad = veclen(vec2,2);          
    }

    *ecx = centre[X];
    *ecy = centre[Y];

    *status = FORTRAN_OKAY;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 11 Nov 1988
 *
 */
void
e1(ax1,ay1,ax2,ay2,epointx,epointy,ecx,ecy,maj_rad,min_rad,
           inc_ang,sta_ang,end_ang,status)
                        /* Description   :- This function generates an ellipse from the
                         *                  two points defining the major axis and one point on the
                         *                  ellipse.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double   *ax1;          /* <i> X co-ord of 1st axis point */
double   *ay1;          /* <i> Y co-ord of 1st axis point */
double   *ax2;          /* <i> X co-ord of 2nd axis point */
double   *ay2;          /* <i> Y co-ord of 2nd axis point */
double   *epointx;      /* <i> X co-ord of point on ellipse */
double   *epointy;      /* <i> Y co-ord of point on ellipse */
double   *ecx;          /* <o> The X co-ord centre point of the ellipse */
double   *ecy;          /* <o> The Y co-ord centre point of the ellipse */
double   *maj_rad;      /* <o> The major radius */
double   *min_rad;      /* <o> The minor radius */
double   *inc_ang;      /* <o> The increment angle */
double   *sta_ang;      /* <o> The start angle  */
double   *end_ang;      /* <o> The end angle    */
int32_t     *status;       /* <o> The status of the call */
{
    POINT_2D     centre;
    POINT_2D     point;                                                              
    POINT_2D     x_axis;
    POINT_2D     axis;
    POINT        z_axis;  
    double       angle;
                     
    /* Calculate the centre of the ellipse */
    centre[X] = (*ax2 - *ax1)/2.0 + *ax1;
    centre[Y] = (*ay2 - *ay1)/2.0 + *ay1;
                                             
    /* Take the first axis point and calculate the angle between it and the x-axis */
    point[X] = *ax1 - centre[X];
    point[Y] = *ay1 - centre[Y];    
                               
    x_axis[X] = 1.0;
    x_axis[Y] = 0.0;
                                          
    /* Calculate the angle between the axis and the x-axis ie the incline angle */
    vecang(point,x_axis,2,TOL,inc_ang);                                            
    if (point[Y] < 0.0)
        *inc_ang = C$2PI - (*inc_ang);

    z_axis[X] = 0.0;
    z_axis[Y] = 0.0;
    z_axis[Z] = 1.0;
    /* Now rotate the points back by the negative incline angle */
    calc_transformation(centre,z_axis,-(*inc_ang));

    axis[X] = *ax1;
    axis[Y] = *ay1;
    rot_point(axis,axis);
    vecdif(centre,axis,2,axis);

    /* Calculate the major radius */
    *maj_rad = veclen(axis,2);
  
    point[X] = *epointx;
    point[Y] = *epointy;
    rot_point(point,point);             /* Rotate the point on the ellipse */

    /* Now calulate the angle between it and the x -axis so we can then calculate */
    /* the size of the radius */
    vecang(point,x_axis,2,TOL,&angle);
               
    *min_rad = (point[Y] - centre[Y] ) / sin(angle);

    *ecx = centre[X];
    *ecy = centre[Y];

    *status = FORTRAN_OKAY;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 14 Nov 1988
 *
 */
void
e7(ax1,ay1,ax2,ay2,tanpx,tanpy,tanvx,tanvy,maj_flag,ecx,ecy,maj_rad,
           min_rad,inc_ang,sta_ang,end_ang,status)
                        /* Description   :- This function generates an ellipse from the
                         *                  two points defining the axes and one point on the
                         *                  ellipse.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double   *ax1;          /* <i> X co-ord of 1st axis point */
double   *ay1;          /* <i> Y co-ord of 1st axis point */
double   *ax2;          /* <i> X co-ord of 2nd axis point */
double   *ay2;          /* <i> Y co-ord of 2nd axis point */
double   *tanpx;        /* <i> X co-ord of point on tangent */
double   *tanpy;        /* <i> Y co-ord of point on tangent */
double   *tanvx;        /* <i> X co-ord of tangent vector */
double   *tanvy;        /* <i> Y co-ord of tangent vector */
int32_t     *maj_flag;     /* <i> If true the axis is the major radius */
                        /*     else the minor radius */
double   *ecx;          /* <o> The X co-ord centre point of the ellipse */
double   *ecy;          /* <o> The Y co-ord centre point of the ellipse */
double   *maj_rad;      /* <o> The major radius */
double   *min_rad;      /* <o> The minor radius */
double   *inc_ang;      /* <o> The increment angle */
double   *sta_ang;      /* <o> The start angle  */
double   *end_ang;      /* <o> The end angle    */
int32_t     *status;       /* <o> The status of the call */
{                           
    POINT_2D   axis_start;
    POINT_2D   axis_end;
    POINT_2D   ecentre;   
    POINT_2D   tanp;
    POINT_2D   tanv;                 
    POINT_2D   x1;
    POINT_2D   x2;
    POINT_2D   focus;
    POINT_2D   S;        
    POINT_2D   Q;
    POINT_2D   circumference;
    POINT      x_axis;       
    POINT      y_axis;
    VECTOR_2D  vec;     
    VECTOR     tanv3d;
    VECTOR     perp_vec;
    VECTOR     perp_axis;
    VECTOR     perp_tangent;
    VECTOR     maj_dir;                                      
    VECTOR     min_dir;                    

    double     s;
    double     t;                          
    double     tanvs[2][2];
    double     tanps[2][2];

    int32_t       num;
        
    axis_start[X] = *ax1;
    axis_start[Y] = *ay1;
    axis_end[X] = *ax2;  
    axis_end[Y] = *ay2;
    tanp[X] = *tanpx;
    tanp[Y] = *tanpy;
    tanv[X] = *tanvx;
    tanv[Y] = *tanvy;
    /* Calculate the centre of the axis */
    vecdif(axis_end,axis_start,2,ecentre); 
    vecsca(0.5,ecentre,2,ecentre);              
    vecsum(axis_start,ecentre,2,ecentre);

    /* Get the direction of the major axis */
    vecdif(axis_start,ecentre,2,maj_dir);
    *maj_rad = veclen(maj_dir,2);

    /* Calculate the incline angle (angle between the major axis */
    /* and the x-axis */       
    vecclr(x_axis,3);
    x_axis[X] = 1.0;
    vecang(maj_dir,x_axis,2,TOL,inc_ang);
    if (maj_dir[Y] < 0.0)
       *inc_ang = C$2PI - (*inc_ang);

    if (*maj_flag)
    {   /* Axis defined is the major axis */
        /* Intersect the tangent with the circle of ecentre and length of*/
        /* the major radius */                
        if (!line_x_circle2D(tanp,tanv,ecentre,*maj_rad,TOL,x1,x2))
        {
            *status = FORTRAN_ERROR;
            return;
        }

        /* Take one of the tangent points that intersect the circle and */
        /* project a perpendicular line from it to intersect the major */
        /* axis this gives us a focus point */
        tanv3d[X] = tanv[X];
        tanv3d[Y] = tanv[Y];
        tanv3d[Z] = 0.0;
        perp_vector(tanv3d,perp_vec);
                           
        if (!line_x_line2D(axis_start,maj_dir,x1,perp_vec,TOL,focus,&s,&t))
        {
            *status = FORTRAN_ERROR;
            return;
        }

        /* Take a radius of CA (half the major axis length) and with a */
        /* centre of F describe arcs to the minor axis in B and B' this */
        /* gives us the length of the minor axes */          
        perp_vector(maj_dir,min_dir);
        if (!line_x_circle2D(ecentre,min_dir,focus,*maj_rad,TOL,x1,x2))
        {
            *status = FORTRAN_ERROR;
            return;
        }
        vecdif(x1,ecentre,2,vec);
        *min_rad = veclen(vec,2);   
    }
    else
    {   /* Axis defined is the minor axis */
        /* Calculate the intersection of the tangent and the minor axis */
        vecdif(axis_start,axis_end,2,min_dir);
        if (!line_x_line2D(ecentre,min_dir,tanp,tanv,TOL,S,&s,&t))
        {
            *status = FORTRAN_ERROR;
            return;
        }

        /* Draw the tangent that touches the minor circle and passes */
        /* through the point of intersection */ 
        circ_tans_to_pnt(S,ecentre,*min_rad,TOL,tanvs,tanps,&num);
    
        /* Take the first tangent and generate a perpindicular line off it */
        /* That passes through the centre of the minor circle */
        perp_vector(tanvs[0],perp_tangent);
        
        /* Generate a line perpindicular to the minor axis that passes */
        /* through Q1 and caclulate where it intersects the ellipse */
        /* tangent */
        perp_vector(min_dir,perp_axis);
        if (!line_x_line2D(tanps[0],perp_axis,tanp,tanv,TOL,Q,&s,&t))
        {
            *status = FORTRAN_ERROR;
            return;
        }

        /* Now generate a line parallel to the minor axes that intersects */
        /* the perpindicular line */
        if (!line_x_line2D(Q,min_dir,tanps[0],perp_tangent,TOL,circumference,&s,&t))
        {
            *status = FORTRAN_ERROR;
            return;
        }

        vecdif(circumference,ecentre,2,vec);
        *maj_rad = veclen(vec,2);
    }
    *ecx = ecentre[X];
    *ecy = ecentre[Y];
    *status = FORTRAN_OKAY;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 15 Nov1988
 *
 */
void
e6(focx1,focy1,focx2,focy2,tanpx,tanpy,tanvx,tanvy,ecx,ecy,maj_rad,
           min_rad,inc_ang,sta_ang,end_ang,status)
                        /* Description   :- This function generates an ellipse from the
                         *                  two points defining the axes and one point on the
                         *                  ellipse.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double   *focx1;        /* <i> X co-ord of 1st focus point */
double   *focy1;        /* <i> Y co-ord of 1st focus point */
double   *focx2;        /* <i> X co-ord of 2nd focus point */
double   *focy2;        /* <i> Y co-ord of 2nd focus point */
double   *tanpx;        /* <i> X co-ord of point on tangent */
double   *tanpy;        /* <i> Y co-ord of point on tangent */
double   *tanvx;        /* <i> X co-ord of tangent vector */
double   *tanvy;        /* <i> Y co-ord of tangent vector */
double   *ecx;          /* <o> The X co-ord centre point of the ellipse */
double   *ecy;          /* <o> The Y co-ord centre point of the ellipse */
double   *maj_rad;      /* <o> The major radius */
double   *min_rad;      /* <o> The minor radius */
double   *inc_ang;      /* <o> The increment angle */
double   *sta_ang;      /* <o> The start angle  */
double   *end_ang;      /* <o> The end angle    */
int32_t     *status;       /* <o> The status of the call */
{
    POINT     foci;                                
    POINT     foci1;
    POINT     tanv;                                       
    POINT     tanp;
    POINT     perp_tanv;
    POINT     H1;
    POINT_2D  ecentre;                                
    POINT_2D  x1;
    POINT_2D  x2;


    VECTOR    maj_axis;
    VECTOR    min_axis;
    VECTOR    x_axis;
    VECTOR    vec;

    double  s;
    double  t;

    vecclr(tanv,3);
    tanv[X] = *tanvx;
    tanv[Y] = *tanvy;

    tanp[X] = *tanpx;
    tanp[Y] = *tanpy;

    foci[X] = *focx1;
    foci[Y] = *focy1;              

    foci1[X] = *focx2;
    foci1[Y] = *focy2;

    /* Calculate the perpindicular to the given tangent */
    perp_vector(tanv,perp_tanv);

    /* Pass it through the first focal point */
    /* and intersect it with the tangent and call that point H1 */
    if (!line_x_line2D(foci,perp_tanv,tanp,tanv,TOL,H1,&s,&t))
    {
        *status = FORTRAN_ERROR;
        return;
    }

    /* That point is on the major auxillary circle */
    /* Calculate the centre of the ellipse and then work out the */
    /* distance from H to the centre which is the major radius */
    vecdif(foci,foci1,2,maj_axis);  
    vecsca(0.5,maj_axis,2,maj_axis);
    vecsum(maj_axis,foci1,2,ecentre);

    vecdif(H1,ecentre,2,vec);
    *maj_rad = veclen(vec,2);

    /* Now calculate the minor axis ie perpindicular to the major axis */
    /* Then intersect a circle of radius maj_rad of centre foci get */
    /* the intersect */
    perp_vector(maj_axis,min_axis);
    if (!line_x_circle2D(ecentre,min_axis,foci,*maj_rad,TOL,x1,x2))
    {
        *status = FORTRAN_ERROR;
        return;
    }
    vecdif(x1,ecentre,2,vec);
    *min_rad = veclen(vec,2);   

    /* Now calculate the incline angle */
    vecclr(x_axis,3);
    x_axis[X] = 1.0;

    vecang(maj_axis,x_axis,2,TOL,inc_ang);
    if (maj_axis[Y] < 0.0)
        *inc_ang = C$2PI - (*inc_ang);

    *ecx = ecentre[X];
    *ecy = ecentre[Y];

    *status = FORTRAN_OKAY;                                       
}

/*      @(#)  256.1 date 12/16/89 geometry.c  */
/*+
 *
 *        Filename    : geometry.c
 *        Version     : 1.5
 *        Retrieved   : 89/03/29 13:54:21
 *        Last change : 89/03/23 13:20:49
 *
 *        Copyright : Practical Technology Limited  
 *        File :- geometry.c
 *
 *        This file contains the following routines:
 *
 *             Fortran interface routines 
 *              
 *              cctc
 *              cctl
 *              cctclp
 *              cctcp
 *              cctlc
 *              cctlp
 *              cctl2p
 *              cctc2p
 *              cct2c
 *              cct2cl
 *              cct2cp
 *              cct2l
 *              cct2lc
 *              cct2lp
 *              cct3c
 *              cct3l
 *              
 *              End of fortran interface routines 
 *
 *              centres_of_similitude
 *              circ_diametric
 *              circ_internal
 *              circ_tans_to_pnt
 *              circ_tanto_circ
 *              circ_tanto_lin
 *             *circ_tanto_circ_lin_pnt
 *             *circ_tanto_circ_pnt
 *             *circ_tanto_lin_circ
 *             *circ_tanto_lin_pnt
 *             *circ_tanto_lin_2pnts
 *             *circ_tanto_circ_2pnts
 *             *circ_tanto_2circs
 *             *circ_tanto_2circs_lin
 *             *circ_tanto_2circs_pnt
 *             *circ_tanto_2lin
 *             *circ_tanto_2lin_circ
 *             *circ_tanto_2lin_pnt
 *             *circ_tanto_3circs
 *              circ_tanto_3circs_o
 *             *circ_tanto_3lin
 *              circ_x_circ
 *              circ_x_lin
 *              circumcentre
 *              implicit_to_para
 *              incentre
 *              para_to_implicit
 *
 *      Note : Many of the routines are derived from algorithms and mathematics
 *             described in the following books:
 *
 *             "A programmer's geometry" by Adrian Bowyer and John Woodwark.
 *             Published by Butterworths ISBN 0-408-01242-0 Pbk.
 *
 *             "Practical Geometry and graphics" by David A Low.
 *             Published by Longmans, Green and Co. New impression August 1956.
 *             Available in Strathclyde University library.
 *
 *             "Practical Plane Geometry" by J. Harrison and G.A. Baxandall.
 *             Published by MacMillan and Co. 1914.
 *             Available in Strathclyde University library.
 *
-*/
  


                    
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 16 Dec 1988
 *
 */
BOOLEAN                                                                       
CCTC(cx,cy,gcx,gcy,g_radius,radius1,radius2,num_results,status)
                        /* Description   :- This function takes a circle and a centre
                         *                  point and calculates the two resultant
                         *                  circle solutions.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  *cx;            /* <i> X-Coord Centre of the required circle */
double  *cy;            /* <i> Y-Coord Centre of the required circle */
double  *gcx;           /* <i> X-Coord Centre of the given circle    */
double  *gcy;           /* <i> Y-Coord Centre of the given circle    */
double  *g_radius;      /* <i> The radius of the given circle        */
double  *radius1;       /* <o> The first radius of the solution      */
double  *radius2;       /* <o> The second radius of the solution     */
int32_t    *num_results;   /* <o> The number of solutions               */
int32_t    *status;        /* <o> The status of the call                */
                        /*     0  -> Successful call                 */
                        /*     -1 -> Unsuccessful call               */
{                  
    double   centre[2];
    double   g_centre[2];

    centre[X]   = *cx;
    centre[Y]   = *cy;
    g_centre[X] = *gcx;
    g_centre[Y] = *gcy;

    if (!circ_tanto_circ( centre, g_centre, *g_radius, radius1, radius2, num_results))
    {
        *num_results = 0;
        *status = FORTRAN_ERROR;
    }
    else                          
    {
        *status = FORTRAN_OKAY;
    }
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 16 Dec 1988
 *
 */
BOOLEAN                                                                       
CCTL(cx,cy,pointx,pointy,dirx,diry,radius,num_results,status)
                        /* Description   :- This function generates the circle that
                         *                  is tangent to the line with centre (cx,cy)
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                    
double   *cx;           /* <i> The X coord centre of the required circle */
double   *cy;           /* <i> The Y coord centre of the required circle */
double   *pointx;       /* <i> The X coord of a point on the line        */
double   *pointy;       /* <i> The Y coord of a point on the line        */
double   *dirx;         /* <i> The X vector of the dir of the line       */
double   *diry;         /* <i> The Y vector of the dir of the line       */
double   *radius;       /* <o> The radius of the required circle         */
int32_t     *num_results;  /* <o> The number of circles calculated          */
int32_t     *status;       /* <o> The status of the call                    */
                        /*     0  -> Successful call                     */
                        /*     -1 -> Unsuccessful call                   */
{
    double   centre[2];
    double   point[2];
    double   dir[2];                                                   

    centre[X] = *cx;
    centre[Y] = *cy;
    point[X]  = *pointx;
    point[Y]  = *pointy;
    dir[X]    = *dirx;
    dir[Y]    = *diry;

    if (!circ_tanto_lin( centre, point, dir, TOL, radius ))
    {
        *num_results = 0;
        *status = FORTRAN_ERROR;
    }
    else
    {
        *num_results = 1;
        *status = FORTRAN_OKAY;
    }
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 17 Oct 1988
 *
 */
void
cctclp( cx,cy, radius, p_on_lx,p_on_ly, dirx,diry, pntx,pnty,
        res1x,res1y,res1r,res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,
        num_results,status )
                        /* Description   :- This function calculates circles that are
                         *                  tangent to the given line and circle and
                         *                  that pass through the given point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 62 of "Practical Plane geometry" by
                         *                  Harrison and Baxandall for an explanation
                         *                  of the mathematics.
                         *
                         *                  In general there will be four solutions
                         *                  to this problem.
                         */
double  *cx;            /* <i> The X co-ord of the circle centre     */
double  *cy;            /* <i> The Y co-ord of the circle centre     */
double  *radius;        /* <i> The radius of the circle              */
double  *p_on_lx;       /* <i> X-coord of point on given line        */
double  *p_on_ly;       /* <i> Y-coord of point on given line        */
double  *dirx;          /* <i> X Co-ord of the direction of the line */
double  *diry;          /* <i> Y Co-ord of the direction of the line */
double  *pntx;          /* <i> The X Co-ord of the point which the circle must pass */
double  *pnty;          /* <i> The Y Co-ord of the point which the circle must pass */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double  centre[2];
    double  p_on_l[2];
    double  dir[2];
    double  pnt[2];
    double  res[4][3];
                                 
    centre[X] = *cx;
    centre[Y] = *cy;
    p_on_l[X] = *p_on_lx;
    p_on_l[Y] = *p_on_ly;
    dir[X] = *dirx;
    dir[Y] = *diry;
    pnt[X] = *pntx;
    pnt[Y] = *pnty;

    if (!circ_tanto_circ_lin_pnt(centre,*radius,p_on_l,dir,pnt,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else                          
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
    }
}
  
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
cctcp( req_radius, pntx,pnty, circ_centrex,circ_centrey, circ_radius, 
           res1x,res1y,res2x,res2y, num_results, status )
                        /* Description   :- Given a radius, point and circle, this
                         *                  function calculates a circle that passes
                         *                  through the point and is tangent to the 
                         *                  circle.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- There may be one or two solutions depending
                         *                  on the location of the point with respect
                         *                  to the given circle.
                         *
                         *                  See pg 40 of "A programmer's Geometry"
                         *                  by Bowyer and Woodwark for a 
                         *                  description of the mathematics.
                         *
                         */
double *req_radius;      /* <i> The required radius                                 */
double *pntx;            /* <i> The X coord of the point which the circle must pass */
double *pnty;            /* <i> The Y coord of the point which the circle must pass */
double *circ_centrex;    /* <i> The X coord of the centre of the given circle       */
double *circ_centrey;    /* <i> The Y coord of the centre of the given circle       */
double *circ_radius;     /* <i> The radius of the given circle                      */
double *res1x;           /* <o> The X coord of the first circle centre              */
double *res1y;           /* <o> The Y coord of the first circle centre              */
double *res2x;           /* <o> The X coord of the second circle centre             */
double *res2y;           /* <o> The Y coord of the second circle centre             */
int32_t   *num_results;     /* <o> The number of solutions found                       */
int32_t   *status;          /* <o> The status of the call                              */
                         /*     0  -> Successful call                               */
                         /*     -1 -> Unsuccessful call                             */
{      
    double  pnt[2];
    double  circ_centre[2];
    double  res[2][2];

    pnt[X] = *pntx;
    pnt[Y] = *pnty;
    circ_centre[X] = *circ_centrex;
    circ_centre[Y] = *circ_centrey;

    if (!circ_tanto_circ_pnt(*req_radius,pnt,circ_centre,*circ_radius,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res2x = res[1][0];
        *res2y = res[1][1];
    }
}

/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
CCTLC( radius, pntx,pnty, dirx,diry, circ_centrex,circ_centrey,circ_radius, 
         res1x,res1y,res2x,res2y,res3x,res3y,res4x,res4y,res5x,res5y,res6x,res6y,
         res7x,res7y,res8x,res8y, num_results,status )
                        /* Description   :- Given a radius, line and circle this 
                         *                  function calculates a circle tangent to
                         *                  the line and circle.
                         *
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- When the given line intersects the circle:
                         *
                         *                  1/ A positive radius will result in external
                         *                     circles.
                         *                  2/ A negative radius will result in internal
                         *                     circles.
                         *
                         *                  See pg 42 of "A programmer's Geometry"
                         *                  by Bowyer and Woodwark for a 
                         *                  description of the mathematics.
                         */
double  *radius;        /* <i> The radius required                            */
double  *pntx;          /* <i> X coord of the point on the line               */
double  *pnty;          /* <i> Y coord of the point on the line               */
double  *dirx;          /* <i> The X vector of the line direction             */
double  *diry;          /* <i> The Y vector of the line direction             */
double  *circ_centrex;  /* <i> The X Co-ord of the centre of the circle       */
double  *circ_centrey;  /* <i> The Y Co-ord of the centre of the circle       */
double  *circ_radius;   /* <i> The radius of the given circle                 */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res8x;         /* <o> The X Co-ordinate of the eighth circle centre  */
double  *res8y;         /* <o> The Y Co-ordinate of the eighth circle centre  */
int32_t    *num_results;   /* <o> The number of circles found                    */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double  pnt[2];
    double  dir[2];
    double  circ_centre[2];
    double  res[8][2];

    pnt[0] = *pntx;
    pnt[1] = *pnty;
    dir[0] = *dirx;
    dir[1] = *diry;
    circ_centre[0] = *circ_centrex;
    circ_centre[1] = *circ_centrey;
    
    if (!circ_tanto_lin_circ(*radius,pnt,dir,circ_centre,*circ_radius,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res8x = res[7][0];
        *res8y = res[7][1];
    }

}
     
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
cctlp( radius, pntx,pnty, dirx,diry,pointx,pointy,res1x,res1y,res2x,res2y,num_results,
          status )
                        /* Description   :- Given a radius, a point and a line this 
                         *                  function calculates a circle tangent to
                         *                  the line and passing through the point.
                         *
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 36 of Bowyer and Woodwark for an 
                         *                  explanation of the mathematics.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         *
                         */
double  *radius;        /* <i> The radius required                          */
double  *pntx;          /* <i> The X Co-ord of the point on the line        */
double  *pnty;          /* <i> The Y Co-ord of the point on the line        */
double  *dirx;          /* <i> The X vector of the direction of the line    */
double  *diry;          /* <i> The Y vector of the direction of the line    */
double  *pointx;        /* <i> The X Coord of the point which the circle passes */
double  *pointy;        /* <i> The Y Coord of the point which the circle passes */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
int32_t    *num_results;   /* <o> The number of circles found                    */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double   pnt[2];
    double   dir[2];
    double   point[2];
    double   res[2][2];

    pnt[0] = *pntx;
    pnt[1] = *pnty;
    point[0] = *pointx;
    point[1] = *pointy;
    dir[0] = *dirx;
    dir[1] = *diry;
    
    if (!circ_tanto_lin_pnt(*radius,pnt,dir,point,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res2x = res[1][0];
        *res2y = res[1][1];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
cctl2p( p_on_linex,p_on_liney, dir_of_linex,dir_of_liney, pointcx,pointcy, pointdx,
          pointdy, res1x,res1y,res1r,res2x,res2y,res2r,num_results,status)
                        /* Description   :- This function calculates the circle that is
                         *                  tangent to a line and that passes through
                         *                  two points.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 22 of "Practical Geometry" by
                         *                   D Low for a description of the algorithm.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         *
                         */
double  *p_on_linex;    /* <i> X Co-ord of the point on the line */
double  *p_on_liney;    /* <i> Y Co-ord of the point on the line */
double  *dir_of_linex;  /* <i> X Vector of the direction         */
double  *dir_of_liney;  /* <i> Y Vector of the direction         */
double  *pointcx;       /* <i> X Co-ord of the first point       */
double  *pointcy;       /* <i> Y Co-ord of the second point      */
double  *pointdx;       /* <i> X Co-ord of the second point      */
double  *pointdy;       /* <i> Y Co-ord of the second point      */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
int32_t    *num_results;   /* <o> The number of results calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double  p_on_line[2];
    double  dir_of_line[2];
    double  pointc[2];
    double  pointd[2];
    double  res[2][3];

    p_on_line[0] = *p_on_linex;
    p_on_line[1] = *p_on_liney;
    dir_of_line[0] = *dir_of_linex;
    dir_of_line[1] = *dir_of_liney;
    pointc[0] = *pointcx;
    pointc[1] = *pointcy;
    pointd[0] = *pointdx;
    pointd[1] = *pointdy;

    if (!circ_tanto_lin_2pnts(p_on_line,dir_of_line,pointc,pointd,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];         
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
    }
}
    
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
cctc2p( point1x,point1y,point2x,point2y,centrex,centrey,radius, res1x,res1y,res1r,
          res2x,res2y,res2r, num_results,status )
                        /* Description   :- Given two points and a circle this function 
                         *                  calculates a circle that passes through
                         *                  the two points and is tangent to the circle
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 23 of "Practical Geometry" by D.A.Low
                         *                  for a description of the algorithm.
                         *
                         *                  There are two solutions to this problem.
                         *                  The first solution will always produce the
                         *                  circle that is tangent to the furthest side
                         *                  of the given circle.
                         */
double  *point1x;       /* <i> The X Co-ord of the first point                */
double  *point1y;       /* <i> The Y Co-ord of the first point                */
double  *point2x;       /* <i> The X Co-ord of the second point               */
double  *point2y;       /* <i> The Y Co-ord of the secondt point              */
double  *centrex;       /* <i> The X Co-ord of the circle centre              */
double  *centrey;       /* <i> The Y Co-ord of the circle centre              */
double  *radius;        /* <i> The radius of the circle                       */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The Radius of the first circle centre          */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The Radius of the second circle centre         */
int32_t    *num_results;   /* <o> The number of circles found                    */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double  point1[2];
    double  point2[2];
    double  centre[2];
    double  res[2][3];         

    point1[0] = *point1x;
    point1[1] = *point1y;
    point2[0] = *point2x;
    point2[1] = *point2y;
    centre[X] = *centrex;
    centre[Y] = *centrey;

    if (!circ_tanto_circ_2pnts(point1,point2,centre,*radius,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 17 Oct 1988
 *
 */
void
CCT2C( radius, centre1x,centre1y, radius1, centre2x,centre2y, radius2,
         res1x,res1y,res2x,res2y,res3x,res3y,res4x,res4y,res5x,res5y,res6x,res6y,
         res7x,res7y,res8x,res8y,num_results,status)
                        /* Description   :- Given a radius and two circles this function
                         *                  calculates the circles that are tangent to
                         *                  the given circles.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 45 of "A programmer's geometry" by
                         *                   Bowyer and Woodwark for a 
                         *                   description of the mathematics 
                         *
                         *                   Negate the given radii to calculate the 
                         *                   internal tangent solutions.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         */
double  *radius;        /* <i> The radius of the required circles             */
double  *centre1x;      /* <i> The X Co-ord centre point of the first circle  */
double  *centre1y;      /* <i> The Y Co-ord centre point of the first circle  */
double  *radius1;       /* <i> The radius of the first circle                 */
double  *centre2x;      /* <i> The X Co-ord centre point of the second circle */
double  *centre2y;      /* <i> The Y Co-ord centre point of the second circle */
double  *radius2;       /* <i> The radius of the second circle                */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res8x;         /* <o> The X Co-ordinate of the eighth circle centre  */
double  *res8y;         /* <o> The Y Co-ordinate of the eighth circle centre  */
int32_t    *num_results;   /* <o> The number of circles found                    */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{                                                                               
    double  centre1[2];
    double  centre2[2];
    double  res[8][2];

    centre1[0] = *centre1x;
    centre1[1] = *centre1y;
    centre2[0] = *centre2x;
    centre2[1] = *centre2y;

    if (!circ_tanto_2circs(*radius,centre1,*radius1,centre2,*radius2,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res8x = res[7][0];
        *res8y = res[7][1];
    }
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 17 Oct 1988
 *
 */
void
CCT2CL( centre1x,centre1y, radius1, centre2x,centre2y, radius2, p_on_lx,p_on_ly, dirx,
         diry, res1x,res1y,res1r,res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,
         res5x,res5y,res5r,res6x,res6y,res6r,res7x,res7y,res7r,res8x,res8y,res8r,
         num_results,status )
                        /* Description   :- This function calculates circles that are
                         *                  tangent to the given circles and line.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 64 of "Practical Plane geometry" by
                         *                  Harrison and Baxandall for an explanation
                         *                  of the mathematics.
                         *
                         *                  In general there will be eight solutions
                         *                  to this problem.
                         */
double  *centre1x;      /* <i> X Coord of centre of first circle */
double  *centre1y;      /* <i> Y Coord of centre of first centre */
double  *radius1;       /* <i> Radius of the first circle */
double  *centre2x;      /* <i> X Coord of centre of second circle */
double  *centre2y;      /* <i> Y Coord of centre of second circle */
double  *radius2;       /* <i> Radius of the second circle */
double  *p_on_lx;       /* <i> X Coord of point on given line */
double  *p_on_ly;       /* <i> Y Coord of point on given line */
double  *dirx;          /* <i> X Vector of the line direction */
double  *diry;          /* <i> Y Vector of the line direction */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res5r;         /* <o> The radius of the fifth circle                 */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res6r;         /* <o> The radius of the sixth circle                 */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res7r;         /* <o> The radius of the seventh circle               */
double  *res8x;         /* <o> The X Co-ordinate of the eighth circle centre  */
double  *res8y;         /* <o> The Y Co-ordinate of the eighth circle centre  */
double  *res8r;         /* <o> The radius of the eighth circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{
    double  centre1[2];
    double  centre2[2];
    double  p_on_l[2];
    double  dir[2];
    double  res[8][3];

    centre1[0] = *centre1x;
    centre1[1] = *centre1y;
    centre2[0] = *centre2x;
    centre2[1] = *centre2y;
    p_on_l[0]  = *p_on_lx;
    p_on_l[1]  = *p_on_ly;
    dir[0]     = *dirx;
    dir[1]     = *diry;

    if (!circ_tanto_2circs_lin(centre1,*radius1,centre2,*radius2,p_on_l,dir,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res5r = res[4][2];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res6r = res[5][2];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res7r = res[6][2];
        *res8x = res[7][0];
        *res8y = res[7][1];
        *res8r = res[7][2];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 1988
 *
 */
void
CCT2CP(pointx,pointy,centre1x,centre1y,radius1,centre2x,centre2y,radius2,
        res1x,res1y,res1r,res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,
        num_results,status )
                        /* Description   :- This function calculates a circle that is
                         *                  tangent to two circles and passes through 
                         *                  a point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be four solutions
                         *                   to this problem.
                         *                   
                         *                   See pg 26 of "Practical Geometry" by D.A.Low
                         *                   for a description of the mathematics.
                         */
double  *pointx;        /* <i> X Coord of point circle passes through         */
double  *pointy;        /* <i> Y Coord of point circle passes through         */
double  *centre1x;      /* <i> X Coord of centre of first circle              */
double  *centre1y;      /* <i> Y Coord of centre of first circle              */
double  *radius1;       /* <i> The radius of the first circle                 */
double  *centre2x;      /* <i> X Coord of centre of second circle             */
double  *centre2y;      /* <i> Y Coord of centre of second circle             */
double  *radius2;       /* <i> The radius of the second circle                */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{                                                                            
    double  point[2];
    double  centre1[2];
    double  centre2[2];
    double  res[4][3];

    point[0] = *pointx;
    point[1] = *pointy;
    centre1[0] = *centre1x;
    centre1[1] = *centre1y;
    centre2[0] = *centre2x;
    centre2[1] = *centre2y;

    if (!circ_tanto_2circs_pnt(point,centre1,*radius1,centre2,*radius2,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 1988
 *
 */
BOOLEAN
CCT2L(pnt_1x,pnt_1y,dir_1x,dir_1y,pnt_2x,pnt_2y,dir_2x,dir_2y,radius,res1x,res1y,
       res2x,res2y,res3x,res3y,res4x,res4y,num_results,status )
                        /* Description   :- Given a radius and two intersecting lines
                         *                  this function calculates the four circles
                         *                  that are tangent to the two lines.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 38 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for an
                         *                   explantion of the mathematics.
                         *
                         *                   In general there will be four solutions
                         *                   to this problem.
                         *
                         */
double  *pnt_1x;        /* <i> X Coord of point which first line passes through */
double  *pnt_1y;        /* <i> Y Coord of point which first line passes through */
double  *dir_1x;        /* <i> X vector of first line direction */
double  *dir_1y;        /* <i> Y vector of first line direction */
double  *pnt_2x;        /* <i> X Coord of point which second line passes through */
double  *pnt_2y;        /* <i> Y Coord of point which second line passes through */
double  *dir_2x;        /* <i> X vector of second line direction */
double  *dir_2y;        /* <i> Y vector of second line direction */
double  *radius;        /* <i> The radius of the resulting circles          */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{                                                                                  
    double pnt1[2];
    double dir1[2];
    double pnt2[2];
    double dir2[2];
    double res[4][2];

    pnt1[0] = *pnt_1x;
    pnt1[1] = *pnt_1y;
    dir1[0] = *dir_1x;
    dir1[1] = *dir_1y;
    pnt2[0] = *pnt_2x;
    pnt2[1] = *pnt_2y;
    dir2[0] = *dir_2x;
    dir2[1] = *dir_2y;

    if (!circ_tanto_2lin(pnt1,dir1,pnt2,dir2,*radius, TOL, res, num_results))
        *status = FORTRAN_ERROR;    
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res4x = res[3][0];
        *res4y = res[3][1];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 1988
 *
 */                                                                            
void
CCT2LC(p1x,p1y,dir1x,dir1y,p2x,p2y,dir2x,dir2y,centrex,centrey,radius,
         res1x,res1y,res1r,res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,
         res5x,res5y,res5r,res6x,res6y,res6r,res7x,res7y,res7r,res8x,res8y,res8r,
         num_results,status )
                        /* Description   :- This function calculates the circle tangent
                         *                  to the two given lines and circle.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be two solutions
                         *                   to this problem.
                         *
                         *                   See pg 22 of "Practical Geometry" by
                         *                   D.A. Low for a description of the 
                         *                   Mathematics.
                         *
                         */                                   
double  *p1x;           /* <i> X Coord of point on first line                 */
double  *p1y;           /* <i> Y Coord of point on first line                 */
double  *dir1x;         /* <i> X vector of direction of first line            */
double  *dir1y;         /* <i> Y vector of direction of first line            */
double  *p2x;           /* <i> X Coord of point on second line                */
double  *p2y;           /* <i> Y Coord of point on second line                */
double  *dir2x;         /* <i> X vector of direction of second line           */
double  *dir2y;         /* <i> Y vector of direction of second line           */
double  *centrex;       /* <i> X Coord of circle centre                       */
double  *centrey;       /* <i> Y Coord of circle centre                       */
double  *radius;        /* <i> The radius of the circle                       */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res5r;         /* <o> The radius of the fifth circle                 */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res6r;         /* <o> The radius of the sixth circle                 */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res7r;         /* <o> The radius of the seventh circle               */
double  *res8x;         /* <o> The X Co-ordinate of the eighth circle centre  */
double  *res8y;         /* <o> The Y Co-ordinate of the eighth circle centre  */
double  *res8r;         /* <o> The radius of the eighth circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{                                                                           
    double  p1[2];
    double  dir1[2];
    double  p2[2];
    double  dir2[2];
    double  centre[2];
    double  res[8][3];

    p1[0] = *p1x;
    p1[1] = *p1y;
    dir1[0] = *dir1x;
    dir1[1] = *dir1y;
    p2[0] = *p2x;
    p2[1] = *p2y;
    dir2[0] = *dir2x;
    dir2[1] = *dir2y;
    centre[0] = *centrex;
    centre[1] = *centrey;

    if (!circ_tanto_2lin_circ(p1,dir1,p2,dir2,centre,*radius,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res5r = res[4][2];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res6r = res[5][2];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res7r = res[6][2];
        *res8x = res[7][0];
        *res8y = res[7][1];
        *res8r = res[7][2];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 1988
 *
 */
BOOLEAN
CCT2LP(p_on_line1x,p_on_line1y,dir1x,dir1y,p_on_line2x,p_on_line2y,dir2x,dir2y,
         pointx,pointy,res1x,res1y,res1r,res2x,res2y,res2r,num_results,status)
                        /* Description   :- This function calculates the circle tangent
                         *                  to the given lines that passes through the
                         *                  given point.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be two solutions
                         *                   to this problem.
                         *
                         *                   See pg 22 of "Practical Geometry" by
                         *                   D.A. Low for a description of the 
                         *                   Mathematics.
                         */                                                   
double  *p_on_line1x;   /* <i> X Coord of point on first line                 */
double  *p_on_line1y;   /* <i> Y Coord of point on first line                 */
double  *dir1x;         /* <i> X vector of direction of first line            */
double  *dir1y;         /* <i> Y vector of direction of first line            */
double  *p_on_line2x;   /* <i> X Coord of point on second line                */
double  *p_on_line2y;   /* <i> Y Coord of point on second line                */
double  *dir2x;         /* <i> X vector of direction of second line           */
double  *dir2y;         /* <i> Y vector of direction of second line           */
double  *pointx;        /* <i> X Coord of point circle must pass through      */
double  *pointy;        /* <i> Y Coord of point circle must pass through      */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{                                                                           
    double  p_on_line1[2];
    double  dir1[2];
    double  p_on_line2[2];
    double  dir2[2];
    double  point[2];
    double  res[2][3];

    p_on_line1[X] = *p_on_line1x;
    p_on_line1[Y] = *p_on_line1y;
    dir1[X] = *dir1x;
    dir1[Y] = *dir1y;
    p_on_line2[X] = *p_on_line2x;
    p_on_line2[Y] = *p_on_line2y;
    dir2[X] = *dir2x;
    dir2[Y] = *dir2y;
    point[X] = *pointx;
    point[Y] = *pointy;

    if (!circ_tanto_2lin_pnt(p_on_line1,dir1,p_on_line2,dir2,point,TOL,0,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
    }
}

/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 1988
 *
 */                                                                            
void
CCT3C( cen1x,cen1y,rad1,cen2x,cen2y,rad2,cen3x,cen3y,rad3,
         res1x,res1y,res1r,res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,
         res5x,res5y,res5r,res6x,res6y,res6r,res7x,res7y,res7r,res8x,res8y,res8r,
         num_results,status)
                        /* Description   :- This function calculates the circles
                         *                  tangent to three circles
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 27 of "Practical Geometry"
                         *                  by D.A.Low for a description of the
                         *                  Algorithm.
                         *
                         *                  There is a maximum of eight solutions to
                         *                  this problem.
                         *                  
                         */
double  *cen1x;         /* <i> X Coord of centre of first circle              */
double  *cen1y;         /* <i> Y Coord of centre of first centre              */
double  *rad1;          /* <i> Radius of the first circle                     */
double  *cen2x;         /* <i> X Coord of centre of second circle             */
double  *cen2y;         /* <i> Y Coord of centre of second circle             */
double  *rad2;          /* <i> Radius of the second circle                    */
double  *cen3x;         /* <i> X Coord of centre of third circle              */
double  *cen3y;         /* <i> Y Coord of centre of third circle              */
double  *rad3;          /* <i> Radius of the third circle                     */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res5r;         /* <o> The radius of the fifth circle                 */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res6r;         /* <o> The radius of the sixth circle                 */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res7r;         /* <o> The radius of the seventh circle               */
double  *res8x;         /* <o> The X Co-ordinate of the eighth circle centre  */
double  *res8y;         /* <o> The Y Co-ordinate of the eighth circle centre  */
double  *res8r;         /* <o> The radius of the eighth circle                */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{      
    double  cen1[2];
    double  cen2[2];
    double  cen3[2];                                             
    double  res[8][3];
    
    cen1[0] = *cen1x;
    cen1[1] = *cen1y;
    cen2[0] = *cen2x;
    cen2[1] = *cen2y;
    cen3[0] = *cen3x;
    cen3[1] = *cen3y;

    if (!circ_tanto_3circs( cen1,*rad1,cen2,*rad2,cen3,*rad3,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res5r = res[4][2];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res6r = res[5][2];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res7r = res[6][2];
        *res8x = res[7][0];
        *res8y = res[7][1];
        *res8r = res[7][2];
    }
}
   
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 18 Oct 19888
 *
 */
void
CCT3L(p1x,p1y,p2x,p2y,p3x,p3y,d1x,d1y,d2x,d2y,d3x,d3y, res1x,res1y,res1r,
         res2x,res2y,res2r,res3x,res3y,res3r,res4x,res4y,res4r,res5x,res5y,res5r,
         res6x,res6y,res6r,res7x,res7y,res7r,num_results,status)
                        /* Description   :- This function calculates the seven circles
                         *                  that are tangent to three lines.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  When none of the directions are parallel
                         *                   the will be 7 solutions.
                         *                   If two of the directions are parallel
                         *                   there will be 2 solutions.
                         *
                         */                                    
double  *p1x;           /* <i> X Coord of point on first line */
double  *p1y;           /* <i> Y Coord of point on first line */
double  *p2x;           /* <i> X Coord of point on second line */
double  *p2y;           /* <i> Y Coord of point on second line */
double  *p3x;           /* <i> X Coord of point on third line */
double  *p3y;           /* <i> Y Coord of point on third line */
double  *d1x;           /* <i> X Vector of first line direction */
double  *d1y;           /* <i> Y Vector of first line direction */
double  *d2x;           /* <i> X Vector of second line direction */
double  *d2y;           /* <i> Y Vector of second line direction */
double  *d3x;           /* <i> X Vector of third line direction */
double  *d3y;           /* <i> Y Vector of third line direction */
double  *res1x;         /* <o> The X Co-ordinate of the first circle centre   */
double  *res1y;         /* <o> The Y Co-ordinate of the first circle centre   */
double  *res1r;         /* <o> The radius of the first circle                 */
double  *res2x;         /* <o> The X Co-ordinate of the second circle centre  */
double  *res2y;         /* <o> The Y Co-ordinate of the second circle centre  */
double  *res2r;         /* <o> The radius of the second circle                */
double  *res3x;         /* <o> The X Co-ordinate of the third circle centre   */
double  *res3y;         /* <o> The Y Co-ordinate of the third circle centre   */
double  *res3r;         /* <o> The radius of the third circle                 */
double  *res4x;         /* <o> The X Co-ordinate of the fourth circle centre  */
double  *res4y;         /* <o> The Y Co-ordinate of the fourth circle centre  */
double  *res4r;         /* <o> The radius of the fourth circle                */
double  *res5x;         /* <o> The X Co-ordinate of the fifth circle centre   */
double  *res5y;         /* <o> The Y Co-ordinate of the fifth circle centre   */
double  *res5r;         /* <o> The radius of the fifth circle                 */
double  *res6x;         /* <o> The X Co-ordinate of the sixth circle centre   */
double  *res6y;         /* <o> The Y Co-ordinate of the sixth circle centre   */
double  *res6r;         /* <o> The radius of the sixth circle                 */
double  *res7x;         /* <o> The X Co-ordinate of the seventh circle centre */
double  *res7y;         /* <o> The Y Co-ordinate of the seventh circle centre */
double  *res7r;         /* <o> The radius of the seventh circle               */
int32_t    *num_results;   /* <o> The number of circles calculated               */
int32_t    *status;        /* <o> The status of the call                         */
                        /*     0  -> Successful call                          */
                        /*     -1 -> Unsuccessful call                        */
{      
    double points[3][2];
    double dir[3][2];
    double res[8][3];

    points[0][0] = *p1x;
    points[0][1] = *p1y;
    points[1][0] = *p2x;
    points[1][1] = *p2y;
    points[2][0] = *p3x;
    points[2][1] = *p3y;

    dir[0][0] = *d1x;
    dir[0][1] = *d1y;
    dir[1][0] = *d2x;
    dir[1][1] = *d2y;
    dir[2][0] = *d3x;
    dir[2][1] = *d3y;

    if (!circs_tanto_3lin(points,dir,TOL,res,num_results))
        *status = FORTRAN_ERROR;
    else
    {
        *status = FORTRAN_OKAY;
        *res1x = res[0][0];
        *res1y = res[0][1];
        *res1r = res[0][2];
        *res2x = res[1][0];
        *res2y = res[1][1];
        *res2r = res[1][2];
        *res3x = res[2][0];
        *res3y = res[2][1];
        *res3r = res[2][2];
        *res4x = res[3][0];
        *res4y = res[3][1];
        *res4r = res[3][2];
        *res5x = res[4][0];
        *res5y = res[4][1];
        *res5r = res[4][2];
        *res6x = res[5][0];
        *res6y = res[5][1];
        *res6r = res[5][2];
        *res7x = res[6][0];
        *res7y = res[6][1];
        *res7r = res[6][2];
    }
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 21st-September-1988
 *
 */
BOOLEAN
centres_of_similitude( centre1,radius1, centre2, radius2, tol, s, num_res )
                        /* Description   :- This function calculates the two centres
                         *                  of similitude of a pair of circles.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The centres are output as
                         *                  the internal followed by the external,
                         *                  if the external exists.
                         *
                         *                  This routine is used internally and is not
                         *                  of use to the user.
                         *
                         *                  See pg 25 of "Practical Geometry" by D.A. Low
                         *                  for a description of the mathematics.
                         *
                         *                  If both circles have the same radius
                         *                  there will be one (internal) centre of
                         *                  similitude.
                         */
double  centre1[2];     /* <i> The centre of the first circle  */
double  radius1;        /* <i> The radius of the first circle  */
double  centre2[2];     /* <i> The centre of the second circle */
double  radius2;        /* <i> The radius of the second circle */
double  tol;            /* <i> The accuracy tolerance          */
double  s[2][2];        /* <o> The centres of similitude       */
INT     *num_res;       /* <o> The number of centres found     */
{
      double    vec1[3];      /* Define a line through the two centre */
      double    vec2[3];      /* Points. Define points on both circles that */
      double    z[3];         /* are perpendicular to this line. Draw a line */
      double    perp[3];      /* through the two points on the circumferences. */
      double    pnt1[2];      /* The intersection of the two lines defines one */
      double    pnt2[2];      /* of the centres of similitude. Reverse the direction */
      double    loc_s[2];     /* of one of the circumference vectors and draw a line */
      double    sp;           /* through the circumference points. The intersection */
      double    tp;           /* of this will the original line will produce the */
                              /* other centre of similitude */
      *num_res = 0;
      if( radius1 < tol || radius2 < tol )
      {                                 /* Positive radii only */
            return(FALSE);
      }

      vecdif(centre2,centre1,2,vec1);
      vec1[Z] = 0.0;
      vecw(z);
      veccp(z,vec1,perp);               /* Calculate a vector perpendicular to vec1 */
      vecnrm(perp,2,tol,perp);          /* Normalise it */
      vecsca(radius1,perp,2,pnt1);
      vecsum(centre1,pnt1,2,pnt1);
      vecneg(perp,2,perp);
      vecsca(radius2,perp,2,pnt2);
      vecsum(centre2,pnt2,2,pnt2);
      vecdif(pnt2,pnt1,2,vec2);
      if(line_x_line2D( centre1, vec1, pnt1, vec2, tol, loc_s, &sp, &tp))
      {  
           s[(*num_res)][X] = loc_s[X];
           s[(*num_res)][Y] = loc_s[Y];
           (*num_res)++;
      }
      vecneg(perp,2,perp);
      vecsca(radius2,perp,2,pnt2);
      vecsum(centre2,pnt2,2,pnt2);
      vecdif(pnt2,pnt1,2,vec2);
      if(line_x_line2D( centre1, vec1, pnt1, vec2, tol, loc_s, &sp, &tp))
      {  
           s[(*num_res)][X] = loc_s[X];
           s[(*num_res)][Y] = loc_s[Y];
           (*num_res)++;
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 5th-October-1988
 *
 */
BOOLEAN
circ_diameric(point1, point2, tol, centre, radius )
                        /* Description   :- This function defines a circle by two
                         *                  diametric points.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  point1[2];      /* <i> First point            */
double  point2[2];      /* <i> Second point           */
double  tol;            /* <i> The accuracy tolerance */
double  centre[2];      /* <o> The resultant centre   */
double  *radius;        /* <o> The resultant radius   */
{
    double  vec[2];
    double  len;

    vecdif(point1, point2, 2, vec );
    if((len = veclen(vec,2))<tol)
    {                                /* Points are too close to each other */
        return(FALSE);
    }
    *radius = len/2.0;

    vecnrm(vec,2,tol,vec);           /* Bisect the vector between the two points */
    vecsca(len/2.0,vec,2,vec);       /* to calculate the circle centre */
    vecsum(point2,vec,2,centre);

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 27th-September-1988
 *
 */
BOOLEAN
circ_internal( centre1, radius1, centre2, radius2, tol  )
                        /* Description   :- This function calculates whether or not
                         *                  circle2 is internal to circle1.
                         *
                         * Return status :- TRUE if circle2 is internal to circle1
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- It is assumed that: 
                         *                       1. The circles are tangent to each
                         *                          other.
                         *                       2. The radii are positive.
                         *
                         *                  This routine is used internally and is not
                         *                  of use to the user.
                         */
double centre1[2];      /* <i> The centre of the first circle  */
double radius1;         /* <i> The radius of the first circle  */
double centre2[2];      /* <i> The centre of the second circle */
double radius2;         /* <i> The radius of the second circle */
double  tol;            /* <i> The accuracy tolerance          */
{
    double  vec1[2];
    double  len;

    vecdif(centre2,centre1,2,vec1);
    len = veclen(vec1, 2);
    if(((radius1 + radius2) - len)< tol )       /* Assuming tangency */
    {                                           /* Circles are external to each other */
        return(FALSE);
    }
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 21st-September-1988
 *
 */
BOOLEAN
circ_tans_to_pnt( point, centre, radius, tol, lin_res, tan_pnts, num_results )
                        /* Description   :- This function calculates the lines tangent
                         *                  to a given circle that pass through a point
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- If the point lies inside the circle there
                         *                  are no tangents.
                         *                  If the point lies on the circle there is
                         *                  one tangent.
                         *                  If the point lies outside the circle there
                         *                  are two tangents.
                         *
                         *                  See pg 29 of "A programmer's Geometry"
                         *                  by Bowyer and Woodwark for a 
                         *                  description of the mathematics.
                         */
double  point[2];       /* <i> The point through which the tangents must pass */
double  centre[2];      /* <i> The centre of the circle                       */
double  radius;         /* <i> The radius of the circle                       */
double  tol;            /* <i> The accuracy tolerance                         */
double  lin_res[2][2];  /* <o> The direction vectors of the tangents          */
double  tan_pnts[2][2]; /* <o> The tangent points on the circle               */
INT     *num_results;   /* <o> The number of tangents found                   */
{
    double  xkj;
    double  ykj;
    double  xkjsq;
    double  ykjsq;
    double  denom;
    double  root;
    double  deninv;
    double  implicit[3];
    double  p_on_line[2];
    double  direction[2];
    double  rksign;
    INT     i;

    *num_results = 0;
    if( radius < tol )
    {                                /* Positive radius only */
        return(FALSE);
    }

    xkj = centre[X] - point[X];
    ykj = centre[Y] - point[Y];
    
    xkjsq = xkj*xkj;
    ykjsq = ykj*ykj;
    denom = xkjsq + ykjsq;
    if( denom < tol )
    {                                /* The point and the centre are coincident */
        return(FALSE);
    }
    root = denom - radius*radius;
    if( root < -tol )
    {                                /* Point lies within the circle */
        return(TRUE);
    }
    deninv = 1.0/denom;
    if( root < tol )
    {                                /* Point lies on the circle */
        implicit[0] = -radius*xkj*deninv;
        implicit[1] = -radius*ykj*deninv;
        implicit[2] = -(implicit[0]*point[X] + implicit[1]*point[Y]);
        implicit_to_para(implicit,p_on_line,direction );
        lin_res[0][X] = direction[X];
        lin_res[0][Y] = direction[Y];
        tan_pnts[0][X] = centre[X] + implicit[0]*radius;
        tan_pnts[0][Y] = centre[Y] + implicit[1]*radius;
        *num_results = 1;
        return(TRUE);
    }
    else
    {                                /* There are two solutions */
        root = sqrt(root);
        for(i=0; i <2 ; i++)
        {
            if(i==0)
            {
                rksign = radius;
            }
            else
            {
                rksign = -radius;
            }
            implicit[0] = (-ykj*root - rksign*xkj)*deninv;
            implicit[1] = (xkj*root - rksign*ykj)*deninv;
            implicit[2] = -(implicit[0]*point[X] + implicit[1]*point[Y]);

            implicit_to_para(implicit,p_on_line,direction );
            lin_res[i][X] = direction[X];
            lin_res[i][Y] = direction[Y];
            tan_pnts[i][X] = centre[X] + implicit[0]*rksign;
            tan_pnts[i][Y] = centre[Y] + implicit[1]*rksign;
        }
        *num_results = 2;
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 5th-October-1988
 *
 */
BOOLEAN
circ_tanto_circ( centre, g_centre, g_radius, radius1, radius2, num )
                        /* Description   :- Given a centre point and circle this
                         *                  function calculates the radii that would 
                         *                  make the circle tangent to the given circle.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double centre[2];       /* <i> The centre point of the required circle */
double g_centre[2];     /* <i> The centre of the given circle          */
double g_radius;        /* <i> The radius of the given circle          */
double *radius1;        /* <o> The inner radius                        */
double *radius2;        /* <o> The outer radius                        */
int32_t   *num;            /* <o> The number of results                   */
{
    double  vec[2];
    double  len;
            
    *num = 0;                               /* Show no results */

    if( g_radius < 0.0 )
    {                                        /* Positive radius only */
        return(FALSE);
    }

    vecdif( centre, g_centre, 2, vec );
    if((len = veclen( vec, 2 ))< g_radius)   /* If point lies inside the circle */
    {                                        /* two solutions are possible */
        *radius1 = g_radius - len;      
        *radius2 = g_radius + len;                                
        *num = 2;
    }
    else
    {
        *radius1 = len - g_radius;              
        *radius2 = len + g_radius;
        *num = 2;
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 5th-October-1988
 *
 */
BOOLEAN
circ_tanto_lin( centre, p_on_line, dir, tol, radius  )
                        /* Description   :- Given a centre point and line equation this
                         *                  function calculates the radius that would 
                         *                  make the circle tangent to the line.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double centre[2];       /* <i> The centre point of the required circle */
double p_on_line[2];    /* <i> A point on the line                     */
double dir[2];          /* <i> The direction of the line               */
double tol;             /* <i> The accuracy tolerance                  */
double *radius;         /* <o> The resultant radius                    */
{
    double  loc_centre[3];
    double  loc_pl[3];
    double  loc_dir[3];

    if( veclen(dir,2) < tol )
    {                                       /* Line has no direction */
        return(FALSE);
    }

    veccpy(centre,2,loc_centre);            /* Covert to 3D variables */
    loc_centre[Z] = 0.0;
    veccpy(p_on_line,2,loc_pl);
    loc_pl[Z] = 0.0;
    veccpy(dir,2,loc_dir);
    loc_dir[Z] = 0.0;

    if(!axperpl( loc_centre, loc_pl, loc_dir, radius ))
    {
        return(FALSE);
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 7th-October-1988
 *
 */
BOOLEAN
circ_tanto_circ_lin_pnt( centre, radius, p_on_l, dir, point, tol, results, num_results )
                        /* Description   :- This function calculates circles that are
                         *                  tangent to the given line and circle and
                         *                  that pass through the given point.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 62 of "Practical Plane geometry" by
                         *                  Harrison and Baxandall for an explanation
                         *                  of the mathematics.
                         *
                         *                  In general there will be four solutions
                         *                  to this problem.
                         */
double centre[2];       /* <i> The circle centre                             */
double radius;          /* <i> The radius of the circle                      */
double p_on_l[2];       /* <i> A point on the given line                     */
double dir[2];          /* <i> The direction of the given line               */
double point[2];        /* <i> The point through which the circles must pass */
double tol;             /* <i> The accuracy tolerance                        */
double results[4][3];   /* <o> The resultant circles and their radii         */
INT    *num_results;    /* <o> The number of circles calculated              */
{
    double  loc_centre[3];
    double  loc_pl[3];
    double  loc_dir[3];
    double  perp[3];
    double  c[2];
    double  c1[2];
    double  d[2];
    double  e[3];
    double  f[2];
    double  loc1_res[2][2];
    double  loc2_res[2][2]; 
    double  loc3_res[2][3];
    double  dir1[2];
    double  vec1[2];
    double  r1;
    INT     num_r;
    INT     num1_r;
    INT     i;
    INT     j;


    *num_results = 0;
    if( radius < tol )
    {                                   /* Positive radius only */
        return(FALSE);
    }
    if( veclen(dir,2) < tol )
    {                                   /* Line has no direction */
        return(FALSE);
    }

    veccpy(centre,2,loc_centre);
    loc_centre[Z] = 0.0;
    veccpy(p_on_l,2,loc_pl);
    loc_pl[Z] = 0.0;
    veccpy(dir,2,loc_dir);
    loc_dir[Z] = 0.0;

    if(!axperp(loc_centre,loc_pl,loc_dir,perp,e))     
    {   
        return(FALSE);  
    }
    if(!circ_x_lin( centre, radius, centre, perp, tol, loc1_res, &num_r ))
    {
        return(FALSE);
    }
    if( num_r != 2 )
    {
        return(FALSE);
    }
    for(i=0;i<num_r;i++)                /* For each of the above solutions */
    {
        if( i == 0 )                    /* Reversing c and d will give the other */
        {                               /* pair of circles */
            c[X] = loc1_res[0][X];
            c[Y] = loc1_res[0][Y];
            d[X] = loc1_res[1][X];
            d[Y] = loc1_res[1][Y];
        }
        else
        {
            c[X] = loc1_res[1][X];
            c[Y] = loc1_res[1][Y];
            d[X] = loc1_res[0][X];
            d[Y] = loc1_res[0][Y];
        }
        vecdif( c, point, 2, dir1 );

        if(!circumcentre( point, d, e, tol, c1, &r1))
        {
            return(FALSE);
        }
        if(!circ_x_lin(c1,r1,point,dir1,tol,loc2_res, &num_r ))
        {
           return(FALSE);
        }
        if(num_r!=2)
        {
            return(FALSE);
        }
        f[X] = loc2_res[0][X];
        f[Y] = loc2_res[0][Y]; 
        vecdif(f,point,2,vec1);         /* Select intersection that is not the point */
        if( veclen(vec1,2) < tol )      /* itself */

        {
            f[X] = loc2_res[1][X];
            f[Y] = loc2_res[1][Y]; 
        }
        if(circ_tanto_lin_2pnts( p_on_l, dir, point, f, tol, loc3_res, &num1_r))
        {
            for( j=0; j < num1_r; j++ )
            {
                results[(*num_results)][X] = loc3_res[j][X];
                results[(*num_results)][Y] = loc3_res[j][Y];
                results[(*num_results)][Z] = loc3_res[j][Z];
                (*num_results)++;
            }
        }
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 20th-September-1988
 *
 */
BOOLEAN
circ_tanto_circ_pnt( req_radius, point, circ_centre, circ_radius, tol, results, num_results )
                        /* Description   :- Given a radius, point and circle, this
                         *                  function calculates a circle that passes
                         *                  through the point and is tangent to the 
                         *                  circle.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- There may be one or two solutions depending
                         *                  on the location of the point with respect
                         *                  to the given circle.
                         *
                         *                  See pg 40 of "A programmer's Geometry"
                         *                  by Bowyer and Woodwark for a 
                         *                  description of the mathematics.
                         *
                         */
double req_radius;      /* <i> The required radius                          */
double point[2];        /* <i> The point through which the circle must pass */
double circ_centre[2];  /* <i> The centre of the given circle               */
double circ_radius;     /* <i> The radius of the given circle               */
double tol;             /* <i> The accuracy tolerance                       */
double results[2][2];   /* <o> The centres of the resultant circles         */
INT    *num_results;    /* <o> The number of solutions found                */
{
    double  xkj;
    double  ykj;
    double  sqsum;
    double  sqinv;
    double  radsum;
    double  subexp;
    double  root;
    double  xconst;
    double  yconst;
    double  xvar;
    double  yvar;

    *num_results = 0;  
    if( req_radius < tol || circ_radius < tol )
    {                                       /* Positive radii only */
        return(FALSE);
    }


    xkj = circ_centre[X] - point[X];        /* Re-locate the origin */
    ykj = circ_centre[Y] - point[Y];

    sqsum = xkj*xkj + ykj*ykj;
    if( sqsum < tol )
    {                                       /* The point and the circle centre are */
        return(FALSE);                      /* coincident */

    }
    sqinv = 0.5/sqsum;
    radsum = (req_radius + req_radius + circ_radius) *circ_radius;
    subexp = sqsum - radsum;
    root = 4.0 *req_radius*req_radius*sqsum - subexp*subexp;
    subexp = subexp*sqinv;
    if(root < -tol)
    {                                       /* No centres possible */
        return(FALSE);
    }
    if(root < tol)
    {                                       /* Only one circle is possible */
        results[0][X] = point[X] + xkj*subexp;
        results[0][Y] = point[Y] + ykj*subexp;
        *num_results = 1;
    }
    else
    {                                       /* Two circles are possible */
        root = sqrt(root) *sqinv;
        xconst = point[X] + xkj*subexp;
        yconst = point[Y] + ykj*subexp;
        xvar = ykj*root;
        yvar = xkj*root;
        results[0][X] = xconst - xvar;
        results[0][Y] = yconst + yvar;
        results[1][X] = xconst + xvar;
        results[1][Y] = yconst - yvar;
        *num_results = 2;
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 20th-September-1988
 *
 */
BOOLEAN
circ_tanto_lin_circ( radius, pnt, dir, circ_centre, circ_radius, tol ,results, num_results )
                        /* Description   :- Given a radius, line and circle this 
                         *                  function calculates a circle tangent to
                         *                  the line and circle.
                         *
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- When the given line intersects the circle:
                         *
                         *                  1/ A positive radius will result in external
                         *                     circles.
                         *                  2/ A negative radius will result in internal
                         *                     circles.
                         *
                         *                  See pg 42 of "A programmer's Geometry"
                         *                  by Bowyer and Woodwark for a 
                         *                  description of the mathematics.
                         */
double  radius;         /* <i> The radius required            */
double  pnt[2];         /* <i> A point on the line            */
double  dir[2];         /* <i> The direction of the line      */
double  circ_centre[2]; /* <i> The centre of the given circle */
double  circ_radius;    /* <i> The radius of the given circle */
double  tol;            /* <i> The accuracy tolerance         */
double  results[8][2];  /* <o> The resultant circle centres   */
INT     *num_results;   /* <o> The number of circles found    */
{
      double    implicit[3];
      double    cdash;        
      double    orig_cdash;
      double    atemp;
      double    btemp;
      double    cfac;
      double    rfac;
      double    root;
      double    xconst;
      double    yconst;
      double    xvar;
      double    yvar;
      INT       i;
                    
      if( veclen(dir,2) < tol )
      {                             /* Line has no direction */
          return(FALSE);
      }

      vecnrm(dir,2,tol,dir);        /* Normalise the line definition */
      para_to_implicit(pnt,dir, implicit  );
      cdash = implicit[2] + implicit[0]*circ_centre[X] + implicit[1]*circ_centre[Y];

      orig_cdash = cdash;           /* Keep a copy so that it works */

      for( i=0, *num_results=0; i < 4; i++)
      {           
          cdash = orig_cdash;
          if ((((i == 0) || (i == 2)) && ( cdash < 0.0 )) 
                || (((i == 1) || (i == 3)) && (cdash >= 0.0)))
          {
             atemp = -implicit[0];
             btemp = -implicit[1];
             cdash = -cdash;
          }
          else
          {
            atemp = implicit[0];
            btemp = implicit[1];
          }
          cfac = cdash + radius;
          if (i < 2)
             rfac = radius - circ_radius;  /* Calculate the internal tangents */
          else
             rfac = radius + circ_radius;  /* Calculate the external tangents */


          root = rfac*rfac - cfac*cfac;
          /* If root < -tol there are no solutions in this region */
          if(root >= -tol)
          {
             if(root < tol )
             {                         /* Only one solution possible */
                results[*num_results][X] = circ_centre[X] + atemp*cfac;
                results[*num_results][Y] = circ_centre[Y] + btemp*cfac;
                (*num_results)++;
             }
             else
             {                         /* Two solutions possible */
                root = sqrt(root);
                xconst = circ_centre[X] - atemp*cfac;
                yconst = circ_centre[Y] - btemp*cfac;
                xvar = btemp*root;
                yvar = atemp*root;
                results[*num_results][X] = xconst - xvar;
                results[*num_results][Y] = yconst + yvar;
                (*num_results)++;
                results[*num_results][X] = xconst + xvar;
                results[*num_results][Y] = yconst - yvar;
                (*num_results)++;
             }
          }  
      }
      if (*num_results == 0)
         return(FALSE);
      else 
         return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 21st-September-1988
 *
 */
BOOLEAN
circ_tanto_lin_pnt( radius, pnt, dir, point, tol ,results, num_results )
                        /* Description   :- Given a radius, a point and a line this 
                         *                  function calculates a circle tangent to
                         *                  the line and passing through the point.
                         *
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 36 of Bowyer and Woodwark for an 
                         *                  explanation of the mathematics.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         *
                         */
double  radius;         /* <i> The radius required                          */
double  pnt[2];         /* <i> A point on the line                          */
double  dir[2];         /* <i> The direction of the line                    */
double  point[2];       /* <i> The point through which the circle must pass */
double  tol;            /* <i> The accuracy tolerance                       */
double  results[2][2];  /* <o> The resultant circle centres                 */
INT     *num_results;   /* <o> The number of circles found                  */
{
      double    implicit[3];
      double    cdash;
      double    atemp;
      double    btemp;
      double    cfac;
      double    root;
      double    xconst;
      double    yconst;
      double    xvar;
      double    yvar;
      INT       i;

      if( veclen(dir,2) < tol )
      {                             /* Line has no direction */
          return(FALSE);
      }
      vecnrm(dir,2,tol,dir);        /* Normalise the line definition */
      para_to_implicit(pnt,dir, implicit  );
      cdash = implicit[2] + implicit[0]*point[X] + implicit[1]*point[Y];

      for( i=0, *num_results=0; i < 2; i++)
      {
          if( i == 1 )
          {                         /* Calculate circles on other side of line */
                cdash = -cdash;
          }
          if( cdash < 0.0 )
          {
             atemp = -implicit[0];
             btemp = -implicit[1];
             cdash = -cdash;
          }
          else
          {
            atemp = implicit[0];
            btemp = implicit[1];
          }
          cfac = cdash + radius;
          root = radius*radius - cfac*cfac;
          if(root < -tol)
          {                         /* No solutions in this region */
                return(FALSE);
          }
          if(root < tol )
          {                         /* Only one solution possible */
            results[*num_results][X] = point[X] - atemp*cfac;
            results[*num_results][Y] = point[Y] - btemp*cfac;
            (*num_results)++;
          }
          else
          {                         /* Two solutions possible */
            root = sqrt(root);
            xconst = point[X] - atemp*cfac;
            yconst = point[Y] - btemp*cfac;
            xvar = btemp*root;
            yvar = atemp*root;
            results[*num_results][X] = xconst + xvar;
            results[*num_results][Y] = yconst - yvar;
            (*num_results)++;
            results[*num_results][X] = xconst - xvar;
            results[*num_results][X] = yconst + yvar;
            (*num_results)++;
          }
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 3rd-September-1988
 *
 */
BOOLEAN
circ_tanto_lin_2pnts( p_on_line, dir_of_line, pointc, pointd, tol, results, num_results)
                        /* Description   :- This function calculates the circle that is
                         *                  tangent to a line and that passes through
                         *                  two points.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 22 of "Practical Geometry" by
                         *                   D Low for a description of the algorithm.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         *
                         */
double  p_on_line[2];   /* <i> A point on the line               */
double  dir_of_line[2]; /* <i> The direction of the line         */
double  pointc[2];      /* <i> The first point                   */
double  pointd[2];      /* <i> The second point                  */
double  tol;            /* <i> The accuracy tolerance            */
double  results[2][3];  /* <o> The resultant circles and centres */
INT     *num_results;   /* <o> The number of results calculated  */
{
    double  loc_res[2][2];
    double  e[2];
    double  f[2];
    double  h[2];
    double  k1[2];
    double  k2[2];
    double  o[2];
    double  dir1[3];
    double  dir2[3];
    double  dir3[2];
    double  dir4[2];
    double  dir5[2];
    double  dir6[2];
    double  mid_pnt1[2];
    double  mid_pnt2[2];
    double  centre[2];
    double  perp1[3];
    double  perp2[3];
    double  z[3];
    double  s;
    double  t;
    double  radius;
    double  diam;
    double  perp_len;
    double  len;
    INT     num_r;

    (*num_results) = 0;
    if( veclen(dir_of_line,2) < tol )
    {                                    /* Line has no direction */
        return(FALSE);
    }

    veccpy(dir_of_line,2,dir1); /* Copy to local vector to allow manipulation */
    dir1[Z]=0.0;

    vecdif( pointd, pointc, 2, dir2 );
    dir2[Z] = 0.0;
    len = veclen( dir2, 2 );
    vecnrm( dir2, 2, tol, dir2 );
    vecsca( len/2.0, dir2, 2, dir2 );
    vecsum( pointc, dir2, 2, mid_pnt1 ); /* Mid point between pointc and pointd */

    vecw(z);                             /* Define the unit vector in Z */
    veccp( z, dir1, perp1 );             /* Perpendicular to given line */
    veccp( z, dir2, perp2 );             /* Perpendicular to line through two points */

    if(!line_x_line2D( p_on_line, dir_of_line, pointc, dir2, tol, e, &s, &t))
    {                                    /* Two lines are parallel */
        if(!line_x_line2D( mid_pnt1, perp2, p_on_line, dir_of_line, tol, e, &s, &t))
        {                                /* Impossible condition */
            return(FALSE);
        }
        if(!circumcentre(pointc,pointd,e,tol,centre,&radius))
        {
            return(FALSE);
        }
        results[0][X] = centre[X];       /* Only one solution is possible */
        results[0][Y] = centre[Y];
        results[0][Z] = radius;
        (*num_results)++;
        return(TRUE);
    }
    if( t > 0.0 && t < 1.0 )
    {                                    /* Line bisects vector from pointc to pointd */
        return(FALSE);                   /* No solution is possible */
    }
    if( t >= 1.0 )                       /* Intersection is on or forward of pointd */
    {
        vecdif( e, pointd, 2, dir3 );
        vecsum( e, dir3, 2, f );
        vecdif( f, pointc, 2, dir4 );
        diam = veclen( dir4, 2 );
        vecnrm( dir4, 2, tol, dir4);
        vecsca( diam/2.0, dir4, 2, dir4 );
        vecsum( pointc, dir4, 2, mid_pnt2 );     /* Centre of derived circle */
    }
    else if( t <= 0.0 )
    {                                    /* Intersection is on or behind pointc */
        vecdif( e, pointc, 2, dir3 );
        vecsum( e, dir3, 2, f );
        vecdif( f, pointd, 2, dir4 );
        diam = veclen( dir4, 2 );
        vecnrm( dir4, 2, tol, dir4);
        vecsca( diam/2.0, dir4, 2, dir4 );
        vecsum( pointd, dir4, 2, mid_pnt2 );
    }
    if(!circ_x_lin(mid_pnt2,diam/2.0,e,perp2,tol,loc_res, &num_r ))
    {
        return(FALSE);
    }
    if( num_r < 1 )
    {
        return(FALSE);
    }
    h[X] = loc_res[0][X];                /* Accept first result */
    h[Y] = loc_res[0][Y];
    vecdif( h, e, 2, dir5 );
    perp_len = veclen( dir5, 2 );

    vecnrm( dir1, 2, tol, dir1);
    vecsca( perp_len, dir1, 2, dir1 );
    vecsum( e, dir1, 2, k1 );
    vecneg( dir1, 2, dir1);
    vecsum( e,dir1,2, k2 );
    if(line_x_line2D( k1, perp1, mid_pnt1, perp2, tol, o, &s, &t))
    {  
        results[(*num_results)][X] = o[X];
        results[(*num_results)][Y] = o[Y];
        vecdif( o, pointd, 2, dir6);
        results[*num_results][Z] = veclen(dir6, 2 );
        (*num_results)++;
    }
    if(line_x_line2D( k2, perp1, mid_pnt1, perp2, tol, o, &s, &t))
    {  
        results[(*num_results)][X] = o[X];
        results[(*num_results)][Y] = o[Y];
        vecdif( o, pointd, 2, dir6);
        results[*num_results][Z] = veclen(dir6, 2 );
        (*num_results)++;
    }
    return( TRUE );
}     
/*
 *===================================================================================*
 *
 *                                                        Author :: J.L.Buchanan
 *                                                        Date   :: 30 Nov 1988
 *
 */
BOOLEAN
circ_tanto_circ_2pnts( point_p, point_q, centre_g,radius, tol, results, num_results )
                        /* Description   :- Given two points and a circle this function 
                         *                  calculates a circle that passes through
                         *                  the two points and is tangent to the circle
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 62 sol 64 of 
                         *                  "Practical Plane Geometry" by 
                         *                  HArrison and Baxandall
                         *                  for a description of the algorithm.
                         *
                         *                  There are two solutions to this problem.
                         */
double  point_p[2];     /* <i> The first point                                */
double  point_q[2];     /* <i> The second point                               */
double  centre_g[2];    /* <i> The centre of the circle                       */
double  radius;         /* <i> The radius of the circle                       */
double  tol;            /* <i> The accuracy tolerance                         */
double  results[2][3];  /* <o> The centres and radii of the resultant circles */
INT     *num_results;   /* <o> The number of circles found                    */
{
    POINT     pq;     
    POINT_2D  point_n;
    POINT_2D  centre1;
    POINT_2D  F;
    POINT_2D  E;
    POINT_2D  O;
    POINT_2D  O1;  
    POINT_2D  R;

    VECTOR    vector_nk;               
    VECTOR    dir;  
    VECTOR    FE;

    double    radius1;                                
    double    tan_dir[2][2];
    double    tan_pnts[2][2]; 
    double    s;
    double    t;                             
    
    INT       num;
                                                                         
    *num_results = 0;                   /* Show no solutions */

    vecdif(point_p,point_q,2,pq);       /* Get vector between the two points */
    vecsca(0.5,pq,2,pq);                /* Get halfway along the vector */
    vecsum(point_q,pq,2,point_n);       /* and generate N halfway along vector */
    
    pq[Z] = 0.0;                        /* Force onto xy plane */
    perp_vector(pq,vector_nk);          /* Generate the perpindicular vector to pq */

    /* Now generate a circle that passes through point Q and intersect the */
    /* circle of centre G */
    circumcentre(centre_g,point_p,point_q,tol,centre1,&radius1);

    /* Intersect the two circles and call the intersection points F and E */
    if (!circle_x_circle2D(centre_g,radius,centre1,radius1,tol,F,E,&num))
        return(FALSE);

    /* Now intersect FE and PQ and call it R */
    vecdif(F,E,2,FE);                   /* Get direction of line fe */
    if (!line_x_line2D(point_q,pq,F,FE,tol,R,&s,&t))
        return(FALSE);

    /* Now generate the two points on the circle g where a line through R is */
    /* Tangential to circle g */
    circ_tans_to_pnt( R, centre_g, radius, tol, tan_dir, tan_pnts, &num);
    if (num == 0)
        return(FALSE);    

    /* Take the line from one of the tangent points and produce it through the */
    /* centre of circle g and intersect it with the line NK the resultant point */
    /* of intersection we will call O and OQ is the required radius */
    vecdif(tan_pnts[0],centre_g,2,dir);
    if (!line_x_line2D(tan_pnts[0],dir,point_n,vector_nk,tol,O,&s,&t))
        return(FALSE);                                        
                                  
    vecdif(O,point_q,2,dir);
    results[0][X] = O[X];
    results[0][Y] = O[Y];
    results[0][Z] = veclen(dir,2);
    (*num_results)++;

    /* Do the same for the other tangent point if there are two of them  and */
    /* Call it O1 */
    if (num == 2)
    {
       vecdif(tan_pnts[1],centre_g,2,dir);
       if (!line_x_line2D(tan_pnts[1],dir,point_n,vector_nk,tol,O1,&s,&t))
           return(FALSE);                                        

       vecdif(O1,point_q,2,dir);
       results[1][X] = O1[X];
       results[1][Y] = O1[Y];
       results[1][Z] = veclen(dir,2);
       (*num_results)++;
    }
             
    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 21st-September-1988
 *
 */
BOOLEAN
circ_tanto_2circs( radius, centre1, radius1, centre2, radius2, tol, results, num_results )
                        /* Description   :- Given a radius and two circles this function
                         *                  calculates the circles that are tangent to
                         *                  the given circles.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 45 of "A programmer's geometry" by
                         *                   Bowyer and Woodwark for a 
                         *                   description of the mathematics 
                         *
                         *                   Negate the given radii to calculate the 
                         *                   internal tangent solutions.
                         *
                         *                   In general there will be two solutions
                         *                   to this problem.
                         */
double  radius;         /* <i> The radius of the required circles    */
double  centre1[2];     /* <i> The centre point of the first circle  */
double  radius1;        /* <i> The radius of the first circle        */
double  centre2[2];     /* <i> The centre point of the second circle */
double  radius2;        /* <i> The radius of the second circle       */
double  tol;            /* <i> The accuracy tolerance                */
double  results[8][2];  /* <o> The resultant circle centres          */
INT     *num_results;   /* <o> The number of circles found           */
{
    double    r_dash;
    double    r2_dash;            
    double    r1;
    double    r2;
    double    p1[2];
    double    p2[2];
    INT       num;

    *num_results = 0;
    /* Calculate the extra circles subtract and add the circles own radius to the */
    /* Required radius and the if the circles intersect use there intersect points */
    /* Add both */
    r1 = radius + radius1;
    r2 = radius + radius2;
    if ((circle_x_circle2D(centre1,r1,centre2,r2,tol,results[*num_results],
                   results[(*num_results)+1],&num))
         && ( num == 2))                                 
    {                                                                     
        (*num_results) += 2;
    } 
    /* Subtract both */
    r1 = radius - radius1;
    r2 = radius - radius2;
    if ((circle_x_circle2D(centre1,r1,centre2,r2,tol,results[*num_results],
                   results[(*num_results)+1],&num))
         && ( num == 2))                                 
    {                                                                     
        (*num_results) += 2;
    }                  
    /* Subtract r1 add r2 */
    r1 = radius - radius1;
    r2 = radius + radius2;
    if ((circle_x_circle2D(centre1,r1,centre2,r2,tol,results[*num_results],
                   results[(*num_results)+1],&num))
         && ( num == 2))                                 
    {                                                                     
        (*num_results) += 2;
    } 
    /* Add r1 subtract r2 */
    r1 = radius + radius1;
    r2 = radius - radius2;
    if ((circle_x_circle2D(centre1,r1,centre2,r2,tol,results[*num_results],
                   results[(*num_results)+1],&num))
         && ( num == 2))                                 
    {                                                                     
        (*num_results) += 2;
    } 
    if (*num_results == 0)
        return(FALSE);
    else
        return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 7th-October-1988
 *
 */
BOOLEAN
circ_tanto_2circs_lin( centre1, radius1, centre2, radius2, p_on_l, dir, tol, 
                         oresults, onum_results )
                        /* Description   :- This function calculates circles that are
                         *                  tangent to the given circles and line.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 64 of "Practical Plane geometry" by
                         *                  Harrison and Baxandall for an explanation
                         *                  of the mathematics.
                         *
                         *                  In general there will be eight solutions
                         *                  to this problem.
                         */
double centre1[2];       /* <i> The first circle centre              */
double radius1;          /* <i> The radius of the first circle       */
double centre2[2];       /* <i> The second circle centre             */
double radius2;          /* <i> The radius of the second circle      */
double p_on_l[2];        /* <i> A point on the given line             */
double dir[2];           /* <i> The direction of the given line       */
double tol;              /* <i> The accuracy tolerance                */
double oresults[8][3];   /* <o> The resultant circles and their radii */
INT    *onum_results;    /* <o> The number of circles calculated      */
{
    double      loc_pl[3];
    double      loc_dir[3];
    double      loc_c1[3];
    double      loc_c2[3];
    double      dummy[3];
    double      perp1[3];
    double      perp2[3];
    double      res_cen[2];
    double      res_rad;
    double      loc1_res[2][3];
    double      loc2_res[4][3];
    double      results[8][3];
    double      temp[2][2];
    INT         num_results;

    INT         i;
    INT         j;
    INT         num_r;
    INT         num;

    num_results = 0;

    if( radius1 < tol || radius2 < tol)
    {                                    /* Positive radii only */
        return(FALSE);
    }
    if( veclen(dir,2) < tol )
    {                                    /* Line has no direction */
        return(FALSE);
    }
     
    veccpy(p_on_l,2,loc_pl);
    loc_pl[Z] = 0.0;
    veccpy(dir,2,loc_dir);
    loc_dir[Z] = 0.0;
    veccpy(centre1,2,loc_c1);
    loc_c1[Z] = 0.0;
    veccpy(centre2,2,loc_c2);
    loc_c2[Z] = 0.0;

    if(!axperp(loc_c2,loc_pl,loc_dir,perp1,dummy))     /* Both circles must be */
    {                                                  /* on the same side of the line */
        return(FALSE);  
    }
    if(!axperp(loc_c1,loc_pl,loc_dir,perp2,dummy))     
    {   
        return(FALSE);  
    }

    if( vecdp(perp1,perp2,2) < 0.0 )                  /* Centres are on opposite sides */
    {                                                 /* Does one circle overlap onto  */
        if( veclen(perp1,2) >= radius1 && veclen(perp2,2) >= radius2 )
        {                                             /* the other side? */
            return(FALSE);                            /* No! */
        }
    }

    vecnrm(perp1,2,tol,perp1);
    vecnrm(perp2,2,tol,perp2);

    if( fabs(radius1-radius2) < tol )           /* Identical radii */
    {
        vecsca(radius1,perp2,2,perp2);
        vecsum(loc_pl,perp2,2,loc_pl);          /* Move line towards circles */

        for(i=0; i < 2; i++)
        {
            if(i==1)                            /* Then move it to other side of */
            {                                   /* original position */
                vecneg(perp2,2,perp2);
                vecsca(2.0,perp2,2,perp2);
                vecsum(loc_pl,perp2,2,loc_pl);
            }
                  
            if(circ_tanto_lin_2pnts( loc_pl, dir, centre1, centre2, tol, loc1_res, &num_r))
            {
                for( j=0; j < num_r; j++ )
                {
                    results[num_results][X] = loc1_res[j][X];
                    results[num_results][Y] = loc1_res[j][Y];
                    if(i==0)
                    {
                         results[num_results][Z] = loc1_res[j][Z]+radius1;
                    }
                    else
                    {
                        results[num_results][Z] = loc1_res[j][Z]-radius1;
                    }
                    num_results++;
                }
            }
            if(circ_tanto_circ_lin_pnt( centre2, radius2+radius1, loc_pl, dir, centre1, tol, loc2_res, &num_r ))
            {
                for( j=0; j < num_r; j++ )
                {
                    res_cen[X] = loc2_res[j][X];
                    res_cen[Y] = loc2_res[j][Y];
                    res_rad    = loc2_res[j][Z];

                    if( i == 0 )
                    {
                        if(!circ_internal( res_cen, res_rad, centre2, radius2+radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] + radius1;
                            num_results++;
                        }
                    }
                    else
                    {
                        if(circ_internal( res_cen, res_rad, centre2, radius2+radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] - radius1;
                            num_results++;
                        }
                    }
                }
            }
        }
    }
    else if( radius1 < radius2 )
    {
        vecsca(radius1,perp2,2,perp2);
        vecsum(loc_pl,perp2,2,loc_pl);          /* Move line towards circles */

        for(i=0; i < 2; i++)
        {
            if(i==1)                            /* Then move it to other side of */
            {                                   /* original position */
                vecneg(perp2,2,perp2);
                vecsca(2.0,perp2,2,perp2);
                vecsum(loc_pl,perp2,2,loc_pl);
            }
                  
            if(circ_tanto_circ_lin_pnt( centre2, radius2-radius1, loc_pl, dir, centre1, tol, loc2_res, &num_r ))
            {
                for( j=0; j < num_r; j++ )
                {
                    res_cen[X] = loc2_res[j][X];
                    res_cen[Y] = loc2_res[j][Y];
                    res_rad    = loc2_res[j][Z];

                    if( i == 0 )
                    {
                        if(circ_internal( res_cen, res_rad, centre2, radius2-radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] + radius1;
                            num_results++;
                        }
                    }
                    else
                    {
                        if(!circ_internal( res_cen, res_rad, centre2, radius2-radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] - radius1;
                            num_results++;
                        }
                    }
                }
            }
            if(circ_tanto_circ_lin_pnt( centre2, radius2+radius1, loc_pl, dir, centre1, tol, loc2_res, &num_r ))
            {
                for( j=0; j < num_r; j++ )
                {
                    res_cen[X] = loc2_res[j][X];
                    res_cen[Y] = loc2_res[j][Y];
                    res_rad    = loc2_res[j][Z];

                    if( i == 0 )
                    {
                        if(!circ_internal( res_cen, res_rad, centre2, radius2+radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] + radius1;
                            num_results++;
                        }
                    }
                    else
                    {
                        if(circ_internal( res_cen, res_rad, centre2, radius2+radius1, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] - radius1;
                            num_results++;
                        }
                    }
                }
            }
        }
    }
    else if( radius2 < radius1 )
    {
        vecsca(radius2,perp1,2,perp1);
        vecsum(loc_pl,perp1,2,loc_pl);          /* Move line towards circles */

        for(i=0; i < 2; i++)
        {
            if(i==1)                            /* Then move it to other side of */
            {                                   /* original position */
                vecneg(perp1,2,perp1);
                vecsca(2.0,perp1,2,perp1);
                vecsum(loc_pl,perp1,2,loc_pl);
            }

            if(circ_tanto_circ_lin_pnt( centre1, radius1-radius2, loc_pl, dir, centre2, tol, loc2_res, &num_r ))
            {
                for( j=0; j < num_r; j++ )
                {
                    res_cen[X] = loc2_res[j][X];
                    res_cen[Y] = loc2_res[j][Y];
                    res_rad    = loc2_res[j][Z];

                    if( i == 0 )
                    {
                        if(circ_internal( res_cen, res_rad, centre1, radius1-radius2, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] + radius2;
                            num_results++;
                        }
                    }
                    else
                    {
                        if(!circ_internal( res_cen, res_rad, centre1, radius1-radius2, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] - radius2;
                            num_results++;
                        }
                    }
                }
            }
            if(circ_tanto_circ_lin_pnt( centre1, radius1+radius2, loc_pl, dir, centre2, tol, loc2_res, &num_r ))
            {
                for( j=0; j < num_r; j++ )
                {
                    res_cen[X] = loc2_res[j][X];
                    res_cen[Y] = loc2_res[j][Y];
                    res_rad    = loc2_res[j][Z];

                    if( i == 0 )
                    {
                        if(!circ_internal( res_cen, res_rad, centre1, radius1+radius2, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] + radius2;
                            num_results++;
                        }
                    }
                    else
                    {
                        if(circ_internal( res_cen, res_rad, centre1, radius1+radius2, tol  ))
                        {
                            results[num_results][X] = loc2_res[j][X];
                            results[num_results][Y] = loc2_res[j][Y];
                            results[num_results][Z] = loc2_res[j][Z] - radius2;
                            num_results++;
                        }
                    }
                }
            }
        }
    }
    *onum_results = 0;
    for (i = 0; i < num_results; i++)
    {                                                             
        results[i][Z] = ABS(results[i][Z]);
        circ_x_lin(results[i],results[i][2],p_on_l,
                     dir,tol,temp,&num);
        if (num == 1)       /* If only one intersection then it is a tangent */
        {   /* Copy in as a valid solution */ 
            oresults[*onum_results][X] = results[i][X];
            oresults[*onum_results][Y] = results[i][Y];
            oresults[*onum_results][Z] = results[i][Z];
            (*onum_results)++;
        }
    }

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 22nd-September-1988
 *
 */
BOOLEAN
circ_tanto_2circs_pnt(point,centre1,radius1,centre2,radius2,tol,results,num_results  )
                        /* Description   :- This function calculates a circle that is
                         *                  tangent to two circles and passes through 
                         *                  a point.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be four solutions
                         *                   to this problem.
                         *                   
                         *                   See pg 26 of "Practical Geometry" by D.A.Low
                         *                   for a description of the mathematics.
                         */
double  point[2];       /* <i> The point through which the circle must pass */
double  centre1[2];     /* <i> The centre of the first circle               */
double  radius1;        /* <i> The radius of the first circle               */
double  centre2[2];     /* <i> The centre of the second circle              */
double  radius2;        /* <i> The radius of the second circle              */
double  tol;            /* <i> The accuracy tolerance                       */
double  results[4][3];  /* <o> The resultant circle centres and radii       */
INT     *num_results;   /* <o> The number of circles generated              */
{            
    double  centre[2];
    double  dir1[2];
    double  dir2[2];
    double  vec1[2];
    double  loc_res[2][2];
    double  loc1_res[2][3];
    double  d[2];
    double  e[2];
    double  f[2];
    double  h[2];
    double  k[2];
    double  s[2];
    double  sim[2][2];
    double  radius;
    INT     num_r;
    INT     num_s;
    INT     i;
    INT     j;

    *num_results = 0;
    if( radius1 < tol || radius2 < tol )
    {                                           /* Positive radii only */
        return(FALSE);
    }

    
    vecdif(centre2,centre1,2,dir1);

    if(!circ_x_lin(centre1,radius1,centre1,dir1,tol,loc_res, &num_r ))
    {
        return(FALSE);
    }
    if(num_r!=2)
    {
        return(FALSE);
    }
    d[X] = loc_res[0][X];
    d[Y] = loc_res[0][Y];
    h[X] = loc_res[1][X];
    h[Y] = loc_res[1][Y];
    if(!circ_x_lin(centre2,radius2,centre1,dir1,tol,loc_res, &num_r ))
    {
        return(FALSE);
    }
    if(num_r!=2)
    {
        return(FALSE);
    }
    e[X] = loc_res[0][X];
    e[Y] = loc_res[0][Y];
    k[X] = loc_res[1][X];
    k[Y] = loc_res[1][Y];

    if(!centres_of_similitude( centre1,radius1, centre2, radius2, tol, sim, &num_s ))
    {
        return(FALSE);
    }

    for(i=0; i <num_s; i++)
    {                                           /* For each centre of similitude */
        s[X] = sim[i][X];
        s[Y] = sim[i][Y];
        if(i==0)
        {            
            if(!circumcentre(d,e,point,tol,centre,&radius))
            {
                return(FALSE);
            }
        }
        else
        {
            if(!circumcentre(h,e,point,tol,centre,&radius))
            {
                return(FALSE);
            }
        }

        vecdif(s,point,2,dir2);

        if(!circ_x_lin(centre,radius,point,dir2,tol,loc_res, &num_r ))
        {
           return(FALSE);
        }
        if(num_r!=2)
        {
            return(FALSE);
        }
        f[X] = loc_res[0][X];
        f[Y] = loc_res[0][Y]; 
        vecdif(f,point,2,vec1);                 /* Select intersection that is not */
        if( veclen(vec1,2) < tol )              /* the point itself */

        {
            f[X] = loc_res[1][X];
            f[Y] = loc_res[1][Y]; 
        }
        if(!circ_tanto_circ_2pnts( f, point, centre1,radius1, tol, loc1_res, &num_r ))
        {
            return(FALSE);
        }
        if(num_r!=2)
        {
            return(FALSE);
        }
        for(j=0; j < 2; j++)
        {                                       /* Both solutions are correct */
            results[2*i+j][X] = loc1_res[j][X];
            results[2*i+j][Y] = loc1_res[j][Y];
            results[2*i+j][Z] = loc1_res[j][Z];
            (*num_results)++;
        }
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn/modelle
 *                                                        Date   :: 20th-September-1988
 *
 */
BOOLEAN
circ_tanto_2lin(pnt_1,dir_1,pnt_2,dir_2,radius, tol, results, num_results)
                        /* Description   :- Given a radius and two intersecting lines
                         *                  this function calculates the four circles
                         *                  that are tangent to the two lines.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 38 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for an
                         *                   explantion of the mathematics.
                         *
                         *                   In general there will be four solutions
                         *                   to this problem.
                         *
                         */
double  pnt_1[2];       /* <i> A point through which the first line passes  */
double  dir_1[2];       /* <i> The direction of the first line              */
double  pnt_2[2];       /* <i> A point through which the second line passes */
double  dir_2[2];       /* <i> The direction of the second line             */
double  radius;         /* <i> The radius of the resulting circles          */
double  tol;            /* <i> The accuracy tolerance                       */
double  results[4][2];  /* <o> The centres of the four solutions            */
INT     *num_results;   /* <o> The number of circles generated              */
{                                               
      double    implicit1[3];
      double    implicit2[3];
      double    determ;
      double    detinv;
      double    ab1;
      double    ab2;
      double    c1rab1;
      double    c2rab2;
      INT       i;
                       
      *num_results = 0;

      if( veclen(dir_1,2) < tol || veclen(dir_2,2) < tol )
      {                                   /* Line has no direction */
          return(FALSE);
      }
      para_to_implicit(pnt_1,dir_1, implicit1  );
      para_to_implicit(pnt_2,dir_2, implicit2  );

      determ = implicit2[0]*implicit1[1] - implicit1[0]*implicit2[1];
      if(ABS(determ) < tol)
      {                                   /* Lines are parallel */
        return(FALSE);
      }
      ab1 = sqrt(implicit1[0]*implicit1[0] + implicit1[1]*implicit1[1]);
      ab2 = sqrt(implicit2[0]*implicit2[0] + implicit2[1]*implicit2[1]);
                                                              
      detinv = 1.0/determ;

      for( i = 0; i < 4; i++)
      {
          switch(i){

          case 0:
              c1rab1 = implicit1[2] + radius*ab1;
              c2rab2 = implicit2[2] + radius*ab2;
              break;
          case 1:
              c1rab1 = implicit1[2] + radius*ab1;
              c2rab2 = implicit2[2] - radius*ab2;
              break;
          case 2:
              c1rab1 = implicit1[2] - radius*ab1;
              c2rab2 = implicit2[2] + radius*ab2;
              break;
          case 3:
              c1rab1 = implicit1[2] - radius*ab1;
              c2rab2 = implicit2[2] - radius*ab2;
              break;
          default:                      /* Impossible condition */
              return(FALSE);
          }
          results[i][X] = (implicit2[1]*c1rab1 - implicit1[1]*c2rab2)*detinv;
          results[i][Y] = (implicit1[0]*c2rab2 - implicit2[0]*c1rab1)*detinv;
      }
      *num_results = 4;
      return( TRUE );                                                     

}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 4th-October-1988
 *
 */
BOOLEAN
circ_tanto_2lin_circ(p1,dir1,p2,dir2,centre,radius,tol,results,num_results )
                        /* Description   :- This function calculates the circle tangent
                         *                  to the two given lines and circle.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be two solutions
                         *                   to this problem. 
                         *                   But if the two lines intersect the 
                         *                   circle we have to generate if possible
                         *                   the internal solutions as well.
                         *                                     
                         *                   See p 64 Solution 69 of
                         *                   "Practical geometry and graphics"
                         *                   by Harrison and Baxandall.
                         *
                         */
double p1[2];           /* <i> A point on the first line             */
double dir1[2];         /* <i> The direction of the first line       */
double p2[2];           /* <i> A point on the second line            */
double dir2[2];         /* <i> The direction of the second line      */
double centre[2];       /* <i> The circle centre                     */
double radius;          /* <i> The radius of the circle              */
double tol;             /* <i> The accuracy tolerance                */
double results[8][3];   /* <o> The resultant circles and their radii */
INT    *num_results;    /* <o> The number of circles calculated      */
{
    BOOLEAN inside_circle;
    BOOLEAN line1;
    BOOLEAN line2;
    double  a[2];
    double  z[3];
    double  loc_p1[3];
    double  loc_dir1[3];
    double  loc_p2[3];
    double  loc_dir2[3];
    double  loc_centre[3];                                          
    double  dir[3];
    double  loc_res[8][3];
    double  loc2_res[8][2];                                              
    double  temp_res[2][2];
    double  s;
    double  t;        
    double  x1[2];
    double  x2[2];                                     
    double  outvec[4];
    double  outvec1[4];  
    double  outpoint[4];                                
    double  outpoint1[4];
    double  intersection[2];
    double  rad;         
    double  A[3];
    double  P1[3];
    double  P2[3];
    double  E[3];
    double  ldir1[3];
    double  ldir2[3];
    INT     num_r;
    INT     num1_r;
    INT     num1;
    INT     num2;
    INT     i;      

    *num_results = 0;
    if( radius < tol )
    {                                       /* Positive radius only */
        return(FALSE);
    }
    if( veclen(dir1,2) < tol || veclen(dir2,2) < tol )
    {                                       /* Lines have no direction */
        return(FALSE);
    }

    vecw(z);                                /* 3D Dependent functions will use these */
    veccpy(p1,2,loc_p1);                    /* version of input variables */
    loc_p1[Z] = 0.0;
    veccpy(p2,2,loc_p2);
    loc_p2[Z] = 0.0;
    veccpy(dir1,2,loc_dir1);
    loc_dir1[Z] = 0.0;
    veccpy(dir2,2,loc_dir2);
    loc_dir2[Z] = 0.0;
    veccpy(centre,2,loc_centre);
    loc_centre[Z] = 0.0;

    if(!line_x_line2D( p1, dir1, p2, dir2, tol, a, &s, &t))
    {   /*  Lines are parallel */   
        /* This is so simple it should have struck me earlier treat it as */
        /* circles tangent to a line and a circle of radius which is half the */
        /* distance between the parallel lines and then check for valid solutions */
        axperpl2D( loc_p2,loc_p1,loc_dir1, &rad );
        rad /= 2.0;

        if (!circ_tanto_lin_circ(rad,loc_p1,loc_dir1,loc_centre,radius,tol,
                         loc2_res,&num_r))
            return(FALSE);

        for (i = 0; i < num_r; i++)
        {   /* Only accept circles that are tangent to both lines */
            circ_x_lin(loc2_res[i],rad,loc_p1,loc_dir1,tol,temp_res,&num1);
            circ_x_lin(loc2_res[i],rad,loc_p2,loc_dir2,tol,temp_res,&num2);

            if ((num1 == 1) && (num2 == 1))
            {   /* Valid solution */
                results[(*num_results)][X] = loc2_res[i][X];
                results[(*num_results)][Y] = loc2_res[i][Y];
                results[(*num_results)][Z] = rad;
                (*num_results)++;
            }                                             
        }
        return(TRUE);
    }      
    /* Calculate a perp axis from the line to the centre of the circle */
    axperp( loc_centre,loc_p1,loc_dir1, outvec, outpoint);
    axperp(loc_centre,loc_p2,loc_dir2, outvec1,outpoint1);         

    /* We want to see if they are both on the same side of the circle centre */
    if (veclen(outvec,2) < veclen(outvec1,2))
    {   /* Outvec is closer so intersect outvec1 with loc_p1 and loc_dir1 */
        if (line_x_line2D(loc_p1,loc_dir1,outpoint1,outvec1,tol,
                              intersection,&s,&t))
        {   
            if ((t >= 0.0) && (t <= 1.0))
                vecneg(outvec,2,outvec);
        }
    }
    else
    {   /* Outvec1 is closer so intersect outvec with loc_p2 and loc_dir2 */
        if (line_x_line2D(loc_p2,loc_dir2,outpoint,outvec,tol,
                              intersection,&s,&t))
        {   
            if ((t >= 0.0) && (t <= 1.0))
                vecneg(outvec1,2,outvec1);
        }
    }                               

    vecnrm(outvec,2,tol,outvec);
    vecneg(outvec,2,outvec);
    vecsca(radius,outvec,2,outvec);
    vecsum(p1,outvec,2,loc_p1);
                           
    vecnrm(outvec1,2,tol,outvec1);
    vecneg(outvec1,2,outvec1);
    vecsca(radius,outvec1,2,outvec1);
    vecsum(p2,outvec1,2,loc_p2);

    if(!circ_tanto_2lin_pnt(loc_p1,dir1,loc_p2,dir2,centre,tol,0,loc_res,&num_r))
    {
        return(FALSE);
    }                

    /* If the two lines intersect the circle see if we can generate the internal */
    /* solutions and the external virtual solutions */                         
    num1_r = 0;
    line1 = line_x_circle2D(p1,dir1,loc_centre,radius,tol,x1,x2);
    line2 = line_x_circle2D(p2,dir2,loc_centre,radius,tol,x1,x2);

    if (line1 && line2)
    {             
        /* Reverse the direction of the two vectors so that we get the internal */
        /* Circles */
        vecneg(outvec,2,outvec);
        vecsum(p1,outvec,2,loc_p1);

        vecneg(outvec1,2,outvec1);
        vecsum(p2,outvec1,2,loc_p2);

        if(!circ_tanto_2lin_pnt(loc_p1,dir1,loc_p2,dir2,centre,tol,0,
                                   loc_res[num_r],&num1_r))
        {
           return(FALSE);
        }                                    
        num_r += num1_r;
    }

    if (line1 || line2)
    {   /* Work out the virtual circles, this is slightly convoluted as you say */
        for (i = 1; i < 3; i++)
        {
            /* We have to work out which way the lines have to be offset set. */
            /* intersect them and work out a point */
            if(!line_x_line2D(p1,dir1,p2,dir2,tol,A,&s,&t))
                return(FALSE);
            /* Work out whether the point of intersection is within the circle or not */
            vecdif(A,loc_centre,2,dir);
            if (radius >= veclen(dir,2))
                inside_circle = TRUE;
            else
                inside_circle = FALSE;
            vecdif(p1,A,2,ldir1);
            vecdif(p2,A,2,ldir2);
                          
            if (!inside_circle)
            {
                if ((i == 1) || (i == 3))
                    vecneg(ldir1,2,ldir1);
                if ((i == 2) || (i == 3))
                    vecneg(ldir2,2,ldir2);
            }                             
            else if ((inside_circle) && (i == 2))
            {   /* Flip both of them */
                    vecneg(ldir1,2,ldir1);
                    vecneg(ldir2,2,ldir2);
            }

            vecnrm(ldir1,2,TOL,ldir1);
            vecnrm(ldir2,2,TOL,ldir2);
            vecsca(100.0,ldir1,2,ldir1);
            vecsca(100.0,ldir2,2,ldir2);
            vecsum(A,ldir1,2,P1);
            vecsum(A,ldir2,2,P2);
            vecdif(P1,P2,2,dir);
            vecsca(0.5,dir,2,dir);
            vecsum(P2,dir,2,E); 

            veccpy(p1,2,loc_p1);                    /* version of input variables */
            loc_p1[Z] = 0.0;
            veccpy(p2,2,loc_p2);
            loc_p2[Z] = 0.0;

            axperp2D(E,p1,loc_dir1,outvec,outpoint);
            axperp2D(E,p2,loc_dir2,outvec1,outpoint1);

            vecnrm(outvec,2,tol,outvec);
            vecneg(outvec,2,outvec);
            vecsca(radius,outvec,2,outvec);
            vecsum(p1,outvec,2,loc_p1);
                           
            vecnrm(outvec1,2,tol,outvec1);
            vecneg(outvec1,2,outvec1);
            vecsca(radius,outvec1,2,outvec1);
            vecsum(p2,outvec1,2,loc_p2);
                                      
            if (!inside_circle)
                circ_tanto_2lin_pnt(loc_p1,dir1,loc_p2,dir2,centre,tol,i,
                            loc_res[num_r],&num1_r);
            else
                circ_tanto_2lin_pnt(loc_p1,dir1,loc_p2,dir2,centre,tol,0,
                            loc_res[num_r],&num1_r);                    

            num_r += num1_r;
        }
    }

    for(i=0; i < num_r; i++)
    {                 
        results[(*num_results)][X] = loc_res[i][X];
        results[(*num_results)][Y] = loc_res[i][Y];
        results[(*num_results)][Z] = ABS((loc_res[i][Z] - radius));
        (*num_results)++;
    }                                   

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 4th-October-1988
 *
 */
BOOLEAN
circ_tanto_2lin_pnt(p_on_line1,dir1,p_on_line2,dir2,point,tol,flip_dir,
                       results,num_results )
                        /* Description   :- This function calculates the circle tangent
                         *                  to the given lines that passes through the
                         *                  given point.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  In general there will be two solutions
                         *                   to this problem.
                         *
                         *                   See pg 22 of "Practical Geometry" by
                         *                   D.A. Low for a description of the 
                         *                   Mathematics.
                         */
double p_on_line1[2];   /* <i> A point on the first line                      */
double dir1[2];         /* <i> The direction of the first line                */
double p_on_line2[2];   /* <i> A point on the second line                     */
double dir2[2];         /* <i> The direction of the second line               */
double point[2];        /* <i> The point through which the circle should pass */
double tol;             /* <i> The accuracy tolerance                         */
int32_t   flip_dir;        /* <i> Whether to flip the directions of dir1 and dir2*/
                        /* 0 = No flip, 1 = dir1, 2 = dir2, 3 = dir1 & dir2   */
double results[2][3];   /* <o> The resultant circles and their radii          */
INT    *num_results;    /* <o> The number of circles calculated               */
{
    double  loc_res[2][2];
    double  loc_p1[3];
    double  loc_p2[3];
    double  loc_dir1[3];
    double  loc_dir2[3];       
    double  ldir1[3];
    double  ldir2[3];
    double  loc_point[3];
    double  outvec[4];
    double  diameter;
    double  dist1;
    double  dist2;
    double  radius;
    double  s;
    double  t;
    INT     num_r; 
    INT     i;     
    POINT_2D    A;
    POINT_2D    E;
    POINT_2D    F;
    POINT_2D    O;
    POINT_2D    K;
    POINT_2D    K1;           
    POINT_2D    P1;
    POINT_2D    P2;
    VECTOR_2D   aedir;
    VECTOR_2D   dir;
    double      frad;

    *num_results = 0;
    if( veclen(dir1,2) < tol || veclen(dir2,2) < tol )
    {                                               /* Lines have no direction */
        return(FALSE);
    }


    veccpy(p_on_line1,2,loc_p1);                    /* Used by 3D dependent routines */
    loc_p1[Z] = 0.0;
    veccpy(p_on_line2,2,loc_p2);
    loc_p2[Z] = 0.0;
    veccpy(dir1,2,loc_dir1);
    loc_dir1[Z] = 0.0;
    veccpy(dir2,2,loc_dir2);
    loc_dir2[Z] = 0.0;
    veccpy(point,2,loc_point);
    loc_point[Z] = 0.0;

    axperp( loc_point,loc_p1,loc_dir1, outvec, loc_p1);
    axperp( loc_point,loc_p2,loc_dir2, outvec, loc_p2);

    if(!line_x_line2D( p_on_line1, dir1, p_on_line2, dir2, tol, A, &s, &t))
    {                                                  /* Lines are parallel */
        if(!axperpl(loc_p1,loc_p2,loc_dir2,&diameter))
        {
            return(FALSE);
        }
        radius = diameter/2.0;

        if(!axperpl(loc_point,loc_p1,loc_dir1,&dist1)) /* Check that point lies */
        {                                              /* between lines */
            return(FALSE);
        }
        if(!axperpl(loc_point,loc_p2,loc_dir2,&dist2))
        {
            return(FALSE);
        }
        if( ((dist1+dist2)-diameter)>tol)
        {                                              /* Point lies outside */
            return(FALSE);
        }
  
        if(!circ_tanto_lin_pnt( radius, p_on_line2, dir2, point, tol ,loc_res, &num_r ))
        {
            return(FALSE);
        }
        for(i=0; i < num_r; i++)
        {                                              /* For each solution */
            results[(*num_results)][X] = loc_res[i][X];
            results[(*num_results)][Y] = loc_res[i][Y];
            results[(*num_results)][Z] = radius;
            (*num_results)++;
        }
        return(TRUE);
    }
    /* Intersection point is A */
    /* Calculate the mid vector */
    vecdif(loc_p1,A,2,ldir1);
    vecdif(loc_p2,A,2,ldir2);
                          
    /* See if we have to flip the sense of the direction vectors to generate the */
    /* virtual circles tangent to the circle and line */
    if (flip_dir != 0)
    {
        if ((flip_dir == 1) || (flip_dir == 3))
            vecneg(ldir1,2,ldir1);
        if ((flip_dir == 2) || (flip_dir == 3))
            vecneg(ldir2,2,ldir2);
    }

    vecnrm(ldir1,2,TOL,ldir1);
    vecnrm(ldir2,2,TOL,ldir2);
    vecsca(100.0,ldir1,2,ldir1);
    vecsca(100.0,ldir2,2,ldir2);
    vecsum(A,ldir1,2,P1);
    vecsum(A,ldir2,2,P2);
    vecdif(P1,P2,2,dir);
    vecsca(0.5,dir,2,dir);
    vecsum(P2,dir,2,E);
    /* Take a mid point along AE and call it F */
    vecdif(A,E,2,dir);  
    vecsca(0.5,dir,2,dir);
    vecsum(E,dir,2,F);
    /* Calculate the perpindicular distance from F to one of the lines */
    /* And this will be the radius of the circle */ 
    axperpl2D(F,A,dir1,&frad);

    /* Now take the point that was given and the point A and intersect this line */
    /* With the circle of centre F and radius frad and call the intersection points */
    /* K and K1 */
    vecdif(A,point,2,dir);
    if (!line_x_circle2D(A,dir,F,frad,TOL,K,K1))
        return(FALSE);

    for (*num_results = 0; *num_results < 2; (*num_results)++)
    {
        if (*num_results == 1)
            veccpy(K1,2,K);

       /* Now take each K and K1 in turn from F and generate a vector */
       vecdif(F,K,2,dir);
       /* Now take this vector passing through AE and calculate the line of intersection*/
       /* with the vector through F,K passing through the point given ie D (point)  */
       vecdif(A,E,2,aedir);
       line_x_line2D(A,aedir,point,dir,TOL,O,&s,&t);

       /* Get the length of the vector OE and subtract the radius from it */
       vecdif(O,point,2,dir);

       results[(*num_results)][X] = O[X];
       results[(*num_results)][Y] = O[Y];
       results[(*num_results)][Z] = veclen(dir,2);
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 22nd-September-1988
 *
 */
BOOLEAN
circ_tanto_3circs( cen1,rad1,cen2,rad2,cen3,rad3,tol,oresults,onum_results )
                        /* Description   :- This function calculates the circles
                         *                  tangent to three circles
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 27 of "Practical Geometry"
                         *                  by D.A.Low for a description of the
                         *                  Algorithm.
                         *
                         *                  There is a maximum of eight solutions to
                         *                  this problem.
                         *                  
                         */
double  cen1[2];        /* <i> The centre of the first circle        */
double  rad1;           /* <i> The radius of the first circle        */
double  cen2[2];        /* <i> The centre of the second circle       */
double  rad2;           /* <i> The radius of the second circle       */
double  cen3[2];        /* <i> The centre of the third circle        */
double  rad3;           /* <i> The radius of the third circle        */
double  tol;            /* <i> The accuracy tolerance                */
double  oresults[8][3]; /* <o> The centres of the resultant circles  */
INT     *onum_results;  /* <o> The number of solutions               */
{    
    double  loc_cen1[2];                /* Local copies of input variables */
    double  loc_rad1; 
    double  loc_cen2[2];  
    double  loc_rad2;    
    double  loc_cen3[2];   
    double  loc_rad3;      
    double  temp;
    double  tempr[2][2];
    INT     num;
    INT     num1;
    INT     num2;
    INT     i;          
    INT     num_results;
    double  results[8][3];

    num_results = 0;  
    if( rad1 <= 0.0 || rad2 <= 0.0 || rad3 <= 0.0 )
    {                                   /* Positive radii only */
        return(FALSE);
    }
                       
    loc_rad1 = rad1;                    /* Assuming rad1 is smallest */
    loc_rad2 = rad2;
    loc_rad3 = rad3;
    veccpy(cen1,2,loc_cen1);
    veccpy(cen2,2,loc_cen2);
    veccpy(cen3,2,loc_cen3);

    if(rad2 <= rad1 && rad2 <= rad3 )
    {                                   /* rad2 is the smallest */
        loc_rad1 = rad2;
        loc_rad2 = rad1;
        vecswap(loc_cen2,2,loc_cen1);
    }
    else if(rad3 <= rad1 && rad3 <= rad2 )
    {                                   /* rad3 is the smallest */
        loc_rad1 = rad3;
        loc_rad3 = rad1;
        vecswap(loc_cen1,2,loc_cen3);
    }
    if (loc_rad3 < loc_rad2)
    {   /* Ensure loc_rad3 is the largest one of the three */
        vecswap(loc_cen2,2,loc_cen3);
        temp = loc_rad3;
        loc_rad3 = loc_rad2;
        loc_rad2 = temp;
    }
        
    if(!circ_tanto_3circs_o(loc_cen1,loc_rad1,loc_cen2,loc_rad2,loc_cen3,loc_rad3,tol,results,&num_results ))
    {
        return(FALSE);
    }         
    for (*onum_results = 0,i = 0; i < num_results; i++)
    {
        results[i][Z] = ABS(results[i][Z]);
        if (circ_x_circ(results[i],results[i][2],cen1,rad1,tol,tempr,&num))
            if (circ_x_circ(results[i],results[i][2],cen2,rad2,tol,tempr,&num1))
               if (circ_x_circ(results[i],results[i][2],cen3,rad3,tol,tempr,&num2))
               {
                   if ((num == 1)  && (num1 == 1) && (num2 == 1))    /* If only one intersection then it is a tangent */
                   {   /* Copy in as a valid solution */ 
                       oresults[*onum_results][X] = results[i][X];
                       oresults[*onum_results][Y] = results[i][Y];
                       oresults[*onum_results][Z] = results[i][Z];
                       (*onum_results)++;
                   }
               }
    }

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 22nd-September-1988
 *
 */
BOOLEAN
circ_tanto_3circs_o( cen1,rad1,cen2,rad2,cen3,rad3,tol,results,num_results )
                        /* Description   :- This function calculates the circles
                         *                  tangent to three circles
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See pg 27 of "Practical Geometry"
                         *                  by D.A.Low for a description of the
                         *                  Algorithm.
                         *
                         *                  There is a maximum of eight solutions to
                         *                  this problem.
                         *                  
                         *                  The circles must be sorted so that the first
                         *                  is the smallest.
                         */
double  cen1[2];        /* <i> The centre of the first circle        */
double  rad1;           /* <i> The radius of the first circle        */
double  cen2[2];        /* <i> The centre of the second circle       */
double  rad2;           /* <i> The radius of the second circle       */
double  cen3[2];        /* <i> The centre of the third circle        */
double  rad3;           /* <i> The radius of the third circle        */
double  tol;            /* <i> The accuracy tolerance                */
double  results[8][3];  /* <o> The centres of the resultant circles */
INT     *num_results;   /* <o> The number of solutions               */
{
    double  loc_res[4][3];
    double  loc1_res[2][3];
    double  centre[2];
    double  vec[2];
    double  radius;
    double  rad_1;
    double  rad_2;
    double  rad_3;
    double  res_cen[2];
    double  res_rad;
    INT     num_r;                      
    INT     i;
    INT     j;

    *num_results = 0;  

    if( rad1 < tol || rad2 < tol || rad3 < tol )
    {                                   /* Positive radii only */
        return(FALSE);
    }
    if( rad1 < rad2 && rad1 < rad3 )
    {                                   /* rad1 is smallest */
        rad_2 = rad2 - rad1;
        rad_3 = rad3 + rad1;
        if(circ_tanto_2circs_pnt(cen1,cen2,rad_2,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)     /* Accept one internal and one external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
            }
        }
        rad_2 = rad2 + rad1;
        rad_3 = rad3 - rad1;
        if(circ_tanto_2circs_pnt(cen1,cen2,rad_2,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)     /* Accept one internal and one external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
            }
        }
        rad_2 = rad2 - rad1;
        rad_3 = rad3 - rad1;
        if(circ_tanto_2circs_pnt(cen1,cen2,rad_2,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)     /* Accept two internal or two external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
            }
        }
        rad_2 = rad2 + rad1;
        rad_3 = rad3 + rad1;
        if(circ_tanto_2circs_pnt(cen1,cen2,rad_2,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)     /* Accept two internal or two external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
            }
        }
    }
    else if( (rad1 - rad2)< tol && rad1 < rad3 )
    {                                   /* Two identical radii and one larger radius */
        rad_3 = rad3 + rad1;
                      
        if(circ_tanto_circ_2pnts(cen1,cen2,cen3,rad_3,tol,loc1_res,&num_r))
        {
            for(i=0; i <num_r; i++)   
            {   
                results[(*num_results)][X] = loc1_res[i][X];
                results[(*num_results)][Y] = loc1_res[i][Y];

                if( i == 0 )        /* Assuming that the circle tangent to the furthest */
                {                   /* side is given first */
                    results[(*num_results)][Z] = loc1_res[i][Z] - rad1;
                }
                else
                {
                    results[(*num_results)][Z] = loc1_res[i][Z] + rad1;
                }

                (*num_results)++;
            }
        }
        rad_3 = rad3 - rad1;
                      
        if(circ_tanto_circ_2pnts(cen1,cen2,cen3,rad_3,tol,loc1_res,&num_r))
        {
            for(i=0; i <num_r; i++)   
            {   
                results[(*num_results)][X] = loc1_res[i][X];
                results[(*num_results)][Y] = loc1_res[i][Y];

                if( i == 0 )        /* Assuming that the circle tangent to the furthest */
                {                   /* side is given first */
                    results[(*num_results)][Z] = loc1_res[i][Z] + rad1;
                }
                else
                {
                    results[(*num_results)][Z] = loc1_res[i][Z] - rad1;
                }

                (*num_results)++;
            }
        }
        rad_2 = rad2 + rad1;
        rad_3 = rad3 - rad1;
        if(circ_tanto_2circs_pnt(cen1,cen2,rad_2,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)    /* Accept one internal and one external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen2, rad_2, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
            }
        }
        rad_1 = rad1 + rad2;
        rad_3 = rad3 - rad2;
        if(circ_tanto_2circs_pnt(cen2,cen1,rad_1,cen3,rad_3,tol,loc_res,&num_r ))
        {
            for(i=0; i <num_r; i++)     /* Accept one internal and one external */
            {
                res_cen[X] = loc_res[i][X];
                res_cen[Y] = loc_res[i][Y];
                res_rad    = loc_res[i][Z];

                if(circ_internal( res_cen, res_rad, cen1, rad_1, tol  ))
                {
                    if(!circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] - rad1;
                        (*num_results)++;
                    }
                }
                else if(!circ_internal( res_cen, res_rad, cen1, rad_1, tol  ))
                {
                    if(circ_internal( res_cen, res_rad, cen3, rad_3, tol  ))
                    {
                        results[(*num_results)][X] = loc_res[i][X];
                        results[(*num_results)][Y] = loc_res[i][Y];
                        results[(*num_results)][Z] = loc_res[i][Z] + rad1;
                        (*num_results)++;
                    }
                }
            }
        }
    }
    else if( fabs(rad1-rad2) < tol && fabs(rad1-rad3) < tol )
    {                                   /* Three identical radii */
        if(!circumcentre(cen1,cen2,cen3,tol,centre,&radius))
        {
            return(FALSE);
        }
        vecdif(cen1,centre,2,vec);
        radius = veclen(vec,2);

        results[(*num_results)][X] = centre[X];       /* Two solutions are at the */
        results[(*num_results)][Y] = centre[Y];       /* circumcentre */
        results[(*num_results)][Z] = radius-rad1;
        (*num_results)++;
        results[(*num_results)][X] = centre[X];
        results[(*num_results)][Y] = centre[Y];
        results[(*num_results)][Z] = radius+rad1;
        (*num_results)++;
        
        for(i=0; i<3;i++)                             /* Six solutions come from */
        {                                             /* combinations of two points */
            switch (i) {                              /* and a circle with radius */
            case 0:                                   /* 2*r */
                rad_3 = rad3+rad1;  

                circ_tanto_circ_2pnts(cen2,cen1,cen3,rad_3,tol,loc1_res,&num_r);
                break;
            case 1:
                rad_2 = rad2+rad1;

                circ_tanto_circ_2pnts(cen3,cen1,cen2,rad_2,tol,loc1_res,&num_r);
                break;
            case 2:
                rad_1 = rad1+rad2;

                circ_tanto_circ_2pnts(cen2,cen3,cen1,rad_1,tol,loc1_res,&num_r);
                break;
            default:
                break;
            }
            for(j=0; j <num_r; j++)   
            {   
                results[(*num_results)][X] = loc1_res[j][X];
                results[(*num_results)][Y] = loc1_res[j][Y];

                if( j == 0 )        /* Assuming that the circle tangent to the furthest */
                {                   /* side is given first */
                    results[(*num_results)][Z] = loc1_res[j][Z] - rad1;
                }
                else
                {
                    results[(*num_results)][Z] = loc1_res[j][Z] + rad1;
                }

                (*num_results)++;
            }
        }
    }
    else
    {
        return(FALSE);
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 7th-September-1988
 *
 */
BOOLEAN
circs_tanto_3lin(p_on_lines,dir_of_lines, tol, oresults, onum_results )
                        /* Description   :- This function calculates the seven circles
                         *                  that are tangent to three lines.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  When none of the directions are parallel
                         *                   the will be 7 solutions.
                         *                   If two of the directions are parallel
                         *                   there will be 2 solutions.
                         *
                         */
double p_on_lines[3][2];   /* <i> Points that lie on the lines          */
double dir_of_lines[3][2]; /* <i> Direction of the lines                */
double tol;                /* <i> Accuracy tolerance                    */
double oresults[7][3];     /* <o> The resultant centres and their radii */
INT    *onum_results;      /* <o> The number of circles calculated      */
{                    
    BOOLEAN first;
    BOOLEAN second;
    BOOLEAN third;
    BOOLEAN tangent;

    double  p_on_1[2];
    double  p_on_2[2];
    double  p_on_3[2];

    double  dir_1[3];
    double  dir_2[3];
    double  dir_3[3];

    double  tri_12[3];
    double  tri_23[3];
    double  tri_31[3];

    double  centre[2];
    double  radius;
    double  s;
    double  t;                              

    double  results[7][3];
    double  temp[2][2];

    double  tmp_1[2];
    double  tmp_2[2];
    double  tmp_3[2];
    double  tmp_4[3];
    double  tmp_5[3];
    double  tmp_6[3];
    double  point[3];
    double  vec[2];

    INT     count;
    INT     i;         
    INT     j;
    INT     num_results;                             
    INT     num;

                   
    point[Z]=0.0;               /* Set Z component to zero for 3D routines */
    tri_12[Z]=0.0;
    tri_23[Z]=0.0;
    tri_31[Z]=0.0;
    dir_1[Z]=0.0;
    dir_2[Z]=0.0;
    dir_3[Z]=0.0;

    p_on_1[X] = p_on_lines[0][X];
    p_on_1[Y] = p_on_lines[0][Y];
    p_on_2[X] = p_on_lines[1][X];
    p_on_2[Y] = p_on_lines[1][Y];
    p_on_3[X] = p_on_lines[2][X];
    p_on_3[Y] = p_on_lines[2][Y];

    dir_1[X] = dir_of_lines[0][X];
    dir_1[Y] = dir_of_lines[0][Y];
    dir_2[X] = dir_of_lines[1][X];
    dir_2[Y] = dir_of_lines[1][Y];
    dir_3[X] = dir_of_lines[2][X];
    dir_3[Y] = dir_of_lines[2][Y];
                
    if( veclen(dir_1,2) < tol || veclen(dir_2,2) < tol || veclen(dir_3,2) < tol )
    {                           /* Lines have no direction */
        return(FALSE);
    }
    count  = 0;
    first  = FALSE;
    second = FALSE;
    third  = FALSE;

    if(line_x_line2D( p_on_1, dir_1, p_on_2, dir_2, tol, tri_12, &s, &t))
    {  
        first = TRUE;
        count++;
    }
    if(line_x_line2D( p_on_2, dir_2, p_on_3, dir_3, tol, tri_23, &s, &t))
    {
        second = TRUE;
        count++;
    }
    if(line_x_line2D( p_on_3, dir_3, p_on_1, dir_1, tol, tri_31, &s, &t))
    {         
        third = TRUE;
        count++;
    }
    switch (count) {
    case    0:
            return(FALSE);      /* All lines are parallel -> No solution */
    case    2: 
            if( first && second )
            {
                if(!vecdir( dir_1,2,dir_3))
                {
                    vecneg(dir_3,2,dir_3);
                }
                vecdif( tri_12, tri_23,2, tmp_1);

                vecnrm(dir_1,2,tol,dir_1);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(dir_3,2,tol,dir_3);

                vecsum(dir_3,tmp_1,2,tmp_2);
                vecneg(tmp_1,2,tmp_1);
                vecsum(dir_1,tmp_1,2,tmp_3);

                if(!line_x_line2D( tri_23, tmp_2, tri_12, tmp_3, tol, point, &s, &t))
                {  
                    return(FALSE);
                }
                if(!axperpl(point,p_on_1,dir_1,&radius))
                {
                    return(FALSE);
                }
                results[0][X] = point[X];
                results[0][Y] = point[Y];
                results[0][Z] = radius;

                vecdif(tmp_1,dir_1,2,tmp_3);
                vecsum(tmp_1,dir_3,2,tmp_2);
                vecneg(tmp_2,2,tmp_2);
                if(!line_x_line2D( tri_23, tmp_2, tri_12, tmp_3, tol, point, &s, &t))
                {
                    return(FALSE);
                }
                results[1][X] = point[X];
                results[1][Y] = point[Y];
                results[1][Z] = radius;
            }
            else if(first && third )
            {
                if(!vecdir( dir_2,2,dir_3))
                {
                    vecneg(dir_3,2,dir_3);
                }
                vecdif( tri_12, tri_31,2, tmp_1);

                vecnrm(dir_2,2,tol,dir_2);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(dir_3,2,tol,dir_3);

                vecsum(dir_3,tmp_1,2,tmp_2);
                vecneg(tmp_1,2,tmp_1);
                vecsum(dir_2,tmp_1,2,tmp_3);

                if(!line_x_line2D( tri_31, tmp_2, tri_12, tmp_3, tol, point, &s, &t))
                {
                    return(FALSE);
                }
                if(!axperpl(point,p_on_2,dir_2,&radius))
                {
                    return(FALSE);
                }
                results[0][X] = point[X];
                results[0][Y] = point[Y];
                results[0][Z] = radius;

                vecdif(tmp_1,dir_2,2,tmp_3);
                vecsum(tmp_1,dir_3,2,tmp_2);
                vecneg(tmp_2,2,tmp_2);
                if(!line_x_line2D( tri_31, tmp_2, tri_12, tmp_3, tol, point, &s, &t))
                {
                    return(FALSE);
                }
                results[1][X] = point[X];
                results[1][Y] = point[Y];
                results[1][Z] = radius;
            }
            else
            {                   /* Second and third are TRUE */
                if(!vecdir( dir_1,2,dir_2))
                {
                    vecneg(dir_2,2,dir_2);
                }
                vecdif( tri_31, tri_23,2, tmp_1);

                vecnrm(dir_1,2,tol,dir_1);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(dir_2,2,tol,dir_2);

                vecsum(dir_2,tmp_1,2,tmp_2);
                vecneg(tmp_1,2,tmp_1);
                vecsum(dir_1,tmp_1,2,tmp_3);

                if(!line_x_line2D( tri_23, tmp_2, tri_31, tmp_3, tol, point, &s, &t))
                {
                    return(FALSE);
                }
                if(!axperpl(point,p_on_1,dir_1,&radius))
                {
                    return(FALSE);
                }
                results[0][X] = point[X];
                results[0][Y] = point[Y];
                results[0][Z] = radius;

                vecdif(tmp_1,dir_1,2,tmp_3);
                vecsum(tmp_1,dir_2,2,tmp_2);
                vecneg(tmp_2,2,tmp_2);
                if(!line_x_line2D( tri_23, tmp_2, tri_31, tmp_3, tol, point, &s, &t))
                {
                    return(FALSE);
                }
                results[1][X] = point[X];
                results[1][Y] = point[Y];
                results[1][Z] = radius;
            }
            num_results = 2;
            break;
    case    3:
        if(!incentre(tri_12,tri_23,tri_31,centre,&radius))
        {
            return(FALSE);
        }
        results[0][X] = centre[X];
        results[0][Y] = centre[Y];
        results[0][Z] = radius;

        for(i = 0; i < 3; i++)
        {
           switch (i) {
           case 0:
               vecdif(tri_12,centre,2,vec);    /* Calculate centre opposite tri_12 */
               break;
           case 1:
               vecdif(tri_23,centre,2,vec);    /* Calculate centre opposite tri_23 */
               break;
           case 2:
               vecdif(tri_31,centre,2,vec);    /* Calculate centre opposite tri_31 */
               break;
           }
           vecsca(2.0,vec,2,vec);
           vecsum(centre,vec,2,point);
           results[i+1][X] = point[X];
           results[i+1][Y] = point[Y];
           results[i+1][Z] = radius;
        }
        vecdif(tri_12,tri_31,2,dir_1); 
        vecdif(tri_23,tri_12,2,dir_2);
        vecdif(tri_31,tri_23,2,dir_3);
             
        for(i=0; i < 3; i++)
        {
            switch (i) {
            case 0:
                vecnrm(dir_1,2,tol,tmp_1);      /* Circle diametrically opposite tri_31 */
                vecnrm(dir_2,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_3);

                vecneg(dir_2,2,tmp_1);
                vecneg(dir_3,2,tmp_2);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(tmp_2,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_2);

                veccpy(tri_12,3,tmp_4);
                veccpy(tri_23,3,tmp_5);
                veccpy(dir_1,3,tmp_6);
                break;
   
            case 1:
                vecnrm(dir_2,2,tol,tmp_1);      /* Circle diametrically opposite tri_12 */
                vecnrm(dir_3,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_3);

                vecneg(dir_3,2,tmp_1);
                vecneg(dir_1,2,tmp_2);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(tmp_2,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_2);

                veccpy(tri_23,3,tmp_4);
                veccpy(tri_31,3,tmp_5);
                veccpy(dir_2,3,tmp_6);
                break;
   
            case 2:
                vecnrm(dir_3,2,tol,tmp_1);      /* Circle diametrically opposite tri_23 */
                vecnrm(dir_1,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_3);

                vecneg(dir_1,2,tmp_1);
                vecneg(dir_2,2,tmp_2);
                vecnrm(tmp_1,2,tol,tmp_1);
                vecnrm(tmp_2,2,tol,tmp_2);
                vecsum(tmp_1,tmp_2,2,tmp_2);

                veccpy(tri_31,3,tmp_4);
                veccpy(tri_12,3,tmp_5);
                veccpy(dir_3,3,tmp_6);
                break;

            default:
                return(FALSE);  
            }

            if(!line_x_line2D( tmp_4, tmp_3, tmp_5, tmp_2, tol, point, &s, &t))
            {
                 return(FALSE);
            }
            if(!axperpl(point,tmp_4,tmp_6,&radius))
            {
                return(FALSE);
            }
            results[i+4][X] = point[X];
            results[i+4][Y] = point[Y];
            results[i+4][Z] = radius;
        }
        num_results = 7;
        break;

    default:
            return(FALSE);      /* Impossible condition */
    }
              
    *onum_results = 0;
    for (i = 0; i < num_results; i++)
    {   
        for (tangent = TRUE,j = 0; j < 3 && tangent; j++)
        {
           circ_x_lin(results[i],results[i][2],p_on_lines[j],
                   dir_of_lines[j],tol,temp,&num);
           if (num != 1)
               tangent = FALSE;              
        }           
        if (tangent)
        {   /* Copy in as a valid solution */ 
            oresults[*onum_results][X] = results[i][X];
            oresults[*onum_results][Y] = results[i][Y];
            oresults[*onum_results][Z] = ABS(results[i][Z]);
            (*onum_results)++;
        }
    }

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 21st-September-1988
 *
 */
BOOLEAN
circ_x_circ( centre1, radius1, centre2, radius2, tol, results, num_results )
                        /* Description   :- This function calculates the intersection 
                         *                  two circles.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- There may be zero, one or two solutions
                         *                  to this problem depending on the 
                         *                  relationship of the circles to each other.
                         *
                         *                   See pg 27 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for a description of
                         *                   the mathematics.
                         *
                         */
double  centre1[2];     /* <i> The centre of the first circle    */
double  radius1;        /* <i> The radius of the first circle    */
double  centre2[2];     /* <i> The centre of the second circle   */
double  radius2;        /* <i> The radius of the second circle   */
double  tol;            /* <i> The accuracy tolerance            */
double  results[2][2];  /* <o> The intersection points, if any   */
INT     *num_results;   /* <o> The number of intersection points */
{
    double  rksq;
    double  rlsq;
    double  xlk;
    double  ylk;
    double  distsq;
    double  delrsq;
    double  sumrsq;
    double  root;
    double  dstinv;
    double  scl;
    double  x;
    double  y;
    double  xfac;
    double  yfac;                         
    double  n1,n2;
    int32_t    same;

    *num_results = 0;
    if( radius1 < tol || radius2 < tol )
    {                               /* Positive radii only */
        return(FALSE);
    }

    rksq = radius1*radius1;
    rlsq = radius2*radius2;
    xlk = centre2[X] - centre1[X];
    ylk = centre2[Y] - centre1[Y];

    distsq = xlk*xlk + ylk*ylk;
    if( distsq < tol )
    {                               /* Circles have same centre */
        return(FALSE);
    }
    delrsq = rlsq - rksq;
    sumrsq = rksq + rlsq;
    root   = 2.0*sumrsq*distsq-distsq*distsq-delrsq*delrsq;
/* Try this fix */
    n1 = radius1 + radius2;
    n2 = sqrt(distsq);
                            
    /* Fortran call 0 = FALSE , -1 = TRUE */ 
    same = DSAME(&n1,&n2);  
    if ((root < -tol) && (same == 0))
    {                               /* The circles do not intersect */
        return(TRUE);
    }
    dstinv = 0.5/distsq;
    scl = 0.5 - delrsq*dstinv;
    x   = xlk*scl + centre1[X];
    y   = ylk*scl + centre1[Y];
    if( root < tol )
    {                               /* Circles touch at a single point */
        results[0][X] = x;
        results[0][Y] = y;
        *num_results = 1;
    }
    else
    {
        root = dstinv*sqrt(root);
        xfac = xlk*root;
        yfac = ylk*root;
        results[0][X] = x - yfac;
        results[0][Y] = y + xfac;
        results[1][X] = x + yfac;
        results[1][Y] = y - xfac;    
/* And also this fix */
        if ((DSAME(&results[0][X],&results[1][X]) == -1)
            && (DSAME(&results[0][Y],&results[1][Y]) == -1))
            *num_results = 1;
        else
            *num_results = 2;
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 22nd-September-1988
 *
 */
BOOLEAN
circ_x_lin(centre,radius,point,dir,tol,results,num_results  )
                        /* Description   :- This function calculates the intersection
                         *                  points of a circle and a straight line.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- There may be zero, one or two solutions
                         *                  to this problem depending on the 
                         *                  relationship of the circle to the line.
                         *
                         *                  See pg 25 of "A Programmer's Geometry" by
                         *                  Bowyer and Woodwark for a description of
                         *                  the mathematics.
                         */
double  centre[2];      /* <i> The centre of the circle           */
double  radius;         /* <i> The radius of the circle           */
double  point[2];       /* <i> A point on the straight line       */
double  dir[2];         /* <i> The direction of the straight line */
double  tol;            /* <i> The accuracy tolerance             */
double  results[2][2];  /* <o> The intersection points            */
INT     *num_results;   /* <o> The number of points found         */
{   
    double  fsq;
    double  gsq;
    double  fgsq;
    double  xjo;
    double  yjo;
    double  fygx;
    double  root;
    double  fxgy;
    double  t;
    double  t1;
    double  t2;
    double  fginv;
    double  dist;

    *num_results = 0;
    if( radius < tol )
    {                               /* Positive radius only */
        return(FALSE);
    }
    if( veclen(dir,2) < tol )
    {                               /* Lines have no direction */
        return(FALSE);
    }

    fsq = dir[X]*dir[X];
    gsq = dir[Y]*dir[Y];

    fgsq = fsq + gsq;
    if( fgsq < tol )
    {                               /* Line coefficients are corrupt */
        return(FALSE);
    }
    xjo = centre[X] - point[X];
    yjo = centre[Y] - point[Y];
    fygx = dir[X]*yjo - dir[Y]*xjo;
    root = radius*radius*fgsq - fygx*fygx;
                                 
    if (!axperpl2D(centre,point,dir,&dist))
        return(TRUE);
                      
    if ((root < -tol) && (DSAME(&dist,&radius) == 0))
    {                               /* Line does not intersect circle */
        return(TRUE);
    }
    fxgy = dir[X]*xjo + dir[Y]*yjo;
    if(root < tol )
    {                               /* Line is tangential */
        t = fxgy/fgsq;
        results[0][X] = point[X] + t*dir[X];
        results[0][Y] = point[Y] + t*dir[Y];
        *num_results = 1;
    }
    else
    {
        root = sqrt(root);
        fginv = 1.0/fgsq;
        t1 = (fxgy - root)*fginv;
        t2 = (fxgy + root)*fginv;
        results[0][X] = point[X] + t1*dir[X];
        results[0][Y] = point[Y] + t1*dir[Y];
        results[1][X] = point[X] + t2*dir[X];
        results[1][Y] = point[Y] + t2*dir[Y];
        if ((DSAME(&results[0][X],&results[1][X]) == -1)
            && (DSAME(&results[0][Y],&results[1][Y]) == -1))
            *num_results = 1;
        else
            *num_results = 2;
    }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 7th-September-1988
 *
 */
BOOLEAN
circumcentre(k,l,m,tol,centre,radius)
                        /* Description   :- This function calculates the circumcentre of
                         *                  a triangle.       
                         *
                         *                  That is, given three points that are not
                         *                  co-linear, a circle is generated that passes
                         *                  through all three.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 65 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for a description of
                         *                   the mathematics.
                         */
double  k[2];           /* <i> A vertex of the triangle   */
double  l[2];           /* <i> A vertex of the triangle   */
double  m[2];           /* <i> A vertex of the triangle   */
double  tol;            /* <i> Accuracy tolerance         */
double  centre[2];      /* <o> The incentre of the circle */
double  *radius;        /* <o> The radius of the circle   */
{
    double  vec_kl[2];
    double  vec_km[2];
    double  local_centre[2];
    double  len_kl_sq;
    double  len_km_sq;
    double  det;
    double  detinv;

    vecdif(l,k,2,vec_kl);
    vecdif(m,k,2,vec_km);

    det = vec_kl[X] * vec_km[Y] - vec_km[X] * vec_kl[Y];
    if( fabs(det) < tol )
    {                               /* At least two of the points are coincident */
        return(FALSE);
    }
    detinv = 0.5/det;
    len_kl_sq = veclensq(vec_kl,2);
    len_km_sq = veclensq(vec_km,2);

    local_centre[X] = detinv * (len_kl_sq*vec_km[Y] - len_km_sq*vec_kl[Y]);
    local_centre[Y] = detinv * (len_km_sq*vec_kl[X] - len_kl_sq*vec_km[X]);

    *radius = sqrt( veclensq( local_centre, 2 ));
                                     
    vecsum(k,local_centre,2,centre);
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 20th-September-1988
 *
 */
void
implicit_to_para(implicit,p_on_line,direction )
                        /* Description   :- This function converts the implicit form
                         *                  of a line equation to the parametric form.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The implicit equation is of the form:
                         *
                         *                  ax + by + c = 0.0
                         *
                         *                  This routine is used internally and is not
                         *                  of use to the user.
                         *
                         *                   See pg 10 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for a description of
                         *                   the mathematics.
                         */
double implicit[3];     /* <i> The coefficients of the implicit form */
double p_on_line[2];    /* <o> A point on the line                   */
double direction[2];    /* <o> The direction of the line             */
{
      double    root;
      double    factor;

      root = 1.0/(implicit[0]*implicit[0] + implicit[1]*implicit[1]);
      factor = -implicit[2]*root;
      p_on_line[X] = implicit[0]*factor;
      p_on_line[Y] = implicit[1]*factor;
      root = sqrt(root);
      direction[X] = implicit[1]*root;
      direction[Y] = -implicit[0]*root;
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 7th-September-1988
 *
 */
BOOLEAN
incentre(k,l,m,centre,radius)
                        /* Description   :- This function calculates the incentre of
                         *                  a triangle.
                         *
                         *                  That is, given three points forming a
                         *                  triangle, a circle is created that is
                         *                  tangent to all the edges.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  See pg 64 of "A Programmer's Geometry" by
                         *                   Bowyer and Woodwark for a description of
                         *                   the mathematics.
                         *
                         */
double  k[2];           /* <i> A vertex of the triangle   */
double  l[2];           /* <i> A vertex of the triangle   */
double  m[2];           /* <i> A vertex of the triangle   */
double  centre[2];      /* <o> The incentre of the circle */
double  *radius;        /* <o> The radius of the circle   */
{
    double  vec_kl[2];
    double  vec_lm[2];
    double  vec_mk[2];
    double  len_kl;
    double  len_lm;
    double  len_mk;
    double  perimeter;

    vecdif(l,k,2,vec_kl);
    len_kl = veclen(vec_kl,2);
    vecdif(m,l,2,vec_lm);
    len_lm = veclen(vec_lm,2);
    vecdif(k,m,2,vec_mk);
    len_mk = veclen(vec_mk,2);

    perimeter = len_kl + len_lm + len_mk;

    centre[X] = ( len_lm * k[X] + len_mk * l[X] + len_kl * m[X])/perimeter;
    centre[Y] = ( len_lm * k[Y] + len_mk * l[Y] + len_kl * m[Y])/perimeter;
                    
    perimeter = perimeter/2.0;
    *radius = sqrt(((perimeter-len_lm)*(perimeter-len_mk)*(perimeter-len_kl))/perimeter);

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                        Author :: Jim Mulhearn
 *                                                        Date   :: 20th-September-1988
 *
 */
void
para_to_implicit( p_on_line, direction, implicit  )
                        /* Description   :- This function converts the parametric form
                         *                  of a line equation to the implicit form.
                         *
                         * Return status :- NOTHING
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The implicit equation is of the form:
                         *
                         *                  ax + by + c = 0.0
                         *
                         *                  This routine is used internally and is not
                         *                  of use to the user.
                         *
                         *                  See pg 11 of "A Programmer's Geometry" by
                         *                  Bowyer and Woodwark for a description of
                         *                  the mathematics.
                         */
double p_on_line[2];    /* <i> A point on the line                   */
double direction[2];    /* <i> The direction of the line             */
double implicit[3];     /* <o> The coefficients of the implicit form */
{
      implicit[0] = -direction[Y];
      implicit[1] = direction[X];
      implicit[2] = p_on_line[X]*direction[Y] - p_on_line[Y]*direction[X];
}

/*      @(#)  256.1 date 12/16/89 lines.c  */
/*+
 *
 *
 *        Filename    : lines.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:08
 *        Last change : 89/12/16 19:23:59
 *
 *        Copyright : Practical Technology Limited  
 *        File :- lines.c
 *
 *        This file contains the following routines:
 * 
 *        arc_x_arc2D
 *        circle_x_circle2D
 *        line_x_arc2D
 *        line_x_arc2DF
 *        line_x_line2D
 *        line_x_line2DF
 *        line_x_circle2D  
 *        local_origin
 *        perp_vector
 *        point_quadrant
 *        point_in_quadrant
 *        point_on_arc
 *        point_on_line
 *        point_xydistance
 *        line_x_line3D
 *        line_x_line3DF
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 6 May 1988
 *
 */
BOOLEAN                                                                  
arc_x_arc2D(a1_start,a1_end,arc1_centre,arc1_radius,arc1_sense,a2_start,a2_end,
                 arc2_centre,arc2_radius,arc2_sense,tol,i1,i2,num)
                        /* Description   :- This function intersects two arcs and returns
                         *                  the intersections and their points.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */          
POINT_2D   a1_start;    /* <i> Start point of first arc */
POINT_2D   a1_end;      /* <i> End point of first arc */
POINT_2D   arc1_centre; /* <i> Centre of first arc */
double     arc1_radius; /* <i> Radius of first arc */ 
BOOLEAN    arc1_sense;  /* <i> The sense of the arc TRUE => anitclockwise */
POINT_2D   a2_start;    /* <i> Start point of second arc */
POINT_2D   a2_end;      /* <i> End point of second arc */
POINT_2D   arc2_centre; /* <i> Centre of second arc */
double     arc2_radius; /* <i> Radius of second arc */ 
BOOLEAN    arc2_sense;  /* <i> The sense of the arc TRUE => anticlockwise */
double     tol;         /* <i> Tolerance to be used in calculation */
POINT_2D   i1;          /* <o> First intersection point */
POINT_2D   i2;          /* <o> Second intersection point */
int32_t       *num;        /* <o> Number of intersections */
{ 
    POINT_2D    arc1_start,arc1_end,arc2_start,arc2_end;
    double      quad_a1i1,quad_a1i2,quad_a2i1,quad_a2i2;
    double      arc1_pstart,arc1_pend,arc2_pstart,arc2_pend; 
    int32_t        i;
                                    
    *num = 0;           /* Set number of intersections to zero */

    if (!circle_x_circle2D(arc1_centre,arc1_radius,arc2_centre,arc2_radius,tol,i1,i2,&i))
        return(FALSE);      /* No intersections */   

    if (arc1_sense)     /* Arc defined anti-clockwise */
    {                   /* Yes ... */
        veccpy(a1_start,2,arc1_start);
        veccpy(a1_end,2,arc1_end);
    }
    else
    {                   /* No ... So swap ends */
        veccpy(a1_start,2,arc1_end);    /* Swap points so arc is anti-clockwise */
        veccpy(a1_end,2,arc1_start);
    } 

    if (arc2_sense)     /* Arc defined anti-clockwise */
    {                   /* Yes ... */
        veccpy(a2_start,2,arc2_start);
        veccpy(a2_end,2,arc2_end);
    }
    else
    {                   /* No ... So swap ends */
        veccpy(a2_start,2,arc2_end);    /* Swap points so arc is anti-clockwise */
        veccpy(a2_end,2,arc2_start);
    } 

    point_quadrant(arc1_centre,i1,tol,&quad_a1i1);
    point_quadrant(arc1_centre,i2,tol,&quad_a1i2);
    point_quadrant(arc1_centre,arc1_start,tol,&arc1_pstart);
    point_quadrant(arc1_centre,arc1_end,tol,&arc1_pend);

    point_quadrant(arc2_centre,i1,tol,&quad_a2i1);
    point_quadrant(arc2_centre,i2,tol,&quad_a2i2);
    point_quadrant(arc2_centre,arc2_start,tol,&arc2_pstart);
    point_quadrant(arc2_centre,arc2_end,tol,&arc2_pend);

    if ((point_on_arc(arc1_pstart,arc1_pend,quad_a1i1))
        && (point_on_arc(arc2_pstart,arc2_pend,quad_a2i1)))
    {
        (*num)++;   /* First intersection lies between arc boundaries */
    }                                                   

    if ((point_on_arc(arc1_pstart,arc1_pend,quad_a1i2))
        && (point_on_arc(arc2_pstart,arc2_pend,quad_a2i2)))
    {
        if (*num == 0)
            veccpy(i2,CART,i1);
    
        (*num)++;
    } 
    if (*num == 0)
        return(FALSE);
    else
        return( TRUE );
}                  
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 3 May 1988
 *
 */
BOOLEAN 
circle_x_circle2D(k,rk,l,rl,tol,p1,p2,num)
                        /* Description   :- This function calculates the points of
                         *                  intersection if any of two circles
                         *
                         * Return status :- TRUE if intersection
                         *                  FALSE if no intersection
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */                                                   
POINT_2D    k;          /* <i> The centre of one first circle */
double      rk;         /* <i> The radius of the first circle */
POINT_2D    l;          /* <i> The centre of the second circle */
double      rl;         /* <i> The radius of the second circle */ 
double      tol;        /* <o> The tolerance to use in the calculation */
POINT_2D    p1;         /* <o> The first point of intersection */
POINT_2D    p2;         /* <o> The second point of intersection */
int32_t        *num;       /* <o> Number of intersections */
{
    double      rksq;
    double      rlsq;
    double      xlk;
    double      ylk;
    double      distsq,delrsq,sumrsq,root,dstinv,x,y;
    double      scl,xfac,yfac;                           

    *num = 0;

    rksq = rk * rk;
    rlsq = rl * rl;
    xlk = l[X] - k[X];
    ylk = l[Y] - k[Y];

    distsq = xlk * xlk + ylk * ylk;
    if (distsq < tol)
          return(FALSE);    /* Two circles have the same centre */
    
    delrsq = rlsq - rksq;
    sumrsq = rksq + rlsq;
    root = 2.0 * sumrsq * distsq - distsq * distsq - delrsq * delrsq;

    if (root < -tol)
        return(FALSE);      /* The circles do not intersect */

    dstinv = 0.5 / distsq;
    scl = 0.5 - delrsq * dstinv;
    x = xlk * scl + k[X];
    y = ylk * scl + k[Y];
    if (root < tol)
    {
        p1[X] = x;
        p1[Y] = y;
        *num = 1;
        return(TRUE);       /* Circle just touch at x,y */
    }
    else
    {
        root = dstinv * sqrt(root);
        xfac = xlk * root;
        yfac = ylk * root;
        p1[X] = x - yfac;
        p1[Y] = y + xfac;
        p2[X] = x + yfac;
        p2[Y] = y - xfac;
        *num = 2;
    }

    return(TRUE);
} 
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 6 May 1988
 *
 */
BOOLEAN
line_x_arc2D(lstart,ldir,a_start,a_end,acentre,asense,radius,tol,i1,i2,num)
                        /* Description   :- This function tries to intersect a 2D 
                         *                  unbounded line with an arc
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  If num is one then i1 will contain the
                         *                   intersection. 
                         *
                         */
POINT_2D   lstart;      /* <i> The start of the line */
POINT_2D   ldir;        /* <i> The direction of the line */
POINT_2D   a_start;     /* <i> Start point of the arc */
POINT_2D   a_end;       /* <i> End point of the arc */
POINT_2D   acentre;     /* <i> Centre of the circle that the arc is part of */
BOOLEAN    asense;      /* <i> The sense TRUE => Anticlockwise, FALSE => Clockwise */
double     radius;      /* <i> Radius of arc */  
double     tol;         /* <i> The tolerance to be used in the calculation */                     
POINT_2D   i1;          /* <o> First intersection point */
POINT_2D   i2;          /* <o> Second intersection point */
int        *num;        /* <o> Number of intersections */
{ 
    double     quad_i1;    /* Quadrant intersection 1 is in */
    double     quad_i2;    /* Quadrant intersection 2 is in */
    double     arc_start;  /* Quadrant arc start is in */
    double     arc_end;    /* Quadrant arc end is in   */ 
    POINT_2D   astart;     /* Internal copy of arc start point to allow us to swap  */
    POINT_2D   aend;       /* the ends of a clockwise arc to make it look anticlockwise */

    *num = 0;           /* Set number of intersections to zero */

    if (!line_x_circle2D(lstart,ldir,acentre,radius,tol,i1,i2))
    {
          return(FALSE);  /* No intersection */
    }
                     
    if (asense)         /* Is arc defined anti clockwise */
    {                   /* Yes .... */
        veccpy(a_start,2,astart);
        veccpy(a_end,2,aend);
    }
    else
    {                   /* No .... So swap end point to make it */
        veccpy(a_start,2,aend);
        veccpy(a_end,2,astart);
    }

    /* Classify end points of arc and intersections into there respective quadrants */
    point_quadrant(acentre,i1,tol,&quad_i1);
    point_quadrant(acentre,i2,tol,&quad_i2);
    point_quadrant(acentre,astart,tol,&arc_start);
    point_quadrant(acentre,aend,tol,&arc_end);
  
    if (point_on_arc(arc_start,arc_end,quad_i1))
    {
        (*num)++;      /* Increment number of intersections */ 
    }

    if (point_on_arc(arc_start,arc_end,quad_i2))
    {
        if (*num == 0)  /* Have we an intersection already */
        {               /* No ... */
            veccpy(i2,2,i1);    /* Copy i2 into i1 as i1 will always have an intersection */
        }
        (*num)++;   /* Increment number of intersections */
    }
            
    if (*num == 0)          /* Any intersections */
        return(FALSE);      /* No .... */
    else
        return(TRUE);       /* Yes ... */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 6 May 1988
 *
 */
BOOLEAN
line_x_arc2DF(lstart,lend,astart,aend,acentre,asense,radius,tol,i1,i2,num)
                        /* Description   :- This function tries to intersect a 2D 
                         *                  unbounded line with an arc
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  If num is one then i1 will contain the
                         *                   intersection. 
                         *
                         */
POINT_2D   lstart;      /* <i> The start of the line */
POINT_2D   lend;        /* <i> The end of the line */
POINT_2D   astart;      /* <i> Start point of the arc */
POINT_2D   aend;        /* <i> End point of the arc */
POINT_2D   acentre;     /* <i> Centre of the circle that the arc is part of */
BOOLEAN    asense;      /* <i> The sense TRUE => Anticlockwise, FALSE => Clockwise */
double     radius;      /* <i> Radius of arc */  
double     tol;         /* <i> The tolerance to be used in the calculation */                     
POINT_2D   i1;          /* <o> First intersection point */
POINT_2D   i2;          /* <o> Second intersection point */
int        *num;        /* <o> Number of intersections */
{   
    VECTOR   diff;
    VECTOR   i1_diff;
    VECTOR   i2_diff;
    double   param1;
    double   param2; 
    double   len;
    double   len1;
    double   len2;

    vecdif(lend,lstart,2,diff);     /* Subtract lend - lstart */

    if (!line_x_arc2D(lstart,diff,astart,aend,acentre,asense,radius,tol,i1,i2,num))
        return(FALSE);          /* No intersections */

    len = veclen(diff,2);           /* Get length of line */
                                                                     
    vecdif(i1,lstart,2,i1_diff);    /* Subtract i1 - lstart */
 
    if (vecdir(i1_diff,2,diff))     /* Are vectors going in the same direction */
    {                               /* Yes ... So calculate parameter value */
        len1 = veclen(i1_diff,CART);
        param1 = len1 / len;
    }
    else
        param1 = 2.0;               /* No ... So force outside line space */
        
    if (*num == 2)
    {
        vecdif(i2,lstart,2,i2_diff);    /* Subtract i2 - lstart */
        if (vecdir(i2_diff,2,diff))     /* Are vectors going in same direction */ 
        {                               /* Yes ... So calculate parameter value */
            len2 = veclen(i2_diff,2);
            param2 = len2 / len;
        }
        else
            param2 = 2.0;
    }
    else
        param2 = 2.0;                   /* Force outside line space */
        
    *num = 0.0;                         /* Set number of intersections to zero */

    if ((param1 >= 0.0) && (param1 <= 1.0))   /* Is first intersection between line */
    {                                                                                 
        (*num)++;
    }

    if ((param2 >= 0.0) && (param2 <= 1.0))   /* Is second intersection between line */
    {
        if (*num == 0)          /* Any intersections so far */   
            veccpy(i2,2,i1);    /* No .... so move i2 into i1 */

        (*num)++;
    }

    if (*num == 0)
        return(FALSE);
    else
        return(TRUE);
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 29 April 1988
 *
 */
BOOLEAN 
line_x_line2D(point1,dir1,point2,dir2,tol,ipoint,s,t)
                        /* Description   :- This function calculates the intersection
                         *                  if any of the two 2D unbounded lines and 
                         *                  returns the intersection point with
                         *                  parameters.
                         *
                         * Return status :- TRUE if intersection
                         *                  FALSE if no intersection
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The lines are unbounded lines which are
                         *                   defined as a point and a direction
                         *
                         */ 
POINT_2D       point1;  /* <i> Point on the first line */
VECTOR_2D      dir1;    /* <i> Direction of first line */
POINT_2D       point2;  /* <i> Point on the second line */
VECTOR_2D      dir2;    /* <i> Direction of second line */ 
double         tol;     /* <i> The tolerance */
POINT_2D       ipoint;  /* <o> Intersection point  */
double         *s;      /* <o> Parameter s on first line */
double         *t;      /* <o> Parameter t on second line */
{                 
    double    dir12;
    double    dir21;
    double    det;
    double    detinv;
    double    xdiff;
    double    ydiff;

    dir12 = dir1[X] * dir2[Y];
    dir21 = dir2[X] * dir1[Y];  
    det = dir21 - dir12;

    if (ABS(det) < tol)
        return(FALSE);      /* Lines are parallel or within the tol */
    else
    {
        ydiff = point2[Y] - point1[Y];
        xdiff = point2[X] - point1[X];
        detinv = 1.0/det;
        *s = (dir2[X] * ydiff - dir2[Y] * xdiff) * detinv;  /* Calculate the parm vals */
        *t = (dir1[X] * ydiff - dir1[Y] * xdiff) * detinv;
        ipoint[X] = point1[X] + dir1[X] * (*s);        /* Calculate the point of */
        ipoint[Y] = point1[Y] + dir1[Y] * (*s);        /* intersection           */
    }
    return( TRUE );
}                     
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 3 May 1988
 *
 */
BOOLEAN
line_x_line2DF(p1,p2,p3,p4,tol,intersection,s,t,parallel)
                        /* Description   :- This function calculates the intersection
                         *                  of two 2D finite lines if they cross.
                         *
                         * Return status :- TRUE if lines intersect
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-
                         *
                         */                                        
POINT_2D     p1;        /* <i> Start of line one */
POINT_2D     p2;        /* <i> End of line one */
POINT_2D     p3;        /* <i> Start of line two */
POINT_2D     p4;        /* <i> End of line two */   
double       tol;       /* <i> The tolerance to be used */
POINT_2D     intersection; /* <o> The intersection point */
double       *s;        /* <o> Parametric value of the intersection on p1p2 */
double       *t;        /* <o> Parametric value of the intersection on p3p4 */
int32_t         *parallel; /* <o> Whether lines are parallel or not */
{
    double     x_21,y_21,x_43,y_43,x_31,y_31;
    double     det,fminx,fmaxx,fminy,fmaxy,smaxx,sminx,smaxy,sminy;
                                                                   
    *parallel = FALSE;           /* Say lines are not parallel */

    if (p1[X] < p2[X])
    {
        fminx = p1[X];
        fmaxx = p2[X];
    }
    else
    {
        fminx = p2[X];
        fmaxx = p1[X];  
    }

    if (p3[X] < p4[X])
    {
        sminx = p3[X];
        smaxx = p4[X];
    }
    else
    {
        sminx = p4[X];
        smaxx = p3[X];
    }

    if (((fminx - tol) > smaxx) || ((sminx - tol) > fmaxx))
        return(FALSE);

    if (p1[Y] < p2[Y])
    {
        fminy = p1[Y];
        fmaxy = p2[Y];
    }
    else
    {
        fminy = p2[Y];
        fmaxy = p1[Y];  
    }

    if (p3[Y] < p4[Y])
    {
        sminy = p3[Y];
        smaxy = p4[Y];
    }
    else
    {
        sminy = p4[Y];
        smaxy = p3[Y];
    }        

    if (((fminy - tol) > smaxy) || ((sminy - tol) > fmaxy))
        return(FALSE);

    x_21 = p2[X] - p1[X];
    y_21 = p2[Y] - p1[Y];
    x_43 = p4[X] - p3[X];
    y_43 = p4[Y] - p3[Y];

    det = x_43 * y_21 - y_43 * x_21;

    if ((det >= tol) || (det <= -tol))
    {
        x_31 = p3[X] - p1[X];
        y_31 = p3[Y] - p1[Y];
        *s = (x_43 * y_31 - y_43 * x_31) / det;
        *t = (x_21 * y_31 - y_21 * x_31) / det;

        if ((*t >= -tol) && (*t <= (1.0 + tol)))
        {                                       
            if ((*s >= -tol) && (*s <= (1.0 + tol)))
            {
                if ((*t > -tol) && (*t < tol))
                    *t = 0.0;
                else if ((*t > (1.0 - tol)) && (*t < (1.0 + tol)))
                    *t = 1.0;

                if ((*s >= tol) && (*s <= (1.0 - tol)))
                {
                    intersection[X] = p1[X] + x_21 * *s;
                    intersection[Y] = p1[Y] + y_21 * *s;
                }
                else if ((*s >= -tol) && (*s < tol))
                {                                   
                    intersection[X] = p1[X];
                    intersection[Y] = p1[Y];
                    *s = 0.0;
                }
                else
                {
                    intersection[X] = p2[X];
                    intersection[Y] = p2[Y];
                    *s = 1.0;
                } 
                return(TRUE);
            }
        }
    }
    return(FALSE);           
}


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 3 May 1988
 *
 */
BOOLEAN 
line_x_circle2D(lpoint,ldir,cpoint,crad,tol,x1,x2)
                        /* Description   :- This function calculates the intersection
                         *                  between an unbounded line and a circle.
                         *
                         * Return status :- TRUE if intersection
                         *                  FALSE if no intersection
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  The lines are unbounded lines which are
                         *                   defined as a point and a direction
                         */
POINT_2D    lpoint;     /* <i> The point which the line passed through */
VECTOR_2D   ldir;       /* <i> The direction of the line */
POINT_2D    cpoint;     /* <i> Centre point of the circle */
double      crad;       /* <i> Radius of the circle */
double      tol;        /* <i> The tolerance to use in the calculation */
POINT_2D    x1;         /* <o> The first intersection point */
POINT_2D    x2;         /* <o> The second intersection point */
{  
    double   f,g,x0,y0,rj,xj,yj;
    double   fsq,gsq,fgsq,fygx,fginv;
    double   xj0,yj0,root,fxgy,t1,t2;

    f = ldir[X];
    g = ldir[Y];
    x0 = lpoint[X];
    y0 = lpoint[Y];

    rj = crad;
    xj = cpoint[X];
    yj = cpoint[Y];

    fsq = f * f;
    gsq = g * g;
    fgsq = fsq + gsq;

    if (ABS(fgsq) < tol)
        return(FALSE);

    xj0 = xj - x0;
    yj0 = yj - y0;
    fygx = f * yj0 - g * xj0;
    root = rj * rj * fgsq - fygx * fygx;
    if (root < -tol)
        return(FALSE);      /* Line does not intersect circle */
    
    fxgy = f * xj0 + g * yj0;
    if (ABS(root) < tol)
    {              /* Line is tangential - one intersection */
        t1 = fxgy / fgsq;
        x1[X] = x0 + f * t1;
        x1[Y] = y0 + g * t1;    
        x2[X] = x1[X];
        x2[Y] = x1[Y];
    }
    else
    {           /* Line is not tangential - two intersections */
        root = sqrt(root);
        fginv = 1.0/fgsq;
        t1 = (fxgy - root) * fginv;
        t2 = (fxgy + root) * fginv;
        x1[X] = x0 + f * t1;
        x1[Y] = y0 + g * t1;
        x2[X] = x0 + f * t2;
        x2[Y] = y0 + g * t2;
    }

    return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 10 June 1988
 *
 */
void
local_origin(p1,p2,p3,x_axis,y_axis,z_axis)
                        /* Description   :- This function generates a local origin
                         *                  and set of 3D axes.
                         *
                         * Return status :- Nothing
                         *   
                         * Externals     :- Used/Modified
                         *
                         * Notes         :- The x-axis passes through p1 and p2.
                         *                  The z-axis is the cross of the x-axis and
                         *                  p3-p2.
                         *                  The y-axis is the cross of the x-axis
                         *                  and the z-axis
                         *
                         */                                       
POINT       p1;         /* <i> The first point */
POINT       p2;         /* <i> The second point */
POINT       p3;         /* <i> The third point */
VECTOR      x_axis;     /* <o> Vector representing the x-axis */
VECTOR      y_axis;     /* <o> Vector representing the y-axis */
VECTOR      z_axis;     /* <o> Vector representing the z-axis */
{                                                              
    VECTOR   a;
    VECTOR   b;

    vecdif(p1,p2,CART,x_axis);         /* The x-axis           */
    vecnrm(x_axis,CART,TOL,x_axis);        /* Normalise the x_axis */                    
                    
    vecdif(p3,p2,CART,a);              /* Get vector from p3 to p2 */
    vecdif(p1,p2,CART,b);              /* Get vector from p2 to p1 */
    veccp(b,a,z_axis);              /* The z_axis               */
    vecnrm(z_axis,CART,TOL,z_axis);        /* Normalise it             */

    veccp(z_axis,x_axis,y_axis);    /* The y-axis    */
    vecnrm(y_axis,CART,TOL,y_axis);        /* Normalise it  */

}                    
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Jun 1988
 *
 */
BOOLEAN                                                                        
perp_vector(invec,outvec)
                        /* Description   :- This function takes a vector and outputs
                         *                  the vector that is perpendicular to it.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE if null vector is specified
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                                  
VECTOR      invec;      /* <i> The vector to have its perpendicular calculated */
VECTOR      outvec;     /* <i> The perpendicular of the vector */
{
    if ((TOL_WITHIN(invec[X],TOL)) && (TOL_WITHIN(invec[Y],TOL)) && (TOL_WITHIN(invec[Z],TOL)))
          return(FALSE);        /* Zero length vector cannot compute */

    if ((TOL_WITHIN(invec[X],TOL)) && (TOL_WITHIN(invec[Y],TOL)))
    {   /* On the xy plane */
        outvec[X] = 1.0;
        outvec[Y] = 0.0;
        outvec[Z] = 0.0;
    }
    else if ((TOL_WITHIN(invec[Y],TOL)) && (TOL_WITHIN(invec[Z],TOL)))
    {   /* On the YZ plane */
        outvec[X] = 0.0;
        outvec[Y] = 1.0;
        outvec[Z] = 0.0;
    }
    else if ((TOL_WITHIN(invec[Z],TOL)) && (TOL_WITHIN(invec[X],TOL)))
    {   /* On the zx plane */
        outvec[X] = 0.0;
        outvec[Y] = 0.0;
        outvec[Z] = 1.0;
    }
    else if (!TOL_FAILURE(invec[X]))
    {
        outvec[X] = (invec[Y] * invec[Y] + invec[Z] * invec[Z]) / invec[X];
        outvec[Y] = -invec[Y];
        outvec[Z] = -invec[Z];
        vecnrm(outvec,CART,TOL,outvec);        /* Ensure vector is normalised */
    }
    else if (!TOL_FAILURE(invec[Y]))
    {
        outvec[X] = -invec[X];
        outvec[Y] = (invec[X] * invec[X] + invec[Z] * invec[Z]) / invec[Y];
        outvec[Z] = -invec[Z];
        vecnrm(outvec,CART,TOL,outvec);        /* Ensure vector is normalised */
    }
    else if (!TOL_FAILURE(invec[Z]))
    {
        outvec[X] = -invec[X];
        outvec[Y] = -invec[Y];
        outvec[Z] = (invec[X] * invec[X] + invec[Y] * invec[Y])/ invec[Z];
        vecnrm(outvec,CART,TOL,outvec);        /* Ensure vector is normalised */
    }
    else
        return(FALSE);

    return( TRUE );
}


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 5 May 1988
 *
 */
void
point_quadrant(origin,point,tol,quadrant)
                        /* Description   :- This function returns which quadrant the 
                         *                  point is in taking the origin specified as
                         *                  the actual origin.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  Values are as follows
                         *                             |
                         *                        2    |   1
                         *                             |
                         *                     --------+--------
                         *                             |
                         *                        3    |   4
                         *                             |
                         *
                         *
                         */
POINT_2D    origin;     /* <i>  The origin to use */
POINT_2D    point;      /* <i>  Point to be classified */
double      tol;        /* <i>  The tolerance to be used */
double      *quadrant;      /* <o>  The quadrant the point is in */
{       
    double     xdiff;
    double     ydiff;

    xdiff = point[X] - origin[X];
    ydiff = point[Y] - origin[Y];                                               

    point_in_quadrant(xdiff,ydiff,tol,quadrant);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 10 June 1988
 *
 */
BOOLEAN
point_in_quadrant(xdiff,ydiff,tol,quadrant)
                        /* Description   :- This function returns which quadrant the 
                         *                  point is in taking the origin specified as
                         *                  the actual origin.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *               
                         * Notes         :-  Values are as follows
                         *                             |
                         *                        2    |   1
                         *                             |
                         *                     --------+--------
                         *                             |
                         *                        3    |   4
                         *                             |
                         *
                         *
                         */
double      xdiff;      /* <i>  The x difference */
double      ydiff;      /* <i>  The y difference */
double      tol;        /* <i>  The tolerance to be used */
double      *quadrant;      /* <o>  The quadrant the point is in */
{
    double     tant;

    if ((xdiff > tol) && (ydiff >= 0.0))
    {                                              
        if ((xdiff > tol) && (!TOL_WITHIN(ydiff,tol)))
        {
            tant = ydiff/xdiff;
            *quadrant = (sqrt(1.0 + tant * tant) - 1.0)/tant;
        }
        else
            *quadrant = 0.0;
    }
    else if ((xdiff <= tol) && (ydiff > tol))
    {
        if ((ydiff > tol) && (!TOL_WITHIN(xdiff,tol)))
        {
            tant = -xdiff/ydiff;
            *quadrant = 1.0 + (sqrt(1.0 + tant * tant) - 1.0)/tant;
        }
        else
            *quadrant = 1.0;
    }
    else if ((xdiff < -tol) && (ydiff <= tol))
    {
        if ((xdiff < -tol) && (!TOL_WITHIN(ydiff,tol)))
        {
            tant = ydiff/xdiff;
            *quadrant = 2.0 + (sqrt(1.0 + tant * tant) - 1.0)/tant;
        }
        else
            *quadrant = 2.0;
    }
    else
    {
        if ((ydiff < 0.0) && (!TOL_WITHIN(xdiff,tol)))
        {
            tant = -xdiff/ydiff;
            *quadrant = 3.0 + (sqrt(1.0 + tant * tant) - 1.0)/tant;  
        }
            *quadrant = 3.0;
    }
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 10 May 1988
 *
 */                                                                            
BOOLEAN
point_on_arc(arc1,arc2,pnt)
                        /* Description   :- This function checsk to see if according to
                         *                  the parameter values if the point lies on
                         *                  the arc.
                         *
                         * Return status :- TRUE if point lies on the arc
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */                                              
double       arc1;      /* <i> Parameter value of endpoint of arc */
double       arc2;      /* <i> Parameter value of other endpoint of arc */
double       pnt;       /* <i> Parameter value of point to be checked */
{
    if (arc2 < arc1)
    {
        if ((pnt > arc1) || (pnt < arc2))
            return(TRUE);
        else
            return(FALSE);
    }
    else
    {
        if ((pnt > arc1) && (pnt < arc2))
            return(TRUE);
        else
            return(FALSE);
    }
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 24 Aug 1988
 *
 */
BOOLEAN                                                                            
point_on_line(lstart,point,lend)
                        /* Description   :- This function checks to see if the point
                         *                  lies within the end points of the line.
                         *                  The point is assumed to lie on the line
                         *                  already but where on it is not known.
                         *
                         * Return status :- TRUE if point lies within end points.
                         *                  FALSE if point lies outwith.
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
POINT     lstart;       /* <i>  The start of the line */
POINT     point;        /* <i>  The point to be checked */
POINT     lend;         /* <i>  The end of the line */
{
    INT       i;     
    BOOLEAN   within;
             
    within = TRUE;

    for (i = 0; ((i < CART) && (within)); i++)
    {
       if (((point[i] >= lstart[i]) && (point[i] <= lend[i]))
           || ((point[i] >= lend[i]) && (point[i] <= lstart[i])))
           within = TRUE;                          
        else
            within = FALSE;
    }

    return( within );
}


/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 10 June 1988
 *
 */
BOOLEAN                                                                         
point_xydistance(point,p1,p2,p3,x_len,y_len)
                        /* Description   :- This function returns the length of the 
                         *                  point along the x-axis and the y-axis.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-  The x-axis is defined as p2 - p1
                         *
                         */
POINT        point;     /* <i> The point to have its lengths returned */
POINT        p1;        /* <i> First point */
POINT        p2;        /* <i> Second point */
POINT        p3;        /* <i> Third point */
double       *x_len;    /* <i> The length of the point along the x-axis */
double       *y_len;    /* <i> The length of the point along the y-axis */
{   
    VECTOR      x_axis;
    VECTOR      y_axis;
    VECTOR      z_axis;
    VECTOR      dist;
    POINT       plane;


    /* Calculate the plane through p1,p2,p3 and check that the point lies on it */
    if (!pln3p(p1,p2,p3,TOL,plane))
        return(FALSE);          /* Could not calculate plane */
    
    plnperp(plane,point,dist);      /* Calculate distance vector of point from plane */
    if (!TOL_FAILURE(veclen(dist,CART)))
        return(FALSE);          /* point is not within tolernace on the plane */

    /* Calculate the x-axis and y-axis and z-axis */
    local_origin(p1,p2,p3,x_axis,y_axis,z_axis);
    
    /* Now calculate the length of the point along the x and y axes */
    *x_len = vecdp(point,x_axis,CART);
    *y_len = vecdp(point,y_axis,CART);

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th July 1988
 *
 */
BOOLEAN 
line_x_line3D(point1,dir1,point2,dir2,tol,ipoint,s,t)
                        /* Description   :- This function calculates the intersection
                         *                  if any of the two 3D unbounded lines and 
                         *                  returns the intersection point with
                         *                  parameters.
                         *
                         * Return status :- TRUE if valid intersection
                         *                  FALSE otherwise
                         *   
                         * Externals     :- Used/Modified
                         *               
                         * Notes         :- The lines are unbounded lines which are
                         *                  defined as a point and a direction
                         *
                         *                  See Pg 18 of "A programmer's geometry" by
                         *                  Bowyer and Woodwark for a description of the
                         *                  mathematics.
                         *
                         */ 
POINT       point1;  /* <i> Point on the first line */
VECTOR      dir1;    /* <i> Direction of first line; from start point to end point */
POINT       point2;  /* <i> Point on the second line */
VECTOR      dir2;    /* <i> Direction of second line; from start point to end point */
double      tol;     /* <i> Tolerance within which two points will be considered equal */
POINT       ipoint;  /* <o> Intersection point  */
double      *s;      /* <o> Parameter s on first line */
double      *t;      /* <o> Parameter t on second line */
{                 
    VECTOR  vec1;
    double  denom_x;
    double  denom_y;
    double  denom_z;

    denom_x =   dir1[Z]*dir2[Y] - dir1[Y]*dir2[Z] ;     /* Assuming X constant */
    denom_y =   dir1[X]*dir2[Z] - dir1[Z]*dir2[X] ;     /* Assuming Y constant */
    denom_z =   dir1[Y]*dir2[X] - dir1[X]*dir2[Y] ;     /* Assuming Z constant */

    if( fabs(denom_x) >= fabs(denom_y))         /* Select the largest denominator */
    {
        if( fabs(denom_x) >= fabs(denom_z))
        {
            if(TOL_FAILURE(denom_x))            /* Directions are parallel */
            {
                return(FALSE);
            }
            *s = (dir2[Y]*(point2[Z]-point1[Z]) - dir2[Z]*(point2[Y]-point1[Y]))/denom_x;
            *t = (dir1[Y]*(point2[Z]-point1[Z]) - dir1[Z]*(point2[Y]-point1[Y]))/denom_x;
            if((point1[X] + (*s)*dir1[X] - point2[X] + (*t)*dir2[X]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
        else
        {
            *s = (dir2[X]*(point2[Y]-point1[Y]) - dir2[Y]*(point2[X]-point1[X]))/denom_z;
            *t = (dir1[X]*(point2[Y]-point1[Y]) - dir1[Y]*(point2[X]-point1[X]))/denom_z;
            if((point1[Z] + (*s)*dir1[Z] - point2[Z] + (*t)*dir2[Z]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
    }
    else
    {
        if( fabs(denom_y) >= fabs(denom_z))            /* Directions are parallel */
        {
            *s = (dir2[Z]*(point2[X]-point1[X]) - dir2[X]*(point2[Z]-point1[Z]))/denom_y;
            *t = (dir1[Z]*(point2[X]-point1[X]) - dir1[X]*(point2[Z]-point1[Z]))/denom_y;
            if((point1[Y] + (*s)*dir1[Y] - point2[Y] + (*t)*dir2[Y]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
        else
        {
            *s = (dir2[X]*(point2[Y]-point1[Y]) - dir2[Y]*(point2[X]-point1[X]))/denom_z;
            *t = (dir1[X]*(point2[Y]-point1[Y]) - dir1[Y]*(point2[X]-point1[X]))/denom_z;
            if((point1[Z] + (*s)*dir1[Z] - point2[Z] + (*t)*dir2[Z]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
    }
    vecsca( *s, dir1, CART, vec1 );         /* Calculate Cartesian point of intersection */
    vecsum( point1, vec1, CART, ipoint );

    return( TRUE );
}                     
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th July 1988
 *
 */
BOOLEAN 
line_x_line3DF(point1,dir1,point2,dir2,tol,ipoint,s,t,parallel)
                        /* Description   :- This function calculates the intersection
                         *                  if any of the two 3D bounded lines and 
                         *                  returns the intersection point with
                         *                  parameters.
                         *
                         * Return status :- TRUE if valid intersection
                         *                  FALSE otherwise
                         *   
                         * Externals     :- Used/Modified
                         *               
                         * Notes         :- The lines are bounded lines which are
                         *                  defined as a point and a direction
                         *
                         *                  See Pg 18 of "A programmer's geometry" by
                         *                  Bowyer and Woodwark for a description of the
                         *                  mathematics.
                         *
                         */ 
POINT       point1;    /* <i> Point on the first line */
VECTOR      dir1;      /* <i> Direction of first line; from start point to end point */
POINT       point2;    /* <i> Point on the second line */
VECTOR      dir2;      /* <i> Direction of second line; from start point to end point */
double      tol;       /* <i> Tolerance within which two points will be considered equal */
POINT       ipoint;    /* <o> Intersection point  */
double      *s;        /* <o> Parameter s on first line */
double      *t;        /* <o> Parameter t on second line */     
BOOLEAN     *parallel; /* <o> Whether the lines are parallel or not only checked */
                       /*     if FALSE is returned in the first place */
{                 
    VECTOR  vec1;
    double  denom_x;
    double  denom_y;
    double  denom_z;             
    double  ptol;       /* Tol within which the lines are considered parallel */

    ptol = 1.0E-06;     /* Currently hard wired */
    *parallel = FALSE;  /* Say lines are not parallel */

    denom_x =   dir1[Z]*dir2[Y] - dir1[Y]*dir2[Z] ;     /* Assuming X constant */
    denom_y =   dir1[X]*dir2[Z] - dir1[Z]*dir2[X] ;     /* Assuming Y constant */
    denom_z =   dir1[Y]*dir2[X] - dir1[X]*dir2[Y] ;     /* Assuming Z constant */

    if( fabs(denom_x) >= fabs(denom_y))         /* Select the largest denominator */
    {
        if( fabs(denom_x) >= fabs(denom_z))
        {
            if(TOL_WITHIN(denom_x,ptol))            /* Directions are parallel */
            {                                           
                *parallel = TRUE;       /* Say lines are parallel */
                return(FALSE);
            }
            *s = (dir2[Y]*(point2[Z]-point1[Z]) - dir2[Z]*(point2[Y]-point1[Y]))/denom_x;
            *t = (dir1[Y]*(point2[Z]-point1[Z]) - dir1[Z]*(point2[Y]-point1[Y]))/denom_x;
            if((point1[X] + (*s)*dir1[X] - point2[X] + (*t)*dir2[X]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
        else
        {
            if(TOL_WITHIN(denom_z,ptol))            /* Directions are parallel */
            {
                *parallel = TRUE;       /* Say lines are parallel */
                return(FALSE);
            }
            *s = (dir2[X]*(point2[Y]-point1[Y]) - dir2[Y]*(point2[X]-point1[X]))/denom_z;
            *t = (dir1[X]*(point2[Y]-point1[Y]) - dir1[Y]*(point2[X]-point1[X]))/denom_z;
            if((point1[Z] + (*s)*dir1[Z] - point2[Z] + (*t)*dir2[Z]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
    }
    else
    {
        if( fabs(denom_y) >= fabs(denom_z))            /* Directions are parallel */
        {                        
            if(TOL_WITHIN(denom_y,ptol))            /* Directions are parallel */
            {
                *parallel = TRUE;       /* Say lines are parallel */
                return(FALSE);
            }

            *s = (dir2[Z]*(point2[X]-point1[X]) - dir2[X]*(point2[Z]-point1[Z]))/denom_y;
            *t = (dir1[Z]*(point2[X]-point1[X]) - dir1[X]*(point2[Z]-point1[Z]))/denom_y;
            if((point1[Y] + (*s)*dir1[Y] - point2[Y] + (*t)*dir2[Y]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
        else
        {                        
            if(TOL_WITHIN(denom_z,ptol))            /* Directions are parallel */
            {
                *parallel = TRUE;       /* Say lines are parallel */
                return(FALSE);
            }

            *s = (dir2[X]*(point2[Y]-point1[Y]) - dir2[Y]*(point2[X]-point1[X]))/denom_z;
            *t = (dir1[X]*(point2[Y]-point1[Y]) - dir1[Y]*(point2[X]-point1[X]))/denom_z;
            if((point1[Z] + (*s)*dir1[Z] - point2[Z] + (*t)*dir2[Z]) > tol)
            {
                return(FALSE);                          /* No intersection in 3 space */
            }
        }
    }
    if( *s < -tol || *s > (1.0 + tol))          /* Outside valid range */
    {
        return(FALSE);
    }
    if( *t < -tol || *t > (1.0 + tol) )          /* Outside valid range */
    {
        return(FALSE);
    }
    vecsca( *s, dir1, CART, vec1 );         /* Calculate Cartesian point of intersection */
    vecsum( point1, vec1, CART, ipoint );

    return( TRUE );
}                     




/*      @(#)  256.1 date 12/16/89 matrices.c  */
/*+
 *
 *
 *        Filename    : matrices.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:16
 *        Last change : 89/12/16 19:24:12
 *
 *        Copyright : Practical Technology Limited  
 *        File :- matrices.c
 *
 *        This file contains the following routines:
 *
 *                 mat_mul3
 *                 mat_vec3
 *                 mat_trsp3
 *                 mat_clr3
 *                 mat_ident3
 *                 mat_add3
 *
 *                 mat_mul4
 *                 mat_vec4
 *                 mat_trsp4
 *                 mat_clr4
 *                 mat_ident4
 *                 mat_add4
 *
 *                 mat_cpy
 *
 *
 *
 *
-*/



/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_mul3( inmat1, inmat2, outmat )
                        /* This function multiplies two ( 3 by 3 ) matrices together. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat1[3][3];    /* <i> The first matrix */
double inmat2[3][3];    /* <i> The second matrix */
double outmat[3][3];    /* <o> The resultant matrix */
{
      double temp[3][3];
      int    i;
      int    j;
      int    k;

      mat_clr3( temp );                    /* Clear the temporary matrix */
      for( i = 0; i < 3; i++ )
      {
            for( j = 0; j < 3; j++ )
            {
                  for( k = 0; k < 3; k++ )
                  {
                          temp[i][j] = temp[i][j] + inmat1[i][k] * inmat2[k][j];
                  }
            }
      }
      mat_cpy( temp, 3, 3, outmat );       /* Copy matrix to output */

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_vec3( inmat, invec, outvec )
                        /* This function multiplies a ( 3 by 1 ) vector by a 
                         * ( 3 by 3 ) matrix. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[3][3];     /* <i> The matrix */
double invec[3];        /* <i> The vector */
double outvec[3];       /* <o> The resultant vector */
{
      double temp[3];
      int    i;
      int    j;

      vecclr( temp, 3 );                    /* Clear the temporary vector */

      for( i = 0; i < 3; i++ )
      {
            for( j = 0; j < 3; j++ )
            {
                  temp[i] = temp[i] + inmat[i][j] * invec[j];
            }
      }
      veccpy( temp, 3, outvec );       /* Copy vector to output */

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_trsp3( inmat, outmat )
                        /* This function transposes a ( 3 by 3 ) matrix 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[3][3];     /* <i> The matrix */
double outmat[3][3];    /* <o> The resultant matrix */
{
      double temp[3][3];
      int    i;
      int    j;

      mat_cpy( inmat, 3, 3, temp );         /* Copy input matrix to working matrix */

      for( i = 0; i < 3; i++ )             
      {
            for( j = 0; j < 3; j++ )
            {
                  outmat[i][j] = temp[j][i];
            }
      }

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_clr3( inmat )
                        /* This function clears a ( 3 by 3 ) matrix 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[3][3];     /* <i> The matrix */
{
      int    i;
      int    j;

      for( i = 0; i < 3; i++ )             
      {
            for( j = 0; j < 3; j++ )
            {
                  inmat[i][j] = 0.0;
            }
      }

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_ident3( inmat )
                        /* This function creates an identity matrix. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[3][3];     /* <i> The matrix */
{
      int    i;
      int    j;

      for( i = 0; i < 3; i++ )             
      {
            for( j = 0; j < 3; j++ )
            {
                  inmat[i][j] = 0.0;
                  inmat[i][i] = 1.0;
            }
      }

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_add3( inmat1, inmat2, outmat )
                        /* This function adds two ( 3 by 3 ) matrices.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat1[3][3];    /* <i> The first matrix */
double inmat2[3][3];    /* <i> The second matrix */
double outmat[3][3];    /* <o> The output matrix */
{
      int    i;
      int    j;

      for( i = 0; i < 3; i++ )             
      {
            for( j = 0; j < 3; j++ )
            {
                  outmat[i][j] = inmat1[i][j] + inmat2[i][j];
            }
      }

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_mul4( inmat1, inmat2, outmat )
                        /* This function multiplies two ( 4 by 4 ) matrices together. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat1[4][4];    /* <i> The first matrix */
double inmat2[4][4];    /* <i> The second matrix */
double outmat[4][4];    /* <o> The resultant matrix */
{
      double temp[4][4];
      int    i;
      int    j;
      int    k;

      mat_clr4( temp );                    /* Clear the temporary matrix */
      for( i = 0; i < 4; i++ )
      {
            for( j = 0; j < 4; j++ )
            {
                  for( k = 0; k < 4; k++ )
                  {
                          temp[i][j] = temp[i][j] + inmat1[i][k] * inmat2[k][j];
                  }
            }
      }
      mat_cpy( temp, 4, 4, outmat );       /* Copy matrix to output */

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_vec4( inmat, invec, outvec )
                        /* This function multiplies a ( 4 by 1 ) vector by a 
                         * ( 4 by 4 ) matrix. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[4][4];     /* <i> The matrix */
double invec[4];        /* <i> The vector */
double outvec[4];       /* <o> The resultant vector */
{
      double temp[4];
      int    i;
      int    j;

      vecclr( temp, 4 );                    /* Clear the temporary vector */

      for( i = 0; i < 4; i++ )
      {
            for( j = 0; j < 4; j++ )
            {
                  temp[i] = temp[i] + inmat[i][j] * invec[j];
            }
      }
      veccpy( temp, 4, outvec );       /* Copy vector to output */

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_trsp4( inmat, outmat )
                        /* This function transposes a ( 4 by 4 ) matrix 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[4][4];     /* <i> The matrix */
double outmat[4][4];    /* <o> The resultant matrix */
{
      double temp[4][4];
      int    i;
      int    j;

      mat_cpy( inmat, 4, 4, temp );         /* Copy input matrix to working matrix */

      for( i = 0; i < 4; i++ )             
      {
            for( j = 0; j < 4; j++ )
            {
                  outmat[i][j] = temp[j][i];
            }
      }

      return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_clr4( inmat )
                        /* This function clears a ( 4 by 4 ) matrix 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[4][4];     /* <i> The matrix */
{
    inmat[0][0] = 0.0;
    inmat[0][1] = 0.0;
    inmat[0][2] = 0.0;
    inmat[0][3] = 0.0;

    inmat[1][0] = 0.0;
    inmat[1][1] = 0.0;
    inmat[1][2] = 0.0;
    inmat[1][3] = 0.0;

    inmat[2][0] = 0.0;
    inmat[2][1] = 0.0;
    inmat[2][2] = 0.0;
    inmat[2][3] = 0.0;

    inmat[3][0] = 0.0;
    inmat[3][1] = 0.0;
    inmat[3][2] = 0.0;
    inmat[3][3] = 0.0;

    return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_ident4( inmat )
                        /* This function creates an identity matrix. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat[4][4];     /* <i> The matrix */
{
    inmat[0][0] = 1.0;
    inmat[0][1] = 0.0;
    inmat[0][2] = 0.0;
    inmat[0][3] = 0.0;

    inmat[1][0] = 0.0;
    inmat[1][1] = 1.0;
    inmat[1][2] = 0.0;
    inmat[1][3] = 0.0;

    inmat[2][0] = 0.0;
    inmat[2][1] = 0.0;
    inmat[2][2] = 1.0;
    inmat[2][3] = 0.0;

    inmat[3][0] = 0.0;
    inmat[3][1] = 0.0;
    inmat[3][2] = 0.0;
    inmat[3][3] = 1.0;

    return( TRUE );
}    
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
mat_add4( inmat1, inmat2, outmat )
                        /* This function adds two ( 4 by 4 ) matrices.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double inmat1[4][4];    /* <i> The first matrix */
double inmat2[4][4];    /* <i> The second matrix */
double outmat[4][4];    /* <o> The output matrix */
{
      int    i;
      int    j;

      for( i = 0; i < 4; i++ )             
      {
            for( j = 0; j < 4; j++ )
            {
                  outmat[i][j] = inmat1[i][j] + inmat2[i][j];
            }
      }

      return( TRUE );
}    


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
void
mat_cpy( inmat, rows, columns, outmat )
                        /* This function copies one matrix to another.
                         *
                         */
double inmat[];          /* <i> The first matrix */
int    rows;            /* <i> The number of rows */
int    columns;         /* <i> The number of columns */
double outmat[];         /* <o> The output matrix */
{
      int    i;

      for( i = 0; i < (rows * columns); i++ )             
      {
          outmat[i] = inmat[i];
      }

}    


/*      @(#)  256.1 date 12/16/89 planes.c  */
/*+
 *
 *        Filename    : planes.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:23
 *        Last change : 89/12/16 19:24:21
 *
 *        Copyright : Practical Technology Limited  
 *        File :- planes.c
 *
 *        This file contains the following routines:
 *
 *
 *             pln3p
 *             plnnp
 *             plnmov
 *             plnrot
 *             plnpnt
 *             plnperp
 *             plnplnx
 *             plnlinx
 *             bpln_bpln_x
 *             blin_bpln_x
 *             ublin_ubpln_x
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
pln3p( p0, p1, p2, tol, plane )
                        /* This function calculates a plane equation given three points
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double p0[];            /* <i> The first point */
double p1[];            /* <i> The second point */
double p2[];            /* <i> The third point */
double tol;             /* <i> The tolerance to avoid zero length vectors */
double plane[];         /* <o>  */
{ 
      double  v1[3];
      double  v2[3];
      double  norm[3];
      double  length;
      vecdif( p1, p0, 3, v1 );
      vecdif( p2, p0, 3, v2 );
      veccp( v1, v2, norm );
      length = veclen( norm, 3 );
      if( length < tol )
      {
            return( FALSE );
      }
      vecnrm( norm, 3, TOL, norm );
      plane[3] = vecdp( p0, norm, 3 ); /* Calculate the perpendicular distance from p0 */
                                       /* to the origin */
      veccpy( norm, 3, plane );


      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnnp( norm, point, plane )
                        /* This function calculates the equation of an infinite 
                         *  plane given the plane normal and a point on the plane.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double  norm[];         /* <i> The plane's normal */
double  point[];        /* <i> A point through which the plane has top pass */
double  plane[];        /* <o> The plane equation */
{
      veccpy( norm, 3, plane );
      vecnrm( plane, 3, TOL, plane );
      plane[3] = vecdp( plane, point, 3 );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnmov( plane_in, disp, plane_out )
                        /* This function moves an infinite plane by a displacement
                         * vector. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double plane_in[];      /* <i> The plane equation */
double disp[];          /* <i> The displacement vector */
double plane_out[];     /* <o> The resultant plane equation */
{
      double  norm[3];
      double  point[3];

      veccpy( plane_in, 3, norm);
      vecnrm( norm, 3, TOL, norm );
      vecsca( plane_in[3], norm, 3, point );
      vecsum( point, disp, 3, point );
      plnnp( norm, point, plane_out );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnrot( plane_in, disp, axis, angrad, plane_out )
                        /* This function rotates an infinite plane about an axis.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double  plane_in[];     /* <i> The plane equation */
double  disp[];         /* <i> A point on the axis */
double  axis[];         /* <i> The axis direction */
double  angrad;         /* <i> The angle of rotation in radians */
double  plane_out[];    /* <o> The resultant plane equation */
{
      double  norm[3];
      double  point1[3];
      double  point2[3];

      veccpy( plane_in, 3, norm );
      vecnrm( norm, 3, TOL, norm );
      vecsca( plane_in[3], norm, 3, point1 );
      vecsum( point1, norm, 3, point2 );
/*
    Rotation code to be inserted here!!!
*/
      vecdif( point1, point2, 3, norm );
      plnnp( norm, point1, plane_out );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnpnt( plane, point )
                        /* This function  returns the implicit point of the infinite 
                         * plane.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double  plane[];        /* <i> The plane equation */
double  point[];        /* <o> The implicit point */
{
      double norm[3];

      veccpy( plane, 3, norm );
      vecnrm( norm, 3, TOL, norm );
      vecsca( plane[3], norm, 3, point );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnperp( plane, point, vector )
                        /* This function calculates the vector that drops from the given
                         *  point perpendicularly onto the given infinite plane. 
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double plane[];         /* <i> The plane equation */
double  point[];        /* <i> The point */
double  vector[];       /* <o> The perpendicular vector from the point to the plane */
{
      double  norm[3];
      double  p;

      veccpy( plane, 3, norm );
      p = plane[3] - vecdp( point, norm, 3 );
      vecsca( p, norm, 3, vector );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnplnx( plane1, plane2, tol, point, dir )
                        /* This function calculates the intersection line between two
                         * infinite planes.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double  plane1[];       /* <i> The first plane */
double  plane2[];       /* <i> The second plane */
double  tol;            /* <i> Tolerance to catch if planes are too close to being */
                        /*     parallel */
double  point[];        /* <o> A point on the intersection */
double  dir[];          /* <o> The direction of the intersection line */
{                                                                               
      double  det;
      double  detinv;
      double  dc;
      double  db;
      double  ad;
      double  d1;
      double  d2;

      veccp( plane1, plane2, dir );

      if( (det = vecdp( dir, dir, 3 ))< tol )
      {
          return( FALSE );        /* Planes are too close to beling parallel */
      }                                                                        
      detinv = 1.0 /det;
      d1 = -1.0 * plane1[3];
      d2 = -1.0 * plane2[3];

      dc = d1           * plane2[2] - plane1[2] * d2;
      db = d1           * plane2[1] - plane1[1] * d2;
      ad = plane1[0]    * d2        - plane2[0] * d1; 

      point[0] =        ( dir[1] * dc - dir[2] * db) * detinv;
      point[1] = -1.0 * ( dir[0] * dc + dir[2] * ad) * detinv;
      point[2] =        ( dir[0] * db + dir[1] * ad) * detinv;

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
plnlinx( plane, point_in, dir, tol, param, point_out )
                        /* This function  calculates the intersection of a line
                         * and a plane.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double  plane[];        /* <i> The plane equation */
double  point_in[];     /* <i> A point on the line */
double  dir[];          /* <i> The direction of the line */
double  tol;            /* <i> Tolerance to catch line and plane parallellism */
double  *param;         /* <o> The parametric value along the line */
double  point_out[];    /* <o> The point of intersection */
{
      double denominator;

      denominator = vecdp( plane, dir, 3 );
      if( fabs( denominator ) < tol )
      {                               /* Line and plane are too close to being parallel */
          return( FALSE );
      }

      *param = ( vecdp( plane, point_in, 3 ) - plane[3] )/ denominator * -1.0;

      vecsca( *param, dir, 3, dir );
      vecsum( point_in, dir, 3, point_out );

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 8th-June-1988
 *
 */
BOOLEAN
bpln_bpln_x(p, q, start_point, start_point_params, end_point, end_point_params )
                        /* Description   :- This function calculates the line of
                         *                  intersection between two bounded planes.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The resultant parameters for each point
                         *                  are returned:
                         *
                         *                  [0] -> u parameter on first plane.
                         *                  [1] -> v parameter on first plane.
                         *                  [2] -> s parameter on second plane.
                         *                  [3] -> t parameter on second plane.
                         *
                         *                  If used in surface/surface intersection
                         *                  note that these parameters are local to the
                         *                  planes and will require re-aligning to
                         *                  surface parameter ranges.
                         *
                         *                  See Pg 321 of Geometric modelling by
                         *                  Michael Mortenson for an explanation of the
                         *                  mathematics.
                         *                 
                         *                  The Points do not have to be orthogonal but
                         *                  must not be co-linear.
                         *
                         *                  The algorithm could  return two coincident
                         *                  points.
                         *
                         *                  The points must be supplied in the following
                         *                  order:
                         *
                         *    Point2
                         *     ----------------------------
                         *     |                          |
                         *     |                          |
                         *     |                          |
                         *     ----------------------------
                         *    Point0                      Point1
                         *
                         *
                         *
                         */
PTR_POINT   p;                 /* <i> Three points defining the first plane */
PTR_POINT   q;                 /* <i> Three points defining the second plane */
POINT   start_point;           /* <o> The resultant start point of the intersection */
double  start_point_params[4]; /* <o> The start point parameters */
POINT   end_point;             /* <o> The resultant end point of the intersection */
double  end_point_params[4];   /* <o> The end point parameters */
{
    BOOLEAN parallel;
    POINT   loc_p[3];
    POINT   pnt1;
    POINT   lin_origin;
    POINT   point;
    VECTOR  direction;
    double  loc_u;
    double  loc_v;
    double  loc_t;
    INT     i;
    INT     num_points;

    for( i = 0, num_points = 0; i < 8; i++ ) 
    {                                /* For each of the eight possible intersections */
        switch (i) {
        case 0:                                 /* Assuming s = 0 */
                pntcopy( p, 3, loc_p );
                pntextract( q, 0, lin_origin );
                pntextract( q, 2, pnt1 );
                vecdif( pnt1, lin_origin, 3, direction );
                break;
        case 1:                                 /* Assuming s = 1 */
                pntextract( q, 1, lin_origin );
                break;
        case 2:                                 /* Assuming t = 0 */
                pntextract( q, 0, lin_origin );
                pntextract( q, 1, pnt1 );
                vecdif( pnt1, lin_origin, 3, direction );
                break;
        case 3:                                 /* Assuming t = 1 */
                pntextract( q, 2, lin_origin );
                break;
        case 4:                                 /* Assuming u = 0 */
                pntcopy( q, 3, loc_p );
                pntextract( p, 0, lin_origin );
                pntextract( p, 2, pnt1 );
                vecdif( pnt1, lin_origin, 3, direction );
                break;
        case 5:                                 /* Assuming u = 1 */
                pntextract( p, 1, lin_origin );
                break;
        case 6:                                 /* Assuming v = 0 */
                pntextract( p, 0, lin_origin );
                pntextract( p, 1, pnt1 );
                vecdif( pnt1, lin_origin, 3, direction );
                break;
        case 7:                                 /* Assuming v = 1 */
                pntextract( p, 2, lin_origin );
                break;
        }
        if( num_points == 0 )      
        {                            /* Looking for first point */
            if(blin_bpln_x( loc_p, lin_origin, direction, point, &loc_u, &loc_v, &loc_t, &parallel ))
            {                        /* Found first point */
                veccpy( point, 3, start_point);
                num_points++;
                switch (i) {
                case 0: 
                    start_point_params[0] = loc_u;
                    start_point_params[1] = loc_v;
                    start_point_params[2] = 0.0;
                    start_point_params[3] = loc_t;
                    break;
                case 1:    
                    start_point_params[0] = loc_u;
                    start_point_params[1] = loc_v;
                    start_point_params[2] = 1.0;
                    start_point_params[3] = loc_t;
                    break;
                case 2:    
                    start_point_params[0] = loc_u;
                    start_point_params[1] = loc_v;
                    start_point_params[2] = loc_t;
                    start_point_params[3] = 0.0;
                    break;
                case 3:    
                    start_point_params[0] = loc_u;
                    start_point_params[1] = loc_v;
                    start_point_params[2] = loc_t;
                    start_point_params[3] = 1.0;
                    break;
                case 4:    
                    start_point_params[0] = 0.0;
                    start_point_params[1] = loc_t;
                    start_point_params[2] = loc_u;
                    start_point_params[3] = loc_v;
                    break;
                case 5:    
                    start_point_params[0] = 1.0;
                    start_point_params[1] = loc_t;
                    start_point_params[2] = loc_u;
                    start_point_params[3] = loc_v;
                    break;
                case 6:    
                    start_point_params[0] = loc_t;
                    start_point_params[1] = 0.0;
                    start_point_params[2] = loc_u;
                    start_point_params[3] = loc_v;
                    break;
                case 7:    
                    start_point_params[0] = loc_t;
                    start_point_params[1] = 1.0;
                    start_point_params[2] = loc_u;
                    start_point_params[3] = loc_v;
                   break;
               }
            }
        }
        else                         /* First point has been found, try for second  */
        {
            if(blin_bpln_x( loc_p, lin_origin, direction, point, &loc_u, &loc_v, &loc_t, &parallel ))
            {                        /* Found second point */
                veccpy( point, 3, end_point);
                switch (i) {
                case 0: 
                    end_point_params[0] = loc_u;
                    end_point_params[1] = loc_v;
                    end_point_params[2] = 0.0;
                    end_point_params[3] = loc_t;
                    break;
                case 1:    
                    end_point_params[0] = loc_u;
                    end_point_params[1] = loc_v;
                    end_point_params[2] = 1.0;
                    end_point_params[3] = loc_t;
                    break;
                case 2:    
                    end_point_params[0] = loc_u;
                    end_point_params[1] = loc_v;
                    end_point_params[2] = loc_t;
                    end_point_params[3] = 0.0;
                    break;
                case 3:    
                    end_point_params[0] = loc_u;
                    end_point_params[1] = loc_v;
                    end_point_params[2] = loc_t;
                    end_point_params[3] = 1.0;
                    break;
                case 4:    
                    end_point_params[0] = 0.0;
                    end_point_params[1] = loc_t;
                    end_point_params[2] = loc_u;
                    end_point_params[3] = loc_v;
                    break;
                case 5:    
                    end_point_params[0] = 1.0;
                    end_point_params[1] = loc_t;
                    end_point_params[2] = loc_u;
                    end_point_params[3] = loc_v;
                    break;
                case 6:    
                    end_point_params[0] = loc_t;
                    end_point_params[1] = 0.0;
                    end_point_params[2] = loc_u;
                    end_point_params[3] = loc_v;
                    break;
                case 7:    
                    end_point_params[0] = loc_t;
                    end_point_params[1] = 1.0;
                    end_point_params[2] = loc_u;
                    end_point_params[3] = loc_v;
                    break;
               }
               return(TRUE);                /* Successfully found two points */
            }
        }
    }
    return(FALSE);      /* Failed to find two intersections */
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 8th-July-1988
 *
 */
BOOLEAN
blin_bpln_x( p, lin_origin, direction, point, u, v, t, parallel )
                        /* Description   :- This function calculates the intersection
                         *                  point of a finite line and bounded plane.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The algorithm will retun FALSE if:
                         *
                         *                  1. The plane axes are co-linear.
                         *                  2. The line is parallel to the plane.
                         *                  3. The intersection lies outside the
                         *                     bounds.
                         *
                         *                  See Pg 321 of Geometric modelling by
                         *                  Michael Mortenson for an explanation of the
                         *                  mathematics.
                         *                 
                         *                  The Points do not have to be orthogonal but
                         *                  must not be co-linear.
                         *
                         *                  The points must be supplied in the following
                         *                  order:
                         *
                         *    Point2
                         *     ----------------------------
                         *     |                          |
                         *     |                          |
                         *     |                          |
                         *     ----------------------------
                         *    Point0                      Point1
                         *
                         */
PTR_POINT   p;          /* <i> Three points defining the plane */
POINT       lin_origin; /* <i> Origin of finite line */
VECTOR      direction;  /* <i> The vector representing the direction of the line */
POINT       point;      /* <o> The resultant point of the intersection */
double      *u;         /* <o> The U parameter on the plane */
double      *v;         /* <o> The V parameter on the plane */
double      *t;         /* <o> The parameter on the line */    
BOOLEAN     *parallel;  /* <o> The lines are deemed parallel */
{
    POINT   pln_origin;
    POINT   point1;
    POINT   point2;
    VECTOR  x_axis;
    VECTOR  y_axis;
    VECTOR  vec1;
    double  sca1;
    double  sca2;
    double  sca3;
                                                    
    *parallel = FALSE;      /* Say that the lines are not parallel */

    pntextract( p, 0, pln_origin );
    pntextract( p, 1, point1 );
    pntextract( p, 2, point2 );
    vecdif( point1, pln_origin, CART, x_axis);
    vecdif( point2, pln_origin, CART, y_axis);

    veccp( x_axis, y_axis, vec1 );
    sca1 = vecdp( vec1, pln_origin, 3 );
    sca2 = vecdp( vec1, lin_origin, 3 );
    sca3 = vecdp( vec1, direction, 3 );

    if(TOL_FAILURE(sca3))
    {                                 /* Line is parallel to plane */
        *parallel = TRUE;             /* Signal that line is parallel to plane */
        return(FALSE);
    }

    *t = (sca1 - sca2)/sca3;
    if( *t < 0.0 || *t > 1.0 )        /* Intersection is outside parameter range */

    {
        return(FALSE);
    }

    veccp( y_axis, direction, vec1 );
    sca1 = vecdp( vec1, lin_origin, 3 );
    sca2 = vecdp( vec1, pln_origin, 3 );
    sca3 = vecdp( vec1, x_axis, 3 );
    if(TOL_FAILURE(sca3))
    {                                 /* Bad geometry */
        return(FALSE);
    }

    *u = (sca1 - sca2)/sca3;   
    if( *u < 0.0 || *u > 1.0 )        /* Intersection is outside parameter range */
    {
        return(FALSE);
    }

    veccp( x_axis, direction, vec1 );
    sca1 = vecdp( vec1, lin_origin, 3 );
    sca2 = vecdp( vec1, pln_origin, 3 );
    sca3 = vecdp( vec1, y_axis, 3 );
    if(TOL_FAILURE(sca3))
    {                                 /* Bad geometry */
        return(FALSE);
    }

    *v = (sca1 - sca2)/sca3;
    if( *v < 0.0 || *v > 1.0 )        /* Intersection is outside parameter range */
    {
        return(FALSE);
    }
    vecsca( *t, direction, CART, vec1 );    /* Calculate the point of intersection */
    vecsum( lin_origin, vec1, 3, point );  

    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 29th Aug 1988
 *
 */
BOOLEAN
ublin_ubpln_x( p, lin_origin, direction, point, u, v, t, parallel )
                        /* Description   :- This function calculates the intersection
                         *                  point of a infinite line and unbounded plane.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- The algorithm will retun FALSE if:
                         *
                         *                  1. The plane axes are co-linear.
                         *                  2. The line is parallel to the plane.
                         *
                         *                  See Pg 321 of Geometric modelling by
                         *                  Michael Mortenson for an explanation of the
                         *                  mathematics.
                         *                 
                         *                  The Points do not have to be orthogonal but
                         *                  must not be co-linear.
                         *
                         *                  The points must be supplied in the following
                         *                  order:
                         *
                         *    Point2
                         *     ----------------------------
                         *     |                          |
                         *     |                          |
                         *     |                          |
                         *     ----------------------------
                         *    Point0                      Point1
                         *
                         */
PTR_POINT   p;          /* <i> Three points defining the plane */
POINT       lin_origin; /* <i> Origin of finite line */
VECTOR      direction;  /* <i> The vector representing the direction of the line */
POINT       point;      /* <o> The resultant point of the intersection */
double      *u;         /* <o> The U parameter on the plane */
double      *v;         /* <o> The V parameter on the plane */
double      *t;         /* <o> The parameter on the line */      
BOOLEAN     *parallel;  /* <o> Signals whether the line and plane is parallel */
{
    POINT   pln_origin;
    POINT   point1;
    POINT   point2;
    VECTOR  x_axis;
    VECTOR  y_axis;
    VECTOR  vec1;
    double  sca1;
    double  sca2;
    double  sca3;
                                                                                
    *parallel = FALSE;      /* Say line and plane are not parallel */

    pntextract( p, 0, pln_origin );
    pntextract( p, 1, point1 );
    pntextract( p, 2, point2 );
    vecdif( point1, pln_origin, CART, x_axis);
    vecdif( point2, pln_origin, CART, y_axis);

    veccp( x_axis, y_axis, vec1 );
    sca1 = vecdp( vec1, pln_origin, 3 );
    sca2 = vecdp( vec1, lin_origin, 3 );
    sca3 = vecdp( vec1, direction, 3 );
    if(TOL_FAILURE(sca3))
    {                                 /* Line is parallel to plane */
        *parallel = TRUE;             /* Line and plane are parallel */
        return(FALSE);
    }

    *t = (sca1 - sca2)/sca3;

    veccp( y_axis, direction, vec1 );
    sca1 = vecdp( vec1, lin_origin, 3 );
    sca2 = vecdp( vec1, pln_origin, 3 );
    sca3 = vecdp( vec1, x_axis, 3 );
    if(TOL_FAILURE(sca3))
    {                                 /* Bad geometry */
        return(FALSE);
    }

    *u = (sca1 - sca2)/sca3;   

    veccp( x_axis, direction, vec1 );
    sca1 = vecdp( vec1, lin_origin, 3 );
    sca2 = vecdp( vec1, pln_origin, 3 );
    sca3 = vecdp( vec1, y_axis, 3 );
    if(TOL_FAILURE(sca3))
    {                                 /* Bad geometry */
        return(FALSE);
    }

    *v = (sca1 - sca2)/sca3;

    vecsca( *t, direction, CART, vec1 );    /* Calculate the point of intersection */
    vecsum( lin_origin, vec1, 3, point );  

    return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 rotate.c  */
/*+
 *
 *
 *        Filename    : rotate.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:32
 *        Last change : 89/12/16 19:24:33
 *
 *        Copyright : Practical Technology Limited  
 *        File :- rotate.c
 *
 *        This file contains the following routines:
 *
 *             rot_point
 *             rot_h_point
 *             calc_transformation
 *             rot_set
 *
-*/


static    MATRIX_4BY4     matrix;     /* The homogeneous transformation matrix */

                                   
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
ROT2DF( inx,iny,outx,outy )
                        /* This routine transforms a Cartesian point.
                         * Note : the transformation matrix must already exist!
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */    
double    *inx;         /* <i> The X input */
double    *iny;         /* <i> The Y input */
double    *outx;        /* <o> The X output */
double    *outy;        /* <o> The Y output */
{
    POINT   in;
    POINT   out;

    in[X] = *inx;
    in[Y] = *iny;

    rot_2Dpoint(in,out);

    *outx = out[X];
    *outy = out[Y];
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
rot_2Dpoint( input, output )
                        /* This routine transforms a Cartesian point.
                         * Note : the transformation matrix must already exist!
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    input[];      /* <i>  The input point */
double    output[];     /* <o>  The output point */
{ 
      double  input_h[4];
      double  output_h[4];

      veccpy( input, 2, input_h);              /* Convert to homogeneous version */
      input_h[2] = 0.0;                        /* Set Z to 0.0 */
      input_h[3] = 1.0;

      rot_h_point( input_h, output_h );        /* Transform the homogeneous point */

      veccpy( output_h, 2, output );           /* Convert to Cartesian co-ordinates */

      return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
rot_point( input, output )
                        /* This routine transforms a Cartesian point.
                         * Note : the transformation matrix must already exist!
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    input[];      /* <i>  The input point */
double    output[];     /* <o>  The output point */
{ 
      double  input_h[4];
      double  output_h[4];

      veccpy( input, 3, input_h);              /* Convert to homogeneous version */
      input_h[3] = 1.0;

      rot_h_point( input_h, output_h );     /* Transform the homogeneous point */

      veccpy( output_h, 3, output );           /* Convert to Cartesian co-ordinates */

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
rot_h_point( input, output )
                        /* This routine transforms a homogeneous point.
                         * Note : the transformation matrix must already exist!
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    input[];      /* <i>  The input point in homogeneous co-ordinates */
double    output[];     /* <o>  The output point in homogeneous co-ordinates */
{ 
      mat_vec4( matrix, input, output );
      return( TRUE );
}

/*
 *===================================================================================*
 *
 *                                                            Author :: J.L.Buchanan
 *                                                            Date   :: 13 Oct 1988
 *
 */
BOOLEAN
CLC2DT( pointx,pointy,angle )
                        /* This routine calculates the transformation matrix necessary
                         * to rotate points about the given axis.
                         * axis and angle.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    *pointx;       /* <i> The X co-ord to rotate about */
double    *pointy;       /* <i> The Y co-ord to rotate about */
double    *angle;        /* <i>  The angle in radians */
{
    POINT    disp;
    POINT    axis;
                                                                                   
    axis[X] = 0.0;
    axis[Y] = 0.0;
    axis[Z] = 1.0;

    disp[X] = *pointx;
    disp[Y] = *pointy;
    
    calc_transformation(disp,axis,*angle);

}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
calc_2Dtransformation( disp, axis, angle )
                        /* This routine calculates the transformation matrix necessary
                         * to rotate points about the given axis.
                         * axis and angle.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    disp[];       /* <i>  A point on the axis of rotation */
double    axis[];       /* <i>  The axis of rotation */
double    angle;        /* <i>  The angle in radians */
{                                                
    /* Ensure point is on the XY plane */
    disp[Z] = 0.0;
    disp[H] = 1.0;

    return(calc_transformation(disp,axis,angle));
}

/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
BOOLEAN
calc_transformation( disp, axis, angle )
                        /* This routine calculates the transformation matrix necessary
                         * to rotate points about the given axis.
                         * axis and angle.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double    disp[];       /* <i>  A point on the axis of rotation */
double    axis[];       /* <i>  The axis of rotation */
double    angle;        /* <i>  The angle in radians */
{               
      MATRIX_4BY4  displacement;
      MATRIX_4BY4  neg_displacement;
              
      mat_ident4(matrix);                       /* Watch out for homogeneous row */
                                                /* and column */
      rotation_set( axis, angle, matrix );      /* Set the rotation transformation */
      mat_ident4( neg_displacement );           /* Allow for translation to the origin */
      mat_ident4( displacement );               /* and back */
      neg_displacement[0][3] = -disp[0];
      neg_displacement[1][3] = -disp[1];
      neg_displacement[2][3] = -disp[2];
      displacement[0][3] = disp[0];
      displacement[1][3] = disp[1];
      displacement[2][3] = disp[2];
      mat_mul4( matrix, neg_displacement, matrix );  
      mat_mul4( displacement, matrix, matrix );      

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 31st-March-1988
 *
 */
void
rotation_set( axis, angle, mat )
                        /* This routine calculates the rotation matrix for a given
                         * axis and angle.
                         *
                         *      = NOTHING
                         *
                         */
double       axis[];    /* <i>  The axis of rotation, passing through the origin */
double       angle;     /* <i>  The angle in radians */
MATRIX_4BY4  mat;       /* <o>  The resultant rotation matrix */
{
      double  t[6];
      double  local_axis[3];

      vecnrm( axis, 3, TOL, local_axis);   /* Ensure that the axis is normalised */

      t[0] = cos( angle );
      t[1] = sin( angle );
      t[2] = 1.0 - t[0];
      t[3] = local_axis[0] * local_axis[0];
      t[4] = local_axis[1] * local_axis[1];
      t[5] = local_axis[2] * local_axis[2];

      mat[0][0] = t[3]                                  + t[0] * (1.0 - t[3]);
      mat[0][1] = local_axis[0] * local_axis[1] * t[2]  - local_axis[2] * t[1]; 
      mat[0][2] = local_axis[2] * local_axis[0] * t[2]  + local_axis[1] * t[1]; 
      mat[1][0] = local_axis[0] * local_axis[1] * t[2]  + local_axis[2] * t[1];
      mat[1][1] = t[4]                                  + t[0] * (1.0 - t[4]);
      mat[1][2] = local_axis[1] * local_axis[2] * t[2]  - local_axis[0] * t[1]; 
      mat[2][0] = local_axis[2] * local_axis[0] * t[2]  - local_axis[1] * t[1]; 
      mat[2][1] = local_axis[1] * local_axis[2] * t[2]  + local_axis[0] * t[1];
      mat[2][2] = t[5]                                  + t[0] * (1.0 - t[5]);

}
/*      @(#)  256.1 date 12/16/89 sort.c  */
/*+
 *
 *
 *        Filename    : sort.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:38
 *        Last change : 89/12/16 19:24:43
 *
 *        Copyright : Practical Technology Limited  
 *        File :- sort.c
 *
 *        This file contains the following routines:
 *          
 *        quicksort
 *
-*/


/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 25th-July-1988
 *
 */
BOOLEAN
quicksort( m, n, tau  )
                        /* Description   :- This function sorts the elements of the
                         *                  given double array into ascending order.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See "Fundamentals of data structures" by
                         *                  E Horowitz and S Sahni. 
                         *                  Publishers : Pitman.
                         *                  ISBN : 0 273 01062 X
                         *
                         *                  This routine is recursive.
                         *
                         *                  To sort the whole array, set m to 0 and n
                         *                  to the upper index.
                         */
INT m;                  /* <i> The lower index */
INT n;                  /* <i> The upper index */
double tau[];           /* <i> The array of doubles to be sorted */
{
      double    t;
      double    k;
      INT       i;
      INT       j;

      if( m < n )
      {
        i = m;
        j = n + 1;
        k = tau[m];
        for( ; ; )                      /* Infinite loop */
        {
            do { i++; }
                while ( tau[i] < k );
            do { j--; }
                while ( tau[j] > k );
            if( i < j )                 /* Interchange array elements */
            {
                t = tau[i];
                tau[i] = tau[j];
                tau[j] = t;
            }
            else
            {
                break;                  /* Exit the loop */
            }
        }
        t = tau[m];                     /* Interchange array elements */
        tau[m] = tau[j];
        tau[j] = t;
        quicksort ( m, j - 1, tau );    /* Sort the lower partition */
        quicksort ( j+1, n, tau );      /* Sort the upper partition */
        }
    return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J L Buchanan
 *                                                            Date   :: 26 Aug 1988
 *
 */
BOOLEAN
quicksort_dim( m, n, dim, ele, tau  )
                        /* Description   :- This function sorts the elements of the
                         *                  given double array into ascending order.
                         *
                         * Return status :- TRUE if successful
                         *                  FALSE otherwise
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :- See "Fundamentals of data structures" by
                         *                  E Horowitz and S Sahni. 
                         *                  Publishers : Pitman.
                         *                  ISBN : 0 273 01062 X
                         *
                         *                  This routine is recursive.
                         *
                         *                  To sort the whole array, set m to 0 and n
                         *                  to the upper index.
                         */
INT m;                  /* <i> The lower index */
INT n;                  /* <i> The upper index */
INT dim;                /* <i> The dimension of the array */
INT ele;                /* <i> The element index within the dimension block to use as*/
                        /*     the sort master */
double tau[];           /* <i> The array of doubles to be sorted */
{
      double    t;
      double    k;
      INT       i;
      INT       j;    
      INT       l;

      if( m < n )
      {
        i = m;
        j = n + 1;
        k = tau[m * dim + ele];
        for( ; ; )                      /* Infinite loop */
        {
            do { i++; }
                while ( tau[i * dim + ele] < k );
            do { j--; }
                while ( tau[j * dim + ele] > k );
            if( i < j )                 /* Interchange array elements */
            {                            
                for (l = 0; l < dim; l++)
                {
                   t = tau[i * dim + l];
                   tau[i * dim + l] = tau[j * dim + l];
                   tau[j * dim + l] = t;
                }
            }
            else
            {
                break;                  /* Exit the loop */
            }
        }                     
        for ( l = 0; l < dim; l++)
        {
           t = tau[m * dim + l];                     /* Interchange array elements */
           tau[m * dim + l] = tau[j * dim + l];
           tau[j * dim + l]= t;
        }
        quicksort_dim ( m, j - 1, dim, ele,tau );    /* Sort the lower partition */
        quicksort_dim ( j+1, n, dim, ele, tau );      /* Sort the upper partition */
        }
    return( TRUE );
}

/*      @(#)  256.1 date 12/16/89 vectors.c  */
/*+
 *
 *        Filename    : vectors.c
 *        Version     : 256.1
 *        Retrieved   : 90/01/22 14:50:43
 *        Last change : 89/12/16 19:24:52
 *
 *        Copyright : Practical Technology Limited  
 *        File :- vectors.c
 *
 *        This file contains the following routines:
 *
 *          vecnrm
 *          vecclr
 *          veclen
 *          veclensq
 *          vecdp
 *          veccp
 *          vecsum
 *          vecdif 
 *          vecdir
 *          vecneg
 *          veccpy
 *          vecsca
 *          vecang
 *          vecswap
 *          vecu
 *          vecv
 *          vecw
 *
-*/


extern  double veclen();
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecnrm( invec, dim, tol, outvec )
                        /* This routine normalises the length of a vector to unity
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec[];         /* <i> The input vector */
int    dim;             /* <i> The dimension of the vector */
double  tol;            /* <i> The tolerance to protect against zero length vectors */
double outvec[];        /* <o> The output vector */
{
      double length;
      int     i;

      length = veclen( invec, dim );
      if( length < tol )            /* Calculate the vectors length */
      {
            return( FALSE );
      }
      for( i = 0; i < dim; i++ )
      {
            outvec[i] = invec[i]/length;
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecclr( invec, dim )
                        /* This routine clears a vector to zero.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec[];         /* <i> The vector to be cleared */
int    dim;             /* <i> The dimension of the vector */
{
      int i;

      for ( i = 0; i < dim; i++ )
      {
            invec[i] = 0.0;
      }

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
double
veclen( invec, dim )
                        /* This routine calculates the length of a vector.
                         *
                         *      = The vector's length
                         *
                         */
double invec[];         /* <i> The input vector */
int    dim;             /* <i> The dimension of the vector */
{
      double length;
      int    i;

      for( length = 0.0, i = 0; i < dim; i++ )
      {
            length = length + invec[i] * invec[i];
      }
      return( sqrt( length ));
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
double
veclensq( invec, dim )
                        /* This routine calculates the square of the length of a vector.
                         *
                         *      = The vector's length
                         *
                         */
double invec[];         /* <i> The input vector */
int    dim;             /* <i> The dimension of the vector */
{
      double length;
      int    i;

      for( length = 0.0, i = 0; i < dim; i++ )
      {
            length = length + invec[i] * invec[i];
      }
      return( length );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
double
vecdp( invec1, invec2, dim )
                        /* This routine calculates the dot product between two vectors.
                         *
                         *      = The resultant scalar
                         *
                         */
double invec1[];        /* <i> The first vector */
double invec2[];        /* <i> The second vector */
int    dim;             /* <i> The dimension of the two vectors */
{ 
      double  out;
      int     i;

      for( out = 0.0, i = 0; i < dim; i++ )
      {
            out = out + invec1[i] * invec2[i];
      }
      return( out );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
veccp( invec1, invec2, outvec )
                        /* This routine calculates the cross product between two vectors.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec1[];        /* <i> The first vector */
double invec2[];        /* <i> The second vector */
double outvec[];        /* <o> The resultant vector */
{ 
      outvec[0] = invec1[1] * invec2[2] - invec1[2] * invec2[1];
      outvec[1] = invec1[2] * invec2[0] - invec1[0] * invec2[2];
      outvec[2] = invec1[0] * invec2[1] - invec1[1] * invec2[0];

      return(TRUE);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecsum( invec1, invec2, dim, outvec )
                        /* This routine calculates the addition of two vectors.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec1[];        /* <i> The first vector */
double invec2[];        /* <i> The second vector */
int    dim;             /* <i> The dimension of the vectors */
double outvec[];        /* <o> The resultant vector */
{
      int i;

      for( i = 0; i < dim; i++ )
      {
            outvec[i] = invec1[i] + invec2[i];
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecdif( invec1, invec2, dim, outvec )
                        /* This routine calculates the subtraction of one vector
                         * from another.   (invec1 - invec2)
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec1[];        /* <i> The first vector */
double invec2[];        /* <i> The second vector */
int    dim;             /* <i> The dimension of the vectors */
double outvec[];        /* <o> The resultant vector */
{
      int i;

      for( i = 0; i < dim; i++ )
      {
            outvec[i] = invec1[i] - invec2[i];
      }
      return( TRUE ); 
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecdir( invec1, dim, invec2 )
                        /* This routine checks to see if the two vectors are going in 
                         * the same direction.
                         *
                         *      = TRUE if vectors are in same direction
                         *      = FALSE if vectors aren't
                         *
                         */
double invec1[];        /* <i> The first vector */
int    dim;             /* <i> The dimension of the vectors */
double invec2[];        /* <i> The second vector */
{
      int i;

      for( i = 0; i < dim; i++ )
      {  
            if (((invec1[i] > 0.0) && (invec2[i] < 0.0))
                || ((invec1[i] < 0.0) && (invec2[i] > 0.0)))
                return(FALSE);
      }
      return( TRUE ); 
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecneg( invec, dim, outvec )
                        /* This routine calculates the negation of a vector.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec[];         /* <i> The vector */
int    dim;             /* <i> The dimension of the vector */
double outvec[];        /* <o> The resultant vector */
{
      int i;

      for( i = 0; i < dim; i++ )
      {
            outvec[i] = -invec[i];
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
veccpy( invec, dim, outvec )
                        /* This routine copies one vector to another.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec[];         /* <i> The vector */
int    dim;             /* <i> The dimension of the vector */
double outvec[];        /* <o> The resultant vector */
{
      int i;

      for( i = 0; i < dim; i++ )
      {
            outvec[i] = invec[i];
      }
      return( TRUE );
}
/*         
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecsca( scalar, invec, dim, outvec )
                        /* This routine scales a vector.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double scalar;          /* <i> The scale factor */
double invec[];         /* <i> The vector */
int    dim;             /* <i> The dimension of the vector */
double outvec[];        /* <o> The resultant vector */
{ 
      int i;

      for( i = 0; i < dim; i++ )
      {
            outvec[i] = scalar * invec[i];
      }
 
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecang( invec1, invec2, dim, tol, angle )
                        /* This routine calculates the angle between two vectors.
                         *
                         *      = The angle in radians
                         *
                         */
double invec1[];        /* <i> The first vector */
double invec2[];        /* <i> The second vector */
int    dim;             /* <i> The dimension of the vectors */
double  tol;            /* <i> The tolerance to protect against zero length vectors */
double  *angle;         /* <o> The resultant angle */
{
      double  length1;
      double  length2;                            
      double  scalar;

      length1 = veclen( invec1, dim );
      length2 = veclen( invec2, dim );

      if( length1 < tol || length2 < tol )
      {
            return( FALSE );
      }
      scalar  = vecdp( invec1, invec2, dim );
      *angle = acos( scalar/( length1 * length2 ));

      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 30th-March-1988
 *
 */
BOOLEAN
vecswap( invec, dim, outvec )
                        /* This routine swaps one vector for another.
                         *
                         *      = TRUE if successful
                         *      = FALSE otherwise
                         *
                         */
double invec[];         /* <i> The first vector */
int    dim;             /* <i> The dimension of the vector */
double outvec[];        /* <i/o> The second vector */
{
      double temp;
      int i;

      for( i = 0; i < dim; i++ )
      {
            temp = invec[i];
            invec[i] = outvec[i]; 
            outvec[i] = temp;
      }
      return( TRUE );
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 24th-August-1988
 *
 */
void
vecu(u)
                        /* Description   :- This function returns the unit vector in X
                         *
                         * Return status :- void
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  u[];            /* <o> The unit vector in X */
{
      u[0] = 1.0;
      u[1] = 0.0;
      u[2] = 0.0;
      return;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 24th-August-1988
 *
 */
void
vecv(v)
                        /* Description   :- This function returns the unit vector in Y
                         *
                         * Return status :- void
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  v[];            /* <o> The unit vector in Y */
{
      v[0] = 0.0;
      v[1] = 1.0;
      v[2] = 0.0;
      return;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: Jim Mulhearn
 *                                                            Date   :: 24th-August-1988
 *
 */
void
vecw(w)
                        /* Description   :- This function returns the unit vector in Z
                         *
                         * Return status :- void
                         *   
                         * Externals     :-  Used/Modified
                         *
                         * Notes         :-
                         *
                         */
double  w[];            /* <o> The unit vector in Z */
{
      w[0] = 0.0;
      w[1] = 0.0;
      w[2] = 1.0;
      return;
}


