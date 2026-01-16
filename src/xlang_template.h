

/* SCCS id Keywords             @(#)  412.1 date 6/11/92 xlang_template.h   */

/*
 *
 *
 *        Filename    : xlang_template.h
 *        Version     : 412.1
 *
 *        Copyright : Practical Technology Limited
 *
 *        Cross language lookup file ( TEMPLATE VERSION ONLY )
 *

*/

#ifdef USE_UNDERSCORE      /* Functions need underscore in X lang  (APOLLO F77 ) */

#endif


#ifdef NO_UNDERSCORE      /* Functions do not need underscore in X lang  (IBM 6000, APOLLO FTN ) */

#endif


#ifdef ILLEGAL_UNDERSCORE      /* Underscores except at the end are requred ( SUN ) */

#endif
