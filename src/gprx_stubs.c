

/*     SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_stubs.c   */

/*

   Stubs for all gpr routines called but not used or needed

*/

#include "gprx_types.h"
#include "gprx_errors.h"
#include "xlang.h"
#include "daxcad_functions.h"


GPR_$ACQUIRE_DISPLAY(st)

      /* Description   :- Acqure the GPR display. Not used.
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

GprStatus *st;

{

   *st = 0;
   return F77true; /* unobscured return */

}

INT
GPR_$RELEASE_DISPLAY(st)

      /* Description   :- Acqure the GPR display. Not used.
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

GprStatus *st;

{

   *st = 0;

}

INT
GPR_$FORCE_RELEASE(Count,st)

      /* Description   :- Acqure the GPR display. Not used.
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

short *Count;     /*   <o> Number of times aquired */
GprStatus *st;

{

   *Count = 0;
   *st = 0;

}

GPR_$SET_ACQ_TIME_OUT(TimeOut,st)

      /* Description   :- Sets timeout of aqcure display. Not used.
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

short *TimeOut;
GprStatus *st;

{

   *st = 0;

}

GPR_$SET_OBSCURED_OPT(ObsOpt,st)

      /* Description   :- Sets the action on diusplay being obscured
       * 
       * 
       *
       * Return status :- NOT USED
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

short *ObsOpt;
GprStatus *st;

{

   *st = 0;

}



GPR_$RASTER_OP_PRIM_SET(PrimSet,st)

      /* Description   :- Allows certain ops to be done on individual primitives
       *                  Not supprted on GPRX ( all primitives have all raster ops )
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

short *PrimSet;    /*  <i> Sets the raster op for GPR primitives */
GprStatus *st;

{

   *st = 0;

}

GPR_$ALLOCATE_ATTRIBUTE_BLOCK(Ablock,st)

      /* Description   :- allocates a descripter for bitmap ops. This allows switching
       *                  of information about bitmaps. GPRX does not support this
       *                  facility. Any attribute block will be undefined.
       *
       * Return status :- NONE
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

int *Ablock;
GprStatus *st;

{

   *Ablock = 0;
   *st = 0;
   
}

GPR_$DEALLOCATE_ATTRIBUTE_BLOCK(Ablock,st)

      /* Description   :- dealocate allocates a descripter for bitmap ops. This allows switching
       *                  of information about bitmaps. GPRX does not support this
       *                  facility. Any attribute block will be undefined.
       *
       * Return status :- NONE
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

int *Ablock;
GprStatus *st;

{

   *Ablock = 0;
   *st = 0;
   
}






INT
GPR_$SET_TEXT_PATH(Direction,st)

      /* Description   :- Sets text direction
       * 
       * 
       *
       * Return status :- NOT USED
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

short *Direction;
GprStatus *st;

{

   *st = 0;

}




INT
GPR_$INQ_COLOR_MAP()

      /* Description   :- Sets text direction
       * 
       * 
       *
       * Return status :- NOT USED
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


}



INT
GPR_$INQ_RASTER_OPS()

      /* Description   :- Sets text direction
       * 
       * 
       *
       * Return status :- NOT USED
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


}


INT
AqDaxDisplay()

      /* Description   :- DAXCAD specific stub
       * 
       * 
       *
       * Return status :- NOT USED
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
}
INT
RelDaxDisplay()

      /* Description   :- DAXCAD specific stub
       * 
       * 
       *
       * Return status :- NOT USED
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
}


INT 
ERROR_$PRINT(st)

      /* Description   :- APOLLO specific stub
       *
       *
       *
       * Return status :- NOT USED
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

int *st;
{

	printf("Apollo error code %d\n",&st);
}

INT
GPR_$SET_HORIZONTAL_SPACING(Font,Space,st)

      /* Description   :- Sets text direction
       * 
       * 
       *
       * Return status :- NOT USED
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

short *Font;
short *Space;
int *st;

{


}
INT
GPR_$INQ_HORIZONTAL_SPACING(Fontid,Space,st)

      /* Description   :- Sets text direction
       * 
       * 
       *
       * Return status :- NOT USED
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

short *Fontid;
short *Space;
int *st;

{

	*Space=0;
	*st = 0;
}


INT
GPR_$REPLICATE_FONT(Font1,Font2,st)

short *Font1;
short *Font2;
int *st;
{
}

