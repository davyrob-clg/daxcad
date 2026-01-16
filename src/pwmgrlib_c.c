
/*      @(#)  412.1 date 6/11/92 pwmgrlib_c.c 
 *
 *
 *        Filename    : pwmgrlib_c.c
 *        Version     : 412.1
 *        Retrieved   : 92/06/12 15:49:09
 *        Last change : 92/06/11 14:39:17
 *
 *        Copyright : Practical Technology Limited  
 *        File :- pwmgrlib_c.c
 *
 *        Contains the New bit encoding  routines
 *        for use with protection system
 *         
 */





#include <stdio.h>
#include <errno.h>
#include "xlang.h"
#include "local.h"
#include "gprx_types.h"
                  
typedef struct BITFIELD {
       unsigned int bit_0 :1;
       unsigned int bit_1 :1;
       unsigned int bit_2 :1;
       unsigned int bit_3 :1;
       unsigned int bit_4 :1;
       unsigned int bit_5 :1;
       unsigned int bit_6 :1;
       unsigned int bit_7 :1;
       unsigned int bit_8 :1;
       unsigned int bit_9 :1;
       unsigned int bit_10 :1;
       unsigned int bit_11 :1;
       unsigned int bit_12 :1;
       unsigned int bit_13 :1;
       unsigned int bit_14 :1;
       unsigned int bit_15 :1;
       unsigned int bit_16 :1;
       unsigned int bit_17 :1;
       unsigned int bit_18 :1;
       unsigned int bit_19 :1;
       unsigned int bit_20 :1;
       unsigned int bit_21 :1;
       unsigned int bit_22 :1;
       unsigned int bit_23 :1;
       unsigned int bit_24 :1;
       unsigned int bit_25 :1;
       unsigned int bit_26 :1;
       unsigned int bit_27 :1;
       unsigned int bit_28 :1;
       unsigned int bit_29 :1;
       unsigned int bit_30 :1;
       unsigned int bit_31 :1;
       } WORD;


K10004(Nodeid, Comp, Codeflag)

      /* Description   :- replacement for Kirks gothic encryption 
       *                  algorythmn replaces K10004 & K10002
       *                
       *                
       * Return status :- cannot fail
       *                
       *                
       *                
       * Notes         :- it will encode the individual bits of Nodeid and Comp
       *                  and then swap bits between them
       *                  
       *                  
       *                  
       *                  
       *
       */

WORD    *Nodeid;    /* <i/o> Node ID 4 byte bitfield to be encoded or decoded */
WORD    *Comp;      /* <i/o> Composite of DATE and PRODUCT to be encoded or decoded */
LOGICAL *Codeflag;  /* <i> TRUE to encode FALSE to decode  */
{

  WORD temp_1, temp_2;

  if(*Codeflag == F77false)
  {
     /* Right decode the fields */

     /* Right  unjumble both the fields together*/    
     Jumble(Nodeid, Comp); 

      /* Right decode the field 1 */    
     temp_1 = *Nodeid;
     Decode(Nodeid, temp_1);

      /* Right decode the field 2 */    
     temp_2 = *Comp;
     Decode(Comp, temp_2);

  }
  else
  {
      /* Right encode the fields */    

      /* Right encode the field 1 */    
     temp_1 = *Nodeid;
     Encode(Nodeid, temp_1);

      /* Right encode the field 2 */    
     temp_2 = *Comp;
     Encode(Comp, temp_2);

      /* Right  jumble both the fields together*/    

     Jumble(Comp, Nodeid);


  }                           
}


void
Encode(Field_1, Field_2)
      /* Description   :- Copies the bits in Field_2 into Field_1
       *                  in a set pattern (4 byte fields)
       *                  for ENCODING 
       *                
       * Return status :- cannot fail
       *                
       *                
       *                
       * Notes         :- FOR use with partner function decode()
       *                  
       *                  
       *                  
       *
       */

WORD *Field_1;     /* <o> 4 byte target bitfield  */
WORD  Field_2;     /* <i>   4 byte source bitfield   */
{
      Field_1->bit_0 = Field_2.bit_0;
      Field_1->bit_1 = Field_2.bit_25;
      Field_1->bit_2 = Field_2.bit_13;
      Field_1->bit_3 = Field_2.bit_17;
      Field_1->bit_4 = Field_2.bit_30;
      Field_1->bit_5 = Field_2.bit_23;
      Field_1->bit_6 = Field_2.bit_4;
      Field_1->bit_7 = Field_2.bit_18;
      Field_1->bit_8 = Field_2.bit_22; 
      Field_1->bit_9 = Field_2.bit_16;
      Field_1->bit_10 = Field_2.bit_11;
      Field_1->bit_11 = Field_2.bit_19;
      Field_1->bit_12 = Field_2.bit_27;
      Field_1->bit_13 = Field_2.bit_7;
      Field_1->bit_14 = Field_2.bit_24;
      Field_1->bit_15 = Field_2.bit_9;
      Field_1->bit_16 = Field_2.bit_5;
      Field_1->bit_17 = Field_2.bit_26; 
      Field_1->bit_18 = Field_2.bit_2;
      Field_1->bit_19 = Field_2.bit_1;
      Field_1->bit_20 = Field_2.bit_3;
      Field_1->bit_21 = Field_2.bit_28;
      Field_1->bit_22 = Field_2.bit_20;
      Field_1->bit_23 = Field_2.bit_21;
      Field_1->bit_24 = Field_2.bit_6;
      Field_1->bit_25 = Field_2.bit_29;
      Field_1->bit_26 = Field_2.bit_8;
      Field_1->bit_27 = Field_2.bit_15;
      Field_1->bit_28 = Field_2.bit_10;
      Field_1->bit_29 = Field_2.bit_12;
      Field_1->bit_30 = Field_2.bit_14;
      Field_1->bit_31 = Field_2.bit_31;
}                                                               

void
Decode(Field_1, Field_2)
      /* Description   :- Copies the bits in Field_2 into Field_1
       *                  in a set pattern (4 byte fields)
       *                  for DECODING 
       *                
       * Return status :- cannot fail
       *                
       *                
       *                
       * Notes         :- FOR use with partner function encode()
       *                  
       *                  
       *                  
       *
       */

WORD *Field_1;     /* <o> 4 byte target bitfield  */
WORD  Field_2;     /* <i>   4 byte source bitfield   */
{
      Field_1->bit_0 = Field_2.bit_0;
      Field_1->bit_25 = Field_2.bit_1;
      Field_1->bit_13 = Field_2.bit_2;
      Field_1->bit_17 = Field_2.bit_3;
      Field_1->bit_30 = Field_2.bit_4;
      Field_1->bit_23 = Field_2.bit_5;
      Field_1->bit_4  = Field_2.bit_6;
      Field_1->bit_18 = Field_2.bit_7;
      Field_1->bit_22 = Field_2.bit_8; 
      Field_1->bit_16 = Field_2.bit_9;
      Field_1->bit_11 = Field_2.bit_10;
      Field_1->bit_19 = Field_2.bit_11;
      Field_1->bit_27 = Field_2.bit_12;
      Field_1->bit_7  = Field_2.bit_13;
      Field_1->bit_24 = Field_2.bit_14;
      Field_1->bit_9  = Field_2.bit_15;
      Field_1->bit_5  = Field_2.bit_16;
      Field_1->bit_26 = Field_2.bit_17; 
      Field_1->bit_2  = Field_2.bit_18;
      Field_1->bit_1  = Field_2.bit_19;
      Field_1->bit_3  = Field_2.bit_20;
      Field_1->bit_28 = Field_2.bit_21;
      Field_1->bit_20 = Field_2.bit_22;
      Field_1->bit_21 = Field_2.bit_23;
      Field_1->bit_6  = Field_2.bit_24;
      Field_1->bit_29 = Field_2.bit_25;
      Field_1->bit_8  = Field_2.bit_26;
      Field_1->bit_15 = Field_2.bit_27;
      Field_1->bit_10 = Field_2.bit_28;
      Field_1->bit_12 = Field_2.bit_29;
      Field_1->bit_14 = Field_2.bit_30;
      Field_1->bit_31 = Field_2.bit_31;
}

void
Jumble(Field_1, Field_2)
      /* Description   :- jumble the bits in Field_2 and Field_1
       *                  in a set pattern symmetrically (4 byte fields)
       *                  swap alernate bits.
       *                
       * Return status :- cannot fail
       *                
       *                
       *                
       * Notes         :- reverse args to unjumble
       *                  
       *                  
       *                  
       *
       */

WORD *Field_1;     /* <i/o> 4 byte bitfield  */
WORD *Field_2;     /* <i/o> 4 byte source    */
{
      WORD temp_1, temp_2;
      
      temp_1 = *Field_1;
      temp_2 = *Field_2;
 
      Field_1->bit_0 = temp_2.bit_0;
      Field_1->bit_2 = temp_2.bit_2;
      Field_1->bit_4 = temp_2.bit_4;
      Field_1->bit_6 = temp_2.bit_6;
      Field_1->bit_8 = temp_2.bit_8; 
      Field_1->bit_10 = temp_2.bit_10;
      Field_1->bit_12 = temp_2.bit_12;
      Field_1->bit_14 = temp_2.bit_14;
      Field_1->bit_16 = temp_2.bit_16;
      Field_1->bit_18 = temp_2.bit_18;
      Field_1->bit_20 = temp_2.bit_20;
      Field_1->bit_22 = temp_2.bit_22;
      Field_1->bit_24 = temp_2.bit_24;
      Field_1->bit_26 = temp_2.bit_26;
      Field_1->bit_28 = temp_2.bit_28;
      Field_1->bit_30 = temp_2.bit_30;

      Field_2->bit_0 = temp_1.bit_0;
      Field_2->bit_2 = temp_1.bit_2;
      Field_2->bit_4 = temp_1.bit_4;
      Field_2->bit_6 = temp_1.bit_6;
      Field_2->bit_8 = temp_1.bit_8; 
      Field_2->bit_10 = temp_1.bit_10;
      Field_2->bit_12 = temp_1.bit_12;
      Field_2->bit_14 = temp_1.bit_14;
      Field_2->bit_16 = temp_1.bit_16;
      Field_2->bit_18 = temp_1.bit_18;
      Field_2->bit_20 = temp_1.bit_20;
      Field_2->bit_22 = temp_1.bit_22;
      Field_2->bit_24 = temp_1.bit_24;
      Field_2->bit_26 = temp_1.bit_26;
      Field_2->bit_28 = temp_1.bit_28;
      Field_2->bit_30 = temp_1.bit_30;
}

