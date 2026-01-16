/*
     @(#)  412.1 date 6/11/92 opcodes.h 


     Filename    : opcodes.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:37:07
     Last change : 92/06/11 14:37:06

     Copyright : Practical Technology International Limited
     File :- opcodes.h

     C include file

     All opcodes and what they mean also associated priority
     1 -> oo is ascending priority Thus all functions have 
     priority 10 * has priority 1

*/


#define MAXOPS 10

static struct {
               char opcode[7];     /* the opcode */
               int priority;       /* Execute priority */
               int args;           /* arguments */      
               int func;           /* function code */
              } OpLookup[MAXOPS] = {
                                     "(",0,0,0,
                                     ")",0,0,0,
                                     "*",4,2,0,
                                     "/",3,2,0,
                                     "+",2,2,0,
                                     "-",1,2,0,
                                     "-",1,1,1,
                                     "+",1,1,1,
                                     ",",1,0,0,
                                     "func1",0,3,1 };
 

#define LEFTBRACKET  0      /* all descriptions fit into an array */
#define RIGHTBRACKET 1
#define MULTIPLY     2
#define DIVIDE       3
#define ADD          4
#define SUBTRACT     5
#define MINUS        6
#define PLUS         7
#define COMMA        8
#define FUNC1        9



