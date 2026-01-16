
/*      @(#)  412.1 date 6/11/92 pattern.c 
 *
 *
 *        Filename    : pattern.c
 *        Version     : 412.1
 *        Retrieved   : 92/06/12 15:47:35
 *        Last change : 92/06/11 14:37:24
 *
 *        Copyright : Practical Technology Int Limited 1991
 */       

/*
      Pattern making and control for DAXTOOLS taken from an idea
      by Brian Kernigham designed by Dr Dobbs and typed in by 
      John lawson.

*/

#include <stdio.h>
#include "daxgrep.h"

/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
void
tools_makpat(pattern, length)
 
                        /* Description   :- makes a grep template for pattern
                         *                  expects to be called from FORTRAN
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */

 char pattern[];    /* < i > string to be searched for */
 int *length;       /* < i > active length of string */
{
   char string[MAXSTR];
   int i, maxlen = MAXSTR-1; 

   maxlen = MIN(maxlen,*length); /* check and assign active length */

   for (i=0; i<maxlen; ++i)
       string[i] = pattern[i]; /* copy string */

   string[i] = '\0';       /* terminator null */

   if(template) tools_unmakepat(template); /* free up old template */

   template = tools_makepat(string, '\0'); /* right then make it up */
} 
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
void
tools_match(string, length, end, index)
 
                        /* Description   :- checks if  the pattern set up in makpat
                         *                  is in the string, if it is not in the string
                         *                  then index is set to 0. If it is in the string 
                         *                  then it's index position is returned.
                         *                  If end is set 0 then the index position will
                         *                  be the index of the first char in the pattern match. 
                         *                  If end is set non 0 then the index position will
                         *                  be the index of the last char in the pattern match 
                         *
                         *                  Note that index indexs from 1 a la FORTRAN
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */

 char string[];    /* < i > string to be searched */
 int *length;      /* < i > active length of string */
 int *end;         /* < i > first or last char flag     */
 int *index;       /* < o > first or last character to match in pattern */

{
   char buffer[MAXSTR], *posn;
   int i, maxlen = MAXSTR-1; 
   maxlen = MIN(maxlen,*length); /* check and assign active length */

   for (i=0; i<maxlen; ++i)
       buffer[i] = string[i]; /* copy string */

   buffer[i] = '\0';       /* terminator null */

   posn = tools_matchs(buffer, template, *end);

   if(posn) 
     *index =(posn - buffer) +1;
   else
     *index = 0;
}       
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
char
*tools_amatch (lin, pat, boln)
 
                        /* Description   :- checks lin to see it contains pat and
                         *                  if it does pointer to last char in pattern
                         *                  is returned otherwise null.
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 char *lin;    /* < i > string to be searched */

 TOKEN *pat;   /* < i > pattern to be checked for */
 
 char *boln;   /* < o > beginning of the matched pattern */

{
  char *bocl, *rval, *strstart;

  if(pat == 0)
        return ((char *)0);

  strstart = lin;

  while( pat )
  {
      if(pat->tok == EOL && *lin == '\0')
       {
         pat = pat->next; /* hack for $ on string with no '\n' char */
       }
      else if(pat->tok == CLOSURE && pat->next)
       {
         /* process a closure */

         pat = pat->next;

         /* now match any possible closures */

         bocl = lin;

         while(*lin && tools_omatch(&lin, pat, boln))
                ;

         /* lin now points to the character that made us fail */

         if ( pat = pat->next)
            {
               while(bocl <= lin)
                    {
                     if (rval = tools_amatch(lin, pat, boln))
                        {
                           /* success */
                           return (rval);
                        }
                     else
                          --lin;
                    }
                    return (0);   /* match failed */
            }
        }
       else if (tools_omatch(&lin,pat,boln))   
        {
           pat = pat->next;
        }
       else
        {
          return(0);
        }
  }

  --lin;
    return( MAX(strstart, lin));
} 
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
void
tools_setbit (c,field)
 
                        /* Description   :- sets bit in ascii bit map corresponding to the char c.
                         *                  Field must be at least 16 bytes long. 
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 int c;    /* < i > input character */
 char field[]; /* <i/o> ASCII BITMAP */

{
  field[(c & 0x7f)>>3] |= 1 << (c & 0x07);
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
int
tools_testbit (c,field)
 
                        /* Description   :-check if bit corresponding to c in field is set 
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 int c;    /* < i > input character */
 char field[]; /* <i/o> ASCII BITMAP */

{
  return (field[(c & 0x7f)>>3] & 1 << (c & 0x07));
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
char
*tools_dodash (delim, src, map)
 
                        /* Description   :- expands a character class range
                         *                  e.g. a-Z
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 int delim;    /* < i > string to be searched */
 char *src;    /* < i >  set to be expanded    */
 char *map;    /* < i > expanded string       */


{
   int first, last;
   char *start=src;


    while(*src && *src != delim)
     {
        if( *src != '-')
                tools_setbit(tools_esc(&src), map);
        else if(src == start || *(src+1) == delim)
               tools_setbit('-',map);
        else
       {
             src++;

             if(*src < *(src - 2))
               {
                 first = *src;
                 last = *(src-2);
               }
             else
               {
                 first = *(src-2);
                 last = *src;
               }
          
             while(++first <= last)
                 tools_setbit(first,map);

             src++;
       }
     }
     return (src);
}
 
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
int
tools_esc(s)
 
                        /* Description   :- map escape sequences into correct sequences
                         *                       
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 char **s;    /* < i > string to be mapped */
{
   int rval;

   if(**s != ESCAPE)
            rval = *((*s)++);
   else
     {
       (*s)++;
       
       switch( TOUPPER(**s) )
       {
        case '\0': rval = ESCAPE;  break;
        case 'B': rval = '\b';     break;
        case 'F': rval = '\f';     break;
        case 'N': rval = '\n';     break;
        case 'R': rval = '\r';     break;
        case 'S': rval = ' ';      break;
        case 'T': rval = '\t';     break;
        default:  rval = **s;      break;
       }
  
       (*s)++;
     }

  return rval;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
TOKEN
*tools_makepat(arg, delim)

 
                        /* Description   :- makes a pattern template from the string
                         *                  pointed to by arg delimited by delim, \0 or \n
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 char *arg;    /* < i > string to be searched */
 int  delim;   /* < i > string delimiter      */

{
  TOKEN *head, *tail;
  TOKEN *ntok;
  int error;

 /*  check  for illegal characters at the beginning of the template */

 if(*arg == '\0' || *arg == delim || *arg == '\n' || *arg == CLOSURE)
   return(0);

 error = 0;
 head  = 0;
 tail  = 0;

 for(; *arg && *arg != delim && *arg != '\n' && !error; arg++)
    {
           ntok =(TOKEN *) malloc(TOKSIZE);

           if(error = !ntok) break; /* malloc knackered */


           switch(*arg)
           {
             case ANY:
                     ntok->tok = ANY;
                     break;
             
             case BOL:

                     if(head==0) /* then this is the first symbol */
                   
                            ntok->tok = BOL;
             
                     else
                      
                            error = 1;

                     break;
             case EOL:
                     if(*(arg+1) == delim || *(arg+1) == '\0'
                                          || *(arg+1) == '\n')
                         ntok->tok = EOL;
                     else
                         error = 1;
                     break;
             case CCL:
                     if(*(arg+1) == NEGATE)
                     {
                         ntok->tok = NCCL;
                         arg += 2;
                     }
                     else
                     {
                         ntok->tok = CCL;
                         arg++;
                     }

                     if(ntok->bitmap = (char *) calloc(16,1))
                     {
                        arg = tools_dodash(CCLEND, arg, ntok->bitmap);
                     }
                     else
                     {
                        error = 1;
                     }

                     break;

             case CLOSURE:
                     switch(tail->tok)
                     {
                        case BOL:
                        case EOL:
                        case CLOSURE:
                           return(0);
                        default:
                           ntok->tok = CLOSURE;
                     }
                     break;

             default:
                    ntok->tok = LITCHAR;
                    ntok->lchar = tools_esc(&arg);
                    --arg; /* escape advanced us past the char */
             }


             if(error)
               {
                tail->next = ntok;
                ntok->next = 0;
                tools_unmakepat(head);
                return 0;   
               }
             else if (head == 0)
               {
                /* this is the first node in the chain
                 */

                 ntok->next = 0;
                 head = tail = ntok;
               }
             else if(ntok->tok != CLOSURE)
               {
                 /* insert at end of list (after tail) */

                 tail->next = ntok;
                 ntok->next = tail;
                 tail = ntok;
               }
             else if(head != tail)
               {
                /* more than one node in the chain. Insert the closure node
                 * immediately in front of the tail
                 */ 
                     
                 (tail->next)->next = ntok;
                 ntok->next = tail;
               }
             else
               {
                 /* Only one node in the chain, Insert the CLOSURE
                  * node at the head of the linked list
                  */

                  ntok->next = head;
                  tail->next = ntok;
                  head = ntok;
               }
 
   }
 tail->next = 0;
 return head;

}
                    
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
char
*tools_matchs (line, pat, ret_endp)
 
                        /* Description   :- compares line and pattern. Line is a
                         *                  character string while pat is a pattern
                         *                  template made by getpat().
                         *
                         * Returns :-    1. NULL if no match found
                         *               2. A pointer to the last character sataisfying the match
                         *                  if ret_endp is non zero.
                         *               3. A pointer to the beginning of the matched string
                         *                  if ret_endp is 0; 
                         *
                         * 
                         * Examples:-   tools_matchs("1234567890",getpat("4[0-9]*7"),0);
                         *              will return a pointer to the '4'
                         *
                         *              tools_matchs("1234567890",getpat("4[0-9]*7"),1);
                         *              will return a pointer to the '4'
                         * 
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 char *line;    /* < i > string to be searched */
 TOKEN *pat;   /* < i > pattern to be matched */
 int ret_endp; /* < i > start or end flag     */

{
    char *rval, *bptr;

    bptr = line;

    while(*line)
    {

      if((rval = tools_amatch(line, pat, bptr)) == 0)
      {
        line++;
      }
      else
      {
            rval = ret_endp ? rval : line;
            break;
      }
    }
    return rval;
}
/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
int
tools_omatch (linp, pat, boln)
 
                        /* Description   :- Match one pattern element, pointed at by pat with the
                         *                  character at **linp. Return non - zero on match.
                         *                  otherwise, return 0. *linp is advanced to skip over 
                         *                  the matched character; it is not advanced on failure.
                         *                  The amount of advance is 0 for patterns that match null
                         *                  strings, 1 otherwise. "boln" should point at the position
                         *                  that will match a BOL token.
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 char **linp;     /* < i > string to be searched */
 TOKEN *pat;      /* <i>   pattern to be searched for  */
 char *boln;      /* < i > beginning of pattern  */

{
  int advance = -1;


  if(**linp)
  {
    switch(pat->tok)
    {
     case LITCHAR:
              if(**linp == pat->lchar)
                  advance =1;
              break;

     case BOL:
             if(*linp == boln)
                 advance = 0;
             break;

     case ANY:
             if ( **linp != '\n')
                 advance = 1;
             break;

     case EOL:
             if(**linp == '\n')
                 advance = 0;
             break;

     case CCL:
             if(tools_testbit(**linp, pat->bitmap))
                advance=1;
             break;

     case NCCL:
             if(!tools_testbit(**linp, pat->bitmap))
                advance=1;
             break;

     default:
              ;
    }    
  }
  
  if (advance >= 0)
      *linp +=advance;

  return ++advance;
} 

/*
 *===================================================================================*
 *
 *                                                            Author :: J. Lawson
 *                                                            Date   :: 19/05/89
 *
 */
void
tools_unmakepat(head)

 
                        /* Description   :-  free up memory  used for tokrn string
                         *
                         *
                         * Externals     :- daxgrep.h
                         *
                         * Notes :-
                         *
                         */
 TOKEN *head;     /* < i > start of token structure */
{
   TOKEN *old_head;

   while (head)
   {

           switch(head->tok)
           {
             case CCL:
             case NCCL:
                      free(head->bitmap);
                      /* no break, fall thru' to default */

             default:
                    old_head = head;
                    head = head->next;
                    free(old_head);
                    break;
            }
   }
}
