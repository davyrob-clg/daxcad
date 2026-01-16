/* SCCS id Keywords             @(#)  412.1 date 6/11/92 daxgrep.h   */
/*
 * header file for daxgrep regular expression checker for daxcad
 * unashamadely cribbed from doctor dobbs
 */

#define NUL 0x00
#define CR  0x0d
#define SUB 0x1a
#define CPMEOF SUB

#define ISLOWER(c) ('a' <= (c) && (c) <='z')
#define TOUPPER(c) ( ISLOWER(c) ? (c) -('a' - 'A') : (c))

 /* regular expression metachars */

#define BOL '^'
#define EOL '$'
#define ANY '?'
#define LITCHAR 'L'
#define ESCAPE '\\'
#define CCL '['
#define CCLEND ']'
#define NEGATE '^'
#define NCCL '!'
#define CLOSURE '*'
#define OR_SYM '|'


/*
 *  tokens used to hold pattern templates see makepat()
 */

typedef struct token
  {
    char  tok;
    char  lchar;
    char  *bitmap;
    struct token *next;
  } TOKEN;

static TOKEN *template = 0; 

#define TOKSIZE sizeof(TOKEN)

#define MAXSTR 1024  /* maximum no of chars in a string */

extern char *tools_matchs();
extern char *tools_amatch();
extern int tools_esc();
extern char *tools_dodash();
extern TOKEN *tools_makepat();
extern void  tools_unmakepat();
extern void  tools_makpat();
extern void  tools_match();

#define MAX(x, y)  (((x) < (y)) ? (y) : (x))
#define MIN(x, y)  (((x) < (y)) ? (x) : (y))

