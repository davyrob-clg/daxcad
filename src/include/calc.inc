C
C     @(#)  412.1 date 6/11/92 calc.inc 
C
C
C     Filename    : calc.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:14:57
C     Last change : 92/06/11 14:24:52
C
C     Copyright : Practical Technology Limited  
C     File :- calc.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C1    This include file contains the definitions
C1    of all variable storage required by the 
C1    calculator function.
C1    It has been modified for IBM 6000 compiler and X conversion
C
      INTEGER VARL(500),MAXV,NVARS
      DOUBLE PRECISION VARD(500)
      CHARACTER*20 VARS(500)
C
      COMMON /CALC1/MAXV,NVARS,VARL,VARD
      COMMON /CALC2/VARS
C
      INTEGER OSL,DSL,NOPS,NOFS
C     set the stack sizes
C
      PARAMETER (OSL=40, DSL=30, NOPS=61, NOFS=NOPS-10)
      INTEGER PREC(0:NOPS),OSTK(0:OSL),DP,OP
      DOUBLE PRECISION DSTK(1:DSL)
C     DSTR is the numeric character set
      COMMON /CAEXPR/DSTK,DP,OSTK,OP 
C
C     these are used to communicate charcter information
C     from the routine aexpression and related routines
C     see routine CEXPRN for their main usage
C
      INTEGER*4 CAP,CSP,LENSTR
      CHARACTER CARST(0:30)*80
      CHARACTER CARNAM(100)*20
      CHARACTER OPTYPE*2
      CHARACTER RESULT*2
      CHARACTER LOC*2
      CHARACTER SBUF*80
      CHARACTER CARR(100)*80
C
      LOGICAL CERR
C
      COMMON/VTYPE/RESULT,LOC,SBUF,CARR,CARST,CAP,CSP,CARNAM,OPTYPE
C
      COMMON/VTYPE4/ CERR,
     +               LENSTR
C
C     the result and optype variables works as follows
C     result is the final outcome defined in an assignment
C     statement. If no assignment then the result will be
C     the last optype. If they dont match up then an error
C     will be generated
