C
C     @(#)  412.1 date 6/11/92 dimendat.inc 
C
C
C     Filename    : dimendat.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:07
C     Last change : 92/06/11 14:28:37
C
C     Copyright : Practical Technology Limited  
C     File :- dimendat.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
C2    This common block section holds data used by Dimensioning code.
C2    Standards used :-
C2    British Standard BS308 - yes if BS = .TRUE.
C2    ANSI Y14.5 -  yes if ANSI = .TRUE.
C2    DIN - yes if DIN = .TRUE.
C2    METRIC = .TRUE. if metric units used.
C2
C2    LTOL=lower tolerance value
C2    UTOL=upper tolerance value
C2    
C2    RTMP= default outer arrow length
C2    
C2    LOGICAL FLAG CONTROL
C2    ====================
C2    DOPFLG(1)=.TRUE. if Point to Point mode selected.                             
C2    DOPFLG(2)=.TRUE. if Along line mode selected.                          
C2    DOPFLG(3)=.TRUE. if Horizontal LOck/Minutes mode applied.
C2    DOPFLG(4)=.TRUE. if Vertical lock/Second mode applied.
C2    DOPFLG(5)=.TRUE. if Align with other dimension asked for.                                
C2    DOPFLG(6)=.TRUE. if Unrelated Dimension,Text added or overwrittn
C2    DOPFLG(7)=.TRUE. if Suppress left witness line.
C2    DOPFLG(8)=.TRUE. if Suppress right witness line.                              
C2    DOPFLG(9)=.TRUE. if outer dimension lines requested.                  
C2    DOPFLG(10)   
C2    DOPFLG(11)=.TRUE. if offset text.
C2    DOPFLG(12)=.TRUE. if outer dimension text.                                
C2    DOPFLG(13)=.TRUE. if Major angle requested.
C2    DOPFLG(14)=                                                  
C2    DOPFLG(15)=.TRUE. if diametral/radial dimension is leader type.           
C2    DOPFLG(16)=.TRUE. if outer text to left of dimension.                                                                                   
C2    DOPFLG(17)=.TRUE. if Supress left dimension line.                                                     
C2    DOPFLG(18)=.TRUE. if Supress right dimension line.                                                     
C2    DOPFLG(19)=.TRUE. if supress correct dimension text.                                                      
C2    DOPFLG(20)=.TRUE. if supress any projection lines.                                                     
C2
C2    DATA RECORDS CONTROL
C2    ====================
C2    RTALLY(1)=No. of original hit points data records
C2    RTALLY(2)=No. of text records following.
C2    RTALLY(3)=No. of arrowhead records following.
C2    RTALLY(4)=No. of left dimension line records.
C2    RTALLY(5)=No. of right dimension line records.
C2    RTALLY(6)=No. of left witness line records,
C2    RTALLY(7)=No. of right witness line records,
C2    RTALLY(8)=No. of left extension line records.
C2    RTALLY(9)=No. of right extension line records.
C2    RTALLY(10)=No. of original text data records.
C2 
C2    CONTROL DATA SECTION
C2    ====================
C2    CTRDAT(1)=MIP held data : std used,related scaled & toled dimn.
C2    CTRDAT(2)=Dimn header : text & dimn posn, dimn dirn, no. records.                              
C2    CTRDAT(3)=Text record : no. text records & suppress state.
C2    CTRDAT(4)=Arrow record : no. & form of arrows & suppres state.
C2    CTRDAT(5)=segment recd : no. & form of & suppres state.
C2    CTRDAT(6)=Original data : tolernce type & special symbol used.
C2
C2    DATA  ARRAYS
C2    ============
C2    DIMCHR(6)  =dimension text strings
C2    DIMCON(6,2)=dimension header control data
C2    DIMARR(6,6)=dimension text part data.
C2    DIMARR(6,4)=dimension arrow data.
C2    DIMLNL(6,5)=left dimension line data
C2    DIMLNR(6,5)=right dimension line data.
C2    DIMWTL(6,5)=left witness line data.
C2    DIMWTR(6,5)=right witness line data.
C2    DIMEXL(6,5)=left extension line data.
C2    DIMEXR(6,5)=right extension line data.
C2    
C2    TOLERANCE TYPES :-
C2      TOLTYP1=No tolerance
C2            2=Unilateral
C2            3=Bilateral
C2            4=Basic Sizes
C2            5=Add text to nominal size
C2            6=Overwrite dimension text
C2
C2    SPECIAL SYMBOL  TYPES :-
C2       SPCSYM 0=no symbol
C2       SPCSYM 1=Diameter
C2              2=radius
C2              3=Degree
C2              4=Degree & Minutes
C2              5=Degree & Minutes & Seconds.
C2                             
C2   LEDIT is set TRUE if EDITING a label

      LOGICAL BS,DIN,ANSI,METRIC,IMP,DOPFLG(20)                         
      LOGICAL LEDIT,CEDIT
      INTEGER*4 TOLTYP,RTALLY(10),SPCSYM,NUMSTR
      INTEGER*2 CTRDAT(10)
      REAL UTOL,LTOL,DIMCON(6,10),DIMARR(6,10),DIMLNL(6,10),
     +               DIMLNR(6,10),DIMWTL(6,10),DIMWTR(6,10),
     1               DIMEXL(6,10),DIMEXR(6,10),DIMTXT(6,10),RTMP
      CHARACTER DIMCHR(15)*80
C      
      COMMON /DIMDAT/BS,DIN,ANSI,METRIC,IMP,DOPFLG,
     +                 TOLTYP,LTOL,UTOL,RTALLY,DIMCON,
     1                 DIMARR,DIMLNL,DIMLNR,DIMWTL,DIMWTR,DIMEXL,
     3                 DIMEXR,DIMTXT,SPCSYM,NUMSTR,RTMP
C
      COMMON /DMCHER/DIMCHR
      COMMON /EDIT/ LEDIT,CEDIT
	  COMMON /CTRCOM/CTRDAT
*
