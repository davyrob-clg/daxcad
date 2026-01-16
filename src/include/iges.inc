C
C     @(#)  412.1 date 6/11/92 iges.inc 
C
C
C     Filename    : iges.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:17
C     Last change : 92/06/11 14:31:49
C
C     Copyright : Practical Technology Limited  
C     File :- iges.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C
      CHARACTER  EOV*1,EOR*1,BIGBUF*16000,IGLABL*8,IGFNAM*80
      CHARACTER*80 AUTHOR,COMPNY,IGUNTD*8
      LOGICAL ALLVWS,EXPCEN
C
      COMMON /IGES1/EOV,EOR,BIGBUF,IGLABL,IGFNAM,AUTHOR,COMPNY,
     +              IGUNTD,ALLVWS,EXPCEN
C
      INTEGER*4 IGDUNT,IGPUNT,IGUNIT,IGEUNT,IGSUNT,BIGWP,BIGNL,IGERRS,
     +          BIGBER,IGDDAT(20),IGSTAT(4),NIGDIP,NIGPAP,IGVERS,IGDSTD,
     +          LINSUB,ARCSUB,HATSUB,TXTSUB,IGDFP,IGPFP,IGDSEQ,IGPSEQ,
     +          IGSSEQ,IGGSEQ,IGTSEQ,NIGTHK,BIGSIZ,INTBIT,MPTENR,
     +          NSIGR,MPTEND,NSIGD,IGMUNT,IGETYP,IGCOLR,IGFORM,
     +          IGLAYR,IGFONT,IGTHIK,IGDIRP,IGSTKS,IGSTKP,
     +          VWSLST(20),VWTOTL,VWCUR,NST408
C
      PARAMETER(BIGSIZ=16000)
C
      REAL THIKIG
C
      COMMON/IGES2/IGDUNT,IGPUNT,IGUNIT,IGEUNT,IGSUNT,BIGWP,BIGNL,
     +             IGERRS,BIGBER,IGDDAT,IGSTAT,NIGDIP,NIGPAP,IGVERS,
     +             IGDSTD,LINSUB,ARCSUB,HATSUB,TXTSUB,IGDFP,IGPFP,
     +             IGDSEQ,IGPSEQ,IGSSEQ,IGGSEQ,IGTSEQ,NIGTHK,THIKIG,
     +             INTBIT,MPTENR,NSIGR,MPTEND,NSIGD,IGMUNT,
     +             IGETYP,IGCOLR,IGFORM,IGLAYR,IGFONT,IGTHIK,IGDIRP,
     +             IGSTKS,IGSTKP,VWSLST,VWTOTL,VWCUR,NST408
C
C2    IGSUNT  contains the unit number of the stack file
C2    IGSTKS  contains the size of the stack in use
C2    IGSTKP  contains the pointer to the current stack entry
C2    EOV     contains the field delimiter (default ',')
C2    EOR     contains the record delimiter (default ';')
C2    BIGBUF  contains the current string of chars
C2    BIGWP   contains the current pointer into BIGBUF
C2    BIGNL   contains active length of BIGBUF
C2    BIGBER  contains end of record position in BIGBUF
C2    BIGSIZ  contains size of bigbuf in bytes
C2    NIGDIP  contains pointer to next directory entry
C2    NIGPAP  contains pointer to next parameter entry
C2    IGDIRP  contains pointer to current independant entity
C2    IGDDAT  contains current directory entry data
C2    IGLABL  contains current entity label
C2    IGSSEQ  contains current start sequence number
C2    IGGSEQ  contains current global sequence number
C2    IGDSEQ  contains current directory sequence number
C2    IGPSEQ  contains current parameter sequence number
C2    IGTSEQ  contains current terminate sequence number
C2    IGVERS  contains the IGES version number in use
C2    IGDSTD  contains the IGES Drafting Standard code
C2    LINSUB  contains current LINE subscript
C2    ARCSUB  contains current ARC  subscript
C2    HATSUB  contains current HATCH  subscript
C2    TXTSUB  contains current TEXT subscript
C2    IGDFP   contains current pointer into directory scratch file
C2    IGPFP   contains current pointer into parameter scratch file
C2    IGERRS  contains the count of errors in translation
C2    NIGTHK  number of thickness indexes available
C2    THIKIG  Thicknes in world units per thickness index
C2    AUTHOR  contains name of sender of data
C2    COMPNY  contains name of sender's organization
C2    INTBIT  contains number of bits in an Integer
C2    MPTENR  contains Maximum Power of Ten in Real number
C2    NSIGR   contains Number of SIGnificant digits in Real number
C2    MPTEND  contains Maximum Power of Ten in Double number
C2    NSIGD   contains Number of SIGnificant digits in Double number
C2    IGMUNT  contains IGES Model UNiTs flag value
C2    IGUNTD  contains IGES model UNITs Descriptor
C2    IGETYP  is the current IGES ENTITY type
C2    IGCOLR  is the current entity COLOUR
C2    IGFORM  is the current entity FORM number
C2    IGSTAT  is the current entity STATUS
C2    IGLAYR  is the current entity LAYER
C2    IGFONT  is the current entity FONT
C2    IGTHIK  is the current entity THICKNESS index
C2    VWSLST  list of the directory entries of all views in the file.
C2    VWTOTL  number if entries in VWLIST.
C2    ALLVWS  this flag forces all views into the same drawing.
C2    VWCUR   The view currently being translated.
C2    EXPCEN  This flag switches the centre line code to explode the
C2            data into individual lines.
C
      LOGICAL TFMFLG
      REAL GNLTFM(4,3)
C
C2    TFMFLG  Is a general transform currently active.
C2    GNLTFM  The general transform.
C
      COMMON/IGES3/GNLTFM
      COMMON/IGES4/TFMFLG



