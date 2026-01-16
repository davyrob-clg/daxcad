C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writedxf2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DXFDEFATTRB(OUNIT,CODE,OK)
C     SUBROUTINE DXFWRITEPARC(OUNIT,OK)
C
C     |-----------------------------------------------------------------|
C

        SUBROUTINE DXFDEFATTRB(OUNIT,CODE,OK)
C       =====================================
C1      VARYPE                  I4    I4  L
C1      IOSTAT                  I     I   O
C
C2      Defines an attribute for a master defintion.
C2      This enables us to write the pathname associated with
C2      the block. If we then read it back in then the path can be restored.
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/dxf.inc'
C
      INTEGER*4 OUNIT
      INTEGER*4 CODE
      INTEGER*4 NLEN
      INTEGER*4 POS
      INTEGER*4 BINDEX
      LOGICAL OK
C
      EXTERNAL NLEN
      EXTERNAL BINDEX
C
      OK = .FALSE.
      IF(CODE.EQ.1) THEN
C         pathname option.
          POS = BINDEX(CBUFF,'/')
          IF ( POS.EQ.0) THEN
C             no path no need for attribute.
              GOTO 999
 
          ENDIF
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'ATTDEF'
C         set style and color to 0
          IMBUFF(6) = 0
          IMBUFF(3) = 0
C         layer number code
          CALL DXFWLN(OUNIT)
C
C         text insertion point and height ( NULLED not needed )
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0.0'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0.0'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '3.0'
C
          CALL FOLDUP(CBUFF)
C         actual pathname to be used.
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '1'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) CBUFF(1:POS)
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '3'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'New Pathname:'
C
C         attribute tag here
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '2'
          IF(IMBUFF(2).EQ.COMPM) THEN
              WRITE(UNIT=OUNIT,FMT='(2A)',ERR=999)
     +                 'DAXCMP',
     +                CBUFF(1:NLEN(CBUFF))
          ELSEIF(IMBUFF(2).EQ.SYMBM) THEN
              WRITE(UNIT=OUNIT,FMT='(2A)',ERR=999)
     +                 'DAXSYM',
     +                CBUFF(1:NLEN(CBUFF))
          ENDIF
C
 
C         attribute flag Set here for invisible and variable values.
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '3'
C
C         text alignamet flag
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '72'
          WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
 
          OK = .TRUE.
          RETURN
C
      ENDIF
 
999   CONTINUE
      OK = .FALSE.
      END
 
 
 
 
 
      SUBROUTINE DXFWRITEPARC(OUNIT,OK)
C     =================================
C1    VARYPE                   I4   L
C1    IOSTAT                   I    O
C
C2    Writes a polyline out to autocad DXF format
C2    Should handle thick lines in DAXCAD. Assumes that
C2    arc has been read and buffers contain info
C2    * NB *  BULGE factor in AUTOCAD is defined as follows:-
C2     TANGENT of 1/4 of the included angle in radians
C
      include  'include/product.inc'
      include  'include/lfont.inc'
      include  'include/wtov.inc'
      include  'include/nbuff.inc'
      include  'include/ndata.inc'
      include  'include/dxf.inc'
C
      INTEGER*4 OUNIT
      INTEGER*4 THK
      INTEGER*4 END
      REAL TOT
      REAL BULGE
      REAL L1,L2,L3
      REAL EXT
      REAL XT1,YT1
      REAL XT2,YT2
      REAL RADIUS
      REAL PI
      INTEGER OPEN
      LOGICAL SAMEA
      LOGICAL OK
      LOGICAL CIRCLE
C
      EXTERNAL PI
      EXTERNAL SAMEA
C
      OPEN = 0
      OK = .FALSE.
      THK=PLTHKI(IMBUFF(12))
      IF ( THK.EQ.0 ) THEN
C         do not draw the line
          OK = .TRUE.
          GOTO 999
      ENDIF
      
      TOT=ABS(PAPTOW*LTHKR(1,MOD(THK,1024)))
C
      CIRCLE=SAMEA(ABS((RDBUFF(6)-RDBUFF(5))),PI(2.0))
C     get start point

C
      IF(CIRCLE) THEN
         RADIUS = RDBUFF(4)
         XT1 = RDBUFF(1)-RADIUS
         YT1 = RDBUFF(2)
         XT2 = RDBUFF(1)+RADIUS
         YT2 = RDBUFF(2)
C        bulge factor 1
         BULGE = 1.0
         OPEN = 1
      ELSE
         XT1 = COS(RDBUFF(5))*RDBUFF(4)+RDBUFF(1)
         YT1 = SIN(RDBUFF(5))*RDBUFF(4)+RDBUFF(2)
         XT2 = COS(RDBUFF(6))*RDBUFF(4)+RDBUFF(1)
         YT2 = SIN(RDBUFF(6))*RDBUFF(4)+RDBUFF(2)
C        normal bulge factor
         BULGE = TAN((RDBUFF(6)-RDBUFF(5))/4)
         OPEN = 0
      ENDIF
C
C     header
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'POLYLINE'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
C     vertex follows flag
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '66'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '1'

      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(I1)',ERR=999) OPEN
C
C     thick line start and end width
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT1
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT1
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
 
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) BULGE
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'VERTEX'
C
C     layer color etc
      CALL DXFWLN(OUNIT)
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '10'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) XT2
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '20'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) YT2
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '40'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '41'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) TOT
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '70'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '42'
      WRITE(UNIT=OUNIT,FMT='(G15.6)',ERR=999) BULGE
C
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) '0'
      WRITE(UNIT=OUNIT,FMT='(A)',ERR=999) 'SEQEND'
 
      OK = .TRUE.
      RETURN
999   CONTINUE
      OK = .FALSE.
C
 
      END
C
 
 
C
C
 
