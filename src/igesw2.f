C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 igesw2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE IGW033(ENTYP)
C     SUBROUTINE IGWL01(ARROW,AHW,SEGM,NS,WITNES,NTS,TA,VISI)      

C     |-----------------------------------------------------------------|
C

      SUBROUTINE IGW033(ENTYP)
C     =======================
C1    vartype            I4
C1    iostatus           I
C
C2    Linear Dimension entity. Write out two leader (Arrow) entities
C2    0 1 or 2 witness lines
 
C
      include  'include/iges.inc'
      include  'include/nbuff.inc'
      include  'include/wrkdat.inc'
      include  'include/entity.inc'
      include   'include/dimendat.inc'
C
      INTEGER*4 ENTNUM,IGDP,I,IGPP,COLOR,THICK,J,LEAD,WIT,IC,LINDIM
      INTEGER*2 CPDP,ENTYP
      REAL ARROW(2,2),SEGM(10,7),WITNES(2,4),AHW(2,2),WITX,WITY
      INTEGER*4 AC,WL,LEADP(2),WITNP(2),GENNP,ARCDIM,NSEG(2),NTS,TA(5)
      INTEGER*4 FONT,GENOTE,RADDIM,DIADIM,NUML
      LOGICAL OK,SAME,TE,VISI(5)
      DATA  LEAD/214/,WIT/106/,LINDIM/216/,ARCDIM/202/,GENOTE/212/
      DATA  RADDIM/222/,DIADIM/206/
C
C     Initialise pointers
      GENNP=0
      LEADP(1)=0
      LEADP(2)=0
      WITNP(1)=0
      WITNP(2)=0
      NUML=2
C     decode arrow segement and witnes parameters into something decent
      CALL IGWL01(ARROW,AHW,SEGM,NSEG,WITNES,NTS,TA,VISI)
C     Write out the number of leader entities and store pointers
      IF(ENTYP.EQ.RDIMN) NUML=1
      DO 120 IC=1,NUML
C         initialise the directory entry
          CALL IGWD00(IMBUFF,IGDP)
C         store directory pointer of leader
          LEADP(IC)=IGDP
          CALL IGWD01(LEAD,'  LEADER',LINSUB)
C         set the form number of the arrow head
          CALL IGWD03(2)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(I,THICK,COLOR)
          IF(VISI(IC)) THEN
C         status type entity
              CALL IGWD04(0,1,1,0)
          ELSE
C         status type entity
              CALL IGWD04(0,1,1,1)
          ENDIF
C         initialize parameter block for entity
          CALL IGWP00(LEAD)
C         write parameter data to buffer
C         number of segemments (1 per arrowhead )
          CALL IGAIBF(NSEG(IC))
D          WRITE(10,*) '[IGW033] AHW(1)= ',AHW(IC,1)
D          WRITE(10,*) '[IGW033] AHW(2)= ',AHW(IC,2)
C         arrowhead height
          CALL IGARBF(AHW(IC,1))
C         arrowhead width
          CALL IGARBF(AHW(IC,2))
C         Z depth (0)
          CALL IGARBF(0)
C         Arrowhead coordintes
          CALL IGARBF(ARROW(IC,1))
          CALL IGARBF(ARROW(IC,2))
C         segement tail
          CALL IGARBF(SEGM(IC,1))
          CALL IGARBF(SEGM(IC,2))
C         extra segements attached to this arrow head or tail ?
D      WRITE(10,*) '[IGEWO1] NSEG(IC)= ',NSEG(IC)
          IF(NSEG(IC).GT.1) THEN
              DO 60 I=2,NSEG(IC)
                  CALL IGARBF(SEGM(1+I,1))
                  CALL IGARBF(SEGM(1+I,2))
 60           CONTINUE
          ELSE
          ENDIF
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
C         loop back
120   CONTINUE
C     ******************************************
C     witnes line directory entry if any !
C     ******************************************
      DO 70 IC=1,2
      IF(VISI(IC+2)) THEN
C         write out the witnes lines
C         initialise the directory entry
          CALL IGWD00(IMBUFF,IGDP)
C         store directory pointer of leader
          WITNP(IC)=IGDP
          CALL IGWD01(WIT,' WITNESS',LINSUB)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(I,THICK,COLOR)
C         set the form number
          CALL IGWD03(40)
C         status type entity
          CALL IGWD04(0,1,1,0)
C         initialize parameter block for entity
          CALL IGWP00(WIT)
C         write parameter data to buffer
C         interpretation flag
          CALL IGAIBF(1)
C         number of trpilets
          CALL IGAIBF(3)
C         z displacement
          CALL IGAIBF(0)
C         set buffer varaiables
          WITX=WITNES(IC,1)
          WITY=WITNES(IC,2)
C         witnes gap point coincident avec geometry de witness line
          CALL IGARBF(WITX)
          CALL IGARBF(WITY)
C         1st point of witnes line
          CALL IGARBF(WITX)
          CALL IGARBF(WITY)
C         set buffer varaiables
          WITX=WITNES(IC,3)
          WITY=WITNES(IC,4)
C         2nd point of witnes line
          CALL IGARBF(WITX)
          CALL IGARBF(WITY)
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ENDIF
 70   CONTINUE
C     ******************************************
C     general note entity storage
C     ******************************************
C         initialise the directory entry
      IF(VISI(5)) THEN
          CALL IGWD00(IMBUFF,IGDP)
C         store the general note pointer
          CALL IGWD01(GENOTE,'GEN_NOTE',LINSUB)
          GENNP=IGDP
C         write out the text entity
          FONT=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(FONT,THICK,COLOR)
C         status type entity
          CALL IGWD04(0,1,1,0)
C         initialize parameter block for entity
          CALL IGWP00(GENOTE)
C         write parameter data to buffer
C         write number of text strings
          CALL IGAIBF(NTS)
          DO 80 IC=1,NTS
              IF(IWORK(4,TA(IC)).GT.0) THEN
                  DO 85 I=1,6
                      RDBUFF(I)=RWORK(I,TA(IC))
 85               CONTINUE
D          WRITE(10,*) '[IGW033] NTS= ',NTS,' TA(IC)= ',TA(IC)
                  CBUFF=DIMCHR(NTS)
D          WRITE(10,*) '[IGW033] CBUFF= ',CBUFF
                  CALL IGW885(RDBUFF,FONT,CBUFF)
              ENDIF
 80       CONTINUE
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ENDIF
C     ******************************************
C     Ok at last write  out the directory entry
C     ******************************************
C     initialise the directory entry
      IF(ENTYP.EQ.LDIMN) THEN
C         **************
C         LINE DIMENSION
C         **************
          CALL IGWD00(IMBUFF,IGDP)
          CALL IGWD01(LINDIM,'  LINEAR',LINSUB)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(I,THICK,COLOR)
C         status type entity
          CALL IGWD04(0,0,1,0)
C         set the form number
C         initialize parameter block for entity
          CALL IGWP00(LINDIM)
C         write parameter data to buffer
C         Pointer data to all entities
          CALL IGAIBF(GENNP)
          CALL IGAIBF(LEADP(1))
          CALL IGAIBF(LEADP(2))
          CALL IGAIBF(WITNP(1))
          CALL IGAIBF(WITNP(2))
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ELSEIF(ENTYP.EQ.ADIMN) THEN
C         *************
C         ARC DIMENSION
C         *************
          CALL IGWD00(IMBUFF,IGDP)
          CALL IGWD01(ARCDIM,' ANGULAR',LINSUB)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(I,THICK,COLOR)
C         status type entity
          CALL IGWD04(0,0,1,0)
C         set the form number
C         initialize parameter block for entity
          CALL IGWP00(ARCDIM)
C         write parameter data to buffer
C         general note pointer
          CALL IGAIBF(GENNP)
C         witness line 1 pointer
          CALL IGAIBF(WITNP(1))
C         witness line 2 pointer
          CALL IGAIBF(WITNP(2))
C         vertex point X Y
          CALL IGARBF(SEGM(1,4))
          CALL IGARBF(SEGM(1,5))
C         radius of segments
          CALL IGARBF(SEGM(1,3))
C         leader anticlockwise from arrow vertex to tail
          CALL IGAIBF(LEADP(2))
C         leader clockwise from arrow vertex to tail
          CALL IGAIBF(LEADP(1))
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ELSEIF(ENTYP.EQ.DDIMN) THEN
C         ******************
C         DIAMETER DIMENSION
C         ******************
          CALL IGWD00(IMBUFF,IGDP)
          CALL IGWD01(DIADIM,'DIAMETER',LINSUB)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
C         status type entity
          CALL IGWD04(0,0,1,0)
          CALL IGWD02(I,THICK,COLOR)
C         set the form number
C         initialize parameter block for entity
          CALL IGWP00(DIADIM)
C         write parameter data to buffer
C         general note pointer
          CALL IGAIBF(GENNP)
C         leader anticlockwise from arrow vertex to tail
          CALL IGAIBF(LEADP(1))
C         leader clockwise from arrow vertex to tail
          CALL IGAIBF(LEADP(2))
C         arc center coordinates
          CALL IGARBF(SEGM(3,6))
          CALL IGARBF(SEGM(3,7))
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ELSEIF(ENTYP.EQ.RDIMN) THEN
C         ****************
C         RADIAL DIMENSION
C         ****************
          CALL IGWD00(IMBUFF,IGDP)
          CALL IGWD01(RADDIM,'  RADIAL',LINSUB)
C         set font,thick,colour here
C         get font number
          I=IMBUFF(6)
C         get thickness
          THICK=1
C         get colour number
          COLOR=IMBUFF(3)
          CALL IGWD02(I,THICK,COLOR)
C         status type entity
          CALL IGWD04(0,0,1,0)
C         set the form number
C         initialize parameter block for entity
          CALL IGWP00(RADDIM)
C         write parameter data to buffer
C         general note pointer
          CALL IGAIBF(GENNP)
C         leader anticlockwise from arrow vertex to tail
          CALL IGAIBF(LEADP(1))
C         arc center coordinates
          CALL IGARBF(SEGM(3,6))
          CALL IGARBF(SEGM(3,7))
C         add number pointers to associativities
          CALL IGAIBF(0)
C         add number pointers to properties
          CALL IGAIBF(0)
C         terminate parameter data
          CALL IGAEOR()
C         write parameter data to file
          CALL IGWP10(IGDP,I,IGPP)
C         I returns number of parameter records written
C         IGPP returns pointer to first parameter record
C         now write directory entry
          CALL IGWD10(I,IGPP)
      ENDIF
C
      END
C
 
C
      SUBROUTINE IGWL01(ARROW,AHW,SEGM,NS,WITNES,NTS,TA,VISI)      
C     =======================================================
C
C1    VARTYPE             R    R    R  I4   R     I4  I4 L
C1    IOSTATUS            O    O    O   O   O     O   O  O
C
C2    This routine decodes the dimension parameters and 
C2    codes it into something the IGES routines can handle
C2    ARROW returns the apex coordintes of the arrowhead
C2    SEGM returns the cordinates of the segment tail
C2    and other segments coming from one of the arrows
C2    WITNES returns the cordinats of the witnes lines 
C2    NTS returns the number of text items in the RWORK array
C2    TA contains the pointrs in sequence of the text items
C2    NS contains the number of segments wich relate to each arrow
C2    head.  Normally each one will have one segment 
C
C
      include   'include/wrkdat.inc'
      include   'include/movdat.inc'
      include   'include/nbuff.inc' 
      include   'include/dimendat.inc'
      include   'include/ndata.inc' 
      include   'include/entity.inc' 
      include   'include/masti.inc' 
C
      REAL ARROW(2,2),SEGM(10,7),WITNES(2,4),AHW(2,2),TSEG(10,5)
      REAL XS,YS,XF,YF,Y,X,SPX,SPY,ZERO
      INTEGER*4 I,J,K,AC,SC,LINS,WL,NSEG,NS(2),NTS,TA(5),ID,TNS,NUM
      INTEGER*2 ENT
      LOGICAL OK,SAME,TE,VISI(5),LINK,S1LINK,S2LINK,A1LINK,A2LINK
      CHARACTER TYPE*2
C
      EXTERNAL SAME
C    
C     initialise local and external variables
      PARAMETER (ZERO=0.0)
      K=ZERO
      AC=ZERO
      SC=ZERO
      WL=ZERO
      LINS=ZERO
      NSEG=ZERO
      NTS=ZERO
      NS(1)=ZERO
      NS(2)=ZERO
C     what entity are we dealing with
      ENT=IMBUFF(2)
C     set visibility status of sub entity
      DO 15 I=1,5
          VISI(I)=.FALSE.
 15   CONTINUE
C     Loop through the stored dimension records.
      DO 101 I=1,RECCNT(1)
      IF ( IWORK(1,I) .EQ. HEADER ) THEN
           IF(ENT.EQ.RDIMN.OR.ENT.EQ.DDIMN) THEN
C             if we have a diameter or radial dimension
C             then store arc center coordinates.
              SEGM(3,1)=RWORK(1,I)
              SEGM(3,2)=RWORK(2,I)
           ENDIF
       ELSEIF ( IWORK(1,I) .EQ. TEXSEG ) THEN
           NTS=NTS+1
C          store the pointer to the text use it later
           TA(NTS)=I 
C          there is now at least one piece of text
           VISI(5)=.TRUE.
        ELSE IF ( IWORK(1,I) .EQ. TERMIN ) THEN
           AC=AC+1
C          apex coords
           ARROW(AC,1)=RWORK(1,I)
           ARROW(AC,2)=RWORK(2,I)
C          height
           AHW(AC,1)=RWORK(5,I)
C          width
           AHW(AC,2)=RWORK(6,I)
C          we can now see it
           IF(IWORK(4,I).GE.0) VISI(AC)=.TRUE.
        ELSE IF ( IWORK(1,I) .EQ. LINSEG ) THEN
           LINS=LINS+1
C          What line segs are we looking at
           IF(LINS.LE.AC) THEN
C          If there are arrowheads used then use line segs
      WRITE(10,*) '[IGWL01] RWORK= ',(RWORK(ID,I),' ',ID=1,6)
               IF(SAME(ARROW(LINS,1),RWORK(1,I)).AND.
     +         SAME(ARROW(LINS,2),RWORK(2,I))) THEN
C                  use the oppostie coordinate for the tail
                   SEGM(LINS,1)=RWORK(4,I)
                   SEGM(LINS,2)=RWORK(5,I)
               ELSE
                   SEGM(LINS,1)=RWORK(1,I)
                   SEGM(LINS,2)=RWORK(2,I)
               ENDIF
      WRITE(10,*) '[IGWL01] SEGM 1= ',SEGM(LINS,1)
      WRITE(10,*) '[IGWL01] SEGM 2= ',SEGM(LINS,2)
C          there are no witnes line on a diameter or radius dimension
           ELSEIF(WL.LT.2.AND.(ENT.EQ.LDIMN.OR.ENT.EQ.ADIMN)) THEN
C              Witness lines must be next
               WL=WL+1
C              set witness line array
               WITNES(WL,1)=RWORK(1,I)
               WITNES(WL,2)=RWORK(2,I)
               WITNES(WL,3)=RWORK(4,I)
               WITNES(WL,4)=RWORK(5,I)
C              if visible then flag
               IF(IWORK(4,I).GE.0) VISI(WL+2)=.TRUE.
            ELSE
C              we have other segments. we gorra link them
C              to existing segemnts
C              store the segments for use later
               NSEG=NSEG+1
               TSEG(NSEG,1)=RWORK(1,I)
               TSEG(NSEG,2)=RWORK(2,I)
               TSEG(NSEG,3)=RWORK(4,I)
               TSEG(NSEG,4)=RWORK(5,I)
               IF(IWORK(4,I).GE.0) THEN
                   TSEG(NSEG,5)=1.0
               ELSE
                   TSEG(NSEG,5)=ZERO
               ENDIF
               WRITE(10,*) '[IGWL01] ',TSEG(NSEG,1)
               WRITE(10,*) '[IGWL01] ',TSEG(NSEG,2)
               WRITE(10,*) '[IGWL01] ',TSEG(NSEG,3)
               WRITE(10,*) '[IGWL01] ',TSEG(NSEG,4)
               WRITE(10,*) '[IGWL01] ',TSEG(NSEG,5)
            ENDIF
        ELSE IF ( IWORK(1,I) .EQ. ARCSEG ) THEN
            LINS=LINS+1
C           What line segs are we looking at
C           If there are arrowheads used then use ARCsegs
            XS=RWORK(1,I)+RWORK(4,I)*COS(RWORK(5,I))
            YS=RWORK(2,I)+RWORK(4,I)*SIN(RWORK(5,I))
            XF=RWORK(1,I)+RWORK(4,I)*COS(RWORK(6,I))
            YF=RWORK(2,I)+RWORK(4,I)*SIN(RWORK(6,I))
            IF(SAME(ARROW(LINS,1),XS).AND.
     +      SAME(ARROW(LINS,2),YS)) THEN
               SEGM(LINS,1)=XF
               SEGM(LINS,2)=YF
            ELSE
               SEGM(LINS,1)=XS
               SEGM(LINS,2)=YS
            ENDIF
C           radius
            SEGM(LINS,3)=RWORK(4,I)
C           center of arc (vertex)
            SEGM(LINS,4)=RWORK(1,I)
            SEGM(LINS,5)=RWORK(2,I)
        END IF
C     continue do loop count of all subentity records
 101  CONTINUE
C     if there are no extra segemnts dont bother
C     IF(NSEG.EQ.0) RETURN
      SC=3
      S1LINK=.TRUE.
      S2LINK=.TRUE.
      A1LINK=.TRUE.
      A2LINK=.TRUE.
      TNS=NSEG
C     set the 3rd segment as the beginning of the extra segemnts
230   CONTINUE
      IF(A1LINK) THEN
          SPX=ARROW(1,1)
          SPY=ARROW(1,2)
          A1LINK=.FALSE.
          SEGM(3,1)=SPX
          SEGM(3,2)=SPY
          TYPE='A1'
          SC=4
      ELSE IF(A2LINK) THEN
          SPX=ARROW(2,1)
          SPY=ARROW(2,2)
          A2LINK=.FALSE.
          SEGM(3,1)=SPX
          SEGM(3,2)=SPY
          TYPE='A2'
      ELSE IF(S1LINK) THEN
          SPX=SEGM(1,1)
          SPY=SEGM(1,2)
          S1LINK=.FALSE.
          TYPE='S1'
          SC=3
      ELSE IF(S2LINK) THEN
          SPX=SEGM(2,1)
          SPY=SEGM(2,2)
          S2LINK=.FALSE.
          TYPE='S2'
      ELSE
          NS(1)=0
          NS(2)=0
          RETURN
      ENDIF
C     set the 3rd segment as the beginning of the extra segemnts
C     hopefully the extra segemnts will string together.
      DO 200 J=1,NSEG
          IF((SAME(SPX,TSEG(J,1)).AND.SAME(SPY,TSEG(J,2))).OR.
     +       (SAME(SPX,TSEG(J,3)).AND.SAME(SPY,TSEG(J,4)))) THEN
              GOTO 210
          ENDIF
200   CONTINUE
C
      GOTO  230
C
210   CONTINUE
      J=1
      IF(NSEG.GT.0) THEN
220       CONTINUE
          WRITE(10,*) '[IGED01] SPX= ',SPX,' SPY= ',SPY
          WRITE(10,*) '[IGED01] J= ',J,' NSEG= ',NSEG
          IF(TSEG(J,5).GT.ZERO) THEN
              IF(SAME(SPX,TSEG(J,1)).AND.SAME(SPY,TSEG(J,2))) THEN
          WRITE(10,*) '[IGED01] FOUND'
                  SEGM(SC,1)=TSEG(J,3)
                  SEGM(SC,2)=TSEG(J,4)
                  SPX=TSEG(J,3)
                  SPY=TSEG(J,4)
              ELSEIF(SAME(SPX,TSEG(J,3)).AND.SAME(SPY,TSEG(J,4))) THEN
          WRITE(10,*) '[IGED01] FOUND'
                  SEGM(SC,1)=TSEG(J,1)
                  SEGM(SC,2)=TSEG(J,2)
                  SPX=TSEG(J,1)
                  SPY=TSEG(J,2)
              ELSE
          WRITE(10,*) '[IGED01] NOT FOUND'
                  J=J+1
C                 if there are segements left to test
                  IF(J.LE.TNS) GOTO 220
C                 insert an extra segement
                  READ(UNIT=TYPE(2:2),FMT='(I2)') NUM
                  SPX=SEGM(NUM,1)
                  SPY=SEGM(NUM,2)
                  SEGM(SC,1)=SPX
                  SEGM(SC,2)=SPY
                  SC=SC+1
                  GOTO 210
              ENDIF
              SPX=SEGM(SC,1)
              SPY=SEGM(SC,2)
              SC=SC+1
              TSEG(J,5)=ZERO
              NSEG=NSEG-1
              GOTO 210
          ENDIF
          WRITE(10,*) '[IGED01] USED ENT'
          J=J+1
          IF(J.LE.TNS) GOTO 220
      ENDIF
      SC=SC-1
C     segement number for extras
      IF(TYPE(2:2).EQ.'1') THEN
         NS(1)=SC
         NS(2)=0
      ELSE
         NS(2)=SC
         NS(1)=0
      ENDIF
C
      END
C
C -------------------------------------------------------------------
C

