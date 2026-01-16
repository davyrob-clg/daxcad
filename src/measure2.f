C
C     @(#)  412.1 date 6/11/92 measure2.f 
C
C
C     Filename    : measure2.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:40:14
C     Last change : 92/06/11 14:34:55
C
C     Copyright : Practical Technology Limited  
C     File :- measure2.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE OTDATA()
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE OTDATA()
C     ===================
C1    VARYPE      NONE      
C1    IOSTAT            
C
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C2    OTDATA outputs the data about any of the following
C2    on to the screen
C2    Line,Arc,Spline,Text,Hatch.
C
      include 'include/wtov.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/movdat.inc'
      include 'include/compd.inc'
      include 'include/lfont.inc'
      include 'include/lfu.inc'
 
      REAL X,Y,ANG,CANG,DIST,DEG,DISTXY,SLA,PI,M(3,3),BRDR,DUMMY,
     +    WXP1,WYP1,WXP2,WYP2, XP1,YP1,XP2,YP2,VD0D13,L1,L2,L3,VAL
      CHARACTER*256 OLIN,ANS*1
      CHARACTER*16 STR,FORM*40,WORD1,WORD2,WORD3,WORD4
      CHARACTER*16 WORD5,WORD6,WORD7,GWORD
      INTEGER*4 C,TMEN,TCELL,I,TC,RES,RES1,NLEN,NLEN1,OUNIT,J,II,JJ,KK
      INTEGER*2 JST,NCHAR,TMIP,ENT,TTMIP,INST
      INTEGER*4 TXTJST(9),CENTYP(3),ISPLIN(4),JSPLIN(6)
      INTEGER*4 TXTCNT
      LOGICAL OK,DELETE,NOFIND,LL0,LL1,LL2,LL3,LL4,GRPED
      INTRINSIC CHAR
      EXTERNAL DEPRNT,CPRINT,NOSRCH,ADSRCH,CRUNCH,GTPMSG,
     1         DSE800,TCURS,UNFLAG,UCODET,VD0D13,NLEN,NLEN1,PI
C
      CHARACTER*400 TEXTO(10)
      DATA (TXTJST(I),I=1,9)/157,154,151,158,155,152,159,156,153/
      DATA (CENTYP(I),I=1,3)/87,86,167/
      DATA (ISPLIN(I),I=1,4)/360,361,362,363/
      DATA (JSPLIN(I),I=1,6)/360,361,350,351,350,350/
C
C
      IF(IMBUFF(1).EQ.(GROUP+128)) THEN
          GWORD = 'Grouped'
      ELSE
          GWORD = 'Single'
      ENDIF
      IF ( IMBUFF(2) .EQ. ARC ) THEN
C  ********************************************************
C *******                                           ********
C*******               A R C                         ********
C *******                                           ********
C  ********************************************************
C
         VAL=MAX(ABS(RDBUFF(1)),ABS(RDBUFF(2)),ABS(RDBUFF(4)),
     +        ABS(DEG(RDBUFF(5))),ABS(DEG(RDBUFF(6))))
         RES=3
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
         WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C
C     ************************ DISPLAY LINE 1 **************
C        create format for writing first output line
C        Format of line="Entity:Arc, Angle1:xx, Angle2:yy"
         FORM=' '
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(6A,',STR(1:NLEN1(STR)),',3A,',STR(1:NLEN1(STR)),')'
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Arc"
         CALL VNWORD(74,WORD2)
C        get word "Angle"
         CALL VNWORD(175,WORD3)
C        write start and end angles to string for output
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2)),
     +      ', ',
     +      WORD3(1:NLEN1(WORD3)),
     +      '1:',
     +      DEG(RDBUFF(5)),
     +      ', ',
     +      WORD3(1:NLEN1(WORD3)),
     +      '2:',
     +      DEG(RDBUFF(6))
C        write to screen for user in form
C        "Entity:Arc, Angle1:xx, Angle2:yy"
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C
C     ************************ DISPLAY LINE 2 **************
C        create format for writing second output line
C        using format "Centre:xx,yy, Radius:rr"
         FORM=' '
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(A,2(A,',STR(1:NLEN1(STR)),'),3A,',STR(1:NLEN1(STR)),')'
C        get word "Centre"
         CALL VNWORD(43,WORD1)
C        get word "Radius"
         CALL VNWORD(218,WORD2)
C        write second line to buffer for output
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +   WORD1(1:NLEN1(WORD1)),
     +   ':',
     +   RDBUFF(1),
     +   ', ',
     +   RDBUFF(2),
     +   ' ,',
     +   WORD2(1:NLEN1(WORD2)),
     +   ':',
     +   RDBUFF(4)
C        send to screen
         TEXTO(2) = OLIN
         CALL CPRINT(OLIN)
C
C     ************************ DISPLAY LINE 3 **************
C        create format for third line of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(6A,', STR(1:NLEN1(STR)), ',2(2A,I3),5A)'
C        find arc length
         ANG=ABS(RDBUFF(6)-RDBUFF(5))
         IF ( RDBUFF(5).GT.RDBUFF(6) ) THEN
            ANG=PI(2.0)-ANG
         END IF
         DIST=RDBUFF(4)*ANG
C        get word "Length'
         CALL VNWORD(100,WORD1)
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Font'
         CALL VNWORD(242,WORD3)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        write to buffer for printing
C        in form "Font:sss, Length:xx, Layer:lll, Thick:ttt,Colour:cc"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD3(1:NLEN1(WORD3)),
     +      ': ',WORD6(1:NLEN1(WORD6)),
     +      ', ',
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      DIST,
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD7)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
 
C        write to screen,wait for key to continue
         TEXTO(3) = OLIN
         CALL INFODIALOG(TEXTO,3)
C
      ELSE IF ( IMBUFF(2) .EQ. LINE ) THEN
C  ********************************************************
C *******                                           ********
C*******               L I N E                       ********
C *******                                           ********
C  ********************************************************
C
C        calculate angle of line
         ANG=DEG(CANG(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5)))
         IF ( ANG .GT. 270 ) ANG=ANG-360
         IF ( ANG .GT. 180 ) ANG=ANG-180
         IF ( ANG .GT.  90 ) ANG=ANG-180
C        calculate length of line
         DIST=DISTXY(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5))
C
         VAL=MAX(ABS(RDBUFF(1)),ABS(RDBUFF(2)),
     +           ABS(RDBUFF(4)),ABS(RDBUFF(5)),DIST,ABS(ANG))
C
         RES=3
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
C        Create format statement for current precision
         WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C     ************************ DISPLAY LINE 1 **************
C        create format for writing buffer string
         FORM=' '
         WRITE(UNIT=FORM,FMT='(6A)')
     +   '(3A,4(A,', STR(1:NLEN1(STR)), '),2A,',STR(1:NLEN1(STR)),')'
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Line"
         CALL VNWORD(75,WORD2)
C        get word "Angle:"
         CALL VNWORD(124,WORD3)
C        write coords and length to string for output
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2)),
     +      ', (',
     +      RDBUFF(1),
     +      ', ',
     +      RDBUFF(2),
     +      ') (',
     +      RDBUFF(4),
     +      ', ',
     +      RDBUFF(5),
     +      '), ',
     +      WORD3(1:NLEN1(WORD3)),
     +      ANG
C        write to screen for user in form
C        "Entity:Line, (xx, yy) (xx, yy), Angle:aa"
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 2 **************
C        create format for third line of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(3A)')
     +   '(6A,', STR(1:NLEN1(STR)), ',2(2A,I3),5A)'
C        get word "Length'
         CALL VNWORD(100,WORD1)
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Font'
         CALL VNWORD(242,WORD3)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        write to buffer for printing
C        "Font:sss, Length:xx, Layer:lll, Thick:ttt,Colour:cc"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD3(1:NLEN1(WORD3)),
     +      ': ',WORD6(1:NLEN1(WORD6)),
     +      ', ',
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      DIST,
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD7)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
C        write to screen,wait for key to continue
         TEXTO(2) = OLIN
         CALL CPRINT(OLIN)
         CALL INFODIALOG(TEXTO,2)

C
      ELSE IF ( IMBUFF(2) .EQ. CENLIN ) THEN
C    ********************************************************
C   *******                                           ********
C  *******          C E N T R E    L I N E             ********
C   *******                                           ********
C    ********************************************************
C
         VAL=MAX(ABS(RDBUFF(1)),ABS(RDBUFF(2)),ABS(RDBUFF(3)),
     +           ABS(RDBUFF(4)),ABS(RDBUFF(5)),ABS(RDBUFF(6)))
C
         RES=3
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
C        Create format statement for current precision
         WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C     ************************ DISPLAY LINE 1 **************
C        create format for writing buffer string
         WRITE(UNIT=FORM,FMT='(7A)')
     +   '(6A,2(',STR(1:NLEN1(STR)),',A),A,',STR(1:NLEN1(STR)),
     +   ',2A,',STR(1:NLEN1(STR)),')'
C        get words "Centre Line"
         CALL VNWORD(82,WORD1)
C        Get centre line type.
         I = CENTYP(IDBUFF(4))
         CALL VNWORD(I,WORD5)
C        get words "Border Size"
         CALL VNWORD(84,WORD2)
C        get word "Angle:"
         CALL VNWORD(124,WORD3)
C        get word "Centre"
         CALL VNWORD(43,WORD4)
C        write data to string for output in the format:
C
C        Center Line: Center(XX,YY) Border SizeXX AngleXX
C
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),': ',
     +      WORD5(1:NLEN1(WORD5)),'   ',
     +      WORD4(1:NLEN1(WORD4)),'(',
     +      RDBUFF(1),',',
     +      RDBUFF(2),')  ',
     +      WORD2(1:NLEN1(WORD2)),RDBUFF(6),'   ',
     +      WORD3(1:NLEN1(WORD3)),DEG(RDBUFF(5))
C
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C
C     ************************ DISPLAY LINE 2 **************
C
C        get word "Radius"
         CALL VNWORD(218,WORD1)
C        get word "Thick"
         CALL VNWORD(247,WORD2)
C
         IF (IDBUFF(4).EQ.1) THEN
C           cross centre line.
C
            WRITE(UNIT=FORM,FMT='(3A)') '(2A,',STR(1:NLEN1(STR)),
     +             ',3A,I3)'
            WRITE(UNIT=OLIN,FMT=FORM)
     +           WORD1(1:NLEN1(WORD1)),': ',RDBUFF(3),',  ',
     +           WORD2(1:NLEN1(WORD2)),': ',IMBUFF(12)
         ELSE IF (IDBUFF(4).EQ.2) THEN
C           Second line for single centre line.
C
C           Get the extension and convert it from paper to world.
            CALL PAP2SC(RDBUFF(6),DIST)
            CALL SC2WO(DIST,0.0,BRDR,DUMMY)
            CALL SC2WO(0.0,0.0,DUMMY,DIST)
            BRDR = BRDR - DUMMY
            DIST = RDBUFF(3) + BRDR
            CALL NEWPNT(RDBUFF(1),RDBUFF(2),DIST,DIST,0,
     +                  RDBUFF(5),XP2,YP2)
            XP1 = RDBUFF(1) - (XP2 - RDBUFF(1)) 
            YP1 = RDBUFF(2) - (YP2 - RDBUFF(2)) 
C
C           Better update the precision.
            VAL=MAX(ABS(XP1),ABS(YP1),ABS(XP2),ABS(YP2))
            RES1=3
            IF ( VAL .GT. 1.0 ) THEN
               RES1=INT(LOG10(VAL))+RES
            END IF
            IF (RES1.GT.RES) THEN
C              Create format statement for current precision
               WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES1,'.',PREC
               CALL CRUNCH(STR)
            ENDIF
C
            WRITE(UNIT=FORM,FMT='(3A)') '(A,4(',STR(1:NLEN1(STR)),
     +                                  ',A),2A,I4)'
            WRITE(UNIT=OLIN,FMT=FORM)
     +           '(',XP1,',',YP1,'),(',XP2,',',YP2,')   ',
     +           WORD2(1:NLEN1(WORD2)),': ',IMBUFF(3)
         ELSE
C            PCD centre line.
C
            WRITE(UNIT=FORM,FMT='(3A)') '(2A,2(',STR(1:NLEN1(STR)),
     +             ',3A),I3)'
            WRITE(UNIT=OLIN,FMT=FORM)
     +           WORD1(1:NLEN1(WORD1)),'1 : ',RDBUFF(3),',  ',
     +           WORD1(1:NLEN1(WORD1)),'2 : ',RDBUFF(4),',  ',
     +           WORD2(1:NLEN1(WORD2)),': ',IMBUFF(12)
         ENDIF                                     
C
         TEXTO(2) = OLIN
         CALL CPRINT(OLIN)
C
C     ************************ DISPLAY LINE 3 **************
C
C        get word "Colour"
         CALL VNWORD(399,WORD3)
C
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C
C        get word "Font'
         CALL VNWORD(242,WORD4)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C
         WRITE(UNIT=OLIN,FMT='(9A)')
     +           WORD3(1:NLEN1(WORD3)),' ',WORD7(1:NLEN1(WORD7)),',  ',
     +           WORD4(1:NLEN1(WORD4)),': ',WORD6(1:NLEN1(WORD6)),',  ',
     +           GWORD(1:NLEN1(GWORD))
C
C        write to screen,wait for key to continue
         TEXTO(3) = OLIN
         CALL INFODIALOG(TEXTO,3)
C
      ELSE IF ( IMBUFF(2) .EQ. TEXT ) THEN
C  ********************************************************
C *******                                           ********
C*******               T E X T                       ********
C *******                                           ********
C  ********************************************************
C
C        We have found text to show him. ?
         CALL UCODET(RDBUFF(6),SLA,JST,NCHAR)
C
         VAL=MAX(ABS(RDBUFF(1)),ABS(RDBUFF(2)),
     +           ABS(RDBUFF(3)),ABS(RDBUFF(4)),
     1           ABS(RDBUFF(5)),
     2        SLA)
         RES=3
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
C        create format for current precision
         WRITE(UNIT=STR,FMT='(2(A,I3))') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C     ************************ DISPLAY LINE 1 **************
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Text"
         CALL VNWORD(72,WORD2)
C        create format for first line of output
C        to write in form "Entity:Text:This is Text"
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A)') '(6A)'
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2)),
     +      ':"',
     +      CBUFF(1:NLEN1(CBUFF)),
     +      '"'
C        write to screen
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 2 **************
C        get word "Origin"
         CALL VNWORD(275,WORD1)
C        get word "Angle"
         CALL VNWORD(175,WORD2)
         FORM=' '
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(A,2(A,',
     +   STR(1:NLEN1(STR)),
     +   '),3A,',
     +   STR(1:NLEN1(STR)),
     +   ',A)'
C
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      RDBUFF(1),
     +      ', ',
     +      RDBUFF(2),
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      ':',
     +      RDBUFF(5),
     +      ' Degs'
C        write to screen
         TEXTO(2) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 3 **************
         FORM=' '
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(3(A,',STR(1:NLEN1(STR)),',A))'
C
C        get word "Height:"
         CALL VNWORD(162,WORD1)
C        get word "Width:"
         CALL VNWORD(163,WORD2)
C        get word "Slant:"
         CALL VNWORD(161,WORD3)
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +       WORD1(1:NLEN1(WORD1)),
     +       RDBUFF(4),
     +       ', ',
     +       WORD2(1:NLEN1(WORD2)),
     +       RDBUFF(3),
     +       ', ',
     +       WORD3(1:NLEN1(WORD3)),
     +       SLA,
     +       ' Degs'
         TEXTO(3) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 4 **************
C        get the word 'JUST:'
         CALL VNWORD(160,WORD1)
C        get text justification
         I = TXTJST(JST)
         CALL VNWORD(I,WORD2)
C        build string for output of justification
         OLIN=' '
         WRITE(UNIT=OLIN,FMT='(3A)')
     +      WORD1(1:NLEN1(WORD1)),
     +      ' ',
     +      WORD2(1:NLEN1(WORD2))
         TEXTO(4) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 5 **************
C        create format for line 5 of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A)')
     +   '(3A,2(2A,I3),5A)'
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Font'
         CALL VNWORD(242,WORD3)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        write to buffer for printing
C        "Font:sss, Length:xx, Layer:lll, Thick:ttt,Colour:cc"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD3(1:NLEN1(WORD3)),
     +      ': ',WORD6(1:NLEN1(WORD6)),
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD6)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
C        print to screen,wait for key
          CALL CPRINT(OLIN)
          TEXTO(5) = OLIN
          CALL INFODIALOG(TEXTO,5)
C
      ELSE IF ( IMBUFF(2) .EQ. HATCH ) THEN
C  ********************************************************
C *******                                           ********
C*******               H A T C H                     ********
C *******                                           ********
C  ********************************************************
C        Reread the header
         DIST = 0
         PDFP=IDBUFF(2)
         CALL DER500(PDFP,OK)
C        Read the first hatch line.
         PDFP=IDBUFF(3)
         IF(PDFP.EQ.0) GOTO 200
         CALL DBR500(PDFP,OK)
C        get angle of hatch lines
         ANG=DEG(CANG(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5)))
         IF ( ANG .GT. 270 ) ANG=ANG-360
         IF ( ANG .GT. 180 ) ANG=ANG-180
         IF ( ANG .GT.  90 ) ANG=ANG-180
         CALL CV0L14(RDBUFF(1),RDBUFF(2),RDBUFF(4),RDBUFF(5),
     +               L1,L2,L3)
         PDFP=IDBUFF(3)
         IF(PDFP.EQ.0) GOTO 200
C        Read the next hatch line.
         CALL DBR500(PDFP,OK)
C        get Pitch between lines (Paper Units)
         DIST=VD0D13(L1,L2,L3,RDBUFF(1),RDBUFF(2))/PAPTOW
 
 200     CONTINUE
         VAL=MAX(DIST,ANG)
         RES=4
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
C        create format for current number precision
         WRITE(UNIT=STR,FMT='(A,I3,A,I3)') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C
C     ************************ DISPLAY LINE 1 **************
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Hatch"
         CALL VNWORD(71,WORD2)
C        get word "Pitch:"
         CALL VNWORD(123,WORD3)
C        get word "Angle:"
         CALL VNWORD(124,WORD4)
C        create format for first line of output
C        to write in form "Entity:Hatch, Pitch:xx, Angle:zz"
         FORM=' '
         WRITE(UNIT=FORM,FMT='(3A)')
     +      '(4A,2(A,', STR(1:NLEN1(STR)), ',A))'
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2)),
     +      ', ',
     +      WORD3(1:NLEN1(WORD3)),
     +      DIST,
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      ANG,
     +      ' Degs'
C        write to screen
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 2 **************
C        create format for line 2 of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A)')
     +   '(3A,3(2A,I3),5A)'
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Font'
         CALL VNWORD(242,WORD3)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        get word "Form:'
         CALL VNWORD(128,WORD1)
C        write to buffer for printing
C        in form "Font:sss, Layer:lll, Thick:ttt, Form:fff, Colour:cc,"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD3(1:NLEN1(WORD3)),
     +      ': ',WORD6(1:NLEN1(WORD6)),
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ',',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD1(1:NLEN1(WORD1)),
     +      IMBUFF(5),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD7)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
C        print to screen,wait for key
         TEXTO(2) = OLIN
         CALL INFODIALOG(TEXTO,2)
C
      ELSE IF ( IMBUFF(2) .EQ. SPLINE ) THEN
C  ********************************************************
C *******                                            ********
C*******               S P L I N E                    ********
C *******                                            ********
C  ********************************************************
C
C     ************************ DISPLAY LINE 1 **************
C        get FORM of representation of curve
         II=MOD(IMBUFF(5)+0,4)
C        get type of FIT of curve
         JJ=(IMBUFF(5)-II)/4
C        add offset for representation form
         II=II+1
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Curve"
         CALL VNWORD(77,WORD2)
C        get word for curve type
         CALL VNWORD(JSPLIN(JJ),WORD3)
C        get word for representation form
         CALL VNWORD(ISPLIN(II),WORD4)
C        get word "Form"
         CALL VNWORD(246,WORD5)
C        create format for first line of output
C        to write in form "Entity:Curve, Form:Hermite,Points"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT='(9A)')
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2)),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      ':',
     +      WORD3(1:NLEN1(WORD3)),
     +      ',',
     +      WORD4(1:NLEN1(WORD4))
C        write to screen
         TEXTO(1) = OLIN
         CALL CPRINT(OLIN)
C     ************************ DISPLAY LINE 2 **************
C        create format for line 5 of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A)')
     +   '(3A,2(2A,I3),5A)'
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Font'
         CALL VNWORD(242,WORD3)
C        Get font name.
         WORD6 = FONTNM(IMBUFF(6))
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        write to buffer for printing
C        in form "Font:ss, Layer:ll, Thick:tt, Colour:cc"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD3(1:NLEN1(WORD3)),
     +      ': ',WORD6(1:NLEN1(WORD6)),
     +      ', ',
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD7)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
C        print to screen,wait for key
         TEXTO(2) = OLIN
         CALL INFODIALOG(TEXTO,2)
C
      ELSE IF ( IMBUFF(2).EQ.COMPI.OR.
     +          IMBUFF(2).EQ.SYMBI    ) THEN
C  ********************************************************
C *******                                            ********
C*******   C O M P O N E N T   I N S T A N C E        ********
C *******     S Y M B O L                              ********
C  *******                                            ********
C   ********************************************************
         TTMIP=MIP
         CALL ALLRD(TTMIP,ENT,M,DELETE)
         INST=ENT
         CALL OPNFFF('COMPONENT.DATA',OUNIT,OK)
C
         IF ( .NOT. OK ) THEN
            CALL DEPRNT(422)
            RETURN
         END IF
C
         CALL I3M(M1)
         CALL MULT3M(M,M1,MM)
         CNEST=0
         NINSTS=0
         NOFIND=.FALSE.
C        find word "Name"
         CALL VNWORD(89,WORD2)
C        original instance.
         IF ( INST .EQ.COMPM ) THEN
C          get word "Component"
           CALL VNWORD(322,WORD1)
         ELSE
C          get word "Symbol"
           CALL VNWORD(334,WORD1)
         END IF
         WRITE(UNIT=OUNIT,FMT='(6A)')
     +     WORD1(1:NLEN1(WORD1)),
     +     ' ',
     +     WORD2(1:NLEN1(WORD2)),
     +     ':"',
     +     CBUFF(1:NLEN1(CBUFF)),
     +     '"'
C
C        get word "Layer:"
         CALL VNWORD(195,WORD1)
         WRITE(UNIT=OUNIT,FMT='(A,I3)')
     +     WORD1(1:NLEN1(WORD1)),
     +     IMBUFF(4)
         XP1=0.0
         YP1=0.0
         XP2=(WXMAX-WXMIN)
         YP2=0.0
         CALL NEWXY(XP1,YP1,WXP1,WYP1,MM)
         CALL NEWXY(XP2,YP2,WXP2,WYP2,MM)
         X=WXP1-XP1
         Y=WYP1-YP1
         RES=4
         VAL=MAX(ABS(X),ABS(Y))
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
C        create format for current precision
         WRITE(UNIT=STR,FMT='(2(A,I3))') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C        write out position
         WRITE(UNIT=FORM,FMT='(3A)')
     +   '(A,2(A,', STR(1:NLEN1(STR)), '))'
C        get word "Origin"
         CALL VNWORD(275,WORD1)
         WRITE(UNIT=OUNIT,FMT=FORM)
     +     WORD1(1:NLEN1(WORD1)),
     +     ':',
     +     X,
     +     ', ',
     +     Y
C
          X= DISTXY(WXP1,WYP1,WXP2,WYP2)/
     1    DISTXY(XP1,YP1,XP2,YP2)
          Y=DEG(CANG(WXP1,WYP1,WXP2,WYP2))
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(2A,', STR(1:NLEN1(STR)), ',3A,', STR(1:NLEN1(STR)) ,')'
C        get word "Scale"
         CALL VNWORD(192,WORD1)
C        get word "Rotate"
         CALL VNWORD(11,WORD2)
C        write out scale and rotation
         WRITE(UNIT=OUNIT,FMT=FORM)
     +    WORD1(1:NLEN1(WORD1)),
     +    ':',
     +    X,
     +    ', ',
     +    WORD2(1:NLEN1(WORD2)),
     +    ':',
     +    Y
C
         WRITE(UNIT=OUNIT,FMT='(A)')
     +   '******************************************************'
C        must read back the next instance to expand
C        save opflags
         LL0=OPFLAG(1)
         LL1=OPFLAG(6)
         LL2=OPFLAG(7)
         LL3=OPFLAG(3)
         LL4=OPFLAG(11)
C        set opflags to ensure scale done properly
         OPFLAG(1)=.TRUE.
         OPFLAG(6)=.TRUE.
         OPFLAG(7)=.TRUE.
         OPFLAG(3)=.FALSE.
         OPFLAG(11)=.FALSE.
C
 5       CONTINUE
C        find relation header from instance data
         RELP=IMBUFF(10)
C        WRITE(UNIT=10,FMT='(I6,2A)')RELP,' ',CBUFF
C        WRITE(UNIT=10,FMT=*)'[DRW066] Start of Component'
C        WRITE(UNIT=10,FMT=*)'Nest=',CNEST,' Ninsts=',NINSTS
C        WRITE(UNIT=10,FMT=*)' Current Transform'
C        WRITE(UNIT=10,FMT='(3F8.3)')((MM(II,JJ),JJ=1,3),II=1,3)
C
C        if null relation pointer,then unresolved instance
         IF (RELP.LE.0) THEN
C           get message "Unresolved Instance"
            CALL PRWORD(418,OLIN)
C           write to extract file.
            WRITE(UNIT=OUNIT,FMT='(3A)')
     +        OLIN(1:NLEN1(OLIN)),':',CBUFF(1:NLEN1(CBUFF))
                NOFIND=.TRUE.
            END IF
            IF (NOFIND) GOTO 20
C           read the relation header
            CALL DRR950(RELP,OK)
C           header data now in buffer
C           test for valid component relation
            IF (RLBUFF(1).NE.COMPM.AND.RLBUFF(1).NE.SYMBM) GOTO 20
C        ensure text scaled proportionaly during display
C        save the number of records,and entities
         NRRECS=RLBUFF(4)
         NENTS=RLBUFF(5)
C
         DO 15 J=1,NRRECS
            NXTRLR=RLBUFF(2)
C           read the list of entities
            CALL DRR950(NXTRLR,OK)
            DO 10 I=4,10
            TTMIP=RLBUFF(I)
            IF (TTMIP.GT.0) THEN
C              read the entity and draw it in position
               CALL ALLRD(TTMIP,ENT,M1,DELETE)
C               pop to screen for display
                IF (ENT.EQ.COMPI.OR.ENT.EQ.SYMBI) THEN
C                 found a nested instance
C                 save reference
                  NINSTS=NINSTS+1
C                 set nest level for this instance,must be 1 greater
C                 than the current level.
                  NSTLEV(NINSTS)=CNEST+1
C                 save pointer to instance
                  NSTMIP(NINSTS)=TTMIP
C                 save instance name
                  NSTNAM(NINSTS)=CBUFF
C                 calculate nested transformation
                  CALL MULT3M(MM,M1,M2)
C                 save the transform
                  KK=0
                  DO 30 JJ=1,3
                     DO 31 II=1,3
                        KK=KK+1
                        NSTTFM(KK,NINSTS)=M2(II,JJ)
 31                  CONTINUE
 30               CONTINUE
C                  WRITE(UNIT=10,FMT=*)'[DRW066] Nested Component'
C                  WRITE(UNIT=10,FMT=*)'[DRW066] Nest level=',
C    +            CNEST+1,' MIP=',TTMIP,' Name=',CBUFF
C            WRITE(UNIT=10,FMT='(3F8.3)')((M2(II,JJ),JJ=1,3),II=1,3)
C                 original instance.
C                 Get message "Nested Instance Name"
                  CALL PRWORD(494,OLIN)
                  WRITE(UNIT=OUNIT,FMT='(4A)')
     +              OLIN(1:NLEN1(OLIN)),
     +              ':"',
     +              CBUFF(1:NLEN1(CBUFF)),'"'
C                get word "Layer:"
                 CALL VNWORD(195,WORD1)
                 WRITE(UNIT=OUNIT,FMT='(A,I3)')
     +             WORD1(1:NLEN1(WORD1)),
     +             IMBUFF(4)
                  XP1=0.0
                  YP1=0.0
                  XP2=(WXMAX-WXMIN)
                  YP2=0.0
                  CALL NEWXY(XP1,YP1,WXP1,WYP1,M1)
                  CALL NEWXY(XP2,YP2,WXP2,WYP2,M1)
                  X=WXP1-XP1
                  Y=WYP1-YP1
                  RES=4
                  VAL=MAX(ABS(X),ABS(Y))
                  IF ( VAL .GT. 1.0 ) THEN
                     RES=INT(LOG10(VAL))+RES
                  END IF
                  WRITE(UNIT=STR,FMT='(2(A,I3))')
     +            'F',PREC+RES,'.',PREC
                  CALL CRUNCH(STR)
C                 write out position
                  WRITE(UNIT=FORM,FMT='(3A)')
     +              '(A,2(A,', STR(1:NLEN1(STR)), '))'
C                   get word "Origin"
                    CALL VNWORD(275,WORD1)
                    WRITE(UNIT=OUNIT,FMT=FORM)
     +              WORD1(1:NLEN1(WORD1)),
     +              ':',
     +              X,
     +              ', ',
     +              Y
 
                  X= DISTXY(WXP1,WYP1,WXP2,WYP2)/
     1               DISTXY(XP1,YP1,XP2,YP2)
                  Y=DEG(CANG(WXP1,WYP1,WXP2,WYP2))
                  WRITE(UNIT=FORM,FMT='(5A)')
     +   '(2A,', STR(1:NLEN1(STR)), ',3A,', STR(1:NLEN1(STR)) ,')'
C                 get word "Scale"
                  CALL VNWORD(192,WORD1)
C                 get word "Rotate"
                  CALL VNWORD(11,WORD2)
C                 write out scale and rotation
                  WRITE(UNIT=OUNIT,FMT=FORM)
     +            WORD1(1:NLEN1(WORD1)),
     +            ':',
     +            X,
     +            ', ',
     +            WORD2(1:NLEN1(WORD2)),
     +            ':',
     +            Y
C
               ELSE
C                 transform the entity
                  CALL ALLTFM(ENT,MM)
               END IF
            END IF
 10      CONTINUE
 15   CONTINUE
 20   CONTINUE
C        handle the nested instances if required
         IF (NINSTS.GT.0) THEN
C           must read back the next instance to expand
            TTMIP=NSTMIP(NINSTS)
            CNEST=NSTLEV(NINSTS)
            CALL ALLRD(TTMIP,ENT,M1,DELETE)
C           retrieve transform
            KK=0
            DO 32 JJ=1,3
               DO 33 II=1,3
                  KK=KK+1
                  MM(II,JJ)=NSTTFM(KK,NINSTS)
 33            CONTINUE
 32         CONTINUE
C           decrement count of instances to be expanded
            NINSTS=NINSTS-1
C           go process this instance
            GOTO 5
         END IF
C        recover opflags
         OPFLAG(1)=LL0
         OPFLAG(6)=LL1
         OPFLAG(7)=LL2
         OPFLAG(3)=LL3
         OPFLAG(11)=LL4
C
         CLOSE(UNIT=OUNIT,STATUS='KEEP')
CIBM
C         LFU(OUNIT)=.FALSE.
CIBM
C
         CALL DCPRNT(367)
         CALL POPPD1('COMPONENT.DATA',673,120,350,667)
C
      ELSE IF (IMBUFF(2).EQ.ADIMN.OR.IMBUFF(2).EQ.RDIMN.OR.
     +         IMBUFF(2).EQ.LDIMN.OR.IMBUFF(2).EQ.DDIMN.OR.
     1                               IMBUFF(2).EQ.GLABEL) THEN
C   ***********************************************************
C  *******                L I N E A R                    ********
C *******                 R A D I A L                     ********
C*******             D I A M E T E R I A L                 ********
C *******               A N G U L A R                     ********
C  *******             D I M E N S I O N                  *******
C   ************************************************************
C
C        right set a text line counter
         TXTCNT=0
C        We have found text to show him. ?
         CALL UCODET(RWORK(6,4),SLA,JST,NCHAR)
         VAL =-1E10
         DO 102 I=2,RECCNT(1)
         VAL=MAX(VAL,RWORK(3,I),RWORK(4,I),RWORK(5,I),RWORK(6,I))
 102     CONTINUE
         RES=4
         IF ( VAL .GT. 1.0 ) THEN
            RES=INT(LOG10(VAL))+RES
         END IF
         WRITE(UNIT=STR,FMT='(2(A,I3))') 'F',PREC+RES,'.',PREC
         CALL CRUNCH(STR)
C
C        get word "Entity"
         CALL VNWORD(177,WORD1)
C        get word "Dimension"
         CALL VNWORD(21,WORD2)
         OLIN=' '
         WRITE(UNIT=OLIN,FMT='(3A)')
     +      WORD1(1:NLEN1(WORD1)),
     +      ':',
     +      WORD2(1:NLEN1(WORD2))
C        print to screen
         TXTCNT=TXTCNT+1
         TEXTO(TXTCNT) = OLIN
         CALL CPRINT(OLIN)
C
C        set format for writing dimension parameters
         WRITE(UNIT=FORM,FMT='(5A)')
     +   '(2A,', STR(1:NLEN1(STR)), ',3A,', STR(1:NLEN1(STR)), ')'
         II=TEXSEG
         JJ=TERMIN
         KK=0
         DO 101 I=2,RECCNT(1)
C           Check supression state of sub-record entity
            IF ( IWORK(4,I) .GE. 0 ) THEN
C              was not surpressed so continue
               NOFIND=.FALSE.
               IF (IWORK(1,I) .EQ.II ) THEN
C                 get word "Text Height"
                  CALL VNWORD(262,WORD1)
C                 get word "Text Width"
                  CALL VNWORD(263,WORD2)
                  OLIN=' '
C                 was a text record so use text buffer as well
                  WRITE(UNIT=OLIN,FMT=FORM)
     +              WORD1(1:NLEN1(WORD1)),
     +              ':',
     +              RWORK(4,I),
     +              ', ',
     +              WORD2(1:NLEN1(WORD2)),
     +              ':',
     +              RWORK(3,I)
                  NOFIND=.TRUE.
                  II=0
               ELSE IF ( IWORK(1,I) .EQ.JJ ) THEN
C                 was arrowhead so must decode the arrowhead parameters.
C                 get word "Arrow Length"
                  CALL VNWORD(265,WORD1)
C                 get word "Arrow Width"
                  CALL VNWORD(266,WORD2)
                  OLIN=' '
C                 was a text record so use text buffer as well
                  WRITE(UNIT=OLIN,FMT=FORM)
     +              WORD1(1:NLEN1(WORD1)),
     +              ':',
     +              RWORK(5,I),
     +              ', ',
     +              WORD2(1:NLEN1(WORD2)),
     +              ':',
     +              RWORK(6,I)
                  NOFIND=.TRUE.
                  JJ=0
               END IF
               IF ( NOFIND ) THEN
                  IF (KK.GT.0 ) THEN
                     KK=0
                  ELSE
                     KK=KK+1
                  END IF
                  TXTCNT=TXTCNT+1
                  TEXTO(TXTCNT) = OLIN
               END IF
            END IF
 101     CONTINUE
C     ************************ DISPLAY LINE 3 **************
C        create format for line 3 of output
         FORM=' '
         WRITE(UNIT=FORM,FMT='(A)')
     +   '(A,I3,2A,I3,5A)'
C        get word "Layer:'
         CALL VNWORD(195,WORD2)
C        get word "Thick:'
         CALL VNWORD(409,WORD4)
C        get word "Colour:'
         CALL VNWORD(399,WORD5)
C        Get colour name.
         I = IMBUFF(3) + 413
         CALL VNWORD(I,WORD7)
C        get word "Form:'
         CALL VNWORD(128,WORD1)
C        write to buffer for printing
C        in form "Layer:lll, Thick:ttt, Colour:cc"
         OLIN=' '
         WRITE(UNIT=OLIN,FMT=FORM)
     +      WORD2(1:NLEN1(WORD2)),
     +      IMBUFF(4),
     +      ', ',
     +      WORD4(1:NLEN1(WORD4)),
     +      IMBUFF(12),
     +      ', ',
     +      WORD5(1:NLEN1(WORD5)),
     +      WORD7(1:NLEN1(WORD7)),
     +      ', ',
     +      GWORD(1:NLEN1(GWORD))
C        print to screen,wait for key
         TXTCNT=TXTCNT+1
         TEXTO(TXTCNT) = OLIN
C   
         CALL INFODIALOG(TEXTO,TXTCNT)
C
      END IF
C
      END
C
C     ---------------------------------------------------------
C
