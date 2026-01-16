C
C     @(#)  412.1 date 6/11/92 menu.f 
C
C
C     Filename    : menu.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:40:22
C     Last change : 92/06/11 14:35:06
C
C     Copyright : Practical Technology Limited  
C     File :- menu.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE FNDPOS(VNNUM,VPOSN)
C     SUBROUTINE FNDTOK(VNNUM,TOKEN)
C     SUBROUTINE LDLCON(FILNAM,ST)
C     SUBROUTINE LDLMRK(FILNAM,ST)
C     SUBROUTINE LDLPAP(FILNAM,ST)
C     SUBROUTINE LDLSTY(FILNAM,ST)
C     SUBROUTINE LDLTHK(FILNAM,ST)
C     SUBROUTINE LODFNT(FILNAM,ST)
C     SUBROUTINE LODPRT(FILNAM,ST)
C     SUBROUTINE LODVNT(FILNAM,ST)
C     SUBROUTINE MNICAL()
C     SUBROUTINE MNIMAS
C     SUBROUTINE MNIVPT(GO)
C     SUBROUTINE MNLPTS
C     SUBROUTINE MNLTX1
C     SUBROUTINE MNUPTS
C     SUBROUTINE PRWORD(DNUM,STRING)
C     SUBROUTINE SUTIL1()
C     SUBROUTINE SAVPIC()
C     SUBROUTINE SUTILS(HILITE)
C     SUBROUTINE VNWORD(VNNUM,VWORD)C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE FNDPOS(VNNUM,VPOSN)
C     ==============================
C
C1    vartype            I4    I4
C1    iostatus           I      O
C2
C2    Subroutine FNDPOS returns the cell position for
C2    VNNUM in the verb-noun table.
C
      include 'include/vntable.inc'
C
      INTEGER*4 VNNUM,VPOSN
C
      VPOSN=VNPOS(VNNUM)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE FNDTOK(VNNUM,TOKEN)
C     ==============================
C
C1    vartype            I4    C*1
C1    iostatus           I      O
C2
C2    Subroutine FNDTOK returns the token from position
C2    VNNUM in the verb-noun table.
C
      include 'include/vntable.inc'
C
      INTEGER*4 VNNUM
      CHARACTER TOKEN*1
C
      TOKEN=VNTOKE(VNNUM)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE LDLCON(FILNAM,ST)
C     ============================
C1    iotype              C*(*) I4
C1    ioostat             I     O
C2
C2    This routine is used to load the configurartion file
C2    and set the configuration parameters whichare currently held
C2    in config.inc . This will require to use the preprocessor of
C2    John_l in future and it is hoped to store the variables not in
C2    an explicit common block but in a calulator variable thus making
C2    it accessable to macros etc.
C2
      include  'include/config.inc'
C
      CHARACTER*(*) FILNAM,LIN*80
      INTEGER ST,UNITN,TVAL
      LOGICAL OK,EX
C
      ST = 0
C     Set the default levels etc
C     Set compact threashold at 20 percent
      CMPVAL = 20
C     set percentage as ture
      PERCNT = .TRUE.
C     set newcopact as false (use old compact by default)
      NEWCMP = .FALSE.
c     set checkit default as flase
      CHEKIT = .FALSE.
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF (EX) THEN
C       find a unit to open the file on
        CALL FINDU1(UNITN,OK)
        IF (.NOT.OK) THEN
C          set status to indicate no units available
           ST=3
           RETURN
        END IF
        OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +       FORM='FORMATTED',ERR=30)
        REWIND(UNIT=UNITN)
 10     CONTINUE
        READ(UNIT = UNITN,FMT = '(A)' ,END = 20 ,ERR = 99) LIN
        CALL CRUNCH(LIN)
        CALL FOLDUP(LIN)
        IF(LIN(1:1) .EQ. '*') GOTO 10
        IF(LIN(1:10) .EQ. 'NEWCOMPACT') THEN
           NEWCMP = (LIN(11:14).EQ. 'TRUE')
        ELSE IF (LIN(1:10).EQ. 'COMPERCENT') THEN
           PERCNT = (LIN(11:14).EQ. 'TRUE')
        ELSE IF (LIN(1:9) .EQ. 'COMPLEVEL') THEN
           CALL IVALU(LIN(10:),TVAL,OK)
           IF(OK) CMPVAL= TVAL
        ELSE IF (LIN(1:12).EQ. 'CHECKDRAWING') THEN
           CHEKIT = (LIN(13:16).EQ. 'TRUE')
        END IF
        GOTO 10
 20     CONTINUE
        CLOSE(UNITN)
      ELSE
        ST = 1
      END IF
      RETURN
 30   CONTINUE
      ST = 1
      WRITE(10,*) '[LDLCON] Error opening file ',FILNAM
      RETURN
 
 99   CONTINUE
      ST = 1
      WRITE(10,*) '[LDLCON] Error reading file ',FILNAM
      END
 
 
 
 
 
 
      SUBROUTINE LDLMRK(FILNAM,ST)
C     ============================
C
      include 'include/lfont.inc'
      include 'include/marker.inc'
 
      INTEGER*4 ST,UNITN,I,SEGCNT,SEGMNT,OP,COUNT,DCOUNT
      LOGICAL OK,EX
      REAL XVAL,YVAL,XTMIN,YTMIN,XTMAX,YTMAX,RADS,SANG,EANG,TRS(3,3)
      REAL CTX,CTY,ANGLE,OX,OY,NX,NY,WRAD,PI,RAD,CIRDIF
      CHARACTER*(*) FILNAM,INLINE*10
      EXTERNAL PI,RAD
C
C     clear maximum no of marker definitions
      DO 1 I=1,MRKRMX
        MRKR(I,1)=0.0
        MRKR(I,2)=0.0
        MRKI(I)  =0
 1    CONTINUE
C     clear maximum no of index definitions
      DO 2 I=1,MRKIMX
        MRKIND(I)=0
 2    CONTINUE
C     clear maximum no of aux data  definitions
      DO 3 I=1,AUXDMX
        AUXDAT(I)=0
 3    CONTINUE
C     clear maximum no of marker text definitions
      DO 4 I=1,TXTCMX
        TXTPNT(I,1)=0
        TXTPNT(I,2)=0
        TXTDAT(I) = '  '
 4    CONTINUE
C
      MRKRCT=0
      MRKICT=0
      AUXDCT=0
      TXTCNT=0
C
      MRKRCR=0
      MRKICR=0
      AUXDCR=0
      TXTCNR=0
      MRKNUM=0
C
 5    CONTINUE
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF (EX) THEN
C       find a unit to open the file on
        CALL FINDU1(UNITN,OK)
        IF (.NOT.OK) THEN
C          set status to indicate no units available
           ST=3
           RETURN
        END IF
        OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +       FORM='FORMATTED',ERR=30)
        REWIND(UNIT=UNITN)
 10     CONTINUE
        READ(UNIT=UNITN,FMT='(A)',END=20) INLINE
C       comment line ignore.
        IF ( INLINE(1:1).EQ.'*' )GOTO 10
        BACKSPACE(UNIT=UNITN)
        READ(UNIT=UNITN,FMT=*,ERR=30,END=20) MRKNUM,SEGMNT
        IF ( MRKNUM.LT.1.OR.MRKNUM.GT.MRKIMX ) THEN
           WRITE(*,*) 'Illegal marker index no in definition'
           WRITE(*,*) 'Marker no:',MRKNUM
           GOTO 10
        END IF
        IF ( SEGMNT.LT.1.OR.SEGMNT+MRKRCT.GT.MRKRMX ) THEN
           WRITE(*,*) 'Illegal marker index no in definition'
           WRITE(*,*) 'Marker no:',MRKNUM
           GOTO 10
        END IF
C       increment the real definition counter
        MRKRCT=MRKRCT+1
C       store the number of definition segments
        MRKI(MRKRCT)=SEGMNT+3
c       incriment the text counter
        TXTCNT=TXTCNT+1
        READ(UNIT=UNITN,FMT='(A20)',ERR=30,END=20) TXTDAT(TXTCNT)
c       set the pointer back to the definition of marker
        TXTPNT(TXTCNT,1)=MRKRCT
c       set the index to the marker to point to the definition
        MRKIND(MRKNUM)= MRKRCT
c       dont know the limits so skip over them
        MRKRCT=MRKRCT+2
        XTMIN=0.0
        YTMIN=0.0
        XTMAX=0.0
        YTMAX=0.0
C
        SEGCNT=0
 15     CONTINUE
        READ(UNIT=UNITN,FMT=*,ERR=30,END=20) XVAL,YVAL,OP
D         WRITE(10,*) '[LDLMRK] SEGCNT,SEGMNT,XVAL,YVAL,OK ',
D     +         SEGCNT,SEGMNT,XVAL,YVAL,OK
C      ----------------------------------
C       arc definition
C      ----------------------------------
       IF(OP.EQ.5) THEN
          DCOUNT = 0
          MRKR(MRKRCT,1)=XVAL
          MRKR(MRKRCT,2)=YVAL
          MRKI(MRKRCT)= OP
          MRKRCT=MRKRCT+1
          CTX=XVAL
          CTY=YVAL
          READ(UNIT=UNITN,FMT=*,ERR=30,END=20) XVAL,YVAL,OP
          MRKR(MRKRCT,1)=XVAL
          MRKR(MRKRCT,2)=YVAL
          MRKI(MRKRCT)= 5
          MRKRCT=MRKRCT+1
          WRAD=XVAL
          CIRDIF = YVAL
          READ(UNIT=UNITN,FMT=*,ERR=30,END=20) XVAL,YVAL,OP
          MRKR(MRKRCT,1)=XVAL
          MRKR(MRKRCT,2)=YVAL
          MRKI(MRKRCT)= OP
          MRKRCT=MRKRCT+1
          SANG=RAD(XVAL)
          EANG=RAD(YVAL)
          AUXDCT=AUXDCT+1
          MRKI(MRKIND(MRKNUM)+1)=AUXDCT
          AUXDAT(AUXDCT)=MRKIND(MRKNUM)
          AUXDCT=AUXDCT+1
          AUXDAT(AUXDCT)=TXTPNT(TXTCNT,1)
          ANGLE=ABS(EANG-SANG)
          IF ( EANG .LT. SANG ) ANGLE=PI(2.0)-ANGLE
C         Recalculate the angle to obtain an whole value which
C         will draw a complete arc.
c         COUNT=MIN(1+ABS(NINT(ANGLE/RADS)),MAXN)
C         Recalculate the angle to obtain an whole value which
C         will draw a complete arc.
          COUNT=OP
 11       CONTINUE
          RADS=ANGLE/REAL(COUNT)
C         Obtain the transformation matrice which rotates about
C         the centre of the circle X,Y by the angle RADS.
          CALL ROTP2D(CTX,CTY,RADS,TRS)
C         starting point of the drawing of the circle.
          OX=CTX+WRAD*COS(SANG)
          OY=CTY+WRAD*SIN(SANG)
C         loop for the whole number calculated above.
          OK=.FALSE.
          MRKR(MRKRCT,1)=OX
          MRKR(MRKRCT,2)=OY
          MRKI(MRKRCT)= 0
          MRKRCT=MRKRCT+1
          DO 12 I=1,COUNT,1
C           transform OX to NX by transformation matrice
C           and also OY to NY.
            CALL NEWXY(OX,OY,NX,NY,TRS)
C           Draw between old point and new.
            XTMIN=MIN(XTMIN,NX)
            YTMIN=MIN(YTMIN,NY)
            XTMAX=MAX(XTMAX,NX)
            YTMAX=MAX(YTMAX,NY)
            MRKR(MRKRCT,1)=NX
            MRKR(MRKRCT,2)=NY
            MRKI(MRKRCT)= 1
            MRKRCT=MRKRCT+1
C           old now new point on the circumference
            OX=NX
            OY=NY
 12       CONTINUE
          DCOUNT = DCOUNT+COUNT+1
          IF (CIRDIF.GT.0.0.AND.WRAD-CIRDIF.GT.0.0) THEN
              WRAD= WRAD - CIRDIF
              GOTO 11
          END IF
          SEGCNT=SEGCNT+DCOUNT+3
          SEGMNT=SEGMNT+DCOUNT
          MRKI(MRKIND(MRKNUM))=SEGMNT+2
          IF(SEGMNT.NE.SEGCNT) GOTO 15
        ELSE IF (OP .EQ. 99) THEN
C         Must be a parameter block
          MRKR(MRKRCT,1)=XVAL
          MRKR(MRKRCT,2)=YVAL
          MRKI(MRKRCT)= OP
          MRKRCT=MRKRCT+1
          READ(UNIT=UNITN,FMT=*,ERR=30,END=20) XVAL,YVAL,OP
          MRKR(MRKRCT,1)=XVAL
          MRKR(MRKRCT,2)=YVAL
          MRKI(MRKRCT)= OP
          MRKRCT=MRKRCT+1
          SEGCNT=SEGCNT+2
          IF(SEGMNT.NE.SEGCNT) GOTO 15
        ELSE
c        ----------------------
C          must be a a line
c        ----------------------
           SEGCNT=SEGCNT+1
           XTMIN=MIN(XTMIN,XVAL)
           YTMIN=MIN(YTMIN,YVAL)
           XTMAX=MAX(XTMAX,XVAL)
           YTMAX=MAX(YTMAX,YVAL)
           MRKR(MRKRCT,1)=XVAL
           MRKR(MRKRCT,2)=YVAL
           MRKI(MRKRCT)= OP
           MRKRCT=MRKRCT+1
           IF(SEGMNT.NE.SEGCNT) GOTO 15
        END IF
C       only allow one segment definition for the time being
C       Now go and store the extents on the marker
        MRKR(MRKIND(MRKNUM),1)=XTMIN
        MRKR(MRKIND(MRKNUM),2)=YTMIN
        MRKR(MRKIND(MRKNUM)+1,1)=XTMAX
        MRKR(MRKIND(MRKNUM)+1,2)=YTMAX
c       set the number of segments to the number of elements
        MRKI(MRKIND(MRKNUM)+2)=SEGCNT
C
        GOTO 10
 30     CONTINUE
C       set status for read error
        ST=1
C       close the file
        GOTO 25
 20     CONTINUE
        ST=0
        DO 40 I=1,MRKRCT
D          WRITE(10,*) '[LDLMKR] DEFINITION DATA = ',MRKR(I,1),
D    +     MRKR(I,2),MRKI(I)
 40     CONTINUE
 
        DO 50 I=1,TXTCNT
D          WRITE(10,*) '[LDLMKR] TEXT DATA = ',TXTPNT(I,1),TXTPNT(I,2),
D    +                  TXTDAT(I)
 50     CONTINUE
D       DO 60 I=1,MRKIMX
D          WRITE(10,*) '[LDLMRK] INDEX DATA = ',MRKIND(I)
 60     CONTINUE
C
 25     CONTINUE
        CLOSE(UNIT=UNITN,STATUS='KEEP')
      ELSE
C       set status to indicate no file found
        ST=2
      END IF
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE LDLPAP(FILNAM,ST)
C     ============================
C
      include 'include/params.inc'
      include 'include/lfu.inc'
 
      INTEGER*4 ST,UNITN,I,LNO
      LOGICAL OK,EX
      CHARACTER*(*) FILNAM,INLINE*10
C
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +        FORM='FORMATTED',ERR=30)
         REWIND(UNIT=UNITN)
         LFU(UNITN)=.TRUE.
         LNO=0
 10      CONTINUE
         LNO=LNO+1
         READ(UNIT=UNITN,FMT='(A2,1X,A14)',END=20,ERR=30)
     +                     PAPLST(LNO),PAPNAM(LNO)
         READ(UNIT=UNITN,FMT=*,END=20,ERR=30)
     +          PAPSIZ(1,LNO),PAPSIZ(2,LNO)
         IF(LNO.LT.5)  GOTO 10
         DRWSHT=PAPLST(1)
         DRWSIZ(1)=PAPSIZ(1,1)
         DRWSIZ(2)=PAPSIZ(2,1)
         GOTO 20
 
 30      CONTINUE
C        set status for read error
         ST=1
C        close the file
         GOTO 25
 20      CONTINUE
         ST=0
C
 25      CONTINUE
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
      END IF
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE LDLSTY(FILNAM,ST)
C     ============================
C
      include 'include/lfont.inc'
      include 'include/lfu.inc'
 
      INTEGER*4 ST,UNITN,P1,P2,NLEN,LENGTH,TMP,
     +    POINT,TPOINT,SPOINT,FONTN
      LOGICAL OP,OK,EX
      DOUBLE PRECISION DANS
      REAL VAL,TOTAL,TEMVAL(20)
      CHARACTER*(*) FILNAM,STRNG2*100,NAME*20
      EXTERNAL NLEN
C
 
      DO 1 P1=0,20
         LFONTI(1,P1)=1
         LFONTI(2,P1)=1
         LFONTR(1,P1)=1.0
         LFONTR(2,P1)=1.0
         FONTNM(P1)='                                  '
 1    CONTINUE
      DO 2 P1=1,100
         LFTAB2(1,P1)=1.0
         LFTAB2(2,P1)=1.0
         LFTAB2(3,P1)=1.0
 2    CONTINUE
 
 5    CONTINUE
C
      INQUIRE(FILE=FILNAM,EXIST=EX,OPENED=OP)
C     wait for access to file if necessary
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +        FORM='FORMATTED')
         REWIND(UNIT=UNITN)
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
C
C        set font number
         FONTN=0
         TPOINT=0
C
 10      CONTINUE
C        increment font number.
         FONTN=FONTN+1
C        reset total length.
         TOTAL=0.0
C        temporary pointer to values.
         TMP=0
C
 11      CONTINUE
         READ(UNIT=UNITN,FMT='(A)',END=20,ERR=30)STRNG2
C        check for blank lines ignore them.
         IF ( NLEN(STRNG2).EQ.0 ) GOTO11
         IF ( STRNG2(1:1) .EQ.'*') THEN
              GOTO 11
         ENDIF
C        correct into upper case.
         CALL FOLDUP(STRNG2)
C        save active length of string.
         LENGTH=NLEN(STRNG2)
C        find the first quote
         P1=INDEX(STRNG2,'"')
         IF ( P1.EQ. 0 ) GOTO 30
         P2=INDEX(STRNG2(P1+1:),'"')+P1
         IF ( P2.EQ. 0 ) GOTO 30
         NAME=STRNG2(P1+1:P2-1)
 
 15      CONTINUE
         TMP=TMP+1
C
 12      CONTINUE
            P2=P2+1
         IF ( STRNG2(P2:P2).EQ. ' '.AND.P2.LT.LENGTH) GOTO 12
         P1=P2
 13      CONTINUE
            P1=P1+1
         IF ( STRNG2(P1:P1).NE. ' '.AND.P1.LT.LENGTH) GOTO 13
C
         CALL AEXPRN(STRNG2(P2:P1),DANS,*30)
         VAL=REAL(DANS)
         TOTAL=TOTAL+VAL
         TEMVAL(TMP)=VAL
         P2=P1
         IF ( P2 .LT. LENGTH ) GOTO 15
C
C        font definition read completely now reconstruct it
         IF ( MOD(TMP,2).NE.0 .AND.FONTN.NE.1 ) THEN
            CALL DEPRNT(364)
            GOTO 30
         END IF
C        save font name.
         FONTNM(FONTN)=NAME
C
C        start at the top of LFTAB2
         POINT=0
 
 35      CONTINUE
         POINT=POINT+1
C
         LFTAB2(1,TPOINT+POINT)=TEMVAL(POINT)/TOTAL
C        set to unity for safety
         LFTAB2(2,TPOINT+POINT)=1.0
         LFTAB2(3,TPOINT+POINT)=1.0
C
         IF ( POINT.LT. TMP ) GOTO 35
C
         LFONTI(1,FONTN)=TPOINT+1
         LFONTI(2,FONTN)=POINT
C
         LFONTR(1,FONTN)=TOTAL
         LFONTR(2,FONTN)=TOTAL
C
         TPOINT=TPOINT+POINT
C
         IF ( FONTN.GT.19 ) THEN
            FONTN=FONTN+1
            CALL DEPRNT(365)
C           go on as if the was nothing wrong.
            GOTO 20
         ELSE
            GOTO 10
         END IF
 30      CONTINUE
C        set status for read error
         ST=1
C        close the file
         GOTO 25
 20      CONTINUE
         ST=0
C
         NFONTS=FONTN-1
C
 25      CONTINUE
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
      END IF
C
      END
C
C-----------------------------------------------------------------
C
C
      SUBROUTINE LDLTHK(FILNAM,ST)
C     ============================
C
      include 'include/lfont.inc'
      include 'include/lfu.inc'
      include 'include/menpop.inc'
      include 'include/thklst.inc'

      INTEGER*4 ST,UNITN,I,INDX,ADD,END,OVER,LNO
      LOGICAL OK,EX
      REAL R1,R2
      CHARACTER*(*) FILNAM,INLINE*10
C
C     maximum no of line thickness definition 
      NUMTHK = 0
      DO 2 I=0,127
C         Note 82944 translates into a default "over"
C         value of 10 (for gerber tool selection)
C         also a default "end" value of 1
         PLTHKI(I)=I+1024
C        default line thickness
         LTHKR(1,I)=1.0
C        default pen thickness
         LTHKR(2,I)=0.25
 2    CONTINUE

C     first line has no thickness 
      PLTHKI(0)=0
      LTHKR(1,0)=0.0
C     default pen thickness
      LTHKR(2,0)=0.25
      LTHKR(2,127)=10.0
 5    CONTINUE
C
      INQUIRE(FILE=FILNAM,EXIST=EX)
      IF (EX) THEN
C       find a unit to open the file on
        CALL FINDU1(UNITN,OK)
        IF (.NOT.OK) THEN
C          set status to indicate no units available
           ST=3
           RETURN
        END IF
        OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +       FORM='FORMATTED',ERR=30)
        REWIND(UNIT=UNITN)
        LFU(UNITN)=.TRUE.
        LNO=0
        DFSTOT = 0
 10     CONTINUE
        READ(UNIT=UNITN,FMT='(A)',END=20) INLINE
C       comment line ignore.
        IF ( INLINE(1:1).EQ.'*' )GOTO 10
        BACKSPACE(UNIT=UNITN)
        READ(UNIT=UNITN,FMT=*,ERR=30,END=20) I,R1,R2,END,OVER
        LNO=LNO+1
        IF ( I.LT.1.OR.I.GT.127 ) THEN
           WRITE(*,*) 'Illegal index no in line thickness definition'
           WRITE(*,*) 'Line no:',LNO
           GOTO 10
        END IF
        IF ( END.LT.1.OR.END.GT.4 ) THEN
           WRITE(*,*) 
     +     'Illegal line end type in line thickness definition'
           WRITE(*,*) 'Line no:',LNO
           GOTO 10
        ELSE IF ( OVER.GT.99.OR.OVER.LT.0 ) THEN
c           WRITE(*,*) 
c     +     'Override value out of range'
c           WRITE(*,*) 'Line no:',LNO
           GOTO 10
        END IF
C
        INDX=OVER*8192+END*1024+I
        PLTHKI(I)=INDX
        LTHKR(1,I)=R1
        LTHKR(2,I)=R2
        IF (I.GT.NUMTHK) NUMTHK = I
C       Record the definition number in the ftndfs list.
        DFSTOT = DFSTOT + 1
        FNTDFS(DFSTOT) = I
C
        GOTO 10
 30     CONTINUE
C       set status for read error
        ST=1
C       close the file
        GOTO 25
 20     CONTINUE
        ST=0
C
 25     CONTINUE
        CLOSE(UNIT=UNITN,STATUS='KEEP')
        LFU(UNITN)=.FALSE.
      ELSE
C       set status to indicate no file found
        ST=2
      END IF
C
      END
C
C-----------------------------------------------------------------
C

      SUBROUTINE LODFNT(FILNAM,ST)
C     ============================
C
      include 'include/daxfont.inc'
      include 'include/lfu.inc'
 
      INTEGER*4 II,X,ST,UNITN
      INTEGER*2 LOOP,I
      LOGICAL OP,EX,OK
      CHARACTER*(*) FILNAM
C
      ST=0
C
 111  CONTINUE
        
      INQUIRE(FILE=FILNAM,NUMBER=II,EXIST=EX,IOSTAT=X,OPENED=OP)
C
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
C
C
C        load font 1
         OPEN (UNIT=UNITN,FILE=FILNAM,ACCESS='DIRECT',
     +   FORM='UNFORMATTED',RECL=4,STATUS='OLD',ERR=222)
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
C
         READ(UNIT=UNITN,REC=1,ERR=222) LOOP,LOOP
C         print*, 'LOOPCOUNT',LOOP
         DO 220 I=1,LOOP-1
            READ(UNIT=UNITN,REC=I,ERR=222) FONT1(I)
C		    print*, 'VAL',I,FONT1(i)
 220     CONTINUE
         CLOSE(UNIT=UNITN)
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
         RETURN
 222     CONTINUE
         ST=1
      ELSE
C        set status to indicate no file found
         ST=2
      END IF
 
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE LODPRT(FILNAM,ST)
C     ============================
C2
C2    Subroutine LODPRT loads the prompt messages
C2    into the internal files from an external file.
C2    Completion status is returned in ST.
C
      include 'include/vntable.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 VNNUM,UNITN,VNP,P1,P2,LNUM,ST
      INTEGER*4 NLEN
      LOGICAL EX,OK,OP
      CHARACTER TOKEN*1,WORD*80,FILNAM*(*),STRNG2*100,STRING*80
      EXTERNAL NLEN
C
 3    CONTINUE
C     clear all words to nulls
CAPOLLO|SUN|PC386
      DO 5 VNNUM=1,VNPMAX
CAPOLLO|SUN|PC386
CIBM
C      DO 5 VNNUM=1,420
CIBM
         DICT01(VNNUM)=' '
 5    CONTINUE
C
      LNUM=0
      INQUIRE(FILE=FILNAM,EXIST=EX,OPENED=OP)
C     wait for access to file if necessary
      IF (EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            WRITE(UNIT=*,FMT=*)'[LODPRT] no units available'
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL',
     +        FORM='FORMATTED',STATUS='OLD')
 
         REWIND(UNIT=UNITN)
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
 10      CONTINUE
         LNUM=LNUM+1
         READ(UNIT=UNITN,FMT='(A)',END=20)STRNG2
C        test for null line and ignore
         IF (NLEN(STRNG2).EQ.0) GOTO 10
C
C        Check for comments in prompt file
C
         IF(STRNG2(1:1).EQ.'*' ) THEN
              GOTO 10
         ENDIF
C
         READ(STRNG2,FMT='(I6,A)',ERR=30) VNP,STRING
C        find delimiters for character string
         P1=INDEX(STRING,'"')
         P2=INDEX(STRING(P1+1:),'"')
         IF (P2.GT.0) THEN
            P2=P1+P2
         ELSE
            P2=P1
         END IF
         OK=P1.NE.0.AND.P2.NE.0.AND.P2.GT.P1
C
         IF (OK) THEN
            IF ( STRING(P1+1:P2-1).EQ.'$') THEN
               WRITE(STRING,FMT='(A7,I4)') 'Prompt ',VNP
               P1=0
               P2=12
            END IF
C
C           read the command word
C           test for null string
            IF ((P2-P1).LE.1) THEN
C              null string
               WORD=' '
            ELSE
               WORD=STRING(P1+1:P2-1)
            END IF
         ELSE
C           cannot delimit command word
            WRITE(UNIT=*,FMT=*)'[LODPRT] Error on line ',LNUM,
     +            ' No Command word'
C            WRITE(10,*) VNP,P1,P2
C            WRITE(10,*) STRNG2
C            WRITE(10,*) STRING
            GOTO 10
         END IF
C         WRITE(10,*) 'VNP',VNP
C        write word to list
         DICT01(VNP)=WORD
C
         GOTO 10
 30      CONTINUE
C        set status for read error
         WRITE(UNIT=*,FMT=*)'[LODVNT] Read error in ',FILNAM,
     +                       ' at line ',LNUM
         ST=1
         WRITE(UNIT=*,FMT=*)'[LODVNT] read error'
         GOTO 25
 20      CONTINUE
         ST=0
         READ(UNIT=DICT01(1)(14:16),FMT='(I3)') LANGID
 25      CONTINUE
C        close the file
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
         WRITE(UNIT=*,FMT=*)'[LODVNT] no system file available'
      END IF
C
      END
C
C-----------------------------------------------------------------
C
C-----------------------------------------------------------------
C
      SUBROUTINE LODVNT(FILNAM,ST)
C     ============================
C2
C2    Subroutine LODVNT loads the verb-noun table
C2    into the internal files from an external file.
C2    Completion status is returned in ST.
C
      include 'include/vntable.inc'
      include 'include/lfu.inc'
C
      INTEGER*4 VNNUM,UNITN,VNP,P1,P2,LNUM,ST
      INTEGER*4 NLEN
      LOGICAL EX,OK,OP
      CHARACTER TOKEN*1,WORD*16,FILNAM*(*),STRNG2*80,STRING*68
      EXTERNAL NLEN
C
 3    CONTINUE
C     clear all words to nulls
      DO 5 VNNUM=1,VNPMAX
         VNOUN(VNNUM)=' '
 5    CONTINUE
C
      LNUM=0
      INQUIRE(FILE=FILNAM,EXIST=EX,OPENED=OP)
C     wait for access to file if necessary
      IF(EX) THEN
C        find a unit to open the file on
         CALL FINDU1(UNITN,OK)
         IF (.NOT.OK) THEN
C           set status to indicate no units available
            ST=3
            RETURN
         END IF
         OPEN(UNIT=UNITN,FILE=FILNAM,ACCESS='SEQUENTIAL')
         REWIND(UNIT=UNITN)
CIBM
C         LFU(UNITN)=.TRUE.
CIBM
 10      CONTINUE
         LNUM=LNUM+1
         READ(UNIT=UNITN,FMT='(A)',END=20,ERR=30)STRNG2
C        test for null line and ignore
         IF (NLEN(STRNG2).EQ.0) GOTO 10
C        test for comment line
C        skip over PT internal comments
         IF (STRNG2(1:1).EQ.'*' .OR. STRNG2(1:1).EQ.'$') GOTO 10
         READ(STRNG2,FMT='(2I6,A)',END=20)VNNUM,VNP,STRING
C        find delimiters for character string
         P1=INDEX(STRING,'/')
         P2=INDEX(STRING(P1+1:),'/')
         IF (P2.GT.0) THEN
            P2=P1+P2
         ELSE
            P2=P1
         END IF
         OK=P1.NE.0 .AND. P2.NE.0 .AND. P2.GT.P1
         IF (OK) THEN
C           read the command word
C           test for null string
            IF ((P2-P1).EQ.1) THEN
C              null string
               WORD=' '
            ELSE
               WORD=STRING(P1+1:P2-1)
            END IF
         ELSE
C           cannot delimit command word
            GOTO 10
         END IF
C        write word to list
         VNOUN(VNNUM)=WORD
C        write cell position
         VNPOS(VNNUM)=VNP
         GOTO 10
 30      CONTINUE
C        set status for read error
         ST=1
         GOTO 25
 20      CONTINUE
         ST=0
 25      CONTINUE
C        close the file
         CLOSE(UNIT=UNITN,STATUS='KEEP')
CIBM
C         LFU(UNITN)=.FALSE.
CIBM
      ELSE
C        set status to indicate no file found
         ST=2
         WRITE(UNIT=*,FMT=*)'[LODVNT] no system file available'
      END IF
C
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE MNICAL()
C     ===================
C1    no arguments required
C
C2
      EXTERNAL GTDMEN
C
C     Define position for last line two options in menu.
C
C2    C is the token for CALCULATOR
      CALL GTCLRM(1)
      CALL GTDMEN(69,1)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE MNIMAS
C     =================
C1    No arguments required.
C
C2    Loads the MASTER MENU options and
C2    enters the DISPLAY menu routine to
C2    load the display options.
C
      INTEGER I
      EXTERNAL GTDMEN,GTDMHD,GTCLRM,MNIDIS,DCPRNT
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C     Clear the major option menu.
      CALL GTCLRM(2)
C     Enter the master menu options.
      I=1
      CALL GTDMHD(1,3)
      CALL DCPRNT(260)
C2    s is the token for SELECT
      CALL GTDMEN(14,3)
C
C2    AutoCAD cell
C      CALL GTPMEN('** AutoCAD **','X',3,5)
C
C2    T is the token for REGENERATE.
      CALL GTDMEN(15,3)
C2    I is the token for INSERT.
      CALL GTDMEN(16,3)
C2    t is the token for TRIM
      CALL GTDMEN(17,3)
C2    P is the token for MOVE.
      CALL GTDMEN(18,3)
C2    O is the token for EXTRACT.
      CALL GTDMEN(33,3)
C2    A is the token for CHANGE
      CALL GTDMEN(19,3)
C2    D is the token for ERASE
      CALL GTDMEN(20,3)
C2    d is the token for DIMENSION.
      CALL GTDMEN(21,3)
C2    m is the token for MEASURE.
      CALL GTDMEN(22,3)
C2    V is the token for CREATE
      CALL GTDMEN(23,3)
C2    D is the token for EXPLODE
      CALL GTDMEN(24,3)
C2    R is the token for READ
      CALL GTDMEN(25,3)
C2    W is the token for WRITE
      CALL GTDMEN(26,3)
C2    p is the token for PLOT
      CALL GTDMEN(27,3)
C2    z is the token for SAVE to workfile
      CALL GTDMEN(28,3)
C2    Z is the token for temporary
      CALL GTDMEN(29,3)
C2    F is the token for FINISH.
      CALL GTDMEN(30,3)
C     load ATTACH
      CALL GTDMEN(31,3)
C     load DEFINE
      CALL GTDMEN(32,3)
C2    Y is the token for SPECIAL
      CALL GTDMEN(34,3)
C
      END
C
C**************************************************
C**************************************************
C     end of menu3 initialization routines
C**************************************************
C**************************************************
C
C
C-----------------------------------------------------------------
C
      SUBROUTINE MNIVPT(GO)
C     ===================
C1    no arguments required
C
C2    This loads up the minor vieport menu
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/vnames.inc'
      include 'include/vntable.inc'
 
      INTEGER*4 VNUM
      CHARACTER*16 WORD
      LOGICAL GO
      EXTERNAL GTDMEN GTDMCH
C
      CALL GTCLRM(4)
      IF(GO) THEN
C         the VERB cell
          CALL GTDMEN(50,4)
 
C         Views 1 to 5
 
          WORD = 'View '//VPNAME(1)
          VNOUN(439) = WORD
          CALL GTDMEN(439,4)
 
          WORD = 'View '//VPNAME(2)
          VNOUN(440) = WORD
          CALL GTDMEN(440,4)
 
          WORD = 'View '//VPNAME(3)
          VNOUN(441) = WORD
          CALL GTDMEN(441,4)
 
          WORD = 'View '//VPNAME(4)
          VNOUN(442) = WORD
          CALL GTDMEN(442,4)
 
          WORD = 'View '//VPNAME(5)
          VNOUN(443) = WORD
          CALL GTDMEN(443,4)
 
C         Define view cell
          CALL GTDMEN(445,4)
          IF(DISLAY) THEN
              CALL GTDMEN(57,4)
C
C             put grid cell in
              CALL GTDMEN(59,4)
              IF ( SETGRD ) THEN
                 CALL GTDMCH(59,4)
              END IF
C
          ELSE
              CALL GTDMEN(66,4)
          ENDIF
      ELSE
          CALL MNIDIS()
      ENDIF
      END
 
 
 
 
 
      SUBROUTINE MNLPTS
C     =================
C1    No arguments required.
C
C2            Subroutine MNLPTS places the point mode
C2    options in  menu 3,at a fixed position. It also
C2    switches on the points mode mouse button.
C
      include 'include/menpop.inc'
C
      INTEGER*4 I
      EXTERNAL GTDMEN
C
C     D is the token for Reference point.
C     Y is the token for DIST LINE,ARC.
C     E is the token for END OF ENTITY.
C     C is the token for CENTRE,MIDPOINT.
C     I is the token for INTERSECTION.
C     N is the token for NEAREST ENTITY.
C     T is the token for typed input.
      DO 10 I=40,46
         CALL GTDMEN(I,3)
 10   CONTINUE
C 
C     Allow the points mode popup on the L.H mouse button (button 1).
      PTSMNU = .TRUE.
      END
C
C-----------------------------------------------------------------
C
C
      SUBROUTINE MNLTX1
C     =================
C
C2    MNLTX1 intialises the text menu with values set
C2    by the user .
C2
C2    Tokens used here are K,L,V,H,W and A.
C SPB 111194 - and now = too ...      *******************************
      include  'include/ndata.inc'
      include  'include/gtxt2.inc'
      include  'include/vntable.inc'
C
      INTEGER*4 J,JMAP(9),I,K
      INTEGER*4 OPHPNT(3),OPVPNT(3)
      EXTERNAL GTDMEN,GTDMWR   
      CHARACTER JSTNAM*16
C
      DATA (JMAP(I),I=1,9)/157,154,151,158,155,152,159,156,153/
C
C     store dictionary pointers for justifications
C     horizontals first
      OPHPNT(1)=155
      OPHPNT(2)=156
      OPHPNT(3)=157
C     now verticals
      OPVPNT(1)=158
      OPVPNT(2)=159
      OPVPNT(3)=160
C
C***************************************************************
C     S E T       MEN 3   CELL 6      TEXT JUSTIFICATION
C***************************************************************
      CALL GTDMEN(160,3)       
      J = JMAP(JUST)
      JSTNAM = VNOUN(J)
      GTMULT=.TRUE.
      CALL GTDMWT(160,3,JSTNAM)
C***************************************************************
C     S E T       MEN 3   CELL 14     TEXT SLANT ANGLE
C***************************************************************
C2    V is the token for SLANT:
C     show current settings
      CALL GTDMWR(161,3,SLANT,'(F8.2)')
C
C***************************************************************
C     S E T       MEN 3   CELL 10     TEXT  HEIGHT
C***************************************************************
C2    H is the token for HEIGHT:
C     show current settings
      CALL GTDMWR(162,3,THIGT,'(F8.2)')
C
C***************************************************************
C     S E T       MEN 3   CELL 11    TEXT TWIDTH
C***************************************************************
C2    W is the token for WIDTH:
C     show current settings
      CALL GTDMWR(163,3,TWIDTH,'(F8.2)')
C
C***************************************************************
C     S E T       MEN 3   CELL 15    TEXT  ANGLE
C***************************************************************
C2    A is the token for ANGLE:
C     show current settings
      CALL GTDMWR(164,3,TANGL,'(F8.2)')
C2
      END
C
C-----------------------------------------------------------------
C

      SUBROUTINE MNUPTS
C     =================
C1    No arguments required.
C
C2         Subroutine MNUPTS unloads the point mode options in
C2    menu 3 and disallows the points mode popup menu on mouse
C2    button 1 (L.H button)
C
      include 'include/menpop.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 I,J
      EXTERNAL GTCLRC
C
C     Clear the cells containing point modes.
C     pointe modes defined in verb-noun table
C     at positions 40-46
      DO 20 J=40,46
C        find cell position from VNPOS array
         I=VNPOS(J)
         CALL GTCLRC(3,I)
 20   CONTINUE
C
C     Disallow the popup on mouse button 1.
      PTSMNU = .FALSE.
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE PRWORD(DNUM,STRING)
C     ==============================
C1    vartype             I4   C*(*)
C1    iostatus            I      O
C2
C2    Returns the message pointed to by DNUM
C2    in STRING.
C
      include 'include/vntable.inc'
C
      INTEGER*4 DNUM
      CHARACTER*(*) STRING
C
      STRING=DICT01(DNUM)
C
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE SUTIL1()
C     ===================
C1    no arguments required.
C
C2    Takes control if the display control menu
C2    (menu no 4) is hit.Carries out ZOOM,PAN,
C2    REDRAW etc If ACCEPT or MASTER MENU commands
C2    are hit,then returns.
C2
C2    CTEXT,CCMD are passed in the common block 'MENUC'
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/viewport.inc'
      include 'include/wtov.inc'
      include 'include/macro.inc'
      include 'include/apollo.inc'
      include 'include/swind.inc'
      include 'include/journal.inc'
      include 'include/gtxt2.inc'
C
      LOGICAL CVERFY,TVPA
      INTEGER TCELL, NLEN, MNCODE
      LOGICAL OK
      LOGICAL RDONLY
      CHARACTER*84 BUFF
C
      INTRINSIC CHAR
C
      EXTERNAL SCALEW,ZOOM,MWIN,REDRAW,GTMCHI,GTMCLO,EGRID
      EXTERNAL GTSAVW,GTRSTW,ZOMEXT,ZPVIEW,GTCLRM,SLAYER,MNIDIS,
     +         GRID1,GTCLRC,GTDOMN,WRTJRN,NLEN,MENPOP
C
C     Save the cell number hit.
      TCELL=CELLN
C
C     redraw only flag
      RDONLY = .FALSE.
      TVPA = VPADD
      VPADD = .FALSE.
C     give us the view immediatly
      IF(CVERFY(CCMD,'abcdefghi')) THEN
          DRAWN = .FALSE.
          IF(MAWS) THEN
C             call a maws view definition
              CALL VPTCTL()
          ENDIF
          GOTO 100
      ENDIF
      IF(MAWS.AND.MVPACT.AND.CVPN.GT.0) THEN
          IF(CVERFY(CCMD,'EMP')) THEN
C             kill curren tmaws view and regen
C             Inheritance flag added to ensure layers will be copied as they are set
              CALL KILLVW(.FALSE.,.TRUE.)
          ELSEIF(CVERFY(CCMD,'Z')) THEN
C             kill MNAws view but copy its display file
              CALL KILLVW(.TRUE.,.TRUE.)
          ENDIF
      ENDIF
      IF ( CCMD.EQ.'S' ) THEN
C        Zoom down the current viewport.
C        save the prompt window status
C         CALL GTSAVW(1)
         CALL SCALEW()
C        recover the prompt window status
C         CALL GTRSTW(1)

      ELSE IF ( CCMD.EQ.'Z' ) THEN
C        Zoom in on current display.
C        save the prompt window status
C         CALL GTSAVW(1)
         CALL ZOOM(OK)
C        add to display file.
         IF ( OK )  CALL ATGEND()
C        recover the prompt window status
C         CALL GTRSTW(1)
      ELSE IF ( CCMD.EQ.'E' ) THEN
C        Zoom out on current display.
         CALL ZOMEXT()
      ELSE IF ( CCMD.EQ.'M' ) THEN
C        Pan  across the current viewport.
C        save the prompt window status
C         CALL GTSAVW(1)
         CALL MWIN()
C        recover the prompt window status
C         CALL GTRSTW(1)
      ELSE IF ( CCMD.EQ.'R' ) THEN
C        Redraw the display.
         CALL REDRAW()
         RDONLY = .TRUE.
      ELSE IF ( CCMD.EQ.'P' ) THEN
C        Redraw the previous viewport
         CALL ZPVIEW()
      ELSE IF ( CCMD.EQ.CHAR(150) ) THEN
C        Accept key hit (ACCEPT).
         GOTO 100
      ELSE IF ( CCMD.EQ.'V' ) THEN
C        punt out a journaling command
         IF(JOURON) THEN
           BUFF = CTEXT(1:NLEN(CTEXT))
           CALL  WRTJRN(0.0,0.0,'m',BUFF,0)
         ENDIF
C        save the prompt window status
C         CALL GTSAVW(1)
C        Layer option
C        Clear the display option menu.
         CALL GTCLRM(4)
C        Insert Layer option menu
         DISLAY=.NOT.DISLAY
         CALL MNIDIS()
C        Layer option subroutine
C==================================
CCCCC         CALL SLAYER()
C=================================
      ELSE IF (CCMD .EQ.'U') THEN
C***************************************************
C2        GRID     On/Off                          *
C***************************************************
C         set the grid type, if unset to cartesian
          IF(GRTYPE .LT. 1 .OR. GRTYPE .GT. 2) GRTYPE = 1     
C
          IF(SETGRD) THEN 
C             erase the grid
              CALL EGRID()
              SETGRD =.FALSE.
              VPGRID(CVPN) = .FALSE.
C             unhighlight the menu cell
              IF(DISLAY) CALL GTDMCL(59,4)             
          ELSE
C             punt out the grid
              SETGRD = .TRUE.
              VPGRID(CVPN) = .TRUE.
              IF(DISLAY) CALL GTDMCH(59,4)          
              CALL GRID1()
          ENDIF
C         no more to be done here
          GOTO 999
C    
      ELSE IF (CCMD.EQ.'q') THEN
C         Quit key hit (MASTER MENU).
C         punt out a journaling command
          IF(JOURON) THEN
              BUFF = CTEXT(1:NLEN(CTEXT))
              CALL  WRTJRN(0.0,0.0,'m',BUFF,0)
          ENDIF
C         no more to be done here
          GOTO 999
      ELSE
C         error in option
          CALL DEPRNT(8)
          GOTO 999
      END IF
C
 100  CONTINUE
C
C     **********************************************
C     Viewport control functions must only be called 
C     if drawing function has been used
C     **********************************************
C
C     now repaint any windows if the backcloth was used
      IF(DRAWN.AND..NOT.MAWS) THEN
C         save portion of bitmap back to backing store
          CALL WNSVBT()
          IF(CVPN.EQ.0) THEN
C             repaint all windows
              CALL WNPNTA()
          ENDIF
      ENDIF
      IF(RDONLY.AND.MAWS.AND.CVPN.GT.0) THEN
C         save the current view just redrawn back storage bitmap
          CALL SAVPIC()
      ENDIF
C     
999   CONTINUE
      END
C
C-----------------------------------------------------------------
C
      SUBROUTINE SAVPIC()
C     ===================
C1    NO ARGS
C
C2    Routine will save current MAWS view back onto its storage
C
      include 'include/viewport.inc'
      include 'include/wtov.inc'
C
      LOGICAL OK
      INTEGER*2 VPN

C
      VPN = CVPN
      IF(CVPN.GT.0) THEN
C         save the back cloth
          CALL ROPREP()
          ORIGIN(1) = VPXMIN
          ORIGIN(2) = VPYMIN
          WINBLT(1) = VPXMIN
          WINBLT(2) = VPYMIN
          WINBLT(3) = (VPXMAX-VPXMIN)
          WINBLT(4) = (VPYMAX-VPYMIN)
C         save the current screen full of data
          CALL SAVE_BITMAP(VPN,OK)
      ENDIF
C
      END


      SUBROUTINE SUTILS(HILITE)
C     ===================     
C1    vartype             L
C1    iostat              I
C
C2     this routine is used to arbitrate between
C2     SUTILS and SLAYER
C2     HILITE is TRUE if the call has been made from
C2     a menu cell hit so that cell can be highlighted
C2     and FALSE if from a graphics screen popup e.g. MAWS
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
C
      INTEGER TCELL                             
      LOGICAL HILITE
C     Save the cell number hit.
      TCELL=CELLN
C
C     Hilite the cell.
      IF(HILITE)  CALL GTMCHI(4,TCELL)
C
      CALL GTSAVW(1)
      IF(.NOT.DISLAY) THEN
         CALL SLAYER()
      ELSE
C        save the prompt window status
         CALL SUTIL1()
C        recover the prompt window status
      END IF
      CALL GTRSTW(1)
C     Remove hilite from command cell.
C
      IF(HILITE .AND. DISLAY) THEN
C       unhighlight cell but not if the man has turned the grid on
        IF(.NOT.(SETGRD .AND. VNCCMD .EQ. 59)) CALL GTMCLO(4,TCELL)
      ENDIF
C
      END
C 
C
C-----------------------------------------------------------------
C
      SUBROUTINE VNWORD(VNNUM,VWORD)
C     ==============================
C1    vartype            I4    C*16
C1    iostatus           I      O
C2
C2    Subroutine VNWORD returns the verb/noun from position
C2    VNNUM in the Verb/Noun list in the command dictionary
C2    system.
C
      include 'include/vntable.inc'
C
      INTEGER*4 VNNUM
      CHARACTER VWORD*16
C
C     read the word for the command
      VWORD=VNOUN(VNNUM)
      CALL CRUNCH(VWORD)
C
      END
C
C-----------------------------------------------------------------
C
