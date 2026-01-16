C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writegi.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE WRTGI1()
C     SUBROUTINE WRTGI2(FILUNT,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE WRTGI1()
C     ===================
C1    no arguments required
C
C2    Saves the current contents of the data base in a disc file
C2    of the name given by the user in response to a request from
C2    this subroutine. In the style of a GENIO 'MOSS' file.
C2    Only SPLINE and TEXT entities are exported by this method.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/filunit.inc'
      include   'include/params.inc'
      include 'include/lfu.inc'
      include 'include/menun.inc'
C
      INTEGER*4 NLEN,DNUM
      CHARACTER*40 FILNM,ANS*1,TDRWNM
      LOGICAL OK,WORKF,YESOK
      EXTERNAL EPRINT,FINDU2,NLEN,SAVALL,YESOK
C
      IF (NMIPOS .EQ. 1) THEN
          DNUM = 758
          CALL DEPRNT(DNUM)
          CCMD = 'q'
          RETURN
      ENDIF
C
C     Save temporaryly the correct drawing file name
      TDRWNM=DRGNAM
C
 5    CONTINUE
C     ask for output file name
      CALL DPRMXP(81,FILNM)
C     if null file name assume abort required
      IF ( NLEN(FILNM).EQ.0 ) THEN
         MEN=0
         PARFUN=0
         RETURN
      END IF
C
  7   CONTINUE
C     go find a unit for the file
      CALL FINDU2(PARFUN,FILNM,OK)
      IF ( .NOT. OK ) THEN
         CALL DEPRNT(275)
         RETURN
      END IF
C
  6   CONTINUE
C     ensure correct drawing name is used
      OPEN(UNIT=PARFUN,FILE=FILNM,ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED',STATUS='NEW',ERR=101)
CIBM
C      LFU(PARFUN)=.TRUE.
CIBM
C
C     *******************************************
C     ***       WRITE GENIO DATA FILE         ***
C     *******************************************
C     go write the data to file
      CALL WRTGI2(PARFUN,OK)
C
      CLOSE(UNIT=PARFUN,STATUS='KEEP',ERR=201)
CIBM
C      LFU(PARFUN)=.FALSE.
CIBM
      RETURN
C
 101  CONTINUE
C     file already exists,ask for help
      CALL DPRMXP(282,ANS)
      IF ( YESOK(ANS) ) THEN
C        he wants to overwrite the file
         OPEN(UNIT=PARFUN,FILE=FILNM,ACCESS='SEQUENTIAL',
     +   FORM='FORMATTED',STATUS='OLD',ERR=101)
         CLOSE(UNIT=PARFUN,STATUS='DELETE',ERR=201)
         GOTO 6
      ELSE
C        no overwrite,get a new file name
         GOTO 5
      END IF
C
C     tell him it's all over
 201  CALL DCPRNT(82)
C
C     Restore correct drawing file name
      DRGNAM=TDRWNM
C
      END
C
C     ----------------------------------------------
C
      SUBROUTINE WRTGI2(FILUNT,OK)
C     ============================
C
C1    vartype           L
C1    iostatus          O
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*2 TMIP,POINT,NF,J,TLAY
      INTEGER*4 FILUNT,NLEN1
      REAL XP,YP,ZP,R(6),TSCAL,AZIMUT
      LOGICAL OK
      CHARACTER*40 FORM1,FORM2,FORM3,FORMT,TLNAM*4
      EXTERNAL NLEN1,AZIMUT
C
      FORM1='(6F12.3)'
      FORM2='(I3.3,A4,4X,4X,8X,2F10.3,F8.3,2F10.3)'
      FORM3='(2F15.3,2F9.3)'
      FORMT='(2F15.3,2F9.3,/,11A4)'
      TLNAM='    '
      TLAY=999
C     set text scale to output text height
c     in cm on paper.
      TSCAL=0.1
C     write job header to file
      WRITE(FILUNT,FMT='(A)')'   DAXCAD-GENIO DATA TRANSFER'
C     write source header to file
      WRITE(FILUNT,FMT='(A)')'MOSS,GENIO'
C     write out strings first
C     give format for strings
      WRITE(FILUNT,FMT='(2A)')'001FORMAT',FORM1(1:NLEN1(FORM1))
      DO 50 TMIP=1,NMIPOS-1
C        read next entity
         CALL DER500(TMIP,OK)
         IF (IMBUFF(2).EQ.SPLINE) THEN
C           ****************************************
C           ***      WRITE STRING                ***
C           ****************************************
C           find layer of entity
            IF (TLAY.NE.IMBUFF(4)) THEN
C              set new label for this layer
               TLAY=IMBUFF(4)
               TLNAM=LNAME(TLAY)
            END IF
C           write string header to outpt file
            WRITE(FILUNT,FMT=FORM2)080,TLNAM,0.0,0.0,3.0,-1.0,-1.0
            POINT=IDBUFF(3)
C           Read the first two records cos they contain
C           the end constraints.
            CALL DBR500(POINT,OK)
            POINT=IDBUFF(3)
            CALL DBR500(POINT,OK)
            POINT=IDBUFF(3)
            NF=1
 10         CONTINUE
            R(NF)=RDBUFF(1)
            R(NF+1)=RDBUFF(2)
            R(NF+2)=RDBUFF(3)
            NF=NF+3
            IF (NF.GT.6) THEN
C              write record out
               WRITE(FILUNT,FMT=FORM1)(R(J),J=1,6)
               NF=1
            END IF
C
            IF ( POINT .NE. 0 ) THEN
               CALL DBR500(POINT,OK)
               IF (OK) THEN
                  POINT=IDBUFF(3)
                  GOTO 10
               END IF
            END IF
C           terminate the spline data
            R(NF)=-1.0
            R(NF+1)=-1.0
            WRITE(FILUNT,FMT=FORM1)(R(J),J=1,NF+1)
         END IF
 50   CONTINUE
C
C     write format for text records
      WRITE(FILUNT,FMT='(2A)')'001FORMAT',FORMT(1:NLEN1(FORMT))
C
      DO 60 TMIP=1,NMIPOS-1
C        read next entity
         CALL DER500(TMIP,OK)
         IF (IMBUFF(2).EQ.TEXT) THEN
C           find layer of entity
C            IF (TLAY.NE.IMBUFF(4)) THEN
C              set new label for this layer
               TLAY=IMBUFF(4)
               TLNAM=LNAME(TLAY)
C              write text header to output file
               WRITE(FILUNT,FMT=FORM2)080,TLNAM,0.0,0.0,15.0,-1.0,-1.0
C            END IF
C           ****************************************
C           ***      WRITE TEXT                  ***
C           ****************************************
C           write TEXT data to output file
            R(1)=RDBUFF(1)
            R(2)=RDBUFF(2)
C           correct text height to cm on paper
            R(3)=RDBUFF(3)*TSCAL
C           correct angle to azimuth
            R(4)=AZIMUT(RDBUFF(5))
            WRITE(FILUNT,FMT=FORM3)(R(J),J=1,4)
            WRITE(FILUNT,FMT='(A)')CBUFF(1:NLEN1(CBUFF))
C           ****************************************
C           ***      WRITE DUMMY RECORD          ***
C           ****************************************
            R(1)=-1.0
            R(2)=-1.0
            R(3)=0.0
            R(4)=0.0
            WRITE(FILUNT,FMT=FORM1)(R(J),J=1,2)
            CBUFF='THIS IS A DUMMY RECORD'
            WRITE(FILUNT,FMT='(A)')CBUFF(1:NLEN1(CBUFF))
         END IF
 60   CONTINUE
      R(1)=-1.0
      R(2)=-1.0
      R(3)=0.0
      R(4)=0.0
      WRITE(FILUNT,FMT=FORM1)(R(J),J=1,2)
      CBUFF='THIS IS A DUMMY RECORD'
      WRITE(FILUNT,FMT='(A)')CBUFF(1:NLEN1(CBUFF))
C
      WRITE(FILUNT,FMT='(I3)') 999
      OK=.TRUE.
C
      END
C
C     ----------------------------------------------
C
