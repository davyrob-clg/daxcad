C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insert1.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CCOLOR()
C     SUBROUTINE INSA00()
C     SUBROUTINE INSATT(TOKEN)
C     SUBROUTINE INSCNL(FMIPOS,ENTYP,REMV,FIRST,OK)
C     SUBROUTINE INSD00()
C     SUBROUTINE INSF00()
C     SUBROUTINE INSH00()
C     SUBROUTINE INSL00()
C     SUBROUTINE MAJIN1()
C     SUBROUTINE MNICOL()
C     SUBROUTINE MNIINS
C     SUBROUTINE MNISTD()
C     SUBROUTINE MNLINS()
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CALC00()
C     ===================
C
      include 'include/calc.inc'
      include  'include/menun.inc'
C
      CHARACTER*80 STRING,STRNG1,VARNAM*20
      INTEGER NLEN,SL
      DOUBLE PRECISION ANS
      LOGICAL OK
      EXTERNAL NLEN,CPRINT,CRUNCH
C
      CALL GTSAVW(1)
C     recover the prompt window status
 10   CONTINUE
      CALL DPRMXP(298,STRING)
      SL=NLEN(STRING)
      IF ( SL.EQ.0 ) THEN
         CALL GTRSTW(1)
         RETURN
      END IF
C
C     test for assignment operator
      CALL AEXPRN(STRING,ANS,*10)
C     check character result
      CALL CEXPRN(STRING,OK)
      CALL DTPMSG(23,1)
      IF(.NOT.OK) THEN
          IF(SBUF.EQ.'INTEGER') THEN
              SL=INT(ANS)
              WRITE(STRING,'(I6)') SL
          ELSE
              WRITE(STRING,'(F20.10)') ANS
          ENDIF
          CALL CRUNCH(STRING)
      ENDIF
      CALL CPRINT(STRING)
      GOTO 10
C
      END
C
C
C
      SUBROUTINE CLNEND(XIN,YIN,XOUT,YOUT)
C     ====================================
C1    VARTYPE            R   R   R     R
C1    IOSTATUS           I   I   O     O
C
C2    This subroutine calculates the end points of the center line in
C2    'NBUFF'. It then returns the point nearest to the input point.
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
C
      REAL DUMMY,RAD,INCANG,RANG,CANG,CENX,CENY,PI, DIST,DIST2,DISTXY,
     +     XT1,YT1,XT2,YT2,XT3,YT3,XIN,YIN,XOUT,YOUT,BRDR
      DOUBLE PRECISION XC1,YC1,XC2,YC2,RAD1,RAD2,DX1,DY1,DX2,DY2
      INTEGER*2 CLTYPE
      LOGICAL FIRST,OK
C
      EXTERNAL CANG,PI,DISTXY
C
C
C     right wobbles doesn't check that DCCP19 fails
C     so some defense required
C
      DX1 = 0.0
      DX2 = 0.0
      DY1 = 0.0
      DY2 = 0.0
C
      CENX = RDBUFF(1)
      CENY = RDBUFF(2)
      RAD = RDBUFF(3)
      INCANG = 0
      RANG = RDBUFF(5)
C     Get the extension and convert it from paper to world.
      CALL PAP2SC(RDBUFF(6),DIST)
      CALL SC2WO(DIST,0.0,BRDR,DUMMY)
      CALL SC2WO(0.0,0.0,DUMMY,DIST)
      BRDR = BRDR - DUMMY
      CLTYPE = IDBUFF(4)
C
      IF (CLTYPE.EQ.3) THEN
C        PCD straight line section.
         DUMMY = RAD
C        Find the referance arc center point.
         CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,RANG,XT1,YT1)
         CENX = XT1
         CENY = YT1
         RAD = RDBUFF(4)
      ENDIF
C
      FIRST = .TRUE.
 50   CONTINUE
C        Firstly ... Find The Line.
C        Find finish point.
         RAD = RAD + BRDR
         DUMMY = RAD
         CALL NEWPNT(CENX,CENY,RAD,DUMMY,INCANG,RANG,XT2,YT2)
C        and start point is the same dist, other direction.
         XT1 = CENX - (XT2 - CENX)
         YT1 = CENY - (YT2 - CENY)
C        Which end is closest to the cursor?
         DIST=DISTXY(XT1,YT1,XIN,YIN)
         DIST2=DISTXY(XT2,YT2,XIN,YIN)
         IF ( DIST2 .LT. DIST ) THEN
            XOUT=XT2
            YOUT=YT2
         ELSE
            XOUT=XT1
            YOUT=YT1
         END IF
C     First line dealt with, how about a second?
      IF ((CLTYPE.EQ.1)) THEN
         IF (FIRST) THEN
C           Second line of a cross center line.
            XT3 = XOUT
            YT3 = YOUT
            RAD = RDBUFF(4)
            RANG = RANG + PI(0.5)
            FIRST = .FALSE.
            GOTO 50
         ELSE
C           Got two end points; but which of these is closest
C           to the cursor?
            DIST=DISTXY(XOUT,YOUT,XIN,YIN)
            DIST2=DISTXY(XT3,YT3,XIN,YIN)
            IF ( DIST2 .LT. DIST ) THEN
               XOUT = XT3
               YOUT = YT3
            END IF
         ENDIF               
      ENDIF
      IF (CLTYPE.EQ.3) THEN
C        PCD ... the curved bit.
C
C        Find start and finish point.
         XC1 = DBLE(RDBUFF(1))
         YC1 = DBLE(RDBUFF(2))
         RAD1 = DBLE(RDBUFF(3))
         XC2 = DBLE(CENX)
          YC2 = DBLE(CENY)
         RAD2 = DBLE(RAD)
         CALL DCCP19(XC1,YC1,RAD1,XC2,YC2,RAD2,DX1,DY1,
     +               DX2,DY2,OK)
         XT1 = REAL(DX1)
         YT1 = REAL(DY1)
         XT2 = REAL(DX2)
         YT2 = REAL(DY2)
C      
C        Which point is closer to the cursor?
         DIST=DISTXY(XT1,YT1,XIN,YIN)
         DIST2=DISTXY(XT2,YT2,XIN,YIN)
         IF ( DIST2 .LT. DIST ) THEN
            XT3=XT2
            YT3=YT2
         ELSE
            XT3=XT1
            YT3=YT1
         END IF                              
C 
C        Is the new point closer to the cursor than the one
C        calculated defore ?
         DIST=DISTXY(XOUT,YOUT,XIN,YIN)
         DIST2=DISTXY(XT3,YT3,XIN,YIN)
         IF ( DIST2 .LT. DIST ) THEN
            XOUT = XT3
            YOUT = YT3
         END IF
      ENDIF
C
      END
C
      SUBROUTINE RESET()
C     ==================
C1    NO ARGUMENTS
C
C2    Resets the marked state of an the current entity in the buffers
C2    Be careful about this one
C 
      include 'include/nbuff.inc'
      include 'include/viewport.inc'
C
      INTEGER*2 TMIP
      REAL M(3,3)
C
      IF(MVPACT.AND..NOT.MAWS) THEN
C         set drawing code for this one please
          DDCODE = 2
      ENDIF
      CALL SPCDRW(IMBUFF(2),TMIP,.FALSE.,M,.FALSE.)
C     reset drawing code
      DDCODE  = 0
      END
 
      SUBROUTINE XYCOOR(STRING,X,Y,Z,OK)
C     ==================================
C1                       *(*)  R,R,R, L
C1                         I   O,O,O  O
C
C2        Subroutine XYCOOR, decodes the input STRING into a series of
C2  arguments. An argument consists of one of the strings listed below
C2  which  MAY  be followed by a number.  Arguments are separated by a
C2  comma.
C2       If an argument does not have a number, a default is used.
C2       The order of the agrguments is not important.
C2  NOTE: There is one exception to this format. If the string begins
C2  with a number, the format absolute_x_coord, absolute_y_coord
C2  (e.g.  10,12  ) is assumed.
C2
C2  NOTE: The Z argument is not fully suported !
C2
C2    Argument    Meaning                        Default
C2    --------    -------                        -------
C2  1.   X        Absolute X coordinate.         0
C2  2.   Y        Absolute Y coordinate.         0
C2  3.   Z        Absolute Z coordinate.         0
C2  4.   R        Absolute radius.
C2                       (Rotate about origin.)  0
C2  5.   A        Absolute angle.                0
C2  6.   IX       Incremental X coordinate.      X coord of last point.
C2  7.   IY       Incremental Y coordinate.      Y coord of last point.
C2  8.   IZ       Incremental Z coordinate.      Z coord of last point.
C2  9.   IR       Relative radius.
C2                   (Rotate about last point.)  0
C2 10.   IA       Relative angle.                0
C2 11.   RX           These all                  X coord of last point.
C2 12.   RY         do the same                  Y coord of last point.
C2 13.   RZ         as  the IA,                  Z coord of last point.
C2 14.   RR         IZ, IR  etc                  0
C2 15.   RA         do.                          0
C
      include 'include/abs.inc'
C
      CHARACTER*(*) STRING
      CHARACTER*80 TMPSTR
      REAL X,Y,Z
      LOGICAL OK
C
      INTEGER*4 OPS
      PARAMETER( OPS = 15 )
      CHARACTER*2 CHK(OPS)
      INTEGER*4 CHKLEN(OPS)
      LOGICAL ARG(OPS)
      REAL VAL(OPS)
C             OPS = Number of argument types.
C             CHK = Names of agrguments.
C          CHKLEN = Lengths of argument names.
C             ARG = Flag to say if this argument has been chosen.
C             VAL = The values corresponding to those arguments.
C
      DOUBLE PRECISION DVAL
      DOUBLE PRECISION  XC1,YC1,RAD1,X1,Y1,X2,Y2,
     +                  XP1,YP1,XP2,YP2
      INTEGER*4 I,POS(2),STRLEN,ARGTOT,FIELD(30),FN
      REAL RELDST, ANGL
      CHARACTER*(*) DSTR*20
      PARAMETER (DSTR='0123456789.-')
C             POS = Pointer to substring containing current
C                   argument.
C          STRLEN = Total Length of input string.
C          ARGTOT = Count of the number of arguments.
C           FIELD = A list of fields.
C              FN = Number of fields.
C          RELDST = Temporary store for relative distances.
C            ANGL = Temporary store for angles.
C            DSTR = A list of valid numerical characters.
C
      REAL RAD
      INTEGER*4 NLEN
      EXTERNAL NLEN,RAD,AEXPRN,FOLDUP,DEPRNT,DCC0P9
      INTRINSIC INDEX,REAL,DBLE
C
      DATA (CHK(I),I=1,OPS)/
     +                      'RX','RY','RZ','RR','RA',
     +                      'IX','IY','IZ','IR','IA',
     +                      'X ','Y ','Z ','R ','A '/
      DATA (CHKLEN(I),I=1,OPS)/2,2,2,2,2,2,2,2,2,2,1,1,1,1,1/
C
C     Set all argument flags to .false. and values to 0.
      DO 10 I=1,OPS
         VAL(I)=0.0
         ARG(I)=.FALSE.
 10   CONTINUE
C
      OK=.FALSE.
 15   CONTINUE
C     fold to upper case.
      CALL FOLDUP(STRING)
C     remove the blanks.
      CALL CRUNCH(STRING)
C     find out the length.
      STRLEN=NLEN(STRING)
C
      IF ( STRLEN.LT.1 ) THEN
C        one letter can't do anything with it.
         CALL DEPRNT(415)
         RETURN
      END  IF
      TMPSTR = STRING
      CALL PARSES(TMPSTR,FIELD,FN) 
C
C*********************************************************************
C     If we have one field, and it is a string variable, expand it.
C     NOTE: This is especially useful for macros.
C*********************************************************************
      IF (FN.EQ.1 .AND. STRING(STRLEN:STRLEN).EQ.'$') THEN
C        Expand it.
         CALL AEXPRN(STRING(1:STRLEN),DVAL,*999)
         CALL CEXPRN(TMPSTR,OK)
         STRING = TMPSTR
C        Go and start again.
         IF (OK) GOTO 15
      ENDIF
C
C*********************************************************************
C     Now work through the input STRING for between 1 and 3 arguments.
C     We decipher them and any corresponding values.
C*********************************************************************
      ARGTOT = 0
      POS(1) = 1
 30   CONTINUE
         I = 0
C
C        Search through CHK to find a matching argument string.
 20      CONTINUE
            I = I + 1
            IF(STRING( POS(1):(POS(1)+CHKLEN(I)-1) ) .NE.
     +                               CHK(I)(1:CHKLEN(I)) )THEN
C              Did we have a bad argument ?
               IF ( I.GE.OPS ) THEN
C                 The first field onlycontains a number or calculation,
C                 it must be the special format
C
                  IF (FN.EQ.2) THEN
C                    Find the next comma. (If any).
                     POS(2) = FIELD(1) - 1
                     IF (POS(2).LT.POS(1)) GOTO 999
                     CALL AEXPRN(STRING(POS(1):POS(2)),DVAL,*999)
C                    Store the X value.
                     VAL(11) = REAL(DVAL)
                     ARG(11) = .TRUE.
                     POS(1) = POS(2) + 2
                     POS(2) = STRLEN
                     IF (POS(2).LT.POS(1)) GOTO 999
                     CALL AEXPRN(STRING(POS(1):POS(2)),DVAL,*999)
C                    Store the Y value.
                     VAL(12) = REAL(DVAL)
                     ARG(12) = .TRUE.
                     ARGTOT = 2
                     GOTO 900
                  ENDIF
C
                  CALL DEPRNT(416)
                  RETURN
               ELSE
                  GOTO 20
               END IF
            END IF
C
C        Got an argument. If it is an R argument, convert it to an I.
         IF (I.LE.5) I=I+5
C
         IF ( ARG(I) ) THEN
C           He's just used the same argument twice.
            CALL DEPRNT(416)
            RETURN
         ENDIF
         ARG(I) = .TRUE.
         ARGTOT = ARGTOT + 1
C        Move past the argument name.
         POS(1) = POS(1) + CHKLEN(I)
C        Find the next comma. (If any).
         POS(2) = FIELD(ARGTOT) - 1
         IF( ARGTOT.GE.FN ) THEN
            POS(2) = STRLEN
         ENDIF
C
C        Anything left between POS(1) and POS(2) must be a number.
C        Get it ...        (Otherwise, we already have the default.)
         IF( POS(1) .LE. POS(2) ) THEN
            CALL AEXPRN(STRING(POS(1):POS(2)),DVAL,*999)
            VAL(I) = REAL(DVAL)
         ENDIF
C
C        Any more arguments ?
         IF( (POS(2) .LT. STRLEN) .AND. (ARGTOT.LT.3) ) THEN
C           Step over the comma.
            POS(1) = POS(2) +2
            GOTO 30
         ENDIF
C
C*******************************************************************
C     Okay. Now we've got all the parameters, let's try and put them
C     together to get a resultant point.
C*******************************************************************
C     First lets get rid of Z.
      IF ( ARG(13) ) THEN
C        Absolute Z. This is OK.
         ARGTOT = ARGTOT - 1
      ELSE IF ( ARG(8) ) THEN
C        Relative Z. This is OK.
         ARGTOT = ARGTOT - 1
         VAL(13) = ABSZ + VAL(8)
      ENDIF
C
C     Now let's calculate x and y.
      IF (ARGTOT .GT. 0) THEN
C        Convert any incremental values into absolute ones.
         IF (ARG(6)) THEN
C           Argument IX.
            IF (ARG(11)) THEN
C              Can't have X and IX !.
               CALL DEPRNT(417)
               RETURN
            ELSE
               VAL(11) = ABSX + VAL(6)
               ARG(11) = .TRUE.
            ENDIF
         ENDIF
C
         IF (ARG(7)) THEN
C           Argument IY.
            IF (ARG(12)) THEN
C              Can't have Y and IY !.
               CALL DEPRNT(417)
               RETURN
            ELSE
               VAL(12) = ABSY + VAL(7)
               ARG(12) = .TRUE.
            ENDIF
         ENDIF
C
C        Now we have to deal with radii and angles.
         IF ( (ARG(11).OR.ARG(12)) .AND. ARGTOT.EQ.1 ) THEN
C
C           X or Y only. This is OK.
C           Other value remains as before.
            IF ( ARG(11) ) THEN
               VAL(12) = ABSY
            ELSE
               VAL(11) = ABSX
            ENDIF
C
         ELSE IF ( ARG(11) .AND. ARG(12) ) THEN
C
C           X and Y. This is also OK.
C           No need to do anything more.
C
         ELSE IF ( ARG(11) .AND. ARG(15) ) THEN
C
C           X, Absolute angle.
            VAL(12) = VAL(11) * TAN(RAD(VAL(15)))
C
         ELSE IF ( ARG(11) .AND. ARG(10)) THEN
C
C           X, Relative angle.
            RELDST = VAL(11) - ABSX
            VAL(12) = ABSY + ( RELDST * TAN(RAD(VAL(10))) )
C
         ELSE IF ( ARG(12) .AND. ARG(15) ) THEN
C
C           Y, Absolute angle.
            VAL(11) = VAL(12) / TAN(RAD(VAL(15)))
C
         ELSE IF ( ARG(12) .AND. ARG(10)) THEN
C
C           Y, Relative angle.
            RELDST = VAL(12) - ABSY
            VAL(11) = ABSX + ( RELDST / TAN(RAD(VAL(10))) )
C
         ELSE IF ( ARG(11) .AND. ARG(14) ) THEN
C
C           X, Absolute radius.
            VAL(12) = SQRT( VAL(14)**2 - VAL(11)**2 )
C
         ELSE IF ( ARG(11) .AND. ARG(9) ) THEN
C
C           X, Relative radius.
            RELDST = VAL(11) - ABSX
            VAL(12) = ABSY + ( SQRT( VAL(9)**2 - RELDST**2 ) )
C
         ELSE IF ( ARG(12) .AND. ARG(14) ) THEN
C
C           Y, Absolute radius.
            VAL(11) = SQRT( VAL(14)**2 - VAL(12)**2 )
C
         ELSE IF ( ARG(12) .AND. ARG(9) ) THEN
C
C           Y, Relative radius.
            RELDST = VAL(12) - ABSY
            VAL(11) = ABSX + ( SQRT( VAL(9)**2 - RELDST**2 ) )
C
         ELSE IF ( ARG(14) .AND. ARG(15) ) THEN
C
C           Absolute radius, Absolute angle.
            VAL(11) = VAL(14) * COS(RAD(VAL(15)))
            VAL(12) = VAL(14) * SIN(RAD(VAL(15)))
C
         ELSE IF ((ARG(14).AND.ARG(10)).OR.(ARG(9).AND.ARG(15))) THEN
C
            IF ( ARG(14) .AND. ARG(10) ) THEN
C              Absolute radius, Relative angle.
               XC1 = 0.0
               YC1 = 0.0
               RAD1 = DBLE(VAL(14))
               ANGL = VAL(10)
               X1 = DBLE(ABSX)
               Y1 = DBLE(ABSY)
               X2 = DBLE(ABSX) + 10.0
               Y2 = DBLE(ABSY) + DBLE( 10.0 * TAN(RAD(ANGL)) )
            ELSE
C              Relative radius, Absolute angle.
               XC1 = DBLE(ABSX)
               YC1 = DBLE(ABSY)
               RAD1 = DBLE(VAL(9))
               ANGL = VAL(15)
               X1 = 0.0
               Y1 = 0.0
               X2 = 10.0
               Y2 = DBLE( 10.0 * TAN(RAD(ANGL)) )
            ENDIF
C
C           Now calculate the intersection points.
            CALL DCC0P9(XC1,YC1,RAD1,X1,Y1,X2,Y2,XP1,YP1,XP2,YP2,OK)
C
C           Which intersection point do we want.
            IF ((ANGL .GT.   0.0 .AND. ANGL .LT. 180.0) .OR.
     +                                (ANGL .LT.-180.0)) THEN
               IF(XP1.GT.XP2) THEN
                  VAL(11) = XP1
                  VAL(12) = YP1
               ELSE
                  VAL(11) = XP2
                  VAL(12) = YP2
               ENDIF
            ELSE
              IF(XP1.GT.XP2) THEN
                  VAL(11) = XP2
                  VAL(12) = YP2
               ELSE
                  VAL(11) = XP1
                  VAL(12) = YP1
               ENDIF
            ENDIF
C
         ELSE IF ( ARG(9) .AND. ARG(10)) THEN
C
C           Relative radius, Relative angle.
            VAL(11) = ABSX + ( VAL(9) * COS(RAD(VAL(10))) )
            VAL(12) = ABSY + ( VAL(9) * SIN(RAD(VAL(10))) )
C
         ELSE
C
C           Any other combination is illegal.
            CALL DEPRNT(417)
            RETURN
C
         ENDIF
      ENDIF
C     
 900  CONTINUE
      X = VAL(11)
      Y = VAL(12)
      Z = VAL(13)
      OK = .TRUE.
      RETURN
C
 999  CONTINUE
      CALL DEPRNT(417)
C
      END
