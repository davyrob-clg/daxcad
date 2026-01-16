C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 trim.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE MAJTRM()
C     SUBROUTINE MNITRM()
C     SUBROUTINE MNLTM1()
C     SUBROUTINE TRMA00()
C     SUBROUTINE TRMA01()
C     SUBROUTINE TRMA02()
C     SUBROUTINE TRML00()
C     SUBROUTINE TRML01()
C     SUBROUTINE TRML02()
C     SUBROUTINE TRML10(OK)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE MAJTRM()
C     ===================
C
C1    no arguments required
C
C2    Subroutine MAJTRM is the entry point
C2    for the TRIM option from the master menu
C
      include 'include/menun.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL
C
      REAL X,Y
C
      EXTERNAL MNITRM,TRML00,TCURS,GTCLRM,GTMCLO,CLRPEW,
     +         GTHFMC,GTMCHI,TRMA00
C
C     Now activate the TRIM major option menu
      CALL MNITRM()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     trim. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C     Making single line the default insert text
      MEN=2
C     'L' is the token used by insert line
      CCMD='L'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select TRIM type
      CALL TCURS(C,X,Y)
C
 20   CONTINUE
C     save pointers to menu and cell which was hit
      TMEN=MEN
      TCELL=CELLN
C     test for quit character
      IF (CCMD.EQ.'Q'.OR.CCMD.EQ.'q') GOTO 99
C     ***************************************************************
C     **************************************MAJOR OPTIONS START******
C     ***************************************************************
      IF (MEN.EQ.2) THEN
C        ensure menu cell is hilited
         CALL GTMCHI(TMEN,TCELL)
C        ensure group search status allows
C        return of grouped entities as individual entities
         GSSTAT=1
         IF (CCMD.EQ.'L') THEN
C           TRIM LINE option
            CALL TRML00()
         ELSE IF (CCMD.EQ.'A') THEN
C           TRIM ARC option
            CALL TRMA00()
         ELSE
C           unrecognized dimension option
            CALL DEPRNT(8)
C           set MEN to 0 to avoid infinite loop on unassigned cell
            MEN=0
         END IF
         IF (CCMD.EQ.'q') GOTO 99
C        ensure the entry cell is not hilited any more
         CALL GTMCLO(TMEN,TCELL)
C        clear the minor option menu
         CALL GTCLRM(3)
C        if another major option,go check it out
         IF (MEN.EQ.2) GOTO 20
         GOTO 10
      ELSE
         CALL DEPRNT(314)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
      END
C
C
      SUBROUTINE MNITRM()
C     ===================
C1    No arguments required.
C
C2    Clears the minor option menu and loads
C2    the TRIM  option list.
C2
      EXTERNAL GTPMEN,GTMCHI,GTCLRM
C
      CALL GTCLRM(3)
C
      CALL GTCLRM(2)
C
C     place menu header and hilite it
C      CALL GTPMEN('TRIM',' ',2,1)
      CALL GTDMHD(17,2)
C2    L is the token for LINE
C      CALL GTPMEN('Line','L',2,7)
      CALL GTDMEN(291,2)
C2    A is the token for ARC
C      CALL GTPMEN('Arc','A',2,4)
      CALL GTDMEN(290,2)
C
      END
C
C
      SUBROUTINE MNLTM1()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLDM1 loads the options
C2    for the trim line function into
C2    menu no3.
C
      include 'include/movdat.inc'
C
      INTEGER*4 I
C
      CHARACTER CHAR
      INTRINSIC CHAR
      EXTERNAL GTPMEN
C
      I=2
C
C2    i is the token for INTOF
C      CALL GTPMEN('Intof','i',3,(I+7))
      CALL GTDMEN(295,3)
C
C2    CHAR(149) is the token for CANCEL
C      CALL GTPMEN('Cancel',CHAR(149),3,(I+14))
      CALL GTDMEN(146,3)
C
      END
C
C
C
      SUBROUTINE TRMA00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    TRIM ARC function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,TRMA01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the TRIM ARC routine
      CALL TRMA01()
C     ensure option for trim arc is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C
      SUBROUTINE TRMA01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
C
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,MNLPTS,MNLTM1,GTCLRM,TRMA02,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C
C     clear mode flags
      CALL CLROPF()
C     initialize TRIM ARC option menu
      CALL MNLTM1()
C     need the point modes as well
      CALL MNLPTS()
C     enter the TRIM ARC routine
      CALL TRMA02()
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE TRMA02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine TRMA02 is the main working
C2    routine for TRIM ARC function.Called by
C2    TRMA01 in normal circumstances.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL HX1,HY1,HX2,HY2,DISTXY,D1,D2
      REAL RDAT1(6),RDAT2(6),X,Y,ABS,PI
      DOUBLE PRECISION DHTANG,DTRANG,DCANG,DBLE
      INTEGER*4 I,C
      INTEGER*2 MIP1,MIP2,ENT1,ENT2,ENTC
      LOGICAL OPTION,QUIT,OK,SAME
      REAL VN00D6,T1,T2,T3,S1,S2,S3,F1,F2,F3,XS,YS
      INTRINSIC DBLE,ABS,REAL
C
      EXTERNAL FINDA0,TRML10,FINDLA,ZSFLAG,DEPRNT,INTLAH,DCPRNT,
     +         GETANS,DCANG,UNFLAG,DER500,DEM500,
     1         ALLDRW,SAME,PENERS,PENDRW,PI
C
C
C     ensure intof flag cleared
      OPFLAG(1)=.FALSE.
 10   CONTINUE
C     go get arc to be trimmed
      CALL FINDA0(315,HX1,HY1,MIP1,OPTION,QUIT)
      IF (QUIT) THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (OPTION) THEN
         CALL TRML10(OK)
         GOTO 10
      END IF
C     save the arc data for later use
      ENT1=IMBUFF(2)
      DO 50 I=1,6
         RDAT1(I)=RDBUFF(I)
 50   CONTINUE
C
20    CONTINUE
C     find trim point,or entity
      IF (OPFLAG(1)) THEN
C       intof lock is set,find entity
         CALL FINDLA(199,HX2,HY2,MIP2,OPTION,QUIT)
         IF (QUIT) THEN
            CALL UNFLAG(.TRUE.)
            RETURN
         END IF
         IF (OPTION) THEN
            CALL TRML10(OK)
            IF ( .NOT. OK ) THEN
               CALL ZSFLAG(.TRUE.,OK)
               GOTO 10
            END IF
            GOTO 20
         END IF
C        test for same entity twice
         IF (MIP1.EQ.MIP2) THEN
C           hit same entity twice reject it
            CALL ZSFLAG(.TRUE.,OK)
            CALL DEPRNT(317)
            GOTO 20
         END IF
C
C        save the entity data
         ENT2=IMBUFF(2)
         DO 60 I=1,6
            RDAT2(I)=RDBUFF(I)
 60      CONTINUE
C
C        got two entities,find trim point
         CALL INTLAH(ENT1,RDAT1,ENT2,RDAT2,HX2,HY2,X,Y,OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(317)
            GOTO 20
         END IF
C
      ELSE
C        trim to point only
         CALL FINDP0(319,HX2,HY2,OPTION,QUIT)
         IF (QUIT) THEN
            CALL UNFLAG(.TRUE.)
            RETURN
         END IF
         IF (OPTION) THEN
            CALL TRML10(OK)
            IF ( .NOT. OK ) THEN
               CALL UNFLAG(.TRUE.)
               CALL ZSFLAG(.TRUE.,OK)
               GOTO 10
            END IF
            GOTO 20
         END IF
C
C        now got point to trim to
         X=HX2
         Y=HY2
C        end of trim point definition
      END IF
C
C     find angle from centre to first hit
C     modify angles
      RDAT1(5) = MOD( RDAT1(5) + PI(2.0) , PI(2.0) )
      RDAT1(6) = MOD( RDAT1(6) + PI(2.0) , PI(2.0) )
      IF ( SAME(RDAT1(6),0.0) ) THEN
           RDAT1(6)=PI(2.0)
      END IF
C      DHTANG=DCANG(DBLE(RDAT1(1)),DBLE(RDAT1(2)),DBLE(HX1),DBLE(HY1))
C      write(10,*) '[trim] dhtang= ',dhtang
C     find angle from centre to trim point
      DTRANG=DCANG(DBLE(RDAT1(1)),DBLE(RDAT1(2)),DBLE(X),DBLE(Y))
C     find the end of the arc to trim
C     genetate trim vector
      CALL CV0L14(RDAT1(1),RDAT1(2),HX1,HY1,T1,T2,T3)
      XS = RDAT1(1) + RDAT1(4) * COS(RDAT1(5))
      YS = RDAT1(2) + RDAT1(4) * SIN(RDAT1(5))
      CALL CV0L14(RDAT1(1),RDAT1(2),XS,YS,S1,S2,S3)
      XS = RDAT1(1) + RDAT1(4) * COS(RDAT1(6))
      YS = RDAT1(2) + RDAT1(4) * SIN(RDAT1(6))
      CALL CV0L14(RDAT1(1),RDAT1(2),XS,YS,F1,F2,F3)
 
      D1 = VN00D6(S1,S2,S3,T1,T2,T3)
      D2 = VN00D6(T1,T2,T3,F1,F2,F3)
C      D1=ABS(REAL(DHTANG)-RDAT1(5))
C      D2=ABS(REAL(DHTANG)-RDAT1(6))
C      write(10,*) '[trim] d1= ',d1
C      write(10,*) '[trim] d2= ',d2
C
      IF (D1.LE.D2) THEN
C        trim first end point
         RDAT1(5)=REAL(DTRANG)
      ELSE
C        trim second end point
         RDAT1(6)=REAL(DTRANG)
      END IF
C     now have new arc angles
C     remove flags from screen
      CALL UNFLAG(.TRUE.)
      IF ( SAME(RDAT1(5)-RDAT1(6),0.0) ) THEN
         RDAT1(5)=0.0
         RDAT1(6)=PI(2.0)
      END IF
CAPOLLO|SUN
      VPMOV = .TRUE.
CAPOLLO|SUN
C     erase the old arc
      CALL PENERS()
      CALL ALLDRW(ENT1,MIP1)
      CALL PENDRW()
C     copy new data into place
      DO 70 I=1,6
         RDBUFF(I)=RDAT1(I)
 70   CONTINUE
C     write the modified data back
      CALL DEM500(MIP1,OK)
C     now draw the new arc with correct colour
      CALL ALLDRW(ENT1,MIP1)
C     go back try for another
CAPOLLO|SUN
      VPMOV = .FALSE.
CAPOLLO|SUN
      GOTO 10
C
      END
C
C
      SUBROUTINE TRML00()
C     ===================
C
C1    no arguments required
C
C2    Subroutine is the mediator for the
C2    TRIM LINE function.Stores the
C2    caller source,and passes control to
C2    the next level in the process.
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CLRPEW,TRML01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the TRIM LINE routine
      CALL TRML01()
C     ensure option for trim line is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C     clear the error and prompt windows
      CALL CLRPEW
C
      END
C
C
      SUBROUTINE TRML01()
C     ===================
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/ndata.inc'
C
C
      INTEGER TMEN,TCELL
      EXTERNAL GTMCLO,MNLPTS,MNLTM1,GTCLRM,TRML02,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C
C     clear mode flags
      CALL CLROPF()
C     initialize TRIM LINE option menu
      CALL MNLTM1()
C     need the point modes as well
      CALL MNLPTS()
C     enter the TRIM LINE routine
      CALL TRML02()
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
      SUBROUTINE TRML02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine TRML02 is the main working
C2    routine for TRIM LINE function.Called by
C2    TRML01 in normal circumstances.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
CAPOLLO|SUN
      include 'include/viewport.inc'
CAPOLLO|SUN
C
      REAL HX1,HY1,HX2,HY2,DISTXY,D1,D2
      REAL RDAT1(6),RDAT2(6),X,Y,ZERO,REAL
      DOUBLE PRECISION DX,DY,DBLE
      INTEGER*4 I,C
      INTEGER*2 MIP1,MIP2,ENT1,ENT2
      LOGICAL OPTION,QUIT,OK,SAME,LNZERO
      INTRINSIC DBLE,REAL
C
      EXTERNAL DISTXY,FINDL0,FINDLA,TRML10,INTLAH,DCCPLP,ZERO,
     +         DER500,DEM500,UNFLAG,ZSFLAG,
     1         GETANS,PENERS,PENDRW,SAME,LNZERO
C
C     ensure intof flag cleared
      OPFLAG(1)=.FALSE.
 10   CONTINUE
C     go get line to be trimmed
      CALL FINDL0(321,HX1,HY1,MIP1,OPTION,QUIT)
      IF (QUIT) THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (OPTION) THEN
         CALL TRML10(OK)
         GOTO 10
      END IF
C     Get the entity type.
      ENT1=IMBUFF(2)
C     FINDL0 picks up centre lines ... we don't want them though.
      IF (ENT1.EQ.CENLIN) THEN
C        OOPS, We got a centre line ... Reject it.
         CALL DEPRNT(106)
         CALL UNFLAG(.TRUE.)
         GOTO 10
      ENDIF
C     save the line data for later use
      DO 50 I=1,6
         RDAT1(I)=RDBUFF(I)
 50   CONTINUE
C
20    CONTINUE
C     find trim point,or entity
      IF (OPFLAG(1)) THEN
C        intof lock is set,find entity
         CALL FINDLA(199,HX2,HY2,MIP2,OPTION,QUIT)
         IF (QUIT) THEN
            CALL UNFLAG(.TRUE.)
            RETURN
         END IF
         IF (OPTION) THEN
            CALL TRML10(OK)
            IF ( .NOT. OK ) THEN
               CALL ZSFLAG(.TRUE.,OK)
               GOTO 10
            END IF
            GOTO 20
         END IF
C        test for same entity twice
         IF (MIP1.EQ.MIP2) THEN
C           hit same entity twice reject it
            CALL ZSFLAG(.TRUE.,OK)
            CALL DEPRNT(317)
            GOTO 20
         END IF
CC        save the entity data
         ENT2=IMBUFF(2)
         DO 60 I=1,6
            RDAT2(I)=RDBUFF(I)
 60      CONTINUE
C
C        got two entities,find trim point
         CALL INTLAH(ENT1,RDAT1,ENT2,RDAT2,HX2,HY2,X,Y,OK)
         IF (.NOT.OK) THEN
            CALL DEPRNT(317)
            GOTO 20
         END IF
         CALL ZSFLAG(.TRUE.,OK)
C
      ELSE
C        trim to point only
C         CALL DCPRNT(319)
C         CALL GETANS(C,HX2,HY2)
C         IF (MEN.EQ.2) RETURN
C         IF (CCMD.EQ.'Q' .OR. CCMD.EQ.'q') RETURN
C         IF (MEN.EQ.3) THEN
         CALL FINDP0(319,HX2,HY2,OPTION,QUIT)
         IF (QUIT) THEN
            CALL UNFLAG(.TRUE.)
            RETURN
         END IF
         IF (OPTION) THEN
            CALL TRML10(OK)
            IF ( .NOT. OK ) THEN
               CALL UNFLAG(.TRUE.)
               CALL ZSFLAG(.TRUE.,OK)
               GOTO 10
            END IF
            GOTO 20
         END IF
C        now got point to trim to
         CALL DCCPLP(DBLE(RDAT1(1)),DBLE(RDAT1(2)),DBLE(RDAT1(4)),
     +               DBLE(RDAT1(5)),DBLE(HX2),DBLE(HY2),DX,DY)
C        new point in DX,DY
C        copy to single precision
         X=REAL(DX)
         Y=REAL(DY)
C        end of trim point definition
      END IF
C
C     find the end of the line to trim
      D1=DISTXY(HX1,HY1,RDAT1(1),RDAT1(2))
      D2=DISTXY(HX1,HY1,RDAT1(4),RDAT1(5))
C
      IF (D1.LE.D2) THEN
C        trim first end point
         RDAT1(1)=X
         RDAT1(2)=Y
      ELSE
C        trim second end point
         RDAT1(4)=X
         RDAT1(5)=Y
      END IF
C
C     now have new line endpoints
C     remove flags from screen
      CALL UNFLAG(.TRUE.)
C
      IF( LNZERO(RDAT1) ) THEN
C      IF (SAME(ZERO(DISTXY(RDAT1(1),RDAT1(2),RDAT1(4),RDAT1(5)))
C     +             , ZERO(0.0))) THEN
         CALL DEPRNT(322)
         GOTO 10
      END IF
CAPOLLO|SUN
      VPMOV = .TRUE.
CAPOLLO|SUN
C     erase the old line.
      CALL PENERS()
      CALL ALLDRW(ENT1,MIP1)
      CALL PENDRW()
C
C     copy new data into place
      DO 70 I=1,6
         RDBUFF(I)=RDAT1(I)
 70   CONTINUE
C     write the modified data back
      CALL DEM500(MIP1,OK)
C     now draw the new line
      CALL ALLDRW(ENT1,MIP1)
C     go back try for another
CAPOLLO|SUN
      VPMOV = .FALSE.
CAPOLLO|SUN
      GOTO 10
C
      END
C
C
      SUBROUTINE TRML10(OK)
C     ===================
C
C1    no arguments required
C
C2    Subroutine TRML10 is the option control
C2     subroutine in the TRIM LINE function.
C2    Sets the necessary switches for trim actions.
C
      include 'include/movdat.inc'
      include 'include/menun.inc'
C
      LOGICAL OK
 
      EXTERNAL GTMCLO
C
      OK=.TRUE.
C
      IF (CCMD.EQ.'i') THEN
C        he has hit intof option
         IF (OPFLAG(1)) THEN
C           intof option already set
C           must cancel it
            OPFLAG(1)=.FALSE.
C           load points menu
            CALL MNLPTS()
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           must set intof option
            OPFLAG(1)=.TRUE.
C           Unload points menu
            CALL MNUPTS()
         END IF
      ELSE IF ( CCMD .EQ. CHAR(149) ) THEN
C        The user has cancelled the entity
         OK=.FALSE.
         CALL GTMCLO(MEN,CELLN)
      ELSE
C        must be invalid function
         CALL DEPRNT(323)
      END IF
C
      END
C
