C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 find.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE FINDA0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     SUBROUTINE FINDET(DNUM,HPX,HPY,MIPP,ENT,OPTION,QUIT)
C     SUBROUTINE FINDL0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     SUBROUTINE FINDLA(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     SUBROUTINE FINDP0(DNUM,HPX,HPY,OPTION,QUIT)
C     SUBROUTINE INTLAH(ENT1,RDAT1,ENT2,RDAT2,HPX,HPY,X,Y,OK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE FINDA0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     ==================================================
C
C1    vartype             I4    R   R   I2     L     L
C1    iostatus            I     O   O   O      O     O
C
C2    Subroutine FINDA0 requests a cursor hit from the operator
C2    with the prompt PROMPT,and searches for a ARC
C2    close to the hit point.If an arc is found,then the MI
C2    pointer is returned in MIPP,the hit point in HPX,HPY.
C2    If option menu 3 is hit,OPTION is returned true,if a
C2    quit status is received QUIT is returned true. This
C2    routine waits for valid input,returning only with a
C2    ARC entity, an option hit,or a quit.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
C
      INTEGER*4 C,DNUM
      INTEGER*2 MIPP
      REAL   HPX,HPY
      LOGICAL OK,OPTION,QUIT
      EXTERNAL DCPRNT,DEPRNT,TCURS,DSE800,NOSRCH,ADSRCH
C
C     start from here
 10   CONTINUE
C     prompt for line or arc input
      CALL DCPRNT(DNUM)
 15   CONTINUE
C     get cursor input
C     show search zone on cursor
      NPOS=12
      CALL TCURS(C,HPX,HPY)
C     cancel search zone on cursor
      NPOS=4
      OPTION=MEN.EQ.3
      QUIT=MEN.EQ.2 .OR. CCMD.EQ.'Q' .OR. CCMD.EQ.'q'
C     return if quit or option hit
      IF (QUIT.OR.OPTION) RETURN
C
C     search for arc close to hit point and flag it
      CALL NOSRCH()
      CALL ADSRCH(ARC)
      CALL DSE800(HPX,HPY,OK)
C
      IF (.NOT.OK) THEN
         CALL DEPRNT(108)
         GOTO 15
      END IF
C
      MIPP=MIP
C
C     return with pointer to entity
C
      END
C
C     -------------------------------------------------------
C
      SUBROUTINE FINDET(DNUM,HPX,HPY,MIPP,ENT,OPTION,QUIT)
C     ======================================================
C
C1    vartype             I4   R   R   I2   I2   L     L
C1    iostatus            I     O   O   O    O    O     O
C
C2    Subroutine FINDET requests a cursor hit from the operator
C2    with the prompt PROMPT,and searches for any entity
C2    close to the hit point.If one is found,then the MI
C2    pointer is returned in MIPP,the hit point in HPX,HPY.
C2    If option menu 3 is hit,OPTION is returned true,if a
C2    quit status is received QUIT is returned true. This
C2    routine waits for valid input,returning only with a
C2    LINE or ARC entity, an option hit,or a quit.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
      include 'include/movdat.inc'
C
      INTEGER*4 C,DNUM
      INTEGER*2 MIPP,ENT
      REAL   HPX,HPY
      LOGICAL OK,OPTION,QUIT
      EXTERNAL DCPRNT,DEPRNT,TCURS,DSE800
C
C     start from here
 10   CONTINUE
C     prompt for entity
      CALL DCPRNT(DNUM)
 15   CONTINUE
C     get cursor input
C     show search zone on cursor
      NPOS=12
      CALL TCURS(C,HPX,HPY)
C     cancel search zone on cursor
      NPOS=4
      OPTION=MEN.EQ.3
      QUIT=MEN.EQ.2 .OR. CCMD.EQ.'Q' .OR. CCMD.EQ.'q'
C     return if quit or option hit
      IF (QUIT.OR.OPTION) RETURN
C
C     search for any legal entity
      CALL DSE800(HPX,HPY,OK)
      IF (OK.AND.GSSTAT.EQ.3.AND.IMBUFF(1).EQ.50 ) GOTO 15
      IF (.NOT.OK) THEN
         CALL DEPRNT(142)
         GOTO 15
      END IF
C
C     Return MI pointer and entity type found
      MIPP=MIP
      ENT=IMBUFF(2)
C
C     return with pointer to entity
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE FINDL0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     ==================================================
C
C1    vartype            I4     R   R   I2     L     L
C1    iostatus            I     O   O   O      O     O
C
C2    Subroutine FINDL0 requests a cursor hit from the operator
C2    with the prompt PROMPT,and searches for a LINE
C2    close to the hit point.If a line is found,then the MI
C2    pointer is returned in MIPP,the hit point in HPX,HPY.
C2    If option menu 3 is hit,OPTION is returned true,if a
C2    quit status is received QUIT is returned true. This
C2    routine waits for valid input,returning only with a
C2    LINE entity, an option hit,or a quit.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
C
      INTEGER*4 C,DNUM
      INTEGER*2 MIPP
      REAL   HPX,HPY
      LOGICAL OK,OPTION,QUIT
      EXTERNAL DCPRNT,DEPRNT,TCURS,DSE800,NOSRCH,ADSRCH
C
C     start from here
 10   CONTINUE
C     prompt for line or arc input
      CALL DCPRNT(DNUM)
 15   CONTINUE
C     get cursor input
C     show search zone on cursor
      NPOS=12
      CALL TCURS(C,HPX,HPY)
C     cancel search zone on cursor
      NPOS=4
      OPTION=MEN.EQ.3
      QUIT=MEN.EQ.2 .OR. CCMD.EQ.'Q' .OR. CCMD.EQ.'q'
C     return if quit or option hit
      IF (QUIT.OR.OPTION) RETURN
C
C     search for line close to hit point
      CALL NOSRCH()
      CALL ADSRCH(LINE)
      CALL ADSRCH(CENLIN)
      CALL DSE800(HPX,HPY,OK)
C
      IF (.NOT.OK) THEN
         CALL DEPRNT(106)
         GOTO 15
      END IF
C
      MIPP=MIP
C
C     return with pointer to entity
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE FINDLA(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
C     ==================================================
C
C1    vartype            I4    R   R   I2     L     L
C1    iostatus            I     O   O   O      O     O
C
C2    Subroutine FINDLA requests a cursor hit from the operator
C2    with the prompt PROMPT,and searches for a LINE or ARC
C2    close to the hit point.If either is found,then the MI
C2    pointer is returned in MIPP,the hit point in HPX,HPY.
C2    If option menu 3 is hit,OPTION is returned true,if a
C2    quit status is received QUIT is returned true. This
C2    routine waits for valid input,returning only with a
C2    LINE or ARC entity, an option hit,or a quit.
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
C
      INTEGER*4 C,DNUM
      INTEGER*2 MIPP
      REAL   HPX,HPY,X2,Y2
      LOGICAL OK,OPTION,QUIT
      EXTERNAL DCPRNT,DEPRNT,TCURS,NOSRCH,ADSRCH,DSE800
C
C     start from here
 10   CONTINUE
C     prompt for line or arc input
      CALL DCPRNT(DNUM)
 15   CONTINUE
C     get cursor input
C     show search zone on cursor
      NPOS=12
      CALL TCURS(C,HPX,HPY)
C     cancel search zone on cursor
      NPOS=4
      OPTION=MEN.EQ.3
      QUIT=MEN.EQ.2 .OR. CCMD.EQ.'Q' .OR. CCMD.EQ.'q'
C     return if quit or option hit
      IF (QUIT.OR.OPTION) RETURN
C
C     search for line or arc close to hit point
      CALL NOSRCH()
      CALL ADSRCH(ARC)
      CALL ADSRCH(LINE)
      CALL ADSRCH(CENLIN)
C
      CALL DSE800(HPX,HPY,OK)
C
      IF (.NOT.OK) THEN
         CALL DEPRNT(142)
         GOTO 15
      END IF
C
      MIPP=MIP
C
C     If the entity is a centre line, disguise it as a line.
      IF (IMBUFF(2).EQ.CENLIN) THEN
C        Entity is center line, pretend it is a line.
C        Use the centre as the first end
C        and snap to the end of the centre line for the other.
         CALL CLNEND(HPX,HPY,X2,Y2)
         RDBUFF(4)=X2
         RDBUFF(5)=Y2
         IMBUFF(2)=LINE
      ENDIF
C
C     return with pointer to entity
C
      END
C
C     --------------------------------------------------
C
      SUBROUTINE FINDP0(DNUM,HPX,HPY,OPTION,QUIT)
C     ===========================================
C
C1    vartype             I4   R   R    L     L
C1    iostatus            I    O   O    O     O
C
C2    Subroutine FINDP0 requests a cursor hit from the operator
C2    with the prompt PROMPT,and returns  the hit point.
C2    HPX,HPY return the hit pint in world coords.
C2    If option menu 3 is hit,OPTION is returned true,if a
C2    quit status is received QUIT is returned true. This
C2    routine waits for valid input,returning only with a
C2    graphics hit, an option hit,or a quit.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/apollo.inc'
C
      INTEGER*4 C,DNUM
      REAL   HPX,HPY
      LOGICAL OPTION,QUIT
      EXTERNAL DCPRNT,GETANS
C
C     start from here
 10   CONTINUE
C     prompt for input
      CALL DCPRNT(DNUM)
 15   CONTINUE
C     get cursor input
C     show search box
      NPOS=12
      CALL GETANS(C,HPX,HPY)
C     cancel search box
      NPOS=4
      OPTION=(MEN.EQ.3)
      QUIT=(MEN.EQ.2 .OR. CCMD.EQ.'Q' .OR. CCMD.EQ.'q')
C
C     return with point and status
C
      END
C
C     ----------------------------------------------------
C
      SUBROUTINE INTLAH(ENT1,RDAT1,ENT2,RDAT2,HPX,HPY,X,Y,OK)
C     =======================================================
C
C1    vartype            I2   R(6)  I2   R(6)  R   R  R R  L
C1    iostatus           I     I    I     I    I   I  O O  O
C
C2    Subroutine INTLAH accepts the input data for two entities
C2    in the two arrays RDAT1,RDAT2, the entity types being passed
C2    in ENT1,ENT2. If the two entities are valid lines or arcs
C2    then the intersection point of the entities closest to HPX,HPY
C2    is returned in X,Y and the flag OK is returned true.
C2    If no intersection is found,or the entities are of the wrong
C2    type OK is returned false.
C
      include 'include/entity.inc'
C
      INTEGER*2 ENT1,ENT2,I
      REAL X,Y,HPX,HPY,RDAT1(6),RDAT2(6),DISTXY,D1,D2,REAL,ABS
      DOUBLE PRECISION DD1(6),DD2(6),DX1,DY1,DX2,DY2,DBLE
      LOGICAL OK
      INTRINSIC DBLE,REAL,ABS
      EXTERNAL DCC0P5,DCC0P9,DCCP19,DISTXY
C
      OK=.FALSE.
C     check entity types first
      IF(ENT1.NE.LINE .AND. ENT1.NE.ARC) RETURN
      IF(ENT2.NE.LINE .AND. ENT2.NE.ARC) RETURN
C
C     copy data to double precision
      DO 5 I=1,6
        DD1(I)=RDAT1(I)
        DD2(I)=RDAT2(I)
 5    CONTINUE
C
C     try for two line intersection first
      IF (ENT1.EQ.LINE .AND. ENT2.EQ.LINE) THEN
C        two lines alright,should be easy
C        go get the intersection
         CALL DCC0P5(DD1(1),DD1(2),DD1(4),DD1(5),
     +               DD2(1),DD2(2),DD2(4),DD2(5),
     1               DX1,DY1,OK)
         IF (OK) THEN
C           intersection found,return it
            X=REAL(DX1)
            Y=REAL(DY1)
         END IF
         RETURN
      ELSE IF (ENT1.EQ.ARC .AND. ENT2.EQ.ARC) THEN
C        try for two arcs intersecting
         CALL DCCP19(DD1(1),DD1(2),DD1(4),
     +               DD2(1),DD2(2),DD2(4),
     1               DX1,DY1,DX2,DY2,OK)
      ELSE IF (ENT1.EQ.LINE) THEN
C        try line as first entity,arc must be second
         CALL DCC0P9(DD2(1),DD2(2),DD2(4),
     +               DD1(1),DD1(2),DD1(4),DD1(5),
     1               DX1,DY1,DX2,DY2,OK)
C
      ELSE
C        must have arc first,line second
         CALL DCC0P9(DD1(1),DD1(2),DD1(4),
     +               DD2(1),DD2(2),DD2(4),DD2(5),
     1               DX1,DY1,DX2,DY2,OK)
C
      END IF
C
C     trap errors here
      IF(.NOT.OK) RETURN
C
C     find intersection closest to hit point
      D1=ABS(DISTXY(HPX,HPY,REAL(DX1),REAL(DY1)))
      D2=ABS(DISTXY(HPX,HPY,REAL(DX2),REAL(DY2)))
C
      IF (D1.LE.D2) THEN
         X=REAL(DX1)
         Y=REAL(DY1)
      ELSE
         X=REAL(DX2)
         Y=REAL(DY2)
      END IF
C
C     return with intersection point in X,Y
C
      END
C
C     --------------------------------------------------
C
C
