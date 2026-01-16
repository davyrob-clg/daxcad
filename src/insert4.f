C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 insert4.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     REAL FUNCTION MINORA(ANGLE)
C     LOGICAL FUNCTION PARRLL(B1,B2)
C     SUBROUTINE INSA10(MIPL,XY)
C     SUBROUTINE INSA3L(CLOCK)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE INSA10(MIPL,XY)
C     ==========================
C1    VARTYPE            I2  R
C1    IOSTATS            I   O
C
C2    This routine copies buffers
C
      include   'include/masti.inc'
      include   'include/nbuff.inc'
C
      REAL XY(4,3)
      INTEGER*2 MIPL(3),I
      LOGICAL OK
C
      DO 10 I =1,3
C
         CALL DER500 ( MIPL(I) , OK)
C
         XY(1,I) =RDBUFF(1)
         XY(2,I) =RDBUFF(2)
         XY(3,I) =RDBUFF(4)
         XY(4,I) =RDBUFF(5)
C
 10   CONTINUE
C
      END
 
      SUBROUTINE INSA3L(CLOCK)
c     ========================
      include   'include/style.inc'
      include   'include/masti.inc'
      include   'include/menun.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
      include   'include/arcdat.inc'
      include   'include/curwin.inc'
      include   'include/viewport.inc'
C
      REAL HPX,HPY,SANG,EANG,TEMP
      REAL XY(4,3),IP(4,3),ANGLES(2,3)
      REAL X1,Y1,X2,Y2,X3,Y3,CANG,LIM
      REAL SX1,SY1,SX2,SY2
      INTEGER*4 IST
      REAL RADIUS,CENTRE(2,4)
      REAL C(3,3),PI,MAGLIN
      REAL DIST1,DIST2,DIST3,DIST,CD0D13,DIST4
      REAL ANG,X,Y,DISTXY,L1,L2,L3,MINORA
      REAL CENX,CENY,XP,YP
      REAL VN00D6
      REAL C1,C2,C3,D1,D2,D3,V1,V2,V3,F1,F2,F3
      REAL T1,T2,P,T3
      INTEGER*2 MIPP,MIPL(3),ENT
      INTEGER*4 TFONT,DNUM,NUM,TCELL,TMEN,I,PL
      LOGICAL OK,OPTION,QUIT,CLOCK,PAR,PARRLL
C
      EXTERNAL CANG,PI,CD0D13,DISTXY,MINORA,PARRLL
C
C
C     ensure the caller cell is hilited
      TMEN = MEN
      TCELL = CELLN
C
C     Load fixed ANGLE option
      FPOS = NMIPOS
      FIRST1 = .TRUE.
      CALL MNI02A()
C     write the current font number into the cell
      CALL MNISTD()
C     enter the point modes to the option menu
C     get thrree lines
C
C
C
1000  CONTINUE
C
320   CONTINUE
C
      PL = 0
      PAR = .FALSE.
      CALL DCPRNT(640)
C
      DNUM= 191
      CALL FINDL0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
      IF(QUIT.OR.MEN.EQ.2) GOTO 99
      IF(OPTION) THEN
          CALL INS0AM(OK)
          IF(.NOT.OK) GOTO 99
          GOTO 320
      ENDIF
      X1 = HPX
      Y1 = HPY
      MIPL(1) = MIPP
 
 300  CONTINUE
 
      CALL FINDL0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
      IF(QUIT.OR.MEN.EQ.2) GOTO 99
      IF(OPTION) THEN
          CALL INS0AM(OK)
          IF(.NOT.OK) GOTO 99
          GOTO 300
      ENDIF
      X2 = HPX
      Y2 = HPY
      IF(MIPL(1).EQ.MIPP) THEN
          CALL DEPRNT(641)
          GOTO 300
      ENDIF
      MIPL(2) = MIPP
 
 310  CONTINUE
      CALL FINDL0(DNUM,HPX,HPY,MIPP,OPTION,QUIT)
      IF(QUIT.OR.MEN.EQ.2) GOTO 99
      IF(OPTION) THEN
          CALL INS0AM(OK)
          IF(.NOT.OK) GOTO 99
          GOTO 320
      ENDIF
      X3 = HPX
      Y3 = HPY
      IF(MIPL(1).EQ.MIPP.OR.MIPL(2).EQ.MIPP) THEN
          CALL DEPRNT(641)
          GOTO 310
      ENDIF
      MIPL(3) = MIPP
C
C     got to here we have three lines
C
C     copy buffers
      CALL INSA10(MIPL,XY)
C
C     find three intersection points
D      WRITE(10,*) 'XY= ',(XY(I,1),I=1,4)
D      WRITE(10,*) 'XY= ',(XY(I,2),I=1,4)
D      WRITE(10,*) 'XY= ',(XY(I,3),I=1,4)
C
      CALL INTERS(LINE,XY(1,1),XY(2,1),XY(3,1),XY(4,1),
     +            LINE,XY(1,2),XY(2,2),XY(3,2),XY(4,2),
     1            IP(1,1),IP(2,1),IP(3,1),IP(4,1),
     2            SX1,SY1,SX2,SY2,IST,OK)
      IF(PARRLL(XY(1,1),XY(1,2)))  THEN
          PAR = .TRUE.
          PL = 12
      ENDIF
      IF(.NOT.OK.AND..NOT.PAR) THEN
C
          CALL DEPRNT(642)
          GOTO 320
      ENDIF
C
      CALL INTERS(LINE,XY(1,1),XY(2,1),XY(3,1),XY(4,1),
     +            LINE,XY(1,3),XY(2,3),XY(3,3),XY(4,3),
     1            IP(1,2),IP(2,2),IP(3,2),IP(4,2),
     2            SX1,SY1,SX2,SY2,IST,OK)
      IF(PARRLL(XY(1,1),XY(1,3))) THEN
          IF(.NOT.PAR) THEN
              PAR = .TRUE.
              PL = 13
          ENDIF
      ENDIF
      IF(.NOT.OK.AND..NOT.PAR) THEN
          CALL DEPRNT(642)
          GOTO 320
      ENDIF
      CALL INTERS(LINE,XY(1,2),XY(2,2),XY(3,2),XY(4,2),
     +            LINE,XY(1,3),XY(2,3),XY(3,3),XY(4,3),
     1            IP(1,3),IP(2,3),IP(3,3),IP(4,3),
     2            SX1,SY1,SX2,SY2,IST,OK)
 
      IF( PARRLL(XY(1,3),XY(1,2))) THEN
          IF(.NOT.PAR) THEN
              PAR = .TRUE.
              PL = 23
          ENDIF
      ENDIF
C
      IF(.NOT.OK.AND..NOT.PAR) THEN
          CALL DEPRNT(642)
          GOTO 320
      ENDIF
C
C     By now  we have 3 intersection points
C
C     Find the angles on between the lines each maps the the ip
D      WRITE(10,*) '[INSAL3] '
D      WRITE(10,*) 'IP= ',(IP(I,1),I=1,4)
D      WRITE(10,*) 'IP= ',(IP(I,2),I=1,4)
D      WRITE(10,*) 'IP= ',(IP(I,3),I=1,4)
C
C
C
 
      IF(PL.NE.0) THEN
C
C     **************************************************
C                 PARALLEL LINES PRESENT
C     **************************************************
C
          CALL CV0L14(XY(1,1),XY(2,1),XY(3,1),XY(4,1),
     +    C(1,1),C(2,1),C(3,1))
          CALL CV0L14(XY(1,2),XY(2,2),XY(3,2),XY(4,2),
     +    C(1,2),C(2,2),C(3,2))
          CALL CV0L14(XY(1,3),XY(2,3),XY(3,3),XY(4,3),
     +    C(1,3),C(2,3),C(3,3))
          IF(PL.EQ.12) THEN
              CALL CV0L14(IP(1,2),IP(2,2),IP(1,3),IP(2,3),
     +                    L1,L2,L3)
 
              C1 = C(1,1)
              C2 = C(2,1)
              C3 = C(3,1)
              CALL VC0PLP(C1,C2,C3,X1,Y1,XP,YP)
              CALL CV0L14(IP(1,2),IP(2,2),XP,YP,C1,C2,C3)
              V1 = C(1,2)
              V2 = C(2,2)
              V3 = C(3,2)
              CALL VC0PLP(V1,V2,V3,X2,Y2,XP,YP)
              CALL CV0L14(IP(1,3),IP(2,3),XP,YP,V1,V2,V3)
              X = IP(1,2)
              Y = IP(2,2)
          ELSEIF(PL.EQ.23) THEN
              CALL CV0L14(IP(1,1),IP(2,1),IP(1,2),IP(2,2),
     +                    L1,L2,L3)
              C1 = C(1,2)
              C2 = C(2,2)
              C3 = C(3,2)
              CALL VC0PLP(C1,C2,C3,X2,Y2,XP,YP)
              CALL CV0L14(IP(1,1),IP(2,1),XP,YP,C1,C2,C3)
              V1 = C(1,3)
              V2 = C(2,3)
              V3 = C(3,3)
              X = IP(1,1)
              Y = IP(2,1)
          ELSEIF(PL.EQ.13) THEN
              CALL CV0L14(IP(1,1),IP(2,1),IP(1,3),IP(2,3),
     +                    L1,L2,L3)
              C1 = C(1,1)
              C2 = C(2,1)
              C3 = C(3,1)
              CALL VC0PLP(C1,C2,C3,X1,Y1,XP,YP)
              CALL CV0L14(IP(1,1),IP(2,1),XP,YP,C1,C2,C3)
              V1 = C(1,3)
              V2 = C(2,3)
              V3 = C(3,3)
              X = IP(1,1)
              Y = IP(2,1)
          ENDIF
          ANG  = VN00D6(L1,L2,L3,C1,C2,C3)
          ANGLES(1,1) = MINORA(ANG)
          ANGLES(1,1) = SIGN(ANGLES(1,1),(PI(1.0) - ANG))
          ANG = MINORA(ANG)
          ANGLES(2,1) = (PI(1.0)- ANG)
          ANGLES(2,1) = SIGN(ANGLES(2,1),-ANGLES(1,1))
C         generate the centre vector
          DIST = MAGLIN(C1,C2,C3)
          T1 = C3/DIST
          T2 = V3/MAGLIN(V1,V2,V3)
D      WRITE(10,*) '[INS4] X Y = ',X,Y
D      WRITE(10,*) '[INS4] T1 T2 = ',T1,T2,PL
D      WRITE(10,*) 'ANGLES= ',(ANGLES(I,1),I=1,2)
          T3 = (T1+T2)/2.0
          D1 = C1
          D2 = C2
          D3 = DIST*T3
C         geneate first centre
          CALL VV00L5(L1,L2,L3,X,Y,ANGLES(1,1)/2.0,
     +            F1,F2,F3)
          CALL VC00P5( D1,D2,D3,F1,F2,F3,
     +            CENTRE( 1,1),CENTRE(2,1) ,OK )
C         geneate second centre
          CALL VV00L5(L1,L2,L3,X,Y,ANGLES(2,1)/2.0,
     +            F1,F2,F3)
          CALL VC00P5( D1,D2,D3,F1,F2,F3,
     +            CENTRE( 1,2),CENTRE(2,2) ,OK )
 
C         skip the rest
          GOTO 2000
      ENDIF
 
      CALL CN00D6(IP(1,1),IP(2,1),IP(1,2),IP(2,2),
     +            IP(1,1),IP(2,1),IP(1,3),IP(2,3),ANG)
 
D      WRITE(10,*) 'ANG =',ANG
C     interior
C     get the correct range
      ANGLES(1,1) = MINORA(ANG)
      ANGLES(1,1) = SIGN(ANGLES(1,1),(PI(1.0) - ANG))
C
C     exterior
      ANG = MINORA(ANG)
      ANGLES(2,1) = (PI(2.0) -2.0*ANG)/2.0
      ANGLES(2,1) = SIGN(ANGLES(2,1),-ANGLES(1,1))
C
C     Find the angles on between the lines each maps the the ip
      CALL CN00D6(IP(1,2),IP(2,2),IP(1,3),IP(2,3),
     +            IP(1,2),IP(2,2),IP(1,1),IP(2,1),ANG)
D      WRITE(10,*) 'ANG =',ANG
C
C     interior
C     get the correct range
      ANGLES(1,2) = MINORA(ANG)
      ANGLES(1,2) = SIGN(ANGLES(1,2),(PI(1.0) - ANG))
C
C
C     exterior
      ANG = MINORA(ANG)
      ANGLES(2,2) = (PI(2.0) -2.0*ANG)/2.0
      ANGLES(2,2) = SIGN(ANGLES(2,2),-ANGLES(1,2) )
C
C     Find the angles on between the lines each maps the the ip
      CALL CN00D6(IP(1,3),IP(2,3),IP(1,1),IP(2,1),
     +            IP(1,3),IP(2,3),IP(1,2),IP(2,2),ANG)
 
      WRITE(10,*) 'ANG =',ANG
C     interior
C     get the correct range
      ANGLES(1,3) = MINORA(ANG)
      ANGLES(1,3) = SIGN(ANGLES(1,3),(PI(1.0) - ANG))
C
C
C     exterior
      ANG = MINORA(ANG)
      ANGLES(2,3) = (PI(2.0) -2.0*ANG)/2.0
      ANGLES(2,3) = SIGN(ANGLES(2,3),-ANGLES(1,3) )
D      WRITE(10,*) 'ANGLES= ',(ANGLES(I,1),I=1,2)
D      WRITE(10,*) 'ANGLES= ',(ANGLES(I,2),I=1,2)
D      WRITE(10,*) 'ANGLES= ',(ANGLES(I,3),I=1,2)
C
C     the 3 angles are now relative to the vectors
C
C
C     ********************************************************
C         CIRCLE 1.      LINE 1  INTERSECTION POINTS 1 AND 2
C     ********************************************************
C     Calculate radius and center points
C
C     calculate the vector along the first line
      CALL CV0L14(IP(1,1),IP(2,1),IP(1,2),IP(2,2),L1,L2,L3)
C
      CALL VV00L5(L1,L2,L3,IP(1,1),IP(2,1),ANGLES(2,1)/2.0,
     +            C(1,1),C(2,1),C(3,1))
C
C
C     ********************************************************
C         CIRCLE 2.      LINE 2  INTERSECTION POINTS 1 AND 3
C     ********************************************************
C     Calculate radius and center points
C     calculate the vector along the second line
      CALL CV0L14(IP(1,3),IP(2,3),IP(1,1),IP(2,1),L1,L2,L3)
C
      CALL VV00L5(L1,L2,L3,IP(1,3),IP(2,3),ANGLES(2,2)/2.0,
     +            C(1,2),C(2,2),C(3,2))
C
C     ********************************************************
C         CIRCLE 3.      LINE 2  INTERSECTION POINTS 1 AND 3
C     ********************************************************
C     calculate the vector along the third line
      CALL CV0L14(IP(1,2),IP(2,2),IP(1,3),IP(2,3),L1,L2,L3)
C
      CALL VV00L5(L1,L2,L3,IP(1,2),IP(2,2),ANGLES(2,3)/2.0,
     +            C(1,3),C(2,3),C(3,3))
C
C     now to get intersection points which will be the 3 centres
      CALL VC00P5( C(1,1),C(2,1),C(3,1),C(1,3),C(2,3),C(3,3),
     +            CENTRE( 1,1),CENTRE(2,1) ,OK )
C     now to get intersection points which will be the 3 centres
      CALL VC00P5( C(1,1),C(2,1),C(3,1),C(1,2),C(2,2),C(3,2),
     +            CENTRE( 1,2),CENTRE(2,2) ,OK )
C     now to get intersection points which will be the 3 centres
      CALL VC00P5( C(1,2),C(2,2),C(3,2),C(1,3),C(2,3),C(3,3),
     +            CENTRE( 1,3),CENTRE(2,3) ,OK )
C
C
C
C     ********************************************************
C         CIRCLE 4.      INCENTRE CIRCLE
C     ********************************************************
C     take lines 1 and 2 for now
C
C     calculate the vector along the first line
      CALL CV0L14(IP(1,1),IP(2,1),IP(1,2),IP(2,2),L1,L2,L3)
C
      CALL VV00L5(L1,L2,L3,IP(1,1),IP(2,1),ANGLES(1,1)/2.0,
     +            C(1,1),C(2,1),C(3,1))
 
C     calculate the vector along the second line
      CALL CV0L14(IP(1,2),IP(2,2),IP(1,3),IP(2,3),L1,L2,L3)
C
      CALL VV00L5(L1,L2,L3,IP(1,2),IP(2,2),ANGLES(1,2)/2.0,
     +            C(1,2),C(2,2),C(3,2))
 
C     get center points
      CALL VC00P5( C(1,1),C(2,1),C(3,1),C(1,2),C(2,2),C(3,2),
     +            CENTRE( 1,4),CENTRE(2,4) ,OK )
C
C
C     **********************************************
C             get start and end points
C     **********************************************
C     determine which center you actaly want
C
2000  CONTINUE
CC
      CALL MNLPTS()
340   CONTINUE
      DNUM = 179
      CALL FINDP0(DNUM,HPX,HPY,OPTION,QUIT)
      IF(QUIT.OR.MEN.EQ.2) GOTO 99
      IF(OPTION) THEN
          CALL INS0AM(OK)
          IF(.NOT.OK) GOTO 99
          GOTO 340
      ENDIF
      X1 = HPX
      Y1 = HPY
      IF(.NOT.ARCSET.AND..NOT.CIRSET) THEN
C         get the end point of the arc
350       CONTINUE
          DNUM = 180
          CALL FINDP0(DNUM,HPX,HPY,OPTION,QUIT)
          IF(QUIT.OR.MEN.EQ.2) GOTO 99
          IF(OPTION) THEN
              CALL INS0AM(OK)
              IF(.NOT.OK) GOTO 99
              GOTO 350
          ENDIF
          X2 = HPX
          Y2 = HPY
      ENDIF
      CALL MNUPTS()
C
C     determine the centre from the start point of the arc
C
      DIST1 = DISTXY(X1,Y1,CENTRE(1,1),CENTRE(2,1))
      DIST2 = DISTXY(X1,Y1,CENTRE(1,2),CENTRE(2,2))
      DIST3 = DISTXY(X1,Y1,CENTRE(1,3),CENTRE(2,3))
C
      DIST = 1E38
C     find the smallest distance
      IF(DIST1.LT.DIST) THEN
          DIST = DIST1
          NUM = 1
      ENDIF
      IF(DIST2.LT.DIST) THEN
          DIST = DIST2
          NUM = 2
      ENDIF
      IF(DIST3.LT.DIST.AND.PL.GT.0) THEN
          NUM = 3
      ENDIF
 
      IF(PL.EQ.0) THEN
 
         IF(NUM.EQ.1) THEN
C             test whic side of line centre and start point are on
C             if same side then choose circle.
              CALL CV0L14(IP(1,1),IP(2,1),IP(1,2),IP(2,2),L1,L2,L3)
          ELSEIF(NUM.EQ.2) THEN
              CALL CV0L14(IP(1,1),IP(2,1),IP(1,3),IP(2,3),L1,L2,L3)
          ELSEIF(NUM.EQ.3) THEN
              CALL CV0L14(IP(1,3),IP(2,3),IP(1,2),IP(2,2),L1,L2,L3)
          ENDIF
C
          DIST1 = (L1*X1 + L2*Y1 + L3)/MAGLIN(L1,L2,L3)
          DIST2 = (L1*CENTRE(1,NUM) + L2*CENTRE(2,NUM) + L3)/
     +            MAGLIN(L1,L2,L3)
 
          TEMP = DIST1*DIST2
C          WRITE (10,*) '[INS4] DIST1= ',DIST1,DIST2,TEMP,NUM
          IF( TEMP.LT.0.0 ) NUM = 4
 
      ENDIF
D      WRITE (10,*) '[INS4] DIST1= ',DIST1,DIST2,NUM
C
C     is the start point in the incentre
C
 
C
2010  CONTINUE
      SANG = CANG(CENTRE(1,NUM),CENTRE(2,NUM),X1,Y1)
C
      IF(ARCSET) THEN
C
          EANG = ANGLE
C
      ELSE
C
          EANG = CANG(CENTRE(1,NUM),CENTRE(2,NUM),X2,Y2)
C
      ENDIF
      IF(CIRSET) EANG=MOD(SANG+360.0,360.0)
C
C     calculate perpendicular distance from centre to a line
      IF(PL.EQ.0) THEN
          IF(NUM.EQ.1) THEN
              RADIUS =    CD0D13(IP(1,1),IP(2,1),IP(1,2),IP(2,2),
     +                    CENTRE(1,NUM),CENTRE(2,NUM) )
          ELSEIF(NUM.EQ.2) THEN
              RADIUS =    CD0D13(IP(1,1),IP(2,1),IP(1,3),IP(2,3),
     +                    CENTRE(1,NUM),CENTRE(2,NUM) )
          ELSEIF(NUM.EQ.3) THEN
              RADIUS =    CD0D13(IP(1,2),IP(2,2),IP(1,3),IP(2,3),
     +                    CENTRE(1,NUM),CENTRE(2,NUM) )
          ELSEIF(NUM.EQ.4) THEN
              RADIUS =    CD0D13(IP(1,2),IP(2,2),IP(1,3),IP(2,3),
     +                    CENTRE(1,NUM),CENTRE(2,NUM) )
          ENDIF
      ELSE
          RADIUS = (T1-T2)/2.0
      ENDIF
C     we now have every thing
C     draw the arc in place
      CENX = CENTRE(1,NUM)
      CENY = CENTRE(2,NUM)
C     mod radius
      RADIUS= ABS(RADIUS)
      IF(CLOCK) THEN
          TEMP = SANG
          SANG = EANG
          EANG = TEMP
      ENDIF
C
C
CAPOLLO|SUN
      VPADD = .TRUE.
CAPOLLO|SUN
C     store the new arc
      CALL DEWC05(CENX,CENY,RADIUS,SANG,EANG,CLFONT,CLAYER,P,OK)
C     erase crosses from screen
C      IF( CR1 ) CALL CROSS(SX1,SY1)
C      IF( CR2 ) CALL CROSS(SX2,SY2)
C      IF( CR3 ) CALL CROSS(SX3,SY3)
      IF ( .NOT. OK ) THEN
         GOTO 1000
      END IF
C
C     draw the arc just created
      ENT = ARC
      CALL ALLDRW(ENT,P)
C
CAPOLLO|SUN
      VPADD = .FALSE.
CAPOLLO|SUN
C
C     set global cancel flag
      FIRST1 = .FALSE.
C     unflag all entities
      COUNT =0
      CALL UNFLAG(.TRUE.)
C     start again
      GOTO 1000
99    CONTINUE
C     low the cell
      CALL GTMCLO(TMEN,TCELL)
      END
C
      REAL FUNCTION MINORA(ANGLE)
C     ===========================
C1    VARTYPE                R
C1    IOSTAT                 I
C
C2    This function will return the minor of the angle given
C
      REAL ANGLE,PI
C
      EXTERNAL PI
C
      IF(ANGLE.GT.PI(1.0)) THEN
C
          MINORA = PI(2.0) - ANGLE
C
      ELSE
C
          MINORA = ANGLE
C
      ENDIF
C
      END
 
      LOGICAL FUNCTION PARRLL(B1,B2)
C     ==============================
C1
C1
C
C2
C2
C
      REAL B1(4),B2(4)
      LOGICAL SAME,OK
      REAL MAGLIN
      REAL T1,T2,X,Y
      REAL L1,L2,L3,C1,C2,C3
C
      EXTERNAL MAGLIN,SAME
 
C
C     generate two vectors
      CALL CV0L14(B1(1),B1(2),B1(3),B1(4),L1,L2,L3)
      CALL CV0L14(B2(1),B2(2),B2(3),B2(4),C1,C2,C3)
C     find a valid intersection
      CALL VC00P5(L1,L2,L3,C1,C2,C3,X,Y,OK)
C     if lines intersect then not parallel (bloody obvious)
      IF(.NOT.OK) THEN
C         get perpendicular offset from origin
          T1 = L3/MAGLIN(L1,L2,L3)
          T2 = C3/MAGLIN(C1,C2,C3)
      ENDIF
C     if same the not parralell
      PARRLL = .NOT.SAME(T1,T2)
      END
 
 
 
 
