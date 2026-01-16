C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 writerc2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE WRTLB1(LOCAL,LIB,OPUNIT,LBNAM,ST)
C     SUBROUTINE WRTLB8(PCBN,CDATA,LBNAM,ST)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE WRTLB1(LOCAL,LIB,OPUNIT,LBNAM,ST)
C     ============================================
C1    vartype
C1    iostatus
C
C2    Writes the component library section
C2    to the opened PASCII file connected to
C2    global unit REDUNT.
C
      include 'include/redboard.inc'
 
      LOGICAL OK
      INTEGER*2 LOCAL,LIB
      INTEGER*4 OPUNIT,ST,I,NLEN
      INTEGER*4 TUNIT,DNUM
      CHARACTER*(*) LBNAM
      EXTERNAL NLEN
C
      CALL OPNOFF(LIBNAM,TUNIT,OK)
 10   READ(UNIT=TUNIT,FMT='(A)',ERR=97,END=100) RDLINE
      EOFL=NLEN(RDLINE)
      IF ( RDLINE(1:1).NE.'L') GOTO 10
C     found library header
      CURP=2
C     get the number of entries in definition.
      CALL REDLIN(4)
      IF ( ERRCOD.NE.0 ) RETURN
C     not the right library.
      IF ( NANS(1).NE.LIB) GOTO 10
C     output header
      WRITE(UNIT=OPUNIT,FMT='(A,I4,3I5)',ERR=96) 'L',LOCAL,
     +                       NANS(2),NANS(3),NANS(4)
      DO 20 I=1,NANS(4)
        READ(UNIT=TUNIT,FMT='(A)',ERR=97,END=100) RDLINE
        WRITE(UNIT=OPUNIT,FMT='(A)',ERR=96) RDLINE(1:NLEN(RDLINE))
 20   CONTINUE
      WRITE(UNIT=OPUNIT,FMT='(A)')' '
C     read the next line to find if there is off grid data.
      READ(UNIT=TUNIT,FMT='(A)',ERR=97,END=100) RDLINE
      IF ( RDLINE(1:3).EQ.'OFF') LIB=-LIB
      CALL CLOSUN(TUNIT,.TRUE.,OK)
      ST=0
      RETURN
C
 100  CONTINUE
      RETURN
C
 96   CONTINUE
C     get here if error writing output file
      I=422
      CALL DEPRNT(I)
      ST=1
      GOTO 100
 97   CONTINUE
C     get here if error reading scratch file
      I=83
      CALL DEPRNT(I)
      ST=1
      GOTO 100
 98   CONTINUE
C     get here if error writing scratch file
      I=171
      CALL DEPRNT(I)
      ST=1
      GOTO 100
 101  CONTINUE
      CALL CLOSUN(TUNIT,.TRUE.,OK)
      DNUM = 759
      CALL DEPRNT(DNUM)
      ST=1
C
      END
C
C     --------------------------------------------------
C
C
C     SCCS id Keywords             @(#)  1.1 date 12/17/90 writerc2.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE WRTLB1(LOCAL,LIB,OPUNIT,LBNAM,ST)
C     SUBROUTINE WRTLB8(PCBN,CDATA,LBNAM,ST)
C
C     |-----------------------------------------------------------------|
C

      SUBROUTINE WRTLB8(PCBN,CDATA,LBNAM,ST)
C     =======================================
C1    vartype            I2   I2(4,2) C*(*) I2
C1    iostatus           I      O      I    O
C
C2    Returns the outline and pad data
C2    from the library comp PCBN
C2    in array CDATA
C
      include 'include/redboard.inc'
C
      LOGICAL OK
      INTEGER*2 PCBN,CDATA(4,2),ST
      INTEGER*4 OPUNIT,I,NLEN
      INTEGER*4 TUNIT
      CHARACTER*(*) LBNAM
      EXTERNAL NLEN
C
      CALL OPNOFF(LIBNAM,TUNIT,OK)
      IF (.NOT.OK) RETURN
      ST=1
 10   READ(UNIT=TUNIT,FMT='(A)',ERR=76,END=76) RDLINE
      EOFL=NLEN(RDLINE)
      IF ( RDLINE(1:1).NE.'L') GOTO 10
C     found library header
      CURP=2
C     get the number of entries in definition.
      CALL REDLIN(4)
      IF ( ERRCOD.NE.0 ) THEN
        OK=.FALSE.
        GOTO 75
      END IF
C     test for correct component number
      IF ( NANS(1).NE.PCBN) GOTO 10
C     copy array data into return array
      DO 15 I=1,4
        CDATA(I,1)=NANS(I)
 15   CONTINUE
C
C     if more than 1 pad,reject the comp
      IF (NANS(4).GT.1) THEN
C       fixing hole must have only 1 pad
        CALL DEPRNT(725)
        OK=.FALSE.
        GOTO 75
      END IF
C
C     read pad data
      READ(UNIT=TUNIT,FMT='(A)',ERR=75,END=76) RDLINE
      EOFL=NLEN(RDLINE)
      CURP=1
C     get the pad data
      CALL REDLIN(3)
      IF ( ERRCOD.NE.0 ) THEN
        OK=.FALSE.
        GOTO 75
      END IF
C     copy array data into return array
      DO 16 I=1,3
        CDATA(I,2)=NANS(I)
 16   CONTINUE
C     read the next line to find if there is off grid data.
      READ(UNIT=TUNIT,FMT='(A)',ERR=75,END=76) RDLINE
      IF ( RDLINE(1:3).EQ.'OFF') THEN
C       reject if off-grid data
        CALL DEPRNT(726)
        OK=.FALSE.
      END IF
C
 75   CONTINUE
      CALL CLOSUN(TUNIT,.TRUE.,OK)
      ST=0
      RETURN
 76   CONTINUE
C     reached end of file
      CALL CLOSUN(TUNIT,.TRUE.,OK)
      ST=1
      RETURN
C
 100  CONTINUE
C
      END
C
C     --------------------------------------------------
C
