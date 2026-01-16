C---------------------------------------------------------------------
C
      SUBROUTINE UCODET(TEMP,SLA,JUST,NCHAR)
C     ======================================
C1    vartype             R , R ,  I2, I2
C1    iostatus            I   O    O   O
C
      REAL TEMP,Y,SLA
      INTEGER*2 NCHAR,JUST,I(2)
      EQUIVALENCE(Y,I)
      INTRINSIC MOD,INT
C
      Y=TEMP
      NCHAR=I(2)
      SLA=MOD(I(1)+0,256)-90
      JUST=INT(I(1)/256)
CSUN386
C      NCHAR=I(1)
C      SLA=MOD(I(2)+0,256)-90
C      JUST=INT(I(2)/256)
CSUN386
      END      

C
C---------------------------------------------------------------------
C
      SUBROUTINE CODET(SLA,JUST,NCHAR,TEMP)
C     ======================================
C1    vartype           R ,  I2, I2  ,  R
C1    iostatus          I    I   I      O
C
C2    Subroutine CODET encodes the SLANT of a text
C2    string,together with JUSTIFICATION and character
C2    count in a REAL variable.
C2    The acceptable justification range is 1 to 9.
C2    The acceptable SLANT range is -90.0 to +90.0 degrees
C2    Positive slant is clockwise from vertical.
C2    The max no chars is 255.
C2    Encoded form of data as follows:
C2          High Word   Low Word
C2        |JUST |SLANT|  NCHAR   |
C2    The encoding assumes Motorola storage convention.
C2    ie 4-byte word stored in ascending memory
C2    as byte4,byte3,byte2,byte1.
C
      REAL TEMP,SLA,X,S
      INTEGER*2 I(2),JUST,NCHAR,J1
      EQUIVALENCE(I,X)
C
C     ensure justification is in range 1-9
      J1=MAX(INT(1),INT(JUST))
      J1=MIN(INT(9),INT(J1))
C     ensure SLANT within range
      S=MOD(SLA,90.0)
      I(1)=((256*J1)+INT(S+90))
      I(2)=NCHAR
CSUN386
C      I(2)=((256*J1)+INT(S+90))
C      I(1)=NCHAR
CSUN386
      TEMP=X
C
      END
C
C-------------------------------------------------------------
C
