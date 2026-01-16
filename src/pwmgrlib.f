C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 pwmgrlib.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE K10001(ST)
C     SUBROUTINE K10002(LINE)
C     SUBROUTINE K10018(LIC,N)
C     SUBROUTINE K10019(LIC,N,YMD)
C     SUBROUTINE K10020(ND,USE,K)
C     SUBROUTINE K10021(K,ND,USE)
C     SUBROUTINE K10022(D,M,Y,YMD)
C     SUBROUTINE K10023(YMD,D,M,Y)
C     SUBROUTINE K10024(H,M,S,HMS)
C     SUBROUTINE K10025(HMS,H,M,S)
C     SUBROUTINE K10030(ST)
C     SUBROUTINE K10031(ST)
C     SUBROUTINE K10033(PASCODE,ST)
C     SUBROUTINE K10040(CODE,ST)
C     SUBROUTINE DECODEPASSWD(PASCODE,NODE,TERM,ACCESS)
C     SUBROUTINE CHKPRD(PROD, AUTH)
C
      SUBROUTINE K10001(ST)
C     ===================
C
C
C2    Subroutine K10001 determines the number
C2    of the working node and returns it in NN.
C
      include  'include/krap.inc'
      include  'include/library.inc'
C
      INTEGER*4 I, ST
      EXTERNAL NLEN

C
C     clear node id
      NODEID = 0
C     get machine dependant nodeid number
      CALL GETNODEID(NODEID)
C
      END
C
C     ------------------------------------------
C
C
      SUBROUTINE K10018(LIC,N)
C     ========================
C
C1    vartype           I   I
C1    iostatus          I   O
C
C2    Subroutine K10018 returns the number of days N
C2    contained within LIC seconds.
C
      INTEGER*4 LIC,N
C
C     calc number of days
      N=LIC/24/3600
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10019(LIC,N,YMD)
C     ============================
C
C1    vartype            I  I  I
C1    iostatus           I  I  O
C
C2    Subroutine K10019 returns the date YMD
C2    which is N days after day 1 of licence number
C2    LIC. YMD is a composite integer of form 19871225
C2    to define 27th Dec 1987.
C
      INTEGER LIC,N,YMD,D,M,Y,N1,N2
C
C     get number of days in licence
      CALL K10018(LIC,N1)
C     calc total number of days authorized
      N2=N1+N
C     get end date N2 days from 11/11/1982
      CALL DAYSFR(N2,11,11,1982,D,M,Y)
C     create composite integer
      CALL K10022(D,M,Y,YMD)
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10020(ND,USE,K)
C     ===========================
C
C1    vartype           I   I  I
C1    iostatus          I   I  O
C
C2    Subroutine K10020 returns the integer K
C2    containing the composite of ND days
C2    and USE user access level.
C
      INTEGER ND,USE,K
C
C     calc composite,allows for 15 access levels
      K = (USE*524288) + ND
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10021(K,ND,USE)
C     ===========================
C
C1    vartype           I  I  I
C1    iostatus          I  O  O
C
C2    Subroutine K10021 returns the integers
C2    containing the number ND days
C2    and USE user access level, extracted from
C2    composite value K.
C
      INTEGER ND,USE,K
C
C     calc composite,allows for 15 access levels
      USE = K/524288
      ND = K - ( USE*524288)
C
      END
C
      SUBROUTINE K10022(D,M,Y,YMD)
C     ============================
C
C1    vartype           I I I  I
C1    iostatus          I O O  O
C
C2    Subroutine K10022 returns the composite
C2    integer containing the passed Day,Month and Year
C2    on the form 21121 for 21st Nov 1992.
C
      INTEGER D,M,Y,YMD
C
C     calc composite
      YMD=Y*10000+M*100+D
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10024(H,M,S,HMS)
C     ============================
C
C1    vartype           I I I  I
C1    iostatus          I O O  O
C
C2    Subroutine K10024 returns the composite
C2    integer containing the passed Hour,Minute,Seconds
C2    on the form 192130 for 19:21:30.
C
      INTEGER H,M,S,HMS
C
C     calc composite
      HMS=H*10000+M*100+S
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10030(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
C2    Subroutine K10030 opens the DAXCAD system
C2    password file, and sets the default entry
C2    parameters in the common block.
C2    ST returns zero if file opened successfully
C2    and basic data retrieved.
C2    Return status:
C2          ST = 1 Cannot find free unit
C2          ST = 2 Cannot get access to file
C
      include  'include/krap.inc'
      include  'include/product.inc'
      include  'include/library.inc'
C
      INTEGER ST,RN,TRIES,I,RECL,TRYLIM
      INTEGER*4 NLEN1
      REAL DELAY
      LOGICAL OK
      CHARACTER*80 SYSFIL
      EXTERNAL NLEN1
C
C     set limitting number of tries to access file
      TRYLIM=8
C     initialize count of tries at file open
      TRIES=0
C     set time to wait before retry for file access (seconds)
      DELAY=3.0
C     set name of system file
      SYSFIL=LIBRARY(1:NLEN1(lIBRARY))//'/passwd'
C     find unit for system file
      CALL FINDU1(PASUNT,OK)
      IF (.NOT.OK) THEN
C        cannot find free unit
         ST=1
         RETURN
      END IF
C
 3    CONTINUE
C     open the system file
      OPEN(UNIT=PASUNT,FILE=SYSFIL,STATUS='UNKNOWN',ERR=701)
      ST = 0
      RETURN
701   ST = 2
      WRITE(*,'(2A)') 
     +'[PROTECTION] Cannot open password file (LOCKED): ',SYSFIL

      END
C
C     ------------------------------------------
C
      SUBROUTINE K10031(ST)
C     =====================
C
C1    vartype           I
C1    iostatus          O
C
C2    Subroutine K10031 closes the system pasowrd file
C2    and returns ST zero to indicate success.
C
      include  'include/krap.inc'
C
      INTEGER*4 ST
C
C     close the file and keep it
      CLOSE(UNIT=PASUNT,STATUS='KEEP',ERR=701)
C     return clear status
      ST=0
      RETURN
C
 701  CONTINUE
C     get here if cannot close file
      WRITE(*,'(A)') '[PROTECTION] Cannot close password file'
      ST=1
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE K10033(PASCODE,ST)
C     =============================
C1    vartype             C*(*) I  
C1    iostatus              O   O
C
C2    Subroutine K10033 reads the record for node index NI
C2    from the password file, and returns the record in NODREC
C2    ST returned zero if successful.
C
      include  'include/krap.inc'
C
      CHARACTER*(*) PASCODE
      INTEGER*4 ST
C
C     read the data item
      READ(UNIT=PASUNT,FMT='(A)',ERR=702,END=703) PASCODE
C     decode number
C
      ST = 0
C
 702  CONTINUE
C     get here if cannot read file
      ST=1
      RETURN
 703  CONTINUE
C     End of file ( No passwords left )
      ST=2
      RETURN
C
      END
C
C     ------------------------------------------
C
C
      SUBROUTINE K10040(CODE,ST)
C     ==========================
C1    VARYPE             I*4 I4
C1    IOSTAT             O   O
C
C2    Checks password file and gets an access code
C2    This will check date and times and exit immedialtly
C2    if passwords do not match
C2  
C2    Arguments:-
C2  
C2    CODE	->	Acces code see datails in include file for codes
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    Nothing returns from this routine if failure if success then return
C2  
C
      include  'include/krap.inc'
C
      INTEGER NI,NODREC(8),NR(8),I,TDAT(6),YMD,HMS
      INTEGER*4 NODE
      INTEGER*4 TERM
      INTEGER*4 ACCESS
      INTEGER*4 CODE
      INTEGER*4 ST
      CHARACTER*100 PASCODE
C
	  ST = 0
	  CODE = 10
	  RETURN
C     open the control file
      CALL K10001(ST)
      CALL K10030(ST)
      IF (ST.NE.0) THEN
C        flag error opening system file
         GOTO 999
      END IF
C
C     read parameters from control file

1000  CONTINUE
C     go find node record
      CALL K10033(PASCODE,ST)
      IF ( ST.EQ.2) THEN
           WRITE(*,'(A)') '[PROTECTION] Software Not Authorized !!'
           GOTO 999
      ENDIF
      CALL LOCALT(TDAT)
C     encode date into integer
      CALL K10022(TDAT(3),TDAT(2),TDAT(1),YMD)
C     encode time into integer
      CALL K10024(TDAT(4),TDAT(5),TDAT(6),HMS)
C     test for date beyond installation
C
C     Decode string into somthing reasonable
C
      CALL DECODEPASSWD(PASCODE,NODE,TERM,ACCESS)
C
      IF ( NODEID.EQ.NODE) THEN
C
C         test for date before termination
          IF (YMD.GT.TERM) THEN
C            software is time expired
             WRITE(*,'(A)') '[PROTECTION] Licence expired'
             GOTO 999
          END IF
          ST = 0
          CODE = ACCESS
          GOTO 998
      ENDIF
C
C
      GOTO 1000
C
 998  continue
C     close the control file
      CALL K10031(ST)
      IF ( ST.GT.0) THEN
C        flag error during close of control file
         GOTO 999
      END IF
      RETURN
C
 999  CONTINUE
C     return with fault status in ST
      STOP
C
      END
C
C     ------------------------------------------
C
      SUBROUTINE DECODEPASSWD(PASCODE,NODE,TERM,ACCESS)
C     =================================================
C1    VARYPE                    C*(*)   I4  I4    I4
C1    IOSTAT                     I      O   O     O
C
C2    Decode a password string into node number and termination date
C2  
C2  
C2    Arguments:-
C2  
C2    PASCODE	->	The string from the password database
C2    NODEID	->	The system nodeid
C2    TERM	->	Termination date
C2    ACCESS	->	The access level of the software
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include  'include/pwmgr.inc'
      CHARACTER*(*) PASCODE
      INTEGER*4 NODE
      INTEGER*4 TERM
      INTEGER*4 COMP
      INTEGER*4 ACCESS
      INTEGER*4 AMAG
      INTEGER*4 I
      INTEGER*4 P1,P2
      LOGICAL ENCODE
C

      CALL DBOUND(PASCODE,P1,P2,*999)
      READ(PASCODE(P1:P2),FMT='(I10)' ) NODE
      I = P2 + 1
      CALL DBOUND(PASCODE(I:),P1,P2,*999)
      READ(PASCODE(I+P1-1:P2+I-1),FMT='(I10)' ) COMP
      ENCODE = .FALSE.
      CALL K10004(NODE, COMP, ENCODE)
      CALL K10021(COMP,TERM,ACCESS)
      TERM = TERM +YEARZERO*10000
      RETURN
999   CONTINUE
C
      END
C
      SUBROUTINE CHKPRD(PROD, ACCESS, AUTH)
C     =================================================
C1    VARYPE             I*4   I*4     L
C1    IOSTAT              I    I       O
C
C2    Check PROD has been authorised
C2  
C2
C2     This is manic crap for decoding the product code
C2     strip off the highest order bits one by one, but
C2     it's written in FORTRAN and uses big KIRKs auth code
C2     nuff said !!
C2  
C2    Arguments:-
C2  
C2    PROD	->	Product code
C2    AUTH	->      SET true if product is installed
C2    ACCESS    ->      product ACCESS level
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2                                                      
      include  'include/pwmgr.inc'
      INTEGER*4 PROD         
      INTEGER*4 ACCESS        
      INTEGER*4 ACCTMP
      LOGICAL   AUTH
C
C     set product not installed
      AUTH = .true.
	  return
C     copy installed product ACCESS  field
      ACCTMP=ACCESS
C
      IF (ACCTMP .GE. GERCODE) THEN
         IF(PROD .EQ. GERCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-GERCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. PATCODE) THEN
         IF(PROD .EQ. PATCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-PATCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. CVXCODE) THEN
         IF(PROD .EQ. CVXCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-CVXCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. GNCCODE) THEN
         IF(PROD .EQ. GNCCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-GNCCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. PEPCODE) THEN
         IF(PROD .EQ. PEPCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-PEPCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. MIFCODE) THEN
         IF(PROD .EQ. MIFCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-MIFCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. HPGCODE) THEN
         IF(PROD .EQ. HPGCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-HPGCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. INTCODE) THEN
         IF(PROD .EQ. INTCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-INTCODE
         ENDIF

      ENDIF
C
      IF (ACCTMP .GE. IGSCODE) THEN
         IF(PROD .EQ. IGSCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-IGSCODE
         ENDIF
      ENDIF
C
      IF (ACCTMP .GE. DXFCODE) THEN
         IF(PROD .EQ. DXFCODE) THEN
           AUTH = .TRUE.
           RETURN 
         ELSE
           ACCTMP = ACCTMP-DXFCODE
         ENDIF
      ENDIF
C
      END        
