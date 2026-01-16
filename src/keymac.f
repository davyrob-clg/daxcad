C
C     @(#)  412.1 date 6/11/92 keymac.f 
C
C
C     Filename    : keymac.f
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:49:29
C     Last change : 92/06/11 14:33:02
C
C     Copyright : Practical Technology International Limited  
C     File :- keymac.f
C
C     DAXCAD FORTRAN 77 Source file
C
C     Functions and subroutines index:-
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE RUNKEYMAC(KEY,ST)
C     ============================
C1    VARYPE               I4  I4  
C1    IOSTAT               I    O 
C
C2    Loads a file for key macro definitions
C2    This is done for speed.
C2    Each line is a sequence of macros commands
C2  
C2  
C2    Arguments:-
C2  
C2    KEY	->	The key hit
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    0   Success key found macro started
C2    -1  No macro found for key
C2    -2  Could not open macro file
C2    -3  Could not open temp file
C2  
      include 'include/product.inc'
      include 'include/keymac.inc'
      include 'include/macro.inc'
      include 'include/viewport.inc'
      include 'include/vpgrid.inc'
C
      INTEGER*4 ST
      INTEGER*4 KUNT
      INTEGER*4 TUNT
      INTEGER*4 ID
      INTEGER*4 LENGTH
      INTEGER*4 NLEN
C
      CHARACTER*256 KEYFILE
      CHARACTER*256 BUFF
      CHARACTER*256 TMPFIL
      CHARACTER*80 TEMP
      CHARACTER KEY
      CHARACTER CHR
C
      LOGICAL OK
      LOGICAL EX
      LOGICAL TRIP
C
      EXTERNAL NLEN
C
      IF ( .NOT.DRAWNG ) THEN
          CALL EPRINT('Key macros only during drawing editor')
          ST = -1
          RETURN
      ENDIF
      IF ( MACOP ) THEN
          CALL CLSMAC()
      ENDIF
C
      KEYFILE = 'user.keymac'
      CALL UNFOLD(KEY)
      TRIP = .FALSE.
C
      CALL FPARS(KEYFILE,ST)
C
      CALL GETTMPFILE(TMPFIL,LENGTH,ID,ST)
C
      CALL FINDU1(TUNT,ST)
C
      OPEN(UNIT=TUNT,FILE=TMPFIL(1:LENGTH),STATUS='NEW',ERR=997)
C
      CALL FINDU1(KUNT,ST)
C
      OPEN(UNIT=KUNT,FILE=KEYFILE,STATUS='OLD',ERR=999)
C
      REWIND(KUNT)
C
100   CONTINUE
C
      READ(KUNT,FMT='(A)',END=200,ERR=995) BUFF
C
      IF ( NLEN(BUFF) .EQ.0 ) THEN
          GOTO 100
      ENDIF
C
      IF ( BUFF(1:3).EQ.'KEY'.AND..NOT.TRIP) THEN
C
           READ(BUFF(5:),FMT='(A)') CHR
           CALL UNFOLD(CHR)
C
           IF ( CHR.EQ.KEY ) THEN
C
               TRIP = .TRUE.
C
           ENDIF
C
      ELSEIF ( BUFF(1:6).EQ.'ENDKEY'.AND.TRIP ) THEN
C
           GOTO 300
C
      ELSEIF((BUFF(1:1).NE.'*'.OR.BUFF(1:1).NE.'#').AND.TRIP) THEN
C
          WRITE(TUNT,FMT='(A)') BUFF
C
      ENDIF
C
      GOTO 100
C
200   CONTINUE
C
      CLOSE (TUNT,STATUS='DELETE')
      CLOSE (KUNT)
      ST = -1
      RETURN
C
300   CONTINUE
C
      CLOSE (TUNT)
      CLOSE (KUNT)
C
      TEMP = TMPFIL(1:LENGTH)
      CALL MACRUN(TEMP)
C
C     set indicate flag for delete in CLSMAC
C
      KEYMACACTIVE = .TRUE.
      ST = 0
      RETURN
C
995   CALL EPRINT('Error reading key macro file')
      CLOSE (TUNT,STATUS='DELETE')
      CLOSE (KUNT)
      ST = -4
      RETURN
996   CALL EPRINT('Error writing key macro temporary file')
      CLOSE (TUNT,STATUS='DELETE')
      CLOSE (KUNT)
      ST = -5
      RETURN
997   CALL EPRINT('Error opening temporary file')
      ST = -3
      RETURN
999   CALL EPRINT('Error opening key macro file')
      CLOSE (TUNT,STATUS='DELETE')
      ST = -5
      RETURN

      END






