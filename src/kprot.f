C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 kprot.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE DIAG1()
C     SUBROUTINE EXIT()
C
C     |-----------------------------------------------------------------|
C
C
      SUBROUTINE DIAG1(DEMO)
C     =====================
C1    VARYPE            L  
C1    IOSTAT            I   
C
C2    This is the new protection routine called DIAG1
C2    to prevent callage thru INLIBing
C2    However this routine initlises diagnostics
C2    and other things including setting user level control.
C2  
C2    Arguments:-
C2  
C2    DEMO	->	Indicates demonstration code
C2  
C2  
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include  'include/krap.inc'
      include  'include/cross.inc'
      include  'include/swind.inc'
      include  'include/pwmgr.inc' 
C
      INTEGER*4 ST
      INTEGER*4 I,J
      INTEGER*4 NN(4)
      LOGICAL OK
      LOGICAL DEMO, AUTFLG
C
C     set user level at normal (no installed products)
      USELEV = -1
C     set global demo flag
      KDEMO = DEMO
C
      IF ( DEMO ) THEN
           WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] Demo'   
      ELSE
C          go login to system
           CALL K10040(USELEV,ST)
C
           WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] Normal'
C
           CALL CHKPRD(GERCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] Gerber'
           ENDIF
C
           CALL CHKPRD(PATCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] PathTrace'
           ENDIF
C
           CALL CHKPRD(CVXCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] CV-Exec'
           ENDIF
C
           CALL CHKPRD(GNCCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] GNC'
           ENDIF
C
           CALL CHKPRD(PEPCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] PEPS'
           ENDIF                                           
C
           CALL CHKPRD(MIFCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] MIF/GENIO'
           ENDIF                                           
C
           CALL CHKPRD(HPGCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] HPGL'
           ENDIF                                           
C
           CALL CHKPRD(INTCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] Interleaf'
           ENDIF                                           
C
           CALL CHKPRD(IGSCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] IGES'
           ENDIF                                           
C
           CALL CHKPRD(DXFCODE, USELEV, AUTFLG)
           IF (AUTFLG) THEN
		WRITE(*,'(A)') '[SYSTEM MODULE ACCESS] DXF'
           ENDIF                                           
      ENDIF
C
C     open scratch file for entity search operations
      CALL OURSCR(SWINDU,14,OK)
      CALL OURSCR(CRUNIT,12,OK)
C     initialize calculator variable space
      CALL INCALC()
C
      CALL DIAG(I)
C
      END
C
C


