
C        /* SCCS id Keywords             @(#)  412.1 date 6/11/92 journal.f   */



      SUBROUTINE CLSJRN(OK)
C     ===================
C1                      L
C1                      O
C
C2    This routine closes the journal file journal.mac
C
C
      include 'include/journal.inc'
 
      LOGICAL OK
C     so clear the PREHIT flag so that the
C     output buffer containing  JOURNAL OFF
C     will not be output
      PREHIT ='w'
      CLOSE(UNIT=JOURNU,STATUS='KEEP',ERR=99)
      OK = .TRUE.
      JOURON = .FALSE.
      RETURN
C
 99   CONTINUE
      WRITE(UNIT=10,FMT=*)'[CLSJRN] Error closing file '
      OK = .FALSE.
      END
C
C------------------------------------------------------------
C
C
C       @(#)  256.1 date 12/16/89 comjrn.ftn 
      SUBROUTINE COMJRN(OK)
C     =================
C1    vartype           L
C1    iostatus          O
C
C2    This function ouputs a comment inserted by the
C2    user into the journal file. The function JRNCHK
C2    will process the comment menu cell into a comment
C2    when wrtjrn is called after the menu hit. This
C2    function calls DPRMXP which calls CPRMXP which
C2    will get the comment line and then output it
C2    to the journal file
C
      include 'include/journal.inc'
      CHARACTER*80 INPL
      INTEGER NLEN
      LOGICAL OK
      EXTERNAL DPRMXP
C
C     prompt for the comment
      CALL DPRMXP(611, INPL)
C     that's it
      END
C
C---------------------------------------------------------------------
C
 
 
C       @(#)  256.1 date 12/16/89 jrnchk.ftn 
      SUBROUTINE JRNCHK(TEMP2,INPL,TEMP, OK)
C     ==================================
C1    vartype           C*(*) C*(*) C*(*) L
C1    iostatus nil      I     I     O     O
C
C2    The function checks the menu commands that
C2    are to be written to a MACRO file from WRTJRN
C2    it checks  if the menu cell has to go out
C2    as is. or if the menu command has to go out
C2    as a comment e.g. END, INTERSECTION etc
C2    as GETANS will calculate the input point from the
C2    screenhit and the nearest entity, and this value from
C2    GETANS is output so the points mode modifier is unecessary.
C2    Certain calls to WRTJRN will require the output of a
C2    special MACRO function call eg $ZOOMI
C2    INPL will input the special function name
C2    TEMP2 will contain the usual menu CTEXT
C2    and TEMP holds the returned output text
C2    OK returns TRUE if no special processing
C2    has been done and FALSE otherwise
C1
      include 'include/menun.inc'
      include 'include/journal.inc'
      include 'include/vntable.inc'
      include 'include/menpop.inc'
      LOGICAL CVERFY, OK
      EXTERNAL CVERFY
      INTEGER NLEN,LINPL,LTEMP2
      CHARACTER TEMP*(*), TEMP2*(*), INPL*(*)
C     get the lengths of the input strings
      LINPL = NLEN(INPL)
      LTEMP2 = NLEN(TEMP2)                 
      OK = .FALSE.
      IF(MEN .EQ. 4) THEN
C       process the  display menu commands
C       but do nothing if he's hit verb menu
        IF(CCMD .EQ. 'q') THEN
           OK = .TRUE.
           GOTO 99    
        ENDIF
        IF(POPUP) THEN
C          boy has hit a popup
           IF( DISLAY ) THEN
C              boy has hit layer menu popup so do nothing
               GOTO 99
           ELSE
C              boy has hit display menu popup so fix it 
               IF(CVERFY(CCMD,'ZMERPSq')) THEN
C                 well it is a special MACRO function
                  TEMP = INPL(1:LINPL)
               ELSE
C                 right comment it out
                  TEMP = '* '//TEMP2(1:LTEMP2)
               ENDIF   
           ENDIF
        ELSE 
C          genuine menu 4 hit
           IF( .NOT. DISLAY ) THEN
C              boy has hit layer so do nothing
               GOTO 99
           ELSE
C              boy has hit display menu  so fix it 
               IF(CVERFY(CCMD,'ZMERPSq')) THEN
C                 well it is a special MACRO function
                  TEMP = INPL(1:LINPL)
               ELSE
C                 right comment it out
                  TEMP = '* '//TEMP2(1:LTEMP2)
               ENDIF   
           ENDIF
        ENDIF
      ELSEIF( MEN .EQ. 1) THEN
C       comment out the calculator command
        TEMP = '* '//TEMP2(1:LTEMP2)
      ELSEIF(CVERFY(CCMD,'TECINDY') .AND. (MEN .EQ. 3 .OR. MEN .EQ .5)
     +  .AND. PNTMOD) THEN
C       comment out the points mode menu commands
        TEMP = '* '//TEMP2(1:LTEMP2)
      ELSEIF(TEMP2(1:LTEMP2) .EQ. VNOUN(66) .AND. MEN .EQ. 4 .AND.
     + .NOT. DISLAY) THEN
C       comment out the DISPLAY MENU option fom the LAYER menu
        TEMP = '* '//TEMP2(1:LTEMP2)
      ELSEIF(TEMP2(1:LTEMP2) .EQ. VNOUN(444)) THEN
C       comment out the noun COMMENT
        TEMP = '* '//TEMP2(1:LTEMP2)//':- '
      ELSE
C       it is ok to output as is
        GOTO 99
      ENDIF
C           
C     specially processed so return   
      RETURN                  
 99   CONTINUE
C     no special processing necessary
      TEMP = TEMP2
      OK = .TRUE.
      END
 
C       @(#)  256.1 date 12/16/89 opnjrn.ftn 
      SUBROUTINE OPNJRN(OK)
C     ===================
C1                      L
C1                      O
C
C2    This routine opens the journal file journal.mac
C
      include 'include/journal.inc'
      include 'include/vntable.inc'
      include 'include/macro.inc'
C
      CHARACTER*80 FILNAM, TEMP, RUNFMT*84
      INTEGER*4 NLEN,INDEX,NLEN1
      LOGICAL OK
C
      EXTERNAL NLEN, FINDU1, DEPRNT, UNFOLD, SUFFIX,
     +         OPNFFA, CRUNCH, DPRMXP, NLEN1
C                                                    
      INTRINSIC INDEX
      OK = .FALSE.
 66   CONTINUE
C     name the drawing to create
      CALL DPRMXP(602,TEMP)
      IF (INDEX(TEMP,'/').GT.0) THEN
C        User has supplied a path, use it as is.
         FILNAM = TEMP
      ELSE
C        User not has supplied a path, use the default.
         IF (NLEN(PATHN(2)).GT.0) THEN
            FILNAM = PATHN(2)//'/'//TEMP
         ELSE
C           Got no current path, use the current.
            FILNAM = TEMP
         ENDIF
      ENDIF
C     if null file name assume abort required
      IF (NLEN(FILNAM).EQ.0 ) THEN
C        exit with FALSE
         GOTO 233
      END IF
C     make it lower case
C SPB - 031194 - Don't change to LC as NEEB don't like it ...
C      CALL UNFOLD(FILNAM)
C***************************************************
C     note this will prevent people not creating
C     their journals without a .mac suffix
C***************************************************
      CALL SUFFIX(FILNAM,'.mac')
      CALL CRUNCH(FILNAM)
C     right open the file   
      CALL OPNFFA(FILNAM,JOURNU,OK)
      IF(.NOT. OK) GOTO 233
C
C
      REWIND(UNIT=JOURNU)
      OK = .TRUE.
      JOURON = .TRUE.
C
C     punt out the MACRO debug commands
C     as comments to allow their easy
C     insertion into the journal MACRO
C
C     **** DBGON ***
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(JRNCMD(4)),')'
      WRITE(UNIT=JOURNU,FMT=RUNFMT
     +,ERR=200) JRNCMD(4)
C
C     ***** HOLDMAC ****
C
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(JRNCMD(5)),')'
      WRITE(UNIT=JOURNU,FMT=RUNFMT
     +,ERR=200) JRNCMD(5)
C
C     make sure that he starts in the correct place
C     eg from select journal 
C
C     ***** pickzone large ******
C     set the pickzone within the macro the size required
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(JRNCMD(3)),')'
      WRITE(UNIT=JOURNU,FMT=RUNFMT
     +,ERR=200) JRNCMD(3)
C
C     ***** Verb Menu ****
C                    
      TEMP=VNOUN(50)
      CALL FOLDUP(TEMP)
C     supress any leading spaces
      CALL LCSUPP(TEMP,' ')
C     replace any spaces with underscores 
      CALL PATRP(TEMP(1:NLEN(TEMP)),' ','_')
C
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(TEMP),')'
      WRITE(UNIT=JOURNU,FMT=RUNFMT
     +,ERR=200) TEMP
C
C     ***** Select ****
C                   
      TEMP=VNOUN(14)
      CALL FOLDUP(TEMP)
C     supress any leading spaces
      CALL LCSUPP(TEMP,' ')
C     replace any spaces with underscores 
      CALL PATRP(TEMP(1:NLEN(TEMP)),' ','_')
C
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(TEMP),')'
      WRITE(UNIT=JOURNU,FMT=RUNFMT
     +,ERR=200) TEMP
C
C     ***** Journal****
C                   
      TEMP=VNOUN(48)
      CALL FOLDUP(TEMP)
C     supress any leading spaces
      CALL LCSUPP(TEMP,' ')
C     replace any spaces with underscores 
      CALL PATRP(TEMP(1:NLEN(TEMP)),' ','_')
C
      WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +,ERR=200)'(A', NLEN(TEMP),')'
      WRITE(UNIT=BUFFER,FMT=RUNFMT
     +,ERR=200) TEMP
C                   
C
C     set the previous hit token
      PREHIT = 'm'
      PNTMOD = .FALSE.
C     set the points mode flag to FALSE
      RETURN
 200  CONTINUE
C     error writing to journal file
      CALL DEPRNT(603)
      WRITE(UNIT=10,FMT=*)'[OPNJRN] Error writing journal file'
      GOTO 266
 233  CONTINUE
C     journal file not opened
      CALL DEPRNT(604)
 266  CONTINUE
      OK = .FALSE.
      JOURON = .FALSE.
      END
C
C------------------------------------------------------------
C
C       @(#)  256.1 date 12/16/89 wrtjrn.ftn 
      SUBROUTINE WRTJRN(X, Y, LOCATN,INPL,   C )
C     ==========================================
C1                      R  R    C    C*(*)  I*4
C1                      I  I    I    I       I
C
C2    This writes an entry to the journal output
C2    file DAXCAD.JOURNAL it expects the position
C2    of a HIT to be contained in X, and Y.
C2
C2    The token LOCATN  specifies where the hit
C2    has taken place and thus the output action 
C2    that must be taken.
C2
C2    INPL contains a character string that was input
C2    as an answer to a query for data in daxcad if a
C2    typed answer is input otherwise, INPL may also
C2    contain a special journaling command if it is
C2    output from functions like ZOOM.  
C2
C2    C returns the token C to show which mouse button
C2    was used on a graphics hit by TCURS  
C2 
C2    'w' = hit in graphics window
C2    'm' = hit a menu cell
C2    'a' = answer to a prompt
C2    'p' = from a popup
C2
C2    Menu hits are buffered to allow
C2    associated numerical data to be
C2    output on the command line
C
      CHARACTER*(*) INPL
      REAL X, Y
      CHARACTER*1 LOCATN, TEMP*84, TEMP2*84, RUNFMT*84
      INTEGER NLEN, INDEX, INDX
      INTEGER*4 C, I
      LOGICAL CVERFY, OK 
      include 'include/menun.inc'
      include 'include/journal.inc'
      include 'include/vntable.inc'
      include 'include/macro.inc'
      include 'include/menpop.inc'
      EXTERNAL NLEN, CVERFY, PATRP, LCSUPP, JRNCHK
      INTRINSIC INDEX
C                      
C     don't bother writing anything if we are
C     running a macro 
C                     
      IF(MACOP) RETURN
C
      IF(LOCATN .EQ. 'w') THEN
C     *************************************
C     *       screen hit                  *
C     *************************************
C       punt out the buffer containing the last action 
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +  ,ERR=400)'(A', NLEN(BUFFER),')'
        WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) BUFFER
C
C       punt out a comment that it is a graphics hit
C
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +  ,ERR=400)'(A', NLEN(JRNCMD(2)),')'
        WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) JRNCMD(2) 
C       right check to see if the points mode mouse buttons
C       have been used
C
C       if the mouse has been used to flag  a points mode
C       modifier then punt out a modifier 
C
        IF(C .EQ. 155) THEN
C         the CENTRE mouse button has been hit
C         get the text from posn 43 in vntable 
          I = 43
          WRITE(UNIT=RUNFMT,FMT='(A7,I2,A1)'
     +    ,ERR=400)'(''* '',A', NLEN(VNOUN(I)),')'
          WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) VNOUN(I) 
        ELSE IF(C .EQ. 156) THEN
C         the END mouse button has been hit
C         get the text from posn 42 in vntable 
          I = 42
          WRITE(UNIT=RUNFMT,FMT='(A7,I2,A1)'
     +    ,ERR=400)'(''* '',A', NLEN(VNOUN(I)),')'
          WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) VNOUN(I) 
        ENDIF
C    
C       right make up a  GINPUT X(), Y()
        WRITE(UNIT=RUNFMT,FMT='(A6,I2,A32)'
     +  ,ERR=400)'(A', NLEN(JRNCMD(1)),
     1  ', '' X('',F14.6,''), Y('',F14.6,'')'')'
C       stuff it in the buffer
        WRITE(UNIT=BUFFER, FMT=RUNFMT, ERR=400) JRNCMD(1),X,Y         
C       store the fact that the last hit was in graphics       
        PREHIT = 'w'
      ELSEIF(LOCATN .EQ. 'm' .OR. LOCATN .EQ. 'p') THEN
C     *************************************
C     *       menu  hit                   *
C     *************************************
C       punt out the buffer containing the last action 
        WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +  ,ERR=400)'(A', NLEN(BUFFER),')'
        WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) BUFFER
C
C       right check that the menu cell chosen is 
C       executable by a MACRO and if not output
C       the command as a comment
C
        IF(LOCATN .EQ. 'm')THEN
C          normal menu cell
           TEMP2 = CTEXT(1:NLEN(CTEXT))
        ELSE
C          routine has been called from a popup
           TEMP2 = INPL(1:NLEN(INPL))
        ENDIF
C                                    
C       turn display menu, points mode commands 
C       into comments or a special function  if necessary
        CALL JRNCHK(TEMP2,INPL,TEMP, OK) 
C
        IF(OK) THEN
C         there has been no special processing of the command
C         by JRNCHK so lets do it now
          CALL FOLDUP(TEMP)
C         supress any leading spaces
          CALL LCSUPP(TEMP,' ')
C         replace any spaces with underscores 
          CALL PATRP(TEMP(1:NLEN(TEMP)),' ','_')
C
C         extract the menu cell text
C         leaving behind the numeric if there
C         is some present i.e. if there is a colon 
          INDX = INDEX(TEMP,':')
C         store the  menu cell text 
          IF(INDX .GT. 0) TEMP = TEMP(1:INDX-1)     
        ENDIF 
        BUFFER = TEMP(1:NLEN(TEMP))     
C       store the fact that the last hit was a menu cell        
        PREHIT = 'm'
      ELSEIF(LOCATN .EQ. 'a') THEN
C     *************************************
C     *     inputing an ans               *
C     *************************************
        IF(MEN .EQ. 1) THEN
C         he is using the calculator
C         punt out the buffer containing the last action 
          WRITE(UNIT=RUNFMT,FMT='(A2,I2,A1)'
     +    ,ERR=400)'(A', NLEN(BUFFER),')'
          WRITE(UNIT=JOURNU,FMT=RUNFMT,ERR=400) BUFFER         
C         check if it is an assignment or a straight expression
          INDX = INDEX(INPL,'=')
          IF(INDX .GT. 0) THEN
C           it is an assignment so punt it out without quotes
            BUFFER = INPL(1:NLEN(INPL))     
          ELSE
C           it is a straight expression so punt it out prefixed
C           with a print statement
            WRITE(UNIT=RUNFMT,FMT='(A11,I2,A5)'
     +      ,ERR=400)'(''PRINT '',A', NLEN(INPL),','' '')'
            WRITE(UNIT=BUFFER,FMT=RUNFMT,ERR=400) INPL
          END IF  
        ELSE
C         it is an answer to a question so append it
C         to the menu cell that fired off the question and
C         check that it does contain some info ie not zero
C         length string ie punter has hit return
C
C         only do this if the answer is not from a
C         display COMMAND eg ZOOM OUT
C         
          IF(.NOT.(DISLAY .AND. MEN .EQ. 4)) THEN 
            IF(NLEN(INPL) .NE. 0) THEN
               WRITE(UNIT=RUNFMT,FMT='(A7,I2,A5)'
     +         ,ERR=400)'('' "'',A', NLEN(INPL),',''"'')'
               WRITE(UNIT=TEMP,FMT=RUNFMT,ERR=400) INPL
               TEMP2 = BUFFER(1:NLEN(BUFFER)) 
               BUFFER = TEMP2(1:NLEN(TEMP2))//' '//TEMP(1:NLEN(TEMP))
            ELSE
C              string is zero length
               TEMP2 = BUFFER(1:NLEN(BUFFER)) 
               BUFFER = TEMP2(1:NLEN(TEMP2))//' ""'
            ENDIF
          ENDIF
        ENDIF
        PREHIT = 'a'
      ENDIF

      RETURN      
C
 400  CONTINUE
      WRITE(UNIT=10,FMT=*)'[WRTJRN] Error writing file '
      END  
C
C------------------------------------------------------------------
C

