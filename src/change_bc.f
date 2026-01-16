C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 change.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE CHDBUN(OK)
C     SUBROUTINE CHGA00()
C     SUBROUTINE CHGA01()
C     SUBROUTINE CHGA02()
C     SUBROUTINE CHGC00()
C     SUBROUTINE CHGC01()
C     SUBROUTINE CHGC02(SYMBOL,NAME,OLDSTR,NEWSTR,MNHT)
C     SUBROUTINE CHGCEN()
C     SUBROUTINE CHGCHK(PNUM,INLINE,FN,SYMBOL,OK)
C     SUBROUTINE CHGCN1(NEWBDR,NEWROT,NEWCLR,NEWTHK,NEWFNT)
C     SUBROUTINE CHGD00()
C     SUBROUTINE CHGD01()
C     SUBROUTINE CHGD02()
C     SUBROUTINE CHGD03(OK)
C     SUBROUTINE CHGE00()
C     SUBROUTINE CHGE02()
C     SUBROUTINE CHGE03(TMIP)
C     SUBROUTINE CHGE04(TMIP,X,Y,TPDP)
C     SUBROUTINE CHGE10(TMIP,X,Y,TPDP)
C     SUBROUTINE CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
C     SUBROUTINE CHGE40(TMIP)
C     SUBROUTINE CHGF00()
C     SUBROUTINE CHGF01()
C     SUBROUTINE CHGF02(OK)
C     SUBROUTINE CHGP00()
C     SUBROUTINE CHGP03()
C     SUBROUTINE CHGP04()
C     SUBROUTINE CHGP05()
C     SUBROUTINE CHGP06(TDRSHT)
C     SUBROUTINE CHGP07(INDEX,TOKEN1)
C     SUBROUTINE CHGPAP(OK)
C     SUBROUTINE CHGPAR()
C     SUBROUTINE CHGS00(FN)
C     SUBROUTINE CHGS02(FN)
C     SUBROUTINE CHGS03(FN,OK)
C     SUBROUTINE CHGSCL(OK)
C     SUBROUTINE CHGT00()
C     SUBROUTINE CHGT02()
C     SUBROUTINE CHGT03(OK,OPTION,QUIT)
C     SUBROUTINE CHSC00()
C     SUBROUTINE CHUN00()
C     SUBROUTINE MAJCHG()
C     SUBROUTINE MASCHG(SYMBOL,NAME,GLOB,OLDSTR,NEWSTR)
C     SUBROUTINE MNIC00()
C     SUBROUTINE MNICHG()
C     SUBROUTINE MNLCA0()
C     SUBROUTINE MNLCE0()
C     SUBROUTINE MNLCE1()
C     SUBROUTINE MNLCEN()
C     SUBROUTINE MNLCRD()
C     SUBROUTINE MNLCS0()
C     SUBROUTINE MNLCT0()
C     SUBROUTINE SETDTX(I)
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE CHDBUN(OK)
C     ===================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine CHDBUN  gets the change in database
C2    units from the user and replaces previously in
C2    line code that was contained in creatd
C
      include 'include/params.inc'
      include 'include/dimendat.inc'
      include 'include/menun.inc'
      include 'include/gtxt2.inc'
      INTEGER*4 I, MNCODE
      CHARACTER*1 TOKEN
      LOGICAL OK
      EXTERNAL FNDTOK, GTMCWT, MENPOP 
      OK = .FALSE.
      MNCODE = 14
      CALL MENPOP(MNCODE, OK)
      IF(OK) THEN
        DO 60 I=1,8
C         check the token in the unit table
C         to see which unit has been selected
          IF (CCMD .EQ. DBUNTK(I)) THEN
            DBUNIT=DBULST(I)
            DBUFAC=DBFACS(I)
C           set the database flag according to Units.
            IF ( I .LE. 4 ) THEN
C               must be Metric
                METRIC=.TRUE.
            ELSE
C               must be Imperial
                METRIC=.FALSE.
            END IF
C
C           show units selected
C2          U is the token for database units
            CALL GTDMEN(9,3)
C           show units selected
            CALL FNDTOK(9,TOKEN)
            GTMULT = .TRUE.
            CALL GTMCWT(3,TOKEN,DBUNIT)
C           units selection now complete           
            OK = .TRUE.
            GOTO 99
          ENDIF
 60     CONTINUE
      ENDIF 
 99   CONTINUE
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE CHGA00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the CHANGE PROPERTY function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CHGA01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the CHANGE PROPERTY routine
      CALL CHGA01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGA01()
C     ===================
C
      EXTERNAL GTCLRM,CHGA02,MNLCRD
C
C     initialize option menu for property change
      CALL GTCLRM(3)
C     load menu with change property options
      CALL MNLCA0()
C     go do the changes
      CALL CHGA02()
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGA02()
C     ===================
C
C2    Subroutine CHGA02 handles the control of
C2    CHANGE PROPERTY function,which allows
C2    modifications to property data attached to entities
C2    within the database.
C
      include 'include/props.inc'
      include 'include/menun.inc'
      include 'include/nbuff.inc'
C
      REAL X,Y
      INTEGER*4 C,TMEN,TCELL,NP,DTYPE,ACTION
      INTEGER*2 MIPP,ENT,MASPRP,TPRPNT,TPLPNT
      LOGICAL OK,OPTION,QUIT,NONE
      CHARACTER*80 OLIN,ANS*1,PNAME,PDATA
      INTRINSIC CHAR
      EXTERNAL ALSRCH,CRUNCH,UPR001,EXPR01,EXPR02,
     +         DSE800,FINDET,UNFLAG,DCPRN2,FNDTOK,FNDPOS
C
C     enable searching for all entities
      CALL ALSRCH()
C     goto default entry point by asking for
C     name of property to change
      OPTION=.TRUE.
C     load token for "Name" option
      CALL FNDTOK(343,CCMD)
C     ensure "Name" is hilited
      MEN=3
      CALL FNDPOS(343,CELLN)
      CALL GTMCHI(MEN,CELLN)
      GOTO 21
C
 10   CONTINUE
      CALL FINDET(348,X,Y,MIPP,ENT,OPTION,QUIT)
 20   CONTINUE
C     test for quit character
      IF (QUIT) THEN
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
C
 21   CONTINUE
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (OPTION) THEN
C        go check out the options
         CALL ATTP10(PNAME,MASPRP,OK)
         IF ( CCMD.EQ.'q') RETURN
         GOTO 10
      END IF
C
C***************************************************************
C                  PROPERTY EXTRACT ROUTINE                    *
C***************************************************************
C     test for attached properties
      CALL EXPR01(MIPP,NONE,OK)
      IF (NONE) THEN
C        no properties attached
C        "No properties attached"
         CALL DEPRNT(343)
      ELSE
C        property link present
C        ensure it is up to date
         CALL UPR001(MIPP,OK)
C        search for named property
         CALL EXPR02(PNAME,PDATA,DTYPE,TPRPNT,NONE,OK)
         IF (.NOT.OK) THEN
C           named property not attached
C           "No property of this name attached"
            CALL DEPRNT(344)
         ELSE
C           save pointer to property link
            TPLPNT=IMBUFF(11)
C           show property name
            CALL DCPRN2(346,PNAME)
C           show current data value
            CALL DCPRN2(345,PDATA)
C           prompt for new value
            CALL ATTP51(DTYPE,ACTION,OK)
C           write text record,or modify existing
            IF (OK.AND.NONE) THEN
C              no data present add text record
               CALL DWPRC0(TPRPNT,7,PRCBUF(3),PRIBUF(7),OK)
C              modify index record for property
               CALL DMPRI0(TPRPNT,OK)
            ELSE IF (OK) THEN
C              write modified text to file
               CALL DMPRC0(TPRPNT,7,PRCBUF(3),PRIBUF(7),OK)
            END IF
         END IF
      END IF
C
C     clear flag from entity
      CALL UNFLAG(.TRUE.)
      GOTO 10
C
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGC00()
C     ===================
C1    No parameters
C2    
C2    This subroutine sets up menu 3 for the change component or
C2    symbol operation. It then calls CHGC01 to do this very
C2    task.
C
      CALL MNIC00()
C
      CALL CHGC01()
C
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGC01()
C     ===================
C1    No parameters.
C2    Change the path or name of a component or symbol.
C               
      include  'include/masti.inc'
      include  'include/entity.inc'
      include  'include/menun.inc'
      include  'include/nbuff.inc'
      include  'include/ftypes.inc'
C    
      INTEGER*4 TCELL,TMEN,C,FN,NLEN,L
      REAL X1,Y1   
      CHARACTER*80 OLDSTR,NEWSTR
      LOGICAL SYMBOL,OK,MNHT
C
      EXTERNAL GTHFMC,GTMCHI,DCPRNT,DEPRNT,TCURS,NOSRCH,ADSRCH,
     +         GTMCLO
C     
C     By default, select symbols to be changed.
      SYMBOL = .TRUE.     
      TMEN = 3
      CALL GTHFMC(TMEN,'s',TCELL)
      CALL GTMCHI(TMEN,TCELL)
      CALL NOSRCH()
      CALL ADSRCH(SYMBI)
      FN = 2
      DAXTYP = DAXSYM
C            
 10   CONTINUE
C     Get a selection from one of the menus.
      CALL DCPRNT(38)
      CALL TCURS(C,X1,Y1)
C                                    
 20   CONTINUE
      MNHT = .FALSE.
      IF (MEN.EQ.3) THEN 
         IF (CCMD.EQ.'s') THEN
C
C           User wants to change a symbol.
            SYMBOL = .TRUE.     
C           Ensure component cell is Un-highlighted.
            CALL GTHFMC(TMEN,'c',TCELL)
            CALL GTMCLO(TMEN,TCELL) 
C           Ensure symbol cell is Highlighted.
C           {This is only necessary when returning from CHGC02}
            CALL GTHFMC(TMEN,'s',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
C           Set the search keys.
            CALL NOSRCH()
            CALL ADSRCH(SYMBI) 
            FN = 2
            DAXTYP = DAXSYM
         ELSE IF (CCMD.EQ.'c') THEN
C
C           User wants to change a component.
            SYMBOL = .FALSE.     
C           Ensure symbol cell is Un-highlighted.
            CALL GTHFMC(TMEN,'s',TCELL)
            CALL GTMCLO(TMEN,TCELL) 
C           Ensure component cell is Highlighted.
C           {This is only necessary when returning from CHGC02}
            CALL GTHFMC(TMEN,'c',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
C           Set the search keys.
            CALL NOSRCH()
            CALL ADSRCH(COMPI)
            FN = 1
            DAXTYP = DAXCMP
         ELSE IF (CCMD.EQ.'N') THEN
C
C           User wants to change a name.
            TMEN = 3   
C           Ensure path cell is Un-highlighted.
            CALL GTHFMC(TMEN,'A',TCELL)
            CALL GTMCLO(TMEN,TCELL) 
C           Ensure name cell is Highlighted.
C           {This is only necessary when returning from CHGC02}
            CALL GTHFMC(TMEN,'N',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
C           Get the relavent names.
            CALL CHGCHK(578,OLDSTR,FN,SYMBOL,OK)
            IF (OK) THEN
               CALL CHGCHK(579,NEWSTR,FN,SYMBOL,OK)
               IF (OK) THEN
C                 Go do the change.
                  CALL CHGC02(SYMBOL,.TRUE.,OLDSTR,NEWSTR,MNHT)
               ENDIF
            ENDIF
C           if .NOT. OK then name cell may still be highlighted.
            IF (.NOT.OK) then
               CALL GTHFMC(TMEN,'N',TCELL)
               CALL GTMCLO(TMEN,TCELL) 
            ENDIF
         ELSE IF (CCMD.EQ.'A') THEN
C
C           User wants to change a path.
            TMEN = 3   
C           Ensure name cell is Un-highlighted.
            CALL GTHFMC(TMEN,'N',TCELL)
            CALL GTMCLO(TMEN,TCELL) 
C           Ensure name cell is Un-highlighted.
C           {This is only necessary when returning from CHGC02}
            CALL GTHFMC(TMEN,'A',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
C
C           Get the relavent names.
            CALL DPRMXP(580,OLDSTR)
            L = NLEN(OLDSTR)
C           Empty path ?
            IF (L.GT.0) THEN
C              If the path ends with a / ... chop it off.
C              Unless, that is, the path IS '/'.
               IF (OLDSTR(L:L).EQ.'/') THEN
                  IF(L.GT.1) THEN
                     L = L - 1
                     OLDSTR = OLDSTR(:L)
                  ENDIF
               ENDIF
            ENDIF
C
            CALL DPRMXP(581,NEWSTR)
            L = NLEN(NEWSTR)
C           Empty path ?
            IF (L.GT.0) THEN
C              If the path ends with a / ... chop it off.
C              Unless, that is, the path IS '/'.
               IF (NEWSTR(L:L).EQ.'/') THEN
                  IF(L.GT.1) THEN
                     L = L - 1
                     NEWSTR = NEWSTR(:L)
                  ENDIF
               ENDIF
            ENDIF
C
C           Go do the change.
            CALL CHGC02(SYMBOL,.FALSE.,OLDSTR,NEWSTR,MNHT)
         ELSE
            CALL GTMCLO(MEN,CELLN)
            CALL DEPRNT(131)
         ENDIF
      ELSE                                        
C        If noun selected ... go back out and deal with it.
         IF (MEN.NE.0) RETURN
         CALL DEPRNT(117)    
         MNHT = .FALSE.
      ENDIF                                              
C                      
C     If a menu was hit. Go deal with it.
      IF (MNHT) GOTO 20
C     Otherwise go back to the start.
      GOTO 10
C           
      END

C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGC02(SYMBOL,NAME,OLDSTR,NEWSTR,MNHT)
C     =================================================
C1    VARTYPE             L     L    c*(*)  c*(*)  L
C1    IOSTATUS            I     I      I      I    O
C
C2   This routine selects the symbols or components to be changed
C2   and changes them.
C2   NOTE: The search patern is already set by the calling routine.
C                                          
      include  'include/masti.inc'
      include  'include/menun.inc'
      include  'include/nbuff.inc'
      include  'include/swind.inc'
C
      LOGICAL SYMBOL,NAME,MNHT,OPTION,QUIT,OK
      INTEGER*4 C,TMEN,TCELL,PMTNUM
      INTEGER*2 MIPP,ENT
      REAL X1,Y1                               
      CHARACTER*(*) OLDSTR,NEWSTR
      CHARACTER ANS*3
C                
      MNHT = .FALSE.
C
C     Window option. Token is 'W'
      CALL GTDMEN(120,3)
C     Global option. Token is 'G'
      CALL GTDMEN(149,3)
C     Cancel option. Token is 149
      CALL GTDMEN(146,3)
C     Accept. Token is 150
      CALL GTDMEN(358,3)
C                              
 10   CONTINUE
      IF (SYMBOL) THEN
         PMTNUM = 356
      ELSE
         PMTNUM = 582
      ENDIF
CCCCC      CALL TCURS(C,X1,Y1)
      CALL FINDET(PMTNUM,X1,Y1,MIPP,ENT,OPTION,QUIT)
C                          
      IF (MEN .NE. 0) THEN 
         IF (MEN.EQ.3) THEN
            IF (CCMD.EQ.'W') THEN
C              *****   WINDOW   *****
C              use window for selection of entities
               CALL WINDOW(.TRUE.)
               CALL CHGC03(.TRUE.,.TRUE.,MIPP,SYMBOL,NAME,OLDSTR,OK)
            ELSE IF (CCMD.EQ.'G') THEN
C              *****   GLOBAL   *****
               CALL DPRMXP(5,ANS)
               IF (ANS(1:1).EQ.'Y'.OR.ANS(1:1).EQ.'y') THEN
                  CALL MASCHG(SYMBOL,NAME,.TRUE.,OLDSTR,NEWSTR)
                  GOTO 100
               ENDIF
            ELSE IF (CCMD.EQ.CHAR(150)) THEN
C              *****   ACCEPT   *****
               IF (NDATA.GT.0) THEN
C                 go do the change
                  CALL MASCHG(SYMBOL,NAME,.FALSE.,OLDSTR,NEWSTR)
                  GOTO 100
               ELSE
C                 nothing in buffer,tell the idiot
                  CALL DEPRNT(34)
               END IF
            ELSE IF (CCMD.EQ.CHAR(149)) THEN
C              *****   CANCEL   *****
C              clear the last entity flag in buffer
               CALL ZSFLAG(.TRUE.,OK)
               IF (.NOT.OK) CALL DEPRNT(33)
            ELSE IF (CCMD.EQ.' ') THEN
C              The twit hit an empty cell.
               CALL DEPRNT(131)
            ELSE
C              He wants to change a parameter. Go back to do it.
               MNHT = .TRUE.
               GOTO 100
            ENDIF
         ELSE               
            IF (CCMD.EQ.' ') THEN
C              The twit hit an empty cell.
               CALL DEPRNT(131)
            ELSE
C              Changed his mind. He wants to do something else.
               MNHT = .TRUE.
               GOTO 100
            ENDIF
         ENDIF
         CALL GTMCLO(MEN,CELLN)
      ELSE
C        Chosen a single instance. Check that its OK
         CALL CHGC03(.FALSE.,.TRUE.,MIPP,SYMBOL,NAME,OLDSTR,OK)
      ENDIF             
C     Go and get more entities.
      GOTO 10
C
C     Afore ye go ... put the modifier menu back
C                     the way you found it.
 100  CONTINUE         
      CALL UNFLAG(.TRUE.)
      CALL MNIC00()
      TMEN = 3
      IF(SYMBOL) THEN
C           Ensure symbol cell is highlighted.
            CALL GTHFMC(TMEN,'s',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
      ELSE
C           Ensure component cell is highlighted.
            CALL GTHFMC(TMEN,'c',TCELL)
            CALL GTMCHI(TMEN,TCELL) 
      ENDIF
C
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGCEN()
C     ===================
C1    no arguments required
C
C2    This routine controls changing center lines. It allows selection
C2    of center lines to be changed and what changes to make. It then
C2    calls CHGCN1 to make the changes to the center line entities it
C2    has stored in SWINDU.
C
      include 'include/masti.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/nbuff.inc'
      include 'include/inscen.inc'
C
      INTEGER*4 C,PNUM,POPNUM,DUD,CELL
      INTEGER*2 FMIPOS,CLTYPE
      REAL X,Y
      LOGICAL FIRST,OK,NEWBDR,NEWROT,NEWCLR,NEWTHK,NEWFNT
C                       
      FMIPOS=NMIPOS
      POPNUM = 12
      FIRST=.TRUE.
      NEWROT = .FALSE.
      NEWBDR = .FALSE.
      NEWCLR = .FALSE.
      NEWTHK = .FALSE.
      NEWFNT = .FALSE.
      CALL MNLCEN()
      DUD = 1
      PNUM = 35
C     Set search mask for Center lines.
      CALL NOSRCH()
      CALL ADSRCH(CENLIN)
C
 10   CONTINUE
      CALL DCPRNT(PNUM)         
      CALL GETANS(C,X,Y)
      IF (MEN.NE.0) THEN
C        Grief! What does he want to change now ?
         IF (MEN.EQ.3) THEN
            IF (CCMD.EQ.CHAR(13)) THEN
C              'Accept' cell.
               CALL CHGCN1(NEWBDR,NEWROT,NEWCLR,NEWTHK,NEWFNT)
               CALL GTMCLO(3,16)
            ELSE IF (CCMD.EQ.'W') THEN
C              'Window' cell.
               CALL WINDOW(.TRUE.)
               CALL GTMCLO(3,17)
            ELSE IF (CCMD.EQ.'c') THEN
C              Cancel the last flaged entity.
               CALL ZSFLAG(.TRUE.,OK)
               CALL GTMCLO(3,19)
               IF (.NOT.OK) THEN
C                 Nothing is flagged twit.
                  CALL DEPRNT(33)
               ENDIF
            ELSE IF (CCMD.EQ.'b') THEN
C              User hit 'Border' cell, but is he switching the option
C              on or off?
               IF (NEWBDR) THEN
C                 Switch off new border.
                  CALL GTMCLO(3,CELLN)
                  NEWBDR = .FALSE.
               ELSE
C                 User wants to set the border. 
                  CELL = CELLN
                  CALL ICPRMS(CLTYPE,DUD,FMIPOS,POPNUM,FIRST)
                  CALL GTMCHI(3,CELL)
                  NEWBDR = .TRUE.
               ENDIF
            ELSE IF (CCMD.EQ.'r') THEN
C              User hit 'Rotation' cell, but is he switching the option
C              on or off?
               IF (NEWROT) THEN
C                 Switch off new rotation angle..
                  CALL GTMCLO(3,CELLN)
                  NEWROT = .FALSE.
               ELSE
C                 User wants to set the rotation angle.. 
                  CELL = CELLN
                  CALL ICPRMS(CLTYPE,DUD,FMIPOS,POPNUM,FIRST)
                  CALL GTMCHI(3,CELL)
                  NEWROT = .TRUE.
               ENDIF
            ELSE IF (CCMD.EQ.'=') THEN
C              User hit 'THICKNESS' cell, but is he switching the option
C              on or off?
               IF (NEWTHK) THEN
C                 Switch off new rotation angle..
                  CALL GTMCLO(3,CELLN)
                  NEWTHK = .FALSE.
               ELSE
C                 User wants to set the rotation angle.. 
                  CELL = CELLN
                  CALL ICPRMS(CLTYPE,DUD,FMIPOS,POPNUM,FIRST)
                  CALL GTMCHI(3,CELL)
                  NEWTHK = .TRUE.
               ENDIF
            ELSE IF (CCMD.EQ.'k') THEN
C              User hit 'COLOUR' cell, but is he switching the option
C              on or off?
               IF (NEWCLR) THEN
C                 Switch off new rotation angle..
                  CALL GTMCLO(3,CELLN)
                  NEWCLR = .FALSE.
               ELSE
C                 User wants to set the rotation angle.. 
                  CELL = CELLN
                  CALL ICPRMS(CLTYPE,DUD,FMIPOS,POPNUM,FIRST)
                  CALL GTMCHI(3,CELL)
                  NEWCLR = .TRUE.
               ENDIF
            ELSE IF (CCMD.EQ.'f') THEN
C              User hit 'FONT' cell, but is he switching the option
C              on or off?
               IF (NEWFNT) THEN
C                 Switch off new rotation angle..
                  CALL GTMCLO(3,CELLN)
                  NEWFNT = .FALSE.
               ELSE
C                 User wants to set the rotation angle.. 
                  CELL = CELLN
                  CALL ICPRMS(CLTYPE,DUD,FMIPOS,POPNUM,FIRST)
                  CALL GTMCHI(3,CELL)
                  NEWFNT = .TRUE.
               ENDIF
            ELSE 
               CALL DEPRNT(61)
            ENDIF
            GOTO 10
         ENDIF
C        Alright. Go and do something else. See if I care.
         GOTO 999
      ENDIF
C
C     Go find out what he is pointing at. (Center lines only!)
      CALL DSE800(X,Y,OK)
      IF (.NOT.OK) THEN
C        Ain't no center line here twit.
         CALL DEPRNT(142)
         GOTO 10
      ENDIF
C
      GOTO 10
C
 999  CONTINUE
      CALL UNFLAG(.TRUE.)
C
      END
C
      SUBROUTINE CHGCHK(PNUM,INLINE,FN,SYMBOL,OK)
C     ===========================================
C1    VARTYPE            I4   C*(*) I4   L     L
C1    IOSTATUS            I    I    I    I     O
C
C2    This routine checks that the requested copmponent or symbol
C2    exists. (To check for a component FN=1 or for a symbol FN=2)
C              
      INTEGER*4 FN,LEN,NLEN,UN,PNUM
      INTEGER*2 PMIP                      
      CHARACTER*(*) INLINE
      CHARACTER ENDING*4  
      LOGICAL SYMBOL,ASK,OK
C                                      
      EXTERNAL NLEN, SUFFIX, INSC10
C                           
 10   CONTINUE          
      CALL DPRMXP(PNUM,INLINE)
C
C     Make sure the name has the right ending.
      IF (SYMBOL) THEN
         ENDING = '.sym'
      ELSE 
         ENDING = '.cmp'
      ENDIF
      IF (NLEN(INLINE).EQ.0) GOTO 999
      CALL SUFFIX(INLINE,ENDING)
C                          
C     test for existance of required name in the database.
      CALL INSC10(INLINE,FN,PMIP,OK)
      IF (.NOT.OK) THEN
C        does not already exist in the drawing
C        test existance of external component file
C NOTE: If OPENNM dosn't like INLINE, it will ask for a new name.
         CALL OPENNM(INLINE,UN,.TRUE.,OK)
         IF (OK) THEN
            CLOSE(UNIT=UN,STATUS='KEEP')
         ENDIF
      ENDIF   
C
C     File found. Everything is hokey kokey.
      IF (OK) GOTO 100
C
C     User typed enter instead of new name.
      IF (NLEN(INLINE).EQ.0) GOTO 999
C
C     The only thing left is an error.
      CALL DEPRNT(59)
      GOTO 10
C
 999  CONTINUE
C     Failed. Tell him it's not ok.
      OK = .FALSE.                 
C
 100  CONTINUE
C     All done, lets go home.
C
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGCN1(NEWBDR,NEWROT,NEWCLR,NEWTHK,NEWFNT)
C     =====================================================
C1    Vartype             L      L      L      L      L 
C1    Iostatus            I      I      I      I      I  
C
C2    This routine makes the changes required to the centre lines 
C2    referenced in SWINDU.
C
      include 'include/swind.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/movdat.inc'
      include 'include/entity.inc'
      include 'include/style.inc'
      include 'include/inscen.inc'
C
      INTEGER*4 I
      INTEGER*2 MP,TDFP,D1
      REAL BX,BY
      LOGICAL NEWBDR,NEWROT,NEWCLR,NEWTHK,NEWFNT,OK
C
      CALL UNFLAG(.FALSE.)
      IF (NDATA.GT.0) THEN
C        Get each, change it, put it back.
         DO 10 I=1,NDATA
C           read the entity from SWINDU.
            CALL RSCRF(I,MP,BX,BY,TDFP,D1)
            CALL DER500(MP,OK)
            MIP = MP
C           Erase the center line from the screen.
            CALL PENERS()
            CALL DRWCEN(.TRUE.)
            CALL PENDRW()
C
C           Make what changes are required.
            IF (NEWBDR) RDBUFF(6) = BRDRSZ
            IF (NEWROT .AND. (IDBUFF(4).EQ.1)) RDBUFF(5) = ROTANG
            IF (NEWCLR) IMBUFF(3) = COLOUR
            IF (NEWTHK) IMBUFF(12) = THICK
            IF (NEWFNT) IMBUFF(6) = CLINEF
C
C           Write the data back.
            CALL DEM500(MP,OK)
C           Draw the modified center line back to the screen.
            CALL DRWCEN(.TRUE.)
10       CONTINUE
      ELSE
C        There is nothing to change. Call him a berk then sod off.
         CALL DEPRNT(34)
      ENDIF
C
      CALL UNFLAG(.TRUE.)
      END
      SUBROUTINE CHGD00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the CHANGE DIMEN function
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CHGD01
C
      TMEN=MEN
      TCELL=CELLN
C     enter the CHANGE DIMEN routine
      CALL CHGD01()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
      SUBROUTINE CHGD01()
C     ===================
C
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/dimendat.inc'
C
C
      INTEGER*4 I,TMPTOL
      REAL RFDAT(20)
      CHARACTER*1 TOKEN
      EXTERNAL GTCLRM,MNLSD0,CHGD02,MNLCS0,UNFLAG,SAVDDT,RSTDDT
      EXTERNAL CLROPF
C
C     initialize CHANGE DIMEN menu
      CALL MNLSD0()
      CALL MNLCS0()
C     Change text
      CALL GTDMEN(627,3)
C     Change precision
C     save the current dim parameters
      CALL SAVDDT()
C     cancel all change dim flags
      DO 50 I=3,20
         DOPFLG(I)=.FALSE.
 50   CONTINUE
C     cancel all change dim flags
      CALL CLROPF()
C     enter the CHANGE DIMEN routine
      CALL CHGD02()
C     cancel all change dim flags
      CALL CLROPF()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     recover text parameters before leaving
      CALL RSTDDT()
C     clear option menu
      CALL GTCLRM(3)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGD02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine CHGD02 is the main working routine
C2    for the CHANGE DIMEN function.Uses the normal
C2    window search routines for entity selection.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/movdat.inc'
C
      REAL X,Y
C
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL CHGD03,FINDET,ADSRCH,NOSRCH,UNFLAG
C
C     disable searching for all entity types except dimen
      CALL NOSRCH()
      CALL ADSRCH(LDIMN)
      CALL ADSRCH(RDIMN)
      CALL ADSRCH(DDIMN)
      CALL ADSRCH(ADIMN)
      CALL ADSRCH(GLABEL)
C
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(25,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) THEN
C        ensure no selections retained
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL CHGD03(OK)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGD03(OK)
C     =====================
C
C2    Subroutine CHGD03 is the main working routine
C2    for the changing of domension parameters
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/wrkdat.inc'
      include 'include/dimendat.inc'
      include 'include/tdmdat.inc'
      include 'include/tmpdat.inc'
C
      REAL WX1,WY1,REAL
      REAL X1,Y1,X2,Y2,X3,Y3,X4,Y4,X5,Y5,XC,YC,R,SANG,EANG,PDAT(2,4)
     +     ,LDAT1(6),LDAT2(6),M(3,3)
      DOUBLE PRECISION DN
C
      INTEGER*2 MP,NCHARS,NLEN2,TJUST,TMIP,D1,D2,ENT1,ENT2,ENT3
      INTEGER*4 TMEN,TCELL,C,I,NLEN
C
      LOGICAL OK,OPTION,QUIT,CVERFY,CHANGE
C
      CHARACTER*80 TSTRNG,FORM*20,CCBUFF
      CHARACTER TOKEN*1
C
      COMMON /CHGTTT/TSTRNG
C
      INTRINSIC REAL,CHAR,MOD
C
      EXTERNAL CVERFY,GTMCLO,NLEN,AEXPRN,GTMCWR,GTMCHI,GTHFMC,
     +         MNLPTS,WINDOW,MNUPTS,ZSFLAG,RSCRF,DBOX,
     +         DIR500,DERDIM,DEMDIM,DRWDIM,UNFLAG
C
      FORM='(F5.2)'
 25   CONTINUE
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processinig here
 20   CONTINUE
      IF ( CVERFY(CCMD,'GeHhgLaPD') ) THEN
C        He wants to change the parameters
C        test for current selection of changes
C***************************************************************
C                PRECISION CONTROL                             *
C***************************************************************
         IF (CCMD.EQ.'P') THEN
             IF ( OPFLAG(8) ) THEN
                 CALL GTMCLO(TMEN,TCELL)
                 OPFLAG(8) = .FALSE.
                 GOTO 99
             ENDIF
C
119          CALL DPRMXP(111,CCBUFF)
C
             IF (NLEN(CCBUFF).EQ.0 )THEN
C                user has returned zero length string
C                assume that he has change his mind and return for input
                 CALL GTMCLO(MEN,CELLN)
             ELSE
C                evaluate an arithmetic expression from the keyboard
                 CALL AEXPRN(CCBUFF,DN,*119)
C                Check range of precision is valid
                 IF ( INT(DN).GT.6 .OR. INT(DN).LT.0 ) THEN
                     CALL DEPRNT(234)
                     GOTO 119
                 END IF
                 PREC=INT(DN)
C                write precision to menu cell
                 CALL GTDMWI(VNCCMD,3,PREC)
                 RFDAT(7) = PREC
                 OPFLAG(8) = .TRUE.
                 CALL GTMCHI(TMEN,TCELL)
             ENDIF
         ELSEIF (CCMD.EQ.'D') THEN
C***************************************************************
C                 OVERWRITTEN  TEXT  OPTION                    *
C***************************************************************
C
             IF ( OPFLAG(9) ) THEN
C                Must change dimension text
                 CALL GTMCLO(TMEN,TCELL)
                 OPFLAG(9) = .FALSE.
                 DOPFLG(6) = .TRUE.
             ELSE
C                Normal regeneration text
                 DOPFLG(6) = .FALSE.
                 CALL GTMCHI(TMEN,TCELL)
                 OPFLAG(9) = .TRUE.
             ENDIF
C***************************************************************
C                 TEXT HEIGHT  OPTION                          *
C***************************************************************
         ELSEIF (CCMD.EQ.'H') THEN
C           test for opflag status
            IF (OPFLAG(1)) THEN
C              text height
C              this one is selected,so cancel it
               OPFLAG(1)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(1)=.TRUE.
C              ask for new data
 113           CONTINUE
               CALL DPRMXP(26,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 set to  the existing value
                  TDTHGT=DTHGT
                  TDTWDT=DTWDT
                  OPFLAG(1)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*113)
                  IF (DN.NE.0.0) THEN
                     TDTHGT=REAL(DN)
                     TDTWDT=TDTHGT*(DTWDT/DTHGT)
C                    find the text width.
C                    rewrite value.
                     CALL GTDMWR(263,MEN,TDTWDT,FORM)
                     CALL GTDMCH(263,MEN)
C                    text height
                     CALL GTDMWR(VNCCMD,MEN,TDTHGT,FORM)
C                    hightlite it.
                     CALL GTMCHI(TMEN,CELLN)
C                    set the width flag
                     OPFLAG(2)=.TRUE.
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 113
                  END IF
               END IF
            END IF
C***************************************************************
C                   TEXT WIDTH  OPTION                         *
C***************************************************************
         ELSE IF (CCMD.EQ.'h') THEN
C           test for opflag status
            IF (OPFLAG(2)) THEN
C              this one is selected,so cancel it
               OPFLAG(2)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(2)=.TRUE.
 114           CALL DPRMXP(28,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 leave existing value alone
                  TDTWDT=DTWDT
                  OPFLAG(2)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*114)
                  IF (DN.NE.0.0) THEN
                     TDTWDT=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TDTWDT,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 114
                  END IF
               END IF
            END IF
C***************************************************************
C                   TEXT OFFSET  OPTION                        *
C***************************************************************
         ELSE IF (CCMD.EQ.'g') THEN
C           test for opflag status
            IF (OPFLAG(7)) THEN
C              this one is selected,so cancel it
               OPFLAG(7)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(7)=.TRUE.
 118           CALL DPRMXP(312,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 leave existing value alone
                  TDTOFF=DTOFF
                  OPFLAG(7)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*118)
                  IF (DN.NE.0.0) THEN
                     TDTOFF=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TDTOFF,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 118
                  END IF
               END IF
            END IF
C***************************************************************
C                   ARROW LENGTH  OPTION                       *
C***************************************************************
         ELSE IF (CCMD.EQ.'L') THEN
C           arrow length
C           test for opflag status
            IF (OPFLAG(3)) THEN
C              this one is selected,so cancel it
               OPFLAG(3)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(3)=.TRUE.
 116           CALL DPRMXP(29,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 assume that he has change his mind and return
C                 for input
                  TALNG=ALNG
                  OPFLAG(3)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression
C                 from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*116)
                  IF (DN.NE.0.0) THEN
                     TALNG=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TALNG,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 116
                  END IF
               END IF
            END IF
C***************************************************************
C                    ARROW WIDTH  OPTION                       *
C***************************************************************
         ELSE IF (CCMD.EQ.'a') THEN
C           arrow width
C           test for opflag status
            IF (OPFLAG(4)) THEN
C              this one is selected,so cancel it
               OPFLAG(4)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(4)=.TRUE.
 117           CALL DPRMXP(30,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 assume that he has change his mind and return
C                 for input
                  TAWDT=AWDT
                  OPFLAG(4)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression
C                 from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*117)
                  IF (DN.NE.0.0) THEN
                     TAWDT=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TAWDT,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 117
                  END IF
               END IF
            END IF
C***************************************************************
C                     GAPL  OPTION                             *
C***************************************************************
         ELSE IF (CCMD.EQ.'G') THEN
C           gap length
C           test for opflag status
            IF (OPFLAG(5)) THEN
C              this one is selected,so cancel it
               OPFLAG(5)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(5)=.TRUE.
 111           CALL DPRMXP(31,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 assume that he has change his mind and return
C                 for input
                  TGAPL=GAPL
                  OPFLAG(5)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression
C                 from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*111)
                  IF (DN.NE.0.0) THEN
                     TGAPL=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TGAPL,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 111
                  END IF
               END IF
            END IF
C***************************************************************
C                     EXTL  OPTION                             *
C***************************************************************
         ELSE IF (CCMD.EQ.'e') THEN
C           extension length
C           test for opflag status
            IF (OPFLAG(6)) THEN
C              this one is selected,so cancel it
               OPFLAG(6)=.FALSE.
               CALL GTMCLO(TMEN,TCELL)
            ELSE
C              enable this one
               OPFLAG(6)=.TRUE.
 112           CALL DPRMXP(32,CCBUFF)
C
               IF ( NLEN(CCBUFF).EQ.0 )THEN
C                 user has returned zero length string
C                 assume that he has change his mind
C                 and return for input.
                  TEXTL=EXTL
                  OPFLAG(6)=.FALSE.
                  CALL GTMCLO(TMEN,TCELL)
                  RETURN
               ELSE
C                 evaluate an arithmetic expression
C                 from the keyboard
                  CALL AEXPRN(CCBUFF,DN,*112)
                  IF (DN.NE.0.0) THEN
                     TEXTL=REAL(DN)
                     CALL GTDMWR(VNCCMD,MEN,TEXTL,FORM)
C                    leave the cell hilited
                     CALL GTMCHI(TMEN,TCELL)
                  ELSE
C                    zero height not allowed
                     CALL DEPRNT(27)
                     GOTO 112
                  END IF
               END IF
            END IF
         END IF
C**********************************************************
C        end of parameter changes
C**********************************************************
      ELSE IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
         CALL WINDOW(.TRUE.)
C        ensure caller no longer hilited
         CALL GTMCLO(TMEN,TCELL)
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C             RETRACE  OPTION                                  *
C***************************************************************
C        if backspace char,remove last entity list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.TRUE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C        reset return status
         OK=.FALSE.
C        ensure caller no longer hilited
         CALL GTMCLO(TMEN,TCELL)
         RETURN
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
            OK=.TRUE.
            DO 50 I=1,NDATA
C           read the entity from list
            CALL RSCRF(I,TMIP,WX1,WY1,D1,D2)
C           read the master index pointer
            CALL DIR500(TMIP,OK)
C           remove the attention flag
            IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C
C************************************************
C
C            ERROR CONDITION ON DIMENSION CHANGE
C            The following Calls to DEM500 $ ALLDRW
C            cause the dimension data to screw up.
C
C************************************************
C            MIP=TMIP
C            ENT1=IMBUFF(2)
C            CALL DEM500(TMIP,OK)
C            CALL ALLDRW(ENT1,TMIP)
C            IF (I.GE.VNDATA) CALL DBOX(WX1,WY1)
C           read the entity  MI data
            IF (IMBUFF(2).EQ.LDIMN) THEN
C              read the entity data into buffer
               CALL DERDIM(TMIP,OK)
C              erase the dimension from screen
               CALL ERSDIM()
C              decode control data from scratch buffers and set flags.
               CALL DIMF33(X1,Y1,X2,Y2,X3,Y3,X4,Y4)
C              change the required parameters
               CALL CHGPAR()
C              now call dimension generation routine
               CALL DIML03(X1,Y1,X2,Y2,X3,Y3,X4,Y4,.TRUE.,M,OK)
               MIP=TMIP
C              reset status code
               IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C              modify the existing dimension records accordingly
               CALL DEMDIM(TMIP,OK)
C              redraw the dim with new pars
               CALL DRWDIM()
C
            ELSE IF (IMBUFF(2).EQ.RDIMN .OR. IMBUFF(2).EQ.DDIMN
     +                         .OR.  IMBUFF(2).EQ.GLABEL ) THEN
C              read the entity data into buffer
               CALL DERDIM(TMIP,OK)
C              erase the dimension from screen
               CALL ERSDIM()
C              generate control flags and data & supress dim data
C              regeneration like dim text height , arrow length.
               CALL DIMF35(IMBUFF(2),X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG
     +                                                        ,EANG)
C              change the required parameters
               CALL CHGPAR()
C              go generate dimension data
C              set LABEL edit flag true
               CEDIT = .TRUE.
               IF ( IMBUFF(2) .EQ. GLABEL ) THEN
                 CALL DIMN03(X1,Y1,X2,Y2,X3,Y3,OK)
               ELSE
                 CALL DIMR03(X1,Y1,X2,Y2,X3,Y3,XC,YC,R,SANG,EANG,OK)
               END IF
C              set LABEL edit flag false
               CEDIT = .FALSE.
C              reset status code
               IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C              write the modified dim back to database
               MIP=TMIP
C              write the modified dim back to database
               CALL DEMDIM(TMIP,OK)
C              redraw the dim with new pars
                CALL ALLDRW(ENT3, TMIP)
C
            ELSE IF (IMBUFF(2).EQ.ADIMN) THEN
C              read the entity data into buffer
               CALL DERDIM(TMIP,OK)
C              erase the dimension from screen
               CALL ERSDIM()
C              change the required parameters
C              generate control flags and data & supress dim data
C              regeneration like dim text height , arrow length.
               CALL DIMF34(PDAT,LDAT1,LDAT2,X4,Y4)
C              change the required parameters
               CALL CHGPAR()
C              go generate dimension data
               CALL DIMA03(PDAT,LDAT1,LDAT2,X4,Y4)
C              reset status code
               IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C              write the modified dim back to database
               MIP=TMIP
C              write the modified dim back to database
               CALL DEMDIM(TMIP,OK)
C              redraw the dim with new pars
               CALL DRWDIM()
C
            END IF
 50         CONTINUE
            VNDATA=NDATA+1
C           ensure screen flags are cleared before leaving
            CALL UNFLAG(.TRUE.)
C           ensure caller no longer hilited
            CALL GTMCLO(TMEN,TCELL)
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
            CALL GTMCLO(TMEN,TCELL)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
         CALL GTMCLO(TMEN,TCELL)
      END IF
 
 99   CONTINUE
C
      END
 
 
      SUBROUTINE CHGE00()
C     =====================
C
C1    no arguments required
C
C2
C2    controls operation of the CURVE EDITfunction
C
      include 'include/menun.inc'
C
      INTEGER TMEN,TCELL
C
      EXTERNAL GTMCLO,CHGD01
C
      TMEN=MEN
      TCELL=CELLN
C
C     enter the CURVE EDIT routine
      CALL CHGE02()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGE02()
C     ===================
C
 
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/curwin.inc'
      include 'include/menun.inc'
      REAL X,Y,M(3,3),XP,YP
      INTEGER*2 MIPP,ENT,TMIP
      INTEGER*4 C,TCELL,TMEN,DRWTYP
      LOGICAL ACCEPT,CANCEL,QUIT,VERB
      LOGICAL OPTION,CURVE,OK
C
      DRWTYP = 0
C
 20   CONTINUE
C     enable spline searching only
      CALL NOSRCH()
      CALL ADSRCH(SPLINE)
      CALL GTCLRM(3)
 10   CONTINUE
      CALL MNLCE0()
      CALL FINDET(489,X,Y,MIPP,ENT,OPTION,QUIT)
      IF(QUIT) THEN
C          CCMD='F'
          GOTO 200
      ELSE IF(OPTION) THEN
          CALL DEPRNT(34)
          GOTO 10
      ENDIF
 30   CONTINUE
C     propmt for modifier menu
      CALL CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
      IF(ACCEPT) THEN
          CALL ZSFLAG(.FALSE.,OK)
          CALL DRWSPL(MIPP,.FALSE.,M,DRWTYP)
          TMIP=MIPP
      ELSEIF (CANCEL) THEN
          CALL ZSFLAG(.FALSE.,OK)
          CALL DRWSPL(MIPP,.FALSE.,M,DRWTYP)
          GOTO 10
      ELSEIF(QUIT.OR.VERB) THEN
          GOTO 200
      ENDIF
C     set a kid on drawer for drawing purposes
      CALL DIR500(MIPP,OK)
      IMBUFF(5)=IMBUFF(5)+128
      CALL DIM500(MIPP,OK)
C     clear the menu 3 and go to the next secttion ediding
      CALL GTCLRM(3)
      CALL CHGE03(MIPP)
C     we want a new flag
      IF(CCMD.EQ.'N') THEN
          CALL GTCLRM(3)
          GOTO 20
      ENDIF
C     home time
200   CONTINUE
C     Unflag all
      CALL ZSFLAG(.FALSE.,OK)
      END
C
      SUBROUTINE CHGE03(TMIP)
C     ======================
C1    VARTYPE            I2
C1    IOSTAT             I
C
C2    Thia routine is designed to activate the major
C2    curve editing functions
C
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/curwin.inc'
      include 'include/menun.inc'
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C
      REAL X,Y,DIST,TDIST,DISTXY,M(3,3)
      REAL X1,Y1,X2,Y2,XP,YP,DISTS,DISTF,WEIGHT
      INTEGER*4 C,TMEN,TCELL,MODE1,LOGIC,NLEN,DRWTYP,MNCODE,MODCEL
      INTEGER*2 TMIP,ENT,POINT,TPDP1,TPDP2,T,TPDP,D1,MODE
      INTEGER*2 EPDP,SPDP,CPPDP,TYPE
      LOGICAL DELETE,OK,OPTION,CVERFY,START
      LOGICAL QUIT,ACCEPT,VERB,CANCEL,HULL
      CHARACTER*6 FORM,STRING*20,INPL*20
      DOUBLE PRECISION DN
C
      EXTERNAL LOGIC,NLEN
      FORM='(F7.3)'
C
      DRWTYP = 0
C
C     clear menu
C     get the mode of this curve
      CALL DIR500(TMIP,OK)
      CALL DBR500(IMBUFF(7),OK)
      MODE=MOD(IMBUFF(5)+0,4)
      MODCEL = 360+MODE
      TYPE=MOD(IMBUFF(5)+0,128)-MODE
      WEIGHT=RDBUFF(6)
      HULL=.TRUE.
      CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
10    CONTINUE
C     load up with editing options main menu
      CALL MNLCE1()
      CALL GTMCWR(3,'t',WEIGHT,FORM)
C     this will not refresh main menu
15    CONTINUE
C     Display the lines display mode.
      GTMULT = .TRUE.
      CALL GTDMWT(359,3,VNOUN(MODCEL))
C     Bspline or hermite?
      IF(TYPE.EQ.24) THEN
          CALL GTDOMN(269,3)
      ELSEIF(TYPE.EQ.16) THEN
          CALL GTDOMN(280,3)
      ENDIF
C     load up erase refersh cell accordingly
      IF(HULL) THEN
C         erase
          CALL GTDOMN(249,3)
      ELSE
C         rfresh
          CALL GTDOMN(258,3)
      ENDIF
C     tell im what to do
20    CONTINUE
      CALL DCPRNT(38)
      CALL TCURS(C,XP,YP)
      TMEN=MEN
      TCELL=CELLN
      IF(CVERFY(CCMD,'QqN').OR.MEN.EQ.2) THEN
          GOTO 200
      ELSE IF(MEN.NE.3) THEN
          CALL DEPRNT(9)
          GOTO 20
      ENDIF
C     We have a menu 3 hit lets decode it to something useful
C     ****************************************************
C     Insert control point mode
C     ****************************************************
      IF(CCMD.EQ.'I') THEN
 70       CONTINUE
          CALL GTCLRM(3)
          CALL GTDMEN(12,3)
C         Curve insert option
C         prompt for first point
          CALL FINDP0(164,X,Y,OPTION,QUIT)
          IF(QUIT) GOTO 200
C         get closest point on curve
          CALL CHGE04(TMIP,X,Y,TPDP1)
C         draw control points
          X1=RDBUFF(1)
          Y1=RDBUFF(2)
          CALL HFONT(.TRUE.)
          CALL WCROSS(X1,Y1)
          CALL HFONT(.FALSE.)
C         next point
 60       CONTINUE
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(QUIT) GOTO 200
          CALL CHGE04(TMIP,X,Y,TPDP2)
          IF(TPDP1.EQ.TPDP2) THEN
              CALL DEPRNT(625)
              GOTO 60
          ENDIF
          X2=RDBUFF(1)
          Y2=RDBUFF(2)
C         draw it
          CALL HFONT(.TRUE.)
          CALL WCROSS(X2,Y2)
          CALL HFONT(.FALSE.)
C         swap round if nessesary
          CALL DBR500(TPDP1,OK)
          IF(IDBUFF(3).NE.TPDP2) THEN
              CALL DBR500(TPDP2,OK)
C             are the points sequential
              IF(IDBUFF(3).NE.TPDP1) THEN
C                 at this stage  not together
                  CALL DEPRNT(626)
                  CALL DCROSS(X1,Y1)
                  CALL DCROSS(X2,Y2)
                  GOTO 70
               ELSE
C                 the plonker dig'd them back to front never mind
C                 we will allow that
                  T=TPDP2
                  TPDP2=TPDP1
                  TPDP1=T
              ENDIF
          ENDIF
 50       CONTINUE
C         get new point which can be any where
          CALL MNLPTS()
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(OPTION.OR.QUIT) GOTO 200
          CALL DCROSS(X,Y)
          CALL MNUPTS()
C         load up accept cells
 40       CONTINUE
C         tell him waht to do
          CALL CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
          IF(VERB) THEN
C             verb menu
              GOTO 200
          ELSEIF(QUIT) THEN
C             Quit the insert mode
              CALL DCROSS(X1,Y1)
              CALL DCROSS(X2,Y2)
              CALL ECROSS(X,Y)
              GOTO 10
          ELSE IF(CANCEL) THEN
C             he didnt like the new insert point allow him a new one
              CALL GTCLRM(3)
              CALL ECROSS(X,Y)
              GOTO 50
          ENDIF
C         ok point accepted
C         erase the past
          CALL DCROSS(X1,Y1)
          CALL DCROSS(X2,Y2)
          CALL ERSSPL(TMIP,.FALSE.,M)
C         add the new point
C         get new part data pointer
          CALL CHGE10(TMIP,X,Y,TPDP)
          IDBUFF(2)=TMIP
          IDBUFF(3)=TPDP2
          CALL DBM500(TPDP,OK)
C         modify start control point
          CALL DBR500(TPDP1,OK)
          IDBUFF(3)=TPDP
          CALL DBM500(TPDP1,OK)
          CALL DCPRNT(627)
C         readraw the spline and hull
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
C     ****************************************************
C     Add control point mode
C     ****************************************************
      ELSE IF (CCMD.EQ.'A') THEN
C         we nned to find the current start and end part data pointers
          CALL ALLRD(TMIP,ENT,M,DELETE)
C         read past control points
          CPPDP=IDBUFF(3)
          CALL DBR500(CPPDP,OK)
          SPDP=IDBUFF(3)
          CALL DBR500(SPDP,OK)
          X1=RDBUFF(1)
          Y1=RDBUFF(2)
300       CONTINUE
          POINT=IDBUFF(3)
          CALL DBR500(POINT,OK)
          IF(IDBUFF(3).NE.0) THEN
              GOTO 300
          ELSE
              X2=RDBUFF(1)
              Y2=RDBUFF(2)
              EPDP=POINT
          ENDIF
C         get new point which can be any where
  80      CONTINUE
          CALL GTCLRM(3)
          CALL MNLPTS()
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(OPTION.OR.QUIT) GOTO 200
          CALL DCROSS(X,Y)
          CALL MNUPTS()
          DISTS=DISTXY(X1,Y1,X,Y)
          DISTF=DISTXY(X2,Y2,X,Y)
          IF(DISTS.GT.DISTF) THEN
              CALL HFONT(.TRUE.)
              CALL ECROSS(X2,Y2)
              CALL HFONT(.FALSE.)
              START=.FALSE.
          ELSE
              CALL HFONT(.TRUE.)
              CALL ECROSS(X1,Y1)
              CALL HFONT(.FALSE.)
              START=.TRUE.
          ENDIF
C         load up accept cells
          CALL CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
          CALL DCROSS(X1,Y1)
          CALL DCROSS(X2,Y2)
          CALL ECROSS(X,Y)
          IF(VERB) THEN
C             verb menu
              GOTO 200
          ELSEIF(CANCEL) THEN
C             cancle the last point
              GOTO 80
          ELSEIF(QUIT) THEN
C             quit the add mode
              GOTO 10
          ENDIF
C         acceptable lets add
          CALL ERSSPL(TMIP,.FALSE.,M)
C         write in the new value
          CALL CHGE10(TMIP,X,Y,TPDP)
C         modify continuation pointers
          IDBUFF(2)=TMIP
          IF(START) THEN
              IDBUFF(3)=SPDP
              CALL DBM500(TPDP,OK)
              CALL DBR500(CPPDP,OK)
              IDBUFF(3)=TPDP
              CALL DBM500(CPPDP,OK)
          ELSE
              CALL DBR500(EPDP,OK)
              IDBUFF(3)=TPDP
              CALL DBM500(EPDP,OK)
          ENDIF
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
          CALL DCPRNT(628)
C     ****************************************************
C     Delete control point mode
C     ****************************************************
      ELSE IF (CCMD.EQ.'D') THEN
C         delete a point option
 110      CONTINUE
          CALL GTCLRM(3)
          CALL GTDMEN(12,3)
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(QUIT) GOTO 200
          CALL CHGE04(TMIP,X,Y,TPDP)
          CALL DBR500(TPDP,OK)
C         mark the rqd point
          CALL HFONT(.TRUE.)
          CALL WCROSS(RDBUFF(1),RDBUFF(2))
          CALL HFONT(.FALSE.)
C         load up accept cells
          CALL CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
          IF(VERB) THEN
C             verb menu
              GOTO 200
          ELSEIF(QUIT) THEN
C             Quit the insert mode
              GOTO 10
          ELSE IF(CANCEL) THEN
              CALL DCROSS(RDBUFF(1),RDBUFF(2))
C             he didnt like the new insert point allow him a new one
              GOTO 110
          ENDIF
C         ok point accepted
C         erase the past
          CALL ERSSPL(TMIP,.FALSE.,M)
C         get master info
          CALL DIR500(TMIP,OK)
          POINT=IMBUFF(7)
C         search thru the points until penultimate point from deleted
310       CONTINUE
          CALL DBR500(POINT,OK)
          IF(IDBUFF(3).NE.TPDP) THEN
C             carry on
              POINT=IDBUFF(3)
              GOTO 310
          ELSE
C             got it save this points part data number
              TPDP1=POINT
          ENDIF
C         get continuation pointer of deleted point
          CALL DBR500(TPDP,OK)
C         set deleted status
          IDBUFF(4)=-1
          CALL DBM500(TPDP,OK)
C         save it
          POINT=IDBUFF(3)
C         get previous point
          CALL DBR500(TPDP1,OK)
C         set its ne continuaion pointer
          IDBUFF(2)=TMIP
          IDBUFF(3)=POINT
C         do it
          CALL DBM500(TPDP1,OK)
C         redraw curve and hull
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
C     ****************************************************
C     Move a control point mode
C     ****************************************************
      ELSE IF (CCMD.EQ.'M') THEN
C         delete a point option
 140      CONTINUE
          CALL GTCLRM(3)
          CALL GTDMEN(12,3)
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(QUIT) GOTO 200
          CALL CHGE04(TMIP,X,Y,TPDP)
          CALL DBR500(TPDP,OK)
C         mark the one he wants to move
          CALL HFONT(.TRUE.)
          CALL WCROSS(RDBUFF(1),RDBUFF(2))
          CALL HFONT(.FALSE.)
C         get the ne point
          CALL MNLPTS()
          CALL FINDP0(192,X,Y,OPTION,QUIT)
          IF(OPTION.OR.QUIT) GOTO 200
          CALL DCROSS(X,Y,.FALSE.)
          CALL MNUPTS()
          CALL CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
          CALL ECROSS(RDBUFF(1),RDBUFF(2))
          IF(VERB) THEN
C             verb menu
              GOTO 200
C         something useful
          ELSEIF(QUIT) THEN
C             Quit the insert mode
              GOTO 10
          ELSE IF(CANCEL) THEN
C             he dosnt want to move this point
              GOTO 140
          ENDIF
C         Ok lets move it
C         rease the existing
          CALL ERSSPL(TMIP,.FALSE.,M)
C         modify data base
          CALL DBR500(TPDP,OK)
          RDBUFF(1)=X
          RDBUFF(2)=Y
          CALL DBM500(TPDP,OK)
C         readraw
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
      ELSE IF (CCMD.EQ.'R') THEN
          HULL=.TRUE.
          CALL DIR500(TMIP,OK)
          IMBUFF(5)=IMBUFF(5)+128
          CALL DIM500(TMIP,OK)
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
          CALL GTCLRC(MEN,CELLN)
      ELSE IF (CCMD.EQ.'H') THEN
          HULL=.FALSE.
          CALL ERSSPL(TMIP,.FALSE.,M)
          CALL DIR500(TMIP,OK)
          IMBUFF(5)=IMBUFF(5)-128
          CALL DIM500(TMIP,OK)
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
          CALL GTCLRC(MEN,CELLN)
      ELSE IF(CCMD.EQ.'C') THEN
C         Channge type of curve
          CALL DCPRNT(629)
          CALL ERSSPL(TMIP,.FALSE.,M)
          CALL DIR500(TMIP,OK)
          IMBUFF(5)=TYPE+MODE
          CALL DIM500(TMIP,OK)
          CALL DBR500(IMBUFF(7),OK)
          RDBUFF(6)=WEIGHT
          CALL DBM500(IMBUFF(7),OK)
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
          HULL=.FALSE.
      ELSE IF (CCMD.EQ.'m') THEN
C        set MODE of curve to be created
         TCELL = CELLN
         MNCODE = 18
         CALL MENPOP(MNCODE,OK)
         CALL GTMCLO(3,TCELL)
         IF (OK) THEN
C            Update mode.
             MODE = ICHAR(CCMD) - 1
C            Update mode display cell.
             MODCEL = 360+MODE
         ENDIF
      ELSE IF (CCMD.EQ.'b') THEN
          TYPE=16
          CALL GTCLRC(MEN,CELLN)
      ELSE IF (CCMD.EQ.'h') THEN
          TYPE=24
          CALL GTCLRC(MEN,CELLN)
      ELSE IF (CCMD.EQ.'t') THEN
          STRING='Enter thumb weight:'
210       CONTINUE
          CALL CPRMXP(STRING,INPL)
          IF(NLEN(INPL).NE.0) THEN
              CALL AEXPRN(INPL,DN,*210)
              WEIGHT=REAL(DN)
          ENDIF
          CALL GTMCWR(MEN,CCMD,WEIGHT,FORM)
      ENDIF
      CALL GTMCLO(TMEN,TCELL)
      IF(CVERFY(CCMD,'bhmtCHR')) GOTO 15
      GOTO 10
200   CONTINUE
C     is this a verb or quit commnand. A quit command returns to
C     the curve edit menu
      IF(CCMD.EQ.'Q') THEN
          CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
          GOTO 10
      ENDIF
C     regen the max min box
      CALL CHGE40(TMIP)
C     make sure the curve is what it used to be
      CALL ERSSPL(TMIP,.FALSE.,M)
      CALL DIR500(TMIP,OK)
      IF(IMBUFF(5)/128.GT.0) THEN
          IMBUFF(5)=IMBUFF(5)-128
          CALL DIM500(TMIP,OK)
      ENDIF
      CALL DRWSPL(TMIP,.FALSE.,M,DRWTYP)
      END
*
      SUBROUTINE CHGE04(TMIP,X,Y,TPDP)
C     ================================
C1    VARTYPE            I2  R R  I2
C1    IOSTAT             I   I I  O
C
C2    This routine is designed to find the closest control
C2    point to the control point returning its PDP
C
 
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      REAL X,Y,DIST,TDIST,DISTXY,M(3,3)
      INTEGER*2 TMIP,ENT,POINT,TPDP
      LOGICAL DELETE,OK
C
      CALL ALLRD(TMIP,ENT,M,DELETE)
      POINT=IDBUFF(3)
      CALL DBR500(POINT,OK)
      POINT=IDBUFF(3)
      CALL DBR500(POINT,OK)
      DIST=DISTXY(X,Y,RDBUFF(1),RDBUFF(2))
      TPDP=POINT
100   CONTINUE
      POINT=IDBUFF(3)
      CALL DBR500(POINT,OK)
      TDIST=DISTXY(RDBUFF(1),RDBUFF(2),X,Y)
      IF(TDIST.LT.DIST) THEN
          DIST=TDIST
          TPDP=POINT
      ENDIF
      IF(IDBUFF(3).NE.0) GOTO 100
      CALL DBR500(TPDP,OK)
      CALL DCROSS(RDBUFF(1),RDBUFF(2))
      END
*
      SUBROUTINE CHGE10(TMIP,X,Y,TPDP)
C     ================================
C1    VARTYPE            I2  R R  I2
C1    IOSTAT             I   O O  O
C
C2    This routine return the first unused space for a new point
C2    It will write the part data that is currently present
C
 
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      REAL X,Y,DIST,TDIST,DISTXY,M(3,3)
      INTEGER*2 TMIP,ENT,POINT,TPDP,I
      LOGICAL DELETE,OK
C
      CALL ALLRD(TMIP,ENT,M,DELETE)
      POINT=1
100   CONTINUE
      POINT=POINT+1
      CALL DBR500(POINT,OK)
      IF(IDBUFF(4).LT.0.AND.IDBUFF(1).EQ.SPLINE) THEN
C         reset status we will use this one
C         modify continuation pointers later
          IDBUFF(3)=0
          IDBUFF(4)=0
          RDBUFF(1)=X
          RDBUFF(2)=Y
          CALL DBM500(POINT,OK)
          TPDP=POINT
          RETURN
      ENDIF
      IF(POINT.LT.NPDPOS-1) GOTO 100
      RDBUFF(1)=X
      RDBUFF(2)=Y
      CALL DBW500(TPDP,OK)
      END
 
*
      SUBROUTINE CHGE20(ACCEPT,CANCEL,QUIT,VERB,OPTION)
C     =================================================
C1    VARTYPE             L      L     L    L     L
C1    IOSTAT              O      O     O    O     O
C
C     This routine will ask the user for an ansewr to the selected
C2    point. Based on his ansewer will come the three variables
C
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/wrkdat.inc'
      include 'include/entity.inc'
      include 'include/curwin.inc'
      include 'include/menun.inc'
 
      REAL X,Y,C
      INTEGER*4 TMEN,TCELL
      LOGICAL CANCEL,ACCEPT,QUIT,VERB,OPTION
 
      VERB=.FALSE.
      CANCEL=.FALSE.
      ACCEPT=.FALSE.
      QUIT=.FALSE.
C     load up the required cells
      CALL MNLCE0()
 10   CONTINUE
C     ask 'im what he wants to do
      CALL DCPRNT(38)
C     get the answer
      CALL TCURS(C,X,Y)
C     check for verb menu
      IF(CCMD.EQ.'q'.OR.MEN.EQ.2) THEN
          VERB=.TRUE.
C     check for something in the work are bot allow
      ELSEIF(MEN.NE.3) THEN
          CALL DEPRNT(9)
          GOTO 10
      ELSEIF (MEN.EQ.3) THEN
C         ok we have a valid hit lets set accordingly
          IF(CCMD.EQ.'c') THEN
              CANCEL=.TRUE.
          ELSEIF(CCMD.EQ.CHAR(13)) THEN
              ACCEPT=.TRUE.
          ELSEIF(CCMD.EQ.'Q') THEN
              QUIT=.TRUE.
          ENDIF
      ENDIF
C     low the cell and come back
      CALL GTMCLO(MEN,CELLN)
      END
C
      SUBROUTINE CHGE40(TMIP)
C     =======================
C1    vartype            I2
C1    iostat             I
C
C
C2    General purpose routine for regenerating the box limits
C2    for a spline or hatch entity. Wll update the data base.
 
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      INTEGER*2 TMIP,ENT,POINT,TPDP,I,P2
      INTEGER*4 NC
      LOGICAL DELETE,OK
      REAL MMAX(6)
 
C     get entity type
      CALL DER500(TMIP,OK)
      if(IMBUFF(2).EQ.SPLINE) THEN
          NC=1
          POINT=IDBUFF(3)
          CALL DBR500(POINT,OK)
C         set start of part data pointer
          P2=IDBUFF(3)
          CALL DBMMAX(P2,NC,MMAX,OK)
C         make the box 20% bigger all round
          MMAX(1)=MMAX(1)-ABS(0.1*(MMAX(1)-MMAX(4)))
          MMAX(2)=MMAX(2)-ABS(0.1*(MMAX(2)-MMAX(5)))
          MMAX(3)=MMAX(3)-ABS(0.1*(MMAX(3)-MMAX(6)))
          MMAX(4)=MMAX(4)+ABS(0.1*(MMAX(1)-MMAX(4)))
          MMAX(5)=MMAX(5)+ABS(0.1*(MMAX(2)-MMAX(5)))
          MMAX(6)=MMAX(6)+ABS(0.1*(MMAX(3)-MMAX(6)))
C         get the buffers
          CALL DBR500(POINT,OK)
C          copy buffers
          DO 10 I=1,6
              RDBUFF(I)=MMAX(I)
10        CONTINUE
C         modify box
          CALL DBM500(POINT,OK)
      ELSE IF(IMBUFF(2).EQ.HATCH) THEN
          POINT=IMBUFF(7)
          NC=2
          P2=IDBUFF(3)
          CALL DBMMAX(P2,NC,MMAX,OK)
C         update the buffers
          CALL DBR500(POINT,OK)
          DO 20 I=1,6
              RDBUFF(I)=MMAX(I)
20        CONTINUE
          CALL DBM500(POINT,OK)
      ENDIF
      END
 
 
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGF00()
C     ===================
C
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/gtxt2.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 TMEN,TCELL,I,MODE
      INTRINSIC REAL
C                
      EXTERNAL GTDMWT
C
      TMEN=MEN
      TCELL=CELLN
C
      MODE = SMODE
C     Display the lines display mode.
      GTMULT = .TRUE.
      CALL GTDMWT(359,3,VNOUN(369))
      IF(STYPE.EQ.24) THEN
          CALL GTDMEN(269,3)
      ELSEIF(STYPE.EQ.16) THEN
          CALL GTDMEN(280,3)
      ENDIF
C
C     and accept
      CALL GTDMEN(358,3)
C     window
      CALL GTDMEN(210,3)
 
      CALL CHGF01()
 
      SMODE=MODE
 
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGF01()
C     ===================
C
C1    no arguments required
C
C2
C2
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/movdat.inc'
C
      REAL X,Y
C
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL CHGT03,ADSRCH,NOSRCH,FINDET,UNFLAG
C
C     disable searching for all entity types except text
      CALL NOSRCH()
      CALL ADSRCH(SPLINE)
C
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(444,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) THEN
C        ensure no selections retained
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
 20   CONTINUE
C
      IF (MEN.EQ.3) THEN
         CALL CHGF02(OK)
         TMEN=MEN
         TCELL=CELLN
         CALL GTMCLO(TMEN,TCELL)
         IF (OK) GOTO 10
         GOTO 20
      END IF
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGF02(OK)
C     =====================
C1                       L
C1                       I
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/swind.inc'
      include 'include/viewport.inc'
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
C 
      LOGICAL OK,DELETE
      INTEGER*2 MP,D1,D2,TENT
      INTEGER*4 I,TCELL,MNCODE
      REAL WX1,WY1,M(3,3)
 
      OK=.TRUE.
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
      ELSE IF ( CCMD .EQ. 'm' ) THEN
C***************************************************************
C                     MODE    OPTION                           *
C***************************************************************
C        set MODE of curve to be created
         TCELL = CELLN
         MNCODE = 18
         CALL MENPOP(MNCODE,OK)
         CALL GTMCLO(3,TCELL)
         IF (OK) THEN
C            Update mode.
             SMODE = ICHAR(CCMD) - 1
C            Update mode display cell.
             GTMULT = .TRUE.
             CALL GTDMWT(359,3,VNOUN(360+SMODE))
         ELSE
             OK = .TRUE.
             GOTO 99                  
         ENDIF
      ELSEIF( CCMD.EQ.'h') THEN
         CALL GTDOMN(269,3)
         STYPE=24
      ELSEIF( CCMD.EQ.'b') THEN
         CALL GTDOMN(280,3)
         STYPE=16
      ELSE IF (CCMD.EQ.CHAR(150)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
            OK=.TRUE.
            DO 50 I=1,NDATA
C           read the entity from list
            CALL RSCRF(I,MP,WX1,WY1,D1,D2)
C           remove the attention flag
C           read the entity data
C           read the entity data into buffers
            CALL ALLRD(MP,TENT,M,DELETE)
C            CALL ALLDRW(TENT,MP)
C            IF (I.GE.VNDATA) THEN
C               CALL DBOX(WX1,WY1)
C            END IF
C
C           erase the entity from screen
               VPMOV = .TRUE.
               CALL PENERS()
               CALL SPCDRW(TENT,MP,.FALSE.,M,.FALSE.)
               CALL PENDRW()
               VPMOV = .FALSE.
C              Store the type of spline we have
C      WRITE(10,*) 'Spline mode',IMBUFF(5),SMODE
               IMBUFF(5)=STYPE+SMODE
C              reset status code
               IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C              write data back to MI
               MIP=MP
               CALL DIM500(MP,OK)
C              redraw the entity on screen
               VPMOV = .TRUE.
               CALL SPCDRW(TENT,MP,.FALSE.,M,.FALSE.)
               VPMOV = .FALSE.
C
 50         CONTINUE
            VNDATA=NDATA+1
C           ensure scratch file is cleared befor leaving
            CALL ZRFLAG()
C           ensure pen reset to draw colour
            CALL PENDRW()
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
      END IF
 99   CONTINUE 
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP00()
C     ===================
C
      include 'include/menun.inc'
 
      INTEGER TMEN,TCELL
      EXTERNAL GTCLRM,CHGP04,CHGP03,MNLCRD
C
 
      TMEN=MEN
      TCELL=CELLN
C     initialize option menu for paper change
      CALL GTCLRM(3)
C     load menu with change options
      CALL MNLCRD()
C     go do the changes
      CALL CHGP03()
C     ensure option is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP03()
C     ===================
C
C2    Subroutine CHGP03 handles the operation
C2    of options for change paper function
C
      include  'include/menun.inc'
      include  'include/swind.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
      include  'include/wtov.inc'
      include  'include/viewport.inc'
C
      REAL X,Y,TX,TY
      DOUBLE PRECISION DN1,DN2
      INTEGER*4 C,P,TMEN,TCELL,I,MOD
      LOGICAL OK,OPTION,QUIT
      CHARACTER TEMP*80,ANS,TDRSHT*3,TOKEN*1
      INTRINSIC CHAR,REAL,INDEX,MOD
      EXTERNAL RSWAP,CPRINT,TCURS,GTMCLO,FNDTOK,GTMCWT
      EXTERNAL FINDP0,PEN,DISFRA,CHGP05,CHGP06,CHUN00,CHGPAP,CHSC00
C
C     Let him hit something, and find out what he wants to do
 600  CONTINUE
      CALL DCPRNT(38)
C     go get cursor hit
      CALL TCURS(C,X,Y)
C
 601  CONTINUE
C     The returned command character from the main menu
C     directs us to the next option.
C     if exit ptcad then stop here
      IF (MEN.EQ.2) RETURN
      IF(MEN.EQ.0) THEN
C        hit in graphics area not allowed
         CALL DEPRNT(9)
         GOTO 600
      END IF
C
      TMEN=MEN
      TCELL=CELLN
C
      IF (CCMD.EQ.'P') THEN
C        save the current drawsheet descriptor
         TDRSHT=DRWSHT
C        set the paper size for the drawing
         CALL CHGPAP(OK)
C        show new paper in position
         IF(OK) THEN
           CALL CHGP06(TDRSHT)
         ENDIF
         CALL GTMCLO(TMEN,TCELL)
C
      ELSE IF (CCMD.EQ.'R') THEN
C        save the current drawsheet descriptor
         TDRSHT=DRWSHT
C        rotate the paper for the drawing
C        test for rotated sheet first
         CALL RSWAP(DRWSIZ(1),DRWSIZ(2))
         IF (DRWSHT(3:3).EQ.'R') THEN
C           sheet is already rotated
C           reset drawing sheet descriptor
            DRWSHT(3:3)=' '
C           ensure cell no longer hilited
            CALL GTMCLO(MEN,CELLN)
         ELSE
C           sheet positioned normally
C           must rotate it now
C           set sheet descriptor accordingly
            DRWSHT(3:3)='R'
         END IF
C        paper rotation now complete
C        show new position of paper
         CALL CHGP06(TDRSHT)
C
      ELSE IF (CCMD.EQ.'O') THEN
C        move paper to new position
         CALL MNLPTS()
         CALL FINDP0(13,X,Y,OPTION,QUIT)
         CALL MNUPTS()
         CALL GTMCLO(TMEN,TCELL)
         IF (QUIT) THEN
            RETURN
         ELSE IF (OPTION) THEN
            GOTO 601
         END IF
C        must have valid position
C        erase paper from current position
         VPADD = .TRUE.
         CALL PENERS()
         CALL DISFRA()
C        set bottom left of paper at new position
         WPORGX=X+DRWSIZ(1)*PAPTOW/2
         WPORGY=Y+DRWSIZ(2)*PAPTOW/2
C        set new paper limits
         CALL CHGP05()
C        set pen to white again
         CALL PENDRW()
C        display frame in new position
         CALL DISFRA()
         VPADD = .FALSE.
C        switch cell off
         CALL GTMCLO(TMEN,TCELL)
C 
      ELSE IF (CCMD.EQ.'S') THEN
C        change the scale
         CALL CHSC00()
      ELSE IF (CCMD.EQ.'U') THEN
C        change the units
         CALL CHUN00()
         CALL GTMCLO(TMEN,TCELL)
      ELSE IF (CCMD.EQ.'Q' .OR.CCMD.EQ.'q') THEN
C        return to caller with quit status
         RETURN
      ELSE
C        function not available at the moment
         CALL DEPRNT(8)
         CALL GTMCLO(TMEN,TCELL)
      END IF
C     go try for another option
      GOTO 600
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP04()
C     ===================
C
C2    Subroutine CHGP04 ensures correct display
C2    of options for change paper function the
C2    function explicitly checks for the paper
C2    size and assigns to default if that size
C2    is not found e.g. it was specified in an 
C2    unavailable paper.pcf file.
C
      include 'include/params.inc'
C
      INTEGER*4 TMEN,TCELL,I,K
C
      CHARACTER*16 TEMP,TOKEN*1
C
      EXTERNAL GTHFMC,GTMCWT,GTMCHI, GTDOMN
C
C     need to show correct conditions
      TMEN=3
C     set the paper size for the drawing
      I=0
      K=0
 50   I=MOD(I,5)+1
C     find the pointer to current sheet size
      K=K+1
      IF (PAPLST(I).NE.DRWSHT(:2).AND.K.LT.5) GOTO 50
C     if k>5 then cant find papaer size in current list
C     so just set to the first within the current list.
      IF (K.GT.5) I=1
      DRWSHT(:2)=PAPLST(I)
      DRWSIZ(1)=PAPSIZ(1,I)
      DRWSIZ(2)=PAPSIZ(2,I)
c     check for rotated paper
      IF(DRWSHT(3:3) .EQ. 'R' ) CALL RSWAP(DRWSIZ(1),DRWSIZ(2))
C
C     hilite rotate paper if required
      IF (DRWSHT(3:3).EQ.'R') THEN
C        hilite the rotate cell
         CALL GTHFMC(TMEN,'R',TCELL)
         CALL GTMCHI(TMEN,TCELL)
      END IF
C     show initial conditions
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP05()
C     ===================
C
C1    no arguments required
C
C2    Subroutine CHGP05 is a utility used to
C2    update the paper to world scale ratio
C2    and to set the limits of the paper mapping
C2    on the world space.
C
      include 'include/ndata.inc'
      include 'include/params.inc'
      include 'include/wtov.inc'
C
CSUN
CC     A bit of a hack to cope with the fact that when you load an
CC     OS map as a raster backcloth the origin changes to 0 0.
CC     Fix the centre in the centre of the new paper- GCU.
CC
      INTEGER NSECTS
      REAL OXMIN,OYMIN,OXMAX,OYMAX,DX,DY
 
      CALL NUMSEC(NSECTS)
      IF (NSECTS.GT.0) THEN
CC     save currect
        OXMIN=WPXMIN
        OYMIN=WPYMIN
        OXMAX=WPXMAX
        OYMAX=WPYMAX
      END IF
CC      WRITE(*,*) '[CHGP05] Old:',WPXMIN,WPYMIN,WPXMAX,WPYMAX
CSUN
C
      PAPTOW=1/DRWSCL*PAPFAC/DBUFAC
C
C     set paper limits on the world space
      WPXMIN=-DRWSIZ(1)*PAPTOW/2+WPORGX
      WPYMIN=-DRWSIZ(2)*PAPTOW/2+WPORGY
      WPXMAX= DRWSIZ(1)*PAPTOW/2+WPORGX
      WPYMAX= DRWSIZ(2)*PAPTOW/2+WPORGY
C
CSUN
      IF (NSECTS.GT.0) THEN
CC     adjust it so the centre remains in the centre of the paper
CC        WRITE(*,*) '[CHGP05] Unadjusted:',WPXMIN,WPYMIN,WPXMAX,WPYMAX
        DX=((WPXMAX-WPXMIN) - (OXMAX-OXMIN))/2
        DY=((WPYMAX-WPYMIN) - (OYMAX-OYMIN))/2
CC        WRITE(*,*) '[CHGP05] DX,DY=',DX,DY
        WPXMAX= WPXMAX - WPXMIN - DX + OXMIN
        WPYMAX= WPYMAX - WPYMIN - DY + OYMIN
        WPXMIN= -DX + OXMIN
        WPYMIN= -DY + OYMIN
      END IF
CC      WRITE(*,*) '[CHGP05] New:',WPXMIN,WPYMIN,WPXMAX,WPYMAX
CSUN
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP06(TDRSHT)
C     =========================
C
C1    no arguments required
C
C2    Subroutine CHGP06 is a utility used to
C2    erase the paper from the screen,and redisplay
C2    a new paper mapping on the world space.
C
      include 'include/params.inc'
      include 'include/wtov.inc'
      include 'include/viewport.inc'
C
      CHARACTER*3 TEMP,TDRSHT
C
      EXTERNAL PEN,DISFRA,CHGP05
C
C        first erase old paper
C        save new sheet descriptor
         VPADD = .TRUE.
         TEMP=DRWSHT
C        set old sheet descriptor
         DRWSHT=TDRSHT
         CALL PENERS()
         CALL DISFRA()
C        recover new sheet descriptor
         DRWSHT=TEMP
C        change paper parameters
         CALL CHGP05()
         CALL PENDRW()
C        show new frame
         CALL DISFRA()
         VPADD = .FALSE.
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGP07(INDEX,TOKEN1)
C     ===================
C
C1    vartype            I4     C*1
C1    iostatus           O       I
C
C2    gets the index I to identify the paper
C2    size from the list of paper sizes
C2    in PAPLST and PAPSIZ. from TOKEN1
C
      include 'include/params.inc'
C
C
      INTEGER*4 INDEX, VNUM
      CHARACTER*1 TOKEN1, TOKEN2
      DO 60 INDEX=1,5
C        get the verb number associated with that paper size
         VNUM = PAPVNO(INDEX)
C        get the token associated with that paper option dict No
         CALL GTVNTK(VNUM, TOKEN2)
C        exit when the index I is set to the correct paper size
         IF(TOKEN1 .EQ. TOKEN2) GOTO 99
 60   CONTINUE
C
C     failed to get a valid token so set the 
C     paper number to the default selection
C
      INDEX = 1
 99   CONTINUE
C
C      
      END
C
C     ----------------------------------------------------
C      
      SUBROUTINE CHGPAP(OK)
C     ===================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine CHGPAP  gets the change in paper
C2    size from the user and replaces previously in
C2    line code that was contained in creatd
C
      include 'include/params.inc'
      include 'include/dimendat.inc'
      include 'include/menun.inc'
      include 'include/gtxt2.inc'  
      include 'include/pendat.inc'
      INTEGER*4 I, MNCODE, TMEN, TCELL
      LOGICAL OK
      CHARACTER TOKEN*1
      EXTERNAL  MENPOP, SETDTX, GTDMEN, SHOPAP, CHGP07,
     +          GTDOMN, GTDMWT
      OK = .FALSE.
      MNCODE = 17
      TMEN = MEN
      TCELL = CELLN
      CALL MENPOP(MNCODE, OK)
      IF(OK) THEN
C       get the paper size
        CALL CHGP07(I, CCMD)
C       save the sheet descriptor
        DRWSHT(:2)=PAPLST(I)
C       set Dimension text according to sheet size.
        CALL SETDTX(I)
C       save the sheet size
C       test for rotated sheet first
        IF (DRWSHT(3:3).EQ.'R') THEN
C           sheet must be rotated
            DRWSIZ(2)=PAPSIZ(1,I)
            DRWSIZ(1)=PAPSIZ(2,I)
        ELSE
C           sheet positioned normally
            DRWSIZ(1)=PAPSIZ(1,I)
            DRWSIZ(2)=PAPSIZ(2,I)
        END IF
C
C        set the paper size for the drawing
C        show current selection in cell
         GTMULT = .TRUE.
         CALL GTDMWT(8,3,DRWSHT(:2))
C        record the name of the selected paper
         CURPNO = I
C        write out the paper name to the cell below.
         TOKEN = ' '
         TCELL = TCELL + 1
         CALL GTCLRC(TMEN,TCELL)
         CALL GTPMEN(PAPNAM(I),TOKEN,TMEN,TCELL)
         OK = .TRUE.
      ENDIF
C
      END
C
C----------------------------------------------------------------
C
      SUBROUTINE CHGPAR()
C     =====================
C
C2    Subroutine CHGPAR is the  routine
C2    for the resetting  of dimension parameters
C
      include 'include/movdat.inc'
      include 'include/ndata.inc'
      include 'include/dimendat.inc'
      include 'include/tdmdat.inc'
      include 'include/tmpdat.inc'
C
C     change the required parameters
 
C     set new height
      IF (OPFLAG(1))  DTHGT=TDTHGT
C     set new width
      IF (OPFLAG(2))  DTWDT=TDTWDT
C     change arrow length
      IF (OPFLAG(3))   ALNG=TALNG
C     change arrow width
      IF (OPFLAG(4))   AWDT=TAWDT
C     change gaplength
      IF (OPFLAG(5))   GAPL=TGAPL
C     change extension length
      IF (OPFLAG(6))   EXTL=TEXTL
C     change text gap
      IF (OPFLAG(7))  DTOFF=TDTOFF
C
      IF (OPFLAG(8))  PREC=RFDAT(7)
C
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE CHGS00(FN)
C     =====================
C
C1    vartype           I4
C1    iostatus           I
C
      include 'include/menun.inc'
      include 'include/masti.inc'
      include  'include/daxcolor.inc'
      include  'include/lfont.inc'
      include  'include/style.inc'
      include  'include/vntable.inc'
      include  'include/gtxt2.inc'
C
      INTEGER*4 C
      CHARACTER*1 TOK,NAME*20,CLRNAM*16
      INTEGER TMEN,TCELL,FN
      INTEGER*4 POPMEN
      LOGICAL OK
      EXTERNAL GTMCLO,GTCLRM,MNLCS0,CHGS02,MNICOL,UNFLAG,
     +         GTDMWT
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize CHANGE STYLE menu
      CALL MNLCS0()
C
C     enter the style option
C      WRITE(10,*) 'CHGS00',FN
      GOTO (10,20,30) FN
 10   CONTINUE
C        show the font number in the cell
         GTMULT = .TRUE.
         CALL GTDMWT(407,3,TXFONT)
         GOTO 100
 20   CONTINUE
C        load colour control cell
         GTMULT = .TRUE.
         CALL GTDMWT(399,3,TXCLR)
         GOTO 100
 30   CONTINUE
C        thickness lines cell
         GTMULT = .TRUE.
         CALL GTDMWT(409,3,TXTHK)
 100  CONTINUE
C
      CALL CHGS02(FN)
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGS02(FN)
C     =====================
C
C1    vartype           I4
C1    iostatus           I
C
C2    Subroutine CHGS02 is the main working routine
C2    for the CHANGE STYLE function.Uses the normal
C2    window search routines for entity selection.
C2    FN passes the function number describing the type of change.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
C
      REAL X,Y
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I,FN
      LOGICAL OK,OPTION,QUIT
      EXTERNAL CHGS03,FINDL0,FINDA0,ADSRCH,NOSRCH,UNFLAG
C
C     disable searching for all entity types
      CALL NOSRCH()
C
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
      IF (FN.EQ.1) THEN
C        change of font required
C        only possible on lines, centre lines and arcs
         CALL ADSRCH(LINE)
         CALL ADSRCH(CENLIN)
         CALL ADSRCH(ARC)
C        find and flag an entity at hit point
         CALL FINDET(35,X,Y,MIPP,ENT,OPTION,QUIT)
C
      ELSE IF (FN.EQ.2.OR.FN.EQ.3) THEN
C        change of colour required
C        enable searching for all entities
C        except for COMPS and SYMBOLS
         CALL ADSRCH(LINE)
         CALL ADSRCH(ARC)
         CALL ADSRCH(SPLINE)
         CALL ADSRCH(TEXT)
         CALL ADSRCH(CENLIN)
         CALL ADSRCH(LDIMN)
         CALL ADSRCH(RDIMN)
         CALL ADSRCH(DDIMN)
         CALL ADSRCH(ADIMN)
         CALL ADSRCH(HATCH)
         CALL ADSRCH(GLABEL)
         CALL ADSRCH(TBLOCK)
C        find and flag an entity at hit point
         CALL FINDET(35,X,Y,MIPP,ENT,OPTION,QUIT)
      ELSE
         CALL DEPRNT(8)
         RETURN
      END IF
      IF (QUIT) THEN
C        ensure no selections retained
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
      IF (MEN.EQ.3) THEN
         CALL CHGS03(FN,OK)
         GOTO 10
      END IF
C
 99   CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGS03(FN,OK)
C     ========================
C
C1    vartype            I4 L
C1    iostatus           I  O
C
C2    Subroutine CHGS03 is the main workiing routine
C2    for the changing of font styles and colours
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/lfont.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/style.inc'
      include 'include/viewport.inc'
      include 'include/vntable.inc'
      include 'include/gtxt2.inc'
      include 'include/daxcolor.inc'
C
      REAL WX1,WY1,M(3,3)
      INTEGER*2 MP,D1,D2,TENT
      INTEGER*4 TMEN,TCELL,C,I,MOD,TFNT,FN
      LOGICAL OK,OPTION,QUIT,DELETE,CVERFY
      CHARACTER NAME*20,TOK,CLRNAM*16
      INTRINSIC MOD,CHAR
C
      EXTERNAL ALLDRW,CFONT,CCOLOR,DEPRNT,DIM500,DRWFAW,DRWFLW
      EXTERNAL ERSFAW,ERSFLW,GTMCLO,PENDRW,RSCRF,SETAPN,UNFLAG
      EXTERNAL WINDOW,ZSFLAG,CVERFY
C
 25   CONTINUE
      OK=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processinig here
 20   CONTINUE
      IF (CCMD.EQ.'W') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
C******************************************************
C                LINE CONFIGURATION                   *
C******************************************************
      ELSE IF (CVERFY(CCMD,'=fk')) THEN
C     ****************************
C       Change line attributes.  
C     ****************************
         CALL INSATT(CCMD)
C        Don't forget to un highlight the "Attribues" cell.
         CALL GTMCLO(TMEN, TCELL)                                  
C
C
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C             RETRACE  OPTION                                  *
C***************************************************************
C        if backspace char,remove last entity list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.TRUE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C        reset return status
         OK=.FALSE.
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
               OK=.TRUE.
         DO 50 I=1,NDATA
C           read the entity from list
            CALL RSCRF(I,MP,WX1,WY1,D1,D2)
C           remove the attention flag
C           read the entity data
C           read the entity data into buffers
            CALL ALLRD(MP,TENT,M,DELETE)
C            CALL ALLDRW(TENT,MP)
C            IF (I.GE.VNDATA) THEN
C               CALL DBOX(WX1,WY1)
C            END IF
C
            TFNT=IMBUFF(6)
C           erase the entity from screen
               VPMOV = .TRUE.
               CALL PENERS()
               CALL SPCDRW(TENT,MP,.FALSE.,M,.FALSE.)
               CALL PENDRW()
               VPMOV = .FALSE.
               GOTO (111,112,113) FN
 111           CONTINUE
C                 change the font of the entity
                  IMBUFF(6)=CLFONT
                  GOTO 120
 112           CONTINUE
C                 change the colour of the entity
                  IMBUFF(3)=COLOUR
                  GOTO 120
 113           CONTINUE
C                 change the colour of the entity
                  IMBUFF(12)=THICK
 120           CONTINUE
C              reset status code
               IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C              write data back to MI
               MIP=MP
               CALL DIM500(MP,OK)
C              redraw the entity on screen
               VPMOV = .TRUE.
               CALL SPCDRW(TENT,MP,.FALSE.,M,.FALSE.)
               VPMOV = .FALSE.
C
 50      CONTINUE
         VNDATA=NDATA+1
C        ensure scratch file is cleared befor leaving
         CALL ZRFLAG()
C        ensure pen reset to draw colour
         CALL PENDRW()
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
      END IF
 99   CONTINUE
C     ensure caller no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGSCL(OK)
C     ===================
C1    vartype            L
C1    iostatus           O
C
C2    Subroutine CHGSCL gets the input of the
C2    new scale in the change scale option      
C2    OK returns FALSE if change of scale aborted
C2    by hiting CR for scale value
C     
C
      include  'include/menun.inc'
      include  'include/ndata.inc'
      include  'include/params.inc'
C
      REAL X
      CHARACTER*1 TOKEN,INPUT*80,TEMP*80
      INTEGER*4 NLEN1,NLEN
      DOUBLE PRECISION DN1
      LOGICAL OK
      INTRINSIC REAL
      EXTERNAL DPRMXP, NLEN1, GTMCLO, AEXPRN, DEPRNT,
     +         CRUNCH, GTMCWT, CLRPEW, SHOPAP, DCPRNT,
     1         NLEN
C     set the drawing scale
 500  CONTINUE
      CALL DPRMXP(14,INPUT)
C
      IF ( NLEN(INPUT).EQ.0 ) THEN
C        zero length string returned,abort this option
C        ensure cell no longer highlighted
         OK = .FALSE.
         CALL GTMCLO(MEN,CELLN)
      ELSE
         CALL AEXPRN(INPUT,DN1,*500)
C        set drawscale to new value
         IF (REAL(DN1).EQ.0.0) THEN
            CALL DEPRNT(160)
            GOTO 500
         ENDIF
         DRWSCL=REAL(DN1)
C        save the scale as character string
         IF (DRWSCL.LE.1) THEN
C           set new character string for scale
            X=1/DRWSCL
            WRITE(UNIT=TEMP,FMT='(F10.3)') X
            INPUT='1/'//TEMP(1:NLEN1(TEMP))
          ELSE
            X=DRWSCL
            WRITE(UNIT=TEMP,FMT='(F10.3)') X
            INPUT=TEMP(1:NLEN1(TEMP))//'/1'
          END IF
          CALL CRUNCH(INPUT)
          DRGSCL=INPUT(1:NLEN1(INPUT))
C         show scale selected
          CALL GTMCWT(MEN,CCMD,DRGSCL)
C         clear any error messages
          CALL CLRPEW()
          OK = .TRUE.
      END IF
C
      END
C
C---------------------------------------------------------------------
C
      SUBROUTINE CHGT00()
C     ===================
C
C1    no arguments required
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
C
      INTEGER TMEN,TCELL,I
      REAL REAL
      INTRINSIC REAL
      EXTERNAL GTMCLO,GTCLRM,MNLCS0,CHGT02,MNLCT0,UNFLAG,CLROPF
C
      TMEN=MEN
      TCELL=CELLN
C
C     initialize CHANGE TEXT menu
      CALL MNLCT0()
C     save the current text parameters
      REFDAT(1,3)=SLANT
      REFDAT(2,3)=THIGT
      REFDAT(3,3)=TWIDTH
      REFDAT(4,3)=TANGL
      REFDAT(5,3)=REAL(JUST)
C     cancel all change text flags
      CALL CLROPF()
C     enter the CHANGE TEXT routine
      CALL CHGT02()
C     ensure screen flags are cleared before leaving
      CALL UNFLAG(.TRUE.)
C     recover text parameters before leaving
      SLANT=REFDAT(1,3)
      THIGT=REFDAT(2,3)
      TWIDTH=REFDAT(3,3)
      TANGL=REFDAT(4,3)
      JUST=INT(REFDAT(5,3))
C     clear option menu
      CALL GTCLRM(3)
C     ensure caller menu,cell is no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGT02()
C     ===================
C
C1    no arguments required
C
C2    Subroutine CHGT02 is the main working routine
C2    for the CHANGE TEXT function.Uses the normal
C2    window search routines for entity selection.
C2    If Backspace is hit this cancels the last selected entity
C2    only and this is indicated by the box at the centre
C2    being removed.
C
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/movdat.inc'
C
      REAL X,Y
C
      INTEGER*2 MIPP,ENT
      INTEGER*4 TMEN,TCELL,I
C
      LOGICAL OK,OPTION,QUIT
C
      EXTERNAL CHGT03,ADSRCH,NOSRCH,FINDET,UNFLAG
C
C     disable searching for all entity types except text
      CALL NOSRCH()
      CALL ADSRCH(TEXT)
C
 10   CONTINUE
C***************************************************************
C                    ENTITY SEARCH ROUTINE                     *
C***************************************************************
C     find and flag an entity at hit point
      CALL FINDET(37,X,Y,MIPP,ENT,OPTION,QUIT)
C
      IF (QUIT) THEN
C        ensure no selections retained
         CALL UNFLAG(.TRUE.)
         RETURN
      END IF
      IF (.NOT.OPTION) GOTO 10
C***************************************************************
C                     MENU3 OPTIONS                            *
C***************************************************************
 20   CONTINUE
C
      IF (MEN.EQ.3) THEN
         CALL CHGT03(OK,OPTION,QUIT)
         IF (QUIT) THEN
C           ensure no selections retained
            CALL UNFLAG(.TRUE.)
            RETURN
         END IF
         IF (.NOT.OPTION) GOTO 10
         GOTO 20
      END IF
C
 99   CONTINUE
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHGT03(OK,OPTION,QUIT)
C     =====================
C
C2    Subroutine CHGT03 is the main workiing routine
C2    for the changing of text parameters
C
      include 'include/menun.inc'
      include 'include/swind.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/ndata.inc'
      include 'include/movdat.inc'
      include 'include/viewport.inc'
C
      REAL WX1,WY1,TSLA,X,Y,DISTXY
C
      INTEGER*2 MP,NCHARS,NLEN2,TJUST,D1,D2,MIPP,ENT,ENTCOL
      INTEGER*4 TMEN,TCELL,C,I,MOD,POS,ST,FIN,NLEN,NINT
C
      LOGICAL OK,OPTION,QUIT,CVERFY,CHANGE
C
      CHARACTER*80 TSTRNG,TEMP,NUM*81,NUMT
C
      COMMON /CHGTTT/TSTRNG
C
      INTRINSIC CHAR,MOD,NINT
C
      EXTERNAL CVERFY,INST10,GTMCHI,GTMCLO,MNLPTS,WINDOW,MNUPTS,
     +         ZSFLAG,DEPRNT,NLEN2,RSCRF,DBOX,DER500,NLEN,
     +         ERSTXT,UCODET,CODET,DEM500,DRWTXT,UNFLAG,DISTXY
C
C
      ENT = TEXT
 25   CONTINUE
      OK=.FALSE.
      OPTION=.FALSE.
      QUIT=.FALSE.
      TMEN=MEN
      TCELL=CELLN
C     start option processing here
 20   CONTINUE
C
      IF (CCMD.EQ.'K') THEN                                     
C***************************************************************
C        Text Justification.                                   *
C***************************************************************
         CALL INST10()
         OPFLAG(1) = .TRUE.
         CALL GTMCLO(TMEN,TCELL)
         RETURN
C
      ELSE IF ( CVERFY(CCMD,'AVHW') ) THEN
C***************************************************************
C        He wants to change the parameters first               *
C        test for current selection of changes                 * 
C***************************************************************
         IF (CCMD.EQ.'A') THEN
C           text base angle
C           test for opflag status
            IF (OPFLAG(3)) THEN
C              this one is selected,so cancel it
               CHANGE=.FALSE.
               OPFLAG(3)=.FALSE.
            ELSE
C              enable this one
               CHANGE=.TRUE.
               OPFLAG(3)=.TRUE.
            END IF
         ELSE IF (CCMD.EQ.'V') THEN
C           text slant angle
C           test for opflag status
            IF (OPFLAG(4)) THEN
C              this one is selected,so cancel it
               CHANGE=.FALSE.
               OPFLAG(4)=.FALSE.
            ELSE
C              enable this one
               CHANGE=.TRUE.
               OPFLAG(4)=.TRUE.
            END IF
         ELSE IF (CCMD.EQ.'W') THEN
C           text width
C           test for opflag status
            IF (OPFLAG(5)) THEN
C              this one is selected,so cancel it
               CHANGE=.FALSE.
               OPFLAG(5)=.FALSE.
            ELSE
C              enable this one
               CHANGE=.TRUE.
               OPFLAG(5)=.TRUE.
            END IF
         ELSE IF (CCMD.EQ.'H') THEN
C           text height
C           test for opflag status
            IF (OPFLAG(6)) THEN
C              this one is selected,so cancel it
               CHANGE=.FALSE.
               OPFLAG(6)=.FALSE.
            ELSE
C              enable this one
               CHANGE=.TRUE.
               OPFLAG(6)=.TRUE.
            END IF
         END IF
         IF (CHANGE) THEN
C           make parameter change,and ensure hilited
            CALL INST10()
            CALL GTMCHI(TMEN,TCELL)
         ELSE
C           ensure hilite cancelled
            CALL GTMCLO(TMEN,TCELL)
         END IF
         RETURN
      ELSE IF (CCMD.EQ.'w') THEN
C***************************************************************
C                     WINDOW  OPTION                           *
C***************************************************************
C        load the point modes to menu
         CALL MNLPTS()
C        use window for selection of entities
         CALL WINDOW(.TRUE.)
C        unload the point modes
         CALL MNUPTS()
      ELSE IF (CCMD.EQ.CHAR(149)) THEN
C***************************************************************
C             RETRACE  OPTION                                  *
C***************************************************************
C        if backspace char,remove last entity list
C        clear the last entity flag in buffer
         CALL ZSFLAG(.TRUE.,OK)
         IF (.NOT.OK) CALL DEPRNT(33)
C        reset return status
         OK=.FALSE.
      ELSE IF (CCMD.EQ.'S') THEN
C***************************************************************
C             NEW TEXT STRING OPTION                           *
C***************************************************************
C        change the text string
 30      CONTINUE
         IF (OPFLAG(7)) THEN
C           this one is selected,so cancel it
            CHANGE=.FALSE.
            OPFLAG(7)=.FALSE.
         ELSE
C           enable this one
            CHANGE=.TRUE.
            OPFLAG(7)=.TRUE.
            CALL DCPRNT(39)
            CALL DPRMXP(90,TSTRNG)
            NCHARS=NLEN2(TSTRNG)
            IF ( NCHARS .GT. 80 ) NCHARS=80
            IF ( NCHARS .EQ. 1 .AND .TSTRNG(1:1) .EQ. ' ' ) GOTO 30
C
         RETURN
         END IF
      ELSE IF (CCMD.EQ.'y') THEN
C***************************************************************
C             Change word OPTION                               *
C***************************************************************
C        change the word
 33      CONTINUE
C        enable this one
C
         CALL FINDET(41,X,Y,MIPP,ENT,OPTION,QUIT)
C
         IF (QUIT .OR. OPTION )  THEN
C           ensure hilite cancelled
            CALL GTMCLO(TMEN,TCELL)
            IF ( QUIT ) RETURN
            GOTO 25
         END IF
C
         CALL DTR500(IMBUFF(9),OK)
         CALL UCODET(RDBUFF(6),TSLA,TJUST,NCHARS)
 
         CALL XYNORG(RDBUFF(1),RDBUFF(2),TJUST,RDBUFF(5),TSLA,NCHARS,
     +            PAPTOW*RDBUFF(3),PAPTOW*RDBUFF(4),WX1,WY1)
C
C        calculate character position in text
         POS=NINT(0.5+ABS((DISTXY(X,Y,WX1,WY1)/(PAPTOW*RDBUFF(3)))))
C
C        Set up maximum position available in character string of DAXCAD text
         POS = MIN(LEN(CBUFF),POS)
C
C        Unflag the string now.
         CALL ZSFLAG(.TRUE.,OK)
C
C        put the pointer to the character first indicated.
         FIN=POS
         ST=POS
C        set up temporary string
         NUM=CBUFF
C
         IF ( NLEN(NUM).EQ.0 ) THEN
            CALL DEPRNT(408)
         ELSE
            IF ( POS .EQ. 1 ) THEN
               ST=1
            ELSE
C              Count back first
 34            CONTINUE
               IF ( ST.GT.1 .AND. NUM(ST:ST).NE.' ' ) THEN
                  ST=ST-1
                  GOTO 34
               ENDIF
            END IF
C           ST  now points to the start of the word
C
CDHR        lets be sensible only show the word
            IF(NUM(POS:POS).EQ.' '.OR.NUM(ST:ST).NE.' ')  ST= ST-1
 
            I=LEN(NUM)
C
            IF ( POS .LT. I ) THEN
              FIN=INDEX(NUM(ST+2:),' ')+ST+1
            ELSE
               FIN=I+1
            END IF
C           FIN now points to the end of the word
C
C           Now get rid of old text
            CALL DTPMSG(42,1)
            CALL CPRINT(NUM(ST+1:FIN-1))
            CALL DPRMX2(86,NUM(ST+1:FIN-1),NUMT)
C
            VPMOV = .TRUE.
            CALL PENERS()
            CALL ALLDRW(ENT,MIPP)
            CALL PENDRW()
            VPMOV = .FALSE.
C            CALL ERSTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                  RDBUFF(4),RDBUFF(5),RDBUFF(6),
C     1                  CBUFF)
C           Concatenate the three strings together
            IF ( NLEN(NUMT) .EQ. 0 ) THEN
               IF ( ST .GT. 0 ) THEN
                  IF ( FIN .LT. LEN(CBUFF) ) THEN
                     TEMP=CBUFF(:ST)//CBUFF(FIN:)
                  ELSE
                     TEMP='                                    '
                  END IF
               ELSE
                  IF ( FIN .LT. LEN(CBUFF) ) THEN
                     TEMP=CBUFF(FIN:)
                  ELSE
                     TEMP='                                    '
                  END IF
               END IF
            ELSE
               IF ( ST .GT. 0 ) THEN
                  IF ( FIN .LT. LEN(CBUFF) ) THEN
C
                     TEMP=CBUFF(:ST)//
     +                    NUMT(1:NLEN2(NUMT))//CBUFF(FIN:)
                  ELSE
                     TEMP=CBUFF(:ST)//NUMT(1:NLEN2(NUMT))
                  END IF
               ELSE
                  IF ( FIN .LT. LEN(CBUFF) ) THEN
                     TEMP=NUMT(1:NLEN2(NUMT))//CBUFF(FIN:)
                  ELSE
                     TEMP=NUMT(1:NLEN2(NUMT))
                  END IF
               END IF
            END IF
C
            NCHARS=MIN(NLEN(TEMP),80)
            CALL CODET(TSLA,TJUST,NCHARS,RDBUFF(6))
            CBUFF=TEMP
C           Restore in data base
            CALL DEM500(MP,OK)
C           Draw the string
            ENTCOL=IMBUFF(3)
C
C           set active colour to that of entity
            CALL SETAPN(ENTCOL)
            VPMOV = .TRUE.
C           Must set this off
            CALL ALLDRW(ENT,MP)
            VPMOV = .FALSE.
C            CALL DRWTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                  RDBUFF(4),RDBUFF(5),RDBUFF(6),
C     1                  CBUFF)
C
         END IF
C
C        stay in the loop
         GOTO 33
C
      ELSE IF (CCMD.EQ.CHAR(13)) THEN
C***************************************************************
C                     ACCEPT OPTION                            *
C***************************************************************
         IF (NDATA.GT.0) THEN
C           must be entities in buffer
            OK=.TRUE.
         DO 50 I=1,NDATA
C           read the entity from list
            CALL RSCRF(I,MP,WX1,WY1,D1,D2)
C           remove the attention flag
            CALL DER500(MP,OK)
            IMBUFF(1)=MOD(IMBUFF(1)+0,128)
C
C            IF (I.GE.VNDATA) CALL DBOX(WX1,WY1)
C           read the entity data
C
            IF (IMBUFF(2).EQ.TEXT) THEN
C              erase the text from screen
               VPMOV = .TRUE.
               CALL PENERS()
C              take off attention flag
               CALL LABNFG(MP)
               CALL ALLDRW(ENT,MP)
               CALL PENDRW()
               VPMOV = .FALSE.
C               CALL ERSTXT(RDBUFF(1),RDBUFF(2),RDBUFF(3),
C     +                     RDBUFF(4),RDBUFF(5),RDBUFF(6),
C     1                     CBUFF)
C              find current text pars
               CALL UCODET(RDBUFF(6),TSLA,TJUST,NCHARS)
C              change the required text parameters
               IF (OPFLAG(4)) THEN
C                 set new slant
                  TSLA=SLANT
               END IF
               IF (OPFLAG(1).OR.OPFLAG(2)) THEN
C                 set new justification
                  TJUST=JUST
               END IF
               IF (OPFLAG(3)) THEN
C                 change text base angle
                  RDBUFF(5)=TANGL
               END IF
               IF (OPFLAG(5)) THEN
C                 change text width
                  RDBUFF(3)=TWIDTH
               END IF
               IF (OPFLAG(6)) THEN
C                 change text height
                  RDBUFF(4)=THIGT
               END IF
               IF (OPFLAG(7)) THEN
C                 replace the text string
                  CBUFF=TSTRNG(1:80)
               END IF
               NCHARS=NLEN2(CBUFF)
C              IF (NCHARS.GT.80) NCHARS=80
C              re-code the text parameters
               CALL CODET(TSLA,TJUST,NCHARS,RDBUFF(6))
C              reset status code
C              write the modified text back to database
               MIP=MP
               CALL DEM500(MP,OK)
C              redraw the text with new pars
               VPMOV = .TRUE.
               CALL ALLDRW(ENT,MP)
               VPMOV = .FALSE.
C
            END IF
 50      CONTINUE
         VNDATA=NDATA+1
C        ensure screen flags are cleared before leaving
         CALL UNFLAG(.TRUE.)
         IF ( OPFLAG(7) ) THEN
            OPFLAG(7)=.FALSE.
            CALL FNDPOS(256,C)
            CALL GTMCLO(3,C)
         END IF
         ELSE
C           nothing in buffer,tell the idiot
            CALL DEPRNT(34)
         END IF
      ELSE
C        function must not be enabled yet
         CALL DEPRNT(8)
      END IF
 99   CONTINUE
C     ensure caller no longer hilited
      CALL GTMCLO(TMEN,TCELL)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE CHSC00()
C     ===================
C1    vartype        
C1    iostatus        
C
C2    Subroutine CHSC00 transforms the scale of the entities 
C2    in the database
C      
C
      include'include/movdat.inc'
      include'include/dhead.inc'
      include'include/ndata.inc'
      include'include/params.inc'
      include'include/wtov.inc'
C
      INTEGER*2 TTMIP
      INTEGER*4 FN, TYPE
      REAL RDHD3, RDHD12,RDHD1,TX,TY
      LOGICAL OK
      EXTERNAL INSP07, RSTMOV, SAVMOV, CLROPF,
     +         CLEAR, DISFRA, CHGSCL, DCPRNT, CLRSCL,
     1         GRID1
C
C     set scale values in rdhead to the
C     required value as if a part has just 
C     been read in to fool insp07
      RDHD3 = RDHEAD(3)
      RDHEAD(3) = PAPTOW 
      RDHD12 = RDHEAD(12)
      RDHEAD(12) = DRWSCL      
      RDHD1 = RDHEAD(1)
      RDHEAD(1) = DBUFAC
C
C     get the new scale value
      CALL CHGSCL(OK)
C     if the man wants to change the scale then do it
      IF(OK) THEN
C       set control data for INSP07
C       set pointer to start of database
        TTMIP = 1
C       save current opflags
        CALL SAVMOV()

C
C       fix world to paper
        CALL CHGP05()
C       save current viewport limits
        CALL OLDVPT()
C       find length and width of paper mapping
        TX=WPXMAX-WPXMIN
        TY=WPYMAX-WPYMIN
C       set view limits to 3% greater than world extents
        CALL WORLD(WPXMIN-0.015*TX,WPYMIN-0.015*TY,
     +             WPXMAX+0.015*TX,WPYMAX+0.015*TY)
C       save current viewport limits
        CALL OLDVPT()
C       set view limits to 3% greater than world extents
        CALL WORLD(WPXMIN-0.015*TX,WPYMIN-0.015*TY,
     +             WPXMAX+0.015*TX,WPYMAX+0.015*TY)
C       change the scale
C
C       clear the graphics screen
        CALL  CLEAR()
C       show the drawing frame
        CALL DISFRA()
C       change the scale
C
C       tell him it's started
        CALL DCPRNT(606)
C       first scale master data
C
C       clear opflags
        CALL CLROPF()
        CALL CLRSCL()
C
C       set opflags as required
C       proportional scaling on 
        OPFLAG(6) = .TRUE.
C       set the data type to MASTER
        TYPE = 1
C       set entity type to COMPONENT
        FN = 1
        CALL INSP07(FN,TTMIP,TYPE)
C
C       now scale ordinary entities
C
C       clear opflags
        CALL CLROPF()
        CALL CLRSCL()
C
C       set text scaling on
        OPFLAG(6) = .TRUE.
C       set dimension scaling on
        OPFLAG(7) = .TRUE.
C       set the data type to other. not MASTER
        TYPE = 2                   
C       set to PART
        FN = 3
        CALL INSP07(FN,TTMIP,TYPE)
        CALL RSTMOV()   
        IF(SETGRD) CALL GRID1()
C       tell him it's finished
        CALL DCPRNT(607)       
      ENDIF               
      RDHEAD(3) = RDHD3
      RDHEAD(12)= RDHD12
      RDHEAD(1) = RDHD1
      END
C
C------------------------------------------------------------
C



      SUBROUTINE CHUN00()
C     ===================
C1    vartype        
C1    iostatus        
C
C2    Subroutine CHUN00 transforms the scale of the entities 
C2    in the database
C      
C
      include'include/movdat.inc'
      include'include/dhead.inc'
      include'include/ndata.inc'
      include'include/params.inc'
      include'include/wtov.inc'
C
      INTEGER*2 TTMIP
      INTEGER*4 FN, TYPE
      REAL RDHD1,TX,TY,RDHD3
      LOGICAL OK
      EXTERNAL INSP07, RSTMOV, SAVMOV, CLROPF, WORLD,
     +         CLEAR, DISFRA, CHDBUN, DCPRNT, OLDVPT,
     1         GRID1
C
C     set unit conversion value in rdhead to the
C     required value as if a part has just 
C     been read in to fool insp07
      RDHD1 = RDHEAD(1)
      RDHD3 = RDHEAD(3)
      RDHEAD(1) = DBUFAC 
      RDHEAD(3) = PAPTOW
C
C     get the new units
      CALL CHDBUN(OK)
C     if the man wants to change the scale then do it
      IF(OK) THEN
C       set control data for INSP07
C       set pointer to start of database
        TTMIP = 1
C       save current opflags
        CALL SAVMOV()

C       fix world to paper
        CALL CHGP05()
C       save current viewport limits
        CALL OLDVPT()
C       find length and width of paper mapping
        TX=WPXMAX-WPXMIN
        TY=WPYMAX-WPYMIN
C       set view limits to 3% greater than world extents
        CALL WORLD(WPXMIN-0.015*TX,WPYMIN-0.015*TY,
     +             WPXMAX+0.015*TX,WPYMAX+0.015*TY)
C       save current viewport limits
        CALL OLDVPT()
C       set view limits to 3% greater than world extents
        CALL WORLD(WPXMIN-0.015*TX,WPYMIN-0.015*TY,
     +             WPXMAX+0.015*TX,WPYMAX+0.015*TY)
C       change the scale
C
C       clear the graphics screen
        CALL  CLEAR()
C       show the drawing frame
        CALL DISFRA()
C
C       reset the grid sizes
        GRIDSX = GRIDSX*RDHEAD(1)/DBUFAC 
        GRIDSY = GRIDSY*RDHEAD(1)/DBUFAC 
        GRIDOX = GRIDOX*RDHEAD(1)/DBUFAC 
        GRIDOY = GRIDOY*RDHEAD(1)/DBUFAC        
C
C       tell him it's started
        CALL DCPRNT(608)
C       first scale master data
C       clear opflags and scale
        CALL CLROPF()
        CALL CLRSCL()
C      
C       scale symbol masters
C       set the data type to MASTER
C        TYPE = 1
C       set entity type to SYMBOL
C        FN = 2
C        CALL INSP07(FN,TTMIP,TYPE)
C
C       scale component masters
C       now scale ordinary entities
C         
C       reset opflags and scale
        CALL CLROPF()
        CALL CLRSCL()               
C     
C       set the data type to MASTER
        TYPE = 1
C       set to COMP
        FN = 1
        CALL INSP07(FN,TTMIP,TYPE)
C
C       now scale ordinary entities
C         
C       reset opflags and scale
        CALL CLROPF()
        CALL CLRSCL()               
C       set the translation on
        OPFLAG(4) = .TRUE.        
C
C       set the data type to OTHER
        TYPE = 2
C       set to PART
        FN = 3
        CALL INSP07(FN,TTMIP,TYPE)
        CALL RSTMOV()   
        IF(SETGRD) CALL GRID1()
C       tell him it's finished
        CALL DCPRNT(609)       
      ENDIF               
      RDHEAD(1) = RDHD1
      RDHEAD(3) = RDHD3
      END
C
C------------------------------------------------------------
C


      SUBROUTINE MAJCHG()
C     ===================
C1    no arguments required
C2
C2    this routine takes control when the CHANGE mode
C2    of operation is selected from the master menu.
C2    controls operation of the CHANGE function
C
      include 'include/menun.inc'
      include 'include/ndata.inc'
      include 'include/masti.inc'
C
      INTEGER CP,C,TMEN,TCELL,ICHAR
C
      REAL X,Y
C
C
      EXTERNAL MNICHG,GTHFMC,CHGS00,CHGT00,CHGP00,CHGD00
      EXTERNAL TCURS,GTMCLO,GTMCHI,CLRPEW,GTCLRM,UNFLAG
C
C     Now activate the CHANGE major option menu
      CALL MNICHG()
C
C     clear the error and prompt windows
      CALL CLRPEW
C     Read the major option menu to find out what he wants to
C     move. He may have made a mistake,and wants to return to
C     previous status of main menu,so check for that as well.
C     Making single line the default insert text
      MEN=2
C     'F' is the token used by change font
      CCMD='F'
C     Find the menu and hightlite it
      CALL GTHFMC(MEN,CCMD,TCELL)
      CALL GTMCHI(MEN,TCELL)
      CELLN=TCELL
C      IF ( .NOT.MACOP ) GOTO 20
      GOTO 20
C
 10   CONTINUE
C     Read a cursor hit to select CHANGE mode
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
C        ensure grouped entities can be returned
C        as individual entities
         GSSTAT=1
         IF (CCMD.EQ.'F') THEN
C           change font style option
            CALL CHGS00(1)
         ELSE IF (CCMD.EQ.'C') THEN
C           change colour option
            CALL CHGS00(2)
         ELSE IF (CCMD.EQ.'t') THEN
C           change thickness option
            CALL CHGS00(3)
         ELSE IF (CCMD.EQ.'T') THEN
C           change text option
            CALL CHGT00()
         ELSE IF (CCMD.EQ.'P') THEN
C           change paper option
            CALL CHGP00()
         ELSE IF (CCMD.EQ.'p') THEN
C           ensure ghroups are returned as group headers
            GSSTAT=4
C           change property option (attributes)
            CALL CHGA00()
         ELSE IF (CCMD.EQ.'D') THEN
C           change dimension option
            CALL CHGD00()
         ELSE IF (CCMD.EQ.'f') THEN
C           change form option
            CALL CHGF00()
         ELSE IF (CCMD.EQ.'E') THEN
C           Edit curve opition
            CALL CHGE00()
         ELSE IF (CCMD.EQ.'s') THEN
C           Change symbol or component.
            CALL CHGC00()
         ELSE IF (CCMD.EQ.'l') THEN
C           Change center line..
            CALL CHGCEN()
         ELSE
C           unrecognized delete option
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
         CALL DEPRNT(24)
      END IF
C     ***************************************************************
C     **************************************MAJOR OPTIONS END********
C     ***************************************************************
      GOTO 10
C
 99   CONTINUE
C
C     clean up the select buffer if necessary
      CALL UNFLAG(.TRUE.)
C
      RETURN
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE MASCHG(SYMBOL,NAME,GLOB,OLDSTR,NEWSTR)
C     -------------------------------------------------
C1    VARTYPE            L      L     L   C*(*)  C*(*)
C1    IOSTATUS           I      I     I     I      I
C                                              
C2    This routine changes the entities whose MI pointers are
C2    stored in SWINDU.
C
      include   'include/masti.inc'
      include   'include/swind.inc'
      include   'include/nbuff.inc'
      include   'include/entity.inc'
C
      CHARACTER*(*) OLDSTR,NEWSTR
      CHARACTER*80 FILETP,NAMSTR
      LOGICAL SYMBOL,NAME,OK,EX,OP,GLOB
      INTEGER*4 I,BINDEX,INDX,FN,TDATA,TSWIN,NLEN,L,UN
      INTEGER*2 P,D1,D2,TENT,TMIP,PMIP,TBUF(2)
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP
      REAL BX,BY        
C
      EXTERNAL BINDEX,NLEN
C                           
C     First ... is there anything to change.
      IF (NDATA.EQ.0.AND..NOT.GLOB) RETURN                
C                          
C     Unflag the entities on the screen.
      CALL UNFLAG(.FALSE.)  
C
      IF (SYMBOL) THEN
         TENT = SYMBI
         FN = 2
      ELSE
         TENT = COMPI                               
         FN = 1
      ENDIF
C                  
      IF(GLOB) THEN
         TDATA = NMIPOS-1
      ELSE
C        Got to make a temporary swindu for regc68 so it does
C        not corrupt our list of symbols/components.
         TDATA = NDATA
         TSWIN = SWINDU
         NDATA = 0
         CALL OURSCR(SWINDU,14,OK) 
      ENDIF
C
C     Now change them.                                              
      DO 10 I=1,TDATA
         IF (GLOB) THEN
            P=I
            CALL DIR500(P,OK)
            CALL CHGC03(.FALSE.,.FALSE.,P,SYMBOL,NAME,OLDSTR,OK)
            IF(.NOT.OK) P=0
         ELSE
C           read from saved SWINDU
            READ(UNIT=TSWIN,REC=I) P,BX,BY,D1,D2
         ENDIF
C
C        SWINDU may have some entities flagged as deleted by 
C        setting mi pointer to 0.
         IF (P.NE.0) THEN
C           Wipe it off the screen.
            CALL PENERS()
            CALL ALLDRW(TENT,P)
            CALL PENDRW()
C           read instance from MI.
            CALL DIR500(P,OK)
C           Get the old name
            TMIP = IMBUFF(9)
            CALL DTR500(TMIP,OK)  
            TBUF(1) = ICBUFF(1)
            TBUF(2) = ICBUFF(2) 
C           Put in the new name.
            IF (NAME) THEN
C              Change the name.
               CBUFF = NEWSTR
               NAMSTR = NEWSTR                                      
C              We already know this new name does exist.
               OK = .TRUE.
            ELSE
C              Change the path.
               INDX = BINDEX(CBUFF,'/')
               L = NLEN(NEWSTR)
C              Is the new path empty ?
               IF (L.GT.0) THEN
C
                  NAMSTR = NEWSTR     
                  if (L.EQ.1 .AND. NAMSTR.EQ.'/') then
C                    The new path is a '/' only.
C                    This little trick will trick the next IF
C                    into doing what we want !
                     INDX = INDX + 1
                  ENDIF
C                 Do we have an old path to remove.
                  IF (INDX.GT.0) THEN
                     CALL SUFFIX(NAMSTR,CBUFF(INDX:))
                  ELSE
                     CALL SUFFIX(NAMSTR,'/')
                     CALL SUFFIX(NAMSTR,CBUFF)
                  ENDIF
C
               ELSE
                  IF (INDX.GT.0) THEN
                     NAMSTR = CBUFF((INDX+1):)
                  ENDIF
               ENDIF
C              Check the new name exists before you try to use it.
               CALL INSC10(NAMSTR,FN,PMIP,OK)
               IF (.NOT.OK) THEN
C                 does not already exist in the drawing
C                 test existance of external component file
                  INQUIRE(FILE=NAMSTR,EXIST=EX,ERR=999)
                  OK = EX 
               ENDIF   
            ENDIF
C
C           Only if we are sure the new name/path exists.     
            IF (OK) THEN
C              Give it the new name.
               ICBUFF(1) = TBUF(1)
               ICBUFF(2) = TBUF(2)
               CBUFF = NAMSTR
               CALL DTM500(TMIP,P,OK)
C           
               IF (SYMBOL) THEN
                  TENT = SYMBM         
               ELSE
                  TENT = COMPM
               ENDIF
C              Regenerate this single instance.
               CALL REGC68(NAMSTR,TENT,FN,P,OK)
            ELSE
               CALL DEPRNT(287)
               CALL ALLDRW(TENT,P)    
            ENDIF
         ENDIF
 10   CONTINUE
C                    
 99   CONTINUE    
C     Remove the spare swindu before we go.                     
      IF (.NOT.GLOB) THEN
         CLOSE(TSWIN)
      ENDIF
      RETURN
C
 999  CONTINUE
C     "File handling error has ocurred"
C      WRITE(10,*) '[MASCHG] ERROR'
      CALL DEPRNT(12)
      END
C
C-----------------------------------------------------------
C
C
      SUBROUTINE MNIC00()
C     ===================
C1    No parameters
C
      EXTERNAL GTDMEN, GTCLRM
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Name option. Token is 'N'
      CALL GTDMEN(321,3)
C     Path option. Token is 'A'
      CALL GTDMEN(148,3)
C     Symbol option. Token is 's'
      CALL GTDMEN(186,3)
C     Component option. Token is 'c'
      CALL GTDMEN(189,3)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNICHG()
C     ===================
C1    No arguments required.
C
C2    Clears the major and minor options, and
C2    enters the major CHANGE options.
C2
      EXTERNAL GTDMEN,GTCLRM,GTDMHD
C
C     Clear the minor option menu.
      CALL GTCLRM(3)
C
C     Clear the major option menu.
      CALL GTCLRM(2)
C
C     Enter the CHANGE major option.
      CALL GTDMHD(19,2)
C
C     Load the nouns into major option menu.
C     enter the change text option,token 'T'
      CALL GTDMEN(240,2)
C     enter the change colour option token 'C'
      CALL GTDMEN(241,2)
C     enter the change font option token 'F'
      CALL GTDMEN(242,2)
C     enter the change dimension option token 'D'
      CALL GTDMEN(243,2)
C     enter the change paper option token 'P'
      CALL GTDMEN(244,2)
C     enter the change property option token 'p'
      CALL GTDMEN(245,2)
C     change symbol or component. Token is 's'
      CALL GTDMEN(147,2)
C     change form of spline.
      CALL GTDMEN(246,2)
C     change Thickness.
      CALL GTDMEN(247,2)
C     Curve edit cell
      CALL GTDMEN(248,2)
C2    l is the token for CENTER LINE.
      CALL GTDMEN(349,2)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNLCA0()
C     ===================
C
C1    No arguments required.
C
C2    Subroutine MNLCA0 loads the CHANGE PROPERTY
C2    portion of the option menu no3.
C
      EXTERNAL GTDMEN
C
C     load the name option
C     "Name"
      CALL GTDMEN(343,3)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNLCE0()
C     ===================
C
C1    No arguments required.
C
C2    Subroutine MNLCA0 loads the CURVE EDIT
C2    portion of the option menu no3.
C
      EXTERNAL GTDMEN
C
C     clear the menu
      CALL GTCLRM(3)
C     accept
      CALL GTDMEN(12,3)
C     quit
      CALL GTDMEN(13,3)
C     cancel
      CALL GTDMEN(99,3)
      END
 
      SUBROUTINE MNLCE1()
C     ===================
C
C1    No arguments required.
C
C2    Subroutine MNLCA0 loads the CURVE EDIT
C2    portion of the option menu no3.
C
      EXTERNAL GTDMEN
C
C     clear menu
      CALL GTCLRM(3)
C     Insert a point mode
      CALL GTDMEN(251,3)
C     Add a point
      CALL GTDMEN(252,3)
C     Delete aa point
      CALL GTDMEN(253,3)
C     Move a point
      CALL GTDMEN(254,3)
C     New curve
      CALL GTDMEN(268,3)
C     Change mode
      CALL GTDMEN(259,3)
C     The weigting factor
      CALL GTDMEN(353,3)
 
C
      END
 
      SUBROUTINE MNLCEN()
C     ===================
C2
C2    This routine puts up the centerline options for the change option.
C2
C
      REAL DEGANG,DEG
      CHARACTER*10 STRNG
      EXTERNAL DEG
C
      include 'include/masti.inc'
      include 'include/gtxt2.inc'
      include 'include/inscen.inc'
      include  'include/style.inc'
C
C     b token for BORDER SIZE
      CALL GTDMEN(84,3)
C     And the size ...
      WRITE(STRNG,'(F8.3)') BRDRSZ
      CALL GTPMEN(STRNG,' ',3,8)
C
C     r token for ROTATION ANGLE
      CALL GTDMEN(85,3)
C     And the size ...
      DEGANG = DEG(ROTANG)
      WRITE(STRNG,'(F8.3)') DEGANG
      CALL GTPMEN(STRNG,' ',3,11)
C
C     w token for WINDOW
      CALL GTDMEN(210,3)
C     <13> token for ACCEPT
      CALL GTDMEN(13,3)
C     c token for CANCEL
      CALL GTDMEN(99,3)
C
C     Load the ATTRIBUTES cells.
C     k is the token for colour
      GTMULT = .TRUE.
      CALL GTDMWT(399,3,TXCLR)
C     f is the token for font  
      GTMULT = .TRUE.
      CALL GTDMWT(407,3,CLINET)
C     = is the token for thickness
      GTMULT = .TRUE.
      CALL GTDMWT(409,3,TXTHK)
C
      END
      SUBROUTINE MNLCRD()
C     ===================
C
C1    No arguments required.
C
C2    Loads the secondary CREATE DRAWING MENU options
C2    for paper size options etc.
C
      include 'include/params.inc'
      include 'include/gtxt2.inc'
      include 'include/menun.inc'
      include 'include/vntable.inc'
C
      INTEGER*4 TCELL,TMEN
      CHARACTER*1 TOKEN
      EXTERNAL GTDMEN, FNDTOK, GTMCWT
C     enter the options available at this point
C                         
C2    P is the token for paper size
      GTMULT = .TRUE.
      CALL GTDMEN(8,3)
C
C2    P is the token for Set paper size
C     show current sheet size, and it is
C     a popup so set the flag for multiple
      GTMULT = .TRUE.
      CALL GTDMWT(8,3,DRWSHT(:2))
C     punt out the paper name in the cell below.
      TOKEN = ' '
      TMEN = 3
      TCELL=VNPOS(8) + 1
      CALL GTPMEN(PAPNAM(CURPNO),TOKEN,TMEN,TCELL)
C
C2    U is the token for database units
      CALL GTDMEN(9,3)
C     show units selected
      CALL FNDTOK(9,TOKEN)
      GTMULT = .TRUE.
      CALL GTMCWT(3,TOKEN,DBUNIT)
C
C2    S is the token for drawscale
      CALL GTDMEN(10,3)
C     show scale selected
      CALL FNDTOK(10,TOKEN)
      CALL GTMCWT(3,TOKEN,DRGSCL)
 
C2    R is the token for rotate drawing sheet
      CALL GTDMEN(11,3)
      IF (DRWSHT(3:3).EQ.'R') THEN
C        hilite the rotate cell
         CALL GTHFMC(TMEN,'R',TCELL)
         CALL GTMCHI(TMEN,TCELL)
      END IF
C
C2    O is the token for origin (Position)
      CALL GTDMEN(250,3)
C
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNLCS0()
C     ===================
C1    No arguments required.
C
C2    Subroutine MNLCS0 loads the AREA
C2    and ACCEPT portions of the option
C2    menu no3.
C2    Intended for use in CHANGE STYLE
C2
C2    Tokens used here are W and CHAR(150).
C2
      INTRINSIC CHAR
C
      EXTERNAL GTDMEN
C
C2    W is the token for WINDOW
      CALL GTDMEN(210,3)
C2    e is the token for EXCLUDE
C     CHAR(149) is the token for CANCEL.
      CALL GTDMEN(146,3)
C2    CHAR(13) is token for ACCEPT.
      CALL GTDMEN(13,3)
C2
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE MNLCT0()
C     ===================
C
C1    no arguments required
C
C2    Subroutine MNLCT0 loads the option
C2    menu with the controls for the change
C2    text function
C
      EXTERNAL MNLTX1,GTDMEN,MNLCS0
C
C     load the window options
      CALL MNLCS0()
C     must redefine the window token
C     to avoid conflict with text parameters
      CALL GTDMEN(255,3)
C     load the text parameters
      CALL MNLTX1()
C
C     load the text string option
      CALL GTDMEN(256,3)
C
C     load the word option
      CALL GTDMEN(257,3)
C
      END
C
C-----------------------------------------------------------
C
      SUBROUTINE SETDTX(I)
C     ===================
C1                      I
C1                      I
C
C2    Subroutine SETDTX sets the dimension text according
C2    to the drawing sheet size. I passed is the drawing
C2    sheet size ranging from A4,A3,A2,A1,A0 as 1,2,3,4,5.
C
      include 'include/dimendat.inc'
      include 'include/ndata.inc'
C
C
      INTEGER*4 I
      REAL BSTDH(5),DNSTDH(5),ANSTDH(5),BSHR,BSWR,DNHR,DNWR,
     +                                             ANHR,ANWR
C     set data for dim text
      DATA BSTDH/2.5,2.5,2.5,2.5,3.5/
      DATA DNSTDH/2.5,2.5,3.5,3.5,3.5/
      DATA ANSTDH/2.5,2.5,3.5,3.5,3.5/
C     set text width/height values for fixed ratio
      DATA BSHR,BSWR,DNHR,DNWR,ANHR,ANWR/3.0,2.25,3.0,2.25,3.0,2.25/
C
      IF ( BS ) THEN
C        check for BS 308
         DTHGT=BSTDH(I)
         DTWDT=BSTDH(I)*(BSWR/BSHR)
      ELSE IF ( DIN ) THEN
C        check for DIN
         DTHGT=DNSTDH(I)
         DTWDT=DNSTDH(I)*(DNWR/DNHR)
      ELSE IF ( ANSI ) THEN
C        check for ANSI
         DTHGT=ANSTDH(I)
         DTWDT=ANSTDH(I)*(ANWR/ANHR)
      END IF
C
      END
C
C-----------------------------------------------------------
C
