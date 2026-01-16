C
C     @(#)  412.1 date 6/11/92 kbd.ins.ftn 
C
C
C     Filename    : kbd.ins.ftn
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:16:00
C     Last change : 92/06/11 14:32:57
C
C     Copyright : Practical Technology Limited  
C     File :- kbd.ins.ftn
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C Changes:
C 01-Oct-85  LTK   APOLLO 3 keyboard has a numeric keypad escape code plus
C                  two new keys:  F0 and F9.
C
C 29-Mar-84  JCM  Corrected KBD_$F1C to KBD_$F8C to match Pascal insert file
C
C 21-Mar-84  CAS  Corrected KBD_$F1S to KBD_$F8S, KBD_$F1U to KBD_$F8U, 
C                 KBD_$F1C to KBD_$F8C to match Pascal insert file
C
C   Definitions for non-ascii keyboard keys, for use in borrow
C   mode and direct mode in gpr_$event_wait.

      character KBD_$L1, KBD_$L2, KBD_$L3, KBD_$L4, KBD_$L5, KBD_$L6,
     +          KBD_$L7, KBD_$L8, KBD_$L9, KBD_$LA, KBD_$LB, KBD_$LC,
     +          KBD_$LD, KBD_$LE, KBD_$LF

      parameter (
     +  KBD_$L1  = char(16#81),
     +  KBD_$L2  = char(16#82),
     +  KBD_$L3  = char(16#83),
     +  KBD_$L4  = char(16#84),
     +  KBD_$L5  = char(16#85),
     +  KBD_$L6  = char(16#86),
     +  KBD_$L7  = char(16#87),
     +  KBD_$L8  = char(16#88),
     +  KBD_$L9  = char(16#89),
     +  KBD_$LA  = char(16#8A),
     +  KBD_$LB  = char(16#8B),
     +  KBD_$LC  = char(16#8C),
     +  KBD_$LD  = char(16#8D),
     +  KBD_$LE  = char(16#8E),
     +  KBD_$LF  = char(16#8F) )

      character KBD_$L1U, KBD_$L2U, KBD_$L3U, KBD_$L4U, KBD_$L5U,
     +          KBD_$L6U, KBD_$L7U, KBD_$L8U, KBD_$L9U, KBD_$LAU,
     +          KBD_$LBU, KBD_$LCU, KBD_$LDU, KBD_$LEU, KBD_$LFU

      parameter (
     +  KBD_$L1U = char(16#A1),
     +  KBD_$L2U = char(16#A2),
     +  KBD_$L3U = char(16#A3),
     +  KBD_$L4U = char(16#A4),
     +  KBD_$L5U = char(16#A5),
     +  KBD_$L6U = char(16#A6),
     +  KBD_$L7U = char(16#A7),
     +  KBD_$L8U = char(16#A8),
     +  KBD_$L9U = char(16#A9),
     +  KBD_$LAU = char(16#AA),
     +  KBD_$LBU = char(16#AB),
     +  KBD_$LCU = char(16#AC),
     +  KBD_$LDU = char(16#AD),
     +  KBD_$LEU = char(16#AE),
     +  KBD_$LFU = char(16#AF) )

      character KBD_$L1S, KBD_$L2S, KBD_$L3S, KBD_$L4S, KBD_$L5S,
     +          KBD_$L6S, KBD_$L7S, KBD_$L8S, KBD_$L9S, KBD_$LAS,
     +          KBD_$LBS, KBD_$LCS, KBD_$LDS, KBD_$LES, KBD_$LFS

      parameter (
     +  KBD_$L1S = char(16#C9),
     +  KBD_$L2S = char(16#CA),
     +  KBD_$L3S = char(16#CB),
     +  KBD_$L4S = char(16#CC),
     +  KBD_$L5S = char(16#CD),
     +  KBD_$L6S = char(16#CE),
     +  KBD_$L7S = char(16#CF),
     +  KBD_$L8S = char(16#D8),
     +  KBD_$L9S = char(16#D9),
     +  KBD_$LAS = char(16#DA),
     +  KBD_$LBS = char(16#DB),
     +  KBD_$LCS = char(16#DC),
     +  KBD_$LDS = char(16#DD),
     +  KBD_$LES = char(16#DE),
     +  KBD_$LFS = char(16#DF) )

      character KBD_$L1A, KBD_$L2A, KBD_$L3A, KBD_$L1AS, KBD_$L2AS,
     +          KBD_$L3AS,KBD_$L1AU, KBD_$L2AU, KBD_$L3AU

      parameter (
     +  KBD_$L1A = char(16#E8),
     +  KBD_$L2A = char(16#E9),
     +  KBD_$L3A = char(16#EA),
     +  KBD_$L1AS = char(16#EC),
     +  KBD_$L2AS = char(16#ED),
     +  KBD_$L3AS = char(16#EE),
     +  KBD_$L1AU = char(16#F8),
     +  KBD_$L2AU = char(16#F9),
     +  KBD_$L3AU = char(16#FA) )

      character KBD_$R1, KBD_$R2, KBD_$R3, KBD_$R4, KBD_$R5, KBD_$R6

      parameter (
     +  KBD_$R1  = char(16#90),
     +  KBD_$R2  = char(16#91),
     +  KBD_$R3  = char(16#92),
     +  KBD_$R4  = char(16#93),
     +  KBD_$R5  = char(16#94),
     +  KBD_$R6  = char(16#EB) )
    
      character KBD_$R1U, KBD_$R2U, KBD_$R3U, KBD_$R4U, KBD_$R5U,
     +          KBD_$R6U

      parameter (
     +  KBD_$R1U = char(16#B0),
     +  KBD_$R2U = char(16#B1),
     +  KBD_$R3U = char(16#B2),
     +  KBD_$R4U = char(16#B3),
     +  KBD_$R5U = char(16#B4),
     +  KBD_$R6U = char(16#FB) )

      character KBD_$R1S, KBD_$R2S, KBD_$R3S, KBD_$R4S, KBD_$R5S,
     +          KBD_$R6S

      parameter (
     +  KBD_$R1S = char(16#C8),
     +  KBD_$R2S = char(16#B5),
     +  KBD_$R3S = char(16#B6),
     +  KBD_$R4S = char(16#B7),
     +  KBD_$R5S = char(16#B8),
     +  KBD_$R6S = char(16#EF) )

      character KBD_$BS, KBD_$CR, KBD_$TAB, KBD_$STAB, KBD_$CTAB

      parameter (
     +  KBD_$BS  = char(16#95),
     +  KBD_$CR  = char(16#96),
     +  KBD_$TAB = char(16#97),
     +  KBD_$STAB = char(16#98),
     +  KBD_$CTAB = char(16#99) )

      character KBD_$F1, KBD_$F2, KBD_$F3, KBD_$F4,
     +          KBD_$F5, KBD_$F6, KBD_$F7, KBD_$F8,
     +          KBD_$F9, KBD_$F0

      parameter (
     +  KBD_$F1  = char(16#C0),
     +  KBD_$F2  = char(16#C1),
     +  KBD_$F3  = char(16#C2),
     +  KBD_$F4  = char(16#C3),
     +  KBD_$F5  = char(16#C4),
     +  KBD_$F6  = char(16#C5),
     +  KBD_$F7  = char(16#C6),
     +  KBD_$F8  = char(16#C7),
     +  KBD_$F9  = char(16#BB),
     +  KBD_$F0  = char(16#BA) )

      character KBD_$F1S, KBD_$F2S, KBD_$F3S, KBD_$F4S,
     +          KBD_$F5S, KBD_$F6S, KBD_$F7S, KBD_$F8S,
     +          KBD_$F9S, KBD_$F0S

      parameter (
     +  KBD_$F1S  = char(16#D0),
     +  KBD_$F2S  = char(16#D1),
     +  KBD_$F3S  = char(16#D2),
     +  KBD_$F4S  = char(16#D3),
     +  KBD_$F5S  = char(16#D4),
     +  KBD_$F6S  = char(16#D5),
     +  KBD_$F7S  = char(16#D6),
     +  KBD_$F8S  = char(16#D7),
     +  KBD_$F9S  = char(16#BF),
     +  KBD_$F0S  = char(16#BE) )
    
      character KBD_$F1U, KBD_$F2U, KBD_$F3U, KBD_$F4U,
     +          KBD_$F5U, KBD_$F6U, KBD_$F7U, KBD_$F8U,
     +          KBD_$F9U, KBD_$F0U

      parameter (
     +  KBD_$F1U  = char(16#E0),
     +  KBD_$F2U  = char(16#E1),
     +  KBD_$F3U  = char(16#E2),
     +  KBD_$F4U  = char(16#E3),
     +  KBD_$F5U  = char(16#E4),
     +  KBD_$F6U  = char(16#E5),
     +  KBD_$F7U  = char(16#E6),
     +  KBD_$F8U  = char(16#E7),
     +  KBD_$F9U  = char(16#BD),
     +  KBD_$F0U  = char(16#BC) )
    
      character KBD_$F1C, KBD_$F2C, KBD_$F3C, KBD_$F4C,
     +          KBD_$F5C, KBD_$F6C, KBD_$F7C, KBD_$F8C,
     +          KBD_$F9C, KBD_$F0C

      parameter (
     +  KBD_$F1C  = char(16#F0),
     +  KBD_$F2C  = char(16#F1),
     +  KBD_$F3C  = char(16#F2),
     +  KBD_$F4C  = char(16#F3),
     +  KBD_$F5C  = char(16#F4),
     +  KBD_$F6C  = char(16#F5),
     +  KBD_$F7C  = char(16#F6),
     +  KBD_$F8C  = char(16#F7),
     +  KBD_$F9C  = char(16#FD),
     +  KBD_$F0C  = char(16#FC) )
                              

C    mouse buttons

      character KBD_$M1D, KBD_$M1U, KBD_$M2D, KBD_$M2U,
     +          KBD_$M3D, KBD_$M3U, KBD_$M4D, KBD_$M4U

      parameter (
     +  KBD_$M1D = 'a',
     +  KBD_$M1U = 'A',
     +  KBD_$M2D = 'b',
     +  KBD_$M2U = 'B',
     +  KBD_$M3D = 'c',
     +  KBD_$M3U = 'C',
     +  KBD_$M4D = 'd',
     +  KBD_$M4U = 'D'  )


C---The following names refer to the key cap names on the APOLLO 1 and
C   APOLLO 2 keyboards. These names can be used only if the key caps are
C   in the standard APOLLO locations.  IF a 1 follows a defined name then 
C   the key cap name refers to an APOLLO 1 keyboard if a 2 follows then 
C   the name refers to an APOLLO 2.  A '1' or '2'  has only been added to 
C   key cap names that are common to both keyboards, but the key caps are in 
C   different locations on the keyboards.

C---Key caps common to both APOLLO 1 and APOLLO 2 keyboards
    
      character KBD_$LINE_DEL, KBD_$CHAR_DEL, KBD_$CMD, 
     +          KBD_$UP_ARROW, KBD_$LEFT_ARROW, KBD_$NEXT_WIN,
     +          KBD_$RIGHT_ARROW, KBD_$DOWN_ARROW,
     +          KBD_$L_BAR_ARROW, KBD_$R_BAR_ARROW,
     +          KBD_$L_BOX_ARROW, KBD_$R_BOX_ARROW,
     +          KBD_$READ, KBD_$EDIT

      parameter (
     +  KBD_$LINE_DEL     =  KBD_$L2,
     +  KBD_$CHAR_DEL     =  KBD_$L3,
     +  KBD_$CMD          =  KBD_$L5,
     +  KBD_$UP_ARROW     =  KBD_$L8,
     +  KBD_$LEFT_ARROW   =  KBD_$LA,
     +  KBD_$NEXT_WIN     =  KBD_$LB,
     +  KBD_$RIGHT_ARROW  =  KBD_$LC,
     +  KBD_$DOWN_ARROW   =  KBD_$LE,
     +  KBD_$L_BAR_ARROW  =  KBD_$L4,
     +  KBD_$R_BAR_ARROW  =  KBD_$L6,
     +  KBD_$L_BOX_ARROW  =  KBD_$L7,
     +  KBD_$R_BOX_ARROW  =  KBD_$L9,
     +  KBD_$READ         =  KBD_$R3,
     +  KBD_$EDIT         =  KBD_$R4 )
    
C---APOLLO 1 keys

      character KBD_$DOWN_BOX_ARROW1, KBD_$UP_BOX_ARROW1,
     +          KBD_$INS_MODE1, KBD_$MARK1, KBD_$SHELL1, KBD_$HOLD1

      parameter (
     +  KBD_$DOWN_BOX_ARROW1 = KBD_$LD,
     +  KBD_$UP_BOX_ARROW1 = KBD_$LF,
     +  KBD_$INS_MODE1    =  KBD_$L1,
     +  KBD_$MARK1        =  KBD_$R1,
     +  KBD_$SHELL1       =  KBD_$R2,
     +  KBD_$HOLD1        =  KBD_$R5 )


C---APOLLO 2 keys
                     
      character KBD_$DOWN_BOX_ARROW2, KBD_$UP_BOX_ARROW2,
     +          KBD_$INS_MODE2, KBD_$COPY, KBD_$CUT, KBD_$PASTE,
     +          KBD_$UNDO, KBD_$EOL, KBD_$DEL, KBD_$MARK2,
     +          KBD_$SHELL2, KBD_$GROW, KBD_$MOVE, KBD_$POP,
     +          KBD_$AGAIN, KBD_$EXIT, KBD_$HOLD2, KBD_$SAVE,
     +          KBD_$ABORT, KBD_$HELP

      parameter (
     +  KBD_$DOWN_BOX_ARROW2 = KBD_$LF,
     +  KBD_$UP_BOX_ARROW2 = KBD_$LD,
     +  KBD_$MARK2        =  KBD_$L1,
     +  KBD_$INS_MODE2    =  KBD_$L1S,
     +  KBD_$COPY         =  KBD_$L1A,
     +  KBD_$CUT          =  KBD_$L1AS,
     +  KBD_$PASTE        =  KBD_$L2A,
     +  KBD_$UNDO         =  KBD_$L2AS,
     +  KBD_$GROW         =  KBD_$L3A,
     +  KBD_$MOVE         =  KBD_$L3AS,
     +  KBD_$SHELL2       =  KBD_$L5S,
     +  KBD_$POP          =  KBD_$R1,
     +  KBD_$AGAIN        =  KBD_$R2,
     +  KBD_$EXIT         =  KBD_$R5,
     +  KBD_$HOLD2        =  KBD_$R6,
     +  KBD_$SAVE         =  KBD_$R4S,
     +  KBD_$ABORT        =  KBD_$R5S,
     +  KBD_$HELP         =  KBD_$R6S )

C---APOLLO 3 key

      character KBD_$NUMERIC_KEYPAD

      parameter (
     + KBD_$NUMERIC_KEYPAD = char(16#9E) )

C---Multi-National KBD  - ALT keys    
           
      character KBD_$AL, KBD_$AR, KBD_$ALU, KBD_$ARU

      parameter (
     +  KBD_$AL   = char(16#9A),
     +  KBD_$AR   = char(16#9B),
     +  KBD_$ALU  = char(16#9C),
     +  KBD_$ARU  = char(16#9D) )


C------------------------------------------------------------
C The following codes are for the KBD 3 when using the       
C "gpr_$function_keys" event. They can be used with KBD 2 with
C  the caveat that many codes can't be generated from  KBD 2 
C

C UNSHIFTED 
      character  KBD3_$ESC, KBD3_$L1,  KBD3_$L2,  KBD3_$L3,  KBD3_$L1A,
     +           KBD3_$L2A, KBD3_$L3A, KBD3_$L4,  KBD3_$L5,  KBD3_$L6,
     +           KBD3_$L7,  KBD3_$L8,  KBD3_$L9,  KBD3_$LA,  KBD3_$LB,
     +           KBD3_$LC,  KBD3_$LD,  KBD3_$LE,  KBD3_$LF,  KBD3_$F0,
     +           KBD3_$F1,  KBD3_$F2,  KBD3_$F3,  KBD3_$F4,  KBD3_$F5,
     +           KBD3_$F6,  KBD3_$F7,  KBD3_$F8,  KBD3_$F9,  KBD3_$R1,
     +           KBD3_$R2,  KBD3_$R3,  KBD3_$R4,  KBD3_$R5,  KBD3_$R6,
     +           KBD3_$NP0, KBD3_$NP1, KBD3_$NP2, KBD3_$NP3, KBD3_$NP4,
     +           KBD3_$NP5, KBD3_$NP6, KBD3_$NP7, KBD3_$NP8, KBD3_$NP9,
     +           KBD3_$NPA, KBD3_$NPB, KBD3_$NPC, KBD3_$NPD, KBD3_$NPE,
     +           KBD3_$NPF, KBD3_$NPG, KBD3_$NPP, KBD3_$AL , KBD3_$AR ,
     +           KBD3_$SHL, KBD3_$SHR, KBD3_$LCK, KBD3_$CTL, KBD3_$RPT,
     +           KBD3_$TAB, KBD3_$RET, KBD3_$BS , KBD3_$DEL

      parameter (
     + KBD3_$ESC = char(16#00), KBD3_$L1  = char(16#01), 
     + KBD3_$L2  = char(16#02), KBD3_$L3  = char(16#03),
     + KBD3_$L1A = char(16#04), KBD3_$L2A = char(16#05), 
     + KBD3_$L3A = char(16#06), KBD3_$L4  = char(16#07),
     + KBD3_$L5  = char(16#08), KBD3_$L6  = char(16#09), 
     + KBD3_$L7  = char(16#0A), KBD3_$L8  = char(16#0B),
     + KBD3_$L9  = char(16#0C), KBD3_$LA  = char(16#0D), 
     + KBD3_$LB  = char(16#0E), KBD3_$LC  = char(16#0F),
     + KBD3_$LD  = char(16#10), KBD3_$LE  = char(16#11), 
     + KBD3_$LF  = char(16#12), KBD3_$F0  = char(16#13),
     + KBD3_$F1  = char(16#14), KBD3_$F2  = char(16#15), 
     + KBD3_$F3  = char(16#16), KBD3_$F4  = char(16#17),
     + KBD3_$F5  = char(16#18), KBD3_$F6  = char(16#19), 
     + KBD3_$F7  = char(16#1A), KBD3_$F8  = char(16#1B),
     + KBD3_$F9  = char(16#1C), KBD3_$R1  = char(16#1D), 
     + KBD3_$R2  = char(16#1E), KBD3_$R3  = char(16#1F))
      parameter (
     + KBD3_$R4  = char(16#20), KBD3_$R5  = char(16#21), 
     + KBD3_$R6  = char(16#22), KBD3_$NP0 = char(16#23),
     + KBD3_$NP1 = char(16#24), KBD3_$NP2 = char(16#25), 
     + KBD3_$NP3 = char(16#26), KBD3_$NP4 = char(16#27),
     + KBD3_$NP5 = char(16#28), KBD3_$NP6 = char(16#29), 
     + KBD3_$NP7 = char(16#2A), KBD3_$NP8 = char(16#2B),
     + KBD3_$NP9 = char(16#2C), KBD3_$NPA = char(16#2D), 
     + KBD3_$NPB = char(16#2E), KBD3_$NPC = char(16#2F),
     + KBD3_$NPD = char(16#30), KBD3_$NPE = char(16#31), 
     + KBD3_$NPF = char(16#32), KBD3_$NPG = char(16#33),
     + KBD3_$NPP = char(16#34), KBD3_$AL  = char(16#35), 
     + KBD3_$AR  = char(16#36), KBD3_$SHL = char(16#37),
     + KBD3_$SHR = char(16#38), KBD3_$LCK = char(16#39), 
     + KBD3_$CTL = char(16#3A), KBD3_$RPT = char(16#3B),
     + KBD3_$TAB = char(16#3C), KBD3_$RET = char(16#3D), 
     + KBD3_$BS  = char(16#3E), KBD3_$DEL = char(16#3F))
 
C SHIFTED 
      character 
     +  KBD3_$ESCS, KBD3_$L1S,  KBD3_$L2S,  KBD3_$L3S,  KBD3_$L1AS,
     +  KBD3_$L2AS, KBD3_$L3AS, KBD3_$L4S,  KBD3_$L5S,  KBD3_$L6S,
     +  KBD3_$L7S,  KBD3_$L8S,  KBD3_$L9S,  KBD3_$LAS,  KBD3_$LBS,
     +  KBD3_$LCS,  KBD3_$LDS,  KBD3_$LES,  KBD3_$LFS,  KBD3_$F0S,
     +  KBD3_$F1S,  KBD3_$F2S,  KBD3_$F3S,  KBD3_$F4S,  KBD3_$F5S,
     +  KBD3_$F6S,  KBD3_$F7S,  KBD3_$F8S,  KBD3_$F9S,  KBD3_$R1S,
     +  KBD3_$R2S,  KBD3_$R3S,  KBD3_$R4S,  KBD3_$R5S,  KBD3_$R6S,
     +  KBD3_$NP0S, KBD3_$NP1S, KBD3_$NP2S, KBD3_$NP3S, KBD3_$NP4S,
     +  KBD3_$NP5S, KBD3_$NP6S, KBD3_$NP7S, KBD3_$NP8S, KBD3_$NP9S,
     +  KBD3_$NPAS, KBD3_$NPBS, KBD3_$NPCS, KBD3_$NPDS, KBD3_$NPES,
     +  KBD3_$NPFS, KBD3_$NPGS, KBD3_$NPPS, KBD3_$ALS,  KBD3_$ARS,
     +  KBD3_$SHLS, KBD3_$SHRS, KBD3_$LCKS, KBD3_$CTLS, KBD3_$RPTS,
     +  KBD3_$TABS, KBD3_$RETS, KBD3_$BSS,  KBD3_$DELS

      parameter (
     + KBD3_$ESCS = char(16#40), KBD3_$L1S  = char(16#41), 
     + KBD3_$L2S  = char(16#42), KBD3_$L3S  = char(16#43),
     + KBD3_$L1AS = char(16#44), KBD3_$L2AS = char(16#45), 
     + KBD3_$L3AS = char(16#46), KBD3_$L4S  = char(16#47),
     + KBD3_$L5S  = char(16#48), KBD3_$L6S  = char(16#49), 
     + KBD3_$L7S  = char(16#4A), KBD3_$L8S  = char(16#4B),
     + KBD3_$L9S  = char(16#4C), KBD3_$LAS  = char(16#4D), 
     + KBD3_$LBS  = char(16#4E), KBD3_$LCS  = char(16#4F),
     + KBD3_$LDS  = char(16#50), KBD3_$LES  = char(16#51), 
     + KBD3_$LFS  = char(16#52), KBD3_$F0S  = char(16#53),
     + KBD3_$F1S  = char(16#54), KBD3_$F2S  = char(16#55), 
     + KBD3_$F3S  = char(16#56), KBD3_$F4S  = char(16#57),
     + KBD3_$F5S  = char(16#58), KBD3_$F6S  = char(16#59), 
     + KBD3_$F7S  = char(16#5A), KBD3_$F8S  = char(16#5B),
     + KBD3_$F9S  = char(16#5C), KBD3_$R1S  = char(16#5D), 
     + KBD3_$R2S  = char(16#5E), KBD3_$R3S  = char(16#5F))
      parameter (
     + KBD3_$R4S  = char(16#60), KBD3_$R5S  = char(16#61), 
     + KBD3_$R6S  = char(16#62), KBD3_$NP0S = char(16#63),
     + KBD3_$NP1S = char(16#64), KBD3_$NP2S = char(16#65), 
     + KBD3_$NP3S = char(16#66), KBD3_$NP4S = char(16#67),
     + KBD3_$NP5S = char(16#68), KBD3_$NP6S = char(16#69), 
     + KBD3_$NP7S = char(16#6A), KBD3_$NP8S = char(16#6B),
     + KBD3_$NP9S = char(16#6C), KBD3_$NPAS = char(16#6D), 
     + KBD3_$NPBS = char(16#6E), KBD3_$NPCS = char(16#6F),
     + KBD3_$NPDS = char(16#70), KBD3_$NPES = char(16#71), 
     + KBD3_$NPFS = char(16#72), KBD3_$NPGS = char(16#73),
     + KBD3_$NPPS = char(16#74), KBD3_$ALS  = char(16#75), 
     + KBD3_$ARS  = char(16#76), KBD3_$SHLS = char(16#77),
     + KBD3_$SHRS = char(16#78), KBD3_$LCKS = char(16#79), 
     + KBD3_$CTLS = char(16#7A), KBD3_$RPTS = char(16#7B),
     + KBD3_$TABS = char(16#7C), KBD3_$RETS = char(16#7D), 
     + KBD3_$BSS  = char(16#7E), KBD3_$DELS = char(16#7F))

C CONTROL
      character   
     + KBD3_$ESCC, KBD3_$L1C,  KBD3_$L2C,  KBD3_$L3C,  KBD3_$L1AC,
     + KBD3_$L2AC, KBD3_$L3AC, KBD3_$L4C,  KBD3_$L5C,  KBD3_$L6C,
     + KBD3_$L7C,  KBD3_$L8C,  KBD3_$L9C,  KBD3_$LAC,  KBD3_$LBC,
     + KBD3_$LCC,  KBD3_$LDC,  KBD3_$LEC,  KBD3_$LFC,  KBD3_$F0C,
     + KBD3_$F1C,  KBD3_$F2C,  KBD3_$F3C,  KBD3_$F4C,  KBD3_$F5C,
     + KBD3_$F6C,  KBD3_$F7C,  KBD3_$F8C,  KBD3_$F9C,  KBD3_$R1C,
     + KBD3_$R2C,  KBD3_$R3C,  KBD3_$R4C,  KBD3_$R5C,  KBD3_$R6C,
     + KBD3_$NP0C, KBD3_$NP1C, KBD3_$NP2C, KBD3_$NP3C, KBD3_$NP4C,
     + KBD3_$NP5C, KBD3_$NP6C, KBD3_$NP7C, KBD3_$NP8C, KBD3_$NP9C,
     + KBD3_$NPAC, KBD3_$NPBC, KBD3_$NPCC, KBD3_$NPDC, KBD3_$NPEC,
     + KBD3_$NPFC, KBD3_$NPGC, KBD3_$NPPC, KBD3_$ALC,  KBD3_$ARC,
     + KBD3_$SHLC, KBD3_$SHRC, KBD3_$LCKC, KBD3_$CTLC, KBD3_$RPTC,
     + KBD3_$TABC, KBD3_$RETC, KBD3_$BSC,  KBD3_$DELC

      parameter (
     + KBD3_$ESCC = char(16#80), KBD3_$L1C  = char(16#81), 
     + KBD3_$L2C  = char(16#82), KBD3_$L3C  = char(16#83),
     + KBD3_$L1AC = char(16#84), KBD3_$L2AC = char(16#85), 
     + KBD3_$L3AC = char(16#86), KBD3_$L4C  = char(16#87),
     + KBD3_$L5C  = char(16#88), KBD3_$L6C  = char(16#89), 
     + KBD3_$L7C  = char(16#8A), KBD3_$L8C  = char(16#8B),
     + KBD3_$L9C  = char(16#8C), KBD3_$LAC  = char(16#8D), 
     + KBD3_$LBC  = char(16#8E), KBD3_$LCC  = char(16#8F),
     + KBD3_$LDC  = char(16#90), KBD3_$LEC  = char(16#91), 
     + KBD3_$LFC  = char(16#92), KBD3_$F0C  = char(16#93),
     + KBD3_$F1C  = char(16#94), KBD3_$F2C  = char(16#95), 
     + KBD3_$F3C  = char(16#96), KBD3_$F4C  = char(16#97),
     + KBD3_$F5C  = char(16#98), KBD3_$F6C  = char(16#99), 
     + KBD3_$F7C  = char(16#9A), KBD3_$F8C  = char(16#9B),
     + KBD3_$F9C  = char(16#9C), KBD3_$R1C  = char(16#9D), 
     + KBD3_$R2C  = char(16#9E), KBD3_$R3C  = char(16#9F))
      parameter (
     + KBD3_$R4C  = char(16#A0), KBD3_$R5C  = char(16#A1), 
     + KBD3_$R6C  = char(16#A2), KBD3_$NP0C = char(16#A3),
     + KBD3_$NP1C = char(16#A4), KBD3_$NP2C = char(16#A5), 
     + KBD3_$NP3C = char(16#A6), KBD3_$NP4C = char(16#A7),
     + KBD3_$NP5C = char(16#A8), KBD3_$NP6C = char(16#A9), 
     + KBD3_$NP7C = char(16#AA), KBD3_$NP8C = char(16#AB),
     + KBD3_$NP9C = char(16#AC), KBD3_$NPAC = char(16#AD), 
     + KBD3_$NPBC = char(16#AE), KBD3_$NPCC = char(16#AF),
     + KBD3_$NPDC = char(16#B0), KBD3_$NPEC = char(16#B1), 
     + KBD3_$NPFC = char(16#B2), KBD3_$NPGC = char(16#B3),
     + KBD3_$NPPC = char(16#B4), KBD3_$ALC  = char(16#B5), 
     + KBD3_$ARC  = char(16#B6), KBD3_$SHLC = char(16#B7),
     + KBD3_$SHRC = char(16#B8), KBD3_$LCKC = char(16#B9), 
     + KBD3_$CTLC = char(16#BA), KBD3_$RPTC = char(16#BB),
     + KBD3_$TABC = char(16#BC), KBD3_$RETC = char(16#BD), 
     + KBD3_$BSC  = char(16#BE), KBD3_$DELC = char(16#BF))

C UP
      character   
     + KBD3_$ESCU, KBD3_$L1U,  KBD3_$L2U,  KBD3_$L3U,  KBD3_$L1AU,
     + KBD3_$L2AU, KBD3_$L3AU, KBD3_$L4U,  KBD3_$L5U,  KBD3_$L6U,
     + KBD3_$L7U,  KBD3_$L8U,  KBD3_$L9U,  KBD3_$LAU,  KBD3_$LBU,
     + KBD3_$LCU,  KBD3_$LDU,  KBD3_$LEU,  KBD3_$LFU,  KBD3_$F0U,
     + KBD3_$F1U,  KBD3_$F2U,  KBD3_$F3U,  KBD3_$F4U,  KBD3_$F5U,
     + KBD3_$F6U,  KBD3_$F7U,  KBD3_$F8U,  KBD3_$F9U,  KBD3_$R1U,
     + KBD3_$R2U,  KBD3_$R3U,  KBD3_$R4U,  KBD3_$R5U,  KBD3_$R6U,
     + KBD3_$NP0U, KBD3_$NP1U, KBD3_$NP2U, KBD3_$NP3U, KBD3_$NP4U,
     + KBD3_$NP5U, KBD3_$NP6U, KBD3_$NP7U, KBD3_$NP8U, KBD3_$NP9U,
     + KBD3_$NPAU, KBD3_$NPBU, KBD3_$NPCU, KBD3_$NPDU, KBD3_$NPEU,
     + KBD3_$NPFU, KBD3_$NPGU, KBD3_$NPPU, KBD3_$ALU,  KBD3_$ARU,
     + KBD3_$SHLU, KBD3_$SHRU, KBD3_$LCKU, KBD3_$CTLU, KBD3_$RPTU,
     + KBD3_$TABU, KBD3_$RETU, KBD3_$BSU,  KBD3_$DELU

      parameter (
     + KBD3_$ESCU = char(16#C0), KBD3_$L1U  = char(16#C1), 
     + KBD3_$L2U  = char(16#C2), KBD3_$L3U  = char(16#C3),
     + KBD3_$L1AU = char(16#C4), KBD3_$L2AU = char(16#C5), 
     + KBD3_$L3AU = char(16#C6), KBD3_$L4U  = char(16#C7),
     + KBD3_$L5U  = char(16#C8), KBD3_$L6U  = char(16#C9), 
     + KBD3_$L7U  = char(16#CA), KBD3_$L8U  = char(16#CB),
     + KBD3_$L9U  = char(16#CC), KBD3_$LAU  = char(16#CD), 
     + KBD3_$LBU  = char(16#CE), KBD3_$LCU  = char(16#CF),
     + KBD3_$LDU  = char(16#D0), KBD3_$LEU  = char(16#D1), 
     + KBD3_$LFU  = char(16#D2), KBD3_$F0U  = char(16#D3),
     + KBD3_$F1U  = char(16#D4), KBD3_$F2U  = char(16#D5), 
     + KBD3_$F3U  = char(16#D6), KBD3_$F4U  = char(16#D7),
     + KBD3_$F5U  = char(16#D8), KBD3_$F6U  = char(16#D9), 
     + KBD3_$F7U  = char(16#DA), KBD3_$F8U  = char(16#DB),
     + KBD3_$F9U  = char(16#DC), KBD3_$R1U  = char(16#DD), 
     + KBD3_$R2U  = char(16#DE), KBD3_$R3U  = char(16#DF))
      parameter (
     + KBD3_$R4U  = char(16#E0), KBD3_$R5U  = char(16#E1), 
     + KBD3_$R6U  = char(16#E2), KBD3_$NP0U = char(16#E3),
     + KBD3_$NP1U = char(16#E4), KBD3_$NP2U = char(16#E5), 
     + KBD3_$NP3U = char(16#E6), KBD3_$NP4U = char(16#E7),
     + KBD3_$NP5U = char(16#E8), KBD3_$NP6U = char(16#E9), 
     + KBD3_$NP7U = char(16#EA), KBD3_$NP8U = char(16#EB),
     + KBD3_$NP9U = char(16#EC), KBD3_$NPAU = char(16#ED), 
     + KBD3_$NPBU = char(16#EE), KBD3_$NPCU = char(16#EF),
     + KBD3_$NPDU = char(16#F0), KBD3_$NPEU = char(16#F1), 
     + KBD3_$NPFU = char(16#F2), KBD3_$NPGU = char(16#F3),
     + KBD3_$NPPU = char(16#F4), KBD3_$ALU  = char(16#F5), 
     + KBD3_$ARU  = char(16#F6), KBD3_$SHLU = char(16#F7),
     + KBD3_$SHRU = char(16#F8), KBD3_$LCKU = char(16#F9), 
     + KBD3_$CTLU = char(16#FA), KBD3_$RPTU = char(16#FB),
     + KBD3_$TABU = char(16#FC), KBD3_$RETU = char(16#FD), 
     + KBD3_$BSU  = char(16#FE), KBD3_$DELU = char(16#FF))

C------------------------------------------------------------
C Physical key codes - The following codes are used with the GPR  
C event type "gpr_$physical_keys". There are only DOWN codes and  
C UP codes - no modifier codes.  

C Physical key UP transition is the DOWN code plus KBD3_$KEY_UP                                                         

      character KBD3_$KEY_UP

      parameter (
     + KBD3_$KEY_UP  = char(16#80))
                                    
C The codes corresponding to thefunction key DOWN transitions are 
C identical to the codes used for "gpr_$function_keys" DOWN 
C transitions, i.e.  KBD3_$ESC..KBD3_$DEL 

                                                                 
C The "white" key names use the ISO identification scheme.        

      character   
     + KBD3_$B00, KBD3_$B01, KBD3_$B02, KBD3_$B03,
     + KBD3_$B04, KBD3_$B05, KBD3_$B06, KBD3_$B07,
     + KBD3_$B08, KBD3_$B09, KBD3_$B10, KBD3_$SP,
                
     + KBD3_$C01, KBD3_$C02, KBD3_$C03, KBD3_$C04,
     + KBD3_$C05, KBD3_$C06, KBD3_$C07, KBD3_$C08,
     + KBD3_$C09, KBD3_$C10, KBD3_$C11, KBD3_$C12,
                
     + KBD3_$D01, KBD3_$D02, KBD3_$D03, KBD3_$D04,
     + KBD3_$D05, KBD3_$D06, KBD3_$D07, KBD3_$D08,
     + KBD3_$D09, KBD3_$D10, KBD3_$D11, KBD3_$D12,
                
     + KBD3_$E00, KBD3_$E01, KBD3_$E02, KBD3_$E03,
     + KBD3_$E04, KBD3_$E05, KBD3_$E06, KBD3_$E07,
     + KBD3_$E08, KBD3_$E09, KBD3_$E10, KBD3_$E11,
     + KBD3_$E12, KBD3_$E13

      parameter (
     + KBD3_$B00 = char(16#40), KBD3_$B01 = char(16#41), 
     + KBD3_$B02 = char(16#42), KBD3_$B03 = char(16#43),
     + KBD3_$B04 = char(16#44), KBD3_$B05 = char(16#45),
     + KBD3_$B06 = char(16#46), KBD3_$B07 = char(16#47),
     + KBD3_$B08 = char(16#48), KBD3_$B09 = char(16#49),
     + KBD3_$B10 = char(16#4A), KBD3_$SP  = char(16#4D),
                     
     + KBD3_$C01 = char(16#51), KBD3_$C02 = char(16#52),
     + KBD3_$C03 = char(16#53), KBD3_$C04 = char(16#54),
     + KBD3_$C05 = char(16#55), KBD3_$C06 = char(16#56),
     + KBD3_$C07 = char(16#57), KBD3_$C08 = char(16#58),
     + KBD3_$C09 = char(16#59), KBD3_$C10 = char(16#5A),
     + KBD3_$C11 = char(16#5B), KBD3_$C12 = char(16#5C))
                     
      parameter (
     + KBD3_$D01 = char(16#61), KBD3_$D02 = char(16#62),
     + KBD3_$D03 = char(16#63), KBD3_$D04 = char(16#64),
     + KBD3_$D05 = char(16#65), KBD3_$D06 = char(16#66),
     + KBD3_$D07 = char(16#67), KBD3_$D08 = char(16#68),
     + KBD3_$D09 = char(16#69), KBD3_$D10 = char(16#6A),
     + KBD3_$D11 = char(16#6B), KBD3_$D12 = char(16#6C),
                     
     + KBD3_$E00 = char(16#70), KBD3_$E01 = char(16#71),
     + KBD3_$E02 = char(16#72), KBD3_$E03 = char(16#73),
     + KBD3_$E04 = char(16#74), KBD3_$E05 = char(16#75),
     + KBD3_$E06 = char(16#76), KBD3_$E07 = char(16#77),
     + KBD3_$E08 = char(16#78), KBD3_$E09 = char(16#79),
     + KBD3_$E10 = char(16#7A), KBD3_$E11 = char(16#7B),
     + KBD3_$E12 = char(16#7C), KBD3_$E13 = char(16#7D))


C------------------------------------------------------------
C  mouse buttons - used with gpr_$buttons
C  up/down codes the same as KBD_$ codes
     
      character  KBD3_$M1D, KBD3_$M1S, KBD3_$M1C, KBD3_$M1U,
     +           KBD3_$M2D, KBD3_$M2S, KBD3_$M2C, KBD3_$M2U,
     +           KBD3_$M3D, KBD3_$M3S, KBD3_$M3C, KBD3_$M3U,
     +           KBD3_$M4D, KBD3_$M4S, KBD3_$M4C, KBD3_$M4U

      parameter (
     +  KBD3_$M1D = char(16#61),
     +  KBD3_$M1S = char(16#21), 
     +  KBD3_$M1C = char(16#01),
     +  KBD3_$M1U = char(16#41),
     +  KBD3_$M2D = char(16#62),  
     +  KBD3_$M2S = char(16#22),  
     +  KBD3_$M2C = char(16#02), 
     +  KBD3_$M2U = char(16#42), 
     +  KBD3_$M3D = char(16#63), 
     +  KBD3_$M3S = char(16#23), 
     +  KBD3_$M3C = char(16#03), 
     +  KBD3_$M3U = char(16#43), 
     +  KBD3_$M4D = char(16#64), 
     +  KBD3_$M4S = char(16#24), 
     +  KBD3_$M4C = char(16#04), 
     +  KBD3_$M4U = char(16#44)) 


