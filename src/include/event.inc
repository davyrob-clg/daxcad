C
C     @(#)  412.1 date 6/11/92 event.inc 
C
C
C     Filename    : event.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:11
C     Last change : 92/06/11 14:29:31
C
C     Copyright : Practical Technology International Limited  
C     File :- event.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     Include file code for eventing types
C
C
C         12   A return key has been hit in the input device all data in buffers
C         11   A non button or input mouse button hit
C         13   A hit has been made but no notify activity
C         14   Buffer will contain data needed from tty window evnum will contain ttywindow number
C         15   Buffer will contain data from key hit value is in integer ascii
C         16   Can return a locator event if signaled to do so
C         17   A process event has been signaled data is returned
C
C     DAX_EVENT_RETURN  12
C     DAX_EVENT_BUTTON  11
C     DAX_EVENT_NULL  13
C     DAX_EVENT_TTY 14
C     DAX_EVENT_KEY 15
C     DAX_EVENT_LOACATOR 16
C     DAX_EVENT_PROCESS 17
C     DAX_EVENT_TIMEOUT 18
C     DAX_EVENT_LABEL   19
C     
C     Button hits are defined as follows. Currently Apollo returns
C     these contain the ascii values and a button number will be returned
C     
C     BUTTON_1_DOWN 'a' 97
C     BUTTON_2_DOWN 'b' 98
C     BUTTON_3_DOWN 'c' 99
C     BUTTON_1_UP 'A' 65
C     BUTTON_2_UP 'B' 66
C     BUTTON_3_UP 'C' 67
C

      INTEGER*4 DAX_TYPE_WAIT      
      INTEGER*4 DAX_TYPE_TIMEOUT   
      INTEGER*4 DAX_EVENT_RETURN
      INTEGER*4 DAX_EVENT_BUTTON
      INTEGER*4 DAX_EVENT_NULL
      INTEGER*4 DAX_EVENT_TTY
      INTEGER*4 DAX_EVENT_KEY
      INTEGER*4 DAX_EVENT_LOCATOR
      INTEGER*4 DAX_EVENT_PROCESS
      INTEGER*4 DAX_EVENT_TIMEOUT
      INTEGER*4 DAX_EVENT_LABEL
      INTEGER*4 DAX_EVENT_CURSORUP
      INTEGER*4 DAX_EVENT_CURSORDN
      INTEGER*4 DAX_EVENT_INPUT
      INTEGER*4 DAX_EVENT_CURSORLEFT
      INTEGER*4 DAX_EVENT_CURSORRIGHT
      INTEGER*4 BUTTON_1_DOWN
      INTEGER*4 BUTTON_2_DOWN
      INTEGER*4 BUTTON_3_DOWN
      INTEGER*4 BUTTON_1_UP
      INTEGER*4 BUTTON_2_UP
      INTEGER*4 BUTTON_3_UP
C
      PARAMETER( DAX_EVENT_RETURN=12,
     +           DAX_EVENT_BUTTON=11,
     +           DAX_EVENT_NULL=13,
     +           DAX_EVENT_TTY=14,
     +           DAX_EVENT_KEY=15,
     +           DAX_EVENT_LOCATOR=16,
     +           DAX_EVENT_PROCESS=17,
     +           DAX_EVENT_TIMEOUT=18,
     +           DAX_EVENT_LABEL=19,
     +           DAX_EVENT_CURSORUP=20,
     +           DAX_EVENT_CURSORDN=21,
     +           DAX_EVENT_INPUT=22,
     +           DAX_EVENT_CURSORLEFT=23,
     +           DAX_EVENT_CURSORRIGHT=24,
     +           DAX_TYPE_WAIT = 1,
     +           DAX_TYPE_TIMEOUT = 2)


      PARAMETER( BUTTON_1_DOWN = 97,
     +           BUTTON_2_DOWN = 98,
     +           BUTTON_3_DOWN = 99,
     +           BUTTON_1_UP = 65,
     +           BUTTON_2_UP = 66,
     +           BUTTON_3_UP = 67)



