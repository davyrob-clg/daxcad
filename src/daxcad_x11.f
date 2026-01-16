C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 daxcad_x11.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE ACRDIS(II)
C     SUBROUTINE POPPD1(FILNAM,WNORGX,WNORGY,WNWID,WNHGT)
C     SUBROUTINE BELL()
C     SUBROUTINE SNAQR()
C     SUBROUTINE SNREL()
C     SUBROUTINE TITLE(SCREEN,TEXT)
C
C     |-----------------------------------------------------------------|
C
C     All routines are X dependant and have equivalent routines
C     For the apollo based version.
C
      SUBROUTINE ACRDIS(II)
C     =====================
C1    VARYPE            I2
C1    IOSTAT            I
C
C2    Acquire the display II number of times
C2    NOT NEEDED UNDER X11
C2  
C2    Arguments:-
C2  
C2    II          ->          Number of time to acquire
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      INTEGER*2 II
      END
C
C
      SUBROUTINE XPOPPD1(FILNAM,WNORGX,WNORGY,WNWID,WNHGT)
C     ===================================================
C1    VARTYPE            C*(*)  I4      I4    I4    I4
C1    IOSTATUS             I    I       I     I     I
C
C2    Subroutine POPPD1 pops a pad on the APOLLO screen
C2    showing the text in the file FILNAM.The top left
C2    of the pad is located at screen coordinates
C2    WNORGX,WNORGY and is of width WNWID and height
C2    WNHGT.
C2  
C2    This Is an X BASED version Must have another one for the apollo
C2
C2  
C2    Arguments:-
C2  
C2    FILNAM      ->      File to display information.
C2    WNORGX      ->      Origin X of file
C2    WNORGY      ->      Origin Y of file
C2    WNWID       ->      Width of window
C2    WNGHT       ->      Height of window
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
C
      include  'include/macro.inc'
      include  'include/daxcad_x.inc'
C
      INTEGER*4 ST
      INTEGER*4 WNORGX
      INTEGER*4 WNORGY
      INTEGER*4 WNWID
      INTEGER*4 WNHGT
      INTEGER*4 NLEN
      INTEGER*4 FNL
      INTEGER*2 WINPOS(4)
C
      CHARACTER*(*) FILNAM
C
      EXTERNAL NLEN
C
C     **************************
C     DEBUG BIT MUST RESET LATER
C     **************************
C
      XVERSION = .TRUE.
C
      IF ( MACOP ) THEN
C         Macro is in operation don`t  pop window
          RETURN
      ENDIF
C
C     set top left pos for window
      WINPOS(1)=WNORGY
      WINPOS(2)=WNORGX
C     set window width and height
      WINPOS(3)=WNWID
      WINPOS(4)=WNHGT
C
C     find length of file name
      FNL=NLEN(FILNAM)
      CALL popawindow(FILNAM,FNL,WINPOS,ST)
C
      END
C
      SUBROUTINE BELL()
C     =================
C1    VARYPE     NONE       
C1    IOSTAT            
C
C2    Rings a bell for a short time
C2  
C2  
C2    Arguments:-NONE
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:NONE
C2  
C2  
      CALL GPRX_XBELL()
      END



      SUBROUTINE SNAQR()
C     ==================
C1    VARYPE       NONE    
C1    IOSTAT       NONE  
C
C2    This routine will allow a batching mode to occur. It must
C2    be followed by SNREL to reset to normal. This is in X systems
C2    for NON hidden bitmap updating. This routine will
C2    turn of Hidden updating if active. SNREL turns it basck on
C2    and forces update of hidden bitmap. This is for performance reasona
C2  
C2  
C2    Arguments:-
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      INTEGER*4 ST


      CALL GPR_$SET_AUTO_UPDATE(.FALSE.,ST)

      END
  
      SUBROUTINE SNREL()
C     ==================
C1    VARYPE       NONE    
C1    IOSTAT       NONE  
C
C2    This routyine forces the current drawing area to be
C2    updated in hidden memory SNAQR must be called first
C2  
C2  
C2    Arguments:-
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      INTEGER*4 ST


      CALL GPR_$FORCE_HIDDEN_UPDATE(ST)
      CALL GPR_$SET_AUTO_UPDATE(.TRUE.,ST)

      END
C
C
      SUBROUTINE TITLE(SCREEN,TEXT)
C     =============================
C1    VARYPE             I4   C*(*)
C1    IOSTAT             I      I
C
C2    This routine generates a TEXT header for and X window for the
C2    any window generated under DAXCAD.
C2    For subwindows it uses SCREEN to determine the window.
C2  
C2    Arguments:-
C2  
C2    SCREEN          ->          GPR bitmap display number.
C2    TEXT            ->          Text to be displayed.
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  
      include  'include/daxcad_x.inc'
      include  'include/product.inc'
C
      INTEGER*4 SCREEN
      INTEGER*4 NLEN
      INTEGER*4 LENGTH
      CHARACTER*(*) TEXT
      CHARACTER*1024 HEADER
C
      IF(SCREEN.EQ.0) THEN
C         Main display some kind of name and Copyright
          WRITE(HEADER,FMT='(2A)' ) 
     +'Practical Technology 2020 '

      ENDIF


C
      END

      SUBROUTINE LOADAPCURSOR(ST)
C     ===========================
C1    VARYPE                  I4
C1    IOSTAT                  O
C
C2  
C2  
C2  
C2    Arguments:-
C2  
C2    NONE
C2  
C2  
C2  
C2    Error Returns: APOLLO SPECIFIC
C2  
C2    This routine allocates a pattern bitmap
C2    for a good system cursor dont blame me if its not the same as the original
C
C2
C2    This is a stub for UNIX not needed
C2
      END


      SUBROUTINE SCREENSIZE(PLANES,ROOT)
C     ==================================
C1    VARYPE                  I4   I4(2)
C1    IOSTAT                  O    O
C
C2    Gets the Apollo defintions of a screen thru a GPR call
C2  
C2  
C2    Arguments:-
C2  
C2    PLANES      ->      The number of color planes available
C2    ROOT        ->      The size of the root window
C2  
C2  
C2    Error Returns:NONE
C2  
C2  
C
      include 'include/gpr.ins.inc'
C
      INTEGER*4 PLANES
      INTEGER*4 ROOT(2)
      INTEGER*4 BLACK
      INTEGER*4 WHITE
      INTEGER*4 ST
      INTEGER*2 BMSIZE(2)
      INTEGER*2 HIPLAN
C

      CALL GPR_$INQ_ROOTWINDOW(BMSIZE,HIPLAN,WHITE,BLACK,ST)
	  
      PLANES = HIPLAN + 1
      ROOT(1) = BMSIZE(1)
      ROOT(2) = BMSIZE(2)

      END

      SUBROUTINE RESIZE_WINDOW(PADNUM,BITMAP,WINDOW,ST)
C     =================================================
C1    VARYPE                             I4    I4(2) I4
C1    IOSTAT                             I       I   O
C
C2    Resize a window. In GPR this can only appply to bitmaps
C2    In GPRX this can apply to both
C2    Thus this one allows the pad to be resized???
C2    Must completely shut down graphics 
C2  
C2    Arguments:-
C2  
C2    PADNUM	->	The pad number ( GPR ONLY )
C2    BITMAP	->	GPR/GPRX bitmap number 
C2    WINDOW	->	The new size of the window
C2  
C2    Error Returns:
C2  
C2    NONE
C2  
C2  
      include 'include/apollo.inc'
C
      INTEGER*2 PADNUM
      INTEGER*4 WINDOW(2)
      INTEGER*2 WIN(4)
      INTEGER*2 POS(2)
      INTEGER*2 PLANE 
      INTEGER*2 OSIZE(2)
      INTEGER*2 NSIZE(2)
      INTEGER*4 BITMAP
      INTEGER*4 COUNT
      INTEGER*4 ST
C
      CALL GPR_$INQ_BITMAP_START(DISPDE,POS,ST)
      CALL GPR_$INQ_BITMAP_DIMENSIONS(DISPDE,OSIZE,PLANE,ST)

C     set window to new size
      WIN(1) = POS(1)
      WIN(2) = POS(2)
      WIN(3) = WINDOW(1)
      WIN(4) = WINDOW(2)
C
      CALL GPR_$SET_BITMAP_WINDOW(DISPDE,WIN,ST)
      CALL GPR_$SERVER_FLUSH_X(ST)

      CALL GPR_$INQ_BITMAP_DIMENSIONS(DISPDE,NSIZE,PLANE,ST)
      CALL GPR_$COLLECT_EXPOSURES(COUNT,ST)

C
C     Set flag based on actual window resize
C
      REPAINTPENDING = OSIZE(1).NE.WINDOW(1).OR.
     +                 OSIZE(2).NE.WINDOW(2)
C
      END





  
      SUBROUTINE KENBLE()
      END

      SUBROUTINE GINITX(INTWIN,REPAINT,ST)
C     =====================================
C1    VARYPE            
C1    IOSTAT            
C
C2  
C2  
C2  
C2    Arguments:-
C2  
C2  
C2  
C2  
C2  
C2    Error Returns:
C2  
C2  
C2  
C2  

      INTEGER*4 INTWIN(4)
      LOGICAL REPAINT
      INTEGER*4 ST
      INTEGER*2 II

      CALL GINIT(II)
      END


      subroutine tpad_$set_mode()
      end

      subroutine tpad_$inquire()
      end

      subroutine pad_$create_window()
      end

      subroutine pad_$set_auto_close()
      end

      subroutine pad_$def_pfk()
      end

      subroutine enableapollokeys()
      end

      subroutine plotepson()

      call eprint('Not available under X windows')
      end
