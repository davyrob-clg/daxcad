C
C     @(#)  412.1 date 6/11/92 daxcad_x.inc 
C
C
C     Filename    : daxcad_x.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:03
C     Last change : 92/06/11 14:27:31
C
C     Copyright : Practical Technology Limited  
C     File :- daxcad_x.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C     Include file for X based Operations on DAXCAD
C
C
C     XVERSION           ->           Logical to indicate Xversion is running
C     MAINWINDOW         ->           Current size of window dimensions are relative to root window
C     INPUTSTATE         ->           The current input state. Either cursor typed input or a popup
C     MAINCURSOR         ->           The main cross hair cursor is active AND drawn on the screen
C     POPUPACTIVE        ->           A popup is on the screen and active.
C     TYPEDINPUT         ->           Text is being typed in.
C     RUBBERBAND         ->           The rubberbanding cursor is active.
C
      INTEGER*4 MAINCURSOR
      INTEGER*4 POPUPACTIVE
      INTEGER*4 TYPEDINPUT
      INTEGER*4 RUBBERBAND
      INTEGER*4 CURSOROUT

      PARAMETER ( MAINCURSOR  =  1  )
      PARAMETER ( POPUPACTIVE =  2  )
      PARAMETER ( RUBBERBAND  =  3  )
      PARAMETER ( TYPEDINPUT  =  4  )
      PARAMETER ( CURSOROUT   =  5  )

      LOGICAL XVERSION
      INTEGER*4 MAINWINDOW(4)
      INTEGER*4 INPUTSTATE
C
      COMMON/XDAXCAD/XVERSION,
     +               INPUTSTATE,
     +               MAINWINDOW
C
C
C     X Cursor defintions pulled from /usr/include/X11/cursorfont.h
C
C
      INTEGER*4 XC_num_glyphs 
      INTEGER*4 XC_X_cursor 
      INTEGER*4 XC_arrow 
      INTEGER*4 XC_based_arrow_down 
      INTEGER*4 XC_based_arrow_up 
      INTEGER*4 XC_boat 
      INTEGER*4 XC_bogosity 
      INTEGER*4 XC_bottom_left_corner 
      INTEGER*4 XC_bottom_right_corner 
      INTEGER*4 XC_bottom_side 
      INTEGER*4 XC_bottom_tee 
      INTEGER*4 XC_box_spiral 
      INTEGER*4 XC_center_ptr 
      INTEGER*4 XC_circle 
      INTEGER*4 XC_clock 
      INTEGER*4 XC_coffee_mug 
      INTEGER*4 XC_cross 
      INTEGER*4 XC_cross_reverse 
      INTEGER*4 XC_crosshair 
      INTEGER*4 XC_diamond_cross 
      INTEGER*4 XC_dot 
      INTEGER*4 XC_dotbox 
      INTEGER*4 XC_double_arrow 
      INTEGER*4 XC_draft_large 
      INTEGER*4 XC_draft_small 
      INTEGER*4 XC_draped_box 
      INTEGER*4 XC_exchange 
      INTEGER*4 XC_fleur 
      INTEGER*4 XC_gobbler 
      INTEGER*4 XC_gumby 
      INTEGER*4 XC_hand1 
      INTEGER*4 XC_hand2 
      INTEGER*4 XC_heart 
      INTEGER*4 XC_icon 
      INTEGER*4 XC_iron_cross 
      INTEGER*4 XC_left_ptr 
      INTEGER*4 XC_left_side 
      INTEGER*4 XC_left_tee 
      INTEGER*4 XC_leftbutton 
      INTEGER*4 XC_ll_angle 
      INTEGER*4 XC_lr_angle 
      INTEGER*4 XC_man 
      INTEGER*4 XC_middlebutton 
      INTEGER*4 XC_mouse 
      INTEGER*4 XC_pencil 
      INTEGER*4 XC_pirate 
      INTEGER*4 XC_plus 
      INTEGER*4 XC_question_arrow 
      INTEGER*4 XC_right_ptr 
      INTEGER*4 XC_right_side 
      INTEGER*4 XC_right_tee 
      INTEGER*4 XC_rightbutton 
      INTEGER*4 XC_rtl_logo 
      INTEGER*4 XC_sailboat 
      INTEGER*4 XC_sb_down_arrow 
      INTEGER*4 XC_sb_h_double_arrow 
      INTEGER*4 XC_sb_left_arrow 
      INTEGER*4 XC_sb_right_arrow 
      INTEGER*4 XC_sb_up_arrow 
      INTEGER*4 XC_sb_v_double_arrow 
      INTEGER*4 XC_shuttle 
      INTEGER*4 XC_sizing 
      INTEGER*4 XC_spider 
      INTEGER*4 XC_spraycan 
      INTEGER*4 XC_star 
      INTEGER*4 XC_target 
      INTEGER*4 XC_tcross 
      INTEGER*4 XC_top_left_arrow 
      INTEGER*4 XC_top_left_corner 
      INTEGER*4 XC_top_right_corner 
      INTEGER*4 XC_top_side 
      INTEGER*4 XC_top_tee 
      INTEGER*4 XC_trek 
      INTEGER*4 XC_ul_angle 
      INTEGER*4 XC_umbrella 
      INTEGER*4 XC_ur_angle 
      INTEGER*4 XC_watch 
      INTEGER*4 XC_xterm 

      PARAMETER (  XC_num_glyphs = 154 )
      PARAMETER (  XC_X_cursor = 0 )
      PARAMETER (  XC_arrow = 2 )
      PARAMETER (  XC_based_arrow_down = 4 )
      PARAMETER (  XC_based_arrow_up = 6 )
      PARAMETER (  XC_boat = 8 )
      PARAMETER (  XC_bogosity = 10 )
      PARAMETER (  XC_bottom_left_corner = 12 )
      PARAMETER (  XC_bottom_right_corner = 14 )
      PARAMETER (  XC_bottom_side = 16 )
      PARAMETER (  XC_bottom_tee = 18 )
      PARAMETER (  XC_box_spiral = 20 )
      PARAMETER (  XC_center_ptr = 22 )
      PARAMETER (  XC_circle = 24 )
      PARAMETER (  XC_clock = 26 )
      PARAMETER (  XC_coffee_mug = 28 )
      PARAMETER (  XC_cross = 30 )
      PARAMETER (  XC_cross_reverse = 32 )
      PARAMETER (  XC_crosshair = 34 )
      PARAMETER (  XC_diamond_cross = 36 )
      PARAMETER (  XC_dot = 38 )
      PARAMETER (  XC_dotbox = 40 )
      PARAMETER (  XC_double_arrow = 42 )
      PARAMETER (  XC_draft_large = 44 )
      PARAMETER (  XC_draft_small = 46 )
      PARAMETER (  XC_draped_box = 48 )
      PARAMETER (  XC_exchange = 50 )
      PARAMETER (  XC_fleur = 52 )
      PARAMETER (  XC_gobbler = 54 )
      PARAMETER (  XC_gumby = 56 )
      PARAMETER (  XC_hand1 = 58 )
      PARAMETER (  XC_hand2 = 60 )
      PARAMETER (  XC_heart = 62 )
      PARAMETER (  XC_icon = 64 )
      PARAMETER (  XC_iron_cross = 66 )
      PARAMETER (  XC_left_ptr = 68 )
      PARAMETER (  XC_left_side = 70 )
      PARAMETER (  XC_left_tee = 72 )
      PARAMETER (  XC_leftbutton = 74 )
      PARAMETER (  XC_ll_angle = 76 )
      PARAMETER (  XC_lr_angle = 78 )
      PARAMETER (  XC_man = 80 )
      PARAMETER (  XC_middlebutton = 82 )
      PARAMETER (  XC_mouse = 84 )
      PARAMETER (  XC_pencil = 86 )
      PARAMETER (  XC_pirate = 88 )
      PARAMETER (  XC_plus = 90 )
      PARAMETER (  XC_question_arrow = 92 )
      PARAMETER (  XC_right_ptr = 94 )
      PARAMETER (  XC_right_side = 96 )
      PARAMETER (  XC_right_tee = 98 )
      PARAMETER (  XC_rightbutton = 100 )
      PARAMETER (  XC_rtl_logo = 102 )
      PARAMETER (  XC_sailboat = 104 )
      PARAMETER (  XC_sb_down_arrow = 106 )
      PARAMETER (  XC_sb_h_double_arrow = 108 )
      PARAMETER (  XC_sb_left_arrow = 110 )
      PARAMETER (  XC_sb_right_arrow = 112 )
      PARAMETER (  XC_sb_up_arrow = 114 )
      PARAMETER (  XC_sb_v_double_arrow = 116 )
      PARAMETER (  XC_shuttle = 118 )
      PARAMETER (  XC_sizing = 120 )
      PARAMETER (  XC_spider = 122 )
      PARAMETER (  XC_spraycan = 124 )
      PARAMETER (  XC_star = 126 )
      PARAMETER (  XC_target = 128 )
      PARAMETER (  XC_tcross = 130 )
      PARAMETER (  XC_top_left_arrow = 132 )
      PARAMETER (  XC_top_left_corner = 134 )
      PARAMETER (  XC_top_right_corner = 136 )
      PARAMETER (  XC_top_side = 138 )
      PARAMETER (  XC_top_tee = 140 )
      PARAMETER (  XC_trek = 142 )
      PARAMETER (  XC_ul_angle = 144 )
      PARAMETER (  XC_umbrella = 146 )
      PARAMETER (  XC_ur_angle = 148 )
      PARAMETER (  XC_watch = 150 )
      PARAMETER (  XC_xterm = 152 )
