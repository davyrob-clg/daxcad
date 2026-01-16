C
C    SCCS id Keywords             @(#)  412.1 date 6/11/92 pad.ins.ftn 
C
C    Practical Technology 1990
C
C    This file is here for 9.7 compatibilty when compiling on SR10 machines
C
C    If the new pad file is used then following error message is given
C
C    [00026]      +  pad_$insuff_rights
C    **** Error #118 on Line 26 OF '/sys/ins/pad.ins.ftn':
C    too many continuation lines
C    [00048]      +  pad_$insuff_rights =   16#02030014 )
C    **** Error #118 on Line 48 OF '/sys/ins/pad.ins.ftn':
C    too many continuation lines
C
C
C    Apollo Bit
C
C    Insert file for Fortran users of Pad calls

C--Status Code Definitions

      integer*4
     +  pad_$stream_not_pad,
     +  pad_$not_input,
     +  pad_$id_oor,
     +  pad_$stream_not_open,
     +  pad_$not_transcript,
     +  pad_$not_raw,
     +  pad_$voor,
     +  pad_$too_many_fonts,
     +  pad_$font_file_err,
     +  pad_$bad_key_name,
     +  pad_$2mny_input_pads,
     +  pad_$ill_param_comb,
     +  pad_$not_ascii,
     +  pad_$2mny_clients,
     +  pad_$edit_quit,
     +  pad_$no_such_window,
     +  pad_$no_window,
     +  pad_$ill_ptype
 
      parameter (
     +  pad_$stream_not_pad =  16#02030001,
     +  pad_$not_input =       16#02030002,
     +  pad_$id_oor =          16#02030003,
     +  pad_$stream_not_open = 16#02030004,
     +  pad_$not_transcript =  16#02030005,
     +  pad_$not_raw =         16#02030006,
     +  pad_$voor =            16#02030007,
     +  pad_$too_many_fonts =  16#02030008,
     +  pad_$font_file_err =   16#02030009,
     +  pad_$bad_key_name =    16#0203000A,
     +  pad_$2mny_input_pads = 16#0203000B,
     +  pad_$ill_param_comb =  16#0203000C,
     +  pad_$not_ascii =       16#0203000D,
     +  pad_$2mny_clients =    16#0203000E,
     +  pad_$edit_quit =       16#0203000F,
     +  pad_$no_such_window =  16#02030010,
     +  pad_$no_window =       16#02030011,
     +  pad_$ill_ptype =       16#02030012 )

C--Rel_abs parameter for pad_$move
 
      integer*2 pad_$relative, pad_$absolute
 
      parameter(
     +  pad_$relative = 0,
     +  pad_$absolute = 1 )

C--Cursor Position Report Type Parameter
 
      integer*2 pad_$cpr_none, pad_$cpr_change, pad_$cpr_all,
     +  pad_$cpr_pick, pad_$cpr_draw
 
      parameter (
     +  pad_$cpr_none = 0,
     +  pad_$cpr_change = 1,
     +  pad_$cpr_all = 2,
     +  pad_$cpr_pick = 3,
     +  pad_$cpr_draw = 4 )

C--Special Characters

      character
     +  pad_$newline,
     +  pad_$cr,
     +  pad_$ff,
     +  pad_$bs,
     +  pad_$tab, 
     +  pad_$escape,
     +  pad_$cpr_flag,
     +  pad_$no_key,
     +  pad_$left_window
 
      parameter (
     +  pad_$cpr_flag = char(16#FF),
     +  pad_$no_key = char(16#FE),
     +  pad_$left_window = char(16#FD),
     +  pad_$newline = char(10),
     +  pad_$cr = char(13),
     +  pad_$ff = char(12),
     +  pad_$bs = char(8),
     +  pad_$tab = char(9),
     +  pad_$escape = char(27) ) 

C--Cre_opt parameter for pad_$create
C    add desired options to get cre_opt parameter

      integer*2 pad_$abs_size, pad_$init_raw

      parameter (
     +  pad_$abs_size = 1,
     +  pad_$init_raw = 2 )

C--Side parameters for pad_$create

      integer*2 pad_$left, pad_$right,
     +          pad_$top, pad_$bottom
      
      parameter (
     +  pad_$left = 0,
     +  pad_$right = 1,
     +  pad_$top = 2,
     +  pad_$bottom = 3 )

C--Type parameters for pad_$create

      integer*2 pad_$transcript, pad_$input, pad_$edit, pad_$read_edit

      parameter (
     +  pad_$transcript = 0,
     +  pad_$input = 1,
     +  pad_$edit = 2,
     +  pad_$read_edit = 3 )

C--Display types for pad_$inq_disp_type

      integer*2 pad_$none, pad_$bw_15P, pad_$bw_19L, pad_$color_display,
     +  pad_$800_color, pad_$color2_display, pad_$color3_display,
     +  pad_$reserved_display, pad_$color4_display, pad_$bw4_1280x1024,
     +  pad_$color5_display, pad_$bw5_1024x800, pad_$color6_display

      parameter (
     +  pad_$none             = 0,
     +  pad_$bw_15P           = 1,
     +  pad_$bw_19L           = 2,
     +  pad_$color_display    = 3,
     +  pad_$800_color        = 4,
     +  pad_$color2_display   = 5,
     +  pad_$color3_display   = 6,
     +  pad_$reserved_display = 7,
     +  pad_$color4_display   = 8,
     +  pad_$bw4_1280x1024    = 9,
     +  pad_$color5_display  = 10,
     +  pad_$bw5_1024x800    = 11,
     +  pad_$color6_display  = 12)

C--function declarations:

      logical
     +  pad_$is_icon


