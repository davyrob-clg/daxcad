C
C     @(#)  412.1 date 6/11/92 gpr.ins.ftn 
C
C
C     Filename    : gpr.ins.ftn
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:16:00
C     Last change : 92/06/11 14:30:44
C
C     Copyright : Practical Technology Limited  
C     File :- gpr.ins.ftn
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C  graphics primitive package definitions

C       SCCS id Keywords             @(#)  1.2 date 1/7/91 gpr.ins.ftn   
C
C       Practical Technology 1990 
C
C       DAXCAD  X windows project.
C
C       Include file for Apollo GPR routines that are portable in GPRX library
C       Used for all routines that needed GPR defintions in DAXCAD
C
C
C
      integer*2
     +    gpr_$default_list_size,
     +    gpr_$string_size,
     +    gpr_$max_x_size, gpr_$max_y_size,
     +    gpr_$highest_plane,
     +    gpr_$highest_rgb_plane,
     +    gpr_$max_physical_color_maps,
     +    gpr_$max_bmf_group,
     +    gpr_$bmf_major_version,
     +    gpr_$bmf_minor_version,
     +    gpr_$max_clip_rects
      integer*4
     +    gpr_$black, gpr_$white,
     +    gpr_$red, gpr_$green, gpr_$blue,
     +    gpr_$cyan, gpr_$magenta, gpr_$yellow,
     +    gpr_$transparent, gpr_$background,
     +    gpr_$nil_attribute_desc, gpr_$nil_bitmap_desc
      integer*2
     +    gpr_$rop_zeros,
     +    gpr_$rop_src_and_dst,
     +    gpr_$rop_src_and_not_dst,
     +    gpr_$rop_src,
     +    gpr_$rop_not_src_and_dst,
     +    gpr_$rop_dst,
     +    gpr_$rop_src_xor_dst,
     +    gpr_$rop_src_or_dst,
     +    gpr_$rop_not_src_and_not_dst,
     +    gpr_$rop_src_equiv_dst,
     +    gpr_$rop_not_dst,
     +    gpr_$rop_src_or_not_dst,
     +    gpr_$rop_not_src,
     +    gpr_$rop_not_src_or_dst,
     +    gpr_$rop_not_src_or_not_dst,
     +    gpr_$rop_ones

      parameter (
     +    gpr_$default_list_size = 10,
     +    gpr_$string_size = 256,
     +    gpr_$max_x_size = 8192, gpr_$max_y_size = 8192,
     +    gpr_$highest_plane = 7,
     +    gpr_$highest_rgb_plane = 31,
     +    gpr_$max_physical_color_maps = 8,
     +    gpr_$max_bmf_group = 0,
     +    gpr_$bmf_major_version = 1,
     +    gpr_$bmf_minor_version = 1,
     +    gpr_$max_clip_rects = 7)
      parameter (
     +    gpr_$black = 0,
     +    gpr_$transparent = -1, gpr_$background = -2,
     +    gpr_$nil_attribute_desc = 0, gpr_$nil_bitmap_desc = 0)
      parameter (
     +    gpr_$rop_zeros                =  0,
     +    gpr_$rop_src_and_dst          =  1,
     +    gpr_$rop_src_and_not_dst      =  2,
     +    gpr_$rop_src                  =  3,
     +    gpr_$rop_not_src_and_dst      =  4,
     +    gpr_$rop_dst                  =  5,
     +    gpr_$rop_src_xor_dst          =  6,
     +    gpr_$rop_src_or_dst           =  7,
     +    gpr_$rop_not_src_and_not_dst  =  8,
     +    gpr_$rop_src_equiv_dst        =  9,
     +    gpr_$rop_not_dst              = 10,
     +    gpr_$rop_src_or_not_dst       = 11,
     +    gpr_$rop_not_src              = 12,
     +    gpr_$rop_not_src_or_dst       = 13,
     +    gpr_$rop_not_src_or_not_dst   = 14,
     +    gpr_$rop_ones                 = 15)


C---the eight ways to use this package

      integer*2
     +      gpr_$borrow, gpr_$frame, gpr_$no_display, gpr_$direct,
     +      gpr_$borrow_nc, gpr_$direct_rgb, gpr_$borrow_rgb,
     +      gpr_$borrow_rgb_nc

      parameter (
     +      gpr_$borrow = 0, gpr_$frame = 1, gpr_$no_display = 2,
     +      gpr_$direct = 3, gpr_$borrow_nc = 4, gpr_$direct_rgb = 5,
     +      gpr_$borrow_rgb = 6, gpr_$borrow_rgb_nc = 7)

C---ways to use gpr_$initialize
      integer*2
     +      gpr_$memory_bitmap, gpr_$pad_id, 
     +      gpr_$pad_frame_id, gpr_$rm_id,
     +      gpr_$screen, gpr_$x_window_id

      parameter (
     +      gpr_$memory_bitmap = 0, gpr_$pad_id = 1,
     +      gpr_$pad_frame_id = 2, gpr_$rm_id = 3,
     +      gpr_$screen = 5, gpr_$x_window_id = 6)

      integer*4
     +      gpr_$no_clear, gpr_$plane_mode, gpr_$pixel_mode

      parameter (
     +      gpr_$no_clear = 0, gpr_$plane_mode = 1,
     +      gpr_$pixel_mode = 2)

C---possible display hardware configurations

      integer*2
     +      gpr_$bw_800x1024,
     +      gpr_$bw_1024x800,
     +      gpr_$color_1024x1024x4,
     +      gpr_$color_1024x1024x8,
     +      gpr_$color_1024x800x4,
     +      gpr_$color_1024x800x8,
     +      gpr_$color_1280x1024x8,
     +      gpr_$color1_1024x800x8,
     +      gpr_$color2_1024x800x4,
     +      gpr_$bw_1280x1024,
     +      gpr_$color2_1024x800x8,
     +      gpr_$bw5_1024x800,
     +      gpr_$color2_1280x1024x8,
     +      gpr_$color10_1280x1024,
     +      gpr_$color7_1280x1024,
     +      gpr_$mono9_2kx1k


      parameter (
     +      gpr_$bw_800x1024        = 0,
     +      gpr_$bw_1024x800        = 1,
     +      gpr_$color_1024x1024x4  = 2,
     +      gpr_$color_1024x1024x8  = 3,
     +      gpr_$color_1024x800x4   = 4,
     +      gpr_$color_1024x800x8   = 5,
     +      gpr_$color_1280x1024x8  = 6,
     +      gpr_$color1_1024x800x8  = 7,
     +      gpr_$color2_1024x800x4  = 8,
     +      gpr_$bw_1280x1024       = 9,
     +      gpr_$color2_1024x800x8  = 10,
     +      gpr_$bw5_1024x800       = 11,
     +      gpr_$color2_1280x1024x8 = 12,
     +      gpr_$color10_1280x1024  = 13,
     +      gpr_$color7_1280x1024   = 14,
     +      gpr_$mono9_2kx1k        = 15
     +      )

C--possible directions for arcs drawn using gpr_$arc_c2p

      integer*2
     +      gpr_$arc_ccw,
     +      gpr_$arc_cw

      parameter (
     +      gpr_$arc_ccw   = 0,
     +      gpr_$arc_cw    = 1)


C--possible options on co-incident points in gpr_$arc_c2p

      integer*2
     +      gpr_$arc_draw_none,
     +      gpr_$arc_draw_full

        parameter (
     +      gpr_$arc_draw_none  = 0,
     +      gpr_$arc_draw_full  = 1)


C--imaging vs interactive display formats

      integer*2
     +      gpr_$interactive,
     +      gpr_$imaging_1024x1024x8,
     +      gpr_$imaging_512x512x24,
     +      gpr_$double_bufferx8,  
     +      gpr_$imaging_1280x1024x24,
     +      gpr_$imaging_windowx24,
     +      gpr_$double_bufferx24

      parameter (
     +      gpr_$interactive          = 0,
     +      gpr_$imaging_1024x1024x8  = 1,
     +      gpr_$imaging_512x512x24   = 2,
     +      gpr_$double_bufferx8      = 3,
     +      gpr_$imaging_1280x1024x24 = 4,
     +      gpr_$imaging_windowx24    = 5,
     +      gpr_$double_bufferx24     = 6)


C--controller types

      integer*2
     +      gpr_$ctl_none,
     +      gpr_$ctl_mono_1,
     +      gpr_$ctl_mono_2,
     +      gpr_$ctl_color_1,
     +      gpr_$ctl_color_2,
     +      gpr_$ctl_color_3,
     +      gpr_$ctl_color_4,
     +      gpr_$ctl_mono_4,
     +      gpr_$ctl_color_5,
     +      gpr_$ctl_mono_5,
     +      gpr_$ctl_color_6,
     +      gpr_$ctl_color_10,
     +      gpr_$ctl_color_7

      parameter (
     +      gpr_$ctl_none     = 0,
     +      gpr_$ctl_mono_1   = 1,
     +      gpr_$ctl_mono_2   = 2,
     +      gpr_$ctl_color_1  = 3,
     +      gpr_$ctl_color_2  = 4,
     +      gpr_$ctl_color_3  = 5,
     +      gpr_$ctl_color_4  = 6,
     +      gpr_$ctl_mono_4   = 7,
     +      gpr_$ctl_color_5  = 8,
     +      gpr_$ctl_mono_5   = 9,
     +      gpr_$ctl_color_6  = 10,
     +      gpr_$ctl_color_10 = 11,
     +      gpr_$ctl_color_7  = 12
     +      )


C--accelerator types

      integer*2
     +      gpr_$accel_none, gpr_$accel_1

      parameter (
     +      gpr_$accel_none = 0, gpr_$accel_1 = 1)


C--memory overlap types

      integer*2
     +      gpr_$hdm_with_bitm_ext, gpr_$hdm_with_buffers,
     +      gpr_$bitm_ext_with_buffers,
     +      gpr_$bitm_ext_with_zbuffer


      parameter (
     +      gpr_$hdm_with_bitm_ext = 0, gpr_$hdm_with_buffers = 1,
     +      gpr_$bitm_ext_with_buffers = 2,
     +      gpr_$bitm_ext_with_zbuffer = 3)


C-access allocation types

      integer*2
     +      gpr_$alloc_1, gpr_$alloc_2, gpr_$alloc_4,
     +      gpr_$alloc_8, gpr_$alloc_16, gpr_$alloc_32

      parameter (
     +      gpr_$alloc_1 = 0, gpr_$alloc_2 = 1, gpr_$alloc_4 = 2, 
     +      gpr_$alloc_8 = 3, gpr_$alloc_16 = 4, gpr_$alloc_32 = 5)


C--INVert implementation types

      integer*2
     +      gpr_$no_invert, gpr_$invert_simulate,
     +      gpr_$invert_hardware

      parameter (
     +      gpr_$no_invert = 0, gpr_$invert_simulate = 1,
     +      gpr_$invert_hardware = 2)


C--RGB modes

      integer*2
     +      gpr_$rgb_none, gpr_$rgb_24

      parameter (
     +      gpr_$rgb_none = 0,
     +      gpr_$rgb_24   = 1)


C---event types

      integer*2 gpr_$input_ec
      parameter (gpr_$input_ec = 0)

      integer*2
     +      gpr_$keystroke, gpr_$buttons, gpr_$locator,
     +      gpr_$entered_window, gpr_$left_window, gpr_$locator_stop,
     +      gpr_$no_event, gpr_$locator_update, gpr_$dial, 
     +      gpr_$coded_keys, gpr_$function_keys, gpr_$pfk, 
     +      gpr_$physical_keys, gpr_$kbd_entered_window, 
     +      gpr_$kbd_left_window
 

      parameter (
     +      gpr_$keystroke = 0, gpr_$buttons = 1, gpr_$locator = 2, 
     +      gpr_$entered_window = 3, gpr_$left_window = 4,
     +      gpr_$locator_stop = 5, gpr_$no_event = 6, 
     +      gpr_$locator_update = 7, gpr_$dial = 8,
     +      gpr_$coded_keys = 9, gpr_$function_keys = 10,
     +      gpr_$pfk = 11, gpr_$physical_keys = 12, 
     +      gpr_$kbd_entered_window = 13, gpr_$kbd_left_window = 14)

C---obscured options for direct graphics

      integer*2
     +      gpr_$ok_if_obs, gpr_$error_if_obs, gpr_$pop_if_obs, 
     +      gpr_$block_if_obs, gpr_$input_ok_if_obs
    
      parameter (
     +      gpr_$ok_if_obs = 0, gpr_$error_if_obs = 1,
     +      gpr_$pop_if_obs = 2, gpr_$block_if_obs = 3,
     +      gpr_$input_ok_if_obs = 4)

C---options for acquire-display behavior when window is an icon }
      integer*2
     +      gpr_$ok_if_icon, gpr_$block_if_icon
     
      parameter (
     +      gpr_$ok_if_icon = 0, gpr_$block_if_icon = 1)
     

      integer*2 gpr_$create,gpr_$update, gpr_$write, gpr_$readonly

      parameter (
     +      gpr_$create = 0,  gpr_$update = 1,
     +      gpr_$write = 2,  gpr_$readonly = 3)

      integer*2
     +       gpr_$undisturbed_buffer,
     +       gpr_$clear_buffer, gpr_$copy_buffer

      parameter (
     +       gpr_$undisturbed_buffer = 0,
     +       gpr_$clear_buffer = 1, gpr_$copy_buffer = 2)

C---pixel modes
      integer*4
     +      gpr_$pixel_monochrome, gpr_$pixel_pseudocolor,
     +      gpr_$pixel_truecolor,  gpr_$pixel_video
      parameter (
     +      gpr_$pixel_monochrome = 1,  gpr_$pixel_pseudocolor = 2,
     +      gpr_$pixel_truecolor = 4,   gpr_$pixel_video = 8)

C---overlay modes
      integer*4 gpr_$ovlay_none, gpr_$ovlay_per_rectangle,
     +          gpr_$ovlay_per_buffer
      parameter (gpr_$ovlay_none = 0, gpr_$ovlay_per_rectangle = 1,
     +          gpr_$ovlay_per_buffer = 2)

C---z modes
      integer*4 gpr_$z_none, gpr_$z_per_rectangle,
     +          gpr_$z_per_buffer
      parameter (gpr_$z_none = 0, gpr_$z_per_rectangle = 1,
     +          gpr_$z_per_buffer = 2)

C---alpha modes
      integer*4 gpr_$alpha_none, gpr_$alpha_per_rectangle,
     +          gpr_$alpha_per_buffer
      parameter (gpr_$alpha_none = 0, gpr_$alpha_per_rectangle = 1,
     +          gpr_$alpha_per_buffer = 2)

C---projection modes
      integer*4 gpr_$proj_mode_argb, gpr_$proj_mode_ovlay,
     +          gpr_$proj_mode_zzzz
      parameter (gpr_$proj_mode_argb = 0, gpr_$proj_mode_ovlay = 1,
     +          gpr_$proj_mode_zzzz = 2)

C---format types
      integer*4 gpr_$pixel_format, gpr_$proj_format,
     +          gpr_$video_format
      parameter (gpr_$pixel_format = 0, gpr_$proj_format = 1,
     +          gpr_$video_format = 2)

C---projection mapping types
      integer*4 gpr_$proj_map_pixel, gpr_$proj_map_plane,
     +          gpr_$proj_map_byte
      parameter (gpr_$proj_map_pixel = 0, gpr_$proj_map_plane = 1,
     +          gpr_$proj_map_byte = 2)

      integer*4 gpr_$max_formats
      parameter (gpr_$max_formats = 100)
      integer*4 gpr_$pixel_format_size
      parameter (gpr_$pixel_format_size = 64)
      integer*4 gpr_$disp_char_size
      parameter (gpr_$disp_char_size = 68)

C---directions

      integer*2
     +      gpr_$up, gpr_$down, gpr_$left, gpr_$right

      parameter (
     +      gpr_$up = 0, gpr_$down = 1, gpr_$left = 2, gpr_$right = 3)


C---line styles

      integer*2
     +      gpr_$solid, gpr_$dotted

      parameter (
     +      gpr_$solid = 0, gpr_$dotted = 1)


C---raster op prim set elements

      integer*2
     +      gpr_$rop_blt, gpr_$rop_line,
     +      gpr_$rop_fill

      parameter (
     +      gpr_$rop_blt = 0, gpr_$rop_line = 1,
     +      gpr_$rop_fill = 2)


C---polygon decomposition techniques

      integer*2
     +      gpr_$fast_traps, gpr_$precise_traps,
     +      gpr_$non_overlapping_tris,
     +      gpr_$render_exact

      parameter (
     +      gpr_$fast_traps = 0, gpr_$precise_traps = 1,
     +      gpr_$non_overlapping_tris = 2,
     +      gpr_$render_exact = 3)


C---winding set
                                                            
      integer*2
     +      gpr_$parity, gpr_$nonzero, gpr_$specific

      parameter (
     +      gpr_$parity = 0, gpr_$nonzero = 1, gpr_$specific = 2)
                       
C---function declarations:

      integer*4
     +      gpr_$attribute_block

      integer*4
     +      gpr_$inq_background

      integer*4
     +      gpr_$inq_foreground

      logical
     +      gpr_$acquire_display

      logical
     +      gpr_$event_wait

      logical
     +      gpr_$cond_event_wait

      integer*2 int2
