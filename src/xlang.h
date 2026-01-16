/*
     @(#)  412.1 date 6/11/92 xlang.h 


     Filename    : xlang.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:44:39
     Last change : 92/06/11 14:44:38

     Copyright : Practical Technology International Limited  

     Cross language include file for C files. 
     This file should be included in files tah want to use
     routines that are to be called by F77. Some routines 
     need an _ character to call them. The Macro USE_UNDERSCORE
     is set if the system needs underscores.

*/


#ifdef USE_UNDERSCORE      /* Functions need underscore in X lang  (APOLLO F77 ) */


/*    DAXCAD standard interface routines */


#define SPAWNPROCESS    spawnprocess_
#define SHELLPC         shellpc_
#define DELETEC         deletec_
#define COPYFC          copyfc_              
#define DIRFINC         dirfinc_             
#define DAXGETERROR     daxgeterror_         
#define EXIT            exit_                
#define INQFS1C         inqfs1c_
#define LOCALT          localt_
#define SIZEC           sizec_
#define TIMEWAIT        timewait_
#define UNIXTIME        unixtime_
#define POPAWINDOW      popawindow_
#define POPPEDWINDOW    poppedwindow_

#define EXISTPATH       existpath_
#define VALDIRC         valdirc_

#define MIRGET          mirget_
#define PDRGET          pdrget_
#define AND_2           and_2_
#define PGM_$EXIT       pgm_$exit_
#define DIAG1           diag1_
#define DAXGETSIZE      daxgetsize_

#define MIRGET          mirget_
#define MIRSET          mirset_
#define PDRSET          pdrset_
#define PDRGET          pdrget_

#define DATABASEINIT    databaseinit_

#define ICHAR           ichar_
#define K10004          k10004_

#define SETHOME         sethome_
#define TOOLPEN         toolpen_
#define DAX_XOR         dax_xor_


#define MIFILELIMIT  100000	/* define limit before spilling onto disk */

/*    GPRX interface routines */

#define GPR_$OPEN_X_DISPLAY                  gpr_$open_x_display_
#define GPR_$SET_AUTO_REFRESH                gpr_$set_auto_refresh_
#define GPR_$SET_BITMAP                      gpr_$set_bitmap_
#define GPR_$LINE                            gpr_$line_
#define GPR_$MOVE                            gpr_$move_
#define GPR_$ALLOCATE_BITMAP                 gpr_$allocate_bitmap_
#define GPR_$INIT                            gpr_$init_
#define GPR_$PIXEL_BLT                       gpr_$pixel_blt_
#define GPR_$DEALLOCATE_BITMAP               gpr_$deallocate_bitmap_
#define GPR_$DEALLOCATE_BITMAP_X             gpr_$deallocate_bitmap_x_
#define GPR_$TERMINATE                       gpr_$terminate_
#define GPR_$ENABLE_INPUT                    gpr_$enable_input_
#define GPR_$DISABLE_INPUT                   gpr_$disable_input_
#define GPR_$EVENT_WAIT                      gpr_$event_wait_
#define GPR_$EVENT_WAIT_X                    gpr_$event_wait_x_
#define GPR_$COND_EVENT_WAIT                 gpr_$cond_event_wait_
#define GPR_$COND_EVENT_WAIT_X               gpr_$cond_event_wait_x_
#define GPR_$CLEAR                           gpr_$clear_
#define GPR_$SET_TEXT_VALUE                  gpr_$set_text_value_
#define GPR_$SET_TEXT_BACKGROUND_VALUE       gpr_$set_text_background_value_
#define GPR_$SET_FILL_VALUE                  gpr_$set_fill_value_
#define GPR_$SET_FILL_BACKGROUND_VALUE       gpr_$set_fill_background_value_
#define GPR_$SET_DRAW_VALUE                  gpr_$set_draw_value_
#define GPR_$SET_WINDOW_ID                   gpr_$set_window_id_
#define GPR_$SET_WINDOW_START                gpr_$set_window_start_
#define GPR_$SET_LINE_WIDTH                  gpr_$set_line_width_
#define GPR_$SET_LINESTYLE                   gpr_$set_linestyle_
#define GPR_$SET_RASTER_OP                   gpr_$set_raster_op_
#define GPR_$LOAD_FONT_FILE                  gpr_$load_font_file_
#define GPR_$UNLOAD_FONT_FILE                gpr_$unload_font_file_
#define GPR_$SET_TEXT_FONT                   gpr_$set_text_font_
#define GPR_$TEXT                            gpr_$text_
#define GPR_$SET_CLIPPING_ACTIVE             gpr_$set_clipping_active_
#define GPR_$SET_CLIP_WINDOW                 gpr_$set_clip_window_
#define GPR_$RECTANGLE                       gpr_$rectangle_
#define GPR_$DRAW_BOX                        gpr_$draw_box_
#define GPR_$MULTILINE                       gpr_$multiline_
#define GPR_$POLYLINE                        gpr_$polyline_
#define GPR_$SET_REFRESH_ENTRY               gpr_$set_refresh_entry_
#define GPR_$SET_PLANE_MASK                  gpr_$set_plane_mask_
#define GPR_$INQ_CONSTRAINTS                 gpr_$inq_constraints_
#define GPR_$SET_X_CURSOR                    gpr_$set_x_cursor_
#define GPR_$SET_WAIT_CURSOR                 gpr_$set_wait_cursor_
#define GPR_$SET_CURSOR_ACTIVE               gpr_$set_cursor_active_
#define GPR_$START_PGON                      gpr_$start_pgon_
#define GPR_$PGON_POLYLINE                   gpr_$pgon_polyline_
#define GPR_$CLOSE_FILL_PGON                 gpr_$close_fill_pgon_
#define GPR_$INQ_TEXT_OFFSET                 gpr_$inq_text_offset_
#define GPR_$INQ_TEXT_EXTENT                 gpr_$inq_text_extent_
#define GPR_$INQ_BITMAP                      gpr_$inq_bitmap_
#define GPR_$SET_FILL_PATTERN                gpr_$set_fill_pattern_
#define GPR_$SET_ATTRIBUTE_BLOCK             gpr_$set_attribute_block_
#define GPR_$INQ_CP                          gpr_$inq_cp_
#define GPR_$INQ_DRAW_VALUE                  gpr_$inq_draw_value_
#define GPR_$INQ_FILL_VALUE                  gpr_$inq_fill_value_
#define GPR_$INQ_FILL_BACKGROUND_VALUE       gpr_$inq_fill_background_value_
#define GPR_$INQ_TEXT_VALUES                 gpr_$inq_text_values_
#define GPR_$CIRCLE                          gpr_$circle_
#define GPR_$CIRCLE_FILLED                   gpr_$circle_filled_
#define GPR_$INQ_BITMAP_DIMENSIONS           gpr_$inq_bitmap_dimensions_
#define GPR_$INQ_BITMAP_START                gpr_$inq_bitmap_start_
#define GPR_$SET_TITLE                       gpr_$set_title_
#define GPR_$SET_BANNER                      gpr_$set_banner_
#define GPR_$SET_BITMAP_WINDOW               gpr_$set_bitmap_window_
#define GPR_$COLLECT_EXPOSURES               gpr_$collect_exposures_
#define GPR_$INQ_RASTER_OP                   gpr_$inq_raster_op_
#define GPR_$SAVE_ENV_X                      gpr_$save_env_x_
#define GPR_$RESTORE_ENV_X                   gpr_$restore_env_x_
#define GPR_$SET_AUTO_SWITCH                 gpr_$set_auto_switch_
#define GPR_$SET_AUTO_UPDATE                 gpr_$set_auto_update_
#define GPR_$FORCE_HIDDEN_UPDATE             gpr_$force_hidden_update_
#define GPR_$INQ_MONO_PIXELS                 gpr_$inq_mono_pixels_
#define GPR_$INQ_ROOTWINDOW                  gpr_$inq_rootwindow_
#define GPR_$SET_ICON_NAME                   gpr_$set_icon_name_
#define GPR_$SERVER_FLUSH_X                  gpr_$server_flush_x_
#define GPR_$ACQUIRE_DISPLAY                 gpr_$acquire_display_
#define GPR_$RELEASE_DISPLAY                 gpr_$release_display_
#define GPR_$FORCE_RELEASE                   gpr_$force_release_
#define GPR_$SET_ACQ_TIME_OUT                gpr_$set_acq_time_out_
#define GPR_$SET_OBSCURED_OPT                gpr_$set_obscured_opt_
#define GPR_$RASTER_OP_PRIM_SET              gpr_$raster_op_prim_set_
#define GPR_$ALLOCATE_ATTRIBUTE_BLOCK        gpr_$allocate_attribute_block_
#define GPR_$DEALLOCATE_ATTRIBUTE_BLOCK      gpr_$deallocate_attribute_block_
#define GPR_$SET_CURSOR_POSITION             gpr_$set_cursor_position_
#define GPR_$POINT                           gpr_$point_
#define GPR_$TRIANGLE                        gpr_$triangle_
#define GPR_$INQ_TEXT                        gpr_$inq_text_
#define GPR_$REPLICATE_FONT                  gpr_$replicate_font_
#define GPR_$INQ_HORIZONTAL_SPACING          gpr_$inq_horizontal_spacing_
#define GPR_$SET_HORIZONTAL_SPACING          gpr_$set_horizontal_spacing_

#define ERROR_$PRINT                          error_$print_
#define GPR_$INQ_COLOR_MAP                    gpr_$inq_color_map_
#define GPR_$INQ_CURSOR                       gpr_$inq_cursor_
#define GPR_$INQ_RASTER_OPS                   gpr_$inq_raster_ops_
#define GPRX_XBELL                            gprx_xbell_
#define GPR_$SET_COLOR_MAP                    gpr_$set_color_map_
#define GPR_$SET_TEXT_PATH                    gpr_$set_text_path_
#define GPR_$SET_INPUT_FOCUS                  gpr_$set_input_focus_
#define GPR_$WRITE_BITMAP                     gpr_$write_bitmap_
#define GPR_$READ_BITMAP                      gpr_$read_bitmap_
#define GPR_$GET_SYSTEM_COLORMAP              gpr_$get_system_colormap_


/*    NURBS interface routines */

#define ROT2DF   rot2df_
#define CLC2DT   clc2dt_
#define CCT2L    cct2l_
#define CCTLC    cctlc_
#define CCT2C    cct2c_
#define CCTC     cctc_
#define CCTL     cctl_
#define CCT3L    cct3l_
#define CCT2LC   cct2lc_
#define CCT2CL   cct2cl_
#define CCT3C    cct3c_
#define DDNTE    ddnte_
#define DSAME    dsame_



#endif


#ifdef GPR_UNDERSCORE      /* Functions need underscore in X lang  except GPR or SYSTEM calls (APOLLO F77 ) */


/*    DAXCAD standard interface routines */


#define SPAWNPROCESS    spawnprocess_
#define SHELLPC         shellpc_
#define DELETEC         deletec_
#define COPYFC          copyfc_              
#define DIRFINC         dirfinc_             
#define DAXGETERROR     daxgeterror_         
#define EXIT            exit_                
#define INQFS1C         inqfs1c_
#define LOCALT          localt_
#define SIZEC           sizec_
#define TIMEWAIT        timewait_
#define UNIXTIME        unixtime_
#define POPAWINDOW      popawindow_
#define POPPEDWINDOW    poppedwindow_

#define EXISTPATH       existpath_
#define VALDIRC         valdirc_

#define MIRGET          mirget_
#define PDRGET          pdrget_
#define AND_2           and_2_
#define PGM_$EXIT       pgm_$exit_
#define DIAG1           diag1_
#define GPRX_XBELL      gprx_xbell_
#define DAXGETSIZE      daxgetsize_

#define MIRGET          mirget_
#define MIRSET          mirset_
#define PDRSET          pdrset_
#define PDRGET          pdrget_

#define DATABASEINIT    databaseinit_

#define ICHAR           ichar_  
#define K10004          k10004_

#define SETHOME         sethome_
#define TOOLPEN         toolpen_
#define DAX_XOR         dax_xor_

/*    GPRX interface routines */

#define GPR_$OPEN_X_DISPLAY                  gpr_$open_x_display
#define GPR_$SET_AUTO_REFRESH                gpr_$set_auto_refresh
#define GPR_$SET_BITMAP                      gpr_$set_bitmap
#define GPR_$LINE                            gpr_$line
#define GPR_$MOVE                            gpr_$move
#define GPR_$ALLOCATE_BITMAP                 gpr_$allocate_bitmap
#define GPR_$INIT                            gpr_$init
#define GPR_$PIXEL_BLT                       gpr_$pixel_blt
#define GPR_$DEALLOCATE_BITMAP               gpr_$deallocate_bitmap
#define GPR_$DEALLOCATE_BITMAP_X             gpr_$deallocate_bitmap_x
#define GPR_$TERMINATE                       gpr_$terminate
#define GPR_$ENABLE_INPUT                    gpr_$enable_input
#define GPR_$DISABLE_INPUT                   gpr_$disable_input
#define GPR_$EVENT_WAIT                      gpr_$event_wait
#define GPR_$EVENT_WAIT_X                    gpr_$event_wait_x
#define GPR_$COND_EVENT_WAIT                 gpr_$cond_event_wait
#define GPR_$COND_EVENT_WAIT_X               gpr_$cond_event_wait_x
#define GPR_$CLEAR                           gpr_$clear
#define GPR_$SET_TEXT_VALUE                  gpr_$set_text_value
#define GPR_$SET_TEXT_BACKGROUND_VALUE       gpr_$set_text_background_value
#define GPR_$SET_FILL_VALUE                  gpr_$set_fill_value
#define GPR_$SET_FILL_BACKGROUND_VALUE       gpr_$set_fill_background_value
#define GPR_$SET_DRAW_VALUE                  gpr_$set_draw_value
#define GPR_$SET_WINDOW_ID                   gpr_$set_window_id
#define GPR_$SET_WINDOW_START                gpr_$set_window_start
#define GPR_$SET_LINE_WIDTH                  gpr_$set_line_width
#define GPR_$SET_LINESTYLE                   gpr_$set_linestyle
#define GPR_$SET_RASTER_OP                   gpr_$set_raster_op
#define GPR_$LOAD_FONT_FILE                  gpr_$load_font_file
#define GPR_$UNLOAD_FONT_FILE                gpr_$unload_font_file
#define GPR_$SET_TEXT_FONT                   gpr_$set_text_font
#define GPR_$TEXT                            gpr_$text
#define GPR_$SET_CLIPPING_ACTIVE             gpr_$set_clipping_active
#define GPR_$SET_CLIP_WINDOW                 gpr_$set_clip_window
#define GPR_$RECTANGLE                       gpr_$rectangle
#define GPR_$DRAW_BOX                        gpr_$draw_box
#define GPR_$MULTILINE                       gpr_$multiline
#define GPR_$POLYLINE                        gpr_$polyline
#define GPR_$SET_REFRESH_ENTRY               gpr_$set_refresh_entry
#define GPR_$SET_PLANE_MASK                  gpr_$set_plane_mask
#define GPR_$INQ_CONSTRAINTS                 gpr_$inq_constraints
#define GPR_$SET_X_CURSOR                    gpr_$set_x_cursor
#define GPR_$SET_WAIT_CURSOR                 gpr_$set_wait_cursor
#define GPR_$SET_CURSOR_ACTIVE               gpr_$set_cursor_active
#define GPR_$START_PGON                      gpr_$start_pgon
#define GPR_$PGON_POLYLINE                   gpr_$pgon_polyline
#define GPR_$CLOSE_FILL_PGON                 gpr_$close_fill_pgon
#define GPR_$INQ_TEXT_OFFSET                 gpr_$inq_text_offset
#define GPR_$INQ_TEXT_EXTENT                 gpr_$inq_text_extent
#define GPR_$INQ_BITMAP                      gpr_$inq_bitmap
#define GPR_$SET_FILL_PATTERN                gpr_$set_fill_pattern
#define GPR_$SET_ATTRIBUTE_BLOCK             gpr_$set_attribute_block
#define GPR_$INQ_CP                          gpr_$inq_cp
#define GPR_$INQ_DRAW_VALUE                  gpr_$inq_draw_value
#define GPR_$INQ_FILL_VALUE                  gpr_$inq_fill_value
#define GPR_$INQ_FILL_BACKGROUND_VALUE       gpr_$inq_fill_background_value
#define GPR_$INQ_TEXT_VALUES                 gpr_$inq_text_values
#define GPR_$CIRCLE                          gpr_$circle
#define GPR_$CIRCLE_FILLED                   gpr_$circle_filled
#define GPR_$INQ_BITMAP_DIMENSIONS           gpr_$inq_bitmap_dimensions
#define GPR_$INQ_BITMAP_START                gpr_$inq_bitmap_start
#define GPR_$SET_TITLE                       gpr_$set_title
#define GPR_$SET_BANNER                      gpr_$set_banner
#define GPR_$SET_BITMAP_WINDOW               gpr_$set_bitmap_window
#define GPR_$COLLECT_EXPOSURES               gpr_$collect_exposures
#define GPR_$INQ_RASTER_OP                   gpr_$inq_raster_op
#define GPR_$SAVE_ENV_X                      gpr_$save_env_x
#define GPR_$RESTORE_ENV_X                   gpr_$restore_env_x
#define GPR_$SET_AUTO_SWITCH                 gpr_$set_auto_switch
#define GPR_$SET_AUTO_UPDATE                 gpr_$set_auto_update
#define GPR_$FORCE_HIDDEN_UPDATE             gpr_$force_hidden_update
#define GPR_$INQ_MONO_PIXELS                 gpr_$inq_mono_pixels
#define GPR_$INQ_ROOTWINDOW                  gpr_$inq_rootwindow
#define GPR_$SET_ICON_NAME                   gpr_$set_icon_name
#define GPR_$SERVER_FLUSH_X                  gpr_$server_flush_x
#define GPR_$ACQUIRE_DISPLAY                 gpr_$acquire_display
#define GPR_$RELEASE_DISPLAY                 gpr_$release_display
#define GPR_$FORCE_RELEASE                   gpr_$force_release
#define GPR_$SET_ACQ_TIME_OUT                gpr_$set_acq_time_out
#define GPR_$SET_OBSCURED_OPT                gpr_$set_obscured_opt
#define GPR_$RASTER_OP_PRIM_SET              gpr_$raster_op_prim_set
#define GPR_$ALLOCATE_ATTRIBUTE_BLOCK        gpr_$allocate_attribute_block
#define GPR_$DEALLOCATE_ATTRIBUTE_BLOCK      gpr_$deallocate_attribute_block
#define GPR_$SET_CURSOR_POSITION             gpr_$set_cursor_position
#define GPR_$POINT                           gpr_$point
#define GPR_$TRIANGLE                        gpr_$triangle
#define GPR_$INQ_TEXT                        gpr_$inq_text
#define GPR_$REPLICATE_FONT                  gpr_$replicate_font
#define GPR_$INQ_HORIZONTAL_SPACING          gpr_$inq_horizontal_spacing
#define GPR_$SET_HORIZONTAL_SPACING          gpr_$set_horizontal_spacing

#define ERROR_$PRINT                          error_$print
#define GPR_$INQ_COLOR_MAP                    gpr_$inq_color_map
#define GPR_$INQ_CURSOR                       gpr_$inq_cursor
#define GPR_$INQ_RASTER_OPS                   gpr_$inq_raster_ops
#define GPR_$SET_COLOR_MAP                    gpr_$set_color_map
#define GPR_$SET_TEXT_PATH                    gpr_$set_text_path
#define GPR_$SET_INPUT_FOCUS                  gpr_$set_input_focus
#define GPR_$WRITE_BITMAP                     gpr_$write_bitmap
#define GPR_$READ_BITMAP                      gpr_$read_bitmap
#define GPR_$GET_SYSTEM_COLORMAP              gpr_$get_system_colormap


/*    NURBS interface routines */

#define ROT2DF   rot2df_
#define CLC2DT   clc2dt_
#define CCT2L    cct2l_
#define CCTLC    cctlc_
#define CCT2C    cct2c_
#define CCTC     cctc_
#define CCTL     cctl_
#define CCT3L    cct3l_
#define CCT2LC   cct2lc_
#define CCT2CL   cct2cl_
#define CCT3C    cct3c_
#define DDNTE    ddnte_
#define DSAME    dsame_



#endif


#ifdef NO_UNDERSCORE      /* Functions do not need underscore in X lang  (IBM 6000, APOLLO FTN ) */

/*    DAXCAD standard interface routines */


#define SPAWNPROCESS    spawnprocess
#define SHELLPC         shellpc
#define DELETEC         deletec
#define COPYFC          copyfc              
#define DIRFINC         dirfinc             
#define DAXGETERROR     daxgeterror         
#define EXIT            exit                
#define INQFS1C         inqfs1c
#define LOCALT          localt
#define SIZEC           sizec
#define TIMEWAIT        timewait
#define UNIXTIME        unixtime
#define POPAWINDOW      popawindow
#define POPPEDWINDOW    poppedwindow

#define EXISTPATH       existpath
#define VALDIRC         valdirc

#define MIRGET          mirget
#define PDRGET          pdrget
#define AND_2           and_2
#define PGM_$EXIT       pgm_$exit
#define DIAG1           diag1
#define DAXGETSIZE      daxgetsize

#define MIRGET          mirget
#define MIRSET          mirset
#define PDRSET          pdrset
#define PDRGET          pdrget

#define DATABASEINIT    databaseinit

#define ICHAR           ichar
#define K10004          k10004

#define SETHOME         sethome
#define TOOLPEN         toolpen
#define DAX_XOR         dax_xor

/*    GPRX interface routines */


#define GPR_$OPEN_X_DISPLAY                  gpr_$open_x_display
#define GPR_$SET_AUTO_REFRESH                gpr_$set_auto_refresh
#define GPR_$SET_BITMAP                      gpr_$set_bitmap
#define GPR_$LINE                            gpr_$line
#define GPR_$MOVE                            gpr_$move
#define GPR_$ALLOCATE_BITMAP                 gpr_$allocate_bitmap
#define GPR_$INIT                            gpr_$init
#define GPR_$PIXEL_BLT                       gpr_$pixel_blt
#define GPR_$DEALLOCATE_BITMAP               gpr_$deallocate_bitmap
#define GPR_$DEALLOCATE_BITMAP_X             gpr_$deallocate_bitmap_x
#define GPR_$TERMINATE                       gpr_$terminate
#define GPR_$ENABLE_INPUT                    gpr_$enable_input
#define GPR_$DISABLE_INPUT                   gpr_$disable_input
#define GPR_$EVENT_WAIT                      gpr_$event_wait
#define GPR_$EVENT_WAIT_X                    gpr_$event_wait_x
#define GPR_$COND_EVENT_WAIT                 gpr_$cond_event_wait
#define GPR_$COND_EVENT_WAIT_X               gpr_$cond_event_wait_x
#define GPR_$CLEAR                           gpr_$clear
#define GPR_$SET_TEXT_VALUE                  gpr_$set_text_value
#define GPR_$SET_TEXT_BACKGROUND_VALUE       gpr_$set_text_background_value
#define GPR_$SET_FILL_VALUE                  gpr_$set_fill_value
#define GPR_$SET_FILL_BACKGROUND_VALUE       gpr_$set_fill_background_value
#define GPR_$SET_DRAW_VALUE                  gpr_$set_draw_value
#define GPR_$SET_WINDOW_ID                   gpr_$set_window_id
#define GPR_$SET_WINDOW_START                gpr_$set_window_start
#define GPR_$SET_LINE_WIDTH                  gpr_$set_line_width
#define GPR_$SET_LINESTYLE                   gpr_$set_linestyle
#define GPR_$SET_RASTER_OP                   gpr_$set_raster_op
#define GPR_$LOAD_FONT_FILE                  gpr_$load_font_file
#define GPR_$UNLOAD_FONT_FILE                gpr_$unload_font_file
#define GPR_$SET_TEXT_FONT                   gpr_$set_text_font
#define GPR_$TEXT                            gpr_$text
#define GPR_$SET_CLIPPING_ACTIVE             gpr_$set_clipping_active
#define GPR_$SET_CLIP_WINDOW                 gpr_$set_clip_window
#define GPR_$RECTANGLE                       gpr_$rectangle
#define GPR_$DRAW_BOX                        gpr_$draw_box
#define GPR_$MULTILINE                       gpr_$multiline
#define GPR_$POLYLINE                        gpr_$polyline
#define GPR_$SET_REFRESH_ENTRY               gpr_$set_refresh_entry
#define GPR_$SET_PLANE_MASK                  gpr_$set_plane_mask
#define GPR_$INQ_CONSTRAINTS                 gpr_$inq_constraints
#define GPR_$SET_X_CURSOR                    gpr_$set_x_cursor
#define GPR_$SET_WAIT_CURSOR                 gpr_$set_wait_cursor
#define GPR_$SET_CURSOR_ACTIVE               gpr_$set_cursor_active
#define GPR_$START_PGON                      gpr_$start_pgon
#define GPR_$PGON_POLYLINE                   gpr_$pgon_polyline
#define GPR_$CLOSE_FILL_PGON                 gpr_$close_fill_pgon
#define GPR_$INQ_TEXT_OFFSET                 gpr_$inq_text_offset
#define GPR_$INQ_TEXT_EXTENT                 gpr_$inq_text_extent
#define GPR_$INQ_BITMAP                      gpr_$inq_bitmap
#define GPR_$SET_FILL_PATTERN                gpr_$set_fill_pattern
#define GPR_$SET_ATTRIBUTE_BLOCK             gpr_$set_attribute_block
#define GPR_$INQ_CP                          gpr_$inq_cp
#define GPR_$INQ_DRAW_VALUE                  gpr_$inq_draw_value
#define GPR_$INQ_FILL_VALUE                  gpr_$inq_fill_value
#define GPR_$INQ_FILL_BACKGROUND_VALUE       gpr_$inq_fill_background_value
#define GPR_$INQ_TEXT_VALUES                 gpr_$inq_text_values
#define GPR_$CIRCLE                          gpr_$circle
#define GPR_$CIRCLE_FILLED                   gpr_$circle_filled
#define GPR_$INQ_BITMAP_DIMENSIONS           gpr_$inq_bitmap_dimensions
#define GPR_$INQ_BITMAP_START                gpr_$inq_bitmap_start
#define GPR_$SET_TITLE                       gpr_$set_title
#define GPR_$SET_BANNER                      gpr_$set_banner
#define GPR_$SET_BITMAP_WINDOW               gpr_$set_bitmap_window
#define GPR_$COLLECT_EXPOSURES               gpr_$collect_exposures
#define GPR_$INQ_RASTER_OP                   gpr_$inq_raster_op
#define GPR_$SAVE_ENV_X                      gpr_$save_env_x
#define GPR_$RESTORE_ENV_X                   gpr_$restore_env_x
#define GPR_$SET_AUTO_SWITCH                 gpr_$set_auto_switch
#define GPR_$SET_AUTO_UPDATE                 gpr_$set_auto_update
#define GPR_$FORCE_HIDDEN_UPDATE             gpr_$force_hidden_update
#define GPR_$INQ_MONO_PIXELS                 gpr_$inq_mono_pixels
#define GPR_$INQ_ROOTWINDOW                  gpr_$inq_rootwindow
#define GPR_$SET_ICON_NAME                   gpr_$set_icon_name
#define GPR_$SERVER_FLUSH_X                  gpr_$server_flush_x
#define GPR_$ACQUIRE_DISPLAY                 gpr_$acquire_display
#define GPR_$RELEASE_DISPLAY                 gpr_$release_display
#define GPR_$FORCE_RELEASE                   gpr_$force_release
#define GPR_$SET_ACQ_TIME_OUT                gpr_$set_acq_time_out
#define GPR_$SET_OBSCURED_OPT                gpr_$set_obscured_opt
#define GPR_$RASTER_OP_PRIM_SET              gpr_$raster_op_prim_set
#define GPR_$ALLOCATE_ATTRIBUTE_BLOCK        gpr_$allocate_attribute_block
#define GPR_$DEALLOCATE_ATTRIBUTE_BLOCK      gpr_$deallocate_attribute_block
#define GPR_$SET_CURSOR_POSITION             gpr_$set_cursor_position
#define GPR_$POINT                           gpr_$point
#define GPR_$INQ_TEXT                        gpr_$inq_text
#define GPR_$REPLICATE_FONT                  gpr_$replicate_font
#define GPR_$INQ_HORIZONTAL_SPACING          gpr_$inq_horizontal_spacing
#define GPR_$SET_HORIZONTAL_SPACING          gpr_$set_horizontal_spacing
#define GPR_$TRIANGLE                        gpr_$triangle
#define ERROR_$PRINT                          error_$print
#define GPR_$INQ_COLOR_MAP                    gpr_$inq_color_map
#define GPR_$INQ_CURSOR                       gpr_$inq_cursor
#define GPR_$INQ_RASTER_OPS                   gpr_$inq_raster_ops
#define GPRX_XBELL                            gprx_xbell
#define GPR_$SET_COLOR_MAP                    gpr_$set_color_map
#define GPR_$SET_TEXT_PATH                    gpr_$set_text_path
#define GPR_$SET_INPUT_FOCUS                  gpr_$set_input_focus
#define GPR_$WRITE_BITMAP                     gpr_$write_bitmap
#define GPR_$READ_BITMAP                      gpr_$read_bitmap
#define GPR_$GET_SYSTEM_COLORMAP              gpr_$get_system_colormap




/*    NURBS interface routines */

#define ROT2DF   rot2df
#define CLC2DT   clc2dt
#define CCT2L    cct2l
#define CCTLC    cctlc
#define CCT2C    cct2c
#define CCTC     cctc
#define CCTL     cctl
#define CCT3L    cct3l
#define CCT2LC   cct2lc
#define CCT2CL   cct2cl
#define CCT3C    cct3c
#define DDNTE    ddnte
#define DSAME    dsame



#endif
