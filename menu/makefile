#
#     SCCS id Keywords             @(#)  300.2 date 1/30/92 makefile 
#
###############################################################################
#                                                                             #
#           Main Makefile for DAXCAD and all supporting programs              #
#                                                                             #
#           Should be portable  for UNIX platforms:                           #
#                                                                             #
#           Apollo SR9.7.x                                                    #
#           Apollo SR10.x                                                     #
#           Sun Sparc                                                         #
#           IBM RS6000                                                        #
#                                                                             #
#                                                                             #
###############################################################################
#

CC=gcc
FC=gcc
CFLAGS = -g -DBSD -DUSE_UNDERSCORE -Iinclude -I./
F77LAGS = -g 

all:

#
#	fetch includes into include directory for building
#
include:	c_includes make_include
		cd include;make -f ../make_include


cdbase:	cdbase.f cdbase
	f77 cdbase.f -o cdbase

prep:	prep.f prep
	f77 prep.f -o prep

dax.9.7:		makefile.9.7
			make -f makefile.9.7

dax.10:		makefile.10
			make -f makefile.10

dax.rs6000:	makefile.rs6000
			make -f makefile.rs6000

dax.sun4:	makefile.sun4
			make -f makefile.sun4

dax.hp700:	makefile.hp700
			make -f makefile.hp700

dax.dec3000:	makefile.dec3000
			make -f makefile.dec3000

conva:    cdbase.f cdbase prep.f prep

filter:     	$(TARGETS) filter.o 
		cc  $(TARGETS) filter.o direct.o pattern.o aposerv_c.o daxproc.o -o filter


Xfontpath:
		/usr/X11/bin/xset fp= /usr/lib/X11/fonts/75dpi,/usr/lib/X11/fonts/misc,/usr/lib/X11/fonts/oldx11,//davy.login/pt_xfonts
		/usr/X11/bin/xset fp rehash


ncpost:		ncpost.o defaults.o ncpost.h
		cc  ncpost.o defaults.o  -o ncpost
		

pwmgr:		pwmgrlib.o pwmgr.o pwmgrlib_c.o
		f77 -o pwmgr pwmgrlib.o pwmgr.o pwmgrlib_c.o

daxauth:	pwmgrlib.o daxauth.o pwmgrlib_c.o
		f77 -o daxauth pwmgrlib.o daxauth.o pwmgrlib_c.o

cdbase.f:		
			sccs get cdbase.f

prep.f:		
			sccs get prep.f


makefile.9.7:
			sccs get makefile.9.7

makefile.10:		
			sccs get makefile.10

makefile.rs6000:		
			sccs get makefile.rs6000

makefile.sun4:		
			sccs get makefile.sun4


c_includes:\
		GPRX_blank_cursor.h\
		GPRX_full_pixmap.h\
		GPRX_wait_cursor.h\
		daxcad_functions.h\
		daxgrep.h\
		defaults.h\
		ds_nurbs.h\
		gprx.h\
		gprx_errors.h\
		gprx_extern.h\
		gprx_lib.h\
		gprx_types.h\
		misomex.h\
		misomex_codes.h\
		ncpost.h\
		xeventnames.h\
		xlang.h\
		xlang_template.h
