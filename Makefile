###############################################################################
#                                                                             #
#        DAXCAD Makefile                                                      #
#                                                                             #
#        Platform:    WSL Ubuntu & Cygwin                                     #
#        Graphics:    X11                                                     # 
#                                                                             #
###############################################################################
#
# Compiler options
EXECUTABLE_NAME=daxcad

LDFLAGS=-Bstatic -Lc:/cygwin/usr/X11R6/lib -lX11  -lgcc  -lgfortran -lm  
LIBBC=libbcmain.a
MISALIGN=-misalign

FFLAGS = -g -fallow-argument-mismatch -fdollar-ok -w  -fno-second-underscore  -finit-local-zero -ff2c -fd-lines-as-comments  
#CFLAGS= -DDAXWIN32 -g -DUSE_UNDERSCORE -w -Iinclude -I. -Ic:/cygwin//usr/X11R6/include
CFLAGS= -DDAXWIN32 -g -DUSE_UNDERSCORE -w -Iinclude -I. -I/usr/include
CC=gcc
FC=gfortran


# Directories
OBJDIR = bin
FOBJDIR = bin
SRCDIR = src

# Files and folders
CSRCS    = $(shell find $(SRCDIR) -name '*.c')
SRCDIRS = $(shell find . -name '*.c' | dirname {} | sort | uniq | sed 's/\/$(SRCDIR)//g' )
COBJS    = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(CSRCS))

FSRCS    = $(shell find $(SRCDIR) -name '*.f')
SRCDIRS = $(shell find . -name '*.f' | dirname {} | sort | uniq | sed 's/\/$(SRCDIR)//g' )
FOBJS    = $(patsubst $(SRCDIR)/%.f,$(OBJDIR)/%.o,$(FSRCS))


# Targets

all: $(EXECUTABLE_NAME)
	
clean:
	rm $(EXECUTABLE_NAME) $(OBJDIR) -Rf

$(EXECUTABLE_NAME):  buildrepo $(COBJS) $(FOBJS) 
	$(CC) $(FOBJS) $(COBJS) $(LDFLAGS) -o $@
	cp daxcad daxcad.4.0

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(FOBJDIR)/%.o: $(SRCDIR)/%.f
	$(FC) $(FFLAGS) -c $< -o $@

	
buildrepo:
	@$(call make-repo)

# Create bin directory structure
define make-repo
	mkdir -p $(OBJDIR)
	for dir in $(SRCDIRS); \
	do \
		mkdir -p $(OBJDIR)/$$dir; \
	done
endef








