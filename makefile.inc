
ifdef WINE
WINEINCPATH = /usr/local/include/wine
endif
.SUFFIXES: .d .iv
.PRECIOUS : %.c %.iv
BINDIR = ../bin
LIBDIR = ../lib
INCDIR = ../include
CC = gcc
DPP = $(BINDIR)/dpp
DPPOPTS = -C
ifdef	DEBUG
OPT = -g
else
OPT = -O
endif
ifdef	BOEHM_GC
OPT += -DBOEHM_GC
endif
ifdef CYGWIN
EXE = .exe
endif
CFLAGS = -I$(INCDIR) $(OPT) -Wno-parentheses
ifdef NO_CYGWIN
EXE = .exe
CFLAGS += -mno-cygwin
endif
ifdef CPLUS
CFLAGS += -x c++
endif
ifdef FASTWIDE
DPPOPTS += -F
endif
ifdef INLINE
DPPOPTS += -S4
endif
ifdef OVERLOADS
DPPOPTS += -X
endif
ifdef WINE
CFLAGS += -I$(WINEINCPATH)/windows -D_WINDOWS -D_WIN32 -D__WINE__
endif
DPPOPTS += $(STRAT)
GFLAGS = $(CFLAGS) -Wno-return-type
%.c: %.d
	$(DPP) $(DPPOPTS) -p $<
