
ifdef DEBUG
OPT = -g
else
OPT = -O3
endif

.SUFFIXES: .d .iv
.PRECIOUS : %.c %.iv
BINDIR = ../bin
LIBDIR = ../lib
INCDIR = ../include

# 32 bit ARM processor with software float
# I had to use -D_FORTIFY_SOURCE=0 because the target system had an old GLIBC
ifdef ARM32SF  
CC = arm-linux-gnueabi-gcc -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=0
HCC = gcc # for the host
else
CC = gcc
HCC = gcc
endif

DPP = $(BINDIR)/dpp
DPPOPTS = -C

ifdef THIRTYTWOBIT
OPT += -m32
endif

ifdef	BOEHM_GC
OPT += -DBOEHM_GC
endif

ifdef CYGWIN
EXE = .exe
endif

# -fPIC required by WINE
CFLAGS = -I$(INCDIR) $(OPT) -Wno-parentheses -fPIC

LDFLAGS = -lm

ifdef NATIVE_THREADS
CFLAGS += -DNATIVE_THREADS
LDFLAGS += -lpthread
endif

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

DPPOPTS += $(STRAT)
GFLAGS = $(CFLAGS) -Wno-return-type -fPIC
%.c: %.d
	$(DPP) $(DPPOPTS) -p $<
